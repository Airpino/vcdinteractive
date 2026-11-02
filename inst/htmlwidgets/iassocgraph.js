/* vcdinteractive: association graph of log-linear models */
HTMLWidgets.widget({
  name: "iassocgraph",
  type: "output",
  factory: function (container, width, height) {
    var V = window.VCDI, D, S, dom;

    function render(x) {
      D = x;
      D.vars = V.arr(D.vars); D.models = V.arr(D.models); D.pairs = V.arr(D.pairs);
      D.models.forEach(function (m) { m.edges = V.arr(m.edges); });
      S = { m: D.settings.model || 0, marg: false, pos: null, hover: -1 };
      dom = V.frame(container, { main: D.main });
      var cl = V.el("label", { "class": "vcdi-toolbar-label" }, dom.toolbar);
      var cb = V.el("input", { type: "checkbox" }, cl);
      cl.appendChild(document.createTextNode(" show marginal associations (Cramér's V) of all pairs"));
      cb.addEventListener("change", function () { S.marg = this.checked; draw(); });
      V.el("span", { "class": "vcdi-mode-hint", text: "Edge width ∝ ΔG² for removing the association; drag the nodes." }, dom.toolbar);
      V.el("div", { "class": "vcdi-section-title", text: "Models" }, dom.side);
      dom.models = V.el("div", { "class": "vcdi-models" }, dom.side);
      dom.models.style.maxHeight = "220px";
      V.el("div", { "class": "vcdi-section-title", text: "Partial associations in the model" }, dom.side);
      dom.edges = V.el("div", { "class": "vcdi-list" }, dom.side);
      dom.g = V.sel("g", {}, dom.svg);
      renderModels();
      resize(width, height);
    }

    function layout(W, H) {
      var n = D.vars.length, cx = W / 2, cy = H / 2, R = Math.min(W - 120, H - 80) * 0.45;
      var pos = D.vars.map(function (_, i) { var a = -Math.PI / 2 + 2 * Math.PI * i / n; return { x: cx + R * Math.cos(a), y: cy + R * Math.sin(a) }; });
      return pos;
    }

    function draw() {
      var g = dom.g; g.innerHTML = "";
      var M = D.models[S.m], pos = S.pos, maxG = 1e-9;
      D.models.forEach(function (m) { m.edges.forEach(function (e) { maxG = Math.max(maxG, e.dG2); }); });
      if (S.marg) D.pairs.forEach(function (p) {
        var a = pos[p.i], b = pos[p.j];
        var l = V.sel("line", { x1: a.x, y1: a.y, x2: b.x, y2: b.y, "stroke-width": 1 + 8 * p.V, "class": "vcdi-ag-marg" }, g);
        l.addEventListener("mousemove", function (ev) {
          dom.showTip("<div class='vcdi-tt-title'>" + V.esc(D.vars[p.i].name) + " – " + V.esc(D.vars[p.j].name) + " (marginal)</div>" +
            "Cramér's V = <b>" + V.fmt(p.V, 3) + "</b><br>χ² = " + V.fmt(p.chisq, 2) + ", df = " + p.df + ", p = " + V.fmtP(p.p), ev);
        });
        l.addEventListener("mouseleave", function () { dom.hideTip(); });
      });
      dom.edgeEls = [];
      M.edges.forEach(function (e) {
        var a = pos[e.i], b = pos[e.j], sig = e.p < 0.05;
        var l = V.sel("line", { x1: a.x, y1: a.y, x2: b.x, y2: b.y, "stroke-width": 1.5 + 12 * Math.sqrt(e.dG2 / maxG),
          "class": "vcdi-ag-edge" + (sig ? "" : " vcdi-ag-ns") }, g);
        l.__e = e; dom.edgeEls.push(l);
        l.addEventListener("mousemove", function (ev) {
          dom.showTip("<div class='vcdi-tt-title'>" + V.esc(D.vars[e.i].name) + " – " + V.esc(D.vars[e.j].name) + "</div><table>" +
            "<tr><td>ΔG² (association removed)</td><td class='num'><b>" + V.fmt(e.dG2, 2) + "</b></td></tr>" +
            "<tr><td>Δdf</td><td class='num'>" + e.ddf + "</td></tr><tr><td>p</td><td class='num'>" + V.fmtP(e.p) + "</td></tr></table>", ev);
        });
        l.addEventListener("mouseleave", function () { dom.hideTip(); });
        var mx = (a.x + b.x) / 2, my = (a.y + b.y) / 2;
        var t = V.sel("text", { x: mx, y: my - 6, "text-anchor": "middle", "class": "vcdi-ag-elab" }, g); t.textContent = V.fmt(e.dG2, 1);
      });
      D.vars.forEach(function (v, i) {
        var p = pos[i], r = Math.max(18, V.textWidth(v.name, "600 12px system-ui, sans-serif") / 2 + 9);
        var gn = V.sel("g", { "class": "vcdi-ag-node", transform: "translate(" + p.x + "," + p.y + ")" }, g);
        V.sel("circle", { r: r }, gn);
        var t = V.sel("text", { y: 4, "text-anchor": "middle" }, gn); t.textContent = v.name;
        var t2 = V.sel("text", { y: r + 13, "text-anchor": "middle", "class": "vcdi-note" }, gn); t2.textContent = v.nlev + " levels";
        gn.addEventListener("mouseenter", function () {
          dom.edgeEls.forEach(function (l) { l.classList.toggle("vcdi-ag-fade", l.__e.i !== i && l.__e.j !== i); });
        });
        gn.addEventListener("mouseleave", function () { dom.edgeEls.forEach(function (l) { l.classList.remove("vcdi-ag-fade"); }); });
        gn.addEventListener("pointerdown", function (ev) {
          ev.preventDefault();
          var box = dom.svg.getBoundingClientRect();
          function mv(e2) { p.x = e2.clientX - box.left; p.y = e2.clientY - box.top; draw(); }
          function up() { window.removeEventListener("pointermove", mv); window.removeEventListener("pointerup", up); }
          window.addEventListener("pointermove", mv); window.addEventListener("pointerup", up);
        });
      });
      renderEdges();
    }

    function renderModels() {
      dom.models.innerHTML = "";
      var tbl = V.el("table", {}, dom.models), hr = V.el("tr", {}, V.el("thead", {}, tbl));
      ["Model", "G²", "df", "p", "AIC"].forEach(function (h) { V.el("th", { text: h }, hr); });
      var tb = V.el("tbody", {}, tbl);
      D.models.forEach(function (m, i) {
        var tr = V.el("tr", { "class": i === S.m ? "vcdi-cur" : "" }, tb);
        var td = V.el("td", { text: m.name }, tr);
        V.el("td", { text: V.fmt(m.G2, 1) }, tr); V.el("td", { text: String(m.df) }, tr);
        V.el("td", { text: m.df > 0 ? V.fmtP(m.p) : "–" }, tr); V.el("td", { text: V.fmt(m.aic, 1) }, tr);
        tr.addEventListener("click", function () { S.m = i; renderModels(); draw(); });
      });
      dom.info.innerHTML = "Model <code>" + V.esc(D.models[S.m].label) + "</code> &nbsp; G² = " + V.fmt(D.models[S.m].G2, 2) +
        ", df = " + D.models[S.m].df + (D.models[S.m].df > 0 ? ", p = " + V.fmtP(D.models[S.m].p) : " (saturated)") + ", AIC = " + V.fmt(D.models[S.m].aic, 1);
    }
    function renderEdges() {
      var M = D.models[S.m], el = dom.edges; el.innerHTML = "";
      if (!M.edges.length) { V.el("div", { "class": "vcdi-empty", text: "No association in this model (mutual independence)." }, el); return; }
      var tbl = V.el("table", {}, el), hr = V.el("tr", {}, V.el("thead", {}, tbl));
      ["Pair", "ΔG²", "Δdf", "p"].forEach(function (h) { V.el("th", { text: h }, hr); });
      var tb = V.el("tbody", {}, tbl);
      M.edges.slice().sort(function (a, b) { return b.dG2 - a.dG2; }).forEach(function (e) {
        var tr = V.el("tr", {}, tb);
        V.el("td", { text: D.vars[e.i].name + " – " + D.vars[e.j].name }, tr);
        V.el("td", { text: V.fmt(e.dG2, 1) }, tr); V.el("td", { text: String(e.ddf) }, tr); V.el("td", { text: V.fmtP(e.p) }, tr);
        tr.addEventListener("mouseenter", function () { dom.edgeEls.forEach(function (l) { l.classList.toggle("vcdi-ag-fade", l.__e !== e); }); });
        tr.addEventListener("mouseleave", function () { dom.edgeEls.forEach(function (l) { l.classList.remove("vcdi-ag-fade"); }); });
      });
    }

    function resize(w, h) {
      if (!D) return;
      var sz = dom.size(w, h); S.W = sz.W; S.H = sz.H;
      S.pos = layout(S.W, S.H);
      draw();
    }
    return {
      renderValue: function (x) { render(x); },
      resize: function (w, h) { width = w; height = h; resize(w, h); }
    };
  }
});
