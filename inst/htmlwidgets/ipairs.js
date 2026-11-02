/* vcdinteractive: matrix of pairwise mosaic plots with linked highlighting */
HTMLWidgets.widget({
  name: "ipairs",
  type: "output",
  factory: function (container, width, height) {
    var V = window.VCDI, D, S, dom;

    function render(x) {
      D = x;
      var st = D.settings;
      st.available = V.arr(st.available);
      D.vars.forEach(function (v) { v.levels = V.arr(v.levels); v.margin = V.arr(v.margin); });
      Object.keys(D.pairs).forEach(function (k) {
        var c = D.pairs[k].cells;
        c.idx = V.arr(c.idx).map(V.arr);
        Object.keys(c).forEach(function (f) { if (f !== "idx") c[f] = V.arr(c[f]); });
      });
      S = { type: st.residuals, adjust: st.adjust, alpha: st.alpha, zoom: null, constraint: null };
      S.shade = V.shader(st);
      dom = V.frame(container, { main: D.main });
      dom.info.textContent = "Pairwise independence in each panel. Hover a tile to highlight the same categories in all panels; click a panel to enlarge it.";
      V.el("span", { "class": "vcdi-toolbar-label", text: "Matrix of" }, dom.toolbar);
      V.el("b", { text: D.vars.length + " variables" }, dom.toolbar);
      dom.back = V.el("button", { "class": "vcdi-btn", type: "button", text: "▦ Matrix" }, dom.toolbar);
      dom.back.style.display = "none";
      dom.back.addEventListener("click", function () { S.zoom = null; draw(); });
      dom.zoomNote = V.el("span", { "class": "vcdi-mode-hint" }, dom.toolbar);

      var side = dom.side;
      V.el("div", { "class": "vcdi-section-title", text: "Residuals" }, side);
      var seg = V.el("div", { "class": "vcdi-seg" }, side), segB = {};
      st.available.forEach(function (t) {
        var b = V.el("button", { type: "button", "class": "vcdi-seg-btn", text: V.residualNames[t] }, seg);
        b.addEventListener("click", function () { setType(t); });
        segB[t] = b;
      });
      dom.segB = segB;
      var r2 = V.el("div", { "class": "vcdi-row" }, side);
      V.el("label", { text: "Correction" }, r2);
      var sa = V.el("select", {}, r2);
      ["holm", "bonferroni", "BH", "none"].forEach(function (a) {
        var o = V.el("option", { value: a, text: V.adjustNames[a] }, sa); if (a === S.adjust) o.selected = true;
      });
      sa.addEventListener("change", function () { S.adjust = this.value; S.padj = null; recolor(); renderList(); });
      V.el("label", { text: "α" }, r2);
      var sl = V.el("select", {}, r2);
      [0.1, 0.05, 0.01, 0.001].forEach(function (a) {
        var o = V.el("option", { value: a, text: String(a) }, sl); if (a === S.alpha) o.selected = true;
      });
      sl.addEventListener("change", function () { S.alpha = +this.value; recolor(); renderList(); });
      V.el("div", { "class": "vcdi-section-title", text: "Shading" }, side);
      dom.legend = V.sel("svg", { "class": "vcdi-legend", width: 270, height: 34 }, side);
      V.residualLegend(dom.legend, st);
      dom.sigTitle = V.el("div", { "class": "vcdi-section-title" }, side);
      dom.list = V.el("div", { "class": "vcdi-list" }, side);
      dom.g = V.sel("g", {}, dom.svg);
      setType(S.type);
      resize(width, height);
    }

    function pairOf(r, c) {
      var a = Math.min(r, c), b = Math.max(r, c);
      return { key: a + "-" + b, t: D.pairs[a + "-" + b] };
    }
    // multiplicity correction over all the cells of all the panels
    function padj() {
      if (S.padj && S.padjKey === S.type + S.adjust) return S.padj;
      var all = [], where = [];
      Object.keys(D.pairs).forEach(function (k) {
        D.pairs[k].cells["p_" + S.type].forEach(function (p, i) { all.push(p); where.push([k, i]); });
      });
      var adj = V.padjust(all, S.adjust), out = {};
      where.forEach(function (w, j) { (out[w[0]] = out[w[0]] || [])[w[1]] = adj[j]; });
      S.padj = out; S.padjKey = S.type + S.adjust;
      return out;
    }
    function setType(t) {
      S.type = t; S.padj = null;
      for (var k in dom.segB) dom.segB[k].classList.toggle("vcdi-on", k === t);
      recolor(); renderList();
    }

    /* ---------------------------------------------------------- drawing */
    function draw() {
      var g = dom.g; g.innerHTML = "";
      dom.tiles = [];
      var W = S.W, H = S.H, nd = D.vars.length;
      dom.back.style.display = S.zoom ? "" : "none";
      if (S.zoom) {
        var z = S.zoom;
        dom.zoomNote.textContent = z.r === z.c ? D.vars[z.r].name : D.vars[z.r].name + " (rows) × " + D.vars[z.c].name + " (columns)";
        var box = { x: 70, y: 46, w: W - 90, h: H - 64 };
        if (z.r === z.c) drawBars(g, z.r, box, true); else drawPanel(g, z.r, z.c, box, 6, true);
      } else {
        dom.zoomNote.textContent = "";
        var left = 22, top = 22, cw = (W - left - 6) / nd, ch = (H - top - 6) / nd, pad = 5;
        for (var r = 0; r < nd; r++) for (var c = 0; c < nd; c++) {
          var bx = { x: left + c * cw + pad, y: top + r * ch + pad, w: cw - 2 * pad, h: ch - 2 * pad };
          var frame = V.sel("rect", { x: bx.x - 3, y: bx.y - 3, width: bx.w + 6, height: bx.h + 6, "class": "vcdi-panel-frame" }, g);
          (function (r, c) {
            frame.addEventListener("click", function () { S.zoom = { r: r, c: c }; draw(); });
          })(r, c);
          if (r === c) drawBars(g, r, bx, false); else drawPanel(g, r, c, bx, 2, false);
        }
        D.vars.forEach(function (v, k) {
          var t1 = V.sel("text", { x: left + (k + .5) * cw, y: 15, "text-anchor": "middle", "class": "vcdi-varname" }, g);
          t1.textContent = v.name;
          var t2 = V.sel("text", { transform: "translate(14," + (top + (k + .5) * ch) + ") rotate(-90)", "text-anchor": "middle", "class": "vcdi-varname" }, g);
          t2.textContent = v.name;
        });
      }
      recolor();
      applyHighlight();
    }

    // panel (r, c): rows = variable r, columns = variable c (within each row)
    function drawPanel(g, r, c, box, gap, labels) {
      var p = pairOf(r, c), cells = p.t.cells, vr = D.vars[r], vc = D.vars[c];
      var Lr = vr.levels.length, Lc = vc.levels.length;
      var cnt = []; for (var a = 0; a < Lr; a++) { cnt.push(new Array(Lc).fill(0)); }
      var cellAt = {};
      for (var i = 0; i < cells.observed.length; i++) {
        var la = cells.idx[r][i], lb = cells.idx[c][i];
        cnt[la][lb] = cells.observed[i]; cellAt[la + "," + lb] = i;
      }
      var rowTot = cnt.map(function (row) { return V.sum(row); }), tot = V.sum(rowTot);
      var availH = Math.max(0, box.h - gap * (Lr - 1)), y = box.y;
      var font = "11px system-ui, sans-serif";
      for (a = 0; a < Lr; a++) {
        var hh = tot > 0 ? availH * rowTot[a] / tot : 0;
        var availW = Math.max(0, box.w - gap * (Lc - 1)), x = box.x;
        for (var b = 0; b < Lc; b++) {
          var ww = rowTot[a] > 0 ? availW * cnt[a][b] / rowTot[a] : 0;
          var ci = cellAt[a + "," + b];
          if (ww > 0.01 && hh > 0.01) {
            var rect = V.sel("rect", { x: x, y: y, width: ww, height: hh, "class": "vcdi-tile" }, g);
            rect.__info = { key: p.key, i: ci, r: r, c: c, a: a, b: b };
            dom.tiles.push(rect);
            rect.addEventListener("mousemove", onTile);
            rect.addEventListener("mouseleave", offTile);
            rect.addEventListener("click", function () { if (!S.zoom) { S.zoom = { r: this.__info.r, c: this.__info.c }; draw(); } });
          } else {
            V.sel("circle", { cx: x + ww / 2, cy: y + hh / 2, r: labels ? 3 : 1.6, "class": "vcdi-zero" }, g);
          }
          if (labels && a === 0) {
            var tx = V.fitText(vc.levels[b], ww + 4, font);
            if (tx) { var t = V.sel("text", { x: x + ww / 2, y: box.y - 8, "text-anchor": "middle", "class": "vcdi-label" }, g); t.textContent = tx; }
          }
          x += ww + gap;
        }
        if (labels) {
          var ty = V.fitText(vr.levels[a], hh + 4, font);
          if (ty) {
            var tl = V.sel("text", { transform: "translate(" + (box.x - 8) + "," + (y + hh / 2) + ") rotate(-90)", "text-anchor": "middle", "class": "vcdi-label" }, g);
            tl.textContent = ty;
          }
        }
        y += hh + gap;
      }
      if (labels) {
        var n1 = V.sel("text", { x: box.x + box.w / 2, y: box.y - 26, "text-anchor": "middle", "class": "vcdi-varname" }, g); n1.textContent = vc.name;
        var n2 = V.sel("text", { transform: "translate(" + (box.x - 32) + "," + (box.y + box.h / 2) + ") rotate(-90)", "text-anchor": "middle", "class": "vcdi-varname" }, g); n2.textContent = vr.name;
      }
    }

    // diagonal: bar chart of the margin of variable k
    function drawBars(g, k, box, big) {
      var v = D.vars[k], L = v.levels.length, mx = Math.max.apply(null, v.margin.concat([1]));
      var top = big ? 10 : 16, gap = Math.min(6, box.w / L * 0.15), bw = (box.w - gap * (L - 1)) / L;
      if (!big) { var t = V.sel("text", { x: box.x + box.w / 2, y: box.y + 11, "text-anchor": "middle", "class": "vcdi-varname" }, g); t.textContent = v.name; }
      for (var l = 0; l < L; l++) {
        var hh = (box.h - top - (big ? 18 : 12)) * v.margin[l] / mx, x = box.x + l * (bw + gap), y = box.y + box.h - (big ? 18 : 12) - hh;
        var rect = V.sel("rect", { x: x, y: y, width: bw, height: hh, "class": "vcdi-bar" }, g);
        rect.__bar = { k: k, l: l };
        rect.addEventListener("mousemove", function (ev) {
          var b = this.__bar, vv = D.vars[b.k];
          S.constraint = {}; S.constraint[b.k] = b.l; applyHighlight();
          dom.showTip("<div class='vcdi-tt-title'>" + V.esc(vv.name) + " = <b>" + V.esc(vv.levels[b.l]) + "</b></div>Count: " +
            V.fmtCount(vv.margin[b.l]) + " (" + (100 * vv.margin[b.l] / V.sum(vv.margin)).toFixed(1) + "%)", ev);
        });
        rect.addEventListener("mouseleave", offTile);
        rect.addEventListener("click", function () { if (!S.zoom) { S.zoom = { r: this.__bar.k, c: this.__bar.k }; draw(); } });
        var lab = V.fitText(v.levels[l], bw + 2);
        if (lab) { var tl = V.sel("text", { x: x + bw / 2, y: box.y + box.h - (big ? 4 : 2), "text-anchor": "middle", "class": "vcdi-label" }, g); tl.textContent = lab; }
      }
    }

    function recolor() {
      var pa = padj();
      (dom.tiles || []).forEach(function (r) {
        var inf = r.__info, c = D.pairs[inf.key].cells;
        r.style.fill = S.shade(c[S.type][inf.i]);
        r.classList.toggle("vcdi-sig", pa[inf.key][inf.i] < S.alpha);
      });
    }

    /* ------------------------------------------------ linked highlighting */
    function applyHighlight() {
      var con = S.constraint;
      dom.svg.classList.toggle("vcdi-dim", !!con);
      if (!con) return;
      (dom.tiles || []).forEach(function (rect) {
        var inf = rect.__info, shares = false, ok = true;
        if (con[inf.r] !== undefined) { shares = true; if (con[inf.r] !== inf.a) ok = false; }
        if (con[inf.c] !== undefined) { shares = true; if (con[inf.c] !== inf.b) ok = false; }
        rect.classList.toggle("vcdi-hl", !shares || ok);
      });
    }
    function onTile(ev) {
      var inf = this.__info, c = D.pairs[inf.key].cells, i = inf.i, pa = padj();
      S.constraint = {}; S.constraint[inf.r] = inf.a; S.constraint[inf.c] = inf.b;
      applyHighlight();
      this.classList.add("vcdi-hover");
      var vr = D.vars[inf.r], vc = D.vars[inf.c], o = c.observed[i];
      var e = S.type === "robust" ? c.expected_robust[i] : c.expected[i], r = c[S.type][i];
      var h = "<div class='vcdi-tt-title'>" + V.esc(vr.name) + " = <b>" + V.esc(vr.levels[inf.a]) + "</b><br>" +
        V.esc(vc.name) + " = <b>" + V.esc(vc.levels[inf.b]) + "</b></div><table>" +
        "<tr><td>Observed</td><td class='num'>" + V.fmtCount(o) + "</td></tr>" +
        "<tr><td>Expected (independence)</td><td class='num'>" + V.fmt(e, 2) + "</td></tr>" +
        "<tr><td>O/E</td><td class='num'>" + (e > 0 ? V.fmt(o / e, 2) : "–") + "</td></tr>" +
        "<tr class='sel'><td>" + V.residualNames[S.type] + " residual</td><td class='num'>" + V.signed(r, 2) + "</td></tr>" +
        "<tr><td>p (adjusted, " + V.adjustNames[S.adjust] + ")</td><td class='num'>" + V.fmtP(pa[inf.key][i]) + "</td></tr></table>";
      dom.showTip(h, ev);
    }
    function offTile() {
      S.constraint = null; applyHighlight(); dom.hideTip();
      (dom.tiles || []).forEach(function (r) { r.classList.remove("vcdi-hover"); });
    }

    /* ----------------------------------------------------------- list */
    function renderList() {
      var pa = padj(), rows = [];
      Object.keys(D.pairs).forEach(function (k) {
        var c = D.pairs[k].cells, ij = k.split("-").map(Number);
        c.observed.forEach(function (o, i) {
          if (pa[k][i] < S.alpha) rows.push({ k: k, i: i, a: ij[0], b: ij[1], r: c[S.type][i] });
        });
      });
      rows.sort(function (p, q) { return Math.abs(q.r) - Math.abs(p.r); });
      dom.sigTitle.textContent = "Significant cells (" + rows.length + ")";
      dom.list.innerHTML = "";
      if (!rows.length) { V.el("div", { "class": "vcdi-empty", text: "No cell is significant at this level." }, dom.list); return; }
      var tbl = V.el("table", {}, dom.list), hr = V.el("tr", {}, V.el("thead", {}, tbl));
      ["Cell", "O", "E", "r", "p adj."].forEach(function (h) { V.el("th", { text: h }, hr); });
      var tb = V.el("tbody", {}, tbl);
      rows.forEach(function (w) {
        var c = D.pairs[w.k].cells, la = c.idx[w.a][w.i], lb = c.idx[w.b][w.i];
        var tr = V.el("tr", {}, tb), td = V.el("td", {}, tr);
        V.el("span", { "class": "vcdi-swatch", style: "background:" + S.shade(w.r) }, td);
        td.appendChild(document.createTextNode(D.vars[w.a].levels[la] + " · " + D.vars[w.b].levels[lb]));
        td.title = D.vars[w.a].name + " = " + D.vars[w.a].levels[la] + ", " + D.vars[w.b].name + " = " + D.vars[w.b].levels[lb];
        V.el("td", { text: V.fmtCount(c.observed[w.i]) }, tr);
        V.el("td", { text: V.fmt(S.type === "robust" ? c.expected_robust[w.i] : c.expected[w.i], 1) }, tr);
        V.el("td", { text: V.signed(w.r, 2) }, tr);
        V.el("td", { text: V.fmtP(pa[w.k][w.i]) }, tr);
        tr.addEventListener("mouseenter", function () {
          S.constraint = {}; S.constraint[w.a] = la; S.constraint[w.b] = lb; applyHighlight();
        });
        tr.addEventListener("mouseleave", function () { S.constraint = null; applyHighlight(); });
      });
    }

    function resize(w, h) {
      if (!D) return;
      var sz = dom.size(w, h);
      S.W = sz.W; S.H = sz.H;
      draw();
    }

    return {
      renderValue: function (x) { render(x); },
      resize: function (w, h) { width = w; height = h; resize(w, h); }
    };
  }
});
