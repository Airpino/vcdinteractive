/* vcdinteractive: interactive rootogram */
HTMLWidgets.widget({
  name: "irootogram",
  type: "output",
  factory: function (container, width, height) {
    var V = window.VCDI, D, S, dom;

    function render(x) {
      D = x;
      D.values = V.arr(D.values); D.freq = V.arr(D.freq); D.fits = V.arr(D.fits);
      D.fits.forEach(function (f) { f.expected = V.arr(f.expected); });
      S = { fit: 0, type: D.settings.type, scale: D.settings.scale, prev: null, hover: -1 };
      S.shade = V.shader(D.settings);
      dom = V.frame(container, { main: D.main, side: false });
      V.el("span", { "class": "vcdi-toolbar-label", text: "Distribution:" }, dom.toolbar);
      V.segmented(dom.toolbar, D.fits.map(function (f, i) {
        return { value: i, label: f.label, title: f.label + " (" + f.params + "): G² = " + V.fmt(f.G2, 2) + ", df = " + f.df + ", p = " + V.fmtP(f.p_G2) };
      }), 0, function (v) { S.fit = +v; draw(true); });
      V.el("span", { "class": "vcdi-toolbar-label", text: "Type:" }, dom.toolbar);
      V.segmented(dom.toolbar, [{ value: "hanging", label: "Hanging" }, { value: "standing", label: "Standing" }, { value: "deviation", label: "Deviation" }],
        S.type, function (v) { S.type = v; draw(true); });
      V.el("span", { "class": "vcdi-toolbar-label", text: "Scale:" }, dom.toolbar);
      V.segmented(dom.toolbar, [{ value: "sqrt", label: "√ Frequency" }, { value: "raw", label: "Frequency" }],
        S.scale, function (v) { S.scale = v; draw(true); });
      dom.gGrid = V.sel("g", { "class": "vcdi-axis" }, dom.svg);
      dom.gBars = V.sel("g", {}, dom.svg);
      dom.gCurve = V.sel("g", {}, dom.svg);
      dom.bars = D.values.map(function (_, k) {
        var r = V.sel("rect", { "class": "vcdi-root-bar" }, dom.gBars);
        r.addEventListener("mousemove", function (ev) { tip(k, ev); });
        r.addEventListener("mouseleave", function () { dom.hideTip(); r.classList.remove("vcdi-hover"); });
        return r;
      });
      resize(width, height);
    }

    function info() {
      var f = D.fits[S.fit], N = V.sum(D.freq);
      var all = D.fits.map(function (g) { return g.label + ": G² = " + V.fmt(g.G2, 2) + " (df " + g.df + ", p " + V.fmtP(g.p_G2) + ")"; });
      dom.info.innerHTML = "<b>" + V.esc(f.label) + "</b> (" + V.esc(f.params) + ") &nbsp; Pearson X² = " + V.fmt(f.X2, 2) +
        ", LR G² = " + V.fmt(f.G2, 2) + ", df = " + f.df + ", p = " + V.fmtP(f.p_G2) + " &nbsp; N = " + V.fmtCount(N) +
        (D.fits.length > 1 ? "<br><span style='color:#656d76'>" + V.esc(all.join("   ·   ")) + "</span>" : "");
    }

    function geometry() {
      var e = D.fits[S.fit].expected, f = D.freq, tr = S.scale === "sqrt" ? Math.sqrt : function (v) { return v; };
      return D.values.map(function (_, k) {
        var te = tr(e[k]), tf = tr(f[k]);
        if (S.type === "standing") return { lo: 0, hi: tf, curve: te };
        if (S.type === "hanging") return { lo: te - tf, hi: te, curve: te };
        return { lo: Math.min(0, te - tf), hi: Math.max(0, te - tf), curve: te };
      });
    }

    function draw(animate) {
      info();
      var W = S.W, H = S.H, m = { l: 58, r: 20, t: 18, b: 46 };
      var P = { x: m.l, y: m.t, w: W - m.l - m.r, h: H - m.t - m.b };
      var G = geometry(), lo = 0, hi = 0;
      G.forEach(function (g) { lo = Math.min(lo, g.lo); hi = Math.max(hi, g.hi, g.curve); });
      var pad = (hi - lo) * 0.05; lo = lo < 0 ? lo - pad : 0; hi += pad;
      var sy = function (v) { return P.y + P.h - (v - lo) / (hi - lo) * P.h; };
      var K = D.values.length, band = P.w / K, bw = band * 0.78;
      var gg = dom.gGrid; gg.innerHTML = "";
      V.niceTicks(lo, hi, 6).forEach(function (t) {
        V.sel("line", { x1: P.x, x2: P.x + P.w, y1: sy(t), y2: sy(t), "class": t === 0 ? "vcdi-zero-line" : "vcdi-gridline" }, gg);
        var tt = V.sel("text", { x: P.x - 6, y: sy(t) + 4, "text-anchor": "end" }, gg); tt.textContent = t;
      });
      var step = Math.max(1, Math.ceil(28 / band));
      D.values.forEach(function (v, k) {
        if (k % step) return;
        var tx = V.sel("text", { x: P.x + band * (k + .5), y: P.y + P.h + 14, "text-anchor": "middle" }, gg); tx.textContent = v;
      });
      var xl = V.sel("text", { x: P.x + P.w / 2, y: H - 8, "text-anchor": "middle", "class": "vcdi-varname" }, gg); xl.textContent = D.xlab;
      var yl = V.sel("text", { transform: "translate(14," + (P.y + P.h / 2) + ") rotate(-90)", "text-anchor": "middle", "class": "vcdi-varname" }, gg);
      yl.textContent = S.scale === "sqrt" ? "√ Frequency" : "Frequency";

      var to = G.map(function (g, k) { return { x: P.x + band * k + (band - bw) / 2, y: sy(g.hi), w: bw, h: Math.max(0, sy(g.lo) - sy(g.hi)) }; });
      var e = D.fits[S.fit].expected;
      dom.bars.forEach(function (r, k) { r.style.fill = S.shade((D.freq[k] - e[k]) / Math.sqrt(e[k])); });
      var from = animate && S.prev ? S.prev : to, t0 = null;
      if (S.anim) cancelAnimationFrame(S.anim);
      function set(t) {
        dom.bars.forEach(function (r, k) {
          var a = from[k], b = to[k];
          r.setAttribute("x", a.x + (b.x - a.x) * t); r.setAttribute("y", a.y + (b.y - a.y) * t);
          r.setAttribute("width", a.w + (b.w - a.w) * t); r.setAttribute("height", a.h + (b.h - a.h) * t);
        });
      }
      function step2(ts) {
        if (t0 === null) t0 = ts;
        var t = Math.min(1, (ts - t0) / 400);
        set(V.ease(t));
        if (t < 1) S.anim = requestAnimationFrame(step2); else { S.anim = null; S.prev = to; }
      }
      if (from === to) { set(1); S.prev = to; } else S.anim = requestAnimationFrame(step2);
      // expected curve
      var gc = dom.gCurve; gc.innerHTML = "";
      var pts = G.map(function (g, k) { return (P.x + band * (k + .5)).toFixed(1) + "," + sy(g.curve).toFixed(1); });
      V.sel("polyline", { points: pts.join(" "), "class": "vcdi-root-curve" }, gc);
      G.forEach(function (g, k) { V.sel("circle", { cx: P.x + band * (k + .5), cy: sy(g.curve), r: 3, "class": "vcdi-root-pt" }, gc); });
    }

    function tip(k, ev) {
      var f = D.fits[S.fit], o = D.freq[k], e = f.expected[k], r = (o - e) / Math.sqrt(e);
      dom.bars[k].classList.add("vcdi-hover");
      dom.showTip("<div class='vcdi-tt-title'>" + V.esc(D.xlab) + " = <b>" + D.values[k] + "</b></div><table>" +
        "<tr><td>Observed</td><td class='num'>" + V.fmtCount(o) + "</td></tr>" +
        "<tr><td>Expected (" + V.esc(f.label) + ")</td><td class='num'>" + V.fmt(e, 2) + "</td></tr>" +
        "<tr><td>√observed − √expected</td><td class='num'>" + V.signed(Math.sqrt(o) - Math.sqrt(e), 3) + "</td></tr>" +
        "<tr class='sel'><td>Pearson residual</td><td class='num'>" + V.signed(r, 2) + "</td></tr>" +
        "<tr><td>Contribution to X²</td><td class='num'>" + V.fmt(r * r, 2) + "</td></tr></table>", ev);
    }

    function resize(w, h) {
      if (!D) return;
      var sz = dom.size(w, h); S.W = sz.W; S.H = sz.H; S.prev = null; draw(false);
    }
    return {
      renderValue: function (x) { render(x); },
      resize: function (w, h) { width = w; height = h; resize(w, h); }
    };
  }
});
