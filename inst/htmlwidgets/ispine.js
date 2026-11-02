/* vcdinteractive: spinogram and conditional density plot */
HTMLWidgets.widget({
  name: "ispine",
  type: "output",
  factory: function (container, width, height) {
    var V = window.VCDI, D, S, dom;

    function render(x) {
      D = x;
      D.levels = V.arr(D.levels); D.fill = V.arr(D.fill); D.x = V.arr(D.x); D.y = V.arr(D.y);
      var st = D.settings;
      S = { mode: st.display, nb: st.breaks, kind: "equal", bwm: st.bw };
      var n = D.x.length;
      S.maxBins = Math.max(3, Math.min(60, Math.floor(n / 3)));
      dom = V.frame(container, { main: D.main, side: false });
      dom.info.textContent = D.yname + " ~ " + D.xname + "   ·   n = " + n;
      var modes = V.segmented(dom.toolbar, [{ value: "spine", label: "Spinogram" }, { value: "cdplot", label: "Conditional density" }],
        S.mode, function (v) { S.mode = v; controls(); draw(); });
      dom.ctl = V.el("span", { "class": "vcdi-ctl" }, dom.toolbar);
      dom.g = V.sel("g", {}, dom.svg);
      controls();
      resize(width, height);
    }

    function controls() {
      var c = dom.ctl; c.innerHTML = "";
      if (S.mode === "spine") {
        V.el("span", { "class": "vcdi-toolbar-label", text: "Bins:" }, c);
        var sl = V.el("input", { type: "range", min: 2, max: S.maxBins, step: 1, value: S.nb, "class": "vcdi-range" }, c);
        var lab = V.el("b", { text: String(S.nb) }, c);
        sl.addEventListener("input", function () { S.nb = +this.value; lab.textContent = this.value; draw(); });
        V.segmented(c, [{ value: "equal", label: "Equal width" }, { value: "quantile", label: "Quantiles" }],
          S.kind, function (v) { S.kind = v; draw(); });
      } else {
        V.el("span", { "class": "vcdi-toolbar-label", text: "Bandwidth ×" }, c);
        var sb = V.el("input", { type: "range", min: -2, max: 2, step: 0.05, value: Math.log2(S.bwm), "class": "vcdi-range" }, c);
        var lb = V.el("b", {}, c);
        var upd = function () { lb.textContent = V.fmt(S.bwm, 2) + "  (h = " + V.fmt(S.bwm * D.bw0, 3) + ")"; };
        upd();
        sb.addEventListener("input", function () { S.bwm = Math.pow(2, +this.value); upd(); draw(); });
      }
    }

    function frame() {
      var W = S.W, H = S.H, rw = 20;
      D.levels.forEach(function (l) { rw = Math.max(rw, V.textWidth(l)); });
      return { x: 56, y: 16, w: W - 56 - Math.min(160, rw + 22), h: H - 16 - 46 };
    }
    function axesY(g, P) {
      var ax = V.sel("g", { "class": "vcdi-axis" }, g);
      [0, .2, .4, .6, .8, 1].forEach(function (t) {
        var y = P.y + P.h * (1 - t);
        V.sel("line", { x1: P.x - 4, x2: P.x, y1: y, y2: y }, ax);
        var tt = V.sel("text", { x: P.x - 7, y: y + 4, "text-anchor": "end" }, ax); tt.textContent = t;
      });
      var yl = V.sel("text", { transform: "translate(14," + (P.y + P.h / 2) + ") rotate(-90)", "text-anchor": "middle", "class": "vcdi-varname" }, g);
      yl.textContent = D.yname;
      var xl = V.sel("text", { x: P.x + P.w / 2, y: S.H - 6, "text-anchor": "middle", "class": "vcdi-varname" }, g);
      xl.textContent = D.xname;
    }

    function breaks() {
      var xs = D.x.slice().sort(function (a, b) { return a - b; }), n = xs.length, lo = xs[0], hi = xs[n - 1], b = [], k;
      if (S.kind === "quantile") {
        for (k = 0; k <= S.nb; k++) {
          var pos = (n - 1) * k / S.nb, f = Math.floor(pos);
          b.push(xs[f] + (pos - f) * ((xs[Math.min(n - 1, f + 1)]) - xs[f]));
        }
        b = b.filter(function (v, i) { return i === 0 || v > b[i - 1]; });
      } else {
        for (k = 0; k <= S.nb; k++) b.push(lo + (hi - lo) * k / S.nb);
      }
      if (b.length < 2) b = [lo, hi + 1];
      return b;
    }

    function draw() {
      var g = dom.g; g.innerHTML = "";
      var P = frame();
      axesY(g, P);
      if (S.mode === "spine") drawSpine(g, P); else drawCD(g, P);
    }

    function drawSpine(g, P) {
      var b = breaks(), nb = b.length - 1, K = D.levels.length;
      var cnt = [], tot = new Array(nb).fill(0);
      for (var j = 0; j < nb; j++) cnt.push(new Array(K).fill(0));
      D.x.forEach(function (v, i) {
        var j = 0; while (j < nb - 1 && v >= b[j + 1]) j++;  // right-closed last bin
        if (v > b[j + 1] && j === nb - 1) j = nb - 1;
        cnt[j][D.y[i]]++; tot[j]++;
      });
      var N = V.sum(tot), gap = 1, avail = P.w - gap * (nb - 1), x = P.x, ax = V.sel("g", { "class": "vcdi-axis" }, g);
      var lastBin = null;
      for (j = 0; j < nb; j++) {
        var w = N > 0 ? avail * tot[j] / N : 0, y = P.y + P.h;
        for (var k = 0; k < K; k++) {
          var h = tot[j] > 0 ? P.h * cnt[j][k] / tot[j] : 0;
          y -= h;
          if (w > 0 && h > 0) {
            var r = V.sel("rect", { x: x, y: y, width: w, height: h, fill: D.fill[k], "class": "vcdi-spine-seg" }, g);
            r.__d = { j: j, k: k, a: b[j], b: b[j + 1], c: cnt[j][k], t: tot[j] };
            r.addEventListener("mousemove", tipSpine);
            r.addEventListener("mouseleave", function () { dom.hideTip(); });
          }
        }
        if (tot[j] > 0) lastBin = { x: x, w: w, j: j };
        if (j === 0 || V.textWidth(fmtB(b[j])) < w * 1.6 || j === nb) {
          var tt = V.sel("text", { x: x - gap / 2, y: P.y + P.h + 14, "text-anchor": "middle" }, ax); tt.textContent = fmtB(b[j]);
        }
        x += w + gap;
      }
      var te = V.sel("text", { x: P.x + P.w, y: P.y + P.h + 14, "text-anchor": "middle" }, ax); te.textContent = fmtB(b[nb]);
      // response labels at the right of the last non-empty bin
      if (lastBin) {
        var yy = P.y + P.h;
        for (k = 0; k < K; k++) {
          var hh = P.h * cnt[lastBin.j][k] / tot[lastBin.j];
          if (hh > 10) { var lt = V.sel("text", { x: P.x + P.w + 6, y: yy - hh / 2 + 4, "class": "vcdi-label" }, g); lt.textContent = D.levels[k]; }
          yy -= hh;
        }
      }
    }
    function fmtB(v) { var a = Math.abs(v); return a >= 100 ? v.toFixed(0) : a >= 10 ? v.toFixed(1) : v.toFixed(2); }

    function cdEstimate(G) {
      var xs = D.x, n = xs.length, lo = Math.min.apply(null, xs), hi = Math.max.apply(null, xs), K = D.levels.length;
      var h = S.bwm * D.bw0, grid = [], P = [];
      for (var gi = 0; gi < G; gi++) {
        var x0 = lo + (hi - lo) * gi / (G - 1), num = new Array(K).fill(0), den = 0;
        for (var i = 0; i < n; i++) {
          var z = (x0 - xs[i]) / h; if (z > 8 || z < -8) continue;
          var w = Math.exp(-0.5 * z * z); num[D.y[i]] += w; den += w;
        }
        grid.push(x0);
        P.push(num.map(function (v) { return den > 0 ? v / den : 1 / K; }));
      }
      return { grid: grid, P: P, lo: lo, hi: hi, h: h };
    }

    function drawCD(g, P) {
      var E = cdEstimate(Math.max(60, Math.min(300, Math.round(P.w / 3)))), K = D.levels.length;
      S.cd = { E: E, P: P };
      var sx = function (v) { return P.x + (v - E.lo) / (E.hi - E.lo) * P.w; };
      var cum = E.P.map(function () { return 0; });
      for (var k = 0; k < K; k++) {
        var top = [], bot = [];
        E.grid.forEach(function (x0, i) {
          bot.push(sx(x0).toFixed(1) + "," + (P.y + P.h * (1 - cum[i])).toFixed(1));
          cum[i] += E.P[i][k];
          top.push(sx(x0).toFixed(1) + "," + (P.y + P.h * (1 - cum[i])).toFixed(1));
        });
        V.sel("polygon", { points: top.concat(bot.reverse()).join(" "), fill: D.fill[k], "class": "vcdi-cd-area" }, g);
        var last = E.P[E.P.length - 1], acc = 0; for (var q = 0; q < k; q++) acc += last[q];
        var hh = P.h * last[k];
        if (hh > 10) { var lt = V.sel("text", { x: P.x + P.w + 6, y: P.y + P.h * (1 - acc) - hh / 2 + 4, "class": "vcdi-label" }, g); lt.textContent = D.levels[k]; }
      }
      var ax = V.sel("g", { "class": "vcdi-axis" }, g);
      V.niceTicks(E.lo, E.hi, 7).forEach(function (t) {
        V.sel("line", { x1: sx(t), x2: sx(t), y1: P.y + P.h, y2: P.y + P.h + 4 }, ax);
        var tt = V.sel("text", { x: sx(t), y: P.y + P.h + 15, "text-anchor": "middle" }, ax); tt.textContent = t;
      });
      var rug = "", step = Math.max(1, Math.floor(D.x.length / 1500));
      for (var i = 0; i < D.x.length; i += step) rug += "M" + sx(D.x[i]).toFixed(1) + " " + (P.y + P.h + 20) + "v5";
      V.sel("path", { d: rug, "class": "vcdi-rug" }, g);
      dom.cursor = V.sel("line", { y1: P.y, y2: P.y + P.h, "class": "vcdi-cursor" }, g);
      var hit = V.sel("rect", { x: P.x, y: P.y, width: P.w, height: P.h, fill: "transparent" }, g);
      hit.addEventListener("mousemove", tipCD);
      hit.addEventListener("mouseleave", function () { dom.hideTip(); dom.cursor.style.opacity = 0; });
    }

    function tipSpine(ev) {
      var d = this.__d;
      var h = "<div class='vcdi-tt-title'>" + V.esc(D.xname) + " in [" + fmtB(d.a) + ", " + fmtB(d.b) + ")</div><table>" +
        "<tr><td>Bin size</td><td class='num'>" + d.t + "</td></tr>";
      D.levels.forEach(function (l, k) { h += "<tr class='" + (k === d.k ? "sel" : "") + "'><td>" + V.esc(l) + "</td></tr>"; });
      h = "<div class='vcdi-tt-title'>" + V.esc(D.xname) + " in [" + fmtB(d.a) + ", " + fmtB(d.b) + ")</div>" +
        "<b>" + V.esc(D.yname) + " = " + V.esc(D.levels[d.k]) + "</b>: " + d.c + " of " + d.t + " (" + (100 * d.c / d.t).toFixed(1) + "%)";
      dom.showTip(h, ev);
    }
    function tipCD(ev) {
      var E = S.cd.E, P = S.cd.P, box = dom.svg.getBoundingClientRect();
      var mx = ev.clientX - box.left, t = (mx - P.x) / P.w, gi = Math.max(0, Math.min(E.grid.length - 1, Math.round(t * (E.grid.length - 1))));
      dom.cursor.setAttribute("x1", mx); dom.cursor.setAttribute("x2", mx); dom.cursor.style.opacity = 1;
      var h = "<div class='vcdi-tt-title'>" + V.esc(D.xname) + " = " + fmtB(E.grid[gi]) + "</div><table>";
      for (var k = D.levels.length - 1; k >= 0; k--)
        h += "<tr><td><span class='vcdi-swatch' style='background:" + D.fill[k] + "'></span>P(" + V.esc(D.levels[k]) + " | x)</td><td class='num'>" + (100 * E.P[gi][k]).toFixed(1) + "%</td></tr>";
      h += "</table><div class='vcdi-note-html'>Gaussian kernel, h = " + V.fmt(E.h, 3) + "</div>";
      dom.showTip(h, ev);
    }

    function resize(w, h) {
      if (!D) return;
      var sz = dom.size(w, h); S.W = sz.W; S.H = sz.H; draw();
    }
    return {
      renderValue: function (x) { render(x); },
      resize: function (w, h) { width = w; height = h; resize(w, h); }
    };
  }
});
