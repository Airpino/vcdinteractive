/* vcdinteractive: fourfold display with linked log odds ratio plot */
HTMLWidgets.widget({
  name: "ifourfold",
  type: "output",
  factory: function (container, width, height) {
    var V = window.VCDI, D, S, dom;
    var Z = { "0.9": 1.6448536, "0.95": 1.959964, "0.99": 2.5758293 };
    var COL = { pos: "#2a6fdb", posL: "#a9c4f0", neg: "#d6404e", negL: "#f0aab1" };

    function render(x) {
      D = x;
      D.row.levels = V.arr(D.row.levels); D.col.levels = V.arr(D.col.levels);
      D.strata = V.arr(D.strata);
      D.strata.forEach(function (s) { s.n = V.arr(s.n); });
      S = { std: D.settings.std, conf: String(D.settings.conf), rings: true, hover: -1 };
      if (!Z[S.conf]) S.conf = "0.95";
      dom = V.frame(container, { main: D.main, side: false });
      var t = D.tests, info = [];
      info.push(D.row.name + " × " + D.col.name + (D.stratum ? " by " + D.stratum : ""));
      if (t && t.woolf) info.push("Woolf test of homogeneity: χ² = " + V.fmt(t.woolf.chisq, 2) + ", df = " + t.woolf.df + ", p = " + V.fmtP(t.woolf.p));
      if (t && t.mh) info.push("Mantel–Haenszel common OR = " + V.fmt(Math.exp(t.mh.logor), 3) + " [" + V.fmt(Math.exp(t.mh.lo), 3) + ", " + V.fmt(Math.exp(t.mh.hi), 3) + "]");
      dom.info.textContent = info.join("   ·   ");

      V.el("span", { "class": "vcdi-toolbar-label", text: "Standardize:" }, dom.toolbar);
      V.segmented(dom.toolbar, [
        { value: "margins", label: "Equal margins", title: "Both margins equalized: the areas show only the odds ratio" },
        { value: "ind.max", label: "Each stratum", title: "Each stratum scaled to its largest cell" },
        { value: "all.max", label: "All strata", title: "All strata scaled to the largest cell overall" }],
        S.std, function (v) { S.std = v; draw(); });
      var cb = V.el("label", { "class": "vcdi-toolbar-label" }, dom.toolbar);
      var ci = V.el("input", { type: "checkbox" }, cb); ci.checked = true;
      cb.appendChild(document.createTextNode(" confidence rings"));
      ci.addEventListener("change", function () { S.rings = this.checked; draw(); });
      V.el("span", { "class": "vcdi-toolbar-label", text: "level" }, dom.toolbar);
      var sc = V.el("select", {}, dom.toolbar);
      ["0.9", "0.95", "0.99"].forEach(function (z) { var o = V.el("option", { value: z, text: z }, sc); if (z === S.conf) o.selected = true; });
      sc.addEventListener("change", function () { S.conf = this.value; draw(); });
      dom.g = V.sel("g", {}, dom.svg);
      resize(width, height);
    }

    function stats(s) {
      var z = Z[S.conf], lo = s.logor - z * s.se, hi = s.logor + z * s.se;
      return { lo: lo, hi: hi, sig: lo > 0 || hi < 0 };
    }
    function aOf(lor) { var q = Math.sqrt(Math.exp(lor)); return 0.5 * q / (1 + q); }
    function quad(cx, cy, r, k, open) {
      var p = [[cx - r, cy, cx, cy - r], [cx, cy - r, cx + r, cy], [cx, cy + r, cx - r, cy], [cx + r, cy, cx, cy + r]][k];
      var arc = "M" + p[0] + " " + p[1] + "A" + r + " " + r + " 0 0 1 " + p[2] + " " + p[3];
      return open ? arc : "M" + cx + " " + cy + "L" + p[0] + " " + p[1] + "A" + r + " " + r + " 0 0 1 " + p[2] + " " + p[3] + "Z";
    }

    function draw() {
      var g = dom.g; g.innerHTML = "";
      var W = S.W, H = S.H, k = D.strata.length;
      var wide = W > 640, fw = wide ? W * (k > 1 ? 0.64 : 0.7) : W, fh = wide ? H : H * 0.62;
      var ncol = Math.max(1, Math.ceil(Math.sqrt(k * fw / fh))), nrow = Math.ceil(k / ncol);
      while (ncol > 1 && Math.ceil(k / (ncol - 1)) * (fw / (ncol - 1)) <= fh) { ncol--; nrow = Math.ceil(k / ncol); }
      var cs = Math.min(fw / ncol, fh / nrow);
      var maxAll = 0;
      D.strata.forEach(function (s) { maxAll = Math.max(maxAll, Math.max.apply(null, s.n)); });
      dom.panels = [];
      D.strata.forEach(function (s, j) {
        var col = j % ncol, row = Math.floor(j / ncol);
        var x0 = col * cs, y0 = row * cs, gp = V.sel("g", { "class": "vcdi-ff" }, g);
        dom.panels.push(gp);
        var pad = 26, size = cs - 2 * pad, cx = x0 + cs / 2, cy = y0 + cs / 2 + 6;
        var R0 = size * 0.34;
        V.sel("rect", { x: x0 + 4, y: y0 + 4, width: cs - 8, height: cs - 8, "class": "vcdi-ff-frame" }, gp);
        var st = stats(s), pos = s.logor >= 0;
        var cDiag = pos ? (st.sig ? COL.pos : COL.posL) : (st.sig ? COL.neg : COL.negL);
        var cOff = pos ? (st.sig ? COL.neg : COL.negL) : (st.sig ? COL.pos : COL.posL);
        // quadrant k: 0 TL = n11, 1 TR = n12, 2 BL = n21, 3 BR = n22  (n is column-major)
        var cnt = [s.n[0], s.n[2], s.n[1], s.n[3]], diag = [true, false, false, true], radii = [];
        var mx = S.std === "all.max" ? maxAll : Math.max.apply(null, cnt);
        for (var q = 0; q < 4; q++) {
          var r;
          if (S.std === "margins") { var a = aOf(s.logor); r = R0 * Math.sqrt(4 * (diag[q] ? a : 0.5 - a)); }
          else r = R0 * Math.SQRT2 * Math.sqrt(mx > 0 ? cnt[q] / mx : 0);
          radii.push(r);
          V.sel("path", { d: quad(cx, cy, Math.max(0, r), q), fill: diag[q] ? cDiag : cOff, "class": "vcdi-ff-q" }, gp);
        }
        V.sel("circle", { cx: cx, cy: cy, r: R0, "class": "vcdi-ff-ref" }, gp);
        V.sel("line", { x1: cx - R0 * 1.45, x2: cx + R0 * 1.45, y1: cy, y2: cy, "class": "vcdi-ff-axis" }, gp);
        V.sel("line", { x1: cx, x2: cx, y1: cy - R0 * 1.45, y2: cy + R0 * 1.45, "class": "vcdi-ff-axis" }, gp);
        if (S.rings && S.std === "margins") {
          [st.lo, st.hi].forEach(function (b) {
            var a = aOf(b);
            for (var q = 0; q < 4; q++) {
              var rr = R0 * Math.sqrt(4 * (diag[q] ? a : 0.5 - a));
              V.sel("path", { d: quad(cx, cy, rr, q, true), "class": "vcdi-ff-ring" }, gp);
            }
          });
        }
        // labels and counts
        var lab = function (txt, x, y, anchor, cls) {
          var t = V.sel("text", { x: x, y: y, "text-anchor": anchor || "middle", "class": cls || "vcdi-label" }, gp);
          t.textContent = txt; return t;
        };
        if (s.label) lab(V.fitText(s.label, cs - 12) || "", cx, y0 + 18, "middle", "vcdi-varname");
        var off = R0 * 1.42;
        lab(V.fitText(D.row.levels[0], cs - 20) || "", cx, cy - off - 4);
        lab(V.fitText(D.row.levels[1], cs - 20) || "", cx, cy + off + 12);
        var tl = V.sel("text", { transform: "translate(" + (cx - off - 6) + "," + cy + ") rotate(-90)", "text-anchor": "middle", "class": "vcdi-label" }, gp);
        tl.textContent = V.fitText(D.col.levels[0], size) || "";
        var tr = V.sel("text", { transform: "translate(" + (cx + off + 6) + "," + cy + ") rotate(90)", "text-anchor": "middle", "class": "vcdi-label" }, gp);
        tr.textContent = V.fitText(D.col.levels[1], size) || "";
        var cpos = [[-1, -1], [1, -1], [-1, 1], [1, 1]];
        for (q = 0; q < 4; q++) {
          lab(V.fmtCount(cnt[q]), cx + cpos[q][0] * R0 * 1.12, cy + cpos[q][1] * R0 * 1.12 + 4, cpos[q][0] < 0 ? "end" : "start", "vcdi-ff-count");
        }
        var hit = V.sel("rect", { x: x0 + 4, y: y0 + 4, width: cs - 8, height: cs - 8, fill: "transparent" }, gp);
        hit.addEventListener("mousemove", function (ev) { hover(j, ev); });
        hit.addEventListener("mouseleave", unhover);
      });
      // forest plot of the log odds ratios
      var fx, fy, fwid, fhei;
      if (wide) { fx = fw + 30; fy = 30; fwid = W - fw - 50; fhei = Math.min(H - 70, 34 * k + 40); }
      else { fx = 90; fy = fh + 30; fwid = W - 110; fhei = Math.min(H - fh - 60, 30 * k + 40); }
      drawForest(g, fx, fy, fwid, fhei);
      applyHover();
    }

    function drawForest(g, x0, y0, w, h) {
      var k = D.strata.length, z = Z[S.conf], lo = 0, hi = 0;
      D.strata.forEach(function (s) { lo = Math.min(lo, s.logor - z * s.se); hi = Math.max(hi, s.logor + z * s.se); });
      var mh = D.tests && D.tests.mh;
      if (mh) { lo = Math.min(lo, mh.lo); hi = Math.max(hi, mh.hi); }
      var padv = (hi - lo) * 0.06 || 0.5; lo -= padv; hi += padv;
      var labW = 0; D.strata.forEach(function (s) { labW = Math.max(labW, V.textWidth(s.label || "")); });
      labW = Math.min(labW + 10, w * 0.35);
      var px = x0 + labW, pw = Math.max(40, w - labW), rowH = (h - 30) / Math.max(1, k);
      var sx = function (v) { return px + (v - lo) / (hi - lo) * pw; };
      var gf = V.sel("g", {}, g);
      var title = V.sel("text", { x: px + pw / 2, y: y0 - 10, "text-anchor": "middle", "class": "vcdi-varname" }, gf);
      title.textContent = "log odds ratio (" + Math.round(100 * +S.conf) + "% CI)";
      var ax = V.sel("g", { "class": "vcdi-axis" }, gf), yb = y0 + rowH * k + 6;
      V.niceTicks(lo, hi, 5).forEach(function (t) {
        V.sel("line", { x1: sx(t), x2: sx(t), y1: y0, y2: yb, "class": "vcdi-gridline" }, ax);
        var tt = V.sel("text", { x: sx(t), y: yb + 14, "text-anchor": "middle" }, ax); tt.textContent = t;
        var to = V.sel("text", { x: sx(t), y: yb + 26, "text-anchor": "middle" }, ax); to.textContent = "OR " + V.fmt(Math.exp(t), Math.exp(t) < 10 ? 2 : 0);
      });
      V.sel("line", { x1: sx(0), x2: sx(0), y1: y0, y2: yb, "class": "vcdi-ff-zero" }, gf);
      if (mh) {
        V.sel("line", { x1: sx(mh.logor), x2: sx(mh.logor), y1: y0, y2: yb, "class": "vcdi-ff-mh" }, gf);
        var mt = V.sel("text", { x: sx(mh.logor) + 4, y: y0 + 10, "class": "vcdi-note" }, gf); mt.textContent = "MH";
      }
      dom.rows = [];
      D.strata.forEach(function (s, j) {
        var yy = y0 + rowH * (j + .5), st = stats(s), c = s.logor >= 0 ? (st.sig ? COL.pos : COL.posL) : (st.sig ? COL.neg : COL.negL);
        var gr = V.sel("g", { "class": "vcdi-ff-row" }, gf);
        dom.rows.push(gr);
        V.sel("rect", { x: x0, y: yy - rowH / 2, width: w, height: rowH, "class": "vcdi-ff-rowbg" }, gr);
        var lt = V.sel("text", { x: px - 8, y: yy + 4, "text-anchor": "end", "class": "vcdi-label" }, gr);
        lt.textContent = V.fitText(s.label || "(all)", labW - 8) || "";
        V.sel("line", { x1: sx(st.lo), x2: sx(st.hi), y1: yy, y2: yy, stroke: c, "stroke-width": 2.5 }, gr);
        V.sel("circle", { cx: sx(s.logor), cy: yy, r: 4.5, fill: c, stroke: "#222", "stroke-width": .6 }, gr);
        gr.addEventListener("mousemove", function (ev) { hover(j, ev); });
        gr.addEventListener("mouseleave", unhover);
      });
    }

    function hover(j, ev) {
      var s = D.strata[j], st = stats(s), n = s.n;
      S.hover = j; applyHover();
      var h = "<div class='vcdi-tt-title'>" + V.esc(s.label || "Table") + "</div><table class='vcdi-tt-2x2'><tr><td></td><td><b>" +
        V.esc(D.col.levels[0]) + "</b></td><td><b>" + V.esc(D.col.levels[1]) + "</b></td></tr>" +
        "<tr><td><b>" + V.esc(D.row.levels[0]) + "</b></td><td class='num'>" + V.fmtCount(n[0]) + "</td><td class='num'>" + V.fmtCount(n[2]) + "</td></tr>" +
        "<tr><td><b>" + V.esc(D.row.levels[1]) + "</b></td><td class='num'>" + V.fmtCount(n[1]) + "</td><td class='num'>" + V.fmtCount(n[3]) + "</td></tr></table>" +
        "<table style='margin-top:6px'><tr><td>Odds ratio</td><td class='num'><b>" + V.fmt(Math.exp(s.logor), 3) + "</b></td></tr>" +
        "<tr><td>" + Math.round(100 * +S.conf) + "% CI</td><td class='num'>[" + V.fmt(Math.exp(st.lo), 3) + ", " + V.fmt(Math.exp(st.hi), 3) + "]</td></tr>" +
        "<tr><td>log OR (SE)</td><td class='num'>" + V.fmt(s.logor, 3) + " (" + V.fmt(s.se, 3) + ")</td></tr>" +
        "<tr><td>p (OR = 1)</td><td class='num'>" + V.fmtP(s.p) + "</td></tr></table>";
      if (n.some(function (v) { return v === 0; })) h += "<div class='vcdi-note-html'>0.5 added to all cells (empty cell).</div>";
      dom.showTip(h, ev);
    }
    function unhover() { S.hover = -1; applyHover(); dom.hideTip(); }
    function applyHover() {
      (dom.panels || []).forEach(function (p, j) { p.classList.toggle("vcdi-fade", S.hover >= 0 && j !== S.hover); });
      (dom.rows || []).forEach(function (p, j) { p.classList.toggle("vcdi-cur", j === S.hover); });
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
