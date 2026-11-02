/* vcdinteractive: correspondence analysis map */
HTMLWidgets.widget({
  name: "imcaplot",
  type: "output",
  factory: function (container, width, height) {
    var V = window.VCDI, D, S, dom;

    function render(x) {
      D = x;
      D.vars = V.arr(D.vars); D.points = V.arr(D.points); D.inertia = V.arr(D.inertia); D.percent = V.arr(D.percent);
      D.points.forEach(function (p) { p.coord = V.arr(p.coord); p.contrib = V.arr(p.contrib); p.cos2 = V.arr(p.cos2); });
      var dims = V.arr(D.settings.dims), k = D.inertia.length;
      S = { dx: dims[0] || 0, dy: Math.min(k - 1, dims.length > 1 ? dims[1] : 1), lines: D.settings.lines,
            hidden: D.vars.map(function () { return false; }), zoom: 1, cx: 0, cy: 0, hover: -1 };
      dom = V.frame(container, { main: D.main, side: false });
      dom.info.textContent = D.method + "   ·   " + D.inertia.map(function (v, i) {
        return "Dim " + (i + 1) + ": " + V.fmt(D.percent[i], 1) + "%"; }).join("  ");
      var mk = function (lab, val, cb) {
        V.el("span", { "class": "vcdi-toolbar-label", text: lab }, dom.toolbar);
        var s = V.el("select", {}, dom.toolbar);
        D.inertia.forEach(function (_, i) { var o = V.el("option", { value: i, text: "Dim " + (i + 1) }, s); if (i === val) o.selected = true; });
        s.addEventListener("change", function () { cb(+this.value); resetView(); draw(); });
      };
      mk("x:", S.dx, function (v) { S.dx = v; });
      mk("y:", S.dy, function (v) { S.dy = v; });
      var cl = V.el("label", { "class": "vcdi-toolbar-label" }, dom.toolbar);
      var cb = V.el("input", { type: "checkbox" }, cl); cb.checked = S.lines;
      cl.appendChild(document.createTextNode(" join categories"));
      cb.addEventListener("change", function () { S.lines = this.checked; draw(); });
      dom.legend = V.el("span", { "class": "vcdi-chips" }, dom.toolbar);
      D.vars.forEach(function (v, g) {
        var chip = V.el("span", { "class": "vcdi-legchip", title: "Show / hide " + v }, dom.legend);
        V.el("span", { "class": "vcdi-dot", style: "background:" + V.PALETTE[g % V.PALETTE.length] }, chip);
        chip.appendChild(document.createTextNode(v));
        chip.addEventListener("click", function () { S.hidden[g] = !S.hidden[g]; chip.classList.toggle("vcdi-off", S.hidden[g]); draw(); });
      });
      V.el("span", { "class": "vcdi-mode-hint", text: "Wheel: zoom · drag: pan · double click: reset" }, dom.toolbar);
      dom.g = V.sel("g", {}, dom.svg);
      // zoom & pan
      dom.svg.addEventListener("wheel", function (ev) {
        ev.preventDefault();
        var f = ev.deltaY < 0 ? 1.15 : 1 / 1.15, p = toData(ev);
        S.cx = p[0] - (p[0] - S.cx) / f; S.cy = p[1] - (p[1] - S.cy) / f; S.zoom *= f; draw();
      }, { passive: false });
      var drag = null;
      dom.svg.addEventListener("mousedown", function (ev) { drag = { x: ev.clientX, y: ev.clientY, cx: S.cx, cy: S.cy }; });
      window.addEventListener("mouseup", function () { drag = null; });
      dom.svg.addEventListener("mousemove", function (ev) {
        if (!drag) return;
        S.cx = drag.cx - (ev.clientX - drag.x) / S.scale; S.cy = drag.cy + (ev.clientY - drag.y) / S.scale; draw();
      });
      dom.svg.addEventListener("dblclick", function () { resetView(); draw(); });
      resetView();
      resize(width, height);
    }

    function visible() { return D.points.filter(function (p) { return !S.hidden[p.group]; }); }
    function resetView() {
      var pts = visible(), xs = pts.map(function (p) { return p.coord[S.dx]; }), ys = pts.map(function (p) { return p.coord[S.dy]; });
      if (!xs.length) { xs = [0]; ys = [0]; }
      var x0 = Math.min.apply(null, xs.concat([0])), x1 = Math.max.apply(null, xs.concat([0]));
      var y0 = Math.min.apply(null, ys.concat([0])), y1 = Math.max.apply(null, ys.concat([0]));
      S.cx = (x0 + x1) / 2; S.cy = (y0 + y1) / 2; S.span = [Math.max(x1 - x0, 1e-6), Math.max(y1 - y0, 1e-6)]; S.zoom = 1;
    }
    function toData(ev) {
      var b = dom.svg.getBoundingClientRect(), mx = ev.clientX - b.left, my = ev.clientY - b.top;
      return [S.cx + (mx - S.P.x - S.P.w / 2) / S.scale, S.cy - (my - S.P.y - S.P.h / 2) / S.scale];
    }

    function draw() {
      var g = dom.g; g.innerHTML = "";
      var W = S.W, H = S.H, P = { x: 40, y: 14, w: W - 60, h: H - 54 };
      S.P = P;
      // equal scale on both axes
      S.scale = Math.min(P.w / (S.span[0] * 1.18), P.h / (S.span[1] * 1.18)) * S.zoom;
      var sx = function (v) { return P.x + P.w / 2 + (v - S.cx) * S.scale; };
      var sy = function (v) { return P.y + P.h / 2 - (v - S.cy) * S.scale; };
      V.sel("rect", { x: P.x, y: P.y, width: P.w, height: P.h, "class": "vcdi-ca-frame" }, g);
      var clip = "vcdi-clip-" + Math.random().toString(36).slice(2);
      var defs = V.sel("defs", {}, g), cp = V.sel("clipPath", { id: clip }, defs);
      V.sel("rect", { x: P.x, y: P.y, width: P.w, height: P.h }, cp);
      var inner = V.sel("g", { "clip-path": "url(#" + clip + ")" }, g);
      V.sel("line", { x1: P.x, x2: P.x + P.w, y1: sy(0), y2: sy(0), "class": "vcdi-ca-axis" }, inner);
      V.sel("line", { x1: sx(0), x2: sx(0), y1: P.y, y2: P.y + P.h, "class": "vcdi-ca-axis" }, inner);
      var xl = V.sel("text", { x: P.x + P.w / 2, y: H - 8, "text-anchor": "middle", "class": "vcdi-varname" }, g);
      xl.textContent = "Dim " + (S.dx + 1) + " (" + V.fmt(D.percent[S.dx], 1) + "%)";
      var yl = V.sel("text", { transform: "translate(14," + (P.y + P.h / 2) + ") rotate(-90)", "text-anchor": "middle", "class": "vcdi-varname" }, g);
      yl.textContent = "Dim " + (S.dy + 1) + " (" + V.fmt(D.percent[S.dy], 1) + "%)";
      // lines joining the categories of each variable
      if (S.lines) D.vars.forEach(function (_, gi) {
        if (S.hidden[gi]) return;
        var pts = D.points.filter(function (p) { return p.group === gi; })
          .map(function (p) { return sx(p.coord[S.dx]).toFixed(1) + "," + sy(p.coord[S.dy]).toFixed(1); });
        V.sel("polyline", { points: pts.join(" "), stroke: V.PALETTE[gi % V.PALETTE.length], "class": "vcdi-ca-line" }, inner);
      });
      // points, larger mass first so that small points stay on top
      var order = D.points.map(function (_, i) { return i; }).sort(function (a, b) { return D.points[b].mass - D.points[a].mass; });
      var maxMass = Math.max.apply(null, D.points.map(function (p) { return p.mass; }));
      dom.labels = []; var boxes = [];
      order.forEach(function (i) {
        var p = D.points[i]; if (S.hidden[p.group]) return;
        var x = sx(p.coord[S.dx]), y = sy(p.coord[S.dy]), col = V.PALETTE[p.group % V.PALETTE.length];
        var r = 3 + 6 * Math.sqrt(p.mass / maxMass);
        var c = V.sel("circle", { cx: x, cy: y, r: r, fill: col, "class": "vcdi-ca-pt" }, inner);
        c.addEventListener("mousemove", function (ev) { tip(i, ev); });
        c.addEventListener("mouseleave", function () { dom.hideTip(); S.hover = -1; showLabels(); });
        var w = V.textWidth(p.label) + 4, bx = { x: x + r + 2, y: y - 8, w: w, h: 14 };
        var overlap = boxes.some(function (b) { return bx.x < b.x + b.w && b.x < bx.x + bx.w && bx.y < b.y + b.h && b.y < bx.y + bx.h; }) ||
          bx.x + bx.w > P.x + P.w || bx.y < P.y || bx.y + bx.h > P.y + P.h || bx.x < P.x;
        if (!overlap) boxes.push(bx);
        var t = V.sel("text", { x: bx.x, y: y + 4, fill: col, "class": "vcdi-ca-label" }, inner);
        t.textContent = p.label; t.__i = i; t.__shown = !overlap;
        dom.labels.push(t);
      });
      showLabels();
    }
    function showLabels() {
      (dom.labels || []).forEach(function (t) { t.style.display = t.__shown || t.__i === S.hover ? "" : "none"; });
    }
    function tip(i, ev) {
      var p = D.points[i], dx = S.dx, dy = S.dy;
      S.hover = i; showLabels();
      dom.showTip("<div class='vcdi-tt-title'>" + V.esc(D.vars[p.group]) + " = <b>" + V.esc(p.label) + "</b></div><table>" +
        "<tr><td>Mass</td><td class='num'>" + V.fmt(p.mass, 4) + "</td></tr>" +
        "<tr><td>Dim " + (dx + 1) + " / Dim " + (dy + 1) + "</td><td class='num'>" + V.fmt(p.coord[dx], 3) + " / " + V.fmt(p.coord[dy], 3) + "</td></tr>" +
        "<tr><td>Contribution (%)</td><td class='num'>" + V.fmt(100 * p.contrib[dx], 1) + " / " + V.fmt(100 * p.contrib[dy], 1) + "</td></tr>" +
        "<tr><td>Quality (cos²)</td><td class='num'>" + V.fmt(p.cos2[dx] + p.cos2[dy], 3) + "</td></tr></table>", ev);
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
