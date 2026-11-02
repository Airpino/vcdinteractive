/* vcdinteractive: helpers shared by the widgets other than imosaic */
(function (global) {
  "use strict";
  var SVGNS = "http://www.w3.org/2000/svg";

  function el(tag, attrs, parent, ns) {
    var e = ns ? document.createElementNS(SVGNS, tag) : document.createElement(tag);
    if (attrs) for (var k in attrs) {
      if (attrs[k] === undefined || attrs[k] === null) continue;
      if (k === "text") e.textContent = attrs[k];
      else if (k === "html") e.innerHTML = attrs[k];
      else if (k === "class") e.setAttribute("class", attrs[k]);
      else e.setAttribute(k, attrs[k]);
    }
    if (parent) parent.appendChild(e);
    return e;
  }
  function sel(tag, attrs, parent) { return el(tag, attrs, parent, true); }
  function fmt(v, d) {
    if (v === null || v === undefined || isNaN(v)) return "–";
    return Number(v).toFixed(d === undefined ? 2 : d);
  }
  function fmtCount(v) {
    if (v === null || v === undefined || isNaN(v)) return "–";
    return Math.abs(v - Math.round(v)) < 1e-9 ? String(Math.round(v)) : Number(v).toFixed(1);
  }
  function fmtP(p) {
    if (p === null || p === undefined || isNaN(p)) return "–";
    if (p < 0.0001) return "<0.0001";
    return p < 0.001 ? p.toExponential(1) : p.toFixed(4);
  }
  function signed(v, d) { return (v > 0 ? "+" : "") + fmt(v, d); }
  function esc(s) {
    return String(s).replace(/[&<>"]/g, function (c) { return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]; });
  }
  function arr(v) { return Array.isArray(v) ? v : (v === null || v === undefined ? [] : [v]); }
  function sum(a) { var t = 0; for (var i = 0; i < a.length; i++) t += a[i]; return t; }
  function ease(t) { return t < .5 ? 4 * t * t * t : 1 - Math.pow(-2 * t + 2, 3) / 2; }

  function padjust(p, method) {
    var n = p.length, out = new Array(n), idx = [], i, m;
    for (i = 0; i < n; i++) { out[i] = NaN; if (p[i] !== null && !isNaN(p[i])) idx.push(i); }
    m = idx.length;
    if (method === "none") { idx.forEach(function (j) { out[j] = p[j]; }); return out; }
    if (method === "bonferroni") { idx.forEach(function (j) { out[j] = Math.min(1, p[j] * m); }); return out; }
    if (method === "BH") {
      idx.sort(function (a, b) { return p[b] - p[a]; });
      var cm = 1;
      idx.forEach(function (j, k) { cm = Math.min(cm, Math.min(1, p[j] * m / (m - k))); out[j] = cm; });
      return out;
    }
    idx.sort(function (a, b) { return p[a] - p[b]; });
    var run = 0;
    idx.forEach(function (j, k) { run = Math.max(run, Math.min(1, (m - k) * p[j])); out[j] = run; });
    return out;
  }

  var ctx = null;
  function textWidth(s, font) {
    if (!ctx) ctx = document.createElement("canvas").getContext("2d");
    ctx.font = font || "11px system-ui, sans-serif";
    return ctx.measureText(s).width;
  }
  function fitText(s, space, font) {
    s = String(s);
    if (textWidth(s, font) <= space) return s;
    for (var k = s.length - 1; k > 0; k--) {
      var t = s.slice(0, k) + "…";
      if (textWidth(t, font) <= space) return t;
    }
    return null;
  }

  /* residual shading as vcd::shading_hcl */
  function shader(settings) {
    var b = arr(settings.breaks), col = settings.colors;
    var pos = arr(col.pos), neg = arr(col.neg);
    return function (r) {
      if (r === null || r === undefined || isNaN(r)) return col.neutral;
      var a = Math.abs(r), k = -1;
      for (var j = 0; j < b.length; j++) if (a >= b[j]) k = j;
      if (k < 0) return col.neutral;
      return r > 0 ? pos[k] : neg[k];
    };
  }
  function residualLegend(svg, settings) {
    svg.innerHTML = "";
    var b = arr(settings.breaks), col = settings.colors, pos = arr(col.pos), neg = arr(col.neg);
    var cols = [], edges = [], j;
    for (j = b.length - 1; j >= 0; j--) { cols.push(neg[j]); edges.push(-b[j]); }
    cols.push(col.neutral);
    for (j = 0; j < b.length; j++) { cols.push(pos[j]); edges.push(b[j]); }
    var w = 260 / cols.length;
    cols.forEach(function (c, k) { sel("rect", { x: 4 + k * w, y: 4, width: w, height: 12, fill: c, stroke: "#444", "stroke-width": .5 }, svg); });
    edges.forEach(function (e, k) {
      var t = sel("text", { x: 4 + (k + 1) * w, y: 28, "text-anchor": "middle" }, svg);
      t.textContent = (e > 0 ? "+" : "") + e;
    });
  }

  var PALETTE = ["#2a6fdb", "#e3622c", "#2e9e5b", "#9b51c9", "#c9a227", "#d2427f", "#1e9eb0", "#7a6a55", "#5b6bd6", "#8aa31f"];

  /* standard frame: header, toolbar, plot area, side panel, tooltip */
  function frame(container, opts) {
    container.innerHTML = "";
    var d = {};
    d.root = el("div", { "class": "vcdi-root" }, container);
    var header = el("div", { "class": "vcdi-header" }, d.root);
    if (opts.main) el("div", { "class": "vcdi-title", text: opts.main }, header);
    d.info = el("div", { "class": "vcdi-model" }, header);
    d.toolbar = el("div", { "class": "vcdi-toolbar" }, d.root);
    d.body = el("div", { "class": "vcdi-body" }, d.root);
    d.plot = el("div", { "class": "vcdi-plot" }, d.body);
    d.svg = sel("svg", { role: "img" }, d.plot);
    if (opts.side !== false) d.side = el("div", { "class": "vcdi-side" }, d.body);
    d.tip = el("div", { "class": "vcdi-tooltip" }, d.root);
    d.showTip = function (html, ev) {
      d.tip.innerHTML = html;
      var box = d.root.getBoundingClientRect();
      var x = ev.clientX - box.left + 14, y = ev.clientY - box.top + 14;
      d.tip.style.opacity = 1;
      var tw = d.tip.offsetWidth, th = d.tip.offsetHeight;
      if (x + tw > box.width - 4) x = ev.clientX - box.left - tw - 14;
      if (y + th > box.height - 4) y = Math.max(4, ev.clientY - box.top - th - 14);
      d.tip.style.left = Math.max(4, x) + "px";
      d.tip.style.top = y + "px";
    };
    d.hideTip = function () { d.tip.style.opacity = 0; };
    d.size = function (w, h) {
      var narrow = w < 720 && opts.side !== false;
      d.root.classList.toggle("vcdi-narrow", narrow);
      var top = d.body.getBoundingClientRect().top - d.root.getBoundingClientRect().top;
      var W, H;
      if (opts.side === false) { W = w; H = Math.max(200, h - top); }
      else if (narrow) { W = w; H = Math.max(260, Math.min(w, h - top - 10)); }
      else { W = Math.max(200, w - 291); H = Math.max(200, h - top); }
      d.svg.setAttribute("width", W); d.svg.setAttribute("height", H);
      d.svg.setAttribute("viewBox", "0 0 " + W + " " + H);
      return { W: W, H: H };
    };
    return d;
  }

  /* segmented control: options = [{value, label, title}] */
  function segmented(parent, options, value, onChange, cls) {
    var wrap = el("div", { "class": cls || "vcdi-modes" }, parent), btns = {};
    options.forEach(function (o) {
      var b = el("button", { type: "button", "class": "vcdi-mode-btn", text: o.label, title: o.title || o.label }, wrap);
      b.addEventListener("click", function () { set(o.value); onChange(o.value); });
      btns[o.value] = b;
    });
    function set(v) { for (var k in btns) btns[k].classList.toggle("vcdi-on", String(k) === String(v)); }
    set(value);
    return { set: set, el: wrap };
  }

  /* axis ticks */
  function niceTicks(lo, hi, n) {
    var span = hi - lo; if (span <= 0) return [lo];
    var step = Math.pow(10, Math.floor(Math.log10(span / (n || 5)))), err = (n || 5) / span * step;
    if (err <= .15) step *= 10; else if (err <= .35) step *= 5; else if (err <= .75) step *= 2;
    var out = [], dec = Math.max(0, -Math.floor(Math.log10(step) + 1e-9));
    for (var t = Math.ceil(lo / step - 1e-9); t * step <= hi + 1e-9; t++) out.push(+(t * step).toFixed(dec));
    return out;
  }

  global.VCDI = {
    el: el, sel: sel, fmt: fmt, fmtCount: fmtCount, fmtP: fmtP, signed: signed, esc: esc, arr: arr,
    sum: sum, ease: ease, padjust: padjust, textWidth: textWidth, fitText: fitText,
    shader: shader, residualLegend: residualLegend, frame: frame, segmented: segmented,
    niceTicks: niceTicks, PALETTE: PALETTE,
    residualNames: { pearson: "Pearson", standardized: "Std. adj.", deviance: "Deviance",
                     freeman_tukey: "Freeman–Tukey", anscombe: "Anscombe", robust: "Robust" },
    adjustNames: { holm: "Holm", bonferroni: "Bonferroni", BH: "Benjamini–Hochberg (FDR)", none: "None" }
  };
})(window);
