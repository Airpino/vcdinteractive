/* vcdinteractive: interactive mosaic plot (no external dependencies) */
(function () {
  "use strict";

  var SVGNS = "http://www.w3.org/2000/svg";

  var I18N = {
    en: {
      order: "Variables (drag to reorder, tick to include):",
      include: "Include / remove this variable", notAvail: "This combination of variables was not precomputed: use subsets = \"all\" in imosaic().",
      atLeastOne: "At least one variable must be displayed.",
      view: "View:", modes: { mosaic: "Mosaic", assoc: "Association", sieve: "Sieve", doubledecker: "Doubledecker" },
      modeHint: { mosaic: "Tile areas \u221d observed counts; colour = residual.",
                  assoc: "Bar height \u221d residual, width \u221d \u221aexpected (area \u221d O \u2212 E for Pearson). Rows: variables split in rows; columns: variables split in columns.",
                  sieve: "Tile areas \u221d expected counts; number of small squares = observed count. Blue solid: more than expected; red dashed: fewer.",
                  doubledecker: "The last displayed variable is the response (drag another one to the end to change it); bars show its conditional distribution." },
      models: "Models", mName: "Model", aic: "AIC", response: "Response",
      condProp: "conditional proportion", colTotal: "column total", saturated: "saturated model: residuals are all zero", reset: "Reset", alternate: "Alternate h/v",
      cols: "cols", rows: "rows", toggleDir: "Toggle split direction",
      moveLeft: "Move left", moveRight: "Move right",
      residuals: "Residuals", adjust: "Correction", alpha: "α", outline: "Outline significant cells",
      legend: "Shading", sig: "Significant cells", none: "No cell is significant at this level.",
      cell: "Cell", obs: "O", exp: "E", res: "r", padj: "p adj.", csv: "CSV",
      observed: "Observed", expected: "Expected", expectedRobust: "Expected (robust fit)",
      ratio: "O/E", diff: "O − E", pval: "p", pvalAdj: "p adjusted",
      significant: "significant", over: "over-represented", under: "under-represented",
      unlabelled: "not labelled on the borders (hover the tiles):",
      model: "Model", robustFit: "robust fit",
      types: { pearson: "Pearson", standardized: "Adjusted standardized", deviance: "Deviance",
               freeman_tukey: "Freeman–Tukey", anscombe: "Anscombe", robust: "Robust (Pearson vs robust fit)" },
      short: { pearson: "Pearson", standardized: "Std. adj.", deviance: "Deviance",
               freeman_tukey: "Freeman–Tukey", anscombe: "Anscombe", robust: "Robust" },
      adj: { holm: "Holm", bonferroni: "Bonferroni", BH: "Benjamini–Hochberg (FDR)", none: "None" },
      formula: {
        pearson: "r = (o \u2212 e) / \u221ae",
        standardized: "r = (o \u2212 e) / \u221a(e(1 \u2212 h))",
        deviance: "r = sign(o \u2212 e) \u221a(2[o log(o/e) \u2212 (o \u2212 e)])",
        freeman_tukey: "r = \u221ao + \u221a(o + 1) \u2212 \u221a(4e + 1)",
        anscombe: "r = 1.5 (o^(2/3) \u2212 e^(2/3)) / e^(1/6)",
        robust: "r = (o \u2212 \u1ebd) / \u221a\u1ebd,  \u1ebd from a robust fit"
      },
      hint: {
        pearson: "Classical vcd shading; squares add up to X\u00b2. Variance < 1, so conservative for testing.",
        standardized: "Asymptotically N(0,1): the recommended choice for testing single cells.",
        deviance: "Squares add up to G\u00b2; consistent with likelihood-ratio model comparison.",
        freeman_tukey: "Variance-stabilized; preferable with small expected counts and sparse tables.",
        anscombe: "Closest to normality for Poisson counts; good with small, skewed counts.",
        robust: "Fit not pulled by outlying cells: reveals deviations masked by them. Use when the model holds for most cells."
      }
    }
  };

  function el(tag, attrs, parent, ns) {
    var e = ns ? document.createElementNS(SVGNS, tag) : document.createElement(tag);
    if (attrs) for (var k in attrs) {
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
    d = d === undefined ? 2 : d;
    return Number(v).toFixed(d);
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
  function esc(s) { return String(s).replace(/[&<>"]/g, function (c) {
    return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]; }); }

  /* multiplicity corrections (as stats::p.adjust) */
  function padjust(p, method) {
    var n = p.length, out = new Array(n), idx = [], i, m;
    for (i = 0; i < n; i++) if (p[i] !== null && !isNaN(p[i])) idx.push(i);
    m = idx.length;
    for (i = 0; i < n; i++) out[i] = NaN;
    if (method === "none") { idx.forEach(function (j) { out[j] = p[j]; }); return out; }
    if (method === "bonferroni") { idx.forEach(function (j) { out[j] = Math.min(1, p[j] * m); }); return out; }
    if (method === "holm") {
      idx.sort(function (a, b) { return p[a] - p[b]; });
      var run = 0;
      idx.forEach(function (j, k) { run = Math.max(run, Math.min(1, (m - k) * p[j])); out[j] = run; });
      return out;
    }
    if (method === "BH") {
      idx.sort(function (a, b) { return p[b] - p[a]; });
      var cm = 1;
      idx.forEach(function (j, k) { var r = m - k; cm = Math.min(cm, Math.min(1, p[j] * m / r)); out[j] = cm; });
      return out;
    }
    return padjust(p, "holm");
  }

  var measureCtx = null;
  function textWidth(s, font) {
    if (!measureCtx) measureCtx = document.createElement("canvas").getContext("2d");
    measureCtx.font = font || "11px system-ui, sans-serif";
    return measureCtx.measureText(s).width;
  }
  function fitText(s, space, font) {
    if (textWidth(s, font) <= space) return s;
    for (var k = s.length - 1; k > 0; k--) {
      var t = s.slice(0, k) + "…";
      if (textWidth(t, font) <= space) return t;
    }
    return null;
  }

  function ease(t) { return t < .5 ? 4 * t * t * t : 1 - Math.pow(-2 * t + 2, 3) / 2; }

  HTMLWidgets.widget({
    name: "imosaic",
    type: "output",
    factory: function (container, width, height) {
      var S = null;          // state
      var D = null;          // data
      var T = null;          // strings
      var dom = {};

      /* ---------------------------------------------------------- build */
      // R may serialize length-1 vectors as scalars: make sure arrays are arrays
      function arr(v) { return Array.isArray(v) ? v : (v === null || v === undefined ? [] : [v]); }
      function normalize(x) {
        x.vars.forEach(function (v) { v.levels = arr(v.levels); });
        var st = x.settings;
        ["available", "direction", "breaks", "active"].forEach(function (k) { st[k] = arr(st[k]); });
        st.colors.pos = arr(st.colors.pos); st.colors.neg = arr(st.colors.neg);
        Object.keys(x.tables).forEach(function (k) {
          var t = x.tables[k], c = t.cells;
          t.vars = arr(t.vars);
          c.idx = arr(c.idx).map(arr);
          Object.keys(c).forEach(function (f) { if (f !== "idx") c[f] = arr(c[f]); });
        });
        return x;
      }

      function build(x) {
        D = normalize(x);
        T = I18N.en;
        var nv = x.vars.length;
        S = {
          full: x.vars.map(function (_, i) { return i; }),
          active: x.settings.active.slice(),
          dirs: x.settings.direction.slice(),
          type: x.settings.residuals,
          mode: x.settings.display || "mosaic", mi: 0,
          adjust: x.settings.adjust,
          alpha: x.settings.alpha,
          outline: true,
          prev: null, anim: null, highlight: null, hoverCell: null,
          sortKey: "absres"
        };
        syncOrder();
        S.init = { full: S.full.slice(), dirs: S.dirs.slice(), active: S.active.slice() };
        S.key = fullKey();
        D.cells = D.tables[S.key].cells;
        D.model = D.tables[S.key].model;
        S.n = D.cells.observed.length;

        container.innerHTML = "";
        var root = dom.root = el("div", { "class": "vcdi-root" }, container);

        var header = el("div", { "class": "vcdi-header" }, root);
        if (x.main) el("div", { "class": "vcdi-title", text: x.main }, header);
        dom.model = el("div", { "class": "vcdi-model" }, header);

        var tbm = el("div", { "class": "vcdi-toolbar vcdi-toolbar-modes" }, root);
        el("span", { "class": "vcdi-toolbar-label", text: T.view }, tbm);
        var segm = el("div", { "class": "vcdi-modes" }, tbm);
        dom.modeBtns = {};
        ["mosaic", "assoc", "sieve", "doubledecker"].forEach(function (m) {
          var b = el("button", { type: "button", "class": "vcdi-mode-btn", text: T.modes[m] }, segm);
          b.addEventListener("click", function () { setMode(m); });
          dom.modeBtns[m] = b;
        });
        dom.modeHint = el("span", { "class": "vcdi-mode-hint" }, tbm);
        var tb = el("div", { "class": "vcdi-toolbar" }, root);
        el("span", { "class": "vcdi-toolbar-label", text: T.order }, tb);
        dom.chips = el("div", { "class": "vcdi-chips" }, tb);
        var bAlt = el("button", { "class": "vcdi-btn", text: T.alternate, type: "button" }, tb);
        bAlt.addEventListener("click", function () {
          var first = S.dirs[0];
          S.dirs = S.full.map(function (_, k) { return k % 2 === 0 ? first : (first === "h" ? "v" : "h"); });
          renderChips(); draw(true);
        });
        var bReset = el("button", { "class": "vcdi-btn", text: T.reset, type: "button" }, tb);
        bReset.addEventListener("click", function () {
          S.full = S.init.full.slice(); S.dirs = S.init.dirs.slice(); S.active = S.init.active.slice();
          syncOrder(); renderChips();
          if (fullKey() !== S.key) switchTable(fullKey()); else { draw(true); renderList(); }
        });

        var body = dom.body = el("div", { "class": "vcdi-body" }, root);
        dom.plot = el("div", { "class": "vcdi-plot" }, body);
        dom.svg = sel("svg", { role: "img" }, dom.plot);
        dom.gTiles = sel("g", {}, dom.svg);
        dom.gZero = sel("g", {}, dom.svg);
        dom.gOver = sel("g", { "class": "vcdi-overlay" }, dom.svg);
        dom.gLabels = sel("g", {}, dom.svg);
        makeTiles();
        dom.gTiles.addEventListener("mousemove", function (ev) {
          var t = ev.target;
          if (t.__cell === undefined) return;
          showTip(t.__cell, ev);
        });
        dom.gTiles.addEventListener("mouseleave", hideTip);

        var side = dom.side = el("div", { "class": "vcdi-side" }, body);
        if (x.models && x.models.length > 1) {
          el("div", { "class": "vcdi-section-title", text: T.models }, side);
          dom.models = el("div", { "class": "vcdi-models" }, side);
        }
        el("div", { "class": "vcdi-section-title", text: T.residuals }, side);
        dom.seg = el("div", { "class": "vcdi-seg", role: "radiogroup" }, side);
        dom.segBtns = {};
        x.settings.available.forEach(function (t) {
          var b = el("button", { type: "button", "class": "vcdi-seg-btn", role: "radio",
                                 title: T.types[t], text: T.short[t] }, dom.seg);
          b.addEventListener("click", function () { setType(t); });
          dom.segBtns[t] = b;
        });
        dom.hint = el("div", { "class": "vcdi-hint" }, side);
        dom.root.tabIndex = 0;
        dom.root.addEventListener("keydown", function (ev) {
          var k = parseInt(ev.key, 10), av = D.settings.available;
          if (k >= 1 && k <= av.length && !/SELECT|INPUT/.test(ev.target.tagName)) setType(av[k - 1]);
        });

        var r2 = el("div", { "class": "vcdi-row" }, side);
        el("label", { text: T.adjust }, r2);
        dom.selAdj = el("select", {}, r2);
        ["holm", "bonferroni", "BH", "none"].forEach(function (a) {
          var o = el("option", { value: a, text: T.adj[a] }, dom.selAdj);
          if (a === S.adjust) o.selected = true;
        });
        dom.selAdj.addEventListener("change", function () { S.adjust = this.value; recolor(); renderList(); });
        var r2b = el("div", { "class": "vcdi-row" }, side);
        el("label", { text: T.alpha }, r2b);
        dom.selAlpha = el("select", {}, r2b);
        var alphas = [0.1, 0.05, 0.01, 0.001];
        if (alphas.indexOf(S.alpha) < 0) alphas.push(S.alpha);
        alphas.sort(function (a, b) { return b - a; }).forEach(function (a) {
          var o = el("option", { value: a, text: String(a) }, dom.selAlpha);
          if (a === S.alpha) o.selected = true;
        });
        dom.selAlpha.addEventListener("change", function () { S.alpha = +this.value; recolor(); renderList(); });

        var r3 = el("div", { "class": "vcdi-row" }, side);
        var cbl = el("label", {}, r3);
        dom.cbOutline = el("input", { type: "checkbox" }, cbl);
        dom.cbOutline.checked = true;
        cbl.appendChild(document.createTextNode(" " + T.outline));
        dom.cbOutline.addEventListener("change", function () { S.outline = this.checked; recolor(); });

        el("div", { "class": "vcdi-section-title", text: T.legend }, side);
        dom.legend = sel("svg", { "class": "vcdi-legend", width: 270, height: 34 }, side);

        var hd = el("div", { "class": "vcdi-row" }, side);
        dom.sigTitle = el("div", { "class": "vcdi-section-title", text: T.sig }, hd);
        var bcsv = el("button", { "class": "vcdi-btn vcdi-btn-ghost", text: "⤓ " + T.csv, type: "button", title: "CSV" }, hd);
        bcsv.style.marginLeft = "auto";
        bcsv.addEventListener("click", downloadCSV);
        dom.list = el("div", { "class": "vcdi-list" }, side);

        dom.tip = el("div", { "class": "vcdi-tooltip" }, root);
        dom.flash = el("div", { "class": "vcdi-flash" }, root);

        renderModel();
        renderChips();
        for (var mk in dom.modeBtns) dom.modeBtns[mk].classList.toggle("vcdi-on", mk === S.mode);
        dom.modeHint.textContent = T.modeHint[S.mode];
        setType(S.type);
        renderLegend();
        resize(width, height);
        renderList();
      }

      /* ------------------------------------------- variable subsets */
      function fullKey() { return S.mi + "|" + tableKey(); }
      function setModel(mi) {
        if (mi === S.mi) return;
        S.mi = mi;
        var key = fullKey();
        if (!D.tables[key]) { flash(T.notAvail); return; }
        switchTable(key);
      }
      function setMode(m) {
        S.mode = m;
        for (var k in dom.modeBtns) dom.modeBtns[k].classList.toggle("vcdi-on", k === m);
        dom.modeHint.textContent = T.modeHint[m];
        renderLegend(); draw(true);
      }
      function syncOrder() { S.order = S.full.filter(function (v) { return S.active[v]; }); }
      function tableKey() {
        return S.full.map(function (v, i) { return i; }).filter(function (i) { return S.active[i]; }).join("-");
      }
      function makeTiles() {
        dom.gTiles.innerHTML = "";
        dom.tiles = [];
        for (var i = 0; i < S.n; i++) {
          var r = sel("rect", { "class": "vcdi-tile" }, dom.gTiles);
          r.__cell = i;
          dom.tiles.push(r);
        }
      }
      function flash(msg) {
        dom.flash.textContent = msg;
        dom.flash.style.opacity = 1;
        clearTimeout(S.flashT);
        S.flashT = setTimeout(function () { dom.flash.style.opacity = 0; }, 2600);
      }
      function toggleVar(v) {
        var nAct = S.active.filter(Boolean).length;
        if (S.active[v] && nAct === 1) { flash(T.atLeastOne); renderChips(); return; }
        S.active[v] = !S.active[v];
        var key = fullKey();
        if (!D.tables[key]) { S.active[v] = !S.active[v]; flash(T.notAvail); renderChips(); return; }
        syncOrder(); renderChips(); switchTable(key); renderLegend();
      }
      // new tiles start from the union of the old tiles that agree with them on
      // the variables common to both tables: splitting animates from the parent
      // tile when a variable is added, merging when it is removed
      function switchTable(key) {
        var old = { cells: D.cells, rects: S.prev, vars: D.tables[S.key].vars };
        var tab = D.tables[key];
        D.cells = tab.cells; D.model = tab.model; S.key = key;
        S.n = D.cells.observed.length; S._padj = null; S.hoverCell = null;
        var common = tab.vars.filter(function (v) { return old.vars.indexOf(v) >= 0; });
        var start = null;
        if (old.rects) {
          var groups = {};
          for (var j = 0; j < old.rects.length; j++) {
            var k = common.map(function (v) { return old.cells.idx[v][j]; }).join(",");
            var r = old.rects[j], g = groups[k];
            if (!g) groups[k] = { x0: r.x, y0: r.y, x1: r.x + r.w, y1: r.y + r.h };
            else { g.x0 = Math.min(g.x0, r.x); g.y0 = Math.min(g.y0, r.y); g.x1 = Math.max(g.x1, r.x + r.w); g.y1 = Math.max(g.y1, r.y + r.h); }
          }
          start = [];
          for (var i = 0; i < S.n; i++) {
            var kk = common.map(function (v) { return D.cells.idx[v][i]; }).join(","), b = groups[kk];
            start.push(b ? { x: b.x0, y: b.y0, w: b.x1 - b.x0, h: b.y1 - b.y0 } : { x: 0, y: 0, w: 0, h: 0 });
          }
        }
        makeTiles();
        S.prev = start;
        renderModel();
        draw(true);
        renderList();
        hideTip();
      }

      /* ------------------------------------------------------ helpers */
      function expectedFor(i) {
        return S.type === "robust" ? D.cells.expected_robust[i] : D.cells.expected[i];
      }
      function residual(i) { return D.cells[S.type][i]; }
      function adjP() {
        if (!S._padj || S._padjKey !== S.type + S.adjust + S.key) {
          S._padj = padjust(D.cells["p_" + S.type], S.adjust);
          S._padjKey = S.type + S.adjust + S.key;
        }
        return S._padj;
      }
      function color(r) {
        var b = D.settings.breaks, col = D.settings.colors, a = Math.abs(r), k = -1;
        if (r === null || isNaN(r)) return col.neutral;
        for (var j = 0; j < b.length; j++) if (a >= b[j]) k = j;
        if (k < 0) return col.neutral;
        return r > 0 ? col.pos[k] : col.neg[k];
      }
      function levelName(v, l) { return D.vars[v].levels[l]; }
      function cellLabel(i, sep) {
        return S.order.map(function (v) {
          return D.vars[v].name + " = " + levelName(v, D.cells.idx[v][i]);
        }).join(sep || ", ");
      }
      function cellShort(i) {
        return S.order.map(function (v) { return levelName(v, D.cells.idx[v][i]); }).join(" · ");
      }

      function renderModels() {
        if (!dom.models) return;
        dom.models.innerHTML = "";
        var tbl = el("table", {}, dom.models);
        var hr = el("tr", {}, el("thead", {}, tbl));
        [T.mName, "G\u00b2", "df", "p", T.aic].forEach(function (h) { el("th", { text: h }, hr); });
        var tb = el("tbody", {}, tbl), sub = tableKey();
        D.models.forEach(function (md, mi) {
          var t = D.tables[mi + "|" + sub];
          var tr = el("tr", { "class": mi === S.mi ? "vcdi-cur" : "" }, tb);
          var td = el("td", { text: t ? t.model.label : md.name }, tr);
          td.title = md.name;
          if (t) {
            el("td", { text: fmt(t.model.G2, 1) }, tr);
            el("td", { text: String(t.model.df) }, tr);
            el("td", { text: t.model.df > 0 ? fmtP(t.model.p_G2) : "\u2013" }, tr);
            el("td", { text: fmt(t.model.aic, 1) }, tr);
          } else { el("td", { text: "\u2013", colspan: 4 }, tr); }
          tr.addEventListener("click", function () { setModel(mi); });
        });
      }
      function renderModel() {
        renderModels();
        var m = D.model;
        dom.model.innerHTML = esc(T.model) + ": <code>" + esc(m.label || m.formula) + "</code> &nbsp; X² = " + fmt(m.X2, 2) +
          ", G² = " + fmt(m.G2, 2) + ", df = " + m.df + (m.df > 0 ? ", p = " + fmtP(m.p_X2) : " (" + esc(T.saturated) + ")") + " &nbsp; n = " + fmtCount(m.n) +
          (m.robust && m.robust !== "none" ? " &nbsp; (" + esc(T.robustFit) + ": " + esc(m.robust) + ")" : "");
      }

      /* ------------------------------------------------- order chips */
      function renderChips() {
        dom.chips.innerHTML = "";
        var apos = 0;
        S.full.forEach(function (v, pos) {
          var on = S.active[v], ap = on ? apos++ : -1;
          var chip = el("div", { "class": "vcdi-chip" + (on ? "" : " vcdi-chip-off"), draggable: "true" }, dom.chips);
          chip.__pos = pos;
          var cb = el("input", { type: "checkbox", title: T.include, "class": "vcdi-chip-cb" }, chip);
          cb.checked = on;
          cb.addEventListener("click", function (ev) { ev.stopPropagation(); });
          cb.addEventListener("change", function () { toggleVar(v); });
          if (on) el("span", { "class": "vcdi-chip-pos", text: (ap + 1) + "." }, chip);
          el("span", { "class": "vcdi-chip-name", text: D.vars[v].name }, chip);
          if (on) {
            var bd = el("button", { "class": "vcdi-btn", type: "button", title: T.toggleDir,
              text: S.dirs[ap] === "v" ? "\u2502\u2502 " + T.cols : "\u2550 " + T.rows }, chip);
            bd.addEventListener("click", function (ev) {
              ev.stopPropagation();
              S.dirs[ap] = S.dirs[ap] === "v" ? "h" : "v";
              renderChips(); draw(true);
            });
          }
          if (pos > 0) {
            var bl = el("button", { "class": "vcdi-btn vcdi-btn-ghost", type: "button", title: T.moveLeft, text: "◀" }, chip);
            bl.addEventListener("click", function (ev) { ev.stopPropagation(); move(pos, pos - 1); });
          }
          if (pos < S.full.length - 1) {
            var br = el("button", { "class": "vcdi-btn vcdi-btn-ghost", type: "button", title: T.moveRight, text: "▶" }, chip);
            br.addEventListener("click", function (ev) { ev.stopPropagation(); move(pos, pos + 1); });
          }
          chip.addEventListener("dragstart", function (ev) {
            S.dragFrom = pos; chip.classList.add("vcdi-dragging");
            try { ev.dataTransfer.setData("text/plain", String(pos)); ev.dataTransfer.effectAllowed = "move"; } catch (e) {}
          });
          chip.addEventListener("dragend", function () {
            chip.classList.remove("vcdi-dragging");
            Array.prototype.forEach.call(dom.chips.children, function (c) { c.classList.remove("vcdi-dropbefore", "vcdi-dropafter"); });
          });
          chip.addEventListener("dragover", function (ev) {
            ev.preventDefault();
            var rect = chip.getBoundingClientRect(), after = ev.clientX > rect.left + rect.width / 2;
            chip.classList.toggle("vcdi-dropafter", after);
            chip.classList.toggle("vcdi-dropbefore", !after);
          });
          chip.addEventListener("dragleave", function () { chip.classList.remove("vcdi-dropbefore", "vcdi-dropafter"); });
          chip.addEventListener("drop", function (ev) {
            ev.preventDefault();
            var rect = chip.getBoundingClientRect(), after = ev.clientX > rect.left + rect.width / 2;
            var from = S.dragFrom, to = pos + (after ? 1 : 0);
            if (from === undefined) return;
            if (from < to) to--;
            move(from, to);
          });
        });
      }
      // moving a variable keeps the direction attached to the position, so the
      // h/v alternation is preserved (as vcd does when variables are permuted)
      function move(from, to) {
        if (from === to) return;
        var v = S.full.splice(from, 1)[0];
        S.full.splice(to, 0, v);
        syncOrder(); renderChips(); draw(true); renderList(); renderLegend();
      }

      /* --------------------------------------------------- layout */
      function sum(a) { var t = 0; for (var i = 0; i < a.length; i++) t += a[i]; return t; }
      function computeLayout(W, H) {
        if (S.mode === "assoc") return assocLayout(W, H);
        var nv = S.order.length, c = D.cells, dd = S.mode === "doubledecker";
        var wts = S.mode === "sieve" ? (S.type === "robust" ? c.expected_robust : c.expected) : c.observed;
        var dirs = dd ? S.order.map(function (_, k) { return k === nv - 1 ? "h" : "v"; }) : S.dirs.slice(0, nv);
        return mosaicLayout(W, H, wts, dirs, dd);
      }

      function mosaicLayout(W, H, wts, dirs, dd) {
        var nv = S.order.length, n = S.n, c = D.cells;
        var base = 5, rate = 1.5, gaps = [];
        if (dd) {
          for (var k = 0; k < nv - 1; k++) gaps.push(nv - 1 < 3 ? base : base * Math.pow(rate, nv - 2 - k));
          gaps.push(0);
        } else for (k = 0; k < nv; k++) gaps.push(nv < 3 ? base : base * Math.pow(rate, nv - 1 - k));

        // which variables get border labels
        var sides = {}, vCount = 0, hCount = 0, unl = [], m;
        if (dd) {
          var font = "bold 12px system-ui, sans-serif", lw = 20;
          S.order.slice(0, -1).forEach(function (v) { lw = Math.max(lw, textWidth(D.vars[v].name, font)); });
          var resp = S.order[nv - 1], rw = 30;
          D.vars[resp].levels.forEach(function (l) { rw = Math.max(rw, textWidth(l)); });
          m = { top: 12, bottom: 14 + 16 * (nv - 1), left: nv > 1 ? Math.min(160, lw + 16) : 12,
                right: Math.min(150, rw + 34) };
        } else {
          S.order.forEach(function (v, k) {
            if (dirs[k] === "v") { if (vCount === 0) sides[k] = "top"; else if (vCount === 1) sides[k] = "bottom"; else unl.push(v); vCount++; }
            else { if (hCount === 0) sides[k] = "left"; else if (hCount === 1) sides[k] = "right"; else unl.push(v); hCount++; }
          });
          var has = { top: false, bottom: false, left: false, right: false };
          for (var key in sides) has[sides[key]] = true;
          m = { top: has.top ? 40 : 12, bottom: (has.bottom ? 40 : 12) + (unl.length ? 16 : 0),
                left: has.left ? 40 : 12, right: has.right ? 40 : 12 };
        }
        var plot = { x: m.left, y: m.top, w: Math.max(10, W - m.left - m.right), h: Math.max(10, H - m.top - m.bottom) };

        var tiles = new Array(n), groups = [], zeros = [];
        var all = []; for (var i = 0; i < n; i++) all.push(i);

        function rec(cells, depth, rect, zeroParent) {
          if (depth === nv) {
            var ci = cells[0];
            tiles[ci] = { x: rect.x, y: rect.y, w: rect.w, h: rect.h, hidden: zeroParent };
            return;
          }
          var v = S.order[depth], L = D.vars[v].levels.length, dir = dirs[depth];
          var byLev = []; for (var l = 0; l < L; l++) byLev.push([]);
          var tot = 0, sums = new Array(L).fill(0);
          cells.forEach(function (ci) { var l = c.idx[v][ci], w = wts[ci] || 0; byLev[l].push(ci); sums[l] += w; tot += w; });
          var g = gaps[depth], span = dir === "v" ? rect.w : rect.h;
          var avail = Math.max(0, span - g * (L - 1)), pos = dir === "v" ? rect.x : rect.y;
          for (l = 0; l < L; l++) {
            var size = tot > 0 ? avail * sums[l] / tot : 0;
            var child = dir === "v" ? { x: pos, y: rect.y, w: size, h: rect.h } : { x: rect.x, y: pos, w: rect.w, h: size };
            groups.push({ depth: depth, v: v, level: l, rect: child, parent: rect });
            var isZero = !zeroParent && sums[l] <= 0;
            if (isZero) zeros.push({ rect: child, dir: dir, depth: depth, v: v, level: l, cells: byLev[l] });
            rec(byLev[l], depth + 1, child, zeroParent || isZero);
            pos += size + g;
          }
        }
        rec(all, 0, plot, false);
        return { mode: S.mode, tiles: tiles, groups: groups, zeros: zeros, sides: sides, plot: plot, unl: unl, W: W, H: H };
      }

      // association plot: rows = combinations of the variables split in rows,
      // columns = combinations of those split in columns (as vcd::assoc)
      function assocLayout(W, H) {
        var c = D.cells, n = S.n, rowV = [], colV = [];
        S.order.forEach(function (v, k) { (S.dirs[k] === "v" ? colV : rowV).push(v); });
        function combos(vs) {
          var out = [[]];
          vs.forEach(function (v) {
            var L = D.vars[v].levels.length, nx = [];
            out.forEach(function (p) { for (var l = 0; l < L; l++) nx.push(p.concat([l])); });
            out = nx;
          });
          return out;
        }
        function code(vs, i) { var k = 0; vs.forEach(function (v) { k = k * D.vars[v].levels.length + c.idx[v][i]; }); return k; }
        function gapsFor(list, vs) {
          var g = [];
          for (var k = 1; k < list.length; k++) {
            var d = 0; while (d < vs.length && list[k][d] === list[k - 1][d]) d++;
            g.push(4 * Math.pow(1.7, vs.length - 1 - d));
          }
          return g;
        }
        var rows = combos(rowV), cols = combos(colV);
        var E = S.type === "robust" ? c.expected_robust : c.expected, R = c[S.type];
        var colMax = new Array(cols.length).fill(0), rowPos = new Array(rows.length).fill(0), rowNeg = new Array(rows.length).fill(0);
        var ri = [], ci = [];
        for (var i = 0; i < n; i++) {
          var a = code(rowV, i), b = code(colV, i), se = Math.sqrt(Math.max(0, E[i] || 0)), r = R[i] || 0;
          ri.push(a); ci.push(b);
          colMax[b] = Math.max(colMax[b], se);
          if (r > 0) rowPos[a] = Math.max(rowPos[a], r); else rowNeg[a] = Math.max(rowNeg[a], -r);
        }
        var gr = gapsFor(rows, rowV), gc = gapsFor(cols, colV);
        var nrd = Math.min(rowV.length, 2), ncd = Math.min(colV.length, 2);
        var m = { top: 10 + 16 * ncd, left: 10 + 16 * nrd, right: 12, bottom: 22 };
        var P = { x: m.left, y: m.top, w: Math.max(10, W - m.left - m.right), h: Math.max(10, H - m.top - m.bottom) };
        var sumC = sum(colMax), sx = sumC > 0 ? Math.max(0, P.w - sum(gc)) / sumC : 0;
        var rowH = rows.map(function (_, a) { return rowPos[a] + rowNeg[a]; });
        if (sum(rowH) <= 0) { rowH = rows.map(function () { return 1; }); rowPos = rows.map(function () { return 0.5; }); }
        var sy = Math.max(0, P.h - sum(gr)) / sum(rowH);
        var cx = [], x = P.x, colW = colMax.map(function (v) { return v * sx; });
        cols.forEach(function (_, b) { cx.push(x); x += colW[b] + (gc[b] || 0); });
        var ry = [], y = P.y, rh = rowH.map(function (h) { return h * sy; });
        rows.forEach(function (_, a) { ry.push(y); y += rh[a] + (gr[a] || 0); });
        var base = rows.map(function (_, a) { return ry[a] + rowPos[a] * sy; });
        var tiles = new Array(n);
        for (i = 0; i < n; i++) {
          var aa = ri[i], bb = ci[i], w = Math.sqrt(Math.max(0, E[i] || 0)) * sx, rr = R[i] || 0;
          var xc = cx[bb] + colW[bb] / 2;
          tiles[i] = { x: xc - w / 2, y: rr > 0 ? base[aa] - rr * sy : base[aa], w: w, h: Math.abs(rr) * sy, hidden: false };
        }
        return { mode: "assoc", tiles: tiles, rows: rows, cols: cols, rowV: rowV, colV: colV, cx: cx, colW: colW,
                 ry: ry, rh: rh, base: base, plot: P, W: W, H: H, groups: [], zeros: [], sides: {}, unl: [] };
      }

      /* ------------------------------------------------------- draw */
      function draw(animate) {
        if (!D) return;
        var W = S.W, H = S.H;
        dom.svg.setAttribute("width", W);
        dom.svg.setAttribute("height", H);
        dom.svg.setAttribute("viewBox", "0 0 " + W + " " + H);
        var L = computeLayout(W, H);
        S.layout = L;
        var from = S.prev, to = L.tiles;
        if (S.anim) cancelAnimationFrame(S.anim);
        drawLabels(L);
        drawZeros(L);
        recolor();
        if (!animate || !from) { dom.gOver.style.opacity = 1; setTiles(to, 1, to); S.prev = to; return; }
        var t0 = null, dur = 450;
        dom.gZero.style.opacity = 0; dom.gLabels.style.opacity = 0; dom.gOver.style.opacity = 0;
        function step(ts) {
          if (t0 === null) t0 = ts;
          var t = Math.min(1, (ts - t0) / dur), e = ease(t);
          setTiles(from, e, to);
          dom.gZero.style.opacity = dom.gLabels.style.opacity = dom.gOver.style.opacity = t;
          if (t < 1) S.anim = requestAnimationFrame(step);
          else { S.prev = to; S.anim = null; }
        }
        S.anim = requestAnimationFrame(step);
      }
      function setTiles(a, e, b) {
        for (var i = 0; i < S.n; i++) {
          var p = a[i], q = b[i], r = dom.tiles[i];
          r.setAttribute("x", p.x + (q.x - p.x) * e);
          r.setAttribute("y", p.y + (q.y - p.y) * e);
          r.setAttribute("width", Math.max(0, p.w + (q.w - p.w) * e));
          r.setAttribute("height", Math.max(0, p.h + (q.h - p.h) * e));
          var zero = q.w < 0.01 || q.h < 0.01;
          r.style.display = (zero && e === 1) || q.hidden ? "none" : "";
        }
      }
      function drawZeros(L) {
        dom.gZero.innerHTML = "";
        if (L.mode === "assoc" || L.mode === "sieve") return;
        var markers = [];
        L.zeros.forEach(function (z) { markers.push(z); });
        L.tiles.forEach(function (t, i) {
          if (!t.hidden && (t.w < 0.01 || t.h < 0.01)) markers.push({ rect: t, dir: t.w < 0.01 ? "v" : "h", cells: [i], leaf: true });
        });
        markers.forEach(function (z) {
          var r = z.rect, x1, y1, x2, y2;
          if (z.dir === "v") { x1 = x2 = r.x; y1 = r.y; y2 = r.y + r.h; }
          else { y1 = y2 = r.y; x1 = r.x; x2 = r.x + r.w; }
          var g = sel("g", {}, dom.gZero);
          sel("line", { x1: x1, y1: y1, x2: x2, y2: y2, "class": "vcdi-zero" }, g);
          var c = sel("circle", { cx: (x1 + x2) / 2, cy: (y1 + y2) / 2, r: 3.2, "class": "vcdi-zero" }, g);
          c.style.cursor = "default";
          c.addEventListener("mousemove", function (ev) { showTip(z.cells[0], ev, z.leaf ? null : z); });
          c.addEventListener("mouseleave", hideTip);
        });
      }
      function drawLabels(L) {
        if (L.mode === "assoc") return drawLabelsAssoc(L);
        if (L.mode === "doubledecker") return drawLabelsDD(L);
        return drawLabelsBorder(L);
      }
      function labelHover(t, v, level) {
        t.addEventListener("mouseenter", function () { highlight(function (i) { return D.cells.idx[v][i] === level; }); });
        t.addEventListener("mouseleave", function () { highlight(null); });
      }
      function drawLabelsDD(L) {
        var g = dom.gLabels; g.innerHTML = "";
        var P = L.plot, nv = S.order.length, eps = 0.5;
        S.order.slice(0, nv - 1).forEach(function (v, k) {
          var y = P.y + P.h + 14 + 16 * k;
          var vn = sel("text", { x: P.x - 8, y: y, "text-anchor": "end", "class": "vcdi-varname" }, g);
          vn.textContent = D.vars[v].name;
          L.groups.forEach(function (q) {
            if (q.depth !== k) return;
            var lab = levelName(v, q.level), txt = fitText(lab, q.rect.w + 2);
            if (!txt) return;
            var t = sel("text", { x: q.rect.x + q.rect.w / 2, y: y, "text-anchor": "middle", "class": "vcdi-label" }, g);
            t.textContent = txt;
            var tt = sel("title", {}, t); tt.textContent = D.vars[v].name + " = " + lab;
            labelHover(t, v, q.level);
          });
        });
        var resp = S.order[nv - 1];
        L.groups.forEach(function (q) {
          if (q.depth !== nv - 1 || Math.abs(q.parent.x + q.parent.w - (P.x + P.w)) > eps || q.rect.h < 9) return;
          var t = sel("text", { x: P.x + P.w + 6, y: q.rect.y + q.rect.h / 2 + 4, "class": "vcdi-label" }, g);
          t.textContent = levelName(resp, q.level);
          labelHover(t, resp, q.level);
        });
        var rn = sel("text", { transform: "translate(" + (L.W - 6) + "," + (P.y + P.h / 2) + ") rotate(90)", "text-anchor": "middle", "class": "vcdi-varname" }, g);
        rn.textContent = D.vars[resp].name;
      }
      function drawLabelsAssoc(L) {
        var g = dom.gLabels; g.innerHTML = "";
        var P = L.plot;
        function spans(list, d, start, size) {
          var out = [], k0 = 0;
          for (var k = 1; k <= list.length; k++) {
            var same = k < list.length;
            for (var e = 0; same && e <= d; e++) if (list[k][e] !== list[k0][e]) same = false;
            if (!same) { out.push({ level: list[k0][d], a: start[k0], b: start[k - 1] + size[k - 1] }); k0 = k; }
          }
          return out;
        }
        var ncd = Math.min(L.colV.length, 2), nrd = Math.min(L.rowV.length, 2);
        for (var d = 0; d < ncd; d++) {
          var v = L.colV[d], y = P.y - 6 - 16 * (ncd - 1 - d);
          spans(L.cols, d, L.cx, L.colW).forEach(function (sp) {
            var lab = levelName(v, sp.level), txt = fitText(lab, sp.b - sp.a + 2);
            if (!txt) return;
            var t = sel("text", { x: (sp.a + sp.b) / 2, y: y, "text-anchor": "middle", "class": "vcdi-label" }, g);
            t.textContent = txt;
            var tt = sel("title", {}, t); tt.textContent = D.vars[v].name + " = " + lab;
            labelHover(t, v, sp.level);
          });
        }
        for (d = 0; d < nrd; d++) {
          var vr = L.rowV[d], x = P.x - 6 - 16 * (nrd - 1 - d);
          spans(L.rows, d, L.ry, L.rh).forEach(function (sp) {
            var lab = levelName(vr, sp.level), txt = fitText(lab, sp.b - sp.a + 2);
            if (!txt) return;
            var cy = (sp.a + sp.b) / 2;
            var t = sel("text", { transform: "translate(" + x + "," + cy + ") rotate(-90)", "text-anchor": "middle", "class": "vcdi-label" }, g);
            t.textContent = txt;
            var tt = sel("title", {}, t); tt.textContent = D.vars[vr].name + " = " + lab;
            labelHover(t, vr, sp.level);
          });
        }
        var nm = function (vs) { return vs.length ? vs.map(function (v) { return D.vars[v].name; }).join(" × ") : "–"; };
        var note = sel("text", { x: P.x, y: L.H - 6, "class": "vcdi-note" }, g);
        note.textContent = T.rows.charAt(0).toUpperCase() + T.rows.slice(1) + ": " + nm(L.rowV) + "   ·   " +
          T.cols.charAt(0).toUpperCase() + T.cols.slice(1) + ": " + nm(L.colV);
      }
      function drawLabelsBorder(L) {
        var g = dom.gLabels; g.innerHTML = "";
        var P = L.plot, font = "11px system-ui, sans-serif", eps = 0.5;
        Object.keys(L.sides).forEach(function (k) {
          k = +k;
          var side = L.sides[k], v = S.order[k];
          var gr = L.groups.filter(function (q) {
            if (q.depth !== k) return false;
            var p = q.parent;
            if (side === "top") return Math.abs(p.y - P.y) < eps;
            if (side === "bottom") return Math.abs(p.y + p.h - (P.y + P.h)) < eps;
            if (side === "left") return Math.abs(p.x - P.x) < eps;
            return Math.abs(p.x + p.w - (P.x + P.w)) < eps;
          });
          var name = D.vars[v].name;
          var vn;
          if (side === "top") vn = sel("text", { x: P.x + P.w / 2, y: P.y - 24, "text-anchor": "middle", "class": "vcdi-varname" }, g);
          else if (side === "bottom") vn = sel("text", { x: P.x + P.w / 2, y: P.y + P.h + 34, "text-anchor": "middle", "class": "vcdi-varname" }, g);
          else if (side === "left") vn = sel("text", { transform: "translate(" + (P.x - 26) + "," + (P.y + P.h / 2) + ") rotate(-90)", "text-anchor": "middle", "class": "vcdi-varname" }, g);
          else vn = sel("text", { transform: "translate(" + (P.x + P.w + 30) + "," + (P.y + P.h / 2) + ") rotate(90)", "text-anchor": "middle", "class": "vcdi-varname" }, g);
          vn.textContent = name;
          gr.forEach(function (q) {
            var r = q.rect, space, txt, t;
            var lab = levelName(v, q.level);
            if (side === "top" || side === "bottom") {
              space = r.w + 4; txt = fitText(lab, space, font); if (!txt) return;
              t = sel("text", { x: r.x + r.w / 2, y: side === "top" ? P.y - 8 : P.y + P.h + 16, "text-anchor": "middle", "class": "vcdi-label" }, g);
            } else {
              space = r.h + 4; txt = fitText(lab, space, font); if (!txt) return;
              var cx = side === "left" ? P.x - 9 : P.x + P.w + 13, cy = r.y + r.h / 2;
              t = sel("text", { transform: "translate(" + cx + "," + cy + ") rotate(" + (side === "left" ? -90 : 90) + ")", "text-anchor": "middle", "class": "vcdi-label" }, g);
            }
            t.textContent = txt;
            var tt = sel("title", {}, t); tt.textContent = name + " = " + lab;
            t.addEventListener("mouseenter", function () { highlight(function (i) { return D.cells.idx[v][i] === q.level; }); });
            t.addEventListener("mouseleave", function () { highlight(null); });
          });
        });
        if (L.unl.length) {
          var nt = sel("text", { x: P.x, y: L.H - 5, "class": "vcdi-note" }, g);
          nt.textContent = L.unl.map(function (v) { return D.vars[v].name; }).join(", ") + " " + T.unlabelled.replace(/:$/, "");
        }
      }

      function setType(t) {
        S.type = t;
        for (var k in dom.segBtns) {
          dom.segBtns[k].classList.toggle("vcdi-on", k === t);
          dom.segBtns[k].setAttribute("aria-checked", k === t ? "true" : "false");
        }
        dom.hint.innerHTML = "<div class='vcdi-hint-f'>" + esc(T.formula[t]) + "</div>" + esc(T.hint[t]);
        if (S.mode === "assoc" && S.W) draw(true); else recolor();
        renderList(); renderLegend();
      }

      function fillFor(i) {
        if (S.mode === "doubledecker") {
          var v = S.order[S.order.length - 1];
          return D.vars[v].fill[D.cells.idx[v][i]];
        }
        if (S.mode === "sieve") return "#ffffff";
        return color(residual(i));
      }
      function drawOverlay() {
        var g = dom.gOver, L = S.layout; g.innerHTML = "";
        if (!L) return;
        if (L.mode === "assoc") {
          L.base.forEach(function (y) {
            sel("line", { x1: L.plot.x, x2: L.plot.x + L.plot.w, y1: y, y2: y, "class": "vcdi-baseline" }, g);
          });
        }
        if (L.mode !== "sieve") return;
        var b0 = D.settings.breaks[0], col = D.settings.colors, nb = D.settings.breaks.length;
        var d = "", dn = "", dz = "";
        for (var i = 0; i < S.n; i++) {
          var t = L.tiles[i], o = D.cells.observed[i];
          if (t.hidden || t.w < 1 || t.h < 1 || o <= 0) continue;
          var nx = Math.max(1, Math.round(Math.sqrt(o * t.w / t.h))), ny = Math.max(1, Math.round(o / nx));
          nx = Math.min(nx, 120); ny = Math.min(ny, 120);
          var path = "";
          for (var k = 1; k < nx; k++) { var xx = t.x + t.w * k / nx; path += "M" + xx.toFixed(1) + " " + t.y.toFixed(1) + "v" + t.h.toFixed(1); }
          for (k = 1; k < ny; k++) { var yy = t.y + t.h * k / ny; path += "M" + t.x.toFixed(1) + " " + yy.toFixed(1) + "h" + t.w.toFixed(1); }
          var r = residual(i);
          if (Math.abs(r) < b0 || isNaN(r)) dz += path; else if (r > 0) d += path; else dn += path;
        }
        if (dz) sel("path", { d: dz, "class": "vcdi-sieve", stroke: "#9a9a9a" }, g);
        if (d) sel("path", { d: d, "class": "vcdi-sieve", stroke: col.pos[nb - 1] }, g);
        if (dn) sel("path", { d: dn, "class": "vcdi-sieve", stroke: col.neg[nb - 1], "stroke-dasharray": "3,2" }, g);
      }
      function recolor() {
        var pa = adjP();
        drawOverlay();
        for (var i = 0; i < S.n; i++) {
          var r = dom.tiles[i];
          r.style.fill = fillFor(i);
          var sig = S.outline && S.mode !== "doubledecker" && pa[i] < S.alpha;
          r.classList.toggle("vcdi-sig", !!sig);
        }
      }

      function highlight(pred) {
        if (!pred) { dom.svg.classList.remove("vcdi-dim"); dom.tiles.forEach(function (r) { r.classList.remove("vcdi-hl"); }); return; }
        dom.svg.classList.add("vcdi-dim");
        dom.tiles.forEach(function (r, i) { r.classList.toggle("vcdi-hl", !!pred(i)); });
      }

      /* ---------------------------------------------------- legend */
      function renderLegend() {
        var g = dom.legend; g.innerHTML = "";
        if (S.mode === "doubledecker") {
          var v = S.order[S.order.length - 1], x = 4, y = 4;
          D.vars[v].levels.forEach(function (l, k) {
            var w = 14 + textWidth(l) + 12;
            if (x + w > 266) { x = 4; y += 16; }
            sel("rect", { x: x, y: y, width: 11, height: 11, fill: D.vars[v].fill[k], stroke: "#444", "stroke-width": .5 }, g);
            var t = sel("text", { x: x + 15, y: y + 9.5, "class": "vcdi-legend-l" }, g); t.textContent = l;
            x += w;
          });
          g.setAttribute("height", y + 18);
          return;
        }
        g.setAttribute("height", 34);
        var b = D.settings.breaks, col = D.settings.colors;
        var cols = [], labs = [];
        for (var j = b.length - 1; j >= 0; j--) cols.push(col.neg[j]);
        cols.push(col.neutral);
        for (j = 0; j < b.length; j++) cols.push(col.pos[j]);
        var edges = [];
        for (j = b.length - 1; j >= 0; j--) edges.push(-b[j]);
        for (j = 0; j < b.length; j++) edges.push(b[j]);
        var w = 260 / cols.length;
        cols.forEach(function (c, k) {
          sel("rect", { x: 4 + k * w, y: 4, width: w, height: 12, fill: c, stroke: "#444", "stroke-width": .5 }, g);
        });
        edges.forEach(function (e, k) {
          var t = sel("text", { x: 4 + (k + 1) * w, y: 28, "text-anchor": "middle" }, g);
          t.textContent = (e > 0 ? "+" : "") + e;
        });
      }

      /* ------------------------------------------------------ list */
      function sigCells() {
        var pa = adjP(), out = [];
        for (var i = 0; i < S.n; i++) if (pa[i] < S.alpha) out.push(i);
        out.sort(function (a, b) { return Math.abs(residual(b)) - Math.abs(residual(a)); });
        return out;
      }
      function renderList() {
        var ids = sigCells(), pa = adjP();
        dom.sigTitle.textContent = T.sig + " (" + ids.length + "/" + S.n + ")";
        dom.list.innerHTML = "";
        if (!ids.length) { el("div", { "class": "vcdi-empty", text: T.none }, dom.list); return; }
        var tbl = el("table", {}, dom.list);
        var hr = el("tr", {}, el("thead", {}, tbl));
        [T.cell, T.obs, T.exp, T.res, T.padj].forEach(function (h) { el("th", { text: h }, hr); });
        var tb = el("tbody", {}, tbl);
        ids.forEach(function (i) {
          var tr = el("tr", {}, tb), r = residual(i);
          var td = el("td", {}, tr);
          el("span", { "class": "vcdi-swatch", style: "background:" + color(r) }, td);
          td.appendChild(document.createTextNode(cellShort(i)));
          td.title = cellLabel(i);
          el("td", { text: fmtCount(D.cells.observed[i]) }, tr);
          el("td", { text: fmt(expectedFor(i), 1) }, tr);
          el("td", { text: signed(r, 2) }, tr);
          el("td", { text: fmtP(pa[i]) }, tr);
          tr.addEventListener("mouseenter", function () {
            highlight(function (j) { return j === i; });
            dom.tiles[i].classList.add("vcdi-hover");
          });
          tr.addEventListener("mouseleave", function () { highlight(null); dom.tiles[i].classList.remove("vcdi-hover"); });
        });
      }
      function downloadCSV() {
        var ids = sigCells(), pa = adjP(), p = D.cells["p_" + S.type];
        var head = S.order.map(function (v) { return D.vars[v].name; }).concat(["observed", "expected", "residual_" + S.type, "p_value", "p_adj_" + S.adjust]);
        var rows = [head.join(",")];
        ids.forEach(function (i) {
          rows.push(S.order.map(function (v) { return '"' + String(levelName(v, D.cells.idx[v][i])).replace(/"/g, '""') + '"'; })
            .concat([D.cells.observed[i], expectedFor(i), residual(i), p[i], pa[i]]).join(","));
        });
        var blob = new Blob([rows.join("\n")], { type: "text/csv" });
        var a = document.createElement("a");
        a.href = URL.createObjectURL(blob); a.download = "significant_cells.csv";
        document.body.appendChild(a); a.click(); setTimeout(function () { URL.revokeObjectURL(a.href); a.remove(); }, 100);
      }

      /* ---------------------------------------------------- tooltip */
      function showTip(i, ev, zeroGroup) {
        var c = D.cells, pa = adjP(), r = residual(i), o = c.observed[i], e = expectedFor(i);
        if (S.hoverCell !== i) {
          if (S.hoverCell !== null) dom.tiles[S.hoverCell].classList.remove("vcdi-hover");
          dom.tiles[i].classList.add("vcdi-hover");
          S.hoverCell = i;
        }
        var h = "";
        if (zeroGroup) {
          h += '<div class="vcdi-tt-title">' + esc(S.order.slice(0, zeroGroup.depth + 1).map(function (v) {
            return D.vars[v].name + " = " + levelName(v, c.idx[v][i]); }).join(", ")) + '</div>';
          h += esc(T.observed) + ": 0";
        } else {
          h += '<div class="vcdi-tt-title">' + S.order.map(function (v) {
            return esc(D.vars[v].name) + " = <b>" + esc(levelName(v, c.idx[v][i])) + "</b>"; }).join("<br>") + "</div>";
          h += "<table>";
          h += "<tr><td>" + esc(T.observed) + "</td><td class='num'>" + fmtCount(o) + "</td></tr>";
          h += "<tr><td>" + esc(S.type === "robust" ? T.expectedRobust : T.expected) + "</td><td class='num'>" + fmt(e, 2) + "</td></tr>";
          h += "<tr><td>" + esc(T.diff) + "</td><td class='num'>" + signed(o - e, 2) + "</td></tr>";
          h += "<tr><td>" + esc(T.ratio) + "</td><td class='num'>" + (e > 0 ? fmt(o / e, 2) : "–") + "</td></tr>";
          if (S.mode === "doubledecker" && S.order.length > 1) {
            var resp = S.order[S.order.length - 1], preds = S.order.slice(0, -1), tot = 0;
            for (var j = 0; j < S.n; j++) {
              var same = true;
              for (var q = 0; q < preds.length; q++) if (c.idx[preds[q]][j] !== c.idx[preds[q]][i]) { same = false; break; }
              if (same) tot += c.observed[j];
            }
            h += "<tr><td>" + esc(T.colTotal) + "</td><td class='num'>" + fmtCount(tot) + "</td></tr>";
            h += "<tr><td><b>P(" + esc(D.vars[resp].name) + " = " + esc(levelName(resp, c.idx[resp][i])) + " | \u2026)</b></td><td class='num'><b>" +
              (tot > 0 ? (100 * o / tot).toFixed(1) + "%" : "\u2013") + "</b></td></tr>";
          }
          h += "</table><table style='margin-top:6px'>";
          D.settings.available.forEach(function (t) {
            h += "<tr class='" + (t === S.type ? "sel" : "") + "'><td>" + esc(T.short[t]) + "</td><td class='num'>" + signed(c[t][i], 2) +
              "</td><td class='num'>" + esc(T.pval) + " = " + fmtP(c["p_" + t][i]) + "</td></tr>";
          });
          h += "</table>";
          h += "<div>" + esc(T.pvalAdj) + " (" + esc(T.adj[S.adjust]) + "): <b>" + fmtP(pa[i]) + "</b></div>";
          if (pa[i] < S.alpha)
            h += "<span class='vcdi-tt-sig'>" + esc(T.significant) + " – " + esc(r > 0 ? T.over : T.under) + "</span>";
        }
        dom.tip.innerHTML = h;
        var box = dom.root.getBoundingClientRect();
        var x = ev.clientX - box.left + 14, y = ev.clientY - box.top + 14;
        dom.tip.style.opacity = 1;
        var tw = dom.tip.offsetWidth, th = dom.tip.offsetHeight;
        if (x + tw > box.width - 4) x = ev.clientX - box.left - tw - 14;
        if (y + th > box.height - 4) y = Math.max(4, ev.clientY - box.top - th - 14);
        dom.tip.style.left = Math.max(4, x) + "px";
        dom.tip.style.top = y + "px";
      }
      function hideTip() {
        dom.tip.style.opacity = 0;
        if (S && S.hoverCell !== null) { dom.tiles[S.hoverCell].classList.remove("vcdi-hover"); S.hoverCell = null; }
      }

      /* ---------------------------------------------------- resize */
      function resize(w, h) {
        if (!D) return;
        var narrow = w < 720;
        dom.root.classList.toggle("vcdi-narrow", narrow);
        var top = dom.body.getBoundingClientRect().top - dom.root.getBoundingClientRect().top;
        var W, H;
        if (narrow) { W = w; H = Math.max(260, Math.min(w, h - top - 10)); }
        else { W = Math.max(200, w - 291); H = Math.max(200, h - top); }
        S.W = W; S.H = H;
        S.prev = null;
        draw(false);
      }

      return {
        renderValue: function (x) { build(x); },
        resize: function (w, h) { width = w; height = h; resize(w, h); }
      };
    }
  });
})();
