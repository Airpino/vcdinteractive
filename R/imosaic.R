#' Interactive mosaic plot
#'
#' Draws a mosaic plot of a contingency table as an interactive
#' 'htmlwidget'. Compared with `vcd::mosaic()`:
#' \itemize{
#'   \item the order of the variables can be changed by dragging them in the
#'     toolbar, and the direction of each split (horizontal/vertical) can be
#'     toggled; the plot is recomputed and animated immediately, in the
#'     browser, without re-running R;
#'   \item hovering a tile shows the combination of categories, the observed
#'     and expected counts, their ratio and difference, the residual and its
#'     p-value; hovering a category label highlights all its tiles;
#'   \item the residuals used for shading can be switched among those listed
#'     in [residual_types] (Pearson, adjusted standardized, deviance,
#'     Freeman-Tukey, Anscombe, robust);
#'   \item variables can be added to or removed from the plot with the
#'     check box of each variable: the sub-tables (margins of the full table)
#'     and their models are precomputed, so that the mosaic can be built up
#'     one variable at a time;
#'   \item a side panel lists the cells that are significantly far from the
#'     model, with a choice of multiplicity correction; hovering a row
#'     highlights the corresponding tile.
#' }
#'
#' Since expected counts and residuals depend on the cells and not on the
#' order in which variables are split, everything is computed once in R by
#' [mosaic_cells()] and the layout is recomputed in JavaScript.
#'
#' When a variable is removed, the table is collapsed over it and the model
#' is projected on the remaining variables: each term of `expected` is
#' restricted to the variables still displayed. For instance, removing
#' `Dept` from the model `~ Admit * Dept + Gender * Dept` gives the model of
#' independence `~ Admit + Gender` for the collapsed `Admit x Gender` table,
#' which is the comparison behind Simpson's paradox.
#'
#' @inheritParams mosaic_cells
#' @param formula optional one-sided formula selecting (and ordering) the
#'   variables of `data`, e.g. `~ Class + Sex + Survived`.
#' @param data a table or data frame, used with the formula interface.
#' @param residuals residual type used for shading at start-up.
#' @param direction initial split directions, a vector of `"h"`/`"v"`
#'   recycled over variables. The default `"h"` alternates `h, v, h, ...`
#'   as in `vcd::mosaic()`.
#' @param alpha,adjust significance level and multiplicity correction used
#'   for the list of significant cells (see [significant_cells()]).
#' @param breaks absolute residual cut-offs for the shading (as
#'   `interpolate` in `vcd::shading_hcl()`).
#' @param colors optional named list with `pos`, `neg` (vectors of
#'   `length(breaks)` colours from light to dark) and `neutral`.
#' @param main optional title.
#' @param width,height,elementId passed to [htmlwidgets::createWidget()].
#' @param ... further arguments (currently ignored).
#' @return an object of class `htmlwidget`.
#' @examples
#' imosaic(HairEyeColor)
#' imosaic(Titanic, residuals = "robust")
#' imosaic(~ Class + Survived, data = Titanic, expected = ~ Class + Survived)
#' # start from two variables and add the others interactively
#' imosaic(Titanic, include = c("Class", "Survived"))
#' @export
imosaic <- function(x, ...) UseMethod("imosaic")

#' @rdname imosaic
#' @export
imosaic.formula <- function(formula, data, ...) {
  tab <- as_vcdi_table(data)
  tab <- subset_by_formula(tab, formula)
  imosaic.default(tab, ...)
}

#' @rdname imosaic
#' @export
imosaic.glm <- function(x, ...) imosaic.default(x, ...)

#' @rdname imosaic
#' @param display initial display: `"mosaic"`, `"assoc"` (association plot,
#'   as `vcd::assoc()`), `"sieve"` (sieve diagram, as `vcd::sieve()`) or
#'   `"doubledecker"` (as `vcd::doubledecker()`, the last displayed variable
#'   being the response). It can be changed in the widget.
#' @param models optional set of log-linear models to compare in the widget,
#'   replacing `expected`: a character vector with some of `"mutual"`,
#'   `"joint"`, `"conditional"`, `"markov"`, `"saturated"` (as in
#'   `vcdExtra::seq_loglm()`, built on the order of the variables of the
#'   table, the last one playing the role of response), and/or a named list
#'   of one-sided formulas or lists of margins. The first model is shown at
#'   start-up; a panel lists them all with \eqn{G^2}, df, p-value and AIC.
#' @param include initially displayed variables (names or positions); the
#'   others can be added from the toolbar. Default: all.
#' @param subsets which sub-tables are precomputed, so that variables can be
#'   added and removed in the browser: `"all"` (every non-empty subset of the
#'   variables, \eqn{2^k - 1} tables), `"sequential"` (only the leading
#'   variables of the initial order, as in `vcdExtra::seq_mosaic()`: variables
#'   can then be added or removed only at the end), or `"none"`. The default
#'   `"auto"` is `"all"` for up to 6 variables and `"sequential"` otherwise.
#'   With a fitted model as input only the full table is available.
#' @export
imosaic.default <- function(x, expected = NULL, residuals = "standardized",
                            direction = "h", alpha = 0.05, adjust = "holm",
                            robust = c("auto", "medpolish", "glmrob", "none"),
                            breaks = c(2, 4), colors = NULL,
                            main = NULL, display = c("mosaic", "assoc", "sieve", "doubledecker"),
                            models = NULL, include = NULL,
                            subsets = c("auto", "all", "sequential", "none"),
                            width = NULL, height = NULL, elementId = NULL, ...) {
  robust <- match.arg(robust)
  subsets <- match.arg(subsets)
  display <- match.arg(display)
  residuals <- match.arg(residuals, residual_types)
  if (is.null(main)) main <- ""
  avail <- residual_types
  tables <- list()

  if (inherits(x, "glm")) {
    ## fitted model: a single table and a single model
    cells <- mosaic_cells(x, robust = robust)
    vars <- attr(cells, "vars")
    nd <- length(vars)
    levs <- lapply(vars, function(v) levels(cells[[v]]))
    if (all(is.na(cells$robust))) avail <- setdiff(avail, "robust")
    lab <- paste(deparse(attr(cells, "model")$formula), collapse = " ")
    mods <- list(list(name = paste0(class(x)[1], ": ", lab)))
    tables[[paste0("0|", paste(seq_len(nd) - 1L, collapse = "-"))]] <-
      table_payload(cells, seq_len(nd), nd, label = mods[[1]]$name)
    subsets <- "none"
    active <- rep(TRUE, nd)
  } else {
    tab <- as_vcdi_table(x)
    vars <- names(dimnames(tab))
    nd <- length(vars)
    levs <- unname(dimnames(tab))
    mods <- model_list(models, expected, vars)

    active <- if (is.null(include)) rep(TRUE, nd)
              else if (is.character(include)) vars %in% include
              else seq_len(nd) %in% include
    if (!any(active)) stop("'include' selects no variable")

    if (subsets == "auto") subsets <- if (nd <= 6) "all" else "sequential"
    sets <- switch(subsets,
      all = unlist(lapply(seq_len(nd), function(k) utils::combn(nd, k, simplify = FALSE)),
                   recursive = FALSE),
      sequential = lapply(seq_len(nd), seq_len),
      none = list(seq_len(nd)))
    sets <- c(sets, list(which(active)))
    keys <- vapply(sets, function(s) paste(sort(s) - 1L, collapse = "-"), "")
    sets <- sets[!duplicated(keys)]
    keys <- keys[!duplicated(keys)]
    full_key <- paste(seq_len(nd) - 1L, collapse = "-")

    for (mi in seq_along(mods)) for (j in seq_along(sets)) {
      s <- sort(sets[[j]])
      is_full <- keys[j] == full_key && mi == 1
      sub <- if (length(s) == nd) tab else margin.table(tab, s)
      exp_s <- project_margins(mods[[mi]]$margins, vars[s])
      cells <- if (is_full) mosaic_cells(sub, expected = exp_s, robust = robust)
               else suppressWarnings(mosaic_cells(sub, expected = exp_s, robust = robust))
      if (is_full && all(is.na(cells$robust))) avail <- setdiff(avail, "robust")
      tables[[paste0(mi - 1L, "|", keys[j])]] <-
        table_payload(cells, s, nd, label = margins_label(exp_s))
    }
  }

  if (length(direction) == 1)
    direction <- rep_len(c(direction, setdiff(c("h", "v"), direction)), nd)
  direction <- rep_len(direction, nd)
  stopifnot(all(direction %in% c("h", "v")))
  if (!residuals %in% avail) residuals <- "standardized"
  if (is.null(colors)) colors <- shading_colors(breaks)

  payload <- list(
    main = main,
    vars = lapply(seq_len(nd), function(i)
      list(name = vars[i], levels = I(levs[[i]]),
           fill = I(rev(grDevices::gray.colors(length(levs[[i]])))))),
    models = lapply(mods, function(m) list(name = m$name)),
    tables = tables,
    settings = list(residuals = residuals, available = I(avail),
                    direction = I(direction), alpha = alpha, adjust = adjust,
                    breaks = I(breaks),
                    colors = list(pos = I(colors$pos), neg = I(colors$neg),
                                  neutral = colors$neutral),
                    active = I(active), subsets = subsets, display = display)
  )

  htmlwidgets::createWidget(
    name = "imosaic", x = payload, width = width, height = height,
    package = "vcdinteractive", elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%", defaultHeight = 620,
      viewer.padding = 0, browser.fill = TRUE, knitr.figure = FALSE,
      knitr.defaultWidth = "100%", knitr.defaultHeight = 620))
}

## models to compare --------------------------------------------------------
model_types <- c(mutual = "Mutual independence", joint = "Joint independence",
                 conditional = "Conditional independence", markov = "Markov chain",
                 saturated = "Saturated")

seq_margins <- function(type, vars) {
  k <- length(vars)
  switch(type,
    mutual = as.list(vars),
    joint = if (k < 2) list(vars) else list(vars[-k], vars[k]),
    conditional = if (k < 3) as.list(vars) else lapply(vars[-k], function(v) c(v, vars[k])),
    markov = if (k < 2) list(vars) else lapply(seq_len(k - 1), function(i) vars[i:(i + 1)]),
    saturated = list(vars),
    stop("unknown model type: ", type))
}

margins_label <- function(m) paste0("[", vapply(m, paste, "", collapse = " "), "]", collapse = "")

model_list <- function(models, expected, vars) {
  if (is.null(models)) {
    m <- model_margins(expected, vars)
    return(list(list(name = margins_label(m), margins = m)))
  }
  if (is.character(models)) models <- as.list(models)
  nm <- names(models)
  if (is.null(nm)) nm <- rep("", length(models))
  out <- vector("list", length(models))
  for (i in seq_along(models)) {
    mi <- models[[i]]
    if (is.character(mi) && length(mi) == 1 && mi %in% names(model_types)) {
      m <- seq_margins(mi, vars)
      name <- if (nzchar(nm[i])) nm[i] else model_types[[mi]]
    } else {
      m <- model_margins(mi, vars)
      name <- if (nzchar(nm[i])) nm[i] else margins_label(m)
    }
    out[[i]] <- list(name = paste0(name, " ", margins_label(m)), margins = m)
  }
  out
}

## generating class of the model as a list of character vectors -------------
model_margins <- function(expected, vars) {
  if (is.null(expected)) return(as.list(vars))
  if (inherits(expected, "formula")) {
    tt <- stats::terms(expected)
    fac <- attr(tt, "factors")
    if (length(fac) == 0) return(as.list(vars))
    rn <- gsub("`", "", rownames(fac))
    m <- lapply(seq_len(ncol(fac)), function(j) rn[fac[, j] > 0])
  } else if (is.list(expected)) {
    m <- lapply(expected, function(v) if (is.numeric(v)) vars[v] else v)
  } else stop("'expected' must be NULL, a formula or a list of margins (as in loglin())")
  miss <- setdiff(unlist(m), vars)
  if (length(miss)) stop("Variable(s) in 'expected' not found: ", paste(miss, collapse = ", "))
  maximal_terms(m)
}

## keep only the terms not contained in another one (the generating class)
maximal_terms <- function(m) {
  m <- unique(lapply(m, function(v) v[order(match(v, unique(unlist(m))))]))
  keep <- vapply(seq_along(m), function(i)
    !any(vapply(seq_along(m)[-i], function(j)
      all(m[[i]] %in% m[[j]]) && length(m[[j]]) > length(m[[i]]), logical(1))), logical(1))
  m[keep]
}

## model for a sub-table: restrict every term to the kept variables, keep the
## maximal terms only, and add main effects of variables left out
project_margins <- function(margins, kept) {
  m <- lapply(margins, function(v) intersect(v, kept))
  m <- m[lengths(m) > 0]
  m <- c(m, as.list(setdiff(kept, unlist(m))))
  m <- unique(lapply(m, sort))
  maximal <- vapply(seq_along(m), function(i)
    !any(vapply(seq_along(m)[-i], function(j)
      all(m[[i]] %in% m[[j]]) && length(m[[j]]) > length(m[[i]]), logical(1))), logical(1))
  m[maximal]
}

table_payload <- function(cells, s, nd, label = "") {
  ## I() keeps length-1 vectors as JSON arrays (htmlwidgets uses auto_unbox)
  num <- function(v) I(unname(round(as.numeric(v), 6)))
  vars <- attr(cells, "vars")
  model <- attr(cells, "model")
  idx <- rep(list(I(integer(0))), nd)
  for (k in seq_along(s)) idx[[s[k]]] <- I(as.integer(cells[[vars[k]]]) - 1L)
  list(
    vars = I(s - 1L),
    cells = c(
      list(idx = idx),
      list(observed = num(cells$observed),
           expected = num(cells$expected),
           expected_robust = num(cells$expected_robust),
           leverage = num(cells$leverage)),
      stats::setNames(lapply(residual_types, function(r) num(cells[[r]])), residual_types),
      stats::setNames(lapply(residual_types, function(r) num(cells[[paste0("p_", r)]])),
                      paste0("p_", residual_types))),
    model = list(formula = paste(deparse(model$formula), collapse = " "),
                 label = label,
                 X2 = model$X2, G2 = model$G2, df = model$df,
                 p_X2 = model$p_X2, p_G2 = model$p_G2, aic = model$aic,
                 n = model$n, robust = model$robust))
}

## colours as in vcd::shading_hcl(): hue 260 (blue) for positive, 0 (red) for
## negative residuals; chroma and luminance interpolated at the breaks.
shading_colors <- function(breaks = c(2, 4), h = c(260, 0), c = 100, l = c(90, 50)) {
  k <- length(breaks)
  v <- seq_len(k) / k
  col <- function(hue) grDevices::hcl(hue, c * v, l[1] - (l[1] - l[2]) * v, fixup = TRUE)
  list(pos = col(h[1]), neg = col(h[2]),
       neutral = grDevices::hcl(0, 0, l[1]))
}

#' Shiny bindings for imosaic
#'
#' @param outputId output variable to read from.
#' @param width,height widget size.
#' @param expr an expression that generates an [imosaic()].
#' @param env the environment in which to evaluate `expr`.
#' @param quoted is `expr` a quoted expression?
#' @return a Shiny output or render function.
#' @export
imosaicOutput <- function(outputId, width = "100%", height = "620px") {
  htmlwidgets::shinyWidgetOutput(outputId, "imosaic", width, height,
                                 package = "vcdinteractive")
}

#' @rdname imosaicOutput
#' @export
renderImosaic <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, imosaicOutput, env, quoted = TRUE)
}
