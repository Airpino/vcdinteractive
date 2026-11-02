## remove the association between variables a and b from a generating class
drop_edge <- function(margins, a, b, vars) {
  out <- list()
  for (m in margins) {
    if (a %in% m && b %in% m) out <- c(out, list(setdiff(m, a), setdiff(m, b)))
    else out <- c(out, list(m))
  }
  project_margins(out, vars)
}

## log-linear model fitted by iterative proportional fitting (robust to
## sampling zeros, unlike glm); returns G2, df and AIC
fit_margins <- function(tab, margins) {
  vars <- names(dimnames(tab))
  ll <- stats::loglin(tab, lapply(margins, match, vars), fit = TRUE, print = FALSE,
                      iter = 1000, eps = 1e-8)
  o <- as.vector(tab); e <- as.vector(ll$fit)
  npar <- length(o) - ll$df
  list(G2 = max(0, ll$lrt), df = ll$df,
       aic = -2 * sum(stats::dpois(o, e, log = TRUE)) + 2 * npar)
}

#' Interactive association graph of a log-linear model
#'
#' An interactive version of `vcdExtra::assoc_graph()` and its `plot()`
#' method. The variables of a contingency table are the nodes; two variables
#' are joined when they appear together in a term of the log-linear model.
#' The width of an edge is proportional to the strength of the partial
#' association, measured by the increase in \eqn{G^2} when that association
#' is removed from the model (with its degrees of freedom and p-value).
#'
#' Several models can be compared: a side table lists them with
#' \eqn{G^2}, df, p-value and AIC, and clicking a row redraws the graph. The
#' nodes can be dragged; hovering an edge shows its statistics, hovering a
#' node highlights its edges. Optionally the marginal associations of all
#' pairs (Cramér's V of the two-way margins) are drawn as dotted lines.
#'
#' @inheritParams imosaic
#' @param x a contingency table, or anything accepted by [mosaic_cells()]
#'   except a fitted model.
#' @param models the models to compare, as in [imosaic()]. The default
#'   compares the standard sequence of models; the one with the smallest AIC
#'   is shown at start-up.
#' @param initial the model shown at start-up (position or name); by default
#'   the one with the smallest AIC.
#' @return an `htmlwidget`.
#' @examples
#' iassoc_graph(Titanic)
#' iassoc_graph(UCBAdmissions, models = list(
#'   "[AD][GD]" = ~ Admit * Dept + Gender * Dept,
#'   "[AGD]" = ~ Admit * Gender * Dept))
#' @export
iassoc_graph <- function(x, models = c("mutual", "joint", "conditional", "markov", "saturated"),
                         initial = NULL, main = NULL, width = NULL, height = NULL, elementId = NULL) {
  tab <- as_vcdi_table(x)
  vars <- names(dimnames(tab))
  nd <- length(vars)
  if (nd < 2) stop("'x' must have at least two variables")
  mods <- model_list(models, NULL, vars)
  out <- lapply(mods, function(md) {
    m <- project_margins(md$margins, vars)
    fit <- fit_margins(tab, m)
    pairs <- unique(do.call(rbind, c(list(matrix(integer(0), 0, 2)), lapply(m, function(t) {
      if (length(t) < 2) return(NULL)
      t(utils::combn(match(t, vars), 2))
    }))))
    edges <- if (nrow(pairs)) lapply(seq_len(nrow(pairs)), function(r) {
      a <- vars[pairs[r, 1]]; b <- vars[pairs[r, 2]]
      red <- fit_margins(tab, drop_edge(m, a, b, vars))
      dG2 <- max(0, red$G2 - fit$G2)
      ddf <- red$df - fit$df
      list(i = pairs[r, 1] - 1L, j = pairs[r, 2] - 1L, dG2 = dG2, ddf = ddf,
           p = stats::pchisq(dG2, ddf, lower.tail = FALSE))
    }) else list()
    list(name = unname(md$name), label = margins_label(m), G2 = fit$G2,
         df = fit$df, p = stats::pchisq(fit$G2, fit$df, lower.tail = FALSE),
         aic = fit$aic, edges = edges)
  })
  pr <- list()
  for (i in seq_len(nd - 1)) for (j in (i + 1):nd) {
    m2 <- margin.table(tab, c(i, j))
    cs <- suppressWarnings(stats::chisq.test(m2, correct = FALSE))
    v <- sqrt(unname(cs$statistic) / (sum(m2) * (min(dim(m2)) - 1)))
    pr[[length(pr) + 1]] <- list(i = i - 1L, j = j - 1L, V = v, chisq = unname(cs$statistic),
                                 df = unname(cs$parameter), p = cs$p.value)
  }
  best <- if (is.null(initial)) which.min(vapply(out, `[[`, 0, "aic"))
          else if (is.character(initial)) match(initial, names(models))
          else initial
  if (is.na(best) || best < 1 || best > length(out)) stop("invalid 'initial' model")
  payload <- list(main = if (is.null(main)) "" else main,
                  vars = lapply(seq_len(nd), function(i) list(name = vars[i], nlev = dim(tab)[i])),
                  models = out, pairs = pr, settings = list(model = best - 1L))
  vcdi_widget("iassocgraph", payload, width, height, elementId, default_height = 560)
}
