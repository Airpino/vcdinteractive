## helpers shared by the widgets ----------------------------------------------
vcdi_widget <- function(name, payload, width = NULL, height = NULL,
                        elementId = NULL, default_height = 620) {
  htmlwidgets::createWidget(
    name = name, x = payload, width = width, height = height,
    package = "vcdinteractive", elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%", defaultHeight = default_height,
      viewer.padding = 0, browser.fill = TRUE, knitr.figure = FALSE,
      knitr.defaultWidth = "100%", knitr.defaultHeight = default_height))
}

shading_settings <- function(breaks, colors) {
  if (is.null(colors)) colors <- shading_colors(breaks)
  list(breaks = I(breaks),
       colors = list(pos = I(colors$pos), neg = I(colors$neg), neutral = colors$neutral))
}

#' Interactive matrix of pairwise mosaic plots
#'
#' The interactive counterpart of `pairs()` for tables in 'vcd': a matrix
#' with one mosaic plot for each pair of variables and a bar chart of each
#' variable on the diagonal. Each pairwise mosaic is shaded by the residuals
#' of the independence model for that two-way margin (`type = "pairwise"` in
#' `vcd::pairs.table()`).
#'
#' Interactions: hovering a tile shows its counts, residuals and p-values and
#' highlights, in all the other panels, the tiles with the same categories
#' (linked highlighting); hovering a bar of the diagonal highlights that
#' category everywhere; clicking a panel enlarges it, with category labels,
#' and the "Matrix" button goes back. The side panel switches the residuals,
#' and lists the significant cells of all panels, with a multiplicity
#' correction applied to all the cells of all the panels.
#'
#' @inheritParams imosaic
#' @param x a contingency table with at least two variables, or anything
#'   accepted by [mosaic_cells()] except a fitted model.
#' @return an `htmlwidget`.
#' @examples
#' ipairs(HairEyeColor)
#' ipairs(Titanic, residuals = "pearson")
#' @export
ipairs <- function(x, residuals = "standardized", alpha = 0.05, adjust = "holm",
                   robust = c("auto", "medpolish", "glmrob", "none"),
                   breaks = c(2, 4), colors = NULL, main = NULL,
                   width = NULL, height = NULL, elementId = NULL) {
  robust <- match.arg(robust)
  residuals <- match.arg(residuals, residual_types)
  tab <- as_vcdi_table(x)
  vars <- names(dimnames(tab))
  nd <- length(vars)
  if (nd < 2) stop("'x' must have at least two variables")
  avail <- residual_types
  pairs <- list()
  for (i in seq_len(nd - 1)) for (j in (i + 1):nd) {
    cells <- suppressWarnings(mosaic_cells(margin.table(tab, c(i, j)), robust = robust))
    if (all(is.na(cells$robust))) avail <- setdiff(avail, "robust")
    pairs[[paste0(i - 1L, "-", j - 1L)]] <- table_payload(cells, c(i, j), nd,
                                                          label = paste(vars[c(i, j)], collapse = " x "))
  }
  if (!residuals %in% avail) residuals <- "standardized"
  payload <- list(
    main = if (is.null(main)) "" else main,
    vars = lapply(seq_len(nd), function(i)
      list(name = vars[i], levels = I(dimnames(tab)[[i]]),
           margin = I(as.vector(margin.table(tab, i))))),
    pairs = pairs,
    settings = c(list(residuals = residuals, available = I(avail),
                      alpha = alpha, adjust = adjust), shading_settings(breaks, colors)))
  vcdi_widget("ipairs", payload, width, height, elementId, default_height = 680)
}
