#' Interactive spinogram and conditional density plot
#'
#' Interactive counterparts of `vcd::spine()` and `vcd::cd_plot()` for a
#' categorical response and a numeric predictor. Both show how the
#' conditional distribution of the response changes along the predictor:
#' the spinogram groups the predictor into bins whose widths are
#' proportional to their counts, the conditional density plot smooths
#' \eqn{P(y = k \mid x)} with kernel density estimates.
#'
#' The widget switches between the two displays; a slider sets the number of
#' bins of the spinogram (equal-width or quantile bins) or the bandwidth of
#' the kernel, as a multiple of the default `stats::bw.nrd0()` bandwidth, and
#' the plot is recomputed in the browser. Hovering shows the bin counts and
#' proportions, or the estimated conditional probabilities at that value.
#'
#' The conditional density uses the same Gaussian kernel and bandwidth for
#' all the classes, so that
#' \eqn{\hat P(y = k \mid x) = \sum_{i: y_i = k} K_h(x - x_i) / \sum_i K_h(x - x_i)},
#' which is what `vcd::cd_plot()` computes from `density()` estimates.
#'
#' @param formula a formula `y ~ x` with a factor (or logical) response and a
#'   numeric predictor.
#' @param data a data frame.
#' @param display initial display, `"spine"` or `"cdplot"`.
#' @param breaks initial number of bins of the spinogram (default: Sturges).
#' @param bw initial bandwidth multiplier for the conditional density.
#' @param main optional title.
#' @param width,height,elementId passed to [htmlwidgets::createWidget()].
#' @param ... further arguments passed to `ispine()`.
#' @return an `htmlwidget`.
#' @examples
#' ispine(am ~ mpg, data = transform(mtcars, am = factor(am, labels = c("automatic", "manual"))))
#' icdplot(Species ~ Sepal.Length, data = iris)
#' @export
ispine <- function(formula, data, display = c("spine", "cdplot"), breaks = NULL, bw = 1,
                   main = NULL, width = NULL, height = NULL, elementId = NULL) {
  display <- match.arg(display)
  mf <- stats::model.frame(formula, data)
  y <- mf[[1]]; x <- mf[[2]]
  if (is.logical(y) || is.numeric(y)) y <- factor(y)
  if (!is.factor(y)) y <- factor(y)
  y <- droplevels(y)
  if (!is.numeric(x)) stop("the predictor must be numeric")
  if (is.null(breaks)) breaks <- grDevices::nclass.Sturges(x)
  payload <- list(
    main = if (is.null(main)) "" else main,
    xname = names(mf)[2], yname = names(mf)[1],
    levels = I(levels(y)), fill = I(rev(grDevices::gray.colors(nlevels(y)))),
    x = I(unname(x)), y = I(as.integer(y) - 1L),
    bw0 = stats::bw.nrd0(x),
    settings = list(display = display, breaks = breaks, bw = bw))
  vcdi_widget("ispine", payload, width, height, elementId, default_height = 560)
}

#' @rdname ispine
#' @export
icdplot <- function(formula, data, ...) ispine(formula, data, display = "cdplot", ...)
