## frequency distribution of a discrete variable --------------------------------
as_freq_dist <- function(x) {
  if (is.data.frame(x)) {
    num <- vapply(x, is.numeric, logical(1))
    fcol <- if ("Freq" %in% names(x)) "Freq" else names(x)[num][2]
    vcol <- setdiff(names(x)[num], fcol)[1]
    if (is.na(vcol) || is.na(fcol)) stop("data frame must have a numeric value column and a frequency column")
    v <- x[[vcol]]; f <- x[[fcol]]
  } else if ((is.table(x) || (is.numeric(x) && !is.null(names(x)))) && length(dim(x)) <= 1 &&
             !anyNA(suppressWarnings(as.numeric(names(x))))) {
    v <- as.numeric(names(x)); f <- as.numeric(x)
  } else if (is.numeric(x)) {
    if (any(x < 0 | x != round(x), na.rm = TRUE)) stop("raw data must be non-negative integers")
    tb <- table(x[!is.na(x)]); v <- as.numeric(names(tb)); f <- as.numeric(tb)
  } else stop("'x' must be a frequency table, a named vector of frequencies, a data frame or a vector of counts")
  if (any(v < 0 | v != round(v))) stop("the values must be non-negative integers")
  full <- seq.int(0, max(v))
  ff <- numeric(length(full)); ff[match(v, full)] <- f
  list(values = full, freq = ff)
}

fit_discrete <- function(dist, v, f, size = NULL) {
  N <- sum(f); m <- sum(v * f) / N
  p <- switch(dist,
    poisson = {
      par <- c(lambda = m)
      stats::dpois(v, m)
    },
    binomial = {
      if (is.null(size)) size <- max(v)
      if (size < max(v)) stop("'size' must be at least the largest value")
      par <- c(size = size, prob = m / size)
      stats::dbinom(v, size, m / size)
    },
    nbinomial = {
      ll <- function(lr) -sum(f * stats::dnbinom(v, size = exp(lr), mu = m, log = TRUE))
      o <- stats::optimize(ll, c(-8, 12))
      r <- exp(o$minimum)
      par <- c(size = r, prob = r / (r + m))
      stats::dnbinom(v, size = r, mu = m)
    },
    stop("unknown distribution: ", dist))
  e <- N * p
  npar <- if (dist == "binomial") 1 else length(par)
  X2 <- sum((f - e)^2 / e)
  G2 <- 2 * sum(ifelse(f > 0, f * log(f / e), 0))
  df <- length(v) - 1 - npar
  list(name = dist,
       label = c(poisson = "Poisson", binomial = "Binomial", nbinomial = "Negative binomial")[[dist]],
       params = paste(names(par), signif(par, 4), sep = " = ", collapse = ", "),
       expected = I(e), X2 = X2, G2 = G2, df = df,
       p_X2 = if (df > 0) stats::pchisq(X2, df, lower.tail = FALSE) else NA,
       p_G2 = if (df > 0) stats::pchisq(G2, df, lower.tail = FALSE) else NA)
}

#' Interactive rootogram for discrete distributions
#'
#' The interactive counterpart of `vcd::rootogram()` applied to
#' `vcd::goodfit()`. The observed frequencies of a count variable are drawn
#' as bars hanging from (or standing on) the curve of the frequencies
#' expected under a fitted discrete distribution, on the square-root scale
#' (Tukey's rootogram), which stabilizes the variance so that departures are
#' comparable along the whole range.
#'
#' The distributions (Poisson, binomial, negative binomial) are fitted by
#' maximum likelihood. In the widget you can switch the distribution, the
#' type of rootogram (`"hanging"`, `"standing"`, `"deviation"`) and the
#' scale (square root or raw); the header reports the parameters and the
#' Pearson and likelihood-ratio goodness-of-fit statistics, with
#' \eqn{df = K - 1 - } number of estimated parameters, \eqn{K} being the
#' number of count values. Hovering a bar shows the observed and expected
#' frequencies and the Pearson residual.
#'
#' @param x the frequency distribution: a one-way table or named numeric
#'   vector whose names are the count values (as `vcd::HorseKicks`), a data
#'   frame with a value column and a frequency column (as `vcd::Federalist`
#'   converted with `as.data.frame()`), or a vector of raw counts.
#' @param dists distributions to fit; the first one is shown at start-up.
#' @param size number of trials of the binomial distribution (default: the
#'   largest observed value).
#' @param type,scale initial type of rootogram and scale.
#' @param xlab label of the count variable.
#' @param main optional title.
#' @param width,height,elementId passed to [htmlwidgets::createWidget()].
#' @return an `htmlwidget`.
#' @examples
#' horse <- c("0" = 109, "1" = 65, "2" = 22, "3" = 3, "4" = 1)
#' irootogram(horse, xlab = "Deaths by horse kicks")
#' irootogram(rpois(200, 3) + rbinom(200, 1, 0.3) * 5, dists = c("nbinomial", "poisson"))
#' @export
irootogram <- function(x, dists = c("poisson", "binomial", "nbinomial"), size = NULL,
                       type = c("hanging", "standing", "deviation"), scale = c("sqrt", "raw"),
                       xlab = NULL, main = NULL, width = NULL, height = NULL, elementId = NULL) {
  type <- match.arg(type); scale <- match.arg(scale)
  dists <- match.arg(dists, c("poisson", "binomial", "nbinomial"), several.ok = TRUE)
  if (is.null(xlab)) {
    xlab <- if (is.table(x) && !is.null(names(dimnames(x))) && nzchar(names(dimnames(x))[1]))
      names(dimnames(x))[1] else "Count"
  }
  fd <- as_freq_dist(x)
  fits <- lapply(dists, function(d) fit_discrete(d, fd$values, fd$freq, size))
  payload <- list(main = if (is.null(main)) "" else main, xlab = xlab,
                  values = I(fd$values), freq = I(fd$freq), fits = fits,
                  settings = c(list(type = type, scale = scale), shading_settings(c(2, 4), NULL)))
  vcdi_widget("irootogram", payload, width, height, elementId, default_height = 560)
}
