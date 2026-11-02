#' Interactive fourfold display of 2 x 2 x k tables
#'
#' The interactive counterpart of `vcd::fourfold()`. Each stratum of a
#' \eqn{2 \times 2 \times k} table is drawn as four quarter circles whose
#' areas are proportional to the cell frequencies, after standardizing the
#' table so that both margins are equal (which leaves the odds ratio
#' unchanged). The diagonal quarters are coloured blue for a positive and
#' red for a negative association, in a darker shade when the confidence
#' interval of the odds ratio excludes 1; the rings show the confidence
#' interval.
#'
#' A second panel plots the log odds ratio of each stratum with its
#' confidence interval, together with the Mantel-Haenszel estimate of the
#' common odds ratio. Woolf's test of homogeneity of the odds ratios across
#' strata is reported in the header. In the widget you can change the
#' standardization (`"margins"`, `"ind.max"`, `"all.max"` as in
#' `vcd::fourfold()`), the confidence level and the display of the rings;
#' hovering a stratum in either panel highlights it in both.
#'
#' @param x a \eqn{2 \times 2} or \eqn{2 \times 2 \times k} table (further
#'   dimensions are crossed into a single stratum variable), or anything
#'   accepted by [mosaic_cells()] except a fitted model.
#' @param std initial standardization.
#' @param conf_level initial confidence level, one of 0.90, 0.95, 0.99.
#' @param main optional title.
#' @param width,height,elementId passed to [htmlwidgets::createWidget()].
#' @return an `htmlwidget`.
#' @examples
#' ifourfold(UCBAdmissions)
#' ifourfold(aperm(UCBAdmissions, c(2, 1, 3)), std = "ind.max")
#' @export
ifourfold <- function(x, std = c("margins", "ind.max", "all.max"), conf_level = 0.95,
                      main = NULL, width = NULL, height = NULL, elementId = NULL) {
  std <- match.arg(std)
  tab <- as_vcdi_table(x)
  d <- dim(tab)
  if (length(d) < 2 || d[1] != 2 || d[2] != 2)
    stop("the first two dimensions of 'x' must have two levels each")
  dn <- dimnames(tab)
  vars <- names(dn)
  if (length(d) == 2) {
    arr <- array(tab, c(2, 2, 1))
    strata <- ""
    svar <- ""
  } else {
    k <- prod(d[-(1:2)])
    arr <- array(tab, c(2, 2, k))
    grid <- expand.grid(dn[-(1:2)], stringsAsFactors = FALSE)
    strata <- do.call(paste, c(grid, sep = " : "))
    svar <- paste(vars[-(1:2)], collapse = " : ")
  }
  k <- dim(arr)[3]
  st <- lapply(seq_len(k), function(s) {
    n <- arr[, , s]
    nn <- if (any(n == 0)) n + 0.5 else n
    lor <- log(nn[1, 1] * nn[2, 2] / (nn[1, 2] * nn[2, 1]))
    se <- sqrt(sum(1 / nn))
    list(label = strata[s], n = I(as.vector(n)), logor = lor, se = se,
         p = 2 * stats::pnorm(-abs(lor / se)))
  })
  ## Woolf test of homogeneity and Mantel-Haenszel common odds ratio
  tests <- list()
  if (k > 1) {
    lor <- vapply(st, `[[`, 0, "logor")
    w <- 1 / vapply(st, `[[`, 0, "se")^2
    chi <- sum(w * (lor - stats::weighted.mean(lor, w))^2)
    tests$woolf <- list(chisq = chi, df = k - 1, p = stats::pchisq(chi, k - 1, lower.tail = FALSE))
    mh <- tryCatch(stats::mantelhaen.test(arr), error = function(e) NULL)
    if (!is.null(mh))
      tests$mh <- list(logor = log(unname(mh$estimate)), lo = log(mh$conf.int[1]),
                       hi = log(mh$conf.int[2]), chisq = unname(mh$statistic), p = mh$p.value)
  }
  payload <- list(
    main = if (is.null(main)) "" else main,
    row = list(name = vars[1], levels = I(dn[[1]])),
    col = list(name = vars[2], levels = I(dn[[2]])),
    stratum = svar, strata = st, tests = tests,
    settings = list(std = std, conf = conf_level))
  vcdi_widget("ifourfold", payload, width, height, elementId, default_height = 620)
}
