## ---------------------------------------------------------------------------
## Statistical engine: fitted values, residuals and cell-wise tests
## ---------------------------------------------------------------------------

#' Residual types available in vcdinteractive
#'
#' Character vector with the names of the residuals computed by
#' [mosaic_cells()]. They are:
#' \describe{
#'   \item{`pearson`}{\eqn{(o - e)/\sqrt{e}}, the residuals used by default
#'     in `vcd::mosaic()`. Their variance is smaller than one, so they are
#'     conservative when compared with normal quantiles.}
#'   \item{`standardized`}{adjusted (Haberman) residuals
#'     \eqn{(o - e)/\sqrt{e(1 - h)}}, where \eqn{h} is the leverage of the
#'     cell in the Poisson log-linear model. Asymptotically N(0, 1): the
#'     recommended choice for cell-wise testing.}
#'   \item{`deviance`}{signed square roots of the cell contributions to
#'     \eqn{G^2}.}
#'   \item{`freeman_tukey`}{\eqn{\sqrt{o} + \sqrt{o + 1} - \sqrt{4e + 1}},
#'     variance-stabilized; better behaved than Pearson residuals with small
#'     expected counts.}
#'   \item{`anscombe`}{Anscombe residuals for the Poisson,
#'     \eqn{1.5 (o^{2/3} - e^{2/3}) / e^{1/6}}, closest to normality.}
#'   \item{`robust`}{Pearson-type residuals \eqn{(o - \tilde e)/\sqrt{\tilde e}}
#'     where \eqn{\tilde e} comes from a robust fit of the same model: by
#'     default a Poisson GLM fitted by robust quasi-likelihood
#'     (`robustbase::glmrob(method = "Mqle")`, Cantoni and Ronchetti, 2001);
#'     alternatively a median polish of the log-counts (mutual independence
#'     only; fast but unreliable for sparse tables). Outlying cells do not
#'     pull the fitted values towards themselves, which avoids the masking of
#'     deviations that affects the classical fit.}
#' }
#' @export
residual_types <- c("pearson", "standardized", "deviance",
                    "freeman_tukey", "anscombe", "robust")

## convert the many possible inputs to a named-dimnames table ----------------
as_vcdi_table <- function(x) {
  if (inherits(x, "structable")) x <- as.table(x)
  if (inherits(x, "ftable")) x <- as.table(x)
  if (is.data.frame(x)) {
    if ("Freq" %in% names(x)) {
      fac <- setdiff(names(x), "Freq")
      x <- stats::xtabs(stats::as.formula(
        paste("Freq ~", paste(sprintf("`%s`", fac), collapse = " + "))),
        data = x)
    } else {
      x <- table(x)
    }
  }
  if (is.matrix(x) || is.array(x)) x <- as.table(x)
  if (!inherits(x, "table")) stop("'x' must be a table, array, ftable, structable or data frame")
  dn <- dimnames(x)
  if (is.null(names(dn)) || any(names(dn) == "")) {
    nm <- names(dn)
    if (is.null(nm)) nm <- rep("", length(dn))
    nm[nm == ""] <- paste0("V", which(nm == ""))
    names(dn) <- nm
    dimnames(x) <- dn
  }
  for (i in seq_along(dn)) if (is.null(dn[[i]])) {
    dn[[i]] <- as.character(seq_len(dim(x)[i]))
    dimnames(x) <- dn
  }
  class(x) <- "table"
  x
}

## select / reorder variables using a one-sided formula ------------------------
subset_by_formula <- function(tab, formula) {
  vars <- all.vars(formula)
  if (length(vars) == 1 && vars == ".") return(tab)
  miss <- setdiff(vars, names(dimnames(tab)))
  if (length(miss)) stop("Variable(s) not found: ", paste(miss, collapse = ", "))
  margin.table(tab, match(vars, names(dimnames(tab))))
}

## translate the 'expected' argument into a model formula ---------------------
model_formula <- function(expected, vars) {
  q <- function(v) sprintf("`%s`", v)
  if (is.null(expected))
    return(stats::as.formula(paste(".Freq ~", paste(q(vars), collapse = " + "))))
  if (inherits(expected, "formula")) {
    rhs <- paste(deparse(expected[[length(expected)]]), collapse = " ")
    return(stats::as.formula(paste(".Freq ~", rhs)))
  }
  if (is.list(expected)) {
    terms <- vapply(expected, function(m) {
      if (is.numeric(m)) m <- vars[m]
      paste(q(m), collapse = ":")
    }, character(1))
    return(stats::as.formula(paste(".Freq ~", paste(terms, collapse = " + "))))
  }
  stop("'expected' must be NULL, a formula or a list of margins (as in loglin())")
}

## k-way median polish on log counts (mutual independence) -------------------
medpolish_kway <- function(tab, maxiter = 50, eps = 1e-6) {
  y <- log(as.vector(tab) + 0.5)
  d <- dim(tab)
  idx <- arrayInd(seq_along(y), d)
  overall <- stats::median(y)
  r <- y - overall
  eff <- lapply(d, function(k) numeric(k))
  for (it in seq_len(maxiter)) {
    change <- 0
    for (j in seq_along(d)) {
      m <- vapply(seq_len(d[j]), function(l) stats::median(r[idx[, j] == l]), 0)
      eff[[j]] <- eff[[j]] + m
      r <- r - m[idx[, j]]
      change <- change + sum(abs(m))
    }
    for (j in seq_along(d)) {         # keep effects centred
      m <- stats::median(eff[[j]])
      eff[[j]] <- eff[[j]] - m
      overall <- overall + m
    }
    if (change < eps) break
  }
  fit <- overall + Reduce(`+`, lapply(seq_along(d), function(j) eff[[j]][idx[, j]]))
  exp(fit)
}

robust_fit <- function(tab, df, form, method, mutual) {
  if (method == "none") return(NULL)
  quiet <- method == "auto"
  if (method == "auto")
    method <- if (requireNamespace("robustbase", quietly = TRUE)) "glmrob"
              else if (mutual) "medpolish"
              else "none"
  if (method == "medpolish") {
    if (!mutual) warning("median polish only fits the mutual independence model; ",
                         "use robust = 'glmrob' for other models")
    return(list(fitted = medpolish_kway(tab), method = "medpolish"))
  }
  if (method == "glmrob") {
    if (!requireNamespace("robustbase", quietly = TRUE)) {
      warning("package 'robustbase' not available: robust residuals not computed")
      return(NULL)
    }
    fit <- NULL
    utils::capture.output(fit <- tryCatch(suppressMessages(
      robustbase::glmrob(form, family = stats::poisson(), data = df,
                         method = "Mqle",
                         control = robustbase::glmrobMqle.control(maxit = 200))),
      error = function(e) {
        if (!quiet) warning("robust fit failed: ", conditionMessage(e))
        NULL }))
    if (is.null(fit)) return(NULL)
    return(list(fitted = as.vector(stats::fitted(fit)), method = "glmrob"))
  }
  stop("unknown robust method: ", method)
}

#' Cell statistics for a mosaic plot
#'
#' Fits a Poisson log-linear model to a contingency table (by default the
#' model of mutual independence, as in `vcd::mosaic(..., shade = TRUE)`) and
#' returns, for every cell, the observed and expected counts, several kinds
#' of residuals (see [residual_types]) and two-sided p-values.
#'
#' P-values are obtained by comparing with the standard normal distribution
#' the *adjusted* version of each residual (divided by \eqn{\sqrt{1-h}},
#' \eqn{h} being the leverage of the cell), which is asymptotically N(0, 1).
#' For the robust residuals no leverage correction is applied. P-values are
#' unadjusted: multiplicity corrections are applied in
#' [significant_cells()] and in the widget.
#'
#' @param x a contingency table (`table`, `xtabs`, `ftable`,
#'   `vcd::structable`, array), a data frame either in case form or in
#'   frequency form (with a `Freq` column), or a fitted Poisson log-linear
#'   model (`glm` with `family = poisson`, or `gnm::gnm`) for frequency data,
#'   whose fitted values are then used as expected counts.
#' @param expected the log-linear model giving the expected counts: `NULL`
#'   (mutual independence), a one-sided formula such as
#'   `~ Class * Sex + Survived`, or a list of margins as in [stats::loglin()].
#' @param robust method for the robust fit: `"auto"` (`robustbase::glmrob()`
#'   if 'robustbase' is installed, otherwise median polish for the mutual
#'   independence model), `"glmrob"`, `"medpolish"` or `"none"`.
#' @return a data frame of class `mosaic_cells` with one row per cell. The
#'   attribute `model` holds the global goodness-of-fit statistics.
#' @examples
#' cells <- mosaic_cells(HairEyeColor)
#' head(cells)
#' attr(cells, "model")
#' @export
mosaic_cells <- function(x, expected = NULL, robust = c("auto", "medpolish", "glmrob", "none")) {
  robust <- match.arg(robust)
  if (inherits(x, "glm")) return(glm_cells(x, robust))
  tab <- as_vcdi_table(x)
  vars <- names(dimnames(tab))
  df <- as.data.frame(tab, responseName = ".Freq", stringsAsFactors = TRUE)
  form <- model_formula(expected, vars)
  fit <- tryCatch(suppressWarnings(stats::glm(form, family = stats::poisson(), data = df)),
                  error = function(e) NULL)
  rob <- robust_fit(tab, df, form, robust, is.null(expected))
  if (is.null(fit)) {
    ## glm failed (typically sampling zeros making some parameters infinite):
    ## fall back on iterative proportional fitting, without leverages
    m <- model_margins(if (is.null(expected)) NULL else expected, vars)
    ll <- stats::loglin(tab, lapply(m, match, vars), fit = TRUE, print = FALSE, iter = 1000, eps = 1e-8)
    e <- as.vector(ll$fit)
    return(cells_from_fit(df[vars], df$.Freq, e, rep(NA_real_, length(e)), rob,
                          formula = stats::update(form, NULL ~ .), G2 = ll$lrt, df = ll$df,
                          aic = -2 * sum(stats::dpois(df$.Freq, e, log = TRUE)) + 2 * (length(e) - ll$df)))
  }
  cells_from_fit(df[vars], df$.Freq, as.vector(stats::fitted(fit)),
                 stats::hatvalues(fit), rob,
                 formula = stats::update(form, NULL ~ .),
                 G2 = stats::deviance(fit), df = fit$df.residual, aic = fit$aic)
}

## cell statistics from observed, fitted values and leverages ----------------
cells_from_fit <- function(levels_df, o, e, h, rob, formula, G2, df, aic = NA_real_) {
  vars <- names(levels_df)
  h <- unname(as.numeric(h))
  h[!is.na(h) & h > 1 - 1e-10] <- NA      # saturated cells: no information
  sq1h <- sqrt(1 - h)

  pear <- (o - e) / sqrt(e)
  dev  <- sign(o - e) * sqrt(pmax(0, 2 * (ifelse(o > 0, o * log(o / e), 0) - (o - e))))
  ft   <- sqrt(o) + sqrt(o + 1) - sqrt(4 * e + 1)
  ans  <- 1.5 * (o^(2/3) - e^(2/3)) / e^(1/6)

  res <- data.frame(levels_df, observed = o, expected = e, check.names = FALSE)
  res$pearson       <- pear
  res$standardized  <- pear / sq1h
  res$deviance      <- dev
  res$freeman_tukey <- ft
  res$anscombe      <- ans
  pz <- function(z) ifelse(is.na(z), 1, 2 * stats::pnorm(-abs(z)))
  res$p_pearson       <- pz(pear / sq1h)
  res$p_standardized  <- pz(pear / sq1h)
  res$p_deviance      <- pz(dev / sq1h)
  res$p_freeman_tukey <- pz(ft / sq1h)
  res$p_anscombe      <- pz(ans / sq1h)

  if (!is.null(rob)) {
    res$expected_robust <- rob$fitted
    res$robust   <- (o - rob$fitted) / sqrt(rob$fitted)
    res$p_robust <- pz(res$robust)
  } else {
    res$expected_robust <- NA_real_
    res$robust   <- NA_real_
    res$p_robust <- NA_real_
  }
  res$leverage <- h
  for (v in c("standardized", "deviance", "freeman_tukey", "anscombe", "pearson"))
    res[[v]][is.na(res[[v]])] <- 0

  X2 <- sum(pear^2)
  G2 <- max(0, G2)
  attr(res, "model") <- list(
    formula = formula,
    X2 = X2, G2 = G2, df = df,
    p_X2 = stats::pchisq(X2, df, lower.tail = FALSE),
    p_G2 = stats::pchisq(G2, df, lower.tail = FALSE),
    aic = aic, n = sum(o),
    robust = if (is.null(rob)) "none" else rob$method)
  attr(res, "vars") <- vars
  class(res) <- c("mosaic_cells", "data.frame")
  res
}

## cell statistics from a fitted Poisson glm (or gnm) on frequency data -------
glm_cells <- function(fit, robust = "auto") {
  fam <- tryCatch(stats::family(fit)$family, error = function(e) "")
  if (!identical(fam, "poisson"))
    stop("the model must be a Poisson log-linear model (family = poisson)")
  mf <- stats::model.frame(fit)
  o <- as.vector(stats::model.response(mf))
  vars <- names(mf)[vapply(mf, is.factor, logical(1))]
  if (!length(vars)) stop("the model has no factor among its terms")
  tab <- stats::xtabs(o ~ ., data = mf[vars])
  df <- as.data.frame(tab, responseName = ".Freq", stringsAsFactors = TRUE)
  key  <- do.call(paste, c(lapply(mf[vars], as.character), sep = "\r"))
  keyt <- do.call(paste, c(lapply(df[vars], as.character), sep = "\r"))
  m <- match(keyt, key)
  if (anyNA(m) || anyDuplicated(key))
    stop("the model frame must contain each cell of the table exactly once")
  e <- as.vector(stats::fitted(fit))[m]
  h <- tryCatch(as.vector(stats::hatvalues(fit))[m], error = function(e) rep(NA_real_, length(m)))
  rob <- NULL
  if (robust != "none" && identical(class(fit)[1], "glm") &&
      requireNamespace("robustbase", quietly = TRUE)) {
    d2 <- mf; d2$.Freq <- o
    form <- stats::update(stats::formula(fit), .Freq ~ .)
    rr <- robust_fit(tab, d2, form, "glmrob", FALSE)
    if (!is.null(rr)) rob <- list(fitted = rr$fitted[m], method = rr$method)
  }
  cells_from_fit(df[vars], df$.Freq, e, h, rob,
                 formula = stats::update(stats::formula(fit), NULL ~ .),
                 G2 = stats::deviance(fit), df = stats::df.residual(fit),
                 aic = tryCatch(stats::AIC(fit), error = function(e) NA_real_))
}

#' @export
print.mosaic_cells <- function(x, digits = 3, ...) {
  m <- attr(x, "model")
  if (is.null(m)) {                    # subset of a mosaic_cells object
    print(as.data.frame(unclass(x), check.names = FALSE), digits = digits, ...)
    return(invisible(x))
  }
  cat("Log-linear model:", deparse(m$formula), "\n")
  cat(sprintf("Pearson X2 = %.2f, G2 = %.2f, df = %d, p(X2) = %s\n",
              m$X2, m$G2, m$df, format.pval(m$p_X2, digits = 3)))
  cat("Robust fit:", m$robust, "\n\n")
  y <- x
  class(y) <- "data.frame"
  attr(y, "model") <- attr(y, "vars") <- NULL
  num <- vapply(y, is.numeric, logical(1))
  y[num] <- lapply(y[num], signif, digits = digits)
  print(y, ...)
  invisible(x)
}

#' Cells that depart significantly from the fitted model
#'
#' Returns the combinations of categories whose residual is significantly
#' different from zero, after a multiple-testing correction across all
#' cells of the table, sorted by decreasing absolute residual.
#'
#' @param x a table (or anything accepted by [mosaic_cells()]) or an object
#'   returned by [mosaic_cells()].
#' @param type residual type, one of [residual_types].
#' @param alpha significance level.
#' @param adjust multiplicity correction, passed to [stats::p.adjust()]
#'   (`"holm"`, `"bonferroni"`, `"BH"`, `"none"`, ...).
#' @param ... further arguments passed to [mosaic_cells()].
#' @return a data frame with the categories, observed and expected counts,
#'   the residual, the raw and adjusted p-values and the direction of the
#'   departure (`"over"`/`"under"`-represented).
#' @examples
#' significant_cells(HairEyeColor)
#' significant_cells(HairEyeColor, type = "robust", adjust = "BH")
#' @export
significant_cells <- function(x, type = "standardized", alpha = 0.05,
                              adjust = "holm", ...) {
  type <- match.arg(type, residual_types)
  cells <- if (inherits(x, "mosaic_cells")) x else mosaic_cells(x, ...)
  vars <- attr(cells, "vars")
  p <- cells[[paste0("p_", type)]]
  if (all(is.na(p))) stop("residuals of type '", type, "' are not available")
  padj <- stats::p.adjust(p, method = adjust)
  out <- data.frame(cells[vars], observed = cells$observed,
                    expected = if (type == "robust") cells$expected_robust else cells$expected,
                    residual = cells[[type]], p_value = p, p_adjusted = padj,
                    check.names = FALSE)
  out$direction <- ifelse(out$residual > 0, "over", "under")
  out <- out[!is.na(padj) & padj < alpha, , drop = FALSE]
  out <- out[order(-abs(out$residual)), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "type") <- type
  attr(out, "adjust") <- adjust
  attr(out, "alpha") <- alpha
  out
}
