## correspondence analysis core (principal / standard coordinates) ----------
ca_core <- function(N) {
  P <- N / sum(N)
  r <- rowSums(P); c <- colSums(P)
  keep_r <- r > 0; keep_c <- c > 0
  P <- P[keep_r, keep_c, drop = FALSE]; r <- r[keep_r]; c <- c[keep_c]
  S <- (P - outer(r, c)) / sqrt(outer(r, c))
  sv <- svd(S)
  k <- max(1, min(dim(P)) - 1)
  d <- sv$d[seq_len(k)]
  list(sv = d,
       rowstd = sv$u[, seq_len(k), drop = FALSE] / sqrt(r),
       colstd = sv$v[, seq_len(k), drop = FALSE] / sqrt(c),
       rmass = r, cmass = c, keep_r = keep_r, keep_c = keep_c)
}

point_stats <- function(std, pc, mass) {
  d2 <- rowSums(pc^2)
  list(contrib = mass * std^2, cos2 = pc^2 / pmax(d2, 1e-300))
}

## multiple correspondence analysis from the Burt matrix --------------------
mca_core <- function(tab) {
  vars <- names(dimnames(tab)); Q <- length(vars)
  labs <- unlist(lapply(vars, function(v) dimnames(tab)[[v]]))
  grp <- rep(seq_len(Q), dim(tab))
  J <- length(labs)
  B <- matrix(0, J, J)
  off <- c(0, cumsum(dim(tab)))
  for (i in seq_len(Q)) for (j in seq_len(Q)) {
    blk <- if (i == j) diag(as.vector(margin.table(tab, i)), nrow = dim(tab)[i])
           else unclass(margin.table(tab, c(i, j)))
    B[off[i] + seq_len(dim(tab)[i]), off[j] + seq_len(dim(tab)[j])] <- blk
  }
  cc <- ca_core(B)
  lamZ <- cc$sv                       # principal inertias of the indicator matrix
  std <- cc$colstd
  pc <- sweep(std, 2, sqrt(lamZ), "*")
  ## Greenacre's adjusted inertias
  adj <- ifelse(lamZ > 1 / Q, (Q / (Q - 1) * (lamZ - 1 / Q))^2, 0)
  tot <- Q / (Q - 1) * (sum(lamZ^2) - (J - Q) / Q^2)
  list(labels = labs[cc$keep_c], group = grp[cc$keep_c], vars = vars,
       std = std, pc = pc, mass = cc$cmass,
       inertia = adj, percent = 100 * adj / tot, method = "MCA (Burt), adjusted inertias")
}

#' Interactive correspondence analysis map
#'
#' An interactive version of the plots of `vcdExtra::mcaplot()` and of
#' `plot()` for correspondence analysis. For a two-way table a simple
#' correspondence analysis is computed; for three or more variables a
#' multiple correspondence analysis (from the Burt matrix), whose
#' percentages of inertia are Greenacre's adjusted ones. Category points are
#' in principal coordinates of the indicator analysis. Objects returned by
#' `ca::ca()` and `ca::mjca()` can also be passed.
#'
#' In the widget you choose the dimensions on the axes, show or hide each
#' variable (click its legend entry), join the categories of each variable
#' with lines, zoom with the mouse wheel, pan by dragging and reset with a
#' double click. Labels that would overlap are hidden and appear on hover;
#' the tooltip reports mass, coordinates, contributions and the quality of
#' representation (squared cosines) on the two displayed dimensions.
#'
#' @param x a contingency table (or anything accepted by [mosaic_cells()]
#'   except a fitted model), or a `ca` or `mjca` object.
#' @param dims dimensions shown at start-up.
#' @param nd maximum number of dimensions made available in the widget.
#' @param lines join the categories of each variable at start-up.
#' @param main optional title.
#' @param width,height,elementId passed to [htmlwidgets::createWidget()].
#' @return an `htmlwidget`.
#' @examples
#' imcaplot(margin.table(HairEyeColor, 1:2))
#' imcaplot(HairEyeColor)
#' @export
imcaplot <- function(x, dims = c(1, 2), nd = 5, lines = TRUE, main = NULL,
                     width = NULL, height = NULL, elementId = NULL) {
  if (inherits(x, "mjca")) {
    std <- x$colcoord; sv <- x$sv
    k <- min(ncol(std), length(sv))
    std <- std[, seq_len(k), drop = FALSE]
    pc <- sweep(std, 2, sv[seq_len(k)], "*")
    lv <- x$levelnames
    fac <- if (!is.null(x$factors)) x$factors[, "factor"] else sub(":.*", "", lv)
    lab <- if (!is.null(x$factors)) x$factors[, "level"] else sub("^[^:]*:", "", lv)
    vars <- unique(fac)
    res <- list(labels = lab, group = match(fac, vars), vars = vars, std = std, pc = pc,
                mass = x$colmass, inertia = sv^2,
                percent = 100 * sv^2 / x$inertia.t, method = "ca::mjca")
  } else if (inherits(x, "ca")) {
    k <- length(x$sv)
    rs <- x$rowcoord[, seq_len(k), drop = FALSE]; cs <- x$colcoord[, seq_len(k), drop = FALSE]
    vn <- if (!is.null(x$DF)) names(x$DF) else c("Rows", "Columns")
    vars <- if (length(vn) == 2 && all(nzchar(vn))) vn else c("Rows", "Columns")
    res <- list(labels = c(x$rownames, x$colnames),
                group = c(rep(1L, nrow(rs)), rep(2L, nrow(cs))), vars = vars,
                std = rbind(rs, cs), pc = rbind(sweep(rs, 2, x$sv, "*"), sweep(cs, 2, x$sv, "*")),
                mass = c(x$rowmass, x$colmass), inertia = x$sv^2,
                percent = 100 * x$sv^2 / sum(x$sv^2), method = "ca::ca")
  } else {
    tab <- as_vcdi_table(x)
    if (length(dim(tab)) == 2) {
      cc <- ca_core(unclass(tab))
      vars <- names(dimnames(tab))
      res <- list(labels = c(rownames(tab)[cc$keep_r], colnames(tab)[cc$keep_c]),
                  group = c(rep(1L, sum(cc$keep_r)), rep(2L, sum(cc$keep_c))), vars = vars,
                  std = rbind(cc$rowstd, cc$colstd),
                  pc = rbind(sweep(cc$rowstd, 2, cc$sv, "*"), sweep(cc$colstd, 2, cc$sv, "*")),
                  mass = c(cc$rmass, cc$cmass), inertia = cc$sv^2,
                  percent = 100 * cc$sv^2 / sum(cc$sv^2), method = "simple CA")
    } else res <- mca_core(tab)
  }
  k <- min(nd, ncol(res$pc))
  if (k < 1) stop("no dimension to display")
  pc <- res$pc[, seq_len(k), drop = FALSE]
  st <- point_stats(res$std[, seq_len(k), drop = FALSE], res$pc, res$mass)
  ## contributions are relative to each set of points in simple CA
  if (identical(res$method, "simple CA") || identical(res$method, "ca::ca")) {
    for (gg in 1:2) {
      w <- res$group == gg
      st$contrib[w, ] <- sweep(st$contrib[w, , drop = FALSE], 2,
                               pmax(colSums(st$contrib[w, , drop = FALSE]), 1e-12), "/")
    }
  } else st$contrib <- sweep(st$contrib, 2, pmax(colSums(st$contrib), 1e-12), "/")
  pts <- lapply(seq_along(res$labels), function(i)
    list(label = unname(res$labels[i]), group = unname(res$group[i]) - 1L, mass = unname(res$mass[i]),
         coord = I(unname(pc[i, ])), contrib = I(unname(st$contrib[i, seq_len(k)])),
         cos2 = I(unname(st$cos2[i, seq_len(k)]))))
  payload <- list(main = if (is.null(main)) "" else main, method = res$method,
                  vars = I(unname(res$vars)), points = pts,
                  inertia = I(unname(res$inertia[seq_len(k)])),
                  percent = I(unname(res$percent[seq_len(k)])),
                  settings = list(dims = I(pmin(dims, k) - 1L), lines = lines))
  vcdi_widget("imcaplot", payload, width, height, elementId, default_height = 600)
}
