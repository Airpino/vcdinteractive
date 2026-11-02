test_that("Pearson and adjusted residuals match the closed forms", {
  tab <- margin.table(HairEyeColor, 1:2)
  cells <- mosaic_cells(tab)
  e <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  expect_equal(cells$expected, as.vector(e), tolerance = 1e-8)
  expect_equal(cells$pearson, as.vector((tab - e) / sqrt(e)), tolerance = 1e-8)
  # Haberman adjusted residuals for a two-way table
  pr <- rowSums(tab) / sum(tab); pc <- colSums(tab) / sum(tab)
  adj <- (tab - e) / sqrt(e * outer(1 - pr, 1 - pc))
  expect_equal(cells$standardized, as.vector(adj), tolerance = 1e-6)
  expect_equal(attr(cells, "model")$X2, unname(chisq.test(tab)$statistic), tolerance = 1e-8)
})

test_that("expected counts agree with vcd/loglin for a 3-way table", {
  cells <- mosaic_cells(HairEyeColor)
  ll <- loglin(HairEyeColor, list(1, 2, 3), fit = TRUE, print = FALSE)
  expect_equal(cells$expected, as.vector(ll$fit), tolerance = 1e-6)
  expect_equal(attr(cells, "model")$df, ll$df)
})

test_that("custom models and inputs work", {
  m <- mosaic_cells(Titanic, expected = ~ Class * Sex * Age + Survived)
  m2 <- mosaic_cells(Titanic, expected = list(1:3, 4))
  expect_equal(m$expected, m2$expected, tolerance = 1e-6)
  df <- as.data.frame(UCBAdmissions)
  expect_equal(mosaic_cells(df)$observed, as.vector(UCBAdmissions))
})

test_that("robust fit unmasks a planted outlier", {
  set.seed(1)
  tab <- as.table(matrix(rpois(25, 40), 5, dimnames = list(A = letters[1:5], B = LETTERS[1:5])))
  tab[2, 3] <- 400
  cells <- mosaic_cells(tab)
  k <- which(cells$A == "b" & cells$B == "C")
  # the outlier is the largest robust residual by far, and robust residuals of
  # the other cells are not dragged away from zero as the classical ones are
  expect_equal(which.max(abs(cells$robust)), k)
  expect_lt(median(abs(cells$robust[-k])), median(abs(cells$pearson[-k])))
})

test_that("significant_cells filters and sorts", {
  s <- significant_cells(HairEyeColor, alpha = 0.05, adjust = "holm")
  expect_true(all(s$p_adjusted < 0.05))
  expect_false(is.unsorted(rev(abs(s$residual))))
  expect_true(all(s$direction %in% c("over", "under")))
})

test_that("imosaic returns an htmlwidget", {
  w <- imosaic(HairEyeColor)
  expect_s3_class(w, "htmlwidget")
  w2 <- imosaic(~ Class + Survived, data = Titanic)
  expect_equal(length(w2$x$vars), 2)
  expect_equal(unclass(w$x$settings$direction), c("h", "v", "h"))
})

test_that("sub-tables and projected models are precomputed", {
  w <- imosaic(UCBAdmissions, expected = ~ Admit * Dept + Gender * Dept)
  expect_length(w$x$tables, 7)                       # 2^3 - 1 subsets
  # collapsing over Dept: [AD][GD] becomes independence of Admit and Gender
  ag <- w$x$tables[["0|0-1"]]
  expect_equal(ag$model$X2, unname(chisq.test(margin.table(UCBAdmissions, 1:2),
                                              correct = FALSE)$statistic), tolerance = 1e-6)
  expect_equal(vcdinteractive:::project_margins(list(c("A", "D"), c("G", "D")), c("A", "G")),
               list("A", "G"))
  expect_equal(vcdinteractive:::project_margins(list(c("A", "D"), c("G", "D")), c("A", "D")),
               list(c("A", "D")))
  s <- imosaic(Titanic, subsets = "sequential", include = c("Class", "Sex"))
  expect_setequal(names(s$x$tables), c("0|0", "0|0-1", "0|0-1-2", "0|0-1-2-3"))
  expect_equal(unclass(s$x$settings$active), c(TRUE, TRUE, FALSE, FALSE))
})

test_that("length-1 vectors are serialized as JSON arrays", {
  w <- imosaic(UCBAdmissions)
  js <- htmlwidgets:::toJSON(w$x)
  expect_match(js, '"0\\|0":\\{"vars":\\[0\\]')
  expect_match(js, '"active":\\[true,true,true\\]')
})

test_that("display modes, models and fitted glm are accepted", {
  w <- imosaic(Titanic, display = "doubledecker", models = c("mutual", "joint", "saturated"))
  expect_equal(w$x$settings$display, "doubledecker")
  expect_length(w$x$models, 3)
  expect_true(all(c("0|0-1-2-3", "2|0-1-2-3") %in% names(w$x$tables)))
  expect_equal(w$x$tables[["2|0-1-2-3"]]$model$df, 0)
  fit <- glm(Freq ~ Admit * Dept + Gender * Dept, family = poisson,
             data = as.data.frame(UCBAdmissions))
  cg <- mosaic_cells(fit)
  ct <- mosaic_cells(UCBAdmissions, expected = ~ Admit * Dept + Gender * Dept)
  key_g <- paste(cg$Admit, cg$Gender, cg$Dept); key_t <- paste(ct$Admit, ct$Gender, ct$Dept)
  expect_equal(cg$expected[match(key_t, key_g)], ct$expected, tolerance = 1e-6)
  expect_s3_class(imosaic(fit), "htmlwidget")
})

test_that("the other widgets are built and their statistics are right", {
  expect_s3_class(ipairs(HairEyeColor), "htmlwidget")
  f <- ifourfold(UCBAdmissions)
  lor <- log(UCBAdmissions[1, 1, ] * UCBAdmissions[2, 2, ] / (UCBAdmissions[1, 2, ] * UCBAdmissions[2, 1, ]))
  expect_equal(vapply(f$x$strata, `[[`, 0, "logor"), unname(lor))
  if (requireNamespace("vcd", quietly = TRUE))
    expect_equal(f$x$tests$woolf$chisq, unname(vcd::woolf_test(UCBAdmissions)$statistic), tolerance = 1e-6)
  horse <- c("0" = 109, "1" = 65, "2" = 22, "3" = 3, "4" = 1)
  r <- irootogram(horse)
  expect_equal(unclass(r$x$fits[[1]]$expected)[1], 200 * dpois(0, 0.61), tolerance = 1e-6)
  expect_s3_class(icdplot(Species ~ Sepal.Length, data = iris), "htmlwidget")
  ca2 <- imcaplot(margin.table(HairEyeColor, 1:2))
  pct <- unclass(ca2$x$percent)
  expect_equal(sum(pct), 100, tolerance = 1e-6)
  g <- iassoc_graph(UCBAdmissions, models = list(a = ~ Admit * Dept + Gender * Dept))
  e <- g$x$models[[1]]$edges
  expect_length(e, 2)
})
