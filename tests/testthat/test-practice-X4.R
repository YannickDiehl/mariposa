# =============================================================================
# 0.7.4 practice-test fixes — batch X4 (SPSS parity, unweighted):
# ancova() / levene_test() Levene test, efa() rotations and ML extraction
# =============================================================================
# One block per finding. The SPSS reference numbers themselves are asserted
# in the *-spss-validation.R files (assert_spss(), Charter tiers); the blocks
# here pin the algorithms against independent oracles.

data(survey_data, envir = environment())

# --- X4-ANCOVA-LEVENE: Levene's test on the full-model residuals -------------

# SPSS UNIANOVA (/PRINT HOMOGENEITY) tests the equality of the ERROR
# variances: a one-way ANOVA of the absolute residuals of the full model
# (covariates + factors) across the design cells.
.levene_resid_oracle <- function(dv, factors, covariates, data = survey_data) {
  d <- as.data.frame(data)
  d <- d[stats::complete.cases(d[c(dv, factors, covariates)]), ]
  fml <- stats::reformulate(c(covariates, paste(factors, collapse = "*")), dv)
  z <- abs(stats::residuals(stats::lm(fml, data = d)))
  cell <- interaction(d[factors], drop = TRUE)
  stats::anova(stats::lm(z ~ cell))
}

test_that("X4-ANCOVA-LEVENE: unweighted Levene uses the residuals of the covariate model", {
  # mariposa centred the DV on the raw cell means and ignored the covariate:
  # F = 1.277 instead of SPSS 1.306 for life_satisfaction BY gender WITH age.
  r <- ancova(survey_data, dv = life_satisfaction, between = gender, covariate = age)
  ref <- .levene_resid_oracle("life_satisfaction", "gender", "age")
  expect_equal(r$levene_test$f, ref[1, "F value"], tolerance = 1e-10)
  expect_equal(r$levene_test$p, ref[1, "Pr(>F)"], tolerance = 1e-10)
  expect_identical(r$levene_test$df1, 1L)
  expect_identical(r$levene_test$df2, as.integer(ref[2, "Df"]))

  # two factors and two covariates: the cells are the factor combinations
  r2 <- ancova(survey_data, dv = income, between = c(gender, education),
               covariate = c(age, political_orientation))
  ref2 <- .levene_resid_oracle("income", c("gender", "education"),
                               c("age", "political_orientation"))
  expect_equal(r2$levene_test$f, ref2[1, "F value"], tolerance = 1e-10)
  expect_identical(r2$levene_test$df1, 7L)
})

test_that("X4-ANCOVA-LEVENE: weighted ANCOVA still reports its Levene test", {
  # The weighted (/REGWGT) path is left unchanged pending a WEIGHT BY run.
  r <- ancova(survey_data, dv = life_satisfaction, between = gender,
              covariate = age, weights = sampling_weight)
  expect_true(is.finite(r$levene_test$f))
  expect_identical(r$levene_test$df1, 1L)
  expect_output(print(summary(r)), "Levene's Test of Equality of Error Variances")
})

test_that("X4-ANCOVA-LEVENE: levene_test() works on ancova() results", {
  # levene_test(ancova_result) failed: "not available for objects of class
  # ancova" (the error even listed only oneway/factorial/t_test).
  r <- ancova(survey_data, dv = life_satisfaction, between = c(gender, region),
              covariate = age)
  lv <- levene_test(r)
  expect_s3_class(lv, "levene_test")
  expect_equal(lv$results$F_statistic, r$levene_test$f)
  expect_equal(lv$results$p_value, r$levene_test$p)
  expect_identical(lv$results$df1, r$levene_test$df1)
  expect_identical(lv$results$Variable, "life_satisfaction")
  expect_identical(lv$group, "gender * region")
  expect_output(print(lv), "Levene's Test: life_satisfaction by gender \\* region")
  out <- capture.output(summary(lv))
  expect_true(any(grepl("Levene Statistic", out)))
  # factorial design: no Welch recommendation
  expect_false(any(grepl("Welch", out)))

  # SPSS UNIANOVA has only the mean-based test for a model with covariates
  expect_error(levene_test(r, center = "median"), "mean")

  # grouped ancova: one row per group, with the group keys
  g <- ancova(dplyr::group_by(survey_data, region), dv = life_satisfaction,
              between = gender, covariate = age)
  lg <- levene_test(g)
  expect_identical(nrow(lg$results), 2L)
  expect_true("region" %in% names(lg$results))
  east <- ancova(dplyr::filter(survey_data, region == "East"),
                 dv = life_satisfaction, between = gender, covariate = age)
  expect_equal(lg$results$F_statistic[as.character(lg$results$region) == "East"],
               east$levene_test$f)
  expect_output(print(lg), "region = East")

  # weighted ancova: the method returns the stored (weighted) test
  w <- ancova(survey_data, dv = life_satisfaction, between = gender,
              covariate = age, weights = sampling_weight)
  expect_equal(levene_test(w)$results$F_statistic, w$levene_test$f)
  expect_identical(levene_test(w)$weights, "sampling_weight")
})

# --- X4-PROMAX: SPSS FACTOR promax rotation -----------------------------------

.six_items <- function(d, ...) {
  efa(d, political_orientation, environmental_concern, life_satisfaction,
      trust_government, trust_media, trust_science, ...)
}

test_that("X4-PROMAX: the promax target is built from Kaiser-normalized loadings", {
  # stats::promax() raises the RAW varimax loadings to the power k; SPSS
  # (IBM SPSS Statistics Algorithms, FACTOR "Promax Rotation") first divides
  # every row by its length (Kaiser normalization). The factor correlations
  # were .055/.155 instead of SPSS -.002/-.012 (efa_ml_promax_output.txt P1).
  r <- .six_items(survey_data, rotation = "promax")

  # SPSS algorithm, step by step, on SPSS's varimax solution
  V <- unname(.six_items(survey_data, rotation = "varimax")$loadings)
  B <- V / sqrt(rowSums(V^2))
  P <- abs(B)^(4 + 1) / B
  L <- solve(t(V) %*% V) %*% t(V) %*% P
  Q <- L %*% diag(1 / sqrt(diag(t(L) %*% L)))
  QQi <- solve(t(Q) %*% Q)
  C <- diag(1 / sqrt(diag(QQi)))
  pattern <- V %*% Q %*% solve(C)
  phi <- C %*% QQi %*% t(C)

  expect_equal(unname(r$pattern_matrix), unname(pattern), tolerance = 1e-8)
  expect_equal(unname(r$factor_correlations), unname(phi), tolerance = 1e-8)
  expect_equal(unname(r$structure_matrix), unname(pattern %*% phi), tolerance = 1e-8)
  # not stats::promax()
  old <- unclass(stats::promax(r$unrotated_loadings, m = 4)$loadings)
  expect_gt(max(abs(unname(r$pattern_matrix) - unname(old))), 0.01)
  expect_identical(dimnames(r$pattern_matrix),
                   list(r$variables, paste0("PC", 1:3)))
})

test_that("X4-PROMAX: oblique rotation sums of squares come from the structure matrix", {
  # SPSS "Rotation Sums of Squared Loadings" for correlated factors are the
  # column sums of squares of the STRUCTURE matrix (P1: 1.599, 1.039, 1.021;
  # the pattern matrix gave 1.604, 1.065, 1.045).
  r <- .six_items(survey_data, rotation = "promax")
  expect_equal(r$rotation_variance$ss_loading,
               unname(colSums(r$structure_matrix^2)))
  o <- .six_items(survey_data, rotation = "oblimin")
  expect_equal(o$rotation_variance$ss_loading,
               unname(colSums(o$structure_matrix^2)))
})

# --- X4-VARIMAX / X4-OBLIMIN: SPSS's rotation algorithms ----------------------

.varimax_criterion <- function(L) {
  A <- L / sqrt(rowSums(L^2))
  n <- nrow(A)
  sum(n * colSums(A^4) - colSums(A^2)^2) / n^2
}

test_that("X4-VARIMAX: SPSS's cyclic varimax, reflected and ordered", {
  # stats::varimax() stopped early (relative criterion) and missed SPSS's
  # Component Transformation Matrix by up to .004 (efa_output.txt 1d); it
  # also kept the unrotated factor order where SPSS orders the rotated
  # factors by their sums of squares and reflects negative-sum factors.
  r <- .six_items(survey_data, n_factors = 2)
  T_r <- qr.solve(r$unrotated_loadings, r$loadings)
  expect_equal(unname(crossprod(T_r)), diag(2), tolerance = 1e-10)
  # at the varimax optimum up to SPSS's criterion (improvement <= 1e-5)
  best <- unclass(stats::varimax(r$unrotated_loadings, eps = 1e-14)$loadings)
  expect_lt(.varimax_criterion(best) - .varimax_criterion(r$loadings), 1e-5)
  expect_identical(r$rotation_iterations, 3L)
})

test_that("X4-VARIMAX: a rotation that hits the iteration limit warns", {
  L <- .six_items(survey_data)$unrotated_loadings
  vm <- mariposa:::.efa_varimax(L, maxit = 1L)
  expect_false(vm$converged)
  expect_warning(mariposa:::.efa_warn_rotation(vm, "Varimax", "region = East"),
                 "Varimax rotation failed to converge in 1 iterations \\(group region = East\\)")
  expect_silent(mariposa:::.efa_warn_rotation(mariposa:::.efa_varimax(L), "Varimax"))
})

test_that("X4-OBLIMIN: SPSS's direct oblimin needs no GPArotation", {
  # GPArotation::oblimin() (a Suggests package, required before) iterated
  # to the exact optimum; SPSS stops when the quartimin criterion improves
  # by less than 1e-4 of its start value (2b differed by up to .002).
  r <- .six_items(survey_data, rotation = "oblimin")
  quartimin <- function(P) {
    B <- P / sqrt(rowSums(P^2))
    sum(rowSums(B^2)^2 - rowSums(B^4))
  }
  expect_lt(quartimin(r$pattern_matrix), quartimin(r$unrotated_loadings))
  expect_equal(unname(diag(r$factor_correlations)), rep(1, 3))
  expect_equal(unname(r$structure_matrix),
               unname(r$pattern_matrix %*% r$factor_correlations))
  # the pattern reproduces the unrotated common-factor space:
  # P Phi P' = L L' (same model-implied correlations)
  expect_equal(unname(r$pattern_matrix %*% r$factor_correlations %*% t(r$pattern_matrix)),
               unname(tcrossprod(r$unrotated_loadings)), tolerance = 1e-10)
  expect_identical(r$rotation_iterations, 6L)
  skip_if_not_installed("GPArotation")
  gpa <- GPArotation::oblimin(r$unrotated_loadings, normalize = TRUE)
  expect_lt(max(abs(unname(r$pattern_matrix) - unname(unclass(gpa$loadings)))), 0.005)
})
