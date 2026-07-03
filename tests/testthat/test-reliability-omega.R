# =============================================================================
# MCDONALD'S OMEGA — internal validation (Tier 4 / R-only)
# =============================================================================
# reliability()'s McDonald's omega is a one-factor maximum-likelihood
# solution (stats::factanal on the (weighted) correlation matrix). SPSS v27+
# offers omega in RELIABILITY, but IBM's algorithm documentation is not
# publicly retrievable and no SPSS v29 reference run exists yet (pending:
# .claude/spss-syntax-omega-references.sps). Per Charter §4 the statistic is
# therefore Tier 4 (Internal) and validated here by:
#   1. a parameter-recovery test on simulated congeneric data,
#   2. an exact cross-check against a manual factanal-based computation,
#   3. cross-checks against psych::omega() and a lavaan one-factor CFA
#      (semTools::compRelSEM), gated by skip_if_not_installed(),
#   4. guard tests (k < 3, omega-if-deleted identification, grouping).
# The w == 1 reduction lives in test-weights-invariance.R.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)

# --- Simulated congeneric data (known loadings) ------------------------------
# x_i = lambda_i * eta + sqrt(theta_i) * e_i with Var(eta) = Var(e_i) = 1,
# so the population correlation-metric omega is
# (sum lambda)^2 / ((sum lambda)^2 + sum theta).

make_congeneric <- function(n = 5000, lambda = c(0.8, 0.7, 0.6, 0.5),
                            seed = 20260703) {
  set.seed(seed)
  theta <- 1 - lambda^2
  eta <- rnorm(n)
  items <- vapply(seq_along(lambda), function(i) {
    lambda[i] * eta + sqrt(theta[i]) * rnorm(n)
  }, numeric(n))
  colnames(items) <- paste0("x", seq_along(lambda))
  as_tibble(as.data.frame(items))
}

true_omega <- function(lambda) {
  theta <- 1 - lambda^2
  sum(lambda)^2 / (sum(lambda)^2 + sum(theta))
}

cong <- make_congeneric()
cong_lambda <- c(0.8, 0.7, 0.6, 0.5)


test_that("omega recovers the population value on congeneric data", {
  res <- reliability(cong, x1, x2, x3, x4)
  target <- true_omega(cong_lambda)   # 0.7494 for lambda = .8/.7/.6/.5

  expect_true(is.finite(res$omega))
  expect_true(is.finite(res$omega_std))
  # Sampling error bound at n = 5000: ~0.02 around the population omega
  expect_lt(abs(res$omega_std - target), 0.02)
  expect_lt(abs(res$omega - target), 0.02)
  # omega and alpha live on the same [0, 1] scale here and should be close
  # (congeneric but near-tau-equivalent loadings)
  expect_gt(res$omega, res$alpha - 0.05)
})


test_that("omega matches a manual factanal-based computation exactly", {
  res <- reliability(cong, x1, x2, x3, x4)

  mat <- as.matrix(cong[, c("x1", "x2", "x3", "x4")])
  R <- stats::cor(mat)
  S <- stats::cov(mat)
  fit <- stats::factanal(covmat = R, factors = 1, n.obs = nrow(mat))
  l  <- as.numeric(fit$loadings)
  th <- as.numeric(fit$uniquenesses)

  manual_std <- sum(l)^2 / (sum(l)^2 + sum(th))
  lr <- l * sqrt(diag(S))
  tr <- th * diag(S)
  manual_raw <- sum(lr)^2 / (sum(lr)^2 + sum(tr))

  expect_equal(res$omega_std, manual_std, tolerance = 1e-10)
  expect_equal(res$omega,     manual_raw, tolerance = 1e-10)
})


test_that("weighted omega matches the manual computation on the weighted matrices", {
  set.seed(99)
  wts <- runif(nrow(cong), 0.5, 1.5)
  dat <- mutate(cong, w = wts)
  res <- reliability(dat, x1, x2, x3, x4, weights = w)

  mat <- as.matrix(cong[, c("x1", "x2", "x3", "x4")])
  Rw <- mariposa:::.weighted_cor(mat, wts)
  Sw <- mariposa:::.weighted_cov(mat, wts)
  fit <- stats::factanal(covmat = Rw, factors = 1, n.obs = sum(wts))
  l  <- as.numeric(fit$loadings)
  th <- as.numeric(fit$uniquenesses)

  manual_std <- sum(l)^2 / (sum(l)^2 + sum(th))
  lr <- l * sqrt(diag(Sw))
  tr <- th * diag(Sw)
  manual_raw <- sum(lr)^2 / (sum(lr)^2 + sum(tr))

  expect_equal(res$omega_std, manual_std, tolerance = 1e-10)
  expect_equal(res$omega,     manual_raw, tolerance = 1e-10)
})


test_that("omega cross-checks against psych::omega (one factor, ML, no flip)", {
  skip_if_not_installed("psych")

  res <- reliability(cong, x1, x2, x3, x4)
  po <- suppressWarnings(suppressMessages(
    psych::omega(as.data.frame(cong[, c("x1", "x2", "x3", "x4")]),
                 nfactors = 1, fm = "ml", flip = FALSE, plot = FALSE)
  ))
  # psych uses its own ML routine (psych::fa) rather than factanal; small
  # optimizer differences are expected, so bound rather than exact-match.
  expect_lt(abs(res$omega_std - po$omega.tot), 0.005)
})


test_that("omega cross-checks against a lavaan one-factor CFA (semTools)", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("semTools", minimum_version = "0.5-6")

  res <- reliability(cong, x1, x2, x3, x4)
  fit <- suppressWarnings(lavaan::cfa(
    "F =~ x1 + x2 + x3 + x4",
    data = as.data.frame(cong), std.lv = TRUE
  ))
  omega_sem <- as.numeric(semTools::compRelSEM(fit))
  # lavaan's default ML uses the N (not N-1) covariance divisor, so a small
  # finite-sample difference from the factanal solution is expected.
  expect_lt(abs(res$omega - omega_sem), 0.005)
})


test_that("k = 2 items: omega is NA with a warning, alpha still computed", {
  expect_warning(
    res <- reliability(cong, x1, x2),
    "at least 3 items"
  )
  expect_true(is.na(res$omega))
  expect_true(is.na(res$omega_std))
  expect_true(all(is.na(res$item_total$omega_if_deleted)))
  expect_true(is.finite(res$alpha))
  expect_true(is.finite(res$alpha_standardized))
})


test_that("omega_if_deleted: one finite value per item for k >= 4", {
  res <- reliability(cong, x1, x2, x3, x4)
  oid <- res$item_total$omega_if_deleted
  expect_length(oid, 4L)
  expect_true(all(is.finite(oid)))
  # Deleting the strongest item (x1, lambda = .8) must hurt omega the most
  expect_equal(which.min(oid), 1L)
})


test_that("omega_if_deleted: NA for k = 3 (reduced model unidentified)", {
  res <- reliability(cong, x1, x2, x3)
  expect_true(is.finite(res$omega))
  expect_length(res$item_total$omega_if_deleted, 3L)
  expect_true(all(is.na(res$item_total$omega_if_deleted)))
})


test_that("grouped data returns a finite omega per group", {
  dat <- mutate(cong, g = factor(rep(c("A", "B"), length.out = nrow(cong))))
  res <- dat %>% group_by(g) %>% reliability(x1, x2, x3, x4)

  expect_length(res$groups, 2L)
  for (grp in res$groups) {
    expect_true(is.finite(grp$omega))
    expect_true(is.finite(grp$omega_std))
    expect_length(grp$item_total$omega_if_deleted, 4L)
  }
})


test_that("existing alpha fields are unchanged by the omega extension", {
  # The omega computation must not perturb the alpha path: recompute alpha
  # by the classical formula from the covariance matrix.
  res <- reliability(cong, x1, x2, x3, x4)
  S <- stats::cov(as.matrix(cong[, c("x1", "x2", "x3", "x4")]))
  k <- 4
  alpha_manual <- (k / (k - 1)) * (1 - sum(diag(S)) / sum(S))
  expect_equal(res$alpha, alpha_manual, tolerance = 1e-10)
})

test_that("3-item scales explain the NA omega_deleted column in the output", {
  data(survey_data)
  rel <- reliability(survey_data, trust_government, trust_media, trust_science)
  out <- capture.output(print(summary(rel)))
  expect_true(all(is.na(rel$item_total$omega_if_deleted)))
  expect_true(any(grepl("requires at least 4 items", out)))
})
