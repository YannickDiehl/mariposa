# =============================================================================
# WEIGHTS == 1 INVARIANCE SUITE (structural safeguard)
# =============================================================================
# THE INVARIANT: for every weighted entry point in mariposa, running the
# analysis with weights that are all exactly 1 must reproduce the unweighted
# result. A weight of 1 per observation carries no information; any weighted
# formula that does not collapse to its unweighted counterpart at w == 1 has
# a formula bug or an undocumented convention drift.
#
# WHY THIS FILE EXISTS: two formula bugs escaped into releases because this
# reduction was only ever tested for wilcoxon_test:
#   1. kendall_tau (weighted): the tau-b denominator omitted double-tied
#      pairs (ties_both) from both factors, deflating |tau| on tied data
#      (0.7501 observed vs 0.8080 correct on 4-level ordinals).
#   2. kruskal_wallis (weighted): the grand mean rank was hard-coded as
#      N/2 (pre-0.6.4 rank convention) instead of (N+1)/2, inflating H
#      (0.93130 observed vs 0.89426 correct).
# Both would have been caught immediately by the assertions below.
#
# CONTRIBUTOR RULE: every NEW weighted statistic added to the package MUST
# get a test_that block in this file asserting its w == 1 reduction on the
# headline statistics. If the reduction is intentionally approximate,
# document the reason in a comment and use a bounded assertion (see the
# mann_whitney block at the bottom for the pattern).
#
# STRICTNESS:
#   - Default: exact reduction, tolerance 1e-10 (relative, via expect_equal).
#   - Documented exceptions are listed per test with the mathematical reason.
#
# The datasets are small, seeded, and generated inline so failures are
# reproducible and independent of the shipped example data.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)

TOL <- 1e-10

# --- Shared test data --------------------------------------------------------
set.seed(2026)
n <- 160
inv_data <- tibble(
  y    = rnorm(n, 50, 10),                 # continuous, tie-free
  x2   = rnorm(n, 5, 2),                   # second continuous
  x3   = rnorm(n, 10, 3),                  # third continuous
  ord1 = sample(1:4, n, replace = TRUE),   # ordinal, heavily tied
  ord2 = pmin(pmax(sample(1:4, n, replace = TRUE) +
                     sample(c(-1, 0, 0, 1), n, replace = TRUE), 1), 4),
  g2   = factor(sample(c("A", "B"), n, replace = TRUE)),
  g3   = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
  bin  = rbinom(n, 1, 0.4),
  w1   = rep(1, n)                          # the all-ones weight vector
)
# Correlated items for reliability()
inv_data$i1 <- inv_data$y + rnorm(n, sd = 4)
inv_data$i2 <- inv_data$y + rnorm(n, sd = 4)
inv_data$i3 <- inv_data$y + rnorm(n, sd = 4)


# =============================================================================
# Hypothesis tests
# =============================================================================

test_that("t_test: weights == 1 reproduces unweighted t/df/p", {
  rw <- t_test(inv_data, y, group = g2, weights = w1)$results
  ru <- t_test(inv_data, y, group = g2)$results
  expect_equal(rw$t_stat,  ru$t_stat,  tolerance = TOL)
  expect_equal(rw$df,      ru$df,      tolerance = TOL)
  expect_equal(rw$p_value, ru$p_value, tolerance = TOL)
})

test_that("oneway_anova: weights == 1 reproduces unweighted F/df/p/eta^2", {
  rw <- oneway_anova(inv_data, y, group = g3, weights = w1)$results
  ru <- oneway_anova(inv_data, y, group = g3)$results
  expect_equal(rw$F_statistic, ru$F_statistic, tolerance = TOL)
  expect_equal(rw$df1,         ru$df1,         tolerance = TOL)
  expect_equal(rw$df2,         ru$df2,         tolerance = TOL)
  expect_equal(rw$p_value,     ru$p_value,     tolerance = TOL)
  expect_equal(rw$eta_squared, ru$eta_squared, tolerance = TOL)
})

test_that("factorial_anova: weights == 1 reproduces unweighted F/p", {
  tw <- factorial_anova(inv_data, dv = y, between = c(g2, g3),
                        weights = w1)$anova_table
  tu <- factorial_anova(inv_data, dv = y, between = c(g2, g3))$anova_table
  expect_identical(tw$source, tu$source)
  expect_equal(tw$f, tu$f, tolerance = TOL)
  expect_equal(tw$p, tu$p, tolerance = TOL)
})

test_that("ancova: weights == 1 reproduces unweighted F/p", {
  tw <- ancova(inv_data, dv = y, between = c(g3), covariate = c(x2),
               weights = w1)$anova_table
  tu <- ancova(inv_data, dv = y, between = c(g3),
               covariate = c(x2))$anova_table
  expect_identical(tw$source, tu$source)
  expect_equal(tw$f, tu$f, tolerance = TOL)
  expect_equal(tw$p, tu$p, tolerance = TOL)
})

test_that("chi_square: weights == 1 reproduces unweighted chi^2/p/Cramer's V", {
  rw <- chi_square(inv_data, g2, g3, weights = w1)$results
  ru <- chi_square(inv_data, g2, g3)$results
  expect_equal(rw$chi_squared, ru$chi_squared, tolerance = TOL)
  expect_equal(rw$p_value,     ru$p_value,     tolerance = TOL)
  expect_equal(rw$cramers_v,   ru$cramers_v,   tolerance = TOL)
})

test_that("binomial_test: weights == 1 reproduces unweighted proportions/p", {
  rw <- binomial_test(inv_data, bin, weights = w1)$results
  ru <- binomial_test(inv_data, bin)$results
  expect_equal(rw$obs_prop1, ru$obs_prop1, tolerance = TOL)
  expect_equal(rw$p_value,   ru$p_value,   tolerance = TOL)
})


# =============================================================================
# Non-parametric tests + post-hoc
# =============================================================================

test_that("kruskal_wallis: weights == 1 reproduces kruskal.test H/p", {
  # This is the exact reduction that Bug 2 (grand mean rank = N/2 instead
  # of (N+1)/2) violated. Tied ordinal data exercises the tie correction.
  rw <- kruskal_wallis(inv_data, ord1, group = g3, weights = w1)$results
  ref <- kruskal.test(ord1 ~ g3, data = inv_data)
  expect_equal(rw$H,       unname(ref$statistic), tolerance = TOL)
  expect_equal(rw$p_value, ref$p.value,           tolerance = TOL)
  # And against mariposa's own unweighted path
  ru <- kruskal_wallis(inv_data, ord1, group = g3)$results
  expect_equal(rw$H, ru$H, tolerance = TOL)
})

test_that("dunn_test: weights == 1 reproduces unweighted z", {
  # Tie-free continuous data: the weighted dunn branch documents an omitted
  # tie correction, so ties would be a known (documented) divergence.
  cw <- dunn_test(kruskal_wallis(inv_data, y, group = g3,
                                 weights = w1))$comparisons
  cu <- dunn_test(kruskal_wallis(inv_data, y, group = g3))$comparisons
  expect_identical(paste(cw$group1, cw$group2), paste(cu$group1, cu$group2))
  expect_equal(cw$z, cu$z, tolerance = TOL)
})

test_that("wilcoxon_test: weights == 1 reproduces unweighted Z/p", {
  rw <- wilcoxon_test(inv_data, y, x2, weights = w1)$results
  ru <- wilcoxon_test(inv_data, y, x2)$results
  expect_equal(rw$Z,       ru$Z,       tolerance = TOL)
  expect_equal(rw$p_value, ru$p_value, tolerance = TOL)
})

test_that("friedman_test: weights == 1 reproduces unweighted chi^2/p", {
  rw <- friedman_test(inv_data, y, x2, x3, weights = w1)$results
  ru <- friedman_test(inv_data, y, x2, x3)$results
  expect_equal(rw$chi_squared, ru$chi_squared, tolerance = TOL)
  expect_equal(rw$p_value,     ru$p_value,     tolerance = TOL)
})

test_that("pairwise_wilcoxon: weights == 1 reproduces unweighted z", {
  cw <- pairwise_wilcoxon(friedman_test(inv_data, y, x2, x3,
                                        weights = w1))$comparisons
  cu <- pairwise_wilcoxon(friedman_test(inv_data, y, x2, x3))$comparisons
  expect_identical(paste(cw$var1, cw$var2), paste(cu$var1, cu$var2))
  expect_equal(cw$z, cu$z, tolerance = TOL)
})

test_that("mann_whitney: weights == 1 approximates unweighted Z (documented exception)", {
  # DOCUMENTED EXCEPTION: mariposa's weighted mann_whitney is a design-based
  # Lumley-Scott estimator. Its sandwich-type variance estimate does not
  # algebraically collapse to the classical normal-approximation Z when all
  # weights are 1 (the design-based SE differs from sqrt(n1*n2*(N+1)/12) by
  # a finite-sample factor), so an exact 1e-10 reduction is NOT expected.
  # We bound the divergence instead: |Z_w - Z_u| < 0.05 and rank means
  # within 0.5 (the weighted mid-ranks use the frequency-expansion
  # convention, which can shift rank means by up to half a rank).
  rw <- mann_whitney(inv_data, y, group = g2, weights = w1)$results
  ru <- mann_whitney(inv_data, y, group = g2)$results
  expect_lt(abs(abs(rw$Z[1]) - abs(ru$Z[1])), 0.05)
  gw <- rw$group_stats[[1]]
  gu <- ru$group_stats[[1]]
  for (k in seq_along(gw)) {
    expect_lte(abs(gw[[k]]$rank_mean - gu[[k]]$rank_mean), 0.5 + 1e-9)
  }
})


# =============================================================================
# Correlation
# =============================================================================

test_that("pearson_cor: weights == 1 reproduces unweighted r/p", {
  rw <- pearson_cor(inv_data, y, x2, weights = w1)$correlations
  ru <- pearson_cor(inv_data, y, x2)$correlations
  expect_equal(rw$correlation, ru$correlation, tolerance = TOL)
  expect_equal(rw$p_value,     ru$p_value,     tolerance = TOL)
})

test_that("spearman_rho: weights == 1 reproduces unweighted rho/p", {
  rw <- spearman_rho(inv_data, ord1, ord2, weights = w1)$correlations
  ru <- spearman_rho(inv_data, ord1, ord2)$correlations
  expect_equal(rw$rho,     ru$rho,     tolerance = TOL)
  expect_equal(rw$p_value, ru$p_value, tolerance = TOL)
})

test_that("kendall_tau: weights == 1 reproduces unweighted tau (tau ONLY)", {
  # This is the exact reduction that Bug 1 (double-tied pairs missing from
  # the tau-b denominator) violated - ord1/ord2 are heavily double-tied
  # 4-level ordinals precisely to exercise the ties_both term.
  # NOTE: only tau is asserted exactly. The weighted z/p use a documented
  # no-ties approximation (see R/kendall_tau.R) and intentionally do NOT
  # reduce to the tie-corrected unweighted z/p at w == 1.
  rw <- kendall_tau(inv_data, ord1, ord2, weights = w1)$correlations
  ru <- kendall_tau(inv_data, ord1, ord2)$correlations
  expect_equal(rw$tau, ru$tau, tolerance = TOL)
})


# =============================================================================
# Regression + scales
# =============================================================================

test_that("linear_regression: weights == 1 reproduces unweighted coef/R^2/F", {
  mw <- linear_regression(inv_data, y ~ x2 + g2, weights = w1)
  mu <- linear_regression(inv_data, y ~ x2 + g2)
  expect_equal(mw$coef_table$B, mu$coef_table$B, tolerance = TOL)
  expect_equal(mw$model_summary$R_squared, mu$model_summary$R_squared,
               tolerance = TOL)
  expect_equal(unname(mw$anova_table$F_statistic[1]),
               unname(mu$anova_table$F_statistic[1]),
               tolerance = TOL)
})

test_that("logistic_regression: weights == 1 reproduces unweighted coef/omnibus", {
  mw <- logistic_regression(inv_data, bin ~ x2 + y, weights = w1)
  mu <- logistic_regression(inv_data, bin ~ x2 + y)
  expect_equal(mw$coef_table$B, mu$coef_table$B, tolerance = TOL)
  expect_equal(mw$omnibus_test$chi_sq, mu$omnibus_test$chi_sq,
               tolerance = TOL)
})

test_that("reliability: weights == 1 reproduces unweighted alpha", {
  rw <- reliability(inv_data, i1, i2, i3, weights = w1)
  ru <- reliability(inv_data, i1, i2, i3)
  expect_equal(rw$alpha, ru$alpha, tolerance = TOL)
})


# =============================================================================
# Descriptives + tables
# =============================================================================

test_that("describe: weights == 1 reproduces unweighted mean/sd/median", {
  rw <- describe(inv_data, y, weights = w1)$results
  ru <- describe(inv_data, y)$results
  expect_equal(rw$y_Mean,   ru$y_Mean,   tolerance = TOL)
  expect_equal(rw$y_SD,     ru$y_SD,     tolerance = TOL)
  expect_equal(rw$y_Median, ru$y_Median, tolerance = TOL)
})

test_that("frequency: weights == 1 reproduces unweighted counts/percentages", {
  rw <- frequency(inv_data, g3, weights = w1)$results
  ru <- frequency(inv_data, g3)$results
  expect_identical(as.character(rw$value), as.character(ru$value))
  expect_equal(rw$freq,      ru$freq,      tolerance = TOL)
  expect_equal(rw$prc,       ru$prc,       tolerance = TOL)
  expect_equal(rw$valid_prc, ru$valid_prc, tolerance = TOL)
})

test_that("crosstab: weights == 1 reproduces unweighted counts", {
  rw <- crosstab(inv_data, g2, g3, weights = w1)
  ru <- crosstab(inv_data, g2, g3)
  expect_equal(unclass(rw$table), unclass(ru$table), tolerance = TOL,
               ignore_attr = TRUE)
})


# =============================================================================
# Transformation
# =============================================================================

test_that("std: weights == 1 reproduces unweighted standardized values", {
  sw <- std(inv_data, y, weights = w1)
  su <- std(inv_data, y)
  expect_equal(sw$y, su$y, tolerance = TOL)
})

test_that("center: weights == 1 reproduces unweighted centered values", {
  cw <- center(inv_data, y, weights = w1)
  cu <- center(inv_data, y)
  expect_equal(cw$y, cu$y, tolerance = TOL)
})


# =============================================================================
# w_* weighted statistics (11 functions)
# =============================================================================
# Base-R counterparts are used where the conventions match. w_quantile and
# w_iqr use quantile Type 6 (SPSS HAVERAGE), not base R's Type 7, so they
# are compared against their own unweighted branch; likewise w_skew /
# w_kurtosis / w_se / w_modus use SPSS formulas without a base-R twin.

test_that("w_* functions: weights == 1 reproduces base R / unweighted values", {
  x <- inv_data$y
  w <- inv_data$w1

  # Base-R counterparts (identical conventions)
  expect_equal(w_mean(x, weights = w),   mean(x),         tolerance = TOL)
  expect_equal(w_median(x, weights = w), median(x),       tolerance = TOL)
  expect_equal(w_sd(x, weights = w),     sd(x),           tolerance = TOL)
  expect_equal(w_var(x, weights = w),    var(x),          tolerance = TOL)
  expect_equal(w_range(x, weights = w),  diff(range(x)),  tolerance = TOL)

  # Own unweighted branch (SPSS conventions: quantile Type 6 etc.)
  expect_equal(w_quantile(x, weights = w), w_quantile(x), tolerance = TOL)
  expect_equal(w_iqr(x, weights = w),      w_iqr(x),      tolerance = TOL)
  expect_equal(w_skew(x, weights = w),     w_skew(x),     tolerance = TOL)
  expect_equal(w_kurtosis(x, weights = w), w_kurtosis(x), tolerance = TOL)
  expect_equal(w_se(x, weights = w),       w_se(x),       tolerance = TOL)

  # Mode on tied ordinal data
  expect_equal(w_modus(inv_data$ord1, weights = w), w_modus(inv_data$ord1),
               tolerance = TOL)
})
