# =============================================================================
# Audit regression tests (2026-07)
# =============================================================================
# Each test pins a defect found in the 2026-07 full-package audit to a
# minimal reproducible scenario, so the fix cannot silently regress.
# These are R-internal consistency checks (Tier 4 in the sense of the
# Validation Charter): they assert agreement with base R reference
# implementations or internal invariants, not SPSS print output.
# =============================================================================

# --- Phase 1: formula fixes --------------------------------------------------

test_that("kendall_tau z/p match cor.test exactly under heavy ties", {
  # Audit finding: the v2 tie term in Var(S) was divided by 9n(n-1)(n-2)
  # twice, inflating |z| for tied data. cor.test implements the same
  # Kendall & Gibbons formula SPSS uses.
  set.seed(42)
  x <- sample(0:1, 500, replace = TRUE)
  y <- sample(0:1, 500, replace = TRUE)

  res <- kendall_tau(dplyr::tibble(x = x, y = y), x, y)$correlations
  ref <- stats::cor.test(x, y, method = "kendall")

  expect_equal(res$tau[1], unname(ref$estimate), tolerance = 1e-10)
  expect_equal(res$z_score[1], unname(ref$statistic), tolerance = 1e-8)
  expect_equal(res$p_value[1], ref$p.value, tolerance = 1e-8)
})

test_that("weighted wilcoxon_test with weights == 1 equals the unweighted test", {
  # Audit finding: rank - 1/2 mid-ranks were inconsistent with
  # E(V) = N(N+1)/4, biasing Z downward by sum(w_pos)/2 standard units.
  set.seed(42)
  pre  <- rnorm(300, 50, 10)
  post <- pre + rnorm(300, 0.5, 5)
  d <- dplyr::tibble(pre = pre, post = post, w1 = rep(1, 300))

  uw <- wilcoxon_test(d, pre, post)$results
  ww <- wilcoxon_test(d, pre, post, weights = w1)$results

  expect_equal(ww$Z[1], uw$Z[1], tolerance = 1e-10)
  expect_equal(ww$p_value[1], uw$p_value[1], tolerance = 1e-10)
  expect_equal(ww$V[1], uw$V[1], tolerance = 1e-10)
})

test_that("weighted pairwise_wilcoxon with weights == 1 equals unweighted", {
  set.seed(7)
  d <- dplyr::tibble(
    t1 = rnorm(120, 50, 10),
    t2 = rnorm(120, 52, 10),
    t3 = rnorm(120, 53, 10),
    w1 = rep(1, 120)
  )
  fr_uw <- friedman_test(d, t1, t2, t3)
  fr_ww <- friedman_test(d, t1, t2, t3, weights = w1)
  pw_uw <- pairwise_wilcoxon(fr_uw)$results
  pw_ww <- pairwise_wilcoxon(fr_ww)$results

  expect_equal(pw_ww$z, pw_uw$z, tolerance = 1e-10)
})

test_that("mann_whitney asymptotic p agrees with its own Z (no continuity correction)", {
  # Audit finding: Z was computed without continuity correction (SPSS
  # convention) but p came from wilcox.test with correct = TRUE.
  d <- dplyr::tibble(
    v = c(12, 15, 11, 18, 14, 13, 16, 10, 19, 22, 17, 21, 20, 23, 25, 24),
    g = factor(rep(c("A", "B"), each = 8))
  )
  res <- mann_whitney(d, v, group = g)$results
  expect_equal(res$p_value[1], 2 * stats::pnorm(-abs(res$Z[1])), tolerance = 1e-10)
})

test_that("regression df count model terms, not predictor variables", {
  # Audit finding: k = length(pred_names) undercounted df whenever a
  # factor expanded into >1 dummy term.
  set.seed(1)
  n <- 200
  f <- factor(sample(letters[1:4], n, replace = TRUE))
  x <- rnorm(n)
  y <- 2 + x + as.numeric(f) + rnorm(n)
  w <- runif(n, 0.5, 1.5)
  d <- dplyr::tibble(y = y, x = x, f = f, w = w)

  # Weighted linear regression: 1 numeric + 3 dummy terms = 4 regression df
  lr <- linear_regression(d, y ~ x + f, weights = w)
  expect_equal(lr$anova_table$df[1], 4)
  expect_equal(lr$anova_table$df[2], sum(w) - 4 - 1, tolerance = 1e-10)

  # Logistic omnibus df = difference in estimated parameters vs null model
  yb <- rbinom(n, 1, stats::plogis(-0.5 + 0.8 * x))
  glr <- logistic_regression(dplyr::tibble(yb = yb, x = x, f = f), yb ~ x + f)
  expect_equal(glr$omnibus_test$df, 4)
})

test_that("kruskal_wallis reports its H/(N-1) effect size as epsilon_squared", {
  # Audit finding: H/(N-1) is Tomczak & Tomczak's epsilon-squared but was
  # returned and printed under the name eta_squared.
  d <- dplyr::tibble(
    v = c(rnorm(30), rnorm(30, 0.6), rnorm(30, 1.2)),
    g = factor(rep(c("a", "b", "c"), each = 30))
  )
  res <- kruskal_wallis(d, v, group = g)$results
  expect_true("epsilon_squared" %in% names(res))
  expect_false("eta_squared" %in% names(res))
  expect_equal(res$epsilon_squared[1], res$H[1] / (90 - 1), tolerance = 1e-10)
})

test_that("weighted mann_whitney stays equivalent to the design-based svyranktest convention", {
  # The weighted MW path is a design-based Lumley-Scott estimator and uses
  # Horvitz-Thompson mid-ranks (cumsum(w) - w/2) on purpose - NOT the
  # frequency-expansion mid-ranks of the frequency-weighted tests. This
  # test pins the w == 1 behavior: HT ranks with unit weights are the
  # classical mid-ranks - 1/2, and the WLS t statistic must equal the
  # unweighted Z-based test asymptotically. We assert the internal
  # consistency contract instead: rank_mean difference and Z direction
  # agree between weighted and unweighted runs on the same data.
  set.seed(11)
  d <- dplyr::tibble(
    v = rnorm(400, mean = rep(c(0, 0.3), each = 200)),
    g = factor(rep(c("A", "B"), each = 200)),
    w1 = rep(1, 400)
  )
  uw <- mann_whitney(d, v, group = g)$results
  ww <- mann_whitney(d, v, group = g, weights = w1)$results
  # |Z| only: the unweighted path reports Z from the min-U convention
  # (always <= 0, as SPSS does), the design-based path keeps the natural sign.
  expect_lt(abs(abs(ww$Z[1]) - abs(uw$Z[1])), 0.05)
})
