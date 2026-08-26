# =============================================================================
# normality_test — PROPERTY-BASED VALIDATION (Charter Tier-4)
# =============================================================================
# Purpose: Validate that normality_test() correctly implements the two
# statistics of the SPSS EXAMINE "Tests of Normality" table, without an
# SPSS reference run (pending, see .claude/BACKLOG.md).
#
# Oracles:
#   - KS statistic: stats::ks.test() against N(mean, sd) — an independent
#     implementation of the same distance definition.
#   - Lilliefors p: nortest::lillie.test() — an independent implementation
#     of the same Dallal-Wilkinson (1986) approximation SPSS uses
#     (skipped when nortest is not installed).
#   - Shapiro-Wilk: wrapper-correctness against stats::shapiro.test()
#     (the implementation delegates to it; the assertions pin the wiring:
#     W vs p not swapped, NA rules honored).
#
# Scenario coverage (Charter §8): unweighted ungrouped + unweighted
# grouped. The weighted scenarios do not apply: normality_test() takes no
# weights argument by design — neither Shapiro-Wilk nor the Lilliefors
# correction has a well-defined fractional-frequency-weight form (see
# @details in R/normality_test.R).
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


data(survey_data, envir = environment())


test_that("KS statistic equals ks.test() distance against fitted normal", {
  r <- normality_test(survey_data, age, income)
  for (v in c("age", "income")) {
    x <- survey_data[[v]]
    x <- x[!is.na(x)]
    ref <- suppressWarnings(
      stats::ks.test(x, "pnorm", mean = mean(x), sd = stats::sd(x))
    )
    row <- r$results[r$results$Variable == v, ]
    assert_spss(row$ks_statistic, unname(ref$statistic),
                tier = "display", precision = 5,
                label = sprintf("KS statistic for %s vs ks.test()", v))
    assert_spss_count(row$ks_df, length(x),
                      label = sprintf("KS df = n for %s", v))
  }
})


test_that("Lilliefors p matches the independent nortest implementation", {
  skip_if_not_installed("nortest")
  r <- normality_test(survey_data, age, income, life_satisfaction)
  for (v in c("age", "income", "life_satisfaction")) {
    x <- survey_data[[v]]
    x <- x[!is.na(x)]
    ref <- nortest::lillie.test(x)
    row <- r$results[r$results$Variable == v, ]
    assert_spss(row$ks_statistic, unname(ref$statistic),
                tier = "display", precision = 5,
                label = sprintf("Lilliefors D for %s vs nortest", v))
    assert_spss(row$ks_p, ref$p.value,
                tier = "display", precision = 5,
                label = sprintf("Lilliefors p for %s vs nortest", v))
  }
})


test_that("Lilliefors p agrees with nortest in the p > 0.1 branch too", {
  skip_if_not_installed("nortest")
  # A genuinely normal small sample exercises the Stephens polynomial
  # branch of the approximation (p > 0.1).
  set.seed(42)
  x <- rnorm(80)
  d <- tibble(x = x)
  r <- normality_test(d, x)
  ref <- nortest::lillie.test(x)
  assert_spss(r$results$ks_p[1], ref$p.value,
              tier = "display", precision = 5,
              label = "Lilliefors p (upper branch) vs nortest")
})


test_that("Shapiro-Wilk wiring: W and p match stats::shapiro.test()", {
  r <- normality_test(survey_data, age)
  x <- survey_data$age[!is.na(survey_data$age)]
  ref <- stats::shapiro.test(x)
  assert_spss(r$results$shapiro_w[1], unname(ref$statistic),
              tier = "display", precision = 5,
              label = "Shapiro-Wilk W wiring")
  assert_spss(r$results$shapiro_p[1], ref$p.value,
              tier = "display", precision = 5,
              label = "Shapiro-Wilk p wiring")
})


test_that("Shapiro-Wilk is NA above n = 5000 (SPSS convention), KS still computed", {
  set.seed(42)
  d <- tibble(x = rnorm(6000))
  r <- normality_test(d, x)
  expect_true(is.na(r$results$shapiro_w[1]))
  expect_true(is.na(r$results$shapiro_p[1]))
  expect_false(is.na(r$results$ks_statistic[1]))
  assert_spss_count(r$results$n[1], 6000L, label = "n above SW limit")
})


test_that("Constant and too-short variables yield NA instead of erroring", {
  d <- tibble(const = rep(1, 50), short = c(1, 2, rep(NA, 48)))
  r <- normality_test(d, const, short)
  expect_true(all(is.na(r$results$ks_statistic)))
  expect_true(all(is.na(r$results$shapiro_w)))
  assert_spss_count(r$results$n[r$results$Variable == "short"], 2L,
                    label = "n for too-short variable")
})


test_that("Grouped results equal per-subset ungrouped results (SPSS EXAMINE BY)", {
  rg <- survey_data |> group_by(gender) |> normality_test(age)
  for (g in unique(survey_data$gender)) {
    sub <- survey_data[survey_data$gender == g, ]
    ru <- normality_test(sub, age)
    row <- rg$results[rg$results$gender == g, ]
    assert_spss(row$ks_statistic, ru$results$ks_statistic[1],
                tier = "display", precision = 6,
                label = sprintf("grouped KS == subset KS (%s)", g))
    assert_spss(row$shapiro_w, ru$results$shapiro_w[1],
                tier = "display", precision = 6,
                label = sprintf("grouped W == subset W (%s)", g))
    assert_spss_count(row$n, ru$results$n[1],
                      label = sprintf("grouped n == subset n (%s)", g))
  }
})
