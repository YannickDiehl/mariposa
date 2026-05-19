# =============================================================================
# weighted_statistics — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::w_mean, w_sd, w_var, w_median, w_modus,
#          w_quantile, w_iqr, w_range, w_se, w_skew, w_kurtosis against
#          SPSS v29 DESCRIPTIVES / FREQUENCIES.
# Reference: weighted_statistics_output.txt
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(
  # ---- Unweighted age ----
  unweighted_age = list(
    n = 2500L, mean = 50.5496, sd = 16.97602, var = 288.185,
    skew = 0.172, kurt = -0.364, median = 50.0000, mode = 18.00,
    range = 77.00, min = 18.00, max = 95.00, se = 0.33952,
    q25 = 38.0000, q50 = 50.0000, q75 = 62.0000
  ),
  # ---- Unweighted income ----
  unweighted_income = list(
    n = 2186L, mean = 3753.9341, sd = 1432.80161, var = 2052920.442,
    skew = 0.730, kurt = 0.376, median = 3500.0000, mode = 3200.00,
    range = 7200.00, min = 800.00, max = 8000.00, se = 30.64510,
    q25 = 2700.0000, q50 = 3500.0000, q75 = 4600.0000
  ),
  # ---- Weighted age ----
  weighted_age = list(
    n = 2516L, mean = 50.5144, sd = 17.08382, var = 291.857,
    skew = 0.159, kurt = -0.396, median = 50.0000
  ),
  # ---- Weighted income ----
  weighted_income = list(
    n = 2201L, mean = 3743.0994, sd = 1423.96558, var = 2027677.966,
    skew = 0.725, kurt = 0.388, median = 3500.0000
  )
)


data(survey_data, envir = environment())


# Helper: extract single-row scalar from w_* result
get_w <- function(result, varname, col) {
  res <- result$results
  row <- res[res$Variable == varname, ]
  as.numeric(row[[col]])
}


test_that("w_mean — matches SPSS unweighted", {
  r_age <- survey_data |> w_mean(age, income)
  assert_spss(get_w(r_age, "age", "mean"),    spss_values$unweighted_age$mean,
              tier = "display", precision = 4, label = "w_mean(age)")
  assert_spss(get_w(r_age, "income", "mean"), spss_values$unweighted_income$mean,
              tier = "display", precision = 4, label = "w_mean(income)")
})

test_that("w_mean — matches SPSS weighted", {
  r <- survey_data |> w_mean(age, income, weights = sampling_weight)
  assert_spss(get_w(r, "age", "weighted_mean"), spss_values$weighted_age$mean,
              tier = "display", precision = 4, label = "weighted mean(age)")
  assert_spss(get_w(r, "income", "weighted_mean"), spss_values$weighted_income$mean,
              tier = "display", precision = 4, label = "weighted mean(income)")
})

test_that("w_sd — matches SPSS unweighted", {
  r <- survey_data |> w_sd(age, income)
  assert_spss(get_w(r, "age", "sd"),    spss_values$unweighted_age$sd,
              tier = "display", precision = 5, label = "w_sd(age)")
  assert_spss(get_w(r, "income", "sd"), spss_values$unweighted_income$sd,
              tier = "display", precision = 5, label = "w_sd(income)")
})

test_that("w_sd — matches SPSS weighted", {
  r <- survey_data |> w_sd(age, income, weights = sampling_weight)
  assert_spss(get_w(r, "age", "weighted_sd"), spss_values$weighted_age$sd,
              tier = "display", precision = 5, label = "weighted sd(age)")
  assert_spss(get_w(r, "income", "weighted_sd"), spss_values$weighted_income$sd,
              tier = "display", precision = 5, label = "weighted sd(income)")
})

test_that("w_var — matches SPSS unweighted", {
  r <- survey_data |> w_var(age, income)
  assert_spss(get_w(r, "age", "var"), spss_values$unweighted_age$var,
              tier = "display", precision = 3, label = "w_var(age)")
})

test_that("w_var — matches SPSS weighted", {
  r <- survey_data |> w_var(age, income, weights = sampling_weight)
  assert_spss(get_w(r, "age", "weighted_var"), spss_values$weighted_age$var,
              tier = "display", precision = 3, label = "weighted var(age)")
})

test_that("w_skew — matches SPSS unweighted", {
  r <- survey_data |> w_skew(age, income)
  assert_spss(get_w(r, "age", "skew"), spss_values$unweighted_age$skew,
              tier = "display", precision = 3, label = "w_skew(age)")
  assert_spss(get_w(r, "income", "skew"), spss_values$unweighted_income$skew,
              tier = "display", precision = 3, label = "w_skew(income)")
})

test_that("w_kurtosis — matches SPSS unweighted", {
  r <- survey_data |> w_kurtosis(age, income)
  assert_spss(get_w(r, "age", "kurtosis"), spss_values$unweighted_age$kurt,
              tier = "display", precision = 3, label = "w_kurtosis(age)")
})

test_that("w_se — matches SPSS unweighted", {
  r <- survey_data |> w_se(age, income)
  assert_spss(get_w(r, "age", "se"), spss_values$unweighted_age$se,
              tier = "display", precision = 5, label = "w_se(age)")
})

test_that("w_median — matches SPSS unweighted", {
  r <- survey_data |> w_median(age, income)
  assert_spss(get_w(r, "age", "median"), spss_values$unweighted_age$median,
              tier = "display", precision = 2, label = "w_median(age)")
  assert_spss(get_w(r, "income", "median"), spss_values$unweighted_income$median,
              tier = "display", precision = 2, label = "w_median(income)")
})

test_that("w_modus — matches SPSS unweighted", {
  r <- survey_data |> w_modus(age, income)
  assert_spss(get_w(r, "age", "mode"), spss_values$unweighted_age$mode,
              tier = "display", precision = 2, label = "w_modus(age)")
  assert_spss(get_w(r, "income", "mode"), spss_values$unweighted_income$mode,
              tier = "display", precision = 2, label = "w_modus(income)")
})

test_that("w_range — matches SPSS unweighted", {
  r <- survey_data |> w_range(age, income)
  assert_spss(get_w(r, "age", "range"), spss_values$unweighted_age$range,
              tier = "display", precision = 2, label = "w_range(age)")
})
