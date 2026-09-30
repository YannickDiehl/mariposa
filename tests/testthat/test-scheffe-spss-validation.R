# =============================================================================
# scheffe_test — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::scheffe_test() against SPSS v29 ONEWAY POSTHOC
#          SCHEFFE.
# Reference output: tests/spss_reference/outputs/scheffe_test_output.txt
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# SPSS Test 1a: life_sat by education (4 levels) -> 6 pairs
spss_values <- list(
  # One row per pair in SPSS "(I) - (J)" orientation (signed difference)
  test_1a_life = list(
    "Basic Secondary - Intermediate Secondary"     = list(diff = -0.497, se = 0.059, p = "<.001", ci_lower = -0.66, ci_upper = -0.33),  # scheffe_test_output.txt:15
    "Basic Secondary - Academic Secondary"         = list(diff = -0.649, se = 0.060, p = "<.001", ci_lower = -0.82, ci_upper = -0.48),  # scheffe_test_output.txt:16
    "Basic Secondary - University"                 = list(diff = -0.843, se = 0.069, p = "<.001", ci_lower = -1.03, ci_upper = -0.65),  # scheffe_test_output.txt:17
    "Intermediate Secondary - Academic Secondary"  = list(diff = -0.153, se = 0.063, p = 0.121,   ci_lower = -0.33, ci_upper = 0.02),   # scheffe_test_output.txt:19
    "Intermediate Secondary - University"          = list(diff = -0.346, se = 0.072, p = "<.001", ci_lower = -0.55, ci_upper = -0.14),  # scheffe_test_output.txt:20
    "Academic Secondary - University"              = list(diff = -0.193, se = 0.072, p = 0.067,   ci_lower = -0.39, ci_upper = 0.01)    # scheffe_test_output.txt:23
  ),
  # Weighted (WEIGHT BY sampling_weight): error MS and df from the weighted
  # ANOVA table, df = floor(sum(w)) - k (0.7.4 audit)
  test_2b_income_w = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -830.95713,  se = 62.53396, p = "<.001", ci_lower = -1005.9055, ci_upper = -656.0088),   # scheffe_test_output.txt:235
    "Basic Secondary - Academic Secondary"        = list(diff = -1466.06486, se = 62.53873, p = "<.001", ci_lower = -1641.0266, ci_upper = -1291.1032),  # scheffe_test_output.txt:236
    "Basic Secondary - University"                = list(diff = -2572.07638, se = 72.84819, p = "<.001", ci_lower = -2775.8804, ci_upper = -2368.2724),  # scheffe_test_output.txt:237
    "Intermediate Secondary - Academic Secondary" = list(diff = -635.10773,  se = 66.79203, p = "<.001", ci_lower = -821.9687,  ci_upper = -448.2468),   # scheffe_test_output.txt:239
    "Intermediate Secondary - University"         = list(diff = -1741.11924, se = 76.53065, p = "<.001", ci_lower = -1955.2255, ci_upper = -1527.0130),  # scheffe_test_output.txt:240
    "Academic Secondary - University"             = list(diff = -1106.01151, se = 76.53455, p = "<.001", ci_lower = -1320.1287, ci_upper = -891.8944)    # scheffe_test_output.txt:243
  ),
  test_4a_east_w = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -0.327, se = 0.140, p = 0.140, ci_lower = -0.72, ci_upper = 0.06),   # scheffe_test_output.txt:571
    "Basic Secondary - Academic Secondary"        = list(diff = -0.505, se = 0.143, p = 0.006, ci_lower = -0.91, ci_upper = -0.11),  # scheffe_test_output.txt:572
    "Basic Secondary - University"                = list(diff = -0.639, se = 0.163, p = 0.002, ci_lower = -1.09, ci_upper = -0.18),  # scheffe_test_output.txt:573
    "Intermediate Secondary - Academic Secondary" = list(diff = -0.178, se = 0.151, p = 0.710, ci_lower = -0.60, ci_upper = 0.25),   # scheffe_test_output.txt:575
    "Intermediate Secondary - University"         = list(diff = -0.311, se = 0.170, p = 0.344, ci_lower = -0.79, ci_upper = 0.17),   # scheffe_test_output.txt:576
    "Academic Secondary - University"             = list(diff = -0.133, se = 0.173, p = 0.897, ci_lower = -0.62, ci_upper = 0.35)    # scheffe_test_output.txt:579
  )
)

compare_scheffe_pairs <- function(res, pairs, prec_diff, prec_ci, scenario) {
  for (pname in names(pairs)) {
    expected <- pairs[[pname]]
    row <- res[res$Comparison == pname, , drop = FALSE]
    if (nrow(row) != 1L) {
      stop(sprintf("[%s] comparison '%s' not found", scenario, pname), call. = FALSE)
    }
    lab <- function(what) sprintf("[%s] %s %s", scenario, pname, what)
    assert_spss(as.numeric(row$Estimate), expected$diff, tier = "display",
                precision = prec_diff, label = lab("Mean Difference (I-J)"))
    assert_spss(as.numeric(row$SE), expected$se, tier = "display",
                precision = prec_diff, label = lab("Std. Error"))
    assert_spss(as.numeric(row$p_adjusted), expected$p, tier = "display",
                precision = 3, what = "p_value", label = lab("Sig."))
    assert_spss(as.numeric(row$conf_low), expected$ci_lower, tier = "display",
                precision = prec_ci, label = lab("CI lower"))
    assert_spss(as.numeric(row$conf_high), expected$ci_upper, tier = "display",
                precision = prec_ci, label = lab("CI upper"))
  }
}


data(survey_data, envir = environment())


test_that("Test 1a: Scheffé life_satisfaction by education — matches SPSS", {
  av <- survey_data |> oneway_anova(life_satisfaction, group = education)
  compare_scheffe_pairs(scheffe_test(av)$results, spss_values$test_1a_life,
                        prec_diff = 3, prec_ci = 2, scenario = "1a")
})

test_that("Test 2b: weighted Scheffé income by education — matches SPSS", {
  av <- survey_data |>
    oneway_anova(income, group = education, weights = sampling_weight)
  compare_scheffe_pairs(scheffe_test(av)$results, spss_values$test_2b_income_w,
                        prec_diff = 5, prec_ci = 4, scenario = "2b weighted")
})

test_that("Test 4a: weighted grouped Scheffé (East) — matches SPSS", {
  av <- survey_data |> group_by(region) |>
    oneway_anova(life_satisfaction, group = education, weights = sampling_weight)
  res <- scheffe_test(av)$results
  compare_scheffe_pairs(res[res$region == "East", , drop = FALSE],
                        spss_values$test_4a_east_w,
                        prec_diff = 3, prec_ci = 2, scenario = "4a East weighted")
})

test_that("Test 6a: ANOVA conf.level = .90 keeps the 95% Scheffé CI — matches SPSS", {
  # SPSS: /POSTHOC ALPHA(.05) with /CRITERIA=CILEVEL(.90) prints a 95%
  # post-hoc interval
  expected <- list(ci_lower = -0.66, ci_upper = -0.33)  # scheffe_test_output.txt:791
  av <- survey_data |>
    oneway_anova(life_satisfaction, group = education, conf.level = 0.90)
  res <- scheffe_test(av)$results
  row <- res[res$Comparison == "Basic Secondary - Intermediate Secondary", ]
  assert_spss(as.numeric(row$conf_low), expected$ci_lower, tier = "display",
              precision = 2, label = "[6a] CI lower")
  assert_spss(as.numeric(row$conf_high), expected$ci_upper, tier = "display",
              precision = 2, label = "[6a] CI upper")
})
