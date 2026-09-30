# =============================================================================
# tukey_test — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::tukey_test() against SPSS v29 ONEWAY POSTHOC
#          TUKEY pairwise comparisons.
# Reference output: tests/spss_reference/outputs/tukey_test_output.txt
#
# mariposa returns one row per pair in the SPSS "(I) - (J)" orientation
# (I before J in the category order, Estimate = mean(I) - mean(J)); SPSS
# lists both directions, these are its first-listed rows (0.7.4, PAR-11).
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# SPSS Test 1a: life_satisfaction by education (4 levels) -> 6 pairs
spss_values <- list(
  test_1a_life = list(
    pairs = list(
      "Basic Secondary - Intermediate Secondary"    = list(diff = -0.497, ci_lower = -0.65, ci_upper = -0.34, p = "<.001"),  # line 15
      "Basic Secondary - Academic Secondary"        = list(diff = -0.649, ci_lower = -0.80, ci_upper = -0.50, p = "<.001"),  # line 16
      "Basic Secondary - University"                = list(diff = -0.843, ci_lower = -1.02, ci_upper = -0.67, p = "<.001"),  # line 17
      "Intermediate Secondary - Academic Secondary" = list(diff = -0.153, ci_lower = -0.32, ci_upper = 0.01,  p = 0.075),    # line 19
      "Intermediate Secondary - University"         = list(diff = -0.346, ci_lower = -0.53, ci_upper = -0.16, p = "<.001"),  # line 20
      "Academic Secondary - University"             = list(diff = -0.193, ci_lower = -0.38, ci_upper = -0.01, p = 0.037)     # line 23
    )
  )
)


data(survey_data, envir = environment())


test_that("Test 1a: Tukey life_satisfaction by education — matches SPSS", {
  av <- survey_data |> oneway_anova(life_satisfaction, group = education)
  r  <- tukey_test(av)
  pairs <- spss_values$test_1a_life$pairs

  for (pname in names(pairs)) {
    expected <- pairs[[pname]]
    row <- r$results[r$results$Comparison == pname, , drop = FALSE]
    if (nrow(row) != 1L) {
      stop(sprintf("Comparison %s not found in mariposa output", pname),
           call. = FALSE)
    }
    assert_spss(as.numeric(row$Estimate), expected$diff,
                tier = "display", precision = 3,
                label = sprintf("[%s] Mean Difference (I-J)", pname))
    assert_spss(as.numeric(row$conf_low),  expected$ci_lower,
                tier = "display", precision = 2,
                label = sprintf("[%s] CI lower", pname))
    assert_spss(as.numeric(row$conf_high), expected$ci_upper,
                tier = "display", precision = 2,
                label = sprintf("[%s] CI upper", pname))
    assert_spss(as.numeric(row$p_adjusted), expected$p,
                tier = "display", precision = 3, what = "p_value",
                label = sprintf("[%s] p_adjusted", pname))
  }
})


# =============================================================================
# WEIGHTED (ONEWAY with WEIGHT BY sampling_weight) — 0.7.4 audit
# =============================================================================
# SPSS takes the error mean square and its df from the weighted ANOVA table
# (df = floor(sum(w)) - k); group n = unrounded sum of weights.

spss_values$test_2b_income_w <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -830.95713,  se = 62.53396, p = "<.001", ci_lower = -991.7319,  ci_upper = -670.1823),   # tukey_test_output.txt:236
  "Basic Secondary - Academic Secondary"        = list(diff = -1466.06486, se = 62.53873, p = "<.001", ci_lower = -1626.8519, ci_upper = -1305.2778),  # tukey_test_output.txt:237
  "Basic Secondary - University"                = list(diff = -2572.07638, se = 72.84819, p = "<.001", ci_lower = -2759.3690, ci_upper = -2384.7837),  # tukey_test_output.txt:238
  "Intermediate Secondary - Academic Secondary" = list(diff = -635.10773,  se = 66.79203, p = "<.001", ci_lower = -806.8300,  ci_upper = -463.3854),   # tukey_test_output.txt:240
  "Intermediate Secondary - University"         = list(diff = -1741.11924, se = 76.53065, p = "<.001", ci_lower = -1937.8795, ci_upper = -1544.3590),  # tukey_test_output.txt:241
  "Academic Secondary - University"             = list(diff = -1106.01151, se = 76.53455, p = "<.001", ci_lower = -1302.7818, ci_upper = -909.2412)    # tukey_test_output.txt:244
)

spss_values$test_4a_east_w <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -0.327, se = 0.140, p = 0.090, ci_lower = -0.69, ci_upper = 0.03),   # tukey_test_output.txt:573
  "Basic Secondary - Academic Secondary"        = list(diff = -0.505, se = 0.143, p = 0.002, ci_lower = -0.87, ci_upper = -0.14),  # tukey_test_output.txt:574
  "Basic Secondary - University"                = list(diff = -0.639, se = 0.163, p = 0.001, ci_lower = -1.06, ci_upper = -0.22),  # tukey_test_output.txt:575
  "Intermediate Secondary - Academic Secondary" = list(diff = -0.178, se = 0.151, p = 0.643, ci_lower = -0.57, ci_upper = 0.21),   # tukey_test_output.txt:577
  "Intermediate Secondary - University"         = list(diff = -0.311, se = 0.170, p = 0.262, ci_lower = -0.75, ci_upper = 0.13),   # tukey_test_output.txt:578
  "Academic Secondary - University"             = list(diff = -0.133, se = 0.173, p = 0.867, ci_lower = -0.58, ci_upper = 0.31)    # tukey_test_output.txt:581
)

compare_tukey_pairs <- function(res, pairs, prec_diff, prec_ci, scenario) {
  for (pname in names(pairs)) {
    expected <- pairs[[pname]]
    row <- res[res$Comparison == pname, , drop = FALSE]
    if (nrow(row) != 1L) {
      stop(sprintf("[%s] comparison %s not found", scenario, pname), call. = FALSE)
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

test_that("Test 2b: weighted Tukey income by education — matches SPSS", {
  av <- survey_data |>
    oneway_anova(income, group = education, weights = sampling_weight)
  compare_tukey_pairs(tukey_test(av)$results, spss_values$test_2b_income_w,
                      prec_diff = 5, prec_ci = 4, scenario = "2b weighted")
})

test_that("Test 4a: weighted grouped Tukey (East) — matches SPSS", {
  av <- survey_data |> group_by(region) |>
    oneway_anova(life_satisfaction, group = education, weights = sampling_weight)
  res <- tukey_test(av)$results
  compare_tukey_pairs(res[res$region == "East", , drop = FALSE],
                      spss_values$test_4a_east_w,
                      prec_diff = 3, prec_ci = 2, scenario = "4a East weighted")
})
