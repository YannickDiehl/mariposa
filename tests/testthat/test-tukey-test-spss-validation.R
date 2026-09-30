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


# =============================================================================
# Test 6a: CILEVEL(.90) with /POSTHOC ALPHA(.05) — post-hoc CI stays 95%
# =============================================================================
# SPSS's post-hoc interval follows the post-hoc ALPHA, not the ANOVA's
# CILEVEL (which only sets the Descriptives intervals).

spss_values$test_6a_cilevel90 <- list(
  "Basic Secondary - Intermediate Secondary" = list(ci_lower = -0.65, ci_upper = -0.34),  # tukey_test_output.txt:793
  "Basic Secondary - University"             = list(ci_lower = -1.02, ci_upper = -0.67)   # tukey_test_output.txt:795
)

test_that("Test 6a: ANOVA conf.level = .90 keeps the 95% Tukey CI — matches SPSS", {
  av <- survey_data |>
    oneway_anova(life_satisfaction, group = education, conf.level = 0.90)
  res <- tukey_test(av)$results
  for (pname in names(spss_values$test_6a_cilevel90)) {
    expected <- spss_values$test_6a_cilevel90[[pname]]
    row <- res[res$Comparison == pname, , drop = FALSE]
    assert_spss(as.numeric(row$conf_low), expected$ci_lower, tier = "display",
                precision = 2, label = sprintf("[6a] %s CI lower", pname))
    assert_spss(as.numeric(row$conf_high), expected$ci_upper, tier = "display",
                precision = 2, label = sprintf("[6a] %s CI upper", pname))
  }
})


# =============================================================================
# Test 1f: income by employment (5 groups) — 4-decimal confidence limits
# =============================================================================
# Needs the studentized-range quantile to more digits than qtukey() gives
# (about 1e-8 relative): 214.4432484 would print as 214.4432.

spss_values$test_1f_income_employment <- list(
  "Student - Employed"      = list(diff = 907.63143,  se = 180.90363, p = "<.001", ci_lower = 413.7538,  ci_upper = 1401.5090),  # tukey_test_output.txt:174
  "Student - Unemployed"    = list(diff = 987.65360,  se = 209.86916, p = "<.001", ci_lower = 414.6984,  ci_upper = 1560.6088),  # tukey_test_output.txt:175
  "Student - Retired"       = list(diff = 886.02319,  se = 188.62321, p = "<.001", ci_lower = 371.0707,  ci_upper = 1400.9757),  # tukey_test_output.txt:176
  "Student - Other"         = list(diff = 833.29779,  se = 226.68174, p = 0.002,   ci_lower = 214.4433,  ci_upper = 1452.1523),  # tukey_test_output.txt:177
  "Employed - Unemployed"   = list(diff = 80.02217,   se = 119.34373, p = 0.963,   ci_lower = -245.7933, ci_upper = 405.8376),   # tukey_test_output.txt:179
  "Employed - Retired"      = list(diff = -21.60824,  se = 76.00378,  p = 0.999,   ci_lower = -229.1031, ci_upper = 185.8866),   # tukey_test_output.txt:180
  "Employed - Other"        = list(diff = -74.33364,  se = 146.90974, p = 0.987,   ci_lower = -475.4059, ci_upper = 326.7386),   # tukey_test_output.txt:181
  "Unemployed - Retired"    = list(diff = -101.63041, se = 130.74983, p = 0.937,   ci_lower = -458.5852, ci_upper = 255.3243),   # tukey_test_output.txt:184
  "Unemployed - Other"      = list(diff = -154.35581, se = 181.38747, p = 0.914,   ci_lower = -649.5543, ci_upper = 340.8427),   # tukey_test_output.txt:185
  "Retired - Other"         = list(diff = -52.72540,  se = 156.31719, p = 0.997,   ci_lower = -479.4806, ci_upper = 374.0298)    # tukey_test_output.txt:189
)

test_that("Test 1f: Tukey income by employment — matches SPSS", {
  av <- survey_data |> oneway_anova(income, group = employment)
  compare_tukey_pairs(tukey_test(av)$results,
                      spss_values$test_1f_income_employment,
                      prec_diff = 5, prec_ci = 4, scenario = "1f")
})
