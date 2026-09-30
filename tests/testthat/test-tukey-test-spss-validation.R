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


# =============================================================================
# REMAINING SPSS SECTIONS — every pair of every table (0.7.4 audit)
# =============================================================================
# Syntax: tests/spss_reference/syntax/tukey_test.sps. One entry per pair in
# SPSS's first-listed (I)-(J) orientation (I before J in the category order);
# the mirrored (J)-(I) rows repeat the same numbers with flipped signs.
# SPLIT FILE tables are keyed by region, multi-variable tables by variable.

spss_values$test_1b_income <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -833.47063, se = 63.16256, p = "<.001", ci_lower = -995.8624, ci_upper = -671.0789),  # tukey_test_output.txt:41
  "Basic Secondary - Academic Secondary"        = list(diff = -1465.03997, se = 63.16256, p = "<.001", ci_lower = -1627.4317, ci_upper = -1302.6482),  # tukey_test_output.txt:42
  "Basic Secondary - University"                = list(diff = -2578.13548, se = 72.33288, p = "<.001", ci_lower = -2764.1042, ci_upper = -2392.1668),  # tukey_test_output.txt:43
  "Intermediate Secondary - Academic Secondary" = list(diff = -631.56934, se = 67.60909, p = "<.001", ci_lower = -805.3931, ci_upper = -457.7455),  # tukey_test_output.txt:45
  "Intermediate Secondary - University"         = list(diff = -1744.66485, se = 76.24648, p = "<.001", ci_lower = -1940.6955, ci_upper = -1548.6342),  # tukey_test_output.txt:46
  "Academic Secondary - University"             = list(diff = -1113.09551, se = 76.24648, p = "<.001", ci_lower = -1309.1261, ci_upper = -917.0649)  # tukey_test_output.txt:49
)

spss_values$test_1c_age <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -1.07584, se = 0.89465, p = 0.625, ci_lower = -3.3758, ci_upper = 1.2241),  # tukey_test_output.txt:67
  "Basic Secondary - Academic Secondary"        = list(diff = -1.11010, se = 0.89384, p = 0.600, ci_lower = -3.4079, ci_upper = 1.1878),  # tukey_test_output.txt:68
  "Basic Secondary - University"                = list(diff = 0.73808, se = 1.03168, p = 0.891, ci_lower = -1.9141, ci_upper = 3.3903),  # tukey_test_output.txt:69
  "Intermediate Secondary - Academic Secondary" = list(diff = -0.03426, se = 0.95623, p = 1.000, ci_lower = -2.4925, ci_upper = 2.4240),  # tukey_test_output.txt:71
  "Intermediate Secondary - University"         = list(diff = 1.81392, se = 1.08618, p = 0.340, ci_lower = -0.9784, ci_upper = 4.6062),  # tukey_test_output.txt:72
  "Academic Secondary - University"             = list(diff = 1.84818, se = 1.08551, p = 0.322, ci_lower = -0.9424, ci_upper = 4.6388)  # tukey_test_output.txt:75
)

spss_values$test_1d_trust <- list(
  trust_government = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = 0.066, se = 0.063, p = 0.722, ci_lower = -0.10, ci_upper = 0.23),  # tukey_test_output.txt:91
    "Basic Secondary - Academic Secondary"        = list(diff = 0.037, se = 0.063, p = 0.935, ci_lower = -0.13, ci_upper = 0.20),  # tukey_test_output.txt:92
    "Basic Secondary - University"                = list(diff = 0.004, se = 0.073, p = 1.000, ci_lower = -0.18, ci_upper = 0.19),  # tukey_test_output.txt:93
    "Intermediate Secondary - Academic Secondary" = list(diff = -0.029, se = 0.068, p = 0.973, ci_lower = -0.20, ci_upper = 0.14),  # tukey_test_output.txt:95
    "Intermediate Secondary - University"         = list(diff = -0.063, se = 0.077, p = 0.847, ci_lower = -0.26, ci_upper = 0.13),  # tukey_test_output.txt:96
    "Academic Secondary - University"             = list(diff = -0.034, se = 0.077, p = 0.972, ci_lower = -0.23, ci_upper = 0.16)  # tukey_test_output.txt:99
  ),
  trust_media = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -0.057, se = 0.063, p = 0.802, ci_lower = -0.22, ci_upper = 0.11),  # tukey_test_output.txt:103
    "Basic Secondary - Academic Secondary"        = list(diff = -0.021, se = 0.063, p = 0.987, ci_lower = -0.18, ci_upper = 0.14),  # tukey_test_output.txt:104
    "Basic Secondary - University"                = list(diff = 0.066, se = 0.073, p = 0.799, ci_lower = -0.12, ci_upper = 0.25),  # tukey_test_output.txt:105
    "Intermediate Secondary - Academic Secondary" = list(diff = 0.036, se = 0.067, p = 0.951, ci_lower = -0.14, ci_upper = 0.21),  # tukey_test_output.txt:107
    "Intermediate Secondary - University"         = list(diff = 0.123, se = 0.077, p = 0.373, ci_lower = -0.07, ci_upper = 0.32),  # tukey_test_output.txt:108
    "Academic Secondary - University"             = list(diff = 0.087, se = 0.076, p = 0.663, ci_lower = -0.11, ci_upper = 0.28)  # tukey_test_output.txt:111
  ),
  trust_science = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = 0.028, se = 0.055, p = 0.956, ci_lower = -0.11, ci_upper = 0.17),  # tukey_test_output.txt:115
    "Basic Secondary - Academic Secondary"        = list(diff = -0.049, se = 0.055, p = 0.817, ci_lower = -0.19, ci_upper = 0.09),  # tukey_test_output.txt:116
    "Basic Secondary - University"                = list(diff = 0.011, se = 0.064, p = 0.998, ci_lower = -0.15, ci_upper = 0.17),  # tukey_test_output.txt:117
    "Intermediate Secondary - Academic Secondary" = list(diff = -0.077, se = 0.059, p = 0.563, ci_lower = -0.23, ci_upper = 0.08),  # tukey_test_output.txt:119
    "Intermediate Secondary - University"         = list(diff = -0.018, se = 0.067, p = 0.993, ci_lower = -0.19, ci_upper = 0.15),  # tukey_test_output.txt:120
    "Academic Secondary - University"             = list(diff = 0.059, se = 0.067, p = 0.815, ci_lower = -0.11, ci_upper = 0.23)  # tukey_test_output.txt:123
  )
)

spss_values$test_1e_life_employment <- list(
  "Student - Employed"    = list(diff = 0.321, se = 0.136, p = 0.128, ci_lower = -0.05, ci_upper = 0.69),  # tukey_test_output.txt:140
  "Student - Unemployed"  = list(diff = 0.284, se = 0.159, p = 0.380, ci_lower = -0.15, ci_upper = 0.72),  # tukey_test_output.txt:141
  "Student - Retired"     = list(diff = 0.389, se = 0.142, p = 0.050, ci_lower = 0.00, ci_upper = 0.78),  # tukey_test_output.txt:142
  "Student - Other"       = list(diff = 0.232, se = 0.172, p = 0.659, ci_lower = -0.24, ci_upper = 0.70),  # tukey_test_output.txt:143
  "Employed - Unemployed" = list(diff = -0.037, se = 0.091, p = 0.994, ci_lower = -0.29, ci_upper = 0.21),  # tukey_test_output.txt:145
  "Employed - Retired"    = list(diff = 0.068, se = 0.059, p = 0.774, ci_lower = -0.09, ci_upper = 0.23),  # tukey_test_output.txt:146
  "Employed - Other"      = list(diff = -0.088, se = 0.113, p = 0.935, ci_lower = -0.40, ci_upper = 0.22),  # tukey_test_output.txt:147
  "Unemployed - Retired"  = list(diff = 0.105, se = 0.100, p = 0.832, ci_lower = -0.17, ci_upper = 0.38),  # tukey_test_output.txt:150
  "Unemployed - Other"    = list(diff = -0.051, se = 0.139, p = 0.996, ci_lower = -0.43, ci_upper = 0.33),  # tukey_test_output.txt:151
  "Retired - Other"       = list(diff = -0.157, se = 0.120, p = 0.690, ci_lower = -0.48, ci_upper = 0.17)  # tukey_test_output.txt:155
)

spss_values$test_2a_life_w <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -0.490, se = 0.059, p = "<.001", ci_lower = -0.64, ci_upper = -0.34),  # tukey_test_output.txt:210
  "Basic Secondary - Academic Secondary"        = list(diff = -0.643, se = 0.059, p = "<.001", ci_lower = -0.79, ci_upper = -0.49),  # tukey_test_output.txt:211
  "Basic Secondary - University"                = list(diff = -0.832, se = 0.069, p = "<.001", ci_lower = -1.01, ci_upper = -0.65),  # tukey_test_output.txt:212
  "Intermediate Secondary - Academic Secondary" = list(diff = -0.153, se = 0.063, p = 0.071, ci_lower = -0.31, ci_upper = 0.01),  # tukey_test_output.txt:214
  "Intermediate Secondary - University"         = list(diff = -0.342, se = 0.072, p = "<.001", ci_lower = -0.53, ci_upper = -0.16),  # tukey_test_output.txt:215
  "Academic Secondary - University"             = list(diff = -0.189, se = 0.073, p = 0.046, ci_lower = -0.38, ci_upper = 0.00)  # tukey_test_output.txt:218
)

spss_values$test_2c_age_w <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -0.84360, se = 0.89412, p = 0.781, ci_lower = -3.1422, ci_upper = 1.4550),  # tukey_test_output.txt:262
  "Basic Secondary - Academic Secondary"        = list(diff = -1.06554, se = 0.89371, p = 0.632, ci_lower = -3.3631, ci_upper = 1.2320),  # tukey_test_output.txt:263
  "Basic Secondary - University"                = list(diff = 0.64261, se = 1.04965, p = 0.928, ci_lower = -2.0558, ci_upper = 3.3410),  # tukey_test_output.txt:264
  "Intermediate Secondary - Academic Secondary" = list(diff = -0.22195, se = 0.95392, p = 0.996, ci_lower = -2.6742, ci_upper = 2.2304),  # tukey_test_output.txt:266
  "Intermediate Secondary - University"         = list(diff = 1.48621, se = 1.10137, p = 0.531, ci_lower = -1.3451, ci_upper = 4.3176),  # tukey_test_output.txt:267
  "Academic Secondary - University"             = list(diff = 1.70815, se = 1.10104, p = 0.407, ci_lower = -1.1224, ci_upper = 4.5387)  # tukey_test_output.txt:270
)

spss_values$test_2d_trust_w <- list(
  trust_government = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = 0.063, se = 0.063, p = 0.744, ci_lower = -0.10, ci_upper = 0.22),  # tukey_test_output.txt:286
    "Basic Secondary - Academic Secondary"        = list(diff = 0.041, se = 0.063, p = 0.916, ci_lower = -0.12, ci_upper = 0.20),  # tukey_test_output.txt:287
    "Basic Secondary - University"                = list(diff = 0.006, se = 0.074, p = 1.000, ci_lower = -0.18, ci_upper = 0.20),  # tukey_test_output.txt:288
    "Intermediate Secondary - Academic Secondary" = list(diff = -0.023, se = 0.067, p = 0.986, ci_lower = -0.19, ci_upper = 0.15),  # tukey_test_output.txt:290
    "Intermediate Secondary - University"         = list(diff = -0.057, se = 0.077, p = 0.882, ci_lower = -0.26, ci_upper = 0.14),  # tukey_test_output.txt:291
    "Academic Secondary - University"             = list(diff = -0.034, se = 0.077, p = 0.971, ci_lower = -0.23, ci_upper = 0.16)  # tukey_test_output.txt:294
  ),
  trust_media = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -0.060, se = 0.063, p = 0.777, ci_lower = -0.22, ci_upper = 0.10),  # tukey_test_output.txt:298
    "Basic Secondary - Academic Secondary"        = list(diff = -0.017, se = 0.063, p = 0.993, ci_lower = -0.18, ci_upper = 0.14),  # tukey_test_output.txt:299
    "Basic Secondary - University"                = list(diff = 0.057, se = 0.074, p = 0.868, ci_lower = -0.13, ci_upper = 0.25),  # tukey_test_output.txt:300
    "Intermediate Secondary - Academic Secondary" = list(diff = 0.043, se = 0.067, p = 0.920, ci_lower = -0.13, ci_upper = 0.21),  # tukey_test_output.txt:302
    "Intermediate Secondary - University"         = list(diff = 0.117, se = 0.077, p = 0.434, ci_lower = -0.08, ci_upper = 0.32),  # tukey_test_output.txt:303
    "Academic Secondary - University"             = list(diff = 0.074, se = 0.077, p = 0.775, ci_lower = -0.12, ci_upper = 0.27)  # tukey_test_output.txt:306
  ),
  trust_science = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = 0.028, se = 0.055, p = 0.958, ci_lower = -0.11, ci_upper = 0.17),  # tukey_test_output.txt:310
    "Basic Secondary - Academic Secondary"        = list(diff = -0.043, se = 0.055, p = 0.863, ci_lower = -0.18, ci_upper = 0.10),  # tukey_test_output.txt:311
    "Basic Secondary - University"                = list(diff = 0.022, se = 0.064, p = 0.986, ci_lower = -0.14, ci_upper = 0.19),  # tukey_test_output.txt:312
    "Intermediate Secondary - Academic Secondary" = list(diff = -0.071, se = 0.059, p = 0.624, ci_lower = -0.22, ci_upper = 0.08),  # tukey_test_output.txt:314
    "Intermediate Secondary - University"         = list(diff = -0.006, se = 0.067, p = 1.000, ci_lower = -0.18, ci_upper = 0.17),  # tukey_test_output.txt:315
    "Academic Secondary - University"             = list(diff = 0.065, se = 0.068, p = 0.772, ci_lower = -0.11, ci_upper = 0.24)  # tukey_test_output.txt:318
  )
)

spss_values$test_2e_life_employment_w <- list(
  "Student - Employed"    = list(diff = 0.306, se = 0.135, p = 0.154, ci_lower = -0.06, ci_upper = 0.67),  # tukey_test_output.txt:335
  "Student - Unemployed"  = list(diff = 0.263, se = 0.157, p = 0.451, ci_lower = -0.17, ci_upper = 0.69),  # tukey_test_output.txt:336
  "Student - Retired"     = list(diff = 0.377, se = 0.141, p = 0.058, ci_lower = -0.01, ci_upper = 0.76),  # tukey_test_output.txt:337
  "Student - Other"       = list(diff = 0.209, se = 0.171, p = 0.738, ci_lower = -0.26, ci_upper = 0.67),  # tukey_test_output.txt:338
  "Employed - Unemployed" = list(diff = -0.043, se = 0.091, p = 0.989, ci_lower = -0.29, ci_upper = 0.20),  # tukey_test_output.txt:340
  "Employed - Retired"    = list(diff = 0.071, se = 0.058, p = 0.745, ci_lower = -0.09, ci_upper = 0.23),  # tukey_test_output.txt:341
  "Employed - Other"      = list(diff = -0.098, se = 0.112, p = 0.908, ci_lower = -0.40, ci_upper = 0.21),  # tukey_test_output.txt:342
  "Unemployed - Retired"  = list(diff = 0.114, se = 0.100, p = 0.782, ci_lower = -0.16, ci_upper = 0.39),  # tukey_test_output.txt:345
  "Unemployed - Other"    = list(diff = -0.054, se = 0.138, p = 0.995, ci_lower = -0.43, ci_upper = 0.32),  # tukey_test_output.txt:346
  "Retired - Other"       = list(diff = -0.168, se = 0.120, p = 0.624, ci_lower = -0.50, ci_upper = 0.16)  # tukey_test_output.txt:350
)

spss_values$test_2f_income_employment_w <- list(
  "Student - Employed"    = list(diff = 939.19226, se = 177.84237, p = "<.001", ci_lower = 453.6747, ci_upper = 1424.7098),  # tukey_test_output.txt:368
  "Student - Unemployed"  = list(diff = 1014.11142, se = 206.53369, p = "<.001", ci_lower = 450.2653, ci_upper = 1577.9576),  # tukey_test_output.txt:369
  "Student - Retired"     = list(diff = 906.12176, se = 185.41085, p = "<.001", ci_lower = 399.9419, ci_upper = 1412.3016),  # tukey_test_output.txt:370
  "Student - Other"       = list(diff = 864.50898, se = 223.50773, p = 0.001, ci_lower = 254.3229, ci_upper = 1474.6950),  # tukey_test_output.txt:371
  "Employed - Unemployed" = list(diff = 74.91917, se = 117.92950, p = 0.969, ci_lower = -247.0336, ci_upper = 396.8719),  # tukey_test_output.txt:373
  "Employed - Retired"    = list(diff = -33.07050, se = 75.02255, p = 0.992, ci_lower = -237.8854, ci_upper = 171.7444),  # tukey_test_output.txt:374
  "Employed - Other"      = list(diff = -74.68328, se = 145.62592, p = 0.986, ci_lower = -472.2485, ci_upper = 322.8819),  # tukey_test_output.txt:375
  "Unemployed - Retired"  = list(diff = -107.98967, se = 129.06061, p = 0.919, ci_lower = -460.3309, ci_upper = 244.3515),  # tukey_test_output.txt:378
  "Unemployed - Other"    = list(diff = -149.60244, se = 179.54154, p = 0.920, ci_lower = -639.7588, ci_upper = 340.5539),  # tukey_test_output.txt:379
  "Retired - Other"       = list(diff = -41.61278, se = 154.77785, p = 0.999, ci_lower = -464.1632, ci_upper = 380.9376)  # tukey_test_output.txt:383
)

spss_values$test_3a_life_split <- list(
  East = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -0.334, se = 0.143, p = 0.092, ci_lower = -0.70, ci_upper = 0.03),  # tukey_test_output.txt:404
    "Basic Secondary - Academic Secondary"        = list(diff = -0.532, se = 0.146, p = 0.002, ci_lower = -0.91, ci_upper = -0.15),  # tukey_test_output.txt:405
    "Basic Secondary - University"                = list(diff = -0.642, se = 0.166, p = 0.001, ci_lower = -1.07, ci_upper = -0.22),  # tukey_test_output.txt:406
    "Intermediate Secondary - Academic Secondary" = list(diff = -0.198, se = 0.157, p = 0.587, ci_lower = -0.60, ci_upper = 0.21),  # tukey_test_output.txt:408
    "Intermediate Secondary - University"         = list(diff = -0.308, se = 0.175, p = 0.292, ci_lower = -0.76, ci_upper = 0.14),  # tukey_test_output.txt:409
    "Academic Secondary - University"             = list(diff = -0.110, se = 0.177, p = 0.925, ci_lower = -0.57, ci_upper = 0.35)  # tukey_test_output.txt:412
  ),
  West = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -0.536, se = 0.065, p = "<.001", ci_lower = -0.70, ci_upper = -0.37),  # tukey_test_output.txt:416
    "Basic Secondary - Academic Secondary"        = list(diff = -0.678, se = 0.065, p = "<.001", ci_lower = -0.85, ci_upper = -0.51),  # tukey_test_output.txt:417
    "Basic Secondary - University"                = list(diff = -0.892, se = 0.075, p = "<.001", ci_lower = -1.08, ci_upper = -0.70),  # tukey_test_output.txt:418
    "Intermediate Secondary - Academic Secondary" = list(diff = -0.142, se = 0.069, p = 0.170, ci_lower = -0.32, ci_upper = 0.04),  # tukey_test_output.txt:420
    "Intermediate Secondary - University"         = list(diff = -0.355, se = 0.079, p = "<.001", ci_lower = -0.56, ci_upper = -0.15),  # tukey_test_output.txt:421
    "Academic Secondary - University"             = list(diff = -0.213, se = 0.079, p = 0.034, ci_lower = -0.42, ci_upper = -0.01)  # tukey_test_output.txt:424
  )
)

spss_values$test_3b_income_split <- list(
  East = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -711.94320, se = 141.45344, p = "<.001", ci_lower = -1076.7833, ci_upper = -347.1031),  # tukey_test_output.txt:442
    "Basic Secondary - Academic Secondary"        = list(diff = -1302.59740, se = 145.23536, p = "<.001", ci_lower = -1677.1919, ci_upper = -928.0029),  # tukey_test_output.txt:443
    "Basic Secondary - University"                = list(diff = -2328.31169, se = 162.01683, p = "<.001", ci_lower = -2746.1894, ci_upper = -1910.4340),  # tukey_test_output.txt:444
    "Intermediate Secondary - Academic Secondary" = list(diff = -590.65421, se = 157.15113, p = 0.001, ci_lower = -995.9822, ci_upper = -185.3263),  # tukey_test_output.txt:446
    "Intermediate Secondary - University"         = list(diff = -1616.36849, se = 172.77910, p = "<.001", ci_lower = -2062.0045, ci_upper = -1170.7325),  # tukey_test_output.txt:447
    "Academic Secondary - University"             = list(diff = -1025.71429, se = 175.88876, p = "<.001", ci_lower = -1479.3708, ci_upper = -572.0578)  # tukey_test_output.txt:450
  ),
  West = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -866.06016, se = 70.60782, p = "<.001", ci_lower = -1047.6280, ci_upper = -684.4923),  # tukey_test_output.txt:454
    "Basic Secondary - Academic Secondary"        = list(diff = -1506.95812, se = 70.20527, p = "<.001", ci_lower = -1687.4908, ci_upper = -1326.4255),  # tukey_test_output.txt:455
    "Basic Secondary - University"                = list(diff = -2642.18619, se = 80.85058, p = "<.001", ci_lower = -2850.0932, ci_upper = -2434.2791),  # tukey_test_output.txt:456
    "Intermediate Secondary - Academic Secondary" = list(diff = -640.89796, se = 74.91141, p = "<.001", ci_lower = -833.5325, ci_upper = -448.2635),  # tukey_test_output.txt:458
    "Intermediate Secondary - University"         = list(diff = -1776.12603, se = 84.96915, p = "<.001", ci_lower = -1994.6240, ci_upper = -1557.6281),  # tukey_test_output.txt:459
    "Academic Secondary - University"             = list(diff = -1135.22807, se = 84.63494, p = "<.001", ci_lower = -1352.8666, ci_upper = -917.5896)  # tukey_test_output.txt:462
  )
)

spss_values$test_3c_age_split <- list(
  East = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = 0.04385, se = 2.07229, p = 1.000, ci_lower = -5.2986, ci_upper = 5.3863),  # tukey_test_output.txt:480
    "Basic Secondary - Academic Secondary"        = list(diff = -3.22097, se = 2.10364, p = 0.420, ci_lower = -8.6442, ci_upper = 2.2023),  # tukey_test_output.txt:481
    "Basic Secondary - University"                = list(diff = -0.99643, se = 2.37237, p = 0.975, ci_lower = -7.1125, ci_upper = 5.1196),  # tukey_test_output.txt:482
    "Intermediate Secondary - Academic Secondary" = list(diff = -3.26482, se = 2.26901, p = 0.476, ci_lower = -9.1144, ci_upper = 2.5848),  # tukey_test_output.txt:484
    "Intermediate Secondary - University"         = list(diff = -1.04028, se = 2.52017, p = 0.976, ci_lower = -7.5374, ci_upper = 5.4568),  # tukey_test_output.txt:485
    "Academic Secondary - University"             = list(diff = 2.22455, se = 2.54601, p = 0.818, ci_lower = -4.3392, ci_upper = 8.7882)  # tukey_test_output.txt:488
  ),
  West = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -1.35522, se = 0.99090, p = 0.520, ci_lower = -3.9030, ci_upper = 1.1926),  # tukey_test_output.txt:492
    "Basic Secondary - Academic Secondary"        = list(diff = -0.66515, se = 0.98652, p = 0.907, ci_lower = -3.2017, ci_upper = 1.8714),  # tukey_test_output.txt:493
    "Basic Secondary - University"                = list(diff = 1.16087, se = 1.14463, p = 0.741, ci_lower = -1.7822, ci_upper = 4.1039),  # tukey_test_output.txt:494
    "Intermediate Secondary - Academic Secondary" = list(diff = 0.69008, se = 1.05307, p = 0.914, ci_lower = -2.0176, ci_upper = 3.3977),  # tukey_test_output.txt:496
    "Intermediate Secondary - University"         = list(diff = 2.51609, se = 1.20247, p = 0.156, ci_lower = -0.5757, ci_upper = 5.6079),  # tukey_test_output.txt:497
    "Academic Secondary - University"             = list(diff = 1.82602, se = 1.19886, p = 0.424, ci_lower = -1.2565, ci_upper = 4.9085)  # tukey_test_output.txt:500
  )
)

spss_values$test_3d_life_employment_split <- list(
  East = list(
    "Student - Employed"    = list(diff = 0.075, se = 0.388, p = 1.000, ci_lower = -0.99, ci_upper = 1.14),  # tukey_test_output.txt:517
    "Student - Unemployed"  = list(diff = 0.067, se = 0.441, p = 1.000, ci_lower = -1.14, ci_upper = 1.27),  # tukey_test_output.txt:518
    "Student - Retired"     = list(diff = 0.186, se = 0.400, p = 0.990, ci_lower = -0.91, ci_upper = 1.28),  # tukey_test_output.txt:519
    "Student - Other"       = list(diff = -0.300, se = 0.464, p = 0.967, ci_lower = -1.57, ci_upper = 0.97),  # tukey_test_output.txt:520
    "Employed - Unemployed" = list(diff = -0.008, se = 0.231, p = 1.000, ci_lower = -0.64, ci_upper = 0.63),  # tukey_test_output.txt:522
    "Employed - Retired"    = list(diff = 0.111, se = 0.137, p = 0.927, ci_lower = -0.26, ci_upper = 0.49),  # tukey_test_output.txt:523
    "Employed - Other"      = list(diff = -0.375, se = 0.273, p = 0.645, ci_lower = -1.12, ci_upper = 0.37),  # tukey_test_output.txt:524
    "Unemployed - Retired"  = list(diff = 0.119, se = 0.250, p = 0.989, ci_lower = -0.57, ci_upper = 0.80),  # tukey_test_output.txt:527
    "Unemployed - Other"    = list(diff = -0.367, se = 0.344, p = 0.824, ci_lower = -1.31, ci_upper = 0.57),  # tukey_test_output.txt:528
    "Retired - Other"       = list(diff = -0.486, se = 0.289, p = 0.446, ci_lower = -1.28, ci_upper = 0.31)  # tukey_test_output.txt:532
  ),
  West = list(
    "Student - Employed"    = list(diff = 0.359, se = 0.145, p = 0.097, ci_lower = -0.04, ci_upper = 0.75),  # tukey_test_output.txt:537
    "Student - Unemployed"  = list(diff = 0.316, se = 0.170, p = 0.338, ci_lower = -0.15, ci_upper = 0.78),  # tukey_test_output.txt:538
    "Student - Retired"     = list(diff = 0.416, se = 0.152, p = 0.050, ci_lower = 0.00, ci_upper = 0.83),  # tukey_test_output.txt:539
    "Student - Other"       = list(diff = 0.336, se = 0.185, p = 0.364, ci_lower = -0.17, ci_upper = 0.84),  # tukey_test_output.txt:540
    "Employed - Unemployed" = list(diff = -0.043, se = 0.099, p = 0.993, ci_lower = -0.31, ci_upper = 0.23),  # tukey_test_output.txt:542
    "Employed - Retired"    = list(diff = 0.057, se = 0.065, p = 0.906, ci_lower = -0.12, ci_upper = 0.23),  # tukey_test_output.txt:543
    "Employed - Other"      = list(diff = -0.022, se = 0.124, p = 1.000, ci_lower = -0.36, ci_upper = 0.32),  # tukey_test_output.txt:544
    "Unemployed - Retired"  = list(diff = 0.100, se = 0.109, p = 0.892, ci_lower = -0.20, ci_upper = 0.40),  # tukey_test_output.txt:547
    "Unemployed - Other"    = list(diff = 0.021, se = 0.152, p = 1.000, ci_lower = -0.39, ci_upper = 0.43),  # tukey_test_output.txt:548
    "Retired - Other"       = list(diff = -0.079, se = 0.132, p = 0.975, ci_lower = -0.44, ci_upper = 0.28)  # tukey_test_output.txt:552
  )
)

spss_values$test_4a_west_w <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -0.531, se = 0.065, p = "<.001", ci_lower = -0.70, ci_upper = -0.36),  # tukey_test_output.txt:585
  "Basic Secondary - Academic Secondary"        = list(diff = -0.677, se = 0.065, p = "<.001", ci_lower = -0.84, ci_upper = -0.51),  # tukey_test_output.txt:586
  "Basic Secondary - University"                = list(diff = -0.882, se = 0.077, p = "<.001", ci_lower = -1.08, ci_upper = -0.69),  # tukey_test_output.txt:587
  "Intermediate Secondary - Academic Secondary" = list(diff = -0.146, se = 0.069, p = 0.146, ci_lower = -0.32, ci_upper = 0.03),  # tukey_test_output.txt:589
  "Intermediate Secondary - University"         = list(diff = -0.351, se = 0.080, p = "<.001", ci_lower = -0.56, ci_upper = -0.15),  # tukey_test_output.txt:590
  "Academic Secondary - University"             = list(diff = -0.205, se = 0.080, p = 0.051, ci_lower = -0.41, ci_upper = 0.00)  # tukey_test_output.txt:593
)

spss_values$test_4b_income_split_w <- list(
  East = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -726.59776, se = 139.03880, p = "<.001", ci_lower = -1085.1461, ci_upper = -368.0494),  # tukey_test_output.txt:611
    "Basic Secondary - Academic Secondary"        = list(diff = -1290.74934, se = 142.33512, p = "<.001", ci_lower = -1657.7981, ci_upper = -923.7006),  # tukey_test_output.txt:612
    "Basic Secondary - University"                = list(diff = -2330.17593, se = 160.43097, p = "<.001", ci_lower = -2743.8896, ci_upper = -1916.4622),  # tukey_test_output.txt:613
    "Intermediate Secondary - Academic Secondary" = list(diff = -564.15158, se = 153.06541, p = 0.001, ci_lower = -958.8712, ci_upper = -169.4319),  # tukey_test_output.txt:615
    "Intermediate Secondary - University"         = list(diff = -1603.57817, se = 170.02303, p = "<.001", ci_lower = -2042.0275, ci_upper = -1165.1288),  # tukey_test_output.txt:616
    "Academic Secondary - University"             = list(diff = -1039.42659, se = 172.72906, p = "<.001", ci_lower = -1484.8542, ci_upper = -593.9990)  # tukey_test_output.txt:619
  ),
  West = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -859.87622, se = 70.01596, p = "<.001", ci_lower = -1039.9227, ci_upper = -679.8298),  # tukey_test_output.txt:623
    "Basic Secondary - Academic Secondary"        = list(diff = -1512.29521, se = 69.64200, p = "<.001", ci_lower = -1691.3800, ci_upper = -1333.2104),  # tukey_test_output.txt:624
    "Basic Secondary - University"                = list(diff = -2637.35703, se = 81.76438, p = "<.001", ci_lower = -2847.6146, ci_upper = -2427.0994),  # tukey_test_output.txt:625
    "Intermediate Secondary - Academic Secondary" = list(diff = -652.41899, se = 74.22507, p = "<.001", ci_lower = -843.2892, ci_upper = -461.5488),  # tukey_test_output.txt:627
    "Intermediate Secondary - University"         = list(diff = -1777.48081, se = 85.70161, p = "<.001", ci_lower = -1997.8630, ci_upper = -1557.0986),  # tukey_test_output.txt:628
    "Academic Secondary - University"             = list(diff = -1125.06183, se = 85.39637, p = "<.001", ci_lower = -1344.6591, ci_upper = -905.4646)  # tukey_test_output.txt:631
  )
)

spss_values$test_4c_age_split_w <- list(
  East = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = 0.16042, se = 2.04093, p = 1.000, ci_lower = -5.1003, ci_upper = 5.4211),  # tukey_test_output.txt:649
    "Basic Secondary - Academic Secondary"        = list(diff = -3.57840, se = 2.06856, p = 0.309, ci_lower = -8.9103, ci_upper = 1.7535),  # tukey_test_output.txt:650
    "Basic Secondary - University"                = list(diff = -1.56890, se = 2.35120, p = 0.909, ci_lower = -7.6294, ci_upper = 4.4916),  # tukey_test_output.txt:651
    "Intermediate Secondary - Academic Secondary" = list(diff = -3.73882, se = 2.21620, p = 0.332, ci_lower = -9.4513, ci_upper = 1.9737),  # tukey_test_output.txt:653
    "Intermediate Secondary - University"         = list(diff = -1.72932, se = 2.48208, p = 0.898, ci_lower = -8.1272, ci_upper = 4.6685),  # tukey_test_output.txt:654
    "Academic Secondary - University"             = list(diff = 2.00950, se = 2.50485, p = 0.853, ci_lower = -4.4470, ci_upper = 8.4660)  # tukey_test_output.txt:657
  ),
  West = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -1.10520, se = 0.99236, p = 0.681, ci_lower = -3.6567, ci_upper = 1.4463),  # tukey_test_output.txt:661
    "Basic Secondary - Academic Secondary"        = list(diff = -0.49437, se = 0.98864, p = 0.959, ci_lower = -3.0363, ci_upper = 2.0476),  # tukey_test_output.txt:662
    "Basic Secondary - University"                = list(diff = 1.25441, se = 1.17076, p = 0.707, ci_lower = -1.7558, ci_upper = 4.2646),  # tukey_test_output.txt:663
    "Intermediate Secondary - Academic Secondary" = list(diff = 0.61083, se = 1.05415, p = 0.938, ci_lower = -2.0996, ci_upper = 3.3212),  # tukey_test_output.txt:665
    "Intermediate Secondary - University"         = list(diff = 2.35961, se = 1.22658, p = 0.218, ci_lower = -0.7942, ci_upper = 5.5134),  # tukey_test_output.txt:666
    "Academic Secondary - University"             = list(diff = 1.74877, se = 1.22357, p = 0.481, ci_lower = -1.3973, ci_upper = 4.8948)  # tukey_test_output.txt:669
  )
)

spss_values$test_4d_life_employment_split_w <- list(
  East = list(
    "Student - Employed"    = list(diff = 0.010, se = 0.379, p = 1.000, ci_lower = -1.03, ci_upper = 1.05),  # tukey_test_output.txt:686
    "Student - Unemployed"  = list(diff = -0.010, se = 0.429, p = 1.000, ci_lower = -1.19, ci_upper = 1.17),  # tukey_test_output.txt:687
    "Student - Retired"     = list(diff = 0.144, se = 0.389, p = 0.996, ci_lower = -0.92, ci_upper = 1.21),  # tukey_test_output.txt:688
    "Student - Other"       = list(diff = -0.398, se = 0.455, p = 0.906, ci_lower = -1.64, ci_upper = 0.85),  # tukey_test_output.txt:689
    "Employed - Unemployed" = list(diff = -0.020, se = 0.224, p = 1.000, ci_lower = -0.63, ci_upper = 0.59),  # tukey_test_output.txt:691
    "Employed - Retired"    = list(diff = 0.134, se = 0.131, p = 0.844, ci_lower = -0.23, ci_upper = 0.49),  # tukey_test_output.txt:692
    "Employed - Other"      = list(diff = -0.408, se = 0.270, p = 0.556, ci_lower = -1.15, ci_upper = 0.33),  # tukey_test_output.txt:693
    "Unemployed - Retired"  = list(diff = 0.154, se = 0.241, p = 0.968, ci_lower = -0.51, ci_upper = 0.81),  # tukey_test_output.txt:696
    "Unemployed - Other"    = list(diff = -0.388, se = 0.337, p = 0.779, ci_lower = -1.31, ci_upper = 0.54),  # tukey_test_output.txt:697
    "Retired - Other"       = list(diff = -0.542, se = 0.284, p = 0.314, ci_lower = -1.32, ci_upper = 0.24)  # tukey_test_output.txt:701
  ),
  West = list(
    "Student - Employed"    = list(diff = 0.354, se = 0.144, p = 0.099, ci_lower = -0.04, ci_upper = 0.75),  # tukey_test_output.txt:706
    "Student - Unemployed"  = list(diff = 0.305, se = 0.168, p = 0.366, ci_lower = -0.15, ci_upper = 0.77),  # tukey_test_output.txt:707
    "Student - Retired"     = list(diff = 0.407, se = 0.151, p = 0.055, ci_lower = -0.01, ci_upper = 0.82),  # tukey_test_output.txt:708
    "Student - Other"       = list(diff = 0.329, se = 0.184, p = 0.380, ci_lower = -0.17, ci_upper = 0.83),  # tukey_test_output.txt:709
    "Employed - Unemployed" = list(diff = -0.049, se = 0.099, p = 0.988, ci_lower = -0.32, ci_upper = 0.22),  # tukey_test_output.txt:711
    "Employed - Retired"    = list(diff = 0.053, se = 0.065, p = 0.928, ci_lower = -0.13, ci_upper = 0.23),  # tukey_test_output.txt:712
    "Employed - Other"      = list(diff = -0.025, se = 0.124, p = 1.000, ci_lower = -0.36, ci_upper = 0.31),  # tukey_test_output.txt:713
    "Unemployed - Retired"  = list(diff = 0.102, se = 0.109, p = 0.885, ci_lower = -0.20, ci_upper = 0.40),  # tukey_test_output.txt:716
    "Unemployed - Other"    = list(diff = 0.024, se = 0.152, p = 1.000, ci_lower = -0.39, ci_upper = 0.44),  # tukey_test_output.txt:717
    "Retired - Other"       = list(diff = -0.078, se = 0.132, p = 0.976, ci_lower = -0.44, ci_upper = 0.28)  # tukey_test_output.txt:721
  )
)

spss_values$test_5a_alpha01 <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -0.497, se = 0.059, p = "<.001", ci_lower = -0.68, ci_upper = -0.31),  # tukey_test_output.txt:741
  "Basic Secondary - Academic Secondary"        = list(diff = -0.649, se = 0.060, p = "<.001", ci_lower = -0.83, ci_upper = -0.46),  # tukey_test_output.txt:742
  "Basic Secondary - University"                = list(diff = -0.843, se = 0.069, p = "<.001", ci_lower = -1.06, ci_upper = -0.63),  # tukey_test_output.txt:743
  "Intermediate Secondary - Academic Secondary" = list(diff = -0.153, se = 0.063, p = 0.075, ci_lower = -0.35, ci_upper = 0.04),  # tukey_test_output.txt:745
  "Intermediate Secondary - University"         = list(diff = -0.346, se = 0.072, p = "<.001", ci_lower = -0.57, ci_upper = -0.12),  # tukey_test_output.txt:746
  "Academic Secondary - University"             = list(diff = -0.193, se = 0.072, p = 0.037, ci_lower = -0.42, ci_upper = 0.03)  # tukey_test_output.txt:749
)

spss_values$test_5b_alpha10 <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -0.497, se = 0.059, p = "<.001", ci_lower = -0.63, ci_upper = -0.36),  # tukey_test_output.txt:767
  "Basic Secondary - Academic Secondary"        = list(diff = -0.649, se = 0.060, p = "<.001", ci_lower = -0.79, ci_upper = -0.51),  # tukey_test_output.txt:768
  "Basic Secondary - University"                = list(diff = -0.843, se = 0.069, p = "<.001", ci_lower = -1.00, ci_upper = -0.69),  # tukey_test_output.txt:769
  "Intermediate Secondary - Academic Secondary" = list(diff = -0.153, se = 0.063, p = 0.075, ci_lower = -0.30, ci_upper = -0.01),  # tukey_test_output.txt:771
  "Intermediate Secondary - University"         = list(diff = -0.346, se = 0.072, p = "<.001", ci_lower = -0.51, ci_upper = -0.18),  # tukey_test_output.txt:772
  "Academic Secondary - University"             = list(diff = -0.193, se = 0.072, p = 0.037, ci_lower = -0.36, ci_upper = -0.03)  # tukey_test_output.txt:775
)

spss_values$test_6b_cilevel99 <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -0.497, se = 0.059, p = "<.001", ci_lower = -0.65, ci_upper = -0.34),  # tukey_test_output.txt:819
  "Basic Secondary - Academic Secondary"        = list(diff = -0.649, se = 0.060, p = "<.001", ci_lower = -0.80, ci_upper = -0.50),  # tukey_test_output.txt:820
  "Basic Secondary - University"                = list(diff = -0.843, se = 0.069, p = "<.001", ci_lower = -1.02, ci_upper = -0.67),  # tukey_test_output.txt:821
  "Intermediate Secondary - Academic Secondary" = list(diff = -0.153, se = 0.063, p = 0.075, ci_lower = -0.32, ci_upper = 0.01),  # tukey_test_output.txt:823
  "Intermediate Secondary - University"         = list(diff = -0.346, se = 0.072, p = "<.001", ci_lower = -0.53, ci_upper = -0.16),  # tukey_test_output.txt:824
  "Academic Secondary - University"             = list(diff = -0.193, se = 0.072, p = 0.037, ci_lower = -0.38, ci_upper = -0.01)  # tukey_test_output.txt:827
)

spss_values$test_7_multi <- list(
  life_satisfaction = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -0.497, se = 0.059, p = "<.001", ci_lower = -0.65, ci_upper = -0.34),  # tukey_test_output.txt:844
    "Basic Secondary - Academic Secondary"        = list(diff = -0.649, se = 0.060, p = "<.001", ci_lower = -0.80, ci_upper = -0.50),  # tukey_test_output.txt:845
    "Basic Secondary - University"                = list(diff = -0.843, se = 0.069, p = "<.001", ci_lower = -1.02, ci_upper = -0.67),  # tukey_test_output.txt:846
    "Intermediate Secondary - Academic Secondary" = list(diff = -0.153, se = 0.063, p = 0.075, ci_lower = -0.32, ci_upper = 0.01),  # tukey_test_output.txt:848
    "Intermediate Secondary - University"         = list(diff = -0.346, se = 0.072, p = "<.001", ci_lower = -0.53, ci_upper = -0.16),  # tukey_test_output.txt:849
    "Academic Secondary - University"             = list(diff = -0.193, se = 0.072, p = 0.037, ci_lower = -0.38, ci_upper = -0.01)  # tukey_test_output.txt:852
  ),
  income = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -833.47063, se = 63.16256, p = "<.001", ci_lower = -995.8624, ci_upper = -671.0789),  # tukey_test_output.txt:856
    "Basic Secondary - Academic Secondary"        = list(diff = -1465.03997, se = 63.16256, p = "<.001", ci_lower = -1627.4317, ci_upper = -1302.6482),  # tukey_test_output.txt:857
    "Basic Secondary - University"                = list(diff = -2578.13548, se = 72.33288, p = "<.001", ci_lower = -2764.1042, ci_upper = -2392.1668),  # tukey_test_output.txt:858
    "Intermediate Secondary - Academic Secondary" = list(diff = -631.56934, se = 67.60909, p = "<.001", ci_lower = -805.3931, ci_upper = -457.7455),  # tukey_test_output.txt:860
    "Intermediate Secondary - University"         = list(diff = -1744.66485, se = 76.24648, p = "<.001", ci_lower = -1940.6955, ci_upper = -1548.6342),  # tukey_test_output.txt:861
    "Academic Secondary - University"             = list(diff = -1113.09551, se = 76.24648, p = "<.001", ci_lower = -1309.1261, ci_upper = -917.0649)  # tukey_test_output.txt:864
  ),
  age = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -1.07584, se = 0.89465, p = 0.625, ci_lower = -3.3758, ci_upper = 1.2241),  # tukey_test_output.txt:868
    "Basic Secondary - Academic Secondary"        = list(diff = -1.11010, se = 0.89384, p = 0.600, ci_lower = -3.4079, ci_upper = 1.1878),  # tukey_test_output.txt:869
    "Basic Secondary - University"                = list(diff = 0.73808, se = 1.03168, p = 0.891, ci_lower = -1.9141, ci_upper = 3.3903),  # tukey_test_output.txt:870
    "Intermediate Secondary - Academic Secondary" = list(diff = -0.03426, se = 0.95623, p = 1.000, ci_lower = -2.4925, ci_upper = 2.4240),  # tukey_test_output.txt:872
    "Intermediate Secondary - University"         = list(diff = 1.81392, se = 1.08618, p = 0.340, ci_lower = -0.9784, ci_upper = 4.6062),  # tukey_test_output.txt:873
    "Academic Secondary - University"             = list(diff = 1.84818, se = 1.08551, p = 0.322, ci_lower = -0.9424, ci_upper = 4.6388)  # tukey_test_output.txt:876
  ),
  political_orientation = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -0.030, se = 0.060, p = 0.960, ci_lower = -0.18, ci_upper = 0.12),  # tukey_test_output.txt:880
    "Basic Secondary - Academic Secondary"        = list(diff = -0.017, se = 0.060, p = 0.991, ci_lower = -0.17, ci_upper = 0.14),  # tukey_test_output.txt:881
    "Basic Secondary - University"                = list(diff = 0.073, se = 0.069, p = 0.716, ci_lower = -0.10, ci_upper = 0.25),  # tukey_test_output.txt:882
    "Intermediate Secondary - Academic Secondary" = list(diff = 0.012, se = 0.064, p = 0.998, ci_lower = -0.15, ci_upper = 0.18),  # tukey_test_output.txt:884
    "Intermediate Secondary - University"         = list(diff = 0.102, se = 0.072, p = 0.491, ci_lower = -0.08, ci_upper = 0.29),  # tukey_test_output.txt:885
    "Academic Secondary - University"             = list(diff = 0.090, se = 0.072, p = 0.596, ci_lower = -0.10, ci_upper = 0.28)  # tukey_test_output.txt:888
  ),
  environmental_concern = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = 0.051, se = 0.064, p = 0.856, ci_lower = -0.11, ci_upper = 0.22),  # tukey_test_output.txt:892
    "Basic Secondary - Academic Secondary"        = list(diff = 0.073, se = 0.064, p = 0.670, ci_lower = -0.09, ci_upper = 0.24),  # tukey_test_output.txt:893
    "Basic Secondary - University"                = list(diff = -0.033, se = 0.074, p = 0.972, ci_lower = -0.22, ci_upper = 0.16),  # tukey_test_output.txt:894
    "Intermediate Secondary - Academic Secondary" = list(diff = 0.021, se = 0.069, p = 0.989, ci_lower = -0.15, ci_upper = 0.20),  # tukey_test_output.txt:896
    "Intermediate Secondary - University"         = list(diff = -0.084, se = 0.078, p = 0.707, ci_lower = -0.28, ci_upper = 0.12),  # tukey_test_output.txt:897
    "Academic Secondary - University"             = list(diff = -0.105, se = 0.078, p = 0.533, ci_lower = -0.31, ci_upper = 0.10)  # tukey_test_output.txt:900
  )
)

spss_values$test_8a_multi_method <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -0.497, se = 0.059, p = "<.001", ci_lower = -0.65, ci_upper = -0.34),  # tukey_test_output.txt:917
  "Basic Secondary - Academic Secondary"        = list(diff = -0.649, se = 0.060, p = "<.001", ci_lower = -0.80, ci_upper = -0.50),  # tukey_test_output.txt:918
  "Basic Secondary - University"                = list(diff = -0.843, se = 0.069, p = "<.001", ci_lower = -1.02, ci_upper = -0.67),  # tukey_test_output.txt:919
  "Intermediate Secondary - Academic Secondary" = list(diff = -0.153, se = 0.063, p = 0.075, ci_lower = -0.32, ci_upper = 0.01),  # tukey_test_output.txt:921
  "Intermediate Secondary - University"         = list(diff = -0.346, se = 0.072, p = "<.001", ci_lower = -0.53, ci_upper = -0.16),  # tukey_test_output.txt:922
  "Academic Secondary - University"             = list(diff = -0.193, se = 0.072, p = 0.037, ci_lower = -0.38, ci_upper = -0.01)  # tukey_test_output.txt:925
)

spss_values$test_9a_subsets <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -0.497, se = 0.059, p = "<.001", ci_lower = -0.65, ci_upper = -0.34),  # tukey_test_output.txt:979
  "Basic Secondary - Academic Secondary"        = list(diff = -0.649, se = 0.060, p = "<.001", ci_lower = -0.80, ci_upper = -0.50),  # tukey_test_output.txt:980
  "Basic Secondary - University"                = list(diff = -0.843, se = 0.069, p = "<.001", ci_lower = -1.02, ci_upper = -0.67),  # tukey_test_output.txt:981
  "Intermediate Secondary - Academic Secondary" = list(diff = -0.153, se = 0.063, p = 0.075, ci_lower = -0.32, ci_upper = 0.01),  # tukey_test_output.txt:983
  "Intermediate Secondary - University"         = list(diff = -0.346, se = 0.072, p = "<.001", ci_lower = -0.53, ci_upper = -0.16),  # tukey_test_output.txt:984
  "Academic Secondary - University"             = list(diff = -0.193, se = 0.072, p = 0.037, ci_lower = -0.38, ci_upper = -0.01)  # tukey_test_output.txt:987
)

# SPSS decimals of mean difference / SE and of the CI bounds follow the
# dependent variable's print format.
posthoc_decimals <- list(
  life_satisfaction     = c(diff = 3, ci = 2),
  trust_government      = c(diff = 3, ci = 2),
  trust_media           = c(diff = 3, ci = 2),
  trust_science         = c(diff = 3, ci = 2),
  political_orientation = c(diff = 3, ci = 2),
  environmental_concern = c(diff = 3, ci = 2),
  income                = c(diff = 5, ci = 4),
  age                   = c(diff = 5, ci = 4)
)

# Compare one pair table (for `var`), or a list of pair tables keyed by `by`
# ("region" for SPLIT FILE, "Variable" for several dependent variables).
compare_tukey_tables <- function(res, tables, scenario, var = NULL, by = NULL) {
  if (is.null(by)) tables <- stats::setNames(list(tables), var)
  for (key in names(tables)) {
    rows <- if (is.null(by)) res else res[as.character(res[[by]]) == key, , drop = FALSE]
    dec  <- posthoc_decimals[[if (identical(by, "Variable")) key else var]]
    compare_tukey_pairs(rows, tables[[key]],
                        prec_diff = dec[["diff"]], prec_ci = dec[["ci"]],
                        scenario = if (is.null(by)) scenario else paste(scenario, key))
  }
}

test_that("Tests 1b-1e: unweighted Tukey (income, age, trust, employment) — matches SPSS", {
  r1b <- survey_data |> oneway_anova(income, group = education) |> tukey_test()
  compare_tukey_tables(r1b$results, spss_values$test_1b_income, "1b", var = "income")

  r1c <- survey_data |> oneway_anova(age, group = education) |> tukey_test()
  compare_tukey_tables(r1c$results, spss_values$test_1c_age, "1c", var = "age")

  r1d <- survey_data |>
    oneway_anova(trust_government, trust_media, trust_science, group = education) |>
    tukey_test()
  compare_tukey_tables(r1d$results, spss_values$test_1d_trust, "1d", by = "Variable")

  r1e <- survey_data |> oneway_anova(life_satisfaction, group = employment) |> tukey_test()
  compare_tukey_tables(r1e$results, spss_values$test_1e_life_employment, "1e",
                       var = "life_satisfaction")
})

test_that("Tests 2a, 2c-2f: weighted Tukey — matches SPSS", {
  r2a <- survey_data |>
    oneway_anova(life_satisfaction, group = education, weights = sampling_weight) |>
    tukey_test()
  compare_tukey_tables(r2a$results, spss_values$test_2a_life_w, "2a weighted",
                       var = "life_satisfaction")

  r2c <- survey_data |>
    oneway_anova(age, group = education, weights = sampling_weight) |>
    tukey_test()
  compare_tukey_tables(r2c$results, spss_values$test_2c_age_w, "2c weighted", var = "age")

  r2d <- survey_data |>
    oneway_anova(trust_government, trust_media, trust_science, group = education,
                 weights = sampling_weight) |>
    tukey_test()
  compare_tukey_tables(r2d$results, spss_values$test_2d_trust_w, "2d weighted",
                       by = "Variable")

  r2e <- survey_data |>
    oneway_anova(life_satisfaction, group = employment, weights = sampling_weight) |>
    tukey_test()
  compare_tukey_tables(r2e$results, spss_values$test_2e_life_employment_w, "2e weighted",
                       var = "life_satisfaction")

  r2f <- survey_data |>
    oneway_anova(income, group = employment, weights = sampling_weight) |>
    tukey_test()
  compare_tukey_tables(r2f$results, spss_values$test_2f_income_employment_w,
                       "2f weighted", var = "income")
})

test_that("Tests 3a-3d: Tukey split by region — matches SPSS", {
  by_region <- survey_data |> group_by(region)

  r3a <- by_region |> oneway_anova(life_satisfaction, group = education) |> tukey_test()
  compare_tukey_tables(r3a$results, spss_values$test_3a_life_split, "3a",
                       var = "life_satisfaction", by = "region")

  r3b <- by_region |> oneway_anova(income, group = education) |> tukey_test()
  compare_tukey_tables(r3b$results, spss_values$test_3b_income_split, "3b",
                       var = "income", by = "region")

  r3c <- by_region |> oneway_anova(age, group = education) |> tukey_test()
  compare_tukey_tables(r3c$results, spss_values$test_3c_age_split, "3c",
                       var = "age", by = "region")

  r3d <- by_region |> oneway_anova(life_satisfaction, group = employment) |> tukey_test()
  compare_tukey_tables(r3d$results, spss_values$test_3d_life_employment_split, "3d",
                       var = "life_satisfaction", by = "region")
})

test_that("Tests 4a (West), 4b-4d: weighted Tukey split by region — matches SPSS", {
  by_region <- survey_data |> group_by(region)

  r4a <- by_region |>
    oneway_anova(life_satisfaction, group = education, weights = sampling_weight) |>
    tukey_test()
  compare_tukey_tables(r4a$results[r4a$results$region == "West", , drop = FALSE],
                       spss_values$test_4a_west_w, "4a West weighted",
                       var = "life_satisfaction")

  r4b <- by_region |>
    oneway_anova(income, group = education, weights = sampling_weight) |>
    tukey_test()
  compare_tukey_tables(r4b$results, spss_values$test_4b_income_split_w, "4b weighted",
                       var = "income", by = "region")

  r4c <- by_region |>
    oneway_anova(age, group = education, weights = sampling_weight) |>
    tukey_test()
  compare_tukey_tables(r4c$results, spss_values$test_4c_age_split_w, "4c weighted",
                       var = "age", by = "region")

  r4d <- by_region |>
    oneway_anova(life_satisfaction, group = employment, weights = sampling_weight) |>
    tukey_test()
  compare_tukey_tables(r4d$results, spss_values$test_4d_life_employment_split_w,
                       "4d weighted", var = "life_satisfaction", by = "region")
})

test_that("Tests 5a/5b: POSTHOC ALPHA(.01)/(.10) = tukey_test(conf.level = .99/.90) — matches SPSS", {
  # /POSTHOC=TUKEY ALPHA(.01) /CRITERIA=CILEVEL(.99) prints a 99% interval;
  # ALPHA(.10) with CILEVEL(.90) a 90% interval
  r5a <- survey_data |>
    oneway_anova(life_satisfaction, group = education, conf.level = 0.99) |>
    tukey_test(conf.level = 0.99)
  compare_tukey_tables(r5a$results, spss_values$test_5a_alpha01, "5a 99%",
                       var = "life_satisfaction")

  r5b <- survey_data |>
    oneway_anova(life_satisfaction, group = education, conf.level = 0.90) |>
    tukey_test(conf.level = 0.90)
  compare_tukey_tables(r5b$results, spss_values$test_5b_alpha10, "5b 90%",
                       var = "life_satisfaction")
})

test_that("Test 6b: ANOVA conf.level = .99 keeps the 95% Tukey table — matches SPSS", {
  # /POSTHOC ALPHA(.05) with /CRITERIA=CILEVEL(.99): the post-hoc interval
  # stays 95%
  r6b <- survey_data |>
    oneway_anova(life_satisfaction, group = education, conf.level = 0.99) |>
    tukey_test()
  compare_tukey_tables(r6b$results, spss_values$test_6b_cilevel99, "6b",
                       var = "life_satisfaction")
})

test_that("Test 7: Tukey for five variables at once — matches SPSS", {
  r7 <- survey_data |>
    oneway_anova(life_satisfaction, income, age, political_orientation,
                 environmental_concern, group = education) |>
    tukey_test()
  compare_tukey_tables(r7$results, spss_values$test_7_multi, "7", by = "Variable")
})

test_that("Tests 8a/9a: Tukey block of the multi-method and homogeneous-subsets runs — matches SPSS", {
  # Both repeat the ONEWAY of Test 1a. 8a requested TUKEY BONFERRONI SCHEFFE
  # LSD; only its Tukey HSD block has a tukey_test() counterpart.
  r <- survey_data |> oneway_anova(life_satisfaction, group = education) |> tukey_test()
  compare_tukey_tables(r$results, spss_values$test_8a_multi_method, "8a Tukey block",
                       var = "life_satisfaction")
  compare_tukey_tables(r$results, spss_values$test_9a_subsets, "9a",
                       var = "life_satisfaction")
})
