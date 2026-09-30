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


# =============================================================================
# REMAINING SPSS SECTIONS — every pair of every table (0.7.4 audit)
# =============================================================================
# Syntax: tests/spss_reference/syntax/scheffe_test.sps. One entry per pair in
# SPSS's first-listed (I)-(J) orientation (I before J in the category order);
# the mirrored (J)-(I) rows repeat the same numbers with flipped signs.
# SPLIT FILE tables are keyed by region, multi-variable tables by variable.
# (The syntax's TITLEs say "Tukey HSD"; every run is /POSTHOC=SCHEFFE.)

spss_values$test_1b_income <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -833.47063, se = 63.16256, p = "<.001", ci_lower = -1010.1785, ci_upper = -656.7628),  # scheffe_test_output.txt:41
  "Basic Secondary - Academic Secondary"        = list(diff = -1465.03997, se = 63.16256, p = "<.001", ci_lower = -1641.7478, ci_upper = -1288.3321),  # scheffe_test_output.txt:42
  "Basic Secondary - University"                = list(diff = -2578.13548, se = 72.33288, p = "<.001", ci_lower = -2780.4988, ci_upper = -2375.7721),  # scheffe_test_output.txt:43
  "Intermediate Secondary - Academic Secondary" = list(diff = -631.56934, se = 67.60909, p = "<.001", ci_lower = -820.7171, ci_upper = -442.4216),  # scheffe_test_output.txt:45
  "Intermediate Secondary - University"         = list(diff = -1744.66485, se = 76.24648, p = "<.001", ci_lower = -1957.9771, ci_upper = -1531.3526),  # scheffe_test_output.txt:46
  "Academic Secondary - University"             = list(diff = -1113.09551, se = 76.24648, p = "<.001", ci_lower = -1326.4078, ci_upper = -899.7832)  # scheffe_test_output.txt:49
)

spss_values$test_1c_age <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -1.07584, se = 0.89465, p = 0.695, ci_lower = -3.5785, ci_upper = 1.4268),  # scheffe_test_output.txt:67
  "Basic Secondary - Academic Secondary"        = list(diff = -1.11010, se = 0.89384, p = 0.673, ci_lower = -3.6105, ci_upper = 1.3903),  # scheffe_test_output.txt:68
  "Basic Secondary - University"                = list(diff = 0.73808, se = 1.03168, p = 0.916, ci_lower = -2.1479, ci_upper = 3.6241),  # scheffe_test_output.txt:69
  "Intermediate Secondary - Academic Secondary" = list(diff = -0.03426, se = 0.95623, p = 1.000, ci_lower = -2.7092, ci_upper = 2.6407),  # scheffe_test_output.txt:71
  "Intermediate Secondary - University"         = list(diff = 1.81392, se = 1.08618, p = 0.425, ci_lower = -1.2246, ci_upper = 4.8524),  # scheffe_test_output.txt:72
  "Academic Secondary - University"             = list(diff = 1.84818, se = 1.08551, p = 0.408, ci_lower = -1.1884, ci_upper = 4.8848)  # scheffe_test_output.txt:75
)

spss_values$test_1d_trust <- list(
  trust_government = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = 0.066, se = 0.063, p = 0.778, ci_lower = -0.11, ci_upper = 0.24),  # scheffe_test_output.txt:91
    "Basic Secondary - Academic Secondary"        = list(diff = 0.037, se = 0.063, p = 0.951, ci_lower = -0.14, ci_upper = 0.21),  # scheffe_test_output.txt:92
    "Basic Secondary - University"                = list(diff = 0.004, se = 0.073, p = 1.000, ci_lower = -0.20, ci_upper = 0.21),  # scheffe_test_output.txt:93
    "Intermediate Secondary - Academic Secondary" = list(diff = -0.029, se = 0.068, p = 0.980, ci_lower = -0.22, ci_upper = 0.16),  # scheffe_test_output.txt:95
    "Intermediate Secondary - University"         = list(diff = -0.063, se = 0.077, p = 0.882, ci_lower = -0.28, ci_upper = 0.15),  # scheffe_test_output.txt:96
    "Academic Secondary - University"             = list(diff = -0.034, se = 0.077, p = 0.979, ci_lower = -0.25, ci_upper = 0.18)  # scheffe_test_output.txt:99
  ),
  trust_media = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -0.057, se = 0.063, p = 0.845, ci_lower = -0.23, ci_upper = 0.12),  # scheffe_test_output.txt:103
    "Basic Secondary - Academic Secondary"        = list(diff = -0.021, se = 0.063, p = 0.990, ci_lower = -0.20, ci_upper = 0.15),  # scheffe_test_output.txt:104
    "Basic Secondary - University"                = list(diff = 0.066, se = 0.073, p = 0.842, ci_lower = -0.14, ci_upper = 0.27),  # scheffe_test_output.txt:105
    "Intermediate Secondary - Academic Secondary" = list(diff = 0.036, se = 0.067, p = 0.963, ci_lower = -0.15, ci_upper = 0.22),  # scheffe_test_output.txt:107
    "Intermediate Secondary - University"         = list(diff = 0.123, se = 0.077, p = 0.459, ci_lower = -0.09, ci_upper = 0.34),  # scheffe_test_output.txt:108
    "Academic Secondary - University"             = list(diff = 0.087, se = 0.076, p = 0.727, ci_lower = -0.13, ci_upper = 0.30)  # scheffe_test_output.txt:111
  ),
  trust_science = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = 0.028, se = 0.055, p = 0.967, ci_lower = -0.13, ci_upper = 0.18),  # scheffe_test_output.txt:115
    "Basic Secondary - Academic Secondary"        = list(diff = -0.049, se = 0.055, p = 0.857, ci_lower = -0.20, ci_upper = 0.11),  # scheffe_test_output.txt:116
    "Basic Secondary - University"                = list(diff = 0.011, se = 0.064, p = 0.999, ci_lower = -0.17, ci_upper = 0.19),  # scheffe_test_output.txt:117
    "Intermediate Secondary - Academic Secondary" = list(diff = -0.077, se = 0.059, p = 0.639, ci_lower = -0.24, ci_upper = 0.09),  # scheffe_test_output.txt:119
    "Intermediate Secondary - University"         = list(diff = -0.018, se = 0.067, p = 0.995, ci_lower = -0.21, ci_upper = 0.17),  # scheffe_test_output.txt:120
    "Academic Secondary - University"             = list(diff = 0.059, se = 0.067, p = 0.856, ci_lower = -0.13, ci_upper = 0.25)  # scheffe_test_output.txt:123
  )
)

spss_values$test_1e_life_employment <- list(
  "Student - Employed"    = list(diff = 0.321, se = 0.136, p = 0.236, ci_lower = -0.10, ci_upper = 0.74),  # scheffe_test_output.txt:140
  "Student - Unemployed"  = list(diff = 0.284, se = 0.159, p = 0.525, ci_lower = -0.21, ci_upper = 0.77),  # scheffe_test_output.txt:141
  "Student - Retired"     = list(diff = 0.389, se = 0.142, p = 0.114, ci_lower = -0.05, ci_upper = 0.83),  # scheffe_test_output.txt:142
  "Student - Other"       = list(diff = 0.232, se = 0.172, p = 0.767, ci_lower = -0.30, ci_upper = 0.76),  # scheffe_test_output.txt:143
  "Employed - Unemployed" = list(diff = -0.037, se = 0.091, p = 0.997, ci_lower = -0.32, ci_upper = 0.24),  # scheffe_test_output.txt:145
  "Employed - Retired"    = list(diff = 0.068, se = 0.059, p = 0.854, ci_lower = -0.11, ci_upper = 0.25),  # scheffe_test_output.txt:146
  "Employed - Other"      = list(diff = -0.088, se = 0.113, p = 0.961, ci_lower = -0.44, ci_upper = 0.26),  # scheffe_test_output.txt:147
  "Unemployed - Retired"  = list(diff = 0.105, se = 0.100, p = 0.894, ci_lower = -0.20, ci_upper = 0.41),  # scheffe_test_output.txt:150
  "Unemployed - Other"    = list(diff = -0.051, se = 0.139, p = 0.998, ci_lower = -0.48, ci_upper = 0.38),  # scheffe_test_output.txt:151
  "Retired - Other"       = list(diff = -0.157, se = 0.120, p = 0.791, ci_lower = -0.53, ci_upper = 0.21)  # scheffe_test_output.txt:155
)

spss_values$test_1f_income_employment <- list(
  "Student - Employed"    = list(diff = 907.63143, se = 180.90363, p = "<.001", ci_lower = 349.9307, ci_upper = 1465.3322),  # scheffe_test_output.txt:173
  "Student - Unemployed"  = list(diff = 987.65360, se = 209.86916, p = "<.001", ci_lower = 340.6562, ci_upper = 1634.6510),  # scheffe_test_output.txt:174
  "Student - Retired"     = list(diff = 886.02319, se = 188.62321, p = "<.001", ci_lower = 304.5241, ci_upper = 1467.5223),  # scheffe_test_output.txt:175
  "Student - Other"       = list(diff = 833.29779, se = 226.68174, p = 0.009, ci_lower = 134.4695, ci_upper = 1532.1261),  # scheffe_test_output.txt:176
  "Employed - Unemployed" = list(diff = 80.02217, se = 119.34373, p = 0.978, ci_lower = -287.8979, ci_upper = 447.9423),  # scheffe_test_output.txt:178
  "Employed - Retired"    = list(diff = -21.60824, se = 76.00378, p = 0.999, ci_lower = -255.9173, ci_upper = 212.7008),  # scheffe_test_output.txt:179
  "Employed - Other"      = list(diff = -74.33364, se = 146.90974, p = 0.992, ci_lower = -527.2359, ci_upper = 378.5687),  # scheffe_test_output.txt:180
  "Unemployed - Retired"  = list(diff = -101.63041, se = 130.74983, p = 0.963, ci_lower = -504.7139, ci_upper = 301.4531),  # scheffe_test_output.txt:183
  "Unemployed - Other"    = list(diff = -154.35581, se = 181.38747, p = 0.948, ci_lower = -713.5482, ci_upper = 404.8365),  # scheffe_test_output.txt:184
  "Retired - Other"       = list(diff = -52.72540, se = 156.31719, p = 0.998, ci_lower = -534.6295, ci_upper = 429.1787)  # scheffe_test_output.txt:188
)

spss_values$test_2a_life_w <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -0.490, se = 0.059, p = "<.001", ci_lower = -0.65, ci_upper = -0.33),  # scheffe_test_output.txt:209
  "Basic Secondary - Academic Secondary"        = list(diff = -0.643, se = 0.059, p = "<.001", ci_lower = -0.81, ci_upper = -0.48),  # scheffe_test_output.txt:210
  "Basic Secondary - University"                = list(diff = -0.832, se = 0.069, p = "<.001", ci_lower = -1.03, ci_upper = -0.64),  # scheffe_test_output.txt:211
  "Intermediate Secondary - Academic Secondary" = list(diff = -0.153, se = 0.063, p = 0.115, ci_lower = -0.33, ci_upper = 0.02),  # scheffe_test_output.txt:213
  "Intermediate Secondary - University"         = list(diff = -0.342, se = 0.072, p = "<.001", ci_lower = -0.55, ci_upper = -0.14),  # scheffe_test_output.txt:214
  "Academic Secondary - University"             = list(diff = -0.189, se = 0.073, p = 0.079, ci_lower = -0.39, ci_upper = 0.01)  # scheffe_test_output.txt:217
)

spss_values$test_2c_age_w <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -0.84360, se = 0.89412, p = 0.828, ci_lower = -3.3448, ci_upper = 1.6576),  # scheffe_test_output.txt:261
  "Basic Secondary - Academic Secondary"        = list(diff = -1.06554, se = 0.89371, p = 0.701, ci_lower = -3.5656, ci_upper = 1.4345),  # scheffe_test_output.txt:262
  "Basic Secondary - University"                = list(diff = 0.64261, se = 1.04965, p = 0.945, ci_lower = -2.2937, ci_upper = 3.5789),  # scheffe_test_output.txt:263
  "Intermediate Secondary - Academic Secondary" = list(diff = -0.22195, se = 0.95392, p = 0.997, ci_lower = -2.8904, ci_upper = 2.4465),  # scheffe_test_output.txt:265
  "Intermediate Secondary - University"         = list(diff = 1.48621, se = 1.10137, p = 0.610, ci_lower = -1.5948, ci_upper = 4.5672),  # scheffe_test_output.txt:266
  "Academic Secondary - University"             = list(diff = 1.70815, se = 1.10104, p = 0.492, ci_lower = -1.3719, ci_upper = 4.7882)  # scheffe_test_output.txt:269
)

spss_values$test_2d_trust_w <- list(
  trust_government = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = 0.063, se = 0.063, p = 0.796, ci_lower = -0.11, ci_upper = 0.24),  # scheffe_test_output.txt:285
    "Basic Secondary - Academic Secondary"        = list(diff = 0.041, se = 0.063, p = 0.936, ci_lower = -0.13, ci_upper = 0.22),  # scheffe_test_output.txt:286
    "Basic Secondary - University"                = list(diff = 0.006, se = 0.074, p = 1.000, ci_lower = -0.20, ci_upper = 0.21),  # scheffe_test_output.txt:287
    "Intermediate Secondary - Academic Secondary" = list(diff = -0.023, se = 0.067, p = 0.990, ci_lower = -0.21, ci_upper = 0.16),  # scheffe_test_output.txt:289
    "Intermediate Secondary - University"         = list(diff = -0.057, se = 0.077, p = 0.909, ci_lower = -0.27, ci_upper = 0.16),  # scheffe_test_output.txt:290
    "Academic Secondary - University"             = list(diff = -0.034, se = 0.077, p = 0.978, ci_lower = -0.25, ci_upper = 0.18)  # scheffe_test_output.txt:293
  ),
  trust_media = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -0.060, se = 0.063, p = 0.824, ci_lower = -0.24, ci_upper = 0.12),  # scheffe_test_output.txt:297
    "Basic Secondary - Academic Secondary"        = list(diff = -0.017, se = 0.063, p = 0.995, ci_lower = -0.19, ci_upper = 0.16),  # scheffe_test_output.txt:298
    "Basic Secondary - University"                = list(diff = 0.057, se = 0.074, p = 0.898, ci_lower = -0.15, ci_upper = 0.26),  # scheffe_test_output.txt:299
    "Intermediate Secondary - Academic Secondary" = list(diff = 0.043, se = 0.067, p = 0.939, ci_lower = -0.14, ci_upper = 0.23),  # scheffe_test_output.txt:301
    "Intermediate Secondary - University"         = list(diff = 0.117, se = 0.077, p = 0.518, ci_lower = -0.10, ci_upper = 0.33),  # scheffe_test_output.txt:302
    "Academic Secondary - University"             = list(diff = 0.074, se = 0.077, p = 0.822, ci_lower = -0.14, ci_upper = 0.29)  # scheffe_test_output.txt:305
  ),
  trust_science = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = 0.028, se = 0.055, p = 0.968, ci_lower = -0.13, ci_upper = 0.18),  # scheffe_test_output.txt:309
    "Basic Secondary - Academic Secondary"        = list(diff = -0.043, se = 0.055, p = 0.894, ci_lower = -0.20, ci_upper = 0.11),  # scheffe_test_output.txt:310
    "Basic Secondary - University"                = list(diff = 0.022, se = 0.064, p = 0.990, ci_lower = -0.16, ci_upper = 0.20),  # scheffe_test_output.txt:311
    "Intermediate Secondary - Academic Secondary" = list(diff = -0.071, se = 0.059, p = 0.693, ci_lower = -0.23, ci_upper = 0.09),  # scheffe_test_output.txt:313
    "Intermediate Secondary - University"         = list(diff = -0.006, se = 0.067, p = 1.000, ci_lower = -0.19, ci_upper = 0.18),  # scheffe_test_output.txt:314
    "Academic Secondary - University"             = list(diff = 0.065, se = 0.068, p = 0.820, ci_lower = -0.12, ci_upper = 0.25)  # scheffe_test_output.txt:317
  )
)

spss_values$test_2e_life_employment_w <- list(
  "Student - Employed"    = list(diff = 0.306, se = 0.135, p = 0.271, ci_lower = -0.11, ci_upper = 0.72),  # scheffe_test_output.txt:334
  "Student - Unemployed"  = list(diff = 0.263, se = 0.157, p = 0.592, ci_lower = -0.22, ci_upper = 0.75),  # scheffe_test_output.txt:335
  "Student - Retired"     = list(diff = 0.377, se = 0.141, p = 0.128, ci_lower = -0.06, ci_upper = 0.81),  # scheffe_test_output.txt:336
  "Student - Other"       = list(diff = 0.209, se = 0.171, p = 0.827, ci_lower = -0.32, ci_upper = 0.73),  # scheffe_test_output.txt:337
  "Employed - Unemployed" = list(diff = -0.043, se = 0.091, p = 0.994, ci_lower = -0.32, ci_upper = 0.24),  # scheffe_test_output.txt:339
  "Employed - Retired"    = list(diff = 0.071, se = 0.058, p = 0.833, ci_lower = -0.11, ci_upper = 0.25),  # scheffe_test_output.txt:340
  "Employed - Other"      = list(diff = -0.098, se = 0.112, p = 0.944, ci_lower = -0.44, ci_upper = 0.25),  # scheffe_test_output.txt:341
  "Unemployed - Retired"  = list(diff = 0.114, se = 0.100, p = 0.859, ci_lower = -0.19, ci_upper = 0.42),  # scheffe_test_output.txt:344
  "Unemployed - Other"    = list(diff = -0.054, se = 0.138, p = 0.997, ci_lower = -0.48, ci_upper = 0.37),  # scheffe_test_output.txt:345
  "Retired - Other"       = list(diff = -0.168, se = 0.120, p = 0.740, ci_lower = -0.54, ci_upper = 0.20)  # scheffe_test_output.txt:349
)

spss_values$test_2f_income_employment_w <- list(
  "Student - Employed"    = list(diff = 939.19226, se = 177.84237, p = "<.001", ci_lower = 390.9320, ci_upper = 1487.4525),  # scheffe_test_output.txt:367
  "Student - Unemployed"  = list(diff = 1014.11142, se = 206.53369, p = "<.001", ci_lower = 377.4003, ci_upper = 1650.8226),  # scheffe_test_output.txt:368
  "Student - Retired"     = list(diff = 906.12176, se = 185.41085, p = "<.001", ci_lower = 334.5290, ci_upper = 1477.7145),  # scheffe_test_output.txt:369
  "Student - Other"       = list(diff = 864.50898, se = 223.50773, p = 0.005, ci_lower = 175.4695, ci_upper = 1553.5484),  # scheffe_test_output.txt:370
  "Employed - Unemployed" = list(diff = 74.91917, se = 117.92950, p = 0.982, ci_lower = -288.6391, ci_upper = 438.4774),  # scheffe_test_output.txt:372
  "Employed - Retired"    = list(diff = -33.07050, se = 75.02255, p = 0.996, ci_lower = -264.3533, ci_upper = 198.2123),  # scheffe_test_output.txt:373
  "Employed - Other"      = list(diff = -74.68328, se = 145.62592, p = 0.992, ci_lower = -523.6253, ci_upper = 374.2587),  # scheffe_test_output.txt:374
  "Unemployed - Retired"  = list(diff = -107.98967, se = 129.06061, p = 0.951, ci_lower = -505.8634, ci_upper = 289.8840),  # scheffe_test_output.txt:377
  "Unemployed - Other"    = list(diff = -149.60244, se = 179.54154, p = 0.952, ci_lower = -703.1010, ci_upper = 403.8961),  # scheffe_test_output.txt:378
  "Retired - Other"       = list(diff = -41.61278, se = 154.77785, p = 0.999, ci_lower = -518.7687, ci_upper = 435.5432)  # scheffe_test_output.txt:382
)

spss_values$test_3a_life_split <- list(
  East = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -0.334, se = 0.143, p = 0.143, ci_lower = -0.74, ci_upper = 0.07),  # scheffe_test_output.txt:403
    "Basic Secondary - Academic Secondary"        = list(diff = -0.532, se = 0.146, p = 0.005, ci_lower = -0.94, ci_upper = -0.12),  # scheffe_test_output.txt:404
    "Basic Secondary - University"                = list(diff = -0.642, se = 0.166, p = 0.002, ci_lower = -1.11, ci_upper = -0.18),  # scheffe_test_output.txt:405
    "Intermediate Secondary - Academic Secondary" = list(diff = -0.198, se = 0.157, p = 0.661, ci_lower = -0.64, ci_upper = 0.24),  # scheffe_test_output.txt:407
    "Intermediate Secondary - University"         = list(diff = -0.308, se = 0.175, p = 0.376, ci_lower = -0.80, ci_upper = 0.18),  # scheffe_test_output.txt:408
    "Academic Secondary - University"             = list(diff = -0.110, se = 0.177, p = 0.943, ci_lower = -0.61, ci_upper = 0.39)  # scheffe_test_output.txt:411
  ),
  West = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -0.536, se = 0.065, p = "<.001", ci_lower = -0.72, ci_upper = -0.35),  # scheffe_test_output.txt:415
    "Basic Secondary - Academic Secondary"        = list(diff = -0.678, se = 0.065, p = "<.001", ci_lower = -0.86, ci_upper = -0.50),  # scheffe_test_output.txt:416
    "Basic Secondary - University"                = list(diff = -0.892, se = 0.075, p = "<.001", ci_lower = -1.10, ci_upper = -0.68),  # scheffe_test_output.txt:417
    "Intermediate Secondary - Academic Secondary" = list(diff = -0.142, se = 0.069, p = 0.240, ci_lower = -0.34, ci_upper = 0.05),  # scheffe_test_output.txt:419
    "Intermediate Secondary - University"         = list(diff = -0.355, se = 0.079, p = "<.001", ci_lower = -0.58, ci_upper = -0.13),  # scheffe_test_output.txt:420
    "Academic Secondary - University"             = list(diff = -0.213, se = 0.079, p = 0.062, ci_lower = -0.43, ci_upper = 0.01)  # scheffe_test_output.txt:423
  )
)

spss_values$test_3b_income_split <- list(
  East = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -711.94320, se = 141.45344, p = "<.001", ci_lower = -1108.9636, ci_upper = -314.9228),  # scheffe_test_output.txt:441
    "Basic Secondary - Academic Secondary"        = list(diff = -1302.59740, se = 145.23536, p = "<.001", ci_lower = -1710.2327, ci_upper = -894.9621),  # scheffe_test_output.txt:442
    "Basic Secondary - University"                = list(diff = -2328.31169, se = 162.01683, p = "<.001", ci_lower = -2783.0479, ci_upper = -1873.5755),  # scheffe_test_output.txt:443
    "Intermediate Secondary - Academic Secondary" = list(diff = -590.65421, se = 157.15113, p = 0.003, ci_lower = -1031.7337, ci_upper = -149.5747),  # scheffe_test_output.txt:445
    "Intermediate Secondary - University"         = list(diff = -1616.36849, se = 172.77910, p = "<.001", ci_lower = -2101.3114, ci_upper = -1131.4256),  # scheffe_test_output.txt:446
    "Academic Secondary - University"             = list(diff = -1025.71429, se = 175.88876, p = "<.001", ci_lower = -1519.3851, ci_upper = -532.0435)  # scheffe_test_output.txt:449
  ),
  West = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -866.06016, se = 70.60782, p = "<.001", ci_lower = -1063.6351, ci_upper = -668.4852),  # scheffe_test_output.txt:453
    "Basic Secondary - Academic Secondary"        = list(diff = -1506.95812, se = 70.20527, p = "<.001", ci_lower = -1703.4067, ci_upper = -1310.5096),  # scheffe_test_output.txt:454
    "Basic Secondary - University"                = list(diff = -2642.18619, se = 80.85058, p = "<.001", ci_lower = -2868.4225, ci_upper = -2415.9499),  # scheffe_test_output.txt:455
    "Intermediate Secondary - Academic Secondary" = list(diff = -640.89796, se = 74.91141, p = "<.001", ci_lower = -850.5152, ci_upper = -431.2807),  # scheffe_test_output.txt:457
    "Intermediate Secondary - University"         = list(diff = -1776.12603, se = 84.96915, p = "<.001", ci_lower = -2013.8869, ci_upper = -1538.3652),  # scheffe_test_output.txt:458
    "Academic Secondary - University"             = list(diff = -1135.22807, se = 84.63494, p = "<.001", ci_lower = -1372.0537, ci_upper = -898.4024)  # scheffe_test_output.txt:461
  )
)

spss_values$test_3c_age_split <- list(
  East = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = 0.04385, se = 2.07229, p = 1.000, ci_lower = -5.7698, ci_upper = 5.8575),  # scheffe_test_output.txt:479
    "Basic Secondary - Academic Secondary"        = list(diff = -3.22097, se = 2.10364, p = 0.505, ci_lower = -9.1225, ci_upper = 2.6806),  # scheffe_test_output.txt:480
    "Basic Secondary - University"                = list(diff = -0.99643, se = 2.37237, p = 0.981, ci_lower = -7.6519, ci_upper = 5.6591),  # scheffe_test_output.txt:481
    "Intermediate Secondary - Academic Secondary" = list(diff = -3.26482, se = 2.26901, p = 0.558, ci_lower = -9.6303, ci_upper = 3.1007),  # scheffe_test_output.txt:483
    "Intermediate Secondary - University"         = list(diff = -1.04028, se = 2.52017, p = 0.982, ci_lower = -8.1104, ci_upper = 6.0298),  # scheffe_test_output.txt:484
    "Academic Secondary - University"             = list(diff = 2.22455, se = 2.54601, p = 0.858, ci_lower = -4.9181, ci_upper = 9.3671)  # scheffe_test_output.txt:487
  ),
  West = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -1.35522, se = 0.99090, p = 0.600, ci_lower = -4.1276, ci_upper = 1.4172),  # scheffe_test_output.txt:491
    "Basic Secondary - Academic Secondary"        = list(diff = -0.66515, se = 0.98652, p = 0.929, ci_lower = -3.4253, ci_upper = 2.0950),  # scheffe_test_output.txt:492
    "Basic Secondary - University"                = list(diff = 1.16087, se = 1.14463, p = 0.794, ci_lower = -2.0416, ci_upper = 4.3634),  # scheffe_test_output.txt:493
    "Intermediate Secondary - Academic Secondary" = list(diff = 0.69008, se = 1.05307, p = 0.934, ci_lower = -2.2563, ci_upper = 3.6364),  # scheffe_test_output.txt:495
    "Intermediate Secondary - University"         = list(diff = 2.51609, se = 1.20247, p = 0.224, ci_lower = -0.8482, ci_upper = 5.8804),  # scheffe_test_output.txt:496
    "Academic Secondary - University"             = list(diff = 1.82602, se = 1.19886, p = 0.509, ci_lower = -1.5282, ci_upper = 5.1802)  # scheffe_test_output.txt:499
  )
)

spss_values$test_3d_life_employment_split <- list(
  East = list(
    "Student - Employed"    = list(diff = 0.075, se = 0.388, p = 1.000, ci_lower = -1.13, ci_upper = 1.28),  # scheffe_test_output.txt:516
    "Student - Unemployed"  = list(diff = 0.067, se = 0.441, p = 1.000, ci_lower = -1.30, ci_upper = 1.43),  # scheffe_test_output.txt:517
    "Student - Retired"     = list(diff = 0.186, se = 0.400, p = 0.995, ci_lower = -1.05, ci_upper = 1.42),  # scheffe_test_output.txt:518
    "Student - Other"       = list(diff = -0.300, se = 0.464, p = 0.981, ci_lower = -1.74, ci_upper = 1.14),  # scheffe_test_output.txt:519
    "Employed - Unemployed" = list(diff = -0.008, se = 0.231, p = 1.000, ci_lower = -0.72, ci_upper = 0.71),  # scheffe_test_output.txt:521
    "Employed - Retired"    = list(diff = 0.111, se = 0.137, p = 0.956, ci_lower = -0.31, ci_upper = 0.53),  # scheffe_test_output.txt:522
    "Employed - Other"      = list(diff = -0.375, se = 0.273, p = 0.757, ci_lower = -1.22, ci_upper = 0.47),  # scheffe_test_output.txt:523
    "Unemployed - Retired"  = list(diff = 0.119, se = 0.250, p = 0.994, ci_lower = -0.65, ci_upper = 0.89),  # scheffe_test_output.txt:526
    "Unemployed - Other"    = list(diff = -0.367, se = 0.344, p = 0.888, ci_lower = -1.43, ci_upper = 0.70),  # scheffe_test_output.txt:527
    "Retired - Other"       = list(diff = -0.486, se = 0.289, p = 0.587, ci_lower = -1.38, ci_upper = 0.41)  # scheffe_test_output.txt:531
  ),
  West = list(
    "Student - Employed"    = list(diff = 0.359, se = 0.145, p = 0.191, ci_lower = -0.09, ci_upper = 0.81),  # scheffe_test_output.txt:536
    "Student - Unemployed"  = list(diff = 0.316, se = 0.170, p = 0.483, ci_lower = -0.21, ci_upper = 0.84),  # scheffe_test_output.txt:537
    "Student - Retired"     = list(diff = 0.416, se = 0.152, p = 0.114, ci_lower = -0.05, ci_upper = 0.88),  # scheffe_test_output.txt:538
    "Student - Other"       = list(diff = 0.336, se = 0.185, p = 0.508, ci_lower = -0.23, ci_upper = 0.91),  # scheffe_test_output.txt:539
    "Employed - Unemployed" = list(diff = -0.043, se = 0.099, p = 0.996, ci_lower = -0.35, ci_upper = 0.26),  # scheffe_test_output.txt:541
    "Employed - Retired"    = list(diff = 0.057, se = 0.065, p = 0.943, ci_lower = -0.14, ci_upper = 0.26),  # scheffe_test_output.txt:542
    "Employed - Other"      = list(diff = -0.022, se = 0.124, p = 1.000, ci_lower = -0.40, ci_upper = 0.36),  # scheffe_test_output.txt:543
    "Unemployed - Retired"  = list(diff = 0.100, se = 0.109, p = 0.934, ci_lower = -0.24, ci_upper = 0.44),  # scheffe_test_output.txt:546
    "Unemployed - Other"    = list(diff = 0.021, se = 0.152, p = 1.000, ci_lower = -0.45, ci_upper = 0.49),  # scheffe_test_output.txt:547
    "Retired - Other"       = list(diff = -0.079, se = 0.132, p = 0.986, ci_lower = -0.49, ci_upper = 0.33)  # scheffe_test_output.txt:551
  )
)

spss_values$test_4a_west_w <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -0.531, se = 0.065, p = "<.001", ci_lower = -0.71, ci_upper = -0.35),  # scheffe_test_output.txt:583
  "Basic Secondary - Academic Secondary"        = list(diff = -0.677, se = 0.065, p = "<.001", ci_lower = -0.86, ci_upper = -0.50),  # scheffe_test_output.txt:584
  "Basic Secondary - University"                = list(diff = -0.882, se = 0.077, p = "<.001", ci_lower = -1.10, ci_upper = -0.67),  # scheffe_test_output.txt:585
  "Intermediate Secondary - Academic Secondary" = list(diff = -0.146, se = 0.069, p = 0.212, ci_lower = -0.34, ci_upper = 0.05),  # scheffe_test_output.txt:587
  "Intermediate Secondary - University"         = list(diff = -0.351, se = 0.080, p = "<.001", ci_lower = -0.57, ci_upper = -0.13),  # scheffe_test_output.txt:588
  "Academic Secondary - University"             = list(diff = -0.205, se = 0.080, p = 0.087, ci_lower = -0.43, ci_upper = 0.02)  # scheffe_test_output.txt:591
)

spss_values$test_4b_income_split_w <- list(
  East = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -726.59776, se = 139.03880, p = "<.001", ci_lower = -1116.7706, ci_upper = -336.4249),  # scheffe_test_output.txt:609
    "Basic Secondary - Academic Secondary"        = list(diff = -1290.74934, se = 142.33512, p = "<.001", ci_lower = -1690.1723, ci_upper = -891.3263),  # scheffe_test_output.txt:610
    "Basic Secondary - University"                = list(diff = -2330.17593, se = 160.43097, p = "<.001", ci_lower = -2780.3798, ci_upper = -1879.9721),  # scheffe_test_output.txt:611
    "Intermediate Secondary - Academic Secondary" = list(diff = -564.15158, se = 153.06541, p = 0.004, ci_lower = -993.6861, ci_upper = -134.6171),  # scheffe_test_output.txt:613
    "Intermediate Secondary - University"         = list(diff = -1603.57817, se = 170.02303, p = "<.001", ci_lower = -2080.6994, ci_upper = -1126.4569),  # scheffe_test_output.txt:614
    "Academic Secondary - University"             = list(diff = -1039.42659, se = 172.72906, p = "<.001", ci_lower = -1524.1415, ci_upper = -554.7116)  # scheffe_test_output.txt:617
  ),
  West = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -859.87622, se = 70.01596, p = "<.001", ci_lower = -1055.7957, ci_upper = -663.9568),  # scheffe_test_output.txt:621
    "Basic Secondary - Academic Secondary"        = list(diff = -1512.29521, se = 69.64200, p = "<.001", ci_lower = -1707.1683, ci_upper = -1317.4222),  # scheffe_test_output.txt:622
    "Basic Secondary - University"                = list(diff = -2637.35703, se = 81.76438, p = "<.001", ci_lower = -2866.1511, ci_upper = -2408.5630),  # scheffe_test_output.txt:623
    "Intermediate Secondary - Academic Secondary" = list(diff = -652.41899, se = 74.22507, p = "<.001", ci_lower = -860.1164, ci_upper = -444.7215),  # scheffe_test_output.txt:625
    "Intermediate Secondary - University"         = list(diff = -1777.48081, se = 85.70161, p = "<.001", ci_lower = -2017.2921, ci_upper = -1537.6696),  # scheffe_test_output.txt:626
    "Academic Secondary - University"             = list(diff = -1125.06183, se = 85.39637, p = "<.001", ci_lower = -1364.0189, ci_upper = -886.1047)  # scheffe_test_output.txt:629
  )
)

spss_values$test_4c_age_split_w <- list(
  East = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = 0.16042, se = 2.04093, p = 1.000, ci_lower = -5.5643, ci_upper = 5.8851),  # scheffe_test_output.txt:647
    "Basic Secondary - Academic Secondary"        = list(diff = -3.57840, se = 2.06856, p = 0.394, ci_lower = -9.3806, ci_upper = 2.2238),  # scheffe_test_output.txt:648
    "Basic Secondary - University"                = list(diff = -1.56890, se = 2.35120, p = 0.931, ci_lower = -8.1639, ci_upper = 5.0261),  # scheffe_test_output.txt:649
    "Intermediate Secondary - Academic Secondary" = list(diff = -3.73882, se = 2.21620, p = 0.417, ci_lower = -9.9551, ci_upper = 2.4775),  # scheffe_test_output.txt:651
    "Intermediate Secondary - University"         = list(diff = -1.72932, se = 2.48208, p = 0.922, ci_lower = -8.6914, ci_upper = 5.2328),  # scheffe_test_output.txt:652
    "Academic Secondary - University"             = list(diff = 2.00950, se = 2.50485, p = 0.886, ci_lower = -5.0164, ci_upper = 9.0354)  # scheffe_test_output.txt:655
  ),
  West = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -1.10520, se = 0.99236, p = 0.743, ci_lower = -3.8817, ci_upper = 1.6713),  # scheffe_test_output.txt:659
    "Basic Secondary - Academic Secondary"        = list(diff = -0.49437, se = 0.98864, p = 0.969, ci_lower = -3.2604, ci_upper = 2.2717),  # scheffe_test_output.txt:660
    "Basic Secondary - University"                = list(diff = 1.25441, se = 1.17076, p = 0.766, ci_lower = -2.0212, ci_upper = 4.5300),  # scheffe_test_output.txt:661
    "Intermediate Secondary - Academic Secondary" = list(diff = 0.61083, se = 1.05415, p = 0.953, ci_lower = -2.3385, ci_upper = 3.5602),  # scheffe_test_output.txt:663
    "Intermediate Secondary - University"         = list(diff = 2.35961, se = 1.22658, p = 0.296, ci_lower = -1.0722, ci_upper = 5.7914),  # scheffe_test_output.txt:664
    "Academic Secondary - University"             = list(diff = 1.74877, se = 1.22357, p = 0.564, ci_lower = -1.6746, ci_upper = 5.1721)  # scheffe_test_output.txt:667
  )
)

spss_values$test_4d_life_employment_split_w <- list(
  East = list(
    "Student - Employed"    = list(diff = 0.010, se = 0.379, p = 1.000, ci_lower = -1.16, ci_upper = 1.18),  # scheffe_test_output.txt:684
    "Student - Unemployed"  = list(diff = -0.010, se = 0.429, p = 1.000, ci_lower = -1.34, ci_upper = 1.32),  # scheffe_test_output.txt:685
    "Student - Retired"     = list(diff = 0.144, se = 0.389, p = 0.998, ci_lower = -1.06, ci_upper = 1.35),  # scheffe_test_output.txt:686
    "Student - Other"       = list(diff = -0.398, se = 0.455, p = 0.943, ci_lower = -1.81, ci_upper = 1.01),  # scheffe_test_output.txt:687
    "Employed - Unemployed" = list(diff = -0.020, se = 0.224, p = 1.000, ci_lower = -0.71, ci_upper = 0.67),  # scheffe_test_output.txt:689
    "Employed - Retired"    = list(diff = 0.134, se = 0.131, p = 0.902, ci_lower = -0.27, ci_upper = 0.54),  # scheffe_test_output.txt:690
    "Employed - Other"      = list(diff = -0.408, se = 0.270, p = 0.684, ci_lower = -1.24, ci_upper = 0.43),  # scheffe_test_output.txt:691
    "Unemployed - Retired"  = list(diff = 0.154, se = 0.241, p = 0.982, ci_lower = -0.59, ci_upper = 0.90),  # scheffe_test_output.txt:694
    "Unemployed - Other"    = list(diff = -0.388, se = 0.337, p = 0.857, ci_lower = -1.43, ci_upper = 0.65),  # scheffe_test_output.txt:695
    "Retired - Other"       = list(diff = -0.542, se = 0.284, p = 0.458, ci_lower = -1.42, ci_upper = 0.34)  # scheffe_test_output.txt:699
  ),
  West = list(
    "Student - Employed"    = list(diff = 0.354, se = 0.144, p = 0.193, ci_lower = -0.09, ci_upper = 0.80),  # scheffe_test_output.txt:704
    "Student - Unemployed"  = list(diff = 0.305, se = 0.168, p = 0.511, ci_lower = -0.21, ci_upper = 0.82),  # scheffe_test_output.txt:705
    "Student - Retired"     = list(diff = 0.407, se = 0.151, p = 0.123, ci_lower = -0.06, ci_upper = 0.87),  # scheffe_test_output.txt:706
    "Student - Other"       = list(diff = 0.329, se = 0.184, p = 0.525, ci_lower = -0.24, ci_upper = 0.90),  # scheffe_test_output.txt:707
    "Employed - Unemployed" = list(diff = -0.049, se = 0.099, p = 0.993, ci_lower = -0.35, ci_upper = 0.26),  # scheffe_test_output.txt:709
    "Employed - Retired"    = list(diff = 0.053, se = 0.065, p = 0.957, ci_lower = -0.15, ci_upper = 0.25),  # scheffe_test_output.txt:710
    "Employed - Other"      = list(diff = -0.025, se = 0.124, p = 1.000, ci_lower = -0.41, ci_upper = 0.36),  # scheffe_test_output.txt:711
    "Unemployed - Retired"  = list(diff = 0.102, se = 0.109, p = 0.929, ci_lower = -0.24, ci_upper = 0.44),  # scheffe_test_output.txt:714
    "Unemployed - Other"    = list(diff = 0.024, se = 0.152, p = 1.000, ci_lower = -0.44, ci_upper = 0.49),  # scheffe_test_output.txt:715
    "Retired - Other"       = list(diff = -0.078, se = 0.132, p = 0.986, ci_lower = -0.49, ci_upper = 0.33)  # scheffe_test_output.txt:719
  )
)

spss_values$test_5a_alpha01 <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -0.497, se = 0.059, p = "<.001", ci_lower = -0.70, ci_upper = -0.30),  # scheffe_test_output.txt:739
  "Basic Secondary - Academic Secondary"        = list(diff = -0.649, se = 0.060, p = "<.001", ci_lower = -0.85, ci_upper = -0.45),  # scheffe_test_output.txt:740
  "Basic Secondary - University"                = list(diff = -0.843, se = 0.069, p = "<.001", ci_lower = -1.07, ci_upper = -0.61),  # scheffe_test_output.txt:741
  "Intermediate Secondary - Academic Secondary" = list(diff = -0.153, se = 0.063, p = 0.121, ci_lower = -0.37, ci_upper = 0.06),  # scheffe_test_output.txt:743
  "Intermediate Secondary - University"         = list(diff = -0.346, se = 0.072, p = "<.001", ci_lower = -0.59, ci_upper = -0.10),  # scheffe_test_output.txt:744
  "Academic Secondary - University"             = list(diff = -0.193, se = 0.072, p = 0.067, ci_lower = -0.44, ci_upper = 0.05)  # scheffe_test_output.txt:747
)

spss_values$test_5b_alpha10 <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -0.497, se = 0.059, p = "<.001", ci_lower = -0.64, ci_upper = -0.35),  # scheffe_test_output.txt:765
  "Basic Secondary - Academic Secondary"        = list(diff = -0.649, se = 0.060, p = "<.001", ci_lower = -0.80, ci_upper = -0.50),  # scheffe_test_output.txt:766
  "Basic Secondary - University"                = list(diff = -0.843, se = 0.069, p = "<.001", ci_lower = -1.01, ci_upper = -0.67),  # scheffe_test_output.txt:767
  "Intermediate Secondary - Academic Secondary" = list(diff = -0.153, se = 0.063, p = 0.121, ci_lower = -0.31, ci_upper = 0.01),  # scheffe_test_output.txt:769
  "Intermediate Secondary - University"         = list(diff = -0.346, se = 0.072, p = "<.001", ci_lower = -0.53, ci_upper = -0.17),  # scheffe_test_output.txt:770
  "Academic Secondary - University"             = list(diff = -0.193, se = 0.072, p = 0.067, ci_lower = -0.37, ci_upper = -0.01)  # scheffe_test_output.txt:773
)

spss_values$test_6b_cilevel99 <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -0.497, se = 0.059, p = "<.001", ci_lower = -0.66, ci_upper = -0.33),  # scheffe_test_output.txt:817
  "Basic Secondary - Academic Secondary"        = list(diff = -0.649, se = 0.060, p = "<.001", ci_lower = -0.82, ci_upper = -0.48),  # scheffe_test_output.txt:818
  "Basic Secondary - University"                = list(diff = -0.843, se = 0.069, p = "<.001", ci_lower = -1.03, ci_upper = -0.65),  # scheffe_test_output.txt:819
  "Intermediate Secondary - Academic Secondary" = list(diff = -0.153, se = 0.063, p = 0.121, ci_lower = -0.33, ci_upper = 0.02),  # scheffe_test_output.txt:821
  "Intermediate Secondary - University"         = list(diff = -0.346, se = 0.072, p = "<.001", ci_lower = -0.55, ci_upper = -0.14),  # scheffe_test_output.txt:822
  "Academic Secondary - University"             = list(diff = -0.193, se = 0.072, p = 0.067, ci_lower = -0.39, ci_upper = 0.01)  # scheffe_test_output.txt:825
)

spss_values$test_7_multi <- list(
  life_satisfaction = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -0.497, se = 0.059, p = "<.001", ci_lower = -0.66, ci_upper = -0.33),  # scheffe_test_output.txt:842
    "Basic Secondary - Academic Secondary"        = list(diff = -0.649, se = 0.060, p = "<.001", ci_lower = -0.82, ci_upper = -0.48),  # scheffe_test_output.txt:843
    "Basic Secondary - University"                = list(diff = -0.843, se = 0.069, p = "<.001", ci_lower = -1.03, ci_upper = -0.65),  # scheffe_test_output.txt:844
    "Intermediate Secondary - Academic Secondary" = list(diff = -0.153, se = 0.063, p = 0.121, ci_lower = -0.33, ci_upper = 0.02),  # scheffe_test_output.txt:846
    "Intermediate Secondary - University"         = list(diff = -0.346, se = 0.072, p = "<.001", ci_lower = -0.55, ci_upper = -0.14),  # scheffe_test_output.txt:847
    "Academic Secondary - University"             = list(diff = -0.193, se = 0.072, p = 0.067, ci_lower = -0.39, ci_upper = 0.01)  # scheffe_test_output.txt:850
  ),
  income = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -833.47063, se = 63.16256, p = "<.001", ci_lower = -1010.1785, ci_upper = -656.7628),  # scheffe_test_output.txt:854
    "Basic Secondary - Academic Secondary"        = list(diff = -1465.03997, se = 63.16256, p = "<.001", ci_lower = -1641.7478, ci_upper = -1288.3321),  # scheffe_test_output.txt:855
    "Basic Secondary - University"                = list(diff = -2578.13548, se = 72.33288, p = "<.001", ci_lower = -2780.4988, ci_upper = -2375.7721),  # scheffe_test_output.txt:856
    "Intermediate Secondary - Academic Secondary" = list(diff = -631.56934, se = 67.60909, p = "<.001", ci_lower = -820.7171, ci_upper = -442.4216),  # scheffe_test_output.txt:858
    "Intermediate Secondary - University"         = list(diff = -1744.66485, se = 76.24648, p = "<.001", ci_lower = -1957.9771, ci_upper = -1531.3526),  # scheffe_test_output.txt:859
    "Academic Secondary - University"             = list(diff = -1113.09551, se = 76.24648, p = "<.001", ci_lower = -1326.4078, ci_upper = -899.7832)  # scheffe_test_output.txt:862
  ),
  age = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -1.07584, se = 0.89465, p = 0.695, ci_lower = -3.5785, ci_upper = 1.4268),  # scheffe_test_output.txt:866
    "Basic Secondary - Academic Secondary"        = list(diff = -1.11010, se = 0.89384, p = 0.673, ci_lower = -3.6105, ci_upper = 1.3903),  # scheffe_test_output.txt:867
    "Basic Secondary - University"                = list(diff = 0.73808, se = 1.03168, p = 0.916, ci_lower = -2.1479, ci_upper = 3.6241),  # scheffe_test_output.txt:868
    "Intermediate Secondary - Academic Secondary" = list(diff = -0.03426, se = 0.95623, p = 1.000, ci_lower = -2.7092, ci_upper = 2.6407),  # scheffe_test_output.txt:870
    "Intermediate Secondary - University"         = list(diff = 1.81392, se = 1.08618, p = 0.425, ci_lower = -1.2246, ci_upper = 4.8524),  # scheffe_test_output.txt:871
    "Academic Secondary - University"             = list(diff = 1.84818, se = 1.08551, p = 0.408, ci_lower = -1.1884, ci_upper = 4.8848)  # scheffe_test_output.txt:874
  ),
  political_orientation = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = -0.030, se = 0.060, p = 0.970, ci_lower = -0.20, ci_upper = 0.14),  # scheffe_test_output.txt:878
    "Basic Secondary - Academic Secondary"        = list(diff = -0.017, se = 0.060, p = 0.993, ci_lower = -0.18, ci_upper = 0.15),  # scheffe_test_output.txt:879
    "Basic Secondary - University"                = list(diff = 0.073, se = 0.069, p = 0.773, ci_lower = -0.12, ci_upper = 0.27),  # scheffe_test_output.txt:880
    "Intermediate Secondary - Academic Secondary" = list(diff = 0.012, se = 0.064, p = 0.998, ci_lower = -0.17, ci_upper = 0.19),  # scheffe_test_output.txt:882
    "Intermediate Secondary - University"         = list(diff = 0.102, se = 0.072, p = 0.573, ci_lower = -0.10, ci_upper = 0.30),  # scheffe_test_output.txt:883
    "Academic Secondary - University"             = list(diff = 0.090, se = 0.072, p = 0.669, ci_lower = -0.11, ci_upper = 0.29)  # scheffe_test_output.txt:886
  ),
  environmental_concern = list(
    "Basic Secondary - Intermediate Secondary"    = list(diff = 0.051, se = 0.064, p = 0.888, ci_lower = -0.13, ci_upper = 0.23),  # scheffe_test_output.txt:890
    "Basic Secondary - Academic Secondary"        = list(diff = 0.073, se = 0.064, p = 0.734, ci_lower = -0.11, ci_upper = 0.25),  # scheffe_test_output.txt:891
    "Basic Secondary - University"                = list(diff = -0.033, se = 0.074, p = 0.979, ci_lower = -0.24, ci_upper = 0.18),  # scheffe_test_output.txt:892
    "Intermediate Secondary - Academic Secondary" = list(diff = 0.021, se = 0.069, p = 0.992, ci_lower = -0.17, ci_upper = 0.21),  # scheffe_test_output.txt:894
    "Intermediate Secondary - University"         = list(diff = -0.084, se = 0.078, p = 0.765, ci_lower = -0.30, ci_upper = 0.13),  # scheffe_test_output.txt:895
    "Academic Secondary - University"             = list(diff = -0.105, se = 0.078, p = 0.612, ci_lower = -0.32, ci_upper = 0.11)  # scheffe_test_output.txt:898
  )
)

spss_values$test_8a_multi_method <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -0.497, se = 0.059, p = "<.001", ci_lower = -0.66, ci_upper = -0.33),  # scheffe_test_output.txt:915
  "Basic Secondary - Academic Secondary"        = list(diff = -0.649, se = 0.060, p = "<.001", ci_lower = -0.82, ci_upper = -0.48),  # scheffe_test_output.txt:916
  "Basic Secondary - University"                = list(diff = -0.843, se = 0.069, p = "<.001", ci_lower = -1.03, ci_upper = -0.65),  # scheffe_test_output.txt:917
  "Intermediate Secondary - Academic Secondary" = list(diff = -0.153, se = 0.063, p = 0.121, ci_lower = -0.33, ci_upper = 0.02),  # scheffe_test_output.txt:919
  "Intermediate Secondary - University"         = list(diff = -0.346, se = 0.072, p = "<.001", ci_lower = -0.55, ci_upper = -0.14),  # scheffe_test_output.txt:920
  "Academic Secondary - University"             = list(diff = -0.193, se = 0.072, p = 0.067, ci_lower = -0.39, ci_upper = 0.01)  # scheffe_test_output.txt:923
)

spss_values$test_9a_subsets <- list(
  "Basic Secondary - Intermediate Secondary"    = list(diff = -0.497, se = 0.059, p = "<.001", ci_lower = -0.66, ci_upper = -0.33),  # scheffe_test_output.txt:965
  "Basic Secondary - Academic Secondary"        = list(diff = -0.649, se = 0.060, p = "<.001", ci_lower = -0.82, ci_upper = -0.48),  # scheffe_test_output.txt:966
  "Basic Secondary - University"                = list(diff = -0.843, se = 0.069, p = "<.001", ci_lower = -1.03, ci_upper = -0.65),  # scheffe_test_output.txt:967
  "Intermediate Secondary - Academic Secondary" = list(diff = -0.153, se = 0.063, p = 0.121, ci_lower = -0.33, ci_upper = 0.02),  # scheffe_test_output.txt:969
  "Intermediate Secondary - University"         = list(diff = -0.346, se = 0.072, p = "<.001", ci_lower = -0.55, ci_upper = -0.14),  # scheffe_test_output.txt:970
  "Academic Secondary - University"             = list(diff = -0.193, se = 0.072, p = 0.067, ci_lower = -0.39, ci_upper = 0.01)  # scheffe_test_output.txt:973
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
compare_scheffe_tables <- function(res, tables, scenario, var = NULL, by = NULL) {
  if (is.null(by)) tables <- stats::setNames(list(tables), var)
  for (key in names(tables)) {
    rows <- if (is.null(by)) res else res[as.character(res[[by]]) == key, , drop = FALSE]
    dec  <- posthoc_decimals[[if (identical(by, "Variable")) key else var]]
    compare_scheffe_pairs(rows, tables[[key]],
                          prec_diff = dec[["diff"]], prec_ci = dec[["ci"]],
                          scenario = if (is.null(by)) scenario else paste(scenario, key))
  }
}

test_that("Tests 1b-1f: unweighted Scheffé (income, age, trust, employment) — matches SPSS", {
  r1b <- survey_data |> oneway_anova(income, group = education) |> scheffe_test()
  compare_scheffe_tables(r1b$results, spss_values$test_1b_income, "1b", var = "income")

  r1c <- survey_data |> oneway_anova(age, group = education) |> scheffe_test()
  compare_scheffe_tables(r1c$results, spss_values$test_1c_age, "1c", var = "age")

  r1d <- survey_data |>
    oneway_anova(trust_government, trust_media, trust_science, group = education) |>
    scheffe_test()
  compare_scheffe_tables(r1d$results, spss_values$test_1d_trust, "1d", by = "Variable")

  r1e <- survey_data |> oneway_anova(life_satisfaction, group = employment) |> scheffe_test()
  compare_scheffe_tables(r1e$results, spss_values$test_1e_life_employment, "1e",
                         var = "life_satisfaction")

  r1f <- survey_data |> oneway_anova(income, group = employment) |> scheffe_test()
  compare_scheffe_tables(r1f$results, spss_values$test_1f_income_employment, "1f",
                         var = "income")
})

test_that("Tests 2a, 2c-2f: weighted Scheffé — matches SPSS", {
  r2a <- survey_data |>
    oneway_anova(life_satisfaction, group = education, weights = sampling_weight) |>
    scheffe_test()
  compare_scheffe_tables(r2a$results, spss_values$test_2a_life_w, "2a weighted",
                         var = "life_satisfaction")

  r2c <- survey_data |>
    oneway_anova(age, group = education, weights = sampling_weight) |>
    scheffe_test()
  compare_scheffe_tables(r2c$results, spss_values$test_2c_age_w, "2c weighted", var = "age")

  r2d <- survey_data |>
    oneway_anova(trust_government, trust_media, trust_science, group = education,
                 weights = sampling_weight) |>
    scheffe_test()
  compare_scheffe_tables(r2d$results, spss_values$test_2d_trust_w, "2d weighted",
                         by = "Variable")

  r2e <- survey_data |>
    oneway_anova(life_satisfaction, group = employment, weights = sampling_weight) |>
    scheffe_test()
  compare_scheffe_tables(r2e$results, spss_values$test_2e_life_employment_w, "2e weighted",
                         var = "life_satisfaction")

  r2f <- survey_data |>
    oneway_anova(income, group = employment, weights = sampling_weight) |>
    scheffe_test()
  compare_scheffe_tables(r2f$results, spss_values$test_2f_income_employment_w,
                         "2f weighted", var = "income")
})

test_that("Tests 3a-3d: Scheffé split by region — matches SPSS", {
  by_region <- survey_data |> group_by(region)

  r3a <- by_region |> oneway_anova(life_satisfaction, group = education) |> scheffe_test()
  compare_scheffe_tables(r3a$results, spss_values$test_3a_life_split, "3a",
                         var = "life_satisfaction", by = "region")

  r3b <- by_region |> oneway_anova(income, group = education) |> scheffe_test()
  compare_scheffe_tables(r3b$results, spss_values$test_3b_income_split, "3b",
                         var = "income", by = "region")

  r3c <- by_region |> oneway_anova(age, group = education) |> scheffe_test()
  compare_scheffe_tables(r3c$results, spss_values$test_3c_age_split, "3c",
                         var = "age", by = "region")

  r3d <- by_region |> oneway_anova(life_satisfaction, group = employment) |> scheffe_test()
  compare_scheffe_tables(r3d$results, spss_values$test_3d_life_employment_split, "3d",
                         var = "life_satisfaction", by = "region")
})

test_that("Tests 4a (West), 4b-4d: weighted Scheffé split by region — matches SPSS", {
  by_region <- survey_data |> group_by(region)

  r4a <- by_region |>
    oneway_anova(life_satisfaction, group = education, weights = sampling_weight) |>
    scheffe_test()
  compare_scheffe_tables(r4a$results[r4a$results$region == "West", , drop = FALSE],
                         spss_values$test_4a_west_w, "4a West weighted",
                         var = "life_satisfaction")

  r4b <- by_region |>
    oneway_anova(income, group = education, weights = sampling_weight) |>
    scheffe_test()
  compare_scheffe_tables(r4b$results, spss_values$test_4b_income_split_w, "4b weighted",
                         var = "income", by = "region")

  r4c <- by_region |>
    oneway_anova(age, group = education, weights = sampling_weight) |>
    scheffe_test()
  compare_scheffe_tables(r4c$results, spss_values$test_4c_age_split_w, "4c weighted",
                         var = "age", by = "region")

  r4d <- by_region |>
    oneway_anova(life_satisfaction, group = employment, weights = sampling_weight) |>
    scheffe_test()
  compare_scheffe_tables(r4d$results, spss_values$test_4d_life_employment_split_w,
                         "4d weighted", var = "life_satisfaction", by = "region")
})

test_that("Tests 5a/5b: POSTHOC ALPHA(.01)/(.10) = scheffe_test(conf.level = .99/.90) — matches SPSS", {
  # /POSTHOC=SCHEFFE ALPHA(.01) /CRITERIA=CILEVEL(.99) prints a 99% interval;
  # ALPHA(.10) with CILEVEL(.90) a 90% interval
  r5a <- survey_data |>
    oneway_anova(life_satisfaction, group = education, conf.level = 0.99) |>
    scheffe_test(conf.level = 0.99)
  compare_scheffe_tables(r5a$results, spss_values$test_5a_alpha01, "5a 99%",
                         var = "life_satisfaction")

  r5b <- survey_data |>
    oneway_anova(life_satisfaction, group = education, conf.level = 0.90) |>
    scheffe_test(conf.level = 0.90)
  compare_scheffe_tables(r5b$results, spss_values$test_5b_alpha10, "5b 90%",
                         var = "life_satisfaction")
})

test_that("Test 6b: ANOVA conf.level = .99 keeps the 95% Scheffé table — matches SPSS", {
  # /POSTHOC ALPHA(.05) with /CRITERIA=CILEVEL(.99): the post-hoc interval
  # stays 95%
  r6b <- survey_data |>
    oneway_anova(life_satisfaction, group = education, conf.level = 0.99) |>
    scheffe_test()
  compare_scheffe_tables(r6b$results, spss_values$test_6b_cilevel99, "6b",
                         var = "life_satisfaction")
})

test_that("Test 7: Scheffé for five variables at once — matches SPSS", {
  r7 <- survey_data |>
    oneway_anova(life_satisfaction, income, age, political_orientation,
                 environmental_concern, group = education) |>
    scheffe_test()
  compare_scheffe_tables(r7$results, spss_values$test_7_multi, "7", by = "Variable")
})

test_that("Tests 8a/9a: Scheffé block of the multi-method and repeated runs — matches SPSS", {
  # Both repeat the ONEWAY of Test 1a. 8a requested SCHEFFE BONFERRONI
  # SCHEFFE LSD; only its Scheffe block has a scheffe_test() counterpart.
  r <- survey_data |> oneway_anova(life_satisfaction, group = education) |> scheffe_test()
  compare_scheffe_tables(r$results, spss_values$test_8a_multi_method, "8a Scheffe block",
                         var = "life_satisfaction")
  compare_scheffe_tables(r$results, spss_values$test_9a_subsets, "9a",
                         var = "life_satisfaction")
})
