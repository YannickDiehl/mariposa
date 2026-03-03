# ============================================================================
# FACTORIAL ANOVA - SPSS VALIDATION TESTS
# ============================================================================
# Purpose: Validate R factorial_anova() against SPSS UNIANOVA
# Dataset: survey_data (2,500 respondent synthetic survey)
# DVs: life_satisfaction, income, trust_government
# Factors: gender (2), region (2), education (4)
# Weight: sampling_weight (via /REGWGT for WLS)
# Created: 2026-03-02
# SPSS Version: 29
#
# SPSS syntax: tests/spss_reference/syntax/factorial_anova.sps
# SPSS output: tests/spss_reference/outputs/factorial_anova_output.txt
#
# Test matrix:
#   Tests 1a-1c: Unweighted, two-way designs
#   Tests 2a-2b: Weighted, two-way designs
#   Tests 3a-3b: Unweighted, three-way designs
#   Tests 4a-4b: Weighted, three-way designs
# ============================================================================

library(testthat)
library(dplyr)
library(haven)
library(mariposa)

# ============================================================================
# SPSS REFERENCE VALUES
# ============================================================================

spss <- list()

# --------------------------------------------------------------------------
# Test 1a: life_satisfaction ~ gender * region (unweighted, 2x2)
# --------------------------------------------------------------------------
spss$test_1a <- list(
  n = 2421,
  anova = list(
    corrected_model = list(ss = 3.311, df = 3, ms = 1.104, f = 0.830, p = 0.477, eta = 0.001),
    intercept       = list(ss = 19718.320, df = 1, ms = 19718.320, f = 14828.083, p = 0.000, eta = 0.860),
    gender          = list(ss = 0.006, df = 1, ms = 0.006, f = 0.005, p = 0.946, eta = 0.000),
    region          = list(ss = 0.025, df = 1, ms = 0.025, f = 0.019, p = 0.891, eta = 0.000),
    gender_region   = list(ss = 1.893, df = 1, ms = 1.893, f = 1.424, p = 0.233, eta = 0.001),
    error           = list(ss = 3214.116, df = 2417, ms = 1.330),
    total           = list(ss = 35088.000, df = 2421),
    corrected_total = list(ss = 3217.428, df = 2420)
  ),
  r_squared = 0.001,
  adj_r_squared = 0.000,
  levene = list(f = 1.586, df1 = 3, df2 = 2417, p = 0.191)
)

# --------------------------------------------------------------------------
# Test 1b: income ~ gender * education (unweighted, 2x4)
# --------------------------------------------------------------------------
spss$test_1b <- list(
  n = 2186,
  anova = list(
    corrected_model  = list(ss = 1754652069.848, df = 7, ms = 250664581.407, f = 199.909, p = 0.000, eta = 0.391),
    intercept        = list(ss = 32212607064.511, df = 1, ms = 32212607064.511, f = 25690.075, p = 0.000, eta = 0.922),
    gender           = list(ss = 122637.601, df = 1, ms = 122637.601, f = 0.098, p = 0.755, eta = 0.000),
    education        = list(ss = 1743617622.524, df = 3, ms = 581205874.175, f = 463.521, p = 0.000, eta = 0.390),
    gender_education = list(ss = 1499440.759, df = 3, ms = 499813.586, f = 0.399, p = 0.754, eta = 0.001),
    error            = list(ss = 2730979096.667, df = 2178, ms = 1253893.066),
    total            = list(ss = 35290790000.000, df = 2186),
    corrected_total  = list(ss = 4485631166.515, df = 2185)
  ),
  r_squared = 0.391,
  adj_r_squared = 0.389,
  levene = list(f = 44.988, df1 = 7, df2 = 2178, p = 0.000)
)

# --------------------------------------------------------------------------
# Test 1c: trust_government ~ gender * region (unweighted, 2x2)
# --------------------------------------------------------------------------
spss$test_1c <- list(
  n = 2354,
  anova = list(
    corrected_model = list(ss = 1.086, df = 3, ms = 0.362, f = 0.267, p = 0.849, eta = 0.000),
    intercept       = list(ss = 10112.993, df = 1, ms = 10112.993, f = 7466.042, p = 0.000, eta = 0.761),
    gender          = list(ss = 1.004, df = 1, ms = 1.004, f = 0.741, p = 0.389, eta = 0.000),
    region          = list(ss = 0.091, df = 1, ms = 0.091, f = 0.067, p = 0.796, eta = 0.000),
    gender_region   = list(ss = 0.394, df = 1, ms = 0.394, f = 0.291, p = 0.590, eta = 0.000),
    error           = list(ss = 3183.150, df = 2350, ms = 1.355),
    total           = list(ss = 19351.000, df = 2354),
    corrected_total = list(ss = 3184.237, df = 2353)
  ),
  r_squared = 0.000,
  adj_r_squared = -0.001,
  levene = list(f = 1.241, df1 = 3, df2 = 2350, p = 0.293)
)

# --------------------------------------------------------------------------
# Test 2a: life_satisfaction ~ gender * region (weighted, 2x2)
# --------------------------------------------------------------------------
spss$test_2a <- list(
  n = 2421,
  anova = list(
    corrected_model = list(ss = 3.714, df = 3, ms = 1.238, f = 0.927, p = 0.427, eta = 0.001),
    intercept       = list(ss = 20468.612, df = 1, ms = 20468.612, f = 15319.285, p = 0.000, eta = 0.864),
    gender          = list(ss = 0.010, df = 1, ms = 0.010, f = 0.008, p = 0.930, eta = 0.000),
    region          = list(ss = 0.001, df = 1, ms = 0.001, f = 0.001, p = 0.979, eta = 0.000),
    gender_region   = list(ss = 2.194, df = 1, ms = 2.194, f = 1.642, p = 0.200, eta = 0.001),
    error           = list(ss = 3229.435, df = 2417, ms = 1.336),
    total           = list(ss = 35249.294, df = 2421),
    corrected_total = list(ss = 3233.149, df = 2420)
  ),
  r_squared = 0.001,
  adj_r_squared = 0.000,
  levene = list(f = 2.470, df1 = 3, df2 = 2417, p = 0.060)
)

# --------------------------------------------------------------------------
# Test 2b: income ~ gender * education (weighted, 2x4)
# --------------------------------------------------------------------------
spss$test_2b <- list(
  n = 2186,
  anova = list(
    corrected_model  = list(ss = 1727817296.803, df = 7, ms = 246831042.400, f = 196.710, p = 0.000, eta = 0.387),
    intercept        = list(ss = 32084812419.728, df = 1, ms = 32084812419.728, f = 25569.690, p = 0.000, eta = 0.922),
    gender           = list(ss = 143986.970, df = 1, ms = 143986.970, f = 0.115, p = 0.735, eta = 0.000),
    education        = list(ss = 1715942242.479, df = 3, ms = 571980747.493, f = 455.835, p = 0.000, eta = 0.386),
    gender_education = list(ss = 1130110.998, df = 3, ms = 376703.666, f = 0.300, p = 0.825, eta = 0.000),
    error            = list(ss = 2732951471.494, df = 2178, ms = 1254798.655),
    total            = list(ss = 35297675826.553, df = 2186),
    corrected_total  = list(ss = 4460768768.296, df = 2185)
  ),
  r_squared = 0.387,
  adj_r_squared = 0.385,
  levene = list(f = 41.244, df1 = 7, df2 = 2178, p = 0.000)
)

# --------------------------------------------------------------------------
# Test 3a: life_satisfaction ~ gender * region * education (unweighted, 2x2x4)
# --------------------------------------------------------------------------
spss$test_3a <- list(
  n = 2421,
  anova = list(
    corrected_model          = list(ss = 258.289, df = 15, ms = 17.219, f = 13.995, p = 0.000, eta = 0.080),
    intercept                = list(ss = 19011.236, df = 1, ms = 19011.236, f = 15451.124, p = 0.000, eta = 0.865),
    gender                   = list(ss = 0.082, df = 1, ms = 0.082, f = 0.067, p = 0.796, eta = 0.000),
    region                   = list(ss = 0.132, df = 1, ms = 0.132, f = 0.107, p = 0.744, eta = 0.000),
    education                = list(ss = 128.816, df = 3, ms = 42.939, f = 34.898, p = 0.000, eta = 0.042),
    gender_region            = list(ss = 2.351, df = 1, ms = 2.351, f = 1.911, p = 0.167, eta = 0.001),
    gender_education         = list(ss = 0.277, df = 3, ms = 0.092, f = 0.075, p = 0.973, eta = 0.000),
    region_education         = list(ss = 3.326, df = 3, ms = 1.109, f = 0.901, p = 0.440, eta = 0.001),
    gender_region_education  = list(ss = 1.655, df = 3, ms = 0.552, f = 0.448, p = 0.719, eta = 0.001),
    error                    = list(ss = 2959.139, df = 2405, ms = 1.230),
    total                    = list(ss = 35088.000, df = 2421),
    corrected_total          = list(ss = 3217.428, df = 2420)
  ),
  r_squared = 0.080,
  adj_r_squared = 0.075,
  levene = list(f = 7.061, df1 = 15, df2 = 2405, p = 0.000)
)

# --------------------------------------------------------------------------
# Test 3b: income ~ gender * region * education (unweighted, 2x2x4)
# --------------------------------------------------------------------------
spss$test_3b <- list(
  n = 2186,
  anova = list(
    corrected_model          = list(ss = 1777233472.651, df = 15, ms = 118482231.510, f = 94.929, p = 0.000, eta = 0.396),
    intercept                = list(ss = 20213547861.158, df = 1, ms = 20213547861.158, f = 16195.332, p = 0.000, eta = 0.882),
    gender                   = list(ss = 3714208.739, df = 1, ms = 3714208.739, f = 2.976, p = 0.085, eta = 0.001),
    region                   = list(ss = 70450.166, df = 1, ms = 70450.166, f = 0.056, p = 0.812, eta = 0.000),
    education                = list(ss = 1045824300.441, df = 3, ms = 348608100.147, f = 279.309, p = 0.000, eta = 0.279),
    gender_region            = list(ss = 7200424.027, df = 1, ms = 7200424.027, f = 5.769, p = 0.016, eta = 0.003),
    gender_education         = list(ss = 2235577.666, df = 3, ms = 745192.555, f = 0.597, p = 0.617, eta = 0.001),
    region_education         = list(ss = 3707480.291, df = 3, ms = 1235826.764, f = 0.990, p = 0.396, eta = 0.001),
    gender_region_education  = list(ss = 14559992.317, df = 3, ms = 4853330.772, f = 3.889, p = 0.009, eta = 0.005),
    error                    = list(ss = 2708397693.864, df = 2170, ms = 1248109.536),
    total                    = list(ss = 35290790000.000, df = 2186),
    corrected_total          = list(ss = 4485631166.515, df = 2185)
  ),
  r_squared = 0.396,
  adj_r_squared = 0.392,
  levene = list(f = 21.493, df1 = 15, df2 = 2170, p = 0.000)
)

# --------------------------------------------------------------------------
# Test 4a: life_satisfaction ~ gender * region * education (weighted, 2x2x4)
# --------------------------------------------------------------------------
spss$test_4a <- list(
  n = 2421,
  anova = list(
    corrected_model          = list(ss = 253.114, df = 15, ms = 16.874, f = 13.618, p = 0.000, eta = 0.078),
    intercept                = list(ss = 19681.194, df = 1, ms = 19681.194, f = 15883.460, p = 0.000, eta = 0.868),
    gender                   = list(ss = 0.119, df = 1, ms = 0.119, f = 0.096, p = 0.756, eta = 0.000),
    region                   = list(ss = 0.093, df = 1, ms = 0.093, f = 0.075, p = 0.784, eta = 0.000),
    education                = list(ss = 128.176, df = 3, ms = 42.725, f = 34.481, p = 0.000, eta = 0.041),
    gender_region            = list(ss = 2.742, df = 1, ms = 2.742, f = 2.213, p = 0.137, eta = 0.001),
    gender_education         = list(ss = 0.339, df = 3, ms = 0.113, f = 0.091, p = 0.965, eta = 0.000),
    region_education         = list(ss = 3.487, df = 3, ms = 1.162, f = 0.938, p = 0.421, eta = 0.001),
    gender_region_education  = list(ss = 2.204, df = 3, ms = 0.735, f = 0.593, p = 0.620, eta = 0.001),
    error                    = list(ss = 2980.035, df = 2405, ms = 1.239),
    total                    = list(ss = 35249.294, df = 2421),
    corrected_total          = list(ss = 3233.149, df = 2420)
  ),
  r_squared = 0.078,
  adj_r_squared = 0.073,
  levene = list(f = 7.501, df1 = 15, df2 = 2405, p = 0.000)
)

# --------------------------------------------------------------------------
# Test 4b: income ~ gender * region * education (weighted, 2x2x4)
# --------------------------------------------------------------------------
spss$test_4b <- list(
  n = 2186,
  anova = list(
    corrected_model          = list(ss = 1753620077.975, df = 15, ms = 116908005.198, f = 93.711, p = 0.000, eta = 0.393),
    intercept                = list(ss = 20931958886.310, df = 1, ms = 20931958886.310, f = 16778.669, p = 0.000, eta = 0.885),
    gender                   = list(ss = 4146338.127, df = 1, ms = 4146338.127, f = 3.324, p = 0.068, eta = 0.002),
    region                   = list(ss = 132753.279, df = 1, ms = 132753.279, f = 0.106, p = 0.744, eta = 0.000),
    education                = list(ss = 1063719443.034, df = 3, ms = 354573147.678, f = 284.219, p = 0.000, eta = 0.282),
    gender_region            = list(ss = 8675213.525, df = 1, ms = 8675213.525, f = 6.954, p = 0.008, eta = 0.003),
    gender_education         = list(ss = 2623899.810, df = 3, ms = 874633.270, f = 0.701, p = 0.551, eta = 0.001),
    region_education         = list(ss = 4005416.106, df = 3, ms = 1335138.702, f = 1.070, p = 0.361, eta = 0.001),
    gender_region_education  = list(ss = 16748612.574, df = 3, ms = 5582870.858, f = 4.475, p = 0.004, eta = 0.006),
    error                    = list(ss = 2707148690.321, df = 2170, ms = 1247533.959),
    total                    = list(ss = 35297675826.553, df = 2186),
    corrected_total          = list(ss = 4460768768.296, df = 2185)
  ),
  r_squared = 0.393,
  adj_r_squared = 0.389,
  levene = list(f = 19.715, df1 = 15, df2 = 2170, p = 0.000)
)


# ============================================================================
# TOLERANCE DEFINITIONS
# ============================================================================

tol <- list(
  ss       = 0.01,      # Sum of Squares
  ms       = 0.01,      # Mean Square
  f        = 0.001,     # F statistic
  p        = 0.001,     # p-value
  eta      = 0.001,     # Partial Eta Squared
  r_sq     = 0.001,     # R-squared
  df       = 0,         # Degrees of freedom: exact match
  n        = 0,         # Sample size: exact match
  levene_f = 0.001,     # Levene F statistic
  levene_p = 0.001      # Levene p-value
)


# ============================================================================
# HELPER: Validate ANOVA table row
# ============================================================================

check_anova_row <- function(r_table, source_name, spss_ref, tol, label) {
  row <- r_table[r_table$source == source_name, ]
  expect_equal(nrow(row), 1, label = paste(label, "- row exists:", source_name))

  # SPSS displays values rounded to 3 decimal places. We compare rounded values

  # to account for display rounding (avoids relative tolerance issues near zero).
  expect_equal(round(unname(row$ss), 3), spss_ref$ss, tolerance = tol$ss,
               label = paste(label, source_name, "- SS"))
  expect_equal(unname(row$df), spss_ref$df, tolerance = tol$df,
               label = paste(label, source_name, "- df"))

  if (!is.null(spss_ref$ms)) {
    expect_equal(round(unname(row$ms), 3), spss_ref$ms, tolerance = tol$ms,
                 label = paste(label, source_name, "- MS"))
  }
  if (!is.null(spss_ref$f)) {
    expect_equal(round(unname(row$f), 3), spss_ref$f, tolerance = tol$f,
                 label = paste(label, source_name, "- F"))
  }
  if (!is.null(spss_ref$p)) {
    expect_equal(round(unname(row$p), 3), spss_ref$p, tolerance = tol$p,
                 label = paste(label, source_name, "- p"))
  }
  if (!is.null(spss_ref$eta)) {
    expect_equal(round(unname(row$partial_eta_sq), 3), spss_ref$eta,
                 tolerance = tol$eta,
                 label = paste(label, source_name, "- Partial Eta Sq"))
  }
}


# ============================================================================
# TEST 1a: Unweighted 2x2 - life_satisfaction ~ gender * region
# ============================================================================

test_that("Test 1a: Unweighted 2x2 ANOVA - life_satisfaction ~ gender * region", {
  result <- factorial_anova(survey_data,
                            dv = life_satisfaction,
                            between = c(gender, region))

  ref <- spss$test_1a
  at <- result$anova_table
  label <- "Test 1a"

  # Sample size
  expect_equal(result$call_info$n_total, ref$n, label = paste(label, "N"))

  # ANOVA table rows
  check_anova_row(at, "Corrected Model", ref$anova$corrected_model, tol, label)
  check_anova_row(at, "Intercept", ref$anova$intercept, tol, label)
  check_anova_row(at, "gender", ref$anova$gender, tol, label)
  check_anova_row(at, "region", ref$anova$region, tol, label)
  check_anova_row(at, "gender * region", ref$anova$gender_region, tol, label)
  check_anova_row(at, "Error", ref$anova$error, tol, label)
  check_anova_row(at, "Total", ref$anova$total, tol, label)
  check_anova_row(at, "Corrected Total", ref$anova$corrected_total, tol, label)

  # R-Squared
  expect_equal(round(unname(result$r_squared["r_squared"]), 3), ref$r_squared,
               tolerance = tol$r_sq, label = paste(label, "R-squared"))
  expect_equal(round(unname(result$r_squared["adj_r_squared"]), 3), ref$adj_r_squared,
               tolerance = tol$r_sq, label = paste(label, "Adj R-squared"))

  # Levene's Test
  expect_equal(round(unname(result$levene_test$f), 3), ref$levene$f,
               tolerance = tol$levene_f, label = paste(label, "Levene F"))
  expect_equal(unname(result$levene_test$df1), ref$levene$df1,
               tolerance = tol$df, label = paste(label, "Levene df1"))
  expect_equal(unname(result$levene_test$df2), ref$levene$df2,
               tolerance = tol$df, label = paste(label, "Levene df2"))
  expect_equal(round(unname(result$levene_test$p), 3), ref$levene$p,
               tolerance = tol$levene_p, label = paste(label, "Levene p"))
})


# ============================================================================
# TEST 1b: Unweighted 2x4 - income ~ gender * education
# ============================================================================

test_that("Test 1b: Unweighted 2x4 ANOVA - income ~ gender * education", {
  result <- factorial_anova(survey_data,
                            dv = income,
                            between = c(gender, education))

  ref <- spss$test_1b
  at <- result$anova_table
  label <- "Test 1b"

  expect_equal(result$call_info$n_total, ref$n, label = paste(label, "N"))

  check_anova_row(at, "Corrected Model", ref$anova$corrected_model, tol, label)
  check_anova_row(at, "Intercept", ref$anova$intercept, tol, label)
  check_anova_row(at, "gender", ref$anova$gender, tol, label)
  check_anova_row(at, "education", ref$anova$education, tol, label)
  check_anova_row(at, "gender * education", ref$anova$gender_education, tol, label)
  check_anova_row(at, "Error", ref$anova$error, tol, label)
  check_anova_row(at, "Total", ref$anova$total, tol, label)
  check_anova_row(at, "Corrected Total", ref$anova$corrected_total, tol, label)

  expect_equal(round(unname(result$r_squared["r_squared"]), 3), ref$r_squared,
               tolerance = tol$r_sq, label = paste(label, "R-squared"))
  expect_equal(round(unname(result$r_squared["adj_r_squared"]), 3), ref$adj_r_squared,
               tolerance = tol$r_sq, label = paste(label, "Adj R-squared"))

  expect_equal(round(unname(result$levene_test$f), 3), ref$levene$f,
               tolerance = tol$levene_f, label = paste(label, "Levene F"))
  expect_equal(unname(result$levene_test$df1), ref$levene$df1,
               tolerance = tol$df, label = paste(label, "Levene df1"))
  expect_equal(unname(result$levene_test$df2), ref$levene$df2,
               tolerance = tol$df, label = paste(label, "Levene df2"))
  expect_equal(round(unname(result$levene_test$p), 3), ref$levene$p,
               tolerance = tol$levene_p, label = paste(label, "Levene p"))
})


# ============================================================================
# TEST 1c: Unweighted 2x2 - trust_government ~ gender * region
# ============================================================================

test_that("Test 1c: Unweighted 2x2 ANOVA - trust_government ~ gender * region", {
  result <- factorial_anova(survey_data,
                            dv = trust_government,
                            between = c(gender, region))

  ref <- spss$test_1c
  at <- result$anova_table
  label <- "Test 1c"

  expect_equal(result$call_info$n_total, ref$n, label = paste(label, "N"))

  check_anova_row(at, "Corrected Model", ref$anova$corrected_model, tol, label)
  check_anova_row(at, "Intercept", ref$anova$intercept, tol, label)
  check_anova_row(at, "gender", ref$anova$gender, tol, label)
  check_anova_row(at, "region", ref$anova$region, tol, label)
  check_anova_row(at, "gender * region", ref$anova$gender_region, tol, label)
  check_anova_row(at, "Error", ref$anova$error, tol, label)
  check_anova_row(at, "Total", ref$anova$total, tol, label)
  check_anova_row(at, "Corrected Total", ref$anova$corrected_total, tol, label)

  expect_equal(round(unname(result$r_squared["r_squared"]), 3), ref$r_squared,
               tolerance = tol$r_sq, label = paste(label, "R-squared"))
  expect_equal(round(unname(result$r_squared["adj_r_squared"]), 3), ref$adj_r_squared,
               tolerance = tol$r_sq, label = paste(label, "Adj R-squared"))

  expect_equal(round(unname(result$levene_test$f), 3), ref$levene$f,
               tolerance = tol$levene_f, label = paste(label, "Levene F"))
  expect_equal(unname(result$levene_test$df1), ref$levene$df1,
               tolerance = tol$df, label = paste(label, "Levene df1"))
  expect_equal(unname(result$levene_test$df2), ref$levene$df2,
               tolerance = tol$df, label = paste(label, "Levene df2"))
  expect_equal(round(unname(result$levene_test$p), 3), ref$levene$p,
               tolerance = tol$levene_p, label = paste(label, "Levene p"))
})


# ============================================================================
# TEST 2a: Weighted 2x2 - life_satisfaction ~ gender * region
# ============================================================================

test_that("Test 2a: Weighted 2x2 ANOVA - life_satisfaction ~ gender * region", {
  result <- factorial_anova(survey_data,
                            dv = life_satisfaction,
                            between = c(gender, region),
                            weights = sampling_weight)

  ref <- spss$test_2a
  at <- result$anova_table
  label <- "Test 2a"

  expect_equal(result$call_info$n_total, ref$n, label = paste(label, "N"))

  check_anova_row(at, "Corrected Model", ref$anova$corrected_model, tol, label)
  check_anova_row(at, "Intercept", ref$anova$intercept, tol, label)
  check_anova_row(at, "gender", ref$anova$gender, tol, label)
  check_anova_row(at, "region", ref$anova$region, tol, label)
  check_anova_row(at, "gender * region", ref$anova$gender_region, tol, label)
  check_anova_row(at, "Error", ref$anova$error, tol, label)
  check_anova_row(at, "Total", ref$anova$total, tol, label)
  check_anova_row(at, "Corrected Total", ref$anova$corrected_total, tol, label)

  expect_equal(round(unname(result$r_squared["r_squared"]), 3), ref$r_squared,
               tolerance = tol$r_sq, label = paste(label, "R-squared"))
  expect_equal(round(unname(result$r_squared["adj_r_squared"]), 3), ref$adj_r_squared,
               tolerance = tol$r_sq, label = paste(label, "Adj R-squared"))

  expect_equal(round(unname(result$levene_test$f), 3), ref$levene$f,
               tolerance = tol$levene_f, label = paste(label, "Levene F"))
  expect_equal(unname(result$levene_test$df1), ref$levene$df1,
               tolerance = tol$df, label = paste(label, "Levene df1"))
  expect_equal(unname(result$levene_test$df2), ref$levene$df2,
               tolerance = tol$df, label = paste(label, "Levene df2"))
  expect_equal(round(unname(result$levene_test$p), 3), ref$levene$p,
               tolerance = tol$levene_p, label = paste(label, "Levene p"))
})


# ============================================================================
# TEST 2b: Weighted 2x4 - income ~ gender * education
# ============================================================================

test_that("Test 2b: Weighted 2x4 ANOVA - income ~ gender * education", {
  result <- factorial_anova(survey_data,
                            dv = income,
                            between = c(gender, education),
                            weights = sampling_weight)

  ref <- spss$test_2b
  at <- result$anova_table
  label <- "Test 2b"

  expect_equal(result$call_info$n_total, ref$n, label = paste(label, "N"))

  check_anova_row(at, "Corrected Model", ref$anova$corrected_model, tol, label)
  check_anova_row(at, "Intercept", ref$anova$intercept, tol, label)
  check_anova_row(at, "gender", ref$anova$gender, tol, label)
  check_anova_row(at, "education", ref$anova$education, tol, label)
  check_anova_row(at, "gender * education", ref$anova$gender_education, tol, label)
  check_anova_row(at, "Error", ref$anova$error, tol, label)
  check_anova_row(at, "Total", ref$anova$total, tol, label)
  check_anova_row(at, "Corrected Total", ref$anova$corrected_total, tol, label)

  expect_equal(round(unname(result$r_squared["r_squared"]), 3), ref$r_squared,
               tolerance = tol$r_sq, label = paste(label, "R-squared"))
  expect_equal(round(unname(result$r_squared["adj_r_squared"]), 3), ref$adj_r_squared,
               tolerance = tol$r_sq, label = paste(label, "Adj R-squared"))

  expect_equal(round(unname(result$levene_test$f), 3), ref$levene$f,
               tolerance = tol$levene_f, label = paste(label, "Levene F"))
  expect_equal(unname(result$levene_test$df1), ref$levene$df1,
               tolerance = tol$df, label = paste(label, "Levene df1"))
  expect_equal(unname(result$levene_test$df2), ref$levene$df2,
               tolerance = tol$df, label = paste(label, "Levene df2"))
  expect_equal(round(unname(result$levene_test$p), 3), ref$levene$p,
               tolerance = tol$levene_p, label = paste(label, "Levene p"))
})


# ============================================================================
# TEST 3a: Unweighted 2x2x4 - life_satisfaction ~ gender * region * education
# ============================================================================

test_that("Test 3a: Unweighted 3-way ANOVA - life_satisfaction ~ gender * region * education", {
  result <- factorial_anova(survey_data,
                            dv = life_satisfaction,
                            between = c(gender, region, education))

  ref <- spss$test_3a
  at <- result$anova_table
  label <- "Test 3a"

  expect_equal(result$call_info$n_total, ref$n, label = paste(label, "N"))

  check_anova_row(at, "Corrected Model", ref$anova$corrected_model, tol, label)
  check_anova_row(at, "Intercept", ref$anova$intercept, tol, label)
  check_anova_row(at, "gender", ref$anova$gender, tol, label)
  check_anova_row(at, "region", ref$anova$region, tol, label)
  check_anova_row(at, "education", ref$anova$education, tol, label)
  check_anova_row(at, "gender * region", ref$anova$gender_region, tol, label)
  check_anova_row(at, "gender * education", ref$anova$gender_education, tol, label)
  check_anova_row(at, "region * education", ref$anova$region_education, tol, label)
  check_anova_row(at, "gender * region * education", ref$anova$gender_region_education, tol, label)
  check_anova_row(at, "Error", ref$anova$error, tol, label)
  check_anova_row(at, "Total", ref$anova$total, tol, label)
  check_anova_row(at, "Corrected Total", ref$anova$corrected_total, tol, label)

  expect_equal(round(unname(result$r_squared["r_squared"]), 3), ref$r_squared,
               tolerance = tol$r_sq, label = paste(label, "R-squared"))
  expect_equal(round(unname(result$r_squared["adj_r_squared"]), 3), ref$adj_r_squared,
               tolerance = tol$r_sq, label = paste(label, "Adj R-squared"))

  expect_equal(round(unname(result$levene_test$f), 3), ref$levene$f,
               tolerance = tol$levene_f, label = paste(label, "Levene F"))
  expect_equal(unname(result$levene_test$df1), ref$levene$df1,
               tolerance = tol$df, label = paste(label, "Levene df1"))
  expect_equal(unname(result$levene_test$df2), ref$levene$df2,
               tolerance = tol$df, label = paste(label, "Levene df2"))
  expect_equal(round(unname(result$levene_test$p), 3), ref$levene$p,
               tolerance = tol$levene_p, label = paste(label, "Levene p"))
})


# ============================================================================
# TEST 3b: Unweighted 2x2x4 - income ~ gender * region * education
# ============================================================================

test_that("Test 3b: Unweighted 3-way ANOVA - income ~ gender * region * education", {
  result <- factorial_anova(survey_data,
                            dv = income,
                            between = c(gender, region, education))

  ref <- spss$test_3b
  at <- result$anova_table
  label <- "Test 3b"

  expect_equal(result$call_info$n_total, ref$n, label = paste(label, "N"))

  check_anova_row(at, "Corrected Model", ref$anova$corrected_model, tol, label)
  check_anova_row(at, "Intercept", ref$anova$intercept, tol, label)
  check_anova_row(at, "gender", ref$anova$gender, tol, label)
  check_anova_row(at, "region", ref$anova$region, tol, label)
  check_anova_row(at, "education", ref$anova$education, tol, label)
  check_anova_row(at, "gender * region", ref$anova$gender_region, tol, label)
  check_anova_row(at, "gender * education", ref$anova$gender_education, tol, label)
  check_anova_row(at, "region * education", ref$anova$region_education, tol, label)
  check_anova_row(at, "gender * region * education", ref$anova$gender_region_education, tol, label)
  check_anova_row(at, "Error", ref$anova$error, tol, label)
  check_anova_row(at, "Total", ref$anova$total, tol, label)
  check_anova_row(at, "Corrected Total", ref$anova$corrected_total, tol, label)

  expect_equal(round(unname(result$r_squared["r_squared"]), 3), ref$r_squared,
               tolerance = tol$r_sq, label = paste(label, "R-squared"))
  expect_equal(round(unname(result$r_squared["adj_r_squared"]), 3), ref$adj_r_squared,
               tolerance = tol$r_sq, label = paste(label, "Adj R-squared"))

  expect_equal(round(unname(result$levene_test$f), 3), ref$levene$f,
               tolerance = tol$levene_f, label = paste(label, "Levene F"))
  expect_equal(unname(result$levene_test$df1), ref$levene$df1,
               tolerance = tol$df, label = paste(label, "Levene df1"))
  expect_equal(unname(result$levene_test$df2), ref$levene$df2,
               tolerance = tol$df, label = paste(label, "Levene df2"))
  expect_equal(round(unname(result$levene_test$p), 3), ref$levene$p,
               tolerance = tol$levene_p, label = paste(label, "Levene p"))
})


# ============================================================================
# TEST 4a: Weighted 2x2x4 - life_satisfaction ~ gender * region * education
# ============================================================================

test_that("Test 4a: Weighted 3-way ANOVA - life_satisfaction ~ gender * region * education", {
  result <- factorial_anova(survey_data,
                            dv = life_satisfaction,
                            between = c(gender, region, education),
                            weights = sampling_weight)

  ref <- spss$test_4a
  at <- result$anova_table
  label <- "Test 4a"

  expect_equal(result$call_info$n_total, ref$n, label = paste(label, "N"))

  check_anova_row(at, "Corrected Model", ref$anova$corrected_model, tol, label)
  check_anova_row(at, "Intercept", ref$anova$intercept, tol, label)
  check_anova_row(at, "gender", ref$anova$gender, tol, label)
  check_anova_row(at, "region", ref$anova$region, tol, label)
  check_anova_row(at, "education", ref$anova$education, tol, label)
  check_anova_row(at, "gender * region", ref$anova$gender_region, tol, label)
  check_anova_row(at, "gender * education", ref$anova$gender_education, tol, label)
  check_anova_row(at, "region * education", ref$anova$region_education, tol, label)
  check_anova_row(at, "gender * region * education", ref$anova$gender_region_education, tol, label)
  check_anova_row(at, "Error", ref$anova$error, tol, label)
  check_anova_row(at, "Total", ref$anova$total, tol, label)
  check_anova_row(at, "Corrected Total", ref$anova$corrected_total, tol, label)

  expect_equal(round(unname(result$r_squared["r_squared"]), 3), ref$r_squared,
               tolerance = tol$r_sq, label = paste(label, "R-squared"))
  expect_equal(round(unname(result$r_squared["adj_r_squared"]), 3), ref$adj_r_squared,
               tolerance = tol$r_sq, label = paste(label, "Adj R-squared"))

  expect_equal(round(unname(result$levene_test$f), 3), ref$levene$f,
               tolerance = tol$levene_f, label = paste(label, "Levene F"))
  expect_equal(unname(result$levene_test$df1), ref$levene$df1,
               tolerance = tol$df, label = paste(label, "Levene df1"))
  expect_equal(unname(result$levene_test$df2), ref$levene$df2,
               tolerance = tol$df, label = paste(label, "Levene df2"))
  expect_equal(round(unname(result$levene_test$p), 3), ref$levene$p,
               tolerance = tol$levene_p, label = paste(label, "Levene p"))
})


# ============================================================================
# TEST 4b: Weighted 2x2x4 - income ~ gender * region * education
# ============================================================================

test_that("Test 4b: Weighted 3-way ANOVA - income ~ gender * region * education", {
  result <- factorial_anova(survey_data,
                            dv = income,
                            between = c(gender, region, education),
                            weights = sampling_weight)

  ref <- spss$test_4b
  at <- result$anova_table
  label <- "Test 4b"

  expect_equal(result$call_info$n_total, ref$n, label = paste(label, "N"))

  check_anova_row(at, "Corrected Model", ref$anova$corrected_model, tol, label)
  check_anova_row(at, "Intercept", ref$anova$intercept, tol, label)
  check_anova_row(at, "gender", ref$anova$gender, tol, label)
  check_anova_row(at, "region", ref$anova$region, tol, label)
  check_anova_row(at, "education", ref$anova$education, tol, label)
  check_anova_row(at, "gender * region", ref$anova$gender_region, tol, label)
  check_anova_row(at, "gender * education", ref$anova$gender_education, tol, label)
  check_anova_row(at, "region * education", ref$anova$region_education, tol, label)
  check_anova_row(at, "gender * region * education", ref$anova$gender_region_education, tol, label)
  check_anova_row(at, "Error", ref$anova$error, tol, label)
  check_anova_row(at, "Total", ref$anova$total, tol, label)
  check_anova_row(at, "Corrected Total", ref$anova$corrected_total, tol, label)

  expect_equal(round(unname(result$r_squared["r_squared"]), 3), ref$r_squared,
               tolerance = tol$r_sq, label = paste(label, "R-squared"))
  expect_equal(round(unname(result$r_squared["adj_r_squared"]), 3), ref$adj_r_squared,
               tolerance = tol$r_sq, label = paste(label, "Adj R-squared"))

  expect_equal(round(unname(result$levene_test$f), 3), ref$levene$f,
               tolerance = tol$levene_f, label = paste(label, "Levene F"))
  expect_equal(unname(result$levene_test$df1), ref$levene$df1,
               tolerance = tol$df, label = paste(label, "Levene df1"))
  expect_equal(unname(result$levene_test$df2), ref$levene$df2,
               tolerance = tol$df, label = paste(label, "Levene df2"))
  expect_equal(round(unname(result$levene_test$p), 3), ref$levene$p,
               tolerance = tol$levene_p, label = paste(label, "Levene p"))
})


# ============================================================================
# STRUCTURAL TESTS
# ============================================================================

test_that("factorial_anova returns correct structure", {
  result <- factorial_anova(survey_data,
                            dv = life_satisfaction,
                            between = c(gender, region))

  expect_s3_class(result, "factorial_anova")
  expect_named(result, c("anova_table", "descriptives", "levene_test",
                          "r_squared", "model", "lm_model", "call_info",
                          "data", "variables", "group", "weights",
                          "is_grouped", "groups"))

  # ANOVA table structure
  expect_s3_class(result$anova_table, "tbl_df")
  expect_true(all(c("source", "ss", "df", "ms", "f", "p", "partial_eta_sq")
                   %in% names(result$anova_table)))

  # Descriptives structure
  expect_s3_class(result$descriptives, "tbl_df")
  expect_true(all(c("mean", "sd", "n") %in% names(result$descriptives)))

  # Levene structure
  expect_s3_class(result$levene_test, "tbl_df")
  expect_true(all(c("f", "df1", "df2", "p") %in% names(result$levene_test)))

  # Call info
  expect_equal(result$call_info$dv, "life_satisfaction")
  expect_equal(result$call_info$factors, c("gender", "region"))
  expect_false(result$call_info$weighted)
  expect_equal(result$call_info$ss_type, 3)
})

test_that("factorial_anova handles weighted analysis metadata", {
  result <- factorial_anova(survey_data,
                            dv = income,
                            between = c(gender, education),
                            weights = sampling_weight)

  expect_true(result$call_info$weighted)
  expect_equal(result$call_info$weight_name, "sampling_weight")
})

test_that("factorial_anova works with 3-way design", {
  result <- factorial_anova(survey_data,
                            dv = life_satisfaction,
                            between = c(gender, region, education))

  expect_equal(length(result$call_info$factors), 3)

  # Should have 7 effect rows + Error + Total + Corrected Total + Corrected Model + Intercept
  at <- result$anova_table
  expected_sources <- c("Corrected Model", "Intercept",
                        "gender", "region", "education",
                        "gender * region", "gender * education",
                        "region * education", "gender * region * education",
                        "Error", "Total", "Corrected Total")
  expect_equal(at$source, expected_sources)
})


# ============================================================================
# INPUT VALIDATION TESTS
# ============================================================================

test_that("factorial_anova rejects invalid inputs", {
  # Non-numeric DV
  expect_error(factorial_anova(survey_data, dv = gender, between = c(region, education)))

  # Only 1 factor
  expect_error(factorial_anova(survey_data, dv = income, between = c(gender)),
               "at least 2")

  # Non-existent variable
  expect_error(factorial_anova(survey_data, dv = nonexistent, between = c(gender, region)))

  # Non-data-frame input
  expect_error(factorial_anova("not_a_df", dv = income, between = c(gender, region)))
})


# ============================================================================
# S3 DISPATCH TESTS
# ============================================================================

test_that("tukey_test works on factorial_anova result", {
  result <- factorial_anova(survey_data,
                            dv = income,
                            between = c(gender, education))

  tukey <- tukey_test(result)
  expect_s3_class(tukey, "tukey_test")
  expect_true(nrow(tukey$results) > 0)

  # Should have comparisons for both factors
  expect_true("gender" %in% tukey$results$Factor)
  expect_true("education" %in% tukey$results$Factor)
})

test_that("scheffe_test works on factorial_anova result", {
  result <- factorial_anova(survey_data,
                            dv = income,
                            between = c(gender, education))

  scheffe <- scheffe_test(result)
  expect_s3_class(scheffe, "scheffe_test")
  expect_true(nrow(scheffe$results) > 0)
})

test_that("levene_test works on factorial_anova result", {
  result <- factorial_anova(survey_data,
                            dv = life_satisfaction,
                            between = c(gender, region))

  lev <- levene_test(result)
  expect_s3_class(lev, "levene_test")
  expect_equal(lev$results$F_statistic, spss$test_1a$levene$f,
               tolerance = tol$levene_f)
})


# ============================================================================
# PRINT METHOD TEST
# ============================================================================

test_that("print.factorial_anova produces output without errors", {
  result <- factorial_anova(survey_data,
                            dv = life_satisfaction,
                            between = c(gender, region))

  expect_output(print(result), "Factorial ANOVA")
  expect_output(print(result), "eta2p")
  # Verbose sections available via summary()
  expect_output(print(summary(result)), "Tests of Between-Subjects Effects")
  expect_output(print(summary(result)), "Descriptive Statistics")
  expect_output(print(summary(result)), "Levene")
})
