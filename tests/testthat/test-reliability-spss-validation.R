# ============================================================================
# RELIABILITY FUNCTION - SPSS VALIDATION TEST
# ============================================================================
# Purpose: Validate R reliability() function against SPSS RELIABILITY procedure
# Dataset: survey_data
# Variables: trust_government, trust_media, trust_science (Trust Scale)
#            life_satisfaction, environmental_concern, political_orientation (Attitude Scale)
# Created: 2026-03-01
# SPSS Version: 29.0.0.0
#
# This validates reliability analysis output against SPSS across 4 scenarios:
# 1. Unweighted/Ungrouped (Tests 1a, 1b, 1c)
# 2. Weighted/Ungrouped (Tests 2a, 2b, 2c)
# 3. Unweighted/Grouped by region (Tests 3a, 3b)
# 4. Weighted/Grouped by region (Tests 4a, 4b)
#
# Statistics validated:
# - Cronbach's Alpha (unstandardized and standardized)
# - Item Statistics (Mean, SD, N)
# - Inter-Item Correlations
# - Corrected Item-Total Correlation
# - Alpha if Item Deleted
#
# Tolerances:
# SPSS displays alpha, correlations, and item-total statistics to 3 decimal
# places. This introduces a display rounding error of up to +/-0.0005.
# We use ABSOLUTE tolerance of +/-0.001 for these values (via expect_spss3)
# to account for both SPSS display rounding and minor computational differences.
#
# - Alpha / Correlations / Item-Total: +/-0.001 absolute (expect_spss3)
# - Means: +/-0.01 relative
# - Standard Deviations: +/-0.01 relative
# - Variances: +/-0.01 relative
# - N (unweighted): exact match
# - N (weighted): +/-1 relative
# ============================================================================

library(testthat)
library(dplyr)
library(mariposa)

# Helper: absolute tolerance check for SPSS 3-decimal display values.
# expect_equal uses relative tolerance (via waldo), which breaks for small
# values like 0.014 where SPSS rounding of +/-0.0005 is a large relative error.
expect_spss3 <- function(actual, expected, abs_tol = 0.001) {
  diff <- abs(actual - expected)
  expect_true(
    diff <= abs_tol,
    label = sprintf("%.6f (expected %.3f, |diff| = %.6f, tol = %.3f)",
                    actual, expected, diff, abs_tol)
  )
}

# ============================================================================
# TEST 1: UNWEIGHTED / UNGROUPED
# ============================================================================

test_that("Test 1a: Trust Scale - unweighted, ungrouped", {
  data(survey_data)
  r <- reliability(survey_data, trust_government, trust_media, trust_science)

  # Reliability Statistics (SPSS: Test 1a)
  expect_spss3(r$alpha, 0.047)
  expect_spss3(r$alpha_standardized, 0.048)
  expect_equal(r$n_items, 3)
  expect_equal(r$n, 2135)

  # Item Statistics
  expect_equal(r$item_statistics$mean[1], 2.62, tolerance = 0.01)  # trust_government
  expect_equal(r$item_statistics$mean[2], 2.43, tolerance = 0.01)  # trust_media
  expect_equal(r$item_statistics$mean[3], 3.62, tolerance = 0.01)  # trust_science
  expect_equal(r$item_statistics$sd[1], 1.162, tolerance = 0.01)
  expect_equal(r$item_statistics$sd[2], 1.156, tolerance = 0.01)
  expect_equal(r$item_statistics$sd[3], 1.034, tolerance = 0.01)

  # Inter-Item Correlations
  expect_spss3(r$inter_item_cor["trust_government", "trust_media"], 0.014)
  expect_spss3(r$inter_item_cor["trust_government", "trust_science"], 0.020)
  expect_spss3(r$inter_item_cor["trust_media", "trust_science"], 0.015)

  # Item-Total Statistics
  expect_equal(r$item_total$scale_mean_if_deleted[1], 6.05, tolerance = 0.01)  # trust_gov
  expect_equal(r$item_total$scale_mean_if_deleted[2], 6.25, tolerance = 0.01)  # trust_media
  expect_equal(r$item_total$scale_mean_if_deleted[3], 5.05, tolerance = 0.01)  # trust_science
  expect_equal(r$item_total$scale_var_if_deleted[1], 2.440, tolerance = 0.01)
  expect_equal(r$item_total$scale_var_if_deleted[2], 2.467, tolerance = 0.01)
  expect_equal(r$item_total$scale_var_if_deleted[3], 2.723, tolerance = 0.01)
  expect_spss3(r$item_total$corrected_item_total_r[1], 0.024)
  expect_spss3(r$item_total$corrected_item_total_r[2], 0.020)
  expect_spss3(r$item_total$corrected_item_total_r[3], 0.025)
  expect_spss3(r$item_total$alpha_if_deleted[1], 0.029)
  expect_spss3(r$item_total$alpha_if_deleted[2], 0.040)
  expect_spss3(r$item_total$alpha_if_deleted[3], 0.027)
})

test_that("Test 1b: Attitude Scale - unweighted, ungrouped", {
  data(survey_data)
  r <- reliability(survey_data, life_satisfaction, environmental_concern,
                   political_orientation)

  # Reliability Statistics (SPSS: Test 1b - negative alpha)
  expect_spss3(r$alpha, -0.929)
  expect_spss3(r$alpha_standardized, -0.958)
  expect_equal(r$n_items, 3)
  expect_equal(r$n, 2139)

  # Item Statistics
  expect_equal(r$item_statistics$mean[1], 3.62, tolerance = 0.01)  # life_satisfaction
  expect_equal(r$item_statistics$mean[2], 3.58, tolerance = 0.01)  # environmental_concern
  expect_equal(r$item_statistics$mean[3], 2.72, tolerance = 0.01)  # political_orientation

  # Inter-Item Correlations
  expect_spss3(r$inter_item_cor["life_satisfaction", "environmental_concern"], 0.002)
  expect_spss3(r$inter_item_cor["environmental_concern", "political_orientation"], -0.588)

  # Item-Total Statistics
  expect_spss3(r$item_total$corrected_item_total_r[1], 0.004)    # life_satisfaction
  expect_spss3(r$item_total$corrected_item_total_r[2], -0.399)   # env_concern
  expect_spss3(r$item_total$corrected_item_total_r[3], -0.419)   # pol_orient
})

test_that("Test 1c: Full Scale (6 items) - unweighted, ungrouped", {
  data(survey_data)
  r <- reliability(survey_data, trust_government, trust_media, trust_science,
                   life_satisfaction, environmental_concern, political_orientation)

  # Reliability Statistics (SPSS: Test 1c)
  expect_spss3(r$alpha, -0.208)
  expect_spss3(r$alpha_standardized, -0.204)
  expect_equal(r$n_items, 6)
  expect_equal(r$n, 1826)

  # Sample of item statistics
  expect_equal(r$item_statistics$mean[1], 2.62, tolerance = 0.01)  # trust_gov
  expect_equal(r$item_statistics$sd[1], 1.162, tolerance = 0.01)

  # Sample of inter-item correlations
  expect_spss3(r$inter_item_cor["environmental_concern", "political_orientation"], -0.594)
})

# ============================================================================
# TEST 2: WEIGHTED / UNGROUPED
# ============================================================================

test_that("Test 2a: Trust Scale - weighted, ungrouped", {
  data(survey_data)
  r <- reliability(survey_data, trust_government, trust_media, trust_science,
                   weights = sampling_weight)

  # Reliability Statistics (SPSS: Test 2a)
  expect_spss3(r$alpha, 0.052)
  expect_spss3(r$alpha_standardized, 0.053)
  expect_equal(r$n_items, 3)
  expect_equal(r$weighted_n, 2150.20, tolerance = 1)

  # Item Statistics
  expect_equal(r$item_statistics$mean[1], 2.62, tolerance = 0.01)
  expect_equal(r$item_statistics$mean[2], 2.43, tolerance = 0.01)
  expect_equal(r$item_statistics$mean[3], 3.62, tolerance = 0.01)
  expect_equal(r$item_statistics$sd[1], 1.162, tolerance = 0.01)
  expect_equal(r$item_statistics$sd[2], 1.158, tolerance = 0.01)
  expect_equal(r$item_statistics$sd[3], 1.033, tolerance = 0.01)

  # Inter-Item Correlations
  expect_spss3(r$inter_item_cor["trust_government", "trust_media"], 0.017)
  expect_spss3(r$inter_item_cor["trust_government", "trust_science"], 0.021)
  expect_spss3(r$inter_item_cor["trust_media", "trust_science"], 0.017)

  # Item-Total Statistics
  expect_equal(r$item_total$scale_mean_if_deleted[1], 6.06, tolerance = 0.01)
  expect_equal(r$item_total$scale_var_if_deleted[1], 2.451, tolerance = 0.01)
  expect_spss3(r$item_total$corrected_item_total_r[1], 0.026)
  expect_spss3(r$item_total$alpha_if_deleted[1], 0.033)
  expect_spss3(r$item_total$corrected_item_total_r[2], 0.023)
  expect_spss3(r$item_total$alpha_if_deleted[2], 0.041)
  expect_spss3(r$item_total$corrected_item_total_r[3], 0.027)
  expect_spss3(r$item_total$alpha_if_deleted[3], 0.033)
})

test_that("Test 2b: Attitude Scale - weighted, ungrouped", {
  data(survey_data)
  r <- reliability(survey_data, life_satisfaction, environmental_concern,
                   political_orientation, weights = sampling_weight)

  # Reliability Statistics (SPSS: Test 2b)
  expect_spss3(r$alpha, -0.929)
  expect_spss3(r$alpha_standardized, -0.957)
  expect_equal(r$n_items, 3)

  # Item Statistics
  expect_equal(r$item_statistics$mean[1], 3.62, tolerance = 0.01)
  expect_equal(r$item_statistics$sd[2], 1.187, tolerance = 0.01)  # env_concern

  # Inter-Item Correlations
  expect_spss3(r$inter_item_cor["life_satisfaction", "political_orientation"], -0.001)
  expect_spss3(r$inter_item_cor["environmental_concern", "political_orientation"], -0.586)
})

test_that("Test 2c: Full Scale - weighted, ungrouped", {
  data(survey_data)
  r <- reliability(survey_data, trust_government, trust_media, trust_science,
                   life_satisfaction, environmental_concern, political_orientation,
                   weights = sampling_weight)

  # Reliability Statistics (SPSS: Test 2c)
  expect_spss3(r$alpha, -0.203)
  expect_spss3(r$alpha_standardized, -0.200)
  expect_equal(r$n_items, 6)

  # Inter-Item Correlations
  expect_spss3(r$inter_item_cor["environmental_concern", "political_orientation"], -0.591)
})

# ============================================================================
# TEST 3: UNWEIGHTED / GROUPED
# ============================================================================

test_that("Test 3a: Trust Scale - unweighted, grouped by region", {
  data(survey_data)
  r <- survey_data %>%
    group_by(region) %>%
    reliability(trust_government, trust_media, trust_science)

  expect_true(r$is_grouped)
  expect_equal(length(r$groups), 2)

  # East group (SPSS: Test 3a, East)
  east <- r$groups[[1]]
  expect_spss3(east$alpha, 0.037)
  expect_spss3(east$alpha_standardized, 0.042)
  expect_equal(east$n, 422)

  # Item-Total Statistics East
  expect_spss3(east$item_total$corrected_item_total_r[1], -0.012)  # trust_gov
  expect_spss3(east$item_total$corrected_item_total_r[2], 0.026)   # trust_media
  expect_spss3(east$item_total$corrected_item_total_r[3], 0.042)   # trust_science

  # West group (SPSS: Test 3a, West)
  west <- r$groups[[2]]
  expect_spss3(west$alpha, 0.050)
  expect_spss3(west$alpha_standardized, 0.050)
  expect_equal(west$n, 1713)

  # Item-Total Statistics West
  expect_spss3(west$item_total$corrected_item_total_r[1], 0.032)
  expect_spss3(west$item_total$corrected_item_total_r[2], 0.019)
  expect_spss3(west$item_total$corrected_item_total_r[3], 0.021)
})

test_that("Test 3b: Attitude Scale - unweighted, grouped by region", {
  data(survey_data)
  r <- survey_data %>%
    group_by(region) %>%
    reliability(life_satisfaction, environmental_concern, political_orientation)

  # East group (SPSS: Test 3b, East)
  east <- r$groups[[1]]
  expect_spss3(east$alpha, -0.855)
  expect_spss3(east$alpha_standardized, -0.934)
  expect_equal(east$n, 411)

  # Inter-Item Correlations East
  expect_spss3(east$inter_item_cor["environmental_concern", "political_orientation"], -0.606)

  # West group (SPSS: Test 3b, West)
  west <- r$groups[[2]]
  expect_spss3(west$alpha, -0.948)
  expect_spss3(west$alpha_standardized, -0.964)
  expect_equal(west$n, 1728)
})

# ============================================================================
# TEST 4: WEIGHTED / GROUPED
# ============================================================================

test_that("Test 4a: Trust Scale - weighted, grouped by region", {
  data(survey_data)
  r <- survey_data %>%
    group_by(region) %>%
    reliability(trust_government, trust_media, trust_science,
                weights = sampling_weight)

  # East group (SPSS: Test 4a, East)
  east <- r$groups[[1]]
  expect_spss3(east$alpha, 0.061)
  expect_spss3(east$alpha_standardized, 0.066)
  expect_equal(east$weighted_n, 443.65, tolerance = 1)

  # Item Statistics East
  expect_equal(east$item_statistics$mean[1], 2.61, tolerance = 0.01)
  expect_equal(east$item_statistics$sd[2], 1.092, tolerance = 0.01)

  # Inter-Item Correlations East
  expect_spss3(east$inter_item_cor["trust_government", "trust_media"], -0.014)
  expect_spss3(east$inter_item_cor["trust_media", "trust_science"], 0.068)

  # Item-Total Statistics East
  expect_spss3(east$item_total$corrected_item_total_r[1], 0.000)
  expect_spss3(east$item_total$corrected_item_total_r[2], 0.034)
  expect_spss3(east$item_total$corrected_item_total_r[3], 0.058)

  # West group (SPSS: Test 4a, West)
  west <- r$groups[[2]]
  expect_spss3(west$alpha, 0.051)
  expect_spss3(west$alpha_standardized, 0.050)
  expect_equal(west$weighted_n, 1706.55, tolerance = 1)

  # Item-Total Statistics West
  expect_spss3(west$item_total$corrected_item_total_r[1], 0.033)
  expect_spss3(west$item_total$corrected_item_total_r[2], 0.021)
  expect_spss3(west$item_total$corrected_item_total_r[3], 0.020)
})

test_that("Test 4b: Attitude Scale - weighted, grouped by region", {
  data(survey_data)
  r <- survey_data %>%
    group_by(region) %>%
    reliability(life_satisfaction, environmental_concern, political_orientation,
                weights = sampling_weight)

  # East group (SPSS: Test 4b, East)
  east <- r$groups[[1]]
  expect_spss3(east$alpha, -0.882)
  expect_spss3(east$alpha_standardized, -0.956)

  # Inter-Item Correlations East
  expect_spss3(east$inter_item_cor["environmental_concern", "political_orientation"], -0.606)

  # Item-Total Statistics East
  expect_spss3(east$item_total$corrected_item_total_r[1], 0.025)
  expect_spss3(east$item_total$corrected_item_total_r[2], -0.394)
  expect_spss3(east$item_total$corrected_item_total_r[3], -0.415)

  # West group (SPSS: Test 4b, West)
  west <- r$groups[[2]]
  expect_spss3(west$alpha, -0.942)
  expect_spss3(west$alpha_standardized, -0.958)

  # Inter-Item Correlations West
  expect_spss3(west$inter_item_cor["environmental_concern", "political_orientation"], -0.580)
})

# ============================================================================
# EDGE CASES AND BASIC FUNCTIONALITY
# ============================================================================

test_that("reliability works with tidyselect helpers", {
  data(survey_data)
  r <- reliability(survey_data, starts_with("trust"))
  expect_equal(r$n_items, 3)
  expect_spss3(r$alpha, 0.047)
})

test_that("reliability errors with fewer than 2 items", {
  data(survey_data)
  expect_error(reliability(survey_data, trust_government))
})

test_that("reliability errors with non-numeric items", {
  data(survey_data)
  expect_error(reliability(survey_data, trust_government, region))
})

test_that("reliability returns correct class", {
  data(survey_data)
  r <- reliability(survey_data, trust_government, trust_media, trust_science)
  expect_s3_class(r, "reliability")
})

test_that("print.reliability runs without error", {
  data(survey_data)
  r <- reliability(survey_data, trust_government, trust_media, trust_science)
  expect_output(print(r))
})

test_that("print.reliability for grouped data runs without error", {
  data(survey_data)
  r <- survey_data %>%
    group_by(region) %>%
    reliability(trust_government, trust_media, trust_science)
  expect_output(print(r))
})
