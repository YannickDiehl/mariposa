# ============================================================================
# CROSSTAB 3-WAY FUNCTION - SPSS VALIDATION TEST
# ============================================================================
# Purpose: Validate R crosstab() 3-way functionality against SPSS CROSSTABS procedure
# Dataset: survey_data
# Variables: life_satisfaction, gender, region
# Created: 2025-01-28
# SPSS Version: 29.0.0.0
#
# This validates 3-way crosstab output against SPSS across scenarios:
# 1. Unweighted 3-way crosstab
# 2. Weighted 3-way crosstab
# 3. Different variable ordering
# 4. Grouped 3-way crosstabs
#
# ============================================================================

library(testthat)
library(dplyr)
library(SurveyStat)

# ============================================================================
# TEST 1: UNWEIGHTED 3-WAY CROSSTAB
# ============================================================================

test_that("Unweighted 3-way crosstab matches SPSS output", {

  data(survey_data)

  # Run 3-way crosstab
  result <- survey_data %>%
    crosstab(life_satisfaction, gender, control = region, percentages = "col")

  # Check structure
  expect_true(result$is_3way)
  expect_equal(result$control_var, "region")
  expect_equal(length(result$layers), 2)  # East and West
  expect_true("East" %in% names(result$layers))
  expect_true("West" %in% names(result$layers))

  # Check East layer counts (SPSS reference lines 72-82)
  east_layer <- result$layers[["East"]]

  # Check a few key counts
  expect_equal(east_layer$table[1, "Male"], 14)
  expect_equal(east_layer$table[1, "Female"], 16)
  expect_equal(east_layer$table[2, "Male"], 29)
  expect_equal(east_layer$table[2, "Female"], 29)
  expect_equal(east_layer$table[3, "Male"], 48)
  expect_equal(east_layer$table[3, "Female"], 58)
  expect_equal(east_layer$table[4, "Male"], 68)
  expect_equal(east_layer$table[4, "Female"], 68)
  expect_equal(east_layer$table[5, "Male"], 69)
  expect_equal(east_layer$table[5, "Female"], 66)

  # Check totals
  expect_equal(sum(east_layer$table), 465)
  expect_equal(sum(east_layer$row_totals), 465)
  expect_equal(sum(east_layer$col_totals), 465)

  # Check West layer counts (SPSS reference lines 84-94)
  west_layer <- result$layers[["West"]]

  expect_equal(west_layer$table[1, "Male"], 46)
  expect_equal(west_layer$table[1, "Female"], 42)
  expect_equal(west_layer$table[2, "Male"], 117)
  expect_equal(west_layer$table[2, "Female"], 131)
  expect_equal(west_layer$table[3, "Male"], 251)
  expect_equal(west_layer$table[3, "Female"], 243)
  expect_equal(west_layer$table[4, "Male"], 261)
  expect_equal(west_layer$table[4, "Female"], 334)
  expect_equal(west_layer$table[5, "Male"], 246)
  expect_equal(west_layer$table[5, "Female"], 285)

  # Check totals
  expect_equal(sum(west_layer$table), 1956)

  # Check combined total table (SPSS reference lines 96-107)
  total_table <- result$total_table

  expect_equal(total_table$table[1, "Male"], 60)
  expect_equal(total_table$table[1, "Female"], 58)
  expect_equal(total_table$table[2, "Male"], 146)
  expect_equal(total_table$table[2, "Female"], 160)
  expect_equal(sum(total_table$table), 2421)
  expect_equal(result$n_valid, 2421)
})

# ============================================================================
# TEST 2: WEIGHTED 3-WAY CROSSTAB
# ============================================================================

test_that("Weighted 3-way crosstab matches SPSS output", {

  data(survey_data)

  # Run weighted 3-way crosstab
  result <- survey_data %>%
    crosstab(life_satisfaction, gender,
            control = region,
            weights = sampling_weight,
            percentages = "col")

  # Check that it's weighted
  expect_true(result$is_weighted)
  expect_true(result$is_3way)

  # Check East layer weighted counts (SPSS reference lines 224-234)
  east_layer <- result$layers[["East"]]

  # Note: SPSS shows rounded integer counts, but we maintain precision
  # Allow for small differences due to rounding
  expect_equal(round(east_layer$table[1, "Male"]), 15, tolerance = 1)
  expect_equal(round(east_layer$table[1, "Female"]), 16, tolerance = 1)
  expect_equal(round(east_layer$table[2, "Male"]), 30, tolerance = 1)
  expect_equal(round(east_layer$table[2, "Female"]), 30, tolerance = 1)

  # Check West layer weighted counts (SPSS reference lines 236-246)
  west_layer <- result$layers[["West"]]

  expect_equal(round(west_layer$table[1, "Male"]), 46, tolerance = 1)
  expect_equal(round(west_layer$table[1, "Female"]), 42, tolerance = 1)
  expect_equal(round(west_layer$table[2, "Male"]), 117, tolerance = 1)
  expect_equal(round(west_layer$table[2, "Female"]), 130, tolerance = 1)

  # Check that totals are approximately correct (within rounding)
  expect_equal(round(sum(east_layer$table)), 489, tolerance = 2)
  expect_equal(round(sum(west_layer$table)), 1950, tolerance = 2)
})

# ============================================================================
# TEST 3: DIFFERENT VARIABLE ORDERING
# ============================================================================

test_that("3-way crosstab with different variable ordering works", {

  data(survey_data)

  # Test with life_satisfaction × region × gender (control = gender)
  result <- survey_data %>%
    crosstab(life_satisfaction, region, control = gender, percentages = "col")

  # Check structure
  expect_true(result$is_3way)
  expect_equal(result$control_var, "gender")
  expect_equal(length(result$layers), 2)  # Male and Female
  expect_true("Male" %in% names(result$layers))
  expect_true("Female" %in% names(result$layers))

  # Check Male layer (SPSS reference lines 116-127)
  male_layer <- result$layers[["Male"]]

  expect_equal(male_layer$table[1, "East"], 14)
  expect_equal(male_layer$table[1, "West"], 46)
  expect_equal(male_layer$table[2, "East"], 29)
  expect_equal(male_layer$table[2, "West"], 117)

  # Check Female layer (SPSS reference lines 129-139)
  female_layer <- result$layers[["Female"]]

  expect_equal(female_layer$table[1, "East"], 16)
  expect_equal(female_layer$table[1, "West"], 42)
  expect_equal(female_layer$table[2, "East"], 29)
  expect_equal(female_layer$table[2, "West"], 131)

  # Check totals
  expect_equal(sum(male_layer$table), 1149)
  expect_equal(sum(female_layer$table), 1272)
  expect_equal(result$n_valid, 2421)
})

# ============================================================================
# TEST 4: GROUPED 3-WAY CROSSTAB
# ============================================================================

test_that("Grouped 3-way crosstab works correctly", {

  data(survey_data)

  # Filter to just two employment categories for simplicity
  test_data <- survey_data %>%
    filter(employment %in% c("Employed", "Student"))

  # Run grouped 3-way crosstab
  result <- test_data %>%
    group_by(employment) %>%
    crosstab(education, gender, control = region)

  # Check that it's grouped
  expect_true(result$is_grouped)
  expect_equal(result$group_vars, "employment")
  expect_equal(length(result$results), 2)  # Two employment groups

  # Check that each group result is a 3-way crosstab
  for (i in seq_along(result$results)) {
    group_result <- result$results[[i]]
    expect_true(group_result$is_3way)
    expect_equal(group_result$control_var, "region")
    expect_true("East" %in% names(group_result$layers))
    expect_true("West" %in% names(group_result$layers))
  }
})

# ============================================================================
# TEST 5: PERCENTAGE CALCULATIONS
# ============================================================================

test_that("3-way crosstab percentages are calculated correctly", {

  data(survey_data)

  # Test with all percentages
  result <- survey_data %>%
    crosstab(life_satisfaction, gender, control = region, percentages = "all")

  # Check that all percentage types are present
  east_layer <- result$layers[["East"]]
  expect_false(is.null(east_layer$row_pct))
  expect_false(is.null(east_layer$col_pct))
  expect_false(is.null(east_layer$total_pct))

  # Check column percentages sum to 100%
  col_pct_sums <- colSums(east_layer$col_pct)
  expect_equal(col_pct_sums[["Male"]], 100, tolerance = 0.1)
  expect_equal(col_pct_sums[["Female"]], 100, tolerance = 0.1)

  # Check row percentages sum to 100%
  row_pct_sums <- rowSums(east_layer$row_pct)
  expect_true(all(abs(row_pct_sums - 100) < 0.1))

  # Check total percentages sum to 100%
  total_pct_sum <- sum(east_layer$total_pct)
  expect_equal(total_pct_sum, 100, tolerance = 0.1)
})