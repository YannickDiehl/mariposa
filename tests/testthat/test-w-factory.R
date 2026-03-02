# ==============================================================================
# Tests for w_factory infrastructure (.w_statistic, .w_format_results,
# .print_w_statistic) exercised through public w_* functions
# ==============================================================================
# These tests verify the shared factory code paths that underlie all w_*
# functions (w_mean, w_median, w_sd, etc.), including data frame mode,
# grouping, weighting, multi-variable long format, error handling, NA handling,
# S3 class assignment, and print methods.
# ==============================================================================

library(testthat)
library(mariposa)
library(dplyr)

data(survey_data)

# ==============================================================================
# 1. Data frame mode - single variable, unweighted
# ==============================================================================

test_that("single variable unweighted returns correct structure", {

  result <- w_mean(survey_data, age)

  # S3 object structure

  expect_type(result, "list")
  expect_true("results" %in% names(result))
  expect_true("variables" %in% names(result))
  expect_true("weights" %in% names(result))
  expect_true("is_grouped" %in% names(result))

  # Results tibble structure
  expect_s3_class(result$results, "tbl_df")
  expect_true("mean" %in% names(result$results))
  expect_true("n" %in% names(result$results))

  # Weighted columns should NOT be present

  expect_false("weighted_mean" %in% names(result$results))
  expect_false("effective_n" %in% names(result$results))

  # Metadata
  expect_null(result$weights)
  expect_false(result$is_grouped)
  expect_equal(result$variables, "age")

  # Values are numeric and plausible
  expect_true(is.numeric(result$results$mean))
  expect_true(result$results$mean > 0)
  expect_true(result$results$n > 0)
})

test_that("single variable unweighted works for w_sd and w_median too", {
  result_sd <- w_sd(survey_data, age)
  result_med <- w_median(survey_data, age)

  expect_true("sd" %in% names(result_sd$results))
  expect_true("n" %in% names(result_sd$results))
  expect_true(result_sd$results$sd > 0)

  expect_true("median" %in% names(result_med$results))
  expect_true("n" %in% names(result_med$results))
  expect_true(result_med$results$median > 0)
})


# ==============================================================================
# 2. Data frame mode - single variable, weighted
# ==============================================================================

test_that("single variable weighted returns correct structure", {
  result <- w_mean(survey_data, age, weights = sampling_weight)

  # Weighted columns should be present
  expect_true("weighted_mean" %in% names(result$results))
  expect_true("effective_n" %in% names(result$results))

  # Unweighted column should NOT be present
  expect_false("mean" %in% names(result$results))

  # Metadata
  expect_equal(result$weights, "sampling_weight")
  expect_false(result$is_grouped)

  # Values are numeric and plausible
  expect_true(is.numeric(result$results$weighted_mean))
  expect_true(result$results$weighted_mean > 0)
  expect_true(result$results$effective_n > 0)

  # Effective N should differ from raw count (weights are not all 1)
  expect_true(is.numeric(result$results$effective_n))
})

test_that("single variable weighted works for w_sd and w_median too", {
  result_sd <- w_sd(survey_data, age, weights = sampling_weight)
  result_med <- w_median(survey_data, age, weights = sampling_weight)

  expect_true("weighted_sd" %in% names(result_sd$results))
  expect_true("effective_n" %in% names(result_sd$results))
  expect_true(result_sd$results$weighted_sd > 0)

  expect_true("weighted_median" %in% names(result_med$results))
  expect_true("effective_n" %in% names(result_med$results))
  expect_true(result_med$results$weighted_median > 0)
})


# ==============================================================================
# 3. Data frame mode - multiple variables
# ==============================================================================

test_that("multiple variables produce long-format output with Variable column", {
  result <- w_mean(survey_data, age, income)

  expect_true("Variable" %in% names(result$results))
  expect_true("mean" %in% names(result$results))
  expect_true("n" %in% names(result$results))

  # Two rows, one per variable
  expect_equal(nrow(result$results), 2)
  expect_equal(result$results$Variable, c("age", "income"))

  # Metadata
  expect_equal(result$variables, c("age", "income"))
  expect_null(result$weights)

  # Values differ between variables
  expect_false(result$results$mean[1] == result$results$mean[2])
})

test_that("multiple variables weighted produce long-format with weighted columns", {
  result <- w_mean(survey_data, age, income, weights = sampling_weight)

  expect_true("Variable" %in% names(result$results))
  expect_true("weighted_mean" %in% names(result$results))
  expect_true("effective_n" %in% names(result$results))
  expect_equal(nrow(result$results), 2)
  expect_equal(result$results$Variable, c("age", "income"))
  expect_equal(result$weights, "sampling_weight")
})


# ==============================================================================
# 4. Data frame mode - grouped (single variable)
# ==============================================================================

test_that("grouped data produces results with group columns", {
  result <- survey_data %>%
    group_by(region) %>%
    w_mean(age)

  expect_true(result$is_grouped)
  expect_equal(result$groups, "region")

  # Group column should be in results
  expect_true("region" %in% names(result$results))
  expect_true("mean" %in% names(result$results))

  # Should have one row per group level
  region_levels <- unique(survey_data$region)
  expect_equal(nrow(result$results), length(region_levels))

  # Each group should have a different mean (very unlikely to be equal)
  expect_equal(length(unique(result$results$mean)), length(region_levels))
})

test_that("grouped weighted data has correct structure", {
  result <- survey_data %>%
    group_by(region) %>%
    w_mean(age, weights = sampling_weight)

  expect_true(result$is_grouped)
  expect_true("region" %in% names(result$results))
  expect_true("weighted_mean" %in% names(result$results))
  expect_true("effective_n" %in% names(result$results))
  expect_equal(result$weights, "sampling_weight")

  region_levels <- unique(survey_data$region)
  expect_equal(nrow(result$results), length(region_levels))
})


# ==============================================================================
# 5. Data frame mode - grouped + weighted + multiple variables (full combo)
# ==============================================================================

test_that("grouped + weighted + multi-variable produces correct long-format output", {
  result <- survey_data %>%
    group_by(region) %>%
    w_mean(age, income, weights = sampling_weight)

  expect_true(result$is_grouped)
  expect_equal(result$weights, "sampling_weight")
  expect_equal(result$variables, c("age", "income"))

  # Should have Variable column (long format) plus group column
  expect_true("Variable" %in% names(result$results))
  expect_true("region" %in% names(result$results))
  expect_true("weighted_mean" %in% names(result$results))
  expect_true("effective_n" %in% names(result$results))

  # Number of rows = n_groups * n_variables
  region_levels <- unique(survey_data$region)
  expected_rows <- length(region_levels) * 2  # 2 variables
  expect_equal(nrow(result$results), expected_rows)

  # Each variable should appear for each group
  for (reg in region_levels) {
    group_rows <- result$results[result$results$region == reg, ]
    expect_equal(sort(group_rows$Variable), c("age", "income"))
  }
})

test_that("grouped + multi-variable unweighted works correctly", {
  result <- survey_data %>%
    group_by(region) %>%
    w_sd(age, income)

  expect_true(result$is_grouped)
  expect_null(result$weights)
  expect_true("Variable" %in% names(result$results))
  expect_true("region" %in% names(result$results))
  expect_true("sd" %in% names(result$results))
  expect_true("n" %in% names(result$results))
})


# ==============================================================================
# 6. Non-data-frame error
# ==============================================================================

test_that("non-data-frame input to data frame mode produces error", {
  expect_error(
    w_mean("not a data frame", age),
    "must be a data frame"
  )

  expect_error(
    w_sd(list(a = 1, b = 2), age),
    "must be a data frame"
  )
})


# ==============================================================================
# 7. Missing weights column error
# ==============================================================================

test_that("nonexistent weights column produces error", {
  expect_error(
    w_mean(survey_data, age, weights = nonexistent_column),
    "not found in data"
  )

  expect_error(
    w_sd(survey_data, age, weights = nonexistent_column),
    "not found in data"
  )
})


# ==============================================================================
# 8. Print method - basic output
# ==============================================================================

test_that("print method produces non-empty output", {
  result <- w_mean(survey_data, age)
  output <- capture.output(print(result))

  expect_true(length(output) > 0)
  expect_true(any(nchar(output) > 0))
})

test_that("print method for multiple variables produces output for each variable", {
  result <- w_mean(survey_data, age, income)
  output <- capture.output(print(result))

  # Both variable names should appear in the output
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("age", combined))
  expect_true(grepl("income", combined))
})

test_that("print method for grouped data mentions group values", {
  result <- survey_data %>%
    group_by(region) %>%
    w_mean(age)
  output <- capture.output(print(result))

  combined <- paste(output, collapse = "\n")
  expect_true(grepl("Group", combined))
  expect_true(grepl("region", combined))
})


# ==============================================================================
# 9. Print method - weighted label
# ==============================================================================

test_that("print method for weighted results shows 'Weighted' in header", {
  result <- w_mean(survey_data, age, weights = sampling_weight)
  output <- capture.output(print(result))

  combined <- paste(output, collapse = "\n")
  expect_true(grepl("Weighted", combined, ignore.case = TRUE))
})

test_that("print method for unweighted results does not show 'Weighted' in header", {
  result <- w_mean(survey_data, age)
  output <- capture.output(print(result))

  combined <- paste(output, collapse = "\n")
  # The title should NOT contain "Weighted" when there are no weights
  # (it uses get_standard_title which distinguishes weighted/unweighted)
  expect_false(grepl("Weighted", combined))
})


# ==============================================================================
# 10. S3 class assignment
# ==============================================================================

test_that("w_mean returns object with class 'w_mean'", {
  result <- w_mean(survey_data, age)
  expect_true(inherits(result, "w_mean"))
  expect_equal(class(result), "w_mean")
})

test_that("w_sd returns object with class 'w_sd'", {
  result <- w_sd(survey_data, age)
  expect_true(inherits(result, "w_sd"))
  expect_equal(class(result), "w_sd")
})

test_that("w_median returns object with class 'w_median'", {
  result <- w_median(survey_data, age)
  expect_true(inherits(result, "w_median"))
  expect_equal(class(result), "w_median")
})

test_that("class assignment works regardless of weights or grouping", {
  # Weighted
  result_w <- w_mean(survey_data, age, weights = sampling_weight)
  expect_true(inherits(result_w, "w_mean"))


  # Grouped
  result_g <- survey_data %>% group_by(region) %>% w_mean(age)
  expect_true(inherits(result_g, "w_mean"))

  # Multi-variable
  result_mv <- w_mean(survey_data, age, income)
  expect_true(inherits(result_mv, "w_mean"))
})


# ==============================================================================
# 11. Effective N calculation
# ==============================================================================

test_that("weighted result has effective_n column with plausible value", {
  result <- w_mean(survey_data, age, weights = sampling_weight)

  expect_true("effective_n" %in% names(result$results))
  eff_n <- result$results$effective_n

  # Effective N should be positive
  expect_true(eff_n > 0)

  # Effective N should not exceed the raw sample size
  n_raw <- sum(!is.na(survey_data$age))
  expect_true(eff_n <= n_raw + 1)  # small tolerance for rounding

  # For non-uniform weights, effective N should be less than raw N
  # (sampling_weight varies between 0.7 and 1.4 per data docs)
  expect_true(eff_n < n_raw)
})

test_that("effective_n matches manual calculation", {
  # Manual calculation: (sum(w))^2 / sum(w^2) for complete cases
  valid <- !is.na(survey_data$age) & !is.na(survey_data$sampling_weight)
  w <- survey_data$sampling_weight[valid]
  expected_eff_n <- sum(w)^2 / sum(w^2)

  result <- w_mean(survey_data, age, weights = sampling_weight)
  expect_equal(result$results$effective_n, expected_eff_n, tolerance = 0.01)
})

test_that("unweighted result has n column but NOT effective_n", {
  result <- w_mean(survey_data, age)

  expect_true("n" %in% names(result$results))
  expect_false("effective_n" %in% names(result$results))
})

test_that("effective_n is computed per group in grouped analysis", {
  result <- survey_data %>%
    group_by(region) %>%
    w_mean(age, weights = sampling_weight)

  # Each group should have its own effective_n
  expect_true("effective_n" %in% names(result$results))
  expect_equal(nrow(result$results), length(unique(survey_data$region)))

  # effective_n values should differ across groups (different group sizes)
  expect_false(
    result$results$effective_n[1] == result$results$effective_n[2]
  )
})


# ==============================================================================
# 12. NA handling
# ==============================================================================

test_that("na.rm = TRUE (default) handles NAs correctly", {
  # income has realistic missing data (3-12% per data docs)
  result <- w_mean(survey_data, income)

  # Should return a valid numeric result despite NAs
  expect_true(is.numeric(result$results$mean))
  expect_false(is.na(result$results$mean))

  # n should be less than total rows (because of NAs)
  expect_true(result$results$n < nrow(survey_data))
})

test_that("NA handling works with weights", {
  result <- w_mean(survey_data, income, weights = sampling_weight)

  expect_true(is.numeric(result$results$weighted_mean))
  expect_false(is.na(result$results$weighted_mean))
  expect_true(result$results$effective_n > 0)
})

test_that("NA handling works with manually introduced NAs", {
  # Create a small dataset with known NAs
  test_data <- tibble::tibble(
    x = c(1, 2, NA, 4, 5),
    w = c(1.0, 1.5, 2.0, 0.5, 1.0)
  )

  # With na.rm = TRUE (default), should compute on non-NA values
  result <- w_mean(test_data, x)
  expect_false(is.na(result$results$mean))
  expect_equal(result$results$n, 4)  # 5 obs minus 1 NA
  expect_equal(result$results$mean, mean(c(1, 2, 4, 5)))

  # Weighted with NAs
  result_w <- w_mean(test_data, x, weights = w)
  expect_false(is.na(result_w$results$weighted_mean))

  # The weighted mean should use only complete cases for both x and w
  valid_x <- c(1, 2, 4, 5)
  valid_w <- c(1.0, 1.5, 0.5, 1.0)
  expected_wmean <- sum(valid_x * valid_w) / sum(valid_w)
  expect_equal(result_w$results$weighted_mean, expected_wmean)
})

test_that("NA handling works for multiple variables with different NA patterns", {
  test_data <- tibble::tibble(
    a = c(1, 2, NA, 4, 5),
    b = c(NA, 2, 3, NA, 5),
    w = c(1.0, 1.5, 2.0, 0.5, 1.0)
  )

  result <- w_mean(test_data, a, b)
  expect_equal(nrow(result$results), 2)
  expect_equal(result$results$Variable, c("a", "b"))

  # Different n values because different NA patterns
  expect_equal(result$results$n[1], 4)  # a: 4 valid

  expect_equal(result$results$n[2], 3)  # b: 3 valid

  expect_equal(result$results$mean[1], mean(c(1, 2, 4, 5)))
  expect_equal(result$results$mean[2], mean(c(2, 3, 5)))
})

test_that("NAs in weights are handled jointly with NAs in values", {
  test_data <- tibble::tibble(
    x = c(1, 2, 3, 4, 5),
    w = c(1.0, NA, 2.0, 0.5, 1.0)
  )

  result <- w_mean(test_data, x, weights = w)

  # Observation 2 (x=2) should be excluded due to NA weight
  valid_x <- c(1, 3, 4, 5)
  valid_w <- c(1.0, 2.0, 0.5, 1.0)
  expected_wmean <- sum(valid_x * valid_w) / sum(valid_w)
  expect_equal(result$results$weighted_mean, expected_wmean)
})


# ==============================================================================
# Additional edge cases for factory coverage
# ==============================================================================

test_that("print method returns the object invisibly", {
  result <- w_mean(survey_data, age)
  returned <- withVisible(print(result))
  expect_false(returned$visible)
  expect_true(inherits(returned$value, "w_mean"))
})

test_that("result object contains correct metadata fields", {
  result <- survey_data %>%
    group_by(region) %>%
    w_mean(age, income, weights = sampling_weight)

  expect_equal(result$variables, c("age", "income"))
  expect_equal(result$weights, "sampling_weight")
  expect_true(result$is_grouped)
  expect_equal(result$groups, "region")
})

test_that("w_sd on single-element groups returns NA as expected", {
  # Create a group with only one observation (SD is NA for n < 2)
  small_data <- tibble::tibble(
    x = c(10, 20, 30),
    grp = c("a", "a", "b"),
    w = c(1.0, 1.5, 2.0)
  )

  result <- small_data %>%
    group_by(grp) %>%
    w_sd(x)

  # Group "b" has only 1 observation, SD should be NA
  b_row <- result$results[result$results$grp == "b", ]
  expect_true(is.na(b_row$sd))

  # Group "a" has 2 observations, SD should be valid
  a_row <- result$results[result$results$grp == "a", ]
  expect_false(is.na(a_row$sd))
})

test_that("factory handles zero-length data gracefully in weighted mode", {
  # All values are NA -> after na.rm, zero-length
  test_data <- tibble::tibble(
    x = c(NA_real_, NA_real_, NA_real_),
    w = c(1.0, 1.5, 2.0)
  )

  result <- w_mean(test_data, x, weights = w)
  expect_true(is.na(result$results$weighted_mean))
})
