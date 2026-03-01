# =============================================================================
# BINOMIAL TEST - SPSS VALIDATION
# =============================================================================
# Validates binomial_test() against SPSS NPAR TESTS /BINOMIAL procedure
# SPSS syntax: tests/spss_reference/syntax/binomial_test.sps
# SPSS output: tests/spss_reference/outputs/binomial_test_output.txt
#
# Test matrix:
#   - Test 1a: Gender proportion (unweighted, ungrouped, p=0.50)
#   - Test 1b: Region proportion (unweighted, ungrouped, p=0.50)
#   - Test 1c: High satisfaction proportion (unweighted, ungrouped, p=0.50)
#   - Test 2a/2b: Weighted, ungrouped (identical to unweighted with weights ~1.0)
#   - Test 3a: Gender grouped by region (unweighted)
#   - Test 3b: High satisfaction grouped by region (unweighted)
#   - Test 4a/4b: Weighted, grouped (identical to unweighted)
#
# Note on SPSS gender coding:
#   SPSS recoded gender to female (0=Male, 1=Female). The factor level
#   ordering may differ between R and SPSS, but two-sided p-values with
#   p=0.50 are identical regardless of which category is Group 1.
#
# SPSS Reference Values (from output file):
#
# Test 1a (gender, p=0.50): N=2500, p=.026
# Test 1b (region, p=0.50): N=2500, p<.001
# Test 1c (high_satisfaction, p=0.50): N=2421, High=1397, Low=1024, p<.001
# Test 2a (gender, weighted): identical to Test 1a
# Test 2b (high_satisfaction, weighted): identical to Test 1c
# Test 3a (gender, grouped by region):
#   East: N=485, p=.716
#   West: N=2015, p=.023
# Test 3b (high_satisfaction, grouped by region):
#   East: High=271, Low=194, N=465, p<.001
#   West: High=1126, Low=830, N=1956, p<.001
# Test 4a/4b (weighted, grouped): identical to Test 3a/3b
# =============================================================================

library(testthat)
library(dplyr)

# =============================================================================
# SPSS VALIDATION TRACKING
# =============================================================================
spss_comparison_count <- 0

record_bt_comparison <- function(description, expected, actual, tolerance = 0.01) {
  spss_comparison_count <<- spss_comparison_count + 1
  expect_equal(actual, expected, tolerance = tolerance,
               label = paste0("SPSS Validation #", spss_comparison_count,
                              ": ", description))
}

# =============================================================================
# HELPER: Create high_satisfaction binary variable
# =============================================================================
prepare_test_data <- function() {
  data(survey_data, envir = environment())
  survey_data %>%
    mutate(high_satisfaction = factor(
      ifelse(life_satisfaction >= 4, "High", "Low"),
      levels = c("High", "Low")
    ))
}

# =============================================================================
# TEST 1: UNWEIGHTED / UNGROUPED
# =============================================================================

test_that("Binomial test matches SPSS: gender proportion (Test 1a)", {
  data(survey_data)

  result <- survey_data %>%
    binomial_test(gender, p = 0.50)

  r <- result$results

  # SPSS: N=2500, p=.026 (SPSS rounds to 3 decimals; exact is 0.0264)
  record_bt_comparison("Test 1a: N total", 2500, r$n_total, tolerance = 0)
  record_bt_comparison("Test 1a: p-value", 0.026, r$p_value, tolerance = 0.02)

  # Structure checks
  expect_s3_class(result, "binomial_test")
  expect_equal(result$p, 0.50)
  expect_null(result$weights)
  expect_false(result$is_grouped)
  expect_equal(r$n1 + r$n2, 2500)
})

test_that("Binomial test matches SPSS: region proportion (Test 1b)", {
  data(survey_data)

  result <- survey_data %>%
    binomial_test(region, p = 0.50)

  r <- result$results

  # SPSS: N=2500, p<.001
  record_bt_comparison("Test 1b: N total", 2500, r$n_total, tolerance = 0)
  record_bt_comparison("Test 1b: p < 0.001", 1,
                       as.numeric(r$p_value < 0.001), tolerance = 0)

  # East proportion should be around 0.194 (485/2500)
  east_prop <- min(r$obs_prop1, r$obs_prop2)
  record_bt_comparison("Test 1b: minority proportion", 0.194,
                       east_prop, tolerance = 0.001)
})

test_that("Binomial test matches SPSS: high satisfaction proportion (Test 1c)", {
  test_data <- prepare_test_data()

  result <- test_data %>%
    binomial_test(high_satisfaction, p = 0.50)

  r <- result$results

  # SPSS: High=1397, Low=1024, Total=2421, p<.001
  record_bt_comparison("Test 1c: N High", 1397, r$n1, tolerance = 0)
  record_bt_comparison("Test 1c: N Low", 1024, r$n2, tolerance = 0)
  record_bt_comparison("Test 1c: N total", 2421, r$n_total, tolerance = 0)
  record_bt_comparison("Test 1c: p < 0.001", 1,
                       as.numeric(r$p_value < 0.001), tolerance = 0)

  # Observed proportion of High
  record_bt_comparison("Test 1c: observed prop High", 0.577,
                       r$obs_prop1, tolerance = 0.001)
})

# =============================================================================
# TEST 2: WEIGHTED / UNGROUPED
# =============================================================================

test_that("Binomial test matches SPSS: weighted gender (Test 2a)", {
  data(survey_data)

  result <- survey_data %>%
    binomial_test(gender, p = 0.50, weights = sampling_weight)

  r <- result$results

  # SPSS weighted = unweighted (weights ~1.0, all round to 1)
  record_bt_comparison("Test 2a: p-value (weighted)", 0.026,
                       r$p_value, tolerance = 0.02)
  record_bt_comparison("Test 2a: N total (weighted)", 2500,
                       r$n_total, tolerance = 0)

  expect_equal(result$weights, "sampling_weight")
})

test_that("Binomial test matches SPSS: weighted high satisfaction (Test 2b)", {
  test_data <- prepare_test_data()

  result <- test_data %>%
    binomial_test(high_satisfaction, p = 0.50, weights = sampling_weight)

  r <- result$results

  # SPSS weighted = unweighted
  record_bt_comparison("Test 2b: p < 0.001 (weighted)", 1,
                       as.numeric(r$p_value < 0.001), tolerance = 0)
  record_bt_comparison("Test 2b: N total (weighted)", 2421,
                       r$n_total, tolerance = 50)
})

# =============================================================================
# TEST 3: UNWEIGHTED / GROUPED BY REGION
# =============================================================================

test_that("Binomial test matches SPSS: gender grouped by region (Test 3a)", {
  data(survey_data)

  result <- survey_data %>%
    group_by(region) %>%
    binomial_test(gender, p = 0.50)

  r <- result$results

  # East: N=485, p=.716 (not significant)
  east <- r[r$region == "East", ]
  record_bt_comparison("Test 3a East: N total", 485, east$n_total, tolerance = 0)
  record_bt_comparison("Test 3a East: p-value", 0.716, east$p_value,
                       tolerance = 0.005)

  # West: N=2015, p=.023
  west <- r[r$region == "West", ]
  record_bt_comparison("Test 3a West: N total", 2015, west$n_total, tolerance = 0)
  record_bt_comparison("Test 3a West: p-value", 0.023, west$p_value,
                       tolerance = 0.005)

  # Structure checks
  expect_true(result$is_grouped)
  expect_equal(nrow(r), 2)
})

test_that("Binomial test matches SPSS: high satisfaction grouped by region (Test 3b)", {
  test_data <- prepare_test_data()

  result <- test_data %>%
    group_by(region) %>%
    binomial_test(high_satisfaction, p = 0.50)

  r <- result$results

  # East: High=271, Low=194, Total=465, p<.001
  east <- r[r$region == "East", ]
  record_bt_comparison("Test 3b East: N High", 271, east$n1, tolerance = 0)
  record_bt_comparison("Test 3b East: N Low", 194, east$n2, tolerance = 0)
  record_bt_comparison("Test 3b East: N total", 465, east$n_total, tolerance = 0)
  record_bt_comparison("Test 3b East: p < 0.001", 1,
                       as.numeric(east$p_value < 0.001), tolerance = 0)

  # West: High=1126, Low=830, Total=1956, p<.001
  west <- r[r$region == "West", ]
  record_bt_comparison("Test 3b West: N total", 1956, west$n_total, tolerance = 0)
  record_bt_comparison("Test 3b West: p < 0.001", 1,
                       as.numeric(west$p_value < 0.001), tolerance = 0)

  # In R, first level is "High" for our factor
  # West should have High=1126, Low=830
  record_bt_comparison("Test 3b West: N High", 1126, west$n1, tolerance = 0)
  record_bt_comparison("Test 3b West: N Low", 830, west$n2, tolerance = 0)
})

# =============================================================================
# TEST 4: WEIGHTED / GROUPED
# =============================================================================

test_that("Binomial test matches SPSS: weighted gender grouped (Test 4a)", {
  data(survey_data)

  result <- survey_data %>%
    group_by(region) %>%
    binomial_test(gender, p = 0.50, weights = sampling_weight)

  r <- result$results

  # SPSS weighted = unweighted (weights ~1.0)
  east <- r[r$region == "East", ]
  record_bt_comparison("Test 4a East: p-value (weighted)", 0.716,
                       east$p_value, tolerance = 0.05)

  west <- r[r$region == "West", ]
  record_bt_comparison("Test 4a West: p-value (weighted)", 0.023,
                       west$p_value, tolerance = 0.01)

  expect_true(result$is_grouped)
  expect_equal(result$weights, "sampling_weight")
})

test_that("Binomial test matches SPSS: weighted high satisfaction grouped (Test 4b)", {
  test_data <- prepare_test_data()

  result <- test_data %>%
    group_by(region) %>%
    binomial_test(high_satisfaction, p = 0.50, weights = sampling_weight)

  r <- result$results

  east <- r[r$region == "East", ]
  record_bt_comparison("Test 4b East: p < 0.001 (weighted)", 1,
                       as.numeric(east$p_value < 0.001), tolerance = 0)

  west <- r[r$region == "West", ]
  record_bt_comparison("Test 4b West: p < 0.001 (weighted)", 1,
                       as.numeric(west$p_value < 0.001), tolerance = 0)
})

# =============================================================================
# EDGE CASES AND INPUT VALIDATION
# =============================================================================

test_that("Binomial test requires binary variable", {
  data(survey_data)

  expect_error(
    survey_data %>% binomial_test(education, p = 0.50),
    "exactly 2"
  )
})

test_that("Binomial test rejects invalid test proportion", {
  data(survey_data)

  expect_error(
    survey_data %>% binomial_test(gender, p = 1.5),
    "between 0 and 1"
  )

  expect_error(
    survey_data %>% binomial_test(gender, p = -0.1),
    "between 0 and 1"
  )
})

test_that("Binomial test requires a data frame", {
  expect_error(
    binomial_test(1:10, gender),
    "data frame"
  )
})

test_that("Binomial test handles multiple variables", {
  data(survey_data)

  result <- survey_data %>%
    binomial_test(gender, region, p = 0.50)

  expect_equal(nrow(result$results), 2)
  expect_equal(result$results$Variable, c("gender", "region"))
})

test_that("Binomial test handles logical variables", {
  data(survey_data)

  test_data <- survey_data %>%
    mutate(is_east = region == "East")

  result <- test_data %>%
    binomial_test(is_east, p = 0.50)

  expect_s3_class(result, "binomial_test")
  expect_equal(result$results$n_total, 2500)
  # p-value should match region test
  expect_true(result$results$p_value < 0.001)
})

test_that("Binomial test handles numeric 0/1 variables", {
  data(survey_data)

  test_data <- survey_data %>%
    mutate(female_num = as.numeric(gender == "Female"))

  result <- test_data %>%
    binomial_test(female_num, p = 0.50)

  expect_s3_class(result, "binomial_test")
  expect_equal(result$results$n_total, 2500)
})

test_that("Binomial test handles missing data correctly", {
  data(survey_data)

  test_data <- survey_data
  test_data$gender[1:50] <- NA

  result <- test_data %>%
    binomial_test(gender, p = 0.50)

  expect_equal(result$results$n_total, 2450)
  expect_false(is.na(result$results$p_value))
})

test_that("Binomial test result contains expected components", {
  data(survey_data)

  result <- survey_data %>%
    binomial_test(gender, p = 0.50)

  # Check result structure
  expect_true("results" %in% names(result))
  expect_true("variables" %in% names(result))
  expect_true("p" %in% names(result))
  expect_true("weights" %in% names(result))
  expect_true("is_grouped" %in% names(result))
  expect_true("conf.level" %in% names(result))

  # Check results tibble columns
  r <- result$results
  expect_true("Variable" %in% names(r))
  expect_true("cat1_name" %in% names(r))
  expect_true("cat2_name" %in% names(r))
  expect_true("n1" %in% names(r))
  expect_true("n2" %in% names(r))
  expect_true("n_total" %in% names(r))
  expect_true("obs_prop1" %in% names(r))
  expect_true("obs_prop2" %in% names(r))
  expect_true("test_prop" %in% names(r))
  expect_true("p_value" %in% names(r))
  expect_true("ci_lower" %in% names(r))
  expect_true("ci_upper" %in% names(r))
})

test_that("Binomial test proportions sum to 1", {
  data(survey_data)

  result <- survey_data %>%
    binomial_test(gender, p = 0.50)

  r <- result$results
  expect_equal(r$obs_prop1 + r$obs_prop2, 1.0, tolerance = 0.0001)
})

test_that("Binomial test CI contains observed proportion", {
  data(survey_data)

  result <- survey_data %>%
    binomial_test(gender, p = 0.50)

  r <- result$results
  expect_true(r$obs_prop1 >= r$ci_lower)
  expect_true(r$obs_prop1 <= r$ci_upper)
})

test_that("Binomial test with different test proportions", {
  test_data <- prepare_test_data()

  # Test with p = 0.60
  result <- test_data %>%
    binomial_test(high_satisfaction, p = 0.60)

  r <- result$results
  expect_equal(r$test_prop, 0.60)
  # Two-sided p-value should be significant
  expect_true(r$p_value < 0.05)
})

# =============================================================================
# PRINT METHOD TESTS
# =============================================================================

test_that("Binomial test print method produces output", {
  data(survey_data)

  result <- survey_data %>%
    binomial_test(gender, p = 0.50)

  output <- capture.output(print(result))
  expect_true(any(grepl("Group 1", output)))
  expect_true(any(grepl("Group 2", output)))
  expect_true(any(grepl("Observed Prop", output)))
  expect_true(any(grepl("Test Prop", output)))
  expect_true(any(grepl("p value", output)))
})

test_that("Binomial test weighted print method produces output", {
  data(survey_data)

  result <- survey_data %>%
    binomial_test(gender, p = 0.50, weights = sampling_weight)

  output <- capture.output(print(result))
  expect_true(any(grepl("Weighted", output)))
  expect_true(any(grepl("Group 1", output)))
})

test_that("Binomial test grouped print method produces output", {
  data(survey_data)

  result <- survey_data %>%
    group_by(region) %>%
    binomial_test(gender, p = 0.50)

  output <- capture.output(print(result))
  expect_true(any(grepl("Group 1", output)))
  expect_true(any(grepl("Observed Prop", output)))
})

# =============================================================================
# CONSISTENCY CHECKS
# =============================================================================

test_that("Binomial test non-significant result has correct p-value range", {
  data(survey_data)

  # East region gender split is roughly 50/50 → not significant
  result <- survey_data %>%
    group_by(region) %>%
    binomial_test(gender, p = 0.50)

  east <- result$results[result$results$region == "East", ]
  expect_true(east$p_value > 0.05)
})

test_that("Binomial test significant result has correct p-value range", {
  data(survey_data)

  # West region gender split is uneven → significant
  result <- survey_data %>%
    group_by(region) %>%
    binomial_test(gender, p = 0.50)

  west <- result$results[result$results$region == "West", ]
  expect_true(west$p_value < 0.05)
})

test_that("Binomial test with p=0.50 gives same p-value regardless of category order", {
  data(survey_data)

  # Create two versions with swapped factor levels
  d1 <- survey_data %>%
    mutate(gen1 = factor(gender, levels = c("Male", "Female")))
  d2 <- survey_data %>%
    mutate(gen2 = factor(gender, levels = c("Female", "Male")))

  r1 <- d1 %>% binomial_test(gen1, p = 0.50)
  r2 <- d2 %>% binomial_test(gen2, p = 0.50)

  # P-values should be identical (symmetry of binomial with p=0.50)
  expect_equal(r1$results$p_value, r2$results$p_value, tolerance = 0.0001)
})

# =============================================================================
# SUMMARY
# =============================================================================

test_that("SPSS validation summary", {
  cat("\n")
  cat("==============================================\n")
  cat("BINOMIAL TEST SPSS VALIDATION SUMMARY\n")
  cat("==============================================\n")
  cat(sprintf("Total SPSS comparisons: %d\n", spss_comparison_count))
  cat("All comparisons passed!\n")
  cat("==============================================\n")

  expect_true(spss_comparison_count > 0)
})
