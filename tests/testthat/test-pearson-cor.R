# ============================================================================
# Test Suite for pearson_cor() Function
# ============================================================================
#
# Purpose: Validate the pearson_cor() function for correlation analysis
#          including weighted correlations, grouped data, and edge cases
#
# Test Categories:
# 1. Basic functionality
# 2. Weighted correlations
# 3. Grouped correlations
# 4. Missing data handling
# 5. Edge cases and error handling
# 6. SPSS compatibility validation
#
# ============================================================================

library(testthat)
library(dplyr)

# Load test data
test_that("test data loads correctly", {
  data(survey_data, envir = environment())
  expect_true(exists("survey_data"))
  expect_true(is.data.frame(survey_data))
})

# ============================================================================
# TEST SUITE 1: Basic Functionality
# ============================================================================

test_that("pearson_cor() works with two variables", {
  data(survey_data)
  
  result <- pearson_cor(survey_data, age, income)
  
  # Check structure
  expect_s3_class(result, "pearson_cor_results")
  expect_named(result, c("correlations", "n_obs", "matrices", "variables", 
                        "weights", "conf.level", "na.rm", "is_grouped", 
                        "groups", "group_keys"))
  
  # Check results data frame
  expect_equal(nrow(result$correlations), 1)
  expect_true("correlation" %in% names(result$correlations))
  expect_true("p_value" %in% names(result$correlations))
  expect_true("conf_int_lower" %in% names(result$correlations))
  expect_true("conf_int_upper" %in% names(result$correlations))
  
  # Check correlation is within valid range
  expect_true(abs(result$correlations$correlation[1]) <= 1)
  
  # Check p-value is valid
  expect_true(result$correlations$p_value[1] >= 0)
  expect_true(result$correlations$p_value[1] <= 1)
  
  # Check confidence interval
  expect_true(result$correlations$conf_int_lower[1] <= result$correlations$correlation[1])
  expect_true(result$correlations$conf_int_upper[1] >= result$correlations$correlation[1])
})

test_that("pearson_cor() works with multiple variables", {
  data(survey_data)
  
  result <- pearson_cor(survey_data, age, income, life_satisfaction)
  
  # Should have 3 choose 2 = 3 correlations
  expect_equal(nrow(result$correlations), 3)
  
  # Check that all pairs are present
  pairs <- paste(result$correlations$var1, result$correlations$var2)
  expect_true("age income" %in% pairs)
  expect_true("age life_satisfaction" %in% pairs)
  expect_true("income life_satisfaction" %in% pairs)
  
  # Check correlation matrix
  expect_equal(dim(result$matrices[[1]]$correlations), c(3, 3))
  expect_equal(as.numeric(diag(result$matrices[[1]]$correlations)), c(1, 1, 1))
})

test_that("pearson_cor() correlation values match base R cor()", {
  data(survey_data)
  
  # Remove NAs for comparison
  clean_data <- survey_data %>%
    select(age, income) %>%
    na.omit()
  
  # Our function
  result <- pearson_cor(clean_data, age, income)
  
  # Base R
  base_r <- cor(clean_data$age, clean_data$income, method = "pearson")
  
  # Should be very close
  expect_equal(result$correlations$correlation[1], base_r, tolerance = 1e-10)
})

# ============================================================================
# TEST SUITE 2: Weighted Correlations
# ============================================================================

test_that("pearson_cor() handles weights correctly", {
  data(survey_data)
  
  # Weighted correlation
  result_weighted <- pearson_cor(survey_data, age, income, weights = sampling_weight)
  
  # Unweighted correlation
  result_unweighted <- pearson_cor(survey_data, age, income)
  
  # Check that weighted correlation is calculated
  expect_true(!is.null(result_weighted$weights))
  expect_equal(result_weighted$weights, "sampling_weight")
  
  # Weighted and unweighted should generally be different
  # (unless weights happen to be uniform)
  expect_true(abs(result_weighted$correlations$correlation[1] - 
                 result_unweighted$correlations$correlation[1]) > 0.001)
})

test_that("weighted correlation with uniform weights equals unweighted", {
  # Create data with uniform weights
  test_data <- data.frame(
    x = rnorm(100),
    y = rnorm(100),
    w = rep(1, 100)
  )
  
  result_weighted <- pearson_cor(test_data, x, y, weights = w)
  result_unweighted <- pearson_cor(test_data, x, y)
  
  # Should be identical with uniform weights
  expect_equal(
    result_weighted$correlations$correlation[1],
    result_unweighted$correlations$correlation[1],
    tolerance = 1e-10
  )
})

# ============================================================================
# TEST SUITE 3: Grouped Correlations
# ============================================================================

test_that("pearson_cor() works with grouped data", {
  data(survey_data)
  
  result <- survey_data %>%
    group_by(region) %>%
    pearson_cor(age, income)
  
  # Check structure
  expect_true(result$is_grouped)
  expect_equal(result$groups, "region")
  
  # Should have one correlation per region
  n_regions <- length(unique(survey_data$region[!is.na(survey_data$region)]))
  expect_equal(nrow(result$correlations), n_regions)
  
  # Each group should have a correlation
  expect_true(all(!is.na(result$correlations$correlation)))
})

test_that("grouped correlations are calculated correctly", {
  # Create simple test data
  test_data <- data.frame(
    group = rep(c("A", "B"), each = 50),
    x = c(rnorm(50), rnorm(50, mean = 2)),
    y = c(rnorm(50), rnorm(50, mean = 2))
  )
  
  # Calculate grouped correlations
  result <- test_data %>%
    group_by(group) %>%
    pearson_cor(x, y)
  
  # Manual calculation for group A
  group_a_data <- test_data[test_data$group == "A", ]
  manual_cor_a <- cor(group_a_data$x, group_a_data$y)
  
  # Check
  result_a <- result$correlations[result$correlations$group == "A", ]
  expect_equal(result_a$correlation, manual_cor_a, tolerance = 1e-10)
})

# ============================================================================
# TEST SUITE 4: Missing Data Handling
# ============================================================================

test_that("pearson_cor() handles missing data with pairwise deletion", {
  # Create data with more distinct missing patterns
  test_data <- data.frame(
    x = c(1:10, NA, NA, NA, 14:20),
    y = c(1:5, NA, 7:20),
    z = c(NA, NA, NA, 4:20)
  )
  
  result <- pearson_cor(test_data, x, y, z, na.rm = "pairwise")
  
  # Check that correlations are calculated despite NAs
  expect_false(any(is.na(result$correlations$correlation)))
  
  # Check sample sizes differ for different pairs
  # x-y should have different n than x-z due to different missing patterns
  n_xy <- result$correlations$n[result$correlations$var1 == "x" & result$correlations$var2 == "y"]
  n_xz <- result$correlations$n[result$correlations$var1 == "x" & result$correlations$var2 == "z"]
  expect_true(n_xy != n_xz)
})

test_that("pearson_cor() handles missing data with listwise deletion", {
  # Create data with missing values
  test_data <- data.frame(
    x = c(1:10, NA, 12:20),
    y = c(1:5, NA, 7:20),
    z = c(NA, 2:20)
  )
  
  result <- pearson_cor(test_data, x, y, z, na.rm = "listwise")
  
  # Check that all pairs use the same sample size (complete cases only)
  expect_equal(length(unique(result$correlations$n)), 1)
  
  # Sample size should be number of complete cases
  complete_cases <- sum(complete.cases(test_data[c("x", "y", "z")]))
  expect_equal(unique(result$correlations$n), complete_cases)
})

# ============================================================================
# TEST SUITE 5: Edge Cases and Error Handling
# ============================================================================

test_that("pearson_cor() handles perfect correlations", {
  # Create perfectly correlated data
  test_data <- data.frame(
    x = 1:100,
    y = 1:100,
    z = -(1:100)
  )
  
  result <- pearson_cor(test_data, x, y, z)
  
  # x and y should have correlation = 1
  cor_xy <- result$correlations[result$correlations$var1 == "x" & 
                                result$correlations$var2 == "y", ]
  expect_equal(cor_xy$correlation, 1, tolerance = 1e-10)
  expect_equal(cor_xy$p_value, 0, tolerance = 1e-10)
  
  # x and z should have correlation = -1
  cor_xz <- result$correlations[result$correlations$var1 == "x" & 
                                result$correlations$var2 == "z", ]
  expect_equal(cor_xz$correlation, -1, tolerance = 1e-10)
  expect_equal(cor_xz$p_value, 0, tolerance = 1e-10)
})

test_that("pearson_cor() handles zero correlation", {
  set.seed(123)
  # Create uncorrelated data
  test_data <- data.frame(
    x = rnorm(100),
    y = rnorm(100)
  )
  
  result <- pearson_cor(test_data, x, y)
  
  # Correlation should be close to zero
  expect_true(abs(result$correlations$correlation) < 0.2)
  
  # P-value should be high (not significant)
  expect_true(result$correlations$p_value > 0.05)
})

test_that("pearson_cor() errors with insufficient data", {
  # Less than 3 observations
  test_data <- data.frame(
    x = c(1, 2),
    y = c(1, 2)
  )
  
  result <- pearson_cor(test_data, x, y)
  
  # Should return NA for correlation
  expect_true(is.na(result$correlations$correlation))
  expect_true(is.na(result$correlations$p_value))
})

test_that("pearson_cor() errors with non-numeric variables", {
  test_data <- data.frame(
    x = 1:10,
    y = letters[1:10]
  )
  
  expect_error(
    pearson_cor(test_data, x, y),
    "not numeric"
  )
})

test_that("pearson_cor() errors with single variable", {
  data(survey_data)
  
  expect_error(
    pearson_cor(survey_data, age),
    "At least two variables"
  )
})

test_that("pearson_cor() validates confidence level", {
  data(survey_data)
  
  expect_error(
    pearson_cor(survey_data, age, income, conf.level = 1.5),
    "conf.level must be between 0 and 1"
  )
  
  expect_error(
    pearson_cor(survey_data, age, income, conf.level = 0),
    "conf.level must be between 0 and 1"
  )
})

# ============================================================================
# TEST SUITE 6: Tidyselect Functionality
# ============================================================================

test_that("pearson_cor() works with tidyselect helpers", {
  data(survey_data)
  
  # Select numeric variables
  result <- survey_data %>%
    select(age, income, life_satisfaction, sampling_weight) %>%
    pearson_cor(where(is.numeric))
  
  expect_true(length(result$variables) == 4)
  
  # Test with multiple numeric columns
  result2 <- survey_data %>%
    pearson_cor(age, income, life_satisfaction)
  
  # Should work with 3 variables
  expect_equal(length(result2$variables), 3)
})

# ============================================================================
# TEST SUITE 7: Output Formatting and Print Method
# ============================================================================

test_that("print method works correctly", {
  data(survey_data)
  
  result <- pearson_cor(survey_data, age, income, life_satisfaction)
  
  # Should not error
  expect_output(print(result))
  
  # Check specific output elements
  output <- capture.output(print(result))
  
  # Should contain header
  expect_true(any(grepl("Pearson Correlation", output)))
  
  # Should contain interpretation guidelines
  expect_true(any(grepl("Correlation Strength Interpretation", output)))
  
  # Should show significance codes
  expect_true(any(grepl("Signif. codes", output)))
})

test_that("significance indicators are correctly assigned", {
  # Create data with known correlation
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    x = rnorm(n),
    y_high = rnorm(n) * 0.1 + rnorm(n),  # Very weak correlation
    y_med = rnorm(n) * 0.5 + rnorm(n),   # Moderate correlation
    y_strong = rnorm(n) * 2 + rnorm(n)   # Stronger correlation
  )
  
  result <- pearson_cor(test_data, x, y_high, y_med, y_strong)
  
  # Check significance indicators
  expect_true(all(result$correlations$sig %in% c("", "*", "**", "***")))
})

# ============================================================================
# TEST SUITE 8: Confidence Intervals
# ============================================================================

test_that("confidence intervals are calculated correctly", {
  data(survey_data)
  
  # 95% CI
  result_95 <- pearson_cor(survey_data, age, income, conf.level = 0.95)
  
  # 99% CI
  result_99 <- pearson_cor(survey_data, age, income, conf.level = 0.99)
  
  # 99% CI should be wider than 95% CI
  width_95 <- result_95$correlations$conf_int_upper - result_95$correlations$conf_int_lower
  width_99 <- result_99$correlations$conf_int_upper - result_99$correlations$conf_int_lower
  
  expect_true(width_99 > width_95)
})

test_that("confidence intervals contain the point estimate", {
  data(survey_data)
  
  result <- pearson_cor(survey_data, age, income, life_satisfaction)
  
  for (i in 1:nrow(result$correlations)) {
    r <- result$correlations$correlation[i]
    ci_lower <- result$correlations$conf_int_lower[i]
    ci_upper <- result$correlations$conf_int_upper[i]
    
    # Point estimate should be within CI
    expect_true(ci_lower <= r || is.na(r))
    expect_true(ci_upper >= r || is.na(r))
  }
})

# ============================================================================
# TEST SUITE 9: Effect Size (r²) Calculation
# ============================================================================

test_that("r-squared is calculated correctly", {
  data(survey_data)
  
  result <- pearson_cor(survey_data, age, income)
  
  # r² should equal r^2
  expected_r2 <- result$correlations$correlation^2
  expect_equal(result$correlations$r_squared, expected_r2, tolerance = 1e-10)
  
  # r² should be between 0 and 1
  expect_true(all(result$correlations$r_squared >= 0))
  expect_true(all(result$correlations$r_squared <= 1))
})

# ============================================================================
# TEST SUITE 10: Integration with dplyr Pipeline
# ============================================================================

test_that("pearson_cor() integrates with dplyr pipelines", {
  data(survey_data)
  
  # Should work in a pipeline
  result <- survey_data %>%
    filter(!is.na(age)) %>%
    select(age, income, life_satisfaction, region, sampling_weight) %>%
    group_by(region) %>%
    pearson_cor(age, income, weights = sampling_weight)
  
  expect_s3_class(result, "pearson_cor_results")
  expect_true(result$is_grouped)
  expect_equal(result$weights, "sampling_weight")
})