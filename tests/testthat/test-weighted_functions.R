# Test file for weighted statistical functions (w_*)
# Tests use package datasets for reproducibility

test_that("w_mean works correctly with survey_data", {
  data(survey_data)
  
  # Test weighted mean
  result <- w_mean(survey_data, life_satisfaction, weights = sampling_weight)
  
  expect_s3_class(result, "w_mean")
  expect_true("results" %in% names(result))
  expect_equal(result$variables, "life_satisfaction")
  expect_equal(result$weight_var, "sampling_weight")
  
  # Should have valid results
  expect_true(!is.na(result$results$weighted_mean[1]))
  expect_true(result$results$weighted_mean[1] > 0)
  
  # Compare with unweighted mean - should differ
  unweighted_mean <- mean(survey_data$life_satisfaction, na.rm = TRUE)
  expect_true(result$results$weighted_mean[1] != unweighted_mean)
})

test_that("w_sd works correctly with survey_data", {
  data(survey_data)
  
  # Test weighted standard deviation
  result <- w_sd(survey_data, life_satisfaction, weights = sampling_weight)
  
  expect_s3_class(result, "w_sd")
  expect_equal(result$variables, "life_satisfaction")
  expect_true(!is.na(result$results$weighted_sd[1]))
  expect_true(result$results$weighted_sd[1] > 0)
  
  # Compare with unweighted SD
  unweighted_sd <- sd(survey_data$life_satisfaction, na.rm = TRUE)
  expect_true(result$results$weighted_sd[1] != unweighted_sd)
})

test_that("w_var works correctly with survey_data", {
  data(survey_data)
  
  # Test weighted variance
  result <- w_var(survey_data, income, weights = sampling_weight)
  
  expect_s3_class(result, "w_var")
  expect_true(!is.na(result$results$weighted_var[1]))
  expect_true(result$results$weighted_var[1] > 0)
  
  # Variance should be square of standard deviation
  sd_result <- w_sd(survey_data, income, weights = sampling_weight)
  expect_equal(result$results$weighted_var[1], 
               sd_result$results$weighted_sd[1]^2, 
               tolerance = 0.001)
})

test_that("w_median works correctly with survey_data", {
  data(survey_data)
  
  # Test weighted median
  result <- w_median(survey_data, age, weights = sampling_weight)
  
  expect_s3_class(result, "w_median")
  expect_true(!is.na(result$results$weighted_median[1]))
  
  # Median should be reasonable
  expect_true(result$results$weighted_median[1] >= min(survey_data$age, na.rm = TRUE))
  expect_true(result$results$weighted_median[1] <= max(survey_data$age, na.rm = TRUE))
})

test_that("w_quantile works correctly with survey_data", {
  data(survey_data)
  
  # Test weighted quantiles (default: quartiles)
  result <- w_quantile(survey_data, income, weights = sampling_weight)
  
  expect_s3_class(result, "w_quantile")
  expect_true(ncol(result$results) >= 4)  # Should have multiple quantiles
  
  # Check quantile values are ordered
  quantile_cols <- grep("^Q", names(result$results), value = TRUE)
  if (length(quantile_cols) > 1) {
    q_values <- as.numeric(result$results[1, quantile_cols])
    expect_true(all(diff(q_values) >= 0))  # Should be non-decreasing
  }
})

test_that("w_iqr works correctly with survey_data", {
  data(survey_data)
  
  # Test weighted IQR
  result <- w_iqr(survey_data, income, weights = sampling_weight)
  
  expect_s3_class(result, "w_iqr")
  expect_true(!is.na(result$results$weighted_iqr[1]))
  expect_true(result$results$weighted_iqr[1] > 0)
  
  # IQR should be reasonable relative to range
  income_range <- max(survey_data$income, na.rm = TRUE) - min(survey_data$income, na.rm = TRUE)
  expect_true(result$results$weighted_iqr[1] <= income_range)
})

test_that("w_range works correctly with survey_data", {
  data(survey_data)
  
  # Test weighted range
  result <- w_range(survey_data, age, weights = sampling_weight)
  
  expect_s3_class(result, "w_range")
  expect_true(!is.na(result$results$weighted_range[1]))
  expect_true(result$results$weighted_range[1] > 0)
  
  # Range should not exceed actual data range
  actual_range <- max(survey_data$age, na.rm = TRUE) - min(survey_data$age, na.rm = TRUE)
  expect_true(result$results$weighted_range[1] <= actual_range + 0.001)  # Small tolerance
})

test_that("w_skew works correctly with survey_data", {
  data(survey_data)
  
  # Test weighted skewness
  result <- w_skew(survey_data, income, weights = sampling_weight)
  
  expect_s3_class(result, "w_skew")
  expect_true(!is.na(result$results$weighted_skew[1]))
  
  # Skewness should be reasonable (typically between -3 and 3)
  expect_true(abs(result$results$weighted_skew[1]) < 10)
})

test_that("w_kurtosis works correctly with survey_data", {
  data(survey_data)
  
  # Test weighted kurtosis
  result <- w_kurtosis(survey_data, life_satisfaction, weights = sampling_weight)
  
  expect_s3_class(result, "w_kurtosis")
  expect_true(!is.na(result$results$weighted_kurtosis[1]))
  
  # Kurtosis should be reasonable
  expect_true(abs(result$results$weighted_kurtosis[1]) < 20)
})

test_that("w_se works correctly with survey_data", {
  data(survey_data)
  
  # Test weighted standard error
  result <- w_se(survey_data, life_satisfaction, weights = sampling_weight)
  
  expect_s3_class(result, "w_se")
  expect_true(!is.na(result$results$weighted_se[1]))
  expect_true(result$results$weighted_se[1] > 0)
  
  # SE should be smaller than SD
  sd_result <- w_sd(survey_data, life_satisfaction, weights = sampling_weight)
  expect_true(result$results$weighted_se[1] < sd_result$results$weighted_sd[1])
})

test_that("w_modus works correctly with survey_data", {
  data(survey_data)
  
  # Test weighted mode with discrete variable
  result <- w_modus(survey_data, trust_government, weights = sampling_weight)
  
  expect_s3_class(result, "w_modus")
  expect_true(!is.na(result$results$weighted_mode[1]))
  
  # Mode should be within valid range
  expect_true(result$results$weighted_mode[1] %in% survey_data$trust_government)
})

test_that("weighted functions handle multiple variables", {
  data(survey_data)
  
  # Test multiple variables
  result <- w_mean(survey_data, 
                   life_satisfaction, 
                   trust_government, 
                   trust_media,
                   weights = sampling_weight)
  
  expect_equal(nrow(result$results), 3)
  expect_equal(result$results$Variable, 
               c("life_satisfaction", "trust_government", "trust_media"))
  
  # All should have valid results
  expect_true(all(!is.na(result$results$weighted_mean)))
})

test_that("weighted functions work with grouped data", {
  data(survey_data)
  
  # Test grouped analysis
  result <- survey_data %>%
    dplyr::group_by(region) %>%
    w_mean(life_satisfaction, weights = sampling_weight)
  
  expect_s3_class(result, "w_mean")
  expect_true(result$is_grouped)
  expect_equal(result$groups, "region")
  
  # Should have results for both regions
  expect_equal(nrow(result$results), 2)
  expect_true(all(c("East", "West") %in% result$results$region))
})

test_that("weighted functions print output is correctly formatted", {
  data(survey_data)
  
  result <- w_mean(survey_data, life_satisfaction, weights = sampling_weight)
  
  # Capture print output
  output <- capture.output(print(result))
  
  # Check for key elements
  expect_true(any(grepl("Weighted Mean", output)))
  expect_true(any(grepl("┌─ life_satisfaction ─┐", output)))
  expect_true(any(grepl("weighted_mean", output)))
  expect_true(any(grepl("effective_n", output)))
})

test_that("weighted functions show effective sample size", {
  data(survey_data)
  
  result <- w_mean(survey_data, life_satisfaction, weights = sampling_weight)
  
  # Should calculate effective sample size
  expect_true("effective_n" %in% names(result$results))
  expect_true(!is.na(result$results$effective_n[1]))
  expect_true(result$results$effective_n[1] > 0)
  expect_true(result$results$effective_n[1] <= nrow(survey_data))
})

test_that("weighted functions handle edge cases", {
  data(survey_data)
  
  # Test with uniform weights (should approximate unweighted)
  uniform_weights <- rep(1, nrow(survey_data))
  survey_data$uniform_weight <- uniform_weights
  
  result_weighted <- w_mean(survey_data, life_satisfaction, weights = uniform_weight)
  unweighted_mean <- mean(survey_data$life_satisfaction, na.rm = TRUE)
  
  # Should be very close
  expect_equal(result_weighted$results$weighted_mean[1], unweighted_mean, tolerance = 0.01)
})

test_that("weighted functions handle missing data", {
  data(survey_data)
  
  # Create data with missing values
  survey_data_na <- survey_data
  survey_data_na$life_satisfaction[c(1, 5, 10)] <- NA
  survey_data_na$sampling_weight[c(2, 7)] <- NA
  
  # Should still work
  result <- w_mean(survey_data_na, life_satisfaction, weights = sampling_weight)
  
  expect_s3_class(result, "w_mean")
  expect_true(!is.na(result$results$weighted_mean[1]))
  
  # Effective n should reflect complete cases
  expect_true(result$results$effective_n[1] < nrow(survey_data))
})

test_that("weighted functions work with tidyselect helpers", {
  data(survey_data)
  
  # Test with starts_with()
  result <- w_mean(survey_data, starts_with("trust"), weights = sampling_weight)
  
  expect_equal(nrow(result$results), 3)  # trust_government, trust_media, trust_science
  expect_true(all(grepl("^trust", result$results$Variable)))
})

test_that("weighted functions mathematical relationships hold", {
  data(survey_data)
  
  # Test variance = sd^2
  var_result <- w_var(survey_data, income, weights = sampling_weight)
  sd_result <- w_sd(survey_data, income, weights = sampling_weight)
  
  expect_equal(var_result$results$weighted_var[1], 
               sd_result$results$weighted_sd[1]^2, 
               tolerance = 0.001)
  
  # Test that SE < SD
  se_result <- w_se(survey_data, income, weights = sampling_weight)
  expect_true(se_result$results$weighted_se[1] < sd_result$results$weighted_sd[1])
})