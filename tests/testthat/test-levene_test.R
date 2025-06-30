# Test file for levene_test function
# Tests use package datasets for reproducibility

test_that("levene_test basic functionality works with survey_data", {
  data(survey_data)
  
  # Test standalone Levene test
  result <- levene_test(survey_data, life_satisfaction, group = gender)
  
  expect_s3_class(result, "levene_test_results")
  expect_true("results" %in% names(result))
  expect_equal(result$variables, "life_satisfaction")
  expect_equal(result$group, "gender")
  expect_equal(result$center, "mean")  # Default
  
  # Check results structure
  expect_true(all(c("Variable", "F_statistic", "df1", "df2", "p_value") %in% names(result$results)))
  expect_equal(nrow(result$results), 1)
  expect_true(!is.na(result$results$F_statistic[1]))
  expect_true(!is.na(result$results$p_value[1]))
})

test_that("levene_test with median center works", {
  data(survey_data)
  
  # Test with median (more robust)
  result_mean <- levene_test(survey_data, life_satisfaction, group = gender, center = "mean")
  result_median <- levene_test(survey_data, life_satisfaction, group = gender, center = "median")
  
  expect_equal(result_mean$center, "mean")
  expect_equal(result_median$center, "median")
  
  # Results should differ between mean and median centering
  expect_true(result_mean$results$F_statistic[1] != result_median$results$F_statistic[1])
  expect_true(result_mean$results$p_value[1] != result_median$results$p_value[1])
})

test_that("levene_test works with multiple variables", {
  data(survey_data)
  
  # Test multiple variables
  result <- levene_test(survey_data, 
                       life_satisfaction, 
                       trust_government, 
                       trust_media,
                       group = gender)
  
  expect_equal(nrow(result$results), 3)
  expect_equal(result$results$Variable, 
               c("life_satisfaction", "trust_government", "trust_media"))
  
  # All should have valid results
  expect_true(all(!is.na(result$results$F_statistic)))
  expect_true(all(!is.na(result$results$p_value)))
})

test_that("levene_test weighted analysis works", {
  data(survey_data)
  
  # Weighted Levene test
  result_weighted <- levene_test(survey_data, life_satisfaction, 
                                group = gender, weights = sampling_weight)
  
  # Unweighted Levene test
  result_unweighted <- levene_test(survey_data, life_satisfaction, group = gender)
  
  # Check weights are recorded
  expect_equal(result_weighted$weights, "sampling_weight")
  expect_null(result_unweighted$weights)
  
  # Results should differ due to weights
  expect_true(result_weighted$results$F_statistic[1] != result_unweighted$results$F_statistic[1])
})

test_that("levene_test S3 method for t_test_results works", {
  data(survey_data)
  
  # First run t-test
  t_result <- t_test(survey_data, life_satisfaction, group = gender)
  
  # Then apply levene_test
  levene_result <- levene_test(t_result)
  
  expect_s3_class(levene_result, "levene_test_results")
  expect_equal(levene_result$variables, "life_satisfaction")
  expect_equal(levene_result$group, "gender")
  expect_equal(levene_result$original_test$method, t_result$method)
  
  # Should have valid results
  expect_true(!is.na(levene_result$results$F_statistic[1]))
  expect_true(!is.na(levene_result$results$p_value[1]))
})

test_that("levene_test S3 method works with weighted t_test", {
  data(survey_data)
  
  # Weighted t-test
  t_result_weighted <- t_test(survey_data, life_satisfaction, 
                             group = gender, weights = sampling_weight)
  
  # Apply levene_test
  levene_result <- levene_test(t_result_weighted)
  
  expect_equal(levene_result$weights, "sampling_weight")
  expect_true(!is.na(levene_result$results$F_statistic[1]))
})

test_that("levene_test S3 method works with grouped t_test", {
  data(survey_data)
  
  # Grouped t-test
  t_result_grouped <- survey_data %>%
    dplyr::group_by(region) %>%
    t_test(life_satisfaction, group = gender)
  
  # Apply levene_test
  levene_result <- levene_test(t_result_grouped)
  
  expect_s3_class(levene_result, "levene_test_results")
  expect_true(levene_result$is_grouped)
  expect_equal(levene_result$groups, "region")
  
  # Should have results for each region
  expect_equal(nrow(levene_result$results), 2)
  expect_true(all(c("East", "West") %in% levene_result$results$region))
})

test_that("levene_test print output is correctly formatted", {
  data(survey_data)
  
  result <- levene_test(survey_data, life_satisfaction, group = gender)
  
  # Capture print output
  output <- capture.output(print(result))
  
  # Check for key elements
  expect_true(any(grepl("Levene's Test for Homogeneity of Variance", output)))
  expect_true(any(grepl("Center: mean", output)))
  expect_true(any(grepl("┌─ life_satisfaction ─┐", output)))
  expect_true(any(grepl("F_statistic", output)))
  expect_true(any(grepl("p_value", output)))
})

test_that("levene_test print output shows weighted header", {
  data(survey_data)
  
  # Weighted analysis
  result_weighted <- levene_test(survey_data, life_satisfaction, 
                                group = gender, weights = sampling_weight)
  output_weighted <- capture.output(print(result_weighted))
  
  # Unweighted analysis
  result_unweighted <- levene_test(survey_data, life_satisfaction, group = gender)
  output_unweighted <- capture.output(print(result_unweighted))
  
  # Check headers
  expect_true(any(grepl("Weighted Levene's Test", output_weighted)))
  expect_false(any(grepl("Weighted", output_unweighted)))
})

test_that("levene_test handles more than 2 groups", {
  data(survey_data)
  
  # Test with education (multiple levels)
  result <- levene_test(survey_data, income, group = education)
  
  expect_s3_class(result, "levene_test_results")
  expect_true(!is.na(result$results$F_statistic[1]))
  
  # df1 should reflect number of groups - 1
  n_groups <- length(unique(survey_data$education))
  expect_equal(result$results$df1[1], n_groups - 1)
})

test_that("levene_test p-value interpretation is shown", {
  data(survey_data)
  
  result <- levene_test(survey_data, life_satisfaction, group = gender)
  output <- capture.output(print(result))
  
  # Should show interpretation
  expect_true(any(grepl("Interpretation:", output)))
  expect_true(any(grepl("p > 0.05.*homogeneous", output)) || 
              any(grepl("p.*0.05.*heterogeneous", output)))
})

test_that("levene_test handles tidyselect helpers", {
  data(survey_data)
  
  # Test with starts_with()
  result <- levene_test(survey_data, starts_with("trust"), group = gender)
  
  expect_equal(nrow(result$results), 3)  # trust_government, trust_media, trust_science
  expect_true(all(grepl("^trust", result$results$Variable)))
})

test_that("levene_test integrates with pipeline workflow", {
  data(survey_data)
  
  # Complex pipeline: t-test -> levene_test
  pipeline_result <- survey_data %>%
    t_test(life_satisfaction, group = gender, weights = sampling_weight) %>%
    levene_test(center = "median")
  
  expect_s3_class(pipeline_result, "levene_test_results")
  expect_equal(pipeline_result$center, "median")
  expect_equal(pipeline_result$weights, "sampling_weight")
})

test_that("levene_test grouped analysis works correctly", {
  data(survey_data)
  
  # Direct grouped analysis
  result_grouped <- survey_data %>%
    dplyr::group_by(region) %>%
    levene_test(life_satisfaction, group = gender)
  
  expect_s3_class(result_grouped, "levene_test_results")
  expect_true(result_grouped$is_grouped)
  expect_equal(nrow(result_grouped$results), 2)
  
  # Check print output for grouped results
  output <- capture.output(print(result_grouped))
  expect_true(any(grepl("Group: region = East", output)))
  expect_true(any(grepl("Group: region = West", output)))
})

test_that("levene_test handles missing data appropriately", {
  data(survey_data)
  
  # Create data with some NA values
  survey_data_na <- survey_data
  survey_data_na$life_satisfaction[c(1, 5, 10)] <- NA
  
  # Should still work with NAs
  result <- levene_test(survey_data_na, life_satisfaction, group = gender)
  
  expect_s3_class(result, "levene_test_results")
  expect_true(!is.na(result$results$F_statistic[1]))
})