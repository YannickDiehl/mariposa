# Test file for t_test function
# Tests use package datasets for reproducibility

test_that("t_test basic functionality works with survey_data", {
  # Load package data
  data(survey_data)
  
  # Test basic two-sample t-test
  result <- t_test(survey_data, life_satisfaction, group = gender)
  
  expect_s3_class(result, "t_test_results")
  expect_true("results" %in% names(result))
  expect_true("variables" %in% names(result))
  expect_equal(result$variables, "life_satisfaction")
  expect_equal(result$group, "gender")
  expect_false(result$var.equal)
  
  # Check that we have results
  expect_equal(nrow(result$results), 1)
  expect_true(!is.na(result$results$t_stat[1]))
  expect_true(!is.na(result$results$p_value[1]))
})

test_that("t_test weighted analysis works with survey_data", {
  data(survey_data)
  
  # Weighted t-test
  result_weighted <- t_test(survey_data, life_satisfaction, group = gender, weights = sampling_weight)
  
  # Unweighted t-test
  result_unweighted <- t_test(survey_data, life_satisfaction, group = gender)
  
  # Results should differ due to weights
  expect_true(result_weighted$results$t_stat[1] != result_unweighted$results$t_stat[1])
  expect_true(!is.null(result_weighted$weights))
  expect_equal(result_weighted$weights, "sampling_weight")
  
  # Weighted results should have different degrees of freedom
  expect_true(result_weighted$results$df[1] != result_unweighted$results$df[1])
})

test_that("t_test handles grouped data correctly with survey_data", {
  data(survey_data)
  
  # Test with grouped data by region
  result_grouped <- survey_data %>%
    dplyr::group_by(region) %>%
    t_test(life_satisfaction, group = gender)
  
  expect_s3_class(result_grouped, "t_test_results")
  expect_true(result_grouped$is_grouped)
  expect_equal(result_grouped$groups, "region")
  
  # Should have results for both regions (East and West)
  expect_equal(nrow(result_grouped$results), 2)
  expect_true(all(c("East", "West") %in% result_grouped$results$region))
  
  # Each region should have valid results
  expect_true(all(!is.na(result_grouped$results$t_stat)))
  expect_true(all(!is.na(result_grouped$results$p_value)))
})

test_that("t_test handles multiple variables with survey_data", {
  data(survey_data)
  
  # Test multiple outcome variables
  result <- t_test(survey_data, 
                   life_satisfaction, 
                   trust_government, 
                   trust_media, 
                   group = gender)
  
  expect_equal(length(result$variables), 3)
  expect_equal(nrow(result$results), 3)
  expect_equal(result$results$Variable, 
               c("life_satisfaction", "trust_government", "trust_media"))
  
  # All should have valid results
  expect_true(all(!is.na(result$results$t_stat)))
  expect_true(all(!is.na(result$results$p_value)))
})

test_that("t_test print output is correctly formatted", {
  data(survey_data)
  
  result <- t_test(survey_data, life_satisfaction, group = gender)
  
  # Capture print output
  output <- capture.output(print(result))
  
  # Check for key elements in output
  expect_true(any(grepl("t-Test Results", output)))
  expect_true(any(grepl("┌─ life_satisfaction ─┐", output)))
  expect_true(any(grepl("Assumption", output)))
  expect_true(any(grepl("Equal variances", output)))
  expect_true(any(grepl("Unequal variances", output)))
  expect_true(any(grepl("Effect Sizes", output)))
  expect_true(any(grepl("Cohen's d", output)))
  expect_true(any(grepl("Hedges' g", output)))
  expect_true(any(grepl("Glass' Delta", output)))
})

test_that("t_test print output shows weighted header correctly", {
  data(survey_data)
  
  # Weighted analysis
  result_weighted <- t_test(survey_data, life_satisfaction, group = gender, weights = sampling_weight)
  output_weighted <- capture.output(print(result_weighted))
  
  # Unweighted analysis
  result_unweighted <- t_test(survey_data, life_satisfaction, group = gender)
  output_unweighted <- capture.output(print(result_unweighted))
  
  # Check headers
  expect_true(any(grepl("Weighted t-Test Results", output_weighted)))
  expect_true(any(grepl("^t-Test Results", output_unweighted)))
  expect_false(any(grepl("Weighted", output_unweighted)))
})

test_that("t_test grouped analysis print output is correct", {
  data(survey_data)
  
  result_grouped <- survey_data %>%
    dplyr::group_by(region) %>%
    t_test(life_satisfaction, group = gender)
  
  output <- capture.output(print(result_grouped))
  
  # Check for group headers
  expect_true(any(grepl("Group: region = East", output)))
  expect_true(any(grepl("Group: region = West", output)))
  
  # Each group should have its own results table
  expect_true(sum(grepl("┌─ life_satisfaction ─┐", output)) == 2)
})

test_that("t_test effect sizes are reasonable for survey_data", {
  data(survey_data)
  
  result <- t_test(survey_data, life_satisfaction, group = gender)
  
  # Check effect sizes exist
  expect_true(all(c("cohens_d", "hedges_g", "glass_delta") %in% names(result$results)))
  
  # Effect sizes should be reasonable (not extreme)
  expect_true(abs(result$results$cohens_d[1]) < 3)  # Very large effects are rare
  expect_true(abs(result$results$hedges_g[1]) < 3)
  expect_true(abs(result$results$glass_delta[1]) < 3)
  
  # Hedges' g should be slightly smaller in magnitude than Cohen's d (bias correction)
  expect_true(abs(result$results$hedges_g[1]) <= abs(result$results$cohens_d[1]))
})

test_that("t_test handles different variable types with survey_data", {
  data(survey_data)
  
  # Test with different education levels (ordinal variable)
  result <- t_test(survey_data, income, group = gender)
  
  expect_s3_class(result, "t_test_results")
  expect_equal(result$variables, "income")
  expect_true(!is.na(result$results$t_stat[1]))
})

test_that("t_test confidence intervals change with conf.level", {
  data(survey_data)
  
  # Test different confidence levels
  result_95 <- t_test(survey_data, life_satisfaction, group = gender, conf.level = 0.95)
  result_99 <- t_test(survey_data, life_satisfaction, group = gender, conf.level = 0.99)
  
  # 99% CI should be wider than 95% CI
  ci_width_95 <- result_95$results$CI_upper[1] - result_95$results$CI_lower[1]
  ci_width_99 <- result_99$results$CI_upper[1] - result_99$results$CI_lower[1]
  
  expect_true(ci_width_99 > ci_width_95)
  expect_equal(result_95$conf.level, 0.95)
  expect_equal(result_99$conf.level, 0.99)
})

test_that("t_test alternative hypotheses work correctly", {
  data(survey_data)
  
  # Test all three alternatives
  result_two <- t_test(survey_data, life_satisfaction, group = gender, alternative = "two.sided")
  result_less <- t_test(survey_data, life_satisfaction, group = gender, alternative = "less")
  result_greater <- t_test(survey_data, life_satisfaction, group = gender, alternative = "greater")
  
  # Check alternatives are recorded correctly
  expect_equal(result_two$alternative, "two.sided")
  expect_equal(result_less$alternative, "less")
  expect_equal(result_greater$alternative, "greater")
  
  # p-values should differ between alternatives
  expect_true(result_two$results$p_value[1] != result_less$results$p_value[1])
  expect_true(result_two$results$p_value[1] != result_greater$results$p_value[1])
  
  # Two-sided p-value should be approximately twice one of the one-sided p-values
  # (depending on which side the effect is on)
  min_one_sided <- min(result_less$results$p_value[1], result_greater$results$p_value[1])
  expect_equal(result_two$results$p_value[1], 2 * min_one_sided, tolerance = 0.001)
})

test_that("t_test variance assumptions affect results", {
  data(survey_data)
  
  # Test with equal and unequal variance assumptions
  result_welch <- t_test(survey_data, life_satisfaction, group = gender, var.equal = FALSE)
  result_student <- t_test(survey_data, life_satisfaction, group = gender, var.equal = TRUE)
  
  # Results should differ slightly
  expect_true(result_welch$results$t_stat[1] != result_student$results$t_stat[1])
  expect_true(result_welch$results$df[1] != result_student$results$df[1])
  
  # Student's t-test should have integer df (n1 + n2 - 2)
  # Welch's t-test typically has non-integer df
  expect_true(result_student$results$df[1] %% 1 == 0)  # Integer check
})

test_that("t_test with tidyselect helpers works", {
  data(survey_data)
  
  # Test with starts_with()
  result <- t_test(survey_data, starts_with("trust"), group = gender)
  
  expect_equal(length(result$variables), 3)  # trust_government, trust_media, trust_science
  expect_true(all(grepl("^trust", result$results$Variable)))
})

test_that("t_test handles missing data appropriately", {
  data(survey_data)
  
  # Create data with some NA values
  survey_data_na <- survey_data
  survey_data_na$life_satisfaction[c(1, 5, 10)] <- NA
  
  # Should still work with NAs (removing them)
  result <- t_test(survey_data_na, life_satisfaction, group = gender)
  
  expect_s3_class(result, "t_test_results")
  expect_true(!is.na(result$results$t_stat[1]))
  
  # Sample size should be reduced
  n_complete <- sum(!is.na(survey_data_na$life_satisfaction))
  expect_true(result$results$n1[1] + result$results$n2[1] == n_complete)
})

test_that("t_test integrates with levene_test", {
  data(survey_data)
  
  # Run t-test then pipe to levene_test
  result_t <- t_test(survey_data, life_satisfaction, group = gender)
  result_levene <- result_t %>% levene_test()
  
  expect_s3_class(result_levene, "levene_test_results")
  expect_equal(result_levene$variables, "life_satisfaction")
  expect_equal(result_levene$group, "gender")
  
  # Check Levene test has results
  expect_true(!is.na(result_levene$results$F_statistic[1]))
  expect_true(!is.na(result_levene$results$p_value[1]))
})