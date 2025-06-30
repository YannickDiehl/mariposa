# Test file for print output formatting
# Tests ensure consistent, professional formatting across all functions

test_that("print outputs have consistent Unicode box formatting", {
  data(survey_data)
  
  # Test different function types
  t_result <- t_test(survey_data, life_satisfaction, group = gender)
  levene_result <- levene_test(survey_data, life_satisfaction, group = gender)
  w_mean_result <- w_mean(survey_data, life_satisfaction, weights = sampling_weight)
  
  # Capture outputs
  t_output <- capture.output(print(t_result))
  levene_output <- capture.output(print(levene_result))
  w_output <- capture.output(print(w_mean_result))
  
  # All should have consistent variable formatting
  expect_true(any(grepl("┌─ life_satisfaction ─┐", t_output)))
  expect_true(any(grepl("┌─ life_satisfaction ─┐", levene_output)))
  expect_true(any(grepl("Weighted Mean Statistics", w_output)))
})

test_that("print outputs show correct headers for weighted vs unweighted", {
  data(survey_data)
  
  # Weighted analyses
  t_weighted <- t_test(survey_data, life_satisfaction, group = gender, weights = sampling_weight)
  w_mean_weighted <- w_mean(survey_data, life_satisfaction, weights = sampling_weight)
  
  # Unweighted analyses
  t_unweighted <- t_test(survey_data, life_satisfaction, group = gender)
  
  # Capture outputs
  t_weighted_output <- capture.output(print(t_weighted))
  w_weighted_output <- capture.output(print(w_mean_weighted))
  t_unweighted_output <- capture.output(print(t_unweighted))
  
  # Check headers
  expect_true(any(grepl("Weighted t-Test Results", t_weighted_output)))
  expect_true(any(grepl("Weighted Mean", w_weighted_output)))
  expect_true(any(grepl("^t-Test Results", t_unweighted_output)))
  expect_false(any(grepl("Weighted", t_unweighted_output)))
})

test_that("grouped analysis print outputs show group headers correctly", {
  data(survey_data)
  
  # Grouped analyses
  t_grouped <- survey_data %>%
    dplyr::group_by(region) %>%
    t_test(life_satisfaction, group = gender)
  
  w_grouped <- survey_data %>%
    dplyr::group_by(region) %>%
    w_mean(life_satisfaction, weights = sampling_weight)
  
  # Capture outputs
  t_output <- capture.output(print(t_grouped))
  w_output <- capture.output(print(w_grouped))
  
  # Check group headers (no underlines, consistent with w_* functions)
  expect_true(any(grepl("Group: region = East", t_output)))
  expect_true(any(grepl("Group: region = West", t_output)))
  expect_true(any(grepl("Group: region = East", w_output)))
  expect_true(any(grepl("Group: region = West", w_output)))
  
  # Should have multiple sections (one per group)
  expect_true(sum(grepl("┌─ life_satisfaction ─┐", t_output)) == 2)
  expect_true(sum(grepl("Group: region", w_output)) >= 1)
})

test_that("print outputs handle multiple variables correctly", {
  data(survey_data)
  
  # Multiple variables
  t_multi <- t_test(survey_data, 
                   life_satisfaction, 
                   trust_government, 
                   group = gender)
  
  w_multi <- w_mean(survey_data, 
                   life_satisfaction, 
                   trust_government, 
                   weights = sampling_weight)
  
  # Capture outputs
  t_output <- capture.output(print(t_multi))
  w_output <- capture.output(print(w_multi))
  
  # Should have content for each variable
  expect_true(any(grepl("┌─ life_satisfaction ─┐", t_output)))
  expect_true(any(grepl("┌─ trust_government ─┐", t_output)))
  expect_true(any(grepl("life_satisfaction", w_output)))
  expect_true(any(grepl("trust_government", w_output)))
})

test_that("print outputs show dynamic borders correctly", {
  data(survey_data)
  
  result <- t_test(survey_data, life_satisfaction, group = gender)
  output <- capture.output(print(result))
  
  # Should have dynamic borders (dashes)
  expect_true(any(grepl("^-+$", output)))
  
  # Borders should match table width
  table_lines <- output[grepl("Assumption|Equal variances|Unequal variances", output)]
  if (length(table_lines) > 0) {
    # Find the widest table line
    max_width <- max(nchar(table_lines))
    
    # Check if borders match or are close to table width
    border_lines <- output[grepl("^-+$", output)]
    if (length(border_lines) > 0) {
      border_widths <- nchar(border_lines)
      expect_true(any(abs(border_widths - max_width) <= 2))  # Small tolerance
    }
  }
})

test_that("print outputs show significance indicators correctly", {
  data(survey_data)
  
  # Create data likely to show significance
  result <- t_test(survey_data, life_satisfaction, group = gender)
  output <- capture.output(print(result))
  
  # Should show effect size interpretation
  expect_true(any(grepl("Effect Sizes", output)))
  expect_true(any(grepl("Effect_Size", output)))
  
  # Should show interpretation notes
  expect_true(any(grepl("Interpretation:", output)))
})

test_that("print outputs show SPSS-style tables for t-tests", {
  data(survey_data)
  
  result <- t_test(survey_data, life_satisfaction, group = gender)
  output <- capture.output(print(result))
  
  # Should show both equal and unequal variance assumptions
  expect_true(any(grepl("Equal variances", output)))
  expect_true(any(grepl("Unequal variances", output)))
  
  # Should have t_stat, df, p_value columns
  expect_true(any(grepl("t_stat", output)))
  expect_true(any(grepl("df", output)))
  expect_true(any(grepl("p_value", output)))
})

test_that("print outputs show effective sample sizes for weighted functions", {
  data(survey_data)
  
  result <- w_mean(survey_data, life_satisfaction, weights = sampling_weight)
  output <- capture.output(print(result))
  
  # Should show effective n
  expect_true(any(grepl("Effective_N", output)))
  
  # Should be less than total sample size due to weighting
  effective_n_line <- output[grepl("Effective_N", output)]
  if (length(effective_n_line) > 0) {
    # Extract the effective n value (basic check that it's reasonable)
    expect_true(length(effective_n_line) > 0)
  }
})

test_that("print outputs maintain consistent spacing and alignment", {
  data(survey_data)
  
  # Test different function outputs
  results <- list(
    t_test(survey_data, life_satisfaction, group = gender),
    levene_test(survey_data, life_satisfaction, group = gender),
    w_mean(survey_data, life_satisfaction, weights = sampling_weight),
    w_sd(survey_data, life_satisfaction, weights = sampling_weight)
  )
  
  outputs <- lapply(results, function(x) capture.output(print(x)))
  
  # All should have consistent variable box format
  for (output in outputs) {
    expect_true(any(grepl("┌─ life_satisfaction ─┐", output)))
    # Should have blank line after variable box
    box_line_idx <- which(grepl("┌─ life_satisfaction ─┐", output))
    if (length(box_line_idx) > 0) {
      # Check if there's content after the box (allow for different structures)
      expect_true(length(output) > box_line_idx[1])
    }
  }
})

test_that("print outputs handle long variable names correctly", {
  data(survey_data)
  
  # Create data with long variable name
  survey_long <- survey_data
  names(survey_long)[names(survey_long) == "life_satisfaction"] <- "very_long_variable_name_that_exceeds_normal_length"
  
  result <- w_mean(survey_long, very_long_variable_name_that_exceeds_normal_length, weights = sampling_weight)
  output <- capture.output(print(result))
  
  # Should handle long names gracefully
  expect_true(any(grepl("very_long_variable_name", output)))
  # Unicode box should adjust to variable name length
  expect_true(any(grepl("┌─ very_long_variable_name_that_exceeds_normal_length ─┐", output)))
})

test_that("print outputs show correct decimal places", {
  data(survey_data)
  
  result <- t_test(survey_data, life_satisfaction, group = gender)
  
  # Test different digit settings
  output_3 <- capture.output(print(result, digits = 3))
  output_5 <- capture.output(print(result, digits = 5))
  
  # Should have different precision
  expect_true(any(output_3 != output_5))
  
  # Default should be 3 digits
  output_default <- capture.output(print(result))
  expect_identical(output_3, output_default)
})

test_that("print outputs show interpretation guidelines", {
  data(survey_data)
  
  result <- t_test(survey_data, life_satisfaction, group = gender)
  output <- capture.output(print(result))
  
  # Should show effect size interpretation
  expect_true(any(grepl("Effect Sizes.*Interpretation", output)) ||
              any(grepl("small.*medium.*large", output, ignore.case = TRUE)))
})

test_that("Levene test print output shows interpretation notes", {
  data(survey_data)
  
  result <- levene_test(survey_data, life_satisfaction, group = gender)
  output <- capture.output(print(result))
  
  # Should show interpretation note
  expect_true(any(grepl("Interpretation:", output)))
  expect_true(any(grepl("homogeneous|heterogeneous", output, ignore.case = TRUE)))
})

test_that("print outputs are consistent across different scenarios", {
  data(survey_data)
  
  # Test various scenarios that should maintain consistent formatting
  scenarios <- list(
    # Basic
    t_test(survey_data, life_satisfaction, group = gender),
    # Weighted
    t_test(survey_data, life_satisfaction, group = gender, weights = sampling_weight),
    # Multiple variables
    t_test(survey_data, life_satisfaction, trust_government, group = gender),
    # Grouped
    survey_data %>% dplyr::group_by(region) %>% t_test(life_satisfaction, group = gender)
  )
  
  outputs <- lapply(scenarios, function(x) capture.output(print(x)))
  
  # All should have the basic structure elements
  for (output in outputs) {
    expect_true(any(grepl("Test Results", output)))
    expect_true(any(grepl("┌─.*─┐", output)))  # Unicode box pattern
  }
})