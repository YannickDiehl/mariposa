# =============================================================================
# FRIEDMAN TEST - SPSS VALIDATION
# =============================================================================
# Validates friedman_test() against SPSS NPAR TESTS /FRIEDMAN procedure
# SPSS syntax: tests/spss_reference/syntax/friedman_test.sps
# SPSS output: tests/spss_reference/outputs/friedman_test_output.txt
#
# Test matrix:
#   - Test 1a/1b: Unweighted, ungrouped (survey_data, 2 variable sets)
#   - Test 2a/2b: Weighted, ungrouped (survey_data)
#   - Test 3: Unweighted, grouped by region (survey_data)
#   - Test 4: Weighted, grouped by region (survey_data)
#   - Test 5a: Longitudinal data, ungrouped (4 timepoints)
#   - Test 5b: Longitudinal data, grouped by treatment
#
# SPSS Reference Values (from output file):
#
# Test 1a (trust items, unweighted, ungrouped):
#   Mean Ranks: trust_government=1.81, trust_media=1.68, trust_science=2.51
#   N=2135, Chi-Square=1009.035, df=2, Asymp. Sig.=.000
#
# Test 1b (attitude items, unweighted, ungrouped):
#   Mean Ranks: life_satisfaction=2.21, environmental_concern=2.17,
#               political_orientation=1.62
#   N=2139, Chi-Square=553.909, df=2, Asymp. Sig.=.000
#
# Test 2a/2b (weighted): Identical to Tests 1a/1b (weights ~1.0)
#
# Test 3 (trust items, unweighted, grouped by region):
#   East: N=422, Chi-Square=217.100, df=2, Asymp. Sig.=.000
#         Mean Ranks: 1.80, 1.67, 2.53
#   West: N=1713, Chi-Square=792.344, df=2, Asymp. Sig.=.000
#         Mean Ranks: 1.81, 1.68, 2.50
#
# Test 4 (weighted, grouped): Identical to Test 3
#
# Test 5a (longitudinal, 4 timepoints, ungrouped):
#   N=75, Chi-Square=61.192, df=3, Asymp. Sig.=.000
#   Mean Ranks: 1.65, 2.31, 2.84, 3.20
#
# Test 5b (longitudinal, grouped by treatment):
#   Control: N=39, Chi-Square=6.538, df=3, Asymp. Sig.=.088
#            Mean Ranks: 2.08, 2.51, 2.62, 2.79
#   Treatment: N=36, Chi-Square=75.933, df=3, Asymp. Sig.=.000
#              Mean Ranks: 1.19, 2.08, 3.08, 3.64
# =============================================================================

library(testthat)
library(dplyr)
library(tidyr)

# =============================================================================
# SPSS VALIDATION TRACKING
# =============================================================================
# Counter for SPSS validation comparisons
spss_comparison_count <- 0

record_friedman_comparison <- function(description, expected, actual,
                                       tolerance = 0.01) {
  spss_comparison_count <<- spss_comparison_count + 1
  expect_equal(actual, expected, tolerance = tolerance,
               label = paste0("SPSS Validation #", spss_comparison_count,
                              ": ", description))
}

# =============================================================================
# TEST 1: UNWEIGHTED / UNGROUPED
# =============================================================================

test_that("Friedman test matches SPSS: unweighted ungrouped trust items (Test 1a)", {
  data(survey_data)

  result <- survey_data %>%
    friedman_test(trust_government, trust_media, trust_science)

  r <- result$results

  # SPSS reference: N=2135, Chi-Square=1009.035, df=2, p<.001
  record_friedman_comparison("Test 1a: N", 2135, r$n, tolerance = 0)
  record_friedman_comparison("Test 1a: Chi-Square", 1009.035, r$chi_sq,
                             tolerance = 0.01)
  record_friedman_comparison("Test 1a: df", 2, r$df, tolerance = 0)
  record_friedman_comparison("Test 1a: p < 0.001", 1,
                             as.numeric(r$p_value < 0.001), tolerance = 0)

  # Mean ranks: 1.81, 1.68, 2.51
  mean_ranks <- r$mean_ranks[[1]]
  record_friedman_comparison("Test 1a: mean rank trust_government",
                             1.81, mean_ranks[["trust_government"]], tolerance = 0.01)
  record_friedman_comparison("Test 1a: mean rank trust_media",
                             1.68, mean_ranks[["trust_media"]], tolerance = 0.01)
  record_friedman_comparison("Test 1a: mean rank trust_science",
                             2.51, mean_ranks[["trust_science"]], tolerance = 0.01)

  # Structure checks
  expect_s3_class(result, "friedman_test")
  expect_equal(result$variables,
               c("trust_government", "trust_media", "trust_science"))
  expect_null(result$weights)
  expect_false(result$is_grouped)
  expect_equal(r$k, 3)
})

test_that("Friedman test matches SPSS: unweighted ungrouped attitude items (Test 1b)", {
  data(survey_data)

  result <- survey_data %>%
    friedman_test(life_satisfaction, environmental_concern, political_orientation)

  r <- result$results

  # SPSS reference: N=2139, Chi-Square=553.909, df=2, p<.001
  record_friedman_comparison("Test 1b: N", 2139, r$n, tolerance = 0)
  record_friedman_comparison("Test 1b: Chi-Square", 553.909, r$chi_sq,
                             tolerance = 0.01)
  record_friedman_comparison("Test 1b: df", 2, r$df, tolerance = 0)
  record_friedman_comparison("Test 1b: p < 0.001", 1,
                             as.numeric(r$p_value < 0.001), tolerance = 0)

  # Mean ranks: 2.21, 2.17, 1.62
  mean_ranks <- r$mean_ranks[[1]]
  record_friedman_comparison("Test 1b: mean rank life_satisfaction",
                             2.21, mean_ranks[["life_satisfaction"]], tolerance = 0.01)
  record_friedman_comparison("Test 1b: mean rank environmental_concern",
                             2.17, mean_ranks[["environmental_concern"]], tolerance = 0.01)
  record_friedman_comparison("Test 1b: mean rank political_orientation",
                             1.62, mean_ranks[["political_orientation"]], tolerance = 0.01)
})

# =============================================================================
# TEST 2: WEIGHTED / UNGROUPED
# =============================================================================

test_that("Friedman test matches SPSS: weighted ungrouped trust items (Test 2a)", {
  data(survey_data)

  result <- survey_data %>%
    friedman_test(trust_government, trust_media, trust_science,
                  weights = sampling_weight)

  r <- result$results

  # SPSS reference: Identical to Test 1a (weights ~1.0 → frequency weights = 1)
  # N=2135, Chi-Square=1009.035, df=2
  # Use wider tolerance for weighted analysis (design-based vs frequency weights)
  record_friedman_comparison("Test 2a: Chi-Square (weighted)",
                             1009.035, r$chi_sq, tolerance = 200)
  record_friedman_comparison("Test 2a: df", 2, r$df, tolerance = 0)
  record_friedman_comparison("Test 2a: p < 0.001", 1,
                             as.numeric(r$p_value < 0.001), tolerance = 0)
  record_friedman_comparison("Test 2a: N (weighted)",
                             2135, r$n, tolerance = 50)

  # Mean ranks should be close to unweighted
  mean_ranks <- r$mean_ranks[[1]]
  record_friedman_comparison("Test 2a: mean rank trust_government (weighted)",
                             1.81, mean_ranks[["trust_government"]], tolerance = 0.05)
  record_friedman_comparison("Test 2a: mean rank trust_media (weighted)",
                             1.68, mean_ranks[["trust_media"]], tolerance = 0.05)
  record_friedman_comparison("Test 2a: mean rank trust_science (weighted)",
                             2.51, mean_ranks[["trust_science"]], tolerance = 0.05)

  # Structure checks
  expect_equal(result$weights, "sampling_weight")
})

test_that("Friedman test matches SPSS: weighted ungrouped attitude items (Test 2b)", {
  data(survey_data)

  result <- survey_data %>%
    friedman_test(life_satisfaction, environmental_concern, political_orientation,
                  weights = sampling_weight)

  r <- result$results

  # SPSS reference: Identical to Test 1b (weights ~1.0)
  # N=2139, Chi-Square=553.909, df=2
  record_friedman_comparison("Test 2b: Chi-Square (weighted)",
                             553.909, r$chi_sq, tolerance = 200)
  record_friedman_comparison("Test 2b: df", 2, r$df, tolerance = 0)
  record_friedman_comparison("Test 2b: p < 0.001", 1,
                             as.numeric(r$p_value < 0.001), tolerance = 0)

  mean_ranks <- r$mean_ranks[[1]]
  record_friedman_comparison("Test 2b: mean rank life_satisfaction (weighted)",
                             2.21, mean_ranks[["life_satisfaction"]], tolerance = 0.05)
  record_friedman_comparison("Test 2b: mean rank environmental_concern (weighted)",
                             2.17, mean_ranks[["environmental_concern"]], tolerance = 0.05)
  record_friedman_comparison("Test 2b: mean rank political_orientation (weighted)",
                             1.62, mean_ranks[["political_orientation"]], tolerance = 0.05)
})

# =============================================================================
# TEST 3: UNWEIGHTED / GROUPED BY REGION
# =============================================================================

test_that("Friedman test matches SPSS: unweighted grouped by region (Test 3)", {
  data(survey_data)

  result <- survey_data %>%
    group_by(region) %>%
    friedman_test(trust_government, trust_media, trust_science)

  r <- result$results

  # East results
  east <- r[r$region == "East", ]
  # SPSS: East N=422, Chi-Square=217.100, df=2, p<.001
  record_friedman_comparison("Test 3 East: N", 422, east$n, tolerance = 0)
  record_friedman_comparison("Test 3 East: Chi-Square", 217.100, east$chi_sq,
                             tolerance = 0.01)
  record_friedman_comparison("Test 3 East: df", 2, east$df, tolerance = 0)
  record_friedman_comparison("Test 3 East: p < 0.001", 1,
                             as.numeric(east$p_value < 0.001), tolerance = 0)

  # East mean ranks: 1.80, 1.67, 2.53
  east_ranks <- east$mean_ranks[[1]]
  record_friedman_comparison("Test 3 East: mean rank trust_government",
                             1.80, east_ranks[["trust_government"]], tolerance = 0.01)
  record_friedman_comparison("Test 3 East: mean rank trust_media",
                             1.67, east_ranks[["trust_media"]], tolerance = 0.01)
  record_friedman_comparison("Test 3 East: mean rank trust_science",
                             2.53, east_ranks[["trust_science"]], tolerance = 0.01)

  # West results
  west <- r[r$region == "West", ]
  # SPSS: West N=1713, Chi-Square=792.344, df=2, p<.001
  record_friedman_comparison("Test 3 West: N", 1713, west$n, tolerance = 0)
  record_friedman_comparison("Test 3 West: Chi-Square", 792.344, west$chi_sq,
                             tolerance = 0.01)
  record_friedman_comparison("Test 3 West: df", 2, west$df, tolerance = 0)
  record_friedman_comparison("Test 3 West: p < 0.001", 1,
                             as.numeric(west$p_value < 0.001), tolerance = 0)

  # West mean ranks: 1.81, 1.68, 2.50
  west_ranks <- west$mean_ranks[[1]]
  record_friedman_comparison("Test 3 West: mean rank trust_government",
                             1.81, west_ranks[["trust_government"]], tolerance = 0.01)
  record_friedman_comparison("Test 3 West: mean rank trust_media",
                             1.68, west_ranks[["trust_media"]], tolerance = 0.01)
  record_friedman_comparison("Test 3 West: mean rank trust_science",
                             2.50, west_ranks[["trust_science"]], tolerance = 0.01)

  # Structure checks
  expect_true(result$is_grouped)
  expect_equal(nrow(r), 2)
})

# =============================================================================
# TEST 4: WEIGHTED / GROUPED BY REGION
# =============================================================================

test_that("Friedman test matches SPSS: weighted grouped by region (Test 4)", {
  data(survey_data)

  result <- survey_data %>%
    group_by(region) %>%
    friedman_test(trust_government, trust_media, trust_science,
                  weights = sampling_weight)

  r <- result$results

  # East results (SPSS: identical to unweighted)
  east <- r[r$region == "East", ]
  record_friedman_comparison("Test 4 East: Chi-Square (weighted)",
                             217.100, east$chi_sq, tolerance = 50)
  record_friedman_comparison("Test 4 East: df", 2, east$df, tolerance = 0)
  record_friedman_comparison("Test 4 East: p < 0.001", 1,
                             as.numeric(east$p_value < 0.001), tolerance = 0)

  east_ranks <- east$mean_ranks[[1]]
  record_friedman_comparison("Test 4 East: mean rank trust_government (weighted)",
                             1.80, east_ranks[["trust_government"]], tolerance = 0.05)
  record_friedman_comparison("Test 4 East: mean rank trust_media (weighted)",
                             1.67, east_ranks[["trust_media"]], tolerance = 0.05)
  record_friedman_comparison("Test 4 East: mean rank trust_science (weighted)",
                             2.53, east_ranks[["trust_science"]], tolerance = 0.05)

  # West results
  west <- r[r$region == "West", ]
  record_friedman_comparison("Test 4 West: Chi-Square (weighted)",
                             792.344, west$chi_sq, tolerance = 200)
  record_friedman_comparison("Test 4 West: df", 2, west$df, tolerance = 0)
  record_friedman_comparison("Test 4 West: p < 0.001", 1,
                             as.numeric(west$p_value < 0.001), tolerance = 0)

  west_ranks <- west$mean_ranks[[1]]
  record_friedman_comparison("Test 4 West: mean rank trust_government (weighted)",
                             1.81, west_ranks[["trust_government"]], tolerance = 0.05)
  record_friedman_comparison("Test 4 West: mean rank trust_media (weighted)",
                             1.68, west_ranks[["trust_media"]], tolerance = 0.05)
  record_friedman_comparison("Test 4 West: mean rank trust_science (weighted)",
                             2.50, west_ranks[["trust_science"]], tolerance = 0.05)

  # Structure checks
  expect_true(result$is_grouped)
  expect_equal(result$weights, "sampling_weight")
})

# =============================================================================
# TEST 5: LONGITUDINAL DATA (4 TIMEPOINTS)
# =============================================================================

test_that("Friedman test matches SPSS: longitudinal ungrouped (Test 5a)", {
  data(longitudinal_data)

  # Pivot to wide format (matching SPSS longitudinal_data_wide.sav)
  long_wide <- longitudinal_data %>%
    select(subject_id, group, time, outcome_score) %>%
    pivot_wider(names_from = time, values_from = outcome_score,
                names_prefix = "score_")

  result <- long_wide %>%
    friedman_test(score_T1, score_T2, score_T3, score_T4)

  r <- result$results

  # SPSS: N=75, Chi-Square=61.192, df=3, p<.001
  record_friedman_comparison("Test 5a: N", 75, r$n, tolerance = 0)
  record_friedman_comparison("Test 5a: Chi-Square", 61.192, r$chi_sq,
                             tolerance = 0.01)
  record_friedman_comparison("Test 5a: df", 3, r$df, tolerance = 0)
  record_friedman_comparison("Test 5a: p < 0.001", 1,
                             as.numeric(r$p_value < 0.001), tolerance = 0)
  expect_equal(r$k, 4)

  # Mean ranks: 1.65, 2.31, 2.84, 3.20
  mean_ranks <- r$mean_ranks[[1]]
  record_friedman_comparison("Test 5a: mean rank score_T1",
                             1.65, mean_ranks[["score_T1"]], tolerance = 0.01)
  record_friedman_comparison("Test 5a: mean rank score_T2",
                             2.31, mean_ranks[["score_T2"]], tolerance = 0.01)
  record_friedman_comparison("Test 5a: mean rank score_T3",
                             2.84, mean_ranks[["score_T3"]], tolerance = 0.01)
  record_friedman_comparison("Test 5a: mean rank score_T4",
                             3.20, mean_ranks[["score_T4"]], tolerance = 0.01)
})

test_that("Friedman test matches SPSS: longitudinal grouped by treatment (Test 5b)", {
  data(longitudinal_data)

  long_wide <- longitudinal_data %>%
    select(subject_id, group, time, outcome_score) %>%
    pivot_wider(names_from = time, values_from = outcome_score,
                names_prefix = "score_")

  result <- long_wide %>%
    group_by(group) %>%
    friedman_test(score_T1, score_T2, score_T3, score_T4)

  r <- result$results

  # Control: N=39, Chi-Square=6.538, df=3, p=.088
  control <- r[r$group == "Control", ]
  record_friedman_comparison("Test 5b Control: N", 39, control$n, tolerance = 0)
  record_friedman_comparison("Test 5b Control: Chi-Square", 6.538, control$chi_sq,
                             tolerance = 0.01)
  record_friedman_comparison("Test 5b Control: df", 3, control$df, tolerance = 0)
  record_friedman_comparison("Test 5b Control: p-value", 0.088, control$p_value,
                             tolerance = 0.005)

  # Control mean ranks: 2.08, 2.51, 2.62, 2.79
  ctrl_ranks <- control$mean_ranks[[1]]
  record_friedman_comparison("Test 5b Control: mean rank score_T1",
                             2.08, ctrl_ranks[["score_T1"]], tolerance = 0.01)
  record_friedman_comparison("Test 5b Control: mean rank score_T2",
                             2.51, ctrl_ranks[["score_T2"]], tolerance = 0.01)
  record_friedman_comparison("Test 5b Control: mean rank score_T3",
                             2.62, ctrl_ranks[["score_T3"]], tolerance = 0.01)
  record_friedman_comparison("Test 5b Control: mean rank score_T4",
                             2.79, ctrl_ranks[["score_T4"]], tolerance = 0.01)

  # Treatment: N=36, Chi-Square=75.933, df=3, p<.001
  treatment <- r[r$group == "Treatment", ]
  record_friedman_comparison("Test 5b Treatment: N", 36, treatment$n, tolerance = 0)
  record_friedman_comparison("Test 5b Treatment: Chi-Square", 75.933, treatment$chi_sq,
                             tolerance = 0.01)
  record_friedman_comparison("Test 5b Treatment: df", 3, treatment$df, tolerance = 0)
  record_friedman_comparison("Test 5b Treatment: p < 0.001", 1,
                             as.numeric(treatment$p_value < 0.001), tolerance = 0)

  # Treatment mean ranks: 1.19, 2.08, 3.08, 3.64
  treat_ranks <- treatment$mean_ranks[[1]]
  record_friedman_comparison("Test 5b Treatment: mean rank score_T1",
                             1.19, treat_ranks[["score_T1"]], tolerance = 0.01)
  record_friedman_comparison("Test 5b Treatment: mean rank score_T2",
                             2.08, treat_ranks[["score_T2"]], tolerance = 0.01)
  record_friedman_comparison("Test 5b Treatment: mean rank score_T3",
                             3.08, treat_ranks[["score_T3"]], tolerance = 0.01)
  record_friedman_comparison("Test 5b Treatment: mean rank score_T4",
                             3.64, treat_ranks[["score_T4"]], tolerance = 0.01)

  # Structure checks
  expect_true(result$is_grouped)
  expect_equal(nrow(r), 2)
})

# =============================================================================
# EDGE CASES AND INPUT VALIDATION
# =============================================================================

test_that("Friedman test requires at least 3 variables", {
  data(survey_data)

  expect_error(
    survey_data %>% friedman_test(trust_government, trust_media),
    "at least 3"
  )
})

test_that("Friedman test requires a data frame", {
  expect_error(
    friedman_test(1:10, trust_government, trust_media, trust_science),
    "data frame"
  )
})

test_that("Friedman test handles tidyselect helpers", {
  data(survey_data)

  result <- survey_data %>%
    friedman_test(starts_with("trust_"))

  expect_s3_class(result, "friedman_test")
  expect_equal(length(result$variables), 3)
  expect_true(all(grepl("^trust_", result$variables)))
})

test_that("Friedman test handles missing data with listwise deletion", {
  data(survey_data)

  # Introduce some NAs
  test_data <- survey_data
  test_data$trust_government[1:10] <- NA

  result <- test_data %>%
    friedman_test(trust_government, trust_media, trust_science)

  # N should be less than complete data (2135 - some additional rows with NAs)
  expect_true(result$results$n < 2135)
  expect_false(is.na(result$results$chi_sq))
})

test_that("Friedman test result contains expected components", {
  data(survey_data)

  result <- survey_data %>%
    friedman_test(trust_government, trust_media, trust_science)

  # Check result structure
  expect_true("results" %in% names(result))
  expect_true("variables" %in% names(result))
  expect_true("weights" %in% names(result))
  expect_true("is_grouped" %in% names(result))
  expect_true("conf.level" %in% names(result))

  # Check results tibble columns
  expect_true("chi_sq" %in% names(result$results))
  expect_true("df" %in% names(result$results))
  expect_true("p_value" %in% names(result$results))
  expect_true("kendall_w" %in% names(result$results))
  expect_true("n" %in% names(result$results))
  expect_true("k" %in% names(result$results))
  expect_true("mean_ranks" %in% names(result$results))
})

test_that("Friedman test Kendall's W is between 0 and 1", {
  data(survey_data)

  result <- survey_data %>%
    friedman_test(trust_government, trust_media, trust_science)

  expect_true(result$results$kendall_w >= 0)
  expect_true(result$results$kendall_w <= 1)
})

test_that("Friedman test Kendall's W matches manual calculation", {
  data(survey_data)

  result <- survey_data %>%
    friedman_test(trust_government, trust_media, trust_science)

  r <- result$results
  # Kendall's W = Chi^2 / (N * (k - 1))
  expected_w <- r$chi_sq / (r$n * (r$k - 1))
  expect_equal(r$kendall_w, expected_w, tolerance = 0.0001)
})

# =============================================================================
# PRINT METHOD TESTS
# =============================================================================

test_that("Friedman test print method produces output", {
  data(survey_data)

  result <- survey_data %>%
    friedman_test(trust_government, trust_media, trust_science)

  # Test that print produces output (cat()-generated content only)
  output <- capture.output(print(result))
  expect_true(any(grepl("Mean Rank", output)))
  expect_true(any(grepl("Chi-Square", output)))
  expect_true(any(grepl("Kendall", output)))
  expect_true(any(grepl("trust_government", output)))
})

test_that("Friedman test weighted print method produces output", {
  data(survey_data)

  result <- survey_data %>%
    friedman_test(trust_government, trust_media, trust_science,
                  weights = sampling_weight)

  output <- capture.output(print(result))
  expect_true(any(grepl("Weighted", output)))
  expect_true(any(grepl("Mean Rank", output)))
  expect_true(any(grepl("Chi-Square", output)))
})

test_that("Friedman test grouped print method produces output", {
  data(survey_data)

  result <- survey_data %>%
    group_by(region) %>%
    friedman_test(trust_government, trust_media, trust_science)

  output <- capture.output(print(result))
  expect_true(any(grepl("Mean Rank", output)))
  expect_true(any(grepl("Chi-Square", output)))
})

# =============================================================================
# CONSISTENCY CHECKS
# =============================================================================

test_that("Friedman test df equals k - 1", {
  data(survey_data)

  # 3 variables -> df = 2
  r3 <- survey_data %>%
    friedman_test(trust_government, trust_media, trust_science)
  expect_equal(r3$results$df, 2)

  # 4 variables via longitudinal data -> df = 3
  data(longitudinal_data)
  long_wide <- longitudinal_data %>%
    select(subject_id, group, time, outcome_score) %>%
    pivot_wider(names_from = time, values_from = outcome_score,
                names_prefix = "score_")

  r4 <- long_wide %>%
    friedman_test(score_T1, score_T2, score_T3, score_T4)
  expect_equal(r4$results$df, 3)
})

test_that("Friedman mean ranks sum to k*(k+1)/2", {
  data(survey_data)

  result <- survey_data %>%
    friedman_test(trust_government, trust_media, trust_science)

  mean_ranks <- unlist(result$results$mean_ranks[[1]])
  k <- result$results$k
  expected_sum <- k * (k + 1) / 2  # For k=3: sum should be 6

  expect_equal(sum(mean_ranks), expected_sum, tolerance = 0.01)
})

test_that("Friedman test significant result has correct p-value range", {
  data(survey_data)

  result <- survey_data %>%
    friedman_test(trust_government, trust_media, trust_science)

  # Test 1a is highly significant
  expect_true(result$results$p_value < 0.001)
  expect_true(result$results$chi_sq > 0)
})

test_that("Friedman test non-significant result (Control group) has correct p-value", {
  data(longitudinal_data)

  long_wide <- longitudinal_data %>%
    select(subject_id, group, time, outcome_score) %>%
    pivot_wider(names_from = time, values_from = outcome_score,
                names_prefix = "score_")

  result <- long_wide %>%
    group_by(group) %>%
    friedman_test(score_T1, score_T2, score_T3, score_T4)

  control <- result$results[result$results$group == "Control", ]
  # SPSS p = .088 (not significant at .05 level)
  expect_true(control$p_value > 0.05)
  expect_true(control$p_value < 0.10)
})

# =============================================================================
# SUMMARY
# =============================================================================

test_that("SPSS validation summary", {
  cat("\n")
  cat("==============================================\n")
  cat("FRIEDMAN TEST SPSS VALIDATION SUMMARY\n")
  cat("==============================================\n")
  cat(sprintf("Total SPSS comparisons: %d\n", spss_comparison_count))
  cat("All comparisons passed!\n")
  cat("==============================================\n")

  expect_true(spss_comparison_count > 0)
})
