# ============================================================================
# SPSS VALIDATION TESTS: Wilcoxon Signed-Rank Test
# ============================================================================
# Validates wilcoxon_test() against SPSS NPAR TESTS /WILCOXON procedure
# SPSS output: tests/spss_reference/outputs/wilcoxon_test_output.txt
# Four standard scenarios: unweighted/weighted x ungrouped/grouped
# Plus: longitudinal_data additional tests
# ============================================================================

library(testthat)
library(dplyr)
library(tidyr)

# ============================================================================
# SPSS REFERENCE VALUES
# ============================================================================

spss_values <- list(
  # Test 1a: trust_government vs trust_media (unweighted, ungrouped)
  test_1a = list(
    n_neg = 955, mean_rank_neg = 887.53, sum_rank_neg = 847592.50,
    n_pos = 770, mean_rank_pos = 832.57, sum_rank_pos = 641082.50,
    n_ties = 502, n_total = 2227,
    Z = -5.097, p = 0.000  # p < .001
  ),
  # Test 1b: trust_government vs trust_science (unweighted, ungrouped)
  test_1b = list(
    n_neg = 355, mean_rank_neg = 671.62, sum_rank_neg = 238424.00,
    n_pos = 1424, mean_rank_pos = 944.44, sum_rank_pos = 1344886.00,
    n_ties = 476, n_total = 2255,
    Z = -25.945, p = 0.000
  ),
  # Test 1c: trust_media vs trust_science (unweighted, ungrouped)
  test_1c = list(
    n_neg = 322, mean_rank_neg = 648.09, sum_rank_neg = 208685.50,
    n_pos = 1558, mean_rank_pos = 1000.93, sum_rank_pos = 1559454.50,
    n_ties = 392, n_total = 2272,
    Z = -29.091, p = 0.000
  ),
  # Test 3a East: trust_government vs trust_media (unweighted, grouped)
  test_3a_east = list(
    n_neg = 191, mean_rank_neg = 183.16, sum_rank_neg = 34983.50,
    n_pos = 155, mean_rank_pos = 161.60, sum_rank_pos = 25047.50,
    n_ties = 89, n_total = 435,
    Z = -2.727, p = 0.006
  ),
  # Test 3a West: trust_government vs trust_media (unweighted, grouped)
  test_3a_west = list(
    n_neg = 764, mean_rank_neg = 705.10, sum_rank_neg = 538697.50,
    n_pos = 615, mean_rank_pos = 671.24, sum_rank_pos = 412812.50,
    n_ties = 413, n_total = 1792,
    Z = -4.346, p = 0.000
  ),
  # Test 3b East: trust_government vs trust_science (unweighted, grouped)
  test_3b_east = list(
    n_neg = 64, mean_rank_neg = 142.14, sum_rank_neg = 9097.00,
    n_pos = 287, mean_rank_pos = 183.55, sum_rank_pos = 52679.00,
    n_ties = 93, n_total = 444,
    Z = -11.635, p = 0.000
  ),
  # Test 3b West: trust_government vs trust_science (unweighted, grouped)
  test_3b_west = list(
    n_neg = 291, mean_rank_neg = 531.02, sum_rank_neg = 154526.50,
    n_pos = 1137, mean_rank_pos = 761.46, sum_rank_pos = 865779.50,
    n_ties = 383, n_total = 1811,
    Z = -23.191, p = 0.000
  ),
  # Test 5a: score_T1 vs score_T2 (longitudinal, ungrouped)
  test_5a = list(
    n_neg = 27, mean_rank_neg = 40.19, sum_rank_neg = 1085.00,
    n_pos = 78, mean_rank_pos = 57.44, sum_rank_pos = 4480.00,
    n_ties = 0, n_total = 105,
    Z = -5.427, p = 0.000
  ),
  # Test 5b: score_T1 vs score_T3 (longitudinal, ungrouped)
  test_5b = list(
    n_neg = 21, mean_rank_neg = 30.95, sum_rank_neg = 650.00,
    n_pos = 75, mean_rank_pos = 53.41, sum_rank_pos = 4006.00,
    n_ties = 0, n_total = 96,
    Z = -6.132, p = 0.000
  ),
  # Test 5c Control: score_T1 vs score_T2 (longitudinal, grouped)
  test_5c_control = list(
    n_neg = 18, mean_rank_neg = 21.94, sum_rank_neg = 395.00,
    n_pos = 33, mean_rank_pos = 28.21, sum_rank_pos = 931.00,
    n_ties = 0, n_total = 51,
    Z = -2.512, p = 0.012
  ),
  # Test 5c Treatment: score_T1 vs score_T2 (longitudinal, grouped)
  test_5c_treatment = list(
    n_neg = 9, mean_rank_neg = 17.89, sum_rank_neg = 161.00,
    n_pos = 45, mean_rank_pos = 29.42, sum_rank_pos = 1324.00,
    n_ties = 0, n_total = 54,
    Z = -5.007, p = 0.000
  )
)

# ============================================================================
# VALIDATION TRACKING
# ============================================================================

wt_validation_results <- list()

record_wt_comparison <- function(test, metric, expected, actual, tolerance = 0.001) {
  match <- abs(expected - actual) <= tolerance
  result <- list(
    test = test, metric = metric,
    expected = expected, actual = actual,
    difference = abs(expected - actual),
    tolerance = tolerance, match = match
  )
  pos <- length(wt_validation_results) + 1
  wt_validation_results[[pos]] <<- result
  expect_equal(actual, expected, tolerance = tolerance,
               label = paste(test, metric))
}

# Helper function for validating a Wilcoxon result against SPSS reference
validate_wt_test <- function(result, spss_ref, test_label, row_idx = 1) {
  r <- if (nrow(result$results) >= row_idx) result$results[row_idx, ] else NULL
  if (is.null(r)) {
    fail(paste("No result row found for", test_label))
    return()
  }

  # Z statistic (compare absolute values - sign is convention)
  record_wt_comparison(test_label, "abs(Z)",
                       abs(spss_ref$Z), abs(r$Z), tolerance = 0.001)

  # p-value
  if (spss_ref$p > 0) {
    record_wt_comparison(test_label, "p_value",
                         spss_ref$p, r$p_value, tolerance = 0.001)
  } else {
    # p = .000 in SPSS means p < .001
    record_wt_comparison(test_label, "p_value < 0.001",
                         1, as.numeric(r$p_value < 0.001), tolerance = 0)
  }

  # Rank counts
  record_wt_comparison(test_label, "n_neg",
                       spss_ref$n_neg, r$n_neg, tolerance = 0)
  record_wt_comparison(test_label, "n_pos",
                       spss_ref$n_pos, r$n_pos, tolerance = 0)
  record_wt_comparison(test_label, "n_ties",
                       spss_ref$n_ties, r$n_ties, tolerance = 0)
  record_wt_comparison(test_label, "n_total",
                       spss_ref$n_total, r$n_total, tolerance = 0)

  # Mean ranks
  record_wt_comparison(test_label, "mean_rank_neg",
                       spss_ref$mean_rank_neg, round(r$mean_rank_neg, 2),
                       tolerance = 0.01)
  record_wt_comparison(test_label, "mean_rank_pos",
                       spss_ref$mean_rank_pos, round(r$mean_rank_pos, 2),
                       tolerance = 0.01)

  # Sum of ranks
  record_wt_comparison(test_label, "sum_rank_neg",
                       spss_ref$sum_rank_neg, round(r$sum_rank_neg, 2),
                       tolerance = 0.5)
  record_wt_comparison(test_label, "sum_rank_pos",
                       spss_ref$sum_rank_pos, round(r$sum_rank_pos, 2),
                       tolerance = 0.5)
}

# ============================================================================
# TEST 1: UNWEIGHTED / UNGROUPED (survey_data)
# ============================================================================

test_that("Wilcoxon: unweighted ungrouped - trust_government vs trust_media", {
  data(survey_data)
  result <- survey_data %>%
    wilcoxon_test(x = trust_government, y = trust_media)

  expect_s3_class(result, "wilcoxon_test")
  validate_wt_test(result, spss_values$test_1a, "Test 1a")
})

test_that("Wilcoxon: unweighted ungrouped - trust_government vs trust_science", {
  data(survey_data)
  result <- survey_data %>%
    wilcoxon_test(x = trust_government, y = trust_science)

  validate_wt_test(result, spss_values$test_1b, "Test 1b")
})

test_that("Wilcoxon: unweighted ungrouped - trust_media vs trust_science", {
  data(survey_data)
  result <- survey_data %>%
    wilcoxon_test(x = trust_media, y = trust_science)

  validate_wt_test(result, spss_values$test_1c, "Test 1c")
})

# ============================================================================
# TEST 2: WEIGHTED / UNGROUPED (survey_data)
# ============================================================================
# SPSS weighted results identical to unweighted (sampling_weight ≈ 1.0,
# SPSS rounds frequency weights to integers)

test_that("Wilcoxon: weighted ungrouped - trust_government vs trust_media", {
  data(survey_data)
  result <- survey_data %>%
    wilcoxon_test(x = trust_government, y = trust_media,
                  weights = sampling_weight)

  expect_s3_class(result, "wilcoxon_test")

  # With weights ≈ 1.0, design-based approach should give similar results
  # but may differ slightly from SPSS frequency-weight rounding.
  # Weighted N can differ because R uses continuous weights while SPSS
  # rounds to integer frequency weights.
  r <- result$results[1, ]
  record_wt_comparison("Test 2a", "abs(Z)",
                       abs(spss_values$test_1a$Z), abs(r$Z), tolerance = 1.0)
  record_wt_comparison("Test 2a", "n_total",
                       spss_values$test_1a$n_total, r$n_total, tolerance = 50)
})

test_that("Wilcoxon: weighted ungrouped - trust_government vs trust_science", {
  data(survey_data)
  result <- survey_data %>%
    wilcoxon_test(x = trust_government, y = trust_science,
                  weights = sampling_weight)

  r <- result$results[1, ]
  record_wt_comparison("Test 2b", "abs(Z)",
                       abs(spss_values$test_1b$Z), abs(r$Z), tolerance = 1.0)
})

# ============================================================================
# TEST 3: UNWEIGHTED / GROUPED (survey_data, by region)
# ============================================================================

test_that("Wilcoxon: unweighted grouped - trust_government vs trust_media by region", {
  data(survey_data)
  result <- survey_data %>%
    group_by(region) %>%
    wilcoxon_test(x = trust_government, y = trust_media)

  expect_s3_class(result, "wilcoxon_test")
  expect_true(result$is_grouped)

  # East
  east_row <- which(result$results$region == "East")
  expect_length(east_row, 1)
  r_east <- result$results[east_row, ]

  record_wt_comparison("Test 3a East", "abs(Z)",
                       abs(spss_values$test_3a_east$Z), abs(r_east$Z),
                       tolerance = 0.001)
  record_wt_comparison("Test 3a East", "p_value",
                       spss_values$test_3a_east$p, round(r_east$p_value, 3),
                       tolerance = 0.001)
  record_wt_comparison("Test 3a East", "n_neg",
                       spss_values$test_3a_east$n_neg, r_east$n_neg,
                       tolerance = 0)
  record_wt_comparison("Test 3a East", "n_pos",
                       spss_values$test_3a_east$n_pos, r_east$n_pos,
                       tolerance = 0)
  record_wt_comparison("Test 3a East", "mean_rank_neg",
                       spss_values$test_3a_east$mean_rank_neg,
                       round(r_east$mean_rank_neg, 2), tolerance = 0.01)
  record_wt_comparison("Test 3a East", "sum_rank_neg",
                       spss_values$test_3a_east$sum_rank_neg,
                       round(r_east$sum_rank_neg, 2), tolerance = 0.5)

  # West
  west_row <- which(result$results$region == "West")
  expect_length(west_row, 1)
  r_west <- result$results[west_row, ]

  record_wt_comparison("Test 3a West", "abs(Z)",
                       abs(spss_values$test_3a_west$Z), abs(r_west$Z),
                       tolerance = 0.001)
  record_wt_comparison("Test 3a West", "n_neg",
                       spss_values$test_3a_west$n_neg, r_west$n_neg,
                       tolerance = 0)
  record_wt_comparison("Test 3a West", "n_pos",
                       spss_values$test_3a_west$n_pos, r_west$n_pos,
                       tolerance = 0)
  record_wt_comparison("Test 3a West", "sum_rank_pos",
                       spss_values$test_3a_west$sum_rank_pos,
                       round(r_west$sum_rank_pos, 2), tolerance = 0.5)
})

test_that("Wilcoxon: unweighted grouped - trust_government vs trust_science by region", {
  data(survey_data)
  result <- survey_data %>%
    group_by(region) %>%
    wilcoxon_test(x = trust_government, y = trust_science)

  # East
  east_row <- which(result$results$region == "East")
  r_east <- result$results[east_row, ]

  record_wt_comparison("Test 3b East", "abs(Z)",
                       abs(spss_values$test_3b_east$Z), abs(r_east$Z),
                       tolerance = 0.001)
  record_wt_comparison("Test 3b East", "n_neg",
                       spss_values$test_3b_east$n_neg, r_east$n_neg,
                       tolerance = 0)
  record_wt_comparison("Test 3b East", "n_pos",
                       spss_values$test_3b_east$n_pos, r_east$n_pos,
                       tolerance = 0)
  record_wt_comparison("Test 3b East", "mean_rank_pos",
                       spss_values$test_3b_east$mean_rank_pos,
                       round(r_east$mean_rank_pos, 2), tolerance = 0.01)

  # West
  west_row <- which(result$results$region == "West")
  r_west <- result$results[west_row, ]

  record_wt_comparison("Test 3b West", "abs(Z)",
                       abs(spss_values$test_3b_west$Z), abs(r_west$Z),
                       tolerance = 0.001)
  record_wt_comparison("Test 3b West", "n_neg",
                       spss_values$test_3b_west$n_neg, r_west$n_neg,
                       tolerance = 0)
  record_wt_comparison("Test 3b West", "n_pos",
                       spss_values$test_3b_west$n_pos, r_west$n_pos,
                       tolerance = 0)
})

# ============================================================================
# TEST 4: WEIGHTED / GROUPED (survey_data)
# ============================================================================

test_that("Wilcoxon: weighted grouped - trust_government vs trust_media by region", {
  data(survey_data)
  result <- survey_data %>%
    group_by(region) %>%
    wilcoxon_test(x = trust_government, y = trust_media,
                  weights = sampling_weight)

  expect_s3_class(result, "wilcoxon_test")
  expect_true(result$is_grouped)

  # With weights ≈ 1.0, should be similar to unweighted
  east_row <- which(result$results$region == "East")
  r_east <- result$results[east_row, ]
  record_wt_comparison("Test 4a East", "abs(Z)",
                       abs(spss_values$test_3a_east$Z), abs(r_east$Z),
                       tolerance = 1.0)

  west_row <- which(result$results$region == "West")
  r_west <- result$results[west_row, ]
  record_wt_comparison("Test 4a West", "abs(Z)",
                       abs(spss_values$test_3a_west$Z), abs(r_west$Z),
                       tolerance = 1.0)
})

# ============================================================================
# TEST 5: LONGITUDINAL DATA
# ============================================================================

test_that("Wilcoxon: longitudinal - score_T1 vs score_T2 (ungrouped)", {
  data(longitudinal_data)
  wide <- longitudinal_data %>%
    select(subject_id, group, time, outcome_score) %>%
    pivot_wider(names_from = time, values_from = outcome_score,
                names_prefix = "score_")

  result <- wide %>%
    wilcoxon_test(x = score_T1, y = score_T2)

  expect_s3_class(result, "wilcoxon_test")
  validate_wt_test(result, spss_values$test_5a, "Test 5a")
})

test_that("Wilcoxon: longitudinal - score_T1 vs score_T3 (ungrouped)", {
  data(longitudinal_data)
  wide <- longitudinal_data %>%
    select(subject_id, group, time, outcome_score) %>%
    pivot_wider(names_from = time, values_from = outcome_score,
                names_prefix = "score_")

  result <- wide %>%
    wilcoxon_test(x = score_T1, y = score_T3)

  validate_wt_test(result, spss_values$test_5b, "Test 5b")
})

test_that("Wilcoxon: longitudinal - score_T1 vs score_T2 by treatment group", {
  data(longitudinal_data)
  wide <- longitudinal_data %>%
    select(subject_id, group, time, outcome_score) %>%
    pivot_wider(names_from = time, values_from = outcome_score,
                names_prefix = "score_")

  result <- wide %>%
    group_by(group) %>%
    wilcoxon_test(x = score_T1, y = score_T2)

  expect_true(result$is_grouped)

  # Control group
  ctrl_row <- which(result$results$group == "Control")
  r_ctrl <- result$results[ctrl_row, ]

  record_wt_comparison("Test 5c Control", "abs(Z)",
                       abs(spss_values$test_5c_control$Z), abs(r_ctrl$Z),
                       tolerance = 0.001)
  record_wt_comparison("Test 5c Control", "p_value",
                       spss_values$test_5c_control$p, round(r_ctrl$p_value, 3),
                       tolerance = 0.001)
  record_wt_comparison("Test 5c Control", "n_neg",
                       spss_values$test_5c_control$n_neg, r_ctrl$n_neg,
                       tolerance = 0)
  record_wt_comparison("Test 5c Control", "n_pos",
                       spss_values$test_5c_control$n_pos, r_ctrl$n_pos,
                       tolerance = 0)
  record_wt_comparison("Test 5c Control", "mean_rank_neg",
                       spss_values$test_5c_control$mean_rank_neg,
                       round(r_ctrl$mean_rank_neg, 2), tolerance = 0.01)
  record_wt_comparison("Test 5c Control", "mean_rank_pos",
                       spss_values$test_5c_control$mean_rank_pos,
                       round(r_ctrl$mean_rank_pos, 2), tolerance = 0.01)

  # Treatment group
  trt_row <- which(result$results$group == "Treatment")
  r_trt <- result$results[trt_row, ]

  record_wt_comparison("Test 5c Treatment", "abs(Z)",
                       abs(spss_values$test_5c_treatment$Z), abs(r_trt$Z),
                       tolerance = 0.001)
  record_wt_comparison("Test 5c Treatment", "n_neg",
                       spss_values$test_5c_treatment$n_neg, r_trt$n_neg,
                       tolerance = 0)
  record_wt_comparison("Test 5c Treatment", "n_pos",
                       spss_values$test_5c_treatment$n_pos, r_trt$n_pos,
                       tolerance = 0)
  record_wt_comparison("Test 5c Treatment", "sum_rank_pos",
                       spss_values$test_5c_treatment$sum_rank_pos,
                       round(r_trt$sum_rank_pos, 2), tolerance = 0.5)
})

# ============================================================================
# EDGE CASES AND STRUCTURE
# ============================================================================

test_that("Wilcoxon: error without x or y argument", {
  data(survey_data)
  expect_error(
    survey_data %>% wilcoxon_test(x = trust_government),
    "x.*y.*required|Both"
  )
})

test_that("Wilcoxon: result structure is correct", {
  data(survey_data)
  result <- survey_data %>%
    wilcoxon_test(x = trust_government, y = trust_media)

  expect_s3_class(result, "wilcoxon_test")
  expect_equal(result$x_name, "trust_government")
  expect_equal(result$y_name, "trust_media")
  expect_true("Z" %in% names(result$results))
  expect_true("p_value" %in% names(result$results))
  expect_true("r_effect" %in% names(result$results))
  expect_true("n_neg" %in% names(result$results))
  expect_true("n_pos" %in% names(result$results))
  expect_true("n_ties" %in% names(result$results))
})

test_that("Wilcoxon: effect size r is correctly computed", {
  data(survey_data)
  result <- survey_data %>%
    wilcoxon_test(x = trust_government, y = trust_media)

  r <- result$results[1, ]
  # r = |Z| / sqrt(N) where N = n_neg + n_pos (ranked pairs)
  n_ranked <- r$n_neg + r$n_pos
  expected_r <- abs(r$Z) / sqrt(n_ranked)
  expect_equal(r$r_effect, expected_r, tolerance = 0.001)
})

# ============================================================================
# PRINT METHOD TESTS
# ============================================================================

test_that("Wilcoxon: print method runs without error", {
  data(survey_data)
  result <- survey_data %>%
    wilcoxon_test(x = trust_government, y = trust_media)

  # cli output goes to message stream; check cat()-generated content
  expect_output(print(result), "Negative Ranks")
  expect_output(print(result), "Positive Ranks")
  expect_output(print(result), "Sum of Ranks")
})

test_that("Wilcoxon: print method for grouped results", {
  data(survey_data)
  result <- survey_data %>%
    group_by(region) %>%
    wilcoxon_test(x = trust_government, y = trust_media)

  expect_output(print(result), "Negative Ranks")
  expect_output(print(result), "Mean Rank")
})

# ============================================================================
# VALIDATION REPORT
# ============================================================================

test_that("Validation report summary", {
  total <- length(wt_validation_results)
  passed <- sum(sapply(wt_validation_results, function(r) r$match))
  failed <- total - passed

  cat("\n")
  cat("==================================================\n")
  cat("  WILCOXON TEST SPSS VALIDATION REPORT\n")
  cat("==================================================\n")
  cat(sprintf("  Total comparisons: %d\n", total))
  cat(sprintf("  Passed: %d\n", passed))
  cat(sprintf("  Failed: %d\n", failed))
  cat("==================================================\n")

  if (failed > 0) {
    cat("\n  FAILED COMPARISONS:\n")
    for (r in wt_validation_results) {
      if (!r$match) {
        cat(sprintf("  - %s | %s: expected=%.4f, actual=%.4f, diff=%.6f\n",
                    r$test, r$metric, r$expected, r$actual, r$difference))
      }
    }
  }

  expect_equal(failed, 0,
               label = "All Wilcoxon SPSS comparisons should pass")
})
