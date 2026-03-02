# ============================================================================
# FISHER'S EXACT TEST - SPSS VALIDATION TEST
# ============================================================================
# Purpose: Validate R fisher_test() against SPSS CROSSTABS with Fisher's Exact
# Dataset: survey_data
# Variables: gender x region (2x2), gender x interview_mode (2x3)
# Split grouping: education
# Created: 2026-03-01
# SPSS Version: 29.0
#
# Validation strategy:
# - 2x2 tables: Fisher's Exact Test (automatic in SPSS CROSSTABS)
# - Larger tables: Fisher-Freeman-Halton (via SPSS EXACT module)
# - 4 scenarios: unweighted/weighted x ungrouped/grouped
# - Additional: small sample subset, effect sizes
#
# SPSS provides: Exact Sig. (2-sided), Exact Sig. (1-sided),
#                Point Probability for 2x2 tables
# ============================================================================

library(testthat)
library(dplyr)
library(mariposa)

# ============================================================================
# GLOBAL TRACKING FOR VALIDATION REPORT
# ============================================================================

fisher_validation_results <- list()

record_fisher_comparison <- function(test_name, metric, expected, actual,
                                     tolerance = 0) {
  match_status <- if (is.na(expected) && is.na(actual)) {
    TRUE
  } else if (is.na(expected) || is.na(actual)) {
    FALSE
  } else {
    abs(expected - actual) <= tolerance
  }

  result <- list(
    test = test_name,
    metric = metric,
    expected = expected,
    actual = actual,
    match = match_status,
    tolerance = tolerance,
    difference = if (!is.na(expected) && !is.na(actual)) abs(expected - actual) else NA
  )

  fisher_validation_results <<- append(fisher_validation_results, list(result))
  return(match_status)
}

# ============================================================================
# SPSS REFERENCE VALUES
# ============================================================================
# Fill from SPSS output (fisher_test.sps)
#
# For 2x2 tables, SPSS CROSSTABS /STATISTICS=CHISQ gives:
#   Fisher's Exact Test: Exact Sig. (2-sided), Exact Sig. (1-sided)
#   Also: Pearson Chi-Square, N of Valid Cases

spss_values <- list(
  # ---- Test 1a: gender x region (2x2, unweighted) ----
  test_1a = list(
    # Crosstab counts
    crosstab = list(
      male_east = NA_integer_, male_west = NA_integer_,
      female_east = NA_integer_, female_west = NA_integer_
    ),
    n = NA_integer_,
    fisher_exact_2sided = NA_real_,
    fisher_exact_1sided = NA_real_,
    # Also from Chi-Square table for comparison:
    chi_sq = NA_real_,
    chi_sq_p = NA_real_,
    phi = NA_real_
  ),

  # ---- Test 1b: gender x interview_mode (2x3, unweighted) ----
  test_1b = list(
    n = NA_integer_,
    fisher_exact_2sided = NA_real_,  # Fisher-Freeman-Halton
    chi_sq = NA_real_,
    chi_sq_p = NA_real_,
    cramers_v = NA_real_
  ),

  # ---- Test 1c: Small sample (id <= 50, 2x2, unweighted) ----
  test_1c = list(
    crosstab = list(
      male_east = NA_integer_, male_west = NA_integer_,
      female_east = NA_integer_, female_west = NA_integer_
    ),
    n = NA_integer_,
    fisher_exact_2sided = NA_real_,
    fisher_exact_1sided = NA_real_
  ),

  # ---- Test 2a: gender x region (2x2, weighted) ----
  test_2a = list(
    n = NA_real_,  # weighted N
    fisher_exact_2sided = NA_real_,
    chi_sq = NA_real_,
    chi_sq_p = NA_real_
  ),

  # ---- Test 3: gender x region, grouped by education ----
  # Only Basic Secondary and University for manageable output
  test_3_basic = list(
    n = NA_integer_,
    fisher_exact_2sided = NA_real_,
    fisher_exact_1sided = NA_real_
  ),
  test_3_university = list(
    n = NA_integer_,
    fisher_exact_2sided = NA_real_,
    fisher_exact_1sided = NA_real_
  )
)

# ============================================================================
# TEST 1: UNWEIGHTED / UNGROUPED
# ============================================================================

test_that("Fisher test: gender x region (2x2, unweighted)", {
  data(survey_data)
  result <- survey_data %>%
    fisher_test(row = gender, col = region)

  expect_s3_class(result, "fisher_test")

  # Basic structure checks
  expect_true("p_value" %in% names(result) || "p.value" %in% names(result))
  expect_true("n" %in% names(result))

  # Validate against SPSS
  if (!is.na(spss_values$test_1a$fisher_exact_2sided)) {
    p_val <- result$p_value %||% result$p.value
    expect_true(
      record_fisher_comparison("1a: gender x region",
                                "Fisher exact p (2-sided)",
                                spss_values$test_1a$fisher_exact_2sided,
                                p_val,
                                tolerance = 0.0001),
      label = "Fisher exact p matches SPSS"
    )
  }

  if (!is.na(spss_values$test_1a$n)) {
    expect_true(
      record_fisher_comparison("1a: gender x region", "N",
                                spss_values$test_1a$n, result$n,
                                tolerance = 0),
      label = "N matches SPSS"
    )
  }
})

test_that("Fisher test: gender x interview_mode (2x3, unweighted)", {
  data(survey_data)
  result <- survey_data %>%
    fisher_test(row = gender, col = interview_mode)

  expect_s3_class(result, "fisher_test")

  # For non-2x2 tables, should use Fisher-Freeman-Halton
  if (!is.na(spss_values$test_1b$fisher_exact_2sided)) {
    p_val <- result$p_value %||% result$p.value
    expect_true(
      record_fisher_comparison("1b: gender x interview_mode",
                                "Fisher-Freeman-Halton p",
                                spss_values$test_1b$fisher_exact_2sided,
                                p_val,
                                tolerance = 0.001),
      label = "Fisher-Freeman-Halton p matches SPSS"
    )
  }
})

test_that("Fisher test: small sample subset", {
  data(survey_data)
  small_data <- survey_data %>% filter(id <= 50)

  result <- small_data %>%
    fisher_test(row = gender, col = region)

  expect_s3_class(result, "fisher_test")

  if (!is.na(spss_values$test_1c$fisher_exact_2sided)) {
    p_val <- result$p_value %||% result$p.value
    expect_true(
      record_fisher_comparison("1c: small sample",
                                "Fisher exact p (2-sided)",
                                spss_values$test_1c$fisher_exact_2sided,
                                p_val,
                                tolerance = 0.0001),
      label = "Fisher exact p for small sample"
    )
  }
})

# ============================================================================
# TEST 2: WEIGHTED / UNGROUPED
# ============================================================================

test_that("Fisher test: gender x region (2x2, weighted)", {
  data(survey_data)
  result <- survey_data %>%
    fisher_test(row = gender, col = region, weights = sampling_weight)

  expect_s3_class(result, "fisher_test")

  if (!is.na(spss_values$test_2a$fisher_exact_2sided)) {
    p_val <- result$p_value %||% result$p.value
    expect_true(
      record_fisher_comparison("2a: gender x region (weighted)",
                                "Fisher exact p (2-sided)",
                                spss_values$test_2a$fisher_exact_2sided,
                                p_val,
                                tolerance = 0.001),
      label = "Weighted Fisher exact p"
    )
  }
})

# ============================================================================
# TEST 3: UNWEIGHTED / GROUPED
# ============================================================================

test_that("Fisher test: gender x region, grouped by education", {
  data(survey_data)
  result <- survey_data %>%
    filter(education %in% c("Basic Secondary", "University")) %>%
    group_by(education) %>%
    fisher_test(row = gender, col = region)

  expect_s3_class(result, "fisher_test")
})

# ============================================================================
# TEST 4: WEIGHTED / GROUPED
# ============================================================================

test_that("Fisher test: gender x region, weighted, grouped by education", {
  data(survey_data)
  result <- survey_data %>%
    filter(education %in% c("Basic Secondary", "University")) %>%
    group_by(education) %>%
    fisher_test(row = gender, col = region, weights = sampling_weight)

  expect_s3_class(result, "fisher_test")
})

# ============================================================================
# COMPARISON WITH CHI-SQUARE
# ============================================================================

test_that("Fisher test: p-value consistent with chi_square for large samples", {
  data(survey_data)
  fisher_result <- survey_data %>%
    fisher_test(row = gender, col = region)

  chi_result <- survey_data %>%
    chi_square(gender, region)

  # For large samples, Fisher and Chi-Square should give similar p-values
  fisher_p <- fisher_result$p_value %||% fisher_result$p.value
  chi_p <- chi_result$results$p_value[1]

  # Both should agree on significance direction
  expect_equal(fisher_p < 0.05, chi_p < 0.05)
})

# ============================================================================
# EDGE CASES
# ============================================================================

test_that("Fisher test: handles zero cells", {
  data(survey_data)
  # Create data where some cells might be zero
  small_data <- survey_data %>% filter(id <= 20)

  expect_no_error({
    result <- small_data %>%
      fisher_test(row = gender, col = region)
  })
})

test_that("Fisher test: handles missing values", {
  data(survey_data)
  test_data <- survey_data
  test_data$gender[1:10] <- NA

  expect_no_error({
    result <- test_data %>%
      fisher_test(row = gender, col = region)
  })
})

test_that("Fisher test: error with non-categorical variables", {
  data(survey_data)
  expect_error(
    survey_data %>% fisher_test(row = age, col = income)
  )
})

# ============================================================================
# RESULT STRUCTURE
# ============================================================================

test_that("Fisher test: result structure is complete", {
  data(survey_data)
  result <- survey_data %>%
    fisher_test(row = gender, col = region)

  # Check expected components
  expect_true("p_value" %in% names(result) || "p.value" %in% names(result))
  expect_true("n" %in% names(result))
  expect_true("method" %in% names(result) || is.character(class(result)))
})

# ============================================================================
# PRINT METHOD
# ============================================================================

test_that("Fisher test: print method runs without error", {
  data(survey_data)
  result <- survey_data %>%
    fisher_test(row = gender, col = region)

  expect_output(print(result), "Fisher")
})

test_that("Fisher test: print method for grouped results", {
  data(survey_data)
  result <- survey_data %>%
    filter(education %in% c("Basic Secondary", "University")) %>%
    group_by(education) %>%
    fisher_test(row = gender, col = region)

  output <- capture.output(print(result))
  expect_true(any(grepl("Fisher", output)))
})

# ============================================================================
# VALIDATION REPORT
# ============================================================================

test_that("Validation report summary", {
  total <- length(fisher_validation_results)
  passed <- if (total > 0) sum(vapply(fisher_validation_results, function(r) r$match, logical(1))) else 0L
  failed <- total - passed

  cat("\n")
  cat("==================================================\n")
  cat("  FISHER'S EXACT TEST SPSS VALIDATION REPORT\n")
  cat("==================================================\n")
  cat(sprintf("  Total comparisons: %d\n", total))
  cat(sprintf("  Passed: %d\n", passed))
  cat(sprintf("  Failed: %d\n", failed))
  cat("==================================================\n")

  if (failed > 0) {
    cat("\n  FAILED COMPARISONS:\n")
    for (r in fisher_validation_results) {
      if (!r$match) {
        cat(sprintf("  - %s | %s: expected=%.4f, actual=%.4f, diff=%.6f\n",
                    r$test, r$metric, r$expected, r$actual, r$difference))
      }
    }
  }

  if (total > 0) {
    expect_equal(failed, 0, label = "All SPSS validation comparisons should pass")
  } else {
    expect_true(TRUE, label = "No SPSS values filled yet")
  }
})
