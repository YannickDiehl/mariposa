# ============================================================================
# McNEMAR TEST - SPSS VALIDATION TEST
# ============================================================================
# Purpose: Validate R mcnemar_test() against SPSS CROSSTABS /STATISTICS=MCNEMAR
# Dataset: survey_data
# Variables: Dichotomized trust variables (trust_gov_high, trust_media_high,
#            trust_sci_high) created from trust_government, trust_media,
#            trust_science using median split at 4.
# Split grouping: region (East/West)
# Created: 2026-03-01
# SPSS Version: 29.0
#
# Validation strategy:
# - Create dichotomous variables in R matching SPSS RECODE
# - Compare McNemar chi-square (with continuity correction) and exact p
# - 4 scenarios: unweighted/weighted x ungrouped/grouped
#
# SPSS provides: McNemar Test Exact Sig. (2-sided), Chi-Square,
#                Asymp. Sig., N of Valid Cases, and the 2x2 table
# ============================================================================

library(testthat)
library(dplyr)
library(mariposa)

# ============================================================================
# GLOBAL TRACKING FOR VALIDATION REPORT
# ============================================================================

mcnemar_validation_results <- list()

record_mcnemar_comparison <- function(test_name, metric, expected, actual,
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

  mcnemar_validation_results <<- append(mcnemar_validation_results, list(result))
  return(match_status)
}

# ============================================================================
# DATA PREPARATION
# ============================================================================
# Create dichotomous trust variables matching SPSS RECODE:
#   1-3 -> 0 (Low), 4-7 -> 1 (High)

prepare_mcnemar_data <- function(data) {
  data %>%
    mutate(
      trust_gov_high = as.integer(trust_government >= 4),
      trust_media_high = as.integer(trust_media >= 4),
      trust_sci_high = as.integer(trust_science >= 4)
    )
}

# ============================================================================
# SPSS REFERENCE VALUES
# ============================================================================
# Fill from SPSS output (mcnemar_test.sps)
#
# SPSS CROSSTABS /STATISTICS=MCNEMAR provides:
#   - The 2x2 table (a, b, c, d)
#   - McNemar Test: Exact Sig. (2-sided)
#   - Chi-Square (continuity corrected): (|b-c|-1)^2 / (b+c)
#   - Asymp. Sig.

spss_values <- list(
  # ---- Test 1a: trust_gov_high x trust_media_high (unweighted) ----
  test_1a = list(
    # 2x2 contingency table cells:
    #              trust_media_high=0   trust_media_high=1
    # trust_gov_high=0    a (0,0)          b (0,1)
    # trust_gov_high=1    c (1,0)          d (1,1)
    cell_a = NA_integer_,  # both low
    cell_b = NA_integer_,  # gov low, media high
    cell_c = NA_integer_,  # gov high, media low
    cell_d = NA_integer_,  # both high
    n = NA_integer_,
    chi_sq = NA_real_,           # with continuity correction
    chi_sq_p = NA_real_,         # asymptotic p (chi-sq)
    exact_p = NA_real_           # exact binomial p (2-sided)
  ),

  # ---- Test 1b: trust_gov_high x trust_sci_high (unweighted) ----
  test_1b = list(
    cell_a = NA_integer_,
    cell_b = NA_integer_,
    cell_c = NA_integer_,
    cell_d = NA_integer_,
    n = NA_integer_,
    chi_sq = NA_real_,
    chi_sq_p = NA_real_,
    exact_p = NA_real_
  ),

  # ---- Test 1c: trust_media_high x trust_sci_high (unweighted) ----
  test_1c = list(
    cell_a = NA_integer_,
    cell_b = NA_integer_,
    cell_c = NA_integer_,
    cell_d = NA_integer_,
    n = NA_integer_,
    chi_sq = NA_real_,
    chi_sq_p = NA_real_,
    exact_p = NA_real_
  ),

  # ---- Test 2a: trust_gov_high x trust_media_high (weighted) ----
  test_2a = list(
    n = NA_real_,
    chi_sq = NA_real_,
    chi_sq_p = NA_real_,
    exact_p = NA_real_
  ),

  # ---- Test 2b: trust_gov_high x trust_sci_high (weighted) ----
  test_2b = list(
    n = NA_real_,
    chi_sq = NA_real_,
    chi_sq_p = NA_real_,
    exact_p = NA_real_
  ),

  # ---- Test 3: trust_gov_high x trust_media_high, grouped by region ----
  test_3_east = list(
    n = NA_integer_,
    chi_sq = NA_real_,
    chi_sq_p = NA_real_,
    exact_p = NA_real_
  ),
  test_3_west = list(
    n = NA_integer_,
    chi_sq = NA_real_,
    chi_sq_p = NA_real_,
    exact_p = NA_real_
  ),

  # ---- Test 3b: trust_gov_high x trust_sci_high, grouped by region ----
  test_3b_east = list(
    n = NA_integer_,
    chi_sq = NA_real_,
    exact_p = NA_real_
  ),
  test_3b_west = list(
    n = NA_integer_,
    chi_sq = NA_real_,
    exact_p = NA_real_
  )
)

# ============================================================================
# HELPER: Validate McNemar result against SPSS
# ============================================================================

validate_mcnemar <- function(result, spss_ref, test_label,
                             tolerance_chi = 0.01, tolerance_p = 0.001) {
  if (!is.na(spss_ref$n)) {
    expect_true(
      record_mcnemar_comparison(test_label, "N",
                                 spss_ref$n, result$n,
                                 tolerance = 1),
      label = paste(test_label, "- N")
    )
  }

  if (!is.na(spss_ref$chi_sq)) {
    expect_true(
      record_mcnemar_comparison(test_label, "Chi-Square",
                                 spss_ref$chi_sq, result$statistic,
                                 tolerance = tolerance_chi),
      label = paste(test_label, "- Chi-Square")
    )
  }

  if (!is.na(spss_ref$exact_p)) {
    expect_true(
      record_mcnemar_comparison(test_label, "Exact p (2-sided)",
                                 spss_ref$exact_p, result$exact_p,
                                 tolerance = tolerance_p),
      label = paste(test_label, "- Exact p")
    )
  }
}

# ============================================================================
# TEST 1: UNWEIGHTED / UNGROUPED
# ============================================================================

test_that("McNemar test: trust_gov vs trust_media (unweighted)", {
  data(survey_data)
  test_data <- prepare_mcnemar_data(survey_data)

  result <- test_data %>%
    mcnemar_test(var1 = trust_gov_high, var2 = trust_media_high)

  expect_s3_class(result, "mcnemar_test")

  validate_mcnemar(result, spss_values$test_1a,
                   "1a: trust_gov vs trust_media (unweighted)")
})

test_that("McNemar test: trust_gov vs trust_science (unweighted)", {
  data(survey_data)
  test_data <- prepare_mcnemar_data(survey_data)

  result <- test_data %>%
    mcnemar_test(var1 = trust_gov_high, var2 = trust_sci_high)

  expect_s3_class(result, "mcnemar_test")

  validate_mcnemar(result, spss_values$test_1b,
                   "1b: trust_gov vs trust_sci (unweighted)")
})

test_that("McNemar test: trust_media vs trust_science (unweighted)", {
  data(survey_data)
  test_data <- prepare_mcnemar_data(survey_data)

  result <- test_data %>%
    mcnemar_test(var1 = trust_media_high, var2 = trust_sci_high)

  expect_s3_class(result, "mcnemar_test")

  validate_mcnemar(result, spss_values$test_1c,
                   "1c: trust_media vs trust_sci (unweighted)")
})

# ============================================================================
# TEST 2: WEIGHTED / UNGROUPED
# ============================================================================

test_that("McNemar test: trust_gov vs trust_media (weighted)", {
  data(survey_data)
  test_data <- prepare_mcnemar_data(survey_data)

  result <- test_data %>%
    mcnemar_test(var1 = trust_gov_high, var2 = trust_media_high,
                 weights = sampling_weight)

  expect_s3_class(result, "mcnemar_test")

  validate_mcnemar(result, spss_values$test_2a,
                   "2a: trust_gov vs trust_media (weighted)",
                   tolerance_chi = 0.1, tolerance_p = 0.01)
})

test_that("McNemar test: trust_gov vs trust_science (weighted)", {
  data(survey_data)
  test_data <- prepare_mcnemar_data(survey_data)

  result <- test_data %>%
    mcnemar_test(var1 = trust_gov_high, var2 = trust_sci_high,
                 weights = sampling_weight)

  expect_s3_class(result, "mcnemar_test")

  validate_mcnemar(result, spss_values$test_2b,
                   "2b: trust_gov vs trust_sci (weighted)",
                   tolerance_chi = 0.1, tolerance_p = 0.01)
})

# ============================================================================
# TEST 3: UNWEIGHTED / GROUPED BY REGION
# ============================================================================

test_that("McNemar test: trust_gov vs trust_media, grouped by region", {
  data(survey_data)
  test_data <- prepare_mcnemar_data(survey_data)

  result <- test_data %>%
    group_by(region) %>%
    mcnemar_test(var1 = trust_gov_high, var2 = trust_media_high)

  expect_s3_class(result, "mcnemar_test")
  expect_true(result$is_grouped)
})

test_that("McNemar test: trust_gov vs trust_sci, grouped by region", {
  data(survey_data)
  test_data <- prepare_mcnemar_data(survey_data)

  result <- test_data %>%
    group_by(region) %>%
    mcnemar_test(var1 = trust_gov_high, var2 = trust_sci_high)

  expect_s3_class(result, "mcnemar_test")
})

# ============================================================================
# TEST 4: WEIGHTED / GROUPED
# ============================================================================

test_that("McNemar test: weighted and grouped", {
  data(survey_data)
  test_data <- prepare_mcnemar_data(survey_data)

  result <- test_data %>%
    group_by(region) %>%
    mcnemar_test(var1 = trust_gov_high, var2 = trust_media_high,
                 weights = sampling_weight)

  expect_s3_class(result, "mcnemar_test")
})

# ============================================================================
# CONTINGENCY TABLE VALIDATION
# ============================================================================

test_that("McNemar test: contingency table matches SPSS", {
  data(survey_data)
  test_data <- prepare_mcnemar_data(survey_data)

  result <- test_data %>%
    mcnemar_test(var1 = trust_gov_high, var2 = trust_media_high)

  # Verify result has expected structure

  expect_s3_class(result, "mcnemar_test")

  # Validate the 2x2 table cells if SPSS reference values available
  if ("table" %in% names(result) && !is.na(spss_values$test_1a$cell_a)) {
    tbl <- result$table
    expect_equal(tbl[1, 1], spss_values$test_1a$cell_a, tolerance = 0)
    expect_equal(tbl[1, 2], spss_values$test_1a$cell_b, tolerance = 0)
    expect_equal(tbl[2, 1], spss_values$test_1a$cell_c, tolerance = 0)
    expect_equal(tbl[2, 2], spss_values$test_1a$cell_d, tolerance = 0)
  }
})

# ============================================================================
# EDGE CASES
# ============================================================================

test_that("McNemar test: error when variables are not dichotomous", {
  data(survey_data)
  # trust_government has 7 levels, should error or warn
  expect_error(
    survey_data %>%
      mcnemar_test(var1 = trust_government, var2 = trust_media)
  )
})

test_that("McNemar test: handles missing values", {
  data(survey_data)
  test_data <- prepare_mcnemar_data(survey_data)
  test_data$trust_gov_high[1:10] <- NA

  expect_no_error({
    result <- test_data %>%
      mcnemar_test(var1 = trust_gov_high, var2 = trust_media_high)
  })

  # N should be less than full data
  expect_true(result$n < nrow(survey_data))
})

test_that("McNemar test: symmetric case (no discordant pairs)", {
  # Create data where both variables are identical -> b = c = 0
  test_data <- data.frame(
    var1 = c(0, 0, 1, 1, 0, 1, 0, 1),
    var2 = c(0, 0, 1, 1, 0, 1, 0, 1)
  )

  # Should handle gracefully (p = 1 or NaN with warning)
  expect_no_error({
    result <- test_data %>%
      mcnemar_test(var1 = var1, var2 = var2)
  })
})

# ============================================================================
# PRINT METHOD
# ============================================================================

test_that("McNemar test: print method runs without error", {
  data(survey_data)
  test_data <- prepare_mcnemar_data(survey_data)

  result <- test_data %>%
    mcnemar_test(var1 = trust_gov_high, var2 = trust_media_high)

  expect_output(print(result), "McNemar")
})

test_that("McNemar test: print method for grouped results", {
  data(survey_data)
  test_data <- prepare_mcnemar_data(survey_data)

  result <- test_data %>%
    group_by(region) %>%
    mcnemar_test(var1 = trust_gov_high, var2 = trust_media_high)

  output <- capture.output(print(result))
  expect_true(any(grepl("McNemar", output)))
})

# ============================================================================
# RESULT STRUCTURE
# ============================================================================

test_that("McNemar test: result structure is complete", {
  data(survey_data)
  test_data <- prepare_mcnemar_data(survey_data)

  result <- test_data %>%
    mcnemar_test(var1 = trust_gov_high, var2 = trust_media_high)

  # Expected components
  expect_true("statistic" %in% names(result) || "chi_sq" %in% names(result))
  expect_true("p_value" %in% names(result) || "p.value" %in% names(result))
  expect_true("n" %in% names(result))
  expect_true("exact_p" %in% names(result) || "p_exact" %in% names(result))
})

# ============================================================================
# VALIDATION REPORT
# ============================================================================

test_that("Validation report summary", {
  total <- length(mcnemar_validation_results)
  passed <- if (total > 0) sum(vapply(mcnemar_validation_results, function(r) r$match, logical(1))) else 0L
  failed <- total - passed

  cat("\n")
  cat("==================================================\n")
  cat("  McNEMAR TEST SPSS VALIDATION REPORT\n")
  cat("==================================================\n")
  cat(sprintf("  Total comparisons: %d\n", total))
  cat(sprintf("  Passed: %d\n", passed))
  cat(sprintf("  Failed: %d\n", failed))
  cat("==================================================\n")

  if (failed > 0) {
    cat("\n  FAILED COMPARISONS:\n")
    for (r in mcnemar_validation_results) {
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
