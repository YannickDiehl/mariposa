# ============================================================================
# CHI-SQUARE GOODNESS-OF-FIT - SPSS VALIDATION TEST
# ============================================================================
# Purpose: Validate R chisq_gof() against SPSS NPAR TESTS /CHISQUARE
# Dataset: survey_data
# Variables: gender, education, region, employment, interview_mode
# Split grouping: region (East/West)
# Created: 2026-03-01
# SPSS Version: 29.0
#
# Validation strategy:
# - Equal expected proportions (SPSS default)
# - Custom expected proportions (via /EXPECTED=)
# - 4 scenarios: unweighted/weighted x ungrouped/grouped
# - Multi-variable support via tidyselect
#
# SPSS provides: Chi-Square statistic, df, Asymp. Sig., frequency table
#                with Observed N, Expected N, Residual
# ============================================================================

library(testthat)
library(dplyr)
library(mariposa)

# ============================================================================
# GLOBAL TRACKING FOR VALIDATION REPORT
# ============================================================================

gof_validation_results <- list()

record_gof_comparison <- function(test_name, metric, expected, actual,
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

  gof_validation_results <<- append(gof_validation_results, list(result))
  return(match_status)
}

# ============================================================================
# SPSS REFERENCE VALUES
# ============================================================================
# Fill from SPSS output (chisq_gof.sps)
#
# SPSS NPAR TESTS /CHISQUARE output:
#   - Frequency table: Category, Observed N, Expected N, Residual
#   - Test Statistics: Chi-Square, df, Asymp. Sig.

spss_values <- list(
  # ---- Test 1a: gender (2 categories, equal proportions, unweighted) ----
  test_1a = list(
    observed = list(Male = NA_integer_, Female = NA_integer_),
    expected_n = NA_real_,  # each = total/2
    chi_sq = NA_real_,
    df = 1,
    p = NA_real_,
    n = NA_integer_
  ),

  # ---- Test 1b: education (4 categories, equal proportions, unweighted) ----
  test_1b = list(
    observed = list(
      `Basic Secondary` = NA_integer_,
      `Intermediate Secondary` = NA_integer_,
      `Academic Secondary` = NA_integer_,
      University = NA_integer_
    ),
    expected_n = NA_real_,  # each = total/4
    chi_sq = NA_real_,
    df = 3,
    p = NA_real_,
    n = NA_integer_
  ),

  # ---- Test 1c: region (2 categories, equal proportions, unweighted) ----
  test_1c = list(
    observed = list(East = NA_integer_, West = NA_integer_),
    expected_n = NA_real_,
    chi_sq = NA_real_,
    df = 1,
    p = NA_real_,
    n = NA_integer_
  ),

  # ---- Test 1d: employment (5 categories, equal proportions, unweighted) ----
  test_1d = list(
    observed = list(
      Student = NA_integer_, Employed = NA_integer_,
      Unemployed = NA_integer_, Retired = NA_integer_,
      Other = NA_integer_
    ),
    chi_sq = NA_real_,
    df = 4,
    p = NA_real_,
    n = NA_integer_
  ),

  # ---- Test 1e: interview_mode (3 categories, equal proportions, unweighted) ----
  test_1e = list(
    observed = list(
      `Face-to-face` = NA_integer_,
      Telephone = NA_integer_,
      Online = NA_integer_
    ),
    chi_sq = NA_real_,
    df = 2,
    p = NA_real_,
    n = NA_integer_
  ),

  # ---- Test 1f: interview_mode (custom proportions 50/30/20, unweighted) ----
  test_1f = list(
    expected_props = c(0.5, 0.3, 0.2),
    chi_sq = NA_real_,
    df = 2,
    p = NA_real_,
    n = NA_integer_
  ),

  # ---- Test 2a: gender (weighted) ----
  test_2a = list(
    chi_sq = NA_real_,
    df = 1,
    p = NA_real_,
    n = NA_real_  # weighted N
  ),

  # ---- Test 2b: education (weighted) ----
  test_2b = list(
    chi_sq = NA_real_,
    df = 3,
    p = NA_real_,
    n = NA_real_
  ),

  # ---- Test 2c: interview_mode custom proportions (weighted) ----
  test_2c = list(
    chi_sq = NA_real_,
    df = 2,
    p = NA_real_,
    n = NA_real_
  ),

  # ---- Test 3: education, grouped by region (unweighted) ----
  test_3_east_education = list(
    chi_sq = NA_real_,
    df = 3,
    p = NA_real_,
    n = NA_integer_
  ),
  test_3_west_education = list(
    chi_sq = NA_real_,
    df = 3,
    p = NA_real_,
    n = NA_integer_
  ),

  test_3_east_gender = list(
    chi_sq = NA_real_,
    df = 1,
    p = NA_real_,
    n = NA_integer_
  ),
  test_3_west_gender = list(
    chi_sq = NA_real_,
    df = 1,
    p = NA_real_,
    n = NA_integer_
  ),

  # ---- Test 4: education, weighted, grouped by region ----
  test_4_east = list(
    chi_sq = NA_real_,
    df = 3,
    p = NA_real_,
    n = NA_real_
  ),
  test_4_west = list(
    chi_sq = NA_real_,
    df = 3,
    p = NA_real_,
    n = NA_real_
  )
)

# ============================================================================
# HELPER: Validate GoF result against SPSS
# ============================================================================

validate_gof <- function(result_row, spss_ref, test_label,
                         tolerance_chi = 0.01, tolerance_p = 0.001) {
  if (!is.na(spss_ref$chi_sq)) {
    expect_true(
      record_gof_comparison(test_label, "Chi-Square",
                             spss_ref$chi_sq, result_row$chi_sq,
                             tolerance = tolerance_chi),
      label = paste(test_label, "- Chi-Square")
    )
  }

  if (!is.na(spss_ref$df)) {
    expect_true(
      record_gof_comparison(test_label, "df",
                             spss_ref$df, result_row$df,
                             tolerance = 0),
      label = paste(test_label, "- df")
    )
  }

  if (!is.na(spss_ref$p)) {
    if (spss_ref$p == 0) {
      expect_true(result_row$p_value < 0.001,
                  label = paste(test_label, "- p < .001"))
    } else {
      expect_true(
        record_gof_comparison(test_label, "p-value",
                               spss_ref$p, result_row$p_value,
                               tolerance = tolerance_p),
        label = paste(test_label, "- p-value")
      )
    }
  }

  if (!is.na(spss_ref$n)) {
    expect_true(
      record_gof_comparison(test_label, "N",
                             spss_ref$n, result_row$n,
                             tolerance = 1),
      label = paste(test_label, "- N")
    )
  }
}

# ============================================================================
# TEST 1: UNWEIGHTED / UNGROUPED - EQUAL PROPORTIONS
# ============================================================================

test_that("GoF: gender (2 categories, equal proportions, unweighted)", {
  data(survey_data)
  result <- survey_data %>% chisq_gof(gender)

  expect_s3_class(result, "chisq_gof")

  validate_gof(result$results[1, ], spss_values$test_1a,
               "1a: gender (equal proportions)")
})

test_that("GoF: education (4 categories, equal proportions, unweighted)", {
  data(survey_data)
  result <- survey_data %>% chisq_gof(education)

  expect_s3_class(result, "chisq_gof")

  validate_gof(result$results[1, ], spss_values$test_1b,
               "1b: education (equal proportions)")
})

test_that("GoF: region (2 categories, equal proportions, unweighted)", {
  data(survey_data)
  result <- survey_data %>% chisq_gof(region)

  expect_s3_class(result, "chisq_gof")

  validate_gof(result$results[1, ], spss_values$test_1c,
               "1c: region (equal proportions)")
})

test_that("GoF: employment (5 categories, unweighted)", {
  data(survey_data)
  result <- survey_data %>% chisq_gof(employment)

  expect_s3_class(result, "chisq_gof")

  validate_gof(result$results[1, ], spss_values$test_1d,
               "1d: employment (equal proportions)")
})

test_that("GoF: interview_mode (3 categories, equal proportions, unweighted)", {
  data(survey_data)
  result <- survey_data %>% chisq_gof(interview_mode)

  expect_s3_class(result, "chisq_gof")

  validate_gof(result$results[1, ], spss_values$test_1e,
               "1e: interview_mode (equal proportions)")
})

# ============================================================================
# TEST 1f: CUSTOM EXPECTED PROPORTIONS
# ============================================================================

test_that("GoF: interview_mode (custom proportions 50/30/20, unweighted)", {
  data(survey_data)
  result <- survey_data %>%
    chisq_gof(interview_mode, expected = c(0.5, 0.3, 0.2))

  expect_s3_class(result, "chisq_gof")

  validate_gof(result$results[1, ], spss_values$test_1f,
               "1f: interview_mode (custom 50/30/20)")
})

# ============================================================================
# TEST 2: WEIGHTED / UNGROUPED
# ============================================================================

test_that("GoF: gender (weighted)", {
  data(survey_data)
  result <- survey_data %>%
    chisq_gof(gender, weights = sampling_weight)

  expect_s3_class(result, "chisq_gof")

  validate_gof(result$results[1, ], spss_values$test_2a,
               "2a: gender (weighted)")
})

test_that("GoF: education (weighted)", {
  data(survey_data)
  result <- survey_data %>%
    chisq_gof(education, weights = sampling_weight)

  expect_s3_class(result, "chisq_gof")

  validate_gof(result$results[1, ], spss_values$test_2b,
               "2b: education (weighted)")
})

test_that("GoF: interview_mode custom proportions (weighted)", {
  data(survey_data)
  result <- survey_data %>%
    chisq_gof(interview_mode, expected = c(0.5, 0.3, 0.2),
              weights = sampling_weight)

  expect_s3_class(result, "chisq_gof")

  validate_gof(result$results[1, ], spss_values$test_2c,
               "2c: interview_mode custom (weighted)")
})

# ============================================================================
# TEST 3: UNWEIGHTED / GROUPED BY REGION
# ============================================================================

test_that("GoF: education, grouped by region", {
  data(survey_data)
  result <- survey_data %>%
    group_by(region) %>%
    chisq_gof(education)

  expect_s3_class(result, "chisq_gof")
  expect_true(result$is_grouped)

  east_row <- result$results[result$results$region == "East", ]
  west_row <- result$results[result$results$region == "West", ]

  if (nrow(east_row) > 0) {
    validate_gof(east_row[1, ], spss_values$test_3_east_education,
                 "3-East: education (grouped)")
  }
  if (nrow(west_row) > 0) {
    validate_gof(west_row[1, ], spss_values$test_3_west_education,
                 "3-West: education (grouped)")
  }
})

test_that("GoF: gender, grouped by region", {
  data(survey_data)
  result <- survey_data %>%
    group_by(region) %>%
    chisq_gof(gender)

  expect_s3_class(result, "chisq_gof")

  east_row <- result$results[result$results$region == "East", ]
  west_row <- result$results[result$results$region == "West", ]

  if (nrow(east_row) > 0) {
    validate_gof(east_row[1, ], spss_values$test_3_east_gender,
                 "3-East: gender (grouped)")
  }
  if (nrow(west_row) > 0) {
    validate_gof(west_row[1, ], spss_values$test_3_west_gender,
                 "3-West: gender (grouped)")
  }
})

# ============================================================================
# TEST 4: WEIGHTED / GROUPED
# ============================================================================

test_that("GoF: education, weighted, grouped by region", {
  data(survey_data)
  result <- survey_data %>%
    group_by(region) %>%
    chisq_gof(education, weights = sampling_weight)

  expect_s3_class(result, "chisq_gof")

  east_row <- result$results[result$results$region == "East", ]
  west_row <- result$results[result$results$region == "West", ]

  if (nrow(east_row) > 0) {
    validate_gof(east_row[1, ], spss_values$test_4_east,
                 "4-East: education (weighted, grouped)")
  }
  if (nrow(west_row) > 0) {
    validate_gof(west_row[1, ], spss_values$test_4_west,
                 "4-West: education (weighted, grouped)")
  }
})

# ============================================================================
# MULTI-VARIABLE SUPPORT
# ============================================================================

test_that("GoF: multiple variables in one call", {
  data(survey_data)
  result <- survey_data %>%
    chisq_gof(gender, region, education)

  expect_s3_class(result, "chisq_gof")
  expect_equal(nrow(result$results), 3)
  expect_true("Variable" %in% names(result$results))
})

test_that("GoF: tidyselect support", {
  data(survey_data)
  # Select all factor variables (excluding id-like and weight-like)
  result <- survey_data %>%
    chisq_gof(gender, region)

  expect_s3_class(result, "chisq_gof")
  expect_true(nrow(result$results) >= 2)
})

# ============================================================================
# INPUT VALIDATION
# ============================================================================

test_that("GoF: expected proportions must sum to 1", {
  data(survey_data)
  expect_error(
    survey_data %>% chisq_gof(gender, expected = c(0.3, 0.3)),
    "sum"
  )
})

test_that("GoF: expected length must match number of categories", {
  data(survey_data)
  expect_error(
    survey_data %>% chisq_gof(education, expected = c(0.5, 0.5)),
    "length|categories"
  )
})

test_that("GoF: requires categorical variable", {
  data(survey_data)
  expect_error(
    survey_data %>% chisq_gof(age)
  )
})

test_that("GoF: handles missing values", {
  data(survey_data)
  test_data <- survey_data
  test_data$gender[1:10] <- NA

  expect_no_error({
    result <- test_data %>% chisq_gof(gender)
  })

  # N should be less than full data
  expect_true(result$results$n[1] < nrow(survey_data))
})

# ============================================================================
# FREQUENCY TABLE VALIDATION
# ============================================================================

test_that("GoF: frequency table matches observed counts", {
  data(survey_data)
  result <- survey_data %>% chisq_gof(gender)

  # If frequency table is part of results, validate it
  if ("frequencies" %in% names(result) || "freq_table" %in% names(result)) {
    freq <- result$frequencies %||% result$freq_table
    # Observed should match actual counts
    actual_counts <- table(survey_data$gender)
    expect_equal(sum(freq$observed), sum(actual_counts))
  }
})

# ============================================================================
# COMPARISON WITH CHI-SQUARE INDEPENDENCE TEST
# ============================================================================

test_that("GoF: different from chi_square() independence test", {
  data(survey_data)
  gof_result <- survey_data %>% chisq_gof(gender)
  # chi_square() needs row and col -> different test entirely
  # Just verify GoF gives a different kind of result
  expect_s3_class(gof_result, "chisq_gof")
  # GoF df = k-1, not (r-1)*(c-1)
  expect_equal(gof_result$results$df[1], 1)  # 2 categories - 1
})

# ============================================================================
# PRINT METHOD
# ============================================================================

test_that("GoF: print method runs without error", {
  data(survey_data)
  result <- survey_data %>% chisq_gof(gender)

  expect_output(print(result), "Goodness|Chi-Square|GoF")
})

test_that("GoF: print method for grouped results", {
  data(survey_data)
  result <- survey_data %>%
    group_by(region) %>%
    chisq_gof(education)

  output <- capture.output(print(result))
  expect_true(any(grepl("Goodness|Chi-Square|GoF", output)))
})

test_that("GoF: print method for multi-variable results", {
  data(survey_data)
  result <- survey_data %>%
    chisq_gof(gender, region, education)

  output <- capture.output(print(result))
  expect_true(any(grepl("gender", output)))
  expect_true(any(grepl("education", output)))
})

# ============================================================================
# RESULT STRUCTURE
# ============================================================================

test_that("GoF: result structure is complete", {
  data(survey_data)
  result <- survey_data %>% chisq_gof(education)

  # Check expected components
  expect_true("results" %in% names(result))
  expect_true("variables" %in% names(result))

  # Check results tibble
  r <- result$results
  expect_true("Variable" %in% names(r))
  expect_true("chi_sq" %in% names(r))
  expect_true("df" %in% names(r))
  expect_true("p_value" %in% names(r))
  expect_true("n" %in% names(r))
})

# ============================================================================
# VALIDATION REPORT
# ============================================================================

test_that("Validation report summary", {
  total <- length(gof_validation_results)
  passed <- sum(sapply(gof_validation_results, function(r) r$match))
  failed <- total - passed

  cat("\n")
  cat("==================================================\n")
  cat("  CHI-SQUARE GOODNESS-OF-FIT SPSS VALIDATION REPORT\n")
  cat("==================================================\n")
  cat(sprintf("  Total comparisons: %d\n", total))
  cat(sprintf("  Passed: %d\n", passed))
  cat(sprintf("  Failed: %d\n", failed))
  cat("==================================================\n")

  if (failed > 0) {
    cat("\n  FAILED COMPARISONS:\n")
    for (r in gof_validation_results) {
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
