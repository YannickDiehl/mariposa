# ============================================================================
# KRUSKAL-WALLIS TEST - SPSS VALIDATION TEST
# ============================================================================
# Purpose: Validate R kruskal_wallis() against SPSS NPAR TESTS /K-W
# Dataset: survey_data
# Variables: life_satisfaction, income, trust_government
# Grouping factors: education (4 levels), employment (5 levels)
# Split grouping: region (East/West)
# Created: 2026-03-01
# SPSS Version: 29.0
#
# Validation strategy:
# - Unweighted tests (1, 3): Validated against SPSS NPAR TESTS /K-W
# - Weighted tests (2, 4): Since sampling_weight ~ 1.0, SPSS rounds to
#   integer frequency weights producing identical results to unweighted.
#   Weighted R implementation uses design-based weighted ranks.
#
# SPSS coding:
# - education: 1=Basic Secondary, 2=Intermediate Secondary,
#              3=Academic Secondary, 4=University
# - employment: 1=Employed, 2=Other, 3=Retired, 4=Student, 5=Unemployed
#   (alphabetical factor level order from R)
# - region: East, West
# ============================================================================

library(testthat)
library(dplyr)
library(mariposa)

# ============================================================================
# GLOBAL TRACKING FOR VALIDATION REPORT
# ============================================================================

kw_validation_results <- list()

record_kw_comparison <- function(test_name, metric, expected, actual, tolerance = 0) {
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

  kw_validation_results <<- append(kw_validation_results, list(result))
  return(match_status)
}

# ============================================================================
# SPSS REFERENCE VALUES (from kruskal_wallis_output.txt)
# ============================================================================

spss_values <- list(
  # ---- Test 1a: life_satisfaction by education (unweighted, ungrouped) ----
  test_1a = list(
    ranks = list(
      `Basic Secondary`        = list(n = 809,  mean_rank = 974.29),
      `Intermediate Secondary` = list(n = 618,  mean_rank = 1250.73),
      `Academic Secondary`     = list(n = 607,  mean_rank = 1329.56),
      University               = list(n = 387,  mean_rank = 1456.42)
    ),
    total_n = 2421,
    H = 171.178,
    df = 3,
    p = 0.000  # < .001
  ),

  # ---- Test 1b: income by employment (unweighted, ungrouped) ----
  test_1b = list(
    ranks = list(
      Student    = list(n = 65,   mean_rank = 1495.58),
      Employed   = list(n = 1390, mean_rank = 1075.60),
      Unemployed = list(n = 159,  mean_rank = 1073.29),
      Retired    = list(n = 471,  mean_rank = 1089.83),
      Other      = list(n = 101,  mean_rank = 1129.95)
    ),
    total_n = 2186,
    H = 28.026,
    df = 4,
    p = 0.000  # < .001
  ),

  # ---- Test 1c: trust_government by education (unweighted, ungrouped) ----
  test_1c = list(
    ranks = list(
      `Basic Secondary`        = list(n = 791,  mean_rank = 1191.27),
      `Intermediate Secondary` = list(n = 592,  mean_rank = 1156.01),
      `Academic Secondary`     = list(n = 595,  mean_rank = 1170.90),
      University               = list(n = 376,  mean_rank = 1192.82)
    ),
    total_n = 2354,
    H = 1.235,
    df = 3,
    p = 0.745
  ),

  # ---- Test 3a: life_satisfaction by education, grouped by region ----
  test_3a_east = list(
    ranks = list(
      `Basic Secondary`        = list(n = 161,  mean_rank = 202.35),
      `Intermediate Secondary` = list(n = 119,  mean_rank = 232.99),
      `Academic Secondary`     = list(n = 110,  mean_rank = 254.85),
      University               = list(n = 75,   mean_rank = 266.77)
    ),
    total_n = 465,
    H = 17.105,
    df = 3,
    p = 0.001
  ),
  test_3a_west = list(
    ranks = list(
      `Basic Secondary`        = list(n = 648,  mean_rank = 771.38),
      `Intermediate Secondary` = list(n = 499,  mean_rank = 1018.44),
      `Academic Secondary`     = list(n = 497,  mean_rank = 1075.24),
      University               = list(n = 312,  mean_rank = 1190.70)
    ),
    total_n = 1956,
    H = 158.807,
    df = 3,
    p = 0.000
  ),

  # ---- Test 3b: income by employment, grouped by region ----
  test_3b_east = list(
    ranks = list(
      Student    = list(n = 8,    mean_rank = 294.19),
      Employed   = list(n = 273,  mean_rank = 213.94),
      Unemployed = list(n = 28,   mean_rank = 170.63),
      Retired    = list(n = 100,  mean_rank = 230.44),
      Other      = list(n = 20,   mean_rank = 182.68)
    ),
    total_n = 429,
    H = 9.787,
    df = 4,
    p = 0.044
  ),
  test_3b_west = list(
    ranks = list(
      Student    = list(n = 57,   mean_rank = 1201.74),
      Employed   = list(n = 1117, mean_rank = 862.13),
      Unemployed = list(n = 131,  mean_rank = 898.09),
      Retired    = list(n = 371,  mean_rank = 858.55),
      Other      = list(n = 81,   mean_rank = 947.28)
    ),
    total_n = 1757,
    H = 26.570,
    df = 4,
    p = 0.000
  )
)

# ============================================================================
# HELPER: Validate a single Kruskal-Wallis result against SPSS reference
# ============================================================================

validate_kw_test <- function(result_row, spss_ref, test_label) {
  # Extract group stats
  gs <- result_row$group_stats[[1]]

  # Validate H statistic
  expect_true(
    record_kw_comparison(test_label, "H statistic",
                         spss_ref$H, result_row$H, tolerance = 0.01),
    label = paste(test_label, "- H statistic")
  )

  # Validate df
  expect_true(
    record_kw_comparison(test_label, "df",
                         spss_ref$df, result_row$df, tolerance = 0),
    label = paste(test_label, "- df")
  )

  # Validate p-value
  if (spss_ref$p == 0) {
    # SPSS shows .000 meaning < .001
    expect_true(
      result_row$p_value < 0.001,
      label = paste(test_label, "- p < .001")
    )
    record_kw_comparison(test_label, "p < .001", TRUE, result_row$p_value < 0.001)
  } else {
    expect_true(
      record_kw_comparison(test_label, "p-value",
                           spss_ref$p, result_row$p_value, tolerance = 0.001),
      label = paste(test_label, "- p-value")
    )
  }

  # Validate N per group and mean ranks
  for (grp_name in names(spss_ref$ranks)) {
    ref_grp <- spss_ref$ranks[[grp_name]]

    # Find matching group in results
    matched <- NULL
    for (s in gs) {
      if (s$name == grp_name) {
        matched <- s
        break
      }
    }

    expect_false(is.null(matched),
                 label = paste(test_label, "- group found:", grp_name))

    if (!is.null(matched)) {
      # N
      expect_true(
        record_kw_comparison(test_label, paste("N -", grp_name),
                             ref_grp$n, matched$n, tolerance = 0),
        label = paste(test_label, "- N for", grp_name)
      )

      # Mean rank
      expect_true(
        record_kw_comparison(test_label, paste("Mean Rank -", grp_name),
                             ref_grp$mean_rank, matched$rank_mean, tolerance = 0.01),
        label = paste(test_label, "- Mean Rank for", grp_name)
      )
    }
  }
}


# ============================================================================
# TEST 1: UNWEIGHTED / UNGROUPED
# ============================================================================

test_that("Kruskal-Wallis: unweighted ungrouped - life_satisfaction by education", {
  data(survey_data)
  result <- survey_data %>%
    kruskal_wallis(life_satisfaction, group = education)

  expect_s3_class(result, "kruskal_wallis")
  expect_equal(nrow(result$results), 1)

  validate_kw_test(result$results[1, ], spss_values$test_1a,
                   "1a: life_satisfaction by education (unweighted)")
})

test_that("Kruskal-Wallis: unweighted ungrouped - income by employment", {
  data(survey_data)
  result <- survey_data %>%
    kruskal_wallis(income, group = employment)

  expect_s3_class(result, "kruskal_wallis")

  validate_kw_test(result$results[1, ], spss_values$test_1b,
                   "1b: income by employment (unweighted)")
})

test_that("Kruskal-Wallis: unweighted ungrouped - trust_government by education (non-sig)", {
  data(survey_data)
  result <- survey_data %>%
    kruskal_wallis(trust_government, group = education)

  expect_s3_class(result, "kruskal_wallis")

  validate_kw_test(result$results[1, ], spss_values$test_1c,
                   "1c: trust_government by education (unweighted, non-sig)")
})

# ============================================================================
# TEST 2: WEIGHTED / UNGROUPED
# ============================================================================
# Note: SPSS weighted output is identical to unweighted because
# sampling_weight ~ 1.0 and SPSS rounds frequency weights to integers.
# R's design-based weighted approach may produce slightly different values.

test_that("Kruskal-Wallis: weighted ungrouped - life_satisfaction by education", {
  data(survey_data)
  result <- survey_data %>%
    kruskal_wallis(life_satisfaction, group = education, weights = sampling_weight)

  expect_s3_class(result, "kruskal_wallis")
  expect_equal(result$weights, "sampling_weight")

  # With weights ~ 1, results should be very close to unweighted
  expect_equal(result$results$H, spss_values$test_1a$H, tolerance = 1.0)
  expect_equal(result$results$df, spss_values$test_1a$df)
})

test_that("Kruskal-Wallis: weighted ungrouped - income by employment", {
  data(survey_data)
  result <- survey_data %>%
    kruskal_wallis(income, group = employment, weights = sampling_weight)

  expect_s3_class(result, "kruskal_wallis")

  # With weights ~ 1, results should be close
  expect_equal(result$results$H, spss_values$test_1b$H, tolerance = 1.0)
  expect_equal(result$results$df, spss_values$test_1b$df)
})

# ============================================================================
# TEST 3: UNWEIGHTED / GROUPED BY REGION
# ============================================================================

test_that("Kruskal-Wallis: unweighted grouped - life_satisfaction by education, by region", {
  data(survey_data)
  result <- survey_data %>%
    group_by(region) %>%
    kruskal_wallis(life_satisfaction, group = education)

  expect_s3_class(result, "kruskal_wallis")
  expect_true(result$is_grouped)
  expect_equal(nrow(result$results), 2)  # East and West

  # East
  east_row <- result$results[result$results$region == "East", ]
  validate_kw_test(east_row, spss_values$test_3a_east,
                   "3a-East: life_satisfaction by education (grouped)")

  # West
  west_row <- result$results[result$results$region == "West", ]
  validate_kw_test(west_row, spss_values$test_3a_west,
                   "3a-West: life_satisfaction by education (grouped)")
})

test_that("Kruskal-Wallis: unweighted grouped - income by employment, by region", {
  data(survey_data)
  result <- survey_data %>%
    group_by(region) %>%
    kruskal_wallis(income, group = employment)

  expect_s3_class(result, "kruskal_wallis")
  expect_equal(nrow(result$results), 2)

  # East
  east_row <- result$results[result$results$region == "East", ]
  validate_kw_test(east_row, spss_values$test_3b_east,
                   "3b-East: income by employment (grouped)")

  # West
  west_row <- result$results[result$results$region == "West", ]
  validate_kw_test(west_row, spss_values$test_3b_west,
                   "3b-West: income by employment (grouped)")
})

# ============================================================================
# TEST 4: WEIGHTED / GROUPED BY REGION
# ============================================================================

test_that("Kruskal-Wallis: weighted grouped - life_satisfaction by education, by region", {
  data(survey_data)
  result <- survey_data %>%
    group_by(region) %>%
    kruskal_wallis(life_satisfaction, group = education, weights = sampling_weight)

  expect_s3_class(result, "kruskal_wallis")
  expect_true(result$is_grouped)
  expect_equal(nrow(result$results), 2)

  # Weighted results should be close to unweighted SPSS values (weights ~ 1)
  east_row <- result$results[result$results$region == "East", ]
  expect_equal(east_row$H, spss_values$test_3a_east$H, tolerance = 1.0)
  expect_equal(east_row$df, spss_values$test_3a_east$df)

  west_row <- result$results[result$results$region == "West", ]
  expect_equal(west_row$H, spss_values$test_3a_west$H, tolerance = 1.0)
  expect_equal(west_row$df, spss_values$test_3a_west$df)
})

# ============================================================================
# MULTI-VARIABLE TESTS
# ============================================================================

test_that("Kruskal-Wallis: multiple variables in one call", {
  data(survey_data)
  result <- survey_data %>%
    kruskal_wallis(life_satisfaction, income, trust_government, group = education)

  expect_s3_class(result, "kruskal_wallis")
  expect_equal(nrow(result$results), 3)
  expect_equal(result$results$Variable, c("life_satisfaction", "income", "trust_government"))

  # Validate each variable against known SPSS values
  validate_kw_test(result$results[1, ], spss_values$test_1a,
                   "multi: life_satisfaction")
  # Note: income by education not in SPSS reference, but test runs
  validate_kw_test(result$results[3, ], spss_values$test_1c,
                   "multi: trust_government")
})

test_that("Kruskal-Wallis: tidyselect with starts_with", {
  data(survey_data)
  result <- survey_data %>%
    kruskal_wallis(starts_with("trust_"), group = education)

  expect_s3_class(result, "kruskal_wallis")
  expect_true(nrow(result$results) >= 1)
  # trust_government should be among the variables
  expect_true("trust_government" %in% result$results$Variable)
})

# ============================================================================
# EDGE CASES AND INPUT VALIDATION
# ============================================================================

test_that("Kruskal-Wallis: error with less than 2 groups", {
  data(survey_data)
  single_group <- survey_data %>% filter(education == "University")

  expect_error(
    single_group %>% kruskal_wallis(life_satisfaction, group = education),
    "at least 2"
  )
})

test_that("Kruskal-Wallis: error without group argument", {
  data(survey_data)
  expect_error(
    survey_data %>% kruskal_wallis(life_satisfaction),
    "group.*required|is required"
  )
})

test_that("Kruskal-Wallis: works with 2 groups (edge case)", {
  data(survey_data)
  # region has exactly 2 levels - should work but Mann-Whitney is preferred
  result <- survey_data %>%
    kruskal_wallis(life_satisfaction, group = region)

  expect_s3_class(result, "kruskal_wallis")
  expect_equal(result$results$df, 1)
})

test_that("Kruskal-Wallis: handles missing values correctly", {
  data(survey_data)
  # income has missing values - should be excluded pairwise
  result <- survey_data %>%
    kruskal_wallis(income, group = education)

  expect_s3_class(result, "kruskal_wallis")
  # N should be less than total due to missing income values
  expect_true(result$results$n_total < nrow(survey_data))
})

test_that("Kruskal-Wallis: print method runs without error", {
  data(survey_data)
  result <- survey_data %>%
    kruskal_wallis(life_satisfaction, group = education)

  # cli output (headers, significance legend) goes to message stream,
  # so only check cat()-generated content
  expect_output(print(result), "Kruskal-Wallis H")
  expect_output(print(result), "Mean Rank")
  expect_output(print(result), "Eta-squared")
})

test_that("Kruskal-Wallis: print method for grouped results", {
  data(survey_data)
  result <- survey_data %>%
    group_by(region) %>%
    kruskal_wallis(life_satisfaction, group = education)

  # Grouped output should produce cat()-based content for both groups
  # cli_h2 group headers go to message stream, so check table data presence
  expect_output(print(result), "Mean Rank")
  expect_output(print(result), "Kruskal-Wallis H")
  # Verify both groups produce output (East has ~465 obs, West has ~1956)
  expect_output(print(result), "Total")
})

# ============================================================================
# EFFECT SIZE VALIDATION
# ============================================================================

test_that("Kruskal-Wallis: eta-squared is correctly computed", {
  data(survey_data)
  result <- survey_data %>%
    kruskal_wallis(life_satisfaction, group = education)

  # Eta-squared = H / (N - 1)
  expected_eta <- spss_values$test_1a$H / (spss_values$test_1a$total_n - 1)
  expect_equal(result$results$eta_squared, expected_eta, tolerance = 0.001)
})

# ============================================================================
# VALIDATION REPORT
# ============================================================================

test_that("Validation report summary", {
  total <- length(kw_validation_results)
  passed <- sum(sapply(kw_validation_results, function(r) r$match))
  failed <- total - passed

  cat("\n")
  cat("==================================================\n")
  cat("  KRUSKAL-WALLIS SPSS VALIDATION REPORT\n")
  cat("==================================================\n")
  cat(sprintf("  Total comparisons: %d\n", total))
  cat(sprintf("  Passed: %d\n", passed))
  cat(sprintf("  Failed: %d\n", failed))
  cat("==================================================\n")

  if (failed > 0) {
    cat("\n  FAILED COMPARISONS:\n")
    for (r in kw_validation_results) {
      if (!r$match) {
        cat(sprintf("  - %s | %s: expected=%.4f, actual=%.4f, diff=%.6f\n",
                    r$test, r$metric, r$expected, r$actual, r$difference))
      }
    }
  }

  expect_equal(failed, 0, label = "All SPSS validation comparisons should pass")
})
