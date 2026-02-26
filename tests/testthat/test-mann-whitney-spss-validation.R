# ============================================================================
# MANN-WHITNEY U TEST - SPSS VALIDATION TEST
# ============================================================================
# Purpose: Validate R mann_whitney() against SPSS NPAR TESTS /M-W
# Dataset: survey_data
# Variables: life_satisfaction, income, age
# Grouping: gender (group), region (grouped analysis)
# Created: 2026-02-26
# SPSS Version: 29.0
#
# Validation strategy:
# - Unweighted tests (1, 3): Validated against SPSS NPAR TESTS /M-W
# - Weighted tests (2, 4): Validated against survey::svyranktest()
#
# Note on weighted validation:
# SPSS NPAR TESTS rounds fractional frequency weights to integers.
# Since survey_data sampling_weight ranges 0.7-1.4, all weights round to 1,
# making SPSS weighted results identical to unweighted. Therefore weighted
# tests validate against survey::svyranktest() (Lumley & Scott, 2013).
# mann_whitney() implements the same algorithm, so exact match is expected.
# ============================================================================

library(testthat)
library(dplyr)
library(mariposa)

# ============================================================================
# GLOBAL TRACKING FOR VALIDATION REPORT
# ============================================================================

mw_validation_results <- list()

record_mw_comparison <- function(test_name, metric, expected, actual, tolerance = 0) {
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

  mw_validation_results <<- append(mw_validation_results, list(result))
  return(match_status)
}

# ============================================================================
# SPSS REFERENCE VALUES (from mann_whitney_output.txt)
# ============================================================================
# Gender coding in SPSS .sav: Male=1, Female=2
# Gender in R: factor with levels c("Male", "Female") -> Male=group1, Female=group2
#
# SPSS reports:
# - Mann-Whitney U: min(U1, U2)
# - Wilcoxon W: rank sum of the group with smaller U
# - Z: normal approximation without continuity correction
# - Asymp. Sig. (2-tailed): two-tailed p-value from Z

spss_values <- list(
  # ---- Test 1: Unweighted/Ungrouped ----
  test_1a_life_satisfaction = list(
    male_n = 1149, male_mean_rank = 1196.71, male_sum_rank = 1375022.00,
    female_n = 1272, female_mean_rank = 1223.91, female_sum_rank = 1556809.00,
    total_n = 2421,
    U = 714347.000, W = 1375022.000, Z = -0.989, p = 0.323
  ),
  test_1b_income = list(
    male_n = 1046, male_mean_rank = 1103.31, male_sum_rank = 1154058.00,
    female_n = 1140, female_mean_rank = 1084.50, female_sum_rank = 1236333.00,
    total_n = 2186,
    U = 585963.000, W = 1236333.000, Z = -0.696, p = 0.486
  ),
  test_1c_age = list(
    male_n = 1194, male_mean_rank = 1248.03, male_sum_rank = 1490147.00,
    female_n = 1306, female_mean_rank = 1252.76, female_sum_rank = 1636103.00,
    total_n = 2500,
    U = 776732.000, W = 1490147.000, Z = -0.164, p = 0.870
  ),

  # ---- Test 3: Unweighted/Grouped by region ----
  # 3a: life_satisfaction by gender, grouped by region
  test_3a_life_satisfaction_east = list(
    male_n = 228, male_mean_rank = 237.05, male_sum_rank = 54046.50,
    female_n = 237, female_mean_rank = 229.11, female_sum_rank = 54298.50,
    total_n = 465,
    U = 26095.500, W = 54298.500, Z = -0.658, p = 0.510
  ),
  test_3a_life_satisfaction_west = list(
    male_n = 921, male_mean_rank = 959.57, male_sum_rank = 883762.00,
    female_n = 1035, female_mean_rank = 995.35, female_sum_rank = 1030184.00,
    total_n = 1956,
    U = 459181.000, W = 883762.000, Z = -1.447, p = 0.148
  ),

  # 3b: income by gender, grouped by region
  test_3b_income_east = list(
    male_n = 207, male_mean_rank = 223.46, male_sum_rank = 46255.50,
    female_n = 222, female_mean_rank = 207.11, female_sum_rank = 45979.50,
    total_n = 429,
    U = 21226.500, W = 45979.500, Z = -1.365, p = 0.172
  ),
  test_3b_income_west = list(
    male_n = 839, male_mean_rank = 880.58, male_sum_rank = 738810.00,
    female_n = 918, female_mean_rank = 877.55, female_sum_rank = 805593.00,
    total_n = 1757,
    U = 383772.000, W = 805593.000, Z = -0.125, p = 0.900
  ),

  # 3c: age by gender, grouped by region
  test_3c_age_east = list(
    male_n = 238, male_mean_rank = 237.11, male_sum_rank = 56433.00,
    female_n = 247, female_mean_rank = 248.67, female_sum_rank = 61422.00,
    total_n = 485,
    U = 27992.000, W = 56433.000, Z = -0.908, p = 0.364
  ),
  test_3c_age_west = list(
    male_n = 956, male_mean_rank = 1011.36, male_sum_rank = 966859.00,
    female_n = 1059, female_mean_rank = 1004.97, female_sum_rank = 1064261.00,
    total_n = 2015,
    U = 502991.000, W = 1064261.000, Z = -0.246, p = 0.805
  )
)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Compare Mann-Whitney results with SPSS reference values
compare_mw_with_spss <- function(r_result, spss_ref, test_name,
                                  tolerance_U = 1,
                                  tolerance_W = 1,
                                  tolerance_Z = 0.01,
                                  tolerance_p = 0.005,
                                  tolerance_n = 0,
                                  tolerance_rank = 0.1) {

  # Extract R results row
  r_stats <- r_result$results[1, ]

  # Mann-Whitney U statistic
  record_mw_comparison(test_name, "U", spss_ref$U, r_stats$U, tolerance_U)
  expect_equal(r_stats$U, spss_ref$U, tolerance = tolerance_U,
               label = paste(test_name, "- Mann-Whitney U"))

  # Wilcoxon W
  record_mw_comparison(test_name, "W", spss_ref$W, r_stats$W, tolerance_W)
  expect_equal(r_stats$W, spss_ref$W, tolerance = tolerance_W,
               label = paste(test_name, "- Wilcoxon W"))

  # Z statistic
  record_mw_comparison(test_name, "Z", spss_ref$Z, r_stats$Z, tolerance_Z)
  expect_equal(r_stats$Z, spss_ref$Z, tolerance = tolerance_Z,
               label = paste(test_name, "- Z statistic"))

  # P-value (R uses wilcox.test with continuity correction, SPSS does not,
  # so slightly wider tolerance)
  record_mw_comparison(test_name, "p_value", spss_ref$p, r_stats$p_value, tolerance_p)
  expect_equal(r_stats$p_value, spss_ref$p, tolerance = tolerance_p,
               label = paste(test_name, "- p-value"))

  # Group statistics (rank means and Ns)
  group_stats <- r_stats$group_stats[[1]]
  if (!is.null(group_stats) && !is.null(group_stats$group1)) {
    # Male (group 1)
    record_mw_comparison(test_name, "male_n", spss_ref$male_n,
                          group_stats$group1$n, tolerance_n)
    expect_equal(group_stats$group1$n, spss_ref$male_n, tolerance = tolerance_n,
                 label = paste(test_name, "- Male N"))

    record_mw_comparison(test_name, "male_mean_rank", spss_ref$male_mean_rank,
                          group_stats$group1$rank_mean, tolerance_rank)
    expect_equal(group_stats$group1$rank_mean, spss_ref$male_mean_rank,
                 tolerance = tolerance_rank,
                 label = paste(test_name, "- Male Mean Rank"))

    # Female (group 2)
    record_mw_comparison(test_name, "female_n", spss_ref$female_n,
                          group_stats$group2$n, tolerance_n)
    expect_equal(group_stats$group2$n, spss_ref$female_n, tolerance = tolerance_n,
                 label = paste(test_name, "- Female N"))

    record_mw_comparison(test_name, "female_mean_rank", spss_ref$female_mean_rank,
                          group_stats$group2$rank_mean, tolerance_rank)
    expect_equal(group_stats$group2$rank_mean, spss_ref$female_mean_rank,
                 tolerance = tolerance_rank,
                 label = paste(test_name, "- Female Mean Rank"))
  }
}

#' Extract results for a specific group from grouped Mann-Whitney analysis
extract_mw_group <- function(result, group_var, group_value) {
  if (is.data.frame(result$results)) {
    group_data <- result$results[result$results[[group_var]] == group_value, ]
    grouped_result <- result
    grouped_result$results <- group_data
    return(grouped_result)
  }
  return(result)
}

# ============================================================================
# TEST SETUP
# ============================================================================

data(survey_data, envir = environment())

# ============================================================================
# TEST 1: UNWEIGHTED / UNGROUPED (SPSS Reference)
# ============================================================================

test_that("Test 1a: Mann-Whitney U (unweighted, life_satisfaction by gender)", {
  result <- survey_data %>%
    mann_whitney(life_satisfaction, group = gender)

  compare_mw_with_spss(
    result,
    spss_values$test_1a_life_satisfaction,
    "Test 1a: Unweighted (life_satisfaction by gender)"
  )
})

test_that("Test 1b: Mann-Whitney U (unweighted, income by gender)", {
  result <- survey_data %>%
    mann_whitney(income, group = gender)

  compare_mw_with_spss(
    result,
    spss_values$test_1b_income,
    "Test 1b: Unweighted (income by gender)"
  )
})

test_that("Test 1c: Mann-Whitney U (unweighted, age by gender)", {
  result <- survey_data %>%
    mann_whitney(age, group = gender)

  compare_mw_with_spss(
    result,
    spss_values$test_1c_age,
    "Test 1c: Unweighted (age by gender)"
  )
})

# ============================================================================
# TEST 2: WEIGHTED / UNGROUPED (survey::svyranktest Exact Match)
# ============================================================================
# SPSS NPAR TESTS rounds fractional frequency weights to integers.
# Since survey_data sampling_weight ranges 0.7-1.4, all weights round to 1,
# making SPSS weighted results identical to unweighted.
#
# mann_whitney() implements the Lumley & Scott (2013) design-based rank test,
# which is mathematically identical to survey::svyranktest(). We validate
# with tight tolerance (0.00001).

test_that("Test 2: Weighted Mann-Whitney exact match vs survey::svyranktest", {
  skip_if_not_installed("survey")

  vars <- c("life_satisfaction", "income", "age")

  for (var in vars) {
    result <- survey_data %>%
      mann_whitney(!!rlang::sym(var), group = gender, weights = sampling_weight)

    valid <- survey_data[!is.na(survey_data[[var]]) &
                          !is.na(survey_data$gender) &
                          !is.na(survey_data$sampling_weight), ]
    des <- survey::svydesign(ids = ~1, weights = ~sampling_weight, data = valid)
    f <- as.formula(paste(var, "~ gender"))
    ref <- survey::svyranktest(f, design = des, test = "wilcoxon")

    r_stats <- result$results[1, ]
    ref_p <- as.numeric(ref$p.value)
    ref_t <- as.numeric(ref$statistic)

    # Record comparison with tight tolerance
    record_mw_comparison(paste("Test 2: Weighted ungrouped", var),
                          "p_value", ref_p, r_stats$p_value, 0.00001)
    record_mw_comparison(paste("Test 2: Weighted ungrouped", var),
                          "t_stat(Z)", ref_t, r_stats$Z, 0.00001)

    # Exact numerical match on p-value
    expect_equal(r_stats$p_value, ref_p, tolerance = 0.00001,
                 label = paste("Test 2", var, "- p-value matches svyranktest"))

    # Exact numerical match on t-statistic (stored as Z)
    expect_equal(r_stats$Z, ref_t, tolerance = 0.00001,
                 label = paste("Test 2", var, "- t-stat matches svyranktest"))

    # Effect size should be finite and in [0, 1]
    expect_true(is.finite(r_stats$effect_size_r),
                label = paste("Test 2", var, "- effect size is finite"))
    expect_true(r_stats$effect_size_r >= 0 && r_stats$effect_size_r <= 1,
                label = paste("Test 2", var, "- effect size in [0, 1]"))

    # Weighted N should be populated
    group_stats <- r_stats$group_stats[[1]]
    expect_true(!is.null(group_stats$group1$n),
                label = paste("Test 2", var, "- group1 n populated"))
    expect_true(!is.null(group_stats$group2$n),
                label = paste("Test 2", var, "- group2 n populated"))
  }
})

# ============================================================================
# TEST 3: UNWEIGHTED / GROUPED BY REGION (SPSS Reference)
# ============================================================================

test_that("Test 3a: Mann-Whitney U (unweighted, life_satisfaction by gender, grouped by region)", {
  result <- survey_data %>%
    group_by(region) %>%
    mann_whitney(life_satisfaction, group = gender)

  # East
  east_result <- extract_mw_group(result, "region", "East")
  compare_mw_with_spss(
    east_result,
    spss_values$test_3a_life_satisfaction_east,
    "Test 3a East: Unweighted/Grouped (life_satisfaction by gender)"
  )

  # West
  west_result <- extract_mw_group(result, "region", "West")
  compare_mw_with_spss(
    west_result,
    spss_values$test_3a_life_satisfaction_west,
    "Test 3a West: Unweighted/Grouped (life_satisfaction by gender)"
  )
})

test_that("Test 3b: Mann-Whitney U (unweighted, income by gender, grouped by region)", {
  result <- survey_data %>%
    group_by(region) %>%
    mann_whitney(income, group = gender)

  # East
  east_result <- extract_mw_group(result, "region", "East")
  compare_mw_with_spss(
    east_result,
    spss_values$test_3b_income_east,
    "Test 3b East: Unweighted/Grouped (income by gender)"
  )

  # West
  west_result <- extract_mw_group(result, "region", "West")
  compare_mw_with_spss(
    west_result,
    spss_values$test_3b_income_west,
    "Test 3b West: Unweighted/Grouped (income by gender)"
  )
})

test_that("Test 3c: Mann-Whitney U (unweighted, age by gender, grouped by region)", {
  result <- survey_data %>%
    group_by(region) %>%
    mann_whitney(age, group = gender)

  # East
  east_result <- extract_mw_group(result, "region", "East")
  compare_mw_with_spss(
    east_result,
    spss_values$test_3c_age_east,
    "Test 3c East: Unweighted/Grouped (age by gender)"
  )

  # West
  west_result <- extract_mw_group(result, "region", "West")
  compare_mw_with_spss(
    west_result,
    spss_values$test_3c_age_west,
    "Test 3c West: Unweighted/Grouped (age by gender)"
  )
})

# ============================================================================
# TEST 4: WEIGHTED / GROUPED BY REGION (survey::svyranktest Exact Match)
# ============================================================================
# Lumley & Scott implementation validated per region against svyranktest.

test_that("Test 4: Weighted/grouped Mann-Whitney exact match vs survey::svyranktest", {
  skip_if_not_installed("survey")

  vars <- c("life_satisfaction", "income", "age")

  for (var in vars) {
    result <- survey_data %>%
      group_by(region) %>%
      mann_whitney(!!rlang::sym(var), group = gender, weights = sampling_weight)

    for (reg in c("East", "West")) {
      reg_result <- extract_mw_group(result, "region", reg)
      r_stats <- reg_result$results[1, ]

      valid <- survey_data[survey_data$region == reg &
                            !is.na(survey_data[[var]]) &
                            !is.na(survey_data$gender) &
                            !is.na(survey_data$sampling_weight), ]
      des <- survey::svydesign(ids = ~1, weights = ~sampling_weight, data = valid)
      f <- as.formula(paste(var, "~ gender"))
      ref <- survey::svyranktest(f, design = des, test = "wilcoxon")

      ref_p <- as.numeric(ref$p.value)
      ref_t <- as.numeric(ref$statistic)

      record_mw_comparison(paste("Test 4:", reg, "Weighted", var),
                            "p_value", ref_p, r_stats$p_value, 0.00001)
      record_mw_comparison(paste("Test 4:", reg, "Weighted", var),
                            "t_stat(Z)", ref_t, r_stats$Z, 0.00001)

      expect_equal(r_stats$p_value, ref_p, tolerance = 0.00001,
                   label = paste("Test 4", var, reg,
                                 "- p-value matches svyranktest"))

      expect_equal(r_stats$Z, ref_t, tolerance = 0.00001,
                   label = paste("Test 4", var, reg,
                                 "- t-stat matches svyranktest"))
    }
  }
})

# ============================================================================
# EDGE CASES
# ============================================================================

test_that("Edge case: Missing values handled correctly", {
  test_data <- survey_data
  test_data$life_satisfaction[1:10] <- NA

  expect_no_error({
    result <- test_data %>% mann_whitney(life_satisfaction, group = gender)
  })

  # Verify sample size is reduced
  group_stats <- result$results$group_stats[[1]]
  total_n <- group_stats$group1$n + group_stats$group2$n
  expect_lt(total_n, nrow(survey_data))
})

test_that("Edge case: Multiple variables simultaneously", {
  expect_no_error({
    result <- survey_data %>%
      mann_whitney(life_satisfaction, income, age, group = gender)
  })

  expect_equal(nrow(result$results), 3)
  expect_true(all(c("life_satisfaction", "income", "age") %in%
                    result$results$Variable))
})

test_that("Edge case: Result object has correct structure", {
  result <- survey_data %>%
    mann_whitney(life_satisfaction, group = gender)

  expect_s3_class(result, "mann_whitney")
  expect_true("results" %in% names(result))
  expect_true(all(c("Variable", "U", "W", "Z", "p_value",
                     "effect_size_r", "rank_mean_diff", "group_stats") %in%
                    names(result$results)))
})

# ============================================================================
# SUMMARY REPORT
# ============================================================================

test_that("Generate validation summary", {
  if (length(mw_validation_results) > 0) {
    df_results <- do.call(rbind, lapply(mw_validation_results, as.data.frame,
                                         stringsAsFactors = FALSE))

    total_comparisons <- nrow(df_results)
    total_matches <- sum(df_results$match, na.rm = TRUE)
    match_rate <- (total_matches / total_comparisons) * 100

    cat("\n")
    cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
    cat("MANN-WHITNEY U TEST VALIDATION SUMMARY\n")
    cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
    cat(sprintf("Total comparisons: %d\n", total_comparisons))
    cat(sprintf("Matches: %d (%.1f%%)\n", total_matches, match_rate))
    cat("\n")

    # Results by test
    test_types <- unique(df_results$test)
    cat("Results by test:\n")
    for (test_type in test_types) {
      test_data <- df_results[df_results$test == test_type, ]
      test_matches <- sum(test_data$match, na.rm = TRUE)
      test_total <- nrow(test_data)
      cat(sprintf("  %s: %d/%d (%.1f%%)\n",
                  test_type, test_matches, test_total,
                  (test_matches / test_total) * 100))
    }

    # Show mismatches
    mismatches <- df_results[!df_results$match, ]
    if (nrow(mismatches) > 0) {
      cat("\nMismatches:\n")
      for (i in seq_len(nrow(mismatches))) {
        cat(sprintf("  - %s %s: expected=%.4f, actual=%.4f (diff=%.4f, tol=%.4f)\n",
                    mismatches$test[i], mismatches$metric[i],
                    mismatches$expected[i], mismatches$actual[i],
                    mismatches$difference[i], mismatches$tolerance[i]))
      }
    }

    cat("\n")
    cat("Test scenarios validated:\n")
    cat("1. Unweighted/Ungrouped: SPSS NPAR TESTS reference\n")
    cat("2. Weighted/Ungrouped: survey::svyranktest exact match (tol 0.00001)\n")
    cat("3. Unweighted/Grouped: SPSS NPAR TESTS with SPLIT FILE\n")
    cat("4. Weighted/Grouped: survey::svyranktest exact match (tol 0.00001)\n")
    cat("\n")
    cat("Weighted method: Lumley & Scott (2013) design-based rank test\n")
    cat("\n")
    cat("Validation criteria:\n")
    cat("- U statistic: tolerance +/- 1\n")
    cat("- W statistic: tolerance +/- 1\n")
    cat("- Z statistic: tolerance +/- 0.01\n")
    cat("- P-values (unweighted): tolerance +/- 0.005\n")
    cat("- P-values (weighted vs svyranktest): tolerance +/- 0.00001\n")
    cat("- t-statistic (weighted vs svyranktest): tolerance +/- 0.00001\n")
    cat("- Rank means: tolerance +/- 0.1\n")
    cat("- Sample sizes: exact match\n")
    cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
  }

  expect_true(TRUE)
})
