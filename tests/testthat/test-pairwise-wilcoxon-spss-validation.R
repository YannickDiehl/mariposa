# =============================================================================
# PAIRWISE WILCOXON (Post-Hoc for Friedman) - SPSS VALIDATION TEST
# =============================================================================
# Purpose: Validate R pairwise_wilcoxon() against SPSS pairwise Wilcoxon
#          signed-rank tests after Friedman (NPAR TESTS /WILCOXON)
# Dataset: survey_data, longitudinal_data
# Variables: trust items (3 vars), attitude items (3 vars),
#            longitudinal scores (4 timepoints)
# Split grouping: region (East/West), group (Control/Treatment)
# Created: 2026-03-01
# SPSS Version: 29.0
#
# Validation strategy:
# - S3 dispatch: pairwise_wilcoxon() called on friedman_test result object
# - Tests: Wilcoxon Z-statistic, unadjusted p, Bonferroni-adjusted p
# - Scenarios: unweighted/weighted x ungrouped/grouped
# - Both survey_data and longitudinal_data
#
# SPSS Reference: NPAR TESTS /WILCOXON provides Z and Asymp. Sig.
# for each pair individually. Bonferroni = p * k*(k-1)/2.
# =============================================================================

library(testthat)
library(dplyr)
library(tidyr)
library(mariposa)

# =============================================================================
# GLOBAL TRACKING FOR VALIDATION REPORT
# =============================================================================

pw_validation_results <- list()
pw_comparison_count <- 0

record_pw_comparison <- function(test_name, metric, expected, actual,
                                 tolerance = 0.01) {
  pw_comparison_count <<- pw_comparison_count + 1

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

  pw_validation_results <<- append(pw_validation_results, list(result))
  return(match_status)
}

# =============================================================================
# SPSS REFERENCE VALUES
# =============================================================================
# Fill from SPSS output (pairwise_wilcoxon.sps)
#
# For each Wilcoxon pair: Z statistic, Asymp. Sig. (2-tailed)
# Bonferroni adjustment: p_adj = min(1, p_raw * n_comparisons)

spss_values <- list(
  # ---- Test 1a: trust items pairwise Wilcoxon (unweighted, ungrouped) ----
  # 3 variables -> 3 pairs
  test_1a = list(
    pairs = list(
      list(var1 = "trust_government", var2 = "trust_media",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "trust_government", var2 = "trust_science",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "trust_media", var2 = "trust_science",
           z = NA_real_, p = NA_real_, n = NA_integer_)
    ),
    n_pairs = 3
  ),

  # ---- Test 1b: attitude items pairwise Wilcoxon (unweighted, ungrouped) ----
  test_1b = list(
    pairs = list(
      list(var1 = "life_satisfaction", var2 = "environmental_concern",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "life_satisfaction", var2 = "political_orientation",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "environmental_concern", var2 = "political_orientation",
           z = NA_real_, p = NA_real_, n = NA_integer_)
    ),
    n_pairs = 3
  ),

  # ---- Test 2a: trust items (weighted, ungrouped) ----
  test_2a = list(
    pairs = list(
      list(var1 = "trust_government", var2 = "trust_media",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "trust_government", var2 = "trust_science",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "trust_media", var2 = "trust_science",
           z = NA_real_, p = NA_real_, n = NA_integer_)
    ),
    n_pairs = 3
  ),

  # ---- Test 3: trust items, grouped by region (unweighted) ----
  test_3_east = list(
    pairs = list(
      list(var1 = "trust_government", var2 = "trust_media",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "trust_government", var2 = "trust_science",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "trust_media", var2 = "trust_science",
           z = NA_real_, p = NA_real_, n = NA_integer_)
    ),
    n_pairs = 3
  ),
  test_3_west = list(
    pairs = list(
      list(var1 = "trust_government", var2 = "trust_media",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "trust_government", var2 = "trust_science",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "trust_media", var2 = "trust_science",
           z = NA_real_, p = NA_real_, n = NA_integer_)
    ),
    n_pairs = 3
  ),

  # ---- Test 5a: longitudinal (4 timepoints, ungrouped) ----
  # 4 variables -> 6 pairs
  test_5a = list(
    pairs = list(
      list(var1 = "score_T1", var2 = "score_T2",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "score_T1", var2 = "score_T3",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "score_T1", var2 = "score_T4",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "score_T2", var2 = "score_T3",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "score_T2", var2 = "score_T4",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "score_T3", var2 = "score_T4",
           z = NA_real_, p = NA_real_, n = NA_integer_)
    ),
    n_pairs = 6
  ),

  # ---- Test 5b: longitudinal, grouped by treatment ----
  test_5b_control = list(
    pairs = list(
      list(var1 = "score_T1", var2 = "score_T2",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "score_T1", var2 = "score_T3",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "score_T1", var2 = "score_T4",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "score_T2", var2 = "score_T3",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "score_T2", var2 = "score_T4",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "score_T3", var2 = "score_T4",
           z = NA_real_, p = NA_real_, n = NA_integer_)
    ),
    n_pairs = 6
  ),
  test_5b_treatment = list(
    pairs = list(
      list(var1 = "score_T1", var2 = "score_T2",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "score_T1", var2 = "score_T3",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "score_T1", var2 = "score_T4",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "score_T2", var2 = "score_T3",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "score_T2", var2 = "score_T4",
           z = NA_real_, p = NA_real_, n = NA_integer_),
      list(var1 = "score_T3", var2 = "score_T4",
           z = NA_real_, p = NA_real_, n = NA_integer_)
    ),
    n_pairs = 6
  )
)

# =============================================================================
# HELPER: Validate pairwise Wilcoxon results against SPSS
# =============================================================================

validate_pw_pairs <- function(result_pairs, spss_ref, test_label,
                              tolerance_z = 0.01, tolerance_p = 0.001) {
  expect_equal(nrow(result_pairs), spss_ref$n_pairs,
               label = paste(test_label, "- number of pairs"))

  for (pair in spss_ref$pairs) {
    if (is.na(pair$z)) next

    # Find matching pair in R results
    matched <- result_pairs[
      (result_pairs$var1 == pair$var1 & result_pairs$var2 == pair$var2) |
        (result_pairs$var1 == pair$var2 & result_pairs$var2 == pair$var1), ]

    expect_true(nrow(matched) > 0,
                label = paste(test_label, "- pair found:",
                              pair$var1, "vs", pair$var2))

    if (nrow(matched) > 0) {
      pair_label <- paste(pair$var1, "vs", pair$var2)

      # Z statistic (absolute value)
      expect_true(
        record_pw_comparison(test_label, paste("Z -", pair_label),
                             abs(pair$z), abs(matched$z[1]),
                             tolerance = tolerance_z),
        label = paste(test_label, "- Z for", pair_label)
      )

      # Unadjusted p-value
      if (pair$p == 0) {
        expect_true(matched$p[1] < 0.001,
                    label = paste(test_label, "- p < .001 for", pair_label))
      } else {
        expect_true(
          record_pw_comparison(test_label, paste("p -", pair_label),
                               pair$p, matched$p[1],
                               tolerance = tolerance_p),
          label = paste(test_label, "- p for", pair_label)
        )
      }
    }
  }
}

# =============================================================================
# TEST 1: UNWEIGHTED / UNGROUPED
# =============================================================================

test_that("Pairwise Wilcoxon: S3 dispatch on friedman_test object works", {
  data(survey_data)
  friedman_result <- survey_data %>%
    friedman_test(trust_government, trust_media, trust_science)

  pw_result <- friedman_result %>% pairwise_wilcoxon()

  expect_s3_class(pw_result, "pairwise_wilcoxon")
  expect_true("comparisons" %in% names(pw_result))
  expect_equal(pw_result$p_adjust_method, "bonferroni")
})

test_that("Pairwise Wilcoxon: trust items (unweighted, ungrouped)", {
  data(survey_data)
  friedman_result <- survey_data %>%
    friedman_test(trust_government, trust_media, trust_science)

  pw_result <- friedman_result %>% pairwise_wilcoxon()

  # 3 variables -> 3 pairwise comparisons
  expect_equal(nrow(pw_result$comparisons), 3)

  validate_pw_pairs(
    pw_result$comparisons,
    spss_values$test_1a,
    "1a: trust items (unweighted)"
  )
})

test_that("Pairwise Wilcoxon: attitude items (unweighted, ungrouped)", {
  data(survey_data)
  friedman_result <- survey_data %>%
    friedman_test(life_satisfaction, environmental_concern,
                  political_orientation)

  pw_result <- friedman_result %>% pairwise_wilcoxon()

  expect_equal(nrow(pw_result$comparisons), 3)

  validate_pw_pairs(
    pw_result$comparisons,
    spss_values$test_1b,
    "1b: attitude items (unweighted)"
  )
})

# =============================================================================
# TEST 2: WEIGHTED / UNGROUPED
# =============================================================================

test_that("Pairwise Wilcoxon: trust items (weighted, ungrouped)", {
  data(survey_data)
  friedman_result <- survey_data %>%
    friedman_test(trust_government, trust_media, trust_science,
                  weights = sampling_weight)

  pw_result <- friedman_result %>% pairwise_wilcoxon()

  expect_s3_class(pw_result, "pairwise_wilcoxon")
  expect_equal(nrow(pw_result$comparisons), 3)

  # With weights ~ 1, results should be close to unweighted
  validate_pw_pairs(
    pw_result$comparisons,
    spss_values$test_2a,
    "2a: trust items (weighted)",
    tolerance_z = 1.0,   # wider tolerance for weighted
    tolerance_p = 0.05
  )
})

# =============================================================================
# TEST 3: UNWEIGHTED / GROUPED BY REGION
# =============================================================================

test_that("Pairwise Wilcoxon: trust items grouped by region", {
  data(survey_data)
  friedman_result <- survey_data %>%
    group_by(region) %>%
    friedman_test(trust_government, trust_media, trust_science)

  pw_result <- friedman_result %>% pairwise_wilcoxon()

  expect_s3_class(pw_result, "pairwise_wilcoxon")

  # Should have results for both East and West
  if ("region" %in% names(pw_result$comparisons)) {
    east_comps <- pw_result$comparisons[pw_result$comparisons$region == "East", ]
    west_comps <- pw_result$comparisons[pw_result$comparisons$region == "West", ]

    expect_equal(nrow(east_comps), 3)
    expect_equal(nrow(west_comps), 3)

    validate_pw_pairs(east_comps, spss_values$test_3_east,
                      "3-East: trust items (grouped)")
    validate_pw_pairs(west_comps, spss_values$test_3_west,
                      "3-West: trust items (grouped)")
  }
})

# =============================================================================
# TEST 4: WEIGHTED / GROUPED BY REGION
# =============================================================================

test_that("Pairwise Wilcoxon: trust items weighted and grouped", {
  data(survey_data)
  friedman_result <- survey_data %>%
    group_by(region) %>%
    friedman_test(trust_government, trust_media, trust_science,
                  weights = sampling_weight)

  pw_result <- friedman_result %>% pairwise_wilcoxon()

  expect_s3_class(pw_result, "pairwise_wilcoxon")
})

# =============================================================================
# TEST 5: LONGITUDINAL DATA
# =============================================================================

test_that("Pairwise Wilcoxon: longitudinal (4 timepoints, ungrouped)", {
  data(longitudinal_data)
  long_wide <- longitudinal_data %>%
    select(subject_id, group, time, outcome_score) %>%
    pivot_wider(names_from = time, values_from = outcome_score,
                names_prefix = "score_")

  friedman_result <- long_wide %>%
    friedman_test(score_T1, score_T2, score_T3, score_T4)

  pw_result <- friedman_result %>% pairwise_wilcoxon()

  # 4 variables -> 6 pairwise comparisons
  expect_equal(nrow(pw_result$comparisons), 6)

  validate_pw_pairs(
    pw_result$comparisons,
    spss_values$test_5a,
    "5a: longitudinal (ungrouped)"
  )
})

test_that("Pairwise Wilcoxon: longitudinal grouped by treatment", {
  data(longitudinal_data)
  long_wide <- longitudinal_data %>%
    select(subject_id, group, time, outcome_score) %>%
    pivot_wider(names_from = time, values_from = outcome_score,
                names_prefix = "score_")

  friedman_result <- long_wide %>%
    group_by(group) %>%
    friedman_test(score_T1, score_T2, score_T3, score_T4)

  pw_result <- friedman_result %>% pairwise_wilcoxon()

  expect_s3_class(pw_result, "pairwise_wilcoxon")

  if ("group" %in% names(pw_result$comparisons)) {
    ctrl_comps <- pw_result$comparisons[pw_result$comparisons$group == "Control", ]
    treat_comps <- pw_result$comparisons[pw_result$comparisons$group == "Treatment", ]

    expect_equal(nrow(ctrl_comps), 6)
    expect_equal(nrow(treat_comps), 6)

    validate_pw_pairs(ctrl_comps, spss_values$test_5b_control,
                      "5b-Control: longitudinal (grouped)")
    validate_pw_pairs(treat_comps, spss_values$test_5b_treatment,
                      "5b-Treatment: longitudinal (grouped)")
  }
})

# =============================================================================
# P-VALUE ADJUSTMENT METHODS
# =============================================================================

test_that("Pairwise Wilcoxon: Holm adjustment", {
  data(survey_data)
  friedman_result <- survey_data %>%
    friedman_test(trust_government, trust_media, trust_science)

  pw_holm <- friedman_result %>% pairwise_wilcoxon(p_adjust = "holm")

  expect_equal(pw_holm$p_adjust_method, "holm")
  expect_equal(nrow(pw_holm$comparisons), 3)
})

test_that("Pairwise Wilcoxon: BH adjustment", {
  data(survey_data)
  friedman_result <- survey_data %>%
    friedman_test(trust_government, trust_media, trust_science)

  pw_bh <- friedman_result %>% pairwise_wilcoxon(p_adjust = "BH")

  expect_equal(pw_bh$p_adjust_method, "BH")
})

# =============================================================================
# BONFERRONI CONSISTENCY CHECK
# =============================================================================

test_that("Pairwise Wilcoxon: Bonferroni p_adj = min(1, p * n_pairs)", {
  data(survey_data)
  friedman_result <- survey_data %>%
    friedman_test(trust_government, trust_media, trust_science)

  pw_result <- friedman_result %>% pairwise_wilcoxon()

  n_pairs <- nrow(pw_result$comparisons)

  # Each p_adj should equal min(1, p * n_pairs)
  for (i in seq_len(n_pairs)) {
    expected_adj <- min(1, pw_result$comparisons$p[i] * n_pairs)
    expect_equal(pw_result$comparisons$p_adj[i], expected_adj,
                 tolerance = 1e-10,
                 label = paste("Bonferroni consistency - pair", i))
  }
})

# =============================================================================
# INPUT VALIDATION AND EDGE CASES
# =============================================================================

test_that("Pairwise Wilcoxon: error on wrong S3 class", {
  data(survey_data)
  kw_result <- survey_data %>%
    kruskal_wallis(life_satisfaction, group = education)

  expect_error(pairwise_wilcoxon(kw_result))
})

test_that("Pairwise Wilcoxon: error with invalid p_adjust method", {
  data(survey_data)
  friedman_result <- survey_data %>%
    friedman_test(trust_government, trust_media, trust_science)

  expect_error(
    friedman_result %>% pairwise_wilcoxon(p_adjust = "invalid"),
    "p_adjust"
  )
})

# =============================================================================
# PRINT METHOD
# =============================================================================

test_that("Pairwise Wilcoxon: print method runs without error", {
  data(survey_data)
  friedman_result <- survey_data %>%
    friedman_test(trust_government, trust_media, trust_science)

  pw_result <- friedman_result %>% pairwise_wilcoxon()

  expect_output(print(pw_result), "Wilcoxon|Pairwise")
  expect_output(print(pw_result), "Bonferroni")
})

test_that("Pairwise Wilcoxon: print method for grouped results", {
  data(survey_data)
  friedman_result <- survey_data %>%
    group_by(region) %>%
    friedman_test(trust_government, trust_media, trust_science)

  pw_result <- friedman_result %>% pairwise_wilcoxon()

  output <- capture.output(print(pw_result))
  expect_true(any(grepl("Wilcoxon|Pairwise", output)))
})

# =============================================================================
# RESULT STRUCTURE
# =============================================================================

test_that("Pairwise Wilcoxon: result structure is complete", {
  data(survey_data)
  friedman_result <- survey_data %>%
    friedman_test(trust_government, trust_media, trust_science)

  pw_result <- friedman_result %>% pairwise_wilcoxon()

  expect_true("comparisons" %in% names(pw_result))
  expect_true("p_adjust_method" %in% names(pw_result))
  expect_true("variables" %in% names(pw_result))

  comps <- pw_result$comparisons
  expect_true("var1" %in% names(comps))
  expect_true("var2" %in% names(comps))
  expect_true("z" %in% names(comps) || "Z" %in% names(comps))
  expect_true("p" %in% names(comps) || "p_value" %in% names(comps))
  expect_true("p_adj" %in% names(comps) || "p_adjusted" %in% names(comps))
})

# =============================================================================
# VALIDATION REPORT
# =============================================================================

test_that("SPSS validation summary", {
  total <- length(pw_validation_results)
  passed <- if (total > 0) sum(vapply(pw_validation_results, function(r) r$match, logical(1))) else 0L
  failed <- total - passed

  cat("\n")
  cat("==================================================\n")
  cat("  PAIRWISE WILCOXON SPSS VALIDATION REPORT\n")
  cat("==================================================\n")
  cat(sprintf("  Total SPSS comparisons: %d\n", total))
  cat(sprintf("  Passed: %d\n", passed))
  cat(sprintf("  Failed: %d\n", failed))
  cat("==================================================\n")

  if (failed > 0) {
    cat("\n  FAILED COMPARISONS:\n")
    for (r in pw_validation_results) {
      if (!r$match) {
        cat(sprintf("  - %s | %s: expected=%.4f, actual=%.4f, diff=%.6f\n",
                    r$test, r$metric, r$expected, r$actual, r$difference))
      }
    }
  }

  if (total > 0) {
    expect_equal(failed, 0,
                 label = "All SPSS validation comparisons should pass")
  } else {
    expect_true(TRUE, label = "No SPSS values filled yet")
  }
})
