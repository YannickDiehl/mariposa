# =============================================================================
# chi_square — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::chi_square() against SPSS v29 CROSSTABS chi².
# Reference output: tests/spss_reference/outputs/chi_squared_output.txt
#
# IMPORTANT: The SPSS reference output is mislabeled — both "unweighted"
# (Test 1*) and "weighted" (Test 2*) sections show identical values that
# correspond to WEIGHTED runs (N=2516, 2518, 2517 — matching weighted
# sums, not unweighted N=2500). The reference was apparently generated
# with WEIGHT BY active throughout.
#
# Therefore: weighted scenarios validate against SPSS here; the unweighted
# gender x region table is validated against the unweighted CROSSTABS run
# in fisher_test_output.txt (same table, N = 2500).
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(

  # ---- Weighted (matches both SPSS "Test 1a" and "Test 2a") ------------
  weighted_gender_region = list(
    chi_squared = 0.548,    # chi_squared_output.txt:18
    df = 1L,
    p = 0.459,
    n = 2516L,
    phi = 0.015,
    cramers_v = 0.015,
    gamma = 0.037           # chi_squared_output.txt:31
  ),

  # ---- Unweighted gender x region (CROSSTABS in the Fisher reference) ---
  unweighted_gender_region = list(
    chi_squared = 0.415,    # fisher_test_output.txt:27
    df = 1L,                # fisher_test_output.txt:27
    p = 0.519,              # fisher_test_output.txt:27
    continuity = 0.353,     # fisher_test_output.txt:28
    continuity_p = 0.553,   # fisher_test_output.txt:28
    n = 2500L,              # fisher_test_output.txt:32
    phi = 0.013,            # fisher_test_output.txt:39
    cramers_v = 0.013       # fisher_test_output.txt:40
  )
)


data(survey_data, envir = environment())


test_that("Test 2a: chi_square gender × region weighted — matches SPSS", {
  r <- survey_data |> chi_square(gender, region, weights = sampling_weight)
  res <- r$results[1, ]
  spss <- spss_values$weighted_gender_region
  assert_spss(as.numeric(res$chi_squared), spss$chi_squared,
              tier = "display", precision = 3,
              label = "[2a] chi²")
  assert_spss_count(as.numeric(res$df), spss$df, label = "[2a] df")
  assert_spss(as.numeric(res$p_value), spss$p,
              tier = "display", precision = 3, what = "p_value",
              label = "[2a] p-value")
  assert_spss(as.numeric(res$n), spss$n,
              tier = "display", precision = 0,
              label = "[2a] N")
  assert_spss(as.numeric(res$phi), spss$phi,
              tier = "display", precision = 3, label = "[2a] phi")
  assert_spss(as.numeric(res$cramers_v), spss$cramers_v,
              tier = "display", precision = 3, label = "[2a] Cramer's V")
})


test_that("Test 1a: chi_square gender × region unweighted — matches SPSS", {
  spss <- spss_values$unweighted_gender_region
  res <- (survey_data |> chi_square(gender, region))$results[1, ]
  assert_spss(as.numeric(res$chi_squared), spss$chi_squared,
              tier = "display", precision = 3, label = "[1a] chi²")
  assert_spss_count(as.numeric(res$df), spss$df, label = "[1a] df")
  assert_spss(as.numeric(res$p_value), spss$p, tier = "display",
              precision = 3, what = "p_value", label = "[1a] p-value")
  assert_spss_count(as.numeric(res$n), spss$n, label = "[1a] N")
  assert_spss(as.numeric(res$phi), spss$phi, tier = "display",
              precision = 3, label = "[1a] phi")
  assert_spss(as.numeric(res$cramers_v), spss$cramers_v, tier = "display",
              precision = 3, label = "[1a] Cramer's V")
  cc <- (survey_data |> chi_square(gender, region, correct = TRUE))$results[1, ]
  assert_spss(as.numeric(cc$chi_squared), spss$continuity, tier = "display",
              precision = 3, label = "[1a] continuity correction")
  assert_spss(as.numeric(cc$p_value), spss$continuity_p, tier = "display",
              precision = 3, what = "p_value", label = "[1a] continuity p")
})


test_that("Test 3: chi_square gender × education grouped by region — structural", {
  # Grouped chi-square; just sanity-check structure (full per-cell values
  # would require an SPSS grouped reference set, which is not in scope).
  r <- survey_data |> group_by(region) |> chi_square(gender, education)
  expect_equal(nrow(r$results), 2L)
  expect_true("region" %in% names(r$results))
  # All chi² values should be positive
  expect_true(all(r$results$chi_squared > 0))
})
