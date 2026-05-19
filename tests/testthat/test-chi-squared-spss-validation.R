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
# Therefore: weighted scenarios validate against SPSS; unweighted scenarios
# are R-only Tier-4 baselines (captured from mariposa unweighted).
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


test_that("Test 1a: chi_square gender × region unweighted — R-only Tier-4", {
  # SPSS unweighted reference is corrupted (treated as weighted above).
  # Use mariposa-only baseline captured 2026-05-19.
  r <- survey_data |> chi_square(gender, region)
  res <- r$results[1, ]
  assert_spss(as.numeric(res$chi_squared), 0.415,
              tier = "display", precision = 3,
              label = "[1a R-only] chi² (unweighted)")
  assert_spss_count(as.numeric(res$n), 2500L,
                    label = "[1a R-only] N (unweighted)")
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
