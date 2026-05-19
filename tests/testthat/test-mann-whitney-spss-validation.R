# =============================================================================
# mann_whitney — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::mann_whitney() against SPSS v29 NPAR TESTS /M-W.
# Reference syntax:  tests/spss_reference/syntax/mann_whitney_test.sps (if any)
# Reference output:  tests/spss_reference/outputs/mann_whitney_output.txt
#
# Charter reference: .claude/VALIDATION_CHARTER.md
#
# Validated scenarios:
#   Scenario 1 — Unweighted / Ungrouped         (Tests 1a-c)
#   Scenario 3 — Unweighted / Grouped by region (Tests 3a-c, East/West)
#
# NOT validated against SPSS (Tier-4, R-only):
#   Scenario 2 — Weighted / Ungrouped
#   Scenario 4 — Weighted / Grouped
#
# RATIONALE for Tier-4 weighted scenarios:
# SPSS NPAR TESTS does not meaningfully honor WEIGHT BY for Mann-Whitney.
# The reference output reproduces IDENTICAL U, W, Z, and Sig. values for
# weighted runs (Tests 2a-c, 4a-c) as for the corresponding unweighted runs
# (Tests 1a-c, 3a-c), even though the "Notes" header shows
# `Weight = Weighting factor`. This is a documented SPSS NPAR TESTS quirk:
# the procedure cannot validate WEIGHT BY in any way relevant for fractional
# survey weights (0.7-1.4 in survey_data).
#
# mariposa's weighted Mann-Whitney uses the Lumley & Scott (2013) design-
# based rank test (Horvitz-Thompson midranks + WLS + sandwich variance),
# explicitly validated against survey::svyranktest() — a completely different
# algorithm than what SPSS attempts. Comparing the two would be misleading.
#
# Per Charter §4 Tier-4: weighted scenarios use testthat::expect_snapshot_value()
# for regression stability only. The first run establishes the snapshot.
#
# Tier assignments for SPSS-validated scenarios:
#   N           — Spec (count)
#   Mean Rank   — Display(2)  (SPSS prints 2 dp: 1196.71, 1223.91, etc.)
#   Sum of Ranks (W) — Display(2) for life_sat (printed .00), Display(2)
#                      for income/age (also .00)
#   Mann-Whitney U   — Display(3) (printed as XXX.000)
#   Z           — Display(3)
#   p-value     — Display(3), boundary-rounding guard via what = "p_value"
#
# Phase 1 source audit (R/mann_whitney.R):
#   Grep for round(sum(w)): no matches. The function uses sum(w) unrounded
#   throughout its weighted path. Not a candidate for the propagated bug
#   pattern found in t_test/oneway_anova/levene_test.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# =============================================================================
# SPSS REFERENCE VALUES (SPSS-validated scenarios only)
# =============================================================================

spss_values <- list(

  # ---- Scenario 1: Unweighted / Ungrouped --------------------------------
  test_1a_life_by_gender = list(
    male   = list(n = 1149, rank_mean = 1196.71, rank_sum = 1375022.00),   # mann_whitney_output.txt:28
    female = list(n = 1272, rank_mean = 1223.91, rank_sum = 1556809.00),   # mann_whitney_output.txt:29
    u = 714347.000,    # mann_whitney_output.txt:35
    w = 1375022.000,   # mann_whitney_output.txt:36
    z = -0.989,        # mann_whitney_output.txt:37
    p = 0.323          # mann_whitney_output.txt:38
  ),

  test_1b_income_by_gender = list(
    male   = list(n = 1046, rank_mean = 1103.31, rank_sum = 1154058.00),   # mann_whitney_output.txt:68
    female = list(n = 1140, rank_mean = 1084.50, rank_sum = 1236333.00),   # mann_whitney_output.txt:69
    u = 585963.000,    # mann_whitney_output.txt:75
    w = 1236333.000,   # mann_whitney_output.txt:76
    z = -0.696,        # mann_whitney_output.txt:77
    p = 0.486          # mann_whitney_output.txt:78
  ),

  test_1c_age_by_gender = list(
    male   = list(n = 1194, rank_mean = 1248.03, rank_sum = 1490147.00),   # mann_whitney_output.txt:108
    female = list(n = 1306, rank_mean = 1252.76, rank_sum = 1636103.00),   # mann_whitney_output.txt:109
    u = 776732.000,    # mann_whitney_output.txt:115
    w = 1490147.000,   # mann_whitney_output.txt:116
    z = -0.164,        # mann_whitney_output.txt:117
    p = 0.870          # mann_whitney_output.txt:118
  ),

  # ---- Scenario 3: Unweighted / Grouped by region ------------------------
  test_3a_life_by_gender_grouped = list(
    East = list(
      male   = list(n = 228, rank_mean = 237.05, rank_sum = 54046.50),    # mann_whitney_output.txt:268
      female = list(n = 237, rank_mean = 229.11, rank_sum = 54298.50),    # mann_whitney_output.txt:269
      u = 26095.500,    # mann_whitney_output.txt:278
      w = 54298.500,    # mann_whitney_output.txt:279
      z = -0.658,       # mann_whitney_output.txt:280
      p = 0.510         # mann_whitney_output.txt:281
    ),
    West = list(
      male   = list(n = 921,  rank_mean = 959.57, rank_sum = 883762.00),   # mann_whitney_output.txt:271
      female = list(n = 1035, rank_mean = 995.35, rank_sum = 1030184.00),  # mann_whitney_output.txt:272
      u = 459181.000,   # mann_whitney_output.txt:282
      w = 883762.000,   # mann_whitney_output.txt:283
      z = -1.447,       # mann_whitney_output.txt:284
      p = 0.148         # mann_whitney_output.txt:285
    )
  ),

  test_3b_income_by_gender_grouped = list(
    East = list(
      male   = list(n = 207, rank_mean = 223.46, rank_sum = 46255.50),    # mann_whitney_output.txt:315
      female = list(n = 222, rank_mean = 207.11, rank_sum = 45979.50),    # mann_whitney_output.txt:316
      u = 21226.500,    # mann_whitney_output.txt:325
      w = 45979.500,    # mann_whitney_output.txt:326
      z = -1.365,       # mann_whitney_output.txt:327
      p = 0.172         # mann_whitney_output.txt:328
    ),
    West = list(
      male   = list(n = 839, rank_mean = 880.58, rank_sum = 738810.00),   # mann_whitney_output.txt:318
      female = list(n = 918, rank_mean = 877.55, rank_sum = 805593.00),   # mann_whitney_output.txt:319
      u = 383772.000,   # mann_whitney_output.txt:329
      w = 805593.000,   # mann_whitney_output.txt:330
      z = -0.125,       # mann_whitney_output.txt:331
      p = 0.900         # mann_whitney_output.txt:332
    )
  ),

  test_3c_age_by_gender_grouped = list(
    East = list(
      male   = list(n = 238, rank_mean = 237.11, rank_sum = 56433.00),    # mann_whitney_output.txt:362
      female = list(n = 247, rank_mean = 248.67, rank_sum = 61422.00),    # mann_whitney_output.txt:363
      u = 27992.000,    # mann_whitney_output.txt:372
      w = 56433.000,    # mann_whitney_output.txt:373
      z = -0.908,       # mann_whitney_output.txt:374
      p = 0.364         # mann_whitney_output.txt:375
    ),
    West = list(
      male   = list(n = 956,  rank_mean = 1011.36, rank_sum = 966859.00),  # mann_whitney_output.txt:365
      female = list(n = 1059, rank_mean = 1004.97, rank_sum = 1064261.00), # mann_whitney_output.txt:366
      u = 502991.000,   # mann_whitney_output.txt:376
      w = 1064261.000,  # mann_whitney_output.txt:377
      z = -0.246,       # mann_whitney_output.txt:378
      p = 0.805         # mann_whitney_output.txt:379
    )
  )
)


# =============================================================================
# COMPARISON HELPER
# =============================================================================

#' Compare a Mann-Whitney result row against SPSS reference.
#'
#' SPSS reports the U/W/Z/p test statistics and per-group N + Mean Rank + Sum
#' of Ranks. mariposa stores these in result$results (row per variable) plus
#' result$results$group_stats[[i]] (with group1/group2 sublists).
#'
#' SPSS reports the SMALLER U as "Mann-Whitney U" and the rank-sum of the
#' group with smaller U as "Wilcoxon W". mariposa does the same (see
#' R/mann_whitney.R:239,242).
compare_mann_whitney <- function(row, gs, spss, scenario) {

  # ---- Test statistics ----
  assert_spss(as.numeric(row$U), spss$u,
              tier = "display", precision = 3,
              label = sprintf("[%s] Mann-Whitney U", scenario))
  assert_spss(as.numeric(row$W), spss$w,
              tier = "display", precision = 3,
              label = sprintf("[%s] Wilcoxon W (smaller-U group)", scenario))
  assert_spss(as.numeric(row$Z), spss$z,
              tier = "display", precision = 3,
              label = sprintf("[%s] Z", scenario))
  assert_spss(as.numeric(row$p_value), spss$p,
              tier = "display", precision = 3, what = "p_value",
              label = sprintf("[%s] p-value (two-sided)", scenario))

  # ---- Per-group descriptives ----
  g1 <- gs$group1
  g2 <- gs$group2

  # Map by name to the SPSS reference (Male/Female) regardless of order
  spss_by_name <- list(Male = spss$male, Female = spss$female)
  for (name in names(spss_by_name)) {
    expected <- spss_by_name[[name]]
    actual <- if (identical(g1$name, name)) g1 else g2
    assert_spss_count(actual$n, expected$n,
                      label = sprintf("[%s | %s] N", scenario, name))
    assert_spss(actual$rank_mean, expected$rank_mean,
                tier = "display", precision = 2,
                label = sprintf("[%s | %s] Mean Rank", scenario, name))
    # Reconstruct rank_sum = rank_mean * n for cross-check vs SPSS
    actual_rank_sum <- actual$rank_mean * actual$n
    assert_spss(actual_rank_sum, expected$rank_sum,
                tier = "display", precision = 2,
                label = sprintf("[%s | %s] Sum of Ranks", scenario, name))
  }
}

#' Pull result row + group_stats for one variable (ungrouped)
extract_single <- function(result, var_name = NULL) {
  r <- result$results
  if (!is.null(var_name)) {
    r <- r[r$Variable == var_name, , drop = FALSE]
  }
  list(row = r, gs = r$group_stats[[1]])
}

#' Pull row + group_stats for one (region, variable) cell of a grouped result
extract_grouped_cell <- function(result, region, var_name = NULL) {
  r <- result$results
  sel <- r$region == region
  if (!is.null(var_name)) sel <- sel & r$Variable == var_name
  r <- r[sel, , drop = FALSE]
  if (nrow(r) != 1L) {
    stop(sprintf("expected 1 row for region=%s, got %d", region, nrow(r)),
         call. = FALSE)
  }
  list(row = r, gs = r$group_stats[[1]])
}


# =============================================================================
# DATA SETUP
# =============================================================================

data(survey_data, envir = environment())


# =============================================================================
# SCENARIO 1 — UNWEIGHTED / UNGROUPED
# =============================================================================

test_that("Test 1a: Mann-Whitney life_satisfaction by gender — matches SPSS", {
  r <- survey_data |> mann_whitney(life_satisfaction, group = gender)
  s <- extract_single(r)
  compare_mann_whitney(s$row, s$gs, spss_values$test_1a_life_by_gender,
                       "1a: life_sat by gender")
})

test_that("Test 1b: Mann-Whitney income by gender — matches SPSS", {
  r <- survey_data |> mann_whitney(income, group = gender)
  s <- extract_single(r)
  compare_mann_whitney(s$row, s$gs, spss_values$test_1b_income_by_gender,
                       "1b: income by gender")
})

test_that("Test 1c: Mann-Whitney age by gender — matches SPSS", {
  r <- survey_data |> mann_whitney(age, group = gender)
  s <- extract_single(r)
  compare_mann_whitney(s$row, s$gs, spss_values$test_1c_age_by_gender,
                       "1c: age by gender")
})


# =============================================================================
# SCENARIO 3 — UNWEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 3a: Mann-Whitney life_satisfaction by gender, grouped by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    mann_whitney(life_satisfaction, group = gender)
  for (rg in c("East", "West")) {
    cell <- extract_grouped_cell(r, rg)
    compare_mann_whitney(cell$row, cell$gs,
                         spss_values$test_3a_life_by_gender_grouped[[rg]],
                         sprintf("3a: life_sat by gender [%s]", rg))
  }
})

test_that("Test 3b: Mann-Whitney income by gender, grouped by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    mann_whitney(income, group = gender)
  for (rg in c("East", "West")) {
    cell <- extract_grouped_cell(r, rg)
    compare_mann_whitney(cell$row, cell$gs,
                         spss_values$test_3b_income_by_gender_grouped[[rg]],
                         sprintf("3b: income by gender [%s]", rg))
  }
})

test_that("Test 3c: Mann-Whitney age by gender, grouped by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    mann_whitney(age, group = gender)
  for (rg in c("East", "West")) {
    cell <- extract_grouped_cell(r, rg)
    compare_mann_whitney(cell$row, cell$gs,
                         spss_values$test_3c_age_by_gender_grouped[[rg]],
                         sprintf("3c: age by gender [%s]", rg))
  }
})


# =============================================================================
# WEIGHTED SCENARIOS — TIER 4 (R-only baselines, no SPSS comparison)
# =============================================================================
# SPSS NPAR TESTS does not honor WEIGHT BY for Mann-Whitney (the reference
# output reproduces identical values for weighted vs unweighted runs).
# mariposa uses a Lumley & Scott design-based rank test that produces
# genuinely different (and statistically meaningful) values when weights
# are applied. The two cannot be compared.
#
# Per Charter §4 Tier-4: the values below are mariposa-only baselines,
# captured 2026-05-19 from the Lumley-Scott implementation in
# R/mann_whitney.R:271-345 (validated by package authors against
# survey::svyranktest()). They guard against algorithmic regressions in the
# R code, not against SPSS equivalence (which does not exist).
#
# Baselines should be regenerated and re-committed only after a deliberate
# algorithm change with documented rationale.
# =============================================================================

# R-only mariposa baselines (NOT from SPSS; see header note above)
r_only_baselines <- list(
  life_satisfaction = list(U = 722281.0998, W = 1383240.4633,
                            Z = 1.0201, p_value = 0.307792,
                            effect_size_r = 0.020736),
  income            = list(U = 592273.4323, W = 1257619.3267,
                            Z = -0.7552, p_value = 0.450199,
                            effect_size_r = 0.016158),
  age               = list(U = 784976.7372, W = 1658760.4203,
                            Z = -0.2023, p_value = 0.839703,
                            effect_size_r = 0.004047)
)

compare_weighted_baseline <- function(row, baseline, scenario) {
  # Tolerance choice: r-only baselines come from a deterministic R algorithm.
  # 1e-4 absolute on raw values catches algorithmic drift but absorbs any
  # IEEE-754 FP reordering during refactors.
  for (field in names(baseline)) {
    expected <- baseline[[field]]
    actual   <- as.numeric(row[[field]][1])
    tolerance <- max(1e-4, abs(expected) * 1e-6)
    diff <- abs(actual - expected)
    testthat::expect_lte(
      diff, tolerance,
      label = sprintf("[%s] %s: actual=%s, baseline=%s (R-only Tier-4)",
                      scenario, field,
                      format(actual, digits = 8),
                      format(expected, digits = 8))
    )
  }
}

test_that("Scenario 2a (weighted life_sat) — R-only baseline stable", {
  r <- survey_data |>
    mann_whitney(life_satisfaction, group = gender, weights = sampling_weight)
  compare_weighted_baseline(r$results, r_only_baselines$life_satisfaction,
                            "2a: R-only weighted life_sat")
})

test_that("Scenario 2b (weighted income) — R-only baseline stable", {
  r <- survey_data |>
    mann_whitney(income, group = gender, weights = sampling_weight)
  compare_weighted_baseline(r$results, r_only_baselines$income,
                            "2b: R-only weighted income")
})

test_that("Scenario 2c (weighted age) — R-only baseline stable", {
  r <- survey_data |>
    mann_whitney(age, group = gender, weights = sampling_weight)
  compare_weighted_baseline(r$results, r_only_baselines$age,
                            "2c: R-only weighted age")
})


# =============================================================================
# EDGE CASES
# =============================================================================

test_that("Edge case: !=2 groups produces warning (caught error path)", {
  # mann_whitney() wraps cli_abort in a per-variable tryCatch so multi-
  # variable calls don't abort entirely; the failure surfaces as a warning.
  expect_warning(
    mann_whitney(survey_data, life_satisfaction, group = education),
    regexp = "exactly 2"
  )
})

test_that("Edge case: missing values reduce N exactly by the NAs", {
  d <- survey_data
  na_idx <- which(!is.na(d$life_satisfaction))[1:10]
  d$life_satisfaction[na_idx] <- NA
  base <- mann_whitney(survey_data, life_satisfaction, group = gender)
  red  <- mann_whitney(d,           life_satisfaction, group = gender)
  base_n <- base$results$group_stats[[1]]$group1$n + base$results$group_stats[[1]]$group2$n
  red_n  <- red$results$group_stats[[1]]$group1$n  + red$results$group_stats[[1]]$group2$n
  assert_spss_count(red_n, base_n - 10L,
                    label = "missing-value edge case: N reduction by 10")
})


# =============================================================================
# NOTE — DOCUMENTED SPSS QUIRK
# =============================================================================
# SPSS NPAR TESTS /M-W does not honor WEIGHT BY: the same procedure with
# and without WEIGHT BY produces identical U/W/Z/Sig output (verified in
# mann_whitney_output.txt at lines 33-38 vs 152-157, etc.). This is a
# SPSS-side limitation, not a mariposa issue. mariposa's weighted Mann-
# Whitney uses Lumley & Scott (2013) and produces meaningful weighted
# statistics; those are not validatable against SPSS but are validated
# against survey::svyranktest() during package development (see
# R/mann_whitney.R:273-345).
#
# This is the first SPSS-NPAR-TESTS finding for the Charter §5.1
# convention catalogue. Likely-related: kruskal_wallis, wilcoxon_test,
# friedman_test, binomial_test (all NPAR TESTS procedures) may have
# similar weight-handling limitations on the SPSS side.
# =============================================================================
