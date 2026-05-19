# =============================================================================
# levene_test — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::levene_test() against SPSS v29 Levene's Test
# Reference syntax:  tests/spss_reference/syntax/levene_test.sps  (if present)
# Reference output:  tests/spss_reference/outputs/levene_test_output.txt
#
# Charter reference: .claude/VALIDATION_CHARTER.md
#
# SPSS source: the Levene rows in SPSS's T-TEST output. SPSS computes Levene's
# test of equality of variances as part of every independent-samples t-test;
# the output shows only F and Sig. per (variable, optional grouping cell).
# Mariposa exposes the test as a standalone function whose result$results
# data frame contains F_statistic, df1, df2, p_value, Variable, and (when
# grouped) a Group column with "var = value" strings.
#
# Tier assignments:
#   F-statistic — Display(3)
#   p-value     — Display(3), sentinel "<.001"
#   df1         — Spec (integer, k - 1)
#   df2         — Spec for unweighted (n - k); for weighted to be diagnosed
#                 (SPSS may use unrounded sum(w) - k like t_test, or
#                 floor(sum(w)) - k like ONEWAY; the convention is
#                 procedure-dependent, see Charter §5.1)
#
# Phase 1 source-bug audit (R/levene_test.R):
#   Line 594 (perform_grouped_levene_test):
#     df2 <- round(sum(w)) - length(g_levels)
#   Line 736 (perform_single_levene_test):
#     df2 <- round(sum(w)) - length(g_levels)
#   Both use round(sum(w)) — same pattern as the earlier t_test/oneway_anova
#   bugs. Fix to be decided empirically (unrounded vs floor) based on which
#   matches SPSS Levene F-statistics.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# =============================================================================
# SPSS REFERENCE VALUES
# =============================================================================

spss_values <- list(

  # ---- Scenario 1: Unweighted / Ungrouped --------------------------------
  test_1b_life_by_gender = list(
    f_stat = 1.277,    # levene_test_output.txt:16
    p      = 0.258     # levene_test_output.txt:16
  ),
  test_1c_income_by_gender = list(
    f_stat = 0.057,    # levene_test_output.txt:27
    p      = 0.811     # levene_test_output.txt:27
  ),
  test_1d_age_by_gender = list(
    f_stat = 0.534,    # levene_test_output.txt:38
    p      = 0.465     # levene_test_output.txt:38
  ),

  # ---- Scenario 2: Weighted / Ungrouped ----------------------------------
  test_2b_life_by_gender_weighted = list(
    f_stat = 1.461,    # levene_test_output.txt:55
    p      = 0.227     # levene_test_output.txt:55
  ),
  test_2c_income_by_gender_weighted = list(
    f_stat = 0.001,    # levene_test_output.txt:66
    p      = 0.975     # levene_test_output.txt:66
  ),
  test_2d_age_by_gender_weighted = list(
    f_stat = 0.123,    # levene_test_output.txt:77
    p      = 0.725     # levene_test_output.txt:77
  ),

  # ---- Scenario 3: Unweighted / Grouped by region ------------------------
  test_3a_life_by_gender_grouped = list(
    East = list(f_stat = 0.002, p = 0.961),   # levene_test_output.txt:90
    West = list(f_stat = 1.575, p = 0.210)    # levene_test_output.txt:91
  ),
  test_3b_income_by_gender_grouped = list(
    East = list(f_stat = 4.830, p = 0.029),   # levene_test_output.txt:102
    West = list(f_stat = 1.781, p = 0.182)    # levene_test_output.txt:103
  ),
  test_3c_age_by_gender_grouped = list(
    East = list(f_stat = 1.342, p = 0.247),   # levene_test_output.txt:114
    West = list(f_stat = 0.110, p = 0.740)    # levene_test_output.txt:115
  ),

  # ---- Scenario 4: Weighted / Grouped by region --------------------------
  test_4a_life_by_gender_weighted_grouped = list(
    East = list(f_stat = 0.004, p = 0.951),   # levene_test_output.txt:128
    West = list(f_stat = 1.604, p = 0.205)    # levene_test_output.txt:129
  ),
  test_4b_income_by_gender_weighted_grouped = list(
    East = list(f_stat = 6.136, p = 0.014),   # levene_test_output.txt:140
    West = list(f_stat = 1.718, p = 0.190)    # levene_test_output.txt:141
  ),
  test_4c_age_by_gender_weighted_grouped = list(
    East = list(f_stat = 1.028, p = 0.311),   # levene_test_output.txt:152
    West = list(f_stat = 0.000, p = 0.989)    # levene_test_output.txt:153
                                              # NB: F = .000 means F < 0.0005;
                                              # sentinel handling via Display(3)
                                              # would be 0.000 tolerance ±5e-4
  ),

  # ---- Test 6: Multiple variables at once (the trust additions) ----------
  # life_sat, income, age duplicate Tests 1b-d; here we add the trust rows
  test_6_multi_var = list(
    trust_government = list(f_stat = 3.217, p = 0.073),   # levene_test_output.txt:177
    trust_media      = list(f_stat = 0.005, p = 0.944),   # levene_test_output.txt:178
    trust_science    = list(f_stat = 3.241, p = 0.072)    # levene_test_output.txt:179
  )
)


# =============================================================================
# COMPARISON HELPER
# =============================================================================

#' Compare a single Levene row (F + p) against SPSS reference.
#'
#' @param r Row from result$results (data frame).
#' @param spss List with `f_stat`, `p`.
#' @param scenario Short label for failure messages.
compare_levene_row <- function(r, spss, scenario) {
  if (nrow(r) != 1L) {
    stop(sprintf("[%s] expected 1 row, got %d", scenario, nrow(r)),
         call. = FALSE)
  }
  assert_spss(as.numeric(r$F_statistic), spss$f_stat,
              tier = "display", precision = 3,
              label = sprintf("[%s] F-statistic", scenario))
  # what = "p_value" engages the Display-tier boundary-rounding guard
  # in the helper, which absorbs sub-tolerance internal-p differences
  # that cross a print-precision boundary.
  assert_spss(as.numeric(r$p_value), spss$p,
              tier = "display", precision = 3, what = "p_value",
              label = sprintf("[%s] p-value", scenario))
}

#' Pick the row for one region from a grouped Levene result.
#' Mariposa stores the grouping in a `Group` column with strings like
#' "region = East" (see R/levene_test.R print code paths).
extract_grouped_row <- function(result, region) {
  rows <- result$results
  sel <- if ("region" %in% names(rows)) {
    rows$region == region
  } else if ("Group" %in% names(rows)) {
    grepl(sprintf("region\\s*=\\s*%s", region), rows$Group)
  } else {
    stop(sprintf("Cannot find region column or Group column in result"),
         call. = FALSE)
  }
  rows[sel, , drop = FALSE]
}


# =============================================================================
# DATA SETUP
# =============================================================================

data(survey_data, envir = environment())


# =============================================================================
# SCENARIO 1 — UNWEIGHTED / UNGROUPED
# =============================================================================

test_that("Test 1b: Levene life_satisfaction by gender — matches SPSS", {
  r <- survey_data |>
    levene_test(life_satisfaction, group = gender, center = "mean")
  compare_levene_row(r$results, spss_values$test_1b_life_by_gender,
                     "1b: life_sat by gender")
})

test_that("Test 1c: Levene income by gender — matches SPSS", {
  r <- survey_data |>
    levene_test(income, group = gender, center = "mean")
  compare_levene_row(r$results, spss_values$test_1c_income_by_gender,
                     "1c: income by gender")
})

test_that("Test 1d: Levene age by gender — matches SPSS", {
  r <- survey_data |>
    levene_test(age, group = gender, center = "mean")
  compare_levene_row(r$results, spss_values$test_1d_age_by_gender,
                     "1d: age by gender")
})


# =============================================================================
# SCENARIO 2 — WEIGHTED / UNGROUPED
# =============================================================================

test_that("Test 2b: Levene life_satisfaction by gender, weighted — matches SPSS", {
  r <- survey_data |>
    levene_test(life_satisfaction, group = gender,
                weights = sampling_weight, center = "mean")
  compare_levene_row(r$results, spss_values$test_2b_life_by_gender_weighted,
                     "2b: weighted life_sat by gender")
})

test_that("Test 2c: Levene income by gender, weighted — matches SPSS", {
  r <- survey_data |>
    levene_test(income, group = gender,
                weights = sampling_weight, center = "mean")
  compare_levene_row(r$results, spss_values$test_2c_income_by_gender_weighted,
                     "2c: weighted income by gender")
})

test_that("Test 2d: Levene age by gender, weighted — matches SPSS", {
  r <- survey_data |>
    levene_test(age, group = gender,
                weights = sampling_weight, center = "mean")
  compare_levene_row(r$results, spss_values$test_2d_age_by_gender_weighted,
                     "2d: weighted age by gender")
})


# =============================================================================
# SCENARIO 3 — UNWEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 3a: Levene life_satisfaction by gender, grouped by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    levene_test(life_satisfaction, group = gender, center = "mean")
  for (rg in c("East", "West")) {
    row <- extract_grouped_row(r, rg)
    compare_levene_row(row, spss_values$test_3a_life_by_gender_grouped[[rg]],
                       sprintf("3a: life_sat by gender [%s]", rg))
  }
})

test_that("Test 3b: Levene income by gender, grouped by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    levene_test(income, group = gender, center = "mean")
  for (rg in c("East", "West")) {
    row <- extract_grouped_row(r, rg)
    compare_levene_row(row, spss_values$test_3b_income_by_gender_grouped[[rg]],
                       sprintf("3b: income by gender [%s]", rg))
  }
})

test_that("Test 3c: Levene age by gender, grouped by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    levene_test(age, group = gender, center = "mean")
  for (rg in c("East", "West")) {
    row <- extract_grouped_row(r, rg)
    compare_levene_row(row, spss_values$test_3c_age_by_gender_grouped[[rg]],
                       sprintf("3c: age by gender [%s]", rg))
  }
})


# =============================================================================
# SCENARIO 4 — WEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 4a: Levene life_satisfaction by gender, weighted, grouped — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    levene_test(life_satisfaction, group = gender,
                weights = sampling_weight, center = "mean")
  for (rg in c("East", "West")) {
    row <- extract_grouped_row(r, rg)
    compare_levene_row(row,
                       spss_values$test_4a_life_by_gender_weighted_grouped[[rg]],
                       sprintf("4a: weighted life_sat by gender [%s]", rg))
  }
})

test_that("Test 4b: Levene income by gender, weighted, grouped — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    levene_test(income, group = gender,
                weights = sampling_weight, center = "mean")
  for (rg in c("East", "West")) {
    row <- extract_grouped_row(r, rg)
    compare_levene_row(row,
                       spss_values$test_4b_income_by_gender_weighted_grouped[[rg]],
                       sprintf("4b: weighted income by gender [%s]", rg))
  }
})

test_that("Test 4c: Levene age by gender, weighted, grouped — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    levene_test(age, group = gender,
                weights = sampling_weight, center = "mean")
  for (rg in c("East", "West")) {
    row <- extract_grouped_row(r, rg)
    compare_levene_row(row,
                       spss_values$test_4c_age_by_gender_weighted_grouped[[rg]],
                       sprintf("4c: weighted age by gender [%s]", rg))
  }
})


# =============================================================================
# TEST 6 — MULTIPLE VARIABLES AT ONCE
# =============================================================================

test_that("Test 6: Levene multiple variables (trust additions) — matches SPSS", {
  r <- survey_data |>
    levene_test(life_satisfaction, income, age,
                trust_government, trust_media, trust_science,
                group = gender, center = "mean")

  expect_equal(nrow(r$results), 6L)
  expect_setequal(r$results$Variable,
                  c("life_satisfaction", "income", "age",
                    "trust_government", "trust_media", "trust_science"))

  # Validate the three trust additions (life_sat/income/age duplicate 1b-d)
  for (var_name in c("trust_government", "trust_media", "trust_science")) {
    row <- r$results[r$results$Variable == var_name, , drop = FALSE]
    compare_levene_row(row, spss_values$test_6_multi_var[[var_name]],
                       sprintf("6: %s by gender", var_name))
  }
})


# =============================================================================
# EDGE CASES
# =============================================================================

test_that("Edge case: < 2 groups produces warning (caught error path)", {
  d <- survey_data[survey_data$gender == "Male", , drop = FALSE]
  d$gender <- droplevels(factor(d$gender))
  # levene_test() wraps cli_abort inside an internal tryCatch and surfaces
  # the failure as a warning so multi-variable calls don't abort entirely.
  expect_warning(
    levene_test(d, life_satisfaction, group = gender, center = "mean"),
    regexp = "at least 2"
  )
})

test_that("Edge case: constant variable produces NaN F (warning, not error)", {
  d <- survey_data
  d$life_satisfaction <- 3  # constant
  expect_warning(
    r <- levene_test(d, life_satisfaction, group = gender, center = "mean"),
    regexp = "constant"
  )
  expect_true(is.nan(as.numeric(r$results$F_statistic[1])) ||
              is.na(as.numeric(r$results$F_statistic[1])))
})


# =============================================================================
# NOTE — VALIDATION GAPS
# =============================================================================
# SPSS does not display Levene's df1 / df2 in the T-TEST output (only F and
# Sig.). The current SPSS reference therefore cannot assert df1/df2 against
# external truth. mariposa nonetheless exposes them on result$results, and
# they can be sanity-checked internally:
#   - df1 = k - 1 (Spec, integer)
#   - df2 = (sum(w) or n) - k, depending on weighted/unweighted convention
# These checks are intentionally omitted here — the SPSS-compatibility claim
# rests on F and p only.
# =============================================================================
