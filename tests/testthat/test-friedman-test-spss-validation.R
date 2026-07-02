# =============================================================================
# friedman_test — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::friedman_test() against SPSS v29 NPAR /FRIEDMAN.
# Reference output: tests/spss_reference/outputs/friedman_test_output.txt
#
# Charter reference: .claude/VALIDATION_CHARTER.md
#
# Fourth NPAR-TESTS data point confirming WEIGHT-BY ignored quirk:
# Weighted runs (Tests 2a-b, 4) produce IDENTICAL chi-sq and Sig. as
# unweighted (Tests 1a-b, 3). Pattern confirmed across 4 NPAR procedures.
#
# Source audit (R/friedman_test.R): no round(sum(w)) bug; uses sum(w)
# unrounded throughout the weighted path (line 186 w_total <- sum(w)).
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# =============================================================================
# SPSS REFERENCE VALUES
# =============================================================================

spss_values <- list(

  # ---- Test 1a: trust_government, trust_media, trust_science ----------
  test_1a_trust = list(
    mean_ranks = c(trust_government = 1.81,        # friedman_test_output.txt:28
                   trust_media       = 1.68,        # friedman_test_output.txt:29
                   trust_science     = 2.51),       # friedman_test_output.txt:30
    n  = 2135L,        # friedman_test_output.txt:34
    chi_sq = 1009.035, # friedman_test_output.txt:35
    df = 2L,           # friedman_test_output.txt:36
    p  = "<.001"       # friedman_test_output.txt:37
  ),

  # ---- Test 1b: life_satisfaction, environmental_concern, political_orientation
  test_1b_lifestyle = list(
    mean_ranks = c(life_satisfaction     = 2.21,    # friedman_test_output.txt:67
                   environmental_concern = 2.17,    # friedman_test_output.txt:68
                   political_orientation = 1.62),   # friedman_test_output.txt:69
    n  = 2139L,        # friedman_test_output.txt:73
    chi_sq = 553.909,  # friedman_test_output.txt:74
    df = 2L,           # friedman_test_output.txt:75
    p  = "<.001"       # friedman_test_output.txt:76
  ),

  # ---- Test 3a: trust variables, grouped by region --------------------
  test_3a_trust_grouped = list(
    East = list(
      mean_ranks = c(trust_government = 1.80,      # friedman_test_output.txt:184
                     trust_media       = 1.67,      # friedman_test_output.txt:185
                     trust_science     = 2.53),     # friedman_test_output.txt:186
      n  = 422L,        # friedman_test_output.txt:193
      chi_sq = 217.100, # friedman_test_output.txt:194
      df = 2L,          # friedman_test_output.txt:195
      p  = "<.001"      # friedman_test_output.txt:196
    ),
    West = list(
      mean_ranks = c(trust_government = 1.81,      # friedman_test_output.txt:187
                     trust_media       = 1.68,      # friedman_test_output.txt:188
                     trust_science     = 2.50),     # friedman_test_output.txt:189
      n  = 1713L,       # friedman_test_output.txt:197
      chi_sq = 792.344, # friedman_test_output.txt:198
      df = 2L,          # friedman_test_output.txt:199
      p  = "<.001"      # friedman_test_output.txt:200
    )
  ),

  # ---- Test 5: longitudinal_data_wide score_T1..T4 --------------------
  test_5_longitudinal = list(
    mean_ranks = c(score_T1 = 1.65,   # friedman_test_output.txt:277
                   score_T2 = 2.31,   # friedman_test_output.txt:278
                   score_T3 = 2.84,   # friedman_test_output.txt:279
                   score_T4 = 3.20),  # friedman_test_output.txt:280
    n  = 75L,         # friedman_test_output.txt:284
    chi_sq = 61.192,  # friedman_test_output.txt:285
    df = 3L,          # friedman_test_output.txt:286
    p  = "<.001"      # friedman_test_output.txt:287
  ),

  # ---- Test 5b: longitudinal grouped by treatment group --------------
  test_5b_longitudinal_grouped = list(
    Control = list(
      mean_ranks = c(score_T1 = 2.08, score_T2 = 2.51,         # friedman_test_output.txt:317-318
                     score_T3 = 2.62, score_T4 = 2.79),         # friedman_test_output.txt:319-320
      n  = 39L,          # friedman_test_output.txt:328
      chi_sq = 6.538,    # friedman_test_output.txt:329
      df = 3L,           # friedman_test_output.txt:330
      p  = 0.088         # friedman_test_output.txt:331
    ),
    Treatment = list(
      mean_ranks = c(score_T1 = 1.19, score_T2 = 2.08,         # friedman_test_output.txt:321-322
                     score_T3 = 3.08, score_T4 = 3.64),         # friedman_test_output.txt:323-324
      n  = 36L,          # friedman_test_output.txt:332
      chi_sq = 75.933,   # friedman_test_output.txt:333
      df = 3L,           # friedman_test_output.txt:334
      p  = "<.001"       # friedman_test_output.txt:335
    )
  )
)


# =============================================================================
# COMPARISON HELPER
# =============================================================================

compare_friedman <- function(row, spss, scenario) {
  assert_spss(as.numeric(row$chi_squared), spss$chi_sq,
              tier = "display", precision = 3,
              label = sprintf("[%s] Chi-Square", scenario))
  assert_spss_count(as.numeric(row$df), spss$df,
                    label = sprintf("[%s] df", scenario))
  assert_spss(as.numeric(row$p_value), spss$p,
              tier = "display", precision = 3, what = "p_value",
              label = sprintf("[%s] p-value", scenario))
  assert_spss_count(as.numeric(row$n), spss$n,
                    label = sprintf("[%s] N", scenario))

  # Per-variable mean ranks
  mr <- row$mean_ranks[[1]]
  for (var in names(spss$mean_ranks)) {
    expected <- spss$mean_ranks[[var]]
    actual   <- as.numeric(mr[[var]])
    assert_spss(actual, expected,
                tier = "display", precision = 2,
                label = sprintf("[%s] Mean Rank: %s", scenario, var))
  }
}

extract_grouped_row <- function(result, group_value) {
  r <- result$results
  group_cols <- setdiff(names(r), c("chi_squared", "df", "p_value",
                                     "kendall_w", "n", "k", "mean_ranks"))
  if (length(group_cols) == 0L) stop("not a grouped result")
  # Just take the first group column
  gc <- group_cols[1]
  sel <- as.character(r[[gc]]) == as.character(group_value)
  r <- r[sel, , drop = FALSE]
  if (nrow(r) != 1L) {
    stop(sprintf("expected 1 row for %s=%s, got %d", gc, group_value, nrow(r)),
         call. = FALSE)
  }
  r
}


# =============================================================================
# DATA SETUP
# =============================================================================

data(survey_data, envir = environment())
data(longitudinal_data_wide, envir = environment())


# =============================================================================
# SCENARIO 1 — UNWEIGHTED / UNGROUPED
# =============================================================================

test_that("Test 1a: Friedman trust variables — matches SPSS", {
  r <- survey_data |>
    friedman_test(trust_government, trust_media, trust_science)
  compare_friedman(r$results, spss_values$test_1a_trust,
                   "1a: trust_gov/media/science")
})

test_that("Test 1b: Friedman life_sat/env_concern/political — matches SPSS", {
  r <- survey_data |>
    friedman_test(life_satisfaction, environmental_concern, political_orientation)
  compare_friedman(r$results, spss_values$test_1b_lifestyle,
                   "1b: life_sat/env/political")
})


# =============================================================================
# SCENARIO 3 — UNWEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 3a: Friedman trust variables, grouped by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    friedman_test(trust_government, trust_media, trust_science)
  for (rg in c("East", "West")) {
    row <- extract_grouped_row(r, rg)
    compare_friedman(row, spss_values$test_3a_trust_grouped[[rg]],
                     sprintf("3a: trust [%s]", rg))
  }
})


# =============================================================================
# LONGITUDINAL SCENARIOS
# =============================================================================

test_that("Test 5: Friedman longitudinal scores T1-T4 — matches SPSS", {
  r <- longitudinal_data_wide |>
    friedman_test(score_T1, score_T2, score_T3, score_T4)
  compare_friedman(r$results, spss_values$test_5_longitudinal,
                   "5: longitudinal scores")
})

test_that("Test 5b: Friedman longitudinal, grouped by treatment group — matches SPSS", {
  r <- longitudinal_data_wide |>
    group_by(group) |>
    friedman_test(score_T1, score_T2, score_T3, score_T4)
  for (grp in c("Control", "Treatment")) {
    row <- extract_grouped_row(r, grp)
    compare_friedman(row, spss_values$test_5b_longitudinal_grouped[[grp]],
                     sprintf("5b: longitudinal [%s]", grp))
  }
})


# =============================================================================
# WEIGHTED SCENARIOS — TIER 4 (R-only baselines)
# =============================================================================
# SPSS NPAR TESTS /FRIEDMAN does not honor WEIGHT BY. mariposa's weighted
# Friedman is a frequency-weighted approximation: substitutes sum(w) for n
# in the standard chi-squared formula, with tie correction applied
# consistently with stats::friedman.test (the unweighted reference).
# NOT a design-based estimator. Baselines captured 2026-05-19 from
# R/friedman_test.R.
# =============================================================================

r_only_baselines <- list(
  trust     = list(chi_squared = 1012.0840, df = 2L, p_value = 0.0000000, n = 2150L),
  lifestyle = list(chi_squared =  551.7114, df = 2L, p_value = 0.0000000, n = 2152L)
)

compare_weighted_baseline <- function(row, baseline, scenario) {
  for (field in names(baseline)) {
    expected <- baseline[[field]]
    actual   <- as.numeric(row[[field]][1])
    if (field %in% c("df", "n")) {
      assert_spss_count(actual, as.integer(expected),
                        label = sprintf("[%s] %s (R-only Tier-4)", scenario, field))
    } else {
      tolerance <- max(1e-4, abs(expected) * 1e-6)
      testthat::expect_lte(
        abs(actual - expected), tolerance,
        label = sprintf("[%s] %s: actual=%s, baseline=%s (R-only Tier-4)",
                        scenario, field, format(actual, digits = 8),
                        format(expected, digits = 8))
      )
    }
  }
}

test_that("Scenario 2a: weighted trust — R-only baseline", {
  r <- survey_data |>
    friedman_test(trust_government, trust_media, trust_science,
                  weights = sampling_weight)
  compare_weighted_baseline(r$results, r_only_baselines$trust,
                            "2a: weighted trust")
})

test_that("Scenario 2b: weighted lifestyle — R-only baseline", {
  r <- survey_data |>
    friedman_test(life_satisfaction, environmental_concern, political_orientation,
                  weights = sampling_weight)
  compare_weighted_baseline(r$results, r_only_baselines$lifestyle,
                            "2b: weighted lifestyle")
})


# =============================================================================
# EDGE CASES
# =============================================================================

test_that("Edge case: fewer than 2 variables triggers error", {
  expect_error(
    friedman_test(survey_data, trust_government),
    regexp = "at least"
  )
})
