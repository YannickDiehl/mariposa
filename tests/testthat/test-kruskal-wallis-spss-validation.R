# =============================================================================
# kruskal_wallis — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::kruskal_wallis() against SPSS v29 NPAR TESTS /K-W.
# Reference output: tests/spss_reference/outputs/kruskal_wallis_output.txt
#
# Charter reference: .claude/VALIDATION_CHARTER.md
#
# Validated scenarios (SPSS-comparable):
#   Scenario 1 — Unweighted / Ungrouped         (Tests 1a-c)
#   Scenario 3 — Unweighted / Grouped by region (Tests 3a-b, East/West)
#
# NOT validated against SPSS (Tier-4, R-only):
#   Scenario 2 — Weighted / Ungrouped
#   Scenario 4 — Weighted / Grouped
#
# RATIONALE for Tier-4 weighted scenarios:
# This is the SECOND data point confirming the NPAR-TESTS WEIGHT BY quirk
# first documented during the mann_whitney migration (2026-05-19). SPSS
# NPAR TESTS /K-W produces IDENTICAL H, df, and Sig. values for weighted
# runs (Tests 2a-c, 4a-b) as for the corresponding unweighted runs (Tests
# 1a-c, 3a-b). Verified across all 10 weighted+unweighted comparisons:
#
#   Test 1a unweighted life_sat by education: H = 171.178 (line 37)
#   Test 2a   weighted life_sat by education: H = 171.178 (line 164)  IDENTICAL
#
#   Test 3a East unweighted: H = 17.105 (line 296)
#   Test 4a East   weighted: H = 17.105 (line 398)  IDENTICAL
#
# This generalises the SPSS NPAR TESTS finding from Charter §5.1: the
# WHOLE family (mann_whitney + kruskal_wallis confirmed; wilcoxon_test +
# friedman_test + binomial_test predicted) does not honor WEIGHT BY in
# any way that affects rank-test statistics.
#
# mariposa's weighted kruskal_wallis uses sum(w)-weighted Horvitz-Thompson
# midranks (R/kruskal_wallis.R, no round(sum(w)) bug; verified via grep).
# Tier-4 inline baselines below capture R-only stability without claiming
# SPSS equivalence.
#
# Tier assignments for SPSS-validated scenarios:
#   N           — Spec (count)
#   Mean Rank   — Display(2)
#   H statistic — Display(3)
#   df          — Spec (count)
#   p-value     — Display(3), boundary-rounding guard
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# =============================================================================
# SPSS REFERENCE VALUES
# =============================================================================

spss_values <- list(

  # ---- Test 1a: life_satisfaction by education --------------------------
  test_1a_life_by_education = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 809, rank_mean = 974.29),    # kruskal_wallis_output.txt:28
      "Intermediate Secondary" = list(n = 618, rank_mean = 1250.73),   # kruskal_wallis_output.txt:29
      "Academic Secondary"     = list(n = 607, rank_mean = 1329.56),   # kruskal_wallis_output.txt:30
      "University"             = list(n = 387, rank_mean = 1456.42)    # kruskal_wallis_output.txt:31
    ),
    total_n = 2421,         # kruskal_wallis_output.txt:32
    h_stat  = 171.178,      # kruskal_wallis_output.txt:37
    df      = 3,            # kruskal_wallis_output.txt:38
    p       = "<.001"       # kruskal_wallis_output.txt:39 (SPSS ".000")
  ),

  # ---- Test 1b: income by employment ------------------------------------
  test_1b_income_by_employment = list(
    descriptives = list(
      "Student"    = list(n = 65,   rank_mean = 1495.58),   # kruskal_wallis_output.txt:70
      "Employed"   = list(n = 1390, rank_mean = 1075.60),   # kruskal_wallis_output.txt:71
      "Unemployed" = list(n = 159,  rank_mean = 1073.29),   # kruskal_wallis_output.txt:72
      "Retired"    = list(n = 471,  rank_mean = 1089.83),   # kruskal_wallis_output.txt:73
      "Other"      = list(n = 101,  rank_mean = 1129.95)    # kruskal_wallis_output.txt:74
    ),
    total_n = 2186,         # kruskal_wallis_output.txt:75
    h_stat  = 28.026,       # kruskal_wallis_output.txt:80
    df      = 4,            # kruskal_wallis_output.txt:81
    p       = "<.001"       # kruskal_wallis_output.txt:82
  ),

  # ---- Test 1c: trust_government by education ---------------------------
  test_1c_trust_gov_by_education = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 791, rank_mean = 1191.27),   # kruskal_wallis_output.txt:113
      "Intermediate Secondary" = list(n = 592, rank_mean = 1156.01),   # kruskal_wallis_output.txt:114
      "Academic Secondary"     = list(n = 595, rank_mean = 1170.90),   # kruskal_wallis_output.txt:115
      "University"             = list(n = 376, rank_mean = 1192.82)    # kruskal_wallis_output.txt:116
    ),
    total_n = 2354,         # kruskal_wallis_output.txt:117
    h_stat  = 1.235,        # kruskal_wallis_output.txt:122
    df      = 3,            # kruskal_wallis_output.txt:123
    p       = 0.745         # kruskal_wallis_output.txt:124
  ),

  # ---- Test 3a: life_satisfaction by education, grouped by region -------
  test_3a_life_by_education_grouped = list(
    East = list(
      descriptives = list(
        "Basic Secondary"        = list(n = 161, rank_mean = 202.35),   # kruskal_wallis_output.txt:282
        "Intermediate Secondary" = list(n = 119, rank_mean = 232.99),   # kruskal_wallis_output.txt:283
        "Academic Secondary"     = list(n = 110, rank_mean = 254.85),   # kruskal_wallis_output.txt:284
        "University"             = list(n = 75,  rank_mean = 266.77)    # kruskal_wallis_output.txt:285
      ),
      total_n = 465,        # kruskal_wallis_output.txt:286
      h_stat  = 17.105,     # kruskal_wallis_output.txt:296
      df      = 3,          # kruskal_wallis_output.txt:297
      p       = 0.001       # kruskal_wallis_output.txt:298
    ),
    West = list(
      descriptives = list(
        "Basic Secondary"        = list(n = 648, rank_mean = 771.38),    # kruskal_wallis_output.txt:287
        "Intermediate Secondary" = list(n = 499, rank_mean = 1018.44),   # kruskal_wallis_output.txt:288
        "Academic Secondary"     = list(n = 497, rank_mean = 1075.24),   # kruskal_wallis_output.txt:289
        "University"             = list(n = 312, rank_mean = 1190.70)    # kruskal_wallis_output.txt:290
      ),
      total_n = 1956,       # kruskal_wallis_output.txt:291
      h_stat  = 158.807,    # kruskal_wallis_output.txt:299
      df      = 3,          # kruskal_wallis_output.txt:300
      p       = "<.001"     # kruskal_wallis_output.txt:301
    )
  ),

  # ---- Test 3b: income by employment, grouped by region -----------------
  test_3b_income_by_employment_grouped = list(
    East = list(
      descriptives = list(
        "Student"    = list(n = 8,   rank_mean = 294.19),   # kruskal_wallis_output.txt:332
        "Employed"   = list(n = 273, rank_mean = 213.94),   # kruskal_wallis_output.txt:333
        "Unemployed" = list(n = 28,  rank_mean = 170.63),   # kruskal_wallis_output.txt:334
        "Retired"    = list(n = 100, rank_mean = 230.44),   # kruskal_wallis_output.txt:335
        "Other"      = list(n = 20,  rank_mean = 182.68)    # kruskal_wallis_output.txt:336
      ),
      total_n = 429,        # kruskal_wallis_output.txt:337
      h_stat  = 9.787,      # kruskal_wallis_output.txt:348
      df      = 4,          # kruskal_wallis_output.txt:349
      p       = 0.044       # kruskal_wallis_output.txt:350
    ),
    West = list(
      descriptives = list(
        "Student"    = list(n = 57,   rank_mean = 1201.74),  # kruskal_wallis_output.txt:338
        "Employed"   = list(n = 1117, rank_mean = 862.13),   # kruskal_wallis_output.txt:339
        "Unemployed" = list(n = 131,  rank_mean = 898.09),   # kruskal_wallis_output.txt:340
        "Retired"    = list(n = 371,  rank_mean = 858.55),   # kruskal_wallis_output.txt:341
        "Other"      = list(n = 81,   rank_mean = 947.28)    # kruskal_wallis_output.txt:342
      ),
      total_n = 1757,       # kruskal_wallis_output.txt:343
      h_stat  = 26.570,     # kruskal_wallis_output.txt:351
      df      = 4,          # kruskal_wallis_output.txt:352
      p       = "<.001"     # kruskal_wallis_output.txt:353
    )
  )
)


# =============================================================================
# COMPARISON HELPER
# =============================================================================

#' Compare a kruskal_wallis result row against SPSS reference.
compare_kw <- function(row, gs, spss, scenario) {

  # ---- Test statistic + df + p ----
  assert_spss(as.numeric(row$H), spss$h_stat,
              tier = "display", precision = 3,
              label = sprintf("[%s] Kruskal-Wallis H", scenario))
  assert_spss_count(as.numeric(row$df), spss$df,
                    label = sprintf("[%s] df", scenario))
  assert_spss(as.numeric(row$p_value), spss$p,
              tier = "display", precision = 3, what = "p_value",
              label = sprintf("[%s] p-value", scenario))

  # ---- Total N ----
  if (!is.null(spss$total_n)) {
    assert_spss_count(as.numeric(row$n_total), spss$total_n,
                      label = sprintf("[%s] total N", scenario))
  }

  # ---- Per-group descriptives ----
  for (lvl in names(spss$descriptives)) {
    expected <- spss$descriptives[[lvl]]
    actual <- gs[[lvl]]
    if (is.null(actual)) {
      stop(sprintf("[%s] missing R-side group: %s", scenario, lvl),
           call. = FALSE)
    }
    assert_spss_count(actual$n, expected$n,
                      label = sprintf("[%s | %s] N", scenario, lvl))
    assert_spss(actual$rank_mean, expected$rank_mean,
                tier = "display", precision = 2,
                label = sprintf("[%s | %s] Mean Rank", scenario, lvl))
  }
}

extract_single <- function(result) {
  list(row = result$results, gs = result$results$group_stats[[1]])
}

extract_grouped_cell <- function(result, region) {
  r <- result$results
  r <- r[r$region == region, , drop = FALSE]
  if (nrow(r) != 1L) {
    stop(sprintf("expected 1 row for region=%s", region), call. = FALSE)
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

test_that("Test 1a: KW life_satisfaction by education — matches SPSS", {
  r <- survey_data |> kruskal_wallis(life_satisfaction, group = education)
  s <- extract_single(r)
  compare_kw(s$row, s$gs, spss_values$test_1a_life_by_education,
             "1a: life_sat by education")
})

test_that("Test 1b: KW income by employment — matches SPSS", {
  r <- survey_data |> kruskal_wallis(income, group = employment)
  s <- extract_single(r)
  compare_kw(s$row, s$gs, spss_values$test_1b_income_by_employment,
             "1b: income by employment")
})

test_that("Test 1c: KW trust_government by education — matches SPSS", {
  r <- survey_data |> kruskal_wallis(trust_government, group = education)
  s <- extract_single(r)
  compare_kw(s$row, s$gs, spss_values$test_1c_trust_gov_by_education,
             "1c: trust_gov by education")
})


# =============================================================================
# SCENARIO 3 — UNWEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 3a: KW life_satisfaction by education, grouped by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    kruskal_wallis(life_satisfaction, group = education)
  for (rg in c("East", "West")) {
    cell <- extract_grouped_cell(r, rg)
    compare_kw(cell$row, cell$gs,
               spss_values$test_3a_life_by_education_grouped[[rg]],
               sprintf("3a: life_sat by education [%s]", rg))
  }
})

test_that("Test 3b: KW income by employment, grouped by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    kruskal_wallis(income, group = employment)
  for (rg in c("East", "West")) {
    cell <- extract_grouped_cell(r, rg)
    compare_kw(cell$row, cell$gs,
               spss_values$test_3b_income_by_employment_grouped[[rg]],
               sprintf("3b: income by employment [%s]", rg))
  }
})


# =============================================================================
# WEIGHTED SCENARIOS — TIER 4 (R-only baselines, no SPSS comparison)
# =============================================================================
# SPSS NPAR TESTS /K-W does not honor WEIGHT BY (verified at 10 reference
# comparisons in kruskal_wallis_output.txt). See header note for details.
#
# The baselines below are mariposa-only weighted KW values, captured
# 2026-05-19 from R/kruskal_wallis.R. The weighted implementation is a
# frequency-weighted approximation (substitutes sum(w) for n in the
# standard H formula) — NOT design-based / Lumley-Scott. They guard
# against R-side algorithmic regressions, not SPSS equivalence and not
# survey-design correctness. For sampling weights far from 1.0 prefer
# survey::svyranktest().
# =============================================================================

r_only_baselines <- list(
  life_satisfaction_x_education = list(
    H = 167.1485, df = 3L, p_value = 0.000000, eta_squared = 0.068616,
    n_total = 2437L
  ),
  income_x_employment = list(
    H = 30.3525, df = 4L, p_value = 0.000004, eta_squared = 0.013797,
    n_total = 2201L
  ),
  trust_government_x_education = list(
    H = 1.1832, df = 3L, p_value = 0.757044, eta_squared = 0.000499,
    n_total = 2371L
  )
)

compare_weighted_baseline <- function(row, baseline, scenario) {
  for (field in names(baseline)) {
    expected <- baseline[[field]]
    actual   <- as.numeric(row[[field]][1])
    # Integer fields use Spec; numeric fields use Display(3) + IEEE floor
    if (field %in% c("df", "n_total")) {
      assert_spss_count(actual, as.integer(expected),
                        label = sprintf("[%s] %s (R-only Tier-4)",
                                        scenario, field))
    } else {
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
}

test_that("Scenario 2a (weighted life_sat by education) — R-only baseline stable", {
  r <- survey_data |>
    kruskal_wallis(life_satisfaction, group = education, weights = sampling_weight)
  compare_weighted_baseline(r$results,
                             r_only_baselines$life_satisfaction_x_education,
                             "2a: R-only weighted life_sat by education")
})

test_that("Scenario 2b (weighted income by employment) — R-only baseline stable", {
  r <- survey_data |>
    kruskal_wallis(income, group = employment, weights = sampling_weight)
  compare_weighted_baseline(r$results,
                             r_only_baselines$income_x_employment,
                             "2b: R-only weighted income by employment")
})

test_that("Scenario 2c (weighted trust_gov by education) — R-only baseline stable", {
  r <- survey_data |>
    kruskal_wallis(trust_government, group = education, weights = sampling_weight)
  compare_weighted_baseline(r$results,
                             r_only_baselines$trust_government_x_education,
                             "2c: R-only weighted trust_gov by education")
})


# =============================================================================
# EDGE CASES
# =============================================================================

test_that("Edge case: error when grouping variable has < 2 levels", {
  d <- survey_data[survey_data$education == "Basic Secondary", , drop = FALSE]
  d$education <- droplevels(factor(d$education))
  expect_error(
    kruskal_wallis(d, life_satisfaction, group = education),
    regexp = "at least 2"
  )
})

test_that("Edge case: missing values reduce total N exactly by the NAs", {
  d <- survey_data
  na_idx <- which(!is.na(d$life_satisfaction))[1:10]
  d$life_satisfaction[na_idx] <- NA
  base <- kruskal_wallis(survey_data, life_satisfaction, group = education)
  red  <- kruskal_wallis(d,           life_satisfaction, group = education)
  assert_spss_count(red$results$n_total, base$results$n_total - 10L,
                    label = "missing-value edge case: total N reduction by 10")
})


# =============================================================================
# NOTE — SPSS NPAR TESTS WEIGHT QUIRK (second data point)
# =============================================================================
# This file confirms (at the second data point after mann_whitney) that the
# SPSS NPAR TESTS family does not honor WEIGHT BY. The Charter §5.1
# convention catalogue can now categorically state:
#
#   NPAR TESTS family: WEIGHT BY effectively IGNORED; mariposa weighted
#                       outputs are R-only (Tier-4) and not validatable
#                       against SPSS.
#
# This applies to: mann_whitney (confirmed), kruskal_wallis (confirmed),
# wilcoxon_test (predicted), friedman_test (predicted), binomial_test
# (predicted). Future pilots should confirm or refute these predictions.
# =============================================================================
