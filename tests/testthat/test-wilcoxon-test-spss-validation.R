# =============================================================================
# wilcoxon_test — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::wilcoxon_test() against SPSS v29 NPAR /WILCOXON.
# Reference output: tests/spss_reference/outputs/wilcoxon_test_output.txt
#
# Charter reference: .claude/VALIDATION_CHARTER.md
#
# Third NPAR-TESTS data point confirming WEIGHT-BY ignored quirk:
# Weighted runs (Tests 2a-b, 4a-b) produce IDENTICAL Z and Sig as their
# unweighted counterparts (Tests 1a-c, 3a-b). Pattern holds.
#
# Mariposa source convention check (R/wilcoxon_test.R):
#   Grep round(sum(w)): line 263 — n_total <- round(sum(w))
#   Context: display value only (n column in output); not in any
#   calculation path. Not a bug.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# =============================================================================
# SPSS REFERENCE VALUES
# =============================================================================

spss_values <- list(

  # ---- Test 1a: trust_government WITH trust_media ----------------------
  # SPSS computes diff = trust_media - trust_government
  test_1a_gov_media = list(
    n_neg = 955L, mean_rank_neg = 887.53, sum_rank_neg = 847592.50,  # wilcoxon_test_output.txt:28
    n_pos = 770L, mean_rank_pos = 832.57, sum_rank_pos = 641082.50,  # wilcoxon_test_output.txt:29
    n_ties = 502L,                                                    # wilcoxon_test_output.txt:30
    n_total = 2227L,                                                  # wilcoxon_test_output.txt:31
    z = -5.097,                                                       # wilcoxon_test_output.txt:39
    p = "<.001"                                                       # wilcoxon_test_output.txt:40
  ),

  # ---- Test 1b: trust_government WITH trust_science ---------------------
  test_1b_gov_science = list(
    n_neg = 355L,  mean_rank_neg = 671.62, sum_rank_neg = 238424.00,   # wilcoxon_test_output.txt:71
    n_pos = 1424L, mean_rank_pos = 944.44, sum_rank_pos = 1344886.00,  # wilcoxon_test_output.txt:72
    n_ties = 476L,                                                     # wilcoxon_test_output.txt:73
    n_total = 2255L,                                                   # wilcoxon_test_output.txt:74
    z = -25.945,                                                       # wilcoxon_test_output.txt:82
    p = "<.001"                                                        # wilcoxon_test_output.txt:83
  ),

  # ---- Test 1c: trust_media WITH trust_science --------------------------
  test_1c_media_science = list(
    n_neg = 322L,  mean_rank_neg = 648.09,  sum_rank_neg = 208685.50,   # wilcoxon_test_output.txt:114
    n_pos = 1558L, mean_rank_pos = 1000.93, sum_rank_pos = 1559454.50,  # wilcoxon_test_output.txt:115
    n_ties = 392L,                                                      # wilcoxon_test_output.txt:116
    n_total = 2272L,                                                    # wilcoxon_test_output.txt:117
    z = -29.091,                                                        # wilcoxon_test_output.txt:125
    p = "<.001"                                                         # wilcoxon_test_output.txt:126
  ),

  # ---- Test 3a: trust_gov WITH trust_media, grouped by region ----------
  test_3a_gov_media_grouped = list(
    East = list(
      n_neg = 191L, mean_rank_neg = 183.16, sum_rank_neg = 34983.50,    # wilcoxon_test_output.txt:243
      n_pos = 155L, mean_rank_pos = 161.60, sum_rank_pos = 25047.50,    # wilcoxon_test_output.txt:244
      n_ties = 89L,                                                     # wilcoxon_test_output.txt:245
      n_total = 435L,                                                   # row total at line 246
      z = -2.727,                                                       # wilcoxon_test_output.txt:258
      p = 0.006                                                         # wilcoxon_test_output.txt:259
    ),
    West = list(
      n_neg = 764L, mean_rank_neg = 705.10, sum_rank_neg = 538697.50,   # wilcoxon_test_output.txt:247
      n_pos = 615L, mean_rank_pos = 671.24, sum_rank_pos = 412812.50,   # wilcoxon_test_output.txt:248
      n_ties = 413L,                                                    # wilcoxon_test_output.txt:249
      n_total = 1792L,                                                  # row total at line 250
      z = -4.346,                                                       # wilcoxon_test_output.txt:260
      p = "<.001"                                                       # wilcoxon_test_output.txt:261
    )
  ),

  # ---- Test 3b: trust_gov WITH trust_science, grouped by region --------
  test_3b_gov_science_grouped = list(
    East = list(
      n_neg = 64L,  mean_rank_neg = 142.14, sum_rank_neg = 9097.00,     # wilcoxon_test_output.txt:292
      n_pos = 287L, mean_rank_pos = 183.55, sum_rank_pos = 52679.00,    # wilcoxon_test_output.txt:293
      n_ties = 93L,                                                     # wilcoxon_test_output.txt:294
      n_total = 444L,                                                   # wilcoxon_test_output.txt:295
      z = -11.635,                                                      # wilcoxon_test_output.txt:307
      p = "<.001"                                                       # wilcoxon_test_output.txt:308
    ),
    West = list(
      n_neg = 291L,  mean_rank_neg = 531.02, sum_rank_neg = 154526.50,  # wilcoxon_test_output.txt:296
      n_pos = 1137L, mean_rank_pos = 761.46, sum_rank_pos = 865779.50,  # wilcoxon_test_output.txt:297
      n_ties = 383L,                                                    # wilcoxon_test_output.txt:298
      n_total = 1811L,                                                  # wilcoxon_test_output.txt:299
      z = -23.191,                                                      # wilcoxon_test_output.txt:309
      p = "<.001"                                                       # wilcoxon_test_output.txt:310
    )
  )
)


# =============================================================================
# COMPARISON HELPER
# =============================================================================

#' Compare a wilcoxon_test row against SPSS reference.
#'
#' SPSS PAIRED WILCOXON computes diff = second - first; negative ranks are
#' rows where second < first. mariposa's wilcoxon_test(data, var1, var2)
#' uses the same convention (verified empirically against Test 1a).
compare_wilcoxon <- function(row, spss, scenario) {
  # Test statistics — SPSS convention picks the smaller-rank-sum side and
  # reports Z as negative; mariposa returns the signed (V - E_V)/sqrt(Var_V)
  # which can be positive. Compare absolute values: the test of significance
  # depends on |Z|, not its sign. Magnitude must match.
  assert_spss(abs(as.numeric(row$Z)), abs(spss$z),
              tier = "display", precision = 3,
              label = sprintf("[%s] |Z|", scenario))
  assert_spss(as.numeric(row$p_value), spss$p,
              tier = "display", precision = 3, what = "p_value",
              label = sprintf("[%s] p-value", scenario))

  # Sign counts
  assert_spss_count(as.numeric(row$n_neg), spss$n_neg,
                    label = sprintf("[%s] N negative ranks", scenario))
  assert_spss_count(as.numeric(row$n_pos), spss$n_pos,
                    label = sprintf("[%s] N positive ranks", scenario))
  assert_spss_count(as.numeric(row$n_ties), spss$n_ties,
                    label = sprintf("[%s] N ties", scenario))
  assert_spss_count(as.numeric(row$n_total), spss$n_total,
                    label = sprintf("[%s] N total", scenario))

  # Mean ranks
  assert_spss(as.numeric(row$mean_rank_neg), spss$mean_rank_neg,
              tier = "display", precision = 2,
              label = sprintf("[%s] mean rank (negative)", scenario))
  assert_spss(as.numeric(row$mean_rank_pos), spss$mean_rank_pos,
              tier = "display", precision = 2,
              label = sprintf("[%s] mean rank (positive)", scenario))

  # Sum of ranks
  assert_spss(as.numeric(row$sum_rank_neg), spss$sum_rank_neg,
              tier = "display", precision = 2,
              label = sprintf("[%s] sum of ranks (negative)", scenario))
  assert_spss(as.numeric(row$sum_rank_pos), spss$sum_rank_pos,
              tier = "display", precision = 2,
              label = sprintf("[%s] sum of ranks (positive)", scenario))
}

extract_grouped_row <- function(result, region) {
  r <- result$results
  r <- r[r$region == region, , drop = FALSE]
  if (nrow(r) != 1L) {
    stop(sprintf("expected 1 row for region=%s", region), call. = FALSE)
  }
  r
}


# =============================================================================
# DATA SETUP
# =============================================================================

data(survey_data, envir = environment())


# =============================================================================
# SCENARIO 1 — UNWEIGHTED / UNGROUPED
# =============================================================================

test_that("Test 1a: Wilcoxon trust_gov / trust_media — matches SPSS", {
  r <- survey_data |> wilcoxon_test(trust_government, trust_media)
  compare_wilcoxon(r$results, spss_values$test_1a_gov_media,
                   "1a: trust_gov / trust_media")
})

test_that("Test 1b: Wilcoxon trust_gov / trust_science — matches SPSS", {
  r <- survey_data |> wilcoxon_test(trust_government, trust_science)
  compare_wilcoxon(r$results, spss_values$test_1b_gov_science,
                   "1b: trust_gov / trust_science")
})

test_that("Test 1c: Wilcoxon trust_media / trust_science — matches SPSS", {
  r <- survey_data |> wilcoxon_test(trust_media, trust_science)
  compare_wilcoxon(r$results, spss_values$test_1c_media_science,
                   "1c: trust_media / trust_science")
})


# =============================================================================
# SCENARIO 3 — UNWEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 3a: Wilcoxon trust_gov / trust_media, grouped by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    wilcoxon_test(trust_government, trust_media)
  for (rg in c("East", "West")) {
    row <- extract_grouped_row(r, rg)
    compare_wilcoxon(row, spss_values$test_3a_gov_media_grouped[[rg]],
                     sprintf("3a: trust_gov / trust_media [%s]", rg))
  }
})

test_that("Test 3b: Wilcoxon trust_gov / trust_science, grouped by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    wilcoxon_test(trust_government, trust_science)
  for (rg in c("East", "West")) {
    row <- extract_grouped_row(r, rg)
    compare_wilcoxon(row, spss_values$test_3b_gov_science_grouped[[rg]],
                     sprintf("3b: trust_gov / trust_science [%s]", rg))
  }
})


# =============================================================================
# WEIGHTED SCENARIOS — TIER 4 (R-only baselines, no SPSS comparison)
# =============================================================================
# SPSS NPAR /WILCOXON does not honor WEIGHT BY. mariposa baselines from
# the design-based implementation in R/wilcoxon_test.R, captured 2026-05-19.
# =============================================================================

r_only_baselines <- list(
  gov_media    = list(Z = -5.0326,  p_value = 0.0000005, r_effect = 0.120820, n_total = 2242L),
  gov_science  = list(Z = 25.9969,  p_value = 0.0000000, r_effect = 0.614462, n_total = 2271L),
  media_science = list(Z = 29.0949, p_value = 0.0000000, r_effect = 0.669424, n_total = 2286L)
)

compare_weighted_baseline <- function(row, baseline, scenario) {
  for (field in names(baseline)) {
    expected <- baseline[[field]]
    actual   <- as.numeric(row[[field]][1])
    if (field == "n_total") {
      assert_spss_count(actual, as.integer(expected),
                        label = sprintf("[%s] %s (R-only Tier-4)", scenario, field))
    } else {
      tolerance <- max(1e-4, abs(expected) * 1e-6)
      testthat::expect_lte(
        abs(actual - expected), tolerance,
        label = sprintf("[%s] %s: actual=%s, baseline=%s (R-only Tier-4)",
                        scenario, field,
                        format(actual, digits = 8),
                        format(expected, digits = 8))
      )
    }
  }
}

test_that("Scenario 2a: weighted trust_gov / trust_media — R-only baseline", {
  r <- survey_data |>
    wilcoxon_test(trust_government, trust_media, weights = sampling_weight)
  compare_weighted_baseline(r$results, r_only_baselines$gov_media,
                            "2a: weighted gov/media")
})

test_that("Scenario 2b: weighted trust_gov / trust_science — R-only baseline", {
  r <- survey_data |>
    wilcoxon_test(trust_government, trust_science, weights = sampling_weight)
  compare_weighted_baseline(r$results, r_only_baselines$gov_science,
                            "2b: weighted gov/science")
})

test_that("Scenario 2c: weighted trust_media / trust_science — R-only baseline", {
  r <- survey_data |>
    wilcoxon_test(trust_media, trust_science, weights = sampling_weight)
  compare_weighted_baseline(r$results, r_only_baselines$media_science,
                            "2c: weighted media/science")
})


# =============================================================================
# EDGE CASES
# =============================================================================

test_that("Edge case: missing values reduce n_total exactly by NA count", {
  d <- survey_data
  na_idx <- which(!is.na(d$trust_government) & !is.na(d$trust_media))[1:10]
  d$trust_government[na_idx] <- NA
  base <- wilcoxon_test(survey_data, trust_government, trust_media)
  red  <- wilcoxon_test(d,           trust_government, trust_media)
  assert_spss_count(red$results$n_total, base$results$n_total - 10L,
                    label = "missing-value edge case")
})
