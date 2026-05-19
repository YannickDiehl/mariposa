# =============================================================================
# pearson_cor — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::pearson_cor() against SPSS v29 CORRELATIONS.
# Reference output: tests/spss_reference/outputs/pearson_cor_output.txt
#
# Charter reference: .claude/VALIDATION_CHARTER.md
#
# SPSS-procedure-family finding: CORRELATIONS DOES honor WEIGHT BY (unlike
# NPAR TESTS). Weighted runs produce different r/p/N values. All 4 canonical
# scenarios are SPSS-validatable.
#
# Source audit (R/pearson_cor.R): n_eff <- sum(w) (line 221) — correct,
# uses unrounded sum(w). round(sum(w)) at line 333 is for n_matrix display
# only. No bug.
#
# Variables: life_satisfaction × political_orientation × trust_media
# (chosen for non-trivial NA patterns; tests pairwise deletion).
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# =============================================================================
# SPSS REFERENCE VALUES
# =============================================================================

spss_values <- list(

  # ---- Test 1: Unweighted / Ungrouped (3x3 correlation matrix) ---------
  test_1 = list(
    cells = list(
      # life_satisfaction vs political_orientation
      "life_political"  = list(r = -0.001, p = 0.962, n = 2228),  # pearson_cor_output.txt:9-11 column 2
      # life_satisfaction vs trust_media
      "life_trust"      = list(r = 0.021,  p = 0.312, n = 2291),  # pearson_cor_output.txt:9-11 column 3
      # political_orientation vs trust_media
      "political_trust" = list(r = 0.002,  p = 0.909, n = 2177)   # pearson_cor_output.txt:12-14 column 3
    ),
    diag_n = c(life_satisfaction = 2421L,                          # pearson_cor_output.txt:11
               political_orientation = 2299L,                       # pearson_cor_output.txt:14
               trust_media = 2367L)                                 # pearson_cor_output.txt:17
  ),

  # ---- Test 2: Weighted / Ungrouped ------------------------------------
  test_2 = list(
    cells = list(
      "life_political"  = list(r = -0.004, p = 0.836, n = 2241),   # pearson_cor_output.txt:27-29
      "life_trust"      = list(r = 0.020,  p = 0.330, n = 2305),   # pearson_cor_output.txt:27-29
      "political_trust" = list(r = 0.004,  p = 0.835, n = 2190)    # pearson_cor_output.txt:30-32
    ),
    diag_n = c(life_satisfaction = 2437L, political_orientation = 2312L,
               trust_media = 2382L)
  ),

  # ---- Test 3: Unweighted / Grouped by region --------------------------
  test_3 = list(
    East = list(
      cells = list(
        "life_political"  = list(r = 0.008,  p = 0.876, n = 427),   # pearson_cor_output.txt:45-47
        "life_trust"      = list(r = -0.062, p = 0.197, n = 440),
        "political_trust" = list(r = 0.087,  p = 0.074, n = 420)
      ),
      diag_n = c(life_satisfaction = 465L, political_orientation = 443L,
                 trust_media = 460L)
    ),
    West = list(
      cells = list(
        "life_political"  = list(r = -0.003, p = 0.894, n = 1801),  # pearson_cor_output.txt:54-56
        "life_trust"      = list(r = 0.041,  p = 0.080, n = 1851),
        "political_trust" = list(r = -0.017, p = 0.480, n = 1757)
      ),
      diag_n = c(life_satisfaction = 1956L, political_orientation = 1856L,
                 trust_media = 1907L)
    )
  ),

  # ---- Test 4: Weighted / Grouped by region ----------------------------
  test_4 = list(
    East = list(
      cells = list(
        "life_political"  = list(r = 0.001,  p = 0.978, n = 447),
        "life_trust"      = list(r = -0.058, p = 0.210, n = 462),
        "political_trust" = list(r = 0.091,  p = 0.056, n = 440)
      ),
      diag_n = c(life_satisfaction = 488L, political_orientation = 464L,
                 trust_media = 483L)
    ),
    West = list(
      cells = list(
        "life_political"  = list(r = -0.006, p = 0.804, n = 1794),
        "life_trust"      = list(r = 0.040,  p = 0.087, n = 1844),
        "political_trust" = list(r = -0.016, p = 0.493, n = 1749)
      ),
      diag_n = c(life_satisfaction = 1949L, political_orientation = 1848L,
                 trust_media = 1899L)
    )
  )
)


# =============================================================================
# COMPARISON HELPER
# =============================================================================

compare_pearson <- function(matrices, spss, scenario, is_weighted = FALSE) {
  cor_mat <- matrices$correlations
  p_mat   <- matrices$p_values
  n_mat   <- matrices$n_obs

  # Off-diagonal cells (upper triangle)
  cell_map <- list(
    "life_political"  = c("life_satisfaction", "political_orientation"),
    "life_trust"      = c("life_satisfaction", "trust_media"),
    "political_trust" = c("political_orientation", "trust_media")
  )
  for (cell_name in names(spss$cells)) {
    expected <- spss$cells[[cell_name]]
    pair <- cell_map[[cell_name]]
    actual_r <- cor_mat[pair[1], pair[2]]
    actual_p <- p_mat[pair[1], pair[2]]
    actual_n <- n_mat[pair[1], pair[2]]

    assert_spss(actual_r, expected$r,
                tier = "display", precision = 3,
                label = sprintf("[%s | %s] r", scenario, cell_name))
    assert_spss(actual_p, expected$p,
                tier = "display", precision = 3, what = "p_value",
                label = sprintf("[%s | %s] Sig", scenario, cell_name))
    # N: integer for unweighted, display(0) for weighted (rounded sum(w))
    if (is_weighted) {
      assert_spss(actual_n, expected$n,
                  tier = "display", precision = 0,
                  label = sprintf("[%s | %s] N", scenario, cell_name))
    } else {
      assert_spss_count(actual_n, expected$n,
                        label = sprintf("[%s | %s] N", scenario, cell_name))
    }
  }

  # Diagonal N (variable's own N, listwise)
  for (var in names(spss$diag_n)) {
    expected <- spss$diag_n[[var]]
    actual <- n_mat[var, var]
    if (is_weighted) {
      assert_spss(actual, expected,
                  tier = "display", precision = 0,
                  label = sprintf("[%s | %s] diag N", scenario, var))
    } else {
      assert_spss_count(actual, expected,
                        label = sprintf("[%s | %s] diag N", scenario, var))
    }
  }
}


# =============================================================================
# DATA SETUP
# =============================================================================

data(survey_data, envir = environment())


# =============================================================================
# SCENARIO 1 — UNWEIGHTED / UNGROUPED
# =============================================================================

test_that("Test 1: Pearson 3-variable unweighted ungrouped — matches SPSS", {
  r <- survey_data |>
    pearson_cor(life_satisfaction, political_orientation, trust_media)
  compare_pearson(r$matrices[[1]], spss_values$test_1,
                  "1: unweighted ungrouped", is_weighted = FALSE)
})


# =============================================================================
# SCENARIO 2 — WEIGHTED / UNGROUPED
# =============================================================================

test_that("Test 2: Pearson 3-variable weighted ungrouped — matches SPSS", {
  r <- survey_data |>
    pearson_cor(life_satisfaction, political_orientation, trust_media,
                weights = sampling_weight)
  compare_pearson(r$matrices[[1]], spss_values$test_2,
                  "2: weighted ungrouped", is_weighted = TRUE)
})


# =============================================================================
# SCENARIO 3 — UNWEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 3: Pearson 3-variable unweighted grouped by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    pearson_cor(life_satisfaction, political_orientation, trust_media)
  # matrices is named/keyed by group; we look up by name
  for (i in seq_along(r$matrices)) {
    rg <- as.character(r$group_keys$region[i])
    compare_pearson(r$matrices[[i]], spss_values$test_3[[rg]],
                    sprintf("3: unweighted grouped [%s]", rg),
                    is_weighted = FALSE)
  }
})


# =============================================================================
# SCENARIO 4 — WEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 4: Pearson 3-variable weighted grouped by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    pearson_cor(life_satisfaction, political_orientation, trust_media,
                weights = sampling_weight)
  for (i in seq_along(r$matrices)) {
    rg <- as.character(r$group_keys$region[i])
    compare_pearson(r$matrices[[i]], spss_values$test_4[[rg]],
                    sprintf("4: weighted grouped [%s]", rg),
                    is_weighted = TRUE)
  }
})


# =============================================================================
# EDGE CASES
# =============================================================================

test_that("Edge case: 2 variables produces 2x2 matrix with off-diagonal = SPSS r", {
  r <- survey_data |>
    pearson_cor(life_satisfaction, political_orientation)
  cor_mat <- r$matrices[[1]]$correlations
  expect_equal(dim(cor_mat), c(2L, 2L))
  # Off-diagonal should match Test 1's life_political cell
  assert_spss(cor_mat["life_satisfaction", "political_orientation"], -0.001,
              tier = "display", precision = 3,
              label = "2-var edge case: r")
})
