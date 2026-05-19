# =============================================================================
# spearman_rho — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::spearman_rho() against SPSS v29 NONPAR CORR.
# Reference output: tests/spss_reference/outputs/spearman_rho_output.txt
#
# NONPAR CORR family (Spearman) does NOT honor WEIGHT BY — same quirk as
# NPAR TESTS. SPSS Tests 2a (weighted) produce identical rho/p/N as Tests
# 1a (unweighted). mariposa's spearman_rho likewise does not produce
# different weighted output (verified empirically). All 4 scenarios are
# therefore comparable against the SPSS unweighted reference.
#
# Source audit (R/spearman_rho.R): n_eff <- n (line 201) for unweighted;
# weighted path also uses sum(w) appropriately. No round(sum(w)) bug.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# =============================================================================
# SPSS REFERENCE VALUES
# =============================================================================

spss_values <- list(

  # ---- Tests 1a/2a: 3-variable matrix (canonical) ----------------------
  # NONPAR CORR ignores WEIGHT BY, so 1a == 2a numerically.
  test_1a_3var = list(
    cells = list(
      "life_political"  = list(r = -0.004, p = 0.833, n = 2228),   # spearman_rho_output.txt:12
      "life_trust"      = list(r = 0.028,  p = 0.181, n = 2291),   # spearman_rho_output.txt:12
      "political_trust" = list(r = 0.003,  p = 0.885, n = 2177)    # spearman_rho_output.txt:15
    ),
    diag_n = c(life_satisfaction = 2421L,
               political_orientation = 2299L,
               trust_media = 2367L)
  ),

  # ---- Tests 3a/4a: grouped by region (also NPAR-ignored for weights) --
  test_3a_grouped = list(
    East = list(
      cells = list(
        "life_political"  = list(r = 0.010,  p = 0.840, n = 427),  # spearman_rho_output.txt:285
        "life_trust"      = list(r = -0.059, p = 0.219, n = 440),
        "political_trust" = list(r = 0.070,  p = 0.153, n = 420)
      ),
      diag_n = c(life_satisfaction = 465L, political_orientation = 443L,
                 trust_media = 460L)
    ),
    West = list(
      cells = list(
        "life_political"  = list(r = -0.008, p = 0.730, n = 1801), # spearman_rho_output.txt:297
        "life_trust"      = list(r = 0.048,  p = 0.038, n = 1851),
        "political_trust" = list(r = -0.012, p = 0.608, n = 1757)
      ),
      diag_n = c(life_satisfaction = 1956L, political_orientation = 1856L,
                 trust_media = 1907L)
    )
  )
)


# =============================================================================
# COMPARISON HELPER
# =============================================================================

compare_spearman <- function(matrices, spss, scenario) {
  rho_mat <- matrices$rho
  p_mat   <- matrices$p_values
  n_mat   <- matrices$n_obs

  cell_map <- list(
    "life_political"  = c("life_satisfaction", "political_orientation"),
    "life_trust"      = c("life_satisfaction", "trust_media"),
    "political_trust" = c("political_orientation", "trust_media")
  )
  for (cell_name in names(spss$cells)) {
    expected <- spss$cells[[cell_name]]
    pair <- cell_map[[cell_name]]
    assert_spss(rho_mat[pair[1], pair[2]], expected$r,
                tier = "display", precision = 3,
                label = sprintf("[%s | %s] rho", scenario, cell_name))
    assert_spss(p_mat[pair[1], pair[2]], expected$p,
                tier = "display", precision = 3, what = "p_value",
                label = sprintf("[%s | %s] Sig", scenario, cell_name))
    assert_spss_count(n_mat[pair[1], pair[2]], expected$n,
                      label = sprintf("[%s | %s] N", scenario, cell_name))
  }

  for (var in names(spss$diag_n)) {
    assert_spss_count(n_mat[var, var], spss$diag_n[[var]],
                      label = sprintf("[%s | %s] diag N", scenario, var))
  }
}


# =============================================================================
# DATA SETUP
# =============================================================================

data(survey_data, envir = environment())


# =============================================================================
# SCENARIO 1 — UNWEIGHTED / UNGROUPED
# =============================================================================

test_that("Test 1a: Spearman 3-variable unweighted ungrouped — matches SPSS", {
  r <- survey_data |>
    spearman_rho(life_satisfaction, political_orientation, trust_media)
  compare_spearman(r$matrices[[1]], spss_values$test_1a_3var,
                   "1a: unweighted ungrouped")
})


# =============================================================================
# SCENARIO 2 — WEIGHTED / UNGROUPED (identical to 1a due to NONPAR quirk)
# =============================================================================

test_that("Test 2a: Spearman weighted ungrouped — matches SPSS (NPAR quirk)", {
  r <- survey_data |>
    spearman_rho(life_satisfaction, political_orientation, trust_media,
                 weights = sampling_weight)
  # SPSS NONPAR CORR ignores WEIGHT BY; reference == unweighted Test 1a.
  compare_spearman(r$matrices[[1]], spss_values$test_1a_3var,
                   "2a: weighted ungrouped (SPSS treats as unweighted)")
})


# =============================================================================
# SCENARIO 3 — UNWEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 3a: Spearman unweighted grouped by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    spearman_rho(life_satisfaction, political_orientation, trust_media)
  # spearman_rho stores grouped matrices in r$matrices, ordered by the
  # first appearance of each region in r$correlations.
  regions_in_order <- unique(r$correlations$region)
  for (i in seq_along(r$matrices)) {
    rg <- as.character(regions_in_order[i])
    compare_spearman(r$matrices[[i]], spss_values$test_3a_grouped[[rg]],
                     sprintf("3a: unweighted grouped [%s]", rg))
  }
})


# =============================================================================
# SCENARIO 4 — WEIGHTED / GROUPED by region (identical to 3a)
# =============================================================================

test_that("Test 4a: Spearman weighted grouped by region — matches SPSS (NPAR quirk)", {
  r <- survey_data |>
    group_by(region) |>
    spearman_rho(life_satisfaction, political_orientation, trust_media,
                 weights = sampling_weight)
  regions_in_order <- unique(r$correlations$region)
  for (i in seq_along(r$matrices)) {
    rg <- as.character(regions_in_order[i])
    compare_spearman(r$matrices[[i]], spss_values$test_3a_grouped[[rg]],
                     sprintf("4a: weighted grouped [%s]", rg))
  }
})
