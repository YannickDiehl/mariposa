# =============================================================================
# kendall_tau — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::kendall_tau() against SPSS v29 NONPAR CORR
# (Kendall's tau-b). Reference: kendall_tau_output.txt
#
# Same NONPAR CORR family quirk as Spearman: WEIGHT BY ignored. Tests 1a
# and 2a produce identical tau/p/N values in SPSS. All 4 scenarios are
# validated against the unweighted SPSS reference.
#
# Source audit: kendall_tau.R has no round(sum(w)) bug.
# Output uses $tau (matrix) not $correlations.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(

  test_1a_3var = list(
    cells = list(
      "life_political"  = list(tau = -0.004, p = 0.832, n = 2228),   # kendall_tau_output.txt:12
      "life_trust"      = list(tau = 0.023,  p = 0.176, n = 2291),
      "political_trust" = list(tau = 0.003,  p = 0.883, n = 2177)
    ),
    diag_n = c(life_satisfaction = 2421L, political_orientation = 2299L,
               trust_media = 2367L)
  ),

  test_3a_grouped = list(
    East = list(
      cells = list(
        "life_political"  = list(tau = 0.007,  p = 0.851, n = 427),   # kendall_tau_output.txt:287
        "life_trust"      = list(tau = -0.049, p = 0.216, n = 440),
        "political_trust" = list(tau = 0.058,  p = 0.153, n = 420)
      ),
      diag_n = c(life_satisfaction = 465L, political_orientation = 443L,
                 trust_media = 460L)
    ),
    West = list(
      cells = list(
        "life_political"  = list(tau = -0.007, p = 0.733, n = 1801),  # kendall_tau_output.txt:299
        "life_trust"      = list(tau = 0.040,  p = 0.037, n = 1851),
        "political_trust" = list(tau = -0.010, p = 0.608, n = 1757)
      ),
      diag_n = c(life_satisfaction = 1956L, political_orientation = 1856L,
                 trust_media = 1907L)
    )
  )
)


compare_kendall <- function(matrices, spss, scenario) {
  tau_mat <- matrices$tau
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
    assert_spss(tau_mat[pair[1], pair[2]], expected$tau,
                tier = "display", precision = 3,
                label = sprintf("[%s | %s] tau", scenario, cell_name))
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


data(survey_data, envir = environment())


test_that("Test 1a: Kendall 3-var unweighted ungrouped — matches SPSS", {
  r <- survey_data |>
    kendall_tau(life_satisfaction, political_orientation, trust_media)
  compare_kendall(r$matrices[[1]], spss_values$test_1a_3var,
                  "1a: unweighted ungrouped")
})

test_that("Test 2a: Kendall weighted ungrouped — R-only Tier-4 baseline", {
  # mariposa kendall_tau DOES honor weights (different from spearman_rho
  # behaviour), but SPSS NONPAR CORR /KENDALL ignores WEIGHT BY. The two
  # implementations diverge meaningfully here. mariposa's weighted output
  # is a separate Tier-4 baseline.
  #
  # Re-captured 2026-07: the weighted tau-b denominator previously omitted
  # double-tied pairs (ties_both) from both factors, deflating |tau|. Fixed
  # to mirror the unweighted (n0 - Tx - Txy)(n0 - Ty - Txy) formula; the
  # weights == 1 reduction to unweighted tau is now guarded by the
  # invariance suite in test-weights-invariance.R. Weighted p remains a
  # documented no-ties approximation.
  r <- survey_data |>
    kendall_tau(life_satisfaction, political_orientation, trust_media,
                weights = sampling_weight)
  m <- r$matrices[[1]]
  baseline <- list(
    life_political_tau  = -0.004541,  life_political_p  = 0.7473,  life_political_n  = 2241L,
    life_trust_tau      = 0.022552,   life_trust_p      = 0.1046,  life_trust_n      = 2305L,
    political_trust_tau = 0.003664,   political_trust_p = 0.7972,  political_trust_n = 2190L,
    diag_n_life = 2437L, diag_n_pol = 2312L, diag_n_tm = 2382L
  )
  assert_spss(m$tau["life_satisfaction","political_orientation"],
              baseline$life_political_tau,
              tier = "display", precision = 3,
              label = "2a Tier-4: life_political tau")
  assert_spss(m$tau["life_satisfaction","trust_media"],
              baseline$life_trust_tau,
              tier = "display", precision = 3,
              label = "2a Tier-4: life_trust tau")
  assert_spss(m$tau["political_orientation","trust_media"],
              baseline$political_trust_tau,
              tier = "display", precision = 3,
              label = "2a Tier-4: political_trust tau")
  assert_spss_count(m$n_obs["life_satisfaction","political_orientation"],
                    baseline$life_political_n,
                    label = "2a Tier-4: life_political N")
})

test_that("Test 3a: Kendall unweighted grouped by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    kendall_tau(life_satisfaction, political_orientation, trust_media)
  regions_in_order <- unique(r$correlations$region)
  for (i in seq_along(r$matrices)) {
    rg <- as.character(regions_in_order[i])
    compare_kendall(r$matrices[[i]], spss_values$test_3a_grouped[[rg]],
                    sprintf("3a: unweighted grouped [%s]", rg))
  }
})

test_that("Test 4a: Kendall weighted grouped by region — R-only Tier-4 baselines", {
  r <- survey_data |>
    group_by(region) |>
    kendall_tau(life_satisfaction, political_orientation, trust_media,
                weights = sampling_weight)
  regions_in_order <- unique(r$correlations$region)

  # Region-ordered baselines, captured 2026-05-19; re-captured 2026-07
  # after the weighted tau-b double-tie denominator fix (see Test 2a).
  baselines <- list(
    East = list(life_political_tau = 0.005992, n_diag_life = 488L),
    West = list(life_political_tau = -0.007363, n_diag_life = 1949L)
  )

  for (i in seq_along(r$matrices)) {
    rg <- as.character(regions_in_order[i])
    m  <- r$matrices[[i]]
    b  <- baselines[[rg]]
    assert_spss(m$tau["life_satisfaction","political_orientation"],
                b$life_political_tau,
                tier = "display", precision = 3,
                label = sprintf("4a Tier-4 [%s]: life_political tau", rg))
    assert_spss_count(m$n_obs["life_satisfaction","life_satisfaction"],
                      b$n_diag_life,
                      label = sprintf("4a Tier-4 [%s]: diag N(life)", rg))
  }
})
