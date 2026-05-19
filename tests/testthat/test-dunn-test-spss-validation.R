# =============================================================================
# dunn_test — VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::dunn_test() — post-hoc for Kruskal-Wallis.
#
# Note: SPSS NPAR TESTS /K-W output does not include Dunn pairwise results
# by default (Dunn's appears in the newer "Independent Samples" non-parametric
# test, which uses a different convention). The reference file
# tests/spss_reference/outputs/dunn_test_output.txt is largely empty.
#
# Per Charter §4 Tier-4: validate against PMCMRplus::kwAllPairsDunnTest()
# as the reference (the established R-side standard), captured 2026-05-19.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


data(survey_data, envir = environment())


test_that("Dunn life_sat by education: structure + R-only baselines stable", {
  kw <- survey_data |> kruskal_wallis(life_satisfaction, group = education)
  r  <- dunn_test(kw)

  # Structure
  expect_equal(nrow(r$comparisons), 6L)  # C(4,2) = 6 pairs
  expect_true(all(c("group1", "group2", "z", "p", "p_adj") %in%
                  names(r$comparisons)))

  # Z values for canonical 4-group comparison (R-only baseline)
  expected_z <- c(
    "Basic Secondary vs Intermediate Secondary" = -7.402,
    "Basic Secondary vs Academic Secondary"     = -9.465,
    "Basic Secondary vs University"             = -11.159
  )
  for (pair_name in names(expected_z)) {
    parts <- strsplit(pair_name, " vs ")[[1]]
    row <- r$comparisons[(r$comparisons$group1 == parts[1] & r$comparisons$group2 == parts[2]) |
                         (r$comparisons$group1 == parts[2] & r$comparisons$group2 == parts[1]), ]
    expect_equal(nrow(row), 1L, label = pair_name)
    assert_spss(abs(as.numeric(row$z)), abs(expected_z[pair_name]),
                tier = "display", precision = 2,
                label = sprintf("[%s] |Z|", pair_name))
  }

  # Bonferroni-adjusted p for top-significant pair should be very small
  top_pair <- r$comparisons[which.max(abs(r$comparisons$z)), ]
  expect_lt(top_pair$p_adj, 1e-15)
})


test_that("Dunn p-adjust method default is bonferroni", {
  kw <- survey_data |> kruskal_wallis(life_satisfaction, group = education)
  r  <- dunn_test(kw)
  expect_equal(r$p_adjust_method, "bonferroni")
  expect_equal(r$n_comparisons, 6L)
})


test_that("Dunn with holm adjustment produces consistent ordering", {
  kw <- survey_data |> kruskal_wallis(life_satisfaction, group = education)
  r_bf  <- dunn_test(kw, p.adjust.method = "bonferroni")
  r_hol <- dunn_test(kw, p.adjust.method = "holm")
  # Holm <= Bonferroni elementwise
  expect_true(all(r_hol$comparisons$p_adj <= r_bf$comparisons$p_adj + 1e-10))
})
