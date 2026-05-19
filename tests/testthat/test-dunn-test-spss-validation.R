# =============================================================================
# dunn_test — VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::dunn_test() — post-hoc for Kruskal-Wallis.
#
# Reference source:
#   PMCMRplus::kwAllPairsDunnTest(p.adjust.method = "bonferroni")
#   v1.9.x — implements Dunn (1964) + Conover (1999, §5.2.3) tie correction.
#   Captured 2026-05-19 against survey_data (life_satisfaction ~ education,
#   N = 2421 after listwise deletion).
#
# Note: SPSS NPAR TESTS /K-W output does not include Dunn pairwise results
# by default; tests/spss_reference/outputs/dunn_test_output.txt is empty.
# Per Charter §4 Tier-4 we validate against the established R-side reference
# (PMCMRplus), not against a missing SPSS output.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


data(survey_data, envir = environment())


test_that("Dunn life_sat by education matches PMCMRplus (Dunn 1964 + tie corr)", {
  kw <- survey_data |> kruskal_wallis(life_satisfaction, group = education)
  r  <- dunn_test(kw)

  # Structure
  expect_equal(nrow(r$comparisons), 6L)  # C(4,2) = 6 pairs
  expect_true(all(c("group1", "group2", "z", "p", "p_adj") %in%
                  names(r$comparisons)))

  # Z values — exact match to PMCMRplus::kwAllPairsDunnTest
  # (signs differ: mariposa = group1 - group2; PMCMRplus = group2 - group1)
  expected_abs_z <- c(
    "Basic Secondary vs Intermediate Secondary"        =  7.6582,
    "Basic Secondary vs Academic Secondary"            =  9.7919,
    "Basic Secondary vs University"                    = 11.5450,
    "Intermediate Secondary vs Academic Secondary"     =  2.0417,
    "Intermediate Secondary vs University"             =  4.6961,
    "Academic Secondary vs University"                 =  2.8862
  )
  for (pair_name in names(expected_abs_z)) {
    parts <- strsplit(pair_name, " vs ")[[1]]
    row <- r$comparisons[(r$comparisons$group1 == parts[1] & r$comparisons$group2 == parts[2]) |
                         (r$comparisons$group1 == parts[2] & r$comparisons$group2 == parts[1]), ]
    expect_equal(nrow(row), 1L, label = pair_name)
    assert_spss(abs(as.numeric(row$z)), expected_abs_z[pair_name],
                tier = "display", precision = 3,
                label = sprintf("[%s] |Z| vs PMCMRplus", pair_name))
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
  r_bf  <- dunn_test(kw, p_adjust = "bonferroni")
  r_hol <- dunn_test(kw, p_adjust = "holm")
  # Holm <= Bonferroni elementwise
  expect_true(all(r_hol$comparisons$p_adj <= r_bf$comparisons$p_adj + 1e-10))
})
