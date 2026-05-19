# =============================================================================
# pairwise_wilcoxon — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::pairwise_wilcoxon() (post-hoc for Friedman)
#          against SPSS pairwise Wilcoxon signed-rank tests.
# Reference: pairwise_wilcoxon_output.txt
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# Same |Z| values as wilcoxon_test pilot (since these are individual pairwise
# Wilcoxons applied to the trust triplet for the Friedman post-hoc)
spss_values <- list(
  trust_pairs = list(
    list(var1 = "trust_government", var2 = "trust_media",   z = -5.097),
    list(var1 = "trust_government", var2 = "trust_science", z = -25.945),
    list(var1 = "trust_media",      var2 = "trust_science", z = -29.091)
  )
)


data(survey_data, envir = environment())


test_that("Test 1: pairwise_wilcoxon trust triplet — |Z| matches SPSS Wilcoxon refs", {
  fr <- survey_data |>
    friedman_test(trust_government, trust_media, trust_science)
  r  <- pairwise_wilcoxon(fr)

  expect_equal(nrow(r$comparisons), 3L)

  for (pair_ref in spss_values$trust_pairs) {
    row <- r$comparisons[
      (r$comparisons$var1 == pair_ref$var1 & r$comparisons$var2 == pair_ref$var2) |
      (r$comparisons$var1 == pair_ref$var2 & r$comparisons$var2 == pair_ref$var1), ]
    expect_equal(nrow(row), 1L,
                 label = sprintf("%s / %s pair found", pair_ref$var1, pair_ref$var2))
    assert_spss(abs(as.numeric(row$z)), abs(pair_ref$z),
                tier = "display", precision = 3,
                label = sprintf("[%s vs %s] |Z|", pair_ref$var1, pair_ref$var2))
  }

  # All p_adj should be effectively zero (extremely strong differences)
  expect_true(all(r$comparisons$p_adj < 1e-5))
})


test_that("pairwise_wilcoxon Bonferroni adjustment ≥ raw p", {
  fr <- survey_data |>
    friedman_test(trust_government, trust_media, trust_science)
  r  <- pairwise_wilcoxon(fr)
  expect_true(all(r$comparisons$p_adj >= r$comparisons$p - 1e-15))
})


test_that("pairwise_wilcoxon for longitudinal_data_wide — 6 pairs from 4 timepoints", {
  data(longitudinal_data_wide, envir = environment())
  fr <- longitudinal_data_wide |>
    friedman_test(score_T1, score_T2, score_T3, score_T4)
  r  <- pairwise_wilcoxon(fr)
  expect_equal(nrow(r$comparisons), 6L)  # C(4,2)
})
