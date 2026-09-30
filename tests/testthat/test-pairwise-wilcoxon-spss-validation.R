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


# Individual Wilcoxon signed-rank tests per pair; SPSS's Z comes from the
# smaller rank sum, so it is <= 0 (0.7.4: asserted with its sign)
spss_values <- list(
  trust_pairs = list(
    list(var1 = "trust_government", var2 = "trust_media",   z = -5.097),   # wilcoxon_test_output.txt:39
    list(var1 = "trust_government", var2 = "trust_science", z = -25.945),  # wilcoxon_test_output.txt:82
    list(var1 = "trust_media",      var2 = "trust_science", z = -29.091)   # wilcoxon_test_output.txt:125
  ),
  # longitudinal_data_wide, Test 5a (T_j - T_i)
  longitudinal = list(
    list(var1 = "score_T1", var2 = "score_T2", z = -5.427),  # pairwise_wilcoxon_output.txt:507
    list(var1 = "score_T1", var2 = "score_T3", z = -6.132),  # pairwise_wilcoxon_output.txt:527
    list(var1 = "score_T1", var2 = "score_T4", z = -6.378),  # pairwise_wilcoxon_output.txt:547
    list(var1 = "score_T2", var2 = "score_T3", z = -4.413),  # pairwise_wilcoxon_output.txt:567
    list(var1 = "score_T2", var2 = "score_T4", z = -5.012),  # pairwise_wilcoxon_output.txt:587
    list(var1 = "score_T3", var2 = "score_T4", z = -3.085)   # pairwise_wilcoxon_output.txt:607
  )
)


data(survey_data, envir = environment())


test_that("Test 1: pairwise_wilcoxon trust triplet — Z matches SPSS Wilcoxon refs", {
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
    assert_spss(as.numeric(row$z), pair_ref$z,
                tier = "display", precision = 3,
                label = sprintf("[%s vs %s] Z", pair_ref$var1, pair_ref$var2))
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
  for (pair_ref in spss_values$longitudinal) {
    row <- r$comparisons[r$comparisons$var1 == pair_ref$var1 &
                           r$comparisons$var2 == pair_ref$var2, ]
    assert_spss(as.numeric(row$z), pair_ref$z, tier = "display", precision = 3,
                label = sprintf("[5a %s vs %s] Z", pair_ref$var1, pair_ref$var2))
  }
})
