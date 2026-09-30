# =============================================================================
# Practice-test regressions: result export and knitr (batch X2)
# =============================================================================
# One block per finding of the 2026-09 field report (phase 2: parametric,
# nonparametric, scales, edge-case and io/labels reports). Each test names
# the finding ID and what was wrong.
# =============================================================================

library(testthat)
library(dplyr)

data(survey_data, envir = environment())

# --- UX-EXPORT (NP-22): post-hoc tables under $results --------------------------

test_that("NP-22 dunn_test() and pairwise_wilcoxon() expose their table as $results", {
  # Was: the comparison table lived only in $comparisons while $results
  # was NULL - unlike every other result class, so x$results silently gave
  # NULL (and a weights-invariance test compared NULL with NULL).
  dn <- dunn_test(kruskal_wallis(survey_data, age, group = education))
  expect_s3_class(dn$results, "data.frame")
  expect_identical(dn$results, dn$comparisons)
  expect_equal(nrow(dn$results), 6L)

  pw <- pairwise_wilcoxon(friedman_test(survey_data, trust_government,
                                        trust_media, trust_science))
  expect_s3_class(pw$results, "data.frame")
  expect_identical(pw$results, pw$comparisons)
  expect_equal(nrow(pw$results), 3L)
})
