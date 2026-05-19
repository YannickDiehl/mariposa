# =============================================================================
# reliability — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::reliability() (Cronbach's alpha) against SPSS v29
#          RELIABILITY procedure.
# Reference: reliability_output.txt
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(
  # Test 1a: trust_government, trust_media, trust_science — unweighted
  test_1a_trust = list(
    alpha = 0.047, alpha_std = 0.048, n_items = 3L, n = 2135L,
    item_means = c(trust_government = 2.62, trust_media = 2.43, trust_science = 3.62),
    item_sds   = c(trust_government = 1.162, trust_media = 1.156, trust_science = 1.034)
  ),
  # Test 2a: same items, weighted (SPSS honors weights here)
  test_2a_trust_weighted = list(
    alpha = 0.052, alpha_std = 0.053, n_items = 3L, n_weighted = 2150L,
    item_means = c(trust_government = 2.62, trust_media = 2.43, trust_science = 3.62),
    item_sds   = c(trust_government = 1.162, trust_media = 1.158, trust_science = 1.033)
  )
)


data(survey_data, envir = environment())


compare_alpha <- function(r, spss, scenario, is_weighted = FALSE) {
  assert_spss(as.numeric(r$alpha), spss$alpha,
              tier = "display", precision = 3,
              label = sprintf("[%s] Cronbach's alpha", scenario))
  if (!is.null(spss$alpha_std)) {
    assert_spss(as.numeric(r$alpha_standardized), spss$alpha_std,
                tier = "display", precision = 3,
                label = sprintf("[%s] alpha standardized", scenario))
  }
  expect_equal(r$n_items, spss$n_items, label = sprintf("[%s] n_items", scenario))
  if (!is.null(spss$n)) {
    assert_spss_count(r$n, spss$n,
                      label = sprintf("[%s] N", scenario))
  }
  if (!is.null(spss$n_weighted)) {
    assert_spss(as.numeric(r$weighted_n), spss$n_weighted,
                tier = "display", precision = 0,
                label = sprintf("[%s] weighted N", scenario))
  }
  # Item means and SDs
  for (var in names(spss$item_means)) {
    actual_mean <- r$item_statistics$mean[r$item_statistics$item == var]
    assert_spss(actual_mean, spss$item_means[[var]],
                tier = "display", precision = 2,
                label = sprintf("[%s | %s] item mean", scenario, var))
    actual_sd <- r$item_statistics$sd[r$item_statistics$item == var]
    assert_spss(actual_sd, spss$item_sds[[var]],
                tier = "display", precision = 3,
                label = sprintf("[%s | %s] item SD", scenario, var))
  }
}


test_that("Test 1a: reliability trust scale unweighted — matches SPSS", {
  r <- survey_data |>
    reliability(trust_government, trust_media, trust_science)
  compare_alpha(r, spss_values$test_1a_trust, "1a: trust scale")
})

test_that("Test 2a: reliability trust scale weighted — matches SPSS", {
  r <- survey_data |>
    reliability(trust_government, trust_media, trust_science,
                weights = sampling_weight)
  compare_alpha(r, spss_values$test_2a_trust_weighted, "2a: weighted trust",
                is_weighted = TRUE)
})
