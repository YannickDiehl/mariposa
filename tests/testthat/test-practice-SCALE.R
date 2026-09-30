# =============================================================================
# Practice-test regressions: scale analysis (reliability(), efa())
# =============================================================================
# Each test pins a defect found in the 2026-09 practice test (ALLBUS 2023
# field report, batch SCALE) to a minimal scenario on survey_data or small
# synthetic data. R-internal consistency checks (Tier 4); the SPSS parity
# of the EFA sign convention is asserted in test-efa-spss-validation.R.
# =============================================================================

library(testthat)
library(dplyr)

data(survey_data, envir = environment())

# --- SCALE-01: component/factor signs ----------------------------------------

test_that("SCALE-01: efa() reflects components to a positive loading sum", {
  # eigen() returns eigenvectors with an arbitrary sign: three positively
  # correlated trust items came out with all-negative loadings
  # (-0.597/-0.475/-0.678). SPSS reflects every extracted column so that
  # its loadings sum to a positive value.
  e <- efa(survey_data, trust_government, trust_media, trust_science,
           n_factors = 1)
  expect_true(all(e$unrotated_loadings > 0))
  expect_true(all(e$loadings > 0))

  for (rot in c("none", "varimax", "promax")) {
    e <- efa(survey_data, political_orientation, environmental_concern,
             life_satisfaction, trust_government, trust_media, trust_science,
             rotation = rot)
    expect_true(all(colSums(e$unrotated_loadings) > 0), label = rot)
  }
})

test_that("SCALE-01: oblique solutions stay consistent after reflection", {
  e <- efa(survey_data, political_orientation, environmental_concern,
           life_satisfaction, trust_government, trust_media, trust_science,
           rotation = "promax")
  expect_true(all(colSums(e$unrotated_loadings) > 0))
  # Structure = Pattern %*% Phi must still hold for the reflected solution
  expect_equal(unname(e$structure_matrix),
               unname(e$pattern_matrix %*% e$factor_correlations),
               tolerance = 1e-10)
})

test_that("SCALE-01: every group of a grouped efa() uses the same rule", {
  g <- efa(group_by(survey_data, region), trust_government, trust_media,
           trust_science, n_factors = 1)
  for (res in g$groups) {
    expect_true(all(colSums(res$unrotated_loadings) > 0))
  }
})
