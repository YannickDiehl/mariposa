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

# --- SCALE-02: ML variance explained -----------------------------------------

test_that("SCALE-02: efa() reports extraction sums of squared loadings", {
  # extraction = "ml" printed the PCA eigenvalue share ("61.0 %") although
  # the ML factors explain ~24 %; SPSS's "Extraction Sums of Squared
  # Loadings" were missing entirely, and the compact line said "components".
  e <- efa(survey_data, political_orientation, environmental_concern,
           life_satisfaction, trust_government, trust_media, trust_science,
           extraction = "ml")
  ss <- unname(colSums(e$unrotated_loadings^2))
  expect_equal(e$extraction_variance$ss_loading, ss)
  expect_equal(e$extraction_variance$prc_variance, ss / 6 * 100)
  expect_equal(e$extraction_variance$cumulative_prc, cumsum(ss / 6 * 100))

  out <- capture.output(print(e))
  pct <- format(round(sum(ss) / 6 * 100, 1), nsmall = 1)
  expect_true(any(grepl(paste0(pct, "%"), out, fixed = TRUE)))
  expect_false(any(grepl("61.0%", out, fixed = TRUE)))
  expect_true(any(grepl("3 factors (ML", out, fixed = TRUE)))
  expect_false(any(grepl("component", out)))

  s <- capture.output(print(summary(e)))
  expect_true(any(grepl("Extraction Sums", s, fixed = TRUE)))
  expect_true(any(grepl("Rotation Sums", s, fixed = TRUE)))
})

test_that("SCALE-02: PCA extraction sums equal the retained eigenvalues", {
  e <- efa(survey_data, political_orientation, environmental_concern,
           life_satisfaction, trust_government, trust_media, trust_science)
  expect_equal(e$extraction_variance$ss_loading, e$eigenvalues[1:3])
  out <- capture.output(print(e))
  expect_true(any(grepl("3 components (PCA", out, fixed = TRUE)))
  expect_true(any(grepl("61.0%", out, fixed = TRUE)))
})

test_that("SCALE-17: Total Variance Explained is an aligned table", {
  # The old free-text lines shifted their columns at PC10 and listed
  # Factor1..Factor15 for 3 extracted factors.
  set.seed(1)
  f <- matrix(rnorm(300 * 2), 300)
  items <- as.data.frame(f %*% matrix(runif(2 * 11, 0.3, 0.8), 2) +
                           matrix(rnorm(300 * 11), 300))
  e <- efa(items, everything(), n_factors = 2)
  s <- capture.output(print(summary(e, kmo_bartlett = FALSE,
                                    communalities = FALSE,
                                    unrotated_matrix = FALSE,
                                    rotated_matrix = FALSE)))
  start <- grep("Total Variance Explained", s, fixed = TRUE)
  rows <- grep("^ +[0-9]+ ", s[start:length(s)], value = TRUE)
  expect_length(rows, 11)
  # The first number of every row (the eigenvalue) ends in the same
  # column, including rows 10 and 11
  first_num_end <- vapply(rows, function(r) {
    m <- gregexpr("[0-9]+\\.[0-9]+", r)[[1]]
    as.integer(m[1] + attr(m, "match.length")[1])
  }, integer(1))
  expect_length(unique(first_num_end), 1)
})
