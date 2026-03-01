# =============================================================================
# SPSS Validation Tests for efa() - Exploratory Factor Analysis
# =============================================================================
# Tests validate against SPSS FACTOR procedure output
# SPSS Syntax: tests/spss_reference/syntax/efa.sps
# SPSS Output: tests/spss_reference/outputs/efa_output.txt
#
# Variables: political_orientation, environmental_concern, life_satisfaction,
#            trust_government, trust_media, trust_science
# Weight: sampling_weight
# Grouping: region (East/West)
#
# Test scenarios:
# 1a: Unweighted, ungrouped, Varimax, Kaiser criterion
# 1b: Unweighted, ungrouped, Oblimin, Kaiser criterion
# 1c: Unweighted, ungrouped, No rotation
# 1d: Unweighted, ungrouped, Varimax, 2 factors fixed
# 2a: Weighted, ungrouped, Varimax, Kaiser criterion
# 2b: Weighted, ungrouped, Oblimin, Kaiser criterion
# 2c: Weighted, ungrouped, Varimax, 2 factors fixed
# 3a: Unweighted, grouped by region, Varimax
# 3b: Unweighted, grouped by region, Oblimin
# 4a: Weighted, grouped by region, Varimax
# 4b: Weighted, grouped by region, Oblimin
# =============================================================================

library(testthat)
library(dplyr)

# Helper for absolute tolerance (SPSS displays 3 decimal places)
expect_spss3 <- function(actual, expected, abs_tol = 0.001) {
  diff <- abs(actual - expected)
  expect_true(
    diff <= abs_tol,
    label = sprintf("%.6f (expected %.3f, |diff| = %.6f, tol = %.3f)",
                    actual, expected, diff, abs_tol)
  )
}

# Helper for comparing loadings with sign ambiguity
# Compares absolute values column-by-column
expect_loading <- function(actual_mat, expected_vec, var_name, abs_tol = 0.002) {
  # expected_vec is a named vector like c(PC1 = 0.887, PC2 = NA, PC3 = NA)
  actual_row <- actual_mat[var_name, ]
  for (pc in names(expected_vec)) {
    if (!is.na(expected_vec[pc])) {
      expect_true(
        abs(abs(actual_row[pc]) - abs(expected_vec[pc])) <= abs_tol,
        label = sprintf("Loading %s on %s: |%.4f| vs expected |%.3f|",
                        var_name, pc, actual_row[pc], expected_vec[pc])
      )
    }
  }
}


# =============================================================================
# TEST 1a: UNWEIGHTED, UNGROUPED, VARIMAX, KAISER CRITERION
# =============================================================================

test_that("Test 1a: Unweighted, ungrouped, Varimax, Kaiser criterion", {
  data(survey_data, package = "mariposa")

  result <- efa(survey_data,
                political_orientation, environmental_concern, life_satisfaction,
                trust_government, trust_media, trust_science,
                rotation = "varimax")

  # KMO
  expect_spss3(result$kmo$overall, 0.505)

  # Bartlett's Test
  expect_spss3(result$bartlett$chi_sq, 932.068)
  expect_equal(result$bartlett$df, 15)
  expect_true(result$bartlett$p_value < 0.001)

  # Number of factors extracted
  expect_equal(result$n_factors, 3)

  # Eigenvalues (first 6)
  expect_spss3(result$eigenvalues[1], 1.600)
  expect_spss3(result$eigenvalues[2], 1.041)
  expect_spss3(result$eigenvalues[3], 1.017)
  expect_spss3(result$eigenvalues[4], 0.980)
  expect_spss3(result$eigenvalues[5], 0.949)
  expect_spss3(result$eigenvalues[6], 0.412)

  # Communalities
  expect_spss3(result$communalities["political_orientation"], 0.786)
  expect_spss3(result$communalities["environmental_concern"], 0.783)
  expect_spss3(result$communalities["life_satisfaction"], 0.668)
  expect_spss3(result$communalities["trust_government"], 0.347)
  expect_spss3(result$communalities["trust_media"], 0.475)
  expect_spss3(result$communalities["trust_science"], 0.598)

  # Rotated Component Matrix (Varimax) - check absolute loadings due to sign ambiguity
  # SPSS: pol=-.887 on PC1, env=.884 on PC1
  expect_loading(result$loadings, c(PC1 = 0.887), "political_orientation")
  expect_loading(result$loadings, c(PC1 = 0.884), "environmental_concern")
  # SPSS: sci=.762 on PC2, gov=.566 on PC2
  expect_loading(result$loadings, c(PC2 = 0.762), "trust_science")
  expect_loading(result$loadings, c(PC2 = 0.566), "trust_government")
  # SPSS: life=.789 on PC3, media=.620 on PC3
  expect_loading(result$loadings, c(PC3 = 0.789), "life_satisfaction")
  expect_loading(result$loadings, c(PC3 = 0.620), "trust_media")

  # Variance explained
  expect_equal(result$variance_explained$prc_variance[1], 26.666, tolerance = 0.01)
  expect_equal(result$variance_explained$prc_variance[2], 17.358, tolerance = 0.01)
  expect_equal(result$variance_explained$prc_variance[3], 16.955, tolerance = 0.01)
  expect_equal(result$variance_explained$cumulative_prc[3], 60.979, tolerance = 0.01)

  # Rotation sums of squared loadings
  expect_spss3(result$rotation_variance$ss_loading[1], 1.598)
  expect_spss3(result$rotation_variance$ss_loading[2], 1.039)
  expect_spss3(result$rotation_variance$ss_loading[3], 1.021)

  # Rotation metadata
  expect_equal(result$rotation, "varimax")
  expect_equal(result$extraction, "pca")
  expect_null(result$pattern_matrix)
  expect_null(result$structure_matrix)
  expect_null(result$factor_correlations)
})


# =============================================================================
# TEST 1b: UNWEIGHTED, UNGROUPED, OBLIMIN, KAISER CRITERION
# =============================================================================

test_that("Test 1b: Unweighted, ungrouped, Oblimin, Kaiser criterion", {
  skip_if_not_installed("GPArotation")
  data(survey_data, package = "mariposa")

  result <- efa(survey_data,
                political_orientation, environmental_concern, life_satisfaction,
                trust_government, trust_media, trust_science,
                rotation = "oblimin")

  # KMO and Bartlett same as 1a (same data, same correlation matrix)
  expect_spss3(result$kmo$overall, 0.505)
  expect_spss3(result$bartlett$chi_sq, 932.068)

  # Same eigenvalues and communalities as 1a
  expect_spss3(result$eigenvalues[1], 1.600)
  expect_equal(result$n_factors, 3)
  expect_spss3(result$communalities["political_orientation"], 0.786)

  # Pattern Matrix (Oblimin)
  expect_false(is.null(result$pattern_matrix))
  expect_loading(result$pattern_matrix, c(PC1 = 0.887), "political_orientation")
  expect_loading(result$pattern_matrix, c(PC1 = 0.884), "environmental_concern")
  expect_loading(result$pattern_matrix, c(PC2 = 0.769), "trust_science")
  expect_loading(result$pattern_matrix, c(PC2 = 0.561), "trust_government")
  expect_loading(result$pattern_matrix, c(PC3 = 0.797), "life_satisfaction")
  expect_loading(result$pattern_matrix, c(PC3 = 0.613), "trust_media")

  # Structure Matrix
  expect_false(is.null(result$structure_matrix))
  expect_loading(result$structure_matrix, c(PC1 = 0.886), "political_orientation")
  expect_loading(result$structure_matrix, c(PC1 = 0.884), "environmental_concern")
  expect_loading(result$structure_matrix, c(PC2 = 0.757), "trust_science")
  expect_loading(result$structure_matrix, c(PC2 = 0.571), "trust_government")
  expect_loading(result$structure_matrix, c(PC3 = 0.781), "life_satisfaction")
  expect_loading(result$structure_matrix, c(PC3 = 0.630), "trust_media")

  # Component Correlation Matrix
  expect_false(is.null(result$factor_correlations))
  # Off-diagonal correlations (absolute values due to sign ambiguity)
  expect_true(abs(abs(result$factor_correlations[1, 2]) - 0.041) < 0.01)
  expect_true(abs(abs(result$factor_correlations[1, 3]) - 0.024) < 0.01)
  expect_true(abs(abs(result$factor_correlations[2, 3]) - 0.063) < 0.01)

  # Rotation sums (oblimin: only SS loadings, no cumulative)
  # Wider tolerance for oblimin SS loadings (oblique rotation implementation differences)
  expect_spss3(result$rotation_variance$ss_loading[1], 1.599, abs_tol = 0.005)
  expect_spss3(result$rotation_variance$ss_loading[2], 1.041, abs_tol = 0.005)
  expect_spss3(result$rotation_variance$ss_loading[3], 1.022, abs_tol = 0.005)

  expect_equal(result$rotation, "oblimin")
})


# =============================================================================
# TEST 1c: UNWEIGHTED, UNGROUPED, NO ROTATION
# =============================================================================

test_that("Test 1c: Unweighted, ungrouped, No rotation, Kaiser criterion", {
  data(survey_data, package = "mariposa")

  result <- efa(survey_data,
                political_orientation, environmental_concern, life_satisfaction,
                trust_government, trust_media, trust_science,
                rotation = "none")

  # Same KMO, Bartlett, eigenvalues
  expect_spss3(result$kmo$overall, 0.505)
  expect_equal(result$n_factors, 3)

  # Unrotated Component Matrix (same as Component Matrix in SPSS)
  # SPSS: pol=-.885 on PC1, env=.885 on PC1
  expect_loading(result$unrotated_loadings, c(PC1 = 0.885), "political_orientation")
  expect_loading(result$unrotated_loadings, c(PC1 = 0.885), "environmental_concern")
  # SPSS: sci=.672 on PC2, gov=.547 on PC2
  expect_loading(result$unrotated_loadings, c(PC2 = 0.672), "trust_science")
  expect_loading(result$unrotated_loadings, c(PC2 = 0.547), "trust_government")
  # SPSS: life=.809 on PC3
  expect_loading(result$unrotated_loadings, c(PC3 = 0.809), "life_satisfaction")

  # loadings = unrotated for rotation = "none"
  expect_equal(result$loadings, result$unrotated_loadings)

  expect_equal(result$rotation, "none")
  expect_null(result$pattern_matrix)
  expect_null(result$structure_matrix)
})


# =============================================================================
# TEST 1d: UNWEIGHTED, UNGROUPED, VARIMAX, 2 FACTORS FIXED
# =============================================================================

test_that("Test 1d: Unweighted, ungrouped, Varimax, 2 factors fixed", {
  data(survey_data, package = "mariposa")

  result <- efa(survey_data,
                political_orientation, environmental_concern, life_satisfaction,
                trust_government, trust_media, trust_science,
                rotation = "varimax",
                n_factors = 2)

  expect_equal(result$n_factors, 2)

  # Eigenvalues same as before
  expect_spss3(result$eigenvalues[1], 1.600)
  expect_spss3(result$eigenvalues[2], 1.041)

  # Communalities (different with 2 factors)
  expect_spss3(result$communalities["political_orientation"], 0.786)
  expect_spss3(result$communalities["environmental_concern"], 0.783)
  expect_spss3(result$communalities["life_satisfaction"], 0.014)
  expect_spss3(result$communalities["trust_government"], 0.327)
  expect_spss3(result$communalities["trust_media"], 0.275)
  expect_spss3(result$communalities["trust_science"], 0.456)

  # Rotated loadings (2 components)
  expect_equal(ncol(result$loadings), 2)
  expect_loading(result$loadings, c(PC1 = 0.887), "political_orientation")
  expect_loading(result$loadings, c(PC1 = 0.885), "environmental_concern")
  expect_loading(result$loadings, c(PC2 = 0.669), "trust_science")
  expect_loading(result$loadings, c(PC2 = 0.552), "trust_government")
  expect_loading(result$loadings, c(PC2 = 0.524), "trust_media")
  # life_satisfaction has very low loading (< 0.40 blank)
})


# =============================================================================
# TEST 2a: WEIGHTED, UNGROUPED, VARIMAX, KAISER CRITERION
# =============================================================================

test_that("Test 2a: Weighted, ungrouped, Varimax, Kaiser criterion", {
  data(survey_data, package = "mariposa")

  result <- efa(survey_data,
                political_orientation, environmental_concern, life_satisfaction,
                trust_government, trust_media, trust_science,
                rotation = "varimax",
                weights = sampling_weight)

  # KMO
  expect_spss3(result$kmo$overall, 0.505)

  # Bartlett's Test
  expect_spss3(result$bartlett$chi_sq, 930.012)
  expect_equal(result$bartlett$df, 15)

  # Number of factors
  expect_equal(result$n_factors, 3)

  # Eigenvalues
  expect_spss3(result$eigenvalues[1], 1.599)
  expect_spss3(result$eigenvalues[2], 1.045)
  expect_spss3(result$eigenvalues[3], 1.018)

  # Communalities
  expect_spss3(result$communalities["political_orientation"], 0.784)
  expect_spss3(result$communalities["environmental_concern"], 0.782)
  expect_spss3(result$communalities["life_satisfaction"], 0.712)
  expect_spss3(result$communalities["trust_government"], 0.332)
  expect_spss3(result$communalities["trust_media"], 0.442)
  expect_spss3(result$communalities["trust_science"], 0.610)

  # Rotated loadings
  expect_loading(result$loadings, c(PC1 = 0.885), "political_orientation")
  expect_loading(result$loadings, c(PC1 = 0.883), "environmental_concern")
  expect_loading(result$loadings, c(PC2 = 0.752), "trust_science")
  expect_loading(result$loadings, c(PC2 = 0.541), "trust_government")
  expect_loading(result$loadings, c(PC3 = 0.828), "life_satisfaction")
  expect_loading(result$loadings, c(PC3 = 0.534), "trust_media")

  # Rotation sums
  expect_spss3(result$rotation_variance$ss_loading[1], 1.597)
  expect_spss3(result$rotation_variance$ss_loading[2], 1.044)
  expect_spss3(result$rotation_variance$ss_loading[3], 1.021)
})


# =============================================================================
# TEST 2b: WEIGHTED, UNGROUPED, OBLIMIN, KAISER CRITERION
# =============================================================================

test_that("Test 2b: Weighted, ungrouped, Oblimin, Kaiser criterion", {
  skip_if_not_installed("GPArotation")
  data(survey_data, package = "mariposa")

  result <- efa(survey_data,
                political_orientation, environmental_concern, life_satisfaction,
                trust_government, trust_media, trust_science,
                rotation = "oblimin",
                weights = sampling_weight)

  expect_spss3(result$kmo$overall, 0.505)
  expect_equal(result$n_factors, 3)

  # Pattern Matrix
  expect_loading(result$pattern_matrix, c(PC1 = 0.886), "political_orientation")
  expect_loading(result$pattern_matrix, c(PC1 = 0.883), "environmental_concern")
  expect_loading(result$pattern_matrix, c(PC2 = 0.764), "trust_science")
  expect_loading(result$pattern_matrix, c(PC2 = 0.534), "trust_government")
  expect_loading(result$pattern_matrix, c(PC3 = 0.837), "life_satisfaction")
  expect_loading(result$pattern_matrix, c(PC3 = 0.521), "trust_media")

  # Structure Matrix
  expect_loading(result$structure_matrix, c(PC1 = 0.885), "political_orientation")
  expect_loading(result$structure_matrix, c(PC1 = 0.883), "environmental_concern")
  expect_loading(result$structure_matrix, c(PC2 = 0.743), "trust_science")
  expect_loading(result$structure_matrix, c(PC2 = 0.548), "trust_government")
  expect_loading(result$structure_matrix, c(PC3 = 0.819), "life_satisfaction")

  # Component correlations
  expect_true(abs(abs(result$factor_correlations[1, 2]) - 0.035) < 0.01)
  expect_true(abs(abs(result$factor_correlations[1, 3]) - 0.040) < 0.01)
  expect_true(abs(abs(result$factor_correlations[2, 3]) - 0.083) < 0.01)
})


# =============================================================================
# TEST 2c: WEIGHTED, UNGROUPED, VARIMAX, 2 FACTORS FIXED
# =============================================================================

test_that("Test 2c: Weighted, ungrouped, Varimax, 2 factors fixed", {
  data(survey_data, package = "mariposa")

  result <- efa(survey_data,
                political_orientation, environmental_concern, life_satisfaction,
                trust_government, trust_media, trust_science,
                rotation = "varimax",
                n_factors = 2,
                weights = sampling_weight)

  expect_equal(result$n_factors, 2)

  # Communalities with 2 factors (weighted)
  expect_spss3(result$communalities["political_orientation"], 0.784)
  expect_spss3(result$communalities["environmental_concern"], 0.781)
  expect_spss3(result$communalities["life_satisfaction"], 0.010)
  expect_spss3(result$communalities["trust_government"], 0.330)
  expect_spss3(result$communalities["trust_media"], 0.294)
  expect_spss3(result$communalities["trust_science"], 0.445)

  # Rotated loadings
  expect_loading(result$loadings, c(PC1 = 0.885), "political_orientation")
  expect_loading(result$loadings, c(PC1 = 0.883), "environmental_concern")
  expect_loading(result$loadings, c(PC2 = 0.660), "trust_science")
  expect_loading(result$loadings, c(PC2 = 0.554), "trust_government")
  expect_loading(result$loadings, c(PC2 = 0.541), "trust_media")
})


# =============================================================================
# TEST 3a: UNWEIGHTED, GROUPED, VARIMAX
# =============================================================================

test_that("Test 3a: Unweighted, grouped by region, Varimax", {
  data(survey_data, package = "mariposa")

  result <- survey_data %>%
    group_by(region) %>%
    efa(political_orientation, environmental_concern, life_satisfaction,
        trust_government, trust_media, trust_science,
        rotation = "varimax")

  expect_true(result$is_grouped)
  expect_equal(length(result$groups), 2)

  # --- East group ---
  east <- result$groups[[1]]

  # KMO and Bartlett
  expect_spss3(east$kmo$overall, 0.475)
  expect_spss3(east$bartlett$chi_sq, 202.026)
  expect_equal(east$bartlett$df, 15)

  # Eigenvalues
  expect_spss3(east$eigenvalues[1], 1.607)
  expect_spss3(east$eigenvalues[2], 1.115)
  expect_spss3(east$eigenvalues[3], 1.007)

  expect_equal(east$n_factors, 3)

  # Communalities
  expect_spss3(east$communalities["political_orientation"], 0.803)
  expect_spss3(east$communalities["environmental_concern"], 0.801)
  expect_spss3(east$communalities["life_satisfaction"], 0.324)
  expect_spss3(east$communalities["trust_government"], 0.939)
  expect_spss3(east$communalities["trust_media"], 0.473)
  expect_spss3(east$communalities["trust_science"], 0.388)

  # Rotated loadings - East (note: sign ambiguity, compare absolute values)
  # SPSS: pol=.895 PC1, env=-.894 PC1
  expect_loading(east$loadings, c(PC1 = 0.895), "political_orientation")
  expect_loading(east$loadings, c(PC1 = 0.894), "environmental_concern")
  # SPSS: media=.673 PC2, sci=.584 PC2, life=-.567 PC2
  expect_loading(east$loadings, c(PC2 = 0.673), "trust_media")
  expect_loading(east$loadings, c(PC2 = 0.584), "trust_science")
  expect_loading(east$loadings, c(PC2 = 0.567), "life_satisfaction")
  # SPSS: gov=.969 PC3
  expect_loading(east$loadings, c(PC3 = 0.969), "trust_government")

  # --- West group ---
  west <- result$groups[[2]]

  expect_spss3(west$kmo$overall, 0.505)
  expect_spss3(west$bartlett$chi_sq, 748.803)

  expect_equal(west$n_factors, 3)

  # Eigenvalues
  expect_spss3(west$eigenvalues[1], 1.601)
  expect_spss3(west$eigenvalues[2], 1.050)
  expect_spss3(west$eigenvalues[3], 1.026)

  # Communalities
  expect_spss3(west$communalities["political_orientation"], 0.786)
  expect_spss3(west$communalities["environmental_concern"], 0.777)
  expect_spss3(west$communalities["life_satisfaction"], 0.554)
  expect_spss3(west$communalities["trust_government"], 0.428)
  expect_spss3(west$communalities["trust_media"], 0.499)
  expect_spss3(west$communalities["trust_science"], 0.633)

  # Rotated loadings - West
  expect_loading(west$loadings, c(PC1 = 0.886), "political_orientation")
  expect_loading(west$loadings, c(PC1 = 0.880), "environmental_concern")
  expect_loading(west$loadings, c(PC2 = 0.737), "life_satisfaction")
  expect_loading(west$loadings, c(PC2 = 0.694), "trust_media")
  expect_loading(west$loadings, c(PC3 = 0.782), "trust_science")
  expect_loading(west$loadings, c(PC3 = 0.629), "trust_government")
})


# =============================================================================
# TEST 3b: UNWEIGHTED, GROUPED, OBLIMIN
# =============================================================================

test_that("Test 3b: Unweighted, grouped by region, Oblimin", {
  skip_if_not_installed("GPArotation")
  data(survey_data, package = "mariposa")

  result <- survey_data %>%
    group_by(region) %>%
    efa(political_orientation, environmental_concern, life_satisfaction,
        trust_government, trust_media, trust_science,
        rotation = "oblimin")

  expect_true(result$is_grouped)

  # --- East group ---
  east <- result$groups[[1]]

  # Pattern Matrix
  expect_loading(east$pattern_matrix, c(PC1 = 0.894), "political_orientation")
  expect_loading(east$pattern_matrix, c(PC1 = 0.894), "environmental_concern")
  expect_loading(east$pattern_matrix, c(PC2 = 0.675), "trust_media")
  expect_loading(east$pattern_matrix, c(PC2 = 0.581), "trust_science")
  expect_loading(east$pattern_matrix, c(PC2 = 0.568), "life_satisfaction")
  expect_loading(east$pattern_matrix, c(PC3 = 0.969), "trust_government")

  # Component Correlations - East
  expect_true(abs(abs(east$factor_correlations[1, 2]) - 0.027) < 0.02)
  expect_true(abs(abs(east$factor_correlations[1, 3]) - 0.005) < 0.02)
  expect_true(abs(abs(east$factor_correlations[2, 3]) - 0.022) < 0.02)

  # --- West group ---
  west <- result$groups[[2]]

  # Pattern Matrix
  expect_loading(west$pattern_matrix, c(PC1 = 0.887), "political_orientation")
  expect_loading(west$pattern_matrix, c(PC1 = 0.881), "environmental_concern")
  expect_loading(west$pattern_matrix, c(PC2 = 0.740), "life_satisfaction")
  expect_loading(west$pattern_matrix, c(PC2 = 0.693), "trust_media")
  expect_loading(west$pattern_matrix, c(PC3 = 0.788), "trust_science")
  expect_loading(west$pattern_matrix, c(PC3 = 0.624), "trust_government")

  # Component Correlations - West
  expect_true(abs(abs(west$factor_correlations[1, 2]) - 0.033) < 0.02)
  expect_true(abs(abs(west$factor_correlations[1, 3]) - 0.052) < 0.02)
  expect_true(abs(abs(west$factor_correlations[2, 3]) - 0.035) < 0.02)
})


# =============================================================================
# TEST 4a: WEIGHTED, GROUPED, VARIMAX
# =============================================================================

test_that("Test 4a: Weighted, grouped by region, Varimax", {
  data(survey_data, package = "mariposa")

  result <- survey_data %>%
    group_by(region) %>%
    efa(political_orientation, environmental_concern, life_satisfaction,
        trust_government, trust_media, trust_science,
        rotation = "varimax",
        weights = sampling_weight)

  expect_true(result$is_grouped)

  # --- East group ---
  east <- result$groups[[1]]

  expect_spss3(east$kmo$overall, 0.475)
  expect_spss3(east$bartlett$chi_sq, 213.039)
  expect_equal(east$n_factors, 3)

  # Eigenvalues
  expect_spss3(east$eigenvalues[1], 1.608)
  expect_spss3(east$eigenvalues[2], 1.128)
  expect_spss3(east$eigenvalues[3], 1.008)

  # Communalities
  expect_spss3(east$communalities["political_orientation"], 0.803)
  expect_spss3(east$communalities["environmental_concern"], 0.801)
  expect_spss3(east$communalities["life_satisfaction"], 0.316)
  expect_spss3(east$communalities["trust_government"], 0.928)
  expect_spss3(east$communalities["trust_media"], 0.475)
  expect_spss3(east$communalities["trust_science"], 0.419)

  # Rotated loadings
  expect_loading(east$loadings, c(PC1 = 0.894), "political_orientation")
  expect_loading(east$loadings, c(PC1 = 0.894), "environmental_concern")
  expect_loading(east$loadings, c(PC2 = 0.671), "trust_media")
  expect_loading(east$loadings, c(PC2 = 0.601), "trust_science")
  expect_loading(east$loadings, c(PC2 = 0.561), "life_satisfaction")
  expect_loading(east$loadings, c(PC3 = 0.963), "trust_government")

  # --- West group ---
  west <- result$groups[[2]]

  expect_spss3(west$kmo$overall, 0.505)
  expect_spss3(west$bartlett$chi_sq, 737.388)
  expect_equal(west$n_factors, 3)

  # Eigenvalues
  expect_spss3(west$eigenvalues[1], 1.600)
  expect_spss3(west$eigenvalues[2], 1.052)
  expect_spss3(west$eigenvalues[3], 1.023)

  # Communalities
  expect_spss3(west$communalities["political_orientation"], 0.783)
  expect_spss3(west$communalities["environmental_concern"], 0.775)
  expect_spss3(west$communalities["life_satisfaction"], 0.560)
  expect_spss3(west$communalities["trust_government"], 0.413)
  expect_spss3(west$communalities["trust_media"], 0.474)
  expect_spss3(west$communalities["trust_science"], 0.669)

  # Rotated loadings
  expect_loading(west$loadings, c(PC1 = 0.885), "political_orientation")
  expect_loading(west$loadings, c(PC1 = 0.879), "environmental_concern")
  expect_loading(west$loadings, c(PC2 = 0.741), "life_satisfaction")
  expect_loading(west$loadings, c(PC2 = 0.673), "trust_media")
  expect_loading(west$loadings, c(PC3 = 0.799), "trust_science")
  expect_loading(west$loadings, c(PC3 = 0.602), "trust_government")
})


# =============================================================================
# TEST 4b: WEIGHTED, GROUPED, OBLIMIN
# =============================================================================

test_that("Test 4b: Weighted, grouped by region, Oblimin", {
  skip_if_not_installed("GPArotation")
  data(survey_data, package = "mariposa")

  result <- survey_data %>%
    group_by(region) %>%
    efa(political_orientation, environmental_concern, life_satisfaction,
        trust_government, trust_media, trust_science,
        rotation = "oblimin",
        weights = sampling_weight)

  expect_true(result$is_grouped)

  # --- East group ---
  east <- result$groups[[1]]

  expect_spss3(east$kmo$overall, 0.475)
  expect_equal(east$n_factors, 3)

  # Pattern Matrix
  expect_loading(east$pattern_matrix, c(PC1 = 0.894), "political_orientation")
  expect_loading(east$pattern_matrix, c(PC1 = 0.894), "environmental_concern")
  expect_loading(east$pattern_matrix, c(PC2 = 0.673), "trust_media")
  expect_loading(east$pattern_matrix, c(PC2 = 0.598), "trust_science")
  expect_loading(east$pattern_matrix, c(PC2 = 0.562), "life_satisfaction")
  expect_loading(east$pattern_matrix, c(PC3 = 0.964), "trust_government")

  # Component Correlations
  expect_true(abs(abs(east$factor_correlations[1, 2]) - 0.029) < 0.02)
  expect_true(abs(abs(east$factor_correlations[1, 3]) - 0.012) < 0.02)
  expect_true(abs(abs(east$factor_correlations[2, 3]) - 0.027) < 0.02)

  # --- West group ---
  west <- result$groups[[2]]

  expect_spss3(west$kmo$overall, 0.505)
  expect_equal(west$n_factors, 3)

  # Pattern Matrix
  expect_loading(west$pattern_matrix, c(PC1 = 0.886), "political_orientation")
  expect_loading(west$pattern_matrix, c(PC1 = 0.880), "environmental_concern")
  expect_loading(west$pattern_matrix, c(PC2 = 0.746), "life_satisfaction")
  expect_loading(west$pattern_matrix, c(PC2 = 0.671), "trust_media")
  expect_loading(west$pattern_matrix, c(PC3 = 0.808), "trust_science")
  expect_loading(west$pattern_matrix, c(PC3 = 0.594), "trust_government")

  # Component Correlations
  expect_true(abs(abs(west$factor_correlations[1, 2]) - 0.049) < 0.02)
  expect_true(abs(abs(west$factor_correlations[1, 3]) - 0.056) < 0.02)
  expect_true(abs(abs(west$factor_correlations[2, 3]) - 0.056) < 0.02)
})


# =============================================================================
# STRUCTURAL TESTS
# =============================================================================

test_that("efa() returns correct class and structure", {
  data(survey_data, package = "mariposa")

  result <- efa(survey_data,
                political_orientation, environmental_concern, life_satisfaction,
                trust_government, trust_media, trust_science)

  expect_s3_class(result, "efa")
  expect_true("loadings" %in% names(result))
  expect_true("unrotated_loadings" %in% names(result))
  expect_true("eigenvalues" %in% names(result))
  expect_true("variance_explained" %in% names(result))
  expect_true("communalities" %in% names(result))
  expect_true("kmo" %in% names(result))
  expect_true("bartlett" %in% names(result))
  expect_true("rotation" %in% names(result))
  expect_true("correlation_matrix" %in% names(result))
})

test_that("efa() validates inputs correctly", {
  data(survey_data, package = "mariposa")

  # Non-numeric variable
  expect_error(
    efa(survey_data, region),
    "not numeric"
  )

  # Too few variables
  expect_error(
    efa(survey_data, political_orientation),
    "at least 2"
  )

  # Invalid rotation
  expect_error(
    efa(survey_data, political_orientation, environmental_concern,
        rotation = "invalid"),
    "arg"
  )

  # Invalid n_factors
  expect_error(
    efa(survey_data, political_orientation, environmental_concern,
        n_factors = 10),
    "n_factors"
  )
})

test_that("efa() print method works without error", {
  data(survey_data, package = "mariposa")

  result <- efa(survey_data,
                political_orientation, environmental_concern, life_satisfaction,
                trust_government, trust_media, trust_science)

  expect_output(print(result), "Exploratory Factor Analysis")
  expect_output(print(result), "KMO")
  expect_output(print(result), "Bartlett")
  expect_output(print(result), "Communalities")
  expect_output(print(result), "Rotated Component Matrix")
})

test_that("efa() print method works for oblimin", {
  skip_if_not_installed("GPArotation")
  data(survey_data, package = "mariposa")

  result <- efa(survey_data,
                political_orientation, environmental_concern, life_satisfaction,
                trust_government, trust_media, trust_science,
                rotation = "oblimin")

  expect_output(print(result), "Pattern Matrix")
  expect_output(print(result), "Structure Matrix")
  expect_output(print(result), "Component Correlation Matrix")
})

test_that("efa() print method works for grouped data", {
  data(survey_data, package = "mariposa")

  result <- survey_data %>%
    group_by(region) %>%
    efa(political_orientation, environmental_concern, life_satisfaction,
        trust_government, trust_media, trust_science)

  expect_output(print(result), "Group:")
})
