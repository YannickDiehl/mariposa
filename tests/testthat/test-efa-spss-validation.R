# =============================================================================
# efa — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::efa() against SPSS v29 FACTOR.
# Reference: efa_output.txt
#
# Validates KMO, Bartlett's, eigenvalues, variance explained, communalities
# (PCA, efa_output.txt), the varimax / oblimin / promax rotations
# (efa_output.txt, efa_ml_promax_output.txt Tests P1-P4) and the ML
# extraction (efa_ml_promax_output.txt Tests 5a-8a).
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(
  test_1a = list(
    # 6 variables: political_orientation, environmental_concern,
    # life_satisfaction, trust_government, trust_media, trust_science
    kmo = 0.505,                              # efa_output.txt:92
    bartlett_chi_sq = 932.068,                # efa_output.txt:93
    bartlett_df = 15L,                        # efa_output.txt:94
    bartlett_p = "<.001",                     # efa_output.txt:95
    eigenvalues = c(1.600, 1.041, 1.017, 0.980, 0.949, 0.412),  # lines 112-117
    var_explained = c(26.666, 17.358, 16.955, 16.334, 15.814, 6.873),
    cumulative = c(26.666, 44.024, 60.979, 77.313, 93.127, 100.000),
    # Extraction Sums of Squared Loadings (lines 112-114, middle block)
    extraction_ss = c(1.600, 1.041, 1.017),
    extraction_pct = c(26.666, 17.358, 16.955),
    extraction_cum = c(26.666, 44.024, 60.979),
    communalities = c(political_orientation = 0.786,                # lines 100-105
                       environmental_concern = 0.783,
                       life_satisfaction     = 0.668,
                       trust_government      = 0.347,
                       trust_media           = 0.475,
                       trust_science         = 0.598)
  )
)


data(survey_data, envir = environment())


test_that("Test 1a: EFA 6-variable PCA/Varimax — matches SPSS", {
  r <- efa(survey_data,
            political_orientation, environmental_concern, life_satisfaction,
            trust_government, trust_media, trust_science)
  spss <- spss_values$test_1a

  # KMO
  assert_spss(as.numeric(r$kmo$overall), spss$kmo,
              tier = "display", precision = 3,
              label = "[1a] KMO overall")

  # Bartlett's test
  assert_spss(as.numeric(r$bartlett$chi_sq), spss$bartlett_chi_sq,
              tier = "display", precision = 3,
              label = "[1a] Bartlett chi²")
  assert_spss_count(as.numeric(r$bartlett$df), spss$bartlett_df,
                    label = "[1a] Bartlett df")
  assert_spss(as.numeric(r$bartlett$p_value), spss$bartlett_p,
              tier = "display", precision = 3, what = "p_value",
              label = "[1a] Bartlett p")

  # Eigenvalues + variance explained
  for (i in seq_along(spss$eigenvalues)) {
    assert_spss(r$variance_explained$eigenvalue[i], spss$eigenvalues[i],
                tier = "display", precision = 3,
                label = sprintf("[1a] component %d eigenvalue", i))
    assert_spss(r$variance_explained$prc_variance[i], spss$var_explained[i],
                tier = "display", precision = 3,
                label = sprintf("[1a] component %d %% variance", i))
    assert_spss(r$variance_explained$cumulative_prc[i], spss$cumulative[i],
                tier = "display", precision = 3,
                label = sprintf("[1a] component %d cumulative %%", i))
  }

  # Extraction Sums of Squared Loadings
  for (i in seq_along(spss$extraction_ss)) {
    assert_spss(r$extraction_variance$ss_loading[i], spss$extraction_ss[i],
                tier = "display", precision = 3,
                label = sprintf("[1a] component %d extraction SS", i))
    assert_spss(r$extraction_variance$prc_variance[i], spss$extraction_pct[i],
                tier = "display", precision = 3,
                label = sprintf("[1a] component %d extraction %% variance", i))
    assert_spss(r$extraction_variance$cumulative_prc[i], spss$extraction_cum[i],
                tier = "display", precision = 3,
                label = sprintf("[1a] component %d extraction cumulative %%", i))
  }

  # Communalities (named numeric vector keyed by variable)
  for (var in names(spss$communalities)) {
    actual <- r$communalities[[var]]
    assert_spss(as.numeric(actual), spss$communalities[[var]],
                tier = "display", precision = 3,
                label = sprintf("[1a] communality (%s)", var))
  }
})


# =============================================================================
# SIGN CONVENTION (practice test SCALE-01)
# =============================================================================
# eigen() returns eigenvectors with an arbitrary sign. SPSS FACTOR reflects
# each extracted column to a positive loading sum; rotated matrices and
# factor correlations follow from the reflected solution. The signed SPSS
# cells below (all loadings SPSS prints at BLANK(.40)) pin that convention.
# PCA loadings are exact eigen results -> Display tier on the signed value.
# The rotated solutions (varimax, oblimin, promax) are asserted in full in
# the sections below.

spss_cells <- function(...) {
  x <- matrix(c(...), ncol = 3, byrow = TRUE)
  data.frame(var = x[, 1], comp = as.integer(x[, 2]),
             value = as.numeric(x[, 3]), stringsAsFactors = FALSE)
}

spss_signs <- list(
  # efa_output.txt:120-128 (Test 1a, Component Matrix)
  t1a_component = spss_cells(
    "political_orientation", 1, -0.885, "environmental_concern", 1,  0.885,
    "trust_science",         2,  0.672, "trust_government",      2,  0.547,
    "trust_media",           2,  0.524, "trust_media",           3,  0.448,
    "life_satisfaction",     3,  0.809),
  # efa_output.txt:133-141 (Test 1a, Rotated Component Matrix, Varimax)
  t1a_rotated = spss_cells(
    "political_orientation", 1, -0.887, "environmental_concern", 1,  0.884,
    "trust_science",         2,  0.762, "trust_government",      2,  0.566,
    "life_satisfaction",     3,  0.789, "trust_media",           3,  0.620),
  # efa_output.txt:1214-1222 (Test 3a, region = East, Component Matrix):
  # the reflection is per solution - East shows political_orientation
  # positive on component 1, West (below) negative.
  t3a_east_component = spss_cells(
    "political_orientation", 1,  0.894, "environmental_concern", 1, -0.889,
    "trust_media",           2,  0.664, "trust_science",         2,  0.585,
    "life_satisfaction",     2, -0.566, "trust_government",      3,  0.968),
  # efa_output.txt:1348-1356 (Test 3a, region = West, Component Matrix)
  t3a_west_component = spss_cells(
    "political_orientation", 1, -0.884, "environmental_concern", 1,  0.880,
    "trust_media",           2,  0.641, "life_satisfaction",     2,  0.542,
    "life_satisfaction",     3, -0.510, "trust_science",         3,  0.689,
    "trust_government",      2,  0.432, "trust_government",      3,  0.449)
)

assert_loading_values <- function(mat, cells, tag) {
  for (i in seq_len(nrow(cells))) {
    assert_spss(mat[cells$var[i], cells$comp[i]], cells$value[i],
                tier = "display", precision = 3,
                label = sprintf("[%s] %s on component %d", tag,
                                cells$var[i], cells$comp[i]))
  }
}

six_items <- function(d, ...) {
  efa(d, political_orientation, environmental_concern, life_satisfaction,
      trust_government, trust_media, trust_science, ...)
}

test_that("Signs 1a: PCA component and varimax matrices match SPSS signs", {
  r <- six_items(survey_data)
  assert_loading_values(r$unrotated_loadings, spss_signs$t1a_component,
                        "1a component matrix")
  assert_loading_values(r$loadings, spss_signs$t1a_rotated,
                        "1a rotated component matrix")
})

test_that("Signs 3a: each region's component matrix matches SPSS signs", {
  r <- six_items(dplyr::group_by(survey_data, region))
  regions <- vapply(r$groups, function(g) as.character(g$group_values$region),
                    character(1))
  assert_loading_values(r$groups[[which(regions == "East")]]$unrotated_loadings,
                        spss_signs$t3a_east_component, "3a East component matrix")
  assert_loading_values(r$groups[[which(regions == "West")]]$unrotated_loadings,
                        spss_signs$t3a_west_component, "3a West component matrix")
})

# =============================================================================
# VARIMAX (efa_output.txt, Tests 1a, 1d, 2a, 2c, 3a, 4a)
# =============================================================================
# SPSS rotates pairs of factors cyclically (Kaiser's algorithm, IBM SPSS
# Statistics Algorithms, FACTOR "Orthogonal Rotations") and stops when the
# varimax criterion improves by at most 1e-5. SPSS prints the complete
# Component Transformation Matrix (T: rotated = unrotated %*% T, no BLANK),
# so T is asserted cell by cell, together with the Rotation Sums of Squared
# Loadings.

vm_T <- function(...) matrix(c(...), nrow = floor(sqrt(length(c(...)))), byrow = TRUE)

spss_varimax <- list(
  # efa_output.txt:108-114, 147-152 (Test 1a, unweighted)
  `1a` = list(weighted = FALSE, region = NULL, n_factors = NULL,
    iterations = 4L,  # efa_output.txt:145 "Rotation converged in 4 iterations"
    T = vm_T(.998, .057, .007, -.055, .915, .400, .017, -.399, .917),
    ss = c(1.598, 1.039, 1.021), pct = c(26.635, 17.324, 17.020),
    cum = c(26.635, 43.959, 60.979)),
  # efa_output.txt:564-569, 589-607 (Test 1d, unweighted, FACTORS(2))
  `1d` = list(weighted = FALSE, region = NULL, n_factors = 2,
    iterations = 3L,  # efa_output.txt:601 "Rotation converged in 3 iterations"
    T = vm_T(.999, .034, -.034, .999),
    ss = c(1.599, 1.042), pct = c(26.655, 17.369), cum = c(26.655, 44.024),
    rotated = spss_cells(
      "political_orientation", 1, -0.887, "environmental_concern", 1, 0.885,
      "trust_science",         2,  0.669, "trust_government",      2, 0.552,
      "trust_media",           2,  0.524)),
  # efa_output.txt:718-724, 757-762 (Test 2a, weighted)
  `2a` = list(weighted = TRUE, region = NULL, n_factors = NULL,
    iterations = 4L,  # efa_output.txt:755 "Rotation converged in 4 iterations"
    T = vm_T(.998, .054, .016, -.056, .949, .310, .002, -.311, .950),
    ss = c(1.597, 1.044, 1.021), pct = c(26.615, 17.396, 17.010),
    cum = c(26.615, 44.011, 61.021)),
  # efa_output.txt:1042-1047, 1081-1085 (Test 2c, weighted, FACTORS(2))
  `2c` = list(weighted = TRUE, region = NULL, n_factors = 2,
    iterations = 3L,  # efa_output.txt:1079 "Rotation converged in 3 iterations"
    T = vm_T(.999, .043, -.043, .999),
    ss = c(1.598, 1.046), pct = c(26.627, 17.429), cum = c(26.627, 44.056)),
  # efa_output.txt:1201-1207, 1228-1248 (Test 3a, unweighted, region = East)
  `3a_East` = list(weighted = FALSE, region = "East", n_factors = NULL,
    iterations = 3L,  # efa_output.txt:1241 "Rotation converged in 3 iterations"
    T = vm_T(.996, .086, -.037, -.085, .996, .030, .039, -.027, .999),
    ss = c(1.603, 1.118, 1.008), pct = c(26.712, 18.641, 16.794),
    cum = c(26.712, 45.353, 62.147),
    rotated = spss_cells(
      "political_orientation", 1,  0.895, "environmental_concern", 1, -0.894,
      "trust_media",           2,  0.673, "trust_science",         2,  0.584,
      "life_satisfaction",     2, -0.567, "trust_government",      3,  0.969)),
  # efa_output.txt:1335-1341, 1362-1382 (Test 3a, unweighted, region = West)
  `3a_West` = list(weighted = FALSE, region = "West", n_factors = NULL,
    iterations = 4L,  # efa_output.txt:1375 "Rotation converged in 4 iterations"
    T = vm_T(.997, .021, .074, -.060, .816, .575, -.048, -.578, .814),
    ss = c(1.598, 1.042, 1.037), pct = c(26.629, 17.371, 17.288),
    cum = c(26.629, 44.000, 61.288),
    rotated = spss_cells(
      "political_orientation", 1, -0.886, "environmental_concern", 1, 0.880,
      "life_satisfaction",     2,  0.737, "trust_media",           2, 0.694,
      "trust_science",         3,  0.782, "trust_government",      3, 0.629)),
  # efa_output.txt:1827-1833, 1869-1874 (Test 4a, weighted, region = East)
  `4a_East` = list(weighted = TRUE, region = "East", n_factors = NULL,
    iterations = 4L,  # efa_output.txt:1867 "Rotation converged in 4 iterations"
    T = vm_T(.995, .096, -.041, -.089, .985, .145, .054, -.141, .989),
    ss = c(1.602, 1.130, 1.011), pct = c(26.699, 18.832, 16.851),
    cum = c(26.699, 45.530, 62.382)),
  # efa_output.txt:1961-1967, 2003-2008 (Test 4a, weighted, region = West)
  `4a_West` = list(weighted = TRUE, region = "West", n_factors = NULL,
    iterations = 4L,  # efa_output.txt:2001 "Rotation converged in 4 iterations"
    T = vm_T(.997, .030, .071, -.065, .829, .556, -.042, -.559, .828),
    ss = c(1.596, 1.044, 1.035), pct = c(26.606, 17.394, 17.246),
    cum = c(26.606, 43.999, 61.245))
)

# Fit one reference scenario (whole sample or one region of a grouped call)
fit_scenario <- function(ref, ...) {
  d <- if (is.null(ref$region)) survey_data else dplyr::group_by(survey_data, region)
  r <- if (ref$weighted) {
    six_items(d, weights = sampling_weight, ...)
  } else {
    six_items(d, ...)
  }
  if (is.null(ref$region)) return(r)
  regions <- vapply(r$groups, function(g) as.character(g$group_values$region),
                    character(1))
  r$groups[[which(regions == ref$region)]]
}

test_that("Varimax 1a-4a: transformation matrix and rotation sums match SPSS", {
  for (id in names(spss_varimax)) {
    ref <- spss_varimax[[id]]
    r <- fit_scenario(ref, n_factors = ref$n_factors)
    T_r <- qr.solve(r$unrotated_loadings, r$loadings)
    for (i in seq_len(nrow(ref$T))) for (j in seq_len(ncol(ref$T))) {
      assert_spss(T_r[i, j], ref$T[i, j], tier = "display", precision = 3,
                  label = sprintf("[%s] transformation matrix [%d,%d]", id, i, j))
    }
    assert_spss_count(r$rotation_iterations, ref$iterations,
                      label = sprintf("[%s] varimax iterations", id))
    rv <- r$rotation_variance
    for (j in seq_along(ref$ss)) {
      assert_spss(rv$ss_loading[j], ref$ss[j], tier = "display", precision = 3,
                  label = sprintf("[%s] rotation SS %d", id, j))
      assert_spss(rv$prc_variance[j], ref$pct[j], tier = "display", precision = 3,
                  label = sprintf("[%s] rotation %% of variance %d", id, j))
      assert_spss(rv$cumulative_prc[j], ref$cum[j], tier = "display", precision = 3,
                  label = sprintf("[%s] rotation cumulative %% %d", id, j))
    }
    if (!is.null(ref$rotated)) {
      assert_loading_values(r$loadings, ref$rotated, sprintf("%s rotated matrix", id))
    }
  }
})


# =============================================================================
# DIRECT OBLIMIN (efa_output.txt, Tests 1b, 2b, 3b, 4b)
# =============================================================================
# SPSS rotates one factor at a time against each other factor (Jennrich &
# Sampson, 1966; IBM SPSS Statistics Algorithms, FACTOR "Oblique
# Rotations", delta = 0) and stops when the criterion improves by less than
# 1e-4 of its start value - before the exact optimum a gradient-projection
# algorithm (GPArotation) reaches (Test 2b differed by up to .002).

spss_oblimin <- list(
  # efa_output.txt:263-269, 289-320 (Test 1b, unweighted)
  `1b` = list(weighted = FALSE, region = NULL,
    iterations = 6L,  # efa_output.txt:301 "Rotation converged in 6 iterations"
    pattern = spss_cells(
      "political_orientation", 1, -0.887, "environmental_concern", 1, 0.884,
      "trust_science",         2,  0.769, "trust_government",      2, 0.561,
      "life_satisfaction",     3,  0.797, "trust_media",           3, 0.613),
    structure = spss_cells(
      "political_orientation", 1, -0.886, "environmental_concern", 1, 0.884,
      "trust_science",         2,  0.757, "trust_government",      2, 0.571,
      "life_satisfaction",     3,  0.781, "trust_media",           3, 0.630),
    phi = c(`1-2` = 0.041, `1-3` = 0.024, `2-3` = 0.063),
    rotation_ss = c(1.599, 1.041, 1.022)),
  # efa_output.txt:873-879, 899-930 (Test 2b, weighted)
  `2b` = list(weighted = TRUE, region = NULL,
    iterations = 9L,  # efa_output.txt:911 "Rotation converged in 9 iterations"
    pattern = spss_cells(
      "political_orientation", 1, -0.886, "environmental_concern", 1, 0.883,
      "trust_science",         2,  0.764, "trust_government",      2, 0.534,
      "life_satisfaction",     3,  0.837, "trust_media",           3, 0.521),
    structure = spss_cells(
      "political_orientation", 1, -0.885, "environmental_concern", 1, 0.883,
      "trust_science",         2,  0.743, "trust_government",      2, 0.548,
      "life_satisfaction",     3,  0.819, "trust_media",           2, 0.414,
      "trust_media",           3,  0.550),
    phi = c(`1-2` = 0.035, `1-3` = 0.040, `2-3` = 0.083),
    rotation_ss = c(1.598, 1.045, 1.022)),
  # efa_output.txt:1499-1505, 1527-1560 (Test 3b, unweighted, region = East)
  `3b_East` = list(weighted = FALSE, region = "East",
    iterations = 3L,  # efa_output.txt:1540 "Rotation converged in 3 iterations"
    pattern = spss_cells(
      "political_orientation", 1,  0.894, "environmental_concern", 1, -0.894,
      "trust_media",           2,  0.675, "trust_science",         2,  0.581,
      "life_satisfaction",     2, -0.568, "trust_government",      3,  0.969),
    structure = spss_cells(
      "political_orientation", 1,  0.895, "environmental_concern", 1, -0.893,
      "trust_media",           2,  0.672, "trust_science",         2,  0.586,
      "life_satisfaction",     2, -0.566, "trust_government",      3,  0.969),
    phi = c(`1-2` = 0.027, `1-3` = -0.005, `2-3` = 0.022),
    rotation_ss = c(1.604, 1.120, 1.008)),
  # efa_output.txt:1648-1654, 1676-1709 (Test 3b, unweighted, region = West)
  `3b_West` = list(weighted = FALSE, region = "West",
    iterations = 5L,  # efa_output.txt:1689 "Rotation converged in 5 iterations"
    pattern = spss_cells(
      "political_orientation", 1, -0.887, "environmental_concern", 1, 0.881,
      "life_satisfaction",     2,  0.740, "trust_media",           2, 0.693,
      "trust_science",         3,  0.788, "trust_government",      3, 0.624),
    structure = spss_cells(
      "political_orientation", 1, -0.886, "environmental_concern", 1, 0.881,
      "life_satisfaction",     2,  0.735, "trust_media",           2, 0.697,
      "trust_science",         3,  0.777, "trust_government",      3, 0.634),
    phi = c(`1-2` = 0.033, `1-3` = 0.052, `2-3` = 0.035),
    rotation_ss = c(1.600, 1.043, 1.040)),
  # efa_output.txt:2125-2131, 2153-2186 (Test 4b, weighted, region = East)
  `4b_East` = list(weighted = TRUE, region = "East",
    iterations = 4L,  # efa_output.txt:2166 "Rotation converged in 4 iterations"
    pattern = spss_cells(
      "political_orientation", 1,  0.894, "environmental_concern", 1, -0.894,
      "trust_media",           2,  0.673, "trust_science",         2,  0.598,
      "life_satisfaction",     2, -0.562, "trust_government",      3,  0.964),
    structure = spss_cells(
      "political_orientation", 1,  0.895, "environmental_concern", 1, -0.893,
      "trust_media",           2,  0.669, "trust_science",         2,  0.605,
      "life_satisfaction",     2, -0.560, "trust_government",      3,  0.963),
    phi = c(`1-2` = 0.029, `1-3` = -0.012, `2-3` = 0.027),
    rotation_ss = c(1.604, 1.132, 1.012)),
  # efa_output.txt:2274-2280, 2302-2335 (Test 4b, weighted, region = West)
  `4b_West` = list(weighted = TRUE, region = "West",
    iterations = 5L,  # efa_output.txt:2315 "Rotation converged in 5 iterations"
    pattern = spss_cells(
      "political_orientation", 1, -0.886, "environmental_concern", 1, 0.880,
      "life_satisfaction",     2,  0.746, "trust_media",           2, 0.671,
      "trust_science",         3,  0.808, "trust_government",      3, 0.594),
    structure = spss_cells(
      "political_orientation", 1, -0.884, "environmental_concern", 1, 0.879,
      "life_satisfaction",     2,  0.737, "trust_media",           2, 0.677,
      "trust_science",         3,  0.793, "trust_government",      3, 0.610),
    phi = c(`1-2` = 0.049, `1-3` = 0.056, `2-3` = 0.056),
    rotation_ss = c(1.599, 1.046, 1.038))
)

assert_oblique <- function(r, ref, id) {
  assert_spss_count(r$rotation_iterations, ref$iterations,
                    label = sprintf("[%s] rotation iterations", id))
  assert_loading_values(r$pattern_matrix, ref$pattern, sprintf("%s pattern matrix", id))
  assert_loading_values(r$structure_matrix, ref$structure,
                        sprintf("%s structure matrix", id))
  for (nm in names(ref$phi)) {
    ij <- as.integer(strsplit(nm, "-")[[1]])
    assert_spss(r$factor_correlations[ij[1], ij[2]], ref$phi[[nm]],
                tier = "display", precision = 3,
                label = sprintf("[%s] component correlation %s", id, nm))
  }
  for (j in seq_along(ref$rotation_ss)) {
    assert_spss(r$rotation_variance$ss_loading[j], ref$rotation_ss[j],
                tier = "display", precision = 3,
                label = sprintf("[%s] rotation SS component %d", id, j))
  }
}

test_that("Oblimin 1b-4b: pattern, structure, correlations match SPSS", {
  for (id in names(spss_oblimin)) {
    assert_oblique(fit_scenario(spss_oblimin[[id]], rotation = "oblimin"),
                   spss_oblimin[[id]], id)
  }
})


# =============================================================================
# PCA + PROMAX (efa_ml_promax_output.txt, Tests P1-P4)
# =============================================================================
# SPSS FACTOR /ROTATION PROMAX(4): varimax, Kaiser-normalized target,
# least-squares fit (IBM SPSS Statistics Algorithms, FACTOR "Promax
# Rotation"). Every loading SPSS prints at BLANK(.40), the complete
# component correlation matrix and the rotation sums of squared loadings
# (structure matrix) are asserted. Weighted scenarios (P2, P4) use
# WEIGHT BY sampling_weight.

spss_promax <- list(
  # efa_ml_promax_output.txt:753-811 (Test P1, unweighted, ungrouped)
  P1 = list(weighted = FALSE, region = NULL,
    iterations = 4L,  # efa_ml_promax_output.txt:791 "Rotation converged in 4 iterations"
    pattern = spss_cells(
      "political_orientation", 1, -0.887, "environmental_concern", 1,  0.885,
      "trust_science",         2,  0.763, "trust_government",      2,  0.565,
      "life_satisfaction",     3,  0.789, "trust_media",           3,  0.621),
    structure = spss_cells(
      "political_orientation", 1, -0.887, "environmental_concern", 1,  0.884,
      "trust_science",         2,  0.764, "trust_government",      2,  0.564,
      "life_satisfaction",     3,  0.791, "trust_media",           3,  0.617),
    phi = c(`1-2` = -0.002, `1-3` = 0.020, `2-3` = -0.012),
    rotation_ss = c(1.599, 1.039, 1.021)),
  # efa_ml_promax_output.txt:1256-1314 (Test P2, weighted, ungrouped)
  P2 = list(weighted = TRUE, region = NULL,
    iterations = 4L,  # efa_ml_promax_output.txt:1294 "Rotation converged in 4 iterations"
    pattern = spss_cells(
      "political_orientation", 1, -0.886, "environmental_concern", 1,  0.884,
      "trust_science",         2,  0.752, "trust_government",      2,  0.541,
      "life_satisfaction",     3,  0.828, "trust_media",           3,  0.536),
    structure = spss_cells(
      "political_orientation", 1, -0.885, "environmental_concern", 1,  0.883,
      "trust_science",         2,  0.754, "trust_government",      2,  0.538,
      "life_satisfaction",     3,  0.828, "trust_media",           3,  0.532),
    phi = c(`1-2` = -0.009, `1-3` = 0.035, `2-3` = -0.007),
    rotation_ss = c(1.598, 1.043, 1.021)),
  # efa_ml_promax_output.txt:1701-1766 (Test P3, unweighted, region = East)
  P3_East = list(weighted = FALSE, region = "East",
    iterations = 3L,  # efa_ml_promax_output.txt:1742 "Rotation converged in 3 iterations"
    pattern = spss_cells(
      "political_orientation", 1,  0.894, "environmental_concern", 1, -0.894,
      "trust_media",           2,  0.673, "trust_science",         2,  0.583,
      "life_satisfaction",     2, -0.568, "trust_government",      3,  0.969),
    structure = spss_cells(
      "political_orientation", 1,  0.895, "environmental_concern", 1, -0.893,
      "trust_media",           2,  0.673, "trust_science",         2,  0.586,
      "life_satisfaction",     2, -0.566, "trust_government",      3,  0.969),
    phi = c(`1-2` = 0.037, `1-3` = -0.015, `2-3` = 0.006),
    rotation_ss = c(1.604, 1.120, 1.008)),
  # efa_ml_promax_output.txt:1850-1915 (Test P3, unweighted, region = West)
  P3_West = list(weighted = FALSE, region = "West",
    iterations = 4L,  # efa_ml_promax_output.txt:1891 "Rotation converged in 4 iterations"
    pattern = spss_cells(
      "political_orientation", 1, -0.886, "environmental_concern", 1,  0.881,
      "life_satisfaction",     2,  0.737, "trust_media",           2,  0.695,
      "trust_science",         3,  0.785, "trust_government",      3,  0.626),
    structure = spss_cells(
      "political_orientation", 1, -0.886, "environmental_concern", 1,  0.881,
      "life_satisfaction",     2,  0.737, "trust_media",           2,  0.695,
      "trust_science",         3,  0.783, "trust_government",      3,  0.629),
    phi = c(`1-2` = 0.028, `1-3` = 0.016, `2-3` = -0.001),
    rotation_ss = c(1.599, 1.043, 1.037)),
  # efa_ml_promax_output.txt:2334-2399 (Test P4, weighted, region = East)
  P4_East = list(weighted = TRUE, region = "East",
    iterations = 4L,  # efa_ml_promax_output.txt:2375 "Rotation converged in 4 iterations"
    pattern = spss_cells(
      "environmental_concern", 1, -0.894, "political_orientation", 1,  0.894,
      "trust_media",           2,  0.672, "trust_science",         2,  0.600,
      "life_satisfaction",     2, -0.562, "trust_government",      3,  0.964),
    structure = spss_cells(
      "political_orientation", 1,  0.895, "environmental_concern", 1, -0.893,
      "trust_media",           2,  0.669, "trust_science",         2,  0.606,
      "life_satisfaction",     2, -0.560, "trust_government",      3,  0.963),
    phi = c(`1-2` = 0.041, `1-3` = -0.019, `2-3` = 0.022),
    rotation_ss = c(1.604, 1.133, 1.012)),
  # efa_ml_promax_output.txt:2483-2548 (Test P4, weighted, region = West)
  P4_West = list(weighted = TRUE, region = "West",
    iterations = 4L,  # efa_ml_promax_output.txt:2524 "Rotation converged in 4 iterations"
    pattern = spss_cells(
      "political_orientation", 1, -0.885, "environmental_concern", 1,  0.881,
      "life_satisfaction",     2,  0.741, "trust_media",           2,  0.675,
      "trust_science",         3,  0.802, "trust_government",      3,  0.598),
    structure = spss_cells(
      "political_orientation", 1, -0.885, "environmental_concern", 1,  0.879,
      "life_satisfaction",     2,  0.740, "trust_media",           2,  0.674,
      "trust_science",         3,  0.801, "trust_government",      3,  0.599),
    phi = c(`1-2` = 0.045, `1-3` = 0.012, `2-3` = -0.006),
    rotation_ss = c(1.598, 1.045, 1.034))
)

test_that("Tests P1-P4: PCA + promax matrices match SPSS", {
  for (id in names(spss_promax)) {
    assert_oblique(fit_scenario(spss_promax[[id]], rotation = "promax"),
                   spss_promax[[id]], id)
  }
})


# =============================================================================
# ML EXTRACTION (efa_ml_promax_output.txt, Tests 5a, 6a, 7a, 8a)
# =============================================================================
# All SPSS ML runs extract 3 factors from 6 items: df = 0 ("The number of
# degrees of freedom (0) is not positive"), and every run hits Heywood
# cases. Initial communalities (SMC) do not depend on the iterations and
# are asserted for every run (Display tier).
#
# Where SPSS reaches the maximum of the likelihood (7a West, 8a West:
# "3 factors extracted") mariposa reproduces the whole solution, including
# SPSS's bound of .999 for a Heywood communality and SPSS's factor order.
# The communalities match at Display tier. SPSS stops its Newton-Raphson
# iteration once no log unique variance changes by .001 or more
# (ECONVERGE), so loadings and sums of squares carry an error of that
# order; the percentages of variance (5 significant digits) resolve it.
# These are asserted with EXC-001.
#
# Where SPSS does not reach the maximum:
# - 5a / 6a: SPSS stops without convergence ("More than 25 iterations
#   required. (Convergence=.008 / .004)"): its communalities are those of
#   an unfinished path on an almost flat likelihood -> EXC-002.
# - 8a East: SPSS stops at a point with a larger discrepancy than
#   mariposa's solution (F = .0049 vs .0034), a different Heywood
#   solution that no tolerance can bridge: not asserted beyond the SMC.
# - 7a East: SPSS reports no solution ("no local minimum was found").

spss_ml_smc <- list(
  # efa_ml_promax_output.txt:101-108 (Test 5a, unweighted)
  `5a` = list(weighted = FALSE, region = NULL,
              smc = c(.346, .345, .001, .005, .001, .003)),
  # efa_ml_promax_output.txt:915-922 (Test 6a, weighted)
  `6a` = list(weighted = TRUE, region = NULL,
              smc = c(.343, .343, .001, .006, .001, .004)),
  # efa_ml_promax_output.txt:1423-1430 (Test 7a, unweighted, region = East)
  `7a_East` = list(weighted = FALSE, region = "East",
                   smc = c(.376, .374, .006, .006, .028, .009)),
  # efa_ml_promax_output.txt:1524-1531 (Test 7a, unweighted, region = West)
  `7a_West` = list(weighted = FALSE, region = "West",
                   smc = c(.344, .342, .002, .008, .002, .004)),
  # efa_ml_promax_output.txt:2022-2029 (Test 8a, weighted, region = East)
  `8a_East` = list(weighted = TRUE, region = "East",
                   smc = c(.375, .373, .006, .008, .030, .012)),
  # efa_ml_promax_output.txt:2157-2164 (Test 8a, weighted, region = West)
  `8a_West` = list(weighted = TRUE, region = "West",
                   smc = c(.340, .338, .002, .008, .002, .004))
)

item_names <- c("political_orientation", "environmental_concern",
                "life_satisfaction", "trust_government", "trust_media",
                "trust_science")

test_that("ML 5a-8a: initial communalities (SMC) match SPSS", {
  for (id in names(spss_ml_smc)) {
    ref <- spss_ml_smc[[id]]
    r <- suppressWarnings(fit_scenario(ref, extraction = "ml"))
    for (k in seq_along(item_names)) {
      assert_spss(r$initial_communalities[[item_names[k]]], ref$smc[k],
                  tier = "display", precision = 3,
                  label = sprintf("[%s] initial communality %s", id, item_names[k]))
    }
  }
})

spss_ml <- list(
  # efa_ml_promax_output.txt:1524-1586 (Test 7a, unweighted, region = West;
  # "3 factors extracted. 25 iterations required.")
  `7a_West` = list(weighted = FALSE, region = "West",
    comm = c(.638, .539, .023, .012, .072, .999),
    ext_ss = c(1.002, 1.181, .100), ext_pct = c(16.708, 19.688, 1.665),
    ext_cum = c(16.708, 36.396, 38.061),
    factor = spss_cells(
      "trust_science",         1,  0.999, "political_orientation", 2, -0.797,
      "environmental_concern", 2,  0.733),
    rotated = spss_cells(
      "political_orientation", 1, -0.797, "environmental_concern", 1, 0.732,
      "trust_science",         2,  0.999),
    rot_ss = c(1.179, 1.004, .101), rot_pct = c(19.656, 16.727, 1.677),
    rot_cum = c(19.656, 36.383, 38.061),
    T = vm_T(.015, 1.000, -.007, 1.000, -.015, -.025, .025, .006, 1.000)),
  # efa_ml_promax_output.txt:2157-2220 (Test 8a, weighted, region = West;
  # "3 factors extracted. 8 iterations required.")
  `8a_West` = list(weighted = TRUE, region = "West",
    comm = c(.615, .555, .030, .016, .054, .999),
    ext_ss = c(1.003, 1.174, .093), ext_pct = c(16.710, 19.560, 1.546),
    ext_cum = c(16.710, 36.270, 37.816),
    factor = spss_cells(
      "trust_science",         1,  0.999, "political_orientation", 2, -0.782,
      "environmental_concern", 2,  0.743),
    rotated = spss_cells(
      "political_orientation", 1, -0.782, "environmental_concern", 1, 0.743,
      "trust_science",         2,  0.999),
    rot_ss = c(1.174, 1.002, .093), rot_pct = c(19.563, 16.707, 1.547),
    rot_cum = c(19.563, 36.270, 37.816),
    T = vm_T(-.002, 1.000, .002, 1.000, .002, -.001, .001, -.002, 1.000))
)

test_that("ML 7a/8a West: converged ML + varimax solutions match SPSS", {
  for (id in names(spss_ml)) {
    ref <- spss_ml[[id]]
    r <- fit_scenario(ref, extraction = "ml")
    for (k in seq_along(item_names)) {
      assert_spss(r$communalities[[item_names[k]]], ref$comm[k],
                  tier = "display", precision = 3,
                  label = sprintf("[%s] extraction communality %s", id, item_names[k]))
    }
    # EXC-001 — SPSS stops the ML iteration at ECONVERGE(.001)
    exc1 <- function(actual, expected, label) {
      assert_spss(actual, expected, tier = "exception", id = "EXC-001",
                  label = sprintf("[%s] %s", id, label))
    }
    ev <- r$extraction_variance
    for (j in seq_along(ref$ext_ss)) {
      exc1(ev$ss_loading[j], ref$ext_ss[j], sprintf("extraction SS %d", j))
      exc1(ev$prc_variance[j], ref$ext_pct[j], sprintf("extraction %% of variance %d", j))
      exc1(ev$cumulative_prc[j], ref$ext_cum[j], sprintf("extraction cumulative %% %d", j))
    }
    for (cells in list(list(r$unrotated_loadings, ref$factor, "factor matrix"),
                       list(r$loadings, ref$rotated, "rotated factor matrix"))) {
      ref_cells <- cells[[2]]
      for (i in seq_len(nrow(ref_cells))) {
        exc1(cells[[1]][ref_cells$var[i], ref_cells$comp[i]], ref_cells$value[i],
             sprintf("%s: %s on factor %d", cells[[3]], ref_cells$var[i], ref_cells$comp[i]))
      }
    }
    rv <- r$rotation_variance
    for (j in seq_along(ref$rot_ss)) {
      exc1(rv$ss_loading[j], ref$rot_ss[j], sprintf("rotation SS %d", j))
      exc1(rv$prc_variance[j], ref$rot_pct[j], sprintf("rotation %% of variance %d", j))
      exc1(rv$cumulative_prc[j], ref$rot_cum[j], sprintf("rotation cumulative %% %d", j))
    }
    T_r <- qr.solve(r$unrotated_loadings, r$loadings)
    for (i in 1:3) for (j in 1:3) {
      exc1(T_r[i, j], ref$T[i, j], sprintf("factor transformation matrix [%d,%d]", i, j))
    }
  }
})

spss_ml_nonconverged <- list(
  # efa_ml_promax_output.txt:101-122 (Test 5a, unweighted: "More than 25
  # iterations required. (Convergence=.008)")
  `5a` = list(weighted = FALSE, region = NULL,
              comm = c(.608, .573, .100, .017, .010, .105),
              ext_ss = c(1.184, .125, .104)),
  # efa_ml_promax_output.txt:915-936 (Test 6a, weighted: "More than 25
  # iterations required. (Convergence=.004)")
  `6a` = list(weighted = TRUE, region = NULL,
              comm = c(.611, .566, .172, .021, .010, .087),
              ext_ss = c(1.181, .177, .109))
)

test_that("ML 5a/6a: SPSS runs without convergence agree within EXC-002", {
  for (id in names(spss_ml_nonconverged)) {
    ref <- spss_ml_nonconverged[[id]]
    r <- fit_scenario(ref, extraction = "ml")
    for (k in seq_along(item_names)) {
      # EXC-002 — SPSS reports a non-converged ML solution (df = 0, Heywood)
      assert_spss(r$communalities[[item_names[k]]], ref$comm[k],
                  tier = "exception", id = "EXC-002",
                  label = sprintf("[%s] extraction communality %s", id, item_names[k]))
    }
    for (j in seq_along(ref$ext_ss)) {
      # EXC-002 — SPSS reports a non-converged ML solution (df = 0, Heywood)
      assert_spss(r$extraction_variance$ss_loading[j], ref$ext_ss[j],
                  tier = "exception", id = "EXC-002",
                  label = sprintf("[%s] extraction SS %d", id, j))
    }
  }
})
