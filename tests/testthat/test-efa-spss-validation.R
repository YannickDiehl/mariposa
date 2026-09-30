# =============================================================================
# efa — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::efa() against SPSS v29 FACTOR.
# Reference: efa_output.txt
#
# Validates KMO, Bartlett's, eigenvalues, variance explained, communalities
# (PCA + Varimax, efa_output.txt) and the PCA + Promax pattern, structure and
# component correlation matrices (efa_ml_promax_output.txt, Tests P1-P4).
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
# Oblimin pattern matrices / factor correlations differ from SPSS in the
# third decimal (SPSS stops its pairwise iterations early), so only their
# signs are asserted (a sign is an integer: Spec tier, exact). Promax values
# are asserted in full below.

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
  # efa_output.txt:289-297 (Test 1b, Pattern Matrix, Oblimin)
  t1b_pattern = spss_cells(
    "political_orientation", 1, -0.887, "environmental_concern", 1,  0.884,
    "trust_science",         2,  0.769, "trust_government",      2,  0.561,
    "life_satisfaction",     3,  0.797, "trust_media",           3,  0.613),
  # efa_output.txt:316-320 (Test 1b, Component Correlation Matrix)
  t1b_phi = c(`1-2` = 0.041, `1-3` = 0.024, `2-3` = 0.063),
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

assert_loading_signs <- function(mat, cells, tag) {
  for (i in seq_len(nrow(cells))) {
    assert_spss(sign(mat[cells$var[i], cells$comp[i]]), sign(cells$value[i]),
                tier = "spec", what = "count",
                label = sprintf("[%s] sign of %s on component %d", tag,
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

test_that("Signs 1b: oblimin pattern matrix and correlations match SPSS signs", {
  skip_if_not_installed("GPArotation")
  r <- six_items(survey_data, rotation = "oblimin")
  assert_loading_values(r$unrotated_loadings, spss_signs$t1a_component,
                        "1b component matrix")
  assert_loading_signs(r$pattern_matrix, spss_signs$t1b_pattern,
                       "1b pattern matrix")
  phi <- r$factor_correlations
  for (nm in names(spss_signs$t1b_phi)) {
    ij <- as.integer(strsplit(nm, "-")[[1]])
    assert_spss(sign(phi[ij[1], ij[2]]), sign(spss_signs$t1b_phi[[nm]]),
                tier = "spec", what = "count",
                label = sprintf("[1b] sign of component correlation %s", nm))
  }
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

test_that("Test 1b: oblimin rotation sums of squared loadings match SPSS", {
  skip_if_not_installed("GPArotation")
  # efa_output.txt:263-272 (Test 1b, Rotation Sums of Squared Loadings: the
  # column sums of squares of the structure matrix)
  r <- six_items(survey_data, rotation = "oblimin")
  expected <- c(1.599, 1.041, 1.022)
  for (j in seq_along(expected)) {
    assert_spss(r$rotation_variance$ss_loading[j], expected[j],
                tier = "display", precision = 3,
                label = sprintf("[1b] rotation SS component %d", j))
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

promax_fit <- function(ref) {
  d <- if (is.null(ref$region)) survey_data else dplyr::group_by(survey_data, region)
  r <- if (ref$weighted) {
    six_items(d, rotation = "promax", weights = sampling_weight)
  } else {
    six_items(d, rotation = "promax")
  }
  if (is.null(ref$region)) return(r)
  regions <- vapply(r$groups, function(g) as.character(g$group_values$region),
                    character(1))
  r$groups[[which(regions == ref$region)]]
}

test_that("Tests P1-P4: PCA + promax matrices match SPSS", {
  for (id in names(spss_promax)) {
    ref <- spss_promax[[id]]
    r <- promax_fit(ref)
    assert_loading_values(r$pattern_matrix, ref$pattern,
                          sprintf("%s pattern matrix", id))
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
})
