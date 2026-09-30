# =============================================================================
# spearman_rho — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::spearman_rho() against SPSS v29 NONPAR CORR.
# Reference output: tests/spss_reference/outputs/spearman_rho_output.txt
#
# Coverage (unweighted, every cell SPSS prints: rho, Sig. and pair N of the
# upper triangle plus the diagonal N, variable order as SPSS prints it):
#   Tests 1a-1f  ungrouped matrices (2x2 up to 5x5)
#   Tests 3a-3d  SPLIT FILE by region (East, West in SPSS order)
#   Test  5a     single pair, 5b listwise deletion (Listwise N),
#         5c     one-tailed (alternative = "greater")
#
# NONPAR CORR family (Spearman) does NOT honor WEIGHT BY — same quirk as
# NPAR TESTS. SPSS Tests 2a (weighted) produce identical rho/p/N as Tests
# 1a (unweighted). mariposa's spearman_rho likewise does not produce
# different weighted output (verified empirically). Tests 2a/4a therefore
# compare the weighted call against the unweighted 1a/3a reference; no
# further weighted sections are asserted while the weighted NONPAR CORR
# semantics are pending an SPSS WEIGHT BY reference run.
#
# Source audit (R/spearman_rho.R): n_eff <- n (line 201) for unweighted;
# weighted path also uses sum(w) appropriately. No round(sum(w)) bug.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# One upper-triangle cell of an SPSS NONPAR CORR matrix: row and column
# variable plus the three values SPSS prints for the pair (coefficient,
# Sig., N). Each cell cites its SPSS row block (coefficient line .. N line;
# the pair's values sit in the column of `col`). Sig. printed as ".000" is
# cached as "<.001" (Charter §4 sentinel).
np_cell <- function(row, col, coef, p, n) {
  list(row = row, col = col, coef = coef, p = p, n = n)
}


# =============================================================================
# SPSS REFERENCE VALUES
# =============================================================================

spss_values <- list(

  # ---- Test 1a (spearman_rho_output.txt:3): Life satisfaction, Political orientation, Trust media
  test_1a = list(
    cells = list(
      np_cell("life_satisfaction",     "political_orientation", -0.004,   0.833, 2228),  # spearman_rho_output.txt:13-15
      np_cell("life_satisfaction",     "trust_media",            0.028,   0.181, 2291),  # spearman_rho_output.txt:13-15
      np_cell("political_orientation", "trust_media",            0.003,   0.885, 2177)   # spearman_rho_output.txt:16-18
    ),
    diag_n = c(
      life_satisfaction     = 2421,  # spearman_rho_output.txt:15
      political_orientation = 2299,  # spearman_rho_output.txt:18
      trust_media           = 2367   # spearman_rho_output.txt:21
    )
  ),

  # ---- Test 1b (spearman_rho_output.txt:23): Income, Age, Life satisfaction
  test_1b = list(
    cells = list(
      np_cell("income",                "age",                    0.003,   0.870, 2186),  # spearman_rho_output.txt:32-34
      np_cell("income",                "life_satisfaction",      0.464, "<.001", 2115),  # spearman_rho_output.txt:32-34
      np_cell("age",                   "life_satisfaction",     -0.024,   0.238, 2421)   # spearman_rho_output.txt:35-37
    ),
    diag_n = c(
      income                = 2186,  # spearman_rho_output.txt:34
      age                   = 2500,  # spearman_rho_output.txt:37
      life_satisfaction     = 2421   # spearman_rho_output.txt:40
    )
  ),

  # ---- Test 1c (spearman_rho_output.txt:43): Trust government, Trust media, Trust science
  test_1c = list(
    cells = list(
      np_cell("trust_government",      "trust_media",            0.008,   0.723, 2227),  # spearman_rho_output.txt:52-54
      np_cell("trust_government",      "trust_science",          0.027,   0.207, 2255),  # spearman_rho_output.txt:52-54
      np_cell("trust_media",           "trust_science",          0.016,   0.453, 2272)   # spearman_rho_output.txt:55-57
    ),
    diag_n = c(
      trust_government      = 2354,  # spearman_rho_output.txt:54
      trust_media           = 2367,  # spearman_rho_output.txt:57
      trust_science         = 2398   # spearman_rho_output.txt:60
    )
  ),

  # ---- Test 1d (spearman_rho_output.txt:62): Political orientation with trust variables
  test_1d = list(
    cells = list(
      np_cell("political_orientation", "trust_government",      -0.055,   0.011, 2168),  # spearman_rho_output.txt:72-75
      np_cell("political_orientation", "trust_media",            0.003,   0.885, 2177),  # spearman_rho_output.txt:72-75
      np_cell("political_orientation", "trust_science",          0.032,   0.130, 2202),  # spearman_rho_output.txt:72-75
      np_cell("trust_government",      "trust_media",            0.008,   0.723, 2227),  # spearman_rho_output.txt:76-79
      np_cell("trust_government",      "trust_science",          0.027,   0.207, 2255),  # spearman_rho_output.txt:76-79
      np_cell("trust_media",           "trust_science",          0.016,   0.453, 2272)   # spearman_rho_output.txt:80-83
    ),
    diag_n = c(
      political_orientation = 2299,  # spearman_rho_output.txt:75
      trust_government      = 2354,  # spearman_rho_output.txt:79
      trust_media           = 2367,  # spearman_rho_output.txt:83
      trust_science         = 2398   # spearman_rho_output.txt:87
    )
  ),

  # ---- Test 1e (spearman_rho_output.txt:90): Income and Age
  test_1e = list(
    cells = list(
      np_cell("income",                "age",                    0.003,   0.870, 2186)   # spearman_rho_output.txt:98-100
    ),
    diag_n = c(
      income                = 2186,  # spearman_rho_output.txt:100
      age                   = 2500   # spearman_rho_output.txt:103
    )
  ),

  # ---- Test 1f (spearman_rho_output.txt:105): Comprehensive variable set
  test_1f = list(
    cells = list(
      np_cell("life_satisfaction",     "income",                 0.464, "<.001", 2115),  # spearman_rho_output.txt:115-118
      np_cell("life_satisfaction",     "age",                   -0.024,   0.238, 2421),  # spearman_rho_output.txt:115-118
      np_cell("life_satisfaction",     "political_orientation", -0.004,   0.833, 2228),  # spearman_rho_output.txt:115-118
      np_cell("life_satisfaction",     "environmental_concern",  0.003,   0.904, 2324),  # spearman_rho_output.txt:115-118
      np_cell("income",                "age",                    0.003,   0.870, 2186),  # spearman_rho_output.txt:119-122
      np_cell("income",                "political_orientation", -0.030,   0.184, 2008),  # spearman_rho_output.txt:119-122
      np_cell("income",                "environmental_concern",  0.004,   0.870, 2097),  # spearman_rho_output.txt:119-122
      np_cell("age",                   "political_orientation", -0.037,   0.080, 2299),  # spearman_rho_output.txt:123-126
      np_cell("age",                   "environmental_concern",  0.021,   0.315, 2400),  # spearman_rho_output.txt:123-126
      np_cell("political_orientation", "environmental_concern", -0.576, "<.001", 2207)   # spearman_rho_output.txt:127-130
    ),
    diag_n = c(
      life_satisfaction     = 2421,  # spearman_rho_output.txt:118
      income                = 2186,  # spearman_rho_output.txt:122
      age                   = 2500,  # spearman_rho_output.txt:126
      political_orientation = 2299,  # spearman_rho_output.txt:130
      environmental_concern = 2400   # spearman_rho_output.txt:134
    )
  ),

  # ---- Test 3a (spearman_rho_output.txt:275): Life satisfaction, Political orientation, Trust media -- by region
  test_3a = list(
    East = list(
      cells = list(
        np_cell("life_satisfaction",     "political_orientation",  0.010,   0.840,  427),  # spearman_rho_output.txt:285-288
        np_cell("life_satisfaction",     "trust_media",           -0.059,   0.219,  440),  # spearman_rho_output.txt:285-288
        np_cell("political_orientation", "trust_media",            0.070,   0.153,  420)   # spearman_rho_output.txt:289-292
      ),
      diag_n = c(
        life_satisfaction     =  465,  # spearman_rho_output.txt:288
        political_orientation =  443,  # spearman_rho_output.txt:292
        trust_media           =  460   # spearman_rho_output.txt:296
      )
    ),
    West = list(
      cells = list(
        np_cell("life_satisfaction",     "political_orientation", -0.008,   0.730, 1801),  # spearman_rho_output.txt:297-300
        np_cell("life_satisfaction",     "trust_media",            0.048,   0.038, 1851),  # spearman_rho_output.txt:297-300
        np_cell("political_orientation", "trust_media",           -0.012,   0.608, 1757)   # spearman_rho_output.txt:301-304
      ),
      diag_n = c(
        life_satisfaction     = 1956,  # spearman_rho_output.txt:300
        political_orientation = 1856,  # spearman_rho_output.txt:304
        trust_media           = 1907   # spearman_rho_output.txt:308
      )
    )
  ),

  # ---- Test 3b (spearman_rho_output.txt:311): Income, Age, Life satisfaction -- by region
  test_3b = list(
    East = list(
      cells = list(
        np_cell("income",                "age",                    0.058,   0.234,  429),  # spearman_rho_output.txt:321-323
        np_cell("income",                "life_satisfaction",      0.440, "<.001",  410),  # spearman_rho_output.txt:321-323
        np_cell("age",                   "life_satisfaction",     -0.040,   0.391,  465)   # spearman_rho_output.txt:324-326
      ),
      diag_n = c(
        income                =  429,  # spearman_rho_output.txt:323
        age                   =  485,  # spearman_rho_output.txt:326
        life_satisfaction     =  465   # spearman_rho_output.txt:329
      )
    ),
    West = list(
      cells = list(
        np_cell("income",                "age",                   -0.008,   0.725, 1757),  # spearman_rho_output.txt:330-332
        np_cell("income",                "life_satisfaction",      0.470, "<.001", 1705),  # spearman_rho_output.txt:330-332
        np_cell("age",                   "life_satisfaction",     -0.020,   0.382, 1956)   # spearman_rho_output.txt:333-335
      ),
      diag_n = c(
        income                = 1757,  # spearman_rho_output.txt:332
        age                   = 2015,  # spearman_rho_output.txt:335
        life_satisfaction     = 1956   # spearman_rho_output.txt:338
      )
    )
  ),

  # ---- Test 3c (spearman_rho_output.txt:341): Trust variables -- by region
  test_3c = list(
    East = list(
      cells = list(
        np_cell("trust_government",      "trust_media",           -0.029,   0.548,  435),  # spearman_rho_output.txt:352-355
        np_cell("trust_government",      "trust_science",          0.004,   0.941,  444),  # spearman_rho_output.txt:352-355
        np_cell("trust_media",           "trust_science",          0.051,   0.285,  447)   # spearman_rho_output.txt:356-359
      ),
      diag_n = c(
        trust_government      =  460,  # spearman_rho_output.txt:355
        trust_media           =  460,  # spearman_rho_output.txt:359
        trust_science         =  469   # spearman_rho_output.txt:363
      )
    ),
    West = list(
      cells = list(
        np_cell("trust_government",      "trust_media",            0.016,   0.502, 1792),  # spearman_rho_output.txt:364-367
        np_cell("trust_government",      "trust_science",          0.032,   0.168, 1811),  # spearman_rho_output.txt:364-367
        np_cell("trust_media",           "trust_science",          0.008,   0.746, 1825)   # spearman_rho_output.txt:368-371
      ),
      diag_n = c(
        trust_government      = 1894,  # spearman_rho_output.txt:367
        trust_media           = 1907,  # spearman_rho_output.txt:371
        trust_science         = 1929   # spearman_rho_output.txt:375
      )
    )
  ),

  # ---- Test 3d (spearman_rho_output.txt:377): Income and Age -- by region
  test_3d = list(
    East = list(
      cells = list(
        np_cell("income",                "age",                    0.058,   0.234,  429)   # spearman_rho_output.txt:385-387
      ),
      diag_n = c(
        income                =  429,  # spearman_rho_output.txt:387
        age                   =  485   # spearman_rho_output.txt:390
      )
    ),
    West = list(
      cells = list(
        np_cell("income",                "age",                   -0.008,   0.725, 1757)   # spearman_rho_output.txt:391-393
      ),
      diag_n = c(
        income                = 1757,  # spearman_rho_output.txt:393
        age                   = 2015   # spearman_rho_output.txt:396
      )
    )
  ),

  # ---- Test 5a (spearman_rho_output.txt:525): Single pair correlation
  test_5a = list(
    cells = list(
      np_cell("life_satisfaction",     "political_orientation", -0.004,   0.833, 2228)   # spearman_rho_output.txt:534-536
    ),
    diag_n = c(
      life_satisfaction     = 2421,  # spearman_rho_output.txt:536
      political_orientation = 2299   # spearman_rho_output.txt:539
    )
  ),

  # ---- Test 5b (spearman_rho_output.txt:541): Listwise deletion
  test_5b_listwise = list(
    cells = list(
      np_cell("life_satisfaction",     "political_orientation",  0.000,   0.991, 2109),  # spearman_rho_output.txt:551-553, N :558
      np_cell("life_satisfaction",     "trust_media",            0.029,   0.177, 2109),  # spearman_rho_output.txt:551-553, N :558
      np_cell("political_orientation", "trust_media",            0.007,   0.749, 2109)   # spearman_rho_output.txt:554-555, N :558
    ),
    diag_n = c(
      life_satisfaction     = 2109,  # spearman_rho_output.txt:558
      political_orientation = 2109,  # spearman_rho_output.txt:558
      trust_media           = 2109   # spearman_rho_output.txt:558
    )
  ),

  # ---- Test 5c (spearman_rho_output.txt:560): One-tailed test
  test_5c_one_tailed = list(
    cells = list(
      np_cell("income",                "age",                    0.003,   0.435, 2186)   # spearman_rho_output.txt:568-570
    ),
    diag_n = c(
      income                = 2186,  # spearman_rho_output.txt:570
      age                   = 2500   # spearman_rho_output.txt:573
    )
  )
)


# =============================================================================
# COMPARISON HELPERS
# =============================================================================

# Asserts every cached cell of one SPSS matrix: rho, Sig. and pair N of the
# upper triangle, the diagonal N, and the variable order SPSS prints.
compare_spearman <- function(matrices, spss, scenario) {
  expect_identical(rownames(matrices$rho), names(spss$diag_n),
                   label = sprintf("[%s] variable order", scenario))
  for (cl in spss$cells) {
    lab <- sprintf("[%s | %s ~ %s]", scenario, cl$row, cl$col)
    assert_spss(matrices$rho[cl$row, cl$col], cl$coef,
                tier = "display", precision = 3,
                label = paste(lab, "rho"))
    assert_spss(matrices$p_values[cl$row, cl$col], cl$p,
                tier = "display", precision = 3, what = "p_value",
                label = paste(lab, "Sig"))
    assert_spss_count(matrices$n_obs[cl$row, cl$col], cl$n,
                      label = paste(lab, "N"))
  }
  for (var in names(spss$diag_n)) {
    assert_spss_count(matrices$n_obs[var, var], spss$diag_n[[var]],
                      label = sprintf("[%s | %s] diag N", scenario, var))
  }
}

# Grouped results: one matrix per region, in SPSS SPLIT FILE order.
# spearman_rho has no $group_keys; its grouped matrices are ordered by the
# first appearance of each region in r$correlations.
compare_spearman_grouped <- function(r, spss, scenario) {
  regions <- as.character(unique(r$correlations$region))
  expect_identical(regions, names(spss),
                   label = sprintf("[%s] region order", scenario))
  for (i in seq_along(r$matrices)) {
    compare_spearman(r$matrices[[i]], spss[[regions[i]]],
                     sprintf("%s [%s]", scenario, regions[i]))
  }
}


# =============================================================================
# DATA SETUP
# =============================================================================

data(survey_data, envir = environment())


# =============================================================================
# SCENARIO 1 — UNWEIGHTED / UNGROUPED
# =============================================================================

test_that("Test 1a: Spearman 3-variable unweighted ungrouped — matches SPSS", {
  r <- survey_data |>
    spearman_rho(life_satisfaction, political_orientation, trust_media)
  compare_spearman(r$matrices[[1]], spss_values$test_1a,
                   "1a: unweighted ungrouped")
})

test_that("Test 1b: Spearman income/age/life_satisfaction — matches SPSS", {
  r <- survey_data |>
    spearman_rho(income, age, life_satisfaction)
  compare_spearman(r$matrices[[1]], spss_values$test_1b, "1b")
})

test_that("Test 1c: Spearman trust variables — matches SPSS", {
  r <- survey_data |>
    spearman_rho(trust_government, trust_media, trust_science)
  compare_spearman(r$matrices[[1]], spss_values$test_1c, "1c")
})

test_that("Test 1d: Spearman 4x4 political_orientation with trust — matches SPSS", {
  r <- survey_data |>
    spearman_rho(political_orientation, trust_government, trust_media,
                 trust_science)
  compare_spearman(r$matrices[[1]], spss_values$test_1d, "1d")
})

test_that("Test 1e: Spearman income/age — matches SPSS", {
  r <- survey_data |>
    spearman_rho(income, age)
  compare_spearman(r$matrices[[1]], spss_values$test_1e, "1e")
})

test_that("Test 1f: Spearman 5x5 comprehensive set — matches SPSS", {
  r <- survey_data |>
    spearman_rho(life_satisfaction, income, age, political_orientation,
                 environmental_concern)
  compare_spearman(r$matrices[[1]], spss_values$test_1f, "1f")
})


# =============================================================================
# SCENARIO 2 — WEIGHTED / UNGROUPED (identical to 1a due to NONPAR quirk)
# =============================================================================

test_that("Test 2a: Spearman weighted ungrouped — matches SPSS (NPAR quirk)", {
  r <- survey_data |>
    spearman_rho(life_satisfaction, political_orientation, trust_media,
                 weights = sampling_weight)
  # SPSS NONPAR CORR ignores WEIGHT BY; reference == unweighted Test 1a.
  compare_spearman(r$matrices[[1]], spss_values$test_1a,
                   "2a: weighted ungrouped (SPSS treats as unweighted)")
})


# =============================================================================
# SCENARIO 3 — UNWEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 3a: Spearman unweighted grouped by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    spearman_rho(life_satisfaction, political_orientation, trust_media)
  compare_spearman_grouped(r, spss_values$test_3a, "3a: unweighted grouped")
})

test_that("Test 3b: Spearman income/age/life_satisfaction by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    spearman_rho(income, age, life_satisfaction)
  compare_spearman_grouped(r, spss_values$test_3b, "3b")
})

test_that("Test 3c: Spearman trust variables by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    spearman_rho(trust_government, trust_media, trust_science)
  compare_spearman_grouped(r, spss_values$test_3c, "3c")
})

test_that("Test 3d: Spearman income/age by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    spearman_rho(income, age)
  compare_spearman_grouped(r, spss_values$test_3d, "3d")
})


# =============================================================================
# SCENARIO 4 — WEIGHTED / GROUPED by region (identical to 3a)
# =============================================================================

test_that("Test 4a: Spearman weighted grouped by region — matches SPSS (NPAR quirk)", {
  r <- survey_data |>
    group_by(region) |>
    spearman_rho(life_satisfaction, political_orientation, trust_media,
                 weights = sampling_weight)
  compare_spearman_grouped(r, spss_values$test_3a, "4a: weighted grouped")
})


# =============================================================================
# EDGE CASES (Tests 5a-5c)
# =============================================================================

test_that("Test 5a: Spearman single pair (2x2) — matches SPSS", {
  r <- survey_data |>
    spearman_rho(life_satisfaction, political_orientation)
  compare_spearman(r$matrices[[1]], spss_values$test_5a, "5a: single pair")
})

test_that("Test 5b: Spearman listwise deletion — matches SPSS (Listwise N)", {
  # /MISSING=LISTWISE: SPSS prints no N rows, only "Listwise N = 2109",
  # which is the N of every cell (pairs and diagonal).
  r <- survey_data |>
    spearman_rho(life_satisfaction, political_orientation, trust_media,
                 use = "listwise")
  compare_spearman(r$matrices[[1]], spss_values$test_5b_listwise,
                   "5b: listwise")
})

test_that("Test 5c: Spearman one-tailed — matches SPSS Sig. (1-tailed)", {
  # /PRINT=ONETAIL tests in the direction of the observed coefficient;
  # rho = .003 > 0, so the mariposa counterpart is alternative = "greater".
  r <- survey_data |>
    spearman_rho(income, age, alternative = "greater")
  compare_spearman(r$matrices[[1]], spss_values$test_5c_one_tailed,
                   "5c: one-tailed")
})
