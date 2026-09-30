# =============================================================================
# kendall_tau — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::kendall_tau() against SPSS v29 NONPAR CORR
# (Kendall's tau-b). Reference: kendall_tau_output.txt
#
# Coverage (unweighted, every cell SPSS prints: tau, Sig. and pair N of the
# upper triangle plus the diagonal N, variable order as SPSS prints it):
#   Tests 1a-1f  ungrouped matrices (2x2 up to 5x5)
#   Tests 3a-3d  SPLIT FILE by region (East, West in SPSS order)
#   Test  5a     single pair, 5b listwise deletion (Listwise N),
#         5c     one-tailed (alternative = "greater")
#
# Weighted scenarios (Tests 2a, 4a) are R-only Tier-4 baselines, not SPSS
# parity: mariposa's kendall_tau honours weights, while the weighted NONPAR
# CORR semantics are pending an SPSS WEIGHT BY reference run.
#
# Source audit: kendall_tau.R has no round(sum(w)) bug.
# Output uses $tau (matrix) not $correlations.
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

  # ---- Test 1a (kendall_tau_output.txt:3): Life satisfaction, Political orientation, Trust media
  test_1a = list(
    cells = list(
      np_cell("life_satisfaction",     "political_orientation", -0.004,   0.832, 2228),  # kendall_tau_output.txt:13-15
      np_cell("life_satisfaction",     "trust_media",            0.023,   0.176, 2291),  # kendall_tau_output.txt:13-15
      np_cell("political_orientation", "trust_media",            0.003,   0.883, 2177)   # kendall_tau_output.txt:16-18
    ),
    diag_n = c(
      life_satisfaction     = 2421,  # kendall_tau_output.txt:15
      political_orientation = 2299,  # kendall_tau_output.txt:18
      trust_media           = 2367   # kendall_tau_output.txt:21
    )
  ),

  # ---- Test 1b (kendall_tau_output.txt:23): Income, Age, Life satisfaction
  test_1b = list(
    cells = list(
      np_cell("income",                "age",                    0.002,   0.867, 2186),  # kendall_tau_output.txt:32-34
      np_cell("income",                "life_satisfaction",      0.353, "<.001", 2115),  # kendall_tau_output.txt:32-34
      np_cell("age",                   "life_satisfaction",     -0.018,   0.232, 2421)   # kendall_tau_output.txt:35-37
    ),
    diag_n = c(
      income                = 2186,  # kendall_tau_output.txt:34
      age                   = 2500,  # kendall_tau_output.txt:37
      life_satisfaction     = 2421   # kendall_tau_output.txt:40
    )
  ),

  # ---- Test 1c (kendall_tau_output.txt:43): Trust government, Trust media, Trust science
  test_1c = list(
    cells = list(
      np_cell("trust_government",      "trust_media",            0.006,   0.722, 2227),  # kendall_tau_output.txt:52-54
      np_cell("trust_government",      "trust_science",          0.022,   0.205, 2255),  # kendall_tau_output.txt:52-54
      np_cell("trust_media",           "trust_science",          0.013,   0.455, 2272)   # kendall_tau_output.txt:55-57
    ),
    diag_n = c(
      trust_government      = 2354,  # kendall_tau_output.txt:54
      trust_media           = 2367,  # kendall_tau_output.txt:57
      trust_science         = 2398   # kendall_tau_output.txt:60
    )
  ),

  # ---- Test 1d (kendall_tau_output.txt:62): Political orientation with trust variables
  test_1d = list(
    cells = list(
      np_cell("political_orientation", "trust_government",      -0.045,   0.011, 2168),  # kendall_tau_output.txt:73-76
      np_cell("political_orientation", "trust_media",            0.003,   0.883, 2177),  # kendall_tau_output.txt:73-76
      np_cell("political_orientation", "trust_science",          0.027,   0.129, 2202),  # kendall_tau_output.txt:73-76
      np_cell("trust_government",      "trust_media",            0.006,   0.722, 2227),  # kendall_tau_output.txt:77-80
      np_cell("trust_government",      "trust_science",          0.022,   0.205, 2255),  # kendall_tau_output.txt:77-80
      np_cell("trust_media",           "trust_science",          0.013,   0.455, 2272)   # kendall_tau_output.txt:81-84
    ),
    diag_n = c(
      political_orientation = 2299,  # kendall_tau_output.txt:76
      trust_government      = 2354,  # kendall_tau_output.txt:80
      trust_media           = 2367,  # kendall_tau_output.txt:84
      trust_science         = 2398   # kendall_tau_output.txt:88
    )
  ),

  # ---- Test 1e (kendall_tau_output.txt:91): Income and Age
  test_1e = list(
    cells = list(
      np_cell("income",                "age",                    0.002,   0.867, 2186)   # kendall_tau_output.txt:99-101
    ),
    diag_n = c(
      income                = 2186,  # kendall_tau_output.txt:101
      age                   = 2500   # kendall_tau_output.txt:104
    )
  ),

  # ---- Test 1f (kendall_tau_output.txt:106): Comprehensive variable set
  test_1f = list(
    cells = list(
      np_cell("life_satisfaction",     "income",                 0.353, "<.001", 2115),  # kendall_tau_output.txt:116-119
      np_cell("life_satisfaction",     "age",                   -0.018,   0.232, 2421),  # kendall_tau_output.txt:116-119
      np_cell("life_satisfaction",     "political_orientation", -0.004,   0.832, 2228),  # kendall_tau_output.txt:116-119
      np_cell("life_satisfaction",     "environmental_concern",  0.002,   0.903, 2324),  # kendall_tau_output.txt:116-119
      np_cell("income",                "age",                    0.002,   0.867, 2186),  # kendall_tau_output.txt:120-123
      np_cell("income",                "political_orientation", -0.022,   0.189, 2008),  # kendall_tau_output.txt:120-123
      np_cell("income",                "environmental_concern",  0.003,   0.868, 2097),  # kendall_tau_output.txt:120-123
      np_cell("age",                   "political_orientation", -0.027,   0.079, 2299),  # kendall_tau_output.txt:124-127
      np_cell("age",                   "environmental_concern",  0.015,   0.321, 2400),  # kendall_tau_output.txt:124-127
      np_cell("political_orientation", "environmental_concern", -0.486, "<.001", 2207)   # kendall_tau_output.txt:128-131
    ),
    diag_n = c(
      life_satisfaction     = 2421,  # kendall_tau_output.txt:119
      income                = 2186,  # kendall_tau_output.txt:123
      age                   = 2500,  # kendall_tau_output.txt:127
      political_orientation = 2299,  # kendall_tau_output.txt:131
      environmental_concern = 2400   # kendall_tau_output.txt:135
    )
  ),

  # ---- Test 3a (kendall_tau_output.txt:277): Life satisfaction, Political orientation, Trust media -- by region
  test_3a = list(
    East = list(
      cells = list(
        np_cell("life_satisfaction",     "political_orientation",  0.007,   0.851,  427),  # kendall_tau_output.txt:287-290
        np_cell("life_satisfaction",     "trust_media",           -0.049,   0.216,  440),  # kendall_tau_output.txt:287-290
        np_cell("political_orientation", "trust_media",            0.058,   0.153,  420)   # kendall_tau_output.txt:291-294
      ),
      diag_n = c(
        life_satisfaction     =  465,  # kendall_tau_output.txt:290
        political_orientation =  443,  # kendall_tau_output.txt:294
        trust_media           =  460   # kendall_tau_output.txt:298
      )
    ),
    West = list(
      cells = list(
        np_cell("life_satisfaction",     "political_orientation", -0.007,   0.733, 1801),  # kendall_tau_output.txt:299-302
        np_cell("life_satisfaction",     "trust_media",            0.040,   0.037, 1851),  # kendall_tau_output.txt:299-302
        np_cell("political_orientation", "trust_media",           -0.010,   0.608, 1757)   # kendall_tau_output.txt:303-306
      ),
      diag_n = c(
        life_satisfaction     = 1956,  # kendall_tau_output.txt:302
        political_orientation = 1856,  # kendall_tau_output.txt:306
        trust_media           = 1907   # kendall_tau_output.txt:310
      )
    )
  ),

  # ---- Test 3b (kendall_tau_output.txt:313): Income, Age, Life satisfaction -- by region
  test_3b = list(
    East = list(
      cells = list(
        np_cell("income",                "age",                    0.040,   0.227,  429),  # kendall_tau_output.txt:323-325
        np_cell("income",                "life_satisfaction",      0.338, "<.001",  410),  # kendall_tau_output.txt:323-325
        np_cell("age",                   "life_satisfaction",     -0.030,   0.380,  465)   # kendall_tau_output.txt:326-328
      ),
      diag_n = c(
        income                =  429,  # kendall_tau_output.txt:325
        age                   =  485,  # kendall_tau_output.txt:328
        life_satisfaction     =  465   # kendall_tau_output.txt:331
      )
    ),
    West = list(
      cells = list(
        np_cell("income",                "age",                   -0.006,   0.726, 1757),  # kendall_tau_output.txt:332-334
        np_cell("income",                "life_satisfaction",      0.357, "<.001", 1705),  # kendall_tau_output.txt:332-334
        np_cell("age",                   "life_satisfaction",     -0.015,   0.377, 1956)   # kendall_tau_output.txt:335-337
      ),
      diag_n = c(
        income                = 1757,  # kendall_tau_output.txt:334
        age                   = 2015,  # kendall_tau_output.txt:337
        life_satisfaction     = 1956   # kendall_tau_output.txt:340
      )
    )
  ),

  # ---- Test 3c (kendall_tau_output.txt:343): Trust variables -- by region
  test_3c = list(
    East = list(
      cells = list(
        np_cell("trust_government",      "trust_media",           -0.022,   0.570,  435),  # kendall_tau_output.txt:354-357
        np_cell("trust_government",      "trust_science",          0.003,   0.937,  444),  # kendall_tau_output.txt:354-357
        np_cell("trust_media",           "trust_science",          0.042,   0.284,  447)   # kendall_tau_output.txt:358-361
      ),
      diag_n = c(
        trust_government      =  460,  # kendall_tau_output.txt:357
        trust_media           =  460,  # kendall_tau_output.txt:361
        trust_science         =  469   # kendall_tau_output.txt:365
      )
    ),
    West = list(
      cells = list(
        np_cell("trust_government",      "trust_media",            0.013,   0.505, 1792),  # kendall_tau_output.txt:366-369
        np_cell("trust_government",      "trust_science",          0.027,   0.168, 1811),  # kendall_tau_output.txt:366-369
        np_cell("trust_media",           "trust_science",          0.006,   0.749, 1825)   # kendall_tau_output.txt:370-373
      ),
      diag_n = c(
        trust_government      = 1894,  # kendall_tau_output.txt:369
        trust_media           = 1907,  # kendall_tau_output.txt:373
        trust_science         = 1929   # kendall_tau_output.txt:377
      )
    )
  ),

  # ---- Test 3d (kendall_tau_output.txt:379): Income and Age -- by region
  test_3d = list(
    East = list(
      cells = list(
        np_cell("income",                "age",                    0.040,   0.227,  429)   # kendall_tau_output.txt:387-389
      ),
      diag_n = c(
        income                =  429,  # kendall_tau_output.txt:389
        age                   =  485   # kendall_tau_output.txt:392
      )
    ),
    West = list(
      cells = list(
        np_cell("income",                "age",                   -0.006,   0.726, 1757)   # kendall_tau_output.txt:393-395
      ),
      diag_n = c(
        income                = 1757,  # kendall_tau_output.txt:395
        age                   = 2015   # kendall_tau_output.txt:398
      )
    )
  ),

  # ---- Test 5a (kendall_tau_output.txt:527): Single pair correlation
  test_5a = list(
    cells = list(
      np_cell("life_satisfaction",     "political_orientation", -0.004,   0.832, 2228)   # kendall_tau_output.txt:536-538
    ),
    diag_n = c(
      life_satisfaction     = 2421,  # kendall_tau_output.txt:538
      political_orientation = 2299   # kendall_tau_output.txt:541
    )
  ),

  # ---- Test 5b (kendall_tau_output.txt:543): Listwise deletion
  test_5b_listwise = list(
    cells = list(
      np_cell("life_satisfaction",     "political_orientation",  0.000,   0.989, 2109),  # kendall_tau_output.txt:553-555, N :560
      np_cell("life_satisfaction",     "trust_media",            0.024,   0.173, 2109),  # kendall_tau_output.txt:553-555, N :560
      np_cell("political_orientation", "trust_media",            0.006,   0.745, 2109)   # kendall_tau_output.txt:556-557, N :560
    ),
    diag_n = c(
      life_satisfaction     = 2109,  # kendall_tau_output.txt:560
      political_orientation = 2109,  # kendall_tau_output.txt:560
      trust_media           = 2109   # kendall_tau_output.txt:560
    )
  ),

  # ---- Test 5c (kendall_tau_output.txt:562): One-tailed test
  test_5c_one_tailed = list(
    cells = list(
      np_cell("income",                "age",                    0.002,   0.433, 2186)   # kendall_tau_output.txt:570-572
    ),
    diag_n = c(
      income                = 2186,  # kendall_tau_output.txt:572
      age                   = 2500   # kendall_tau_output.txt:575
    )
  )
)


# =============================================================================
# COMPARISON HELPERS
# =============================================================================

# Asserts every cached cell of one SPSS matrix: tau, Sig. and pair N of the
# upper triangle, the diagonal N, and the variable order SPSS prints.
compare_kendall <- function(matrices, spss, scenario) {
  expect_identical(rownames(matrices$tau), names(spss$diag_n),
                   label = sprintf("[%s] variable order", scenario))
  for (cl in spss$cells) {
    lab <- sprintf("[%s | %s ~ %s]", scenario, cl$row, cl$col)
    assert_spss(matrices$tau[cl$row, cl$col], cl$coef,
                tier = "display", precision = 3,
                label = paste(lab, "tau"))
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
compare_kendall_grouped <- function(r, spss, scenario) {
  regions <- as.character(r$group_keys$region)
  expect_identical(regions, names(spss),
                   label = sprintf("[%s] region order", scenario))
  for (i in seq_along(r$matrices)) {
    compare_kendall(r$matrices[[i]], spss[[regions[i]]],
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

test_that("Test 1a: Kendall 3-var unweighted ungrouped — matches SPSS", {
  r <- survey_data |>
    kendall_tau(life_satisfaction, political_orientation, trust_media)
  compare_kendall(r$matrices[[1]], spss_values$test_1a,
                  "1a: unweighted ungrouped")
})

test_that("Test 1b: Kendall income/age/life_satisfaction — matches SPSS", {
  r <- survey_data |>
    kendall_tau(income, age, life_satisfaction)
  compare_kendall(r$matrices[[1]], spss_values$test_1b, "1b")
})

test_that("Test 1c: Kendall trust variables — matches SPSS", {
  r <- survey_data |>
    kendall_tau(trust_government, trust_media, trust_science)
  compare_kendall(r$matrices[[1]], spss_values$test_1c, "1c")
})

test_that("Test 1d: Kendall 4x4 political_orientation with trust — matches SPSS", {
  r <- survey_data |>
    kendall_tau(political_orientation, trust_government, trust_media,
                trust_science)
  compare_kendall(r$matrices[[1]], spss_values$test_1d, "1d")
})

test_that("Test 1e: Kendall income/age — matches SPSS", {
  r <- survey_data |>
    kendall_tau(income, age)
  compare_kendall(r$matrices[[1]], spss_values$test_1e, "1e")
})

test_that("Test 1f: Kendall 5x5 comprehensive set — matches SPSS", {
  r <- survey_data |>
    kendall_tau(life_satisfaction, income, age, political_orientation,
                environmental_concern)
  compare_kendall(r$matrices[[1]], spss_values$test_1f, "1f")
})


# =============================================================================
# SCENARIO 2 — WEIGHTED / UNGROUPED (R-only Tier-4 baseline)
# =============================================================================

test_that("Test 2a: Kendall weighted ungrouped — R-only Tier-4 baseline", {
  # mariposa kendall_tau DOES honor weights (different from spearman_rho
  # behaviour), but SPSS NONPAR CORR /KENDALL ignores WEIGHT BY. The two
  # implementations diverge meaningfully here. mariposa's weighted output
  # is a separate Tier-4 baseline.
  #
  # Re-captured 2026-07: the weighted tau-b denominator previously omitted
  # double-tied pairs (ties_both) from both factors, deflating |tau|. Fixed
  # to mirror the unweighted (n0 - Tx - Txy)(n0 - Ty - Txy) formula; the
  # weights == 1 reduction to unweighted tau is now guarded by the
  # invariance suite in test-weights-invariance.R. Weighted p remains a
  # documented no-ties approximation.
  r <- survey_data |>
    kendall_tau(life_satisfaction, political_orientation, trust_media,
                weights = sampling_weight)
  m <- r$matrices[[1]]
  baseline <- list(
    life_political_tau  = -0.004541,  life_political_p  = 0.7473,  life_political_n  = 2241L,
    life_trust_tau      = 0.022552,   life_trust_p      = 0.1046,  life_trust_n      = 2305L,
    political_trust_tau = 0.003664,   political_trust_p = 0.7972,  political_trust_n = 2190L,
    diag_n_life = 2437L, diag_n_pol = 2312L, diag_n_tm = 2382L
  )
  # Tier 4 (R-only regression baselines, no SPSS value): plain expects so
  # the compatibility vignette does not count them as SPSS assertions.
  # Weighted NONPAR CORR semantics are pending an SPSS WEIGHT BY run.
  expect_equal(round(m$tau["life_satisfaction","political_orientation"], 3),
               round(baseline$life_political_tau, 3))
  expect_equal(round(m$tau["life_satisfaction","trust_media"], 3),
               round(baseline$life_trust_tau, 3))
  expect_equal(round(m$tau["political_orientation","trust_media"], 3),
               round(baseline$political_trust_tau, 3))
  expect_equal(m$n_obs["life_satisfaction","political_orientation"],
               baseline$life_political_n)
})


# =============================================================================
# SCENARIO 3 — UNWEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 3a: Kendall unweighted grouped by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    kendall_tau(life_satisfaction, political_orientation, trust_media)
  compare_kendall_grouped(r, spss_values$test_3a, "3a: unweighted grouped")
})

test_that("Test 3b: Kendall income/age/life_satisfaction by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    kendall_tau(income, age, life_satisfaction)
  compare_kendall_grouped(r, spss_values$test_3b, "3b")
})

test_that("Test 3c: Kendall trust variables by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    kendall_tau(trust_government, trust_media, trust_science)
  compare_kendall_grouped(r, spss_values$test_3c, "3c")
})

test_that("Test 3d: Kendall income/age by region — matches SPSS", {
  r <- survey_data |>
    group_by(region) |>
    kendall_tau(income, age)
  compare_kendall_grouped(r, spss_values$test_3d, "3d")
})


# =============================================================================
# SCENARIO 4 — WEIGHTED / GROUPED by region (R-only Tier-4 baselines)
# =============================================================================

test_that("Test 4a: Kendall weighted grouped by region — R-only Tier-4 baselines", {
  r <- survey_data |>
    group_by(region) |>
    kendall_tau(life_satisfaction, political_orientation, trust_media,
                weights = sampling_weight)
  regions_in_order <- unique(r$correlations$region)

  # Region-ordered baselines, captured 2026-05-19; re-captured 2026-07
  # after the weighted tau-b double-tie denominator fix (see Test 2a).
  baselines <- list(
    East = list(life_political_tau = 0.005992, n_diag_life = 488L),
    West = list(life_political_tau = -0.007363, n_diag_life = 1949L)
  )

  for (i in seq_along(r$matrices)) {
    rg <- as.character(regions_in_order[i])
    m  <- r$matrices[[i]]
    b  <- baselines[[rg]]
    # Tier 4 (R-only baselines, see Test 2a)
    expect_equal(round(m$tau["life_satisfaction","political_orientation"], 3),
                 round(b$life_political_tau, 3))
    expect_equal(m$n_obs["life_satisfaction","life_satisfaction"],
                 b$n_diag_life)
  }
})


# =============================================================================
# EDGE CASES (Tests 5a-5c)
# =============================================================================

test_that("Test 5a: Kendall single pair (2x2) — matches SPSS", {
  r <- survey_data |>
    kendall_tau(life_satisfaction, political_orientation)
  compare_kendall(r$matrices[[1]], spss_values$test_5a, "5a: single pair")
})

test_that("Test 5b: Kendall listwise deletion — matches SPSS (Listwise N)", {
  # /MISSING=LISTWISE: SPSS prints no N rows, only "Listwise N = 2109",
  # which is the N of every cell (pairs and diagonal).
  r <- survey_data |>
    kendall_tau(life_satisfaction, political_orientation, trust_media,
                use = "listwise")
  compare_kendall(r$matrices[[1]], spss_values$test_5b_listwise,
                  "5b: listwise")
})

test_that("Test 5c: Kendall one-tailed — matches SPSS Sig. (1-tailed)", {
  # /PRINT=ONETAIL tests in the direction of the observed coefficient;
  # tau = .002 > 0, so the mariposa counterpart is alternative = "greater".
  r <- survey_data |>
    kendall_tau(income, age, alternative = "greater")
  compare_kendall(r$matrices[[1]], spss_values$test_5c_one_tailed,
                  "5c: one-tailed")
})
