# =============================================================================
# chi_square — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::chi_square() against SPSS v29 CROSSTABS chi².
# Reference output: tests/spss_reference/outputs/chi_squared_output.txt
#
# IMPORTANT: The SPSS reference output is mislabeled — both "unweighted"
# (Test 1*) and "weighted" (Test 2*) sections show identical values that
# correspond to WEIGHTED runs (N=2516, 2518, 2517 — matching weighted
# sums, not unweighted N=2500). The reference was apparently generated
# with WEIGHT BY active throughout. The citations below point at the
# Test 2* copies.
#
# Therefore: weighted scenarios validate against SPSS here; the unweighted
# gender x region table is validated against the unweighted CROSSTABS run
# in fisher_test_output.txt (same table, N = 2500). Tests 3*/4* (SPLIT FILE
# by region) are labelled correctly.
#
# Weighted cells: CROSSTABS with WEIGHT BY rounds every cell (/COUNT ROUND
# CELL), so the observed table, N, the expected counts and every statistic
# are computed from the rounded cells. Weighted counts are asserted at
# Display precision 0 (Charter §5, n from weighted data).
#
# Not asserted: Likelihood Ratio, Linear-by-Linear Association and the gamma
# ASE / approximate T (mariposa does not report them).
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(

  # ---- Test 2a: weighted gender x region (SPSS "Test 1a" repeats it) -----
  weighted_gender_region = list(
    observed = rbind(Male   = c(East = 249, West = 945),   # chi_squared_output.txt:111
                     Female = c(East = 260, West = 1062)), # chi_squared_output.txt:112
    chi_squared = 0.548,    # chi_squared_output.txt:118
    df = 1L,                # chi_squared_output.txt:118
    p = 0.459,              # chi_squared_output.txt:118
    continuity = 0.477,     # chi_squared_output.txt:119
    continuity_p = 0.490,   # chi_squared_output.txt:119
    n = 2516L,              # chi_squared_output.txt:123
    min_expected = 241.55,  # chi_squared_output.txt:124
    phi = 0.015,            # chi_squared_output.txt:130
    phi_p = 0.459,          # chi_squared_output.txt:130
    cramers_v = 0.015,      # chi_squared_output.txt:131
    cramers_v_p = 0.459,    # chi_squared_output.txt:131
    gamma = 0.037,          # chi_squared_output.txt:132
    gamma_p = 0.460         # chi_squared_output.txt:132
  ),

  # ---- Test 2b: weighted education x employment (4 x 5) -------------------
  weighted_education_employment = list(
    observed = rbind(
      `Basic Secondary`        = c(Student = 0,  Employed = 573, Unemployed = 66,   # chi_squared_output.txt:145
                                   Retired = 175, Other = 34),
      `Intermediate Secondary` = c(0,  420, 52, 139, 29),                         # chi_squared_output.txt:146
      `Academic Secondary`     = c(46, 370, 45, 149, 33),                         # chi_squared_output.txt:147
      University               = c(34, 240, 21, 72,  20)),                        # chi_squared_output.txt:148
    chi_squared = 130.696,  # chi_squared_output.txt:154
    df = 12L,               # chi_squared_output.txt:154
    p = "<.001",            # chi_squared_output.txt:154
    n = 2518L,              # chi_squared_output.txt:157
    min_expected = 12.30,   # chi_squared_output.txt:158
    phi = 0.228,            # chi_squared_output.txt:163
    phi_p = "<.001",        # chi_squared_output.txt:163
    cramers_v = 0.132,      # chi_squared_output.txt:164
    cramers_v_p = "<.001",  # chi_squared_output.txt:164
    gamma = -0.062,         # chi_squared_output.txt:165
    gamma_p = 0.027         # chi_squared_output.txt:165
  ),

  # ---- Test 2c: weighted gender x education (2 x 4) -----------------------
  weighted_gender_education = list(
    observed = rbind(
      Male   = c(`Basic Secondary` = 402, `Intermediate Secondary` = 291,   # chi_squared_output.txt:178
                 `Academic Secondary` = 326, University = 176),
      Female = c(447, 350, 316, 209)),                                      # chi_squared_output.txt:179
    chi_squared = 4.403,    # chi_squared_output.txt:185
    df = 3L,                # chi_squared_output.txt:185
    p = 0.221,              # chi_squared_output.txt:185
    n = 2517L,              # chi_squared_output.txt:188
    min_expected = 182.79,  # chi_squared_output.txt:189
    phi = 0.042,            # chi_squared_output.txt:194
    phi_p = 0.221,          # chi_squared_output.txt:194
    cramers_v = 0.042,      # chi_squared_output.txt:195
    cramers_v_p = 0.221,    # chi_squared_output.txt:195
    gamma = -0.011,         # chi_squared_output.txt:196
    gamma_p = 0.708         # chi_squared_output.txt:196
  ),

  # ---- Unweighted gender x region (CROSSTABS in the Fisher reference) ---
  unweighted_gender_region = list(
    chi_squared = 0.415,    # fisher_test_output.txt:27
    df = 1L,                # fisher_test_output.txt:27
    p = 0.519,              # fisher_test_output.txt:27
    continuity = 0.353,     # fisher_test_output.txt:28
    continuity_p = 0.553,   # fisher_test_output.txt:28
    n = 2500L,              # fisher_test_output.txt:32
    phi = 0.013,            # fisher_test_output.txt:39
    cramers_v = 0.013       # fisher_test_output.txt:40
  ),

  # ---- Test 3a: unweighted gender x education, SPLIT FILE region ----------
  grouped_gender_education = list(
    East = list(
      observed = rbind(
        Male   = c(`Basic Secondary` = 82, `Intermediate Secondary` = 60,  # chi_squared_output.txt:211
                   `Academic Secondary` = 59, University = 37),
        Female = c(88, 61, 56, 42)),                                       # chi_squared_output.txt:212
      chi_squared = 0.448,    # chi_squared_output.txt:221
      df = 3L,                # chi_squared_output.txt:221
      p = 0.930,              # chi_squared_output.txt:221
      n = 485L,               # chi_squared_output.txt:224
      min_expected = 38.77,   # chi_squared_output.txt:229
      phi = 0.030,            # chi_squared_output.txt:235
      phi_p = 0.930,          # chi_squared_output.txt:235
      cramers_v = 0.030,      # chi_squared_output.txt:236
      cramers_v_p = 0.930,    # chi_squared_output.txt:236
      gamma = -0.006,         # chi_squared_output.txt:237
      gamma_p = 0.930         # chi_squared_output.txt:237
    ),
    West = list(
      observed = rbind(
        Male   = c(`Basic Secondary` = 319, `Intermediate Secondary` = 229,  # chi_squared_output.txt:214
                   `Academic Secondary` = 261, University = 147),
        Female = c(352, 279, 255, 173)),                                     # chi_squared_output.txt:215
      chi_squared = 3.471,    # chi_squared_output.txt:225
      df = 3L,                # chi_squared_output.txt:225
      p = 0.325,              # chi_squared_output.txt:225
      n = 2015L,              # chi_squared_output.txt:228
      min_expected = 151.82,  # chi_squared_output.txt:230
      phi = 0.042,            # chi_squared_output.txt:239
      phi_p = 0.325,          # chi_squared_output.txt:239
      cramers_v = 0.042,      # chi_squared_output.txt:240
      cramers_v_p = 0.325,    # chi_squared_output.txt:240
      gamma = -0.009,         # chi_squared_output.txt:241
      gamma_p = 0.785         # chi_squared_output.txt:241
    )
  ),

  # ---- Test 3b: unweighted gender x employment, SPLIT FILE region ---------
  grouped_gender_employment = list(
    East = list(
      observed = rbind(
        Male   = c(Student = 7, Employed = 145, Unemployed = 16,   # chi_squared_output.txt:254
                   Retired = 55, Other = 15),
        Female = c(4, 166, 15, 56, 6)),                            # chi_squared_output.txt:255
      chi_squared = 5.970,    # chi_squared_output.txt:264
      df = 4L,                # chi_squared_output.txt:264
      p = 0.201,              # chi_squared_output.txt:264
      n = 485L,               # chi_squared_output.txt:267
      min_expected = 5.40,    # chi_squared_output.txt:272
      phi = 0.111,            # chi_squared_output.txt:278
      phi_p = 0.201,          # chi_squared_output.txt:278
      cramers_v = 0.111,      # chi_squared_output.txt:279
      cramers_v_p = 0.201,    # chi_squared_output.txt:279
      gamma = -0.093,         # chi_squared_output.txt:280
      gamma_p = 0.269         # chi_squared_output.txt:280
    ),
    West = list(
      observed = rbind(
        Male   = c(Student = 29, Employed = 605, Unemployed = 68,  # chi_squared_output.txt:257
                   Retired = 201, Other = 53),
        Female = c(38, 684, 83, 213, 41)),                         # chi_squared_output.txt:258
      chi_squared = 4.166,    # chi_squared_output.txt:268
      df = 4L,                # chi_squared_output.txt:268
      p = 0.384,              # chi_squared_output.txt:268
      n = 2015L,              # chi_squared_output.txt:271
      min_expected = 31.79,   # chi_squared_output.txt:273
      phi = 0.045,            # chi_squared_output.txt:282
      phi_p = 0.384,          # chi_squared_output.txt:282
      cramers_v = 0.045,      # chi_squared_output.txt:283
      cramers_v_p = 0.384,    # chi_squared_output.txt:283
      gamma = -0.053,         # chi_squared_output.txt:284
      gamma_p = 0.196         # chi_squared_output.txt:284
    )
  ),

  # ---- Test 4a: weighted gender x education, SPLIT FILE region ------------
  weighted_grouped_gender_education = list(
    East = list(
      observed = rbind(
        Male   = c(`Basic Secondary` = 83, `Intermediate Secondary` = 63,  # chi_squared_output.txt:299
                   `Academic Secondary` = 64, University = 39),
        Female = c(92, 66, 59, 43)),                                       # chi_squared_output.txt:300
      chi_squared = 0.694,    # chi_squared_output.txt:309
      df = 3L,                # chi_squared_output.txt:309
      p = 0.875,              # chi_squared_output.txt:309
      n = 509L,               # chi_squared_output.txt:312
      min_expected = 40.11,   # chi_squared_output.txt:317
      phi = 0.037,            # chi_squared_output.txt:323
      phi_p = 0.875,          # chi_squared_output.txt:323
      cramers_v = 0.037,      # chi_squared_output.txt:324
      cramers_v_p = 0.875,    # chi_squared_output.txt:324
      gamma = -0.026,         # chi_squared_output.txt:325
      gamma_p = 0.695         # chi_squared_output.txt:325
    ),
    West = list(
      observed = rbind(
        Male   = c(`Basic Secondary` = 318, `Intermediate Secondary` = 228,  # chi_squared_output.txt:302
                   `Academic Secondary` = 262, University = 137),
        Female = c(355, 284, 257, 166)),                                     # chi_squared_output.txt:303
      chi_squared = 4.176,    # chi_squared_output.txt:313
      df = 3L,                # chi_squared_output.txt:313
      p = 0.243,              # chi_squared_output.txt:313
      n = 2007L,              # chi_squared_output.txt:316
      min_expected = 142.67,  # chi_squared_output.txt:318
      phi = 0.046,            # chi_squared_output.txt:327
      phi_p = 0.243,          # chi_squared_output.txt:327
      cramers_v = 0.046,      # chi_squared_output.txt:328
      cramers_v_p = 0.243,    # chi_squared_output.txt:328
      gamma = -0.009,         # chi_squared_output.txt:329
      gamma_p = 0.799         # chi_squared_output.txt:329
    )
  ),

  # ---- Test 4b: weighted gender x employment, SPLIT FILE region -----------
  weighted_grouped_gender_employment = list(
    East = list(
      observed = rbind(
        Male   = c(Student = 7, Employed = 149, Unemployed = 17,   # chi_squared_output.txt:342
                   Retired = 61, Other = 15),
        Female = c(4, 172, 16, 61, 6)),                            # chi_squared_output.txt:343
      chi_squared = 6.159,    # chi_squared_output.txt:352
      df = 4L,                # chi_squared_output.txt:352
      p = 0.188,              # chi_squared_output.txt:352
      n = 508L,               # chi_squared_output.txt:355
      min_expected = 5.39,    # chi_squared_output.txt:360
      phi = 0.110,            # chi_squared_output.txt:366
      phi_p = 0.188,          # chi_squared_output.txt:366
      cramers_v = 0.110,      # chi_squared_output.txt:367
      cramers_v_p = 0.188,    # chi_squared_output.txt:367
      gamma = -0.099,         # chi_squared_output.txt:368
      gamma_p = 0.225         # chi_squared_output.txt:368
    ),
    West = list(
      observed = rbind(
        Male   = c(Student = 29, Employed = 599, Unemployed = 66,  # chi_squared_output.txt:345
                   Retired = 199, Other = 53),
        Female = c(40, 683, 85, 212, 41)),                         # chi_squared_output.txt:346
      chi_squared = 5.018,    # chi_squared_output.txt:356
      df = 4L,                # chi_squared_output.txt:356
      p = 0.285,              # chi_squared_output.txt:356
      n = 2007L,              # chi_squared_output.txt:359
      min_expected = 32.52,   # chi_squared_output.txt:361
      phi = 0.050,            # chi_squared_output.txt:370
      phi_p = 0.285,          # chi_squared_output.txt:370
      cramers_v = 0.050,      # chi_squared_output.txt:371
      cramers_v_p = 0.285,    # chi_squared_output.txt:371
      gamma = -0.054,         # chi_squared_output.txt:372
      gamma_p = 0.182         # chi_squared_output.txt:372
    )
  )
)


data(survey_data, envir = environment())


# -----------------------------------------------------------------------------
# Helper: assert one chi_square() result row against one SPSS block.
# Every field present in `spss` is asserted; `weighted` switches counts from
# exact (Spec) to Display precision 0 (Charter §5: n from weighted data).
# Precisions are what SPSS prints: statistics/phi/V/gamma/p 3 dp, the
# minimum expected count (table footnote) 2 dp.
# -----------------------------------------------------------------------------
assert_chi_row <- function(res, spss, label, weighted = FALSE) {
  assert_n <- function(actual, expected, what) {
    if (weighted) {
      assert_spss(actual, expected, tier = "display", precision = 0,
                  label = sprintf("%s %s", label, what))
    } else {
      assert_spss_count(actual, expected, label = sprintf("%s %s", label, what))
    }
  }

  if (!is.null(spss$observed)) {
    obs <- res$observed[[1]]
    expect_identical(dimnames(obs)[[1]], rownames(spss$observed),
                     label = sprintf("%s row category order", label))
    expect_identical(dimnames(obs)[[2]], colnames(spss$observed),
                     label = sprintf("%s column category order", label))
    for (i in seq_len(nrow(spss$observed))) {
      for (j in seq_len(ncol(spss$observed))) {
        assert_n(as.numeric(obs[i, j]), spss$observed[i, j],
                 sprintf("observed [%s, %s]", rownames(spss$observed)[i],
                         colnames(spss$observed)[j]))
      }
    }
  }

  assert_n(as.numeric(res$n), spss$n, "N of valid cases")
  assert_spss_count(as.numeric(res$df), spss$df, label = sprintf("%s df", label))

  if (!is.null(spss$min_expected)) {
    assert_spss(min(res$expected[[1]]), spss$min_expected,
                tier = "display", precision = 2,
                label = sprintf("%s minimum expected count", label))
  }

  stats <- c(chi_squared = "chi_squared", phi = "phi",
             cramers_v = "cramers_v", gamma = "gamma")
  for (key in names(stats)) {
    if (is.null(spss[[key]])) next
    assert_spss(as.numeric(res[[stats[[key]]]]), spss[[key]],
                tier = "display", precision = 3,
                label = sprintf("%s %s", label, key))
  }

  pvals <- c(p = "p_value", phi_p = "phi_p_value",
             cramers_v_p = "cramers_v_p_value", gamma_p = "gamma_p_value")
  for (key in names(pvals)) {
    if (is.null(spss[[key]])) next
    assert_spss(as.numeric(res[[pvals[[key]]]]), spss[[key]],
                tier = "display", precision = 3, what = "p_value",
                label = sprintf("%s %s", label, key))
  }
}


test_that("Test 2a: chi_square gender × region weighted — matches SPSS", {
  spss <- spss_values$weighted_gender_region
  r <- survey_data |> chi_square(gender, region, weights = sampling_weight)
  assert_chi_row(r$results[1, ], spss, "[2a]", weighted = TRUE)

  # Continuity Correction row (2x2 only): the corrected chi² and its p.
  cc <- (survey_data |>
           chi_square(gender, region, weights = sampling_weight,
                      correct = TRUE))$results[1, ]
  assert_spss(as.numeric(cc$chi_squared), spss$continuity, tier = "display",
              precision = 3, label = "[2a] continuity correction")
  assert_spss(as.numeric(cc$p_value), spss$continuity_p, tier = "display",
              precision = 3, what = "p_value", label = "[2a] continuity p")
})


test_that("Test 2b: chi_square education × employment weighted — matches SPSS", {
  r <- survey_data |> chi_square(education, employment, weights = sampling_weight)
  assert_chi_row(r$results[1, ], spss_values$weighted_education_employment,
                 "[2b]", weighted = TRUE)
})


test_that("Test 2c: chi_square gender × education weighted — matches SPSS", {
  r <- survey_data |> chi_square(gender, education, weights = sampling_weight)
  assert_chi_row(r$results[1, ], spss_values$weighted_gender_education,
                 "[2c]", weighted = TRUE)
})


test_that("Test 1a: chi_square gender × region unweighted — matches SPSS", {
  spss <- spss_values$unweighted_gender_region
  res <- (survey_data |> chi_square(gender, region))$results[1, ]
  assert_spss(as.numeric(res$chi_squared), spss$chi_squared,
              tier = "display", precision = 3, label = "[1a] chi²")
  assert_spss_count(as.numeric(res$df), spss$df, label = "[1a] df")
  assert_spss(as.numeric(res$p_value), spss$p, tier = "display",
              precision = 3, what = "p_value", label = "[1a] p-value")
  assert_spss_count(as.numeric(res$n), spss$n, label = "[1a] N")
  assert_spss(as.numeric(res$phi), spss$phi, tier = "display",
              precision = 3, label = "[1a] phi")
  assert_spss(as.numeric(res$cramers_v), spss$cramers_v, tier = "display",
              precision = 3, label = "[1a] Cramer's V")
  cc <- (survey_data |> chi_square(gender, region, correct = TRUE))$results[1, ]
  assert_spss(as.numeric(cc$chi_squared), spss$continuity, tier = "display",
              precision = 3, label = "[1a] continuity correction")
  assert_spss(as.numeric(cc$p_value), spss$continuity_p, tier = "display",
              precision = 3, what = "p_value", label = "[1a] continuity p")
})


# -----------------------------------------------------------------------------
# Grouped scenarios (SPSS SPLIT FILE by region; East before West)
# -----------------------------------------------------------------------------
assert_chi_grouped <- function(r, spss, label, weighted = FALSE) {
  expect_identical(as.character(r$results$region), names(spss),
                   label = sprintf("%s split-file group order", label))
  for (g in names(spss)) {
    row <- r$results[as.character(r$results$region) == g, , drop = FALSE]
    expect_equal(nrow(row), 1L, label = sprintf("%s %s rows", label, g))
    assert_chi_row(row, spss[[g]], sprintf("%s %s", label, g), weighted)
  }
}


test_that("Test 3a: chi_square gender × education grouped by region — matches SPSS", {
  r <- survey_data |> group_by(region) |> chi_square(gender, education)
  assert_chi_grouped(r, spss_values$grouped_gender_education, "[3a]")
})


test_that("Test 3b: chi_square gender × employment grouped by region — matches SPSS", {
  r <- survey_data |> group_by(region) |> chi_square(gender, employment)
  assert_chi_grouped(r, spss_values$grouped_gender_employment, "[3b]")
})


test_that("Test 4a: chi_square gender × education weighted, grouped by region — matches SPSS", {
  r <- survey_data |> group_by(region) |>
    chi_square(gender, education, weights = sampling_weight)
  assert_chi_grouped(r, spss_values$weighted_grouped_gender_education, "[4a]",
                     weighted = TRUE)
})


test_that("Test 4b: chi_square gender × employment weighted, grouped by region — matches SPSS", {
  r <- survey_data |> group_by(region) |>
    chi_square(gender, employment, weights = sampling_weight)
  assert_chi_grouped(r, spss_values$weighted_grouped_gender_employment, "[4b]",
                     weighted = TRUE)
})
