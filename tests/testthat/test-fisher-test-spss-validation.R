# =============================================================================
# fisher_test — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::fisher_test() against SPSS v29 CROSSTABS
#          Fisher's Exact row, and the CROSSTABS tables printed with it
#          (Count / Expected Count / row, column and total percentages via
#          crosstab(); Pearson chi², Continuity Correction, Phi, Cramer's V
#          and the minimum expected count via chi_square()).
# Reference output: tests/spss_reference/outputs/fisher_test_output.txt
#
# mariposa fisher_test exposes the two-sided p_value and N (SPSS's one-sided
# exact p has no counterpart). SPSS prints Fisher's exact test for 2x2 tables
# only, so the 2x3 Fisher p (Test 1b/2b) has no reference value. CROSSTABS
# honors WEIGHT BY (/COUNT ROUND CELL), so all 4 scenarios are validatable.
# Test 1a's Pearson/continuity/Phi/V values are asserted in
# test-chi-squared-spss-validation.R (unweighted_gender_region).
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(
  # ---- Test 1a: gender × region 2×2 unweighted ----
  test_1a = list(
    p_exact_2sided = 0.544,   # fisher_test_output.txt:30
    n = 2500L,                # fisher_test_output.txt:32
    min_expected = 231.64,    # fisher_test_output.txt:33
    phi_p = 0.519,            # fisher_test_output.txt:39
    cramers_v_p = 0.519       # fisher_test_output.txt:40
  ),

  # ---- Test 1b: gender × interview_mode 2×3 unweighted (no Fisher row) ----
  test_1b = list(
    chi_squared = 0.792,      # fisher_test_output.txt:69
    df = 2L,                  # fisher_test_output.txt:69
    chi_p = 0.673,            # fisher_test_output.txt:69
    n = 2500L,                # fisher_test_output.txt:72
    min_expected = 171.94,    # fisher_test_output.txt:73
    phi = 0.018,              # fisher_test_output.txt:78
    phi_p = 0.673,            # fisher_test_output.txt:78
    cramers_v = 0.018,        # fisher_test_output.txt:79
    cramers_v_p = 0.673       # fisher_test_output.txt:79
  ),

  # ---- Test 2a: gender × region 2×2 weighted ------
  test_2a = list(
    p_exact_2sided = 0.487,   # fisher_test_output.txt:153
    n = 2516L                 # fisher_test_output.txt:155
  ),

  # ---- Test 2b: gender × interview_mode 2×3 weighted (no Fisher row) ----
  test_2b = list(
    chi_squared = 0.585,      # fisher_test_output.txt:192
    df = 2L,                  # fisher_test_output.txt:192
    chi_p = 0.746,            # fisher_test_output.txt:192
    n = 2516L,                # fisher_test_output.txt:195
    min_expected = 171.79,    # fisher_test_output.txt:196
    phi = 0.015,              # fisher_test_output.txt:201
    phi_p = 0.746,            # fisher_test_output.txt:201
    cramers_v = 0.015,        # fisher_test_output.txt:202
    cramers_v_p = 0.746       # fisher_test_output.txt:202
  ),

  # ---- Test 1c: gender × region, SELECT IF id <= 50 ------
  # A negative association: SPSS prints Phi of a 2x2 table with its sign
  test_1c = list(
    p_exact_2sided = 0.734,   # fisher_test_output.txt:111
    n = 50L,                  # fisher_test_output.txt:113
    chi_squared = 0.334,      # fisher_test_output.txt:108
    chi_p = 0.563,            # fisher_test_output.txt:108
    continuity = 0.055,       # fisher_test_output.txt:109
    continuity_p = 0.815,     # fisher_test_output.txt:109
    min_expected = 4.84,      # fisher_test_output.txt:114
    phi = -0.082,             # fisher_test_output.txt:120
    phi_p = 0.563,            # fisher_test_output.txt:120
    cramers_v = 0.082,        # fisher_test_output.txt:121
    cramers_v_p = 0.563       # fisher_test_output.txt:121
  ),

  # ---- Test 3: gender × region, SPLIT FILE education (levels 1 and 4) ----
  test_3 = list(
    `Basic Secondary` = list(
      chi_squared = 0.026,    # fisher_test_output.txt:248
      df = 1L,                # fisher_test_output.txt:248
      chi_p = 0.871,          # fisher_test_output.txt:248
      continuity = 0.006,     # fisher_test_output.txt:249
      continuity_p = 0.939,   # fisher_test_output.txt:249
      p_exact_2sided = 0.932, # fisher_test_output.txt:251
      n = 841L,               # fisher_test_output.txt:253
      min_expected = 81.06,   # fisher_test_output.txt:260
      phi = 0.006,            # fisher_test_output.txt:267
      phi_p = 0.871,          # fisher_test_output.txt:267
      cramers_v = 0.006,      # fisher_test_output.txt:268
      cramers_v_p = 0.871     # fisher_test_output.txt:268
    ),
    University = list(
      chi_squared = 0.021,    # fisher_test_output.txt:254
      df = 1L,                # fisher_test_output.txt:254
      chi_p = 0.886,          # fisher_test_output.txt:254
      continuity = 0.000,     # fisher_test_output.txt:255
      continuity_p = 0.986,   # fisher_test_output.txt:255
      p_exact_2sided = 0.900, # fisher_test_output.txt:257
      n = 399L,               # fisher_test_output.txt:259
      min_expected = 36.43,   # fisher_test_output.txt:261
      phi = 0.007,            # fisher_test_output.txt:270
      phi_p = 0.886,          # fisher_test_output.txt:270
      cramers_v = 0.007,      # fisher_test_output.txt:271
      cramers_v_p = 0.886     # fisher_test_output.txt:271
    )
  ),

  # ---- Test 4: weighted, SPLIT FILE education (levels 1 and 4) ----
  test_4 = list(
    `Basic Secondary` = list(
      chi_squared = 0.002,    # fisher_test_output.txt:317
      df = 1L,                # fisher_test_output.txt:317
      chi_p = 0.967,          # fisher_test_output.txt:317
      continuity = 0.000,     # fisher_test_output.txt:318
      continuity_p = 1.000,   # fisher_test_output.txt:318
      p_exact_2sided = 1.000, # fisher_test_output.txt:320
      n = 848L,               # fisher_test_output.txt:322
      min_expected = 82.75,   # fisher_test_output.txt:329
      phi = 0.001,            # fisher_test_output.txt:336
      phi_p = 0.967,          # fisher_test_output.txt:336
      cramers_v = 0.001,      # fisher_test_output.txt:337
      cramers_v_p = 0.967     # fisher_test_output.txt:337
    ),
    University = list(
      chi_squared = 0.143,    # fisher_test_output.txt:323
      df = 1L,                # fisher_test_output.txt:323
      chi_p = 0.705,          # fisher_test_output.txt:323
      continuity = 0.064,     # fisher_test_output.txt:324
      continuity_p = 0.800,   # fisher_test_output.txt:324
      p_exact_2sided = 0.710, # fisher_test_output.txt:326
      n = 385L,               # fisher_test_output.txt:328
      min_expected = 37.49,   # fisher_test_output.txt:330
      phi = 0.019,            # fisher_test_output.txt:339
      phi_p = 0.705,          # fisher_test_output.txt:339
      cramers_v = 0.019,      # fisher_test_output.txt:340
      cramers_v_p = 0.705     # fisher_test_output.txt:340
    )
  ),

  # ===========================================================================
  # CROSSTABS cells (Count, Expected Count, % within Gender, % within the
  # column variable, % of Total). Layout: see helper-crosstab-cells.R.
  # ===========================================================================

  # ---- Test 1a crosstabulation ----
  cells_1a = list(
    all = list(
      count = c(238, 956, 1194,    # fisher_test_output.txt:8
                247, 1059, 1306,   # fisher_test_output.txt:13
                485, 2015, 2500),  # fisher_test_output.txt:18
      expected = c(231.6, 962.4,    # fisher_test_output.txt:9
                   253.4, 1052.6),  # fisher_test_output.txt:14
      row_pct = c(19.9, 80.1,   # fisher_test_output.txt:10
                  18.9, 81.1,   # fisher_test_output.txt:15
                  19.4, 80.6),  # fisher_test_output.txt:20
      col_pct = c(49.1, 47.4, 47.8,   # fisher_test_output.txt:11
                  50.9, 52.6, 52.2),  # fisher_test_output.txt:16
      total_pct = c(9.5, 38.2, 47.8,  # fisher_test_output.txt:12
                    9.9, 42.4, 52.2,  # fisher_test_output.txt:17
                    19.4, 80.6)       # fisher_test_output.txt:22
    )
  ),

  # ---- Test 1b crosstabulation ----
  cells_1b = list(
    all = list(
      count = c(720, 307, 167, 1194,    # fisher_test_output.txt:50
                765, 348, 193, 1306,    # fisher_test_output.txt:55
                1485, 655, 360, 2500),  # fisher_test_output.txt:60
      expected = c(709.2, 312.8, 171.9,   # fisher_test_output.txt:51
                   775.8, 342.2, 188.1),  # fisher_test_output.txt:56
      row_pct = c(60.3, 25.7, 14.0,   # fisher_test_output.txt:52
                  58.6, 26.6, 14.8,   # fisher_test_output.txt:57
                  59.4, 26.2, 14.4),  # fisher_test_output.txt:62
      col_pct = c(48.5, 46.9, 46.4, 47.8,   # fisher_test_output.txt:53
                  51.5, 53.1, 53.6, 52.2),  # fisher_test_output.txt:58
      total_pct = c(28.8, 12.3, 6.7, 47.8,  # fisher_test_output.txt:54
                    30.6, 13.9, 7.7, 52.2,  # fisher_test_output.txt:59
                    59.4, 26.2, 14.4)       # fisher_test_output.txt:64
    )
  ),

  # ---- Test 1c crosstabulation ----
  cells_1c = list(
    all = list(
      count = c(4, 18, 22,    # fisher_test_output.txt:89
                7, 21, 28,    # fisher_test_output.txt:94
                11, 39, 50),  # fisher_test_output.txt:99
      expected = c(4.8, 17.2,   # fisher_test_output.txt:90
                   6.2, 21.8),  # fisher_test_output.txt:95
      row_pct = c(18.2, 81.8,   # fisher_test_output.txt:91
                  25.0, 75.0,   # fisher_test_output.txt:96
                  22.0, 78.0),  # fisher_test_output.txt:101
      col_pct = c(36.4, 46.2, 44.0,   # fisher_test_output.txt:92
                  63.6, 53.8, 56.0),  # fisher_test_output.txt:97
      total_pct = c(8.0, 36.0, 44.0,   # fisher_test_output.txt:93
                    14.0, 42.0, 56.0,  # fisher_test_output.txt:98
                    22.0, 78.0)        # fisher_test_output.txt:103
    )
  ),

  # ---- Test 2a crosstabulation (weighted) ----
  cells_2a = list(
    all = list(
      count = c(249, 945, 1194,    # fisher_test_output.txt:131
                260, 1062, 1322,   # fisher_test_output.txt:136
                509, 2007, 2516),  # fisher_test_output.txt:141
      expected = c(241.6, 952.4,    # fisher_test_output.txt:132
                   267.4, 1054.6),  # fisher_test_output.txt:137
      row_pct = c(20.9, 79.1,   # fisher_test_output.txt:133
                  19.7, 80.3,   # fisher_test_output.txt:138
                  20.2, 79.8),  # fisher_test_output.txt:143
      col_pct = c(48.9, 47.1, 47.5,   # fisher_test_output.txt:134
                  51.1, 52.9, 52.5),  # fisher_test_output.txt:139
      total_pct = c(9.9, 37.6, 47.5,   # fisher_test_output.txt:135
                    10.3, 42.2, 52.5,  # fisher_test_output.txt:140
                    20.2, 79.8)        # fisher_test_output.txt:145
    )
  ),

  # ---- Test 2b crosstabulation (weighted) ----
  cells_2b = list(
    all = list(
      count = c(719, 308, 167, 1194,    # fisher_test_output.txt:173
                777, 350, 195, 1322,    # fisher_test_output.txt:178
                1496, 658, 362, 2516),  # fisher_test_output.txt:183
      expected = c(709.9, 312.3, 171.8,   # fisher_test_output.txt:174
                   786.1, 345.7, 190.2),  # fisher_test_output.txt:179
      row_pct = c(60.2, 25.8, 14.0,   # fisher_test_output.txt:175
                  58.8, 26.5, 14.8,   # fisher_test_output.txt:180
                  59.5, 26.2, 14.4),  # fisher_test_output.txt:185
      col_pct = c(48.1, 46.8, 46.1, 47.5,   # fisher_test_output.txt:176
                  51.9, 53.2, 53.9, 52.5),  # fisher_test_output.txt:181
      total_pct = c(28.6, 12.2, 6.6, 47.5,  # fisher_test_output.txt:177
                    30.9, 13.9, 7.8, 52.5,  # fisher_test_output.txt:182
                    59.5, 26.2, 14.4)       # fisher_test_output.txt:187
    )
  ),

  # ---- Test 3 crosstabulation (SPLIT FILE education) ----
  cells_3 = list(
    `Basic Secondary` = list(
      count = c(82, 319, 401,    # fisher_test_output.txt:212
                88, 352, 440,    # fisher_test_output.txt:217
                170, 671, 841),  # fisher_test_output.txt:222
      expected = c(81.1, 319.9,   # fisher_test_output.txt:213
                   88.9, 351.1),  # fisher_test_output.txt:218
      row_pct = c(20.4, 79.6,   # fisher_test_output.txt:214
                  20.0, 80.0,   # fisher_test_output.txt:219
                  20.2, 79.8),  # fisher_test_output.txt:224
      col_pct = c(48.2, 47.5, 47.7,   # fisher_test_output.txt:215
                  51.8, 52.5, 52.3),  # fisher_test_output.txt:220
      total_pct = c(9.8, 37.9, 47.7,   # fisher_test_output.txt:216
                    10.5, 41.9, 52.3,  # fisher_test_output.txt:221
                    20.2, 79.8)        # fisher_test_output.txt:226
    ),
    University = list(
      count = c(37, 147, 184,   # fisher_test_output.txt:227
                42, 173, 215,   # fisher_test_output.txt:232
                79, 320, 399),  # fisher_test_output.txt:237
      expected = c(36.4, 147.6,   # fisher_test_output.txt:228
                   42.6, 172.4),  # fisher_test_output.txt:233
      row_pct = c(20.1, 79.9,   # fisher_test_output.txt:229
                  19.5, 80.5,   # fisher_test_output.txt:234
                  19.8, 80.2),  # fisher_test_output.txt:239
      col_pct = c(46.8, 45.9, 46.1,   # fisher_test_output.txt:230
                  53.2, 54.1, 53.9),  # fisher_test_output.txt:235
      total_pct = c(9.3, 36.8, 46.1,   # fisher_test_output.txt:231
                    10.5, 43.4, 53.9,  # fisher_test_output.txt:236
                    19.8, 80.2)        # fisher_test_output.txt:241
    )
  ),

  # ---- Test 4 crosstabulation (weighted, SPLIT FILE education) ----
  cells_4 = list(
    `Basic Secondary` = list(
      count = c(83, 318, 401,    # fisher_test_output.txt:281
                92, 355, 447,    # fisher_test_output.txt:286
                175, 673, 848),  # fisher_test_output.txt:291
      expected = c(82.8, 318.2,   # fisher_test_output.txt:282
                   92.2, 354.8),  # fisher_test_output.txt:287
      row_pct = c(20.7, 79.3,   # fisher_test_output.txt:283
                  20.6, 79.4,   # fisher_test_output.txt:288
                  20.6, 79.4),  # fisher_test_output.txt:293
      col_pct = c(47.4, 47.3, 47.3,   # fisher_test_output.txt:284
                  52.6, 52.7, 52.7),  # fisher_test_output.txt:289
      total_pct = c(9.8, 37.5, 47.3,   # fisher_test_output.txt:285
                    10.8, 41.9, 52.7,  # fisher_test_output.txt:290
                    20.6, 79.4)        # fisher_test_output.txt:295
    ),
    University = list(
      count = c(39, 137, 176,   # fisher_test_output.txt:296
                43, 166, 209,   # fisher_test_output.txt:301
                82, 303, 385),  # fisher_test_output.txt:306
      expected = c(37.5, 138.5,   # fisher_test_output.txt:297
                   44.5, 164.5),  # fisher_test_output.txt:302
      row_pct = c(22.2, 77.8,   # fisher_test_output.txt:298
                  20.6, 79.4,   # fisher_test_output.txt:303
                  21.3, 78.7),  # fisher_test_output.txt:308
      col_pct = c(47.6, 45.2, 45.7,   # fisher_test_output.txt:299
                  52.4, 54.8, 54.3),  # fisher_test_output.txt:304
      total_pct = c(10.1, 35.6, 45.7,  # fisher_test_output.txt:300
                    11.2, 43.1, 54.3,  # fisher_test_output.txt:305
                    21.3, 78.7)        # fisher_test_output.txt:310
    )
  )
)


data(survey_data, envir = environment())

# SPLIT FILE education with only levels 1 and 4 (as the SPSS syntax selects)
edu_1_4 <- dplyr::filter(survey_data, education %in% c("Basic Secondary", "University"))


# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

# Assert the chi_square() side of one CROSSTABS block: every field present in
# `spss` among chi_squared/df/chi_p/n/min_expected/phi(+p)/cramers_v(+p), and
# the Continuity Correction row (2x2 only) from a correct = TRUE run.
# Weighted N is asserted at Display(0) (Charter §5).
assert_crosstabs_stats <- function(res, spss, label, res_cc = NULL,
                                   weighted = FALSE) {
  if (!is.null(spss$n)) {
    if (weighted) {
      assert_spss(as.numeric(res$n), spss$n, tier = "display", precision = 0,
                  label = sprintf("%s N", label))
    } else {
      assert_spss_count(as.numeric(res$n), spss$n, label = sprintf("%s N", label))
    }
  }
  if (!is.null(spss$df)) {
    assert_spss_count(as.numeric(res$df), spss$df, label = sprintf("%s df", label))
  }
  if (!is.null(spss$min_expected)) {
    assert_spss(min(res$expected[[1]]), spss$min_expected, tier = "display",
                precision = 2, label = sprintf("%s minimum expected count", label))
  }
  for (key in c("chi_squared", "phi", "cramers_v")) {
    if (is.null(spss[[key]])) next
    col <- if (key == "chi_squared") "pearson_chi_squared" else key
    assert_spss(as.numeric(res[[col]]), spss[[key]], tier = "display",
                precision = 3, label = sprintf("%s %s", label, key))
  }
  pcols <- c(chi_p = "pearson_p_value", phi_p = "phi_p_value",
             cramers_v_p = "cramers_v_p_value")
  for (key in names(pcols)) {
    if (is.null(spss[[key]])) next
    assert_spss(as.numeric(res[[pcols[[key]]]]), spss[[key]], tier = "display",
                precision = 3, what = "p_value", label = sprintf("%s %s", label, key))
  }
  if (!is.null(spss$continuity)) {
    assert_spss(as.numeric(res_cc$chi_squared), spss$continuity, tier = "display",
                precision = 3, label = sprintf("%s continuity correction", label))
    assert_spss(as.numeric(res_cc$p_value), spss$continuity_p, tier = "display",
                precision = 3, what = "p_value",
                label = sprintf("%s continuity correction p", label))
  }
}


# -----------------------------------------------------------------------------
# Tests
# -----------------------------------------------------------------------------

test_that("Test 1a: Fisher gender × region unweighted — matches SPSS", {
  r <- survey_data |> fisher_test(gender, region)
  assert_spss(as.numeric(r$results$p_value), spss_values$test_1a$p_exact_2sided,
              tier = "display", precision = 3, what = "p_value",
              label = "[1a] Fisher p-value (2-sided)")
  assert_spss_count(as.numeric(r$results$n), spss_values$test_1a$n,
                    label = "[1a] N")

  cs <- (survey_data |> chi_square(gender, region))$results
  assert_crosstabs_stats(cs, spss_values$test_1a[c("min_expected", "phi_p",
                                                   "cramers_v_p")], "[1a]")
  assert_xt_cells(crosstab(survey_data, gender, region, percentages = "all"),
                  spss_values$cells_1a$all, "[1a]")
})

test_that("Test 1b: gender × interview_mode 2x3 unweighted — CROSSTABS matches SPSS", {
  cs <- (survey_data |> chi_square(gender, interview_mode))$results
  assert_crosstabs_stats(cs, spss_values$test_1b, "[1b]")
  assert_xt_cells(crosstab(survey_data, gender, interview_mode, percentages = "all"),
                  spss_values$cells_1b$all, "[1b]")
})

test_that("Test 2a: Fisher gender × region weighted — matches SPSS", {
  r <- survey_data |> fisher_test(gender, region, weights = sampling_weight)
  assert_spss(as.numeric(r$results$p_value), spss_values$test_2a$p_exact_2sided,
              tier = "display", precision = 3, what = "p_value",
              label = "[2a] Fisher p-value weighted")
  assert_spss(as.numeric(r$results$n), spss_values$test_2a$n,
              tier = "display", precision = 0,
              label = "[2a] N weighted")
  assert_xt_cells(crosstab(survey_data, gender, region, weights = sampling_weight,
                           percentages = "all"),
                  spss_values$cells_2a$all, "[2a]")
})

test_that("Test 2b: gender × interview_mode 2x3 weighted — CROSSTABS matches SPSS", {
  cs <- (survey_data |>
           chi_square(gender, interview_mode, weights = sampling_weight))$results
  assert_crosstabs_stats(cs, spss_values$test_2b, "[2b]", weighted = TRUE)
  assert_xt_cells(crosstab(survey_data, gender, interview_mode,
                           weights = sampling_weight, percentages = "all"),
                  spss_values$cells_2b$all, "[2b]")
})

test_that("Test 1c: small subset — Fisher p and signed Phi match SPSS", {
  small <- dplyr::filter(survey_data, id <= 50)
  r <- fisher_test(small, gender, region)
  assert_spss(as.numeric(r$results$p_value), spss_values$test_1c$p_exact_2sided,
              tier = "display", precision = 3, what = "p_value",
              label = "[1c] Fisher p-value (2-sided)")
  assert_spss_count(as.numeric(r$results$n), spss_values$test_1c$n,
                    label = "[1c] N")
  cs <- suppressWarnings(chi_square(small, gender, region))$results
  assert_spss(cs$pearson_chi_squared, spss_values$test_1c$chi_squared,
              tier = "display", precision = 3, label = "[1c] Pearson chi-square")
  assert_spss(cs$pearson_p_value, spss_values$test_1c$chi_p,
              tier = "display", precision = 3, what = "p_value",
              label = "[1c] Pearson p")
  assert_spss(cs$phi, spss_values$test_1c$phi,
              tier = "display", precision = 3, label = "[1c] Phi (signed)")
  assert_spss(cs$cramers_v, spss_values$test_1c$cramers_v,
              tier = "display", precision = 3, label = "[1c] Cramer's V")
  assert_spss(suppressWarnings(phi(small, gender, region)),
              spss_values$test_1c$phi, tier = "display", precision = 3,
              label = "[1c] phi()")

  cc <- suppressWarnings(chi_square(small, gender, region, correct = TRUE))$results
  assert_crosstabs_stats(cs, spss_values$test_1c[c("min_expected", "phi_p",
                                                   "cramers_v_p", "continuity",
                                                   "continuity_p")],
                         "[1c]", res_cc = cc)
  assert_xt_cells(crosstab(small, gender, region, percentages = "all"),
                  spss_values$cells_1c$all, "[1c]")
})

test_that("Test 3: Fisher gender × region grouped by education — matches SPSS", {
  spss <- spss_values$test_3
  r  <- edu_1_4 |> group_by(education) |> fisher_test(gender, region)
  cs <- (edu_1_4 |> group_by(education) |> chi_square(gender, region))$results
  cc <- (edu_1_4 |> group_by(education) |>
           chi_square(gender, region, correct = TRUE))$results
  xt <- edu_1_4 |> group_by(education) |> crosstab(gender, region, percentages = "all")

  expect_identical(as.character(r$results$education), names(spss),
                   label = "[3] split-file group order")
  for (g in names(spss)) {
    lab <- sprintf("[3 %s]", g)
    fr <- r$results[as.character(r$results$education) == g, ]
    assert_spss(as.numeric(fr$p_value), spss[[g]]$p_exact_2sided, tier = "display",
                precision = 3, what = "p_value", label = paste(lab, "Fisher p (2-sided)"))
    assert_spss_count(as.numeric(fr$n), spss[[g]]$n, label = paste(lab, "Fisher N"))
    assert_crosstabs_stats(cs[as.character(cs$education) == g, ], spss[[g]], lab,
                           res_cc = cc[as.character(cc$education) == g, ])
    assert_xt_cells(xt_layer(xt, education = g), spss_values$cells_3[[g]], lab)
  }
})

test_that("Test 4: Fisher gender × region weighted, grouped by education — matches SPSS", {
  spss <- spss_values$test_4
  r  <- edu_1_4 |> group_by(education) |>
    fisher_test(gender, region, weights = sampling_weight)
  cs <- (edu_1_4 |> group_by(education) |>
           chi_square(gender, region, weights = sampling_weight))$results
  cc <- (edu_1_4 |> group_by(education) |>
           chi_square(gender, region, weights = sampling_weight,
                      correct = TRUE))$results
  xt <- edu_1_4 |> group_by(education) |>
    crosstab(gender, region, weights = sampling_weight, percentages = "all")

  expect_identical(as.character(r$results$education), names(spss),
                   label = "[4] split-file group order")
  for (g in names(spss)) {
    lab <- sprintf("[4 %s]", g)
    fr <- r$results[as.character(r$results$education) == g, ]
    assert_spss(as.numeric(fr$p_value), spss[[g]]$p_exact_2sided, tier = "display",
                precision = 3, what = "p_value", label = paste(lab, "Fisher p (2-sided)"))
    assert_spss(as.numeric(fr$n), spss[[g]]$n, tier = "display", precision = 0,
                label = paste(lab, "Fisher N"))
    assert_crosstabs_stats(cs[as.character(cs$education) == g, ], spss[[g]], lab,
                           res_cc = cc[as.character(cc$education) == g, ],
                           weighted = TRUE)
    assert_xt_cells(xt_layer(xt, education = g), spss_values$cells_4[[g]], lab)
  }
})

test_that("Edge case: Fisher 2x3 table returns numeric p-value", {
  # SPSS prints no Fisher row for a 2x3 table (Test 1b), so there is no
  # reference value for this p; structural check only.
  r <- suppressWarnings(fisher_test(survey_data, gender, interview_mode))
  expect_true(as.numeric(r$results$p_value) > 0 &&
              as.numeric(r$results$p_value) < 1)
  expect_true(as.numeric(r$results$n) > 0)
})
