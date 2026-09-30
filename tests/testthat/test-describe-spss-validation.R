# =============================================================================
# describe — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::describe() against SPSS v29 FREQUENCIES /STATS.
# Reference output: tests/spss_reference/outputs/describe_output.txt
#
# Tests show = "all" mode, which exposes every statistic SPSS prints in the
# FREQUENCIES Statistics table: N (Valid), Missing, Mean, SE, Median, Mode,
# SD, Variance, Skewness, Kurtosis, Range, Minimum, Maximum and the
# 25/50/75 percentiles.
#
# Coverage: all four scenarios for age, income and life_satisfaction
#   1a-1c unweighted/ungrouped, 2a-2c weighted/ungrouped,
#   3a-3c unweighted/grouped by region, 4a-4c weighted/grouped by region.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(

  # ---- Test 1: Unweighted Ungrouped (3 variables) ---------------------
  # describe_output.txt:11-26 (Test 1a)
  test_1a_age = list(
    n = 2500L, missing = 0L,
    mean = 50.5496, se = 0.33952, median = 50.0000, mode = 18.00,
    sd = 16.97602, variance = 288.185, skewness = 0.172, kurtosis = -0.364,
    range = 77.00, min = 18.00, max = 95.00,
    q25 = 38.0000, q50 = 50.0000, q75 = 62.0000
  ),
  # describe_output.txt:36-51 (Test 1b)
  test_1b_income = list(
    n = 2186L, missing = 314L,
    mean = 3753.9341, se = 30.64510, median = 3500.0000, mode = 3200.00,
    sd = 1432.80161, variance = 2052920.442, skewness = 0.730, kurtosis = 0.376,
    range = 7200.00, min = 800.00, max = 8000.00,
    q25 = 2700.0000, q50 = 3500.0000, q75 = 4600.0000
  ),
  # describe_output.txt:61-76 (Test 1c)
  test_1c_life_sat = list(
    n = 2421L, missing = 79L,
    mean = 3.63, se = 0.023, median = 4.00, mode = 4,
    sd = 1.153, variance = 1.330, skewness = -0.501, kurtosis = -0.602,
    range = 4, min = 1, max = 5,
    q25 = 3.00, q50 = 4.00, q75 = 5.00
  ),

  # ---- Test 2: Weighted Ungrouped -------------------------------------
  # describe_output.txt:88-103 (Test 2a)
  test_2a_age_weighted = list(
    n = 2516L, missing = 0L,
    mean = 50.5144, se = 0.34058, median = 50.0000, mode = 18.00,
    sd = 17.08382, variance = 291.857, skewness = 0.159, kurtosis = -0.396,
    range = 77.00, min = 18.00, max = 95.00,
    q25 = 38.0000, q50 = 50.0000, q75 = 63.0000
  ),
  # describe_output.txt:113-128 (Test 2b). Missing is the weighted missing
  # count (sum of the weights of cases without income), printed rounded.
  test_2b_income_weighted = list(
    n = 2201L, missing = 315L,
    mean = 3743.0994, se = 30.35257, median = 3500.0000, mode = 3200.00,
    sd = 1423.96558, variance = 2027677.966, skewness = 0.725, kurtosis = 0.388,
    range = 7200.00, min = 800.00, max = 8000.00,
    q25 = 2700.0000, q50 = 3500.0000, q75 = 4600.0000
  ),
  # describe_output.txt:138-153 (Test 2c)
  test_2c_life_sat_weighted = list(
    n = 2437L, missing = 79L,
    mean = 3.62, se = 0.023, median = 4.00, mode = 4,
    sd = 1.152, variance = 1.327, skewness = -0.499, kurtosis = -0.598,
    range = 4, min = 1, max = 5,
    q25 = 3.00, q50 = 4.00, q75 = 5.00
  ),

  # ---- Test 3: Unweighted, grouped by region --------------------------
  test_3a_age_grouped = list(
    # describe_output.txt:165-180 (Test 3a East)
    East = list(n = 485L, missing = 0L,
                mean = 51.8680, se = 0.79101, median = 52.0000, mode = 18.00,
                sd = 17.42028, variance = 303.466, skewness = 0.148,
                kurtosis = -0.337, range = 77.00, min = 18.00, max = 95.00,
                q25 = 39.0000, q50 = 52.0000, q75 = 63.0000),
    # describe_output.txt:181-196 (Test 3a West)
    West = list(n = 2015L, missing = 0L,
                mean = 50.2323, se = 0.37551, median = 49.0000, mode = 18.00,
                sd = 16.85635, variance = 284.137, skewness = 0.175,
                kurtosis = -0.373, range = 77.00, min = 18.00, max = 95.00,
                q25 = 38.0000, q50 = 49.0000, q75 = 62.0000)
  ),
  test_3b_income_grouped = list(
    # describe_output.txt:206-221 (Test 3b East)
    East = list(n = 429L, missing = 56L,
                mean = 3752.4476, se = 66.95917, median = 3600.0000,
                mode = 3800.00, sd = 1386.87938, variance = 1923434.416,
                skewness = 0.729, kurtosis = 0.489,
                range = 7200.00, min = 800.00, max = 8000.00,
                q25 = 2800.0000, q50 = 3600.0000, q75 = 4500.0000),
    # describe_output.txt:222-237 (Test 3b West)
    West = list(n = 1757L, missing = 258L,
                mean = 3754.2971, se = 34.45361, median = 3500.0000,
                mode = 3100.00, sd = 1444.17770, variance = 2085649.235,
                skewness = 0.731, kurtosis = 0.353,
                range = 7200.00, min = 800.00, max = 8000.00,
                q25 = 2700.0000, q50 = 3500.0000, q75 = 4600.0000)
  ),
  test_3c_life_sat_grouped = list(
    # describe_output.txt:247-262 (Test 3c East)
    East = list(n = 465L, missing = 20L,
                mean = 3.62, se = 0.056, median = 4.00, mode = 4,
                sd = 1.207, variance = 1.456, skewness = -0.552,
                kurtosis = -0.631, range = 4, min = 1, max = 5,
                q25 = 3.00, q50 = 4.00, q75 = 5.00),
    # describe_output.txt:263-278 (Test 3c West)
    West = list(n = 1956L, missing = 59L,
                mean = 3.63, se = 0.026, median = 4.00, mode = 4,
                sd = 1.140, variance = 1.300, skewness = -0.486,
                kurtosis = -0.600, range = 4, min = 1, max = 5,
                q25 = 3.00, q50 = 4.00, q75 = 5.00)
  ),

  # ---- Test 4: Weighted, grouped by region ----------------------------
  test_4a_age_weighted_grouped = list(
    # describe_output.txt:290-305 (Test 4a East)
    East = list(n = 509L, missing = 0L,
                mean = 52.2778, se = 0.77988, median = 53.0000, mode = 18.00,
                sd = 17.59548, variance = 309.601, skewness = 0.098,
                kurtosis = -0.389, range = 77.00, min = 18.00, max = 95.00,
                q25 = 40.0000, q50 = 53.0000, q75 = 64.0000),
    # describe_output.txt:306-321 (Test 4a West)
    West = list(n = 2007L, missing = 0L,
                mean = 50.0672, se = 0.37783, median = 49.0000, mode = 18.00,
                sd = 16.92689, variance = 286.520, skewness = 0.170,
                kurtosis = -0.396, range = 77.00, min = 18.00, max = 95.00,
                q25 = 38.0000, q50 = 49.0000, q75 = 62.0000)
  ),
  test_4b_income_weighted_grouped = list(
    # describe_output.txt:331-346 (Test 4b East)
    East = list(n = 449L, missing = 60L,
                mean = 3760.6866, se = 65.48281, median = 3600.0000,
                mode = 3800.00, sd = 1388.32120, variance = 1927435.755,
                skewness = 0.721, kurtosis = 0.502,
                range = 7200.00, min = 800.00, max = 8000.00,
                q25 = 2800.0000, q50 = 3600.0000, q75 = 4500.0000),
    # describe_output.txt:347-362 (Test 4b West)
    West = list(n = 1751L, missing = 256L,
                mean = 3738.5858, se = 34.24889, median = 3500.0000,
                mode = 3100.00, sd = 1433.32495, variance = 2054420.399,
                skewness = 0.727, kurtosis = 0.364,
                range = 7200.00, min = 800.00, max = 8000.00,
                q25 = 2700.0000, q50 = 3500.0000, q75 = 4600.0000)
  ),
  test_4c_life_sat_weighted_grouped = list(
    # describe_output.txt:372-387 (Test 4c East)
    East = list(n = 488L, missing = 21L,
                mean = 3.62, se = 0.054, median = 4.00, mode = 4,
                sd = 1.203, variance = 1.448, skewness = -0.558,
                kurtosis = -0.616, range = 4, min = 1, max = 5,
                q25 = 3.00, q50 = 4.00, q75 = 5.00),
    # describe_output.txt:388-403 (Test 4c West)
    West = list(n = 1949L, missing = 58L,
                mean = 3.63, se = 0.026, median = 4.00, mode = 4,
                sd = 1.139, variance = 1.298, skewness = -0.481,
                kurtosis = -0.598, range = 4, min = 1, max = 5,
                q25 = 3.00, q50 = 4.00, q75 = 5.00)
  )
)


compare_describe <- function(row, spss, var, scenario, is_weighted = FALSE) {
  pfx <- function(field) paste0(var, "_", field)
  # N: Spec(integer) for unweighted; Display(0) for weighted (non-integer
  # internally, SPSS displays as integer)
  if (!is.null(spss$n)) {
    if (is_weighted) {
      assert_spss(as.numeric(row[[pfx("N")]]), spss$n,
                  tier = "display", precision = 0,
                  label = sprintf("[%s] N (weighted)", scenario))
    } else {
      assert_spss_count(as.numeric(row[[pfx("N")]]), spss$n,
                        label = sprintf("[%s] N", scenario))
    }
  }
  if (!is.null(spss$missing)) {
    if (is_weighted) {
      # Weighted missing = sum of weights, displayed rounded by SPSS
      assert_spss(as.numeric(row[[pfx("Missing")]]), spss$missing,
                  tier = "display", precision = 0,
                  label = sprintf("[%s] Missing (weighted)", scenario))
    } else {
      assert_spss_count(as.numeric(row[[pfx("Missing")]]), spss$missing,
                        label = sprintf("[%s] Missing", scenario))
    }
  }

  # Numeric stats: variable precision per SPSS print precision
  cmp <- function(field, expected, precision) {
    if (is.null(expected) || is.na(expected)) return(invisible(NULL))
    actual <- as.numeric(row[[pfx(field)]])
    assert_spss(actual, expected,
                tier = "display", precision = precision,
                label = sprintf("[%s] %s", scenario, field))
  }
  # Precision per variable (life_sat at 2-3 dp; age/income at 4-5 dp).
  # Mode, Range, Minimum and Maximum are data values; they are asserted at
  # the variable's mean precision, which is at least the number of decimals
  # SPSS prints for them (age/income "77.00", life_sat "4").
  prec_main <- if (var == "life_satisfaction") 2 else 4
  prec_sd   <- if (var == "life_satisfaction") 3 else 5
  prec_q    <- if (var == "life_satisfaction") 2 else 4

  cmp("Mean",     spss$mean,     prec_main)
  cmp("SE",       spss$se,       prec_sd)
  cmp("Median",   spss$median,   prec_main)
  cmp("Mode",     spss$mode,     prec_main)
  cmp("SD",       spss$sd,       prec_sd)
  cmp("Variance", spss$variance, 3)
  cmp("Skewness", spss$skewness, 3)
  # Kurtosis: SPSS prints 3 decimals for every variable
  cmp("Kurtosis", spss$kurtosis, 3)
  cmp("Range",    spss$range,    prec_main)
  cmp("Min",      spss$min,      prec_main)
  cmp("Max",      spss$max,      prec_main)
  cmp("Q25",      spss$q25,      prec_q)
  cmp("Q50",      spss$q50,      prec_q)
  cmp("Q75",      spss$q75,      prec_q)
}

# Grouped scenarios: SPSS prints East before West; every region row is
# compared against its own SPSS block.
compare_describe_grouped <- function(r, spss, var, scenario,
                                     is_weighted = FALSE) {
  expect_identical(as.character(r$results$region), c("East", "West"),
                   label = sprintf("[%s] region order", scenario))
  for (i in seq_len(nrow(r$results))) {
    rg <- as.character(r$results$region[i])
    compare_describe(r$results[i, ], spss[[rg]], var,
                     sprintf("%s [%s]", scenario, rg),
                     is_weighted = is_weighted)
  }
}


data(survey_data, envir = environment())


# =============================================================================
# SCENARIO 1 — UNWEIGHTED / UNGROUPED
# =============================================================================

test_that("Test 1a: describe age unweighted — matches SPSS", {
  r <- survey_data |> describe(age, show = "all")
  compare_describe(r$results[1, ], spss_values$test_1a_age, "age",
                   "1a: age unweighted")
})

test_that("Test 1b: describe income unweighted — matches SPSS", {
  r <- survey_data |> describe(income, show = "all")
  compare_describe(r$results[1, ], spss_values$test_1b_income, "income",
                   "1b: income unweighted")
})

test_that("Test 1c: describe life_satisfaction unweighted — matches SPSS", {
  r <- survey_data |> describe(life_satisfaction, show = "all")
  compare_describe(r$results[1, ], spss_values$test_1c_life_sat,
                   "life_satisfaction", "1c: life_sat unweighted")
})


# =============================================================================
# SCENARIO 2 — WEIGHTED / UNGROUPED
# =============================================================================

test_that("Test 2a: describe age weighted — matches SPSS", {
  r <- survey_data |> describe(age, weights = sampling_weight, show = "all")
  # Weighted N is integer-rounded in SPSS but mariposa keeps non-integer.
  # Weighted skew/kurtosis now use Type-2 formula (via .calc_kurtosis /
  # .calc_skewness in helpers.R), matching w_kurtosis() / w_skew() and SPSS.
  compare_describe(r$results[1, ], spss_values$test_2a_age_weighted,
                   "age", "2a: age weighted", is_weighted = TRUE)
})

test_that("Test 2b: describe income weighted — matches SPSS (incl. weighted Missing)", {
  r <- survey_data |> describe(income, weights = sampling_weight, show = "all")
  compare_describe(r$results[1, ], spss_values$test_2b_income_weighted,
                   "income", "2b: income weighted", is_weighted = TRUE)
})

test_that("Test 2c: describe life_satisfaction weighted — matches SPSS", {
  r <- survey_data |>
    describe(life_satisfaction, weights = sampling_weight, show = "all")
  compare_describe(r$results[1, ], spss_values$test_2c_life_sat_weighted,
                   "life_satisfaction", "2c: life_sat weighted",
                   is_weighted = TRUE)
})


# =============================================================================
# SCENARIO 3 — UNWEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 3a: describe age grouped by region — matches SPSS", {
  r <- survey_data |> group_by(region) |> describe(age, show = "all")
  compare_describe_grouped(r, spss_values$test_3a_age_grouped, "age",
                           "3a: age")
})

test_that("Test 3b: describe income grouped by region — matches SPSS", {
  r <- survey_data |> group_by(region) |> describe(income, show = "all")
  compare_describe_grouped(r, spss_values$test_3b_income_grouped, "income",
                           "3b: income")
})

test_that("Test 3c: describe life_satisfaction grouped by region — matches SPSS", {
  r <- survey_data |> group_by(region) |>
    describe(life_satisfaction, show = "all")
  compare_describe_grouped(r, spss_values$test_3c_life_sat_grouped,
                           "life_satisfaction", "3c: life_sat")
})


# =============================================================================
# SCENARIO 4 — WEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 4a: describe age weighted, grouped by region — matches SPSS", {
  r <- survey_data |> group_by(region) |>
    describe(age, weights = sampling_weight, show = "all")
  compare_describe_grouped(r, spss_values$test_4a_age_weighted_grouped,
                           "age", "4a: age weighted", is_weighted = TRUE)
})

test_that("Test 4b: describe income weighted, grouped by region — matches SPSS", {
  r <- survey_data |> group_by(region) |>
    describe(income, weights = sampling_weight, show = "all")
  compare_describe_grouped(r, spss_values$test_4b_income_weighted_grouped,
                           "income", "4b: income weighted", is_weighted = TRUE)
})

test_that("Test 4c: describe life_satisfaction weighted, grouped by region — matches SPSS", {
  r <- survey_data |> group_by(region) |>
    describe(life_satisfaction, weights = sampling_weight, show = "all")
  compare_describe_grouped(r, spss_values$test_4c_life_sat_weighted_grouped,
                           "life_satisfaction", "4c: life_sat weighted",
                           is_weighted = TRUE)
})
