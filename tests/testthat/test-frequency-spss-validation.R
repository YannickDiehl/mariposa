# =============================================================================
# frequency — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::frequency() against SPSS v29 FREQUENCIES.
# Reference output: tests/spss_reference/outputs/frequencies_output.txt
#
# SPSS FREQUENCIES honors WEIGHT BY: weighted counts and percentages differ.
# All 4 scenarios are SPSS-validatable.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(

  test_1_unweighted = list(
    rows = list(
      list(value = 1, freq = 118L, prc = 4.7, valid_prc = 4.9, cum_prc = 4.9),    # line 7
      list(value = 2, freq = 306L, prc = 12.2, valid_prc = 12.6, cum_prc = 17.5),
      list(value = 3, freq = 600L, prc = 24.0, valid_prc = 24.8, cum_prc = 42.3),
      list(value = 4, freq = 731L, prc = 29.2, valid_prc = 30.2, cum_prc = 72.5),
      list(value = 5, freq = 666L, prc = 26.6, valid_prc = 27.5, cum_prc = 100.0)
    ),
    missing_freq = 79L,
    total_freq = 2500L
  ),

  test_2_weighted = list(
    rows = list(
      list(value = 1, freq = 119L),
      list(value = 2, freq = 307L),
      list(value = 3, freq = 607L),
      list(value = 4, freq = 737L),
      list(value = 5, freq = 666L)
    ),
    missing_freq = 79L,
    total_freq = 2516L
  ),

  test_3_unweighted_grouped = list(
    East = list(
      rows = list(
        list(value = 1, freq = 30L), list(value = 2, freq = 58L),
        list(value = 3, freq = 106L), list(value = 4, freq = 136L),
        list(value = 5, freq = 135L)
      ),
      missing_freq = 20L, total_freq = 485L
    ),
    West = list(
      rows = list(
        list(value = 1, freq = 88L), list(value = 2, freq = 248L),
        list(value = 3, freq = 494L), list(value = 4, freq = 595L),
        list(value = 5, freq = 531L)
      ),
      missing_freq = 59L, total_freq = 2015L
    )
  ),

  test_4_weighted_grouped = list(
    East = list(
      rows = list(
        list(value = 1, freq = 31L), list(value = 2, freq = 60L),
        list(value = 3, freq = 111L), list(value = 4, freq = 144L),
        list(value = 5, freq = 141L)
      ),
      missing_freq = 21L, total_freq = 509L
    ),
    West = list(
      rows = list(
        list(value = 1, freq = 88L), list(value = 2, freq = 247L),
        list(value = 3, freq = 496L), list(value = 4, freq = 593L),
        list(value = 5, freq = 524L)
      ),
      missing_freq = 58L, total_freq = 2007L
    )
  )
)


compare_freq <- function(freq_df, spss, scenario, is_weighted = FALSE) {
  # Filter to non-NA value rows
  valid_rows <- freq_df[!is.na(freq_df$value), ]
  na_rows    <- freq_df[is.na(freq_df$value), ]

  for (expected_row in spss$rows) {
    val <- expected_row$value
    actual <- valid_rows[valid_rows$value == val, ]
    if (nrow(actual) != 1) {
      stop(sprintf("[%s] missing row for value=%s", scenario, val), call. = FALSE)
    }
    if (is_weighted) {
      assert_spss(as.numeric(actual$freq), expected_row$freq,
                  tier = "display", precision = 0,
                  label = sprintf("[%s] value=%s freq", scenario, val))
    } else {
      assert_spss_count(as.numeric(actual$freq), expected_row$freq,
                        label = sprintf("[%s] value=%s freq", scenario, val))
    }
    # Percentages
    if (!is.null(expected_row$prc)) {
      assert_spss(as.numeric(actual$prc), expected_row$prc,
                  tier = "display", precision = 1,
                  label = sprintf("[%s] value=%s prc", scenario, val))
    }
    if (!is.null(expected_row$valid_prc)) {
      assert_spss(as.numeric(actual$valid_prc), expected_row$valid_prc,
                  tier = "display", precision = 1,
                  label = sprintf("[%s] value=%s valid_prc", scenario, val))
    }
    if (!is.null(expected_row$cum_prc)) {
      assert_spss(as.numeric(actual$cum_prc), expected_row$cum_prc,
                  tier = "display", precision = 1,
                  label = sprintf("[%s] value=%s cum_prc", scenario, val))
    }
  }

  # Missing count
  if (!is.null(spss$missing_freq) && nrow(na_rows) > 0) {
    if (is_weighted) {
      assert_spss(as.numeric(na_rows$freq[1]), spss$missing_freq,
                  tier = "display", precision = 0,
                  label = sprintf("[%s] missing freq", scenario))
    } else {
      assert_spss_count(as.numeric(na_rows$freq[1]), spss$missing_freq,
                        label = sprintf("[%s] missing freq", scenario))
    }
  }
}


extract_grouped_subset <- function(result, region) {
  r <- result$results
  rows <- r[as.character(r$region) == region, ]
  rows
}


data(survey_data, envir = environment())


test_that("Test 1: frequency life_satisfaction unweighted — matches SPSS", {
  r <- survey_data |> frequency(life_satisfaction)
  compare_freq(r$results, spss_values$test_1_unweighted, "1: life_sat unweighted")
})

test_that("Test 2: frequency life_satisfaction weighted — matches SPSS", {
  r <- survey_data |> frequency(life_satisfaction, weights = sampling_weight)
  compare_freq(r$results, spss_values$test_2_weighted, "2: life_sat weighted",
               is_weighted = TRUE)
})

test_that("Test 3: frequency life_satisfaction grouped by region — matches SPSS", {
  r <- survey_data |> group_by(region) |> frequency(life_satisfaction)
  for (rg in c("East", "West")) {
    rows <- extract_grouped_subset(r, rg)
    compare_freq(rows, spss_values$test_3_unweighted_grouped[[rg]],
                 sprintf("3: life_sat [%s]", rg))
  }
})

test_that("Test 4: frequency life_satisfaction weighted+grouped — matches SPSS", {
  r <- survey_data |> group_by(region) |>
    frequency(life_satisfaction, weights = sampling_weight)
  for (rg in c("East", "West")) {
    rows <- extract_grouped_subset(r, rg)
    compare_freq(rows, spss_values$test_4_weighted_grouped[[rg]],
                 sprintf("4: life_sat weighted [%s]", rg), is_weighted = TRUE)
  }
})
