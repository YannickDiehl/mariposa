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


# Per value row: Frequency, Percent, Valid Percent, Cumulative Percent.
# Weighted rows: SPSS prints the weighted frequency rounded to an integer and
# computes the percentages from the unrounded weighted counts.
spss_values <- list(

  test_1_unweighted = list(
    rows = list(
      list(value = 1, freq = 118L, prc = 4.7, valid_prc = 4.9, cum_prc = 4.9),     # frequencies_output.txt:7
      list(value = 2, freq = 306L, prc = 12.2, valid_prc = 12.6, cum_prc = 17.5),  # frequencies_output.txt:8
      list(value = 3, freq = 600L, prc = 24.0, valid_prc = 24.8, cum_prc = 42.3),  # frequencies_output.txt:9
      list(value = 4, freq = 731L, prc = 29.2, valid_prc = 30.2, cum_prc = 72.5),  # frequencies_output.txt:10
      list(value = 5, freq = 666L, prc = 26.6, valid_prc = 27.5, cum_prc = 100.0)  # frequencies_output.txt:11
    ),
    valid_freq = 2421,        # frequencies_output.txt:12
    valid_prc = 96.8,         # frequencies_output.txt:12
    missing_freq = 79,        # frequencies_output.txt:13
    missing_prc = 3.2,        # frequencies_output.txt:13
    total_freq = 2500         # frequencies_output.txt:14
  ),

  test_2_weighted = list(
    rows = list(
      list(value = 1, freq = 119L, prc = 4.7, valid_prc = 4.9, cum_prc = 4.9),     # frequencies_output.txt:22
      list(value = 2, freq = 307L, prc = 12.2, valid_prc = 12.6, cum_prc = 17.5),  # frequencies_output.txt:23
      list(value = 3, freq = 607L, prc = 24.1, valid_prc = 24.9, cum_prc = 42.4),  # frequencies_output.txt:24
      list(value = 4, freq = 737L, prc = 29.3, valid_prc = 30.3, cum_prc = 72.7),  # frequencies_output.txt:25
      list(value = 5, freq = 666L, prc = 26.5, valid_prc = 27.3, cum_prc = 100.0)  # frequencies_output.txt:26
    ),
    valid_freq = 2437,        # frequencies_output.txt:27
    valid_prc = 96.8,         # frequencies_output.txt:27
    missing_freq = 79,        # frequencies_output.txt:28
    missing_prc = 3.2,        # frequencies_output.txt:28
    total_freq = 2516         # frequencies_output.txt:29
  ),

  test_3_unweighted_grouped = list(
    East = list(
      rows = list(
        list(value = 1, freq = 30L, prc = 6.2, valid_prc = 6.5, cum_prc = 6.5),        # frequencies_output.txt:37
        list(value = 2, freq = 58L, prc = 12.0, valid_prc = 12.5, cum_prc = 18.9),     # frequencies_output.txt:38
        list(value = 3, freq = 106L, prc = 21.9, valid_prc = 22.8, cum_prc = 41.7),    # frequencies_output.txt:39
        list(value = 4, freq = 136L, prc = 28.0, valid_prc = 29.2, cum_prc = 71.0),    # frequencies_output.txt:40
        list(value = 5, freq = 135L, prc = 27.8, valid_prc = 29.0, cum_prc = 100.0)    # frequencies_output.txt:41
      ),
      valid_freq = 465,       # frequencies_output.txt:42
      valid_prc = 95.9,       # frequencies_output.txt:42
      missing_freq = 20,      # frequencies_output.txt:43
      missing_prc = 4.1,      # frequencies_output.txt:43
      total_freq = 485        # frequencies_output.txt:44
    ),
    West = list(
      rows = list(
        list(value = 1, freq = 88L, prc = 4.4, valid_prc = 4.5, cum_prc = 4.5),        # frequencies_output.txt:45
        list(value = 2, freq = 248L, prc = 12.3, valid_prc = 12.7, cum_prc = 17.2),    # frequencies_output.txt:46
        list(value = 3, freq = 494L, prc = 24.5, valid_prc = 25.3, cum_prc = 42.4),    # frequencies_output.txt:47
        list(value = 4, freq = 595L, prc = 29.5, valid_prc = 30.4, cum_prc = 72.9),    # frequencies_output.txt:48
        list(value = 5, freq = 531L, prc = 26.4, valid_prc = 27.1, cum_prc = 100.0)    # frequencies_output.txt:49
      ),
      valid_freq = 1956,      # frequencies_output.txt:50
      valid_prc = 97.1,       # frequencies_output.txt:50
      missing_freq = 59,      # frequencies_output.txt:51
      missing_prc = 2.9,      # frequencies_output.txt:51
      total_freq = 2015       # frequencies_output.txt:52
    )
  ),

  test_4_weighted_grouped = list(
    East = list(
      rows = list(
        list(value = 1, freq = 31L, prc = 6.1, valid_prc = 6.4, cum_prc = 6.4),        # frequencies_output.txt:60
        list(value = 2, freq = 60L, prc = 11.8, valid_prc = 12.3, cum_prc = 18.7),     # frequencies_output.txt:61
        list(value = 3, freq = 111L, prc = 21.8, valid_prc = 22.8, cum_prc = 41.5),    # frequencies_output.txt:62
        list(value = 4, freq = 144L, prc = 28.3, valid_prc = 29.5, cum_prc = 71.0),    # frequencies_output.txt:63
        list(value = 5, freq = 141L, prc = 27.8, valid_prc = 29.0, cum_prc = 100.0)    # frequencies_output.txt:64
      ),
      valid_freq = 488,       # frequencies_output.txt:65
      valid_prc = 95.9,       # frequencies_output.txt:65
      missing_freq = 21,      # frequencies_output.txt:66
      missing_prc = 4.1,      # frequencies_output.txt:66
      total_freq = 509        # frequencies_output.txt:67
    ),
    West = list(
      rows = list(
        list(value = 1, freq = 88L, prc = 4.4, valid_prc = 4.5, cum_prc = 4.5),        # frequencies_output.txt:68
        list(value = 2, freq = 247L, prc = 12.3, valid_prc = 12.7, cum_prc = 17.2),    # frequencies_output.txt:69
        list(value = 3, freq = 496L, prc = 24.7, valid_prc = 25.5, cum_prc = 42.7),    # frequencies_output.txt:70
        list(value = 4, freq = 593L, prc = 29.6, valid_prc = 30.5, cum_prc = 73.1),    # frequencies_output.txt:71
        list(value = 5, freq = 524L, prc = 26.1, valid_prc = 26.9, cum_prc = 100.0)    # frequencies_output.txt:72
      ),
      valid_freq = 1949,      # frequencies_output.txt:73
      valid_prc = 97.1,       # frequencies_output.txt:73
      missing_freq = 58,      # frequencies_output.txt:74
      missing_prc = 2.9,      # frequencies_output.txt:74
      total_freq = 2007       # frequencies_output.txt:75
    )
  )
)


# Counts: exact when unweighted, Display(0) when weighted (SPSS rounds the
# printed weighted frequency). Percentages: Display(1).
compare_freq <- function(freq_df, stats_row, spss, scenario, is_weighted = FALSE) {
  assert_n <- function(actual, expected, what) {
    lab <- sprintf("[%s] %s", scenario, what)
    if (is_weighted) {
      assert_spss(as.numeric(actual), expected, tier = "display", precision = 0,
                  label = lab)
    } else {
      assert_spss_count(as.numeric(actual), expected, label = lab)
    }
  }
  assert_pct <- function(actual, expected, what) {
    assert_spss(as.numeric(actual), expected, tier = "display", precision = 1,
                label = sprintf("[%s] %s", scenario, what))
  }

  valid_rows <- freq_df[!is.na(freq_df$value), ]
  na_rows    <- freq_df[is.na(freq_df$value), ]

  # Valid values in SPSS order (ascending codes)
  expect_equal(as.numeric(valid_rows$value),
               vapply(spss$rows, function(x) x$value, numeric(1)),
               label = sprintf("[%s] value order", scenario))

  for (expected_row in spss$rows) {
    val <- expected_row$value
    actual <- valid_rows[valid_rows$value == val, ]
    if (nrow(actual) != 1) {
      stop(sprintf("[%s] missing row for value=%s", scenario, val), call. = FALSE)
    }
    assert_n(actual$freq, expected_row$freq, sprintf("value=%s freq", val))
    assert_pct(actual$prc, expected_row$prc, sprintf("value=%s prc", val))
    assert_pct(actual$valid_prc, expected_row$valid_prc,
               sprintf("value=%s valid_prc", val))
    assert_pct(actual$cum_prc, expected_row$cum_prc,
               sprintf("value=%s cum_prc", val))
  }

  # Valid Total row: frequency and its share of all cases
  assert_n(stats_row$valid_n, spss$valid_freq, "valid total freq")
  assert_pct(100 * stats_row$valid_n / stats_row$total_n, spss$valid_prc,
             "valid total prc")

  # Missing System row: SPSS prints it, so mariposa must return exactly one
  # NA row (the assertion used to be skipped silently without one)
  expect_equal(nrow(na_rows), 1L,
               label = sprintf("[%s] number of missing (NA) rows", scenario))
  assert_n(na_rows$freq[1], spss$missing_freq, "missing freq")
  assert_pct(na_rows$prc[1], spss$missing_prc, "missing prc")

  # Total row
  assert_n(stats_row$total_n, spss$total_freq, "total freq")
}


extract_grouped_subset <- function(result, region) {
  r <- result$results
  rows <- r[as.character(r$region) == region, ]
  rows
}

extract_grouped_stats <- function(result, region) {
  st <- result$stats
  row <- st[as.character(st$region) == region, ]
  stopifnot(nrow(row) == 1L)
  row
}


data(survey_data, envir = environment())


test_that("Test 1: frequency life_satisfaction unweighted — matches SPSS", {
  r <- survey_data |> frequency(life_satisfaction)
  compare_freq(r$results, r$stats, spss_values$test_1_unweighted,
               "1: life_sat unweighted")
})

test_that("Test 2: frequency life_satisfaction weighted — matches SPSS", {
  r <- survey_data |> frequency(life_satisfaction, weights = sampling_weight)
  compare_freq(r$results, r$stats, spss_values$test_2_weighted,
               "2: life_sat weighted", is_weighted = TRUE)
})

test_that("Test 3: frequency life_satisfaction grouped by region — matches SPSS", {
  r <- survey_data |> group_by(region) |> frequency(life_satisfaction)
  for (rg in c("East", "West")) {
    rows <- extract_grouped_subset(r, rg)
    compare_freq(rows, extract_grouped_stats(r, rg),
                 spss_values$test_3_unweighted_grouped[[rg]],
                 sprintf("3: life_sat [%s]", rg))
  }
})

test_that("Test 4: frequency life_satisfaction weighted+grouped — matches SPSS", {
  r <- survey_data |> group_by(region) |>
    frequency(life_satisfaction, weights = sampling_weight)
  for (rg in c("East", "West")) {
    rows <- extract_grouped_subset(r, rg)
    compare_freq(rows, extract_grouped_stats(r, rg),
                 spss_values$test_4_weighted_grouped[[rg]],
                 sprintf("4: life_sat weighted [%s]", rg), is_weighted = TRUE)
  }
})
