# ============================================================================
# COMPREHENSIVE FREQUENCY TABLE VALIDATION TESTS
# ============================================================================
# This file tests all aspects of frequency tables including:
# - Cell counts/frequencies
# - All percentage types (raw, valid, cumulative)
# - Missing value handling
# - Weighted frequencies
# - Grouped frequencies
# - Sorting options
# - Statistical summaries (mode, median for ordinal)
# ============================================================================

test_that("frequency() produces exact cell counts matching SPSS", {
  skip_if_not(file.exists("../spss_reference/data/survey_data.sav"))
  skip_if_not(file.exists("../spss_reference/outputs/frequency_comprehensive_output.txt"))
  
  # Load test data
  survey_data <- haven::read_sav("../spss_reference/data/survey_data.sav")
  spss_output <- "../spss_reference/outputs/frequency_comprehensive_output.txt"
  
  # Source parser if not already loaded
  if (!exists("parse_spss_frequency_table")) {
    source("helper-frequency-parser.R")
  }
  
  # TEST 1: Binary variable complete table (unweighted)
  spss_table <- parse_spss_frequency_table(
    readLines(spss_output),
    "TEST 1",
    "gender"
  )
  
  r_result <- SurveyStat::frequency(survey_data, gender)
  
  # Extract R frequency data
  r_freq_df <- r_result$results[["gender"]]
  
  # Check each cell
  expect_true(!is.null(spss_table), "SPSS table should be parsed")
  expect_true(!is.null(r_freq_df), "R result should have frequency data")
  
  # Compare counts for each category
  for (i in seq_along(spss_table$frequencies)) {
    spss_row <- spss_table$frequencies[[i]]
    
    # Find corresponding R row
    r_row <- r_freq_df[r_freq_df$value == spss_row$value, ]
    
    if (nrow(r_row) > 0) {
      # Test frequency count
      expect_equal(
        r_row$n[1], 
        spss_row$frequency,
        tolerance = 0.1,
        label = sprintf("Frequency for value %s", spss_row$value)
      )
      
      # Test raw percentage
      expect_equal(
        r_row$percent[1],
        spss_row$percent,
        tolerance = 0.01,
        label = sprintf("Percent for value %s", spss_row$value)
      )
      
      # Test valid percentage
      if (!is.na(spss_row$valid_percent)) {
        expect_equal(
          r_row$valid_percent[1],
          spss_row$valid_percent,
          tolerance = 0.01,
          label = sprintf("Valid percent for value %s", spss_row$value)
        )
      }
      
      # Test cumulative percentage
      if (!is.na(spss_row$cumulative_percent)) {
        expect_equal(
          r_row$cumulative[1],
          spss_row$cumulative_percent,
          tolerance = 0.01,
          label = sprintf("Cumulative percent for value %s", spss_row$value)
        )
      }
    }
  }
  
  # Check total
  if (!is.null(spss_table$totals$frequency)) {
    r_total <- sum(r_freq_df$n, na.rm = TRUE)
    expect_equal(
      r_total,
      spss_table$totals$frequency,
      tolerance = 0.1,
      label = "Total frequency count"
    )
  }
})

test_that("frequency() handles multi-category variables correctly", {
  skip_if_not(file.exists("../spss_reference/outputs/frequency_comprehensive_output.txt"))
  
  survey_data <- haven::read_sav("../spss_reference/data/survey_data.sav")
  spss_output <- readLines("../spss_reference/outputs/frequency_comprehensive_output.txt")
  
  # TEST 2: Multi-category variable (education)
  spss_table <- parse_spss_frequency_table(spss_output, "TEST 2", "education")
  r_result <- SurveyStat::frequency(survey_data, education)
  
  r_freq_df <- r_result$results[["education"]]
  
  # Check we have the right number of categories
  expect_equal(
    nrow(r_freq_df[!is.na(r_freq_df$value), ]),
    length(spss_table$frequencies),
    label = "Number of categories should match"
  )
  
  # Check statistics if available
  if (!is.null(spss_table$statistics$mode)) {
    r_mode <- r_result$stats[["education"]]$mode
    expect_equal(
      as.numeric(r_mode),
      spss_table$statistics$mode,
      tolerance = 0.01,
      label = "Mode should match"
    )
  }
})

test_that("frequency() calculates weighted frequencies correctly", {
  skip_if_not(file.exists("../spss_reference/outputs/frequency_comprehensive_output.txt"))
  
  survey_data <- haven::read_sav("../spss_reference/data/survey_data.sav")
  spss_output <- readLines("../spss_reference/outputs/frequency_comprehensive_output.txt")
  
  # TEST 3: Weighted binary variable
  spss_table <- parse_spss_frequency_table(spss_output, "TEST 3", "gender")
  r_result <- SurveyStat::frequency(survey_data, gender, weights = sampling_weight)
  
  r_freq_df <- r_result$results[["gender"]]
  
  # Weighted frequencies should be different from unweighted
  unweighted_result <- SurveyStat::frequency(survey_data, gender)
  unweighted_df <- unweighted_result$results[["gender"]]
  
  expect_false(
    all(r_freq_df$n == unweighted_df$n),
    "Weighted frequencies should differ from unweighted"
  )
  
  # Compare weighted frequencies with SPSS
  for (i in seq_along(spss_table$frequencies)) {
    spss_row <- spss_table$frequencies[[i]]
    r_row <- r_freq_df[r_freq_df$value == spss_row$value, ]
    
    if (nrow(r_row) > 0) {
      # Weighted frequencies might need higher tolerance
      expect_equal(
        r_row$n[1],
        spss_row$frequency,
        tolerance = 1.0,  # Higher tolerance for weighted counts
        label = sprintf("Weighted frequency for value %s", spss_row$value)
      )
      
      # Percentages should be very close
      expect_equal(
        r_row$percent[1],
        spss_row$percent,
        tolerance = 0.1,
        label = sprintf("Weighted percent for value %s", spss_row$value)
      )
    }
  }
})

test_that("frequency() handles missing values correctly", {
  skip_if_not(file.exists("../spss_reference/outputs/frequency_comprehensive_output.txt"))
  
  survey_data <- haven::read_sav("../spss_reference/data/survey_data.sav")
  spss_output <- readLines("../spss_reference/outputs/frequency_comprehensive_output.txt")
  
  # TEST 5: Missing values excluded
  spss_excluded <- parse_spss_frequency_table(spss_output, "TEST 5", "employment")
  r_excluded <- SurveyStat::frequency(survey_data, employment, show.na = FALSE)
  
  # TEST 6: Missing values included
  spss_included <- parse_spss_frequency_table(spss_output, "TEST 6", "employment")
  r_included <- SurveyStat::frequency(survey_data, employment, show.na = TRUE)
  
  # Check that included has more total cases than excluded
  r_excluded_total <- sum(r_excluded$results[["employment"]]$n, na.rm = TRUE)
  r_included_total <- sum(r_included$results[["employment"]]$n, na.rm = TRUE)
  
  expect_true(
    r_included_total >= r_excluded_total,
    "Including missing should have at least as many cases"
  )
  
  # Check for NA row in included results
  r_included_df <- r_included$results[["employment"]]
  has_na_row <- any(is.na(r_included_df$value))
  
  if (has_na_row && !is.null(spss_included)) {
    # Find the missing row in SPSS output
    missing_rows <- spss_included$frequencies[
      sapply(spss_included$frequencies, function(x) 
        grepl("Missing|System", x$label, ignore.case = TRUE)
      )
    ]
    
    if (length(missing_rows) > 0) {
      r_na_row <- r_included_df[is.na(r_included_df$value), ]
      spss_na_freq <- sum(sapply(missing_rows, function(x) x$frequency))
      
      expect_equal(
        sum(r_na_row$n),
        spss_na_freq,
        tolerance = 0.1,
        label = "Missing value frequency"
      )
    }
  }
})

test_that("frequency() produces correct cumulative percentages", {
  skip_if_not(file.exists("../spss_reference/outputs/frequency_comprehensive_output.txt"))
  
  survey_data <- haven::read_sav("../spss_reference/data/survey_data.sav")
  spss_output <- readLines("../spss_reference/outputs/frequency_comprehensive_output.txt")
  
  # TEST 7: Cumulative statistics
  spss_table <- parse_spss_frequency_table(spss_output, "TEST 7", "education")
  r_result <- SurveyStat::frequency(survey_data, education, show.sum = TRUE)
  
  r_freq_df <- r_result$results[["education"]]
  
  # Check cumulative percentages add up correctly
  last_row_idx <- which(!is.na(r_freq_df$value))
  if (length(last_row_idx) > 0) {
    last_row <- r_freq_df[max(last_row_idx), ]
    
    # Last valid cumulative should be 100%
    expect_equal(
      last_row$cumulative,
      100,
      tolerance = 0.01,
      label = "Last cumulative percentage should be 100%"
    )
  }
  
  # Check cumulative values are monotonically increasing
  valid_rows <- r_freq_df[!is.na(r_freq_df$value), ]
  if (nrow(valid_rows) > 1) {
    cumulative_diffs <- diff(valid_rows$cumulative)
    expect_true(
      all(cumulative_diffs >= 0),
      "Cumulative percentages should be non-decreasing"
    )
  }
})

test_that("frequency() handles grouped analysis correctly", {
  skip_if_not(file.exists("../spss_reference/outputs/frequency_comprehensive_output.txt"))
  
  library(dplyr)
  survey_data <- haven::read_sav("../spss_reference/data/survey_data.sav")
  
  # TEST 10: Grouped frequencies
  r_grouped <- survey_data %>%
    group_by(region) %>%
    SurveyStat::frequency(gender, education)
  
  # Check that we have results for each group
  expect_true(r_grouped$is_grouped, "Result should be grouped")
  
  # Each region should have frequency tables
  for (region_val in unique(survey_data$region)) {
    region_results <- r_grouped$results[[as.character(region_val)]]
    
    expect_true(
      !is.null(region_results),
      sprintf("Should have results for region %s", region_val)
    )
    
    # Should have tables for both variables
    expect_true(
      "gender" %in% names(region_results),
      "Should have gender frequencies"
    )
    expect_true(
      "education" %in% names(region_results),
      "Should have education frequencies"
    )
  }
})

test_that("frequency() sorting options work correctly", {
  skip_if_not(file.exists("../spss_reference/data/survey_data.sav"))
  
  survey_data <- haven::read_sav("../spss_reference/data/survey_data.sav")
  
  # Test different sorting options
  r_none <- SurveyStat::frequency(survey_data, education, sort.frq = "none")
  r_asc <- SurveyStat::frequency(survey_data, education, sort.frq = "asc")
  r_desc <- SurveyStat::frequency(survey_data, education, sort.frq = "desc")
  
  # Extract valid rows (non-NA)
  none_df <- r_none$results[["education"]][!is.na(r_none$results[["education"]]$value), ]
  asc_df <- r_asc$results[["education"]][!is.na(r_asc$results[["education"]]$value), ]
  desc_df <- r_desc$results[["education"]][!is.na(r_desc$results[["education"]]$value), ]
  
  # Check ascending sort
  expect_true(
    all(diff(asc_df$n) >= 0),
    "Ascending sort should have non-decreasing frequencies"
  )
  
  # Check descending sort
  expect_true(
    all(diff(desc_df$n) <= 0),
    "Descending sort should have non-increasing frequencies"
  )
  
  # Check that totals are the same regardless of sorting
  expect_equal(
    sum(none_df$n),
    sum(asc_df$n),
    label = "Total should be same regardless of sorting"
  )
  expect_equal(
    sum(none_df$n),
    sum(desc_df$n),
    label = "Total should be same regardless of sorting"
  )
})

test_that("frequency() display options work correctly", {
  skip_if_not(file.exists("../spss_reference/data/survey_data.sav"))
  
  survey_data <- haven::read_sav("../spss_reference/data/survey_data.sav")
  
  # Test with all display options off
  r_minimal <- SurveyStat::frequency(
    survey_data, 
    gender,
    show.na = FALSE,
    show.prc = FALSE,
    show.valid = FALSE,
    show.sum = FALSE
  )
  
  # Test with all display options on
  r_full <- SurveyStat::frequency(
    survey_data,
    gender,
    show.na = TRUE,
    show.prc = TRUE,
    show.valid = TRUE,
    show.sum = TRUE
  )
  
  # Check that options are stored correctly
  expect_false(r_minimal$options$show.na)
  expect_false(r_minimal$options$show.prc)
  expect_false(r_minimal$options$show.valid)
  expect_false(r_minimal$options$show.sum)
  
  expect_true(r_full$options$show.na)
  expect_true(r_full$options$show.prc)
  expect_true(r_full$options$show.valid)
  expect_true(r_full$options$show.sum)
  
  # Both should have the same underlying data
  minimal_df <- r_minimal$results[["gender"]]
  full_df <- r_full$results[["gender"]]
  
  # Frequency counts should be identical
  expect_equal(
    minimal_df$n[!is.na(minimal_df$value)],
    full_df$n[!is.na(full_df$value)],
    label = "Frequency counts should be same regardless of display options"
  )
})