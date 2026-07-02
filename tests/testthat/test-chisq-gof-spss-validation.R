# =============================================================================
# chisq_gof — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::chisq_gof() against SPSS v29 NPAR /CHISQUARE.
# Reference output: tests/spss_reference/outputs/chisq_gof_output.txt
#
# NPAR TESTS family — WEIGHT BY ignored (confirmed pattern). All tests in
# the SPSS reference are unweighted; weighted scenarios for mariposa are
# Tier-4 (R-only inline baselines).
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(
  test_gender       = list(chi_sq = 5.018,    df = 1L, p = 0.025, n = 2500L),    # chisq_gof_output.txt:32
  test_education    = list(chi_sq = 156.454,  df = 3L, p = "<.001", n = 2500L),  # chisq_gof_output.txt:70
  test_region       = list(chi_sq = 936.360,  df = 1L, p = "<.001", n = 2500L),  # chisq_gof_output.txt:106
  test_employment   = list(chi_sq = 3276.116, df = 4L, p = "<.001", n = 2500L),  # chisq_gof_output.txt:145
  test_interview    = list(chi_sq = 816.620,  df = 2L, p = "<.001", n = 2500L)   # chisq_gof_output.txt:182
)


data(survey_data, envir = environment())


compare_gof <- function(row, spss, scenario) {
  assert_spss(as.numeric(row$chi_squared), spss$chi_sq,
              tier = "display", precision = 3,
              label = sprintf("[%s] chi²", scenario))
  assert_spss_count(as.numeric(row$df), spss$df,
                    label = sprintf("[%s] df", scenario))
  assert_spss(as.numeric(row$p_value), spss$p,
              tier = "display", precision = 3, what = "p_value",
              label = sprintf("[%s] p-value", scenario))
  assert_spss_count(as.numeric(row$n), spss$n,
                    label = sprintf("[%s] N", scenario))
}


test_that("Test 1: chisq_gof gender — matches SPSS", {
  r <- survey_data |> chisq_gof(gender)
  compare_gof(r$results, spss_values$test_gender, "gender")
})

test_that("Test 2: chisq_gof education — matches SPSS", {
  r <- survey_data |> chisq_gof(education)
  compare_gof(r$results, spss_values$test_education, "education")
})

test_that("Test 3: chisq_gof region — matches SPSS", {
  r <- survey_data |> chisq_gof(region)
  compare_gof(r$results, spss_values$test_region, "region")
})

test_that("Test 4: chisq_gof employment — matches SPSS", {
  r <- survey_data |> chisq_gof(employment)
  compare_gof(r$results, spss_values$test_employment, "employment")
})

test_that("Test 5: chisq_gof interview_mode — matches SPSS", {
  r <- survey_data |> chisq_gof(interview_mode)
  compare_gof(r$results, spss_values$test_interview, "interview_mode")
})

test_that("Test grouped: chisq_gof gender grouped by region — structural", {
  r <- survey_data |> group_by(region) |> chisq_gof(gender)
  expect_equal(nrow(r$results), 2L)
  expect_true(all(r$results$chi_squared > 0))
})
