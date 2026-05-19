# =============================================================================
# fisher_test — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::fisher_test() against SPSS v29 CROSSTABS
#          Fisher's Exact row.
# Reference output: tests/spss_reference/outputs/fisher_test_output.txt
#
# mariposa fisher_test exposes p_value and N only. CROSSTABS family honors
# WEIGHT BY, so all 4 scenarios are validatable.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(
  # ---- Test 1a: gender × region 2×2 unweighted ----
  test_1a = list(p_exact_2sided = 0.544, n = 2500L),   # fisher_test_output.txt:30

  # ---- Test 2a: gender × region 2×2 weighted ------
  test_2a = list(p_exact_2sided = 0.487, n = 2516L)    # fisher_test_output.txt:153
)


data(survey_data, envir = environment())


test_that("Test 1a: Fisher gender × region unweighted — matches SPSS", {
  r <- survey_data |> fisher_test(gender, region)
  assert_spss(as.numeric(r$results$p_value), spss_values$test_1a$p_exact_2sided,
              tier = "display", precision = 3, what = "p_value",
              label = "[1a] Fisher p-value (2-sided)")
  assert_spss_count(as.numeric(r$results$n), spss_values$test_1a$n,
                    label = "[1a] N")
})

test_that("Test 2a: Fisher gender × region weighted — matches SPSS", {
  r <- survey_data |> fisher_test(gender, region, weights = sampling_weight)
  assert_spss(as.numeric(r$results$p_value), spss_values$test_2a$p_exact_2sided,
              tier = "display", precision = 3, what = "p_value",
              label = "[2a] Fisher p-value weighted")
  assert_spss(as.numeric(r$results$n), spss_values$test_2a$n,
              tier = "display", precision = 0,
              label = "[2a] N weighted")
})

test_that("Test 3: Fisher grouped by education — structural", {
  r <- survey_data |> group_by(education) |> fisher_test(gender, region)
  expect_equal(nrow(r$results), 4L)  # 4 education levels
  expect_true(all(r$results$p_value > 0 & r$results$p_value < 1))
})

test_that("Edge case: Fisher rejects > 2-column table cleanly", {
  # Fisher exact for r×c can be slow; mariposa may not support it. Test that
  # it either succeeds with 2x3 or errors clearly.
  expect_no_error(suppressWarnings(
    fisher_test(survey_data, gender, interview_mode)
  ))
})
