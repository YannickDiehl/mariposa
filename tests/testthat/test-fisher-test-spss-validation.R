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
  test_2a = list(p_exact_2sided = 0.487, n = 2516L),   # fisher_test_output.txt:153

  # ---- Test 1c: gender × region, SELECT IF id <= 50 ------
  # A negative association: SPSS prints Phi of a 2x2 table with its sign
  test_1c = list(
    p_exact_2sided = 0.734,   # fisher_test_output.txt:111
    n = 50L,                  # fisher_test_output.txt:113
    chi_squared = 0.334,      # fisher_test_output.txt:108
    chi_p = 0.563,            # fisher_test_output.txt:108
    phi = -0.082,             # fisher_test_output.txt:120
    cramers_v = 0.082         # fisher_test_output.txt:121
  )
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
})

test_that("Test 3: Fisher grouped by education — structural", {
  r <- survey_data |> group_by(education) |> fisher_test(gender, region)
  expect_equal(nrow(r$results), 4L)  # 4 education levels
  expect_true(all(r$results$p_value > 0 & r$results$p_value < 1))
})

test_that("Edge case: Fisher 2x3 table returns numeric p-value", {
  r <- suppressWarnings(fisher_test(survey_data, gender, interview_mode))
  # Validate result structure: p in (0,1), N positive
  expect_true(as.numeric(r$results$p_value) > 0 &&
              as.numeric(r$results$p_value) < 1)
  expect_true(as.numeric(r$results$n) > 0)
})
