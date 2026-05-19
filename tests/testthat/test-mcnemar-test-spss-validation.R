# =============================================================================
# mcnemar_test — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::mcnemar_test() against SPSS v29 CROSSTABS
#          /STATISTICS=MCNEMAR.
# Reference output: tests/spss_reference/outputs/mcnemar_test_output.txt
#
# SPSS uses derived binary variables (trust_*_high = trust_* >= 4).
# mariposa exposes: statistic (chi²), p_value (asymptotic), exact_p, N, b, c.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(
  # ---- Test 1: trust_gov_high vs trust_media_high ----
  test_1_gov_media = list(
    n = 2227L, b = 338L, c = 448L,
    exact_p = "<.001"      # mcnemar_test_output.txt:45
  ),
  # ---- Test 2: trust_gov_high vs trust_sci_high ----
  test_2_gov_sci = list(
    n = 2255L, b = 1016L, c = 203L,
    exact_p = "<.001"      # mcnemar_test_output.txt:93
  ),
  # ---- Test 3: trust_media_high vs trust_sci_high ----
  test_3_media_sci = list(
    n = 2272L, b = 1128L, c = 169L,
    exact_p = "<.001"      # mcnemar_test_output.txt:141
  )
)


data(survey_data, envir = environment())

# Derive binary variables matching SPSS labels (>=4)
survey_data$trust_gov_high   <- as.integer(survey_data$trust_government >= 4)
survey_data$trust_media_high <- as.integer(survey_data$trust_media     >= 4)
survey_data$trust_sci_high   <- as.integer(survey_data$trust_science   >= 4)


compare_mcnemar <- function(row, spss, scenario) {
  assert_spss_count(as.numeric(row$n), spss$n,
                    label = sprintf("[%s] N", scenario))
  assert_spss_count(as.numeric(row$b), spss$b,
                    label = sprintf("[%s] b (discordant Low→High)", scenario))
  assert_spss_count(as.numeric(row$c), spss$c,
                    label = sprintf("[%s] c (discordant High→Low)", scenario))
  assert_spss(as.numeric(row$exact_p), spss$exact_p,
              tier = "display", precision = 3, what = "p_value",
              label = sprintf("[%s] exact p (2-sided)", scenario))
}


test_that("Test 1: McNemar trust_gov vs trust_media — matches SPSS", {
  r <- survey_data |> mcnemar_test(trust_gov_high, trust_media_high)
  compare_mcnemar(r$results, spss_values$test_1_gov_media, "1: gov vs media")
})

test_that("Test 2: McNemar trust_gov vs trust_sci — matches SPSS", {
  r <- survey_data |> mcnemar_test(trust_gov_high, trust_sci_high)
  compare_mcnemar(r$results, spss_values$test_2_gov_sci, "2: gov vs sci")
})

test_that("Test 3: McNemar trust_media vs trust_sci — matches SPSS", {
  r <- survey_data |> mcnemar_test(trust_media_high, trust_sci_high)
  compare_mcnemar(r$results, spss_values$test_3_media_sci, "3: media vs sci")
})
