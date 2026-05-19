# =============================================================================
# scheffe_test — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::scheffe_test() against SPSS v29 ONEWAY POSTHOC
#          SCHEFFE.
# Reference output: tests/spss_reference/outputs/scheffe_test_output.txt
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# SPSS Test 1a: life_sat by education (4 levels) -> 6 pairs
spss_values <- list(
  # mariposa Scheffé puts first-level first; absolute diff matches SPSS.
  test_1a_life = list(
    "Basic Secondary - Intermediate Secondary"     = list(diff = 0.497, p = "<.001"),
    "Basic Secondary - Academic Secondary"         = list(diff = 0.649, p = "<.001"),
    "Basic Secondary - University"                 = list(diff = 0.843, p = "<.001"),
    "Intermediate Secondary - Academic Secondary"  = list(diff = 0.153, p = 0.121),
    "Intermediate Secondary - University"          = list(diff = 0.346, p = "<.001"),
    "Academic Secondary - University"              = list(diff = 0.193, p = 0.067)
  )
)


data(survey_data, envir = environment())


test_that("Test 1a: Scheffé life_satisfaction by education — matches SPSS", {
  av <- survey_data |> oneway_anova(life_satisfaction, group = education)
  r  <- scheffe_test(av)

  for (pname in names(spss_values$test_1a_life)) {
    expected <- spss_values$test_1a_life[[pname]]
    row <- r$results[r$results$Comparison == pname, , drop = FALSE]
    if (nrow(row) != 1L) {
      stop(sprintf("Comparison '%s' not found in mariposa output", pname),
           call. = FALSE)
    }
    assert_spss(abs(as.numeric(row$Estimate)), abs(expected$diff),
                tier = "display", precision = 3,
                label = sprintf("[%s] |diff|", pname))
    assert_spss(as.numeric(row$p_adjusted), expected$p,
                tier = "display", precision = 3, what = "p_value",
                label = sprintf("[%s] p_adjusted", pname))
  }
})
