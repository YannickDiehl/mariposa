# =============================================================================
# tukey_test — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::tukey_test() against SPSS v29 ONEWAY POSTHOC
#          TUKEY pairwise comparisons.
# Reference output: tests/spss_reference/outputs/tukey_test_output.txt
#
# mariposa returns one row per pair in the SPSS "(I) - (J)" orientation
# (I before J in the category order, Estimate = mean(I) - mean(J)); SPSS
# lists both directions, these are its first-listed rows (0.7.4, PAR-11).
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# SPSS Test 1a: life_satisfaction by education (4 levels) -> 6 pairs
spss_values <- list(
  test_1a_life = list(
    pairs = list(
      "Basic Secondary - Intermediate Secondary"    = list(diff = -0.497, ci_lower = -0.65, ci_upper = -0.34, p = "<.001"),  # line 15
      "Basic Secondary - Academic Secondary"        = list(diff = -0.649, ci_lower = -0.80, ci_upper = -0.50, p = "<.001"),  # line 16
      "Basic Secondary - University"                = list(diff = -0.843, ci_lower = -1.02, ci_upper = -0.67, p = "<.001"),  # line 17
      "Intermediate Secondary - Academic Secondary" = list(diff = -0.153, ci_lower = -0.32, ci_upper = 0.01,  p = 0.075),    # line 19
      "Intermediate Secondary - University"         = list(diff = -0.346, ci_lower = -0.53, ci_upper = -0.16, p = "<.001"),  # line 20
      "Academic Secondary - University"             = list(diff = -0.193, ci_lower = -0.38, ci_upper = -0.01, p = 0.037)     # line 23
    )
  )
)


data(survey_data, envir = environment())


test_that("Test 1a: Tukey life_satisfaction by education — matches SPSS", {
  av <- survey_data |> oneway_anova(life_satisfaction, group = education)
  r  <- tukey_test(av)
  pairs <- spss_values$test_1a_life$pairs

  for (pname in names(pairs)) {
    expected <- pairs[[pname]]
    row <- r$results[r$results$Comparison == pname, , drop = FALSE]
    if (nrow(row) != 1L) {
      stop(sprintf("Comparison %s not found in mariposa output", pname),
           call. = FALSE)
    }
    assert_spss(as.numeric(row$Estimate), expected$diff,
                tier = "display", precision = 3,
                label = sprintf("[%s] Mean Difference (I-J)", pname))
    assert_spss(as.numeric(row$conf_low),  expected$ci_lower,
                tier = "display", precision = 2,
                label = sprintf("[%s] CI lower", pname))
    assert_spss(as.numeric(row$conf_high), expected$ci_upper,
                tier = "display", precision = 2,
                label = sprintf("[%s] CI upper", pname))
    assert_spss(as.numeric(row$p_adjusted), expected$p,
                tier = "display", precision = 3, what = "p_value",
                label = sprintf("[%s] p_adjusted", pname))
  }
})
