# =============================================================================
# tukey_test — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::tukey_test() against SPSS v29 ONEWAY POSTHOC
#          TUKEY pairwise comparisons.
# Reference output: tests/spss_reference/outputs/tukey_test_output.txt
#
# mariposa returns one row per pair (signed Estimate, CI, p_adjusted).
# SPSS shows both directions; we match by |Estimate|.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# SPSS Test 1a: life_satisfaction by education (4 levels) -> 6 pairs
spss_values <- list(
  test_1a_life = list(
    pairs = list(
      "Intermediate Secondary-Basic Secondary"      = list(diff = 0.497,  ci_lower = 0.34,  ci_upper = 0.65, p = "<.001"),  # line 12
      "Academic Secondary-Basic Secondary"          = list(diff = 0.649,  ci_lower = 0.50,  ci_upper = 0.80, p = "<.001"),
      "University-Basic Secondary"                  = list(diff = 0.843,  ci_lower = 0.67,  ci_upper = 1.02, p = "<.001"),
      "Academic Secondary-Intermediate Secondary"   = list(diff = 0.153,  ci_lower = -0.01, ci_upper = 0.32, p = 0.075),
      "University-Intermediate Secondary"           = list(diff = 0.346,  ci_lower = 0.16,  ci_upper = 0.53, p = "<.001"),
      "University-Academic Secondary"               = list(diff = 0.193,  ci_lower = 0.01,  ci_upper = 0.38, p = 0.037)
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
    assert_spss(abs(as.numeric(row$Estimate)), abs(expected$diff),
                tier = "display", precision = 3,
                label = sprintf("[%s] |diff|", pname))
    # CI bounds: sign should match (mariposa returns signed)
    if (as.numeric(row$Estimate) > 0) {
      assert_spss(as.numeric(row$conf_low),  expected$ci_lower,
                  tier = "display", precision = 2,
                  label = sprintf("[%s] CI lower", pname))
      assert_spss(as.numeric(row$conf_high), expected$ci_upper,
                  tier = "display", precision = 2,
                  label = sprintf("[%s] CI upper", pname))
    }
    assert_spss(as.numeric(row$p_adjusted), expected$p,
                tier = "display", precision = 3, what = "p_value",
                label = sprintf("[%s] p_adjusted", pname))
  }
})
