# =============================================================================
# factorial_anova — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::factorial_anova() against SPSS v29 UNIANOVA
#          (Type III SS).
# Reference: factorial_anova_output.txt
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# SPSS Test 1a: life_satisfaction by gender × region
spss_values <- list(
  test_1a_life_gender_region = list(
    rows = list(
      "Corrected Model"  = list(ss = 3.311,    df = 3L,    ms = 1.104,    f = 0.830,    p = 0.477, eta2 = 0.001),    # factorial_anova_output.txt:61
      "Intercept"        = list(ss = 19718.320, df = 1L,   ms = 19718.320, f = 14828.083, p = "<.001", eta2 = 0.860),
      "gender"           = list(ss = 0.006,    df = 1L,    ms = 0.006,    f = 0.005,    p = 0.946, eta2 = 0.000),
      "region"           = list(ss = 0.025,    df = 1L,    ms = 0.025,    f = 0.019,    p = 0.891, eta2 = 0.000),
      "gender * region" = list(ss = 1.893,    df = 1L,    ms = 1.893,    f = 1.424,    p = 0.233, eta2 = 0.001),
      "Error"            = list(ss = 3214.116,  df = 2417L, ms = 1.330),
      "Total"            = list(ss = 35088.000, df = 2421L),
      "Corrected Total"  = list(ss = 3217.428,  df = 2420L)
    )
  )
)


data(survey_data, envir = environment())


test_that("Test 1a: factorial life_sat ~ gender * region — matches SPSS", {
  r <- factorial_anova(survey_data, dv = life_satisfaction,
                        between = c(gender, region))
  spss <- spss_values$test_1a_life_gender_region
  at <- r$anova_table

  for (source_name in names(spss$rows)) {
    expected <- spss$rows[[source_name]]
    row <- at[at$source == source_name, , drop = FALSE]
    if (nrow(row) != 1L) {
      stop(sprintf("source '%s' not found in mariposa anova_table", source_name),
           call. = FALSE)
    }
    if (!is.null(expected$ss)) {
      assert_spss(as.numeric(row$ss), expected$ss,
                  tier = "display", precision = 3,
                  label = sprintf("[%s] SS", source_name))
    }
    if (!is.null(expected$df)) {
      assert_spss_count(as.numeric(row$df), expected$df,
                        label = sprintf("[%s] df", source_name))
    }
    if (!is.null(expected$ms)) {
      assert_spss(as.numeric(row$ms), expected$ms,
                  tier = "display", precision = 3,
                  label = sprintf("[%s] MS", source_name))
    }
    if (!is.null(expected$f)) {
      assert_spss(as.numeric(row$f), expected$f,
                  tier = "display", precision = 3,
                  label = sprintf("[%s] F", source_name))
    }
    if (!is.null(expected$p)) {
      assert_spss(as.numeric(row$p), expected$p,
                  tier = "display", precision = 3, what = "p_value",
                  label = sprintf("[%s] p", source_name))
    }
    if (!is.null(expected$eta2)) {
      assert_spss(as.numeric(row$partial_eta_sq), expected$eta2,
                  tier = "display", precision = 3,
                  label = sprintf("[%s] partial eta²", source_name))
    }
  }
})
