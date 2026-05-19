# =============================================================================
# ancova — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::ancova() against SPSS v29 UNIANOVA with WITH.
# Reference: ancova_output.txt
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(
  # SPSS Test 1a: income ~ age + education (covariate = age, factor = education)
  test_1a_income_age_education = list(
    "Corrected Model" = list(ss = 1752821002.875, df = 4L, ms = 438205250.719, f = 349.723, p = "<.001", eta2 = 0.391),    # line 55
    "Intercept"       = list(ss = 3472400779.184, df = 1L, f = 2771.252, p = "<.001", eta2 = 0.560),
    "age"             = list(ss = 37721.406,      df = 1L, f = 0.030, p = 0.862, eta2 = 0.000),
    "education"       = list(ss = 1752630664.966, df = 3L, f = 466.246, p = "<.001", eta2 = 0.391),
    "Error"           = list(ss = 2732810163.640, df = 2181L, ms = 1253007.870),
    "Total"           = list(ss = 35290790000.000, df = 2186L),
    "Corrected Total" = list(ss = 4485631166.515, df = 2185L)
  )
)


data(survey_data, envir = environment())


test_that("Test 1a: ancova income ~ age (covariate) + education — matches SPSS", {
  r <- ancova(survey_data, dv = income, between = education, covariate = age)
  spss <- spss_values$test_1a_income_age_education

  for (source_name in names(spss)) {
    expected <- spss[[source_name]]
    row <- r$anova_table[r$anova_table$source == source_name, , drop = FALSE]
    if (nrow(row) != 1L) {
      stop(sprintf("source '%s' not in ancova output", source_name), call. = FALSE)
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
