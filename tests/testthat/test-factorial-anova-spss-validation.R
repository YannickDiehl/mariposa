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


# =============================================================================
# Weighted cell descriptives (/REGWGT reference, 0.7.4 audit)
# =============================================================================
# The weighted references use UNIANOVA /REGWGT: weighted cell means, SD with
# the weighted sum of squares over n - 1, N = number of cases.

spss_values$test_2a_cells <- list(
  list(gender = "Male",   region = "East", mean = 3.66, sd = 1.238, n = 228L),   # factorial_anova_output.txt:255
  list(gender = "Male",   region = "West", mean = 3.58, sd = 1.147, n = 921L),   # factorial_anova_output.txt:256
  list(gender = "Female", region = "East", mean = 3.59, sd = 1.230, n = 237L),   # factorial_anova_output.txt:258
  list(gender = "Female", region = "West", mean = 3.66, sd = 1.128, n = 1035L)   # factorial_anova_output.txt:259
)

test_that("Test 2a: weighted cell descriptives — match SPSS /REGWGT", {
  r <- factorial_anova(survey_data, dv = life_satisfaction,
                       between = c(gender, region), weights = sampling_weight)
  d <- r$descriptives
  for (cell in spss_values$test_2a_cells) {
    row <- d[d$gender == cell$gender & d$region == cell$region, , drop = FALSE]
    lab <- sprintf("[2a %s/%s]", cell$gender, cell$region)
    assert_spss(as.numeric(row$mean), cell$mean, tier = "display",
                precision = 2, label = paste(lab, "mean"))
    assert_spss(as.numeric(row$sd), cell$sd, tier = "display",
                precision = 3, label = paste(lab, "SD"))
    assert_spss_count(as.numeric(row$n), cell$n, label = paste(lab, "N"))
  }
})
