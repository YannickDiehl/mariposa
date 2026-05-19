# =============================================================================
# linear_regression — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::linear_regression() against SPSS v29 REGRESSION.
# Reference: linear_regression_output.txt
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(

  # ---- Test 1a: life_sat ~ age (bivariate) ----
  test_1a_life_age = list(
    n = 2421L,
    R = 0.029, R2 = 0.001, adj_R2 = 0.000,
    se_estimate = 1.153,
    anova = list(ss_reg = 2.653, df_reg = 1L, ms_reg = 2.653,
                 ss_res = 3214.775, df_res = 2419L, ms_res = 1.329,
                 ss_tot = 3217.428, df_tot = 2420L,
                 F = 1.996, p = 0.158),
    coefs = list(
      intercept = list(B = 3.727, SE = 0.074, t = 50.663, p = "<.001"),
      age       = list(B = -0.002, SE = 0.001, Beta = -0.029, t = -1.413, p = 0.158)
    )
  )
)


data(survey_data, envir = environment())


test_that("Test 1a: linear_regression life_sat ~ age — matches SPSS", {
  r <- linear_regression(survey_data, life_satisfaction ~ age)
  spss <- spss_values$test_1a_life_age

  # Model summary
  assert_spss(r$model_summary$R,           spss$R,
              tier = "display", precision = 3, label = "[1a] R")
  assert_spss(r$model_summary$R_squared,   spss$R2,
              tier = "display", precision = 3, label = "[1a] R²")
  assert_spss(r$model_summary$adj_R_squared, spss$adj_R2,
              tier = "display", precision = 3, label = "[1a] adj-R²")
  assert_spss(r$model_summary$std_error,   spss$se_estimate,
              tier = "display", precision = 3, label = "[1a] SE of estimate")

  # ANOVA
  reg_row <- r$anova[r$anova$Source == "Regression", ]
  assert_spss(reg_row$Sum_of_Squares, spss$anova$ss_reg,
              tier = "display", precision = 3, label = "[1a] SS Regression")
  assert_spss_count(reg_row$df, spss$anova$df_reg, label = "[1a] df Regression")
  assert_spss(reg_row$F_statistic, spss$anova$F,
              tier = "display", precision = 3, label = "[1a] F")
  assert_spss(reg_row$Sig, spss$anova$p,
              tier = "display", precision = 3, what = "p_value",
              label = "[1a] ANOVA Sig")

  # Coefficients
  intercept <- r$coefficients[r$coefficients$Term == "(Intercept)", ]
  assert_spss(intercept$B,         spss$coefs$intercept$B,
              tier = "display", precision = 3, label = "[1a] Intercept B")
  assert_spss(intercept$Std.Error, spss$coefs$intercept$SE,
              tier = "display", precision = 3, label = "[1a] Intercept SE")
  assert_spss(intercept$t,         spss$coefs$intercept$t,
              tier = "display", precision = 3, label = "[1a] Intercept t")
  assert_spss(intercept$p,         spss$coefs$intercept$p,
              tier = "display", precision = 3, what = "p_value",
              label = "[1a] Intercept p")

  age_row <- r$coefficients[r$coefficients$Term == "age", ]
  assert_spss(age_row$B,    spss$coefs$age$B,
              tier = "display", precision = 3, label = "[1a] Age B")
  assert_spss(age_row$Beta, spss$coefs$age$Beta,
              tier = "display", precision = 3, label = "[1a] Age Beta")
  assert_spss(age_row$t,    spss$coefs$age$t,
              tier = "display", precision = 3, label = "[1a] Age t")
  assert_spss(age_row$p,    spss$coefs$age$p,
              tier = "display", precision = 3, what = "p_value",
              label = "[1a] Age p")
})
