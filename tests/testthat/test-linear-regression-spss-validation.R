# ==============================================================================
# SPSS Validation Tests for linear_regression()
# ==============================================================================
# Reference: tests/spss_reference/outputs/linear_regression_output.txt
# SPSS Syntax: tests/spss_reference/syntax/linear_regression.sps
#
# Test structure:
#   Test 1a-1d: Unweighted/Ungrouped (bivariate, trust items, income model, 4-pred)
#   Test 2a-2c: Weighted/Ungrouped
#   Test 3a-3b: Unweighted/Grouped by region
#   Test 4a-4b: Weighted/Grouped by region
#   Structural tests: class, print, interface
# ==============================================================================

# Helper: absolute tolerance comparison (matches SPSS rounding)
expect_spss <- function(actual, expected, abs_tol = 0.001, label = NULL) {
  diff <- abs(actual - expected)
  expect_true(diff <= abs_tol,
    label = paste0(label %||% deparse(substitute(actual)),
                   ": R=", round(actual, 6), " SPSS=", expected,
                   " diff=", round(diff, 6), " tol=", abs_tol))
}

# ==============================================================================
# TEST 1a: Unweighted, Ungrouped - Bivariate (life_satisfaction ~ age)
# ==============================================================================

test_that("Test 1a: Unweighted, ungrouped, bivariate regression", {
  data(survey_data)
  result <- linear_regression(survey_data, life_satisfaction ~ age)

  # Model summary
  expect_spss(result$model_summary$R, 0.029, abs_tol = 0.0005)
  expect_spss(result$model_summary$R_squared, 0.001, abs_tol = 0.0005)
  expect_spss(result$model_summary$adj_R_squared, 0.000, abs_tol = 0.001)
  expect_spss(result$model_summary$std_error, 1.153, abs_tol = 0.001)

  # Sample size
  expect_equal(result$n, 2421)

  # ANOVA
  expect_spss(result$anova$Sum_of_Squares[1], 2.653, abs_tol = 0.01)   # Regression SS
  expect_spss(result$anova$Sum_of_Squares[2], 3214.775, abs_tol = 0.01) # Residual SS
  expect_spss(result$anova$Sum_of_Squares[3], 3217.428, abs_tol = 0.01) # Total SS
  expect_equal(result$anova$df[1], 1L)     # Regression df
  expect_equal(result$anova$df[2], 2419L)  # Residual df
  expect_equal(result$anova$df[3], 2420L)  # Total df
  expect_spss(result$anova$F_statistic[1], 1.996, abs_tol = 0.001)
  expect_true(result$anova$Sig[1] > 0.05)  # Not significant

  # Coefficients
  # (Constant)
  expect_spss(result$coefficients$B[1], 3.727, abs_tol = 0.001)
  expect_spss(result$coefficients$Std.Error[1], 0.074, abs_tol = 0.001)
  expect_spss(result$coefficients$t[1], 50.663, abs_tol = 0.01)
  expect_true(result$coefficients$p[1] < 0.001)

  # Age
  expect_spss(result$coefficients$B[2], -0.002, abs_tol = 0.001)
  expect_spss(result$coefficients$Std.Error[2], 0.001, abs_tol = 0.001)
  expect_spss(result$coefficients$Beta[2], -0.029, abs_tol = 0.001)
  expect_spss(result$coefficients$t[2], -1.413, abs_tol = 0.01)

  # Intercept has no Beta
  expect_true(is.na(result$coefficients$Beta[1]))
})

# ==============================================================================
# TEST 1b: Unweighted, Ungrouped - Multiple (trust items)
# ==============================================================================

test_that("Test 1b: Unweighted, ungrouped, trust items regression", {
  data(survey_data)
  result <- linear_regression(survey_data,
    life_satisfaction ~ trust_government + trust_media + trust_science)

  # Model summary
  expect_spss(result$model_summary$R, 0.041, abs_tol = 0.001)
  expect_spss(result$model_summary$R_squared, 0.002, abs_tol = 0.001)
  expect_spss(result$model_summary$adj_R_squared, 0.000, abs_tol = 0.001)
  expect_spss(result$model_summary$std_error, 1.144, abs_tol = 0.001)

  expect_equal(result$n, 2066)

  # ANOVA
  expect_spss(result$anova$Sum_of_Squares[1], 4.570, abs_tol = 0.01)
  expect_spss(result$anova$Sum_of_Squares[2], 2699.432, abs_tol = 0.01)
  expect_spss(result$anova$F_statistic[1], 1.164, abs_tol = 0.001)
  expect_equal(result$anova$df[1], 3L)
  expect_equal(result$anova$df[2], 2062L)

  # Coefficients
  expect_spss(result$coefficients$B[1], 3.675, abs_tol = 0.001)   # Constant
  expect_spss(result$coefficients$B[2], -0.008, abs_tol = 0.001)  # trust_gov
  expect_spss(result$coefficients$B[3], 0.035, abs_tol = 0.001)   # trust_media
  expect_spss(result$coefficients$B[4], -0.023, abs_tol = 0.001)  # trust_science

  # Standardized coefficients
  expect_spss(result$coefficients$Beta[2], -0.008, abs_tol = 0.001)
  expect_spss(result$coefficients$Beta[3], 0.035, abs_tol = 0.001)
  expect_spss(result$coefficients$Beta[4], -0.021, abs_tol = 0.001)

  # t-statistics
  expect_spss(result$coefficients$t[2], -0.361, abs_tol = 0.01)
  expect_spss(result$coefficients$t[3], 1.598, abs_tol = 0.01)
  expect_spss(result$coefficients$t[4], -0.932, abs_tol = 0.01)
})

# ==============================================================================
# TEST 1c: Unweighted, Ungrouped - Income model (with factor variable)
# ==============================================================================

test_that("Test 1c: Unweighted, ungrouped, income model", {
  data(survey_data)
  result <- linear_regression(survey_data,
    income ~ age + education + life_satisfaction)

  # Model summary
  expect_spss(result$model_summary$R, 0.686, abs_tol = 0.001)
  expect_spss(result$model_summary$R_squared, 0.471, abs_tol = 0.001)
  expect_spss(result$model_summary$adj_R_squared, 0.470, abs_tol = 0.001)
  expect_spss(result$model_summary$std_error, 1041.889, abs_tol = 0.01)

  expect_equal(result$n, 2115)

  # ANOVA
  expect_spss(result$anova$Sum_of_Squares[1], 2036941094.578, abs_tol = 1)
  expect_spss(result$anova$Sum_of_Squares[2], 2291561553.177, abs_tol = 1)
  expect_spss(result$anova$F_statistic[1], 625.481, abs_tol = 0.01)
  expect_equal(result$anova$df[1], 3L)
  expect_equal(result$anova$df[2], 2111L)

  # Coefficients
  expect_spss(result$coefficients$B[1], 800.493, abs_tol = 0.01)    # Constant
  expect_spss(result$coefficients$B[2], -0.194, abs_tol = 0.001)    # age
  expect_spss(result$coefficients$B[3], 711.841, abs_tol = 0.01)    # education
  expect_spss(result$coefficients$B[4], 377.527, abs_tol = 0.01)    # life_satisfaction

  # Standardized
  expect_spss(result$coefficients$Beta[2], -0.002, abs_tol = 0.001)
  expect_spss(result$coefficients$Beta[3], 0.539, abs_tol = 0.001)
  expect_spss(result$coefficients$Beta[4], 0.303, abs_tol = 0.001)

  # Standard errors
  expect_spss(result$coefficients$Std.Error[1], 105.893, abs_tol = 0.01)
  expect_spss(result$coefficients$Std.Error[2], 1.333, abs_tol = 0.001)
  expect_spss(result$coefficients$Std.Error[3], 21.706, abs_tol = 0.01)

  # t-statistics
  expect_spss(result$coefficients$t[3], 32.794, abs_tol = 0.01)
  expect_spss(result$coefficients$t[4], 18.412, abs_tol = 0.01)
})

# ==============================================================================
# TEST 1d: Unweighted, Ungrouped - 4 predictors
# ==============================================================================

test_that("Test 1d: Unweighted, ungrouped, 4 predictors", {
  data(survey_data)
  result <- linear_regression(survey_data,
    life_satisfaction ~ age + environmental_concern + political_orientation + trust_government)

  # Model summary
  expect_spss(result$model_summary$R, 0.037, abs_tol = 0.001)
  expect_spss(result$model_summary$R_squared, 0.001, abs_tol = 0.001)
  expect_spss(result$model_summary$adj_R_squared, -0.001, abs_tol = 0.001)

  expect_equal(result$n, 2017)

  # ANOVA
  expect_spss(result$anova$F_statistic[1], 0.699, abs_tol = 0.001)
  expect_equal(result$anova$df[1], 4L)
  expect_equal(result$anova$df[2], 2012L)

  # Coefficients
  expect_spss(result$coefficients$B[1], 3.581, abs_tol = 0.001)   # Constant
  expect_spss(result$coefficients$B[2], -0.002, abs_tol = 0.001)  # age
  expect_spss(result$coefficients$B[3], 0.014, abs_tol = 0.001)   # env_concern
  expect_spss(result$coefficients$B[4], 0.006, abs_tol = 0.001)   # pol_orient
  expect_spss(result$coefficients$B[5], 0.025, abs_tol = 0.001)   # trust_gov

  # Beta
  expect_spss(result$coefficients$Beta[2], -0.023, abs_tol = 0.001)
  expect_spss(result$coefficients$Beta[3], 0.015, abs_tol = 0.001)
  expect_spss(result$coefficients$Beta[4], 0.005, abs_tol = 0.001)
  expect_spss(result$coefficients$Beta[5], 0.026, abs_tol = 0.001)
})

# ==============================================================================
# TEST 2a: Weighted, Ungrouped - Bivariate
# ==============================================================================

test_that("Test 2a: Weighted, ungrouped, bivariate regression", {
  data(survey_data)
  result <- linear_regression(survey_data, life_satisfaction ~ age,
                              weights = sampling_weight)

  # Weighted N
  expect_equal(result$n, 2437)

  # Model summary
  expect_spss(result$model_summary$R, 0.029, abs_tol = 0.001)
  expect_spss(result$model_summary$R_squared, 0.001, abs_tol = 0.001)
  expect_spss(result$model_summary$std_error, 1.152, abs_tol = 0.001)

  # ANOVA
  expect_spss(result$anova$Sum_of_Squares[1], 2.757, abs_tol = 0.01)
  expect_spss(result$anova$Sum_of_Squares[2], 3230.392, abs_tol = 0.1)
  expect_spss(result$anova$F_statistic[1], 2.078, abs_tol = 0.01)
  expect_equal(result$anova$df[1], 1L)
  expect_equal(result$anova$df[2], 2435L)

  # Coefficients
  expect_spss(result$coefficients$B[1], 3.724, abs_tol = 0.001)
  expect_spss(result$coefficients$Std.Error[1], 0.073, abs_tol = 0.001)
  expect_spss(result$coefficients$t[1], 51.152, abs_tol = 0.01)

  expect_spss(result$coefficients$B[2], -0.002, abs_tol = 0.001)
  expect_spss(result$coefficients$Std.Error[2], 0.001, abs_tol = 0.001)
  expect_spss(result$coefficients$Beta[2], -0.029, abs_tol = 0.001)
  expect_spss(result$coefficients$t[2], -1.441, abs_tol = 0.01)
})

# ==============================================================================
# TEST 2b: Weighted, Ungrouped - Trust items
# ==============================================================================

test_that("Test 2b: Weighted, ungrouped, trust items regression", {
  data(survey_data)
  result <- linear_regression(survey_data,
    life_satisfaction ~ trust_government + trust_media + trust_science,
    weights = sampling_weight)

  expect_equal(result$n, 2080)

  # Model summary
  expect_spss(result$model_summary$R, 0.042, abs_tol = 0.001)
  expect_spss(result$model_summary$R_squared, 0.002, abs_tol = 0.001)
  expect_spss(result$model_summary$std_error, 1.143, abs_tol = 0.001)

  # ANOVA
  expect_spss(result$anova$Sum_of_Squares[1], 4.808, abs_tol = 0.01)
  expect_spss(result$anova$Sum_of_Squares[2], 2712.928, abs_tol = 0.1)
  expect_spss(result$anova$F_statistic[1], 1.226, abs_tol = 0.01)

  # Coefficients
  expect_spss(result$coefficients$B[1], 3.680, abs_tol = 0.001)
  expect_spss(result$coefficients$B[2], -0.003, abs_tol = 0.001)
  expect_spss(result$coefficients$B[3], 0.034, abs_tol = 0.001)
  expect_spss(result$coefficients$B[4], -0.027, abs_tol = 0.001)

  expect_spss(result$coefficients$Beta[2], -0.004, abs_tol = 0.001)
  expect_spss(result$coefficients$Beta[3], 0.034, abs_tol = 0.001)
  expect_spss(result$coefficients$Beta[4], -0.025, abs_tol = 0.001)

  expect_spss(result$coefficients$t[2], -0.161, abs_tol = 0.01)
  expect_spss(result$coefficients$t[3], 1.567, abs_tol = 0.01)
  expect_spss(result$coefficients$t[4], -1.127, abs_tol = 0.01)
})

# ==============================================================================
# TEST 2c: Weighted, Ungrouped - Income model
# ==============================================================================

test_that("Test 2c: Weighted, ungrouped, income model", {
  data(survey_data)
  result <- linear_regression(survey_data,
    income ~ age + education + life_satisfaction,
    weights = sampling_weight)

  expect_equal(result$n, 2130)

  # Model summary
  expect_spss(result$model_summary$R, 0.686, abs_tol = 0.001)
  expect_spss(result$model_summary$R_squared, 0.470, abs_tol = 0.001)
  expect_spss(result$model_summary$adj_R_squared, 0.469, abs_tol = 0.001)
  expect_spss(result$model_summary$std_error, 1036.287, abs_tol = 0.05)

  # ANOVA
  expect_spss(result$anova$Sum_of_Squares[1], 2026419335.048, abs_tol = 100)
  expect_spss(result$anova$F_statistic[1], 628.996, abs_tol = 0.1)
  expect_equal(result$anova$df[1], 3L)
  expect_equal(result$anova$df[2], 2126L)

  # Coefficients
  expect_spss(result$coefficients$B[1], 784.981, abs_tol = 0.1)
  expect_spss(result$coefficients$B[2], -0.110, abs_tol = 0.001)
  expect_spss(result$coefficients$B[3], 709.765, abs_tol = 0.01)
  expect_spss(result$coefficients$B[4], 381.257, abs_tol = 0.01)

  expect_spss(result$coefficients$Beta[3], 0.537, abs_tol = 0.001)
  expect_spss(result$coefficients$Beta[4], 0.307, abs_tol = 0.001)

  expect_spss(result$coefficients$t[3], 32.770, abs_tol = 0.01)
  expect_spss(result$coefficients$t[4], 18.773, abs_tol = 0.01)
})

# ==============================================================================
# TEST 3a: Unweighted, Grouped - Bivariate
# ==============================================================================

test_that("Test 3a: Unweighted, grouped, bivariate regression", {
  data(survey_data)
  result <- linear_regression(
    survey_data %>% dplyr::group_by(region),
    life_satisfaction ~ age)

  expect_true(result$is_grouped)
  expect_equal(length(result$groups), 2)

  # East
  east <- result$groups[[1]]
  expect_equal(east$n, 465)
  expect_spss(east$model_summary$R, 0.043, abs_tol = 0.001)
  expect_spss(east$model_summary$R_squared, 0.002, abs_tol = 0.001)
  expect_spss(east$model_summary$std_error, 1.207, abs_tol = 0.001)

  expect_spss(east$anova$Sum_of_Squares[1], 1.276, abs_tol = 0.01)
  expect_spss(east$anova$Sum_of_Squares[2], 674.350, abs_tol = 0.01)
  expect_spss(east$anova$F_statistic[1], 0.876, abs_tol = 0.001)
  expect_equal(east$anova$df[2], 463L)

  expect_spss(east$coefficients$B[1], 3.775, abs_tol = 0.001)
  expect_spss(east$coefficients$B[2], -0.003, abs_tol = 0.001)
  expect_spss(east$coefficients$Beta[2], -0.043, abs_tol = 0.001)
  expect_spss(east$coefficients$t[2], -0.936, abs_tol = 0.01)

  # West
  west <- result$groups[[2]]
  expect_equal(west$n, 1956)
  expect_spss(west$model_summary$R, 0.025, abs_tol = 0.001)
  expect_spss(west$model_summary$std_error, 1.140, abs_tol = 0.001)

  expect_spss(west$anova$Sum_of_Squares[1], 1.556, abs_tol = 0.01)
  expect_spss(west$anova$F_statistic[1], 1.197, abs_tol = 0.001)
  expect_equal(west$anova$df[2], 1954L)

  expect_spss(west$coefficients$B[1], 3.714, abs_tol = 0.001)
  expect_spss(west$coefficients$B[2], -0.002, abs_tol = 0.001)
  expect_spss(west$coefficients$Beta[2], -0.025, abs_tol = 0.001)
  expect_spss(west$coefficients$t[2], -1.094, abs_tol = 0.01)
})

# ==============================================================================
# TEST 3b: Unweighted, Grouped - Trust items
# ==============================================================================

test_that("Test 3b: Unweighted, grouped, trust items regression", {
  data(survey_data)
  result <- linear_regression(
    survey_data %>% dplyr::group_by(region),
    life_satisfaction ~ trust_government + trust_media + trust_science)

  # East
  east <- result$groups[[1]]
  expect_equal(east$n, 404)
  expect_spss(east$model_summary$R, 0.095, abs_tol = 0.001)
  expect_spss(east$model_summary$R_squared, 0.009, abs_tol = 0.001)
  expect_spss(east$anova$F_statistic[1], 1.222, abs_tol = 0.001)
  expect_equal(east$anova$df[1], 3L)
  expect_equal(east$anova$df[2], 400L)

  expect_spss(east$coefficients$B[1], 4.169, abs_tol = 0.001)
  expect_spss(east$coefficients$B[2], -0.035, abs_tol = 0.001)
  expect_spss(east$coefficients$B[3], -0.065, abs_tol = 0.001)
  expect_spss(east$coefficients$B[4], -0.077, abs_tol = 0.001)

  expect_spss(east$coefficients$Beta[2], -0.033, abs_tol = 0.001)
  expect_spss(east$coefficients$Beta[3], -0.058, abs_tol = 0.001)
  expect_spss(east$coefficients$Beta[4], -0.064, abs_tol = 0.001)

  # West
  west <- result$groups[[2]]
  expect_equal(west$n, 1662)
  expect_spss(west$model_summary$R, 0.059, abs_tol = 0.001)
  expect_spss(west$model_summary$R_squared, 0.003, abs_tol = 0.001)
  expect_spss(west$anova$F_statistic[1], 1.914, abs_tol = 0.001)
  expect_equal(west$anova$df[2], 1658L)

  expect_spss(west$coefficients$B[1], 3.561, abs_tol = 0.001)
  expect_spss(west$coefficients$B[2], -0.002, abs_tol = 0.001)
  expect_spss(west$coefficients$B[3], 0.056, abs_tol = 0.001)
  expect_spss(west$coefficients$B[4], -0.009, abs_tol = 0.001)

  expect_spss(west$coefficients$Beta[3], 0.058, abs_tol = 0.001)
  expect_spss(west$coefficients$t[3], 2.375, abs_tol = 0.01)
})

# ==============================================================================
# TEST 4a: Weighted, Grouped - Bivariate
# ==============================================================================

test_that("Test 4a: Weighted, grouped, bivariate regression", {
  data(survey_data)
  result <- linear_regression(
    survey_data %>% dplyr::group_by(region),
    life_satisfaction ~ age,
    weights = sampling_weight)

  # East
  east <- result$groups[[1]]
  expect_equal(east$n, 488)
  expect_spss(east$model_summary$R, 0.046, abs_tol = 0.001)
  expect_spss(east$model_summary$R_squared, 0.002, abs_tol = 0.001)
  expect_spss(east$model_summary$std_error, 1.203, abs_tol = 0.001)

  expect_spss(east$anova$Sum_of_Squares[1], 1.514, abs_tol = 0.01)
  expect_spss(east$anova$F_statistic[1], 1.045, abs_tol = 0.01)
  expect_equal(east$anova$df[2], 486L)

  expect_spss(east$coefficients$B[1], 3.789, abs_tol = 0.001)
  expect_spss(east$coefficients$B[2], -0.003, abs_tol = 0.001)
  expect_spss(east$coefficients$Beta[2], -0.046, abs_tol = 0.001)
  expect_spss(east$coefficients$t[2], -1.022, abs_tol = 0.01)

  # West
  west <- result$groups[[2]]
  expect_equal(west$n, 1949)
  expect_spss(west$model_summary$R, 0.025, abs_tol = 0.001)
  expect_spss(west$model_summary$std_error, 1.139, abs_tol = 0.001)

  expect_spss(west$anova$Sum_of_Squares[1], 1.518, abs_tol = 0.01)
  expect_spss(west$anova$F_statistic[1], 1.170, abs_tol = 0.01)
  expect_equal(west$anova$df[2], 1947L)

  expect_spss(west$coefficients$B[1], 3.708, abs_tol = 0.001)
  expect_spss(west$coefficients$B[2], -0.002, abs_tol = 0.001)
  expect_spss(west$coefficients$Beta[2], -0.025, abs_tol = 0.001)
  expect_spss(west$coefficients$t[2], -1.082, abs_tol = 0.01)
})

# ==============================================================================
# TEST 4b: Weighted, Grouped - Trust items
# ==============================================================================

test_that("Test 4b: Weighted, grouped, trust items regression", {
  data(survey_data)
  result <- linear_regression(
    survey_data %>% dplyr::group_by(region),
    life_satisfaction ~ trust_government + trust_media + trust_science,
    weights = sampling_weight)

  # East
  east <- result$groups[[1]]
  expect_equal(east$n, 424)
  expect_spss(east$model_summary$R, 0.102, abs_tol = 0.001)
  expect_spss(east$model_summary$R_squared, 0.010, abs_tol = 0.001)
  expect_spss(east$model_summary$std_error, 1.204, abs_tol = 0.001)

  expect_spss(east$anova$Sum_of_Squares[1], 6.418, abs_tol = 0.01)
  expect_spss(east$anova$F_statistic[1], 1.475, abs_tol = 0.01)
  expect_equal(east$anova$df[1], 3L)
  expect_equal(east$anova$df[2], 420L)

  expect_spss(east$coefficients$B[1], 4.215, abs_tol = 0.001)
  expect_spss(east$coefficients$B[2], -0.039, abs_tol = 0.001)
  expect_spss(east$coefficients$B[3], -0.062, abs_tol = 0.001)
  expect_spss(east$coefficients$B[4], -0.086, abs_tol = 0.001)

  expect_spss(east$coefficients$Beta[2], -0.038, abs_tol = 0.001)
  expect_spss(east$coefficients$Beta[3], -0.056, abs_tol = 0.001)
  expect_spss(east$coefficients$Beta[4], -0.072, abs_tol = 0.001)

  # West
  west <- result$groups[[2]]
  expect_equal(west$n, 1656)
  expect_spss(west$model_summary$R, 0.059, abs_tol = 0.001)
  expect_spss(west$model_summary$R_squared, 0.003, abs_tol = 0.001)

  expect_spss(west$anova$F_statistic[1], 1.930, abs_tol = 0.01)
  expect_equal(west$anova$df[2], 1652L)

  expect_spss(west$coefficients$B[1], 3.548, abs_tol = 0.001)
  expect_spss(west$coefficients$B[2], 0.005, abs_tol = 0.001)
  expect_spss(west$coefficients$B[3], 0.056, abs_tol = 0.001)
  expect_spss(west$coefficients$B[4], -0.012, abs_tol = 0.001)

  expect_spss(west$coefficients$Beta[2], 0.005, abs_tol = 0.001)
  expect_spss(west$coefficients$Beta[3], 0.058, abs_tol = 0.001)
  expect_spss(west$coefficients$Beta[4], -0.011, abs_tol = 0.001)

  expect_spss(west$coefficients$t[3], 2.356, abs_tol = 0.01)
})

# ==============================================================================
# STRUCTURAL TESTS
# ==============================================================================

test_that("linear_regression returns correct structure", {
  data(survey_data)
  result <- linear_regression(survey_data, life_satisfaction ~ age)

  expect_s3_class(result, "linear_regression")
  expect_true("coefficients" %in% names(result))
  expect_true("model_summary" %in% names(result))
  expect_true("anova" %in% names(result))
  expect_true("descriptives" %in% names(result))
  expect_true("model" %in% names(result))
  expect_true("n" %in% names(result))
  expect_true("formula" %in% names(result))
  expect_true("dependent" %in% names(result))
  expect_true("predictor_names" %in% names(result))
  expect_true("weighted" %in% names(result))

  expect_s3_class(result$coefficients, "tbl_df")
  expect_s3_class(result$anova, "tbl_df")
  expect_s3_class(result$descriptives, "tbl_df")
  expect_s3_class(result$model, "lm")
})

test_that("SPSS-style interface works", {
  data(survey_data)
  result <- linear_regression(survey_data,
    dependent = life_satisfaction,
    predictors = c(trust_government, trust_media))

  expect_s3_class(result, "linear_regression")
  expect_equal(result$dependent, "life_satisfaction")
  expect_equal(result$predictor_names, c("trust_government", "trust_media"))
  expect_equal(result$n, 2156)  # listwise N
})

test_that("Print method works without error", {
  data(survey_data)

  # Ungrouped
  result <- linear_regression(survey_data, life_satisfaction ~ age)
  expect_output(print(result), "Linear Regression:")
  expect_output(print(result), "R2")
  expect_output(print(result), "F\\(")
  expect_output(print(result), "N = ")

  # Weighted
  result_w <- linear_regression(survey_data, life_satisfaction ~ age,
                                weights = sampling_weight)
  expect_output(print(result_w), "\\[Weighted\\]")

  # Grouped
  result_g <- linear_regression(
    survey_data %>% dplyr::group_by(region),
    life_satisfaction ~ age)
  expect_output(print(result_g), "\\[Grouped:")
})

test_that("Confidence intervals are included", {
  data(survey_data)
  result <- linear_regression(survey_data, life_satisfaction ~ age)

  expect_true("CI_lower" %in% names(result$coefficients))
  expect_true("CI_upper" %in% names(result$coefficients))
  expect_true(result$coefficients$CI_lower[1] < result$coefficients$B[1])
  expect_true(result$coefficients$CI_upper[1] > result$coefficients$B[1])
})

test_that("Descriptive statistics match SPSS", {
  data(survey_data)
  result <- linear_regression(survey_data, life_satisfaction ~ age)

  expect_equal(nrow(result$descriptives), 2)
  expect_spss(result$descriptives$Mean[1], 3.63, abs_tol = 0.01)
  expect_spss(result$descriptives$Std.Deviation[1], 1.153, abs_tol = 0.001)
  expect_equal(result$descriptives$N[1], 2421)
})

test_that("Error handling works correctly", {
  data(survey_data)

  expect_error(linear_regression(survey_data))  # No formula or dependent
  expect_error(linear_regression("not_a_df", life_satisfaction ~ age))
  expect_error(linear_regression(survey_data, life_satisfaction ~ nonexistent_var))
})
