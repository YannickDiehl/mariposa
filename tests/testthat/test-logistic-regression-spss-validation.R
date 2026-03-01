# =============================================================================
# LOGISTIC REGRESSION - VALIDATION TESTS
# =============================================================================
# Validates logistic_regression() function
# Reference values computed from R's glm(family=binomial)
# NOTE: SPSS validation values can be added once SPSS output is available
# Test structure follows standard mariposa pattern:
#   1a-1d: Unweighted/Ungrouped
#   2a-2c: Weighted/Ungrouped
#   3a-3b: Unweighted/Grouped
#   4a-4b: Weighted/Grouped
#   Structural tests
# =============================================================================

# Helper function for tolerance-based testing
expect_ref <- function(actual, expected, abs_tol = 0.001,
                       label = deparse(substitute(actual))) {
  diff <- abs(actual - expected)
  msg <- sprintf("%s: R=%.6f REF=%.6f diff=%.6f tol=%.6f",
                 label, actual, expected, diff, abs_tol)
  expect_true(diff <= abs_tol, label = msg)
}

# ============================================================================
# DATA PREPARATION
# ============================================================================
# Create binary DVs matching SPSS RECODE syntax:
# RECODE life_satisfaction (1 thru 3=0)(4 thru 5=1) INTO high_satisfaction.
# RECODE gender ('Male'=0)('Female'=1) INTO female.

data(survey_data)
survey_data$high_satisfaction <- ifelse(survey_data$life_satisfaction >= 4, 1, 0)
survey_data$female <- ifelse(survey_data$gender == "Female", 1, 0)

# ============================================================================
# TEST 1: UNWEIGHTED / UNGROUPED
# ============================================================================

test_that("Test 1a: Unweighted, ungrouped, bivariate (high_satisfaction ~ age)", {
  result <- logistic_regression(survey_data, high_satisfaction ~ age)

  expect_equal(result$n, 2421)

  # Model summary
  expect_ref(result$model_summary$minus2LL, 3298.239, abs_tol = 0.01)
  expect_ref(result$model_summary$cox_snell_r2, 0.000117, abs_tol = 0.001)
  expect_ref(result$model_summary$nagelkerke_r2, 0.000157, abs_tol = 0.001)
  expect_ref(result$model_summary$mcfadden_r2, 0.0000858, abs_tol = 0.001)

  # Omnibus test
  expect_ref(result$omnibus_test$chi_sq, 0.283, abs_tol = 0.01)
  expect_equal(result$omnibus_test$df, 1)
  expect_ref(result$omnibus_test$p, 0.5948, abs_tol = 0.001)

  # Coefficients
  coefs <- result$coefficients

  # Intercept
  expect_ref(coefs$B[1], 0.375765, abs_tol = 0.001)
  expect_ref(coefs$S.E.[1], 0.129293, abs_tol = 0.001)
  expect_ref(coefs$Wald[1], 8.449, abs_tol = 0.01)
  expect_equal(coefs$df[1], 1L)
  expect_ref(coefs$Sig.[1], 0.00365, abs_tol = 0.001)
  expect_ref(coefs$`Exp(B)`[1], 1.456, abs_tol = 0.001)

  # age
  expect_ref(coefs$B[2], -0.001287, abs_tol = 0.001)
  expect_ref(coefs$S.E.[2], 0.002420, abs_tol = 0.001)
  expect_ref(coefs$Wald[2], 0.283, abs_tol = 0.01)
  expect_ref(coefs$Sig.[2], 0.5948, abs_tol = 0.001)
  expect_ref(coefs$`Exp(B)`[2], 0.9987, abs_tol = 0.001)

  # Classification
  expect_ref(result$classification$overall_pct, 57.7, abs_tol = 0.5)
})


test_that("Test 1b: Unweighted, ungrouped, trust items", {
  result <- logistic_regression(survey_data,
    high_satisfaction ~ trust_government + trust_media + trust_science)

  expect_equal(result$n, 2066)

  # Model summary
  expect_ref(result$model_summary$minus2LL, 2801.242, abs_tol = 0.01)

  # Omnibus test
  expect_ref(result$omnibus_test$chi_sq, 4.623, abs_tol = 0.01)
  expect_equal(result$omnibus_test$df, 3)

  # Coefficients
  coefs <- result$coefficients
  expect_equal(nrow(coefs), 4)  # intercept + 3 predictors

  # trust_media (significant predictor)
  idx_media <- which(coefs$Term == "trust_media")
  expect_ref(coefs$B[idx_media], 0.0805, abs_tol = 0.001)
  expect_ref(coefs$S.E.[idx_media], 0.0389, abs_tol = 0.001)
  expect_ref(coefs$Wald[idx_media], 4.27, abs_tol = 0.01)
  expect_ref(coefs$`Exp(B)`[idx_media], 1.0838, abs_tol = 0.001)

  # trust_government (non-significant)
  idx_gov <- which(coefs$Term == "trust_government")
  expect_ref(coefs$B[idx_gov], -0.00767, abs_tol = 0.001)

  # trust_science
  idx_sci <- which(coefs$Term == "trust_science")
  expect_ref(coefs$B[idx_sci], -0.0255, abs_tol = 0.001)
})


test_that("Test 1c: Unweighted, ungrouped, 4 predictors", {
  result <- logistic_regression(survey_data,
    high_satisfaction ~ age + income + environmental_concern + political_orientation)

  expect_equal(result$n, 1865)

  # Model summary
  expect_ref(result$model_summary$minus2LL, 2194.155, abs_tol = 0.01)
  expect_ref(result$model_summary$nagelkerke_r2, 0.228163, abs_tol = 0.001)

  # Omnibus test (significant model)
  expect_ref(result$omnibus_test$chi_sq, 346.942, abs_tol = 0.1)
  expect_equal(result$omnibus_test$df, 4)
  expect_true(result$omnibus_test$p < 0.001)

  # Coefficients
  coefs <- result$coefficients
  expect_equal(nrow(coefs), 5)

  # income (highly significant predictor)
  idx_inc <- which(coefs$Term == "income")
  expect_ref(coefs$B[idx_inc], 0.000742, abs_tol = 0.0001)
  expect_ref(coefs$Wald[idx_inc], 254.501, abs_tol = 1.0)
  expect_true(coefs$Sig.[idx_inc] < 0.001)
  expect_ref(coefs$`Exp(B)`[idx_inc], 1.000742, abs_tol = 0.001)

  # Classification (should be better than chance)
  expect_true(result$classification$overall_pct > 60)
})


test_that("Test 1d: Unweighted, ungrouped, different DV (female)", {
  result <- logistic_regression(survey_data,
    female ~ age + income + education)

  expect_equal(result$n, 2186)

  # Model summary
  expect_ref(result$model_summary$minus2LL, 3025.557, abs_tol = 0.01)

  # Coefficients
  coefs <- result$coefficients
  expect_equal(nrow(coefs), 4)

  # Intercept
  expect_ref(coefs$B[1], 0.0984, abs_tol = 0.001)
  expect_ref(coefs$S.E.[1], 0.178, abs_tol = 0.001)

  # All predictors should be non-significant for gender
  expect_true(all(coefs$Sig.[-1] > 0.05))
})


# ============================================================================
# TEST 2: WEIGHTED / UNGROUPED
# ============================================================================

test_that("Test 2a: Weighted, ungrouped, bivariate", {
  result <- logistic_regression(survey_data, high_satisfaction ~ age,
                                weights = sampling_weight)

  expect_equal(result$n, 2437)

  # Model summary
  expect_ref(result$model_summary$minus2LL, 3298.256, abs_tol = 0.01)
  expect_ref(result$model_summary$cox_snell_r2, 0.000116, abs_tol = 0.001)
  expect_ref(result$model_summary$nagelkerke_r2, 0.000156, abs_tol = 0.001)

  # Omnibus test
  expect_ref(result$omnibus_test$chi_sq, 0.283, abs_tol = 0.01)
  expect_equal(result$omnibus_test$df, 1)

  # Coefficients
  coefs <- result$coefficients

  # Intercept
  expect_ref(coefs$B[1], 0.371328, abs_tol = 0.001)
  expect_ref(coefs$S.E.[1], 0.128011, abs_tol = 0.001)
  expect_ref(coefs$Wald[1], 8.414, abs_tol = 0.01)
  expect_ref(coefs$`Exp(B)`[1], 1.4497, abs_tol = 0.001)

  # age
  expect_ref(coefs$B[2], -0.001305, abs_tol = 0.001)
  expect_ref(coefs$S.E.[2], 0.002396, abs_tol = 0.001)
  expect_ref(coefs$Wald[2], 0.297, abs_tol = 0.01)
  expect_ref(coefs$`Exp(B)`[2], 0.9987, abs_tol = 0.001)

  # Classification
  expect_ref(result$classification$overall_pct, 57.6, abs_tol = 0.5)
})


test_that("Test 2b: Weighted, ungrouped, trust items", {
  result <- logistic_regression(survey_data,
    high_satisfaction ~ trust_government + trust_media + trust_science,
    weights = sampling_weight)

  expect_equal(result$n, 2080)

  # Model summary
  expect_ref(result$model_summary$minus2LL, 2801.337, abs_tol = 0.01)

  # Omnibus
  expect_ref(result$omnibus_test$chi_sq, 4.535, abs_tol = 0.01)
  expect_equal(result$omnibus_test$df, 3)

  # Coefficients
  coefs <- result$coefficients

  idx_media <- which(coefs$Term == "trust_media")
  expect_ref(coefs$B[idx_media], 0.077397, abs_tol = 0.001)
  expect_ref(coefs$S.E.[idx_media], 0.038713, abs_tol = 0.001)
  expect_ref(coefs$Wald[idx_media], 3.997, abs_tol = 0.01)
  expect_ref(coefs$`Exp(B)`[idx_media], 1.0805, abs_tol = 0.001)
})


test_that("Test 2c: Weighted, ungrouped, 4 predictors", {
  result <- logistic_regression(survey_data,
    high_satisfaction ~ age + income + environmental_concern + political_orientation,
    weights = sampling_weight)

  expect_equal(result$n, 1877)

  # Model summary
  expect_ref(result$model_summary$minus2LL, 2194.271, abs_tol = 0.01)
  expect_ref(result$model_summary$nagelkerke_r2, 0.227451, abs_tol = 0.001)

  # Omnibus (significant)
  expect_ref(result$omnibus_test$chi_sq, 346.826, abs_tol = 0.1)
  expect_true(result$omnibus_test$p < 0.001)

  # income (significant)
  coefs <- result$coefficients
  idx_inc <- which(coefs$Term == "income")
  expect_ref(coefs$B[idx_inc], 0.000754, abs_tol = 0.0001)
  expect_ref(coefs$Wald[idx_inc], 258.936, abs_tol = 1.0)
  expect_ref(coefs$`Exp(B)`[idx_inc], 1.0008, abs_tol = 0.001)
})


# ============================================================================
# TEST 3: UNWEIGHTED / GROUPED
# ============================================================================

test_that("Test 3a: Unweighted, grouped, bivariate", {
  result <- logistic_regression(
    survey_data %>% dplyr::group_by(region),
    high_satisfaction ~ age)

  expect_true(result$is_grouped)
  expect_equal(length(result$groups), 2)

  # East
  east <- result$groups[[1]]
  expect_equal(east$n, 465)
  expect_ref(east$model_summary$minus2LL, 631.079, abs_tol = 0.01)
  expect_ref(east$omnibus_test$chi_sq, 0.738, abs_tol = 0.01)

  # Intercept
  expect_ref(east$coefficients$B[1], 0.575601, abs_tol = 0.001)
  expect_ref(east$coefficients$S.E.[1], 0.297023, abs_tol = 0.001)
  expect_ref(east$coefficients$`Exp(B)`[1], 1.7782, abs_tol = 0.001)

  # age
  expect_ref(east$coefficients$B[2], -0.004641, abs_tol = 0.001)
  expect_ref(east$coefficients$`Exp(B)`[2], 0.9954, abs_tol = 0.001)

  # West
  west <- result$groups[[2]]
  expect_equal(west$n, 1956)
  expect_ref(west$model_summary$minus2LL, 2666.595, abs_tol = 0.01)
  expect_ref(west$omnibus_test$chi_sq, 0.031, abs_tol = 0.01)

  expect_ref(west$coefficients$B[1], 0.329000, abs_tol = 0.001)
  expect_ref(west$coefficients$B[2], -0.000477, abs_tol = 0.001)
  expect_ref(west$coefficients$`Exp(B)`[2], 0.9995, abs_tol = 0.001)
})


test_that("Test 3b: Unweighted, grouped, trust items", {
  result <- logistic_regression(
    survey_data %>% dplyr::group_by(region),
    high_satisfaction ~ trust_government + trust_media + trust_science)

  # East
  east <- result$groups[[1]]
  expect_equal(east$n, 404)
  expect_ref(east$model_summary$minus2LL, 542.852, abs_tol = 0.01)
  expect_ref(east$omnibus_test$chi_sq, 3.579, abs_tol = 0.01)

  # Intercept
  expect_ref(east$coefficients$B[1], 1.203817, abs_tol = 0.001)
  expect_ref(east$coefficients$`Exp(B)`[1], 3.3328, abs_tol = 0.01)

  # trust_science (largest predictor in East)
  idx_sci <- which(east$coefficients$Term == "trust_science")
  expect_ref(east$coefficients$B[idx_sci], -0.168337, abs_tol = 0.001)
  expect_ref(east$coefficients$`Exp(B)`[idx_sci], 0.8451, abs_tol = 0.001)

  # West
  west <- result$groups[[2]]
  expect_equal(west$n, 1662)
  expect_ref(west$model_summary$minus2LL, 2252.047, abs_tol = 0.01)
  expect_ref(west$omnibus_test$chi_sq, 7.258, abs_tol = 0.01)

  # trust_media (significant in West)
  idx_media <- which(west$coefficients$Term == "trust_media")
  expect_ref(west$coefficients$B[idx_media], 0.114986, abs_tol = 0.001)
  expect_ref(west$coefficients$`Exp(B)`[idx_media], 1.1219, abs_tol = 0.001)
})


# ============================================================================
# TEST 4: WEIGHTED / GROUPED
# ============================================================================

test_that("Test 4a: Weighted, grouped, bivariate", {
  result <- logistic_regression(
    survey_data %>% dplyr::group_by(region),
    high_satisfaction ~ age, weights = sampling_weight)

  expect_true(result$is_grouped)
  expect_true(result$weighted)

  # East
  east <- result$groups[[1]]
  expect_equal(east$n, 488)
  expect_ref(east$model_summary$minus2LL, 631.112, abs_tol = 0.01)
  expect_ref(east$omnibus_test$chi_sq, 0.712, abs_tol = 0.01)

  expect_ref(east$coefficients$B[1], 0.626957, abs_tol = 0.001)
  expect_ref(east$coefficients$`Exp(B)`[1], 1.8719, abs_tol = 0.001)
  expect_ref(east$coefficients$B[2], -0.005428, abs_tol = 0.001)
  expect_ref(east$coefficients$`Exp(B)`[2], 0.9946, abs_tol = 0.001)

  # West
  west <- result$groups[[2]]
  expect_equal(west$n, 1949)
  expect_ref(west$model_summary$minus2LL, 2666.639, abs_tol = 0.01)
  expect_ref(west$omnibus_test$chi_sq, 0.025, abs_tol = 0.01)

  expect_ref(west$coefficients$B[1], 0.309985, abs_tol = 0.001)
  expect_ref(west$coefficients$B[2], -0.000278, abs_tol = 0.001)
})


test_that("Test 4b: Weighted, grouped, trust items", {
  result <- logistic_regression(
    survey_data %>% dplyr::group_by(region),
    high_satisfaction ~ trust_government + trust_media + trust_science,
    weights = sampling_weight)

  # East
  east <- result$groups[[1]]
  expect_equal(east$n, 424)
  expect_ref(east$model_summary$minus2LL, 542.971, abs_tol = 0.01)
  expect_ref(east$model_summary$nagelkerke_r2, 0.011263, abs_tol = 0.001)
  expect_ref(east$omnibus_test$chi_sq, 3.473, abs_tol = 0.01)

  expect_ref(east$coefficients$B[1], 1.362019, abs_tol = 0.001)
  expect_ref(east$coefficients$`Exp(B)`[1], 3.9041, abs_tol = 0.01)

  idx_sci <- which(east$coefficients$Term == "trust_science")
  expect_ref(east$coefficients$B[idx_sci], -0.195771, abs_tol = 0.001)
  expect_ref(east$coefficients$`Exp(B)`[idx_sci], 0.8222, abs_tol = 0.001)

  # West
  west <- result$groups[[2]]
  expect_equal(west$n, 1656)
  expect_ref(west$model_summary$minus2LL, 2252.128, abs_tol = 0.01)
  expect_ref(west$model_summary$nagelkerke_r2, 0.005831, abs_tol = 0.001)
  expect_ref(west$omnibus_test$chi_sq, 7.204, abs_tol = 0.01)

  idx_media <- which(west$coefficients$Term == "trust_media")
  expect_ref(west$coefficients$B[idx_media], 0.113331, abs_tol = 0.001)
  expect_ref(west$coefficients$`Exp(B)`[idx_media], 1.1200, abs_tol = 0.001)
})


# ============================================================================
# STRUCTURAL TESTS
# ============================================================================

test_that("S3 class is correct", {
  result <- logistic_regression(survey_data, high_satisfaction ~ age)
  expect_s3_class(result, "logistic_regression")
})


test_that("SPSS-style interface works", {
  result <- logistic_regression(survey_data,
    dependent = high_satisfaction,
    predictors = c(trust_government, trust_media))

  expect_s3_class(result, "logistic_regression")
  expect_equal(result$dependent, "high_satisfaction")
  expect_equal(result$predictor_names, c("trust_government", "trust_media"))
  expect_true(result$n > 0)
})


test_that("Print method works without error", {
  # Ungrouped
  result <- logistic_regression(survey_data, high_satisfaction ~ age)
  expect_output(print(result), "Logistic Regression Results")
  expect_output(print(result), "Model Summary")
  expect_output(print(result), "Variables in the Equation")
  expect_output(print(result), "Classification Table")
  expect_output(print(result), "Omnibus Tests")
  expect_output(print(result), "Hosmer and Lemeshow")

  # Weighted
  result_w <- logistic_regression(survey_data, high_satisfaction ~ age,
                                  weights = sampling_weight)
  expect_output(print(result_w), "Weighted Logistic Regression")

  # Grouped
  result_g <- logistic_regression(
    survey_data %>% dplyr::group_by(region),
    high_satisfaction ~ age)
  expect_output(print(result_g), "Grouped by")
  expect_output(print(result_g), "region = East")
  expect_output(print(result_g), "region = West")
})


test_that("Confidence intervals for odds ratios are correct", {
  result <- logistic_regression(survey_data, high_satisfaction ~ age)

  # CI should bracket Exp(B) for non-intercept terms
  expect_true(result$coefficients$CI_lower[2] < result$coefficients$`Exp(B)`[2])
  expect_true(result$coefficients$CI_upper[2] > result$coefficients$`Exp(B)`[2])
})


test_that("Factor DV is handled correctly", {
  # Create factor DV
  d <- survey_data
  d$satisfaction_cat <- factor(ifelse(d$life_satisfaction >= 4, "High", "Low"),
                               levels = c("Low", "High"))
  result <- logistic_regression(d, satisfaction_cat ~ age)

  expect_s3_class(result, "logistic_regression")
  expect_true(result$n > 0)
  # Should give same results as numeric 0/1
  result_num <- logistic_regression(survey_data, high_satisfaction ~ age)
  expect_equal(result$coefficients$B, result_num$coefficients$B, tolerance = 0.001)
})


test_that("Hosmer-Lemeshow test is computed", {
  result <- logistic_regression(survey_data,
    high_satisfaction ~ age + income + environmental_concern)

  expect_true(!is.null(result$hosmer_lemeshow))
  expect_true(result$hosmer_lemeshow$chi_sq >= 0)
  expect_true(result$hosmer_lemeshow$df > 0)
  expect_true(result$hosmer_lemeshow$p >= 0 && result$hosmer_lemeshow$p <= 1)
})


test_that("Error handling works", {
  # Non-binary DV
  expect_error(
    logistic_regression(survey_data, life_satisfaction ~ age),
    "binary"
  )

  # Missing formula and dependent
  expect_error(
    logistic_regression(survey_data),
    "formula.*dependent"
  )

  # Non-existent variable
  expect_error(
    logistic_regression(survey_data, high_satisfaction ~ nonexistent_var),
    "not found"
  )
})


test_that("Omnibus test matches deviance difference", {
  result <- logistic_regression(survey_data, high_satisfaction ~ age + income)

  # Omnibus chi-sq should equal null deviance - residual deviance
  # (i.e., -2LL_null - (-2LL_model))
  null_model <- glm(high_satisfaction ~ 1, data = survey_data[complete.cases(
    survey_data[, c("high_satisfaction", "age", "income")]), ],
    family = binomial())
  full_model <- glm(high_satisfaction ~ age + income, data = survey_data[complete.cases(
    survey_data[, c("high_satisfaction", "age", "income")]), ],
    family = binomial())

  expected_chi <- deviance(null_model) - deviance(full_model)
  expect_ref(result$omnibus_test$chi_sq, expected_chi, abs_tol = 0.001)
})


test_that("Exp(B) equals exp of B coefficients", {
  result <- logistic_regression(survey_data,
    high_satisfaction ~ age + trust_government + trust_media)

  for (i in seq_len(nrow(result$coefficients))) {
    expect_equal(result$coefficients$`Exp(B)`[i],
                 exp(result$coefficients$B[i]),
                 tolerance = 1e-6)
  }
})


test_that("Pseudo R-squared values are in valid range", {
  result <- logistic_regression(survey_data,
    high_satisfaction ~ age + income + education)

  # All pseudo R² should be between 0 and 1

  expect_true(result$model_summary$cox_snell_r2 >= 0)
  expect_true(result$model_summary$cox_snell_r2 <= 1)
  expect_true(result$model_summary$nagelkerke_r2 >= 0)
  expect_true(result$model_summary$nagelkerke_r2 <= 1)
  expect_true(result$model_summary$mcfadden_r2 >= 0)
  expect_true(result$model_summary$mcfadden_r2 <= 1)

  # Nagelkerke >= Cox & Snell (by definition)
  expect_true(result$model_summary$nagelkerke_r2 >= result$model_summary$cox_snell_r2)
})


test_that("Classification percentages are in valid range", {
  result <- logistic_regression(survey_data,
    high_satisfaction ~ age + income)

  expect_true(result$classification$pct_correct_0 >= 0 &&
              result$classification$pct_correct_0 <= 100)
  expect_true(result$classification$pct_correct_1 >= 0 &&
              result$classification$pct_correct_1 <= 100)
  expect_true(result$classification$overall_pct >= 0 &&
              result$classification$overall_pct <= 100)
})
