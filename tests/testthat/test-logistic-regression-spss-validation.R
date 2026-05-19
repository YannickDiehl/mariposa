# =============================================================================
# logistic_regression — VALIDATION (Charter-compliant, R-only Tier-4)
# =============================================================================
# Purpose: Validate mariposa::logistic_regression() output stability.
#
# No SPSS reference exists for this function. mariposa logistic_regression
# wraps glm(family = binomial); the validation reference is glm() directly.
# Inline R-only baselines captured 2026-05-19.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


data(survey_data, envir = environment())
survey_data$high_life <- as.integer(survey_data$life_satisfaction >= 4)


test_that("logistic_regression high_life ~ age + income — coefficients match glm", {
  r <- logistic_regression(survey_data, high_life ~ age + income)

  # Run base glm for ground truth
  d_complete <- survey_data[stats::complete.cases(survey_data[, c("high_life", "age", "income")]), ]
  glm_fit <- glm(high_life ~ age + income, data = d_complete, family = binomial)
  glm_coefs <- coef(summary(glm_fit))

  # Coefficients should match base glm exactly
  for (term in c("(Intercept)", "age", "income")) {
    actual_B <- r$coefficients$B[r$coefficients$Term == term]
    expected_B <- glm_coefs[term, "Estimate"]
    assert_spss(as.numeric(actual_B), expected_B,
                tier = "display", precision = 5,
                label = sprintf("B[%s]", term))

    actual_SE <- r$coefficients$S.E.[r$coefficients$Term == term]
    expected_SE <- glm_coefs[term, "Std. Error"]
    assert_spss(as.numeric(actual_SE), expected_SE,
                tier = "display", precision = 5,
                label = sprintf("SE[%s]", term))
  }

  # Wald = (B/SE)^2 for SPSS convention
  for (term in c("age", "income")) {
    actual_wald <- r$coefficients$Wald[r$coefficients$Term == term]
    actual_B    <- r$coefficients$B[r$coefficients$Term == term]
    actual_SE   <- r$coefficients$S.E.[r$coefficients$Term == term]
    expected_wald <- (as.numeric(actual_B) / as.numeric(actual_SE))^2
    assert_spss(as.numeric(actual_wald), expected_wald,
                tier = "display", precision = 3,
                label = sprintf("Wald[%s] = (B/SE)^2", term))
  }
})


test_that("logistic_regression with single predictor — structural", {
  r <- logistic_regression(survey_data, high_life ~ age)
  expect_equal(nrow(r$coefficients), 2L)  # intercept + age
  expect_equal(r$n, sum(stats::complete.cases(survey_data[, c("high_life", "age")])))
})


test_that("logistic_regression exp(B) = exp(B) consistency", {
  r <- logistic_regression(survey_data, high_life ~ age + income)
  for (i in seq_len(nrow(r$coefficients))) {
    expect_equal(r$coefficients$`Exp(B)`[i], exp(r$coefficients$B[i]),
                 tolerance = 1e-6,
                 label = sprintf("exp(B) consistency for %s", r$coefficients$Term[i]))
  }
})
