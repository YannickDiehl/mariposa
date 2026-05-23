# =============================================================================
# logistic_regression — PROPERTY-BASED VALIDATION (Charter Tier-4)
# =============================================================================
# Purpose: Validate that logistic_regression() correctly implements the
# SPSS LOGISTIC REGRESSION output formulas, without circular self-comparison.
#
# Strategy: where logistic_regression() wraps stats::glm(family=binomial),
# the wrapper's job is to expose specific SPSS-style derived quantities
# (Wald, Exp(B), pseudo-R^2, classification table, Hosmer-Lemeshow). We
# validate each derived quantity against its TEXTBOOK FORMULA evaluated
# independently — NOT against another mariposa call or a glm refit that
# already produces the same intermediate values.
#
# References:
#   - IBM SPSS Statistics Algorithms, "LOGISTIC REGRESSION" chapter
#   - Hosmer & Lemeshow (2013), Applied Logistic Regression, 3rd ed.
#
# No SPSS .txt reference is generated; the textbook formulas serve as the
# oracle. Captured 2026-05-19.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


data(survey_data, envir = environment())
survey_data$high_life <- as.integer(survey_data$life_satisfaction >= 4)
# A clean binary predictor for OR-from-2x2 cross-check
survey_data$is_female <- as.integer(survey_data$gender == "Female")


test_that("Wald = (B/SE)^2 exactly (catches wrapper formula errors)", {
  r <- logistic_regression(survey_data, high_life ~ age + income)
  for (i in seq_len(nrow(r$coef_table))) {
    B  <- as.numeric(r$coef_table$B[i])
    SE <- as.numeric(r$coef_table$S.E.[i])
    W  <- as.numeric(r$coef_table$Wald[i])
    assert_spss(W, (B / SE)^2,
                tier = "spec", what = "statistic",
                label = sprintf("Wald[%s] = (B/SE)^2", r$coef_table$Term[i]))
  }
})


test_that("Sig = pchisq(Wald, df=1, lower.tail=FALSE) exactly", {
  r <- logistic_regression(survey_data, high_life ~ age + income)
  for (i in seq_len(nrow(r$coef_table))) {
    W <- as.numeric(r$coef_table$Wald[i])
    p <- as.numeric(r$coef_table$Sig.[i])
    assert_spss(p, pchisq(W, df = 1, lower.tail = FALSE),
                tier = "spec", what = "p_value",
                label = sprintf("Sig[%s] from chi-sq", r$coef_table$Term[i]))
  }
})


test_that("Exp(B) = exp(B); for binary predictor matches OR from 2x2 table", {
  r <- logistic_regression(survey_data, high_life ~ is_female)

  # Exp(B) should equal exp(B) elementwise
  for (i in seq_len(nrow(r$coef_table))) {
    expB <- as.numeric(r$coef_table$`Exp(B)`[i])
    B    <- as.numeric(r$coef_table$B[i])
    assert_spss(expB, exp(B),
                tier = "spec", what = "statistic",
                label = sprintf("Exp(B) = exp(B) for %s", r$coef_table$Term[i]))
  }

  # For the single binary predictor, Exp(B) should equal the odds ratio
  # computed directly from the 2x2 table — completely independent of glm.
  d <- survey_data[stats::complete.cases(survey_data[, c("high_life", "is_female")]), ]
  tbl <- table(female = d$is_female, high = d$high_life)
  or_2x2 <- (tbl["1", "1"] * tbl["0", "0"]) / (tbl["1", "0"] * tbl["0", "1"])
  expB_is_female <- as.numeric(r$coef_table$`Exp(B)`[r$coef_table$Term == "is_female"])
  assert_spss(expB_is_female, or_2x2,
              tier = "display", precision = 4,
              label = "Exp(B) for binary predictor matches 2x2 OR")
})


test_that("CI for Exp(B) = exp(B +/- z_crit * SE)", {
  r <- logistic_regression(survey_data, high_life ~ age + income, conf.level = 0.95)
  z95 <- qnorm(0.975)
  for (i in seq_len(nrow(r$coef_table))) {
    B  <- as.numeric(r$coef_table$B[i])
    SE <- as.numeric(r$coef_table$S.E.[i])
    lo <- as.numeric(r$coef_table$CI_lower[i])
    hi <- as.numeric(r$coef_table$CI_upper[i])
    assert_spss(lo, exp(B - z95 * SE), tier = "display", precision = 5,
                label = sprintf("CI_lower[%s]", r$coef_table$Term[i]))
    assert_spss(hi, exp(B + z95 * SE), tier = "display", precision = 5,
                label = sprintf("CI_upper[%s]", r$coef_table$Term[i]))
  }
})


test_that("Cox & Snell and Nagelkerke R^2 match textbook formulas", {
  r <- logistic_regression(survey_data, high_life ~ age + income)
  d <- survey_data[stats::complete.cases(survey_data[, c("high_life","age","income")]), ]
  n <- nrow(d)

  # Independent computation of -2LL_null and -2LL_model
  null_fit <- glm(high_life ~ 1, data = d, family = binomial)
  full_fit <- glm(high_life ~ age + income, data = d, family = binomial)
  m2_null <- -2 * as.numeric(logLik(null_fit))
  m2_full <- -2 * as.numeric(logLik(full_fit))

  # Textbook formulas
  cs_expected   <- 1 - exp((m2_full - m2_null) / n)
  cs_max        <- 1 - exp(-m2_null / n)
  nagk_expected <- cs_expected / cs_max
  mcf_expected  <- 1 - (-0.5 * m2_full) / (-0.5 * m2_null)

  assert_spss(r$model_summary$cox_snell_r2, cs_expected,
              tier = "display", precision = 5, label = "Cox & Snell R^2")
  assert_spss(r$model_summary$nagelkerke_r2, nagk_expected,
              tier = "display", precision = 5, label = "Nagelkerke R^2")
  assert_spss(r$model_summary$mcfadden_r2, mcf_expected,
              tier = "display", precision = 5, label = "McFadden R^2")
  assert_spss(r$model_summary$minus2LL, m2_full,
              tier = "display", precision = 3, label = "-2 Log Likelihood")
})


test_that("Omnibus chi-sq = -2LL_null - -2LL_model (independent recomputation)", {
  r <- logistic_regression(survey_data, high_life ~ age + income)
  d <- survey_data[stats::complete.cases(survey_data[, c("high_life","age","income")]), ]

  null_fit <- glm(high_life ~ 1, data = d, family = binomial)
  full_fit <- glm(high_life ~ age + income, data = d, family = binomial)
  expected_chi <- -2 * as.numeric(logLik(null_fit)) - (-2 * as.numeric(logLik(full_fit)))

  assert_spss(r$omnibus_test$chi_sq, expected_chi,
              tier = "display", precision = 3,
              label = "Omnibus chi-sq from likelihood ratio")
  assert_spss_count(r$omnibus_test$df, 2L, label = "Omnibus df = number of predictors")
  assert_spss(r$omnibus_test$p,
              pchisq(expected_chi, df = 2, lower.tail = FALSE),
              tier = "spec", what = "p_value",
              label = "Omnibus p from chi-sq")
})


test_that("Classification accuracy reproducible from cutoff 0.5", {
  r <- logistic_regression(survey_data, high_life ~ age + income)
  d <- survey_data[stats::complete.cases(survey_data[, c("high_life","age","income")]), ]
  fit <- glm(high_life ~ age + income, data = d, family = binomial)
  pred_class <- as.integer(fitted(fit) >= 0.5)
  expected_pct <- mean(pred_class == d$high_life) * 100

  assert_spss(r$classification$overall_pct, expected_pct,
              tier = "display", precision = 2,
              label = "Overall classification %")
  assert_spss_count(r$classification$n_0, sum(d$high_life == 0),
                    label = "Classification N for class 0")
  assert_spss_count(r$classification$n_1, sum(d$high_life == 1),
                    label = "Classification N for class 1")
})


test_that("Structural sanity: single predictor + n + Term names", {
  r <- logistic_regression(survey_data, high_life ~ age)
  expect_equal(nrow(r$coef_table), 2L)  # intercept + age
  expect_setequal(r$coef_table$Term, c("(Intercept)", "age"))
  expect_equal(r$n,
               sum(stats::complete.cases(survey_data[, c("high_life", "age")])))
})
