# =============================================================================
# marginal_effects — internal validation (Charter Tier 4)
# =============================================================================
# SPSS has no AME procedure, so there is no SPSS reference to validate
# against (the function is listed as out of SPSS-validation scope in
# vignettes/_build-spss-compatibility.R). The oracles here are:
#   - the analytic logit AME formula: for a predictor entering the model
#     linearly, AME = wmean( beta_j * mu * (1 - mu) )
#   - predict()-based discrete changes for factor levels
#   - an independent finite-difference delta-method SE recomputation
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)

data(survey_data, envir = environment())
survey_data$high_life <- as.integer(survey_data$life_satisfaction >= 4)


test_that("continuous AME equals the analytic logit formula (unweighted)", {
  m <- logistic_regression(survey_data, high_life ~ age + income)
  ame <- marginal_effects(m)

  p_hat <- fitted(m)
  for (v in c("age", "income")) {
    expected <- mean(coef(m)[[v]] * p_hat * (1 - p_hat))
    got <- ame$results$AME[ame$results$Term == v]
    expect_equal(got, expected, tolerance = 1e-6)
  }
})


test_that("weighted AME uses unrounded frequency weights", {
  m <- logistic_regression(survey_data, high_life ~ age + income,
                           weights = sampling_weight)
  ame <- marginal_effects(m)

  p_hat <- fitted(m)
  w <- m$prior.weights
  expected <- sum(w * coef(m)[["age"]] * p_hat * (1 - p_hat)) / sum(w)
  got <- ame$results$AME[ame$results$Term == "age"]
  expect_equal(got, expected, tolerance = 1e-6)
})


test_that("factor AME equals the predict()-based average discrete change", {
  m <- logistic_regression(survey_data, high_life ~ age + education)
  ame <- marginal_effects(m)

  mf <- m$model
  levs <- levels(mf$education)
  for (lv in levs[-1]) {
    da <- mf; da$education <- factor(lv, levels = levs)
    db <- mf; db$education <- factor(levs[1], levels = levs)
    expected <- mean(predict(m, newdata = da, type = "response") -
                       predict(m, newdata = db, type = "response"))
    term <- sprintf("education: %s vs. %s", lv, levs[1])
    got <- ame$results$AME[ame$results$Term == term]
    expect_equal(got, expected, tolerance = 1e-10)
  }
})


test_that("delta-method SE matches an independent finite-difference Jacobian", {
  m <- logistic_regression(survey_data, high_life ~ age + income)
  ame <- marginal_effects(m)

  # AME_age as a function of beta, recomputed from scratch
  X <- model.matrix(m)
  ame_fn <- function(beta) {
    p <- plogis(as.vector(X %*% beta))
    mean(beta[["age"]] * p * (1 - p))
  }
  beta <- coef(m)
  h <- 1e-6
  grad <- vapply(seq_along(beta), function(k) {
    bp <- beta; bp[k] <- bp[k] + h
    bm <- beta; bm[k] <- bm[k] - h
    (ame_fn(bp) - ame_fn(bm)) / (2 * h)
  }, numeric(1))
  se_expected <- sqrt(as.numeric(t(grad) %*% vcov(m) %*% grad))

  got <- ame$results$SE[ame$results$Term == "age"]
  expect_equal(got, se_expected, tolerance = 1e-4)

  # z / p / CI wiring
  row <- ame$results[ame$results$Term == "age", ]
  expect_equal(row$z, row$AME / row$SE, tolerance = 1e-10)
  expect_equal(row$p_value, 2 * pnorm(-abs(row$z)), tolerance = 1e-10)
  expect_equal(row$CI_lower, row$AME - qnorm(0.975) * row$SE, tolerance = 1e-10)
})


test_that("grouped AMEs equal per-subset results", {
  mg <- survey_data |> group_by(gender) |>
    logistic_regression(high_life ~ age + income)
  ameg <- marginal_effects(mg)

  g <- unique(survey_data$gender)[1]
  sub <- survey_data[survey_data$gender == g, ]
  mu <- logistic_regression(sub, high_life ~ age + income)
  ameu <- marginal_effects(mu)

  row <- ameg$results[ameg$results$gender == g & ameg$results$Term == "age", ]
  expect_equal(row$AME, ameu$results$AME[ameu$results$Term == "age"],
               tolerance = 1e-10)
  expect_equal(row$SE, ameu$results$SE[ameu$results$Term == "age"],
               tolerance = 1e-10)
})


test_that("linear_regression and unsupported objects get clear errors", {
  lin <- linear_regression(survey_data, life_satisfaction ~ age + income)
  expect_error(marginal_effects(lin), "is.*the marginal effect")
  expect_error(marginal_effects(lm(income ~ age, data = survey_data)),
               "not implemented")
})


test_that("three-layer output prints", {
  m <- logistic_regression(survey_data, high_life ~ age + education)
  ame <- marginal_effects(m)

  out <- capture.output(print(ame))
  expect_true(any(grepl("Average Marginal Effects", out, fixed = TRUE)))
  expect_true(any(grepl("education: ", out, fixed = TRUE)))

  s_out <- capture.output(print(summary(ame)))
  expect_true(any(grepl("Delta method", s_out, fixed = TRUE)))
  expect_true(any(grepl("CI Lower", s_out, fixed = TRUE)))

  s_off <- capture.output(print(summary(ame, effects = FALSE)))
  expect_false(any(grepl("CI Lower", s_off, fixed = TRUE)))
})
