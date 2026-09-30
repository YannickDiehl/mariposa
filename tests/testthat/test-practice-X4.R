# =============================================================================
# 0.7.4 practice-test fixes — batch X4 (SPSS parity, unweighted):
# ancova() / levene_test() Levene test, efa() rotations and ML extraction
# =============================================================================
# One block per finding. The SPSS reference numbers themselves are asserted
# in the *-spss-validation.R files (assert_spss(), Charter tiers); the blocks
# here pin the algorithms against independent oracles.

data(survey_data, envir = environment())

# --- X4-ANCOVA-LEVENE: Levene's test on the full-model residuals -------------

# SPSS UNIANOVA (/PRINT HOMOGENEITY) tests the equality of the ERROR
# variances: a one-way ANOVA of the absolute residuals of the full model
# (covariates + factors) across the design cells.
.levene_resid_oracle <- function(dv, factors, covariates, data = survey_data) {
  d <- as.data.frame(data)
  d <- d[stats::complete.cases(d[c(dv, factors, covariates)]), ]
  fml <- stats::reformulate(c(covariates, paste(factors, collapse = "*")), dv)
  z <- abs(stats::residuals(stats::lm(fml, data = d)))
  cell <- interaction(d[factors], drop = TRUE)
  stats::anova(stats::lm(z ~ cell))
}

test_that("X4-ANCOVA-LEVENE: unweighted Levene uses the residuals of the covariate model", {
  # mariposa centred the DV on the raw cell means and ignored the covariate:
  # F = 1.277 instead of SPSS 1.306 for life_satisfaction BY gender WITH age.
  r <- ancova(survey_data, dv = life_satisfaction, between = gender, covariate = age)
  ref <- .levene_resid_oracle("life_satisfaction", "gender", "age")
  expect_equal(r$levene_test$f, ref[1, "F value"], tolerance = 1e-10)
  expect_equal(r$levene_test$p, ref[1, "Pr(>F)"], tolerance = 1e-10)
  expect_identical(r$levene_test$df1, 1L)
  expect_identical(r$levene_test$df2, as.integer(ref[2, "Df"]))

  # two factors and two covariates: the cells are the factor combinations
  r2 <- ancova(survey_data, dv = income, between = c(gender, education),
               covariate = c(age, political_orientation))
  ref2 <- .levene_resid_oracle("income", c("gender", "education"),
                               c("age", "political_orientation"))
  expect_equal(r2$levene_test$f, ref2[1, "F value"], tolerance = 1e-10)
  expect_identical(r2$levene_test$df1, 7L)
})

test_that("X4-ANCOVA-LEVENE: weighted ANCOVA still reports its Levene test", {
  # The weighted (/REGWGT) path is left unchanged pending a WEIGHT BY run.
  r <- ancova(survey_data, dv = life_satisfaction, between = gender,
              covariate = age, weights = sampling_weight)
  expect_true(is.finite(r$levene_test$f))
  expect_identical(r$levene_test$df1, 1L)
  expect_output(print(summary(r)), "Levene's Test of Equality of Error Variances")
})

test_that("X4-ANCOVA-LEVENE: levene_test() works on ancova() results", {
  # levene_test(ancova_result) failed: "not available for objects of class
  # ancova" (the error even listed only oneway/factorial/t_test).
  r <- ancova(survey_data, dv = life_satisfaction, between = c(gender, region),
              covariate = age)
  lv <- levene_test(r)
  expect_s3_class(lv, "levene_test")
  expect_equal(lv$results$F_statistic, r$levene_test$f)
  expect_equal(lv$results$p_value, r$levene_test$p)
  expect_identical(lv$results$df1, r$levene_test$df1)
  expect_identical(lv$results$Variable, "life_satisfaction")
  expect_identical(lv$group, "gender * region")
  expect_output(print(lv), "Levene's Test: life_satisfaction by gender \\* region")
  out <- capture.output(summary(lv))
  expect_true(any(grepl("Levene Statistic", out)))
  # factorial design: no Welch recommendation
  expect_false(any(grepl("Welch", out)))

  # SPSS UNIANOVA has only the mean-based test for a model with covariates
  expect_error(levene_test(r, center = "median"), "mean")

  # grouped ancova: one row per group, with the group keys
  g <- ancova(dplyr::group_by(survey_data, region), dv = life_satisfaction,
              between = gender, covariate = age)
  lg <- levene_test(g)
  expect_identical(nrow(lg$results), 2L)
  expect_true("region" %in% names(lg$results))
  east <- ancova(dplyr::filter(survey_data, region == "East"),
                 dv = life_satisfaction, between = gender, covariate = age)
  expect_equal(lg$results$F_statistic[as.character(lg$results$region) == "East"],
               east$levene_test$f)
  expect_output(print(lg), "region = East")

  # weighted ancova: the method returns the stored (weighted) test
  w <- ancova(survey_data, dv = life_satisfaction, between = gender,
              covariate = age, weights = sampling_weight)
  expect_equal(levene_test(w)$results$F_statistic, w$levene_test$f)
  expect_identical(levene_test(w)$weights, "sampling_weight")
})
