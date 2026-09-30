# =============================================================================
# Practice-test regressions: correlation & regression batch (REG-*, EDGE-17)
# =============================================================================
# Each block names the finding ID from the 2026-09 field report and what was
# wrong before the fix.

data(survey_data)

.reg_sd2 <- function() {
  d <- survey_data
  d$high_sat <- as.integer(d$life_satisfaction >= 4)
  d
}

# REG-01: a formula longer than ~70 characters deparsed to two lines - the
# compact title was printed twice and summary() aborted in
# print_info_section() ("'length = 2' in coercion to 'logical(1)'").
test_that("REG-01: long formulas print on one line in every regression output", {
  f <- life_satisfaction ~ age + income + trust_government + trust_media + trust_science
  m <- linear_regression(survey_data, f)
  out <- capture.output(print(m))
  expect_equal(sum(grepl("^Linear Regression:", out)), 1L)
  expect_true(any(grepl("trust_media + trust_science", out, fixed = TRUE)))
  expect_no_error(out_s <- capture.output(print(summary(m))))
  expect_true(any(grepl("Formula: .*trust_science", out_s)))

  d <- .reg_sd2()
  fl <- high_sat ~ age + income + trust_government + trust_media + trust_science
  ml <- logistic_regression(d, fl)
  out <- capture.output(print(ml))
  expect_equal(sum(grepl("^Logistic Regression:", out)), 1L)
  expect_no_error(capture.output(print(summary(ml))))

  me <- marginal_effects(ml)
  out <- capture.output(print(me))
  expect_equal(sum(grepl("^Average Marginal Effects:", out)), 1L)
  expect_no_error(capture.output(print(summary(me))))

  g <- dplyr::group_by(survey_data, region)
  mg <- linear_regression(g, f)
  expect_no_error(capture.output(print(summary(mg))))
  expect_equal(sum(grepl("^Linear Regression:", capture.output(print(mg)))), 1L)
})

# REG-02: confint()/profile() failed on every logistic_regression result
# ("falsche Anzahl von Dimensionen"): profiling called summary(), which
# dispatched to mariposa's SPSS-style summary instead of summary.glm().
test_that("REG-02: confint() and profile() work on logistic_regression", {
  d <- .reg_sd2()
  m <- logistic_regression(d, high_sat ~ age + gender)
  ci <- confint(m)
  expect_true(is.matrix(ci))
  expect_equal(dim(ci), c(3L, 2L))
  # Default = Wald, the interval SPSS prints as "95% C.I. for EXP(B)" and
  # the one summary() shows (exponentiated)
  expect_equal(unname(exp(ci[-1, 1])), unname(m$coef_table$CI_lower[-1]))
  expect_equal(unname(exp(ci[-1, 2])), unname(m$coef_table$CI_upper[-1]))
  ref <- stats::glm(high_sat ~ age + gender, family = stats::binomial(), data = d)
  expect_equal(unname(ci), unname(stats::confint.default(ref)))
  # Profile-likelihood intervals on request (glm's method)
  ci_p <- suppressMessages(confint(m, method = "profile"))
  expect_equal(unname(ci_p), unname(suppressMessages(confint(ref))),
               tolerance = 1e-6)
  expect_s3_class(profile(m), "profile")

  # Weighted: no leaked non-integer warnings; broom's conf.int agrees
  mw <- logistic_regression(d, high_sat ~ age + gender, weights = sampling_weight)
  expect_no_warning(ciw <- confint(mw))
  expect_no_warning(suppressMessages(confint(mw, method = "profile")))
  skip_if_not_installed("broom")
  expect_no_warning(td <- broom::tidy(mw, conf.int = TRUE))
  expect_equal(td$conf.low, unname(ciw[, 1]))
  td_or <- broom::tidy(mw, conf.int = TRUE, exponentiate = TRUE)
  expect_equal(td_or$conf.high[-1], unname(mw$coef_table$CI_upper[-1]))
})
