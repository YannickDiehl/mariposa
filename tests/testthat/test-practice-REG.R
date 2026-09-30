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

# Numerical AME oracle: average centered difference of glm predictions on
# the original data, perturbing one raw variable
.ame_oracle <- function(g, dat, v, h = 1e-5) {
  a1 <- dat; a1[[v]] <- a1[[v]] + h
  a0 <- dat; a0[[v]] <- a0[[v]] - h
  mean((stats::predict(g, a1, type = "response") -
          stats::predict(g, a0, type = "response")) / (2 * h))
}

# REG-03: marginal_effects() perturbed one column of model$model; for
# transformed terms (I(x^2), log(x), poly(x)) the transformed column was not
# recomputed (AME of inc_k 0.466 instead of 0.173), and variables that only
# enter transformed, character and logical predictors were dropped silently.
test_that("REG-03: AMEs are rebuilt from the original data for every term type", {
  d <- .reg_sd2()
  d$inc_k <- d$income / 1000
  d$gender_chr <- as.character(d$gender)
  d$female <- d$gender == "Female"
  dat <- d[stats::complete.cases(d[c("high_sat", "inc_k", "age")]), ]

  f5 <- high_sat ~ inc_k + I(inc_k^2)
  me5 <- marginal_effects(logistic_regression(d, f5))$results
  g5 <- stats::glm(f5, family = stats::binomial(), data = dat)
  expect_equal(me5$AME[me5$Term == "inc_k"], .ame_oracle(g5, dat, "inc_k"),
               tolerance = 1e-6)
  expect_equal(nrow(me5), 1L)

  # Variable entering only transformed: log(), poly(), I()
  for (f in list(high_sat ~ age + log(income), high_sat ~ age + poly(inc_k, 2),
                 high_sat ~ age + I(inc_k / 10))) {
    me <- marginal_effects(logistic_regression(d, f))$results
    g <- stats::glm(f, family = stats::binomial(), data = dat)
    raw <- setdiff(all.vars(f[[3]]), "age")
    expect_true(raw %in% me$Term, info = deparse(f))
    expect_equal(me$AME[me$Term == raw], .ame_oracle(g, dat, raw),
                 tolerance = 1e-6, info = deparse(f))
    expect_equal(me$AME[me$Term == "age"], .ame_oracle(g, dat, "age"),
                 tolerance = 1e-6, info = deparse(f))
  }

  # Character and logical predictors: discrete changes, same as the factor
  me_f <- marginal_effects(logistic_regression(d, high_sat ~ age + gender))$results
  me_c <- marginal_effects(logistic_regression(d, high_sat ~ age + gender_chr))$results
  me_l <- marginal_effects(logistic_regression(d, high_sat ~ age + female))$results
  # character levels sort alphabetically (as glm does): Female is reference
  expect_equal(me_c$Term[2], "gender_chr: Male vs. Female")
  expect_equal(me_c$AME[2], -me_f$AME[2], tolerance = 1e-10)
  expect_equal(me_l$Term[2], "female: TRUE vs. FALSE")
  expect_equal(me_l$AME[2], me_f$AME[2], tolerance = 1e-10)

  # A numeric variable that enters only as factor() cannot be perturbed:
  # clear warning naming it, never a silent drop
  d$edu_num <- as.integer(d$education)
  expect_warning(
    me_n <- marginal_effects(logistic_regression(d, high_sat ~ age + factor(edu_num))),
    "edu_num"
  )
  expect_equal(me_n$results$Term, "age")
})

# REG-04: marginal_effects() on a grouped model with a haven-labelled group
# variable crashed ("arguments imply differing number of rows: 1, 2").
test_that("REG-04: grouped marginal_effects() with a labelled group variable", {
  skip_if_not_installed("haven")
  d <- .reg_sd2()
  d$reg <- haven::labelled(as.integer(d$region), c(East = 1, West = 2))
  mg <- logistic_regression(dplyr::group_by(d, reg), high_sat ~ age + income)
  expect_no_error(me <- marginal_effects(mg))
  expect_equal(nrow(me$results), 4L)
  out <- capture.output(print(me))
  expect_true(any(grepl("reg = East", out, fixed = TRUE)))
  expect_true(any(grepl("reg = West", out, fixed = TRUE)))
  expect_no_error(capture.output(print(summary(me))))
})
