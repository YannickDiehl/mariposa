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

# REG-06: weighted linear_regression - summary() used SPSS frequency-weight
# df (N = sum(w)), but vcov()/confint()/nobs()/tidy()/glance()/anova()
# inherited lm's analytic-weight df: SE .0527 in the summary vs .0916 in
# tidy(), nobs() 2115 vs N 6388.
test_that("REG-06: weighted lm generics follow the SPSS frequency-weight df", {
  d <- survey_data
  d$w3 <- d$sampling_weight * 3
  m <- linear_regression(d, life_satisfaction ~ age + income, weights = w3)
  ct <- m$coef_table
  cc <- stats::complete.cases(d[c("life_satisfaction", "age", "income")])
  sw <- sum(d$w3[cc])

  expect_equal(nobs(m), sw)
  expect_equal(df.residual(m), sw - 3)
  expect_equal(unname(sqrt(diag(vcov(m)))), unname(ct$Std.Error))
  ci <- confint(m)
  expect_equal(unname(ci[, 1]), unname(ct$CI_lower))
  expect_equal(unname(ci[, 2]), unname(ct$CI_upper))
  expect_equal(colnames(confint(m, level = 0.9)), c("5 %", "95 %"))
  expect_equal(rownames(confint(m, parm = "age")), "age")

  a <- anova(m)
  expect_equal(a["Residuals", "Df"], sw - 3)
  m1 <- linear_regression(d, life_satisfaction ~ age, weights = w3)
  a1 <- anova(m1)
  expect_equal(a1["age", "F value"], m1$anova_table$F_statistic[1])
  expect_equal(a1["age", "Pr(>F)"], m1$anova_table$Sig[1])

  p <- predict(m, newdata = head(d), se.fit = TRUE)
  X <- stats::model.matrix(~ age + income, head(d))
  expect_equal(unname(p$se.fit), unname(sqrt(diag(X %*% vcov(m) %*% t(X)))))
  expect_equal(p$df, sw - 3)

  # Unweighted models keep lm's own generics
  mu <- linear_regression(d, life_satisfaction ~ age + income)
  ref <- stats::lm(life_satisfaction ~ age + income, data = d)
  expect_equal(unname(vcov(mu)), unname(vcov(ref)))
  expect_equal(nobs(mu), nobs(ref))

  skip_if_not_installed("broom")
  td <- broom::tidy(m, conf.int = TRUE)
  expect_equal(td$std.error, unname(ct$Std.Error))
  expect_equal(td$p.value, unname(ct$p))
  expect_equal(td$conf.low, unname(ct$CI_lower))
  gl <- broom::glance(m)
  expect_equal(gl$adj.r.squared, m$model_summary$adj_R_squared)
  expect_equal(gl$sigma, m$model_summary$std_error)
  expect_equal(gl$statistic, m$anova_table$F_statistic[1])
  expect_equal(gl$nobs, sw)
  expect_equal(gl$df.residual, sw - 3)
})

# REG-07: logistic_regression() rejected SPSS-style binary outcomes: 1/2
# coding ("must be binary (0/1)") and factors with an unused level (as
# to_label() leaves them).
# REG-08: the output never said which category is modelled.
test_that("REG-07/08: any two-valued outcome is accepted and its encoding shown", {
  d <- .reg_sd2()
  ref <- logistic_regression(d, high_sat ~ age + income)

  # 1/2 coding: lower value = 0, higher = 1 (SPSS)
  d$sat12 <- d$high_sat + 1L
  m12 <- logistic_regression(d, sat12 ~ age + income)
  expect_equal(unname(coef(m12)), unname(coef(ref)))
  expect_equal(m12$dv_encoding$Internal, c(0L, 1L))
  expect_equal(m12$dv_encoding$Original, c("1", "2"))

  # Factor with an unused level: dropped; first remaining level = 0
  d$sat_f <- factor(ifelse(d$high_sat == 1, "satisfied", "not satisfied"),
                    levels = c("unused", "not satisfied", "satisfied"))
  mf <- logistic_regression(d, sat_f ~ age + income)
  expect_equal(unname(coef(mf)), unname(coef(ref)))
  expect_equal(mf$dv_encoding$Original, c("not satisfied", "satisfied"))

  # Character and logical outcomes
  d$sat_chr <- as.character(d$sat_f)
  expect_equal(unname(coef(logistic_regression(d, sat_chr ~ age + income))),
               unname(coef(ref)))
  d$sat_lgl <- d$high_sat == 1
  expect_equal(unname(coef(logistic_regression(d, sat_lgl ~ age + income))),
               unname(coef(ref)))

  # Output states the modelled category
  out <- capture.output(print(mf))
  expect_true(any(grepl("P(sat_f = satisfied)", out, fixed = TRUE)))
  out_s <- capture.output(print(summary(mf)))
  expect_true(any(grepl("Dependent Variable Encoding", out_s, fixed = TRUE)))
  expect_true(any(grepl("not satisfied", out_s, fixed = TRUE)))

  skip_if_not_installed("haven")
  d$sat_lab <- haven::labelled(d$sat12, c(unzufrieden = 1, zufrieden = 2))
  ml <- logistic_regression(d, sat_lab ~ age + income)
  expect_equal(unname(coef(ml)), unname(coef(ref)))
  expect_true(any(grepl("P(sat_lab = zufrieden)",
                        capture.output(print(ml)), fixed = TRUE)))
  out_l <- capture.output(print(summary(ml)))
  expect_true(any(grepl("2 (zufrieden)", out_l, fixed = TRUE)))
})

# REG-21 (logistic part): a constant outcome gave "Nagelkerke R2 = -Inf ...
# Accuracy = 100%"; outcomes with more than two values or a character
# outcome gave cryptic messages.
test_that("REG-21: non-binary and constant outcomes get clear errors", {
  d <- .reg_sd2()
  d$one <- 1L
  expect_error(logistic_regression(d, one ~ age), "only one observed value")
  expect_error(logistic_regression(d, life_satisfaction ~ age),
               "5 distinct values")
  d$chr3 <- c("a", "b", "c")[(seq_len(nrow(d)) %% 3) + 1]
  expect_error(logistic_regression(d, chr3 ~ age), "3 distinct values")
})

# REG-10 (+EDGE-13): one small group aborted the whole grouped linear /
# logistic regression ("Insufficient observations ...") without naming it.
# SPSS SPLIT FILE skips such a split and carries on.
test_that("REG-10: a degenerate group is skipped with a warning naming it", {
  d <- .reg_sd2()
  d$grp <- ifelse(seq_len(nrow(d)) <= 3, "tiny", "big")
  g <- dplyr::group_by(d, grp)

  expect_warning(
    m <- linear_regression(g, life_satisfaction ~ age + income + trust_media),
    "grp = tiny"
  )
  expect_length(m$groups, 1L)
  expect_equal(m$groups[[1]]$group_values$grp, "big")
  out <- capture.output(print(m))
  expect_true(any(grepl("grp = tiny: not computed", out, fixed = TRUE)))
  out_s <- capture.output(print(summary(m)))
  expect_true(any(grepl("not computed", out_s, fixed = TRUE)))

  expect_warning(
    ml <- logistic_regression(g, high_sat ~ age + income + trust_media),
    "grp = tiny"
  )
  expect_length(ml$groups, 1L)
  expect_true(any(grepl("grp = tiny: not computed",
                        capture.output(print(ml)), fixed = TRUE)))
  expect_no_error(capture.output(print(summary(ml))))
  expect_no_error(marginal_effects(ml))

  # A group whose outcome is constant is skipped the same way
  d$high_sat[d$region == "East"] <- 1L
  expect_warning(
    logistic_regression(dplyr::group_by(d, region), high_sat ~ age),
    "region = East"
  )

  # All groups degenerate: one clear error
  d$grp2 <- rep(1:1250, each = 2)
  expect_error(
    suppressWarnings(linear_regression(dplyr::group_by(d, grp2),
                                       life_satisfaction ~ age + income)),
    "No group"
  )
})

# REG-21 (linear part): an all-NA predictor gave "Insufficient
# observations for the number of predictors" without naming the cause.
test_that("REG-21: an all-missing predictor is named in the error", {
  d <- .reg_sd2()
  d$empty <- NA_real_
  expect_error(linear_regression(d, life_satisfaction ~ age + empty),
               "empty.*no non-missing")
  expect_error(logistic_regression(d, high_sat ~ age + empty),
               "empty.*no non-missing")
  expect_error(linear_regression(d[1:3, ], life_satisfaction ~ age + income + trust_media),
               "complete case")
})

# REG-11 (+EDGE-07): dependent=/predictors= pasted names into a formula
# without backticks - non-syntactic names ("my var", "Zufriedenheit (0-10)")
# failed with a parse error.
test_that("REG-11: SPSS-style interface handles non-syntactic names", {
  d <- .reg_sd2()[c("life_satisfaction", "high_sat", "age", "income")]
  names(d) <- c("my var", "hoch zufrieden", "Alter (Jahre)", "income")
  m <- linear_regression(d, dependent = `my var`,
                         predictors = c(`Alter (Jahre)`, income))
  ref <- stats::lm(`my var` ~ `Alter (Jahre)` + income, data = d)
  expect_equal(unname(coef(m)), unname(coef(ref)))
  expect_no_error(capture.output(print(summary(m))))
  ml <- logistic_regression(d, dependent = `hoch zufrieden`,
                            predictors = c(`Alter (Jahre)`, income))
  expect_equal(length(coef(ml)), 3L)
})

# REG-12: log(income) ~ age said "Variable(s) not found in data: log.";
# y ~ . and y ~ 1 failed cryptically.
test_that("REG-12: transformed outcome, dot and intercept-only formulas", {
  d <- .reg_sd2()
  m <- linear_regression(d, log(income) ~ age)
  ref <- stats::lm(log(income) ~ age, data = d)
  expect_equal(unname(coef(m)), unname(coef(ref)))
  expect_equal(m$model_summary$R_squared, summary(ref)$r.squared)
  expect_equal(m$descriptives$Variable[1], "log(income)")
  expect_equal(m$descriptives$Mean[1], mean(log(d$income[!is.na(d$income) & !is.na(d$age)])))
  expect_no_error(capture.output(print(summary(m))))
  # Weighted: SPSS frequency-weight R2 of the transformed outcome
  mw <- linear_regression(d, log(income) ~ age, weights = sampling_weight)
  refw <- stats::lm(log(income) ~ age, data = d, weights = sampling_weight)
  expect_equal(unname(coef(mw)), unname(coef(refw)))
  expect_equal(mw$model_summary$R_squared, summary(refw)$r.squared)

  expect_error(logistic_regression(d, I(high_sat == 1) ~ age),
               "single variable")
  expect_error(linear_regression(d, cbind(income, age) ~ gender),
               "single variable")

  # y ~ . : all other columns except weights and grouping variables
  ds <- d[c("life_satisfaction", "age", "income", "trust_media",
            "sampling_weight", "region")]
  md <- linear_regression(dplyr::group_by(ds, region), life_satisfaction ~ .,
                          weights = sampling_weight)
  expect_equal(md$predictor_names, c("age", "income", "trust_media"))
  mdu <- linear_regression(ds[c("life_satisfaction", "age", "income")],
                           life_satisfaction ~ .)
  expect_equal(names(coef(mdu)), c("(Intercept)", "age", "income"))

  expect_error(linear_regression(d, life_satisfaction ~ 1), "no predictor")
  expect_error(logistic_regression(d, high_sat ~ 1), "no predictor")
  expect_error(linear_regression(d, life_satisfaction ~ age + life_satisfaction),
               "both the dependent variable and a predictor")
  expect_error(linear_regression(d, life_satisfaction ~ age + nope),
               "not found.*nope")
})

# REG-20: tidyselect helpers that also select the dependent variable (e.g.
# where(is.numeric)) put it among the predictors; character predictors
# produced NA descriptives plus base-R warnings; anova() on a weighted
# logistic model leaked non-integer warnings.
test_that("REG-20: DV/weights excluded from selected predictors, character predictors", {
  d <- survey_data[c("life_satisfaction", "age", "income", "trust_media",
                     "sampling_weight")]
  expect_message(
    m <- linear_regression(d, dependent = life_satisfaction,
                           predictors = where(is.numeric),
                           weights = sampling_weight),
    "life_satisfaction"
  )
  expect_equal(m$predictor_names, c("age", "income", "trust_media"))

  d2 <- .reg_sd2()
  d2$gender_chr <- as.character(d2$gender)
  expect_no_warning(mc <- linear_regression(d2, life_satisfaction ~ age + gender_chr))
  expect_true("gender_chrMale" %in% names(coef(mc)))
  expect_false(anyNA(mc$descriptives$Mean))
  expect_no_warning(logistic_regression(d2, high_sat ~ age + gender_chr))

  mw <- logistic_regression(d2, high_sat ~ age + gender, weights = sampling_weight)
  expect_no_warning(anova(mw))
})

# REG-21: weights = sampling_weight * 2 failed with "Can't convert a call to
# a string."
test_that("REG-21: weights can be an expression", {
  d <- .reg_sd2()
  d$w2 <- d$sampling_weight * 2
  m <- linear_regression(d, life_satisfaction ~ age, weights = sampling_weight * 2)
  ref <- linear_regression(d, life_satisfaction ~ age, weights = w2)
  expect_equal(m$coef_table$Std.Error, ref$coef_table$Std.Error)
  expect_equal(m$weight_name, "sampling_weight * 2")
  expect_true(any(grepl("sampling_weight * 2", capture.output(print(summary(m))),
                        fixed = TRUE)))
  ml <- logistic_regression(d, high_sat ~ age, weights = sampling_weight * 2)
  refl <- logistic_regression(d, high_sat ~ age, weights = w2)
  expect_equal(ml$coef_table$S.E., refl$coef_table$S.E.)
  expect_error(linear_regression(d, life_satisfaction ~ age, weights = nope * 2),
               "weights")
})
