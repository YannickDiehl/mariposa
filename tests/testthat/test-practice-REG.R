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

# REG-13: update()/step() failed ("object 'data_complete' not found") and
# the stored call showed internal names; coef()/residuals() returned NULL
# silently on grouped and pairwise results; confint()/nobs() gave base-R
# (German) errors there.
test_that("REG-13: proper call, update(), step() and informative generics", {
  sd_reg <- survey_data
  m <- linear_regression(sd_reg, life_satisfaction ~ age + income + trust_media)
  expect_identical(m$call[[1]], quote(linear_regression))
  expect_identical(m$call$data, quote(sd_reg))
  m2 <- update(m, . ~ . - income)
  expect_s3_class(m2, "linear_regression")
  expect_equal(names(coef(m2)), c("(Intercept)", "age", "trust_media"))
  expect_s3_class(step(m, trace = 0), "linear_regression")

  # SPSS-style interface: the call records the equivalent formula
  m3 <- linear_regression(sd_reg, dependent = life_satisfaction,
                          predictors = c(age, income), weights = sampling_weight)
  expect_s3_class(m3$call$formula, "formula")
  expect_null(m3$call$dependent)
  m4 <- update(m3, . ~ . + trust_media)
  expect_true(isTRUE(m4$weighted))
  expect_equal(m4$predictor_names, c("age", "income", "trust_media"))

  d <- .reg_sd2()
  ml <- logistic_regression(d, high_sat ~ age + income)
  expect_equal(names(coef(update(ml, . ~ . - income))), c("(Intercept)", "age"))

  # Fitted inside a magrittr pipe: the data has no name to re-use
  mp <- survey_data %>% linear_regression(life_satisfaction ~ age)
  expect_error(update(mp, . ~ . + income), "pipe")

  # Grouped and pairwise results
  g <- dplyr::group_by(sd_reg, region)
  rg <- linear_regression(g, life_satisfaction ~ age)
  expect_s3_class(update(rg, . ~ . + income), "linear_regression")
  for (fn in list(coef, residuals, fitted, confint, nobs, vcov)) {
    expect_error(fn(rg), "grouped")
  }
  expect_error(update(rg$groups[[1]], . ~ . + income), "single group")
  rp <- linear_regression(sd_reg, life_satisfaction ~ age + income,
                          use = "pairwise")
  expect_equal(coef(rp), stats::setNames(rp$coef_table$B, rp$coef_table$Term))
  for (fn in list(residuals, fitted, confint, nobs, vcov)) {
    expect_error(fn(rp), "pairwise")
  }
  gl <- logistic_regression(dplyr::group_by(d, region), high_sat ~ age)
  for (fn in list(coef, residuals, fitted, confint, nobs, vcov)) {
    expect_error(fn(gl), "grouped")
  }
})

# REG-22: weighted use = "pairwise" rounded the effective N before using it
# in df, SS and standard errors (round(min(n_mat))), contrary to Charter
# 5.1 (unrounded sum(w) in formulas, rounding for display only).
test_that("REG-22: weighted pairwise regression uses the unrounded N", {
  d <- survey_data
  vars <- c("life_satisfaction", "age", "income")
  rp <- linear_regression(d, life_satisfaction ~ age + income,
                          weights = sampling_weight, use = "pairwise")
  w <- d$sampling_weight
  pair_n <- c(vapply(vars, function(v) sum(w[!is.na(d[[v]])]), numeric(1)),
              utils::combn(vars, 2, function(p) {
                sum(w[!is.na(d[[p[1]]]) & !is.na(d[[p[2]]])])
              }))
  n_eff <- min(pair_n)
  expect_false(n_eff == round(n_eff))
  expect_equal(rp$anova_table$df[2], n_eff - 3)
  expect_equal(rp$anova_table$df[3], n_eff - 1)
  expect_equal(rp$n, round(n_eff))
  # Unweighted pairwise keeps integer df
  ru <- linear_regression(d, life_satisfaction ~ age + income, use = "pairwise")
  expect_equal(ru$anova_table$df[2], round(ru$anova_table$df[2]))
})

# Row of a printed table whose first cell is `term`
.table_row <- function(out, term) {
  out[grepl(paste0("^\\s+", term, "\\s"), out)]
}

# REG-17: regression summaries ignored digits; small-unit effects printed as
# 0.000 / -0.000 (income B 0.000 [0.000, 0.000], Exp(B) 1.001 [1.001,
# 1.001], AME 0.000, adj.R2 -0.000); huge values (separation) in fixed
# notation broke the tables.
test_that("REG-17: regression output honours digits and shows small/huge values", {
  m <- linear_regression(survey_data, life_satisfaction ~ age + income)
  out <- capture.output(print(summary(m)))
  inc <- .table_row(out, "income")
  expect_length(inc, 3L)            # descriptives, coefficients, collinearity
  expect_false(grepl("\\s0\\.000\\s", inc[2]))
  expect_true(grepl("e-0", inc[2]))
  out5 <- capture.output(print(summary(m, digits = 5)))
  expect_true(any(grepl(sprintf("%.5f", m$model_summary$R_squared), out5,
                        fixed = TRUE)))
  expect_true(any(grepl(sprintf("%.5f", coef(m)[["age"]]), out5, fixed = TRUE)))
  expect_false(any(grepl("-0.000", capture.output(print(m)), fixed = TRUE)))
  expect_equal(.fmt_fixed(c(-0.0002, 0.1234, NA), 3), c("0.000", "0.123", ""))

  # Odds ratio close to 1: enough decimals to separate the CI limits
  d <- .reg_sd2()
  ml <- logistic_regression(d, high_sat ~ age + income)
  inc <- .table_row(capture.output(print(summary(ml))), "income")
  cells <- strsplit(trimws(inc), "\\s+")[[1]]
  n <- length(cells)
  lims <- if (cells[n] %in% c("*", "**", "***")) cells[(n - 2):(n - 1)] else cells[(n - 1):n]
  expect_false(lims[1] == lims[2])

  # AME per income unit is tiny: not printed as 0.000
  me_out <- capture.output(print(summary(marginal_effects(ml))))
  expect_false(grepl("\\s0\\.000\\s", .table_row(me_out, "income")))
  expect_false(any(grepl("AME = 0.000", capture.output(print(marginal_effects(ml))),
                         fixed = TRUE)))

  # Separation: huge estimates in scientific notation, no 30-digit numbers
  set.seed(4)
  x <- stats::rnorm(60)
  ds <- data.frame(y = as.integer(x > 0), x = x)
  ms <- suppressWarnings(logistic_regression(ds, y ~ x))
  out_s <- capture.output(print(summary(ms)))
  expect_false(any(grepl("[0-9]{12,}", out_s)))
})

# REG-18: term names were truncated to 20 (logistic) / 25 (linear)
# characters ("educationIntermediate S..."), making dummies ambiguous.
test_that("REG-18: term names are printed in full", {
  d <- .reg_sd2()
  d$edu <- factor(as.character(d$education))
  m <- linear_regression(d, life_satisfaction ~ age + edu)
  out <- capture.output(print(summary(m)))
  for (lv in levels(d$edu)[-1]) {
    expect_true(any(grepl(paste0("edu", lv), out, fixed = TRUE)), info = lv)
  }
  expect_false(any(grepl("...", out, fixed = TRUE)))
  ml <- logistic_regression(d, high_sat ~ age + edu)
  out_l <- capture.output(print(summary(ml)))
  for (lv in levels(d$edu)[-1]) {
    expect_true(any(grepl(paste0("edu", lv), out_l, fixed = TRUE)), info = lv)
  }
})

# REG-19: labelled SPSS predictors silently entered as numeric codes
# although factors = "dummy"; the descriptives showed the mean of the
# factor level index for factor predictors.
test_that("REG-19: labelled-as-numeric note and meaningful factor descriptives", {
  skip_if_not_installed("haven")
  d <- .reg_sd2()
  d$edu_lab <- haven::labelled(as.integer(d$education),
                               c(Basic = 1, Intermediate = 2, Academic = 3, University = 4))
  m <- linear_regression(d, life_satisfaction ~ age + edu_lab)
  expect_equal(m$labelled_predictors, "edu_lab")
  out <- capture.output(print(summary(m)))
  expect_true(any(grepl("numeric codes", out, fixed = TRUE)))
  expect_true(any(grepl("to_label", out, fixed = TRUE)))
  ml <- logistic_regression(d, high_sat ~ age + edu_lab)
  expect_true(any(grepl("numeric codes", capture.output(print(summary(ml))),
                        fixed = TRUE)))

  # Factor predictor (dummy coding): descriptives per dummy = proportion
  mf <- linear_regression(d, life_satisfaction ~ age + gender)
  row <- mf$descriptives[mf$descriptives$Variable == "genderFemale", ]
  cc <- stats::complete.cases(d[c("life_satisfaction", "age", "gender")])
  expect_equal(row$Mean, mean(d$gender[cc] == "Female"))
  expect_false("gender" %in% mf$descriptives$Variable)
})

# EDGE-17: sum of weights > 2^31 (expansion weights): print failed with
# "invalid format '%d'".
test_that("EDGE-17: regression prints survive very large weighted N", {
  d <- .reg_sd2()
  d$wbig <- d$sampling_weight * 1e7
  m <- linear_regression(d, life_satisfaction ~ age, weights = wbig)
  expect_no_error(out <- capture.output(print(m)))
  expect_true(any(grepl("N = 2", out)))
  expect_no_error(capture.output(print(summary(m))))
  # glm's IRLS may warn about convergence with weights this large; the
  # point here is the print layer
  ml <- suppressWarnings(logistic_regression(d, high_sat ~ age, weights = wbig))
  expect_no_error(capture.output(print(ml)))
  expect_no_error(capture.output(print(summary(ml))))
  mg <- linear_regression(dplyr::group_by(d, region), life_satisfaction ~ age,
                          weights = wbig)
  expect_no_error(capture.output(print(mg), print(summary(mg))))
})

# ---------------------------------------------------------------------------
# Correlations
# ---------------------------------------------------------------------------

# REG-14: the compact print of a correlation matrix showed the N of the
# first pair only (even "N = 0") although pairwise deletion gives each
# pair its own N.
test_that("REG-14: compact correlation print shows the N range", {
  p5 <- pearson_cor(survey_data, age, income, life_satisfaction, trust_media,
                    political_orientation)
  n <- p5$correlations$n
  out <- capture.output(print(p5))
  expect_true(any(grepl(sprintf("N = %d-%d", min(n), max(n)), out, fixed = TRUE)))
  pl <- pearson_cor(survey_data, age, income, life_satisfaction, use = "listwise")
  out_l <- capture.output(print(pl))
  expect_true(any(grepl(sprintf("N = %d$", pl$correlations$n[1]), out_l)))
  k <- kendall_tau(survey_data[1:200, ], age, income, trust_media)
  expect_true(any(grepl(sprintf("N = %d-%d", min(k$correlations$n),
                                max(k$correlations$n)),
                        capture.output(print(k)), fixed = TRUE)))
})

# REG-15: pearson_cor labelled every interval "95% CI" (and named the
# column CI_95) whatever conf.level was.
test_that("REG-15: the CI label follows conf.level", {
  s2 <- capture.output(print(summary(pearson_cor(survey_data, age, income,
                                                 conf.level = 0.90))))
  expect_true(any(grepl("90% CI", s2, fixed = TRUE)))
  expect_false(any(grepl("95% CI", s2, fixed = TRUE)))
  s3 <- capture.output(print(summary(pearson_cor(survey_data, age, income,
                                                 trust_media, conf.level = 0.99))))
  expect_true(any(grepl("99% CI", s3, fixed = TRUE)))
  expect_false(any(grepl("CI_95", s3, fixed = TRUE)))
})

# REG-16: correlation summaries - p diagonal 0.0000 (SPSS leaves it blank),
# tiny p as 0.0000, matrices silently at 2 decimals for > 6 variables,
# options(width) raised (lines wider than the console), overflowing pair
# labels, a wrapping pairwise table, no significance flags in the matrix,
# a 45-line compact print for 10 variables, and a constant variable
# printed as "r = NA, p = NA ," with repeated base-R warnings.
test_that("REG-16: correlation matrices and tables are SPSS-like and fit", {
  withr::local_options(width = 80)
  p3 <- pearson_cor(survey_data, age, income, life_satisfaction, trust_media)
  out <- capture.output(print(summary(p3)))
  expect_false(any(grepl("0.0000", out, fixed = TRUE)))
  expect_true(any(grepl("<.001", out, fixed = TRUE)))
  expect_true(any(grepl("0.448***", out, fixed = TRUE)))   # SPSS-like flag
  sig_start <- grep("Significance Matrix", out)
  age_row <- out[sig_start + which(grepl("^age\\s", out[-(1:sig_start)]))[1]]
  expect_false(grepl("1.000|\\.000\\b", age_row))
  expect_true(all(nchar(out, type = "width") <= 80))
  expect_equal(getOption("width"), 80)
  expect_false(any(grepl("Variable_Pair", out, fixed = TRUE)))

  vars8 <- c("age", "income", "life_satisfaction", "trust_media",
             "trust_science", "trust_government", "political_orientation",
             "environmental_concern")
  p8 <- pearson_cor(survey_data, dplyr::all_of(vars8))
  out8 <- capture.output(print(summary(p8, digits = 3)))
  expect_true(any(grepl("-0.587", out8, fixed = TRUE)))
  # The matrices (which grow with the number of variables) fit the console
  matrix_part <- out8[seq_len(grep("Pairwise Results", out8) - 1)]
  expect_true(all(nchar(matrix_part, type = "width") <= 80))
  expect_lte(length(capture.output(print(p8))), 16L)

  # Constant variable: one clear warning, "not computed" instead of NA text
  d <- survey_data
  d$const <- 3
  warns <- character(0)
  res <- withCallingHandlers(
    pearson_cor(d, age, income, const),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warns, 1L)
  expect_match(warns, "const")
  out_c <- capture.output(print(res))
  expect_true(any(grepl("not computed (no variance)", out_c, fixed = TRUE)))
  expect_false(any(grepl("NA ,", out_c, fixed = TRUE)))
  expect_warning(spearman_rho(d, age, const), "const")
  expect_no_error(capture.output(print(summary(res))))
})

# REG-23: alternative = "less"/"greater" was not shown in print/summary and
# the CI stayed two-sided.
test_that("REG-23: one-sided pearson tests are labelled and get one-sided CIs", {
  pl <- pearson_cor(survey_data, age, income, alternative = "less")
  ref <- stats::cor.test(survey_data$age, survey_data$income,
                         alternative = "less")
  expect_equal(pl$correlations$conf_int_lower, -1)
  expect_equal(pl$correlations$conf_int_upper, ref$conf.int[2],
               tolerance = 1e-10)
  expect_equal(pl$correlations$p_value, ref$p.value, tolerance = 1e-10)
  expect_true(any(grepl("less", capture.output(print(pl)), fixed = TRUE)))
  out <- capture.output(print(summary(pl)))
  expect_true(any(grepl("Alternative hypothesis: less", out, fixed = TRUE)))
  expect_true(any(grepl("one-sided", out, fixed = TRUE)))
  pg <- pearson_cor(survey_data, age, income, alternative = "greater")
  expect_equal(pg$correlations$conf_int_upper, 1)
})

# EDGE-17 (correlations): a sum of weights above 2^31 broke the prints
# with "invalid format '%d'".
test_that("EDGE-17: correlation prints survive a very large weighted N", {
  d <- survey_data
  d$wbig <- d$sampling_weight * 1e7
  p <- pearson_cor(d, age, income, trust_media, weights = wbig)
  expect_no_error(capture.output(print(p), print(summary(p))))
  p2 <- pearson_cor(d, age, income, weights = wbig)
  expect_no_error(capture.output(print(p2), print(summary(p2))))
  k <- kendall_tau(d[1:150, ], age, income, weights = wbig)
  expect_no_error(capture.output(print(k), print(summary(k))))
  pc <- partial_cor(d, life_satisfaction, income, controls = age, weights = wbig)
  expect_no_error(capture.output(print(pc), print(summary(pc))))
})

# REG-21 (partial_cor): a constant control variable crashed with "missing
# value where TRUE/FALSE needed".
test_that("REG-21: partial_cor with a constant control variable", {
  d <- survey_data
  d$const <- 3
  expect_warning(pc <- partial_cor(d, age, income, controls = const), "const")
  expect_true(is.na(pc$correlations$partial_r))
  out <- capture.output(print(pc))
  expect_true(any(grepl("not computed", out, fixed = TRUE)))
  expect_no_error(capture.output(print(summary(pc))))
})
