# =============================================================================
# 0.7.4 practice-test fixes — batch PAR (parametric tests, post-hoc,
# assumption checks): t_test, oneway_anova, factorial_anova, ancova,
# levene_test, tukey_test, scheffe_test, normality_test
# =============================================================================
# One block per finding ID (see NEWS "Bug fixes (2026-09 field report)").

data(survey_data, envir = environment())

# Labelled copies of survey_data factors, as read_spss() would deliver them
.lab <- function(f) {
  haven::labelled(as.integer(f), stats::setNames(seq_along(levels(f)), levels(f)))
}

# --- PAR-02 / PAR-10: grouping variables in SPSS order with value labels -----

test_that("PAR-02: t_test() group order follows the codes, not the row order", {
  # Levels came from unique() (order of appearance): sorting the data by the
  # grouping variable flipped the sign of t and of the mean difference.
  d <- survey_data
  d$female <- as.integer(d$gender == "Female")
  r1 <- t_test(d, life_satisfaction, group = female)
  r2 <- t_test(d[order(-d$female), ], life_satisfaction, group = female)
  expect_equal(r1$results$t_stat, r2$results$t_stat)
  expect_equal(r1$results$mean_diff, r2$results$mean_diff)
  expect_identical(as.character(r2$group_levels), c("0", "1"))
})

test_that("PAR-10: labelled grouping variables show value labels, not codes", {
  skip_if_not_installed("haven")
  # Codes were printed ("Groups compared: 1 vs. 2", Tukey rows "1 - 2",
  # descriptives and EMMs by code) although group_by() headers showed labels.
  d <- survey_data
  d$sex <- .lab(d$gender)
  d$edu <- .lab(d$education)

  tt <- t_test(d, life_satisfaction, group = sex)
  expect_identical(as.character(tt$group_levels), c("Male", "Female"))
  out <- capture.output(summary(tt))
  expect_true(any(grepl("Male vs. Female", out, fixed = TRUE)))

  ow <- oneway_anova(d, life_satisfaction, group = edu)
  expect_identical(names(ow$results$group_stats[[1]]), levels(survey_data$education))
  tk <- tukey_test(ow)
  expect_true(all(grepl("Secondary|University", tk$results$Comparison)))
  sc <- scheffe_test(ow)
  expect_true(all(grepl("Secondary|University", sc$results$Comparison)))

  fa <- factorial_anova(d, dv = life_satisfaction, between = c(sex, edu))
  expect_setequal(as.character(fa$descriptives$sex), c("Male", "Female"))
  expect_true(all(grepl("Secondary|University|Male|Female",
                        tukey_test(fa)$results$Comparison)))

  an <- ancova(d, dv = life_satisfaction, between = sex, covariate = age)
  expect_setequal(as.character(an$estimated_marginal_means$sex), c("Male", "Female"))

  # numbers are unchanged by the relabelling
  ref <- oneway_anova(survey_data, life_satisfaction, group = education)
  expect_equal(ow$results$F_statistic, ref$results$F_statistic)
})

# --- PAR-01 / PAR-24: degenerate dependent variables --------------------------

test_that("PAR-01: a constant DV is not tested (no spurious F from rounding noise)", {
  # SS were 0/0 up to floating-point noise: weighted F = 7256 ***, eta2 .897;
  # unweighted F = 0.992; factorial F = 4.011 *; Tukey p-values.
  d <- survey_data
  d$const <- 3
  expect_warning(
    r <- oneway_anova(d, const, group = education, weights = sampling_weight),
    "const.*no variance"
  )
  expect_true(is.na(r$results$F_statistic))
  expect_warning(
    r2 <- oneway_anova(d, const, life_satisfaction, group = education),
    "no variance"
  )
  expect_true(is.na(r2$results$F_statistic[1]))
  expect_false(is.na(r2$results$F_statistic[2]))
  out <- capture.output(print(r2))
  expect_true(any(grepl("not computed (no variance", out, fixed = TRUE)))
  out_s <- capture.output(print(summary(r2)))
  expect_false(any(grepl("NaN", out_s, fixed = TRUE)))
  expect_true(any(grepl("not computed", out_s, fixed = TRUE)))

  expect_error(factorial_anova(d, dv = const, between = c(gender, region)),
               "no variance")
  expect_error(ancova(d, dv = const, between = gender, covariate = age),
               "no variance")

  ow <- suppressWarnings(oneway_anova(d, const, group = education))
  expect_warning(tk <- tukey_test(ow), "const.*no variance")
  expect_equal(nrow(tk$results), 0L)
  expect_warning(scheffe_test(ow), "const.*no variance")
})

test_that("PAR-01/PAR-24: t_test() skips constant or empty variables with a clear warning", {
  # A constant variable aborted the whole multi-variable call with the raw
  # base-R message (German locale: "Daten sind praktisch konstant", no
  # variable name, printed twice); an all-NA variable was reported as a
  # grouping problem ("must have exactly 2 levels. Found 0 levels").
  d <- survey_data
  d$const <- 3
  d$allna <- NA_real_
  w <- NULL
  r <- withCallingHandlers(
    t_test(d, const, life_satisfaction, group = gender),
    warning = function(cnd) {
      w <<- conditionMessage(cnd)
      invokeRestart("muffleWarning")
    }
  )
  expect_match(w, "const.*no variance")
  expect_false(grepl("konstant|constant data", w))
  expect_true(is.na(r$results$t_stat[1]))
  expect_false(is.na(r$results$t_stat[2]))
  expect_true(any(grepl("not computed (no variance",
                        capture.output(print(r)), fixed = TRUE)))

  expect_warning(r2 <- t_test(d, allna, group = gender), "allna.*no non-missing")
  expect_true(is.na(r2$results$t_stat))
  expect_warning(t_test(d, const), "const.*no variance")

  # A grouping variable with 3+ groups is an input error for all variables:
  # one clear error with a hint instead of "t_test() failed: X / Caused by: X"
  err <- tryCatch(t_test(d, age, group = education), error = identity)
  msg <- conditionMessage(err)
  expect_match(msg, "exactly 2 groups")
  expect_match(msg, "oneway_anova")
  expect_false(grepl("Caused by", msg))
})
