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

# --- PAR-07 / PAR-17: groups whose variance is undefined or zero --------------

test_that("PAR-07: a one-case group gives the classical ANOVA, Welch not computed", {
  # oneway.test() (Welch) aborted the whole call: "not enough observations";
  # the weighted path printed a Welch row "NaN 4 NaN NA <NA>".
  d <- survey_data
  d$edu2 <- as.character(d$education)
  d$edu2[1] <- "Solo"
  r <- oneway_anova(d, life_satisfaction, group = edu2)
  ref <- stats::anova(stats::lm(life_satisfaction ~ edu2, data = d))
  expect_equal(r$results$F_statistic, ref[["F value"]][1])
  welch <- r$results$welch_result[[1]]
  expect_true(is.na(welch$statistic))
  expect_match(welch$note, "Solo")
  out <- capture.output(print(summary(r)))
  expect_true(any(grepl("Welch.*not computed", out)))
  expect_false(any(grepl("NaN|<NA>", out)))

  rw <- oneway_anova(d, life_satisfaction, group = edu2, weights = sampling_weight)
  expect_false(is.na(rw$results$F_statistic))
  out_w <- capture.output(print(summary(rw)))
  expect_false(any(grepl("NaN|<NA>", out_w)))
  expect_true(any(grepl("Welch.*not computed", out_w)))
})

test_that("PAR-07: t_test() with a one-case group reports Student's t, Welch not computed", {
  # t.test(var.equal = FALSE) aborted: "not enough 'y' observations"; the
  # weighted path computed a Welch t from an undefined variance.
  d <- survey_data
  d$g2 <- ifelse(seq_len(nrow(d)) == 1, "Solo", "Rest")
  expect_warning(tt <- t_test(d, life_satisfaction, group = g2), "Welch.*Solo")
  ref <- stats::t.test(life_satisfaction ~ g2, data = d, var.equal = TRUE)
  expect_equal(tt$results$t_stat, unname(ref$statistic))
  expect_equal(tt$results$df, unname(ref$parameter))
  expect_true(is.na(tt$results$unequal_var_result[[1]]$statistic))
  expect_false(any(grepl("NaN", capture.output(print(summary(tt))))))

  d$sampling_weight[1] <- 1  # group weight sum 1: variance undefined
  expect_warning(tw <- t_test(d, life_satisfaction, group = g2,
                              weights = sampling_weight), "Welch.*Solo")
  expect_false(is.na(tw$results$t_stat))
  expect_true(is.na(tw$results$unequal_var_result[[1]]$statistic))
})

test_that("PAR-17: zero-variance groups: no NaN Welch rows, no infinite Glass' Delta", {
  d <- survey_data
  d$life_satisfaction[d$education == "University"] <- 5
  for (w in list(NULL, "sampling_weight")) {
    r <- if (is.null(w)) {
      oneway_anova(d, life_satisfaction, group = education)
    } else {
      oneway_anova(d, life_satisfaction, group = education, weights = sampling_weight)
    }
    expect_true(is.na(r$results$welch_result[[1]]$statistic))
    out <- capture.output(print(summary(r)))
    expect_false(any(grepl("NaN|<NA>", out)))
    expect_true(any(grepl("zero variance", out)))
  }
  d2 <- survey_data
  d2$life_satisfaction[d2$gender == "Male"] <- 3
  tt <- t_test(d2, life_satisfaction, group = gender)
  expect_true(is.na(tt$results$glass_delta))
  expect_false(any(grepl("Inf", capture.output(print(summary(tt))))))
})

# --- PAR-18 / EDGE-13: grouped runs name the group that cannot be tested ------

test_that("PAR-18: grouped oneway/tukey/levene warn with the group label", {
  # oneway_anova() printed a silent "Results not available", tukey_test()
  # dropped the group without a word, levene_test() warned "in group 1".
  d <- dplyr::filter(survey_data, region == "West" | education == "University")
  g <- dplyr::group_by(d, region)
  expect_warning(r <- oneway_anova(g, life_satisfaction, group = education),
                 "region = East")
  east <- r$results[r$results$region == "East", ]
  expect_true(is.na(east$F_statistic))
  expect_match(east$note, "groups? with")
  out <- capture.output(print(r))
  expect_false(any(grepl("Results not available", out, fixed = TRUE)))
  expect_true(any(grepl("not computed (", out, fixed = TRUE)))
  out_s <- capture.output(print(summary(r)))
  expect_true(any(grepl("not computed", out_s, fixed = TRUE)))

  expect_warning(tk <- tukey_test(r), "region = East")
  expect_warning(scheffe_test(r), "region = East")
  expect_warning(lv <- levene_test(r), "region = East")
  expect_false(any(grepl("in group 1", tryCatch(levene_test(r), warning = conditionMessage))))
})

# --- PAR-05 / PAR-06: one-sample t-test ---------------------------------------

test_that("PAR-05: the weighted one-sample CI follows `alternative`", {
  # The weighted CI was always two-sided ([3.579, 3.671] for "greater",
  # unweighted gives a one-sided [3.590, Inf)).
  r <- t_test(survey_data, life_satisfaction, mu = 3.5,
              alternative = "greater", weights = sampling_weight)
  expect_identical(r$results$conf_int_upper, Inf)
  expect_true(is.finite(r$results$conf_int_lower))
  r_less <- t_test(survey_data, life_satisfaction, mu = 3.5,
                   alternative = "less", weights = sampling_weight)
  expect_identical(r_less$results$conf_int_lower, -Inf)
  # the w == 1 case reproduces the unweighted one-sided interval
  d <- survey_data
  d$one <- 1
  rw <- t_test(d, life_satisfaction, mu = 3.5, alternative = "greater",
               weights = one)
  ru <- t_test(d, life_satisfaction, mu = 3.5, alternative = "greater")
  expect_equal(rw$results$conf_int_lower, ru$results$conf_int_lower)
})

test_that("PAR-06: one-sample mean difference and CI are relative to mu (SPSS)", {
  # mean_diff held the mean (3.628) and the CI the CI of the mean; SPSS's
  # One-Sample Test shows Mean Difference .628 and the CI of the difference.
  r <- t_test(survey_data, life_satisfaction, mu = 3)
  ref <- stats::t.test(survey_data$life_satisfaction, mu = 3)
  expect_equal(r$results$mean_diff, unname(ref$estimate) - 3)
  expect_equal(c(r$results$conf_int_lower, r$results$conf_int_upper),
               as.numeric(ref$conf.int) - 3)
  assert_spss(r$results$mean_diff, 0.628, tier = "display", precision = 3,
              label = "PAR-06 one-sample Mean Difference")  # t_test_output.txt:18

  out <- capture.output(print(summary(r)))
  expect_true(any(grepl("Test value", out)))
  expect_true(any(grepl("Alternative hypothesis", out)))
  expect_true(any(grepl("Confidence level", out)))
  expect_true(any(grepl("Std. Deviation", out, fixed = TRUE)))
  expect_true(any(grepl("2421", out, fixed = TRUE)))        # N
  expect_true(any(grepl("1.153", out, fixed = TRUE)))       # SD (SPSS 1.153)
  # no legend for effect sizes that are not shown
  expect_false(any(grepl("Effect Size Interpretation", out, fixed = TRUE)))
  expect_false(any(grepl("Cohen", out, fixed = TRUE)))
})

# --- PAR-15 / PAR-21 / PAR-26: t_test() summary tables -------------------------

.p_data <- function(shift) {
  base <- stats::qnorm(stats::ppoints(40))
  data.frame(y = c(base, base + shift), g = rep(c("a", "b"), each = 40))
}

test_that("PAR-15: summary stars come from the exact p, not the rounded one", {
  # p = 0.0008 was rounded to 0.001 first and got "**" (compact print: ***);
  # p = 0.0497 printed as "0.05" without a star.
  r <- t_test(.p_data(0.7777), y, group = g)
  expect_lt(r$results$unequal_var_result[[1]]$p.value, 0.001)
  out <- capture.output(print(summary(r)))
  row <- out[grepl("not assumed", out)]
  expect_match(row, "<\\.001 .*\\*\\*\\* *$")
  r2 <- t_test(.p_data(0.4444), y, group = g)
  out2 <- capture.output(print(summary(r2)))
  row2 <- out2[grepl("not assumed", out2)]
  expect_match(row2, "\\.050 .* \\* *$")
})

test_that("PAR-21/PAR-26: t_test() summary tables: SPSS formats, Group Statistics, digits", {
  # p printed as a bare 0, df column mixing 2419 / 2384.147 / 2419.000,
  # weighted n "1149.0", digits ignored; no SD / SE of the mean per group.
  r <- t_test(.p_data(2), y, group = g)
  out <- capture.output(print(summary(r)))
  expect_true(any(grepl("<.001", out, fixed = TRUE)))
  expect_false(any(grepl("(^| )0( |$)", out[grepl("assumed", out)])))

  tt <- t_test(survey_data, life_satisfaction, group = gender,
               weights = sampling_weight)
  out <- capture.output(print(summary(tt)))
  expect_true(any(grepl("Group Statistics", out, fixed = TRUE)))
  expect_true(any(grepl("Std. Deviation", out, fixed = TRUE)))
  expect_true(any(grepl("Std. Error Mean", out, fixed = TRUE)))
  expect_false(any(grepl("\\b\\d+\\.0\\b", out[grepl("^ +(Male|Female) ", out)])))
  stats <- tt$results$group_stats[[1]]
  expect_equal(stats$group1$sd,
               sqrt(w_var(survey_data[survey_data$gender == "Male", ],
                          life_satisfaction, weights = sampling_weight)$results$weighted_var))

  out2 <- capture.output(print(summary(tt, digits = 2)))
  t_row <- out2[grepl("^ +Equal variances assumed", out2)]
  expect_match(t_row, "-1\\.07 ")
})

# --- PAR-19 / PAR-21: oneway_anova() summary and conf.level --------------------

test_that("PAR-19: oneway conf.level reaches the descriptives CI and the post-hoc tests", {
  # The header said "99%" but no interval was printed anywhere, and
  # tukey_test() on the result silently used 95%.
  r <- oneway_anova(survey_data, life_satisfaction, group = education,
                    conf.level = 0.99)
  out <- capture.output(print(summary(r)))
  expect_true(any(grepl("99% CI Lower", out, fixed = TRUE)))
  gs <- r$results$group_stats[[1]][["Basic Secondary"]]
  expect_true(any(grepl(sprintf("%.3f", gs$ci_lower), out, fixed = TRUE)))
  expect_equal(tukey_test(r)$conf.level, 0.99)
  expect_equal(scheffe_test(r)$conf.level, 0.99)
  expect_equal(tukey_test(r, conf.level = 0.9)$conf.level, 0.9)
})

test_that("PAR-21: oneway summary tables use fixed decimals, integer N and digits", {
  # Weighted n printed as "618.0", Mean Square as "289.79" next to "1.077",
  # effect sizes and descriptives ignored `digits`.
  r <- oneway_anova(survey_data, life_satisfaction, group = education,
                    weights = sampling_weight)
  out <- capture.output(print(summary(r)))
  expect_false(any(grepl("618.0", out, fixed = TRUE)))
  expect_true(any(grepl("^ +Basic Secondary +816 ", out)))
  between <- out[grepl("^ +Between Groups", out)]
  expect_match(between, "241\\.130 +3 +80\\.377 +65\\.333 +<\\.001")
  out2 <- capture.output(print(summary(r, digits = 2)))
  expect_true(any(grepl("65.33", out2, fixed = TRUE)))
  expect_true(any(grepl("^ +life_satisfaction +0\\.07 ", out2)))
  expect_true(any(grepl("^ +Basic Secondary +816 +3\\.21 ", out2)))
})

# --- PAR-03 / PAR-12 / weights policy: levene_test() ---------------------------

test_that("PAR-03: grouped levene_test() takes several variables via ...", {
  # The grouped method's signature (x, variable, group, weights) bound the
  # second variable to `weights`: "[Weighted] F(3, 1544896) = 40250".
  g <- dplyr::group_by(survey_data, region)
  r <- levene_test(g, life_satisfaction, income, group = education)
  expect_null(r$weights)
  expect_setequal(unique(r$results$Variable), c("life_satisfaction", "income"))
  east <- r$results[r$results$region == "East" & r$results$Variable == "income", ]
  ref <- levene_test(dplyr::filter(survey_data, region == "East"), income,
                     group = education)
  expect_equal(east$F_statistic, ref$results$F_statistic)
  expect_false(any(grepl("Weighted", capture.output(print(r)))))
})

test_that("PAR-12: grouped levene_test(): labels, NA key, tidyselect, strings, center", {
  # Two group_by() variables printed "region = East, gender = East"; an NA
  # key gave a false "constant" warning and "F(NA, NA) = ,"; starts_with()
  # was taken literally, group = "education" found 0 groups, an invalid
  # `center` was accepted.
  r <- levene_test(dplyr::group_by(survey_data, region, gender),
                   life_satisfaction, group = education)
  out <- capture.output(print(r))
  expect_true(any(grepl("region = East, gender = Male", out, fixed = TRUE)))
  expect_false(any(grepl("gender = East", out, fixed = TRUE)))

  d <- survey_data
  d$region[1:30] <- NA
  expect_no_warning(
    r2 <- levene_test(dplyr::group_by(d, region), life_satisfaction,
                      group = education)
  )
  expect_equal(nrow(r2$results), 3L)
  expect_false(anyNA(r2$results$F_statistic))

  r3 <- levene_test(dplyr::group_by(survey_data, region), starts_with("trust"),
                    group = "education")
  expect_setequal(unique(r3$results$Variable),
                  c("trust_government", "trust_media", "trust_science"))

  expect_error(levene_test(dplyr::group_by(survey_data, region),
                           life_satisfaction, group = education, center = "mode"),
               "center")
  expect_error(levene_test(survey_data, life_satisfaction, group = education,
                           center = "mode"), "center")
})

test_that("PAR-12: a constant variable prints 'not computed', not 'F(NA, NA) = ,'", {
  d <- survey_data
  d$const <- 3
  expect_warning(r <- levene_test(d, const, group = education), "no variance")
  out <- capture.output(print(r))
  expect_false(any(grepl("F(NA", out, fixed = TRUE)))
  expect_true(any(grepl("not computed (no variance", out, fixed = TRUE)))
})

test_that("levene_test() applies the package weights policy", {
  # Negative weights were accepted; character weights gave "F(NA, NA) = ,".
  d <- survey_data
  d$w_neg <- d$sampling_weight
  d$w_neg[1] <- -1
  expect_error(levene_test(d, life_satisfaction, group = gender, weights = w_neg),
               "negative")
  d$w_chr <- as.character(d$sampling_weight)
  expect_error(levene_test(d, life_satisfaction, group = gender, weights = w_chr),
               "numeric")
  expect_error(levene_test(dplyr::group_by(d, region), life_satisfaction,
                           group = gender, weights = w_neg), "negative")
})

test_that("PAR-14: Levene recommends Welch's ANOVA for k > 2, not Welch's t-test", {
  ow <- oneway_anova(survey_data, income, group = education)
  out <- capture.output(print(summary(levene_test(ow))))
  expect_false(any(grepl("t-test", out, fixed = TRUE)))
  expect_true(any(grepl("Welch's ANOVA", out, fixed = TRUE)))

  og <- oneway_anova(dplyr::group_by(survey_data, region), income, group = education)
  out_g <- capture.output(print(summary(levene_test(og))))
  expect_false(any(grepl("t-test", out_g, fixed = TRUE)))

  fa <- factorial_anova(survey_data, dv = income, between = c(gender, education))
  out_f <- capture.output(print(summary(levene_test(fa))))
  expect_false(any(grepl("t-test", out_f, fixed = TRUE)))

  tt <- t_test(survey_data, income, group = gender)
  out_t <- capture.output(print(summary(levene_test(tt))))
  expect_true(any(grepl("t-test", out_t, fixed = TRUE)))
})

test_that("PAR-21: levene summary: p as <.001, SPSS df, digits; compact without dangling space", {
  # p printed as a bare 0, df2 "474.2032" next to df1 3, digits ignored;
  # compact "p = 0.125 , variances equal".
  r <- levene_test(dplyr::group_by(survey_data, region), life_satisfaction,
                   group = education, weights = sampling_weight)
  out <- capture.output(print(summary(r)))
  row <- out[grepl("^ +life_satisfaction", out)][1]
  expect_match(row, "<\\.001")
  expect_false(grepl(" 0 ", row))
  expect_match(row, " [0-9]+\\.[0-9]{3} ")
  out2 <- capture.output(print(summary(r, digits = 1)))
  expect_true(any(grepl("^ +life_satisfaction +[0-9]+\\.[0-9] ", out2)))
  d <- survey_data
  out_c <- capture.output(print(levene_test(d, life_satisfaction, group = gender)))
  expect_false(any(grepl(" ,", out_c, fixed = TRUE)))
})

# --- PAR-11 / PAR-22: post-hoc row convention and alignment --------------------

test_that("PAR-11: Tukey/Scheffe rows follow SPSS (I) - (J) on every path", {
  # Unweighted Tukey printed TukeyHSD's "Intermediate Secondary-Basic
  # Secondary 0.497" (later minus earlier, unspaced), weighted Tukey and
  # Scheffe "Basic Secondary - Intermediate Secondary -0.490".
  ow <- oneway_anova(survey_data, life_satisfaction, group = education)
  oww <- oneway_anova(survey_data, life_satisfaction, group = education,
                      weights = sampling_weight)
  expected <- c("Basic Secondary - Intermediate Secondary",
                "Basic Secondary - Academic Secondary",
                "Basic Secondary - University",
                "Intermediate Secondary - Academic Secondary",
                "Intermediate Secondary - University",
                "Academic Secondary - University")
  for (r in list(tukey_test(ow), tukey_test(oww), scheffe_test(ow),
                 scheffe_test(oww))) {
    expect_identical(r$results$Comparison, expected)
    expect_true(all(r$results$Estimate < 0))
  }
  tk <- tukey_test(ow)
  # SPSS Multiple Comparisons, Std. Error of (I) Basic - (J) Intermediate
  assert_spss(tk$results$SE[1], 0.059, tier = "display", precision = 3,
              label = "PAR-11 Tukey Std. Error")  # tukey_test_output.txt:15
  expect_equal(tk$results$t_value, tk$results$Estimate / tk$results$SE)

  fa <- factorial_anova(survey_data, dv = life_satisfaction,
                        between = c(gender, education))
  fw <- factorial_anova(survey_data, dv = life_satisfaction,
                        between = c(gender, education), weights = sampling_weight)
  ft <- tukey_test(fa)
  expect_true("Male - Female" %in% ft$results$Comparison)
  expect_identical(ft$results$Comparison, tukey_test(fw)$results$Comparison)
  expect_identical(ft$results$Comparison, scheffe_test(fa)$results$Comparison)
})

test_that("PAR-22: post-hoc tables stay aligned with umlaut labels", {
  # print_stat_table() padded with sprintf() (bytes): every umlaut shifted
  # the rest of its row one column to the left.
  d <- survey_data
  levels(d$education) <- c("Hauptschule", "Realschule", "Abitur", "Universität")
  out <- capture.output(print(summary(
    tukey_test(oneway_anova(d, life_satisfaction, group = education)),
    parameters = FALSE, interpretation = FALSE
  )))
  rows <- out[grepl(" - ", out) & grepl("[0-9]\\.[0-9]{3}", out)]
  expect_length(rows, 6L)
  expect_length(unique(nchar(rows, type = "width")), 1L)
})

# --- PAR-20: factorial_anova() / ancova() output ------------------------------

test_that("PAR-20: factorial/ancova summaries: one block, fixed decimals, SS type, digits", {
  # print(data.frame) wrapped the effect table at 80 columns, sums of squares
  # printed as 1.754652e+09, "Type III Sum of Squares: Type 3", Levene
  # "p = <.001", N only on the last compact line, descriptives ignored digits.
  fa <- factorial_anova(survey_data, dv = income, between = c(gender, education))
  out <- capture.output(print(summary(fa)))
  expect_false(any(grepl("e+0", out, fixed = TRUE)))
  expect_false(any(grepl("Type 3", out, fixed = TRUE)))
  expect_true(any(grepl("Type III", out, fixed = TRUE)))
  expect_false(any(grepl("p = <", out, fixed = TRUE)))
  hdr <- out[grepl("Source", out, fixed = TRUE)]
  expect_length(hdr, 1L)
  expect_match(hdr, "Partial Eta Squared")
  expect_match(out[grepl("^ +Corrected Model", out)], "\\*\\*\\* *$")

  comp <- capture.output(print(fa))
  expect_match(comp[1], "N = 2186")
  expect_false(any(grepl("N = ", comp[-1], fixed = TRUE)))

  out1 <- capture.output(print(summary(fa, digits = 1)))
  expect_true(any(grepl("^ +Male +Basic Secondary +2803\\.4 ", out1)))

  an <- ancova(survey_data, dv = income, between = education, covariate = age)
  out_a <- capture.output(print(summary(an)))
  expect_false(any(grepl("e+0", out_a, fixed = TRUE)))
  expect_false(any(grepl("Type 3", out_a, fixed = TRUE)))
  expect_false(any(grepl("p = <", out_a, fixed = TRUE)))
  expect_length(out_a[grepl("Source", out_a, fixed = TRUE)], 1L)
  comp_a <- capture.output(print(an))
  expect_match(comp_a[1], "N = 2186")
})

test_that("PAR-16: ancova Parameter Estimates use SPSS coding (last category = 0)", {
  # R's contrasts leaked into the table: education.L/.Q/.C for the ordered
  # factor, gender1 for contr.sum, and an intercept that is not SPSS's.
  an <- ancova(survey_data, dv = income, between = education, covariate = age)
  pe <- an$parameter_estimates
  expect_identical(pe$parameter, c(
    "Intercept", "age", "[education=Basic Secondary]",
    "[education=Intermediate Secondary]", "[education=Academic Secondary]",
    "[education=University]"
  ))
  # ancova_output.txt, Test 1a Parameter Estimates
  assert_spss(pe$b[1], 5349.320, tier = "display", precision = 3, label = "Intercept B")
  assert_spss(pe$se[1], 91.774, tier = "display", precision = 3, label = "Intercept SE")
  assert_spss(pe$b[2], -0.245, tier = "display", precision = 3, label = "age B")
  assert_spss(pe$se[2], 1.412, tier = "display", precision = 3, label = "age SE")
  assert_spss(pe$b[3], -2577.931, tier = "display", precision = 3, label = "[education=1] B")
  assert_spss(pe$se[3], 72.359, tier = "display", precision = 3, label = "[education=1] SE")
  assert_spss(pe$t[3], -35.627, tier = "display", precision = 3, label = "[education=1] t")
  assert_spss(pe$ci_lower[3], -2719.830, tier = "display", precision = 3, label = "[education=1] CI lower")
  assert_spss(pe$ci_upper[3], -2436.032, tier = "display", precision = 3, label = "[education=1] CI upper")
  assert_spss(pe$partial_eta_sq[3], 0.368, tier = "display", precision = 3, label = "[education=1] eta")
  assert_spss(pe$b[5], -1112.550, tier = "display", precision = 3, label = "[education=3] B")
  expect_true(pe$redundant[6])
  expect_equal(pe$b[6], 0)
  expect_true(is.na(pe$se[6]))
  out <- capture.output(print(summary(an)))
  expect_false(any(grepl("education.L|education1", out)))
  expect_true(any(grepl("redundant", out, fixed = TRUE)))

  # two factors with interaction (ancova_output.txt, gender x education WITH age)
  an2 <- ancova(survey_data, dv = income, between = c(gender, education),
                covariate = age)
  pe2 <- an2$parameter_estimates
  b <- function(label) pe2[pe2$parameter == label, , drop = FALSE]
  assert_spss(b("Intercept")$b, 5363.034, tier = "display", precision = 3, label = "2f Intercept")
  assert_spss(b("[gender=Male]")$b, -35.274, tier = "display", precision = 3, label = "2f gender=1")
  assert_spss(b("[gender=Male] * [education=Basic Secondary]")$b, 119.601,
              tier = "display", precision = 3, label = "2f interaction B")
  assert_spss(b("[gender=Male] * [education=Basic Secondary]")$se, 145.023,
              tier = "display", precision = 3, label = "2f interaction SE")
  expect_true(b("[gender=Female] * [education=Basic Secondary]")$redundant)
  expect_true(b("[gender=Male] * [education=University]")$redundant)
  expect_equal(nrow(pe2), 2 + 2 + 4 + 8)
})

test_that("PAR-16: multi-factor ANCOVA reports main-effect marginal means", {
  # Only the cell means were reported; SPSS /EMMEANS=TABLES(gender) gives the
  # unweighted average of the cell means at the covariate means.
  an2 <- ancova(survey_data, dv = income, between = c(gender, education),
                covariate = age)
  mm <- an2$emm_main_effects
  cells <- an2$estimated_marginal_means
  expect_setequal(names(mm), c("gender", "education"))
  expect_equal(mm$gender$mean[mm$gender$gender == "Male"],
               mean(cells$mean[cells$gender == "Male"]))
  expect_equal(mm$education$mean[mm$education$education == "University"],
               mean(cells$mean[cells$education == "University"]))
  expect_true(all(mm$gender$se > 0))
  out <- capture.output(print(summary(an2)))
  expect_true(any(grepl("gender * education", out, fixed = TRUE)))
  expect_true(any(grepl("^ +Male +[0-9]", out)))
})

test_that("PAR-26: ?factorial_anova examples only use existing summary() toggles", {
  # The example called summary(result, marginal_means = FALSE), a toggle
  # factorial_anova's summary() does not have (silently ignored).
  expect_false("marginal_means" %in% names(formals(summary.factorial_anova)))
  src <- testthat::test_path("..", "..", "R", "factorial_anova.R")
  skip_if(!file.exists(src), "R/ sources not available")
  expect_false(any(grepl("marginal_means", readLines(src), fixed = TRUE)))
})
