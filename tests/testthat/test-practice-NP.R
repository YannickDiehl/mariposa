# =============================================================================
# Practice-test regressions: non-parametric & categorical tests (batch NP)
# =============================================================================
# One block per finding of the 2026-09 field report (phase 2, nonparametric
# and edge-case reports). Each test names the finding ID and what was wrong.
# =============================================================================

library(testthat)
library(dplyr)

data(survey_data, envir = environment())

# --- NP-01: chisq_gof expected proportions -----------------------------------

test_that("NP-01 chisq_gof applies `expected` to every selected variable", {
  # Was: with several variables `expected` was silently dropped (equal
  # proportions used) while the summary header still showed it.
  d <- dplyr::mutate(survey_data,
                     g2 = factor(ifelse(age > 50, "old", "young")))
  multi <- chisq_gof(d, gender, g2, expected = c(0.3, 0.7))
  single_g <- chisq_gof(d, gender, expected = c(0.3, 0.7))
  single_a <- chisq_gof(d, g2, expected = c(0.3, 0.7))
  expect_equal(multi$results$chi_squared,
               c(single_g$results$chi_squared, single_a$results$chi_squared))
})

test_that("NP-01 chisq_gof errors when `expected` does not fit a variable", {
  # Was: the mismatch was swallowed (NA row / expected ignored).
  expect_error(chisq_gof(survey_data, gender, education,
                         expected = c(0.3, 0.7)),
               "education")
})

test_that("NP-01 named `expected` is matched by category name", {
  # Was: matched by position, so c(Female = .3, Male = .7) gave Male .3.
  named <- chisq_gof(survey_data, gender, expected = c(Female = 0.3, Male = 0.7))
  positional <- chisq_gof(survey_data, gender, expected = c(0.7, 0.3))
  expect_equal(named$results$chi_squared, positional$results$chi_squared)
  expect_equal(named$frequencies$expected, c(1750, 750))
  expect_error(chisq_gof(survey_data, gender,
                         expected = c(Female = 0.3, Other = 0.7)),
               "Other")
})

test_that("NP-01 proportions summing to ~1 are rescaled with a message", {
  # Was: a sum of 0.995 was accepted unscaled (expected counts summed to
  # 0.995 * N, inflating chi-square).
  expect_message(
    r <- chisq_gof(survey_data, gender, expected = c(0.3, 0.695)),
    "rescaled"
  )
  ref <- chisq_gof(survey_data, gender, expected = c(0.3, 0.695) / 0.995)
  expect_equal(r$results$chi_squared, ref$results$chi_squared)
  expect_equal(sum(r$frequencies$expected), 2500)
  expect_error(chisq_gof(survey_data, gender, expected = c(0.3, 0.3)),
               "sum to 1")
})

test_that("NP-24 chisq_gof accepts expected counts like SPSS /EXPECTED", {
  # Was: counts (or any relative values) were rejected; SPSS /EXPECTED
  # treats the values as relative frequencies.
  counts <- chisq_gof(survey_data, gender, expected = c(1250, 1250))
  equal <- chisq_gof(survey_data, gender)
  expect_equal(counts$results$chi_squared, equal$results$chi_squared)
  rel <- chisq_gof(survey_data, education, expected = c(40, 30, 20, 10))
  prop <- chisq_gof(survey_data, education, expected = c(.4, .3, .2, .1))
  expect_equal(rel$results$chi_squared, prop$results$chi_squared)
})

test_that("NP-24 chisq_gof warns when expected counts fall below 5", {
  # Was: no warning (chi_square() warns, SPSS footnotes it).
  d <- data.frame(x = factor(c(rep("a", 8), "b", "c")))
  expect_warning(chisq_gof(d, x), "expected")
})

# --- NP-06 / EDGE-03: empty factor levels ------------------------------------

test_that("NP-06 chi_square and effect sizes ignore empty factor levels", {
  # Was: an unused level (e.g. after filter()) gave chi2 = NaN, V = NaN,
  # df counted the phantom level. SPSS uses the observed categories.
  d <- dplyr::mutate(survey_data,
                     gender3 = factor(gender, levels = c("Male", "Female", "Diverse")))
  ref <- chi_square(survey_data, gender, region)
  r <- chi_square(d, gender3, region)
  expect_equal(r$results$chi_squared, ref$results$chi_squared)
  expect_equal(r$results$df, 1)
  expect_equal(unname(cramers_v(d, gender3, region)),
               unname(cramers_v(survey_data, gender, region)))
  expect_equal(unname(goodman_gamma(d, gender3, region)),
               unname(goodman_gamma(survey_data, gender, region)))

  # EDGE-03: the same after filter()
  f <- dplyr::filter(survey_data, education != "University")
  r2 <- chi_square(f, education, region)
  expect_false(is.nan(r2$results$chi_squared))
  expect_equal(r2$results$df, 2)
})

test_that("NP-06 grouped chi_square has no silent NA row from empty levels", {
  # Was: the root of the silent NA row of grouped chi_square().
  d <- dplyr::filter(survey_data, !(region == "East" & education == "University"))
  r <- chi_square(dplyr::group_by(d, region), gender, education)
  expect_false(anyNA(r$results$chi_squared))
  expect_equal(r$results$df[r$results$region == "East"], 2)
})

test_that("NP-06 chisq_gof drops phantom categories", {
  # Was: the unused level entered as an observed 0 (chi2 1257.5, df 2).
  d <- dplyr::mutate(survey_data,
                     gender3 = factor(gender, levels = c("Male", "Female", "Diverse")))
  r <- chisq_gof(d, gender3)
  expect_equal(r$results$chi_squared, chisq_gof(survey_data, gender)$results$chi_squared)
  expect_equal(r$results$df, 1)
  f <- dplyr::filter(survey_data, education != "University")
  expect_equal(chisq_gof(f, education)$results$df, 2)
  # explicit expected must match the observed categories
  expect_error(chisq_gof(d, gender3, expected = c(.4, .4, .2)), "categor")
})

test_that("NP-06 grouped chisq_gof skips a group with one observed category", {
  # Was: a constant variable in one group gave chi2 = 485 (phantom level).
  d <- dplyr::filter(survey_data, !(region == "East" & gender == "Female"))
  expect_warning(
    r <- chisq_gof(dplyr::group_by(d, region), gender),
    "East"
  )
  expect_true(is.na(r$results$chi_squared[r$results$region == "East"]))
  expect_false(is.na(r$results$chi_squared[r$results$region == "West"]))
})

# --- NP-09: constant variable -------------------------------------------------

test_that("NP-09 chi_square and effect sizes handle a constant variable", {
  # Was: "Ersetzung hat Laenge 0" crash (phi/cramers_v/goodman_gamma too).
  d <- dplyr::mutate(survey_data, const = "x")
  expect_warning(r <- chi_square(d, const, region), "const")
  expect_true(is.na(r$results$chi_squared))
  out <- capture.output(print(r))
  expect_true(any(grepl("not computed", out)))
  expect_false(any(grepl("= ,", out, fixed = TRUE)))
  out_s <- capture.output(print(summary(r)))
  expect_true(any(grepl("not computed", out_s)))
  expect_warning(v <- cramers_v(d, const, region), "const")
  expect_true(is.na(v))
  expect_warning(p <- phi(d, region, const), "const")
  expect_true(is.na(p))
  expect_warning(g <- goodman_gamma(d, const, region), "const")
  expect_true(is.na(g))
})

# --- NP-23: gamma speed (+ SPSS ASE0 p-value) ---------------------------------

test_that("NP-23 gamma p-value uses the SPSS ASE0 (both pair directions)", {
  # Was: C_ij/D_ij counted only the cells below the current cell, which
  # halves P and Q and mis-states ASE0: p = .122 where SPSS prints .027.
  ref <- list(  # chi_squared_output.txt, Symmetric Measures (Tests 2a-2c)
    list(v = c("gender", "region"), gamma = 0.037, p = 0.460),
    list(v = c("education", "employment"), gamma = -0.062, p = 0.027),
    list(v = c("gender", "education"), gamma = -0.011, p = 0.708)
  )
  for (r in ref) {
    res <- chi_square(survey_data, dplyr::all_of(r$v),
                      weights = sampling_weight)$results
    lab <- paste(r$v, collapse = " x ")
    assert_spss(res$gamma, r$gamma, tier = "display", precision = 3,
                label = paste(lab, "gamma"))
    assert_spss(res$gamma_p_value, r$p, tier = "display", precision = 3,
                what = "p_value", label = paste(lab, "gamma p"))
  }
})

test_that("NP-05 chi_square(correct = TRUE): Phi/V from the Pearson chi-square", {
  # Was: Phi and Cramer's V were computed from the Yates-corrected chi2
  # (0.011876 instead of SPSS/phi() 0.012888); the compact print did not
  # say that a continuity correction was applied.
  r <- chi_square(survey_data, gender, region, correct = TRUE)
  expect_equal(r$results$phi, unname(phi(survey_data, gender, region)))
  expect_equal(r$results$cramers_v, unname(cramers_v(survey_data, gender, region)))
  out <- capture.output(print(r))
  expect_true(any(grepl("continuity", out, ignore.case = TRUE)))

  # SPSS Test 2a (chi_squared_output.txt): Continuity Correction .477 /
  # .490, Pearson .548 / .459, Phi = V = .015
  w <- chi_square(survey_data, gender, region, correct = TRUE,
                  weights = sampling_weight)$results
  assert_spss(w$chi_squared, 0.477, tier = "display", precision = 3,
              label = "2a continuity correction")
  assert_spss(w$p_value, 0.490, tier = "display", precision = 3,
              what = "p_value", label = "2a continuity p")
  assert_spss(w$pearson_chi_squared, 0.548, tier = "display", precision = 3,
              label = "2a Pearson chi2")
  assert_spss(w$phi, 0.015, tier = "display", precision = 3, label = "2a phi")
  assert_spss(w$phi_p_value, 0.459, tier = "display", precision = 3,
              what = "p_value", label = "2a phi p")
  out_s <- capture.output(print(summary(r)))
  expect_true(any(grepl("Pearson Chi-Square", out_s, fixed = TRUE)))
  expect_true(any(grepl("Continuity Correction", out_s, fixed = TRUE)))
})

test_that("NP-15 chi_square shows gamma only for two ordinal variables", {
  # Was: Goodman's gamma (with a verbal label) was reported for any pair,
  # also for nominal variables where its sign is arbitrary.
  nominal <- capture.output(print(summary(chi_square(survey_data, gender, region))))
  expect_false(any(grepl("^Gamma", nominal)))
  ordinal <- capture.output(print(summary(
    chi_square(survey_data, education, life_satisfaction))))
  expect_true(any(grepl("^Gamma", ordinal)))
  # goodman_gamma() still returns gamma on request
  expect_false(is.na(goodman_gamma(survey_data, gender, region)))
})

test_that("NP-24 Phi is reported for tables larger than 2x2, as in SPSS", {
  # Was: chi_square() hid Phi outside 2x2 ("only shown for 2x2 tables")
  # while phi() returned it; SPSS prints Phi for any table.
  r <- chi_square(survey_data, education, employment, weights = sampling_weight)
  out <- capture.output(print(summary(r)))
  expect_true(any(grepl("^Phi", out)))
  expect_false(any(grepl("only shown for 2x2", out, fixed = TRUE)))
  # chi_squared_output.txt Test 2b: Phi .228, Cramer's V .132
  assert_spss(r$results$phi, 0.228, tier = "display", precision = 3,
              label = "2b phi (4x5)")
  assert_spss(r$results$cramers_v, 0.132, tier = "display", precision = 3,
              label = "2b Cramer's V (4x5)")
})

test_that("known note: small expected counts warn once, in English", {
  # Was: base chisq.test()'s "Chi-squared approximation may be incorrect"
  # (German here: "Chi-Quadrat-Approximation kann inkorrekt sein") leaked
  # next to mariposa's own expected-count warning - twice the same news.
  small <- data.frame(a = factor(c("A", "A", "A", "B", "B")),
                      b = factor(c("X", "X", "Y", "X", "Y")))
  collect <- function(expr) {
    msgs <- character()
    withCallingHandlers(expr, warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    msgs
  }
  for (f in list(chi_square, cramers_v, goodman_gamma, phi)) {
    msgs <- collect(f(small, a, b))
    expect_length(msgs, 1)
    expect_match(msgs, "expected count")
  }
  expect_length(collect(chi_square(small, a, b, correct = TRUE)), 1)
})

# --- NP-03 / NP-21: mann_whitney mu, alternative, conf.level -----------------

test_that("NP-03 mann_whitney(mu =): U, Z and p refer to the same shift", {
  # Was: U/Z/r were computed for mu = 0 but p came from wilcox.test(mu =),
  # e.g. "Z = -0.696, p < 0.001".
  r <- mann_whitney(survey_data, income, group = gender, mu = 500)$results
  x1 <- survey_data$income[survey_data$gender == "Male"]
  x2 <- survey_data$income[survey_data$gender == "Female"]
  x1 <- x1[!is.na(x1)]
  x2 <- x2[!is.na(x2)]
  wt <- suppressWarnings(stats::wilcox.test(x1, x2, mu = 500, exact = FALSE,
                                            correct = FALSE))
  u1 <- unname(wt$statistic)
  expect_equal(r$U, min(u1, length(x1) * length(x2) - u1))
  expect_equal(r$p_value, wt$p.value, tolerance = 1e-10)
  expect_equal(r$p_value, 2 * stats::pnorm(-abs(r$Z)), tolerance = 1e-10)
})

test_that("NP-03 one-sided mann_whitney: Z is directional and matches p", {
  # Was: Z = -0.226 printed next to p(less) = .589 (Z from min(U), p from
  # the directional test).
  for (alt in c("less", "greater")) {
    r <- mann_whitney(survey_data, life_satisfaction, group = region,
                      alternative = alt)$results
    x1 <- survey_data$life_satisfaction[survey_data$region == "East"]
    x2 <- survey_data$life_satisfaction[survey_data$region == "West"]
    wt <- stats::wilcox.test(x1[!is.na(x1)], x2[!is.na(x2)],
                             alternative = alt, exact = FALSE, correct = FALSE)
    expect_equal(r$p_value, wt$p.value, tolerance = 1e-10)
    expected_p <- if (alt == "less") stats::pnorm(r$Z) else
      stats::pnorm(r$Z, lower.tail = FALSE)
    expect_equal(r$p_value, expected_p, tolerance = 1e-10)
  }
  # two-sided keeps the SPSS convention (Z from the smaller U, <= 0)
  two <- mann_whitney(survey_data, life_satisfaction, group = region)$results
  expect_lte(two$Z, 0)
})

test_that("NP-03 weighted mann_whitney refuses mu != 0 instead of ignoring it", {
  # Was: the weighted test ignored mu but printed "Null hypothesis (mu): 500".
  expect_error(
    mann_whitney(survey_data, income, group = gender, mu = 500,
                 weights = sampling_weight),
    "mu"
  )
})

test_that("NP-21 no confidence level is advertised for rank tests", {
  # Was: summary() printed "Confidence level: 95.0%" although no interval
  # is shown, and wilcox.test(conf.int = TRUE) was computed and discarded
  # (source of German warnings for a constant variable).
  r <- mann_whitney(survey_data, life_satisfaction, group = gender)
  out <- capture.output(print(summary(r)))
  expect_false(any(grepl("Confidence level", out, fixed = TRUE)))
  expect_false(any(grepl("Null hypothesis (mu)", out, fixed = TRUE)))
  src <- paste(deparse(mann_whitney), collapse = "\n")
  expect_false(grepl("conf.int = TRUE", src, fixed = TRUE))
})

# --- NP-04 / NP-14: pairwise_wilcoxon ----------------------------------------

test_that("NP-04 pairwise_wilcoxon legend matches the sign of Z", {
  # Was: Z is based on Var 2 - Var 1 (positive: Var 2 higher, as
  # wilcoxon_test and SPSS "Var 2 - Var 1"), but the legend said
  # "Positive Z: First variable tends to have higher values".
  set.seed(4)
  d <- data.frame(t1 = sample(1:5, 60, TRUE))
  d$t2 <- pmin(d$t1 + sample(0:2, 60, TRUE), 7)
  d$t3 <- d$t1
  pw <- pairwise_wilcoxon(friedman_test(d, t1, t2, t3))
  row <- pw$comparisons[pw$comparisons$var1 == "t1" & pw$comparisons$var2 == "t2", ]
  expect_gt(row$z, 0)                       # t2 is higher
  out <- capture.output(print(summary(pw)))
  expect_true(any(grepl("Positive Z: Second variable", out, fixed = TRUE)))
  expect_false(any(grepl("Positive Z: First", out, fixed = TRUE)))
})

test_that("NP-14 pairwise_wilcoxon reports the N of each pair", {
  # Was: pairs used pairwise-complete cases (as the SPSS /WILCOXON
  # references they are validated against) while friedman_test() uses
  # complete cases, without any n column or note - N 105 vs 75 unexplained.
  fr <- friedman_test(survey_data, trust_government, trust_media, trust_science)
  pw <- pairwise_wilcoxon(fr)
  expect_true("n" %in% names(pw$comparisons))
  # SPSS pairwise_wilcoxon_output.txt: Totals 2227 / 2255 / 2272
  expect_equal(pw$comparisons$n, c(2227, 2255, 2272))
  out <- capture.output(print(summary(pw)))
  expect_true(any(grepl("pairwise deletion", out, fixed = TRUE)))
  expect_true(any(grepl(as.character(fr$results$n), out, fixed = TRUE)))
})

# --- NP-10: fisher_test on larger tables --------------------------------------

test_that("NP-10 fisher_test falls back to Monte Carlo when FEXACT fails", {
  # Was: raw "FEXACT error 501 ... hash table key" abort for a 4x5 table.
  set.seed(42)
  expect_warning(
    r <- fisher_test(survey_data, row = education, col = employment),
    "Monte Carlo"
  )
  expect_true(is.numeric(r$p_value) && r$p_value > 0 && r$p_value <= 1)
  expect_match(r$method, "simulated")
})

test_that("NP-10 fisher_test accepts simulate.p.value and B", {
  # Was: simulate.p.value = TRUE landed in `...` and was ignored.
  set.seed(1)
  expect_no_warning(
    r <- fisher_test(survey_data, row = education, col = employment,
                     simulate.p.value = TRUE, B = 2000)
  )
  expect_match(r$method, "simulated")
  expect_match(r$method, "2000")
})

# --- NP-11: ordered factors in the rank tests ---------------------------------

test_that("NP-11 rank tests accept ordered factors via their codes", {
  # Was: mann_whitney() failed with "'x' must be numeric", wilcoxon_test()
  # with "'-' not meaningful for factors" and printed "Z = ,";
  # kruskal_wallis()/friedman_test() accepted them.
  d <- dplyr::mutate(
    survey_data,
    edu_code = as.integer(education),
    t_gov = factor(trust_government, levels = 1:5, ordered = TRUE),
    t_med = factor(trust_media, levels = 1:5, ordered = TRUE),
    t_sci = factor(trust_science, levels = 1:5, ordered = TRUE)
  )
  mw_f <- mann_whitney(d, education, group = gender)$results
  mw_i <- mann_whitney(d, edu_code, group = gender)$results
  expect_equal(mw_f$Z, mw_i$Z)
  expect_equal(mw_f$U, mw_i$U)

  wt_f <- wilcoxon_test(d, x = t_gov, y = t_med)$results
  wt_i <- wilcoxon_test(d, x = trust_government, y = trust_media)$results
  expect_equal(wt_f$Z, wt_i$Z)

  kw_f <- kruskal_wallis(d, education, group = region)$results
  kw_i <- kruskal_wallis(d, edu_code, group = region)$results
  expect_equal(kw_f$H, kw_i$H)

  fr_f <- friedman_test(d, t_gov, t_med, t_sci)$results
  fr_i <- friedman_test(d, trust_government, trust_media, trust_science)$results
  expect_equal(fr_f$chi_squared, fr_i$chi_squared)
})

test_that("NP-11 rank tests reject nominal variables with a clear error", {
  # Was: a nominal factor ran (kruskal_wallis) or produced
  # "not computed for this group" without a group (mann_whitney).
  expect_error(mann_whitney(survey_data, employment, group = gender),
               "ordered")
  expect_error(kruskal_wallis(survey_data, gender, group = education),
               "ordered")
  expect_error(wilcoxon_test(survey_data, x = gender, y = region), "ordered")
})

# --- NP-12: grouped binomial_test ----------------------------------------------

test_that("NP-12 grouped binomial_test skips a group with one category", {
  # Was: one group where the variable is constant aborted the whole call.
  d <- dplyr::filter(survey_data, !(region == "East" & gender == "Male"))
  expect_warning(
    r <- binomial_test(dplyr::group_by(d, region), gender),
    "East"
  )
  expect_equal(nrow(r$results), 2L)
  expect_true(is.na(r$results$p_value[r$results$region == "East"]))
  expect_false(is.na(r$results$p_value[r$results$region == "West"]))
  out <- c(capture.output(print(r)), capture.output(print(summary(r))))
  expect_true(any(grepl("not computed", out, ignore.case = TRUE)))
  expect_false(any(grepl("(NA)", out, fixed = TRUE)))
  # ungrouped single variable: still a clear error
  expect_error(binomial_test(dplyr::filter(d, region == "East"), gender),
               "2 categories")
})

# --- NP-13: degenerate cases print a reason, not broken text ------------------

no_broken_text <- function(out) {
  expect_false(any(grepl("= ,", out, fixed = TRUE)))
  expect_false(any(grepl("(NA)", out, fixed = TRUE)))
  expect_false(any(grepl("= NA", out, fixed = TRUE)))
  expect_false(any(grepl("NaN", out, fixed = TRUE)))
}

test_that("NP-13 wilcoxon_test with identical variables: Z = 0, p = 1 (SPSS)", {
  # Was: "Z = ," and NA statistics; SPSS reports Z = 0, p = 1.
  d <- dplyr::mutate(survey_data, tg2 = trust_government)
  r <- wilcoxon_test(d, x = trust_government, y = tg2)
  expect_equal(r$results$Z, 0)
  expect_equal(r$results$p_value, 1)
  no_broken_text(c(capture.output(print(r)), capture.output(print(summary(r)))))
})

test_that("NP-13 grouped friedman_test: failing group named, no broken line", {
  # Was: "chi2(NA) = ,  , W = , N = NA" and a warning without the group.
  d <- survey_data
  east <- which(d$region == "East")
  d$trust_media[east[-1]] <- NA
  expect_warning(
    r <- friedman_test(dplyr::group_by(d, region), trust_government,
                       trust_media, trust_science),
    "East"
  )
  out <- c(capture.output(print(r)), capture.output(print(summary(r))))
  no_broken_text(out)
  expect_true(any(grepl("not computed", out, ignore.case = TRUE)))
})

test_that("NP-13 constant and all-NA variables in MW/KW: clear reasons", {
  # Was: KW printed "(see warning)" without any warning (H = NaN); MW
  # leaked German CI warnings and "Fehlender Wert"; an all-NA variable
  # blamed the grouping variable ("Found 0 groups ... use Kruskal-Wallis").
  d <- dplyr::mutate(survey_data, const = 3, allna = NA_real_)
  expect_warning(kw <- kruskal_wallis(d, const, group = education), "identical")
  expect_warning(mw <- mann_whitney(d, const, group = gender), "identical")
  for (res in list(kw, mw)) {
    out <- c(capture.output(print(res)), capture.output(print(summary(res))))
    no_broken_text(out)
    expect_true(any(grepl("not computed", out, ignore.case = TRUE)))
    # no group_by(): no "for this group"
    expect_false(any(grepl("for this group", out, fixed = TRUE)))
  }
  w <- character()
  withCallingHandlers(
    mann_whitney(d, allna, group = gender),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("no valid values", w)))
  expect_false(any(grepl("groups", w)))
  expect_warning(kruskal_wallis(d, allna, group = education), "no valid values")
})

# --- NP-07: value labels instead of codes -------------------------------------

make_labelled_np <- function() {
  set.seed(7)
  n <- 90
  data.frame(
    y = round(rnorm(n, 50, 10)),
    g2 = haven::labelled(rep(c(1, 2), length.out = n),
                         labels = c(East = 1, West = 2)),
    g3 = haven::labelled(rep(c(3, 1, 2), length.out = n),
                         labels = c(Low = 1, Mid = 2, High = 3)),
    bin = haven::labelled(rep(c(1, 2, 2), length.out = n),
                          labels = c(No = 1, Yes = 2))
  )
}

test_that("NP-07 rank tests show value labels of labelled groups", {
  # Was: "Groups: 1, 2, 3", Dunn pairs "1 - 2", "1 vs. 2", rank rows "1".
  skip_if_not_installed("haven")
  d <- make_labelled_np()
  kw <- kruskal_wallis(d, y, group = g3)
  expect_equal(as.character(kw$group_levels), c("Low", "Mid", "High"))
  expect_equal(names(kw$results$group_stats[[1]]), c("Low", "Mid", "High"))
  out <- capture.output(print(summary(kw)))
  expect_true(any(grepl("Low, Mid, High", out, fixed = TRUE)))

  dn <- dunn_test(kw)
  expect_true(all(c(dn$comparisons$group1, dn$comparisons$group2) %in%
                    c("Low", "Mid", "High")))

  mw <- mann_whitney(d, y, group = g2)
  out <- capture.output(print(summary(mw)))
  expect_true(any(grepl("East vs. West", out, fixed = TRUE)))
  expect_equal(mw$results$group_stats[[1]]$group1$name, "East")
})

test_that("NP-07 binomial_test and chisq_gof show value labels", {
  # Was: "Group 1 (1)" and category codes.
  skip_if_not_installed("haven")
  d <- make_labelled_np()
  bt <- binomial_test(d, bin)
  expect_equal(bt$results$cat1_name, "No")
  expect_true(any(grepl("Group 1 (No)", capture.output(print(bt)), fixed = TRUE)))
  gof <- chisq_gof(d, g3)
  expect_equal(gof$frequencies$category, c("Low", "Mid", "High"))
  # a named `expected` may use labels or codes
  by_label <- chisq_gof(d, g3, expected = c(High = .5, Low = .25, Mid = .25))
  by_code <- chisq_gof(d, g3, expected = c(`3` = .5, `1` = .25, `2` = .25))
  expect_equal(by_label$results$chi_squared, by_code$results$chi_squared)
})

test_that("NP-07 numeric 0/1 group is ordered by value (was '1 vs. 0')", {
  # Was: group levels in order of first appearance for non-factors.
  d <- data.frame(y = c(5, 3, 4, 1, 2, 6, 7, 8),
                  g = c(1, 0, 1, 0, 0, 1, 1, 0))
  mw <- mann_whitney(d, y, group = g)
  expect_equal(as.character(mw$group_levels), c("0", "1"))
  expect_equal(mw$results$group_stats[[1]]$group1$name, "0")
})

# --- fisher_test output (NP-07, NP-18, NP-20, NP-06) ---------------------------

test_that("NP-18 fisher_test tables carry variable names and labels", {
  # Was: dimnames "r"/"cc", codes for labelled variables, grouped
  # summaries without the table.
  r <- fisher_test(survey_data, row = gender, col = region)
  expect_equal(names(dimnames(r$table)), c("gender", "region"))
  out <- capture.output(print(summary(r)))
  expect_false(any(grepl("^\\s*r\\s", out)))
  expect_true(any(grepl("gender", out, fixed = TRUE)))

  g <- fisher_test(dplyr::group_by(survey_data, education), row = gender,
                   col = region)
  out_g <- capture.output(print(summary(g)))
  expect_equal(sum(grepl("Contingency Table", out_g, fixed = TRUE)), 4L)

  skip_if_not_installed("haven")
  d <- make_labelled_np()
  l <- fisher_test(d, row = bin, col = g2)
  expect_equal(dimnames(l$table), list(bin = c("No", "Yes"), g2 = c("East", "West")))
})

test_that("NP-20 fisher_test compact line: 3 decimals and the odds ratio", {
  # Was: "p = 0.5435" (4 decimals, unlike the family) and no effect size.
  r <- fisher_test(survey_data, row = gender, col = region)
  out <- capture.output(print(r))
  expect_true(any(grepl("p = 0\\.[0-9]{3}(,| |$)", out)))
  expect_true(any(grepl("OR = ", out, fixed = TRUE)))
  tb <- r$table
  expect_equal(r$results$odds_ratio,
               (tb[1, 1] * tb[2, 2]) / (tb[1, 2] * tb[2, 1]))
})

test_that("NP-06 fisher_test ignores empty factor levels", {
  # Was: a phantom all-zero row entered the table.
  d <- dplyr::mutate(survey_data,
                     gender3 = factor(gender, levels = c("Male", "Female", "Diverse")))
  r3 <- fisher_test(d, row = gender3, col = region)
  r2 <- fisher_test(survey_data, row = gender, col = region)
  expect_equal(dim(r3$table), c(2L, 2L))
  expect_equal(r3$p_value, r2$p_value)
})

# --- mcnemar_test output (NP-13, NP-18, NP-20, NP-07, NP-24) -------------------

mcnemar_data <- function() {
  dplyr::mutate(survey_data,
                gov_high = as.integer(trust_government >= 4),
                media_high = as.integer(trust_media >= 4))
}

test_that("NP-13 mcnemar_test without discordant pairs prints a reason", {
  # Was: "chi2 = ,  (asymp)".
  d <- dplyr::mutate(mcnemar_data(), same = gov_high)
  r <- mcnemar_test(d, var1 = gov_high, var2 = same)
  expect_equal(r$exact_p, 1)
  out <- c(capture.output(print(r)), capture.output(print(summary(r))))
  no_broken_text(out)
  expect_true(any(grepl("no discordant pairs", out, fixed = TRUE)))
})

test_that("NP-18/NP-20 mcnemar_test: named tables, grouped tables, chi2(1)", {
  # Was: dimnames "v1"/"v2"; grouped summaries dropped the table and the
  # "(cc)" note; the compact chi2 had no df.
  d <- mcnemar_data()
  r <- mcnemar_test(d, var1 = gov_high, var2 = media_high)
  expect_equal(names(dimnames(r$table)), c("gov_high", "media_high"))
  expect_true(any(grepl("chi2(1) = ", capture.output(print(r)), fixed = TRUE)))

  g <- mcnemar_test(dplyr::group_by(d, region), var1 = gov_high,
                    var2 = media_high)
  out <- capture.output(print(summary(g)))
  expect_equal(sum(grepl("Contingency Table", out, fixed = TRUE)), 2L)
  expect_true(any(grepl("(cc)", out, fixed = TRUE)))
})

test_that("NP-07 mcnemar_test shows value labels", {
  skip_if_not_installed("haven")
  d <- make_labelled_np()
  d$bin2 <- haven::labelled(rev(as.numeric(d$bin)), labels = c(No = 1, Yes = 2))
  r <- mcnemar_test(d, var1 = bin, var2 = bin2)
  expect_equal(dimnames(r$table), list(bin = c("No", "Yes"), bin2 = c("No", "Yes")))
})

test_that("NP-24 mcnemar_test rejects variables with different categories", {
  # Was: {0,1} vs {1,2} was tabulated as if the categories matched.
  d <- dplyr::mutate(mcnemar_data(), media12 = media_high + 1L)
  expect_error(mcnemar_test(d, var1 = gov_high, var2 = media12), "categor")
  # one variable observing only one of the two shared categories is fine
  d2 <- dplyr::mutate(mcnemar_data(), all_high = 1L)
  expect_no_error(suppressWarnings(
    mcnemar_test(d2, var1 = gov_high, var2 = all_high)))
  r2 <- mcnemar_test(d2, var1 = gov_high, var2 = all_high)
  expect_equal(dim(r2$table), c(2L, 2L))
})

test_that("NP-24 mann_whitney without `group` gives a clear error", {
  # Was: internal rlang error "Can't extract column with `g_name`".
  expect_error(mann_whitney(survey_data, age), "group.*required")
})

# --- NP-16 / NP-17 / NP-19 / NP-20: output formatting -------------------------

test_that("NP-16/NP-17 summaries: SPSS p-values, no NA cells, fixed decimals", {
  # Was: "p value 0" in the KW/Wilcoxon/Friedman/binomial/MW summaries,
  # "Total 2500 NA", "Ties 0 NA NA", MW "n = 1149.0", and columns mixing
  # decimals (0.52 vs 0.537, Mean Rank 19 vs 40.19).
  outs <- list(
    kw = capture.output(print(summary(
      kruskal_wallis(survey_data, income, group = education)))),
    wt = capture.output(print(summary(
      wilcoxon_test(survey_data, x = trust_government, y = trust_science)))),
    fr = capture.output(print(summary(
      friedman_test(survey_data, trust_government, trust_media, trust_science)))),
    mw = capture.output(print(summary(
      mann_whitney(dplyr::mutate(survey_data, retired = employment == "Retired"),
                   age, group = retired)))),
    bt = capture.output(print(summary(binomial_test(survey_data, gender, p = 0.4))))
  )
  for (nm in names(outs)) {
    out <- outs[[nm]]
    expect_true(any(grepl("<.001", out, fixed = TRUE)), label = nm)
    expect_false(any(grepl("\\bNA\\b", out)), label = nm)
  }
  mw <- capture.output(print(summary(
    mann_whitney(survey_data, life_satisfaction, group = gender))))
  expect_true(any(grepl("n = 1149$", mw)))
  expect_false(any(grepl("1149.0", mw, fixed = TRUE)))

  bt <- outs$bt
  prop_rows <- grep("Group [12]:|Total", bt, value = TRUE)
  expect_true(all(grepl("[0-9]\\.[0-9]{3}\\s*$", prop_rows)))
})

test_that("NP-20 compact lines: labelled effect sizes, summary hint", {
  # Was: KW "eps2 = 0.044" and Friedman "W = 0.236" without interpretation,
  # no "Use summary()" hint for mann_whitney.
  kw <- capture.output(print(kruskal_wallis(survey_data, income, group = education)))
  expect_true(any(grepl("eps2 = [0-9.]+ \\((negligible|small|medium|large)\\)", kw)))
  fr <- capture.output(print(friedman_test(survey_data, trust_government,
                                           trust_media, trust_science)))
  expect_true(any(grepl("Kendall's W = [0-9.]+ \\((negligible|weak|moderate|strong)\\)", fr)))
  mw <- capture.output(print(mann_whitney(survey_data, life_satisfaction,
                                          group = gender)))
  expect_true(any(grepl("Use summary()", mw, fixed = TRUE)))
  expect_false(any(grepl(" ,", c(kw, fr, mw), fixed = TRUE)))
})

test_that("NP-20 Mann-Whitney and Wilcoxon share one r interpretation", {
  # Was: different thresholds/wording in the two summaries.
  mw <- capture.output(print(summary(mann_whitney(survey_data, life_satisfaction,
                                                  group = gender))))
  wt <- capture.output(print(summary(wilcoxon_test(survey_data, x = trust_government,
                                                   y = trust_media))))
  block <- function(out) {
    i <- grep("Effect Size Interpretation (r)", out, fixed = TRUE)
    out[i:(i + 4)]
  }
  expect_equal(block(mw), block(wt))
})

test_that("NP-19 dunn_test summary does not wrap long labels", {
  # Was: print(data.frame) wrapped the comparison table at 80 characters.
  set.seed(9)
  labs <- paste("A rather long education category name", 1:5)
  d <- data.frame(y = rnorm(200), g = factor(sample(labs, 200, TRUE), levels = labs))
  dn <- dunn_test(kruskal_wallis(d, y, group = g))
  out <- capture.output(print(summary(dn)))
  for (i in seq_len(nrow(dn$comparisons))) {
    hit <- grepl(dn$comparisons$group1[i], out, fixed = TRUE) &
      grepl(dn$comparisons$group2[i], out, fixed = TRUE)
    expect_true(any(hit))
  }
})

test_that("EDGE-13 mann_whitney names the group count, not a KW hint, for 1 group", {
  # Was: "Found 1 group ... For >2 groups, use a Kruskal-Wallis test" when
  # a split had only one group left.
  d <- dplyr::filter(survey_data, !(region == "East" & gender == "Male"))
  w <- character()
  withCallingHandlers(
    mann_whitney(dplyr::group_by(d, region), life_satisfaction, group = gender),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("East", w)))
  expect_true(any(grepl("1 group", w)))
  expect_false(any(grepl("Kruskal", w)))
})

test_that("EDGE-13/NP-13 pairwise_wilcoxon: tied pair Z = 0, warnings name the group", {
  # Was: an all-tied pair returned NA silently; failures were reported
  # without the group.
  d <- dplyr::mutate(survey_data, tg2 = trust_government)
  pw <- pairwise_wilcoxon(friedman_test(d, trust_government, tg2, trust_media))
  tied <- pw$comparisons[pw$comparisons$var1 == "trust_government" &
                           pw$comparisons$var2 == "tg2", ]
  expect_equal(tied$z, 0)
  expect_equal(tied$p, 1)
})

test_that("NP-23 cramers_v on a large table is fast", {
  # Was: ~50 s for age x income (quadruple R loop over `[.table`).
  skip_on_cran()
  elapsed <- system.time(
    suppressWarnings(v <- cramers_v(survey_data, age, income))
  )[["elapsed"]]
  expect_lt(elapsed, 10)
  expect_true(is.finite(v))
})
