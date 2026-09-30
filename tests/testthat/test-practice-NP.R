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

test_that("NP-23 cramers_v on a large table is fast", {
  # Was: ~50 s for age x income (quadruple R loop over `[.table`).
  skip_on_cran()
  elapsed <- system.time(
    suppressWarnings(v <- cramers_v(survey_data, age, income))
  )[["elapsed"]]
  expect_lt(elapsed, 10)
  expect_true(is.finite(v))
})
