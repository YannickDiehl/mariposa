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
