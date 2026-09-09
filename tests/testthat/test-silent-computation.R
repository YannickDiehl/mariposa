# =============================================================================
# Silent-computation contract (CRAN review 2026-09)
# =============================================================================
# CRAN policy: functions must not write to the console with print()/cat()
# except in print/summary/interactive methods. All cat() calls in this
# package live in the print()/summary() display layer (print_helpers.R,
# utils-format.R, and per-class print methods / display closures such as
# the correlation specs' pair_extras). This suite PROVES the contract:
# every analysis (computation) entry point must produce zero stdout.
# Conditions (messages/warnings, e.g. cli_inform) are allowed — they are
# suppressable via suppressMessages()/suppressWarnings() — and are
# silenced here so only raw stdout writes would fail the assertion.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)

data(survey_data, envir = environment())

expect_no_stdout <- function(expr, label) {
  # invisible() keeps capture.output from auto-printing the (visible)
  # return value — only genuine cat()/print() calls inside the
  # computation would produce output here.
  out <- capture.output(
    suppressMessages(suppressWarnings(invisible(force(expr)))),
    type = "output"
  )
  expect_identical(out, character(0),
                   label = sprintf("stdout from %s", label))
}

test_that("descriptive and transformation functions compute silently", {
  expect_no_stdout(describe(survey_data, age, weights = sampling_weight), "describe")
  expect_no_stdout(frequency(survey_data, education), "frequency")
  expect_no_stdout(crosstab(survey_data, gender, region, weights = sampling_weight), "crosstab")
  expect_no_stdout(multiple_response(
    mutate(survey_data, g = as.integer(trust_government >= 4),
           m = as.integer(trust_media >= 4)), g, m), "multiple_response")
  expect_no_stdout(std(survey_data, age), "std")
  expect_no_stdout(center(survey_data, age), "center")
  expect_no_stdout(row_means(survey_data, trust_government, trust_media,
                             trust_science), "row_means")
  expect_no_stdout(w_mean(survey_data, age, weights = sampling_weight), "w_mean")
})

test_that("hypothesis tests compute silently", {
  expect_no_stdout(t_test(survey_data, life_satisfaction, group = gender), "t_test")
  expect_no_stdout(oneway_anova(survey_data, life_satisfaction,
                                group = education), "oneway_anova")
  aov1 <- oneway_anova(survey_data, life_satisfaction, group = education)
  expect_no_stdout(factorial_anova(survey_data, dv = income,
                                   between = c(gender, region)), "factorial_anova")
  expect_no_stdout(ancova(survey_data, dv = income, between = gender,
                          covariate = age), "ancova")
  expect_no_stdout(chi_square(survey_data, gender, region), "chi_square")
  expect_no_stdout(mann_whitney(survey_data, life_satisfaction, group = gender), "mann_whitney")
  expect_no_stdout(kruskal_wallis(survey_data, life_satisfaction,
                                  group = education), "kruskal_wallis")
  expect_no_stdout(binomial_test(survey_data, gender), "binomial_test")
  expect_no_stdout(fisher_test(survey_data[1:80, ], gender, region), "fisher_test")
  expect_no_stdout(chisq_gof(survey_data, education), "chisq_gof")
  expect_no_stdout(normality_test(survey_data, age, income), "normality_test")
  expect_no_stdout(levene_test(aov1), "levene_test")
  expect_no_stdout(tukey_test(aov1), "tukey_test")
  expect_no_stdout(scheffe_test(aov1), "scheffe_test")
  expect_no_stdout(dunn_test(kruskal_wallis(survey_data, life_satisfaction,
                                            group = education)), "dunn_test")
})

test_that("import/export functions write nothing to stdout (messages only)", {
  skip_if_not_installed("haven")
  tmp <- tempfile(fileext = ".sav")
  on.exit(unlink(tmp), add = TRUE)
  expect_no_stdout(write_spss(survey_data, tmp), "write_spss")
  expect_no_stdout(read_spss(tmp), "read_spss")
  # The success note is a suppressable message-based condition, not stdout
  expect_message(write_spss(survey_data, tmp))
})

test_that("correlation, scale, and regression functions compute silently", {
  expect_no_stdout(pearson_cor(survey_data, age, income), "pearson_cor")
  expect_no_stdout(spearman_rho(survey_data, age, income), "spearman_rho")
  expect_no_stdout(kendall_tau(survey_data[1:200, ], age, income), "kendall_tau")
  expect_no_stdout(partial_cor(survey_data, life_satisfaction, income,
                               controls = age), "partial_cor")
  expect_no_stdout(reliability(survey_data, trust_government, trust_media,
                               trust_science), "reliability")
  expect_no_stdout(efa(survey_data, trust_government, trust_media,
                       trust_science, political_orientation,
                       n_factors = 1), "efa")
  expect_no_stdout(linear_regression(survey_data,
                                     life_satisfaction ~ age + income), "linear_regression")
  d <- survey_data
  d$hl <- as.integer(d$life_satisfaction >= 4)
  expect_no_stdout(logistic_regression(d, hl ~ age + income), "logistic_regression")
  m <- logistic_regression(d, hl ~ age + income)
  expect_no_stdout(marginal_effects(m), "marginal_effects")
})
