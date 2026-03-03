# ===========================================================================
# Summary Method Tests — Coverage for all S3 summary methods
# ===========================================================================
# These tests ensure summary methods return correct classes, boolean toggles
# control output sections, and compact print() remains functional.

library(dplyr)

# --- Helpers ----------------------------------------------------------------
expect_summary_class <- function(obj, expected_class) {
  expect_s3_class(obj, expected_class)
  expect_true("show" %in% names(obj))
  expect_true("digits" %in% names(obj))
}

expect_summary_prints <- function(obj, pattern = NULL) {
  output <- capture.output(print(obj))
  expect_true(length(output) > 0)
  if (!is.null(pattern)) {
    expect_true(
      any(grepl(pattern, output, fixed = TRUE)),
      info = paste0("Pattern '", pattern, "' not found in summary output")
    )
  }
  invisible(output)
}

expect_compact_print <- function(obj, pattern = NULL, max_lines = 15) {
  output <- capture.output(print(obj))
  expect_true(length(output) > 0)
  expect_true(length(output) <= max_lines,
              info = paste0("Compact print has ", length(output),
                            " lines, expected <= ", max_lines))
  if (!is.null(pattern)) {
    expect_true(
      any(grepl(pattern, output, fixed = TRUE)),
      info = paste0("Pattern '", pattern, "' not found in compact output")
    )
  }
  invisible(output)
}

# ===========================================================================
# 1. mann_whitney()
# ===========================================================================
test_that("summary.mann_whitney returns correct class", {
  result <- mann_whitney(survey_data, life_satisfaction, group = gender)
  s <- summary(result)
  expect_summary_class(s, "summary.mann_whitney")
  expect_true(all(c("ranks", "results", "effect_sizes") %in% names(s$show)))
})

test_that("print.summary.mann_whitney produces output", {
  result <- mann_whitney(survey_data, life_satisfaction, group = gender)
  s <- summary(result)
  expect_summary_prints(s, "Mann-Whitney")
})

test_that("summary.mann_whitney: ranks toggle works", {
  result <- mann_whitney(survey_data, life_satisfaction, group = gender)

  out_on <- capture.output(print(summary(result, ranks = TRUE)))
  expect_true(any(grepl("rank mean", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, ranks = FALSE)))
  expect_false(any(grepl("rank mean", out_off, fixed = TRUE)))
})

test_that("summary.mann_whitney: results toggle works", {
  result <- mann_whitney(survey_data, life_satisfaction, group = gender)

  out_on <- capture.output(print(summary(result, results = TRUE)))
  expect_true(any(grepl("Mann-Whitney U", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, results = FALSE)))
  # The header always shows, but test results table should not
  out_off_no_header <- out_off[!grepl("Mann-Whitney U Test", out_off, fixed = TRUE)]
  expect_false(any(grepl("Mann-Whitney U", out_off_no_header, fixed = TRUE)))
})

test_that("summary.mann_whitney: all FALSE shows minimal output", {
  result <- mann_whitney(survey_data, life_satisfaction, group = gender)
  s <- summary(result, ranks = FALSE, results = FALSE, effect_sizes = FALSE)
  output <- capture.output(print(s))
  expect_true(any(grepl("Mann-Whitney", output)))
  expect_false(any(grepl("rank mean", output, fixed = TRUE)))
})

test_that("summary.mann_whitney preserves original data", {
  result <- mann_whitney(survey_data, life_satisfaction, group = gender)
  s <- summary(result)
  expect_equal(s$group, result$group)
  expect_equal(s$variables, result$variables)
  expect_equal(nrow(s$results), nrow(result$results))
})

test_that("compact print.mann_whitney works", {
  result <- mann_whitney(survey_data, life_satisfaction, group = gender)
  output <- expect_compact_print(result, "Mann-Whitney U Test")
  expect_true(any(grepl("U = ", output, fixed = TRUE)))
  expect_true(any(grepl("Z = ", output, fixed = TRUE)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("compact print.mann_whitney: weighted", {
  result <- mann_whitney(survey_data, life_satisfaction, group = gender,
                         weights = sampling_weight)
  output <- expect_compact_print(result, "Mann-Whitney")
  expect_true(any(grepl("Weighted", output, fixed = TRUE)))
})

# ===========================================================================
# 2. t_test()
# ===========================================================================
test_that("summary.t_test returns correct class", {
  result <- t_test(survey_data, life_satisfaction, group = gender)
  s <- summary(result)
  expect_summary_class(s, "summary.t_test")
  expect_true(all(c("descriptives", "results", "effect_sizes") %in% names(s$show)))
})

test_that("print.summary.t_test produces output", {
  result <- t_test(survey_data, life_satisfaction, group = gender)
  s <- summary(result)
  expect_summary_prints(s, "t-Test")
})

test_that("summary.t_test: descriptives toggle works", {
  result <- t_test(survey_data, life_satisfaction, group = gender)

  out_on <- capture.output(print(summary(result, descriptives = TRUE)))
  expect_true(any(grepl("Group", out_on)))

  out_off <- capture.output(print(summary(result, descriptives = FALSE)))
  expect_false(any(grepl("Group Statistics", out_off, fixed = TRUE)))
})

test_that("summary.t_test: effect_sizes toggle works", {
  result <- t_test(survey_data, life_satisfaction, group = gender)

  out_on <- capture.output(print(summary(result, effect_sizes = TRUE)))
  expect_true(any(grepl("Effect", out_on)))

  out_off <- capture.output(print(summary(result, effect_sizes = FALSE)))
  expect_false(any(grepl("Effect Size", out_off, fixed = TRUE)))
})

test_that("summary.t_test: all FALSE shows minimal output", {
  result <- t_test(survey_data, life_satisfaction, group = gender)
  s <- summary(result, descriptives = FALSE, results = FALSE, effect_sizes = FALSE)
  output <- capture.output(print(s))
  expect_true(any(grepl("t-Test", output)))
  expect_false(any(grepl("Group Statistics", output, fixed = TRUE)))
  expect_false(any(grepl("Effect Size", output, fixed = TRUE)))
})

test_that("summary.t_test preserves original data", {
  result <- t_test(survey_data, life_satisfaction, group = gender)
  s <- summary(result)
  expect_equal(s$group, result$group)
  expect_equal(s$variables, result$variables)
  expect_equal(nrow(s$results), nrow(result$results))
})

test_that("compact print.t_test works", {
  result <- t_test(survey_data, life_satisfaction, group = gender)
  output <- expect_compact_print(result, "t-Test")
  expect_true(any(grepl("t(", output, fixed = TRUE)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("compact print.t_test: weighted", {
  result <- t_test(survey_data, life_satisfaction, group = gender,
                   weights = sampling_weight)
  output <- expect_compact_print(result, "t-Test")
  expect_true(any(grepl("Weighted", output, fixed = TRUE)))
})

test_that("compact print.t_test: multiple variables", {
  result <- t_test(survey_data, life_satisfaction, age, group = gender)
  output <- expect_compact_print(result, "t-Test", max_lines = 15)
  expect_true(any(grepl("life_satisfaction", output)))
  expect_true(any(grepl("age", output)))
})

# ===========================================================================
# 3. chi_square()
# ===========================================================================
test_that("summary.chi_square returns correct class", {
  result <- chi_square(survey_data, gender, region)
  s <- summary(result)
  expect_summary_class(s, "summary.chi_square")
  expect_true(all(c("observed", "expected", "results", "effect_sizes") %in% names(s$show)))
})

test_that("print.summary.chi_square produces output", {
  result <- chi_square(survey_data, gender, region)
  s <- summary(result)
  expect_summary_prints(s, "Chi-Squared")
})

test_that("summary.chi_square: observed toggle works", {
  result <- chi_square(survey_data, gender, region)

  out_on <- capture.output(print(summary(result, observed = TRUE)))
  expect_true(any(grepl("Observed Frequencies", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, observed = FALSE)))
  expect_false(any(grepl("Observed Frequencies", out_off, fixed = TRUE)))
})

test_that("summary.chi_square: expected toggle works", {
  result <- chi_square(survey_data, gender, region)

  out_on <- capture.output(print(summary(result, expected = TRUE)))
  expect_true(any(grepl("Expected Frequencies", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, expected = FALSE)))
  expect_false(any(grepl("Expected Frequencies", out_off, fixed = TRUE)))
})

test_that("summary.chi_square: effect_sizes toggle works", {
  result <- chi_square(survey_data, gender, region)

  out_on <- capture.output(print(summary(result, effect_sizes = TRUE)))
  expect_true(any(grepl("Effect", out_on)))

  out_off <- capture.output(print(summary(result, effect_sizes = FALSE)))
  expect_false(any(grepl("Effect Size", out_off, fixed = TRUE)))
})

test_that("summary.chi_square: all FALSE shows minimal output", {
  result <- chi_square(survey_data, gender, region)
  s <- summary(result, observed = FALSE, expected = FALSE,
               results = FALSE, effect_sizes = FALSE)
  output <- capture.output(print(s))
  expect_true(any(grepl("Chi-Squared", output)))
  expect_false(any(grepl("Observed", output, fixed = TRUE)))
  expect_false(any(grepl("Expected", output, fixed = TRUE)))
  expect_false(any(grepl("Effect", output)))
})

test_that("summary.chi_square preserves original data", {
  result <- chi_square(survey_data, gender, region)
  s <- summary(result)
  expect_equal(s$variables, result$variables)
  expect_equal(nrow(s$results), nrow(result$results))
})

test_that("compact print.chi_square works", {
  result <- chi_square(survey_data, gender, region)
  output <- expect_compact_print(result, "Chi-Squared Test")
  expect_true(any(grepl("chi2(", output, fixed = TRUE)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("compact print.chi_square: weighted", {
  result <- chi_square(survey_data, gender, region,
                       weights = sampling_weight)
  output <- expect_compact_print(result, "Chi-Squared Test")
  expect_true(any(grepl("Weighted", output, fixed = TRUE)))
})

# ===========================================================================
# 4. pearson_cor()
# ===========================================================================
test_that("summary.pearson_cor returns correct class", {
  result <- pearson_cor(survey_data, age, income)
  s <- summary(result)
  expect_summary_class(s, "summary.pearson_cor")
  expect_true(all(c("correlation_matrix", "pvalue_matrix", "n_matrix") %in% names(s$show)))
})

test_that("print.summary.pearson_cor produces output", {
  result <- pearson_cor(survey_data, age, income)
  s <- summary(result)
  expect_summary_prints(s, "Pearson Correlation")
})

test_that("summary.pearson_cor: correlation_matrix toggle works (2 vars)", {
  result <- pearson_cor(survey_data, age, income)

  out_on <- capture.output(print(summary(result, correlation_matrix = TRUE)))
  expect_true(any(grepl("Correlation", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, correlation_matrix = FALSE)))
  expect_false(any(grepl("r =", out_off, fixed = TRUE)))
})

test_that("summary.pearson_cor: matrix toggles work (3+ vars)", {
  result <- pearson_cor(survey_data, age, income, life_satisfaction)

  out_cor <- capture.output(print(summary(result, correlation_matrix = TRUE,
                                           pvalue_matrix = FALSE, n_matrix = FALSE)))
  expect_true(any(grepl("Correlation Matrix", out_cor, fixed = TRUE)))
  expect_false(any(grepl("Significance Matrix", out_cor, fixed = TRUE)))
  expect_false(any(grepl("Sample Size Matrix", out_cor, fixed = TRUE)))

  out_p <- capture.output(print(summary(result, correlation_matrix = FALSE,
                                         pvalue_matrix = TRUE, n_matrix = FALSE)))
  expect_true(any(grepl("Significance Matrix", out_p, fixed = TRUE)))
  expect_false(any(grepl("Correlation Matrix", out_p, fixed = TRUE)))

  out_n <- capture.output(print(summary(result, correlation_matrix = FALSE,
                                         pvalue_matrix = FALSE, n_matrix = TRUE)))
  expect_true(any(grepl("Sample Size Matrix", out_n, fixed = TRUE)))
  expect_false(any(grepl("Correlation Matrix", out_n, fixed = TRUE)))
})

test_that("summary.pearson_cor: all FALSE shows only header/info", {
  result <- pearson_cor(survey_data, age, income, life_satisfaction)
  s <- summary(result, correlation_matrix = FALSE, pvalue_matrix = FALSE, n_matrix = FALSE)
  output <- capture.output(print(s))
  expect_true(any(grepl("Pearson Correlation", output)))
  expect_false(any(grepl("Correlation Matrix", output, fixed = TRUE)))
  expect_false(any(grepl("Significance Matrix", output, fixed = TRUE)))
  expect_false(any(grepl("Sample Size Matrix", output, fixed = TRUE)))
})

test_that("summary.pearson_cor preserves original data", {
  result <- pearson_cor(survey_data, age, income)
  s <- summary(result)
  expect_equal(s$variables, result$variables)
  expect_equal(s$weights, result$weights)
  expect_equal(nrow(s$correlations), nrow(result$correlations))
})

test_that("compact print.pearson_cor: 2 variables, ungrouped", {
  result <- pearson_cor(survey_data, age, income)
  output <- expect_compact_print(result, "Pearson Correlation")
  expect_true(any(grepl("r = ", output, fixed = TRUE)))
  expect_true(any(grepl("p ", output)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("compact print.pearson_cor: 3+ variables", {
  result <- pearson_cor(survey_data, age, income, life_satisfaction)
  output <- expect_compact_print(result, "Pearson Correlation")
  expect_true(any(grepl("variables", output, fixed = TRUE)))
  expect_true(any(grepl("significant", output, fixed = TRUE)))
})

test_that("compact print.pearson_cor: weighted", {
  result <- pearson_cor(survey_data, age, income, weights = sampling_weight)
  output <- expect_compact_print(result, "Pearson Correlation")
  expect_true(any(grepl("Weighted", output, fixed = TRUE)))
})

test_that("compact print.pearson_cor: grouped", {
  result <- survey_data %>%
    group_by(region) %>%
    pearson_cor(age, income)
  output <- expect_compact_print(result, "Pearson Correlation", max_lines = 30)
  expect_true(any(grepl("region =", output, fixed = TRUE)))
})

test_that("summary.pearson_cor: grouped produces output", {
  result <- survey_data %>%
    group_by(region) %>%
    pearson_cor(age, income)
  s <- summary(result)
  output <- expect_summary_prints(s, "Pearson Correlation")
  expect_true(any(grepl("Group", output)))
})

# ===========================================================================
# 5. spearman_rho()
# ===========================================================================
test_that("summary.spearman_rho returns correct class", {
  result <- spearman_rho(survey_data, age, income)
  s <- summary(result)
  expect_summary_class(s, "summary.spearman_rho")
  expect_true(all(c("correlation_matrix", "pvalue_matrix", "n_matrix") %in% names(s$show)))
})

test_that("print.summary.spearman_rho produces output", {
  result <- spearman_rho(survey_data, age, income)
  s <- summary(result)
  expect_summary_prints(s, "Spearman")
})

test_that("summary.spearman_rho: correlation_matrix toggle works (2 vars)", {
  result <- spearman_rho(survey_data, age, income)

  out_on <- capture.output(print(summary(result, correlation_matrix = TRUE)))
  expect_true(any(grepl("Spearman", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, correlation_matrix = FALSE)))
  expect_false(any(grepl("rho =", out_off, fixed = TRUE)))
})

test_that("summary.spearman_rho: matrix toggles work (3+ vars)", {
  result <- spearman_rho(survey_data, age, income, life_satisfaction)

  out_cor <- capture.output(print(summary(result, correlation_matrix = TRUE,
                                           pvalue_matrix = FALSE, n_matrix = FALSE)))
  expect_true(any(grepl("Rho Matrix", out_cor, fixed = TRUE)))
  expect_false(any(grepl("Significance Matrix", out_cor, fixed = TRUE)))
  expect_false(any(grepl("Sample Size Matrix", out_cor, fixed = TRUE)))

  out_p <- capture.output(print(summary(result, correlation_matrix = FALSE,
                                         pvalue_matrix = TRUE, n_matrix = FALSE)))
  expect_true(any(grepl("Significance Matrix", out_p, fixed = TRUE)))
  expect_false(any(grepl("Rho Matrix", out_p, fixed = TRUE)))

  out_n <- capture.output(print(summary(result, correlation_matrix = FALSE,
                                         pvalue_matrix = FALSE, n_matrix = TRUE)))
  expect_true(any(grepl("Sample Size Matrix", out_n, fixed = TRUE)))
  expect_false(any(grepl("Rho Matrix", out_n, fixed = TRUE)))
})

test_that("summary.spearman_rho: all FALSE shows only header/info", {
  result <- spearman_rho(survey_data, age, income, life_satisfaction)
  s <- summary(result, correlation_matrix = FALSE, pvalue_matrix = FALSE, n_matrix = FALSE)
  output <- capture.output(print(s))
  expect_true(any(grepl("Spearman", output)))
  expect_false(any(grepl("Rho Matrix", output, fixed = TRUE)))
  expect_false(any(grepl("Significance Matrix", output, fixed = TRUE)))
  expect_false(any(grepl("Sample Size Matrix", output, fixed = TRUE)))
})

test_that("summary.spearman_rho preserves original data", {
  result <- spearman_rho(survey_data, age, income)
  s <- summary(result)
  expect_equal(s$variables, result$variables)
  expect_equal(s$weights, result$weights)
  expect_equal(nrow(s$correlations), nrow(result$correlations))
})

test_that("compact print.spearman_rho: 2 variables, ungrouped", {
  result <- spearman_rho(survey_data, age, income)
  output <- expect_compact_print(result, "Spearman Correlation")
  expect_true(any(grepl("rho = ", output, fixed = TRUE)))
  expect_true(any(grepl("p ", output)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("compact print.spearman_rho: 3+ variables", {
  result <- spearman_rho(survey_data, age, income, life_satisfaction)
  output <- expect_compact_print(result, "Spearman Correlation")
  expect_true(any(grepl("variables", output, fixed = TRUE)))
  expect_true(any(grepl("significant", output, fixed = TRUE)))
})

test_that("compact print.spearman_rho: weighted", {
  result <- spearman_rho(survey_data, age, income, weights = sampling_weight)
  output <- expect_compact_print(result, "Spearman Correlation")
  expect_true(any(grepl("Weighted", output, fixed = TRUE)))
})

test_that("compact print.spearman_rho: grouped", {
  result <- survey_data %>%
    group_by(region) %>%
    spearman_rho(age, income)
  output <- expect_compact_print(result, "Spearman Correlation", max_lines = 30)
  expect_true(any(grepl("region =", output, fixed = TRUE)))
})

test_that("summary.spearman_rho: grouped produces output", {
  result <- survey_data %>%
    group_by(region) %>%
    spearman_rho(age, income)
  s <- summary(result)
  output <- expect_summary_prints(s, "Spearman")
  expect_true(any(grepl("Group", output)))
})

# ===========================================================================
# 6. kendall_tau()
# ===========================================================================
test_that("summary.kendall_tau returns correct class", {
  result <- kendall_tau(survey_data, age, income)
  s <- summary(result)
  expect_summary_class(s, "summary.kendall_tau")
  expect_true(all(c("correlation_matrix", "pvalue_matrix", "n_matrix") %in% names(s$show)))
})

test_that("print.summary.kendall_tau produces output", {
  result <- kendall_tau(survey_data, age, income)
  s <- summary(result)
  expect_summary_prints(s, "Kendall")
})

test_that("summary.kendall_tau: correlation_matrix toggle works (2 vars)", {
  result <- kendall_tau(survey_data, age, income)

  out_on <- capture.output(print(summary(result, correlation_matrix = TRUE)))
  expect_true(any(grepl("Kendall", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, correlation_matrix = FALSE)))
  expect_false(any(grepl("tau-b =", out_off, fixed = TRUE)))
})

test_that("summary.kendall_tau: matrix toggles work (3+ vars)", {
  result <- kendall_tau(survey_data, age, income, life_satisfaction)

  out_cor <- capture.output(print(summary(result, correlation_matrix = TRUE,
                                           pvalue_matrix = FALSE, n_matrix = FALSE)))
  expect_true(any(grepl("Tau-b Matrix", out_cor, fixed = TRUE)))
  expect_false(any(grepl("Significance Matrix", out_cor, fixed = TRUE)))
  expect_false(any(grepl("Sample Size Matrix", out_cor, fixed = TRUE)))

  out_p <- capture.output(print(summary(result, correlation_matrix = FALSE,
                                         pvalue_matrix = TRUE, n_matrix = FALSE)))
  expect_true(any(grepl("Significance Matrix", out_p, fixed = TRUE)))
  expect_false(any(grepl("Tau-b Matrix", out_p, fixed = TRUE)))

  out_n <- capture.output(print(summary(result, correlation_matrix = FALSE,
                                         pvalue_matrix = FALSE, n_matrix = TRUE)))
  expect_true(any(grepl("Sample Size Matrix", out_n, fixed = TRUE)))
  expect_false(any(grepl("Tau-b Matrix", out_n, fixed = TRUE)))
})

test_that("summary.kendall_tau: all FALSE shows only header/info", {
  result <- kendall_tau(survey_data, age, income, life_satisfaction)
  s <- summary(result, correlation_matrix = FALSE, pvalue_matrix = FALSE, n_matrix = FALSE)
  output <- capture.output(print(s))
  expect_true(any(grepl("Kendall", output)))
  expect_false(any(grepl("Tau-b Matrix", output, fixed = TRUE)))
  expect_false(any(grepl("Significance Matrix", output, fixed = TRUE)))
  expect_false(any(grepl("Sample Size Matrix", output, fixed = TRUE)))
})

test_that("summary.kendall_tau preserves original data", {
  result <- kendall_tau(survey_data, age, income)
  s <- summary(result)
  expect_equal(s$variables, result$variables)
  expect_equal(s$weights, result$weights)
  expect_equal(nrow(s$correlations), nrow(result$correlations))
})

test_that("compact print.kendall_tau: 2 variables, ungrouped", {
  result <- kendall_tau(survey_data, age, income)
  output <- expect_compact_print(result, "Kendall's Tau")
  expect_true(any(grepl("tau = ", output, fixed = TRUE)))
  expect_true(any(grepl("p ", output)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("compact print.kendall_tau: 3+ variables", {
  result <- kendall_tau(survey_data, age, income, life_satisfaction)
  output <- expect_compact_print(result, "Kendall's Tau")
  expect_true(any(grepl("variables", output, fixed = TRUE)))
  expect_true(any(grepl("significant", output, fixed = TRUE)))
})

test_that("compact print.kendall_tau: weighted", {
  result <- kendall_tau(survey_data, age, income, weights = sampling_weight)
  output <- expect_compact_print(result, "Kendall's Tau")
  expect_true(any(grepl("Weighted", output, fixed = TRUE)))
})

test_that("compact print.kendall_tau: grouped", {
  result <- survey_data %>%
    group_by(region) %>%
    kendall_tau(age, income)
  output <- expect_compact_print(result, "Kendall's Tau", max_lines = 30)
  expect_true(any(grepl("region =", output, fixed = TRUE)))
})

test_that("summary.kendall_tau: grouped produces output", {
  result <- survey_data %>%
    group_by(region) %>%
    kendall_tau(age, income)
  s <- summary(result)
  output <- expect_summary_prints(s, "Kendall")
  expect_true(any(grepl("Group", output)))
})

# ===========================================================================
# 7. oneway_anova()
# ===========================================================================
test_that("summary.oneway_anova returns correct class", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education)
  s <- summary(result)
  expect_summary_class(s, "summary.oneway_anova")
  expect_true(all(c("descriptives", "anova_table", "effect_sizes") %in% names(s$show)))
})

test_that("print.summary.oneway_anova produces output", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education)
  s <- summary(result)
  expect_summary_prints(s, "One-Way ANOVA")
})

test_that("summary.oneway_anova: descriptives toggle works", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education)

  out_on <- capture.output(print(summary(result, descriptives = TRUE)))
  expect_true(any(grepl("Descriptive Statistics", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, descriptives = FALSE)))
  expect_false(any(grepl("Descriptive Statistics", out_off, fixed = TRUE)))
})

test_that("summary.oneway_anova: anova_table toggle works", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education)

  out_on <- capture.output(print(summary(result, anova_table = TRUE)))
  expect_true(any(grepl("ANOVA", out_on)))

  out_off <- capture.output(print(summary(result, anova_table = FALSE)))
  # The header always shows ANOVA, but test results table should not
  out_off_no_header <- out_off[!grepl("One-Way ANOVA", out_off, fixed = TRUE)]
  expect_false(any(grepl("ANOVA Results", out_off_no_header, fixed = TRUE)))
})

test_that("summary.oneway_anova: effect_sizes toggle works", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education)

  out_on <- capture.output(print(summary(result, effect_sizes = TRUE)))
  expect_true(any(grepl("Effect Size", out_on) | grepl("eta", out_on, ignore.case = TRUE)))

  out_off <- capture.output(print(summary(result, effect_sizes = FALSE)))
  expect_false(any(grepl("Effect Size", out_off, fixed = TRUE)))
})

test_that("summary.oneway_anova: all FALSE shows minimal output", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education)
  out_all <- capture.output(print(summary(result, descriptives = FALSE,
                                           anova_table = FALSE,
                                           effect_sizes = FALSE)))
  out_full <- capture.output(print(summary(result)))
  expect_true(length(out_all) < length(out_full))
})

test_that("summary.oneway_anova preserves original data", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education)
  s <- summary(result)
  expect_equal(s$variables, result$variables)
  expect_equal(s$group, result$group)
  expect_equal(nrow(s$results), nrow(result$results))
})

test_that("compact print.oneway_anova works", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education)
  output <- expect_compact_print(result, "One-Way ANOVA")
  expect_true(any(grepl("F(", output, fixed = TRUE)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("summary.oneway_anova: weighted produces output", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education,
                          weights = sampling_weight)
  s <- summary(result)
  output <- expect_summary_prints(s, "Weighted")
})

test_that("compact print.oneway_anova: weighted", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education,
                          weights = sampling_weight)
  output <- expect_compact_print(result, "One-Way ANOVA")
  expect_true(any(grepl("Weighted", output, fixed = TRUE)))
})

# ===========================================================================
# 8. reliability()
# ===========================================================================
test_that("summary.reliability returns correct class", {
  result <- reliability(survey_data, trust_government, trust_media, trust_science)
  s <- summary(result)
  expect_summary_class(s, "summary.reliability")
  expect_true(all(c("reliability_statistics", "item_statistics",
                     "inter_item_correlations", "item_total_statistics") %in% names(s$show)))
})

test_that("print.summary.reliability produces output", {
  result <- reliability(survey_data, trust_government, trust_media, trust_science)
  s <- summary(result)
  expect_summary_prints(s, "Reliability")
})

test_that("summary.reliability: reliability_statistics toggle works", {
  result <- reliability(survey_data, trust_government, trust_media, trust_science)

  out_on <- capture.output(print(summary(result, reliability_statistics = TRUE)))
  expect_true(any(grepl("Reliability Statistics", out_on, fixed = TRUE)) ||
              any(grepl("Cronbach", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, reliability_statistics = FALSE)))
  expect_false(any(grepl("Reliability Statistics", out_off, fixed = TRUE)))
})

test_that("summary.reliability: item_statistics toggle works", {
  result <- reliability(survey_data, trust_government, trust_media, trust_science)

  out_on <- capture.output(print(summary(result, item_statistics = TRUE)))
  expect_true(any(grepl("Item Statistics", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, item_statistics = FALSE)))
  expect_false(any(grepl("Item Statistics", out_off, fixed = TRUE)))
})

test_that("summary.reliability: inter_item_correlations toggle works", {
  result <- reliability(survey_data, trust_government, trust_media, trust_science)

  out_on <- capture.output(print(summary(result, inter_item_correlations = TRUE)))
  expect_true(any(grepl("Inter-Item Correlation", out_on, fixed = TRUE)) ||
              any(grepl("Correlation", out_on)))

  out_off <- capture.output(print(summary(result, inter_item_correlations = FALSE)))
  expect_false(any(grepl("Inter-Item Correlation", out_off, fixed = TRUE)))
})

test_that("summary.reliability: item_total_statistics toggle works", {
  result <- reliability(survey_data, trust_government, trust_media, trust_science)

  out_on <- capture.output(print(summary(result, item_total_statistics = TRUE)))
  expect_true(any(grepl("Item-Total", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, item_total_statistics = FALSE)))
  expect_false(any(grepl("Item-Total", out_off, fixed = TRUE)))
})

test_that("summary.reliability: all FALSE shows minimal output", {
  result <- reliability(survey_data, trust_government, trust_media, trust_science)
  out_all <- capture.output(print(summary(result, reliability_statistics = FALSE,
                                           item_statistics = FALSE,
                                           inter_item_correlations = FALSE,
                                           item_total_statistics = FALSE)))
  out_full <- capture.output(print(summary(result)))
  expect_true(length(out_all) < length(out_full))
})

test_that("summary.reliability preserves original data", {
  result <- reliability(survey_data, trust_government, trust_media, trust_science)
  s <- summary(result)
  expect_equal(s$variables, result$variables)
  expect_equal(s$n_items, result$n_items)
  expect_equal(s$alpha, result$alpha)
})

test_that("compact print.reliability works", {
  result <- reliability(survey_data, trust_government, trust_media, trust_science)
  output <- expect_compact_print(result, "Reliability")
  expect_true(any(grepl("Alpha", output, fixed = TRUE)))
})

test_that("compact print.reliability: weighted", {
  result <- reliability(survey_data, trust_government, trust_media, trust_science,
                        weights = sampling_weight)
  output <- expect_compact_print(result, "Reliability")
  expect_true(any(grepl("Weighted", output, fixed = TRUE)))
})

# ===========================================================================
# 9. linear_regression()
# ===========================================================================
test_that("summary.linear_regression returns correct class", {
  result <- linear_regression(survey_data, life_satisfaction ~ age + income)
  s <- summary(result)
  expect_summary_class(s, "summary.linear_regression")
  expect_true(all(c("model_summary", "anova_table", "coefficients",
                     "descriptives") %in% names(s$show)))
})

test_that("print.summary.linear_regression produces output", {
  result <- linear_regression(survey_data, life_satisfaction ~ age + income)
  s <- summary(result)
  expect_summary_prints(s, "Linear Regression")
})

test_that("summary.linear_regression: model_summary toggle works", {
  result <- linear_regression(survey_data, life_satisfaction ~ age + income)

  out_on <- capture.output(print(summary(result, model_summary = TRUE)))
  expect_true(any(grepl("Model Summary", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, model_summary = FALSE)))
  expect_false(any(grepl("Model Summary", out_off, fixed = TRUE)))
})

test_that("summary.linear_regression: anova_table toggle works", {
  result <- linear_regression(survey_data, life_satisfaction ~ age + income)

  out_on <- capture.output(print(summary(result, anova_table = TRUE)))
  expect_true(any(grepl("ANOVA", out_on)))

  out_off <- capture.output(print(summary(result, anova_table = FALSE)))
  # The header always has "Linear Regression", filter it out
  out_off_no_header <- out_off[!grepl("Linear Regression", out_off, fixed = TRUE)]
  expect_false(any(grepl("ANOVA", out_off_no_header)))
})

test_that("summary.linear_regression: coefficients toggle works", {
  result <- linear_regression(survey_data, life_satisfaction ~ age + income)

  out_on <- capture.output(print(summary(result, coefficients = TRUE)))
  expect_true(any(grepl("Coefficients", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, coefficients = FALSE)))
  expect_false(any(grepl("Coefficients", out_off, fixed = TRUE)))
})

test_that("summary.linear_regression: all FALSE shows minimal output", {
  result <- linear_regression(survey_data, life_satisfaction ~ age + income)
  out_all <- capture.output(print(summary(result, model_summary = FALSE,
                                           anova_table = FALSE,
                                           coefficients = FALSE,
                                           descriptives = FALSE)))
  out_full <- capture.output(print(summary(result)))
  expect_true(length(out_all) < length(out_full))
})

test_that("summary.linear_regression preserves original data", {
  result <- linear_regression(survey_data, life_satisfaction ~ age + income)
  s <- summary(result)
  expect_equal(s$formula, result$formula)
  expect_equal(s$n, result$n)
  expect_equal(s$model_summary$R_squared, result$model_summary$R_squared)
})

test_that("compact print.linear_regression works", {
  result <- linear_regression(survey_data, life_satisfaction ~ age + income)
  output <- expect_compact_print(result, "Linear Regression")
  expect_true(any(grepl("R2", output, fixed = TRUE)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("compact print.linear_regression: weighted", {
  result <- linear_regression(survey_data, life_satisfaction ~ age + income,
                               weights = sampling_weight)
  output <- expect_compact_print(result, "Linear Regression")
  expect_true(any(grepl("Weighted", output, fixed = TRUE)))
})

# ===========================================================================
# 10. logistic_regression()
# ===========================================================================
test_that("summary.logistic_regression returns correct class", {
  sd2 <- survey_data %>%
    mutate(high_sat = as.integer(life_satisfaction > median(life_satisfaction, na.rm = TRUE)))
  result <- logistic_regression(sd2, dependent = high_sat, predictors = c(age, income))
  s <- summary(result)
  expect_summary_class(s, "summary.logistic_regression")
  expect_true(all(c("omnibus_test", "model_summary", "hosmer_lemeshow",
                     "classification", "coefficients") %in% names(s$show)))
})

test_that("print.summary.logistic_regression produces output", {
  sd2 <- survey_data %>%
    mutate(high_sat = as.integer(life_satisfaction > median(life_satisfaction, na.rm = TRUE)))
  result <- logistic_regression(sd2, dependent = high_sat, predictors = c(age, income))
  s <- summary(result)
  expect_summary_prints(s, "Logistic Regression")
})

test_that("summary.logistic_regression: omnibus_test toggle works", {
  sd2 <- survey_data %>%
    mutate(high_sat = as.integer(life_satisfaction > median(life_satisfaction, na.rm = TRUE)))
  result <- logistic_regression(sd2, dependent = high_sat, predictors = c(age, income))

  out_on <- capture.output(print(summary(result, omnibus_test = TRUE)))
  expect_true(any(grepl("Omnibus", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, omnibus_test = FALSE)))
  expect_false(any(grepl("Omnibus", out_off, fixed = TRUE)))
})

test_that("summary.logistic_regression: model_summary toggle works", {
  sd2 <- survey_data %>%
    mutate(high_sat = as.integer(life_satisfaction > median(life_satisfaction, na.rm = TRUE)))
  result <- logistic_regression(sd2, dependent = high_sat, predictors = c(age, income))

  out_on <- capture.output(print(summary(result, model_summary = TRUE)))
  expect_true(any(grepl("Model Summary", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, model_summary = FALSE)))
  expect_false(any(grepl("Model Summary", out_off, fixed = TRUE)))
})

test_that("summary.logistic_regression: hosmer_lemeshow toggle works", {
  sd2 <- survey_data %>%
    mutate(high_sat = as.integer(life_satisfaction > median(life_satisfaction, na.rm = TRUE)))
  result <- logistic_regression(sd2, dependent = high_sat, predictors = c(age, income))

  out_on <- capture.output(print(summary(result, hosmer_lemeshow = TRUE)))
  expect_true(any(grepl("Hosmer", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, hosmer_lemeshow = FALSE)))
  expect_false(any(grepl("Hosmer", out_off, fixed = TRUE)))
})

test_that("summary.logistic_regression: classification toggle works", {
  sd2 <- survey_data %>%
    mutate(high_sat = as.integer(life_satisfaction > median(life_satisfaction, na.rm = TRUE)))
  result <- logistic_regression(sd2, dependent = high_sat, predictors = c(age, income))

  out_on <- capture.output(print(summary(result, classification = TRUE)))
  expect_true(any(grepl("Classification", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, classification = FALSE)))
  expect_false(any(grepl("Classification", out_off, fixed = TRUE)))
})

test_that("summary.logistic_regression: coefficients toggle works", {
  sd2 <- survey_data %>%
    mutate(high_sat = as.integer(life_satisfaction > median(life_satisfaction, na.rm = TRUE)))
  result <- logistic_regression(sd2, dependent = high_sat, predictors = c(age, income))

  out_on <- capture.output(print(summary(result, coefficients = TRUE)))
  expect_true(any(grepl("Equation", out_on) | grepl("Coefficients", out_on)))

  out_off <- capture.output(print(summary(result, coefficients = FALSE)))
  expect_false(any(grepl("Variables in the Equation", out_off, fixed = TRUE)))
})

test_that("summary.logistic_regression: all FALSE shows minimal output", {
  sd2 <- survey_data %>%
    mutate(high_sat = as.integer(life_satisfaction > median(life_satisfaction, na.rm = TRUE)))
  result <- logistic_regression(sd2, dependent = high_sat, predictors = c(age, income))
  out_all <- capture.output(print(summary(result, omnibus_test = FALSE,
                                           model_summary = FALSE,
                                           hosmer_lemeshow = FALSE,
                                           classification = FALSE,
                                           coefficients = FALSE)))
  out_full <- capture.output(print(summary(result)))
  expect_true(length(out_all) < length(out_full))
})

test_that("summary.logistic_regression preserves original data", {
  sd2 <- survey_data %>%
    mutate(high_sat = as.integer(life_satisfaction > median(life_satisfaction, na.rm = TRUE)))
  result <- logistic_regression(sd2, dependent = high_sat, predictors = c(age, income))
  s <- summary(result)
  expect_equal(s$formula, result$formula)
  expect_equal(s$n, result$n)
  expect_equal(s$omnibus_test$chi_sq, result$omnibus_test$chi_sq)
})

test_that("compact print.logistic_regression works", {
  sd2 <- survey_data %>%
    mutate(high_sat = as.integer(life_satisfaction > median(life_satisfaction, na.rm = TRUE)))
  result <- logistic_regression(sd2, dependent = high_sat, predictors = c(age, income))
  output <- expect_compact_print(result, "Logistic Regression")
  expect_true(any(grepl("chi2(", output, fixed = TRUE)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

# ===========================================================================
# 11. factorial_anova()
# ===========================================================================
test_that("summary.factorial_anova returns correct class", {
  result <- factorial_anova(survey_data, dv = life_satisfaction,
                             between = c(gender, education))
  s <- summary(result)
  expect_summary_class(s, "summary.factorial_anova")
  expect_true(all(c("between_subjects", "descriptives", "levene_test") %in% names(s$show)))
})

test_that("print.summary.factorial_anova produces output", {
  result <- factorial_anova(survey_data, dv = life_satisfaction,
                             between = c(gender, education))
  s <- summary(result)
  expect_summary_prints(s, "Factorial ANOVA")
})

test_that("summary.factorial_anova: between_subjects toggle works", {
  result <- factorial_anova(survey_data, dv = life_satisfaction,
                             between = c(gender, education))

  out_on <- capture.output(print(summary(result, between_subjects = TRUE)))
  expect_true(any(grepl("Between-Subjects", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, between_subjects = FALSE)))
  expect_false(any(grepl("Between-Subjects", out_off, fixed = TRUE)))
})

test_that("summary.factorial_anova: descriptives toggle works", {
  result <- factorial_anova(survey_data, dv = life_satisfaction,
                             between = c(gender, education))

  out_on <- capture.output(print(summary(result, descriptives = TRUE)))
  expect_true(any(grepl("Descriptive", out_on)))

  out_off <- capture.output(print(summary(result, descriptives = FALSE)))
  expect_false(any(grepl("Descriptive Statistics", out_off, fixed = TRUE)))
})

test_that("summary.factorial_anova: levene_test toggle works", {
  result <- factorial_anova(survey_data, dv = life_satisfaction,
                             between = c(gender, education))

  out_on <- capture.output(print(summary(result, levene_test = TRUE)))
  expect_true(any(grepl("Levene", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, levene_test = FALSE)))
  expect_false(any(grepl("Levene", out_off, fixed = TRUE)))
})

test_that("summary.factorial_anova: all FALSE shows minimal output", {
  result <- factorial_anova(survey_data, dv = life_satisfaction,
                             between = c(gender, education))
  out_all <- capture.output(print(summary(result, between_subjects = FALSE,
                                           descriptives = FALSE,
                                           levene_test = FALSE)))
  out_full <- capture.output(print(summary(result)))
  expect_true(length(out_all) < length(out_full))
})

test_that("summary.factorial_anova preserves original data", {
  result <- factorial_anova(survey_data, dv = life_satisfaction,
                             between = c(gender, education))
  s <- summary(result)
  expect_equal(s$call_info$dv, result$call_info$dv)
  expect_equal(s$call_info$factors, result$call_info$factors)
  expect_equal(nrow(s$anova_table), nrow(result$anova_table))
})

test_that("compact print.factorial_anova works", {
  result <- factorial_anova(survey_data, dv = life_satisfaction,
                             between = c(gender, education))
  output <- expect_compact_print(result, "Factorial ANOVA", max_lines = 20)
  expect_true(any(grepl("eta2p", output, fixed = TRUE)))
})

test_that("compact print.factorial_anova: weighted", {
  result <- factorial_anova(survey_data, dv = life_satisfaction,
                             between = c(gender, education),
                             weights = sampling_weight)
  output <- expect_compact_print(result, "Factorial ANOVA", max_lines = 20)
  expect_true(any(grepl("Weighted", output, fixed = TRUE)))
})

# ===========================================================================
# 12. ancova()
# ===========================================================================
test_that("summary.ancova returns correct class", {
  result <- ancova(survey_data, dv = income, between = c(education),
                    covariate = c(age))
  s <- summary(result)
  expect_summary_class(s, "summary.ancova")
  expect_true(all(c("between_subjects", "parameter_estimates",
                     "marginal_means", "levene_test") %in% names(s$show)))
})

test_that("print.summary.ancova produces output", {
  result <- ancova(survey_data, dv = income, between = c(education),
                    covariate = c(age))
  s <- summary(result)
  expect_summary_prints(s, "ANCOVA")
})

test_that("summary.ancova: between_subjects toggle works", {
  result <- ancova(survey_data, dv = income, between = c(education),
                    covariate = c(age))

  out_on <- capture.output(print(summary(result, between_subjects = TRUE)))
  expect_true(any(grepl("Between-Subjects", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, between_subjects = FALSE)))
  expect_false(any(grepl("Between-Subjects", out_off, fixed = TRUE)))
})

test_that("summary.ancova: parameter_estimates toggle works", {
  result <- ancova(survey_data, dv = income, between = c(education),
                    covariate = c(age))

  out_on <- capture.output(print(summary(result, parameter_estimates = TRUE)))
  expect_true(any(grepl("Parameter", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, parameter_estimates = FALSE)))
  expect_false(any(grepl("Parameter Estimates", out_off, fixed = TRUE)))
})

test_that("summary.ancova: marginal_means toggle works", {
  result <- ancova(survey_data, dv = income, between = c(education),
                    covariate = c(age))

  out_on <- capture.output(print(summary(result, marginal_means = TRUE)))
  expect_true(any(grepl("Marginal", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, marginal_means = FALSE)))
  expect_false(any(grepl("Estimated Marginal Means", out_off, fixed = TRUE)))
})

test_that("summary.ancova: levene_test toggle works", {
  result <- ancova(survey_data, dv = income, between = c(education),
                    covariate = c(age))

  out_on <- capture.output(print(summary(result, levene_test = TRUE)))
  expect_true(any(grepl("Levene", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, levene_test = FALSE)))
  expect_false(any(grepl("Levene", out_off, fixed = TRUE)))
})

test_that("summary.ancova: all FALSE shows minimal output", {
  result <- ancova(survey_data, dv = income, between = c(education),
                    covariate = c(age))
  out_all <- capture.output(print(summary(result, between_subjects = FALSE,
                                           parameter_estimates = FALSE,
                                           marginal_means = FALSE,
                                           levene_test = FALSE)))
  out_full <- capture.output(print(summary(result)))
  expect_true(length(out_all) < length(out_full))
})

test_that("summary.ancova preserves original data", {
  result <- ancova(survey_data, dv = income, between = c(education),
                    covariate = c(age))
  s <- summary(result)
  expect_equal(s$call_info$dv, result$call_info$dv)
  expect_equal(s$call_info$factors, result$call_info$factors)
  expect_equal(s$call_info$covariates, result$call_info$covariates)
  expect_equal(nrow(s$anova_table), nrow(result$anova_table))
})

test_that("compact print.ancova works", {
  result <- ancova(survey_data, dv = income, between = c(education),
                    covariate = c(age))
  output <- expect_compact_print(result, "ANCOVA", max_lines = 20)
  expect_true(any(grepl("eta2p", output, fixed = TRUE)))
})

test_that("compact print.ancova: weighted", {
  result <- ancova(survey_data, dv = income, between = c(education),
                    covariate = c(age), weights = sampling_weight)
  output <- expect_compact_print(result, "ANCOVA", max_lines = 20)
  expect_true(any(grepl("Weighted", output, fixed = TRUE)))
})

# ===========================================================================
# 13. efa()
# ===========================================================================
test_that("summary.efa returns correct class", {
  result <- efa(survey_data, trust_government, trust_media, trust_science,
                life_satisfaction)
  s <- summary(result)
  expect_summary_class(s, "summary.efa")
  expect_true(all(c("kmo_bartlett", "communalities", "variance_explained",
                     "unrotated_matrix", "rotated_matrix", "pattern_matrix",
                     "structure_matrix", "factor_correlations") %in% names(s$show)))
})

test_that("print.summary.efa produces output", {
  result <- efa(survey_data, trust_government, trust_media, trust_science,
                life_satisfaction)
  s <- summary(result)
  expect_summary_prints(s, "Exploratory Factor Analysis")
})

test_that("summary.efa: kmo_bartlett toggle works", {
  result <- efa(survey_data, trust_government, trust_media, trust_science,
                life_satisfaction)

  out_on <- capture.output(print(summary(result, kmo_bartlett = TRUE)))
  expect_true(any(grepl("KMO", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, kmo_bartlett = FALSE)))
  expect_false(any(grepl("KMO and Bartlett", out_off, fixed = TRUE)))
})

test_that("summary.efa: communalities toggle works", {
  result <- efa(survey_data, trust_government, trust_media, trust_science,
                life_satisfaction)

  out_on <- capture.output(print(summary(result, communalities = TRUE)))
  expect_true(any(grepl("Communalit", out_on)))

  out_off <- capture.output(print(summary(result, communalities = FALSE)))
  expect_false(any(grepl("Communalities", out_off, fixed = TRUE)))
})

test_that("summary.efa: variance_explained toggle works", {
  result <- efa(survey_data, trust_government, trust_media, trust_science,
                life_satisfaction)

  out_on <- capture.output(print(summary(result, variance_explained = TRUE)))
  expect_true(any(grepl("Variance Explained", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, variance_explained = FALSE)))
  expect_false(any(grepl("Total Variance Explained", out_off, fixed = TRUE)))
})

test_that("summary.efa: unrotated_matrix toggle works", {
  result <- efa(survey_data, trust_government, trust_media, trust_science,
                life_satisfaction)

  out_on <- capture.output(print(summary(result, unrotated_matrix = TRUE)))
  expect_true(any(grepl("unrotated", out_on, ignore.case = TRUE)) ||
              any(grepl("Component Matrix", out_on, fixed = TRUE)) ||
              any(grepl("Factor Matrix", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, unrotated_matrix = FALSE)))
  expect_false(any(grepl("(unrotated)", out_off, fixed = TRUE)))
})

test_that("summary.efa: rotated_matrix toggle works (varimax)", {
  result <- efa(survey_data, trust_government, trust_media, trust_science,
                life_satisfaction, rotation = "varimax")

  out_on <- capture.output(print(summary(result, rotated_matrix = TRUE)))
  expect_true(any(grepl("Rotated", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, rotated_matrix = FALSE)))
  expect_false(any(grepl("Rotated Component Matrix", out_off, fixed = TRUE)) &&
               !any(grepl("Rotated Factor Matrix", out_off, fixed = TRUE)))
})

test_that("summary.efa: all FALSE shows minimal output", {
  result <- efa(survey_data, trust_government, trust_media, trust_science,
                life_satisfaction)
  out_all <- capture.output(print(summary(result, kmo_bartlett = FALSE,
                                           communalities = FALSE,
                                           variance_explained = FALSE,
                                           unrotated_matrix = FALSE,
                                           rotated_matrix = FALSE,
                                           pattern_matrix = FALSE,
                                           structure_matrix = FALSE,
                                           factor_correlations = FALSE)))
  out_full <- capture.output(print(summary(result)))
  expect_true(length(out_all) < length(out_full))
})

test_that("summary.efa preserves original data", {
  result <- efa(survey_data, trust_government, trust_media, trust_science,
                life_satisfaction)
  s <- summary(result)
  expect_equal(s$variables, result$variables)
  expect_equal(s$n_factors, result$n_factors)
  expect_equal(s$kmo$overall, result$kmo$overall)
})

test_that("compact print.efa works", {
  result <- efa(survey_data, trust_government, trust_media, trust_science,
                life_satisfaction)
  output <- expect_compact_print(result, "Exploratory Factor Analysis")
  expect_true(any(grepl("KMO", output, fixed = TRUE)))
})

test_that("compact print.efa: weighted", {
  result <- efa(survey_data, trust_government, trust_media, trust_science,
                life_satisfaction, weights = sampling_weight)
  output <- expect_compact_print(result, "Exploratory Factor Analysis")
  expect_true(any(grepl("Weighted", output, fixed = TRUE)))
})
