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

# ===========================================================================
# 14. kruskal_wallis()
# ===========================================================================
test_that("summary.kruskal_wallis returns correct class", {
  result <- kruskal_wallis(survey_data, life_satisfaction, group = education)
  s <- summary(result)
  expect_summary_class(s, "summary.kruskal_wallis")
  expect_true(all(c("ranks", "results") %in% names(s$show)))
})

test_that("print.summary.kruskal_wallis produces output", {
  result <- kruskal_wallis(survey_data, life_satisfaction, group = education)
  s <- summary(result)
  output <- expect_summary_prints(s, "Kruskal-Wallis")
  expect_true(any(grepl("Ranks", output, fixed = TRUE)))
  expect_true(any(grepl("Mean Rank", output, fixed = TRUE)))
  expect_true(any(grepl("Kruskal-Wallis H", output, fixed = TRUE)))
  expect_true(any(grepl("Epsilon-squared", output, fixed = TRUE)))
})

test_that("summary.kruskal_wallis: ranks toggle works", {
  result <- kruskal_wallis(survey_data, life_satisfaction, group = education)

  out_on <- capture.output(print(summary(result, ranks = TRUE)))
  expect_true(any(grepl("Ranks:", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, ranks = FALSE)))
  expect_false(any(grepl("Ranks:", out_off, fixed = TRUE)))
})

test_that("summary.kruskal_wallis: results toggle works", {
  result <- kruskal_wallis(survey_data, life_satisfaction, group = education)

  out_on <- capture.output(print(summary(result, results = TRUE)))
  expect_true(any(grepl("Test Statistics", out_on, fixed = TRUE)))
  expect_true(any(grepl("Epsilon-squared", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, results = FALSE)))
  expect_false(any(grepl("Test Statistics", out_off, fixed = TRUE)))
  expect_false(any(grepl("Epsilon-squared", out_off, fixed = TRUE)))
})

test_that("summary.kruskal_wallis: all FALSE shows minimal output", {
  result <- kruskal_wallis(survey_data, life_satisfaction, group = education)
  s <- summary(result, ranks = FALSE, results = FALSE)
  output <- capture.output(print(s))
  expect_true(any(grepl("Kruskal-Wallis", output)))
  expect_false(any(grepl("Ranks:", output, fixed = TRUE)))
  expect_false(any(grepl("Test Statistics", output, fixed = TRUE)))
})

test_that("summary.kruskal_wallis preserves original data", {
  result <- kruskal_wallis(survey_data, life_satisfaction, group = education)
  s <- summary(result)
  expect_equal(s$group, result$group)
  expect_equal(s$variables, result$variables)
  expect_equal(nrow(s$results), nrow(result$results))
})

test_that("compact print.kruskal_wallis works", {
  result <- kruskal_wallis(survey_data, life_satisfaction, group = education)
  output <- expect_compact_print(result, "Kruskal-Wallis Test")
  expect_true(any(grepl("H(", output, fixed = TRUE)))
  expect_true(any(grepl("eps2 = ", output, fixed = TRUE)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("compact print.kruskal_wallis: weighted", {
  result <- kruskal_wallis(survey_data, life_satisfaction, group = education,
                           weights = sampling_weight)
  output <- expect_compact_print(result, "Kruskal-Wallis")
  expect_true(any(grepl("[Weighted]", output, fixed = TRUE)))
})

test_that("summary.kruskal_wallis: weighted shows note", {
  result <- kruskal_wallis(survey_data, life_satisfaction, group = education,
                           weights = sampling_weight)
  output <- expect_summary_prints(summary(result), "Weighted")
  expect_true(any(grepl("frequency-weighted ranks", output, fixed = TRUE)))
})

test_that("summary.kruskal_wallis: grouped produces output", {
  result <- survey_data %>% group_by(region) %>%
    kruskal_wallis(life_satisfaction, group = education)
  output <- expect_summary_prints(summary(result), "Kruskal-Wallis")
  expect_true(any(grepl("Group", output)))
})

# ===========================================================================
# 15. wilcoxon_test()
# ===========================================================================
test_that("summary.wilcoxon_test returns correct class", {
  result <- wilcoxon_test(survey_data, x = trust_government, y = trust_media)
  s <- summary(result)
  expect_summary_class(s, "summary.wilcoxon_test")
  expect_true(all(c("ranks", "results") %in% names(s$show)))
})

test_that("print.summary.wilcoxon_test produces output", {
  result <- wilcoxon_test(survey_data, x = trust_government, y = trust_media)
  s <- summary(result)
  output <- expect_summary_prints(s, "Wilcoxon")
  expect_true(any(grepl("Ranks", output, fixed = TRUE)))
  expect_true(any(grepl("Negative Ranks", output, fixed = TRUE)))
  expect_true(any(grepl("Positive Ranks", output, fixed = TRUE)))
  expect_true(any(grepl("Effect r", output, fixed = TRUE)))
})

test_that("summary.wilcoxon_test: ranks toggle works", {
  result <- wilcoxon_test(survey_data, x = trust_government, y = trust_media)

  out_on <- capture.output(print(summary(result, ranks = TRUE)))
  expect_true(any(grepl("Negative Ranks", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, ranks = FALSE)))
  expect_false(any(grepl("Negative Ranks", out_off, fixed = TRUE)))
})

test_that("summary.wilcoxon_test: results toggle works", {
  result <- wilcoxon_test(survey_data, x = trust_government, y = trust_media)

  out_on <- capture.output(print(summary(result, results = TRUE)))
  expect_true(any(grepl("Test Statistics", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, results = FALSE)))
  expect_false(any(grepl("Test Statistics", out_off, fixed = TRUE)))
})

test_that("summary.wilcoxon_test: all FALSE shows minimal output", {
  result <- wilcoxon_test(survey_data, x = trust_government, y = trust_media)
  s <- summary(result, ranks = FALSE, results = FALSE)
  output <- capture.output(print(s))
  expect_true(any(grepl("Wilcoxon", output)))
  expect_false(any(grepl("Negative Ranks", output, fixed = TRUE)))
  expect_false(any(grepl("Test Statistics", output, fixed = TRUE)))
})

test_that("summary.wilcoxon_test preserves original data", {
  result <- wilcoxon_test(survey_data, x = trust_government, y = trust_media)
  s <- summary(result)
  expect_equal(s$x_name, result$x_name)
  expect_equal(s$y_name, result$y_name)
  expect_equal(nrow(s$results), nrow(result$results))
})

test_that("compact print.wilcoxon_test works", {
  result <- wilcoxon_test(survey_data, x = trust_government, y = trust_media)
  output <- expect_compact_print(result, "Wilcoxon Signed-Rank Test")
  expect_true(any(grepl("Z = ", output, fixed = TRUE)))
  expect_true(any(grepl("r = ", output, fixed = TRUE)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("compact print.wilcoxon_test: weighted", {
  result <- wilcoxon_test(survey_data, x = trust_government, y = trust_media,
                          weights = sampling_weight)
  output <- expect_compact_print(result, "Wilcoxon")
  expect_true(any(grepl("[Weighted]", output, fixed = TRUE)))
})

test_that("summary.wilcoxon_test: grouped produces output", {
  result <- survey_data %>% group_by(region) %>%
    wilcoxon_test(x = trust_government, y = trust_media)
  output <- expect_summary_prints(summary(result), "Wilcoxon")
  expect_true(any(grepl("Group", output)))
})

# ===========================================================================
# 16. friedman_test()
# ===========================================================================
test_that("summary.friedman_test returns correct class", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science)
  s <- summary(result)
  expect_summary_class(s, "summary.friedman_test")
  expect_true(all(c("ranks", "results") %in% names(s$show)))
})

test_that("print.summary.friedman_test produces output", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science)
  s <- summary(result)
  output <- expect_summary_prints(s, "Friedman")
  expect_true(any(grepl("Ranks", output, fixed = TRUE)))
  expect_true(any(grepl("Mean Rank", output, fixed = TRUE)))
  expect_true(any(grepl("Chi-Square", output, fixed = TRUE)))
  expect_true(any(grepl("Kendall's W", output, fixed = TRUE)))
})

test_that("summary.friedman_test: ranks toggle works", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science)

  out_on <- capture.output(print(summary(result, ranks = TRUE)))
  expect_true(any(grepl("Mean Rank", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, ranks = FALSE)))
  expect_false(any(grepl("Mean Rank", out_off, fixed = TRUE)))
})

test_that("summary.friedman_test: results toggle works", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science)

  out_on <- capture.output(print(summary(result, results = TRUE)))
  expect_true(any(grepl("Test Statistics", out_on, fixed = TRUE)))
  expect_true(any(grepl("Kendall's W", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, results = FALSE)))
  expect_false(any(grepl("Test Statistics", out_off, fixed = TRUE)))
  expect_false(any(grepl("Kendall's W", out_off, fixed = TRUE)))
})

test_that("summary.friedman_test: all FALSE shows minimal output", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science)
  s <- summary(result, ranks = FALSE, results = FALSE)
  output <- capture.output(print(s))
  expect_true(any(grepl("Friedman", output)))
  expect_false(any(grepl("Mean Rank", output, fixed = TRUE)))
  expect_false(any(grepl("Test Statistics", output, fixed = TRUE)))
})

test_that("summary.friedman_test preserves original data", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science)
  s <- summary(result)
  expect_equal(s$variables, result$variables)
  expect_equal(nrow(s$results), nrow(result$results))
})

test_that("compact print.friedman_test works", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science)
  output <- expect_compact_print(result, "Friedman Test")
  expect_true(any(grepl("chi2(", output, fixed = TRUE)))
  expect_true(any(grepl("W = ", output, fixed = TRUE)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("compact print.friedman_test: weighted", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science, weights = sampling_weight)
  output <- expect_compact_print(result, "Friedman")
  expect_true(any(grepl("[Weighted]", output, fixed = TRUE)))
})

test_that("summary.friedman_test: grouped produces output", {
  result <- survey_data %>% group_by(region) %>%
    friedman_test(trust_government, trust_media, trust_science)
  output <- expect_summary_prints(summary(result), "Friedman")
  expect_true(any(grepl("Group", output)))
})

# ===========================================================================
# 17. binomial_test()
# ===========================================================================
test_that("summary.binomial_test returns correct class", {
  result <- binomial_test(survey_data, gender, p = 0.50)
  s <- summary(result)
  expect_summary_class(s, "summary.binomial_test")
  expect_true(all(c("categories", "results") %in% names(s$show)))
})

test_that("print.summary.binomial_test produces output", {
  result <- binomial_test(survey_data, gender, p = 0.50)
  s <- summary(result)
  output <- expect_summary_prints(s, "Binomial")
  expect_true(any(grepl("Categories", output, fixed = TRUE)))
  expect_true(any(grepl("Observed Prop.", output, fixed = TRUE)))
  expect_true(any(grepl("Test Prop.", output, fixed = TRUE)))
  expect_true(any(grepl("CI lower", output, fixed = TRUE)))
})

test_that("summary.binomial_test: categories toggle works", {
  result <- binomial_test(survey_data, gender, p = 0.50)

  out_on <- capture.output(print(summary(result, categories = TRUE)))
  expect_true(any(grepl("Categories:", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, categories = FALSE)))
  expect_false(any(grepl("Categories:", out_off, fixed = TRUE)))
})

test_that("summary.binomial_test: results toggle works", {
  result <- binomial_test(survey_data, gender, p = 0.50)

  out_on <- capture.output(print(summary(result, results = TRUE)))
  expect_true(any(grepl("Test Statistics", out_on, fixed = TRUE)))
  expect_true(any(grepl("CI lower", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, results = FALSE)))
  expect_false(any(grepl("Test Statistics", out_off, fixed = TRUE)))
  expect_false(any(grepl("CI lower", out_off, fixed = TRUE)))
})

test_that("summary.binomial_test: all FALSE shows minimal output", {
  result <- binomial_test(survey_data, gender, p = 0.50)
  s <- summary(result, categories = FALSE, results = FALSE)
  output <- capture.output(print(s))
  expect_true(any(grepl("Binomial", output)))
  expect_false(any(grepl("Categories:", output, fixed = TRUE)))
  expect_false(any(grepl("Test Statistics", output, fixed = TRUE)))
})

test_that("summary.binomial_test preserves original data", {
  result <- binomial_test(survey_data, gender, p = 0.50)
  s <- summary(result)
  expect_equal(s$p, result$p)
  expect_equal(s$variables, result$variables)
  expect_equal(nrow(s$results), nrow(result$results))
})

test_that("compact print.binomial_test works", {
  result <- binomial_test(survey_data, gender, p = 0.50)
  output <- expect_compact_print(result, "Binomial Test")
  expect_true(any(grepl("prop = ", output, fixed = TRUE)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("compact print.binomial_test: weighted", {
  result <- binomial_test(survey_data, gender, p = 0.50,
                          weights = sampling_weight)
  output <- expect_compact_print(result, "Binomial")
  expect_true(any(grepl("[Weighted]", output, fixed = TRUE)))
})

test_that("summary.binomial_test: grouped produces output", {
  result <- survey_data %>% group_by(region) %>%
    binomial_test(gender, p = 0.50)
  output <- expect_summary_prints(summary(result), "Binomial")
  expect_true(any(grepl("Group", output)))
})

# ===========================================================================
# 18. fisher_test()
# ===========================================================================
test_that("summary.fisher_test returns correct class", {
  result <- fisher_test(survey_data, row = gender, col = region)
  s <- summary(result)
  expect_summary_class(s, "summary.fisher_test")
  expect_true(all(c("contingency_table", "results") %in% names(s$show)))
})

test_that("print.summary.fisher_test produces output", {
  result <- fisher_test(survey_data, row = gender, col = region)
  s <- summary(result)
  output <- expect_summary_prints(s, "Fisher")
  expect_true(any(grepl("Contingency Table", output, fixed = TRUE)))
  expect_true(any(grepl("Test Results:", output, fixed = TRUE)))
})

test_that("summary.fisher_test: contingency_table toggle works", {
  result <- fisher_test(survey_data, row = gender, col = region)

  out_on <- capture.output(print(summary(result, contingency_table = TRUE)))
  expect_true(any(grepl("Contingency Table", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, contingency_table = FALSE)))
  expect_false(any(grepl("Contingency Table", out_off, fixed = TRUE)))
})

test_that("summary.fisher_test: results toggle works", {
  result <- fisher_test(survey_data, row = gender, col = region)

  out_on <- capture.output(print(summary(result, results = TRUE)))
  expect_true(any(grepl("Test Results:", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, results = FALSE)))
  expect_false(any(grepl("Test Results:", out_off, fixed = TRUE)))
})

test_that("summary.fisher_test: all FALSE shows minimal output", {
  result <- fisher_test(survey_data, row = gender, col = region)
  s <- summary(result, contingency_table = FALSE, results = FALSE)
  output <- capture.output(print(s))
  expect_true(any(grepl("Fisher", output)))
  expect_false(any(grepl("Contingency Table", output, fixed = TRUE)))
  expect_false(any(grepl("Test Results:", output, fixed = TRUE)))
})

test_that("summary.fisher_test preserves original data", {
  result <- fisher_test(survey_data, row = gender, col = region)
  s <- summary(result)
  expect_equal(s$row_var, result$row_var)
  expect_equal(s$col_var, result$col_var)
  expect_equal(s$p_value, result$p_value)
  expect_equal(s$n, result$n)
})

test_that("compact print.fisher_test works", {
  result <- fisher_test(survey_data, row = gender, col = region)
  output <- expect_compact_print(result, "Fisher's Exact Test")
  expect_true(any(grepl("p ", output, fixed = TRUE)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("summary.fisher_test: grouped produces output", {
  result <- survey_data %>% group_by(education) %>%
    fisher_test(row = gender, col = region)
  output <- expect_summary_prints(summary(result), "Fisher")
  expect_true(any(grepl("Group", output)))
})

# ===========================================================================
# 19. chisq_gof()
# ===========================================================================
test_that("summary.chisq_gof returns correct class", {
  result <- chisq_gof(survey_data, gender)
  s <- summary(result)
  expect_summary_class(s, "summary.chisq_gof")
  expect_true(all(c("frequency_table", "results") %in% names(s$show)))
})

test_that("print.summary.chisq_gof produces output", {
  result <- chisq_gof(survey_data, gender)
  s <- summary(result)
  output <- expect_summary_prints(s, "Goodness-of-Fit")
  expect_true(any(grepl("Frequency Table", output, fixed = TRUE)))
  expect_true(any(grepl("observed", output, fixed = TRUE)))
  expect_true(any(grepl("expected", output, fixed = TRUE)))
  expect_true(any(grepl("residual", output, fixed = TRUE)))
})

test_that("summary.chisq_gof: frequency_table toggle works", {
  result <- chisq_gof(survey_data, gender)

  out_on <- capture.output(print(summary(result, frequency_table = TRUE)))
  expect_true(any(grepl("Frequency Table", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, frequency_table = FALSE)))
  expect_false(any(grepl("Frequency Table", out_off, fixed = TRUE)))
})

test_that("summary.chisq_gof: results toggle works", {
  result <- chisq_gof(survey_data, gender)

  out_on <- capture.output(print(summary(result, results = TRUE)))
  expect_true(any(grepl("p-value", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, results = FALSE)))
  expect_false(any(grepl("p-value", out_off, fixed = TRUE)))
})

test_that("summary.chisq_gof: all FALSE shows minimal output", {
  result <- chisq_gof(survey_data, gender)
  s <- summary(result, frequency_table = FALSE, results = FALSE)
  output <- capture.output(print(s))
  expect_true(any(grepl("Goodness-of-Fit", output)))
  expect_false(any(grepl("Frequency Table", output, fixed = TRUE)))
  expect_false(any(grepl("p-value", output, fixed = TRUE)))
})

test_that("summary.chisq_gof preserves original data", {
  result <- chisq_gof(survey_data, gender)
  s <- summary(result)
  expect_equal(s$variables, result$variables)
  expect_equal(s$expected, result$expected)
  expect_equal(nrow(s$results), nrow(result$results))
})

test_that("summary.chisq_gof: multi-variable frequency tables", {
  result <- chisq_gof(survey_data, gender, region)
  output <- expect_summary_prints(summary(result), "Goodness-of-Fit")
  expect_true(any(grepl("gender - Frequency Table", output, fixed = TRUE)))
  expect_true(any(grepl("region - Frequency Table", output, fixed = TRUE)))
})

test_that("compact print.chisq_gof works", {
  result <- chisq_gof(survey_data, gender)
  output <- expect_compact_print(result, "Goodness-of-Fit")
  expect_true(any(grepl("chi2(", output, fixed = TRUE)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("compact print.chisq_gof: weighted", {
  result <- chisq_gof(survey_data, gender, weights = sampling_weight)
  output <- expect_compact_print(result, "Goodness-of-Fit")
  expect_true(any(grepl("[Weighted]", output, fixed = TRUE)))
})

test_that("summary.chisq_gof: grouped produces output", {
  result <- survey_data %>% group_by(region) %>% chisq_gof(education)
  output <- expect_summary_prints(summary(result), "Goodness-of-Fit")
  expect_true(any(grepl("Group", output)))
})

# ===========================================================================
# 20. mcnemar_test()
# ===========================================================================
test_that("summary.mcnemar_test returns correct class", {
  test_data <- survey_data %>%
    mutate(
      trust_gov_high = as.integer(trust_government >= 4),
      trust_media_high = as.integer(trust_media >= 4)
    )
  result <- mcnemar_test(test_data, var1 = trust_gov_high,
                         var2 = trust_media_high)
  s <- summary(result)
  expect_summary_class(s, "summary.mcnemar_test")
  expect_true(all(c("contingency_table", "results", "discordant_pairs")
                  %in% names(s$show)))
})

test_that("print.summary.mcnemar_test produces output", {
  test_data <- survey_data %>%
    mutate(
      trust_gov_high = as.integer(trust_government >= 4),
      trust_media_high = as.integer(trust_media >= 4)
    )
  result <- mcnemar_test(test_data, var1 = trust_gov_high,
                         var2 = trust_media_high)
  s <- summary(result)
  output <- expect_summary_prints(s, "McNemar")
  expect_true(any(grepl("Contingency Table", output, fixed = TRUE)))
  expect_true(any(grepl("p (exact)", output, fixed = TRUE)))
  expect_true(any(grepl("Discordant pairs", output, fixed = TRUE)))
})

test_that("summary.mcnemar_test: contingency_table toggle works", {
  test_data <- survey_data %>%
    mutate(
      trust_gov_high = as.integer(trust_government >= 4),
      trust_media_high = as.integer(trust_media >= 4)
    )
  result <- mcnemar_test(test_data, var1 = trust_gov_high,
                         var2 = trust_media_high)

  out_on <- capture.output(print(summary(result, contingency_table = TRUE)))
  expect_true(any(grepl("Contingency Table", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, contingency_table = FALSE)))
  expect_false(any(grepl("Contingency Table", out_off, fixed = TRUE)))
})

test_that("summary.mcnemar_test: results toggle works", {
  test_data <- survey_data %>%
    mutate(
      trust_gov_high = as.integer(trust_government >= 4),
      trust_media_high = as.integer(trust_media >= 4)
    )
  result <- mcnemar_test(test_data, var1 = trust_gov_high,
                         var2 = trust_media_high)

  out_on <- capture.output(print(summary(result, results = TRUE)))
  expect_true(any(grepl("Test Results:", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, results = FALSE)))
  expect_false(any(grepl("Test Results:", out_off, fixed = TRUE)))
})

test_that("summary.mcnemar_test: discordant_pairs toggle works", {
  test_data <- survey_data %>%
    mutate(
      trust_gov_high = as.integer(trust_government >= 4),
      trust_media_high = as.integer(trust_media >= 4)
    )
  result <- mcnemar_test(test_data, var1 = trust_gov_high,
                         var2 = trust_media_high)

  out_on <- capture.output(print(summary(result, discordant_pairs = TRUE)))
  expect_true(any(grepl("Discordant pairs", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, discordant_pairs = FALSE)))
  expect_false(any(grepl("Discordant pairs", out_off, fixed = TRUE)))
})

test_that("summary.mcnemar_test: all FALSE shows minimal output", {
  test_data <- survey_data %>%
    mutate(
      trust_gov_high = as.integer(trust_government >= 4),
      trust_media_high = as.integer(trust_media >= 4)
    )
  result <- mcnemar_test(test_data, var1 = trust_gov_high,
                         var2 = trust_media_high)
  s <- summary(result, contingency_table = FALSE, results = FALSE,
               discordant_pairs = FALSE)
  output <- capture.output(print(s))
  expect_true(any(grepl("McNemar", output)))
  expect_false(any(grepl("Contingency Table", output, fixed = TRUE)))
  expect_false(any(grepl("Discordant pairs", output, fixed = TRUE)))
})

test_that("summary.mcnemar_test preserves original data", {
  test_data <- survey_data %>%
    mutate(
      trust_gov_high = as.integer(trust_government >= 4),
      trust_media_high = as.integer(trust_media >= 4)
    )
  result <- mcnemar_test(test_data, var1 = trust_gov_high,
                         var2 = trust_media_high)
  s <- summary(result)
  expect_equal(s$statistic, result$statistic)
  expect_equal(s$exact_p, result$exact_p)
  expect_equal(s$b, result$b)
  expect_equal(s$c, result$c)
})

test_that("compact print.mcnemar_test works", {
  test_data <- survey_data %>%
    mutate(
      trust_gov_high = as.integer(trust_government >= 4),
      trust_media_high = as.integer(trust_media >= 4)
    )
  result <- mcnemar_test(test_data, var1 = trust_gov_high,
                         var2 = trust_media_high)
  output <- expect_compact_print(result, "McNemar Test")
  expect_true(any(grepl("chi2 = ", output, fixed = TRUE)))
  expect_true(any(grepl("(exact)", output, fixed = TRUE)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("summary.mcnemar_test: grouped produces output", {
  test_data <- survey_data %>%
    mutate(
      trust_gov_high = as.integer(trust_government >= 4),
      trust_media_high = as.integer(trust_media >= 4)
    )
  result <- test_data %>% group_by(region) %>%
    mcnemar_test(var1 = trust_gov_high, var2 = trust_media_high)
  output <- expect_summary_prints(summary(result), "McNemar")
  expect_true(any(grepl("Group", output)))
})

# ===========================================================================
# 21. levene_test()
# ===========================================================================
test_that("summary.levene_test returns correct class", {
  result <- levene_test(survey_data, life_satisfaction, group = education)
  s <- summary(result)
  expect_summary_class(s, "summary.levene_test")
  expect_true(all(c("results", "interpretation", "recommendation")
                  %in% names(s$show)))
})

test_that("print.summary.levene_test produces output", {
  result <- levene_test(survey_data, life_satisfaction, group = education)
  s <- summary(result)
  output <- expect_summary_prints(s, "Levene")
  expect_true(any(grepl("Levene's Test Results", output, fixed = TRUE)))
  expect_true(any(grepl("F_statistic", output, fixed = TRUE)))
  expect_true(any(grepl("Variance", output, ignore.case = TRUE)))
})

test_that("summary.levene_test: results toggle works", {
  result <- levene_test(survey_data, life_satisfaction, group = education)

  out_on <- capture.output(print(summary(result, results = TRUE)))
  expect_true(any(grepl("Levene's Test Results", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, results = FALSE)))
  expect_false(any(grepl("Levene's Test Results", out_off, fixed = TRUE)))
})

test_that("summary.levene_test: interpretation toggle works", {
  result <- levene_test(survey_data, life_satisfaction, group = education)

  out_on <- capture.output(print(summary(result, interpretation = TRUE)))
  expect_true(any(grepl("Interpretation:", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, interpretation = FALSE)))
  expect_false(any(grepl("Interpretation:", out_off, fixed = TRUE)))
})

test_that("summary.levene_test: recommendation toggle works", {
  result <- t_test(survey_data, life_satisfaction, group = gender) %>%
    levene_test()

  out_on <- capture.output(print(summary(result, recommendation = TRUE)))
  expect_true(any(grepl("Recommendation", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, recommendation = FALSE)))
  expect_false(any(grepl("Recommendation", out_off, fixed = TRUE)))
})

test_that("summary.levene_test: all FALSE shows minimal output", {
  result <- levene_test(survey_data, life_satisfaction, group = education)
  s <- summary(result, results = FALSE, interpretation = FALSE,
               recommendation = FALSE)
  output <- capture.output(print(s))
  expect_true(any(grepl("Levene", output)))
  expect_false(any(grepl("Levene's Test Results", output, fixed = TRUE)))
  expect_false(any(grepl("Interpretation:", output, fixed = TRUE)))
})

test_that("summary.levene_test preserves original data", {
  result <- levene_test(survey_data, life_satisfaction, group = education)
  s <- summary(result)
  expect_equal(s$group, result$group)
  expect_equal(s$variables, result$variables)
  expect_equal(s$center, result$center)
  expect_equal(nrow(s$results), nrow(result$results))
})

test_that("compact print.levene_test works", {
  result <- levene_test(survey_data, life_satisfaction, group = education)
  output <- expect_compact_print(result, "Levene's Test")
  expect_true(any(grepl("F(", output, fixed = TRUE)))
  expect_true(any(grepl("variances", output, fixed = TRUE)))
})

test_that("compact print.levene_test: weighted", {
  result <- levene_test(survey_data, life_satisfaction, group = education,
                        weights = sampling_weight)
  output <- expect_compact_print(result, "Levene")
  expect_true(any(grepl("[Weighted]", output, fixed = TRUE)))
})

test_that("summary.levene_test: grouped produces output", {
  result <- survey_data %>% group_by(region) %>%
    oneway_anova(life_satisfaction, group = education) %>%
    levene_test()
  output <- expect_summary_prints(summary(result), "Levene")
  expect_true(any(grepl("Group", output)))
  expect_true(any(grepl("Recommendation", output, fixed = TRUE)))
})

# ===========================================================================
# 22. tukey_test()
# ===========================================================================
test_that("summary.tukey_test returns correct class", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education) %>%
    tukey_test()
  s <- summary(result)
  expect_summary_class(s, "summary.tukey_test")
  expect_true(all(c("parameters", "comparisons", "interpretation")
                  %in% names(s$show)))
})

test_that("print.summary.tukey_test produces output", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education) %>%
    tukey_test()
  s <- summary(result)
  output <- expect_summary_prints(s, "Tukey HSD Post-Hoc Test Results")
  expect_true(any(grepl("Family-wise", output, ignore.case = TRUE) |
                    grepl("Tukey Results", output, fixed = TRUE)))
  expect_true(any(grepl("Difference", output, fixed = TRUE)))
  expect_true(any(grepl("Lower CI", output, fixed = TRUE)))
  expect_true(any(grepl("Upper CI", output, fixed = TRUE)))
  expect_true(any(grepl("p-value", output, fixed = TRUE)))
  expect_true(any(grepl("Signif. codes", output, fixed = TRUE)))
})

test_that("summary.tukey_test: parameters toggle works", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education) %>%
    tukey_test()

  out_on <- capture.output(print(summary(result, parameters = TRUE)))
  expect_true(any(grepl("Confidence level", out_on, fixed = TRUE)))
  expect_true(any(grepl("Family-wise", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, parameters = FALSE)))
  expect_false(any(grepl("Confidence level", out_off, fixed = TRUE)))
  expect_false(any(grepl("Family-wise", out_off, fixed = TRUE)))
})

test_that("summary.tukey_test: comparisons toggle works", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education) %>%
    tukey_test()

  out_on <- capture.output(print(summary(result, comparisons = TRUE)))
  expect_true(any(grepl("Tukey Results", out_on, fixed = TRUE)))
  expect_true(any(grepl("Difference", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, comparisons = FALSE)))
  expect_false(any(grepl("Tukey Results", out_off, fixed = TRUE)))
  expect_false(any(grepl("Difference", out_off, fixed = TRUE)))
  expect_false(any(grepl("Signif. codes", out_off, fixed = TRUE)))
})

test_that("summary.tukey_test: interpretation toggle works", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education) %>%
    tukey_test()

  out_on <- capture.output(print(summary(result, interpretation = TRUE)))
  expect_true(any(grepl("Interpretation:", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, interpretation = FALSE)))
  expect_false(any(grepl("Interpretation:", out_off, fixed = TRUE)))
})

test_that("summary.tukey_test: all FALSE shows minimal output", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education) %>%
    tukey_test()
  s <- summary(result, parameters = FALSE, comparisons = FALSE,
               interpretation = FALSE)
  output <- capture.output(print(s))
  expect_true(any(grepl("Tukey", output)))
  expect_false(any(grepl("Tukey Results", output, fixed = TRUE)))
  expect_false(any(grepl("Interpretation:", output, fixed = TRUE)))
})

test_that("summary.tukey_test preserves original data", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education) %>%
    tukey_test()
  s <- summary(result)
  expect_equal(s$group, result$group)
  expect_equal(s$variables, result$variables)
  expect_equal(s$conf.level, result$conf.level)
  expect_equal(nrow(s$results), nrow(result$results))
})

test_that("compact print.tukey_test works", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education) %>%
    tukey_test()
  output <- expect_compact_print(result, "Tukey HSD Post-Hoc Test")
  expect_true(any(grepl("comparisons", output, fixed = TRUE)))
  expect_true(any(grepl("significant (p < .05)", output, fixed = TRUE)))
})

test_that("compact print.tukey_test: weighted", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education,
                         weights = sampling_weight) %>%
    tukey_test()
  output <- expect_compact_print(result, "Tukey")
  expect_true(any(grepl("[Weighted]", output, fixed = TRUE)))
})

test_that("summary.tukey_test: grouped produces output", {
  result <- survey_data %>% group_by(region) %>%
    oneway_anova(life_satisfaction, group = education) %>%
    tukey_test()
  output <- expect_summary_prints(summary(result), "Tukey")
  expect_true(any(grepl("Group", output)))
})

test_that("summary.tukey_test: factorial ANOVA post-hoc produces output", {
  result <- factorial_anova(survey_data, dv = life_satisfaction,
                            between = c(gender, education)) %>%
    tukey_test()
  output <- expect_summary_prints(summary(result), "Tukey")
  expect_true(any(grepl("Factor:", output, fixed = TRUE)))
})

# ===========================================================================
# 23. scheffe_test()
# ===========================================================================
test_that("summary.scheffe_test returns correct class", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education) %>%
    scheffe_test()
  s <- summary(result)
  expect_summary_class(s, "summary.scheffe_test")
  expect_true(all(c("parameters", "comparisons", "interpretation")
                  %in% names(s$show)))
})

test_that("print.summary.scheffe_test produces output", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education) %>%
    scheffe_test()
  s <- summary(result)
  output <- expect_summary_prints(s, "Scheffe Post-Hoc Test Results")
  expect_true(any(grepl("Scheffe Results", output, fixed = TRUE)))
  expect_true(any(grepl("Most conservative", output, fixed = TRUE)))
  expect_true(any(grepl("Difference", output, fixed = TRUE)))
  expect_true(any(grepl("Signif. codes", output, fixed = TRUE)))
})

test_that("summary.scheffe_test: parameters toggle works", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education) %>%
    scheffe_test()

  out_on <- capture.output(print(summary(result, parameters = TRUE)))
  expect_true(any(grepl("Confidence level", out_on, fixed = TRUE)))
  expect_true(any(grepl("Most conservative", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, parameters = FALSE)))
  expect_false(any(grepl("Confidence level", out_off, fixed = TRUE)))
  expect_false(any(grepl("Most conservative", out_off, fixed = TRUE)))
})

test_that("summary.scheffe_test: comparisons toggle works", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education) %>%
    scheffe_test()

  out_on <- capture.output(print(summary(result, comparisons = TRUE)))
  expect_true(any(grepl("Scheffe Results", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, comparisons = FALSE)))
  expect_false(any(grepl("Scheffe Results", out_off, fixed = TRUE)))
  expect_false(any(grepl("Signif. codes", out_off, fixed = TRUE)))
})

test_that("summary.scheffe_test: interpretation toggle works", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education) %>%
    scheffe_test()

  out_on <- capture.output(print(summary(result, interpretation = TRUE)))
  expect_true(any(grepl("Interpretation:", out_on, fixed = TRUE)))
  expect_true(any(grepl("wider CIs than Tukey", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, interpretation = FALSE)))
  expect_false(any(grepl("Interpretation:", out_off, fixed = TRUE)))
})

test_that("summary.scheffe_test: all FALSE shows minimal output", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education) %>%
    scheffe_test()
  s <- summary(result, parameters = FALSE, comparisons = FALSE,
               interpretation = FALSE)
  output <- capture.output(print(s))
  expect_true(any(grepl("Scheffe", output)))
  expect_false(any(grepl("Scheffe Results", output, fixed = TRUE)))
  expect_false(any(grepl("Interpretation:", output, fixed = TRUE)))
})

test_that("summary.scheffe_test preserves original data", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education) %>%
    scheffe_test()
  s <- summary(result)
  expect_equal(s$group, result$group)
  expect_equal(s$variables, result$variables)
  expect_equal(s$conf.level, result$conf.level)
  expect_equal(nrow(s$results), nrow(result$results))
})

test_that("compact print.scheffe_test works", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education) %>%
    scheffe_test()
  output <- expect_compact_print(result, "Scheffe Post-Hoc Test")
  expect_true(any(grepl("comparisons", output, fixed = TRUE)))
  expect_true(any(grepl("significant (p < .05)", output, fixed = TRUE)))
})

test_that("compact print.scheffe_test: weighted", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education,
                         weights = sampling_weight) %>%
    scheffe_test()
  output <- expect_compact_print(result, "Scheffe")
  expect_true(any(grepl("[Weighted]", output, fixed = TRUE)))
})

test_that("summary.scheffe_test: grouped produces output", {
  result <- survey_data %>% group_by(region) %>%
    oneway_anova(life_satisfaction, group = education) %>%
    scheffe_test()
  output <- expect_summary_prints(summary(result), "Scheffe")
  expect_true(any(grepl("Group", output)))
})

# ===========================================================================
# 24. dunn_test()
# ===========================================================================
test_that("summary.dunn_test returns correct class", {
  result <- kruskal_wallis(survey_data, life_satisfaction,
                           group = education) %>%
    dunn_test()
  s <- summary(result)
  expect_summary_class(s, "summary.dunn_test")
  expect_true(all(c("comparisons", "interpretation") %in% names(s$show)))
})

test_that("print.summary.dunn_test produces output", {
  result <- kruskal_wallis(survey_data, life_satisfaction,
                           group = education) %>%
    dunn_test()
  s <- summary(result)
  output <- expect_summary_prints(s, "Dunn Post-Hoc Test (Bonferroni) Results")
  expect_true(any(grepl("Group 1", output, fixed = TRUE)))
  expect_true(any(grepl("p (adj)", output, fixed = TRUE)))
  expect_true(any(grepl("P-value adjustment", output, fixed = TRUE)))
  expect_true(any(grepl("Signif. codes", output, fixed = TRUE)))
})

test_that("summary.dunn_test: comparisons toggle works", {
  result <- kruskal_wallis(survey_data, life_satisfaction,
                           group = education) %>%
    dunn_test()

  out_on <- capture.output(print(summary(result, comparisons = TRUE)))
  expect_true(any(grepl("Group 1", out_on, fixed = TRUE)))
  expect_true(any(grepl("p (adj)", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, comparisons = FALSE)))
  expect_false(any(grepl("Group 1", out_off, fixed = TRUE)))
  expect_false(any(grepl("Signif. codes", out_off, fixed = TRUE)))
})

test_that("summary.dunn_test: interpretation toggle works", {
  result <- kruskal_wallis(survey_data, life_satisfaction,
                           group = education) %>%
    dunn_test()

  out_on <- capture.output(print(summary(result, interpretation = TRUE)))
  expect_true(any(grepl("Interpretation:", out_on, fixed = TRUE)))
  expect_true(any(grepl("mean rank", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, interpretation = FALSE)))
  expect_false(any(grepl("Interpretation:", out_off, fixed = TRUE)))
})

test_that("summary.dunn_test: all FALSE shows minimal output", {
  result <- kruskal_wallis(survey_data, life_satisfaction,
                           group = education) %>%
    dunn_test()
  s <- summary(result, comparisons = FALSE, interpretation = FALSE)
  output <- capture.output(print(s))
  expect_true(any(grepl("Dunn", output)))
  expect_false(any(grepl("Group 1", output, fixed = TRUE)))
  expect_false(any(grepl("Interpretation:", output, fixed = TRUE)))
})

test_that("summary.dunn_test preserves original data", {
  result <- kruskal_wallis(survey_data, life_satisfaction,
                           group = education) %>%
    dunn_test()
  s <- summary(result)
  expect_equal(s$group, result$group)
  expect_equal(s$variables, result$variables)
  expect_equal(s$p_adjust_method, result$p_adjust_method)
  expect_equal(nrow(s$comparisons), nrow(result$comparisons))
})

test_that("compact print.dunn_test works", {
  result <- kruskal_wallis(survey_data, life_satisfaction,
                           group = education) %>%
    dunn_test()
  output <- expect_compact_print(result, "Dunn Post-Hoc Test")
  expect_true(any(grepl("comparisons", output, fixed = TRUE)))
  expect_true(any(grepl("significant (p < .05)", output, fixed = TRUE)))
})

test_that("compact print.dunn_test: weighted", {
  result <- kruskal_wallis(survey_data, life_satisfaction, group = education,
                           weights = sampling_weight) %>%
    dunn_test()
  output <- expect_compact_print(result, "Dunn")
  expect_true(any(grepl("[Weighted]", output, fixed = TRUE)))
})

test_that("summary.dunn_test: grouped produces output", {
  result <- survey_data %>% group_by(region) %>%
    kruskal_wallis(life_satisfaction, group = education) %>%
    dunn_test()
  output <- expect_summary_prints(summary(result), "Dunn")
  expect_true(any(grepl("Group", output)))
})

# ===========================================================================
# 25. pairwise_wilcoxon()
# ===========================================================================
test_that("summary.pairwise_wilcoxon returns correct class", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science) %>%
    pairwise_wilcoxon()
  s <- summary(result)
  expect_summary_class(s, "summary.pairwise_wilcoxon")
  expect_true(all(c("comparisons", "interpretation") %in% names(s$show)))
})

test_that("print.summary.pairwise_wilcoxon produces output", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science) %>%
    pairwise_wilcoxon()
  s <- summary(result)
  output <- expect_summary_prints(
    s, "Pairwise Wilcoxon Post-Hoc Test (Bonferroni) Results")
  expect_true(any(grepl("Var 1", output, fixed = TRUE)))
  expect_true(any(grepl("p (adj)", output, fixed = TRUE)))
  expect_true(any(grepl("Number of comparisons", output, fixed = TRUE)))
  expect_true(any(grepl("Signif. codes", output, fixed = TRUE)))
})

test_that("summary.pairwise_wilcoxon: comparisons toggle works", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science) %>%
    pairwise_wilcoxon()

  out_on <- capture.output(print(summary(result, comparisons = TRUE)))
  expect_true(any(grepl("Var 1", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, comparisons = FALSE)))
  expect_false(any(grepl("Var 1", out_off, fixed = TRUE)))
  expect_false(any(grepl("Signif. codes", out_off, fixed = TRUE)))
})

test_that("summary.pairwise_wilcoxon: interpretation toggle works", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science) %>%
    pairwise_wilcoxon()

  out_on <- capture.output(print(summary(result, interpretation = TRUE)))
  expect_true(any(grepl("Interpretation:", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, interpretation = FALSE)))
  expect_false(any(grepl("Interpretation:", out_off, fixed = TRUE)))
})

test_that("summary.pairwise_wilcoxon: all FALSE shows minimal output", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science) %>%
    pairwise_wilcoxon()
  s <- summary(result, comparisons = FALSE, interpretation = FALSE)
  output <- capture.output(print(s))
  expect_true(any(grepl("Pairwise Wilcoxon", output)))
  expect_false(any(grepl("Var 1", output, fixed = TRUE)))
  expect_false(any(grepl("Interpretation:", output, fixed = TRUE)))
})

test_that("summary.pairwise_wilcoxon preserves original data", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science) %>%
    pairwise_wilcoxon()
  s <- summary(result)
  expect_equal(s$variables, result$variables)
  expect_equal(s$p_adjust_method, result$p_adjust_method)
  expect_equal(s$n_comparisons, result$n_comparisons)
  expect_equal(nrow(s$comparisons), nrow(result$comparisons))
})

test_that("compact print.pairwise_wilcoxon works", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science) %>%
    pairwise_wilcoxon()
  output <- expect_compact_print(result, "Pairwise Wilcoxon Post-Hoc Test")
  expect_true(any(grepl("comparisons", output, fixed = TRUE)))
  expect_true(any(grepl("significant (p < .05)", output, fixed = TRUE)))
})

test_that("compact print.pairwise_wilcoxon: weighted", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science, weights = sampling_weight) %>%
    pairwise_wilcoxon()
  output <- expect_compact_print(result, "Pairwise Wilcoxon")
  expect_true(any(grepl("[Weighted]", output, fixed = TRUE)))
})

test_that("summary.pairwise_wilcoxon: grouped produces output", {
  result <- survey_data %>% group_by(region) %>%
    friedman_test(trust_government, trust_media, trust_science) %>%
    pairwise_wilcoxon()
  output <- expect_summary_prints(summary(result), "Pairwise Wilcoxon")
  expect_true(any(grepl("Group", output)))
})

# ===========================================================================
# 26. describe()
# ===========================================================================
test_that("summary.describe returns correct class", {
  result <- describe(survey_data, age, income)
  s <- summary(result)
  expect_summary_class(s, "summary.describe")
  expect_true("statistics" %in% names(s$show))
})

test_that("print.summary.describe produces output", {
  result <- describe(survey_data, age, income)
  s <- summary(result)
  output <- expect_summary_prints(s, "Descriptive Statistics")
  expect_true(any(grepl("Mean", output, fixed = TRUE)))
  expect_true(any(grepl("Median", output, fixed = TRUE)))
  expect_true(any(grepl("SD", output, fixed = TRUE)))
})

test_that("summary.describe: statistics toggle works", {
  result <- describe(survey_data, age, income)

  out_on <- capture.output(print(summary(result, statistics = TRUE)))
  expect_true(any(grepl("Mean", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, statistics = FALSE)))
  expect_true(any(grepl("Descriptive Statistics", out_off, fixed = TRUE)))
  expect_false(any(grepl("Mean", out_off, fixed = TRUE)))
})

test_that("summary.describe preserves original data", {
  result <- describe(survey_data, age, income)
  s <- summary(result)
  expect_equal(s$variables, result$variables)
  expect_equal(s$results, result$results)
  expect_equal(s$stat_selection, result$show)
})

test_that("summary.describe respects digits", {
  result <- describe(survey_data, age)
  out_2 <- capture.output(print(summary(result, digits = 2)))
  expect_true(any(grepl("50.55", out_2, fixed = TRUE)))
})

test_that("print.describe stays tabular (compact by nature)", {
  result <- describe(survey_data, age, income)
  output <- expect_compact_print(result, "Descriptive Statistics")
  expect_true(any(grepl("Mean", output, fixed = TRUE)))
})

test_that("summary.describe: grouped, weighted produces output", {
  result <- survey_data %>% group_by(region) %>%
    describe(age, weights = sampling_weight)
  output <- expect_summary_prints(summary(result), "Weighted")
  expect_true(any(grepl("Group", output)))
})

# ===========================================================================
# 27. frequency()
# ===========================================================================
test_that("summary.frequency returns correct class", {
  result <- frequency(survey_data, gender)
  s <- summary(result)
  expect_summary_class(s, "summary.frequency")
  expect_true(all(c("frequency_table", "summary_stats") %in% names(s$show)))
})

test_that("print.summary.frequency produces output", {
  result <- frequency(survey_data, gender)
  s <- summary(result)
  output <- expect_summary_prints(s, "Frequency Analysis Results")
  expect_true(any(grepl("Value", output, fixed = TRUE)))
  expect_true(any(grepl("Raw %", output, fixed = TRUE)))
  expect_true(any(grepl("Valid %", output, fixed = TRUE)))
  expect_true(any(grepl("Cum. %", output, fixed = TRUE)))
  expect_true(any(grepl("Total", output, fixed = TRUE)))
})

test_that("summary.frequency: frequency_table toggle works", {
  result <- frequency(survey_data, gender)

  out_on <- capture.output(print(summary(result, frequency_table = TRUE)))
  expect_true(any(grepl("Raw %", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, frequency_table = FALSE)))
  expect_false(any(grepl("Raw %", out_off, fixed = TRUE)))
})

test_that("summary.frequency: summary_stats toggle works", {
  result <- frequency(survey_data, gender)

  out_on <- capture.output(print(summary(result, summary_stats = TRUE)))
  expect_true(any(grepl("# total N=", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, summary_stats = FALSE)))
  expect_false(any(grepl("# total N=", out_off, fixed = TRUE)))
})

test_that("summary.frequency: all FALSE shows minimal output", {
  result <- frequency(survey_data, gender)
  s <- summary(result, frequency_table = FALSE, summary_stats = FALSE)
  output <- capture.output(print(s))
  expect_true(any(grepl("Frequency Analysis", output, fixed = TRUE)))
  expect_false(any(grepl("Raw %", output, fixed = TRUE)))
  expect_false(any(grepl("# total N=", output, fixed = TRUE)))
})

test_that("summary.frequency preserves original data", {
  result <- frequency(survey_data, gender)
  s <- summary(result)
  expect_equal(s$variables, result$variables)
  expect_equal(s$results, result$results)
  expect_equal(s$stats, result$stats)
  expect_equal(s$options, result$options)
})

test_that("compact print.frequency works", {
  result <- frequency(survey_data, gender)
  output <- expect_compact_print(result, "Frequency: gender")
  expect_true(any(grepl("categories", output, fixed = TRUE)))
  expect_true(any(grepl("N valid = ", output, fixed = TRUE)))
})

test_that("compact print.frequency: weighted", {
  result <- frequency(survey_data, gender, weights = sampling_weight)
  output <- expect_compact_print(result, "Frequency")
  expect_true(any(grepl("[Weighted]", output, fixed = TRUE)))
})

test_that("summary.frequency: grouped produces output", {
  result <- survey_data %>% group_by(region) %>% frequency(gender)
  output <- expect_summary_prints(summary(result), "Frequency Analysis")
  expect_true(any(grepl("Group", output)))
})

# ===========================================================================
# 28. crosstab()
# ===========================================================================
test_that("summary.crosstab returns correct class", {
  result <- crosstab(survey_data, gender, region)
  s <- summary(result)
  expect_summary_class(s, "summary.crosstab")
  expect_true(all(c("crosstab_table", "percentages") %in% names(s$show)))
})

test_that("print.summary.crosstab produces output", {
  result <- crosstab(survey_data, gender, region)
  s <- summary(result)
  output <- expect_summary_prints(s, "Crosstabulation")
  expect_true(any(grepl("Row variable", output, fixed = TRUE)))
  expect_true(any(grepl("Column variable", output, fixed = TRUE)))
  expect_true(any(grepl("Total", output, fixed = TRUE)))
  expect_true(any(grepl("row %", output, fixed = TRUE)))
})

test_that("summary.crosstab: crosstab_table toggle works", {
  result <- crosstab(survey_data, gender, region)

  out_on <- capture.output(print(summary(result, crosstab_table = TRUE)))
  expect_true(any(grepl("Total", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, crosstab_table = FALSE)))
  expect_false(any(grepl("| Total", out_off, fixed = TRUE)))
})

test_that("summary.crosstab: percentages toggle works", {
  result <- crosstab(survey_data, gender, region)

  out_on <- capture.output(print(summary(result, percentages = TRUE)))
  expect_true(any(grepl("row %", out_on, fixed = TRUE)))

  out_off <- capture.output(print(summary(result, percentages = FALSE)))
  expect_false(any(grepl("row %", out_off, fixed = TRUE)))
})

test_that("summary.crosstab preserves original data", {
  result <- crosstab(survey_data, gender, region)
  s <- summary(result)
  expect_equal(s$row_var, result$row_var)
  expect_equal(s$col_var, result$col_var)
  expect_equal(s$table, result$table)
  expect_equal(s$n_valid, result$n_valid)
})

test_that("compact print.crosstab works", {
  result <- crosstab(survey_data, gender, region)
  output <- expect_compact_print(result, "Crosstab: gender x region")
  expect_true(any(grepl("table, N = ", output, fixed = TRUE)))
  expect_true(any(grepl("chi_square()", output, fixed = TRUE)))
})

test_that("compact print.crosstab: weighted", {
  result <- crosstab(survey_data, gender, region,
                     weights = sampling_weight)
  output <- expect_compact_print(result, "Crosstab")
  expect_true(any(grepl("[Weighted]", output, fixed = TRUE)))
})

test_that("summary.crosstab: grouped produces output", {
  result <- survey_data %>% group_by(education) %>%
    crosstab(gender, region)
  output <- expect_summary_prints(summary(result), "Grouped Crosstabulation")
  expect_true(any(grepl("Group", output)))
})
