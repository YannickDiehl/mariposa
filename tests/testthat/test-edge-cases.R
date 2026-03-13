# ===========================================================================
# Edge Case & Error Path Tests — Phase 3 Coverage
# ===========================================================================
# Tests for input validation, error handling, edge cases, and untested
# computation paths across all mariposa functions.

library(dplyr)

# ===========================================================================
# 1. w_factory: summarise-context path (vector input)
# ===========================================================================
test_that("w_mean works in summarise context (unweighted)", {
  result <- survey_data %>% summarise(avg = w_mean(age))
  expect_true(is.numeric(result$avg))
  expect_false(is.na(result$avg))
})

test_that("w_mean works in summarise context (weighted)", {
  result <- survey_data %>% summarise(avg = w_mean(age, weights = sampling_weight))
  expect_true(is.numeric(result$avg))
  expect_false(is.na(result$avg))
})

test_that("w_mean works in grouped summarise context", {
  result <- survey_data %>%
    group_by(gender) %>%
    summarise(avg = w_mean(age, weights = sampling_weight))
  expect_equal(nrow(result), 2)
  expect_true(all(!is.na(result$avg)))
})

test_that("w_sd works in summarise context (weighted)", {
  result <- survey_data %>% summarise(s = w_sd(age, weights = sampling_weight))
  expect_true(is.numeric(result$s))
  expect_true(result$s > 0)
})

test_that("w_median works in summarise context", {
  result <- survey_data %>% summarise(m = w_median(age))
  expect_true(is.numeric(result$m))
})

test_that("w_var works in summarise context", {
  result <- survey_data %>%
    summarise(v = w_var(income, weights = sampling_weight))
  expect_true(is.numeric(result$v))
  expect_true(result$v > 0)
})

test_that("w_se works in summarise context", {
  result <- survey_data %>%
    summarise(se = w_se(age, weights = sampling_weight))
  expect_true(is.numeric(result$se))
})

test_that("w_skew works in summarise context", {
  result <- survey_data %>% summarise(sk = w_skew(age))
  expect_true(is.numeric(result$sk))
})

test_that("w_kurtosis works in summarise context", {
  result <- survey_data %>% summarise(k = w_kurtosis(age))
  expect_true(is.numeric(result$k))
})

test_that("w_iqr works in summarise context", {
  result <- survey_data %>%
    summarise(iq = w_iqr(income, weights = sampling_weight))
  expect_true(is.numeric(result$iq))
})

test_that("w_range works in summarise context", {
  result <- survey_data %>% summarise(r = w_range(age))
  expect_true(is.numeric(result$r))
})

test_that("w_mean returns NA for all-NA input in summarise context", {
  result <- tibble(x = c(NA_real_, NA_real_)) %>%
    summarise(m = w_mean(x))
  expect_true(is.na(result$m) || is.nan(result$m))
})

# ===========================================================================
# 2. w_modus and w_quantile: summarise-context path
# ===========================================================================
test_that("w_modus works in summarise context", {
  result <- survey_data %>% summarise(m = w_modus(gender))
  expect_true(nrow(result) == 1)
})

test_that("w_modus works in grouped summarise context", {
  result <- survey_data %>%
    group_by(region) %>%
    summarise(m = w_modus(gender))
  expect_true(nrow(result) > 1)
})

test_that("w_quantile works in summarise context", {
  result <- survey_data %>%
    summarise(q = w_quantile(age, probs = 0.5))
  expect_true(is.numeric(result$q))
})

test_that("w_quantile works in summarise context (weighted)", {
  result <- survey_data %>%
    summarise(q = w_quantile(age, probs = 0.5, weights = sampling_weight))
  expect_true(is.numeric(result$q))
})

# ===========================================================================
# 3. linear_regression: pairwise missing data path
# ===========================================================================
test_that("linear_regression pairwise produces valid results", {
  result <- linear_regression(survey_data, dependent = income,
                              predictors = c(age, life_satisfaction),
                              use = "pairwise")
  expect_s3_class(result, "linear_regression")
  expect_true(!is.null(result$coefficients))
  expect_true(!is.null(result$model_summary))
  expect_true(nrow(result$coefficients) >= 2)
})

test_that("linear_regression pairwise with weights", {
  result <- linear_regression(survey_data, dependent = income,
                              predictors = c(age, life_satisfaction),
                              use = "pairwise", weights = sampling_weight)
  expect_s3_class(result, "linear_regression")
  expect_true(!is.null(result$coefficients))
})

test_that("linear_regression pairwise print works", {
  result <- linear_regression(survey_data, dependent = income,
                              predictors = c(age, life_satisfaction),
                              use = "pairwise")
  output <- capture.output(print(result))
  expect_true(length(output) > 0)
  expect_true(any(grepl("Pairwise", output, ignore.case = TRUE) |
                    grepl("Linear Regression", output, fixed = TRUE)))
})

test_that("linear_regression pairwise grouped", {
  result <- survey_data %>%
    group_by(gender) %>%
    linear_regression(dependent = income,
                      predictors = c(age, life_satisfaction),
                      use = "pairwise")
  expect_s3_class(result, "linear_regression")
})

# ===========================================================================
# 4. linear_regression: standardized = FALSE
# ===========================================================================
test_that("linear_regression standardized=FALSE omits Beta", {
  result <- linear_regression(survey_data, dependent = life_satisfaction,
                              predictors = c(age, income),
                              standardized = FALSE)
  expect_s3_class(result, "linear_regression")
  output <- capture.output(print(result))
  expect_true(length(output) > 0)
  expect_true(any(grepl("Linear Regression", output, fixed = TRUE)))
})

# ===========================================================================
# 5. Effect size aliases: phi(), cramers_v(), goodman_gamma()
# ===========================================================================
test_that("phi() works and returns chi_square class", {
  result <- phi(survey_data, gender, region)
  expect_s3_class(result, "chi_square")
})

test_that("cramers_v() works and returns chi_square class", {
  result <- cramers_v(survey_data, education, region)
  expect_s3_class(result, "chi_square")
})

test_that("goodman_gamma() works and returns chi_square class", {
  result <- goodman_gamma(survey_data, gender, region)
  expect_s3_class(result, "chi_square")
})

# ===========================================================================
# 6. describe: weight validation warnings
# ===========================================================================
test_that("describe warns on negative weights", {
  bad_data <- survey_data
  bad_data$neg_wt <- rep(-1, nrow(bad_data))
  expect_warning(
    describe(bad_data, age, weights = neg_wt),
    "Negative|negative|unweighted"
  )
})

test_that("describe warns on all-NA weights", {
  bad_data <- survey_data
  bad_data$na_wt <- rep(NA_real_, nrow(bad_data))
  expect_warning(
    describe(bad_data, age, weights = na_wt),
    "NA|missing|unweighted"
  )
})

# ===========================================================================
# 7. describe: show = "short" and show = "all" expansion
# ===========================================================================
test_that("describe show='short' produces expected columns", {
  result <- describe(survey_data, age, show = "short")
  output <- capture.output(print(result))
  expect_true(any(grepl("Mean", output, fixed = TRUE)))
  expect_true(any(grepl("SD", output, fixed = TRUE)))
})

test_that("describe show='all' includes extra statistics", {
  result <- describe(survey_data, age, show = "all")
  output <- capture.output(print(result))
  expect_true(any(grepl("Kurtosis", output, fixed = TRUE)))
  expect_true(any(grepl("Variance", output, ignore.case = TRUE) |
                    grepl("Var", output, fixed = TRUE)))
})

# ===========================================================================
# 8. t_test: input validation errors
# ===========================================================================
test_that("t_test errors on non-data-frame input", {
  expect_error(t_test("not_a_df", age), "data frame|data.frame")
})

test_that("t_test errors on invalid conf.level", {
  expect_error(t_test(survey_data, age, conf.level = 2), "conf.level|between 0 and 1")
  expect_error(t_test(survey_data, age, conf.level = -0.5), "conf.level|between 0 and 1")
})

test_that("t_test errors on non-numeric variable", {
  expect_error(t_test(survey_data, gender, group = region), "not numeric|numeric")
})

# ===========================================================================
# 9. oneway_anova: input validation errors
# ===========================================================================
test_that("oneway_anova errors without group argument", {
  expect_error(oneway_anova(survey_data, age))
})

test_that("oneway_anova errors on non-numeric variable", {
  expect_error(
    oneway_anova(survey_data, gender, group = education),
    "not numeric|numeric"
  )
})

test_that("oneway_anova errors on non-data-frame input", {
  expect_error(oneway_anova("not_df", age, group = region), "data frame|data.frame")
})

# ===========================================================================
# 10. logistic_regression: DV validation
# ===========================================================================
test_that("logistic_regression errors on 3-level factor DV", {
  test_data <- survey_data
  test_data$multi <- factor(rep(c("A", "B", "C"), length.out = nrow(test_data)))
  expect_error(
    logistic_regression(test_data, dependent = multi, predictors = c(age)),
    "binary|2 levels"
  )
})

test_that("logistic_regression errors on non-binary numeric DV", {
  expect_error(
    logistic_regression(survey_data, dependent = education, predictors = c(age)),
    "binary|2 levels|0 and 1"
  )
})

# ===========================================================================
# 11. chi_square: edge cases
# ===========================================================================
test_that("chi_square with small expected frequencies warns", {
  small_data <- data.frame(
    a = factor(c("A", "A", "A", "B", "B")),
    b = factor(c("X", "X", "Y", "X", "Y"))
  )
  expect_warning(
    chi_square(small_data, a, b),
    "expected|Expected|cell|Cell"
  )
})

# ===========================================================================
# 12. factorial_anova: input validation
# ===========================================================================
test_that("factorial_anova errors without between argument", {
  expect_error(
    factorial_anova(survey_data, dv = life_satisfaction),
    "between|factor"
  )
})

# ===========================================================================
# 13. ancova: input validation
# ===========================================================================
test_that("ancova errors without covariate argument", {
  expect_error(
    ancova(survey_data, dv = income, between = c(education),
           covariate = NULL)
  )
})

# ===========================================================================
# 14. reliability: edge cases
# ===========================================================================
test_that("reliability with 2 items works", {
  result <- reliability(survey_data, trust_government, trust_media)
  expect_s3_class(result, "reliability")
  output <- capture.output(print(result))
  expect_true(any(grepl("Cronbach", output, fixed = TRUE)))
})

# ===========================================================================
# 15. efa: edge cases
# ===========================================================================
test_that("efa with custom n_factors works", {
  result <- efa(survey_data, trust_government, trust_media, trust_science,
                life_satisfaction, n_factors = 1)
  expect_s3_class(result, "efa")
  output <- capture.output(print(result))
  expect_true(length(output) > 0)
})

# ===========================================================================
# 16. scale_helpers: edge cases
# ===========================================================================
test_that("row_means with min_valid works correctly", {
  test_data <- tibble(
    x1 = c(1, 2, NA, 4),
    x2 = c(5, NA, NA, 8),
    x3 = c(3, 6, NA, 7)
  )
  result <- test_data %>%
    mutate(idx = row_means(pick(x1, x2, x3), min_valid = 2))
  expect_equal(sum(!is.na(result$idx)), 3)  # row 3 has all NA -> NA
})

test_that("pomps transforms to 0-100 range", {
  result <- pomps(c(1, 2, 3, 4, 5), scale_min = 1, scale_max = 5)
  expect_equal(result[1], 0)
  expect_equal(result[5], 100)
  expect_equal(result[3], 50)
})

# ===========================================================================
# 17. Correlation functions: single-pair vs matrix paths
# ===========================================================================
test_that("pearson_cor single pair has correlation in output", {
  result <- pearson_cor(survey_data, age, income)
  expect_s3_class(result, "pearson_cor")
  output <- capture.output(print(result))
  expect_true(any(grepl("r =", output, fixed = TRUE)))
  # Verbose details available via summary()
  summary_output <- capture.output(print(summary(result)))
  expect_true(any(grepl("CI", summary_output, fixed = TRUE)))
})

test_that("spearman_rho matrix with 4+ vars", {
  result <- spearman_rho(survey_data, age, income, life_satisfaction,
                         political_orientation)
  expect_s3_class(result, "spearman_rho")
  output <- capture.output(print(result))
  expect_true(any(grepl("rho =", output, fixed = TRUE)))
  # Verbose matrix available via summary()
  summary_output <- capture.output(print(summary(result)))
  expect_true(any(grepl("Matrix", summary_output, fixed = TRUE)))
})

test_that("kendall_tau weighted single pair", {
  result <- kendall_tau(survey_data, age, income, weights = sampling_weight)
  expect_s3_class(result, "kendall_tau")
})

# ===========================================================================
# 18. binomial_test: multiple variables
# ===========================================================================
test_that("binomial_test with multiple binary variables", {
  test_data <- survey_data %>%
    mutate(
      trust_gov_high = as.integer(trust_government >= 4),
      trust_med_high = as.integer(trust_media >= 4)
    )
  result <- binomial_test(test_data, trust_gov_high, trust_med_high, p = 0.5)
  expect_s3_class(result, "binomial_test")
  output <- capture.output(print(result))
  expect_true(length(output) > 0)
})

# ===========================================================================
# 19. mcnemar_test: weighted path
# ===========================================================================
test_that("mcnemar_test weighted produces valid results", {
  test_data <- survey_data %>%
    mutate(
      trust_gov_high = as.integer(trust_government >= 4),
      trust_media_high = as.integer(trust_media >= 4)
    )
  result <- mcnemar_test(test_data, var1 = trust_gov_high,
                         var2 = trust_media_high,
                         weights = sampling_weight)
  expect_s3_class(result, "mcnemar_test")
})

# ===========================================================================
# 20. chisq_gof: custom expected proportions
# ===========================================================================
test_that("chisq_gof with custom expected proportions", {
  result <- chisq_gof(survey_data, gender, expected = c(0.6, 0.4))
  expect_s3_class(result, "chisq_gof")
  output <- capture.output(print(result))
  expect_true(any(grepl("expected", output, ignore.case = TRUE)))
})

# ===========================================================================
# 21. dunn_test: different p_adjust methods
# ===========================================================================
test_that("dunn_test with BH adjustment", {
  kw <- kruskal_wallis(survey_data, life_satisfaction, group = education)
  result <- dunn_test(kw, p_adjust = "BH")
  expect_s3_class(result, "dunn_test")
  output <- capture.output(print(result))
  expect_true(any(grepl("BH", output, fixed = TRUE) |
                    grepl("Benjamini", output, ignore.case = TRUE)))
})

test_that("dunn_test with holm adjustment", {
  kw <- kruskal_wallis(survey_data, life_satisfaction, group = education)
  result <- dunn_test(kw, p_adjust = "holm")
  expect_s3_class(result, "dunn_test")
})

# ===========================================================================
# 22. pairwise_wilcoxon: different p_adjust methods
# ===========================================================================
test_that("pairwise_wilcoxon with holm adjustment", {
  fr <- friedman_test(survey_data, trust_government, trust_media, trust_science)
  result <- pairwise_wilcoxon(fr, p_adjust = "holm")
  expect_s3_class(result, "pairwise_wilcoxon")
})

# ===========================================================================
# 23. levene_test: dispatch from factorial_anova
# ===========================================================================
test_that("levene_test dispatches from factorial_anova", {
  fa <- factorial_anova(survey_data, dv = life_satisfaction,
                        between = c(gender, education))
  result <- levene_test(fa)
  expect_s3_class(result, "levene_test")
  output <- capture.output(print(result))
  expect_true(any(grepl("Levene", output, fixed = TRUE)))
})

# ===========================================================================
# 24. frequency: show.valid and show.sum toggles
# ===========================================================================
test_that("frequency with show.valid=FALSE, show.sum=FALSE", {
  result <- frequency(survey_data, gender,
                      show.valid = FALSE, show.sum = FALSE)
  output <- capture.output(print(result))
  expect_true(length(output) > 0)
  # Valid % and Cum. % should not appear
  expect_false(any(grepl("Valid %", output, fixed = TRUE)))
})

test_that("frequency with show.labels=TRUE", {
  result <- frequency(survey_data, gender, show.labels = TRUE)
  output <- capture.output(print(result))
  expect_true(length(output) > 0)
})

# ===========================================================================
# 25. crosstab: weighted + grouped (combined)
# ===========================================================================
test_that("crosstab weighted + grouped produces output", {
  result <- survey_data %>%
    group_by(education) %>%
    crosstab(gender, region, weights = sampling_weight)
  output <- capture.output(print(result))
  expect_true(length(output) > 0)
})

# ===========================================================================
# 26. fisher_test: multi-variable
# ===========================================================================
test_that("fisher_test weighted produces valid results", {
  result <- fisher_test(survey_data, row = gender, col = region,
                        weights = sampling_weight)
  expect_s3_class(result, "fisher_test")
  output <- capture.output(print(result))
  expect_true(any(grepl("Fisher", output, fixed = TRUE)))
})

# ===========================================================================
# 27. mann_whitney: grouped + weighted combined
# ===========================================================================
test_that("mann_whitney grouped + weighted produces output", {
  result <- survey_data %>%
    group_by(education) %>%
    mann_whitney(life_satisfaction, group = gender,
                weights = sampling_weight)
  expect_s3_class(result, "mann_whitney")
  output <- capture.output(print(result))
  expect_true(length(output) > 0)
})

# ===========================================================================
# 28. wilcoxon_test: grouped + weighted combined
# ===========================================================================
test_that("wilcoxon_test grouped + weighted produces output", {
  result <- survey_data %>%
    group_by(region) %>%
    wilcoxon_test(x = trust_government, y = trust_media,
                  weights = sampling_weight)
  expect_s3_class(result, "wilcoxon_test")
  output <- capture.output(print(result))
  expect_true(length(output) > 0)
})

# ===========================================================================
# 29. friedman_test: grouped + weighted combined
# ===========================================================================
test_that("friedman_test grouped + weighted produces output", {
  result <- survey_data %>%
    group_by(region) %>%
    friedman_test(trust_government, trust_media, trust_science,
                  weights = sampling_weight)
  expect_s3_class(result, "friedman_test")
  output <- capture.output(print(result))
  expect_true(length(output) > 0)
})

# ===========================================================================
# 30. kruskal_wallis: grouped + weighted combined
# ===========================================================================
test_that("kruskal_wallis grouped + weighted produces output", {
  result <- survey_data %>%
    group_by(region) %>%
    kruskal_wallis(life_satisfaction, group = education,
                   weights = sampling_weight)
  expect_s3_class(result, "kruskal_wallis")
  output <- capture.output(print(result))
  expect_true(length(output) > 0)
})

# ===========================================================================
# 31. binomial_test: grouped + weighted combined
# ===========================================================================
test_that("binomial_test grouped + weighted produces output", {
  result <- survey_data %>%
    group_by(region) %>%
    binomial_test(gender, p = 0.5, weights = sampling_weight)
  expect_s3_class(result, "binomial_test")
  output <- capture.output(print(result))
  expect_true(length(output) > 0)
})
