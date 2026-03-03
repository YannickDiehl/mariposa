# ===========================================================================
# Print Method Tests — Coverage for all S3 print methods
# ===========================================================================
# These tests ensure all print methods execute without error across their
# major code paths (ungrouped/grouped, weighted/unweighted, single/multi-var).
# They validate output is produced and contains expected key patterns.

library(dplyr)

# --- Helper ------------------------------------------------------------------
expect_prints <- function(obj, pattern = NULL) {
  output <- capture.output(print(obj))
  expect_true(length(output) > 0)
  if (!is.null(pattern)) {
    expect_true(
      any(grepl(pattern, output, fixed = TRUE)),
      info = paste0("Pattern '", pattern, "' not found in output")
    )
  }
  invisible(output)
}

# ===========================================================================
# 1. describe()
# ===========================================================================
test_that("print.describe: ungrouped, unweighted", {
  result <- describe(survey_data, age, income)
  expect_prints(result, "Descriptive")
})

test_that("print.describe: ungrouped, weighted", {

  result <- describe(survey_data, age, weights = sampling_weight)
  expect_prints(result, "Weighted")
})

test_that("print.describe: grouped, unweighted", {
  result <- survey_data %>% group_by(region) %>% describe(age)
  expect_prints(result, "Descriptive")
})

test_that("print.describe: grouped, weighted", {
  result <- survey_data %>% group_by(region) %>%
    describe(age, weights = sampling_weight)
  expect_prints(result, "Weighted")
})

test_that("print.describe: show = 'all'", {
  result <- describe(survey_data, age, show = "all")
  output <- expect_prints(result, "Descriptive")
  expect_true(any(grepl("Kurtosis", output, fixed = TRUE)))
})

# ===========================================================================
# 2. frequency()
# ===========================================================================
test_that("print.frequency: ungrouped, unweighted", {
  result <- frequency(survey_data, gender)
  expect_prints(result, "Frequency")
})

test_that("print.frequency: ungrouped, weighted", {
  result <- frequency(survey_data, gender, weights = sampling_weight)
  expect_prints(result, "Frequency")
})

test_that("print.frequency: grouped", {
  result <- survey_data %>% group_by(region) %>% frequency(gender)
  expect_prints(result, "Frequency")
})

test_that("print.frequency: multiple variables", {
  result <- frequency(survey_data, gender, education)
  expect_prints(result, "Frequency")
})

test_that("print.frequency: toggled columns", {
  result <- frequency(survey_data, gender, show.labels = FALSE, show.prc = FALSE)
  expect_prints(result, "Frequency")
})

# ===========================================================================
# 3. crosstab()
# ===========================================================================
test_that("print.crosstab: ungrouped, row pct (default)", {
  result <- crosstab(survey_data, gender, region)
  expect_prints(result, "Crosstab")
})

test_that("print.crosstab: ungrouped, weighted", {
  result <- crosstab(survey_data, gender, region, weights = sampling_weight)
  output <- expect_prints(result, "Crosstab")
  expect_true(any(grepl("weighted", output, ignore.case = TRUE)))
})

test_that("print.crosstab: col percentages", {
  result <- crosstab(survey_data, gender, region, percentages = "col")
  expect_prints(result, "Crosstab")
})

test_that("print.crosstab: total percentages", {
  result <- crosstab(survey_data, gender, region, percentages = "total")
  expect_prints(result, "Crosstab")
})

test_that("print.crosstab: all percentages", {
  result <- crosstab(survey_data, gender, region, percentages = "all")
  expect_prints(result, "Crosstab")
})

test_that("print.crosstab: no percentages", {
  result <- crosstab(survey_data, gender, region, percentages = "none")
  expect_prints(result, "Crosstab")
})

test_that("print.crosstab: grouped", {
  result <- survey_data %>% group_by(education) %>%
    crosstab(gender, region)
  expect_prints(result, "Crosstab")
})

# ===========================================================================
# 4. t_test()
# ===========================================================================
test_that("print.t_test: ungrouped, unweighted, two-sample", {
  result <- t_test(survey_data, life_satisfaction, group = gender)
  output <- expect_prints(result, "t-Test")
  expect_true(any(grepl("t(", output, fixed = TRUE)))
})

test_that("print.t_test: ungrouped, weighted, two-sample", {
  result <- t_test(survey_data, life_satisfaction, group = gender,
                   weights = sampling_weight)
  expect_prints(result, "Weighted")
})

test_that("print.t_test: one-sample", {
  result <- t_test(survey_data, life_satisfaction, mu = 5)
  expect_prints(result, "t-Test")
})

test_that("print.t_test: grouped, two-sample", {
  result <- survey_data %>% group_by(region) %>%
    t_test(life_satisfaction, group = gender)
  expect_prints(result, "t-Test")
})

test_that("print.t_test: multiple variables", {
  result <- t_test(survey_data, life_satisfaction, income, group = gender)
  expect_prints(result, "t-Test")
})

# ===========================================================================
# 5. oneway_anova()
# ===========================================================================
test_that("print.oneway_anova: ungrouped, unweighted", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education)
  output <- expect_prints(result, "One-Way ANOVA")
  expect_true(any(grepl("F(", output, fixed = TRUE)))
  expect_true(any(grepl("N =", output, fixed = TRUE)))
})

test_that("print.oneway_anova: ungrouped, weighted", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education,
                         weights = sampling_weight)
  expect_prints(result, "Weighted")
})

test_that("print.oneway_anova: multi-variable", {
  result <- oneway_anova(survey_data, life_satisfaction, income,
                         group = education)
  expect_prints(result, "One-Way ANOVA")
})

test_that("print.oneway_anova: grouped", {
  result <- survey_data %>% group_by(region) %>%
    oneway_anova(life_satisfaction, group = education)
  expect_prints(result, "ANOVA")
})

test_that("print.oneway_anova: grouped, weighted", {
  result <- survey_data %>% group_by(region) %>%
    oneway_anova(life_satisfaction, group = education,
                 weights = sampling_weight)
  expect_prints(result, "ANOVA")
})

# ===========================================================================
# 6. factorial_anova()
# ===========================================================================
test_that("print.factorial_anova: unweighted", {
  result <- factorial_anova(survey_data, dv = life_satisfaction,
                            between = c(gender, education))
  output <- expect_prints(result, "Factorial ANOVA")
  expect_true(any(grepl("eta2p", output, fixed = TRUE)))
})

test_that("print.factorial_anova: weighted", {
  result <- factorial_anova(survey_data, dv = life_satisfaction,
                            between = c(gender, education),
                            weights = sampling_weight)
  expect_prints(result, "Weighted")
})

# ===========================================================================
# 7. ancova()
# ===========================================================================
test_that("print.ancova: one-way, unweighted", {
  result <- ancova(survey_data, dv = income, between = c(education),
                   covariate = c(age))
  output <- expect_prints(result, "ANCOVA")
  expect_true(any(grepl("eta2p", output, fixed = TRUE)))
  expect_true(any(grepl("covariate", output, fixed = TRUE)))
})

test_that("print.ancova: one-way, weighted", {
  result <- ancova(survey_data, dv = income, between = c(education),
                   covariate = c(age), weights = sampling_weight)
  expect_prints(result, "Weighted")
})

test_that("print.ancova: two-way", {
  result <- ancova(survey_data, dv = income,
                   between = c(gender, education), covariate = c(age))
  expect_prints(result, "ANCOVA")
})

# ===========================================================================
# 8. chi_square()
# ===========================================================================
test_that("print.chi_square: ungrouped", {
  result <- chi_square(survey_data, education, region)
  output <- expect_prints(result, "Chi-Squared")
  expect_true(any(grepl("chi2(", output, fixed = TRUE)))
  expect_true(any(grepl("N =", output, fixed = TRUE)))
})

test_that("print.chi_square: ungrouped, weighted", {
  result <- chi_square(survey_data, education, region,
                       weights = sampling_weight)
  expect_prints(result, "Weighted")
})

test_that("print.chi_square: grouped", {
  result <- survey_data %>% group_by(gender) %>%
    chi_square(education, region)
  expect_prints(result, "Chi-Squared")
})

# ===========================================================================
# 9. fisher_test()
# ===========================================================================
test_that("print.fisher_test: ungrouped", {
  result <- fisher_test(survey_data, row = gender, col = region)
  output <- expect_prints(result, "Fisher")
  expect_true(any(grepl("Contingency Table", output, fixed = TRUE)))
})

test_that("print.fisher_test: grouped", {
  result <- survey_data %>% group_by(education) %>%
    fisher_test(row = gender, col = region)
  expect_prints(result, "Fisher")
})

test_that("print.fisher_test: weighted", {
  result <- fisher_test(survey_data, row = gender, col = region,
                        weights = sampling_weight)
  expect_prints(result, "Fisher")
})

# ===========================================================================
# 10. chisq_gof()
# ===========================================================================
test_that("print.chisq_gof: single variable, equal proportions", {
  result <- chisq_gof(survey_data, gender)
  output <- expect_prints(result, "Goodness-of-Fit")
  expect_true(any(grepl("observed", output, fixed = TRUE)))
})

test_that("print.chisq_gof: multiple variables", {
  result <- chisq_gof(survey_data, gender, region)
  expect_prints(result, "Goodness-of-Fit")
})

test_that("print.chisq_gof: grouped", {
  result <- survey_data %>% group_by(region) %>%
    chisq_gof(education)
  expect_prints(result, "Goodness-of-Fit")
})

test_that("print.chisq_gof: weighted", {
  result <- chisq_gof(survey_data, gender, weights = sampling_weight)
  expect_prints(result, "Goodness-of-Fit")
})

# ===========================================================================
# 11. mcnemar_test()
# ===========================================================================
test_that("print.mcnemar_test: ungrouped", {
  test_data <- survey_data %>%
    mutate(
      trust_gov_high = as.integer(trust_government >= 4),
      trust_media_high = as.integer(trust_media >= 4)
    )
  result <- mcnemar_test(test_data, var1 = trust_gov_high,
                         var2 = trust_media_high)
  output <- expect_prints(result, "McNemar")
  expect_true(any(grepl("Contingency Table", output, fixed = TRUE)))
})

test_that("print.mcnemar_test: grouped", {
  test_data <- survey_data %>%
    mutate(
      trust_gov_high = as.integer(trust_government >= 4),
      trust_media_high = as.integer(trust_media >= 4)
    )
  result <- test_data %>% group_by(region) %>%
    mcnemar_test(var1 = trust_gov_high, var2 = trust_media_high)
  expect_prints(result, "McNemar")
})

test_that("print.mcnemar_test: weighted", {
  test_data <- survey_data %>%
    mutate(
      trust_gov_high = as.integer(trust_government >= 4),
      trust_media_high = as.integer(trust_media >= 4)
    )
  result <- mcnemar_test(test_data, var1 = trust_gov_high,
                         var2 = trust_media_high,
                         weights = sampling_weight)
  expect_prints(result, "McNemar")
})

# ===========================================================================
# 12. pearson_cor()
# ===========================================================================
test_that("print.pearson_cor: ungrouped, 2 vars", {
  result <- pearson_cor(survey_data, age, income)
  output <- expect_prints(result, "Pearson Correlation")
  expect_true(any(grepl("r = ", output, fixed = TRUE)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("print.pearson_cor: ungrouped, 3+ vars (compact)", {
  result <- pearson_cor(survey_data, age, income, life_satisfaction)
  output <- expect_prints(result, "Pearson Correlation")
  expect_true(any(grepl("variables", output, fixed = TRUE)))
})

test_that("print.pearson_cor: ungrouped, weighted", {
  result <- pearson_cor(survey_data, age, income, weights = sampling_weight)
  output <- expect_prints(result, "Pearson Correlation")
  expect_true(any(grepl("Weighted", output, fixed = TRUE)))
})

test_that("print.pearson_cor: grouped, 2 vars", {
  result <- survey_data %>% group_by(region) %>%
    pearson_cor(age, income)
  expect_prints(result, "Pearson Correlation")
})

test_that("print.pearson_cor: grouped, 3+ vars (compact)", {
  result <- survey_data %>% group_by(region) %>%
    pearson_cor(age, income, life_satisfaction)
  expect_prints(result, "Pearson Correlation")
})

# ===========================================================================
# 13. spearman_rho()
# ===========================================================================
test_that("print.spearman_rho: ungrouped, 2 vars", {
  result <- spearman_rho(survey_data, age, income)
  output <- expect_prints(result, "Spearman Correlation")
  expect_true(any(grepl("rho = ", output, fixed = TRUE)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("print.spearman_rho: ungrouped, 3+ vars (compact)", {
  result <- spearman_rho(survey_data, age, income, life_satisfaction)
  output <- expect_prints(result, "Spearman Correlation")
  expect_true(any(grepl("variables", output, fixed = TRUE)))
})

test_that("print.spearman_rho: ungrouped, weighted", {
  result <- spearman_rho(survey_data, age, income, weights = sampling_weight)
  output <- expect_prints(result, "Spearman Correlation")
  expect_true(any(grepl("Weighted", output, fixed = TRUE)))
})

test_that("print.spearman_rho: grouped", {
  result <- survey_data %>% group_by(region) %>%
    spearman_rho(age, income)
  expect_prints(result, "Spearman Correlation")
})

# ===========================================================================
# 14. kendall_tau()
# ===========================================================================
test_that("print.kendall_tau: ungrouped, 2 vars", {
  result <- kendall_tau(survey_data, age, income)
  output <- expect_prints(result, "Kendall's Tau")
  expect_true(any(grepl("tau = ", output, fixed = TRUE)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("print.kendall_tau: ungrouped, 3+ vars (compact)", {
  result <- kendall_tau(survey_data, age, income, life_satisfaction)
  output <- expect_prints(result, "Kendall's Tau")
  expect_true(any(grepl("variables", output, fixed = TRUE)))
})

test_that("print.kendall_tau: ungrouped, weighted", {
  result <- kendall_tau(survey_data, age, income, weights = sampling_weight)
  output <- expect_prints(result, "Kendall's Tau")
  expect_true(any(grepl("Weighted", output, fixed = TRUE)))
})

test_that("print.kendall_tau: grouped", {
  result <- survey_data %>% group_by(region) %>%
    kendall_tau(age, income)
  expect_prints(result, "Kendall's Tau")
})


# ===========================================================================
# 15. mann_whitney()
# ===========================================================================
test_that("print.mann_whitney: ungrouped, unweighted", {
  result <- mann_whitney(survey_data, life_satisfaction, group = gender)
  output <- expect_prints(result, "Mann-Whitney")
  expect_true(any(grepl("U =", output, fixed = TRUE)))
})

test_that("print.mann_whitney: ungrouped, weighted", {
  result <- mann_whitney(survey_data, life_satisfaction, group = gender,
                         weights = sampling_weight)
  expect_prints(result, "Mann-Whitney")
})

test_that("print.mann_whitney: multi-variable", {
  result <- mann_whitney(survey_data, life_satisfaction, income,
                         group = gender)
  expect_prints(result, "Mann-Whitney")
})

test_that("print.mann_whitney: grouped", {
  result <- survey_data %>% group_by(education) %>%
    mann_whitney(life_satisfaction, group = gender)
  expect_prints(result, "Mann-Whitney")
})

# ===========================================================================
# 16. kruskal_wallis()
# ===========================================================================
test_that("print.kruskal_wallis: ungrouped, unweighted", {
  result <- kruskal_wallis(survey_data, life_satisfaction, group = education)
  output <- expect_prints(result, "Kruskal-Wallis")
  expect_true(any(grepl("Ranks", output, fixed = TRUE)))
})

test_that("print.kruskal_wallis: ungrouped, weighted", {
  result <- kruskal_wallis(survey_data, life_satisfaction, group = education,
                           weights = sampling_weight)
  expect_prints(result, "Weighted")
})

test_that("print.kruskal_wallis: multi-variable", {
  result <- kruskal_wallis(survey_data, life_satisfaction, trust_government,
                           group = education)
  expect_prints(result, "Kruskal-Wallis")
})

test_that("print.kruskal_wallis: grouped", {
  result <- survey_data %>% group_by(region) %>%
    kruskal_wallis(life_satisfaction, group = education)
  expect_prints(result, "Kruskal-Wallis")
})

# ===========================================================================
# 17. wilcoxon_test()
# ===========================================================================
test_that("print.wilcoxon_test: ungrouped, unweighted", {
  result <- wilcoxon_test(survey_data, x = trust_government, y = trust_media)
  output <- expect_prints(result, "Wilcoxon")
  expect_true(any(grepl("Ranks", output, fixed = TRUE)))
})

test_that("print.wilcoxon_test: ungrouped, weighted", {
  result <- wilcoxon_test(survey_data, x = trust_government, y = trust_media,
                          weights = sampling_weight)
  expect_prints(result, "Weighted")
})

test_that("print.wilcoxon_test: grouped", {
  result <- survey_data %>% group_by(region) %>%
    wilcoxon_test(x = trust_government, y = trust_media)
  expect_prints(result, "Wilcoxon")
})

# ===========================================================================
# 18. friedman_test()
# ===========================================================================
test_that("print.friedman_test: ungrouped, unweighted", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science)
  output <- expect_prints(result, "Friedman")
  expect_true(any(grepl("Ranks", output, fixed = TRUE)))
})

test_that("print.friedman_test: ungrouped, weighted", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science, weights = sampling_weight)
  expect_prints(result, "Weighted")
})

test_that("print.friedman_test: grouped", {
  result <- survey_data %>% group_by(region) %>%
    friedman_test(trust_government, trust_media, trust_science)
  expect_prints(result, "Friedman")
})

# ===========================================================================
# 19. binomial_test()
# ===========================================================================
test_that("print.binomial_test: ungrouped, unweighted", {
  result <- binomial_test(survey_data, gender, p = 0.50)
  output <- expect_prints(result, "Binomial")
  expect_true(any(grepl("Category", output, ignore.case = TRUE) |
                    grepl("Group", output, fixed = TRUE)))
})

test_that("print.binomial_test: ungrouped, weighted", {
  result <- binomial_test(survey_data, gender, p = 0.50,
                          weights = sampling_weight)
  expect_prints(result, "Binomial")
})

test_that("print.binomial_test: grouped", {
  result <- survey_data %>% group_by(region) %>%
    binomial_test(gender, p = 0.50)
  expect_prints(result, "Binomial")
})

# ===========================================================================
# 20. levene_test()
# ===========================================================================
test_that("print.levene_test: ungrouped, unweighted (direct)", {
  result <- levene_test(survey_data, life_satisfaction, group = education)
  output <- expect_prints(result, "Levene")
  expect_true(any(grepl("Variance", output, ignore.case = TRUE)))
})

test_that("print.levene_test: ungrouped, weighted (direct)", {
  result <- levene_test(survey_data, life_satisfaction, group = education,
                        weights = sampling_weight)
  expect_prints(result, "Weighted")
})

test_that("print.levene_test: piped from oneway_anova", {
  result <- oneway_anova(survey_data, life_satisfaction,
                         group = education) %>%
    levene_test()
  expect_prints(result, "Levene")
})

test_that("print.levene_test: multi-variable", {
  result <- levene_test(survey_data, life_satisfaction, income,
                        group = education)
  expect_prints(result, "Levene")
})

test_that("print.levene_test: piped from t_test", {
  result <- t_test(survey_data, life_satisfaction, group = gender) %>%
    levene_test()
  output <- expect_prints(result, "Levene")
  expect_true(any(grepl("Recommendation", output, fixed = TRUE)))
})

# ===========================================================================
# 21. tukey_test()
# ===========================================================================
test_that("print.tukey_test: ungrouped, unweighted", {
  result <- oneway_anova(survey_data, life_satisfaction,
                         group = education) %>%
    tukey_test()
  output <- expect_prints(result, "Tukey")
  expect_true(any(grepl("Family-wise", output, ignore.case = TRUE) |
                    grepl("Tukey Results", output, fixed = TRUE)))
})

test_that("print.tukey_test: ungrouped, weighted", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education,
                         weights = sampling_weight) %>%
    tukey_test()
  expect_prints(result, "Tukey")
})

test_that("print.tukey_test: grouped", {
  result <- survey_data %>% group_by(region) %>%
    oneway_anova(life_satisfaction, group = education) %>%
    tukey_test()
  expect_prints(result, "Tukey")
})

test_that("print.tukey_test: factorial ANOVA post-hoc", {
  result <- factorial_anova(survey_data, dv = life_satisfaction,
                            between = c(gender, education)) %>%
    tukey_test()
  expect_prints(result, "Tukey")
})

# ===========================================================================
# 22. scheffe_test()
# ===========================================================================
test_that("print.scheffe_test: ungrouped, unweighted", {
  result <- oneway_anova(survey_data, life_satisfaction,
                         group = education) %>%
    scheffe_test()
  expect_prints(result, "Scheffe")
})

test_that("print.scheffe_test: ungrouped, weighted", {
  result <- oneway_anova(survey_data, life_satisfaction, group = education,
                         weights = sampling_weight) %>%
    scheffe_test()
  expect_prints(result, "Scheffe")
})

test_that("print.scheffe_test: grouped", {
  result <- survey_data %>% group_by(region) %>%
    oneway_anova(life_satisfaction, group = education) %>%
    scheffe_test()
  expect_prints(result, "Scheffe")
})

test_that("print.scheffe_test: factorial ANOVA post-hoc", {
  result <- factorial_anova(survey_data, dv = life_satisfaction,
                            between = c(gender, education)) %>%
    scheffe_test()
  expect_prints(result, "Scheffe")
})

# ===========================================================================
# 23. dunn_test()
# ===========================================================================
test_that("print.dunn_test: ungrouped, single var", {
  result <- kruskal_wallis(survey_data, life_satisfaction,
                           group = education) %>%
    dunn_test()
  output <- expect_prints(result, "Dunn")
  expect_true(any(grepl("Group 1", output, fixed = TRUE) |
                    grepl("p (adj)", output, fixed = TRUE)))
})

test_that("print.dunn_test: ungrouped, multi-var", {
  result <- kruskal_wallis(survey_data, life_satisfaction, trust_government,
                           group = education) %>%
    dunn_test()
  expect_prints(result, "Dunn")
})

test_that("print.dunn_test: grouped", {
  result <- survey_data %>% group_by(region) %>%
    kruskal_wallis(life_satisfaction, group = education) %>%
    dunn_test()
  expect_prints(result, "Dunn")
})

test_that("print.dunn_test: weighted", {
  result <- kruskal_wallis(survey_data, life_satisfaction, group = education,
                           weights = sampling_weight) %>%
    dunn_test()
  expect_prints(result, "Dunn")
})

# ===========================================================================
# 24. pairwise_wilcoxon()
# ===========================================================================
test_that("print.pairwise_wilcoxon: ungrouped", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science) %>%
    pairwise_wilcoxon()
  expect_prints(result, "Pairwise Wilcoxon")
})

test_that("print.pairwise_wilcoxon: grouped", {
  result <- survey_data %>% group_by(region) %>%
    friedman_test(trust_government, trust_media, trust_science) %>%
    pairwise_wilcoxon()
  expect_prints(result, "Pairwise Wilcoxon")
})

test_that("print.pairwise_wilcoxon: weighted", {
  result <- friedman_test(survey_data, trust_government, trust_media,
                          trust_science, weights = sampling_weight) %>%
    pairwise_wilcoxon()
  expect_prints(result, "Pairwise Wilcoxon")
})

# ===========================================================================
# 25. reliability()
# ===========================================================================
test_that("print.reliability: ungrouped, unweighted (compact)", {
  result <- reliability(survey_data, trust_government, trust_media,
                        trust_science)
  output <- expect_prints(result, "Reliability")
  expect_true(any(grepl("Alpha", output, fixed = TRUE)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("print.reliability: ungrouped, weighted", {
  result <- reliability(survey_data, trust_government, trust_media,
                        trust_science, weights = sampling_weight)
  output <- expect_prints(result, "Reliability")
  expect_true(any(grepl("weighted", output, ignore.case = TRUE)))
})

test_that("print.reliability: grouped", {
  result <- survey_data %>% group_by(region) %>%
    reliability(trust_government, trust_media, trust_science)
  expect_prints(result, "Reliability")
})

# ===========================================================================
# 26. efa()
# ===========================================================================
test_that("print.efa: PCA + varimax (default, compact)", {
  result <- efa(survey_data, trust_government, trust_media, trust_science,
                life_satisfaction)
  output <- expect_prints(result, "Exploratory Factor Analysis")
  expect_true(any(grepl("KMO", output, fixed = TRUE)))
  expect_true(any(grepl("Variance explained", output, fixed = TRUE)))
})

test_that("print.efa: PCA + oblimin (compact)", {
  skip_if_not_installed("GPArotation")
  result <- efa(survey_data, trust_government, trust_media, trust_science,
                life_satisfaction, rotation = "oblimin")
  output <- expect_prints(result, "Exploratory Factor Analysis")
  expect_true(any(grepl("Oblimin", output, fixed = TRUE)))
})

test_that("print.efa: PCA + promax (compact)", {
  result <- efa(survey_data, trust_government, trust_media, trust_science,
                life_satisfaction, rotation = "promax")
  output <- expect_prints(result, "Exploratory Factor Analysis")
  expect_true(any(grepl("Promax", output, fixed = TRUE)))
})

test_that("print.efa: PCA + none", {
  result <- efa(survey_data, trust_government, trust_media, trust_science,
                life_satisfaction, rotation = "none")
  expect_prints(result, "Exploratory Factor Analysis")
})

test_that("print.efa: ML extraction (compact)", {
  result <- efa(survey_data, trust_government, trust_media, trust_science,
                life_satisfaction, extraction = "ml")
  output <- expect_prints(result, "Exploratory Factor Analysis")
  expect_true(any(grepl("ML", output, fixed = TRUE)))
})

test_that("print.efa: weighted", {
  result <- efa(survey_data, trust_government, trust_media, trust_science,
                life_satisfaction, weights = sampling_weight)
  expect_prints(result, "Weighted")
})

test_that("print.efa: grouped", {
  result <- survey_data %>% group_by(gender) %>%
    efa(trust_government, trust_media, trust_science, life_satisfaction)
  expect_prints(result, "Exploratory Factor Analysis")
})
# ===========================================================================
# 27. linear_regression()
# ===========================================================================
test_that("print.linear_regression: ungrouped, unweighted (compact)", {
  result <- linear_regression(survey_data, dependent = life_satisfaction,
                              predictors = c(age, income))
  output <- expect_prints(result, "Linear Regression")
  expect_true(any(grepl("R2", output, fixed = TRUE)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
  expect_true(any(grepl("F(", output, fixed = TRUE)))
})

test_that("print.linear_regression: weighted (compact)", {
  result <- linear_regression(survey_data, dependent = life_satisfaction,
                              predictors = c(age, income),
                              weights = sampling_weight)
  output <- expect_prints(result, "Linear Regression")
  expect_true(any(grepl("[Weighted]", output, fixed = TRUE)))
})

test_that("print.linear_regression: grouped (compact)", {
  result <- survey_data %>% group_by(gender) %>%
    linear_regression(dependent = life_satisfaction,
                      predictors = c(age, income))
  output <- expect_prints(result, "Linear Regression")
  expect_true(any(grepl("[Grouped:", output, fixed = TRUE)))
})

# ===========================================================================
# 28. logistic_regression()
# ===========================================================================
test_that("print.logistic_regression: ungrouped, unweighted (compact)", {
  sd2 <- survey_data %>%
    mutate(high_sat = as.integer(
      life_satisfaction > median(life_satisfaction, na.rm = TRUE)))
  result <- logistic_regression(sd2, dependent = high_sat,
                                predictors = c(age, income))
  output <- expect_prints(result, "Logistic Regression")
  expect_true(any(grepl("Nagelkerke R2", output, fixed = TRUE)))
  expect_true(any(grepl("chi2(", output, fixed = TRUE)))
  expect_true(any(grepl("Accuracy", output, fixed = TRUE)))
  expect_true(any(grepl("N = ", output, fixed = TRUE)))
})

test_that("print.logistic_regression: weighted (compact)", {
  sd2 <- survey_data %>%
    mutate(high_sat = as.integer(
      life_satisfaction > median(life_satisfaction, na.rm = TRUE)))
  result <- logistic_regression(sd2, dependent = high_sat,
                                predictors = c(age, income),
                                weights = sampling_weight)
  output <- expect_prints(result, "Logistic Regression")
  expect_true(any(grepl("[Weighted]", output, fixed = TRUE)))
})

test_that("print.logistic_regression: grouped (compact)", {
  sd2 <- survey_data %>%
    mutate(high_sat = as.integer(
      life_satisfaction > median(life_satisfaction, na.rm = TRUE)))
  result <- sd2 %>% group_by(gender) %>%
    logistic_regression(dependent = high_sat,
                        predictors = c(age, income))
  output <- expect_prints(result, "Logistic Regression")
  expect_true(any(grepl("[Grouped:", output, fixed = TRUE)))
})


# ===========================================================================
# 29. w_modus()
# ===========================================================================
test_that("print.w_modus: ungrouped, unweighted, single var", {
  result <- w_modus(survey_data, gender)
  expect_prints(result, "Mode")
})

test_that("print.w_modus: ungrouped, weighted, single var", {
  result <- w_modus(survey_data, gender, weights = sampling_weight)
  expect_prints(result, "Mode")
})

test_that("print.w_modus: ungrouped, multi var", {
  result <- w_modus(survey_data, gender, region)
  expect_prints(result, "Mode")
})

test_that("print.w_modus: grouped, weighted", {
  result <- survey_data %>% group_by(region) %>%
    w_modus(gender, weights = sampling_weight)
  expect_prints(result, "Mode")
})

# ===========================================================================
# 30. w_quantile()
# ===========================================================================
test_that("print.w_quantile: ungrouped, unweighted", {
  result <- w_quantile(survey_data, age)
  expect_prints(result, "Quantile")
})

test_that("print.w_quantile: ungrouped, weighted", {
  result <- w_quantile(survey_data, age, weights = sampling_weight)
  expect_prints(result, "Quantile")
})

test_that("print.w_quantile: multi var, weighted", {
  result <- w_quantile(survey_data, age, income, weights = sampling_weight)
  expect_prints(result, "Quantile")
})

test_that("print.w_quantile: grouped", {
  result <- survey_data %>% group_by(region) %>%
    w_quantile(age, weights = sampling_weight)
  expect_prints(result, "Quantile")
})
