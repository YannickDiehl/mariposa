# Package index

## Descriptive Statistics

Summarize and explore your survey data

- [`describe()`](https://YannickDiehl.github.io/mariposa/dev/reference/describe.md)
  : Get to Know Your Numeric Data
- [`frequency()`](https://YannickDiehl.github.io/mariposa/dev/reference/frequency.md)
  : Count How Many People Chose Each Option
- [`crosstab()`](https://YannickDiehl.github.io/mariposa/dev/reference/crosstab.md)
  : Compare Two Categories: See How They Relate

## Hypothesis Testing

Compare groups and test for significant differences

- [`t_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/t_test.md)
  : Test If Two Groups Differ
- [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/dev/reference/oneway_anova.md)
  : Compare Multiple Groups: Are Their Averages Different?
- [`factorial_anova()`](https://YannickDiehl.github.io/mariposa/dev/reference/factorial_anova.md)
  : Compare Groups Across Multiple Factors: Factorial ANOVA
- [`ancova()`](https://YannickDiehl.github.io/mariposa/dev/reference/ancova.md)
  : Analysis of Covariance: ANCOVA
- [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/dev/reference/mann_whitney.md)
  : Compare Two Groups Without Assuming Normal Data
- [`chi_square()`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md)
  [`phi()`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md)
  [`cramers_v()`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md)
  [`goodman_gamma()`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md)
  : Test If Two Categories Are Related
- [`fisher_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/fisher_test.md)
  : Fisher's Exact Test for Small Samples
- [`chisq_gof()`](https://YannickDiehl.github.io/mariposa/dev/reference/chisq_gof.md)
  : Chi-Square Goodness-of-Fit Test
- [`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/mcnemar_test.md)
  : McNemar's Test for Paired Proportions

## Non-Parametric Tests

Distribution-free tests for ordinal and nominal data

- [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/dev/reference/kruskal_wallis.md)
  : Compare Multiple Groups Without Assuming Normal Data
- [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/wilcoxon_test.md)
  : Compare Two Related Measurements Without Assuming Normality
- [`friedman_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/friedman_test.md)
  : Compare Three or More Related Measurements Without Assuming
  Normality
- [`binomial_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/binomial_test.md)
  : Test Whether a Proportion Matches an Expected Value

## Correlation Analysis

Measure relationships between variables

- [`pearson_cor()`](https://YannickDiehl.github.io/mariposa/dev/reference/pearson_cor.md)
  : Measure How Strongly Variables Are Related
- [`spearman_rho()`](https://YannickDiehl.github.io/mariposa/dev/reference/spearman_rho.md)
  : Spearman's Rank Correlation Analysis
- [`kendall_tau()`](https://YannickDiehl.github.io/mariposa/dev/reference/kendall_tau.md)
  : Kendall's Tau Correlation Analysis

## Post-Hoc Analysis

Follow-up tests for detailed group comparisons

- [`tukey_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/tukey_test.md)
  : Find Which Specific Groups Differ After ANOVA
- [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/scheffe_test.md)
  : Compare All Groups More Conservatively After ANOVA
- [`levene_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/levene_test.md)
  : Test If Groups Vary Similarly
- [`dunn_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/dunn_test.md)
  : Find Which Specific Groups Differ After Kruskal-Wallis
- [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/dev/reference/pairwise_wilcoxon.md)
  : Find Which Specific Measurements Differ After Friedman Test

## Scale Analysis

Factor analysis, reliability, and scale construction

- [`reliability()`](https://YannickDiehl.github.io/mariposa/dev/reference/reliability.md)
  : Check How Reliably Your Scale Measures a Concept
- [`efa()`](https://YannickDiehl.github.io/mariposa/dev/reference/efa.md)
  : Explore the Structure Behind Your Survey Items
- [`scale_index()`](https://YannickDiehl.github.io/mariposa/dev/reference/scale_index.md)
  : Create a Mean Index Across Items
- [`pomps()`](https://YannickDiehl.github.io/mariposa/dev/reference/pomps.md)
  : Transform Scores to Percent of Maximum Possible (POMPS)

## Regression Analysis

Linear and logistic regression with SPSS-compatible output

- [`linear_regression()`](https://YannickDiehl.github.io/mariposa/dev/reference/linear_regression.md)
  : Run a Linear Regression
- [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/dev/reference/logistic_regression.md)
  : Run a Logistic Regression

## Weighted Statistics

Individual weighted statistics for survey data

- [`w_mean()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_mean.md)
  : Calculate Population-Representative Averages
- [`w_median()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_median.md)
  : Find the Population-Representative Middle Value
- [`w_sd()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_sd.md)
  : Calculate Population-Representative Standard Deviations
- [`w_var()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_var.md)
  : Calculate Population-Representative Variance
- [`w_se()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_se.md)
  : Calculate Population-Representative Standard Errors
- [`w_iqr()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_iqr.md)
  : Measure Population-Representative Spread (IQR)
- [`w_range()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_range.md)
  : Find the Range of Your Data
- [`w_quantile()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_quantile.md)
  : Calculate Population-Representative Percentiles
- [`w_modus()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_modus.md)
  : Find the Most Common Value in Your Population
- [`w_skew()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_skew.md)
  : Measure Population-Representative Skewness
- [`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_kurtosis.md)
  : Measure Population-Representative Kurtosis

## Datasets

Example datasets for learning and testing

- [`survey_data`](https://YannickDiehl.github.io/mariposa/dev/reference/survey_data.md)
  : Social Survey Data (Synthetic)
- [`longitudinal_data`](https://YannickDiehl.github.io/mariposa/dev/reference/longitudinal_data.md)
  : Longitudinal Study Data (Synthetic)
- [`longitudinal_data_wide`](https://YannickDiehl.github.io/mariposa/dev/reference/longitudinal_data_wide.md)
  : Longitudinal Study Data - Wide Format (Synthetic)

## Print Methods

Formatted output for all result types

- [`print(`*`<ancova>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.ancova.md)
  : Print ANCOVA results
- [`print(`*`<binomial_test>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.binomial_test.md)
  : Print method for binomial test results
- [`print(`*`<chi_square>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.chi_square.md)
  : Print method for chi_square
- [`print(`*`<chisq_gof>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.chisq_gof.md)
  : Print chi-square goodness-of-fit test results
- [`print(`*`<crosstab>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.crosstab.md)
  : Print method for crosstab results
- [`print(`*`<describe>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.describe.md)
  : Print method for describe objects
- [`print(`*`<dunn_test>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.dunn_test.md)
  : Print Dunn post-hoc test results
- [`print(`*`<factorial_anova>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.factorial_anova.md)
  : Print factorial ANOVA results
- [`print(`*`<fisher_test>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.fisher_test.md)
  : Print Fisher's exact test results
- [`print(`*`<frequency>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.frequency.md)
  : Print method for frequency objects
- [`print(`*`<friedman_test>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.friedman_test.md)
  : Print method for Friedman test results
- [`print(`*`<kendall_tau>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.kendall_tau.md)
  : Print method for kendall_tau
- [`print(`*`<kruskal_wallis>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.kruskal_wallis.md)
  : Print method for Kruskal-Wallis test results
- [`print(`*`<levene_test>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.levene_test.md)
  : Print method for Levene test results
- [`print(`*`<linear_regression>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.linear_regression.md)
  : Print Linear Regression Results
- [`print(`*`<logistic_regression>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.logistic_regression.md)
  : Print Logistic Regression Results
- [`print(`*`<mann_whitney>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.mann_whitney.md)
  : Print method for Mann-Whitney test results
- [`print(`*`<mcnemar_test>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.mcnemar_test.md)
  : Print McNemar test results
- [`print(`*`<oneway_anova>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.oneway_anova.md)
  : Print ANOVA test results
- [`print(`*`<pairwise_wilcoxon>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.pairwise_wilcoxon.md)
  : Print pairwise Wilcoxon post-hoc test results
- [`print(`*`<pearson_cor>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.pearson_cor.md)
  : Print method for pearson_cor
- [`print(`*`<scheffe_test>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.scheffe_test.md)
  : Print Scheffe test results
- [`print(`*`<spearman_rho>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.spearman_rho.md)
  : Print method for spearman_rho
- [`print(`*`<t_test>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.t_test.md)
  : Print t-test results
- [`print(`*`<tukey_test>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.tukey_test.md)
  : Print Tukey HSD test results
- [`print(`*`<w_modus>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.w_modus.md)
  : Print method for w_modus objects
- [`print(`*`<wilcoxon_test>`*`)`](https://YannickDiehl.github.io/mariposa/dev/reference/print.wilcoxon_test.md)
  : Print method for Wilcoxon signed-rank test results
