# Package index

## 🎯 Descriptive Statistics

Core descriptive and frequency analysis functions

- [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
  : Get to Know Your Numeric Data
- [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  : Count How Many People Chose Each Option
- [`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)
  : Compare Two Categories: See How They Relate

## 📈 Hypothesis Testing

Statistical tests for comparing groups and testing hypotheses

- [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)
  : Test If Two Groups Differ
- [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)
  : Compare Two Groups Without Assuming Normal Data
- [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  [`phi()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  [`cramers_v()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  [`goodman_gamma()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  : Test If Two Categories Are Related
- [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
  : Compare Multiple Groups: Are Their Averages Different?

## 🔗 Correlation Analysis

Correlation coefficients for continuous and ordinal variables

- [`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)
  : Measure How Strongly Variables Are Related
- [`kendall_tau()`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md)
  : Kendall's Tau Correlation Analysis
- [`spearman_rho()`](https://YannickDiehl.github.io/mariposa/reference/spearman_rho.md)
  : Spearman's Rank Correlation Analysis

## 🔍 Post-Hoc Analysis

S3 generics for extended analysis of test results

- [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)
  : Find Which Specific Groups Differ After ANOVA
- [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md)
  : Compare All Groups More Conservatively After ANOVA
- [`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)
  : Test If Groups Vary Similarly

## 📊 Weighted Statistics

Specialized weighted statistical functions for survey data

- [`w_mean()`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md)
  : Calculate Population-Representative Averages
- [`w_sd()`](https://YannickDiehl.github.io/mariposa/reference/w_sd.md)
  : Weighted Standard Deviation
- [`w_var()`](https://YannickDiehl.github.io/mariposa/reference/w_var.md)
  : Weighted Variance
- [`w_se()`](https://YannickDiehl.github.io/mariposa/reference/w_se.md)
  : Weighted Standard Error
- [`w_median()`](https://YannickDiehl.github.io/mariposa/reference/w_median.md)
  : Weighted Median
- [`w_iqr()`](https://YannickDiehl.github.io/mariposa/reference/w_iqr.md)
  : Weighted Interquartile Range (IQR)
- [`w_range()`](https://YannickDiehl.github.io/mariposa/reference/w_range.md)
  : Weighted Range
- [`w_quantile()`](https://YannickDiehl.github.io/mariposa/reference/w_quantile.md)
  : Weighted Quantiles
- [`w_modus()`](https://YannickDiehl.github.io/mariposa/reference/w_modus.md)
  : Weighted Mode (Modus)
- [`w_skew()`](https://YannickDiehl.github.io/mariposa/reference/w_skew.md)
  : Weighted Skewness
- [`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/reference/w_kurtosis.md)
  : Weighted Kurtosis

## 📋 Datasets

Synthetic survey and longitudinal datasets for testing and examples

- [`survey_data`](https://YannickDiehl.github.io/mariposa/reference/survey_data.md)
  : Social Survey Data (Synthetic)
- [`longitudinal_data`](https://YannickDiehl.github.io/mariposa/reference/longitudinal_data.md)
  : Longitudinal Study Data (Synthetic)
- [`longitudinal_data_wide`](https://YannickDiehl.github.io/mariposa/reference/longitudinal_data_wide.md)
  : Longitudinal Study Data - Wide Format (Synthetic)

## 🛠️ Print Methods

Professional output formatting methods

- [`print(`*`<chi_square>`*`)`](https://YannickDiehl.github.io/mariposa/reference/print.chi_square.md)
  : Print method for chi_square
- [`print(`*`<crosstab>`*`)`](https://YannickDiehl.github.io/mariposa/reference/print.crosstab.md)
  : Print method for crosstab results
- [`print(`*`<describe>`*`)`](https://YannickDiehl.github.io/mariposa/reference/print.describe.md)
  : Print method for describe objects
- [`print(`*`<frequency>`*`)`](https://YannickDiehl.github.io/mariposa/reference/print.frequency.md)
  : Print method for frequency objects
- [`print(`*`<kendall_tau>`*`)`](https://YannickDiehl.github.io/mariposa/reference/print.kendall_tau.md)
  : Print method for kendall_tau
- [`print(`*`<levene_test>`*`)`](https://YannickDiehl.github.io/mariposa/reference/print.levene_test.md)
  : Print method for Levene test results
- [`print(`*`<mann_whitney>`*`)`](https://YannickDiehl.github.io/mariposa/reference/print.mann_whitney.md)
  : Print method for Mann-Whitney test results
- [`print(`*`<oneway_anova>`*`)`](https://YannickDiehl.github.io/mariposa/reference/print.oneway_anova.md)
  : Print ANOVA test results
- [`print(`*`<pearson_cor>`*`)`](https://YannickDiehl.github.io/mariposa/reference/print.pearson_cor.md)
  : Print method for pearson_cor
- [`print(`*`<scheffe_test>`*`)`](https://YannickDiehl.github.io/mariposa/reference/print.scheffe_test.md)
  : Print Scheffe test results
- [`print(`*`<spearman_rho>`*`)`](https://YannickDiehl.github.io/mariposa/reference/print.spearman_rho.md)
  : Print method for spearman_rho
- [`print(`*`<t_test>`*`)`](https://YannickDiehl.github.io/mariposa/reference/print.t_test.md)
  : Print t-test results
- [`print(`*`<tukey_test>`*`)`](https://YannickDiehl.github.io/mariposa/reference/print.tukey_test.md)
  : Print Tukey HSD test results
- [`print(`*`<w_modus>`*`)`](https://YannickDiehl.github.io/mariposa/reference/print.w_modus.md)
  : Print method for w_modus objects
