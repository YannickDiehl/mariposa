# Package index

## Descriptive Statistics

Summarize and explore your survey data

- [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
  : Get to Know Your Numeric Data
- [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  : Count How Many People Chose Each Option
- [`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)
  : Compare Two Categories: See How They Relate

## Hypothesis Testing

Compare groups and test for significant differences

- [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)
  : Test If Two Groups Differ
- [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
  : Compare Multiple Groups: Are Their Averages Different?
- [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)
  : Compare Two Groups Without Assuming Normal Data
- [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  [`phi()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  [`cramers_v()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  [`goodman_gamma()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  : Test If Two Categories Are Related

## Correlation Analysis

Measure relationships between variables

- [`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)
  : Measure How Strongly Variables Are Related
- [`spearman_rho()`](https://YannickDiehl.github.io/mariposa/reference/spearman_rho.md)
  : Spearman's Rank Correlation Analysis
- [`kendall_tau()`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md)
  : Kendall's Tau Correlation Analysis

## Post-Hoc Analysis

Follow-up tests for detailed group comparisons

- [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)
  : Find Which Specific Groups Differ After ANOVA
- [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md)
  : Compare All Groups More Conservatively After ANOVA
- [`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)
  : Test If Groups Vary Similarly

## Weighted Statistics

Individual weighted statistics for survey data

- [`w_mean()`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md)
  : Calculate Population-Representative Averages
- [`w_median()`](https://YannickDiehl.github.io/mariposa/reference/w_median.md)
  : Find the Population-Representative Middle Value
- [`w_sd()`](https://YannickDiehl.github.io/mariposa/reference/w_sd.md)
  : Calculate Population-Representative Standard Deviations
- [`w_var()`](https://YannickDiehl.github.io/mariposa/reference/w_var.md)
  : Calculate Population-Representative Variance
- [`w_se()`](https://YannickDiehl.github.io/mariposa/reference/w_se.md)
  : Calculate Population-Representative Standard Errors
- [`w_iqr()`](https://YannickDiehl.github.io/mariposa/reference/w_iqr.md)
  : Measure Population-Representative Spread (IQR)
- [`w_range()`](https://YannickDiehl.github.io/mariposa/reference/w_range.md)
  : Find the Range of Your Data
- [`w_quantile()`](https://YannickDiehl.github.io/mariposa/reference/w_quantile.md)
  : Calculate Population-Representative Percentiles
- [`w_modus()`](https://YannickDiehl.github.io/mariposa/reference/w_modus.md)
  : Find the Most Common Value in Your Population
- [`w_skew()`](https://YannickDiehl.github.io/mariposa/reference/w_skew.md)
  : Measure Population-Representative Skewness
- [`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/reference/w_kurtosis.md)
  : Measure Population-Representative Kurtosis

## Datasets

Example datasets for learning and testing

- [`survey_data`](https://YannickDiehl.github.io/mariposa/reference/survey_data.md)
  : Social Survey Data (Synthetic)
- [`longitudinal_data`](https://YannickDiehl.github.io/mariposa/reference/longitudinal_data.md)
  : Longitudinal Study Data (Synthetic)
- [`longitudinal_data_wide`](https://YannickDiehl.github.io/mariposa/reference/longitudinal_data_wide.md)
  : Longitudinal Study Data - Wide Format (Synthetic)

## Print Methods

Formatted output for all result types

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
