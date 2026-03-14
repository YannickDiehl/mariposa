# Package index

## Data Import

Import statistical data files with preserved missing value information

- [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
  : Read SPSS Data with Tagged Missing Values
- [`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md)
  : Read SPSS Portable Data with Tagged Missing Values
- [`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md)
  : Read Stata Data with Tagged Missing Values
- [`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md)
  : Read SAS Data with Tagged Missing Values
- [`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md)
  : Read SAS Transport File with Tagged Missing Values
- [`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md)
  : Read Excel Data with Label Reconstruction

## Data Export

Export data to statistical formats with preserved labels and missing
values

- [`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md)
  : Export Data to SPSS Format
- [`write_stata()`](https://YannickDiehl.github.io/mariposa/reference/write_stata.md)
  : Export Data to Stata Format
- [`write_xpt()`](https://YannickDiehl.github.io/mariposa/reference/write_xpt.md)
  : Export Data to SAS Transport Format
- [`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)
  : Export Data to Excel with Label Support

## Label Management

Inspect, modify, and convert labelled survey data

- [`var_label()`](https://YannickDiehl.github.io/mariposa/reference/var_label.md)
  : Get or Set Variable Labels
- [`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md)
  : Get or Set Value Labels
- [`copy_labels()`](https://YannickDiehl.github.io/mariposa/reference/copy_labels.md)
  : Copy Labels from One Data Frame to Another
- [`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md)
  : Remove Unused Value Labels
- [`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)
  : Convert Labelled Variables to Factors
- [`to_character()`](https://YannickDiehl.github.io/mariposa/reference/to_character.md)
  : Convert Labelled Variables to Character
- [`to_numeric()`](https://YannickDiehl.github.io/mariposa/reference/to_numeric.md)
  : Convert Factors or Labelled Variables to Numeric
- [`to_labelled()`](https://YannickDiehl.github.io/mariposa/reference/to_labelled.md)
  : Convert Variables to Labelled Format
- [`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md)
  : Declare Values as Missing
- [`unlabel()`](https://YannickDiehl.github.io/mariposa/reference/unlabel.md)
  : Remove All Label Metadata

## Data Transformation

Recode, dichotomize, dummy-code, standardize, and center variables

- [`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md) :
  Recode Variables Using String Syntax
- [`to_dummy()`](https://YannickDiehl.github.io/mariposa/reference/to_dummy.md)
  : Create Dummy Variables (One-Hot Encoding)
- [`std()`](https://YannickDiehl.github.io/mariposa/reference/std.md) :
  Standardize Variables (Z-Scores)
- [`center()`](https://YannickDiehl.github.io/mariposa/reference/center.md)
  : Center Variables (Mean Centering)

## Data Exploration

Search and inspect your data

- [`find_var()`](https://YannickDiehl.github.io/mariposa/reference/find_var.md)
  : Find Variables by Name or Label

## Descriptive Statistics

Summarize and explore your survey data

- [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
  : Create a Codebook for Your Data
- [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
  : Get to Know Your Numeric Data
- [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  [`fre()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  : Count How Many People Chose Each Option
- [`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)
  : Compare Two Categories: See How They Relate

## Hypothesis Testing

Compare groups and test for significant differences

- [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)
  : Test If Two Groups Differ
- [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
  : Compare Multiple Groups: Are Their Averages Different?
- [`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md)
  : Compare Groups Across Multiple Factors: Factorial ANOVA
- [`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)
  : Analysis of Covariance: ANCOVA
- [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)
  : Compare Two Groups Without Assuming Normal Data
- [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  [`phi()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  [`cramers_v()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  [`goodman_gamma()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  : Test If Two Categories Are Related
- [`fisher_test()`](https://YannickDiehl.github.io/mariposa/reference/fisher_test.md)
  : Fisher's Exact Test for Small Samples
- [`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md)
  : Chi-Square Goodness-of-Fit Test
- [`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md)
  : McNemar's Test for Paired Proportions

## Non-Parametric Tests

Distribution-free tests for ordinal and nominal data

- [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md)
  : Compare Multiple Groups Without Assuming Normal Data
- [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)
  : Compare Two Related Measurements Without Assuming Normality
- [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)
  : Compare Three or More Related Measurements Without Assuming
  Normality
- [`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md)
  : Test Whether a Proportion Matches an Expected Value

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
- [`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md)
  : Find Which Specific Groups Differ After Kruskal-Wallis
- [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md)
  : Find Which Specific Measurements Differ After Friedman Test

## Scale Analysis

Factor analysis, reliability, and scale construction

- [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md)
  : Check How Reliably Your Scale Measures a Concept
- [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md) :
  Explore the Structure Behind Your Survey Items
- [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md)
  : Transform Scores to Percent of Maximum Possible (POMPS)
- [`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md)
  : Compute Row Means Across Items
- [`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md)
  : Compute Row Sums Across Items
- [`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md)
  : Count Occurrences of a Value Across Columns

## Regression Analysis

Linear and logistic regression with SPSS-compatible output

- [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
  : Run a Linear Regression
- [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
  : Run a Logistic Regression

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
