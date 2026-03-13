# mariposa

**Professional statistical analysis for survey data in R.**

mariposa (*Marburg Initiative for Political and Social Analysis*)
provides 76 functions for importing, managing, transforming, and
analyzing survey data. Covers the full workflow from data import (SPSS,
Stata, SAS, Excel) through label management, recoding, and
standardization to statistical analysis with survey weights, grouped
operations via
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
and publication-ready output. All statistical results are validated
against SPSS v29 for full reproducibility.

## Installation

``` r
# Install from GitHub
devtools::install_github("YannickDiehl/mariposa")
```

## Quick Start

``` r
library(mariposa)
library(dplyr)

# Load example survey data (2,500 respondents)
data(survey_data)

# Interactive HTML codebook in RStudio Viewer
codebook(survey_data)

# Descriptive statistics with survey weights
survey_data %>%
  describe(age, income, life_satisfaction, weights = sampling_weight)

# Frequency table
survey_data %>%
  frequency(education, weights = sampling_weight)

# Compare groups with t-test
survey_data %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)

# Detailed SPSS-style output with summary()
survey_data %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight) %>%
  summary()

# Scale analysis workflow
reliability(survey_data, trust_government, trust_media, trust_science) %>%
  summary()    # item statistics, inter-item correlations

survey_data <- survey_data %>%
  mutate(m_trust = row_means(., trust_government, trust_media, trust_science))

# Regression
survey_data %>%
  linear_regression(life_satisfaction ~ age + income, weights = sampling_weight) %>%
  summary()    # coefficients, ANOVA table, diagnostics
```

## Core Features

### Statistical Functions

| Category                | Functions                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | Purpose                                                   |
|-------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------|
| **Data Import**         | [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md), [`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md), [`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md), [`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md), + 2 more                                                                                                                                       | Import SPSS, Stata, SAS, and Excel with tagged NA support |
| **Data Export**         | [`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md), [`write_stata()`](https://YannickDiehl.github.io/mariposa/reference/write_stata.md), [`write_xpt()`](https://YannickDiehl.github.io/mariposa/reference/write_xpt.md), [`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)                                                                                                                                         | Export with full label and missing value roundtripping    |
| **Label Management**    | [`var_label()`](https://YannickDiehl.github.io/mariposa/reference/var_label.md), [`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md), [`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md), [`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md), + 6 more                                                                                                                                             | Get/set labels, convert formats, declare missing values   |
| **Data Transformation** | [`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md), [`to_dummy()`](https://YannickDiehl.github.io/mariposa/reference/to_dummy.md), [`std()`](https://YannickDiehl.github.io/mariposa/reference/std.md), [`center()`](https://YannickDiehl.github.io/mariposa/reference/center.md), [`find_var()`](https://YannickDiehl.github.io/mariposa/reference/find_var.md)                                                                                                  | Recoding, dummy coding, standardization, centering        |
| **Descriptive**         | [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md), [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md), [`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md), [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)                                                                                                                                                       | Summaries, distributions, and data dictionaries           |
| **T-Tests**             | [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)                                                                                                                                                                                                                                                                                                                                                                                                          | Mean comparisons (independent, paired, one-sample)        |
| **ANOVA**               | [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md), [`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md), [`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)                                                                                                                                                                                                                      | One-way, multi-factor ANOVA, and ANCOVA with Type III SS  |
| **Non-parametric**      | [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md), [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md), [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md), [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md), [`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md)                        | Distribution-free tests                                   |
| **Exact tests**         | [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md), [`fisher_test()`](https://YannickDiehl.github.io/mariposa/reference/fisher_test.md), [`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md), [`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md)                                                                                                                                     | Categorical associations and exact tests                  |
| **Correlation**         | [`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md), [`spearman_rho()`](https://YannickDiehl.github.io/mariposa/reference/spearman_rho.md), [`kendall_tau()`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md)                                                                                                                                                                                                                    | Relationships between variables                           |
| **Post-hoc**            | [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md), [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md), [`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md), [`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md), [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md)                                    | Follow-up analyses (parametric and non-parametric)        |
| **Scale analysis**      | [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md), [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md), [`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md), [`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md), [`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md), [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md) | Cronbach’s Alpha, factor analysis, index construction     |
| **Regression**          | [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md), [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)                                                                                                                                                                                                                                                                               | Linear and logistic models with SPSS-style output         |
| **Effect sizes**        | [`phi()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md), [`cramers_v()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md), [`goodman_gamma()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)                                                                                                                                                                                                                                 | Effect size measures for categorical data                 |
| **Weighted stats**      | [`w_mean()`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md), [`w_median()`](https://YannickDiehl.github.io/mariposa/reference/w_median.md), [`w_sd()`](https://YannickDiehl.github.io/mariposa/reference/w_sd.md), + 8 more                                                                                                                                                                                                                                          | Individual weighted statistics                            |

### Survey Weights Built-In

Every function handles survey weights correctly:

``` r
# Weighted mean, median, SD
survey_data %>%
  w_mean(age, income, weights = sampling_weight)

# Grouped weighted analysis
survey_data %>%
  group_by(region) %>%
  describe(satisfaction, weights = sampling_weight)
```

### Tidyverse Integration

Full support for pipes and grouped operations:

``` r
survey_data %>%
  filter(age >= 18) %>%
  group_by(region) %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)
```

### Multi-Factor ANOVA & ANCOVA

``` r
# Factorial ANOVA with Type III SS
survey_data %>%
  factorial_anova(dv = income, between = c(gender, education),
                  weights = sampling_weight)

# ANCOVA with covariate adjustment
survey_data %>%
  ancova(dv = income, between = gender, covariate = age,
         weights = sampling_weight)
```

### S3 Post-Hoc Methods

``` r
# Parametric: ANOVA → Tukey/Scheffe
result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education, weights = sampling_weight)

result %>% tukey_test()    # Pairwise comparisons
result %>% levene_test()   # Variance homogeneity

# Non-parametric: Kruskal-Wallis → Dunn
kw_result <- survey_data %>%
  kruskal_wallis(life_satisfaction, group = education)

kw_result %>% dunn_test()  # Pairwise Dunn comparisons
```

### Flexible Output: Compact & Detailed

Every analysis function provides two output levels. Typing the result
name prints a compact one-line summary. Calling
[`summary()`](https://rdrr.io/r/base/summary.html) produces the full
SPSS-style output with all details. You can toggle individual sections
on or off:

``` r
# Compact one-line summary (default)
survey_data %>%
  t_test(life_satisfaction, group = gender)

# Full detailed output
survey_data %>%
  t_test(life_satisfaction, group = gender) %>%
  summary()

# Toggle individual sections
survey_data %>%
  t_test(life_satisfaction, group = gender) %>%
  summary(effect_sizes = FALSE)
```

This works for all analysis functions —
[`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md),
[`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md),
[`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md),
[`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md),
[`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md),
[`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md),
and more.

## SPSS Compatibility

Every function is validated against SPSS v29 across four scenarios:
weighted/unweighted and grouped/ungrouped. If you’re migrating from
SPSS, your results will match:

**SPSS:**

``` spss
WEIGHT BY sampling_weight.
T-TEST GROUPS=gender(1 2)
  /VARIABLES=satisfaction.
```

**mariposa:**

``` r
survey_data %>%
  t_test(satisfaction, group = gender, weights = sampling_weight)
```

## Documentation

- [Complete
  Reference](https://YannickDiehl.github.io/mariposa/reference/) - All
  functions with examples
- [Getting
  Started](https://YannickDiehl.github.io/mariposa/articles/introduction.html) -
  Introduction and first steps
- [Scale
  Analysis](https://YannickDiehl.github.io/mariposa/articles/scale-analysis.html) -
  Reliability, factor analysis, and scale construction
- [Regression
  Analysis](https://YannickDiehl.github.io/mariposa/articles/regression-analysis.html) -
  Linear and logistic regression
- [Survey Weights
  Guide](https://YannickDiehl.github.io/mariposa/articles/survey-weights.html) -
  Working with weighted data

## Support

- [GitHub Issues](https://github.com/YannickDiehl/mariposa/issues) - Bug
  reports and feature requests
- [GitHub
  Discussions](https://github.com/YannickDiehl/mariposa/discussions) -
  Questions and ideas

## License

MIT - Yannick Diehl
