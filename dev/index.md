# mariposa

**Professional statistical analysis for survey data in R.**

mariposa (*Marburg Initiative for Political and Social Analysis*)
provides 44 statistical functions for analyzing survey data. All
functions support survey weights, grouped analysis via
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
and produce publication-ready output. Results are validated against SPSS
v29 for full reproducibility (4,986+ tests pass).

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

# Descriptive statistics with survey weights
survey_data %>%
  describe(age, income, life_satisfaction, weights = sampling_weight)

# Frequency table
survey_data %>%
  frequency(education, weights = sampling_weight)

# Compare groups with t-test
result <- survey_data %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)

result              # compact one-line overview

summary(result)     # full SPSS-style output with all details

# Scale analysis workflow
rel <- reliability(survey_data, trust_government, trust_media, trust_science)

rel                 # compact: Alpha + interpretation

summary(rel)        # detailed: item statistics, inter-item correlations

survey_data <- survey_data %>%
  mutate(m_trust = scale_index(., trust_government, trust_media, trust_science))

# Regression
lm_result <- survey_data %>%
  linear_regression(life_satisfaction ~ age + income, weights = sampling_weight)

lm_result           # compact: R-squared + significant predictors

summary(lm_result)  # detailed: coefficients, ANOVA table, diagnostics
```

## Core Features

### Statistical Functions

| Category           | Functions                                                                                                                                                                                                                                                                                                                                                                                                                                                                       | Purpose                                                  |
|--------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------|
| **Descriptive**    | [`describe()`](https://YannickDiehl.github.io/mariposa/dev/reference/describe.md), [`frequency()`](https://YannickDiehl.github.io/mariposa/dev/reference/frequency.md), [`crosstab()`](https://YannickDiehl.github.io/mariposa/dev/reference/crosstab.md)                                                                                                                                                                                                                       | Summaries and distributions                              |
| **T-Tests**        | [`t_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/t_test.md)                                                                                                                                                                                                                                                                                                                                                                                                   | Mean comparisons (independent, paired, one-sample)       |
| **ANOVA**          | [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/dev/reference/oneway_anova.md), [`factorial_anova()`](https://YannickDiehl.github.io/mariposa/dev/reference/factorial_anova.md), [`ancova()`](https://YannickDiehl.github.io/mariposa/dev/reference/ancova.md)                                                                                                                                                                                                       | One-way, multi-factor ANOVA, and ANCOVA with Type III SS |
| **Non-parametric** | [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/dev/reference/mann_whitney.md), [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/dev/reference/kruskal_wallis.md), [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/wilcoxon_test.md), [`friedman_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/friedman_test.md), [`binomial_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/binomial_test.md) | Distribution-free tests                                  |
| **Exact tests**    | [`chi_square()`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md), [`fisher_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/fisher_test.md), [`chisq_gof()`](https://YannickDiehl.github.io/mariposa/dev/reference/chisq_gof.md), [`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/mcnemar_test.md)                                                                                                                  | Categorical associations and exact tests                 |
| **Correlation**    | [`pearson_cor()`](https://YannickDiehl.github.io/mariposa/dev/reference/pearson_cor.md), [`spearman_rho()`](https://YannickDiehl.github.io/mariposa/dev/reference/spearman_rho.md), [`kendall_tau()`](https://YannickDiehl.github.io/mariposa/dev/reference/kendall_tau.md)                                                                                                                                                                                                     | Relationships between variables                          |
| **Post-hoc**       | [`tukey_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/tukey_test.md), [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/scheffe_test.md), [`levene_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/levene_test.md), [`dunn_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/dunn_test.md), [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/dev/reference/pairwise_wilcoxon.md)             | Follow-up analyses (parametric and non-parametric)       |
| **Scale analysis** | [`reliability()`](https://YannickDiehl.github.io/mariposa/dev/reference/reliability.md), [`efa()`](https://YannickDiehl.github.io/mariposa/dev/reference/efa.md), [`scale_index()`](https://YannickDiehl.github.io/mariposa/dev/reference/scale_index.md), [`pomps()`](https://YannickDiehl.github.io/mariposa/dev/reference/pomps.md)                                                                                                                                          | Cronbach’s Alpha, factor analysis, index construction    |
| **Regression**     | [`linear_regression()`](https://YannickDiehl.github.io/mariposa/dev/reference/linear_regression.md), [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/dev/reference/logistic_regression.md)                                                                                                                                                                                                                                                                    | Linear and logistic models with SPSS-style output        |
| **Effect sizes**   | [`phi()`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md), [`cramers_v()`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md), [`goodman_gamma()`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md)                                                                                                                                                                                                                  | Effect size measures for categorical data                |
| **Weighted stats** | [`w_mean()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_mean.md), [`w_median()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_median.md), [`w_sd()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_sd.md), + 8 more                                                                                                                                                                                                                           | Individual weighted statistics                           |

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
result <- t_test(survey_data, life_satisfaction, group = gender)
result              # compact one-line summary
summary(result)     # full detailed output
summary(result, effect_sizes = FALSE)  # hide effect sizes
```

This works for all 13 analysis functions —
[`t_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/t_test.md),
[`oneway_anova()`](https://YannickDiehl.github.io/mariposa/dev/reference/oneway_anova.md),
[`chi_square()`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md),
[`pearson_cor()`](https://YannickDiehl.github.io/mariposa/dev/reference/pearson_cor.md),
[`reliability()`](https://YannickDiehl.github.io/mariposa/dev/reference/reliability.md),
[`linear_regression()`](https://YannickDiehl.github.io/mariposa/dev/reference/linear_regression.md),
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
