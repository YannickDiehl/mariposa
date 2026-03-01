# mariposa

**Professional statistical analysis for survey data in R.**

mariposa (*Marburg Initiative for Political and Social Analysis*)
provides 37 statistical functions for analyzing survey data. All
functions support survey weights, grouped analysis via
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
and produce publication-ready output. Results are validated against SPSS
v29 for full reproducibility.

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
survey_data %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)

# Scale analysis workflow
reliability(survey_data, trust_government, trust_media, trust_science)

survey_data <- survey_data %>%
  mutate(m_trust = scale_index(., trust_government, trust_media, trust_science))

# Regression
survey_data %>%
  linear_regression(life_satisfaction ~ age + income, weights = sampling_weight)
```

## Core Features

### Statistical Functions

| Category           | Functions                                                                                                                                                                                                                                                                                                                                                                                                                                                   | Purpose                                               |
|--------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------|
| **Descriptive**    | [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md), [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md), [`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)                                                                                                                                                                                                               | Summaries and distributions                           |
| **T-Tests**        | [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)                                                                                                                                                                                                                                                                                                                                                                                   | Mean comparisons (independent, paired, one-sample)    |
| **ANOVA**          | [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)                                                                                                                                                                                                                                                                                                                                                                       | Multiple group comparisons                            |
| **Non-parametric** | [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md), [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md), [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md), [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md), [`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md) | Distribution-free tests                               |
| **Correlation**    | [`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md), [`spearman_rho()`](https://YannickDiehl.github.io/mariposa/reference/spearman_rho.md), [`kendall_tau()`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md)                                                                                                                                                                                             | Relationships between variables                       |
| **Post-hoc**       | [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md), [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md), [`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)                                                                                                                                                                                               | Follow-up analyses                                    |
| **Chi-square**     | [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)                                                                                                                                                                                                                                                                                                                                                                           | Categorical associations                              |
| **Scale analysis** | [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md), [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md), [`scale_index()`](https://YannickDiehl.github.io/mariposa/reference/scale_index.md), [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md)                                                                                                                                      | Cronbach’s Alpha, factor analysis, index construction |
| **Regression**     | [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md), [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)                                                                                                                                                                                                                                                        | Linear and logistic models with SPSS-style output     |
| **Weighted stats** | [`w_mean()`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md), [`w_median()`](https://YannickDiehl.github.io/mariposa/reference/w_median.md), [`w_sd()`](https://YannickDiehl.github.io/mariposa/reference/w_sd.md), + 8 more                                                                                                                                                                                                                   | Individual weighted statistics                        |

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

### S3 Post-Hoc Methods

``` r
# Run ANOVA, then chain post-hoc tests
result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education, weights = sampling_weight)

result %>% tukey_test()    # Pairwise comparisons
result %>% levene_test()   # Variance homogeneity
```

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
