# mariposa <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/YannickDiehl/mariposa/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/YannickDiehl/mariposa/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/YannickDiehl/mariposa/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/YannickDiehl/mariposa/actions/workflows/test-coverage.yaml)
[![Codecov test coverage](https://codecov.io/gh/YannickDiehl/mariposa/branch/main/graph/badge.svg)](https://app.codecov.io/gh/YannickDiehl/mariposa?branch=main)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**Professional statistical analysis for survey data in R.**

mariposa (*Marburg Initiative for Political and Social Analysis*) provides 37 statistical functions for analyzing survey data. All functions support survey weights, grouped analysis via `dplyr::group_by()`, and produce publication-ready output. Results are validated against SPSS v29 for full reproducibility.

## Installation

```r
# Install from GitHub
devtools::install_github("YannickDiehl/mariposa")
```

## Quick Start

```r
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

| Category | Functions | Purpose |
|----------|-----------|---------|
| **Descriptive** | `describe()`, `frequency()`, `crosstab()` | Summaries and distributions |
| **T-Tests** | `t_test()` | Mean comparisons (independent, paired, one-sample) |
| **ANOVA** | `oneway_anova()` | Multiple group comparisons |
| **Non-parametric** | `mann_whitney()`, `kruskal_wallis()`, `wilcoxon_test()`, `friedman_test()`, `binomial_test()` | Distribution-free tests |
| **Correlation** | `pearson_cor()`, `spearman_rho()`, `kendall_tau()` | Relationships between variables |
| **Post-hoc** | `tukey_test()`, `scheffe_test()`, `levene_test()` | Follow-up analyses |
| **Chi-square** | `chi_square()` | Categorical associations |
| **Scale analysis** | `reliability()`, `efa()`, `scale_index()`, `pomps()` | Cronbach's Alpha, factor analysis, index construction |
| **Regression** | `linear_regression()`, `logistic_regression()` | Linear and logistic models with SPSS-style output |
| **Weighted stats** | `w_mean()`, `w_median()`, `w_sd()`, + 8 more | Individual weighted statistics |

### Survey Weights Built-In

Every function handles survey weights correctly:

```r
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

```r
survey_data %>%
  filter(age >= 18) %>%
  group_by(region) %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)
```

### S3 Post-Hoc Methods

```r
# Run ANOVA, then chain post-hoc tests
result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education, weights = sampling_weight)

result %>% tukey_test()    # Pairwise comparisons
result %>% levene_test()   # Variance homogeneity
```

## SPSS Compatibility

Every function is validated against SPSS v29 across four scenarios: weighted/unweighted and grouped/ungrouped. If you're migrating from SPSS, your results will match:

**SPSS:**
```spss
WEIGHT BY sampling_weight.
T-TEST GROUPS=gender(1 2)
  /VARIABLES=satisfaction.
```

**mariposa:**
```r
survey_data %>%
  t_test(satisfaction, group = gender, weights = sampling_weight)
```

## Documentation

- [Complete Reference](https://YannickDiehl.github.io/mariposa/reference/) - All functions with examples
- [Getting Started](https://YannickDiehl.github.io/mariposa/articles/introduction.html) - Introduction and first steps
- [Scale Analysis](https://YannickDiehl.github.io/mariposa/articles/scale-analysis.html) - Reliability, factor analysis, and scale construction
- [Regression Analysis](https://YannickDiehl.github.io/mariposa/articles/regression-analysis.html) - Linear and logistic regression
- [Survey Weights Guide](https://YannickDiehl.github.io/mariposa/articles/survey-weights.html) - Working with weighted data

## Support

- [GitHub Issues](https://github.com/YannickDiehl/mariposa/issues) - Bug reports and feature requests
- [GitHub Discussions](https://github.com/YannickDiehl/mariposa/discussions) - Questions and ideas

## License

MIT - Yannick Diehl
