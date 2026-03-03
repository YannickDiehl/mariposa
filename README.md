# mariposa <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/YannickDiehl/mariposa/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/YannickDiehl/mariposa/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/YannickDiehl/mariposa/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/YannickDiehl/mariposa/actions/workflows/test-coverage.yaml)
[![Codecov test coverage](https://codecov.io/gh/YannickDiehl/mariposa/branch/main/graph/badge.svg)](https://app.codecov.io/gh/YannickDiehl/mariposa?branch=main)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
<!-- badges: end -->

**Professional statistical analysis for survey data in R.**

mariposa (*Marburg Initiative for Political and Social Analysis*) provides 44 statistical functions for analyzing survey data. All functions support survey weights, grouped analysis via `dplyr::group_by()`, and produce publication-ready output. Results are validated against SPSS v29 for full reproducibility (4,986+ tests pass).

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
| **ANOVA** | `oneway_anova()`, `factorial_anova()`, `ancova()` | One-way, multi-factor ANOVA, and ANCOVA with Type III SS |
| **Non-parametric** | `mann_whitney()`, `kruskal_wallis()`, `wilcoxon_test()`, `friedman_test()`, `binomial_test()` | Distribution-free tests |
| **Exact tests** | `chi_square()`, `fisher_test()`, `chisq_gof()`, `mcnemar_test()` | Categorical associations and exact tests |
| **Correlation** | `pearson_cor()`, `spearman_rho()`, `kendall_tau()` | Relationships between variables |
| **Post-hoc** | `tukey_test()`, `scheffe_test()`, `levene_test()`, `dunn_test()`, `pairwise_wilcoxon()` | Follow-up analyses (parametric and non-parametric) |
| **Scale analysis** | `reliability()`, `efa()`, `scale_index()`, `pomps()` | Cronbach's Alpha, factor analysis, index construction |
| **Regression** | `linear_regression()`, `logistic_regression()` | Linear and logistic models with SPSS-style output |
| **Effect sizes** | `phi()`, `cramers_v()`, `goodman_gamma()` | Effect size measures for categorical data |
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

### Multi-Factor ANOVA & ANCOVA

```r
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

```r
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

Every analysis function provides two output levels. Typing the result name prints a compact one-line summary. Calling `summary()` produces the full SPSS-style output with all details. You can toggle individual sections on or off:

```r
result <- t_test(survey_data, life_satisfaction, group = gender)
result              # compact one-line summary
summary(result)     # full detailed output
summary(result, effect_sizes = FALSE)  # hide effect sizes
```

This works for all 13 analysis functions — `t_test()`, `oneway_anova()`, `chi_square()`, `pearson_cor()`, `reliability()`, `linear_regression()`, and more.

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
