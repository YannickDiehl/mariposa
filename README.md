# *mariposa*: marburg initiative for political and social analysis <img src="man/figures/logo.png" align="right" height="180" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/YannickDiehl/mariposa/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/YannickDiehl/mariposa/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/YannickDiehl/mariposa/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/YannickDiehl/mariposa/actions/workflows/test-coverage.yaml)
[![Codecov test coverage](https://codecov.io/gh/YannickDiehl/mariposa/branch/main/graph/badge.svg)](https://app.codecov.io/gh/YannickDiehl/mariposa?branch=main)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## 🎯 SPSS → R Migration Made Simple

**Finally, R analysis that matches SPSS exactly.**

mariposa delivers 100% SPSS-compatible results for survey researchers migrating to R. No more explaining why your R numbers don't match SPSS. No more parallel analyses. Just identical results, every time.

```r
# Your SPSS workflow, now in R
survey_data %>%
  t_test(satisfaction, group = gender, weights = weight)

# ✓ Same test statistics
# ✓ Same p-values
# ✓ Same confidence intervals
# ✓ Same weighted calculations
```

## Why mariposa?

### The Migration Challenge

Moving from SPSS to R typically means:

🔴 Results that don't match 
🔴 Rewriting all validation procedures
🔴 Learning complex survey packages with different outputs
🔴 Losing confidence in your analyses

### The mariposa Solution

✅ **100% SPSS-validated** - Statistical tests match
✅ **Familiar workflow** - Functions work like you expect
✅ **Survey-ready** - Built-in weight handling for every function
✅ **Production-quality output** - Publication-ready tables with proper formatting
✅ **Tidyverse native** - Modern R workflow with dplyr integration

## Installation

```r
# Install from GitHub
devtools::install_github("YannickDiehl/mariposa")

# Load the package
library(mariposa)
library(dplyr)  # For modern workflow
```

## Quick Start

### Your First Analysis

```r
# Load example survey data (2,500 respondents)
data(survey_data)

# Descriptive statistics with survey weights
survey_data %>%
  describe(age, income, satisfaction, weights = sampling_weight)

# Frequency table for categorical data
survey_data %>%
  frequency(education, weights = sampling_weight)

# Compare groups with t-test
survey_data %>%
  t_test(satisfaction, group = gender, weights = sampling_weight)
```

### Professional Output

```
Weighted t-Test Results
─────────────────────────

Group: gender (Male vs. Female)
  Male:   mean = 6.89, sd = 1.85, n = 1,204.4
  Female: mean = 7.12, sd = 1.79, n = 1,264.4

Test Statistics:
────────────────────────────────────────────
Test      t      df      p_value  d_cohen  sig
────────────────────────────────────────────
t-test  -2.456  2466.8    0.014    0.125    *
────────────────────────────────────────────

Significance codes: *** p<0.001, ** p<0.01, * p<0.05
```

## Core Features

### 📊 Statistical Functions (46+)

| Category | Functions | Purpose |
|----------|-----------|---------|
| **Descriptive** | `describe()`, `frequency()`, `crosstab()` | Summaries and distributions |
| **T-Tests** | `t_test()` | Mean comparisons |
| **ANOVA** | `oneway_anova()` | Multiple group analysis |
| **Non-parametric** | `mann_whitney()` | Distribution-free tests |
| **Correlation** | `pearson_cor()`, `spearman_rho()`, `kendall_tau()` | Relationships |
| **Post-hoc** | `tukey_test()`, `scheffe_test()`, `levene_test()` | Follow-up analyses |
| **Chi-square** | `chi_square()`, `phi()`, `cramers_v()`, `gamma()` | Categorical associations |

### ⚖️ Survey Weights Built-In

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

### 🔄 Modern R Workflow

Full tidyverse integration:

```r
# Complex analysis pipeline
survey_data %>%
  filter(age >= 18) %>%
  group_by(region, education) %>%
  t_test(satisfaction, group = gender, weights = sampling_weight) %>%
  filter(p_value < 0.05)
```

### 🧩 S3 Methods for Extended Analysis

Seamless post-hoc testing:

```r
# Run ANOVA
anova_result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education, weights = sampling_weight)

# Automatic post-hoc tests
anova_result %>% tukey_test()    # Pairwise comparisons
anova_result %>% levene_test()   # Variance homogeneity
```

## SPSS Compatibility

### Validated Against SPSS v29

Every function tested across four scenarios:

✓ Unweighted, ungrouped
✓ Weighted, ungrouped
✓ Unweighted, grouped
✓ Weighted, grouped

### Migration Example

**SPSS Syntax:**
```spss
WEIGHT BY sampling_weight.
T-TEST GROUPS=gender(1 2)
  /VARIABLES=satisfaction.
```

**mariposa Equivalent:**
```r
survey_data %>%
  t_test(satisfaction, group = gender, weights = sampling_weight)
```

**Result: Identical output, guaranteed.**

## Documentation

- 📖 [Complete Reference](https://yanndiehl.github.io/mariposa/reference/) - All functions with examples
- 🎓 [Tutorials](https://yanndiehl.github.io/mariposa/articles/) - Step-by-step guides
- 🔄 [SPSS Migration Guide](https://yanndiehl.github.io/mariposa/articles/spss-migration.html) - Side-by-side comparisons
- 📊 [Survey Weights Guide](https://yanndiehl.github.io/mariposa/articles/survey-weights.html) - Weight handling explained

## Support

- 🐛 **Issues**: [GitHub Issues](https://github.com/YannickDiehl/mariposa/issues)
- 💡 **Discussions**: [GitHub Discussions](https://github.com/YannickDiehl/mariposa/discussions)


## License

MIT © Yannick Diehl

---

**mariposa** - When your R results must match SPSS exactly.