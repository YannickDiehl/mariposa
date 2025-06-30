# SurveyStat <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/yourusername/SurveyStat/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yourusername/SurveyStat/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/yourusername/SurveyStat/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/yourusername/SurveyStat/actions/workflows/test-coverage.yaml)
[![Codecov test coverage](https://codecov.io/gh/yourusername/SurveyStat/branch/main/graph/badge.svg)](https://app.codecov.io/gh/yourusername/SurveyStat?branch=main)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

## Overview

**SurveyStat** is a comprehensive R package for professional statistical analysis of survey data. Designed as a production-ready framework for survey researchers, data scientists, and statistical analysts working with complex survey designs.

### 🎯 **Core Features**

- **🔬 SPSS-Compatible Results**: 100% identical statistical output with SPSS
- **⚖️ Survey-Weighted Statistics**: Mathematically correct weighted analysis  
- **🔄 Tidyverse Integration**: Seamless workflow with dplyr, tidyselect, group_by()
- **🧩 S3 Generics System**: Extensible architecture for cross-test compatibility
- **🎨 Professional Output**: Unicode boxes, dynamic borders, publication-ready formatting

### 📊 **Statistical Framework**

**46+ Functions across 6 core categories:**

| Category | Functions | Description |
|----------|-----------|-------------|
| 🎯 **Descriptive** | `describe()`, `frequency()` | Core descriptive and frequency analysis |
| 📈 **Hypothesis Testing** | `t_test()`, `oneway_anova_test()`, `mann_whitney_test()`, `chi_squared_test()` | Independent and paired sample tests |
| 🔍 **Post-Hoc Analysis** | `tukey_test()`, `emmeans()`, `levene_test()`, `mauchly_test()` | S3 generics for extended analysis |
| 📊 **Weighted Statistics** | 11 `w_*` functions | Survey-ready weighted statistical functions |
| 🧪 **Advanced Testing** | `rm_t_test()`, `rm_anova_test()` | Repeated measures and longitudinal analysis |
| 🎨 **Professional Output** | Dynamic print methods | Unicode formatting, adaptive borders |

## Installation

Install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("yourusername/SurveyStat")
```

## Quick Start

Load the package and example survey data:

```r
library(SurveyStat)
library(dplyr)

# Load built-in survey dataset
data(survey_data)
glimpse(survey_data)
```

## Examples

### 🎯 **Descriptive Statistics**

```r
# Basic descriptive analysis with weights
survey_data %>% 
  describe(age, income, life_satisfaction, weights = sampling_weight)

# Frequency analysis for categorical variables
survey_data %>% 
  frequency(gender, region, education, weights = sampling_weight)
```

### 📈 **Hypothesis Testing**

```r
# Independent samples t-test with weights
survey_data %>% 
  t_test(life_satisfaction, group = gender, weights = sampling_weight)

# One-way ANOVA with effect sizes
survey_data %>% 
  oneway_anova_test(life_satisfaction, group = education, weights = sampling_weight)

# Non-parametric Mann-Whitney test  
survey_data %>%
  mann_whitney_test(age, group = gender, weights = sampling_weight)
```

### 🔍 **Post-Hoc Analysis Pipeline**

```r
# Complete analysis pipeline with S3 generics
result <- survey_data %>%
  oneway_anova_test(life_satisfaction, group = education, weights = sampling_weight)

# Seamless post-hoc analysis
result %>% tukey_test()    # Multiple comparisons
result %>% levene_test()   # Homogeneity of variance  
result %>% emmeans()       # Estimated marginal means
```

### 📊 **Weighted Statistics**

```r
# Individual weighted statistics
survey_data %>% w_mean(age, income, weights = sampling_weight)
survey_data %>% w_sd(age, income, weights = sampling_weight) 
survey_data %>% w_median(age, income, weights = sampling_weight)

# Grouped weighted analysis
survey_data %>% 
  group_by(region) %>% 
  w_mean(age, life_satisfaction, weights = sampling_weight)
```

### 🔄 **Grouped Analysis**

```r
# Automatic grouped operations with any function
survey_data %>%
  group_by(region, education) %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)

# Complex survey design analysis
survey_data %>%
  group_by(region) %>%
  oneway_anova_test(income, group = education, weights = sampling_weight) %>%
  tukey_test()
```

## Professional Output

SurveyStat produces publication-ready output with professional formatting:

```
Weighted t-Test Results
-----------------------

┌─ life_satisfaction ─┐

Group: gender (Male vs. Female)
  Male: mean = 6.89, sd = 1.85, n = 1204.4
  Female: mean = 7.12, sd = 1.79, n = 1264.4

Weighted t-Test Results:
----------------------------------------------
Test         t      df    p_value  d_cohen  sig
t-test   -2.456  2466.8     0.014    0.125    *
----------------------------------------------

Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

## Survey Design Support

**Built for real-world survey data:**

- ✅ **Sampling Weights**: Mathematically correct weighted statistics
- ✅ **Complex Survey Designs**: Stratification, clustering, post-stratification  
- ✅ **Missing Data Handling**: Robust NA treatment across all functions
- ✅ **Large Datasets**: Optimized for surveys with 10,000+ respondents
- ✅ **SPSS Migration**: Drop-in replacement with identical results

## Comparison with Other Packages

| Feature | SurveyStat | Base R | survey | sjstats |
|---------|------------|--------|--------|---------|
| **SPSS-Compatible Results** | ✅ | ❌ | ⚠️ | ⚠️ |
| **Weighted Statistics** | ✅ | ❌ | ✅ | ✅ |
| **Tidyverse Integration** | ✅ | ❌ | ❌ | ✅ |
| **S3 Generics Pipeline** | ✅ | ❌ | ❌ | ❌ |
| **Professional Output** | ✅ | ❌ | ❌ | ⚠️ |
| **Grouped Operations** | ✅ | ⚠️ | ❌ | ⚠️ |

## Documentation

- 📖 **[Function Reference](https://yourusername.github.io/SurveyStat/reference/)**: Complete documentation with examples
- 🎓 **[Getting Started Guide](https://yourusername.github.io/SurveyStat/articles/introduction.html)**: Step-by-step tutorials
- 📊 **[Advanced Analysis](https://yourusername.github.io/SurveyStat/articles/advanced-analysis.html)**: Complex survey designs
- 🔧 **[SPSS Migration](https://yourusername.github.io/SurveyStat/articles/spss-migration.html)**: Side-by-side comparisons

## Getting Help

- 🐛 **Bug Reports**: [GitHub Issues](https://github.com/yourusername/SurveyStat/issues) with minimal reproducible examples
- 💡 **Feature Requests**: [GitHub Discussions](https://github.com/yourusername/SurveyStat/discussions) for new functionality
- 📧 **Contact**: Professional support available for enterprise users

## Contributing

We welcome contributions! Please see our [Contributing Guide](CONTRIBUTING.md) for details on:

- Code style and standards
- Testing requirements  
- Documentation guidelines
- Development workflow

## Citation

```r
citation("SurveyStat")
```

## License

MIT License - see [LICENSE](LICENSE) file for details.

---

**SurveyStat** - Professional Statistical Analysis for Survey Data  
*Developed with ❤️ for survey researchers*