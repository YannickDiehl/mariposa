# mariposa Package Development Guide

## 🎯 Quick Start

mariposa is a comprehensive R package for professional statistical analysis of survey data, designed as a production-ready framework with SPSS-compatible results.

### Package Purpose
- **Primary Goal**: Provide SPSS-compatible statistical analysis for survey researchers migrating from SPSS to R
- **Key Differentiator**: 100% validated results matching SPSS output (83/83 tests pass)
- **Target Users**: Survey researchers, data scientists, statistical analysts working with complex survey designs

### Core Capabilities
- 46+ statistical functions across 6 categories
- Full survey weight support with 11 specialized `w_*` functions
- Tidyverse integration (dplyr, tidyselect, group_by support)
- S3 generics system for extensible post-hoc analysis
- Professional CLI-formatted output (via `cli` package + ASCII borders)

## 📁 Project Structure

```
mariposa/
├── R/                          # Source code (31 function files)
│   ├── describe.R              # Descriptive statistics
│   ├── frequency.R             # Frequency tables
│   ├── crosstab.R              # Cross-tabulation analysis
│   ├── t_test.R                # T-tests (paired/independent)
│   ├── oneway_anova_test.R     # One-way ANOVA
│   ├── chi_squared_test.R      # Chi-square tests
│   ├── mann_whitney_test.R     # Non-parametric tests
│   ├── pearson_cor.R           # Pearson correlation
│   ├── spearman_rho.R          # Spearman correlation
│   ├── kendall_tau.R           # Kendall's tau
│   ├── tukey_test.R            # Post-hoc Tukey HSD
│   ├── scheffe_test.R          # Post-hoc Scheffe test
│   ├── levene_test.R           # Homogeneity of variance
│   ├── w_*.R                   # Weighted statistics (11 files)
│   ├── helpers.R               # Utility functions
│   ├── globals.R               # Global variables
│   └── imports.R               # Package imports
│
├── man/                        # Documentation (auto-generated)
├── tests/                      # Test suite
│   ├── testthat/              # Unit tests with SPSS validation
│   └── spss_reference/        # SPSS reference data
│
├── vignettes/                  # User guides (5 comprehensive guides)
│   ├── introduction.Rmd        # Getting started guide
│   ├── descriptive-statistics.Rmd
│   ├── hypothesis-testing.Rmd
│   ├── correlation-analysis.Rmd
│   └── survey-weights.Rmd
│
├── data/                       # Example datasets
│   ├── survey_data.rda         # 2,500 respondent synthetic survey
│   └── longitudinal_data.rda   # Repeated measures data
│
├── .claude/                    # Development resources (flat structure)
│   ├── CLAUDE.md              # This file - main development guide
│   ├── spss-validation-guide.md  # SPSS validation process guide
│   ├── spss-validation-test.R    # Test template for SPSS validation
│   ├── spss-syntax-template.sps  # SPSS syntax template
│   └── settings.local.json       # Local Claude settings
│
└── _pkgdown.yml               # Documentation website config
```

## 🛠️ Development Guidelines

### Code Style & Conventions

1. **Function Naming**
   - Descriptive verbs: `describe()`, `frequency()`, `t_test()`
   - Weighted functions prefix: `w_mean()`, `w_median()`
   - S3 methods: `tukey_test()`, `levene_test()`

2. **Parameter Conventions**
   - `data`: Always first parameter (tibble/data.frame)
   - `...`: Variables to analyze (tidyselect support)
   - `weights`: Optional survey weights
   - `group`: Grouping variable for comparisons
   - `na.rm`: Handle missing values (default TRUE)

3. **Output Format**
   - Return tibble with consistent structure
   - Include class for S3 dispatch
   - Standardized print methods via `print_helpers.R` (see `.claude/PRINT_METHOD_STYLE_GUIDE.md`)
   - `cli` package for headers/separators, ASCII `-` for table borders
   - SPSS-compatible statistics names

4. **Documentation Standards**
   - User-friendly descriptions (avoid technical jargon)
   - Practical examples with survey_data
   - "Understanding the Output" sections
   - "When to Use This" guidance

### Testing Approach

**SPSS Validation Framework**
- Every statistical function validated against SPSS
- Four test scenarios: weighted/unweighted × grouped/ungrouped
- Tolerance-based testing:
  - Counts: Exact match (tolerance = 0)
  - Percentages: ±0.1
  - Test statistics: ±0.00001
  - Weighted totals: ±1

**Test Organization**
```r
# Each function has corresponding SPSS validation test
tests/testthat/test-{function}-spss-validation.R
```

### Adding New Functions

1. **Create function file in R/**
```r
#' User-friendly title
#' @description Clear explanation
#' @param data Your survey data
#' @param ... Variables to analyze
#' @param weights Survey weights
#' @export
my_function <- function(data, ..., weights = NULL) {
  # Implementation
}
```

2. **Add SPSS validation test**
   - Use template: `.claude/spss-validation-test.R`
   - Generate SPSS reference values
   - Test all four scenarios

3. **Create print method**
```r
#' @export
print.my_function_result <- function(x, ...) {
  # Use helpers from print_helpers.R (see PRINT_METHOD_STYLE_GUIDE.md)
}
```

4. **Update documentation**
   - Run `devtools::document()`
   - Add to appropriate vignette
   - Update _pkgdown.yml reference section

## 📋 Common Tasks

### Running Tests
```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-t-test-spss-validation.R")

# Check package
devtools::check()
```

### Building Documentation
```r
# Generate man pages
devtools::document()

# Build vignettes
devtools::build_vignettes()

# Build pkgdown site
pkgdown::build_site()
```

### Package Building
```r
# Build package
devtools::build()

# Install locally
devtools::install()

# Check before CRAN submission
devtools::check(cran = TRUE)
```

## 🔑 Key Design Patterns

### Weighted Statistics Pattern
```r
w_function <- function(data, ..., weights = NULL, na.rm = TRUE) {
  # 1. Handle both vector and data frame inputs
  # 2. Validate weights
  # 3. Calculate weighted statistic
  # 4. Return consistent structure with effective_n
}
```

### Group-By Support Pattern
```r
# All functions automatically support grouped operations
data %>%
  group_by(region) %>%
  describe(age, weights = sampling_weight)
```

### S3 Generic Pattern
```r
# Primary function creates classed result
result <- oneway_anova_test(...)
class(result) <- c("anova_test_result", "data.frame")

# S3 methods dispatch on result class
tukey_test(result)  # Works automatically
```

### Print Method Pattern
```r
# All print methods use helpers from print_helpers.R:
# print_header() → cli_rule() with standardized title
# print_info_section() → cli_bullets() for test metadata
# add_significance_stars() → consistent *** ** * formatting
# print_group_header() → cli_h2() for grouped analyses
# ASCII "-" for table borders, no Unicode box-drawing characters
# See .claude/PRINT_METHOD_STYLE_GUIDE.md for full details
```

## 🧪 SPSS Validation

### Validation Process
1. Generate SPSS syntax using template: `.claude/spss-syntax-template.sps`
2. Extract reference values from SPSS output
3. Structure values in test file
4. Apply appropriate tolerances
5. Test all four scenarios

### Key Resources
- **Validation Guide**: `.claude/spss-validation-guide.md`
- **Test Template**: `.claude/spss-validation-test.R`
- **SPSS Syntax Template**: `.claude/spss-syntax-template.sps`

### Tolerance Guidelines
| Value Type | Tolerance | Reason |
|------------|-----------|---------|
| Counts | 0 | Exact match required |
| Percentages | ±0.1 | Display rounding |
| Test statistics | ±0.00001 | Calculation precision |
| Weighted totals | ±1 | SPSS display quirks |
| P-values | ±0.0001 | Rounding differences |

## 📊 Function Categories

### 1. Descriptive Statistics (3 functions)
- `describe()` - Comprehensive numeric summaries
- `frequency()` - Categorical frequency tables
- `crosstab()` - Cross-tabulations with percentages

### 2. Hypothesis Testing (6 functions)
- `t_test()` - Independent/paired t-tests
- `oneway_anova()` - One-way ANOVA
- `mann_whitney()` - Non-parametric alternative
- `chi_square()` - Independence tests
- `rm_t_test()` - Repeated measures t-test
- `rm_anova_test()` - Repeated measures ANOVA

### 3. Correlation Analysis (3 functions)
- `pearson_cor()` - Linear correlation
- `spearman_rho()` - Rank correlation
- `kendall_tau()` - Ordinal correlation

### 4. Post-Hoc Analysis (5 S3 generics)
- `tukey_test()` - Tukey HSD comparisons
- `scheffe_test()` - Scheffe comparisons
- `levene_test()` - Variance homogeneity
- `emmeans()` - Estimated marginal means
- `mauchly_test()` - Sphericity test

### 5. Weighted Statistics (11 functions)
- `w_mean()`, `w_median()`, `w_sd()`, `w_var()`
- `w_quantile()`, `w_iqr()`, `w_range()`
- `w_skew()`, `w_kurtosis()`, `w_se()`, `w_modus()`

### 6. Print Methods (20+ methods)
- Professional output formatting for all result types

### 7. Planned for v0.2.0 (Scale Analysis & Regression)
- `reliability()` - Cronbach's Alpha with item statistics (genuine implementation)
- `efa()` - Exploratory Factor Analysis with PCA, Varimax, Oblimin (hybrid: Base R + GPArotation)
- `scale_index()` - Mean index across items (genuine implementation)
- `pomps()` - Percent of Maximum Possible Scores transformation (genuine implementation)
- `linear_regression()` - Linear regression with SPSS-style output (wrapper around `stats::lm()`)
- `logistic_regression()` - Logistic regression with odds ratios (wrapper around `stats::glm()`)
- See [Implementation Roadmap](IMPLEMENTATION_ROADMAP.md) for full details

## 🚀 Build & Release

### GitHub Actions Workflows
- **R-CMD-check.yaml**: Multi-platform package checks
- **test-coverage.yaml**: Code coverage reporting
- **pkgdown.yaml**: Documentation website deployment

### Pre-Release Checklist
- [ ] All tests pass (`devtools::test()`)
- [ ] R CMD check clean (`devtools::check()`)
- [ ] Documentation complete (`devtools::document()`)
- [ ] Vignettes build (`devtools::build_vignettes()`)
- [ ] NEWS.md updated
- [ ] Version bumped in DESCRIPTION
- [ ] SPSS validation complete (83/83 tests)

### CRAN Submission
```r
# Final checks
devtools::check(cran = TRUE)
rhub::check_for_cran()

# Submit
devtools::release()
```

## 📚 Important Links

### Internal Resources (in .claude/)
- [Implementation Roadmap v0.2.0](IMPLEMENTATION_ROADMAP.md) - Strategic plan for new functions
- [SPSS Validation Guide](spss-validation-guide.md)
- [Test Template](spss-validation-test.R)
- [SPSS Syntax Template](spss-syntax-template.sps)
- [Print Method Style Guide](PRINT_METHOD_STYLE_GUIDE.md)

### External Resources
- [Package Website](https://yourusername.github.io/mariposa/)
- [GitHub Repository](https://github.com/YannickDiehl/mariposa)
- [Issue Tracker](https://github.com/YannickDiehl/mariposa/issues)

### R Package Development
- [R Packages Book](https://r-pkgs.org/)
- [Tidyverse Style Guide](https://style.tidyverse.org/)
- [pkgdown Documentation](https://pkgdown.r-lib.org/)

## 💡 Development Tips

1. **Always validate against SPSS** - Users expect identical results
2. **Test with weights** - Core differentiator of the package
3. **Support group_by** - Tidyverse integration is key
4. **Format output professionally** - First impressions matter
5. **Document for non-statisticians** - Clear, practical language
6. **Handle missing data gracefully** - Common in survey data
7. **Provide helpful error messages** - Guide users to solutions

## 🐛 Debugging

### Common Issues
1. **Weight validation failures**: Check `.validate_weights()` in helpers.R
2. **Group-by not working**: Ensure `grouped_df` handling in function
3. **SPSS mismatch**: Review tolerance levels and SPSS display rounding
4. **Output formatting issues**: Check `print_helpers.R` and `cli` package behavior

### Debugging Tools
```r
# Enable verbose output
options(mariposa.verbose = TRUE)

# Check intermediate calculations
debug(function_name)

# Trace weight handling
trace(.validate_weights, browser)
```

## 📝 Version History

- **0.1.0**: Initial release with 27 exported functions, full SPSS validation (83/83 tests)
- **0.2.0** (planned): Scale analysis (reliability, EFA) + regression (linear, logistic)
- Future: Non-parametric extensions, factorial ANOVA, cluster analysis

---

*Last updated: February 2026*
*Maintained by: Yannick Diehl*