# SurveyStat 0.1.1

## Breaking changes

* Renamed functions for consistency and clarity:
  - `oneway_anova_test()` is now `oneway_anova()`
  - `mann_whitney_test()` is now `mann_whitney()`
  - `chi_squared_test()` is now `chi_square()`
* Class names updated accordingly:
  - `oneway_anova_test_results` is now `oneway_anova_results`
  - `mann_whitney_test_results` is now `mann_whitney_results`
  - `chi_squared_test_results` is now `chi_square_results`

# SurveyStat 0.1.0

* Initial CRAN submission
* Core features:
  - Comprehensive survey statistics functions
    - `describe()`: Descriptive statistics with survey weights support
    - `frequency()`: Frequency tables with percentage calculations
  - Hypothesis testing suite
    - `t_test()`: One-sample and two-sample t-tests
    - `oneway_anova()`: One-way ANOVA with effect sizes
    - `chi_square()`: Chi-square test with Cramér's V and Phi
    - `mann_whitney()`: Non-parametric alternative to t-test
  - Post-hoc analysis functions
    - `tukey_test()`: Tukey HSD post-hoc test
    - `scheffe_test()`: Scheffé post-hoc test
    - `levene_test()`: Test for homogeneity of variances
  - Correlation analysis
    - `pearson_cor()`: Pearson correlation with significance tests
    - `spearman_rho()`: Spearman rank correlation
    - `kendall_tau()`: Kendall's tau correlation
  - Survey-weighted statistics (11 functions)
    - `w_mean()`, `w_median()`, `w_sd()`, `w_var()`
    - `w_quantile()`, `w_iqr()`, `w_range()`
    - `w_skew()`, `w_kurtosis()`, `w_se()`, `w_modus()`
  - Cross-tabulation analysis
    - `crosstab()`: Cross-tabulation with row/column/cell percentages
* SPSS-compatible statistical results
* Full tidyverse integration with dplyr and tidyselect
* Support for grouped operations via `group_by()`
* Comprehensive test coverage with SPSS validation
* Example datasets included:
  - `survey_data`: Simulated survey dataset
  - `longitudinal_data`: Repeated measures dataset
  - `longitudinal_data_wide`: Wide format longitudinal data