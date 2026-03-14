# Introduction to mariposa

``` r
library(mariposa)
library(dplyr)
```

## What is mariposa?

mariposa (*Marburg Initiative for Political and Social Analysis*) is a
comprehensive R package for professional survey data analysis. It covers
the entire workflow — from importing SPSS, Stata, SAS, and Excel files
through label management, recoding, and standardization to statistical
analysis with survey weights and publication-ready output.

Every statistical result is validated against SPSS v29, so researchers
migrating from SPSS can trust their numbers.

### Key Features

- **76 functions** across 15 categories
- **Full data pipeline**: import → labels → transformation → analysis →
  export
- **Survey weights** built into every function
- **Tidyverse integration**: pipes (`%>%`),
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
  tidyselect
- **Two-level output**: compact
  [`print()`](https://rdrr.io/r/base/print.html) and detailed
  [`summary()`](https://rdrr.io/r/base/summary.html)
- **SPSS-validated**: 4,986+ tests ensure results match SPSS v29

## The Example Dataset

mariposa includes `survey_data`, a synthetic survey of 2,500 respondents
with demographics, attitudes, and a sampling weight:

``` r
data(survey_data)
glimpse(survey_data)
#> Rows: 2,500
#> Columns: 16
#> $ id                    <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1…
#> $ age                   <dbl> 68, 58, 48, 46, 71, 73, 60, 48, 28, 30, 20, 58, …
#> $ gender                <fct> Female, Male, Male, Female, Male, Female, Male, …
#> $ region                <fct> East, West, West, West, West, East, East, West, …
#> $ education             <ord> Intermediate Secondary, Academic Secondary, Acad…
#> $ income                <dbl> 3500, 4800, 3500, 2600, 3000, 5200, 3200, NA, 37…
#> $ employment            <fct> Retired, Employed, Employed, Employed, Retired, …
#> $ political_orientation <int> 2, 3, 3, 5, 1, NA, NA, 3, 4, 4, 2, 2, 1, 3, 3, 1…
#> $ environmental_concern <int> 3, 5, 3, 2, NA, NA, 5, 4, 2, 4, 3, 5, 4, 4, 3, 3…
#> $ life_satisfaction     <int> 4, 3, 2, 2, 4, 4, 3, 3, 4, 3, 1, 1, 2, 5, 3, 2, …
#> $ trust_government      <int> 3, 4, 1, 1, 2, 1, 3, 3, 4, 3, NA, 4, 3, 3, 2, 2,…
#> $ trust_media           <int> 3, 3, 3, 2, 4, 4, 4, 1, 4, 2, 2, 1, 1, 3, 3, 3, …
#> $ trust_science         <int> 2, 4, 4, 1, 3, 5, 5, 3, 3, 4, 4, 3, 5, 4, 3, 4, …
#> $ sampling_weight       <dbl> 1.2690774, 0.8926824, 1.0424119, 1.0024385, 1.02…
#> $ stratum               <fct> East_Old, West_Old, West_Middle, West_Middle, We…
#> $ interview_mode        <fct> Face-to-face, Face-to-face, Online, Telephone, T…
```

All examples in this guide use this dataset.

## Five-Minute Tour

Here is a complete analysis workflow showing what mariposa can do:

### 1. Explore the Data

``` r
# Find variables related to "trust"
find_var(survey_data, "trust")
#>   col             name                                    label
#> 1  11 trust_government Trust in government (1=none, 5=complete)
#> 2  12      trust_media      Trust in media (1=none, 5=complete)
#> 3  13    trust_science    Trust in science (1=none, 5=complete)
```

``` r
# Descriptive statistics with survey weights
survey_data %>%
  describe(age, income, life_satisfaction, weights = sampling_weight)
#> 
#> Weighted Descriptive Statistics
#> -------------------------------
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>                age   50.514     50   17.084    77   25    0.159      2468.8
#>             income 3743.099   3500 1423.966  7200 1900    0.724      2158.9
#>  life_satisfaction    3.625      4    1.152     4    2   -0.498      2390.9
#> ----------------------------------------
```

``` r
# Frequency table
survey_data %>%
  frequency(education, weights = sampling_weight)
#> 
#> Weighted Frequency Analysis Results
#> -----------------------------------
#> 
#> education (Highest educational attainment)
#> # total N=2516 valid N=2516 mean=NA sd=NA skewness=NA
#> 
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Value |                  Label |      N |  Raw % |Valid % | Cum. % |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |        Basic Secondary |        Basic Secondary |    848 |  33.71 |  33.71 |  33.71 |
#> | Intermediate Secondary | Intermediate Secondary |    641 |  25.47 |  25.47 |  59.18 |
#> |     Academic Secondary |     Academic Secondary |    642 |  25.51 |  25.51 |  84.69 |
#> |             University |             University |    385 |  15.31 |  15.31 | 100.00 |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Total |                        |   2516 | 100.00 | 100.00 |        |
#> +------------------------+------------------------+--------+--------+--------+--------+
```

### 2. Transform Variables

``` r
# Create age groups
survey_data <- rec(survey_data, age,
  rules = "18:29=1 [Young]; 30:49=2 [Middle]; 50:99=3 [Older]",
  suffix = "_group", as.factor = TRUE)

# Build a trust scale
survey_data <- survey_data %>%
  mutate(m_trust = row_means(., trust_government, trust_media, trust_science,
                             min_valid = 2))
```

### 3. Compare Groups

``` r
# t-test with survey weights
survey_data %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)
#> t-Test: life_satisfaction by gender [Weighted]
#>   t(2390.8) = -1.069, p = 0.285 , g = -0.043 (negligible), N = 2436
```

``` r
# ANOVA across education levels
result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education, weights = sampling_weight)
result
#> One-Way ANOVA: life_satisfaction by education [Weighted]
#>   F(3, 2433) = 65.359, p < 0.001 ***, eta2 = 0.075 (medium), N = 2437
```

Every result has a detailed view with
[`summary()`](https://rdrr.io/r/base/summary.html):

``` r
summary(result, descriptives = FALSE)
#> Weighted One-Way ANOVA Results
#> ------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - Weights variable: sampling_weight
#> - Confidence level: 95.0%
#>   Null hypothesis: All group means are equal
#>   Alternative hypothesis: At least one group mean differs
#> 
#> 
#> --- life_satisfaction ---
#> 
#> 
#> Weighted ANOVA Results:
#> -------------------------------------------------------------------------------- 
#>          Source Sum_Squares   df Mean_Square      F p_value sig
#>  Between Groups     241.130    3      80.377 65.359   <.001   1
#>   Within Groups    2992.019 2433        1.23                   
#>           Total    3233.149 2436                               
#> -------------------------------------------------------------------------------- 
#> 
#> Assumption Tests:
#> ---------------- 
#>  Assumption Statistic df1  df2 p_value sig
#>       Welch    62.636   3 1216   <.001 ***
#> 
#> Effect Sizes:
#> ------------ 
#>           Variable Eta_Squared Epsilon_Squared Omega_Squared Effect_Size
#>  life_satisfaction       0.075           0.073         0.073      medium
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation:
#> - Eta-squared: Proportion of variance explained (biased upward)
#> - Epsilon-squared: Less biased than eta-squared
#> - Omega-squared: Unbiased estimate (preferred for publication)
#> - Small effect: eta-squared ~ 0.01, Medium effect: eta-squared ~ 0.06, Large effect: eta-squared ~ 0.14
#> 
#> Post-hoc tests: Use tukey_test() for pairwise comparisons
```

### 4. Post-Hoc Analysis

``` r
# Which education groups differ?
tukey_test(result)
#> Weighted Tukey HSD Post-Hoc Test Results
#> ----------------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - Weights variable: sampling_weight
#> - Confidence level: 95.0%
#>   Family-wise error rate controlled using Tukey HSD
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Weighted Tukey Results:
#> ------------------------------------------------------------------------------------ 
#>                                   Comparison Difference Lower CI Upper CI
#>     Basic Secondary - Intermediate Secondary     -0.490   -0.641   -0.339
#>         Basic Secondary - Academic Secondary     -0.643   -0.795   -0.491
#>                 Basic Secondary - University     -0.832   -1.011   -0.654
#>  Intermediate Secondary - Academic Secondary     -0.153   -0.314    0.008
#>          Intermediate Secondary - University     -0.342   -0.529   -0.156
#>              Academic Secondary - University     -0.189   -0.376   -0.003
#>  p-value Sig
#>    <.001 ***
#>    <.001 ***
#>    <.001 ***
#>    0.071    
#>    <.001 ***
#>    0.046   *
#> ------------------------------------------------------------------------------------ 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for multiple comparisons (family-wise error control)
```

### 5. Measure Relationships

``` r
survey_data %>%
  pearson_cor(age, income, life_satisfaction, weights = sampling_weight)
#> Pearson Correlation: 3 variables [Weighted]
#>   age x income:                  r = -0.005, p = 0.828  
#>   age x life_satisfaction:       r = -0.029, p = 0.150  
#>   income x life_satisfaction:    r = 0.450, p < 0.001 *** 
#>   1/3 pairs significant (p < .05), N = 2201
```

### 6. Build Models

``` r
survey_data %>%
  linear_regression(life_satisfaction ~ age + income + m_trust,
                    weights = sampling_weight)
#> Linear Regression: life_satisfaction ~ age + income + m_trust [Weighted]
#>   R2 = 0.201, adj.R2 = 0.200, F(3, 2109) = 177.00, p < 0.001 ***, N = 2113
```

## Compact vs. Detailed Output

Every analysis function in mariposa provides two output levels:

- **[`print()`](https://rdrr.io/r/base/print.html)** (default): A
  compact one-line summary with the key statistic
- **[`summary()`](https://rdrr.io/r/base/summary.html)**: Full
  SPSS-style output with all details

You can toggle individual sections in the detailed output:

``` r
result <- survey_data %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)

# Compact
result
#> t-Test: life_satisfaction by gender [Weighted]
#>   t(2390.8) = -1.069, p = 0.285 , g = -0.043 (negligible), N = 2436

# Detailed
summary(result)
#> Weighted t-Test Results
#> -----------------------
#> 
#> - Grouping variable: gender
#> - Groups compared: Male vs. Female
#> - Weights variable: sampling_weight
#> - Confidence level: 95.0%
#> - Alternative hypothesis: two.sided
#> - Null hypothesis (mu): 0.000
#> 
#> 
#> --- life_satisfaction ---
#> 
#>   Male: mean = 3.598, n = 1149.0
#>   Female: mean = 3.648, n = 1287.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat       df p_value mean_diff        conf_int sig
#>    Equal variances -1.070 2434.000   0.285     -0.05 [-0.142, 0.042]    
#>  Unequal variances -1.069 2390.755   0.285     -0.05 [-0.142, 0.042]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>           Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>  life_satisfaction   -0.043   -0.043      -0.043  negligible
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation:
#> - Cohen's d: pooled standard deviation (classic)
#> - Hedges' g: bias-corrected Cohen's d (preferred)
#> - Glass' Delta: control group standard deviation only
#> - Small effect: |effect| ~ 0.2
#> - Medium effect: |effect| ~ 0.5
#> - Large effect: |effect| ~ 0.8

# Detailed, skip effect sizes
summary(result, effect_sizes = FALSE)
#> Weighted t-Test Results
#> -----------------------
#> 
#> - Grouping variable: gender
#> - Groups compared: Male vs. Female
#> - Weights variable: sampling_weight
#> - Confidence level: 95.0%
#> - Alternative hypothesis: two.sided
#> - Null hypothesis (mu): 0.000
#> 
#> 
#> --- life_satisfaction ---
#> 
#>   Male: mean = 3.598, n = 1149.0
#>   Female: mean = 3.648, n = 1287.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat       df p_value mean_diff        conf_int sig
#>    Equal variances -1.070 2434.000   0.285     -0.05 [-0.142, 0.042]    
#>  Unequal variances -1.069 2390.755   0.285     -0.05 [-0.142, 0.042]    
#> -------------------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

## Grouped Analysis

All functions support
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
for subgroup analysis:

``` r
survey_data %>%
  group_by(region) %>%
  describe(income, life_satisfaction, weights = sampling_weight)
#> 
#> Weighted Descriptive Statistics
#> -------------------------------
#> 
#> Group: region = East
#> --------------------
#> ----------------------------------------
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>             income 3760.687   3600 1388.321  7200 1700    0.718       421.9
#>  life_satisfaction    3.623      4    1.203     4    2   -0.556       457.4
#> ----------------------------------------
#> 
#> Group: region = West
#> --------------------
#> ----------------------------------------
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>             income 3738.586   3500 1433.325  7200 1900    0.726      1738.1
#>  life_satisfaction    3.625      4    1.139     4    2   -0.481      1934.8
#> ----------------------------------------
```

``` r
survey_data %>%
  group_by(region) %>%
  t_test(income, group = gender, weights = sampling_weight)
#> [region = 1]
#> t-Test: income by gender [Weighted]
#>   t(431.6) = 1.676, p = 0.094 , g = 0.158 (negligible), N = 450
#> [region = 2]
#> t-Test: income by gender [Weighted]
#>   t(1739.9) = 0.009, p = 0.993 , g = 0.000 (negligible), N = 1751
```

## Quick Reference

### Data Import & Export

| Function                                                                                                                                                       | Purpose                                         |
|----------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------|
| [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md), [`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md) | Import SPSS files with tagged NA support        |
| [`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md)                                                                              | Import Stata files                              |
| [`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md), [`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md)   | Import SAS files                                |
| [`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md)                                                                                | Import Excel files with label reconstruction    |
| [`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md)                                                                              | Export to SPSS with label/missing roundtripping |
| [`write_stata()`](https://YannickDiehl.github.io/mariposa/reference/write_stata.md)                                                                            | Export to Stata                                 |
| [`write_xpt()`](https://YannickDiehl.github.io/mariposa/reference/write_xpt.md)                                                                                | Export to SAS transport format                  |
| [`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)                                                                              | Export to Excel (data, codebook, frequencies)   |

### Label Management

| Function                                                                              | Purpose                               |
|---------------------------------------------------------------------------------------|---------------------------------------|
| [`var_label()`](https://YannickDiehl.github.io/mariposa/reference/var_label.md)       | Get/set variable labels               |
| [`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md)     | Get/set value labels                  |
| [`find_var()`](https://YannickDiehl.github.io/mariposa/reference/find_var.md)         | Search variables by name or label     |
| [`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)         | Labelled → factor                     |
| [`to_character()`](https://YannickDiehl.github.io/mariposa/reference/to_character.md) | Labelled → character                  |
| [`to_numeric()`](https://YannickDiehl.github.io/mariposa/reference/to_numeric.md)     | Factor/labelled → numeric             |
| [`to_labelled()`](https://YannickDiehl.github.io/mariposa/reference/to_labelled.md)   | Factor/character → labelled           |
| [`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md)             | Declare values as missing             |
| [`unlabel()`](https://YannickDiehl.github.io/mariposa/reference/unlabel.md)           | Strip all label metadata              |
| [`copy_labels()`](https://YannickDiehl.github.io/mariposa/reference/copy_labels.md)   | Restore labels after dplyr operations |
| [`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md)   | Remove unused value labels            |

### Data Transformation

| Function                                                                        | Purpose                                                   |
|---------------------------------------------------------------------------------|-----------------------------------------------------------|
| [`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md)             | Recode with string syntax (ranges, reverse, median split) |
| [`to_dummy()`](https://YannickDiehl.github.io/mariposa/reference/to_dummy.md)   | One-hot encoding / dummy variables                        |
| [`std()`](https://YannickDiehl.github.io/mariposa/reference/std.md)             | Z-standardization (sd, 2sd, mad, gmd methods)             |
| [`center()`](https://YannickDiehl.github.io/mariposa/reference/center.md)       | Mean-centering (grand-mean, group-mean)                   |
| [`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md) | Row-wise means with min_valid threshold                   |
| [`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md)   | Row-wise sums                                             |
| [`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md) | Count specific values per row                             |
| [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md)         | Percent of Maximum Possible Scores (0–100)                |

### Descriptive Statistics

| Function                                                                        | Purpose                                               |
|---------------------------------------------------------------------------------|-------------------------------------------------------|
| [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)   | Interactive HTML data dictionary                      |
| [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)   | Numeric summaries (mean, sd, median, range, skewness) |
| [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md) | Frequency tables with valid/cumulative percent        |
| [`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)   | Cross-tabulations with row/column/cell percentages    |

### Hypothesis Testing

| Function                                                                                    | Purpose                                     |
|---------------------------------------------------------------------------------------------|---------------------------------------------|
| [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)                   | Independent, paired, and one-sample t-tests |
| [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)       | One-way ANOVA                               |
| [`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md) | Multi-factor ANOVA with Type III SS         |
| [`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)                   | ANCOVA with estimated marginal means        |
| [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)       | Mann-Whitney U test                         |
| [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md)   | Kruskal-Wallis H test                       |
| [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)     | Wilcoxon signed-rank test                   |
| [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)     | Friedman test                               |
| [`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md)     | Exact binomial test                         |
| [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)           | Chi-square test of independence             |
| [`fisher_test()`](https://YannickDiehl.github.io/mariposa/reference/fisher_test.md)         | Fisher’s exact test                         |
| [`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md)             | Chi-square goodness-of-fit                  |
| [`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md)       | McNemar’s test for paired proportions       |

### Post-Hoc & Effect Sizes

| Function                                                                                        | Purpose                            |
|-------------------------------------------------------------------------------------------------|------------------------------------|
| [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)               | Tukey HSD pairwise comparisons     |
| [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md)           | Scheffe pairwise comparisons       |
| [`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)             | Test for homogeneity of variances  |
| [`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md)                 | Dunn’s post-hoc for Kruskal-Wallis |
| [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md) | Pairwise Wilcoxon for Friedman     |
| [`phi()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)                      | Phi coefficient                    |
| [`cramers_v()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)                | Cramer’s V                         |
| [`goodman_gamma()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)            | Goodman-Kruskal gamma              |

### Scale Analysis

| Function                                                                            | Purpose                                                      |
|-------------------------------------------------------------------------------------|--------------------------------------------------------------|
| [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md) | Cronbach’s Alpha with item statistics                        |
| [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)                 | Exploratory Factor Analysis (PCA/ML, Varimax/Oblimin/Promax) |

### Regression

| Function                                                                                            | Purpose                                  |
|-----------------------------------------------------------------------------------------------------|------------------------------------------|
| [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)     | Linear regression with SPSS-style output |
| [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md) | Logistic regression with odds ratios     |

### Weighted Statistics

| Function                                                                                                                                                                                                                                                                                                       | Purpose                     |
|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------|
| [`w_mean()`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md), [`w_median()`](https://YannickDiehl.github.io/mariposa/reference/w_median.md), [`w_sd()`](https://YannickDiehl.github.io/mariposa/reference/w_sd.md), [`w_var()`](https://YannickDiehl.github.io/mariposa/reference/w_var.md)       | Central tendency and spread |
| [`w_se()`](https://YannickDiehl.github.io/mariposa/reference/w_se.md), [`w_quantile()`](https://YannickDiehl.github.io/mariposa/reference/w_quantile.md), [`w_iqr()`](https://YannickDiehl.github.io/mariposa/reference/w_iqr.md), [`w_range()`](https://YannickDiehl.github.io/mariposa/reference/w_range.md) | Precision and distribution  |
| [`w_skew()`](https://YannickDiehl.github.io/mariposa/reference/w_skew.md), [`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/reference/w_kurtosis.md), [`w_modus()`](https://YannickDiehl.github.io/mariposa/reference/w_modus.md)                                                                      | Shape and mode              |

## Guides

Explore the full documentation:

- **Data Management**
  - [`vignette("data-io")`](https://YannickDiehl.github.io/mariposa/articles/data-io.md)
    — Importing and exporting data
  - [`vignette("labels-and-missing-values")`](https://YannickDiehl.github.io/mariposa/articles/labels-and-missing-values.md)
    — Working with labels and missing values
  - [`vignette("data-transformation")`](https://YannickDiehl.github.io/mariposa/articles/data-transformation.md)
    — Recoding, standardization, and row operations
- **Core Analysis**
  - [`vignette("descriptive-statistics")`](https://YannickDiehl.github.io/mariposa/articles/descriptive-statistics.md)
    — Summaries, frequencies, and cross-tabulations
  - [`vignette("hypothesis-testing")`](https://YannickDiehl.github.io/mariposa/articles/hypothesis-testing.md)
    — Comparing groups and testing hypotheses
  - [`vignette("correlation-analysis")`](https://YannickDiehl.github.io/mariposa/articles/correlation-analysis.md)
    — Measuring relationships between variables
- **Advanced Topics**
  - [`vignette("scale-analysis")`](https://YannickDiehl.github.io/mariposa/articles/scale-analysis.md)
    — Reliability, factor analysis, and scale construction
  - [`vignette("regression-analysis")`](https://YannickDiehl.github.io/mariposa/articles/regression-analysis.md)
    — Linear and logistic regression
  - [`vignette("survey-weights")`](https://YannickDiehl.github.io/mariposa/articles/survey-weights.md)
    — Working with weighted data
