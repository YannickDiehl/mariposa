# Descriptive Statistics and Frequencies

``` r

library(mariposa)
library(dplyr)
data(survey_data)
```

## Overview

Before running any statistical tests, you should understand your data.
Descriptive statistics summarize what is typical, what is unusual, and
how responses are distributed.

mariposa provides four functions for data exploration:

| Function | Best for |
|----|----|
| [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md) | Numeric variables — means, medians, spread |
| [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md) | Categorical variables — counts and percentages |
| [`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md) | Relationships between two categorical variables |
| [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md) | Full data dictionary with types, labels, and values |

All four support survey weights and grouped analysis.

## Summary Statistics with describe()

### Basic Usage

``` r

survey_data %>%
  describe(age)
#> 
#> Descriptive Statistics
#> ----------------------
#>  Variable  Mean Median     SD Range IQR Skewness    N Missing
#>       age 50.55     50 16.976    77  24    0.172 2500       0
#> ----------------------------------------
```

### Multiple Variables

``` r

survey_data %>%
  describe(age, income, life_satisfaction)
#> 
#> Descriptive Statistics
#> ----------------------
#>           Variable     Mean Median       SD Range  IQR Skewness    N Missing
#>                age   50.550     50   16.976    77   24    0.172 2500       0
#>             income 3753.934   3500 1432.802  7200 1900    0.730 2186     314
#>  life_satisfaction    3.628      4    1.153     4    2   -0.501 2421      79
#> ----------------------------------------
```

### With Survey Weights

Add `weights` for population-representative statistics:

``` r

survey_data %>%
  describe(age, income, life_satisfaction, weights = sampling_weight)
#> 
#> Weighted Descriptive Statistics
#> -------------------------------
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>                age   50.514     50   17.084    77   25    0.159      2468.8
#>             income 3743.099   3500 1423.966  7200 1900    0.725      2158.9
#>  life_satisfaction    3.625      4    1.152     4    2   -0.499      2390.9
#> ----------------------------------------
```

### Grouped Analysis

Compare statistics across subgroups:

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
#>             income 3760.687   3600 1388.321  7200 1700    0.721       421.9
#>  life_satisfaction    3.623      4    1.203     4    2   -0.558       457.4
#> ----------------------------------------
#> 
#> Group: region = West
#> --------------------
#> ----------------------------------------
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>             income 3738.586   3500 1433.325  7200 1900    0.727      1738.1
#>  life_satisfaction    3.625      4    1.139     4    2   -0.481      1934.8
#> ----------------------------------------
```

### Choosing Statistics

The `show` argument controls which statistics are displayed:

``` r

# Just the essentials
survey_data %>%
  describe(age, income, show = c("mean", "sd", "range"))
#> 
#> Descriptive Statistics
#> ----------------------
#>  Variable     Mean       SD Range    N Missing
#>       age   50.550   16.976    77 2500       0
#>    income 3753.934 1432.802  7200 2186     314
#> ----------------------------------------
```

``` r

# Everything available
survey_data %>%
  describe(age, show = "all")
#> 
#> Descriptive Statistics
#> ----------------------
#>  Variable  Mean Median     SD   SE Range IQR Skewness Kurtosis Variance Mode
#>       age 50.55     50 16.976 0.34    77  24    0.172   -0.364  288.185   18
#>  Q25 Q50 Q75    N Missing
#>   38  50  62 2500       0
#> ----------------------------------------
```

### Understanding the Output

- **n**: Number of valid (non-missing) cases
- **mean** ($`\bar{x}`$): The arithmetic average
- **sd**: Standard deviation — how spread out the values are
- **median**: The middle value (50th percentile)
- **min / max**: The range of observed values
- **skewness**: Distribution symmetry. Values near 0 indicate symmetry;
  values beyond $`\pm 1`$ indicate notable skew
- **kurtosis**: Tail heaviness. Values near 0 indicate a normal-like
  shape

## Frequency Tables with frequency()

### Basic Frequency

``` r

survey_data %>%
  frequency(education)
#> Frequency: education
#>   4 categories, N valid = 2500, missing = 0
#> Use summary() for detailed output.
```

### Understanding the Output

- **Frequency**: Number of respondents in each category
- **Percent**: Percentage of all cases, including missing
- **Valid Percent**: Percentage of non-missing cases only
- **Cumulative Percent**: Running total of valid percentages

Use *Valid Percent* when missing values are truly missing (e.g., skip
patterns). Use *Percent* when non-response is itself meaningful.

### Weighted Frequencies

``` r

survey_data %>%
  frequency(education, weights = sampling_weight)
#> Frequency: education [Weighted]
#>   4 categories, N valid = 2516, missing = 0
#> Use summary() for detailed output.
```

### Multiple Variables

``` r

survey_data %>%
  frequency(education, employment, region, weights = sampling_weight)
#> Frequency: education [Weighted]
#>   4 categories, N valid = 2516, missing = 0
#> Frequency: employment [Weighted]
#>   5 categories, N valid = 2516, missing = 0
#> Frequency: region [Weighted]
#>   2 categories, N valid = 2516, missing = 0
#> Use summary() for detailed output.
```

### Grouped Frequencies

``` r

survey_data %>%
  group_by(gender) %>%
  frequency(education, weights = sampling_weight)
#> Frequency: education [Weighted]
#>   [gender = Male] 4 categories, N valid = 1195, missing = 0
#>   [gender = Female] 4 categories, N valid = 1321, missing = 0
#> Use summary() for detailed output.
```

## Cross-Tabulation with crosstab()

### Basic Crosstab

Examine the relationship between two categorical variables:

``` r

survey_data %>%
  crosstab(education, employment)
#> Crosstab: education x employment
#>   4 x 5 table, N = 2500
#>   Note: no significance test included - use chi_square() for a test of independence.
#> Use summary() for detailed output.
```

### Understanding the Output

- **Count**: Number of cases in each cell
- **Row %**: Percentage within each row (sums to 100% across)
- **Column %**: Percentage within each column (sums to 100% down)
- **Cell %**: Percentage of the total sample

Row percentages answer: “Of people with this education level, what
proportion has each employment status?”

Column percentages answer: “Of people with this employment status, what
proportion has each education level?”

### Weighted Crosstabs

``` r

survey_data %>%
  crosstab(education, employment, weights = sampling_weight)
#> Crosstab: education x employment [Weighted]
#>   4 x 5 table, N = 2516
#>   Note: no significance test included - use chi_square() for a test of independence.
#> Use summary() for detailed output.
```

### Grouped Crosstabs

``` r

survey_data %>%
  group_by(region) %>%
  crosstab(education, employment, weights = sampling_weight)
#> Crosstab: education x employment [Weighted]
#>   [region = East] 4 x 5 table, N = 509
#>   [region = West] 4 x 5 table, N = 2007
#>   Note: no significance test included - use chi_square() for a test of independence.
#> Use summary() for detailed output.
```

## Data Dictionary with codebook()

For a comprehensive overview of your entire dataset, use
[`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md):

``` r

codebook(survey_data)
```

This opens an interactive HTML view in the RStudio Viewer showing
variable names, types, labels, value distributions, and missing data
patterns. It is the fastest way to orient yourself in a new dataset.

## Complete Example

A typical descriptive analysis workflow:

``` r

# 1. Explore the dataset
find_var(survey_data, "trust|satisfaction")
#>   col              name                                           label
#> 1  10 life_satisfaction Life satisfaction (1=dissatisfied, 5=satisfied)
#> 2  11  trust_government        Trust in government (1=none, 5=complete)
#> 3  12       trust_media             Trust in media (1=none, 5=complete)
#> 4  13     trust_science           Trust in science (1=none, 5=complete)

# 2. Summarize numeric variables
survey_data %>%
  describe(age, income, life_satisfaction,
           weights = sampling_weight)
#> 
#> Weighted Descriptive Statistics
#> -------------------------------
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>                age   50.514     50   17.084    77   25    0.159      2468.8
#>             income 3743.099   3500 1423.966  7200 1900    0.725      2158.9
#>  life_satisfaction    3.625      4    1.152     4    2   -0.499      2390.9
#> ----------------------------------------

# 3. Check categorical distributions
survey_data %>%
  frequency(education, employment,
            weights = sampling_weight)
#> Frequency: education [Weighted]
#>   4 categories, N valid = 2516, missing = 0
#> Frequency: employment [Weighted]
#>   5 categories, N valid = 2516, missing = 0
#> Use summary() for detailed output.

# 4. Cross-tabulate key relationships
survey_data %>%
  crosstab(education, employment,
           weights = sampling_weight)
#> Crosstab: education x employment [Weighted]
#>   4 x 5 table, N = 2516
#>   Note: no significance test included - use chi_square() for a test of independence.
#> Use summary() for detailed output.

# 5. Compare across regions
survey_data %>%
  group_by(region) %>%
  describe(income, life_satisfaction,
           weights = sampling_weight)
#> 
#> Weighted Descriptive Statistics
#> -------------------------------
#> 
#> Group: region = East
#> --------------------
#> ----------------------------------------
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>             income 3760.687   3600 1388.321  7200 1700    0.721       421.9
#>  life_satisfaction    3.623      4    1.203     4    2   -0.558       457.4
#> ----------------------------------------
#> 
#> Group: region = West
#> --------------------
#> ----------------------------------------
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>             income 3738.586   3500 1433.325  7200 1900    0.727      1738.1
#>  life_satisfaction    3.625      4    1.139     4    2   -0.481      1934.8
#> ----------------------------------------
```

## Practical Tips

1.  **Start with descriptives before testing.** Understanding
    distributions, ranges, and missing data patterns prevents surprises
    in later analyses.

2.  **Check for impossible values.** Negative ages, incomes above
    plausible limits, or out-of-range Likert responses indicate data
    quality issues.

3.  **Look at skewness.** Values beyond $`\pm 1`$ suggest the
    distribution departs notably from normality. This affects the choice
    between parametric and non-parametric tests.

4.  **Always use weights when available.** Unweighted statistics
    describe your sample; weighted statistics estimate the population.

5.  **Consider your audience.** For technical readers, include SD,
    skewness, and kurtosis. For general audiences, focus on means,
    medians, and percentages.

## Summary

1.  **[`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)**
    summarizes numeric variables (mean, SD, median, range, skewness)
2.  **[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)**
    counts categories with percent, valid percent, and cumulative
    percent
3.  **[`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)**
    shows the joint distribution of two categorical variables
4.  **[`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)**
    provides an interactive HTML data dictionary
5.  All four functions support **survey weights** and
    **[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)**

## Next Steps

- Test for significant differences — see
  [`vignette("hypothesis-testing")`](https://YannickDiehl.github.io/mariposa/articles/hypothesis-testing.md)
- Measure relationships between variables — see
  [`vignette("correlation-analysis")`](https://YannickDiehl.github.io/mariposa/articles/correlation-analysis.md)
- Learn about survey weights — see
  [`vignette("survey-weights")`](https://YannickDiehl.github.io/mariposa/articles/survey-weights.md)
