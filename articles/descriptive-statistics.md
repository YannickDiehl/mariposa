# Descriptive Statistics and Frequencies

``` r
library(mariposa)
library(dplyr)
data(survey_data)
```

## Overview

Descriptive statistics help you understand your data before doing any
complex analysis. Think of it as getting to know your survey responses —
what is typical, what is unusual, and how spread out the answers are.

This guide covers three functions:

- [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
  — Summary statistics for numeric variables
- [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  — Frequency tables for categorical variables
- [`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)
  — Cross-tabulations comparing two categorical variables

## Getting Summary Statistics

### Basic Usage

The simplest way to get descriptive statistics:

``` r
survey_data %>% describe(age)
#>  Variable  Mean Median     SD Range IQR Skewness    N Missing
#>       age 50.55     50 16.976    77  24    0.172 2500       0
```

This gives you the mean ($\bar{x}$), median, standard deviation (*SD*),
and other key statistics.

### Multiple Variables at Once

Analyze several variables together:

``` r
survey_data %>%
  describe(age, income, life_satisfaction)
#>           Variable     Mean Median       SD Range  IQR Skewness    N Missing
#>                age   50.550     50   16.976    77   24    0.172 2500       0
#>             income 3753.934   3500 1432.802  7200 1900    0.730 2186     314
#>  life_satisfaction    3.628      4    1.153     4    2   -0.501 2421      79
```

### With Survey Weights

To get population-representative statistics, add the `weights` argument:

``` r
survey_data %>%
  describe(age, income, life_satisfaction,
           weights = sampling_weight)
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>                age   50.514     50   17.084    77   25    0.159      2468.8
#>             income 3743.099   3500 1423.966  7200 1900    0.724      2158.9
#>  life_satisfaction    3.625      4    1.152     4    2   -0.498      2390.9
```

The weighted results better represent your target population by
correcting for sampling biases.

### Grouped Analysis

Compare statistics across groups using
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html):

``` r
survey_data %>%
  group_by(region) %>%
  describe(age, income, weights = sampling_weight)
#>  Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>       age   52.278     53   17.595    77   24    0.098       477.0
#>    income 3760.687   3600 1388.321  7200 1700    0.718       421.9
#>  Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>       age   50.067     49   16.927    77   24    0.170      1993.1
#>    income 3738.586   3500 1433.325  7200 1900    0.726      1738.1
```

### Customizing Output

Choose which statistics to display with the `show` argument:

``` r
# Just the essentials
survey_data %>%
  describe(age, income, show = c("mean", "sd", "range"))
#>  Variable     Mean       SD Range    N Missing
#>       age   50.550   16.976    77 2500       0
#>    income 3753.934 1432.802  7200 2186     314

# Everything available
survey_data %>%
  describe(age, show = "all")
#>  Variable  Mean Median     SD   SE Range IQR Skewness Kurtosis Variance Mode
#>       age 50.55     50 16.976 0.34    77  24    0.172   -0.364  288.185   18
#>  Q25 Q50 Q75    N Missing
#>   38  50  62 2500       0
```

## Frequency Tables

### Basic Frequency

For categorical variables, use
[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md):

``` r
survey_data %>%
  frequency(education)
#> 
#> education (Highest educational attainment)
#> # total N=2500 valid N=2500 mean=NA sd=NA skewness=NA
#> 
#> +------+--------------------+--------+--------+--------+--------+
#> |Value |Label               |N       |Raw %   |Valid % |Cum. %  |
#> +------+--------------------+--------+--------+--------+--------+
#> |Basic |Basic Secondary     |841     |33.64   |33.64   |33.64   |
#> |Interm|Intermediate Seconda|629     |25.16   |25.16   |58.80   |
#> |Academ|Academic Secondary  |631     |25.24   |25.24   |84.04   |
#> |Univer|University          |399     |15.96   |15.96   |100.00  |
#> +------+--------------------+--------+--------+--------+--------+
```

### Understanding the Output

The output includes:

- **Frequency**: Count of responses in each category
- **Percent**: Percentage of all cases (including missing)
- **Valid Percent**: Percentage of non-missing cases only
- **Cumulative Percent**: Running total of valid percentages

When reporting results, use *Valid Percent* if missing values are truly
missing (e.g., skip patterns), and *Percent* when non-response is
meaningful.

### Weighted Frequencies

``` r
survey_data %>%
  frequency(education, weights = sampling_weight)
#> 
#> education (Highest educational attainment)
#> # total N=2516 valid N=2516 mean=NA sd=NA skewness=NA
#> 
#> +------+--------------------+--------+--------+--------+--------+
#> |Value |Label               |N       |Raw %   |Valid % |Cum. %  |
#> +------+--------------------+--------+--------+--------+--------+
#> |Basic |Basic Secondary     |848     |33.71   |33.71   |33.71   |
#> |Interm|Intermediate Seconda|641     |25.47   |25.47   |59.18   |
#> |Academ|Academic Secondary  |642     |25.51   |25.51   |84.69   |
#> |Univer|University          |385     |15.31   |15.31   |100.00  |
#> +------+--------------------+--------+--------+--------+--------+
```

### Multiple Variables

Analyze several categorical variables at once:

``` r
survey_data %>%
  frequency(education, employment, region,
            weights = sampling_weight)
#> 
#> education (Highest educational attainment)
#> # total N=2516 valid N=2516 mean=NA sd=NA skewness=NA
#> 
#> +------+--------------------+--------+--------+--------+--------+
#> |Value |Label               |N       |Raw %   |Valid % |Cum. %  |
#> +------+--------------------+--------+--------+--------+--------+
#> |Basic |Basic Secondary     |848     |33.71   |33.71   |33.71   |
#> |Interm|Intermediate Seconda|641     |25.47   |25.47   |59.18   |
#> |Academ|Academic Secondary  |642     |25.51   |25.51   |84.69   |
#> |Univer|University          |385     |15.31   |15.31   |100.00  |
#> +------+--------------------+--------+--------+--------+--------+
#> 
#> 
#> employment (Employment status)
#> # total N=2516 valid N=2516 mean=NA sd=NA skewness=NA
#> 
#> +------+--------------------+--------+--------+--------+--------+
#> |Value |Label               |N       |Raw %   |Valid % |Cum. %  |
#> +------+--------------------+--------+--------+--------+--------+
#> |Studen|Student             |80      |3.18    |3.18    |3.18    |
#> |Employ|Employed            |1603    |63.71   |63.71   |66.89   |
#> |Unempl|Unemployed          |184     |7.32    |7.32    |74.21   |
#> |Retire|Retired             |534     |21.21   |21.21   |95.41   |
#> |Other |Other               |115     |4.59    |4.59    |100.00  |
#> +------+--------------------+--------+--------+--------+--------+
#> 
#> 
#> region (Region (East/West))
#> # total N=2516 valid N=2516 mean=NA sd=NA skewness=NA
#> 
#> +------+--------------------+--------+--------+--------+--------+
#> |Value |Label               |N       |Raw %   |Valid % |Cum. %  |
#> +------+--------------------+--------+--------+--------+--------+
#> |East  |East                |509     |20.23   |20.23   |20.23   |
#> |West  |West                |2007    |79.77   |79.77   |100.00  |
#> +------+--------------------+--------+--------+--------+--------+
```

### Grouped Frequencies

See how distributions vary by group:

``` r
survey_data %>%
  group_by(gender) %>%
  frequency(education, weights = sampling_weight)
#> 
#> education (Highest educational attainment)
#> # total N=1195 valid N=1195 mean=NA sd=NA skewness=NA
#> 
#> +------+--------------------+--------+--------+--------+--------+
#> |Value |Label               |N       |Raw %   |Valid % |Cum. %  |
#> +------+--------------------+--------+--------+--------+--------+
#> |Basic |Basic Secondary     |402     |33.61   |33.61   |33.61   |
#> |Interm|Intermediate Seconda|291     |24.35   |24.35   |57.96   |
#> |Academ|Academic Secondary  |326     |27.31   |27.31   |85.27   |
#> |Univer|University          |176     |14.73   |14.73   |100.00  |
#> +------+--------------------+--------+--------+--------+--------+
#> # total N=1321 valid N=1321 mean=NA sd=NA skewness=NA
#> 
#> +------+--------------------+--------+--------+--------+--------+
#> |Value |Label               |N       |Raw %   |Valid % |Cum. %  |
#> +------+--------------------+--------+--------+--------+--------+
#> |Basic |Basic Secondary     |447     |33.79   |33.79   |33.79   |
#> |Interm|Intermediate Seconda|350     |26.49   |26.49   |60.28   |
#> |Academ|Academic Secondary  |316     |23.89   |23.89   |84.17   |
#> |Univer|University          |209     |15.83   |15.83   |100.00  |
#> +------+--------------------+--------+--------+--------+--------+
```

## Cross-Tabulation

### Basic Crosstab

Examine the relationship between two categorical variables:

``` r
survey_data %>%
  crosstab(education, employment)
#> 
#> Crosstabulation: education × employment
#> ────────────────────────────────────────────────── 
#> 
#>                         employment 
#> education 
#>                              Student    Employed  Unemployed     Retired       Other       Total
#> ──────────────────────────────────────────────────────────────────────────────────────────────── 
#> Basic Secondary                    0         571          65         171          34         841
#>                                 0.0%       67.9%        7.7%       20.3%        4.0%      100.0%  (row %)
#> 
#> Intermediate Secondary             0         412          51         137          29         629
#>                                 0.0%       65.5%        8.1%       21.8%        4.6%      100.0%  (row %)
#> 
#> Academic Secondary                44         366          44         145          32         631
#>                                 7.0%       58.0%        7.0%       23.0%        5.1%      100.0%  (row %)
#> 
#> University                        34         251          22          72          20         399
#>                                 8.5%       62.9%        5.5%       18.0%        5.0%      100.0%  (row %)
#> ──────────────────────────────────────────────────────────────────────────────────────────────── 
#> Total                             78        1600         182         525         115        2500
#> 
#> N = 2500
```

### Understanding the Output

Cross-tabulations show several types of percentages:

- **Count**: Number of cases in each cell
- **Row %**: Percentage within each row (sums to 100% across)
- **Column %**: Percentage within each column (sums to 100% down)
- **Cell %**: Percentage of the total sample

Row percentages answer: “Of people with this education level, what
proportion has each employment status?” Column percentages answer: “Of
people with this employment status, what proportion has each education
level?”

### Weighted Crosstabs

``` r
survey_data %>%
  crosstab(education, employment, weights = sampling_weight)
#> 
#> Crosstabulation: education × employment
#> ────────────────────────────────────────────────── 
#> 
#>                         employment 
#> education 
#>                              Student    Employed  Unemployed     Retired       Other       Total
#> ──────────────────────────────────────────────────────────────────────────────────────────────── 
#> Basic Secondary                    0         573          66         175          34         848
#>                                 0.0%       67.6%        7.8%       20.6%        4.0%      100.0%  (row %)
#> 
#> Intermediate Secondary             0         420          52         139          29         641
#>                                 0.0%       65.6%        8.1%       21.7%        4.6%      100.0%  (row %)
#> 
#> Academic Secondary                46         370          45         149          33         642
#>                                 7.2%       57.6%        7.0%       23.1%        5.1%      100.0%  (row %)
#> 
#> University                        34         240          21          72          20         385
#>                                 8.7%       62.2%        5.4%       18.6%        5.1%      100.0%  (row %)
#> ──────────────────────────────────────────────────────────────────────────────────────────────── 
#> Total                             80        1603         184         534         115        2516
#> 
#> N = 2516 (Weighted)
```

### Grouped Crosstabs

Create separate cross-tabulations for each subgroup:

``` r
survey_data %>%
  group_by(region) %>%
  crosstab(education, employment, weights = sampling_weight)
#> 
#> 
#> --- Group: region = East ---
#> 
#> Crosstabulation: education × employment
#> ────────────────────────────────────────────────── 
#> 
#>                         employment 
#> education 
#>                              Student    Employed  Unemployed     Retired       Other       Total
#> ──────────────────────────────────────────────────────────────────────────────────────────────── 
#> Basic Secondary                    0         112          11          39          12         175
#>                                 0.0%       64.1%        6.6%       22.4%        6.9%      100.0%  (row %)
#> 
#> Intermediate Secondary             0          83          13          29           4         129
#>                                 0.0%       64.7%        9.8%       22.4%        3.1%      100.0%  (row %)
#> 
#> Academic Secondary                 6          72           8          35           1         123
#>                                 5.2%       58.5%        6.5%       28.9%        0.9%      100.0%  (row %)
#> 
#> University                         5          53           1          19           4          82
#>                                 6.4%       64.3%        1.2%       23.2%        4.9%      100.0%  (row %)
#> ──────────────────────────────────────────────────────────────────────────────────────────────── 
#> Total                             12         320          33         123          21         509
#> 
#> N = 509 (Weighted)
#> 
#> 
#> --- Group: region = West ---
#> 
#> Crosstabulation: education × employment
#> ────────────────────────────────────────────────── 
#> 
#>                         employment 
#> education 
#>                              Student    Employed  Unemployed     Retired       Other       Total
#> ──────────────────────────────────────────────────────────────────────────────────────────────── 
#> Basic Secondary                    0         461          55         136          21         673
#>                                 0.0%       68.5%        8.2%       20.1%        3.2%      100.0%  (row %)
#> 
#> Intermediate Secondary             0         337          40         110          26         512
#>                                 0.0%       65.8%        7.7%       21.5%        5.0%      100.0%  (row %)
#> 
#> Academic Secondary                40         298          37         113          31         519
#>                                 7.7%       57.4%        7.1%       21.8%        6.1%      100.0%  (row %)
#> 
#> University                        28         187          20          52          16         303
#>                                 9.4%       61.7%        6.5%       17.3%        5.2%      100.0%  (row %)
#> ──────────────────────────────────────────────────────────────────────────────────────────────── 
#> Total                             68        1282         151         411          94        2007
#> 
#> N = 2007 (Weighted)
```

## Practical Tips

### 1. Always Check Your Data First

Start with basic descriptives to spot potential issues:

``` r
survey_data %>%
  describe(age, income, show = "all")
#>  Variable     Mean Median       SD     SE Range  IQR Skewness Kurtosis
#>       age   50.550     50   16.976  0.340    77   24    0.172   -0.364
#>    income 3753.934   3500 1432.802 30.645  7200 1900    0.730    0.376
#>     Variance Mode  Q25  Q50  Q75    N Missing
#>      288.185   18   38   50   62 2500       0
#>  2052920.442 3200 2700 3500 4600 2186     314
```

Look for:

- **Impossible values** (e.g., negative ages) indicating data entry
  errors
- **High rates of missing data** that might bias results
- **Extreme skewness** (values beyond $\pm 1$) suggesting non-normal
  distributions

### 2. Use Weights When Available

Unweighted results can be misleading:

``` r
unweighted_mean <- survey_data %>%
  summarise(mean_age = mean(age, na.rm = TRUE)) %>%
  pull()

weighted_result <- w_mean(survey_data, age, weights = sampling_weight)

cat("Unweighted mean age:", round(unweighted_mean, 1), "\n")
#> Unweighted mean age: 50.5
cat("Weighted mean age:", round(weighted_result$results$weighted_mean, 1), "\n")
#> Weighted mean age: 50.5
```

### 3. Consider Your Audience

- **Technical audience**: Include *SD*, skewness, kurtosis
- **General audience**: Focus on mean, median, and percentages

## Complete Example

A typical descriptive analysis workflow:

``` r
# 1. Overall summary
survey_data %>%
  describe(age, income, life_satisfaction,
           weights = sampling_weight,
           show = "short")
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>                age   50.514     50   17.084    77   25    0.159      2468.8
#>             income 3743.099   3500 1423.966  7200 1900    0.724      2158.9
#>  life_satisfaction    3.625      4    1.152     4    2   -0.498      2390.9

# 2. Key distributions
survey_data %>%
  frequency(education, employment,
            weights = sampling_weight)
#> 
#> education (Highest educational attainment)
#> # total N=2516 valid N=2516 mean=NA sd=NA skewness=NA
#> 
#> +------+--------------------+--------+--------+--------+--------+
#> |Value |Label               |N       |Raw %   |Valid % |Cum. %  |
#> +------+--------------------+--------+--------+--------+--------+
#> |Basic |Basic Secondary     |848     |33.71   |33.71   |33.71   |
#> |Interm|Intermediate Seconda|641     |25.47   |25.47   |59.18   |
#> |Academ|Academic Secondary  |642     |25.51   |25.51   |84.69   |
#> |Univer|University          |385     |15.31   |15.31   |100.00  |
#> +------+--------------------+--------+--------+--------+--------+
#> 
#> 
#> employment (Employment status)
#> # total N=2516 valid N=2516 mean=NA sd=NA skewness=NA
#> 
#> +------+--------------------+--------+--------+--------+--------+
#> |Value |Label               |N       |Raw %   |Valid % |Cum. %  |
#> +------+--------------------+--------+--------+--------+--------+
#> |Studen|Student             |80      |3.18    |3.18    |3.18    |
#> |Employ|Employed            |1603    |63.71   |63.71   |66.89   |
#> |Unempl|Unemployed          |184     |7.32    |7.32    |74.21   |
#> |Retire|Retired             |534     |21.21   |21.21   |95.41   |
#> |Other |Other               |115     |4.59    |4.59    |100.00  |
#> +------+--------------------+--------+--------+--------+--------+

# 3. Relationship between categories
survey_data %>%
  crosstab(education, employment,
           weights = sampling_weight)
#> 
#> Crosstabulation: education × employment
#> ────────────────────────────────────────────────── 
#> 
#>                         employment 
#> education 
#>                              Student    Employed  Unemployed     Retired       Other       Total
#> ──────────────────────────────────────────────────────────────────────────────────────────────── 
#> Basic Secondary                    0         573          66         175          34         848
#>                                 0.0%       67.6%        7.8%       20.6%        4.0%      100.0%  (row %)
#> 
#> Intermediate Secondary             0         420          52         139          29         641
#>                                 0.0%       65.6%        8.1%       21.7%        4.6%      100.0%  (row %)
#> 
#> Academic Secondary                46         370          45         149          33         642
#>                                 7.2%       57.6%        7.0%       23.1%        5.1%      100.0%  (row %)
#> 
#> University                        34         240          21          72          20         385
#>                                 8.7%       62.2%        5.4%       18.6%        5.1%      100.0%  (row %)
#> ──────────────────────────────────────────────────────────────────────────────────────────────── 
#> Total                             80        1603         184         534         115        2516
#> 
#> N = 2516 (Weighted)

# 4. Regional comparisons
survey_data %>%
  group_by(region) %>%
  describe(income, life_satisfaction,
           weights = sampling_weight)
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>             income 3760.687   3600 1388.321  7200 1700    0.718       421.9
#>  life_satisfaction    3.623      4    1.203     4    2   -0.556       457.4
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>             income 3738.586   3500 1433.325  7200 1900    0.726      1738.1
#>  life_satisfaction    3.625      4    1.139     4    2   -0.481      1934.8
```

## Summary

1.  **[`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)**
    gives you a full picture of numeric variables
2.  **[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)**
    shows the distribution of categorical variables
3.  **[`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)**
    reveals relationships between two categorical variables
4.  All three functions support **survey weights** and **grouped
    analysis**
5.  Always explore your data descriptively before running statistical
    tests

## Next Steps

- Test for significant differences with
  [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)
  or
  [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  — see
  [`vignette("hypothesis-testing")`](https://YannickDiehl.github.io/mariposa/articles/hypothesis-testing.md)
- Explore relationships with
  [`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)
  — see
  [`vignette("correlation-analysis")`](https://YannickDiehl.github.io/mariposa/articles/correlation-analysis.md)
- Learn about weighted analysis — see
  [`vignette("survey-weights")`](https://YannickDiehl.github.io/mariposa/articles/survey-weights.md)
