# Descriptive Statistics and Frequencies

``` r
library(mariposa)
#> 
#> Attaching package: 'mariposa'
#> The following object is masked from 'package:stats':
#> 
#>     frequency
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
data(survey_data)
```

## What Does This Do?

Descriptive statistics help you understand your data before doing any
complex analysis. Think of it as getting to know your survey responses -
what’s typical, what’s unusual, and how spread out the answers are.

The main functions covered here: -
[`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md) -
Get a complete summary of numeric variables -
[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md) -
See how many people chose each option -
[`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md) -
Compare responses across two categorical variables

## Getting Summary Statistics

### Basic Usage

The simplest way to get descriptive statistics:

``` r
survey_data %>% describe(age)
#> ── Descriptive Statistics ──────────────────────────────────────────────────────
#>  Variable  Mean Median     SD Range IQR Skewness    N Missing
#>       age 50.55     50 16.976    77  24    0.172 2500       0
#> ────────────────────────────────────────────────────────────────────────────────
```

This gives you the mean, median, standard deviation, and other key
statistics for age.

### Multiple Variables at Once

Analyze several variables together:

``` r
survey_data %>%
  describe(age, income, life_satisfaction)
#> ── Descriptive Statistics ──────────────────────────────────────────────────────
#>           Variable     Mean Median       SD Range  IQR Skewness    N Missing
#>                age   50.550     50   16.976    77   24    0.172 2500       0
#>             income 3753.934   3500 1432.802  7200 1900    0.730 2186     314
#>  life_satisfaction    3.628      4    1.153     4    2   -0.501 2421      79
#> ────────────────────────────────────────────────────────────────────────────────
```

### With Survey Weights

To get population-representative statistics:

``` r
survey_data %>%
  describe(age, income, life_satisfaction,
           weights = sampling_weight)
#> ── Weighted Descriptive Statistics ─────────────────────────────────────────────
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>                age   50.514     50   17.084    77   25    0.159      2468.8
#>             income 3743.099   3500 1423.966  7200 1900    0.724      2158.9
#>  life_satisfaction    3.625      4    1.152     4    2   -0.498      2390.9
#> ────────────────────────────────────────────────────────────────────────────────
```

The weighted results better represent your target population.

### Grouped Analysis

Compare statistics across groups:

``` r
survey_data %>%
  group_by(region) %>%
  describe(age, income, weights = sampling_weight)
#> ── Weighted Descriptive Statistics ─────────────────────────────────────────────
#> 
#> ── Group: region = East ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>  Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>       age   52.278     53   17.595    77   24    0.098       477.0
#>    income 3760.687   3600 1388.321  7200 1700    0.718       421.9
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Group: region = West ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>  Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>       age   50.067     49   16.927    77   24    0.170      1993.1
#>    income 3738.586   3500 1433.325  7200 1900    0.726      1738.1
#> ────────────────────────────────────────────────────────────────────────────────
```

### Customizing Output

Choose which statistics to display:

``` r
# Just the essentials
survey_data %>%
  describe(age, income, show = c("mean", "sd", "range"))
#> ── Descriptive Statistics ──────────────────────────────────────────────────────
#>  Variable     Mean       SD Range    N Missing
#>       age   50.550   16.976    77 2500       0
#>    income 3753.934 1432.802  7200 2186     314
#> ────────────────────────────────────────────────────────────────────────────────

# Everything available
survey_data %>%
  describe(age, show = "all")
#> ── Descriptive Statistics ──────────────────────────────────────────────────────
#>  Variable  Mean Median     SD   SE Range IQR Skewness Kurtosis Variance Mode
#>       age 50.55     50 16.976 0.34    77  24    0.172   -0.364  288.185   18
#>  Q25 Q50 Q75    N Missing
#>   38  50  62 2500       0
#> ────────────────────────────────────────────────────────────────────────────────
```

## Frequency Tables

### Basic Frequency

For categorical variables, use
[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md):

``` r
survey_data %>%
  frequency(education)
#> ── Frequency Analysis Results ──────────────────────────────────────────────────
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

### With Percentages

The output shows: - **Frequency**: How many people gave each answer -
**Percent**: Percentage including missing values - **Valid Percent**:
Percentage excluding missing values - **Cumulative Percent**: Running
total of valid percentages

### Weighted Frequencies

Include survey weights for representative results:

``` r
survey_data %>%
  frequency(education, weights = sampling_weight)
#> ── Weighted Frequency Analysis Results ─────────────────────────────────────────
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

Analyze several categorical variables:

``` r
survey_data %>%
  frequency(education, employment, region,
            weights = sampling_weight)
#> ── Weighted Frequency Analysis Results ─────────────────────────────────────────
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
#> ── Weighted Frequency Analysis Results ─────────────────────────────────────────
#> 
#> education (Highest educational attainment)
#> 
#> 
#> ── Group: gender = Male ──
#> 
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
#> 
#> ── Group: gender = Female ──
#> 
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

## Cross-tabulation

### Basic Crosstab

Examine relationships between two categorical variables:

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

Crosstabs show: - Count in each cell - Row percentages (sum to 100%
across rows) - Column percentages (sum to 100% down columns) - Cell
percentages (percentage of total)

### Weighted Crosstabs

Include survey weights:

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

Create separate crosstabs for each group:

``` r
survey_data %>%
  group_by(region) %>%
  crosstab(education, employment, weights = sampling_weight)
#> ── Weighted Grouped Crosstabulation  ───────────────────────────────────────────
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

Start with basic descriptives to spot issues:

``` r
# Check for outliers and missing data
survey_data %>%
  describe(age, income, show = "all")
#> ── Descriptive Statistics ──────────────────────────────────────────────────────
#>  Variable     Mean Median       SD     SE Range  IQR Skewness Kurtosis
#>       age   50.550     50   16.976  0.340    77   24    0.172   -0.364
#>    income 3753.934   3500 1432.802 30.645  7200 1900    0.730    0.376
#>     Variance Mode  Q25  Q50  Q75    N Missing
#>      288.185   18   38   50   62 2500       0
#>  2052920.442 3200 2700 3500 4600 2186     314
#> ────────────────────────────────────────────────────────────────────────────────
```

Look for: - Data entry errors (impossible values) - Missing data
patterns - Distribution issues (extreme skewness)

### 2. Use Weights When Available

Unweighted results can be misleading. Here’s a comparison:

``` r
# Compare weighted vs unweighted
unweighted_mean <- survey_data %>%
  summarise(mean_age = mean(age, na.rm = TRUE)) %>%
  pull()

weighted_result <- w_mean(survey_data, age, weights = sampling_weight)

cat("Unweighted mean age:", unweighted_mean, "\n")
#> Unweighted mean age: 50.5496
cat("Weighted mean age:", weighted_result$results$weighted_mean, "\n")
#> Weighted mean age: 50.5144
```

### 3. Report the Right Percentage

- Use **Valid %** when missing values are truly missing (skip patterns)
- Use **Raw %** when “no response” is meaningful

### 4. Consider Your Audience

- For technical audiences: Include SD, skewness, kurtosis
- For general audiences: Focus on mean, median, percentages

## Complete Example

Here’s a typical descriptive analysis workflow:

``` r
# 1. Overall summary
cat("=== Overall Summary ===\n")
#> === Overall Summary ===
survey_data %>%
  describe(age, income, life_satisfaction,
           weights = sampling_weight,
           show = "short")
#> ── Weighted Descriptive Statistics ─────────────────────────────────────────────
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>                age   50.514     50   17.084    77   25    0.159      2468.8
#>             income 3743.099   3500 1423.966  7200 1900    0.724      2158.9
#>  life_satisfaction    3.625      4    1.152     4    2   -0.498      2390.9
#> ────────────────────────────────────────────────────────────────────────────────

# 2. Check distributions
cat("\n=== Key Frequencies ===\n")
#> 
#> === Key Frequencies ===
survey_data %>%
  frequency(education, employment,
            weights = sampling_weight)
#> ── Weighted Frequency Analysis Results ─────────────────────────────────────────
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

# 3. Examine relationships
cat("\n=== Education by Employment ===\n")
#> 
#> === Education by Employment ===
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

# 4. Group comparisons
cat("\n=== Regional Differences ===\n")
#> 
#> === Regional Differences ===
survey_data %>%
  group_by(region) %>%
  describe(income, life_satisfaction,
           weights = sampling_weight)
#> ── Weighted Descriptive Statistics ─────────────────────────────────────────────
#> 
#> ── Group: region = East ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>             income 3760.687   3600 1388.321  7200 1700    0.718       421.9
#>  life_satisfaction    3.623      4    1.203     4    2   -0.556       457.4
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Group: region = West ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>             income 3738.586   3500 1433.325  7200 1900    0.726      1738.1
#>  life_satisfaction    3.625      4    1.139     4    2   -0.481      1934.8
#> ────────────────────────────────────────────────────────────────────────────────
```

## Next Steps

Now that you understand your data’s basic patterns: - Test hypotheses
with
[`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)
or
[`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md) -
Explore relationships with
[`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)
or
[`spearman_rho()`](https://YannickDiehl.github.io/mariposa/reference/spearman_rho.md) -
Build models based on these insights

Remember: Good analysis starts with understanding your data!
