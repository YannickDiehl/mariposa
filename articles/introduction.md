# Introduction to mariposa

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
```

## What is mariposa?

mariposa (Marburg Initiative for Political and Social Analysis) makes it
easy to analyze survey data in R. Whether you’re working with customer
satisfaction surveys, social research, or market research data, this
package helps you get insights quickly and accurately.

The package handles the complexities of survey analysis for you - things
like sampling weights, grouped comparisons, and missing data are all
taken care of automatically.

## Getting Started

Let’s start with a simple example. We’ll use the included survey data
which contains responses from 2,500 people about their life
satisfaction, demographics, and opinions.

``` r
# Load the example survey data
data(survey_data)

# Take a quick look at what we have
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

## Understanding Your Data

### Getting Summary Statistics

Want to know the average age or income in your survey? The
[`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
function gives you a complete summary:

``` r
# Get a summary of age and income
# The weights ensure results represent the population correctly
survey_data %>%
  describe(age, income, weights = sampling_weight)
#> ── Weighted Descriptive Statistics ─────────────────────────────────────────────
#>  Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>       age   50.514     50   17.084    77   25    0.159      2468.8
#>    income 3743.099   3500 1423.966  7200 1900    0.724      2158.9
#> ────────────────────────────────────────────────────────────────────────────────
```

This shows you: - **n**: How many valid responses - **mean**: The
average value - **sd**: Standard deviation (how spread out values are) -
**median**: The middle value - **range**: Minimum and maximum values

### Looking at Categories

For categorical data like education or employment status, use
[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md):

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

This tells you: - How many people are in each category - What percentage
they represent - Cumulative percentages

## Comparing Groups

### Are There Differences?

Want to know if men and women differ in life satisfaction? Use a t-test:

``` r
survey_data %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)
#> ── Weighted t-Test Results ─────────────────────────────────────────────────────
#> • Grouping variable: gender
#> • Groups compared: Male vs. Female
#> • Weights variable: sampling_weight
#> • Confidence level: 95.0%
#> • Alternative hypothesis: two.sided
#> • Null hypothesis (mu): 0.000
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
#> Signif. codes: 0 *** 0.001 ** 0.01 * 0.05
#> 
#> Effect Size Interpretation:
#> - Cohen's d: pooled standard deviation (classic)
#> - Hedges' g: bias-corrected Cohen's d (preferred)
#> - Glass' Delta: control group standard deviation only
#> - Small effect: |effect| ~ 0.2
#> - Medium effect: |effect| ~ 0.5
#> - Large effect: |effect| ~ 0.8
```

If the p-value is less than 0.05, there’s a statistically significant
difference.

### Multiple Group Comparisons

When you have more than two groups, use ANOVA:

``` r
survey_data %>%
  oneway_anova(life_satisfaction, group = education, weights = sampling_weight)
#> ── Weighted One-Way ANOVA Results ──────────────────────────────────────────────
#> 
#> Dependent Variable: life_satisfaction
#> Grouping Variable: education
#> Weights Variable: sampling_weight
#> Null hypothesis: All group means are equal
#> Alternative hypothesis: At least one group mean differs
#> Confidence level: 95.0%
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Weighted Descriptive Statistics by Group:
#>   Basic Secondary: mean = 3.208, sd = 1.243, n = 815.7
#>   Intermediate Secondary: mean = 3.698, sd = 1.109, n = 629.6
#>   Academic Secondary: mean = 3.851, sd = 0.996, n = 618.2
#>   University: mean = 4.040, sd = 0.961, n = 373.2
#> 
#> Weighted ANOVA Results:
#> -------------------------------------------------------------------------------- 
#>          Source Sum_Squares   df Mean_Square      F p_value sig
#>  Between Groups     241.130    3      80.377 65.359   <.001 ***
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

## Finding Relationships

### Correlation Analysis

Are age and income related? Check their correlation:

``` r
survey_data %>%
  pearson_cor(age, income, weights = sampling_weight)
#> ── Weighted Pearson Correlation  ───────────────────────────────────────────────
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> --- age × income ---
#> 
#>   Correlation: r = -0.005
#>   Effect size: r² = 0.000
#>   Sample size: n = 2201
#>   95% CI: [-0.046, 0.037]
#>   p-value: 0.8276
#>   Significance: ns
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Correlation Strength Interpretation:
#>   |r| < 0.30:        Weak correlation
#>   0.30 ≤ |r| < 0.70: Moderate correlation
#>   |r| ≥ 0.70:        Strong correlation
#> 
#> r² represents the proportion of variance explained
```

- Positive correlation: As one goes up, the other tends to go up
- Negative correlation: As one goes up, the other tends to go down
- Near zero: No linear relationship

### Cross-tabulation

See how two categorical variables relate:

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

## Working with Groups

### Regional Analysis

Compare statistics across regions easily:

``` r
survey_data %>%
  group_by(region) %>%
  describe(income, life_satisfaction, weights = sampling_weight)
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

### Multiple Tests at Once

Test several variables simultaneously:

``` r
survey_data %>%
  t_test(trust_government, trust_media, trust_science,
         group = gender, weights = sampling_weight)
#> ── Weighted t-Test Results ─────────────────────────────────────────────────────
#> • Grouping variable: gender
#> • Groups compared: Male vs. Female
#> • Weights variable: sampling_weight
#> • Confidence level: 95.0%
#> • Alternative hypothesis: two.sided
#> • Null hypothesis (mu): 0.000
#> 
#> 
#> --- trust_government ---
#> 
#>   Male: mean = 2.603, n = 1124.0
#>   Female: mean = 2.636, n = 1247.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat       df p_value mean_diff        conf_int sig
#>    Equal variances -0.683 2369.000   0.495    -0.033 [-0.127, 0.061]    
#>  Unequal variances -0.682 2322.704   0.495    -0.033 [-0.127, 0.061]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>          Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>  trust_government   -0.028   -0.028      -0.028  negligible
#> 
#> 
#> --- trust_media ---
#> 
#>   Male: mean = 2.400, n = 1128.0
#>   Female: mean = 2.505, n = 1254.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat       df p_value mean_diff         conf_int sig
#>    Equal variances -2.197 2380.000   0.028    -0.105 [-0.199, -0.011]   *
#>  Unequal variances -2.196 2350.189   0.028    -0.105 [-0.199, -0.011]   *
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>     Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>  trust_media    -0.09    -0.09       -0.09  negligible
#> 
#> 
#> --- trust_science ---
#> 
#>   Male: mean = 3.610, n = 1143.0
#>   Female: mean = 3.669, n = 1271.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat      df p_value mean_diff        conf_int sig
#>    Equal variances -1.424 2412.00   0.155     -0.06 [-0.142, 0.022]    
#>  Unequal variances -1.421 2360.95   0.156     -0.06 [-0.142, 0.023]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>       Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>  trust_science   -0.058   -0.058      -0.057  negligible
#> 
#> Signif. codes: 0 *** 0.001 ** 0.01 * 0.05
#> 
#> Effect Size Interpretation:
#> - Cohen's d: pooled standard deviation (classic)
#> - Hedges' g: bias-corrected Cohen's d (preferred)
#> - Glass' Delta: control group standard deviation only
#> - Small effect: |effect| ~ 0.2
#> - Medium effect: |effect| ~ 0.5
#> - Large effect: |effect| ~ 0.8
```

## Why Weights Matter

Survey weights make your results representative of the population.
Here’s the difference they make:

``` r
# Without weights - just your sample
unweighted <- mean(survey_data$age, na.rm = TRUE)

# With weights - represents the population
weighted_result <- w_mean(survey_data, age, weights = sampling_weight)
weighted <- weighted_result$results$weighted_mean

cat("Sample average age:", round(unweighted, 1), "\n")
#> Sample average age: 50.5
cat("Population average age:", round(weighted, 1), "\n")
#> Population average age: 50.5
cat("Difference:", round(weighted - unweighted, 1), "years\n")
#> Difference: 0 years
```

## Common Analysis Workflow

Here’s a typical analysis pattern:

``` r
# 1. Start with descriptive statistics
cat("=== Basic Summary ===\n")
#> === Basic Summary ===
survey_data %>%
  describe(life_satisfaction, weights = sampling_weight)
#> ── Weighted Descriptive Statistics ─────────────────────────────────────────────
#>           Variable  Mean Median    SD Range IQR Skewness Effective_N
#>  life_satisfaction 3.625      4 1.152     4   2   -0.498      2390.9
#> ────────────────────────────────────────────────────────────────────────────────

# 2. Check distribution across groups
cat("\n=== By Education Level ===\n")
#> 
#> === By Education Level ===
survey_data %>%
  group_by(education) %>%
  describe(life_satisfaction, weights = sampling_weight)
#> ── Weighted Descriptive Statistics ─────────────────────────────────────────────
#> 
#> ── Group: education = Basic Secondary ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>           Variable  Mean Median    SD Range IQR Skewness Effective_N
#>  life_satisfaction 3.208      3 1.243     4   2   -0.056       801.2
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Group: education = Intermediate Secondary ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>           Variable  Mean Median   SD Range IQR Skewness Effective_N
#>  life_satisfaction 3.698      4 1.11     4   2    -0.59       611.8
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Group: education = Academic Secondary ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>           Variable  Mean Median    SD Range IQR Skewness Effective_N
#>  life_satisfaction 3.851      4 0.997     4   2    -0.58       600.6
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Group: education = University ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>           Variable Mean Median    SD Range IQR Skewness Effective_N
#>  life_satisfaction 4.04      4 0.962     4   1   -0.963       377.8
#> ────────────────────────────────────────────────────────────────────────────────

# 3. Test for significant differences
cat("\n=== Statistical Test ===\n")
#> 
#> === Statistical Test ===
result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)
print(result)
#> ── Weighted One-Way ANOVA Results ──────────────────────────────────────────────
#> 
#> Dependent Variable: life_satisfaction
#> Grouping Variable: education
#> Weights Variable: sampling_weight
#> Null hypothesis: All group means are equal
#> Alternative hypothesis: At least one group mean differs
#> Confidence level: 95.0%
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Weighted Descriptive Statistics by Group:
#>   Basic Secondary: mean = 3.208, sd = 1.243, n = 815.7
#>   Intermediate Secondary: mean = 3.698, sd = 1.109, n = 629.6
#>   Academic Secondary: mean = 3.851, sd = 0.996, n = 618.2
#>   University: mean = 4.040, sd = 0.961, n = 373.2
#> 
#> Weighted ANOVA Results:
#> -------------------------------------------------------------------------------- 
#>          Source Sum_Squares   df Mean_Square      F p_value sig
#>  Between Groups     241.130    3      80.377 65.359   <.001 ***
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

# 4. If significant, see which groups differ
# Check if result has p_value column (data frame structure)
if (!is.null(result) && "p_value" %in% names(result) && result$p_value[1] < 0.05) {
  cat("\n=== Post-hoc Comparisons ===\n")
  tukey_test(result)
}
```

## Tips for Success

### 1. Always Start with Descriptives

Before running tests, understand your data:

``` r
survey_data %>%
  describe(income, show = "all", weights = sampling_weight)
#> ── Weighted Descriptive Statistics ─────────────────────────────────────────────
#>  Variable     Mean Median       SD     SE Range  IQR Skewness Kurtosis Variance
#>    income 3743.099   3500 1423.966 30.353  7200 1900    0.724    0.384  2027678
#>  Mode  Q25  Q50  Q75 Effective_N
#>  3200 2700 3500 4600      2158.9
#> ────────────────────────────────────────────────────────────────────────────────
```

### 2. Check Your Assumptions

Many tests assume normal distribution. Check skewness: - Skewness near
0: Roughly normal - Skewness \> 1 or \< -1: Quite skewed

### 3. Use Appropriate Tests

- **Continuous data** (age, income): t-test, ANOVA, Pearson correlation
- **Ordinal data** (ratings): Mann-Whitney, Spearman correlation
- **Categorical data** (yes/no): Chi-square test

### 4. Report Effect Sizes

Statistical significance doesn’t always mean practical importance. Check
effect sizes!

## Getting Help

### Function Documentation

Every function has detailed help:

``` r
?describe
?t_test
?pearson_cor
```

### Vignettes

For detailed guides: -
[`vignette("descriptive-statistics")`](https://YannickDiehl.github.io/mariposa/articles/descriptive-statistics.md) -
Deep dive into summaries and frequencies -
[`vignette("hypothesis-testing")`](https://YannickDiehl.github.io/mariposa/articles/hypothesis-testing.md) -
All about comparing groups -
[`vignette("correlation-analysis")`](https://YannickDiehl.github.io/mariposa/articles/correlation-analysis.md) -
Understanding relationships -
[`vignette("survey-weights")`](https://YannickDiehl.github.io/mariposa/articles/survey-weights.md) -
Working with weighted data

### Workflow Example

Here’s how to analyze a survey question from start to finish:

``` r
# Question: Does income relate to life satisfaction?

# 1. Look at both variables
cat("=== Income Summary ===\n")
#> === Income Summary ===
survey_data %>%
  describe(income, weights = sampling_weight)
#> ── Weighted Descriptive Statistics ─────────────────────────────────────────────
#>  Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>    income 3743.099   3500 1423.966  7200 1900    0.724      2158.9
#> ────────────────────────────────────────────────────────────────────────────────

cat("\n=== Life Satisfaction Summary ===\n")
#> 
#> === Life Satisfaction Summary ===
survey_data %>%
  describe(life_satisfaction, weights = sampling_weight)
#> ── Weighted Descriptive Statistics ─────────────────────────────────────────────
#>           Variable  Mean Median    SD Range IQR Skewness Effective_N
#>  life_satisfaction 3.625      4 1.152     4   2   -0.498      2390.9
#> ────────────────────────────────────────────────────────────────────────────────

# 2. Check correlation
cat("\n=== Correlation ===\n")
#> 
#> === Correlation ===
cor_result <- survey_data %>%
  pearson_cor(income, life_satisfaction, weights = sampling_weight)
print(cor_result)
#> ── Weighted Pearson Correlation  ───────────────────────────────────────────────
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> --- income × life_satisfaction ---
#> 
#>   Correlation: r = 0.450
#>   Effect size: r² = 0.203
#>   Sample size: n = 2130
#>   95% CI: [0.416, 0.483]
#>   p-value: 0.0000
#>   Significance: ***
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Correlation Strength Interpretation:
#>   |r| < 0.30:        Weak correlation
#>   0.30 ≤ |r| < 0.70: Moderate correlation
#>   |r| ≥ 0.70:        Strong correlation
#> 
#> r² represents the proportion of variance explained

# 3. Interpret
if (!is.null(cor_result) && "p_value" %in% names(cor_result) && cor_result$p_value[1] < 0.05) {
  if (cor_result$correlation[1] > 0) {
    cat("\nHigher income is associated with greater life satisfaction.\n")
  } else {
    cat("\nHigher income is associated with lower life satisfaction.\n")
  }
  cat("Correlation strength:", round(cor_result$correlation[1], 2),
      "(", ifelse(abs(cor_result$correlation[1]) < 0.3, "weak",
                  ifelse(abs(cor_result$correlation[1]) < 0.7, "moderate", "strong")),
      ")\n")
} else {
  cat("\nNo significant relationship found between income and life satisfaction.\n")
}
#> 
#> No significant relationship found between income and life satisfaction.
```

## Next Steps

Now that you understand the basics:

1.  **Explore your own data** - Start with
    [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
    and
    [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
2.  **Test your hypotheses** - Use t-tests, ANOVA, or chi-square
3.  **Dig deeper** - Check out the other vignettes for advanced
    techniques
4.  **Stay accurate** - Always use weights when provided

Remember: Good analysis tells a story. Start broad, then focus on what’s
interesting!

## Quick Reference

| Task                | Function                                                                              | Example                                   |
|---------------------|---------------------------------------------------------------------------------------|-------------------------------------------|
| Summarize numeric   | [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)         | `describe(age, income)`                   |
| Count categories    | [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)       | `frequency(education)`                    |
| Compare 2 groups    | [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)             | `t_test(income, group = gender)`          |
| Compare 3+ groups   | [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md) | `oneway_anova(income, group = education)` |
| Test association    | [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)     | `chi_square(education, employment)`       |
| Measure correlation | [`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)   | `pearson_cor(age, income)`                |
| Cross-tabulate      | [`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)         | `crosstab(education, region)`             |

All functions support: - ✓ Survey weights via `weights =` - ✓ Grouped
analysis via
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html) - ✓
Multiple variables at once - ✓ Missing data handling

Welcome to mariposa - happy analyzing!
