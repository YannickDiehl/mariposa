# Compare Multiple Groups Without Assuming Normal Data

`kruskal_wallis()` compares three or more groups when your data isn't
normally distributed or when you have ordinal data (like ratings or
rankings). It's the non-parametric alternative to one-way ANOVA.

Think of it as:

- An extension of the Mann-Whitney test for more than two groups

- A robust way to compare groups that works with any data shape

- Perfect for Likert scales, rankings, or skewed distributions

The test tells you:

- Whether at least one group is different from the others

- How strong the overall group effect is (effect size)

- Which groups tend to have higher or lower values (via mean ranks)

## Usage

``` r
kruskal_wallis(data, ..., group, weights = NULL, conf.level = 0.95)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The variables you want to compare between groups. You can list
  multiple variables or use helpers like `starts_with("satisfaction")`

- group:

  The categorical variable that defines your groups (e.g., education,
  employment). Must have at least 2 groups (3+ for meaningful use).

- weights:

  Optional survey weights for population-representative results

- conf.level:

  Confidence level for intervals (Default: 0.95 = 95%)

## Value

Test results showing whether groups differ, including:

- H statistic (Kruskal-Wallis chi-square test statistic)

- Degrees of freedom (number of groups minus 1)

- P-value (are groups different?)

- Effect size eta-squared (how big is the group effect?)

- Mean rank for each group (which groups are higher/lower?)

## Details

### Understanding the Results

**P-value**: If p \< 0.05, at least one group is significantly different

- p \< 0.001: Very strong evidence of group differences

- p \< 0.01: Strong evidence of group differences

- p \< 0.05: Moderate evidence of group differences

- p \> 0.05: No significant group differences found

**Effect Size Eta-squared** (How much do groups matter?):

- \< 0.01: Negligible effect

- 0.01-0.06: Small effect

- 0.06-0.14: Medium effect

- 0.14 or higher: Large effect

**Mean Ranks**:

- Higher mean rank = group tends to have higher values

- Lower mean rank = group tends to have lower values

- Compare mean ranks to see the pattern of group differences

### When to Use This

Use Kruskal-Wallis test when:

- Your data is not normally distributed (skewed, outliers)

- You have ordinal data (rankings, Likert scales)

- Sample sizes are small or very unequal across groups

- You want a robust alternative to one-way ANOVA

- You're comparing satisfaction ratings, income, or other skewed
  variables

### What Comes Next?

If the Kruskal-Wallis test is significant:

1.  Look at mean ranks to see the pattern

2.  Use pairwise Mann-Whitney tests with Bonferroni correction to find
    which specific groups differ

3.  Consider effect sizes to judge practical importance

### Relationship to Other Tests

- For 2 groups: Use
  [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)
  instead

- For normally distributed data: Use
  [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
  instead

- For repeated measures (same subjects): Use
  [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)
  instead

## References

Kruskal, W. H., & Wallis, W. A. (1952). Use of ranks in one-criterion
variance analysis. Journal of the American Statistical Association,
47(260), 583-621.

Tomczak, M., & Tomczak, E. (2014). The need to report effect size
estimates revisited. An overview of some recommended measures of effect
size. Trends in Sport Sciences, 1(21), 19-25.

## See also

[`kruskal.test`](https://rdrr.io/r/stats/kruskal.test.html) for the base
R Kruskal-Wallis test.

[`mann_whitney`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)
for comparing exactly two groups.

[`oneway_anova`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
for parametric one-way ANOVA.

Other hypothesis_tests:
[`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md),
[`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md),
[`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md),
[`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md),
[`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md),
[`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md),
[`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic Kruskal-Wallis test (comparing across education levels)
survey_data %>%
  kruskal_wallis(life_satisfaction, group = education)
#> 
#> Kruskal-Wallis Test Results
#> ---------------------------
#> 
#> - Grouping variable: education
#> - Groups: Basic Secondary, Intermediate Secondary, Academic Secondary, University
#> 
#> ── life_satisfaction ───────────────────────────────────────────────────────────
#> 
#>   Ranks:
#>   --------------------------------------
#>                     Group    N Mean Rank
#>           Basic Secondary  809    974.29
#>    Intermediate Secondary  618   1250.73
#>        Academic Secondary  607   1329.56
#>                University  387   1456.42
#>                     Total 2421        NA
#>   --------------------------------------
#> 
#>   Test Statistics:
#>   --------------------------------------------
#>    Kruskal-Wallis H df p value Eta-squared sig
#>             171.178  3       0       0.071 ***
#>   --------------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (Eta-squared):
#> - Small effect: 0.01 - 0.06
#> - Medium effect: 0.06 - 0.14
#> - Large effect: > 0.14

# Multiple variables
survey_data %>%
  kruskal_wallis(life_satisfaction, income, trust_government,
                 group = education)
#> 
#> Kruskal-Wallis Test Results
#> ---------------------------
#> 
#> - Grouping variable: education
#> - Groups: Basic Secondary, Intermediate Secondary, Academic Secondary, University
#> 
#> ── life_satisfaction ───────────────────────────────────────────────────────────
#> 
#>   Ranks:
#>   --------------------------------------
#>                     Group    N Mean Rank
#>           Basic Secondary  809    974.29
#>    Intermediate Secondary  618   1250.73
#>        Academic Secondary  607   1329.56
#>                University  387   1456.42
#>                     Total 2421        NA
#>   --------------------------------------
#> 
#>   Test Statistics:
#>   --------------------------------------------
#>    Kruskal-Wallis H df p value Eta-squared sig
#>             171.178  3       0       0.071 ***
#>   --------------------------------------------
#> 
#> ── income ──────────────────────────────────────────────────────────────────────
#> 
#>   Ranks:
#>   --------------------------------------
#>                     Group    N Mean Rank
#>           Basic Secondary  735    623.53
#>    Intermediate Secondary  548   1076.65
#>        Academic Secondary  548   1359.50
#>                University  355   1681.93
#>                     Total 2186        NA
#>   --------------------------------------
#> 
#>   Test Statistics:
#>   --------------------------------------------
#>    Kruskal-Wallis H df p value Eta-squared sig
#>             814.174  3       0       0.373 ***
#>   --------------------------------------------
#> 
#> ── trust_government ────────────────────────────────────────────────────────────
#> 
#>   Ranks:
#>   --------------------------------------
#>                     Group    N Mean Rank
#>           Basic Secondary  791   1191.27
#>    Intermediate Secondary  592   1156.01
#>        Academic Secondary  595   1170.90
#>                University  376   1192.82
#>                     Total 2354        NA
#>   --------------------------------------
#> 
#>   Test Statistics:
#>   --------------------------------------------
#>    Kruskal-Wallis H df p value Eta-squared sig
#>               1.235  3   0.745       0.001    
#>   --------------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (Eta-squared):
#> - Small effect: 0.01 - 0.06
#> - Medium effect: 0.06 - 0.14
#> - Large effect: > 0.14

# Using tidyselect helpers
survey_data %>%
  kruskal_wallis(starts_with("trust_"), group = education)
#> 
#> Kruskal-Wallis Test Results
#> ---------------------------
#> 
#> - Grouping variable: education
#> - Groups: Basic Secondary, Intermediate Secondary, Academic Secondary, University
#> 
#> ── trust_government ────────────────────────────────────────────────────────────
#> 
#>   Ranks:
#>   --------------------------------------
#>                     Group    N Mean Rank
#>           Basic Secondary  791   1191.27
#>    Intermediate Secondary  592   1156.01
#>        Academic Secondary  595   1170.90
#>                University  376   1192.82
#>                     Total 2354        NA
#>   --------------------------------------
#> 
#>   Test Statistics:
#>   --------------------------------------------
#>    Kruskal-Wallis H df p value Eta-squared sig
#>               1.235  3   0.745       0.001    
#>   --------------------------------------------
#> 
#> ── trust_media ─────────────────────────────────────────────────────────────────
#> 
#>   Ranks:
#>   --------------------------------------
#>                     Group    N Mean Rank
#>           Basic Secondary  797   1178.24
#>    Intermediate Secondary  594   1210.37
#>        Academic Secondary  599   1192.69
#>                University  377   1140.82
#>                     Total 2367        NA
#>   --------------------------------------
#> 
#>   Test Statistics:
#>   --------------------------------------------
#>    Kruskal-Wallis H df p value Eta-squared sig
#>               2.709  3   0.439       0.001    
#>   --------------------------------------------
#> 
#> ── trust_science ───────────────────────────────────────────────────────────────
#> 
#>   Ranks:
#>   --------------------------------------
#>                     Group    N Mean Rank
#>           Basic Secondary  807   1200.79
#>    Intermediate Secondary  610   1172.31
#>        Academic Secondary  597   1235.52
#>                University  384   1183.98
#>                     Total 2398        NA
#>   --------------------------------------
#> 
#>   Test Statistics:
#>   --------------------------------------------
#>    Kruskal-Wallis H df p value Eta-squared sig
#>               3.047  3   0.384       0.001    
#>   --------------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (Eta-squared):
#> - Small effect: 0.01 - 0.06
#> - Medium effect: 0.06 - 0.14
#> - Large effect: > 0.14

# Weighted analysis
survey_data %>%
  kruskal_wallis(life_satisfaction, group = education,
                 weights = sampling_weight)
#> 
#> Weighted Kruskal-Wallis Test Results
#> ------------------------------------
#> 
#> - Grouping variable: education
#> - Groups: Basic Secondary, Intermediate Secondary, Academic Secondary, University
#> - Weights variable: sampling_weight
#> 
#> ── life_satisfaction ───────────────────────────────────────────────────────────
#> 
#>   Ranks:
#>   ----------------------------------------
#>                     Group      N Mean Rank
#>           Basic Secondary  815.7    984.25
#>    Intermediate Secondary  629.6   1258.61
#>        Academic Secondary  618.2   1338.55
#>                University  373.2   1464.69
#>                     Total 2436.7        NA
#>   ----------------------------------------
#> 
#>   Weighted Test Statistics:
#>   --------------------------------------------
#>    Kruskal-Wallis H df p value Eta-squared sig
#>             167.149  3       0       0.069 ***
#>   --------------------------------------------
#> 
#> Note: Weighted analysis uses frequency-weighted ranks.
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (Eta-squared):
#> - Small effect: 0.01 - 0.06
#> - Medium effect: 0.06 - 0.14
#> - Large effect: > 0.14

# Grouped analysis (separate test for each region)
survey_data %>%
  group_by(region) %>%
  kruskal_wallis(life_satisfaction, group = education)
#> 
#> Kruskal-Wallis Test Results
#> ---------------------------
#> 
#> - Grouping variable: education
#> - Groups: Basic Secondary, Intermediate Secondary, Academic Secondary, University
#> 
#> 
#> Group: region = East
#> --------------------
#> 
#> ── life_satisfaction ───────────────────────────────────────────────────────────
#> 
#>   Ranks:
#>   -------------------------------------
#>                     Group   N Mean Rank
#>           Basic Secondary 161    202.35
#>    Intermediate Secondary 119    232.99
#>        Academic Secondary 110    254.85
#>                University  75    266.77
#>                     Total 465        NA
#>   -------------------------------------
#> 
#>   Test Statistics:
#>   --------------------------------------------
#>    Kruskal-Wallis H df p value Eta-squared sig
#>              17.105  3   0.001       0.037 ***
#>   --------------------------------------------
#> 
#> 
#> Group: region = West
#> --------------------
#> 
#> ── life_satisfaction ───────────────────────────────────────────────────────────
#> 
#>   Ranks:
#>   --------------------------------------
#>                     Group    N Mean Rank
#>           Basic Secondary  648    771.38
#>    Intermediate Secondary  499   1018.44
#>        Academic Secondary  497   1075.24
#>                University  312   1190.70
#>                     Total 1956        NA
#>   --------------------------------------
#> 
#>   Test Statistics:
#>   --------------------------------------------
#>    Kruskal-Wallis H df p value Eta-squared sig
#>             158.807  3       0       0.081 ***
#>   --------------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (Eta-squared):
#> - Small effect: 0.01 - 0.06
#> - Medium effect: 0.06 - 0.14
#> - Large effect: > 0.14

# Compare across employment status (5 groups)
survey_data %>%
  kruskal_wallis(income, group = employment)
#> 
#> Kruskal-Wallis Test Results
#> ---------------------------
#> 
#> - Grouping variable: employment
#> - Groups: Student, Employed, Unemployed, Retired, Other
#> 
#> ── income ──────────────────────────────────────────────────────────────────────
#> 
#>   Ranks:
#>   --------------------------
#>         Group    N Mean Rank
#>       Student   65   1495.58
#>      Employed 1390   1075.60
#>    Unemployed  159   1073.29
#>       Retired  471   1089.83
#>         Other  101   1129.95
#>         Total 2186        NA
#>   --------------------------
#> 
#>   Test Statistics:
#>   --------------------------------------------
#>    Kruskal-Wallis H df p value Eta-squared sig
#>              28.026  4       0       0.013 ***
#>   --------------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (Eta-squared):
#> - Small effect: 0.01 - 0.06
#> - Medium effect: 0.06 - 0.14
#> - Large effect: > 0.14
```
