# Test If Two Categories Are Related

`chi_square()` helps you discover if two categorical variables are
related or independent. For example, is education level related to
voting preference? Or are they independent of each other?

The test tells you:

- Whether the relationship is statistically significant

- How strong the relationship is (effect sizes)

- What patterns exist in your data

## Usage

``` r
chi_square(data, ..., weights = NULL, correct = FALSE)

phi(data, ..., weights = NULL, correct = FALSE)

cramers_v(data, ..., weights = NULL, correct = FALSE)

goodman_gamma(data, ..., weights = NULL, correct = FALSE)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  Two categorical variables to test (e.g., gender, region)

- weights:

  Optional survey weights for population-representative results

- correct:

  Apply continuity correction for small samples? (Default: FALSE)

## Value

Test results showing whether the variables are related, including:

- Chi-squared statistic and p-value

- Observed vs expected frequencies

- Effect sizes to measure relationship strength

## Details

### Understanding the Results

**P-value**: If p \< 0.05, the variables are likely related (not
independent)

- p \< 0.001: Very strong evidence of relationship

- p \< 0.01: Strong evidence of relationship

- p \< 0.05: Moderate evidence of relationship

- p ≥ 0.05: No significant relationship found

**Effect Sizes** (How strong is the relationship?):

- **Cramer's V**: Works for any table size (0 = no relationship, 1 =
  perfect relationship)

  - \< 0.1: Negligible relationship

  - 0.1-0.3: Small relationship

  - 0.3-0.5: Medium relationship

  - 0.5 or higher: Large relationship

- **Phi**: Only for 2x2 tables (similar interpretation as Cramer's V)

- **Gamma**: For ordinal data (-1 to +1, shows direction of
  relationship)

### When to Use This

Use chi-squared test when:

- Both variables are categorical (gender, region, education level, etc.)

- You want to know if they're related or independent

- You have at least 5 observations in most cells

### Reading the Frequency Tables

- **Observed**: What you actually found in your data

- **Expected**: What you'd expect if variables were independent

- Large differences suggest a relationship exists

### Tips for Success

- Check that most cells have at least 5 observations

- Use weights for population estimates

- Look at both significance (p-value) and strength (effect sizes)

- Consider using crosstab() for detailed percentage breakdowns

## See also

Other hypothesis_tests:
[`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md),
[`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md),
[`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
data(survey_data)

# Basic chi-squared test for independence
survey_data %>% chi_square(gender, region)
#> 
#> ── Chi-Squared Test of Independence  ───────────────────────────────────────────
#> 
#> Variables: gender x region
#> 
#> Observed Frequencies:
#>         var2
#> var1     East West
#>   Male    238  956
#>   Female  247 1059
#> 
#> Expected Frequencies:
#>         var2
#> var1        East     West
#>   Male   231.636  962.364
#>   Female 253.364 1052.636
#> 
#> Chi-Squared Test Results:
#> -------------------------------------------------- 
#>  Chi_squared df p_value sig
#>        0.415  1   0.519    
#> -------------------------------------------------- 
#> 
#> Effect Sizes:
#> ---------------------------------------------------------------------- 
#>     Measure Value p_value sig Interpretation
#>  Cramer's V 0.013   0.519            Neglig.
#>         Phi 0.013   0.519            Neglig.
#>       Gamma 0.033   0.659               Weak
#> ---------------------------------------------------------------------- 
#> Table size: 2×2 | N = 2500
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# With weights
survey_data %>% chi_square(gender, education, weights = sampling_weight)
#> 
#> ── Weighted Chi-Squared Test of Independence  ──────────────────────────────────
#> 
#> Variables: gender x education
#> Weights variable: sampling_weight
#> 
#> Observed Frequencies:
#>         var2
#> var1     Basic Secondary Intermediate Secondary Academic Secondary University
#>   Male               402                    291                326        176
#>   Female             447                    350                316        209
#> 
#> Expected Frequencies:
#>         var2
#> var1     Basic Secondary Intermediate Secondary Academic Secondary University
#>   Male           403.081                304.329            304.803    182.787
#>   Female         445.919                336.671            337.197    202.213
#> 
#> Chi-Squared Test Results:
#> -------------------------------------------------- 
#>  Chi_squared df p_value sig
#>        4.403  3   0.221    
#> -------------------------------------------------- 
#> 
#> Effect Sizes:
#> ---------------------------------------------------------------------- 
#>     Measure  Value p_value sig Interpretation
#>  Cramer's V  0.042   0.221            Neglig.
#>       Gamma -0.011   0.795               Weak
#> ---------------------------------------------------------------------- 
#> Table size: 2×4 | N = 2517
#> Note: Phi coefficient only shown for 2x2 tables
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Grouped analysis
survey_data %>% 
  group_by(region) %>% 
  chi_square(gender, employment)
#> 
#> ── Chi-Squared Test of Independence  ───────────────────────────────────────────
#> 
#> Variables tested: gender x employment 
#> Grouped by: region 
#> 
#> --- Group: region = East ---
#> 
#> Observed Frequencies:
#>         var2
#> var1     Student Employed Unemployed Retired Other
#>   Male         7      145         16      55    15
#>   Female       4      166         15      56     6
#> 
#> Chi-Squared Test Results:
#> -------------------------------------------------- 
#>  Chi_squared df p_value sig
#>         5.97  4   0.201    
#> -------------------------------------------------- 
#> 
#> Effect Sizes:
#> ---------------------------------------------------------------------- 
#>     Measure  Value p_value sig Interpretation
#>  Cramer's V  0.111   0.201              Small
#>       Gamma -0.093   0.468               Weak
#> ---------------------------------------------------------------------- 
#> Table size: 2×5 | N = 485
#> Note: Phi coefficient only shown for 2x2 tables
#> 
#> --- Group: region = West ---
#> 
#> Observed Frequencies:
#>         var2
#> var1     Student Employed Unemployed Retired Other
#>   Male        29      605         68     201    53
#>   Female      38      684         83     213    41
#> 
#> Chi-Squared Test Results:
#> -------------------------------------------------- 
#>  Chi_squared df p_value sig
#>        4.166  4   0.384    
#> -------------------------------------------------- 
#> 
#> Effect Sizes:
#> ---------------------------------------------------------------------- 
#>     Measure  Value p_value sig Interpretation
#>  Cramer's V  0.045   0.384            Neglig.
#>       Gamma -0.053   0.381               Weak
#> ---------------------------------------------------------------------- 
#> Table size: 2×5 | N = 2015
#> Note: Phi coefficient only shown for 2x2 tables
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# With continuity correction
survey_data %>% chi_square(gender, region, correct = TRUE)
#> 
#> ── Chi-Squared Test of Independence  ───────────────────────────────────────────
#> 
#> Variables: gender x region
#> Yates' continuity correction applied
#> 
#> Observed Frequencies:
#>         var2
#> var1     East West
#>   Male    238  956
#>   Female  247 1059
#> 
#> Expected Frequencies:
#>         var2
#> var1        East     West
#>   Male   231.636  962.364
#>   Female 253.364 1052.636
#> 
#> Chi-Squared Test Results:
#> -------------------------------------------------- 
#>  Chi_squared df p_value sig
#>        0.353  1   0.553    
#> -------------------------------------------------- 
#> 
#> Effect Sizes:
#> ---------------------------------------------------------------------- 
#>     Measure Value p_value sig Interpretation
#>  Cramer's V 0.012   0.553            Neglig.
#>         Phi 0.012   0.553            Neglig.
#>       Gamma 0.033   0.659               Weak
#> ---------------------------------------------------------------------- 
#> Table size: 2×2 | N = 2500
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
