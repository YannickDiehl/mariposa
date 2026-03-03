# Chi-Square Goodness-of-Fit Test

`chisq_gof()` tests whether observed frequencies of a categorical
variable match expected frequencies. By default, it tests against equal
proportions (uniform distribution). You can also specify custom expected
proportions.

Think of it as:

- Testing whether categories are equally distributed

- Comparing observed distribution to a theoretical distribution

- The one-sample version of the chi-square test

The test tells you:

- Whether observed frequencies differ from expected frequencies

- How strong the deviation is (chi-square statistic)

- A frequency table with observed, expected, and residual counts

## Usage

``` r
chisq_gof(data, ..., expected = NULL, weights = NULL)
```

## Arguments

- data:

  Your survey data (data frame or tibble)

- ...:

  One or more categorical variables to test (tidyselect supported)

- expected:

  Optional numeric vector of expected proportions (must sum to 1). Only
  used when a single variable is tested. If NULL (default), equal
  proportions are assumed.

- weights:

  Optional survey weights for population-representative results

## Value

Test results showing whether observed frequencies match expected,
including:

- Chi-square statistic and p-value for each variable

- Degrees of freedom

- Frequency table with observed, expected, and residual counts

- Sample size (N)

## Details

### Understanding the Results

**P-value**: If p \< 0.05, the distribution differs from expected

- p \< 0.001: Very strong evidence the distribution differs

- p \< 0.01: Strong evidence the distribution differs

- p \< 0.05: Moderate evidence the distribution differs

- p \>= 0.05: No significant deviation from expected distribution

**Residuals**: The difference between observed and expected counts.
Large positive residuals indicate a category has more cases than
expected; large negative residuals indicate fewer cases than expected.

### When to Use This

Use this test when:

- You want to check whether a categorical variable follows a specific
  distribution

- You want to test if categories are equally distributed (uniform)

- You have a single categorical variable and a hypothesised distribution

### The Chi-Square Goodness-of-Fit Statistic

\$\$\chi^2 = \sum \frac{(O_i - E_i)^2}{E_i}\$\$

where O_i = observed frequency, E_i = expected frequency.

Degrees of freedom = number of categories - 1.

### Relationship to Other Tests

- For testing association between two categorical variables: Use
  [`chi_square()`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md)
  instead

- For testing a single binary proportion: Use
  [`binomial_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/binomial_test.md)
  instead

- For small samples where expected frequencies are below 5: Use
  [`fisher_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/fisher_test.md)
  instead

### SPSS Equivalent

SPSS: `NPAR TESTS /CHISQUARE=variable /EXPECTED=EQUAL` or:
`NPAR TESTS /CHISQUARE=variable /EXPECTED=50 30 20`

## References

Pearson, K. (1900). On the criterion that a given system of deviations
from the probable in the case of a correlated system of variables is
such that it can be reasonably supposed to have arisen from random
sampling. *Philosophical Magazine*, 50(302), 157-175.

## See also

[`chi_square`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md)
for chi-square test of independence (two variables).

[`binomial_test`](https://YannickDiehl.github.io/mariposa/dev/reference/binomial_test.md)
for testing a single proportion.

Other hypothesis_tests:
[`ancova()`](https://YannickDiehl.github.io/mariposa/dev/reference/ancova.md),
[`binomial_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/binomial_test.md),
[`chi_square()`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md),
[`factorial_anova()`](https://YannickDiehl.github.io/mariposa/dev/reference/factorial_anova.md),
[`fisher_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/fisher_test.md),
[`friedman_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/friedman_test.md),
[`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/dev/reference/kruskal_wallis.md),
[`mann_whitney()`](https://YannickDiehl.github.io/mariposa/dev/reference/mann_whitney.md),
[`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/mcnemar_test.md),
[`oneway_anova()`](https://YannickDiehl.github.io/mariposa/dev/reference/oneway_anova.md),
[`t_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/t_test.md),
[`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/wilcoxon_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Test whether gender is equally distributed
survey_data %>%
  chisq_gof(gender)
#> Chi-Square Goodness-of-Fit Test Results
#> ---------------------------------------
#> 
#> - Variables: gender
#> - Expected: Equal proportions
#> 
#>   gender - Frequency Table:
#>   ------------------------------------
#>    category observed expected residual
#>        Male     1194     1250      -56
#>      Female     1306     1250       56
#>   ------------------------------------
#> 
#> ---------------------------------------- 
#>  Variable Chi-Square df p-value    N Sig 
#>    gender      5.018  1  0.0251 2500   * 
#> ---------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Test multiple variables at once
survey_data %>%
  chisq_gof(gender, region, education)
#> Chi-Square Goodness-of-Fit Test Results
#> ---------------------------------------
#> 
#> - Variables: gender, region, education
#> - Expected: Equal proportions
#> 
#>   gender - Frequency Table:
#>   ------------------------------------
#>    category observed expected residual
#>        Male     1194     1250      -56
#>      Female     1306     1250       56
#>   ------------------------------------
#> 
#>   region - Frequency Table:
#>   ------------------------------------
#>    category observed expected residual
#>        East      485     1250     -765
#>        West     2015     1250      765
#>   ------------------------------------
#> 
#>   education - Frequency Table:
#>   --------------------------------------------------
#>                  category observed expected residual
#>           Basic Secondary      841      625      216
#>    Intermediate Secondary      629      625        4
#>        Academic Secondary      631      625        6
#>                University      399      625     -226
#>   --------------------------------------------------
#> 
#> ----------------------------------------- 
#>   Variable Chi-Square df p-value    N Sig 
#>     gender      5.018  1  0.0251 2500   * 
#>     region    936.360  1   <.001 2500 *** 
#>  education    156.454  3   <.001 2500 *** 
#> ----------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Custom expected proportions
survey_data %>%
  chisq_gof(interview_mode, expected = c(0.5, 0.3, 0.2))
#> Chi-Square Goodness-of-Fit Test Results
#> ---------------------------------------
#> 
#> - Variables: interview_mode
#> - Expected: 0.5, 0.3, 0.2
#> 
#>   interview_mode - Frequency Table:
#>   ----------------------------------------
#>        category observed expected residual
#>    Face-to-face     1485     1250      235
#>       Telephone      655      750      -95
#>          Online      360      500     -140
#>   ----------------------------------------
#> 
#> ---------------------------------------------- 
#>        Variable Chi-Square df p-value    N Sig 
#>  interview_mode     95.413  2   <.001 2500 *** 
#> ---------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# With weights
survey_data %>%
  chisq_gof(gender, weights = sampling_weight)
#> Weighted Chi-Square Goodness-of-Fit Test Results
#> ------------------------------------------------
#> 
#> - Variables: gender
#> - Expected: Equal proportions
#> - Weights variable: sampling_weight
#> 
#>   gender - Frequency Table:
#>   ------------------------------------
#>    category observed expected residual
#>        Male     1195     1258      -63
#>      Female     1321     1258       63
#>   ------------------------------------
#> 
#> ---------------------------------------- 
#>  Variable Chi-Square df p-value    N Sig 
#>    gender       6.31  1  0.0120 2516   * 
#> ---------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Grouped analysis
survey_data %>%
  group_by(region) %>%
  chisq_gof(education)
#> Chi-Square Goodness-of-Fit Test Results
#> ---------------------------------------
#> 
#> - Variables: education
#> - Expected: Equal proportions
#> 
#> 
#> Group: region = East
#> --------------------
#> ---------------------------------------- 
#>   Variable Chi-Square df p-value   N Sig 
#>  education     34.645  3   <.001 485 *** 
#> ---------------------------------------- 
#> 
#> 
#> Group: region = West
#> --------------------
#> ----------------------------------------- 
#>   Variable Chi-Square df p-value    N Sig 
#>  education    122.888  3   <.001 2015 *** 
#> ----------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
