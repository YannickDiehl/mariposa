# Fisher's Exact Test for Small Samples

`fisher_test()` performs Fisher's exact test for independence in a
contingency table. Use this instead of
[`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
when your sample is small or when expected cell frequencies are below 5.

Think of it as:

- An exact alternative to the chi-square test of independence

- Best for small samples where the chi-square approximation may be
  inaccurate

- SPSS automatically reports Fisher's Exact Test for 2x2 tables

The test tells you:

- Whether two categorical variables are related (exact p-value)

- The contingency table of observed frequencies

- Results that are valid even with very small samples

## Usage

``` r
fisher_test(data, row, col, weights = NULL, ...)
```

## Arguments

- data:

  Your survey data (data frame or tibble)

- row:

  The row variable (categorical)

- col:

  The column variable (categorical)

- weights:

  Optional survey weights for population-representative results

- ...:

  Additional arguments (currently unused)

## Value

Test results showing whether two categorical variables are related,
including:

- Exact p-value (two-sided)

- Contingency table of observed frequencies

- Sample size (N)

## Details

### Understanding the Results

**P-value**: If p \< 0.05, the variables are likely related (not
independent)

- p \< 0.001: Very strong evidence of relationship

- p \< 0.01: Strong evidence of relationship

- p \< 0.05: Moderate evidence of relationship

- p \>= 0.05: No significant relationship found

Unlike the chi-square test which uses a large-sample approximation,
Fisher's exact test computes the exact probability of observing the
given table (or a more extreme one) under the null hypothesis of
independence.

For tables larger than 2x2, the Fisher-Freeman-Halton extension is used.

### When to Use This

Use Fisher's exact test when:

- Any expected cell frequency is less than 5

- Your total sample size is small (typically N \< 30)

- You have a 2x2 table (Fisher's exact is standard here)

- You want an exact p-value rather than an approximation

### Relationship to Other Tests

- For large samples with all expected frequencies \>= 5: Use
  [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  instead

- For paired binary data (before/after): Use
  [`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md)
  instead

- For testing a single proportion: Use
  [`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md)
  instead

### SPSS Equivalent

SPSS: `CROSSTABS /STATISTICS=CHISQ` (Fisher's exact test is
automatically reported for 2x2 tables)

## References

Fisher, R. A. (1922). On the interpretation of chi-square from
contingency tables, and the calculation of P. *Journal of the Royal
Statistical Society*, 85(1), 87-94.

Freeman, G. H., & Halton, J. H. (1951). Note on an exact treatment of
contingency, goodness of fit and other problems of significance.
*Biometrika*, 38(1/2), 141-149.

## See also

[`chi_square`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
for chi-square test of independence (large samples).

[`mcnemar_test`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md)
for paired proportions.

Other hypothesis_tests:
[`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md),
[`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md),
[`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md),
[`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md),
[`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md),
[`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md),
[`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md),
[`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md),
[`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md),
[`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md),
[`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md),
[`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic 2x2 Fisher test
survey_data %>%
  fisher_test(row = gender, col = region)
#> Fisher's Exact Test Results
#> ---------------------------
#> 
#> - Row variable: gender
#> - Column variable: region
#> 
#> Contingency Table:
#> ---------------------------------------- 
#>         cc
#> r        East West
#>   Male    238  956
#>   Female  247 1059
#> ---------------------------------------- 
#> 
#> Test Results:
#> ---------------------------------------------------- 
#>                              Method p-value    N Sig 
#>  Fisher's Exact Test for Count Data  0.5435 2500     
#> ---------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Fisher test for larger table
survey_data %>%
  fisher_test(row = gender, col = interview_mode)
#> Fisher's Exact Test Results
#> ---------------------------
#> 
#> - Row variable: gender
#> - Column variable: interview_mode
#> 
#> Contingency Table:
#> ---------------------------------------- 
#>         cc
#> r        Face-to-face Telephone Online
#>   Male            720       307    167
#>   Female          765       348    193
#> ---------------------------------------- 
#> 
#> Test Results:
#> ---------------------------------------------------- 
#>                              Method p-value    N Sig 
#>  Fisher's Exact Test for Count Data  0.6782 2500     
#> ---------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# With weights
survey_data %>%
  fisher_test(row = gender, col = region, weights = sampling_weight)
#> Weighted Fisher's Exact Test Results
#> ------------------------------------
#> 
#> - Row variable: gender
#> - Column variable: region
#> - Weights variable: sampling_weight
#> 
#> Contingency Table:
#> ---------------------------------------- 
#>         cc
#> r        East West
#>   Male    249  945
#>   Female  260 1062
#> ---------------------------------------- 
#> 
#> Test Results:
#> ---------------------------------------------------- 
#>                              Method p-value    N Sig 
#>  Fisher's Exact Test for Count Data  0.4867 2516     
#> ---------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Grouped analysis
survey_data %>%
  group_by(education) %>%
  fisher_test(row = gender, col = region)
#> Fisher's Exact Test Results
#> ---------------------------
#> 
#> - Row variable: gender
#> - Column variable: region
#> 
#> 
#> Group: education = Basic Secondary
#> ----------------------------------
#> ---------------- 
#>  p-value   N Sig 
#>   0.9315 841     
#> ---------------- 
#> 
#> 
#> Group: education = Intermediate Secondary
#> -----------------------------------------
#> ---------------- 
#>  p-value   N Sig 
#>   0.4170 629     
#> ---------------- 
#> 
#> 
#> Group: education = Academic Secondary
#> -------------------------------------
#> ---------------- 
#>  p-value   N Sig 
#>   0.9181 631     
#> ---------------- 
#> 
#> 
#> Group: education = University
#> -----------------------------
#> ---------------- 
#>  p-value   N Sig 
#>   0.9003 399     
#> ---------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
