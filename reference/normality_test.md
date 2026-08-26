# Test Variables for Normality

`normality_test()` checks whether numeric variables follow a normal
distribution, producing the two tests SPSS prints in its EXAMINE "Tests
of Normality" table:

- **Kolmogorov-Smirnov** with Lilliefors significance correction (the
  "Kolmogorov-Smirnov(a)" column in SPSS)

- **Shapiro-Wilk** (computed for n between 3 and 5000, matching the SPSS
  convention)

Use it before a t-test, ANOVA, or Pearson correlation to check the
normality assumption, together with
[`levene_test`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)
for the equal-variances assumption.

## Usage

``` r
normality_test(data, ...)
```

## Arguments

- data:

  Your survey data (a data frame or tibble). If grouped (via
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)),
  the tests are run separately per group — matching SPSS
  `EXAMINE ... BY factor`.

- ...:

  Variables to test (unquoted, supports tidyselect). All selected
  variables must be numeric.

## Value

An object of class `"normality_test"` whose `$results` tibble holds one
row per variable (and group combination) with:

- n:

  Number of valid (non-missing) cases

- ks_statistic, ks_df, ks_p:

  Kolmogorov-Smirnov statistic with Lilliefors-corrected p-value
  (Dallal-Wilkinson approximation); df equals n as in SPSS

- shapiro_w, shapiro_p:

  Shapiro-Wilk W and p-value (`NA` when n \< 3 or n \> 5000)

## Details

### Understanding the Output

For both tests the null hypothesis is "the variable is normally
distributed":

- **p \>= 0.05**: No significant deviation from normality detected.

- **p \< 0.05**: The variable deviates significantly from a normal
  distribution.

With large survey samples these tests flag even tiny, practically
irrelevant deviations. Combine them with the skewness and kurtosis from
[`describe`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
before deciding against a parametric test.

### When to Use This

- Before
  [`t_test`](https://YannickDiehl.github.io/mariposa/reference/t_test.md),
  [`oneway_anova`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md),
  or
  [`pearson_cor`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)
  to check the normality assumption

- Grouped (via
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html))
  to check normality *within* each comparison group — the form of the
  assumption that actually matters for group comparisons

If normality is clearly violated, consider the rank-based alternatives:
[`mann_whitney`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md),
[`kruskal_wallis`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md),
or
[`spearman_rho`](https://YannickDiehl.github.io/mariposa/reference/spearman_rho.md).

### Technical Details

The Lilliefors-corrected p-value uses the Dallal-Wilkinson (1986)
approximation, the same correction SPSS applies in EXAMINE ("Lilliefors
Significance Correction"). Shapiro-Wilk uses
[`stats::shapiro.test()`](https://rdrr.io/r/stats/shapiro.test.html) and
is reported for 3 \<= n \<= 5000, as in SPSS. Cases with missing values
are excluded per variable.

**Weights**: `normality_test()` deliberately takes no `weights`
argument. Normality checks are a diagnostic of the *sample*
distribution, and neither Shapiro-Wilk nor the Lilliefors correction has
a well-defined fractional-frequency-weight form. Run the test on the
unweighted sample.

An SPSS v29 EXAMINE reference run is pending; until it lands the
statistics are verified against independent R implementations of the
same published formulas (see the SPSS compatibility vignette).

## References

Dallal, G. E., & Wilkinson, L. (1986). An analytic approximation to the
distribution of Lilliefors's test statistic for normality. *The American
Statistician*, 40(4), 294-296.

## See also

[`levene_test`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)
for the equal-variances assumption.

[`describe`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
for skewness and kurtosis.

[`summary.normality_test`](https://YannickDiehl.github.io/mariposa/reference/summary.normality_test.md)
for detailed output.

Other descriptive:
[`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md),
[`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md),
[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md),
[`multiple_response()`](https://YannickDiehl.github.io/mariposa/reference/multiple_response.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# Test a single variable
normality_test(survey_data, age)
#> Normality Tests: age
#>   age: KS = 0.028, p < 0.001; Shapiro-Wilk W = 0.990, p < 0.001 (n = 2500)
#> Use summary() for detailed output.

# Several variables at once
normality_test(survey_data, age, income, life_satisfaction)
#> Normality Tests: age, income, life_satisfaction
#>   age: KS = 0.028, p < 0.001; Shapiro-Wilk W = 0.990, p < 0.001 (n = 2500)
#>   income: KS = 0.079, p < 0.001; Shapiro-Wilk W = 0.963, p < 0.001 (n = 2186)
#>   life_satisfaction: KS = 0.203, p < 0.001; Shapiro-Wilk W = 0.884, p < 0.001 (n = 2421)
#> Use summary() for detailed output.

# Within comparison groups (SPSS: EXAMINE ... BY gender)
survey_data %>%
  group_by(gender) %>%
  normality_test(age, income)
#> Normality Tests: age, income
#>   2 group combination(s) x 2 variable(s) [Grouped: gender]
#> Use summary() for detailed output.

# --- Three-layer output ---
result <- normality_test(survey_data, age, income)
result              # compact overview
#> Normality Tests: age, income
#>   age: KS = 0.028, p < 0.001; Shapiro-Wilk W = 0.990, p < 0.001 (n = 2500)
#>   income: KS = 0.079, p < 0.001; Shapiro-Wilk W = 0.963, p < 0.001 (n = 2186)
#> Use summary() for detailed output.
summary(result)     # full SPSS-style table
#> 
#> Normality Tests Results
#> -----------------------
#> - Variables: age, income
#> 
#> Tests of Normality
#>   ------------------------------------------ 
#>   Variable     KS    df   KS p      W    W p 
#>   ------------------------------------------ 
#>   age       0.028  2500  <.001  0.990  <.001 
#>   income    0.079  2186  <.001  0.963  <.001 
#>   ------------------------------------------ 
#> 
#> KS = Kolmogorov-Smirnov statistic with Lilliefors significance correction.
#> W = Shapiro-Wilk statistic (computed for 3 <= n <= 5000, as in SPSS).
#> p < 0.05 indicates a significant deviation from normality.
```
