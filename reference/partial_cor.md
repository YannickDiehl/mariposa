# Partial Correlation

`partial_cor()` computes partial correlations between variables while
controlling for one or more other variables — the SPSS `PARTIAL CORR`
procedure (Stata: `pcorr`). It answers the question "how strongly are x
and y related once the influence of the control variables is removed?"

## Usage

``` r
partial_cor(data, ..., controls, weights = NULL)
```

## Arguments

- data:

  Your survey data (a data frame or tibble). If grouped (via
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)),
  separate partial correlations are computed per group (SPSS SPLIT
  FILE).

- ...:

  Variables to correlate (unquoted, supports tidyselect). At least two.

- controls:

  Control variable(s) to partial out (unquoted, supports tidyselect;
  SPSS `BY` list). At least one, must not overlap with the analysis
  variables.

- weights:

  Optional survey weights (unquoted variable name), treated as frequency
  weights matching SPSS `WEIGHT BY`.

## Value

An object of class `"partial_cor"` whose `$correlations` tibble holds
one row per variable pair (and group combination) with:

- partial_r:

  Partial correlation controlling for the `controls`

- zero_order_r:

  The ordinary (zero-order) Pearson correlation of the pair, for
  comparison — SPSS `/STATISTICS=CORR`

- df:

  Degrees of freedom, n - 2 - k with k control variables (non-integer
  for weighted data, Charter §5.1)

- t_stat, p_value:

  t-test of the partial correlation (two-tailed, the SPSS default)

- n:

  Listwise-complete sample size (rounded weighted N when weighted)

For three or more analysis variables, `$matrices` additionally holds the
full partial-correlation matrix per group.

## Details

### Understanding the Output

Comparing `partial_r` against `zero_order_r` tells you what the controls
contribute:

- **Partial clearly smaller than zero-order**: much of the original
  association runs through the control variables (confounding or
  mediation).

- **Partial similar to zero-order**: the association is largely
  independent of the controls.

- **Partial larger than zero-order**: a suppressor situation — the
  controls masked part of the association.

### When to Use This

- Check whether a bivariate correlation survives controlling for
  demographics (age, education, ...)

- Separate the direct association of two attitudes from what a shared
  cause explains

For full multivariate control with several predictors, use
[`linear_regression`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
instead.

### Technical Details

Cases are deleted **listwise** across the analysis and control variables
(SPSS `/MISSING=LISTWISE`, the PARTIAL CORR default). The partial
correlation is computed from the Pearson correlation matrix of all
variables via the inverse of the control-variable block; the same
Pearson formula as
[`pearson_cor`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)
is used throughout, so weighted results follow the SPSS frequency-weight
convention with unrounded `sum(w)` in df and test statistics.
Significance is two-tailed (the SPSS default).

An SPSS v29 PARTIAL CORR reference run is pending; until it lands the
statistics are verified against the independent residual-of-regressions
characterization (see the SPSS compatibility vignette).

## See also

[`pearson_cor`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)
for zero-order correlations.

[`linear_regression`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
for multivariate control.

[`summary.partial_cor`](https://YannickDiehl.github.io/mariposa/reference/summary.partial_cor.md)
for detailed output.

Other correlation:
[`kendall_tau()`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md),
[`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md),
[`spearman_rho()`](https://YannickDiehl.github.io/mariposa/reference/spearman_rho.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# Does the satisfaction-income correlation survive controlling for age?
partial_cor(survey_data, life_satisfaction, income, controls = age)
#> Partial Correlation: life_satisfaction, income | controlling for age
#>   life_satisfaction x income: partial r = 0.448, p < 0.001 *** (zero-order r = 0.448), N = 2115
#> Use summary() for detailed output.

# Several variables, several controls
partial_cor(survey_data, trust_government, trust_media, trust_science,
            controls = c(age, political_orientation))
#> Partial Correlation: trust_government, trust_media, trust_science | controlling for age, political_orientation
#>   trust_government x trust_media: partial r = 0.019, p = 0.390  (zero-order r = 0.019), N = 1964
#>   trust_government x trust_science: partial r = 0.033, p = 0.148  (zero-order r = 0.030), N = 1964
#>   trust_media x trust_science: partial r = 0.009, p = 0.684  (zero-order r = 0.010), N = 1964
#> Use summary() for detailed output.

# Weighted (SPSS WEIGHT BY)
partial_cor(survey_data, life_satisfaction, income,
            controls = age, weights = sampling_weight)
#> Partial Correlation: life_satisfaction, income | controlling for age [Weighted]
#>   life_satisfaction x income: partial r = 0.450, p < 0.001 *** (zero-order r = 0.450), N = 2130
#> Use summary() for detailed output.

# Grouped (SPSS SPLIT FILE)
survey_data %>%
  group_by(gender) %>%
  partial_cor(life_satisfaction, income, controls = age)
#> Partial Correlation: life_satisfaction, income | controlling for age
#> [gender = Male]
#>   life_satisfaction x income: partial r = 0.455, p < 0.001 *** (zero-order r = 0.455), N = 1005
#> [gender = Female]
#>   life_satisfaction x income: partial r = 0.441, p < 0.001 *** (zero-order r = 0.442), N = 1110
#> Use summary() for detailed output.

# --- Three-layer output ---
result <- partial_cor(survey_data, life_satisfaction, income, controls = age)
result              # compact overview
#> Partial Correlation: life_satisfaction, income | controlling for age
#>   life_satisfaction x income: partial r = 0.448, p < 0.001 *** (zero-order r = 0.448), N = 2115
#> Use summary() for detailed output.
summary(result)     # full detailed output
#> 
#> Partial Correlation Results
#> ---------------------------
#> - Variables: life_satisfaction, income
#> - Controlling for: age
#> - Missing: Listwise deletion
#> 
#> Pairwise Results:
#>   -------------------------------------------------------------------------------------- 
#>   Variable 1         Variable 2  Partial r  Zero-order r    df       t      p     n  sig 
#>   -------------------------------------------------------------------------------------- 
#>   life_satisfaction      income      0.448         0.448  2112  23.037  <.001  2115  *** 
#>   -------------------------------------------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
