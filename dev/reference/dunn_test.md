# Find Which Specific Groups Differ After Kruskal-Wallis

`dunn_test()` tells you exactly which groups are different from each
other after Kruskal-Wallis finds overall differences. It's the
non-parametric equivalent of a Tukey or Scheffe post-hoc test.

Think of it as:

- Kruskal-Wallis says "there are differences somewhere"

- Dunn test says "specifically, Group A differs from Group C"

- A way to make all possible pairwise comparisons using rank-based
  statistics

## Usage

``` r
dunn_test(x, ...)

# Default S3 method
dunn_test(x, ...)
```

## Arguments

- x:

  Kruskal-Wallis results from
  [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/dev/reference/kruskal_wallis.md)

- ...:

  Additional arguments passed to methods. The method for
  `kruskal_wallis` objects accepts `p_adjust` (character): method for
  adjusting p-values for multiple comparisons. Options: `"bonferroni"`
  (default, most conservative), `"holm"`, `"BH"`, `"hochberg"`,
  `"hommel"`, `"BY"`, `"fdr"`, `"none"`.

## Value

Pairwise comparison results showing:

- Which group pairs are significantly different

- Z-statistics based on rank differences

- Adjusted p-values (controlling for multiple comparisons)

## Details

### Understanding the Results

**Z-Statistics**: Based on differences in mean ranks between groups

- Large absolute Z values indicate big rank differences

- Positive Z: First group has higher mean rank than second

- Negative Z: Second group has higher mean rank than first

**Adjusted P-values**: Control for multiple comparisons

- p \< 0.05: Groups are significantly different

- p \>= 0.05: No significant difference between these groups

### The Dunn Test Formula

For each pair of groups (i, j): \$\$Z\_{ij} = \frac{\bar{R}\_i -
\bar{R}\_j}{\sqrt{\frac{N(N+1)}{12} \left(\frac{1}{n_i} +
\frac{1}{n_j}\right)}}\$\$

where \\\bar{R}\_i\\ is the mean rank for group i, \\N\\ is the total
sample size, and \\n_i\\ is the size of group i.

### P-Value Adjustment Methods

- **Bonferroni** (default): Most conservative, multiplies p by number of
  comparisons

- **Holm**: Step-down method, less conservative than Bonferroni

- **BH**: Controls false discovery rate, good for many comparisons

### When to Use This

Use Dunn test when:

- Your Kruskal-Wallis test shows significant differences (p \< 0.05)

- You want to know which specific groups differ

- Your data violates normality assumptions

- You have ordinal data or skewed distributions

### Relationship to Other Tests

- Non-parametric equivalent of
  [`tukey_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/tukey_test.md)
  and
  [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/scheffe_test.md)

- Follow-up to
  [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/dev/reference/kruskal_wallis.md),
  just like Tukey follows ANOVA

- Uses ranks instead of raw values, making it robust to outliers

## References

Dunn, O. J. (1964). Multiple comparisons using rank sums. Technometrics,
6(3), 241-252.

## See also

[`kruskal_wallis`](https://YannickDiehl.github.io/mariposa/dev/reference/kruskal_wallis.md)
for performing Kruskal-Wallis tests.

[`tukey_test`](https://YannickDiehl.github.io/mariposa/dev/reference/tukey_test.md)
for parametric post-hoc comparisons after ANOVA.

[`scheffe_test`](https://YannickDiehl.github.io/mariposa/dev/reference/scheffe_test.md)
for conservative parametric post-hoc comparisons.

Other posthoc:
[`levene_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/levene_test.md),
[`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/dev/reference/pairwise_wilcoxon.md),
[`scheffe_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/scheffe_test.md),
[`tukey_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/tukey_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Perform Kruskal-Wallis followed by Dunn post-hoc test
kw_result <- survey_data %>%
  kruskal_wallis(life_satisfaction, group = education)

# Dunn post-hoc comparisons (default: Bonferroni)
kw_result %>% dunn_test()
#> Dunn Post-Hoc Test (Bonferroni) Results
#> ---------------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - P-value adjustment: Bonferroni
#> 
#> ---------------------------------------------------------------------------- 
#>                 Group 1                Group 2       Z p (unadj) p (adj) Sig 
#>         Basic Secondary Intermediate Secondary  -7.402     <.001   <.001 *** 
#>         Basic Secondary     Academic Secondary  -9.465     <.001   <.001 *** 
#>         Basic Secondary             University -11.159     <.001   <.001 *** 
#>  Intermediate Secondary     Academic Secondary  -1.973     0.048   0.291     
#>  Intermediate Secondary             University  -4.539     <.001   <.001 *** 
#>      Academic Secondary             University  -2.790     0.005   0.032   * 
#> ---------------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive Z: First group has higher mean rank
#> - Negative Z: First group has lower mean rank
#> - p-values are adjusted for multiple comparisons

# With Holm correction (less conservative)
kw_result %>% dunn_test(p_adjust = "holm")
#> Dunn Post-Hoc Test (Holm) Results
#> ---------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - P-value adjustment: Holm
#> 
#> ---------------------------------------------------------------------------- 
#>                 Group 1                Group 2       Z p (unadj) p (adj) Sig 
#>         Basic Secondary Intermediate Secondary  -7.402     <.001   <.001 *** 
#>         Basic Secondary     Academic Secondary  -9.465     <.001   <.001 *** 
#>         Basic Secondary             University -11.159     <.001   <.001 *** 
#>  Intermediate Secondary     Academic Secondary  -1.973     0.048   0.048   * 
#>  Intermediate Secondary             University  -4.539     <.001   <.001 *** 
#>      Academic Secondary             University  -2.790     0.005   0.011   * 
#> ---------------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive Z: First group has higher mean rank
#> - Negative Z: First group has lower mean rank
#> - p-values are adjusted for multiple comparisons

# With Benjamini-Hochberg (controls false discovery rate)
kw_result %>% dunn_test(p_adjust = "BH")
#> Dunn Post-Hoc Test (Benjamini-Hochberg) Results
#> -----------------------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - P-value adjustment: Benjamini-Hochberg
#> 
#> ---------------------------------------------------------------------------- 
#>                 Group 1                Group 2       Z p (unadj) p (adj) Sig 
#>         Basic Secondary Intermediate Secondary  -7.402     <.001   <.001 *** 
#>         Basic Secondary     Academic Secondary  -9.465     <.001   <.001 *** 
#>         Basic Secondary             University -11.159     <.001   <.001 *** 
#>  Intermediate Secondary     Academic Secondary  -1.973     0.048   0.048   * 
#>  Intermediate Secondary             University  -4.539     <.001   <.001 *** 
#>      Academic Secondary             University  -2.790     0.005   0.006  ** 
#> ---------------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive Z: First group has higher mean rank
#> - Negative Z: First group has lower mean rank
#> - p-values are adjusted for multiple comparisons

# With weights
kw_weighted <- survey_data %>%
  kruskal_wallis(life_satisfaction, group = education,
                 weights = sampling_weight)

kw_weighted %>% dunn_test()
#> Weighted Dunn Post-Hoc Test (Bonferroni) Results
#> ------------------------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - Weights variable: sampling_weight
#> - P-value adjustment: Bonferroni
#> 
#> ---------------------------------------------------------------------------- 
#>                 Group 1                Group 2       Z p (unadj) p (adj) Sig 
#>         Basic Secondary Intermediate Secondary  -7.351     <.001   <.001 *** 
#>         Basic Secondary     Academic Secondary  -9.444     <.001   <.001 *** 
#>         Basic Secondary             University -10.927     <.001   <.001 *** 
#>  Intermediate Secondary     Academic Secondary  -2.007     0.045   0.269     
#>  Intermediate Secondary             University  -4.484     <.001   <.001 *** 
#>      Academic Secondary             University  -2.735     0.006   0.037   * 
#> ---------------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive Z: First group has higher mean rank
#> - Negative Z: First group has lower mean rank
#> - p-values are adjusted for multiple comparisons

# Multiple variables
kw_multi <- survey_data %>%
  kruskal_wallis(life_satisfaction, trust_government,
                 group = education)

kw_multi %>% dunn_test()
#> Dunn Post-Hoc Test (Bonferroni) Results
#> ---------------------------------------
#> 
#> - Grouping variable: education
#> - P-value adjustment: Bonferroni
#> 
#> 
#> --- life_satisfaction ---
#> 
#> ---------------------------------------------------------------------------- 
#>                 Group 1                Group 2       Z p (unadj) p (adj) Sig 
#>         Basic Secondary Intermediate Secondary  -7.402     <.001   <.001 *** 
#>         Basic Secondary     Academic Secondary  -9.465     <.001   <.001 *** 
#>         Basic Secondary             University -11.159     <.001   <.001 *** 
#>  Intermediate Secondary     Academic Secondary  -1.973     0.048   0.291     
#>  Intermediate Secondary             University  -4.539     <.001   <.001 *** 
#>      Academic Secondary             University  -2.790     0.005   0.032   * 
#> ---------------------------------------------------------------------------- 
#> 
#> 
#> --- trust_government ---
#> 
#> --------------------------------------------------------------------------- 
#>                 Group 1                Group 2      Z p (unadj) p (adj) Sig 
#>         Basic Secondary Intermediate Secondary  0.955     0.340   1.000     
#>         Basic Secondary     Academic Secondary  0.552     0.581   1.000     
#>         Basic Secondary             University -0.037     0.971   1.000     
#>  Intermediate Secondary     Academic Secondary -0.378     0.706   1.000     
#>  Intermediate Secondary             University -0.821     0.411   1.000     
#>      Academic Secondary             University -0.490     0.624   1.000     
#> --------------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive Z: First group has higher mean rank
#> - Negative Z: First group has lower mean rank
#> - p-values are adjusted for multiple comparisons

# Grouped analysis
kw_grouped <- survey_data %>%
  group_by(region) %>%
  kruskal_wallis(life_satisfaction, group = education)

kw_grouped %>% dunn_test()
#> Dunn Post-Hoc Test (Bonferroni) Results
#> ---------------------------------------
#> 
#> - Grouping variable: education
#> - P-value adjustment: Bonferroni
#> 
#> 
#> Group: region = East
#> --------------------
#> 
#> --- life_satisfaction ---
#> 
#> --------------------------------------------------------------------------- 
#>                 Group 1                Group 2      Z p (unadj) p (adj) Sig 
#>         Basic Secondary Intermediate Secondary -1.886     0.059   0.356     
#>         Basic Secondary     Academic Secondary -3.158     0.002   0.010  ** 
#>         Basic Secondary             University -3.429     <.001   0.004  ** 
#>  Intermediate Secondary     Academic Secondary -1.230     0.219   1.000     
#>  Intermediate Secondary             University -1.705     0.088   0.529     
#>      Academic Secondary             University -0.592     0.554   1.000     
#> --------------------------------------------------------------------------- 
#> 
#> 
#> Group: region = West
#> --------------------
#> 
#> --- life_satisfaction ---
#> 
#> ---------------------------------------------------------------------------- 
#>                 Group 1                Group 2       Z p (unadj) p (adj) Sig 
#>         Basic Secondary Intermediate Secondary  -7.345     <.001   <.001 *** 
#>         Basic Secondary     Academic Secondary  -9.023     <.001   <.001 *** 
#>         Basic Secondary             University -10.774     <.001   <.001 *** 
#>  Intermediate Secondary     Academic Secondary  -1.587     0.113   0.675     
#>  Intermediate Secondary             University  -4.226     <.001   <.001 *** 
#>      Academic Secondary             University  -2.830     0.005   0.028   * 
#> ---------------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive Z: First group has higher mean rank
#> - Negative Z: First group has lower mean rank
#> - p-values are adjusted for multiple comparisons
```
