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
  [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md)

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
  [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)
  and
  [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md)

- Follow-up to
  [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md),
  just like Tukey follows ANOVA

- Uses ranks instead of raw values, making it robust to outliers

## References

Dunn, O. J. (1964). Multiple comparisons using rank sums. Technometrics,
6(3), 241-252.

## See also

[`kruskal_wallis`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md)
for performing Kruskal-Wallis tests.

[`tukey_test`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)
for parametric post-hoc comparisons after ANOVA.

[`scheffe_test`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md)
for conservative parametric post-hoc comparisons.

Other posthoc:
[`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md),
[`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md),
[`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md),
[`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)

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
#>         Basic Secondary Intermediate Secondary  -7.658     <.001   <.001 *** 
#>         Basic Secondary     Academic Secondary  -9.792     <.001   <.001 *** 
#>         Basic Secondary             University -11.545     <.001   <.001 *** 
#>  Intermediate Secondary     Academic Secondary  -2.042     0.041   0.247     
#>  Intermediate Secondary             University  -4.696     <.001   <.001 *** 
#>      Academic Secondary             University  -2.886     0.004   0.023   * 
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
#>         Basic Secondary Intermediate Secondary  -7.658     <.001   <.001 *** 
#>         Basic Secondary     Academic Secondary  -9.792     <.001   <.001 *** 
#>         Basic Secondary             University -11.545     <.001   <.001 *** 
#>  Intermediate Secondary     Academic Secondary  -2.042     0.041   0.041   * 
#>  Intermediate Secondary             University  -4.696     <.001   <.001 *** 
#>      Academic Secondary             University  -2.886     0.004   0.008  ** 
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
#>         Basic Secondary Intermediate Secondary  -7.658     <.001   <.001 *** 
#>         Basic Secondary     Academic Secondary  -9.792     <.001   <.001 *** 
#>         Basic Secondary             University -11.545     <.001   <.001 *** 
#>  Intermediate Secondary     Academic Secondary  -2.042     0.041   0.041   * 
#>  Intermediate Secondary             University  -4.696     <.001   <.001 *** 
#>      Academic Secondary             University  -2.886     0.004   0.005  ** 
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
#>         Basic Secondary Intermediate Secondary  -7.658     <.001   <.001 *** 
#>         Basic Secondary     Academic Secondary  -9.792     <.001   <.001 *** 
#>         Basic Secondary             University -11.545     <.001   <.001 *** 
#>  Intermediate Secondary     Academic Secondary  -2.042     0.041   0.247     
#>  Intermediate Secondary             University  -4.696     <.001   <.001 *** 
#>      Academic Secondary             University  -2.886     0.004   0.023   * 
#> ---------------------------------------------------------------------------- 
#> 
#> 
#> --- trust_government ---
#> 
#> --------------------------------------------------------------------------- 
#>                 Group 1                Group 2      Z p (unadj) p (adj) Sig 
#>         Basic Secondary Intermediate Secondary  0.983     0.325   1.000     
#>         Basic Secondary     Academic Secondary  0.569     0.570   1.000     
#>         Basic Secondary             University -0.038     0.970   1.000     
#>  Intermediate Secondary     Academic Secondary -0.389     0.697   1.000     
#>  Intermediate Secondary             University -0.846     0.398   1.000     
#>      Academic Secondary             University -0.504     0.614   1.000     
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
#>         Basic Secondary Intermediate Secondary -1.949     0.051   0.308     
#>         Basic Secondary     Academic Secondary -3.263     0.001   0.007  ** 
#>         Basic Secondary             University -3.543     <.001   0.002  ** 
#>  Intermediate Secondary     Academic Secondary -1.271     0.204   1.000     
#>  Intermediate Secondary             University -1.762     0.078   0.469     
#>      Academic Secondary             University -0.612     0.540   1.000     
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
#>         Basic Secondary Intermediate Secondary  -7.601     <.001   <.001 *** 
#>         Basic Secondary     Academic Secondary  -9.338     <.001   <.001 *** 
#>         Basic Secondary             University -11.151     <.001   <.001 *** 
#>  Intermediate Secondary     Academic Secondary  -1.642     0.101   0.603     
#>  Intermediate Secondary             University  -4.373     <.001   <.001 *** 
#>      Academic Secondary             University  -2.929     0.003   0.020   * 
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
