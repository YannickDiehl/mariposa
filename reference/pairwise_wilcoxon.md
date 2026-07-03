# Find Which Specific Measurements Differ After Friedman Test

`pairwise_wilcoxon()` tells you exactly which pairs of measurements
differ from each other after the Friedman test finds overall
differences. It performs all pairwise Wilcoxon signed-rank tests with
p-value correction.

Think of it as:

- Friedman says "there are differences somewhere among the measurements"

- Pairwise Wilcoxon says "specifically, Measurement A differs from
  Measurement C"

- A way to make all possible pairwise comparisons for repeated measures

## Usage

``` r
pairwise_wilcoxon(x, ...)

# Default S3 method
pairwise_wilcoxon(x, ...)
```

## Arguments

- x:

  Friedman test results from
  [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)

- ...:

  Additional arguments passed to methods. The method for `friedman_test`
  objects accepts `p_adjust` (character): method for adjusting p-values
  for multiple comparisons. Options: `"bonferroni"` (default, most
  conservative), `"holm"`, `"BH"`, `"hochberg"`, `"hommel"`, `"BY"`,
  `"fdr"`, `"none"`.

## Value

Pairwise comparison results showing:

- Which measurement pairs are significantly different

- Z-statistics from Wilcoxon signed-rank tests

- Adjusted p-values (controlling for multiple comparisons)

## Details

### Understanding the Results

**Z-Statistics**: Based on the Wilcoxon signed-rank test for each pair

- Large absolute Z values indicate big differences between two
  measurements

- Positive Z: Values in var1 tend to be higher than var2

- Negative Z: Values in var2 tend to be higher than var1

**Adjusted P-values**: Control for multiple comparisons

- p \< 0.05: Measurements are significantly different

- p \>= 0.05: No significant difference between these measurements

### The Wilcoxon Signed-Rank Test

For each pair of measurements, the Wilcoxon signed-rank test:

1.  Computes differences between the two measurements

2.  Ranks the absolute differences

3.  Computes a Z-statistic based on the rank sums

4.  Uses normal approximation with tie correction

### P-Value Adjustment Methods

- **Bonferroni** (default): Most conservative, multiplies p by number of
  comparisons

- **Holm**: Step-down method, less conservative than Bonferroni

- **BH**: Controls false discovery rate, good for many comparisons

### When to Use This

Use pairwise Wilcoxon when:

- Your Friedman test shows significant differences (p \< 0.05)

- You want to know which specific measurements differ

- Your data are ordinal or violate normality assumptions

- You have repeated measures or matched groups

### Relationship to Other Tests

- Non-parametric post-hoc for repeated measures (like Dunn is for
  independent groups)

- Follow-up to
  [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md),
  like
  [`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md)
  follows
  [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md)

### Weighted variants

When the parent
[`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)
result is weighted, each pairwise test uses the same frequency-weighted
signed-rank formulas. SPSS `NPAR TESTS` ignores `WEIGHT BY`, so weighted
results have no SPSS reference (R-only, guarded by an internal
invariance suite); see
[`vignette("spss-compatibility")`](https://YannickDiehl.github.io/mariposa/articles/spss-compatibility.md)
for validation status.

- Each pairwise comparison uses
  [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)
  logic internally

## References

Wilcoxon, F. (1945). Individual comparisons by ranking methods.
Biometrics Bulletin, 1(6), 80-83.

## See also

[`friedman_test`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)
for performing Friedman tests.

[`wilcoxon_test`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)
for individual paired Wilcoxon tests.

[`dunn_test`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md)
for post-hoc comparisons after Kruskal-Wallis.

Other posthoc:
[`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md),
[`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md),
[`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md),
[`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Perform Friedman followed by pairwise Wilcoxon post-hoc
friedman_result <- survey_data %>%
  friedman_test(trust_government, trust_media, trust_science)

# Pairwise Wilcoxon comparisons (default: Bonferroni)
friedman_result %>% pairwise_wilcoxon()
#> Pairwise Wilcoxon Post-Hoc Test (Bonferroni)
#>   3 comparisons, 3 significant (p < .05)
#> Use summary() for the full comparison table.

# With Holm correction (less conservative)
friedman_result %>% pairwise_wilcoxon(p_adjust = "holm")
#> Pairwise Wilcoxon Post-Hoc Test (Holm)
#>   3 comparisons, 3 significant (p < .05)
#> Use summary() for the full comparison table.

# With Benjamini-Hochberg (controls false discovery rate)
friedman_result %>% pairwise_wilcoxon(p_adjust = "BH")
#> Pairwise Wilcoxon Post-Hoc Test (Benjamini-Hochberg)
#>   3 comparisons, 3 significant (p < .05)
#> Use summary() for the full comparison table.

# With weights
fw_weighted <- survey_data %>%
  friedman_test(trust_government, trust_media, trust_science,
                weights = sampling_weight)

fw_weighted %>% pairwise_wilcoxon()
#> Pairwise Wilcoxon Post-Hoc Test (Bonferroni) [Weighted]
#>   3 comparisons, 3 significant (p < .05)
#> Use summary() for the full comparison table.

# Grouped analysis
fw_grouped <- survey_data %>%
  group_by(region) %>%
  friedman_test(trust_government, trust_media, trust_science)

fw_grouped %>% pairwise_wilcoxon()
#> Pairwise Wilcoxon Post-Hoc Test (Bonferroni)
#> [region = East]
#>   3 comparisons, 3 significant (p < .05)
#> [region = West]
#>   3 comparisons, 3 significant (p < .05)
#> Use summary() for the full comparison table.
```
