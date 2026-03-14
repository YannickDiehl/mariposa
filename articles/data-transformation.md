# Data Transformation

``` r
library(mariposa)
library(dplyr)
data(survey_data)
```

## Overview

Before analyzing survey data, you often need to transform variables —
recode responses, create dummy variables, standardize scales, or compute
row-wise indices. mariposa provides a set of functions that handle these
tasks with a clean, survey-oriented syntax.

| Function                                                                        | Purpose                                                 |
|---------------------------------------------------------------------------------|---------------------------------------------------------|
| [`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md)             | Recode values using a string syntax                     |
| [`to_dummy()`](https://YannickDiehl.github.io/mariposa/reference/to_dummy.md)   | Create dummy (0/1) variables from categorical columns   |
| [`std()`](https://YannickDiehl.github.io/mariposa/reference/std.md)             | Z-standardize variables (4 methods)                     |
| [`center()`](https://YannickDiehl.github.io/mariposa/reference/center.md)       | Mean-center variables (grand-mean or group-mean)        |
| [`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md) | Compute row-wise means across items                     |
| [`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md)   | Compute row-wise sums across items                      |
| [`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md) | Count specific values per row                           |
| [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md)         | Transform to Percent of Maximum Possible Scores (0–100) |

## Recoding with rec()

[`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md) uses
a concise string syntax to recode variables. It works inside
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) and
supports ranges, reverse-coding, and inline value labels.

### Simple Recoding

Collapse categories by mapping old values to new values:

``` r
survey_data <- rec(survey_data, age,
  rules = "18:29=1 [Young]; 30:49=2 [Middle]; 50:99=3 [Older]",
  suffix = "_group", as.factor = TRUE
)

frequency(survey_data, age_group)
#> 
#> Frequency Analysis Results
#> --------------------------
#> 
#> age_group (Age in years (recoded))
#> # total N=2500 valid N=2500 mean=NA sd=NA skewness=NA
#> 
#> +--------+--------+--------+--------+--------+--------+
#> |  Value |  Label |      N |  Raw % |Valid % | Cum. % |
#> +--------+--------+--------+--------+--------+--------+
#> |  Young |  Young |    292 |  11.68 |  11.68 |  11.68 |
#> | Middle | Middle |    935 |  37.40 |  37.40 |  49.08 |
#> |  Older |  Older |   1273 |  50.92 |  50.92 | 100.00 |
#> +--------+--------+--------+--------+--------+--------+
#> |  Total |        |   2500 | 100.00 | 100.00 |        |
#> +--------+--------+--------+--------+--------+--------+
```

The bracket notation (`[Young]`) automatically creates value labels on
the recoded variable. The `suffix` argument creates a new column (here
`age_group`) instead of overwriting the original.

### Reverse-Coding

Reverse the direction of a Likert scale. Useful when some items in a
scale are negatively worded:

``` r
survey_data <- rec(survey_data, trust_government,
  rules = "rev", suffix = "_rev"
)

# Original: 1=low trust ... 5=high trust
# Reversed: 1=high trust ... 5=low trust
head(data.frame(
  original = survey_data$trust_government,
  reversed = survey_data$trust_government_rev
))
#>   original reversed
#> 1        3        3
#> 2        4        2
#> 3        1        5
#> 4        1        5
#> 5        2        4
#> 6        1        5
```

### Dichotomizing

Split a variable at its median into two groups:

``` r
survey_data <- rec(survey_data, income,
  rules = "dicho", suffix = "_dicho"
)

frequency(survey_data, income_dicho)
#> 
#> Frequency Analysis Results
#> --------------------------
#> 
#> income_dicho (Monthly household income (EUR) (recoded))
#> # total N=2500 valid N=2186 mean=0.49 sd=0.50 skewness=0.03
#> 
#> +------+--------+--------+--------+--------+
#> |Value |      N |  Raw % |Valid % | Cum. % |
#> +------+--------+--------+--------+--------+
#> |    0 |   1108 |  44.32 |  50.69 |  50.69 |
#> |    1 |   1078 |  43.12 |  49.31 | 100.00 |
#> +------+--------+--------+--------+--------+
#> |Total |   2186 |  87.44 | 100.00 |        |
#> +------+--------+--------+--------+--------+
#> |   NA |    314 |  12.56 |     NA |     NA |
#> +------+--------+--------+--------+--------+
#> |Total |    314 |  12.56 |     NA |        |
#> +------+--------+--------+--------+--------+
```

Other split options:

``` r
# Split at the mean
survey_data <- rec(survey_data, income,
  rules = "mean", suffix = "_mean_split"
)

# Split at a fixed cut-point
survey_data <- rec(survey_data, income,
  rules = "dicho(3000)", suffix = "_custom"
)
```

### Keeping and Copying Values

Use `copy` to keep values that are not explicitly recoded, and `else`
for a catch-all:

``` r
survey_data <- rec(survey_data, education,
  rules = "1:2=1 [Lower]; else=2 [Higher]",
  suffix = "_binary", as.factor = TRUE
)

frequency(survey_data, education_binary)
#> 
#> Frequency Analysis Results
#> --------------------------
#> 
#> education_binary (Highest educational attainment (recoded))
#> # total N=2500 valid N=2500 mean=NA sd=NA skewness=NA
#> 
#> +--------+--------+--------+--------+--------+--------+
#> |  Value |  Label |      N |  Raw % |Valid % | Cum. % |
#> +--------+--------+--------+--------+--------+--------+
#> |  Lower |  Lower |   1470 |  58.80 |  58.80 |  58.80 |
#> | Higher | Higher |   1030 |  41.20 |  41.20 | 100.00 |
#> +--------+--------+--------+--------+--------+--------+
#> |  Total |        |   2500 | 100.00 | 100.00 |        |
#> +--------+--------+--------+--------+--------+--------+
```

## Dummy Coding with to_dummy()

[`to_dummy()`](https://YannickDiehl.github.io/mariposa/reference/to_dummy.md)
creates binary (0/1) indicator variables from categorical columns — also
known as one-hot encoding.

### Basic Usage

``` r
# Create dummy variables for region
dummies <- to_dummy(survey_data, region, append = FALSE)
head(dummies)
#> # A tibble: 6 × 2
#>   region_East region_West
#>         <int>       <int>
#> 1           1           0
#> 2           0           1
#> 3           0           1
#> 4           0           1
#> 5           0           1
#> 6           1           0
```

### Label-Based Column Names

Use `suffix = "label"` to name columns after the value labels instead of
numeric codes:

``` r
dummies <- to_dummy(survey_data, gender, suffix = "label", append = FALSE)
head(dummies)
#> # A tibble: 6 × 2
#>   gender_Male gender_Female
#>         <int>         <int>
#> 1           0             1
#> 2           1             0
#> 3           1             0
#> 4           0             1
#> 5           1             0
#> 6           0             1
```

### Reference Category (n-1 Coding)

For regression analysis, you typically need n-1 dummies (one category
omitted as reference):

``` r
dummies <- to_dummy(survey_data, education, ref = 1, append = FALSE)
head(dummies)
#> # A tibble: 6 × 4
#>   `education_Basic Secondary` education_Intermediate Se…¹ education_Academic S…²
#>                         <int>                       <int>                  <int>
#> 1                           0                           1                      0
#> 2                           0                           0                      1
#> 3                           0                           0                      1
#> 4                           1                           0                      0
#> 5                           1                           0                      0
#> 6                           0                           1                      0
#> # ℹ abbreviated names: ¹​`education_Intermediate Secondary`,
#> #   ²​`education_Academic Secondary`
#> # ℹ 1 more variable: education_University <int>
```

### Adding to Existing Data

By default, dummy columns are appended to the original data:

``` r
survey_data <- to_dummy(survey_data, gender, suffix = "label")
# Adds gender_Male, gender_Female to the data frame
```

## Standardization with std()

[`std()`](https://YannickDiehl.github.io/mariposa/reference/std.md)
z-standardizes variables so they have mean 0 and standard deviation 1.
This is useful for comparing variables on different scales.

### Basic Standardization

``` r
survey_data <- survey_data %>%
  std(age, income)

# Check: mean ≈ 0, sd ≈ 1
survey_data %>%
  describe(age, income, show = c("mean", "sd"))
#> 
#> Descriptive Statistics
#> ----------------------
#>  Variable Mean SD    N Missing
#>       age    0  1 2500       0
#>    income    0  1 2186     314
#> ----------------------------------------
```

### Standardization Methods

[`std()`](https://YannickDiehl.github.io/mariposa/reference/std.md)
supports four methods:

``` r
# Default: divide by SD
survey_data_methods <- survey_data %>%
  std(life_satisfaction, method = "sd", suffix = "_sd") %>%
  std(life_satisfaction, method = "2sd", suffix = "_2sd") %>%
  std(life_satisfaction, method = "mad", suffix = "_mad")

survey_data_methods %>%
  describe(life_satisfaction_sd, life_satisfaction_2sd, life_satisfaction_mad,
           show = c("mean", "sd"))
#> 
#> Descriptive Statistics
#> ----------------------
#>               Variable   Mean    SD    N Missing
#>   life_satisfaction_sd  0.000 1.000 2421      79
#>  life_satisfaction_2sd  0.000 0.500 2421      79
#>  life_satisfaction_mad -0.251 0.778 2421      79
#> ----------------------------------------
```

- **`"sd"`** (default): Classic z-standardization
  ($\frac{x - \bar{x}}{SD}$)
- **`"2sd"`**: Gelman’s (2008) recommendation — divides by 2 SD, making
  coefficients comparable to untransformed binary predictors
- **`"mad"`**: Robust standardization using median and MAD (resistant to
  outliers)
- **`"gmd"`**: Standardization using the Gini Mean Difference

### Weighted Standardization

``` r
survey_data <- survey_data %>%
  std(income, weights = sampling_weight, suffix = "_wstd")

survey_data %>%
  describe(income_wstd, show = c("mean", "sd"))
#> 
#> Descriptive Statistics
#> ----------------------
#>     Variable  Mean    SD    N Missing
#>  income_wstd 0.008 1.006 2186     314
#> ----------------------------------------
```

### Group-Wise Standardization

Standardize within subgroups:

``` r
survey_data <- survey_data %>%
  group_by(region) %>%
  std(income, suffix = "_gstd") %>%
  ungroup()
```

## Centering with center()

[`center()`](https://YannickDiehl.github.io/mariposa/reference/center.md)
subtracts the mean from each value, shifting the distribution so the
mean is zero while preserving the original scale.

### Grand-Mean Centering

``` r
survey_data <- survey_data %>%
  center(age, income, suffix = "_c")

survey_data %>%
  describe(age_c, income_c, show = c("mean", "sd", "min", "max"))
#> 
#> Descriptive Statistics
#> ----------------------
#>  Variable Mean SD    N Missing
#>     age_c    0  1 2500       0
#>  income_c    0  1 2186     314
#> ----------------------------------------
```

### Group-Mean Centering

Center within groups — each observation is expressed as a deviation from
its group mean:

``` r
survey_data <- survey_data %>%
  group_by(region) %>%
  center(income, suffix = "_gc") %>%
  ungroup()

# Group means are now zero within each region
survey_data %>%
  group_by(region) %>%
  describe(income_gc, show = c("mean", "sd"))
#> 
#> Descriptive Statistics
#> ----------------------
#> 
#> Group: region = East
#> --------------------
#> ----------------------------------------
#>   Variable Mean    SD   N Missing
#>  income_gc    0 0.968 429      56
#> ----------------------------------------
#> 
#> Group: region = West
#> --------------------
#> ----------------------------------------
#>   Variable Mean    SD    N Missing
#>  income_gc    0 1.008 1757     258
#> ----------------------------------------
```

### Weighted Centering

``` r
survey_data <- survey_data %>%
  center(age, weights = sampling_weight, suffix = "_wc")
```

## Row Operations

Row operations compute values *across columns* for each respondent —
essential for creating scale scores from multiple survey items.

### Row Means

[`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md)
computes the arithmetic mean across selected variables for each row:

``` r
survey_data <- survey_data %>%
  mutate(m_trust = row_means(., trust_government, trust_media, trust_science))

survey_data %>%
  describe(m_trust)
#> 
#> Descriptive Statistics
#> ----------------------
#>  Variable  Mean Median  SD Range IQR Skewness    N Missing
#>   m_trust 2.915      3 0.7     4   1    0.015 2500       0
#> ----------------------------------------
```

#### Using tidyselect

``` r
survey_data <- survey_data %>%
  mutate(m_trust2 = row_means(., starts_with("trust")))
```

#### Using pick()

The [`pick()`](https://dplyr.tidyverse.org/reference/pick.html) function
works with both `%>%` and `|>`:

``` r
survey_data <- survey_data %>%
  mutate(m_trust3 = row_means(
    pick(trust_government, trust_media, trust_science)
  ))
```

#### Minimum Valid Items

Require a minimum number of non-missing items per row. This matches SPSS
`MEAN.2()` syntax:

``` r
survey_data <- survey_data %>%
  mutate(m_trust_strict = row_means(
    ., trust_government, trust_media, trust_science,
    min_valid = 2
  ))
```

If a respondent answered fewer than 2 of the 3 items, they receive `NA`
instead of a potentially unreliable score.

### Row Sums

[`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md)
works like
[`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md)
but returns the total:

``` r
survey_data <- survey_data %>%
  mutate(trust_total = row_sums(., trust_government, trust_media, trust_science))

survey_data %>%
  describe(trust_total)
#> 
#> Descriptive Statistics
#> ----------------------
#>     Variable  Mean Median   SD Range IQR Skewness    N Missing
#>  trust_total 8.282      8 2.17    13   3   -0.164 2500       0
#> ----------------------------------------
```

### Row Count

[`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md)
counts how many times a specific value appears in each row:

``` r
# How many trust items did each person rate as 5 (highest)?
survey_data <- survey_data %>%
  mutate(n_high_trust = row_count(
    ., trust_government, trust_media, trust_science,
    count = 5
  ))

frequency(survey_data, n_high_trust)
#> 
#> Frequency Analysis Results
#> --------------------------
#> 
#> n_high_trust
#> # total N=2500 valid N=2500 mean=0.29 sd=0.49 skewness=1.38
#> 
#> +------+--------+--------+--------+--------+
#> |Value |      N |  Raw % |Valid % | Cum. % |
#> +------+--------+--------+--------+--------+
#> |    0 |   1829 |  73.16 |  73.16 |  73.16 |
#> |    1 |    628 |  25.12 |  25.12 |  98.28 |
#> |    2 |     43 |   1.72 |   1.72 | 100.00 |
#> +------+--------+--------+--------+--------+
#> |Total |   2500 | 100.00 | 100.00 |        |
#> +------+--------+--------+--------+--------+
```

## POMPS Transformation

[`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md)
transforms scores to a Percent of Maximum Possible Scores scale (0–100).
This makes scores from different scales directly comparable:

``` r
survey_data <- survey_data %>%
  mutate(trust_pomps = pomps(m_trust, scale_min = 1, scale_max = 5))

survey_data %>%
  describe(trust_pomps)
#> 
#> Descriptive Statistics
#> ----------------------
#>     Variable   Mean Median   SD Range IQR Skewness    N Missing
#>  trust_pomps 47.885     50 17.5   100  25    0.015 2500       0
#> ----------------------------------------
```

A score of 0 means the respondent chose the minimum on every item; 100
means the maximum on every item.

Always specify `scale_min` and `scale_max` based on the theoretical
scale range, not the observed range. This ensures scores are comparable
across samples.

``` r
# Transform multiple variables at once
survey_data <- survey_data %>%
  mutate(across(
    c(trust_government, trust_media, trust_science),
    ~ pomps(.x, scale_min = 1, scale_max = 5),
    .names = "{.col}_pomps"
  ))
```

## Complete Example

A typical data transformation workflow before analysis:

``` r
data(survey_data)  # fresh copy

# 1. Recode: create age groups and reverse-code an item
survey_data <- rec(survey_data, age,
  rules = "18:29=1 [Young]; 30:49=2 [Middle]; 50:99=3 [Older]",
  suffix = "_group", as.factor = TRUE)
survey_data <- rec(survey_data, trust_government,
  rules = "rev", suffix = "_rev")

# 2. Create scale score
survey_data <- survey_data %>%
  mutate(m_trust = row_means(., trust_government, trust_media, trust_science,
                             min_valid = 2))

# 3. Standardize for regression
survey_data <- survey_data %>%
  std(age, income, suffix = "_z")

# 4. Use in analysis
survey_data %>%
  t_test(m_trust, group = gender, weights = sampling_weight)
#> t-Test: m_trust by gender [Weighted]
#>   t(2457.2) = -2.362, p = 0.018 *, g = -0.095 (negligible), N = 2499

survey_data %>%
  linear_regression(life_satisfaction ~ age_z + income_z + m_trust,
                    weights = sampling_weight)
#> Linear Regression: life_satisfaction ~ age_z + income_z + m_trust [Weighted]
#>   R2 = 0.201, adj.R2 = 0.200, F(3, 2109) = 177.00, p < 0.001 ***, N = 2113
```

## Practical Tips

1.  **Use
    [`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md)
    for survey-specific recoding.** The string syntax is more readable
    than nested [`ifelse()`](https://rdrr.io/r/base/ifelse.html) or
    [`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
    for typical survey transformations.

2.  **Always specify `min_valid` for
    [`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md).**
    Without it, a respondent who answered only 1 out of 10 items gets a
    scale score based on a single response. Setting `min_valid` to half
    the number of items is a common rule of thumb.

3.  **Center variables before regression.** Centering makes the
    intercept interpretable as the expected value at the mean of all
    predictors. Group-mean centering separates within-group and
    between-group effects.

4.  **Use `std(method = "2sd")` for mixed models.** Gelman (2008)
    recommends dividing by 2 SD so that standardized coefficients for
    continuous predictors are comparable to those for binary predictors.

5.  **Always use theoretical min/max in
    [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md).**
    Using observed min/max makes scores sample-dependent and
    non-comparable.

## Summary

1.  [`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md)
    recodes variables with a concise string syntax (ranges, reverse,
    dichotomize, inline labels)
2.  [`to_dummy()`](https://YannickDiehl.github.io/mariposa/reference/to_dummy.md)
    creates binary indicator variables for regression
3.  [`std()`](https://YannickDiehl.github.io/mariposa/reference/std.md)
    z-standardizes with four methods (sd, 2sd, mad, gmd) and
    weight/group support
4.  [`center()`](https://YannickDiehl.github.io/mariposa/reference/center.md)
    mean-centers variables (grand-mean or group-mean)
5.  [`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md),
    [`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md),
    [`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md)
    compute values across columns per row
6.  [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md)
    transforms scores to a comparable 0–100 scale

## Next Steps

- Explore your transformed data — see
  [`vignette("descriptive-statistics")`](https://YannickDiehl.github.io/mariposa/articles/descriptive-statistics.md)
- Build and validate scales — see
  [`vignette("scale-analysis")`](https://YannickDiehl.github.io/mariposa/articles/scale-analysis.md)
- Test group differences — see
  [`vignette("hypothesis-testing")`](https://YannickDiehl.github.io/mariposa/articles/hypothesis-testing.md)
- Predict outcomes with regression — see
  [`vignette("regression-analysis")`](https://YannickDiehl.github.io/mariposa/articles/regression-analysis.md)
