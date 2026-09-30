# Convert Analysis Results to Data Frames

Turns the result of any mariposa analysis into a plain data frame that
can be saved with
[`utils::write.csv()`](https://rdrr.io/r/utils/write.table.html),
combined with
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
or used for plotting.
[`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
works the same way (it calls
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)),
[`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)
writes the table (plus the secondary tables SPSS shows next to it) to
Excel, and with the broom package loaded,
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
returns the table with broom's column names.

## Usage

``` r
# S3 method for class 'ancova'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'binomial_test'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'chi_square'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'chisq_gof'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'codebook'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'crosstab'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'describe'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'dunn_test'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'efa'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'factorial_anova'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'fisher_test'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'frequency'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'friedman_test'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'kendall_tau'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'kruskal_wallis'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'levene_test'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'linear_regression'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'logistic_regression'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'mann_whitney'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'marginal_effects'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'mcnemar_test'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'multiple_response'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'normality_test'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'oneway_anova'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'pairwise_wilcoxon'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'partial_cor'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'pearson_cor'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'reliability'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'scheffe_test'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'spearman_rho'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 't_test'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'tukey_test'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'w_iqr'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'w_kurtosis'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'w_mean'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'w_median'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'w_modus'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'w_quantile'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'w_range'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'w_sd'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'w_se'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'w_skew'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'w_var'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'wilcoxon_test'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A result of one of mariposa's analysis functions, e.g.
  [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md),
  [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md),
  [`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)
  or
  [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md).

- row.names, optional:

  Ignored; present for compatibility with
  [`base::as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- ...:

  Ignored.

## Value

A plain `data.frame` (no list-columns, no row names) holding the main
result table:

- one row per variable (per test) for tests and descriptive statistics,
  with the columns of `x$results`; group-level details stored as
  list-columns are flattened into columns (for example `group1`,
  `group2`, `mean1`, `mean2` for
  [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md))
  or dropped (the observed/expected tables of
  [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md));

- one row per pair for correlations and post-hoc tests;

- one row per term for ANOVA tables and regression coefficients, and one
  row per item for
  [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)
  loadings;

- one row per cell for
  [`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)
  and per category for
  [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  (summary rows dropped, a `missing` column flags missing values);

- one row per scale for
  [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md).

For grouped analyses
([`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html))
the grouping variables are the leading columns; labelled grouping
variables show their value labels.

## Details

[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
(available once broom is loaded) renames the common columns to broom's
conventions: `statistic`, `p.value`, `parameter` (or `num.df`/`den.df`
for F tests), `estimate`, `conf.low`, `conf.high`, `std.error`,
`adj.p.value` (post-hoc tests), `term` (ANOVA tables), plus `method`
and, where it applies, `alternative`.
[`normality_test()`](https://YannickDiehl.github.io/mariposa/reference/normality_test.md)
results are tidied to one row per variable and test. The regressions
keep their own `tidy()`, `glance()` and `augment()` methods.

## See also

[`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)
for Excel export of results.

## Examples

``` r
tt <- t_test(survey_data, age, income, group = gender)
as.data.frame(tt)
#>   Variable group1 group2     t_stat       df   p_value  mean_diff     cohens_d
#> 1      age   Male Female -0.2290591 2468.094 0.8188420 -0.1558687 -0.009179957
#> 2   income   Male Female  0.6898740 2169.337 0.4903472 42.3196136  0.029532725
#>       hedges_g glass_delta conf_int_lower conf_int_upper   n1   n2      mean1
#> 1 -0.009177201 -0.00908362      -1.490227        1.17849 1194 1306   50.46817
#> 2  0.029522583  0.02959312     -77.979492      162.61872 1046 1140 3776.00382
#>        mean2        sd1        sd2 is_weighted note
#> 1   50.62404   17.15931   16.81293       FALSE <NA>
#> 2 3733.68421 1430.04925 1435.65121       FALSE <NA>

# Grouped analyses get the grouping variables as leading columns
survey_data |>
  dplyr::group_by(region) |>
  describe(age, income) |>
  as.data.frame()
#>   region Variable       Mean Median         SD Range  IQR  Skewness    N
#> 1   East      age   51.86804     52   17.42028    77   24 0.1478054  485
#> 2   East   income 3752.44755   3600 1386.87938  7200 1700 0.7288556  429
#> 3   West      age   50.23226     49   16.85635    77   24 0.1751428 2015
#> 4   West   income 3754.29710   3500 1444.17770  7200 1900 0.7306505 1757
#>   Missing
#> 1       0
#> 2      56
#> 3       0
#> 4     258

# One row per cell of a crosstab
as.data.frame(crosstab(survey_data, gender, region))
#>   gender region    n  row_pct expected adj_residual
#> 1   Male   East  238 19.93300  231.636    0.6444037
#> 2 Female   East  247 18.91271  253.364   -0.6444037
#> 3   Male   West  956 80.06700  962.364   -0.6444037
#> 4 Female   West 1059 81.08729 1052.636    0.6444037

# Save as CSV or convert to a tibble
csv <- tempfile(fileext = ".csv")
utils::write.csv(as.data.frame(tt), csv, row.names = FALSE)
tibble::as_tibble(chi_square(survey_data, gender, region))
#> # A tibble: 1 × 19
#>   row_var col_var chi_squared    df p_value     n pearson_chi_squared
#>   <chr>   <chr>         <dbl> <dbl>   <dbl> <int>               <dbl>
#> 1 gender  region        0.415     1   0.519  2500               0.415
#> # ℹ 12 more variables: pearson_p_value <dbl>, continuity_correction <lgl>,
#> #   cramers_v <dbl>, phi <dbl>, gamma <dbl>, contingency_c <dbl>,
#> #   table_rows <int>, table_cols <int>, phi_p_value <dbl>,
#> #   cramers_v_p_value <dbl>, gamma_p_value <dbl>, reason <chr>
unlink(csv)

# broom-style columns
if (requireNamespace("broom", quietly = TRUE)) {
  broom::tidy(oneway_anova(survey_data, age, group = education))
}
#> # A tibble: 1 × 11
#>   variable statistic num.df den.df p.value eta_squared epsilon_squared
#>   <chr>        <dbl>  <dbl>  <dbl>   <dbl>       <dbl>           <dbl>
#> 1 age           1.45      3   2496   0.226     0.00174        0.000542
#> # ℹ 4 more variables: omega_squared <dbl>, is_weighted <lgl>, note <chr>,
#> #   method <chr>
```
