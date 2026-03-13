# Convert Factors or Labelled Variables to Numeric

Converts factor levels or labelled values to numeric. For factors, uses
the underlying integer codes (or the numeric value of levels if they are
numeric strings). For labelled variables, extracts the underlying
numeric values.

## Usage

``` r
to_numeric(data, ..., use.labels = TRUE, start.at = NULL, keep.labels = FALSE)
```

## Arguments

- data:

  A data frame, tibble, or a single vector.

- ...:

  Optional: unquoted variable names (tidyselect supported). If empty,
  converts all factor columns.

- use.labels:

  If `TRUE` (default), attempts to use the numeric value of factor
  levels (e.g., level `"3"` becomes `3`). If `FALSE`, uses sequential
  integers (1, 2, 3, ...).

- start.at:

  If not `NULL`, the lowest numeric value in the output starts at this
  number. Default: `NULL` (use original values).

- keep.labels:

  If `TRUE`, the former factor levels are stored as value labels on the
  result. Default: `FALSE`.

## Value

The input with variables converted to numeric. For single vector input,
returns a numeric vector.

## Details

This function handles three input types:

1.  **Numeric factors** (levels like `"1"`, `"2"`, `"3"`): Extracts the
    numeric values from the level names.

2.  **Text factors** (levels like `"Male"`, `"Female"`): Converts to
    sequential integers by default; use `use.labels = FALSE` to force
    this behavior even for numeric-looking levels.

3.  **haven_labelled**: Extracts the underlying numeric vector,
    stripping the labelled class.

## See also

[`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)
for the reverse (numeric → factor),
[`to_labelled()`](https://YannickDiehl.github.io/mariposa/reference/to_labelled.md)
for creating labelled vectors

Other labels:
[`copy_labels()`](https://YannickDiehl.github.io/mariposa/reference/copy_labels.md),
[`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md),
[`find_var()`](https://YannickDiehl.github.io/mariposa/reference/find_var.md),
[`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md),
[`to_character()`](https://YannickDiehl.github.io/mariposa/reference/to_character.md),
[`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md),
[`to_labelled()`](https://YannickDiehl.github.io/mariposa/reference/to_labelled.md),
[`unlabel()`](https://YannickDiehl.github.io/mariposa/reference/unlabel.md),
[`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md),
[`var_label()`](https://YannickDiehl.github.io/mariposa/reference/var_label.md)

## Examples

``` r
# Numeric factor levels → numeric
x <- factor(c("1", "3", "5", "3"))
to_numeric(x)
#> [1] 1 3 5 3
# [1] 1 3 5 3

# Sequential integers
to_numeric(x, use.labels = FALSE)
#> [1] 1 2 3 2
# [1] 1 2 3 2

# Haven labelled → plain numeric
to_numeric(survey_data$life_satisfaction)
#>    [1]  4  3  2  2  4  4  3  3  4  3  1  1  2  5  3  2  3  4  3  3  2  5  1  5
#>   [25]  5  2  5  4  5  5  3  3  2  3  5  1 NA  2  3  3  3  2  4  3  3  3  4  3
#>   [49]  2  3  3  4  3  3  3  1  4  4  2  5  2  5  4  2  2  5  2  5  5  3  5 NA
#>   [73]  2  5  3  2  5  3  5  4  5  2  5  5  5  4  4 NA  5  4  4  5  3  4  3  4
#>   [97]  2 NA  5  4  1  4  1  3  4  3  4  2  4  2  4  3 NA  5  2  5 NA  4  4  5
#>  [121]  5  3  3  1  3  1  4  4  5  4  4 NA  5  2  3  5  5  4  5  2  3  5  5  5
#>  [145]  3  4  5  5  3  5  4  4  5  5  5  2  4  5  3  3  4  4  2  4  5  4  2  1
#>  [169]  5  4  3  3  3  4  4  5  4  3  2  4  4  5  4  4  5  4  5  5 NA  2  1  4
#>  [193]  4  5  4  4  5  4  2  4  5  1 NA  4  4 NA  1  3  3  5  4  5  3  3 NA  3
#>  [217]  4  5  2  3  5  5  5  5  5  2  3  2  4  3  3  4  5  5  5  2  5 NA  5  5
#>  [241]  5  5  5  2  2  2  5  4  5  4  3  4  4  5  4  3  5  3 NA  5  5  1  2  5
#>  [265]  4  2  2  3  4  1  4  5  5  1  3  4  5  4  3  5  3  4  3  4  4  4  4  3
#>  [289]  1  4  3  5  4  4  3  5  5  5  5  2  3  2  3  3  5  4  3  5  4  3  3  3
#>  [313]  5  4  3  3  3 NA  5  3  3  1  5  4  4  4  4  5  4  3  4  5  2  4  4  5
#>  [337]  5  4  3  3  5  5  4  3  5  4  2  2  4  5  1  3  2  2  3  3  3  5  5  2
#>  [361]  5  2  3  3  4  4  5  4  5  5  2  5  5  4  5  1 NA  4  2  4  3  5  3  2
#>  [385]  3  3  2  3  3  5  5  5  2  2  2  5  5  4  2  4  4  5  1  1  4  4  3  5
#>  [409]  4  5  5  3  4  4  5  3  4  5  3  1  3  3  5  4  4  4  4  4  2  2  2  4
#>  [433]  3  4  2  3  4  3  1  4  2  4  5  2  3  3  4  5 NA  4  4  5  5  4  2  1
#>  [457]  1  3  3  4 NA  3  2  3  2  4  4  3  4  3  2  5  4  3  3  4  3  4  5  4
#>  [481]  1  2  3  4  3  3  2  3  3  3  4  5  4  5  2  5  5  2  5  4  3  2  3  4
#>  [505]  4  2  2  5  2  2  4  4  5  3  1  4  3  5  4  5 NA  3  2  5  1  3  3  5
#>  [529]  3  4  2  2  3  5  5  3  4  2  5  5  4  5  1  4  5  2  4  5  2 NA  4  4
#>  [553]  5  4  4  2  5  4  4  4  3  4  3  3  4  5  3  5  3  4  5  4  3  5  1  4
#>  [577]  4  5  1  4  3  5  3  4 NA  3  3  3  3  4  5  5  5  3  5  5  4  3  4  4
#>  [601]  3  5  2  5  2  4  5  3  5  2  3  4  3  4  1  4  4  4  5  4  5  3  5  5
#>  [625]  3  4  4  5  3  3  5  5  4  5  3  4  2  4  5  4  5  5  4  5  4  3  4  4
#>  [649]  1  4  2  4  4  5 NA  1  2  4  5  4  4 NA  4  3  5  5  4  5  2  3  5  3
#>  [673]  5  3  4  5  4  4  5  3  3  4  2  5  4  3  2  5  3  3  4  1  5  3  4  4
#>  [697]  4  3  4  4  1  4  5  5  3  5  3  3  5  5  4  1  1  3  3  5 NA  3  5  4
#>  [721]  5  5  5  3  5  1  5  4  4  4  4  3  4  3  5  5  2  5  5  4  3  1  5  3
#>  [745]  3  3  3  2  5  5  4  2  4  2  4  3  3  4  5  5  4  3  3  3  5  5 NA  4
#>  [769]  1  1  3  5  4  5  3  4  2  3  2  5  2  5  5  3  3  5  5  4  5  4  5  3
#>  [793]  4  4  3  3  5  4  5  3  3  3 NA  4  3  4  3  4  1  3  4  5  1  5 NA  2
#>  [817]  3  2  1  5  5  4  5  3  3  5  3  5  1  4  1  5  3  4  3  2  3  4  3  3
#>  [841]  4  5  2  5  5  1  3  2  3  2  4 NA  5  3  4  3  5  2  3  4  5  5  4  5
#>  [865]  5  3  4  5  4  2  3  1  4  5  4  3  5  4  5  5  5  3  2  4  5  5  4  4
#>  [889]  4  2  4  3  1  5  3  4  5  3  4  4  4 NA  4  5  4  4  4  4  3  2  4  4
#>  [913]  5  5  5  4  4  5  5  2  3  5  3  5  2  2  2  3  3  4  3  4  4  4  5  5
#>  [937]  2  5  5  5  5  4  4  3  2  2  4  4  4  2  3  3  3  3  5  5  4  5  4  5
#>  [961]  3  2  5  2  5  3  1  4  2 NA  3  3  2  5  3  2  5  3  4  3  3  4  3  4
#>  [985]  2  5  2  2  3  5  3  5  4  4  4  3  3  5  3  3  4  4  3  3  3  5  4  4
#> [1009]  5  4  5  3  4 NA  2  3  5  3  5  5  1  5  4  3  5  4  3  5  4  3  5  4
#> [1033]  5  3  5  3 NA  1  5  4  4  3  2  3  3  4  3  5  3  5 NA  2  3  5  5  1
#> [1057]  2  5  5  4  3  1  3  5  2  3  4  5  5  4  5  4  3 NA  4  4  2  5  2  4
#> [1081]  5  2  3  4  4  3  2  5  2  4  5  3  5  3  4  2  5  3  4  5  4  4  3  5
#> [1105]  3  2  3  5  5 NA  5  5  5  3  2  5  3  3  3  2  4  3  3  5  4  3  5  5
#> [1129]  3  5  4  4  4  5  4  3  5  3  5  5 NA  4  1  3  5  3  5  4 NA  4  4  4
#> [1153]  4  5  5  4  3  4  2  4  4  4  5  2  5  5  4  3  3  4  2  4  5  5  1  3
#> [1177] NA  2  5  2  4  3  3  2  4  5  4  5  3  3  3  1  2  4  5  3  2  4  1  4
#> [1201]  3  5  5  4  5  3  3  5  3  5  3  5  5  5  1  2  2  4  4  5  3  4  1  5
#> [1225]  3  2  1  1  3  5  5  5  4  2  4  4  2  2 NA  4  2  5  5  4  5  4  3  3
#> [1249]  2  4  3  3  3  3  3  3  2 NA  5  4  2  3  5  5  5  3  5  3  4  3  5  5
#> [1273]  5  2  5  4  4  4  4 NA  5  5  5  4 NA  4  4  5 NA  3  2  5  3  2  4  4
#> [1297]  3  5  4  5  4  5  5  5  2  4  2  3  4  4  4  2  5  3  3  4  4  4 NA  4
#> [1321]  3  5  3  5 NA  4  5  3  5  2  2  5  3  5  4  4  5  4  2  5  4  1  4  3
#> [1345]  4  3  2  2  4  1  4  5  5  4  5  4  1  2  5  5  3  4  4  4  4  5  3  1
#> [1369]  4  3  3  5  3  5  4  5  3  5  4  4  4  3 NA  5  3  4  3  1  5  5  3  3
#> [1393]  4  2  4  5  2  4  4  3  5  2  4  4  4  3  5  5 NA  5  4  2  2  2  4  4
#> [1417]  5  3  4  3  4  4  3  4  5  2  2 NA  4  5  1  5  5  3  5  5  5  4  4  5
#> [1441]  3  5  5  2  3  3  3  5  4  5  2  5  3  4  3  4  5  1  4 NA  4  3  5  4
#> [1465]  3  5  4  3  5  4  2  4  4  4  4  2  2  5  4  3  2  4  2  2  5  4  3  3
#> [1489]  5  4  4  2  4  2  5  5  3  2  4  4  4  4  1  4  2  4  4  4  4  2  2  4
#> [1513]  5  5  3  4  4  3  4  2  3  4  3  5  4  3  4  4  4  5  4  2  2  4  5  3
#> [1537]  3  3  3  4  3  2  5  1  3  3  3  3  5  1  3  3  5  4  1  4  4  3  2  3
#> [1561]  2  1  4  4  5  5  5  3  3  4  5  4  2 NA  5  4  5  2  3  2  3  4  5  3
#> [1585]  4  2  4  4  3  3  2  5  4  2 NA  2  4  4  4  3  2  4  4  3  3  3  4  2
#> [1609]  2  3  3  4  5 NA  5  2  3  4  3  4  3  4  5  2  4  5  3  3  5  4  5  5
#> [1633]  4  1  3  4  3  5  5  3  4 NA  4  4  4  2  5  2  4  3  2  4  3  3  4  4
#> [1657]  3  5  3  4  5  4  5  2  3  5  3  5  4  4  2  4  1  3  5  4  5  3  5  1
#> [1681]  4  5  3  4  4  2  2  5  3  5  5  5  5  4  2  4  3  3  3  2  5  4  1  4
#> [1705]  2 NA  5  4  4  3  2  4  3  4  5  4  4  3  5  4  4  5  5  2  3 NA  4  3
#> [1729]  2  1  3  3  2  4 NA  1  4  5  2  5  3  3  4  4  4  5  3  5  3 NA  5  3
#> [1753]  4  4  3  5  5  2  3  4  4  5  3  5  2  5  5  4  4  5  4  3  3  4  4  5
#> [1777]  3  4  1  5  5  4  4  4  2  4  5  4  4  4  4  5  4  5 NA  5  3  5  5  5
#> [1801]  5  3  5  3  4 NA  2  5  3  2  4  5  3  1  4  4  3  1  2  3  4  4  5  5
#> [1825]  5  2  4  4  5  1  5  3  4  3  4  3  4  3  5  4  5  3  2  5 NA  5  5  3
#> [1849]  4  2  5  1  4  5  4  2  3  3  3  3  3  3  4  4 NA  3  1  4  4  4  3  3
#> [1873]  3  4  4  5  5  3  4 NA  4  3  3  4  5  5  4  4  4  4  3  3  3  5  4  4
#> [1897]  3  2  4  4  4  4  2  4  1  5  2  5  3  5 NA  5  2  5  2  4  1  2  5  3
#> [1921]  4  3  2  3  2  4  4  5  4  5  4  4  5  5  4  2  5  5  4  2  2  5  5  4
#> [1945]  3  5 NA  2  3  4  3  2  4  5  1  4  4  4  5  3  5  5  4  2  2  3  2  4
#> [1969]  5  4  5  1  5  4  4  4  3  3  5  5  4  5  5  4  2  3  3  5  4 NA  5  5
#> [1993]  3  5  5  1  3  4  3  2  4  3  4  5  5  5  5  3  3  3  4  4  3  4  5  4
#> [2017]  5  3  2  3  5  4  4  5  4  3  4  4  5  3  2  4  5  2  4 NA  4  4  3  3
#> [2041]  4  3  4  4  3  3  3  3  5  3  1  3  3  1  2  4  3  3  5  2  5  3  1  3
#> [2065]  5  1  4  4  4  5  5  3  5  4  4  3  5  5  3  5  4  4  3  4  5  2  5  4
#> [2089]  4  4  3  3  4  1  4  3  1  3  3  5  4  4  5  2  3  1  5  4  3  5  1  4
#> [2113]  5  5  4 NA  1  5  5  4  4  2  5  4  3  4  3  3  3  4  3  3  5  3  3  5
#> [2137]  5  5  2  2  3  3  4  5  4  5  1  5  2  4  3  3  4  3  5  3  4  3  5  4
#> [2161]  2  5  3  5  4  5  2  3  5  5  5  3  4  2  5  4  4  5  1  5 NA  4  5  3
#> [2185]  2  4  4  4  3  2  5  4  3  4  4  4  5  2 NA  4  3  3  3  5  2  3  2  3
#> [2209]  4  3  4  5 NA  4  3  4  4  4  5  4  4  3  3  2  3  3  5  4  4  5  2  1
#> [2233]  5  4  2  1  3  3  4  5  4  5  2  5  5  5  4  3  4  2  3  4  4  5  4  5
#> [2257]  4  3  4  5  3  4  2  5  4  1  4  2  4  3  4  4  4  5  4  5  2  5 NA  4
#> [2281]  5  5  3  4 NA  5  3  5  1  3  2  4  5  5  5  2  4  3  1  3  3  5  3  3
#> [2305]  2  4  2  5  2  5  4  5  5  5  3  5  4  4  4  2 NA  2 NA  3  2  3  3  3
#> [2329]  4  4  5  2  4  3  1  2  3  3  2  4  1  4 NA  4  4  3 NA  3  2  5  4  5
#> [2353]  3  3  1  4  2  5  4  1  4  2  4  3  2  3  4  5  5  5  4  3  5  4  2  4
#> [2377] NA  1  5  5  1  1  4  3  4  5  3  2  5  4  3  4  4  3  4  4  3  4  5  3
#> [2401]  4  2  5  5  5  5  4  3  5  5  4  3  3  5  5  3  1  5  5 NA  5  5  4  4
#> [2425]  4  4  3  5  5  3  5  4  4  2  4  5  3  2  4  5  5  5  3  3  4  2  3  5
#> [2449]  3  4 NA  5  5  5  4  3  3  4  3  3  5  5  3  3  4  5  2  4  4  4  3  4
#> [2473]  5  3  4  2  4  5  3  3  3  3  5  3  5  3  2  5  4  4 NA  5  1  5  4  4
#> [2497]  2  4  2  4
```
