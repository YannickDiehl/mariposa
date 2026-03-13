# Convert Labelled Variables to Factors

Converts `haven_labelled` variables to factors, using value labels as
factor levels. This is the primary function for making labelled survey
data ready for plotting, cross-tabulation, and regression analysis.

## Usage

``` r
to_label(
  data,
  ...,
  ordered = FALSE,
  drop.na = TRUE,
  drop.unused = FALSE,
  add.non.labelled = FALSE
)
```

## Arguments

- data:

  A data frame, tibble, or a single vector.

- ...:

  Optional: unquoted variable names (tidyselect supported). If empty,
  converts all `haven_labelled` columns.

- ordered:

  If `TRUE`, creates an ordered factor. Default: `FALSE`.

- drop.na:

  If `TRUE` (default), tagged NAs are converted to regular `NA`
  (excluded from factor levels). If `FALSE`, tagged NAs are kept as
  factor levels with their label text.

- drop.unused:

  If `TRUE`, removes factor levels with zero observations. Default:
  `FALSE`.

- add.non.labelled:

  If `TRUE`, values without labels are included as factor levels using
  their numeric value as the level name. Default: `FALSE` (unlabelled
  values become `NA`).

## Value

The input with labelled variables converted to factors. For single
vector input, returns a factor.

## Details

For each labelled variable, the numeric codes are replaced by their
associated value labels. The resulting factor levels are ordered by the
original numeric values (not alphabetically).

### When to Use This

Use `to_label()` when you:

- Want human-readable labels in plots (ggplot2)

- Need factor variables for regression models

- Want to create frequency tables with label text

## See also

[`to_character()`](https://YannickDiehl.github.io/mariposa/reference/to_character.md)
for character output,
[`to_labelled()`](https://YannickDiehl.github.io/mariposa/reference/to_labelled.md)
for the reverse operation,
[`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md)
for viewing labels

Other labels:
[`copy_labels()`](https://YannickDiehl.github.io/mariposa/reference/copy_labels.md),
[`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md),
[`find_var()`](https://YannickDiehl.github.io/mariposa/reference/find_var.md),
[`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md),
[`to_character()`](https://YannickDiehl.github.io/mariposa/reference/to_character.md),
[`to_labelled()`](https://YannickDiehl.github.io/mariposa/reference/to_labelled.md),
[`to_numeric()`](https://YannickDiehl.github.io/mariposa/reference/to_numeric.md),
[`unlabel()`](https://YannickDiehl.github.io/mariposa/reference/unlabel.md),
[`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md),
[`var_label()`](https://YannickDiehl.github.io/mariposa/reference/var_label.md)

## Examples

``` r
# Convert a single variable
to_label(survey_data$gender)
#>    [1] Female Male   Male   Female Male   Female Male   Male   Male   Male  
#>   [11] Female Male   Male   Female Female Female Female Female Male   Female
#>   [21] Female Female Female Male   Male   Female Female Female Male   Female
#>   [31] Female Female Female Female Male   Female Female Female Male   Male  
#>   [41] Male   Female Male   Male   Female Male   Female Female Male   Male  
#>   [51] Female Female Female Female Male   Female Male   Male   Female Female
#>   [61] Female Female Male   Male   Male   Female Male   Female Female Male  
#>   [71] Male   Female Female Female Male   Female Female Male   Female Male  
#>   [81] Female Female Female Male   Male   Male   Female Male   Female Female
#>   [91] Female Male   Male   Male   Male   Male   Male   Male   Male   Female
#>  [101] Male   Male   Male   Male   Female Male   Female Male   Male   Male  
#>  [111] Female Female Male   Female Male   Male   Male   Female Female Male  
#>  [121] Male   Male   Male   Male   Female Male   Male   Male   Male   Male  
#>  [131] Female Male   Male   Female Female Male   Female Female Female Female
#>  [141] Female Female Male   Male   Female Female Female Female Female Female
#>  [151] Male   Female Male   Female Male   Male   Female Female Female Female
#>  [161] Male   Male   Female Male   Male   Female Female Female Female Male  
#>  [171] Female Male   Female Female Female Male   Female Male   Male   Male  
#>  [181] Female Female Female Male   Female Female Male   Male   Female Female
#>  [191] Female Male   Male   Female Female Female Male   Female Male   Female
#>  [201] Female Male   Male   Male   Male   Male   Male   Female Male   Female
#>  [211] Female Male   Female Male   Female Female Female Male   Male   Female
#>  [221] Male   Female Male   Female Male   Female Female Female Female Female
#>  [231] Male   Male   Male   Female Female Female Female Female Female Female
#>  [241] Female Female Male   Male   Male   Female Female Male   Male   Male  
#>  [251] Female Male   Male   Female Female Male   Male   Male   Male   Female
#>  [261] Male   Male   Female Female Male   Male   Female Female Female Male  
#>  [271] Female Male   Male   Female Male   Male   Female Female Female Female
#>  [281] Male   Female Male   Male   Female Male   Female Male   Female Female
#>  [291] Male   Male   Male   Male   Male   Female Female Male   Female Male  
#>  [301] Female Female Female Male   Male   Female Female Male   Female Male  
#>  [311] Male   Female Male   Female Male   Female Male   Female Female Female
#>  [321] Female Female Female Male   Female Male   Female Male   Female Male  
#>  [331] Female Male   Male   Female Female Female Male   Female Female Male  
#>  [341] Male   Male   Female Male   Female Male   Female Male   Male   Male  
#>  [351] Male   Female Female Female Male   Female Male   Male   Male   Male  
#>  [361] Female Female Male   Female Female Female Female Male   Female Female
#>  [371] Female Male   Male   Female Female Male   Female Female Male   Male  
#>  [381] Female Female Male   Female Female Male   Male   Male   Male   Female
#>  [391] Female Male   Male   Male   Male   Male   Female Female Male   Female
#>  [401] Female Male   Female Male   Male   Male   Female Female Female Female
#>  [411] Female Female Male   Female Male   Male   Female Female Female Male  
#>  [421] Male   Male   Male   Male   Male   Male   Male   Male   Male   Male  
#>  [431] Female Female Female Male   Female Female Female Female Female Female
#>  [441] Male   Female Male   Male   Male   Female Male   Female Male   Female
#>  [451] Female Female Male   Male   Female Male   Male   Female Female Male  
#>  [461] Male   Male   Female Male   Male   Male   Female Female Male   Female
#>  [471] Male   Female Female Male   Female Male   Male   Female Male   Female
#>  [481] Male   Male   Male   Male   Female Male   Male   Female Male   Male  
#>  [491] Male   Female Male   Female Female Male   Male   Male   Male   Female
#>  [501] Female Male   Female Male   Female Female Male   Female Female Female
#>  [511] Female Male   Female Male   Male   Male   Female Male   Male   Female
#>  [521] Male   Male   Female Female Male   Female Male   Female Female Female
#>  [531] Female Male   Male   Male   Male   Female Male   Male   Male   Female
#>  [541] Female Female Female Female Male   Male   Female Male   Female Female
#>  [551] Female Male   Female Male   Male   Female Female Female Female Female
#>  [561] Male   Male   Female Female Male   Female Female Female Male   Female
#>  [571] Female Male   Male   Male   Male   Female Male   Female Male   Male  
#>  [581] Female Female Female Male   Female Male   Male   Female Female Female
#>  [591] Male   Male   Female Female Male   Male   Male   Female Female Male  
#>  [601] Female Female Male   Female Female Male   Male   Male   Female Male  
#>  [611] Female Female Female Male   Female Female Male   Male   Male   Female
#>  [621] Male   Male   Female Female Male   Female Female Female Female Female
#>  [631] Female Male   Female Female Female Male   Male   Male   Male   Female
#>  [641] Female Male   Female Female Male   Female Male   Female Female Female
#>  [651] Male   Female Female Male   Male   Female Female Male   Female Female
#>  [661] Female Female Female Female Female Female Female Female Male   Female
#>  [671] Female Female Male   Male   Female Male   Female Female Male   Male  
#>  [681] Female Male   Female Male   Female Female Female Female Female Male  
#>  [691] Female Male   Female Female Female Male   Male   Female Male   Male  
#>  [701] Female Male   Male   Female Female Male   Female Male   Female Male  
#>  [711] Male   Female Female Female Male   Female Male   Male   Male   Female
#>  [721] Female Female Male   Female Male   Male   Female Female Male   Male  
#>  [731] Male   Male   Male   Male   Male   Male   Female Male   Male   Female
#>  [741] Female Female Male   Female Male   Male   Male   Female Female Male  
#>  [751] Female Female Female Male   Male   Male   Male   Male   Female Male  
#>  [761] Female Female Male   Male   Male   Male   Male   Female Female Male  
#>  [771] Female Male   Female Male   Male   Female Male   Female Male   Male  
#>  [781] Female Female Female Male   Male   Female Male   Female Female Female
#>  [791] Female Male   Male   Female Male   Male   Male   Female Male   Male  
#>  [801] Female Female Female Male   Female Female Male   Female Female Female
#>  [811] Female Female Female Male   Female Male   Male   Female Male   Female
#>  [821] Male   Male   Female Male   Female Male   Female Female Male   Male  
#>  [831] Female Female Male   Female Male   Male   Female Male   Female Male  
#>  [841] Female Female Female Male   Female Male   Male   Female Male   Male  
#>  [851] Male   Female Female Female Male   Male   Male   Female Female Female
#>  [861] Female Female Female Male   Male   Male   Female Male   Male   Male  
#>  [871] Male   Male   Female Male   Female Female Male   Male   Female Female
#>  [881] Male   Female Male   Male   Male   Male   Female Male   Male   Male  
#>  [891] Female Female Male   Male   Female Male   Male   Female Female Male  
#>  [901] Male   Male   Female Male   Female Female Male   Male   Female Female
#>  [911] Female Female Female Female Male   Female Female Male   Female Female
#>  [921] Male   Female Female Female Female Male   Female Male   Male   Female
#>  [931] Female Male   Female Female Female Female Male   Female Male   Male  
#>  [941] Female Female Male   Female Male   Male   Female Female Male   Female
#>  [951] Male   Female Female Male   Female Female Male   Male   Male   Male  
#>  [961] Female Female Male   Male   Male   Female Male   Female Female Female
#>  [971] Male   Male   Female Male   Female Female Male   Male   Female Female
#>  [981] Female Male   Male   Female Male   Male   Female Male   Male   Female
#>  [991] Male   Male   Male   Male   Male   Female Female Female Female Female
#> [1001] Female Female Male   Female Male   Female Female Male   Male   Female
#> [1011] Male   Female Female Male   Female Male   Female Male   Male   Female
#> [1021] Female Male   Female Female Male   Female Female Male   Male   Female
#> [1031] Female Female Female Male   Male   Male   Male   Female Female Female
#> [1041] Male   Male   Male   Male   Male   Female Female Female Female Male  
#> [1051] Male   Female Female Male   Female Female Female Female Male   Female
#> [1061] Female Female Male   Male   Female Female Female Male   Male   Male  
#> [1071] Male   Female Female Male   Male   Male   Female Female Female Female
#> [1081] Male   Male   Male   Female Female Male   Male   Female Male   Male  
#> [1091] Male   Female Female Female Male   Female Female Male   Female Female
#> [1101] Female Male   Female Female Female Female Male   Female Female Female
#> [1111] Female Male   Male   Female Male   Female Male   Female Male   Male  
#> [1121] Male   Male   Male   Male   Female Female Female Female Male   Female
#> [1131] Female Female Male   Female Female Male   Female Female Female Female
#> [1141] Male   Male   Female Male   Male   Male   Male   Male   Female Female
#> [1151] Male   Male   Male   Male   Male   Female Male   Male   Male   Female
#> [1161] Male   Male   Male   Female Female Male   Male   Female Male   Male  
#> [1171] Female Female Female Female Female Male   Female Female Male   Female
#> [1181] Male   Female Female Male   Female Male   Male   Male   Male   Female
#> [1191] Male   Male   Male   Male   Female Male   Male   Male   Female Female
#> [1201] Male   Male   Male   Male   Male   Female Male   Male   Male   Male  
#> [1211] Male   Male   Female Male   Female Male   Female Male   Male   Female
#> [1221] Female Female Male   Female Female Female Male   Male   Male   Female
#> [1231] Female Male   Male   Male   Male   Male   Female Female Female Female
#> [1241] Female Male   Female Male   Male   Female Female Male   Male   Female
#> [1251] Female Female Female Female Male   Male   Female Male   Male   Female
#> [1261] Male   Male   Female Male   Female Male   Male   Female Female Male  
#> [1271] Female Male   Female Female Male   Female Female Male   Female Male  
#> [1281] Female Female Female Female Male   Male   Female Male   Female Female
#> [1291] Female Female Female Female Male   Female Female Male   Female Female
#> [1301] Female Female Female Female Male   Female Male   Female Male   Male  
#> [1311] Female Female Female Male   Male   Male   Female Female Female Female
#> [1321] Female Male   Female Female Male   Female Male   Female Female Male  
#> [1331] Male   Male   Male   Female Male   Female Female Male   Female Female
#> [1341] Female Female Male   Female Female Female Female Male   Female Male  
#> [1351] Male   Male   Male   Female Male   Male   Female Male   Female Female
#> [1361] Male   Female Female Female Female Male   Female Female Male   Female
#> [1371] Female Male   Male   Female Female Female Male   Female Male   Male  
#> [1381] Female Male   Male   Female Female Female Female Female Female Female
#> [1391] Female Male   Female Female Male   Female Male   Male   Male   Female
#> [1401] Male   Female Male   Female Female Male   Female Female Male   Female
#> [1411] Male   Male   Male   Male   Female Female Male   Female Male   Female
#> [1421] Female Female Male   Female Male   Male   Male   Female Female Female
#> [1431] Female Female Male   Male   Female Female Female Female Female Female
#> [1441] Female Male   Female Female Female Female Female Female Female Female
#> [1451] Female Male   Male   Female Male   Male   Male   Female Male   Male  
#> [1461] Male   Male   Female Male   Female Female Male   Male   Female Female
#> [1471] Male   Male   Male   Female Male   Female Female Female Female Male  
#> [1481] Male   Male   Male   Male   Female Female Female Male   Female Male  
#> [1491] Male   Male   Male   Male   Female Female Male   Male   Female Female
#> [1501] Male   Male   Male   Female Female Male   Male   Female Female Female
#> [1511] Female Male   Male   Male   Male   Male   Male   Female Female Male  
#> [1521] Female Female Male   Female Female Female Female Male   Female Male  
#> [1531] Male   Female Female Female Male   Female Female Male   Female Male  
#> [1541] Male   Female Male   Female Male   Male   Male   Female Female Female
#> [1551] Female Female Female Female Male   Female Male   Female Female Male  
#> [1561] Male   Female Female Male   Male   Female Female Female Female Male  
#> [1571] Male   Male   Female Male   Male   Female Female Male   Male   Male  
#> [1581] Male   Male   Female Male   Female Female Female Female Female Male  
#> [1591] Female Male   Female Female Male   Female Male   Female Female Male  
#> [1601] Male   Male   Male   Female Male   Male   Female Female Female Female
#> [1611] Male   Female Female Male   Male   Male   Male   Female Male   Male  
#> [1621] Female Male   Male   Male   Female Female Female Female Female Female
#> [1631] Female Male   Male   Female Female Female Female Female Female Male  
#> [1641] Male   Male   Female Female Female Male   Female Male   Female Male  
#> [1651] Female Male   Female Female Male   Female Male   Male   Female Male  
#> [1661] Female Female Male   Male   Female Female Male   Male   Male   Female
#> [1671] Male   Male   Female Female Male   Female Male   Female Male   Male  
#> [1681] Female Female Female Female Female Female Male   Male   Female Male  
#> [1691] Female Male   Female Female Female Female Female Male   Male   Female
#> [1701] Female Male   Male   Female Male   Female Male   Female Male   Female
#> [1711] Female Female Male   Male   Male   Male   Female Female Male   Female
#> [1721] Male   Female Male   Female Female Female Male   Male   Female Male  
#> [1731] Male   Male   Female Male   Female Male   Female Male   Male   Male  
#> [1741] Female Male   Female Female Female Female Female Female Male   Male  
#> [1751] Male   Male   Female Male   Male   Male   Female Female Male   Male  
#> [1761] Male   Male   Female Male   Female Male   Male   Male   Female Female
#> [1771] Male   Male   Male   Male   Female Male   Male   Male   Female Female
#> [1781] Female Female Female Female Male   Male   Male   Female Female Female
#> [1791] Male   Male   Female Female Female Female Female Male   Male   Female
#> [1801] Female Female Male   Male   Female Male   Female Male   Male   Female
#> [1811] Female Female Male   Male   Male   Male   Female Male   Male   Female
#> [1821] Female Female Female Female Female Female Female Female Female Male  
#> [1831] Male   Female Female Male   Male   Female Female Male   Female Male  
#> [1841] Male   Female Female Male   Male   Male   Male   Male   Female Male  
#> [1851] Male   Male   Female Female Male   Female Male   Female Female Female
#> [1861] Female Male   Female Female Male   Male   Male   Male   Female Male  
#> [1871] Male   Male   Female Male   Male   Male   Female Female Male   Male  
#> [1881] Female Female Female Female Male   Female Male   Female Female Male  
#> [1891] Male   Female Male   Male   Male   Female Female Female Female Female
#> [1901] Male   Male   Female Female Female Male   Female Female Female Female
#> [1911] Male   Male   Female Female Female Female Male   Male   Male   Female
#> [1921] Female Female Male   Female Male   Male   Male   Male   Male   Female
#> [1931] Male   Female Female Female Female Male   Male   Male   Male   Female
#> [1941] Female Female Female Female Male   Male   Female Female Male   Female
#> [1951] Male   Male   Female Female Male   Female Female Male   Female Male  
#> [1961] Male   Male   Male   Female Male   Male   Female Female Male   Male  
#> [1971] Male   Female Female Male   Female Male   Male   Male   Male   Female
#> [1981] Female Male   Female Male   Male   Female Female Male   Male   Female
#> [1991] Male   Male   Female Male   Male   Female Female Female Female Male  
#> [2001] Male   Female Female Female Male   Male   Male   Male   Female Male  
#> [2011] Male   Male   Male   Female Male   Male   Female Male   Male   Male  
#> [2021] Male   Female Female Female Female Female Male   Male   Female Male  
#> [2031] Female Female Female Female Male   Female Male   Female Male   Male  
#> [2041] Male   Female Male   Female Male   Male   Female Female Female Female
#> [2051] Female Male   Female Male   Male   Female Female Male   Male   Female
#> [2061] Female Female Male   Female Female Female Male   Male   Male   Male  
#> [2071] Female Female Female Female Female Male   Female Female Female Female
#> [2081] Male   Male   Male   Female Male   Male   Female Female Male   Male  
#> [2091] Female Female Male   Male   Male   Male   Female Male   Male   Male  
#> [2101] Male   Female Female Male   Male   Male   Female Female Male   Female
#> [2111] Male   Female Female Male   Male   Male   Female Male   Female Female
#> [2121] Female Male   Male   Male   Female Female Female Female Male   Male  
#> [2131] Male   Female Male   Male   Female Female Female Female Female Female
#> [2141] Male   Female Male   Female Female Male   Female Male   Male   Female
#> [2151] Female Female Male   Female Female Male   Female Male   Female Male  
#> [2161] Male   Female Male   Male   Male   Male   Female Female Female Male  
#> [2171] Female Female Female Male   Male   Male   Female Female Female Female
#> [2181] Male   Female Female Male   Female Female Male   Female Male   Male  
#> [2191] Male   Female Female Male   Female Female Female Male   Female Male  
#> [2201] Female Female Male   Male   Female Female Female Female Male   Female
#> [2211] Male   Female Male   Male   Female Female Male   Male   Female Male  
#> [2221] Male   Female Female Female Male   Male   Male   Male   Male   Male  
#> [2231] Female Female Female Male   Male   Male   Female Male   Male   Male  
#> [2241] Male   Male   Female Female Female Male   Male   Male   Male   Female
#> [2251] Male   Male   Male   Male   Male   Male   Female Female Female Female
#> [2261] Male   Male   Female Male   Female Female Male   Male   Female Female
#> [2271] Female Female Male   Female Male   Female Female Male   Female Female
#> [2281] Male   Male   Female Female Male   Female Female Female Male   Female
#> [2291] Female Female Male   Female Female Male   Female Male   Female Male  
#> [2301] Female Male   Male   Male   Female Male   Male   Male   Male   Female
#> [2311] Female Male   Female Female Male   Male   Male   Male   Female Female
#> [2321] Male   Male   Female Male   Female Female Female Male   Female Female
#> [2331] Female Male   Female Female Female Female Female Male   Male   Female
#> [2341] Male   Male   Male   Male   Female Female Male   Female Male   Female
#> [2351] Female Female Male   Male   Male   Female Female Female Female Male  
#> [2361] Female Female Female Female Female Male   Female Male   Female Female
#> [2371] Female Male   Male   Male   Male   Male   Male   Female Male   Male  
#> [2381] Male   Male   Female Male   Male   Female Female Female Male   Female
#> [2391] Female Female Female Male   Female Female Male   Male   Female Male  
#> [2401] Female Male   Male   Female Female Female Female Male   Female Female
#> [2411] Female Male   Male   Male   Male   Male   Male   Male   Male   Female
#> [2421] Female Female Female Male   Male   Male   Female Male   Female Male  
#> [2431] Male   Female Male   Female Female Male   Male   Female Male   Female
#> [2441] Male   Female Male   Male   Female Male   Male   Female Female Female
#> [2451] Female Female Male   Female Male   Male   Male   Female Female Male  
#> [2461] Male   Female Male   Female Female Male   Male   Female Female Female
#> [2471] Male   Male   Female Male   Female Male   Female Female Female Male  
#> [2481] Male   Female Male   Female Female Male   Male   Female Female Male  
#> [2491] Female Female Female Female Female Female Female Female Male   Male  
#> attr(,"label")
#> [1] Gender
#> Levels: Male Female

# Convert specific columns in a data frame
data <- to_label(survey_data, gender, region)

# Convert all labelled columns
data <- to_label(survey_data)

# Keep values without labels as factor levels
to_label(survey_data$life_satisfaction, add.non.labelled = TRUE)
#>    [1] 4    3    2    2    4    4    3    3    4    3    1    1    2    5   
#>   [15] 3    2    3    4    3    3    2    5    1    5    5    2    5    4   
#>   [29] 5    5    3    3    2    3    5    1    <NA> 2    3    3    3    2   
#>   [43] 4    3    3    3    4    3    2    3    3    4    3    3    3    1   
#>   [57] 4    4    2    5    2    5    4    2    2    5    2    5    5    3   
#>   [71] 5    <NA> 2    5    3    2    5    3    5    4    5    2    5    5   
#>   [85] 5    4    4    <NA> 5    4    4    5    3    4    3    4    2    <NA>
#>   [99] 5    4    1    4    1    3    4    3    4    2    4    2    4    3   
#>  [113] <NA> 5    2    5    <NA> 4    4    5    5    3    3    1    3    1   
#>  [127] 4    4    5    4    4    <NA> 5    2    3    5    5    4    5    2   
#>  [141] 3    5    5    5    3    4    5    5    3    5    4    4    5    5   
#>  [155] 5    2    4    5    3    3    4    4    2    4    5    4    2    1   
#>  [169] 5    4    3    3    3    4    4    5    4    3    2    4    4    5   
#>  [183] 4    4    5    4    5    5    <NA> 2    1    4    4    5    4    4   
#>  [197] 5    4    2    4    5    1    <NA> 4    4    <NA> 1    3    3    5   
#>  [211] 4    5    3    3    <NA> 3    4    5    2    3    5    5    5    5   
#>  [225] 5    2    3    2    4    3    3    4    5    5    5    2    5    <NA>
#>  [239] 5    5    5    5    5    2    2    2    5    4    5    4    3    4   
#>  [253] 4    5    4    3    5    3    <NA> 5    5    1    2    5    4    2   
#>  [267] 2    3    4    1    4    5    5    1    3    4    5    4    3    5   
#>  [281] 3    4    3    4    4    4    4    3    1    4    3    5    4    4   
#>  [295] 3    5    5    5    5    2    3    2    3    3    5    4    3    5   
#>  [309] 4    3    3    3    5    4    3    3    3    <NA> 5    3    3    1   
#>  [323] 5    4    4    4    4    5    4    3    4    5    2    4    4    5   
#>  [337] 5    4    3    3    5    5    4    3    5    4    2    2    4    5   
#>  [351] 1    3    2    2    3    3    3    5    5    2    5    2    3    3   
#>  [365] 4    4    5    4    5    5    2    5    5    4    5    1    <NA> 4   
#>  [379] 2    4    3    5    3    2    3    3    2    3    3    5    5    5   
#>  [393] 2    2    2    5    5    4    2    4    4    5    1    1    4    4   
#>  [407] 3    5    4    5    5    3    4    4    5    3    4    5    3    1   
#>  [421] 3    3    5    4    4    4    4    4    2    2    2    4    3    4   
#>  [435] 2    3    4    3    1    4    2    4    5    2    3    3    4    5   
#>  [449] <NA> 4    4    5    5    4    2    1    1    3    3    4    <NA> 3   
#>  [463] 2    3    2    4    4    3    4    3    2    5    4    3    3    4   
#>  [477] 3    4    5    4    1    2    3    4    3    3    2    3    3    3   
#>  [491] 4    5    4    5    2    5    5    2    5    4    3    2    3    4   
#>  [505] 4    2    2    5    2    2    4    4    5    3    1    4    3    5   
#>  [519] 4    5    <NA> 3    2    5    1    3    3    5    3    4    2    2   
#>  [533] 3    5    5    3    4    2    5    5    4    5    1    4    5    2   
#>  [547] 4    5    2    <NA> 4    4    5    4    4    2    5    4    4    4   
#>  [561] 3    4    3    3    4    5    3    5    3    4    5    4    3    5   
#>  [575] 1    4    4    5    1    4    3    5    3    4    <NA> 3    3    3   
#>  [589] 3    4    5    5    5    3    5    5    4    3    4    4    3    5   
#>  [603] 2    5    2    4    5    3    5    2    3    4    3    4    1    4   
#>  [617] 4    4    5    4    5    3    5    5    3    4    4    5    3    3   
#>  [631] 5    5    4    5    3    4    2    4    5    4    5    5    4    5   
#>  [645] 4    3    4    4    1    4    2    4    4    5    <NA> 1    2    4   
#>  [659] 5    4    4    <NA> 4    3    5    5    4    5    2    3    5    3   
#>  [673] 5    3    4    5    4    4    5    3    3    4    2    5    4    3   
#>  [687] 2    5    3    3    4    1    5    3    4    4    4    3    4    4   
#>  [701] 1    4    5    5    3    5    3    3    5    5    4    1    1    3   
#>  [715] 3    5    <NA> 3    5    4    5    5    5    3    5    1    5    4   
#>  [729] 4    4    4    3    4    3    5    5    2    5    5    4    3    1   
#>  [743] 5    3    3    3    3    2    5    5    4    2    4    2    4    3   
#>  [757] 3    4    5    5    4    3    3    3    5    5    <NA> 4    1    1   
#>  [771] 3    5    4    5    3    4    2    3    2    5    2    5    5    3   
#>  [785] 3    5    5    4    5    4    5    3    4    4    3    3    5    4   
#>  [799] 5    3    3    3    <NA> 4    3    4    3    4    1    3    4    5   
#>  [813] 1    5    <NA> 2    3    2    1    5    5    4    5    3    3    5   
#>  [827] 3    5    1    4    1    5    3    4    3    2    3    4    3    3   
#>  [841] 4    5    2    5    5    1    3    2    3    2    4    <NA> 5    3   
#>  [855] 4    3    5    2    3    4    5    5    4    5    5    3    4    5   
#>  [869] 4    2    3    1    4    5    4    3    5    4    5    5    5    3   
#>  [883] 2    4    5    5    4    4    4    2    4    3    1    5    3    4   
#>  [897] 5    3    4    4    4    <NA> 4    5    4    4    4    4    3    2   
#>  [911] 4    4    5    5    5    4    4    5    5    2    3    5    3    5   
#>  [925] 2    2    2    3    3    4    3    4    4    4    5    5    2    5   
#>  [939] 5    5    5    4    4    3    2    2    4    4    4    2    3    3   
#>  [953] 3    3    5    5    4    5    4    5    3    2    5    2    5    3   
#>  [967] 1    4    2    <NA> 3    3    2    5    3    2    5    3    4    3   
#>  [981] 3    4    3    4    2    5    2    2    3    5    3    5    4    4   
#>  [995] 4    3    3    5    3    3    4    4    3    3    3    5    4    4   
#> [1009] 5    4    5    3    4    <NA> 2    3    5    3    5    5    1    5   
#> [1023] 4    3    5    4    3    5    4    3    5    4    5    3    5    3   
#> [1037] <NA> 1    5    4    4    3    2    3    3    4    3    5    3    5   
#> [1051] <NA> 2    3    5    5    1    2    5    5    4    3    1    3    5   
#> [1065] 2    3    4    5    5    4    5    4    3    <NA> 4    4    2    5   
#> [1079] 2    4    5    2    3    4    4    3    2    5    2    4    5    3   
#> [1093] 5    3    4    2    5    3    4    5    4    4    3    5    3    2   
#> [1107] 3    5    5    <NA> 5    5    5    3    2    5    3    3    3    2   
#> [1121] 4    3    3    5    4    3    5    5    3    5    4    4    4    5   
#> [1135] 4    3    5    3    5    5    <NA> 4    1    3    5    3    5    4   
#> [1149] <NA> 4    4    4    4    5    5    4    3    4    2    4    4    4   
#> [1163] 5    2    5    5    4    3    3    4    2    4    5    5    1    3   
#> [1177] <NA> 2    5    2    4    3    3    2    4    5    4    5    3    3   
#> [1191] 3    1    2    4    5    3    2    4    1    4    3    5    5    4   
#> [1205] 5    3    3    5    3    5    3    5    5    5    1    2    2    4   
#> [1219] 4    5    3    4    1    5    3    2    1    1    3    5    5    5   
#> [1233] 4    2    4    4    2    2    <NA> 4    2    5    5    4    5    4   
#> [1247] 3    3    2    4    3    3    3    3    3    3    2    <NA> 5    4   
#> [1261] 2    3    5    5    5    3    5    3    4    3    5    5    5    2   
#> [1275] 5    4    4    4    4    <NA> 5    5    5    4    <NA> 4    4    5   
#> [1289] <NA> 3    2    5    3    2    4    4    3    5    4    5    4    5   
#> [1303] 5    5    2    4    2    3    4    4    4    2    5    3    3    4   
#> [1317] 4    4    <NA> 4    3    5    3    5    <NA> 4    5    3    5    2   
#> [1331] 2    5    3    5    4    4    5    4    2    5    4    1    4    3   
#> [1345] 4    3    2    2    4    1    4    5    5    4    5    4    1    2   
#> [1359] 5    5    3    4    4    4    4    5    3    1    4    3    3    5   
#> [1373] 3    5    4    5    3    5    4    4    4    3    <NA> 5    3    4   
#> [1387] 3    1    5    5    3    3    4    2    4    5    2    4    4    3   
#> [1401] 5    2    4    4    4    3    5    5    <NA> 5    4    2    2    2   
#> [1415] 4    4    5    3    4    3    4    4    3    4    5    2    2    <NA>
#> [1429] 4    5    1    5    5    3    5    5    5    4    4    5    3    5   
#> [1443] 5    2    3    3    3    5    4    5    2    5    3    4    3    4   
#> [1457] 5    1    4    <NA> 4    3    5    4    3    5    4    3    5    4   
#> [1471] 2    4    4    4    4    2    2    5    4    3    2    4    2    2   
#> [1485] 5    4    3    3    5    4    4    2    4    2    5    5    3    2   
#> [1499] 4    4    4    4    1    4    2    4    4    4    4    2    2    4   
#> [1513] 5    5    3    4    4    3    4    2    3    4    3    5    4    3   
#> [1527] 4    4    4    5    4    2    2    4    5    3    3    3    3    4   
#> [1541] 3    2    5    1    3    3    3    3    5    1    3    3    5    4   
#> [1555] 1    4    4    3    2    3    2    1    4    4    5    5    5    3   
#> [1569] 3    4    5    4    2    <NA> 5    4    5    2    3    2    3    4   
#> [1583] 5    3    4    2    4    4    3    3    2    5    4    2    <NA> 2   
#> [1597] 4    4    4    3    2    4    4    3    3    3    4    2    2    3   
#> [1611] 3    4    5    <NA> 5    2    3    4    3    4    3    4    5    2   
#> [1625] 4    5    3    3    5    4    5    5    4    1    3    4    3    5   
#> [1639] 5    3    4    <NA> 4    4    4    2    5    2    4    3    2    4   
#> [1653] 3    3    4    4    3    5    3    4    5    4    5    2    3    5   
#> [1667] 3    5    4    4    2    4    1    3    5    4    5    3    5    1   
#> [1681] 4    5    3    4    4    2    2    5    3    5    5    5    5    4   
#> [1695] 2    4    3    3    3    2    5    4    1    4    2    <NA> 5    4   
#> [1709] 4    3    2    4    3    4    5    4    4    3    5    4    4    5   
#> [1723] 5    2    3    <NA> 4    3    2    1    3    3    2    4    <NA> 1   
#> [1737] 4    5    2    5    3    3    4    4    4    5    3    5    3    <NA>
#> [1751] 5    3    4    4    3    5    5    2    3    4    4    5    3    5   
#> [1765] 2    5    5    4    4    5    4    3    3    4    4    5    3    4   
#> [1779] 1    5    5    4    4    4    2    4    5    4    4    4    4    5   
#> [1793] 4    5    <NA> 5    3    5    5    5    5    3    5    3    4    <NA>
#> [1807] 2    5    3    2    4    5    3    1    4    4    3    1    2    3   
#> [1821] 4    4    5    5    5    2    4    4    5    1    5    3    4    3   
#> [1835] 4    3    4    3    5    4    5    3    2    5    <NA> 5    5    3   
#> [1849] 4    2    5    1    4    5    4    2    3    3    3    3    3    3   
#> [1863] 4    4    <NA> 3    1    4    4    4    3    3    3    4    4    5   
#> [1877] 5    3    4    <NA> 4    3    3    4    5    5    4    4    4    4   
#> [1891] 3    3    3    5    4    4    3    2    4    4    4    4    2    4   
#> [1905] 1    5    2    5    3    5    <NA> 5    2    5    2    4    1    2   
#> [1919] 5    3    4    3    2    3    2    4    4    5    4    5    4    4   
#> [1933] 5    5    4    2    5    5    4    2    2    5    5    4    3    5   
#> [1947] <NA> 2    3    4    3    2    4    5    1    4    4    4    5    3   
#> [1961] 5    5    4    2    2    3    2    4    5    4    5    1    5    4   
#> [1975] 4    4    3    3    5    5    4    5    5    4    2    3    3    5   
#> [1989] 4    <NA> 5    5    3    5    5    1    3    4    3    2    4    3   
#> [2003] 4    5    5    5    5    3    3    3    4    4    3    4    5    4   
#> [2017] 5    3    2    3    5    4    4    5    4    3    4    4    5    3   
#> [2031] 2    4    5    2    4    <NA> 4    4    3    3    4    3    4    4   
#> [2045] 3    3    3    3    5    3    1    3    3    1    2    4    3    3   
#> [2059] 5    2    5    3    1    3    5    1    4    4    4    5    5    3   
#> [2073] 5    4    4    3    5    5    3    5    4    4    3    4    5    2   
#> [2087] 5    4    4    4    3    3    4    1    4    3    1    3    3    5   
#> [2101] 4    4    5    2    3    1    5    4    3    5    1    4    5    5   
#> [2115] 4    <NA> 1    5    5    4    4    2    5    4    3    4    3    3   
#> [2129] 3    4    3    3    5    3    3    5    5    5    2    2    3    3   
#> [2143] 4    5    4    5    1    5    2    4    3    3    4    3    5    3   
#> [2157] 4    3    5    4    2    5    3    5    4    5    2    3    5    5   
#> [2171] 5    3    4    2    5    4    4    5    1    5    <NA> 4    5    3   
#> [2185] 2    4    4    4    3    2    5    4    3    4    4    4    5    2   
#> [2199] <NA> 4    3    3    3    5    2    3    2    3    4    3    4    5   
#> [2213] <NA> 4    3    4    4    4    5    4    4    3    3    2    3    3   
#> [2227] 5    4    4    5    2    1    5    4    2    1    3    3    4    5   
#> [2241] 4    5    2    5    5    5    4    3    4    2    3    4    4    5   
#> [2255] 4    5    4    3    4    5    3    4    2    5    4    1    4    2   
#> [2269] 4    3    4    4    4    5    4    5    2    5    <NA> 4    5    5   
#> [2283] 3    4    <NA> 5    3    5    1    3    2    4    5    5    5    2   
#> [2297] 4    3    1    3    3    5    3    3    2    4    2    5    2    5   
#> [2311] 4    5    5    5    3    5    4    4    4    2    <NA> 2    <NA> 3   
#> [2325] 2    3    3    3    4    4    5    2    4    3    1    2    3    3   
#> [2339] 2    4    1    4    <NA> 4    4    3    <NA> 3    2    5    4    5   
#> [2353] 3    3    1    4    2    5    4    1    4    2    4    3    2    3   
#> [2367] 4    5    5    5    4    3    5    4    2    4    <NA> 1    5    5   
#> [2381] 1    1    4    3    4    5    3    2    5    4    3    4    4    3   
#> [2395] 4    4    3    4    5    3    4    2    5    5    5    5    4    3   
#> [2409] 5    5    4    3    3    5    5    3    1    5    5    <NA> 5    5   
#> [2423] 4    4    4    4    3    5    5    3    5    4    4    2    4    5   
#> [2437] 3    2    4    5    5    5    3    3    4    2    3    5    3    4   
#> [2451] <NA> 5    5    5    4    3    3    4    3    3    5    5    3    3   
#> [2465] 4    5    2    4    4    4    3    4    5    3    4    2    4    5   
#> [2479] 3    3    3    3    5    3    5    3    2    5    4    4    <NA> 5   
#> [2493] 1    5    4    4    2    4    2    4   
#> Levels: 1 2 3 4 5
```
