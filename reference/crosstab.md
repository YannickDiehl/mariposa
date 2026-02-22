# Compare Two Categories: See How They Relate

`crosstab()` shows you how two categorical variables relate to each
other. It creates a table that reveals patterns - like whether education
level differs by region, or if gender influences product preferences.

Think of it as a two-way frequency table that shows:

- How many people fall into each combination of categories

- What percentage each cell represents

- Whether there are patterns or associations

## Usage

``` r
crosstab(
  data,
  row,
  col,
  weights = NULL,
  percentages = c("row", "none", "col", "total", "all"),
  na.rm = TRUE,
  digits = 1
)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- row:

  The variable for table rows (e.g., education, age_group)

- col:

  The variable for table columns (e.g., region, gender)

- weights:

  Optional survey weights for population-representative results

- percentages:

  Which percentages to show:

  - `"row"` (default): Percentages across each row (adds to 100%
    horizontally)

  - `"col"`: Percentages down each column (adds to 100% vertically)

  - `"total"`: Percentage of the entire table

  - `"all"`: Show all three types

  - `"none"`: Just counts, no percentages

- na.rm:

  Remove missing values? (Default: TRUE)

- digits:

  Decimal places for percentages (Default: 1)

## Value

A cross-tabulation table showing the relationship between two variables

## Details

### Understanding the Output

The crosstab table shows:

- **Cell counts**: Number of people in each combination

- **Row %**: Distribution within each row (e.g., "Among those with high
  school education, X% live in the East")

- **Column %**: Distribution within each column (e.g., "Among those in
  the East, X% have high school education")

- **Total %**: Percentage of the entire sample (e.g., "X% of all
  respondents have high school education AND live in the East")

### When to Use This

Use crosstab when you want to:

- See if two categorical variables are related

- Compare distributions across groups

- Find patterns in survey responses

- Create demographic breakdowns

### Choosing Percentages

- **Row %**: Use when your row variable is the grouping factor (e.g.,
  "How does region vary BY education level?")

- **Column %**: Use when your column variable is the grouping factor
  (e.g., "How does education vary BY region?")

- **Total %**: Use to understand the overall sample composition

### Tips for Success

- Start with row or column percentages, not both at once

- Use chi-squared test to check if the relationship is statistically
  significant

- Watch for small cell counts (\< 5) which may be unreliable

- Consider combining sparse categories if many cells are empty

## See also

Other descriptive:
[`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md),
[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic crosstab
survey_data %>% crosstab(gender, region)
#> 
#> Crosstabulation: gender × region
#> ────────────────────────────────────────────────── 
#> 
#>             region 
#> gender 
#>                   East      West     Total
#> ────────────────────────────────────────── 
#> Male               238       956      1194
#>                  19.9%     80.1%    100.0%  (row %)
#> 
#> Female             247      1059      1306
#>                  18.9%     81.1%    100.0%  (row %)
#> ────────────────────────────────────────── 
#> Total              485      2015      2500
#> 
#> N = 2500

# With weights and all percentages
survey_data %>% crosstab(gender, education,
                         weights = sampling_weight,
                         percentages = "all")
#> 
#> Crosstabulation: gender × education
#> ────────────────────────────────────────────────── 
#> 
#>             education 
#> gender 
#>                      Basic Secondary  Intermediate Secondary      Academic Secondary              University                   Total
#> ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> Male                             402                     291                     326                     176                    1195
#>                                33.6%                   24.3%                   27.3%                   14.7%                  100.0%  (row %)
#>                                47.3%                   45.4%                   50.8%                   45.7%                    47.5%  (col %)
#>                                16.0%                   11.6%                   13.0%                    7.0%                    47.5%  (total %)
#> 
#> Female                           447                     350                     316                     209                    1321
#>                                33.8%                   26.5%                   23.9%                   15.8%                  100.0%  (row %)
#>                                52.7%                   54.6%                   49.2%                   54.3%                    52.5%  (col %)
#>                                17.7%                   13.9%                   12.5%                    8.3%                    52.5%  (total %)
#> ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> Total                            848                     641                     642                     385                    2516
#>                                 33.7%                    25.5%                    25.5%                    15.3%                  100.0%
#> 
#> N = 2516 (Weighted)

# Grouped analysis
survey_data %>%
  group_by(employment) %>%
  crosstab(gender, region, weights = sampling_weight)
#> 
#> ── Weighted Grouped Crosstabulation  ───────────────────────────────────────────
#> 
#> 
#> --- Group: employment = Student ---
#> 
#> Crosstabulation: gender × region
#> ────────────────────────────────────────────────── 
#> 
#>             region 
#> gender 
#>                   East      West     Total
#> ────────────────────────────────────────── 
#> Male                 7        29        36
#>                  20.0%     80.0%    100.0%  (row %)
#> 
#> Female               4        40        44
#>                  10.2%     89.8%    100.0%  (row %)
#> ────────────────────────────────────────── 
#> Total               12        68        80
#> 
#> N = 80 (Weighted)
#> 
#> 
#> --- Group: employment = Employed ---
#> 
#> Crosstabulation: gender × region
#> ────────────────────────────────────────────────── 
#> 
#>             region 
#> gender 
#>                   East      West     Total
#> ────────────────────────────────────────── 
#> Male               149       599       748
#>                  19.9%     80.1%    100.0%  (row %)
#> 
#> Female             172       683       855
#>                  20.1%     79.9%    100.0%  (row %)
#> ────────────────────────────────────────── 
#> Total              320      1282      1603
#> 
#> N = 1603 (Weighted)
#> 
#> 
#> --- Group: employment = Unemployed ---
#> 
#> Crosstabulation: gender × region
#> ────────────────────────────────────────────────── 
#> 
#>             region 
#> gender 
#>                   East      West     Total
#> ────────────────────────────────────────── 
#> Male                17        66        83
#>                  20.8%     79.2%    100.0%  (row %)
#> 
#> Female              16        85       101
#>                  15.5%     84.5%    100.0%  (row %)
#> ────────────────────────────────────────── 
#> Total               33       151       184
#> 
#> N = 184 (Weighted)
#> 
#> 
#> --- Group: employment = Retired ---
#> 
#> Crosstabulation: gender × region
#> ────────────────────────────────────────────────── 
#> 
#>             region 
#> gender 
#>                   East      West     Total
#> ────────────────────────────────────────── 
#> Male                61       199       260
#>                  23.5%     76.5%    100.0%  (row %)
#> 
#> Female              61       212       273
#>                  22.5%     77.5%    100.0%  (row %)
#> ────────────────────────────────────────── 
#> Total              123       411       534
#> 
#> N = 534 (Weighted)
#> 
#> 
#> --- Group: employment = Other ---
#> 
#> Crosstabulation: gender × region
#> ────────────────────────────────────────────────── 
#> 
#>             region 
#> gender 
#>                   East      West     Total
#> ────────────────────────────────────────── 
#> Male                15        53        68
#>                  22.2%     77.8%    100.0%  (row %)
#> 
#> Female               6        41        47
#>                  13.1%     86.9%    100.0%  (row %)
#> ────────────────────────────────────────── 
#> Total               21        94       115
#> 
#> N = 115 (Weighted)
#> 

# Column percentages only
survey_data %>% crosstab(education, employment, percentages = "col")
#> 
#> Crosstabulation: education × employment
#> ────────────────────────────────────────────────── 
#> 
#>                         employment 
#> education 
#>                              Student    Employed  Unemployed     Retired       Other       Total
#> ──────────────────────────────────────────────────────────────────────────────────────────────── 
#> Basic Secondary                    0         571          65         171          34         841
#>                                 0.0%       35.7%       35.7%       32.6%       29.6%        33.6%  (col %)
#> 
#> Intermediate Secondary             0         412          51         137          29         629
#>                                 0.0%       25.8%       28.0%       26.1%       25.2%        25.2%  (col %)
#> 
#> Academic Secondary                44         366          44         145          32         631
#>                                56.4%       22.9%       24.2%       27.6%       27.8%        25.2%  (col %)
#> 
#> University                        34         251          22          72          20         399
#>                                43.6%       15.7%       12.1%       13.7%       17.4%        16.0%  (col %)
#> ──────────────────────────────────────────────────────────────────────────────────────────────── 
#> Total                             78        1600         182         525         115        2500
#>                                  3.1%        64.0%         7.3%        21.0%         4.6%      100.0%
#> 
#> N = 2500
```
