# Longitudinal Study Data - Wide Format (Synthetic)

Wide format version of the longitudinal_data dataset with one row per
subject and separate columns for each time point. Useful for certain
repeated measures analyses and data visualization.

## Usage

``` r
longitudinal_data_wide
```

## Format

A data frame with 120 rows and 8 variables:

- subject_id:

  Subject identifier (factor, 1-120)

- group:

  Treatment group (Control, Treatment)

- age:

  Age in years (numeric)

- gender:

  Gender (Male, Female)

- score_T1:

  Outcome score at Time 1

- score_T2:

  Outcome score at Time 2

- score_T3:

  Outcome score at Time 3

- score_T4:

  Outcome score at Time 4

## Source

Generated synthetically from the long format longitudinal_data.

## Details

This is the wide format version of
[`longitudinal_data`](https://YannickDiehl.github.io/mariposa/reference/longitudinal_data.md)
with outcome scores spread across columns by time point. Missing values
represent subject dropout or missed assessments.

## See also

[`longitudinal_data`](https://YannickDiehl.github.io/mariposa/reference/longitudinal_data.md)
for the long format version

## Examples

``` r
# Load required packages and data
library(dplyr)
data(longitudinal_data_wide)

# Analysis of change scores
# (Repeated measures functions coming in future version)
```
