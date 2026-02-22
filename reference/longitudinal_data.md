# Longitudinal Study Data (Synthetic)

A synthetic repeated measures dataset suitable for testing longitudinal
analysis functions. Contains realistic treatment effects, missing data
patterns, and within-subject correlations.

## Usage

``` r
longitudinal_data
```

## Format

A data frame with 480 rows and 10 variables:

- subject_id:

  Subject identifier (factor, 1-120)

- group:

  Treatment group (Control, Treatment)

- age:

  Age in years (numeric)

- gender:

  Gender (Male, Female)

- time:

  Time point (T1, T2, T3, T4)

- time_numeric:

  Time point as numeric (1-4)

- outcome_score:

  Primary outcome measure (continuous)

- secondary_outcome:

  Secondary outcome measure (continuous)

- physio_measure:

  Physiological measure (continuous)

- questionnaire_score:

  Questionnaire rating (ordered factor, 1-7)

## Source

Generated synthetically with realistic longitudinal correlation patterns
and treatment effects. No real study data was used.

## Details

Study design features:

- 120 subjects (60 per group)

- 4 measurement occasions

- Balanced treatment assignment

- Realistic dropout patterns (0% at T1, 35% by T4)

- Treatment effect grows over time (interaction effect)

- Autoregressive error structure

- Effect size approximately Cohen's d = 0.6

Missing data patterns reflect realistic longitudinal study attrition
with higher dropout in later time points. The treatment effect
demonstrates a clear Group x Time interaction suitable for repeated
measures testing.

## Examples

``` r
# Load required packages and data
library(dplyr)
data(longitudinal_data)

# Simple example analysis with wide format data
# data(longitudinal_data_wide)
# (Repeated measures functions coming in future version)

# Descriptive statistics by group and time
longitudinal_data %>%
  group_by(group, time) %>%
  describe(outcome_score)
#> 
#> ── Descriptive Statistics ──────────────────────────────────────────────────────
#> 
#> 
#> ── Group: group = Control, time = T1 ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>       Variable   Mean Median    SD  Range   IQR Skewness  N Missing
#>  outcome_score 26.224  26.36 3.352 14.937 4.221   -0.387 60       0
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> 
#> ── Group: group = Control, time = T2 ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>       Variable   Mean Median    SD  Range   IQR Skewness  N Missing
#>  outcome_score 26.996 27.505 2.749 11.326 4.242   -0.508 51       9
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> 
#> ── Group: group = Control, time = T3 ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>       Variable  Mean Median    SD  Range   IQR Skewness  N Missing
#>  outcome_score 27.46 27.901 2.865 11.694 4.823   -0.081 49      11
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> 
#> ── Group: group = Control, time = T4 ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>       Variable   Mean Median    SD  Range   IQR Skewness  N Missing
#>  outcome_score 27.944 28.253 3.452 14.255 4.564   -0.334 41      19
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> 
#> ── Group: group = Treatment, time = T1 ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>       Variable   Mean Median    SD  Range   IQR Skewness  N Missing
#>  outcome_score 25.216 25.328 3.924 17.558 4.364   -0.098 59       1
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> 
#> ── Group: group = Treatment, time = T2 ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>       Variable   Mean Median  SD  Range   IQR Skewness  N Missing
#>  outcome_score 27.078 27.087 4.2 20.332 5.534    0.189 55       5
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> 
#> ── Group: group = Treatment, time = T3 ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>       Variable   Mean Median    SD  Range   IQR Skewness  N Missing
#>  outcome_score 29.288 29.515 3.619 15.305 5.775    0.211 48      12
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> 
#> ── Group: group = Treatment, time = T4 ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>       Variable   Mean Median    SD  Range   IQR Skewness  N Missing
#>  outcome_score 30.826 30.552 3.059 11.133 5.038     0.25 37      23
#> ────────────────────────────────────────────────────────────────────────────────
```
