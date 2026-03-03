# Social Survey Data (Synthetic)

A synthetic dataset modeled after large-scale social surveys with
demographics, attitudes, and survey design features. Created for testing
and demonstration purposes without licensing concerns.

## Usage

``` r
survey_data
```

## Format

A data frame with 2,500 rows and 16 variables:

- id:

  Unique identifier (1-2500)

- age:

  Age in years (18-95)

- gender:

  Gender (Male, Female)

- region:

  Region (East, West)

- education:

  Education level (Basic Secondary, Intermediate Secondary, Academic
  Secondary, University)

- income:

  Monthly household income in EUR (800-15000)

- employment:

  Employment status (Student, Employed, Unemployed, Retired, Other)

- political_orientation:

  Political orientation (1=very left to 5=very right)

- environmental_concern:

  Environmental concern (1=not concerned to 5=very concerned)

- life_satisfaction:

  Life satisfaction (1=very dissatisfied to 5=very satisfied)

- trust_government:

  Trust in government (1=no trust to 5=complete trust)

- trust_media:

  Trust in media (1=no trust to 5=complete trust)

- trust_science:

  Trust in science (1=no trust to 5=complete trust)

- sampling_weight:

  Post-stratification sampling weight (0.7-1.4)

- stratum:

  Stratification variable combining region and age group

- interview_mode:

  Interview mode (Face-to-face, Telephone, Online)

## Source

Generated synthetically using realistic demographic and attitudinal
patterns. No real survey data was used.

## Details

This dataset contains realistic patterns of correlation between
variables:

- Education correlates with income and political attitudes

- Regional differences reflect East/West patterns

- Age effects on employment and attitudes

- Realistic missing data patterns (3-12% depending on sensitivity)

- Survey weights for post-stratification adjustment

The data includes proper variable labels and follows social survey
conventions for coding and structure. Generated with set.seed(2024) for
reproducibility.

## See also

[`describe`](https://YannickDiehl.github.io/mariposa/dev/reference/describe.md)
for descriptive statistics of numeric variables.

[`frequency`](https://YannickDiehl.github.io/mariposa/dev/reference/frequency.md)
for categorical frequency tables.

[`t_test`](https://YannickDiehl.github.io/mariposa/dev/reference/t_test.md)
for group comparisons.

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic descriptive statistics
survey_data %>% describe(age, income, weights = sampling_weight)
#> 
#> Weighted Descriptive Statistics
#> -------------------------------
#>  Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>       age   50.514     50   17.084    77   25    0.159      2468.8
#>    income 3743.099   3500 1423.966  7200 1900    0.724      2158.9
#> ----------------------------------------

# Frequency analysis
survey_data %>% frequency(education, region, weights = sampling_weight)
#> 
#> Weighted Frequency Analysis Results
#> -----------------------------------
#> 
#> education (Highest educational attainment)
#> # total N=2516 valid N=2516 mean=NA sd=NA skewness=NA
#> 
#> +------+--------------------+--------+--------+--------+--------+
#> |Value |Label               |N       |Raw %   |Valid % |Cum. %  |
#> +------+--------------------+--------+--------+--------+--------+
#> |Basic |Basic Secondary     |848     |33.71   |33.71   |33.71   |
#> |Interm|Intermediate Seconda|641     |25.47   |25.47   |59.18   |
#> |Academ|Academic Secondary  |642     |25.51   |25.51   |84.69   |
#> |Univer|University          |385     |15.31   |15.31   |100.00  |
#> +------+--------------------+--------+--------+--------+--------+
#> 
#> 
#> region (Region (East/West))
#> # total N=2516 valid N=2516 mean=NA sd=NA skewness=NA
#> 
#> +------+--------------------+--------+--------+--------+--------+
#> |Value |Label               |N       |Raw %   |Valid % |Cum. %  |
#> +------+--------------------+--------+--------+--------+--------+
#> |East  |East                |509     |20.23   |20.23   |20.23   |
#> |West  |West                |2007    |79.77   |79.77   |100.00  |
#> +------+--------------------+--------+--------+--------+--------+
#> 

# Group comparisons
survey_data %>%
  group_by(region) %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)
#> [region = 1]
#> t-Test: life_satisfaction by gender [Weighted]
#>   t(484.9) = 0.641, p = 0.522 , g = 0.058 (negligible), N = 488
#> [region = 2]
#> t-Test: life_satisfaction by gender [Weighted]
#>   t(1902.0) = -1.548, p = 0.122 , g = -0.070 (negligible), N = 1949
```
