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

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic descriptive statistics
survey_data %>% describe(age, income, weights = sampling_weight)
#> 
#> ── Weighted Descriptive Statistics ─────────────────────────────────────────────
#>  Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>       age   50.514     50   17.084    77   25    0.159      2468.8
#>    income 3743.099   3500 1423.966  7200 1900    0.724      2158.9
#> ────────────────────────────────────────────────────────────────────────────────

# Frequency analysis
survey_data %>% frequency(education, region, weights = sampling_weight)
#> 
#> ── Weighted Frequency Analysis Results ─────────────────────────────────────────
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
#> ── Weighted t-Test Results ─────────────────────────────────────────────────────
#> 
#> • Grouping variable: gender
#> • Groups compared: Male vs. Female
#> • Weights variable: sampling_weight
#> • Confidence level: 95.0%
#> • Alternative hypothesis: two.sided
#> • Null hypothesis (mu): 0.000
#> 
#> 
#> Group: region = East
#> 
#> --- life_satisfaction ---
#> 
#>   Male: mean = 3.659, n = 239.0
#>   Female: mean = 3.589, n = 249.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat      df p_value mean_diff        conf_int sig
#>    Equal variances  0.641 486.000   0.522      0.07 [-0.144, 0.284]    
#>  Unequal variances  0.641 484.867   0.522      0.07 [-0.144, 0.284]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>           Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>  life_satisfaction    0.058    0.058       0.058  negligible
#> 
#> 
#> Group: region = West
#> 
#> --- life_satisfaction ---
#> 
#>   Male: mean = 3.583, n = 911.0
#>   Female: mean = 3.663, n = 1038.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat      df p_value mean_diff        conf_int sig
#>    Equal variances -1.551 1947.00   0.121     -0.08 [-0.182, 0.021]    
#>  Unequal variances -1.548 1901.97   0.122     -0.08 [-0.182, 0.021]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>           Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>  life_satisfaction    -0.07    -0.07       -0.07  negligible
#> 
#> 
#> Signif. codes: 0 *** 0.001 ** 0.01 * 0.05
#> 
#> Effect Size Interpretation:
#> - Cohen's d: pooled standard deviation (classic)
#> - Hedges' g: bias-corrected Cohen's d (preferred)
#> - Glass' Delta: control group standard deviation only
#> - Small effect: |effect| ~ 0.2
#> - Medium effect: |effect| ~ 0.5
#> - Large effect: |effect| ~ 0.8
```
