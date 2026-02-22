# Count How Many People Chose Each Option

`frequency()` helps you understand categorical data by showing how many
people chose each option. It's perfect for survey questions with fixed
choices like education level, yes/no questions, or rating scales.

Think of it as creating a summary table that shows:

- How many people chose each option

- What percentage that represents

- Running totals to see cumulative patterns

## Usage

``` r
frequency(
  data,
  ...,
  weights = NULL,
  sort.frq = "none",
  show.na = TRUE,
  show.prc = TRUE,
  show.valid = TRUE,
  show.sum = TRUE,
  show.labels = "auto"
)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The categorical variables you want to analyze. You can list multiple
  variables separated by commas, or use helpers like
  `starts_with("trust")`

- weights:

  Optional survey weights to make results representative of your
  population. Without weights, you get sample frequencies. With weights,
  you get population estimates.

- sort.frq:

  How to order the results:

  - `"none"` (default): Keep original order

  - `"asc"`: Sort from lowest to highest frequency

  - `"desc"`: Sort from highest to lowest frequency

- show.na:

  Include missing values in the table? (Default: TRUE)

- show.prc:

  Show raw percentages including missing values? (Default: TRUE)

- show.valid:

  Show percentages excluding missing values? (Default: TRUE)

- show.sum:

  Show cumulative totals? (Default: TRUE)

- show.labels:

  Show category labels if available? (Default: "auto" - shows labels
  when they exist)

## Value

A frequency table showing counts and percentages for each category

## Details

### Understanding the Output

The frequency table shows:

- **Freq**: Number of responses in each category

- **%**: Percentage including missing values (use for "response rate")

- **Valid %**: Percentage excluding missing values (use for "among those
  who answered")

- **Cum %**: Running total percentage (helps identify cutoff points)

### When to Use This

Use `frequency()` when you have:

- Categorical variables (gender, region, education level)

- Yes/No questions

- Rating scales (satisfied/neutral/dissatisfied)

- Any question with a fixed set of options

### Weights Make a Difference

Without weights, you're describing your sample. With weights, you're
estimating population values. Always use weights for population
inference.

## See also

Other descriptive:
[`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md),
[`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic categorical analysis
survey_data %>% frequency(gender)
#> 
#> ── Frequency Analysis Results ──────────────────────────────────────────────────
#> 
#> gender (Gender)
#> # total N=2500 valid N=2500 mean=NA sd=NA skewness=NA
#> 
#> +------+--------------------+--------+--------+--------+--------+
#> |Value |Label               |N       |Raw %   |Valid % |Cum. %  |
#> +------+--------------------+--------+--------+--------+--------+
#> |Male  |Male                |1194    |47.76   |47.76   |47.76   |
#> |Female|Female              |1306    |52.24   |52.24   |100.00  |
#> +------+--------------------+--------+--------+--------+--------+
#> 

# Multiple variables with weights
survey_data %>% frequency(gender, region, weights = sampling_weight)
#> 
#> ── Weighted Frequency Analysis Results ─────────────────────────────────────────
#> 
#> gender (Gender)
#> # total N=2516 valid N=2516 mean=NA sd=NA skewness=NA
#> 
#> +------+--------------------+--------+--------+--------+--------+
#> |Value |Label               |N       |Raw %   |Valid % |Cum. %  |
#> +------+--------------------+--------+--------+--------+--------+
#> |Male  |Male                |1195    |47.48   |47.48   |47.48   |
#> |Female|Female              |1321    |52.52   |52.52   |100.00  |
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

# Grouped analysis by region
survey_data %>% 
  group_by(region) %>% 
  frequency(gender, weights = sampling_weight)
#> 
#> ── Weighted Frequency Analysis Results ─────────────────────────────────────────
#> 
#> gender (Gender)
#> 
#> 
#> ── Group: region = East ──
#> 
#> # total N=509 valid N=509 mean=NA sd=NA skewness=NA
#> 
#> +------+--------------------+--------+--------+--------+--------+
#> |Value |Label               |N       |Raw %   |Valid % |Cum. %  |
#> +------+--------------------+--------+--------+--------+--------+
#> |Male  |Male                |249     |49.01   |49.01   |49.01   |
#> |Female|Female              |260     |50.99   |50.99   |100.00  |
#> +------+--------------------+--------+--------+--------+--------+
#> 
#> 
#> 
#> ── Group: region = West ──
#> 
#> # total N=2007 valid N=2007 mean=NA sd=NA skewness=NA
#> 
#> +------+--------------------+--------+--------+--------+--------+
#> |Value |Label               |N       |Raw %   |Valid % |Cum. %  |
#> +------+--------------------+--------+--------+--------+--------+
#> |Male  |Male                |945     |47.09   |47.09   |47.09   |
#> |Female|Female              |1062    |52.91   |52.91   |100.00  |
#> +------+--------------------+--------+--------+--------+--------+
#> 

# Education levels with sorting
survey_data %>% frequency(education, sort.frq = "desc")
#> 
#> ── Frequency Analysis Results ──────────────────────────────────────────────────
#> 
#> education (Highest educational attainment)
#> # total N=2500 valid N=2500 mean=NA sd=NA skewness=NA
#> 
#> +------+--------------------+--------+--------+--------+--------+
#> |Value |Label               |N       |Raw %   |Valid % |Cum. %  |
#> +------+--------------------+--------+--------+--------+--------+
#> |Univer|University          |399     |15.96   |15.96   |100.00  |
#> |Interm|Intermediate Seconda|629     |25.16   |25.16   |58.80   |
#> |Basic |Basic Secondary     |841     |33.64   |33.64   |33.64   |
#> |Academ|Academic Secondary  |631     |25.24   |25.24   |84.04   |
#> +------+--------------------+--------+--------+--------+--------+
#> 

# Employment status with custom display options
survey_data %>% frequency(employment, weights = sampling_weight, 
                         show.na = TRUE, show.sum = TRUE)
#> 
#> ── Weighted Frequency Analysis Results ─────────────────────────────────────────
#> 
#> employment (Employment status)
#> # total N=2516 valid N=2516 mean=NA sd=NA skewness=NA
#> 
#> +------+--------------------+--------+--------+--------+--------+
#> |Value |Label               |N       |Raw %   |Valid % |Cum. %  |
#> +------+--------------------+--------+--------+--------+--------+
#> |Studen|Student             |80      |3.18    |3.18    |3.18    |
#> |Employ|Employed            |1603    |63.71   |63.71   |66.89   |
#> |Unempl|Unemployed          |184     |7.32    |7.32    |74.21   |
#> |Retire|Retired             |534     |21.21   |21.21   |95.41   |
#> |Other |Other               |115     |4.59    |4.59    |100.00  |
#> +------+--------------------+--------+--------+--------+--------+
#> 
```
