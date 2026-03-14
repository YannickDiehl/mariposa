# Descriptive Statistics and Frequencies

``` r
library(mariposa)
library(dplyr)
data(survey_data)
```

## Overview

Before running any statistical tests, you should understand your data.
Descriptive statistics summarize what is typical, what is unusual, and
how responses are distributed.

mariposa provides four functions for data exploration:

| Function                                                                        | Best for                                            |
|---------------------------------------------------------------------------------|-----------------------------------------------------|
| [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)   | Numeric variables — means, medians, spread          |
| [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md) | Categorical variables — counts and percentages      |
| [`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)   | Relationships between two categorical variables     |
| [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)   | Full data dictionary with types, labels, and values |

All four support survey weights and grouped analysis.

## Summary Statistics with describe()

### Basic Usage

``` r
survey_data %>%
  describe(age)
#> 
#> Descriptive Statistics
#> ----------------------
#>  Variable  Mean Median     SD Range IQR Skewness    N Missing
#>       age 50.55     50 16.976    77  24    0.172 2500       0
#> ----------------------------------------
```

### Multiple Variables

``` r
survey_data %>%
  describe(age, income, life_satisfaction)
#> 
#> Descriptive Statistics
#> ----------------------
#>           Variable     Mean Median       SD Range  IQR Skewness    N Missing
#>                age   50.550     50   16.976    77   24    0.172 2500       0
#>             income 3753.934   3500 1432.802  7200 1900    0.730 2186     314
#>  life_satisfaction    3.628      4    1.153     4    2   -0.501 2421      79
#> ----------------------------------------
```

### With Survey Weights

Add `weights` for population-representative statistics:

``` r
survey_data %>%
  describe(age, income, life_satisfaction, weights = sampling_weight)
#> 
#> Weighted Descriptive Statistics
#> -------------------------------
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>                age   50.514     50   17.084    77   25    0.159      2468.8
#>             income 3743.099   3500 1423.966  7200 1900    0.724      2158.9
#>  life_satisfaction    3.625      4    1.152     4    2   -0.498      2390.9
#> ----------------------------------------
```

### Grouped Analysis

Compare statistics across subgroups:

``` r
survey_data %>%
  group_by(region) %>%
  describe(income, life_satisfaction, weights = sampling_weight)
#> 
#> Weighted Descriptive Statistics
#> -------------------------------
#> 
#> Group: region = East
#> --------------------
#> ----------------------------------------
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>             income 3760.687   3600 1388.321  7200 1700    0.718       421.9
#>  life_satisfaction    3.623      4    1.203     4    2   -0.556       457.4
#> ----------------------------------------
#> 
#> Group: region = West
#> --------------------
#> ----------------------------------------
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>             income 3738.586   3500 1433.325  7200 1900    0.726      1738.1
#>  life_satisfaction    3.625      4    1.139     4    2   -0.481      1934.8
#> ----------------------------------------
```

### Choosing Statistics

The `show` argument controls which statistics are displayed:

``` r
# Just the essentials
survey_data %>%
  describe(age, income, show = c("mean", "sd", "range"))
#> 
#> Descriptive Statistics
#> ----------------------
#>  Variable     Mean       SD Range    N Missing
#>       age   50.550   16.976    77 2500       0
#>    income 3753.934 1432.802  7200 2186     314
#> ----------------------------------------
```

``` r
# Everything available
survey_data %>%
  describe(age, show = "all")
#> 
#> Descriptive Statistics
#> ----------------------
#>  Variable  Mean Median     SD   SE Range IQR Skewness Kurtosis Variance Mode
#>       age 50.55     50 16.976 0.34    77  24    0.172   -0.364  288.185   18
#>  Q25 Q50 Q75    N Missing
#>   38  50  62 2500       0
#> ----------------------------------------
```

### Understanding the Output

- **n**: Number of valid (non-missing) cases
- **mean** ($\bar{x}$): The arithmetic average
- **sd**: Standard deviation — how spread out the values are
- **median**: The middle value (50th percentile)
- **min / max**: The range of observed values
- **skewness**: Distribution symmetry. Values near 0 indicate symmetry;
  values beyond $\pm 1$ indicate notable skew
- **kurtosis**: Tail heaviness. Values near 0 indicate a normal-like
  shape

## Frequency Tables with frequency()

### Basic Frequency

``` r
survey_data %>%
  frequency(education)
#> 
#> Frequency Analysis Results
#> --------------------------
#> 
#> education (Highest educational attainment)
#> # total N=2500 valid N=2500 mean=NA sd=NA skewness=NA
#> 
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Value |                  Label |      N |  Raw % |Valid % | Cum. % |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |        Basic Secondary |        Basic Secondary |    841 |  33.64 |  33.64 |  33.64 |
#> | Intermediate Secondary | Intermediate Secondary |    629 |  25.16 |  25.16 |  58.80 |
#> |     Academic Secondary |     Academic Secondary |    631 |  25.24 |  25.24 |  84.04 |
#> |             University |             University |    399 |  15.96 |  15.96 | 100.00 |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Total |                        |   2500 | 100.00 | 100.00 |        |
#> +------------------------+------------------------+--------+--------+--------+--------+
```

### Understanding the Output

- **Frequency**: Number of respondents in each category
- **Percent**: Percentage of all cases, including missing
- **Valid Percent**: Percentage of non-missing cases only
- **Cumulative Percent**: Running total of valid percentages

Use *Valid Percent* when missing values are truly missing (e.g., skip
patterns). Use *Percent* when non-response is itself meaningful.

### Weighted Frequencies

``` r
survey_data %>%
  frequency(education, weights = sampling_weight)
#> 
#> Weighted Frequency Analysis Results
#> -----------------------------------
#> 
#> education (Highest educational attainment)
#> # total N=2516 valid N=2516 mean=NA sd=NA skewness=NA
#> 
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Value |                  Label |      N |  Raw % |Valid % | Cum. % |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |        Basic Secondary |        Basic Secondary |    848 |  33.71 |  33.71 |  33.71 |
#> | Intermediate Secondary | Intermediate Secondary |    641 |  25.47 |  25.47 |  59.18 |
#> |     Academic Secondary |     Academic Secondary |    642 |  25.51 |  25.51 |  84.69 |
#> |             University |             University |    385 |  15.31 |  15.31 | 100.00 |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Total |                        |   2516 | 100.00 | 100.00 |        |
#> +------------------------+------------------------+--------+--------+--------+--------+
```

### Multiple Variables

``` r
survey_data %>%
  frequency(education, employment, region, weights = sampling_weight)
#> 
#> Weighted Frequency Analysis Results
#> -----------------------------------
#> 
#> education (Highest educational attainment)
#> # total N=2516 valid N=2516 mean=NA sd=NA skewness=NA
#> 
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Value |                  Label |      N |  Raw % |Valid % | Cum. % |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |        Basic Secondary |        Basic Secondary |    848 |  33.71 |  33.71 |  33.71 |
#> | Intermediate Secondary | Intermediate Secondary |    641 |  25.47 |  25.47 |  59.18 |
#> |     Academic Secondary |     Academic Secondary |    642 |  25.51 |  25.51 |  84.69 |
#> |             University |             University |    385 |  15.31 |  15.31 | 100.00 |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Total |                        |   2516 | 100.00 | 100.00 |        |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> 
#> 
#> employment (Employment status)
#> # total N=2516 valid N=2516 mean=NA sd=NA skewness=NA
#> 
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Value |                  Label |      N |  Raw % |Valid % | Cum. % |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                Student |                Student |     80 |   3.18 |   3.18 |   3.18 |
#> |               Employed |               Employed |   1603 |  63.71 |  63.71 |  66.89 |
#> |             Unemployed |             Unemployed |    184 |   7.32 |   7.32 |  74.21 |
#> |                Retired |                Retired |    534 |  21.21 |  21.21 |  95.41 |
#> |                  Other |                  Other |    115 |   4.59 |   4.59 | 100.00 |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Total |                        |   2516 | 100.00 | 100.00 |        |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> 
#> 
#> region (Region (East/West))
#> # total N=2516 valid N=2516 mean=NA sd=NA skewness=NA
#> 
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Value |                  Label |      N |  Raw % |Valid % | Cum. % |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                   East |                   East |    509 |  20.23 |  20.23 |  20.23 |
#> |                   West |                   West |   2007 |  79.77 |  79.77 | 100.00 |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Total |                        |   2516 | 100.00 | 100.00 |        |
#> +------------------------+------------------------+--------+--------+--------+--------+
```

### Grouped Frequencies

``` r
survey_data %>%
  group_by(gender) %>%
  frequency(education, weights = sampling_weight)
#> 
#> Weighted Frequency Analysis Results
#> -----------------------------------
#> 
#> education (Highest educational attainment)
#> 
#> Group: gender = Male
#> --------------------
#> # total N=1195 valid N=1195 mean=NA sd=NA skewness=NA
#> 
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Value |                  Label |      N |  Raw % |Valid % | Cum. % |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |        Basic Secondary |        Basic Secondary |    402 |  33.61 |  33.61 |  33.61 |
#> | Intermediate Secondary | Intermediate Secondary |    291 |  24.35 |  24.35 |  57.96 |
#> |     Academic Secondary |     Academic Secondary |    326 |  27.31 |  27.31 |  85.27 |
#> |             University |             University |    176 |  14.73 |  14.73 | 100.00 |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Total |                        |   1195 | 100.00 | 100.00 |        |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> 
#> 
#> Group: gender = Female
#> ----------------------
#> # total N=1321 valid N=1321 mean=NA sd=NA skewness=NA
#> 
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Value |                  Label |      N |  Raw % |Valid % | Cum. % |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |        Basic Secondary |        Basic Secondary |    447 |  33.79 |  33.79 |  33.79 |
#> | Intermediate Secondary | Intermediate Secondary |    350 |  26.49 |  26.49 |  60.28 |
#> |     Academic Secondary |     Academic Secondary |    316 |  23.89 |  23.89 |  84.17 |
#> |             University |             University |    209 |  15.83 |  15.83 | 100.00 |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Total |                        |   1321 | 100.00 | 100.00 |        |
#> +------------------------+------------------------+--------+--------+--------+--------+
```

## Cross-Tabulation with crosstab()

### Basic Crosstab

Examine the relationship between two categorical variables:

``` r
survey_data %>%
  crosstab(education, employment)
#> 
#> Crosstabulation: education × employment
#> --------------------------------------- 
#> - Row variable: education
#> - Column variable: employment
#> - Percentages: Row percentages
#> - N (valid): 2500
#> 
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> |                      |                                 employment                                  |
#> | education            |    Student |   Employed | Unemployed |    Retired |      Other |      Total |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | Basic Secondary      |          0 |        571 |         65 |        171 |         34 |        841 |
#> |   row %              |       0.0% |      67.9% |       7.7% |      20.3% |       4.0% |     100.0% |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | Intermediate Seco... |          0 |        412 |         51 |        137 |         29 |        629 |
#> |   row %              |       0.0% |      65.5% |       8.1% |      21.8% |       4.6% |     100.0% |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | Academic Secondary   |         44 |        366 |         44 |        145 |         32 |        631 |
#> |   row %              |       7.0% |      58.0% |       7.0% |      23.0% |       5.1% |     100.0% |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | University           |         34 |        251 |         22 |         72 |         20 |        399 |
#> |   row %              |       8.5% |      62.9% |       5.5% |      18.0% |       5.0% |     100.0% |
#> +======================+============+============+============+============+============+============+
#> | Total                |         78 |       1600 |        182 |        525 |        115 |       2500 |
#> +----------------------+------------+------------+------------+------------+------------+------------+
```

### Understanding the Output

- **Count**: Number of cases in each cell
- **Row %**: Percentage within each row (sums to 100% across)
- **Column %**: Percentage within each column (sums to 100% down)
- **Cell %**: Percentage of the total sample

Row percentages answer: “Of people with this education level, what
proportion has each employment status?”

Column percentages answer: “Of people with this employment status, what
proportion has each education level?”

### Weighted Crosstabs

``` r
survey_data %>%
  crosstab(education, employment, weights = sampling_weight)
#> 
#> Crosstabulation: education × employment
#> --------------------------------------- 
#> - Row variable: education
#> - Column variable: employment
#> - Percentages: Row percentages
#> - Weights variable: sampling_weight
#> - N (valid): 2516 (weighted)
#> 
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> |                      |                                 employment                                  |
#> | education            |    Student |   Employed | Unemployed |    Retired |      Other |      Total |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | Basic Secondary      |          0 |        573 |         66 |        175 |         34 |        848 |
#> |   row %              |       0.0% |      67.6% |       7.8% |      20.6% |       4.0% |     100.0% |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | Intermediate Seco... |          0 |        420 |         52 |        139 |         29 |        641 |
#> |   row %              |       0.0% |      65.6% |       8.1% |      21.7% |       4.6% |     100.0% |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | Academic Secondary   |         46 |        370 |         45 |        149 |         33 |        642 |
#> |   row %              |       7.2% |      57.6% |       7.0% |      23.1% |       5.1% |     100.0% |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | University           |         34 |        240 |         21 |         72 |         20 |        385 |
#> |   row %              |       8.7% |      62.2% |       5.4% |      18.6% |       5.1% |     100.0% |
#> +======================+============+============+============+============+============+============+
#> | Total                |         80 |       1603 |        184 |        534 |        115 |       2516 |
#> +----------------------+------------+------------+------------+------------+------------+------------+
```

### Grouped Crosstabs

``` r
survey_data %>%
  group_by(region) %>%
  crosstab(education, employment, weights = sampling_weight)
#> 
#> Weighted Grouped Crosstabulation 
#> ---------------------------------
#> 
#> 
#> Group: region = East
#> --------------------
#> 
#> Crosstabulation: education × employment
#> --------------------------------------- 
#> - Row variable: education
#> - Column variable: employment
#> - Percentages: Row percentages
#> - Weights variable: sampling_weight
#> - N (valid): 509 (weighted)
#> 
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> |                      |                                 employment                                  |
#> | education            |    Student |   Employed | Unemployed |    Retired |      Other |      Total |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | Basic Secondary      |          0 |        112 |         11 |         39 |         12 |        175 |
#> |   row %              |       0.0% |      64.1% |       6.6% |      22.4% |       6.9% |     100.0% |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | Intermediate Seco... |          0 |         83 |         13 |         29 |          4 |        129 |
#> |   row %              |       0.0% |      64.7% |       9.8% |      22.4% |       3.1% |     100.0% |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | Academic Secondary   |          6 |         72 |          8 |         35 |          1 |        123 |
#> |   row %              |       5.2% |      58.5% |       6.5% |      28.9% |       0.9% |     100.0% |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | University           |          5 |         53 |          1 |         19 |          4 |         82 |
#> |   row %              |       6.4% |      64.3% |       1.2% |      23.2% |       4.9% |     100.0% |
#> +======================+============+============+============+============+============+============+
#> | Total                |         12 |        320 |         33 |        123 |         21 |        509 |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> 
#> 
#> Group: region = West
#> --------------------
#> 
#> Crosstabulation: education × employment
#> --------------------------------------- 
#> - Row variable: education
#> - Column variable: employment
#> - Percentages: Row percentages
#> - Weights variable: sampling_weight
#> - N (valid): 2007 (weighted)
#> 
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> |                      |                                 employment                                  |
#> | education            |    Student |   Employed | Unemployed |    Retired |      Other |      Total |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | Basic Secondary      |          0 |        461 |         55 |        136 |         21 |        673 |
#> |   row %              |       0.0% |      68.5% |       8.2% |      20.1% |       3.2% |     100.0% |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | Intermediate Seco... |          0 |        337 |         40 |        110 |         26 |        512 |
#> |   row %              |       0.0% |      65.8% |       7.7% |      21.5% |       5.0% |     100.0% |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | Academic Secondary   |         40 |        298 |         37 |        113 |         31 |        519 |
#> |   row %              |       7.7% |      57.4% |       7.1% |      21.8% |       6.1% |     100.0% |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | University           |         28 |        187 |         20 |         52 |         16 |        303 |
#> |   row %              |       9.4% |      61.7% |       6.5% |      17.3% |       5.2% |     100.0% |
#> +======================+============+============+============+============+============+============+
#> | Total                |         68 |       1282 |        151 |        411 |         94 |       2007 |
#> +----------------------+------------+------------+------------+------------+------------+------------+
```

## Data Dictionary with codebook()

For a comprehensive overview of your entire dataset, use
[`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md):

``` r
codebook(survey_data)
```

This opens an interactive HTML view in the RStudio Viewer showing
variable names, types, labels, value distributions, and missing data
patterns. It is the fastest way to orient yourself in a new dataset.

## Complete Example

A typical descriptive analysis workflow:

``` r
# 1. Explore the dataset
find_var(survey_data, "trust|satisfaction")
#>   col              name                                           label
#> 1  10 life_satisfaction Life satisfaction (1=dissatisfied, 5=satisfied)
#> 2  11  trust_government        Trust in government (1=none, 5=complete)
#> 3  12       trust_media             Trust in media (1=none, 5=complete)
#> 4  13     trust_science           Trust in science (1=none, 5=complete)

# 2. Summarize numeric variables
survey_data %>%
  describe(age, income, life_satisfaction,
           weights = sampling_weight)
#> 
#> Weighted Descriptive Statistics
#> -------------------------------
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>                age   50.514     50   17.084    77   25    0.159      2468.8
#>             income 3743.099   3500 1423.966  7200 1900    0.724      2158.9
#>  life_satisfaction    3.625      4    1.152     4    2   -0.498      2390.9
#> ----------------------------------------

# 3. Check categorical distributions
survey_data %>%
  frequency(education, employment,
            weights = sampling_weight)
#> 
#> Weighted Frequency Analysis Results
#> -----------------------------------
#> 
#> education (Highest educational attainment)
#> # total N=2516 valid N=2516 mean=NA sd=NA skewness=NA
#> 
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Value |                  Label |      N |  Raw % |Valid % | Cum. % |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |        Basic Secondary |        Basic Secondary |    848 |  33.71 |  33.71 |  33.71 |
#> | Intermediate Secondary | Intermediate Secondary |    641 |  25.47 |  25.47 |  59.18 |
#> |     Academic Secondary |     Academic Secondary |    642 |  25.51 |  25.51 |  84.69 |
#> |             University |             University |    385 |  15.31 |  15.31 | 100.00 |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Total |                        |   2516 | 100.00 | 100.00 |        |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> 
#> 
#> employment (Employment status)
#> # total N=2516 valid N=2516 mean=NA sd=NA skewness=NA
#> 
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Value |                  Label |      N |  Raw % |Valid % | Cum. % |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                Student |                Student |     80 |   3.18 |   3.18 |   3.18 |
#> |               Employed |               Employed |   1603 |  63.71 |  63.71 |  66.89 |
#> |             Unemployed |             Unemployed |    184 |   7.32 |   7.32 |  74.21 |
#> |                Retired |                Retired |    534 |  21.21 |  21.21 |  95.41 |
#> |                  Other |                  Other |    115 |   4.59 |   4.59 | 100.00 |
#> +------------------------+------------------------+--------+--------+--------+--------+
#> |                  Total |                        |   2516 | 100.00 | 100.00 |        |
#> +------------------------+------------------------+--------+--------+--------+--------+

# 4. Cross-tabulate key relationships
survey_data %>%
  crosstab(education, employment,
           weights = sampling_weight)
#> 
#> Crosstabulation: education × employment
#> --------------------------------------- 
#> - Row variable: education
#> - Column variable: employment
#> - Percentages: Row percentages
#> - Weights variable: sampling_weight
#> - N (valid): 2516 (weighted)
#> 
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> |                      |                                 employment                                  |
#> | education            |    Student |   Employed | Unemployed |    Retired |      Other |      Total |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | Basic Secondary      |          0 |        573 |         66 |        175 |         34 |        848 |
#> |   row %              |       0.0% |      67.6% |       7.8% |      20.6% |       4.0% |     100.0% |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | Intermediate Seco... |          0 |        420 |         52 |        139 |         29 |        641 |
#> |   row %              |       0.0% |      65.6% |       8.1% |      21.7% |       4.6% |     100.0% |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | Academic Secondary   |         46 |        370 |         45 |        149 |         33 |        642 |
#> |   row %              |       7.2% |      57.6% |       7.0% |      23.1% |       5.1% |     100.0% |
#> +----------------------+------------+------------+------------+------------+------------+------------+
#> | University           |         34 |        240 |         21 |         72 |         20 |        385 |
#> |   row %              |       8.7% |      62.2% |       5.4% |      18.6% |       5.1% |     100.0% |
#> +======================+============+============+============+============+============+============+
#> | Total                |         80 |       1603 |        184 |        534 |        115 |       2516 |
#> +----------------------+------------+------------+------------+------------+------------+------------+

# 5. Compare across regions
survey_data %>%
  group_by(region) %>%
  describe(income, life_satisfaction,
           weights = sampling_weight)
#> 
#> Weighted Descriptive Statistics
#> -------------------------------
#> 
#> Group: region = East
#> --------------------
#> ----------------------------------------
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>             income 3760.687   3600 1388.321  7200 1700    0.718       421.9
#>  life_satisfaction    3.623      4    1.203     4    2   -0.556       457.4
#> ----------------------------------------
#> 
#> Group: region = West
#> --------------------
#> ----------------------------------------
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>             income 3738.586   3500 1433.325  7200 1900    0.726      1738.1
#>  life_satisfaction    3.625      4    1.139     4    2   -0.481      1934.8
#> ----------------------------------------
```

## Practical Tips

1.  **Start with descriptives before testing.** Understanding
    distributions, ranges, and missing data patterns prevents surprises
    in later analyses.

2.  **Check for impossible values.** Negative ages, incomes above
    plausible limits, or out-of-range Likert responses indicate data
    quality issues.

3.  **Look at skewness.** Values beyond $\pm 1$ suggest the distribution
    departs notably from normality. This affects the choice between
    parametric and non-parametric tests.

4.  **Always use weights when available.** Unweighted statistics
    describe your sample; weighted statistics estimate the population.

5.  **Consider your audience.** For technical readers, include SD,
    skewness, and kurtosis. For general audiences, focus on means,
    medians, and percentages.

## Summary

1.  **[`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)**
    summarizes numeric variables (mean, SD, median, range, skewness)
2.  **[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)**
    counts categories with percent, valid percent, and cumulative
    percent
3.  **[`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)**
    shows the joint distribution of two categorical variables
4.  **[`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)**
    provides an interactive HTML data dictionary
5.  All four functions support **survey weights** and
    **[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)**

## Next Steps

- Test for significant differences — see
  [`vignette("hypothesis-testing")`](https://YannickDiehl.github.io/mariposa/articles/hypothesis-testing.md)
- Measure relationships between variables — see
  [`vignette("correlation-analysis")`](https://YannickDiehl.github.io/mariposa/articles/correlation-analysis.md)
- Learn about survey weights — see
  [`vignette("survey-weights")`](https://YannickDiehl.github.io/mariposa/articles/survey-weights.md)
