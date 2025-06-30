# T-Test SPSS Reference Outputs

This directory contains SPSS output files for t-test validation.

## Expected Files

### Basic T-Test
- `t_test_basic.txt` - Independent samples t-test output
  - Variables: life_satisfaction ~ gender
  - Dataset: survey_data  
  - SPSS syntax: T-TEST GROUPS=gender(1 2) /VARIABLES=life_satisfaction

### Weighted T-Test  
- `t_test_weighted.txt` - Weighted independent samples t-test
  - Variables: life_satisfaction ~ gender  
  - Weights: sampling_weight
  - SPSS syntax: WEIGHT BY sampling_weight. T-TEST GROUPS=gender(1 2) /VARIABLES=life_satisfaction

### Grouped T-Test
- `t_test_grouped.txt` - T-test by groups (e.g., by region)
  - Variables: life_satisfaction ~ gender, grouped by region
  - SPSS syntax: SPLIT FILE BY region. T-TEST GROUPS=gender(1 2) /VARIABLES=life_satisfaction

## SPSS Output Format

Each TXT file should contain the complete SPSS output including:

1. **Group Statistics**: N, Mean, Std. Deviation, Std. Error Mean for each group
2. **Levene's Test**: F statistic and significance for equality of variances  
3. **Independent Samples Test**: 
   - Equal variances assumed: t, df, Sig. (2-tailed), Mean Difference, 95% CI
   - Equal variances not assumed: t, df, Sig. (2-tailed), Mean Difference, 95% CI

## Example SPSS Output Structure

```
Group Statistics
                           Gender    N    Mean  Std. Deviation  Std. Error Mean
Life_Satisfaction          Male     150  5.234         1.234           0.123
                           Female   145  5.456         1.345           0.134

Levene's Test for Equality of Variances
                                          F      Sig.
Life_Satisfaction  Equal variances assumed  2.345  0.127

Independent Samples Test
                                                  t-test for Equality of Means
                                       t     df  Sig.(2-tailed)  Mean Difference  95% CI Lower  95% CI Upper
Life_Satisfaction  Equal variances assumed    -1.234    293        0.218            -0.222        -0.567      0.123
                   Equal variances not assumed -1.235  291.234      0.217            -0.222        -0.568      0.124
```