# Levene Test SPSS Reference Outputs

This directory contains SPSS Levene test output files for levene_test() function validation.

## Expected Files

### Basic Levene Test
- `levene_test_basic.txt` - Test of homogeneity of variance
  - Variable: life_satisfaction
  - Group: gender
  - SPSS syntax: ONEWAY life_satisfaction BY gender /STATISTICS DESCRIPTIVES HOMOGENEITY

### Weighted Levene Test  
- `levene_test_weighted.txt` - Weighted Levene test
  - Variable: life_satisfaction
  - Group: gender
  - Weights: sampling_weight
  - SPSS syntax: WEIGHT BY sampling_weight. ONEWAY life_satisfaction BY gender /STATISTICS DESCRIPTIVES HOMOGENEITY

### Multiple Group Levene Test
- `levene_test_grouped.txt` - Levene test with multiple groups
  - Variable: life_satisfaction
  - Group: education (multiple levels)
  - SPSS syntax: ONEWAY life_satisfaction BY education /STATISTICS DESCRIPTIVES HOMOGENEITY

## Expected SPSS Output Format

```
Test of Homogeneity of Variances
Life_Satisfaction
Levene Statistic    df1    df2    Sig.
      2.345          1     293    0.127

ANOVA
Life_Satisfaction
                Sum of Squares    df    Mean Square      F      Sig.
Between Groups        12.345       1       12.345    3.456   0.064
Within Groups        1234.567    293        4.214
Total               1246.912     294
```