# Descriptive Statistics SPSS Reference Outputs

This directory contains SPSS DESCRIPTIVES output files for describe() function validation.

## Expected Files

### Basic Descriptives
- `describe_basic.txt` - Basic descriptive statistics
  - Variables: life_satisfaction, trust_government, trust_media
  - SPSS syntax: DESCRIPTIVES VARIABLES=life_satisfaction trust_government trust_media /STATISTICS=MEAN STDDEV MIN MAX N

### Weighted Descriptives  
- `describe_weighted.txt` - Weighted descriptive statistics
  - Variables: life_satisfaction, trust_government, trust_media
  - Weights: sampling_weight
  - SPSS syntax: WEIGHT BY sampling_weight. DESCRIPTIVES VARIABLES=life_satisfaction trust_government trust_media /STATISTICS=MEAN STDDEV MIN MAX N

### Grouped Descriptives
- `describe_grouped.txt` - Descriptives by groups
  - Variables: life_satisfaction, trust_government, trust_media
  - Groups: region
  - SPSS syntax: SPLIT FILE BY region. DESCRIPTIVES VARIABLES=life_satisfaction trust_government trust_media /STATISTICS=MEAN STDDEV MIN MAX N

## Expected SPSS Output Format

```
Descriptive Statistics
                        N  Minimum  Maximum   Mean   Std. Deviation
Life_Satisfaction      295    1.00     7.00  5.345         1.234
Trust_Government       293    1.00     7.00  4.567         1.567  
Trust_Media           294    1.00     7.00  3.789         1.345
Valid N (listwise)    290
```