# 📁 Test Data Files

SPSS-compatible test datasets for validation.

## Files

### survey_data.sav
- **Size:** 2,500 observations
- **Variables:** Demographics, attitudes, survey weights
- **Use:** Main validation dataset

### longitudinal_data.sav
- **Size:** 500 subjects × 3 timepoints
- **Variables:** Subject ID, time, groups, measurements
- **Use:** Repeated measures tests

## Variable Dictionary

### survey_data.sav
| Variable | Type | Description |
|----------|------|-------------|
| age | Numeric | Age in years (18-89) |
| gender | Numeric | 1=Male, 2=Female |
| region | Numeric | 1=North, 2=South, 3=East, 4=West |
| education | Numeric | 1=Low, 2=Medium, 3=High, 4=Very High |
| income | Numeric | Annual income (thousands) |
| life_satisfaction | Numeric | 1-5 scale |
| sampling_weight | Numeric | Survey weights |

### longitudinal_data.sav
| Variable | Type | Description |
|----------|------|-------------|
| subject_id | Numeric | Unique identifier |
| time | Numeric | 1, 2, 3 (timepoints) |
| group | Numeric | 1=Control, 2=Treatment |
| score | Numeric | Outcome measure |

## Quick Regeneration

If files are missing or corrupted:
```r
# From R console
library(haven)
data(survey_data, package = "SurveyStat")
data(longitudinal_data, package = "SurveyStat")

write_sav(survey_data, "survey_data.sav")
write_sav(longitudinal_data, "longitudinal_data.sav")
```

---
**Note:** These are synthetic datasets created for testing purposes.