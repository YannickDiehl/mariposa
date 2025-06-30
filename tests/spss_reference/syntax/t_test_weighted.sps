* SPSS Syntax for SurveyStat Package Validation
* Function: t_test
* Analysis Type: weighted (with sampling weights)
* Generated: 2024-12-29
* 
* Purpose: Generate reference output for R package t_test() weighted validation
* Dataset: survey_data.sav (identical to R survey_data)
* Variables: life_satisfaction ~ gender, weights = sampling_weight

* ==============================================
* SPSS SYNTAX: WEIGHTED T-TEST VALIDATION
* ==============================================

* Load the dataset
GET FILE='survey_data.sav'.

* Set output format for validation (export as TXT)
OUTPUT EXPORT
  /CONTENTS EXPORT=VISIBLE LAYERS=PRINTSETTING MODELVIEWS=PRINTSETTING
  /TXT DOCUMENTFILE='t_test_weighted.txt'
  /REPLACE.

* Apply sampling weights (critical for survey data)
WEIGHT BY sampling_weight.

* Verify weighting is active
DESCRIPTIVES VARIABLES=sampling_weight
  /STATISTICS=MEAN STDDEV MIN MAX SUM.

* Display weighted frequencies for verification
FREQUENCIES VARIABLES=gender life_satisfaction
  /STATISTICS=MEAN STDDEV MIN MAX
  /ORDER=ANALYSIS.

* Weighted Independent Samples T-Test
* Compare life_satisfaction between gender groups with sampling weights
T-TEST GROUPS=gender(1 2)
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.95).

* Weighted descriptive statistics by group
MEANS TABLES=life_satisfaction BY gender
  /CELLS=MEAN COUNT STDDEV MIN MAX.

* Weighted Levene's Test for Equality of Variances
ONEWAY life_satisfaction BY gender
  /STATISTICS DESCRIPTIVES HOMOGENEITY.

* Additional weighted analysis for verification
EXAMINE VARIABLES=life_satisfaction BY gender
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95.

* Turn off weighting for comparison (optional)
WEIGHT OFF.

* ==============================================
* END OF SYNTAX
* ==============================================

* Expected Output Sections (WEIGHTED):
* 1. Weighted Group Statistics (N, Mean, Std. Deviation, Std. Error Mean)
* 2. Weighted Levene's Test for Equality of Variances (F, Sig.)
* 3. Weighted Independent Samples Test:
*    - Equal variances assumed (t, df, Sig. 2-tailed, Mean Diff, 95% CI)
*    - Equal variances not assumed (t, df, Sig. 2-tailed, Mean Diff, 95% CI)
*
* Note: SPSS weighted analysis uses effective sample sizes and design-adjusted
* standard errors. Results should match SurveyStat:
* t_test(survey_data, life_satisfaction, group = gender, weights = sampling_weight)