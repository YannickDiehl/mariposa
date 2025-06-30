* SPSS Syntax for SurveyStat Package Validation
* Function: t_test
* Analysis Type: basic (unweighted, independent samples)
* Generated: 2024-12-29
* 
* Purpose: Generate reference output for R package t_test() validation
* Dataset: survey_data.sav (identical to R survey_data)
* Variables: life_satisfaction ~ gender

* ==============================================
* SPSS SYNTAX: BASIC T-TEST VALIDATION
* ==============================================

* Load the dataset
GET FILE='survey_data.sav'.

* Set output format for validation (export as TXT)
OUTPUT EXPORT
  /CONTENTS EXPORT=VISIBLE LAYERS=PRINTSETTING MODELVIEWS=PRINTSETTING
  /TXT DOCUMENTFILE='t_test_basic.txt'
  /REPLACE.

* Display dataset information for verification
FREQUENCIES VARIABLES=gender life_satisfaction
  /STATISTICS=MEAN STDDEV MIN MAX
  /ORDER=ANALYSIS.

* Independent Samples T-Test
* Compare life_satisfaction between gender groups
* This generates both equal and unequal variance results (SPSS standard)
T-TEST GROUPS=gender(1 2)
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.95).

* Additional descriptive statistics by group
MEANS TABLES=life_satisfaction BY gender
  /CELLS=MEAN COUNT STDDEV MIN MAX.

* Levene's Test for Equality of Variances (standalone)
ONEWAY life_satisfaction BY gender
  /STATISTICS DESCRIPTIVES HOMOGENEITY.

* Effect size calculation (if available in SPSS version)
* Note: Cohen's d may need to be calculated manually in older SPSS versions

* ==============================================
* END OF SYNTAX
* ==============================================

* Expected Output Sections:
* 1. Group Statistics (N, Mean, Std. Deviation, Std. Error Mean)
* 2. Levene's Test for Equality of Variances (F, Sig.)
* 3. Independent Samples Test:
*    - Equal variances assumed (t, df, Sig. 2-tailed, Mean Diff, 95% CI)
*    - Equal variances not assumed (t, df, Sig. 2-tailed, Mean Diff, 95% CI)
*
* This output will be parsed by spss_parser.R for validation against
* SurveyStat t_test(survey_data, life_satisfaction, group = gender)