* SPSS Syntax for SurveyStat Package Validation
* Function: levene_test
* Analysis Type: basic (test of homogeneity of variance)
* Generated: 2024-12-29
* 
* Purpose: Generate reference output for R package levene_test() validation
* Dataset: survey_data.sav (identical to R survey_data)
* Variables: life_satisfaction ~ gender

* ==============================================
* SPSS SYNTAX: LEVENE TEST VALIDATION
* ==============================================

* Load the dataset
GET FILE='survey_data.sav'.

* Set output format for validation (export as TXT)
OUTPUT EXPORT
  /CONTENTS EXPORT=VISIBLE LAYERS=PRINTSETTING MODELVIEWS=PRINTSETTING
  /TXT DOCUMENTFILE='levene_test_basic.txt'
  /REPLACE.

* Display group information for verification
FREQUENCIES VARIABLES=gender
  /STATISTICS=MEAN MODE
  /ORDER=ANALYSIS.

* Primary Levene's Test for Homogeneity of Variances
* This is the main test - equivalent to:
* levene_test(survey_data, life_satisfaction, group = gender)
ONEWAY life_satisfaction BY gender
  /STATISTICS DESCRIPTIVES HOMOGENEITY.

* Alternative method using EXAMINE (for cross-validation)
EXAMINE VARIABLES=life_satisfaction BY gender
  /STATISTICS DESCRIPTIVES
  /PLOT NONE
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

* T-TEST with Levene's included (provides same Levene statistics)
T-TEST GROUPS=gender(1 2)
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.95).

* Additional robustness check - multiple variables
ONEWAY trust_government trust_media BY gender
  /STATISTICS DESCRIPTIVES HOMOGENEITY.

* ==============================================
* END OF SYNTAX
* ==============================================

* Expected Output Sections:
* 
* 1. Test of Homogeneity of Variances (from ONEWAY):
*    Variable: life_satisfaction
*    Levene Statistic: [F-value]
*    df1: [degrees of freedom numerator]
*    df2: [degrees of freedom denominator]  
*    Sig.: [p-value]
*
* 2. ANOVA Table (for context):
*    Between Groups: Sum of Squares, df, Mean Square, F, Sig.
*    Within Groups: Sum of Squares, df, Mean Square
*    Total: Sum of Squares, df
*
* 3. Descriptives by Group:
*    Gender groups with N, Mean, Std. Deviation, Std. Error, 95% CI
*
* This output will be parsed by spss_parser.R for validation against
* SurveyStat levene_test(survey_data, life_satisfaction, group = gender)
*
* Key validation points:
* - Levene F-statistic should match exactly (±1e-6 tolerance)
* - Degrees of freedom should match exactly
* - p-value should match exactly (±1e-8 tolerance)
*
* Interpretation:
* - p > 0.05: Variances are homogeneous (assumption met)
* - p ≤ 0.05: Variances are heterogeneous (assumption violated)