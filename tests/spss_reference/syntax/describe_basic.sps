* SPSS Syntax for SurveyStat Package Validation
* Function: describe
* Analysis Type: basic (unweighted descriptive statistics)
* Generated: 2024-12-29
* 
* Purpose: Generate reference output for R package describe() validation
* Dataset: survey_data.sav (identical to R survey_data)
* Variables: life_satisfaction, trust_government, trust_media

* ==============================================
* SPSS SYNTAX: BASIC DESCRIPTIVES VALIDATION
* ==============================================

* Load the dataset
GET FILE='survey_data.sav'.

* Set output format for validation (export as TXT)
OUTPUT EXPORT
  /CONTENTS EXPORT=VISIBLE LAYERS=PRINTSETTING MODELVIEWS=PRINTSETTING
  /TXT DOCUMENTFILE='describe_basic.txt'
  /REPLACE.

* Basic Descriptive Statistics
* Equivalent to: describe(survey_data, life_satisfaction, trust_government, trust_media)
DESCRIPTIVES VARIABLES=life_satisfaction trust_government trust_media
  /STATISTICS=MEAN STDDEV MIN MAX N.

* Extended descriptive statistics for additional validation
FREQUENCIES VARIABLES=life_satisfaction trust_government trust_media
  /STATISTICS=MEAN STDDEV VARIANCE SKEWNESS KURTOSIS MIN MAX SUM
  /ORDER=ANALYSIS.

* Detailed descriptive statistics with percentiles
EXAMINE VARIABLES=life_satisfaction trust_government trust_media
  /STATISTICS DESCRIPTIVES
  /PERCENTILES(5,10,25,50,75,90,95) HAVERAGE
  /MISSING LISTWISE
  /NOTOTAL.

* Cross-tabulation for categorical understanding
CROSSTABS TABLES=gender BY region
  /CELLS=COUNT ROW COLUMN TOTAL
  /STATISTICS=CHISQ PHI CC LAMBDA.

* ==============================================
* END OF SYNTAX
* ==============================================

* Expected Output Sections:
* 1. Descriptive Statistics Table:
*    - Variable names
*    - N (valid cases)
*    - Minimum value
*    - Maximum value  
*    - Mean
*    - Std. Deviation
*
* 2. Extended Statistics (from FREQUENCIES):
*    - Mean, Std. Deviation, Variance
*    - Skewness, Kurtosis
*    - Sum, Valid N, Missing N
*
* This output will be parsed by spss_parser.R for validation against
* SurveyStat describe(survey_data, life_satisfaction, trust_government, trust_media)
*
* Key validation points:
* - Means should match exactly (±1e-6 tolerance)
* - Standard deviations should match exactly (±1e-6 tolerance)
* - Sample sizes (N) should match exactly
* - Min/Max values should match exactly