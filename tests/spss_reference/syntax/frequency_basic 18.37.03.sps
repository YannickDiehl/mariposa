* SPSS Syntax for Testing frequency() Function
* Dataset: survey_data.sav
* Purpose: Generate reference output for validating R frequency() function
* Created: 2025-09-16
* Package: SurveyStat

* ==============================================================================
* OUTPUT MANAGEMENT SETUP
* ==============================================================================

* IMPORTANT: This file generates output ONLY for frequency() function validation
* The OMS command below starts output capture and MUST be closed with OMSEND

* Start Output Management System to save results as text

OMS
  /SELECT ALL
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/outputs/frequency_basic_output.txt'
  /TAG='frequency_validation'.

* Load the dataset

GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/data/survey_data.sav'.

ECHO '################################################################################'.
ECHO '#                    BEGIN FREQUENCY() FUNCTION VALIDATION                     #'.
ECHO '################################################################################'.
ECHO ''.
ECHO '================================================================'.
ECHO 'SPSS OUTPUT FOR FREQUENCY() FUNCTION VALIDATION'.
ECHO 'Dataset: survey_data.sav'.
ECHO 'Generated: 2025-09-16'.
ECHO 'Purpose: Reference output for R SurveyStat::frequency() validation'.
ECHO '================================================================'.

* ==============================================================================
* UNWEIGHTED FREQUENCY DISTRIBUTIONS
* ==============================================================================

ECHO ''.
ECHO '==== TEST 1: UNWEIGHTED SINGLE CATEGORICAL VARIABLE (gender) ===='.
ECHO 'Expected: Frequency, Percent, Valid Percent, Cumulative Percent'.

* Basic frequency table for single categorical variable

FREQUENCIES VARIABLES=gender
  /ORDER=ANALYSIS
  /STATISTICS=MODE.

ECHO ''.  
ECHO '==== TEST 2: UNWEIGHTED SINGLE CATEGORICAL VARIABLE (region) ===='.
ECHO 'Testing different category counts'.

* Single categorical variable with different categories

FREQUENCIES VARIABLES=region
  /ORDER=ANALYSIS
  /STATISTICS=MODE.

ECHO ''.  
ECHO '==== TEST 3: UNWEIGHTED SINGLE ORDINAL VARIABLE (education) ===='.
ECHO 'Testing ordinal variable with natural ordering'.

* Ordinal variable - education levels

FREQUENCIES VARIABLES=education
  /ORDER=ANALYSIS
  /STATISTICS=MODE MEDIAN.

ECHO ''.  
ECHO '==== TEST 4: UNWEIGHTED MULTIPLE VARIABLES ===='.  
ECHO 'Variables: gender, region, education together'.

* Multiple variables in one command

FREQUENCIES VARIABLES=gender region education
  /ORDER=ANALYSIS
  /STATISTICS=MODE.

ECHO ''.  
ECHO '==== TEST 5: UNWEIGHTED WITH MISSING DATA (employment) ===='.  
ECHO 'Testing handling of missing values'.

* Variable with missing data

FREQUENCIES VARIABLES=employment
  /ORDER=ANALYSIS
  /STATISTICS=MODE
  /MISSING=INCLUDE.

* ==============================================================================
* WEIGHTED FREQUENCY DISTRIBUTIONS
* ==============================================================================

ECHO ''.  
ECHO '********** SWITCHING TO WEIGHTED ANALYSIS **********'.  
ECHO 'Weight variable: sampling_weight'.

* Apply sampling weights

WEIGHT BY sampling_weight.

ECHO ''.  
ECHO '==== TEST 6: WEIGHTED SINGLE CATEGORICAL VARIABLE (gender) ===='.  
ECHO 'Compare with unweighted to validate weight application'.

* Weighted frequency for gender

FREQUENCIES VARIABLES=gender
  /ORDER=ANALYSIS
  /STATISTICS=MODE.

ECHO ''.  
ECHO '==== TEST 7: WEIGHTED SINGLE CATEGORICAL VARIABLE (region) ===='.
ECHO 'Weighted distribution by region'.

* Weighted frequency for region

FREQUENCIES VARIABLES=region
  /ORDER=ANALYSIS
  /STATISTICS=MODE.

ECHO ''.  
ECHO '==== TEST 8: WEIGHTED ORDINAL VARIABLE (education) ===='.
ECHO 'Weighted education distribution'.

* Weighted frequency for education

FREQUENCIES VARIABLES=education
  /ORDER=ANALYSIS
  /STATISTICS=MODE MEDIAN.

ECHO ''.  
ECHO '==== TEST 9: WEIGHTED MULTIPLE VARIABLES ===='.
ECHO 'Multiple variables with weights'.

* Weighted multiple variables

FREQUENCIES VARIABLES=gender region education employment
  /ORDER=ANALYSIS
  /STATISTICS=MODE.

ECHO ''.  
ECHO '==== TEST 10: WEIGHTED WITH MISSING DATA (employment) ===='.
ECHO 'Weighted analysis including missing values'.

* Weighted with missing values included

FREQUENCIES VARIABLES=employment
  /ORDER=ANALYSIS
  /STATISTICS=MODE
  /MISSING=INCLUDE.

* ==============================================================================
* SORTED FREQUENCY DISTRIBUTIONS
* ==============================================================================

* Reset weight for clarity

WEIGHT OFF.

ECHO ''.
ECHO '==== TEST 11: SORTED ASCENDING BY VALUE (education) ===='.
ECHO 'Sort order: Ascending by variable value'.

* Sort ascending by value

FREQUENCIES VARIABLES=education
  /ORDER=VARIABLE
  /STATISTICS=MODE.

ECHO ''.
ECHO '==== TEST 12: SORTED DESCENDING BY FREQUENCY (region) ===='.
ECHO 'Sort order: Descending by frequency count'.

* Sort descending by frequency

FREQUENCIES VARIABLES=region
  /ORDER=FREQ
  /STATISTICS=MODE.

* ==============================================================================
* GROUPED FREQUENCY DISTRIBUTIONS (BY REGION)
* ==============================================================================

* Sort cases by region for grouped analysis

SORT CASES BY region.

* Unweighted grouped frequencies

WEIGHT OFF.

ECHO ''.
ECHO '==== TEST 13: GROUPED BY REGION - GENDER (UNWEIGHTED) ===='.
ECHO 'Split by: region (East/West)'.
ECHO 'Expected: Gender frequencies for each region separately'.

SPLIT FILE BY region.

FREQUENCIES VARIABLES=gender
  /ORDER=ANALYSIS
  /STATISTICS=MODE.

ECHO ''.
ECHO '==== TEST 14: GROUPED BY REGION - EDUCATION (UNWEIGHTED) ===='.
ECHO 'Education distribution by region'.

FREQUENCIES VARIABLES=education
  /ORDER=ANALYSIS
  /STATISTICS=MODE MEDIAN.

* Weighted grouped frequencies

ECHO ''.
ECHO '==== TEST 15: GROUPED BY REGION - GENDER (WEIGHTED) ===='.
ECHO 'Split by: region with sampling weights'.

WEIGHT BY sampling_weight.

FREQUENCIES VARIABLES=gender
  /ORDER=ANALYSIS
  /STATISTICS=MODE.

ECHO ''.
ECHO '==== TEST 16: GROUPED BY REGION - EDUCATION (WEIGHTED) ===='.
ECHO 'Weighted education by region'.

FREQUENCIES VARIABLES=education
  /ORDER=ANALYSIS
  /STATISTICS=MODE MEDIAN.

* Turn off split file

SPLIT FILE OFF.

* ==============================================================================
* GROUPED FREQUENCY DISTRIBUTIONS (BY GENDER)
* ==============================================================================

* Sort cases by gender for grouped analysis

SORT CASES BY gender.

* Unweighted grouped frequencies

WEIGHT OFF.

ECHO ''.
ECHO '==== TEST 17: GROUPED BY GENDER - EDUCATION (UNWEIGHTED) ===='.
ECHO 'Split by: gender (Male/Female)'.
ECHO 'Expected: Education frequencies for each gender separately'.

SPLIT FILE BY gender.

FREQUENCIES VARIABLES=education
  /ORDER=ANALYSIS
  /STATISTICS=MODE MEDIAN.

ECHO ''.
ECHO '==== TEST 18: GROUPED BY GENDER - EMPLOYMENT (UNWEIGHTED) ===='.
ECHO 'Employment distribution by gender'.

FREQUENCIES VARIABLES=employment
  /ORDER=ANALYSIS
  /STATISTICS=MODE
  /MISSING=INCLUDE.

* Weighted grouped frequencies

ECHO ''.
ECHO '==== TEST 19: GROUPED BY GENDER - EDUCATION (WEIGHTED) ===='.
ECHO 'Split by: gender with sampling weights'.

WEIGHT BY sampling_weight.

FREQUENCIES VARIABLES=education
  /ORDER=ANALYSIS
  /STATISTICS=MODE MEDIAN.

ECHO ''.
ECHO '==== TEST 20: GROUPED BY GENDER - EMPLOYMENT (WEIGHTED) ===='.
ECHO 'Weighted employment by gender'.

FREQUENCIES VARIABLES=employment
  /ORDER=ANALYSIS
  /STATISTICS=MODE
  /MISSING=INCLUDE.

* Turn off split file

SPLIT FILE OFF.

* ==============================================================================
* CROSSTABS FOR VALIDATION OF GROUPED FREQUENCIES
* ==============================================================================

WEIGHT OFF.

ECHO ''.
ECHO '==== TEST 21: CROSSTAB GENDER BY REGION (UNWEIGHTED) ===='.
ECHO 'Cross-tabulation for validation'.

* Crosstab as alternative validation

CROSSTABS
  /TABLES=gender BY region
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT COLUMN ROW TOTAL.

ECHO ''.
ECHO '==== TEST 22: CROSSTAB GENDER BY REGION (WEIGHTED) ===='.
ECHO 'Weighted cross-tabulation'.

WEIGHT BY sampling_weight.

CROSSTABS
  /TABLES=gender BY region
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT COLUMN ROW TOTAL.

ECHO ''.
ECHO '==== TEST 23: CROSSTAB EDUCATION BY GENDER (WEIGHTED) ===='.
ECHO 'Education by gender weighted'.

CROSSTABS
  /TABLES=education BY gender
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT COLUMN ROW TOTAL.

* ==============================================================================
* SPECIAL CASES AND EDGE CONDITIONS
* ==============================================================================

WEIGHT OFF.

ECHO ''.
ECHO '==== TEST 24: VARIABLE WITH MANY CATEGORIES (age_group) ===='.
ECHO 'Testing performance with multiple categories'.

* Create age groups for testing
RECODE age 
  (18 thru 25=1) 
  (26 thru 35=2) 
  (36 thru 45=3) 
  (46 thru 55=4) 
  (56 thru 65=5) 
  (66 thru 75=6) 
  (76 thru 100=7) 
  INTO age_group.
VARIABLE LABELS age_group 'Age Groups'.
VALUE LABELS age_group 
  1 '18-25' 
  2 '26-35' 
  3 '36-45' 
  4 '46-55' 
  5 '56-65' 
  6 '66-75' 
  7 '76+'.
EXECUTE.

FREQUENCIES VARIABLES=age_group
  /ORDER=ANALYSIS
  /STATISTICS=MODE MEDIAN
  /PERCENTILES=25 50 75.

ECHO ''.
ECHO '==== TEST 25: WEIGHTED AGE GROUPS ===='.
ECHO 'Age group distribution with weights'.

WEIGHT BY sampling_weight.

FREQUENCIES VARIABLES=age_group
  /ORDER=ANALYSIS
  /STATISTICS=MODE MEDIAN
  /PERCENTILES=25 50 75.

* ==============================================================================
* STATISTICAL SUMMARY FOR CATEGORICAL VARIABLES
* ==============================================================================

WEIGHT OFF.

ECHO ''.
ECHO '==== TEST 26: SUMMARY STATISTICS FOR CATEGORICAL (UNWEIGHTED) ===='.
ECHO 'N, Missing, Mode for all categorical variables'.

* Summary statistics for all categorical variables

FREQUENCIES VARIABLES=gender region education employment age_group
  /ORDER=ANALYSIS
  /STATISTICS=MODE
  /MISSING=INCLUDE.

ECHO ''.
ECHO '==== TEST 27: SUMMARY STATISTICS FOR CATEGORICAL (WEIGHTED) ===='.
ECHO 'Weighted summary statistics'.

WEIGHT BY sampling_weight.

FREQUENCIES VARIABLES=gender region education employment age_group
  /ORDER=ANALYSIS
  /STATISTICS=MODE
  /MISSING=INCLUDE.

* ==============================================================================
* OUTPUT NOTES
* ==============================================================================

* Expected outputs to validate:
* 1. Frequency - Raw count for each category
* 2. Percent - Percentage including missing values
* 3. Valid Percent - Percentage excluding missing values
* 4. Cumulative Percent - Running total of valid percentages
* 5. Mode - Most frequent category
* 6. N Valid - Count of non-missing cases
* 7. N Missing - Count of missing cases
* 8. Total N - Total sample size

* For weighted frequencies:
* - Weighted counts (sum of weights for each category)
* - Weighted percentages
* - Effective sample size = (sum of weights)^2 / sum of weights^2

* Note on sorting:
* - ORDER=ANALYSIS: Original order (as in data)
* - ORDER=VARIABLE: Sort by variable value (ascending)
* - ORDER=FREQ: Sort by frequency (descending)

* For grouped analysis:
* - Each group produces separate frequency table
* - Group totals should sum to overall totals

* ==============================================================================
* END OUTPUT MANAGEMENT - CRITICAL TO PREVENT CONTAMINATION
* ==============================================================================

ECHO ''.
ECHO '================================================================'.
ECHO 'END OF FREQUENCY() VALIDATION OUTPUT'.
ECHO 'Total Tests: 27 (TEST 1 through TEST 27)'.
ECHO '================================================================'.
ECHO ''.
ECHO '################################################################################'.
ECHO '#                     END FREQUENCY() FUNCTION VALIDATION                      #'.
ECHO '################################################################################'.

* CRITICAL: Close the Output Management System to prevent output contamination
* This OMSEND command stops output capture and is ESSENTIAL to prevent
* subsequent SPSS commands from being appended to this output file

OMSEND.

EXECUTE.