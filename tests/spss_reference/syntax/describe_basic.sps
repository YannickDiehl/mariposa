* SPSS Syntax for Testing describe() Function
* Dataset: survey_data.sav
* Purpose: Generate reference output for validating R describe() function
* Created: 2025-09-05
* Package: SurveyStat

* ==============================================================================
* OUTPUT MANAGEMENT SETUP
* ==============================================================================

* IMPORTANT: This file generates output ONLY for describe() function validation
* The OMS command below starts output capture and MUST be closed with OMSEND

* Start Output Management System to save results as text

OMS
  /SELECT ALL
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/outputs/describe_basic_output.txt'
  /TAG='describe_validation'.

* Load the dataset

GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/data/survey_data.sav'.

ECHO '################################################################################'.
ECHO '#                     BEGIN DESCRIBE() FUNCTION VALIDATION                     #'.
ECHO '################################################################################'.
ECHO ''.
ECHO '================================================================'.
ECHO 'SPSS OUTPUT FOR DESCRIBE() FUNCTION VALIDATION'.
ECHO 'Dataset: survey_data.sav'.
ECHO 'Generated: 2025-09-05'.
ECHO 'Purpose: Reference output for R SurveyStat::describe() validation'.
ECHO '================================================================'.

* ==============================================================================
* UNWEIGHTED DESCRIPTIVE STATISTICS
* ==============================================================================

ECHO ''.
ECHO '==== TEST 1: UNWEIGHTED SINGLE VARIABLE (AGE) ===='.
ECHO 'Expected: N, Mean, SD, Variance, Min, Max, Range, SE, Skewness, Kurtosis'.

* Basic descriptive statistics for numeric variables
* Testing single variable: age

DESCRIPTIVES VARIABLES=age
  /STATISTICS=MEAN STDDEV VARIANCE MIN MAX RANGE SEMEAN KURTOSIS SKEWNESS.

ECHO ''.  
ECHO '==== TEST 2: UNWEIGHTED MULTIPLE VARIABLES ===='.  
ECHO 'Variables: age, income, life_satisfaction'.

* Multiple variables: age, income, life_satisfaction

DESCRIPTIVES VARIABLES=age income life_satisfaction
  /STATISTICS=MEAN STDDEV VARIANCE MIN MAX RANGE SEMEAN KURTOSIS SKEWNESS.

ECHO ''.  
ECHO '==== TEST 3: FREQUENCIES FOR MODE AND PERCENTILES ===='.  
ECHO 'Additional statistics: Mode, Q1, Q2, Q3'.

* Frequencies for additional statistics (mode, percentiles)

FREQUENCIES VARIABLES=age income life_satisfaction
  /FORMAT=NOTABLE
  /STATISTICS=MEAN MODE STDDEV VARIANCE RANGE MINIMUM MAXIMUM SEMEAN 
             KURTOSIS SKEWNESS
  /PERCENTILES=25 50 75.

* ==============================================================================
* WEIGHTED DESCRIPTIVE STATISTICS
* ==============================================================================

ECHO ''.  
ECHO '********** SWITCHING TO WEIGHTED ANALYSIS **********'.  
ECHO 'Weight variable: sampling_weight'.

* Apply sampling weights

WEIGHT BY sampling_weight.

ECHO ''.  
ECHO '==== TEST 4: WEIGHTED SINGLE VARIABLE (AGE) ===='.  
ECHO 'Compare with unweighted to validate weight application'.

* Weighted descriptive statistics for single variable: age

DESCRIPTIVES VARIABLES=age
  /STATISTICS=MEAN STDDEV VARIANCE MIN MAX RANGE SEMEAN KURTOSIS SKEWNESS.

ECHO ''.  
ECHO '==== TEST 5: WEIGHTED MULTIPLE VARIABLES ===='.  
ECHO 'Variables: age, income, life_satisfaction with sampling weights'.

* Weighted multiple variables: age, income, life_satisfaction

DESCRIPTIVES VARIABLES=age income life_satisfaction
  /STATISTICS=MEAN STDDEV VARIANCE MIN MAX RANGE SEMEAN KURTOSIS SKEWNESS.

ECHO ''.  
ECHO '==== TEST 5B: WEIGHTED FREQUENCIES ===='.  
ECHO 'Mode and percentiles with weights'.

* Weighted frequencies for additional statistics

FREQUENCIES VARIABLES=age income life_satisfaction
  /FORMAT=NOTABLE
  /STATISTICS=MEAN MODE STDDEV VARIANCE RANGE MINIMUM MAXIMUM SEMEAN 
             KURTOSIS SKEWNESS
  /PERCENTILES=25 50 75.

* ==============================================================================
* GROUPED DESCRIPTIVE STATISTICS (BY REGION)
* ==============================================================================

* Sort cases by region for grouped analysis

SORT CASES BY region.

* Unweighted grouped statistics

WEIGHT OFF.

ECHO ''.
ECHO '==== TEST 6: GROUPED BY REGION (UNWEIGHTED) ===='.
ECHO 'Split by: region (East/West)'.
ECHO 'Expected: Statistics for each region separately'.

SPLIT FILE BY region.

DESCRIPTIVES VARIABLES=age income life_satisfaction
  /STATISTICS=MEAN STDDEV VARIANCE MIN MAX RANGE SEMEAN KURTOSIS SKEWNESS.

* Weighted grouped statistics

ECHO ''.
ECHO '==== TEST 7: GROUPED BY REGION (WEIGHTED) ===='.
ECHO 'Split by: region with sampling weights'.

WEIGHT BY sampling_weight.

DESCRIPTIVES VARIABLES=age income life_satisfaction
  /STATISTICS=MEAN STDDEV VARIANCE MIN MAX RANGE SEMEAN KURTOSIS SKEWNESS.

* Turn off split file

SPLIT FILE OFF.

* ==============================================================================
* GROUPED DESCRIPTIVE STATISTICS (BY EDUCATION)
* ==============================================================================

* Sort cases by education for grouped analysis

SORT CASES BY education.

* Unweighted grouped statistics

WEIGHT OFF.

ECHO ''.
ECHO '==== TEST 8: GROUPED BY EDUCATION (UNWEIGHTED) ===='.
ECHO 'Split by: education levels'.
ECHO 'Expected: Statistics for each education level separately'.

SPLIT FILE BY education.

DESCRIPTIVES VARIABLES=age income life_satisfaction
  /STATISTICS=MEAN STDDEV VARIANCE MIN MAX RANGE SEMEAN KURTOSIS SKEWNESS.

* Weighted grouped statistics

ECHO ''.
ECHO '==== TEST 9: GROUPED BY EDUCATION (WEIGHTED) ===='.
ECHO 'Split by: education with sampling weights'.

WEIGHT BY sampling_weight.

DESCRIPTIVES VARIABLES=age income life_satisfaction
  /STATISTICS=MEAN STDDEV VARIANCE MIN MAX RANGE SEMEAN KURTOSIS SKEWNESS.

* Turn off split file

SPLIT FILE OFF.

* ==============================================================================
* DETAILED STATISTICS FOR VALIDATION
* ==============================================================================

* Reset weight

WEIGHT OFF.

ECHO ''.
ECHO '==== TEST 10: EXAMINE FOR IQR AND DETAILED PERCENTILES (UNWEIGHTED) ===='.
ECHO 'Additional percentile statistics and IQR'.

* Examine provides comprehensive statistics including IQR

EXAMINE VARIABLES=age income life_satisfaction
  /PLOT=NONE
  /STATISTICS=DESCRIPTIVES
  /PERCENTILES(5,10,25,50,75,90,95) HAVERAGE.

* With weights

ECHO ''.
ECHO '==== TEST 11: EXAMINE FOR IQR AND DETAILED PERCENTILES (WEIGHTED) ===='.
ECHO 'Weighted percentiles and IQR'.

WEIGHT BY sampling_weight.

EXAMINE VARIABLES=age income life_satisfaction
  /PLOT=NONE
  /STATISTICS=DESCRIPTIVES
  /PERCENTILES(5,10,25,50,75,90,95) HAVERAGE.

* ==============================================================================
* MISSING DATA PATTERNS
* ==============================================================================

ECHO ''.  
ECHO '==== TEST 12: MISSING DATA PATTERNS ===='.  
ECHO 'Checking N for all variables to identify missing patterns'.

* Check missing data patterns

WEIGHT OFF.
DESCRIPTIVES VARIABLES=age income life_satisfaction trust_government trust_media trust_science
  /STATISTICS=N MEAN STDDEV MIN MAX.

* ==============================================================================
* OUTPUT NOTES
* ==============================================================================

* Expected outputs to validate:
* 1. Mean - arithmetic mean
* 2. Median - 50th percentile
* 3. Mode - most frequent value
* 4. Std Dev - standard deviation
* 5. Variance - squared standard deviation
* 6. Range - max minus min
* 7. IQR - interquartile range (Q3-Q1)
* 8. Skewness - measure of asymmetry
* 9. Kurtosis - measure of tail heaviness
* 10. Standard Error - std dev / sqrt(n)
* 11. Quantiles - 25th, 50th, 75th percentiles

* Note: SPSS uses different formulas for skewness and kurtosis than some R packages
* SPSS Skewness: g1 = sum((x - mean)^3) / (n * sd^3)
* SPSS Kurtosis: g2 = sum((x - mean)^4) / (n * sd^4) - 3 (excess kurtosis)

* For weighted statistics:
* - Effective sample size = (sum of weights)^2 / sum of weights^2
* - Weighted mean = sum(x * w) / sum(w)
* - Weighted variance with bias correction

* ==============================================================================
* END OUTPUT MANAGEMENT - CRITICAL TO PREVENT CONTAMINATION
* ==============================================================================

ECHO ''.
ECHO '================================================================'.
ECHO 'END OF DESCRIBE() VALIDATION OUTPUT'.
ECHO 'Total Tests: 12 (TEST 1 through TEST 12)'.
ECHO '================================================================'.
ECHO ''.
ECHO '################################################################################'.
ECHO '#                      END DESCRIBE() FUNCTION VALIDATION                      #'.
ECHO '################################################################################'.

* CRITICAL: Close the Output Management System to prevent output contamination
* This OMSEND command stops output capture and is ESSENTIAL to prevent
* subsequent SPSS commands from being appended to this output file

OMSEND.

EXECUTE.