* ============================================================================
* SPSS Syntax for Testing t_test() Function
* Dataset: survey_data.sav
* Purpose: Generate reference output for validating R t_test() function
* Created: 2025-09-16
* Package: SurveyStat
* ============================================================================

* ==============================================================================
* OUTPUT MANAGEMENT SETUP
* ==============================================================================

* IMPORTANT: This file generates output ONLY for t_test() function validation
* The OMS command below starts output capture and MUST be closed with OMSEND

* Start Output Management System to save results as text

OMS
  /SELECT ALL
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/outputs/t_test_basic_output.txt'
  /TAG='t_test_validation'.

* Load the dataset

GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/data/survey_data.sav'.

ECHO '################################################################################'.
ECHO '#                      BEGIN T_TEST() FUNCTION VALIDATION                      #'.
ECHO '################################################################################'.
ECHO ''.
ECHO '================================================================'.
ECHO 'SPSS OUTPUT FOR T_TEST() FUNCTION VALIDATION'.
ECHO 'Dataset: survey_data.sav'.
ECHO 'Generated: 2025-09-16'.
ECHO 'Purpose: Reference output for R SurveyStat::t_test() validation'.
ECHO '================================================================'.

* ==============================================================================
* SECTION 1: INDEPENDENT SAMPLES T-TESTS (UNWEIGHTED)
* ==============================================================================

ECHO ''.
ECHO '********** SECTION 1: INDEPENDENT SAMPLES T-TESTS (UNWEIGHTED) **********'.
ECHO ''.

* ==============================================================================
* TEST 1: Basic Independent Samples T-test - Single Variable
* ==============================================================================

ECHO '==== TEST 1: UNWEIGHTED INDEPENDENT T-TEST (SINGLE VARIABLE) ===='.
ECHO 'Variable: life_satisfaction'.
ECHO 'Grouping: gender (Male vs Female)'.
ECHO 'Expected: Both Equal and Unequal variance results'.

T-TEST GROUPS=gender(1 2)
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.95).

* ==============================================================================
* TEST 2: Independent Samples T-test - Multiple Variables
* ==============================================================================

ECHO ''.
ECHO '==== TEST 2: UNWEIGHTED INDEPENDENT T-TEST (MULTIPLE VARIABLES) ===='.
ECHO 'Variables: age, income, life_satisfaction'.
ECHO 'Grouping: gender (Male vs Female)'.

T-TEST GROUPS=gender(1 2)
  /VARIABLES=age income life_satisfaction
  /CRITERIA=CI(.95).

* ==============================================================================
* TEST 3: Independent Samples T-test - Different Group (Region)
* ==============================================================================

ECHO ''.
ECHO '==== TEST 3: UNWEIGHTED INDEPENDENT T-TEST (REGION GROUPS) ===='.
ECHO 'Variable: life_satisfaction'.
ECHO 'Grouping: region (East vs West)'.

T-TEST GROUPS=region(1 2)
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.95).

* ==============================================================================
* TEST 4: Independent Samples T-test - Education Groups
* ==============================================================================

ECHO ''.
ECHO '==== TEST 4: UNWEIGHTED INDEPENDENT T-TEST (EDUCATION GROUPS) ===='.
ECHO 'Variables: income, life_satisfaction'.
ECHO 'Grouping: education (comparing first two levels)'.
ECHO 'Note: Filtering to only two education levels for valid comparison'.

* Create binary education variable for testing
RECODE education (1=1) (2=2) (ELSE=SYSMIS) INTO education_binary.
EXECUTE.

T-TEST GROUPS=education_binary(1 2)
  /VARIABLES=income life_satisfaction
  /CRITERIA=CI(.95).

* ==============================================================================
* SECTION 2: ONE-SAMPLE T-TESTS (UNWEIGHTED)
* ==============================================================================

ECHO ''.
ECHO '********** SECTION 2: ONE-SAMPLE T-TESTS (UNWEIGHTED) **********'.
ECHO ''.

* ==============================================================================
* TEST 5: One-Sample T-test - Test Against Zero
* ==============================================================================

ECHO '==== TEST 5: ONE-SAMPLE T-TEST (MU=0) ===='.
ECHO 'Variable: life_satisfaction'.
ECHO 'Test value: 0'.

T-TEST
  /TESTVAL=0
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.95).

* ==============================================================================
* TEST 6: One-Sample T-test - Test Against Specific Value
* ==============================================================================

ECHO ''.
ECHO '==== TEST 6: ONE-SAMPLE T-TEST (MU=3.5) ===='.
ECHO 'Variable: life_satisfaction'.
ECHO 'Test value: 3.5 (midpoint of scale)'.

T-TEST
  /TESTVAL=3.5
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.95).

* ==============================================================================
* TEST 7: One-Sample T-test - Multiple Variables
* ==============================================================================

ECHO ''.
ECHO '==== TEST 7: ONE-SAMPLE T-TEST MULTIPLE VARIABLES (MU=50) ===='.
ECHO 'Variables: age, political_orientation'.
ECHO 'Test value: 50'.

T-TEST
  /TESTVAL=50
  /VARIABLES=age political_orientation
  /CRITERIA=CI(.95).

* ==============================================================================
* SECTION 3: WEIGHTED T-TESTS
* ==============================================================================

ECHO ''.
ECHO '********** SECTION 3: WEIGHTED T-TESTS **********'.
ECHO ''.
ECHO 'Weight variable: sampling_weight'.

* Apply sampling weights

WEIGHT BY sampling_weight.

* ==============================================================================
* TEST 8: Weighted Independent Samples T-test - Single Variable
* ==============================================================================

ECHO ''.
ECHO '==== TEST 8: WEIGHTED INDEPENDENT T-TEST (SINGLE VARIABLE) ===='.
ECHO 'Variable: life_satisfaction'.
ECHO 'Grouping: gender (Male vs Female)'.
ECHO 'Weights: sampling_weight'.

T-TEST GROUPS=gender(1 2)
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.95).

* ==============================================================================
* TEST 9: Weighted Independent Samples T-test - Multiple Variables
* ==============================================================================

ECHO ''.
ECHO '==== TEST 9: WEIGHTED INDEPENDENT T-TEST (MULTIPLE VARIABLES) ===='.
ECHO 'Variables: age, income, life_satisfaction'.
ECHO 'Grouping: gender (Male vs Female)'.
ECHO 'Weights: sampling_weight'.

T-TEST GROUPS=gender(1 2)
  /VARIABLES=age income life_satisfaction
  /CRITERIA=CI(.95).

* ==============================================================================
* TEST 10: Weighted Independent Samples T-test - Region Groups
* ==============================================================================

ECHO ''.
ECHO '==== TEST 10: WEIGHTED INDEPENDENT T-TEST (REGION GROUPS) ===='.
ECHO 'Variable: life_satisfaction'.
ECHO 'Grouping: region (East vs West)'.
ECHO 'Weights: sampling_weight'.

T-TEST GROUPS=region(1 2)
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.95).

* ==============================================================================
* TEST 11: Weighted One-Sample T-test
* ==============================================================================

ECHO ''.
ECHO '==== TEST 11: WEIGHTED ONE-SAMPLE T-TEST (MU=3.5) ===='.
ECHO 'Variable: life_satisfaction'.
ECHO 'Test value: 3.5'.
ECHO 'Weights: sampling_weight'.

T-TEST
  /TESTVAL=3.5
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.95).

* ==============================================================================
* TEST 12: Weighted One-Sample T-test - Multiple Variables
* ==============================================================================

ECHO ''.
ECHO '==== TEST 12: WEIGHTED ONE-SAMPLE T-TEST MULTIPLE VARIABLES ===='.
ECHO 'Variables: age, political_orientation'.
ECHO 'Test value: 50'.
ECHO 'Weights: sampling_weight'.

T-TEST
  /TESTVAL=50
  /VARIABLES=age political_orientation
  /CRITERIA=CI(.95).

* ==============================================================================
* SECTION 4: GROUPED ANALYSES (SPLIT FILE)
* ==============================================================================

ECHO ''.
ECHO '********** SECTION 4: GROUPED ANALYSES (SPLIT FILE) **********'.
ECHO ''.

* Reset weights for unweighted grouped analysis
WEIGHT OFF.

* ==============================================================================
* TEST 13: Grouped by Region - Unweighted
* ==============================================================================

ECHO '==== TEST 13: GROUPED T-TEST BY REGION (UNWEIGHTED) ===='.
ECHO 'Split by: region'.
ECHO 'Variables: life_satisfaction'.
ECHO 'Grouping: gender'.

SORT CASES BY region.
SPLIT FILE BY region.

T-TEST GROUPS=gender(1 2)
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.95).

SPLIT FILE OFF.

* ==============================================================================
* TEST 14: Grouped by Region - Weighted
* ==============================================================================

ECHO ''.
ECHO '==== TEST 14: GROUPED T-TEST BY REGION (WEIGHTED) ===='.
ECHO 'Split by: region'.
ECHO 'Variables: life_satisfaction'.
ECHO 'Grouping: gender'.
ECHO 'Weights: sampling_weight'.

WEIGHT BY sampling_weight.
SORT CASES BY region.
SPLIT FILE BY region.

T-TEST GROUPS=gender(1 2)
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.95).

SPLIT FILE OFF.

* ==============================================================================
* TEST 15: Grouped by Education - Unweighted
* ==============================================================================

ECHO ''.
ECHO '==== TEST 15: GROUPED T-TEST BY EDUCATION (UNWEIGHTED) ===='.
ECHO 'Split by: education'.
ECHO 'Variables: income'.
ECHO 'Grouping: gender'.

WEIGHT OFF.
SORT CASES BY education.
SPLIT FILE BY education.

T-TEST GROUPS=gender(1 2)
  /VARIABLES=income
  /CRITERIA=CI(.95).

SPLIT FILE OFF.

* ==============================================================================
* SECTION 5: CONFIDENCE INTERVALS AND ALTERNATIVE HYPOTHESES
* ==============================================================================

ECHO ''.
ECHO '********** SECTION 5: CONFIDENCE INTERVALS **********'.
ECHO ''.

WEIGHT OFF.

* ==============================================================================
* TEST 16: 90% Confidence Interval
* ==============================================================================

ECHO '==== TEST 16: T-TEST WITH 90% CONFIDENCE INTERVAL ===='.
ECHO 'Variable: life_satisfaction'.
ECHO 'Grouping: gender'.
ECHO 'Confidence level: 90%'.

T-TEST GROUPS=gender(1 2)
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.90).

* ==============================================================================
* TEST 17: 99% Confidence Interval
* ==============================================================================

ECHO ''.
ECHO '==== TEST 17: T-TEST WITH 99% CONFIDENCE INTERVAL ===='.
ECHO 'Variable: life_satisfaction'.
ECHO 'Grouping: gender'.
ECHO 'Confidence level: 99%'.

T-TEST GROUPS=gender(1 2)
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.99).

* ==============================================================================
* SECTION 6: EFFECT SIZES AND ADDITIONAL STATISTICS
* ==============================================================================

ECHO ''.
ECHO '********** SECTION 6: EFFECT SIZES **********'.
ECHO ''.
ECHO 'Note: SPSS does not directly compute Cohen\'s d, Hedges\' g, or Glass\' Delta'.
ECHO 'These will need to be calculated from group means and standard deviations'.

* ==============================================================================
* TEST 18: Detailed Group Statistics for Effect Size Calculation
* ==============================================================================

ECHO ''.
ECHO '==== TEST 18: DETAILED GROUP STATISTICS FOR EFFECT SIZES ===='.
ECHO 'Variables: age, income, life_satisfaction'.
ECHO 'Grouping: gender'.

* Get detailed descriptive statistics by group for effect size calculations
MEANS TABLES=age income life_satisfaction BY gender
  /CELLS=MEAN COUNT STDDEV VARIANCE SUM MIN MAX RANGE SEMEAN.

* Also run T-TEST for comparison
T-TEST GROUPS=gender(1 2)
  /VARIABLES=age income life_satisfaction
  /CRITERIA=CI(.95).

* ==============================================================================
* TEST 19: Weighted Group Statistics for Effect Sizes
* ==============================================================================

ECHO ''.
ECHO '==== TEST 19: WEIGHTED GROUP STATISTICS FOR EFFECT SIZES ===='.
ECHO 'Variables: age, income, life_satisfaction'.
ECHO 'Grouping: gender'.
ECHO 'Weights: sampling_weight'.

WEIGHT BY sampling_weight.

MEANS TABLES=age income life_satisfaction BY gender
  /CELLS=MEAN COUNT STDDEV VARIANCE SUM MIN MAX RANGE SEMEAN.

T-TEST GROUPS=gender(1 2)
  /VARIABLES=age income life_satisfaction
  /CRITERIA=CI(.95).

* ==============================================================================
* SECTION 7: MISSING DATA PATTERNS
* ==============================================================================

ECHO ''.
ECHO '********** SECTION 7: MISSING DATA HANDLING **********'.
ECHO ''.

WEIGHT OFF.

* ==============================================================================
* TEST 20: T-test with Variables Having Missing Data
* ==============================================================================

ECHO '==== TEST 20: T-TEST WITH MISSING DATA ===='.
ECHO 'Variables with missing: income, trust_government'.
ECHO 'Grouping: gender'.
ECHO 'Note: SPSS uses listwise deletion by default'.

T-TEST GROUPS=gender(1 2)
  /VARIABLES=income trust_government
  /MISSING=LISTWISE
  /CRITERIA=CI(.95).

* Show the sample sizes for comparison
FREQUENCIES VARIABLES=income trust_government
  /STATISTICS=N
  /FORMAT=NOTABLE.

* ==============================================================================
* LEVENE'S TEST INFORMATION
* ==============================================================================

ECHO ''.
ECHO '********** LEVENE\'S TEST FOR EQUALITY OF VARIANCES **********'.
ECHO ''.
ECHO 'Note: SPSS T-TEST automatically includes Levene\'s test'.
ECHO 'Both equal and unequal variance t-statistics are provided'.
ECHO 'This matches the R implementation which shows both assumptions'.

* ==============================================================================
* OUTPUT NOTES AND FORMULAS
* ==============================================================================

ECHO ''.
ECHO '================================================================'.
ECHO 'STATISTICAL NOTES'.
ECHO '================================================================'.
ECHO ''.
ECHO 'T-TEST Statistics Computed:'.
ECHO '1. Levene\'s Test for Equality of Variances'.
ECHO '2. T-statistic (both equal and unequal variance assumptions)'.
ECHO '3. Degrees of freedom'.
ECHO '4. Significance (2-tailed p-value)'.
ECHO '5. Mean difference'.
ECHO '6. Standard error of difference'.
ECHO '7. Confidence intervals of difference'.
ECHO ''.
ECHO 'Effect Size Formulas (to be calculated from group statistics):'.
ECHO 'Cohen\'s d = (Mean1 - Mean2) / Pooled_SD'.
ECHO 'Pooled_SD = sqrt(((n1-1)*SD1^2 + (n2-1)*SD2^2) / (n1+n2-2))'.
ECHO 'Hedges\' g = Cohen\'s d * (1 - 3/(4*(n1+n2-2)-1))'.
ECHO 'Glass\' Delta = (Mean1 - Mean2) / SD1'.
ECHO ''.
ECHO 'For weighted analyses:'.
ECHO '- Weighted means and SDs are used'.
ECHO '- Effective sample size = (sum of weights)^2 / sum(weights^2)'.
ECHO '- Degrees of freedom adjusted accordingly'.

* ==============================================================================
* END OUTPUT MANAGEMENT - CRITICAL TO PREVENT CONTAMINATION
* ==============================================================================

ECHO ''.
ECHO '================================================================'.
ECHO 'END OF T_TEST() VALIDATION OUTPUT'.
ECHO 'Total Tests: 20 (TEST 1 through TEST 20)'.
ECHO '================================================================'.
ECHO ''.
ECHO '################################################################################'.
ECHO '#                       END T_TEST() FUNCTION VALIDATION                       #'.
ECHO '################################################################################'.

* CRITICAL: Close the Output Management System to prevent output contamination
* This OMSEND command stops output capture and is ESSENTIAL

OMSEND.
EXECUTE.

* ==============================================================================
* END OF SPSS SYNTAX FOR T_TEST VALIDATION
* ==============================================================================