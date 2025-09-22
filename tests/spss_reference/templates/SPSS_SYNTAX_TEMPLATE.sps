* ============================================================================
* SPSS Validation Syntax for [FUNCTION_NAME]() Function
* ============================================================================
* Package: SurveyStat
* Created: [DATE]
* Purpose: Generate SPSS reference output for R function validation
* ============================================================================

* CRITICAL: Open Output Management System to capture output
OMS
  /SELECT ALL
  /DESTINATION FORMAT=TEXT 
    OUTFILE='[ABSOLUTE_PATH]/[function]_output.txt'
  /TAG='validation'.

* Load test dataset
GET FILE='[ABSOLUTE_PATH]/survey_data.sav'.

* ============================================================================
* SECTION 1: BASIC UNWEIGHTED TESTS
* ============================================================================

ECHO '==== TEST 1: UNWEIGHTED SINGLE VARIABLE ===='.
* [Your SPSS command here]
* Example: DESCRIPTIVES VARIABLES=age /STATISTICS=MEAN STDDEV.

ECHO '==== TEST 2: UNWEIGHTED MULTIPLE VARIABLES ===='.
* [Your SPSS command here]

* ============================================================================
* SECTION 2: WEIGHTED TESTS
* ============================================================================

ECHO '==== TEST 3: WEIGHTED SINGLE VARIABLE ===='.
WEIGHT BY sampling_weight.
* [Your SPSS command here]
WEIGHT OFF.

ECHO '==== TEST 4: WEIGHTED MULTIPLE VARIABLES ===='.
WEIGHT BY sampling_weight.
* [Your SPSS command here]
WEIGHT OFF.

* ============================================================================
* SECTION 3: GROUPED ANALYSIS
* ============================================================================

ECHO '==== TEST 5: GROUPED BY REGION (UNWEIGHTED) ===='.
SORT CASES BY region.
SPLIT FILE BY region.
* [Your SPSS command here]
SPLIT FILE OFF.

ECHO '==== TEST 6: GROUPED BY REGION (WEIGHTED) ===='.
WEIGHT BY sampling_weight.
SORT CASES BY region.
SPLIT FILE BY region.
* [Your SPSS command here]
SPLIT FILE OFF.
WEIGHT OFF.

* ============================================================================
* CLEANUP
* ============================================================================

ECHO '================================================================================'.
ECHO 'Total Tests Completed: 6'.
ECHO '================================================================================'.

* CRITICAL: Close Output Management System
OMSEND.
EXECUTE.