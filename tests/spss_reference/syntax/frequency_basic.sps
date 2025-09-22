* ============================================================================
* FREQUENCY() FUNCTION SPSS VALIDATION SYNTAX
* ============================================================================
* Package: SurveyStat
* Purpose: Generate standardized reference output for R function validation
* Dataset: survey_data.sav
* Created: 2025-09-16
* Version: 2.0 (Restructured)
* ============================================================================

* =============================================================================
* OUTPUT MANAGEMENT SYSTEM
* =============================================================================
OMS
  /SELECT ALL
  /DESTINATION FORMAT=TEXT 
    OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/outputs/frequency_validation_output.txt'
  /TAG='frequency_validation'.

* =============================================================================
* DATA SETUP
* =============================================================================
GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/data/survey_data.sav'.
DATASET NAME survey_data.

* Display dataset information
ECHO '################################################################################'.
ECHO '# FREQUENCY() VALIDATION - SPSS REFERENCE OUTPUT                               #'.
ECHO '# Dataset: survey_data.sav                                                     #'.
ECHO '# Generated: 2025-09-16                                                        #'.
ECHO '################################################################################'.

* =============================================================================
* SECTION 1: BASIC UNWEIGHTED FREQUENCIES
* =============================================================================

ECHO ''.
ECHO '================================================================================'.
ECHO 'SECTION 1: BASIC UNWEIGHTED FREQUENCIES'.
ECHO '================================================================================'.

* -----------------------------------------------------------------------------
* Test 1.1: Single Binary Categorical Variable
* -----------------------------------------------------------------------------
ECHO ''.
ECHO '### TEST 1.1: BINARY CATEGORICAL (gender) ###'.
FREQUENCIES VARIABLES=gender
  /ORDER=ANALYSIS
  /STATISTICS=NONE
  /FORMAT=NOTABLE.

FREQUENCIES VARIABLES=gender
  /ORDER=ANALYSIS.

* -----------------------------------------------------------------------------
* Test 1.2: Single Multi-Category Variable
* -----------------------------------------------------------------------------
ECHO ''.
ECHO '### TEST 1.2: MULTI-CATEGORY (education) ###'.
FREQUENCIES VARIABLES=education
  /ORDER=ANALYSIS.

* -----------------------------------------------------------------------------
* Test 1.3: Multiple Variables Together
* -----------------------------------------------------------------------------
ECHO ''.
ECHO '### TEST 1.3: MULTIPLE VARIABLES ###'.
FREQUENCIES VARIABLES=gender region education
  /ORDER=ANALYSIS.

* =============================================================================
* SECTION 2: WEIGHTED FREQUENCIES
* =============================================================================

ECHO ''.
ECHO '================================================================================'.
ECHO 'SECTION 2: WEIGHTED FREQUENCIES'.
ECHO '================================================================================'.

* Apply weights
WEIGHT BY sampling_weight.

* -----------------------------------------------------------------------------
* Test 2.1: Weighted Binary Categorical
* -----------------------------------------------------------------------------
ECHO ''.
ECHO '### TEST 2.1: WEIGHTED BINARY (gender) ###'.
FREQUENCIES VARIABLES=gender
  /ORDER=ANALYSIS.

* -----------------------------------------------------------------------------
* Test 2.2: Weighted Multi-Category
* -----------------------------------------------------------------------------
ECHO ''.
ECHO '### TEST 2.2: WEIGHTED MULTI-CATEGORY (education) ###'.
FREQUENCIES VARIABLES=education
  /ORDER=ANALYSIS.

* -----------------------------------------------------------------------------
* Test 2.3: Weighted Multiple Variables
* -----------------------------------------------------------------------------
ECHO ''.
ECHO '### TEST 2.3: WEIGHTED MULTIPLE VARIABLES ###'.
FREQUENCIES VARIABLES=gender region education
  /ORDER=ANALYSIS.

* Reset weights
WEIGHT OFF.

* =============================================================================
* SECTION 3: MISSING DATA HANDLING
* =============================================================================

ECHO ''.
ECHO '================================================================================'.
ECHO 'SECTION 3: MISSING DATA HANDLING'.
ECHO '================================================================================'.

* -----------------------------------------------------------------------------
* Test 3.1: Default Missing Handling (Exclude)
* -----------------------------------------------------------------------------
ECHO ''.
ECHO '### TEST 3.1: MISSING EXCLUDED (employment) ###'.
FREQUENCIES VARIABLES=employment
  /ORDER=ANALYSIS.

* -----------------------------------------------------------------------------
* Test 3.2: Include Missing Values
* -----------------------------------------------------------------------------
ECHO ''.
ECHO '### TEST 3.2: MISSING INCLUDED (employment) ###'.
FREQUENCIES VARIABLES=employment
  /ORDER=ANALYSIS
  /MISSING=INCLUDE.

* -----------------------------------------------------------------------------
* Test 3.3: Weighted with Missing
* -----------------------------------------------------------------------------
ECHO ''.
ECHO '### TEST 3.3: WEIGHTED WITH MISSING ###'.
WEIGHT BY sampling_weight.
FREQUENCIES VARIABLES=employment
  /ORDER=ANALYSIS
  /MISSING=INCLUDE.
WEIGHT OFF.

* =============================================================================
* SECTION 4: GROUPED ANALYSIS
* =============================================================================

ECHO ''.
ECHO '================================================================================'.
ECHO 'SECTION 4: GROUPED ANALYSIS (SPLIT FILE)'.
ECHO '================================================================================'.

* Sort for split file
SORT CASES BY region.

* -----------------------------------------------------------------------------
* Test 4.1: Grouped Unweighted
* -----------------------------------------------------------------------------
ECHO ''.
ECHO '### TEST 4.1: GROUPED BY REGION - UNWEIGHTED ###'.
SPLIT FILE BY region.
FREQUENCIES VARIABLES=gender education
  /ORDER=ANALYSIS.
SPLIT FILE OFF.

* -----------------------------------------------------------------------------
* Test 4.2: Grouped Weighted
* -----------------------------------------------------------------------------
ECHO ''.
ECHO '### TEST 4.2: GROUPED BY REGION - WEIGHTED ###'.
WEIGHT BY sampling_weight.
SPLIT FILE BY region.
FREQUENCIES VARIABLES=gender education
  /ORDER=ANALYSIS.
SPLIT FILE OFF.
WEIGHT OFF.

* =============================================================================
* SECTION 5: STATISTICS OPTIONS
* =============================================================================

ECHO ''.
ECHO '================================================================================'.
ECHO 'SECTION 5: STATISTICS OPTIONS'.
ECHO '================================================================================'.

* -----------------------------------------------------------------------------
* Test 5.1: With Mode
* -----------------------------------------------------------------------------
ECHO ''.
ECHO '### TEST 5.1: WITH MODE STATISTIC ###'.
FREQUENCIES VARIABLES=education
  /ORDER=ANALYSIS
  /STATISTICS=MODE.

* -----------------------------------------------------------------------------
* Test 5.2: With Percentiles (for ordinal)
* -----------------------------------------------------------------------------
ECHO ''.
ECHO '### TEST 5.2: WITH PERCENTILES ###'.
FREQUENCIES VARIABLES=education
  /ORDER=ANALYSIS
  /STATISTICS=MODE MEDIAN
  /PERCENTILES=25 50 75.

* -----------------------------------------------------------------------------
* Test 5.3: Complete Statistics
* -----------------------------------------------------------------------------
ECHO ''.
ECHO '### TEST 5.3: ALL STATISTICS ###'.
FREQUENCIES VARIABLES=education
  /ORDER=ANALYSIS
  /STATISTICS=MODE MEDIAN MEAN STDDEV VARIANCE RANGE MINIMUM MAXIMUM
  /PERCENTILES=25 50 75.

* =============================================================================
* CLEANUP AND CLOSE
* =============================================================================

ECHO ''.
ECHO '################################################################################'.
ECHO '# END OF FREQUENCY VALIDATION OUTPUT                                           #'.
ECHO '################################################################################'.

* Close Output Management System
OMSEND.

* Execute all pending transformations
EXECUTE.