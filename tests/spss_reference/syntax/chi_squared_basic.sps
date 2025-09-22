* ============================================================================
* SPSS Syntax for Testing chi_squared_test() Function
* Dataset: survey_data.sav
* Purpose: Generate reference output for validating R chi_squared_test() function
* Created: 2025-09-16
* Package: SurveyStat
* ============================================================================

* ==============================================================================
* OUTPUT MANAGEMENT SETUP
* ==============================================================================

* IMPORTANT: This file generates output ONLY for chi_squared_test() function validation
* The OMS command below starts output capture and MUST be closed with OMSEND

* Start Output Management System to save results as text

OMS
  /SELECT ALL
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/outputs/chi_squared_basic_output.txt'
  /TAG='chi_squared_validation'.

* Load the dataset

GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/data/survey_data.sav'.

ECHO '################################################################################'.
ECHO '#                  BEGIN CHI_SQUARED_TEST() FUNCTION VALIDATION                #'.
ECHO '################################################################################'.
ECHO ''.
ECHO '================================================================'.
ECHO 'SPSS OUTPUT FOR CHI_SQUARED_TEST() FUNCTION VALIDATION'.
ECHO 'Dataset: survey_data.sav'.
ECHO 'Generated: 2025-09-16'.
ECHO 'Purpose: Reference output for R SurveyStat::chi_squared_test() validation'.
ECHO '================================================================'.

* ==============================================================================
* TEST 1: BASIC UNWEIGHTED CHI-SQUARED TEST (gender x region)
* ==============================================================================

ECHO ''.
ECHO '==== TEST 1: UNWEIGHTED CHI-SQUARED TEST (gender x region) ===='.
ECHO 'Expected: Chi-square statistic, df, p-value, observed and expected frequencies'.
ECHO 'Effect sizes: Phi, Cramer V, Contingency Coefficient'.

CROSSTABS
  /TABLES=gender BY region
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI CC
  /CELLS=COUNT EXPECTED RESID.

* ==============================================================================
* TEST 2: UNWEIGHTED CHI-SQUARED TEST (gender x education)
* ==============================================================================

ECHO ''.
ECHO '==== TEST 2: UNWEIGHTED CHI-SQUARED TEST (gender x education) ===='.
ECHO 'Testing with more categories (2x4 table)'.

CROSSTABS
  /TABLES=gender BY education
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI CC
  /CELLS=COUNT EXPECTED RESID.

* ==============================================================================
* TEST 3: UNWEIGHTED CHI-SQUARED TEST (region x education)
* ==============================================================================

ECHO ''.
ECHO '==== TEST 3: UNWEIGHTED CHI-SQUARED TEST (region x education) ===='.
ECHO 'Testing 2x4 contingency table'.

CROSSTABS
  /TABLES=region BY education
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI CC
  /CELLS=COUNT EXPECTED RESID.

* ==============================================================================
* TEST 4: CHI-SQUARED WITH CONTINUITY CORRECTION (gender x region)
* ==============================================================================

ECHO ''.
ECHO '==== TEST 4: CHI-SQUARED WITH CONTINUITY CORRECTION (gender x region) ===='.
ECHO 'Yates continuity correction for 2x2 table'.

* Note: In SPSS, continuity correction is automatically applied for 2x2 tables
* and shown in output as "Continuity Correction"

CROSSTABS
  /TABLES=gender BY region
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI CC
  /CELLS=COUNT EXPECTED.

* ==============================================================================
* WEIGHTED ANALYSES
* ==============================================================================

ECHO ''.
ECHO '********** SWITCHING TO WEIGHTED ANALYSIS **********'.
ECHO 'Weight variable: sampling_weight'.

* Apply sampling weights

WEIGHT BY sampling_weight.

* ==============================================================================
* TEST 5: WEIGHTED CHI-SQUARED TEST (gender x region)
* ==============================================================================

ECHO ''.
ECHO '==== TEST 5: WEIGHTED CHI-SQUARED TEST (gender x region) ===='.
ECHO 'Compare with TEST 1 to validate weight application'.

CROSSTABS
  /TABLES=gender BY region
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI CC
  /CELLS=COUNT EXPECTED RESID.

* ==============================================================================
* TEST 6: WEIGHTED CHI-SQUARED TEST (gender x education)
* ==============================================================================

ECHO ''.
ECHO '==== TEST 6: WEIGHTED CHI-SQUARED TEST (gender x education) ===='.
ECHO 'Weighted analysis with 2x4 table'.

CROSSTABS
  /TABLES=gender BY education
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI CC
  /CELLS=COUNT EXPECTED RESID.

* ==============================================================================
* TEST 7: WEIGHTED CHI-SQUARED TEST (region x education)
* ==============================================================================

ECHO ''.
ECHO '==== TEST 7: WEIGHTED CHI-SQUARED TEST (region x education) ===='.
ECHO 'Weighted 2x4 contingency table'.

CROSSTABS
  /TABLES=region BY education
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI CC
  /CELLS=COUNT EXPECTED RESID.

* ==============================================================================
* GROUPED ANALYSES BY REGION
* ==============================================================================

* Reset weights for grouped analysis
WEIGHT OFF.

* Sort cases by grouping variable
SORT CASES BY region.

ECHO ''.
ECHO '==== TEST 8: GROUPED BY REGION - UNWEIGHTED (gender x education) ===='.
ECHO 'Split by: region (East/West)'.
ECHO 'Expected: Separate chi-squared tests for each region'.

SPLIT FILE BY region.

CROSSTABS
  /TABLES=gender BY education
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI CC
  /CELLS=COUNT EXPECTED RESID.

* ==============================================================================
* TEST 9: GROUPED BY REGION - WEIGHTED
* ==============================================================================

ECHO ''.
ECHO '==== TEST 9: GROUPED BY REGION - WEIGHTED (gender x education) ===='.
ECHO 'Split by region with sampling weights'.

WEIGHT BY sampling_weight.

CROSSTABS
  /TABLES=gender BY education
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI CC
  /CELLS=COUNT EXPECTED RESID.

* Turn off split file
SPLIT FILE OFF.

* ==============================================================================
* GROUPED ANALYSES BY GENDER
* ==============================================================================

* Reset weights
WEIGHT OFF.

* Sort cases by gender
SORT CASES BY gender.

ECHO ''.
ECHO '==== TEST 10: GROUPED BY GENDER - UNWEIGHTED (region x education) ===='.
ECHO 'Split by: gender (Male/Female)'.

SPLIT FILE BY gender.

CROSSTABS
  /TABLES=region BY education
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI CC
  /CELLS=COUNT EXPECTED RESID.

* ==============================================================================
* TEST 11: GROUPED BY GENDER - WEIGHTED
* ==============================================================================

ECHO ''.
ECHO '==== TEST 11: GROUPED BY GENDER - WEIGHTED (region x education) ===='.
ECHO 'Split by gender with sampling weights'.

WEIGHT BY sampling_weight.

CROSSTABS
  /TABLES=region BY education
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI CC
  /CELLS=COUNT EXPECTED RESID.

* Turn off split file
SPLIT FILE OFF.

* ==============================================================================
* TEST 12: LARGER TABLE (3+ CATEGORIES)
* ==============================================================================

* Reset weights
WEIGHT OFF.

ECHO ''.
ECHO '==== TEST 12: LARGER CONTINGENCY TABLE (education x employment) ===='.
ECHO 'Testing with 4x3 or larger table'.

* Create employment categories if needed
* If employment doesn't exist, you can use another categorical variable

CROSSTABS
  /TABLES=education BY employment
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI CC
  /CELLS=COUNT EXPECTED RESID.

* ==============================================================================
* TEST 13: WEIGHTED LARGER TABLE
* ==============================================================================

ECHO ''.
ECHO '==== TEST 13: WEIGHTED LARGER TABLE (education x employment) ===='.
ECHO 'Weighted analysis of larger contingency table'.

WEIGHT BY sampling_weight.

CROSSTABS
  /TABLES=education BY employment
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI CC
  /CELLS=COUNT EXPECTED RESID.

* ==============================================================================
* TEST 14: ASSOCIATION MEASURES COMPARISON
* ==============================================================================

WEIGHT OFF.

ECHO ''.
ECHO '==== TEST 14: FULL ASSOCIATION MEASURES (gender x region) ===='.
ECHO 'All available association measures for comparison'.

CROSSTABS
  /TABLES=gender BY region
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI CC LAMBDA UC BTAU CTAU GAMMA D CORR ETA
  /CELLS=COUNT EXPECTED RESID SRESID ASRESID.

* ==============================================================================
* TEST 15: MISSING DATA HANDLING
* ==============================================================================

ECHO ''.
ECHO '==== TEST 15: MISSING DATA HANDLING ===='.
ECHO 'Testing with variables that may have missing values'.

* Test with variables that might have missing values
CROSSTABS
  /TABLES=gender BY trust_government
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI CC
  /CELLS=COUNT EXPECTED
  /MISSING=LISTWISE.

* ==============================================================================
* OUTPUT NOTES
* ==============================================================================

ECHO ''.
ECHO '================================================================'.
ECHO 'NOTES ON CHI-SQUARED TEST STATISTICS:'.
ECHO '----------------------------------------------------------------'.
ECHO ''.
ECHO 'Chi-squared statistic: sum((O-E)^2/E)'.
ECHO 'Degrees of freedom: (rows-1) * (columns-1)'.
ECHO ''.
ECHO 'Effect Size Measures:'.
ECHO '  Phi coefficient: sqrt(chi-squared/N) - for 2x2 tables'.
ECHO '  Cramers V: sqrt(chi-squared/(N*min(r-1,c-1))) - for any size'.
ECHO '  Contingency coefficient: sqrt(chi-squared/(chi-squared+N))'.
ECHO ''.
ECHO 'Continuity Correction (Yates):'.
ECHO '  Applied automatically in SPSS for 2x2 tables'.
ECHO '  Reduces chi-squared value for small samples'.
ECHO ''.
ECHO 'Weighted Analysis:'.
ECHO '  Uses frequency weights for cell counts'.
ECHO '  Affects both observed and expected frequencies'.
ECHO '================================================================'.

* ==============================================================================
* END OUTPUT MANAGEMENT - CRITICAL TO PREVENT CONTAMINATION
* ==============================================================================

ECHO ''.
ECHO '================================================================'.
ECHO 'END OF CHI_SQUARED_TEST() VALIDATION OUTPUT'.
ECHO 'Total Tests: 15 (TEST 1 through TEST 15)'.
ECHO '================================================================'.
ECHO ''.
ECHO '################################################################################'.
ECHO '#                   END CHI_SQUARED_TEST() FUNCTION VALIDATION                 #'.
ECHO '################################################################################'.

* CRITICAL: Close the Output Management System to prevent output contamination
* This OMSEND command stops output capture and is ESSENTIAL to prevent
* subsequent SPSS commands from being appended to this output file

OMSEND.

EXECUTE.