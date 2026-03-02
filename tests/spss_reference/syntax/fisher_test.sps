* Encoding: UTF-8.
* ============================================================================
* FISHER'S EXACT TEST - SPSS VALIDATION SYNTAX
* ============================================================================
* Dataset: survey_data.sav
* Purpose: Generate Fisher's Exact Test reference values
*
* SPSS computes Fisher's Exact Test automatically for 2x2 tables
* in CROSSTABS when /STATISTICS=CHISQ is specified.
* For larger tables, EXACT TESTS module is needed.
*
* Variable pairs tested:
*   a) gender x region (2x2 table) - classic Fisher's Exact
*   b) gender x interview_mode (2x3 table) - generalized Fisher's Exact
*   c) Small sample subset for clear small-cell scenario
*
* Split grouping: education for grouped tests
* ============================================================================

GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/data/survey_data.sav'.

* Start Output Management System to save results as text

OMS
  /IF SUBTYPES=['Crosstabulation' 'Chi-Square Tests' 'Symmetric Measures']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/outputs/fisher_test_output.txt'.

COMPUTE original_order = $CASENUM.

* ============================================================================
* TEST 1: UNWEIGHTED / UNGROUPED
* ============================================================================

TITLE '=========== TEST 1a: gender x region (2x2, unweighted) ==========='.

CROSSTABS
  /TABLES=gender BY region
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI
  /CELLS=COUNT EXPECTED ROW COLUMN TOTAL.

* Fisher's Exact Test values appear automatically for 2x2 tables.
* Extract: Exact Sig. (2-sided), Exact Sig. (1-sided), Point Probability

TITLE '=========== TEST 1b: gender x interview_mode (2x3, unweighted) ==========='.

CROSSTABS
  /TABLES=gender BY interview_mode
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI
  /CELLS=COUNT EXPECTED ROW COLUMN TOTAL.

* For larger-than-2x2 tables, Fisher's Exact requires the EXACT module:
* CROSSTABS ... /METHOD=EXACT.
* If EXACT module is not available, extract the Chi-Square p-value
* and note that Fisher's Exact was not computed.

TITLE '=========== TEST 1c: Small sample subset ==========='.

* Create small subsample where expected frequencies < 5.
TEMPORARY.
SELECT IF (id <= 50).

CROSSTABS
  /TABLES=gender BY region
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI
  /CELLS=COUNT EXPECTED ROW COLUMN TOTAL.

* ============================================================================
* TEST 2: WEIGHTED / UNGROUPED
* ============================================================================

TITLE '=========== TEST 2a: gender x region (2x2, weighted) ==========='.

WEIGHT BY sampling_weight.

CROSSTABS
  /TABLES=gender BY region
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI
  /CELLS=COUNT EXPECTED ROW COLUMN TOTAL.

TITLE '=========== TEST 2b: gender x interview_mode (2x3, weighted) ==========='.

CROSSTABS
  /TABLES=gender BY interview_mode
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI
  /CELLS=COUNT EXPECTED ROW COLUMN TOTAL.

WEIGHT OFF.

* ============================================================================
* TEST 3: UNWEIGHTED / GROUPED
* ============================================================================

TITLE '=========== TEST 3: gender x region, grouped by education ==========='.

* Use only 2 education levels to keep output manageable.
* Note: TEMPORARY cannot precede SORT CASES (Error #5801),
* so we use permanent SELECT IF and reload the data afterwards.
SELECT IF (education = 1 OR education = 4).
EXECUTE.

SORT CASES BY education.
SPLIT FILE BY education.

CROSSTABS
  /TABLES=gender BY region
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI
  /CELLS=COUNT EXPECTED ROW COLUMN TOTAL.

SPLIT FILE OFF.

* Reload full dataset for Test 4.
GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/data/survey_data.sav'.
COMPUTE original_order = $CASENUM.
EXECUTE.

* ============================================================================
* TEST 4: WEIGHTED / GROUPED
* ============================================================================

TITLE '=========== TEST 4: gender x region, weighted, grouped by education ==========='.

SELECT IF (education = 1 OR education = 4).
EXECUTE.

WEIGHT BY sampling_weight.
SORT CASES BY education.
SPLIT FILE BY education.

CROSSTABS
  /TABLES=gender BY region
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI
  /CELLS=COUNT EXPECTED ROW COLUMN TOTAL.

WEIGHT OFF.
SPLIT FILE OFF.
SORT CASES BY original_order.

OMSEND.

EXECUTE.

* ============================================================================
* NOTES:
* ============================================================================
* From SPSS CROSSTABS output with /STATISTICS=CHISQ, extract:
*
* For 2x2 tables (Fisher's Exact provided automatically):
*   - Fisher's Exact Test: Exact Sig. (2-sided)
*   - Fisher's Exact Test: Exact Sig. (1-sided)
*   - Fisher's Exact Test: Point Probability
*   - Pearson Chi-Square (for comparison)
*   - N of Valid Cases
*   - Phi coefficient
*
* For larger tables (Fisher-Freeman-Halton):
*   - Requires /METHOD=EXACT or the Exact Tests module
*   - Fisher's Exact Test: Exact Sig. (2-sided)
*   - Likelihood Ratio Exact Sig.
*
* Crosstab cell counts are also useful for validation.
* ============================================================================
