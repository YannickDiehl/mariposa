* Encoding: UTF-8.
* SPSS Syntax for Chi-Square Test Validation
* Dataset: survey_data.sav
* Purpose: Validate chi-squared_test() function against SPSS results

* Start Output Management System to save results as text

OMS
  /IF SUBTYPES=['Crosstabulation']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/outputs/chi_squared_output.txt'.

COMPUTE original_order = $CASENUM.

* ================================================
* TEST 1: UNWEIGHTED / UNGROUPED
* ================================================

TITLE '=========== UNWEIGHTED / UNGROUPED ==========='.

* Test 1a: Gender × Region
SUBTITLE 'Gender by Region'.
CROSSTABS
  /TABLES=gender BY region
  /FORMAT= AVALUE TABLES
  /STATISTICS=CHISQ PHI LAMBDA
  /CELLS= COUNT ROW COLUMN TOTAL.

* Test 1b: Education × Employment  
SUBTITLE 'Education by Employment'.
CROSSTABS
  /TABLES=education BY employment
  /FORMAT= AVALUE TABLES
  /STATISTICS=CHISQ PHI LAMBDA
  /CELLS= COUNT ROW COLUMN TOTAL.

* Test 1c: Gender × Education
SUBTITLE 'Gender by Education'.
CROSSTABS
  /TABLES=gender BY education
  /FORMAT= AVALUE TABLES
  /STATISTICS=CHISQ PHI LAMBDA
  /CELLS= COUNT ROW COLUMN TOTAL.

* ================================================
* TEST 2: WEIGHTED / UNGROUPED
* ================================================

TITLE '=========== WEIGHTED / UNGROUPED ==========='.

WEIGHT BY sampling_weight.

* Test 2a: Gender × Region (weighted)
SUBTITLE 'Gender by Region (weighted)'.
CROSSTABS
  /TABLES=gender BY region
  /FORMAT= AVALUE TABLES
  /STATISTICS=CHISQ PHI LAMBDA
  /CELLS= COUNT ROW COLUMN TOTAL.

* Test 2b: Education × Employment (weighted)
SUBTITLE 'Education by Employment (weighted)'.
CROSSTABS
  /TABLES=education BY employment
  /FORMAT= AVALUE TABLES
  /STATISTICS=CHISQ PHI LAMBDA
  /CELLS= COUNT ROW COLUMN TOTAL.

* Test 2c: Gender × Education (weighted)
SUBTITLE 'Gender by Education (weighted)'.
CROSSTABS
  /TABLES=gender BY education
  /FORMAT= AVALUE TABLES
  /STATISTICS=CHISQ PHI LAMBDA
  /CELLS= COUNT ROW COLUMN TOTAL.

WEIGHT OFF.

* ================================================
* TEST 3: UNWEIGHTED / GROUPED
* ================================================

TITLE '=========== UNWEIGHTED / GROUPED ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

* Test 3a: Gender × Education (grouped by region)
SUBTITLE 'Gender by Education (grouped by region)'.
CROSSTABS
  /TABLES=gender BY education
  /FORMAT= AVALUE TABLES
  /STATISTICS=CHISQ PHI LAMBDA
  /CELLS= COUNT ROW COLUMN TOTAL.

* Test 3b: Gender × Employment (grouped by region)
SUBTITLE 'Gender by Employment (grouped by region)'.
CROSSTABS
  /TABLES=gender BY employment
  /FORMAT= AVALUE TABLES
  /STATISTICS=CHISQ PHI LAMBDA
  /CELLS= COUNT ROW COLUMN TOTAL.

* ================================================
* TEST 4: WEIGHTED / GROUPED
* ================================================

TITLE '=========== WEIGHTED / GROUPED ==========='.

WEIGHT BY sampling_weight.

* Test 4a: Gender × Education (weighted, grouped by region)
SUBTITLE 'Gender by Education (weighted, grouped by region)'.
CROSSTABS
  /TABLES=gender BY education
  /FORMAT= AVALUE TABLES
  /STATISTICS=CHISQ PHI LAMBDA
  /CELLS= COUNT ROW COLUMN TOTAL.

* Test 4b: Gender × Employment (weighted, grouped by region)
SUBTITLE 'Gender by Employment (weighted, grouped by region)'.
CROSSTABS
  /TABLES=gender BY employment
  /FORMAT= AVALUE TABLES
  /STATISTICS=CHISQ PHI LAMBDA
  /CELLS= COUNT ROW COLUMN TOTAL.

WEIGHT OFF.
SPLIT FILE OFF.
SORT CASES BY original_order.

OMSEND.

EXECUTE.