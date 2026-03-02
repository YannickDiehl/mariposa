* Encoding: UTF-8.
* ============================================================================
* McNEMAR TEST - SPSS VALIDATION SYNTAX
* ============================================================================
* Dataset: survey_data.sav
* Purpose: Generate McNemar test reference values for paired dichotomous data
*
* SPSS: CROSSTABS with /STATISTICS=MCNEMAR
* or NPAR TESTS /MCNEMAR
*
* McNemar test requires two related dichotomous variables.
* We create dichotomous versions of existing variables for testing.
*
* Variable pairs tested:
*   a) trust_gov_high x trust_media_high (dichotomized trust variables)
*   b) trust_gov_high x trust_science_high
*   c) gender x region (already dichotomous, but not naturally paired -
*      used only for computational validation)
*
* Split grouping: region for grouped tests
* ============================================================================

GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/data/survey_data.sav'.

* Start Output Management System to save results as text

OMS
  /IF COMMANDS=['Crosstabs' 'NPAR Tests']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/outputs/mcnemar_test_output.txt'.

COMPUTE original_order = $CASENUM.

* ============================================================================
* VARIABLE PREPARATION
* ============================================================================
* Create dichotomous trust variables (median split: >= 4 = High, < 4 = Low).

RECODE trust_government (1 thru 3 = 0)(4 thru 7 = 1) INTO trust_gov_high.
RECODE trust_media (1 thru 3 = 0)(4 thru 7 = 1) INTO trust_media_high.
RECODE trust_science (1 thru 3 = 0)(4 thru 7 = 1) INTO trust_sci_high.

VARIABLE LABELS trust_gov_high 'Trust Government High (>=4)'.
VARIABLE LABELS trust_media_high 'Trust Media High (>=4)'.
VARIABLE LABELS trust_sci_high 'Trust Science High (>=4)'.

VALUE LABELS trust_gov_high trust_media_high trust_sci_high
  0 'Low'
  1 'High'.

EXECUTE.

* ============================================================================
* TEST 1: UNWEIGHTED / UNGROUPED
* ============================================================================

TITLE '=========== TEST 1a: trust_gov_high x trust_media_high (unweighted) ==========='.

CROSSTABS
  /TABLES=trust_gov_high BY trust_media_high
  /FORMAT=AVALUE TABLES
  /STATISTICS=MCNEMAR
  /CELLS=COUNT.

TITLE '=========== TEST 1b: trust_gov_high x trust_sci_high (unweighted) ==========='.

CROSSTABS
  /TABLES=trust_gov_high BY trust_sci_high
  /FORMAT=AVALUE TABLES
  /STATISTICS=MCNEMAR
  /CELLS=COUNT.

TITLE '=========== TEST 1c: trust_media_high x trust_sci_high (unweighted) ==========='.

CROSSTABS
  /TABLES=trust_media_high BY trust_sci_high
  /FORMAT=AVALUE TABLES
  /STATISTICS=MCNEMAR
  /CELLS=COUNT.

* Alternative via NPAR TESTS (gives same results):
NPAR TESTS
  /MCNEMAR = trust_gov_high WITH trust_media_high (PAIRED)
  /MCNEMAR = trust_gov_high WITH trust_sci_high (PAIRED)
  /MCNEMAR = trust_media_high WITH trust_sci_high (PAIRED).

* ============================================================================
* TEST 2: WEIGHTED / UNGROUPED
* ============================================================================

TITLE '=========== TEST 2: McNemar tests (weighted) ==========='.

WEIGHT BY sampling_weight.

CROSSTABS
  /TABLES=trust_gov_high BY trust_media_high
  /FORMAT=AVALUE TABLES
  /STATISTICS=MCNEMAR
  /CELLS=COUNT.

CROSSTABS
  /TABLES=trust_gov_high BY trust_sci_high
  /FORMAT=AVALUE TABLES
  /STATISTICS=MCNEMAR
  /CELLS=COUNT.

WEIGHT OFF.

* ============================================================================
* TEST 3: UNWEIGHTED / GROUPED BY REGION
* ============================================================================

TITLE '=========== TEST 3: McNemar tests, grouped by region ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

CROSSTABS
  /TABLES=trust_gov_high BY trust_media_high
  /FORMAT=AVALUE TABLES
  /STATISTICS=MCNEMAR
  /CELLS=COUNT.

CROSSTABS
  /TABLES=trust_gov_high BY trust_sci_high
  /FORMAT=AVALUE TABLES
  /STATISTICS=MCNEMAR
  /CELLS=COUNT.

* ============================================================================
* TEST 4: WEIGHTED / GROUPED BY REGION
* ============================================================================

TITLE '=========== TEST 4: McNemar tests, weighted, grouped ==========='.

WEIGHT BY sampling_weight.

CROSSTABS
  /TABLES=trust_gov_high BY trust_media_high
  /FORMAT=AVALUE TABLES
  /STATISTICS=MCNEMAR
  /CELLS=COUNT.

WEIGHT OFF.
SPLIT FILE OFF.
SORT CASES BY original_order.

OMSEND.

EXECUTE.

* ============================================================================
* NOTES:
* ============================================================================
* From SPSS McNemar output, extract:
*
* From CROSSTABS /STATISTICS=MCNEMAR:
*   - McNemar Test: Exact Sig. (2-sided) - for exact binomial test
*   - McNemar Test: N of Valid Cases
*   - The 2x2 contingency table (a, b, c, d cells)
*     where b = discordant pair type 1, c = discordant pair type 2
*   - Chi-Square (with continuity correction) = (|b-c|-1)^2 / (b+c)
*   - Chi-Square (without correction) = (b-c)^2 / (b+c)
*
* Key values:
*   - n_concordant = a + d (agree on both)
*   - n_discordant_bc = b (var1=0, var2=1)
*   - n_discordant_cb = c (var1=1, var2=0)
*   - exact_p = exact binomial test of b vs c
*   - chi_sq = (|b-c|-1)^2 / (b+c) [continuity corrected]
*   - chi_sq_p = p-value from chi-square approximation
*
* R's mcnemar.test() uses continuity correction by default.
* ============================================================================
