* Encoding: UTF-8.
* ============================================================================
* CHI-SQUARE GOODNESS-OF-FIT - SPSS VALIDATION SYNTAX
* ============================================================================
* Dataset: survey_data.sav
* Purpose: Generate Chi-Square Goodness-of-Fit reference values
*
* SPSS: NPAR TESTS /CHISQUARE
* Tests whether observed frequencies match expected frequencies.
*
* Variables tested:
*   a) gender (2 categories) - equal expected proportions
*   b) education (4 categories) - equal expected proportions
*   c) region (2 categories) - equal expected proportions
*   d) education with custom expected proportions (25%, 25%, 25%, 25%)
*   e) employment (5 categories) - equal expected proportions
*   f) interview_mode (3 categories) - custom expected (50%, 30%, 20%)
*
* Split grouping: region for grouped tests
* ============================================================================

GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/data/survey_data.sav'.

* Start Output Management System to save results as text

OMS
  /IF COMMANDS=['NPAR Tests']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/outputs/chisq_gof_output.txt'.

COMPUTE original_order = $CASENUM.

* ============================================================================
* TEST 1: UNWEIGHTED / UNGROUPED
* ============================================================================

TITLE '=========== TEST 1a: gender GoF (equal proportions, unweighted) ==========='.

NPAR TESTS
  /CHISQUARE=gender.

TITLE '=========== TEST 1b: education GoF (equal proportions, unweighted) ==========='.

NPAR TESTS
  /CHISQUARE=education.

TITLE '=========== TEST 1c: region GoF (equal proportions, unweighted) ==========='.

NPAR TESTS
  /CHISQUARE=region.

TITLE '=========== TEST 1d: employment GoF (equal proportions, unweighted) ==========='.

NPAR TESTS
  /CHISQUARE=employment.

TITLE '=========== TEST 1e: interview_mode GoF (equal proportions, unweighted) ==========='.

NPAR TESTS
  /CHISQUARE=interview_mode.

TITLE '=========== TEST 1f: interview_mode GoF (custom proportions 50/30/20, unweighted) ==========='.

* Custom expected proportions: 50% Face-to-face, 30% Telephone, 20% Online.
* SPSS expects these as raw expected counts or ratios.
* With N=2500 and 3 categories: expected = 1250, 750, 500.
* Or use proportional ratios: /EXPECTED=5 3 2.

NPAR TESTS
  /CHISQUARE=interview_mode
  /EXPECTED=5 3 2.

* ============================================================================
* TEST 2: WEIGHTED / UNGROUPED
* ============================================================================

TITLE '=========== TEST 2a: gender GoF (weighted) ==========='.

WEIGHT BY sampling_weight.

NPAR TESTS
  /CHISQUARE=gender.

TITLE '=========== TEST 2b: education GoF (weighted) ==========='.

NPAR TESTS
  /CHISQUARE=education.

TITLE '=========== TEST 2c: interview_mode GoF (custom proportions, weighted) ==========='.

NPAR TESTS
  /CHISQUARE=interview_mode
  /EXPECTED=5 3 2.

WEIGHT OFF.

* ============================================================================
* TEST 3: UNWEIGHTED / GROUPED BY REGION
* ============================================================================

TITLE '=========== TEST 3: education GoF, grouped by region ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

NPAR TESTS
  /CHISQUARE=education.

NPAR TESTS
  /CHISQUARE=gender.

* ============================================================================
* TEST 4: WEIGHTED / GROUPED BY REGION
* ============================================================================

TITLE '=========== TEST 4: education GoF, weighted, grouped ==========='.

WEIGHT BY sampling_weight.

NPAR TESTS
  /CHISQUARE=education.

WEIGHT OFF.
SPLIT FILE OFF.
SORT CASES BY original_order.

OMSEND.

EXECUTE.

* ============================================================================
* NOTES:
* ============================================================================
* From SPSS NPAR TESTS /CHISQUARE output, extract:
*
* Frequency table:
*   - Category, Observed N, Expected N, Residual (Observed - Expected)
*
* Test Statistics:
*   - Chi-Square statistic
*   - df (= number of categories - 1)
*   - Asymp. Sig. (p-value)
*
* For custom expected proportions (/EXPECTED=):
*   - SPSS distributes total N across categories using the given ratios
*   - e.g., /EXPECTED=5 3 2 with N=2500 -> expected = 1250, 750, 500
*
* R's chisq.test() equivalent:
*   chisq.test(table(data$var))                    # equal proportions
*   chisq.test(table(data$var), p = c(0.5, 0.3, 0.2))  # custom proportions
* ============================================================================
