* Encoding: UTF-8.
* SPSS Syntax for Binomial Test Validation
* Dataset: survey_data.sav
* Purpose: Validate binomial_test() function against SPSS NPAR TESTS /BINOMIAL procedure
* Variables: gender (Male/Female), region (East/West), high_satisfaction (recoded),
*            high_income (recoded)
* Weight: sampling_weight
* Split grouping: region

* ============================================================================.
* DATA PREPARATION: Create binary dependent variables
* ============================================================================.

* Recode gender to numeric binary (factor levels: Female=1, Male=2 in SPSS).
* We create a clean 0/1 variable for clarity.
RECODE gender (1=1)(2=0) INTO female.
VARIABLE LABELS female 'Female (0=Male, 1=Female)'.
VALUE LABELS female 0 'Male' 1 'Female'.
EXECUTE.

* Recode region to numeric binary (factor levels: East=1, West=2 in SPSS).
RECODE region (1=1)(2=0) INTO east.
VARIABLE LABELS east 'East region (0=West, 1=East)'.
VALUE LABELS east 0 'West' 1 'East'.
EXECUTE.

* Binary from life_satisfaction: 1-3 = 0 (Low), 4-5 = 1 (High).
RECODE life_satisfaction (1 thru 3=0)(4 thru 5=1) INTO high_satisfaction.
VARIABLE LABELS high_satisfaction 'High Life Satisfaction (0=No, 1=Yes)'.
VALUE LABELS high_satisfaction 0 'Low' 1 'High'.
EXECUTE.

* Binary from income: below/above 4000 EUR threshold.
RECODE income (LOWEST thru 4000=0)(4001 thru HIGHEST=1) INTO high_income.
VARIABLE LABELS high_income 'High Income (0=No, 1=Yes)'.
VALUE LABELS high_income 0 'Low' 1 'High'.
EXECUTE.

* Start Output Management System to save results as text

OMS
  /IF SUBTYPES=['Binomial Test']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/outputs/binomial_test_output.txt'.

COMPUTE original_order = $CASENUM.

* ============================================================================.
* TEST 1: UNWEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== UNWEIGHTED / UNGROUPED ==========='.

TITLE '=========== Test 1a ==========='.

* Test 1a: Gender proportion test (expected 50%)
SUBTITLE 'Binomial Test: female, test proportion = 0.50 (unweighted, ungrouped)'.
NPAR TESTS
  /BINOMIAL(.50)=female
  /MISSING ANALYSIS.

TITLE '=========== Test 1b ==========='.

* Test 1b: Region proportion test (expected 50%)
SUBTITLE 'Binomial Test: east, test proportion = 0.50 (unweighted, ungrouped)'.
NPAR TESTS
  /BINOMIAL(.50)=east
  /MISSING ANALYSIS.

TITLE '=========== Test 1c ==========='.

* Test 1c: High satisfaction proportion (expected 50%)
SUBTITLE 'Binomial Test: high_satisfaction, test proportion = 0.50 (unweighted, ungrouped)'.
NPAR TESTS
  /BINOMIAL(.50)=high_satisfaction
  /MISSING ANALYSIS.

TITLE '=========== Test 1d ==========='.

* Test 1d: High satisfaction with different expected proportion (60%)
SUBTITLE 'Binomial Test: high_satisfaction, test proportion = 0.60 (unweighted, ungrouped)'.
NPAR TESTS
  /BINOMIAL(.60)=high_satisfaction
  /MISSING ANALYSIS.

* ============================================================================.
* TEST 2: WEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== WEIGHTED / UNGROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 2a ==========='.

* Test 2a: Gender proportion (weighted)
SUBTITLE 'Binomial Test: female, test proportion = 0.50 (weighted, ungrouped)'.
NPAR TESTS
  /BINOMIAL(.50)=female
  /MISSING ANALYSIS.

TITLE '=========== Test 2b ==========='.

* Test 2b: High satisfaction (weighted)
SUBTITLE 'Binomial Test: high_satisfaction, test proportion = 0.50 (weighted, ungrouped)'.
NPAR TESTS
  /BINOMIAL(.50)=high_satisfaction
  /MISSING ANALYSIS.

WEIGHT OFF.

* ============================================================================.
* TEST 3: UNWEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== UNWEIGHTED / GROUPED ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

TITLE '=========== Test 3a ==========='.

* Test 3a: Gender proportion (grouped by region)
SUBTITLE 'Binomial Test: female, test proportion = 0.50 (unweighted, grouped by region)'.
NPAR TESTS
  /BINOMIAL(.50)=female
  /MISSING ANALYSIS.

TITLE '=========== Test 3b ==========='.

* Test 3b: High satisfaction (grouped by region)
SUBTITLE 'Binomial Test: high_satisfaction, test proportion = 0.50 (unweighted, grouped by region)'.
NPAR TESTS
  /BINOMIAL(.50)=high_satisfaction
  /MISSING ANALYSIS.

* ============================================================================.
* TEST 4: WEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== WEIGHTED / GROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 4a ==========='.

* Test 4a: Gender proportion (weighted, grouped by region)
SUBTITLE 'Binomial Test: female, test proportion = 0.50 (weighted, grouped by region)'.
NPAR TESTS
  /BINOMIAL(.50)=female
  /MISSING ANALYSIS.

TITLE '=========== Test 4b ==========='.

* Test 4b: High satisfaction (weighted, grouped by region)
SUBTITLE 'Binomial Test: high_satisfaction, test proportion = 0.50 (weighted, grouped by region)'.
NPAR TESTS
  /BINOMIAL(.50)=high_satisfaction
  /MISSING ANALYSIS.

WEIGHT OFF.
SPLIT FILE OFF.
SORT CASES BY original_order.

OMSEND.

EXECUTE.

* ============================================================================.
* NOTES FOR VALIDATION
* ============================================================================.
* Key values to extract from SPSS output:
*
* Binomial Test Table:
*   - Category (Group 1 / Group 2)
*   - N (per category)
*   - Observed Prop. (observed proportion for each category)
*   - Test Prop. (specified test proportion)
*   - Exact Sig. (2-tailed) (p-value based on exact binomial distribution)
*
* Tolerances for R tests:
*   - Observed Proportion: +/-0.001
*   - Exact Sig. (p-value): +/-0.0001
*   - N: exact match (unweighted), +/-1 (weighted)
*   - Test Proportion: exact match (user-specified)
*
* Data Preparation Notes:
*   - gender recoded: Female=1, Male=0 (from SPSS factor levels)
*   - region recoded: East=1, West=0
*   - high_satisfaction: life_satisfaction 1-3=0, 4-5=1
*   - high_income: income <= 4000=0, > 4000=1
*   - All RECODEs must run BEFORE the OMS block
*   - Cases with missing on original variable get SYSMIS in recoded variable
*
* Important SPSS Behavior:
*   - BINOMIAL test uses exact binomial probability (not normal approximation)
*   - For large N, exact p-value may differ slightly from normal approximation
*   - Test proportion (.50) specifies the null hypothesis proportion for Group 1
*   - Group 1 = lower value (0), Group 2 = higher value (1)
*   - Two-tailed test: p = 2 * min(P(X <= observed), P(X >= observed))
*   - WEIGHT BY treats weights as frequency weights
*   - Weighted N is rounded for binomial calculation
*
* Weighted Analyses:
*   - Weighted counts used for binomial probability calculation
*   - Observed proportion based on weighted N
*   - Exact significance based on weighted sample size
*   - Weighted N may be non-integer (SPSS rounds for calculation)
*
* Grouped Analyses:
*   - Separate binomial test per region (East/West)
*   - Different N per group
*   - Proportions and significance may differ between groups
*
* Comparison to R:
*   - R's binom.test() provides exact binomial test
*   - R uses 2-sided p-value = 2 * min(lower.tail, upper.tail)
*   - Should match SPSS exact significance closely
*   - For weighted: may need custom implementation (weighted binomial)
* ============================================================================.
