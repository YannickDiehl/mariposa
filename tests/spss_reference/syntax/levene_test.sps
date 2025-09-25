* Encoding: UTF-8.
* SPSS Syntax for Testing
* Dataset: survey_data.sav

* Start Output Management System to save results as text

OMS
  /IF SUBTYPES=['Homogeneity of Variance Test']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/outputs/levene_test_output.txt'.

COMPUTE original_order = $CASENUM.

* ================================================
* TEST 1: UNWEIGHTED / UNGROUPED
* ================================================

TITLE '=========== UNWEIGHTED / UNGROUPED ==========='.

TITLE '=========== Test 1a ==========='.

* Test 1a: One-sample t-test for life_satisfaction (mu = 3.0)

TITLE 'One-Sample T-Test: Life Satisfaction (mu = 3.0)'.

T-TEST
  /TESTVAL=3.0
  /MISSING=ANALYSIS
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.95).

TITLE '=========== Test 1b ==========='.

* Test 1b: Independent samples t-test for life_satisfaction by gender

TITLE 'Independent Samples T-Test: Life Satisfaction by Gender'.

T-TEST GROUPS=gender
  /MISSING=ANALYSIS
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.95).

TITLE '=========== Test 1c ==========='.

* Test 1c: Independent samples t-test for income by gender

TITLE 'Independent Samples T-Test: Income by Gender'.

T-TEST GROUPS=gender
  /MISSING=ANALYSIS
  /VARIABLES=income
  /CRITERIA=CI(.95).

TITLE '=========== Test 1d ==========='.

* Test 1d: Independent samples t-test for age by gender

TITLE 'Independent Samples T-Test: Age by Gender'.

T-TEST GROUPS=gender
  /MISSING=ANALYSIS
  /VARIABLES=age
  /CRITERIA=CI(.95).

* ================================================
* TEST 2: WEIGHTED / UNGROUPED
* ================================================

TITLE '=========== WEIGHTED / UNGROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 2a ==========='.

* Test 2a: One-sample t-test for life_satisfaction (weighted, mu = 3.0)

TITLE 'One-Sample T-Test: Life Satisfaction (weighted, mu = 3.0)'.

T-TEST
  /TESTVAL=3.0
  /MISSING=ANALYSIS
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.95).

TITLE '=========== Test 2b ==========='.

* Test 2b: Independent samples t-test for life_satisfaction by gender (weighted)

TITLE 'Independent Samples T-Test: Life Satisfaction by Gender (weighted)'.

T-TEST GROUPS=gender
  /MISSING=ANALYSIS
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.95).

TITLE '=========== Test 2c ==========='.

* Test 2c: Independent samples t-test for income by gender (weighted)

TITLE 'Independent Samples T-Test: Income by Gender (weighted)'.

T-TEST GROUPS=gender
  /MISSING=ANALYSIS
  /VARIABLES=income
  /CRITERIA=CI(.95).

TITLE '=========== Test 2d ==========='.

* Test 2d: Independent samples t-test for age by gender (weighted)

TITLE 'Independent Samples T-Test: Age by Gender (weighted)'.

T-TEST GROUPS=gender
  /MISSING=ANALYSIS
  /VARIABLES=age
  /CRITERIA=CI(.95).

WEIGHT OFF.

* ================================================
* TEST 3: UNWEIGHTED / GROUPED
* ================================================

TITLE '=========== UNWEIGHTED / GROUPED ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

TITLE '=========== Test 3a ==========='.

* Test 3a: Independent samples t-test for life_satisfaction by gender (grouped by region)

TITLE 'Independent Samples T-Test: Life Satisfaction by Gender (grouped by region)'.

T-TEST GROUPS=gender
  /MISSING=ANALYSIS
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.95).

TITLE '=========== Test 3b ==========='.

* Test 3b: Independent samples t-test for income by gender (grouped by region)

TITLE 'Independent Samples T-Test: Income by Gender (grouped by region)'.

T-TEST GROUPS=gender
  /MISSING=ANALYSIS
  /VARIABLES=income
  /CRITERIA=CI(.95).

TITLE '=========== Test 3c ==========='.

* Test 3c: Independent samples t-test for age by gender (grouped by region)

TITLE 'Independent Samples T-Test: Age by Gender (grouped by region)'.

T-TEST GROUPS=gender
  /MISSING=ANALYSIS
  /VARIABLES=age
  /CRITERIA=CI(.95).

* ================================================
* TEST 4: WEIGHTED / GROUPED
* ================================================

TITLE '=========== WEIGHTED / GROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 4a ==========='.

* Test 4a: Independent samples t-test for life_satisfaction by gender (weighted, grouped by region)

TITLE 'Independent Samples T-Test: Life Satisfaction by Gender (weighted, grouped by region)'.

T-TEST GROUPS=gender
  /MISSING=ANALYSIS
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.95).

TITLE '=========== Test 4b ==========='.

* Test 4b: Independent samples t-test for income by gender (weighted, grouped by region)

TITLE 'Independent Samples T-Test: Income by Gender (weighted, grouped by region)'.

T-TEST GROUPS=gender
  /MISSING=ANALYSIS
  /VARIABLES=income
  /CRITERIA=CI(.95).

TITLE '=========== Test 4c ==========='.

* Test 4c: Independent samples t-test for age by gender (weighted, grouped by region)

TITLE 'Independent Samples T-Test: Age by Gender (weighted, grouped by region)'.

T-TEST GROUPS=gender
  /MISSING=ANALYSIS
  /VARIABLES=age
  /CRITERIA=CI(.95).

WEIGHT OFF.
SPLIT FILE OFF.
SORT CASES BY original_order.

* ================================================
* ADDITIONAL TESTS FOR SPECIAL CASES
* ================================================

TITLE '=========== ADDITIONAL TEST CASES ==========='.

* Test 5: One-sample tests with different mu values

TITLE '=========== Test 5a ==========='.

TITLE 'One-Sample T-Test: Income (mu = 5000)'.

T-TEST
  /TESTVAL=5000
  /MISSING=ANALYSIS
  /VARIABLES=income
  /CRITERIA=CI(.95).

TITLE '=========== Test 5b ==========='.

TITLE 'One-Sample T-Test: Age (mu = 45)'.

T-TEST
  /TESTVAL=45
  /MISSING=ANALYSIS
  /VARIABLES=age
  /CRITERIA=CI(.95).

* Test 6: Multiple variables in single command (for efficiency check)

TITLE '=========== Test 6 ==========='.

TITLE 'Multiple Variables T-Test by Gender'.

T-TEST GROUPS=gender
  /MISSING=ANALYSIS
  /VARIABLES=life_satisfaction income age trust_government trust_media trust_science
  /CRITERIA=CI(.95).

* Test 7: Different confidence intervals

TITLE '=========== Test 7a ==========='.

TITLE 'T-Test with 90% CI'.

T-TEST GROUPS=gender
  /MISSING=ANALYSIS
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.90).

TITLE '=========== Test 7b ==========='.

TITLE 'T-Test with 99% CI'.

T-TEST GROUPS=gender
  /MISSING=ANALYSIS
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.99).

OMSEND.

EXECUTE.

* ================================================
* NOTES FOR VALIDATION
* ================================================
*
* Key values to extract from SPSS output:
*
* One-Sample Tests:
* - N (sample size)
* - Mean
* - Std. Deviation
* - Std. Error Mean
* - t-statistic
* - df (degrees of freedom)
* - Sig. (2-tailed) = p-value
* - Mean Difference
* - 95% CI Lower
* - 95% CI Upper
*
* Independent Samples Tests:
* - N for each group
* - Mean for each group
* - Std. Deviation for each group
* - Std. Error Mean for each group
* - Levene's Test (F, Sig.)
* - t-statistic (both equal and unequal variances)
* - df (both assumptions)
* - Sig. (2-tailed) for both
* - Mean Difference
* - Std. Error Difference
* - 95% CI Lower and Upper (both assumptions)
*
* Effect Sizes (if available in SPSS version):
* - Cohen's d
* - Hedges' g
* - Glass' Delta
*
* Note: SPSS may not provide all effect sizes directly.
* Manual calculation may be needed for validation.
*
* Weighted analyses:
* - Weighted N may be decimal
* - Check both displayed and actual values
* - Verify weight application in grouped analyses