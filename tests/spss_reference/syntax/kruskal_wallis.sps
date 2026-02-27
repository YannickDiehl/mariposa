* Encoding: UTF-8.
* SPSS Syntax for Kruskal-Wallis Test Validation
* Dataset: survey_data.sav
* Purpose: Validate kruskal_wallis() function against SPSS NPAR TESTS /K-W procedure
* DVs: life_satisfaction, income, trust_government (ordinal/continuous)
* Grouping factors: education (4 levels, coded 1-4), employment (5 levels, coded 1-5)
* Weight: sampling_weight
* Split grouping: region

* Start Output Management System to save results as text

OMS
  /IF SUBTYPES=['Kruskal-Wallis Test' 'Test Statistics' 'Ranks']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/outputs/kruskal_wallis_output.txt'.

COMPUTE original_order = $CASENUM.

* ============================================================================.
* TEST 1: UNWEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== UNWEIGHTED / UNGROUPED ==========='.

TITLE '=========== Test 1a ==========='.

* Test 1a: life_satisfaction by education (4 groups)
SUBTITLE 'Kruskal-Wallis: life_satisfaction by education (unweighted, ungrouped)'.
NPAR TESTS
  /K-W=life_satisfaction BY education(1,4)
  /MISSING ANALYSIS.

TITLE '=========== Test 1b ==========='.

* Test 1b: income by employment (5 groups)
SUBTITLE 'Kruskal-Wallis: income by employment (unweighted, ungrouped)'.
NPAR TESTS
  /K-W=income BY employment(1,5)
  /MISSING ANALYSIS.

TITLE '=========== Test 1c ==========='.

* Test 1c: trust_government by education (4 groups, different outcome)
SUBTITLE 'Kruskal-Wallis: trust_government by education (unweighted, ungrouped)'.
NPAR TESTS
  /K-W=trust_government BY education(1,4)
  /MISSING ANALYSIS.

* ============================================================================.
* TEST 2: WEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== WEIGHTED / UNGROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 2a ==========='.

* Test 2a: life_satisfaction by education (weighted)
SUBTITLE 'Kruskal-Wallis: life_satisfaction by education (weighted, ungrouped)'.
NPAR TESTS
  /K-W=life_satisfaction BY education(1,4)
  /MISSING ANALYSIS.

TITLE '=========== Test 2b ==========='.

* Test 2b: income by employment (weighted)
SUBTITLE 'Kruskal-Wallis: income by employment (weighted, ungrouped)'.
NPAR TESTS
  /K-W=income BY employment(1,5)
  /MISSING ANALYSIS.

WEIGHT OFF.

* ============================================================================.
* TEST 3: UNWEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== UNWEIGHTED / GROUPED ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

TITLE '=========== Test 3a ==========='.

* Test 3a: life_satisfaction by education (grouped by region)
SUBTITLE 'Kruskal-Wallis: life_satisfaction by education (unweighted, grouped by region)'.
NPAR TESTS
  /K-W=life_satisfaction BY education(1,4)
  /MISSING ANALYSIS.

TITLE '=========== Test 3b ==========='.

* Test 3b: income by employment (grouped by region)
SUBTITLE 'Kruskal-Wallis: income by employment (unweighted, grouped by region)'.
NPAR TESTS
  /K-W=income BY employment(1,5)
  /MISSING ANALYSIS.

* ============================================================================.
* TEST 4: WEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== WEIGHTED / GROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 4a ==========='.

* Test 4a: life_satisfaction by education (weighted, grouped by region)
SUBTITLE 'Kruskal-Wallis: life_satisfaction by education (weighted, grouped by region)'.
NPAR TESTS
  /K-W=life_satisfaction BY education(1,4)
  /MISSING ANALYSIS.

TITLE '=========== Test 4b ==========='.

* Test 4b: income by employment (weighted, grouped by region)
SUBTITLE 'Kruskal-Wallis: income by employment (weighted, grouped by region)'.
NPAR TESTS
  /K-W=income BY employment(1,5)
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
* Ranks Table (for each group):
*   - Group label
*   - N (sample size per group)
*   - Mean Rank
*
* Test Statistics Table:
*   - Chi-Square (Kruskal-Wallis H statistic)
*   - df (number of groups - 1)
*   - Asymp. Sig. (p-value)
*
* Tolerances for R tests:
*   - Chi-Square (H statistic): +/-0.01
*   - df: exact match
*   - p-value: +/-0.0001
*   - Mean Ranks: +/-0.01
*   - N (unweighted): exact match
*   - N (weighted): +/-1
*
* Important SPSS Behavior:
*   - NPAR TESTS with WEIGHT BY treats weights as frequency weights
*     (cases are replicated according to weight value)
*   - education coded as 1=Basic Secondary, 2=Intermediate Secondary,
*     3=Academic Secondary, 4=University
*   - employment coded as 1=Employed, 2=Other, 3=Retired,
*     4=Student, 5=Unemployed (alphabetical factor level order from R)
*   - Missing values: /MISSING ANALYSIS excludes cases with missing DV
*   - Ties are handled using average ranks (SPSS default)
*
* Relationship to Mann-Whitney:
*   - For 2 groups, Kruskal-Wallis H equals Mann-Whitney U (adjusted)
*   - Chi-Square = Z^2 for 2-group case
*   - Kruskal-Wallis is the extension to k > 2 groups
*
* Weighted Analyses:
*   - Weighted ranks are computed using frequency-weighted observations
*   - Total N reflects sum of weights
*   - Chi-Square adjusted for weighted sample size
*
* Grouped Analyses:
*   - Separate Kruskal-Wallis test per region (East/West)
*   - Ranks computed within each group independently
*   - Test statistics and significance may differ between groups
* ============================================================================.
