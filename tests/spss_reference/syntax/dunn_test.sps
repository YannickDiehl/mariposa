* Encoding: UTF-8.
* ============================================================================
* DUNN TEST (Post-Hoc for Kruskal-Wallis) - SPSS VALIDATION SYNTAX
* ============================================================================
* Dataset: survey_data.sav
* Purpose: Generate Kruskal-Wallis rank data for Dunn test validation
*
* Strategy: Use NPAR TESTS /K-W to get ranks and omnibus test statistics.
* Dunn pairwise comparisons are computed from the K-W rank data using:
*   Z_ij = (Rbar_i - Rbar_j) / sqrt((N*(N+1)/12) * (1/n_i + 1/n_j))
*   p_adj = min(1, p_unadj * k*(k-1)/2)  [Bonferroni]
*
* Note: SPSS NPTESTS command outputs to Model Viewer format which is
* incompatible with OMS export. Additionally, NPTESTS fails when combined
* with WEIGHT BY + SPLIT FILE. Therefore, we use only NPAR TESTS /K-W
* and compute Dunn pairwise values from the validated rank output.
*
* Variables tested:
*   a) life_satisfaction by education (4 groups -> 6 comparisons)
*   b) income by employment (5 groups -> 10 comparisons)
*   c) trust_government by education (4 groups, non-significant KW)
*
* Split grouping: region (East/West) for grouped tests
* ============================================================================

GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/data/survey_data.sav'.

OMS
  /IF COMMANDS=['NPAR Tests']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/outputs/dunn_test_output.txt'.

COMPUTE original_order = $CASENUM.

* ============================================================================
* TEST 1: UNWEIGHTED / UNGROUPED
* ============================================================================

TITLE '=========== TEST 1a: life_satisfaction by education (unweighted) ==========='.

NPAR TESTS
  /K-W = life_satisfaction BY education(1,4).

TITLE '=========== TEST 1b: income by employment (unweighted) ==========='.

NPAR TESTS
  /K-W = income BY employment(1,5).

TITLE '=========== TEST 1c: trust_government by education (unweighted, non-sig KW) ==========='.

NPAR TESTS
  /K-W = trust_government BY education(1,4).

* ============================================================================
* TEST 2: WEIGHTED / UNGROUPED
* ============================================================================

TITLE '=========== TEST 2a: life_satisfaction by education (weighted) ==========='.

WEIGHT BY sampling_weight.

NPAR TESTS
  /K-W = life_satisfaction BY education(1,4).

TITLE '=========== TEST 2b: income by employment (weighted) ==========='.

NPAR TESTS
  /K-W = income BY employment(1,5).

WEIGHT OFF.

* ============================================================================
* TEST 3: UNWEIGHTED / GROUPED BY REGION
* ============================================================================

TITLE '=========== TEST 3: life_satisfaction by education, grouped by region ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

NPAR TESTS
  /K-W = life_satisfaction BY education(1,4).

* ============================================================================
* TEST 4: WEIGHTED / GROUPED BY REGION
* ============================================================================

TITLE '=========== TEST 4: life_satisfaction by education, weighted, grouped ==========='.

WEIGHT BY sampling_weight.

NPAR TESTS
  /K-W = life_satisfaction BY education(1,4).

WEIGHT OFF.
SPLIT FILE OFF.
SORT CASES BY original_order.

OMSEND.

EXECUTE.

* ============================================================================
* NOTES:
* ============================================================================
* Output file: dunn_test_output.txt (TEXT)
*   - Ranks table: Group, N, Mean Rank (for each test scenario)
*   - Test Statistics: Chi-Square (H), df, Asymp. Sig.
*
* Dunn pairwise reference values are computed in R from the validated
* SPSS rank output. See: tests/spss_reference/compute_dunn_references.R
*
* The computation is deterministic:
*   1. Extract N_i and Rbar_i from SPSS Ranks output
*   2. Z_ij = (Rbar_i - Rbar_j) / sqrt((N*(N+1)/12) * (1/n_i + 1/n_j))
*   3. p_unadj = 2 * pnorm(-abs(Z_ij))
*   4. p_adj = min(1, p_unadj * k*(k-1)/2)  [Bonferroni]
* ============================================================================
