* Encoding: UTF-8.
* ============================================================================
* PAIRWISE WILCOXON (Post-Hoc for Friedman) - SPSS VALIDATION SYNTAX
* ============================================================================
* Dataset: survey_data.sav, longitudinal_data_wide.sav
* Purpose: Generate pairwise Wilcoxon signed-rank reference values after Friedman
*
* SPSS uses "Pairwise Comparisons" with Bonferroni correction when running
* Related Samples via the newer nonparametric tests dialog.
*
* Variable sets tested:
*   a) trust_government, trust_media, trust_science (3 vars -> 3 pairs)
*   b) life_satisfaction, environmental_concern, political_orientation (3 vars)
*   c) longitudinal: score_T1 - score_T4 (4 vars -> 6 pairs)
*
* Split grouping: region (East/West) for grouped tests (survey_data)
*                 group (Control/Treatment) for grouped tests (longitudinal)
* ============================================================================

GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/data/survey_data.sav'.

* Start Output Management System to save results as text

OMS
  /IF COMMANDS=['NPAR Tests' 'NPTESTS']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/outputs/pairwise_wilcoxon_output.txt'.

* ============================================================================
* PART A: SURVEY DATA (3 related measurements)
* ============================================================================

COMPUTE original_order = $CASENUM.

* ============================================================================
* TEST 1a: UNWEIGHTED / UNGROUPED - trust items
* ============================================================================

TITLE '=========== TEST 1a: trust items pairwise Wilcoxon (unweighted) ==========='.

* First run Friedman to confirm overall significance.
NPAR TESTS
  /FRIEDMAN = trust_government trust_media trust_science.

* Then run pairwise Wilcoxon tests (each pair individually).
* SPSS: Analyze -> Nonparametric Tests -> Related Samples
*   -> Fields: all 3 trust variables
*   -> Settings: Customize Tests -> Friedman's 2-way ANOVA (k samples)
*     -> Check "All pairwise" under Multiple Comparisons

NPTESTS
  /RELATED TEST(trust_government trust_media trust_science)
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE
  /CRITERIA ALPHA=0.05 CILEVEL=95.

* Individual Wilcoxon tests for exact reference values:
NPAR TESTS
  /WILCOXON = trust_government WITH trust_media (PAIRED)
  /WILCOXON = trust_government WITH trust_science (PAIRED)
  /WILCOXON = trust_media WITH trust_science (PAIRED).

* ============================================================================
* TEST 1b: UNWEIGHTED / UNGROUPED - attitude items
* ============================================================================

TITLE '=========== TEST 1b: attitude items pairwise Wilcoxon (unweighted) ==========='.

NPAR TESTS
  /FRIEDMAN = life_satisfaction environmental_concern political_orientation.

NPAR TESTS
  /WILCOXON = life_satisfaction WITH environmental_concern (PAIRED)
  /WILCOXON = life_satisfaction WITH political_orientation (PAIRED)
  /WILCOXON = environmental_concern WITH political_orientation (PAIRED).

* ============================================================================
* TEST 2: WEIGHTED / UNGROUPED
* ============================================================================

TITLE '=========== TEST 2a: trust items pairwise Wilcoxon (weighted) ==========='.

WEIGHT BY sampling_weight.

NPAR TESTS
  /WILCOXON = trust_government WITH trust_media (PAIRED)
  /WILCOXON = trust_government WITH trust_science (PAIRED)
  /WILCOXON = trust_media WITH trust_science (PAIRED).

WEIGHT OFF.

* ============================================================================
* TEST 3: UNWEIGHTED / GROUPED BY REGION
* ============================================================================

TITLE '=========== TEST 3: trust items pairwise Wilcoxon, grouped by region ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

NPAR TESTS
  /WILCOXON = trust_government WITH trust_media (PAIRED)
  /WILCOXON = trust_government WITH trust_science (PAIRED)
  /WILCOXON = trust_media WITH trust_science (PAIRED).

* ============================================================================
* TEST 4: WEIGHTED / GROUPED BY REGION
* ============================================================================

TITLE '=========== TEST 4: trust items pairwise Wilcoxon, weighted, grouped ==========='.

WEIGHT BY sampling_weight.

NPAR TESTS
  /WILCOXON = trust_government WITH trust_media (PAIRED)
  /WILCOXON = trust_government WITH trust_science (PAIRED)
  /WILCOXON = trust_media WITH trust_science (PAIRED).

WEIGHT OFF.
SPLIT FILE OFF.

* ============================================================================
* PART B: LONGITUDINAL DATA (4 timepoints -> 6 pairs)
* ============================================================================

TITLE '=========== TEST 5a: longitudinal pairwise Wilcoxon (ungrouped) ==========='.

GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/data/longitudinal_data_wide.sav'.

NPAR TESTS
  /FRIEDMAN = score_T1 score_T2 score_T3 score_T4.

NPAR TESTS
  /WILCOXON = score_T1 WITH score_T2 (PAIRED)
  /WILCOXON = score_T1 WITH score_T3 (PAIRED)
  /WILCOXON = score_T1 WITH score_T4 (PAIRED)
  /WILCOXON = score_T2 WITH score_T3 (PAIRED)
  /WILCOXON = score_T2 WITH score_T4 (PAIRED)
  /WILCOXON = score_T3 WITH score_T4 (PAIRED).

TITLE '=========== TEST 5b: longitudinal pairwise Wilcoxon, grouped by treatment ==========='.

SORT CASES BY group.
SPLIT FILE BY group.

NPAR TESTS
  /WILCOXON = score_T1 WITH score_T2 (PAIRED)
  /WILCOXON = score_T1 WITH score_T3 (PAIRED)
  /WILCOXON = score_T1 WITH score_T4 (PAIRED)
  /WILCOXON = score_T2 WITH score_T3 (PAIRED)
  /WILCOXON = score_T2 WITH score_T4 (PAIRED)
  /WILCOXON = score_T3 WITH score_T4 (PAIRED).

SPLIT FILE OFF.

OMSEND.

EXECUTE.

* ============================================================================
* NOTES:
* ============================================================================
* For each pairwise Wilcoxon test, extract:
*   - Z statistic
*   - Asymp. Sig. (2-tailed) = unadjusted p-value
*   - N (number of valid pairs)
*   - Positive ranks, Negative ranks, Ties
*
* For Bonferroni adjustment:
*   adj_p = min(1, p * k*(k-1)/2)
*   where k = number of measurements
*
* The NPTESTS pairwise comparisons table gives Bonferroni-adjusted p directly.
* Individual NPAR TESTS /WILCOXON gives the raw (unadjusted) values.
* ============================================================================
