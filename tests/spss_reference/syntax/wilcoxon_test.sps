* Encoding: UTF-8.
* SPSS Syntax for Wilcoxon Signed-Rank Test Validation
* Datasets: survey_data.sav (trust items as paired measures)
*           longitudinal_data.sav (pre/post comparisons, additional tests)
* Purpose: Validate wilcoxon_test() function against SPSS NPAR TESTS /WILCOXON procedure
* Pairs: trust_government WITH trust_media, trust_government WITH trust_science, etc.
* Weight: sampling_weight (survey_data only)
* Split grouping: region (survey_data), group (longitudinal_data)

* ============================================================================.
* PART A: SURVEY_DATA (trust items as paired measures, supports weighting)
* ============================================================================.

GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/data/survey_data.sav'.

* Start Output Management System to save results as text

OMS
  /IF COMMANDS=['NPAR Tests']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/outputs/wilcoxon_test_output.txt'.

COMPUTE original_order = $CASENUM.
EXECUTE.

* ============================================================================.
* TEST 1: UNWEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== UNWEIGHTED / UNGROUPED ==========='.

TITLE '=========== Test 1a ==========='.

* Test 1a: trust_government vs trust_media
SUBTITLE 'Wilcoxon: trust_government vs trust_media (unweighted, ungrouped)'.
NPAR TESTS
  /WILCOXON=trust_government WITH trust_media (PAIRED)
  /MISSING ANALYSIS.

TITLE '=========== Test 1b ==========='.

* Test 1b: trust_government vs trust_science
SUBTITLE 'Wilcoxon: trust_government vs trust_science (unweighted, ungrouped)'.
NPAR TESTS
  /WILCOXON=trust_government WITH trust_science (PAIRED)
  /MISSING ANALYSIS.

TITLE '=========== Test 1c ==========='.

* Test 1c: trust_media vs trust_science
SUBTITLE 'Wilcoxon: trust_media vs trust_science (unweighted, ungrouped)'.
NPAR TESTS
  /WILCOXON=trust_media WITH trust_science (PAIRED)
  /MISSING ANALYSIS.

* ============================================================================.
* TEST 2: WEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== WEIGHTED / UNGROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 2a ==========='.

* Test 2a: trust_government vs trust_media (weighted)
SUBTITLE 'Wilcoxon: trust_government vs trust_media (weighted, ungrouped)'.
NPAR TESTS
  /WILCOXON=trust_government WITH trust_media (PAIRED)
  /MISSING ANALYSIS.

TITLE '=========== Test 2b ==========='.

* Test 2b: trust_government vs trust_science (weighted)
SUBTITLE 'Wilcoxon: trust_government vs trust_science (weighted, ungrouped)'.
NPAR TESTS
  /WILCOXON=trust_government WITH trust_science (PAIRED)
  /MISSING ANALYSIS.

WEIGHT OFF.

* ============================================================================.
* TEST 3: UNWEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== UNWEIGHTED / GROUPED ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

TITLE '=========== Test 3a ==========='.

* Test 3a: trust_government vs trust_media (grouped by region)
SUBTITLE 'Wilcoxon: trust_government vs trust_media (unweighted, grouped by region)'.
NPAR TESTS
  /WILCOXON=trust_government WITH trust_media (PAIRED)
  /MISSING ANALYSIS.

TITLE '=========== Test 3b ==========='.

* Test 3b: trust_government vs trust_science (grouped by region)
SUBTITLE 'Wilcoxon: trust_government vs trust_science (unweighted, grouped by region)'.
NPAR TESTS
  /WILCOXON=trust_government WITH trust_science (PAIRED)
  /MISSING ANALYSIS.

* ============================================================================.
* TEST 4: WEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== WEIGHTED / GROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 4a ==========='.

* Test 4a: trust_government vs trust_media (weighted, grouped by region)
SUBTITLE 'Wilcoxon: trust_government vs trust_media (weighted, grouped by region)'.
NPAR TESTS
  /WILCOXON=trust_government WITH trust_media (PAIRED)
  /MISSING ANALYSIS.

TITLE '=========== Test 4b ==========='.

* Test 4b: trust_government vs trust_science (weighted, grouped by region)
SUBTITLE 'Wilcoxon: trust_government vs trust_science (weighted, grouped by region)'.
NPAR TESTS
  /WILCOXON=trust_government WITH trust_science (PAIRED)
  /MISSING ANALYSIS.

WEIGHT OFF.
SPLIT FILE OFF.
SORT CASES BY original_order.

* ============================================================================.
* PART B: LONGITUDINAL_DATA (classic pre/post design, no weights)
* ============================================================================.

TITLE '=========== ADDITIONAL TESTS: LONGITUDINAL DATA ==========='.

GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/data/longitudinal_data_wide.sav'.

* Note: longitudinal_data_wide.sav is in wide format (120 rows, 1 per subject)
* with columns: subject_id, group, age, gender, score_T1, score_T2, score_T3, score_T4

TITLE '=========== Test 5a ==========='.

* Test 5a: Pre-Post comparison (T1 vs T2)
SUBTITLE 'Wilcoxon: score_T1 vs score_T2 (longitudinal, ungrouped)'.
NPAR TESTS
  /WILCOXON=score_T1 WITH score_T2 (PAIRED)
  /MISSING ANALYSIS.

TITLE '=========== Test 5b ==========='.

* Test 5b: Longer time span (T1 vs T3)
SUBTITLE 'Wilcoxon: score_T1 vs score_T3 (longitudinal, ungrouped)'.
NPAR TESTS
  /WILCOXON=score_T1 WITH score_T3 (PAIRED)
  /MISSING ANALYSIS.

TITLE '=========== Test 5c ==========='.

* Test 5c: Pre-Post by treatment group
SUBTITLE 'Wilcoxon: score_T1 vs score_T2 (longitudinal, grouped by treatment)'.
SORT CASES BY group.
SPLIT FILE BY group.

NPAR TESTS
  /WILCOXON=score_T1 WITH score_T2 (PAIRED)
  /MISSING ANALYSIS.

SPLIT FILE OFF.

OMSEND.

EXECUTE.

* ============================================================================.
* NOTES FOR VALIDATION
* ============================================================================.
* Key values to extract from SPSS output:
*
* Ranks Table:
*   - Negative Ranks: N, Mean Rank, Sum of Ranks
*   - Positive Ranks: N, Mean Rank, Sum of Ranks
*   - Ties: N
*   - Total: N
*
* Test Statistics Table:
*   - Z (standardized test statistic)
*   - Asymp. Sig. (2-tailed) (p-value)
*   - Note: "Based on positive/negative ranks" footnote
*
* Tolerances for R tests:
*   - Z statistic: +/-0.001
*   - p-value: +/-0.0001
*   - Mean Ranks: +/-0.01
*   - Sum of Ranks: +/-0.1
*   - N (ranks categories): exact match
*
* Important SPSS Behavior:
*   - PAIRED keyword ensures paired comparison (not independent groups)
*   - Differences computed as: variable2 - variable1
*   - Negative ranks = variable1 > variable2 (decreased)
*   - Positive ranks = variable2 > variable1 (increased)
*   - Ties = variable1 == variable2 (no change)
*   - Z uses normal approximation with continuity correction
*   - WEIGHT BY treats weights as frequency weights
*   - Missing: pairwise deletion (case excluded if either variable missing)
*
* Survey Data Tests (Part A):
*   - Trust items are on same 1-5 Likert scale (valid paired comparison)
*   - Same respondent provides both ratings (within-subject design)
*   - Supports all 4 standard scenarios including weights
*
* Longitudinal Data Tests (Part B):
*   - Classic pre/post repeated measures design
*   - 120 subjects, 4 timepoints
*   - No weight variable available
*   - Uses longitudinal_data_wide.sav (wide format: score_T1, score_T2, score_T3, score_T4)
*   - Treatment group (Control/Treatment) used for grouped analysis
*
* Weighted Analyses:
*   - Weighted ranks adjust for differential sampling probabilities
*   - Z statistic and p-value affected by weighted sample size
*   - Mean Rank and Sum of Ranks reflect weighted observations
*
* Grouped Analyses:
*   - Separate Wilcoxon test per region (East/West) or treatment group
*   - Ranks computed within each group independently
* ============================================================================.
