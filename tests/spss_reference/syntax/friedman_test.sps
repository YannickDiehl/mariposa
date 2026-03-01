* Encoding: UTF-8.
* SPSS Syntax for Friedman Test Validation
* Datasets: survey_data.sav (trust/attitude items as repeated measures)
*           longitudinal_data.sav (4-timepoint repeated measures, additional tests)
* Purpose: Validate friedman_test() function against SPSS NPAR TESTS /FRIEDMAN procedure
* Variables: trust_government, trust_media, trust_science (3 related measures)
*            life_satisfaction, environmental_concern, political_orientation (alternative set)
* Weight: sampling_weight (survey_data only)
* Split grouping: region (survey_data), group (longitudinal_data)

* ============================================================================.
* PART A: SURVEY_DATA (trust/attitude items as repeated measures)
* ============================================================================.

GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/data/survey_data.sav'.

* Start Output Management System to save results as text

OMS
  /IF COMMANDS=['NPAR Tests']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/outputs/friedman_test_output.txt'.

COMPUTE original_order = $CASENUM.
EXECUTE.

* ============================================================================.
* TEST 1: UNWEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== UNWEIGHTED / UNGROUPED ==========='.

TITLE '=========== Test 1a ==========='.

* Test 1a: Trust items (3 related measures on same 1-5 scale)
SUBTITLE 'Friedman: trust_government, trust_media, trust_science (unweighted, ungrouped)'.
NPAR TESTS
  /FRIEDMAN=trust_government trust_media trust_science
  /MISSING LISTWISE.

TITLE '=========== Test 1b ==========='.

* Test 1b: Attitude items (3 related measures on same 1-5 scale)
SUBTITLE 'Friedman: life_satisfaction, environmental_concern, political_orientation (unweighted, ungrouped)'.
NPAR TESTS
  /FRIEDMAN=life_satisfaction environmental_concern political_orientation
  /MISSING LISTWISE.

* ============================================================================.
* TEST 2: WEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== WEIGHTED / UNGROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 2a ==========='.

* Test 2a: Trust items (weighted)
SUBTITLE 'Friedman: trust items (weighted, ungrouped)'.
NPAR TESTS
  /FRIEDMAN=trust_government trust_media trust_science
  /MISSING LISTWISE.

TITLE '=========== Test 2b ==========='.

* Test 2b: Attitude items (weighted)
SUBTITLE 'Friedman: attitude items (weighted, ungrouped)'.
NPAR TESTS
  /FRIEDMAN=life_satisfaction environmental_concern political_orientation
  /MISSING LISTWISE.

WEIGHT OFF.

* ============================================================================.
* TEST 3: UNWEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== UNWEIGHTED / GROUPED ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

TITLE '=========== Test 3a ==========='.

* Test 3a: Trust items (grouped by region)
SUBTITLE 'Friedman: trust items (unweighted, grouped by region)'.
NPAR TESTS
  /FRIEDMAN=trust_government trust_media trust_science
  /MISSING LISTWISE.

* ============================================================================.
* TEST 4: WEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== WEIGHTED / GROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 4a ==========='.

* Test 4a: Trust items (weighted, grouped by region)
SUBTITLE 'Friedman: trust items (weighted, grouped by region)'.
NPAR TESTS
  /FRIEDMAN=trust_government trust_media trust_science
  /MISSING LISTWISE.

WEIGHT OFF.
SPLIT FILE OFF.
SORT CASES BY original_order.

* ============================================================================.
* PART B: LONGITUDINAL_DATA (classic repeated measures, 4 timepoints)
* ============================================================================.

TITLE '=========== ADDITIONAL TESTS: LONGITUDINAL DATA ==========='.

GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/data/longitudinal_data_wide.sav'.

* Note: longitudinal_data_wide.sav is in wide format (120 rows, 1 per subject)
* with columns: subject_id, group, age, gender, score_T1, score_T2, score_T3, score_T4

TITLE '=========== Test 5a ==========='.

* Test 5a: All 4 timepoints (classic repeated measures Friedman)
SUBTITLE 'Friedman: score_T1 to score_T4 (longitudinal, ungrouped)'.
NPAR TESTS
  /FRIEDMAN=score_T1 score_T2 score_T3 score_T4
  /MISSING LISTWISE.

TITLE '=========== Test 5b ==========='.

* Test 5b: By treatment group
SUBTITLE 'Friedman: score_T1 to score_T4 (longitudinal, grouped by treatment)'.
SORT CASES BY group.
SPLIT FILE BY group.

NPAR TESTS
  /FRIEDMAN=score_T1 score_T2 score_T3 score_T4
  /MISSING LISTWISE.

SPLIT FILE OFF.

OMSEND.

EXECUTE.

* ============================================================================.
* NOTES FOR VALIDATION
* ============================================================================.
* Key values to extract from SPSS output:
*
* Ranks Table:
*   - Mean Rank for each variable/condition
*
* Test Statistics Table:
*   - N (number of valid cases)
*   - Chi-Square (Friedman test statistic)
*   - df (number of conditions - 1)
*   - Asymp. Sig. (p-value)
*
* Tolerances for R tests:
*   - Chi-Square: +/-0.01
*   - df: exact match
*   - p-value: +/-0.0001
*   - Mean Ranks: +/-0.01
*   - N: exact match (unweighted), +/-1 (weighted)
*
* Important SPSS Behavior:
*   - Friedman test ranks within each subject across conditions
*   - Requires at least 3 related measures (conditions)
*   - LISTWISE deletion: cases excluded if ANY variable has missing
*   - Chi-Square uses asymptotic approximation
*   - Ties are averaged within subjects
*   - WEIGHT BY treats weights as frequency weights
*
* Survey Data Tests (Part A):
*   - Trust items (3 measures): trust_government, trust_media, trust_science
*     All on same 1-5 Likert scale, conceptually related
*   - Attitude items (3 measures): life_satisfaction, environmental_concern,
*     political_orientation -- same scale but less conceptually coherent
*   - Same respondent provides all ratings (within-subject design)
*
* Longitudinal Data Tests (Part B):
*   - 4 timepoints (score_T1 through score_T4)
*   - 120 subjects, classic repeated measures design
*   - No weight variable available
*   - Uses longitudinal_data_wide.sav (wide format)
*   - Treatment group for grouped analysis (separate test per group)
*
* Relationship to other tests:
*   - Friedman is the non-parametric alternative to repeated-measures ANOVA
*   - For k=2 conditions, Friedman reduces to the sign test
*   - Post-hoc pairwise comparisons typically done with Wilcoxon tests
*     (with Bonferroni correction)
*
* Weighted Analyses:
*   - Weighted observations affect rank computation
*   - Chi-Square and p-value adjusted for weighted sample
*
* Grouped Analyses:
*   - Separate Friedman test per region (East/West) or treatment group
*   - Ranks computed within each group independently
*   - Smaller N per group may affect power
* ============================================================================.
