* Encoding: UTF-8.
* SPSS Syntax for Reliability Analysis Validation
* Dataset: survey_data.sav
* Purpose: Validate reliability() function against SPSS RELIABILITY procedure
* Variables: trust_government, trust_media, trust_science (Trust Scale)
*            life_satisfaction, environmental_concern, political_orientation (Attitude Scale)
* Weight: sampling_weight
* Grouping: region

* Start Output Management System to save results as text

OMS
  /IF SUBTYPES=['Reliability Statistics' 'Item-Total Statistics' 'Item Statistics' 'Inter-Item Correlation Matrix' 'Scale Statistics' 'Summary Item Statistics']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/outputs/reliability_output.txt'.

COMPUTE original_order = $CASENUM.

* ============================================================================.
* TEST 1: UNWEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== UNWEIGHTED / UNGROUPED ==========='.

TITLE '=========== Test 1a ==========='.

* Test 1a: Trust Scale (3 items)
SUBTITLE 'Reliability Analysis: Trust Scale (unweighted, ungrouped)'.
RELIABILITY
  /VARIABLES=trust_government trust_media trust_science
  /SCALE('Trust Scale') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR
  /SUMMARY=TOTAL.

TITLE '=========== Test 1b ==========='.

* Test 1b: Attitude Scale (3 items)
SUBTITLE 'Reliability Analysis: Attitude Scale (unweighted, ungrouped)'.
RELIABILITY
  /VARIABLES=life_satisfaction environmental_concern political_orientation
  /SCALE('Attitude Scale') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR
  /SUMMARY=TOTAL.

TITLE '=========== Test 1c ==========='.

* Test 1c: Full Scale (6 items)
SUBTITLE 'Reliability Analysis: Full Scale (unweighted, ungrouped)'.
RELIABILITY
  /VARIABLES=trust_government trust_media trust_science life_satisfaction environmental_concern political_orientation
  /SCALE('Full Scale') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR
  /SUMMARY=TOTAL.

* ============================================================================.
* TEST 2: WEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== WEIGHTED / UNGROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 2a ==========='.

* Test 2a: Trust Scale (weighted)
SUBTITLE 'Reliability Analysis: Trust Scale (weighted, ungrouped)'.
RELIABILITY
  /VARIABLES=trust_government trust_media trust_science
  /SCALE('Trust Scale') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR
  /SUMMARY=TOTAL.

TITLE '=========== Test 2b ==========='.

* Test 2b: Attitude Scale (weighted)
SUBTITLE 'Reliability Analysis: Attitude Scale (weighted, ungrouped)'.
RELIABILITY
  /VARIABLES=life_satisfaction environmental_concern political_orientation
  /SCALE('Attitude Scale') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR
  /SUMMARY=TOTAL.

TITLE '=========== Test 2c ==========='.

* Test 2c: Full Scale (weighted)
SUBTITLE 'Reliability Analysis: Full Scale (weighted, ungrouped)'.
RELIABILITY
  /VARIABLES=trust_government trust_media trust_science life_satisfaction environmental_concern political_orientation
  /SCALE('Full Scale') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR
  /SUMMARY=TOTAL.

WEIGHT OFF.

* ============================================================================.
* TEST 3: UNWEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== UNWEIGHTED / GROUPED ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

TITLE '=========== Test 3a ==========='.

* Test 3a: Trust Scale (grouped by region)
SUBTITLE 'Reliability Analysis: Trust Scale (unweighted, grouped by region)'.
RELIABILITY
  /VARIABLES=trust_government trust_media trust_science
  /SCALE('Trust Scale') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR
  /SUMMARY=TOTAL.

TITLE '=========== Test 3b ==========='.

* Test 3b: Attitude Scale (grouped by region)
SUBTITLE 'Reliability Analysis: Attitude Scale (unweighted, grouped by region)'.
RELIABILITY
  /VARIABLES=life_satisfaction environmental_concern political_orientation
  /SCALE('Attitude Scale') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR
  /SUMMARY=TOTAL.

* ============================================================================.
* TEST 4: WEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== WEIGHTED / GROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 4a ==========='.

* Test 4a: Trust Scale (weighted, grouped by region)
SUBTITLE 'Reliability Analysis: Trust Scale (weighted, grouped by region)'.
RELIABILITY
  /VARIABLES=trust_government trust_media trust_science
  /SCALE('Trust Scale') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR
  /SUMMARY=TOTAL.

TITLE '=========== Test 4b ==========='.

* Test 4b: Attitude Scale (weighted, grouped by region)
SUBTITLE 'Reliability Analysis: Attitude Scale (weighted, grouped by region)'.
RELIABILITY
  /VARIABLES=life_satisfaction environmental_concern political_orientation
  /SCALE('Attitude Scale') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR
  /SUMMARY=TOTAL.

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
* Reliability Statistics:
*   - Cronbach's Alpha (unstandardized)
*   - Cronbach's Alpha Based on Standardized Items
*   - N of Items
*
* Item Statistics (for each item):
*   - Mean
*   - Std. Deviation
*   - N
*
* Inter-Item Correlation Matrix:
*   - Full pairwise correlation matrix between all items
*
* Item-Total Statistics (for each item):
*   - Scale Mean if Item Deleted
*   - Scale Variance if Item Deleted
*   - Corrected Item-Total Correlation
*   - Squared Multiple Correlation
*   - Cronbach's Alpha if Item Deleted
*
* Scale Statistics:
*   - Mean (sum of item means)
*   - Variance (total scale variance)
*   - Std. Deviation
*   - N of Items
*
* Summary Item Statistics:
*   - Mean, Minimum, Maximum, Range, Max/Min, Variance
*   - for: Item Means, Item Variances, Inter-Item Correlations
*
* Tolerances for R tests:
*   - Cronbach's Alpha: +/-0.001
*   - Means: +/-0.01
*   - Standard Deviations: +/-0.01
*   - Correlations: +/-0.001
*   - Variances: +/-0.01
*   - N (unweighted): exact match
*   - N (weighted): +/-1
*
* Weighted Analyses:
*   - SPSS RELIABILITY respects WEIGHT BY command
*   - Weighted covariance matrix used for alpha calculation
*   - Weighted N replaces unweighted N in all tables
*   - Item statistics use weighted means and SDs
*
* Grouped Analyses:
*   - Separate reliability analysis for each region (East/West)
*   - Alpha may differ between groups due to variance differences
*   - Smaller sample sizes per group may affect stability
* ============================================================================.
