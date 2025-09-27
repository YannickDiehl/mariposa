* Encoding: UTF-8.
* SPSS Syntax for Spearman's Rho Correlation Test Validation
* Dataset: survey_data.sav
* Purpose: Validate spearman_rho() function against SPSS results
* Created: 2025-01-27
* SPSS Version: 29.0.0.0

* Start Output Management System to save results as text

OMS
  /IF SUBTYPES=['Nonparametric Correlations']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/outputs/spearman_rho_output.txt'.

COMPUTE original_order = $CASENUM.

* ================================================
* TEST 1: UNWEIGHTED / UNGROUPED
* ================================================

TITLE '=========== UNWEIGHTED / UNGROUPED ==========='.

TITLE '=========== Test 1a ==========='.

* Test 1a: Life satisfaction with political orientation and trust media
SUBTITLE 'Spearman Rho: Life satisfaction, Political orientation, Trust media'.
NONPAR CORR
  /VARIABLES=life_satisfaction political_orientation trust_media
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

TITLE '=========== Test 1b ==========='.

* Test 1b: Income with age and life satisfaction
SUBTITLE 'Spearman Rho: Income, Age, Life satisfaction'.
NONPAR CORR
  /VARIABLES=income age life_satisfaction
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

TITLE '=========== Test 1c ==========='.

* Test 1c: All trust variables together
SUBTITLE 'Spearman Rho: Trust government, Trust media, Trust science'.
NONPAR CORR
  /VARIABLES=trust_government trust_media trust_science
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

TITLE '=========== Test 1d ==========='.

* Test 1d: Political orientation with trust variables
SUBTITLE 'Spearman Rho: Political orientation with trust variables'.
NONPAR CORR
  /VARIABLES=political_orientation trust_government trust_media trust_science
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

TITLE '=========== Test 1e ==========='.

* Test 1e: Income and age (simple two-variable correlation)
SUBTITLE 'Spearman Rho: Income and Age'.
NONPAR CORR
  /VARIABLES=income age
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

TITLE '=========== Test 1f ==========='.

* Test 1f: Large set of variables for comprehensive testing
SUBTITLE 'Spearman Rho: Comprehensive variable set'.
NONPAR CORR
  /VARIABLES=life_satisfaction income age political_orientation environmental_concern
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

* ================================================
* TEST 2: WEIGHTED / UNGROUPED
* ================================================

TITLE '=========== WEIGHTED / UNGROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 2a ==========='.

* Test 2a: Life satisfaction with political orientation and trust media (weighted)
SUBTITLE 'Spearman Rho: Life satisfaction, Political orientation, Trust media (weighted)'.
NONPAR CORR
  /VARIABLES=life_satisfaction political_orientation trust_media
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

TITLE '=========== Test 2b ==========='.

* Test 2b: Income with age and life satisfaction (weighted)
SUBTITLE 'Spearman Rho: Income, Age, Life satisfaction (weighted)'.
NONPAR CORR
  /VARIABLES=income age life_satisfaction
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

TITLE '=========== Test 2c ==========='.

* Test 2c: All trust variables together (weighted)
SUBTITLE 'Spearman Rho: Trust government, Trust media, Trust science (weighted)'.
NONPAR CORR
  /VARIABLES=trust_government trust_media trust_science
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

TITLE '=========== Test 2d ==========='.

* Test 2d: Political orientation with trust variables (weighted)
SUBTITLE 'Spearman Rho: Political orientation with trust variables (weighted)'.
NONPAR CORR
  /VARIABLES=political_orientation trust_government trust_media trust_science
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

TITLE '=========== Test 2e ==========='.

* Test 2e: Income and age (weighted, simple two-variable correlation)
SUBTITLE 'Spearman Rho: Income and Age (weighted)'.
NONPAR CORR
  /VARIABLES=income age
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

TITLE '=========== Test 2f ==========='.

* Test 2f: Large set of variables for comprehensive testing (weighted)
SUBTITLE 'Spearman Rho: Comprehensive variable set (weighted)'.
NONPAR CORR
  /VARIABLES=life_satisfaction income age political_orientation environmental_concern
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

WEIGHT OFF.

* ================================================
* TEST 3: UNWEIGHTED / GROUPED
* ================================================

TITLE '=========== UNWEIGHTED / GROUPED ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

TITLE '=========== Test 3a ==========='.

* Test 3a: Life satisfaction with political orientation and trust media (grouped by region)
SUBTITLE 'Spearman Rho: Life satisfaction, Political orientation, Trust media (grouped by region)'.
NONPAR CORR
  /VARIABLES=life_satisfaction political_orientation trust_media
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

TITLE '=========== Test 3b ==========='.

* Test 3b: Income with age and life satisfaction (grouped by region)
SUBTITLE 'Spearman Rho: Income, Age, Life satisfaction (grouped by region)'.
NONPAR CORR
  /VARIABLES=income age life_satisfaction
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

TITLE '=========== Test 3c ==========='.

* Test 3c: All trust variables together (grouped by region)
SUBTITLE 'Spearman Rho: Trust variables (grouped by region)'.
NONPAR CORR
  /VARIABLES=trust_government trust_media trust_science
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

TITLE '=========== Test 3d ==========='.

* Test 3d: Income and age only (grouped by region)
SUBTITLE 'Spearman Rho: Income and Age (grouped by region)'.
NONPAR CORR
  /VARIABLES=income age
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

* ================================================
* TEST 4: WEIGHTED / GROUPED
* ================================================

TITLE '=========== WEIGHTED / GROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 4a ==========='.

* Test 4a: Life satisfaction with political orientation and trust media (weighted, grouped by region)
SUBTITLE 'Spearman Rho: Life satisfaction, Political orientation, Trust media (weighted, grouped by region)'.
NONPAR CORR
  /VARIABLES=life_satisfaction political_orientation trust_media
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

TITLE '=========== Test 4b ==========='.

* Test 4b: Income with age and life satisfaction (weighted, grouped by region)
SUBTITLE 'Spearman Rho: Income, Age, Life satisfaction (weighted, grouped by region)'.
NONPAR CORR
  /VARIABLES=income age life_satisfaction
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

TITLE '=========== Test 4c ==========='.

* Test 4c: All trust variables together (weighted, grouped by region)
SUBTITLE 'Spearman Rho: Trust variables (weighted, grouped by region)'.
NONPAR CORR
  /VARIABLES=trust_government trust_media trust_science
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

TITLE '=========== Test 4d ==========='.

* Test 4d: Income and age only (weighted, grouped by region)
SUBTITLE 'Spearman Rho: Income and Age (weighted, grouped by region)'.
NONPAR CORR
  /VARIABLES=income age
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

WEIGHT OFF.
SPLIT FILE OFF.
SORT CASES BY original_order.

* ================================================
* ADDITIONAL EDGE CASE TESTS
* ================================================

TITLE '=========== EDGE CASES ==========='.

TITLE '=========== Test 5a ==========='.

* Test 5a: Single pair correlation for validation simplicity
SUBTITLE 'Edge Case: Single pair correlation'.
NONPAR CORR
  /VARIABLES=life_satisfaction political_orientation
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

TITLE '=========== Test 5b ==========='.

* Test 5b: Listwise deletion comparison
SUBTITLE 'Edge Case: Listwise deletion'.
NONPAR CORR
  /VARIABLES=life_satisfaction political_orientation trust_media
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=LISTWISE.

TITLE '=========== Test 5c ==========='.

* Test 5c: One-tailed significance test
SUBTITLE 'Edge Case: One-tailed test'.
NONPAR CORR
  /VARIABLES=income age
  /PRINT=SPEARMAN ONETAIL NOSIG
  /MISSING=PAIRWISE.

OMSEND.

EXECUTE.