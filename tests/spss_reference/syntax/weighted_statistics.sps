* Encoding: UTF-8.
* SPSS Syntax for Weighted Statistics (w_* functions) Validation
* Dataset: survey_data.sav
* Purpose: Generate reference values for validating mariposa w_* functions
* Variables: age, income (numeric), sampling_weight (weight variable)
* Grouping: region

* Start Output Management System to save results as text

OMS
  /IF SUBTYPES=['Descriptive Statistics' 'Statistics']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/outputs/weighted_statistics_output.txt'.

COMPUTE original_order = $CASENUM.

* ============================================================================.
* SCENARIO 1: UNWEIGHTED, UNGROUPED
* ============================================================================.

TITLE '=========== UNWEIGHTED / UNGROUPED ==========='.

TITLE '=========== Test 1a ==========='.

* 1a. Descriptives (Mean, SD, Variance, SE, Range, Min, Max, Skewness, Kurtosis).
SUBTITLE 'Descriptives for age and income (unweighted, ungrouped)'.
DESCRIPTIVES VARIABLES=age income
  /STATISTICS=MEAN STDDEV VARIANCE SEMEAN RANGE MIN MAX SKEWNESS KURTOSIS.

TITLE '=========== Test 1b ==========='.

* 1b. Median, Quartiles, IQR, Mode.
SUBTITLE 'Frequencies for age and income (unweighted, ungrouped)'.
FREQUENCIES VARIABLES=age income
  /FORMAT=NOTABLE
  /PERCENTILES=25 50 75
  /STATISTICS=MEDIAN MODE.

* ============================================================================.
* SCENARIO 2: WEIGHTED, UNGROUPED
* ============================================================================.

TITLE '=========== WEIGHTED / UNGROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 2a ==========='.

* 2a. Descriptives (Mean, SD, Variance, SE, Range, Min, Max, Skewness, Kurtosis).
SUBTITLE 'Descriptives for age and income (weighted, ungrouped)'.
DESCRIPTIVES VARIABLES=age income
  /STATISTICS=MEAN STDDEV VARIANCE SEMEAN RANGE MIN MAX SKEWNESS KURTOSIS.

TITLE '=========== Test 2b ==========='.

* 2b. Median, Quartiles, IQR, Mode.
SUBTITLE 'Frequencies for age and income (weighted, ungrouped)'.
FREQUENCIES VARIABLES=age income
  /FORMAT=NOTABLE
  /PERCENTILES=25 50 75
  /STATISTICS=MEDIAN MODE.

WEIGHT OFF.

* ============================================================================.
* SCENARIO 3: UNWEIGHTED, GROUPED BY REGION
* ============================================================================.

TITLE '=========== UNWEIGHTED / GROUPED ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

TITLE '=========== Test 3a ==========='.

* 3a. Descriptives grouped by region.
SUBTITLE 'Descriptives for age and income (unweighted, grouped by region)'.
DESCRIPTIVES VARIABLES=age income
  /STATISTICS=MEAN STDDEV VARIANCE SEMEAN RANGE MIN MAX SKEWNESS KURTOSIS.

TITLE '=========== Test 3b ==========='.

* 3b. Frequencies grouped by region.
SUBTITLE 'Frequencies for age and income (unweighted, grouped by region)'.
FREQUENCIES VARIABLES=age income
  /FORMAT=NOTABLE
  /PERCENTILES=25 50 75
  /STATISTICS=MEDIAN MODE.

* ============================================================================.
* SCENARIO 4: WEIGHTED, GROUPED BY REGION
* ============================================================================.

TITLE '=========== WEIGHTED / GROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 4a ==========='.

* 4a. Descriptives grouped by region (weighted).
SUBTITLE 'Descriptives for age and income (weighted, grouped by region)'.
DESCRIPTIVES VARIABLES=age income
  /STATISTICS=MEAN STDDEV VARIANCE SEMEAN RANGE MIN MAX SKEWNESS KURTOSIS.

TITLE '=========== Test 4b ==========='.

* 4b. Frequencies grouped by region (weighted).
SUBTITLE 'Frequencies for age and income (weighted, grouped by region)'.
FREQUENCIES VARIABLES=age income
  /FORMAT=NOTABLE
  /PERCENTILES=25 50 75
  /STATISTICS=MEDIAN MODE.

SPLIT FILE OFF.
WEIGHT OFF.
SORT CASES BY original_order.

OMSEND.

EXECUTE.

* ============================================================================.
* NOTES FOR RECORDING VALUES
* ============================================================================.
* From DESCRIPTIVES output:
*   - Mean          -> w_mean() reference
*   - Std. Deviation -> w_sd() reference
*   - Variance      -> w_var() reference
*   - Std. Error    -> w_se() reference
*   - Range         -> w_range() reference
*   - Minimum       -> w_range() min reference
*   - Maximum       -> w_range() max reference
*   - Skewness      -> w_skew() reference
*   - Kurtosis      -> w_kurtosis() reference (SPSS uses excess kurtosis)
*
* From FREQUENCIES output:
*   - Median (50th percentile) -> w_median() reference
*   - 25th percentile -> w_quantile(probs=0.25) reference
*   - 75th percentile -> w_quantile(probs=0.75) reference
*   - IQR = Q3 - Q1   -> w_iqr() reference
*   - Mode             -> w_modus() reference
*
* Tolerances to use in R tests:
*   - Mean, SD, Var: +/-0.01
*   - SE: +/-0.001
*   - Median, Quartiles: +/-0.5 (weighted median algorithms may differ)
*   - Skewness, Kurtosis: +/-0.01
*   - Range, Min, Max: exact match
*   - Mode: exact match
*   - IQR: +/-0.5
* ============================================================================.
