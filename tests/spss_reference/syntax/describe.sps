* Encoding: UTF-8.
* SPSS Syntax for Describe Function Validation
* Dataset: survey_data.sav
* Purpose: Validate describe() function against SPSS results

* Start Output Management System to save results as text

OMS
  /IF SUBTYPES=['Descriptive Statistics' 'Statistics']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/outputs/describe_output.txt'.

COMPUTE original_order = $CASENUM.

* ================================================
* TEST 1: UNWEIGHTED / UNGROUPED
* ================================================

TITLE '=========== UNWEIGHTED / UNGROUPED ==========='.

TITLE '=========== Test 1a ==========='.

* Test 1a: Age
SUBTITLE 'Descriptive Statistics for Age'.
FREQUENCIES VARIABLES=age
  /FORMAT=NOTABLE
  /STATISTICS=MEAN MEDIAN MODE STDDEV VARIANCE RANGE MINIMUM MAXIMUM
    SEMEAN SKEWNESS KURTOSIS
  /PERCENTILES=25 50 75.

TITLE '=========== Test 1b ==========='.

* Test 1b: Income
SUBTITLE 'Descriptive Statistics for Income'.
FREQUENCIES VARIABLES=income
  /FORMAT=NOTABLE
  /STATISTICS=MEAN MEDIAN MODE STDDEV VARIANCE RANGE MINIMUM MAXIMUM
    SEMEAN SKEWNESS KURTOSIS
  /PERCENTILES=25 50 75.

TITLE '=========== Test 1c ==========='.

* Test 1c: Life Satisfaction
SUBTITLE 'Descriptive Statistics for Life Satisfaction'.
FREQUENCIES VARIABLES=life_satisfaction
  /FORMAT=NOTABLE
  /STATISTICS=MEAN MEDIAN MODE STDDEV VARIANCE RANGE MINIMUM MAXIMUM
    SEMEAN SKEWNESS KURTOSIS
  /PERCENTILES=25 50 75.

* ================================================
* TEST 2: WEIGHTED / UNGROUPED
* ================================================

TITLE '=========== WEIGHTED / UNGROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 2a ==========='.

* Test 2a: Age (weighted)
SUBTITLE 'Descriptive Statistics for Age (weighted)'.
FREQUENCIES VARIABLES=age
  /FORMAT=NOTABLE
  /STATISTICS=MEAN MEDIAN MODE STDDEV VARIANCE RANGE MINIMUM MAXIMUM
    SEMEAN SKEWNESS KURTOSIS
  /PERCENTILES=25 50 75.

TITLE '=========== Test 2b ==========='.

* Test 2b: Income (weighted)
SUBTITLE 'Descriptive Statistics for Income (weighted)'.
FREQUENCIES VARIABLES=income
  /FORMAT=NOTABLE
  /STATISTICS=MEAN MEDIAN MODE STDDEV VARIANCE RANGE MINIMUM MAXIMUM
    SEMEAN SKEWNESS KURTOSIS
  /PERCENTILES=25 50 75.

TITLE '=========== Test 2c ==========='.

* Test 2c: Life Satisfaction (weighted)
SUBTITLE 'Descriptive Statistics for Life Satisfaction (weighted)'.
FREQUENCIES VARIABLES=life_satisfaction
  /FORMAT=NOTABLE
  /STATISTICS=MEAN MEDIAN MODE STDDEV VARIANCE RANGE MINIMUM MAXIMUM
    SEMEAN SKEWNESS KURTOSIS
  /PERCENTILES=25 50 75.

WEIGHT OFF.

* ================================================
* TEST 3: UNWEIGHTED / GROUPED
* ================================================

TITLE '=========== UNWEIGHTED / GROUPED ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

TITLE '=========== Test 3a ==========='.

* Test 3a: Age (grouped by region)
SUBTITLE 'Descriptive Statistics for Age (grouped by region)'.
FREQUENCIES VARIABLES=age
  /FORMAT=NOTABLE
  /STATISTICS=MEAN MEDIAN MODE STDDEV VARIANCE RANGE MINIMUM MAXIMUM
    SEMEAN SKEWNESS KURTOSIS
  /PERCENTILES=25 50 75.

TITLE '=========== Test 3b ==========='.

* Test 3b: Income (grouped by region)
SUBTITLE 'Descriptive Statistics for Income (grouped by region)'.
FREQUENCIES VARIABLES=income
  /FORMAT=NOTABLE
  /STATISTICS=MEAN MEDIAN MODE STDDEV VARIANCE RANGE MINIMUM MAXIMUM
    SEMEAN SKEWNESS KURTOSIS
  /PERCENTILES=25 50 75.

TITLE '=========== Test 3c ==========='.

* Test 3c: Life Satisfaction (grouped by region)
SUBTITLE 'Descriptive Statistics for Life Satisfaction (grouped by region)'.
FREQUENCIES VARIABLES=life_satisfaction
  /FORMAT=NOTABLE
  /STATISTICS=MEAN MEDIAN MODE STDDEV VARIANCE RANGE MINIMUM MAXIMUM
    SEMEAN SKEWNESS KURTOSIS
  /PERCENTILES=25 50 75.

* ================================================
* TEST 4: WEIGHTED / GROUPED
* ================================================

TITLE '=========== WEIGHTED / GROUPED ==========='.

WEIGHT BY sampling_weight.

* Test 4a: Age (weighted, grouped by region)
SUBTITLE 'Descriptive Statistics for Age (weighted, grouped by region)'.

TITLE '=========== Test 4a ==========='.

FREQUENCIES VARIABLES=age
  /FORMAT=NOTABLE
  /STATISTICS=MEAN MEDIAN MODE STDDEV VARIANCE RANGE MINIMUM MAXIMUM
    SEMEAN SKEWNESS KURTOSIS
  /PERCENTILES=25 50 75.

TITLE '=========== Test 4b ==========='.

* Test 4b: Income (weighted, grouped by region)
SUBTITLE 'Descriptive Statistics for Income (weighted, grouped by region)'.
FREQUENCIES VARIABLES=income
  /FORMAT=NOTABLE
  /STATISTICS=MEAN MEDIAN MODE STDDEV VARIANCE RANGE MINIMUM MAXIMUM
    SEMEAN SKEWNESS KURTOSIS
  /PERCENTILES=25 50 75.

TITLE '=========== Test 4c ==========='.

* Test 4c: Life Satisfaction (weighted, grouped by region)
SUBTITLE 'Descriptive Statistics for Life Satisfaction (weighted, grouped by region)'.
FREQUENCIES VARIABLES=life_satisfaction
  /FORMAT=NOTABLE
  /STATISTICS=MEAN MEDIAN MODE STDDEV VARIANCE RANGE MINIMUM MAXIMUM
    SEMEAN SKEWNESS KURTOSIS
  /PERCENTILES=25 50 75.

WEIGHT OFF.
SPLIT FILE OFF.
SORT CASES BY original_order.

OMSEND.

EXECUTE.