* Encoding: UTF-8.
* SPSS Syntax for Testing
* Dataset: survey_data.sav

* Start Output Management System to save results as text

OMS
  /IF SUBTYPES=['Frequencies']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/outputs/frequencies_output.txt'.

COMPUTE original_order = $CASENUM.

TITLE '=========== UNWEIGHTED / UNGROUPED =========== '.

FREQUENCIES
  VARIABLES=life_satisfaction
  /STATISTICS=MEAN STDDEV SKEWNESS
  /ORDER=  ANALYSIS .

TITLE '=========== WEIGHTED / UNGROUPED =========== '.

WEIGHT BY sampling_weight.

FREQUENCIES
  VARIABLES=life_satisfaction
  /STATISTICS=MEAN STDDEV SKEWNESS
  /ORDER=  ANALYSIS .

WEIGHT OFF.

TITLE '=========== UNWEIGHTED / GROUPED =========== '.

SORT CASES BY region.
SPLIT FILE BY region.

FREQUENCIES
  VARIABLES=life_satisfaction
  /STATISTICS=MEAN STDDEV SKEWNESS
  /ORDER=  ANALYSIS .

TITLE '=========== WEIGHTED / GROUPED =========== '.

WEIGHT BY sampling_weight.

FREQUENCIES
  VARIABLES=life_satisfaction
  /STATISTICS=MEAN STDDEV SKEWNESS
  /ORDER=  ANALYSIS .

WEIGHT OFF.

SPLIT FILE OFF.
SORT CASES BY original_order.

OMSEND.

EXECUTE.

