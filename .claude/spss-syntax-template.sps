* Encoding: UTF-8.
* SPSS Syntax for Testing
* Dataset: survey_data.sav

* Start Output Management System to save results as text

OMS
  /IF SUBTYPES=['###']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/outputs/pearson_cor_output.txt'.

COMPUTE original_order = $CASENUM.

TITLE '=========== UNWEIGHTED / UNGROUPED =========== '.

####

TITLE '=========== WEIGHTED / UNGROUPED =========== '.

WEIGHT BY sampling_weight.

####

WEIGHT OFF.

TITLE '=========== UNWEIGHTED / GROUPED =========== '.

SORT CASES BY region.
SPLIT FILE BY region.

###

TITLE '=========== WEIGHTED / GROUPED =========== '.

WEIGHT BY sampling_weight.

###

WEIGHT OFF.

SPLIT FILE BY region.
SORT CASES BY original_order.

OMSEND.

EXECUTE.

