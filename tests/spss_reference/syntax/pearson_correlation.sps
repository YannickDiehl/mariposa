* Encoding: UTF-8.
* SPSS Syntax for Testing
* Dataset: survey_data.sav

* Start Output Management System to save results as text

OMS
  /IF SUBTYPES=['Correlations']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/outputs/pearson_cor_output.txt'.

COMPUTE original_order = $CASENUM.

TITLE '=========== UNWEIGHTED / UNGROUPED =========== '.

CORRELATIONS
  /VARIABLES=life_satisfaction political_orientation trust_media
  /PRINT=TWOTAIL NOSIG .

TITLE '=========== WEIGHTED / UNGROUPED =========== '.

WEIGHT BY sampling_weight.

CORRELATIONS
  /VARIABLES=life_satisfaction political_orientation trust_media 
  /PRINT=TWOTAIL NOSIG .

WEIGHT OFF.

TITLE '=========== UNWEIGHTED / GROUPED =========== '.

SORT CASES BY region.
SPLIT FILE BY region.

CORRELATIONS
  /VARIABLES=life_satisfaction political_orientation trust_media
  /PRINT=TWOTAIL NOSIG .

TITLE '=========== WEIGHTED / GROUPED =========== '.

WEIGHT BY sampling_weight.

CORRELATIONS
  /VARIABLES=life_satisfaction political_orientation trust_media
  /PRINT=TWOTAIL NOSIG .

WEIGHT OFF.

SPLIT FILE OFF.
SORT CASES BY original_order.

OMSEND.

EXECUTE.
