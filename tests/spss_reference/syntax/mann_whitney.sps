* Encoding: UTF-8.
* SPSS Syntax for Mann-Whitney U Test Validation
* Dataset: survey_data.sav
* Purpose: Validate mann_whitney() function against SPSS results
* Created: 2026-02-26
* SPSS Version: 29.0.0.0
*
* Variables tested: life_satisfaction, income, age
* Group variable: gender (1=Male, 2=Female, already numeric in .sav)
* Split file variable: region (East vs West)
* Weight variable: sampling_weight
*
* SPSS outputs for Mann-Whitney (NPAR TESTS /M-W):
*   Ranks table:     N, Mean Rank, Sum of Ranks (per group)
*   Test Statistics:  Mann-Whitney U, Wilcoxon W, Z, Asymp. Sig. (2-tailed)
*
* Note: SPSS does not output effect size r directly.
*   Calculate manually: r = |Z| / sqrt(N_total)
*
* Group order: gender(1 2) = Male vs Female
*   This matches R factor levels: levels(survey_data$gender) = c("Male","Female")

GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/data/survey_data.sav'.

* Save output to text file.
* Use COMMANDS filter instead of SUBTYPES to capture all NPAR Tests output.
OMS
  /IF COMMANDS=['NPAR Tests']
  /DESTINATION FORMAT=TEXT
   OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/outputs/mann_whitney_output.txt'.

COMPUTE original_order = $CASENUM.
EXECUTE.

* ================================================
* TEST 1: UNWEIGHTED / UNGROUPED
* ================================================

TITLE '=========== UNWEIGHTED / UNGROUPED ==========='.

TITLE '=========== Test 1a: life_satisfaction by gender ==========='.
NPAR TESTS
  /M-W= life_satisfaction BY gender(1 2)
  /MISSING ANALYSIS.

TITLE '=========== Test 1b: income by gender ==========='.
NPAR TESTS
  /M-W= income BY gender(1 2)
  /MISSING ANALYSIS.

TITLE '=========== Test 1c: age by gender ==========='.
NPAR TESTS
  /M-W= age BY gender(1 2)
  /MISSING ANALYSIS.

* ================================================
* TEST 2: WEIGHTED / UNGROUPED
* ================================================

TITLE '=========== WEIGHTED / UNGROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 2a: life_satisfaction by gender (weighted) ==========='.
NPAR TESTS
  /M-W= life_satisfaction BY gender(1 2)
  /MISSING ANALYSIS.

TITLE '=========== Test 2b: income by gender (weighted) ==========='.
NPAR TESTS
  /M-W= income BY gender(1 2)
  /MISSING ANALYSIS.

TITLE '=========== Test 2c: age by gender (weighted) ==========='.
NPAR TESTS
  /M-W= age BY gender(1 2)
  /MISSING ANALYSIS.

WEIGHT OFF.

* ================================================
* TEST 3: UNWEIGHTED / GROUPED (by region)
* ================================================

TITLE '=========== UNWEIGHTED / GROUPED ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

TITLE '=========== Test 3a: life_satisfaction by gender (grouped by region) ==========='.
NPAR TESTS
  /M-W= life_satisfaction BY gender(1 2)
  /MISSING ANALYSIS.

TITLE '=========== Test 3b: income by gender (grouped by region) ==========='.
NPAR TESTS
  /M-W= income BY gender(1 2)
  /MISSING ANALYSIS.

TITLE '=========== Test 3c: age by gender (grouped by region) ==========='.
NPAR TESTS
  /M-W= age BY gender(1 2)
  /MISSING ANALYSIS.

* ================================================
* TEST 4: WEIGHTED / GROUPED (by region)
* ================================================

TITLE '=========== WEIGHTED / GROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 4a: life_satisfaction by gender (weighted, grouped by region) ==========='.
NPAR TESTS
  /M-W= life_satisfaction BY gender(1 2)
  /MISSING ANALYSIS.

TITLE '=========== Test 4b: income by gender (weighted, grouped by region) ==========='.
NPAR TESTS
  /M-W= income BY gender(1 2)
  /MISSING ANALYSIS.

TITLE '=========== Test 4c: age by gender (weighted, grouped by region) ==========='.
NPAR TESTS
  /M-W= age BY gender(1 2)
  /MISSING ANALYSIS.

WEIGHT OFF.
SPLIT FILE OFF.
SORT CASES BY original_order.

OMSEND.

EXECUTE.

* ================================================
* NOTES FOR VALIDATION
* ================================================
*
* Gender coding in .sav file:
*   1 = Male, 2 = Female (numeric with value labels)
*   Matches R factor: levels(survey_data$gender) = c("Male", "Female")
*   So gender(1 2) => Group 1 = Male, Group 2 = Female
*
* Key values to extract from SPSS output per test:
*
* Ranks Table:
*   - Male (1):   N, Mean Rank, Sum of Ranks
*   - Female (2): N, Mean Rank, Sum of Ranks
*   - Total:      N
*
* Test Statistics Table:
*   - Mann-Whitney U
*   - Wilcoxon W
*   - Z
*   - Asymp. Sig. (2-tailed)
*
* Manual calculations needed:
*   - Effect size r = |Z| / sqrt(N_total)
*   - Rank mean difference = Mean_Rank_Male - Mean_Rank_Female
*
* Weighted analyses:
*   - SPSS applies frequency weights to ranks
*   - Weighted N may be decimal
*   - All rank statistics reflect weighted values
*
* Expected output structure per test:
*   12 test combinations (4 scenarios x 3 variables)
*   For grouped tests (3 & 4): separate output per region (East, West)
*   Total reference values: 6 ungrouped + 12 grouped = 18 result sets
*
* Output saved to:
*   tests/spss_reference/outputs/mann_whitney_output.txt
