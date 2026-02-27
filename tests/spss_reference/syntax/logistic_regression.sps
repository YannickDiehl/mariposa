* Encoding: UTF-8.
* SPSS Syntax for Logistic Regression Validation
* Dataset: survey_data.sav
* Purpose: Validate logistic_regression() function against SPSS LOGISTIC REGRESSION procedure
* DVs: high_satisfaction (recoded from life_satisfaction), female (recoded from gender)
* IVs: age, income, trust_government, trust_media, trust_science,
*       education, environmental_concern, political_orientation
* Weight: sampling_weight
* Grouping: region

* ============================================================================.
* DATA PREPARATION: Create binary dependent variables
* ============================================================================.

* Recode life_satisfaction into binary: 1-3 = 0 (Low), 4-5 = 1 (High).
RECODE life_satisfaction (1 thru 3=0)(4 thru 5=1) INTO high_satisfaction.
VARIABLE LABELS high_satisfaction 'High Life Satisfaction (0=No, 1=Yes)'.
VALUE LABELS high_satisfaction 0 'Low' 1 'High'.
EXECUTE.

* Recode gender into numeric binary: Male = 0, Female = 1.
RECODE gender ('Male'=0)('Female'=1) INTO female.
VARIABLE LABELS female 'Female (0=Male, 1=Female)'.
EXECUTE.

* Start Output Management System to save results as text

OMS
  /IF SUBTYPES=['Variables in the Equation' 'Model Summary' 'Classification Table' 'Hosmer and Lemeshow Test' 'Omnibus Tests of Model Coefficients']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/outputs/logistic_regression_output.txt'.

COMPUTE original_order = $CASENUM.

* ============================================================================.
* TEST 1: UNWEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== UNWEIGHTED / UNGROUPED ==========='.

TITLE '=========== Test 1a ==========='.

* Test 1a: Bivariate logistic regression - high_satisfaction ~ age
SUBTITLE 'Logistic Regression: high_satisfaction ~ age (unweighted, ungrouped)'.
LOGISTIC REGRESSION VARIABLES high_satisfaction
  /METHOD=ENTER age
  /PRINT=GOODFIT CI(95)
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).

TITLE '=========== Test 1b ==========='.

* Test 1b: Multiple logistic regression - high_satisfaction ~ trust items
SUBTITLE 'Logistic Regression: high_satisfaction ~ trust items (unweighted, ungrouped)'.
LOGISTIC REGRESSION VARIABLES high_satisfaction
  /METHOD=ENTER trust_government trust_media trust_science
  /PRINT=GOODFIT CI(95)
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).

TITLE '=========== Test 1c ==========='.

* Test 1c: Multiple logistic regression - high_satisfaction ~ 4 diverse predictors
SUBTITLE 'Logistic Regression: high_satisfaction ~ 4 predictors (unweighted, ungrouped)'.
LOGISTIC REGRESSION VARIABLES high_satisfaction
  /METHOD=ENTER age income environmental_concern political_orientation
  /PRINT=GOODFIT CI(95)
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).

TITLE '=========== Test 1d ==========='.

* Test 1d: Different DV - female ~ age + income + education
SUBTITLE 'Logistic Regression: female ~ age + income + education (unweighted, ungrouped)'.
LOGISTIC REGRESSION VARIABLES female
  /METHOD=ENTER age income education
  /PRINT=GOODFIT CI(95)
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).

* ============================================================================.
* TEST 2: WEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== WEIGHTED / UNGROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 2a ==========='.

* Test 2a: Bivariate logistic regression (weighted)
SUBTITLE 'Logistic Regression: high_satisfaction ~ age (weighted, ungrouped)'.
LOGISTIC REGRESSION VARIABLES high_satisfaction
  /METHOD=ENTER age
  /PRINT=GOODFIT CI(95)
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).

TITLE '=========== Test 2b ==========='.

* Test 2b: Multiple logistic regression - trust items (weighted)
SUBTITLE 'Logistic Regression: high_satisfaction ~ trust items (weighted, ungrouped)'.
LOGISTIC REGRESSION VARIABLES high_satisfaction
  /METHOD=ENTER trust_government trust_media trust_science
  /PRINT=GOODFIT CI(95)
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).

TITLE '=========== Test 2c ==========='.

* Test 2c: Multiple logistic regression - 4 predictors (weighted)
SUBTITLE 'Logistic Regression: high_satisfaction ~ 4 predictors (weighted, ungrouped)'.
LOGISTIC REGRESSION VARIABLES high_satisfaction
  /METHOD=ENTER age income environmental_concern political_orientation
  /PRINT=GOODFIT CI(95)
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).

WEIGHT OFF.

* ============================================================================.
* TEST 3: UNWEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== UNWEIGHTED / GROUPED ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

TITLE '=========== Test 3a ==========='.

* Test 3a: Bivariate logistic regression (grouped by region)
SUBTITLE 'Logistic Regression: high_satisfaction ~ age (unweighted, grouped by region)'.
LOGISTIC REGRESSION VARIABLES high_satisfaction
  /METHOD=ENTER age
  /PRINT=GOODFIT CI(95)
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).

TITLE '=========== Test 3b ==========='.

* Test 3b: Multiple logistic regression - trust items (grouped by region)
SUBTITLE 'Logistic Regression: high_satisfaction ~ trust items (unweighted, grouped by region)'.
LOGISTIC REGRESSION VARIABLES high_satisfaction
  /METHOD=ENTER trust_government trust_media trust_science
  /PRINT=GOODFIT CI(95)
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).

* ============================================================================.
* TEST 4: WEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== WEIGHTED / GROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 4a ==========='.

* Test 4a: Bivariate logistic regression (weighted, grouped by region)
SUBTITLE 'Logistic Regression: high_satisfaction ~ age (weighted, grouped by region)'.
LOGISTIC REGRESSION VARIABLES high_satisfaction
  /METHOD=ENTER age
  /PRINT=GOODFIT CI(95)
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).

TITLE '=========== Test 4b ==========='.

* Test 4b: Multiple logistic regression - trust items (weighted, grouped by region)
SUBTITLE 'Logistic Regression: high_satisfaction ~ trust items (weighted, grouped by region)'.
LOGISTIC REGRESSION VARIABLES high_satisfaction
  /METHOD=ENTER trust_government trust_media trust_science
  /PRINT=GOODFIT CI(95)
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).

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
* Omnibus Tests of Model Coefficients:
*   - Chi-square (Model row)
*   - df
*   - Sig. (p-value for overall model test)
*
* Model Summary:
*   - -2 Log likelihood
*   - Cox & Snell R Square
*   - Nagelkerke R Square
*
* Hosmer and Lemeshow Test:
*   - Chi-square
*   - df
*   - Sig.
*
* Classification Table:
*   - Correct predictions for category 0 (Low)
*   - Correct predictions for category 1 (High)
*   - Overall Percentage correct
*
* Variables in the Equation (for each predictor + Constant):
*   - B (logistic regression coefficient)
*   - S.E. (standard error)
*   - Wald (Wald chi-square statistic)
*   - df (always 1 for individual predictors)
*   - Sig. (p-value)
*   - Exp(B) (odds ratio)
*   - 95% C.I. for EXP(B): Lower, Upper
*
* Tolerances for R tests:
*   - B coefficients: +/-0.001
*   - Standard Errors: +/-0.001
*   - Wald statistics: +/-0.01
*   - p-values: +/-0.0001
*   - Exp(B) / Odds Ratios: +/-0.001
*   - CI for Exp(B): +/-0.001
*   - -2 Log Likelihood: +/-0.01
*   - Cox & Snell R Square: +/-0.001
*   - Nagelkerke R Square: +/-0.001
*   - Hosmer-Lemeshow Chi-square: +/-0.01
*   - Classification percentages: +/-0.1
*   - Degrees of freedom: exact match
*   - Omnibus Chi-square: +/-0.01
*
* Data Preparation:
*   - high_satisfaction: RECODE life_satisfaction (1 thru 3=0)(4 thru 5=1)
*   - female: RECODE gender ('Male'=0)('Female'=1)
*   - RECODE must run BEFORE the analysis
*   - Cases with missing life_satisfaction will have SYSMIS for high_satisfaction
*
* McFadden R-squared (not in standard SPSS output):
*   - McFadden = 1 - (LL_model / LL_null)
*   - LL_null from Block 0 (constant-only model): -2LL_null / -2
*   - LL_model from final model: -2LL_model / -2
*   - Must be computed manually from -2LL values
*
* Important SPSS Behavior:
*   - ENTER method: all predictors enter simultaneously
*   - CUT(.5): classification threshold at 0.5
*   - ITERATE(20): maximum iterations for convergence
*   - GOODFIT: produces Hosmer-Lemeshow goodness-of-fit test
*   - CI(95): produces 95% confidence intervals for odds ratios
*   - Missing data: listwise deletion by default
*   - WEIGHT BY multiplies case contributions to the likelihood
*
* Weighted Analyses:
*   - Weighted N affects -2LL and chi-square statistics
*   - Odds ratio interpretation unchanged but standard errors differ
*   - Classification counts based on weighted observations
*
* Grouped Analyses:
*   - Separate logistic regression for each region (East/West)
*   - Base rates (proportion of 1s) may differ between groups
*   - Classification accuracy depends on group-specific distributions
* ============================================================================.
