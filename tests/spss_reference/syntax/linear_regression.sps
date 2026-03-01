* Encoding: UTF-8.
* SPSS Syntax for Linear Regression Validation
* Dataset: survey_data.sav
* Purpose: Validate linear_regression() function against SPSS REGRESSION procedure
* DVs: life_satisfaction, income
* IVs: age, trust_government, trust_media, trust_science, education,
*       environmental_concern, political_orientation
* Weight: sampling_weight
* Grouping: region

* Start Output Management System to save results as text

OMS
  /IF SUBTYPES=['Coefficients' 'ANOVA' 'Model Summary' 'Descriptive Statistics' 'Correlations']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/outputs/linear_regression_output.txt'.

COMPUTE original_order = $CASENUM.

* ============================================================================.
* TEST 1: UNWEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== UNWEIGHTED / UNGROUPED ==========='.

TITLE '=========== Test 1a ==========='.

* Test 1a: Bivariate regression - life_satisfaction ~ age
SUBTITLE 'Linear Regression: life_satisfaction ~ age (unweighted, ungrouped)'.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /DESCRIPTIVES
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT life_satisfaction
  /METHOD=ENTER age.

TITLE '=========== Test 1b ==========='.

* Test 1b: Multiple regression - life_satisfaction ~ trust items
SUBTITLE 'Linear Regression: life_satisfaction ~ trust items (unweighted, ungrouped)'.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /DESCRIPTIVES
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT life_satisfaction
  /METHOD=ENTER trust_government trust_media trust_science.

TITLE '=========== Test 1c ==========='.

* Test 1c: Multiple regression - income ~ age + education + life_satisfaction
SUBTITLE 'Linear Regression: income ~ age + education + life_satisfaction (unweighted, ungrouped)'.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /DESCRIPTIVES
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT income
  /METHOD=ENTER age education life_satisfaction.

TITLE '=========== Test 1d ==========='.

* Test 1d: Multiple regression - life_satisfaction ~ 4 predictors
SUBTITLE 'Linear Regression: life_satisfaction ~ 4 predictors (unweighted, ungrouped)'.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /DESCRIPTIVES
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT life_satisfaction
  /METHOD=ENTER age environmental_concern political_orientation trust_government.

* ============================================================================.
* TEST 2: WEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== WEIGHTED / UNGROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 2a ==========='.

* Test 2a: Bivariate regression (weighted)
SUBTITLE 'Linear Regression: life_satisfaction ~ age (weighted, ungrouped)'.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /DESCRIPTIVES
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT life_satisfaction
  /METHOD=ENTER age.

TITLE '=========== Test 2b ==========='.

* Test 2b: Multiple regression - trust items (weighted)
SUBTITLE 'Linear Regression: life_satisfaction ~ trust items (weighted, ungrouped)'.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /DESCRIPTIVES
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT life_satisfaction
  /METHOD=ENTER trust_government trust_media trust_science.

TITLE '=========== Test 2c ==========='.

* Test 2c: Multiple regression - income model (weighted)
SUBTITLE 'Linear Regression: income ~ age + education + life_satisfaction (weighted, ungrouped)'.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /DESCRIPTIVES
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT income
  /METHOD=ENTER age education life_satisfaction.

WEIGHT OFF.

* ============================================================================.
* TEST 3: UNWEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== UNWEIGHTED / GROUPED ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

TITLE '=========== Test 3a ==========='.

* Test 3a: Bivariate regression (grouped by region)
SUBTITLE 'Linear Regression: life_satisfaction ~ age (unweighted, grouped by region)'.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /DESCRIPTIVES
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT life_satisfaction
  /METHOD=ENTER age.

TITLE '=========== Test 3b ==========='.

* Test 3b: Multiple regression - trust items (grouped by region)
SUBTITLE 'Linear Regression: life_satisfaction ~ trust items (unweighted, grouped by region)'.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /DESCRIPTIVES
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT life_satisfaction
  /METHOD=ENTER trust_government trust_media trust_science.

* ============================================================================.
* TEST 4: WEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== WEIGHTED / GROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 4a ==========='.

* Test 4a: Bivariate regression (weighted, grouped by region)
SUBTITLE 'Linear Regression: life_satisfaction ~ age (weighted, grouped by region)'.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /DESCRIPTIVES
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT life_satisfaction
  /METHOD=ENTER age.

TITLE '=========== Test 4b ==========='.

* Test 4b: Multiple regression - trust items (weighted, grouped by region)
SUBTITLE 'Linear Regression: life_satisfaction ~ trust items (weighted, grouped by region)'.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /DESCRIPTIVES
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT life_satisfaction
  /METHOD=ENTER trust_government trust_media trust_science.

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
* Model Summary:
*   - R (multiple correlation coefficient)
*   - R Square
*   - Adjusted R Square
*   - Std. Error of the Estimate
*
* ANOVA Table:
*   - Sum of Squares (Regression, Residual, Total)
*   - df (Regression = k predictors, Residual = n-k-1, Total = n-1)
*   - Mean Square (Regression, Residual)
*   - F statistic
*   - Sig. (p-value for overall model F-test)
*
* Coefficients Table:
*   - B (unstandardized regression coefficient) for (Constant) and each IV
*   - Std. Error for each coefficient
*   - Beta (standardized coefficient) -- NOT available for (Constant)
*   - t statistic for each coefficient
*   - Sig. (p-value for each coefficient)
*
* Descriptive Statistics:
*   - Mean, Std. Deviation, N for DV and all IVs
*
* Correlations:
*   - Pearson correlation matrix among DV and all IVs
*
* Tolerances for R tests:
*   - R, R Square, Adjusted R Square: +/-0.0001
*   - Std. Error of Estimate: +/-0.001
*   - B coefficients: +/-0.001
*   - Std. Error of coefficients: +/-0.001
*   - Beta (standardized): +/-0.001
*   - t statistics: +/-0.001
*   - F statistic: +/-0.001
*   - p-values: +/-0.0001
*   - Sum of Squares: +/-0.01
*   - Degrees of freedom: exact match
*   - Means: +/-0.01
*   - Standard Deviations: +/-0.01
*   - Correlations: +/-0.001
*
* Important SPSS Behavior:
*   - LISTWISE deletion: cases with any missing variable are excluded
*   - Standardized Beta = B * (sd_x / sd_y)
*   - ENTER method includes all predictors simultaneously
*   - education enters as numeric (1-4), not as dummy variables
*   - WEIGHT BY applies weighted least squares (WLS)
*   - Weighted N replaces N in Descriptive Statistics
*
* Grouped Analyses:
*   - Separate regression for each region (East/West)
*   - Coefficients and model fit differ by group
*   - Smaller sample sizes per group may affect significance
* ============================================================================.
