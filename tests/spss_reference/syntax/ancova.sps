* Encoding: UTF-8.
* SPSS Syntax for ANCOVA Validation
* Dataset: survey_data.sav
* Purpose: Validate ancova() function against SPSS UNIANOVA procedure with covariates
* DVs: income, life_satisfaction
* Factors: gender (2 levels), region (2 levels), education (4 levels)
* Covariates: age, political_orientation
* Weight: sampling_weight (via /REGWGT for proper WLS weighting)
*
* IMPORTANT: UNIANOVA requires /REGWGT for WLS weighting, NOT WEIGHT BY.
* WEIGHT BY applies frequency replication weighting, which does not produce
* proper WLS estimates in the GLM framework. /REGWGT applies diagonal
* weight matrix W in the WLS estimation: (X'WX)^-1 X'Wy.

GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/data/survey_data.sav'.

* Start Output Management System to save results as text.

OMS
  /IF COMMANDS=['UNIANOVA']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/outputs/ancova_output.txt'.

COMPUTE original_order = $CASENUM.
EXECUTE.

* ============================================================================.
* TEST 1: UNWEIGHTED - ONE-WAY ANCOVA (1 factor + 1 covariate)
* ============================================================================.

TITLE '=========== UNWEIGHTED / ONE-WAY ANCOVA ==========='.

TITLE '=========== Test 1a ==========='.

* Test 1a: income BY education WITH age (unweighted)
* Classic ANCOVA: Does education affect income after controlling for age?
SUBTITLE 'ANCOVA: income BY education WITH age (unweighted)'.
UNIANOVA income BY education WITH age
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY PARAMETER
  /EMMEANS=TABLES(education) WITH(age=MEAN)
  /CRITERIA=ALPHA(.05)
  /DESIGN=age education.

TITLE '=========== Test 1b ==========='.

* Test 1b: life_satisfaction BY gender WITH age (unweighted)
* Simple 2-group ANCOVA with age as covariate
SUBTITLE 'ANCOVA: life_satisfaction BY gender WITH age (unweighted)'.
UNIANOVA life_satisfaction BY gender WITH age
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY PARAMETER
  /EMMEANS=TABLES(gender) WITH(age=MEAN)
  /CRITERIA=ALPHA(.05)
  /DESIGN=age gender.

TITLE '=========== Test 1c ==========='.

* Test 1c: life_satisfaction BY education WITH political_orientation (unweighted)
* Different covariate to test generality
SUBTITLE 'ANCOVA: life_satisfaction BY education WITH political_orientation (unweighted)'.
UNIANOVA life_satisfaction BY education WITH political_orientation
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY PARAMETER
  /EMMEANS=TABLES(education) WITH(political_orientation=MEAN)
  /CRITERIA=ALPHA(.05)
  /DESIGN=political_orientation education.

* ============================================================================.
* TEST 2: WEIGHTED - ONE-WAY ANCOVA (via /REGWGT for WLS)
* ============================================================================.

TITLE '=========== WEIGHTED / ONE-WAY ANCOVA ==========='.

TITLE '=========== Test 2a ==========='.

* Test 2a: income BY education WITH age (weighted)
SUBTITLE 'ANCOVA: income BY education WITH age (weighted)'.
UNIANOVA income BY education WITH age
  /REGWGT=sampling_weight
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY PARAMETER
  /EMMEANS=TABLES(education) WITH(age=MEAN)
  /CRITERIA=ALPHA(.05)
  /DESIGN=age education.

TITLE '=========== Test 2b ==========='.

* Test 2b: life_satisfaction BY gender WITH age (weighted)
SUBTITLE 'ANCOVA: life_satisfaction BY gender WITH age (weighted)'.
UNIANOVA life_satisfaction BY gender WITH age
  /REGWGT=sampling_weight
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY PARAMETER
  /EMMEANS=TABLES(gender) WITH(age=MEAN)
  /CRITERIA=ALPHA(.05)
  /DESIGN=age gender.

* ============================================================================.
* TEST 3: UNWEIGHTED - TWO-WAY ANCOVA (2 factors + 1 covariate)
* ============================================================================.

TITLE '=========== UNWEIGHTED / TWO-WAY ANCOVA ==========='.

TITLE '=========== Test 3a ==========='.

* Test 3a: income BY gender education WITH age (unweighted)
* Factorial ANCOVA: 2x4 design with age controlled
SUBTITLE 'ANCOVA: income BY gender * education WITH age (unweighted)'.
UNIANOVA income BY gender education WITH age
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY PARAMETER
  /EMMEANS=TABLES(gender*education) WITH(age=MEAN)
  /CRITERIA=ALPHA(.05)
  /DESIGN=age gender education gender*education.

TITLE '=========== Test 3b ==========='.

* Test 3b: life_satisfaction BY gender region WITH age (unweighted)
* 2x2 factorial ANCOVA
SUBTITLE 'ANCOVA: life_satisfaction BY gender * region WITH age (unweighted)'.
UNIANOVA life_satisfaction BY gender region WITH age
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY PARAMETER
  /EMMEANS=TABLES(gender*region) WITH(age=MEAN)
  /CRITERIA=ALPHA(.05)
  /DESIGN=age gender region gender*region.

* ============================================================================.
* TEST 4: WEIGHTED - TWO-WAY ANCOVA (via /REGWGT for WLS)
* ============================================================================.

TITLE '=========== WEIGHTED / TWO-WAY ANCOVA ==========='.

TITLE '=========== Test 4a ==========='.

* Test 4a: income BY gender education WITH age (weighted)
SUBTITLE 'ANCOVA: income BY gender * education WITH age (weighted)'.
UNIANOVA income BY gender education WITH age
  /REGWGT=sampling_weight
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY PARAMETER
  /EMMEANS=TABLES(gender*education) WITH(age=MEAN)
  /CRITERIA=ALPHA(.05)
  /DESIGN=age gender education gender*education.

TITLE '=========== Test 4b ==========='.

* Test 4b: life_satisfaction BY gender region WITH age (weighted)
SUBTITLE 'ANCOVA: life_satisfaction BY gender * region WITH age (weighted)'.
UNIANOVA life_satisfaction BY gender region WITH age
  /REGWGT=sampling_weight
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY PARAMETER
  /EMMEANS=TABLES(gender*region) WITH(age=MEAN)
  /CRITERIA=ALPHA(.05)
  /DESIGN=age gender region gender*region.

* ============================================================================.
* TEST 5: UNWEIGHTED - ANCOVA WITH MULTIPLE COVARIATES
* ============================================================================.

TITLE '=========== UNWEIGHTED / MULTIPLE COVARIATES ==========='.

TITLE '=========== Test 5a ==========='.

* Test 5a: income BY education WITH age political_orientation (unweighted)
* Two covariates controlled simultaneously
SUBTITLE 'ANCOVA: income BY education WITH age, political_orientation (unweighted)'.
UNIANOVA income BY education WITH age political_orientation
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY PARAMETER
  /EMMEANS=TABLES(education) WITH(age=MEAN political_orientation=MEAN)
  /CRITERIA=ALPHA(.05)
  /DESIGN=age political_orientation education.

* ============================================================================.
* TEST 6: WEIGHTED - ANCOVA WITH MULTIPLE COVARIATES (via /REGWGT)
* ============================================================================.

TITLE '=========== WEIGHTED / MULTIPLE COVARIATES ==========='.

TITLE '=========== Test 6a ==========='.

* Test 6a: income BY education WITH age political_orientation (weighted)
SUBTITLE 'ANCOVA: income BY education WITH age, political_orientation (weighted)'.
UNIANOVA income BY education WITH age political_orientation
  /REGWGT=sampling_weight
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY PARAMETER
  /EMMEANS=TABLES(education) WITH(age=MEAN political_orientation=MEAN)
  /CRITERIA=ALPHA(.05)
  /DESIGN=age political_orientation education.

SORT CASES BY original_order.

OMSEND.

EXECUTE.

* ============================================================================.
* NOTES FOR VALIDATION
* ============================================================================.
* Key values to extract from SPSS output:
*
* Tests of Between-Subjects Effects:
*   For each source (Corrected Model, Intercept, Covariate(s), Factor(s),
*   Interaction(s), Error, Corrected Total):
*   - Type III Sum of Squares
*   - df (degrees of freedom)
*   - Mean Square
*   - F statistic
*   - Sig. (p-value)
*   - Partial Eta Squared (effect size)
*
* Parameter Estimates:
*   For each parameter (Intercept, covariate, factor levels, interactions):
*   - B (regression coefficient)
*   - Std. Error
*   - t statistic
*   - Sig.
*   - Partial Eta Squared
*   - 95% CI (Lower Bound, Upper Bound)
*
* Estimated Marginal Means:
*   - Mean (adjusted for covariates at their grand mean)
*   - Std. Error
*   - 95% CI
*
* Descriptive Statistics:
*   - Mean, Std. Deviation, N for each factor level
*   - Note: These are UNADJUSTED means (not controlling for covariate)
*
* Levene's Test of Equality of Error Variances:
*   - F statistic, df1, df2, Sig.
*
* Tolerances for R tests:
*   - F statistic: +/-0.001
*   - Type III SS: +/-0.01
*   - Mean Square: +/-0.01
*   - df: exact match
*   - p-value: +/-0.0001
*   - Partial Eta Squared: +/-0.001
*   - B (parameter estimates): +/-0.01
*   - SE (parameter estimates): +/-0.01
*   - t statistic: +/-0.01
*   - Estimated Marginal Means: +/-0.01
*   - Means (descriptive): +/-0.01
*   - Standard Deviations: +/-0.01
*   - N (unweighted): exact match
*
* WEIGHTING NOTE:
*   - /REGWGT applies WLS (weighted least squares) in the GLM framework
*   - This is the correct approach for analytical/survey weights
*   - WEIGHT BY would apply frequency weighting (case replication),
*     which does not produce proper WLS estimates in UNIANOVA
*   - R equivalent: lm(dv ~ covariates + factors, weights = w)
*   - Both use the diagonal weight matrix approach
*
* ANCOVA-Specific Notes:
*   - Covariates are entered BEFORE factors in the /DESIGN statement
*   - /PARAMETER prints the regression table (B coefficients)
*   - /EMMEANS provides estimated marginal means = adjusted means
*     WITH(covariate=MEAN) evaluates at the grand mean of the covariate
*   - Type III SS for the factor = SS after adjusting for covariate
*   - Estimated Marginal Means differ from raw descriptive means
*   - Homogeneity of regression slopes assumed
* ============================================================================.
