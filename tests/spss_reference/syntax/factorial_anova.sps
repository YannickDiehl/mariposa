* Encoding: UTF-8.
* SPSS Syntax for Factorial ANOVA Validation
* Dataset: survey_data.sav
* Purpose: Validate factorial_anova() function against SPSS UNIANOVA procedure
* DVs: life_satisfaction, income, trust_government
* Factors: gender (2 levels), region (2 levels), education (4 levels)
* Weight: sampling_weight (via /REGWGT for proper WLS weighting)
* Note: No SPLIT FILE used -- instead, 3-way ANOVA replaces grouped scenario
*
* IMPORTANT: UNIANOVA requires /REGWGT for WLS weighting, NOT WEIGHT BY.
* WEIGHT BY applies frequency replication weighting, which does not produce
* proper WLS estimates in the GLM framework. /REGWGT applies diagonal
* weight matrix W in the WLS estimation: (X'WX)^-1 X'Wy.

GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/data/survey_data.sav'.

* Start Output Management System to save results as text

OMS
  /IF COMMANDS=['UNIANOVA']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/outputs/factorial_anova_output.txt'.

COMPUTE original_order = $CASENUM.
EXECUTE.

* ============================================================================.
* TEST 1: UNWEIGHTED - TWO-WAY DESIGNS
* ============================================================================.

TITLE '=========== UNWEIGHTED / TWO-WAY ANOVA ==========='.

TITLE '=========== Test 1a ==========='.

* Test 1a: 2x2 ANOVA - life_satisfaction by gender * region
SUBTITLE 'Factorial ANOVA: life_satisfaction BY gender * region (unweighted, 2x2)'.
UNIANOVA life_satisfaction BY gender region
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=gender region gender*region.

TITLE '=========== Test 1b ==========='.

* Test 1b: 2x4 ANOVA - income by gender * education
SUBTITLE 'Factorial ANOVA: income BY gender * education (unweighted, 2x4)'.
UNIANOVA income BY gender education
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=gender education gender*education.

TITLE '=========== Test 1c ==========='.

* Test 1c: 2x2 ANOVA - trust_government by gender * region (different DV)
SUBTITLE 'Factorial ANOVA: trust_government BY gender * region (unweighted, 2x2)'.
UNIANOVA trust_government BY gender region
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=gender region gender*region.

* ============================================================================.
* TEST 2: WEIGHTED - TWO-WAY DESIGNS (via /REGWGT for WLS)
* ============================================================================.

TITLE '=========== WEIGHTED / TWO-WAY ANOVA ==========='.

TITLE '=========== Test 2a ==========='.

* Test 2a: 2x2 ANOVA - life_satisfaction by gender * region (weighted)
SUBTITLE 'Factorial ANOVA: life_satisfaction BY gender * region (weighted, 2x2)'.
UNIANOVA life_satisfaction BY gender region
  /REGWGT=sampling_weight
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=gender region gender*region.

TITLE '=========== Test 2b ==========='.

* Test 2b: 2x4 ANOVA - income by gender * education (weighted)
SUBTITLE 'Factorial ANOVA: income BY gender * education (weighted, 2x4)'.
UNIANOVA income BY gender education
  /REGWGT=sampling_weight
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=gender education gender*education.

* ============================================================================.
* TEST 3: UNWEIGHTED - THREE-WAY DESIGN (replaces grouped scenario)
* ============================================================================.

TITLE '=========== UNWEIGHTED / THREE-WAY ANOVA ==========='.

TITLE '=========== Test 3a ==========='.

* Test 3a: 2x2x4 ANOVA - life_satisfaction by gender * region * education
SUBTITLE 'Factorial ANOVA: life_satisfaction BY gender * region * education (unweighted, 2x2x4)'.
UNIANOVA life_satisfaction BY gender region education
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=gender region education gender*region gender*education region*education gender*region*education.

TITLE '=========== Test 3b ==========='.

* Test 3b: 2x2x4 ANOVA - income by gender * region * education
SUBTITLE 'Factorial ANOVA: income BY gender * region * education (unweighted, 2x2x4)'.
UNIANOVA income BY gender region education
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=gender region education gender*region gender*education region*education gender*region*education.

* ============================================================================.
* TEST 4: WEIGHTED - THREE-WAY DESIGN (via /REGWGT for WLS)
* ============================================================================.

TITLE '=========== WEIGHTED / THREE-WAY ANOVA ==========='.

TITLE '=========== Test 4a ==========='.

* Test 4a: 2x2x4 ANOVA - life_satisfaction by gender * region * education (weighted)
SUBTITLE 'Factorial ANOVA: life_satisfaction BY gender * region * education (weighted, 2x2x4)'.
UNIANOVA life_satisfaction BY gender region education
  /REGWGT=sampling_weight
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=gender region education gender*region gender*education region*education gender*region*education.

TITLE '=========== Test 4b ==========='.

* Test 4b: 2x2x4 ANOVA - income by gender * region * education (weighted)
SUBTITLE 'Factorial ANOVA: income BY gender * region * education (weighted, 2x2x4)'.
UNIANOVA income BY gender region education
  /REGWGT=sampling_weight
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=gender region education gender*region gender*education region*education gender*region*education.

SORT CASES BY original_order.

OMSEND.

EXECUTE.

* ============================================================================.
* NOTES FOR VALIDATION
* ============================================================================.
* Key values to extract from SPSS output:
*
* Tests of Between-Subjects Effects:
*   For each source (Corrected Model, Intercept, Factor A, Factor B,
*   A*B interaction, Error, Corrected Total):
*   - Type III Sum of Squares
*   - df (degrees of freedom)
*   - Mean Square
*   - F statistic
*   - Sig. (p-value)
*   - Partial Eta Squared (effect size)
*
* Descriptive Statistics:
*   - Mean, Std. Deviation, N for each cell combination
*   - Total row/column means
*
* Levene's Test of Equality of Error Variances:
*   - F statistic
*   - df1, df2
*   - Sig.
*
* For Three-Way ANOVA (Tests 3a, 4a), additional sources:
*   - Three main effects (gender, region, education)
*   - Three 2-way interactions (gender*region, gender*education, region*education)
*   - One 3-way interaction (gender*region*education)
*
* Tolerances for R tests:
*   - F statistic: +/-0.001
*   - Type III SS: +/-0.01
*   - Mean Square: +/-0.01
*   - df: exact match
*   - p-value: +/-0.0001
*   - Partial Eta Squared: +/-0.001
*   - Means: +/-0.01
*   - Standard Deviations: +/-0.01
*   - Levene F: +/-0.001
*   - N (unweighted): exact match
*
* WEIGHTING NOTE:
*   - /REGWGT applies WLS (weighted least squares) in the GLM framework
*   - This is the correct approach for analytical/survey weights
*   - WEIGHT BY would apply frequency weighting (case replication),
*     which is not appropriate for continuous survey weights in GLM
*   - R equivalent: aov(dv ~ factors, weights = w) or lm(..., weights = w)
*   - Both use the diagonal weight matrix approach
*
* Design-Specific Notes:
*   - 2x2 (gender * region): Simple interaction test
*   - 2x4 (gender * education): Larger design, more interaction df
*   - 2x2x4 (gender * region * education): Full factorial, many cells
* ============================================================================.
