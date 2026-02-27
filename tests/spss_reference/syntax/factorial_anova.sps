* Encoding: UTF-8.
* SPSS Syntax for Factorial ANOVA Validation
* Dataset: survey_data.sav
* Purpose: Validate factorial_anova() function against SPSS UNIANOVA procedure
* DVs: life_satisfaction, income, trust_government
* Factors: gender (2 levels), region (2 levels), education (4 levels)
* Weight: sampling_weight
* Note: No SPLIT FILE used -- instead, 3-way ANOVA replaces grouped scenario

* Start Output Management System to save results as text

OMS
  /IF SUBTYPES=['Tests of Between-Subjects Effects' 'Descriptive Statistics' 'Levene''s Test of Equality of Error Variances' 'Estimated Marginal Means' 'Grand Mean']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/outputs/factorial_anova_output.txt'.

COMPUTE original_order = $CASENUM.

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
* TEST 2: WEIGHTED - TWO-WAY DESIGNS
* ============================================================================.

TITLE '=========== WEIGHTED / TWO-WAY ANOVA ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 2a ==========='.

* Test 2a: 2x2 ANOVA - life_satisfaction by gender * region (weighted)
SUBTITLE 'Factorial ANOVA: life_satisfaction BY gender * region (weighted, 2x2)'.
UNIANOVA life_satisfaction BY gender region
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=gender region gender*region.

TITLE '=========== Test 2b ==========='.

* Test 2b: 2x4 ANOVA - income by gender * education (weighted)
SUBTITLE 'Factorial ANOVA: income BY gender * education (weighted, 2x4)'.
UNIANOVA income BY gender education
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=gender education gender*education.

WEIGHT OFF.

* ============================================================================.
* TEST 3: UNWEIGHTED - THREE-WAY DESIGN (replaces grouped scenario)
* ============================================================================.

TITLE '=========== UNWEIGHTED / THREE-WAY ANOVA ==========='.

TITLE '=========== Test 3a ==========='.

* Test 3a: 2x2x4 ANOVA - life_satisfaction by gender * region * education
* Note: Three-way ANOVA replaces SPLIT FILE grouping for factorial designs.
* This provides main effects, 2-way interactions, and 3-way interaction.
SUBTITLE 'Factorial ANOVA: life_satisfaction BY gender * region * education (unweighted, 2x2x4)'.
UNIANOVA life_satisfaction BY gender region education
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=gender region education gender*region gender*education region*education gender*region*education.

* ============================================================================.
* TEST 4: WEIGHTED - THREE-WAY DESIGN
* ============================================================================.

TITLE '=========== WEIGHTED / THREE-WAY ANOVA ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 4a ==========='.

* Test 4a: 2x2x4 ANOVA - life_satisfaction by gender * region * education (weighted)
SUBTITLE 'Factorial ANOVA: life_satisfaction BY gender * region * education (weighted, 2x2x4)'.
UNIANOVA life_satisfaction BY gender region education
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=gender region education gender*region gender*education region*education gender*region*education.

WEIGHT OFF.
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
*   - N (weighted): +/-1
*
* Important SPSS Behavior:
*   - SSTYPE(3) = Type III Sum of Squares (standard for unbalanced designs)
*     Type III SS tests each effect after adjusting for all other effects
*   - ETASQ prints Partial Eta Squared (not Eta Squared)
*     Partial Eta Sq = SS_effect / (SS_effect + SS_error)
*   - HOMOGENEITY prints Levene's test for each DV
*   - DESIGN statement explicitly specifies the model terms
*   - For 2x2: DESIGN = A B A*B
*   - For 2x4: DESIGN = A B A*B
*   - For 2x2x4: DESIGN = A B C A*B A*C B*C A*B*C
*   - Unbalanced designs are common with survey data (unequal cell sizes)
*   - WEIGHT BY applies weighted least squares
*
* Design-Specific Notes:
*   - 2x2 (gender * region): Simple interaction test
*   - 2x4 (gender * education): Larger design, more interaction df
*   - 2x2x4 (gender * region * education): Full factorial, many cells
*     Some cells may have small N, affecting stability
*
* Weighted Analyses:
*   - WEIGHT BY applies WLS (weighted least squares) estimation
*   - Cell means and SDs are weighted
*   - SS values differ from unweighted analysis
*   - F statistics and p-values adjust for differential weighting
*   - Levene's test also uses weighted observations
*
* Comparison to R:
*   - R's aov() / car::Anova() provide Type III SS with car package
*   - Base R aov() uses Type I SS by default (sequential)
*   - For SPSS compatibility, must use Type III SS
*   - Partial Eta Squared must be manually computed from SS values
*   - Levene's test via car::leveneTest() or manual implementation
*
* Why no SPLIT FILE:
*   - Factorial ANOVA inherently handles multiple factors
*   - SPLIT FILE would create a redundant additional factor
*   - Instead, three-way ANOVA incorporates the grouping variable (region)
*     as a proper factor with interaction terms
* ============================================================================.
