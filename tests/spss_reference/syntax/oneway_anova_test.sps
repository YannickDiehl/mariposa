* Encoding: UTF-8.
* SPSS Syntax for One-way ANOVA Test Validation
* Dataset: survey_data.sav

* Start Output Management System to save results as text

OMS
  /IF SUBTYPES=['ANOVA' 'Descriptives' 'Test of Homogeneity of Variances' 'Robust Tests of Equality of Means' 'Multiple Comparisons' 'Means Report']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/outputs/oneway_anova_output.txt'.

COMPUTE original_order = $CASENUM.

* ================================================
* TEST 1: UNWEIGHTED / UNGROUPED
* ================================================

TITLE '=========== UNWEIGHTED / UNGROUPED ==========='.

TITLE '=========== Test 1a ==========='.

* Test 1a: Life satisfaction by education (4 groups)

TITLE 'One-Way ANOVA: Life Satisfaction by Education'.

ONEWAY life_satisfaction BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 1b ==========='.

* Test 1b: Income by education

TITLE 'One-Way ANOVA: Income by Education'.

ONEWAY income BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 1c ==========='.

* Test 1c: Age by education

TITLE 'One-Way ANOVA: Age by Education'.
ONEWAY age BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 1d ==========='.

* Test 1d: Trust variables by education (multiple DVs)

TITLE 'One-Way ANOVA: Trust Variables by Education'.
ONEWAY trust_government trust_media trust_science BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 1e ==========='.

* Test 1e: Life satisfaction by employment (5 groups)

TITLE 'One-Way ANOVA: Life Satisfaction by Employment'.
ONEWAY life_satisfaction BY employment
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 1f ==========='.

* Test 1f: Income by employment

TITLE 'One-Way ANOVA: Income by Employment'.
ONEWAY income BY employment
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

* ================================================
* TEST 2: WEIGHTED / UNGROUPED
* ================================================

TITLE '=========== WEIGHTED / UNGROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 2a ==========='.

* Test 2a: Life satisfaction by education (weighted)

TITLE 'One-Way ANOVA: Life Satisfaction by Education (weighted)'.
ONEWAY life_satisfaction BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 2b ==========='.

* Test 2b: Income by education (weighted)

TITLE 'One-Way ANOVA: Income by Education (weighted)'.
ONEWAY income BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 2c ==========='.

* Test 2c: Age by education (weighted)

TITLE 'One-Way ANOVA: Age by Education (weighted)'.
ONEWAY age BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 2d ==========='.

* Test 2d: Trust variables by education (weighted, multiple DVs)

TITLE 'One-Way ANOVA: Trust Variables by Education (weighted)'.
ONEWAY trust_government trust_media trust_science BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 2e ==========='.

* Test 2e: Life satisfaction by employment (weighted, 5 groups)

TITLE 'One-Way ANOVA: Life Satisfaction by Employment (weighted)'.
ONEWAY life_satisfaction BY employment
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 2f ==========='.

* Test 2f: Income by employment (weighted)

TITLE 'One-Way ANOVA: Income by Employment (weighted)'.
ONEWAY income BY employment
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

WEIGHT OFF.

* ================================================
* TEST 3: UNWEIGHTED / GROUPED
* ================================================

TITLE '=========== UNWEIGHTED / GROUPED ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

TITLE '=========== Test 3a ==========='.

* Test 3a: Life satisfaction by education (grouped by region)

TITLE 'One-Way ANOVA: Life Satisfaction by Education (grouped by region)'.
ONEWAY life_satisfaction BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 3b ==========='.

* Test 3b: Income by education (grouped by region)

TITLE 'One-Way ANOVA: Income by Education (grouped by region)'.
ONEWAY income BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 3c ==========='.

* Test 3c: Age by education (grouped by region)

TITLE 'One-Way ANOVA: Age by Education (grouped by region)'.
ONEWAY age BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 3d ==========='.

* Test 3d: Life satisfaction by employment (grouped by region, 5 groups)

TITLE 'One-Way ANOVA: Life Satisfaction by Employment (grouped by region)'.
ONEWAY life_satisfaction BY employment
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

* ================================================
* TEST 4: WEIGHTED / GROUPED
* ================================================

TITLE '=========== WEIGHTED / GROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 4a ==========='.

* Test 4a: Life satisfaction by education (weighted, grouped by region)

TITLE 'One-Way ANOVA: Life Satisfaction by Education (weighted, grouped by region)'.
ONEWAY life_satisfaction BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 4b ==========='.

* Test 4b: Income by education (weighted, grouped by region)

TITLE 'One-Way ANOVA: Income by Education (weighted, grouped by region)'.
ONEWAY income BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 4c ==========='.

* Test 4c: Age by education (weighted, grouped by region)

TITLE 'One-Way ANOVA: Age by Education (weighted, grouped by region)'.
ONEWAY age BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 4d ==========='.

* Test 4d: Life satisfaction by employment (weighted, grouped by region, 5 groups)

TITLE 'One-Way ANOVA: Life Satisfaction by Employment (weighted, grouped by region)'.
ONEWAY life_satisfaction BY employment
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH BROWNFORSYTHE
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

WEIGHT OFF.
SPLIT FILE OFF.
SORT CASES BY original_order.

* ================================================
* ADDITIONAL TESTS FOR SPECIAL CASES
* ================================================

TITLE '=========== ADDITIONAL TEST CASES ==========='.

* Test 5: Post-hoc tests (minimal, as tukey_test handles most)

TITLE '=========== Test 5a ==========='.

TITLE 'One-Way ANOVA with Tukey Post-Hoc'.
ONEWAY life_satisfaction BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

* Test 6: Different confidence intervals

TITLE '=========== Test 6a ==========='.

TITLE 'One-Way ANOVA with 90% CI'.
ONEWAY life_satisfaction BY education
  /STATISTICS DESCRIPTIVES
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.90).

TITLE '=========== Test 6b ==========='.

TITLE 'One-Way ANOVA with 99% CI'.
ONEWAY life_satisfaction BY education
  /STATISTICS DESCRIPTIVES
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.99).

* Test 7: Multiple variables simultaneously (efficiency check)

TITLE '=========== Test 7 ==========='.

TITLE 'Multiple Variables ANOVA by Education'.
ONEWAY life_satisfaction income age political_orientation environmental_concern BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY WELCH
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95).

OMSEND.

EXECUTE.

* ================================================
* NOTES FOR VALIDATION
* ================================================
*
* Key values to extract from SPSS output:
*
* ANOVA Table:
* - Sum of Squares (Between Groups, Within Groups, Total)
* - df (degrees of freedom for between, within)
* - Mean Square (Between, Within)
* - F-statistic
* - Sig. (p-value)
*
* Descriptives Table (for each group):
* - N (sample size per group)
* - Mean
* - Std. Deviation
* - Std. Error
* - 95% CI Lower Bound
* - 95% CI Upper Bound
* - Minimum
* - Maximum
*
* Test of Homogeneity of Variances:
* - Levene Statistic
* - df1, df2
* - Sig. (p-value for Levene's test)
*
* Robust Tests of Equality of Means:
* - Welch: Statistic, df1, df2, Sig.
* - Brown-Forsythe: Statistic, df1, df2, Sig.
*
* Effect Size Calculations (manual):
* - Eta-squared = SS_between / SS_total
* - Epsilon-squared = (SS_between - (k-1)*MS_within) / SS_total
* - Omega-squared = (SS_between - (k-1)*MS_within) / (SS_total + MS_within)
*   where k = number of groups
*
* Weighted Analyses:
* - Weighted N may be decimal
* - Sum of weights used in calculations
* - Effective sample size considerations
*
* Grouped Analyses:
* - Separate ANOVA for each region (East/West)
* - Check interaction effects if needed
*
* Post-hoc Tests (minimal here, see tukey_test.sps):
* - Multiple comparison p-values
* - Mean differences between groups
* - Standard errors of differences
* - Confidence intervals for differences
*
* Validation Notes:
* - F-statistics should match to 4 decimal places
* - P-values should match to 5 decimal places
* - Degrees of freedom must match exactly
* - Effect sizes may require manual calculation from SS values
* - Welch test is crucial for unequal variances assumption