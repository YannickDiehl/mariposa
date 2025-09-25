* Encoding: UTF-8.
* SPSS Syntax for Tukey HSD Post-Hoc Test Validation
* Dataset: survey_data.sav

* Start Output Management System to save results as text

OMS
  /IF SUBTYPES=['Multiple Comparisons']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/outputs/scheffe_test_output.txt'.

COMPUTE original_order = $CASENUM.

* ================================================
* TEST 1: UNWEIGHTED / UNGROUPED
* ================================================

TITLE '=========== UNWEIGHTED / UNGROUPED ==========='.

TITLE '=========== Test 1a ==========='.

* Test 1a: Life satisfaction by education (4 groups) - Standard 95% CI

TITLE 'Tukey HSD: Life Satisfaction by Education (unweighted)'.

ONEWAY life_satisfaction BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=SCHEFFE ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 1b ==========='.

* Test 1b: Income by education - Standard 95% CI

TITLE 'Tukey HSD: Income by Education (unweighted)'.

ONEWAY income BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=SCHEFFE ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 1c ==========='.

* Test 1c: Age by education - Standard 95% CI

TITLE 'Tukey HSD: Age by Education (unweighted)'.

ONEWAY age BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=SCHEFFE ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 1d ==========='.

* Test 1d: Trust variables by education (multiple DVs) - Standard 95% CI

TITLE 'Tukey HSD: Trust Variables by Education (unweighted)'.

ONEWAY trust_government trust_media trust_science BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 1e ==========='.

* Test 1e: Life satisfaction by employment (5 groups) - Standard 95% CI

TITLE 'Tukey HSD: Life Satisfaction by Employment (unweighted, 5 groups)'.

ONEWAY life_satisfaction BY employment
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 1f ==========='.

* Test 1f: Income by employment - Standard 95% CI

TITLE 'Tukey HSD: Income by Employment (unweighted, 5 groups)'.

ONEWAY income BY employment
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

* ================================================
* TEST 2: WEIGHTED / UNGROUPED
* ================================================

TITLE '=========== WEIGHTED / UNGROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 2a ==========='.

* Test 2a: Life satisfaction by education (weighted) - Standard 95% CI

TITLE 'Tukey HSD: Life Satisfaction by Education (weighted)'.

ONEWAY life_satisfaction BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 2b ==========='.

* Test 2b: Income by education (weighted) - Standard 95% CI

TITLE 'Tukey HSD: Income by Education (weighted)'.

ONEWAY income BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 2c ==========='.

* Test 2c: Age by education (weighted) - Standard 95% CI

TITLE 'Tukey HSD: Age by Education (weighted)'.

ONEWAY age BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 2d ==========='.

* Test 2d: Trust variables by education (weighted, multiple DVs) - Standard 95% CI

TITLE 'Tukey HSD: Trust Variables by Education (weighted)'.

ONEWAY trust_government trust_media trust_science BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 2e ==========='.

* Test 2e: Life satisfaction by employment (weighted, 5 groups) - Standard 95% CI

TITLE 'Tukey HSD: Life Satisfaction by Employment (weighted, 5 groups)'.

ONEWAY life_satisfaction BY employment
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 2f ==========='.

* Test 2f: Income by employment (weighted) - Standard 95% CI

TITLE 'Tukey HSD: Income by Employment (weighted, 5 groups)'.

ONEWAY income BY employment
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
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

TITLE 'Tukey HSD: Life Satisfaction by Education (grouped by region, unweighted)'.

ONEWAY life_satisfaction BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 3b ==========='.

* Test 3b: Income by education (grouped by region)

TITLE 'Tukey HSD: Income by Education (grouped by region, unweighted)'.

ONEWAY income BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 3c ==========='.

* Test 3c: Age by education (grouped by region)

TITLE 'Tukey HSD: Age by Education (grouped by region, unweighted)'.

ONEWAY age BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 3d ==========='.

* Test 3d: Life satisfaction by employment (grouped by region, 5 groups)

TITLE 'Tukey HSD: Life Satisfaction by Employment (grouped by region, unweighted)'.

ONEWAY life_satisfaction BY employment
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

* ================================================
* TEST 4: WEIGHTED / GROUPED
* ================================================

TITLE '=========== WEIGHTED / GROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 4a ==========='.

* Test 4a: Life satisfaction by education (weighted, grouped by region)

TITLE 'Tukey HSD: Life Satisfaction by Education (weighted, grouped by region)'.

ONEWAY life_satisfaction BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 4b ==========='.

* Test 4b: Income by education (weighted, grouped by region)

TITLE 'Tukey HSD: Income by Education (weighted, grouped by region)'.

ONEWAY income BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 4c ==========='.

* Test 4c: Age by education (weighted, grouped by region)

TITLE 'Tukey HSD: Age by Education (weighted, grouped by region)'.

ONEWAY age BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

TITLE '=========== Test 4d ==========='.

* Test 4d: Life satisfaction by employment (weighted, grouped by region, 5 groups)

TITLE 'Tukey HSD: Life Satisfaction by Employment (weighted, grouped by region)'.

ONEWAY life_satisfaction BY employment
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

WEIGHT OFF.
SPLIT FILE OFF.
SORT CASES BY original_order.

* ================================================
* ADDITIONAL TESTS FOR SPECIAL CASES
* ================================================

TITLE '=========== ADDITIONAL TEST CASES ==========='.

* Test 5: Different alpha levels (for significance testing)

TITLE '=========== Test 5a ==========='.

TITLE 'Tukey HSD with alpha = 0.01 (99% family-wise confidence)'.

ONEWAY life_satisfaction BY education
  /STATISTICS DESCRIPTIVES
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.01)
  /CRITERIA=CILEVEL(0.99).

TITLE '=========== Test 5b ==========='.

TITLE 'Tukey HSD with alpha = 0.10 (90% family-wise confidence)'.

ONEWAY life_satisfaction BY education
  /STATISTICS DESCRIPTIVES
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.10)
  /CRITERIA=CILEVEL(0.90).

* Test 6: Different confidence intervals (for interval estimation)

TITLE '=========== Test 6a ==========='.

TITLE 'Tukey HSD with 90% Confidence Intervals'.

ONEWAY life_satisfaction BY education
  /STATISTICS DESCRIPTIVES
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.90).

TITLE '=========== Test 6b ==========='.

TITLE 'Tukey HSD with 99% Confidence Intervals'.

ONEWAY life_satisfaction BY education
  /STATISTICS DESCRIPTIVES
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.99).

* Test 7: Multiple variables simultaneously (efficiency check)

TITLE '=========== Test 7 ==========='.

TITLE 'Tukey HSD: Multiple Variables by Education'.

ONEWAY life_satisfaction income age political_orientation environmental_concern BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

* Test 8: Comparison with other post-hoc tests (for reference)

TITLE '=========== Test 8a ==========='.

TITLE 'Multiple Post-Hoc Tests Comparison'.

ONEWAY life_satisfaction BY education
  /STATISTICS DESCRIPTIVES HOMOGENEITY
  /MISSING ANALYSIS
  /POSTHOC=TUKEY BONFERRONI SCHEFFE LSD ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

* Test 9: Homogeneous subsets output

TITLE '=========== Test 9a ==========='.

TITLE 'Tukey HSD with Homogeneous Subsets'.

ONEWAY life_satisfaction BY education
  /STATISTICS DESCRIPTIVES
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05)
  /CRITERIA=CILEVEL(0.95).

* Note: SPSS automatically generates homogeneous subsets for Tukey

OMSEND.

EXECUTE.

* ================================================
* NOTES FOR VALIDATION
* ================================================
*
* Key values to extract from SPSS Tukey HSD output:
*
* Multiple Comparisons Table:
* - (I) Group and (J) Group identifiers
* - Mean Difference (I-J)
* - Std. Error of the difference
* - Sig. (adjusted p-value using Tukey HSD)
* - 95% Confidence Interval Lower Bound
* - 95% Confidence Interval Upper Bound
*
* Homogeneous Subsets Table:
* - Groups that are not significantly different (same subset)
* - Subset membership for alpha = 0.05
* - Harmonic Mean of group sizes (for unequal n)
*
* Critical Values (from studentized range distribution):
* - q critical value depends on:
*   - Number of groups (k)
*   - Degrees of freedom within groups (df_within)
*   - Alpha level (family-wise error rate)
*
* Weighted Analyses Considerations:
* - Effective sample sizes used in calculations
* - Weighted means and standard errors
* - Sum of weights affects degrees of freedom
*
* Grouped Analyses:
* - Separate Tukey tests for each region group
* - Cannot compare across regions (separate analyses)
*
* Validation Tolerances:
* - Mean differences: Should match to 4 decimal places
* - Standard errors: Should match to 4 decimal places
* - P-values (adjusted): Should match to 5 decimal places
* - Confidence intervals: Should match to 3-4 decimal places
*
* Important SPSS Behaviors:
* 1. Tukey HSD uses studentized range distribution (not t-distribution)
* 2. Family-wise error rate controlled at specified alpha
* 3. All pairwise comparisons included (k*(k-1)/2 comparisons)
* 4. Assumes equal variances (use Games-Howell for unequal)
* 5. Missing data handled pairwise by default
*
* Comparison with R tukey_test():
* - R function should produce identical adjusted p-values
* - Confidence intervals should match SPSS output
* - Ordering of comparisons may differ but content same
* - Check handling of tied values and rounding
*
* Edge Cases to Verify:
* - Two groups only (degenerates to t-test correction)
* - Many groups (10+) to test computational accuracy
* - Unbalanced designs (very different group sizes)
* - Groups with small n (< 5 observations)
* - Missing data patterns
* - Weighted analyses with extreme weights
*
* Formula Reference:
* Tukey HSD Test Statistic:
* q = (mean1 - mean2) / sqrt(MSE * (1/n1 + 1/n2) / 2)
*
* Where:
* - MSE = Mean Square Error from ANOVA
* - n1, n2 = sample sizes of groups being compared
* - q follows studentized range distribution
*
* Confidence Interval:
* (mean1 - mean2) ± q_crit * sqrt(MSE * (1/n1 + 1/n2) / 2)
*
* Where q_crit is from studentized range distribution table