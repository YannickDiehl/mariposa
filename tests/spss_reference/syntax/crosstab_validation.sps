* ============================================================================
* CROSSTAB VALIDATION SYNTAX FOR SURVEYSTAT PACKAGE
* ============================================================================
* Purpose: Generate SPSS reference values for crosstab() function validation
* Dataset: survey_data.sav
* Created: 2025-01-27
* SPSS Version: 29.0.0.0 (adjust as needed)
*
* This syntax generates comprehensive crosstab outputs for validation:
* - 2-way crosstabs (simple associations)
* - 3-way crosstabs (layered/stratified analysis)
* - Various statistics (Chi-square, Phi, Cramer's V, Gamma, Lambda, etc.)
* - Different percentage bases (row, column, total)
* - Weighted and unweighted analyses
* - Grouped analyses using SPLIT FILE
* ============================================================================

* Start Output Management System to capture results
OMS
  /IF SUBTYPES=['Crosstabulation' 'Chi-Square Tests' 'Symmetric Measures' 
                 'Directional Measures' 'Risk Estimate']
  /DESTINATION FORMAT=TEXT 
    OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/outputs/crosstab_validation_output.txt'.

* Create case order variable for later restoration
COMPUTE original_order = $CASENUM.
EXECUTE.

* ============================================================================
* SCENARIO 1: UNWEIGHTED / UNGROUPED
* ============================================================================

TITLE '=========== SCENARIO 1: UNWEIGHTED / UNGROUPED ==========='.

* Test 1.1: Simple 2-way crosstab (gender × region)
* Binary × Binary table for basic validation
SUBTITLE 'Test 1.1: Gender × Region (2×2 table)'.

CROSSTABS
  /TABLES=gender BY region
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI LAMBDA GAMMA D BTAU CTAU CORR RISK
  /CELLS=COUNT ROW COLUMN TOTAL EXPECTED RESID STDRESID ADJRESID
  /COUNT ROUND CELL.

* Test 1.2: Larger crosstab (education × employment)
* Multi-category variables for complex table validation
SUBTITLE 'Test 1.2: Education × Employment Status (4×5 table)'.

CROSSTABS
  /TABLES=education BY employment
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI LAMBDA GAMMA D BTAU CTAU
  /CELLS=COUNT ROW COLUMN TOTAL EXPECTED RESID
  /COUNT ROUND CELL.

* Test 1.3: 3-way layered crosstab (life_satisfaction × gender × region)
* Following the inspiration code pattern: AV by UV by Layer
* Compares life satisfaction between genders within each region
SUBTITLE 'Test 1.3: Life Satisfaction × Gender × Region (3-way)'.

CROSSTABS
  /TABLES=life_satisfaction BY gender BY region
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI GAMMA
  /CELLS=COUNT COLUMN
  /COUNT ROUND CELL.

* Test 1.4: Alternative 3-way arrangement
* Compares life satisfaction between regions within each gender
SUBTITLE 'Test 1.4: Life Satisfaction × Region × Gender (3-way reordered)'.

CROSSTABS
  /TABLES=life_satisfaction BY region BY gender
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI GAMMA
  /CELLS=COUNT COLUMN
  /COUNT ROUND CELL.

* Test 1.5: Ordinal association (age_group × life_satisfaction)
* Tests ordinal measures of association
SUBTITLE 'Test 1.5: Age Group × Life Satisfaction (ordinal)'.

CROSSTABS
  /TABLES=age_group BY life_satisfaction
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ GAMMA D BTAU CTAU
  /CELLS=COUNT ROW COLUMN TOTAL
  /COUNT ROUND CELL.

* ============================================================================
* SCENARIO 2: WEIGHTED / UNGROUPED
* ============================================================================

TITLE '=========== SCENARIO 2: WEIGHTED / UNGROUPED ==========='.

WEIGHT BY sampling_weight.

* Test 2.1: Weighted 2-way crosstab (gender × region)
SUBTITLE 'Test 2.1: Gender × Region (weighted, 2×2)'.

CROSSTABS
  /TABLES=gender BY region
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI LAMBDA GAMMA D BTAU CTAU CORR RISK
  /CELLS=COUNT ROW COLUMN TOTAL EXPECTED RESID STDRESID ADJRESID
  /COUNT ROUND CELL.

* Test 2.2: Weighted larger crosstab (education × employment)
SUBTITLE 'Test 2.2: Education × Employment Status (weighted, 4×5)'.

CROSSTABS
  /TABLES=education BY employment
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI LAMBDA GAMMA D BTAU CTAU
  /CELLS=COUNT ROW COLUMN TOTAL EXPECTED RESID
  /COUNT ROUND CELL.

* Test 2.3: Weighted 3-way layered crosstab
SUBTITLE 'Test 2.3: Life Satisfaction × Gender × Region (weighted, 3-way)'.

CROSSTABS
  /TABLES=life_satisfaction BY gender BY region
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI GAMMA
  /CELLS=COUNT COLUMN
  /COUNT ROUND CELL.

* Test 2.4: Weighted alternative 3-way arrangement
SUBTITLE 'Test 2.4: Life Satisfaction × Region × Gender (weighted, 3-way reordered)'.

CROSSTABS
  /TABLES=life_satisfaction BY region BY gender
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI GAMMA
  /CELLS=COUNT COLUMN
  /COUNT ROUND CELL.

* Test 2.5: Weighted ordinal association
SUBTITLE 'Test 2.5: Age Group × Life Satisfaction (weighted, ordinal)'.

CROSSTABS
  /TABLES=age_group BY life_satisfaction
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ GAMMA D BTAU CTAU
  /CELLS=COUNT ROW COLUMN TOTAL
  /COUNT ROUND CELL.

WEIGHT OFF.

* ============================================================================
* SCENARIO 3: UNWEIGHTED / GROUPED BY REGION
* ============================================================================

TITLE '=========== SCENARIO 3: UNWEIGHTED / GROUPED BY REGION ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

* Test 3.1: Grouped 2-way crosstab (gender × education)
SUBTITLE 'Test 3.1: Gender × Education by Region (unweighted, grouped)'.

CROSSTABS
  /TABLES=gender BY education
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI GAMMA
  /CELLS=COUNT ROW COLUMN TOTAL
  /COUNT ROUND CELL.

* Test 3.2: Grouped crosstab (life_satisfaction × gender)
SUBTITLE 'Test 3.2: Life Satisfaction × Gender by Region (unweighted, grouped)'.

CROSSTABS
  /TABLES=life_satisfaction BY gender
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI GAMMA D
  /CELLS=COUNT COLUMN EXPECTED
  /COUNT ROUND CELL.

* Test 3.3: Grouped 3-way crosstab (education × employment × gender)
SUBTITLE 'Test 3.3: Education × Employment × Gender by Region (unweighted, grouped, 3-way)'.

CROSSTABS
  /TABLES=education BY employment BY gender
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI
  /CELLS=COUNT COLUMN
  /COUNT ROUND CELL.

* ============================================================================
* SCENARIO 4: WEIGHTED / GROUPED BY REGION
* ============================================================================

TITLE '=========== SCENARIO 4: WEIGHTED / GROUPED BY REGION ==========='.

WEIGHT BY sampling_weight.

* Test 4.1: Weighted grouped 2-way crosstab (gender × education)
SUBTITLE 'Test 4.1: Gender × Education by Region (weighted, grouped)'.

CROSSTABS
  /TABLES=gender BY education
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI GAMMA
  /CELLS=COUNT ROW COLUMN TOTAL
  /COUNT ROUND CELL.

* Test 4.2: Weighted grouped crosstab (life_satisfaction × gender)
SUBTITLE 'Test 4.2: Life Satisfaction × Gender by Region (weighted, grouped)'.

CROSSTABS
  /TABLES=life_satisfaction BY gender
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI GAMMA D
  /CELLS=COUNT COLUMN EXPECTED
  /COUNT ROUND CELL.

* Test 4.3: Weighted grouped 3-way crosstab
SUBTITLE 'Test 4.3: Education × Employment × Gender by Region (weighted, grouped, 3-way)'.

CROSSTABS
  /TABLES=education BY employment BY gender
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI
  /CELLS=COUNT COLUMN
  /COUNT ROUND CELL.

* ============================================================================
* SCENARIO 5: SPECIAL CASES AND EDGE CONDITIONS
* ============================================================================

TITLE '=========== SCENARIO 5: SPECIAL CASES ==========='.

WEIGHT OFF.
SPLIT FILE OFF.

* Test 5.1: Table with missing values
* Tests how missing values are handled
SUBTITLE 'Test 5.1: Variables with Missing Values'.

CROSSTABS
  /TABLES=income BY life_satisfaction
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI GAMMA
  /CELLS=COUNT ROW COLUMN
  /COUNT ROUND CELL
  /MISSING=INCLUDE.

* Test 5.2: Very sparse table (many empty cells)
* Tests handling of tables with many zero cells
SUBTITLE 'Test 5.2: Sparse Table (age_group × employment)'.

CROSSTABS
  /TABLES=age_group BY employment
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI LAMBDA
  /CELLS=COUNT EXPECTED
  /COUNT ROUND CELL.

* Test 5.3: Large table dimensions
* Tests performance and accuracy with many categories
SUBTITLE 'Test 5.3: Large Dimensions (if available)'.

* Note: Adjust variables based on available multi-category variables
CROSSTABS
  /TABLES=political_orientation BY life_satisfaction
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI GAMMA
  /CELLS=COUNT TOTAL
  /COUNT ROUND CELL.

* ============================================================================
* SCENARIO 6: PERCENTAGE COMPARISON PATTERNS (Following Inspiration Code)
* ============================================================================

TITLE '=========== SCENARIO 6: PERCENTAGE COMPARISON PATTERNS ==========='.

* Test 6.1: Within-group comparisons (East vs West within Gender groups)
* Pattern: Outcome BY Region BY Gender
SUBTITLE 'Test 6.1: Political Orientation × Region × Gender'.

CROSSTABS
  /TABLES=political_orientation BY region BY gender
  /FORMAT=AVALUE TABLES
  /STATISTICS=PHI
  /CELLS=COUNT COLUMN
  /COUNT ROUND CELL.

* Test 6.2: Between-group comparisons (Male vs Female within Region)
* Pattern: Outcome BY Gender BY Region (reordered)
SUBTITLE 'Test 6.2: Political Orientation × Gender × Region'.

CROSSTABS
  /TABLES=political_orientation BY gender BY region
  /FORMAT=AVALUE TABLES
  /STATISTICS=PHI
  /CELLS=COUNT COLUMN
  /COUNT ROUND CELL.

* Test 6.3: Weighted within-group comparisons
SUBTITLE 'Test 6.3: Weighted Political Orientation × Region × Gender'.

WEIGHT BY sampling_weight.

CROSSTABS
  /TABLES=political_orientation BY region BY gender
  /FORMAT=AVALUE TABLES
  /STATISTICS=PHI
  /CELLS=COUNT COLUMN
  /COUNT ROUND CELL.

* Test 6.4: Weighted between-group comparisons
SUBTITLE 'Test 6.4: Weighted Political Orientation × Gender × Region'.

CROSSTABS
  /TABLES=political_orientation BY gender BY region
  /FORMAT=AVALUE TABLES
  /STATISTICS=PHI
  /CELLS=COUNT COLUMN
  /COUNT ROUND CELL.

WEIGHT OFF.

* ============================================================================
* CLEANUP AND FINALIZATION
* ============================================================================

* Restore original case order
SORT CASES BY original_order.
SPLIT FILE OFF.

* End Output Management System
OMSEND.

* Final execution
EXECUTE.

* ============================================================================
* VALIDATION NOTES
* ============================================================================
* 
* This syntax generates comprehensive crosstab outputs for validation:
* 
* 1. Basic 2-way tables with all statistics
* 2. 3-way layered tables for stratified analysis
* 3. Weighted analyses to test weight handling
* 4. Grouped analyses using SPLIT FILE
* 5. Special cases (missing values, sparse tables)
* 6. Percentage comparison patterns (inspired by the example code)
* 
* Key statistics captured:
* - Chi-square test (Pearson, Likelihood Ratio, Linear-by-Linear)
* - Phi and Cramer's V (nominal association)
* - Gamma, Kendall's tau-b/c, Somers' d (ordinal association)
* - Lambda (nominal prediction)
* - Risk estimates (for 2×2 tables)
* - Cell statistics (counts, percentages, residuals)
* 
* The output file will contain all necessary reference values for
* validating the R crosstab() function implementation.
* ============================================================================