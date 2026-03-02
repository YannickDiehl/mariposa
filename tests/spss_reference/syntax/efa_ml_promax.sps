* Encoding: UTF-8.
* SPSS Syntax for ML Extraction and Promax Rotation Validation
* Dataset: survey_data.sav
* Purpose: Validate efa() ML extraction and Promax rotation against SPSS FACTOR
* Variables: political_orientation, environmental_concern, life_satisfaction,
*            trust_government, trust_media, trust_science (6 Likert items)
* Weight: sampling_weight
* Grouping: region
*
* NOTE: ML extraction uses /EXTRACTION ML instead of /EXTRACTION PC
*       Promax rotation uses /ROTATION PROMAX instead of /ROTATION VARIMAX
*       SPSS Promax default power (Kappa) = 4
*       R stats::promax() default power (m) = 3
*       For SPSS compatibility, we test with SPSS default Kappa=4.
*       If needed, adjust R's promax(m=4) to match.
*
* Key differences from PCA:
*   - Initial communalities = SMC (not 1.000)
*   - "Factor Matrix" instead of "Component Matrix"
*   - "Rotated Factor Matrix" instead of "Rotated Component Matrix"
*   - Goodness-of-fit Test: Chi-Square, df, Sig.
*   - Uniquenesses = 1 - communalities

GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/data/survey_data.sav'.

* Start Output Management System to save results as text

OMS
  /IF COMMANDS=['Factor Analysis']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/outputs/efa_ml_promax_output.txt'.

COMPUTE original_order = $CASENUM.
EXECUTE.

* ============================================================================.
* TEST 5: ML EXTRACTION - UNWEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== ML EXTRACTION: UNWEIGHTED / UNGROUPED ==========='.

TITLE '=========== Test 5a ==========='.

* Test 5a: ML with Varimax rotation, Kaiser criterion (unweighted, ungrouped)
SUBTITLE 'Factor Analysis: ML + Varimax, Kaiser criterion (unweighted, ungrouped)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION ML
  /ROTATION VARIMAX
  /METHOD=CORRELATION.

TITLE '=========== Test 5b ==========='.

* Test 5b: ML with Promax rotation, Kaiser criterion (unweighted, ungrouped)
SUBTITLE 'Factor Analysis: ML + Promax, Kaiser criterion (unweighted, ungrouped)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION ML
  /ROTATION PROMAX
  /METHOD=CORRELATION.

TITLE '=========== Test 5c ==========='.

* Test 5c: ML with Oblimin rotation, Kaiser criterion (unweighted, ungrouped)
SUBTITLE 'Factor Analysis: ML + Oblimin, Kaiser criterion (unweighted, ungrouped)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION ML
  /ROTATION OBLIMIN
  /METHOD=CORRELATION.

TITLE '=========== Test 5d ==========='.

* Test 5d: ML without rotation, Kaiser criterion (unweighted, ungrouped)
SUBTITLE 'Factor Analysis: ML + No rotation, Kaiser criterion (unweighted, ungrouped)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION ML
  /ROTATION NOROTATE
  /METHOD=CORRELATION.

* ============================================================================.
* TEST P1: PCA + PROMAX - UNWEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== PCA + PROMAX: UNWEIGHTED / UNGROUPED ==========='.

TITLE '=========== Test P1 ==========='.

* Test P1: PCA with Promax rotation, Kaiser criterion (unweighted, ungrouped)
SUBTITLE 'Factor Analysis: PCA + Promax, Kaiser criterion (unweighted, ungrouped)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /ROTATION PROMAX
  /METHOD=CORRELATION.

* ============================================================================.
* TEST 6: ML EXTRACTION - WEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== ML EXTRACTION: WEIGHTED / UNGROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 6a ==========='.

* Test 6a: ML with Varimax rotation, Kaiser criterion (weighted, ungrouped)
SUBTITLE 'Factor Analysis: ML + Varimax, Kaiser criterion (weighted, ungrouped)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION ML
  /ROTATION VARIMAX
  /METHOD=CORRELATION.

TITLE '=========== Test 6b ==========='.

* Test 6b: ML with Promax rotation, Kaiser criterion (weighted, ungrouped)
SUBTITLE 'Factor Analysis: ML + Promax, Kaiser criterion (weighted, ungrouped)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION ML
  /ROTATION PROMAX
  /METHOD=CORRELATION.

* ============================================================================.
* TEST P2: PCA + PROMAX - WEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== PCA + PROMAX: WEIGHTED / UNGROUPED ==========='.

TITLE '=========== Test P2 ==========='.

* Test P2: PCA with Promax rotation, Kaiser criterion (weighted, ungrouped)
SUBTITLE 'Factor Analysis: PCA + Promax, Kaiser criterion (weighted, ungrouped)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /ROTATION PROMAX
  /METHOD=CORRELATION.

WEIGHT OFF.

* ============================================================================.
* TEST 7: ML EXTRACTION - UNWEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== ML EXTRACTION: UNWEIGHTED / GROUPED ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

TITLE '=========== Test 7a ==========='.

* Test 7a: ML with Varimax rotation, Kaiser criterion (unweighted, grouped by region)
SUBTITLE 'Factor Analysis: ML + Varimax, Kaiser criterion (unweighted, grouped by region)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION ML
  /ROTATION VARIMAX
  /METHOD=CORRELATION.

* ============================================================================.
* TEST P3: PCA + PROMAX - UNWEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== PCA + PROMAX: UNWEIGHTED / GROUPED ==========='.

TITLE '=========== Test P3 ==========='.

* Test P3: PCA with Promax rotation, Kaiser criterion (unweighted, grouped by region)
SUBTITLE 'Factor Analysis: PCA + Promax, Kaiser criterion (unweighted, grouped by region)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /ROTATION PROMAX
  /METHOD=CORRELATION.

* ============================================================================.
* TEST 8: ML EXTRACTION - WEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== ML EXTRACTION: WEIGHTED / GROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 8a ==========='.

* Test 8a: ML with Varimax rotation, Kaiser criterion (weighted, grouped by region)
SUBTITLE 'Factor Analysis: ML + Varimax, Kaiser criterion (weighted, grouped by region)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION ML
  /ROTATION VARIMAX
  /METHOD=CORRELATION.

* ============================================================================.
* TEST P4: PCA + PROMAX - WEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== PCA + PROMAX: WEIGHTED / GROUPED ==========='.

TITLE '=========== Test P4 ==========='.

* Test P4: PCA with Promax rotation, Kaiser criterion (weighted, grouped by region)
SUBTITLE 'Factor Analysis: PCA + Promax, Kaiser criterion (weighted, grouped by region)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /ROTATION PROMAX
  /METHOD=CORRELATION.

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
* For ML Extraction (Tests 5a-5d, 6a-6b, 7a, 8a):
*   - KMO and Bartlett's Test (same as PCA for same data/weights)
*   - Goodness-of-fit Test: Chi-Square, df, Sig.
*   - Communalities: Initial (SMC, not 1.000), Extraction
*   - Total Variance Explained: Eigenvalues, %, Cumulative %
*   - Factor Matrix (unrotated ML loadings)
*   - Rotated Factor Matrix (Varimax) or Pattern/Structure Matrix (Promax/Oblimin)
*   - Factor Correlation Matrix (Promax/Oblimin only)
*
* For Promax Rotation (Tests 5b, 6b, P1-P4):
*   - Pattern Matrix (regression coefficients)
*   - Structure Matrix (correlations)
*   - Factor/Component Correlation Matrix
*   - Rotation Sums of Squared Loadings (SS only, no cumulative)
*
* SPSS Promax default: Kappa = 4
* R stats::promax() default: m = 3
* IMPORTANT: Match these for validation. Either:
*   - Use R's promax(m=4) to match SPSS default, or
*   - Use SPSS /CRITERIA KAPPA(3) to match R default
*
* Tolerances for R tests:
*   - KMO: +/-0.001
*   - Bartlett Chi-Square: +/-0.01
*   - Eigenvalues: +/-0.001
*   - Variance %: +/-0.01
*   - Factor loadings: +/-0.002
*   - Communalities: +/-0.001
*   - Goodness-of-fit Chi-Square: +/-0.01
*   - Goodness-of-fit df: exact match
*   - Goodness-of-fit p-value: +/-0.0001
*   - Factor correlations (Promax): +/-0.01 (may differ due to Kappa)
*
* ML-specific considerations:
*   - ML may extract fewer factors than PCA (convergence issues)
*   - ML assumes multivariate normality
*   - ML provides a formal goodness-of-fit test
*   - Non-convergence: increase ITERATE or reduce factors
*   - Heywood cases: communalities > 1.0 indicate identification problems
* ============================================================================.
