* Encoding: UTF-8.
* SPSS Syntax for Exploratory Factor Analysis Validation
* Dataset: survey_data.sav
* Purpose: Validate efa() function against SPSS FACTOR procedure
* Variables: political_orientation, environmental_concern, life_satisfaction,
*            trust_government, trust_media, trust_science (6 Likert items)
* Weight: sampling_weight
* Grouping: region

GET FILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/data/survey_data.sav'.

* Start Output Management System to save results as text

OMS
  /IF COMMANDS=['Factor Analysis']
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/mariposa/tests/spss_reference/outputs/efa_output.txt'.

COMPUTE original_order = $CASENUM.
EXECUTE.

* ============================================================================.
* TEST 1: UNWEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== UNWEIGHTED / UNGROUPED ==========='.

TITLE '=========== Test 1a ==========='.

* Test 1a: PCA with Varimax rotation, Kaiser criterion (eigenvalue > 1)
SUBTITLE 'Factor Analysis: Varimax, Kaiser criterion (unweighted, ungrouped)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /ROTATION VARIMAX
  /METHOD=CORRELATION.

TITLE '=========== Test 1b ==========='.

* Test 1b: PCA with Oblimin rotation, Kaiser criterion
SUBTITLE 'Factor Analysis: Oblimin, Kaiser criterion (unweighted, ungrouped)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /ROTATION OBLIMIN
  /METHOD=CORRELATION.

TITLE '=========== Test 1c ==========='.

* Test 1c: PCA without rotation, Kaiser criterion
SUBTITLE 'Factor Analysis: No rotation, Kaiser criterion (unweighted, ungrouped)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /ROTATION NOROTATE
  /METHOD=CORRELATION.

TITLE '=========== Test 1d ==========='.

* Test 1d: PCA with Varimax rotation, fixed 2 factors
SUBTITLE 'Factor Analysis: Varimax, 2 factors fixed (unweighted, ungrouped)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA FACTORS(2) ITERATE(25)
  /EXTRACTION PC
  /ROTATION VARIMAX
  /METHOD=CORRELATION.

* ============================================================================.
* TEST 2: WEIGHTED / UNGROUPED
* ============================================================================.

TITLE '=========== WEIGHTED / UNGROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 2a ==========='.

* Test 2a: PCA with Varimax rotation, Kaiser criterion (weighted)
SUBTITLE 'Factor Analysis: Varimax, Kaiser criterion (weighted, ungrouped)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /ROTATION VARIMAX
  /METHOD=CORRELATION.

TITLE '=========== Test 2b ==========='.

* Test 2b: PCA with Oblimin rotation, Kaiser criterion (weighted)
SUBTITLE 'Factor Analysis: Oblimin, Kaiser criterion (weighted, ungrouped)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /ROTATION OBLIMIN
  /METHOD=CORRELATION.

TITLE '=========== Test 2c ==========='.

* Test 2c: PCA with Varimax rotation, fixed 2 factors (weighted)
SUBTITLE 'Factor Analysis: Varimax, 2 factors fixed (weighted, ungrouped)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA FACTORS(2) ITERATE(25)
  /EXTRACTION PC
  /ROTATION VARIMAX
  /METHOD=CORRELATION.

WEIGHT OFF.

* ============================================================================.
* TEST 3: UNWEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== UNWEIGHTED / GROUPED ==========='.

SORT CASES BY region.
SPLIT FILE BY region.

TITLE '=========== Test 3a ==========='.

* Test 3a: PCA with Varimax rotation, Kaiser criterion (grouped by region)
SUBTITLE 'Factor Analysis: Varimax, Kaiser criterion (unweighted, grouped by region)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /ROTATION VARIMAX
  /METHOD=CORRELATION.

TITLE '=========== Test 3b ==========='.

* Test 3b: PCA with Oblimin rotation, Kaiser criterion (grouped by region)
SUBTITLE 'Factor Analysis: Oblimin, Kaiser criterion (unweighted, grouped by region)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /ROTATION OBLIMIN
  /METHOD=CORRELATION.

* ============================================================================.
* TEST 4: WEIGHTED / GROUPED
* ============================================================================.

TITLE '=========== WEIGHTED / GROUPED ==========='.

WEIGHT BY sampling_weight.

TITLE '=========== Test 4a ==========='.

* Test 4a: PCA with Varimax rotation, Kaiser criterion (weighted, grouped by region)
SUBTITLE 'Factor Analysis: Varimax, Kaiser criterion (weighted, grouped by region)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /ROTATION VARIMAX
  /METHOD=CORRELATION.

TITLE '=========== Test 4b ==========='.

* Test 4b: PCA with Oblimin rotation, Kaiser criterion (weighted, grouped by region)
SUBTITLE 'Factor Analysis: Oblimin, Kaiser criterion (weighted, grouped by region)'.
FACTOR
  /VARIABLES political_orientation environmental_concern life_satisfaction
   trust_government trust_media trust_science
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL KMO CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /ROTATION OBLIMIN
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
* KMO and Bartlett's Test:
*   - Kaiser-Meyer-Olkin Measure of Sampling Adequacy (overall KMO)
*   - Bartlett's Test of Sphericity: Approx. Chi-Square, df, Sig.
*   - Per-item KMO from Anti-image Correlation Matrix diagonal
*
* Communalities (for each variable):
*   - Initial (always 1.000 for PCA)
*   - Extraction
*
* Total Variance Explained (for each component):
*   - Initial Eigenvalues: Total, % of Variance, Cumulative %
*   - Extraction Sums of Squared Loadings: Total, % of Variance, Cumulative %
*   - Rotation Sums of Squared Loadings: Total, % of Variance, Cumulative %
*     (for Varimax; for Oblimin this section shows only Total)
*
* Component Matrix (unrotated loadings):
*   - Factor loadings for each variable on each extracted component
*
* Rotated Component Matrix (Varimax only):
*   - Rotated factor loadings for each variable
*
* Pattern Matrix (Oblimin only):
*   - Pattern coefficients (regression-style weights)
*
* Structure Matrix (Oblimin only):
*   - Structure coefficients (correlations with factors)
*
* Component Correlation Matrix (Oblimin only):
*   - Correlations between oblique factors
*
* Correlation Matrix:
*   - Full pairwise correlation matrix among all input variables
*
* Anti-image Matrices:
*   - Anti-image Correlation Matrix diagonal = per-item KMO values
*
* Tolerances for R tests:
*   - KMO (overall and per-item): +/-0.001
*   - Bartlett Chi-Square: +/-0.01
*   - Bartlett df: exact match
*   - Bartlett p-value: +/-0.0001
*   - Eigenvalues: +/-0.001
*   - Variance % explained: +/-0.01
*   - Cumulative %: +/-0.01
*   - Factor loadings (all matrices): +/-0.001
*   - Communalities: +/-0.001
*   - Factor correlations (Oblimin): +/-0.001
*   - Correlations: +/-0.001
*
* Special Considerations:
*   - Sign ambiguity: factor loadings can be reflected (column * -1)
*     The R test must account for this by comparing absolute values
*     or checking sign-flipped columns
*   - Oblimin delta: SPSS default is delta=0
*   - BLANK(.40) only affects display, not the actual computed values
*   - Kaiser criterion may yield different n_factors across groups
*   - Varimax rotation preserves orthogonality (factor correlations = 0)
*   - Oblimin allows correlated factors (non-zero correlations)
*   - For Oblimin: Pattern Matrix = unique factor contributions,
*     Structure Matrix = total correlations with factors
*
* Weighted Analyses:
*   - FACTOR with WEIGHT BY uses weighted correlation matrix
*   - Weighted N in descriptive statistics
*   - KMO and Bartlett's test based on weighted correlations
*   - Eigenvalues and loadings computed from weighted matrix
*
* Grouped Analyses:
*   - Separate factor analysis per region (East/West)
*   - Factor structure may differ between groups
*   - Smaller sample size per group (East ~500, West ~2000)
*   - Number of extracted factors may differ per group
*     (Kaiser criterion applied independently)
* ============================================================================.
