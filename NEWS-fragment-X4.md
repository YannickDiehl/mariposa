* `ancova()`: Levene's test of equality of error variances now matches
  SPSS UNIANOVA. It was computed on the deviations of the dependent
  variable from its raw cell means, ignoring the covariates
  (`life_satisfaction BY gender WITH age`: F = 1.277, p = .258 instead of
  SPSS 1.306, p = .253). SPSS tests the absolute residuals of the full
  model (covariates and factors) across the design cells; mariposa now does
  the same and reproduces all six unweighted Levene tests of the SPSS
  reference run. The weighted path is unchanged until the pending weighted
  reference run. New `levene_test()` method for `ancova()` results
  (`ancova(...) |> levene_test()`, also per group under `group_by()`).
* `efa(rotation = "promax")` now matches SPSS FACTOR `/ROTATION PROMAX`.
  mariposa used `stats::promax()`, which builds the promax target from the
  raw varimax loadings; SPSS first normalizes every row (Kaiser
  normalization). Pattern loadings differed by up to .03 and the component
  correlations were .055 / .155 instead of SPSS -.002 / -.012. The new
  implementation follows the SPSS algorithm and reproduces every pattern,
  structure and correlation matrix of the SPSS reference runs (unweighted,
  weighted, per region), now asserted in the validation suite.
* `efa()` with an oblique rotation (promax, oblimin): the "Rotation Sums of
  Squared Loadings" are now the sums of squares of the structure matrix, as
  SPSS reports them (they were taken from the pattern matrix: 1.604 / 1.065
  / 1.045 instead of SPSS 1.599 / 1.039 / 1.021).
* `efa()` varimax and oblimin rotations now use SPSS FACTOR's own
  algorithms and stopping rules (Kaiser's cyclic pairwise varimax; the
  Jennrich-Sampson direct oblimin, delta = 0; at most 25 iterations).
  `stats::varimax()` stopped earlier and missed SPSS's component
  transformation matrix by up to .004 (2-factor solution: 26.653 % instead
  of SPSS 26.655 % for the first rotated component);
  `GPArotation::oblimin()` iterated past SPSS's stopping point (weighted
  solution: pattern loadings off by up to .002). The rotated factors are
  reflected to a positive sum and ordered by their sums of squares as in
  SPSS. Every varimax, oblimin and promax reference solution (unweighted,
  weighted, per region) now matches SPSS, including SPSS's iteration
  counts, which `summary()` prints as SPSS does ("Rotation converged in 4
  iterations."). Oblimin no longer needs the GPArotation package.
* `efa(extraction = "ml")`: a Heywood variable is now bounded at a
  communality of .999, as SPSS FACTOR does (it was .995, the default bound
  of `stats::factanal()`), and the factors keep SPSS's order (by the
  eigenvalues of the rescaled correlation matrix; `factanal()` re-sorted
  them by sums of squares, so the unrotated factor matrix and the
  extraction sums came in a different order). Where SPSS's ML iteration
  converges (reference runs per region, West), the whole solution now
  matches SPSS: communalities, factor and rotated factor matrices,
  extraction and rotation sums, transformation matrix. The remaining
  reference runs end without convergence in SPSS itself (a model with
  0 degrees of freedom and Heywood cases); see Validation.
* `levene_test()` compact print: a whole-number weighted `df2` above 2^31
  (sums of weights in the billions) printed as `F(1, NA)` with an integer
  overflow warning; it is now printed in full.

## Validation (for the lead: NEWS "## Validation" subsection)

* New Tier-3 exceptions for `efa()` ML extraction (to be added to
  `.claude/VALIDATION_EXCEPTIONS.md`): EXC-001 (±.002; SPSS stops its
  Newton-Raphson iteration at ECONVERGE(.001), so loadings, sums of
  squares and percentages of variance of converged solutions agree to
  about 1e-3) and EXC-002 (±.05; SPSS reference runs 5a/6a that end
  without convergence, "More than 25 iterations required").
* `efa()` rotations are now validated in full against SPSS: 8 varimax,
  6 oblimin and 6 promax solutions (transformation, pattern, structure and
  correlation matrices, rotation sums, iteration counts); the ML initial
  communalities of all 6 ML reference runs; `ancova()` Levene tests of the
  6 unweighted reference runs.
* Weighted `ancova()`: Levene's test uses sqrt(w) * |WLS residual| of the
  full model, as SPSS UNIANOVA does with /REGWGT. This reproduces all five
  weighted SPSS references exactly (e.g. 0.902 instead of 0.880) and
  restores the rule that `weights = 1` gives the unweighted result (the
  residual-based unweighted test made the old cell-mean-based weighted
  test disagree with it).
