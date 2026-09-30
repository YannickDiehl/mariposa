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
