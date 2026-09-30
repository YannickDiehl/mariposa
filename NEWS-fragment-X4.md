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
