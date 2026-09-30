* `efa()` components and factors now carry SPSS's sign convention. The
  eigenvectors kept the arbitrary sign `eigen()` returned, so three
  positively correlated trust items could load -0.597/-0.475/-0.678 on
  their single component and signs could flip between groups. Every
  extracted column is now reflected to a positive loading sum (as SPSS
  FACTOR does); rotated, pattern and structure matrices and factor
  correlations follow from the reflected solution. This reproduces every
  signed loading SPSS prints in the reference runs, now asserted in the
  SPSS validation tests.
* `efa(extraction = "ml")` no longer reports the PCA share of variance.
  The compact line read "Variance explained: 61.0%" (the eigenvalue share
  of three components) although the three ML factors explain about 24%,
  and it called ML factors "components". The result gains
  `$extraction_variance` (SPSS's "Extraction Sums of Squared Loadings");
  the compact line reports its cumulative percentage and says "factors"
  for ML. `summary()` prints "Total Variance Explained" as one aligned
  SPSS-style table (initial eigenvalues, extraction sums, rotation sums)
  instead of free-text lines whose columns shifted from the 10th
  component on.
* `efa()` no longer crashes with the base error "infinite or missing
  values in 'x'" (German: "unendliche oder fehlende Werte in 'x'") when a
  correlation cannot be computed. A constant item, an item without valid
  values, two items without cases in common, or too few complete cases
  now give an error that names the item(s) and the reason. Under
  `group_by()`, such a group is skipped with a warning naming the group,
  and every other group is still analysed (previously the whole grouped
  result was lost); `print()`/`summary()` show "not computed (...)" for it.
* `efa()` flags singular correlation matrices like SPSS ("not positive
  definite"). A duplicated item used to yield KMO 0.500 (from a
  pseudo-inverse), Bartlett's chi-square `Inf` and "Sig.: 0.000", and
  fewer cases than variables gave KMO `NaN`. Now a warning names the
  perfectly correlated items or the case shortage, KMO and Bartlett's
  test are reported as not computed, and ML extraction stops with a clear
  message instead of "Lapack routine dgesv: system is exactly singular".
  A separate warning appears when there are no more cases than variables.
  Bartlett's and the goodness-of-fit significance use the SPSS style
  ("<.001") instead of "0.000".
