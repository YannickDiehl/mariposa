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
