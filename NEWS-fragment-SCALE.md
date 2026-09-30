* `efa()` components and factors now carry SPSS's sign convention. The
  eigenvectors kept the arbitrary sign `eigen()` returned, so three
  positively correlated trust items could load -0.597/-0.475/-0.678 on
  their single component and signs could flip between groups. Every
  extracted column is now reflected to a positive loading sum (as SPSS
  FACTOR does); rotated, pattern and structure matrices and factor
  correlations follow from the reflected solution. This reproduces every
  signed loading SPSS prints in the reference runs, now asserted in the
  SPSS validation tests.
