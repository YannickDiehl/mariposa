# Compute weighted covariance matrix (SPSS-compatible)

Uses frequency-weighted formula: cov = sum(w\*(x-mx)\*(y-my)) / (V1 - 1)

## Usage

``` r
.weighted_cov(mat, w)
```
