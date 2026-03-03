# Calculate kurtosis using the SPSS sample-corrected formula

Uses the Type 2 (sample excess kurtosis) formula matching SPSS
FREQUENCIES output. For unweighted data: G2 = ((n+1)*g2 + 6) \* (n-1) /
((n-2)*(n-3)) where g2 = m4/m2^2 - 3 (population excess kurtosis). For
weighted data: Uses frequency-weighted N = sum(w) in place of n.
Reference: Joanes & Gill (1998), "Comparing measures of sample skewness
and kurtosis"

## Usage

``` r
.calc_kurtosis(x, w = NULL, excess = TRUE)
```

## Arguments

- x:

  Numeric vector of values (must have length \>= 4)

- w:

  Numeric vector of weights, or NULL for unweighted

- excess:

  If TRUE, return excess kurtosis; if FALSE, return raw kurtosis

## Value

Numeric scalar
