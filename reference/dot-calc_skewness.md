# Calculate skewness using the SPSS Type-2 sample-corrected formula

For unweighted data: type1_skew \* sqrt(n\*(n-1)) / (n-2) where
type1_skew = m3 / m2^(3/2) (population skewness). For weighted data:
substitutes n = sum(w) per SPSS frequency-weight convention. Reference:
Joanes & Gill (1998).

## Usage

``` r
.calc_skewness(x, w = NULL)
```

## Arguments

- x:

  Numeric vector (length \>= 3)

- w:

  Numeric vector of weights, or NULL

## Value

Numeric scalar
