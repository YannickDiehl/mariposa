# Print a correlation/p-value/sample-size matrix with adaptive console width

Print a correlation/p-value/sample-size matrix with adaptive console
width

## Usage

``` r
.print_cor_matrix(
  mat,
  digits = 3,
  title = "Correlation Matrix:",
  type = "correlation"
)
```

## Arguments

- mat:

  Named numeric matrix

- digits:

  Number of decimal places

- title:

  Section title printed above the matrix

- type:

  One of "correlation", "pvalue", or "n"
