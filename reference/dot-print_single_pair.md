# Print a single correlation pair in detailed block format

Used by pearson_cor, spearman_rho, and kendall_tau print methods for
two-variable analyses.

## Usage

``` r
.print_single_pair(
  corr_row,
  stat_label,
  stat_col,
  corr_name = "Correlation",
  secondary_label = NULL,
  secondary_col = NULL,
  ci_lower_col = NULL,
  ci_upper_col = NULL,
  alternative = NULL
)
```

## Arguments

- corr_row:

  One-row data.frame with the correlation results

- stat_label:

  Display label for the statistic (e.g. "r", rho, tau)

- stat_col:

  Column name holding the correlation value

- corr_name:

  Full name for the statistic line (e.g. "Correlation", "Spearman's
  rho")

- secondary_label:

  Optional label for a second statistic

- secondary_col:

  Optional column name for the second statistic

- ci_lower_col:

  Column name for CI lower bound (NULL to skip)

- ci_upper_col:

  Column name for CI upper bound (NULL to skip)

- alternative:

  NULL or "two.sided"/"less"/"greater" - shown as suffix on p-value
