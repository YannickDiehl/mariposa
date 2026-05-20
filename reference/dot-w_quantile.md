# Weighted quantiles (SPSS Type-6 HAVERAGE with linear interpolation)

For weighted data, weights are treated as frequency multipliers. The
Type-6 position is h = p \* (W + 1) where W = sum(w). The percentile is
linearly interpolated between the values whose cumulative weights
bracket h. Matches IBM SPSS FREQUENCIES /PERCENTILES (HAVERAGE)
algorithm.

## Usage

``` r
.w_quantile(x, weights = NULL, probs = 0.5, na.rm = TRUE)
```

## Details

For unweighted data, delegates to stats::quantile(type = 6) which is the
SPSS-compatible default (HAVERAGE) — base R's default is type = 7.
