# Compute descriptive statistics for regression variables

For factor predictors entered as dummies (the default), this still
reports Mean/SD of the integer-coded factor levels — that matches what
SPSS `REGRESSION` prints in its Descriptive Statistics block
(ordinal-as- scale summary), regardless of how the factor is entered
into the model.

## Usage

``` r
.lm_descriptives(data, var_names, weights_vec)
```
