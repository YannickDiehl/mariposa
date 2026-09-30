* `chisq_gof(expected = )` is applied to every selected variable. With
  several variables it was silently dropped (equal proportions were
  tested while the summary header still showed the custom proportions);
  a variable whose categories do not fit `expected` is now a clear error.
  A named `expected` vector is matched by category name instead of by
  position (`c(Female = .3, Male = .7)` gave Male 30%). As with SPSS
  `/EXPECTED=50 30 20`, counts or other relative frequencies are accepted
  and divided by their sum; proportions that sum to about 1 (e.g. 0.995
  from rounding) are rescaled with a message instead of shrinking every
  expected count, and proportions that clearly do not sum to 1 are an
  error. Categories with an expected count below 5 now trigger a warning,
  as in `chi_square()`, and a group that cannot be tested is reported
  with a warning naming the group instead of an unexplained `NA` row.
* `chi_square()`, `phi()`, `cramers_v()`, `goodman_gamma()` and
  `chisq_gof()` use the categories that actually occur in the data, as
  SPSS does. An empty factor level (typically left over after
  `filter()`) made chi-square, V and gamma `NaN` - also the cause of the
  silent `NA` row of grouped `chi_square()` - and gave `chisq_gof()` a
  phantom category with an extra degree of freedom (chi2 = 1257.5
  instead of 5.0). A constant variable no longer crashes
  `chi_square()` and the effect-size helpers ("replacement has length
  zero"): the result is `NA` with a warning naming the variable (and
  group), and the output says "not computed (x has only one observed
  category)". The `summary()` tables show full value labels (no longer
  cut at 20 characters) and fixed decimals.
