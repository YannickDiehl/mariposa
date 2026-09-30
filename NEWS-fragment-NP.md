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
* `goodman_gamma()` and the gamma row of `chi_square()` are dramatically
  faster and their p-value now matches SPSS. The concordant/discordant
  counts came from a quadruple R loop over the table (`cramers_v(survey_data,
  age, income)` took ~50 s, because `chi_square()` always computes gamma);
  they now come from 2-D cumulative sums (0.03 s). The loop also counted
  only the pairs below each cell, which mis-stated the null-hypothesis
  standard error (ASE0): the approximate significance of gamma was wrong
  (education x employment: p = .122 where SPSS prints .027). The gamma
  value itself is unchanged.
* `chi_square(correct = TRUE)` computes Phi, Cramer's V and the
  contingency coefficient from the Pearson chi-square, as SPSS does; they
  were computed from the Yates-corrected statistic (gender x region: 0.0119
  instead of 0.0129). The Pearson statistic is kept in
  `pearson_chi_squared`/`pearson_p_value`, `summary()` shows both rows
  ("Pearson Chi-Square" and "Continuity Correction") like SPSS, and the
  compact `print()` marks the statistic as "(continuity-corrected)". The
  compact line now spells out "negligible" (was "neglig.") and ends with
  the "Use summary()" hint.
* `summary()` of `chi_square()` follows the SPSS "Symmetric Measures"
  table: Phi and Cramer's V are shown for every table (Phi was hidden
  outside 2x2 although `phi()` returned it and SPSS prints it), and
  Goodman's gamma - with its verbal label - only when both variables are
  ordinal (ordered factor or numeric). For nominal variables such as
  gender x region its sign depends on the arbitrary category order.
  `goodman_gamma()` still computes gamma on request.
* `chi_square()` and the effect-size helpers warn once about expected
  counts below 5. Base `chisq.test()` added its own "Chi-squared
  approximation may be incorrect" warning (German: "Chi-Quadrat-
  Approximation kann inkorrekt sein") next to mariposa's message; that
  specific warning is now muffled in every locale.
