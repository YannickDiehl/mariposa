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
* `mann_whitney(mu = , alternative = )`: U, Z, r and the p-value now
  refer to the same hypothesis. With `mu` the statistics were computed
  for a shift of 0 while the p-value came from `wilcox.test(mu = )`
  ("Z = -0.696, p < 0.001"); group-1 values are now shifted by `mu`
  before ranking, as `wilcox.test()` does. For one-sided tests Z is
  directional (positive when group 1 tends to be larger) so that its sign
  matches the p-value (was Z = -0.226 next to p(less) = .589); the
  two-sided Z keeps the SPSS convention. The weighted (design-based) test
  supports only `mu = 0` and now says so instead of printing
  "Null hypothesis (mu): 500" for an unshifted test.
* The unused confidence interval of `mann_whitney()` is no longer
  computed: `wilcox.test(conf.int = TRUE)` ran for every variable and
  was discarded (the source of German "cannot compute confidence
  interval" warnings for a constant variable), and `summary()` no longer
  advertises a "Confidence level: 95.0%" for which no interval exists.
  `conf.level` of `mann_whitney()`, `kruskal_wallis()`,
  `wilcoxon_test()` and `friedman_test()` is documented as not used.
* `pairwise_wilcoxon()`: the interpretation legend and help page now match
  the sign of Z. Z is computed from the differences second minus first
  variable (like `wilcoxon_test(x, y)` and the SPSS pair "var2 - var1"),
  so a positive Z means the *second* variable tends to be higher; the
  legend said the opposite (score_T1 vs score_T2: Z = +5.43 while T2 is
  higher).
* `pairwise_wilcoxon()` reports the number of cases of every pair (new
  `n` column, shown in `summary()`) and explains why it can exceed the
  Friedman N: each pair uses all cases with both values (pairwise
  deletion, exactly like the SPSS `/WILCOXON` tests the results are
  validated against), while `friedman_test()` uses complete cases. The
  comparison table prints p-values in SPSS style (`<.001`, `.123`).
* `fisher_test()` handles larger tables: instead of aborting with the raw
  "FEXACT error 501 ... hash table key cannot be computed" it now falls
  back to a Monte Carlo p-value with a warning (SPSS offers the same
  "Monte Carlo" option next to "Exact"). The new arguments
  `simulate.p.value` and `B` (default 10000 replicates, the SPSS default)
  choose it directly; before, `simulate.p.value = TRUE` was silently
  swallowed by `...`. A group that cannot be tested under `group_by()`
  is reported with a warning instead of a silent `NA` row.
* The rank tests treat ordered factors consistently as ordinal: they are
  ranked by their level order. `mann_whitney()` aborted with "'x' must be
  numeric" and `wilcoxon_test()` with "'-' not meaningful for factors"
  (printing "Z = ,"), while `kruskal_wallis()` and `friedman_test()`
  accepted them. A nominal (unordered) factor or character variable is
  now a clear error in all four tests instead of running silently
  (`kruskal_wallis(gender, group = education)`) or failing with "not
  computed for this group" outside any `group_by()`.
* `group_by() %>% binomial_test()` no longer aborts because the variable
  has only one category in one group: that group gets an `NA` row, a
  warning naming the group and the reason, and the output says "not
  computed (gender has 1 observed category; ...)". An ungrouped single
  variable still stops with a clear error.
* Degenerate cases in the rank tests print a reason instead of broken
  text. `wilcoxon_test()` with identical variables reports Z = 0 and
  p = 1 as SPSS does (was "Z = ," and `NA`); a grouped `friedman_test()`
  whose group cannot be tested no longer prints "chi2(NA) = ,  , W = ,
  N = NA" and its warning names the group; a constant variable in
  `kruskal_wallis()`/`mann_whitney()` is reported ("all values of x are
  identical") instead of "(see warning)" without a warning or German
  "cannot compute confidence interval" warnings; an all-missing variable
  says "x has no valid values" instead of blaming the grouping variable
  ("Found 0 groups ... use a Kruskal-Wallis test"). "not computed for
  this group" appears only under `group_by()`; otherwise the output says
  "not computed (reason)".
* Labelled (SPSS) variables show their value labels instead of codes in
  `kruskal_wallis()` ("Groups: 1, 2, ..., 7"), the pairs of
  `dunn_test()`, `mann_whitney()` ("1 vs. 2"), `binomial_test()`
  ("Group 1 (1)") and the categories of `chisq_gof()`, as `chi_square()`
  already did. Grouping variables are ordered by code, as in SPSS: a
  numeric 0/1 group was ordered by first appearance ("1 vs. 0" while the
  ranks listed 0 first).
* `fisher_test()` output: the contingency table is labelled with the
  variable names and value labels (was "r"/"cc" and codes), empty factor
  levels are dropped, grouped `summary()` shows each group's table (it
  was dropped), the compact line uses 3 decimals like the rest of the
  family (was "p = 0.5435") and reports the odds ratio with its 95% CI for
  2x2 tables (SPSS "Risk Estimate": sample odds ratio, Woolf interval).
* `mcnemar_test()` output: without discordant pairs the output says
  "chi2 not computed (no discordant pairs), p = 1.000 (exact)" instead
  of "chi2 = ,  (asymp)"; tables are labelled with the variable names and
  value labels (was "v1"/"v2" and codes); grouped `summary()` shows each
  group's table and the "(cc)" continuity-correction marker (both were
  dropped); the compact line reports the degrees of freedom
  ("chi2(1) = ..."). Two variables with different category sets (e.g.
  0/1 against 1/2) are now an error instead of being tabulated as if the
  categories matched.
* `mann_whitney()` without `group` stops with "`group` is required" (as
  `kruskal_wallis()` does) instead of the internal "Can't extract column
  with `g_name`".
* Output of the rank and exact tests is formatted consistently.
  `summary()` tables print p-values in SPSS style ("<.001", ".026")
  instead of a bare "p value 0" (Kruskal-Wallis, Wilcoxon, Friedman,
  binomial, Mann-Whitney), leave empty cells blank instead of "NA"
  ("Total 2500 NA", "Ties 502 NA NA"), show counts as integers (Mann-
  Whitney "n = 1149.0") and give every column a fixed number of decimals
  (0.52 next to 0.537, Mean Rank 19 next to 40.19, expected 538.24 next
  to 26.239). Compact lines label Kruskal-Wallis epsilon-squared and
  Kendall's W with an interpretation, `mann_whitney()` ends with the
  "Use summary()" hint, and Mann-Whitney and Wilcoxon share one set of
  r thresholds. The `dunn_test()` comparison table no longer wraps at 80
  characters with long group labels, and a group or variable that
  `dunn_test()` cannot compare is reported with a warning naming the
  group.
* Skipped groups are reported consistently: `mann_whitney()` says how
  many groups with valid values a split has ("gender has 1 group ...")
  instead of suggesting a Kruskal-Wallis test, and `pairwise_wilcoxon()`
  names the group in its warnings; a pair whose values are all tied gets
  Z = 0, p = 1 (as `wilcoxon_test()` and SPSS) instead of a silent `NA`.
