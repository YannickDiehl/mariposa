* `t_test()`, `oneway_anova()`, `factorial_anova()`, `ancova()` and the
  post-hoc tests on their results order the groups of a numeric or
  labelled grouping variable by code, as SPSS does, and show value labels
  instead of codes. `t_test()` took the group order from the order of
  appearance in the data, so sorting the data flipped the sign of t and
  of the mean difference; labelled groups printed as "Groups compared:
  1 vs. 2", Tukey/Scheffe rows as "1 - 2", and factorial descriptives and
  ANCOVA marginal means by code. (PAR-02, PAR-10)
* A dependent variable without variance is no longer "tested".
  `oneway_anova()` on a constant variable reported F = 7256 *** (weighted)
  or F = 0.992 computed from floating-point noise in sums of squares that
  are exactly 0, `factorial_anova()` F = 4.011 *, and `tukey_test()`
  adjusted p-values. `oneway_anova()`, `t_test()`, `tukey_test()` and
  `scheffe_test()` now report such a variable (and one without any
  non-missing value) as "not computed (<reason>)" with a warning naming the
  variable; the other variables of the call are still tested.
  `factorial_anova()` and `ancova()` stop with a clear error. `t_test()`
  no longer aborts a multi-variable call with the raw base-R message
  (German locale: "Daten sind praktisch konstant", printed twice as
  "t_test() failed: ... / Caused by: ..."), no longer reports an all-missing
  variable as a grouping problem ("Found 0 levels"), and a grouping
  variable with 3 or more groups gives one error that points to
  `oneway_anova()`. (PAR-01, PAR-24)
* A group with a single case (weighted: a sum of weights <= 1) no longer
  aborts `oneway_anova()` and `t_test()` with the raw base-R error "not
  enough observations" from the Welch part. As in SPSS, the classical
  ANOVA / Student's t-test is computed and Welch's test is marked "not
  computed" with the reason; `t_test(var.equal = FALSE)` then reports
  Student's t with a warning. A group with zero variance no longer
  prints a Welch row "NaN 3 NaN NA <NA>" (weighted) or an infinite Glass'
  Delta, and the Welch block of `summary(oneway_anova())` is titled
  "Robust Tests of Equality of Means" (SPSS) instead of "Assumption
  Tests", with df2 shown with decimals (1229.456, not 1229). (PAR-07,
  PAR-17)
* Grouped `oneway_anova()`, `tukey_test()`, `scheffe_test()` and
  `levene_test()`: a group that cannot be tested (e.g. only one level of
  `group` present in that group) is reported with a warning that names the
  group label and the reason. `oneway_anova()` printed a silent "Results
  not available", the post-hoc tests dropped the group without a word,
  and `levene_test()` warned "in group 1" (the factor code). (PAR-18,
  EDGE-13)
* One-sample `t_test()` follows the SPSS One-Sample Test: `mean_diff` is
  now the mean minus the test value `mu` (was: the mean itself, e.g.
  3.628 instead of 0.628) and `conf_int_lower`/`conf_int_upper` are the
  confidence interval of that difference. The weighted interval now
  follows `alternative` (it was always two-sided). `summary()` shows the
  test value, alternative, confidence level and a One-Sample Statistics
  table (N, Mean, Std. Deviation, Std. Error Mean) and no longer prints an
  effect-size legend for effect sizes a one-sample test does not have.
  (PAR-05, PAR-06)
* `summary(t_test())` prints SPSS-style tables: a Group Statistics table
  (N, Mean, Std. Deviation, Std. Error Mean per group; SD and SE were
  missing) and an Independent Samples Test table with t, df, p, mean
  difference, its standard error and the confidence interval for both
  variance assumptions. Significance stars now come from the exact
  p-value (p = 0.0008 was rounded to 0.001 first and got "**", p = 0.0497
  printed as "0.05" without a star); p-values print as "<.001"/".308"
  instead of a bare "0", df as 2419 / 2384.147, weighted N as whole
  numbers ("1149", not "1149.0"), and `digits` applies to every column.
  Tables with umlaut labels stay aligned. (PAR-15, PAR-21, PAR-26)
* `summary(oneway_anova())` prints an SPSS-style Descriptives table (N,
  Mean, Std. Deviation, Std. Error and the confidence interval of each
  group mean at `conf.level`) and formats the ANOVA, Welch and effect-size
  tables with fixed decimals and `digits`. With `conf.level = 0.99` the
  header said 99% but no interval was printed anywhere; weighted N printed
  as "618.0", Mean Square as "289.79" next to "1.077", and `digits` had no
  effect. `tukey_test()` and `scheffe_test()` on a `oneway_anova()` result
  now default to the ANOVA's `conf.level` instead of silently using 95%.
  (PAR-19, PAR-21)
* `group_by() %>% levene_test()` takes its variables through `...` like
  the ungrouped method: several variables, tidyselect helpers
  (`starts_with("trust")`) and `group = "education"` as a string work, and
  `group`/`weights` must be named. The old grouped method had the
  signature `(x, variable, group, weights)`, so a second variable was
  silently used as `weights` ("[Weighted] F(3, 1544896) = 40250"). Grouped
  results now carry the group keys as columns: two `group_by()` variables
  no longer print "region = East, gender = East", and a missing key (`NA`)
  is its own group instead of a false "constant values" warning with
  "F(NA, NA) = ,". An invalid `center` is an error, a variable without
  variance prints "not computed (no variance ...)", and `weights` follow
  the package policy (negative or non-numeric weights are an error; they
  were accepted). (PAR-03, PAR-12, EDGE-04)
* `summary(levene_test())` prints one SPSS-style table per group (Levene
  Statistic, df1, df2, Sig.) with fixed decimals and `digits` (p printed as
  a bare 0, df2 as 474.2032 next to 3), and its recommendation fits the
  design: Welch's ANOVA for three or more groups, Welch's t-test only for
  two groups, a caution for factorial designs. It used to recommend
  "Welch's t-test" after `oneway_anova()` and `factorial_anova()`. The
  compact line no longer ends in "p = 0.125 , variances equal". (PAR-14,
  PAR-21)
* `tukey_test()` and `scheffe_test()` report every comparison in the SPSS
  "(I) - (J)" orientation (I before J in the category order, difference =
  mean(I) - mean(J)) with a spaced separator, identically for the
  unweighted, weighted, Scheffe and `factorial_anova()` paths, and add the
  standard error of the difference. Unweighted Tukey rows came from
  `TukeyHSD()` as "Intermediate Secondary-Basic Secondary 0.497" (later
  minus earlier, unspaced) while weighted Tukey and Scheffe printed
  "Basic Secondary - Intermediate Secondary -0.490". The comparison tables
  stay aligned with umlaut labels (they were padded by bytes). (PAR-11,
  PAR-22)
