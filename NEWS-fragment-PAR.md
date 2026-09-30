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
