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
