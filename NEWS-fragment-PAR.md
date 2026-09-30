* `t_test()`, `oneway_anova()`, `factorial_anova()`, `ancova()` and the
  post-hoc tests on their results order the groups of a numeric or
  labelled grouping variable by code, as SPSS does, and show value labels
  instead of codes. `t_test()` took the group order from the order of
  appearance in the data, so sorting the data flipped the sign of t and
  of the mean difference; labelled groups printed as "Groups compared:
  1 vs. 2", Tukey/Scheffe rows as "1 - 2", and factorial descriptives and
  ANCOVA marginal means by code. (PAR-02, PAR-10)
