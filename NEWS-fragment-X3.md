### Across functions (batch X3: formatting and weighted-formula hygiene)

* Result tables stay aligned with umlauts and other multi-byte characters
  in variable names, value labels and terms (Tukey/Scheffe, Dunn,
  pairwise correlations, regression coefficients, Kruskal-Wallis ranks,
  goodness-of-fit and many more): the shared table printer padded cells
  with `sprintf("%-20s")`, which counts bytes, so every umlaut shifted the
  rest of its row one column to the left. Cells are now padded by display
  width. Counts and sums of weights of 2^31 or more (expansion weights)
  print as whole numbers instead of `NA` with a coercion warning; leading
  label columns such as "Group 2" are left-aligned like "Group 1"; table
  lines no longer end in a blank.
* Internal: the legacy correlation-matrix printer `.print_cor_matrix()`
  (temporary `options(width)` changes, 2 decimals above 6 variables, a
  `0.0000` p-value diagonal) and the unused `.print_single_pair()` are
  removed; `reliability()` and the correlation functions have their own
  console-fitting matrix printers.
* Sums of weights of 2^31 or more (expansion weights) no longer print
  as `N = NA` in the compact lines of the rank tests and the
  goodness-of-fit test, as `NA` in the N column of the one-sample t-test,
  and no longer raise integer-coercion warnings in the t-test, one-way
  ANOVA, factorial ANOVA/ANCOVA, Levene and reliability tables: every
  count is formatted as a whole number without integer coercion.
* Grouped output uses one group-header style in every verbose table:
  `describe()`, the `w_*()` functions and the summaries of
  `levene_test()`, `normality_test()`, `marginal_effects()` and the
  post-hoc tests printed "Group: region = East " with a trailing blank and
  no underline, all other summaries an underlined header. Section titles
  without a suffix ("Pearson Correlation", "Chi-Squared Test of
  Independence", "Levene's Test for Homogeneity of Variance") no longer end
  in a blank with an underline one dash too long.
* Internal: the weighted covariance/correlation formulas used by
  `reliability()` and `efa()` (`.weighted_cov()`, `.weighted_cor()`,
  `.weighted_cor_vec()`) moved from `R/reliability.R` to
  `R/kernels-weighted.R`, the single home of every weighted formula; a
  static test keeps `.weighted_*` definitions out of other files. Results
  are unchanged.
* Counts carry no thousands separators anywhere, as in SPSS tables and
  the console: the HTML `codebook()` header wrote "2,500 observations",
  `mann_whitney()`'s compact line "U = 776,732" (its summary table
  776732). The N/discordant-pair lines of `fisher_test()`,
  `mcnemar_test()`, `chi_square()` and the Friedman note of
  `pairwise_wilcoxon()` use the same whole-number formatter (the latter
  printed "N = 9.36e+09" for large sums of weights).
* Weighted `frequency()` of a factor together with a numeric variable
  (`frequency(survey_data, education, life_satisfaction, weights =
  sampling_weight)`) no longer turns the numeric variable's categories
  into `NA` (listed under "Total missing") with the base-R warning
  "invalid factor level, NA generated": the weighted branch kept the
  factor as the value column when the tables were combined.
* Variable pairs are joined by an ASCII "x" in every output: the titles
  of `chi_square()`, `fisher_test()`, `mcnemar_test()` and `crosstab()`
  and the chi-square "Table size" line used the multiplication sign
  (U+00D7), the correlation, factorial and post-hoc output an "x".
* `print()` of `linear_regression()`, `logistic_regression()` and
  `normality_test()` results accepts `digits` like every other compact
  print (R-squared, statistics and p-values ignored it). The grouped
  `normality_test()` compact print shows the test results under a
  `[group]` line for every group instead of only the number of groups.
