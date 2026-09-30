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
