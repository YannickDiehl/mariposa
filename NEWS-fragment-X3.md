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
