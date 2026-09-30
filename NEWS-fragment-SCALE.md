* `efa()` components and factors now carry SPSS's sign convention. The
  eigenvectors kept the arbitrary sign `eigen()` returned, so three
  positively correlated trust items could load -0.597/-0.475/-0.678 on
  their single component and signs could flip between groups. Every
  extracted column is now reflected to a positive loading sum (as SPSS
  FACTOR does); rotated, pattern and structure matrices and factor
  correlations follow from the reflected solution. This reproduces every
  signed loading SPSS prints in the reference runs, now asserted in the
  SPSS validation tests.
* `efa(extraction = "ml")` no longer reports the PCA share of variance.
  The compact line read "Variance explained: 61.0%" (the eigenvalue share
  of three components) although the three ML factors explain about 24%,
  and it called ML factors "components". The result gains
  `$extraction_variance` (SPSS's "Extraction Sums of Squared Loadings");
  the compact line reports its cumulative percentage and says "factors"
  for ML. `summary()` prints "Total Variance Explained" as one aligned
  SPSS-style table (initial eigenvalues, extraction sums, rotation sums)
  instead of free-text lines whose columns shifted from the 10th
  component on.
* `efa()` no longer crashes with the base error "infinite or missing
  values in 'x'" (German: "unendliche oder fehlende Werte in 'x'") when a
  correlation cannot be computed. A constant item, an item without valid
  values, two items without cases in common, or too few complete cases
  now give an error that names the item(s) and the reason. Under
  `group_by()`, such a group is skipped with a warning naming the group,
  and every other group is still analysed (previously the whole grouped
  result was lost); `print()`/`summary()` show "not computed (...)" for it.
* `efa()` flags singular correlation matrices like SPSS ("not positive
  definite"). A duplicated item used to yield KMO 0.500 (from a
  pseudo-inverse), Bartlett's chi-square `Inf` and "Sig.: 0.000", and
  fewer cases than variables gave KMO `NaN`. Now a warning names the
  perfectly correlated items or the case shortage, KMO and Bartlett's
  test are reported as not computed, and ML extraction stops with a clear
  message instead of "Lapack routine dgesv: system is exactly singular".
  A separate warning appears when there are no more cases than variables.
  Bartlett's and the goodness-of-fit significance use the SPSS style
  ("<.001") instead of "0.000".
* `efa()` shows its sample size. `print()` adds "N = 2168 (smallest
  pairwise)" (or "(listwise)"), `summary()` an N line and SPSS's
  "Descriptive Statistics" table (mean, SD, analysis N, missing N; new
  toggle `descriptives`). With `use = "complete"`, `$item_statistics` now
  describes the complete cases the analysis uses; the analysis N was
  pairwise before.
* `efa()` input and output details: `n_factors = 2.7` is an error instead
  of being truncated to 2; `use = "listwise"` (the SPSS term) is accepted
  as an alias of `"complete"`, and invalid `rotation`/`extraction`/`use`
  values give an English error instead of a translated `match.arg()`
  message. A requested rotation of a single component is no longer
  dropped silently: output says "Only one component was extracted. The
  solution cannot be rotated." as SPSS does. Communalities print with
  fixed decimals ("1.000" instead of "1" next to "0.457"), and the
  summary says "N of Components" for PCA.
* `reliability(na.rm = FALSE)` no longer crashes with "missing value where
  TRUE/FALSE needed" (German: "Fehlender Wert, wo TRUE/FALSE nötig ist")
  as soon as a value is missing. All statistics are `NA`, a warning names
  the items with missing values and points to `na.rm = TRUE` (listwise
  deletion, as SPSS), and `print()`/`summary()` say "not computed (...)"
  instead of "Cronbach's Alpha = NA ()".
* `reliability()` warnings are clearer. An item with zero variance is
  removed from the scale with a warning naming it, as SPSS RELIABILITY
  does; it used to stay in (alpha 0.042 instead of 0.047, standardized
  alpha `NA`) next to the German base warnings "Standardabweichung ist
  Null" and "NaNs wurden erzeugt". When omega cannot be computed (e.g. a
  duplicated item makes the correlation matrix singular), the warning
  says why in English and names the perfectly correlated items instead
  of relaying factanal's translated error. The "omega requires at least
  3 items" warning appears once per call instead of once per group, and
  "Insufficient data (n = 0)" now names the group and the items without
  valid values.
* `reliability()` no longer reports McDonald's omega from a Heywood
  solution. For the trust items in the East region the one-factor model
  put one uniqueness at its lower bound (loading of about 1), and omega
  0.349 was printed next to alpha 0.037 without comment. Omega is now
  `NA` in such cases, with a warning that names the item and the group.
* `reliability()` flags a negative Cronbach's alpha like SPSS: "The value
  is negative due to a negative average covariance among items ... check
  item codings." A warning and the `summary()` footnote name the items
  with a negative corrected item-total correlation (usually items that
  need reverse-coding; new `$negative_items`), and the compact print says
  "negative; check item coding" instead of classifying alpha -0.929 as
  "Poor".
* `reliability()` output is easier to read. The Item-Total Statistics
  table no longer wraps at 80 columns under snake_case headers
  (`scale_mean_deleted`, `corrected_r`, ...); it has SPSS-style two-line
  headers ("Scale Mean / if Deleted", "Alpha if / Deleted", ...). The
  inter-item correlation matrix honours `digits` for more than six items
  (it was forced to 2 decimals) and uses numbered columns so it stays
  narrow. Item statistics print with fixed decimals (no more "1.16" next
  to "2.615"), and a missing omega reads "not computed" with the reason
  instead of "NA".
* `reliability()` and `efa()` show variable labels, as SPSS does.
  `summary()` lists every item with its full label, and the per-item
  tables (item statistics, communalities, loading matrices) add the
  label next to the name, shortened with "..." so that rows fit the
  console width; wide tables keep the names only. Labels with umlauts
  stay aligned. The labels are stored in `$variable_labels`.
