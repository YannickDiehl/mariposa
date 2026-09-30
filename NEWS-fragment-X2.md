* `dunn_test()` and `pairwise_wilcoxon()` store their comparison table as
  `$results`, like every other result class (it lived only in
  `$comparisons`, so `x$results` was `NULL`; `$comparisons` is kept). A
  weights-invariance regression test that compared these `NULL`s with
  each other now checks the real comparisons.
