# SPSS Compatibility

The statistical functions of **mariposa** are checked against reference
output of IBM SPSS Statistics 29. This article lists, for each function,
how many SPSS values its tests compare and which results are not
compared with SPSS.

*mariposa 0.7.4, status of 2026-10-19.*

## Summary

- **42** of 53 statistical functions are compared with SPSS output, with
  **14,622** SPSS values in total.
- **2** documented algorithmic differences (see below).

## How results are compared

Each comparison uses the precision with which SPSS prints the value:

- **Exact**: counts, degrees of freedom, and p-values that SPSS prints
  as `.000` (p \< .001).
- **Printed precision**: all other values agree with SPSS to within half
  a unit of the last decimal SPSS prints (one unit for p-values).
- **Documented difference**: where SPSS and mariposa use different
  algorithms by design, the values agree within the tolerance listed
  under “Documented differences”.

The reference values come from SPSS syntax runs on the `survey_data`
example data. The comparisons run with the package’s test suite on every
change; the SPSS syntax and output are part of the source repository
([tests/spss_reference](https://github.com/YannickDiehl/mariposa/tree/main/tests/spss_reference)).

## Results by function

The `w_*()` functions share one set of SPSS references; each row shows
that total. The last column lists results that are not compared with
SPSS output. They are verified by other means: the weighted results of
the rank-based tests reproduce the unweighted results exactly when all
weights equal 1 (the weighted Mann-Whitney test also agrees with
[`survey::svyranktest()`](https://rdrr.io/pkg/survey/man/svyranktest.html)),
and the remaining results are checked against published formulas or
independent implementations.

| Function | SPSS values | Exact | Printed precision | Documented difference | Not compared with SPSS |
|----|---:|---:|---:|---:|----|
| [`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md) | 800 | 147 | 653 | 0 | — |
| [`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md) | 58 | 24 | 34 | 0 | weighted results |
| [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md) | 235 | 53 | 182 | 0 | — |
| [`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md) | 133 | 51 | 82 | 0 | — |
| [`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md) | 3,369 | 1,419 | 1,950 | 0 | adjusted standardized residuals |
| [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md) | 288 | 18 | 270 | 0 | — |
| [`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md) | 6 | 0 | 6 | 0 | weighted results |
| [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md) | 578 | 21 | 473 | 84 | — |
| [`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md) | 609 | 200 | 409 | 0 | — |
| [`fisher_test()`](https://YannickDiehl.github.io/mariposa/reference/fisher_test.md) | 412 | 100 | 312 | 0 | — |
| [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md) | 150 | 24 | 126 | 0 | — |
| [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md) | 56 | 18 | 38 | 0 | weighted results |
| [`kendall_tau()`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md) | 202 | 100 | 102 | 0 | weighted results |
| [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md) | 97 | 52 | 45 | 0 | weighted results |
| [`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md) | 144 | 0 | 144 | 0 | — |
| [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md) | 602 | 35 | 567 | 0 | — |
| [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md) | 29 | 12 | 17 | 0 | all results (checked against the formulas of the SPSS algorithm) |
| [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md) | 93 | 19 | 74 | 0 | weighted results |
| [`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md) | 143 | 84 | 59 | 0 | — |
| [`multiple_response()`](https://YannickDiehl.github.io/mariposa/reference/multiple_response.md) | 52 | 19 | 33 | 0 | all results (checked by direct computation) |
| [`normality_test()`](https://YannickDiehl.github.io/mariposa/reference/normality_test.md) | 21 | 6 | 15 | 0 | all results (checked against independent implementations) |
| [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md) | 1,443 | 234 | 1,209 | 0 | — |
| [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md) | 242 | 106 | 136 | 0 | weighted results |
| [`partial_cor()`](https://YannickDiehl.github.io/mariposa/reference/partial_cor.md) | 20 | 4 | 16 | 0 | all results (checked against an equivalent regression computation) |
| [`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md) | 73 | 18 | 55 | 0 | — |
| [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md) | 458 | 45 | 413 | 0 | McDonald’s omega |
| [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md) | 1,422 | 0 | 1,422 | 0 | — |
| [`spearman_rho()`](https://YannickDiehl.github.io/mariposa/reference/spearman_rho.md) | 238 | 118 | 120 | 0 | — |
| [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md) | 673 | 9 | 664 | 0 | — |
| [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md) | 1,418 | 0 | 1,418 | 0 | — |
| [`w_iqr()`](https://YannickDiehl.github.io/mariposa/reference/w_iqr.md) | 444 | 132 | 312 | 0 | — |
| [`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/reference/w_kurtosis.md) | 444 | 132 | 312 | 0 | — |
| [`w_mean()`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md) | 444 | 132 | 312 | 0 | — |
| [`w_median()`](https://YannickDiehl.github.io/mariposa/reference/w_median.md) | 444 | 132 | 312 | 0 | — |
| [`w_modus()`](https://YannickDiehl.github.io/mariposa/reference/w_modus.md) | 444 | 132 | 312 | 0 | — |
| [`w_quantile()`](https://YannickDiehl.github.io/mariposa/reference/w_quantile.md) | 444 | 132 | 312 | 0 | — |
| [`w_range()`](https://YannickDiehl.github.io/mariposa/reference/w_range.md) | 444 | 132 | 312 | 0 | — |
| [`w_sd()`](https://YannickDiehl.github.io/mariposa/reference/w_sd.md) | 444 | 132 | 312 | 0 | — |
| [`w_se()`](https://YannickDiehl.github.io/mariposa/reference/w_se.md) | 444 | 132 | 312 | 0 | — |
| [`w_skew()`](https://YannickDiehl.github.io/mariposa/reference/w_skew.md) | 444 | 132 | 312 | 0 | — |
| [`w_var()`](https://YannickDiehl.github.io/mariposa/reference/w_var.md) | 444 | 132 | 312 | 0 | — |
| [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md) | 114 | 48 | 66 | 0 | weighted results |

## Documented differences

| Function | Results | Tolerance | Reason |
|----|----|---:|----|
| [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md) | ML loadings, sums of squares, % of variance | 0.002 | SPSS stops the ML iteration at its convergence criterion (.001); mariposa iterates to the optimum. |
| [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md) | ML communalities / SS where SPSS did not converge | 0.05 | The SPSS reference runs end without convergence (3 factors from 6 items, df = 0, Heywood cases). |

A documented difference describes a known difference between the
algorithms, not an error; its tolerance is the agreement the two
algorithms reach.

## Functions without their own SPSS comparison

- Computed by
  [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md),
  whose results are compared with SPSS:
  [`cramers_v()`](https://YannickDiehl.github.io/mariposa/reference/phi.md),
  [`goodman_gamma()`](https://YannickDiehl.github.io/mariposa/reference/phi.md),
  [`phi()`](https://YannickDiehl.github.io/mariposa/reference/phi.md)
- Data transformations, covered by unit tests:
  [`center()`](https://YannickDiehl.github.io/mariposa/reference/center.md),
  [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md),
  [`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md),
  [`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md),
  [`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md),
  [`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md),
  [`std()`](https://YannickDiehl.github.io/mariposa/reference/std.md)
- Data documentation without an SPSS counterpart:
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
