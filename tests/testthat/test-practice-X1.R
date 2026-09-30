# =============================================================================
# Practice-test regressions: API and input handling (batch X1)
# =============================================================================
# Entry handling shared by the exported functions: grouping variables inside
# the variable selection, misspelled arguments, the forms of `weights`,
# labelled data without a loaded haven namespace, missing required
# arguments. Each test names the finding ID from the 2026-09 practice test
# (field report) and what was wrong.
# =============================================================================

library(testthat)
library(dplyr)

data(survey_data)

# Grouped by a numeric copy of region: where(is.numeric) selects `g` too
.x1_grouped <- function() {
  survey_data |> mutate(g = as.integer(region)) |> group_by(g)
}


# --- EDGE-08: grouping variable inside the selection ---------------------------

test_that("EDGE-08: correlations drop the grouping variable from the selection", {
  # pearson_cor(grouped, where(is.numeric)) correlated the constant grouping
  # variable within each group: "No variance: g" warnings and NA rows.
  g <- .x1_grouped()
  for (fn in list(pearson_cor, spearman_rho, kendall_tau)) {
    expect_no_warning(expect_message(
      r <- fn(g, age, income, g), "Grouping variable"
    ))
    expect_false("g" %in% r$variables)
  }
})

test_that("EDGE-08: reliability() does not use the grouping variable as an item", {
  # group_by(trust_science) |> reliability(starts_with("trust")) used the
  # constant grouping variable as an item (zero-variance warnings per group).
  g <- .x1_grouped()
  warns <- character(0)
  expect_message(
    r <- withCallingHandlers(
      reliability(g, starts_with("trust"), g),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    "Grouping variable"
  )
  expect_setequal(r$variables, c("trust_government", "trust_media", "trust_science"))
  expect_false(any(grepl("zero variance", warns)))
})

test_that("EDGE-08: tests and transformations drop the grouping variable", {
  # t_test(grouped, where(is.numeric)) warned "no variance: constant value"
  # for the grouping variable in every group.
  g <- .x1_grouped()
  expect_no_warning(expect_message(
    r <- t_test(g, age, g, group = gender), "Grouping variable"
  ))
  expect_false("g" %in% r$results$Variable)
  expect_message(o <- oneway_anova(g, age, g, group = education), "Grouping variable")
  expect_false("g" %in% o$results$Variable)
  expect_message(f <- frequency(g, g, gender), "Grouping variable")
  expect_equal(f$variables, "gender")
  # std() within groups: the constant grouping variable gave NA z-scores
  expect_message(s <- std(g, age, g), "Grouping variable")
  expect_identical(s$g, g$g)
  expect_error(suppressMessages(pearson_cor(g, g)), "grouping")
  # rec() may recode a grouping variable (no per-group computation)
  expect_no_message(r2 <- rec(g, g, rules = "1=10; else=copy", suffix = "_r"))
  expect_true("g_r" %in% names(r2))
})


# --- EDGE-10: misspelled argument names -----------------------------------------

test_that("EDGE-10: a misspelled argument in `...` is an error naming the argument", {
  # `weight =` (for `weights`) landed in `...` and became a tidyselect
  # rename: w_mean() returned an unweighted result plus a bogus "weight"
  # row, binomial_test() a garbage row, describe() "Variable weight is not
  # numeric", frequency() a base-R "replacement has 1 row" error,
  # chi_square() "Exactly two variables must be specified".
  d <- survey_data
  expect_error(w_mean(d, age, weight = sampling_weight),
               "Unknown argument `weight` of `w_mean\\(\\)`")
  expect_error(w_mean(d, age, weight = sampling_weight),
               "Did you mean `weights`")
  expect_error(binomial_test(d, gender, weight = sampling_weight), "`weights`")
  expect_error(describe(d, age, weight = sampling_weight), "`weights`")
  expect_error(frequency(d, gender, weight = sampling_weight), "`weights`")
  expect_error(chi_square(d, gender, region, weight = sampling_weight), "`weights`")
  expect_error(kruskal_wallis(d, age, group = education, weight = sampling_weight),
               "`weights`")
  expect_error(mann_whitney(d, age, group = gender, weight = sampling_weight),
               "`weights`")
  expect_error(reliability(d, trust_government, trust_media, trust_science,
                           weight = sampling_weight), "`weights`")
  expect_error(efa(d, trust_government, trust_media, trust_science,
                   life_satisfaction, weight = sampling_weight), "`weights`")
  expect_error(pearson_cor(d, age, income, weigths = sampling_weight), "`weights`")
  expect_error(phi(d, gender, region, weight = sampling_weight), "`weights`")
  expect_error(levene_test(d, age, group = gender, weight = sampling_weight),
               "`weights`")
  expect_error(std(d, age, weight = sampling_weight), "`weights`")
  expect_error(codebook(d, age, weight = sampling_weight), "`weights`")
  # other arguments
  expect_error(describe(d, age, na_rm = TRUE), "Did you mean `na.rm`")
  expect_error(t_test(d, age, groups = gender), "Did you mean `group`")
  expect_error(oneway_anova(d, age, grp = education), "Did you mean `group`")
  expect_error(t_test(d, age, group = gender, conf = 0.9), "`conf.level`")
  expect_error(to_label(d, gender, ordred = TRUE), "`ordered`")
  expect_error(row_count(d, trust_media, trust_science, cout = 5), "`count`")
  # summarise()/vector mode ignored `...` entirely: silently unweighted
  expect_error(
    dplyr::summarise(d, m = w_mean(age, weight = sampling_weight)),
    "`weights`"
  )
  expect_error(std(d$age, weight = d$sampling_weight), "`weights`")
  # A tidyselect rename is refused with its own explanation
  expect_error(describe(d, Age = age), "selected without names")
  # The correct names still work
  expect_no_error(w_mean(d, age, weights = sampling_weight))
  expect_no_error(describe(d, age, na.rm = TRUE))
})

test_that("EDGE-10: abbreviated argument names are not partially matched", {
  # Functions without `...` before `weights` partially matched `weight =`
  # (crosstab, the regressions, ...) while the others errored: the same
  # typo worked in some functions only. Full names are now required
  # everywhere, with a pointer to the full name.
  d <- survey_data
  expect_error(crosstab(d, gender, region, weight = sampling_weight),
               "Did you mean `weights`")
  expect_error(crosstab(d, gender, region, percent = "col"), "`percentages`")
  expect_error(fisher_test(d, gender, region, weight = sampling_weight), "`weights`")
  expect_error(
    mcnemar_test(d |> mutate(a = trust_media > 3, b = trust_science > 3), a, b,
                 weight = sampling_weight),
    "`weights`"
  )
  expect_error(wilcoxon_test(d, trust_media, trust_science, weight = sampling_weight),
               "`weights`")
  expect_error(ancova(d, life_satisfaction, between = education, covariate = age,
                      weight = sampling_weight), "`weights`")
  expect_error(factorial_anova(d, life_satisfaction, between = c(gender, region),
                               weight = sampling_weight), "`weights`")
  expect_error(linear_regression(d, age ~ income, weight = sampling_weight),
               "`weights`")
  expect_error(logistic_regression(d, gender ~ age, weight = sampling_weight),
               "`weights`")
  expect_error(tukey_test(oneway_anova(d, age, group = education), conf = 0.9),
               "`conf.level`")
  # fisher_test()/mcnemar_test() silently ignored anything in `...`
  expect_error(fisher_test(d, gender, region, conf.level = 0.9), "Unknown argument")
  # full names still work
  expect_no_error(crosstab(d, gender, region, weights = sampling_weight,
                           percentages = "col"))
  expect_no_error(linear_regression(d, age ~ income, weights = sampling_weight))
})
