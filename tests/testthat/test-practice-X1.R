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


# --- UX-WEIGHTS / EDGE-16: the forms of `weights` -------------------------------

test_that("UX-WEIGHTS: weights as string, all_of(), !!, data$col or expression", {
  # Only a bare column name worked outside the regressions:
  # `weights = survey_data$sampling_weight` and `weights = all_of(w)` gave
  # "Can't convert a call to a string.", `weights = 1` "Can't convert a
  # double vector to a string", `weights = sampling_weight * 2` failed.
  d <- survey_data
  wname <- "sampling_weight"
  ref <- describe(d, age, weights = sampling_weight)$results
  forms <- list(
    string = describe(d, age, weights = "sampling_weight"),
    all_of = describe(d, age, weights = all_of(wname)),
    bang = describe(d, age, weights = !!wname),
    dollar = describe(d, age, weights = d$sampling_weight),
    expr = describe(d, age, weights = sampling_weight * 1)
  )
  for (r in forms) {
    expect_equal(r$results$age_Mean, ref$age_Mean)
    expect_equal(r$results$age_N, ref$age_N)
  }
  expect_identical(forms$string$weights, "sampling_weight")
  expect_identical(forms$all_of$weights, "sampling_weight")
  # An expression is shown by its text
  expect_identical(forms$expr$weights, "sampling_weight * 1")
  expect_identical(forms$dollar$weights, "d$sampling_weight")
  # A numeric vector from the environment, and a constant
  wts <- d$sampling_weight
  expect_equal(describe(d, age, weights = wts)$results$age_Mean, ref$age_Mean)
  expect_equal(describe(d, age, weights = 1)$results$age_Mean,
               describe(d, age)$results$age_Mean)
})

test_that("UX-WEIGHTS: every weighted entry point accepts the weight forms", {
  d <- survey_data
  same <- function(a, b) expect_equal(a, b, ignore_attr = TRUE)
  same(w_mean(d, age, weights = d$sampling_weight)$results$weighted_mean,
       w_mean(d, age, weights = sampling_weight)$results$weighted_mean)
  same(t_test(d, age, group = gender, weights = sampling_weight * 1)$results$t_stat,
       t_test(d, age, group = gender, weights = sampling_weight)$results$t_stat)
  same(crosstab(d, gender, region, weights = d$sampling_weight)$table,
       crosstab(d, gender, region, weights = sampling_weight)$table)
  same(chi_square(d, gender, region, weights = all_of("sampling_weight"))$results$chi_squared,
       chi_square(d, gender, region, weights = sampling_weight)$results$chi_squared)
  same(oneway_anova(d, age, group = education, weights = d$sampling_weight)$results$F_statistic,
       oneway_anova(d, age, group = education, weights = sampling_weight)$results$F_statistic)
  same(levene_test(d, age, group = gender, weights = d$sampling_weight)$results$F_statistic,
       levene_test(d, age, group = gender, weights = sampling_weight)$results$F_statistic)
  same(frequency(d, gender, weights = d$sampling_weight)$results$freq,
       frequency(d, gender, weights = sampling_weight)$results$freq)
  same(coef(linear_regression(d, age ~ income, weights = "sampling_weight")),
       coef(linear_regression(d, age ~ income, weights = sampling_weight)))
  # Grouped paths re-read the weights per group
  g <- group_by(d, region)
  same(w_mean(g, age, weights = sampling_weight * 1)$results$weighted_mean,
       w_mean(g, age, weights = sampling_weight)$results$weighted_mean)
  same(crosstab(g, gender, education, weights = d$sampling_weight)$results[[1]]$table,
       crosstab(g, gender, education, weights = sampling_weight)$results[[1]]$table)
  same(describe(g, age, weights = d$sampling_weight)$results$age_Mean,
       describe(g, age, weights = sampling_weight)$results$age_Mean)
})

test_that("UX-WEIGHTS: clear errors for unusable weights", {
  d <- survey_data
  wname <- "sampling_weight"
  # A bare name holding a column name: pointer to all_of()
  expect_error(describe(d, age, weights = wname), "all_of")
  expect_error(w_mean(d, age, weights = wname), "all_of")
  # Wrong length
  expect_error(describe(d, age, weights = c(1, 2, 3)), "one value per row")
  # Unknown variable inside an expression
  expect_error(describe(d, age, weights = sampling_wt * 2), "sampling_wt")
  # A factor used as weights: the w_* functions took its integer codes
  expect_error(w_mean(d, age, weights = gender), "must be numeric")
  expect_error(describe(d, age, weights = gender), "must be numeric")
  expect_error(describe(d, age, weights = nonexistent), "not found in data")
})

test_that("UX-WEIGHTS: std()/center() return the data without a weights column", {
  # The expression weights must not leak into the returned data, and the
  # weights column keeps its attributes
  d <- survey_data
  s <- std(d, age, weights = sampling_weight * 1)
  expect_identical(names(s), names(d))
  expect_equal(s$age, std(d, age, weights = sampling_weight)$age)
  c1 <- center(d, age, weights = d$sampling_weight)
  expect_identical(names(c1), names(d))
  expect_identical(attributes(std(d, age, weights = sampling_weight)$sampling_weight),
                   attributes(d$sampling_weight))
})


# --- haven not loaded ---------------------------------------------------------

test_that("haven-not-loaded: labelled data from readRDS() work without haven loaded", {
  # haven is only suggested. Labelled data restored with readRDS() in a
  # session where haven was never loaded have no registered vctrs methods:
  # frequency() failed with "Can't convert `x` <haven_labelled> to
  # <character>", describe()/w_mean() with "<haven_labelled_spss> *
  # <double> is not permitted", codebook() and crosstab() likewise. Each
  # call runs in a fresh R process (the methods cannot be unregistered).
  skip_on_cran()
  skip_if_not_installed("haven")
  pkg_root <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  dev <- file.exists(file.path(pkg_root, "DESCRIPTION"))
  if (dev) skip_if_not_installed("pkgload")
  load_line <- if (dev) {
    sprintf("suppressMessages(pkgload::load_all(%s, quiet = TRUE, helpers = FALSE))",
            deparse(pkg_root))
  } else {
    "suppressMessages(library(mariposa))"
  }

  rds <- tempfile(fileext = ".rds")
  set.seed(7)
  n <- 40
  saveRDS(data.frame(
    x = haven::labelled(rep(1:2, n / 2), c(low = 1, high = 2), label = "X"),
    y = haven::labelled_spss(rep(1:4, n / 4), c(a = 1, d = 4), na_values = 9),
    g = haven::labelled(rep(1:2, each = n / 2), c(East = 1, West = 2)),
    w = haven::labelled(runif(n, 0.5, 1.5), c(none = 0))
  ), rds)
  on.exit(unlink(rds), add = TRUE)

  run_fresh <- function(call_text) {
    script <- tempfile(fileext = ".R")
    on.exit(unlink(script))
    writeLines(c(
      sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")),
      load_line,
      sprintf("d <- readRDS(%s)", deparse(rds)),
      "if (isNamespaceLoaded('haven')) stop('precondition: haven is loaded')",
      sprintf(
        "r <- tryCatch({invisible(capture.output(suppressMessages(%s))); 'OK'}, error = function(e) conditionMessage(e))",
        call_text
      ),
      "cat('RESULT:', r, '\\n')"
    ), script)
    out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                    c("--vanilla", shQuote(script)),
                                    stdout = TRUE, stderr = TRUE))
    paste(out, collapse = "\n")
  }

  for (cl in c("frequency(d, x)",
               "describe(d, y, weights = w)",
               "crosstab(d, x, g)",
               "dplyr::summarise(d, m = w_mean(y, weights = w))",
               "codebook(d, view = FALSE)",
               "unlabel(d)")) {
    expect_match(run_fresh(cl), "RESULT: OK", fixed = TRUE, info = cl)
  }
})
