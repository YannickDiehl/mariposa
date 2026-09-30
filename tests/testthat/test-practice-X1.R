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
