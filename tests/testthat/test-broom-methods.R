# =============================================================================
# broom tidiers for linear_regression / logistic_regression
# =============================================================================
# Regression test for the 0.6.4 fix: with class = c("linear_regression", "lm"),
# broom's lm tidiers used to call summary(x) which dispatched to our
# specialised summary.linear_regression and broke both tidy() (4 columns
# instead of 5) and glance() ("object 'r.squared' not found").
#
# We now register explicit tidy / glance / augment methods that strip our
# class so the inner summary() call dispatches to summary.lm / summary.glm.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)

data(survey_data, envir = environment())
survey_data$high_sat <- as.integer(survey_data$life_satisfaction >= 4)


# ---- linear_regression -------------------------------------------------------

test_that("broom::tidy() returns the full 5-column lm tidy tibble", {
  skip_if_not_installed("broom")
  r <- linear_regression(survey_data, life_satisfaction ~ age + income)
  td <- broom::tidy(r)
  expect_s3_class(td, "tbl_df")
  expect_setequal(names(td),
                  c("term", "estimate", "std.error", "statistic", "p.value"))
  expect_equal(nrow(td), 3L)
})

test_that("broom::tidy(r, conf.int = TRUE) adds conf.low / conf.high", {
  skip_if_not_installed("broom")
  r <- linear_regression(survey_data, life_satisfaction ~ age + income)
  td <- broom::tidy(r, conf.int = TRUE)
  expect_true(all(c("conf.low", "conf.high") %in% names(td)))
  expect_equal(nrow(td), 3L)
})

test_that("broom::glance() returns the full scalar model-summary tibble", {
  skip_if_not_installed("broom")
  r <- linear_regression(survey_data, life_satisfaction ~ age + income)
  gl <- broom::glance(r)
  expect_s3_class(gl, "tbl_df")
  expect_equal(nrow(gl), 1L)
  # Hits the exact slots broom::glance.lm builds
  for (col in c("r.squared", "adj.r.squared", "sigma", "statistic",
                "p.value", "df", "logLik", "AIC", "BIC", "deviance",
                "df.residual", "nobs")) {
    expect_true(col %in% names(gl),
                info = paste("missing glance column:", col))
  }
  # Values match the underlying lm computation
  inner <- r; class(inner) <- "lm"
  ref <- broom::glance(inner)
  expect_equal(gl$r.squared,     ref$r.squared)
  expect_equal(gl$adj.r.squared, ref$adj.r.squared)
})

test_that("broom::augment() returns the fitted-values augmented tibble", {
  skip_if_not_installed("broom")
  r <- linear_regression(survey_data, life_satisfaction ~ age + income)
  ag <- broom::augment(r)
  expect_s3_class(ag, "tbl_df")
  expect_true(".fitted" %in% names(ag))
  expect_true(".resid" %in% names(ag))
})


# ---- logistic_regression -----------------------------------------------------

test_that("broom::tidy() on logistic returns the full glm tidy tibble", {
  skip_if_not_installed("broom")
  g <- logistic_regression(survey_data, high_sat ~ age + income)
  td <- broom::tidy(g)
  expect_s3_class(td, "tbl_df")
  expect_setequal(names(td),
                  c("term", "estimate", "std.error", "statistic", "p.value"))
})

test_that("broom::tidy(g, exponentiate = TRUE) returns odds ratios", {
  skip_if_not_installed("broom")
  g <- logistic_regression(survey_data, high_sat ~ age + income)
  td_raw <- broom::tidy(g)
  td_exp <- broom::tidy(g, exponentiate = TRUE)
  # Exponentiated estimates equal exp(raw) for non-intercept terms
  expect_equal(td_exp$estimate, exp(td_raw$estimate))
})

test_that("broom::glance() on logistic returns glm glance scalars", {
  skip_if_not_installed("broom")
  g <- logistic_regression(survey_data, high_sat ~ age + income)
  gl <- broom::glance(g)
  expect_s3_class(gl, "tbl_df")
  for (col in c("null.deviance", "df.null", "logLik", "AIC", "BIC",
                "deviance", "df.residual", "nobs")) {
    expect_true(col %in% names(gl),
                info = paste("missing glance column:", col))
  }
})

test_that("broom::augment() on logistic returns .fitted / .resid", {
  skip_if_not_installed("broom")
  g <- logistic_regression(survey_data, high_sat ~ age + income)
  ag <- broom::augment(g)
  expect_true(".fitted" %in% names(ag))
})


# ---- error paths -------------------------------------------------------------

test_that("broom::tidy / glance / augment error helpfully on grouped lm", {
  skip_if_not_installed("broom")
  rg <- survey_data |>
    dplyr::group_by(region) |>
    linear_regression(life_satisfaction ~ age)
  expect_error(broom::tidy(rg),    regexp = "grouped")
  expect_error(broom::glance(rg),  regexp = "grouped")
  expect_error(broom::augment(rg), regexp = "grouped")
})

test_that("broom::tidy / glance / augment error helpfully on pairwise lm", {
  skip_if_not_installed("broom")
  rp <- linear_regression(survey_data, life_satisfaction ~ age + income,
                          use = "pairwise")
  expect_error(broom::tidy(rp),    regexp = "pairwise|listwise|lm")
  expect_error(broom::glance(rp),  regexp = "pairwise|listwise|lm")
  expect_error(broom::augment(rp), regexp = "pairwise|listwise|lm")
})

test_that("broom::tidy / glance error helpfully on grouped glm", {
  skip_if_not_installed("broom")
  gg <- survey_data |>
    dplyr::group_by(region) |>
    logistic_regression(high_sat ~ age)
  expect_error(broom::tidy(gg),   regexp = "grouped")
  expect_error(broom::glance(gg), regexp = "grouped")
})


# ---- per-group tidier dispatch still works -----------------------------------

test_that("each grouped element is independently tidy()-able", {
  skip_if_not_installed("broom")
  rg <- survey_data |>
    dplyr::group_by(region) |>
    linear_regression(life_satisfaction ~ age)
  for (grp in rg$groups) {
    td <- broom::tidy(grp)
    expect_setequal(names(td),
                    c("term", "estimate", "std.error", "statistic", "p.value"))
  }
})
