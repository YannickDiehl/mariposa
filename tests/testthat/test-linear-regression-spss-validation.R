# =============================================================================
# linear_regression — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::linear_regression() against SPSS v29 REGRESSION.
# Reference: tests/spss_reference/outputs/linear_regression_output.txt
#
# Coverage (Charter §8 four-scenario rule):
#   1a — unweighted / ungrouped / bivariate
#   1c — unweighted / ungrouped / multiple (factor predictor via factors="numeric")
#   2a — weighted / ungrouped / bivariate  (Charter §5.1 weighted-df fix)
#   2c — weighted / ungrouped / multiple
#   3a — unweighted / grouped (region) / bivariate
#   4a — weighted / grouped (region) / bivariate
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(

  # -------------------------------------------------------------------------
  # Test 1a — life_satisfaction ~ age (unweighted, ungrouped)
  # -------------------------------------------------------------------------
  test_1a_life_age = list(
    n = 2421L,
    R = 0.029, R2 = 0.001, adj_R2 = 0.000,        # linear_regression_output.txt:21
    se_estimate = 1.153,                          # linear_regression_output.txt:21
    anova = list(ss_reg = 2.653, df_reg = 1L, ms_reg = 2.653,
                 ss_res = 3214.775, df_res = 2419L, ms_res = 1.329,
                 ss_tot = 3217.428, df_tot = 2420L,
                 F = 1.996, p = 0.158),           # linear_regression_output.txt:27-29
    coefs = list(
      intercept = list(B = 3.727, SE = 0.074, t = 50.663, p = "<.001"),  # :37
      age       = list(B = -0.002, SE = 0.001, Beta = -0.029,
                       t = -1.413, p = 0.158)                            # :38
    )
  ),

  # -------------------------------------------------------------------------
  # Test 1c — income ~ age + education + life_satisfaction
  # education is an ordered factor; SPSS treats it ordinal-as-scale.
  # mariposa default is factors="dummy" (polynomial contrasts for ordered),
  # so this test must call with factors="numeric" to match SPSS.
  # -------------------------------------------------------------------------
  test_1c_income_multi = list(
    n = 2115L,
    R = 0.686, R2 = 0.471, adj_R2 = 0.470,         # linear_regression_output.txt:120
    se_estimate = 1041.88947,                      # linear_regression_output.txt:120
    anova = list(ss_reg = 2036941094.578, df_reg = 3L, ms_reg = 678980364.860,
                 ss_res = 2291561553.177, df_res = 2111L, ms_res = 1085533.659,
                 ss_tot = 4328502647.755, df_tot = 2114L,
                 F = 625.481, p = "<.001"),        # linear_regression_output.txt:126-128
    coefs = list(
      intercept    = list(B = 800.493, SE = 105.893, t = 7.559,  p = "<.001"),  # :136
      age          = list(B = -0.194,  SE = 1.333,   Beta = -0.002,
                          t = -0.145, p = 0.885),                               # :137
      education    = list(B = 711.841, SE = 21.706,  Beta = 0.539,
                          t = 32.794, p = "<.001"),                             # :138
      life_satisfaction = list(B = 377.527, SE = 20.505, Beta = 0.303,
                               t = 18.412, p = "<.001")                         # :139
    )
  ),

  # -------------------------------------------------------------------------
  # Test 2a — life_satisfaction ~ age (weighted, ungrouped)
  # KEY TEST for Charter §5.1 weighted-df fix.
  # -------------------------------------------------------------------------
  test_2a_life_age_weighted = list(
    n = 2437L,
    R = 0.029, R2 = 0.001, adj_R2 = 0.000,         # linear_regression_output.txt:220
    se_estimate = 1.152,                           # linear_regression_output.txt:220
    anova = list(ss_reg = 2.757, df_reg = 1L, ms_reg = 2.757,
                 ss_res = 3230.392, df_res = 2435L, ms_res = 1.327,
                 ss_tot = 3233.149, df_tot = 2436L,
                 F = 2.078, p = 0.150),            # linear_regression_output.txt:226-228
    coefs = list(
      intercept = list(B = 3.724, SE = 0.073, t = 51.152, p = "<.001"),         # :236
      age       = list(B = -0.002, SE = 0.001, Beta = -0.029,
                       t = -1.441, p = 0.150)                                   # :237
    )
  ),

  # -------------------------------------------------------------------------
  # Test 2c — weighted multiple: income ~ age + education + life_satisfaction
  # -------------------------------------------------------------------------
  test_2c_income_multi_weighted = list(
    n = 2130L,
    R = 0.686, R2 = 0.470, adj_R2 = 0.469,         # linear_regression_output.txt:319
    se_estimate = 1036.28697,                      # linear_regression_output.txt:319
    anova = list(ss_reg = 2026419335.048, df_reg = 3L, ms_reg = 675473111.683,
                 ss_res = 2282895332.715, df_res = 2126L, ms_res = 1073890.693,
                 ss_tot = 4309314667.763, df_tot = 2129L,
                 F = 628.996, p = "<.001"),        # linear_regression_output.txt:325-327
    coefs = list(
      intercept    = list(B = 784.981, SE = 104.720, t = 7.496,  p = "<.001"),  # :335
      age          = list(B = -0.110,  SE = 1.315,   Beta = -0.001,
                          t = -0.083, p = 0.934),                               # :336
      education    = list(B = 709.765, SE = 21.659,  Beta = 0.537,
                          t = 32.770, p = "<.001"),                             # :337
      life_satisfaction = list(B = 381.257, SE = 20.309, Beta = 0.307,
                               t = 18.773, p = "<.001")                         # :338
    )
  ),

  # -------------------------------------------------------------------------
  # Test 3a — life_satisfaction ~ age grouped by region (unweighted)
  # -------------------------------------------------------------------------
  test_3a_grouped_east = list(
    n = 465L,
    R = 0.043, R2 = 0.002, adj_R2 = 0.000, se_estimate = 1.207,   # :367
    anova = list(ss_reg = 1.276, df_reg = 1L, ms_reg = 1.276,
                 ss_res = 674.350, df_res = 463L, ms_res = 1.456,
                 ss_tot = 675.626, df_tot = 464L,
                 F = 0.876, p = 0.350),                            # :374
    coefs = list(
      intercept = list(B = 3.775, SE = 0.176, t = 21.467, p = "<.001"),  # :387
      age       = list(B = -0.003, SE = 0.003, Beta = -0.043,
                       t = -0.936, p = 0.350)                            # :388
    )
  ),

  test_3a_grouped_west = list(
    n = 1956L,
    R = 0.025, R2 = 0.001, adj_R2 = 0.000, se_estimate = 1.140,   # :368
    anova = list(ss_reg = 1.556, df_reg = 1L, ms_reg = 1.556,
                 ss_res = 2540.200, df_res = 1954L, ms_res = 1.300,
                 ss_tot = 2541.756, df_tot = 1955L,
                 F = 1.197, p = 0.274),                            # :377
    coefs = list(
      intercept = list(B = 3.714, SE = 0.081, t = 45.860, p = "<.001"),  # :389
      age       = list(B = -0.002, SE = 0.002, Beta = -0.025,
                       t = -1.094, p = 0.274)                            # :390
    )
  ),

  # -------------------------------------------------------------------------
  # Test 4a — life_satisfaction ~ age weighted + grouped by region
  # KEY TEST for §5.1 fix interaction with grouping.
  # -------------------------------------------------------------------------
  test_4a_weighted_grouped_east = list(
    n = 488L,
    R = 0.046, R2 = 0.002, adj_R2 = 0.000, se_estimate = 1.203,   # :501
    anova = list(ss_reg = 1.514, df_reg = 1L, ms_reg = 1.514,
                 ss_res = 703.788, df_res = 486L, ms_res = 1.448,
                 ss_tot = 705.302, df_tot = 487L,
                 F = 1.045, p = 0.307),                            # :508
    coefs = list(
      intercept = list(B = 3.789, SE = 0.171, t = 22.179, p = "<.001"),  # :521
      age       = list(B = -0.003, SE = 0.003, Beta = -0.046,
                       t = -1.022, p = 0.307)                            # :522
    )
  ),

  test_4a_weighted_grouped_west = list(
    n = 1949L,
    R = 0.025, R2 = 0.001, adj_R2 = 0.000, se_estimate = 1.139,   # :502
    anova = list(ss_reg = 1.518, df_reg = 1L, ms_reg = 1.518,
                 ss_res = 2526.327, df_res = 1947L, ms_res = 1.298,
                 ss_tot = 2527.845, df_tot = 1948L,
                 F = 1.170, p = 0.280),                            # :511
    coefs = list(
      intercept = list(B = 3.708, SE = 0.081, t = 46.034, p = "<.001"),  # :523
      age       = list(B = -0.002, SE = 0.002, Beta = -0.025,
                       t = -1.082, p = 0.280)                            # :524
    )
  )
)


data(survey_data, envir = environment())


# =============================================================================
# Reusable per-test assertion helpers
# =============================================================================

assert_lm_model <- function(result, spss, scenario) {
  assert_spss(result$model_summary$R,             spss$R,
              tier = "display", precision = 3, label = sprintf("[%s] R", scenario))
  assert_spss(result$model_summary$R_squared,     spss$R2,
              tier = "display", precision = 3, label = sprintf("[%s] R^2", scenario))
  assert_spss(result$model_summary$adj_R_squared, spss$adj_R2,
              tier = "display", precision = 3, label = sprintf("[%s] adj-R^2", scenario))
  assert_spss(result$model_summary$std_error,     spss$se_estimate,
              tier = "display", precision = 3, label = sprintf("[%s] SE estimate", scenario))
}

assert_lm_anova <- function(result, spss, scenario) {
  reg <- result$anova_table[result$anova_table$Source == "Regression", ]
  res <- result$anova_table[result$anova_table$Source == "Residual", ]
  tot <- result$anova_table[result$anova_table$Source == "Total", ]

  assert_spss(reg$Sum_of_Squares, spss$anova$ss_reg,
              tier = "display", precision = 3, label = sprintf("[%s] SS Reg", scenario))
  assert_spss(res$Sum_of_Squares, spss$anova$ss_res,
              tier = "display", precision = 3, label = sprintf("[%s] SS Res", scenario))
  assert_spss(tot$Sum_of_Squares, spss$anova$ss_tot,
              tier = "display", precision = 3, label = sprintf("[%s] SS Tot", scenario))

  # df: SPSS prints integer; mariposa stores non-integer for weighted; precision=0
  assert_spss(reg$df, spss$anova$df_reg,
              tier = "display", precision = 0, label = sprintf("[%s] df Reg", scenario))
  assert_spss(res$df, spss$anova$df_res,
              tier = "display", precision = 0, label = sprintf("[%s] df Res", scenario))
  assert_spss(tot$df, spss$anova$df_tot,
              tier = "display", precision = 0, label = sprintf("[%s] df Tot", scenario))

  assert_spss(reg$Mean_Square, spss$anova$ms_reg,
              tier = "display", precision = 3, label = sprintf("[%s] MS Reg", scenario))
  assert_spss(res$Mean_Square, spss$anova$ms_res,
              tier = "display", precision = 3, label = sprintf("[%s] MS Res", scenario))

  assert_spss(reg$F_statistic, spss$anova$F,
              tier = "display", precision = 3, label = sprintf("[%s] F", scenario))
  assert_spss(reg$Sig, spss$anova$p,
              tier = "display", precision = 3, what = "p_value",
              label = sprintf("[%s] ANOVA Sig", scenario))
}

assert_lm_coef <- function(result, term_label, spss_coef, scenario) {
  row <- result$coef_table[result$coef_table$Term == term_label, ]
  if (nrow(row) == 0) {
    fail(sprintf("[%s] term %s not found in coefficients", scenario, term_label))
    return(invisible(FALSE))
  }
  assert_spss(row$B,         spss_coef$B,
              tier = "display", precision = 3,
              label = sprintf("[%s] %s B", scenario, term_label))
  assert_spss(row$Std.Error, spss_coef$SE,
              tier = "display", precision = 3,
              label = sprintf("[%s] %s SE", scenario, term_label))
  assert_spss(row$t,         spss_coef$t,
              tier = "display", precision = 3,
              label = sprintf("[%s] %s t", scenario, term_label))
  assert_spss(row$p,         spss_coef$p,
              tier = "display", precision = 3, what = "p_value",
              label = sprintf("[%s] %s p", scenario, term_label))
  if (!is.null(spss_coef$Beta)) {
    assert_spss(row$Beta, spss_coef$Beta,
                tier = "display", precision = 3,
                label = sprintf("[%s] %s Beta", scenario, term_label))
  }
}


# =============================================================================
# TESTS
# =============================================================================

# ---- Test 1a: unweighted / ungrouped / bivariate ----------------------------
test_that("[1a] linear_regression life_sat ~ age — matches SPSS", {
  r <- linear_regression(survey_data, life_satisfaction ~ age)
  spss <- spss_values$test_1a_life_age

  assert_spss_count(r$n, spss$n, label = "[1a] N")
  assert_lm_model(r, spss, "1a")
  assert_lm_anova(r, spss, "1a")
  assert_lm_coef(r, "(Intercept)", spss$coefs$intercept, "1a")
  assert_lm_coef(r, "age",          spss$coefs$age,       "1a")
})


# ---- Test 1c: unweighted / ungrouped / multiple with factor ----------------
test_that("[1c] linear_regression income ~ age + education + life_sat — matches SPSS", {
  # SPSS treats education ordinally; pass factors="numeric" for parity.
  r <- suppressMessages(
    linear_regression(survey_data,
                      income ~ age + education + life_satisfaction,
                      factors = "numeric")
  )
  spss <- spss_values$test_1c_income_multi

  assert_spss_count(r$n, spss$n, label = "[1c] N")
  assert_lm_model(r, spss, "1c")
  assert_lm_anova(r, spss, "1c")
  assert_lm_coef(r, "(Intercept)",       spss$coefs$intercept,         "1c")
  assert_lm_coef(r, "age",                spss$coefs$age,                "1c")
  assert_lm_coef(r, "education",          spss$coefs$education,          "1c")
  assert_lm_coef(r, "life_satisfaction",  spss$coefs$life_satisfaction,  "1c")
})


# ---- Test 2a: weighted / ungrouped / bivariate -----------------------------
# Charter §5.1 — uses unrounded sum(w) in all internal calculations.
test_that("[2a] weighted linear_regression life_sat ~ age — matches SPSS (Charter §5.1)", {
  r <- linear_regression(survey_data, life_satisfaction ~ age,
                         weights = sampling_weight)
  spss <- spss_values$test_2a_life_age_weighted

  assert_spss(r$n, spss$n, tier = "display", precision = 0, label = "[2a] N")
  assert_lm_model(r, spss, "2a")
  assert_lm_anova(r, spss, "2a")
  assert_lm_coef(r, "(Intercept)", spss$coefs$intercept, "2a")
  assert_lm_coef(r, "age",          spss$coefs$age,       "2a")
})


# ---- Test 2c: weighted / ungrouped / multiple ------------------------------
test_that("[2c] weighted linear_regression income ~ multi — matches SPSS", {
  r <- suppressMessages(
    linear_regression(survey_data,
                      income ~ age + education + life_satisfaction,
                      weights = sampling_weight,
                      factors = "numeric")
  )
  spss <- spss_values$test_2c_income_multi_weighted

  assert_spss(r$n, spss$n, tier = "display", precision = 0, label = "[2c] N")
  assert_lm_model(r, spss, "2c")
  assert_lm_anova(r, spss, "2c")
  assert_lm_coef(r, "(Intercept)",       spss$coefs$intercept,         "2c")
  assert_lm_coef(r, "age",                spss$coefs$age,                "2c")
  assert_lm_coef(r, "education",          spss$coefs$education,          "2c")
  assert_lm_coef(r, "life_satisfaction",  spss$coefs$life_satisfaction,  "2c")
})


# ---- Test 3a: unweighted / grouped / bivariate -----------------------------
test_that("[3a] grouped linear_regression life_sat ~ age (by region) — matches SPSS", {
  r <- survey_data |>
    dplyr::group_by(region) |>
    linear_regression(life_satisfaction ~ age)

  expect_true(isTRUE(r$is_grouped))
  expect_equal(length(r$groups), 2L)

  # Match groups by region value (factor levels: East = level 1, West = level 2)
  east <- Filter(function(g) identical(g$group_values$region, "East"), r$groups)[[1]]
  west <- Filter(function(g) identical(g$group_values$region, "West"), r$groups)[[1]]

  spss_e <- spss_values$test_3a_grouped_east
  spss_w <- spss_values$test_3a_grouped_west

  assert_spss_count(east$n, spss_e$n, label = "[3a-East] N")
  assert_lm_model(east, spss_e, "3a-East")
  assert_lm_anova(east, spss_e, "3a-East")
  assert_lm_coef(east, "(Intercept)", spss_e$coefs$intercept, "3a-East")
  assert_lm_coef(east, "age",          spss_e$coefs$age,       "3a-East")

  assert_spss_count(west$n, spss_w$n, label = "[3a-West] N")
  assert_lm_model(west, spss_w, "3a-West")
  assert_lm_anova(west, spss_w, "3a-West")
  assert_lm_coef(west, "(Intercept)", spss_w$coefs$intercept, "3a-West")
  assert_lm_coef(west, "age",          spss_w$coefs$age,       "3a-West")
})


# ---- Test 4a: weighted / grouped / bivariate -------------------------------
test_that("[4a] weighted+grouped linear_regression — matches SPSS (Charter §5.1)", {
  r <- survey_data |>
    dplyr::group_by(region) |>
    linear_regression(life_satisfaction ~ age, weights = sampling_weight)

  expect_true(isTRUE(r$is_grouped))
  expect_equal(length(r$groups), 2L)

  east <- Filter(function(g) identical(g$group_values$region, "East"), r$groups)[[1]]
  west <- Filter(function(g) identical(g$group_values$region, "West"), r$groups)[[1]]

  spss_e <- spss_values$test_4a_weighted_grouped_east
  spss_w <- spss_values$test_4a_weighted_grouped_west

  assert_spss(east$n, spss_e$n, tier = "display", precision = 0, label = "[4a-East] N")
  assert_lm_model(east, spss_e, "4a-East")
  assert_lm_anova(east, spss_e, "4a-East")
  assert_lm_coef(east, "(Intercept)", spss_e$coefs$intercept, "4a-East")
  assert_lm_coef(east, "age",          spss_e$coefs$age,       "4a-East")

  assert_spss(west$n, spss_w$n, tier = "display", precision = 0, label = "[4a-West] N")
  assert_lm_model(west, spss_w, "4a-West")
  assert_lm_anova(west, spss_w, "4a-West")
  assert_lm_coef(west, "(Intercept)", spss_w$coefs$intercept, "4a-West")
  assert_lm_coef(west, "age",          spss_w$coefs$age,       "4a-West")
})


# =============================================================================
# Behavior tests for the new `factors` argument
# =============================================================================

test_that("factors='dummy' (default) produces L-1 contrasts for unordered factor", {
  set.seed(1)
  df <- tibble::tibble(
    y = rnorm(60),
    x = factor(rep(c("a", "b", "c"), each = 20))
  )
  r <- linear_regression(df, y ~ x)
  # Two dummy contrasts + intercept = 3 rows
  expect_equal(nrow(r$coef_table), 3L)
  expect_setequal(r$coef_table$Term, c("(Intercept)", "xb", "xc"))
})

test_that("factors='numeric' coerces factor levels to integer codes (single B)", {
  set.seed(1)
  df <- tibble::tibble(
    y = rnorm(60),
    x = factor(rep(c("a", "b", "c"), each = 20))
  )
  r <- suppressMessages(linear_regression(df, y ~ x, factors = "numeric"))
  # Single ordinal-as-scale slope + intercept = 2 rows
  expect_equal(nrow(r$coef_table), 2L)
  expect_setequal(r$coef_table$Term, c("(Intercept)", "x"))
})

test_that("factors='numeric' emits one-line cli_inform listing coerced variables", {
  set.seed(1)
  df <- tibble::tibble(y = rnorm(30), x = factor(rep(c("a","b","c"), each = 10)))
  expect_message(
    linear_regression(df, y ~ x, factors = "numeric"),
    regexp = "coerced to numeric"
  )
})

test_that("pairwise + dummy + factor predictor errors with actionable message", {
  set.seed(1)
  df <- tibble::tibble(y = rnorm(30), x = factor(rep(c("a","b","c"), each = 10)))
  expect_error(
    linear_regression(df, y ~ x, use = "pairwise"),
    regexp = "Pairwise deletion"
  )
})


# =============================================================================
# Native generic dispatch (the object IS an lm) — added 0.6.3
# =============================================================================

test_that("listwise+ungrouped result inherits from lm", {
  r <- linear_regression(survey_data, life_satisfaction ~ age + income)
  expect_true(inherits(r, "lm"))
  expect_true(inherits(r, "linear_regression"))
  expect_identical(class(r)[1], "linear_regression")
})

test_that("coef(r) returns lm-style named numeric vector matching coef_table$B", {
  r <- linear_regression(survey_data, life_satisfaction ~ age + income)
  cv <- coef(r)
  expect_type(cv, "double")
  expect_named(cv)
  expect_equal(unname(cv), unname(r$coef_table$B))
})

test_that("predict(r, newdata) dispatches to predict.lm", {
  r <- linear_regression(survey_data, life_satisfaction ~ age + income)
  nd <- head(survey_data, 5)
  p <- predict(r, newdata = nd)
  # Independent recompute via stats::lm on the same complete cases
  d <- survey_data[stats::complete.cases(
    survey_data[, c("life_satisfaction","age","income")]), ]
  expect_equal(unname(p),
               unname(predict(stats::lm(life_satisfaction ~ age + income, d),
                              newdata = nd)))
})

test_that("anova(r) dispatches to anova.lm (sequential SS table)", {
  r <- linear_regression(survey_data, life_satisfaction ~ age + income)
  a <- anova(r)
  expect_s3_class(a, "anova")
  # Three rows: age, income, Residuals
  expect_equal(nrow(a), 3L)
})

test_that("vcov / confint / residuals / fitted / formula dispatch natively", {
  r <- linear_regression(survey_data, life_satisfaction ~ age + income)
  expect_true(is.matrix(vcov(r)))
  expect_true(is.matrix(confint(r)))
  expect_type(residuals(r), "double")
  expect_type(fitted(r), "double")
  expect_s3_class(formula(r), "formula")
  expect_equal(nobs(r), r$n)
})

test_that("model.matrix(r) dispatches natively", {
  r <- linear_regression(survey_data, life_satisfaction ~ age + income)
  X <- stats::model.matrix(r)
  expect_true(is.matrix(X))
  expect_equal(ncol(X), 3L)  # intercept + 2 predictors
})

test_that("predict() on grouped result errors with actionable message", {
  rg <- survey_data |>
    dplyr::group_by(region) |>
    linear_regression(life_satisfaction ~ age)
  expect_error(predict(rg), regexp = "grouped")
})

test_that("predict() on pairwise result errors with actionable message", {
  rp <- linear_regression(survey_data, life_satisfaction ~ age + income,
                          use = "pairwise")
  expect_error(predict(rp), regexp = "pairwise")
})

test_that("each group element of a grouped result inherits from lm", {
  rg <- survey_data |>
    dplyr::group_by(region) |>
    linear_regression(life_satisfaction ~ age)
  for (grp in rg$groups) {
    expect_true(inherits(grp, "lm"))
    # Per-group predict should work directly and return finite predictions
    nd <- head(survey_data, 3)
    pred <- predict(grp, newdata = nd)
    expect_type(pred, "double")
    expect_length(pred, 3L)
    expect_true(all(is.finite(pred)))
  }
})
