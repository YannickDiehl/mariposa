# =============================================================================
# partial_cor — PROPERTY-BASED VALIDATION (Charter Tier-4)
# =============================================================================
# Purpose: Validate that partial_cor() correctly implements the SPSS
# PARTIAL CORR statistics, without an SPSS reference run (pending, see
# .claude/BACKLOG.md).
#
# Oracle: the residual-of-regressions characterization — the partial
# correlation of x and y given Z equals the (weighted) Pearson
# correlation of the residuals of x ~ Z and y ~ Z. This recomputes the
# statistic through lm()/weighted least squares, fully independent of
# the correlation-matrix inversion the implementation uses. df, t, and p
# are pinned against their textbook formulas (IBM SPSS Statistics
# Algorithms, "PARTIAL CORR": df = n - 2 - k, two-tailed t-test).
#
# Scenario coverage (Charter §8): all four scenarios (unweighted/weighted
# x ungrouped/grouped).
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


data(survey_data, envir = environment())

# Residual-based reference: partial r of x, y given Z (optionally weighted)
.ref_partial_r <- function(data, x, y, z_vars, w = NULL) {
  vars <- c(x, y, z_vars, if (!is.null(w)) w)
  d <- data[stats::complete.cases(data[, vars, drop = FALSE]), vars, drop = FALSE]
  if (!is.null(w)) d <- d[d[[w]] > 0, , drop = FALSE]
  fml_x <- stats::as.formula(paste(x, "~", paste(z_vars, collapse = "+")))
  fml_y <- stats::as.formula(paste(y, "~", paste(z_vars, collapse = "+")))
  if (is.null(w)) {
    rx <- stats::resid(stats::lm(fml_x, data = d))
    ry <- stats::resid(stats::lm(fml_y, data = d))
    r <- stats::cor(rx, ry)
    n_eff <- nrow(d)
  } else {
    wv <- d[[w]]
    rx <- stats::resid(stats::lm(fml_x, data = d, weights = wv))
    ry <- stats::resid(stats::lm(fml_y, data = d, weights = wv))
    mx <- sum(wv * rx) / sum(wv)
    my <- sum(wv * ry) / sum(wv)
    r <- sum(wv * (rx - mx) * (ry - my)) /
      sqrt(sum(wv * (rx - mx)^2) * sum(wv * (ry - my)^2))
    n_eff <- sum(wv)
  }
  list(r = r, n_eff = n_eff)
}


test_that("Unweighted partial r matches the residual-regression oracle", {
  r <- partial_cor(survey_data, life_satisfaction, income, controls = age)
  ref <- .ref_partial_r(survey_data, "life_satisfaction", "income", "age")

  assert_spss(r$correlations$partial_r[1], ref$r,
              tier = "display", precision = 5,
              label = "partial r (satisfaction x income | age)")
  assert_spss_count(r$correlations$df[1], as.integer(ref$n_eff - 3),
                    label = "df = n - 2 - k, one control")

  # t and p from the textbook formulas
  rp <- r$correlations$partial_r[1]
  df <- r$correlations$df[1]
  assert_spss(r$correlations$t_stat[1], rp * sqrt(df / (1 - rp^2)),
              tier = "display", precision = 5, label = "t from partial r")
  assert_spss(r$correlations$p_value[1],
              2 * pt(abs(r$correlations$t_stat[1]), df, lower.tail = FALSE),
              tier = "spec", what = "p_value", label = "two-tailed p from t")
})


test_that("Multiple controls: partial r matches the oracle, zero-order matches cor()", {
  r <- partial_cor(survey_data, trust_government, trust_media, trust_science,
                   controls = c(age, political_orientation))
  vars <- c("trust_government", "trust_media", "trust_science")
  z <- c("age", "political_orientation")
  d <- survey_data[stats::complete.cases(survey_data[, c(vars, z)]), ]

  for (i in 1:2) {
    for (j in (i + 1):3) {
      ref <- .ref_partial_r(d, vars[i], vars[j], z)
      row <- r$correlations[r$correlations$var1 == vars[i] &
                              r$correlations$var2 == vars[j], ]
      assert_spss(row$partial_r, ref$r,
                  tier = "display", precision = 5,
                  label = sprintf("partial r %s x %s | 2 controls", vars[i], vars[j]))
      assert_spss(row$zero_order_r, stats::cor(d[[vars[i]]], d[[vars[j]]]),
                  tier = "display", precision = 5,
                  label = sprintf("zero-order r %s x %s (listwise)", vars[i], vars[j]))
      # matrix slot mirrors the long table
      assert_spss(r$matrices[[1]][vars[i], vars[j]], row$partial_r,
                  tier = "display", precision = 6,
                  label = sprintf("matrix cell %s x %s", vars[i], vars[j]))
    }
  }
})


test_that("Weighted partial r matches the weighted residual oracle, df uses unrounded sum(w)", {
  r <- partial_cor(survey_data, life_satisfaction, income,
                   controls = age, weights = sampling_weight)
  ref <- .ref_partial_r(survey_data, "life_satisfaction", "income", "age",
                        w = "sampling_weight")

  assert_spss(r$correlations$partial_r[1], ref$r,
              tier = "display", precision = 5,
              label = "weighted partial r vs weighted residual oracle")
  assert_spss(r$correlations$df[1], ref$n_eff - 3,
              tier = "display", precision = 5,
              label = "weighted df = sum(w) - 2 - k, unrounded")
  expect_false(r$correlations$df[1] == round(r$correlations$df[1]))
})


test_that("Grouped results equal per-subset ungrouped results (SPSS SPLIT FILE)", {
  rg <- survey_data |> group_by(gender) |>
    partial_cor(life_satisfaction, income, controls = age)
  for (g in unique(survey_data$gender)) {
    sub <- survey_data[survey_data$gender == g, ]
    ru <- partial_cor(sub, life_satisfaction, income, controls = age)
    row <- rg$correlations[rg$correlations$gender == g, ]
    assert_spss(row$partial_r, ru$correlations$partial_r[1],
                tier = "display", precision = 6,
                label = sprintf("grouped partial r == subset (%s)", g))
    assert_spss_count(row$n, ru$correlations$n[1],
                      label = sprintf("grouped n == subset n (%s)", g))
  }
})


test_that("Weighted grouped partial r equals per-subset weighted results", {
  rg <- survey_data |> group_by(gender) |>
    partial_cor(life_satisfaction, income, controls = age,
                weights = sampling_weight)
  g <- unique(survey_data$gender)[1]
  sub <- survey_data[survey_data$gender == g, ]
  ru <- partial_cor(sub, life_satisfaction, income, controls = age,
                    weights = sampling_weight)
  row <- rg$correlations[rg$correlations$gender == g, ]
  assert_spss(row$partial_r, ru$correlations$partial_r[1],
              tier = "display", precision = 6,
              label = "weighted grouped partial r == subset")
})


test_that("Input contracts: overlap, missing controls, non-numeric error clearly", {
  expect_error(
    partial_cor(survey_data, age, income, controls = age),
    "both analysis and control"
  )
  expect_error(
    partial_cor(survey_data, age, income),
    "controls"
  )
  expect_error(
    partial_cor(survey_data, age, income, controls = gender),
    "not numeric"
  )
})
