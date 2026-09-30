# =============================================================================
# Practice-test regressions: descriptives batch (DESC)
# =============================================================================
# describe(), frequency(), crosstab(), multiple_response(), the w_* family
# and the codebook console output. Each test names the finding ID from the
# 2026-09 practice test (field report) and what was wrong.
# =============================================================================

library(testthat)
library(dplyr)

data(survey_data)

# Statistic columns of a describe() result (everything except N/Missing)
.desc_stat_cols <- function(r) {
  nm <- names(r$results)
  nm[!grepl("_(N|Missing|Effective_N)$", nm) & nm != "Variable"]
}


# --- DESC-02: na.rm = FALSE ---------------------------------------------------

test_that("DESC-02: na.rm = FALSE gives NA for every statistic, never a crash", {
  # The weighted quantile family returned shifted numbers (the weighted
  # median of income was 3800 instead of NA) and the unweighted describe()
  # / w_iqr() crashed with a base-R quantile() error.
  w <- survey_data$sampling_weight
  expect_true(is.na(w_median(survey_data$income, weights = w, na.rm = FALSE)))
  expect_true(all(is.na(w_quantile(c(1, 2, 3, NA, NA, NA), weights = rep(1, 6),
                                   na.rm = FALSE))))
  expect_true(all(is.na(w_quantile(c(1, 2, 3, NA), na.rm = FALSE))))
  expect_true(is.na(w_median(survey_data, income, weights = sampling_weight,
                             na.rm = FALSE)$results$weighted_median))
  expect_true(is.na(expect_no_error(
    w_iqr(survey_data, income, na.rm = FALSE))$results$iqr))
  expect_true(is.na(w_iqr(survey_data, income, weights = sampling_weight,
                          na.rm = FALSE)$results$weighted_iqr))
  q <- expect_no_error(w_quantile(survey_data, income, na.rm = FALSE))
  q_cols <- setdiff(grep("^income_", names(q$results), value = TRUE),
                    c("income_n", "income_eff_n"))
  expect_true(all(is.na(unlist(q$results[q_cols]))))

  r_u <- expect_no_error(describe(survey_data, income, na.rm = FALSE, show = "all"))
  r_w <- expect_no_error(describe(survey_data, income, weights = sampling_weight,
                                  na.rm = FALSE, show = "all"))
  for (r in list(r_u, r_w)) {
    vals <- unlist(r$results[.desc_stat_cols(r)])
    expect_true(all(is.na(vals)))
  }
  expect_no_error(capture.output(print(r_u), print(r_w)))

  # Variables without missing values are unaffected by na.rm = FALSE
  expect_equal(describe(survey_data, age, na.rm = FALSE, show = "all")$results,
               describe(survey_data, age, show = "all")$results)
  expect_equal(w_median(survey_data, age, weights = sampling_weight,
                        na.rm = FALSE)$results$weighted_median,
               w_median(survey_data, age, weights = sampling_weight)$results$weighted_median)
})


# --- DESC-14: quantile columns by exact name ---------------------------------

test_that("DESC-14: describe() finds quantile columns by exact name", {
  # The print located quantile columns with grep("^<var>_Q"): a second
  # variable called income_Quintile crashed the print (rbind column
  # mismatch) and a name with regex metacharacters (`Einkommen (EUR)`)
  # silently lost its quantile columns.
  d <- survey_data |>
    mutate(income_Quintile = dplyr::ntile(income, 5),
           `Einkommen (EUR)` = income)
  r <- describe(d, income, income_Quintile, show = "all")
  out <- expect_no_error(capture.output(print(r)))
  expect_true(any(grepl("Q25", out)) && any(grepl("Q75", out)))

  r2 <- describe(d, `Einkommen (EUR)`, show = "all")
  out2 <- capture.output(print(r2))
  expect_true(any(grepl("Q25", out2)) && any(grepl("Q75", out2)))
  expect_true(any(grepl("2700", out2)))  # SPSS Q25 of income
})
