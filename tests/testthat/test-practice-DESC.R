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


# --- DESC-15: show validation, quantile names, duplicate Q50, NaN -------------

test_that("DESC-15: describe() validates show and prints no NaN or duplicates", {
  # Unknown show values were silently ignored (show = "min" printed only
  # N/Missing), probs = 1/3 gave the header "Q33.3333333333333",
  # show = "all" printed Q50 next to the identical Median, and a constant
  # variable showed Skewness "NaN".
  expect_error(describe(survey_data, age, show = "min"), "min")
  expect_error(describe(survey_data, age, show = c("mean", "sdev")), "sdev")

  r <- describe(survey_data, age, show = "quantiles", probs = 1 / 3)
  expect_true("age_Q33.33" %in% names(r$results))
  expect_false(any(grepl("33.3333", capture.output(print(r)))))

  r_all <- describe(survey_data, age, show = "all")
  out <- capture.output(print(r_all))
  expect_false(any(grepl("Q50", out)))
  expect_true(any(grepl("Median", out)))
  expect_true("age_Q50" %in% names(r_all$results))  # still in the result

  rk <- describe(survey_data |> mutate(k = 5), k, show = "all")
  expect_false(any(vapply(rk$results, function(v) any(is.nan(v)), logical(1))))
  expect_false(any(grepl("NaN", capture.output(print(rk)))))
})


# --- EDGE-08: grouping variable inside the selection ---------------------------

test_that("EDGE-08: describe()/w_* drop grouping variables from the selection", {
  # Selecting the grouping variable itself (explicitly or via
  # where(is.numeric)) crashed: "'x' ist NULL" in describe(), a dplyr
  # internal error in w_mean(). Like dplyr::across(), grouping columns are
  # now excluded, with a message.
  g <- survey_data |> mutate(g = as.integer(region)) |> group_by(g)
  expect_message(r <- describe(g, age, g), "Grouping variable")
  expect_equal(r$variables, "age")
  expect_message(r2 <- describe(g, where(is.numeric)), "Grouping variable")
  expect_false("g" %in% r2$variables)
  expect_true("age" %in% r2$variables)
  expect_message(w <- w_mean(g, age, g, weights = sampling_weight),
                 "Grouping variable")
  expect_equal(w$variables, "age")
  expect_equal(nrow(w$results), 2L)
  expect_error(suppressMessages(describe(g, g)), "grouping")

  skip_if_not_installed("haven")
  d <- survey_data
  d$lab <- haven::labelled(as.integer(d$region), c(East = 1, West = 2))
  expect_message(r3 <- describe(group_by(d, lab), age, lab), "Grouping variable")
  expect_equal(r3$variables, "age")
})


# --- DESC-13: weighted N and Missing (describe) --------------------------------

test_that("DESC-13: weighted describe() prints N = sum of weights and weighted Missing", {
  # Weighted describe() printed only Kish's effective N (2158.9 for income)
  # under the name Effective_N and dropped the Missing column. SPSS
  # FREQUENCIES with WEIGHT BY prints N Valid = sum of weights (2201) and
  # Missing = weighted missing (315), see describe_output.txt Test 2b.
  r <- describe(survey_data, age, income, weights = sampling_weight)
  expect_equal(round(r$results$income_N), 2201)
  expect_equal(round(r$results$income_Missing), 315)
  expect_true("income_Effective_N" %in% names(r$results))  # kept in the object

  out <- capture.output(print(r))
  expect_false(any(grepl("Effective", out)))
  hdr <- out[grepl("Variable", out)]
  expect_true(any(grepl("Missing", hdr)))
  row <- out[grepl("^\\s*income\\s", out)]
  expect_true(any(grepl(" 2201 ", row)) && any(grepl(" 315\\b", row)))
})


# --- DESC-16: describe() table layout -----------------------------------------

test_that("DESC-16: describe() tables are sized to content and wrap with the Variable column", {
  # Grouped output put a 40-dash separator right under the group underline
  # (double rule), the footer was always 40 dashes whatever the table
  # width, and tables wider than the console were wrapped by
  # print.data.frame so the continuation block lost the Variable column.
  rule <- function(l) grepl("^\\s*-+\\s*$", l)

  out <- capture.output(print(survey_data |> group_by(region) |> describe(age, income)))
  r <- rule(out)
  expect_false(any(r[-1] & r[-length(r)]))            # never two rules in a row
  hdr <- which(grepl("^\\s*Variable\\s", out))
  expect_length(hdr, 2L)                               # one table per group
  for (h in hdr) {
    expect_true(rule(out[h - 1]) && rule(out[h + 1]))
    expect_equal(nchar(trimws(out[h - 1])), nchar(trimws(out[h])))
  }

  # One decimal policy per column (no "50" next to "50.550"); the title
  # underline is not directly followed by the table rule
  out_u <- capture.output(print(describe(survey_data, age, income)))
  r_u <- rule(out_u)
  expect_false(any(r_u[-1] & r_u[-length(r_u)]))
  age_row <- out_u[grepl("^\\s*age\\s", out_u)]
  expect_length(age_row, 1L)                           # fits in one block
  expect_true(grepl("50\\.000", age_row))              # Median of age

  # A table wider than the console is split into column blocks that each
  # repeat the Variable column
  old <- options(width = 60)
  on.exit(options(old))
  out_w <- capture.output(print(describe(survey_data, age, income, show = "all")))
  expect_true(all(nchar(out_w) <= 60))
  expect_gte(sum(grepl("^\\s*Variable\\s", out_w)), 2L)
  data_rows <- out_w[grepl("[0-9]", out_w) & !rule(out_w) &
                       !grepl("^\\s*Variable\\s", out_w)]
  expect_true(all(grepl("^\\s*(age|income)\\s", data_rows)))
})


# --- DESC-01 / EDGE-01: w_* print with two grouping variables ------------------

test_that("DESC-01: w_* print shows every combination of two group_by variables", {
  # The print iterated over the first grouping variable only and took the
  # first row per level: region x gender printed two blocks labelled
  # "region = East"/"West" that silently showed the Male rows.
  g <- survey_data |> group_by(region, gender)
  res <- w_mean(g, age, weights = sampling_weight)
  out <- capture.output(print(res))
  grp <- grep("^Group:", out, value = TRUE)
  expect_length(grp, 4L)
  expect_true(any(grepl("region = East, gender = Female", grp)))
  r <- res$results
  v <- r$weighted_mean[r$region == "East" & r$gender == "Female"]
  expect_true(any(grepl(sub("0+$", "", sprintf("%.3f", v)), out, fixed = TRUE)))

  out_m <- capture.output(print(w_modus(g, education, weights = sampling_weight)))
  expect_length(grep("^Group:", out_m), 4L)
  expect_true(any(grepl("region = West, gender = Female", out_m)))
})
