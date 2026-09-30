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
                    paste0("income_", c("n", "eff_n", "weighted_n", "missing")))
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


# --- DESC-18 / DESC-13: uniform w_* print, N = sum of weights, summary() ------

test_that("DESC-18/DESC-13: the w_* family prints one uniform table", {
  # The prints differed across the family: raw column names
  # (weighted_mean, Effective_N) under "--- var ---" headers, w_modus as a
  # raw "# A tibble", w_quantile repeating the weights name on every row.
  # Weighted results showed Kish's effective N instead of SPSS's
  # N = sum of weights and no Missing; summary() fell back to
  # summary.default.
  fns <- list(w_mean = w_mean, w_median = w_median, w_sd = w_sd,
              w_var = w_var, w_se = w_se, w_range = w_range, w_iqr = w_iqr,
              w_skew = w_skew, w_kurtosis = w_kurtosis, w_modus = w_modus)
  for (nm in names(fns)) {
    f <- fns[[nm]]
    for (wt in c(FALSE, TRUE)) {
      r <- if (wt) f(survey_data, age, income, weights = sampling_weight)
           else f(survey_data, age, income)
      out <- capture.output(print(r))
      info <- paste(nm, if (wt) "weighted" else "unweighted")
      expect_false(any(grepl("weighted_|Effective|effective_n|# A tibble|^--- ",
                             out)), info = info)
      hdr <- out[grepl("^\\s*Variable\\s", out)]
      expect_length(hdr, 1L)
      expect_true(grepl("\\sN\\s", hdr) && grepl("Missing", hdr), info = info)
      inc <- out[grepl("^\\s*income\\s", out)]
      if (wt) {
        expect_true(grepl(" 2201 ", inc) && grepl(" 315$", inc), info = info)
      } else {
        expect_true(grepl(" 2186 ", inc) && grepl(" 314$", inc), info = info)
      }
      s <- summary(r)
      expect_s3_class(s, "summary.w_statistic")
      out_s <- capture.output(print(s))
      expect_equal(any(grepl("Effective N", out_s)), wt, info = info)
    }
  }

  q <- w_quantile(survey_data, age, income, weights = sampling_weight)
  out_q <- capture.output(print(q))
  expect_equal(sum(grepl("sampling_weight", out_q)), 1L)   # named once
  hdr_q <- out_q[grepl("^\\s*Variable\\s", out_q)]
  expect_true(grepl("Min", hdr_q) && grepl("25%", hdr_q) && grepl("Missing", hdr_q))
  expect_true(any(grepl("^\\s*income\\s.* 2201 ", out_q)))
  expect_s3_class(summary(q), "summary.w_quantile")
  expect_true(any(grepl("Effective N", capture.output(print(summary(q))))))

  # Single-variable results carry the same columns as multi-variable ones
  # (no duplicated raw columns income/income_n/income_eff_n)
  expect_named(w_mean(survey_data, age)$results,
               c("Variable", "mean", "n", "missing"))
  r1w <- w_mean(survey_data, income, weights = sampling_weight)$results
  expect_named(r1w, c("Variable", "weighted_mean", "weighted_n",
                      "effective_n", "missing"))
  expect_equal(round(r1w$weighted_n), 2201)
  expect_equal(round(r1w$missing), 315)
  expect_true(all(c("Variable", "mode", "n", "missing") %in%
                    names(w_modus(survey_data, gender)$results)))
})


# --- DESC-17: w_* input checks and w_modus ties -------------------------------

test_that("DESC-17: w_* give clear errors for bad input and report tied modes", {
  # w_mean(1:4, weights = c(1, 3)) failed with a cryptic base-R error
  # (German "Fehlender Wert, wo TRUE/FALSE noetig ist"), w_mean(c("a", "b"))
  # claimed "data must be a data frame", w_mean(survey_data, gender)
  # returned NA with a base-R warning, and a tie in w_modus() was not
  # reported at all (weighted: first value in data order).
  expect_error(w_mean(1:4, weights = c(1, 3)), "length")
  expect_error(w_median(c(1, 2, 3), weights = 1:2), "length")
  expect_error(w_mean(c("a", "b")), "numeric")
  expect_error(w_mean(survey_data, gender), "gender")
  expect_error(w_sd(survey_data, age, gender), "not numeric")
  expect_no_warning(expect_error(w_mean(survey_data, gender)))
  expect_error(w_mean("not a data frame", age), "must be a data frame")
  # w_modus works on any variable type
  expect_no_error(w_modus(survey_data, gender))

  # Ties: the smallest value (first level) is shown, as in SPSS, weighted
  # and unweighted alike, and the print says so
  tie <- data.frame(t = c(2, 2, 1, 1, 3), f = factor(c("b", "b", "a", "a", "c")),
                    w = rep(1, 5))
  expect_equal(w_modus(tie$t), 1)
  expect_equal(w_modus(tie$t, weights = tie$w), 1)
  expect_equal(as.character(w_modus(tie$f, weights = tie$w)), "a")
  r <- w_modus(tie, t, f, weights = w)
  expect_equal(r$results$n_modes, c(2L, 2L))
  out <- capture.output(print(r))
  expect_true(any(grepl("Multiple modes exist", out)))
  out1 <- capture.output(print(w_modus(survey_data, gender)))
  expect_false(any(grepl("Multiple modes", out1)))
})


# --- IO-12: weighted frequency() of several labelled variables -----------------

test_that("IO-12: weighted frequency() of several labelled variables does not crash", {
  # The weighted branch kept the haven_labelled class in the value column;
  # rbind() of two variables with different label sets then failed in
  # vec_cast ("loss of precision"), depending on the variable order.
  skip_if_not_installed("haven")
  set.seed(1)
  df <- data.frame(
    x = haven::labelled(rep(c(1, 2), 10), c(West = 1, East = 2)),
    y = haven::labelled(c(rep(1:3, 6), -9, -9),
                        c(Male = 1, Female = 2, Diverse = 3, "No answer" = -9)),
    w = runif(20, 0.5, 1.5)
  )
  r_xy <- expect_no_error(frequency(df, x, y, weights = w))
  r_yx <- expect_no_error(frequency(df, y, x, weights = w))
  expect_false(inherits(r_xy$results$value, "haven_labelled"))
  pick <- function(r, v) {
    out <- r$results[r$results$Variable == v, c("value", "label", "freq")]
    rownames(out) <- NULL
    out
  }
  expect_equal(pick(r_xy, "y"), pick(r_yx, "y"))
  expect_equal(pick(r_xy, "x"), pick(r_yx, "x"))
  expect_equal(pick(r_xy, "y")$label, c("No answer", "Male", "Female", "Diverse"))
})


# --- DESC-08 (frequency part): empty factor levels ----------------------------

test_that("DESC-08: frequency() hides empty factor levels unless show_unused = TRUE", {
  # table() keeps every factor level, so after filter(employment !=
  # "Student") the table listed "Student 0 0.00" although show_unused =
  # FALSE is the default (SPSS FREQUENCIES lists observed values only).
  d <- dplyr::filter(survey_data, employment != "Student")
  r <- frequency(d, employment)
  expect_false("Student" %in% r$results$value)
  expect_false(any(grepl("Student", capture.output(print(r)))))
  expect_true("Student" %in% frequency(d, employment, show_unused = TRUE)$results$value)

  rw <- frequency(d, employment, weights = sampling_weight)
  expect_false("Student" %in% as.character(rw$results$value))
  rwu <- frequency(d, employment, weights = sampling_weight, show_unused = TRUE)
  expect_true("Student" %in% as.character(rwu$results$value))
  expect_equal(rwu$results$freq[as.character(rwu$results$value) %in% "Student"], 0)
})


# --- DESC-12 (label mode): labels of missing-value codes ----------------------

test_that("DESC-12: auto label mode shows the labels of missing-value codes", {
  # show_labels = "auto" looked only at the labels of valid values, so a
  # metric variable such as ALLBUS age printed its missing code -32
  # without the label "NICHT GENERIERBAR".
  skip_if_not_installed("haven")
  x <- haven::labelled(c(18, 25, 40, 40, haven::tagged_na("a")),
                       labels = c("NICHT GENERIERBAR" = haven::tagged_na("a")),
                       label = "Age")
  attr(x, "na_tag_map") <- c(a = -32)
  attr(x, "na_tag_format") <- "spss"
  d <- tibble::tibble(age = x)
  r <- frequency(d, age)
  expect_true(r$options$show_labels)
  out <- capture.output(print(r))
  expect_true(any(grepl("-32", out) & grepl("NICHT GENERIERBAR", out)))
  # without labelled missing codes the auto mode stays off
  expect_false(frequency(survey_data, age)$options$show_labels)
})


# --- DESC-03/09/10/11/12: frequency() table layout -----------------------------

# Cells of the "|"-delimited table rows of a frequency print
.fre_rows <- function(out) {
  rows <- out[grepl("^\\|", out)]
  lapply(strsplit(rows, "|", fixed = TRUE), function(r) trimws(r[-1]))
}

test_that("DESC-11: frequency() table has SPSS-like total rows and no NA cells", {
  # Missing and total rows showed literal "NA" in Valid %/Cum. %, plain
  # numeric/logical/character variables got two rows both called "Total",
  # tagged missing values ended with a "NA(total)" row, and there was no
  # grand total (SPSS ends with "Total 2500 100.0").
  out <- capture.output(print(frequency(survey_data, life_satisfaction)))
  rows <- .fre_rows(out)
  first <- vapply(rows, `[`, "", 1)
  expect_false(any(unlist(lapply(rows, `[`, -1)) == "NA"))
  expect_equal(sum(first == "Total valid"), 1L)
  expect_equal(sum(first == "Total"), 1L)
  expect_equal(first[length(first)], "Total")
  last <- rows[[length(rows)]]
  expect_true("2500" %in% last && "100.00" %in% last)
  expect_equal(sum(first == "NA"), 1L)                   # system missing row
  expect_false(any(first == "Total missing"))           # only one missing row

  for (x in list(c(1, 2, 2, 3, NA), c(TRUE, FALSE, NA), c("a", "b", NA))) {
    o <- capture.output(print(frequency(tibble::tibble(x = x), x)))
    f <- vapply(.fre_rows(o), `[`, "", 1)
    expect_equal(sum(f == "Total"), 1L)
    expect_equal(sum(f == "Total valid"), 1L)
  }

  # Without missing values there is a single Total row (as in SPSS)
  f <- vapply(.fre_rows(capture.output(print(frequency(survey_data, gender)))),
              `[`, "", 1)
  expect_equal(sum(grepl("^Total", f)), 1L)

  skip_if_not_installed("haven")
  x <- haven::labelled(c(1, 2, 2, haven::tagged_na("a"), haven::tagged_na("b"), NA),
                       labels = c(Low = 1, High = 2,
                                  "No answer" = haven::tagged_na("a"),
                                  "Refused" = haven::tagged_na("b")))
  attr(x, "na_tag_map") <- c(a = -9, b = -8)
  attr(x, "na_tag_format") <- "spss"
  o <- capture.output(print(frequency(tibble::tibble(x = x), x)))
  expect_false(any(grepl("NA(total)", o, fixed = TRUE)))
  f <- vapply(.fre_rows(o), `[`, "", 1)
  expect_true(all(c("Total valid", "-9", "-8", "NA", "Total missing", "Total") %in% f))
  expect_false(any(unlist(lapply(.fre_rows(o), `[`, -1)) == "NA"))
})

test_that("DESC-10: show_valid = FALSE also hides the cumulative (valid) percent", {
  out <- capture.output(print(frequency(survey_data, political_orientation,
                                        show_valid = FALSE)))
  expect_false(any(grepl("Cum. %", out, fixed = TRUE)))
  expect_false(any(grepl("Valid %", out, fixed = TRUE)))
  expect_true(any(grepl("Raw %", out, fixed = TRUE)))
})

test_that("DESC-03: frequency() N column is sized to its content", {
  # A fixed 8-character N column cut large weighted counts to "2798...".
  d <- survey_data |> mutate(popw = sampling_weight * 33000)
  out <- capture.output(print(frequency(d, education, weights = popw)))
  expect_false(any(grepl("...", out, fixed = TRUE)))
  expect_true(any(grepl("83031049", out, fixed = TRUE)))
})

test_that("DESC-09: all-missing variable prints without NaN, 100% of 0 or warnings", {
  r <- frequency(tibble::tibble(allna = rep(NA_real_, 5)), allna)
  out <- expect_no_warning(capture.output(print(r)))
  expect_false(any(grepl("NaN", out)))
  rows <- .fre_rows(out)
  expect_false(any(vapply(rows, function(r) r[1] == "Total valid", logical(1))))
  last <- rows[[length(rows)]]
  expect_equal(last[1], "Total")
  expect_true("5" %in% last && "100.00" %in% last)
})

test_that("DESC-12: frequency() table formatting (factors, labels, width, header)", {
  # Factors printed identical Value and Label columns and a header
  # "mean=NA sd=NA skewness=NA"; labels were right-aligned and cut at 40
  # characters; the table ignored the console width; print(digits = 0)
  # rounded the header statistics to integers.
  out <- capture.output(print(frequency(survey_data, gender)))
  hdr <- out[grepl("^\\|\\s*Value", out)]
  expect_false(grepl("Label", hdr))
  expect_false(any(grepl("=NA", out)))
  expect_true(any(grepl("# total N=2500 valid N=2500", out, fixed = TRUE)))
  # an unlabelled numeric variable next to a factor gets no empty Label column
  out_ga <- capture.output(print(frequency(survey_data, gender, age)))
  expect_false(any(grepl("Label", out_ga)))

  skip_if_not_installed("haven")
  long <- "A very long value label that clearly exceeds forty characters"
  x <- haven::labelled(c(1, 1, 2), labels = c(Short = 1, stats::setNames(2, long)))
  old <- options(width = 200)
  on.exit(options(old))
  o <- capture.output(print(frequency(tibble::tibble(x = x), x)))
  expect_true(any(grepl(long, o, fixed = TRUE)))        # no 40-char cut
  expect_true(any(grepl("^\\| +1 \\| Short +\\|", o)))  # left-aligned label

  options(width = 60)
  o2 <- capture.output(print(frequency(tibble::tibble(x = x), x)))
  expect_true(all(nchar(o2) <= 60))
  rows2 <- .fre_rows(o2)
  rows2 <- rows2[!vapply(rows2, function(r) grepl("^(Total|Value)", r[1]), logical(1))]
  cells <- vapply(rows2, `[`, "", 2)
  expect_equal(paste(cells[cells != "" & cells != "Short"], collapse = " "),
               long)                                     # wrapped, not cut
  options(old)

  o3 <- capture.output(print(frequency(survey_data, life_satisfaction), digits = 0))
  expect_true(any(grepl("mean=3.63", o3, fixed = TRUE)))
  expect_equal(.fre_rows(o3)[[2]], c("1", "118", "5", "5", "5"))  # rounded %
})


# --- EDGE-11: weighted crosstab with SPSS /COUNT ROUND CELL -------------------

test_that("EDGE-11: weighted crosstab rounds cells first; margins add up", {
  # Cells were rounded only for display while margins and percentages came
  # from the unrounded sums: 402 + 447 was shown with a margin of 848, and
  # DIVERS 15 | 3 had a row % of 81.7 (15/18 = 83.3). SPSS's default
  # /COUNT ROUND CELL rounds each cell first (see
  # test-crosstab-spss-validation.R for the SPSS reference values).
  r <- crosstab(survey_data, education, gender, weights = sampling_weight,
                percentages = "all")
  expect_true(all(r$table == round(r$table)))
  expect_equal(unname(r$row_totals), unname(rowSums(r$table)))
  expect_equal(unname(r$col_totals), unname(colSums(r$table)))
  expect_equal(r$total, sum(r$table))
  expect_equal(unname(r$row_pct), unname(r$table / rowSums(r$table) * 100))
  basic <- which(rownames(r$table) == "Basic Secondary")
  expect_equal(unname(r$row_totals[basic]), 849)       # 402 + 447
  out <- capture.output(print(r))
  expect_true(any(grepl("402.*447.*849", out)))
})
