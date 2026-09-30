# =============================================================================
# 0.7.4 practice-test fixes — batch X3 (cross-cutting): central formatting
# helpers (print_stat_table, pad_utf8, group headers, whole-number display),
# weighted-formula hygiene and the print/summary consistency sweep
# =============================================================================
# One block per finding ID (see NEWS "Practice-test fixes").

data(survey_data, envir = environment())

# Display width of every line of a captured table block
.widths <- function(lines) nchar(sub("\\s+$", "", lines), type = "width")

# --- EDGE-19 / PAR-22 / EDGE-17: print_stat_table() ---------------------------

test_that("EDGE-19: print_stat_table() pads by display width, not bytes", {
  # sprintf("%-20s") counts bytes: every umlaut shifted the rest of its row
  # one column to the left (pearson/tukey/linreg/dunn tables).
  df <- data.frame(
    Variable = c("Größe_öü", "age", "Zufriedenheit_ä"),
    value = c(1.5, 2.25, 3),
    stringsAsFactors = FALSE
  )
  out <- capture.output(mariposa:::print_stat_table(df, digits = 3))
  expect_length(unique(.widths(out)), 1L)
  # right-aligned numbers end in the same display column
  rows <- out[grepl("[0-9]\\.[0-9]{3}", out)]
  ends <- regexpr("[0-9]$", sub("\\s+$", "", rows))
  expect_length(unique(nchar(substr(rows, 1, ends), type = "width")), 1L)
})

test_that("EDGE-17: print_stat_table() shows whole numbers beyond 2^31", {
  # formatC(format = "d") coerces to integer: counts or sums of weights of
  # 2^31 and more (expansion weights) printed "NA" with a coercion warning.
  df <- data.frame(Group = c("a", "b"), N = c(3e9, 12))
  expect_silent(out <- capture.output(mariposa:::print_stat_table(df)))
  expect_true(any(grepl("3000000000", out, fixed = TRUE)))
  expect_false(any(grepl("NA", out, fixed = TRUE)))
})

test_that("EDGE-17: sums of weights beyond 2^31 print as whole numbers everywhere", {
  # formatC(format = "d") / as.integer() on a sum of expansion weights
  # printed "N = NA" (rank tests, goodness of fit) and an N of "NA" in the
  # one-sample table, with integer-coercion warnings.
  d <- survey_data
  d$big_w <- d$sampling_weight * 1e6          # sum(w) ~ 2.5e9 > 2^31
  expect_no_warning(out <- c(
    capture.output(print(kruskal_wallis(d, life_satisfaction,
                                        group = education, weights = big_w))),
    capture.output(print(summary(kruskal_wallis(d, life_satisfaction,
                                                group = education,
                                                weights = big_w)))),
    capture.output(print(chisq_gof(d, education, weights = big_w))),
    capture.output(print(mann_whitney(d, age, group = gender, weights = big_w))),
    capture.output(print(wilcoxon_test(d, trust_government, trust_media,
                                       weights = big_w))),
    capture.output(print(summary(t_test(d, age, mu = 50, weights = big_w)))),
    capture.output(print(summary(oneway_anova(d, age, group = education,
                                              weights = big_w))))
  ))
  expect_false(any(grepl("\\bNA\\b", out)))
  expect_true(any(grepl("N = 2516092404", out, fixed = TRUE)))
})

test_that("FMT-SPACE: print_stat_table() lines carry no trailing blank", {
  # cat(row, "\n") appended a space to every table line.
  df <- data.frame(Term = c("a", "b"), B = c(1.5, -2))
  out <- capture.output(mariposa:::print_stat_table(df))
  expect_false(any(grepl(" $", out)))
})

test_that("FMT-ALIGN: leading text columns of print_stat_table() are left-aligned", {
  # Only the first column was left-aligned: "Group 2" of dunn_test() and
  # "Variable 2" of partial_cor() were right-aligned under their header.
  df <- data.frame(g1 = c("Basic", "University"), g2 = c("University", "Basic"),
                   z = c(-1.5, 2), stringsAsFactors = FALSE)
  out <- capture.output(mariposa:::print_stat_table(df, digits = 3))
  hdr <- out[2]
  row2 <- out[5]
  expect_identical(regexpr("g2", hdr, fixed = TRUE)[1],
                   regexpr("Basic", row2, fixed = TRUE)[1])
  # a numeric first column is right-aligned like every number
  num_first <- data.frame(H = c(1.5, 171.178), df = c(3, 3))
  out2 <- capture.output(mariposa:::print_stat_table(num_first, digits = 3))
  expect_match(out2[4], "^    1\\.500")
})

# --- REG-16 / SCALE: legacy correlation-matrix printer ------------------------

test_that("REG-16: the legacy .print_cor_matrix()/.print_single_pair() are gone", {
  # .print_cor_matrix() raised options(width) temporarily, dropped to 2
  # decimals above 6 variables ignoring digits and printed a 0.0000
  # diagonal; reliability() now prints its own numbered matrix and the
  # correlation classes use .print_cor_matrix_fit(). Neither helper had a
  # caller left, only tests kept them alive.
  ns <- asNamespace("mariposa")
  expect_false(exists(".print_cor_matrix", envir = ns, inherits = FALSE))
  expect_false(exists(".print_single_pair", envir = ns, inherits = FALSE))
  # reliability honours digits for more than 6 items and leaves width alone
  set.seed(3)
  f <- stats::rnorm(200)
  d <- as.data.frame(replicate(7, round(f + stats::rnorm(200), 1)))
  old <- getOption("width")
  out <- capture.output(print(summary(reliability(d, dplyr::everything()),
                                      digits = 4)))
  expect_identical(getOption("width"), old)
  i <- grep("Inter-Item Correlation Matrix", out, fixed = TRUE)
  expect_true(any(grepl("1\\.0000", out[i:(i + 12)])))
})

# --- EDGE-23: one group-header style, no trailing blanks in headers -----------

test_that("EDGE-23: verbose group headers share one style without a trailing blank", {
  # describe()/w_*()/levene summary printed "Group: region = East " (cat()
  # added a blank, no underline) while every other summary underlined the
  # header: four header styles for the same thing.
  hdr <- capture.output(mariposa:::print_group_label("region = East"))
  expect_identical(hdr, c("", "Group: region = East", "--------------------"))
  expect_identical(
    hdr,
    capture.output(mariposa:::print_group_header(
      data.frame(region = factor("East", levels = c("East", "West")))
    ))
  )
  g <- dplyr::group_by(survey_data, region)
  for (out in list(capture.output(print(describe(g, age))),
                   capture.output(print(w_mean(g, age))),
                   capture.output(print(summary(levene_test(g, age,
                                                            group = gender)))))) {
    i <- grep("^Group: region = East", out)
    expect_length(i, 1L)
    expect_identical(out[i], "Group: region = East")
    expect_match(out[i + 1L], "^-{20}$")
  }
})

test_that("EDGE-23: grouped tables never follow the header underline directly", {
  # With the underlined header a table rule right below it would make a
  # double rule; each grouped table is separated by a blank line.
  rule <- function(l) grepl("^\\s*-+\\s*$", l)
  g <- dplyr::group_by(survey_data, region)
  for (out in list(capture.output(print(describe(g, age))),
                   capture.output(print(w_mean(g, age))),
                   capture.output(print(w_quantile(g, age))),
                   capture.output(print(summary(normality_test(g, age)))))) {
    r <- rule(out)
    expect_false(any(r[-1] & r[-length(r)]))
  }
})

test_that("EDGE-23: section titles without a suffix end without a blank", {
  # get_standard_title(name, w, "") returned "name " - the underline of
  # "Pearson Correlation " was one dash longer than the title.
  expect_identical(mariposa:::get_standard_title("Levene's Test", NULL, ""),
                   "Levene's Test")
  expect_identical(mariposa:::get_standard_title("Levene's Test", "w", ""),
                   "Weighted Levene's Test")
  out <- capture.output(print(summary(pearson_cor(survey_data, age, income,
                                                  life_satisfaction))))
  i <- grep("^Pearson Correlation", out)[1]
  expect_identical(out[i], "Pearson Correlation")
  expect_identical(nchar(out[i + 1L]), nchar(out[i]))
  for (out in list(
    capture.output(print(summary(chi_square(survey_data, education, gender)))),
    capture.output(print(summary(levene_test(survey_data, age, group = gender))))
  )) {
    expect_false(any(grepl("[^ ] $", out)))
  }
})
