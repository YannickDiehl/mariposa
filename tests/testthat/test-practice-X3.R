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

# --- Weighted-formula hygiene -------------------------------------------------

.weighted_defs_outside_kernels <- function(r_dir) {
  files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  bad <- character(0)
  for (f in files) {
    if (basename(f) == "kernels-weighted.R") next
    exprs <- parse(f, keep.source = FALSE)
    for (e in exprs) {
      if (is.call(e) && (identical(e[[1]], as.name("<-")) ||
                         identical(e[[1]], as.name("="))) &&
          is.name(e[[2]]) && grepl("^\\.weighted_", as.character(e[[2]]))) {
        bad <- c(bad, paste0(basename(f), ": ", as.character(e[[2]])))
      }
    }
  }
  bad
}

test_that("SCALE note: weighted formulas are defined only in kernels-weighted.R", {
  # .weighted_cov/.weighted_cor/.weighted_cor_vec lived in reliability.R
  # (also used by efa()), against the rule that every weighted formula
  # lives in R/kernels-weighted.R (the weighted variance once drifted in
  # six files).
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if(!dir.exists(r_dir), "R/ sources not available (installed package)")
  expect_identical(.weighted_defs_outside_kernels(r_dir), character(0))
  # behaviour unchanged: w == 1 reproduces the unweighted matrices
  m <- as.matrix(stats::na.omit(survey_data[c("trust_government",
                                               "trust_media", "trust_science")]))
  w1 <- rep(1, nrow(m))
  expect_equal(mariposa:::.weighted_cov(m, w1), stats::cov(m), ignore_attr = TRUE)
  expect_equal(mariposa:::.weighted_cor(m, w1), stats::cor(m), ignore_attr = TRUE)
  expect_equal(mariposa:::.weighted_cor_vec(m[, 1], m[, 2], w1),
               stats::cor(m[, 1], m[, 2]))
})

# --- FMT-THOU: no thousands separators -----------------------------------------

test_that("DESC-22: counts carry no thousands separators (codebook HTML, MW U)", {
  skip_if_not_installed("htmltools")
  # The HTML codebook wrote "2,500 observations" while the console header
  # and every table print 2500 (SPSS default); mann_whitney()'s compact line
  # printed "U = 776,732" but its summary table 776732.
  cb <- codebook(survey_data, age, gender, view = FALSE)
  html <- paste(as.character(cb$html), collapse = "\n")
  expect_match(html, "2500 observations", fixed = TRUE)
  expect_false(grepl("2,500", html, fixed = TRUE))

  out <- capture.output(print(mann_whitney(survey_data, age, group = gender)))
  expect_true(any(grepl("U = 776732,", out, fixed = TRUE)))
  d <- survey_data[survey_data$region == "East", ]
  out2 <- capture.output(print(mann_whitney(d, life_satisfaction, group = gender)))
  expect_true(any(grepl("U = 26095.5,", out2, fixed = TRUE)))
})

# --- Sweep: weighted frequency() of a factor and a numeric variable -----------

test_that("sweep: weighted frequency() of a factor next to a numeric variable", {
  # The weighted branch kept the factor as the value column; rbind() with
  # the numeric variable's rows coerced its values 1..5 to factor NA
  # ("ungueltiges Faktorniveau, NA erzeugt"): every category of
  # life_satisfaction printed as NA under "Total missing".
  expect_no_warning(
    f <- frequency(survey_data, education, life_satisfaction,
                   weights = sampling_weight)
  )
  ls <- f$results[f$results$Variable == "life_satisfaction", ]
  expect_identical(sum(!is.na(ls$value)), 5L)
  u <- frequency(survey_data, life_satisfaction, weights = sampling_weight)
  expect_equal(ls$freq, u$results$freq)
  out <- capture.output(print(f))
  expect_false(any(grepl("|            NA |  119", out, fixed = TRUE)))
  # grouped as well
  expect_no_warning(
    fg <- frequency(dplyr::group_by(survey_data, region), education,
                    life_satisfaction, weights = sampling_weight)
  )
  expect_true(all(c(1, 5) %in% suppressWarnings(as.numeric(fg$results$value))))
})

# --- EDGE-23: one pair separator ----------------------------------------------

test_that("EDGE-23: variable pairs are joined by ASCII 'x' everywhere", {
  # chi_square/fisher_test/mcnemar_test/crosstab titles used the
  # multiplication sign (U+00D7), the correlation, partial-correlation,
  # factorial and post-hoc output an ASCII "x"; print output is ASCII.
  d <- survey_data
  d$hi_gov <- as.integer(d$trust_government >= 4)
  d$hi_med <- as.integer(d$trust_media >= 4)
  outs <- c(
    capture.output(print(chi_square(d, education, gender))),
    capture.output(print(summary(chi_square(d, education, gender)))),
    capture.output(print(fisher_test(d, gender, region))),
    capture.output(print(mcnemar_test(d, hi_gov, hi_med))),
    capture.output(print(crosstab(d, education, gender)))
  )
  expect_false(any(grepl("×", outs, fixed = TRUE)))
  expect_true(any(grepl("education x gender", outs, fixed = TRUE)))
  expect_true(any(grepl("Table size: 4 x 2", outs, fixed = TRUE)))
})

# --- EDGE-22: digits honoured in the compact lines ------------------------------

test_that("EDGE-22: print(digits =) works for regressions and normality_test", {
  # print.linear_regression/print.logistic_regression/print.normality_test
  # had no digits argument: R2 = 0.201 and KS = 0.028 whatever was asked.
  lr <- linear_regression(survey_data, life_satisfaction ~ age + income)
  expect_match(capture.output(print(lr, digits = 1))[2], "R2 = 0\\.2, adj\\.R2 = 0\\.2")
  expect_match(capture.output(print(lr, digits = 4))[2], "R2 = 0\\.2010, adj\\.R2 = 0\\.2002")
  d <- survey_data
  d$hi <- as.integer(d$life_satisfaction >= 4)
  lg <- logistic_regression(d, hi ~ age + income)
  expect_match(capture.output(print(lg, digits = 2))[2], "Nagelkerke R2 = 0\\.21,")
  nt <- normality_test(survey_data, age)
  expect_match(capture.output(print(nt, digits = 2))[2],
               "KS = 0\\.03, p < 0\\.001; Shapiro-Wilk W = 0\\.99")
})

test_that("EDGE-23: grouped normality_test() compact print shows the results", {
  # It printed only "2 group combination(s) x 1 variable(s)".
  nt <- normality_test(dplyr::group_by(survey_data, region), age)
  out <- capture.output(print(nt))
  expect_true(any(grepl("Grouped: region", out, fixed = TRUE)))
  expect_true(any(grepl("[region = East]", out, fixed = TRUE)))
  expect_true(any(grepl("[region = West]", out, fixed = TRUE)))
  expect_identical(sum(grepl("^  age: KS = ", out)), 2L)
  expect_false(any(grepl("group combination", out, fixed = TRUE)))
})

test_that("EDGE-23: grouped regression compact prints use [group] lines", {
  # Regressions printed "  region = East: R2 = ..." while every other
  # compact print puts the group on its own "[region = East]" line.
  d <- survey_data
  d$hi <- as.integer(d$life_satisfaction >= 4)
  g <- dplyr::group_by(d, region)
  for (o in list(linear_regression(g, life_satisfaction ~ age),
                 logistic_regression(g, hi ~ age))) {
    out <- capture.output(print(o))
    expect_true(any(grepl("[Grouped: region]", out, fixed = TRUE)))
    i <- which(out == "[region = East]")
    expect_length(i, 1L)
    expect_match(out[i + 1L], "^  (Nagelkerke )?R2 = ")
    expect_true("[region = West]" %in% out)
    expect_false(any(grepl("region = East:", out, fixed = TRUE)))
  }
})

test_that("EDGE-23: every compact test/model print points to summary()", {
  # Only about half of the compact prints (rank tests, chi-square family,
  # Levene, normality, partial_cor) ended with the summary() hint; t-test,
  # ANOVAs, correlations, regressions, reliability and efa did not.
  d <- survey_data
  d$hi <- as.integer(d$life_satisfaction >= 4)
  g <- dplyr::group_by(d, region)
  objs <- list(
    t_test(d, age, group = gender),
    oneway_anova(d, age, group = education),
    factorial_anova(d, dv = age, between = c(gender, region)),
    ancova(d, dv = life_satisfaction, between = gender, covariate = age),
    pearson_cor(d, age, income),
    spearman_rho(d, age, income, life_satisfaction),
    kendall_tau(d, age, life_satisfaction),
    linear_regression(d, life_satisfaction ~ age),
    logistic_regression(d, hi ~ age),
    reliability(d, trust_government, trust_media, trust_science),
    efa(d, trust_government, trust_media, trust_science, life_satisfaction),
    t_test(g, age, group = gender),
    linear_regression(g, life_satisfaction ~ age)
  )
  for (o in objs) {
    out <- capture.output(print(o))
    expect_identical(sum(grepl("Use summary() for detailed output.", out,
                               fixed = TRUE)), 1L,
                     info = class(o)[1])
    expect_identical(out[length(out)], "Use summary() for detailed output.",
                     info = class(o)[1])
  }
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

# --- 0.7.4 audit: rounding of exact halves ---------------------------------------

test_that("fmt_num rounds halves up like SPSS, not to the even digit", {
  # Was: formatC() rounds an exact .xx5 to the even digit: a mean rank of
  # 306/16 = 19.125 printed as 19.12 where SPSS shows 19.13
  # (pairwise_wilcoxon_output.txt:744).
  expect_identical(mariposa:::fmt_num(19.125, 2), "19.13")
  expect_identical(mariposa:::fmt_num(-19.125, 2), "-19.13")
  expect_identical(mariposa:::fmt_num(0.5, 0), "1")
  expect_identical(mariposa:::fmt_num(2.5, 0), "3")
  expect_identical(mariposa:::fmt_num(2.675, 2), "2.68")   # binary 2.67499999...
  expect_identical(mariposa:::fmt_num(1.23449, 3), "1.234")
  expect_identical(mariposa:::fmt_num(c(NA, 1), 1), c("", "1.0"))
  expect_identical(mariposa:::fmt_num(-0.0004, 3), "0.000")
})

# --- Tables as one block wherever they can be shown so (maintainer decision) ------

test_that("knitted HTML prints wide tables as one block, the console splits them", {
  # Rule (2026-09-30): tables stay one block like SPSS whenever that can be
  # displayed. A knitted HTML page scrolls wide blocks, so the console
  # width (80 in knitr) must not split describe() or correlation matrices.
  skip_if_not_installed("knitr")
  withr::local_options(width = 50)
  res <- describe(survey_data, age, income, show = "all")
  n_headers <- function(out) sum(grepl("Variable", out, fixed = TRUE))
  expect_gt(n_headers(capture.output(print(res))), 1)   # console: blocks

  withr::local_options(knitr.in.progress = TRUE)
  old <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  knitr::opts_knit$set(rmarkdown.pandoc.to = "html")
  withr::defer(knitr::opts_knit$set(rmarkdown.pandoc.to = old))
  expect_equal(n_headers(capture.output(print(res))), 1)  # HTML: one block
  cm <- capture.output(print(summary(pearson_cor(survey_data, age, income,
    life_satisfaction, trust_government, trust_media, trust_science))))
  expect_true(any(grepl(
    "age\\s+income\\s+life_satisfaction\\s+trust_government\\s+trust_media\\s+trust_science",
    cm)))
})
