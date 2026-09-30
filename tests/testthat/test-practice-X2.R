# =============================================================================
# Practice-test regressions: result export and knitr (batch X2)
# =============================================================================
# One block per finding of the 2026-09 field report (phase 2: parametric,
# nonparametric, scales, edge-case and io/labels reports). Each test names
# the finding ID and what was wrong.
# =============================================================================

library(testthat)
library(dplyr)

data(survey_data, envir = environment())

# --- UX-EXPORT (NP-22): post-hoc tables under $results --------------------------

test_that("NP-22 dunn_test() and pairwise_wilcoxon() expose their table as $results", {
  # Was: the comparison table lived only in $comparisons while $results
  # was NULL - unlike every other result class, so x$results silently gave
  # NULL (and a weights-invariance test compared NULL with NULL).
  dn <- dunn_test(kruskal_wallis(survey_data, age, group = education))
  expect_s3_class(dn$results, "data.frame")
  expect_identical(dn$results, dn$comparisons)
  expect_equal(nrow(dn$results), 6L)

  pw <- pairwise_wilcoxon(friedman_test(survey_data, trust_government,
                                        trust_media, trust_science))
  expect_s3_class(pw$results, "data.frame")
  expect_identical(pw$results, pw$comparisons)
  expect_equal(nrow(pw$results), 3L)
})

# --- UX-EXPORT (PAR-25, NP-22, SCALE-20, EDGE-20): as.data.frame() / tidy() ----

# One result object of every exported result class (small, fast calls)
x2_results <- function() {
  d <- survey_data
  set.seed(11)
  d$b1 <- rbinom(nrow(d), 1, 0.5)
  d$b2 <- rbinom(nrow(d), 1, 0.5)
  d$b3 <- rbinom(nrow(d), 1, 0.4)
  ow <- oneway_anova(d, age, group = education)
  kw <- kruskal_wallis(d, age, group = education)
  fr <- friedman_test(d, trust_government, trust_media, trust_science)
  lg <- logistic_regression(d, gender ~ age)
  suppressWarnings(list(
    ancova = ancova(d, dv = income, between = c(gender), covariate = c(age)),
    binomial_test = binomial_test(d, gender),
    chi_square = chi_square(d, gender, region),
    chisq_gof = chisq_gof(d, region),
    codebook = codebook(d, age, gender, view = FALSE),
    crosstab = crosstab(d, gender, region),
    describe = describe(d, age, income),
    dunn_test = dunn_test(kw),
    efa = efa(d, trust_government, trust_media, trust_science,
              political_orientation, life_satisfaction),
    factorial_anova = factorial_anova(d, dv = income,
                                      between = c(gender, region)),
    fisher_test = fisher_test(d, gender, region),
    frequency = frequency(d, gender, education),
    friedman_test = fr,
    kendall_tau = kendall_tau(d, age, income),
    kruskal_wallis = kw,
    levene_test = levene_test(d, age, group = gender),
    linear_regression = linear_regression(d, income ~ age + gender),
    logistic_regression = lg,
    mann_whitney = mann_whitney(d, age, group = gender),
    marginal_effects = marginal_effects(lg),
    mcnemar_test = mcnemar_test(d, b1, b2),
    multiple_response = multiple_response(d, b1, b2, b3),
    normality_test = normality_test(d, age, income),
    oneway_anova = ow,
    pairwise_wilcoxon = pairwise_wilcoxon(fr),
    partial_cor = partial_cor(d, age, income, controls = life_satisfaction),
    pearson_cor = pearson_cor(d, age, income, life_satisfaction),
    reliability = reliability(d, trust_government, trust_media, trust_science),
    scheffe_test = scheffe_test(ow),
    spearman_rho = spearman_rho(d, age, income),
    t_test = t_test(d, age, income, group = gender),
    tukey_test = tukey_test(ow),
    w_iqr = w_iqr(d, age),
    w_kurtosis = w_kurtosis(d, age),
    w_mean = w_mean(d, age, income, weights = sampling_weight),
    w_median = w_median(d, age),
    w_modus = w_modus(d, age),
    w_quantile = w_quantile(d, age, income),
    w_range = w_range(d, age),
    w_sd = w_sd(d, age),
    w_se = w_se(d, age),
    w_skew = w_skew(d, age),
    w_var = w_var(d, age),
    wilcoxon_test = wilcoxon_test(d, x = trust_government, y = trust_media)
  ))
}

test_that("UX-EXPORT every result class has an as.data.frame() method", {
  # Was: as.data.frame() failed for every class ("cannot coerce class
  # '"t_test"' to a data.frame"). Guard: any class with a print() method
  # (i.e. every result class, including future ones) must be convertible.
  s3 <- getNamespaceInfo(asNamespace("mariposa"), "S3methods")
  result_classes <- unique(s3[s3[, 1] == "print" &
                                !startsWith(s3[, 2], "summary."), 2])
  expect_gt(length(result_classes), 40)
  missing <- result_classes[vapply(result_classes, function(cls) {
    is.null(utils::getS3method("as.data.frame", cls, optional = TRUE))
  }, logical(1))]
  expect_identical(missing, character(0))
})

test_that("UX-EXPORT as.data.frame() gives a flat, CSV-writable table for every class", {
  # Was: as.data.frame() errored, and $results had list-columns
  # (group_stats, observed tables), so write.csv() failed as well.
  res <- x2_results()
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv))
  for (cls in names(res)) {
    x <- res[[cls]]
    expect_s3_class(x, cls)
    df <- as.data.frame(x)
    expect_identical(class(df), "data.frame", info = cls)
    expect_gt(nrow(df), 0)
    expect_false(any(vapply(df, is.list, logical(1))), info = cls)
    expect_identical(rownames(df), as.character(seq_len(nrow(df))),
                     info = cls)
    expect_no_error(utils::write.csv(df, csv, row.names = FALSE))
    tb <- tibble::as_tibble(x)
    expect_s3_class(tb, "tbl_df")
    expect_equal(as.data.frame(tb), df, info = cls)
  }
})

test_that("UX-EXPORT t_test/mann_whitney rows carry the flattened group statistics", {
  # Was: group names, means and SDs were only in the group_stats
  # list-column.
  tt <- t_test(survey_data, age, income, group = gender)
  df <- as.data.frame(tt)
  expect_equal(df$Variable, c("age", "income"))
  expect_equal(df$t_stat, tt$results$t_stat)
  expect_equal(df$p_value, tt$results$p_value)
  expect_equal(df$group1, c("Male", "Male"))
  expect_equal(df$group2, c("Female", "Female"))
  expect_equal(df$mean1[1], tt$results$group_stats[[1]]$group1$mean)
  expect_equal(df$sd2[2], tt$results$group_stats[[2]]$group2$sd)
  expect_false("group_stats" %in% names(df))

  one <- as.data.frame(t_test(survey_data, age, mu = 50))
  expect_equal(one$mean, mean(survey_data$age))

  mw <- as.data.frame(mann_whitney(survey_data, age, group = gender))
  expect_equal(mw$n1 + mw$n2, 2500)
  expect_true(all(c("mean_rank1", "mean_rank2", "U", "Z") %in% names(mw)))
})

test_that("UX-EXPORT describe() and w_quantile() become one row per variable", {
  # Was: the wide "<variable>_<statistic>" layout (one row per group, one
  # column per variable and statistic) could not be tabulated directly.
  d <- dplyr::mutate(survey_data, trust = trust_media)
  ds <- describe(d, trust, trust_media, age)
  df <- as.data.frame(ds)
  expect_equal(df$Variable, c("trust", "trust_media", "age"))
  expect_equal(df$Mean, c(ds$results$trust_Mean, ds$results$trust_media_Mean,
                          ds$results$age_Mean))
  expect_true(all(c("Median", "SD", "N", "Missing") %in% names(df)))
  expect_false(any(grepl("_Mean$", names(df))))

  wq <- as.data.frame(w_quantile(survey_data, age, income))
  expect_equal(wq$Variable, c("age", "income"))
  expect_true(all(c("Min", "25%", "50%", "Max") %in% names(wq)))
})

test_that("UX-EXPORT grouped results have the group keys as leading label columns", {
  # Was: no conversion; labelled grouping variables must appear as their
  # value labels (not codes), in SPSS order, as the first columns.
  skip_if_not_installed("haven")
  d <- survey_data
  d$reg <- haven::labelled(as.integer(d$region), labels = c(East = 1, West = 2))
  g <- dplyr::group_by(d, reg)

  ds <- as.data.frame(describe(g, age, income))
  expect_equal(names(ds)[1:2], c("reg", "Variable"))
  expect_s3_class(ds$reg, "factor")
  expect_equal(as.character(ds$reg), c("East", "East", "West", "West"))
  expect_equal(ds$Variable, c("age", "income", "age", "income"))

  tt <- as.data.frame(t_test(g, age, group = gender))
  expect_equal(names(tt)[1], "reg")
  expect_equal(as.character(tt$reg), c("East", "West"))

  rel <- suppressWarnings(reliability(dplyr::group_by(survey_data, region),
                                      trust_government, trust_media,
                                      trust_science))
  rel <- as.data.frame(rel)
  expect_equal(nrow(rel), 2L)
  expect_equal(names(rel)[1], "region")
  expect_true(all(c("alpha", "n_items", "n") %in% names(rel)))

  lr <- as.data.frame(linear_regression(dplyr::group_by(survey_data, region),
                                        income ~ age))
  expect_equal(names(lr)[1:2], c("region", "Term"))
  expect_equal(nrow(lr), 4L)

  ct <- as.data.frame(crosstab(dplyr::group_by(survey_data, region),
                               gender, education))
  expect_equal(names(ct)[1:3], c("region", "gender", "education"))
  expect_equal(sum(ct$n), 2500)
})

test_that("UX-EXPORT crosstab(), frequency(), efa(), reliability() tables", {
  # Was: no tabular export for these classes at all.
  ct <- crosstab(survey_data, gender, region)
  df <- as.data.frame(ct)
  expect_equal(nrow(df), 4L)
  expect_equal(sum(df$n), ct$total)
  expect_equal(levels(df$gender), c("Male", "Female"))
  expect_true(all(c("row_pct", "expected", "adj_residual") %in% names(df)))

  fr <- as.data.frame(frequency(survey_data, income))
  expect_true("missing" %in% names(fr))
  expect_equal(sum(fr$freq), 2500)
  expect_equal(sum(fr$missing), 1L)

  ef <- as.data.frame(efa(survey_data, trust_government, trust_media,
                          trust_science, political_orientation,
                          life_satisfaction))
  expect_equal(nrow(ef), 5L)
  expect_true(all(c("Variable", "communality") %in% names(ef)))

  rl <- as.data.frame(reliability(survey_data, trust_government,
                                  trust_media, trust_science))
  expect_equal(nrow(rl), 1L)
  expect_equal(rl$n_items, 3L)
})

test_that("UX-EXPORT frequency() with tagged missing values drops the summary rows", {
  # Was: the tagged-NA layout mixes "Total Valid"/"Total Missing" rows into
  # $results; a data table must only hold categories.
  skip_if_not_installed("haven")
  x <- haven::labelled(
    c(1, 2, 2, 1, 3, haven::tagged_na("a"), haven::tagged_na("b"), NA, 2, 1),
    labels = c(Low = 1, Mid = 2, High = 3,
               "No answer" = haven::tagged_na("a"),
               Refused = haven::tagged_na("b")))
  attr(x, "na_tag_map") <- c(a = -9, b = -8)
  df <- as.data.frame(frequency(data.frame(r = x), r))
  expect_false(any(df$label %in% c("Total Valid", "Total Missing")))
  expect_equal(sum(df$freq), 10)
  expect_equal(df$value[df$label == "No answer"], -9)
  expect_equal(sum(df$missing), 3L)
})

test_that("UX-EXPORT broom::tidy() works for the test classes with broom names", {
  # Was: tidy() worked only for the two regressions.
  skip_if_not_installed("broom")
  res <- x2_results()
  for (cls in setdiff(names(res), c("codebook", "linear_regression",
                                    "logistic_regression"))) {
    td <- broom::tidy(res[[cls]])
    expect_s3_class(td, "tbl_df")
    expect_gt(nrow(td), 0)
  }
  tt <- broom::tidy(res$t_test)
  expect_true(all(c("variable", "estimate", "statistic", "parameter",
                    "p.value", "conf.low", "conf.high", "method",
                    "alternative") %in% names(tt)))
  expect_equal(tt$statistic, res$t_test$results$t_stat)

  ow <- broom::tidy(res$oneway_anova)
  expect_true(all(c("statistic", "num.df", "den.df", "p.value") %in% names(ow)))

  tk <- broom::tidy(res$tukey_test)
  expect_true(all(c("contrast", "estimate", "adj.p.value") %in% names(tk)))

  fa <- broom::tidy(res$factorial_anova)
  expect_true(all(c("term", "sumsq", "meansq", "statistic", "p.value") %in%
                    names(fa)))

  nt <- broom::tidy(res$normality_test)
  expect_equal(nrow(nt), 4L)
  expect_setequal(unique(nt$method),
                  c("Kolmogorov-Smirnov (Lilliefors)", "Shapiro-Wilk"))

  pc <- broom::tidy(res$pearson_cor)
  expect_false("sig" %in% names(pc))
  expect_equal(pc$estimate, res$pearson_cor$correlations$correlation)

  # The regressions keep their lm/glm tidiers
  expect_true(all(c("term", "std.error") %in%
                    names(broom::tidy(res$linear_regression))))
})

# --- UX-EXPORT (IO-26, PAR-25, NP-22): write_xlsx() for analysis results ------

# Cell values of a sheet as a character matrix (no header interpretation)
x2_sheet <- function(file, sheet) {
  df <- openxlsx2::read_xlsx(file, sheet = sheet, col_names = FALSE,
                             skip_empty_rows = FALSE, skip_empty_cols = FALSE)
  m <- as.matrix(df)
  m[is.na(m)] <- ""
  dimnames(m) <- NULL
  m
}

test_that("IO-26 write_xlsx() exports describe(), t_test() and other results", {
  # Was: write_xlsx() had no method for describe/crosstab/test results
  # ("no applicable method"), and a list containing one was rejected.
  skip_if_not_installed("openxlsx2")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))

  tt <- t_test(survey_data, age, income, group = gender)
  expect_invisible(write_xlsx(tt, tmp))
  expect_equal(openxlsx2::wb_load(tmp)$get_sheet_names(),
               c(`t-Test` = "t-Test"))
  m <- x2_sheet(tmp, "t-Test")
  expect_equal(m[1, 1], "t-Test")
  back <- openxlsx2::read_xlsx(tmp, sheet = "t-Test", start_row = 2)
  expect_equal(back$Variable[1:2], c("age", "income"))
  expect_equal(back$t_stat[1:2], tt$results$t_stat)

  ow <- oneway_anova(survey_data, age, group = education)
  write_xlsx(ow, tmp)
  m <- x2_sheet(tmp, 1)
  expect_true("Descriptives" %in% m[, 1])      # secondary table below
  expect_true("University" %in% m[, 2])

  ds <- describe(dplyr::group_by(survey_data, region), age)
  write_xlsx(ds, tmp)
  back <- openxlsx2::read_xlsx(tmp, sheet = 1, start_row = 2)
  expect_equal(back$region, c("East", "West"))
  expect_equal(back$Mean, as.data.frame(ds)$Mean)
})

test_that("IO-26 write_xlsx() writes crosstabs in the SPSS table layout", {
  # Was: no crosstab export at all.
  skip_if_not_installed("openxlsx2")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))

  ct <- crosstab(survey_data, gender, region, percentages = "all")
  write_xlsx(ct, tmp)
  m <- x2_sheet(tmp, "Crosstabulation")
  expect_equal(m[1, 1], "gender * region Crosstabulation")
  expect_equal(m[3, ], c("gender", "", "East", "West", "Total"))
  male <- which(m[, 1] == "Male")
  expect_equal(m[male, 2], "Count")
  expect_equal(as.numeric(m[male, 3:5]),
               unname(c(unclass(ct$table)["Male", ], ct$row_totals[["Male"]])))
  expect_equal(m[male + 1:3, 2],
               c("% within gender", "% within region", "% of Total"))
  total <- which(m[, 1] == "Total")
  expect_equal(as.numeric(m[total, 5]), ct$total)

  # Grouped: one block per group, titled with the group
  write_xlsx(crosstab(dplyr::group_by(survey_data, region), gender,
                      education), tmp)
  m <- x2_sheet(tmp, 1)
  titles <- grep("Crosstabulation", m[, 1], value = TRUE)
  expect_equal(titles,
               c("gender * education Crosstabulation (region = East)",
                 "gender * education Crosstabulation (region = West)"))
})

test_that("IO-26 write_xlsx() lists may mix data frames and any result", {
  # Was: list elements other than data frames/frequency/codebook were
  # rejected ("All list elements must be data frames, ...").
  skip_if_not_installed("openxlsx2")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  res <- x2_results()
  expect_no_error(suppressMessages(write_xlsx(res, tmp)))
  sheets <- openxlsx2::wb_load(tmp)$get_sheet_names()
  expect_equal(unname(sheets), names(res))

  write_xlsx(list(Deskriptiv = describe(survey_data, age, income),
                  Kreuztabelle = crosstab(survey_data, gender, region),
                  Daten = head(survey_data)), tmp)
  sheets <- openxlsx2::wb_load(tmp)$get_sheet_names()
  expect_true(all(c("Deskriptiv", "Kreuztabelle", "Daten") %in% sheets))
})

test_that("IO-26 write_xlsx() names unsupported objects instead of dispatch errors", {
  # Was: "no applicable method for 'write_xlsx' applied to an object of
  # class ..." (German under a German locale).
  tmp <- tempfile(fileext = ".xlsx")
  expect_error(write_xlsx(matrix(1:4, 2), tmp), "cannot export")
  expect_error(write_xlsx(summary(t_test(survey_data, age, group = gender)),
                          tmp),
               "not its")
  expect_false(file.exists(tmp))
})

# --- UX-RMD (EDGE-21): codebook() in R Markdown ---------------------------------

test_that("EDGE-21 codebook() returns visibly", {
  # Was: invisible(), so a knitted chunk `codebook(data)` showed nothing
  # (view = interactive() is FALSE while knitting).
  expect_true(withVisible(codebook(survey_data, age, gender,
                                   view = FALSE))$visible)
})

test_that("EDGE-21 knit_print() embeds the HTML codebook in HTML documents", {
  # Was: no knit_print method; the knitted document got neither the HTML
  # codebook nor (because of the invisible return) the console overview.
  skip_if_not_installed("knitr")
  cb <- codebook(survey_data, age, gender, view = FALSE)
  old <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = old))

  knitr::opts_knit$set(rmarkdown.pandoc.to = "html")
  out <- knitr::knit_print(cb)
  expect_s3_class(out, "knit_asis")
  html <- paste(as.character(out), collapse = "\n")
  expect_match(html, "<table", fixed = TRUE)
  expect_match(html, "Age in years", fixed = TRUE)
  expect_match(html, "mariposa-codebook", fixed = TRUE)
  # A fragment, not a nested page, and CSS scoped to the codebook: the
  # standalone page's `body {...}`/`table {...}` rules must not restyle
  # the whole document
  expect_no_match(html, "<html|<body|<head")
  expect_no_match(html, "(^|[\n}])\\s*(body|table|h2)\\s*\\{")

  # Other output formats (PDF, Word) get the console overview
  knitr::opts_knit$set(rmarkdown.pandoc.to = "latex")
  txt <- utils::capture.output(res <- knitr::knit_print(cb))
  expect_match(paste(c(txt, as.character(res)), collapse = "\n"),
               "Codebook: survey_data", fixed = TRUE)
})

test_that("EDGE-21 a knitted chunk shows the codebook without opening a viewer", {
  # Was: the chunk output was empty.
  skip_if_not_installed("knitr")
  skip_if_not_installed("withr")
  rmd <- tempfile(fileext = ".Rmd")
  md <- tempfile(fileext = ".md")
  on.exit(unlink(c(rmd, md)))
  writeLines(c("```{r}", "codebook(survey_data, age, gender)", "```"), rmd)
  viewed <- FALSE
  withr::local_options(viewer = function(...) viewed <<- TRUE)
  suppressMessages(knitr::knit(rmd, output = md, quiet = TRUE,
                               envir = environment()))
  out <- paste(readLines(md), collapse = "\n")
  expect_match(out, "Age in years", fixed = TRUE)
  expect_match(out, "<table", fixed = TRUE)
  expect_false(viewed)
})
