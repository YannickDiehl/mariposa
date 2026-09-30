# =============================================================================
# Practice-test regressions: scale analysis (reliability(), efa())
# =============================================================================
# Each test pins a defect found in the 2026-09 practice test (ALLBUS 2023
# field report, batch SCALE) to a minimal scenario on survey_data or small
# synthetic data. R-internal consistency checks (Tier 4); the SPSS parity
# of the EFA sign convention is asserted in test-efa-spss-validation.R.
# =============================================================================

library(testthat)
library(dplyr)

data(survey_data, envir = environment())

# --- SCALE-01: component/factor signs ----------------------------------------

test_that("SCALE-01: efa() reflects components to a positive loading sum", {
  # eigen() returns eigenvectors with an arbitrary sign: three positively
  # correlated trust items came out with all-negative loadings
  # (-0.597/-0.475/-0.678). SPSS reflects every extracted column so that
  # its loadings sum to a positive value.
  e <- efa(survey_data, trust_government, trust_media, trust_science,
           n_factors = 1)
  expect_true(all(e$unrotated_loadings > 0))
  expect_true(all(e$loadings > 0))

  for (rot in c("none", "varimax", "promax")) {
    e <- efa(survey_data, political_orientation, environmental_concern,
             life_satisfaction, trust_government, trust_media, trust_science,
             rotation = rot)
    expect_true(all(colSums(e$unrotated_loadings) > 0), label = rot)
  }
})

test_that("SCALE-01: oblique solutions stay consistent after reflection", {
  e <- efa(survey_data, political_orientation, environmental_concern,
           life_satisfaction, trust_government, trust_media, trust_science,
           rotation = "promax")
  expect_true(all(colSums(e$unrotated_loadings) > 0))
  # Structure = Pattern %*% Phi must still hold for the reflected solution
  expect_equal(unname(e$structure_matrix),
               unname(e$pattern_matrix %*% e$factor_correlations),
               tolerance = 1e-10)
})

test_that("SCALE-01: every group of a grouped efa() uses the same rule", {
  g <- efa(group_by(survey_data, region), trust_government, trust_media,
           trust_science, n_factors = 1)
  for (res in g$groups) {
    expect_true(all(colSums(res$unrotated_loadings) > 0))
  }
})

# --- SCALE-02: ML variance explained -----------------------------------------

test_that("SCALE-02: efa() reports extraction sums of squared loadings", {
  # extraction = "ml" printed the PCA eigenvalue share ("61.0 %") although
  # the ML factors explain ~24 %; SPSS's "Extraction Sums of Squared
  # Loadings" were missing entirely, and the compact line said "components".
  e <- efa(survey_data, political_orientation, environmental_concern,
           life_satisfaction, trust_government, trust_media, trust_science,
           extraction = "ml")
  ss <- unname(colSums(e$unrotated_loadings^2))
  expect_equal(e$extraction_variance$ss_loading, ss)
  expect_equal(e$extraction_variance$prc_variance, ss / 6 * 100)
  expect_equal(e$extraction_variance$cumulative_prc, cumsum(ss / 6 * 100))

  out <- capture.output(print(e))
  pct <- format(round(sum(ss) / 6 * 100, 1), nsmall = 1)
  expect_true(any(grepl(paste0(pct, "%"), out, fixed = TRUE)))
  expect_false(any(grepl("61.0%", out, fixed = TRUE)))
  expect_true(any(grepl("3 factors (ML", out, fixed = TRUE)))
  expect_false(any(grepl("component", out)))

  s <- capture.output(print(summary(e)))
  expect_true(any(grepl("Extraction Sums", s, fixed = TRUE)))
  expect_true(any(grepl("Rotation Sums", s, fixed = TRUE)))
})

test_that("SCALE-02: PCA extraction sums equal the retained eigenvalues", {
  e <- efa(survey_data, political_orientation, environmental_concern,
           life_satisfaction, trust_government, trust_media, trust_science)
  expect_equal(e$extraction_variance$ss_loading, e$eigenvalues[1:3])
  out <- capture.output(print(e))
  expect_true(any(grepl("3 components (PCA", out, fixed = TRUE)))
  expect_true(any(grepl("61.0%", out, fixed = TRUE)))
})

test_that("SCALE-17: Total Variance Explained is an aligned table", {
  # The old free-text lines shifted their columns at PC10 and listed
  # Factor1..Factor15 for 3 extracted factors.
  set.seed(1)
  f <- matrix(rnorm(300 * 2), 300)
  items <- as.data.frame(f %*% matrix(runif(2 * 11, 0.3, 0.8), 2) +
                           matrix(rnorm(300 * 11), 300))
  e <- efa(items, everything(), n_factors = 2)
  s <- capture.output(print(summary(e, kmo_bartlett = FALSE,
                                    communalities = FALSE,
                                    unrotated_matrix = FALSE,
                                    rotated_matrix = FALSE)))
  start <- grep("Total Variance Explained", s, fixed = TRUE)
  rows <- grep("^ +[0-9]+ ", s[start:length(s)], value = TRUE)
  expect_length(rows, 11)
  # The first number of every row (the eigenvalue) ends in the same
  # column, including rows 10 and 11
  first_num_end <- vapply(rows, function(r) {
    m <- gregexpr("[0-9]+\\.[0-9]+", r)[[1]]
    as.integer(m[1] + attr(m, "match.length")[1])
  }, integer(1))
  expect_length(unique(first_num_end), 1)
})

# --- SCALE-03: undefined correlations ----------------------------------------

# Collect every condition message (the German base-R texts must not appear)
conditions_of <- function(expr) {
  msgs <- character(0)
  res <- withCallingHandlers(
    tryCatch(expr, error = function(e) {
      msgs <<- c(msgs, paste("ERROR:", conditionMessage(e)))
      NULL
    }),
    warning = function(w) {
      msgs <<- c(msgs, paste("WARNING:", conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )
  list(result = res, msgs = msgs)
}

test_that("SCALE-03: a constant item gives a mariposa error naming it", {
  # Crashed with the base error "unendliche oder fehlende Werte in 'x'"
  # (plus "Standardabweichung ist Null").
  d <- survey_data
  d$const <- 3L
  cnd <- conditions_of(efa(d, trust_government, trust_media, trust_science,
                           const))
  expect_null(cnd$result)
  expect_length(cnd$msgs, 1)
  expect_match(cnd$msgs, "^ERROR:")
  expect_match(cnd$msgs, "const", fixed = TRUE)
  expect_match(cnd$msgs, "constant", fixed = TRUE)
  expect_false(any(grepl("fehlende|Standardabweichung|infinite or missing",
                         cnd$msgs)))
})

test_that("SCALE-03: all-NA items and disjoint pairs are named", {
  d <- survey_data
  d$allna <- NA_real_
  cnd <- conditions_of(efa(d, trust_government, trust_media, allna))
  expect_match(cnd$msgs, "allna", fixed = TRUE)
  expect_match(cnd$msgs, "no valid values", fixed = TRUE)

  d$a <- ifelse(seq_len(nrow(d)) <= 1250, d$trust_government, NA)
  d$b <- ifelse(seq_len(nrow(d)) > 1250, d$trust_media, NA)
  cnd <- conditions_of(efa(d, a, b, trust_science, life_satisfaction))
  expect_length(cnd$msgs, 1)
  expect_match(cnd$msgs, "`a` and `b`", fixed = TRUE)

  cnd <- conditions_of(efa(d, a, b, trust_science, use = "complete"))
  expect_length(cnd$msgs, 1)
  expect_match(cnd$msgs, "complete case", fixed = TRUE)
})

test_that("SCALE-03: a grouped efa() skips an undefined group with a warning", {
  # An item constant in one region (or a 1-case group) aborted the whole
  # grouped analysis, losing the results of every other group.
  d <- survey_data
  d$cg <- ifelse(d$region == "East", 3, d$trust_government)
  cnd <- conditions_of(efa(group_by(d, region), cg, trust_media,
                           trust_science, life_satisfaction))
  expect_s3_class(cnd$result, "efa")
  expect_length(cnd$msgs, 1)
  expect_match(cnd$msgs, "^WARNING:")
  expect_match(cnd$msgs, "region = East", fixed = TRUE)
  expect_match(cnd$msgs, "cg", fixed = TRUE)
  east <- cnd$result$groups[[1]]
  west <- cnd$result$groups[[2]]
  expect_false(is.null(east$not_computed))
  expect_true(is.null(west$not_computed))
  expect_true(is.finite(west$kmo$overall))

  out <- c(capture.output(print(cnd$result)),
           capture.output(print(summary(cnd$result))))
  expect_true(any(grepl("not computed", out, fixed = TRUE)))
  expect_false(any(grepl("NA", out, fixed = TRUE)))
})

# --- SCALE-04: singular correlation matrices ---------------------------------

test_that("SCALE-04: a singular matrix warns and leaves KMO/Bartlett empty", {
  # A duplicated item gave KMO 0.500 (pseudo-inverse), Bartlett "Inf" and
  # "Sig.: 0.000" without any hint.
  d <- survey_data
  d$dup <- d$trust_government
  cnd <- conditions_of(efa(d, trust_government, trust_media, trust_science,
                           dup))
  e <- cnd$result
  expect_s3_class(e, "efa")
  expect_true(any(grepl("not positive definite", cnd$msgs, fixed = TRUE)))
  expect_true(any(grepl("`trust_government` and `dup`", cnd$msgs,
                        fixed = TRUE)))
  expect_true(is.na(e$kmo$overall))
  expect_true(is.na(e$bartlett$chi_sq))
  expect_true(is.na(e$bartlett$p_value))
  out <- c(capture.output(print(e)), capture.output(print(summary(e))))
  expect_true(any(grepl("not computed", out, fixed = TRUE)))
  expect_false(any(grepl("Inf|NaN|0\\.500", out)))
})

test_that("SCALE-04: ML on a singular matrix gives a clear error", {
  d <- survey_data
  d$dup <- d$trust_government
  cnd <- conditions_of(efa(d, trust_government, trust_media, trust_science,
                           life_satisfaction, dup, extraction = "ml",
                           n_factors = 1))
  expect_null(cnd$result)
  err <- grep("^ERROR", cnd$msgs, value = TRUE)
  expect_length(err, 1)
  expect_match(err, "positive definite", fixed = TRUE)
  expect_false(any(grepl("singul|Lapack", cnd$msgs)))
})

test_that("SCALE-04: fewer cases than variables are flagged", {
  # efa(survey_data[1:5, ], 6 items) printed KMO NaN / 0.000 and 83.8 %
  # variance explained without any warning.
  cnd <- conditions_of(efa(survey_data[1:5, ], political_orientation,
                           environmental_concern, life_satisfaction,
                           trust_government, trust_media, trust_science))
  expect_s3_class(cnd$result, "efa")
  # (4 = the smallest pairwise N of the first five rows)
  expect_true(any(grepl("4 cases for 6 variables", cnd$msgs, fixed = TRUE)))
  expect_false(any(grepl("NaN|erzeugt|produced", cnd$msgs)))
  expect_true(is.na(cnd$result$kmo$overall))
  expect_false(anyNA(cnd$result$unrotated_loadings))
})

# --- SCALE-13: sample size -----------------------------------------------------

test_that("SCALE-13: efa() shows N in print() and summary()", {
  # e$n existed but was never printed; with pairwise deletion the smallest
  # pairwise N was invisible.
  e <- efa(survey_data, political_orientation, environmental_concern,
           life_satisfaction, trust_government, trust_media, trust_science)
  out <- capture.output(print(e))
  expect_true(any(grepl(sprintf("N = %d (smallest pairwise)", e$n), out,
                        fixed = TRUE)))
  s <- capture.output(print(summary(e)))
  expect_true(any(grepl(sprintf("N (smallest pairwise): %d", e$n), s,
                        fixed = TRUE)))
  expect_true(any(grepl("Descriptive Statistics", s, fixed = TRUE)))
  expect_true(any(grepl("Analysis N", s, fixed = TRUE)))
})

test_that("SCALE-13: use = 'complete' reports the listwise N per item", {
  # item_statistics$analysis_n stayed pairwise with use = "complete".
  e <- efa(survey_data, political_orientation, environmental_concern,
           life_satisfaction, trust_government, trust_media, trust_science,
           use = "complete")
  cc <- stats::complete.cases(survey_data[, e$variables])
  expect_equal(e$n, sum(cc))
  expect_true(all(e$item_statistics$analysis_n == sum(cc)))
  expect_true(all(e$item_statistics$missing_n == sum(!cc)))
  expect_equal(e$item_statistics$mean[1],
               mean(survey_data$political_orientation[cc]))
  out <- capture.output(print(e))
  expect_true(any(grepl(sprintf("N = %d (listwise)", sum(cc)), out,
                        fixed = TRUE)))
})

# --- SCALE-17: efa() input handling and output details ------------------------

test_that("SCALE-17: fractional n_factors is an error, 'listwise' an alias", {
  expect_error(
    efa(survey_data, trust_government, trust_media, trust_science,
        n_factors = 2.7),
    "whole number"
  )
  a <- efa(survey_data, trust_government, trust_media, trust_science,
           use = "listwise")
  b <- efa(survey_data, trust_government, trust_media, trust_science,
           use = "complete")
  expect_equal(a$loadings, b$loadings)
  expect_equal(a$n, b$n)
  # A typo gives an English error listing the choices (was a translated
  # match.arg() error)
  expect_error(
    efa(survey_data, trust_government, trust_media, trust_science,
        use = "pairwize"),
    "must be one of"
  )
})

test_that("SCALE-17: a one-factor solution says why it is not rotated", {
  # rotation = "varimax" was silently turned into "Unrotated".
  e <- efa(survey_data, trust_government, trust_media, trust_science,
           n_factors = 1)
  out <- c(capture.output(print(e)), capture.output(print(summary(e))))
  expect_true(any(grepl("Only one component was extracted", out,
                        fixed = TRUE)))
  expect_true(any(grepl("cannot be rotated", out, fixed = TRUE)))
})

test_that("SCALE-17: communalities print with fixed decimals", {
  # Initial communalities printed as "1" next to "0.457".
  e <- efa(survey_data, political_orientation, environmental_concern,
           life_satisfaction, trust_government, trust_media, trust_science)
  s <- capture.output(print(summary(e, kmo_bartlett = FALSE,
                                    variance_explained = FALSE,
                                    unrotated_matrix = FALSE,
                                    rotated_matrix = FALSE)))
  row <- grep("political_orientation", s, value = TRUE)
  row <- row[grepl("0\\.786", row)]
  expect_length(row, 1)
  expect_match(row, "1.000", fixed = TRUE)
  expect_true(any(grepl("N of Components: 3", s, fixed = TRUE)))
})

# --- SCALE-07: reliability(na.rm = FALSE) ---------------------------------------

test_that("SCALE-07: na.rm = FALSE with missing values gives NA, not a crash", {
  # Crashed with "Fehlender Wert, wo TRUE/FALSE noetig ist" (after a
  # German factanal warning) as soon as any value was missing.
  cnd <- conditions_of(reliability(survey_data, trust_government, trust_media,
                                   trust_science, na.rm = FALSE))
  r <- cnd$result
  expect_s3_class(r, "reliability")
  expect_true(is.na(r$alpha))
  expect_true(is.na(r$omega))
  expect_length(cnd$msgs, 1)
  expect_match(cnd$msgs, "^WARNING:")
  expect_match(cnd$msgs, "na.rm = FALSE", fixed = TRUE)
  expect_match(cnd$msgs, "trust_government", fixed = TRUE)
  out <- c(capture.output(print(r)), capture.output(print(summary(r))))
  expect_true(any(grepl("not computed", out, fixed = TRUE)))
  expect_false(any(grepl("NA", out, fixed = TRUE)))

  # Without missing values na.rm = FALSE computes as usual
  cc <- survey_data[stats::complete.cases(
    survey_data[, c("trust_government", "trust_media", "trust_science")]), ]
  a <- reliability(cc, trust_government, trust_media, trust_science,
                   na.rm = FALSE)
  b <- reliability(cc, trust_government, trust_media, trust_science)
  expect_equal(a$alpha, b$alpha)
})

# --- SCALE-18: reliability() warnings -------------------------------------------

german <- "Standardabweichung|NaNs wurden|erzeugt|nicht-fehlendes|factanal reported|singulär|reziproke|optim"

test_that("SCALE-18: a constant item is removed with a warning naming it", {
  # alpha 0.042 included the constant item without any message, next to
  # standardized alpha NA and German base warnings. SPSS removes
  # zero-variance items from the scale with a warning.
  d <- survey_data
  d$const <- 3
  cnd <- conditions_of(reliability(d, trust_government, trust_media,
                                   trust_science, const))
  r <- cnd$result
  expect_length(cnd$msgs, 1)
  expect_match(cnd$msgs, "zero variance", fixed = TRUE)
  expect_match(cnd$msgs, "const", fixed = TRUE)
  expect_false(any(grepl(german, cnd$msgs)))
  ref <- reliability(d, trust_government, trust_media, trust_science)
  expect_equal(r$alpha, ref$alpha)
  expect_equal(r$alpha_standardized, ref$alpha_standardized)
  expect_equal(r$omega, ref$omega)
  expect_equal(r$removed_items, "const")
  out <- capture.output(print(summary(r)))
  expect_true(any(grepl("zero variance", out, fixed = TRUE)))
})

test_that("SCALE-18: a singular item set gives an English omega warning", {
  # A duplicated item leaked "System ist fuer den Rechner singulaer" and
  # "NaNs wurden erzeugt" from factanal.
  d <- survey_data
  d$dup <- d$trust_government
  cnd <- conditions_of(reliability(d, trust_government, trust_media,
                                   trust_science, dup))
  expect_length(cnd$msgs, 1)
  expect_match(cnd$msgs, "omega", ignore.case = TRUE)
  expect_match(cnd$msgs, "`trust_government` and `dup`", fixed = TRUE)
  expect_false(any(grepl(german, cnd$msgs)))
  expect_true(is.finite(cnd$result$alpha))
  expect_true(is.na(cnd$result$omega))
})

test_that("SCALE-18: the k = 2 omega warning appears once per call", {
  g <- group_by(survey_data, education)
  cnd <- conditions_of(reliability(g, trust_government, trust_media))
  expect_length(cnd$msgs, 1)
  expect_match(cnd$msgs, "at least 3 items", fixed = TRUE)
})

test_that("SCALE-18: too few cases name the group and the empty item", {
  # "Insufficient data for reliability analysis (n = 0)." named neither.
  d <- survey_data
  d$allna <- NA_real_
  cnd <- conditions_of(reliability(group_by(d, region), trust_government,
                                   trust_media, allna))
  expect_length(cnd$msgs, 2)
  expect_true(all(grepl("allna", cnd$msgs, fixed = TRUE)))
  expect_true(any(grepl("region = East", cnd$msgs, fixed = TRUE)))
  expect_true(any(grepl("region = West", cnd$msgs, fixed = TRUE)))
})
