# =============================================================================
# _build-spss-compatibility.R — COMPATIBILITY VIGNETTE GENERATOR
# =============================================================================
# Generates vignettes/spss-compatibility.Rmd from an analysis of the test
# suite and the exception registry. Run by the maintainer before each release.
#
# Charter reference: .claude/VALIDATION_CHARTER.md §10
#
# What it produces:
#   vignettes/spss-compatibility.Rmd
#
# Sources of information:
#   - tests/testthat/test-*-spss-validation.R   (which functions are validated)
#   - tests/testthat/helper-validation-tolerances.R::exception_registry
#   - .claude/VALIDATION_EXCEPTIONS.md
#   - NAMESPACE (list of exported functions)
#   - Charter coverage policy (§9)
#
# Usage:
#   Rscript vignettes/_build-spss-compatibility.R
# =============================================================================

# ---- Helpers --------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

find_repo_root <- function(start = ".") {
  d <- normalizePath(start, winslash = "/", mustWork = TRUE)
  for (i in 1:8L) {
    if (file.exists(file.path(d, "DESCRIPTION"))) {
      return(normalizePath(d, winslash = "/", mustWork = TRUE))
    }
    parent <- normalizePath(file.path(d, ".."), winslash = "/", mustWork = FALSE)
    if (identical(parent, d)) break
    d <- parent
  }
  stop("Could not locate repo root from: ", start)
}


# ---- Paths ----------------------------------------------------------------

repo_root <- find_repo_root()
test_dir  <- file.path(repo_root, "tests", "testthat")
out_path  <- file.path(repo_root, "vignettes", "spss-compatibility.Rmd")

cat(sprintf("[build-compat] repo root: %s\n", repo_root))
cat(sprintf("[build-compat] output:    %s\n", out_path))


# ---- Inputs ---------------------------------------------------------------

# 1. Exported functions from NAMESPACE
namespace_path <- file.path(repo_root, "NAMESPACE")
exports <- character()
if (file.exists(namespace_path)) {
  ns <- readLines(namespace_path, warn = FALSE)
  # Non-syntactic names are quoted: export("%>%"), export("var_label<-")
  exports <- sub('^export\\("?([^")]+)"?\\).*$', "\\1",
                 grep("^export\\(", ns, value = TRUE))
}

# 2. Functions with SPSS-validation test files
validation_files <- list.files(test_dir,
                               pattern = "^test-.*-spss-validation\\.R$",
                               full.names = TRUE)
validated_fns <- sub("^test-(.*)-spss-validation\\.R$", "\\1",
                     basename(validation_files))
# Convert dash to underscore (e.g., test-t-test → t_test, friedman-test → friedman_test)
validated_fns <- gsub("-", "_", validated_fns)
# Handle filename-to-function aliases
validated_fns[validated_fns == "chi_squared"] <- "chi_square"
validated_fns[validated_fns == "scheffe"]     <- "scheffe_test"
# weighted_statistics test covers all 11 w_* functions; expand for coverage table
w_funcs <- c("w_mean", "w_median", "w_sd", "w_var", "w_se", "w_iqr",
             "w_quantile", "w_range", "w_skew", "w_kurtosis", "w_modus")
if ("weighted_statistics" %in% validated_fns) {
  validated_fns <- c(validated_fns, w_funcs)
}

# Weighted variants that are R-only (Charter §4, Tier 4): SPSS NPAR TESTS and
# NONPAR CORR ignore WEIGHT BY, so no SPSS reference exists for the weighted
# paths of the rank-based family. Their weighted scenarios are covered by
# internal regression tests and the weights == 1 invariance suite
# (tests/testthat/test-weights-invariance.R), not by SPSS comparison.
tier4_weighted <- c("mann_whitney", "kruskal_wallis", "wilcoxon_test",
                    "friedman_test", "binomial_test", "dunn_test",
                    "pairwise_wilcoxon", "kendall_tau")

# R-only statistics inside otherwise SPSS-validated functions (Charter §4,
# Tier 4), named per function. reliability()'s McDonald's omega (0.6.13) is
# a one-factor ML solution; IBM does not publicly document the SPSS omega
# algorithm and no SPSS v29 reference run exists yet
# (.claude/spss-syntax-0.7.4-references.sps), so omega/omega_std/
# omega_if_deleted are Internal (Tier 4) in both the weighted and the
# unweighted path. Remove the entry once the reference run lands.
# logistic_regression has no SPSS v29 reference output at all: its
# validation file asserts textbook formulas (IBM SPSS Statistics
# Algorithms) as the oracle, so every statistic is Tier 4 until an SPSS
# LOGISTIC REGRESSION reference run lands.
tier4_statistics <- c(
  reliability = "McDonald's omega",
  logistic_regression = "all results (checked against the formulas of the SPSS algorithm)",
  crosstab = "adjusted standardized residuals",
  normality_test = "all results (checked against independent implementations)",
  partial_cor = "all results (checked against an equivalent regression computation)",
  multiple_response = "all results (checked by direct computation)"
)

# 3. Exception registry
helper_path <- file.path(test_dir, "helper-validation-tolerances.R")
exception_registry <- list()
if (file.exists(helper_path)) {
  env <- new.env()
  tryCatch(source(helper_path, local = env), error = function(e) {
    cat(sprintf("[build-compat] warning: could not source helper: %s\n",
                conditionMessage(e)))
  })
  if (exists("exception_registry", envir = env, inherits = FALSE)) {
    exception_registry <- env$exception_registry
  }
}


# ---- Assertion counting per file ------------------------------------------
# Count assert_spss() calls per test file, broken down by tier.
# This gives a rough validation "depth" per function.

count_assertions <- function(path) {
  if (!file.exists(path)) return(c(spec = 0L, display = 0L, exception = 0L,
                                   total = 0L, legacy = 0L))
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  # Strip comments (anything from # to end-of-line)
  text <- gsub("#[^\n]*", "", text)

  # gregexpr() signals "no match" as a single -1, so length() alone would
  # report 1 for zero occurrences; guard on the first element instead.
  count_matches <- function(pattern) {
    m <- gregexpr(pattern, text)[[1]]
    if (m[1] == -1L) 0L else length(m)
  }
  # assert_spss_count() is the spec-tier wrapper for exact integer counts;
  # its call sites carry no explicit tier= argument, so count it separately.
  n_count <- count_matches("\\bassert_spss_count\\s*\\(")
  total   <- count_matches("\\bassert_spss\\s*\\(") + n_count
  spec    <- count_matches("tier\\s*=\\s*[\"']spec[\"']") + n_count
  display <- count_matches("tier\\s*=\\s*[\"']display[\"']")
  except  <- count_matches("tier\\s*=\\s*[\"']exception[\"']")

  # Legacy: count expect_equal calls with tolerance= within the same call
  # (line-by-line to avoid cross-call matches in collapsed text).
  legacy_eq <- 0L
  text_lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  for (line in text_lines) {
    if (grepl("expect_equal\\(", line) && grepl("tolerance\\s*=", line)) {
      legacy_eq <- legacy_eq + 1L
    }
  }

  c(spec = spec, display = display, exception = except, total = total,
    legacy = legacy_eq)
}

# Executed comparisons per tier: run each validation file with the helper's
# tally switched on (MARIPOSA_ASSERT_TALLY). The tests are data-driven, so a
# single assert_spss() call site can check hundreds of SPSS values; counting
# call sites understated the depth. Falls back to the static count when a
# file cannot be run.
run_tally <- function(path) {
  tally <- tempfile(fileext = ".txt")
  old_tally <- Sys.getenv("MARIPOSA_ASSERT_TALLY", unset = NA)
  old_cran <- Sys.getenv("NOT_CRAN", unset = NA)
  Sys.setenv(MARIPOSA_ASSERT_TALLY = tally, NOT_CRAN = "true")
  on.exit({
    if (is.na(old_tally)) Sys.unsetenv("MARIPOSA_ASSERT_TALLY") else Sys.setenv(MARIPOSA_ASSERT_TALLY = old_tally)
    if (is.na(old_cran)) Sys.unsetenv("NOT_CRAN") else Sys.setenv(NOT_CRAN = old_cran)
  })
  ok <- tryCatch({
    suppressMessages(suppressWarnings(
      testthat::test_file(path, reporter = testthat::SilentReporter$new())
    ))
    TRUE
  }, error = function(e) FALSE)
  if (!ok || !file.exists(tally)) return(NULL)
  tiers <- readLines(tally, warn = FALSE)
  c(spec = sum(tiers == "spec"), display = sum(tiers == "display"),
    exception = sum(tiers == "exception"), total = length(tiers))
}

pkg_loaded <- tryCatch({
  suppressMessages(pkgload::load_all(repo_root, quiet = TRUE))
  TRUE
}, error = function(e) FALSE)

per_file_stats <- lapply(validation_files, function(p) {
  counts <- count_assertions(p)
  runtime <- if (pkg_loaded) run_tally(p)
  if (!is.null(runtime)) counts[names(runtime)] <- runtime
  list(file = basename(p), fn = sub("-", "_", sub("^test-(.*)-spss-validation\\.R$",
                                                  "\\1", basename(p))),
       counts = counts)
})


# ---- Coverage classification (Charter §9) ---------------------------------
# Functions explicitly NOT in scope for SPSS validation:

not_in_scope <- c(
  # No SPSS equivalent exists (Charter Tier 4 by construction):
  # AMEs are a Stata margins concept; validated against analytic
  # formulas and finite-difference recomputation in
  # tests/testthat/test-marginal-effects.R
  "marginal_effects",
  # Label management
  "var_label", "var_label<-", "val_labels", "copy_labels", "drop_labels",
  "to_label", "to_character", "to_numeric", "to_labelled",
  "set_na", "unlabel",
  # Type/exploration
  "to_dummy", "find_var",
  # Tagged NA utilities
  "na_frequencies", "untag_na", "strip_tags", "fre",
  # I/O
  "read_spss", "read_por", "read_stata", "read_sas", "read_xpt", "read_xlsx",
  "write_spss", "write_stata", "write_xpt", "write_xlsx",
  # Re-exported from dplyr
  "%>%"
)

# Functions in SPSS-validation scope but missing a test file (gap)
in_scope_fns  <- setdiff(exports, not_in_scope)
validated_set <- intersect(in_scope_fns, validated_fns)
missing_set   <- setdiff(in_scope_fns, validated_fns)


# ---- Render the vignette --------------------------------------------------

render_table_row <- function(fn) {
  # Aliases for known filename-to-function mismatches
  aliases <- c(chi_square = "chi_squared", scheffe_test = "scheffe")
  fn_alias <- if (fn %in% names(aliases)) aliases[[fn]] else fn
  # The shared weighted-statistics file covers all w_* functions
  if (fn %in% w_funcs) fn_alias <- "weighted_statistics"

  notes <- character()
  if (fn %in% tier4_weighted) notes <- c(notes, "weighted results")
  if (fn %in% names(tier4_statistics)) notes <- c(notes, tier4_statistics[[fn]])
  notes_col <- if (length(notes)) paste(notes, collapse = "; ") else "—"

  matched <- per_file_stats[vapply(per_file_stats,
                                   function(x) x$fn == fn || x$fn == fn_alias,
                                   logical(1))]
  if (length(matched) == 0L) return(NULL)
  cs <- matched[[1]]$counts
  sprintf("| `%s()` | %s | %s | %s | %s | %s |",
          fn, fmt_count(cs["total"]), fmt_count(cs["spec"]),
          fmt_count(cs["display"]), fmt_count(cs["exception"]), notes_col)
}

fmt_count <- function(x) formatC(as.numeric(x), format = "d", big.mark = ",")

today <- format(Sys.Date(), "%Y-%m-%d")
pkg_version <- read.dcf(file.path(repo_root, "DESCRIPTION"), fields = "Version")[1, 1]
n_exceptions <- length(exception_registry)
n_validated  <- length(validated_set)
n_missing    <- length(missing_set)
n_in_scope   <- length(in_scope_fns)
# Each validation file once (the w_* functions share one file)
total_values <- sum(vapply(per_file_stats, function(x) as.numeric(x$counts["total"]),
                           numeric(1)))

rmd_lines <- c(
  "---",
  "title: \"SPSS Compatibility\"",
  "output: rmarkdown::html_vignette",
  "vignette: >",
  "  %\\VignetteIndexEntry{SPSS Compatibility}",
  "  %\\VignetteEngine{knitr::rmarkdown}",
  "  %\\VignetteEncoding{UTF-8}",
  "---",
  "",
  "```{r setup, include = FALSE}",
  "knitr::opts_chunk$set(",
  "  collapse = TRUE,",
  "  comment = \"#>\",",
  "  echo = FALSE",
  ")",
  "```",
  "",
  "The statistical functions of **mariposa** are checked against reference",
  "output of IBM SPSS Statistics 29. This article lists, for each function,",
  "how many SPSS values its tests compare and which results are not compared",
  "with SPSS.",
  "",
  sprintf("*mariposa %s, status of %s.*", pkg_version, today),
  "",
  "## Summary",
  "",
  sprintf("- **%d** of %d statistical functions are compared with SPSS output,",
          n_validated, n_in_scope),
  sprintf("  with **%s** SPSS values in total.", fmt_count(total_values)),
  sprintf("- **%d** documented algorithmic difference%s (see below).",
          n_exceptions, if (n_exceptions == 1L) "" else "s"),
  "",
  "## How results are compared",
  "",
  "Each comparison uses the precision with which SPSS prints the value:",
  "",
  "- **Exact**: counts, degrees of freedom, and p-values that SPSS prints as",
  "  `.000` (p < .001).",
  "- **Printed precision**: all other values agree with SPSS to within half a",
  "  unit of the last decimal SPSS prints (one unit for p-values).",
  "- **Documented difference**: where SPSS and mariposa use different",
  "  algorithms by design, the values agree within the tolerance listed under",
  "  \"Documented differences\".",
  "",
  "The reference values come from SPSS syntax runs on the `survey_data`",
  "example data. The comparisons run with the package's test suite on every",
  "change; the SPSS syntax and output are part of the source repository",
  "([tests/spss_reference](https://github.com/YannickDiehl/mariposa/tree/main/tests/spss_reference)).",
  "",
  "## Results by function",
  "",
  "The `w_*()` functions share one set of SPSS references; each row shows",
  "that total. The last column lists results that are not compared with",
  "SPSS output. They are verified by other means: the weighted results of",
  "the rank-based tests reproduce the unweighted results exactly when all",
  "weights equal 1 (the weighted Mann-Whitney test also agrees with",
  "`survey::svyranktest()`), and the remaining results are checked against",
  "published formulas or independent implementations.",
  "",
  "| Function | SPSS values | Exact | Printed precision | Documented difference | Not compared with SPSS |",
  "|---|---:|---:|---:|---:|---|"
)

for (fn in sort(validated_set)) {
  rmd_lines <- c(rmd_lines, render_table_row(fn))
}

if (n_exceptions > 0L) {
  rmd_lines <- c(rmd_lines, "",
                 "## Documented differences",
                 "",
                 "| Function | Results | Tolerance | Reason |",
                 "|---|---|---:|---|")
  for (id in sort(names(exception_registry))) {
    e <- exception_registry[[id]]
    rmd_lines <- c(rmd_lines,
                   sprintf("| `%s()` | %s | %g | %s |",
                           e$function_name, e$statistic, e$tolerance,
                           e$reason %||% "—"))
  }
  rmd_lines <- c(rmd_lines, "",
                 "A documented difference describes a known difference between",
                 "the algorithms, not an error; its tolerance is the agreement the",
                 "two algorithms reach.",
                 "")
}

if (n_missing > 0L) {
  groups <- list(
    "Computed by `chi_square()`, whose results are compared with SPSS" =
      c("phi", "cramers_v", "goodman_gamma"),
    "Data transformations, covered by unit tests" =
      c("center", "std", "rec", "row_count", "row_means", "row_sums", "pomps"),
    "Data documentation without an SPSS counterpart" = "codebook"
  )
  rmd_lines <- c(rmd_lines,
                 "## Functions without their own SPSS comparison",
                 "")
  listed <- character()
  for (label in names(groups)) {
    fns <- intersect(groups[[label]], missing_set)
    if (length(fns)) {
      rmd_lines <- c(rmd_lines, sprintf("- %s: %s", label,
                                        paste0("`", sort(fns), "()`", collapse = ", ")))
      listed <- c(listed, fns)
    }
  }
  rest <- setdiff(missing_set, listed)
  if (length(rest)) {
    rmd_lines <- c(rmd_lines, sprintf("- Covered by unit tests: %s",
                                      paste0("`", sort(rest), "()`", collapse = ", ")))
  }
  rmd_lines <- c(rmd_lines, "")
}

writeLines(rmd_lines, out_path)

cat(sprintf("\n[build-compat] wrote %d lines to %s\n",
            length(rmd_lines), out_path))
cat(sprintf("[build-compat] in-scope=%d, validated=%d, missing=%d, exceptions=%d\n",
            n_in_scope, n_validated, n_missing, n_exceptions))
