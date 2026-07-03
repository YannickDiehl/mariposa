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
  exports <- sub("^export\\(([A-Za-z_][A-Za-z0-9_.]*)\\).*$", "\\1",
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
# (.claude/spss-syntax-omega-references.sps), so omega/omega_std/
# omega_if_deleted are Internal (Tier 4) in both the weighted and the
# unweighted path. Remove the entry once the reference run lands.
tier4_statistics <- c(reliability = "McDonald's omega (all paths)")

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

per_file_stats <- lapply(validation_files, function(p) {
  list(file = basename(p), fn = sub("-", "_", sub("^test-(.*)-spss-validation\\.R$",
                                                  "\\1", basename(p))),
       counts = count_assertions(p))
})


# ---- Coverage classification (Charter §9) ---------------------------------
# Functions explicitly NOT in scope for SPSS validation:

not_in_scope <- c(
  # Label management
  "var_label", "val_labels", "copy_labels", "drop_labels",
  "to_label", "to_character", "to_numeric", "to_labelled",
  "set_na", "unlabel",
  # Type/exploration
  "to_dummy", "find_var",
  # Tagged NA utilities
  "na_frequencies", "untag_na", "strip_tags", "fre",
  # I/O
  "read_spss", "read_por", "read_stata", "read_sas", "read_xpt", "read_xlsx",
  "write_spss", "write_stata", "write_xpt", "write_xlsx"
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

  tier4_notes <- character()
  if (fn %in% tier4_weighted) tier4_notes <- c(tier4_notes, "weighted variant")
  if (fn %in% names(tier4_statistics)) {
    tier4_notes <- c(tier4_notes, tier4_statistics[[fn]])
  }
  tier4_col <- if (length(tier4_notes)) {
    paste(tier4_notes, collapse = "; ")
  } else {
    "—"
  }

  matched <- per_file_stats[vapply(per_file_stats,
                                   function(x) x$fn == fn || x$fn == fn_alias,
                                   logical(1))]
  if (length(matched) == 0L) {
    return(sprintf("| `%s` | not validated | — | — | — | — | %s |",
                   fn, tier4_col))
  }
  cs <- matched[[1]]$counts
  status <- if (cs["legacy"] > 0L) "legacy (migration needed)" else "compliant"
  sprintf("| `%s` | %s | %d | %d | %d | %d | %s |",
          fn, status, cs["spec"], cs["display"], cs["exception"], cs["total"],
          tier4_col)
}

today <- format(Sys.Date(), "%Y-%m-%d")
n_exceptions <- length(exception_registry)
n_validated  <- length(validated_set)
n_missing    <- length(missing_set)
n_in_scope   <- length(in_scope_fns)

rmd_lines <- c(
  "---",
  "title: \"SPSS Compatibility Status\"",
  "output: rmarkdown::html_vignette",
  "vignette: >",
  "  %\\VignetteIndexEntry{SPSS Compatibility Status}",
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
  "This vignette reports the SPSS-compatibility status of every statistical",
  "function in **mariposa**. It is auto-generated from the test suite and the",
  "validation exception registry.",
  "",
  sprintf("**Generated:** %s", today),
  "",
  "## Summary",
  "",
  sprintf("- Functions in SPSS-validation scope: **%d**", n_in_scope),
  sprintf("- With validation tests in place:      **%d**", n_validated),
  sprintf("- Validation gaps (no test file yet):  **%d**", n_missing),
  sprintf("- Active Tier-3 algorithmic exceptions: **%d**", n_exceptions),
  "",
  "## Tier Definitions",
  "",
  "Every numerical comparison between an R-side value and an SPSS reference",
  "falls into one of four tiers (Charter §4):",
  "",
  "- **Spec** — exact integers and SPSS-truncated sentinels (exact match)",
  "- **Display** — SPSS rounds for print; tolerance is half a unit of the last printed decimal",
  "- **Exception** — documented algorithmic difference; see `VALIDATION_EXCEPTIONS.md`",
  "- **Internal** — statistic has no SPSS equivalent; verified via snapshot tests only",
  "",
  "## Per-Function Status",
  "",
  "The columns show how many `assert_spss()` calls per tier the validation file",
  "contains. \"Total\" is the total number of charter-compliant assertions.",
  "",
  "The \"Internal (Tier 4)\" column flags statistics that have no SPSS",
  "reference and are therefore R-only: for the rank-based family the",
  "*weighted variant* (SPSS `NPAR TESTS` and `NONPAR CORR` ignore",
  "`WEIGHT BY`); for `reliability`, McDonald's omega in all paths (IBM does",
  "not publicly document the SPSS omega algorithm; an SPSS v29 reference run",
  "is pending). Tier-4 statistics are covered by internal regression and",
  "cross-check tests plus a weights-equal-1 invariance suite instead of SPSS",
  "references. (`mann_whitney`'s weighted variant is a design-based rank",
  "test additionally validated against `survey::svyranktest()`.) All other",
  "statistics of these functions are SPSS-validated as shown in the tier",
  "columns.",
  "",
  "| Function | Status | Spec | Display | Exception | Total | Internal (Tier 4) |",
  "|---|---|---:|---:|---:|---:|---|"
)

for (fn in sort(in_scope_fns)) {
  rmd_lines <- c(rmd_lines, render_table_row(fn))
}

rmd_lines <- c(rmd_lines, "",
               "## Active Exceptions",
               "")

if (n_exceptions == 0L) {
  rmd_lines <- c(rmd_lines,
                 "No active Tier-3 exceptions. All validated statistics agree with",
                 "SPSS within Spec or Display tolerances.",
                 "")
} else {
  rmd_lines <- c(rmd_lines,
                 "| ID | Function | Statistic | Tolerance |",
                 "|---|---|---|---:|")
  for (id in sort(names(exception_registry))) {
    e <- exception_registry[[id]]
    rmd_lines <- c(rmd_lines,
                   sprintf("| %s | `%s` | %s | %g |",
                           id, e$function_name, e$statistic, e$tolerance))
  }
  rmd_lines <- c(rmd_lines, "",
                 "Full provenance for each exception lives in",
                 "[VALIDATION_EXCEPTIONS.md](https://github.com/YannickDiehl/mariposa/blob/main/.claude/VALIDATION_EXCEPTIONS.md).",
                 "")
}

if (n_missing > 0L) {
  rmd_lines <- c(rmd_lines,
                 "## Validation Gaps",
                 "",
                 "The following functions are in SPSS-validation scope (per Charter §9)",
                 "but do not yet have a `test-<fn>-spss-validation.R` file:",
                 "")
  for (fn in sort(missing_set)) {
    rmd_lines <- c(rmd_lines, sprintf("- `%s`", fn))
  }
  rmd_lines <- c(rmd_lines, "")
}

rmd_lines <- c(rmd_lines,
               "## How to Read \"SPSS-Compatible\"",
               "",
               "A function is **SPSS-compatible** when:",
               "",
               "1. It has a validation test file (column \"Status: compliant\")",
               "2. Its Legacy column is zero",
               "3. Every value it asserts falls into Spec, Display, or a registered",
               "   Exception",
               "4. It carries no `NA` placeholders in its `spss_values` block",
               "",
               "Functions with non-zero Legacy counts are mid-migration and not yet",
               "Charter-compliant; their assertions may pass but do not yet enforce",
               "the documented tolerance policy.",
               "",
               "## Methodology",
               "",
               "All validation runs SPSS v29 syntax scripts in",
               "`tests/spss_reference/syntax/` against `tests/spss_reference/data/`,",
               "saves the output to `tests/spss_reference/outputs/`, and asserts the",
               "R-side computation matches via `assert_spss()` from the",
               "validation helpers in `tests/testthat/`.",
               "Inline cached values in test files cite their source line via",
               "trailing `# <output_file>:<line>` comments; the dev-only script",
               "`tests/spss_reference/verify_inline.R` audits this citation chain",
               "before every release.",
               "")

writeLines(rmd_lines, out_path)

cat(sprintf("\n[build-compat] wrote %d lines to %s\n",
            length(rmd_lines), out_path))
cat(sprintf("[build-compat] in-scope=%d, validated=%d, missing=%d, exceptions=%d\n",
            n_in_scope, n_validated, n_missing, n_exceptions))
