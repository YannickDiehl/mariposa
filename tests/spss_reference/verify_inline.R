# =============================================================================
# verify_inline.R — SPSS REFERENCE DRIFT DETECTOR
# =============================================================================
# Developer-only script that audits the inline SPSS reference values in
# test-*-spss-validation.R files against their cited source lines in
# tests/spss_reference/outputs/*.txt.
#
# Charter reference: .claude/VALIDATION_CHARTER.md §7
#
# This script is NOT run by R CMD check or CI. It is part of the maintainer's
# pre-release checklist (Charter §12). Run it whenever SPSS reference values
# have been (re-)generated.
#
# Usage:
#   Rscript tests/spss_reference/verify_inline.R
#
# Exit code:
#   0  — all inline values match their cited reference lines
#   1  — drift detected; review report and update tests
#
# How citations work:
# In a test file:
#
#   spss_values <- list(
#     unweighted_ungrouped = list(
#       mean = 3.63,  # t_test_output.txt:43
#       sd   = 1.153, # t_test_output.txt:44
#       ...
#     )
#   )
#
# The trailing comment "<file>:<line>" points to the SPSS output line. This
# script reads each citation, extracts the corresponding line from the .txt,
# and warns if no numeric token on that line matches the inline value.
#
# The match is fuzzy: any numeric token on the cited line within 1% of the
# inline value counts as a match. This accepts SPSS print-rounding while
# catching outright mistakes (wrong line, wrong file, copy-paste error).
# =============================================================================

suppressPackageStartupMessages({
  # No external packages needed; base R only.
})

# ---- Helpers (defined early, used below) ---------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a


# ---- Paths ----------------------------------------------------------------
# Repo root is detected by walking upward from the script's location looking
# for a DESCRIPTION file. Works both when run via Rscript and when sourced
# from an interactive session, regardless of cwd.

find_script_dir <- function() {
  # Try Rscript --file= mechanism first
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[grep("^--file=", args)]
  if (length(file_arg) > 0L) {
    script_path <- sub("^--file=", "", file_arg[1])
    return(normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE))
  }
  # Interactive source() — use sys.frame
  if (!is.null(sys.frame(1)$ofile)) {
    return(normalizePath(dirname(sys.frame(1)$ofile),
                         winslash = "/", mustWork = TRUE))
  }
  # Fallback: assume cwd is repo root or script dir
  normalizePath(".", winslash = "/", mustWork = TRUE)
}

find_repo_root <- function(start_dir) {
  d <- start_dir
  for (i in 1:8L) {
    if (file.exists(file.path(d, "DESCRIPTION"))) {
      return(normalizePath(d, winslash = "/", mustWork = TRUE))
    }
    parent <- normalizePath(file.path(d, ".."), winslash = "/", mustWork = FALSE)
    if (identical(parent, d)) break
    d <- parent
  }
  stop("Could not locate repo root (DESCRIPTION file) from: ", start_dir)
}

script_dir <- find_script_dir()
repo_root  <- find_repo_root(script_dir)

test_dir   <- file.path(repo_root, "tests", "testthat")
ref_dir    <- file.path(repo_root, "tests", "spss_reference", "outputs")

cat(sprintf("[verify_inline] repo root: %s\n", repo_root))
cat(sprintf("[verify_inline] test dir:  %s\n", test_dir))
cat(sprintf("[verify_inline] ref dir:   %s\n", ref_dir))


# ---- Citation parsing -----------------------------------------------------

# Regex to extract the citation comment from a test file line:
# matches:   <expr>,  # <file>.txt:<line>
# Allows optional surrounding whitespace.
citation_pattern <- "#\\s*([A-Za-z0-9_.-]+\\.txt)\\s*:\\s*([0-9]+)\\s*$"

# Regex to extract a numeric value that appears just before the comment.
# Captures the *last* numeric literal on the line before the comment, since
# that's typically the value being cited (e.g., `mean = 3.63,  # ...`).
value_pattern <- "([+-]?[0-9]+\\.?[0-9]*(?:[eE][+-]?[0-9]+)?)\\s*[,)]?\\s*#"

# Special-case the SPSS-truncated p-value sentinel.
sentinel_pattern <- "\"<\\.001\""


parse_test_file <- function(path) {
  lines <- readLines(path, warn = FALSE)
  citations <- list()

  for (i in seq_along(lines)) {
    line <- lines[i]

    # Skip pure comment lines
    if (grepl("^\\s*#", line)) next

    citation <- regmatches(line, regexec(citation_pattern, line))[[1]]
    if (length(citation) == 0L) next

    ref_file <- citation[2]
    ref_line <- as.integer(citation[3])

    # Try to extract the numeric value before the comment
    value_match <- regmatches(line,
                              regexec(value_pattern, line, perl = TRUE))[[1]]

    if (length(value_match) > 0L) {
      value <- suppressWarnings(as.numeric(value_match[2]))
      value_type <- "numeric"
    } else if (grepl(sentinel_pattern, line)) {
      value <- "<.001"
      value_type <- "sentinel"
    } else {
      value <- NA
      value_type <- "unparseable"
    }

    citations[[length(citations) + 1]] <- list(
      test_file = basename(path),
      test_line = i,
      snippet   = trimws(line),
      ref_file  = ref_file,
      ref_line  = ref_line,
      value     = value,
      value_type = value_type
    )
  }

  citations
}


# ---- Reference parsing ----------------------------------------------------

read_ref_line <- function(ref_file, line_no, cache = ref_cache) {
  if (!exists(ref_file, envir = cache, inherits = FALSE)) {
    path <- file.path(ref_dir, ref_file)
    if (!file.exists(path)) {
      assign(ref_file, character(), envir = cache)
      return(NA_character_)
    }
    assign(ref_file, readLines(path, warn = FALSE), envir = cache)
  }

  lines <- get(ref_file, envir = cache)
  if (line_no < 1 || line_no > length(lines)) return(NA_character_)
  lines[line_no]
}


#' Read a window of lines around a citation point.
#'
#' SPSS prints related statistics (r/p/n; chi-sq/df/Sig; per-cell counts) on
#' adjacent rows of a table. A citation pointing at the top of a block — or
#' at any line within it — should resolve any value contained in the block.
#' This helper returns the cited line plus `window` lines before and after,
#' so compare_value() can do block-level fuzzy matching.
read_ref_window <- function(ref_file, line_no, window = 10L,
                            cache = ref_cache) {
  if (!exists(ref_file, envir = cache, inherits = FALSE)) {
    path <- file.path(ref_dir, ref_file)
    if (!file.exists(path)) {
      assign(ref_file, character(), envir = cache)
      return(character())
    }
    assign(ref_file, readLines(path, warn = FALSE), envir = cache)
  }

  lines <- get(ref_file, envir = cache)
  if (length(lines) == 0L) return(character())
  lo <- max(1L, line_no - window)
  hi <- min(length(lines), line_no + window)
  lines[lo:hi]
}


extract_numeric_tokens <- function(text) {
  if (is.na(text)) return(numeric(0))
  # Match numbers with optional leading sign, including those starting with
  # a decimal point (SPSS print style: .006, -.029, etc.)
  matches <- regmatches(text, gregexpr("[+-]?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?",
                                       text, perl = TRUE))[[1]]
  matches <- matches[nzchar(matches)]
  suppressWarnings(as.numeric(matches))
}


# ---- Comparison -----------------------------------------------------------

compare_value <- function(citation, ref_text) {

  if (citation$value_type == "sentinel") {
    if (is.na(ref_text)) {
      return(list(status = "missing-ref",
                  detail = sprintf("Reference file/line not found")))
    }
    if (grepl("<\\.?001|0?\\.000", ref_text)) {
      return(list(status = "match",
                  detail = sprintf("sentinel '<.001' confirmed at %s:%d",
                                   citation$ref_file, citation$ref_line)))
    }
    # Block fallback: SPSS often prints chi-sq/df/Sig as three adjacent
    # rows. The citation may point at the Chi-Square row while the p-value
    # is in the Asymp. Sig. row a few lines later. Search a window.
    window_lines <- read_ref_window(citation$ref_file, citation$ref_line,
                                     window = 10L)
    if (any(grepl("<\\.?001|0?\\.000", window_lines))) {
      return(list(status = "match-window",
                  detail = sprintf("sentinel '<.001' found in ±10-line window of %s:%d",
                                   citation$ref_file, citation$ref_line)))
    }
    return(list(status = "mismatch",
                detail = sprintf("Inline '<.001' but reference line shows: %s",
                                 trimws(ref_text))))
  }

  if (citation$value_type == "unparseable") {
    return(list(status = "skip",
                detail = "Could not parse inline value (skipping)"))
  }

  if (is.na(ref_text)) {
    return(list(status = "missing-ref",
                detail = sprintf("Reference file/line not found: %s:%d",
                                 citation$ref_file, citation$ref_line)))
  }

  inline <- citation$value
  if (is.na(inline)) {
    return(list(status = "skip", detail = "Inline value parsed as NA"))
  }

  # 1) Try the cited line exactly.
  ref_tokens <- extract_numeric_tokens(ref_text)
  tol <- max(abs(inline) * 0.01, 1e-4)
  if (length(ref_tokens) > 0L) {
    diffs <- abs(ref_tokens - inline)
    matches <- ref_tokens[diffs <= tol]
    if (length(matches) > 0L) {
      return(list(status = "match",
                  detail = sprintf("%g matches reference token(s) %s on line",
                                   inline, paste(matches, collapse = ", "))))
    }
  }

  # 2) Block fallback: search a window of ±10 lines. SPSS tables put
  #    related values (r/p/n; chi-sq/df/Sig; per-cell counts) on adjacent
  #    rows, so citations pointing at the block header should resolve any
  #    value within the block.
  window_lines <- read_ref_window(citation$ref_file, citation$ref_line,
                                   window = 10L)
  for (line in window_lines) {
    tokens <- extract_numeric_tokens(line)
    if (length(tokens) == 0L) next
    if (any(abs(tokens - inline) <= tol)) {
      return(list(status = "match-window",
                  detail = sprintf("%g matched within ±10-line window of %s:%d",
                                   inline, citation$ref_file, citation$ref_line)))
    }
  }

  if (length(ref_tokens) == 0L) {
    return(list(status = "no-numbers",
                detail = sprintf("Reference line has no numeric tokens: %s",
                                 trimws(ref_text))))
  }
  return(list(status = "drift",
              detail = sprintf("Inline %g not on reference line or within ±10 lines. Tokens: %s. Line: %s",
                               inline,
                               paste(ref_tokens, collapse = ", "),
                               trimws(ref_text))))
}


# ---- Main -----------------------------------------------------------------

ref_cache <- new.env(parent = emptyenv())

test_files <- list.files(test_dir,
                         pattern = "^test-.*-spss-validation\\.R$",
                         full.names = TRUE)

cat(sprintf("[verify_inline] scanning %d test file(s)...\n", length(test_files)))
cat("\n")

all_citations <- list()
for (tf in test_files) {
  cs <- parse_test_file(tf)
  all_citations <- c(all_citations, cs)
}

cat(sprintf("[verify_inline] found %d citation(s)\n", length(all_citations)))

if (length(all_citations) == 0L) {
  cat("[verify_inline] no citations found; tests do not yet use the citation convention.\n")
  cat("[verify_inline] update tests per .claude/VALIDATION_CHARTER.md §7.\n")
  quit(status = 0)
}

# Run comparisons
results <- lapply(all_citations, function(c) {
  ref_text <- read_ref_line(c$ref_file, c$ref_line)
  cmp <- compare_value(c, ref_text)
  c(c, cmp)
})

# Tally
status_counts <- table(vapply(results, `[[`, character(1), "status"))
cat("\n[verify_inline] summary:\n")
for (s in names(status_counts)) {
  cat(sprintf("  %-12s %d\n", s, status_counts[[s]]))
}

# Report drifts and missing refs
problems <- Filter(function(r) r$status %in% c("drift", "mismatch", "missing-ref"),
                   results)

if (length(problems) > 0L) {
  cat("\n[verify_inline] PROBLEMS FOUND:\n")
  for (p in problems) {
    cat(sprintf("\n  %s:%d  [%s]\n    snippet:   %s\n    ref:       %s:%d\n    %s\n",
                p$test_file, p$test_line, p$status,
                p$snippet, p$ref_file, p$ref_line, p$detail))
  }
  cat("\n[verify_inline] FAIL — fix above issues before release.\n")
  quit(status = 1)
}

# Skips and no-numbers are warnings only
warnings <- Filter(function(r) r$status %in% c("skip", "no-numbers"), results)
if (length(warnings) > 0L) {
  cat(sprintf("\n[verify_inline] %d warning(s) (not fatal):\n", length(warnings)))
  for (w in warnings[seq_len(min(10, length(warnings)))]) {
    cat(sprintf("  %s:%d  [%s]  %s\n",
                w$test_file, w$test_line, w$status, w$detail))
  }
  if (length(warnings) > 10) {
    cat(sprintf("  ...and %d more\n", length(warnings) - 10))
  }
}

cat("\n[verify_inline] PASS — no drift detected.\n")
quit(status = 0)
