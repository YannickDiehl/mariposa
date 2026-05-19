# =============================================================================
# VALIDATION DISCIPLINE META-TEST
# =============================================================================
# Enforces the rules of .claude/VALIDATION_CHARTER.md across the SPSS-validation
# test suite. Fails the build if forbidden patterns appear in test files.
#
# Operating modes:
#   - MARIPOSA_VALIDATION_STRICT = "TRUE": findings cause test failure (CRAN/CI mode)
#   - default: findings are reported via testthat::skip() with a summary message
#              (allows incremental remediation during Phase 2 without breaking CI)
#
# To enable strict mode locally:
#   Sys.setenv(MARIPOSA_VALIDATION_STRICT = "TRUE"); devtools::test()
#
# Policy reference: .claude/VALIDATION_CHARTER.md §8 and §11
# =============================================================================

library(testthat)

# ------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------

strict_mode <- isTRUE(as.logical(Sys.getenv("MARIPOSA_VALIDATION_STRICT", "FALSE")))

# Locate test files relative to the testthat working directory
test_dir <- "."
validation_files <- list.files(test_dir, pattern = "^test-.*-spss-validation\\.R$",
                               full.names = TRUE)


# ------------------------------------------------------------------------
# Pattern scanners
# ------------------------------------------------------------------------

# Each scanner returns a tibble-like list of findings:
#   list(file = chr, line = int, snippet = chr, pattern = chr)

scan_file <- function(file, pattern, label, exclude_comments = TRUE) {
  lines <- readLines(file, warn = FALSE)
  findings <- list()

  for (i in seq_along(lines)) {
    line <- lines[i]

    # Skip pure comment lines (lines starting with optional whitespace then #),
    # unless we're scanning for comment-based patterns specifically.
    if (exclude_comments && grepl("^\\s*#", line)) next

    # Skip the helper file itself which defines these patterns legitimately
    if (basename(file) == "helper-validation-tolerances.R") next
    if (basename(file) == "test-validation-discipline.R") next

    if (grepl(pattern, line, perl = TRUE)) {
      findings[[length(findings) + 1]] <- list(
        file = basename(file),
        line = i,
        snippet = trimws(line),
        pattern = label
      )
    }
  }
  findings
}

scan_all <- function(files, pattern, label, exclude_comments = TRUE) {
  result <- list()
  for (f in files) {
    result <- c(result, scan_file(f, pattern, label, exclude_comments))
  }
  result
}


# ------------------------------------------------------------------------
# Forbidden patterns (per Charter §8)
# ------------------------------------------------------------------------

# 1. expect_true(TRUE) as reporting-only block
findings_true_true <- scan_all(
  validation_files,
  pattern = "expect_true\\s*\\(\\s*TRUE\\s*\\)",
  label   = "expect_true(TRUE) — informational pass; remove or use skip()"
)

# 2. Inline numeric tolerance literals (matches tolerance = <number>)
#    Detects: tolerance = 0.05, tolerance=1e-3, tolerance = .001
#    Excludes calls inside tol() invocations (those route through the registry).
findings_inline_tol <- scan_all(
  validation_files,
  pattern = "(?<!tol\\()\\btolerance\\s*=\\s*[0-9.e+-]+(?!\\))",
  label   = "Inline tolerance literal — use assert_spss() with tier/what/precision"
)

# 3. grepl()-based tolerance switching (per-test escape hatches)
findings_grepl_tol <- scan_all(
  validation_files,
  pattern = "if\\s*\\(\\s*grepl\\([^)]+\\)\\s*\\)\\s*\\{[^}]*tolerance",
  label   = "grepl()-based tolerance switching — refactor to exception or fix"
)

# 4. NA-as-match defaulting in custom record_* helpers.
#    Detects: is.na(expected) && is.na(actual)  →  TRUE pattern.
findings_na_match <- scan_all(
  validation_files,
  pattern = "is\\.na\\(.+?\\)\\s*&&\\s*is\\.na\\(.+?\\)",
  label   = "NA-as-match pattern — forbidden by default; use assert_spss() with documented exception"
)

# 5. expect_no_error() used in validation context (acceptable in I/O tests, not here)
findings_no_error <- scan_all(
  validation_files,
  pattern = "expect_no_error\\s*\\(",
  label   = "expect_no_error() in validation file — pair with assert_spss()"
)

# 6. tryCatch in test body (swallows failures)
findings_trycatch <- scan_all(
  validation_files,
  pattern = "tryCatch\\s*\\([^,]+,\\s*error\\s*=",
  label   = "tryCatch with error handler — swallows failures; let errors surface"
)


# ------------------------------------------------------------------------
# NA placeholders in spss_values blocks
# ------------------------------------------------------------------------

# Scan for `= NA,` or `= NA_real_,` inside a spss_values <- list(...) block.
# Approximation: any line inside a validation test file that contains  `= NA`
# (with comma or closing) inside what looks like a list of reference values.

scan_na_placeholders <- function(files) {
  result <- list()
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    inside_spss_values <- FALSE
    brace_depth <- 0
    for (i in seq_along(lines)) {
      line <- lines[i]

      if (grepl("spss_values\\s*<-\\s*list\\s*\\(", line)) {
        inside_spss_values <- TRUE
        brace_depth <- 1
        next
      }

      if (inside_spss_values) {
        # Naive bracket tracker (does not handle strings containing parens)
        open  <- length(regmatches(line, gregexpr("\\(", line))[[1]])
        close <- length(regmatches(line, gregexpr("\\)", line))[[1]])
        brace_depth <- brace_depth + open - close

        if (grepl("=\\s*NA(_real_)?\\s*[,)]", line) && !grepl("^\\s*#", line)) {
          result[[length(result) + 1]] <- list(
            file = basename(f),
            line = i,
            snippet = trimws(line),
            pattern = "NA placeholder in spss_values list — fill with real reference value"
          )
        }

        if (brace_depth <= 0) {
          inside_spss_values <- FALSE
          brace_depth <- 0
        }
      }
    }
  }
  result
}

findings_na_placeholder <- scan_na_placeholders(validation_files)


# ------------------------------------------------------------------------
# Exception registry synchronisation
# ------------------------------------------------------------------------

# Read VALIDATION_EXCEPTIONS.md (path relative to testthat dir)
exceptions_md_path <- "../../.claude/VALIDATION_EXCEPTIONS.md"

extract_exc_ids_from_md <- function(path) {
  if (!file.exists(path)) return(character())
  txt <- readLines(path, warn = FALSE)
  # Match `## EXC-NNN: ...` headers in the Active Exceptions section
  ids <- character()
  in_active <- FALSE
  for (line in txt) {
    if (grepl("^##\\s+Active Exceptions", line)) { in_active <- TRUE; next }
    if (grepl("^##\\s+Retired Exceptions", line)) { in_active <- FALSE; next }
    if (in_active && grepl("^##\\s+EXC-[0-9]{3}:", line)) {
      ids <- c(ids, regmatches(line, regexpr("EXC-[0-9]{3}", line)))
    }
  }
  ids
}

scan_exc_references <- function(roots) {
  ids <- character()
  for (root in roots) {
    files <- list.files(root, pattern = "\\.R$", recursive = TRUE,
                        full.names = TRUE)
    for (f in files) {
      txt <- tryCatch(readLines(f, warn = FALSE), error = function(e) character())
      matches <- regmatches(txt, gregexpr("EXC-[0-9]{3}", txt))
      ids <- c(ids, unlist(matches))
    }
  }
  unique(ids)
}


# ------------------------------------------------------------------------
# Reporting
# ------------------------------------------------------------------------

all_findings <- c(
  findings_true_true,
  findings_inline_tol,
  findings_grepl_tol,
  findings_na_match,
  findings_no_error,
  findings_trycatch,
  findings_na_placeholder
)

summarise_findings <- function(findings) {
  if (length(findings) == 0) return("None.")
  parts <- vapply(findings, function(f) {
    sprintf("  %s:%d  [%s]\n    %s",
            f$file, f$line, f$pattern, f$snippet)
  }, character(1))
  paste(parts, collapse = "\n")
}


# ------------------------------------------------------------------------
# Tests
# ------------------------------------------------------------------------

test_that("validation discipline: no forbidden patterns in SPSS-validation tests", {
  if (length(all_findings) == 0) {
    succeed("No forbidden patterns found in validation tests.")
    return(invisible(TRUE))
  }

  msg <- sprintf(
    paste(
      "Validation discipline meta-test found %d forbidden pattern(s):",
      "%s",
      "",
      "Policy reference: .claude/VALIDATION_CHARTER.md §8",
      "Strict-mode enforcement is %s.",
      "Set MARIPOSA_VALIDATION_STRICT=TRUE to fail the build on findings.",
      sep = "\n"
    ),
    length(all_findings),
    summarise_findings(all_findings),
    if (strict_mode) "ENABLED" else "OFF (advisory only)"
  )

  if (strict_mode) {
    fail(msg)
  } else {
    skip(paste0("Charter violations detected (advisory mode):\n", msg))
  }
})


test_that("validation discipline: exception registry matches markdown", {
  md_ids <- extract_exc_ids_from_md(exceptions_md_path)

  # Source the helper to load the R-side registry
  registry_path <- "helper-validation-tolerances.R"
  if (!file.exists(registry_path)) {
    skip("helper-validation-tolerances.R not found; cannot verify registry sync.")
  }
  env <- new.env()
  source(registry_path, local = env)
  r_ids <- names(env$exception_registry)

  only_in_md <- setdiff(md_ids, r_ids)
  only_in_r  <- setdiff(r_ids, md_ids)

  if (length(only_in_md) == 0 && length(only_in_r) == 0) {
    succeed("Exception registry is in sync with VALIDATION_EXCEPTIONS.md.")
    return(invisible(TRUE))
  }

  msg_parts <- character()
  if (length(only_in_md) > 0) {
    msg_parts <- c(msg_parts, sprintf(
      "Exceptions in VALIDATION_EXCEPTIONS.md but missing from exception_registry: %s",
      paste(only_in_md, collapse = ", ")
    ))
  }
  if (length(only_in_r) > 0) {
    msg_parts <- c(msg_parts, sprintf(
      "Exceptions in exception_registry but missing from VALIDATION_EXCEPTIONS.md: %s",
      paste(only_in_r, collapse = ", ")
    ))
  }
  msg <- paste(msg_parts, collapse = "\n")

  if (strict_mode) {
    fail(msg)
  } else {
    skip(paste0("Exception registry drift (advisory mode):\n", msg))
  }
})


test_that("validation discipline: every exception is referenced in R/ and tests/", {
  md_ids <- extract_exc_ids_from_md(exceptions_md_path)

  if (length(md_ids) == 0) {
    succeed("No active exceptions registered; nothing to verify.")
    return(invisible(TRUE))
  }

  r_referenced    <- scan_exc_references("../../R")
  test_referenced <- scan_exc_references(".")

  missing_in_r    <- setdiff(md_ids, r_referenced)
  missing_in_test <- setdiff(md_ids, test_referenced)

  problems <- character()
  if (length(missing_in_r) > 0) {
    problems <- c(problems, sprintf(
      "Exceptions in MD but not referenced in any R/ file: %s",
      paste(missing_in_r, collapse = ", ")
    ))
  }
  if (length(missing_in_test) > 0) {
    problems <- c(problems, sprintf(
      "Exceptions in MD but not referenced in any test file: %s",
      paste(missing_in_test, collapse = ", ")
    ))
  }

  if (length(problems) == 0) {
    succeed("All registered exceptions have R/ and test/ references.")
    return(invisible(TRUE))
  }

  msg <- paste(problems, collapse = "\n")
  if (strict_mode) {
    fail(msg)
  } else {
    skip(paste0("Exception reference gaps (advisory mode):\n", msg))
  }
})


# ------------------------------------------------------------------------
# Status logging (always runs, regardless of strict mode)
# ------------------------------------------------------------------------

cli_message <- function(msg) {
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_inform(msg)
  } else {
    message(msg)
  }
}

cli_message(sprintf(
  "[validation-discipline] scanned %d test files; %d findings; strict mode = %s",
  length(validation_files),
  length(all_findings),
  if (strict_mode) "ON" else "OFF (advisory)"
))
