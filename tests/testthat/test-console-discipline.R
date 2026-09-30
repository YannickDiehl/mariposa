# =============================================================================
# CONSOLE OUTPUT DISCIPLINE META-TEST
# =============================================================================
# CRAN rule (manual review rounds, 0.7.x submissions): package code must not
# write to the console except in print()/summary()/interactive display
# functions — "Instead of print()/cat() rather use message()/warning() or
# if(verbose)cat(..) ... (except for print, summary, interactive functions)".
#
# The 0.7.2 review flagged a cat() that lived in a display *callback*
# (.kendall_tau_spec$pair_extras): functionally part of the print layer, but
# lexically inside a top-level list whose name says nothing about printing,
# so a reviewer (or their grep) cannot tell it is exempt.
#
# This test enforces the lexical rule package-wide so that pattern cannot
# come back in a resubmission:
#
#   Every top-level object in R/ whose code contains a cat() or print() call
#   must have a name starting with "print" or ".print" (the display layer).
#   print()/cat() nested inside capture.output(...) is silent and exempt.
#
# If this test fails on new code, do NOT add a whitelist entry here:
#   - computation code: put the information in the returned object, or use
#     message()/warning() (both suppressible)
#   - display code: move the cat() into a print-named function, or have the
#     callback return formatted lines that a print-layer function emits
#     (see pair_extras in R/correlation-engine.R)
#
# Runtime complement: test-silent-computation.R asserts zero stdout for every
# analysis entry point. This file closes the static side of the contract.
# =============================================================================

library(testthat)

test_that("cat()/print() calls appear only in print-layer functions", {
  # R/ sources are only present in the development tree (devtools::test(),
  # CI checkout), not in an installed package, on CRAN's test runner or
  # under covr (see r_source_dir() in helper-mariposa.R).
  r_dir <- r_source_dir()
  skip_if(is.null(r_dir), "R/ sources not available (installed package)")

  console_fns <- c("cat", "print", "writeLines")
  silent_wrappers <- "capture.output" # print() inside these writes nowhere

  # Recursively test whether an expression contains a cat()/print() call,
  # ignoring anything wrapped in a silent wrapper like capture.output().
  call_name <- function(fn) {
    if (is.name(fn)) return(as.character(fn))
    # base::cat / utils::capture.output style
    if (is.call(fn) && is.name(fn[[1]]) &&
        as.character(fn[[1]]) %in% c("::", ":::")) {
      return(as.character(fn[[3]]))
    }
    NA_character_
  }

  has_console_call <- function(expr) {
    if (is.call(expr)) {
      fn <- call_name(expr[[1]])
      if (!is.na(fn) && fn %in% silent_wrappers) return(FALSE)
      if (!is.na(fn) && fn %in% console_fns) return(TRUE)
      return(any(vapply(as.list(expr), has_console_call, logical(1))))
    }
    if (is.pairlist(expr) || is.expression(expr) || is.list(expr)) {
      return(any(vapply(as.list(expr), has_console_call, logical(1))))
    }
    FALSE
  }

  files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  expect_gt(length(files), 0)

  violations <- character(0)
  for (f in files) {
    exprs <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
    if (is.null(exprs)) next

    for (e in exprs) {
      is_assign <- is.call(e) && is.name(e[[1]]) &&
        as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]])

      if (is_assign) {
        obj_name <- as.character(e[[2]])
        if (grepl("^\\.?print", obj_name)) next # display layer, exempt
        if (has_console_call(e[[3]])) {
          violations <- c(violations,
                          sprintf("%s: `%s`", basename(f), obj_name))
        }
      } else if (has_console_call(e)) {
        violations <- c(violations,
                        sprintf("%s: <top-level expression>", basename(f)))
      }
    }
  }

  expect(length(violations) == 0, sprintf(
    paste0("cat()/print() found outside print-layer functions ",
           "(CRAN console contract, see file header):\n  %s"),
    paste(violations, collapse = "\n  ")))
})
