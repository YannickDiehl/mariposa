# =============================================================================
# SUMMARY METHOD HELPER FUNCTIONS
# =============================================================================
# Centralized helpers for all summary.* and print.summary.* methods.
# See the plan at .claude/plans/ for the full style guide.
#
# Pattern:
#   summary.{class}()         -> builds summary object with $show booleans
#   print.summary.{class}()   -> verbose output with boolean-gated sections
#   print.{class}()           -> compact one-liner overview
# =============================================================================

#' Build a summary object from a mariposa result
#'
#' Copies all fields from the original result object and adds
#' \code{$show} (boolean toggle list) and \code{$digits}.
#'
#' @param object The original result object
#' @param show Named list of logical toggles for sections
#' @param digits Number of decimal places for formatting
#' @param class_name Summary class name (e.g. \code{"summary.t_test"})
#' @return A list with class \code{class_name}
#' @keywords internal
build_summary_object <- function(object, show, digits, class_name) {
  out <- object
  out$show <- show
  out$digits <- digits
  class(out) <- class_name
  out
}

#' Format a compact p-value string
#'
#' Returns \code{"p < 0.001"} for very small values,
#' otherwise \code{"p = 0.XXX"}.
#'
#' @param p Numeric p-value
#' @param digits Decimal places (default 3)
#' @return Character string
#' @keywords internal
format_p_compact <- function(p, digits = 3) {
  if (is.na(p)) return("p = NA")
  if (p < 0.001) return("p < 0.001")
  sprintf("p = %s", format(round(p, digits), nsmall = digits))
}
