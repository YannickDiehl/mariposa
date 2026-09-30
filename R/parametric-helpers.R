# =============================================================================
# Shared internal helpers for the parametric tests
# =============================================================================
# t_test(), oneway_anova(), factorial_anova(), ancova() and the pairwise
# post-hoc engine share one policy for degenerate data: a dependent variable
# that cannot be tested (no valid values, no variance, ...) is reported as
# "not computed (<reason>)" with a warning that names the variable and, for
# grouped data, the group - never with a spurious statistic computed from
# floating-point noise, a raw (locale-dependent) base-R error, or an NA
# table printed as "F(NA, NA) = ,".
# =============================================================================

#' Why a dependent variable cannot be tested
#'
#' @param y Numeric vector of valid (non-missing) values
#' @param g Optional factor of the same length: the groups/cells
#' @return NULL when the data can be tested, otherwise a short reason
#' @noRd
.dv_degenerate_reason <- function(y, g = NULL) {
  y <- .plain_numeric(y)
  if (length(y) == 0) return("no non-missing values")
  if (all(y == y[1])) {
    return(sprintf("no variance: all values are %s", format(y[1])))
  }
  if (!is.null(g)) {
    const_within <- vapply(split(y, g, drop = TRUE),
                           function(v) all(v == v[1]), logical(1))
    if (all(const_within)) return("no variance within any group")
  }
  NULL
}

#' Signal that a statistic cannot be computed for this variable/group
#'
#' A classed condition: the per-variable loops catch it, warn once (naming
#' variable and group) and keep an NA row with the reason in `note`.
#' @noRd
.not_computed <- function(reason) {
  rlang::abort(reason, class = "mariposa_not_computed")
}

#' Warn that a variable (in a group) was not computed
#'
#' @param fn Function name for the message, e.g. "t_test"
#' @param var_name Dependent variable
#' @param reason Short reason (from .dv_degenerate_reason() or similar)
#' @param group_info Optional one-row data frame of group keys
#' @noRd
.warn_not_computed <- function(fn, var_name, reason, group_info = NULL) {
  where <- if (!is.null(group_info) && ncol(group_info) > 0) {
    paste0(" in group ", .format_group_label(group_info))
  } else ""
  cli_warn(c(
    "{.fn {fn}}: {.var {var_name}} not computed{where}.",
    "i" = "{reason}"
  ), call = NULL)
}
