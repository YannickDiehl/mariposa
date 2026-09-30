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

#' Groups whose variance is undefined or zero
#'
#' Welch-type statistics need a variance estimate in every group: a group
#' with fewer than 2 cases (weighted: sum of weights <= 1, SPSS frequency
#' weights) has none, a group with zero variance gets an infinite Welch
#' weight n/s^2. SPSS prints no robust test in either case ("cannot be
#' performed ... because at least one group has 0 variance / sum of case
#' weights less than or equal to 1").
#'
#' @param y Numeric vector (valid values)
#' @param g Factor of the same length
#' @param w Optional weights of the same length
#' @return list(small = <levels without a variance estimate>,
#'   zero = <levels with zero variance>)
#' @noRd
.group_variance_problems <- function(y, g, w = NULL) {
  y <- .plain_numeric(y)
  lv <- levels(droplevels(g))
  small <- zero <- character(0)
  for (l in lv) {
    idx <- which(g == l)
    too_small <- if (is.null(w)) length(idx) < 2 else sum(w[idx]) <= 1
    if (too_small) {
      small <- c(small, l)
    } else if (all(y[idx] == y[idx[1]])) {
      zero <- c(zero, l)
    }
  }
  list(small = small, zero = zero)
}

#' Reason text for .group_variance_problems() (NULL when there is none)
#' @noRd
.group_variance_reason <- function(problems, weighted = FALSE) {
  quote_levels <- function(l) paste0("\"", l, "\"", collapse = ", ")
  if (length(problems$small) > 0) {
    what <- if (weighted) "a sum of weights <= 1" else "only 1 case"
    return(sprintf("group%s %s ha%s %s",
                   if (length(problems$small) > 1) "s" else "",
                   quote_levels(problems$small),
                   if (length(problems$small) > 1) "ve" else "s", what))
  }
  if (length(problems$zero) > 0) {
    return(sprintf("group%s %s ha%s zero variance",
                   if (length(problems$zero) > 1) "s" else "",
                   quote_levels(problems$zero),
                   if (length(problems$zero) > 1) "ve" else "s"))
  }
  NULL
}

#' Warn that a variable (in a group) was not computed
#'
#' @param fn Function name for the message, e.g. "t_test"
#' @param var_name Dependent variable
#' @param reason Short reason (from .dv_degenerate_reason() or similar)
#' @param group_info Optional one-row data frame of group keys
#' @noRd
.warn_not_computed <- function(fn, var_name, reason, group_info = NULL) {
  where <- .where_group(group_info)
  cli_warn(c(
    "{.fn {fn}}: {.var {var_name}} not computed{where}.",
    "i" = "{reason}"
  ), call = NULL)
}

#' " in group region = East" for warnings ("" when ungrouped)
#' @param group_info Optional one-row data frame of group keys
#' @noRd
.where_group <- function(group_info = NULL) {
  if (!is.null(group_info) && ncol(group_info) > 0) {
    paste0(" in group ", .format_group_label(group_info))
  } else ""
}

#' Degrees of freedom for display
#'
#' Whole numbers without decimals (2419), others with `digits` decimals
#' (Welch 2384.147; weighted sum(w) - 1), as in SPSS tables. NA -> "".
#' @noRd
.fmt_df <- function(df, digits = 3) {
  df <- as.numeric(df)
  whole <- !is.na(df) & abs(df - round(df)) < 1e-8
  out <- fmt_num(df, digits)
  out[whole] <- formatC(round(df[whole]), format = "d")
  out
}
