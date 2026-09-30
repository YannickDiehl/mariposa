# =============================================================================
# Shared helpers for the non-parametric and categorical tests
# =============================================================================
# Used by mann_whitney, kruskal_wallis, wilcoxon_test, friedman_test,
# binomial_test, chisq_gof, fisher_test, mcnemar_test, chi_square,
# dunn_test and pairwise_wilcoxon.
# =============================================================================

#' " in group var = value" suffix for warnings about one split
#'
#' @param key One-row data frame of group values (from group_keys()/
#'   group_modify()), or NULL for ungrouped data
#' @return Character scalar ("" when ungrouped)
#' @noRd
.np_where <- function(key) {
  if (is.null(key) || length(key) == 0 || NROW(key) == 0) return("")
  paste0(" in group ", .format_group_label(key))
}

#' Observed categories of a categorical variable, SPSS style
#'
#' SPSS tabulates the categories that occur in the data, in code order, and
#' shows value labels. Factor levels without cases (e.g. after filter())
#' are dropped - an empty level used to produce NaN statistics or a phantom
#' category with an extra degree of freedom. Labelled variables become a
#' factor of their value labels (codes without a label keep the code).
#'
#' @param x Vector (factor, haven_labelled, character, numeric, logical)
#' @return A factor without unused levels
#' @noRd
.np_factor <- function(x) {
  droplevels(.group_factor(x))
}

#' Codes behind the levels of a .np_factor() result
#'
#' @param x The original vector
#' @param levels Levels of the .np_factor() result
#' @return Character vector of codes (NULL when x is not labelled)
#' @noRd
.np_codes <- function(x, levels) {
  if (!inherits(x, "haven_labelled")) return(NULL)
  v <- .plain_numeric(x)
  vals <- sort(unique(v[!is.na(v)]))
  f <- .group_factor(x)
  as.character(vals[match(levels, levels(f))])
}
