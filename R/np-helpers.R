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
