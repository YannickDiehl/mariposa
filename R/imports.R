# =============================================================================
# SURVEYSTAT PACKAGE IMPORTS
# =============================================================================
# Central import file for all external package dependencies
# This file contains all necessary @importFrom statements to make the package functional

#' @importFrom rlang enquo quo_is_null as_name expr %||% enquos syms sym quo eval_tidy
#' @importFrom dplyr %>% group_vars group_modify select all_of distinct ungroup arrange desc group_by summarise group_split bind_rows group_keys mutate across rowwise c_across
#' @importFrom tidyselect eval_select starts_with ends_with contains where all_of any_of
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tibble as_tibble
#' @importFrom stats var sd median quantile IQR na.omit complete.cases TukeyHSD aov as.formula chisq.test confint cor lm oneway.test pf pnorm pt ptukey qf qnorm qt qtukey t.test weighted.mean wilcox.test xtabs
#' @importFrom utils head tail str capture.output combn
NULL

# This file ensures all external functions are available throughout the package
# No additional code needed - roxygen2 will process these imports into NAMESPACE