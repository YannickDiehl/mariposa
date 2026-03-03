# =============================================================================
# MARIPOSA PACKAGE IMPORTS
# =============================================================================
# Central import file for all external package dependencies
# This file contains all necessary @importFrom statements to make the package functional

#' @importFrom cli cli_abort cli_warn cli_inform cli_h1 cli_h2 cli_text cli_rule cli_bullets cli_alert_success cli_alert_info cli_alert_warning cli_div cli_end
#' @importFrom rlang enquo quo_is_null as_name expr %||% enquos syms sym quo eval_tidy
#' @importFrom dplyr %>% group_vars group_modify select all_of distinct ungroup arrange desc group_by summarise group_split bind_rows group_keys mutate across rowwise c_across
#' @importFrom tidyselect eval_select starts_with ends_with contains where all_of any_of
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tibble as_tibble
#' @importFrom stats var sd median quantile IQR na.omit complete.cases TukeyHSD aov as.formula ave binom.test chisq.test confint cor fisher.test friedman.test kruskal.test lm oneway.test p.adjust pchisq pf pnorm pt ptukey qf qnorm qt qtukey t.test weighted.mean wilcox.test xtabs
#' @importFrom htmltools tags tagList save_html HTML
#' @importFrom utils head tail str capture.output combn
NULL

# This file ensures all external functions are available throughout the package
# No additional code needed - roxygen2 will process these imports into NAMESPACE