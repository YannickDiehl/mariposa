# =============================================================================
# SURVEYSTAT PACKAGE IMPORTS
# =============================================================================
# Central import file for all external package dependencies
# This file contains all necessary @importFrom statements to make the package functional

#' @importFrom rlang enquo quo_is_null as_name expr %||% enquos syms sym quo eval_tidy
#' @importFrom dplyr group_vars group_modify select all_of distinct ungroup arrange desc group_by summarise group_split bind_rows group_keys mutate filter
#' @importFrom tidyselect eval_select starts_with ends_with contains where all_of any_of
#' @importFrom tibble tibble as_tibble
#' @importFrom stats var sd median quantile IQR na.omit complete.cases
#' @importFrom utils head tail str
NULL

# This file ensures all external functions are available throughout the package
# No additional code needed - roxygen2 will process these imports into NAMESPACE