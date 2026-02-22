#' Weighted Range
#'
#' Calculate range (max - min) for numeric variables, with support for
#' grouped data and multiple variables simultaneously.
#' Note: Range is not affected by weights (same data values regardless of weighting),
#' but this function supports the standard w_* interface for consistency.
#'
#' @param data A data frame, or a numeric vector when used in summarise() context
#' @param ... Variable names (unquoted) or tidyselect expressions
#' @param weights Name of the weights variable (unquoted), or a numeric vector of weights
#' @param na.rm Logical; if TRUE, missing values are removed (default: TRUE)
#'
#' @return A w_range object (list) containing results and metadata, or numeric values in summarise context
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Basic range
#' survey_data %>% w_range(age, weights = sampling_weight)
#'
#' # Multiple variables
#' survey_data %>% w_range(age, income, weights = sampling_weight)
#'
#' # Grouped data
#' survey_data %>% group_by(region) %>% w_range(age, weights = sampling_weight)
#'
#' # In summarise context
#' survey_data %>% summarise(range_age = w_range(age, weights = sampling_weight))
#'
#' # Unweighted (for comparison)
#' survey_data %>% w_range(age)
#'
#' @family weighted_statistics
#' @export
w_range <- function(data, ..., weights = NULL, na.rm = TRUE) {
  .w_statistic(
    data, ...,
    weights = {{ weights }},
    na.rm = na.rm,
    stat_fn = function(x, w) {
      if (length(x) == 0) return(NA_real_)
      max(x) - min(x)
    },
    stat_name = "range",
    weighted_col = "weighted_range",
    unweighted_col = "range",
    class_name = "w_range"
  )
}

#' @export
#' @method print w_range
print.w_range <- function(x, digits = 3, ...) {
  .print_w_statistic(x, "Range", "weighted_range", "range", digits)
}
