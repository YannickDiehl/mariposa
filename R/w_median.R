#' Weighted Median
#'
#' Calculate weighted median for numeric variables, with support for
#' grouped data and multiple variables simultaneously.
#'
#' @param data A data frame, or a numeric vector when used in summarise() context
#' @param ... Variable names (unquoted) or tidyselect expressions
#' @param weights Name of the weights variable (unquoted), or a numeric vector of weights
#' @param na.rm Logical; if TRUE, missing values are removed (default: TRUE)
#'
#' @return A w_median object (list) containing results and metadata, or numeric values in summarise context
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Basic weighted median
#' survey_data %>% w_median(age, weights = sampling_weight)
#'
#' # Multiple variables
#' survey_data %>% w_median(age, income, weights = sampling_weight)
#'
#' # Grouped data
#' survey_data %>% group_by(region) %>% w_median(age, weights = sampling_weight)
#'
#' # In summarise context
#' survey_data %>% summarise(med_age = w_median(age, weights = sampling_weight))
#'
#' # Unweighted (for comparison)
#' survey_data %>% w_median(age)
#'
#' @family weighted_statistics
#' @export
w_median <- function(data, ..., weights = NULL, na.rm = TRUE) {
  .w_statistic(
    data, ...,
    weights = {{ weights }},
    na.rm = na.rm,
    stat_fn = function(x, w) {
      if (length(x) == 0) return(NA_real_)
      if (is.null(w)) {
        stats::median(x)
      } else {
        .w_median(x, w, na.rm = FALSE)  # NA already handled by factory
      }
    },
    stat_name = "median",
    weighted_col = "weighted_median",
    unweighted_col = "median",
    class_name = "w_median"
  )
}

#' @export
#' @method print w_median
print.w_median <- function(x, digits = 3, ...) {
  .print_w_statistic(x, "Median", "weighted_median", "median", digits)
}
