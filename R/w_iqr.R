#' Weighted Interquartile Range (IQR)
#'
#' Calculate weighted interquartile range (Q3 - Q1) for numeric variables,
#' with support for grouped data and multiple variables simultaneously.
#'
#' @param data A data frame, or a numeric vector when used in summarise() context
#' @param ... Variable names (unquoted) or tidyselect expressions
#' @param weights Name of the weights variable (unquoted), or a numeric vector of weights
#' @param na.rm Logical; if TRUE, missing values are removed (default: TRUE)
#'
#' @return A w_iqr object (list) containing results and metadata, or numeric values in summarise context
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Basic weighted IQR
#' survey_data %>% w_iqr(age, weights = sampling_weight)
#'
#' # Multiple variables
#' survey_data %>% w_iqr(age, income, weights = sampling_weight)
#'
#' # Grouped data
#' survey_data %>% group_by(region) %>% w_iqr(age, weights = sampling_weight)
#'
#' # In summarise context
#' survey_data %>% summarise(iqr_age = w_iqr(age, weights = sampling_weight))
#'
#' # Unweighted (for comparison)
#' survey_data %>% w_iqr(age)
#'
#' @family weighted_statistics
#' @export
w_iqr <- function(data, ..., weights = NULL, na.rm = TRUE) {
  .w_statistic(
    data, ...,
    weights = {{ weights }},
    na.rm = na.rm,
    stat_fn = function(x, w) {
      if (length(x) == 0) return(NA_real_)
      if (is.null(w)) {
        unname(stats::IQR(x))
      } else {
        q <- .w_quantile(x, w, probs = c(0.25, 0.75), na.rm = FALSE)
        unname(q[2] - q[1])
      }
    },
    stat_name = "iqr",
    weighted_col = "weighted_iqr",
    unweighted_col = "iqr",
    class_name = "w_iqr"
  )
}

#' @export
#' @method print w_iqr
print.w_iqr <- function(x, digits = 3, ...) {
  .print_w_statistic(x, "Interquartile Range", "weighted_iqr", "iqr", digits)
}
