#' Weighted Variance
#'
#' Calculate weighted variance for numeric variables, with support for
#' grouped data and multiple variables simultaneously.
#' Uses the SPSS frequency weights formula: \eqn{s^2_w = \sum w_i (x_i - \bar{x}_w)^2 / (V_1 - 1)}
#' where \eqn{V_1 = \sum w_i}.
#'
#' @param data A data frame, or a numeric vector when used in summarise() context
#' @param ... Variable names (unquoted) or tidyselect expressions
#' @param weights Name of the weights variable (unquoted), or a numeric vector of weights
#' @param na.rm Logical; if TRUE, missing values are removed (default: TRUE)
#'
#' @return A w_var object (list) containing results and metadata, or numeric values in summarise context
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Basic weighted variance
#' survey_data %>% w_var(age, weights = sampling_weight)
#'
#' # Multiple variables
#' survey_data %>% w_var(age, income, weights = sampling_weight)
#'
#' # Grouped data
#' survey_data %>% group_by(region) %>% w_var(age, weights = sampling_weight)
#'
#' # In summarise context
#' survey_data %>% summarise(var_age = w_var(age, weights = sampling_weight))
#'
#' # Unweighted (for comparison)
#' survey_data %>% w_var(age)
#'
#' @family weighted_statistics
#' @export
w_var <- function(data, ..., weights = NULL, na.rm = TRUE) {
  .w_statistic(
    data, ...,
    weights = {{ weights }},
    na.rm = na.rm,
    stat_fn = function(x, w) {
      if (length(x) < 2) return(NA_real_)
      if (is.null(w)) {
        stats::var(x)
      } else {
        V1 <- sum(w)
        if (V1 <= 1) return(NA_real_)
        w_mean <- sum(x * w) / V1
        sum(w * (x - w_mean)^2) / (V1 - 1)
      }
    },
    stat_name = "var",
    weighted_col = "weighted_var",
    unweighted_col = "var",
    class_name = "w_var"
  )
}

#' @export
#' @method print w_var
print.w_var <- function(x, digits = 3, ...) {
  .print_w_statistic(x, "Variance", "weighted_var", "var", digits)
}
