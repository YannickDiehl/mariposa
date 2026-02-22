#' Weighted Standard Deviation
#'
#' Calculate weighted standard deviation for numeric variables, with support for
#' grouped data and multiple variables simultaneously.
#' Uses the SPSS frequency weights formula: \eqn{s_w = \sqrt{\sum w_i (x_i - \bar{x}_w)^2 / (V_1 - 1)}}
#' where \eqn{V_1 = \sum w_i}.
#'
#' @param data A data frame, or a numeric vector when used in summarise() context
#' @param ... Variable names (unquoted) or tidyselect expressions
#' @param weights Name of the weights variable (unquoted), or a numeric vector of weights
#' @param na.rm Logical; if TRUE, missing values are removed (default: TRUE)
#'
#' @return A w_sd object (list) containing results and metadata, or numeric values in summarise context
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Basic weighted standard deviation
#' survey_data %>% w_sd(age, weights = sampling_weight)
#'
#' # Multiple variables
#' survey_data %>% w_sd(age, income, life_satisfaction, weights = sampling_weight)
#'
#' # Grouped data
#' survey_data %>% group_by(region) %>% w_sd(age, weights = sampling_weight)
#'
#' # In summarise context
#' survey_data %>% summarise(sd_age = w_sd(age, weights = sampling_weight))
#'
#' # Unweighted (for comparison)
#' survey_data %>% w_sd(age)
#'
#' @family weighted_statistics
#' @export
w_sd <- function(data, ..., weights = NULL, na.rm = TRUE) {
  .w_statistic(
    data, ...,
    weights = {{ weights }},
    na.rm = na.rm,
    stat_fn = function(x, w) {
      if (length(x) < 2) return(NA_real_)
      if (is.null(w)) {
        stats::sd(x)
      } else {
        V1 <- sum(w)
        if (V1 <= 1) return(NA_real_)
        w_mean <- sum(x * w) / V1
        sqrt(sum(w * (x - w_mean)^2) / (V1 - 1))
      }
    },
    stat_name = "sd",
    weighted_col = "weighted_sd",
    unweighted_col = "sd",
    class_name = "w_sd"
  )
}

#' @export
#' @method print w_sd
print.w_sd <- function(x, digits = 3, ...) {
  .print_w_statistic(x, "Standard Deviation", "weighted_sd", "sd", digits)
}
