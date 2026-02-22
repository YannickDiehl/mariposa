#' Weighted Standard Error
#'
#' Calculate weighted standard error for numeric variables, with support for
#' grouped data and multiple variables simultaneously.
#' Formula: \eqn{SE_w = s_w / \sqrt{V_1}} where \eqn{s_w} is the weighted SD
#' and \eqn{V_1 = \sum w_i}.
#'
#' @param data A data frame, or a numeric vector when used in summarise() context
#' @param ... Variable names (unquoted) or tidyselect expressions
#' @param weights Name of the weights variable (unquoted), or a numeric vector of weights
#' @param na.rm Logical; if TRUE, missing values are removed (default: TRUE)
#'
#' @return A w_se object (list) containing results and metadata, or numeric values in summarise context
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Basic weighted standard error
#' survey_data %>% w_se(age, weights = sampling_weight)
#'
#' # Multiple variables
#' survey_data %>% w_se(age, income, weights = sampling_weight)
#'
#' # Grouped data
#' survey_data %>% group_by(region) %>% w_se(age, weights = sampling_weight)
#'
#' # In summarise context
#' survey_data %>% summarise(se_age = w_se(age, weights = sampling_weight))
#'
#' # Unweighted (for comparison)
#' survey_data %>% w_se(age)
#'
#' @family weighted_statistics
#' @export
w_se <- function(data, ..., weights = NULL, na.rm = TRUE) {
  .w_statistic(
    data, ...,
    weights = {{ weights }},
    na.rm = na.rm,
    stat_fn = function(x, w) {
      if (length(x) < 2) return(NA_real_)
      if (is.null(w)) {
        stats::sd(x) / sqrt(length(x))
      } else {
        V1 <- sum(w)
        if (V1 <= 1) return(NA_real_)
        w_mean <- sum(x * w) / V1
        w_sd <- sqrt(sum(w * (x - w_mean)^2) / (V1 - 1))
        w_sd / sqrt(V1)
      }
    },
    stat_name = "se",
    weighted_col = "weighted_se",
    unweighted_col = "se",
    class_name = "w_se"
  )
}

#' @export
#' @method print w_se
print.w_se <- function(x, digits = 3, ...) {
  .print_w_statistic(x, "Standard Error", "weighted_se", "se", digits)
}
