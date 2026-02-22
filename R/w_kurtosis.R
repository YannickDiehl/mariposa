#' Weighted Kurtosis
#'
#' Calculate weighted kurtosis for numeric variables, with support for
#' grouped data and multiple variables simultaneously.
#' Uses the SPSS Type 2 (sample-corrected) excess kurtosis formula.
#' Reference: Joanes & Gill (1998), "Comparing measures of sample skewness and kurtosis"
#'
#' @param data A data frame, or a numeric vector when used in summarise() context
#' @param ... Variable names (unquoted) or tidyselect expressions
#' @param weights Name of the weights variable (unquoted), or a numeric vector of weights
#' @param na.rm Logical; if TRUE, missing values are removed (default: TRUE)
#' @param excess Logical; if TRUE, returns excess kurtosis (default: TRUE, matching SPSS)
#'
#' @return A w_kurtosis object (list) containing results and metadata, or numeric values in summarise context
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Basic weighted kurtosis (excess kurtosis, default)
#' survey_data %>% w_kurtosis(age, weights = sampling_weight)
#'
#' # Multiple variables
#' survey_data %>% w_kurtosis(age, income, life_satisfaction, weights = sampling_weight)
#'
#' # Grouped data
#' survey_data %>% group_by(region) %>% w_kurtosis(age, weights = sampling_weight)
#'
#' # Raw kurtosis (not excess)
#' survey_data %>% w_kurtosis(age, weights = sampling_weight, excess = FALSE)
#'
#' # In summarise context
#' survey_data %>% summarise(kurt_age = w_kurtosis(age, weights = sampling_weight))
#'
#' # Unweighted (for comparison)
#' survey_data %>% w_kurtosis(age)
#'
#' @family weighted_statistics
#' @export
w_kurtosis <- function(data, ..., weights = NULL, na.rm = TRUE, excess = TRUE) {
  .w_statistic(
    data, ...,
    weights = {{ weights }},
    na.rm = na.rm,
    stat_fn = function(x, w) {
      if (length(x) < 4) return(NA_real_)
      .calc_kurtosis(x, w = w, excess = excess)
    },
    stat_name = "kurtosis",
    weighted_col = "weighted_kurtosis",
    unweighted_col = "kurtosis",
    class_name = "w_kurtosis",
    extra_args = list(excess = excess)
  )
}

#' @export
#' @method print w_kurtosis
print.w_kurtosis <- function(x, digits = 3, ...) {
  kurtosis_type <- if (x$excess) "Excess Kurtosis" else "Kurtosis"
  .print_w_statistic(x, kurtosis_type, "weighted_kurtosis", "kurtosis", digits)
}
