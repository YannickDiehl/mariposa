#' Weighted Skewness
#'
#' Calculate weighted skewness for numeric variables, with support for
#' grouped data and multiple variables simultaneously.
#' Uses the SPSS Type 2 (sample-corrected) skewness formula.
#'
#' @param data A data frame, or a numeric vector when used in summarise() context
#' @param ... Variable names (unquoted) or tidyselect expressions
#' @param weights Name of the weights variable (unquoted), or a numeric vector of weights
#' @param na.rm Logical; if TRUE, missing values are removed (default: TRUE)
#'
#' @return A w_skew object (list) containing results and metadata, or numeric values in summarise context
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Basic weighted skewness
#' survey_data %>% w_skew(age, weights = sampling_weight)
#'
#' # Multiple variables
#' survey_data %>% w_skew(age, income, life_satisfaction, weights = sampling_weight)
#'
#' # Grouped data
#' survey_data %>% group_by(region) %>% w_skew(age, weights = sampling_weight)
#'
#' # In summarise context
#' survey_data %>% summarise(skew_age = w_skew(age, weights = sampling_weight))
#'
#' # Unweighted (for comparison)
#' survey_data %>% w_skew(age)
#'
#' @family weighted_statistics
#' @export
w_skew <- function(data, ..., weights = NULL, na.rm = TRUE) {
  .w_statistic(
    data, ...,
    weights = {{ weights }},
    na.rm = na.rm,
    stat_fn = function(x, w) {
      if (length(x) < 3) return(NA_real_)
      if (is.null(w)) {
        # Unweighted: Type 2 sample-corrected skewness (SPSS formula)
        n <- length(x)
        mean_x <- mean(x)
        m2 <- sum((x - mean_x)^2) / n
        m3 <- sum((x - mean_x)^3) / n
        type1_skew <- m3 / (m2^(3/2))
        type1_skew * sqrt(n * (n - 1)) / (n - 2)
      } else {
        # Weighted: Type 2 with frequency-weighted N = sum(w)
        w_sum <- sum(w)
        w_mean <- sum(x * w) / w_sum
        m2 <- sum(w * (x - w_mean)^2) / w_sum
        m3 <- sum(w * (x - w_mean)^3) / w_sum
        type1_skew <- m3 / (m2^(3/2))
        n <- w_sum  # SPSS treats sum of frequency weights as N
        type1_skew * sqrt(n * (n - 1)) / (n - 2)
      }
    },
    stat_name = "skew",
    weighted_col = "weighted_skew",
    unweighted_col = "skew",
    class_name = "w_skew"
  )
}

#' @export
#' @method print w_skew
print.w_skew <- function(x, digits = 3, ...) {
  .print_w_statistic(x, "Skewness", "weighted_skew", "skew", digits)
}
