#' Find the Range of Your Data
#'
#' @description
#' \code{w_range()} calculates the range (maximum minus minimum) of your data.
#' The range gives you the total spread from the smallest to the largest observed
#' value. It provides the \code{w_*} interface for consistency with other
#' weighted statistics, though the range itself is not affected by weights
#' (the minimum and maximum values remain the same regardless of weighting).
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The numeric variables you want to analyze. You can list multiple
#'   variables or use helpers like \code{starts_with("income")}
#' @param weights Survey weights are accepted for interface consistency, but do
#'   not affect the range calculation. The range depends only on the observed
#'   minimum and maximum values.
#' @param na.rm Remove missing values before calculating? (Default: TRUE)
#'
#' @return The range (max - min) with sample size information,
#'   including the effective sample size (effective N) when weights are provided,
#'   and the number of valid observations used.
#'
#' @details
#' ## Understanding the Results
#'
#' - **Range**: The difference between the largest and smallest values. A large
#'   range indicates that at least some values are far apart.
#' - **Effective N**: Reported when weights are provided, for consistency with
#'   other weighted statistics.
#' - **N**: The actual number of observations used.
#'
#' Note: The range is sensitive to outliers. A single extreme value can
#' dramatically increase the range. Consider using \code{\link{w_iqr}} for a
#' more robust measure of spread.
#'
#' ## When to Use This
#'
#' Use \code{w_range()} when:
#' - You want a quick overview of the total spread of your data
#' - You need to check for data entry errors (impossible values)
#' - You want to compare the total spread across groups
#'
#' ## Formula
#'
#' \eqn{Range = \max(x) - \min(x)}
#'
#' Note: This statistic is weight-invariant. The minimum and maximum observed
#' values do not change when weights are applied.
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
#' @seealso
#' \code{\link[base]{range}} for the base R range function.
#'
#' \code{\link{w_iqr}} for the weighted interquartile range (more robust).
#'
#' \code{\link{w_sd}} for weighted standard deviation (another spread measure).
#'
#' \code{\link{describe}} for comprehensive descriptive statistics including range.
#'
#' @references
#' IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.
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
