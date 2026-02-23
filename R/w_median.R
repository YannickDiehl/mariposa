#' Find the Population-Representative Middle Value
#'
#' @description
#' \code{w_median()} finds the median (middle value) of your data using survey
#' weights. The weighted median is the value where half the population falls below
#' and half falls above. Unlike the mean, the median is not pulled by extreme
#' values, making it a robust measure of the "typical" value in your population.
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The numeric variables you want to analyze. You can list multiple
#'   variables or use helpers like \code{starts_with("income")}
#' @param weights Survey weights to make results representative of your population.
#'   Without weights, you get the simple sample median.
#' @param na.rm Remove missing values before calculating? (Default: TRUE)
#'
#' @return Population-weighted median(s) with sample size information,
#'   including the weighted median, effective sample size (effective N), and the
#'   number of valid observations used.
#'
#' @details
#' ## Understanding the Results
#'
#' - **Weighted Median**: The value that splits the weighted population in half.
#'   50% of the population (by weight) falls below this value, 50% above.
#' - **Effective N**: How many independent observations your weighted data
#'   represents.
#' - **N**: The actual number of observations used.
#'
#' Comparing the weighted median to the weighted mean is informative:
#' - If they are similar, the distribution is roughly symmetric.
#' - If the mean is much larger than the median, the distribution is
#'   right-skewed (a few very high values pull the mean up, e.g., income).
#'
#' ## When to Use This
#'
#' Use \code{w_median()} when:
#' - Your data has outliers or is skewed (e.g., income, housing prices)
#' - You want a robust "typical value" not influenced by extremes
#' - You need the weighted 50th percentile
#' - You need SPSS-compatible weighted median values
#'
#' ## Formula
#'
#' The weighted median is calculated using cumulative weights: observations are
#' sorted by value, weights are accumulated, and the median is the value where
#' the cumulative weight reaches 50% of the total weight. Linear interpolation
#' is used when the 50% point falls between two observations.
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
#' @seealso
#' \code{\link[stats]{median}} for the base R median function.
#'
#' \code{\link{w_mean}} for weighted means.
#'
#' \code{\link{w_quantile}} for arbitrary weighted percentiles.
#'
#' \code{\link{describe}} for comprehensive descriptive statistics including the median.
#'
#' @references
#' IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.
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
