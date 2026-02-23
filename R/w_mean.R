#' Calculate Population-Representative Averages
#'
#' @description
#' \code{w_mean()} calculates averages that accurately represent your population
#' by using survey weights. This ensures that groups who were over- or under-sampled
#' contribute appropriately to the final average.
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The numeric variables you want to average. You can list multiple
#'   variables or use helpers like \code{starts_with("income")}
#' @param weights Survey weights to make the average representative of your population.
#'   Without weights, you get the simple sample average.
#' @param na.rm Remove missing values before calculating? (Default: TRUE)
#'
#' @return Population-weighted average(s) with sample size information
#'
#' @details
#' ## When to Use This
#'
#' Use \code{w_mean()} when your survey uses sampling weights and you need
#' population-representative averages. Weights correct for:
#' - Oversampling of certain groups (weights < 1)
#' - Undersampling of other groups (weights > 1)
#' - Non-response patterns
#' - Complex survey designs
#'
#' ## Understanding the Results
#'
#' - **Weighted Mean**: The population-representative average
#' - **Effective N**: How many independent observations your weighted data represents
#' - **N**: Actual number of observations used
#'
#' ## Formula
#'
#' \eqn{\bar{x}_w = \frac{\sum w_i \cdot x_i}{\sum w_i}}
#'
#' The effective sample size is: \eqn{n_{eff} = (\sum w_i)^2 / \sum w_i^2}
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Basic weighted usage
#' survey_data %>% w_mean(age, weights = sampling_weight)
#'
#' # Multiple variables
#' survey_data %>% w_mean(age, income, life_satisfaction, weights = sampling_weight)
#'
#' # Grouped data
#' survey_data %>% group_by(region) %>% w_mean(age, weights = sampling_weight)
#'
#' # In summarise context
#' survey_data %>% summarise(mean_age = w_mean(age, weights = sampling_weight))
#'
#' # Unweighted (for comparison)
#' survey_data %>% w_mean(age)
#'
#' @seealso
#' \code{\link[stats]{weighted.mean}} for the base R weighted mean function.
#'
#' \code{\link{w_sd}} for weighted standard deviation.
#'
#' \code{\link{w_median}} for weighted median.
#'
#' \code{\link{describe}} for comprehensive descriptive statistics.
#'
#' @references
#' IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.
#'
#' @family weighted_statistics
#' @export
w_mean <- function(data, ..., weights = NULL, na.rm = TRUE) {
  .w_statistic(
    data, ...,
    weights = {{ weights }},
    na.rm = na.rm,
    stat_fn = function(x, w) {
      if (is.null(w)) {
        mean(x, na.rm = FALSE)  # NA already removed by factory
      } else {
        sum(x * w) / sum(w)
      }
    },
    stat_name = "mean",
    weighted_col = "weighted_mean",
    unweighted_col = "mean",
    class_name = "w_mean"
  )
}

#' @export
#' @method print w_mean
print.w_mean <- function(x, digits = 3, ...) {
  .print_w_statistic(x, "Mean", "weighted_mean", "mean", digits)
}
