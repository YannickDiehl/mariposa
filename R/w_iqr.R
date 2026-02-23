#' Measure Population-Representative Spread (IQR)
#'
#' @description
#' \code{w_iqr()} calculates the interquartile range using survey weights. The IQR
#' is the distance between the 25th and 75th percentiles -- it tells you the range
#' that contains the middle 50% of your population. Unlike the standard deviation,
#' the IQR is not affected by outliers, making it a robust measure of spread.
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The numeric variables you want to analyze. You can list multiple
#'   variables or use helpers like \code{starts_with("income")}
#' @param weights Survey weights to make results representative of your population.
#'   Without weights, you get the simple sample IQR.
#' @param na.rm Remove missing values before calculating? (Default: TRUE)
#'
#' @return Population-weighted IQR(s) with sample size information,
#'   including the weighted IQR, effective sample size (effective N), and the
#'   number of valid observations used.
#'
#' @details
#' ## Understanding the Results
#'
#' - **Weighted IQR**: The range that covers the middle 50% of the weighted
#'   population. A larger IQR means more spread in the central part of the data.
#' - **Effective N**: How many independent observations your weighted data
#'   represents.
#' - **N**: The actual number of observations used.
#'
#' The IQR is especially useful when your data is skewed. For example, with
#' income data, the IQR gives a better sense of "typical spread" than the SD
#' because extreme incomes do not distort it.
#'
#' ## When to Use This
#'
#' Use \code{w_iqr()} when:
#' - Your data has outliers or is skewed (e.g., income, response times)
#' - You want a robust measure of spread that is not influenced by extremes
#' - You need to describe the spread of the middle 50% of your population
#' - You need SPSS-compatible weighted IQR values
#'
#' ## Formula
#'
#' \eqn{IQR_w = Q_{3,w} - Q_{1,w}}
#'
#' where \eqn{Q_{1,w}} and \eqn{Q_{3,w}} are the weighted 25th and 75th
#' percentiles, calculated using cumulative weights (see \code{\link{w_quantile}}).
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
#' @seealso
#' \code{\link[stats]{IQR}} for the base R IQR function.
#'
#' \code{\link{w_quantile}} for arbitrary weighted percentiles.
#'
#' \code{\link{w_sd}} for weighted standard deviation (another spread measure).
#'
#' \code{\link{w_range}} for the full weighted range.
#'
#' \code{\link{describe}} for comprehensive descriptive statistics including IQR.
#'
#' @references
#' IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.
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
