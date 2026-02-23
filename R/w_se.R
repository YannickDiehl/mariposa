#' Calculate Population-Representative Standard Errors
#'
#' @description
#' \code{w_se()} calculates the standard error of the mean using survey weights.
#' The standard error tells you how precisely you have estimated the population
#' mean -- a smaller SE means your estimate is more precise. This is essential
#' for constructing confidence intervals and assessing the reliability of your
#' weighted mean estimates.
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The numeric variables you want to analyze. You can list multiple
#'   variables or use helpers like \code{starts_with("trust")}
#' @param weights Survey weights to make results representative of your population.
#'   Without weights, you get the simple sample standard error.
#' @param na.rm Remove missing values before calculating? (Default: TRUE)
#'
#' @return Population-weighted standard error(s) with sample size information,
#'   including the weighted SE, effective sample size (effective N), and the
#'   number of valid observations used.
#'
#' @details
#' ## Understanding the Results
#'
#' - **Weighted SE**: The precision of your weighted mean estimate. Smaller
#'   values mean more precise estimates. You can build a 95% confidence
#'   interval as: weighted mean +/- 1.96 * weighted SE.
#' - **Effective N**: How many independent observations your weighted data
#'   represents. Weights that vary a lot reduce effective N, increasing the SE.
#' - **N**: The actual number of observations used.
#'
#' ## When to Use This
#'
#' Use \code{w_se()} when:
#' - You need to report precision of mean estimates
#' - You want to construct confidence intervals for weighted means
#' - You need to compare precision across subgroups
#' - You need SPSS-compatible weighted standard errors
#'
#' ## Formula
#'
#' The weighted standard error is calculated as:
#'
#' \eqn{SE_w = \frac{s_w}{\sqrt{V_1}}}
#'
#' where \eqn{s_w} is the weighted standard deviation (see \code{\link{w_sd}})
#' and \eqn{V_1 = \sum w_i} is the sum of all weights.
#'
#' For the unweighted case: \eqn{SE = s / \sqrt{n}}
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
#' @seealso
#' \code{\link{w_sd}} for weighted standard deviation.
#'
#' \code{\link{w_mean}} for weighted means.
#'
#' \code{\link{describe}} for comprehensive descriptive statistics including SE.
#'
#' @references
#' IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.
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
