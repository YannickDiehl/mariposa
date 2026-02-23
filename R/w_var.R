#' Calculate Population-Representative Variance
#'
#' @description
#' \code{w_var()} calculates variance that accurately represents your population
#' by using survey weights. Variance measures how far values spread out from the
#' average -- it is the square of the standard deviation. A larger variance means
#' more dispersion in your population's responses.
#'
#' Without weights, you describe the spread in your sample only. With weights, you
#' estimate how spread out values are in the entire population.
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The numeric variables you want to analyze. You can list multiple
#'   variables or use helpers like \code{starts_with("trust")}
#' @param weights Survey weights to make results representative of your population.
#'   Without weights, you get the simple sample variance.
#' @param na.rm Remove missing values before calculating? (Default: TRUE)
#'
#' @return Population-weighted variance(s) with sample size information,
#'   including the weighted variance, effective sample size (effective N), and the
#'   number of valid observations used.
#'
#' @details
#' ## Understanding the Results
#'
#' - **Weighted Variance**: The population-representative variance. Because
#'   variance is in squared units (e.g., years-squared for age), it is often
#'   easier to interpret the standard deviation (\code{\link{w_sd}}) instead.
#' - **Effective N**: How many independent observations your weighted data
#'   represents. Always less than or equal to the actual sample size.
#' - **N**: The actual number of observations used in the calculation.
#'
#' ## When to Use This
#'
#' Use \code{w_var()} when:
#' - You need variance for further calculations (e.g., pooled variance, F-tests)
#' - You want to compare variability across groups with proper weighting
#' - Your analysis formulas require variance rather than SD
#' - You need SPSS-compatible weighted variance values
#'
#' For reporting purposes, \code{\link{w_sd}} is usually preferred because
#' the standard deviation is in the same units as the original variable.
#'
#' ## Formula
#'
#' The weighted variance uses the SPSS frequency weights formula:
#'
#' \eqn{s^2_w = \frac{\sum w_i (x_i - \bar{x}_w)^2}{V_1 - 1}}
#'
#' where \eqn{V_1 = \sum w_i} is the sum of all weights and
#' \eqn{\bar{x}_w = \sum w_i x_i / V_1} is the weighted mean.
#'
#' Note: \eqn{s^2_w = (s_w)^2}, i.e., the weighted variance is the square of
#' the weighted standard deviation.
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
#' @seealso
#' \code{\link[stats]{var}} for the base R variance function.
#'
#' \code{\link{w_sd}} for weighted standard deviation (the square root of variance).
#'
#' \code{\link{w_mean}} for weighted means.
#'
#' \code{\link{describe}} for comprehensive descriptive statistics including variance.
#'
#' @references
#' IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.
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
