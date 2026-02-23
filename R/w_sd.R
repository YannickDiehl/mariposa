#' Calculate Population-Representative Standard Deviations
#'
#' @description
#' \code{w_sd()} calculates standard deviations that accurately represent your
#' population by using survey weights. The standard deviation tells you how spread
#' out your data is around the average -- a larger SD means more variation in
#' responses, while a smaller SD means responses cluster tightly around the mean.
#'
#' Without weights, you describe spread in your sample only. With weights, you
#' estimate how spread out values are in the entire population.
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The numeric variables you want to analyze. You can list multiple
#'   variables or use helpers like \code{starts_with("trust")}
#' @param weights Survey weights to make results representative of your population.
#'   Without weights, you get the simple sample standard deviation.
#' @param na.rm Remove missing values before calculating? (Default: TRUE)
#'
#' @return Population-weighted standard deviation(s) with sample size information,
#'   including the weighted SD, effective sample size (effective N), and the
#'   number of valid observations used.
#'
#' @details
#' ## Understanding the Results
#'
#' - **Weighted SD**: The population-representative standard deviation. Roughly
#'   68% of your population falls within one SD of the weighted mean.
#' - **Effective N**: How many independent observations your weighted data
#'   represents. Always less than or equal to the actual sample size.
#' - **N**: The actual number of observations used in the calculation.
#'
#' A large difference between weighted and unweighted SD suggests that the
#' variability in your sample does not accurately reflect the population.
#'
#' ## When to Use This
#'
#' Use \code{w_sd()} when:
#' - You need to report how spread out a variable is in the population
#' - You want to compare variability across groups with proper weighting
#' - Your survey used complex sampling (oversampling, stratification)
#' - You need SPSS-compatible weighted standard deviations
#'
#' ## Formula
#'
#' The weighted standard deviation uses the SPSS frequency weights formula:
#'
#' \eqn{s_w = \sqrt{\frac{\sum w_i (x_i - \bar{x}_w)^2}{V_1 - 1}}}
#'
#' where \eqn{V_1 = \sum w_i} is the sum of all weights and
#' \eqn{\bar{x}_w = \sum w_i x_i / V_1} is the weighted mean.
#'
#' The effective sample size is: \eqn{n_{eff} = (\sum w_i)^2 / \sum w_i^2}
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
#' @seealso
#' \code{\link[stats]{sd}} for the base R standard deviation function.
#'
#' \code{\link{w_var}} for weighted variance (the square of weighted SD).
#'
#' \code{\link{w_mean}} for weighted means.
#'
#' \code{\link{describe}} for comprehensive descriptive statistics including SD.
#'
#' @references
#' IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.
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
