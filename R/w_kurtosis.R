#' Measure Population-Representative Kurtosis
#'
#' @description
#' \code{w_kurtosis()} measures how heavy the tails of your data's distribution
#' are, using survey weights for population-representative results. Kurtosis tells
#' you whether your data has unusually many extreme values (outliers) compared to
#' a normal distribution:
#' - **Positive (excess) kurtosis**: Heavier tails than normal (more outliers)
#' - **Negative (excess) kurtosis**: Lighter tails than normal (fewer outliers)
#' - **Near zero**: Similar tail behavior to a normal distribution
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The numeric variables you want to analyze. You can list multiple
#'   variables or use helpers like \code{starts_with("trust")}
#' @param weights Survey weights to make results representative of your population.
#'   Without weights, you get the simple sample kurtosis.
#' @param na.rm Remove missing values before calculating? (Default: TRUE)
#' @param excess Show excess kurtosis? (Default: TRUE, matching SPSS). Excess
#'   kurtosis subtracts 3 so that a normal distribution has kurtosis of 0,
#'   making interpretation easier.
#'
#' @return Population-weighted kurtosis value(s) with sample size information,
#'   including the weighted kurtosis, effective sample size (effective N), and the
#'   number of valid observations used.
#'
#' @details
#' ## Understanding the Results
#'
#' - **Weighted Kurtosis** (excess, default):
#'   - Near 0: Tail behavior similar to a normal distribution
#'   - Positive (> 0): Heavier tails, more extreme values than normal
#'   - Negative (< 0): Lighter tails, fewer extreme values than normal
#'   - Values beyond 2 or below -2 indicate notable departure from normality
#' - **Effective N**: How many independent observations your weighted data
#'   represents.
#' - **N**: The actual number of observations used.
#'
#' Kurtosis is often checked together with skewness to assess normality. Both
#' should be close to zero for normally distributed data.
#'
#' ## When to Use This
#'
#' Use \code{w_kurtosis()} when:
#' - You need to assess whether a variable follows a normal distribution
#' - You want to check for unusually many outliers
#' - You are deciding whether parametric tests are appropriate
#' - You need SPSS-compatible weighted kurtosis values
#'
#' ## Formula
#'
#' Uses the SPSS Type 2 (sample-corrected) excess kurtosis formula. First, the
#' population excess kurtosis (\eqn{g_2}) is calculated:
#'
#' \eqn{g_2 = \frac{m_4}{m_2^2} - 3}
#'
#' where \eqn{m_2 = \sum w_i(x_i - \bar{x}_w)^2 / V_1} and
#' \eqn{m_4 = \sum w_i(x_i - \bar{x}_w)^4 / V_1} with \eqn{V_1 = \sum w_i}.
#'
#' Then, the bias-corrected (Type 2) kurtosis is:
#'
#' \eqn{G_2 = \frac{(n+1) \cdot g_2 + 6}{(n-2)(n-3)} \cdot (n-1)}
#'
#' where \eqn{n = V_1} for weighted data.
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
#' @seealso
#' \code{\link{w_skew}} for weighted skewness (distribution asymmetry).
#'
#' \code{\link{describe}} for comprehensive descriptive statistics including kurtosis.
#'
#' @references
#' Joanes, D. N., & Gill, C. A. (1998). Comparing measures of sample skewness
#' and kurtosis. The Statistician, 47(1), 183-189.
#'
#' IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.
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
