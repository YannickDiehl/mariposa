#' Measure Population-Representative Skewness
#'
#' @description
#' \code{w_skew()} measures how asymmetric your data's distribution is, using
#' survey weights for population-representative results. Skewness tells you
#' whether values tend to trail off more to the left or right:
#' - **Positive skew**: A long tail to the right (e.g., income -- many low, few very high)
#' - **Negative skew**: A long tail to the left (e.g., exam scores -- many high, few very low)
#' - **Near zero**: Roughly symmetric (e.g., height in a population)
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The numeric variables you want to analyze. You can list multiple
#'   variables or use helpers like \code{starts_with("trust")}
#' @param weights Survey weights to make results representative of your population.
#'   Without weights, you get the simple sample skewness.
#' @param na.rm Remove missing values before calculating? (Default: TRUE)
#'
#' @return Population-weighted skewness value(s) with sample size information,
#'   including the weighted skewness, effective sample size (effective N), and the
#'   number of valid observations used.
#'
#' @details
#' ## Understanding the Results
#'
#' - **Weighted Skewness**: The population-representative skewness value.
#'   - Near 0: Distribution is roughly symmetric
#'   - Between -0.5 and 0.5: Approximately symmetric
#'   - Between -1 and -0.5 or 0.5 and 1: Moderately skewed
#'   - Beyond -1 or 1: Highly skewed
#' - **Effective N**: How many independent observations your weighted data
#'   represents.
#' - **N**: The actual number of observations used.
#'
#' High skewness suggests you might want to use the median instead of the mean
#' as a measure of center, and non-parametric tests instead of t-tests.
#'
#' ## When to Use This
#'
#' Use \code{w_skew()} when:
#' - You need to check if a variable is normally distributed
#' - You want to decide between parametric and non-parametric tests
#' - You need to assess whether the mean is a good summary measure
#' - You need SPSS-compatible weighted skewness values
#'
#' ## Formula
#'
#' Uses the SPSS Type 2 (sample-corrected) skewness formula. First, the
#' population skewness (\eqn{g_1}) is calculated:
#'
#' \eqn{g_1 = \frac{m_3}{m_2^{3/2}}}
#'
#' where \eqn{m_2 = \sum w_i(x_i - \bar{x}_w)^2 / V_1} and
#' \eqn{m_3 = \sum w_i(x_i - \bar{x}_w)^3 / V_1} with \eqn{V_1 = \sum w_i}.
#'
#' Then, the bias-corrected (Type 2) skewness is:
#'
#' \eqn{G_1 = g_1 \cdot \frac{\sqrt{n(n-1)}}{n-2}}
#'
#' where \eqn{n = V_1} for weighted data.
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
#' @seealso
#' \code{\link{w_kurtosis}} for weighted kurtosis (tail heaviness).
#'
#' \code{\link{describe}} for comprehensive descriptive statistics including skewness.
#'
#' @references
#' Joanes, D. N., & Gill, C. A. (1998). Comparing measures of sample skewness
#' and kurtosis. The Statistician, 47(1), 183-189.
#'
#' IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.
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
