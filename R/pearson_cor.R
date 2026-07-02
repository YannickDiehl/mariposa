#' Measure How Strongly Variables Are Related
#'
#' @description
#' \code{pearson_cor()} shows you how strongly numeric variables are related to each
#' other. For example, is age related to income? Does satisfaction increase with
#' experience? This helps you understand patterns in your data.
#'
#' The correlation tells you:
#' - **Direction**: Positive (both increase together) or negative (one increases as other decreases)
#' - **Strength**: How closely the variables move together (from 0 = no relationship to 1 = perfect relationship)
#' - **Significance**: Whether the relationship is real or could be due to chance
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The numeric variables you want to correlate. List two for a single
#'   correlation or more for a correlation matrix.
#' @param weights Optional survey weights for population-representative
#'   results. Following SPSS CORRELATIONS, the weighted degrees of freedom
#'   use \code{n = sum(w)} (not Kish's effective sample size). This is
#'   appropriate for normalized survey weights with mean \eqn{\approx 1}.
#'   For raw expansion weights (e.g., summing to millions of population
#'   units), the resulting standard errors and confidence intervals will
#'   be drastically too narrow — in that case, normalize weights so that
#'   \code{sum(w) == n}, or use the survey package for design-based inference.
#' @param conf.level Confidence level for intervals (Default: 0.95 = 95%)
#' @param alternative Direction of the test: \code{"two.sided"} (default),
#'   \code{"less"}, or \code{"greater"}.
#' @param use How to handle missing values:
#'   \itemize{
#'     \item \code{"pairwise"} (default): Use all available data for each pair
#'     \item \code{"listwise"}: Only use complete cases across all variables
#'   }
#' @param na.rm Deprecated. Use \code{use} instead.
#'
#' @return Correlation results showing relationships between variables, including:
#' - Correlation coefficient (r): Strength and direction of relationship
#' - P-value: Whether the relationship is statistically significant
#' - Confidence interval: Range of plausible correlation values
#' - Sample size: Number of observations used
#'   Use \code{summary()} for the full SPSS-style output with toggleable sections.
#'
#' @details
#' ## Understanding the Results
#'
#' **Correlation coefficient (r)** ranges from -1 to +1:
#' - **+1**: Perfect positive relationship (as one goes up, the other always goes up)
#' - **0**: No linear relationship
#' - **-1**: Perfect negative relationship (as one goes up, the other always goes down)
#'
#' **Interpreting strength** (absolute value of r):
#' - 0.00 - 0.10: Negligible relationship
#' - 0.10 - 0.30: Weak relationship
#' - 0.30 - 0.50: Moderate relationship
#' - 0.50 - 0.70: Strong relationship
#' - 0.70 - 0.90: Very strong relationship
#' - 0.90 - 1.00: Extremely strong relationship
#'
#' **P-value interpretation**:
#' - p < 0.001: Very strong evidence of a relationship
#' - p < 0.01: Strong evidence of a relationship
#' - p < 0.05: Moderate evidence of a relationship
#' - p >= 0.05: No significant relationship found
#'
#' A correlation of 0.65 with p < 0.001 means:
#' - Strong positive relationship (r = 0.65)
#' - As one variable increases, the other tends to increase
#' - Very unlikely to be due to chance (p < 0.001)
#' - About 42% of variation is shared (r-squared = 0.65 squared = 0.42)
#'
#' ## When to Use This
#'
#' Use Pearson correlation when:
#' - Both variables are numeric and continuous
#' - You expect a linear relationship
#' - Data is roughly normally distributed
#' - You want to measure strength of linear association
#'
#' Don't use when:
#' - Data has extreme outliers (consider Spearman instead)
#' - Relationship is curved/non-linear
#' - Variables are categorical (use chi-squared test)
#' - You need to establish causation (correlation does not imply causation)
#'
#' ## Tips for Success
#'
#' - Always plot your data first to check for non-linear patterns
#' - Consider both statistical significance (p-value) and practical importance (r value)
#' - Remember: correlation does not imply causation
#' - Check for outliers that might inflate or deflate correlations
#' - Use Spearman correlation for ordinal data or non-normal distributions
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Basic correlation between two variables
#' survey_data %>%
#'   pearson_cor(age, income)
#'
#' # Correlation matrix for multiple variables
#' survey_data %>%
#'   pearson_cor(age, income, life_satisfaction)
#'
#' # Weighted correlations
#' survey_data %>%
#'   pearson_cor(age, income, weights = sampling_weight)
#'
#' # Grouped correlations
#' survey_data %>%
#'   group_by(region) %>%
#'   pearson_cor(age, income, life_satisfaction)
#'
#' # Using tidyselect helpers
#' survey_data %>%
#'   pearson_cor(where(is.numeric), weights = sampling_weight)
#'
#' # Listwise deletion for missing data
#' survey_data %>%
#'   pearson_cor(age, income, use = "listwise")
#'
#' # --- Three-layer output ---
#' result <- survey_data %>%
#'   pearson_cor(age, income, life_satisfaction, weights = sampling_weight)
#' result              # compact one-line overview
#' summary(result)     # full correlation, p-value, and N matrices
#' summary(result, pvalue_matrix = FALSE)  # hide p-values
#'
#' @seealso
#' \code{\link[stats]{cor}} for the base R correlation function.
#'
#' \code{\link[stats]{cor.test}} for correlation significance testing.
#'
#' \code{\link{spearman_rho}} for rank-based correlation (robust to outliers).
#'
#' \code{\link{kendall_tau}} for ordinal correlation.
#'
#' \code{\link{summary.pearson_cor}} for detailed output with toggleable sections.
#'
#' @references
#' Cohen, J. (1988). \emph{Statistical Power Analysis for the Behavioral
#' Sciences} (2nd ed.). Lawrence Erlbaum Associates.
#'
#' Fisher, R. A. (1915). Frequency distribution of the values of the
#' correlation coefficient in samples from an indefinitely large population.
#' \emph{Biometrika}, 10(4), 507--521.
#'
#' @family correlation
#' @export
pearson_cor <- function(data, ..., weights = NULL, conf.level = 0.95,
                        alternative = c("two.sided", "less", "greater"),
                        use = c("pairwise", "listwise"), na.rm = NULL) {
  .correlate(
    data, ...,
    weights = rlang::enquo(weights),
    alternative = alternative,
    use = use,
    na.rm = na.rm,
    conf.level = conf.level,
    pair_fn = function(x, y, w, alternative) {
      .pearson_pair(x, y, w, conf.level, alternative)
    },
    spec = .pearson_cor_spec
  )
}

#' Calculate one weighted/unweighted Pearson correlation pair
#' @noRd
.pearson_pair <- function(x, y, w = NULL, conf.level = 0.95, alternative = "two.sided") {
  if (!is.null(w)) {
    # Remove missing values
    valid <- !is.na(x) & !is.na(y) & !is.na(w) & w > 0
    x <- x[valid]
    y <- y[valid]
    w <- w[valid]

    n <- length(x)
    if (n < 3) {
      return(list(
        correlation = NA_real_,
        p_value = NA_real_,
        conf_int = c(NA_real_, NA_real_),
        n = n,
        df = NA_integer_
      ))
    }

    # Calculate weighted means
    mean_x <- sum(w * x) / sum(w)
    mean_y <- sum(w * y) / sum(w)

    # Calculate weighted covariance and variances
    cov_xy <- sum(w * (x - mean_x) * (y - mean_y)) / sum(w)
    var_x <- sum(w * (x - mean_x)^2) / sum(w)
    var_y <- sum(w * (y - mean_y)^2) / sum(w)

    # Calculate weighted correlation
    r <- cov_xy / sqrt(var_x * var_y)

    # SPSS compatibility: use sum of weights for significance testing
    n_eff <- sum(w)  # SPSS uses sum of weights for df calculation

  } else {
    # Unweighted correlation
    valid <- !is.na(x) & !is.na(y)
    x <- x[valid]
    y <- y[valid]

    n <- length(x)
    if (n < 3) {
      return(list(
        correlation = NA_real_,
        p_value = NA_real_,
        conf_int = c(NA_real_, NA_real_),
        n = n,
        df = NA_integer_
      ))
    }

    r <- cor(x, y, method = "pearson")
    n_eff <- n
  }

  # Undefined correlation (zero variance in x or y, as with a constant
  # variable): return NA instead of crashing on the abs(r) check below.
  # SPSS leaves the cell blank in this situation.
  if (is.na(r)) {
    return(list(
      correlation = NA_real_,
      p_value = NA_real_,
      conf_int = c(NA_real_, NA_real_),
      n = if (!is.null(w)) round(n_eff) else n,
      df = NA_integer_
    ))
  }

  # Ensure correlation is within valid range
  r <- max(-1, min(1, r))

  # Calculate t-statistic for significance test
  df <- n_eff - 2
  if (abs(r) == 1) {
    t_stat <- if (r > 0) Inf else -Inf
    p_value <- 0
  } else {
    t_stat <- r * sqrt(df / (1 - r^2))
    p_value <- switch(alternative,
      "two.sided" = 2 * pt(-abs(t_stat), df),
      "less" = pt(t_stat, df),
      "greater" = pt(t_stat, df, lower.tail = FALSE)
    )
  }

  # Calculate confidence interval using Fisher's z transformation
  if (abs(r) < 1) {
    z <- 0.5 * log((1 + r) / (1 - r))
    se_z <- 1 / sqrt(n_eff - 3)
    z_crit <- qnorm((1 + conf.level) / 2)

    z_lower <- z - z_crit * se_z
    z_upper <- z + z_crit * se_z

    # Transform back to correlation scale
    ci_lower <- (exp(2 * z_lower) - 1) / (exp(2 * z_lower) + 1)
    ci_upper <- (exp(2 * z_upper) - 1) / (exp(2 * z_upper) + 1)
  } else {
    ci_lower <- r
    ci_upper <- r
  }

  # For weighted correlations, report sum of weights as N (SPSS compatibility)
  # For unweighted, report actual sample size
  n_report <- if (!is.null(w)) round(n_eff) else n

  return(list(
    correlation = r,
    p_value = p_value,
    conf_int = c(ci_lower, ci_upper),
    n = n_report,
    df = df
  ))
}

#' Engine spec for pearson_cor (see R/correlation-engine.R)
#' @noRd
.pearson_cor_spec <- list(
  class_name = "pearson_cor",
  result_names = c("correlations", "n_obs", "matrices", "variables", "weights",
                   "conf.level", "use", "alternative", "is_grouped", "groups",
                   "group_keys"),
  matrices = list(
    correlations = list(init = NA_real_,    diag = 1),
    p_values     = list(init = NA_real_,    diag = 0),
    n_obs        = list(init = NA_integer_, diag = "n"),
    ci_lower     = list(init = NA_real_,    diag = 1),
    ci_upper     = list(init = NA_real_,    diag = 1)
  ),
  extract = function(res) {
    list(correlations = res$correlation, p_values = res$p_value,
         n_obs = res$n, ci_lower = res$conf_int[1], ci_upper = res$conf_int[2])
  },
  diag_n = "weight_sum",
  df_source = "matrices",
  df_cols = c(correlation = "correlations", p_value = "p_values",
              conf_int_lower = "ci_lower", conf_int_upper = "ci_upper",
              n = "n_obs"),
  sig_inline = FALSE,
  name_rows = FALSE,
  group_cols = "front",
  finalize_df = function(df) {
    # Add significance indicators
    df$sig <- add_significance_stars(df$p_value)
    # Calculate r-squared for effect size
    df$r_squared <- df$correlation^2
    df
  },
  # --- print/summary display ---
  compact_title = "Pearson Correlation",
  stat_col = "correlation",
  stat_label = "r",
  verbose_title = "Pearson Correlation",
  info = function(x) {
    list(
      "Weights variable" = x$weights,
      "Missing data handling" = paste(x$use, "deletion")
    )
  },
  params = function(x) list(conf.level = x$conf.level),
  pair_stat_prefix = "Correlation: r",
  pair_extras = function(corrs, digits) {
    # Always show CI and r-squared
    if ("conf_int_lower" %in% names(corrs)) {
      cat(sprintf("  95%% CI: [%.*f, %.*f]\n", digits, corrs$conf_int_lower[1],
                  digits, corrs$conf_int_upper[1]))
    }
    if ("r_squared" %in% names(corrs)) {
      cat(sprintf("  r-squared: %.*f\n", digits, corrs$r_squared[1]))
    }
  },
  matrix_key = "correlations",
  matrix_title = "Correlation Matrix:",
  p_title = function(x) "Significance Matrix (p-values):",
  pairwise_df = function(corrs, digits) {
    data.frame(
      Variable_Pair = paste(corrs$var1, "\u00d7", corrs$var2),
      r = round(corrs$correlation, digits),
      r_squared = round(corrs$r_squared, digits),
      p_value = round(as.numeric(corrs$p_value), 4),
      CI_95 = sprintf("[%.*f, %.*f]", digits, corrs$conf_int_lower,
                      digits, corrs$conf_int_upper),
      n = corrs$n,
      sig = corrs$sig,
      stringsAsFactors = FALSE
    )
  }
)

#' Print Pearson correlation results (compact)
#'
#' @description
#' Compact print method for objects of class \code{"pearson_cor"}.
#' Shows correlation coefficient, p-value, and sample size per pair.
#'
#' For the full detailed output including matrices, use \code{summary()}.
#'
#' @param x An object of class \code{"pearson_cor"} returned by
#'   \code{\link{pearson_cor}}.
#' @param digits Number of decimal places to display. Default is \code{3}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- pearson_cor(survey_data, age, life_satisfaction)
#' result              # compact one-line overview
#' summary(result)     # full correlation matrices
#'
#' @export
#' @method print pearson_cor
print.pearson_cor <- function(x, digits = 3, ...) {
  .print_cor_result(x, .pearson_cor_spec, digits)
}

#' Summary method for Pearson correlation results
#'
#' @description
#' Creates a summary object that produces detailed output when printed,
#' including correlation matrices, p-value matrices, and sample size matrices.
#'
#' @param object A \code{pearson_cor} result object.
#' @param correlation_matrix Logical. Show the correlation coefficient matrix? (Default: TRUE)
#' @param pvalue_matrix Logical. Show the p-value matrix? (Default: TRUE)
#' @param n_matrix Logical. Show the sample size matrix? (Default: TRUE)
#' @param digits Number of decimal places for formatting (Default: 3).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.pearson_cor} object.
#'
#' @examples
#' result <- pearson_cor(survey_data, trust_government, trust_media)
#' summary(result)
#' summary(result, pvalue_matrix = FALSE)
#'
#' @seealso \code{\link{pearson_cor}} for the main analysis function.
#' @export
#' @method summary pearson_cor
summary.pearson_cor <- function(object, correlation_matrix = TRUE,
                                pvalue_matrix = TRUE, n_matrix = TRUE,
                                digits = 3, ...) {
  build_summary_object(
    object     = object,
    show       = list(correlation_matrix = correlation_matrix,
                      pvalue_matrix = pvalue_matrix,
                      n_matrix = n_matrix),
    digits     = digits,
    class_name = "summary.pearson_cor"
  )
}

#' Print summary of Pearson correlation results (detailed output)
#'
#' @description
#' Displays the detailed SPSS-style output for a Pearson correlation, with
#' sections controlled by the boolean parameters passed to
#' \code{\link{summary.pearson_cor}}.  Sections include the correlation matrix,
#' p-value matrix, and sample size matrix.
#'
#' @param x A \code{summary.pearson_cor} object created by
#'   \code{\link{summary.pearson_cor}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- pearson_cor(survey_data, age, life_satisfaction)
#' summary(result)                             # all matrices
#' summary(result, pvalue_matrix = FALSE)      # hide p-values
#'
#' @seealso \code{\link{pearson_cor}} for the main analysis,
#'   \code{\link{summary.pearson_cor}} for summary options.
#' @export
#' @method print summary.pearson_cor
print.summary.pearson_cor <- function(x, ...) {
  .print_cor_summary(x, .pearson_cor_spec)
}
