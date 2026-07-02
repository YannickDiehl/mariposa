#' Kendall's Tau Correlation Analysis
#'
#' @description
#' Calculates Kendall's tau-b rank correlation coefficients between variables with support
#' for weighted correlations, grouped data, and multiple variable pairs. Provides
#' significance testing and SPSS-compatible output formatting.
#'
#' The function computes tau-b, which is adjusted for ties and is particularly suitable
#' for ordinal data or when the assumptions of Pearson correlation are not met. For weighted
#' correlations, it uses survey-weighted rank calculations.
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The variables you want to correlate. List two for a single
#'   correlation or more for a correlation matrix. You can use helpers like
#'   \code{starts_with("trust")}.
#' @param weights Optional survey weights for population-representative results.
#' @param alternative Direction of the test:
#'   \itemize{
#'     \item \code{"two.sided"} (default): Two-tailed test
#'     \item \code{"less"}: One-tailed test (negative correlation)
#'     \item \code{"greater"}: One-tailed test (positive correlation)
#'   }
#' @param use How to handle missing values:
#'   \itemize{
#'     \item \code{"pairwise"} (default): Pairwise deletion - each correlation uses all available cases
#'     \item \code{"listwise"}: Listwise deletion - only complete cases across all variables
#'   }
#' @param na.rm Deprecated. Use \code{use} instead.
#'
#' @return Correlation results showing rank-based relationships between variables,
#'   including the tau-b coefficient, p-value, z-score, and sample size for each
#'   pair. For multiple variables, correlation, significance, and sample size
#'   matrices are also provided.
#'   Use \code{summary()} for the full SPSS-style output with toggleable sections.
#'
#' @details
#' ## Understanding the Results
#'
#' Kendall's tau measures how often pairs of observations are in the same order
#' (concordant) versus different order (discordant). It is particularly useful for
#' ordinal data, data with outliers, small sample sizes, and non-linear but
#' monotonic relationships.
#'
#' The tau value ranges from -1 to +1:
#' - **Strong positive** (0.5 to 1.0): High values of one variable tend to go with high values of the other
#' - **Moderate positive** (0.3 to 0.5): Some tendency for values to increase together
#' - **Weak positive** (0 to 0.3): Slight tendency for values to increase together
#' - **No correlation** (near 0): No relationship between the variables
#' - **Negative values**: As one variable increases, the other tends to decrease
#'
#' The output also provides:
#' - **p-value**: Probability of seeing this correlation by chance (smaller = stronger evidence)
#' - **n**: Number of observations used
#' - **significance stars**: Quick visual indicator of statistical significance
#'
#' ## When to Use This
#'
#' Choose Kendall's tau when:
#' - Your data is ordinal (ranked categories)
#' - You have a small sample size (< 30 observations)
#' - Your data has outliers that might affect Pearson correlation
#' - You want a more conservative measure than Spearman's rho
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Basic correlation between two variables
#' survey_data %>%
#'   kendall_tau(life_satisfaction, political_orientation)
#'
#' # Correlation matrix for multiple variables
#' survey_data %>%
#'   kendall_tau(life_satisfaction, political_orientation, trust_media)
#'
#' # Weighted correlations
#' survey_data %>%
#'   kendall_tau(age, income, weights = sampling_weight)
#'
#' # Listwise deletion for missing data
#' survey_data %>%
#'   kendall_tau(age, income, use = "listwise")
#'
#' # One-tailed test
#' survey_data %>%
#'   kendall_tau(age, income, alternative = "greater")
#'
#' \donttest{
#' # Grouped correlations
#' survey_data %>%
#'   group_by(region) %>%
#'   kendall_tau(age, income, life_satisfaction)
#'
#' # Using tidyselect helpers for ordinal variables
#' survey_data %>%
#'   kendall_tau(starts_with("trust"), weights = sampling_weight)
#'
#' # --- Three-layer output ---
#' result <- survey_data %>%
#'   kendall_tau(life_satisfaction, political_orientation, trust_media,
#'               weights = sampling_weight)
#' result              # compact one-line overview
#' summary(result)     # full correlation, p-value, and N matrices
#' summary(result, pvalue_matrix = FALSE)  # hide p-values
#' }
#'
#' @seealso
#' \code{\link[stats]{cor}} with \code{method = "kendall"} for the base R
#' implementation.
#'
#' \code{\link{spearman_rho}} for Spearman's rank correlation.
#'
#' \code{\link{pearson_cor}} for Pearson correlation analysis.
#'
#' \code{\link{summary.kendall_tau}} for detailed output with toggleable sections.
#'
#' @references
#' Kendall, M. G. (1938). A new measure of rank correlation.
#' \emph{Biometrika}, 30(1/2), 81--93.
#'
#' Kendall, M. G. (1945). The treatment of ties in ranking problems.
#' \emph{Biometrika}, 33(3), 239--251.
#'
#' Agresti, A. (2010). \emph{Analysis of Ordinal Categorical Data} (2nd ed.).
#' John Wiley & Sons.
#'
#' @family correlation
#' @export
kendall_tau <- function(data, ..., weights = NULL,
                        alternative = c("two.sided", "less", "greater"),
                        use = c("pairwise", "listwise"), na.rm = NULL) {
  .correlate(
    data, ...,
    weights = rlang::enquo(weights),
    alternative = alternative,
    use = use,
    na.rm = na.rm,
    pair_fn = .kendall_pair,
    spec = .kendall_tau_spec
  )
}

#' Calculate one weighted/unweighted Kendall's tau-b pair
#' @noRd
.kendall_pair <- function(x, y, w = NULL, alternative = "two.sided") {
  if (!is.null(w)) {
    # Remove missing values
    valid <- !is.na(x) & !is.na(y) & !is.na(w) & w > 0
    x <- x[valid]
    y <- y[valid]
    w <- w[valid]

    n <- length(x)
    if (n < 2) {
      return(list(
        tau = NA_real_,
        p_value = NA_real_,
        z_score = NA_real_,
        n = n
      ))
    }

    # Calculate weighted Kendall's tau
    # For weighted version, we weight each pair comparison
    concordant <- 0
    discordant <- 0
    ties_x <- 0
    ties_y <- 0
    ties_both <- 0
    total_weight <- 0

    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        # Weight for this pair
        pair_weight <- sqrt(w[i] * w[j])
        total_weight <- total_weight + pair_weight

        # Compare pairs
        diff_x <- x[i] - x[j]
        diff_y <- y[i] - y[j]

        if (diff_x != 0 && diff_y != 0) {
          if (sign(diff_x) == sign(diff_y)) {
            concordant <- concordant + pair_weight
          } else {
            discordant <- discordant + pair_weight
          }
        } else if (diff_x == 0 && diff_y == 0) {
          ties_both <- ties_both + pair_weight
        } else if (diff_x == 0) {
          ties_x <- ties_x + pair_weight
        } else {
          ties_y <- ties_y + pair_weight
        }
      }
    }

    # Calculate tau-b (adjusted for ties)
    # Pairs tied on BOTH variables (ties_both) must be excluded from each
    # factor, mirroring the unweighted (n0 - Tx - Txy) * (n0 - Ty - Txy)
    # denominator below. With weights == 1 this reduces exactly to the
    # unweighted tau-b.
    numerator <- concordant - discordant
    denominator <- sqrt((total_weight - ties_x - ties_both) *
                          (total_weight - ties_y - ties_both))

    if (denominator == 0) {
      # Undefined coefficient (a variable with no untied pairs); NA, not 0
      tau <- NA_real_
    } else {
      tau <- numerator / denominator
    }

    # For weighted analysis, use effective sample size
    n_eff <- sum(w)

  } else {
    # Unweighted Kendall's tau - SPSS-compatible calculation
    valid <- !is.na(x) & !is.na(y)
    x <- x[valid]
    y <- y[valid]

    n <- length(x)
    if (n < 2) {
      return(list(
        tau = NA_real_,
        p_value = NA_real_,
        z_score = NA_real_,
        n = n
      ))
    }

    # Manual calculation for SPSS compatibility
    # Count concordant, discordant, and tied pairs
    P <- 0  # concordant
    Q <- 0  # discordant
    Tx <- 0 # ties only in X
    Ty <- 0 # ties only in Y
    Txy <- 0 # ties in both

    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        dx <- x[i] - x[j]
        dy <- y[i] - y[j]

        if (dx == 0 && dy == 0) {
          Txy <- Txy + 1
        } else if (dx == 0) {
          Tx <- Tx + 1
        } else if (dy == 0) {
          Ty <- Ty + 1
        } else if (sign(dx) == sign(dy)) {
          P <- P + 1
        } else {
          Q <- Q + 1
        }
      }
    }

    # Total pairs
    n0 <- n * (n - 1) / 2

    # Kendall's tau-b formula (SPSS-compatible)
    denominator <- sqrt((n0 - Tx - Txy) * (n0 - Ty - Txy))

    if (denominator == 0) {
      # Undefined coefficient (a variable with no untied pairs); NA, not 0
      tau <- NA_real_
    } else {
      tau <- (P - Q) / denominator
    }

    n_eff <- n
  }

  # Undefined coefficient (e.g. constant variable): return NA row
  if (is.na(tau)) {
    return(list(
      tau = NA_real_,
      p_value = NA_real_,
      z_score = NA_real_,
      n = n_eff
    ))
  }

  # Ensure tau is within valid range
  tau <- max(-1, min(1, tau))

  # Calculate z-score for significance test using SPSS-compatible method
  if (!is.null(w)) {
    # Weighted: simplified no-ties approximation with n_eff = sum(w).
    # (The former small/large-n branch was algebraically identical:
    # 2*(2n+5) == 4n+10.)
    # NOTE: the weighted tau itself is exact (tie-corrected tau-b), but this
    # z score and the resulting p-value are APPROXIMATE - the variance omits
    # tie corrections and treats sum(w) as a sample size. Do not expect the
    # weighted z/p to equal the unweighted values at weights == 1.
    se_tau <- sqrt((4 * n_eff + 10) / (9 * n_eff * (n_eff - 1)))
  } else {
    # For unweighted, use SPSS-compatible variance calculation with tie corrections
    # Get frequency tables for tie corrections
    x_freq <- table(x)
    y_freq <- table(y)

    # Variance formula from Kendall & Gibbons (1990) - SPSS method
    if (n < 10) {
      # Small sample - use simpler approximation
      var_tau <- (4 * n + 10) / (9 * n * (n - 1))
    } else {
      # Large sample with tie correction
      # Var(S) per Kendall & Gibbons (1990), eq. 4.12 (same formula as
      # SPSS and stats::cor.test):
      #   Var(S) = [n(n-1)(2n+5) - sum t(t-1)(2t+5) - sum u(u-1)(2u+5)] / 18
      #          + [sum t(t-1)(t-2) * sum u(u-1)(u-2)] / [9n(n-1)(n-2)]
      #          + [sum t(t-1) * sum u(u-1)] / [2n(n-1)]
      var0 <- n * (n - 1) * (2 * n + 5) / 18
      var1 <- sum(x_freq * (x_freq - 1) * (2 * x_freq + 5)) / 18
      var2 <- sum(y_freq * (y_freq - 1) * (2 * y_freq + 5)) / 18
      var3 <- sum(x_freq * (x_freq - 1) * (x_freq - 2)) *
        sum(y_freq * (y_freq - 1) * (y_freq - 2)) /
        (9 * n * (n - 1) * (n - 2))
      var4 <- sum(x_freq * (x_freq - 1)) * sum(y_freq * (y_freq - 1)) /
        (2 * n * (n - 1))

      var_tau <- (var0 - var1 - var2) + var3 + var4
      var_tau <- var_tau / ((n0 - Tx - Txy) * (n0 - Ty - Txy))
    }

    se_tau <- sqrt(var_tau)
  }

  if (!is.na(se_tau) && se_tau > 0) {
    z_score <- tau / se_tau
  } else {
    z_score <- NA_real_
  }

  # Calculate p-value based on alternative hypothesis
  if (alternative == "two.sided") {
    p_value <- 2 * pnorm(-abs(z_score))
  } else if (alternative == "greater") {
    p_value <- pnorm(-z_score)
  } else { # less
    p_value <- pnorm(z_score)
  }

  # For weighted correlations, report sum of weights as N (SPSS compatibility)
  # For unweighted, report actual sample size
  n_report <- if (!is.null(w)) round(n_eff) else n

  return(list(
    tau = tau,
    p_value = p_value,
    z_score = z_score,
    n = n_report
  ))
}

#' Engine spec for kendall_tau (see R/correlation-engine.R)
#' @noRd
.kendall_tau_spec <- list(
  class_name = "kendall_tau",
  result_names = c("correlations", "n_obs", "matrices", "variables", "weights",
                   "alternative", "use", "is_grouped", "groups", "group_keys"),
  matrices = list(
    tau      = list(init = NA_real_,    diag = 1),
    p_values = list(init = NA_real_,    diag = 0),
    n_obs    = list(init = NA_integer_, diag = "n"),
    z_scores = list(init = NA_real_,    diag = Inf)
  ),
  extract = function(res) {
    list(tau = res$tau, p_values = res$p_value, n_obs = res$n,
         z_scores = res$z_score)
  },
  diag_n = "weight_sum",
  df_source = "matrices",
  df_cols = c(tau = "tau", p_value = "p_values", z_score = "z_scores",
              n = "n_obs"),
  sig_inline = FALSE,
  name_rows = FALSE,
  group_cols = "front",
  finalize_df = function(df) {
    # Add significance indicators (standard three-level style)
    df$sig <- add_significance_stars(df$p_value)
    df
  },
  # --- print/summary display ---
  compact_title = "Kendall's Tau",
  stat_col = "tau",
  stat_label = "tau",
  verbose_title = "Kendall's Tau-b Correlation",
  info = function(x) {
    list(
      "Weights variable" = x$weights,
      "Missing data handling" = paste(x$use, "deletion"),
      "Alternative hypothesis" = x$alternative
    )
  },
  params = NULL,
  pair_stat_prefix = "Kendall's tau-b: tau-b",
  pair_extras = function(corrs, digits) {
    # Always show z-score
    if ("z_score" %in% names(corrs)) {
      cat(sprintf("  z-score: %.*f\n", digits, corrs$z_score[1]))
    }
  },
  matrix_key = "tau",
  matrix_title = "Kendall's Tau-b Matrix:",
  p_title = function(x) {
    sprintf("Significance Matrix (p-values, %s):",
            if (x$alternative == "two.sided") "2-tailed" else "1-tailed")
  },
  pairwise_df = function(corrs, digits) {
    data.frame(
      Pair = paste(corrs$var1, "\u00d7", corrs$var2),
      tau_b = sprintf("%.*f", digits, corrs$tau),
      z = sprintf("%.*f", digits, corrs$z_score),
      p = sprintf("%.4f", as.numeric(corrs$p_value)),
      n = corrs$n,
      sig = corrs$sig,
      stringsAsFactors = FALSE
    )
  }
)

#' Print Kendall's tau results (compact)
#'
#' @description
#' Compact print method for objects of class \code{"kendall_tau"}.
#' Shows tau coefficient, p-value, and sample size per pair.
#'
#' For the full detailed output including matrices, use \code{summary()}.
#'
#' @param x An object of class \code{"kendall_tau"} returned by
#'   \code{\link{kendall_tau}}.
#' @param digits Number of decimal places to display. Default is \code{3}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- kendall_tau(survey_data, age, life_satisfaction)
#' result              # compact one-line overview
#' summary(result)     # full correlation matrices
#'
#' @export
#' @method print kendall_tau
print.kendall_tau <- function(x, digits = 3, ...) {
  .print_cor_result(x, .kendall_tau_spec, digits)
}

#' Summary method for Kendall's tau correlation results
#'
#' @description
#' Creates a summary object that produces detailed output when printed,
#' including tau matrices, p-value matrices, and sample size matrices.
#'
#' @param object A \code{kendall_tau} result object.
#' @param correlation_matrix Logical. Show the tau coefficient matrix? (Default: TRUE)
#' @param pvalue_matrix Logical. Show the p-value matrix? (Default: TRUE)
#' @param n_matrix Logical. Show the sample size matrix? (Default: TRUE)
#' @param digits Number of decimal places for formatting (Default: 3).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.kendall_tau} object.
#'
#' @examples
#' result <- kendall_tau(survey_data, trust_government, trust_media)
#' summary(result)
#' summary(result, pvalue_matrix = FALSE)
#'
#' @seealso \code{\link{kendall_tau}} for the main analysis function.
#' @export
#' @method summary kendall_tau
summary.kendall_tau <- function(object, correlation_matrix = TRUE,
                                pvalue_matrix = TRUE, n_matrix = TRUE,
                                digits = 3, ...) {
  build_summary_object(
    object     = object,
    show       = list(correlation_matrix = correlation_matrix,
                      pvalue_matrix = pvalue_matrix,
                      n_matrix = n_matrix),
    digits     = digits,
    class_name = "summary.kendall_tau"
  )
}

#' Print summary of Kendall's tau correlation results (detailed output)
#'
#' @description
#' Displays the detailed SPSS-style output for a Kendall's tau correlation,
#' with sections controlled by the boolean parameters passed to
#' \code{\link{summary.kendall_tau}}.  Sections include the correlation
#' matrix, p-value matrix, and sample size matrix.
#'
#' @param x A \code{summary.kendall_tau} object created by
#'   \code{\link{summary.kendall_tau}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- kendall_tau(survey_data, age, life_satisfaction)
#' summary(result)                             # all matrices
#' summary(result, pvalue_matrix = FALSE)      # hide p-values
#'
#' @seealso \code{\link{kendall_tau}} for the main analysis,
#'   \code{\link{summary.kendall_tau}} for summary options.
#' @export
#' @method print summary.kendall_tau
print.summary.kendall_tau <- function(x, ...) {
  .print_cor_summary(x, .kendall_tau_spec)
}
