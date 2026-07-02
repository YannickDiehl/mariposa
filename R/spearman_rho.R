#' Spearman's Rank Correlation Analysis
#'
#' @description
#' Calculates Spearman's rank correlation coefficients (rho) between variables with support
#' for weighted correlations, grouped data, and multiple variable pairs. Provides
#' significance testing and SPSS-compatible output formatting.
#'
#' Spearman's rho is a non-parametric measure of rank correlation that assesses monotonic
#' relationships between variables. It is particularly suitable for ordinal data or when
#' the assumptions of Pearson correlation are not met.
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The variables you want to correlate. List two for a single
#'   correlation or more for a correlation matrix. You can use helpers like
#'   \code{starts_with("trust")}.
#' @param weights Optional survey weights. Following the SPSS NONPAR CORR
#'   convention, weights are used only to filter cases (rows with weight
#'   <= 0 or NA are dropped); the rank correlation itself is computed
#'   unweighted on the remaining sample. For design-based weighted
#'   Spearman use \code{survey::svyolr()} or
#'   \code{wCorr::weightedCorr(method = "Spearman")}.
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
#'   including the rho coefficient, p-value, t-statistic, and sample size for
#'   each pair. For multiple variables, correlation, significance, and sample
#'   size matrices are also provided.
#'   Use \code{summary()} for the full SPSS-style output with toggleable sections.
#'
#' @details
#' ## Understanding the Results
#'
#' Spearman's rho measures the strength and direction of a monotonic relationship
#' between two variables. Unlike Pearson correlation, it does not require a linear
#' relationship -- it just needs one variable to consistently increase (or decrease)
#' as the other increases. Think of it as ranking your data first, then checking if
#' the ranks tend to go together.
#'
#' The rho value ranges from -1 to +1:
#' - **Strong positive** (0.7 to 1.0): High ranks in one variable go with high ranks in the other
#' - **Moderate positive** (0.3 to 0.7): Moderate tendency for ranks to increase together
#' - **Weak positive** (0 to 0.3): Slight tendency for ranks to increase together
#' - **No correlation** (near 0): No relationship between the variables
#' - **Negative values**: As one variable's rank increases, the other's tends to decrease
#'
#' The output also provides:
#' - **p-value**: Probability of seeing this correlation by chance
#' - **n**: Number of observation pairs used
#' - **significance stars**: Visual indicator (*** very strong, ** strong, * moderate evidence)
#'
#' ## When to Use This
#'
#' Choose Spearman's rho when:
#' - Your relationship is monotonic but not necessarily linear
#' - Your data has outliers (they have less impact on ranks)
#' - Your variables are ordinal (ordered categories)
#' - You are not sure if your data meets Pearson correlation assumptions
#' - You want to detect any monotonic trend, not just linear ones
#'
#' ## Spearman vs. Kendall
#'
#' - **Spearman's rho** is usually larger in magnitude than Kendall's tau
#' - **Spearman's rho** is better for detecting linear relationships in ranks
#' - **Kendall's tau** is more robust and has better statistical properties
#' - **Spearman's rho** is more commonly reported in research
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Basic correlation between two variables
#' survey_data %>%
#'   spearman_rho(life_satisfaction, political_orientation)
#'
#' # Correlation matrix for multiple variables
#' survey_data %>%
#'   spearman_rho(life_satisfaction, political_orientation, trust_media)
#'
#' # Weighted correlations (mathematically correct, though SPSS may not apply weights)
#' survey_data %>%
#'   spearman_rho(age, income, weights = sampling_weight)
#'
#' # Grouped correlations
#' survey_data %>%
#'   group_by(region) %>%
#'   spearman_rho(age, income, life_satisfaction)
#'
#' # Using tidyselect helpers
#' survey_data %>%
#'   spearman_rho(starts_with("trust"), weights = sampling_weight)
#'
#' # Listwise deletion for missing data
#' survey_data %>%
#'   spearman_rho(age, income, use = "listwise")
#'
#' # One-tailed test
#' survey_data %>%
#'   spearman_rho(age, income, alternative = "greater")
#'
#' # --- Three-layer output ---
#' result <- spearman_rho(survey_data, age, income, life_satisfaction)
#' result              # compact one-line overview
#' summary(result)     # full correlation, p-value, and N matrices
#' summary(result, pvalue_matrix = FALSE)  # hide p-values
#'
#' @seealso
#' \code{\link[stats]{cor}} with \code{method = "spearman"} for the base R
#' implementation.
#'
#' \code{\link{kendall_tau}} for Kendall's rank correlation.
#'
#' \code{\link{pearson_cor}} for Pearson correlation analysis.
#'
#' \code{\link{summary.spearman_rho}} for detailed output with toggleable sections.
#'
#' @references
#' Spearman, C. (1904). The proof and measurement of association between two
#' things. \emph{American Journal of Psychology}, 15(1), 72--101.
#'
#' Lehmann, E. L. (1975). \emph{Nonparametrics: Statistical Methods Based on
#' Ranks}. Holden-Day.
#'
#' @family correlation
#' @export
spearman_rho <- function(data, ..., weights = NULL,
                        alternative = c("two.sided", "less", "greater"),
                        use = c("pairwise", "listwise"), na.rm = NULL) {
  .correlate(
    data, ...,
    weights = rlang::enquo(weights),
    alternative = alternative,
    use = use,
    na.rm = na.rm,
    pair_fn = .spearman_pair,
    spec = .spearman_rho_spec
  )
}

#' Calculate one Spearman's rho pair (SPSS NONPAR CORR convention)
#' @noRd
.spearman_pair <- function(x, y, w = NULL, alternative = "two.sided") {
  if (!is.null(w)) {
    # SPSS-compatible Spearman's rho: ignore weights entirely
    # This matches SPSS NONPAR CORR behavior
    valid <- !is.na(x) & !is.na(y) & !is.na(w) & w > 0
    x <- x[valid]
    y <- y[valid]
    # Note: weights are used only for filtering, not for calculation

    n <- length(x)
    if (n < 3) {
      return(list(
        rho = NA_real_,
        p_value = NA_real_,
        t_stat = NA_real_,
        n = n
      ))
    }

    # Calculate unweighted ranks
    rank_x <- rank(x, na.last = "keep", ties.method = "average")
    rank_y <- rank(y, na.last = "keep", ties.method = "average")

    # Calculate unweighted Pearson correlation of ranks
    rho <- cor(rank_x, rank_y, use = "complete.obs", method = "pearson")
    n_eff <- n

  } else {
    # Unweighted Spearman's rho
    valid <- !is.na(x) & !is.na(y)
    x <- x[valid]
    y <- y[valid]

    n <- length(x)
    if (n < 3) {
      return(list(
        rho = NA_real_,
        p_value = NA_real_,
        t_stat = NA_real_,
        n = n
      ))
    }

    # Calculate ranks (ties get average rank)
    rank_x <- rank(x, na.last = "keep", ties.method = "average")
    rank_y <- rank(y, na.last = "keep", ties.method = "average")

    # Calculate Pearson correlation of ranks
    rho <- cor(rank_x, rank_y, use = "complete.obs", method = "pearson")
    n_eff <- n
  }

  # Undefined correlation (zero variance, e.g. a constant variable):
  # return NA instead of crashing on the abs(rho) check below.
  if (is.na(rho)) {
    return(list(
      rho = NA_real_,
      p_value = NA_real_,
      t_stat = NA_real_,
      n = n
    ))
  }

  # Significance testing using t-distribution
  # For Spearman's rho: t = rho * sqrt((n-2)/(1-rho^2))
  if (abs(rho) == 1) {
    t_stat <- sign(rho) * Inf
    p_value <- 0
  } else {
    t_stat <- rho * sqrt((n_eff - 2) / (1 - rho^2))
    df <- n_eff - 2

    # Calculate p-value based on alternative hypothesis
    if (alternative == "two.sided") {
      p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
    } else if (alternative == "greater") {
      p_value <- pt(t_stat, df = df, lower.tail = FALSE)
    } else {  # alternative == "less"
      p_value <- pt(t_stat, df = df, lower.tail = TRUE)
    }
  }

  return(list(
    rho = rho,
    p_value = p_value,
    t_stat = t_stat,
    n = n
  ))
}

#' Engine spec for spearman_rho (see R/correlation-engine.R)
#' @noRd
.spearman_rho_spec <- list(
  class_name = "spearman_rho",
  result_names = c("correlations", "matrices", "variables", "weights",
                   "alternative", "use", "is_grouped", "groups", "n_obs"),
  matrices = list(
    rho      = list(init = NA, diag = 1),
    p_values = list(init = NA, diag = 0),
    n_obs    = list(init = NA, diag = "n")
  ),
  extract = function(res) {
    list(rho = res$rho, p_values = res$p_value, n_obs = res$n)
  },
  diag_n = "count",
  df_source = "pairs",
  df_cols = list(
    rho     = function(res) res$rho,
    t_stat  = function(res) res$t_stat,
    p_value = function(res) res$p_value,
    n       = function(res) res$n
  ),
  # Significance symbols via the shared helper (uniform boundary
  # convention across the whole package)
  sig_inline = TRUE,
  name_rows = TRUE,
  group_cols = "back",
  finalize_df = NULL,
  # --- print/summary display ---
  compact_title = "Spearman Correlation",
  stat_col = "rho",
  stat_label = "rho",
  verbose_title = "Spearman's Rank Correlation Analysis",
  info = function(x) {
    list(
      "Method" = "Spearman's rho (rank correlation)",
      "Variables" = paste(x$variables, collapse = ", "),
      "Weights variable" = x$weights,
      "Missing data handling" = paste(if ("use" %in% names(x)) x$use else "pairwise", "deletion")
    )
  },
  params = function(x) list(alternative = x$alternative),
  pair_stat_prefix = "Spearman's rho: rho",
  pair_extras = function(corrs, digits) {
    # Always show t-stat
    if ("t_stat" %in% names(corrs)) {
      cat(sprintf("  t-statistic: %.*f\n", digits, corrs$t_stat[1]))
    }
  },
  matrix_key = "rho",
  matrix_title = "Spearman's Rho Matrix:",
  p_title = function(x) {
    sprintf("Significance Matrix (p-values, %s):",
            if (x$alternative == "two.sided") "2-tailed" else "1-tailed")
  },
  pairwise_df = function(corrs, digits) {
    data.frame(
      Pair = paste(corrs$var1, "\u00d7", corrs$var2),
      rho = sprintf("%.*f", digits, corrs$rho),
      t = sprintf("%.*f", digits, corrs$t_stat),
      p = sprintf("%.4f", as.numeric(corrs$p_value)),
      n = corrs$n,
      sig = corrs$sig,
      stringsAsFactors = FALSE
    )
  }
)

#' Print Spearman correlation results (compact)
#'
#' @description
#' Compact print method for objects of class \code{"spearman_rho"}.
#' Shows rank correlation coefficient, p-value, and sample size per pair.
#'
#' For the full detailed output including matrices, use \code{summary()}.
#'
#' @param x An object of class \code{"spearman_rho"} returned by
#'   \code{\link{spearman_rho}}.
#' @param digits Number of decimal places to display. Default is \code{3}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- spearman_rho(survey_data, age, life_satisfaction)
#' result              # compact one-line overview
#' summary(result)     # full correlation matrices
#'
#' @export
#' @method print spearman_rho
print.spearman_rho <- function(x, digits = 3, ...) {
  .print_cor_result(x, .spearman_rho_spec, digits)
}

#' Summary method for Spearman correlation results
#'
#' @description
#' Creates a summary object that produces detailed output when printed,
#' including rho matrices, p-value matrices, and sample size matrices.
#'
#' @param object A \code{spearman_rho} result object.
#' @param correlation_matrix Logical. Show the rho coefficient matrix? (Default: TRUE)
#' @param pvalue_matrix Logical. Show the p-value matrix? (Default: TRUE)
#' @param n_matrix Logical. Show the sample size matrix? (Default: TRUE)
#' @param digits Number of decimal places for formatting (Default: 3).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.spearman_rho} object.
#'
#' @examples
#' result <- spearman_rho(survey_data, trust_government, trust_media)
#' summary(result)
#' summary(result, pvalue_matrix = FALSE)
#'
#' @seealso \code{\link{spearman_rho}} for the main analysis function.
#' @export
#' @method summary spearman_rho
summary.spearman_rho <- function(object, correlation_matrix = TRUE,
                                 pvalue_matrix = TRUE, n_matrix = TRUE,
                                 digits = 3, ...) {
  build_summary_object(
    object     = object,
    show       = list(correlation_matrix = correlation_matrix,
                      pvalue_matrix = pvalue_matrix,
                      n_matrix = n_matrix),
    digits     = digits,
    class_name = "summary.spearman_rho"
  )
}

#' Print summary of Spearman correlation results (detailed output)
#'
#' @description
#' Displays the detailed SPSS-style output for a Spearman rank correlation,
#' with sections controlled by the boolean parameters passed to
#' \code{\link{summary.spearman_rho}}.  Sections include the correlation
#' matrix, p-value matrix, and sample size matrix.
#'
#' @param x A \code{summary.spearman_rho} object created by
#'   \code{\link{summary.spearman_rho}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- spearman_rho(survey_data, age, life_satisfaction)
#' summary(result)                             # all matrices
#' summary(result, pvalue_matrix = FALSE)      # hide p-values
#'
#' @seealso \code{\link{spearman_rho}} for the main analysis,
#'   \code{\link{summary.spearman_rho}} for summary options.
#' @export
#' @method print summary.spearman_rho
print.summary.spearman_rho <- function(x, ...) {
  .print_cor_summary(x, .spearman_rho_spec)
}
