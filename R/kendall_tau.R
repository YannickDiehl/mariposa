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
#' @param na.rm \lifecycle{deprecated} Use \code{use} instead.
#'
#' @return Correlation results showing rank-based relationships between variables,
#'   including the tau-b coefficient, p-value, z-score, and sample size for each
#'   pair. For multiple variables, correlation, significance, and sample size
#'   matrices are also provided.
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
#' # Grouped correlations
#' survey_data %>%
#'   group_by(region) %>%
#'   kendall_tau(age, income, life_satisfaction)
#'
#' # Using tidyselect helpers for ordinal variables
#' survey_data %>%
#'   kendall_tau(starts_with("trust"), weights = sampling_weight)
#'
#' # Listwise deletion for missing data
#' survey_data %>%
#'   kendall_tau(age, income, use = "listwise")
#'
#' # One-tailed test
#' survey_data %>%
#'   kendall_tau(age, income, alternative = "greater")
#'
#' # Store results for further analysis
#' result <- survey_data %>%
#'   kendall_tau(life_satisfaction, political_orientation, trust_media,
#'               weights = sampling_weight)
#' print(result)
#'
#' @seealso
#' \code{\link[stats]{cor}} with \code{method = "kendall"} for the base R
#' implementation.
#'
#' \code{\link{spearman_rho}} for Spearman's rank correlation.
#'
#' \code{\link{pearson_cor}} for Pearson correlation analysis.
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

  # Input validation
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }

  # Handle deprecated na.rm parameter
  if (!is.null(na.rm)) {
    cli_warn("{.arg na.rm} is deprecated in correlation functions. Use {.arg use} instead.")
    use <- na.rm
  }
  use <- match.arg(use)
  alternative <- match.arg(alternative)


  # Check if data is grouped
  is_grouped <- inherits(data, "grouped_df")
  group_vars <- if (is_grouped) dplyr::group_vars(data) else NULL

  # Select variables using centralized helper
  vars <- .process_variables(data, ...)
  var_names <- names(vars)

  if (length(var_names) < 2) {
    cli_abort("At least two variables must be specified for correlation analysis.")
  }

  # Validate that all selected variables are numeric
  for (var_name in var_names) {
    if (!is.numeric(data[[var_name]])) {
      cli_abort("Variable {.var {var_name}} is not numeric.")
    }
  }

  # Process weights using centralized helper
  weights_info <- .process_weights(data, rlang::enquo(weights))
  w_name <- weights_info$name

  # Helper function to calculate weighted Kendall's tau
  calculate_weighted_tau <- function(x, y, w = NULL, alternative = "two.sided") {
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
      numerator <- concordant - discordant
      denominator <- sqrt((total_weight - ties_x) * (total_weight - ties_y))

      if (denominator == 0) {
        tau <- 0
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
        tau <- 0
      } else {
        tau <- (P - Q) / denominator
      }

      n_eff <- n
    }

    # Ensure tau is within valid range
    tau <- max(-1, min(1, tau))

    # Calculate z-score for significance test using SPSS-compatible method
    if (!is.null(w)) {
      # For weighted, use simplified approximation
      if (n_eff < 10) {
        se_tau <- sqrt(2 * (2 * n_eff + 5) / (9 * n_eff * (n_eff - 1)))
      } else {
        se_tau <- sqrt((4 * n_eff + 10) / (9 * n_eff * (n_eff - 1)))
      }
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
        var0 <- n * (n - 1) * (2 * n + 5) / 18
        var1 <- sum(x_freq * (x_freq - 1) * (2 * x_freq + 5)) / 18
        var2 <- sum(y_freq * (y_freq - 1) * (2 * y_freq + 5)) / 18
        var3 <- sum(x_freq * (x_freq - 1) * (x_freq - 2)) / (9 * n * (n - 1) * (n - 2))
        var4 <- sum(y_freq * (y_freq - 1) * (y_freq - 2)) / (9 * n * (n - 1) * (n - 2))
        var5 <- sum(x_freq * (x_freq - 1)) * sum(y_freq * (y_freq - 1)) / (2 * n * (n - 1))

        var_tau <- (var0 - var1 - var2) + var3 * var4 + var5
        var_tau <- var_tau / ((n0 - Tx - Txy) * (n0 - Ty - Txy))
      }

      se_tau <- sqrt(var_tau)
    }

    if (se_tau > 0) {
      z_score <- tau / se_tau
    } else {
      z_score <- 0
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

  # Helper function to calculate all pairwise correlations
  calculate_correlation_matrix <- function(data, var_names, w_name = NULL, alternative = "two.sided", use = "pairwise") {
    n_vars <- length(var_names)

    # Initialize storage
    tau_matrix <- matrix(NA_real_, n_vars, n_vars)
    p_matrix <- matrix(NA_real_, n_vars, n_vars)
    n_matrix <- matrix(NA_integer_, n_vars, n_vars)
    z_matrix <- matrix(NA_real_, n_vars, n_vars)

    rownames(tau_matrix) <- colnames(tau_matrix) <- var_names
    rownames(p_matrix) <- colnames(p_matrix) <- var_names
    rownames(n_matrix) <- colnames(n_matrix) <- var_names
    rownames(z_matrix) <- colnames(z_matrix) <- var_names

    # Get weights vector if provided
    weights_vec <- if (!is.null(w_name)) data[[w_name]] else NULL

    # Handle listwise deletion if requested
    if (use == "listwise") {
      complete_cases <- complete.cases(data[var_names])
      if (!is.null(weights_vec)) {
        complete_cases <- complete_cases & !is.na(weights_vec)
      }
      data <- data[complete_cases, ]
      if (!is.null(weights_vec)) {
        weights_vec <- weights_vec[complete_cases]
      }
    }

    # Calculate correlations for each pair
    for (i in seq_len(n_vars)) {
      for (j in i:n_vars) {
        if (i == j) {
          # Diagonal: perfect correlation with self
          tau_matrix[i, j] <- 1
          p_matrix[i, j] <- 0
          z_matrix[i, j] <- Inf
          # For weighted analysis, use sum of weights; for unweighted, use count
          if (!is.null(weights_vec)) {
            valid_idx <- !is.na(data[[var_names[i]]]) & !is.na(weights_vec)
            n_matrix[i, j] <- round(sum(weights_vec[valid_idx]))
          } else {
            n_matrix[i, j] <- sum(!is.na(data[[var_names[i]]]))
          }
        } else {
          # Calculate correlation
          result <- calculate_weighted_tau(
            data[[var_names[i]]],
            data[[var_names[j]]],
            weights_vec,
            alternative
          )

          # Store results (symmetric)
          tau_matrix[i, j] <- tau_matrix[j, i] <- result$tau
          p_matrix[i, j] <- p_matrix[j, i] <- result$p_value
          n_matrix[i, j] <- n_matrix[j, i] <- result$n
          z_matrix[i, j] <- z_matrix[j, i] <- result$z_score
        }
      }
    }

    return(list(
      tau = tau_matrix,
      p_values = p_matrix,
      n_obs = n_matrix,
      z_scores = z_matrix
    ))
  }

  # Main execution logic
  if (is_grouped) {
    # Group analysis
    data_list <- group_split(data)
    group_keys <- group_keys(data)

    results_list <- lapply(seq_along(data_list), function(i) {
      group_data <- data_list[[i]]
      group_info <- group_keys[i, , drop = FALSE]

      # Calculate correlation matrix for this group
      cor_results <- calculate_correlation_matrix(
        group_data,
        var_names,
        w_name,
        alternative,
        use
      )

      # Convert to long format for results data frame
      long_results <- list()
      k <- 1

      for (i in seq_along(var_names)) {
        for (j in i:length(var_names)) {
          if (i != j) {  # Skip diagonal
            result_row <- cbind(
              group_info,
              data.frame(
                var1 = var_names[i],
                var2 = var_names[j],
                tau = cor_results$tau[i, j],
                p_value = cor_results$p_values[i, j],
                z_score = cor_results$z_scores[i, j],
                n = cor_results$n_obs[i, j],
                stringsAsFactors = FALSE
              )
            )
            long_results[[k]] <- result_row
            k <- k + 1
          }
        }
      }

      do.call(rbind, long_results)
    })

    # Combine all results
    correlations_df <- do.call(rbind, results_list)

    # Store matrices for each group (for print method)
    matrices_list <- lapply(seq_along(data_list), function(i) {
      group_data <- data_list[[i]]
      calculate_correlation_matrix(
        group_data,
        var_names,
        w_name,
        alternative,
        use
      )
    })

    n_obs <- NULL  # Will be stored in matrices_list

  } else {
    # Single analysis for whole dataset
    cor_results <- calculate_correlation_matrix(
      data,
      var_names,
      w_name,
      alternative,
      use
    )

    # Convert to long format for results data frame
    long_results <- list()
    k <- 1

    for (i in seq_along(var_names)) {
      for (j in i:length(var_names)) {
        if (i != j) {  # Skip diagonal
          result_row <- data.frame(
            var1 = var_names[i],
            var2 = var_names[j],
            tau = cor_results$tau[i, j],
            p_value = cor_results$p_values[i, j],
            z_score = cor_results$z_scores[i, j],
            n = cor_results$n_obs[i, j],
            stringsAsFactors = FALSE
          )
          long_results[[k]] <- result_row
          k <- k + 1
        }
      }
    }

    correlations_df <- do.call(rbind, long_results)
    n_obs <- cor_results$n_obs
    matrices_list <- list(cor_results)
  }

  # Add significance indicators (standard three-level style)
  correlations_df$sig <- cut(correlations_df$p_value,
                             breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                             labels = c("***", "**", "*", ""),
                             right = TRUE)

  # Create result object
  result <- list(
    correlations = correlations_df,
    n_obs = n_obs,
    matrices = matrices_list,
    variables = var_names,
    weights = w_name,
    alternative = alternative,
    use = use,
    is_grouped = is_grouped,
    groups = group_vars,
    group_keys = if(is_grouped) group_keys else NULL
  )

  class(result) <- "kendall_tau"
  return(result)
}

#' Print method for kendall_tau
#'
#' @param x A kendall_tau object
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments passed to print
#'
#' @export
print.kendall_tau <- function(x, digits = 3, ...) {

  # Print header using standardized helper
  test_type <- get_standard_title("Kendall's Tau-b Correlation", x$weights, "")
  print_header(test_type)

  # Print test information using standardized helpers
  cat("\n")
  test_info <- list(
    "Weights variable" = x$weights,
    "Missing data handling" = paste(x$use, "deletion"),
    "Alternative hypothesis" = x$alternative
  )
  print_info_section(test_info)
  cat("\n")

  if (x$is_grouped) {
    # Grouped analysis
    group_combinations <- unique(x$correlations[x$groups])

    for (i in seq_len(nrow(group_combinations))) {
      # Print group header using standardized helper
      print_group_header(group_combinations[i, , drop = FALSE])

      # Filter correlations for this group
      group_corrs <- x$correlations
      for (g in names(group_combinations)) {
        group_corrs <- group_corrs[group_corrs[[g]] == group_combinations[i, g], ]
      }

      # For each pair of variables, show results
      if (length(x$variables) == 2) {
        var_pair <- paste(x$variables[1], "\u00d7", x$variables[2])
        cat(sprintf("\n--- %s ---\n\n", var_pair))
        .print_single_pair(group_corrs, stat_label = "\u03c4", stat_col = "tau",
                           corr_name = "Kendall's tau-b",
                           secondary_label = "z-score",
                           secondary_col = "z_score",
                           alternative = x$alternative)

      } else {
        # Multiple correlations - show matrix first, then detailed results
        # Use the smart matrix printer for all matrices
        tau_matrix <- x$matrices[[i]]$tau
        .print_cor_matrix(tau_matrix, digits = digits,
                         title = "Kendall's Tau-b Matrix:",
                         type = "correlation")

        # Print p-value matrix
        p_matrix <- x$matrices[[i]]$p_values
        .print_cor_matrix(p_matrix, digits = 4,
                         title = sprintf("Significance Matrix (p-values, %s):",
                                       if(x$alternative == "two.sided") "2-tailed" else "1-tailed"),
                         type = "pvalue")

        # Print sample size matrix
        n_matrix <- x$matrices[[i]]$n_obs
        .print_cor_matrix(n_matrix, digits = 0,
                         title = "Sample Size Matrix:",
                         type = "n")

        # Detailed pairwise results
        cat("\nPairwise Results:\n")
        cat(paste(rep("-", 69), collapse = ""), "\n")

        # Create results table
        output_df <- data.frame(
          Pair = paste(group_corrs$var1, "×", group_corrs$var2),
          tau_b = sprintf("%.3f", group_corrs$tau),
          z = sprintf("%.3f", group_corrs$z_score),
          p = sprintf("%.4f", group_corrs$p_value),
          n = group_corrs$n,
          sig = group_corrs$sig,
          stringsAsFactors = FALSE
        )

        # Print the table
        print(output_df, row.names = FALSE, right = TRUE)
        cat(paste(rep("-", 69), collapse = ""), "\n")
      }
    }

  } else {
    # Ungrouped analysis
    if (length(x$variables) == 2) {
      var_pair <- paste(x$variables[1], "\u00d7", x$variables[2])
      cat(sprintf("\n--- %s ---\n\n", var_pair))
      .print_single_pair(x$correlations, stat_label = "\u03c4", stat_col = "tau",
                         corr_name = "Kendall's tau-b",
                         secondary_label = "z-score",
                         secondary_col = "z_score",
                         alternative = x$alternative)

    } else {
      # Multiple correlations - show matrices
      tau_matrix <- x$matrices[[1]]$tau
      .print_cor_matrix(tau_matrix, digits = digits,
                       title = "Kendall's Tau-b Matrix:",
                       type = "correlation")

      # Print p-value matrix
      p_matrix <- x$matrices[[1]]$p_values
      .print_cor_matrix(p_matrix, digits = 4,
                       title = sprintf("Significance Matrix (p-values, %s):",
                                     if(x$alternative == "two.sided") "2-tailed" else "1-tailed"),
                       type = "pvalue")

      # Print sample size matrix
      n_matrix <- x$matrices[[1]]$n_obs
      .print_cor_matrix(n_matrix, digits = 0,
                       title = "Sample Size Matrix:",
                       type = "n")

      # Detailed pairwise results
      cat("\nPairwise Results:\n")
      cat(paste(rep("-", 69), collapse = ""), "\n")

      # Create results table
      output_df <- data.frame(
        Pair = paste(x$correlations$var1, "×", x$correlations$var2),
        tau_b = sprintf("%.3f", x$correlations$tau),
        z = sprintf("%.3f", x$correlations$z_score),
        p = sprintf("%.4f", x$correlations$p_value),
        n = x$correlations$n,
        sig = x$correlations$sig,
        stringsAsFactors = FALSE
      )

      # Print the table
      print(output_df, row.names = FALSE, right = TRUE)
      cat(paste(rep("-", 69), collapse = ""), "\n")
    }
  }

  # Print significance codes
  print_significance_legend()

  # Note about interpretation
  if (length(x$variables) == 2) {
    abs_tau <- abs(x$correlations$tau[1])
    strength <- if (abs_tau < 0.3) "weak"
                else if (abs_tau < 0.5) "moderate"
                else "strong"
    cat(sprintf("\nInterpretation: %s %s correlation\n", strength,
               if(x$correlations$tau[1] > 0) "positive" else if(x$correlations$tau[1] < 0) "negative" else "no"))
  }

  invisible(x)
}