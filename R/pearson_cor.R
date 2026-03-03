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
#' @param weights Optional survey weights for population-representative results
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
  
  if (conf.level <= 0 || conf.level >= 1) {
    cli_abort("{.arg conf.level} must be between 0 and 1.")
  }
  
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
  
  # Helper function to calculate weighted correlation
  calculate_weighted_cor <- function(x, y, w = NULL, conf.level = 0.95, alternative = "two.sided") {
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
      # Also calculate true effective sample size for reference
      n_eff_true <- .effective_n(w)  # Statistically correct effective sample size
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
  
  # Helper function to calculate all pairwise correlations
  calculate_correlation_matrix <- function(data, var_names, w_name = NULL, conf.level = 0.95, alternative = "two.sided", use = "pairwise") {
    n_vars <- length(var_names)
    
    # Initialize storage
    cor_matrix <- matrix(NA_real_, n_vars, n_vars)
    p_matrix <- matrix(NA_real_, n_vars, n_vars)
    n_matrix <- matrix(NA_integer_, n_vars, n_vars)
    ci_lower_matrix <- matrix(NA_real_, n_vars, n_vars)
    ci_upper_matrix <- matrix(NA_real_, n_vars, n_vars)
    
    rownames(cor_matrix) <- colnames(cor_matrix) <- var_names
    rownames(p_matrix) <- colnames(p_matrix) <- var_names
    rownames(n_matrix) <- colnames(n_matrix) <- var_names
    rownames(ci_lower_matrix) <- colnames(ci_lower_matrix) <- var_names
    rownames(ci_upper_matrix) <- colnames(ci_upper_matrix) <- var_names
    
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
          cor_matrix[i, j] <- 1
          p_matrix[i, j] <- 0
          # For weighted analysis, use sum of weights; for unweighted, use count
          if (!is.null(weights_vec)) {
            valid_idx <- !is.na(data[[var_names[i]]]) & !is.na(weights_vec)
            n_matrix[i, j] <- round(sum(weights_vec[valid_idx]))
          } else {
            n_matrix[i, j] <- sum(!is.na(data[[var_names[i]]]))
          }
          ci_lower_matrix[i, j] <- 1
          ci_upper_matrix[i, j] <- 1
        } else {
          # Calculate correlation
          result <- calculate_weighted_cor(
            data[[var_names[i]]],
            data[[var_names[j]]],
            weights_vec,
            conf.level,
            alternative
          )
          
          # Store results (symmetric)
          cor_matrix[i, j] <- cor_matrix[j, i] <- result$correlation
          p_matrix[i, j] <- p_matrix[j, i] <- result$p_value
          n_matrix[i, j] <- n_matrix[j, i] <- result$n
          ci_lower_matrix[i, j] <- ci_lower_matrix[j, i] <- result$conf_int[1]
          ci_upper_matrix[i, j] <- ci_upper_matrix[j, i] <- result$conf_int[2]
        }
      }
    }
    
    return(list(
      correlations = cor_matrix,
      p_values = p_matrix,
      n_obs = n_matrix,
      ci_lower = ci_lower_matrix,
      ci_upper = ci_upper_matrix
    ))
  }
  
  # Main execution logic
  if (is_grouped) {
    # Group analysis
    data_list <- dplyr::group_split(data)
    group_keys <- dplyr::group_keys(data)
    
    results_list <- lapply(seq_along(data_list), function(i) {
      group_data <- data_list[[i]]
      group_info <- group_keys[i, , drop = FALSE]
      
      # Calculate correlation matrix for this group
      cor_results <- calculate_correlation_matrix(
        group_data,
        var_names,
        w_name,
        conf.level,
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
                correlation = cor_results$correlations[i, j],
                p_value = cor_results$p_values[i, j],
                conf_int_lower = cor_results$ci_lower[i, j],
                conf_int_upper = cor_results$ci_upper[i, j],
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
        conf.level,
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
      conf.level,
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
            correlation = cor_results$correlations[i, j],
            p_value = cor_results$p_values[i, j],
            conf_int_lower = cor_results$ci_lower[i, j],
            conf_int_upper = cor_results$ci_upper[i, j],
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
  
  # Add significance indicators
  correlations_df$sig <- cut(correlations_df$p_value,
                             breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                             labels = c("***", "**", "*", ""),
                             right = FALSE)
  
  # Calculate r-squared for effect size
  correlations_df$r_squared <- correlations_df$correlation^2
  
  # Create result object
  result <- list(
    correlations = correlations_df,
    n_obs = n_obs,
    matrices = matrices_list,
    variables = var_names,
    weights = w_name,
    conf.level = conf.level,
    use = use,
    alternative = alternative,
    is_grouped = is_grouped,
    groups = group_vars,
    group_keys = if(is_grouped) group_keys else NULL
  )
  
  class(result) <- "pearson_cor"
  return(result)
}

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
  weighted_tag <- if (!is.null(x$weights)) " [Weighted]" else ""
  corrs <- x$correlations

  if (isTRUE(x$is_grouped)) {
    groups <- unique(corrs[x$groups])
    for (gi in seq_len(nrow(groups))) {
      group_values <- groups[gi, , drop = FALSE]
      group_label <- paste(names(group_values), "=", group_values, collapse = ", ")
      cat(sprintf("[%s]\n", group_label))

      group_corrs <- corrs
      for (g in names(group_values)) {
        group_corrs <- group_corrs[group_corrs[[g]] == group_values[[g]], ]
      }
      .print_pearson_compact(x, group_corrs, weighted_tag, digits)
    }
  } else {
    .print_pearson_compact(x, corrs, weighted_tag, digits)
  }

  invisible(x)
}

#' Print compact pearson_cor output for one group or ungrouped
#' @keywords internal
.print_pearson_compact <- function(x, corrs, weighted_tag, digits) {
  n_vars <- length(x$variables)

  if (n_vars == 2) {
    pair_label <- paste(x$variables[1], "x", x$variables[2])
    cat(sprintf("Pearson Correlation: %s%s\n", pair_label, weighted_tag))
    r_val <- corrs$correlation[1]
    p_val <- as.numeric(corrs$p_value[1])
    n_val <- corrs$n[1]
    cat(sprintf("  r = %.*f, %s %s, N = %d\n",
                digits, r_val,
                format_p_compact(p_val, digits),
                add_significance_stars(p_val),
                n_val))
  } else {
    n_sig <- sum(as.numeric(corrs$p_value) < 0.05, na.rm = TRUE)
    n_pairs <- nrow(corrs)
    cat(sprintf("Pearson Correlation: %d variables%s\n", n_vars, weighted_tag))

    for (i in seq_len(n_pairs)) {
      pair_label <- paste(corrs$var1[i], "x", corrs$var2[i])
      r_val <- corrs$correlation[i]
      p_val <- as.numeric(corrs$p_value[i])
      line <- sprintf("  %-30s r = %.*f, %s %s",
                       paste0(pair_label, ":"),
                       digits, r_val,
                       format_p_compact(p_val, digits),
                       add_significance_stars(p_val))
      cat(line, "\n")
    }
    cat(sprintf("  %d/%d pairs significant (p < .05), N = %d\n",
                n_sig, n_pairs, corrs$n[1]))
  }
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
  digits <- x$digits
  show_cor <- if (!is.null(x$show)) isTRUE(x$show$correlation_matrix) else TRUE
  show_p   <- if (!is.null(x$show)) isTRUE(x$show$pvalue_matrix) else TRUE
  show_n   <- if (!is.null(x$show)) isTRUE(x$show$n_matrix) else TRUE

  # Header
  title <- get_standard_title("Pearson Correlation", x$weights, "")
  print_header(title)

  # Info section
  cat("\n")
  test_info <- list(
    "Weights variable" = x$weights,
    "Missing data handling" = paste(x$use, "deletion")
  )
  print_info_section(test_info)
  test_params <- list(conf.level = x$conf.level)
  print_test_parameters(test_params)
  cat("\n")

  if (isTRUE(x$is_grouped)) {
    group_combinations <- unique(x$correlations[x$groups])

    for (gi in seq_len(nrow(group_combinations))) {
      print_group_header(group_combinations[gi, , drop = FALSE])

      group_corrs <- x$correlations
      for (g in names(group_combinations)) {
        group_corrs <- group_corrs[group_corrs[[g]] == group_combinations[gi, g], ]
      }

      .print_pearson_verbose(x, group_corrs, gi, show_cor, show_p, show_n, digits)
    }
  } else {
    .print_pearson_verbose(x, x$correlations, 1, show_cor, show_p, show_n, digits)
  }

  # Footer
  print_significance_legend()
  invisible(x)
}

#' Print verbose pearson_cor output for one group
#' @keywords internal
.print_pearson_verbose <- function(x, corrs, matrix_idx, show_cor, show_p, show_n, digits) {
  n_vars <- length(x$variables)

  if (n_vars == 2) {
    # For 2 variables, show single-pair detail with optional sections
    if (show_cor) {
      cat(sprintf("\n  Correlation: r = %.*f\n", digits, corrs$correlation[1]))
    }
    if (show_p) {
      cat(sprintf("  p-value: %s %s\n",
                  format_p_compact(as.numeric(corrs$p_value[1]), digits),
                  add_significance_stars(as.numeric(corrs$p_value[1]))))
    }
    if (show_n) {
      cat(sprintf("  N = %d\n", corrs$n[1]))
    }
    # Always show CI and r-squared
    if ("conf_int_lower" %in% names(corrs)) {
      cat(sprintf("  95%% CI: [%.*f, %.*f]\n", digits, corrs$conf_int_lower[1],
                  digits, corrs$conf_int_upper[1]))
    }
    if ("r_squared" %in% names(corrs)) {
      cat(sprintf("  r-squared: %.*f\n", digits, corrs$r_squared[1]))
    }
  } else {
    # For 3+ variables, show matrices and pairwise table

    if (show_cor) {
      cor_matrix <- x$matrices[[matrix_idx]]$correlations
      .print_cor_matrix(cor_matrix, digits = digits,
                        title = "Correlation Matrix:",
                        type = "correlation")
    }

    if (show_p) {
      p_matrix <- x$matrices[[matrix_idx]]$p_values
      .print_cor_matrix(p_matrix, digits = 4,
                        title = "Significance Matrix (p-values):",
                        type = "pvalue")
    }

    if (show_n) {
      n_matrix <- x$matrices[[matrix_idx]]$n_obs
      .print_cor_matrix(n_matrix, digits = 0,
                        title = "Sample Size Matrix:",
                        type = "n")
    }

    # Pairwise results always shown
    cat("\nPairwise Results:\n")
    border_width <- paste(rep("-", 16), collapse = "")
    cat(border_width, "\n")

    output_df <- data.frame(
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

    print(output_df, row.names = FALSE)
    cat(border_width, "\n")
  }
}
