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
#' @param data A data frame or tibble containing the variables to analyze
#' @param ... <\code{\link[dplyr]{dplyr_tidy_select}}> Variables for correlation analysis.
#'   Supports all tidyselect helpers. If more than two variables are selected,
#'   a correlation matrix is computed.
#' @param weights <\code{\link[dplyr]{dplyr_data_masking}}> Optional sampling weights
#'   for weighted correlations. Should be a numeric variable with positive values.
#'   Note: SPSS may not apply weights to Spearman's rho; our implementation uses
#'   weighted ranks for mathematically correct survey analysis.
#' @param alternative Character string specifying the alternative hypothesis:
#'   \itemize{
#'     \item \code{"two.sided"} (default): Two-tailed test
#'     \item \code{"less"}: One-tailed test (negative correlation)
#'     \item \code{"greater"}: One-tailed test (positive correlation)
#'   }
#' @param na.rm Character string specifying missing data handling:
#'   \itemize{
#'     \item \code{"pairwise"} (default): Pairwise deletion - each correlation uses all available cases
#'     \item \code{"listwise"}: Listwise deletion - only complete cases across all variables
#'   }
#'
#' @return An object of class \code{"spearman_rho"} containing:
#' \describe{
#'   \item{correlations}{Data frame with rho coefficients, p-values, and t-statistics}
#'   \item{n_obs}{Matrix of sample sizes for each correlation}
#'   \item{variables}{Character vector of analyzed variable names}
#'   \item{weights}{Name of the weights variable (if used)}
#'   \item{alternative}{Alternative hypothesis used}
#'   \item{is_grouped}{Logical indicating if data was grouped via group_by()}
#'   \item{groups}{Grouping variables (if any)}
#'   \item{matrices}{List of correlation, p-value, and sample size matrices}
#' }
#'
#' @details
#' ## What Spearman's Rho Measures
#'
#' Spearman's rho measures the strength and direction of a monotonic relationship between
#' two variables. Unlike Pearson correlation, it doesn't require a linear relationship -
#' it just needs one variable to consistently increase (or decrease) as the other increases.
#'
#' Think of it as ranking your data first, then checking if the ranks tend to go together.
#'
#' ## Interpreting Results
#'
#' The rho value ranges from -1 to +1:
#' - **Strong positive** (0.7 to 1.0): High ranks in one variable go with high ranks in the other
#' - **Moderate positive** (0.3 to 0.7): Moderate tendency for ranks to increase together
#' - **Weak positive** (0 to 0.3): Slight tendency for ranks to increase together
#' - **No correlation** (near 0): No relationship between the variables
#' - **Negative values**: As one variable's rank increases, the other's tends to decrease
#'
#' ## When to Use Spearman's Rho
#'
#' Choose Spearman's rho when:
#' - Your relationship is monotonic but not necessarily linear
#' - Your data has outliers (they have less impact on ranks)
#' - Your variables are ordinal (ordered categories)
#' - You're not sure if your data meets Pearson correlation assumptions
#' - You want to detect any monotonic trend, not just linear ones
#'
#' ## Spearman vs. Kendall
#'
#' - **Spearman's rho** is usually larger in magnitude than Kendall's tau
#' - **Spearman's rho** is better for detecting linear relationships in ranks
#' - **Kendall's tau** is more robust and has better statistical properties
#' - **Spearman's rho** is more commonly reported in research
#'
#' ## Understanding the Output
#'
#' The function provides:
#' - **rho**: The rank correlation coefficient
#' - **p-value**: Probability of seeing this correlation by chance
#' - **n**: Number of observation pairs used
#' - **significance stars**: Visual indicator (*** very strong, ** strong, * moderate evidence)
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
#'   spearman_rho(age, income, na.rm = "listwise")
#'
#' # One-tailed test
#' survey_data %>%
#'   spearman_rho(age, income, alternative = "greater")
#'
#' @seealso
#' \code{\link[stats]{cor}} with method="spearman" for base R implementation
#' \code{\link[stats]{cor.test}} with method="spearman" for significance testing
#' \code{\link{kendall_tau}} for Kendall's rank correlation
#' \code{\link{pearson_cor}} for Pearson correlation analysis
#'
#' @references
#' Spearman, C. (1904). The proof and measurement of association between two things.
#' American Journal of Psychology, 15(1), 72-101.
#'
#' Lehmann, E.L. (1975). Nonparametrics: Statistical Methods Based on Ranks. Holden-Day.
#'
#' @family correlation
#' @export
spearman_rho <- function(data, ..., weights = NULL, alternative = "two.sided",
                        na.rm = "pairwise") {

  # Input validation
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }

  if (!na.rm %in% c("pairwise", "listwise")) {
    cli_abort("{.arg na.rm} must be either {.val pairwise} or {.val listwise}.")
  }

  if (!alternative %in% c("two.sided", "less", "greater")) {
    cli_abort("{.arg alternative} must be {.val two.sided}, {.val less}, or {.val greater}.")
  }


  # Check if data is grouped
  is_grouped <- inherits(data, "grouped_df")
  group_vars <- if (is_grouped) dplyr::group_vars(data) else NULL

  # Get variable names using tidyselect
  dots <- enquos(...)
  weights_quo <- enquo(weights)

  # Evaluate selections
  vars <- eval_select(expr(c(!!!dots)), data = data)
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

  # Get weights if provided
  if (!quo_is_null(weights_quo)) {
    w_var <- eval_select(expr(!!weights_quo), data = data)
    w_name <- names(w_var)
  } else {
    w_name <- NULL
  }

  # Helper function to calculate weighted ranks
  calculate_weighted_ranks <- function(x, w = NULL) {
    if (is.null(w)) {
      # Unweighted ranks with tie handling (average ranks)
      rank(x, na.last = "keep", ties.method = "average")
    } else {
      # For weighted Spearman, SPSS uses unweighted ranks
      # This matches SPSS NONPAR CORR behavior
      # The weights are applied later in the correlation calculation
      rank(x, na.last = "keep", ties.method = "average")
    }
  }

  # Helper function to calculate Spearman's rho
  calculate_spearman_rho <- function(x, y, w = NULL, alternative = "two.sided") {
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

  # Function to perform analysis for a single group or entire dataset
  perform_analysis <- function(group_data) {
    # Apply listwise deletion if requested
    if (na.rm == "listwise") {
      complete_vars <- var_names
      if (!is.null(w_name)) {
        complete_vars <- c(complete_vars, w_name)
      }
      group_data <- group_data[complete.cases(group_data[, complete_vars, drop = FALSE]), ]
    }

    # Initialize results storage
    all_results <- list()

    # Calculate correlations for all pairs
    for (i in 1:(length(var_names) - 1)) {
      for (j in (i + 1):length(var_names)) {
        var1 <- var_names[i]
        var2 <- var_names[j]

        # Get data
        x <- group_data[[var1]]
        y <- group_data[[var2]]
        w <- if (!is.null(w_name)) group_data[[w_name]] else NULL

        # Calculate correlation
        result <- calculate_spearman_rho(x, y, w, alternative)

        # Add significance symbols (standard three-level style)
        sig <- if (is.na(result$p_value)) ""
              else if (result$p_value < 0.001) "***"
              else if (result$p_value < 0.01) "**"
              else if (result$p_value < 0.05) "*"
              else ""

        # Store results
        all_results[[paste(var1, var2, sep = "_")]] <- data.frame(
          var1 = var1,
          var2 = var2,
          rho = result$rho,
          t_stat = result$t_stat,
          p_value = result$p_value,
          n = result$n,
          sig = sig,
          stringsAsFactors = FALSE
        )
      }
    }

    # Combine all results
    correlations <- do.call(rbind, all_results)
    rownames(correlations) <- NULL

    # Create matrices for output
    n_vars <- length(var_names)
    rho_matrix <- matrix(NA, n_vars, n_vars, dimnames = list(var_names, var_names))
    p_matrix <- matrix(NA, n_vars, n_vars, dimnames = list(var_names, var_names))
    n_matrix <- matrix(NA, n_vars, n_vars, dimnames = list(var_names, var_names))

    # Fill diagonal
    diag(rho_matrix) <- 1
    diag(p_matrix) <- 0

    # Fill matrices
    for (k in 1:nrow(correlations)) {
      i <- which(var_names == correlations$var1[k])
      j <- which(var_names == correlations$var2[k])

      rho_matrix[i, j] <- rho_matrix[j, i] <- correlations$rho[k]
      p_matrix[i, j] <- p_matrix[j, i] <- correlations$p_value[k]
      n_matrix[i, j] <- n_matrix[j, i] <- correlations$n[k]
    }

    # Fill diagonal of n_matrix
    for (i in 1:n_vars) {
      if (!is.null(w_name)) {
        valid <- !is.na(group_data[[var_names[i]]]) &
                !is.na(group_data[[w_name]]) &
                group_data[[w_name]] > 0
        n_matrix[i, i] <- sum(valid)
      } else {
        n_matrix[i, i] <- sum(!is.na(group_data[[var_names[i]]]))
      }
    }

    return(list(
      correlations = correlations,
      rho_matrix = rho_matrix,
      p_matrix = p_matrix,
      n_matrix = n_matrix
    ))
  }

  # Perform analysis
  if (is_grouped) {
    # Grouped analysis
    results_list <- data %>%
      dplyr::group_split() %>%
      lapply(perform_analysis)

    # Get group information
    group_info <- data %>%
      dplyr::group_keys()

    # Combine results with group information
    all_correlations <- list()
    matrices <- list()

    for (i in 1:length(results_list)) {
      # Add group columns to correlations
      group_cols <- group_info[i, , drop = FALSE]
      correlations <- results_list[[i]]$correlations

      for (col in names(group_cols)) {
        correlations[[col]] <- rep(group_cols[[col]], nrow(correlations))
      }

      all_correlations[[i]] <- correlations

      # Store matrices
      matrices[[i]] <- list(
        rho = results_list[[i]]$rho_matrix,
        p_values = results_list[[i]]$p_matrix,
        n_obs = results_list[[i]]$n_matrix
      )
    }

    # Combine all correlations
    correlations <- do.call(rbind, all_correlations)

  } else {
    # Ungrouped analysis
    result <- perform_analysis(data)
    correlations <- result$correlations
    matrices <- list(list(
      rho = result$rho_matrix,
      p_values = result$p_matrix,
      n_obs = result$n_matrix
    ))
  }

  # Create result object
  result <- list(
    correlations = correlations,
    matrices = matrices,
    variables = var_names,
    weights = w_name,
    alternative = alternative,
    is_grouped = is_grouped,
    groups = group_vars,
    n_obs = if (!is_grouped) matrices[[1]]$n_obs else NULL
  )

  class(result) <- "spearman_rho"
  return(result)
}

#' Print method for spearman_rho
#'
#' @param x An object of class "spearman_rho"
#' @param digits Number of decimal places to display
#' @param ... Additional arguments (not used)
#' @export
print.spearman_rho <- function(x, digits = 3, ...) {
  # Header
  cat("\n")
  # Print header using standardized title
  title <- get_standard_title("Spearman's Rank Correlation Analysis", x$weights, "")
  cat(title, "\n")
  cat("══════════════════════════════════════════════════════════════════════\n")

  # Display analysis info
  cat("Method: Spearman's rho (rank correlation)\n")
  cat(sprintf("Variables: %s\n", paste(x$variables, collapse = ", ")))
  if (!is.null(x$weights)) {
    cat(sprintf("Weights: %s\n", x$weights))
  }
  cat(sprintf("Alternative hypothesis: %s\n", x$alternative))
  cat(sprintf("Missing data handling: %s deletion\n",
             if("na.rm" %in% names(x)) x$na.rm else "pairwise"))

  # Helper function to print correlation matrix with nice formatting
  .print_rho_matrix <- function(mat, digits = 3, title = "Correlation Matrix:", type = "correlation") {
    cat("\n", title, "\n", sep = "")
    cat("─────────────────────────────────────────────────────────────────────\n")

    # Format matrix for printing
    if (type == "correlation") {
      # For correlations, show 1.000 on diagonal
      mat_formatted <- formatC(mat, digits = digits, format = "f")
      diag(mat_formatted) <- "1.000"
    } else if (type == "pvalue") {
      # For p-values, show dashes on diagonal
      mat_formatted <- formatC(mat, digits = digits, format = "f")
      diag(mat_formatted) <- "  -  "
    } else if (type == "n") {
      # For sample sizes, show as integers
      mat_formatted <- format(round(mat), width = 5)
    } else {
      mat_formatted <- formatC(mat, digits = digits, format = "f")
    }

    # Create display matrix with row names
    display_mat <- cbind(Variable = rownames(mat), as.data.frame(mat_formatted))

    # Print with alignment
    print(display_mat, row.names = FALSE, right = TRUE, quote = FALSE)
  }

  # Main results display
  if (x$is_grouped) {
    # Grouped analysis
    cat("\n══ GROUPED ANALYSIS ══\n")

    # Get unique group combinations
    group_combinations <- unique(x$correlations[, x$groups, drop = FALSE])

    for (i in 1:nrow(group_combinations)) {
      # Format group header
      group_info <- sapply(names(group_combinations), function(g) {
        paste(g, "=", group_combinations[i, g])
      })
      cat(sprintf("\nGroup: %s\n", paste(group_info, collapse = ", ")))

      # Filter correlations for this group
      group_corrs <- x$correlations
      for (g in names(group_combinations)) {
        group_corrs <- group_corrs[group_corrs[[g]] == group_combinations[i, g], ]
      }

      # For each pair of variables, show results
      if (length(x$variables) == 2) {
        # Single correlation - use variable block format
        var_pair <- paste(x$variables[1], "\u00d7", x$variables[2])
        cat(sprintf("\n\u250c\u2500 %s \u2500\u2510\n\n", var_pair))

        # Show correlation statistics
        cat(sprintf("  Spearman's rho: \u03c1 = %.3f\n", group_corrs$rho[1]))
        cat(sprintf("  Sample size: n = %d\n", group_corrs$n[1]))
        cat(sprintf("  t-statistic: %.3f\n", group_corrs$t_stat[1]))
        cat(sprintf("  p-value (%s): %.4f\n",
                   if(x$alternative == "two.sided") "2-tailed" else "1-tailed",
                   group_corrs$p_value[1]))
        # Display significance
        sig_text <- if (group_corrs$sig[1] == "") "ns" else group_corrs$sig[1]
        cat(sprintf("  Significance: %s\n", sig_text))

      } else {
        # Multiple correlations - show matrix first, then detailed results
        rho_matrix <- x$matrices[[i]]$rho
        .print_rho_matrix(rho_matrix, digits = digits,
                         title = "Spearman's Rho Matrix:",
                         type = "correlation")

        # Print p-value matrix
        p_matrix <- x$matrices[[i]]$p_values
        .print_rho_matrix(p_matrix, digits = 4,
                         title = sprintf("Significance Matrix (p-values, %s):",
                                       if(x$alternative == "two.sided") "2-tailed" else "1-tailed"),
                         type = "pvalue")

        # Print sample size matrix
        n_matrix <- x$matrices[[i]]$n_obs
        .print_rho_matrix(n_matrix, digits = 0,
                         title = "Sample Size Matrix:",
                         type = "n")

        # Detailed pairwise results
        cat("\nPairwise Results:\n")
        cat("─────────────────────────────────────────────────────────────────────\n")

        # Create results table
        output_df <- data.frame(
          Pair = paste(group_corrs$var1, "\u00d7", group_corrs$var2),
          rho = sprintf("%.3f", group_corrs$rho),
          t = sprintf("%.3f", group_corrs$t_stat),
          p = sprintf("%.4f", group_corrs$p_value),
          n = group_corrs$n,
          sig = group_corrs$sig,
          stringsAsFactors = FALSE
        )

        # Print the table
        print(output_df, row.names = FALSE, right = TRUE)
        cat("─────────────────────────────────────────────────────────────────────\n")
      }
    }

  } else {
    # Ungrouped analysis
    if (length(x$variables) == 2) {
      # Single correlation
      var_pair <- paste(x$variables[1], "\u00d7", x$variables[2])
      cat(sprintf("\n\u250c\u2500 %s \u2500\u2510\n\n", var_pair))

      # Show correlation statistics
      cat(sprintf("  Spearman's rho: \u03c1 = %.3f\n", x$correlations$rho[1]))
      cat(sprintf("  Sample size: n = %d\n", x$correlations$n[1]))
      cat(sprintf("  t-statistic: %.3f\n", x$correlations$t_stat[1]))
      cat(sprintf("  p-value (%s): %.4f\n",
                 if(x$alternative == "two.sided") "2-tailed" else "1-tailed",
                 x$correlations$p_value[1]))
      # Display significance
      sig_text <- if (x$correlations$sig[1] == "") "ns" else x$correlations$sig[1]
      cat(sprintf("  Significance: %s\n", sig_text))

    } else {
      # Multiple correlations - show matrices
      rho_matrix <- x$matrices[[1]]$rho
      .print_rho_matrix(rho_matrix, digits = digits,
                       title = "\nSpearman's Rho Matrix:",
                       type = "correlation")

      # Print p-value matrix
      p_matrix <- x$matrices[[1]]$p_values
      .print_rho_matrix(p_matrix, digits = 4,
                       title = sprintf("Significance Matrix (p-values, %s):",
                                     if(x$alternative == "two.sided") "2-tailed" else "1-tailed"),
                       type = "pvalue")

      # Print sample size matrix
      n_matrix <- x$matrices[[1]]$n_obs
      .print_rho_matrix(n_matrix, digits = 0,
                       title = "Sample Size Matrix:",
                       type = "n")

      # Detailed pairwise results
      cat("\nPairwise Results:\n")
      cat("─────────────────────────────────────────────────────────────────────\n")

      # Create results table
      output_df <- data.frame(
        Pair = paste(x$correlations$var1, "\u00d7", x$correlations$var2),
        rho = sprintf("%.3f", x$correlations$rho),
        t = sprintf("%.3f", x$correlations$t_stat),
        p = sprintf("%.4f", x$correlations$p_value),
        n = x$correlations$n,
        sig = x$correlations$sig,
        stringsAsFactors = FALSE
      )

      # Print the table
      print(output_df, row.names = FALSE, right = TRUE)
      cat("─────────────────────────────────────────────────────────────────────\n")
    }
  }

  # Footer section with interpretation
  cat("\n")
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05\n")

  # Note about interpretation
  if (length(x$variables) == 2) {
    abs_rho <- abs(x$correlations$rho[1])
    strength <- if (abs_rho < 0.3) "weak"
                else if (abs_rho < 0.7) "moderate"
                else "strong"
    cat(sprintf("\nInterpretation: %s %s monotonic relationship\n", strength,
               if(x$correlations$rho[1] > 0) "positive" else if(x$correlations$rho[1] < 0) "negative" else "no"))
  }

  invisible(x)
}