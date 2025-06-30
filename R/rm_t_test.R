
#' Perform paired t-tests for repeated measures data
#'
#' @description
#' \code{rm_t_test()} performs paired t-tests on exactly two variables in a data frame. 
#' The function is designed for repeated measures analyses where the same subjects 
#' are measured at two different time points or under two different conditions.
#' It supports grouped data operations for performing paired comparisons within each group.
#'
#' The function implements paired t-tests with comprehensive effect size calculations 
#' including Cohen's d and Hedges' g (bias-corrected). Results are SPSS-compatible.
#'
#' @param data A data frame or tibble containing the variables to analyze.
#' @param ... <\code{\link[dplyr]{dplyr_tidy_select}}> Exactly 2 variables for which 
#'   to perform paired t-tests. Supports all tidyselect helpers such as 
#'   \code{starts_with()}, \code{ends_with()}, \code{contains()}, etc.
#' @param mu A number specifying the null hypothesis value for the mean difference. 
#'   Default is \code{0} (no difference between paired observations).
#' @param alternative Character string specifying the alternative hypothesis. 
#'   Must be one of \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}.
#' @param conf.level Confidence level for the confidence interval. Must be between 
#'   0 and 1. Default is \code{0.95} (95% confidence interval).
#' @param weights <\code{\link[dplyr]{dplyr_tidy_select}}> Optional. A variable name 
#'   specifying case weights for the analysis. When provided, weighted statistics are 
#'   calculated for means and standard deviations.
#'
#' @return An object of class \code{"rm_t_test_results"} containing:
#' \describe{
#'   \item{results}{A data frame with test statistics, p-values, effect sizes, 
#'     and confidence intervals}
#'   \item{variables}{Character vector of the two analyzed variable names}
#'   \item{is_grouped}{Logical indicating if data was grouped via group_by()}
#'   \item{mu, alternative, conf.level}{Test parameters}
#' }
#' 
#' The results data frame contains the following columns:
#' \describe{
#'   \item{Variable}{Name of the comparison ("var1 vs var2")}
#'   \item{t_stat}{t-statistic}
#'   \item{df}{Degrees of freedom}
#'   \item{p_value}{p-value}
#'   \item{mean_diff}{Mean difference (var1 - var2)}
#'   \item{cohens_d}{Cohen's d effect size}
#'   \item{hedges_g}{Hedges' g (bias-corrected Cohen's d)}
#'   \item{conf_int_lower, conf_int_upper}{Confidence interval bounds}
#'   \item{group_stats}{List column with variable-specific statistics}
#' }
#'
#' @details
#' ## Statistical Methods
#' 
#' ### Paired Sample Tests
#' For paired tests with difference scores \eqn{d_i = x_{1i} - x_{2i}}:
#' \deqn{t = \frac{\bar{d} - \mu_0}{s_d/\sqrt{n}}}
#' where \eqn{\bar{d}} is the mean difference and \eqn{s_d} is the standard deviation of differences.
#' 
#' ### Effect Sizes
#' 
#' **Cohen's d (paired samples):**
#' \deqn{d = \frac{\bar{d}}{s_d}}
#' 
#' **Hedges' g (bias-corrected Cohen's d):**
#' For paired samples: \eqn{g = d \times J} where \eqn{J = 1 - \frac{3}{4(n-1)-1}}
#' 
#' ## Grouped Analyses
#' When data is grouped using \code{group_by()}, the function performs separate 
#' paired analyses for each group combination.
#' 
#' ## SPSS Compatibility
#' Results match SPSS output for:
#' - t-statistics and p-values
#' - Confidence intervals
#' - Effect size calculations
#' 
#' ## Interpretation Guidelines
#' - **p-values**: p < 0.05 indicates statistical significance at α = 0.05
#' - **Effect sizes**: |effect| ≈ 0.2 (small), |effect| ≈ 0.5 (medium), |effect| ≈ 0.8 (large)
#' - **Cohen's d**: Classic effect size using standard deviation of differences
#' - **Hedges' g**: Bias-corrected version, preferred for publication
#' - **Confidence intervals**: Range of plausible values for the true mean difference
#'
#' @seealso 
#' \code{\link[stats]{t.test}} for the base R t-test function.
#' 
#' \code{\link{print.rm_t_test_results}} for printing results.
#' 
#' \code{\link[dplyr]{group_by}} for grouped analyses.
#' 
#' For independent samples t-tests, use \code{\link{t_test}}.
#' 
#' @references
#' Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). 
#' Lawrence Erlbaum Associates.
#' 
#' Hedges, L. V. (1981). Distribution theory for Glass's estimator of effect size 
#' and related estimators. Journal of Educational Statistics, 6(2), 107-128.
#'
#' @examples
#' # Load required packages
#' library(dplyr)
#' 
#' # Create sample data
#' set.seed(123)
#' survey_data <- data.frame(
#'   id = 1:200,
#'   pre_test = rnorm(200, 50, 10),
#'   post_test = rnorm(200, 55, 12),
#'   condition = factor(rep(c("A", "B"), each = 100))
#' )
#' 
#' # Basic paired t-test
#' survey_data %>%
#'   rm_t_test(pre_test, post_test)
#' 
#' # Grouped paired t-test (paired comparison within each condition)
#' survey_data %>%
#'   group_by(condition) %>%
#'   rm_t_test(pre_test, post_test)
#' 
#' # One-sided test
#' survey_data %>%
#'   rm_t_test(pre_test, post_test, alternative = "greater")
#' 
#' # Test against different null hypothesis
#' survey_data %>%
#'   rm_t_test(pre_test, post_test, mu = 5)
#' 
#' # Weighted paired t-test
#' survey_data$weight <- runif(200, 0.5, 2.0)
#' survey_data %>%
#'   rm_t_test(pre_test, post_test, weights = weight)
#'
#' @export
rm_t_test <- function(data, ..., mu = 0, alternative = c("two.sided", "less", "greater"),
                      conf.level = 0.95, weights = NULL) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  alternative <- match.arg(alternative)
  
  # Check if data is grouped
  is_grouped <- inherits(data, "grouped_df")
  group_vars <- if (is_grouped) group_vars(data) else NULL
  
  # Get variable names using tidyselect
  dots <- enquos(...)
  weights_quo <- enquo(weights)
  
  # Evaluate selections
  vars <- eval_select(expr(c(!!!dots)), data = data)
  var_names <- names(vars)
  
  # Paired t-test validation
  if (length(var_names) != 2) {
    stop("rm_t_test requires exactly 2 variables")
  }
  
  # Handle weights parameter
  if (!quo_is_null(weights_quo)) {
    w_var <- eval_select(expr(!!weights_quo), data = data)
    w_name <- names(w_var)
  } else {
    w_name <- NULL
  }

  # Helper function to perform paired t-test
  perform_paired_t_test <- function(data, var1_name, var2_name) {
    # Get the variable values
    x1 <- data[[var1_name]]
    x2 <- data[[var2_name]]
    
    # Get weights if specified
    w <- if (!is.null(w_name)) data[[w_name]] else NULL
    
    # Remove NA values (complete cases only for paired test)
    if (!is.null(w)) {
      valid_indices <- !is.na(x1) & !is.na(x2) & !is.na(w) & w > 0
      w <- w[valid_indices]
    } else {
      valid_indices <- !is.na(x1) & !is.na(x2)
    }
    
    x1 <- x1[valid_indices]
    x2 <- x2[valid_indices]
    
    if (length(x1) < 2) {
      stop("Insufficient data for paired t-test (need at least 2 complete pairs)")
    }
    
    # Calculate differences
    differences <- x1 - x2
    
    # Calculate statistics using the exact method from t_test.R
    if (!is.null(w)) {
      # Weighted paired t-test using sjstats method (adapted for paired design)
      # Calculate weighted means for individual variables
      mu_x1 <- sum(x1 * w) / sum(w)  # weighted mean variable 1
      mu_x2 <- sum(x2 * w) / sum(w)  # weighted mean variable 2
      
      # Calculate differences and their weighted statistics
      differences <- x1 - x2
      mu_diff <- sum(differences * w) / sum(w)  # weighted mean difference
      
      # Calculate weighted variance of differences using Bessel's correction (SPSS method)
      var_diff <- sum(w * (differences - mu_diff)^2) / (sum(w) - 1)
      
      # SPSS-compatible degrees of freedom calculation
      # Use effective sample size (sum of weights) for standard errors
      n_eff <- sum(w)
      
      # SPSS uses the rounded effective sample size for SE calculation in weighted tests
      n_eff_rounded <- round(n_eff)
      
      # Calculate standard error using rounded effective sample size (SPSS method for weighted paired tests)
      se_diff <- sqrt(var_diff / n_eff_rounded)
      
      # SPSS uses weighted sample size - 1 as degrees of freedom for paired tests
      df <- n_eff_rounded - 1
      
      # Calculate t-statistic
      t_stat <- (mu_diff - mu) / se_diff
      
      # Calculate p-value
      if (alternative == "two.sided") {
        p_value <- 2 * pt(-abs(t_stat), df)
      } else if (alternative == "less") {
        p_value <- pt(t_stat, df)
      } else {
        p_value <- pt(t_stat, df, lower.tail = FALSE)
      }
      
      # Calculate confidence interval
      if (alternative == "two.sided") {
        conf_int <- mu_diff + c(-1, 1) * qt((1 + conf.level)/2, df) * se_diff
      } else if (alternative == "less") {
        conf_int <- c(-Inf, mu_diff + qt(conf.level, df) * se_diff)
      } else {
        conf_int <- c(mu_diff - qt(conf.level, df) * se_diff, Inf)
      }
      
      # Set values for display
      mean_x1 <- mu_x1
      mean_x2 <- mu_x2
      mean_diff <- mu_diff
      sd_diff <- sqrt(var_diff)
      
    } else {
      # Unweighted paired t-test
      test_result <- t.test(x1, x2, paired = TRUE, mu = mu, 
                           alternative = alternative, conf.level = conf.level)
      t_stat <- test_result$statistic
      df <- test_result$parameter
      p_value <- test_result$p.value
      conf_int <- test_result$conf.int
      mean_diff <- test_result$estimate
      
      # Calculate descriptive statistics
      differences <- x1 - x2
      mean_x1 <- mean(x1)
      mean_x2 <- mean(x2)
      sd_diff <- sd(differences)
    }
    
    # Calculate Cohen's d for paired samples (effect size for the difference)
    cohens_d <- mean_diff / sd_diff
    
    # Hedges' bias correction for paired samples
    # For paired samples: J = 1 - 3/(4*(n-1)-1)
    hedges_j <- 1 - (3 / (4 * df - 1))
    hedges_g <- cohens_d * hedges_j
    
    # Prepare group statistics for display
    group_stats <- list(
      var1 = list(name = var1_name, mean = mean_x1, n = length(x1)),
      var2 = list(name = var2_name, mean = mean_x2, n = length(x2)),
      differences = list(mean = mean_diff, sd = sd_diff),
      weighted = !is.null(w)
    )
    
    return(list(
      t_stat = t_stat,
      df = df,
      p_value = p_value,
      mean_diff = mean_diff,
      conf_int = conf_int,
      cohens_d = cohens_d,
      hedges_g = hedges_g,
      group_stats = group_stats
    ))
  }

  # Main analysis logic
  if (is_grouped) {
    # Split data by groups
    data_list <- group_split(data)
    group_keys <- group_keys(data)
    
    # Perform paired t-tests for each group
    results_list <- lapply(seq_along(data_list), function(i) {
      group_data <- data_list[[i]]
      group_info <- group_keys[i, , drop = FALSE]
      
      var1_name <- var_names[1]
      var2_name <- var_names[2]
      
      tryCatch({
        test_result <- perform_paired_t_test(group_data, var1_name, var2_name)
        
        result_df <- data.frame(
          group_info,
          Variable = paste(var1_name, "vs", var2_name),
          t_stat = test_result$t_stat,
          df = test_result$df,
          p_value = test_result$p_value,
          mean_diff = test_result$mean_diff,
          cohens_d = test_result$cohens_d,
          hedges_g = test_result$hedges_g,
          conf_int_lower = test_result$conf_int[1],
          conf_int_upper = test_result$conf_int[2]
        )
        result_df$group_stats <- list(test_result$group_stats)
        result_df
      }, error = function(e) {
        # Ensure var_name is scalar  
        var_name_safe <- paste(var1_name, "vs", var2_name)
        
        # Create error data frame with explicit dimensions
        error_df <- data.frame(
          Variable = var_name_safe,
          t_stat = NA,
          df = NA,
          p_value = NA,
          mean_diff = NA,
          cohens_d = NA,
          hedges_g = NA,
          conf_int_lower = NA,
          conf_int_upper = NA,
          group_stats = I(list(NULL))
        )
        
        # Combine with group info
        cbind(group_info, error_df)
      })
    })
    
    results_df <- do.call(rbind, results_list)
    
  } else {
    # Perform paired t-test (exactly 2 variables required)
    var1_name <- var_names[1]
    var2_name <- var_names[2]
    
    test_result <- perform_paired_t_test(data, var1_name, var2_name)
    
    # Create single result row for paired comparison
    results_df <- data.frame(
      Variable = paste(var1_name, "vs", var2_name),
      t_stat = test_result$t_stat,
      df = test_result$df,
      p_value = test_result$p_value,
      mean_diff = test_result$mean_diff,
      cohens_d = test_result$cohens_d,
      hedges_g = test_result$hedges_g,
      conf_int_lower = test_result$conf_int[1],
      conf_int_upper = test_result$conf_int[2]
    )
    results_df$group_stats <- list(test_result$group_stats)
  }
  
  # Create S3 object (Template Standard)
  structure(
    list(
      results = results_df,
      variables = var_names,
      groups = group_vars,
      group_vars = group_vars,        # Alternative naming (Template Standard)
      is_grouped = is_grouped,
      grouped = is_grouped,           # Alternative naming (Template Standard)
      weights = w_name,
      weight_var = w_name,            # Alternative naming (Template Standard)
      mu = mu,
      alternative = alternative,
      conf.level = conf.level
    ),
    class = "rm_t_test_results"
  )
}

#' Create effect size data frame for paired tests
#' @keywords internal
.create_paired_effect_size_df <- function(var_name, cohens_d_val, hedges_g_val) {
  effect_size_g <- if (abs(hedges_g_val) < 0.2) "negligible" else
                  if (abs(hedges_g_val) < 0.5) "small" else
                  if (abs(hedges_g_val) < 0.8) "medium" else "large"
  
  data.frame(
    Variable = var_name,
    Cohens_d = round(cohens_d_val, 3),
    Hedges_g = round(hedges_g_val, 3),
    Effect_Size = effect_size_g,
    stringsAsFactors = FALSE
  )
}

#' Print paired t-test results
#'
#' @description
#' Print method for objects of class \code{"rm_t_test_results"}.
#' Displays test statistics, p-values, confidence intervals, and effect sizes
#' in a formatted, easy-to-read layout similar to SPSS output.
#'
#' @param x An object of class \code{"rm_t_test_results"} from \code{rm_t_test()}.
#' @param digits Number of decimal places to display for numeric values. Default is 3.
#' @param ... Additional arguments passed to print methods.
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @export
print.rm_t_test_results <- function(x, digits = 3, ...) {
  # Determine if weighted analysis was used
  is_weighted <- !is.null(x$weights)
  
  # Header
  header_text <- if (is_weighted) "Weighted Paired t-Test Results" else "Paired t-Test Results"
  header_length <- nchar(header_text)
  cat(sprintf("\n%s\n", header_text))
  cat(paste(rep("-", header_length), collapse = ""), "\n")
  
  # Ensure p-values are numeric
  x$results$p_value <- as.numeric(x$results$p_value)
  
  # Add significance stars (Template Standard)
  x$results$sig <- cut(x$results$p_value, 
                      breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                      labels = c("***", "**", "*", ""),
                      right = FALSE)
                      
  # Paired t-test info
  cat(sprintf("\nVariables compared: %s vs %s\n", x$variables[1], x$variables[2]))
  if (is_weighted) {
    cat(sprintf("Weights variable: %s\n", x$weights))
  }
  cat(sprintf("Null hypothesis (mu): %.3f\n", x$mu))
  cat(sprintf("Alternative hypothesis: %s\n", x$alternative))
  cat(sprintf("Confidence level: %.1f%%\n", x$conf.level * 100))
  cat("\n")
  
  # Template Standard: Dual grouped data detection
  is_grouped_data <- (!is.null(x$grouped) && x$grouped) || 
                     (!is.null(x$is_grouped) && x$is_grouped)
  
  if (is_grouped_data && !is.null(x$groups) && length(x$groups) > 0) {
    # Print results for grouped data
    unique_groups <- unique(x$results[, x$groups, drop = FALSE])
    
    for (i in seq_len(nrow(unique_groups))) {
      group_info <- unique_groups[i, , drop = FALSE]
      group_key <- paste(names(group_info), "=", group_info, collapse = ", ")
      
      # Filter results for this group
      group_results <- x$results
      for (col in names(group_info)) {
        group_results <- group_results[group_results[[col]] == group_info[[col]], ]
      }
      
      for (j in seq_len(nrow(group_results))) {
        var <- group_results$Variable[j]
        stats <- group_results$group_stats[[j]]
        
        cat(sprintf("\nGroup: %s\n", group_key))
        cat(sprintf("\n┌─ %s ─┐\n", var))
        cat("\n")  # Add blank line after variable name
         
        # Print paired test statistics
        if (!is.null(stats)) {
          if (!is.null(stats$var1) && !is.null(stats$var2)) {
            cat(sprintf("  %s: mean = %.3f, n = %.0f\n", 
                        stats$var1$name, stats$var1$mean, stats$var1$n))
            cat(sprintf("  %s: mean = %.3f, n = %.0f\n", 
                        stats$var2$name, stats$var2$mean, stats$var2$n))
            if (!is.null(stats$differences)) {
              cat(sprintf("  Difference: mean = %.3f, sd = %.3f\n", 
                          stats$differences$mean, stats$differences$sd))
            }
          }
        }
        
        # Create and print t-test table for this variable
        results_df <- data.frame(
          Assumption = "Paired t-test",
          t_stat = round(group_results$t_stat[j], digits),
          df = round(group_results$df[j], digits),
          p_value = round(group_results$p_value[j], digits),
          mean_diff = round(group_results$mean_diff[j], digits),
          conf_int = sprintf("[%.3f, %.3f]", 
                            group_results$conf_int_lower[j],
                            group_results$conf_int_upper[j]),
          sig = group_results$sig[j]
        )
        
        table_title <- if (is_weighted) "Weighted Paired t-test Results:" else "Paired t-test Results:"
        cat(sprintf("\n%s\n", table_title))
        border_width <- paste(rep("-", 70), collapse = "")
        cat(border_width, "\n")
        print(results_df, row.names = FALSE)
        cat(border_width, "\n")
        
        # Add Effect Sizes for this variable
        cohens_d_val <- group_results$cohens_d[j]
        hedges_g_val <- group_results$hedges_g[j]
        
        if (!is.na(cohens_d_val)) {
          effect_df <- .create_paired_effect_size_df(var, cohens_d_val, hedges_g_val)
          
          cat("\nEffect Sizes:\n")
          cat(paste(rep("-", 12), collapse = ""), "\n")
          print(effect_df, row.names = FALSE)
        }
        cat("\n")
      }
    }
  } else {
    # Print results for ungrouped data
    valid_results <- x$results[!is.na(x$results$Variable), ]
    
    for (i in seq_len(nrow(valid_results))) {
      var_name <- valid_results$Variable[i]
      stats <- valid_results$group_stats[[i]]
      
      cat(sprintf("\n┌─ %s ─┐\n", var_name))
      cat("\n")  # Add blank line after variable name
      
      # Print paired test statistics
      if (!is.null(stats)) {
        if (!is.null(stats$var1) && !is.null(stats$var2)) {
          cat(sprintf("  %s: mean = %.3f, n = %.0f\n", 
                      stats$var1$name, stats$var1$mean, stats$var1$n))
          cat(sprintf("  %s: mean = %.3f, n = %.0f\n", 
                      stats$var2$name, stats$var2$mean, stats$var2$n))
          if (!is.null(stats$differences)) {
            cat(sprintf("  Difference: mean = %.3f, sd = %.3f\n", 
                        stats$differences$mean, stats$differences$sd))
          }
        }
        cat("\n")
      }
      
      # Create and print t-test table
      results_df <- data.frame(
        Assumption = "Paired t-test",
        t_stat = round(valid_results$t_stat[i], digits),
        df = round(valid_results$df[i], digits),
        p_value = round(valid_results$p_value[i], digits),
        mean_diff = round(valid_results$mean_diff[i], digits),
        conf_int = sprintf("[%.3f, %.3f]", 
                          valid_results$conf_int_lower[i],
                          valid_results$conf_int_upper[i]),
        sig = valid_results$sig[i]
      )
      
      table_title <- if (is_weighted) "Weighted Paired t-test Results:" else "Paired t-test Results:"
      cat(sprintf("\n%s\n", table_title))
      border_width <- paste(rep("-", 70), collapse = "")
      cat(border_width, "\n")
      print(results_df, row.names = FALSE)
      cat(border_width, "\n")
      
      # Add Effect Sizes for this variable
      cohens_d_val <- valid_results$cohens_d[i]
      hedges_g_val <- valid_results$hedges_g[i]
      
      if (!is.na(cohens_d_val)) {
        effect_df <- .create_paired_effect_size_df(var_name, cohens_d_val, hedges_g_val)
        
        cat("\nEffect Sizes:\n")
        cat(paste(rep("-", 12), collapse = ""), "\n")
        print(effect_df, row.names = FALSE)
        cat("\n")
      }
    }
  }
  
  # Template Standard: Footer
  cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05\n")
  
  # Template Standard: Test-specific interpretation
  cat("\nInterpretation:\n")
  cat("- Cohen's d: standard deviation of differences\n")
  cat("- Hedges' g: bias-corrected Cohen's d (preferred)\n")
  cat("- Small effect: |effect| ≈ 0.2\n")
  cat("- Medium effect: |effect| ≈ 0.5\n")
  cat("- Large effect: |effect| ≈ 0.8\n")
  
  # Template Standard: Invisible return
  invisible(x)
}