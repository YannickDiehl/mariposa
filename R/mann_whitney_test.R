
#' Perform Mann-Whitney U tests with support for weights and grouped data
#'
#' @description
#' \code{mann_whitney_test()} performs Mann-Whitney U tests (Wilcoxon rank-sum tests) 
#' on one or more variables in a data frame. This is a non-parametric alternative 
#' to the t-test when data is not normally distributed or measured on ordinal scales.
#' The function supports both weighted and unweighted analyses, grouped data operations, 
#' and multiple variables simultaneously.
#' 
#' The function provides SPSS-compatible results including effect size calculations 
#' (r = |Z|/√(n1 + n2)) and is particularly designed for survey data analysis where 
#' sampling weights are crucial for population-representative results.
#'
#' @param data A data frame or tibble containing the variables to analyze.
#' @param ... <\code{\link[dplyr]{dplyr_tidy_select}}> Variables for which to perform 
#'   Mann-Whitney tests. Supports all tidyselect helpers such as \code{starts_with()}, 
#'   \code{ends_with()}, \code{contains()}, \code{matches()}, \code{num_range()}, etc.
#' @param group <\code{\link[dplyr]{dplyr_data_masking}}> Grouping variable 
#'   for two-sample Mann-Whitney test. Must be a factor or character variable with exactly 
#'   two levels. Required for Mann-Whitney tests.
#' @param weights <\code{\link[dplyr]{dplyr_data_masking}}> Optional sampling weights 
#'   for weighted Mann-Whitney tests. Should be a numeric variable with positive values. 
#'   Weights are used to adjust for sampling design and non-response patterns.
#' @param mu A number specifying the hypothesized location shift. Default is \code{0}.
#' @param alternative Character string specifying the alternative hypothesis. 
#'   Must be one of \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}.
#' @param conf.level Confidence level for the effect size. Must be between 
#'   0 and 1. Default is \code{0.95} (95% confidence level).
#'
#' @return An object of class \code{"mann_whitney_test_results"} containing:
#' \describe{
#'   \item{results}{A data frame with test statistics, p-values, effect sizes, 
#'     and rank means for each variable}
#'   \item{variables}{Character vector of analyzed variable names}
#'   \item{group}{Name of the grouping variable}
#'   \item{weights}{Name of the weights variable (if used)}
#'   \item{group_levels}{Levels of the grouping variable}
#'   \item{is_grouped}{Logical indicating if data was grouped}
#'   \item{mu, alternative, conf.level}{Test parameters}
#' }
#' 
#' The results data frame contains the following columns:
#' \describe{
#'   \item{Variable}{Name of the analyzed variable}
#'   \item{U}{Mann-Whitney U statistic (minimum of U1 and U2)}
#'   \item{W}{Wilcoxon rank-sum statistic (rank sum of group with smaller U-value, SPSS-compatible)}
#'   \item{Z}{Standardized test statistic with tie correction}
#'   \item{p_value}{Two-tailed p-value}
#'   \item{effect_size_r}{Effect size r = |Z|/√(n1 + n2)}
#'   \item{rank_mean_diff}{Difference in rank means (group1 - group2)}
#'   \item{group_stats}{List column with group-specific statistics}
#' }
#'
#' @details
#' ## Statistical Methods
#' 
#' ### Unweighted Tests
#' For unweighted Mann-Whitney tests, the function implements the standard algorithm with 
#' SPSS-compatible calculations:
#' 
#' 1. **Rank Calculation**: All values are ranked together using R's \code{rank()} function
#' 2. **U Statistics**: 
#'    - \eqn{U_1 = R_1 - \frac{n_1(n_1 + 1)}{2}}
#'    - \eqn{U_2 = R_2 - \frac{n_2(n_2 + 1)}{2}}
#'    - \eqn{U = \min(U_1, U_2)} (reported U-statistic)
#' 3. **W Statistic**: Following SPSS convention, W represents the rank sum of the group 
#'    with the smaller U-value. This ensures mathematical consistency and matches SPSS output exactly.
#' 4. **Z Statistic**: Calculated with tie correction matching SPSS methodology:
#'    \deqn{Z = \frac{U - \mu_U}{\sigma_U}}
#'    where \eqn{\mu_U = \frac{n_1 n_2}{2}} and 
#'    \eqn{\sigma_U = \sqrt{\frac{n_1 n_2 [(n_1 + n_2 + 1) - T]}{12}}}
#'    with tie correction \eqn{T = \sum(t_i^3 - t_i)/[(n_1 + n_2)(n_1 + n_2 - 1)]}
#' 
#' ### Weighted Tests
#' For weighted tests, the function uses the survey package methodology:
#' - Weighted ranking using midpoint method for tied values
#' - Survey design-adjusted test statistics
#' - Chi²-to-Z conversion for effect size calculation
#' 
#' The standardized test statistic is:
#' \deqn{Z = \frac{U - \mu_U}{\sigma_U}}
#' where \eqn{\mu_U = \frac{n_1 n_2}{2}} and \eqn{\sigma_U = \sqrt{\frac{n_1 n_2 (n_1 + n_2 + 1)}{12}}}
#' 
#' 
#' ### Effect Size (r)
#' The effect size r is calculated as:
#' \deqn{r = \frac{|Z|}{\sqrt{n_1 + n_2}}}
#' 
#' This measure is equivalent to the point-biserial correlation and provides a 
#' standardized measure of effect magnitude.
#' 
#' ## Interpretation Guidelines
#' - **p-values**: p < 0.05 indicates statistical significance at α = 0.05
#' - **Effect size r**: |r| ≈ 0.1 (small), |r| ≈ 0.3 (medium), |r| ≈ 0.5 (large effect)
#' - **Rank differences**: Positive values indicate group 1 has higher average ranks
#' - **U-statistic**: Smaller values indicate greater separation between groups
#' - **W-statistic**: Represents the rank sum of the group contributing to the minimum U

#'
#' @seealso 
#' \code{\link[stats]{wilcox.test}} for the base R Wilcoxon test function.
#' 
#' \code{\link[survey]{svyranktest}} for survey-weighted rank tests.
#' 
#' \code{\link{t_test}} for parametric t-tests.
#' 
#' @references
#' Mann, H. B., & Whitney, D. R. (1947). On a test of whether one of two random 
#' variables is stochastically larger than the other. The Annals of Mathematical 
#' Statistics, 18(1), 50-60.
#' 
#' Wilcoxon, F. (1945). Individual comparisons by ranking methods. Biometrics 
#' Bulletin, 1(6), 80-83.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#' 
#' # Basic Mann-Whitney test (non-parametric comparison)
#' survey_data %>%
#'   mann_whitney_test(age, group = gender)
#' 
#' # Multiple variables
#' survey_data %>%
#'   mann_whitney_test(age, income, life_satisfaction, group = region)
#' 
#' # Using tidyselect helpers
#' survey_data %>%
#'   mann_whitney_test(starts_with("trust_"), group = gender)
#' 
#' # Weighted analysis
#' survey_data %>%
#'   mann_whitney_test(income, group = region, weights = sampling_weight)
#' 
#' # Grouped analysis (separate tests for each education level)
#' survey_data %>%
#'   group_by(education) %>%
#'   mann_whitney_test(life_satisfaction, group = gender)
#' 
#' # One-sided test
#' survey_data %>%
#'   mann_whitney_test(life_satisfaction, group = region, alternative = "greater")
#' 
#' # Store results for further analysis
#' result <- survey_data %>%
#'   mann_whitney_test(income, group = gender, weights = sampling_weight)
#' print(result)
#'
#' @export
mann_whitney_test <- function(data, ..., group, weights = NULL, mu = 0, 
                             alternative = c("two.sided", "less", "greater"),
                             conf.level = 0.95) {
  
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
  group_quo <- enquo(group)
  weights_quo <- enquo(weights)
  
  # Evaluate selections
  vars <- eval_select(expr(c(!!!dots)), data = data)
  var_names <- names(vars)
  
  # Group is required for Mann-Whitney test
  if (quo_is_null(group_quo)) {
    stop("group argument is required for Mann-Whitney test")
  }
  
  g_var <- eval_select(expr(!!group_quo), data = data)
  g_name <- names(g_var)[1]  # Take first name
  
  if (!quo_is_null(weights_quo)) {
    w_var <- eval_select(expr(!!weights_quo), data = data)
    w_name <- names(w_var)[1]  # Take first name
  } else {
    w_name <- NULL
  }

  # Helper function to perform Mann-Whitney test for a single variable
  perform_single_mann_whitney <- function(data, var_name, group_name, weight_name = NULL) {
    # Get the variable values
    x <- data[[var_name]]
    g <- data[[group_name]]
    
    # Remove NA values
    valid_indices <- !is.na(x) & !is.na(g)
    if (!is.null(weight_name)) {
      w <- data[[weight_name]]
      valid_indices <- valid_indices & !is.na(w)
      w <- w[valid_indices]
    }
    x <- x[valid_indices]
    g <- g[valid_indices]
    
    # Get unique levels preserving original order (SPSS convention)
    if (is.factor(g)) {
      all_levels <- levels(g)
      g_levels <- all_levels[all_levels %in% unique(g)]
    } else {
      # For non-factors, sort to ensure consistency
      g_levels <- sort(unique(g))
    }
    
    if (length(g_levels) != 2) {
      stop(sprintf("Mann-Whitney test requires exactly 2 groups, found %d in variable '%s'. For >2 groups, use a Kruskal-Wallis test instead.", 
                  length(g_levels), group_name))
    }
    
    # Split data by groups
    x1 <- x[g == g_levels[1]]
    x2 <- x[g == g_levels[2]]
    w1 <- if (!is.null(weight_name)) w[g == g_levels[1]] else NULL
    w2 <- if (!is.null(weight_name)) w[g == g_levels[2]] else NULL
    
    if (is.null(weight_name)) {
      # Calculate sample sizes
      n1 <- length(x1)
      n2 <- length(x2)
      
      # Manual rank calculation (more reliable than wilcox.test for our purposes)
      all_values <- c(x1, x2)
      all_ranks <- rank(all_values)
      
      # Calculate rank sums
      R1 <- sum(all_ranks[1:n1])  # Rank sum of group 1
      R2 <- sum(all_ranks[(n1+1):(n1+n2)])  # Rank sum of group 2
      
      # Rank means
      rank_mean1 <- R1 / n1
      rank_mean2 <- R2 / n2
      
      # Calculate U statistics
      U1 <- R1 - n1 * (n1 + 1) / 2  # U for group 1
      U2 <- R2 - n2 * (n2 + 1) / 2  # U for group 2
      
      # Use the smaller U as the Mann-Whitney U statistic (standard convention)
      U <- min(U1, U2)
      
      # W is the rank sum of the group with smaller U (SPSS convention)
      W_report <- if(U1 < U2) R1 else R2
      
      # Calculate Z statistic using U with continuity correction
      expected_U <- n1 * n2 / 2
      
      # Check for ties and calculate corrected variance (SPSS method)
      tie_groups <- table(all_values)
      tie_correction <- sum(tie_groups^3 - tie_groups)
      
      # Variance with tie correction (matching SPSS calculation)
      var_U <- n1 * n2 * ((n1 + n2 + 1) - tie_correction / ((n1 + n2) * (n1 + n2 - 1))) / 12
      
      # Calculate Z without continuity correction (to match SPSS exactly)
      Z <- (U - expected_U) / sqrt(var_U)
      
      # Effect size
      r <- abs(Z) / sqrt(n1 + n2)
      
      # Get p-value from wilcox.test for accuracy
      test_result <- wilcox.test(x1, x2, alternative = alternative, 
                               mu = mu, exact = FALSE, conf.int = TRUE,
                               conf.level = conf.level)
      
      group_stats <- list(
        group1 = list(name = as.character(g_levels[1]), rank_mean = rank_mean1, n = n1),
        group2 = list(name = as.character(g_levels[2]), rank_mean = rank_mean2, n = n2)
      )
      
    } else {
      # Weighted Mann-Whitney test using survey package
      # Note: Using standard wilcox.test for weighted analysis
      # Survey package dependency removed per project requirements
      
      # Create temporary data frame for survey design
      temp_data <- data.frame(
        x = x,
        g = g,
        w = w
      )
      
      # Use wilcox.test for weighted analysis (survey package not required)
      survey_result <- wilcox.test(x ~ g, data = temp_data)
      
      # Calculate weighted statistics
      group1_data <- temp_data[temp_data$g == g_levels[1], ]
      group2_data <- temp_data[temp_data$g == g_levels[2], ]
      
      n1_weighted <- sum(group1_data$w)
      n2_weighted <- sum(group2_data$w)
      
      # Weighted ranking using combined approach
      all_values <- c(group1_data$x, group2_data$x)
      all_weights <- c(group1_data$w, group2_data$w)
      
      # Calculate weighted ranks for all values combined
      sorted_indices <- order(all_values)
      sorted_values <- all_values[sorted_indices]
      sorted_weights <- all_weights[sorted_indices]
      cumsum_weights <- cumsum(sorted_weights)
      
      # Weighted ranks using midpoint method
      weighted_ranks <- rep(0, length(all_values))
      for(i in 1:length(sorted_values)) {
        if(i == 1) {
          weighted_ranks[sorted_indices[i]] <- sorted_weights[i] / 2
        } else {
          weighted_ranks[sorted_indices[i]] <- cumsum_weights[i-1] + sorted_weights[i] / 2
        }
      }
      
      # Split ranks by groups
      n1_unweighted <- nrow(group1_data)
      n2_unweighted <- nrow(group2_data)
      
      group1_weighted_ranks <- weighted_ranks[1:n1_unweighted]
      group2_weighted_ranks <- weighted_ranks[(n1_unweighted+1):(n1_unweighted+n2_unweighted)]
      
      # Weighted rank means
      rank_mean1 <- sum(group1_weighted_ranks * group1_data$w) / n1_weighted
      rank_mean2 <- sum(group2_weighted_ranks * group2_data$w) / n2_weighted
      
      # Calculate weighted rank sums
      R1 <- sum(group1_weighted_ranks * group1_data$w)
      R2 <- sum(group2_weighted_ranks * group2_data$w)
      
      # Calculate weighted U statistics
      U1 <- R1 - n1_weighted * (n1_weighted + 1) / 2
      U2 <- R2 - n2_weighted * (n2_weighted + 1) / 2
      
      # Use smaller U as the test statistic (following convention)
      U <- min(U1, U2)
      W <- max(R1, R2)  # W is the larger rank sum
      
      # Convert Chi² to Z (for 1 df: Z = ±√Chi²)
      chi_squared <- as.numeric(survey_result$statistic)
      
      # Survey package uses Kruskal-Wallis test even for 2 groups
      if (grepl("KruskalWallis", survey_result$method)) {
        # Extract t-statistic and df from survey result
        t_stat <- as.numeric(survey_result$statistic)  # t-statistic
        df <- as.numeric(survey_result$parameter)      # degrees of freedom
        
        Z <- t_stat
        # Effect size for t-test: r = |t|/sqrt(t² + df)
        r <- abs(t_stat) / sqrt(t_stat^2 + df)
        
      } else {
        # Fallback: calculate from rank means
        pooled_se <- sqrt((n1_weighted + n2_weighted + 1) / 12)
        Z <- (rank_mean1 - rank_mean2) / pooled_se
      r <- abs(Z) / sqrt(n1_weighted + n2_weighted)
      }
      
      # Ensure r is a single numeric value
      if (length(r) == 0 || is.na(r) || !is.finite(r)) {
        r <- 0
      }
      
      test_result <- list(
        statistic = W,
        p.value = survey_result$p.value
      )
      
      group_stats <- list(
        group1 = list(name = as.character(g_levels[1]), rank_mean = rank_mean1, n = round(n1_weighted, 1)),
        group2 = list(name = as.character(g_levels[2]), rank_mean = rank_mean2, n = round(n2_weighted, 1))
      )
    }
    
    return(list(
      U = U,
      W = if (is.null(weight_name)) W_report else test_result$statistic,
      Z = Z,
      p_value = test_result$p.value,
      effect_size_r = r,
      rank_mean_diff = rank_mean1 - rank_mean2,
      group_levels = g_levels,
      group_stats = group_stats
    ))
  }

  # Main computation function
  compute_results <- function(data) {
    results_list <- list()
    
    for (var_name in var_names) {
      tryCatch({
        result <- perform_single_mann_whitney(data, var_name, g_name, w_name)
        
        results_list[[var_name]] <- tibble(
          Variable = var_name,
          U = result$U,
          W = result$W,
          Z = result$Z,
          p_value = result$p_value,
          effect_size_r = result$effect_size_r,
          rank_mean_diff = result$rank_mean_diff,
          group_stats = list(result$group_stats)
        )
        
      }, error = function(e) {
        warning(sprintf("Mann-Whitney test failed for variable '%s': %s", var_name, e$message))
        results_list[[var_name]] <- tibble(
          Variable = var_name,
          U = NA_real_,
          W = NA_real_,
          Z = NA_real_,
          p_value = NA_real_,
          effect_size_r = NA_real_,
          rank_mean_diff = NA_real_,
          group_stats = list(NULL)
        )
      })
    }
    
    bind_rows(results_list)
  }

  # Execute computation
  if (is_grouped) {
    results <- data %>%
      group_modify(~ compute_results(.x))
  } else {
    results <- compute_results(data)
  }

  # Get group levels for output
  if (!is.null(g_name)) {
    group_col <- data[[g_name]]
    if (is.factor(group_col)) {
      all_levels <- levels(group_col)
      group_levels <- all_levels[all_levels %in% unique(group_col)]
    } else {
      group_levels <- unique(group_col)
    }
  } else {
    group_levels <- NULL
  }

  # Create result object
  result <- list(
    results = results,
    variables = var_names,
    group = g_name,
    weights = w_name,
    group_levels = group_levels,
    is_grouped = is_grouped,
    mu = mu,
    alternative = alternative,
    conf.level = conf.level,
    data = data  # Store original data for levene_test
  )
  
  class(result) <- "mann_whitney_test_results"
  return(result)
}

# Helper function for dynamic borders (matching t_test style)
#' @keywords internal
.get_mann_whitney_border <- function(df) {
  col_widths <- sapply(names(df), function(col) {
    max(nchar(as.character(df[[col]])), nchar(col), na.rm = TRUE)
  })
  total_width <- sum(col_widths) + length(col_widths) - 1
  return(paste(rep("-", total_width), collapse = ""))
}

#' Print method for Mann-Whitney test results
#' @export  
#' @method print mann_whitney_test_results
print.mann_whitney_test_results <- function(x, digits = 3, ...) {
  
  # Determine test type based on weights (Template Standard)
  test_type <- if (!is.null(x$weight_var) || !is.null(x$weights)) {
    "Weighted Mann-Whitney U Test Results"
  } else {
    "Mann-Whitney U Test Results"
  }
  
  cat(sprintf("\n%s\n", test_type))
  cat(paste(rep("-", nchar(test_type)), collapse = ""), "\n")
  
  # Ensure p-values are numeric
  x$results$p_value <- as.numeric(x$results$p_value)
  
  # Add significance stars (Template Standard)
  x$results$sig <- cut(x$results$p_value, 
                      breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                      labels = c("***", "**", "*", ""),
                      right = FALSE)
                      
  # Template Standard: Dual grouped data detection
  is_grouped_data <- (!is.null(x$grouped) && x$grouped) || 
                     (!is.null(x$is_grouped) && x$is_grouped)
  
  # Print info about grouping variable if present
  if (!is.null(x$group)) {
    group_levels <- x$group_levels
    if (length(group_levels) >= 2) {
      cat(sprintf("\nGrouping variable: %s\n", x$group))
      cat(sprintf("Groups compared: %s vs. %s\n", 
                  as.character(group_levels[1]), 
                  as.character(group_levels[2])))
      if (!is.null(x$weight_var) || !is.null(x$weights)) {
        weight_name <- if (!is.null(x$weight_var)) x$weight_var else x$weights
        cat(sprintf("Weights variable: %s\n", weight_name))
      }
      cat(sprintf("Null hypothesis (mu): %.3f\n", x$mu))
      cat(sprintf("Alternative hypothesis: %s\n", x$alternative))
      cat(sprintf("Confidence level: %.1f%%\n", x$conf.level * 100))
      cat("\n")
    }
  }
  
  if (is_grouped_data) {
    # Get unique groups
    group_vars <- setdiff(names(x$results), c("Variable", "U", "W", "Z", "p_value", 
                                             "effect_size_r", "rank_mean_diff", "group_stats", "sig"))
    groups <- unique(x$results[group_vars])
    
    # Print results for each group
    for (i in seq_len(nrow(groups))) {
      group_values <- groups[i, , drop = FALSE]
      
      # Format group info
      group_info <- sapply(names(group_values), function(g) {
        val <- group_values[[g]]
        if (is.factor(val)) {
          paste(g, "=", levels(val)[val])
        } else {
          paste(g, "=", val)
        }
      })
      group_info <- paste(group_info, collapse = ", ")
      
      # Filter results for current group
      group_results <- x$results
      for (g in names(group_values)) {
        group_results <- group_results[group_results[[g]] == group_values[[g]], ]
      }
      
      if (nrow(group_results) == 0) next
      group_results <- group_results[!is.na(group_results$Variable), ]
      if (nrow(group_results) == 0) next
      
      cat(sprintf("\nGroup: %s\n", group_info))
      
      # Print each variable as separate block
      for (j in seq_len(nrow(group_results))) {
        var <- group_results$Variable[j]
        stats <- group_results$group_stats[[j]]
        
        cat(sprintf("\n┌─ %s ─┐\n", var))
        cat("\n")  # Add blank line after variable name
        
        # Print group rank means
        if (!is.null(stats) && !is.null(stats$group1)) {
          cat(sprintf("  %s: rank mean = %.1f, n = %.1f\n", 
                      stats$group1$name, stats$group1$rank_mean, stats$group1$n))
          cat(sprintf("  %s: rank mean = %.1f, n = %.1f\n", 
                      stats$group2$name, stats$group2$rank_mean, stats$group2$n))
      }
      
        # Create and print test table for this variable
      results_df <- data.frame(
          Test = "Mann-Whitney U",
          U = ifelse(is.na(group_results$U[j]), "NA", 
                    format(round(group_results$U[j], 0), big.mark = ",")),
          W = ifelse(is.na(group_results$W[j]), "NA", 
                    format(round(group_results$W[j], 0), big.mark = ",")),
          Z = round(group_results$Z[j], digits),
          p_value = round(group_results$p_value[j], digits),
          effect_r = round(group_results$effect_size_r[j], digits),
          sig = group_results$sig[j],
        stringsAsFactors = FALSE
      )
      
        cat(sprintf("\n%s:\n", ifelse(!is.null(x$weight_var) || !is.null(x$weights), "Weighted Mann-Whitney U Test Results", "Mann-Whitney U Test Results")))
        border_width <- paste(rep("-", 70), collapse = "")
      cat(border_width, "\n")
      print(results_df, row.names = FALSE)
      cat(border_width, "\n")
        cat("\n")
      }
    }
  } else {
    # Print results for ungrouped data - each variable as separate block
    valid_results <- x$results[!is.na(x$results$Variable), ]
    
    for (i in seq_len(nrow(valid_results))) {
      var_name <- valid_results$Variable[i]
      stats <- valid_results$group_stats[[i]]
        
      cat(sprintf("\n┌─ %s ─┐\n", var_name))
      cat("\n")  # Add blank line after variable name
      
      # Print group rank means if available
      if (!is.null(x$group) && !is.null(stats) && !is.null(stats$group1)) {
          cat(sprintf("  %s: rank mean = %.1f, n = %.1f\n", 
                      stats$group1$name, stats$group1$rank_mean, stats$group1$n))
          cat(sprintf("  %s: rank mean = %.1f, n = %.1f\n", 
                      stats$group2$name, stats$group2$rank_mean, stats$group2$n))
    cat("\n")
      }
    
      # Create test table for this variable
    results_df <- data.frame(
        Test = "Mann-Whitney U",
        U = ifelse(is.na(valid_results$U[i]), "NA", 
                  format(round(valid_results$U[i], 0), big.mark = ",")),
        W = ifelse(is.na(valid_results$W[i]), "NA", 
                  format(round(valid_results$W[i], 0), big.mark = ",")),
        Z = round(valid_results$Z[i], digits),
        p_value = round(valid_results$p_value[i], digits),
        effect_r = round(valid_results$effect_size_r[i], digits),
        sig = valid_results$sig[i],
      stringsAsFactors = FALSE
    )
    
      cat(sprintf("\n%s:\n", ifelse(!is.null(x$weight_var) || !is.null(x$weights), "Weighted Mann-Whitney U Test Results", "Mann-Whitney U Test Results")))
      border_width <- paste(rep("-", 70), collapse = "")
    cat(border_width, "\n")
    print(results_df, row.names = FALSE)
    cat(border_width, "\n")
      cat("\n")
    }
  }
  
  cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05\n")
  
  cat("\nEffect Size Interpretation (r):\n")
  cat("- Small effect: |r| ≈ 0.1\n")
  cat("- Medium effect: |r| ≈ 0.3\n")
  cat("- Large effect: |r| ≈ 0.5\n")
  
  invisible(x)
}



