
#' Find Which Specific Groups Differ After ANOVA
#'
#' @description
#' \code{tukey_test()} tells you exactly which groups are different from each other
#' after ANOVA finds overall differences. It's like a follow-up investigation that
#' pinpoints where the differences lie.
#'
#' Think of it as:
#' - ANOVA says "there are differences somewhere"
#' - Tukey test says "specifically, Group A differs from Group C"
#' - A way to make all possible comparisons while controlling error rates
#'
#' @param x ANOVA results from \code{oneway_anova()}
#' @param conf.level Confidence level for intervals (Default: 0.95 = 95%)
#' @param ... Additional arguments (currently unused)
#'
#' @return Pairwise comparison results showing:
#' - Which group pairs are significantly different
#' - Size of the difference between each pair
#' - Adjusted p-values (controlling for multiple comparisons)
#' - Confidence intervals for each difference
#'
#' @details
#' ## Understanding the Results
#'
#' **Adjusted P-values**: Control for multiple comparisons
#' - p < 0.05: Groups are significantly different
#' - p ≥ 0.05: No significant difference between these groups
#' - When you make many comparisons, chance alone could produce false positives
#' - Tukey adjustment protects against this by being more conservative
#'
#' **Mean Differences**:
#' - Positive: First group has higher average than second
#' - Negative: Second group has higher average than first
#' - Zero in confidence interval: No significant difference
#'
#' ## When to Use Tukey Test
#'
#' Use Tukey test when:
#' - Your ANOVA shows significant differences (p < 0.05)
#' - You want to know which specific groups differ
#' - You need to compare all possible pairs
#' - Group sizes are roughly equal
#' - Variances are roughly equal across groups
#'
#' ## Tukey vs. Scheffe
#'
#' **Tukey Test:**
#' - Less conservative (easier to find differences)
#' - Best for equal group sizes
#' - Protects only pairwise comparisons
#' - Narrower confidence intervals
#'
#' **Scheffe Test:**
#' - Most conservative (hardest to find differences)
#' - Best for unequal group sizes
#' - Protects against all possible comparisons
#' - Wider confidence intervals
#'
#' ## Reading the Output
#'
#' Example: "Group A - Group B: Diff = 3.2, p = 0.012"
#' - Group A's average is 3.2 units higher than Group B's
#' - This difference is statistically significant (p < 0.05)
#' - You can be confident these groups truly differ
#'
#' ## Tips for Success
#'
#' - Only run post-hoc tests if ANOVA is significant
#' - Focus on comparisons that make theoretical sense
#' - Consider practical significance, not just statistical
#' - Report both the difference and its confidence interval
#' - Remember: non-significant doesn't mean "exactly equal"
#'
#' @seealso 
#' \code{\link{oneway_anova}} for performing ANOVA tests.
#' 
#' \code{\link[stats]{TukeyHSD}} for the base R Tukey HSD function.
#' 
#' \code{\link{levene_test}} for testing homogeneity of variances.
#' 
#' @references
#' Tukey, J. W. (1949). Comparing individual means in the analysis of variance. 
#' Biometrics, 5(2), 99-114.
#' 
#' Kramer, C. Y. (1956). Extension of multiple range tests to group means with 
#' unequal numbers of replications. Biometrics, 12(3), 307-310.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Perform ANOVA followed by Tukey post-hoc test
#' anova_result <- survey_data %>%
#'   oneway_anova(life_satisfaction, group = education)
#'
#' # Tukey post-hoc comparisons
#' anova_result %>% tukey_test()
#'
#' # With weights
#' anova_weighted <- survey_data %>%
#'   oneway_anova(life_satisfaction, group = education, weights = sampling_weight)
#'
#' anova_weighted %>% tukey_test()
#'
#' # Multiple variables
#' anova_multi <- survey_data %>%
#'   oneway_anova(trust_government, trust_companies, group = education)
#'
#' anova_multi %>% tukey_test()
#'
#' # Grouped analysis
#' anova_grouped <- survey_data %>%
#'   group_by(region) %>%
#'   oneway_anova(life_satisfaction, group = education)
#'
#' anova_grouped %>% tukey_test()
#'
#' @family posthoc
#' @export
tukey_test <- function(x, conf.level = 0.95, ...) {
  UseMethod("tukey_test")
}

#' @export
tukey_test.oneway_anova <- function(x, conf.level = 0.95, ...) {
  
  # Input validation
  if (conf.level <= 0 || conf.level >= 1) {
    cli_abort("{.arg conf.level} must be between 0 and 1.")
  }
  
  # Helper function to perform Tukey test for single variable
  perform_single_tukey <- function(data, var_name, group_name, weight_name = NULL) {
    # Get the variable values
    y <- data[[var_name]]
    g <- data[[group_name]]
    
    # Remove NA values
    valid_indices <- !is.na(y) & !is.na(g)
    if (!is.null(weight_name)) {
      w <- data[[weight_name]]
      valid_indices <- valid_indices & !is.na(w) & w > 0
      w <- w[valid_indices]
    }
    y <- y[valid_indices]
    g <- g[valid_indices]
    
    # Get group levels
    if (is.factor(g)) {
      group_levels <- levels(g)[levels(g) %in% unique(g)]
    } else {
      group_levels <- unique(g)
    }
    
    if (length(group_levels) < 2) {
      return(NULL)
    }
    
    if (is.null(weight_name)) {
      # Unweighted Tukey HSD
      aov_result <- aov(y ~ g)
      tukey_result <- TukeyHSD(aov_result, conf.level = conf.level)
      
      # Extract results
      tukey_data <- tukey_result$g
      comparisons <- rownames(tukey_data)
      
      results_df <- data.frame(
        Variable = var_name,
        Comparison = comparisons,
        Estimate = tukey_data[, "diff"],
        conf_low = tukey_data[, "lwr"],
        conf_high = tukey_data[, "upr"],
        p_adjusted = tukey_data[, "p adj"],
        stringsAsFactors = FALSE
      )
      
    } else {
      # Weighted Tukey HSD (custom implementation)
      
      # Calculate weighted means and effective sample sizes for each group
      group_stats <- lapply(group_levels, function(level) {
        group_indices <- g == level
        group_data <- y[group_indices]
        group_weights <- w[group_indices]
        
        n <- length(group_data)
        weighted_n <- sum(group_weights)
        
        # For SPSS compatibility: Use sum of weights as sample size
        # SPSS treats frequency weights as literal counts
        eff_n <- sum(group_weights)
        
        # Weighted mean and variance (SPSS formula)
        weighted_mean <- sum(group_data * group_weights) / sum(group_weights)
        # SPSS uses Bessel's correction: divide by (sum(weights) - 1)
        weighted_var <- sum(group_weights * (group_data - weighted_mean)^2) / (sum(group_weights) - 1)
        
        list(
          level = level,
          n = n,
          eff_n = eff_n,
          weighted_n = weighted_n,
          mean = weighted_mean,
          var = weighted_var
        )
      })
      names(group_stats) <- group_levels
      
      # Calculate MSE (Mean Square Error) using SPSS approach
      # Use the classical Welch formula components from the ANOVA results
      anova_result <- x$anova_results
      
      # Get MSE from classical ANOVA (within-group variance)
      # For weighted data, we need to calculate weighted MSE
      total_weighted_ss <- 0
      total_weighted_df <- 0
      
      for (level in group_levels) {
        stat <- group_stats[[level]]
        # Weight the sum of squares by effective sample size
        ss_within <- (stat$eff_n - 1) * stat$var
        total_weighted_ss <- total_weighted_ss + ss_within
        total_weighted_df <- total_weighted_df + (stat$eff_n - 1)
      }
      
      mse <- total_weighted_ss / total_weighted_df
      
      # Generate all pairwise comparisons
      n_groups <- length(group_levels)
      comparisons <- combn(group_levels, 2, simplify = FALSE)
      
      results_list <- lapply(comparisons, function(pair) {
        g1 <- pair[1]
        g2 <- pair[2]
        
        stat1 <- group_stats[[g1]]
        stat2 <- group_stats[[g2]]
        
        # Mean difference
        diff <- stat1$mean - stat2$mean
        
        # Standard error using SPSS formula: SE = sqrt(MSE * (1/n1 + 1/n2))
        se <- sqrt(mse * (1/stat1$eff_n + 1/stat2$eff_n))
        
        # t-statistic
        t_stat <- diff / se
        
        # Degrees of freedom for MSE
        df <- total_weighted_df
        
        # Critical value from studentized range distribution
        q_crit <- qtukey(1 - (1 - conf.level), n_groups, df) / sqrt(2)
        
        # Confidence interval
        margin <- q_crit * se
        conf_low <- diff - margin
        conf_high <- diff + margin
        
        # Adjusted p-value using studentized range distribution
        # Convert t-statistic to q-statistic: q = t * sqrt(2)
        q_stat <- abs(t_stat) * sqrt(2)
        p_adjusted <- ptukey(q_stat, n_groups, df, lower.tail = FALSE)
        
        data.frame(
          Variable = var_name,
          Comparison = paste(g1, "-", g2),
          Estimate = diff,
          SE = se,
          t_value = t_stat,
          conf_low = conf_low,
          conf_high = conf_high,
          p_adjusted = p_adjusted,
          stringsAsFactors = FALSE
        )
      })
      
      results_df <- do.call(rbind, results_list)
    }
    
    return(results_df)
  }
  
  # Main execution logic
  all_results <- list()
  
  if (x$is_grouped) {
    # Get original data and split by groups
    data <- x$data
    data_list <- group_split(data)
    group_keys <- group_keys(data)
    
    # Perform Tukey test for each group
    for (i in seq_along(data_list)) {
      group_data <- data_list[[i]]
      group_info <- group_keys[i, , drop = FALSE]
      
      # Perform Tukey for each variable in this group
      for (var_name in x$variables) {
        tryCatch({
          tukey_result <- perform_single_tukey(group_data, var_name, x$group, x$weights)
          
          if (!is.null(tukey_result)) {
            # Add group information - repeat group_info for each row of tukey_result
            n_rows <- nrow(tukey_result)
            group_info_expanded <- group_info[rep(1, n_rows), , drop = FALSE]
            result_with_groups <- cbind(group_info_expanded, tukey_result)
            all_results <- append(all_results, list(result_with_groups))
          }
        }, error = function(e) {
          # Skip this combination if error occurs
        })
      }
    }
    
  } else {
    # Perform Tukey for each variable (ungrouped)
    for (var_name in x$variables) {
      tryCatch({
        tukey_result <- perform_single_tukey(x$data, var_name, x$group, x$weights)
        
        if (!is.null(tukey_result)) {
          all_results <- append(all_results, list(tukey_result))
        }
      }, error = function(e) {
        # Skip this variable if error occurs
      })
    }
  }
  
  # Combine all results
  if (length(all_results) > 0) {
    results_df <- do.call(rbind, all_results)
  } else {
    results_df <- data.frame()
  }
  
  # Create S3 object
  structure(
    list(
      results = results_df,
      variables = x$variables,
      group = x$group,
      weights = x$weights,
      groups = x$groups,
      is_grouped = x$is_grouped,
      conf.level = conf.level,
      anova_results = x  # Store original ANOVA results
    ),
    class = "tukey_test"
  )
}

#' Print Tukey HSD test results
#'
#' @description
#' Print method for objects of class \code{"tukey_test"}. Provides a 
#' formatted display of Tukey post-hoc test results including pairwise 
#' comparisons, confidence intervals, and adjusted p-values.
#'
#' @param x An object of class \code{"tukey_test"} returned by \code{\link{tukey_test}}.
#' @param digits Integer specifying the number of decimal places to display 
#'   for numeric values. Default is \code{3}.
#' @param ... Additional arguments passed to \code{\link[base]{print}}. Currently unused.
#'
#' @details
#' The print method displays:
#' \itemize{
#'   \item Pairwise group comparisons with mean differences
#'   \item Confidence intervals for differences
#'   \item Tukey-adjusted p-values controlling family-wise error rate
#'   \item Significance indicators (* p < 0.05, ** p < 0.01, *** p < 0.001)
#' }
#' 
#' For grouped analyses, results are displayed separately for each group combination.
#' For weighted analyses, effective sample sizes are used in calculations.
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @export
print.tukey_test <- function(x, digits = 3, ...) {
  # Determine test type using standardized helper
  weights_name <- x$weight_var %||% x$weights
  test_type <- get_standard_title("Tukey HSD Post-Hoc Test", weights_name, "Results")
  print_header(test_type, newline_before = FALSE)
  
  # Print basic info
  if (!x$is_grouped) {
    if (length(x$variables) == 1) {
      cat(sprintf("\nDependent Variable: %s\n", x$variables[1]))
    }
    cat(sprintf("Grouping Variable: %s\n", x$group))
    if (!is.null(x$weight_var) || !is.null(x$weights)) {
      cat(sprintf("Weights Variable: %s\n", x$weights))
    }
    cat(sprintf("Confidence level: %.1f%%\n", x$conf.level * 100))
    cat("Family-wise error rate controlled using Tukey HSD\n\n")
  } else {
    cat(sprintf("\nGrouping Variable: %s\n", x$group))
    if (!is.null(x$weight_var) || !is.null(x$weights)) {
      cat(sprintf("Weights Variable: %s\n", x$weights))
    }
    cat(sprintf("Confidence level: %.1f%%\n", x$conf.level * 100))
    cat("Family-wise error rate controlled using Tukey HSD\n\n")
  }
  
  if (nrow(x$results) == 0) {
    cat("No post-hoc comparisons available.\n")
    cat("This may occur when:\n")
    cat("- ANOVA was not significant\n")
    cat("- Groups have insufficient sample sizes\n")
    cat("- Data contains only missing values\n")
    return(invisible(x))
  }
  
  # Add significance stars
  x$results$sig <- cut(x$results$p_adjusted, 
                      breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                      labels = c("***", "**", "*", ""),
                      right = FALSE)
  
  # Template Standard: Dual grouped data detection
  is_grouped_data <- (!is.null(x$grouped) && x$grouped) || (!is.null(x$is_grouped) && x$is_grouped)
  if (is_grouped_data) {
    # Get unique groups
    groups <- unique(x$results[x$groups])
    
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
      
      cat(sprintf("\nGroup: %s\n", group_info))
      
      # Print each variable as separate block
      for (var in x$variables) {
        var_results <- group_results[group_results$Variable == var, ]
        if (nrow(var_results) == 0) next
        
        cat(sprintf("\n--- %s ---\n", var))
        cat("\n")  # Add blank line after variable name
        
        cat(sprintf("%s:\n", ifelse(!is.null(x$weight_var) || !is.null(x$weights), "Weighted Tukey Results", "Tukey Results")))
        
        # Format display table
        display_table <- var_results[, c("Comparison", "Estimate", "conf_low", 
                                        "conf_high", "p_adjusted", "sig")]
        names(display_table) <- c("Comparison", "Difference", "Lower CI", 
                                 "Upper CI", "p-value", "Sig")
        
        # Round numeric columns
        display_table$Difference <- round(display_table$Difference, digits)
        display_table$`Lower CI` <- round(display_table$`Lower CI`, digits)
        display_table$`Upper CI` <- round(display_table$`Upper CI`, digits)
        display_table$`p-value` <- ifelse(display_table$`p-value` < 0.001, 
                                         "<.001", 
                                         round(display_table$`p-value`, digits))
        
        # Calculate border width based on table content
        col_widths <- sapply(names(display_table), function(col) {
          max(nchar(as.character(display_table[[col]])), nchar(col), na.rm = TRUE)
        })
        total_width <- sum(col_widths) + length(col_widths) - 1
        border_width <- paste(rep("-", total_width), collapse = "")
        cat(border_width, "\n")
        print(display_table, row.names = FALSE)
        cat(border_width, "\n")
        cat("\n")
      }
    }
  } else {
    # Print results for ungrouped data - each variable as separate block
    for (var in x$variables) {
      var_results <- x$results[x$results$Variable == var, ]
      if (nrow(var_results) == 0) next
      
      cat(sprintf("\n--- %s ---\n", var))
      cat("\n")  # Add blank line after variable name
      
      cat(sprintf("%s:\n", ifelse(!is.null(x$weight_var) || !is.null(x$weights), "Weighted Tukey Results", "Tukey Results")))
      
      # Format display table
      display_table <- var_results[, c("Comparison", "Estimate", "conf_low", 
                                      "conf_high", "p_adjusted", "sig")]
      names(display_table) <- c("Comparison", "Difference", "Lower CI", 
                               "Upper CI", "p-value", "Sig")
      
      # Round numeric columns
      display_table$Difference <- round(display_table$Difference, digits)
      display_table$`Lower CI` <- round(display_table$`Lower CI`, digits)
      display_table$`Upper CI` <- round(display_table$`Upper CI`, digits)
      display_table$`p-value` <- ifelse(display_table$`p-value` < 0.001, 
                                       "<.001", 
                                       round(display_table$`p-value`, digits))
      
      # Calculate border width based on table content
      col_widths <- sapply(names(display_table), function(col) {
        max(nchar(as.character(display_table[[col]])), nchar(col), na.rm = TRUE)
      })
      total_width <- sum(col_widths) + length(col_widths) - 1
      border_width <- paste(rep("-", total_width), collapse = "")
      cat(border_width, "\n")
      print(display_table, row.names = FALSE)
      cat(border_width, "\n")
      cat("\n")
    }
  }
  
  cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05\n")
  
  cat("\nInterpretation:\n")
  cat("- Positive differences: First group > Second group\n")
  cat("- Negative differences: First group < Second group\n")
  cat("- Confidence intervals not containing 0 indicate significant differences\n")
  cat("- p-values are adjusted for multiple comparisons (family-wise error control)\n")
  
  invisible(x)
} 