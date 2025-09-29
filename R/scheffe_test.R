#' Compare All Groups More Conservatively After ANOVA
#'
#' @description
#' \code{scheffe_test()} tells you which groups differ after ANOVA, using the most
#' conservative approach. It's like Tukey's test but even more careful about
#' avoiding false positives.
#'
#' Think of it as:
#' - The most cautious post-hoc test available
#' - A way to compare groups when sample sizes are very unequal
#' - Insurance against finding differences that aren't real
#'
#' @param x ANOVA results from \code{oneway_anova()}
#' @param conf.level Confidence level for intervals (Default: 0.95 = 95%)
#' @param ... Additional arguments (currently unused)
#'
#' @return Pairwise comparison results showing:
#' - Which group pairs are significantly different
#' - Size of the difference between each pair
#' - Adjusted p-values (extra conservative)
#' - Confidence intervals for each difference
#'
#' @details
#' ## Understanding the Results
#'
#' **Adjusted P-values**: Extra conservative to prevent false positives
#' - p < 0.05: Groups are significantly different (you can be very confident)
#' - p ≥ 0.05: No significant difference between these groups
#' - Scheffe adjustments are stricter than other methods
#'
#' **Confidence Intervals**: Wider than Tukey's
#' - Do not include 0: Groups differ significantly
#' - Include 0: No significant difference
#' - Wider intervals reflect extra caution
#'
#' ## When to Use Scheffe Test
#'
#' Use Scheffe test when:
#' - Your ANOVA shows significant differences (p < 0.05)
#' - Group sizes are very unequal
#' - You want to be extra cautious about false positives
#' - You might test complex comparisons (not just pairs)
#' - Sample sizes are small
#'
#' ## Scheffe vs. Tukey
#'
#' **Scheffe Test:**
#' - Most conservative (hardest to find differences)
#' - Best for unequal group sizes
#' - Protects against all possible comparisons
#' - Wider confidence intervals
#'
#' **Tukey Test:**
#' - Less conservative (easier to find differences)
#' - Best for equal group sizes
#' - Protects only pairwise comparisons
#' - Narrower confidence intervals
#'
#' ## Reading the Output
#'
#' Example: "Group A - Group B: Diff = 3.2, p = 0.082"
#' - Group A's average is 3.2 units higher than Group B's
#' - This difference is NOT significant with Scheffe (p > 0.05)
#' - It might be significant with less conservative tests
#'
#' ## Tips for Success
#'
#' - Scheffe may not find differences even when ANOVA does
#' - This is normal - it's being extra careful
#' - Consider Tukey if group sizes are similar
#' - Report which post-hoc test you used and why
#' - Focus on confidence intervals, not just p-values
#'
#' @seealso
#' \code{\link{oneway_anova}} for performing ANOVA tests.
#'
#' \code{\link{tukey_test}} for Tukey HSD post-hoc tests.
#'
#' \code{\link{levene_test}} for testing homogeneity of variances.
#'
#' @references
#' Scheffe, H. (1953). A method for judging all contrasts in the analysis of variance.
#' Biometrika, 40(1-2), 87-110.
#'
#' Scheffe, H. (1959). The Analysis of Variance. New York: Wiley.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Perform ANOVA followed by Scheffe post-hoc test
#' anova_result <- survey_data %>%
#'   oneway_anova(life_satisfaction, group = education)
#'
#' # Scheffe post-hoc comparisons
#' anova_result %>% scheffe_test()
#'
#' # Multiple variables
#' anova_result_multi <- survey_data %>%
#'   oneway_anova(life_satisfaction, income, group = education)
#'
#' anova_result_multi %>% scheffe_test()
#'
#' # Weighted analysis
#' anova_weighted <- survey_data %>%
#'   oneway_anova(life_satisfaction, group = education, weights = sampling_weight)
#'
#' anova_weighted %>% scheffe_test()
#'
#' # Grouped analysis
#' anova_grouped <- survey_data %>%
#'   group_by(region) %>%
#'   oneway_anova(life_satisfaction, group = education)
#'
#' anova_grouped %>% scheffe_test()
#'
#' # Custom confidence level (99%)
#' anova_result %>% scheffe_test(conf.level = 0.99)
#'
#' @export
scheffe_test <- function(x, conf.level = 0.95, ...) {
  UseMethod("scheffe_test")
}

#' @export
scheffe_test.oneway_anova_results <- function(x, conf.level = 0.95, ...) {

  # Input validation
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1")
  }

  # Helper function to perform Scheffe test for single variable
  perform_single_scheffe <- function(data, var_name, group_name, weight_name = NULL) {
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

    # Calculate group statistics
    if (is.null(weight_name)) {
      # Unweighted statistics
      group_stats <- lapply(group_levels, function(level) {
        group_indices <- g == level
        group_data <- y[group_indices]

        n <- length(group_data)
        mean_val <- mean(group_data, na.rm = TRUE)
        var_val <- var(group_data, na.rm = TRUE)

        list(
          level = level,
          n = n,
          eff_n = n,
          mean = mean_val,
          var = var_val
        )
      })
    } else {
      # Weighted statistics
      group_stats <- lapply(group_levels, function(level) {
        group_indices <- g == level
        group_data <- y[group_indices]
        group_weights <- w[group_indices]

        n <- length(group_data)
        weighted_n <- sum(group_weights)

        # For SPSS compatibility: Use sum of weights as sample size
        eff_n <- sum(group_weights)

        # Weighted mean and variance (SPSS formula)
        weighted_mean <- sum(group_data * group_weights) / sum(group_weights)
        # SPSS uses Bessel's correction: divide by (sum(weights) - 1)
        weighted_var <- sum(group_weights * (group_data - weighted_mean)^2) / (sum(group_weights) - 1)

        list(
          level = level,
          n = n,
          eff_n = eff_n,
          mean = weighted_mean,
          var = weighted_var
        )
      })
    }
    names(group_stats) <- group_levels

    # Calculate MSE (Mean Square Error)
    # Use pooled within-group variance
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

    # Number of groups
    k <- length(group_levels)

    # Generate all pairwise comparisons
    comparisons <- combn(group_levels, 2, simplify = FALSE)

    results_list <- lapply(comparisons, function(pair) {
      g1 <- pair[1]
      g2 <- pair[2]

      stat1 <- group_stats[[g1]]
      stat2 <- group_stats[[g2]]

      # Mean difference
      diff <- stat1$mean - stat2$mean

      # Standard error using SPSS formula
      se <- sqrt(mse * (1/stat1$eff_n + 1/stat2$eff_n))

      # Scheffe F-statistic
      # F = (mean_diff^2) / ((k-1) * MSE * (1/n1 + 1/n2))
      f_stat <- (diff^2) / ((k - 1) * mse * (1/stat1$eff_n + 1/stat2$eff_n))

      # Degrees of freedom
      df1 <- k - 1
      df2 <- total_weighted_df

      # P-value from F-distribution
      p_value <- pf(f_stat, df1, df2, lower.tail = FALSE)

      # Critical value for Scheffe test
      # S = sqrt((k-1) * F_critical)
      f_critical <- qf(conf.level, df1, df2)
      s_critical <- sqrt((k - 1) * f_critical)

      # Confidence interval
      margin <- s_critical * se
      conf_low <- diff - margin
      conf_high <- diff + margin

      data.frame(
        Variable = var_name,
        Comparison = paste(g1, "-", g2),
        Estimate = diff,
        SE = se,
        F_value = f_stat,
        conf_low = conf_low,
        conf_high = conf_high,
        p_adjusted = p_value,
        stringsAsFactors = FALSE
      )
    })

    results_df <- do.call(rbind, results_list)
    return(results_df)
  }

  # Main execution logic
  all_results <- list()

  if (x$is_grouped) {
    # Get original data and split by groups
    data <- x$data
    data_list <- group_split(data)
    group_keys <- group_keys(data)

    # Perform Scheffe test for each group
    for (i in seq_along(data_list)) {
      group_data <- data_list[[i]]
      group_info <- group_keys[i, , drop = FALSE]

      # Perform Scheffe for each variable in this group
      for (var_name in x$variables) {
        tryCatch({
          scheffe_result <- perform_single_scheffe(group_data, var_name, x$group, x$weights)

          if (!is.null(scheffe_result)) {
            # Add group information - repeat group_info for each row of scheffe_result
            n_rows <- nrow(scheffe_result)
            group_info_expanded <- group_info[rep(1, n_rows), , drop = FALSE]
            result_with_groups <- cbind(group_info_expanded, scheffe_result)
            all_results <- append(all_results, list(result_with_groups))
          }
        }, error = function(e) {
          # Skip this combination if error occurs
        })
      }
    }

  } else {
    # Perform Scheffe for each variable (ungrouped)
    for (var_name in x$variables) {
      tryCatch({
        scheffe_result <- perform_single_scheffe(x$data, var_name, x$group, x$weights)

        if (!is.null(scheffe_result)) {
          all_results <- append(all_results, list(scheffe_result))
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
    class = "scheffe_test_results"
  )
}

#' Print Scheffe test results
#'
#' @description
#' Print method for objects of class \code{"scheffe_test_results"}. Provides a
#' formatted display of Scheffe post-hoc test results including pairwise
#' comparisons, confidence intervals, and adjusted p-values.
#'
#' @param x An object of class \code{"scheffe_test_results"} returned by \code{\link{scheffe_test}}.
#' @param digits Integer specifying the number of decimal places to display
#'   for numeric values. Default is \code{3}.
#' @param ... Additional arguments passed to \code{\link[base]{print}}. Currently unused.
#'
#' @details
#' The print method displays:
#' \itemize{
#'   \item Pairwise group comparisons with mean differences
#'   \item Confidence intervals for differences (widest among all post-hoc tests)
#'   \item Scheffe-adjusted p-values controlling family-wise error rate
#'   \item Significance indicators (* p < 0.05, ** p < 0.01, *** p < 0.001)
#' }
#'
#' For grouped analyses, results are displayed separately for each group combination.
#' For weighted analyses, effective sample sizes are used in calculations.
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @export
print.scheffe_test_results <- function(x, digits = 3, ...) {
  # Determine test type using standardized helper
  weights_name <- x$weight_var %||% x$weights
  test_type <- get_standard_title("Scheffe Post-Hoc Test", weights_name, "Results")
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
    cat("Family-wise error rate controlled using Scheffe's method\n")
    cat("Note: Most conservative post-hoc test (widest confidence intervals)\n\n")
  } else {
    cat(sprintf("\nGrouping Variable: %s\n", x$group))
    if (!is.null(x$weight_var) || !is.null(x$weights)) {
      cat(sprintf("Weights Variable: %s\n", x$weights))
    }
    cat(sprintf("Confidence level: %.1f%%\n", x$conf.level * 100))
    cat("Family-wise error rate controlled using Scheffe's method\n")
    cat("Note: Most conservative post-hoc test (widest confidence intervals)\n\n")
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

        cat(sprintf("%s:\n", ifelse(!is.null(x$weight_var) || !is.null(x$weights), "Weighted Scheffe Results", "Scheffe Results")))

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

      cat(sprintf("%s:\n", ifelse(!is.null(x$weight_var) || !is.null(x$weights), "Weighted Scheffe Results", "Scheffe Results")))

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
  cat("- p-values are adjusted for all possible contrasts (most conservative)\n")
  cat("- Scheffe test has wider CIs than Tukey HSD\n")

  invisible(x)
}