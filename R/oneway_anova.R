
#' Compare Multiple Groups: Are Their Averages Different?
#'
#' @description
#' \code{oneway_anova()} helps you determine if average values differ across
#' three or more groups. For example, does average income vary by education level?
#' Or is customer satisfaction different across regions?
#'
#' Think of it as:
#' - An extension of t-test for more than two groups
#' - A way to test if group membership affects outcomes
#' - A tool to identify meaningful group differences
#'
#' The test tells you:
#' - Whether at least one group is different from the others
#' - How much variation is explained by group membership
#' - Which variance assumption fits your data best
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The numeric variables you want to analyze. You can list multiple
#'   variables or use helpers like \code{starts_with("income")}
#' @param group The categorical variable that defines your groups (e.g., education,
#'   region, age_group). Must have at least 3 groups for ANOVA.
#' @param weights Optional survey weights for population-representative results
#' @param var.equal Should we assume all groups have similar variance? (Default: TRUE)
#'   - TRUE: Standard ANOVA (assumes equal variances)
#'   - FALSE: Welch's ANOVA (allows unequal variances)
#' @param conf.level Confidence level for group statistics (Default: 0.95 = 95%)
#'
#' @return ANOVA results showing whether groups differ, including:
#' - F-statistic and p-value (are groups different?)
#' - Effect sizes (how much do groups matter?)
#' - Group statistics (means and standard deviations)
#' - Both standard and Welch ANOVA results
#'
#' @details
#' ## Understanding the Results
#'
#' **P-value**: If p < 0.05, at least one group average is different
#' - p < 0.001: Very strong evidence of group differences
#' - p < 0.01: Strong evidence of group differences
#' - p < 0.05: Moderate evidence of group differences
#' - p ≥ 0.05: No significant group differences found
#'
#' **Effect Sizes** (How much do groups matter?):
#' - **Eta-squared**: Proportion of variance explained by groups
#'   - < 0.01: Negligible effect
#'   - 0.01-0.06: Small effect
#'   - 0.06-0.14: Medium effect
#'   - 0.14 or higher: Large effect
#' - **Omega-squared**: More conservative estimate (usually preferred)
#'
#' ## When to Use This
#'
#' Use ANOVA when:
#' - You have one numeric outcome (income, satisfaction score, etc.)
#' - You have one categorical grouping variable with 3+ groups
#' - You want to know if group averages differ
#' - Your data is roughly normally distributed within groups
#'
#' ## Choosing Variance Assumptions
#'
#' - **Standard ANOVA** (var.equal = TRUE): Use when group variances are similar
#' - **Welch's ANOVA** (var.equal = FALSE): Use when group variances differ or you're unsure
#' - The function shows both results for comparison
#'
#' ## What Comes Next?
#'
#' If ANOVA is significant:
#' 1. Look at group means to see the pattern
#' 2. Use \code{tukey_test()} to find which specific groups differ
#' 3. Consider effect sizes to judge practical importance
#'
#' ## Tips for Success
#'
#' - Check that each group has sufficient observations (ideally 20+)
#' - Look at both p-values and effect sizes
#' - Use Welch's ANOVA if group sizes are very unequal
#' - Follow up with post-hoc tests to identify specific differences
#' - Welch's robust test for equality of means
#' 
#' ## Interpretation Guidelines
#' - **p-values**: p < 0.05 indicates significant group differences at alpha = 0.05
#' - **Effect sizes**: eta-squared ~ 0.01 (small), eta-squared ~ 0.06 (medium), eta-squared ~ 0.14 (large)
#' - **Eta-squared**: Proportion of variance explained (tends to overestimate)
#' - **Epsilon-squared**: Less biased than eta-squared
#' - **Omega-squared**: Unbiased estimate (preferred for publication)
#'
#' @seealso 
#' \code{\link[stats]{aov}} for the base R ANOVA function.
#' 
#' \code{\link[stats]{oneway.test}} for Welch's ANOVA.
#' 
#' \code{\link{tukey_test}} for post-hoc pairwise comparisons.
#' 
#' \code{\link{levene_test}} for testing homogeneity of variances.
#' 
#' @references
#' Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). 
#' Lawrence Erlbaum Associates.
#' 
#' Welch, B. L. (1951). On the comparison of several mean values: an alternative approach. 
#' Biometrika, 38(3/4), 330-336.
#' 
#' Olejnik, S., & Algina, J. (2003). Generalized eta and omega squared statistics: 
#' measures of effect size for some common research designs. Psychological Methods, 8(4), 434-447.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#' 
#' # Basic one-way ANOVA (comparing across education levels)
#' survey_data %>%
#'   oneway_anova(life_satisfaction, group = education)
#' 
#' # Multiple dependent variables
#' survey_data %>%
#'   oneway_anova(life_satisfaction, trust_government, group = education)
#' 
#' # Using tidyselect helpers
#' survey_data %>%
#'   oneway_anova(starts_with("trust_"), group = education)
#' 
#' # Weighted analysis
#' survey_data %>%
#'   oneway_anova(income, group = education, weights = sampling_weight)
#' 
#' # Grouped analysis (separate ANOVA for each region)
#' survey_data %>%
#'   group_by(region) %>%
#'   oneway_anova(life_satisfaction, group = education)
#' 
#' # Unequal variances (Welch's ANOVA)
#' survey_data %>%
#'   oneway_anova(income, group = education, var.equal = FALSE)
#' 
#' # Store results for post-hoc analysis
#' result <- survey_data %>%
#'   oneway_anova(life_satisfaction, group = education)
#' 
#' # Follow up with post-hoc tests
#' result %>% tukey_test()
#' result %>% levene_test()  # Check homogeneity of variances
#'
#' # Note: For repeated measures ANOVA, use rm_anova_test() function instead
#'
#' @family hypothesis_tests
#' @export
oneway_anova <- function(data, ..., group, weights = NULL, var.equal = TRUE, 
                      conf.level = 0.95) {
  
  # Input validation
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }
  
  if (conf.level <= 0 || conf.level >= 1) {
    cli_abort("{.arg conf.level} must be between 0 and 1.")
  }
  
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
  
  if (length(var_names) == 0) {
    cli_abort("At least one dependent variable must be specified.")
  }

  # Validate that selected variables are numeric
  for (vn in var_names) {
    if (!is.numeric(data[[vn]])) {
      cli_abort("Variable {.var {vn}} is not numeric. {.fn oneway_anova} requires numeric dependent variables.")
    }
  }

  # Group variable (required for ANOVA)
  if (quo_is_null(group_quo)) {
    cli_abort("{.arg group} is required for ANOVA.")
  }
  
  g_var <- eval_select(expr(!!group_quo), data = data)
  g_name <- names(g_var)
  
  # Weights variable (optional)
  if (!quo_is_null(weights_quo)) {
    w_var <- eval_select(expr(!!weights_quo), data = data)
    w_name <- names(w_var)
  } else {
    w_name <- NULL
  }
  
  # Validate and prepare grouping variable
  g_values <- data[[g_name]]
  
  # Handle different variable types for grouping
  if (is.factor(g_values)) {
    # Factor: use existing levels that have data
    g_levels <- levels(g_values)[levels(g_values) %in% unique(g_values)]
  } else if (is.character(g_values) || is.numeric(g_values)) {
    # Non-factor: get unique values and convert to factor for ANOVA
    g_levels <- unique(g_values[!is.na(g_values)])
    # Convert to factor for proper ANOVA handling
    data[[g_name]] <- factor(data[[g_name]])
    g_values <- data[[g_name]]
  } else {
    cli_abort("Grouping variable {.var {g_name}} must be numeric, character, or factor.")
  }
  
  if (length(g_levels) < 2) {
    cli_abort(c(
      "Grouping variable {.var {g_name}} must have at least 2 unique values for ANOVA.",
      "x" = "Found {length(g_levels)} level{?s}."
    ))
  }
  
  # Perform standard between-subjects ANOVA
  return(perform_between_subjects_anova(data, var_names, g_name, w_name, conf.level, var.equal, 
                                      is_grouped, group_vars, g_levels))
}

# Helper function to perform standard between-subjects ANOVA  
perform_between_subjects_anova <- function(data, var_names, group_name, weight_name, conf.level, var.equal, is_grouped, group_vars, g_levels) {
  
  # Helper function to perform single ANOVA
  perform_single_anova <- function(data, var_name, group_name, weight_name = NULL) {
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
    
    if (length(unique(g)) < 2) {
      cli_abort("Variable {.var {var_name}}: After removing NAs, grouping variable must have at least 2 levels.")
    }
    
    # Get group levels
    if (is.factor(g)) {
      group_levels <- levels(g)[levels(g) %in% unique(g)]
    } else {
      group_levels <- unique(g)
    }
    
    if (is.null(weight_name)) {
      # Unweighted ANOVA
      
      # Descriptive statistics by group
      group_stats <- purrr::map(group_levels, function(level) {
        group_data <- y[g == level]
        n <- length(group_data)
        mean_val <- mean(group_data)
        sd_val <- sd(group_data)
        se_val <- sd_val / sqrt(n)
        
        # Confidence interval for mean
        t_val <- qt((1 + conf.level) / 2, df = n - 1)
        ci_lower <- mean_val - t_val * se_val
        ci_upper <- mean_val + t_val * se_val
        
        list(
          level = level,
          n = n,
          mean = mean_val,
          sd = sd_val,
          se = se_val,
          ci_lower = ci_lower,
          ci_upper = ci_upper
        )
      })
      names(group_stats) <- group_levels
      
      # Standard ANOVA
      aov_result <- aov(y ~ g)
      aov_summary <- summary(aov_result)[[1]]
      
      # Extract ANOVA components
      ss_between <- aov_summary["g", "Sum Sq"]
      ss_within <- aov_summary["Residuals", "Sum Sq"]
      ss_total <- ss_between + ss_within
      df_between <- aov_summary["g", "Df"]
      df_within <- aov_summary["Residuals", "Df"]
      ms_between <- aov_summary["g", "Mean Sq"]
      ms_within <- aov_summary["Residuals", "Mean Sq"]
      f_stat <- aov_summary["g", "F value"]
      p_value <- aov_summary["g", "Pr(>F)"]
      
      # Welch's ANOVA (robust test)
      welch_result <- oneway.test(y ~ g, var.equal = FALSE)
      
      # Effect sizes
      eta_squared <- ss_between / ss_total
      epsilon_squared <- (ss_between - (df_between * ms_within)) / ss_total
      omega_squared <- (ss_between - (df_between * ms_within)) / (ss_total + ms_within)
      
      # Ensure effect sizes are non-negative
      epsilon_squared <- max(0, epsilon_squared)
      omega_squared <- max(0, omega_squared)
      
    } else {
      # Weighted ANOVA
      
      # Weighted descriptive statistics by group
      group_stats <- purrr::map(group_levels, function(level) {
        group_indices <- g == level
        group_data <- y[group_indices]
        group_weights <- w[group_indices]
        
        n <- length(group_data)
        weighted_n <- sum(group_weights)
        
        # Weighted mean and variance
        weighted_mean <- sum(group_data * group_weights) / sum(group_weights)
        weighted_var <- sum(group_weights * (group_data - weighted_mean)^2) / sum(group_weights)
        weighted_sd <- sqrt(weighted_var)
        weighted_se <- weighted_sd / sqrt(n)  # Use actual sample size for SE
        
        # Approximate confidence interval (using effective sample size)
        eff_n <- sum(group_weights)^2 / sum(group_weights^2)
        t_val <- qt((1 + conf.level) / 2, df = eff_n - 1)
        ci_lower <- weighted_mean - t_val * weighted_se
        ci_upper <- weighted_mean + t_val * weighted_se
        
        list(
          level = level,
          n = n,
          weighted_n = weighted_n,
          mean = weighted_mean,
          sd = weighted_sd,
          se = weighted_se,
          ci_lower = ci_lower,
          ci_upper = ci_upper
        )
      })
      names(group_stats) <- group_levels
      
      # Weighted ANOVA calculations
      # Overall weighted mean
      grand_mean <- sum(y * w) / sum(w)
      
      # Between-group sum of squares (weighted)
      ss_between <- 0
      for (level in group_levels) {
        group_indices <- g == level
        group_weights <- w[group_indices]
        group_mean <- sum(y[group_indices] * group_weights) / sum(group_weights)
        ss_between <- ss_between + sum(group_weights) * (group_mean - grand_mean)^2
      }
      
      # Within-group sum of squares (weighted)
      ss_within <- 0
      for (level in group_levels) {
        group_indices <- g == level
        group_data <- y[group_indices]
        group_weights <- w[group_indices]
        group_mean <- sum(group_data * group_weights) / sum(group_weights)
        ss_within <- ss_within + sum(group_weights * (group_data - group_mean)^2)
      }
      
      ss_total <- ss_between + ss_within
      df_between <- length(group_levels) - 1
      
      # For weighted analyses, use effective sample size for df calculation (SPSS style)
      if (!is.null(weight_name)) {
        df_within <- round(sum(w)) - length(group_levels)  # Total weighted N (rounded) minus number of groups
      } else {
        df_within <- length(y) - length(group_levels)  # Classical df
      }
      
      ms_between <- ss_between / df_between
      ms_within <- ss_within / df_within
      f_stat <- ms_between / ms_within
      p_value <- 1 - pf(f_stat, df_between, df_within)
      
      # Weighted Welch test (SPSS-optimized)
      # Calculate group statistics for SPSS-compatible Welch test
      w_total_groups <- numeric(length(group_levels))
      group_means_welch <- numeric(length(group_levels))
      group_vars_welch <- numeric(length(group_levels))
      
      for (i in seq_along(group_levels)) {
        level <- group_levels[i]
        group_indices <- g == level
        group_data <- y[group_indices]
        group_weights <- w[group_indices]
        
        w_total_groups[i] <- sum(group_weights)
        group_means_welch[i] <- sum(group_data * group_weights) / sum(group_weights)
        
        # SPSS-compatible variance calculation
        group_vars_welch[i] <- sum(group_weights * (group_data - group_means_welch[i])^2) / (sum(group_weights) - 1)
      }
      
      # Overall weighted mean
      grand_mean_welch <- sum(w_total_groups * group_means_welch) / sum(w_total_groups)
      
      # Classical Welch ANOVA with frequency weights (SPSS-compatible)
      # This is the standard textbook formula that SPSS uses
      # Frequency weights treat weights as replications of observations
      
      # Welch weights: w_i / s_i^2 (effective sample size / variance)
      welch_weights <- w_total_groups / group_vars_welch
      
      # Weighted grand mean for Welch test
      grand_mean_welch <- sum(welch_weights * group_means_welch) / sum(welch_weights)
      
      # Classical Welch F-statistic
      numerator_welch <- sum(welch_weights * (group_means_welch - grand_mean_welch)^2) / (length(group_levels) - 1)
      denominator_welch <- sum((1 - welch_weights / sum(welch_weights))^2 / (w_total_groups - 1))
      welch_f <- numerator_welch / (1 + 2 * (length(group_levels) - 2) / (length(group_levels)^2 - 1) * denominator_welch)
      
      df1_welch <- length(group_levels) - 1
      
      # Satterthwaite degrees of freedom approximation
      df2_welch <- (length(group_levels)^2 - 1) / (3 * denominator_welch)
      
      welch_p <- pf(welch_f, df1_welch, df2_welch, lower.tail = FALSE)
      
              welch_result <- list(
          statistic = welch_f,
          parameter = c(df1_welch, df2_welch),
          p.value = welch_p,
          method = "Weighted Welch ANOVA (Classical Formula)"
        )
      
      # Effect sizes (weighted)
      eta_squared <- ss_between / ss_total
      epsilon_squared <- (ss_between - (df_between * ms_within)) / ss_total
      omega_squared <- (ss_between - (df_between * ms_within)) / (ss_total + ms_within)
      
      # Ensure effect sizes are non-negative
      epsilon_squared <- max(0, epsilon_squared)
      omega_squared <- max(0, omega_squared)
    }
    
    # ANOVA table - always show classical ANOVA values like SPSS
    anova_table <- data.frame(
      Source = c("Between Groups", "Within Groups", "Total"),
      Sum_Squares = c(ss_between, ss_within, ss_total),
      df = c(df_between, df_within, df_between + df_within),
      Mean_Square = c(ms_between, ms_within, ""),
      F = c(f_stat, "", ""),
      p_value = c(p_value, "", ""),
      stringsAsFactors = FALSE
    )
    
    # Always return classical ANOVA as primary (like SPSS), Welch test is separate
    return(list(
      f_stat = f_stat,
      df1 = df_between,
      df2 = df_within,
      p_value = p_value,
      eta_squared = eta_squared,
      epsilon_squared = epsilon_squared,
      omega_squared = omega_squared,
      group_stats = group_stats,
      anova_table = anova_table,
      welch_result = welch_result,
      is_weighted = !is.null(weight_name)
    ))
  }
  
  # Main execution logic
  if (is_grouped) {
    # Split data by groups
    data_list <- group_split(data)
    group_keys <- group_keys(data)
    
    # Perform ANOVA for each group
    results_list <- lapply(seq_along(data_list), function(i) {
      group_data <- data_list[[i]]
      group_info <- group_keys[i, , drop = FALSE]
      
      # Perform ANOVA for each variable in this group
      group_results <- lapply(var_names, function(var_name) {
        tryCatch({
          anova_result <- perform_single_anova(group_data, var_name, group_name, weight_name)
          
          result_df <- data.frame(
            group_info,
            Variable = var_name,
            F_stat = anova_result$f_stat,
            df1 = anova_result$df1,
            df2 = anova_result$df2,
            p_value = anova_result$p_value,
            eta_squared = anova_result$eta_squared,
            epsilon_squared = anova_result$epsilon_squared,
            omega_squared = anova_result$omega_squared
          )
          result_df$group_stats <- list(anova_result$group_stats)
          result_df$anova_table <- list(anova_result$anova_table)
          result_df$welch_result <- list(anova_result$welch_result)
          result_df$is_weighted <- anova_result$is_weighted
          result_df
        }, error = function(e) {
          # Ensure var_name is scalar
          var_name_safe <- if(length(var_name) == 0) "unknown" else as.character(var_name[1])
          
          # Create error data frame with explicit dimensions
          error_df <- data.frame(
            Variable = var_name_safe,
            F_stat = NA,
            df1 = NA,
            df2 = NA,
            p_value = NA,
            eta_squared = NA,
            epsilon_squared = NA,
            omega_squared = NA,
            group_stats = I(list(NULL)),
            anova_table = I(list(NULL)),
            welch_result = I(list(NULL)),
            is_weighted = NA,
            stringsAsFactors = FALSE
          )
          
          # Combine with group_info
          cbind(group_info, error_df)
        })
      })
      
      do.call(rbind, group_results)
    })
    
    results_df <- do.call(rbind, results_list)
    
  } else {
    # Perform ANOVA for each variable (ungrouped)
    results_list <- lapply(var_names, function(var_name) {
      tryCatch({
        anova_result <- perform_single_anova(data, var_name, group_name, weight_name)
        
        result_df <- data.frame(
          Variable = var_name,
          F_stat = anova_result$f_stat,
          df1 = anova_result$df1,
          df2 = anova_result$df2,
          p_value = anova_result$p_value,
          eta_squared = anova_result$eta_squared,
          epsilon_squared = anova_result$epsilon_squared,
          omega_squared = anova_result$omega_squared
        )
        result_df$group_stats <- list(anova_result$group_stats)
        result_df$anova_table <- list(anova_result$anova_table)
        result_df$welch_result <- list(anova_result$welch_result)
        result_df$is_weighted <- anova_result$is_weighted
        result_df
      }, error = function(e) {
        stop(e$message)
      })
    })
    
    results_df <- do.call(rbind, results_list)
  }
  
  # Create S3 object
  structure(
    list(
      results = results_df,
      variables = var_names,
      group = group_name,
      weights = weight_name,
      var.equal = var.equal,
      groups = group_vars,
      is_grouped = is_grouped,
      conf.level = conf.level,
      group_levels = g_levels,
      data = data[, unique(c(var_names, group_name, weight_name, group_vars)), drop = FALSE]
    ),
    class = "oneway_anova"
  )
}

#' Print ANOVA test results
#'
#' @description
#' Print method for objects of class \code{"oneway_anova"}. Provides a 
#' formatted display of ANOVA results including descriptive statistics, ANOVA 
#' tables, assumption tests, and effect sizes.
#'
#' @param x An object of class \code{"oneway_anova"} returned by \code{\link{oneway_anova}}.
#' @param digits Integer specifying the number of decimal places to display 
#'   for numeric values. Default is \code{3}.
#' @param ... Additional arguments passed to \code{\link[base]{print}}. Currently unused.
#'
#' @details
#' The print method displays:
#' \itemize{
#'   \item Descriptive statistics by group (means, standard deviations, sample sizes)
#'   \item Complete ANOVA table (Sum of Squares, degrees of freedom, F-statistic, p-value)
#'   \item Welch's robust test for unequal variances
#'   \item Effect sizes (Eta-squared, Epsilon-squared, Omega-squared) with interpretation
#'   \item Significance indicators (* p < 0.05, ** p < 0.01, *** p < 0.001)
#' }
#' 
#' For grouped analyses (when data is grouped with \code{\link[dplyr]{group_by}}), 
#' results are displayed separately for each group combination.
#' 
#' For weighted analyses, both actual sample sizes and effective sample sizes 
#' (weighted n) are displayed.
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @export
print.oneway_anova <- function(x, digits = 3, ...) {
  
  # Check if this is repeated measures ANOVA
  if (!is.null(x$repeated) && x$repeated) {
    return(print_repeated_measures_anova(x, digits, ...))
  }
  # Determine test type based on weights
  test_type <- get_standard_title("One-Way ANOVA", x$weights, "Results")
  print_header(test_type)
  
  # Print basic info
  if (!x$is_grouped) {
    if (length(x$variables) == 1) {
      cat(sprintf("\nDependent Variable: %s\n", x$variables[1]))
    }
    cat(sprintf("Grouping Variable: %s\n", x$group))
    if (!is.null(x$weights)) {
      cat(sprintf("Weights Variable: %s\n", x$weights))
    }
    cat(sprintf("Null hypothesis: All group means are equal%s\n", 
                ifelse(length(x$variables) > 1, " for each variable", "")))
    cat(sprintf("Alternative hypothesis: At least one group mean differs%s\n",
                ifelse(length(x$variables) > 1, " for each variable", "")))
    cat(sprintf("Confidence level: %.1f%%\n", x$conf.level * 100))
    cat("\n")
  } else {
    cat(sprintf("\nGrouping Variable: %s\n", x$group))
    if (!is.null(x$weights)) {
      cat(sprintf("Weights Variable: %s\n", x$weights))
    }
    cat(sprintf("Null hypothesis: All group means are equal%s\n", 
                ifelse(length(x$variables) > 1, " for each variable", "")))
    cat(sprintf("Alternative hypothesis: At least one group mean differs%s\n",
                ifelse(length(x$variables) > 1, " for each variable", "")))
    cat(sprintf("Confidence level: %.1f%%\n", x$conf.level * 100))
    cat("\n")
  }
  
  # Add significance stars (handle both numeric and character p_values)
  p_values <- x$results$p_value
  if (is.character(p_values)) {
    # Convert character p_values to numeric, handling "n/a"
    p_numeric <- suppressWarnings(as.numeric(p_values))
    p_numeric[is.na(p_numeric)] <- 1  # Set "n/a" to 1 (non-significant)
  } else {
    p_numeric <- p_values
  }

  # Create significance stars properly
  sig_stars <- rep("", length(p_numeric))
  sig_stars[p_numeric < 0.001] <- "***"
  sig_stars[p_numeric >= 0.001 & p_numeric < 0.01] <- "**"
  sig_stars[p_numeric >= 0.01 & p_numeric < 0.05] <- "*"
  sig_stars[p_numeric >= 0.05] <- ""
  
  x$results$sig <- sig_stars
  
  # Template Standard: Dual grouped data detection
  is_grouped_data <- (!is.null(x$grouped) && x$grouped) || (!is.null(x$is_grouped) && x$is_grouped)
  if (is_grouped_data) {
    # Get unique groups
    groups <- unique(x$results[x$groups])
    
    # Print results for each group
    for (i in seq_len(nrow(groups))) {
      group_values <- groups[i, , drop = FALSE]
      
      # Filter results for current group
      group_results <- x$results
      for (g in names(group_values)) {
        group_results <- group_results[group_results[[g]] == group_values[[g]], ]
      }

      if (nrow(group_results) == 0) next
      group_results <- group_results[!is.na(group_results$Variable), ]
      if (nrow(group_results) == 0) next

      # Print group header using standardized helper
      print_group_header(group_values)
      
      # Print each variable as separate block
      for (j in seq_len(nrow(group_results))) {
        var <- group_results$Variable[j]
        stats <- group_results$group_stats[[j]]
        anova_table <- group_results$anova_table[[j]]
        welch_result <- group_results$welch_result[[j]]
        
        cat(sprintf("\n--- %s ---\n", var))
        cat("\n")  # Add blank line after variable name
        
        # Print descriptive statistics
        if (!is.null(stats)) {
          if (!is.null(x$weights)) {
            cat("Weighted Descriptive Statistics by Group:\n")
            for (level in names(stats)) {
              stat <- stats[[level]]
              cat(sprintf("  %s: mean = %.3f, sd = %.3f, n = %.1f\n", 
                          level, stat$mean, stat$sd, stat$weighted_n))
            }
          } else {
            cat("Descriptive Statistics by Group:\n")
            for (level in names(stats)) {
              stat <- stats[[level]]
              cat(sprintf("  %s: mean = %.3f, sd = %.3f, n = %.0f\n", 
                          level, stat$mean, stat$sd, stat$n))
            }
          }
        }
        
        # Print ANOVA table
        if (!is.null(anova_table)) {
          cat(sprintf("\n%s:\n", ifelse(!is.null(x$weights), "Weighted ANOVA Results", "ANOVA Results")))
          
                  # Format ANOVA table for display
        display_table <- anova_table
        display_table$Sum_Squares <- round(display_table$Sum_Squares, 3)
        # Handle Mean_Square column carefully
        display_table$Mean_Square <- ifelse(display_table$Mean_Square == "", "", 
                                           round(as.numeric(display_table$Mean_Square), 3))
        # Handle F column carefully  
        display_table$F <- ifelse(display_table$F == "", "", 
                                 round(as.numeric(display_table$F), 3))
        # Handle p_value column
        display_table$p_value <- ifelse(display_table$p_value == "", "", 
                                       ifelse(as.numeric(display_table$p_value) < 0.001, "<.001", 
                                             round(as.numeric(display_table$p_value), 3)))
          
          # Add significance stars to F row
          f_sig <- group_results$sig[j]
          display_table$sig <- c(f_sig, "", "")
          
          # Create box around ANOVA table like t-test
          border_width <- paste(rep("-", 80), collapse = "")
          cat(border_width, "\n")
          print(display_table, row.names = FALSE, na.print = "")
          cat(border_width, "\n")
        }
        
        # Print assumption tests (Welch)
        if (!is.null(welch_result)) {
          cat("\nAssumption Tests:\n")
          cat(paste(rep("-", 16), collapse = ""), "\n")
          
          welch_p <- welch_result$p.value
          welch_sig <- cut(welch_p, 
                          breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                          labels = c("***", "**", "*", ""),
                          right = FALSE)
          
          welch_table <- data.frame(
            Assumption = "Welch",
            Statistic = round(welch_result$statistic, 3),
            df1 = welch_result$parameter[1],
            df2 = round(welch_result$parameter[2], 0),
            p_value = ifelse(welch_p < 0.001, "<.001", round(welch_p, 3)),
            sig = welch_sig
          )
          
          print(welch_table, row.names = FALSE)
        }
        
        # Print effect sizes
        eta_val <- group_results$eta_squared[j]
        epsilon_val <- group_results$epsilon_squared[j]
        omega_val <- group_results$omega_squared[j]
        
        if (!is.na(eta_val)) {
          effect_size_category <- if (abs(eta_val) < 0.01) "negligible" else
                                 if (abs(eta_val) < 0.06) "small" else
                                 if (abs(eta_val) < 0.14) "medium" else "large"
          
          effect_df <- data.frame(
            Variable = var,
            Eta_Squared = round(eta_val, 3),
            Epsilon_Squared = round(epsilon_val, 3),
            Omega_Squared = round(omega_val, 3),
            Effect_Size = effect_size_category,
            stringsAsFactors = FALSE
          )
          
          cat("\nEffect Sizes:\n")
          cat(paste(rep("-", 12), collapse = ""), "\n")
          print(effect_df, row.names = FALSE)
        }
        cat("\n")
      }
    }
  } else {
    # Print results for ungrouped data - each variable as separate block
    valid_results <- x$results[!is.na(x$results$Variable), ]
    
    for (i in seq_len(nrow(valid_results))) {
      var_name <- valid_results$Variable[i]
      stats <- valid_results$group_stats[[i]]
      anova_table <- valid_results$anova_table[[i]]
      welch_result <- valid_results$welch_result[[i]]
      
      cat(sprintf("\n--- %s ---\n", var_name))
      cat("\n")  # Add blank line after variable name
      
      # Print descriptive statistics
      if (!is.null(stats)) {
        if (!is.null(x$weights)) {
          cat("Weighted Descriptive Statistics by Group:\n")
          for (level in names(stats)) {
            stat <- stats[[level]]
            cat(sprintf("  %s: mean = %.3f, sd = %.3f, n = %.1f\n", 
                        level, stat$mean, stat$sd, stat$weighted_n))
          }
        } else {
          cat("Descriptive Statistics by Group:\n")
          for (level in names(stats)) {
            stat <- stats[[level]]
            cat(sprintf("  %s: mean = %.3f, sd = %.3f, n = %.0f\n", 
                        level, stat$mean, stat$sd, stat$n))
          }
        }
      }
      
      # Print ANOVA table
      if (!is.null(anova_table)) {
        cat(sprintf("\n%s:\n", ifelse(!is.null(x$weights), "Weighted ANOVA Results", "ANOVA Results")))
        
        # Format ANOVA table for display
        display_table <- anova_table
        display_table$Sum_Squares <- round(display_table$Sum_Squares, 3)
        # Handle Mean_Square column carefully
        display_table$Mean_Square <- ifelse(display_table$Mean_Square == "", "", 
                                           round(as.numeric(display_table$Mean_Square), 3))
        # Handle F column carefully  
        display_table$F <- ifelse(display_table$F == "", "", 
                                 round(as.numeric(display_table$F), 3))
        # Handle p_value column
        display_table$p_value <- ifelse(display_table$p_value == "", "", 
                                       ifelse(as.numeric(display_table$p_value) < 0.001, "<.001", 
                                             round(as.numeric(display_table$p_value), 3)))
        
            # Add significance stars to F row
    f_sig <- valid_results$sig[i]
    display_table$sig <- c(f_sig, "", "")
    
    # Create box around ANOVA table like t-test
    border_width <- paste(rep("-", 80), collapse = "")
    cat(border_width, "\n")
    print(display_table, row.names = FALSE, na.print = "")
    cat(border_width, "\n")
      }
      
      # Print assumption tests (Welch)
      if (!is.null(welch_result)) {
        cat("\nAssumption Tests:\n")
        cat(paste(rep("-", 16), collapse = ""), "\n")
        
        welch_p <- welch_result$p.value
        welch_sig <- cut(welch_p, 
                        breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                        labels = c("***", "**", "*", ""),
                        right = FALSE)
        
        welch_table <- data.frame(
          Assumption = "Welch",
          Statistic = round(welch_result$statistic, 3),
          df1 = welch_result$parameter[1],
          df2 = round(welch_result$parameter[2], 0),
          p_value = ifelse(welch_p < 0.001, "<.001", round(welch_p, 3)),
          sig = welch_sig
        )
        
        print(welch_table, row.names = FALSE)
      }
      
      # Print effect sizes
      eta_val <- valid_results$eta_squared[i]
      epsilon_val <- valid_results$epsilon_squared[i]
      omega_val <- valid_results$omega_squared[i]
      
      if (!is.na(eta_val)) {
        effect_size_category <- if (abs(eta_val) < 0.01) "negligible" else
                               if (abs(eta_val) < 0.06) "small" else
                               if (abs(eta_val) < 0.14) "medium" else "large"
        
        effect_df <- data.frame(
          Variable = var_name,
          Eta_Squared = round(eta_val, 3),
          Epsilon_Squared = round(epsilon_val, 3),
          Omega_Squared = round(omega_val, 3),
          Effect_Size = effect_size_category,
          stringsAsFactors = FALSE
        )
        
        cat("\nEffect Sizes:\n")
        cat(paste(rep("-", 12), collapse = ""), "\n")
        print(effect_df, row.names = FALSE)
        cat("\n")
      }
    }
  }
  
  cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05\n")
  
  # Add interpretation guidelines only once at the end
  cat("\nEffect Size Interpretation:\n")
  cat("- Eta-squared: Proportion of variance explained (biased upward)\n")
  cat("- Epsilon-squared: Less biased than eta-squared\n")
  cat("- Omega-squared: Unbiased estimate (preferred for publication)\n")
  cat("- Small effect: eta-squared ~ 0.01, Medium effect: eta-squared ~ 0.06, Large effect: eta-squared ~ 0.14\n")
  
  # Add note about post-hoc tests
  if (x$is_grouped) {
    cat("\nPost-hoc tests: Use tukey_test() for pairwise comparisons on each variable within each group\n")
  } else {
    cat(sprintf("\nPost-hoc tests: Use tukey_test() for pairwise comparisons%s\n",
                ifelse(length(x$variables) > 1, " on each variable", "")))
  }
}

# Print method for repeated measures ANOVA
print_repeated_measures_anova <- function(x, digits = 3, ...) {
  
  # Determine test type
  test_type <- if (!is.null(x$weights)) {
    "Weighted Mixed ANOVA Results (Repeated Measures)"
  } else {
    "Mixed ANOVA Results (Repeated Measures)"
  }
  
  cat(sprintf("\n%s\n", test_type))
  cat(paste(rep("-", nchar(test_type)), collapse = ""), "\n")
  
  # Print test information
  cat("\n")
  cat(sprintf("Between-Subjects Factor: %s\n", x$group))
  # Dynamic display of time points
  if (length(x$variables) == 2) {
    cat(sprintf("Within-Subjects Factor: Time (%s vs %s)\n", x$variables[1], x$variables[2]))
  } else {
    cat(sprintf("Within-Subjects Factor: Time (%s)\n", paste(x$variables, collapse = " vs ")))
  }
  if (!is.null(x$weights)) {
    cat(sprintf("Weights variable: %s\n", x$weights))
  }
  if (!is.null(x$subject_id)) {
    cat(sprintf("Subject ID: %s\n", x$subject_id))
  }
  cat(sprintf("Confidence level: %.1f%%\n", x$conf.level * 100))
  cat("\n")
  
  # Add significance stars (handle both numeric and character p_values)
  p_values <- x$results$p_value
  if (is.character(p_values)) {
    # Convert character p_values to numeric, handling "n/a"
    p_numeric <- suppressWarnings(as.numeric(p_values))
    p_numeric[is.na(p_numeric)] <- 1  # Set "n/a" to 1 (non-significant)
  } else {
    p_numeric <- p_values
  }

  # Create significance stars properly
  sig_stars <- rep("", length(p_numeric))
  sig_stars[p_numeric < 0.001] <- "***"
  sig_stars[p_numeric >= 0.001 & p_numeric < 0.01] <- "**"
  sig_stars[p_numeric >= 0.01 & p_numeric < 0.05] <- "*"
  sig_stars[p_numeric >= 0.05] <- ""
  
  x$results$sig <- sig_stars
  
  # Print multivariate tests first (if available)
  if (!is.null(x$multivariate_tests)) {
    cat("\nMultivariate Tests:\n")
    
    # Format multivariate tests for display
    mv_formatted <- x$multivariate_tests
    mv_formatted$Value <- sprintf("%.3f", mv_formatted$Value)
    mv_formatted$F_stat <- sprintf("%.3f", mv_formatted$F_stat)
    mv_formatted$df1 <- as.character(mv_formatted$df1)
    mv_formatted$df2 <- sprintf("%.0f", mv_formatted$df2)
    mv_formatted$p_value <- ifelse(mv_formatted$p_value < 0.001, "<.001", sprintf("%.3f", mv_formatted$p_value))
    mv_formatted$eta_squared <- sprintf("%.3f", mv_formatted$eta_squared)
    
    # Add significance stars
    p_numeric_mv <- suppressWarnings(as.numeric(x$multivariate_tests$p_value))
    mv_formatted$sig <- cut(p_numeric_mv, 
                           breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                           labels = c("***", "**", "*", ""),
                           right = FALSE)
    
    # Calculate border width
    captured_output <- capture.output(print(mv_formatted, row.names = FALSE))
    actual_width <- max(nchar(captured_output), na.rm = TRUE)
    border_width <- paste(rep("-", actual_width), collapse = "")
    
    cat(border_width, "\n")
    print(mv_formatted, row.names = FALSE)
    cat(border_width, "\n")
  }

  # Print Within-Subjects Effects table (if available)
  if (!is.null(x$within_subjects_effects)) {
    cat("\nWithin-Subjects Effects:\n")
    
    # Format within-subjects effects for display
    ws_formatted <- x$within_subjects_effects
    
    # Handle NA values with compact formatting
    ws_formatted$SS_III <- ifelse(is.na(ws_formatted$SS_III), "", sprintf("%.1f", ws_formatted$SS_III))
    ws_formatted$df <- ifelse(is.na(ws_formatted$df), "", 
                             ifelse(ws_formatted$df == 1, "1", sprintf("%.0f", ws_formatted$df)))
    
    # Handle NA values in MS (compact)
    ws_formatted$MS <- ifelse(is.na(ws_formatted$MS), "", sprintf("%.1f", ws_formatted$MS))
    
    # Handle F values (compact)
    ws_formatted$F <- ifelse(is.na(ws_formatted$F), "", sprintf("%.2f", ws_formatted$F))
    
    # Handle p-values (compact) - Note: Sig is already formatted as string
    ws_formatted$Sig <- ifelse(is.na(ws_formatted$Sig), "", ws_formatted$Sig)
    
    # Handle partial eta squared (compact)
    ws_formatted$Eta2_p <- ifelse(is.na(ws_formatted$Eta2_p), "", 
                                              sprintf("%.2f", ws_formatted$Eta2_p))
    
    # Handle noncentrality parameter (compact)
    ws_formatted$Noncent <- ifelse(is.na(ws_formatted$Noncent), "", 
                                            sprintf("%.1f", ws_formatted$Noncent))
    
    # Handle observed power (compact)
    ws_formatted$Power <- ifelse(is.na(ws_formatted$Power), "", 
                                         sprintf("%.2f", ws_formatted$Power))
    
    # Create a compact SPSS-style display with shortened column names
    ws_display <- data.frame(
      Source = ws_formatted$Source,
      Correction = ws_formatted$Correction,
      SS_III = ws_formatted$SS_III,
      df = ws_formatted$df,
      MS = ws_formatted$MS,
      F = ws_formatted$F,
      Sig = ws_formatted$Sig,
      eta_sq = ws_formatted$Eta2_p,
      ncp = ws_formatted$Noncent,
      power = ws_formatted$Power,
      stringsAsFactors = FALSE
    )
    
    # Optimize column widths for compact display
    ws_display$Source <- ifelse(nchar(ws_display$Source) > 12, 
                               substr(ws_display$Source, 1, 12), 
                               ws_display$Source)
    ws_display$Correction <- ifelse(nchar(ws_display$Correction) > 10, 
                                   substr(ws_display$Correction, 1, 10), 
                                   ws_display$Correction)
    
    # Custom compact formatting to avoid line wrapping
    cat("--------------------------------------------------------------------------------\n")
    
    # Print header
    cat(sprintf("%-12s %-10s %8s %3s %8s %6s %6s %6s %6s %6s\n",
                "Source", "Correction", "SS_III", "df", "MS", "F", "Sig", "eta_sq", "ncp", "power"))
    cat("--------------------------------------------------------------------------------\n")
    
    # Print each row with controlled width
    for (i in 1:nrow(ws_display)) {
      # Format p-values and add significance stars
      sig_val <- ws_display$Sig[i]
      if (!is.na(sig_val) && sig_val != "") {
        p_num <- as.numeric(sig_val)
        if (!is.na(p_num)) {
          # Add significance stars
          if (p_num < 0.001) {
            sig_val <- "***"
          } else if (p_num < 0.01) {
            sig_val <- "**"
          } else if (p_num < 0.05) {
            sig_val <- "*"
          } else {
            sig_val <- ""
          }
        }
      } else {
        sig_val <- ""
      }
      
      cat(sprintf("%-12s %-10s %8.1f %3s %8.1f %6s %6s %6s %6s %6s\n",
                  substr(ws_display$Source[i], 1, 12),
                  substr(ws_display$Correction[i], 1, 10),
                  as.numeric(ws_display$SS_III[i]),
                  ws_display$df[i],
                  as.numeric(ws_display$MS[i]),
                  ws_display$F[i],
                  sig_val,
                  ws_display$eta_sq[i],
                  ws_display$ncp[i],
                  ws_display$power[i]))
    }
    
    cat("--------------------------------------------------------------------------------\n")
    cat("a. Computed using alpha = .05\n")
  }

  # Combine time and interaction effects into a single "Within-Subjects Contrasts" table
  cat("\nWithin-Subjects Contrasts:\n")
  
  # Extract time and interaction effects
  within_time_effect <- NULL
  interaction_effect <- NULL
  
  for (i in seq_len(nrow(x$results))) {
    if (grepl("Within-Subjects.*Time", x$results$Effect[i])) {
      within_time_effect <- x$results[i, ]
    } else if (grepl("Interaction", x$results$Effect[i])) {
      interaction_effect <- x$results[i, ]
    }
  }
  
  # Create combined results table
  results_df <- data.frame(
    Source = c("Time", "Time * Group"),
    F_stat = c(
      if(is.null(within_time_effect) || is.na(within_time_effect$F_statistic)) "n/a" else round(as.numeric(within_time_effect$F_statistic), digits),
      if(is.null(interaction_effect) || is.na(interaction_effect$F_statistic)) "n/a" else round(as.numeric(interaction_effect$F_statistic), digits)
    ),
    df1 = c(
      if(is.null(within_time_effect) || is.na(within_time_effect$df1)) "n/a" else as.character(within_time_effect$df1),
      if(is.null(interaction_effect) || is.na(interaction_effect$df1)) "n/a" else as.character(interaction_effect$df1)
    ),
    df2 = c(
      if(is.null(within_time_effect) || is.na(within_time_effect$df2)) "n/a" else as.character(round(as.numeric(within_time_effect$df2), 0)),
      if(is.null(interaction_effect) || is.na(interaction_effect$df2)) "n/a" else as.character(round(as.numeric(interaction_effect$df2), 0))
    ),
    p_value = c(
      if(is.null(within_time_effect) || is.na(within_time_effect$p_value)) "n/a" else {
        p_val <- as.numeric(within_time_effect$p_value)
        if(p_val < 0.001) "<.001" else round(p_val, digits)
      },
      if(is.null(interaction_effect) || is.na(interaction_effect$p_value)) "n/a" else {
        p_val <- as.numeric(interaction_effect$p_value)
        if(p_val < 0.001) "<.001" else round(p_val, digits)
      }
    ),
    sig = c(
      if(is.null(within_time_effect) || is.na(within_time_effect$p_value)) "" else as.character(within_time_effect$sig),
      if(is.null(interaction_effect) || is.na(interaction_effect$p_value)) "" else as.character(interaction_effect$sig)
    ),
    eta_squared = c(
      if(is.null(within_time_effect) || is.na(within_time_effect$eta_squared)) "n/a" else round(as.numeric(within_time_effect$eta_squared), digits),
      if(is.null(interaction_effect) || is.na(interaction_effect$eta_squared)) "n/a" else round(as.numeric(interaction_effect$eta_squared), digits)
    ),
    stringsAsFactors = FALSE
  )
  
  # Calculate border width
  col_widths <- sapply(names(results_df), function(col) {
    max(nchar(as.character(results_df[[col]])), nchar(col), na.rm = TRUE)
  })
  total_width <- sum(col_widths) + length(col_widths) - 1
  border_width <- paste(rep("-", max(total_width, 60)), collapse = "")
  
  cat(border_width, "\n")
  print(results_df, row.names = FALSE)
  cat(border_width, "\n")
  
  # Still print Between-Subjects effect separately
  between_effect_row <- NULL
  for (i in seq_len(nrow(x$results))) {
    if (grepl("Between-Subjects", x$results$Effect[i])) {
      between_effect_row <- x$results[i, ]
      break
    }
  }
  if(!is.null(between_effect_row)) {
    effect_name <- between_effect_row$Effect
    
    cat(sprintf("\n%s:\n", effect_name))
    
    # Create results table for between effect
    between_df <- data.frame(
      Effect = between_effect_row$Effect,
      F_stat = if(is.na(between_effect_row$F_statistic)) "n/a" else round(as.numeric(between_effect_row$F_statistic), digits),
      df1 = if(is.na(between_effect_row$df1)) "n/a" else as.character(between_effect_row$df1),
      df2 = if(is.na(between_effect_row$df2)) "n/a" else as.character(round(as.numeric(between_effect_row$df2), 0)),
      p_value = if(is.na(between_effect_row$p_value)) "n/a" else {
        p_val <- as.numeric(between_effect_row$p_value)
        if(p_val < 0.001) "<.001" else round(p_val, digits)
      },
      sig = if(is.na(between_effect_row$p_value)) "" else as.character(between_effect_row$sig),
      eta_squared = if(is.na(between_effect_row$eta_squared)) "n/a" else round(as.numeric(between_effect_row$eta_squared), digits),
      stringsAsFactors = FALSE
    )
    
    # Calculate border width
    col_widths <- sapply(names(between_df), function(col) {
      max(nchar(as.character(between_df[[col]])), nchar(col), na.rm = TRUE)
    })
    total_width <- sum(col_widths) + length(col_widths) - 1
    border_width <- paste(rep("-", max(total_width, 60)), collapse = "")
    
    cat(border_width, "\n")
    print(between_df, row.names = FALSE)
    cat(border_width, "\n")
  }
  
  # Print descriptive statistics in SPSS format (grouped by variable/time)
  if (!is.null(x$descriptives)) {
    cat("\nDescriptive Statistics:\n")
    
    # Reorganize data in SPSS format: variables as main groups, groups as subrows
    desc_data <- as.data.frame(x$descriptives)
    variables <- unique(desc_data$Time)
    groups <- unique(desc_data$group)
    
         # Create SPSS-style formatted table
     spss_table <- data.frame(
       Variable = character(0),
       Group = character(0),
       Mean = character(0),
       SD = character(0),
       N = character(0),
       stringsAsFactors = FALSE
     )
    
         for (var in variables) {
       first_group_for_var <- TRUE
       
       # Add variable rows for each group
       for (grp in groups) {
         var_group_data <- desc_data[desc_data$Time == var & desc_data$group == grp, ]
         if (nrow(var_group_data) > 0) {
           spss_table <- rbind(spss_table, data.frame(
             Variable = ifelse(first_group_for_var, var, ""),  # Only show variable name for first group
             Group = as.character(grp),
             Mean = sprintf("%.4f", var_group_data$mean),
             SD = sprintf("%.5f", var_group_data$sd),
             N = sprintf("%.0f", var_group_data$n),
             stringsAsFactors = FALSE
           ))
           first_group_for_var <- FALSE
         }
       }
       
       # Add total for this variable
       var_total <- desc_data[desc_data$Time == var, ]
       if (nrow(var_total) > 0) {
         total_mean <- sum(var_total$mean * var_total$n) / sum(var_total$n)
         total_sd <- sqrt(sum((var_total$n - 1) * var_total$sd^2 + var_total$n * (var_total$mean - total_mean)^2) / (sum(var_total$n) - 1))
         total_n <- sum(var_total$n)
         
         spss_table <- rbind(spss_table, data.frame(
           Variable = "",
           Group = "Total",
           Mean = sprintf("%.4f", total_mean),
           SD = sprintf("%.5f", total_sd),
           N = sprintf("%.0f", total_n),
           stringsAsFactors = FALSE
         ))
       }
     }
    
    # Calculate border width
    captured_output <- capture.output(print(spss_table, row.names = FALSE))
    actual_width <- max(nchar(captured_output), na.rm = TRUE)
    border_width <- paste(rep("-", actual_width), collapse = "")
    
    cat(border_width, "\n")
    print(spss_table, row.names = FALSE)
    cat(border_width, "\n")
  }
  
  cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05\n")
  
  cat("\nInterpretation:\n")
  cat("- Between-Subjects: Tests differences between groups\n")
  cat("- Within-Subjects: Tests differences between time points\n") 
  cat("- Interaction: Tests if group differences change over time\n")
  
  cat("\nEffect Size Interpretation:\n")
  cat("- Eta-squared: Proportion of variance explained\n")
  cat("- Small effect: eta-squared ~ 0.01, Medium effect: eta-squared ~ 0.06, Large effect: eta-squared ~ 0.14\n")
  
  invisible(x)
}

 