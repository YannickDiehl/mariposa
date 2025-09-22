
#' Levene's Test for Homogeneity of Variance
#'
#' @description
#' \code{levene_test()} performs Levene's test for homogeneity of variance across groups.
#' This function can be used as a standalone test or piped after other statistical tests
#' to check the assumption of equal variances. The function supports both weighted and 
#' unweighted analyses and works with various statistical test result objects.
#'
#' @param x Either a data frame or a statistical test result object (e.g., from \code{t_test()})
#' @param ... Additional arguments passed to methods
#' @param data A data frame (when x is not a test result object)
#' @param group <\code{\link[dplyr]{dplyr_data_masking}}> Grouping variable for the test
#' @param weights <\code{\link[dplyr]{dplyr_data_masking}}> Optional sampling weights
#' @param center Character string specifying the center to use. Options are \code{"mean"} 
#'   (default, classical Levene test, SPSS-compatible) or \code{"median"} (more robust)
#'
#' @return An object of class \code{"levene_test_results"} containing:
#' \describe{
#'   \item{results}{A data frame with Levene test statistics and p-values for each variable}
#'   \item{variables}{Character vector of analyzed variable names}
#'   \item{group}{Name of the grouping variable}
#'   \item{weights}{Name of the weights variable (if used)}
#'   \item{center}{Type of center used (median or mean)}
#'   \item{original_test}{Original test result object (if applicable)}
#' }
#'
#' @details
#' ## Statistical Method
#' 
#' Levene's test examines the null hypothesis that the population variances are equal.
#' The test statistic is calculated as:
#' 
#' \deqn{W = \frac{(N-k) \sum_{i=1}^{k} n_i (\bar{Z}_{i.} - \bar{Z}_{..})^2}{(k-1) \sum_{i=1}^{k} \sum_{j=1}^{n_i} (Z_{ij} - \bar{Z}_{i.})^2}}
#' 
#' where \eqn{Z_{ij} = |Y_{ij} - \tilde{Y}_{i.}|} and \eqn{\tilde{Y}_{i.}} is the median 
#' (or mean) of the i-th group.
#' 
#' ## Interpretation
#' - **p > 0.05**: Variances are homogeneous (assumption met)
#' - **p ≤ 0.05**: Variances are heterogeneous (assumption violated)
#' 
#' ## Usage with Other Tests
#' The function can be used standalone or piped after other statistical tests:
#' 
#' ```r
#' # Standalone usage
#' levene_test(data, variable, group = grouping_var)
#' 
#' # Piped after t-test
#' t_test(data, variable, group = grouping_var) %>% levene_test()
#' 

#' ```
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#' 
#' # Standalone Levene test (test homogeneity of variances)
#' survey_data %>% levene_test(life_satisfaction, group = region)
#' 
#' # Multiple variables
#' survey_data %>% levene_test(life_satisfaction, trust_government, group = region)
#' 
#' # Weighted analysis
#' survey_data %>% levene_test(income, group = education, weights = sampling_weight)
#' 
#' # Piped after ANOVA (common workflow)
#' result <- survey_data %>%
#'   oneway_anova_test(life_satisfaction, group = education)
#' result %>% levene_test()
#' 
#' # Piped after t-test
#' survey_data %>%
#'   t_test(age, group = gender) %>%
#'   levene_test()
#' 
#' # Using mean instead of median as center
#' survey_data %>% levene_test(income, group = region, center = "mean")
#'
#' @export
levene_test <- function(x, ...) {
  UseMethod("levene_test")
}

#' @rdname levene_test
#' @export
levene_test.data.frame <- function(x, ..., group, weights = NULL, center = c("mean", "median")) {
  
  # Input validation
  if (!is.data.frame(x)) {
    stop("x must be a data frame")
  }
  
  center <- match.arg(center)
  
  # Get variable names using tidyselect
  dots <- enquos(...)
  group_quo <- enquo(group)
  weights_quo <- enquo(weights)
  
  # Evaluate selections
  vars <- eval_select(expr(c(!!!dots)), data = x)
  var_names <- names(vars)
  
  if (length(var_names) == 0) {
    stop("At least one variable must be specified")
  }
  
  # Group is required
  if (quo_is_null(group_quo)) {
    stop("group argument is required for Levene's test")
  }
  
  g_var <- eval_select(expr(!!group_quo), data = x)
  g_name <- names(g_var)[1]
  
  if (!quo_is_null(weights_quo)) {
    w_var <- eval_select(expr(!!weights_quo), data = x)
    w_name <- names(w_var)[1]
  } else {
    w_name <- NULL
  }
  
  # Perform Levene test for each variable
  results_list <- list()
  
  for (var_name in var_names) {
    tryCatch({
      result <- perform_single_levene_test(x, var_name, g_name, w_name, center)
      
      results_list[[var_name]] <- tibble(
        Variable = var_name,
        F_statistic = result$F_stat,
        df1 = result$df1,
        df2 = result$df2,
        p_value = result$p_value,
        conclusion = ifelse(result$p_value > 0.05, "Variances equal", "Variances unequal")
      )
      
    }, error = function(e) {
      warning(sprintf("Levene test failed for variable '%s': %s", var_name, e$message))
      results_list[[var_name]] <- tibble(
        Variable = var_name,
        F_statistic = NA_real_,
        df1 = NA_integer_,
        df2 = NA_integer_,
        p_value = NA_real_,
        conclusion = NA_character_
      )
    })
  }
  
  results_df <- bind_rows(results_list)
  
  # Create result object
  result <- list(
    results = results_df,
    variables = var_names,
    group = g_name,
    weights = w_name,
    center = center,
    original_test = NULL
  )
  
  class(result) <- "levene_test_results"
  return(result)
}

#' @rdname levene_test
#' @export
levene_test.oneway_anova_test_results <- function(x, center = c("mean", "median"), ...) {
  
  center <- match.arg(center)
  
  # Extract information from ANOVA results
  if (is.null(x$group)) {
    stop("Levene test requires a grouping variable")
  }
  
  if (is.null(x$data)) {
    stop("Original data not available in oneway_anova_test results. Please use: levene_test(data, variables, group = group)")
  }
  
  # Check if this is a grouped analysis
  if (x$is_grouped && !is.null(x$groups)) {
    # Handle grouped data - perform Levene test for each group
    group_vars <- x$groups
    
    # Split data by groups and perform Levene test for each
    results_list <- list()
    
    data_list <- group_split(x$data)
    group_keys <- group_keys(x$data)
    
    for (i in seq_along(data_list)) {
      group_data <- data_list[[i]]
      group_info <- group_keys[i, , drop = FALSE]
      
      # Perform Levene test for each variable in this group
      for (var_name in x$variables) {
        tryCatch({
          result <- perform_single_levene_test(group_data, var_name, x$group, x$weights, center)
          
          result_row <- tibble(
            group_info,
            Variable = var_name,
            F_statistic = result$F_stat,
            df1 = result$df1,
            df2 = result$df2,
            p_value = result$p_value,
            conclusion = ifelse(result$p_value > 0.05, "Variances equal", "Variances unequal")
          )
          
          results_list <- append(results_list, list(result_row))
          
        }, error = function(e) {
          warning(sprintf("Levene test failed for variable '%s' in group %s: %s", 
                         var_name, paste(group_info, collapse = ", "), e$message))
        })
      }
    }
    
    if (length(results_list) > 0) {
      results_df <- bind_rows(results_list)
    } else {
      results_df <- tibble()
    }
    
  } else {
    # Ungrouped analysis
    results_list <- list()
    
    for (var_name in x$variables) {
      tryCatch({
        result <- perform_single_levene_test(x$data, var_name, x$group, x$weights, center)
        
        results_list[[var_name]] <- tibble(
          Variable = var_name,
          F_statistic = result$F_stat,
          df1 = result$df1,
          df2 = result$df2,
          p_value = result$p_value,
          conclusion = ifelse(result$p_value > 0.05, "Variances equal", "Variances unequal")
        )
        
      }, error = function(e) {
        warning(sprintf("Levene test failed for variable '%s': %s", var_name, e$message))
        results_list[[var_name]] <- tibble(
          Variable = var_name,
          F_statistic = NA_real_,
          df1 = NA_integer_,
          df2 = NA_integer_,
          p_value = NA_real_,
          conclusion = NA_character_
        )
      })
    }
    
    results_df <- bind_rows(results_list)
  }
  
  # Create result object
  result <- list(
    results = results_df,
    variables = x$variables,
    group = x$group,
    weights = x$weights,
    center = center,
    is_grouped = x$is_grouped,
    groups = x$groups,
    original_test = x
  )
  
  class(result) <- "levene_test_results"
  return(result)
}

#' @rdname levene_test
#' @export
levene_test.t_test_results <- function(x, center = c("mean", "median"), ...) {
  
  center <- match.arg(center)
  
  # Extract information from t-test results
  if (is.null(x$group)) {
    stop("Levene test requires a grouping variable (two-sample t-test)")
  }
  
  if (is.null(x$data)) {
    stop("Original data not available in t_test results. Please use: levene_test(data, variables, group = group)")
  }
  
  # Check if this is a grouped analysis
  if (x$is_grouped && !is.null(x$groups)) {
    # Handle grouped data - perform Levene test for each group
    group_vars <- x$groups
    
    # Split data by groups and perform Levene test for each
    results_list <- list()
    
    # Get unique group combinations
    unique_groups <- unique(x$data[group_vars])
    
    for (i in seq_len(nrow(unique_groups))) {
      group_filter <- unique_groups[i, , drop = FALSE]
      
      # Filter data for this group combination
      group_data <- x$data
      for (g in names(group_filter)) {
        group_data <- group_data[group_data[[g]] == group_filter[[g]], ]
      }
      
      # Perform Levene test for this group
      if (!is.null(x$weight_var) || !is.null(x$weights)) {
        group_result <- levene_test.data.frame(group_data, !!!syms(x$variables), 
                                              group = !!sym(x$group), 
                                              weights = !!sym(x$weights),
                                              center = center)
      } else {
        group_result <- levene_test.data.frame(group_data, !!!syms(x$variables), 
                                              group = !!sym(x$group), 
                                              center = center)
      }
      
      # Add group information to results
      for (g in names(group_filter)) {
        group_result$results[[g]] <- rep(group_filter[[g]], nrow(group_result$results))
      }
      
      results_list[[i]] <- group_result$results
    }
    
    # Combine results
    combined_results <- bind_rows(results_list)
    
    # Create grouped result object
    result <- list(
      results = combined_results,
      variables = x$variables,
      group = x$group,
      weights = x$weights,
      center = center,
      original_test = x,
      is_grouped = TRUE,
      groups = group_vars
    )
    
    class(result) <- "levene_test_results"
    return(result)
    
  } else {
    # Handle ungrouped data (original behavior)
    if (!is.null(x$weight_var) || !is.null(x$weights)) {
      result <- levene_test.data.frame(x$data, !!!syms(x$variables), 
                                      group = !!sym(x$group), 
                                      weights = !!sym(x$weights),
                                      center = center)
    } else {
      result <- levene_test.data.frame(x$data, !!!syms(x$variables), 
                                      group = !!sym(x$group), 
                                      center = center)
    }
    
    # Add reference to original test
    result$original_test <- x
    return(result)
  }
}

#' @rdname levene_test
#' @export
levene_test.mann_whitney_test_results <- function(x, ...) {
  stop("Levene's test is not appropriate for Mann-Whitney U test results.\n",
       "Mann-Whitney U test is non-parametric and does not assume equal variances.\n",
       "Use Levene's test only with parametric tests like t-test.")
}

#' @rdname levene_test
#' @export
levene_test.grouped_df <- function(data, variable, group = NULL, weights = NULL, center = "mean", ...) {
  
  # Get grouping variables
  group_vars <- dplyr::group_vars(data)
  
  if (length(group_vars) == 0) {
    stop("Data must be grouped using group_by() for grouped analysis")
  }
  
  # Get variable names
  var_name <- deparse(substitute(variable))
  group_name <- if (!is.null(substitute(group))) deparse(substitute(group)) else NULL
  weight_name <- if (!is.null(substitute(weights))) deparse(substitute(weights)) else NULL
  
  if (is.null(group_name)) {
    stop("Group variable must be specified for Levene test")
  }
  
  # Get unique groups
  groups <- data %>% 
    dplyr::group_keys()
  
  # Initialize results list
  all_results <- list()
  
  # Process each group separately using SPSS method (filter then test)
  for (i in seq_len(nrow(groups))) {
    group_filter <- groups[i, , drop = FALSE]
    
    # Create filter condition
    filter_conditions <- purrr::map2(names(group_filter), group_filter, ~ quo(!!sym(.x) == !!.y))
    
    # Filter data for this group (SPSS SELECT IF method)
    filtered_data <- data %>%
      dplyr::ungroup() %>%
      dplyr::filter(!!!filter_conditions)
    
    # Perform Levene test on filtered data (this matches SPSS approach)
    if (!is.null(weight_name)) {
      result <- perform_single_levene_test(filtered_data, var_name, group_name, weight_name, center)
    } else {
      result <- perform_single_levene_test(filtered_data, var_name, group_name, NULL, center)
    }
    
         # Store result with group information
     group_result <- data.frame(
       Group = paste(names(group_filter), as.character(group_filter[[1]]), sep = " = ", collapse = ", "),
       Variable = var_name,
       F_statistic = round(result$F_stat, 3),
       df1 = result$df1,
       df2 = result$df2,
       p_value = round(result$p_value, 3),
       sig = ifelse(result$p_value < 0.001, "***",
                    ifelse(result$p_value < 0.01, "**",
                           ifelse(result$p_value < 0.05, "*", ""))),
       conclusion = ifelse(result$p_value > 0.05, "Variances equal", "Variances unequal"),
       stringsAsFactors = FALSE
     )
    
    all_results[[i]] <- group_result
  }
  
  # Combine all results
  final_results <- do.call(rbind, all_results)
  
  # Create result object
  result_obj <- list(
    results = final_results,
    variables = var_name,
    group = group_name,
    weights = weight_name,
    center = center,
    is_grouped = TRUE,
    groups = group_vars,
    method = if (!is.null(weight_name)) "Weighted Levene's Test" else "Levene's Test"
  )
  
  class(result_obj) <- "levene_test_results"
  return(result_obj)
}

# SPSS-compatible helper function for grouped Levene tests
perform_grouped_levene_test <- function(data, var_name, group_name, weight_name = NULL, center = "mean") {
  
  # SPSS-compatible grouped Levene test
  # In SPSS, when doing grouped analysis, the Levene test is performed on the complete
  # dataset within each group, using all available data for that group
  
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
  
  # Get unique groups
  if (is.factor(g)) {
    all_levels <- levels(g)
    g_levels <- all_levels[all_levels %in% unique(g)]
  } else {
    g_levels <- unique(g)
  }
  
  if (length(g_levels) < 2) {
    stop(sprintf("Levene test requires at least 2 groups, found %d", length(g_levels)))
  }
  
  # Calculate deviations from group centers
  z_values <- numeric(length(x))
  
  # For weighted tests, use weighted group centers
  if (!is.null(weight_name)) {
    for (level in g_levels) {
      group_indices <- g == level
      group_data <- x[group_indices]
      group_weights <- w[group_indices]
      
      if (center == "median") {
        # For weighted median, use a simple approximation
        group_center <- median(group_data, na.rm = TRUE)
      } else {
        # Weighted mean
        group_center <- sum(group_data * group_weights) / sum(group_weights)
      }
      
      z_values[group_indices] <- abs(group_data - group_center)
    }
    
    # SPSS-compatible weighted Levene test for grouped data
    # Calculate effective sample sizes per group (sum of weights)
    group_eff_n <- sapply(g_levels, function(level) {
      group_indices <- g == level
      sum(w[group_indices])
    })
    
    # Calculate weighted group means of absolute deviations
    z_group_means <- sapply(g_levels, function(level) {
      group_indices <- g == level
      sum(z_values[group_indices] * w[group_indices]) / sum(w[group_indices])
    })
    names(z_group_means) <- as.character(g_levels)
    
    # Calculate overall weighted mean of absolute deviations
    z_overall_mean <- sum(z_values * w) / sum(w)
    
    # Between-groups sum of squares (SPSS method)
    ss_between <- sum(group_eff_n * (z_group_means - z_overall_mean)^2)
    
    # Within-groups sum of squares (SPSS method)
    ss_within <- 0
    for (i in seq_along(z_values)) {
      group_level <- as.character(g[i])
      group_mean <- z_group_means[group_level]
      ss_within <- ss_within + w[i] * (z_values[i] - group_mean)^2
    }
    
    # SPSS degrees of freedom for grouped analysis
    # SPSS uses the total weighted sample size minus number of groups
    df1 <- length(g_levels) - 1
    df2 <- round(sum(w)) - length(g_levels)  # SPSS method: N - k
    
    # Calculate F-statistic (SPSS method)
    F_stat <- (ss_between / df1) / (ss_within / df2)
    p_value <- pf(F_stat, df1, df2, lower.tail = FALSE)
    
  } else {
    # Unweighted case
    for (level in g_levels) {
      group_indices <- g == level
      group_data <- x[group_indices]
      
      if (center == "median") {
        group_center <- median(group_data, na.rm = TRUE)
      } else {
        group_center <- mean(group_data, na.rm = TRUE)
      }
      
      z_values[group_indices] <- abs(group_data - group_center)
    }
    
    # Unweighted Levene test for grouped data
    # SPSS uses N - k for degrees of freedom
    test_data <- data.frame(z = z_values, group = g)
    anova_result <- aov(z ~ group, data = test_data)
    anova_summary <- summary(anova_result)
    
    F_stat <- anova_summary[[1]][["F value"]][1]
    df1 <- anova_summary[[1]][["Df"]][1]
    df2 <- length(z_values) - length(g_levels)  # SPSS method: N - k
    p_value <- anova_summary[[1]][["Pr(>F)"]][1]
  }
  
  return(list(
    F_stat = F_stat,
    df1 = df1,
    df2 = df2,
    p_value = p_value
  ))
}

# Helper function to perform Levene test for a single variable (non-grouped)
perform_single_levene_test <- function(data, var_name, group_name, weight_name = NULL, center = "mean") {
  
  # Get the variable values
  x <- data[[var_name]]
  g <- data[[group_name]]
  
  # Check for constant variable (all values identical)
  if (length(unique(x[!is.na(x)])) <= 1) {
    warning(sprintf("Variable '%s' has constant values. Levene test requires variability.", var_name))
    return(list(
      F_stat = NaN,
      df1 = NA_integer_,
      df2 = NA_integer_, 
      p_value = NaN
    ))
  }
  
  # Remove NA values
  valid_indices <- !is.na(x) & !is.na(g)
  if (!is.null(weight_name)) {
    w <- data[[weight_name]]
    valid_indices <- valid_indices & !is.na(w)
    w <- w[valid_indices]
  }
  x <- x[valid_indices]
  g <- g[valid_indices]
  
  # Get unique groups
  if (is.factor(g)) {
    all_levels <- levels(g)
    g_levels <- all_levels[all_levels %in% unique(g)]
  } else {
    g_levels <- unique(g)
  }
  
  if (length(g_levels) < 2) {
    stop(sprintf("Levene test requires at least 2 groups, found %d", length(g_levels)))
  }
  
  # Calculate deviations from group centers
  z_values <- numeric(length(x))
  
  # Calculate group centers and deviations
  for (level in g_levels) {
    group_indices <- g == level
    group_data <- x[group_indices]
    
    if (!is.null(weight_name)) {
      group_weights <- w[group_indices]
      if (center == "median") {
        # For weighted median, use a simple approximation
        group_center <- median(group_data, na.rm = TRUE)
      } else {
        # Weighted mean
        group_center <- sum(group_data * group_weights) / sum(group_weights)
      }
    } else {
      if (center == "median") {
        group_center <- median(group_data, na.rm = TRUE)
      } else {
        group_center <- mean(group_data, na.rm = TRUE)
      }
    }
    
    z_values[group_indices] <- abs(group_data - group_center)
  }
  
  if (!is.null(weight_name)) {
    # SPSS-compatible weighted Levene test
    # Calculate effective sample sizes per group (sum of weights)
    group_eff_n <- sapply(g_levels, function(level) {
      group_indices <- g == level
      sum(w[group_indices])
    })
    
    # Calculate weighted group means of absolute deviations
    z_group_means <- sapply(g_levels, function(level) {
      group_indices <- g == level
      sum(z_values[group_indices] * w[group_indices]) / sum(w[group_indices])
    })
    names(z_group_means) <- as.character(g_levels)
    
    # Calculate overall weighted mean of absolute deviations
    z_overall_mean <- sum(z_values * w) / sum(w)
    
    # Between-groups sum of squares (SPSS method)
    ss_between <- sum(group_eff_n * (z_group_means - z_overall_mean)^2)
    
    # Within-groups sum of squares (SPSS method)
    ss_within <- 0
    for (i in seq_along(z_values)) {
      group_level <- as.character(g[i])
      group_mean <- z_group_means[group_level]
      ss_within <- ss_within + w[i] * (z_values[i] - group_mean)^2
    }
    
    # SPSS degrees of freedom: 
    # df1 = k-1 (number of groups - 1)
    # df2 = sum of weights - k (total weighted sample size - number of groups)
    df1 <- length(g_levels) - 1
    df2 <- round(sum(w)) - length(g_levels)  # SPSS method: sum(weights) - k
    
    # Calculate F-statistic (SPSS method)
    # F = (SS_between / df1) / (SS_within / df2)
    F_stat <- (ss_between / df1) / (ss_within / df2)
    p_value <- pf(F_stat, df1, df2, lower.tail = FALSE)
    
  } else {
    # Unweighted Levene test using standard ANOVA
    test_data <- data.frame(z = z_values, group = as.factor(g))
    anova_result <- aov(z ~ group, data = test_data)
    anova_summary <- summary(anova_result)
    
    F_stat <- anova_summary[[1]][["F value"]][1]
    df1 <- anova_summary[[1]][["Df"]][1]
    df2 <- anova_summary[[1]][["Df"]][2]
    p_value <- anova_summary[[1]][["Pr(>F)"]][1]
  }
  
  return(list(
    F_stat = F_stat,
    df1 = df1,
    df2 = df2,
    p_value = p_value
  ))
}

#' Print method for Levene test results
#' @export
#' @method print levene_test_results
print.levene_test_results <- function(x, digits = 3, ...) {
  
  # Determine test type based on weights
  test_type <- if (!is.null(x$weight_var) || !is.null(x$weights)) {
    "Weighted Levene's Test for Homogeneity of Variance"
  } else {
    "Levene's Test for Homogeneity of Variance"
  }
  
  cat(sprintf("\n%s\n", test_type))
  cat(paste(rep("-", nchar(test_type)), collapse = ""), "\n")
  
  # Print test information (always show for both grouped and ungrouped)
  cat("\n")
  if (!is.null(x$group_var) || !is.null(x$group)) {
    group_name <- if (!is.null(x$group_var)) x$group_var else x$group
    cat(sprintf("Grouping variable: %s\n", group_name))
  }
  if (!is.null(x$weight_var) || !is.null(x$weights)) {
    weight_name <- if (!is.null(x$weight_var)) x$weight_var else x$weights
    cat(sprintf("Weights variable: %s\n", weight_name))
  }
  if (!is.null(x$center)) {
    cat(sprintf("Center: %s\n", x$center))
  }
  cat("\n")
  
  # Add significance stars
  x$results$sig <- cut(x$results$p_value, 
                      breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                      labels = c("***", "**", "*", ""),
                      right = FALSE)
  
  # Check if this is grouped data
  is_grouped_data <- (!is.null(x$grouped) && x$grouped) || (!is.null(x$is_grouped) && x$is_grouped)
    if (is_grouped_data) {
    # Handle grouped results - group by group identity, then show all variables for each group
    
    # Get unique groups
    if ("Group" %in% names(x$results)) {
      # Direct grouped analysis: Group column like "eastwest=East"
      unique_groups <- unique(x$results$Group)
      group_column <- "Group"
    } else if (!is.null(x$groups) && length(x$groups) > 0) {
      # t_test_results grouped analysis: separate column for each group variable
      group_combinations <- unique(x$results[x$groups])
      unique_groups <- apply(group_combinations, 1, function(row) {
        group_parts <- sapply(x$groups, function(group_var) {
          paste(group_var, "=", row[group_var])
        })
        paste(group_parts, collapse = ", ")
      })
      group_column <- x$groups
    } else {
      # Fallback - check for any non-standard columns that might be group indicators
      non_standard_cols <- setdiff(names(x$results), c("Variable", "F_statistic", "df1", "df2", "p_value", "sig", "conclusion"))
      if (length(non_standard_cols) > 0) {
        group_column <- non_standard_cols[1]
        unique_groups <- unique(x$results[[group_column]])
        unique_groups <- paste(group_column, "=", unique_groups)
      } else {
        unique_groups <- "Unknown Group"
        group_column <- NULL
      }
    }
    
    # Process each unique group - similar to t_test format
    for (group_idx in seq_along(unique_groups)) {
      group_label <- unique_groups[group_idx]
      
      # Filter results for current group
      if ("Group" %in% names(x$results)) {
        # Direct group column - find rows with this group label (with tolerance for formatting)
        original_group_name <- gsub("eastwest = ", "eastwest=", group_label)
        group_results <- x$results[x$results$Group == original_group_name, ]
      } else if (!is.null(x$groups) && length(x$groups) > 0) {
        # For multi-column grouping, match all group columns
        group_results <- x$results
        group_combination <- group_combinations[group_idx, , drop = FALSE]
        for (g in x$groups) {
          group_results <- group_results[group_results[[g]] == group_combination[[g]], ]
        }
      } else if (!is.null(group_column)) {
        group_value <- unique(x$results[[group_column]])[group_idx]
        group_results <- x$results[x$results[[group_column]] == group_value, ]
      } else {
        group_results <- x$results
      }
      
      if (nrow(group_results) == 0) next
      
      # Show group header only once - similar to t_test format
      # Format group label according to template standard (ensure single spaces around =)
      formatted_group_label <- gsub(" = ", "=", group_label)  # Remove existing spaces
      formatted_group_label <- gsub("=", " = ", formatted_group_label)  # Add single spaces
      cat(sprintf("\nGroup: %s\n", formatted_group_label))
      
      # Process each variable in this group as separate blocks
      for (i in seq_len(nrow(group_results))) {
        group_row <- group_results[i, ]
        var_name <- group_row$Variable
        
        cat(sprintf("\n--- %s ---\n", var_name))
        cat("\n")  # Add blank line after variable name
        
        # Create results table for this variable
        results_df <- data.frame(
          Variable = group_row$Variable,
          F_statistic = round(group_row$F_statistic, digits),
          df1 = group_row$df1,
          df2 = group_row$df2,
          p_value = round(group_row$p_value, digits),
          sig = group_row$sig,
          Conclusion = group_row$conclusion,
          stringsAsFactors = FALSE
        )
        
        # Calculate border width
        col_widths <- sapply(names(results_df), function(col) {
          max(nchar(as.character(results_df[[col]])), nchar(col), na.rm = TRUE)
        })
        total_width <- sum(col_widths) + length(col_widths) - 1
        border_width <- paste(rep("-", total_width), collapse = "")
        
        cat(sprintf("%s:\n", ifelse(!is.null(x$weight_var) || !is.null(x$weights), "Weighted Levene's Test Results", "Levene's Test Results")))
        cat(border_width, "\n")
        print(results_df, row.names = FALSE)
        cat(border_width, "\n")
      }
    }
    
  } else {
    # Handle ungrouped results (original behavior)
    valid_results <- x$results[!is.na(x$results$Variable), ]
    
    # Process each variable separately like in t-test
    for (i in seq_len(nrow(valid_results))) {
      var_name <- valid_results$Variable[i]
      
      # Add variable box like in t-test
      cat(sprintf("\n--- %s ---\n", var_name))
      cat("\n")  # Add blank line after variable name
      
      # Create results table for this variable
      results_df <- data.frame(
        Variable = valid_results$Variable[i],
        F_statistic = round(valid_results$F_statistic[i], digits),
        df1 = valid_results$df1[i],
        df2 = valid_results$df2[i],
        p_value = round(valid_results$p_value[i], digits),
        sig = valid_results$sig[i],
        Conclusion = valid_results$conclusion[i],
        stringsAsFactors = FALSE
      )
      
      # Calculate border width
      col_widths <- sapply(names(results_df), function(col) {
        max(nchar(as.character(results_df[[col]])), nchar(col), na.rm = TRUE)
      })
      total_width <- sum(col_widths) + length(col_widths) - 1
      border_width <- paste(rep("-", total_width), collapse = "")
      
      cat(sprintf("%s:\n", ifelse(!is.null(x$weight_var) || !is.null(x$weights), "Weighted Levene's Test Results", "Levene's Test Results")))
      cat(border_width, "\n")
      print(results_df, row.names = FALSE)
      cat(border_width, "\n")
      
      if (i < nrow(valid_results)) {
        cat("\n")  # Add spacing between variables
      }
    }
  }
  
  cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05\n")
  
  cat("\nInterpretation:\n")
  cat("- p > 0.05: Variances are homogeneous (equal variances assumed)\n")
  cat("- p ≤ 0.05: Variances are heterogeneous (equal variances NOT assumed)\n")
  
  # Always show recommendation for grouped data
  if (is_grouped_data) {
    cat("\nRecommendation based on Levene test:\n")
    unequal_groups <- sum(x$results$p_value <= 0.05, na.rm = TRUE)
    if (unequal_groups > 0) {
      cat(sprintf("- %d group(s) show unequal variances (p ≤ 0.05)\n", unequal_groups))
      cat("- Consider using Welch's t-test for groups with unequal variances\n")
    } else {
      cat("- All groups show equal variances (p > 0.05)\n")
      cat("- Standard t-test assumptions are met for all groups\n")
    }
  } else if (!is.null(x$original_test)) {
    cat("\nRecommendation based on Levene test:\n")
    if (any(x$results$p_value <= 0.05, na.rm = TRUE)) {
      cat("- Use Welch's t-test (unequal variances)\n")
    } else {
      cat("- Student's t-test or Welch's t-test both appropriate\n")
    }
  }
  
  invisible(x)
}



 