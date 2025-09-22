#' Parameter Estimates for Repeated Measures ANOVA
#'
#' @description
#' Calculates parameter estimates (regression coefficients) for repeated measures 
#' ANOVA results. Provides detailed information about group differences at each 
#' time point, including confidence intervals and effect sizes.
#'
#' @param x An object containing repeated measures ANOVA results
#' @param conf.level Confidence level for intervals. Default is \code{0.95}.
#' @param reference_group Character string specifying which group to use as reference. 
#'   Options: \code{"first"} (default), \code{"last"}, or specific group value.
#' @param ... Additional arguments passed to methods
#'
#' @details
#' Parameter estimates show regression coefficients for each group comparison 
#' relative to a reference group. This is equivalent to SPSS "Parameterschätzungen" 
#' output and helps interpret significant interactions by showing where group 
#' differences occur across time points.
#'
#' The function calculates:
#' \itemize{
#'   \item Intercept (constant term) = mean of reference group
#'   \item Group effects = difference from reference group
#'   \item Standard errors and t-statistics for all parameters
#'   \item 95% confidence intervals
#'   \item Partial eta-squared for each parameter
#'   \item Observed power (statistical power analysis)
#' }
#'
#' @return An object of class \code{"parameter_estimates_results"} containing:
#' \itemize{
#'   \item \code{estimates}: Parameter estimates table for each time point
#'   \item \code{reference_group}: Which group was used as reference
#'   \item \code{group_means}: Actual group means for verification
#'   \item \code{original_test}: Reference to original ANOVA results
#' }
#'
#' @examples
#' \dontrun{
#' # Load data and perform repeated measures ANOVA
#' result <- data %>% 
#'   oneway_anova_test(var1, var2, group = group, repeated = TRUE, subject_id = id)
#' 
#' # Calculate parameter estimates
#' parameter_estimates(result)
#' 
#' # Use different reference group
#' parameter_estimates(result, reference_group = "last")
#' parameter_estimates(result, reference_group = "Treatment")
#' }
#'
#' @export
parameter_estimates <- function(x, conf.level = 0.95, reference_group = "first", ...) {
  UseMethod("parameter_estimates")
}

#' @rdname parameter_estimates
#' @export
parameter_estimates.oneway_anova_test_results <- function(x, conf.level = 0.95, reference_group = "first", ...) {
  
  # Check if this is repeated measures ANOVA
  if (is.null(x$repeated) || !x$repeated) {
    stop("parameter_estimates() is currently only implemented for repeated measures ANOVA. Use repeated = TRUE in oneway_anova_test()")
  }
  
  # Get the data and variable names
  long_data <- x$raw_data
  group_name <- x$group
  var_names <- x$variables
  subject_name <- x$subject_id
  
  # Get group levels (convert to character to avoid factor issues)
  group_levels <- as.character(sort(unique(long_data[[group_name]])))
  n_groups <- length(group_levels)
  
  if (n_groups < 2) {
    stop("Parameter estimates require at least 2 groups")
  }
  
  # Determine reference group
  if (reference_group == "first") {
    ref_group <- group_levels[1]
  } else if (reference_group == "last") {
    ref_group <- group_levels[n_groups]
  } else if (reference_group %in% group_levels) {
    ref_group <- reference_group
  } else {
    stop(sprintf("Reference group '%s' not found in data. Available groups: %s", 
                 reference_group, paste(group_levels, collapse = ", ")))
  }
  
  # Reorder groups so reference is last (matches SPSS convention)
  other_groups <- group_levels[group_levels != ref_group]
  ordered_groups <- c(other_groups, ref_group)
  
  # Calculate parameter estimates for each time point
  estimates_list <- list()
  group_means_list <- list()
  
  for (time_point in var_names) {
    # Get data for this time point
    time_data <- long_data %>%
      filter(Time == time_point) %>%
      select(all_of(c(subject_name, group_name, "Response")))
    
    # Ensure all groups are present in the data
    present_groups <- unique(time_data[[group_name]])
    if (length(present_groups) != n_groups) {
      stop(sprintf("Not all groups present in time point %s. Expected: %s, Found: %s", 
                   time_point, paste(group_levels, collapse = ", "), 
                   paste(present_groups, collapse = ", ")))
    }
    
    # Convert group to factor with reference group as last level
    time_data[[group_name]] <- factor(time_data[[group_name]], levels = ordered_groups)
    
    # Fit regression model (equivalent to ANOVA with contrasts)
    formula_str <- paste("Response ~", group_name)
    model <- lm(as.formula(formula_str), data = time_data)
    
    # Extract model summary
    model_summary <- summary(model)
    coefficients <- model_summary$coefficients
    
    # Calculate group means for verification
    group_means <- time_data %>%
      group_by(across(all_of(group_name))) %>%
      summarise(
        n = n(),
        mean = mean(Response, na.rm = TRUE),
        sd = sd(Response, na.rm = TRUE),
        se = sd / sqrt(n),
        .groups = "drop"
      )
    
    # Calculate confidence intervals
    conf_ints <- confint(model, level = conf.level)
    
    # Calculate partial eta-squared for each parameter
    # For regression: partial eta² = t² / (t² + df_residual)
    t_values <- coefficients[, "t value"]
    df_residual <- model_summary$df[2]
    partial_eta_sq <- t_values^2 / (t_values^2 + df_residual)
    
    # Calculate observed power (approximate)
    # Power = P(|t| > t_critical | noncentrality parameter)
    t_critical <- qt(1 - (1-conf.level)/2, df_residual)
    ncp <- abs(t_values)  # noncentrality parameter
    observed_power <- 1 - pt(t_critical, df_residual, ncp) + pt(-t_critical, df_residual, ncp)
    
    # Create parameter estimates table
    param_names <- rownames(coefficients)
    n_params <- length(param_names)
    
    # Format parameter names for display and create estimates table
    estimates_table <- data.frame(
      Parameter = character(0),
      Estimate = numeric(0),
      Std_Error = numeric(0),
      t_value = numeric(0),
      p_value = numeric(0),
      CI_Lower = numeric(0),
      CI_Upper = numeric(0),
      Partial_Eta_Sq = numeric(0),
      Observed_Power = numeric(0),
      stringsAsFactors = FALSE
    )
    
    # Process each parameter
    for (i in 1:n_params) {
      original_name <- param_names[i]
      
      if (original_name == "(Intercept)") {
        display_name <- "Konstanter Term"
      } else {
        # Extract group value from parameter name (e.g., "groupControl" -> "Control")
        group_value <- gsub(paste0("^", group_name), "", original_name)
        # Use numeric coding for cleaner display: 1 for non-reference, 2 for reference
        display_name <- sprintf("[%s=1]", group_name)
      }
      
      # Add row to estimates table
      row_data <- data.frame(
        Parameter = display_name,
        Estimate = coefficients[i, "Estimate"],
        Std_Error = coefficients[i, "Std. Error"],
        t_value = coefficients[i, "t value"],
        p_value = coefficients[i, "Pr(>|t|)"],
        CI_Lower = conf_ints[i, 1],
        CI_Upper = conf_ints[i, 2],
        Partial_Eta_Sq = partial_eta_sq[i],
        Observed_Power = observed_power[i],
        stringsAsFactors = FALSE
      )
      
      estimates_table <- rbind(estimates_table, row_data)
    }
    
    # Add reference group row (set to 0) 
    ref_row <- data.frame(
      Parameter = sprintf("[%s=2]", group_name),
      Estimate = 0,
      Std_Error = 0,
      t_value = 0,
      p_value = NA,
      CI_Lower = 0,
      CI_Upper = 0,
      Partial_Eta_Sq = NA,
      Observed_Power = NA,
      stringsAsFactors = FALSE
    )
    estimates_table <- rbind(estimates_table, ref_row)
    

    
    # Add variable name
    estimates_table$Variable <- time_point
    
    # Reorder columns
    estimates_table <- estimates_table[, c("Variable", "Parameter", "Estimate", "Std_Error", 
                                         "t_value", "p_value", "CI_Lower", "CI_Upper", 
                                         "Partial_Eta_Sq", "Observed_Power")]
    
    estimates_list[[time_point]] <- estimates_table
    group_means_list[[time_point]] <- group_means
  }
  
  # Combine all estimates
  all_estimates <- do.call(rbind, estimates_list)
  rownames(all_estimates) <- NULL
  
  # Combine all group means
  all_group_means <- do.call(rbind, group_means_list)
  all_group_means$Variable <- rep(var_names, each = n_groups)
  all_group_means <- all_group_means[, c("Variable", group_name, "n", "mean", "sd", "se")]
  
  result <- list(
    estimates = all_estimates,
    group_means = all_group_means,
    reference_group = ref_group,
    group_levels = ordered_groups,
    original_test = x,
    variables = var_names,
    group = group_name,
    subject_id = subject_name,
    conf.level = conf.level
  )
  
  class(result) <- "parameter_estimates_results"
  return(result)
}

#' Print method for parameter estimates
#' 
#' @param x An object of class \code{"parameter_estimates_results"}
#' @param digits Integer specifying the number of decimal places to display. Default is \code{3}.
#' @param ... Additional arguments passed to \code{\link[base]{print}}. Currently unused.
#' 
#' @export
print.parameter_estimates_results <- function(x, digits = 3, ...) {
  
  cat("\nParameter Estimates (Repeated Measures ANOVA)\n")
  cat("=============================================\n")
  
  # Print test information
  cat("\n")
  cat(sprintf("Between-Subjects Factor: %s\n", x$group))
  cat(sprintf("Within-Subjects Factor: Time (%s)\n", paste(x$variables, collapse = " vs ")))
  cat(sprintf("Reference Group: %s (set to 0)\n", x$reference_group))
  cat(sprintf("Confidence Level: %.1f%%\n", x$conf.level * 100))
  cat("\n")
  
  # Process estimates for display
  estimates_display <- x$estimates
  
  # Round numeric columns
  numeric_cols <- c("Estimate", "Std_Error", "t_value", "p_value", "CI_Lower", "CI_Upper", 
                   "Partial_Eta_Sq", "Observed_Power")
  
  for (col in numeric_cols) {
    if (col %in% names(estimates_display)) {
      estimates_display[[col]] <- round(estimates_display[[col]], digits)
    }
  }
  
  # Format p-values
  estimates_display$p_value <- ifelse(
    is.na(estimates_display$p_value), ".",
    ifelse(estimates_display$p_value < 0.001, "<.001", 
           sprintf("%.3f", estimates_display$p_value))
  )
  
  # Format other columns for missing reference group values
  ref_cols <- c("Std_Error", "t_value", "CI_Lower", "CI_Upper", "Partial_Eta_Sq", "Observed_Power")
  for (col in ref_cols) {
    if (col %in% names(estimates_display)) {
      estimates_display[[col]] <- ifelse(
        estimates_display$Parameter == sprintf("[%s=2]", x$group) & 
        estimates_display[[col]] == 0, ".", 
        ifelse(is.na(estimates_display[[col]]), ".", as.character(estimates_display[[col]]))
      )
    }
  }
  
  # Rename columns for display (shorter names to fit in one line)
  names(estimates_display) <- c("Variable", "Parameter", "Est", "SE", "t", "Sig.", 
                               "CI_Low", "CI_High", "Eta2", "Power")
  
  # Print table for each variable
  for (var in x$variables) {
    var_data <- estimates_display[estimates_display$Variable == var, -1]  # Remove Variable column
    
    # Print variable header in framework design
    cat(sprintf("\n--- %s ---\n\n", var))
    
    # Print table title
    cat("Parameter Estimates:\n")
    
    # Calculate border width based on actual table output
    captured_output <- capture.output(print(var_data, row.names = FALSE))
    actual_width <- max(nchar(captured_output), na.rm = TRUE)
    border_width <- paste(rep("-", actual_width), collapse = "")
    
    # Print dynamic box with table
    cat(border_width, "\n")
    print(var_data, row.names = FALSE, na.print = ".")
    cat(border_width, "\n")
  }
  
  cat("Notes:\n")
  cat(sprintf("- Reference group (%s) is set to 0 (redundant parameter)\n", x$reference_group))
  cat(sprintf("- Confidence intervals based on %.1f%% level\n", x$conf.level * 100))
  cat("- Eta2 shows partial eta-squared effect size for each parameter\n")
  cat("- Observed Power shows statistical power achieved\n")
  
  invisible(x)
} 