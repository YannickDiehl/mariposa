
#' Get to Know Your Numeric Data
#'
#' @description
#' \code{describe()} gives you a complete summary of numeric variables - like age,
#' income, or satisfaction scores. It's your first step in any analysis, helping you
#' understand what's typical, what's unusual, and how spread out your data is.
#'
#' Think of it as a health check for your data that reveals:
#' - What's the average value?
#' - What's the middle value?
#' - How spread out are the responses?
#' - Are there unusual patterns or outliers?
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The numeric variables you want to summarize. List them separated by
#'   commas, or use helpers like \code{starts_with("trust")}
#' @param weights Optional survey weights for population-representative results.
#'   Without weights, you describe your sample. With weights, you describe the population.
#' @param show Which statistics to display:
#'   \itemize{
#'     \item \code{"short"} (default): Essential stats (mean, median, SD, range, IQR, skewness)
#'     \item \code{"all"}: Everything including variance, kurtosis, mode, quantiles
#'     \item Custom list: Choose specific stats like \code{c("mean", "sd", "range")}
#'       from \code{"mean"}, \code{"median"}, \code{"sd"}, \code{"se"},
#'       \code{"var"}, \code{"range"}, \code{"iqr"}, \code{"skew"},
#'       \code{"kurtosis"}, \code{"mode"}, \code{"quantiles"}. Unknown names
#'       are an error.
#'   }
#' @param probs For quantiles, which percentiles to show (default: 25th, 50th,
#'   75th). The 50th percentile is the median; when the median is shown it
#'   is not printed a second time (the result keeps both columns).
#' @param na.rm Remove missing values before calculating? (Default: TRUE).
#'   With \code{FALSE}, the result for a variable that contains missing
#'   values is \code{NA} (as in base R).
#' @param excess For kurtosis, show excess kurtosis? (Default: TRUE, easier to interpret)
#'
#' @return A summary table with descriptive statistics for each variable
#'
#' @details
#' ## Understanding the Results
#'
#' Key statistics and what they tell you:
#' - **n**: How many valid responses (watch for too many missing)
#' - **Mean**: The average value
#' - **Median**: The middle value (half above, half below)
#' - **SD**: Standard deviation - how spread out values are
#' - **Range**: The minimum and maximum values
#' - **IQR**: Interquartile range - the middle 50% of values
#' - **Skewness**: Whether data leans left (negative) or right (positive)
#' - **Kurtosis**: Whether you have unusual outliers
#'
#' ## When to Use This
#'
#' Always start here! Use \code{describe()} to:
#' - Check data quality (impossible values?)
#' - Understand distributions before testing
#' - Spot outliers that might affect analyses
#' - Compare groups side by side
#'
#' ## Interpreting Patterns
#'
#' - **Mean close to Median**: Data is roughly symmetric
#' - **Mean > Median**: Right-skewed (tail extends right)
#' - **Mean < Median**: Left-skewed (tail extends left)
#' - **Large SD**: Responses vary widely
#' - **Small SD**: Responses are similar
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#' 
#' # Basic unweighted analysis
#' survey_data %>% describe(age)
#' 
#' # Weighted analysis
#' survey_data %>% describe(age, weights = sampling_weight)
#' 
#' # Multiple variables with custom statistics
#' survey_data %>% describe(age, income, life_satisfaction, 
#'                         weights = sampling_weight, 
#'                         show = c("mean", "sd", "skew"))
#' 
#' # Grouped analysis
#' survey_data %>% 
#'   group_by(region) %>% 
#'   describe(age, weights = sampling_weight)
#'
#' @seealso
#' \code{\link[base]{summary}} for base R summary statistics.
#'
#' \code{\link{frequency}} for categorical variable summaries.
#'
#' \code{\link{w_mean}}, \code{\link{w_sd}}, \code{\link{w_median}} for
#' individual weighted statistics.
#'
#' \code{\link{t_test}} and \code{\link{oneway_anova}} for group comparisons.
#'
#' @family descriptive
#' @export
describe <- function(data, ..., weights = NULL,
                     show = "short", 
                     probs = c(0.25, 0.5, 0.75),
                     na.rm = TRUE, 
                     excess = TRUE) {
  
  # ============================================================================
  # INPUT VALIDATION AND SETUP
  # ============================================================================
  
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }
  
  # Validate show: an unknown value (e.g. "min") used to be ignored
  # silently, leaving a table with nothing but N and Missing
  valid_show <- c("short", "all", "mean", "median", "sd", "se", "var",
                  "range", "iqr", "skew", "kurtosis", "mode", "quantiles")
  if (!is.character(show) || length(show) == 0) {
    cli_abort("{.arg show} must be a character vector of statistic names.")
  }
  bad_show <- setdiff(show, valid_show)
  if (length(bad_show) > 0) {
    cli_abort(c(
      "Unknown {.arg show} value{?s}: {.val {bad_show}}.",
      "i" = "Valid values: {.val {valid_show}}."
    ))
  }

  # Process show parameter - expand shortcuts to full statistic names
  if ("all" %in% show) {
    show <- c("mean", "median", "sd", "se", "range", "iqr", "skew", "kurtosis", "var", "mode", "quantiles")
  } else if ("short" %in% show) {
    show <- c("mean", "median", "sd", "range", "iqr", "skew")
  }
  
  # Get variable names using tidyselect
  vars <- .process_variables(data, ...)

  # Validate that all selected variables are numeric (describe-specific)
  for (var_name in names(vars)) {
    if (!is.numeric(data[[var_name]])) {
      cli_abort("Variable {.var {var_name}} is not numeric. {.fn describe} only works with numeric variables.")
    }
  }

  # Process weights parameter
  weights_info <- .process_weights(data, rlang::enquo(weights))
  data <- weights_info$data
  
  # Check if data is grouped
  is_grouped <- inherits(data, "grouped_df")
  
  # ============================================================================
  # MAIN CALCULATION LOGIC
  # ============================================================================
  
  if (is_grouped) {
    results_df <- .calculate_grouped_stats(data, vars, weights_info, show, probs, na.rm, excess)
  } else {
    results_df <- .calculate_ungrouped_stats(data, vars, weights_info, show, probs, na.rm, excess)
  }
  
  # ============================================================================
  # CREATE RESULT OBJECT
  # ============================================================================
  
  result <- list(
    results = results_df,
    variables = names(vars),
    weights = weights_info$name,
    is_grouped = is_grouped,
    group_vars = if (is_grouped) dplyr::group_vars(data) else NULL,
    show = show,
    probs = probs
  )
  
  class(result) <- "describe"
  return(result)
}

# ============================================================================
# MAIN CALCULATION FUNCTIONS
# ============================================================================

#' Calculate statistics for ungrouped data
#' @noRd
.calculate_ungrouped_stats <- function(data, vars, weights_info, show, probs, na.rm, excess) {
  results_list <- list()
  
  for (i in seq_along(vars)) {
    var_name <- names(vars)[i]
    x <- data[[var_name]]
    w <- weights_info$vector
    
    # Calculate all statistics for this variable
    var_stats <- .calculate_variable_stats(x, w, var_name, show, probs, na.rm, excess)
    
    # Add to results with variable prefix for column names
    for (stat_name in names(var_stats)) {
      col_name <- paste0(var_name, "_", stat_name)
      results_list[[col_name]] <- var_stats[[stat_name]]
    }
  }
  
  return(tibble::tibble(!!!results_list))
}

#' Calculate statistics for grouped data
#' @noRd
.calculate_grouped_stats <- function(data, vars, weights_info, show, probs, na.rm, excess) {
  results <- data %>%
    dplyr::group_modify(~ {
      group_data <- .x
      results_list <- list()
      
      for (i in seq_along(vars)) {
        var_name <- names(vars)[i]
        x <- group_data[[var_name]]
        w <- if (!is.null(weights_info$name)) group_data[[weights_info$name]] else NULL
        
        # Calculate all statistics for this variable in this group
        var_stats <- .calculate_variable_stats(x, w, var_name, show, probs, na.rm, excess)
        
        # Add to results with variable prefix for column names
        for (stat_name in names(var_stats)) {
          col_name <- paste0(var_name, "_", stat_name)
          results_list[[col_name]] <- var_stats[[stat_name]]
        }
      }
      
      tibble::tibble(!!!results_list)
    })
  
  return(results %>% dplyr::ungroup())
}

#' Calculate all requested statistics for a single variable
#' @noRd
.calculate_variable_stats <- function(x, w, var_name, show, probs, na.rm, excess) {
  stats_list <- list()
  
  # Calculate missing values count
  n_missing <- sum(is.na(x))

  # na.rm = FALSE with missing values: every statistic is undefined (NA),
  # as in base R. The kernels must not see the NAs: the quantile family
  # returned shifted values (weighted) or aborted in quantile() (unweighted).
  # Statistics are computed on xs/ws, which are empty in that case (the
  # kernels give NA for empty input); N and Missing still describe x.
  if (!na.rm && .w_has_missing(x, w)) {
    xs <- x[0]
    ws <- NULL
  } else {
    xs <- x
    ws <- w
  }

  # Calculate each requested statistic
  if ("mean" %in% show) stats_list$Mean <- .w_mean(xs, ws, na.rm)
  if ("median" %in% show) stats_list$Median <- .w_median(xs, ws, na.rm)
  if ("sd" %in% show) stats_list$SD <- .w_sd(xs, ws, na.rm)
  if ("se" %in% show) stats_list$SE <- .w_se(xs, ws, na.rm)
  if ("var" %in% show) stats_list$Variance <- .w_var(xs, ws, na.rm)
  if ("range" %in% show) stats_list$Range <- .w_range(xs, ws, na.rm)
  if ("iqr" %in% show) stats_list$IQR <- .w_iqr(xs, ws, na.rm)
  if ("skew" %in% show) stats_list$Skewness <- .w_skew(xs, ws, na.rm)
  if ("kurtosis" %in% show) stats_list$Kurtosis <- .w_kurtosis(xs, ws, na.rm, excess)
  if ("mode" %in% show) stats_list$Mode <- .w_mode(xs, ws, na.rm)

  # Handle quantiles - can be multiple values
  if ("quantiles" %in% show) {
    quantiles <- .w_quantile(xs, ws, probs = probs, na.rm = na.rm)
    q_names <- .desc_quantile_name(probs)
    for (i in seq_along(probs)) {
      stats_list[[q_names[i]]] <- unname(quantiles[i])
    }
  }

  # Undefined statistics are NA, never NaN (a constant variable gives 0/0
  # in the skewness and kurtosis formulas)
  stats_list <- lapply(stats_list, function(v) {
    if (is.numeric(v)) v[is.nan(v)] <- NA_real_
    v
  })

  # Add sample size information
  if (is.null(w)) {
    stats_list$N <- sum(!is.na(x))
  } else {
    # Only sum weights where x is not missing (like frequency.R does)
    stats_list$N <- sum(w[!is.na(x)], na.rm = TRUE)
    # Effective N should also only consider weights for non-missing x
    stats_list$Effective_N <- .effective_n(w[!is.na(x)])
  }
  
  stats_list$Missing <- n_missing
  
  return(stats_list)
}

# ============================================================================
# PRINT METHOD AND FORMATTING
# ============================================================================

#' Print method for describe objects
#'
#' @description
#' Prints formatted descriptive statistics with professional layout.
#' Automatically adjusts output based on whether analysis was weighted or
#' unweighted. The output of \code{describe()} is a summary table by
#' nature, so \code{print()} and \code{summary()} display the same
#' statistics table; \code{summary()} additionally offers a
#' \code{statistics} section toggle and a \code{digits} option.
#'
#' @param x A describe object
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments (currently unused)
#' @return Invisibly returns the input object \code{x}.
#' @export
print.describe <- function(x, digits = 3, ...) {

  # Determine if weighted analysis
  is_weighted <- !is.null(x$weights)

  # Print header using standardized helper
  title <- get_standard_title("Descriptive", x$weights, "Statistics")
  print_header(title)

  # Print results based on grouping
  if (x$is_grouped) {
    .print_grouped_results(x, is_weighted, digits = digits)
  } else {
    .print_ungrouped_results(x, is_weighted, digits = digits)
  }

  invisible(x)
}

#' Summary method for describe results
#'
#' @description
#' Creates a summary object that produces the descriptive statistics
#' table when printed. Since \code{describe()} output is already a
#' summary table by nature, the printed content matches \code{print()};
#' the summary layer adds the \code{statistics} section toggle and the
#' \code{digits} formatting option for consistency with the other
#' analysis functions.
#'
#' @param object A \code{describe} result object.
#' @param statistics Logical. Show the descriptive statistics table?
#'   (Default: TRUE)
#' @param digits Number of decimal places for formatting (Default: 3).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.describe} object.
#'
#' @examples
#' result <- describe(survey_data, age, income)
#' summary(result)
#' summary(result, digits = 2)
#'
#' @seealso \code{\link{describe}} for the main analysis function.
#' @export
#' @method summary describe
summary.describe <- function(object, statistics = TRUE, digits = 3, ...) {
  out <- build_summary_object(
    object     = object,
    show       = list(statistics = statistics),
    digits     = digits,
    class_name = "summary.describe"
  )
  # describe objects already carry a $show field (the selected statistics);
  # preserve it under a different name so the summary $show toggles can
  # take its place.
  out$stat_selection <- object$show
  out
}

#' Print summary of describe results (detailed output)
#'
#' @description
#' Displays the descriptive statistics table for a \code{describe}
#' result, controlled by the boolean parameter passed to
#' \code{\link{summary.describe}}.
#'
#' @param x A \code{summary.describe} object created by
#'   \code{\link{summary.describe}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- describe(survey_data, age, income)
#' summary(result)
#'
#' @seealso \code{\link{describe}} for the main analysis,
#'   \code{\link{summary.describe}} for summary options.
#' @export
#' @method print summary.describe
print.summary.describe <- function(x, ...) {
  digits <- x$digits

  # Determine if weighted analysis
  is_weighted <- !is.null(x$weights)

  # Print header using standardized helper
  title <- get_standard_title("Descriptive", x$weights, "Statistics")
  print_header(title)

  if (isTRUE(x$show$statistics)) {
    # Restore the statistic selection under $show for the shared renderers
    obj <- x
    obj$show <- x$stat_selection

    if (x$is_grouped) {
      .print_grouped_results(obj, is_weighted, digits = digits)
    } else {
      .print_ungrouped_results(obj, is_weighted, digits = digits)
    }
  }

  invisible(x)
}

# Note: .print_header function removed - using standardized print_header from print_helpers.R

#' Print results for ungrouped data
#' @noRd
.print_ungrouped_results <- function(x, is_weighted, digits = 3) {
  output_df <- .create_output_df(x$results, x$variables, x$show, is_weighted,
                                 digits = digits, probs = x$probs)
  print(output_df, row.names = FALSE)

  # Print footer border
  print_separator(get_table_width(output_df))
}

#' Print results for grouped data
#' @noRd
.print_grouped_results <- function(x, is_weighted, digits = 3) {
  group_vars <- x$group_vars
  results_df <- x$results

  # Get unique group combinations
  group_combinations <- results_df %>%
    dplyr::select(dplyr::all_of(group_vars)) %>%
    dplyr::distinct()

  for (i in seq_len(nrow(group_combinations))) {
    # Filter data for this group
    group_filter <- group_combinations[i, , drop = FALSE]
    filter_condition <- TRUE
    for (gvar in group_vars) {
      filter_condition <- filter_condition & .group_match(results_df[[gvar]], group_filter[[gvar]])
    }
    group_data <- results_df[filter_condition, , drop = FALSE]

    # Print group header using standardized helper
    print_group_header(group_filter)

    # Create and print output
    temp_output <- .create_output_df(group_data, x$variables, x$show, is_weighted,
                                     digits = digits, probs = x$probs)
    print_separator(get_table_width(temp_output))
    print(temp_output, row.names = FALSE)
    print_separator(get_table_width(temp_output))
  }
}

#' Create formatted output data frame for printing
#' @noRd
.create_output_df <- function(results_df, variables, show, is_weighted, digits = 3,
                              probs = c(0.25, 0.5, 0.75)) {
  output_rows <- list()
  
  for (var_name in variables) {
    row_data <- list(Variable = var_name)
    
    # Extract statistics for this variable
    for (stat in show) {
      col_name <- paste0(var_name, "_", 
                        switch(stat,
                               "mean" = "Mean", "median" = "Median", "sd" = "SD",
                               "se" = "SE", "var" = "Variance", "range" = "Range",
                               "iqr" = "IQR", "skew" = "Skewness", "kurtosis" = "Kurtosis",
                               "mode" = "Mode", "quantiles" = "Q25"))
      
      if (stat == "quantiles") {
        # Quantile columns by their exact names: a regex prefix match
        # (grep("^var_Q")) also caught variables such as income_Quintile
        # and failed on names with metacharacters (`Einkommen (EUR)`)
        # The 50th percentile IS the median (same HAVERAGE kernel); it
        # stays in the result but is not printed twice
        q_probs <- if ("median" %in% show) probs[probs != 0.5] else probs
        for (q_name in .desc_quantile_name(q_probs)) {
          q_col <- paste0(var_name, "_", q_name)
          if (q_col %in% names(results_df)) {
            row_data[[q_name]] <- round(results_df[[q_col]], digits)
          }
        }
      } else if (col_name %in% names(results_df)) {
        value <- results_df[[col_name]]
        if (is.numeric(value)) {
          stat_name <- switch(stat, "mean" = "Mean", "median" = "Median", "sd" = "SD",
                             "se" = "SE", "var" = "Variance", "range" = "Range",
                             "iqr" = "IQR", "skew" = "Skewness", "kurtosis" = "Kurtosis",
                             "mode" = "Mode")
          row_data[[stat_name]] <- round(value, digits)
        } else {
          row_data[["Mode"]] <- value
        }
      }
    }
    
    # Add sample size information - different for weighted vs unweighted
    if (is_weighted) {
      # For weighted analysis, show effective sample size
      eff_n_col <- paste0(var_name, "_Effective_N")
      if (eff_n_col %in% names(results_df)) {
        row_data$Effective_N <- round(results_df[[eff_n_col]], 1)
      }
    } else {
      # For unweighted analysis, show regular N and missing count
      n_col <- paste0(var_name, "_N")
      if (n_col %in% names(results_df)) {
        row_data$N <- round(results_df[[n_col]], 0)
      }
      
      missing_col <- paste0(var_name, "_Missing")
      if (missing_col %in% names(results_df)) {
        row_data$Missing <- round(results_df[[missing_col]], 0)
      }
    }
    
    output_rows[[length(output_rows) + 1]] <- row_data
  }
  
  # Convert to data frame
  output_df <- do.call(rbind, lapply(output_rows, function(x) {
    data.frame(x, stringsAsFactors = FALSE)
  }))
  
  return(output_df)
}

# Note: .get_border function removed - using standardized get_table_width from print_helpers.R
 


#' Result/display name of a quantile column ("Q25" for probs = 0.25)
#'
#' Percent values are shown with at most two decimals ("Q33.33" for
#' probs = 1/3 instead of "Q33.3333333333333"); more decimals are used only
#' when two requested probabilities would otherwise get the same name.
#'
#' @param p Probabilities
#' @return Character vector
#' @noRd
.desc_quantile_name <- function(p) {
  pct <- p * 100
  for (d in 2:10) {
    nm <- as.character(round(pct, d))
    if (!anyDuplicated(nm) || anyDuplicated(pct)) break
  }
  paste0("Q", nm)
}
