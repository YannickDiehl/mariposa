
#' Calculate Population-Representative Percentiles
#'
#' @description
#' \code{w_quantile()} calculates percentiles (quantiles) using survey weights
#' for population-representative results. Percentiles divide your data into
#' equal portions -- for example, the 25th percentile is the value below which
#' 25% of your population falls. This is essential for understanding the
#' distribution of variables like income, age, or satisfaction scores.
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The numeric variables you want to analyze. You can list multiple
#'   variables or use helpers like \code{starts_with("income")}
#' @param weights Survey weights to make results representative of your population.
#'   Without weights, you get the simple sample quantiles.
#' @param probs Which percentiles to calculate, as proportions between 0 and 1.
#'   Default: \code{c(0, 0.25, 0.5, 0.75, 1)} for the minimum, 25th percentile,
#'   median, 75th percentile, and maximum. Use \code{c(0.1, 0.5, 0.9)} for
#'   deciles, or \code{c(0.25, 0.5, 0.75)} for quartiles only.
#' @param na.rm Remove missing values before calculating? (Default: TRUE)
#'
#' @return Population-weighted quantile(s) with sample size information,
#'   including the weighted percentile values, effective sample size (effective N),
#'   and the number of valid observations used.
#'
#' @details
#' ## Understanding the Results
#'
#' - **Percentile values**: The data values at each requested percentile
#'   in the weighted population. For example, if the weighted 25th percentile
#'   of income is 35,000, then 25% of the population earns less than that.
#' - **Effective N**: How many independent observations your weighted data
#'   represents.
#' - **N**: The actual number of observations used.
#'
#' Common percentiles and their meaning:
#' - **0% (minimum)**: The smallest observed value
#' - **25% (Q1)**: One quarter of the population falls below this value
#' - **50% (median)**: Half the population falls below this value
#' - **75% (Q3)**: Three quarters of the population falls below this value
#' - **100% (maximum)**: The largest observed value
#'
#' ## When to Use This
#'
#' Use \code{w_quantile()} when:
#' - You want to know at what values the population splits into groups
#' - You need to construct population-representative income brackets or age groups
#' - You want to identify the median or quartiles with proper weighting
#' - You need SPSS-compatible weighted percentile values
#'
#' ## Formula
#'
#' Weighted quantiles are calculated using cumulative weights. Observations
#' are sorted by value, weights are accumulated, and the requested percentile
#' is found by linear interpolation at the point where the cumulative weight
#' proportion reaches the target probability.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Basic weighted quantiles (0%, 25%, 50%, 75%, 100%)
#' survey_data %>% w_quantile(age, weights = sampling_weight)
#'
#' # Custom quantiles
#' survey_data %>% w_quantile(income, weights = sampling_weight, probs = c(0.1, 0.5, 0.9))
#'
#' # Multiple variables
#' survey_data %>% w_quantile(age, income, weights = sampling_weight)
#'
#' # Grouped data
#' survey_data %>% group_by(region) %>% w_quantile(age, weights = sampling_weight)
#'
#' # Unweighted (for comparison)
#' survey_data %>% w_quantile(age)
#'
#' @seealso
#' \code{\link[stats]{quantile}} for the base R quantile function.
#'
#' \code{\link{w_median}} for the weighted median (50th percentile).
#'
#' \code{\link{w_iqr}} for the weighted interquartile range (Q3 - Q1).
#'
#' \code{\link{describe}} for comprehensive descriptive statistics including quantiles.
#'
#'
#' ## Validation status
#'
#' Unweighted values use quantile Type 6 (SPSS HAVERAGE). Weighted values
#' apply the HAVERAGE position to cumulative weights with linear
#' interpolation - an R-internal extension (Tier 4 of the Validation
#' Charter); SPSS reference validation for weighted percentiles is
#' pending.
#' @references
#' IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.
#'
#' @family weighted_statistics
#' @export
w_quantile <- function(data, ..., weights = NULL, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE) {

  # Display labels used in data-frame mode ("Min"/"Max" instead of "0%"/"100%")
  quantile_labels <- paste0(probs * 100, "%")
  quantile_labels[quantile_labels == "0%"] <- "Min"
  quantile_labels[quantile_labels == "100%"] <- "Max"

  # Weighted computation with zero valid observations
  empty_quantiles <- rep(NA_real_, length(probs))
  names(empty_quantiles) <- paste0(probs * 100, "%")

  result <- .w_statistic(
    data, ...,
    weights = {{ weights }},
    na.rm = na.rm,
    stat_fn = function(x, w) .quantile_stat(x, w, probs = probs, na.rm = na.rm),
    stat_name = "quantile",
    class_name = "w_quantile",
    extra_args = list(probs = probs),
    multi_value = TRUE,
    value_names = quantile_labels,
    empty_stat = empty_quantiles
  )

  # Summarise context: the factory returns the named quantile vector directly
  if (!is.data.frame(data)) {
    return(result)
  }

  # Preserve the historic element order (probs sits before is_grouped)
  out <- unclass(result)[c("results", "variables", "weights", "probs",
                           "is_grouped", "groups")]
  class(out) <- "w_quantile"
  out
}

#' Quantile statistic kernel for the w_* factory
#'
#' Returns a named vector of quantiles (one per probability).
#' Unweighted: quantile Type 6 = SPSS HAVERAGE (R's default Type 7 is NOT
#' SPSS-compatible). Weighted: HAVERAGE position on cumulative weights via
#' the shared .w_quantile() kernel.
#'
#' @noRd
.quantile_stat <- function(x, w, probs, na.rm) {
  if (is.null(w)) {
    quantile(x, probs = probs, na.rm = na.rm, type = 6)
  } else {
    .w_quantile(x, w, probs = probs, na.rm = FALSE)
  }
}

#' Print method for w_quantile objects
#'
#' @description
#' Prints a formatted summary of weighted quantile calculations, including quantiles,
#' sample sizes, and weights (if applicable). For grouped data, results are
#' displayed by group.
#'
#' @param x An object of class "w_quantile"
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments passed to print
#'
#' @keywords internal
#' @export
print.w_quantile <- function(x, digits = 3, ...) {
  is_weighted <- !is.null(x$weights)

  test_type <- get_standard_title("Quantile", x$weights, "Statistics")
  print_header(test_type)

  # Build the per-variable display table (Variable / Quantile / Value / N /
  # Effective_N / Weights) from the wide results columns
  .quantile_display <- function(results) {
    all_results <- list()

    for (var_name in x$variables) {
      var_quantile_cols <- names(results)[grepl(paste0("^", var_name, "_"), names(results))]
      var_quantile_cols <- var_quantile_cols[!grepl("_(n|eff_n)$", var_quantile_cols)]
      if (length(var_quantile_cols) == 0) next

      quantile_values <- as.numeric(results[1, var_quantile_cols])
      quantile_names <- gsub(paste0(var_name, "_"), "", var_quantile_cols)

      var_results <- data.frame(
        Variable = rep(var_name, length(quantile_names)),
        Quantile = quantile_names,
        Value = round(quantile_values, digits),
        stringsAsFactors = FALSE
      )

      n_col <- paste0(var_name, "_n")
      n_eff_col <- paste0(var_name, "_eff_n")
      if (n_col %in% names(results)) {
        var_results$N <- rep(round(results[[n_col]][1], 1), nrow(var_results))
      }
      if (is_weighted && n_eff_col %in% names(results)) {
        var_results$Effective_N <- rep(round(results[[n_eff_col]][1], 1), nrow(var_results))
      }
      if (!is.null(x$weights)) {
        var_results$Weights <- rep(x$weights, nrow(var_results))
      }

      all_results[[var_name]] <- var_results
    }

    if (length(all_results) == 0) return(NULL)
    as.data.frame(do.call(rbind, all_results))
  }

  if (x$is_grouped) {
    groups <- unique(x$results[x$groups])

    for (i in seq_len(nrow(groups))) {
      group_values <- groups[i, , drop = FALSE]

      # Format group info with factor levels if available
      group_info <- vapply(names(group_values), function(g) {
        val <- group_values[[g]]
        if (is.factor(val)) paste(g, "=", levels(val)[val]) else paste(g, "=", val)
      }, character(1))
      group_info <- paste(group_info, collapse = ", ")

      group_results <- x$results
      for (g in names(group_values)) {
        group_results <- group_results[group_results[[g]] == group_values[[g]], ]
      }
      if (nrow(group_results) == 0) next

      cat(sprintf("\nGroup: %s\n", group_info))

      results_df_print <- .quantile_display(group_results)
      if (!is.null(results_df_print)) {
        print_separator(get_table_width(results_df_print))
        print(results_df_print, row.names = FALSE)
        print_separator(get_table_width(results_df_print))
      }
    }
  } else {
    results_df_print <- .quantile_display(x$results)
    if (!is.null(results_df_print)) {
      print(results_df_print, row.names = FALSE)
      print_separator(get_table_width(results_df_print))
    } else {
      cat("No results to display.\n")
      cat("------------------------\n")
    }
  }
  invisible(x)
}
