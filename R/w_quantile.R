
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
#' @param weights Survey weights to make results representative of your
#'   population. Without weights, you get the simple sample quantiles. Give a
#'   column name (unquoted or as a string), an expression such as
#'   \code{sampling_weight * 2}, or a numeric vector with one weight per row.
#' @param probs Which percentiles to calculate, as proportions between 0 and 1.
#'   Default: \code{c(0, 0.25, 0.5, 0.75, 1)} for the minimum, 25th percentile,
#'   median, 75th percentile, and maximum. Use \code{c(0.1, 0.5, 0.9)} for
#'   deciles, or \code{c(0.25, 0.5, 0.75)} for quartiles only.
#' @param na.rm Remove missing values before calculating? (Default: TRUE).
#'   With \code{FALSE}, the result for a variable that contains missing
#'   values is \code{NA} (as in base R).
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
#' - **N / Missing**: Valid and missing cases. With weights, both are sums
#'   of weights (displayed rounded), as SPSS reports them under
#'   \code{WEIGHT BY}; Kish's effective N is shown by \code{summary()}.
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
  quantile_labels <- .w_quantile_labels(probs)

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

#' Column labels of w_quantile() results
#'
#' "25%", "33.33%" (at most two decimals, see .pct_label()), "Min"/"Max"
#' for 0 and 1.
#' @noRd
.w_quantile_labels <- function(probs) {
  labels <- paste0(.pct_label(probs), "%")
  labels[probs == 0] <- "Min"
  labels[probs == 1] <- "Max"
  labels
}

#' Print method for w_quantile objects
#'
#' @description
#' Prints one row per variable with the requested quantiles, N and Missing
#' (with weights: sums of weights, as in SPSS). For grouped data, one table
#' per group.
#'
#' @param x An object of class "w_quantile"
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments passed to print
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @keywords internal
#' @export
print.w_quantile <- function(x, digits = 3, ...) {
  .print_w_quantile(x, digits = digits)
}

#' @export
#' @method summary w_quantile
summary.w_quantile <- function(object, effective_n = TRUE, digits = 3, ...) {
  build_summary_object(object,
                       show = list(statistics = TRUE, effective_n = effective_n),
                       digits = digits, class_name = "summary.w_quantile")
}

#' Print summary of w_quantile results (with Kish's effective N)
#'
#' @param x A \code{summary.w_quantile} object.
#' @param ... Additional arguments (not used).
#' @return Invisibly returns the input object \code{x}.
#' @export
#' @method print summary.w_quantile
print.summary.w_quantile <- function(x, ...) {
  .print_w_quantile(x, digits = x$digits, effective_n = isTRUE(x$show$effective_n))
}

#' Shared printer for w_quantile results and their summary
#'
#' One table (per group) with Variable, one column per quantile, N and
#' Missing. The weights variable is named once in the header block (it was
#' repeated on every row).
#' @noRd
.print_w_quantile <- function(x, digits = 3, effective_n = FALSE) {
  weighted <- !is.null(x$weights)
  print_header(get_standard_title("Quantile", x$weights, "Statistics"))
  if (weighted) cat("Weights: ", x$weights, "\n", sep = "")

  labels <- .w_quantile_labels(x$probs)

  emit <- function(rows) {
    tab <- data.frame(Variable = x$variables, stringsAsFactors = FALSE)
    for (lab in labels) {
      tab[[lab]] <- vapply(x$variables, function(v) {
        col <- paste0(v, "_", lab)
        if (col %in% names(rows)) as.numeric(rows[[col]][1]) else NA_real_
      }, numeric(1))
    }
    get_col <- function(suffix) {
      vapply(x$variables, function(v) {
        col <- paste0(v, suffix)
        if (col %in% names(rows)) as.numeric(rows[[col]][1]) else NA_real_
      }, numeric(1))
    }
    tab$N <- if (weighted) get_col("_weighted_n") else get_col("_n")
    tab$Missing <- get_col("_missing")
    if (weighted && effective_n) tab[["Effective N"]] <- get_col("_eff_n")
    .print_desc_table(tab, digits = digits, col_digits = c("Effective N" = 1))
  }

  if (isTRUE(x$is_grouped)) {
    for_each_group(x$results, x$groups, function(rows, combo) {
      cat("\n")
      emit(rows)
    })
  } else {
    cat("\n")
    emit(x$results)
  }
  if (weighted && effective_n) {
    cat("  N and Missing are sums of weights; Effective N = (sum w)^2 / sum w^2 (Kish).\n")
  }
  invisible(x)
}


