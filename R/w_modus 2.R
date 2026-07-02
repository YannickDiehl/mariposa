
#' Find the Most Common Value in Your Population
#'
#' @description
#' \code{w_modus()} finds the mode (most frequently occurring value) of your data
#' using survey weights for population-representative results. The mode tells you
#' which category or value is the most common in your population. This is
#' especially useful for categorical variables (e.g., the most common education
#' level, the most frequent employment status).
#'
#' Unlike mean and median, the mode works with both numeric and categorical data.
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The variables you want to analyze. Works best with categorical or
#'   discrete numeric variables. You can list multiple variables or use helpers
#'   like \code{starts_with("trust")}
#' @param weights Survey weights to make results representative of your population.
#'   Without weights, the mode is simply the most frequent value in your sample.
#' @param na.rm Remove missing values before calculating? (Default: TRUE)
#'
#' @return Population-weighted mode(s) with sample size information,
#'   including the most common value (by weighted frequency), effective sample
#'   size (effective N), and the number of valid observations used.
#'
#' @details
#' ## Understanding the Results
#'
#' - **Weighted Mode**: The value that occurs most frequently in the weighted
#'   population. For weighted data, the mode is the value whose observations
#'   have the largest total weight.
#' - **Effective N**: How many independent observations your weighted data
#'   represents.
#' - **N**: The actual number of observations used.
#'
#' If multiple values share the highest weighted frequency (ties), the first
#' value encountered is returned.
#'
#' ## When to Use This
#'
#' Use \code{w_modus()} when:
#' - You want to find the most common response (e.g., most popular education level)
#' - You are working with categorical or ordinal data
#' - You want the "typical" value for discrete data where mean is not meaningful
#' - You need SPSS-compatible weighted mode values
#'
#' ## Formula
#'
#' The weighted mode is the value \eqn{x_k} that maximizes the total weight:
#'
#' \eqn{\text{Mode}_w = \arg\max_{x_k} \sum_{i: x_i = x_k} w_i}
#'
#' In other words, sum the weights for each unique value and pick the value
#' with the largest total weight.
#'
#' @family weighted_statistics
#' @export
#'
#' @seealso
#' \code{\link{w_median}} for the weighted middle value.
#'
#' \code{\link{w_mean}} for weighted means.
#'
#' \code{\link{frequency}} for complete frequency tables of categorical variables.
#'
#' \code{\link{describe}} for comprehensive descriptive statistics including the mode.
#'
#' @references
#' IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Basic weighted mode (most frequent value)
#' survey_data %>% w_modus(gender, weights = sampling_weight)
#'
#' # Multiple variables (works best with categorical/discrete data)
#' survey_data %>% w_modus(gender, region, weights = sampling_weight)
#'
#' # Grouped data
#' survey_data %>% group_by(region) %>% w_modus(gender, weights = sampling_weight)
#'
#' # In summarise context
#' survey_data %>% summarise(mode_gender = w_modus(gender, weights = sampling_weight))
#'
#' # Unweighted (for comparison)
#' survey_data %>% w_modus(gender)
w_modus <- function(data, ..., weights = NULL, na.rm = TRUE) {
  .w_statistic(
    data, ...,
    weights = {{ weights }},
    na.rm = na.rm,
    stat_fn = .modus_stat,
    stat_name = "modus",
    weighted_col = "weighted_mode",
    unweighted_col = "mode",
    class_name = "w_modus",
    # The mode may be non-numeric; an empty computation yields logical NA
    # (not NA_real_), matching the historic behavior
    empty_stat = NA,
    empty_n = 0,
    # The mode accepts any vector type in summarise context (factor,
    # character, ...), not just numeric
    vector_ok = function(x) TRUE,
    # Historic w_modus results carry no Variable column for a single variable
    single_var_variable_col = FALSE
  )
}

#' Mode statistic kernel for the w_* factory
#'
#' Unweighted: most frequent value via table(); ties resolved by first
#' occurrence in table order; numeric input is converted back from the
#' character table names. Weighted: the value whose observations have the
#' largest total weight; ties resolved by first occurrence in the data.
#'
#' @noRd
.modus_stat <- function(x, w) {
  if (length(x) == 0) return(NA)

  if (is.null(w)) {
    freq_table <- table(x)
    max_freq <- max(freq_table)
    modes <- names(freq_table)[freq_table == max_freq]
    result <- modes[1]
    if (is.numeric(x)) result <- as.numeric(result)
    result
  } else {
    unique_vals <- unique(x)
    weighted_freqs <- vapply(unique_vals, function(val) {
      sum(w[x == val])
    }, numeric(1))
    max_weight <- max(weighted_freqs)
    modes <- unique_vals[weighted_freqs == max_weight]
    modes[1]
  }
}

#' Print method for w_modus objects
#'
#' @param x A w_modus object
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object \code{x}.
#' @export
#' @method print w_modus
print.w_modus <- function(x, digits = 3, ...) {
  test_type <- get_standard_title("Mode", x$weights, "Statistics")
  print_header(test_type)

  is_grouped_data <- !is.null(x$is_grouped) && x$is_grouped

  if (is_grouped_data) {
    # Handle grouped results
    for (group_val in unique(x$results[[x$groups[1]]])) {
      group_results <- x$results[x$results[[x$groups[1]]] == group_val, ]

      cat(sprintf("\nGroup: %s = %s\n", x$groups[1], group_val))

      print_df <- group_results
      if (!is.null(x$weights)) {
        print_df <- print_df %>%
          dplyr::select(dplyr::any_of(c("Variable", "weighted_mode", "effective_n")))
        if (is.numeric(print_df$weighted_mode)) {
          print_df$weighted_mode <- round(print_df$weighted_mode, digits)
        }
        print_df$effective_n <- round(print_df$effective_n, 1)
      } else {
        print_df <- print_df %>%
          dplyr::select(dplyr::any_of(c("Variable", "mode", "n")))
        if (is.numeric(print_df$mode)) {
          print_df$mode <- round(print_df$mode, digits)
        }
      }

      print(print_df, row.names = FALSE)
    }
  } else {
    # Handle ungrouped results
    print_df <- x$results

    if (!is.null(x$weights)) {
      if ("Variable" %in% names(print_df)) {
        print_df <- print_df %>%
          dplyr::select(dplyr::any_of(c("Variable", "weighted_mode", "effective_n")))
      } else {
        mode_val <- print_df$weighted_mode[1]
        if (is.numeric(mode_val)) {
          mode_val <- round(mode_val, digits)
        }
        print_df <- tibble::tibble(
          Variable = x$variables[1],
          weighted_mode = mode_val,
          effective_n = round(print_df$effective_n[1], 1)
        )
      }
      if (is.numeric(print_df$weighted_mode)) {
        print_df$weighted_mode <- round(print_df$weighted_mode, digits)
      }
      print_df$effective_n <- round(print_df$effective_n, 1)
    } else {
      if ("Variable" %in% names(print_df)) {
        print_df <- print_df %>%
          dplyr::select(dplyr::any_of(c("Variable", "mode", "n")))
      } else {
        mode_val <- print_df$mode[1]
        if (is.numeric(mode_val)) {
          mode_val <- round(mode_val, digits)
        }
        print_df <- tibble::tibble(
          Variable = x$variables[1],
          mode = mode_val,
          n = round(print_df$n[1], 0)
        )
      }
      if (is.numeric(print_df$mode)) {
        print_df$mode <- round(print_df$mode, digits)
      }
    }

    print(print_df, row.names = FALSE)
  }

  cat("\n")
  invisible(x)
}
