
#' Chi-Square Goodness-of-Fit Test
#'
#' @description
#' \code{chisq_gof()} tests whether observed frequencies of a categorical
#' variable match expected frequencies. By default, it tests against equal
#' proportions (uniform distribution). You can also specify custom expected
#' proportions.
#'
#' Think of it as:
#' - Testing whether categories are equally distributed
#' - Comparing observed distribution to a theoretical distribution
#' - The one-sample version of the chi-square test
#'
#' The test tells you:
#' - Whether observed frequencies differ from expected frequencies
#' - How strong the deviation is (chi-square statistic)
#' - A frequency table with observed, expected, and residual counts
#'
#' @param data Your survey data (data frame or tibble)
#' @param ... One or more categorical variables to test (tidyselect supported)
#' @param expected Optional numeric vector of expected proportions (must sum to 1).
#'   Only used when a single variable is tested. If NULL (default), equal
#'   proportions are assumed.
#' @param weights Optional survey weights for population-representative results
#'
#' @return Test results showing whether observed frequencies match expected,
#'   including:
#' - Chi-square statistic and p-value for each variable
#' - Degrees of freedom
#' - Frequency table with observed, expected, and residual counts
#' - Sample size (N)
#'
#' @details
#' ## Understanding the Results
#'
#' **P-value**: If p < 0.05, the distribution differs from expected
#' - p < 0.001: Very strong evidence the distribution differs
#' - p < 0.01: Strong evidence the distribution differs
#' - p < 0.05: Moderate evidence the distribution differs
#' - p >= 0.05: No significant deviation from expected distribution
#'
#' **Residuals**: The difference between observed and expected counts.
#' Large positive residuals indicate a category has more cases than expected;
#' large negative residuals indicate fewer cases than expected.
#'
#' ## When to Use This
#'
#' Use this test when:
#' - You want to check whether a categorical variable follows a specific distribution
#' - You want to test if categories are equally distributed (uniform)
#' - You have a single categorical variable and a hypothesised distribution
#'
#' ## The Chi-Square Goodness-of-Fit Statistic
#'
#' \deqn{\chi^2 = \sum \frac{(O_i - E_i)^2}{E_i}}
#'
#' where O_i = observed frequency, E_i = expected frequency.
#'
#' Degrees of freedom = number of categories - 1.
#'
#' ## Relationship to Other Tests
#'
#' - For testing association between two categorical variables:
#'   Use \code{\link{chi_square}()} instead
#' - For testing a single binary proportion:
#'   Use \code{\link{binomial_test}()} instead
#' - For small samples where expected frequencies are below 5:
#'   Use \code{\link{fisher_test}()} instead
#'
#' ## SPSS Equivalent
#'
#' SPSS: \code{NPAR TESTS /CHISQUARE=variable /EXPECTED=EQUAL}
#' or: \code{NPAR TESTS /CHISQUARE=variable /EXPECTED=50 30 20}
#'
#' @seealso
#' \code{\link{chi_square}} for chi-square test of independence (two variables).
#'
#' \code{\link{binomial_test}} for testing a single proportion.
#'
#' @references
#' Pearson, K. (1900). On the criterion that a given system of deviations from
#' the probable in the case of a correlated system of variables is such that it
#' can be reasonably supposed to have arisen from random sampling.
#' \emph{Philosophical Magazine}, 50(302), 157-175.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Test whether gender is equally distributed
#' survey_data %>%
#'   chisq_gof(gender)
#'
#' # Test multiple variables at once
#' survey_data %>%
#'   chisq_gof(gender, region, education)
#'
#' # Custom expected proportions
#' survey_data %>%
#'   chisq_gof(interview_mode, expected = c(0.5, 0.3, 0.2))
#'
#' # With weights
#' survey_data %>%
#'   chisq_gof(gender, weights = sampling_weight)
#'
#' # Grouped analysis
#' survey_data %>%
#'   group_by(region) %>%
#'   chisq_gof(education)
#'
#' @family hypothesis_tests
#' @export
chisq_gof <- function(data, ..., expected = NULL, weights = NULL) {

  # Input validation
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }

  # Check if data is grouped
  is_grouped <- inherits(data, "grouped_df")
  grp_vars <- if (is_grouped) dplyr::group_vars(data) else NULL

  # Select variables using centralized helper
  vars <- .process_variables(data, ...)
  var_names <- names(vars)

  # Validate expected proportions
  if (!is.null(expected)) {
    if (!is.numeric(expected) || any(expected < 0)) {
      cli_abort("{.arg expected} must be a numeric vector of non-negative proportions.")
    }
    if (abs(sum(expected) - 1) > 0.01) {
      cli_abort(c(
        "{.arg expected} proportions must sum to 1.",
        "x" = "Current sum: {round(sum(expected), 4)}"
      ))
    }
  }

  # Process weights
  weights_info <- .process_weights(data, rlang::enquo(weights))
  w_name <- weights_info$name

  # Validate all variables are categorical (before any computation)
  for (vn in var_names) {
    vals_check <- data[[vn]]
    if (is.numeric(vals_check) && !is.factor(vals_check)) {
      n_unique <- length(unique(na.omit(vals_check)))
      if (n_unique > 20) {
        cli_abort(c(
          "{.var {vn}} appears to be a continuous variable.",
          "i" = "Chi-square goodness-of-fit requires a categorical variable.",
          "i" = "Found {n_unique} unique values."
        ))
      }
    }
  }

  # Validate expected proportions length against categories (before computation)
  if (!is.null(expected) && length(var_names) == 1) {
    k_check <- length(unique(na.omit(data[[var_names[1]]])))
    if (length(expected) != k_check) {
      cli_abort(c(
        "Length of {.arg expected} does not match number of categories.",
        "x" = "Expected {k_check} proportions, got {length(expected)}."
      ))
    }
  }

  # Helper to perform GoF test on a single variable in a single data slice
  perform_single_gof <- function(data_slice, var_name, expected_props = NULL) {
    vals <- data_slice[[var_name]]

    # Remove NAs
    valid <- !is.na(vals)
    if (!is.null(w_name)) {
      w <- data_slice[[w_name]]
      valid <- valid & !is.na(w)
      w <- w[valid]
    }
    vals <- vals[valid]

    # Build frequency table
    if (!is.null(w_name)) {
      freq_tbl <- xtabs(w ~ vals)
      freq_tbl <- round(freq_tbl)
    } else {
      freq_tbl <- table(vals)
    }

    n <- sum(freq_tbl)
    k <- length(freq_tbl)

    # Determine expected frequencies
    if (!is.null(expected_props)) {
      if (length(expected_props) != k) {
        cli_abort(c(
          "Length of {.arg expected} does not match number of categories.",
          "x" = "Expected {k} proportions, got {length(expected_props)}."
        ))
      }
      expected_freq <- n * expected_props
    } else {
      # Equal proportions (SPSS default)
      expected_freq <- rep(n / k, k)
    }

    # Chi-square statistic
    chi_sq <- sum((as.numeric(freq_tbl) - expected_freq)^2 / expected_freq)
    df_val <- k - 1
    p_value <- pchisq(chi_sq, df = df_val, lower.tail = FALSE)

    # Build frequency detail table
    freq_df <- data.frame(
      category = names(freq_tbl),
      observed = as.integer(freq_tbl),
      expected = round(expected_freq, 1),
      residual = round(as.numeric(freq_tbl) - expected_freq, 1),
      stringsAsFactors = FALSE
    )

    list(
      chi_sq = chi_sq,
      df = df_val,
      p_value = p_value,
      n = n,
      freq_table = freq_df
    )
  }

  # Main execution
  if (is_grouped) {
    data_list <- dplyr::group_split(data)
    group_keys_df <- dplyr::group_keys(data)

    results_list <- lapply(seq_along(data_list), function(i) {
      var_results <- lapply(var_names, function(vn) {
        tryCatch({
          res <- perform_single_gof(data_list[[i]], vn,
                                    if (length(var_names) == 1) expected else NULL)
          cbind(
            group_keys_df[i, , drop = FALSE],
            data.frame(
              Variable = vn,
              chi_sq = res$chi_sq,
              df = res$df,
              p_value = res$p_value,
              n = res$n,
              stringsAsFactors = FALSE
            )
          )
        }, error = function(e) {
          cbind(
            group_keys_df[i, , drop = FALSE],
            data.frame(
              Variable = vn,
              chi_sq = NA_real_,
              df = NA_integer_,
              p_value = NA_real_,
              n = NA_integer_,
              stringsAsFactors = FALSE
            )
          )
        })
      })
      do.call(rbind, var_results)
    })

    results_df <- do.call(rbind, results_list)
    rownames(results_df) <- NULL

    result <- list(
      results = results_df,
      variables = var_names,
      weights = w_name,
      is_grouped = TRUE,
      groups = grp_vars,
      expected = expected,
      frequencies = NULL
    )

  } else {
    var_results <- lapply(var_names, function(vn) {
      tryCatch({
        res <- perform_single_gof(data, vn,
                                  if (length(var_names) == 1) expected else NULL)
        list(
          row = data.frame(
            Variable = vn,
            chi_sq = res$chi_sq,
            df = res$df,
            p_value = res$p_value,
            n = res$n,
            stringsAsFactors = FALSE
          ),
          freq = res$freq_table
        )
      }, error = function(e) {
        list(
          row = data.frame(
            Variable = vn,
            chi_sq = NA_real_,
            df = NA_integer_,
            p_value = NA_real_,
            n = NA_integer_,
            stringsAsFactors = FALSE
          ),
          freq = NULL
        )
      })
    })

    results_df <- do.call(rbind, lapply(var_results, `[[`, "row"))
    rownames(results_df) <- NULL

    freq_list <- lapply(var_results, `[[`, "freq")
    names(freq_list) <- var_names

    # For single variable, store flat data frame for easy access (freq$observed)
    freq_out <- if (length(var_names) == 1) freq_list[[1]] else freq_list

    result <- list(
      results = results_df,
      variables = var_names,
      weights = w_name,
      is_grouped = FALSE,
      groups = NULL,
      expected = expected,
      frequencies = freq_out
    )
  }

  class(result) <- "chisq_gof"
  return(result)
}

#' Print chi-square goodness-of-fit test results
#'
#' @param x An object of class \code{"chisq_gof"}
#' @param digits Number of decimal places (default: 4)
#' @param ... Additional arguments (currently unused)
#' @return Invisibly returns the input object \code{x}.
#' @export
print.chisq_gof <- function(x, digits = 4, ...) {
  weights_name <- x$weights
  test_type <- get_standard_title("Chi-Square Goodness-of-Fit Test",
                                  weights_name, "Results")
  print_header(test_type, newline_before = FALSE)

  cat("\n")
  test_info <- list(
    "Variables" = paste(x$variables, collapse = ", "),
    "Expected" = if (is.null(x$expected)) "Equal proportions" else
      paste(x$expected, collapse = ", "),
    "Weights variable" = x$weights
  )
  print_info_section(test_info)
  cat("\n")

  if (isTRUE(x$is_grouped)) {
    groups <- unique(x$results[x$groups])

    for (i in seq_len(nrow(groups))) {
      group_values <- groups[i, , drop = FALSE]
      print_group_header(group_values)

      group_results <- x$results
      for (g in names(group_values)) {
        group_results <- group_results[group_results[[g]] == group_values[[g]], ]
      }

      if (nrow(group_results) > 0) {
        .print_gof_table(group_results, digits)
      }
    }
  } else {
    # Print frequency tables if available
    if (!is.null(x$frequencies)) {
      if (is.data.frame(x$frequencies)) {
        # Single variable case
        cat(sprintf("  %s - Frequency Table:\n", x$variables[1]))
        output <- capture.output(print(x$frequencies, row.names = FALSE))
        border <- paste(rep("-", max(nchar(output))), collapse = "")
        cat("  ", border, "\n", sep = "")
        for (line in output) cat("  ", line, "\n", sep = "")
        cat("  ", border, "\n\n", sep = "")
      } else {
        # Multi-variable case (named list)
        for (vn in x$variables) {
          freq <- x$frequencies[[vn]]
          if (!is.null(freq)) {
            cat(sprintf("  %s - Frequency Table:\n", vn))
            output <- capture.output(print(freq, row.names = FALSE))
            border <- paste(rep("-", max(nchar(output))), collapse = "")
            cat("  ", border, "\n", sep = "")
            for (line in output) cat("  ", line, "\n", sep = "")
            cat("  ", border, "\n\n", sep = "")
          }
        }
      }
    }

    # Print test statistics
    .print_gof_table(x$results, digits)
  }

  print_significance_legend()
  invisible(x)
}

#' @keywords internal
.print_gof_table <- function(results_df, digits = 4) {
  display <- data.frame(
    Variable = results_df$Variable,
    `Chi-Square` = round(results_df$chi_sq, 3),
    df = as.integer(results_df$df),
    `p-value` = ifelse(results_df$p_value < 0.001, "<.001",
                       format(round(results_df$p_value, digits),
                              nsmall = digits)),
    N = results_df$n,
    Sig = sapply(results_df$p_value, add_significance_stars),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  output <- capture.output(print(display, row.names = FALSE))
  border <- paste(rep("-", max(nchar(output))), collapse = "")
  cat(border, "\n")
  for (line in output) cat(line, "\n")
  cat(border, "\n\n")
}
