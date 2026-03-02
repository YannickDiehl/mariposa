
#' Fisher's Exact Test for Small Samples
#'
#' @description
#' \code{fisher_test()} performs Fisher's exact test for independence in a
#' contingency table. Use this instead of \code{\link{chi_square}()} when your
#' sample is small or when expected cell frequencies are below 5.
#'
#' Think of it as:
#' - An exact alternative to the chi-square test of independence
#' - Best for small samples where the chi-square approximation may be inaccurate
#' - SPSS automatically reports Fisher's Exact Test for 2x2 tables
#'
#' The test tells you:
#' - Whether two categorical variables are related (exact p-value)
#' - The contingency table of observed frequencies
#' - Results that are valid even with very small samples
#'
#' @param data Your survey data (data frame or tibble)
#' @param row The row variable (categorical)
#' @param col The column variable (categorical)
#' @param weights Optional survey weights for population-representative results
#' @param ... Additional arguments (currently unused)
#'
#' @return Test results showing whether two categorical variables are related,
#'   including:
#' - Exact p-value (two-sided)
#' - Contingency table of observed frequencies
#' - Sample size (N)
#'
#' @details
#' ## Understanding the Results
#'
#' **P-value**: If p < 0.05, the variables are likely related (not independent)
#' - p < 0.001: Very strong evidence of relationship
#' - p < 0.01: Strong evidence of relationship
#' - p < 0.05: Moderate evidence of relationship
#' - p >= 0.05: No significant relationship found
#'
#' Unlike the chi-square test which uses a large-sample approximation,
#' Fisher's exact test computes the exact probability of observing the
#' given table (or a more extreme one) under the null hypothesis of
#' independence.
#'
#' For tables larger than 2x2, the Fisher-Freeman-Halton extension is used.
#'
#' ## When to Use This
#'
#' Use Fisher's exact test when:
#' - Any expected cell frequency is less than 5
#' - Your total sample size is small (typically N < 30)
#' - You have a 2x2 table (Fisher's exact is standard here)
#' - You want an exact p-value rather than an approximation
#'
#' ## Relationship to Other Tests
#'
#' - For large samples with all expected frequencies >= 5:
#'   Use \code{\link{chi_square}()} instead
#' - For paired binary data (before/after):
#'   Use \code{\link{mcnemar_test}()} instead
#' - For testing a single proportion:
#'   Use \code{\link{binomial_test}()} instead
#'
#' ## SPSS Equivalent
#'
#' SPSS: \code{CROSSTABS /STATISTICS=CHISQ} (Fisher's exact test is
#' automatically reported for 2x2 tables)
#'
#' @seealso
#' \code{\link{chi_square}} for chi-square test of independence (large samples).
#'
#' \code{\link{mcnemar_test}} for paired proportions.
#'
#' @references
#' Fisher, R. A. (1922). On the interpretation of chi-square from contingency
#' tables, and the calculation of P. \emph{Journal of the Royal Statistical
#' Society}, 85(1), 87-94.
#'
#' Freeman, G. H., & Halton, J. H. (1951). Note on an exact treatment of
#' contingency, goodness of fit and other problems of significance.
#' \emph{Biometrika}, 38(1/2), 141-149.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Basic 2x2 Fisher test
#' survey_data %>%
#'   fisher_test(row = gender, col = region)
#'
#' # Fisher test for larger table
#' survey_data %>%
#'   fisher_test(row = gender, col = interview_mode)
#'
#' # With weights
#' survey_data %>%
#'   fisher_test(row = gender, col = region, weights = sampling_weight)
#'
#' # Grouped analysis
#' survey_data %>%
#'   group_by(education) %>%
#'   fisher_test(row = gender, col = region)
#'
#' @family hypothesis_tests
#' @export
fisher_test <- function(data, row, col, weights = NULL, ...) {

  # Input validation
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }

  # Check if data is grouped
  is_grouped <- inherits(data, "grouped_df")
  grp_vars <- if (is_grouped) dplyr::group_vars(data) else NULL

  # Get variable names
  row_name <- rlang::as_name(rlang::enquo(row))
  col_name <- rlang::as_name(rlang::enquo(col))

  if (!row_name %in% names(data)) {
    cli_abort("Row variable {.var {row_name}} not found in data.")
  }
  if (!col_name %in% names(data)) {
    cli_abort("Column variable {.var {col_name}} not found in data.")
  }

  # Validate that variables are categorical
  row_vals <- data[[row_name]]
  col_vals <- data[[col_name]]
  if (is.numeric(row_vals) && length(unique(na.omit(row_vals))) > 20) {
    cli_abort("{.arg row} variable {.var {row_name}} appears to be continuous. Fisher's exact test requires categorical variables.")
  }
  if (is.numeric(col_vals) && length(unique(na.omit(col_vals))) > 20) {
    cli_abort("{.arg col} variable {.var {col_name}} appears to be continuous. Fisher's exact test requires categorical variables.")
  }

  # Process weights
  weights_info <- .process_weights(data, rlang::enquo(weights))
  w_name <- weights_info$name

  # Helper to perform Fisher test on a single data slice
  perform_single_fisher <- function(data_slice) {
    r <- data_slice[[row_name]]
    cc <- data_slice[[col_name]]

    # Remove NAs
    valid <- !is.na(r) & !is.na(cc)
    if (!is.null(w_name)) {
      w <- data_slice[[w_name]]
      valid <- valid & !is.na(w)
      w <- w[valid]
    }
    r <- r[valid]
    cc <- cc[valid]

    # Build contingency table
    if (!is.null(w_name)) {
      tbl <- xtabs(w ~ r + cc)
      tbl <- round(tbl)
    } else {
      tbl <- table(r, cc)
    }

    n <- sum(tbl)

    # Perform Fisher's exact test
    ft <- fisher.test(tbl, workspace = 2e7)

    list(
      p_value = ft$p.value,
      n = n,
      table = tbl,
      method = ft$method
    )
  }

  # Main execution
  if (is_grouped) {
    data_list <- dplyr::group_split(data)
    group_keys_df <- dplyr::group_keys(data)

    results_list <- lapply(seq_along(data_list), function(i) {
      tryCatch({
        res <- perform_single_fisher(data_list[[i]])
        cbind(
          group_keys_df[i, , drop = FALSE],
          data.frame(
            p_value = res$p_value,
            n = res$n,
            method = res$method,
            stringsAsFactors = FALSE
          )
        )
      }, error = function(e) {
        cbind(
          group_keys_df[i, , drop = FALSE],
          data.frame(
            p_value = NA_real_,
            n = NA_integer_,
            method = NA_character_,
            stringsAsFactors = FALSE
          )
        )
      })
    })

    results_df <- do.call(rbind, results_list)
    rownames(results_df) <- NULL

    result <- list(
      results = results_df,
      p_value = results_df$p_value[1],
      n = results_df$n[1],
      table = NULL,
      method = results_df$method[1],
      row_var = row_name,
      col_var = col_name,
      weights = w_name,
      is_grouped = TRUE,
      groups = grp_vars
    )

  } else {
    res <- perform_single_fisher(data)

    results_df <- data.frame(
      p_value = res$p_value,
      n = res$n,
      method = res$method,
      stringsAsFactors = FALSE
    )

    result <- list(
      results = results_df,
      p_value = res$p_value,
      n = res$n,
      table = res$table,
      method = res$method,
      row_var = row_name,
      col_var = col_name,
      weights = w_name,
      is_grouped = FALSE,
      groups = NULL
    )
  }

  class(result) <- "fisher_test"
  return(result)
}

#' Print Fisher's exact test results
#'
#' @param x An object of class \code{"fisher_test"}
#' @param digits Number of decimal places (default: 4)
#' @param ... Additional arguments (currently unused)
#' @return Invisibly returns the input object \code{x}.
#' @export
print.fisher_test <- function(x, digits = 4, ...) {
  weights_name <- x$weights
  test_type <- get_standard_title("Fisher's Exact Test", weights_name, "Results")
  print_header(test_type, newline_before = FALSE)

  cat("\n")
  test_info <- list(
    "Row variable" = x$row_var,
    "Column variable" = x$col_var,
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
        p_val <- group_results$p_value[1]
        sig <- add_significance_stars(p_val)

        display <- data.frame(
          `p-value` = ifelse(p_val < 0.001, "<.001",
                             format(round(p_val, digits), nsmall = digits)),
          N = group_results$n[1],
          Sig = as.character(sig),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )

        output <- capture.output(print(display, row.names = FALSE))
        border <- paste(rep("-", max(nchar(output))), collapse = "")
        cat(border, "\n")
        for (line in output) cat(line, "\n")
        cat(border, "\n\n")
      }
    }
  } else {
    # Print contingency table if available
    if (!is.null(x$table)) {
      cat("Contingency Table:\n")
      border <- paste(rep("-", 40), collapse = "")
      cat(border, "\n")
      print(x$table)
      cat(border, "\n\n")
    }

    p_val <- x$p_value
    sig <- add_significance_stars(p_val)

    cat("Test Results:\n")
    display <- data.frame(
      Method = x$method,
      `p-value` = ifelse(p_val < 0.001, "<.001",
                         format(round(p_val, digits), nsmall = digits)),
      N = x$n,
      Sig = as.character(sig),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    output <- capture.output(print(display, row.names = FALSE))
    border <- paste(rep("-", max(nchar(output))), collapse = "")
    cat(border, "\n")
    for (line in output) cat(line, "\n")
    cat(border, "\n")
  }

  print_significance_legend()
  invisible(x)
}
