
#' McNemar's Test for Paired Proportions
#'
#' @description
#' \code{mcnemar_test()} tests whether paired proportions have changed between
#' two dichotomous measurements. Use this for before/after comparisons of
#' categorical outcomes.
#'
#' Think of it as:
#' - A paired comparison test for binary (yes/no) data
#' - The categorical equivalent of a paired t-test
#' - Tests whether the proportion of "changers" is symmetric
#'
#' The test tells you:
#' - Whether paired proportions changed significantly
#' - Both asymptotic and exact p-values for maximum reliability
#' - The number of discordant pairs (who actually changed)
#'
#' @param data Your survey data (data frame or tibble)
#' @param var1 First dichotomous variable (0/1 or two-level factor)
#' @param var2 Second dichotomous variable (0/1 or two-level factor)
#' @param weights Optional survey weights for population-representative results
#' @param correct Logical, whether to apply continuity correction (default: TRUE)
#' @param ... Additional arguments (currently unused)
#'
#' @return Test results showing whether paired proportions changed, including:
#' - McNemar chi-square statistic (with continuity correction)
#' - Asymptotic p-value
#' - Exact binomial p-value (two-sided)
#' - 2x2 contingency table
#' - Discordant pair counts (b and c)
#'
#' @details
#' ## Understanding the Results
#'
#' **P-value**: If p < 0.05, the paired proportions are significantly different
#' - p < 0.001: Very strong evidence of change
#' - p < 0.01: Strong evidence of change
#' - p < 0.05: Moderate evidence of change
#' - p >= 0.05: No significant change found
#'
#' **Exact vs Asymptotic**: The exact binomial p-value is more reliable for
#' small samples. For large samples, both p-values will be very similar.
#'
#' **Discordant Pairs**: Only pairs where the two measurements differ (b and c)
#' contribute to the test. If b approximately equals c, there is no evidence of
#' systematic change.
#'
#' ## When to Use This
#'
#' Use McNemar's test when:
#' - You have paired or matched binary data
#' - You're comparing before/after proportions
#' - Both variables must be dichotomous (exactly 2 levels)
#'
#' ## The McNemar Statistic
#'
#' For a 2x2 table with discordant cells b and c:
#' \deqn{\chi^2 = \frac{(|b - c| - 1)^2}{b + c}}
#' (with continuity correction)
#'
#' The exact test uses a binomial test on the discordant pairs.
#'
#' ## Relationship to Other Tests
#'
#' - For unpaired categorical data:
#'   Use \code{\link{chi_square}()} instead
#' - For paired ordinal/continuous data:
#'   Use \code{\link{wilcoxon_test}()} instead
#' - For paired data with more than 2 levels:
#'   Consider the Bowker test of symmetry
#'
#' ## SPSS Equivalent
#'
#' SPSS: \code{CROSSTABS /STATISTICS=MCNEMAR}
#'
#' @seealso
#' \code{\link{chi_square}} for independence tests.
#'
#' \code{\link{wilcoxon_test}} for paired non-parametric tests on ordinal data.
#'
#' @references
#' McNemar, Q. (1947). Note on the sampling error of the difference between
#' correlated proportions or percentages. \emph{Psychometrika}, 12(2), 153-157.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Create dichotomous variables
#' test_data <- survey_data %>%
#'   mutate(
#'     trust_gov_high = as.integer(trust_government >= 4),
#'     trust_media_high = as.integer(trust_media >= 4)
#'   )
#'
#' # McNemar test
#' test_data %>%
#'   mcnemar_test(var1 = trust_gov_high, var2 = trust_media_high)
#'
#' # Grouped analysis
#' test_data %>%
#'   group_by(region) %>%
#'   mcnemar_test(var1 = trust_gov_high, var2 = trust_media_high)
#'
#' @family hypothesis_tests
#' @export
mcnemar_test <- function(data, var1, var2, weights = NULL,
                         correct = TRUE, ...) {

  # Input validation
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }

  # Check if data is grouped
  is_grouped <- inherits(data, "grouped_df")
  grp_vars <- if (is_grouped) dplyr::group_vars(data) else NULL

  # Get variable names
  var1_name <- rlang::as_name(rlang::enquo(var1))
  var2_name <- rlang::as_name(rlang::enquo(var2))

  if (!var1_name %in% names(data)) {
    cli_abort("Variable {.var {var1_name}} not found in data.")
  }
  if (!var2_name %in% names(data)) {
    cli_abort("Variable {.var {var2_name}} not found in data.")
  }

  # Validate dichotomous
  v1_unique <- length(unique(na.omit(data[[var1_name]])))
  v2_unique <- length(unique(na.omit(data[[var2_name]])))
  if (v1_unique > 2) {
    cli_abort(c(
      "{.arg var1} must be dichotomous (exactly 2 levels).",
      "x" = "{.var {var1_name}} has {v1_unique} unique values."
    ))
  }
  if (v2_unique > 2) {
    cli_abort(c(
      "{.arg var2} must be dichotomous (exactly 2 levels).",
      "x" = "{.var {var2_name}} has {v2_unique} unique values."
    ))
  }

  # Process weights
  weights_info <- .process_weights(data, rlang::enquo(weights))
  w_name <- weights_info$name

  # Helper to perform McNemar test on a single data slice
  perform_single_mcnemar <- function(data_slice) {
    v1 <- data_slice[[var1_name]]
    v2 <- data_slice[[var2_name]]

    # Remove NAs
    valid <- !is.na(v1) & !is.na(v2)
    if (!is.null(w_name)) {
      w <- data_slice[[w_name]]
      valid <- valid & !is.na(w)
      w <- w[valid]
    }
    v1 <- v1[valid]
    v2 <- v2[valid]

    # Build 2x2 table
    if (!is.null(w_name)) {
      tbl <- xtabs(w ~ v1 + v2)
      tbl <- round(tbl)
    } else {
      tbl <- table(v1, v2)
    }

    n <- sum(tbl)

    # Discordant cells: b = tbl[1,2], c = tbl[2,1]
    b <- tbl[1, 2]
    c_val <- tbl[2, 1]

    # McNemar chi-square (with continuity correction)
    if ((b + c_val) == 0) {
      chi_sq <- NaN
      p_value <- NaN
      exact_p <- 1.0
    } else {
      if (correct) {
        chi_sq <- (abs(b - c_val) - 1)^2 / (b + c_val)
      } else {
        chi_sq <- (b - c_val)^2 / (b + c_val)
      }
      p_value <- pchisq(chi_sq, df = 1, lower.tail = FALSE)

      # Exact binomial test (2-sided)
      exact_result <- binom.test(b, b + c_val, p = 0.5)
      exact_p <- exact_result$p.value
    }

    list(
      statistic = chi_sq,
      p_value = p_value,
      exact_p = exact_p,
      n = n,
      table = tbl,
      b = b,
      c = c_val
    )
  }

  # Main execution
  if (is_grouped) {
    data_list <- dplyr::group_split(data)
    group_keys_df <- dplyr::group_keys(data)

    results_list <- lapply(seq_along(data_list), function(i) {
      tryCatch({
        res <- perform_single_mcnemar(data_list[[i]])
        cbind(
          group_keys_df[i, , drop = FALSE],
          data.frame(
            statistic = res$statistic,
            p_value = res$p_value,
            exact_p = res$exact_p,
            n = res$n,
            b = res$b,
            c = res$c,
            stringsAsFactors = FALSE
          )
        )
      }, error = function(e) {
        cbind(
          group_keys_df[i, , drop = FALSE],
          data.frame(
            statistic = NA_real_,
            p_value = NA_real_,
            exact_p = NA_real_,
            n = NA_integer_,
            b = NA_integer_,
            c = NA_integer_,
            stringsAsFactors = FALSE
          )
        )
      })
    })

    results_df <- do.call(rbind, results_list)
    rownames(results_df) <- NULL

    result <- list(
      results = results_df,
      statistic = results_df$statistic[1],
      p_value = results_df$p_value[1],
      exact_p = results_df$exact_p[1],
      n = results_df$n[1],
      table = NULL,
      b = results_df$b[1],
      c = results_df$c[1],
      var1_name = var1_name,
      var2_name = var2_name,
      weights = w_name,
      is_grouped = TRUE,
      groups = grp_vars
    )

  } else {
    res <- perform_single_mcnemar(data)

    results_df <- data.frame(
      statistic = res$statistic,
      p_value = res$p_value,
      exact_p = res$exact_p,
      n = res$n,
      b = res$b,
      c = res$c,
      stringsAsFactors = FALSE
    )

    result <- list(
      results = results_df,
      statistic = res$statistic,
      p_value = res$p_value,
      exact_p = res$exact_p,
      n = res$n,
      table = res$table,
      b = res$b,
      c = res$c,
      var1_name = var1_name,
      var2_name = var2_name,
      weights = w_name,
      is_grouped = FALSE,
      groups = NULL
    )
  }

  class(result) <- "mcnemar_test"
  return(result)
}

#' Print McNemar test results
#'
#' @param x An object of class \code{"mcnemar_test"}
#' @param digits Number of decimal places (default: 4)
#' @param ... Additional arguments (currently unused)
#' @return Invisibly returns the input object \code{x}.
#' @export
print.mcnemar_test <- function(x, digits = 4, ...) {
  weights_name <- x$weights
  test_type <- get_standard_title("McNemar Test", weights_name, "Results")
  print_header(test_type, newline_before = FALSE)

  cat("\n")
  test_info <- list(
    "Variable 1" = x$var1_name,
    "Variable 2" = x$var2_name,
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
        sig <- add_significance_stars(group_results$exact_p[1])

        display <- data.frame(
          `Chi-Sq` = round(group_results$statistic[1], 3),
          `p (asymp)` = ifelse(group_results$p_value[1] < 0.001, "<.001",
                               format(round(group_results$p_value[1], digits),
                                      nsmall = digits)),
          `p (exact)` = ifelse(group_results$exact_p[1] < 0.001, "<.001",
                               format(round(group_results$exact_p[1], digits),
                                      nsmall = digits)),
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
    # Print 2x2 table if available
    if (!is.null(x$table)) {
      cat("2x2 Contingency Table:\n")
      border <- paste(rep("-", 40), collapse = "")
      cat(border, "\n")
      print(x$table)
      cat(border, "\n\n")
    }

    sig <- add_significance_stars(x$exact_p)

    cat("Test Results:\n")
    display <- data.frame(
      `Chi-Sq (cc)` = round(x$statistic, 3),
      `p (asymp)` = ifelse(x$p_value < 0.001, "<.001",
                           format(round(x$p_value, digits), nsmall = digits)),
      `p (exact)` = ifelse(x$exact_p < 0.001, "<.001",
                           format(round(x$exact_p, digits), nsmall = digits)),
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

    cat(sprintf("\nDiscordant pairs: b = %d, c = %d\n", x$b, x$c))
  }

  print_significance_legend()
  invisible(x)
}
