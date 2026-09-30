
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
#' @param simulate.p.value Compute the p-value by Monte Carlo simulation
#'   instead of exactly? (Default: FALSE). For large tables (many rows and
#'   columns, large N) the exact network algorithm can run out of memory
#'   ("FEXACT error"); \code{fisher_test()} then switches to the Monte Carlo
#'   p-value automatically with a warning. SPSS offers the same choice
#'   (Exact Tests: "Exact" or "Monte Carlo"). Call \code{set.seed()} first
#'   for reproducible Monte Carlo results.
#' @param B Number of Monte Carlo replicates (Default: 10000, the SPSS
#'   default number of samples).
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
fisher_test <- function(data, row, col, weights = NULL,
                        simulate.p.value = FALSE, B = 10000, ...) {
  .reject_partial_args()
  .check_dots_unused(...)

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
  data <- weights_info$data
  w_name <- weights_info$name

  # Helper to perform Fisher test on a single data slice
  perform_single_fisher <- function(data_slice, key = NULL) {
    w <- if (!is.null(w_name)) data_slice[[w_name]] else NULL
    tbl <- .np_crosstab(data_slice[[row_name]], data_slice[[col_name]], w,
                        c(row_name, col_name))
    if (nrow(tbl) < 2 || ncol(tbl) < 2) {
      one <- c(row_name, col_name)[c(nrow(tbl) < 2, ncol(tbl) < 2)]
      cli_abort("{.var {one}} ha{?s/ve} fewer than 2 observed categories")
    }

    # Perform Fisher's exact test (Monte Carlo fallback for large tables)
    ft <- .fisher_exact_or_mc(tbl, simulate.p.value, B, key)

    c(list(p_value = ft$p.value, n = sum(tbl), table = tbl,
           method = gsub("\\s+", " ", ft$method)),
      .odds_ratio_2x2(tbl))
  }

  result_row <- function(res) {
    data.frame(
      p_value = res$p_value,
      n = res$n,
      method = res$method,
      odds_ratio = res$odds_ratio,
      or_ci_lower = res$or_ci_lower,
      or_ci_upper = res$or_ci_upper,
      reason = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  # Main execution
  if (is_grouped) {
    data_list <- dplyr::group_split(data)
    group_keys_df <- dplyr::group_keys(data)
    tables <- vector("list", length(data_list))

    results_list <- lapply(seq_along(data_list), function(i) {
      key <- group_keys_df[i, , drop = FALSE]
      tryCatch({
        res <- perform_single_fisher(data_list[[i]], key)
        tables[[i]] <<- res$table
        cbind(key, result_row(res))
      }, error = function(e) {
        where <- .np_where(key)
        reason <- .np_error_reason(e)
        cli_warn(c(
          "Fisher's exact test skipped{where}.",
          "x" = "{reason}."
        ))
        cbind(key, data.frame(
          p_value = NA_real_, n = NA_real_, method = NA_character_,
          odds_ratio = NA_real_, or_ci_lower = NA_real_,
          or_ci_upper = NA_real_, reason = reason,
          stringsAsFactors = FALSE
        ))
      })
    })

    results_df <- do.call(rbind, results_list)
    rownames(results_df) <- NULL

    result <- list(
      results = results_df,
      p_value = results_df$p_value[1],
      n = results_df$n[1],
      table = NULL,
      tables = tables,
      method = results_df$method[1],
      row_var = row_name,
      col_var = col_name,
      weights = w_name,
      is_grouped = TRUE,
      groups = grp_vars
    )

  } else {
    res <- perform_single_fisher(data)
    results_df <- result_row(res)

    result <- list(
      results = results_df,
      p_value = res$p_value,
      n = res$n,
      table = res$table,
      tables = list(res$table),
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

#' Print Fisher's exact test results (compact)
#'
#' @description
#' Compact print method for objects of class \code{"fisher_test"}.
#' Shows a one-line summary with the exact p-value, significance stars,
#' the odds ratio for 2x2 tables, and sample size.
#'
#' For the full detailed output (contingency table, test results), use
#' \code{summary()}.
#'
#' @param x An object of class \code{"fisher_test"}
#' @param digits Number of decimal places (default: 3)
#' @param ... Additional arguments (currently unused)
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- fisher_test(survey_data, row = gender, col = region)
#' result              # compact one-line overview
#' summary(result)     # full detailed output
#'
#' @export
print.fisher_test <- function(x, digits = 3, ...) {
  weighted_tag <- if (!is.null(x$weights)) " [Weighted]" else ""
  pair_label <- paste(x$row_var, "x", x$col_var)

  for_each_group(x$results, if (isTRUE(x$is_grouped)) x$groups, function(rows, key) {
    if (!is.null(key)) cat(sprintf("[%s]\n", .format_group_label(key)))
    cat(sprintf("Fisher's Exact Test: %s%s\n", pair_label, weighted_tag))
    .print_fisher_compact(rows, 1, digits, grouped = isTRUE(x$is_grouped))
  }, header = FALSE)

  cat("Use summary() for detailed output.\n")
  invisible(x)
}

#' One compact Fisher line
#' @noRd
.print_fisher_compact <- function(results, i, digits, grouped = FALSE) {
  p_val <- results$p_value[i]
  if (is.na(p_val)) {
    cat(sprintf("  %s\n", .np_not_computed(results, i, grouped)))
    return(invisible(NULL))
  }
  mc <- if (!is.na(results$method[i]) && grepl("simulated", results$method[i]))
    " (Monte Carlo)" else ""
  or_part <- ""
  if (!is.null(results$odds_ratio) && !is.na(results$odds_ratio[i])) {
    or_part <- sprintf(", OR = %s", fmt_num(results$odds_ratio[i], digits))
    if (!is.na(results$or_ci_lower[i])) {
      or_part <- sprintf("%s [%s, %s]", or_part,
                         fmt_num(results$or_ci_lower[i], digits),
                         fmt_num(results$or_ci_upper[i], digits))
    }
  }
  cat(sprintf("  %s%s%s, N = %s\n", format_p_stars(p_val, digits), mc, or_part,
              fmt_int(results$n[i])))
}

#' Summary method for Fisher's exact test results
#'
#' @description
#' Creates a summary object that produces detailed output when printed,
#' including the contingency table of observed frequencies and the test
#' results table with method, exact p-value, sample size, and (for 2x2
#' tables) the odds ratio with its 95\% confidence interval.
#'
#' @param object A \code{fisher_test} result object.
#' @param contingency_table Logical. Show the contingency table?
#'   (Default: TRUE)
#' @param results Logical. Show the test results table? (Default: TRUE)
#' @param digits Number of decimal places for formatting (Default: 3).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.fisher_test} object.
#'
#' @examples
#' result <- fisher_test(survey_data, row = gender, col = region)
#' summary(result)
#' summary(result, contingency_table = FALSE)
#'
#' @seealso \code{\link{fisher_test}} for the main analysis function.
#' @export
#' @method summary fisher_test
summary.fisher_test <- function(object, contingency_table = TRUE,
                                results = TRUE, digits = 3, ...) {
  build_summary_object(
    object     = object,
    show       = list(contingency_table = contingency_table,
                      results = results),
    digits     = digits,
    class_name = "summary.fisher_test"
  )
}

#' Print summary of Fisher's exact test results (detailed output)
#'
#' @description
#' Displays the detailed output for Fisher's exact test, with sections
#' controlled by the boolean parameters passed to
#' \code{\link{summary.fisher_test}}.  Sections include the contingency
#' table and the test results table.
#'
#' @param x A \code{summary.fisher_test} object created by
#'   \code{\link{summary.fisher_test}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- fisher_test(survey_data, row = gender, col = region)
#' summary(result)                            # all sections
#' summary(result, contingency_table = FALSE) # hide contingency table
#'
#' @seealso \code{\link{fisher_test}} for the main analysis,
#'   \code{\link{summary.fisher_test}} for summary options.
#' @export
#' @method print summary.fisher_test
print.summary.fisher_test <- function(x, ...) {
  digits <- x$digits
  weights_name <- x$weights
  test_type <- get_standard_title("Fisher's Exact Test", weights_name, "Results")
  print_header(test_type, newline_before = FALSE)

  # Resolve show toggles
  show_table   <- isTRUE(x$show$contingency_table)
  show_results <- isTRUE(x$show$results)

  cat("\n")
  test_info <- list(
    "Row variable" = x$row_var,
    "Column variable" = x$col_var,
    "Weights variable" = x$weights
  )
  print_info_section(test_info)

  tables <- x$tables
  if (is.null(tables)) tables <- list(x$table)

  for (i in seq_len(nrow(x$results))) {
    if (isTRUE(x$is_grouped)) {
      print_group_header(x$results[i, x$groups, drop = FALSE])
    }
    .print_fisher_block(x$results, i, tables[[i]], digits, show_table,
                        show_results, grouped = isTRUE(x$is_grouped))
  }

  if (show_results) {
    print_significance_legend()
  }
  invisible(x)
}

#' Contingency table and results of one Fisher test
#' @noRd
.print_fisher_block <- function(results, i, tbl, digits, show_table,
                                show_results, grouped = FALSE) {
  if (is.na(results$p_value[i])) {
    txt <- .np_not_computed(results, i, grouped)
    cat(sprintf("\n%s%s.\n", toupper(substr(txt, 1, 1)), substring(txt, 2)))
    return(invisible(NULL))
  }

  if (show_table && !is.null(tbl)) {
    cat("\nContingency Table:\n")
    .print_chi_matrix(tbl, digits = 0)
  }

  if (show_results) {
    cat("\nTest Results:\n")
    display <- data.frame(
      Method = results$method[i],
      p = results$p_value[i],
      stars = add_significance_stars(results$p_value[i]),
      N = round(results$n[i]),
      stringsAsFactors = FALSE
    )
    labels <- c(p = "p value", stars = "")
    if (!is.null(results$odds_ratio) && !is.na(results$odds_ratio[i])) {
      display$OR <- results$odds_ratio[i]
      if (!is.na(results$or_ci_lower[i])) {
        display$CI <- sprintf("[%s, %s]", fmt_num(results$or_ci_lower[i], digits),
                              fmt_num(results$or_ci_upper[i], digits))
        labels <- c(labels, CI = "95% CI (OR)")
      }
    }
    print_stat_table(display, digits = digits, indent = 0,
                     col_types = c(N = "int", OR = "num"),
                     col_labels = labels)
  }
  invisible(NULL)
}

#' Fisher's exact test with a Monte Carlo fallback
#'
#' The FEXACT network algorithm fails on larger tables ("FEXACT error 501:
#' the hash table key cannot be computed", "LDKEY is too small", ...). SPSS
#' offers a Monte Carlo p-value for that case; so does fisher.test(). The
#' fallback is announced with a warning naming the group.
#'
#' @param tbl Contingency table
#' @param simulate Use the Monte Carlo p-value from the start?
#' @param B Monte Carlo replicates
#' @param key One-row group key or NULL (for the warning)
#' @return htest object
#' @noRd
.fisher_exact_or_mc <- function(tbl, simulate, B, key = NULL) {
  if (isTRUE(simulate)) {
    return(stats::fisher.test(tbl, simulate.p.value = TRUE, B = B))
  }
  tryCatch(
    stats::fisher.test(tbl, workspace = 2e7),
    error = function(e) {
      msg <- conditionMessage(e)
      if (!grepl("FEXACT|LDKEY|LDSTP|workspace", msg)) stop(e)
      where <- .np_where(key)
      cli_warn(c(
        "Exact p-value not computable for this {nrow(tbl)}x{ncol(tbl)} table{where}; using a Monte Carlo p-value ({B} replicates).",
        "i" = "SPSS offers the same Monte Carlo option; use {.code set.seed()} for reproducible results or {.code simulate.p.value = TRUE} to choose it directly."
      ))
      stats::fisher.test(tbl, simulate.p.value = TRUE, B = B)
    }
  )
}

#' Sample odds ratio of a 2x2 table with Woolf 95% CI (SPSS Risk Estimate)
#'
#' @param tbl Contingency table
#' @return list(odds_ratio, or_ci_lower, or_ci_upper); NA unless 2x2 (CI
#'   also NA when a cell is 0)
#' @noRd
.odds_ratio_2x2 <- function(tbl) {
  out <- list(odds_ratio = NA_real_, or_ci_lower = NA_real_,
              or_ci_upper = NA_real_)
  if (!identical(dim(tbl), c(2L, 2L))) return(out)
  n <- as.numeric(tbl)
  a <- n[1]; c <- n[2]; b <- n[3]; d <- n[4]
  if (b * c == 0) return(out)
  or <- (a * d) / (b * c)
  out$odds_ratio <- or
  if (all(n > 0)) {
    se <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
    out$or_ci_lower <- exp(log(or) - stats::qnorm(0.975) * se)
    out$or_ci_upper <- exp(log(or) + stats::qnorm(0.975) * se)
  }
  out
}
