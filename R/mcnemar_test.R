
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
#' - McNemar chi-square statistic (\code{chi_squared}, with continuity
#'   correction)
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
  data <- weights_info$data
  w_name <- weights_info$name

  # Both variables must use the same two categories (SPSS builds a square
  # table from them); {0,1} against {1,2} used to be tabulated as if the
  # categories matched.
  l1 <- levels(.np_factor(data[[var1_name]]))
  l2 <- levels(.np_factor(data[[var2_name]]))
  cats <- union(l1, l2)
  if (length(cats) > 2) {
    cli_abort(c(
      "{.var {var1_name}} and {.var {var2_name}} must share the same two categories.",
      "x" = "{.var {var1_name}}: {.val {l1}}; {.var {var2_name}}: {.val {l2}}."
    ))
  }
  # category order of the variable that shows both (SPSS: by code)
  if (length(l1) == 2) cats <- l1 else if (length(l2) == 2) cats <- l2

  # Helper to perform McNemar test on a single data slice
  perform_single_mcnemar <- function(data_slice) {
    f1 <- factor(as.character(.np_factor(data_slice[[var1_name]])), levels = cats)
    f2 <- factor(as.character(.np_factor(data_slice[[var2_name]])), levels = cats)

    # Remove NAs
    valid <- !is.na(f1) & !is.na(f2)
    if (!is.null(w_name)) {
      w <- data_slice[[w_name]]
      valid <- valid & !is.na(w)
      tbl <- round(tapply(w[valid], list(f1[valid], f2[valid]), sum))
      tbl[is.na(tbl)] <- 0
      tbl <- as.table(tbl)
    } else {
      tbl <- table(f1[valid], f2[valid])
    }
    names(dimnames(tbl)) <- c(var1_name, var2_name)

    n <- sum(tbl)
    if (n == 0) {
      cli_abort("no valid pairs of {.var {var1_name}} and {.var {var2_name}}")
    }

    # Discordant cells: b = tbl[1,2], c = tbl[2,1]
    if (length(cats) == 2) {
      b <- tbl[1, 2]
      c_val <- tbl[2, 1]
    } else {
      b <- 0
      c_val <- 0
    }

    reason <- NA_character_
    if ((b + c_val) == 0) {
      # Nothing changed: no evidence of change (exact binomial p = 1)
      chi_sq <- NA_real_
      p_value <- NA_real_
      exact_p <- 1.0
      reason <- "no discordant pairs"
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
      b = as.numeric(b),
      c = as.numeric(c_val),
      reason = reason
    )
  }

  result_row <- function(res) {
    data.frame(
      chi_squared = res$statistic,
      df = 1,
      p_value = res$p_value,
      exact_p = res$exact_p,
      n = res$n,
      b = res$b,
      c = res$c,
      reason = res$reason,
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
        res <- perform_single_mcnemar(data_list[[i]])
        tables[[i]] <<- res$table
        cbind(key, result_row(res))
      }, error = function(e) {
        where <- .np_where(key)
        reason <- .np_error_reason(e)
        cli_warn(c("McNemar test skipped{where}.", "x" = "{reason}."))
        cbind(key, data.frame(
          chi_squared = NA_real_, df = 1, p_value = NA_real_,
          exact_p = NA_real_, n = NA_real_, b = NA_real_, c = NA_real_,
          reason = reason, stringsAsFactors = FALSE
        ))
      })
    })

    results_df <- do.call(rbind, results_list)
    rownames(results_df) <- NULL

    result <- list(
      results = results_df,
      statistic = results_df$chi_squared[1],
      p_value = results_df$p_value[1],
      exact_p = results_df$exact_p[1],
      n = results_df$n[1],
      table = NULL,
      tables = tables,
      b = results_df$b[1],
      c = results_df$c[1],
      var1_name = var1_name,
      var2_name = var2_name,
      weights = w_name,
      correct = correct,
      is_grouped = TRUE,
      groups = grp_vars
    )

  } else {
    res <- perform_single_mcnemar(data)
    results_df <- result_row(res)

    result <- list(
      results = results_df,
      statistic = res$statistic,
      p_value = res$p_value,
      exact_p = res$exact_p,
      n = res$n,
      table = res$table,
      tables = list(res$table),
      b = res$b,
      c = res$c,
      var1_name = var1_name,
      var2_name = var2_name,
      weights = w_name,
      correct = correct,
      is_grouped = FALSE,
      groups = NULL
    )
  }

  class(result) <- "mcnemar_test"
  return(result)
}

#' Print McNemar test results (compact)
#'
#' @description
#' Compact print method for objects of class \code{"mcnemar_test"}.
#' Shows a one-line summary with the McNemar chi-square statistic, the
#' asymptotic and exact p-values, and sample size.
#'
#' For the full detailed output (2x2 contingency table, test results,
#' discordant pairs), use \code{summary()}.
#'
#' @param x An object of class \code{"mcnemar_test"}
#' @param digits Number of decimal places (default: 3)
#' @param ... Additional arguments (currently unused)
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' test_data <- transform(survey_data,
#'   trust_gov_high = as.integer(trust_government >= 4),
#'   trust_media_high = as.integer(trust_media >= 4))
#' result <- mcnemar_test(test_data, var1 = trust_gov_high,
#'                        var2 = trust_media_high)
#' result              # compact one-line overview
#' summary(result)     # full detailed output
#'
#' @export
print.mcnemar_test <- function(x, digits = 3, ...) {
  weighted_tag <- if (!is.null(x$weights)) " [Weighted]" else ""
  pair_label <- paste(x$var1_name, "\u00d7", x$var2_name)
  correct <- !isFALSE(x$correct)

  for_each_group(x$results, if (isTRUE(x$is_grouped)) x$groups, function(rows, key) {
    if (!is.null(key)) cat(sprintf("[%s]\n", .format_group_label(key)))
    cat(sprintf("McNemar Test: %s%s\n", pair_label, weighted_tag))
    .print_mcnemar_compact(rows, 1, digits, correct,
                           grouped = isTRUE(x$is_grouped))
  }, header = FALSE)

  cat("Use summary() for detailed output.\n")
  invisible(x)
}

#' One compact McNemar line
#' @noRd
.print_mcnemar_compact <- function(results, i, digits, correct, grouped = FALSE) {
  if (is.na(results$exact_p[i])) {
    cat(sprintf("  %s\n", .np_not_computed(results, i, grouped)))
    return(invisible(NULL))
  }
  n_txt <- fmt_int(results$n[i])
  if (is.na(results$chi_squared[i])) {
    cat(sprintf("  chi2 not computed (%s), %s (exact), N = %s\n",
                .np_reason(results, i), format_p_stars(results$exact_p[i], digits),
                n_txt))
    return(invisible(NULL))
  }
  cat(sprintf("  chi2(1) = %s%s, %s (asymptotic), %s (exact), N = %s\n",
              fmt_num(results$chi_squared[i], digits),
              if (correct) " (cc)" else "",
              format_p_compact(results$p_value[i], digits),
              format_p_stars(results$exact_p[i], digits),
              n_txt))
}

#' Summary method for McNemar test results
#'
#' @description
#' Creates a summary object that produces detailed output when printed,
#' including the 2x2 contingency table, the test results table with
#' asymptotic and exact p-values, and the discordant pair counts.
#'
#' @param object A \code{mcnemar_test} result object.
#' @param contingency_table Logical. Show the 2x2 contingency table?
#'   (Default: TRUE)
#' @param results Logical. Show the test results table? (Default: TRUE)
#' @param discordant_pairs Logical. Show the discordant pair counts?
#'   (Default: TRUE)
#' @param digits Number of decimal places for formatting (Default: 3).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.mcnemar_test} object.
#'
#' @examples
#' test_data <- transform(survey_data,
#'   trust_gov_high = as.integer(trust_government >= 4),
#'   trust_media_high = as.integer(trust_media >= 4))
#' result <- mcnemar_test(test_data, var1 = trust_gov_high,
#'                        var2 = trust_media_high)
#' summary(result)
#' summary(result, contingency_table = FALSE)
#'
#' @seealso \code{\link{mcnemar_test}} for the main analysis function.
#' @export
#' @method summary mcnemar_test
summary.mcnemar_test <- function(object, contingency_table = TRUE,
                                 results = TRUE, discordant_pairs = TRUE,
                                 digits = 3, ...) {
  build_summary_object(
    object     = object,
    show       = list(contingency_table = contingency_table,
                      results = results,
                      discordant_pairs = discordant_pairs),
    digits     = digits,
    class_name = "summary.mcnemar_test"
  )
}

#' Print summary of McNemar test results (detailed output)
#'
#' @description
#' Displays the detailed output for a McNemar test, with sections
#' controlled by the boolean parameters passed to
#' \code{\link{summary.mcnemar_test}}.  Sections include the 2x2
#' contingency table, the test results table, and the discordant pairs.
#'
#' @param x A \code{summary.mcnemar_test} object created by
#'   \code{\link{summary.mcnemar_test}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' test_data <- transform(survey_data,
#'   trust_gov_high = as.integer(trust_government >= 4),
#'   trust_media_high = as.integer(trust_media >= 4))
#' result <- mcnemar_test(test_data, var1 = trust_gov_high,
#'                        var2 = trust_media_high)
#' summary(result)                            # all sections
#' summary(result, discordant_pairs = FALSE)  # hide discordant pairs
#'
#' @seealso \code{\link{mcnemar_test}} for the main analysis,
#'   \code{\link{summary.mcnemar_test}} for summary options.
#' @export
#' @method print summary.mcnemar_test
print.summary.mcnemar_test <- function(x, ...) {
  digits <- x$digits
  weights_name <- x$weights
  test_type <- get_standard_title("McNemar Test", weights_name, "Results")
  print_header(test_type, newline_before = FALSE)

  # Resolve show toggles
  show_table      <- isTRUE(x$show$contingency_table)
  show_results    <- isTRUE(x$show$results)
  show_discordant <- isTRUE(x$show$discordant_pairs)
  correct <- !isFALSE(x$correct)

  cat("\n")
  test_info <- list(
    "Variable 1" = x$var1_name,
    "Variable 2" = x$var2_name,
    "Weights variable" = x$weights,
    "Continuity correction" = if (correct) "yes (cc)" else "no"
  )
  print_info_section(test_info)

  tables <- x$tables
  if (is.null(tables)) tables <- list(x$table)

  for (i in seq_len(nrow(x$results))) {
    if (isTRUE(x$is_grouped)) {
      print_group_header(x$results[i, x$groups, drop = FALSE])
    }
    .print_mcnemar_block(x$results, i, tables[[i]], digits, correct,
                         show_table, show_results, show_discordant,
                         grouped = isTRUE(x$is_grouped))
  }

  if (show_results) {
    print_significance_legend()
  }
  invisible(x)
}

#' Table, test results and discordant pairs of one McNemar test
#' @noRd
.print_mcnemar_block <- function(results, i, tbl, digits, correct, show_table,
                                 show_results, show_discordant,
                                 grouped = FALSE) {
  if (is.na(results$exact_p[i])) {
    txt <- .np_not_computed(results, i, grouped)
    cat(sprintf("\n%s%s.\n", toupper(substr(txt, 1, 1)), substring(txt, 2)))
    return(invisible(NULL))
  }

  if (show_table && !is.null(tbl)) {
    cat("\n2x2 Contingency Table:\n")
    .print_chi_matrix(tbl, digits = 0)
  }

  if (show_results) {
    cat("\nTest Results:\n")
    display <- data.frame(
      test = "McNemar",
      chi = results$chi_squared[i],
      df = 1,
      p = results$p_value[i],
      p_exact = results$exact_p[i],
      stars = add_significance_stars(results$exact_p[i]),
      N = round(results$n[i]),
      stringsAsFactors = FALSE
    )
    print_stat_table(display, digits = digits, indent = 0,
                     col_types = c(chi = "num", df = "int", p = "pvalue",
                                   p_exact = "pvalue", N = "int"),
                     col_labels = c(test = "", chi = if (correct) "Chi-Sq (cc)" else "Chi-Sq",
                                    p = "p (asymp)", p_exact = "p (exact)",
                                    stars = ""))
    if (is.na(results$chi_squared[i])) {
      cat(sprintf("Chi-square not computed (%s); exact p = 1.\n",
                  .np_reason(results, i)))
    }
  }

  if (show_discordant) {
    cat(sprintf("\nDiscordant pairs: b = %s, c = %s\n",
                fmt_int(results$b[i]),
                fmt_int(results$c[i])))
  }
  invisible(NULL)
}
