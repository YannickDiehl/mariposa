
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
#' @param expected Optional numeric vector of expected proportions, one per
#'   category. If NULL (default), equal proportions are assumed (SPSS
#'   \code{/EXPECTED=EQUAL}). Applied to every selected variable, so each
#'   variable must have that many categories. A named vector is matched to
#'   the categories by name (category label, or the code of a labelled
#'   variable); an unnamed vector is used in category order. As in SPSS
#'   \code{/EXPECTED=50 30 20}, values may also be counts or any relative
#'   frequencies: they are divided by their sum. Proportions (all values
#'   below 1) must sum to 1; a sum within 0.01 of 1 (e.g. 0.995 from
#'   rounding) is rescaled with a message.
#' @param weights Optional survey weights for population-representative results
#'
#' @return Test results showing whether observed frequencies match expected,
#'   including:
#' - Chi-square statistic (\code{chi_squared}) and p-value for each variable
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
  .check_required("data")

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

  # Validate and normalise expected proportions (SPSS /EXPECTED semantics)
  expected <- .gof_normalize_expected(expected)

  # Process weights
  weights_info <- .process_weights(data, rlang::enquo(weights))
  data <- weights_info$data
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

  # Build the observed frequency table of one variable in one data slice
  gof_table <- function(data_slice, var_name) {
    vals <- data_slice[[var_name]]

    # Remove NAs
    valid <- !is.na(vals)
    if (!is.null(w_name)) {
      w <- data_slice[[w_name]]
      valid <- valid & !is.na(w)
      w <- w[valid]
    }
    raw <- vals[valid]
    # Observed categories only (SPSS): no phantom empty factor levels
    vals <- .np_factor(raw)

    if (!is.null(w_name)) {
      freq_tbl <- xtabs(w ~ vals)
      freq_tbl <- round(freq_tbl)
    } else {
      freq_tbl <- table(vals)
    }
    # A category whose rounded weighted count is 0 is not observed either
    keep <- as.numeric(freq_tbl) > 0
    codes <- .np_codes(raw, names(freq_tbl))
    freq_tbl <- freq_tbl[keep]
    attr(freq_tbl, "codes") <- codes[keep]
    freq_tbl
  }

  # Check `expected` against every variable's categories up front, so a
  # mismatch is a clear error instead of a silently skipped variable
  if (!is.null(expected)) {
    for (vn in var_names) {
      .gof_align_expected(expected, gof_table(data, vn), vn)
    }
  }

  # Helper to perform GoF test on a single variable in a single data slice
  perform_single_gof <- function(data_slice, var_name, expected_props = NULL,
                                 key = NULL) {
    freq_tbl <- gof_table(data_slice, var_name)

    n <- sum(freq_tbl)
    k <- length(freq_tbl)
    if (k < 2) {
      cli_abort("{.var {var_name}} has {k} observed categor{?y/ies}; at least 2 are needed.")
    }

    # Determine expected frequencies
    if (!is.null(expected_props)) {
      expected_freq <- n * .gof_align_expected(expected_props, freq_tbl,
                                               var_name)
    } else {
      # Equal proportions (SPSS default)
      expected_freq <- rep(n / k, k)
    }
    expected_freq <- unname(expected_freq)

    # SPSS footnotes cells with an expected frequency below 5
    n_low <- sum(expected_freq < 5)
    if (n_low > 0) {
      where <- .np_where(key)
      pct_low <- round(100 * n_low / k, 1)
      cli_warn(c(
        "{.var {var_name}}{where}: {n_low} categor{?y/ies} ({pct_low}%) ha{?s/ve} an expected count below 5.",
        "i" = "The chi-square approximation may be unreliable (minimum expected count {round(min(expected_freq), 1)})."
      ))
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

  na_row <- function(vn) {
    data.frame(
      Variable = vn,
      chi_squared = NA_real_,
      df = NA_integer_,
      p_value = NA_real_,
      n = NA_integer_,
      stringsAsFactors = FALSE
    )
  }

  # Main execution
  if (is_grouped) {
    data_list <- dplyr::group_split(data)
    group_keys_df <- dplyr::group_keys(data)

    results_list <- lapply(seq_along(data_list), function(i) {
      key <- group_keys_df[i, , drop = FALSE]
      var_results <- lapply(var_names, function(vn) {
        tryCatch({
          res <- perform_single_gof(data_list[[i]], vn, expected, key)
          cbind(
            key,
            data.frame(
              Variable = vn,
              chi_squared = res$chi_sq,
              df = res$df,
              p_value = res$p_value,
              n = res$n,
              stringsAsFactors = FALSE
            )
          )
        }, error = function(e) {
          where <- .np_where(key)
          cli_warn(c(
            "Chi-square goodness-of-fit test skipped for {.var {vn}}{where}.",
            "x" = "{conditionMessage(e)}"
          ))
          cbind(key, na_row(vn))
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
        res <- perform_single_gof(data, vn, expected)
        list(
          row = data.frame(
            Variable = vn,
            chi_squared = res$chi_sq,
            df = res$df,
            p_value = res$p_value,
            n = res$n,
            stringsAsFactors = FALSE
          ),
          freq = res$freq_table
        )
      }, error = function(e) {
        cli_warn(c(
          "Chi-square goodness-of-fit test skipped for {.var {vn}}.",
          "x" = "{conditionMessage(e)}"
        ))
        list(row = na_row(vn), freq = NULL)
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

# Internal: compact one-line summary for a single GoF test row
#' @noRd
.print_gof_compact <- function(results, i, weighted_tag, digits) {
  cat(sprintf("Chi-Square Goodness-of-Fit Test: %s%s\n",
              results$Variable[i], weighted_tag))
  if (is.na(results$chi_squared[i])) {
    cat("  not computed (see warning)\n")
    return(invisible(NULL))
  }
  cat(sprintf("  chi2(%s) = %s, %s, N = %s\n",
              formatC(as.integer(results$df[i]), format = "d"),
              fmt_num(results$chi_squared[i], digits),
              format_p_stars(results$p_value[i], digits),
              .np_count(results$n[i])))
}

#' Print chi-square goodness-of-fit test results (compact)
#'
#' @description
#' Compact print method for objects of class \code{"chisq_gof"}.
#' Shows a one-line summary per variable with the chi-square statistic,
#' degrees of freedom, p-value, and sample size.
#'
#' For the full detailed output (frequency tables with observed, expected,
#' and residual counts), use \code{summary()}.
#'
#' @param x An object of class \code{"chisq_gof"}
#' @param digits Number of decimal places (default: 3)
#' @param ... Additional arguments (currently unused)
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- chisq_gof(survey_data, gender)
#' result              # compact one-line overview
#' summary(result)     # full detailed output
#'
#' @export
print.chisq_gof <- function(x, digits = 3, ...) {
  weighted_tag <- if (!is.null(x$weights)) " [Weighted]" else ""

  if (isTRUE(x$is_grouped)) {
    groups <- unique(x$results[x$groups])

    for (i in seq_len(nrow(groups))) {
      group_values <- groups[i, , drop = FALSE]
      group_label <- .format_group_label(group_values)
      cat(sprintf("[%s]\n", group_label))

      group_results <- x$results
      for (g in names(group_values)) {
        group_results <- group_results[.group_match(group_results[[g]], group_values[[g]]), ]
      }
      for (j in seq_len(nrow(group_results))) {
        .print_gof_compact(group_results, j, weighted_tag, digits)
      }
    }
  } else {
    for (i in seq_len(nrow(x$results))) {
      .print_gof_compact(x$results, i, weighted_tag, digits)
    }
  }

  cat("Use summary() for detailed output.\n")
  invisible(x)
}

#' Summary method for chi-square goodness-of-fit test results
#'
#' @description
#' Creates a summary object that produces detailed output when printed,
#' including the frequency table with observed, expected, and residual
#' counts, and the test statistics table.
#'
#' @param object A \code{chisq_gof} result object.
#' @param frequency_table Logical. Show the frequency table with observed,
#'   expected, and residual counts? (Default: TRUE)
#' @param results Logical. Show the test statistics table? (Default: TRUE)
#' @param digits Number of decimal places for formatting (Default: 3).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.chisq_gof} object.
#'
#' @examples
#' result <- chisq_gof(survey_data, gender)
#' summary(result)
#' summary(result, frequency_table = FALSE)
#'
#' @seealso \code{\link{chisq_gof}} for the main analysis function.
#' @export
#' @method summary chisq_gof
summary.chisq_gof <- function(object, frequency_table = TRUE, results = TRUE,
                              digits = 3, ...) {
  build_summary_object(
    object     = object,
    show       = list(frequency_table = frequency_table, results = results),
    digits     = digits,
    class_name = "summary.chisq_gof"
  )
}

#' Print summary of chi-square goodness-of-fit results (detailed output)
#'
#' @description
#' Displays the detailed output for a chi-square goodness-of-fit test, with
#' sections controlled by the boolean parameters passed to
#' \code{\link{summary.chisq_gof}}.  Sections include per-variable
#' frequency tables and the test statistics table.
#'
#' @param x A \code{summary.chisq_gof} object created by
#'   \code{\link{summary.chisq_gof}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- chisq_gof(survey_data, gender)
#' summary(result)                          # all sections
#' summary(result, frequency_table = FALSE) # hide frequency tables
#'
#' @seealso \code{\link{chisq_gof}} for the main analysis,
#'   \code{\link{summary.chisq_gof}} for summary options.
#' @export
#' @method print summary.chisq_gof
print.summary.chisq_gof <- function(x, ...) {
  digits <- x$digits
  weights_name <- x$weights
  test_type <- get_standard_title("Chi-Square Goodness-of-Fit Test",
                                  weights_name, "Results")
  print_header(test_type, newline_before = FALSE)

  # Resolve show toggles
  show_freq    <- isTRUE(x$show$frequency_table)
  show_results <- isTRUE(x$show$results)

  cat("\n")
  test_info <- list(
    "Variables" = paste(x$variables, collapse = ", "),
    "Expected" = if (is.null(x$expected)) "Equal proportions" else {
      shown <- fmt_num(x$expected, digits)
      if (!is.null(names(x$expected))) {
        shown <- paste(names(x$expected), "=", shown)
      }
      paste(shown, collapse = ", ")
    },
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
        group_results <- group_results[.group_match(group_results[[g]], group_values[[g]]), ]
      }

      if (nrow(group_results) > 0 && show_results) {
        cat("\n")
        .print_gof_table(group_results, digits)
      }
    }
  } else {
    # Print frequency tables if available (gated by frequency_table toggle)
    if (show_freq && !is.null(x$frequencies)) {
      freqs <- if (is.data.frame(x$frequencies)) {
        stats::setNames(list(x$frequencies), x$variables[1])
      } else {
        x$frequencies
      }
      for (vn in x$variables) {
        freq <- freqs[[vn]]
        if (!is.null(freq)) {
          cat(sprintf("  %s - Frequency Table:\n", vn))
          .print_gof_frequencies(freq)
        }
      }
    }

    # Print test statistics (gated by results toggle)
    if (show_results) {
      .print_gof_table(x$results, digits)
    }
  }

  if (show_results) {
    print_significance_legend()
  }
  invisible(x)
}

#' @noRd
.print_gof_table <- function(results_df, digits = 3) {
  display <- data.frame(
    Variable = results_df$Variable,
    chi = results_df$chi_squared,
    df = results_df$df,
    p = results_df$p_value,
    N = .np_count(results_df$n),
    stars = add_significance_stars(results_df$p_value),
    stringsAsFactors = FALSE
  )
  print_stat_table(display, digits = digits, indent = 0,
                   col_types = c(chi = "num", df = "int", N = "char"),
                   col_labels = c(chi = "Chi-Square", p = "p-value",
                                  stars = ""))
  cat("\n")
}

#' Observed/expected/residual table of one variable (SPSS: 1 decimal)
#' @noRd
.print_gof_frequencies <- function(freq) {
  display <- data.frame(
    category = freq$category,
    observed = .np_count(freq$observed),
    expected = fmt_num(freq$expected, 1),
    residual = fmt_num(freq$residual, 1),
    stringsAsFactors = FALSE
  )
  print_stat_table(display, indent = 2,
                   col_types = c(observed = "char", expected = "char",
                                 residual = "char"))
  cat("\n")
}

#' Validate and normalise `expected` of chisq_gof() (SPSS /EXPECTED)
#'
#' SPSS treats the /EXPECTED values as relative frequencies and divides them
#' by their sum, so counts (50 30 20) or equal weights (1 1) are valid.
#' Values that are all below 1 are read as proportions: they must sum to 1,
#' and a sum within 0.01 of 1 (rounded proportions such as 0.995) is
#' rescaled with a message.
#'
#' @param expected NULL or numeric vector (optionally named)
#' @return NULL or the proportions (names kept)
#' @noRd
.gof_normalize_expected <- function(expected, call = rlang::caller_env()) {
  if (is.null(expected)) return(NULL)
  if (!is.numeric(expected) || anyNA(expected) || any(expected <= 0)) {
    cli_abort(
      "{.arg expected} must be a numeric vector of positive proportions (or relative frequencies).",
      call = call
    )
  }
  nm <- names(expected)
  if (!is.null(nm) && (anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm))) {
    cli_abort(
      "{.arg expected} must be either unnamed or have a unique name for every category.",
      call = call
    )
  }
  total <- sum(expected)
  if (all(expected < 1)) {
    if (abs(total - 1) > 0.01) {
      cli_abort(c(
        "{.arg expected} proportions must sum to 1.",
        "x" = "Current sum: {round(total, 4)}.",
        "i" = "Counts or other relative frequencies (as in SPSS {.code /EXPECTED=50 30 20}) are also accepted; they are divided by their sum."
      ), call = call)
    }
    if (abs(total - 1) > sqrt(.Machine$double.eps)) {
      cli_inform(
        "{.arg expected} proportions sum to {round(total, 4)}; rescaled to sum to 1."
      )
    }
  }
  out <- expected / total
  names(out) <- nm
  out
}

#' Align normalised `expected` proportions with a variable's categories
#'
#' Named vectors are matched by category name (or by the code of a labelled
#' variable, stored in attr(freq_tbl, "codes")); unnamed vectors are used in
#' category order.
#'
#' @param expected Output of .gof_normalize_expected()
#' @param freq_tbl Observed frequency table (names = category labels)
#' @param var_name Variable name for error messages
#' @return Unnamed numeric vector aligned with freq_tbl
#' @noRd
.gof_align_expected <- function(expected, freq_tbl, var_name,
                                call = rlang::caller_env()) {
  cats <- names(freq_tbl)
  codes <- attr(freq_tbl, "codes", exact = TRUE)
  k <- length(cats)
  nm <- names(expected)

  if (!is.null(nm)) {
    idx <- match(cats, nm)
    if (!is.null(codes)) {
      by_code <- match(codes, nm)
      idx[is.na(idx)] <- by_code[is.na(idx)]
    }
    unknown <- setdiff(nm, c(cats, codes))
    if (length(unknown) > 0 || anyNA(idx) || length(expected) != k) {
      missing_cats <- cats[is.na(idx)]
      cli_abort(c(
        "The names of {.arg expected} do not match the categories of {.var {var_name}}.",
        "x" = if (length(unknown) > 0) "Unknown name{?s}: {.val {unknown}}.",
        "x" = if (length(missing_cats) > 0) "No value for: {.val {missing_cats}}.",
        "i" = "Categories of {.var {var_name}}: {.val {cats}}."
      ), call = call)
    }
    return(unname(expected[idx]))
  }

  if (length(expected) != k) {
    cli_abort(c(
      "Length of {.arg expected} does not match the number of categories of {.var {var_name}}.",
      "x" = "{.var {var_name}} has {k} categor{?y/ies} ({.val {cats}}); {.arg expected} has {length(expected)} value{?s}."
    ), call = call)
  }
  unname(expected)
}
