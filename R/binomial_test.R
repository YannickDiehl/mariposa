
#' Test Whether a Proportion Matches an Expected Value
#'
#' @description
#' \code{binomial_test()} tests whether the observed proportion of a binary
#' variable differs from a hypothesized proportion. It uses the exact binomial
#' test, making it valid for any sample size.
#'
#' Think of it as:
#' - Testing whether a coin is fair (proportion of heads = 50 percent)
#' - Checking if your sample's gender ratio matches the population
#' - Verifying if satisfaction rates meet a target proportion
#'
#' The test tells you:
#' - Whether the observed proportion differs significantly from the expected
#' - The exact p-value (based on the binomial distribution)
#' - A confidence interval for the true proportion
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... One or more binary variables to test. Each must have exactly 2
#'   categories (e.g., Yes/No, Male/Female, 0/1, TRUE/FALSE)
#' @param p The hypothesized proportion to test against (Default: 0.50 = 50 percent).
#'   It refers to Group 1, the category of the first valid case in the data
#'   (as in SPSS). With 0.5 the test is two-tailed; with any other value it
#'   is one-tailed in the direction of the observed proportion, as SPSS
#'   reports it.
#' @param weights Optional survey weights for population-representative results
#' @param conf.level Confidence level for intervals (Default: 0.95 = 95 percent)
#'
#' @return Test results showing whether the proportion differs from expected,
#'   including:
#' - Category counts and observed proportions
#' - Test proportion (null hypothesis)
#' - Exact p-value (two-tailed for \code{p = 0.5}, otherwise one-tailed;
#'   column \code{alternative}: "two.sided", "less" or "greater")
#' - Confidence interval for the true proportion of Group 1 (Clopper-Pearson)
#'
#' @details
#' ## Understanding the Results
#'
#' **P-value**: If p < 0.05, the proportion differs significantly from
#' the test proportion
#' - p < 0.001: Very strong evidence the proportion differs
#' - p < 0.01: Strong evidence the proportion differs
#' - p < 0.05: Moderate evidence the proportion differs
#' - p > 0.05: No significant difference from expected proportion
#'
#' **Observed Proportion**: The actual proportion in your data.
#' Compare this with the test proportion to see the direction of any
#' difference.
#'
#' **Confidence Interval**: The range likely to contain the true
#' population proportion. If the test proportion falls outside this range,
#' the result is significant.
#'
#' ## When to Use This
#'
#' Use the binomial test when:
#' - You want to compare an observed proportion to a known value
#' - Your variable has exactly 2 categories
#' - You need an exact test (not relying on normal approximation)
#' - Sample size is small (where chi-square may not be reliable)
#'
#' ## Relationship to Other Tests
#'
#' - For testing association between two categorical variables:
#'   Use \code{\link{chi_square}()} instead
#' - For comparing proportions between groups:
#'   Use chi-square or z-test for proportions
#' - For larger samples with normal approximation: results will be very
#'   similar to a one-sample z-test for proportions
#'
#' ## Group 1 and one-tailed tests
#'
#' As in SPSS \code{NPAR TESTS /BINOMIAL}, Group 1 is the category of the
#' first valid case in the data (per group with \code{group_by()}), and
#' \code{p} is its hypothesized proportion. For \code{p = 0.5} the p-value
#' is two-tailed. For any other \code{p} SPSS reports a one-tailed p-value
#' in the direction of the observed proportion ("the proportion of cases in
#' the first group < p"); \code{binomial_test()} does the same. To test the
#' other category, put a case of it first or use \code{1 - p}.
#'
#' ## Weighted variants
#'
#' SPSS \code{NPAR TESTS} ignores \code{WEIGHT BY}, so weighted results have
#' no SPSS reference. The weighted variant is an R-only frequency-weight
#' extension that reduces exactly to the unweighted test when all weights
#' equal 1 (enforced by an internal invariance suite); see
#' \code{vignette("spss-compatibility")} for validation status.
#'
#' @seealso
#' \code{\link[stats]{binom.test}} for the base R exact binomial test.
#'
#' \code{\link{chi_square}} for testing associations between categorical
#' variables.
#'
#' @references
#' Conover, W. J. (1999). Practical nonparametric statistics (3rd ed.).
#' John Wiley & Sons.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Test whether gender split is 50/50
#' survey_data %>%
#'   binomial_test(gender, p = 0.50)
#'
#' # Test whether East region proportion is 50%
#' survey_data %>%
#'   binomial_test(region, p = 0.50)
#'
#' # Multiple variables at once
#' survey_data %>%
#'   binomial_test(gender, region, p = 0.50)
#'
#' # Weighted analysis
#' survey_data %>%
#'   binomial_test(gender, p = 0.50, weights = sampling_weight)
#'
#' # Grouped analysis (separate test per region)
#' survey_data %>%
#'   group_by(region) %>%
#'   binomial_test(gender, p = 0.50)
#'
#' @family hypothesis_tests
#' @export
binomial_test <- function(data, ..., p = 0.50, weights = NULL,
                           conf.level = 0.95) {
  .check_required("data")

  # Input validation
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }

  if (p < 0 || p > 1) {
    cli_abort("{.arg p} must be between 0 and 1.")
  }

  # Check if data is grouped
  is_grouped <- inherits(data, "grouped_df")
  grp_vars <- if (is_grouped) dplyr::group_vars(data) else NULL

  # Select variables using centralized helper
  vars <- .process_variables(data, ...)
  var_names <- names(vars)

  # Process weights using centralized helper
  weights_info <- .process_weights(data, rlang::enquo(weights))
  data <- weights_info$data
  w_name <- weights_info$name

  # Helper function to perform binomial test for a single variable
  perform_single_binomial <- function(data, var_name, weight_name = NULL,
                                       test_prop = 0.50, conf_level = 0.95) {
    x <- data[[var_name]]

    # Handle weights
    if (!is.null(weight_name)) {
      w <- data[[weight_name]]
      valid_idx <- !is.na(x) & !is.na(w)
      x <- x[valid_idx]
      w <- w[valid_idx]
      first <- which(w > 0)[1]
    } else {
      first <- 1L
      valid_idx <- !is.na(x)
      x <- x[valid_idx]
    }
    if (is.na(first)) first <- 1L

    # Observed categories in SPSS order (by code), with value labels
    x <- .np_factor(x)
    cats <- levels(x)

    if (length(cats) != 2) {
      cli_abort(c(
        "{.var {var_name}} has {length(cats)} observed categor{?y/ies}; the binomial test needs exactly 2 categories.",
        "i" = "Ensure your variable is binary (e.g., Yes/No, 0/1, TRUE/FALSE)."
      ))
    }

    # Group 1 = category of the first valid case, as SPSS defines it ("the
    # first value encountered in the data"); the test proportion refers to it
    cat1 <- as.character(x[first])
    cat2 <- setdiff(cats, cat1)

    if (is.null(weight_name)) {
      # Unweighted
      n1 <- sum(x == cat1)
      n2 <- sum(x == cat2)
      n_total <- n1 + n2
    } else {
      # Weighted: SPSS rounds individual weights to integers first,
      # then sums as frequency weights
      w_rounded <- round(w)
      n1 <- sum(w_rounded[x == cat1])
      n2 <- sum(w_rounded[x == cat2])
      n_total <- n1 + n2
    }

    # Observed proportion of Group 1
    obs_prop1 <- n1 / n_total
    obs_prop2 <- n2 / n_total

    # Exact binomial test: two-tailed at .5, otherwise one-tailed (SPSS)
    bt <- .binom_exact_p(n1, n_total, test_prop)
    ci <- .clopper_pearson(n1, n_total, conf_level)

    return(list(
      cat1_name = as.character(cat1),
      cat2_name = as.character(cat2),
      n1 = n1,
      n2 = n2,
      n_total = n_total,
      obs_prop1 = obs_prop1,
      obs_prop2 = obs_prop2,
      test_prop = test_prop,
      p_value = bt$p_value,
      alternative = bt$alternative,
      ci_lower = ci[1],
      ci_upper = ci[2]
    ))
  }

  result_row <- function(var_name, res) {
    tibble(
      Variable = var_name,
      cat1_name = res$cat1_name,
      cat2_name = res$cat2_name,
      n1 = res$n1,
      n2 = res$n2,
      n_total = res$n_total,
      obs_prop1 = res$obs_prop1,
      obs_prop2 = res$obs_prop2,
      test_prop = res$test_prop,
      p_value = res$p_value,
      alternative = res$alternative,
      ci_lower = res$ci_lower,
      ci_upper = res$ci_upper,
      reason = NA_character_
    )
  }
  na_row <- function(var_name, reason) {
    tibble(
      Variable = var_name,
      cat1_name = NA_character_,
      cat2_name = NA_character_,
      n1 = NA_real_,
      n2 = NA_real_,
      n_total = NA_real_,
      obs_prop1 = NA_real_,
      obs_prop2 = NA_real_,
      test_prop = p,
      p_value = NA_real_,
      alternative = NA_character_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      reason = reason
    )
  }

  # Main computation function (loops over variables). An ungrouped single
  # variable lets its error reach the user; otherwise a variable or group
  # that cannot be tested is skipped with a warning naming it (a constant
  # variable in one group_by() group used to abort the whole call).
  compute_results <- function(data, key = NULL) {
    strict <- is.null(key) && length(var_names) == 1
    rows <- lapply(var_names, function(var_name) {
      if (strict) {
        return(result_row(var_name, perform_single_binomial(
          data, var_name, w_name, p, conf.level)))
      }
      tryCatch(
        result_row(var_name, perform_single_binomial(
          data, var_name, w_name, p, conf.level)),
        error = function(e) {
          where <- .np_where(key)
          reason <- .np_error_reason(e)
          cli_warn(c(
            "Binomial test skipped for {.var {var_name}}{where}.",
            "x" = "{reason}."
          ))
          na_row(var_name, reason)
        }
      )
    })
    bind_rows(rows)
  }

  # Execute computation (with or without group_by)
  if (is_grouped) {
    results <- data %>%
      group_modify(~ compute_results(.x, .y))
  } else {
    results <- compute_results(data)
  }

  # Create result object
  result <- list(
    results = results,
    variables = var_names,
    p = p,
    weights = w_name,
    is_grouped = is_grouped,
    groups = grp_vars,
    conf.level = conf.level
  )

  class(result) <- "binomial_test"
  return(result)
}

# Helper: print a single variable's binomial test block
#' @noRd
.print_bt_block <- function(var_name, row_data, weights, digits,
                            show_categories = TRUE, show_results = TRUE) {
  print_header(var_name, newline_before = FALSE)

  if (is.na(row_data$p_value)) {
    cat(sprintf("  Not computed (%s).\n\n", .np_reason(row_data, 1)))
    return(invisible(NULL))
  }

  # Category table (gated by categories toggle): integer N, proportions
  # with a fixed number of decimals
  if (show_categories) {
    cat("  Categories:\n")
    cat_df <- data.frame(
      category = c(paste("Group 1:", row_data$cat1_name),
                   paste("Group 2:", row_data$cat2_name),
                   "Total"),
      N = .np_count(c(row_data$n1, row_data$n2, row_data$n_total)),
      prop = c(row_data$obs_prop1, row_data$obs_prop2, 1),
      stringsAsFactors = FALSE
    )
    print_stat_table(cat_df, digits = digits, indent = 2,
                     col_types = c(N = "char", prop = "num"),
                     col_labels = c(category = "", prop = "Observed Prop."))
    cat("\n")
  }

  # Test statistics (gated by results toggle)
  if (show_results) {
    test_df <- data.frame(
      test_prop = row_data$test_prop,
      p = row_data$p_value,
      ci_lower = row_data$ci_lower,
      ci_upper = row_data$ci_upper,
      stars = add_significance_stars(row_data$p_value),
      stringsAsFactors = FALSE
    )
    one_tailed <- .bt_one_tailed(row_data, 1)
    label <- if (!is.null(weights)) "Weighted Test Statistics:" else "Test Statistics:"
    cat(sprintf("  %s\n", label))
    print_stat_table(test_df, digits = digits, indent = 2,
                     col_types = c(test_prop = "num", ci_lower = "num",
                                   ci_upper = "num"),
                     col_labels = c(test_prop = "Test Prop.",
                                    p = if (one_tailed) "p (1-tailed)" else "p (2-tailed)",
                                    ci_lower = "CI lower", ci_upper = "CI upper",
                                    stars = ""))
    if (one_tailed) {
      cat(sprintf("  H1: the proportion of Group 1 is %s %s (one-tailed, as SPSS tests\n  a proportion other than 0.5).\n",
                  if (row_data$alternative == "less") "<" else ">",
                  fmt_num(row_data$test_prop, digits)))
    }
    cat("\n")
  }
}

# Internal: compact one-line summary for a single binomial test variable
#' @noRd
.print_bt_compact <- function(results, i, weighted_tag, digits) {
  cat(sprintf("Binomial Test: %s%s\n", results$Variable[i], weighted_tag))
  if (is.na(results$p_value[i])) {
    cat(sprintf("  not computed (%s)\n", .np_reason(results, i)))
    return(invisible(NULL))
  }
  p_text <- format_p_stars(results$p_value[i], digits)
  if (.bt_one_tailed(results, i)) p_text <- sub("^p", "p (1-tailed)", p_text)
  cat(sprintf("  Group 1 (%s): prop = %s vs %s, %s, N = %s\n",
              results$cat1_name[i],
              fmt_num(results$obs_prop1[i], digits),
              fmt_num(results$test_prop[i], digits),
              p_text,
              .np_count(results$n_total[i])))
}

#' Print binomial test results (compact)
#'
#' @description
#' Compact print method for objects of class \code{"binomial_test"}.
#' Shows a one-line summary per variable with the observed vs. test
#' proportion, p-value, and sample size.
#'
#' For the full detailed output (category table, test statistics with
#' confidence interval), use \code{summary()}.
#'
#' @param x A binomial_test object
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- binomial_test(survey_data, gender, p = 0.50)
#' result              # compact one-line overview
#' summary(result)     # full detailed output
#'
#' @export
#' @method print binomial_test
print.binomial_test <- function(x, digits = 3, ...) {
  weighted_tag <- if (!is.null(x$weights)) " [Weighted]" else ""
  results <- x$results
  results$p_value <- as.numeric(results$p_value)

  if (isTRUE(x$is_grouped)) {
    group_vars <- .bt_group_vars(x)
    groups <- unique(results[group_vars])

    for (i in seq_len(nrow(groups))) {
      group_values <- groups[i, , drop = FALSE]
      group_label <- .format_group_label(group_values)
      cat(sprintf("[%s]\n", group_label))

      group_results <- results
      for (g in names(group_values)) {
        group_results <- group_results[.group_match(group_results[[g]], group_values[[g]]), ]
      }
      group_results <- group_results[!is.na(group_results$Variable), ]
      for (j in seq_len(nrow(group_results))) {
        .print_bt_compact(group_results, j, weighted_tag, digits)
      }
    }
  } else {
    for (i in seq_len(nrow(results))) {
      if (is.na(results$Variable[i])) next
      .print_bt_compact(results, i, weighted_tag, digits)
    }
  }

  cat("Use summary() for detailed output.\n")
  invisible(x)
}

#' Summary method for binomial test results
#'
#' @description
#' Creates a summary object that produces detailed output when printed,
#' including the category table with counts and observed proportions, and
#' the test statistics table with test proportion, p-value, and confidence
#' interval.
#'
#' @param object A \code{binomial_test} result object.
#' @param categories Logical. Show the category table? (Default: TRUE)
#' @param results Logical. Show test statistics table? (Default: TRUE)
#' @param digits Number of decimal places for formatting (Default: 3).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.binomial_test} object.
#'
#' @examples
#' result <- binomial_test(survey_data, gender, p = 0.50)
#' summary(result)
#' summary(result, categories = FALSE)
#'
#' @seealso \code{\link{binomial_test}} for the main analysis function.
#' @export
#' @method summary binomial_test
summary.binomial_test <- function(object, categories = TRUE, results = TRUE,
                                  digits = 3, ...) {
  build_summary_object(
    object     = object,
    show       = list(categories = categories, results = results),
    digits     = digits,
    class_name = "summary.binomial_test"
  )
}

#' Print summary of binomial test results (detailed output)
#'
#' @description
#' Displays the detailed SPSS-style output for a binomial test, with
#' sections controlled by the boolean parameters passed to
#' \code{\link{summary.binomial_test}}.  Sections include the category
#' table and the test statistics table with confidence interval.
#'
#' @param x A \code{summary.binomial_test} object created by
#'   \code{\link{summary.binomial_test}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- binomial_test(survey_data, gender, p = 0.50)
#' summary(result)                     # all sections
#' summary(result, categories = FALSE) # hide category tables
#'
#' @seealso \code{\link{binomial_test}} for the main analysis,
#'   \code{\link{summary.binomial_test}} for summary options.
#' @export
#' @method print summary.binomial_test
print.summary.binomial_test <- function(x, ...) {
  digits <- x$digits

  # Determine test type using standardized helper
  weights_name <- x$weights
  test_type <- get_standard_title("Binomial Test", weights_name, "Results")
  print_header(test_type, newline_before = FALSE)

  # Ensure p-values are numeric
  x$results$p_value <- as.numeric(x$results$p_value)

  # Add significance stars
  x$results$sig <- sapply(x$results$p_value, add_significance_stars)

  # Resolve show toggles
  show_categories <- isTRUE(x$show$categories)
  show_results    <- isTRUE(x$show$results)

  is_grouped_data <- isTRUE(x$is_grouped)

  # Print info section
  cat("\n")
  test_info <- list(
    "Test proportion" = x$p,
    "Confidence level" = sprintf("%.1f%%", x$conf.level * 100),
    "Weights variable" = weights_name
  )
  print_info_section(test_info)
  cat("\n")

  if (is_grouped_data) {
    # Get unique groups
    group_vars <- .bt_group_vars(x)
    groups <- unique(x$results[group_vars])

    for (i in seq_len(nrow(groups))) {
      group_values <- groups[i, , drop = FALSE]
      print_group_header(group_values)
      cat("\n")

      # Filter results for current group
      group_results <- x$results
      for (g in names(group_values)) {
        group_results <- group_results[.group_match(group_results[[g]], group_values[[g]]), ]
      }
      group_results <- group_results[!is.na(group_results$Variable), ]
      if (nrow(group_results) == 0) next

      for (j in seq_len(nrow(group_results))) {
        .print_bt_block(
          var_name = group_results$Variable[j],
          row_data = group_results[j, ],
          weights = x$weights,
          digits = digits,
          show_categories = show_categories,
          show_results = show_results
        )
      }
    }
  } else {
    valid_results <- x$results[!is.na(x$results$Variable), ]

    for (i in seq_len(nrow(valid_results))) {
      .print_bt_block(
        var_name = valid_results$Variable[i],
        row_data = valid_results[i, ],
        weights = x$weights,
        digits = digits,
        show_categories = show_categories,
        show_results = show_results
      )
    }
  }

  if (!is.null(x$weights) && (show_categories || show_results)) {
    cat("Note: Weighted analysis uses rounded frequency weights.\n")
  }

  if (show_results) {
    print_significance_legend()
  }

  invisible(x)
}

#' Is row i of a binomial_test result a one-tailed test?
#' @noRd
.bt_one_tailed <- function(results, i) {
  alt <- results[["alternative"]]
  !is.null(alt) && !is.na(alt[i]) && alt[i] != "two.sided"
}

#' Grouping columns of a binomial_test result
#' @noRd
.bt_group_vars <- function(x) {
  if (!is.null(x$groups)) return(x$groups)
  setdiff(names(x$results), c("Variable", "cat1_name", "cat2_name", "n1",
                              "n2", "n_total", "obs_prop1", "obs_prop2",
                              "test_prop", "p_value", "alternative",
                              "ci_lower",
                              "ci_upper", "reason", "sig"))
}
