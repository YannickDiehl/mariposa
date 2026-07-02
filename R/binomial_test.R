
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
#'   This refers to the proportion of the first category (first factor level,
#'   or the lower numeric value).
#' @param weights Optional survey weights for population-representative results
#' @param conf.level Confidence level for intervals (Default: 0.95 = 95 percent)
#'
#' @return Test results showing whether the proportion differs from expected,
#'   including:
#' - Category counts and observed proportions
#' - Test proportion (null hypothesis)
#' - Exact p-value (two-sided)
#' - Confidence interval for the true proportion
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
#' - For larger samples with normal approximation:
#' ## Weighted variants
#'
#' SPSS \code{NPAR TESTS} ignores \code{WEIGHT BY}, so weighted results have
#' no SPSS reference. The weighted variant is an R-only frequency-weight
#' extension that reduces exactly to the unweighted test when all weights
#' equal 1 (enforced by an internal invariance suite); see
#' \code{vignette("spss-compatibility")} for validation status.
#'
#'   Results will be very similar to a one-sample z-test for proportions
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
    } else {
      valid_idx <- !is.na(x)
      x <- x[valid_idx]
    }

    # Get categories
    if (is.factor(x)) {
      cats <- levels(x)
      cats <- cats[cats %in% unique(as.character(x))]
    } else {
      cats <- sort(unique(x))
    }

    if (length(cats) != 2) {
      cli_abort(c(
        "Binomial test requires exactly 2 categories.",
        "x" = "Variable {.var {var_name}} has {length(cats)} unique value{?s}.",
        "i" = "Ensure your variable is binary (e.g., Yes/No, 0/1, TRUE/FALSE)."
      ))
    }

    # Group 1 = first category (first factor level or lower value)
    cat1 <- cats[1]
    cat2 <- cats[2]

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

    # Exact binomial test
    bt <- binom.test(n1, n_total, p = test_prop,
                     alternative = "two.sided", conf.level = conf_level)

    p_value <- bt$p.value
    ci_lower <- bt$conf.int[1]
    ci_upper <- bt$conf.int[2]

    return(list(
      cat1_name = as.character(cat1),
      cat2_name = as.character(cat2),
      n1 = n1,
      n2 = n2,
      n_total = n_total,
      obs_prop1 = obs_prop1,
      obs_prop2 = obs_prop2,
      test_prop = test_prop,
      p_value = p_value,
      ci_lower = ci_lower,
      ci_upper = ci_upper
    ))
  }

  # Main computation function (loops over variables)
  compute_results <- function(data) {
    results_list <- list()
    single_var <- length(var_names) == 1

    for (var_name in var_names) {
      if (single_var) {
        # Single variable: let errors propagate to the user
        result <- perform_single_binomial(data, var_name, w_name, p, conf.level)

        results_list[[var_name]] <- tibble(
          Variable = var_name,
          cat1_name = result$cat1_name,
          cat2_name = result$cat2_name,
          n1 = result$n1,
          n2 = result$n2,
          n_total = result$n_total,
          obs_prop1 = result$obs_prop1,
          obs_prop2 = result$obs_prop2,
          test_prop = result$test_prop,
          p_value = result$p_value,
          ci_lower = result$ci_lower,
          ci_upper = result$ci_upper
        )
      } else {
        # Multiple variables: catch errors so other variables still run
        tryCatch({
          result <- perform_single_binomial(data, var_name, w_name, p, conf.level)

          results_list[[var_name]] <- tibble(
            Variable = var_name,
            cat1_name = result$cat1_name,
            cat2_name = result$cat2_name,
            n1 = result$n1,
            n2 = result$n2,
            n_total = result$n_total,
            obs_prop1 = result$obs_prop1,
            obs_prop2 = result$obs_prop2,
            test_prop = result$test_prop,
            p_value = result$p_value,
            ci_lower = result$ci_lower,
            ci_upper = result$ci_upper
          )

        }, error = function(e) {
          cli_warn("Binomial test failed for variable {.var {var_name}}: {e$message}")
          results_list[[var_name]] <<- tibble(
            Variable = var_name,
            cat1_name = NA_character_,
            cat2_name = NA_character_,
            n1 = NA_integer_,
            n2 = NA_integer_,
            n_total = NA_integer_,
            obs_prop1 = NA_real_,
            obs_prop2 = NA_real_,
            test_prop = p,
            p_value = NA_real_,
            ci_lower = NA_real_,
            ci_upper = NA_real_
          )
        })
      }
    }

    bind_rows(results_list)
  }

  # Execute computation (with or without group_by)
  if (is_grouped) {
    results <- data %>%
      group_modify(~ compute_results(.x))
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

  # Category table (gated by categories toggle)
  if (show_categories) {
    cat("  Categories:\n")
    cat_df <- data.frame(
      ` ` = c(paste("Group 1:", row_data$cat1_name),
              paste("Group 2:", row_data$cat2_name),
              "Total"),
      N = c(as.integer(row_data$n1), as.integer(row_data$n2),
            as.integer(row_data$n_total)),
      `Observed Prop.` = c(round(row_data$obs_prop1, 3),
                            round(row_data$obs_prop2, 3),
                            1.000),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    output <- capture.output(print(cat_df, row.names = FALSE))
    border_width <- max(nchar(output), na.rm = TRUE)
    border <- paste(rep("-", border_width), collapse = "")

    cat("  ", border, "\n", sep = "")
    for (line in output) {
      cat("  ", line, "\n", sep = "")
    }
    cat("  ", border, "\n\n", sep = "")
  }

  # Test statistics (gated by results toggle)
  if (show_results) {
    test_df <- data.frame(
      `Test Prop.` = row_data$test_prop,
      `p value` = round(row_data$p_value, digits),
      `CI lower` = round(row_data$ci_lower, digits),
      `CI upper` = round(row_data$ci_upper, digits),
      sig = row_data$sig,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    label <- if (!is.null(weights)) "Weighted Test Statistics:" else "Test Statistics:"
    cat(sprintf("  %s\n", label))
    output <- capture.output(print(test_df, row.names = FALSE))
    border_width <- max(nchar(output), na.rm = TRUE)
    border <- paste(rep("-", border_width), collapse = "")

    cat("  ", border, "\n", sep = "")
    for (line in output) {
      cat("  ", line, "\n", sep = "")
    }
    cat("  ", border, "\n\n", sep = "")
  }
}

# Internal: compact one-line summary for a single binomial test variable
#' @noRd
.print_bt_compact <- function(results, i, weighted_tag, digits) {
  cat(sprintf("Binomial Test: %s%s\n", results$Variable[i], weighted_tag))
  cat(sprintf("  Group 1 (%s): prop = %s vs %s, %s %s, N = %s\n",
              results$cat1_name[i],
              fmt_num(results$obs_prop1[i], digits),
              fmt_num(results$test_prop[i], digits),
              fmt_p(results$p_value[i], digits, style = "compact"),
              add_significance_stars(results$p_value[i]),
              formatC(as.integer(results$n_total[i]), format = "d")))
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
    group_vars <- setdiff(names(results), c("Variable", "cat1_name",
                                            "cat2_name", "n1", "n2",
                                            "n_total", "obs_prop1",
                                            "obs_prop2", "test_prop",
                                            "p_value", "ci_lower",
                                            "ci_upper"))
    groups <- unique(results[group_vars])

    for (i in seq_len(nrow(groups))) {
      group_values <- groups[i, , drop = FALSE]
      group_label <- paste(names(group_values), "=", group_values, collapse = ", ")
      cat(sprintf("[%s]\n", group_label))

      group_results <- results
      for (g in names(group_values)) {
        group_results <- group_results[group_results[[g]] == group_values[[g]], ]
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
    group_vars <- setdiff(names(x$results), c("Variable", "cat1_name",
                                                "cat2_name", "n1", "n2",
                                                "n_total", "obs_prop1",
                                                "obs_prop2", "test_prop",
                                                "p_value", "ci_lower",
                                                "ci_upper", "sig"))
    groups <- unique(x$results[group_vars])

    for (i in seq_len(nrow(groups))) {
      group_values <- groups[i, , drop = FALSE]
      print_group_header(group_values)
      cat("\n")

      # Filter results for current group
      group_results <- x$results
      for (g in names(group_values)) {
        group_results <- group_results[group_results[[g]] == group_values[[g]], ]
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
