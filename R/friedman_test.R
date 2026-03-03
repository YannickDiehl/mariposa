
#' Compare Three or More Related Measurements Without Assuming Normality
#'
#' @description
#' \code{friedman_test()} compares three or more related measurements from the
#' same subjects when your data isn't normally distributed. It's the non-parametric
#' alternative to repeated-measures ANOVA.
#'
#' Think of it as:
#' - Comparing ratings of multiple items by the same respondents
#' - Testing whether scores change across three or more time points
#' - A robust repeated-measures comparison that works with any data shape
#'
#' The test tells you:
#' - Whether at least one measurement is significantly different from the others
#' - Which measurements tend to be rated higher or lower (via mean ranks)
#' - The strength of the overall effect (Kendall's W)
#'
#' @param data Your survey data (a data frame or tibble) in wide format,
#'   with one row per subject and each measurement in a separate column
#' @param ... The measurement variables to compare (at least 3). You can list
#'   them individually or use helpers like \code{starts_with("trust_")}
#' @param weights Optional survey weights for population-representative results
#' @param conf.level Confidence level for intervals (Default: 0.95 = 95 percent)
#'
#' @return Test results showing whether the measurements differ, including:
#' - Chi-Square statistic (Friedman test statistic)
#' - Degrees of freedom (number of measurements minus 1)
#' - P-value (are measurements different?)
#' - Kendall's W (effect size: how strong is the overall pattern?)
#' - Mean rank for each measurement (which measurements are higher/lower?)
#'
#' @details
#' ## Understanding the Results
#'
#' **P-value**: If p < 0.05, at least one measurement is significantly different
#' - p < 0.001: Very strong evidence of differences
#' - p < 0.01: Strong evidence of differences
#' - p < 0.05: Moderate evidence of differences
#' - p > 0.05: No significant differences found
#'
#' **Kendall's W** (Effect size: How consistent is the pattern?):
#' - < 0.1: Negligible agreement/effect
#' - 0.1 - 0.3: Weak agreement
#' - 0.3 - 0.5: Moderate agreement
#' - 0.5 or higher: Strong agreement
#'
#' **Mean Ranks**:
#' - Higher mean rank = measurement tends to have higher values
#' - Lower mean rank = measurement tends to have lower values
#' - Compare mean ranks to see which measurements stand out
#'
#' ## When to Use This
#'
#' Use the Friedman test when:
#' - You have 3 or more related measurements from the same subjects
#' - Your data is not normally distributed
#' - You have ordinal data (ratings, rankings)
#' - You want a robust alternative to repeated-measures ANOVA
#' - You're comparing multiple ratings by the same respondents
#'
#' ## Relationship to Other Tests
#'
#' - For 2 related measurements: Use \code{\link{wilcoxon_test}()} instead
#' - For normally distributed repeated measures: Use repeated-measures ANOVA
#' - For independent groups: Use \code{\link{kruskal_wallis}()} instead
#'
#' @seealso
#' \code{\link[stats]{friedman.test}} for the base R Friedman test.
#'
#' \code{\link{wilcoxon_test}} for comparing two related measurements.
#'
#' \code{\link{kruskal_wallis}} for comparing independent groups.
#'
#' @references
#' Friedman, M. (1937). The use of ranks to avoid the assumption of normality
#' implicit in the analysis of variance. Journal of the American Statistical
#' Association, 32(200), 675-701.
#'
#' Kendall, M. G., & Babington Smith, B. (1939). The problem of m rankings.
#' The Annals of Mathematical Statistics, 10(3), 275-287.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Compare three trust items (rated by same respondents)
#' survey_data %>%
#'   friedman_test(trust_government, trust_media, trust_science)
#'
#' # Using tidyselect helpers
#' survey_data %>%
#'   friedman_test(starts_with("trust_"))
#'
#' # Weighted analysis
#' survey_data %>%
#'   friedman_test(trust_government, trust_media, trust_science,
#'                 weights = sampling_weight)
#'
#' # Grouped analysis (separate test per region)
#' survey_data %>%
#'   group_by(region) %>%
#'   friedman_test(trust_government, trust_media, trust_science)
#'
#' @family hypothesis_tests
#' @export
friedman_test <- function(data, ..., weights = NULL, conf.level = 0.95) {

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

  # Friedman requires at least 3 related measurements
  if (length(var_names) < 3) {
    cli_abort(c(
      "Friedman test requires at least 3 related measurements.",
      "x" = "Found {length(var_names)} variable{?s}.",
      "i" = "For 2 measurements, use {.fn wilcoxon_test} instead."
    ))
  }

  # Process weights using centralized helper
  weights_info <- .process_weights(data, rlang::enquo(weights))
  w_name <- weights_info$name

  # Helper function to perform Friedman test
  perform_single_friedman <- function(data, var_names, weight_name = NULL) {
    # Build matrix of measurements (rows = subjects, cols = variables)
    mat <- as.matrix(data[, var_names, drop = FALSE])

    # Remove rows with any NA (listwise deletion, matching SPSS)
    if (!is.null(weight_name)) {
      w <- data[[weight_name]]
      valid_rows <- complete.cases(mat) & !is.na(w)
      w <- w[valid_rows]
    } else {
      valid_rows <- complete.cases(mat)
    }
    mat <- mat[valid_rows, , drop = FALSE]

    n <- nrow(mat)
    k <- ncol(mat)

    if (n < 2) {
      cli_abort("Friedman test requires at least 2 complete cases.")
    }

    if (is.null(weight_name)) {
      # ----------------------------------------------------------------
      # Unweighted Friedman Test
      # ----------------------------------------------------------------

      # Use stats::friedman.test with matrix input
      fr_result <- friedman.test(mat)

      chi_sq <- fr_result$statistic[[1]]
      df <- fr_result$parameter[[1]]
      p_value <- fr_result$p.value

      # Compute mean ranks per variable (rank within each subject)
      rank_mat <- t(apply(mat, 1, rank))
      mean_ranks <- colMeans(rank_mat)

    } else {
      # ----------------------------------------------------------------
      # Weighted Friedman Test
      # ----------------------------------------------------------------
      # SPSS treats weights as frequency weights (rounded to integers).
      # Since sampling_weight is close to 1.0, results are nearly identical
      # to unweighted. We implement the full weighted version for correctness.

      # Rank within each subject (row)
      rank_mat <- t(apply(mat, 1, rank))

      # Weighted mean ranks
      w_total <- sum(w)
      mean_ranks <- colSums(w * rank_mat) / w_total

      # Weighted Friedman chi-squared:
      # Chi^2 = [12 * N / (k * (k+1))] * sum((Rbar_j - (k+1)/2)^2)
      # This is algebraically equivalent to the standard formula
      expected_rank <- (k + 1) / 2
      chi_sq <- (12 * w_total) / (k * (k + 1)) *
        sum((mean_ranks - expected_rank)^2)

      df <- k - 1
      p_value <- pchisq(chi_sq, df = df, lower.tail = FALSE)

      n <- round(w_total)
    }

    # Effect size: Kendall's W = Chi^2 / (N * (k - 1))
    kendall_w <- chi_sq / (n * (k - 1))

    # Build mean_ranks as named list
    mean_ranks_list <- as.list(round(mean_ranks, 4))
    names(mean_ranks_list) <- var_names

    return(list(
      chi_sq = chi_sq,
      df = df,
      p_value = p_value,
      kendall_w = kendall_w,
      n = n,
      k = k,
      mean_ranks = mean_ranks_list
    ))
  }

  # Main computation function
  compute_results <- function(data) {
    tryCatch({
      result <- perform_single_friedman(data, var_names, w_name)

      tibble(
        chi_sq = result$chi_sq,
        df = result$df,
        p_value = result$p_value,
        kendall_w = result$kendall_w,
        n = result$n,
        k = result$k,
        mean_ranks = list(result$mean_ranks)
      )
    }, error = function(e) {
      cli_warn("Friedman test failed: {e$message}")
      tibble(
        chi_sq = NA_real_,
        df = NA_integer_,
        p_value = NA_real_,
        kendall_w = NA_real_,
        n = NA_integer_,
        k = NA_integer_,
        mean_ranks = list(NULL)
      )
    })
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
    weights = w_name,
    is_grouped = is_grouped,
    conf.level = conf.level,
    data = data[, unique(c(var_names, w_name, grp_vars)), drop = FALSE]
  )

  class(result) <- "friedman_test"
  return(result)
}

# Helper: print the rank table and test statistics for a Friedman test
#' @keywords internal
.print_friedman_block <- function(row_data, var_names, weights, digits) {
  # Print rank table
  cat("  Ranks:\n")
  mean_ranks <- row_data$mean_ranks[[1]]

  rank_df <- data.frame(
    Variable = var_names,
    `Mean Rank` = sapply(var_names, function(v) round(mean_ranks[[v]], 2)),
    check.names = FALSE,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  output <- capture.output(print(rank_df, row.names = FALSE))
  border_width <- max(nchar(output), na.rm = TRUE)
  border <- paste(rep("-", border_width), collapse = "")

  cat("  ", border, "\n", sep = "")
  for (line in output) {
    cat("  ", line, "\n", sep = "")
  }
  cat("  ", border, "\n\n", sep = "")

  # Print test statistics table
  test_df <- data.frame(
    N = as.integer(row_data$n),
    `Chi-Square` = round(row_data$chi_sq, digits),
    df = as.integer(row_data$df),
    `p value` = round(row_data$p_value, digits),
    `Kendall's W` = round(row_data$kendall_w, digits),
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

#' Print method for Friedman test results
#'
#' @param x A friedman_test object
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object \code{x}.
#' @export
#' @method print friedman_test
print.friedman_test <- function(x, digits = 3, ...) {

  # Determine test type using standardized helper
  weights_name <- x$weights
  test_type <- get_standard_title("Friedman Test", weights_name, "Results")
  print_header(test_type)

  # Ensure p-values are numeric
  x$results$p_value <- as.numeric(x$results$p_value)

  # Add significance stars
  x$results$sig <- sapply(x$results$p_value, add_significance_stars)

  is_grouped_data <- isTRUE(x$is_grouped)

  # Print info section
  cat("\n")
  test_info <- list(
    "Variables" = paste(x$variables, collapse = ", "),
    "Number of conditions" = length(x$variables),
    "Weights variable" = weights_name
  )
  print_info_section(test_info)
  cat("\n")

  if (is_grouped_data) {
    # Get unique groups
    group_vars <- setdiff(names(x$results), c("chi_sq", "df", "p_value",
                                                "kendall_w", "n", "k",
                                                "mean_ranks", "sig"))
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
      if (nrow(group_results) == 0) next

      .print_friedman_block(
        row_data = group_results[1, ],
        var_names = x$variables,
        weights = x$weights,
        digits = digits
      )
    }
  } else {
    .print_friedman_block(
      row_data = x$results[1, ],
      var_names = x$variables,
      weights = x$weights,
      digits = digits
    )
  }

  if (!is.null(x$weights)) {
    cat("Note: Weighted analysis uses frequency-weighted ranks.\n")
  }

  print_significance_legend()

  cat("\nEffect Size Interpretation (Kendall's W):\n")
  cat("- Weak agreement: 0.1 - 0.3\n")
  cat("- Moderate agreement: 0.3 - 0.5\n")
  cat("- Strong agreement: > 0.5\n")

  invisible(x)
}
