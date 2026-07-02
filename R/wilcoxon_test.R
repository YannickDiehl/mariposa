
#' Compare Two Related Measurements Without Assuming Normality
#'
#' @description
#' \code{wilcoxon_test()} compares two paired measurements from the same
#' subjects when your data isn't normally distributed. It's the non-parametric
#' alternative to the paired t-test.
#'
#' Think of it as:
#' - A way to test whether scores changed between two time points
#' - Comparing ratings of two items from the same respondents
#' - A robust paired comparison that works with any data shape
#'
#' The test tells you:
#' - Whether scores are significantly different between the two measurements
#' - How many subjects increased, decreased, or stayed the same
#' - The strength of the effect (effect size r)
#'
#' @param data Your survey data (a data frame or tibble) in wide format,
#'   with one row per subject and the two measurements in separate columns
#' @param x The first measurement variable (e.g., pre-test, trust in government)
#' @param y The second measurement variable (e.g., post-test, trust in media).
#'   The difference is computed as \code{y - x}
#' @param weights Optional survey weights for population-representative results
#' @param conf.level Confidence level for intervals (Default: 0.95 = 95 percent)
#'
#' @return Test results showing whether the two measurements differ, including:
#' - Z statistic (standardized test statistic, normal approximation)
#' - P-value (is there a significant difference?)
#' - Effect size r (how strong is the difference?)
#' - Rank statistics (negative ranks, positive ranks, ties)
#'
#' @details
#' ## Understanding the Results
#'
#' **P-value**: If p < 0.05, the two measurements are significantly different
#' - p < 0.001: Very strong evidence of a difference
#' - p < 0.01: Strong evidence of a difference
#' - p < 0.05: Moderate evidence of a difference
#' - p > 0.05: No significant difference found
#'
#' **Effect Size r** (How strong is the difference?):
#' - < 0.1: Negligible effect
#' - 0.1 - 0.3: Small effect
#' - 0.3 - 0.5: Medium effect
#' - 0.5 or higher: Large effect
#'
#' **Rank Categories**:
#' - Negative Ranks: subjects where y < x (decreased)
#' - Positive Ranks: subjects where y > x (increased)
#' - Ties: subjects where y = x (no change)
#'
#' ## When to Use This
#'
#' Use Wilcoxon signed-rank test when:
#' - Comparing two related measurements from the same subjects
#' - Your data is not normally distributed
#' - You have ordinal data (ratings, rankings)
#' - Sample size is small
#' - You want a robust alternative to the paired t-test
#'
#' ## Relationship to Other Tests
#'
#' - For normally distributed paired data: Use paired t-test instead
#' - For independent groups: Use \code{\link{mann_whitney}()} instead
#' - For 3+ related measurements: Use \code{friedman_test()} instead
#'
#' @seealso
#' \code{\link[stats]{wilcox.test}} for the base R Wilcoxon test.
#'
#' \code{\link{mann_whitney}} for comparing two independent groups.
#'
#' @references
#' Wilcoxon, F. (1945). Individual comparisons by ranking methods. Biometrics
#' Bulletin, 1(6), 80-83.
#'
#' Fritz, C. O., Morris, P. E., & Richler, J. J. (2012). Effect size estimates:
#' current use, calculations, and interpretation. Journal of Experimental
#' Psychology: General, 141(1), 2.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Compare trust in government vs trust in media
#' survey_data %>%
#'   wilcoxon_test(x = trust_government, y = trust_media)
#'
#' # Weighted analysis
#' survey_data %>%
#'   wilcoxon_test(x = trust_government, y = trust_media,
#'                 weights = sampling_weight)
#'
#' # Grouped analysis (separate test per region)
#' survey_data %>%
#'   group_by(region) %>%
#'   wilcoxon_test(x = trust_government, y = trust_media)
#'
#' @family hypothesis_tests
#' @export
wilcoxon_test <- function(data, x, y, weights = NULL, conf.level = 0.95) {

  # Input validation
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }

  # Check if data is grouped
  is_grouped <- inherits(data, "grouped_df")
  grp_vars <- if (is_grouped) dplyr::group_vars(data) else NULL

  # Process x and y variables
  if (missing(x) || missing(y)) {
    cli_abort("Both {.arg x} and {.arg y} are required for the Wilcoxon signed-rank test.")
  }

  x_quo <- enquo(x)
  y_quo <- enquo(y)

  x_var <- eval_select(expr(!!x_quo), data = data)
  y_var <- eval_select(expr(!!y_quo), data = data)

  x_name <- names(x_var)[1]
  y_name <- names(y_var)[1]

  # Process weights using centralized helper
  weights_info <- .process_weights(data, rlang::enquo(weights))
  w_name <- weights_info$name

  # Helper function to perform Wilcoxon signed-rank test for a single pair
  perform_single_wilcoxon <- function(data, x_name, y_name, weight_name = NULL) {
    x_vals <- data[[x_name]]
    y_vals <- data[[y_name]]

    # Remove NA values (pairwise deletion)
    valid_indices <- !is.na(x_vals) & !is.na(y_vals)
    if (!is.null(weight_name)) {
      w <- data[[weight_name]]
      valid_indices <- valid_indices & !is.na(w)
      w <- w[valid_indices]
    }
    x_vals <- x_vals[valid_indices]
    y_vals <- y_vals[valid_indices]

    # Compute differences (y - x, matching SPSS convention)
    d <- y_vals - x_vals

    # Separate into ranks categories
    neg_idx <- d < 0   # y < x (decreased)
    pos_idx <- d > 0   # y > x (increased)
    tie_idx <- d == 0  # y == x (no change)

    n_neg <- sum(neg_idx)
    n_pos <- sum(pos_idx)
    n_ties <- sum(tie_idx)
    n_total <- length(d)

    # Remove ties for rank computation
    d_no_ties <- d[!tie_idx]
    n_ranked <- length(d_no_ties)

    if (n_ranked == 0) {
      return(list(
        Z = NA_real_, p_value = NA_real_, r_effect = NA_real_,
        n_neg = n_neg, n_pos = n_pos, n_ties = n_ties, n_total = n_total,
        mean_rank_neg = NA_real_, mean_rank_pos = NA_real_,
        sum_rank_neg = NA_real_, sum_rank_pos = NA_real_,
        V = NA_real_
      ))
    }

    if (is.null(weight_name)) {
      # ------------------------------------------------------------------
      # Unweighted Wilcoxon Signed-Rank Test
      # ------------------------------------------------------------------

      # Rank absolute differences
      abs_d <- abs(d_no_ties)
      ranks <- rank(abs_d)

      # Assign ranks to positive and negative groups
      pos_in_ranked <- d_no_ties > 0
      neg_in_ranked <- d_no_ties < 0

      sum_rank_pos <- sum(ranks[pos_in_ranked])
      sum_rank_neg <- sum(ranks[neg_in_ranked])

      mean_rank_pos <- if (n_pos > 0) sum_rank_pos / n_pos else NA_real_
      mean_rank_neg <- if (n_neg > 0) sum_rank_neg / n_neg else NA_real_

      V <- sum_rank_pos  # V = W+ (sum of positive ranks)

      # Z statistic (normal approximation with tie correction)
      # E(W+) = N*(N+1)/4
      # Var(W+) = N*(N+1)*(2N+1)/24 - tie_correction/48
      N <- n_ranked
      E_V <- N * (N + 1) / 4

      # Tie correction for variance
      tie_groups <- table(abs_d)
      tie_correction <- sum(tie_groups^3 - tie_groups)
      Var_V <- N * (N + 1) * (2 * N + 1) / 24 - tie_correction / 48

      if (Var_V > 0) {
        Z <- (V - E_V) / sqrt(Var_V)
      } else {
        Z <- 0
      }

      p_value <- 2 * pnorm(abs(Z), lower.tail = FALSE)

    } else {
      # ------------------------------------------------------------------
      # Weighted Wilcoxon Signed-Rank Test (frequency weights)
      # ------------------------------------------------------------------
      # NOTE: Substitutes sum(w) for n in the standard variance formula.
      # For integer frequency weights this reproduces the expanded-data
      # Wilcoxon statistic exactly (weights == 1 is identical to the
      # unweighted test); fractional weights are a continuous extension.
      # NOT a design-based estimator - for complex sampling designs use
      # the survey package instead.

      w_no_ties <- w[!tie_idx]

      # Shared core: frequency-expansion mid-ranks, V, E(V), tie-corrected
      # variance, Z and two-sided p (see .weighted_signed_rank_z in
      # helpers.R; also used by pairwise_wilcoxon so the math cannot drift)
      core <- .weighted_signed_rank_z(d_no_ties, w_no_ties)
      rankhat <- core$rankhat
      V <- core$V
      Z <- core$Z
      p_value <- core$p_value

      # Display statistics: weighted rank sums/means per sign group
      pos_in_ranked <- d_no_ties > 0
      neg_in_ranked <- d_no_ties < 0
      sum_rank_pos <- sum(w_no_ties[pos_in_ranked] * rankhat[pos_in_ranked])
      sum_rank_neg <- sum(w_no_ties[neg_in_ranked] * rankhat[neg_in_ranked])

      n_pos_w <- sum(w[pos_idx])
      n_neg_w <- sum(w[neg_idx])
      n_ties_w <- sum(w[tie_idx])

      mean_rank_pos <- if (n_pos_w > 0) sum_rank_pos / n_pos_w else NA_real_
      mean_rank_neg <- if (n_neg_w > 0) sum_rank_neg / n_neg_w else NA_real_

      n_neg <- round(n_neg_w)
      n_pos <- round(n_pos_w)
      n_ties <- round(n_ties_w)
      n_total <- round(sum(w))
      n_ranked <- round(core$N_pop)
    }

    # Effect size: r = Z / sqrt(N)
    r_effect <- abs(Z) / sqrt(n_ranked)

    return(list(
      Z = Z,
      p_value = p_value,
      r_effect = r_effect,
      n_neg = n_neg,
      n_pos = n_pos,
      n_ties = n_ties,
      n_total = n_total,
      mean_rank_neg = mean_rank_neg,
      mean_rank_pos = mean_rank_pos,
      sum_rank_neg = sum_rank_neg,
      sum_rank_pos = sum_rank_pos,
      V = V
    ))
  }

  # Main computation function
  compute_results <- function(data) {
    tryCatch({
      result <- perform_single_wilcoxon(data, x_name, y_name, w_name)

      tibble(
        pair = paste(y_name, "-", x_name),
        Z = result$Z,
        p_value = result$p_value,
        r_effect = result$r_effect,
        n_neg = result$n_neg,
        n_pos = result$n_pos,
        n_ties = result$n_ties,
        n_total = result$n_total,
        mean_rank_neg = result$mean_rank_neg,
        mean_rank_pos = result$mean_rank_pos,
        sum_rank_neg = result$sum_rank_neg,
        sum_rank_pos = result$sum_rank_pos,
        V = result$V
      )
    }, error = function(e) {
      cli_warn("Wilcoxon test failed: {e$message}")
      tibble(
        pair = paste(y_name, "-", x_name),
        Z = NA_real_, p_value = NA_real_, r_effect = NA_real_,
        n_neg = NA_integer_, n_pos = NA_integer_,
        n_ties = NA_integer_, n_total = NA_integer_,
        mean_rank_neg = NA_real_, mean_rank_pos = NA_real_,
        sum_rank_neg = NA_real_, sum_rank_pos = NA_real_,
        V = NA_real_
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
    x_name = x_name,
    y_name = y_name,
    weights = w_name,
    is_grouped = is_grouped,
    conf.level = conf.level
  )

  class(result) <- "wilcoxon_test"
  return(result)
}

# Helper: print the rank table and test statistics for one comparison
#' @noRd
.print_wt_block <- function(row_data, x_name, y_name, weights, digits,
                            show_ranks = TRUE, show_results = TRUE) {
  print_header(paste(y_name, "-", x_name), newline_before = FALSE)

  # Rank table (gated by ranks toggle)
  if (show_ranks) {
    cat("  Ranks:\n")
    rank_df <- data.frame(
      ` ` = c("Negative Ranks", "Positive Ranks", "Ties", "Total"),
      N = c(as.integer(row_data$n_neg), as.integer(row_data$n_pos),
            as.integer(row_data$n_ties), as.integer(row_data$n_total)),
      `Mean Rank` = c(round(row_data$mean_rank_neg, 2),
                      round(row_data$mean_rank_pos, 2),
                      NA, NA),
      `Sum of Ranks` = c(round(row_data$sum_rank_neg, 2),
                         round(row_data$sum_rank_pos, 2),
                         NA, NA),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    output <- capture.output(print(rank_df, row.names = FALSE, na.print = ""))
    border_width <- max(nchar(output), na.rm = TRUE)
    border <- paste(rep("-", border_width), collapse = "")

    cat("  ", border, "\n", sep = "")
    for (line in output) {
      cat("  ", line, "\n", sep = "")
    }
    cat("  ", border, "\n\n", sep = "")

    # Direction note
    cat(sprintf("  a %s < %s\n", y_name, x_name))
    cat(sprintf("  b %s > %s\n", y_name, x_name))
    cat(sprintf("  c %s = %s\n\n", y_name, x_name))
  }

  # Test statistics table (gated by results toggle)
  if (show_results) {
    test_df <- data.frame(
      Z = round(row_data$Z, digits),
      `p value` = round(row_data$p_value, digits),
      `Effect r` = round(row_data$r_effect, digits),
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

# Internal: compact one-line summary for a single Wilcoxon comparison
#' @noRd
.print_wt_compact <- function(results, i, pair_label, weighted_tag, digits) {
  Z_val <- results$Z[i]
  p_val <- as.numeric(results$p_value[i])
  r_val <- results$r_effect[i]

  cat(sprintf("Wilcoxon Signed-Rank Test: %s%s\n", pair_label, weighted_tag))

  if (!is.na(r_val)) {
    r_interp <- if (abs(r_val) < 0.1) "negligible"
                else if (abs(r_val) < 0.3) "small"
                else if (abs(r_val) < 0.5) "medium"
                else "large"
    cat(sprintf("  Z = %s, %s %s, r = %s (%s), N = %s\n",
                fmt_num(Z_val, digits),
                fmt_p(p_val, digits, style = "compact"),
                add_significance_stars(p_val),
                fmt_num(r_val, digits), r_interp,
                formatC(as.integer(results$n_total[i]), format = "d")))
  } else {
    cat(sprintf("  Z = %s, %s %s\n",
                fmt_num(Z_val, digits),
                fmt_p(p_val, digits, style = "compact"),
                add_significance_stars(p_val)))
  }
}

#' Print Wilcoxon signed-rank test results (compact)
#'
#' @description
#' Compact print method for objects of class \code{"wilcoxon_test"}.
#' Shows a one-line summary per comparison with the Z statistic, p-value,
#' effect size r, and sample size.
#'
#' For the full detailed output (rank tables, test statistics), use
#' \code{summary()}.
#'
#' @param x A wilcoxon_test object
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- wilcoxon_test(survey_data, x = trust_government, y = trust_media)
#' result              # compact one-line overview
#' summary(result)     # full detailed output
#'
#' @export
#' @method print wilcoxon_test
print.wilcoxon_test <- function(x, digits = 3, ...) {
  weighted_tag <- if (!is.null(x$weights)) " [Weighted]" else ""
  pair_label <- paste(x$y_name, "-", x$x_name)
  results <- x$results
  results$p_value <- as.numeric(results$p_value)

  if (isTRUE(x$is_grouped)) {
    group_vars <- setdiff(names(results), c("pair", "Z", "p_value", "r_effect",
                                            "n_neg", "n_pos", "n_ties",
                                            "n_total", "mean_rank_neg",
                                            "mean_rank_pos", "sum_rank_neg",
                                            "sum_rank_pos", "V"))
    groups <- unique(results[group_vars])

    for (i in seq_len(nrow(groups))) {
      group_values <- groups[i, , drop = FALSE]
      group_label <- paste(names(group_values), "=", group_values, collapse = ", ")
      cat(sprintf("[%s]\n", group_label))

      group_results <- results
      for (g in names(group_values)) {
        group_results <- group_results[group_results[[g]] == group_values[[g]], ]
      }
      if (nrow(group_results) == 0) next
      .print_wt_compact(group_results, 1, pair_label, weighted_tag, digits)
    }
  } else {
    .print_wt_compact(results, 1, pair_label, weighted_tag, digits)
  }

  cat("Use summary() for detailed output.\n")
  invisible(x)
}

#' Summary method for Wilcoxon signed-rank test results
#'
#' @description
#' Creates a summary object that produces detailed output when printed,
#' including the rank table (negative/positive ranks, ties) and the test
#' statistics table with Z, p-value, and effect size r.
#'
#' @param object A \code{wilcoxon_test} result object.
#' @param ranks Logical. Show the rank table? (Default: TRUE)
#' @param results Logical. Show test statistics table? (Default: TRUE)
#' @param digits Number of decimal places for formatting (Default: 3).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.wilcoxon_test} object.
#'
#' @examples
#' result <- wilcoxon_test(survey_data, x = trust_government, y = trust_media)
#' summary(result)
#' summary(result, ranks = FALSE)
#'
#' @seealso \code{\link{wilcoxon_test}} for the main analysis function.
#' @export
#' @method summary wilcoxon_test
summary.wilcoxon_test <- function(object, ranks = TRUE, results = TRUE,
                                  digits = 3, ...) {
  build_summary_object(
    object     = object,
    show       = list(ranks = ranks, results = results),
    digits     = digits,
    class_name = "summary.wilcoxon_test"
  )
}

#' Print summary of Wilcoxon signed-rank test results (detailed output)
#'
#' @description
#' Displays the detailed SPSS-style output for a Wilcoxon signed-rank test,
#' with sections controlled by the boolean parameters passed to
#' \code{\link{summary.wilcoxon_test}}.  Sections include the rank table
#' and the test statistics table with effect size interpretation.
#'
#' @param x A \code{summary.wilcoxon_test} object created by
#'   \code{\link{summary.wilcoxon_test}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- wilcoxon_test(survey_data, x = trust_government, y = trust_media)
#' summary(result)                # all sections
#' summary(result, ranks = FALSE) # hide rank table
#'
#' @seealso \code{\link{wilcoxon_test}} for the main analysis,
#'   \code{\link{summary.wilcoxon_test}} for summary options.
#' @export
#' @method print summary.wilcoxon_test
print.summary.wilcoxon_test <- function(x, ...) {
  digits <- x$digits

  # Determine test type using standardized helper
  weights_name <- x$weights
  test_type <- get_standard_title("Wilcoxon Signed-Rank Test", weights_name, "Results")
  print_header(test_type, newline_before = FALSE)

  # Ensure p-values are numeric
  x$results$p_value <- as.numeric(x$results$p_value)

  # Add significance stars
  x$results$sig <- sapply(x$results$p_value, add_significance_stars)

  # Resolve show toggles
  show_ranks   <- isTRUE(x$show$ranks)
  show_results <- isTRUE(x$show$results)

  is_grouped_data <- isTRUE(x$is_grouped)

  # Print info section
  cat("\n")
  test_info <- list(
    "Pair" = paste(x$y_name, "vs", x$x_name),
    "Weights variable" = weights_name
  )
  print_info_section(test_info)
  cat("\n")

  if (is_grouped_data) {
    group_vars <- setdiff(names(x$results), c("pair", "Z", "p_value", "r_effect",
                                                "n_neg", "n_pos", "n_ties",
                                                "n_total", "mean_rank_neg",
                                                "mean_rank_pos", "sum_rank_neg",
                                                "sum_rank_pos", "V", "sig"))
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

      .print_wt_block(
        row_data = group_results[1, ],
        x_name = x$x_name,
        y_name = x$y_name,
        weights = x$weights,
        digits = digits,
        show_ranks = show_ranks,
        show_results = show_results
      )
    }
  } else {
    .print_wt_block(
      row_data = x$results[1, ],
      x_name = x$x_name,
      y_name = x$y_name,
      weights = x$weights,
      digits = digits,
      show_ranks = show_ranks,
      show_results = show_results
    )
  }

  if (!is.null(x$weights) && (show_ranks || show_results)) {
    cat("Note: Weighted analysis uses frequency-weighted ranks.\n")
  }

  if (show_results) {
    print_significance_legend()

    cat("\nEffect Size Interpretation (r):\n")
    cat("- Small effect: 0.1 - 0.3\n")
    cat("- Medium effect: 0.3 - 0.5\n")
    cat("- Large effect: > 0.5\n")
  }

  invisible(x)
}
