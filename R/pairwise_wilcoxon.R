
#' Find Which Specific Measurements Differ After Friedman Test
#'
#' @description
#' \code{pairwise_wilcoxon()} tells you exactly which pairs of measurements differ
#' from each other after the Friedman test finds overall differences. It performs
#' all pairwise Wilcoxon signed-rank tests with p-value correction.
#'
#' Think of it as:
#' - Friedman says "there are differences somewhere among the measurements"
#' - Pairwise Wilcoxon says "specifically, Measurement A differs from Measurement C"
#' - A way to make all possible pairwise comparisons for repeated measures
#'
#' @param x Friedman test results from \code{friedman_test()}
#' @param ... Additional arguments passed to methods. The method for
#'   \code{friedman_test} objects accepts \code{p_adjust} (character):
#'   method for adjusting p-values for multiple comparisons.
#'   Options: \code{"bonferroni"} (default, most conservative),
#'   \code{"holm"}, \code{"BH"}, \code{"hochberg"}, \code{"hommel"},
#'   \code{"BY"}, \code{"fdr"}, \code{"none"}.
#'
#' @return Pairwise comparison results showing:
#' - Which measurement pairs are significantly different
#' - Z-statistics from Wilcoxon signed-rank tests
#' - Adjusted p-values (controlling for multiple comparisons)
#'
#' @details
#' ## Understanding the Results
#'
#' **Z-Statistics**: Based on the Wilcoxon signed-rank test for each pair
#' - Large absolute Z values indicate big differences between two measurements
#' - Positive Z: Values in var1 tend to be higher than var2
#' - Negative Z: Values in var2 tend to be higher than var1
#'
#' **Adjusted P-values**: Control for multiple comparisons
#' - p < 0.05: Measurements are significantly different
#' - p >= 0.05: No significant difference between these measurements
#'
#' ## The Wilcoxon Signed-Rank Test
#'
#' For each pair of measurements, the Wilcoxon signed-rank test:
#' 1. Computes differences between the two measurements
#' 2. Ranks the absolute differences
#' 3. Computes a Z-statistic based on the rank sums
#' 4. Uses normal approximation with tie correction
#'
#' ## P-Value Adjustment Methods
#'
#' - **Bonferroni** (default): Most conservative, multiplies p by number of comparisons
#' - **Holm**: Step-down method, less conservative than Bonferroni
#' - **BH**: Controls false discovery rate, good for many comparisons
#'
#' ## When to Use This
#'
#' Use pairwise Wilcoxon when:
#' - Your Friedman test shows significant differences (p < 0.05)
#' - You want to know which specific measurements differ
#' - Your data are ordinal or violate normality assumptions
#' - You have repeated measures or matched groups
#'
#' ## Relationship to Other Tests
#'
#' - Non-parametric post-hoc for repeated measures (like Dunn is for
#'   independent groups)
#' - Follow-up to \code{\link{friedman_test}()}, like \code{\link{dunn_test}()}
#'   follows \code{\link{kruskal_wallis}()}
#' - Each pairwise comparison uses \code{\link{wilcoxon_test}()} logic internally
#'
#' @seealso
#' \code{\link{friedman_test}} for performing Friedman tests.
#'
#' \code{\link{wilcoxon_test}} for individual paired Wilcoxon tests.
#'
#' \code{\link{dunn_test}} for post-hoc comparisons after Kruskal-Wallis.
#'
#' @references
#' Wilcoxon, F. (1945). Individual comparisons by ranking methods. Biometrics
#' Bulletin, 1(6), 80-83.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Perform Friedman followed by pairwise Wilcoxon post-hoc
#' friedman_result <- survey_data %>%
#'   friedman_test(trust_government, trust_media, trust_science)
#'
#' # Pairwise Wilcoxon comparisons (default: Bonferroni)
#' friedman_result %>% pairwise_wilcoxon()
#'
#' # With Holm correction (less conservative)
#' friedman_result %>% pairwise_wilcoxon(p_adjust = "holm")
#'
#' # With Benjamini-Hochberg (controls false discovery rate)
#' friedman_result %>% pairwise_wilcoxon(p_adjust = "BH")
#'
#' # With weights
#' fw_weighted <- survey_data %>%
#'   friedman_test(trust_government, trust_media, trust_science,
#'                 weights = sampling_weight)
#'
#' fw_weighted %>% pairwise_wilcoxon()
#'
#' # Grouped analysis
#' fw_grouped <- survey_data %>%
#'   group_by(region) %>%
#'   friedman_test(trust_government, trust_media, trust_science)
#'
#' fw_grouped %>% pairwise_wilcoxon()
#'
#' @family posthoc
#' @export
pairwise_wilcoxon <- function(x, ...) {
  UseMethod("pairwise_wilcoxon")
}

#' @rdname pairwise_wilcoxon
#' @export
pairwise_wilcoxon.default <- function(x, ...) {
  cls <- paste(class(x), collapse = "/")
  cli_abort(c(
    "{.fn pairwise_wilcoxon} is not available for objects of class {.cls {cls}}.",
    "i" = "Pairwise Wilcoxon requires results from {.fn friedman_test}.",
    "i" = "Example: {.code friedman_test(data, var1, var2, var3) |> pairwise_wilcoxon()}"
  ))
}

#' @export
pairwise_wilcoxon.friedman_test <- function(x, p_adjust = "bonferroni", ...) {

  # Input validation
  valid_methods <- c("bonferroni", "holm", "BH", "hochberg",
                     "hommel", "BY", "fdr", "none")
  if (!p_adjust %in% valid_methods) {
    cli_abort(c(
      "{.arg p_adjust} must be one of {.or {.val {valid_methods}}}.",
      "x" = "Got {.val {p_adjust}}."
    ))
  }

  # Helper function to perform a single paired Wilcoxon signed-rank test
  # Reuses the exact algorithm from wilcoxon_test.R
  perform_single_pw_wilcoxon <- function(data, var1_name, var2_name,
                                         weight_name = NULL) {
    x_vals <- data[[var1_name]]
    y_vals <- data[[var2_name]]

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

    # Separate into rank categories
    neg_idx <- d < 0
    pos_idx <- d > 0
    tie_idx <- d == 0

    n_total <- length(d)

    # Remove ties for rank computation
    d_no_ties <- d[!tie_idx]
    n_ranked <- length(d_no_ties)

    if (n_ranked == 0) {
      return(list(z = NA_real_, p = NA_real_))
    }

    if (is.null(weight_name)) {
      # ----------------------------------------------------------------
      # Unweighted Wilcoxon Signed-Rank Test
      # ----------------------------------------------------------------

      # Rank absolute differences
      abs_d <- abs(d_no_ties)
      ranks <- rank(abs_d)

      # Assign ranks to positive and negative groups
      pos_in_ranked <- d_no_ties > 0

      sum_rank_pos <- sum(ranks[pos_in_ranked])
      V <- sum_rank_pos

      # Z statistic (normal approximation with tie correction)
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
      # ----------------------------------------------------------------
      # Weighted Wilcoxon Signed-Rank Test
      # ----------------------------------------------------------------

      w_no_ties <- w[!tie_idx]

      # Weighted mid-ranks of absolute differences
      abs_d <- abs(d_no_ties)
      ii <- order(abs_d)
      rankhat <- numeric(length(abs_d))
      rankhat[ii] <- ave(cumsum(w_no_ties[ii]) - w_no_ties[ii] / 2,
                         factor(abs_d[ii]))

      # Assign ranks to positive and negative groups
      pos_in_ranked <- d_no_ties > 0

      # Weighted rank sums
      sum_rank_pos <- sum(w_no_ties[pos_in_ranked] * rankhat[pos_in_ranked])

      V <- sum_rank_pos
      N_pop <- sum(w_no_ties)

      E_V <- N_pop * (N_pop + 1) / 4

      tie_groups_w <- tapply(w_no_ties, factor(abs_d), sum)
      tie_correction <- sum(tie_groups_w^3 - tie_groups_w)
      Var_V <- N_pop * (N_pop + 1) * (2 * N_pop + 1) / 24 -
        tie_correction / 48

      if (Var_V > 0) {
        Z <- (V - E_V) / sqrt(Var_V)
      } else {
        Z <- 0
      }

      p_value <- 2 * pnorm(abs(Z), lower.tail = FALSE)
    }

    return(list(z = Z, p = p_value))
  }

  # Generate all pairs of variables
  vars <- x$variables
  pairs <- combn(seq_along(vars), 2)
  n_total_pairs <- ncol(pairs)

  # Main execution logic
  all_results <- list()

  if (x$is_grouped) {
    # Get original data and split by groups
    data <- x$data
    data_list <- dplyr::group_split(data)
    group_keys_df <- dplyr::group_keys(data)

    for (i in seq_along(data_list)) {
      group_data <- data_list[[i]]
      group_info <- group_keys_df[i, , drop = FALSE]

      # Compute all pairwise Wilcoxon tests for this group
      pair_results <- vector("list", n_total_pairs)

      for (idx in seq_len(n_total_pairs)) {
        v1 <- pairs[1, idx]
        v2 <- pairs[2, idx]

        tryCatch({
          result <- perform_single_pw_wilcoxon(
            group_data, vars[v1], vars[v2], x$weights
          )

          pair_results[[idx]] <- data.frame(
            var1 = vars[v1],
            var2 = vars[v2],
            z = result$z,
            p = result$p,
            stringsAsFactors = FALSE
          )
        }, error = function(e) {
          # Skip on error
        })
      }

      pair_df <- do.call(rbind, pair_results[!sapply(pair_results, is.null)])

      if (!is.null(pair_df) && nrow(pair_df) > 0) {
        # Adjust p-values within this group
        pair_df$p_adj <- p.adjust(pair_df$p, method = p_adjust)

        # Add group information
        n_rows <- nrow(pair_df)
        group_info_expanded <- group_info[rep(1, n_rows), , drop = FALSE]
        rownames(group_info_expanded) <- NULL
        result_with_groups <- cbind(group_info_expanded, pair_df)
        all_results <- append(all_results, list(result_with_groups))
      }
    }

  } else {
    # Ungrouped: compute all pairwise Wilcoxon tests
    pair_results <- vector("list", n_total_pairs)

    for (idx in seq_len(n_total_pairs)) {
      v1 <- pairs[1, idx]
      v2 <- pairs[2, idx]

      tryCatch({
        result <- perform_single_pw_wilcoxon(
          x$data, vars[v1], vars[v2], x$weights
        )

        pair_results[[idx]] <- data.frame(
          var1 = vars[v1],
          var2 = vars[v2],
          z = result$z,
          p = result$p,
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        # Skip on error
      })
    }

    pair_df <- do.call(rbind, pair_results[!sapply(pair_results, is.null)])

    if (!is.null(pair_df) && nrow(pair_df) > 0) {
      # Adjust p-values
      pair_df$p_adj <- p.adjust(pair_df$p, method = p_adjust)
      all_results <- append(all_results, list(pair_df))
    }
  }

  # Combine all results
  if (length(all_results) > 0) {
    results_df <- do.call(rbind, all_results)
    rownames(results_df) <- NULL
  } else {
    results_df <- data.frame()
  }

  # Number of pairwise comparisons (per group)
  n_comps <- n_total_pairs

  # Create S3 object
  structure(
    list(
      comparisons = results_df,
      variables = x$variables,
      weights = x$weights,
      groups = if (x$is_grouped) dplyr::group_vars(x$data) else NULL,
      is_grouped = x$is_grouped,
      p_adjust_method = p_adjust,
      n_comparisons = as.integer(n_comps),
      friedman_results = x
    ),
    class = "pairwise_wilcoxon"
  )
}

#' Print pairwise Wilcoxon post-hoc test results
#'
#' @description
#' Print method for objects of class \code{"pairwise_wilcoxon"}. Provides a
#' formatted display of pairwise Wilcoxon comparison results including
#' Z-statistics and adjusted p-values.
#'
#' @param x An object of class \code{"pairwise_wilcoxon"} returned by
#'   \code{\link{pairwise_wilcoxon}}.
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments passed to \code{\link[base]{print}}.
#'   Currently unused.
#'
#' @details
#' The print method displays:
#' \itemize{
#'   \item Pairwise measurement comparisons with Z-statistics
#'   \item Adjusted p-values controlling for multiple comparisons
#'   \item Significance indicators (* p < 0.05, ** p < 0.01, *** p < 0.001)
#' }
#'
#' For grouped analyses, results are displayed separately for each group.
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @export
print.pairwise_wilcoxon <- function(x, digits = 3, ...) {
  # Determine test type using standardized helper
  weights_name <- x$weights
  adjust_label <- switch(x$p_adjust_method,
    "bonferroni" = "Bonferroni",
    "holm"       = "Holm",
    "BH"         = "Benjamini-Hochberg",
    "fdr"        = "Benjamini-Hochberg",
    "hochberg"   = "Hochberg",
    "hommel"     = "Hommel",
    "BY"         = "Benjamini-Yekutieli",
    "none"       = "None",
    x$p_adjust_method
  )

  title_name <- paste0("Pairwise Wilcoxon Post-Hoc Test (", adjust_label, ")")
  test_type <- get_standard_title(title_name, weights_name, "Results")
  print_header(test_type, newline_before = FALSE)

  # Print basic info using standardized helpers
  cat("\n")
  test_info <- list(
    "Variables" = paste(x$variables, collapse = ", "),
    "Weights variable" = x$weights,
    "P-value adjustment" = adjust_label,
    "Number of comparisons" = as.character(x$n_comparisons)
  )
  print_info_section(test_info)
  cat("\n")

  if (nrow(x$comparisons) == 0) {
    cat("No post-hoc comparisons available.\n")
    return(invisible(x))
  }

  # Add significance stars
  x$comparisons$sig <- sapply(x$comparisons$p_adj, add_significance_stars)

  is_grouped_data <- isTRUE(x$is_grouped)

  if (is_grouped_data) {
    # Get unique groups
    groups <- unique(x$comparisons[x$groups])

    for (i in seq_len(nrow(groups))) {
      group_values <- groups[i, , drop = FALSE]

      # Print group header using standardized helper
      print_group_header(group_values)

      # Filter results for current group
      group_results <- x$comparisons
      for (g in names(group_values)) {
        group_results <- group_results[group_results[[g]] == group_values[[g]], ]
      }

      if (nrow(group_results) == 0) next

      .print_pw_table(group_results, digits)
    }
  } else {
    .print_pw_table(x$comparisons, digits)
  }

  print_significance_legend()

  cat("\nInterpretation:\n")
  cat("- Positive Z: First variable tends to have higher values\n")
  cat("- Negative Z: Second variable tends to have higher values\n")
  cat("- p-values are adjusted for multiple comparisons\n")

  invisible(x)
}

#' Print a pairwise Wilcoxon comparison table
#' @param pw_results Data frame with comparison results
#' @param digits Number of decimal places
#' @keywords internal
.print_pw_table <- function(pw_results, digits = 3) {
  display_table <- data.frame(
    `Var 1` = pw_results$var1,
    `Var 2` = pw_results$var2,
    Z = round(pw_results$z, digits),
    `p (unadj)` = ifelse(pw_results$p < 0.001,
                          "<.001",
                          format(round(pw_results$p, digits), nsmall = digits)),
    `p (adj)` = ifelse(pw_results$p_adj < 0.001,
                        "<.001",
                        format(round(pw_results$p_adj, digits), nsmall = digits)),
    Sig = pw_results$sig,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # Calculate border width based on table content
  output <- capture.output(print(display_table, row.names = FALSE))
  border_width <- max(nchar(output), na.rm = TRUE)
  border <- paste(rep("-", border_width), collapse = "")

  cat(border, "\n")
  for (line in output) {
    cat(line, "\n")
  }
  cat(border, "\n\n")
}
