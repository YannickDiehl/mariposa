
#' Find Which Specific Groups Differ After Kruskal-Wallis
#'
#' @description
#' \code{dunn_test()} tells you exactly which groups are different from each other
#' after Kruskal-Wallis finds overall differences. It's the non-parametric
#' equivalent of a Tukey or Scheffe post-hoc test.
#'
#' Think of it as:
#' - Kruskal-Wallis says "there are differences somewhere"
#' - Dunn test says "specifically, Group A differs from Group C"
#' - A way to make all possible pairwise comparisons using rank-based statistics
#'
#' @param x Kruskal-Wallis results from \code{kruskal_wallis()}
#' @param ... Additional arguments passed to methods. The method for
#'   \code{kruskal_wallis} objects accepts \code{p_adjust} (character):
#'   method for adjusting p-values for multiple comparisons.
#'   Options: \code{"bonferroni"} (default, most conservative),
#'   \code{"holm"}, \code{"BH"}, \code{"hochberg"}, \code{"hommel"},
#'   \code{"BY"}, \code{"fdr"}, \code{"none"}.
#'
#' @return Pairwise comparison results showing:
#' - Which group pairs are significantly different
#' - Z-statistics based on rank differences
#' - Adjusted p-values (controlling for multiple comparisons)
#'
#' @details
#' ## Understanding the Results
#'
#' **Z-Statistics**: Based on differences in mean ranks between groups
#' - Large absolute Z values indicate big rank differences
#' - Positive Z: First group has higher mean rank than second
#' - Negative Z: Second group has higher mean rank than first
#'
#' **Adjusted P-values**: Control for multiple comparisons
#' - p < 0.05: Groups are significantly different
#' - p >= 0.05: No significant difference between these groups
#'
#' ## The Dunn Test Formula
#'
#' For each pair of groups (i, j):
#' \deqn{Z_{ij} = \frac{\bar{R}_i - \bar{R}_j}{\sqrt{\frac{N(N+1)}{12}
#'   \left(\frac{1}{n_i} + \frac{1}{n_j}\right)}}}
#'
#' where \eqn{\bar{R}_i} is the mean rank for group i, \eqn{N} is the total
#' sample size, and \eqn{n_i} is the size of group i.
#'
#' ## P-Value Adjustment Methods
#'
#' - **Bonferroni** (default): Most conservative, multiplies p by number of comparisons
#' - **Holm**: Step-down method, less conservative than Bonferroni
#' - **BH**: Controls false discovery rate, good for many comparisons
#'
#' ## When to Use This
#'
#' Use Dunn test when:
#' - Your Kruskal-Wallis test shows significant differences (p < 0.05)
#' - You want to know which specific groups differ
#' - Your data violates normality assumptions
#' - You have ordinal data or skewed distributions
#'
#' ## Relationship to Other Tests
#'
#' - Non-parametric equivalent of \code{\link{tukey_test}()} and
#'   \code{\link{scheffe_test}()}
#' - Follow-up to \code{\link{kruskal_wallis}()}, just like Tukey follows ANOVA
#' - Uses ranks instead of raw values, making it robust to outliers
#'
#' @seealso
#' \code{\link{kruskal_wallis}} for performing Kruskal-Wallis tests.
#'
#' \code{\link{tukey_test}} for parametric post-hoc comparisons after ANOVA.
#'
#' \code{\link{scheffe_test}} for conservative parametric post-hoc comparisons.
#'
#' @references
#' Dunn, O. J. (1964). Multiple comparisons using rank sums. Technometrics,
#' 6(3), 241-252.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Perform Kruskal-Wallis followed by Dunn post-hoc test
#' kw_result <- survey_data %>%
#'   kruskal_wallis(life_satisfaction, group = education)
#'
#' # Dunn post-hoc comparisons (default: Bonferroni)
#' kw_result %>% dunn_test()
#'
#' # With Holm correction (less conservative)
#' kw_result %>% dunn_test(p_adjust = "holm")
#'
#' # With Benjamini-Hochberg (controls false discovery rate)
#' kw_result %>% dunn_test(p_adjust = "BH")
#'
#' # With weights
#' kw_weighted <- survey_data %>%
#'   kruskal_wallis(life_satisfaction, group = education,
#'                  weights = sampling_weight)
#'
#' kw_weighted %>% dunn_test()
#'
#' # Multiple variables
#' kw_multi <- survey_data %>%
#'   kruskal_wallis(life_satisfaction, trust_government,
#'                  group = education)
#'
#' kw_multi %>% dunn_test()
#'
#' # Grouped analysis
#' kw_grouped <- survey_data %>%
#'   group_by(region) %>%
#'   kruskal_wallis(life_satisfaction, group = education)
#'
#' kw_grouped %>% dunn_test()
#'
#' @family posthoc
#' @export
dunn_test <- function(x, ...) {
  UseMethod("dunn_test")
}

#' @rdname dunn_test
#' @export
dunn_test.default <- function(x, ...) {
  cls <- paste(class(x), collapse = "/")
  cli_abort(c(
    "{.fn dunn_test} is not available for objects of class {.cls {cls}}.",
    "i" = "Dunn's test requires results from {.fn kruskal_wallis}.",
    "i" = "Example: {.code kruskal_wallis(data, dv, group) |> dunn_test()}"
  ))
}

#' @export
dunn_test.kruskal_wallis <- function(x, p_adjust = "bonferroni", ...) {

  # Input validation
  valid_methods <- c("bonferroni", "holm", "BH", "hochberg",
                     "hommel", "BY", "fdr", "none")
  if (!p_adjust %in% valid_methods) {
    cli_abort(c(
      "{.arg p_adjust} must be one of {.or {.val {valid_methods}}}.",
      "x" = "Got {.val {p_adjust}}."
    ))
  }

  # Helper function to perform Dunn test for a single variable
  perform_single_dunn <- function(data, var_name, group_name,
                                  weight_name = NULL, p_adjust_method) {
    # Get variable values
    y <- data[[var_name]]
    g <- data[[group_name]]

    # Remove NA values
    valid_indices <- !is.na(y) & !is.na(g)
    if (!is.null(weight_name)) {
      w <- data[[weight_name]]
      valid_indices <- valid_indices & !is.na(w) & w > 0
      w <- w[valid_indices]
    }
    y <- y[valid_indices]
    g <- g[valid_indices]

    # Get group levels
    if (is.factor(g)) {
      group_levels <- levels(g)[levels(g) %in% unique(as.character(g))]
    } else {
      group_levels <- sort(unique(as.character(g)))
    }

    if (length(group_levels) < 2) {
      return(NULL)
    }

    g <- as.character(g)

    if (is.null(weight_name)) {
      # ------------------------------------------------------------------
      # Unweighted Dunn test
      # ------------------------------------------------------------------
      N <- length(y)
      all_ranks <- rank(y, ties.method = "average")

      # Group-level statistics
      group_n <- numeric(length(group_levels))
      group_mean_rank <- numeric(length(group_levels))
      names(group_n) <- group_levels
      names(group_mean_rank) <- group_levels

      for (k in seq_along(group_levels)) {
        idx <- g == group_levels[k]
        group_n[k] <- sum(idx)
        group_mean_rank[k] <- mean(all_ranks[idx])
      }

    } else {
      # ------------------------------------------------------------------
      # Weighted Dunn test (matching kruskal_wallis.R weighted mid-ranks)
      # ------------------------------------------------------------------
      N <- sum(w)

      # Weighted mid-ranks (same approach as kruskal_wallis.R)
      ii <- order(y)
      rankhat <- numeric(length(y))
      rankhat[ii] <- ave(cumsum(w[ii]) - w[ii] / 2, factor(y[ii]))

      # Group-level statistics (weighted)
      group_n <- numeric(length(group_levels))
      group_mean_rank <- numeric(length(group_levels))
      names(group_n) <- group_levels
      names(group_mean_rank) <- group_levels

      for (k in seq_along(group_levels)) {
        idx <- g == group_levels[k]
        w_grp <- w[idx]
        group_n[k] <- sum(w_grp)
        group_mean_rank[k] <- sum(w_grp * rankhat[idx]) / sum(w_grp)
      }
    }

    # Generate all pairwise comparisons
    pairs <- combn(seq_along(group_levels), 2)
    n_pairs <- ncol(pairs)

    results_list <- vector("list", n_pairs)

    for (idx in seq_len(n_pairs)) {
      i <- pairs[1, idx]
      j <- pairs[2, idx]

      # Z-statistic
      diff <- group_mean_rank[i] - group_mean_rank[j]
      se <- sqrt((N * (N + 1) / 12) * (1 / group_n[i] + 1 / group_n[j]))
      z <- diff / se

      # Unadjusted p-value (two-sided)
      p_unadj <- 2 * pnorm(-abs(z))

      results_list[[idx]] <- data.frame(
        Variable = var_name,
        group1 = group_levels[i],
        group2 = group_levels[j],
        z = z,
        p = p_unadj,
        stringsAsFactors = FALSE
      )
    }

    result_df <- do.call(rbind, results_list)

    # Adjust p-values using stats::p.adjust
    result_df$p_adj <- p.adjust(result_df$p, method = p_adjust_method)

    return(result_df)
  }

  # Main execution logic
  all_results <- list()

  if (x$is_grouped) {
    # Get original data and split by groups
    data <- x$data
    data_list <- dplyr::group_split(data)
    group_keys <- dplyr::group_keys(data)

    # Perform Dunn test for each group
    for (i in seq_along(data_list)) {
      group_data <- data_list[[i]]
      group_info <- group_keys[i, , drop = FALSE]

      # Perform Dunn for each variable in this group
      for (var_name in x$variables) {
        tryCatch({
          dunn_result <- perform_single_dunn(
            group_data, var_name, x$group, x$weights, p_adjust
          )

          if (!is.null(dunn_result)) {
            # Add group information
            n_rows <- nrow(dunn_result)
            group_info_expanded <- group_info[rep(1, n_rows), , drop = FALSE]
            rownames(group_info_expanded) <- NULL
            result_with_groups <- cbind(group_info_expanded, dunn_result)
            all_results <- append(all_results, list(result_with_groups))
          }
        }, error = function(e) {
          # Skip this combination if error occurs
        })
      }
    }

  } else {
    # Perform Dunn for each variable (ungrouped)
    for (var_name in x$variables) {
      tryCatch({
        dunn_result <- perform_single_dunn(
          x$data, var_name, x$group, x$weights, p_adjust
        )

        if (!is.null(dunn_result)) {
          all_results <- append(all_results, list(dunn_result))
        }
      }, error = function(e) {
        # Skip this variable if error occurs
      })
    }
  }

  # Combine all results
  if (length(all_results) > 0) {
    results_df <- do.call(rbind, all_results)
    rownames(results_df) <- NULL
  } else {
    results_df <- data.frame()
  }

  # Determine n_comparisons (per variable for the first variable)
  n_comps <- if (nrow(results_df) > 0) {
    first_var <- results_df$Variable[1]
    if (x$is_grouped) {
      # Per group per variable
      grp_cols <- dplyr::group_vars(x$data)
      first_grp <- results_df[1, grp_cols, drop = FALSE]
      sub <- results_df[results_df$Variable == first_var, ]
      for (gc in grp_cols) {
        sub <- sub[sub[[gc]] == first_grp[[gc]], ]
      }
      nrow(sub)
    } else {
      sum(results_df$Variable == first_var)
    }
  } else {
    0L
  }

  # Create S3 object
  structure(
    list(
      comparisons = results_df,
      variables = x$variables,
      group = x$group,
      weights = x$weights,
      groups = if (x$is_grouped) dplyr::group_vars(x$data) else NULL,
      is_grouped = x$is_grouped,
      p_adjust_method = p_adjust,
      n_comparisons = as.integer(n_comps),
      kruskal_wallis_results = x
    ),
    class = "dunn_test"
  )
}

#' Print Dunn post-hoc test results
#'
#' @description
#' Print method for objects of class \code{"dunn_test"}. Provides a
#' formatted display of Dunn pairwise comparison results including
#' Z-statistics and adjusted p-values.
#'
#' @param x An object of class \code{"dunn_test"} returned by \code{\link{dunn_test}}.
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments passed to \code{\link[base]{print}}. Currently unused.
#'
#' @details
#' The print method displays:
#' \itemize{
#'   \item Pairwise group comparisons with Z-statistics
#'   \item Adjusted p-values controlling for multiple comparisons
#'   \item Significance indicators (* p < 0.05, ** p < 0.01, *** p < 0.001)
#' }
#'
#' For grouped analyses, results are displayed separately for each group combination.
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @export
print.dunn_test <- function(x, digits = 3, ...) {
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

  title_name <- paste0("Dunn Post-Hoc Test (", adjust_label, ")")
  test_type <- get_standard_title(title_name, weights_name, "Results")
  print_header(test_type, newline_before = FALSE)

  # Print basic info using standardized helpers
  cat("\n")
  test_info <- list(
    "Dependent variable" = if (!x$is_grouped && length(x$variables) == 1) {
      x$variables[1]
    } else {
      NULL
    },
    "Grouping variable" = x$group,
    "Weights variable" = x$weights,
    "P-value adjustment" = adjust_label
  )
  print_info_section(test_info)
  cat("\n")

  if (nrow(x$comparisons) == 0) {
    cat("No post-hoc comparisons available.\n")
    return(invisible(x))
  }

  # Add significance stars
  x$comparisons$sig <- sapply(x$comparisons$p_adj, add_significance_stars)

  # Template Standard: Dual grouped data detection
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

      # Print each variable as separate block
      for (var in x$variables) {
        var_results <- group_results[group_results$Variable == var, ]
        if (nrow(var_results) == 0) next

        cat(sprintf("\n--- %s ---\n\n", var))

        .print_dunn_table(var_results, digits)
      }
    }
  } else {
    # Print results for ungrouped data
    for (var in x$variables) {
      var_results <- x$comparisons[x$comparisons$Variable == var, ]
      if (nrow(var_results) == 0) next

      if (length(x$variables) > 1) {
        cat(sprintf("\n--- %s ---\n\n", var))
      }

      .print_dunn_table(var_results, digits)
    }
  }

  print_significance_legend()

  cat("\nInterpretation:\n")
  cat("- Positive Z: First group has higher mean rank\n")
  cat("- Negative Z: First group has lower mean rank\n")
  cat("- p-values are adjusted for multiple comparisons\n")

  invisible(x)
}

#' Print a Dunn test comparison table
#' @param var_results Data frame with comparison results for one variable
#' @param digits Number of decimal places
#' @keywords internal
.print_dunn_table <- function(var_results, digits = 3) {
  display_table <- data.frame(
    `Group 1` = var_results$group1,
    `Group 2` = var_results$group2,
    Z = round(var_results$z, digits),
    `p (unadj)` = ifelse(var_results$p < 0.001,
                          "<.001",
                          format(round(var_results$p, digits), nsmall = digits)),
    `p (adj)` = ifelse(var_results$p_adj < 0.001,
                        "<.001",
                        format(round(var_results$p_adj, digits), nsmall = digits)),
    Sig = var_results$sig,
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
