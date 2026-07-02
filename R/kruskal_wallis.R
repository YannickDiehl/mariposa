
#' Compare Multiple Groups Without Assuming Normal Data
#'
#' @description
#' \code{kruskal_wallis()} compares three or more groups when your data isn't
#' normally distributed or when you have ordinal data (like ratings or rankings).
#' It's the non-parametric alternative to one-way ANOVA.
#'
#' Think of it as:
#' - An extension of the Mann-Whitney test for more than two groups
#' - A robust way to compare groups that works with any data shape
#' - Perfect for Likert scales, rankings, or skewed distributions
#'
#' The test tells you:
#' - Whether at least one group is different from the others
#' - How strong the overall group effect is (effect size)
#' - Which groups tend to have higher or lower values (via mean ranks)
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The variables you want to compare between groups. You can list
#'   multiple variables or use helpers like \code{starts_with("satisfaction")}
#' @param group The categorical variable that defines your groups (e.g., education,
#'   employment). Must have at least 2 groups (3+ for meaningful use).
#' @param weights Optional survey weights for population-representative results
#' @param conf.level Confidence level for intervals (Default: 0.95 = 95%)
#'
#' @return Test results showing whether groups differ, including:
#' - H statistic (Kruskal-Wallis chi-square test statistic)
#' - Degrees of freedom (number of groups minus 1)
#' - P-value (are groups different?)
#' - Effect size epsilon-squared (how big is the group effect?)
#' - Mean rank for each group (which groups are higher/lower?)
#'
#' @details
#' ## Understanding the Results
#'
#' **P-value**: If p < 0.05, at least one group is significantly different
#' - p < 0.001: Very strong evidence of group differences
#' - p < 0.01: Strong evidence of group differences
#' - p < 0.05: Moderate evidence of group differences
#' - p > 0.05: No significant group differences found
#'
#' **Effect Size Epsilon-squared** (How much do groups matter?):
#' - < 0.01: Negligible effect
#' - 0.01-0.06: Small effect
#' - 0.06-0.14: Medium effect
#' - 0.14 or higher: Large effect
#'
#' **Mean Ranks**:
#' - Higher mean rank = group tends to have higher values
#' - Lower mean rank = group tends to have lower values
#' - Compare mean ranks to see the pattern of group differences
#'
#' ## When to Use This
#'
#' Use Kruskal-Wallis test when:
#' - Your data is not normally distributed (skewed, outliers)
#' - You have ordinal data (rankings, Likert scales)
#' - Sample sizes are small or very unequal across groups
#' - You want a robust alternative to one-way ANOVA
#' - You're comparing satisfaction ratings, income, or other skewed variables
#'
#' ## What Comes Next?
#'
#' If the Kruskal-Wallis test is significant:
#' 1. Look at mean ranks to see the pattern
#' 2. Use pairwise Mann-Whitney tests with Bonferroni correction to find
#'    which specific groups differ
#' 3. Consider effect sizes to judge practical importance
#'
#' ## Relationship to Other Tests
#'
#' - For 2 groups: Use \code{\link{mann_whitney}()} instead
#' - For normally distributed data: Use \code{\link{oneway_anova}()} instead
#' - For repeated measures (same subjects): Use \code{friedman_test()} instead
#'
#' @seealso
#' \code{\link[stats]{kruskal.test}} for the base R Kruskal-Wallis test.
#'
#' \code{\link{mann_whitney}} for comparing exactly two groups.
#'
#' \code{\link{oneway_anova}} for parametric one-way ANOVA.
#'
#' @references
#' Kruskal, W. H., & Wallis, W. A. (1952). Use of ranks in one-criterion
#' variance analysis. Journal of the American Statistical Association,
#' 47(260), 583-621.
#'
#' Tomczak, M., & Tomczak, E. (2014). The need to report effect size estimates
#' revisited. An overview of some recommended measures of effect size. Trends
#' in Sport Sciences, 1(21), 19-25.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Basic Kruskal-Wallis test (comparing across education levels)
#' survey_data %>%
#'   kruskal_wallis(life_satisfaction, group = education)
#'
#' # Multiple variables
#' survey_data %>%
#'   kruskal_wallis(life_satisfaction, income, trust_government,
#'                  group = education)
#'
#' # Using tidyselect helpers
#' survey_data %>%
#'   kruskal_wallis(starts_with("trust_"), group = education)
#'
#' # Weighted analysis
#' survey_data %>%
#'   kruskal_wallis(life_satisfaction, group = education,
#'                  weights = sampling_weight)
#'
#' # Grouped analysis (separate test for each region)
#' survey_data %>%
#'   group_by(region) %>%
#'   kruskal_wallis(life_satisfaction, group = education)
#'
#' # Compare across employment status (5 groups)
#' survey_data %>%
#'   kruskal_wallis(income, group = employment)
#'
#' @family hypothesis_tests
#' @export
kruskal_wallis <- function(data, ..., group, weights = NULL,
                           conf.level = 0.95) {

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

  # Process group variable (required for Kruskal-Wallis)
  if (missing(group)) {
    cli_abort("{.arg group} is required for Kruskal-Wallis test.")
  }
  group_quo <- enquo(group)
  if (quo_is_null(group_quo)) {
    cli_abort("{.arg group} is required for Kruskal-Wallis test.")
  }

  g_var <- eval_select(expr(!!group_quo), data = data)
  g_name <- names(g_var)[1]

  # Process weights using centralized helper
  weights_info <- .process_weights(data, rlang::enquo(weights))
  w_name <- weights_info$name

  # Helper function to perform Kruskal-Wallis test for a single variable
  perform_single_kw <- function(data, var_name, group_name, weight_name = NULL) {
    # Get variable values
    x <- data[[var_name]]
    g <- data[[group_name]]

    # Remove NA values
    valid_indices <- !is.na(x) & !is.na(g)
    if (!is.null(weight_name)) {
      w <- data[[weight_name]]
      valid_indices <- valid_indices & !is.na(w)
      w <- w[valid_indices]
    }
    x <- x[valid_indices]
    g <- g[valid_indices]

    # Ensure grouping variable is a factor
    if (!is.factor(g)) {
      g <- factor(g)
    }

    # Get group levels (only those present in data)
    g_levels <- levels(g)[levels(g) %in% unique(as.character(g))]

    if (length(g_levels) < 2) {
      cli_abort(c(
        "Kruskal-Wallis test requires at least 2 groups.",
        "x" = "Found {length(g_levels)} group{?s} in variable {.var {group_name}}.",
        "i" = "Check that your grouping variable has sufficient levels."
      ))
    }

    n_total <- length(x)

    if (is.null(weight_name)) {
      # ------------------------------------------------------------------
      # Unweighted Kruskal-Wallis
      # ------------------------------------------------------------------

      # Compute ranks over all observations
      all_ranks <- rank(x)

      # Group-level statistics
      group_stats <- lapply(g_levels, function(lvl) {
        idx <- g == lvl
        n_grp <- sum(idx)
        rank_mean <- if (n_grp > 0) mean(all_ranks[idx]) else NA_real_
        list(name = lvl, n = n_grp, rank_mean = rank_mean)
      })
      names(group_stats) <- g_levels

      # Kruskal-Wallis test via stats::kruskal.test
      kw_result <- kruskal.test(x ~ g)

      H <- kw_result$statistic[[1]]
      df <- kw_result$parameter[[1]]
      p_value <- kw_result$p.value

    } else {
      # ------------------------------------------------------------------
      # Weighted Kruskal-Wallis (frequency-weighted approximation)
      # ------------------------------------------------------------------
      # NOTE: This is NOT a design-based / Lumley-Scott implementation.
      # It substitutes sum(w) for n in the standard H formula and
      # computes mid-ranks from cumulative weights. Matches SPSS WEIGHT BY
      # behaviour when weights are frequency weights. For sampling weights
      # that differ meaningfully from 1.0, use survey::svyranktest()
      # instead.
      # ------------------------------------------------------------------

      N_pop <- sum(w)

      # Weighted mid-ranks (frequency-expansion convention, shared helper).
      # H is invariant to the former rank offset; the displayed rank means
      # now match SPSS's expanded-data mid-ranks.
      rankhat <- .weighted_midranks(x, w)

      # Group-level statistics
      group_stats <- lapply(g_levels, function(lvl) {
        idx <- g == lvl
        w_grp <- w[idx]
        n_grp <- sum(w_grp)
        rank_mean <- if (n_grp > 0) sum(w_grp * rankhat[idx]) / n_grp else NA_real_
        list(name = lvl, n = round(n_grp, 1), rank_mean = rank_mean)
      })
      names(group_stats) <- g_levels

      # Weighted H statistic
      # H = (12 / (N*(N+1))) * sum(n_j * (Rbar_j - Rbar)^2)
      # where Rbar = (N+1)/2, but using weighted ranks we compute directly
      grand_mean_rank <- N_pop / 2  # expected mid-rank under H0
      H_numerator <- 0
      for (lvl in g_levels) {
        idx <- g == lvl
        w_grp <- w[idx]
        n_grp <- sum(w_grp)
        rank_mean_grp <- sum(w_grp * rankhat[idx]) / n_grp
        H_numerator <- H_numerator + n_grp * (rank_mean_grp - grand_mean_rank)^2
      }
      H <- (12 / (N_pop * (N_pop + 1))) * H_numerator

      # Tie correction
      tie_groups <- tapply(w, factor(x), sum)
      tie_correction <- sum(tie_groups^3 - tie_groups)
      correction_factor <- 1 - tie_correction / (N_pop^3 - N_pop)
      if (correction_factor > 0) {
        H <- H / correction_factor
      }

      df <- length(g_levels) - 1
      p_value <- pchisq(H, df = df, lower.tail = FALSE)

      n_total <- round(N_pop)
    }

    # Effect size: Epsilon-squared = H / (N - 1)
    epsilon_squared <- H / (n_total - 1)

    return(list(
      H = H,
      df = df,
      p_value = p_value,
      epsilon_squared = epsilon_squared,
      n_total = n_total,
      group_stats = group_stats
    ))
  }

  # Main computation function (loops over variables)
  compute_results <- function(data) {
    results_list <- list()

    for (var_name in var_names) {
      tryCatch({
        result <- perform_single_kw(data, var_name, g_name, w_name)

        results_list[[var_name]] <- tibble(
          Variable = var_name,
          H = result$H,
          df = result$df,
          p_value = result$p_value,
          epsilon_squared = result$epsilon_squared,
          n_total = result$n_total,
          group_stats = list(result$group_stats)
        )

      }, error = function(e) {
        cli_warn("Kruskal-Wallis test failed for variable {.var {var_name}}: {e$message}")
        results_list[[var_name]] <- tibble(
          Variable = var_name,
          H = NA_real_,
          df = NA_integer_,
          p_value = NA_real_,
          epsilon_squared = NA_real_,
          n_total = NA_integer_,
          group_stats = list(NULL)
        )
      })
    }

    bind_rows(results_list)
  }

  # Validate group levels upfront (before entering the loop)
  if (!is_grouped) {
    group_col_pre <- data[[g_name]]
    group_col_pre <- group_col_pre[!is.na(group_col_pre)]
    n_groups <- length(unique(as.character(group_col_pre)))
    if (n_groups < 2) {
      cli_abort(c(
        "Kruskal-Wallis test requires at least 2 groups.",
        "x" = "Found {n_groups} group{?s} in variable {.var {g_name}}.",
        "i" = "Check that your grouping variable has sufficient levels."
      ))
    }
  }

  # Execute computation (with or without group_by)
  if (is_grouped) {
    results <- data %>%
      group_modify(~ compute_results(.x))
  } else {
    results <- compute_results(data)
  }

  # Get group levels for output
  group_col <- data[[g_name]]
  if (is.factor(group_col)) {
    all_levels <- levels(group_col)
    group_levels <- all_levels[all_levels %in% unique(as.character(group_col))]
  } else {
    group_levels <- sort(unique(group_col[!is.na(group_col)]))
  }

  # Create result object
  result <- list(
    results = results,
    variables = var_names,
    group = g_name,
    weights = w_name,
    group_levels = group_levels,
    is_grouped = is_grouped,
    conf.level = conf.level,
    data = data[, unique(c(var_names, g_name, w_name, grp_vars)), drop = FALSE]
  )

  class(result) <- "kruskal_wallis"
  return(result)
}

# Helper: print a single variable block (rank table + test statistics)
#' @noRd
.print_kw_variable_block <- function(var_name, row_data, stats, weights, digits,
                                     show_ranks = TRUE, show_results = TRUE) {
  print_header(var_name, newline_before = FALSE)

  # Print group rank table (gated by ranks toggle)
  if (show_ranks && !is.null(stats)) {
    cat("  Ranks:\n")
    rank_df <- data.frame(
      Group = sapply(stats, function(s) s$name),
      N = sapply(stats, function(s) {
        if (is.null(weights)) as.integer(s$n) else round(s$n, 1)
      }),
      `Mean Rank` = sapply(stats, function(s) round(s$rank_mean, 2)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    # Add total row
    total_n <- sum(rank_df$N)
    rank_df <- rbind(rank_df, data.frame(
      Group = "Total",
      N = if (is.null(weights)) as.integer(total_n) else round(total_n, 1),
      `Mean Rank` = NA,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))

    # Dynamic border
    output <- capture.output(print(rank_df, row.names = FALSE, na.print = ""))
    border_width <- max(nchar(output), na.rm = TRUE)
    border <- paste(rep("-", border_width), collapse = "")

    cat("  ", border, "\n", sep = "")
    for (line in output) {
      cat("  ", line, "\n", sep = "")
    }
    cat("  ", border, "\n\n", sep = "")
  }

  # Print test statistics table (gated by results toggle)
  if (show_results) {
    test_df <- data.frame(
      `Kruskal-Wallis H` = round(row_data$H, digits),
      df = as.integer(row_data$df),
      `p value` = round(row_data$p_value, digits),
      `Epsilon-squared` = round(row_data$epsilon_squared, digits),
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

# Internal: compact one-line summary for a single Kruskal-Wallis variable
#' @noRd
.print_kw_compact <- function(results, i, group_tag, weighted_tag, digits) {
  cat(sprintf("Kruskal-Wallis Test: %s%s%s\n",
              results$Variable[i], group_tag, weighted_tag))
  cat(sprintf("  H(%s) = %s, %s %s, eps2 = %s, N = %s\n",
              formatC(as.integer(results$df[i]), format = "d"),
              fmt_num(results$H[i], digits),
              fmt_p(results$p_value[i], digits, style = "compact"),
              add_significance_stars(results$p_value[i]),
              fmt_num(results$epsilon_squared[i], digits),
              formatC(as.integer(results$n_total[i]), format = "d")))
}

#' Print Kruskal-Wallis test results (compact)
#'
#' @description
#' Compact print method for objects of class \code{"kruskal_wallis"}.
#' Shows a one-line summary per variable with the H statistic, p-value,
#' effect size (epsilon-squared), and sample size.
#'
#' For the full detailed output (rank tables, test statistics), use
#' \code{summary()}.
#'
#' @param x A kruskal_wallis object
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- kruskal_wallis(survey_data, life_satisfaction, group = education)
#' result              # compact one-line overview
#' summary(result)     # full detailed output
#'
#' @export
#' @method print kruskal_wallis
print.kruskal_wallis <- function(x, digits = 3, ...) {
  weighted_tag <- if (!is.null(x$weights)) " [Weighted]" else ""
  group_tag <- if (!is.null(x$group)) paste0(" by ", x$group) else ""
  results <- x$results
  results$p_value <- as.numeric(results$p_value)

  if (isTRUE(x$is_grouped)) {
    group_vars <- setdiff(names(results), c("Variable", "H", "df", "p_value",
                                            "epsilon_squared", "n_total",
                                            "group_stats"))
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
        .print_kw_compact(group_results, j, group_tag, weighted_tag, digits)
      }
    }
  } else {
    for (i in seq_len(nrow(results))) {
      if (is.na(results$Variable[i])) next
      .print_kw_compact(results, i, group_tag, weighted_tag, digits)
    }
  }

  cat("Use summary() for detailed output.\n")
  invisible(x)
}

#' Summary method for Kruskal-Wallis test results
#'
#' @description
#' Creates a summary object that produces detailed output when printed,
#' including rank statistics per group and the test statistics table with
#' H, degrees of freedom, p-value, and epsilon-squared.
#'
#' @param object A \code{kruskal_wallis} result object.
#' @param ranks Logical. Show rank statistics per group? (Default: TRUE)
#' @param results Logical. Show test statistics table? (Default: TRUE)
#' @param digits Number of decimal places for formatting (Default: 3).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.kruskal_wallis} object.
#'
#' @examples
#' result <- kruskal_wallis(survey_data, life_satisfaction, group = education)
#' summary(result)
#' summary(result, ranks = FALSE)
#'
#' @seealso \code{\link{kruskal_wallis}} for the main analysis function.
#' @export
#' @method summary kruskal_wallis
summary.kruskal_wallis <- function(object, ranks = TRUE, results = TRUE,
                                   digits = 3, ...) {
  build_summary_object(
    object     = object,
    show       = list(ranks = ranks, results = results),
    digits     = digits,
    class_name = "summary.kruskal_wallis"
  )
}

#' Print summary of Kruskal-Wallis test results (detailed output)
#'
#' @description
#' Displays the detailed SPSS-style output for a Kruskal-Wallis test, with
#' sections controlled by the boolean parameters passed to
#' \code{\link{summary.kruskal_wallis}}.  Sections include the per-group
#' rank table and the test statistics table with effect size.
#'
#' @param x A \code{summary.kruskal_wallis} object created by
#'   \code{\link{summary.kruskal_wallis}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- kruskal_wallis(survey_data, life_satisfaction, group = education)
#' summary(result)                # all sections
#' summary(result, ranks = FALSE) # hide rank tables
#'
#' @seealso \code{\link{kruskal_wallis}} for the main analysis,
#'   \code{\link{summary.kruskal_wallis}} for summary options.
#' @export
#' @method print summary.kruskal_wallis
print.summary.kruskal_wallis <- function(x, ...) {
  digits <- x$digits

  # Determine test type using standardized helper
  weights_name <- x$weights
  test_type <- get_standard_title("Kruskal-Wallis Test", weights_name, "Results")
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
    "Grouping variable" = x$group,
    "Groups" = paste(x$group_levels, collapse = ", "),
    "Weights variable" = weights_name
  )
  print_info_section(test_info)
  cat("\n")

  if (is_grouped_data) {
    # Get unique groups
    group_vars <- setdiff(names(x$results), c("Variable", "H", "df", "p_value",
                                               "epsilon_squared", "n_total",
                                               "group_stats", "sig"))
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
        .print_kw_variable_block(
          var_name = group_results$Variable[j],
          row_data = group_results[j, ],
          stats    = group_results$group_stats[[j]],
          weights  = x$weights,
          digits   = digits,
          show_ranks   = show_ranks,
          show_results = show_results
        )
      }
    }
  } else {
    valid_results <- x$results[!is.na(x$results$Variable), ]

    for (i in seq_len(nrow(valid_results))) {
      .print_kw_variable_block(
        var_name = valid_results$Variable[i],
        row_data = valid_results[i, ],
        stats    = valid_results$group_stats[[i]],
        weights  = x$weights,
        digits   = digits,
        show_ranks   = show_ranks,
        show_results = show_results
      )
    }
  }

  if (!is.null(x$weights) && (show_ranks || show_results)) {
    cat("Note: Weighted analysis uses frequency-weighted ranks.\n")
  }

  if (show_results) {
    print_significance_legend()

    cat("\nEffect Size Interpretation (Epsilon-squared):\n")
    cat("- Small effect: 0.01 - 0.06\n")
    cat("- Medium effect: 0.06 - 0.14\n")
    cat("- Large effect: > 0.14\n")
  }

  invisible(x)
}
