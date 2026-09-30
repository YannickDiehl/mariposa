
#' Test If Groups Vary Similarly
#'
#' @description
#' \code{levene_test()} checks if different groups have similar amounts of variation.
#' This is an important assumption for many statistical tests - groups should spread
#' out in similar ways.
#'
#' The test tells you:
#' - Whether variance is consistent across groups
#' - If you can trust standard ANOVA and t-test results
#' - When to use alternative tests that don't assume equal variance
#'
#' @param x Either your data (a data frame, optionally grouped with
#'   \code{group_by()}) or test results from \code{t_test()},
#'   \code{oneway_anova()} or \code{factorial_anova()}
#' @param ... Variables to test (when using a data frame). List several
#'   variables or use tidyselect helpers like \code{starts_with("trust")}.
#' @param group The grouping variable for comparison (unquoted or a string)
#' @param weights Optional survey weights for population-representative
#'   results. Must be numeric and non-negative (the package-wide weights
#'   policy).
#' @param center How to measure center: \code{"mean"} (default) or \code{"median"} (more robust)
#'
#' @return Test results showing:
#' - Whether groups have equal variances (p-value)
#' - F-statistic measuring variance differences
#' - Which variables meet the assumption
#'
#' @details
#' ## Understanding the Results
#'
#' **P-value interpretation**:
#' - p > 0.05: Good! Groups have similar variance (assumption met)
#' - p <= 0.05: Problem - groups vary differently (assumption violated)
#'
#' Think of it like checking if all groups are equally "spread out":
#' - Similar spread = can use standard tests
#' - Different spread = need special methods
#'
#' ## When to Use This
#'
#' Check variance equality when:
#' - Before running t-tests or ANOVA
#' - Comparing groups with different sizes
#' - Your statistical test assumes equal variances
#' - You see very different standard deviations
#'
#' ## What If Variances Are Unequal?
#'
#' If Levene's test is significant (p <= 0.05):
#' - For t-tests: Use Welch's t-test (var.equal = FALSE)
#' - For ANOVA: Use Welch's ANOVA
#' - Consider transforming your data
#' - Use non-parametric alternatives
#' - Report that equal variance assumption was violated
#'
#' ## Usage Flexibility
#'
#' You can use this function two ways:
#' - **Standalone**: Check any variables for equal variance
#' - **After tests**: Pipe after t_test() or oneway_anova() to verify assumptions
#'
#' ## Tips for Success
#'
#' - Always check this assumption for group comparisons
#' - Visual inspection (boxplots) can supplement the test
#' - Large samples make the test very sensitive
#' - Use median-based test for skewed data (center = "median")
#' - Don't panic if violated - alternatives exist!
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Standalone Levene test (test homogeneity of variances)
#' survey_data %>% levene_test(life_satisfaction, group = region)
#'
#' # Multiple variables
#' survey_data %>% levene_test(life_satisfaction, trust_government, group = region)
#'
#' # Weighted analysis
#' survey_data %>% levene_test(income, group = education, weights = sampling_weight)
#'
#' # Piped after ANOVA (common workflow)
#' result <- survey_data %>%
#'   oneway_anova(life_satisfaction, group = education)
#' result %>% levene_test()
#'
#' # Piped after t-test
#' survey_data %>%
#'   t_test(age, group = gender) %>%
#'   levene_test()
#'
#' # Using mean instead of median as center
#' survey_data %>% levene_test(income, group = region, center = "mean")
#'
#' @seealso
#' \code{\link{oneway_anova}} for one-way ANOVA (which assumes equal variances).
#'
#' \code{\link{t_test}} for group mean comparisons.
#'
#' \code{\link[stats]{var.test}} for the base R F-test of variance equality.
#'
#' @references
#' Levene, H. (1960). Robust tests for equality of variances. In I. Olkin
#' (Ed.), \emph{Contributions to Probability and Statistics} (pp. 278--292).
#' Stanford University Press.
#'
#' Brown, M. B., & Forsythe, A. B. (1974). Robust tests for the equality of
#' variances. \emph{Journal of the American Statistical Association}, 69(346),
#' 364--367.
#'
#' IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.
#'
#' @family posthoc
#' @export
levene_test <- function(x, ...) {
  UseMethod("levene_test")
}

#' @rdname levene_test
#' @export
levene_test.default <- function(x, ...) {
  cls <- paste(class(x), collapse = "/")
  cli_abort(c(
    "{.fn levene_test} is not available for objects of class {.cls {cls}}.",
    "i" = "Levene's test works with {.fn oneway_anova}, {.fn t_test}, or directly on a data frame.",
    "i" = "Example: {.code oneway_anova(data, dv, group) |> levene_test()}"
  ))
}

#' @rdname levene_test
#' @export
levene_test.data.frame <- function(x, ..., group, weights = NULL, center = c("mean", "median")) {
  center <- rlang::arg_match(center)
  spec <- .levene_spec(x, rlang::enquos(...), rlang::enquo(group),
                       rlang::enquo(weights))

  results_df <- .levene_rows(spec$data, spec$variables, spec$group,
                             spec$weights, center)

  result <- list(
    results = results_df,
    variables = spec$variables,
    group = spec$group,
    weights = spec$weights,
    center = center,
    is_grouped = FALSE,
    groups = NULL,
    original_test = NULL
  )
  class(result) <- "levene_test"
  result
}

#' Resolve the variables, grouping variable and weights of a Levene call
#'
#' Shared by the data.frame and grouped_df methods: tidyselect for `...`
#' and `group` (so strings and helpers like starts_with() work), the
#' package weights policy via .process_weights() (numeric, non-negative).
#' @noRd
.levene_spec <- function(data, dots, group_quo, weights_quo,
                         call = rlang::caller_env()) {
  if (length(dots) == 0) {
    cli_abort("At least one variable must be specified.", call = call)
  }
  vars <- tidyselect::eval_select(rlang::expr(c(!!!dots)), data = data)
  var_names <- names(vars)
  if (length(var_names) == 0) {
    cli_abort("At least one variable must be specified.", call = call)
  }
  for (vn in var_names) {
    if (!is.numeric(data[[vn]])) {
      cli_abort("Variable {.var {vn}} is not numeric. {.fn levene_test} requires numeric variables.",
                call = call)
    }
  }

  if (rlang::quo_is_missing(group_quo) || rlang::quo_is_null(group_quo)) {
    cli_abort("{.arg group} is required for Levene's test.", call = call)
  }
  g_name <- names(tidyselect::eval_select(rlang::expr(!!group_quo), data = data))
  if (length(g_name) != 1) {
    cli_abort("{.arg group} must select exactly one variable.", call = call)
  }

  weights_info <- .process_weights(data, weights_quo, call = call)

  list(data = weights_info$data, variables = var_names, group = g_name,
       weights = weights_info$name)
}

#' Levene rows for several variables (one group of a grouped analysis)
#'
#' A variable that cannot be tested (no variance, fewer than 2 groups, ...)
#' keeps an NA row with the reason in `note` and a warning that names the
#' variable and the group.
#' @noRd
.levene_rows <- function(data, var_names, g_name, w_name, center,
                         group_info = NULL) {
  rows <- lapply(var_names, function(var_name) {
    res <- tryCatch(
      perform_single_levene_test(data, var_name, g_name, w_name, center),
      error = function(e) {
        .warn_not_computed("levene_test", var_name, conditionMessage(e),
                           group_info)
        list(F_stat = NA_real_, df1 = NA_real_, df2 = NA_real_,
             p_value = NA_real_, note = conditionMessage(e))
      }
    )
    row <- tibble(
      Variable = var_name,
      F_statistic = as.numeric(res$F_stat),
      df1 = as.numeric(res$df1),
      df2 = as.numeric(res$df2),
      p_value = as.numeric(res$p_value),
      conclusion = if (is.na(res$p_value)) NA_character_ else
        ifelse(res$p_value > 0.05, "Variances equal", "Variances unequal"),
      note = res$note %||% NA_character_
    )
    if (!is.null(group_info)) row <- dplyr::bind_cols(group_info, row)
    row
  })
  bind_rows(rows)
}

#' Levene rows per group of a grouped analysis
#'
#' dplyr keys (group_split()/group_keys()): a missing key (NA, including
#' tagged NAs) is its own group instead of matching nothing.
#' @noRd
.levene_grouped <- function(data, group_vars, var_names, g_name, w_name,
                            center) {
  data <- dplyr::group_by(dplyr::ungroup(data),
                          dplyr::across(dplyr::all_of(group_vars)))
  data_list <- dplyr::group_split(data)
  keys <- dplyr::group_keys(data)
  bind_rows(lapply(seq_along(data_list), function(i) {
    .levene_rows(dplyr::ungroup(data_list[[i]]), var_names, g_name, w_name,
                 center, keys[i, , drop = FALSE])
  }))
}

#' @rdname levene_test
#' @export
levene_test.oneway_anova <- function(x, center = c("mean", "median"), ...) {
  center <- rlang::arg_match(center)

  # Extract information from ANOVA results
  if (is.null(x$group)) {
    cli_abort("Levene test requires a grouping variable.")
  }

  if (is.null(x$data)) {
    cli_abort(c("Original data not available in {.fn oneway_anova} results.", "i" = "Use: {.code levene_test(data, variables, group = group)}"))
  }

  .levene_from_result(x, center)
}

#' @rdname levene_test
#' @export
levene_test.t_test <- function(x, center = c("mean", "median"), ...) {
  center <- rlang::arg_match(center)

  # Extract information from t-test results
  if (is.null(x$group)) {
    cli_abort("Levene test requires a grouping variable (two-sample t-test).")
  }

  if (is.null(x$data)) {
    cli_abort(c("Original data not available in {.fn t_test} results.", "i" = "Use: {.code levene_test(data, variables, group = group)}"))
  }

  .levene_from_result(x, center)
}

#' Levene test on the data stored in a t_test / oneway_anova result
#' @noRd
.levene_from_result <- function(x, center) {
  is_grouped <- isTRUE(x$is_grouped) && length(x$groups) > 0
  results_df <- if (is_grouped) {
    .levene_grouped(x$data, x$groups, x$variables, x$group, x$weights, center)
  } else {
    .levene_rows(dplyr::ungroup(x$data), x$variables, x$group, x$weights,
                 center)
  }
  structure(
    list(
      results = results_df,
      variables = x$variables,
      group = x$group,
      weights = x$weights,
      center = center,
      is_grouped = is_grouped,
      groups = if (is_grouped) x$groups else NULL,
      original_test = x
    ),
    class = "levene_test"
  )
}

#' @rdname levene_test
#' @export
levene_test.mann_whitney <- function(x, ...) {
  cli_abort(c(
    "Levene's test is not appropriate for Mann-Whitney U test results.",
    "i" = "Mann-Whitney U test is non-parametric and does not assume equal variances.",
    "i" = "Use Levene's test only with parametric tests like t-test."
  ))
}

#' @rdname levene_test
#' @export
levene_test.grouped_df <- function(x, ..., group, weights = NULL, center = c("mean", "median")) {
  center <- rlang::arg_match(center)
  group_vars <- dplyr::group_vars(x)

  # Same argument handling as the data.frame method: variables via `...`
  # (several variables and tidyselect helpers), `group`/`weights` by name.
  # The old signature (x, variable, group, weights) bound a second variable
  # positionally to `weights`.
  spec <- .levene_spec(x, rlang::enquos(...), rlang::enquo(group),
                       rlang::enquo(weights))

  results_df <- .levene_grouped(spec$data, group_vars, spec$variables,
                                spec$group, spec$weights, center)

  result <- list(
    results = results_df,
    variables = spec$variables,
    group = spec$group,
    weights = spec$weights,
    center = center,
    is_grouped = TRUE,
    groups = group_vars,
    original_test = NULL
  )
  class(result) <- "levene_test"
  result
}


# Helper function to perform Levene test for a single variable (non-grouped)
perform_single_levene_test <- function(data, var_name, group_name, weight_name = NULL, center = "mean") {

  # Get the variable values
  x <- .plain_numeric(data[[var_name]])
  g <- .group_factor(data[[group_name]])

  # Remove NA values
  valid_indices <- !is.na(x) & !is.na(g)
  if (!is.null(weight_name)) {
    w <- .plain_numeric(data[[weight_name]])  # SPSS weights: see .plain_numeric
    valid_indices <- valid_indices & !is.na(w)
    w <- w[valid_indices]
  }
  x <- x[valid_indices]
  g <- droplevels(g[valid_indices])
  g_levels <- levels(g)

  if (length(g_levels) < 2) {
    .not_computed(sprintf(
      "only %d group with valid data; Levene's test needs at least 2 groups",
      length(g_levels)
    ))
  }

  # No variance at all / within every group: all absolute deviations are 0
  # and F = 0/0 (was printed as "F(NA, NA) = ,")
  reason <- .dv_degenerate_reason(x, g)
  if (!is.null(reason)) .not_computed(reason)

  # Calculate deviations from group centers
  z_values <- numeric(length(x))

  # Calculate group centers and deviations
  for (level in g_levels) {
    group_indices <- g == level
    group_data <- x[group_indices]

    if (!is.null(weight_name)) {
      group_weights <- w[group_indices]
      if (center == "median") {
        # For weighted median, use a simple approximation
        group_center <- median(group_data, na.rm = TRUE)
      } else {
        # Weighted mean
        group_center <- sum(group_data * group_weights) / sum(group_weights)
      }
    } else {
      if (center == "median") {
        group_center <- median(group_data, na.rm = TRUE)
      } else {
        group_center <- mean(group_data, na.rm = TRUE)
      }
    }

    z_values[group_indices] <- abs(group_data - group_center)
  }

  if (!is.null(weight_name)) {
    # SPSS-compatible weighted Levene test
    # Calculate effective sample sizes per group (sum of weights)
    group_eff_n <- sapply(g_levels, function(level) {
      group_indices <- g == level
      sum(w[group_indices])
    })

    # Calculate weighted group means of absolute deviations
    z_group_means <- sapply(g_levels, function(level) {
      group_indices <- g == level
      sum(z_values[group_indices] * w[group_indices]) / sum(w[group_indices])
    })
    names(z_group_means) <- as.character(g_levels)

    # Calculate overall weighted mean of absolute deviations
    z_overall_mean <- sum(z_values * w) / sum(w)

    # Between-groups sum of squares (SPSS method)
    ss_between <- sum(group_eff_n * (z_group_means - z_overall_mean)^2)

    # Within-groups sum of squares (SPSS method)
    ss_within <- 0
    for (i in seq_along(z_values)) {
      group_level <- as.character(g[i])
      group_mean <- z_group_means[group_level]
      ss_within <- ss_within + w[i] * (z_values[i] - group_mean)^2
    }

    # SPSS Levene df: df1 = k - 1, df2 = sum(w) - k (UNROUNDED).
    # T-TEST family convention; see grouped path above for empirical
    # justification.
    df1 <- length(g_levels) - 1
    df2 <- sum(w) - length(g_levels)

    # Calculate F-statistic (SPSS method)
    # F = (SS_between / df1) / (SS_within / df2)
    F_stat <- (ss_between / df1) / (ss_within / df2)
    p_value <- pf(F_stat, df1, df2, lower.tail = FALSE)

  } else {
    # Unweighted Levene test using standard ANOVA
    test_data <- data.frame(z = z_values, group = as.factor(g))
    anova_result <- aov(z ~ group, data = test_data)
    anova_summary <- summary(anova_result)

    F_stat <- anova_summary[[1]][["F value"]][1]
    df1 <- anova_summary[[1]][["Df"]][1]
    df2 <- anova_summary[[1]][["Df"]][2]
    p_value <- anova_summary[[1]][["Pr(>F)"]][1]
  }

  return(list(
    F_stat = F_stat,
    df1 = df1,
    df2 = df2,
    p_value = p_value
  ))
}

# Internal: compact one-line summary for a single Levene test row
#' @noRd
.print_levene_compact <- function(rows, i, group_tag, weighted_tag, digits) {
  F_val <- rows$F_statistic[i]
  df1   <- rows$df1[i]
  df2   <- rows$df2[i]
  p_val <- as.numeric(rows$p_value[i])
  concl <- if ("conclusion" %in% names(rows)) rows$conclusion[i] else NA_character_

  if (is.na(F_val)) {
    note <- if ("note" %in% names(rows)) rows$note[i] else NA_character_
    cat(sprintf("Levene's Test: %s%s%s\n", rows$Variable[i], group_tag, weighted_tag))
    cat(sprintf("  not computed (%s)\n", if (!is.na(note)) note else "see warning"))
    return(invisible(NULL))
  }

  df2_str <- if (is.na(df2)) {
    "NA"
  } else if (abs(df2 - round(df2)) < 1e-8) {
    formatC(as.integer(round(df2)), format = "d")
  } else {
    fmt_num(df2, 1)
  }

  concl_str <- if (!is.na(concl) && nzchar(concl)) {
    paste0(", ", tolower(concl))
  } else {
    ""
  }

  cat(sprintf("Levene's Test: %s%s%s\n", rows$Variable[i], group_tag, weighted_tag))
  cat(sprintf("  F(%s, %s) = %s, %s %s%s\n",
              formatC(as.integer(df1), format = "d"), df2_str,
              fmt_num(F_val, digits),
              fmt_p(p_val, digits, style = "compact"),
              add_significance_stars(p_val),
              concl_str))
}

#' Print Levene test results (compact)
#'
#' @description
#' Compact print method for objects of class \code{"levene_test"}.
#' Shows a one-line summary per variable with the F statistic, degrees of
#' freedom, p-value, and the equal/unequal variances conclusion.
#'
#' For the full detailed output (results tables, interpretation,
#' recommendation), use \code{summary()}.
#'
#' @param x A levene_test object
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- levene_test(survey_data, life_satisfaction, group = education)
#' result              # compact one-line overview
#' summary(result)     # full detailed output
#'
#' @export
#' @method print levene_test
print.levene_test <- function(x, digits = 3, ...) {
  weighted_tag <- if (!is.null(x$weights)) " [Weighted]" else ""
  group_name <- x$group_var %||% x$group
  group_tag <- if (!is.null(group_name)) paste0(" by ", group_name) else ""
  results <- x$results

  if (isTRUE(x$is_grouped) && "Group" %in% names(results)) {
    # Direct grouped analysis: Group column like "region = East"
    for (i in seq_len(nrow(results))) {
      cat(sprintf("[%s]\n", results$Group[i]))
      .print_levene_compact(results, i, group_tag, weighted_tag, digits)
    }
  } else if (isTRUE(x$is_grouped) && !is.null(x$groups) && length(x$groups) > 0 &&
             all(x$groups %in% names(results))) {
    # Grouped analysis with separate group columns
    groups <- unique(results[x$groups])

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
        .print_levene_compact(group_results, j, group_tag, weighted_tag, digits)
      }
    }
  } else {
    valid_results <- results[!is.na(results$Variable), ]
    for (i in seq_len(nrow(valid_results))) {
      .print_levene_compact(valid_results, i, group_tag, weighted_tag, digits)
    }
  }

  cat("Use summary() for detailed output.\n")
  invisible(x)
}

#' Summary method for Levene test results
#'
#' @description
#' Creates a summary object that produces detailed output when printed,
#' including the per-variable results tables, the interpretation of the
#' homogeneity-of-variance test, and the follow-up recommendation.
#'
#' @param object A \code{levene_test} result object.
#' @param results Logical. Show the test results tables? (Default: TRUE)
#' @param interpretation Logical. Show the interpretation section?
#'   (Default: TRUE)
#' @param recommendation Logical. Show the recommendation section (when
#'   available)? (Default: TRUE)
#' @param digits Number of decimal places for formatting (Default: 3).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.levene_test} object.
#'
#' @examples
#' result <- levene_test(survey_data, life_satisfaction, group = education)
#' summary(result)
#' summary(result, interpretation = FALSE)
#'
#' @seealso \code{\link{levene_test}} for the main analysis function.
#' @export
#' @method summary levene_test
summary.levene_test <- function(object, results = TRUE, interpretation = TRUE,
                                recommendation = TRUE, digits = 3, ...) {
  build_summary_object(
    object     = object,
    show       = list(results = results, interpretation = interpretation,
                      recommendation = recommendation),
    digits     = digits,
    class_name = "summary.levene_test"
  )
}

#' Print summary of Levene test results (detailed output)
#'
#' @description
#' Displays the detailed output for Levene's test of homogeneity of
#' variance, with sections controlled by the boolean parameters passed to
#' \code{\link{summary.levene_test}}.  Sections include the results
#' tables, interpretation, and recommendation.
#'
#' @param x A \code{summary.levene_test} object created by
#'   \code{\link{summary.levene_test}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- levene_test(survey_data, life_satisfaction, group = education)
#' summary(result)                          # all sections
#' summary(result, interpretation = FALSE)  # hide interpretation
#'
#' @seealso \code{\link{levene_test}} for the main analysis,
#'   \code{\link{summary.levene_test}} for summary options.
#' @export
#' @method print summary.levene_test
print.summary.levene_test <- function(x, ...) {
  digits <- x$digits

  # Determine test type using standardized helper
  weights_name <- x$weights
  test_type <- get_standard_title("Levene's Test for Homogeneity of Variance", weights_name, "")
  print_header(test_type, newline_before = FALSE)

  # Resolve show toggles
  show_results        <- isTRUE(x$show$results)
  show_interpretation <- isTRUE(x$show$interpretation)
  show_recommendation <- isTRUE(x$show$recommendation)

  # Print test information using standardized helpers
  cat("\n")
  group_name <- x$group_var %||% x$group
  weight_name <- x$weights
  test_info <- list(
    "Grouping variable" = group_name,
    "Weights variable" = weight_name,
    "Center" = x$center
  )
  print_info_section(test_info)
  cat("\n")

  # Add significance stars
  x$results$sig <- sapply(x$results$p_value, add_significance_stars)

  # Check if this is grouped data
  is_grouped_data <- isTRUE(x$is_grouped)
  if (show_results && is_grouped_data) {
    # Handle grouped results - group by group identity, then show all variables for each group

    # Get unique groups
    if ("Group" %in% names(x$results)) {
      # Direct grouped analysis: Group column like "eastwest=East"
      unique_groups <- unique(x$results$Group)
      group_column <- "Group"
    } else if (!is.null(x$groups) && length(x$groups) > 0) {
      # t_test grouped analysis: separate column for each group variable
      group_combinations <- unique(x$results[x$groups])
      unique_groups <- apply(group_combinations, 1, function(row) {
        group_parts <- sapply(x$groups, function(group_var) {
          paste(group_var, "=", row[group_var])
        })
        paste(group_parts, collapse = ", ")
      })
      group_column <- x$groups
    } else {
      # Fallback - check for any non-standard columns that might be group indicators
      non_standard_cols <- setdiff(names(x$results), c("Variable", "F_statistic", "df1", "df2", "p_value", "sig", "conclusion"))
      if (length(non_standard_cols) > 0) {
        group_column <- non_standard_cols[1]
        unique_groups <- unique(x$results[[group_column]])
        unique_groups <- paste(group_column, "=", unique_groups)
      } else {
        unique_groups <- "Unknown Group"
        group_column <- NULL
      }
    }

    # Process each unique group - similar to t_test format
    for (group_idx in seq_along(unique_groups)) {
      group_label <- unique_groups[group_idx]

      # Filter results for current group
      if ("Group" %in% names(x$results)) {
        # Direct group column - group_label is the raw Group value
        group_results <- x$results[x$results$Group == group_label, ]
      } else if (!is.null(x$groups) && length(x$groups) > 0) {
        # For multi-column grouping, match all group columns
        group_results <- x$results
        group_combination <- group_combinations[group_idx, , drop = FALSE]
        for (g in x$groups) {
          group_results <- group_results[.group_match(group_results[[g]], group_combination[[g]]), ]
        }
      } else if (!is.null(group_column)) {
        group_value <- unique(x$results[[group_column]])[group_idx]
        group_results <- x$results[.group_match(x$results[[group_column]], group_value), ]
      } else {
        group_results <- x$results
      }

      if (nrow(group_results) == 0) next

      # Show group header only once - similar to t_test format
      # Format group label according to template standard (ensure single spaces around =)
      formatted_group_label <- gsub(" = ", "=", group_label)  # Remove existing spaces
      formatted_group_label <- gsub("=", " = ", formatted_group_label)  # Add single spaces
      cat(sprintf("\nGroup: %s\n", formatted_group_label))

      # Process each variable in this group as separate blocks
      for (i in seq_len(nrow(group_results))) {
        group_row <- group_results[i, ]
        var_name <- group_row$Variable

        cat(sprintf("\n--- %s ---\n", var_name))
        cat("\n")  # Add blank line after variable name

        # Create results table for this variable
        results_df <- data.frame(
          Variable = group_row$Variable,
          F_statistic = round(group_row$F_statistic, digits),
          df1 = group_row$df1,
          df2 = group_row$df2,
          p_value = round(group_row$p_value, digits),
          sig = group_row$sig,
          Conclusion = group_row$conclusion,
          stringsAsFactors = FALSE
        )

        # Calculate border width
        col_widths <- sapply(names(results_df), function(col) {
          max(nchar(as.character(results_df[[col]])), nchar(col), na.rm = TRUE)
        })
        total_width <- sum(col_widths) + length(col_widths) - 1
        border_width <- paste(rep("-", total_width), collapse = "")

        cat(sprintf("%s:\n", ifelse(!is.null(x$weights), "Weighted Levene's Test Results", "Levene's Test Results")))
        cat(border_width, "\n")
        print(results_df, row.names = FALSE)
        cat(border_width, "\n")
      }
    }

  } else if (show_results) {
    # Handle ungrouped results (original behavior)
    valid_results <- x$results[!is.na(x$results$Variable), ]

    # Process each variable separately like in t-test
    for (i in seq_len(nrow(valid_results))) {
      var_name <- valid_results$Variable[i]

      # Add variable box like in t-test
      cat(sprintf("\n--- %s ---\n", var_name))
      cat("\n")  # Add blank line after variable name

      # Create results table for this variable
      results_df <- data.frame(
        Variable = valid_results$Variable[i],
        F_statistic = round(valid_results$F_statistic[i], digits),
        df1 = valid_results$df1[i],
        df2 = valid_results$df2[i],
        p_value = round(valid_results$p_value[i], digits),
        sig = valid_results$sig[i],
        Conclusion = valid_results$conclusion[i],
        stringsAsFactors = FALSE
      )

      # Calculate border width
      col_widths <- sapply(names(results_df), function(col) {
        max(nchar(as.character(results_df[[col]])), nchar(col), na.rm = TRUE)
      })
      total_width <- sum(col_widths) + length(col_widths) - 1
      border_width <- paste(rep("-", total_width), collapse = "")

      cat(sprintf("%s:\n", ifelse(!is.null(x$weights), "Weighted Levene's Test Results", "Levene's Test Results")))
      cat(border_width, "\n")
      print(results_df, row.names = FALSE)
      cat(border_width, "\n")

      if (i < nrow(valid_results)) {
        cat("\n")  # Add spacing between variables
      }
    }
  }

  if (show_results) {
    print_significance_legend()
  }

  if (show_interpretation) {
    cat("\nInterpretation:\n")
    cat("- p > 0.05: Variances are homogeneous (equal variances assumed)\n")
    cat("- p <= 0.05: Variances are heterogeneous (equal variances NOT assumed)\n")
  }

  # Always show recommendation for grouped data (gated by recommendation toggle)
  if (show_recommendation) {
    if (is_grouped_data) {
      cat("\nRecommendation based on Levene test:\n")
      unequal_groups <- sum(x$results$p_value <= 0.05, na.rm = TRUE)
      if (unequal_groups > 0) {
        cat(sprintf("- %d group(s) show unequal variances (p <= 0.05)\n", unequal_groups))
        cat("- Consider using Welch's t-test for groups with unequal variances\n")
      } else {
        cat("- All groups show equal variances (p > 0.05)\n")
        cat("- Standard t-test assumptions are met for all groups\n")
      }
    } else if (!is.null(x$original_test)) {
      cat("\nRecommendation based on Levene test:\n")
      if (any(x$results$p_value <= 0.05, na.rm = TRUE)) {
        cat("- Use Welch's t-test (unequal variances)\n")
      } else {
        cat("- Student's t-test or Welch's t-test both appropriate\n")
      }
    }
  }

  invisible(x)
}




