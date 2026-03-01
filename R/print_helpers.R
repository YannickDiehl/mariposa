# =============================================================================
# PRINT METHOD STYLE GUIDE AND HELPER FUNCTIONS
# =============================================================================
# This file contains standardized helper functions and the style guide for
# all print methods in the mariposa package.
#
# STYLE GUIDE:
# 1. Headers: cli_rule with title
# 2. Weighted prefix: "[Weighted] {Test Name} Results/Statistics"
# 3. Information order: Title -> Test Info -> Parameters -> Results -> Significance
# 4. Grouped data: "Group: var = value" format with consistent indentation
# 5. Tables: Dynamic width calculation with proper alignment
# 6. Significance codes: Standard *** ** * convention at the bottom
# =============================================================================

#' Print standardized header with title and separator
#' @param title Character string for the title
#' @param newline_before Logical, whether to print newline before header
#' @keywords internal
print_header <- function(title, newline_before = TRUE) {
  if (newline_before) cat("\n")
  cli_rule(title)
}

#' Print test information section
#' @param info Named list of information to display
#' @param indent Number of spaces to indent
#' @keywords internal
print_info_section <- function(info, indent = 0) {
  items <- character(0)
  for (name in names(info)) {
    value <- info[[name]]
    if (!is.null(value) && !is.na(value) && value != "") {
      items <- c(items, paste0(name, ": ", value))
    }
  }
  if (length(items) > 0) {
    names(items) <- rep("*", length(items))
    cli_bullets(items)
  }
}

#' Format p-value with significance stars
#' @param p_value Numeric p-value
#' @param breaks Cut points for significance levels
#' @param labels Significance symbols
#' @keywords internal
add_significance_stars <- function(p_value,
                                  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                                  labels = c("***", "**", "*", "")) {
  if (is.na(p_value)) return("")
  cut(p_value, breaks = breaks, labels = labels, right = FALSE)
}

#' Print significance codes legend
#' @param show Logical, whether to show the legend
#' @keywords internal
print_significance_legend <- function(show = TRUE) {
  if (show) {
    cli_text("")
    cli_text("Signif. codes: 0 {.strong ***} 0.001 {.strong **} 0.01 {.strong *} 0.05")
  }
}

#' Print grouped data header
#' @param group_values Named vector or data frame of group values
#' @param prefix Text to print before group info
#' @keywords internal
print_group_header <- function(group_values, prefix = "Group") {
  if (is.data.frame(group_values)) {
    group_str <- paste(sapply(names(group_values), function(g) {
      val <- group_values[[g]]
      paste(g, "=", if (is.factor(val)) as.character(val) else val)
    }), collapse = ", ")
  } else {
    group_str <- paste(names(group_values), "=", group_values, collapse = ", ")
  }
  header_text <- paste0(prefix, ": ", group_str)
  cat("\n", header_text, "\n", sep = "")
  cat(paste(rep("-", nchar(header_text)), collapse = ""), "\n", sep = "")
}

#' Calculate dynamic table width
#' @param df Data frame to be printed
#' @param min_width Minimum table width
#' @keywords internal
get_table_width <- function(df, min_width = 40) {
  if (is.null(df) || nrow(df) == 0) return(min_width)

  output <- capture.output(print(df, row.names = FALSE))
  if (length(output) > 0) {
    max_width <- max(nchar(output), na.rm = TRUE)
    return(max(min_width, max_width))
  }
  return(min_width)
}

#' Print horizontal separator line
#' @param ... Ignored (retained for backward compatibility)
#' @keywords internal
print_separator <- function(...) {
  cli_rule()
}

#' Format numeric value with appropriate decimal places
#' @param x Numeric value
#' @param digits Number of decimal places
#' @param scientific Whether to use scientific notation for small p-values
#' @keywords internal
format_number <- function(x, digits = 3, scientific = FALSE) {
  if (is.na(x)) return("NA")
  if (scientific && abs(x) < 0.0001) {
    return(format(x, scientific = TRUE, digits = digits))
  }
  format(round(x, digits), nsmall = digits)
}

#' Standardize title based on weights and test type
#' @param test_name Name of the statistical test
#' @param weights Weights variable name or NULL
#' @param suffix "Results" or "Statistics"
#' @keywords internal
get_standard_title <- function(test_name, weights = NULL, suffix = "Results") {
  prefix <- if (!is.null(weights)) "Weighted " else ""
  paste0(prefix, test_name, " ", suffix)
}

#' Print standard test parameters
#' @param params List of parameters (conf.level, alternative, etc.)
#' @keywords internal
print_test_parameters <- function(params) {
  items <- character(0)
  if (!is.null(params$conf.level)) {
    items <- c(items, paste0("Confidence level: ", sprintf("%.1f%%", params$conf.level * 100)))
  }
  if (!is.null(params$alternative)) {
    items <- c(items, paste0("Alternative hypothesis: ", params$alternative))
  }
  if (!is.null(params$mu)) {
    items <- c(items, paste0("Null hypothesis (mu): ", sprintf("%.3f", params$mu)))
  }
  if (length(items) > 0) {
    names(items) <- rep("*", length(items))
    cli_bullets(items)
  }
}

#' Format variable name or label
#' @param var Variable name
#' @param label Optional label
#' @keywords internal
format_variable_name <- function(var, label = NULL) {
  if (!is.null(label) && label != "" && label != var) {
    sprintf("%s (%s)", var, label)
  } else {
    var
  }
}

#' Print a correlation/p-value/sample-size matrix with adaptive console width
#' @param mat Named numeric matrix
#' @param digits Number of decimal places
#' @param title Section title printed above the matrix
#' @param type One of "correlation", "pvalue", or "n"
#' @keywords internal
.print_cor_matrix <- function(mat, digits = 3, title = "Correlation Matrix:",
                              type = "correlation") {
  old_width <- getOption("width")

  n_vars <- ncol(mat)
  max_rowname_length <- max(nchar(rownames(mat)), na.rm = TRUE)

  if (type == "correlation") {
    value_width <- digits + 4
  } else if (type == "pvalue") {
    value_width <- digits + 3
  } else {
    value_width <- max(nchar(format(mat, scientific = FALSE)), na.rm = TRUE) + 1
  }

  required_width <- max_rowname_length + 2 + (n_vars * (value_width + 1))

  width_adjusted <- FALSE
  if (required_width > old_width && required_width <= 200) {
    options(width = required_width)
    on.exit(options(width = old_width), add = TRUE)
    width_adjusted <- TRUE
  } else if (required_width > 200) {
    options(width = 200)
    on.exit(options(width = old_width), add = TRUE)
    width_adjusted <- TRUE
  }

  if (n_vars > 6 && type == "correlation") {
    digits <- min(digits, 2)
    value_width <- digits + 4
  }

  cat(paste0("\n", title, "\n"))
  border_width <- paste(rep("-", nchar(title)), collapse = "")
  cat(border_width, "\n")

  if (type == "correlation") {
    formatted_mat <- format(round(mat, digits), width = value_width, nsmall = digits, justify = "right")
  } else if (type == "pvalue") {
    formatted_mat <- format(round(mat, digits), width = value_width, nsmall = digits, justify = "right")
  } else {
    formatted_mat <- format(mat, width = value_width, justify = "right")
  }

  print(formatted_mat, quote = FALSE, right = TRUE)
  cat(border_width, "\n")

  if (width_adjusted && required_width > 200) {
    cat("Note: Matrix display adjusted for console width.\n")
  }
}

#' Print a single correlation pair in detailed block format
#'
#' Used by pearson_cor, spearman_rho, and kendall_tau print methods
#' for two-variable analyses.
#'
#' @param corr_row One-row data.frame with the correlation results
#' @param stat_label Display label for the statistic (e.g. "r", rho, tau)
#' @param stat_col Column name holding the correlation value
#' @param corr_name Full name for the statistic line (e.g. "Correlation", "Spearman's rho")
#' @param secondary_label Optional label for a second statistic
#' @param secondary_col Optional column name for the second statistic
#' @param ci_lower_col Column name for CI lower bound (NULL to skip)
#' @param ci_upper_col Column name for CI upper bound (NULL to skip)
#' @param alternative NULL or "two.sided"/"less"/"greater" - shown as suffix on p-value
#' @keywords internal
.print_single_pair <- function(corr_row, stat_label, stat_col,
                               corr_name = "Correlation",
                               secondary_label = NULL, secondary_col = NULL,
                               ci_lower_col = NULL, ci_upper_col = NULL,
                               alternative = NULL) {
  cat(sprintf("  %s: %s = %.3f\n", corr_name, stat_label, corr_row[[stat_col]][1]))
  if (!is.null(secondary_label) && !is.null(secondary_col)) {
    cat(sprintf("  %s: %.3f\n", secondary_label, corr_row[[secondary_col]][1]))
  }
  cat(sprintf("  Sample size: n = %d\n", corr_row$n[1]))
  if (!is.null(ci_lower_col) && !is.null(ci_upper_col)) {
    cat(sprintf("  95%% CI: [%.3f, %.3f]\n",
                corr_row[[ci_lower_col]][1], corr_row[[ci_upper_col]][1]))
  }
  if (!is.null(alternative) && alternative != "two.sided") {
    p_label <- "1-tailed"
  } else if (!is.null(alternative)) {
    p_label <- "2-tailed"
  } else {
    p_label <- NULL
  }
  if (!is.null(p_label)) {
    cat(sprintf("  p-value (%s): %.4f\n", p_label, corr_row$p_value[1]))
  } else {
    cat(sprintf("  p-value: %.4f\n", corr_row$p_value[1]))
  }
  sig_text <- if (corr_row$sig[1] == "") "ns" else corr_row$sig[1]
  cat(sprintf("  Significance: %s\n", sig_text))
}

