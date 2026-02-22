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
  cli_text("")
  cli_h2("{prefix}: {group_str}")
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
#' @param width Width of the line
#' @param char Character to use for the line
#' @keywords internal
print_separator <- function(width = 40, char = "-") {
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

#' Print results table with consistent formatting
#' @param df Data frame to print
#' @param digits Number of decimal places for numeric columns
#' @param row.names Whether to show row names
#' @keywords internal
print_results_table <- function(df, digits = 3, row.names = FALSE) {
  # Format numeric columns
  numeric_cols <- sapply(df, is.numeric)
  df[numeric_cols] <- lapply(df[numeric_cols], function(x) {
    ifelse(is.na(x), NA, round(x, digits))
  })

  print(df, row.names = row.names)
}

#' Print method footer with optional notes
#' @param notes Character vector of notes to display
#' @keywords internal
print_footer_notes <- function(notes = NULL) {
  if (!is.null(notes) && length(notes) > 0) {
    cli_text("")
    cli_text("{.strong Notes:}")
    names(notes) <- rep("i", length(notes))
    cli_bullets(notes)
  }
}