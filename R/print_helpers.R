# =============================================================================
# PRINT METHOD STYLE GUIDE AND HELPER FUNCTIONS
# =============================================================================
# This file contains standardized helper functions and the style guide for
# all print methods in the mariposa package.
#
# STYLE GUIDE:
# 1. Headers: plain text with dash underline
# 2. Weighted prefix: "[Weighted] {Test Name} Results/Statistics"
# 3. Information order: Title -> Test Info -> Parameters -> Results -> Significance
# 4. Grouped data: "Group: var = value" format with consistent indentation
# 5. Tables: Dynamic width calculation with proper alignment
# 6. Significance codes: Standard *** ** * convention at the bottom
# =============================================================================

#' Print standardized header with title and separator
#' @param title Character string for the title
#' @param newline_before Logical, whether to print newline before header
#' @noRd
print_header <- function(title, newline_before = TRUE) {
  if (newline_before) cat("\n")
  cat(title, "\n", sep = "")
  cat(strrep("-", nchar(title, type = "width")), "\n", sep = "")
}

#' Print test information section
#' @param info Named list of information to display
#' @param indent Number of spaces to indent
#' @noRd
print_info_section <- function(info, indent = 0) {
  prefix <- paste(rep(" ", indent), collapse = "")
  for (name in names(info)) {
    value <- info[[name]]
    if (!is.null(value) && !is.na(value) && value != "") {
      cat(prefix, "- ", name, ": ", value, "\n", sep = "")
    }
  }
}

#' Format p-value with significance stars
#' @param p_value Numeric p-value
#' @param breaks Cut points for significance levels
#' @param labels Significance symbols
#' @noRd
add_significance_stars <- function(p_value,
                                  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                                  labels = c("***", "**", "*", "")) {
  # right = TRUE: boundary values follow the symnum convention printed in
  # the legend (p = 0.001 -> '***', p = 0.05 -> '*'). Vectorized; returns
  # character, NA p-values map to "" (as.numeric: a bare logical NA must
  # not crash cut()).
  out <- as.character(cut(as.numeric(p_value), breaks = breaks,
                          labels = labels, right = TRUE))
  out[is.na(out)] <- ""
  out
}

#' Print significance codes legend
#' @param show Logical, whether to show the legend
#' @noRd
print_significance_legend <- function(show = TRUE) {
  if (show) {
    cat("\n")
    cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05\n")
  }
}

#' Print the group header used by for_each_group()
#'
#' The single verbose group-header style: "Group: var = value, ..." plus a
#' dash underline of the same display width (as print_group_header()).
#' It printed "Group: ... " with a trailing blank and no underline, one of
#' four header styles in the verbose outputs.
#'
#' @param label Pre-formatted "var = value, ..." string
#' @param prefix Text before the label
#' @return invisible(NULL)
#' @noRd
print_group_label <- function(label, prefix = "Group") {
  header_text <- paste0(prefix, ": ", label)
  cat("\n", header_text, "\n",
      strrep("-", nchar(header_text, type = "width")), "\n", sep = "")
  invisible(NULL)
}

#' Print grouped data header
#' @param group_values Named vector or data frame of group values
#' @param prefix Text to print before group info
#' @noRd
print_group_header <- function(group_values, prefix = "Group") {
  # One-row data frame or the equivalent named list of length-1 values
  # (some classes store the key via as.list())
  if (is.list(group_values)) {
    group_str <- .format_group_label(group_values)
  } else {
    group_str <- paste(names(group_values), "=", group_values, collapse = ", ")
  }
  print_group_label(group_str, prefix)
}

#' Pad strings to a given display width (UTF-8 safe)
#'
#' \code{sprintf("\%-20s")} counts bytes instead of display characters for
#' multi-byte UTF-8 strings (umlauts), which misaligned columns. Pads by
#' display width (\code{nchar(type = "width")}); text longer than
#' \code{width} is returned unchanged. Vectorized over \code{text}.
#'
#' @param text Character vector to pad (NA shows as "NA")
#' @param width Target display width
#' @param align "left" for left-aligned (default), "right" for right-aligned
#' @return Padded character vector
#' @noRd
pad_utf8 <- function(text, width, align = "left") {
  text <- as.character(text)
  text[is.na(text)] <- "NA"
  w <- nchar(text, type = "width", allowNA = TRUE)
  w[is.na(w)] <- nchar(text[is.na(w)], type = "bytes")
  spaces <- strrep(" ", pmax(0L, width - w))
  if (identical(align, "right")) paste0(spaces, text) else paste0(text, spaces)
}

#' Calculate dynamic table width
#' @param df Data frame to be printed
#' @param min_width Minimum table width
#' @noRd
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
#' @noRd
print_separator <- function(...) {
  cat(paste(rep("-", 40), collapse = ""), "\n", sep = "")
}

#' Format numeric value with appropriate decimal places
#' @param x Numeric value
#' @param digits Number of decimal places
#' @param scientific Whether to use scientific notation for small p-values
#' @noRd
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
#' @noRd
get_standard_title <- function(test_name, weights = NULL, suffix = "Results") {
  prefix <- if (!is.null(weights)) "Weighted " else ""
  # an empty suffix left a trailing blank ("Pearson Correlation "), and the
  # underline came out one dash longer than the visible title
  if (nzchar(suffix)) paste0(prefix, test_name, " ", suffix)
  else paste0(prefix, test_name)
}

#' Print standard test parameters
#' @param params List of parameters (conf.level, alternative, etc.)
#' @noRd
print_test_parameters <- function(params) {
  if (!is.null(params$conf.level)) {
    cat("- Confidence level: ", sprintf("%.1f%%", params$conf.level * 100), "\n", sep = "")
  }
  if (!is.null(params$alternative)) {
    cat("- Alternative hypothesis: ", params$alternative, "\n", sep = "")
  }
  if (!is.null(params$mu)) {
    cat("- Null hypothesis (mu): ", sprintf("%.3f", params$mu), "\n", sep = "")
  }
}

#' Format variable name or label
#' @param var Variable name
#' @param label Optional label
#' @noRd
format_variable_name <- function(var, label = NULL) {
  if (!is.null(label) && !is.na(label) && label != "" && label != var) {
    sprintf("%s (%s)", var, label)
  } else {
    var
  }
}
