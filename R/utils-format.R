# =============================================================================
# Output formatting utilities
# =============================================================================
# The three building blocks every print method should use instead of
# hand-rolled sprintf/strrep blocks (the audit found ~25 private table
# renderers with three different width algorithms):
#
#   fmt_num()           one rounding + padding policy for numerics
#   fmt_p()             one p-value display policy ("<.001" / "p < 0.001")
#   format_stat_table() column-spec driven bordered ASCII table
#   for_each_group()    iterate grouped results uniformly
# =============================================================================

#' Format numeric values with a fixed number of decimals
#'
#' Single number-display policy: fixed decimals (so columns align), "" for
#' NA, integers without decimals when `digits = 0`.
#'
#' @param x Numeric vector
#' @param digits Decimal places
#' @return Character vector
#' @noRd
fmt_num <- function(x, digits = 3) {
  out <- ifelse(
    is.na(x),
    "",
    formatC(x, format = "f", digits = digits)
  )
  as.character(out)
}

#' Format p-values for display
#'
#' @param p Numeric vector of p-values
#' @param digits Decimal places for non-tiny values
#' @param style "table" gives "<.001" / ".123" (SPSS column style);
#'   "compact" gives "p < 0.001" / "p = 0.123" (inline style)
#' @return Character vector
#' @noRd
fmt_p <- function(p, digits = 3, style = c("table", "compact")) {
  style <- match.arg(style)
  p <- as.numeric(p)
  tiny <- !is.na(p) & p < 0.001

  if (style == "table") {
    out <- ifelse(is.na(p), "",
                  ifelse(tiny, "<.001",
                         sub("^0\\.", ".", formatC(p, format = "f", digits = digits))))
  } else {
    out <- ifelse(is.na(p), "",
                  ifelse(tiny, "p < 0.001",
                         paste0("p = ", formatC(p, format = "f", digits = digits))))
  }
  as.character(out)
}

#' Print a bordered statistics table (column-spec driven)
#'
#' Replaces the hand-rolled "compute widths -> border -> sprintf rows ->
#' border" blocks. Columns are auto-typed and can be overridden:
#'
#'   format_stat_table(df, digits = 3,
#'                     col_types = c(p_adjusted = "pvalue", n = "int"),
#'                     col_labels = c(p_adjusted = "Adj. p"))
#'
#' Types: "num" (fmt_num), "pvalue" (fmt_p table style), "int"
#' (rounded, no decimals), "char" (as.character). Auto-detection: columns
#' named like p-values -> "pvalue"; whole-number numerics -> "int"; other
#' numerics -> "num"; everything else "char". NA displays as "".
#'
#' @param df Data frame to render
#' @param digits Decimal places for "num" columns
#' @param indent Spaces of left indent
#' @param col_types Named character vector overriding per-column types
#' @param col_labels Named character vector overriding header labels
#' @return invisible(df); prints as a side effect
#' @noRd
format_stat_table <- function(df, digits = 3, indent = 2,
                              col_types = NULL, col_labels = NULL) {
  if (nrow(df) == 0) return(invisible(df))

  p_like <- c("p", "p_value", "p_adj", "p_adjusted", "Sig", "sig_level",
              "p.value")

  detect_type <- function(name, values) {
    if (!is.null(col_types) && name %in% names(col_types)) {
      return(col_types[[name]])
    }
    if (name %in% p_like) return("pvalue")
    if (is.numeric(values)) {
      vals <- values[!is.na(values)]
      if (length(vals) > 0 && all(vals == round(vals)) && all(abs(vals) < 1e15)) {
        return("int")
      }
      return("num")
    }
    "char"
  }

  format_col <- function(name, values) {
    switch(detect_type(name, values),
      pvalue = fmt_p(values, digits, style = "table"),
      num    = fmt_num(values, digits),
      int    = ifelse(is.na(values), "", formatC(round(as.numeric(values)), format = "d")),
      ifelse(is.na(values), "", as.character(values))
    )
  }

  cols <- names(df)
  labels <- vapply(cols, function(nm) {
    if (!is.null(col_labels) && nm %in% names(col_labels)) col_labels[[nm]] else nm
  }, character(1))

  formatted <- lapply(cols, function(nm) format_col(nm, df[[nm]]))
  widths <- mapply(function(lab, vals) max(nchar(lab), nchar(vals), 1L),
                   labels, formatted)

  pad_left <- strrep(" ", indent)
  border <- paste0(pad_left, strrep("-", sum(widths) + 2L * (length(widths) - 1L)))

  # First column left-aligned (usually a label), the rest right-aligned
  align <- c("-", rep("", length(cols) - 1L))
  row_fmt <- paste0(
    pad_left,
    paste0("%", align, widths, "s", collapse = "  ")
  )

  cat(border, "\n")
  cat(do.call(sprintf, c(list(row_fmt), as.list(labels))), "\n")
  cat(border, "\n")
  for (i in seq_len(nrow(df))) {
    row_vals <- vapply(formatted, `[[`, character(1), i)
    cat(do.call(sprintf, c(list(row_fmt), as.list(row_vals))), "\n")
  }
  cat(border, "\n")

  invisible(df)
}

#' Iterate a grouped results table uniformly
#'
#' Replaces the ~30 hand-rolled "unique(results[group_vars]) + nested
#' filter loop" blocks in print methods. Handles factor group values,
#' skips empty combinations, and prints a group header unless disabled.
#'
#' @param results Data frame with group columns
#' @param group_vars Character vector of group column names
#' @param fun Called as fun(rows, group_values) per combination, where
#'   group_values is a one-row data frame
#' @param header Print a group header line before each call?
#' @return invisible(NULL)
#' @noRd
for_each_group <- function(results, group_vars, fun, header = TRUE) {
  if (is.null(group_vars) || length(group_vars) == 0) {
    fun(results, NULL)
    return(invisible(NULL))
  }

  combos <- unique(results[group_vars])

  for (i in seq_len(nrow(combos))) {
    combo <- combos[i, , drop = FALSE]
    rows <- results
    for (g in group_vars) {
      rows <- rows[rows[[g]] == combo[[g]], , drop = FALSE]
    }
    if (nrow(rows) == 0) next

    if (header) {
      label <- paste(
        vapply(group_vars, function(g) {
          val <- combo[[g]]
          if (is.factor(val)) val <- as.character(val)
          paste(g, "=", val)
        }, character(1)),
        collapse = ", "
      )
      cat("\nGroup:", label, "\n")
    }
    fun(rows, combo)
  }
  invisible(NULL)
}
