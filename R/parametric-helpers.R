# =============================================================================
# Shared internal helpers for the parametric tests
# =============================================================================
# t_test(), oneway_anova(), factorial_anova(), ancova() and the pairwise
# post-hoc engine share one policy for degenerate data: a dependent variable
# that cannot be tested (no valid values, no variance, ...) is reported as
# "not computed (<reason>)" with a warning that names the variable and, for
# grouped data, the group - never with a spurious statistic computed from
# floating-point noise, a raw (locale-dependent) base-R error, or an NA
# table printed as "F(NA, NA) = ,".
# =============================================================================

#' Why a dependent variable cannot be tested
#'
#' @param y Numeric vector of valid (non-missing) values
#' @param g Optional factor of the same length: the groups/cells
#' @return NULL when the data can be tested, otherwise a short reason
#' @noRd
.dv_degenerate_reason <- function(y, g = NULL) {
  y <- .plain_numeric(y)
  if (length(y) == 0) return("no non-missing values")
  if (all(y == y[1])) {
    return(sprintf("no variance: constant value %s", format(y[1])))
  }
  if (!is.null(g)) {
    const_within <- vapply(split(y, g, drop = TRUE),
                           function(v) all(v == v[1]), logical(1))
    if (all(const_within)) return("no variance within any group")
  }
  NULL
}

#' Signal that a statistic cannot be computed for this variable/group
#'
#' A classed condition: the per-variable loops catch it, warn once (naming
#' variable and group) and keep an NA row with the reason in `note`.
#' @noRd
.not_computed <- function(reason) {
  rlang::abort(reason, class = "mariposa_not_computed")
}

#' Groups whose variance is undefined or zero
#'
#' Welch-type statistics need a variance estimate in every group: a group
#' with fewer than 2 cases (weighted: sum of weights <= 1, SPSS frequency
#' weights) has none, a group with zero variance gets an infinite Welch
#' weight n/s^2. SPSS prints no robust test in either case ("cannot be
#' performed ... because at least one group has 0 variance / sum of case
#' weights less than or equal to 1").
#'
#' @param y Numeric vector (valid values)
#' @param g Factor of the same length
#' @param w Optional weights of the same length
#' @return list(small = <levels without a variance estimate>,
#'   zero = <levels with zero variance>)
#' @noRd
.group_variance_problems <- function(y, g, w = NULL) {
  y <- .plain_numeric(y)
  lv <- levels(droplevels(g))
  small <- zero <- character(0)
  for (l in lv) {
    idx <- which(g == l)
    too_small <- if (is.null(w)) length(idx) < 2 else sum(w[idx]) <= 1
    if (too_small) {
      small <- c(small, l)
    } else if (all(y[idx] == y[idx[1]])) {
      zero <- c(zero, l)
    }
  }
  list(small = small, zero = zero)
}

#' Reason text for .group_variance_problems() (NULL when there is none)
#' @noRd
.group_variance_reason <- function(problems, weighted = FALSE) {
  quote_levels <- function(l) paste0("\"", l, "\"", collapse = ", ")
  if (length(problems$small) > 0) {
    what <- if (weighted) "a sum of weights <= 1" else "only 1 case"
    return(sprintf("group%s %s ha%s %s",
                   if (length(problems$small) > 1) "s" else "",
                   quote_levels(problems$small),
                   if (length(problems$small) > 1) "ve" else "s", what))
  }
  if (length(problems$zero) > 0) {
    return(sprintf("group%s %s ha%s zero variance",
                   if (length(problems$zero) > 1) "s" else "",
                   quote_levels(problems$zero),
                   if (length(problems$zero) > 1) "ve" else "s"))
  }
  NULL
}

#' Warn that a variable (in a group) was not computed
#'
#' @param fn Function name for the message, e.g. "t_test"
#' @param var_name Dependent variable
#' @param reason Short reason (from .dv_degenerate_reason() or similar)
#' @param group_info Optional one-row data frame of group keys
#' @noRd
.warn_not_computed <- function(fn, var_name, reason, group_info = NULL) {
  where <- .where_group(group_info)
  cli_warn(c(
    "{.fn {fn}}: {.var {var_name}} not computed{where}.",
    "i" = "{reason}"
  ), call = NULL)
}

#' " in group region = East" for warnings ("" when ungrouped)
#' @param group_info Optional one-row data frame of group keys
#' @noRd
.where_group <- function(group_info = NULL) {
  if (!is.null(group_info) && ncol(group_info) > 0) {
    paste0(" in group ", .format_group_label(group_info))
  } else ""
}

#' Degrees of freedom for display
#'
#' Whole numbers without decimals (2419), others with `digits` decimals
#' (Welch 2384.147; weighted sum(w) - 1), as in SPSS tables. NA -> "".
#' @noRd
.fmt_df <- function(df, digits = 3) {
  df <- as.numeric(df)
  whole <- !is.na(df) & abs(df - round(df)) < 1e-8
  out <- fmt_num(df, digits)
  out[whole] <- formatC(round(df[whole]), format = "d")
  out
}

#' Bordered table with display-width (UTF-8 safe) padding
#'
#' print_stat_table() pads with sprintf(), which counts bytes: a label with
#' umlauts ("Hochschulabschluss (Universit\u00e4t)") ends up one column short
#' per multi-byte character. Pre-padding every cell (and header) to the
#' column's display width with pad_utf8() leaves sprintf() nothing to pad.
#' The first column is left-aligned, the others right-aligned (as in
#' print_stat_table()). All cells are expected to be formatted already.
#'
#' @param df Data frame of pre-formatted (character) columns
#' @param col_labels Named character vector of header labels
#' @param left Number of leading (label) columns to left-align
#' @noRd
.print_table_utf8 <- function(df, col_labels = NULL, left = 1L) {
  labels <- stats::setNames(names(df), names(df))
  if (!is.null(col_labels)) labels[names(col_labels)] <- col_labels
  for (j in seq_along(df)) {
    nm <- names(df)[j]
    vals <- as.character(df[[j]])
    vals[is.na(vals)] <- ""
    w <- max(nchar(c(labels[[nm]], vals), type = "width"), 1L)
    align <- if (j <= left) "left" else "right"
    df[[j]] <- vapply(vals, pad_utf8, character(1), width = w, align = align,
                      USE.NAMES = FALSE)
    labels[[nm]] <- pad_utf8(labels[[nm]], w, align)
  }
  print_stat_table(df, col_types = stats::setNames(rep("char", ncol(df)), names(df)),
                   col_labels = labels)
}

#' Coefficients for display: fixed decimals, tiny values in e-notation
#'
#' A covariate slope such as 2.46e-05 printed as "0.000 [0.000, 0.000]"
#' with 3 decimals; values that would round to zero keep their digits.
#' @noRd
.fmt_coef <- function(x, digits = 3) {
  x <- as.numeric(x)
  out <- fmt_num(x, digits)
  tiny <- !is.na(x) & x != 0 & abs(x) < 0.5 * 10^(-digits)
  out[tiny] <- formatC(x[tiny], format = "e", digits = max(digits - 1, 1))
  out
}

#' Type III terms that can be tested
#'
#' A term whose Type III hypothesis has no degrees of freedom (the design
#' has empty cells, so the term is aliased) cannot be tested: SPSS prints
#' it with df 0 and no F. mariposa printed "F(0, 2208) = NaN" (factorial)
#' or F = -Inf from a -0.000 sum of squares (ANCOVA).
#'
#' @return list(ss, ms, f, p, eta, note) for the terms
#' @noRd
.type3_testable_terms <- function(term_ss, term_df, ms_error, ss_error,
                                  df_error) {
  testable <- !is.na(term_df) & term_df > 0
  ss <- ifelse(testable, term_ss, 0)
  ms <- ifelse(testable, ss / pmax(term_df, 1), NA_real_)
  f <- ms / ms_error
  p <- rep(NA_real_, length(f))
  p[testable] <- stats::pf(f[testable], term_df[testable], df_error,
                           lower.tail = FALSE)
  eta <- ifelse(testable, ss / (ss + ss_error), NA_real_)
  note <- ifelse(testable, NA_character_,
                 "not testable: the design has empty cells")
  list(ss = ss, ms = ms, f = f, p = p, eta = eta, note = note)
}

#' Warn about empty cells of a factorial design
#'
#' @param fn Function name for the message
#' @param data Complete-case data with the factors (factors, unused levels
#'   dropped)
#' @param between_names Factor names
#' @param group_info Optional group keys (grouped analyses)
#' @noRd
.warn_empty_cells <- function(fn, data, between_names, group_info = NULL) {
  if (length(between_names) < 2) return(invisible(character(0)))
  tab <- table(data[between_names])
  idx <- which(tab == 0, arr.ind = TRUE)
  if (length(idx) == 0) return(invisible(character(0)))
  idx <- matrix(idx, ncol = length(between_names))
  dn <- dimnames(tab)
  cells <- apply(idx, 1, function(r) {
    paste(vapply(seq_along(r), function(k) {
      paste(between_names[k], "=", dn[[k]][r[k]])
    }, character(1)), collapse = " x ")
  })
  shown <- if (length(cells) > 3) {
    c(cells[1:3], sprintf("and %d more", length(cells) - 3))
  } else cells
  where <- .where_group(group_info)
  n_cells <- length(cells)
  cell_list <- paste(shown, collapse = "; ")
  cli_warn(c(
    "{.fn {fn}}: the design has {n_cells} empty cell{?s}{where}: {cell_list}.",
    "i" = "Effects that cannot be tested with Type III sums of squares are reported as not computed."
  ), call = NULL)
  invisible(cells)
}

#' Abort (ungrouped) or skip the group (grouped) when a model cannot be fit
#' @noRd
.fit_problem <- function(reason, dv_name, group_info = NULL,
                         call = rlang::caller_env()) {
  if (!is.null(group_info)) .not_computed(reason)
  cli_abort(c(
    "Dependent variable {.var {dv_name}} cannot be analysed.",
    "x" = "{reason}"
  ), call = call)
}

#' Run a model fit once per group_by() group
#'
#' @param fn Function name for warnings
#' @param data grouped_df
#' @param dv_name Dependent variable (for warnings)
#' @param fit_fun function(group_data, group_info) returning a result list
#' @return list(group_vars, keys, fits, notes): fits is NULL (and notes the
#'   reason) for a group that could not be analysed; a warning names it
#' @noRd
.grouped_model_fits <- function(fn, data, dv_name, fit_fun) {
  group_vars <- dplyr::group_vars(data)
  data_list <- dplyr::group_split(data)
  keys <- dplyr::group_keys(data)
  notes <- rep(NA_character_, length(data_list))
  fits <- lapply(seq_along(data_list), function(i) {
    gi <- keys[i, , drop = FALSE]
    skip <- function(e) {
      notes[i] <<- conditionMessage(e)
      .warn_not_computed(fn, dv_name, conditionMessage(e), gi)
      NULL
    }
    tryCatch({
      res <- fit_fun(dplyr::ungroup(data_list[[i]]), gi)
      res$group_info <- gi
      res
    }, mariposa_not_computed = skip, error = skip)
  })
  list(group_vars = group_vars, keys = keys, fits = fits, notes = notes)
}

#' Bind one table of every group fit, group keys as leading columns
#' @noRd
.bind_group_tables <- function(fits, keys, element) {
  rows <- lapply(seq_along(fits), function(i) {
    tab <- fits[[i]][[element]]
    if (is.null(tab) || nrow(tab) == 0) return(NULL)
    dplyr::bind_cols(keys[rep(i, nrow(tab)), , drop = FALSE],
                     tibble::as_tibble(tab))
  })
  dplyr::bind_rows(rows)
}

#' Iterate the per-group fits of a grouped factorial_anova / ancova result
#'
#' @param x Grouped result (group_results, group_keys, group_notes)
#' @param fun function(fit, label) printing one group
#' @param style "compact" ("[label]" lines) or "header" (print_group_header)
#' @noRd
.print_grouped_fits <- function(x, fun, style = c("compact", "header")) {
  style <- match.arg(style)
  for (i in seq_along(x$group_results)) {
    keys <- x$group_keys[i, , drop = FALSE]
    label <- .format_group_label(keys)
    fit <- x$group_results[[i]]
    if (style == "header") print_group_header(keys)
    if (is.null(fit)) {
      note <- x$group_notes[i]
      if (style == "compact") cat(sprintf("[%s]\n", label))
      cat(sprintf("  not computed (%s)\n",
                  if (!is.na(note)) note else "see warning"))
      next
    }
    fun(fit, label)
  }
  invisible(NULL)
}
