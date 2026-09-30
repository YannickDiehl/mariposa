# =============================================================================
# Factory for Weighted Statistics Functions (w_*)
# =============================================================================
# This file provides the shared infrastructure for all w_* functions.
# Each w_* function delegates to .w_statistic() with its specific computation
# function, reducing ~3,000 lines of duplicated boilerplate to a single
# shared implementation.
#
# Architecture:
#   w_mean(data, ..., weights)  -->  .w_statistic(data, ..., stat_fn, ...)
#   w_sd(data, ..., weights)    -->  .w_statistic(data, ..., stat_fn, ...)
#   ...etc
# =============================================================================

#' Internal factory function for all weighted statistics
#'
#' Handles summarise-context detection, data frame processing, grouped/ungrouped
#' flow, result structuring, and S3 object creation for all w_* functions.
#'
#' @param data Data frame or numeric vector (in summarise context)
#' @param ... Variable selection (tidyselect)
#' @param weights Weight variable (unquoted)
#' @param na.rm Remove missing values
#' @param stat_fn Function(x, w) that computes the statistic. Returns a scalar,
#'   or - if `multi_value = TRUE` - a named vector with one value per
#'   requested quantity (e.g. one value per quantile probability).
#'   Takes (x, w) where x is the (cleaned) data vector, w is the weight
#'   vector or NULL. Values need not be numeric (e.g. the mode of a
#'   factor/character variable).
#' @param stat_name Short name for the statistic (e.g., "mean", "sd")
#' @param weighted_col Column name for weighted results (e.g., "weighted_mean").
#'   Unused when `multi_value = TRUE`.
#' @param unweighted_col Column name for unweighted results (e.g., "mean").
#'   Unused when `multi_value = TRUE`.
#' @param class_name S3 class name (e.g., "w_mean")
#' @param extra_args Named list of extra arguments stored in the result object
#' @param multi_value If TRUE, stat_fn returns a named vector; each value
#'   becomes its own `{var}_{name}` column and results stay in wide format
#'   (no long-format assembly).
#' @param value_names Optional character vector of display names for
#'   multi-value results (applied in data-frame mode only; summarise context
#'   keeps stat_fn's own names).
#' @param empty_stat Value stored when a weighted computation has zero valid
#'   observations (default NA_real_ for scalar-numeric statistics).
#' @param empty_n n/effective_n value recorded for an unweighted computation
#'   with zero valid observations. Default 0L; w_modus historically records
#'   a double 0, which promotes the n columns to double across groups.
#' @param vector_ok Predicate deciding whether a non-data-frame `data` is
#'   treated as a vector in summarise context (default: numeric vectors only).
#' @return S3 object of class `class_name`, or scalar/named vector in
#'   summarise context. `$results` has one row per variable (and group):
#'   group columns, `Variable`, the statistic (`<stat>` or
#'   `weighted_<stat>`), `n` (valid cases; unweighted) or `weighted_n`
#'   (sum of the weights of the valid cases) plus `effective_n` (Kish;
#'   weighted), and `missing` (missing cases, weighted: sum of their
#'   weights). Multi-value statistics (w_quantile) stay wide:
#'   `<var>_<value>`, `<var>_n`, `<var>_eff_n`, `<var>_weighted_n`,
#'   `<var>_missing`.
#' @noRd
.w_statistic <- function(data, ..., weights = NULL, na.rm = TRUE,
                         stat_fn, stat_name, weighted_col = NULL,
                         unweighted_col = NULL,
                         class_name, extra_args = list(),
                         multi_value = FALSE, value_names = NULL,
                         empty_stat = NA_real_, empty_n = 0L,
                         vector_ok = is.numeric,
                         call = rlang::caller_env()) {

  # Capture the weights expression once as a quosure. Because the w_*
  # wrappers pass weights = {{ weights }}, this quosure carries the
  # *original* caller environment - inside summarise() that is the data
  # mask, so eval_tidy() resolves bare column names without any
  # parent.frame() walking (which the previous implementation relied on).
  weights_quo <- rlang::enquo(weights)

  # --- Summarise context: data is a vector, not a data frame -----------------
  if (!is.data.frame(data) && vector_ok(data)) {
    # `...` selects variables of a data frame; with a vector it is unused
    # (`w_mean(x, weight = w)` silently computed an unweighted mean)
    .check_dots_unused(..., call = call)
    x <- data
    weights_vec <- if (rlang::quo_is_null(weights_quo)) NULL else rlang::eval_tidy(weights_quo)
    weighted <- !is.null(weights_vec)
    if (weighted) {
      # Type check before stripping: a factor must not pass as its codes,
      # a character vector must not fall back to unweighted
      .check_weights(weights_vec, call = call)
      weights_vec <- .plain_numeric(weights_vec)
      if (length(weights_vec) == 1L) weights_vec <- rep(weights_vec, length(x))
    }
    if (weighted && length(weights_vec) != length(x)) {
      cli_abort(c(
        "{.arg weights} and the data must have the same length.",
        "x" = "The data have {length(x)} value{?s}, {.arg weights} has {length(weights_vec)}."
      ), call = call)
    }
    # na.rm = FALSE with missing values: the statistic is undefined (NA),
    # as in base R - never a crash or a value computed on shifted positions
    if (!na.rm && .w_has_missing(x, if (weighted) weights_vec)) {
      return(empty_stat)
    }
    if (!weighted) {
      # Unweighted
      if (na.rm) x <- x[!is.na(x)]
      # Empty input: same short-circuit as the weighted branch (stat_fn on
      # numeric(0) gave e.g. mean() = NaN)
      if (length(x) == 0) return(empty_stat)
      return(.w_strip_extra(stat_fn(x, w = NULL)))
    } else {
      # Weighted
      if (na.rm) {
        valid <- !is.na(x) & !is.na(weights_vec)
        x <- x[valid]
        weights_vec <- weights_vec[valid]
      }
      if (length(x) == 0) return(empty_stat)
      return(.w_strip_extra(stat_fn(x, w = weights_vec)))
    }
  }

  # --- Data frame mode -------------------------------------------------------
  if (!is.data.frame(data)) {
    if (...length() == 0 && is.atomic(data)) {
      # A vector of the wrong type (e.g. character): say so instead of
      # claiming that a data frame was expected
      cli_abort(c(
        "{.fn {class_name}} needs a numeric vector or a data frame.",
        "x" = "{.arg data} is a {.cls {class(data)[1]}} vector."
      ), call = call)
    }
    cli_abort("{.arg data} must be a data frame.", call = call)
  }

  vars <- .process_variables(data, ..., call = call)
  var_names <- names(vars)

  # Selected variables must suit the statistic (numeric, except for the
  # mode): a factor used to give NA plus a base-R warning
  bad <- var_names[!vapply(var_names, function(v) vector_ok(data[[v]]), logical(1))]
  if (length(bad) > 0) {
    cli_abort(c(
      "Variable{?s} {.var {bad}} {?is/are} not numeric.",
      "i" = "{.fn {class_name}} needs numeric variables; use {.fn w_modus} or {.fn frequency} for categorical ones."
    ), call = call)
  }

  # The package-wide weights entry (column, string, all_of(), expression;
  # bare numbers in the vector AND the column, which the grouped path
  # re-reads)
  weights_info <- .process_weights(data, weights_quo, call = call)
  data <- weights_info$data
  weights_vec <- weights_info$vector
  weights_name <- weights_info$name

  is_grouped <- inherits(data, "grouped_df")

  # --- Compute per variable (grouped or ungrouped) ---------------------------
  .compute_vars <- function(df, w_vec) {
    result_cols <- list()
    for (var_name in var_names) {
      x <- df[[var_name]]
      # na.rm = FALSE with missing values: NA statistic (see vector mode)
      undefined <- !na.rm && .w_has_missing(x, w_vec)

      if (is.null(w_vec)) {
        n_missing <- sum(is.na(x))
        n_valid <- length(x) - n_missing
        if (na.rm) x <- x[!is.na(x)]
        stat_val <- if (length(x) == 0 || undefined) empty_stat else stat_fn(x, w = NULL)
        n_val <- if (n_valid == 0) empty_n else n_valid
        eff_n <- n_val
      } else {
        # SPSS WEIGHT BY: N and Missing are sums of weights; cases without
        # a weight count nowhere
        has_w <- !is.na(w_vec)
        valid <- !is.na(x) & has_w
        n_missing <- sum(w_vec[is.na(x) & has_w])
        sum_w <- sum(w_vec[valid])
        if (na.rm) {
          x <- x[valid]
          w <- w_vec[valid]
        } else {
          w <- w_vec
        }

        if (length(x) == 0) {
          stat_val <- empty_stat
          n_val <- 0
          eff_n <- 0
        } else {
          stat_val <- if (undefined) empty_stat else stat_fn(x, w = w)
          n_val <- sum(valid)
          eff_n <- .effective_n(w_vec[valid])
        }
      }

      # Extra per-variable information returned by stat_fn as attribute
      # "w_extra" (w_modus: the number of modes) becomes its own column
      extra <- attr(stat_val, "w_extra")
      if (!is.null(extra)) {
        stat_val <- .w_strip_extra(stat_val)
        for (e in names(extra)) {
          result_cols[[paste0(var_name, "_", e)]] <- extra[[e]]
        }
      }

      if (multi_value) {
        # One column per value; elements keep their names attribute
        # (byte-compatible with the historic bespoke implementations).
        if (!is.null(value_names)) names(stat_val) <- value_names
        for (j in seq_along(stat_val)) {
          result_cols[[paste0(var_name, "_", names(stat_val)[j])]] <- stat_val[j]
        }
      } else {
        result_cols[[var_name]] <- stat_val
      }
      result_cols[[paste0(var_name, "_n")]] <- n_val
      result_cols[[paste0(var_name, "_eff_n")]] <- eff_n
      if (!is.null(w_vec)) result_cols[[paste0(var_name, "_weighted_n")]] <- sum_w
      result_cols[[paste0(var_name, "_missing")]] <- n_missing
    }
    tibble::tibble(!!!result_cols)
  }

  if (is_grouped) {
    group_vars <- dplyr::group_vars(data)

    results <- data %>%
      dplyr::group_modify(~ {
        w_vec <- if (!is.null(weights_name)) .x[[weights_name]] else NULL
        .compute_vars(.x, w_vec)
      }) %>%
      dplyr::ungroup()
  } else {
    results <- .compute_vars(data, weights_vec)
  }

  # --- Transform to standardized long/wide format ---------------------------
  # Multi-value statistics stay in wide format (one column per value).
  final_results <- if (multi_value) {
    results
  } else {
    .w_format_results(
      results, var_names, weights_name,
      group_vars = if (is_grouped) dplyr::group_vars(data) else character(0),
      weighted_col = weighted_col,
      unweighted_col = unweighted_col
    )
  }

  # --- Create S3 object -----------------------------------------------------
  result <- c(
    list(
      results = final_results,
      variables = var_names,
      weights = weights_name,
      is_grouped = is_grouped,
      groups = if (is_grouped) dplyr::group_vars(data) else NULL
    ),
    extra_args
  )

  class(result) <- class_name
  result
}


#' Format raw w_* results into the standard long format
#'
#' One row per variable (and group combination), the same columns for a
#' single and for several variables: single-variable results used to carry
#' the raw computation columns (`age`, `age_n`, `age_eff_n`) next to the
#' formatted ones.
#'
#' @param results Raw tibble from computation (one row per group)
#' @param var_names Character vector of variable names
#' @param weights_name Weight variable name or NULL
#' @param group_vars Grouping column names (character(0) when ungrouped)
#' @param weighted_col Name for weighted statistic column
#' @param unweighted_col Name for unweighted statistic column
#' @return Formatted tibble
#' @noRd
.w_format_results <- function(results, var_names, weights_name,
                              group_vars = character(0),
                              weighted_col, unweighted_col) {
  weighted <- !is.null(weights_name)

  # Statistic values per variable. With several variables they share one
  # column: factors (the mode of a factor) become character, and if the
  # types still differ (numeric mode next to a text mode) all values are
  # shown as text. Label classes are dropped (bare numbers).
  vals <- lapply(var_names, function(v) {
    val <- results[[v]]
    if (inherits(val, "haven_labelled")) val <- .plain_numeric(val)
    if (length(var_names) > 1 && is.factor(val)) val <- as.character(val)
    val
  })
  if (length(var_names) > 1 &&
      length(unique(vapply(vals, function(v) class(v)[1], character(1)))) > 1) {
    vals <- lapply(vals, as.character)
  }

  parts <- lapply(seq_along(var_names), function(i) {
    v <- var_names[i]
    out <- results[group_vars]
    out$Variable <- rep(v, nrow(results))
    if (weighted) {
      out[[weighted_col]] <- vals[[i]]
      out$weighted_n <- results[[paste0(v, "_weighted_n")]]
      out$effective_n <- results[[paste0(v, "_eff_n")]]
    } else {
      out[[unweighted_col]] <- vals[[i]]
      out$n <- results[[paste0(v, "_n")]]
    }
    out$missing <- results[[paste0(v, "_missing")]]
    n_modes_col <- paste0(v, "_n_modes")
    if (n_modes_col %in% names(results)) out$n_modes <- results[[n_modes_col]]
    out
  })
  dplyr::bind_rows(parts)
}


#' Generic print method for w_* statistic objects
#'
#' Shared print implementation used by all standard w_* functions (and
#' their summary() output): one table per group with Variable, the
#' statistic, N and Missing. With weights, N and Missing are sums of
#' weights (display-rounded) as in SPSS; Kish's effective N is shown only
#' by summary().
#'
#' @param x A w_* object (or its summary object)
#' @param stat_label Column header for the statistic (e.g., "Mean")
#' @param weighted_col Column name for weighted values
#' @param unweighted_col Column name for unweighted values
#' @param digits Number of decimal places to display (default: 3)
#' @param effective_n Show Kish's effective N (weighted results only)?
#' @param title Statistic name used in the title (default: stat_label)
#' @noRd
.print_w_statistic <- function(x, stat_label, weighted_col, unweighted_col,
                               digits = 3, effective_n = FALSE,
                               title = stat_label) {
  weighted <- !is.null(x$weights)
  print_header(get_standard_title(title, x$weights, "Statistics"))
  if (weighted) cat("Weights: ", x$weights, "\n", sep = "")

  stat_col <- if (weighted) weighted_col else unweighted_col
  multiple_modes <- FALSE

  emit <- function(rows) {
    val <- rows[[stat_col]]
    if (is.factor(val)) val <- as.character(val)
    # Several values share the highest frequency: flag the cell (SPSS
    # footnote "Multiple modes exist. The smallest value is shown.")
    tied <- if ("n_modes" %in% names(rows)) !is.na(rows$n_modes) & rows$n_modes > 1 else FALSE
    if (any(tied)) {
      txt <- if (is.numeric(val)) fmt_num(val, digits) else as.character(val)
      val <- paste0(txt, ifelse(tied, " (a)", ""))
      multiple_modes <<- TRUE
    }
    tab <- data.frame(Variable = rows$Variable, stringsAsFactors = FALSE)
    tab[[stat_label]] <- val
    tab$N <- if (weighted) rows$weighted_n else rows$n
    tab$Missing <- rows$missing
    if (weighted && effective_n) tab[["Effective N"]] <- rows$effective_n
    .print_desc_table(tab, digits = digits, col_digits = c("Effective N" = 1))
  }

  if (isTRUE(x$is_grouped)) {
    for_each_group(x$results, x$groups, function(rows, combo) {
      cat("\n")
      emit(rows)
    })
  } else {
    cat("\n")
    emit(x$results)
  }
  if (multiple_modes) {
    cat("  (a) Multiple modes exist; the smallest value is shown.\n")
  }
  if (weighted && effective_n) {
    cat("  N and Missing are sums of weights; Effective N = (sum w)^2 / sum w^2 (Kish).\n")
  }
  invisible(x)
}


#' Summary object for the standard w_* statistics
#'
#' summary() adds Kish's effective sample size to the table (weighted
#' results) and a digits option; the statistic metadata travels with the
#' object so print.summary.w_statistic() needs no per-class method.
#'
#' @noRd
.w_summary <- function(object, stat_label, weighted_col, unweighted_col,
                       effective_n = TRUE, digits = 3, title = stat_label) {
  out <- build_summary_object(
    object,
    show = list(statistics = TRUE, effective_n = effective_n),
    digits = digits,
    class_name = "summary.w_statistic"
  )
  out$stat_info <- list(label = stat_label, weighted_col = weighted_col,
                        unweighted_col = unweighted_col, title = title)
  out
}

#' Print summary of a w_* statistic (detailed output)
#'
#' @param x A \code{summary.w_statistic} object created by
#'   \code{summary()} on a \code{w_*} result.
#' @param ... Additional arguments (not used).
#' @return Invisibly returns the input object \code{x}.
#' @export
#' @method print summary.w_statistic
print.summary.w_statistic <- function(x, ...) {
  info <- x$stat_info
  .print_w_statistic(x, info$label, info$weighted_col, info$unweighted_col,
                     digits = x$digits,
                     effective_n = isTRUE(x$show$effective_n),
                     title = info$title)
}


#' Does a variable (or its weights) contain missing values?
#'
#' With na.rm = FALSE a statistic over data with missing values is
#' undefined (NA), as in base R. Used by the w_* factory and describe().
#'
#' @param x Data vector
#' @param w Weight vector or NULL
#' @return Logical scalar
#' @noRd
.w_has_missing <- function(x, w = NULL) {
  anyNA(x) || (!is.null(w) && anyNA(w))
}


#' Percent label of a probability, with at most two decimals
#'
#' "33.33" for p = 1/3 instead of "33.3333333333333"; more decimals only
#' when two requested probabilities would otherwise get the same label.
#' Used for describe()'s Q-columns and w_quantile()'s columns.
#'
#' @param p Probabilities
#' @return Character vector
#' @noRd
.pct_label <- function(p) {
  pct <- p * 100
  for (d in 2:10) {
    nm <- as.character(round(pct, d))
    if (!anyDuplicated(nm) || anyDuplicated(pct)) break
  }
  nm
}


#' Remove the "w_extra" attribute from a statistic value
#' @noRd
.w_strip_extra <- function(val) {
  attr(val, "w_extra") <- NULL
  val
}
