# Helper Functions for Weighted Statistical Functions
# ===================================================
# This file contains shared utility functions used across all w_* functions
# to ensure consistency and reduce code duplication.

#' @importFrom dplyr %>%
NULL

# Weight Validation Functions
# ---------------------------

#' Check if weights are valid (similar to datawizard)
#' @param weights Numeric vector of weights
#' @return Logical indicating if weights are valid
#' @noRd
.are_weights <- function(weights) {
  !is.null(weights) && is.numeric(weights) && length(weights) > 0
}

#' Validate weights (similar to datawizard)
#' @param weights Numeric vector of weights
#' @param verbose Logical; if TRUE, show warnings
#' @return Logical indicating if weights are positive
#' @noRd
.validate_weights <- function(weights, verbose = TRUE) {
  if (!.are_weights(weights)) {
    return(FALSE)
  }

  pos <- all(weights > 0, na.rm = TRUE)

  if (isTRUE(!pos) && isTRUE(verbose)) {
    cli_warn("Some weights were negative or zero. Weighting not carried out.")
  }

  pos
}

#' Enforce the package-wide weights policy
#'
#' Single policy applied by every weighted entry point: weights must be
#' numeric and non-negative, otherwise the call errors. Zero weights are
#' allowed (the case contributes nothing to weighted sums; SPSS likewise
#' excludes zero-weighted cases), NA weights are handled by each caller's
#' na.rm logic. This replaces the historical mix of silent
#' fall-back-to-unweighted, warn-and-continue, and no validation at all,
#' which produced different results for the same input depending on the
#' entry point.
#'
#' @param weights Numeric vector of weights (NULL is allowed and ignored)
#' @param name Optional display name of the weights variable for messages
#' @return invisible(TRUE); errors on violation
#' @noRd
.check_weights <- function(weights, name = NULL, call = rlang::caller_env()) {
  if (is.null(weights)) return(invisible(TRUE))
  label <- if (!is.null(name)) paste0("Weights variable {.var ", name, "}") else "Weights"
  if (!is.numeric(weights)) {
    cli_abort(paste0(label, " must be numeric, not {.cls {class(weights)[1]}}."),
              call = call)
  }
  n_neg <- sum(weights < 0, na.rm = TRUE)
  if (n_neg > 0) {
    cli_abort(c(
      paste0(label, " must not contain negative values."),
      "x" = "{n_neg} negative weight{?s} found.",
      "i" = "Negative weights are not meaningful for survey analysis."
    ), call = call)
  }
  invisible(TRUE)
}

# Data Processing Functions
# -------------------------

#' Process variable selection using tidyselect
#' @description
#' Central function for processing variable selection expressions using tidyselect.
#' Handles the ... expressions passed to statistical functions and returns a named
#' vector of column positions.
#'
#' @param data A data frame
#' @param ... Variable selection expressions (tidyselect compatible)
#' @return Named integer vector of column positions
#' @noRd
.process_variables <- function(data, ..., call = rlang::caller_env()) {
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.", call = call)
  }

  vars <- tidyselect::eval_select(rlang::expr(c(...)), data)

  if (length(vars) == 0) {
    cli_abort("No variables selected. Please specify at least one variable.", call = call)
  }

  vars
}

#' Process weights parameter
#' @description
#' Central function for processing the weights parameter in statistical functions.
#' Handles both NULL weights (unweighted) and specified weights with validation.
#'
#' @param data A data frame
#' @param weights_quo Quoted expression for weights (from rlang::enquo)
#' @return List with vector (numeric vector or NULL) and name (character or NULL)
#' @noRd
.process_weights <- function(data, weights_quo, call = rlang::caller_env()) {
  if (rlang::quo_is_null(weights_quo)) {
    return(list(vector = NULL, name = NULL))
  }

  weights_name <- rlang::as_name(weights_quo)

  if (!weights_name %in% names(data)) {
    cli_abort("Weights variable {.var {weights_name}} not found in data.", call = call)
  }

  weights_vec <- data[[weights_name]]

  if (!is.numeric(weights_vec)) {
    cli_abort("Weights variable {.var {weights_name}} must be numeric.", call = call)
  }

  .check_weights(weights_vec, weights_name, call = call)

  list(vector = weights_vec, name = weights_name)
}

#' Calculate effective sample size for weighted data
#' @param weights Numeric vector of weights
#' @return Effective sample size
#' @noRd
.effective_n <- function(weights) {
  if (is.null(weights) || !.are_weights(weights)) {
    return(length(weights))
  }
  sum(weights, na.rm = TRUE)^2 / sum(weights^2, na.rm = TRUE)
}

# Value Label Functions
# --------------------

#' Get value labels for a variable
#' @param x The data vector (factor or haven-labelled)
#' @param freq_names Values to look up labels for
#' @return Character vector of labels (or empty strings if no labels)
#' @noRd
get_value_labels <- function(x, freq_names) {
  if (is.factor(x)) {
    factor_levels <- levels(x)
    ifelse(is.na(freq_names), NA, factor_levels[match(freq_names, factor_levels)] %||% as.character(freq_names))
  } else if (!is.null(attr(x, "labels"))) {
    value_labels <- attr(x, "labels")
    ifelse(is.na(freq_names), NA, names(value_labels)[match(freq_names, value_labels)] %||% as.character(freq_names))
  } else {
    # For variables without value labels, return empty strings instead of duplicating values
    rep("", length(freq_names))
  }
}

#' Build a named vector mapping raw values to display labels
#' @param x The original data vector (factor or haven-labelled)
#' @return Named character vector (names = raw values as strings, values = display labels).
#'   Returns NULL if no meaningful labels exist.
#' @noRd
.build_label_map <- function(x) {
  if (is.factor(x)) {
    lvls <- levels(x)
    # Check if levels are just sequential numbers (not meaningful)
    numeric_levels <- suppressWarnings(as.numeric(lvls))
    if (all(!is.na(numeric_levels) & numeric_levels == seq_along(lvls))) {
      return(NULL)
    }
    # Factor levels are the labels; map level string to itself
    result <- stats::setNames(lvls, lvls)
    return(result)
  }
  if (!is.null(attr(x, "labels"))) {
    value_labels <- attr(x, "labels")
    # Map: names are the raw numeric values (as strings), values are the label text
    result <- stats::setNames(names(value_labels), as.character(value_labels))
    return(result)
  }
  return(NULL)
}

#' Truncate labels that exceed a maximum width
#' @param labels Character vector of labels
#' @param max_width Maximum character width
#' @return Character vector with truncated labels
#' @noRd
.truncate_labels <- function(labels, max_width = 20) {
  ifelse(nchar(labels) > max_width,
         paste0(substr(labels, 1, max_width - 3), "..."),
         labels)
}

#' Apply label map to a set of level names and truncate
#' @param levels Character vector of raw level names
#' @param label_map Named character vector from .build_label_map(), or NULL
#' @param max_width Maximum character width for truncation
#' @return Character vector of display labels
#' @noRd
.apply_labels <- function(levels, label_map, max_width = 20) {
  if (is.null(label_map)) return(levels)
  mapped <- label_map[levels]
  display <- ifelse(is.na(mapped), levels, mapped)
  .truncate_labels(display, max_width)
}

#' Relabel a matrix's dimnames using label maps
#' @param mat A matrix with dimnames
#' @param label_maps Named list of label maps (one per dimension)
#' @param max_width Maximum character width for truncation
#' @return Matrix with relabelled dimnames
#' @noRd
.relabel_matrix <- function(mat, label_maps, max_width = 20) {
  if (is.null(mat) || is.null(label_maps)) return(mat)
  dn <- dimnames(mat)
  for (i in seq_along(dn)) {
    dim_name <- names(dn)[i]
    lm <- label_maps[[dim_name]]
    if (!is.null(lm)) {
      dn[[i]] <- .apply_labels(dn[[i]], lm, max_width)
    }
  }
  dimnames(mat) <- dn
  mat
}

#' Require the haven package for an import/export feature
#'
#' Central guard replacing 20+ hand-rolled requireNamespace blocks
#' (pattern borrowed from .check_openxlsx2 in write_xlsx.R).
#'
#' @param purpose Short text naming the feature, e.g. "SPSS import"
#' @noRd
.check_haven <- function(purpose) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg haven} is required for {purpose}.",
      "i" = "Install it with: {.code install.packages(\"haven\")}"
    ), call = rlang::caller_env())
  }
  invisible(TRUE)
}

#' Warn (once per session) that a dot-case argument is deprecated
#'
#' Soft-deprecation bridge for the sjmisc-heritage dot-case argument names
#' renamed to snake_case (see VERSIONING_POLICY.md, section 4). Callers pass
#' the old value through when the new argument was not supplied:
#'
#'   if (!is.null(sort.frq)) {
#'     .warn_deprecated_arg("sort.frq", "sort_frq")
#'     if (missing(sort_frq)) sort_frq <- sort.frq
#'   }
#'
#' @param old_name The deprecated dot-case argument name (string)
#' @param new_name The replacement snake_case argument name (string)
#' @noRd
.warn_deprecated_arg <- function(old_name, new_name) {
  cli::cli_warn(
    "The {.arg {old_name}} argument is deprecated; use {.arg {new_name}} instead.",
    .frequency = "once",
    .frequency_id = paste0("mariposa_", old_name)
  )
  invisible(NULL)
}

#' Error because a removed dot-case argument was used
#'
#' Hard-error stub for dot-case argument names whose deprecation bridge was
#' removed (see VERSIONING_POLICY.md, section 4). Used only in functions
#' whose `...` is consumed by tidyselect: full removal of the formal would
#' let the old name land silently in `...` and be misinterpreted as a
#' variable selection. Keeping the formal as a `NULL` sentinel guarantees a
#' clear error instead.
#'
#' @param old_name The removed dot-case argument name (string)
#' @param new_name The replacement snake_case argument name (string)
#' @noRd
.stop_removed_arg <- function(old_name, new_name) {
  cli::cli_abort(
    "The {.arg {old_name}} argument was removed in mariposa 0.6.9; use {.arg {new_name}} instead.",
    call = rlang::caller_env()
  )
}
