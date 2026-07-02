# ============================================================================
# Label Conversion Functions
# ============================================================================
# Functions for converting between labelled, factor, character, and numeric
# representations of survey data.


# ============================================================================
# to_label() — Labelled → Factor
# ============================================================================

#' Convert Labelled Variables to Factors
#'
#' @description
#' Converts `haven_labelled` variables to factors, using value labels as
#' factor levels. This is the primary function for making labelled survey
#' data ready for plotting, cross-tabulation, and regression analysis.
#'
#' @param data A data frame, tibble, or a single vector.
#' @param ... Optional: unquoted variable names (tidyselect supported). If
#'   empty, converts all `haven_labelled` columns.
#' @param ordered If `TRUE`, creates an ordered factor. Default: `FALSE`.
#' @param drop_na If `TRUE` (default), tagged NAs are converted to regular
#'   `NA` (excluded from factor levels). If `FALSE`, tagged NAs are kept as
#'   factor levels with their label text.
#' @param drop_unused If `TRUE`, removes factor levels with zero observations.
#'   Default: `FALSE`.
#' @param add_non_labelled If `TRUE`, values without labels are included as
#'   factor levels using their numeric value as the level name.
#'   Default: `FALSE` (unlabelled values become `NA`).
#' @param drop.na,drop.unused,add.non.labelled Deprecated dot-case argument
#'   names. See the "Deprecated arguments" section below.
#'
#' @return The input with labelled variables converted to factors. For
#'   single vector input, returns a factor.
#'
#' @details
#' ## Deprecated arguments
#'
#' The dot-case argument names `drop.na`, `drop.unused`, and
#' `add.non.labelled` are deprecated; use the snake_case equivalents
#' `drop_na`, `drop_unused`, and `add_non_labelled` instead. The old names
#' still work but issue a deprecation warning (once per session) and will
#' be removed in a future release.
#'
#' For each labelled variable, the numeric codes are replaced by their
#' associated value labels. The resulting factor levels are ordered by the
#' original numeric values (not alphabetically).
#'
#' ## When to Use This
#'
#' Use `to_label()` when you:
#' \itemize{
#'   \item Want human-readable labels in plots (ggplot2)
#'   \item Need factor variables for regression models
#'   \item Want to create frequency tables with label text
#' }
#'
#' @seealso [to_character()] for character output, [to_labelled()] for the
#'   reverse operation, [val_labels()] for viewing labels
#'
#' @family labels
#'
#' @examples
#' # Convert a single variable
#' to_label(survey_data$gender)
#'
#' # Convert specific columns in a data frame
#' data <- to_label(survey_data, gender, region)
#'
#' # Convert all labelled columns
#' data <- to_label(survey_data)
#'
#' # Keep values without labels as factor levels
#' to_label(survey_data$life_satisfaction, add_non_labelled = TRUE)
#'
#' @export
to_label <- function(data, ..., ordered = FALSE, drop_na = TRUE,
                     drop_unused = FALSE, add_non_labelled = FALSE,
                     drop.na = NULL, drop.unused = NULL,
                     add.non.labelled = NULL) {
  # ---- Deprecated dot-case argument bridge (see VERSIONING_POLICY.md, 4) ----
  if (!is.null(drop.na)) {
    .warn_deprecated_arg("drop.na", "drop_na")
    if (missing(drop_na)) drop_na <- drop.na
  }
  if (!is.null(drop.unused)) {
    .warn_deprecated_arg("drop.unused", "drop_unused")
    if (missing(drop_unused)) drop_unused <- drop.unused
  }
  if (!is.null(add.non.labelled)) {
    .warn_deprecated_arg("add.non.labelled", "add_non_labelled")
    if (missing(add_non_labelled)) add_non_labelled <- add.non.labelled
  }

  if (!is.data.frame(data)) {
    return(.to_label_vec(data, ordered, drop_na, drop_unused,
                         add_non_labelled))
  }

  dots <- rlang::enexprs(...)
  if (length(dots) == 0L) {
    # Convert all haven_labelled columns
    cols <- which(vapply(data, inherits, logical(1), "haven_labelled"))
  } else {
    cols <- tidyselect::eval_select(rlang::expr(c(...)), data)
  }

  for (i in cols) {
    data[[i]] <- .to_label_vec(data[[i]], ordered, drop_na, drop_unused,
                                add_non_labelled)
  }

  data
}


#' Internal: convert a single vector to factor using labels
#' @noRd
.to_label_vec <- function(x, ordered = FALSE, drop_na = TRUE,
                          drop_unused = FALSE, add_non_labelled = FALSE) {
  # Already a factor? Return as-is
  if (is.factor(x)) return(x)

  labels <- attr(x, "labels", exact = TRUE)

  # No labels: convert to factor using unique values
  if (is.null(labels) || length(labels) == 0L) {
    if (is.character(x)) return(factor(x))
    return(factor(x))
  }

  # Separate valid labels from NA labels
  na_mask <- is.na(labels)
  valid_labels <- labels[!na_mask]
  na_labels <- labels[na_mask]

  # Build level order: sorted by numeric value
  level_order <- sort(valid_labels)
  level_names <- names(level_order)
  level_values <- unname(level_order)

  # Map data values to label text
  raw <- as.double(x)
  mapped <- rep(NA_character_, length(raw))

  for (j in seq_along(level_values)) {
    mapped[!is.na(raw) & raw == level_values[j]] <- level_names[j]
  }

  # Handle tagged NAs
  if (!isTRUE(drop_na) && length(na_labels) > 0L &&
      requireNamespace("haven", quietly = TRUE)) {
    na_idx <- which(is.na(raw))
    for (k in na_idx) {
      tag <- haven::na_tag(x[k])
      if (!is.na(tag)) {
        na_tag_labels <- vapply(na_labels, haven::na_tag, character(1))
        match_idx <- match(tag, na_tag_labels)
        if (!is.na(match_idx)) {
          mapped[k] <- names(na_labels)[match_idx]
          level_names <- c(level_names, names(na_labels)[match_idx])
        }
      }
    }
    level_names <- unique(level_names)
  }

  # Handle unlabelled values
  if (isTRUE(add_non_labelled)) {
    unlabelled_mask <- is.na(mapped) & !is.na(raw)
    if (any(unlabelled_mask)) {
      unlabelled_vals <- sort(unique(raw[unlabelled_mask]))
      unlabelled_names <- as.character(unlabelled_vals)
      mapped[unlabelled_mask] <- as.character(raw[unlabelled_mask])
      level_names <- c(level_names, unlabelled_names)
    }
  }

  result <- factor(mapped, levels = unique(level_names), ordered = ordered)

  if (isTRUE(drop_unused)) {
    result <- droplevels(result)
  }

  # Preserve variable label
  var_lbl <- attr(x, "label", exact = TRUE)
  if (!is.null(var_lbl)) attr(result, "label") <- var_lbl

  result
}


# ============================================================================
# to_character() — Labelled → Character
# ============================================================================

#' Convert Labelled Variables to Character
#'
#' @description
#' Converts `haven_labelled` variables to character vectors using value
#' labels as text. Same as [to_label()] but returns character instead of
#' factor.
#'
#' @param data A data frame, tibble, or a single vector.
#' @param ... Optional: unquoted variable names (tidyselect supported). If
#'   empty, converts all `haven_labelled` columns.
#' @param drop_na If `TRUE` (default), tagged NAs become regular `NA`.
#' @param add_non_labelled If `TRUE`, unlabelled values are included as
#'   their numeric string representation. Default: `FALSE`.
#' @param drop.na,add.non.labelled Deprecated dot-case argument names. See
#'   the "Deprecated arguments" section below.
#'
#' @return The input with labelled variables converted to character. For
#'   single vector input, returns a character vector.
#'
#' @details
#' ## Deprecated arguments
#'
#' The dot-case argument names `drop.na` and `add.non.labelled` are
#' deprecated; use the snake_case equivalents `drop_na` and
#' `add_non_labelled` instead. The old names still work but issue a
#' deprecation warning (once per session) and will be removed in a future
#' release.
#'
#' This is a convenience wrapper around [to_label()] that calls
#' `as.character()` on the result. Use this when you need character output
#' (e.g., for string operations) rather than factor output.
#'
#' The variable label (`"label"` attribute) is preserved on the output.
#'
#' @seealso [to_label()] for factor output, [val_labels()]
#'
#' @family labels
#'
#' @examples
#' # Convert a single variable to character
#' to_character(survey_data$gender)
#'
#' # Convert specific columns in a data frame
#' data <- to_character(survey_data, gender, region)
#'
#' @export
to_character <- function(data, ..., drop_na = TRUE,
                         add_non_labelled = FALSE,
                         drop.na = NULL, add.non.labelled = NULL) {
  # ---- Deprecated dot-case argument bridge (see VERSIONING_POLICY.md, 4) ----
  if (!is.null(drop.na)) {
    .warn_deprecated_arg("drop.na", "drop_na")
    if (missing(drop_na)) drop_na <- drop.na
  }
  if (!is.null(add.non.labelled)) {
    .warn_deprecated_arg("add.non.labelled", "add_non_labelled")
    if (missing(add_non_labelled)) add_non_labelled <- add.non.labelled
  }

  if (!is.data.frame(data)) {
    result <- .to_label_vec(data, ordered = FALSE, drop_na = drop_na,
                            drop_unused = FALSE,
                            add_non_labelled = add_non_labelled)
    out <- as.character(result)
    var_lbl <- attr(data, "label", exact = TRUE)
    if (!is.null(var_lbl)) attr(out, "label") <- var_lbl
    return(out)
  }

  dots <- rlang::enexprs(...)
  if (length(dots) == 0L) {
    cols <- which(vapply(data, inherits, logical(1), "haven_labelled"))
  } else {
    cols <- tidyselect::eval_select(rlang::expr(c(...)), data)
  }

  for (i in cols) {
    result <- .to_label_vec(data[[i]], ordered = FALSE, drop_na = drop_na,
                            drop_unused = FALSE,
                            add_non_labelled = add_non_labelled)
    var_lbl <- attr(data[[i]], "label", exact = TRUE)
    data[[i]] <- as.character(result)
    if (!is.null(var_lbl)) attr(data[[i]], "label") <- var_lbl
  }

  data
}


# ============================================================================
# to_numeric() — Factor/Labelled → Numeric
# ============================================================================

#' Convert Factors or Labelled Variables to Numeric
#'
#' @description
#' Converts factor levels or labelled values to numeric. For factors, uses
#' the underlying integer codes (or the numeric value of levels if they are
#' numeric strings). For labelled variables, extracts the underlying numeric
#' values.
#'
#' @param data A data frame, tibble, or a single vector.
#' @param ... Optional: unquoted variable names (tidyselect supported). If
#'   empty, converts all factor columns.
#' @param use_labels If `TRUE` (default), attempts to use the numeric value
#'   of factor levels (e.g., level `"3"` becomes `3`). If `FALSE`, uses
#'   sequential integers (1, 2, 3, ...).
#' @param start_at If not `NULL`, the lowest numeric value in the output
#'   starts at this number. Default: `NULL` (use original values).
#' @param keep_labels If `TRUE`, the former factor levels are stored as
#'   value labels on the result. Default: `FALSE`.
#' @param use.labels,start.at,keep.labels Deprecated dot-case argument
#'   names. See the "Deprecated arguments" section below.
#'
#' @return The input with variables converted to numeric. For single vector
#'   input, returns a numeric vector.
#'
#' @details
#' ## Deprecated arguments
#'
#' The dot-case argument names `use.labels`, `start.at`, and `keep.labels`
#' are deprecated; use the snake_case equivalents `use_labels`, `start_at`,
#' and `keep_labels` instead. The old names still work but issue a
#' deprecation warning (once per session) and will be removed in a future
#' release.
#'
#' This function handles three input types:
#' \enumerate{
#'   \item **Numeric factors** (levels like `"1"`, `"2"`, `"3"`): Extracts
#'     the numeric values from the level names.
#'   \item **Text factors** (levels like `"Male"`, `"Female"`): Converts
#'     to sequential integers by default; use `use_labels = FALSE` to force
#'     this behavior even for numeric-looking levels.
#'   \item **haven_labelled**: Extracts the underlying numeric vector,
#'     stripping the labelled class.
#' }
#'
#' @seealso [to_label()] for the reverse (numeric → factor),
#'   [to_labelled()] for creating labelled vectors
#'
#' @family labels
#'
#' @examples
#' # Numeric factor levels → numeric
#' x <- factor(c("1", "3", "5", "3"))
#' to_numeric(x)
#' # [1] 1 3 5 3
#'
#' # Sequential integers
#' to_numeric(x, use_labels = FALSE)
#' # [1] 1 2 3 2
#'
#' # Haven labelled → plain numeric
#' to_numeric(survey_data$life_satisfaction)
#'
#' @export
to_numeric <- function(data, ..., use_labels = TRUE, start_at = NULL,
                       keep_labels = FALSE,
                       use.labels = NULL, start.at = NULL,
                       keep.labels = NULL) {
  # ---- Deprecated dot-case argument bridge (see VERSIONING_POLICY.md, 4) ----
  if (!is.null(use.labels)) {
    .warn_deprecated_arg("use.labels", "use_labels")
    if (missing(use_labels)) use_labels <- use.labels
  }
  if (!is.null(start.at)) {
    .warn_deprecated_arg("start.at", "start_at")
    if (missing(start_at)) start_at <- start.at
  }
  if (!is.null(keep.labels)) {
    .warn_deprecated_arg("keep.labels", "keep_labels")
    if (missing(keep_labels)) keep_labels <- keep.labels
  }

  if (!is.data.frame(data)) {
    return(.to_numeric_vec(data, use_labels, start_at, keep_labels))
  }

  dots <- rlang::enexprs(...)
  if (length(dots) == 0L) {
    cols <- which(vapply(data, function(x) {
      is.factor(x) || inherits(x, "haven_labelled")
    }, logical(1)))
  } else {
    cols <- tidyselect::eval_select(rlang::expr(c(...)), data)
  }

  for (i in cols) {
    data[[i]] <- .to_numeric_vec(data[[i]], use_labels, start_at,
                                  keep_labels)
  }

  data
}


#' Internal: convert a single vector to numeric
#' @noRd
.to_numeric_vec <- function(x, use_labels = TRUE, start_at = NULL,
                            keep_labels = FALSE) {
  var_lbl <- attr(x, "label", exact = TRUE)

  # haven_labelled → extract underlying numeric
  if (inherits(x, "haven_labelled")) {
    old_labels <- attr(x, "labels", exact = TRUE)
    result <- as.double(x)
    if (!is.null(var_lbl)) attr(result, "label") <- var_lbl
    if (isTRUE(keep_labels) && !is.null(old_labels)) {
      attr(result, "labels") <- old_labels
    }
    return(result)
  }

  # Factor → numeric
  if (is.factor(x)) {
    lvls <- levels(x)

    if (isTRUE(use_labels)) {
      # Try to parse levels as numbers
      numeric_lvls <- suppressWarnings(as.numeric(lvls))
      if (!any(is.na(numeric_lvls))) {
        # Levels are numeric strings
        result <- numeric_lvls[as.integer(x)]
      } else {
        # Non-numeric levels: use sequential integers
        result <- as.integer(x)
      }
    } else {
      result <- as.integer(x)
    }

    # Apply start_at offset
    if (!is.null(start_at)) {
      min_val <- min(result, na.rm = TRUE)
      result <- result - min_val + start_at
    }

    if (!is.null(var_lbl)) attr(result, "label") <- var_lbl

    if (isTRUE(keep_labels)) {
      vals <- if (isTRUE(use_labels)) {
        numeric_lvls <- suppressWarnings(as.numeric(lvls))
        if (!any(is.na(numeric_lvls))) numeric_lvls else seq_along(lvls)
      } else {
        seq_along(lvls)
      }
      if (!is.null(start_at)) {
        vals <- vals - min(vals, na.rm = TRUE) + start_at
      }
      attr(result, "labels") <- stats::setNames(vals, lvls)
    }

    return(result)
  }

  # Already numeric
  if (is.numeric(x)) {
    if (!is.null(start_at)) {
      min_val <- min(x, na.rm = TRUE)
      x <- x - min_val + start_at
    }
    return(as.double(x))
  }

  # Character → try numeric conversion
  result <- suppressWarnings(as.numeric(x))
  if (!is.null(var_lbl)) attr(result, "label") <- var_lbl
  result
}


# ============================================================================
# to_labelled() — Any → haven_labelled
# ============================================================================

#' Convert Variables to Labelled Format
#'
#' @description
#' Converts factors, character vectors, or plain numeric vectors to
#' `haven_labelled` class, optionally assigning value labels and a variable
#' label. This is the reverse of [to_label()].
#'
#' @param data A data frame, tibble, or a single vector.
#' @param ... Optional: unquoted variable names (tidyselect supported). If
#'   empty on a data frame, converts all factor columns.
#' @param labels Optional: a named numeric vector of value labels to assign.
#'   If `NULL` and the input is a factor, factor levels are used as labels.
#' @param label Optional: a variable label string.
#'
#' @return The input with variables converted to `haven_labelled`. For
#'   single vector input, returns a `haven_labelled` vector.
#'
#' @details
#' ## Factor Conversion
#'
#' When converting a factor, the integer codes (1, 2, 3, ...) become the
#' numeric values and the factor levels become the value labels.
#'
#' ## Character Conversion
#'
#' Character vectors are converted to numeric (sequential integers) with
#' the unique character values as labels.
#'
#' @seealso [to_label()] for the reverse operation, [val_labels()] for
#'   setting labels on existing labelled vectors
#'
#' @family labels
#'
#' @examples
#' # Factor → haven_labelled
#' x <- factor(c("Male", "Female", "Male"))
#' to_labelled(x)
#'
#' # Numeric with custom labels
#' to_labelled(c(1, 2, 3),
#'   labels = c("Low" = 1, "Medium" = 2, "High" = 3),
#'   label = "Satisfaction level"
#' )
#'
#' # All factors in a data frame
#' data <- to_labelled(survey_data)
#'
#' @export
to_labelled <- function(data, ..., labels = NULL, label = NULL) {
  .check_haven("{.fn to_labelled}")

  if (!is.data.frame(data)) {
    return(.to_labelled_vec(data, labels, label))
  }

  dots <- rlang::enexprs(...)
  if (length(dots) == 0L) {
    cols <- which(vapply(data, is.factor, logical(1)))
  } else {
    cols <- tidyselect::eval_select(rlang::expr(c(...)), data)
  }

  for (i in cols) {
    data[[i]] <- .to_labelled_vec(data[[i]], labels, label)
  }

  data
}


#' Internal: convert a single vector to haven_labelled
#' @noRd
.to_labelled_vec <- function(x, labels = NULL, label = NULL) {
  # Already labelled? Just update labels if provided
  if (inherits(x, "haven_labelled")) {
    if (!is.null(labels)) attr(x, "labels") <- labels
    if (!is.null(label)) attr(x, "label") <- label
    return(x)
  }

  var_lbl <- label %||% attr(x, "label", exact = TRUE)

  if (is.factor(x)) {
    lvls <- levels(x)
    vals <- as.integer(x)
    if (is.null(labels)) {
      labels <- stats::setNames(seq_along(lvls), lvls)
    }
    result <- haven::labelled(as.double(vals), labels = labels,
                              label = var_lbl)
    return(result)
  }

  if (is.character(x)) {
    unique_vals <- sort(unique(x[!is.na(x)]))
    vals <- match(x, unique_vals)
    if (is.null(labels)) {
      labels <- stats::setNames(seq_along(unique_vals), unique_vals)
    }
    result <- haven::labelled(as.double(vals), labels = labels,
                              label = var_lbl)
    return(result)
  }

  if (is.numeric(x)) {
    result <- haven::labelled(as.double(x), labels = labels,
                              label = var_lbl)
    return(result)
  }

  cli::cli_abort("Cannot convert {.cls {class(x)}} to labelled. Expected factor, character, or numeric.")
}
