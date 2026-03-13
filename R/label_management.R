# ============================================================================
# Label Management Functions
# ============================================================================
# Functions for getting, setting, and manipulating variable labels and value
# labels on labelled survey data. Integrates with haven's labelling system
# and mariposa's tagged NA infrastructure.


# ============================================================================
# var_label() — Get/Set/Remove Variable Labels
# ============================================================================

#' Get or Set Variable Labels
#'
#' @description
#' Retrieves or assigns variable labels (the `"label"` attribute) on vectors
#' or data frame columns. Variable labels describe what a variable measures
#' (e.g., "Age of respondent") and are commonly used in survey data imported
#' from SPSS, Stata, or SAS.
#'
#' The function operates in two modes:
#' \itemize{
#'   \item **GET mode**: Called with bare variable names or no `...` arguments,
#'     returns existing labels.
#'   \item **SET mode**: Called with `name = "label"` pairs, assigns labels to
#'     variables and returns the modified data.
#' }
#'
#' @param data A data frame, tibble, or a single vector.
#' @param ... In GET mode: unquoted variable names (tidyselect supported).
#'   If empty, returns labels for all variables.
#'   In SET mode: named pairs where the name is a variable and the value is
#'   the label string. Use `NULL` to remove a label.
#'
#' @return
#' \itemize{
#'   \item **GET mode (vector input)**: A single character string (the label),
#'     or `NULL` if no label exists.
#'   \item **GET mode (data frame input)**: A named character vector of labels.
#'     Variables without labels return `NA`.
#'   \item **SET mode**: The modified data frame (invisibly), with labels
#'     assigned.
#' }
#'
#' @details
#' Variable labels are stored as the `"label"` attribute on each column,
#' which is the standard used by the `haven` package for SPSS/Stata/SAS
#' imports. These labels are preserved by mariposa's [codebook()],
#' [frequency()], and [describe()] functions.
#'
#' ## When to Use This
#'
#' Use `var_label()` when you:
#' \itemize{
#'   \item Want to inspect what variables measure in imported survey data
#'   \item Need to add labels to manually created data before using
#'     [codebook()] or [write_spss()]
#'   \item Want to update or correct labels after import
#' }
#'
#' @seealso [val_labels()] for value labels, [codebook()] for viewing all
#'   metadata, [copy_labels()] for preserving labels after dplyr operations
#'
#' @family labels
#'
#' @examples
#' # GET: retrieve all variable labels
#' var_label(survey_data)
#'
#' # GET: specific variables
#' var_label(survey_data, age, gender)
#'
#' # GET: from a single vector
#' var_label(survey_data$age)
#'
#' # SET: assign labels
#' data <- var_label(survey_data,
#'   age = "Age of respondent",
#'   gender = "Gender identity"
#' )
#'
#' # REMOVE: set to NULL
#' data <- var_label(data, age = NULL)
#'
#' @export
var_label <- function(data, ...) {
  # --- Single vector input ---
  if (!is.data.frame(data)) {
    dots <- rlang::enexprs(...)
    if (length(dots) > 0L) {
      cli::cli_abort("{.fn var_label} does not accept {.arg ...} when {.arg data} is a vector. Use a data frame for SET mode.")
    }
    return(attr(data, "label", exact = TRUE))
  }

  dots <- rlang::enexprs(...)

  # No arguments → GET all
  if (length(dots) == 0L) {
    return(.get_var_labels(data, seq_len(ncol(data))))
  }

  # Detect mode: all named → SET; all unnamed → GET
  nms <- names(dots)
  has_names <- !is.null(nms) && all(nzchar(nms))
  no_names  <- is.null(nms) || all(!nzchar(nms))

  if (no_names) {
    # GET mode with tidyselect
    pos <- tidyselect::eval_select(rlang::expr(c(...)), data)
    return(.get_var_labels(data, pos))
  }

  if (!has_names) {
    cli::cli_abort("Cannot mix named and unnamed arguments in {.fn var_label}. Use all named pairs for SET mode or all bare names for GET mode.")
  }

  # SET mode
  for (nm in nms) {
    if (!nm %in% names(data)) {
      cli::cli_abort("Variable {.var {nm}} not found in data.")
    }
    val <- eval(dots[[nm]], envir = parent.frame())
    if (is.null(val)) {
      attr(data[[nm]], "label") <- NULL
    } else {
      if (!is.character(val) || length(val) != 1L) {
        cli::cli_abort("Label for {.var {nm}} must be a single character string.")
      }
      attr(data[[nm]], "label") <- val
    }
  }

  invisible(data)
}


#' Internal: extract variable labels from selected columns
#' @param data A data frame.
#' @param positions Integer vector of column positions (or named).
#' @return Named character vector.
#' @noRd
.get_var_labels <- function(data, positions) {
  col_names <- names(data)[positions]
  labels <- vapply(col_names, function(nm) {
    lbl <- attr(data[[nm]], "label", exact = TRUE)
    if (is.null(lbl)) NA_character_ else as.character(lbl)
  }, character(1), USE.NAMES = FALSE)
  stats::setNames(labels, col_names)
}


# ============================================================================
# val_labels() — Get/Set/Add/Remove Value Labels
# ============================================================================

#' Get or Set Value Labels
#'
#' @description
#' Retrieves or assigns value labels (the `"labels"` attribute) on labelled
#' vectors or data frame columns. Value labels map numeric codes to
#' descriptive text (e.g., `1 = "Male"`, `2 = "Female"`).
#'
#' The function operates in two modes:
#' \itemize{
#'   \item **GET mode**: Called with bare variable names, returns existing
#'     value labels.
#'   \item **SET mode**: Called with `name = c(...)` pairs, assigns value
#'     labels to variables.
#' }
#'
#' @param data A data frame, tibble, or a single vector.
#' @param ... In GET mode: unquoted variable names (tidyselect supported).
#'   In SET mode: named pairs where the name is a variable and the value is
#'   a named vector of labels (e.g., `c("Male" = 1, "Female" = 2)`).
#'   Use `NULL` to remove all value labels from a variable.
#' @param .add If `TRUE`, adds labels to any existing ones instead of
#'   replacing them. Default: `FALSE`.
#' @param drop.na If `TRUE` (default), tagged NA labels are excluded from
#'   GET results.
#'
#' @return
#' \itemize{
#'   \item **GET mode (vector input)**: A named numeric vector of value
#'     labels, or `NULL`.
#'   \item **GET mode (data frame, single variable)**: A named numeric
#'     vector of value labels.
#'   \item **GET mode (data frame, multiple variables)**: A named list of
#'     label vectors.
#'   \item **SET mode**: The modified data frame (invisibly).
#' }
#'
#' @details
#' Value labels are stored as the `"labels"` attribute in haven's format:
#' a named numeric vector where names are the label text and values are
#' the numeric codes. This is the standard used by [read_spss()],
#' [read_stata()], and [read_sas()].
#'
#' ## Adding vs. Replacing Labels
#'
#' By default, SET mode replaces all existing value labels. Use `.add = TRUE`
#' to keep existing labels and only add new ones. If a value already has a
#' label, the new label overwrites it.
#'
#' @seealso [var_label()] for variable labels, [to_label()] for converting
#'   labelled vectors to factors, [drop_labels()] for removing unused labels
#'
#' @family labels
#'
#' @examples
#' # GET: retrieve value labels from a vector
#' val_labels(survey_data$gender)
#'
#' # GET: from specific variables (returns list)
#' val_labels(survey_data, gender, region)
#'
#' # SET: assign value labels
#' data <- val_labels(survey_data,
#'   gender = c("Male" = 1, "Female" = 2, "Non-binary" = 3)
#' )
#'
#' # ADD: add labels without removing existing ones
#' data <- val_labels(data,
#'   gender = c("Prefer not to say" = 4),
#'   .add = TRUE
#' )
#'
#' # REMOVE: set to NULL
#' data <- val_labels(data, gender = NULL)
#'
#' @export
val_labels <- function(data, ..., .add = FALSE, drop.na = TRUE) {
  # --- Single vector input ---
  if (!is.data.frame(data)) {
    dots <- rlang::enexprs(...)
    if (length(dots) > 0L) {
      cli::cli_abort("{.fn val_labels} does not accept {.arg ...} when {.arg data} is a vector. Use a data frame for SET mode.")
    }
    labels <- attr(data, "labels", exact = TRUE)
    if (!is.null(labels) && isTRUE(drop.na)) {
      labels <- labels[!is.na(labels)]
    }
    return(labels)
  }

  dots <- rlang::enexprs(...)

  # No arguments → GET all labelled variables
  if (length(dots) == 0L) {
    return(.get_val_labels_all(data, drop.na))
  }

  # Detect mode
  nms <- names(dots)
  has_names <- !is.null(nms) && all(nzchar(nms))
  no_names  <- is.null(nms) || all(!nzchar(nms))

  if (no_names) {
    # GET mode with tidyselect
    pos <- tidyselect::eval_select(rlang::expr(c(...)), data)
    col_names <- names(data)[pos]
    result <- lapply(col_names, function(nm) {
      labels <- attr(data[[nm]], "labels", exact = TRUE)
      if (!is.null(labels) && isTRUE(drop.na)) {
        labels <- labels[!is.na(labels)]
      }
      labels
    })
    names(result) <- col_names
    # Simplify: if single variable, return vector directly
    if (length(result) == 1L) return(result[[1L]])
    return(result)
  }

  if (!has_names) {
    cli::cli_abort("Cannot mix named and unnamed arguments in {.fn val_labels}.")
  }

  # SET mode
  for (nm in nms) {
    if (!nm %in% names(data)) {
      cli::cli_abort("Variable {.var {nm}} not found in data.")
    }
    new_labels <- eval(dots[[nm]], envir = parent.frame())

    if (is.null(new_labels)) {
      # Remove all value labels
      attr(data[[nm]], "labels") <- NULL
      next
    }

    if (!is.numeric(new_labels) || is.null(names(new_labels))) {
      cli::cli_abort("Labels for {.var {nm}} must be a named numeric vector (e.g., {.code c(\"Male\" = 1, \"Female\" = 2)}).")
    }

    if (isTRUE(.add)) {
      existing <- attr(data[[nm]], "labels", exact = TRUE)
      if (!is.null(existing)) {
        # Merge: new labels overwrite existing for same values
        existing <- existing[!unname(existing) %in% unname(new_labels)]
        new_labels <- c(existing, new_labels)
      }
    }

    attr(data[[nm]], "labels") <- new_labels

    # Ensure haven_labelled class if not already
    if (!inherits(data[[nm]], "haven_labelled") && is.numeric(data[[nm]])) {
      if (requireNamespace("haven", quietly = TRUE)) {
        data[[nm]] <- haven::labelled(
          as.double(data[[nm]]),
          labels = new_labels,
          label = attr(data[[nm]], "label", exact = TRUE)
        )
      }
    }
  }

  invisible(data)
}


#' Internal: get value labels from all labelled columns
#' @noRd
.get_val_labels_all <- function(data, drop.na = TRUE) {
  has_labels <- vapply(data, function(x) {
    !is.null(attr(x, "labels", exact = TRUE)) || is.factor(x)
  }, logical(1))

  if (!any(has_labels)) return(list())

  col_names <- names(data)[has_labels]
  result <- lapply(col_names, function(nm) {
    x <- data[[nm]]
    if (is.factor(x)) {
      # Return factor levels as pseudo-labels
      lvls <- levels(x)
      stats::setNames(seq_along(lvls), lvls)
    } else {
      labels <- attr(x, "labels", exact = TRUE)
      if (!is.null(labels) && isTRUE(drop.na)) {
        labels <- labels[!is.na(labels)]
      }
      labels
    }
  })
  names(result) <- col_names
  result
}


# ============================================================================
# copy_labels() — Preserve Labels After dplyr Operations
# ============================================================================

#' Copy Labels from One Data Frame to Another
#'
#' @description
#' Copies variable labels, value labels, and tagged NA metadata from a source
#' data frame to a target data frame. This is essential after dplyr operations
#' like [dplyr::filter()], [dplyr::select()], or [dplyr::mutate()] which can
#' strip label attributes.
#'
#' @param data The target data frame (e.g., after filtering or subsetting).
#' @param source The source data frame with the original labels.
#'
#' @return The target data frame with labels copied from the source. Only
#'   columns present in both data frames are affected. Columns only in
#'   the target are left unchanged.
#'
#' @details
#' The following attributes are copied for each shared column:
#' \itemize{
#'   \item `"label"` — variable label
#'   \item `"labels"` — value labels
#'   \item `"na_tag_map"` — tagged NA mapping
#'   \item `"na_tag_format"` — tagged NA format (spss/stata/sas)
#'   \item `"class"` — vector class (e.g., `haven_labelled`)
#' }
#'
#' @seealso [var_label()], [val_labels()]
#'
#' @family labels
#'
#' @examples
#' # Labels are lost after dplyr operations
#' data_subset <- dplyr::filter(survey_data, age >= 18)
#'
#' # Restore them
#' data_subset <- copy_labels(data_subset, survey_data)
#'
#' @export
copy_labels <- function(data, source) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  if (!is.data.frame(source)) {
    cli::cli_abort("{.arg source} must be a data frame.")
  }

  shared_cols <- intersect(names(data), names(source))

  for (nm in shared_cols) {
    src <- source[[nm]]
    tgt <- data[[nm]]

    # Copy variable label
    lbl <- attr(src, "label", exact = TRUE)
    if (!is.null(lbl)) attr(tgt, "label") <- lbl

    # Copy value labels
    val_lbl <- attr(src, "labels", exact = TRUE)
    if (!is.null(val_lbl)) attr(tgt, "labels") <- val_lbl

    # Copy tagged NA metadata
    tag_map <- attr(src, "na_tag_map", exact = TRUE)
    if (!is.null(tag_map)) attr(tgt, "na_tag_map") <- tag_map

    tag_fmt <- attr(src, "na_tag_format", exact = TRUE)
    if (!is.null(tag_fmt)) attr(tgt, "na_tag_format") <- tag_fmt

    # Restore class if needed (e.g., haven_labelled)
    src_class <- class(src)
    if (!identical(class(tgt), src_class) &&
        any(grepl("haven_labelled", src_class))) {
      class(tgt) <- src_class
    }

    data[[nm]] <- tgt
  }

  data
}


# ============================================================================
# drop_labels() — Remove Unused Value Labels
# ============================================================================

#' Remove Unused Value Labels
#'
#' @description
#' Removes value labels for values that are not present in the data. This is
#' useful after filtering or subsetting, when some categories may no longer
#' exist but their labels remain attached.
#'
#' @param data A data frame or a single vector.
#' @param ... Optional: unquoted variable names (tidyselect supported). If
#'   empty, applies to all labelled columns.
#' @param drop.na If `TRUE`, also removes tagged NA labels. Default: `FALSE`
#'   (tagged NA labels are preserved even if no tagged NAs of that type exist).
#'
#' @return The data with unused labels removed.
#'
#' @details
#' This is useful after subsetting data (e.g., filtering out a category).
#' The removed category's label still exists on the variable, which can
#' cause confusing output in [frequency()] or [codebook()].
#' `drop_labels()` cleans this up by keeping only labels for values
#' actually present in the data.
#'
#' By default, tagged NA labels are preserved (`drop.na = FALSE`) because
#' they represent missing value types, not substantive categories.
#'
#' @seealso [val_labels()], [copy_labels()]
#'
#' @family labels
#'
#' @examples
#' # After filtering, region == 4 no longer exists but its label remains
#' data_subset <- dplyr::filter(survey_data, region != 4)
#' data_clean <- drop_labels(data_subset)
#'
#' @export
drop_labels <- function(data, ..., drop.na = FALSE) {
  if (!is.data.frame(data)) {
    # Single vector
    return(.drop_labels_vec(data, drop.na))
  }

  dots <- rlang::enexprs(...)
  if (length(dots) == 0L) {
    cols <- seq_len(ncol(data))
  } else {
    cols <- tidyselect::eval_select(rlang::expr(c(...)), data)
  }

  for (i in cols) {
    data[[i]] <- .drop_labels_vec(data[[i]], drop.na)
  }

  data
}


#' Internal: drop unused labels from a single vector
#' @noRd
.drop_labels_vec <- function(x, drop.na = FALSE) {
  labels <- attr(x, "labels", exact = TRUE)
  if (is.null(labels) || length(labels) == 0L) return(x)

  # Separate NA labels from valid labels
  na_mask <- is.na(labels)
  valid_labels <- labels[!na_mask]
  na_labels <- labels[na_mask]

  # Keep only labels whose values exist in the data
  if (length(valid_labels) > 0L) {
    present <- unname(valid_labels) %in% unique(as.double(x[!is.na(x)]))
    valid_labels <- valid_labels[present]
  }

  # Optionally drop NA labels too
  if (isTRUE(drop.na) && length(na_labels) > 0L) {
    # Keep only tagged NA labels whose tags exist in the data
    if (requireNamespace("haven", quietly = TRUE)) {
      data_tags <- unique(vapply(
        x[is.na(x)], haven::na_tag, character(1)
      ))
      label_tags <- vapply(na_labels, haven::na_tag, character(1))
      na_labels <- na_labels[label_tags %in% data_tags]
    }
  }

  attr(x, "labels") <- c(valid_labels, na_labels)
  x
}
