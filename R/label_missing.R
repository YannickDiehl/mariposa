# ============================================================================
# Missing Value and Label Stripping Functions
# ============================================================================
# Functions for declaring missing values post-import and removing all
# label metadata from data.


# ============================================================================
# set_na() — Declare Values as Missing Post-Import
# ============================================================================

#' Declare Values as Missing
#'
#' @description
#' Replaces specific numeric values with `NA` (or tagged NAs) across one or
#' more variables. This is essential for data cleaning workflows where missing
#' value codes (e.g., -9, -8, 99) are stored as regular values and need to be
#' declared as missing after import.
#'
#' @param data A data frame, tibble, or a single vector.
#' @param ... Values to set as missing. Can be:
#'   \itemize{
#'     \item **Unnamed numeric values**: Applied to all numeric columns
#'       (e.g., `set_na(data, -9, -8)`)
#'     \item **Named pairs**: Applied to specific variables
#'       (e.g., `set_na(data, income = c(-9, -8), age = -1)`)
#'   }
#' @param tag If `TRUE` (default), uses tagged NAs to preserve distinct
#'   missing types. The resulting tagged NAs integrate with
#'   [na_frequencies()], [frequency()], and [codebook()].
#'   If `FALSE`, replaces with regular `NA`.
#' @param verbose If `TRUE`, prints a summary of conversions.
#'
#' @return The modified data (invisibly for data frames, visibly for vectors).
#'
#' @details
#' ## Tagged vs. Regular NA
#'
#' When `tag = TRUE` (default), each missing value code gets a unique tag
#' character, so you can distinguish between "No answer" (-9) and "Not
#' applicable" (-8) in downstream analysis. This is the same system used by
#' [read_spss()] with `tag.na = TRUE`.
#'
#' When `tag = FALSE`, all specified values become regular `NA` and the
#' distinction between different missing types is lost.
#'
#' ## Interaction with Existing Labels
#'
#' If a value being set to missing has an existing value label, that label
#' is preserved as a tagged NA label (when `tag = TRUE`), making it visible
#' in [frequency()] and [codebook()] output.
#'
#' @seealso [na_frequencies()] for inspecting missing types,
#'   [strip_tags()] for converting tagged NAs to regular NA,
#'   [untag_na()] for recovering original codes
#'
#' @family labels
#'
#' @examples
#' \dontrun{
#' # Set -9 and -8 as missing across all numeric variables
#' data <- set_na(survey_data, -9, -8)
#'
#' # Set missing for specific variables only
#' data <- set_na(survey_data,
#'   income = c(-9, -8, -42),
#'   life_satisfaction = c(-9, -11)
#' )
#'
#' # Use regular NA instead of tagged NA
#' data <- set_na(survey_data, -9, -8, tag = FALSE)
#'
#' # Check the result
#' na_frequencies(data$income)
#' }
#'
#' @export
set_na <- function(data, ..., tag = TRUE, verbose = FALSE) {
  dots <- list(...)

  # Remove named arguments that are parameters, not data
  # (tag and verbose are already captured by name)

  if (!is.data.frame(data)) {
    # Single vector mode
    if (length(dots) == 0L) {
      cli::cli_abort("No values specified. Provide numeric values to set as missing.")
    }
    na_values <- unlist(dots, use.names = FALSE)
    if (!is.numeric(na_values)) {
      cli::cli_abort("Missing values must be numeric.")
    }
    return(.set_na_vec(data, na_values, tag, verbose))
  }

  # Data frame mode
  nms <- names(dots)
  has_names <- !is.null(nms) && any(nzchar(nms))
  no_names  <- is.null(nms) || all(!nzchar(nms))

  if (no_names) {
    # Unnamed values → apply to all numeric columns
    na_values <- unlist(dots, use.names = FALSE)
    if (length(na_values) == 0L) {
      cli::cli_abort("No values specified. Provide numeric values to set as missing.")
    }
    if (!is.numeric(na_values)) {
      cli::cli_abort("Missing values must be numeric.")
    }

    if (isTRUE(tag)) {
      data <- .tag_user_missing_values(data, na_values, "spss", verbose)
    } else {
      n_converted <- 0L
      for (i in seq_len(ncol(data))) {
        if (!is.numeric(data[[i]])) next
        raw <- as.double(data[[i]])
        for (val in na_values) {
          mask <- !is.na(raw) & raw == val
          if (any(mask)) {
            raw[mask] <- NA_real_
            n_converted <- n_converted + sum(mask)
          }
        }
        data[[i]] <- raw
      }
      if (verbose && n_converted > 0L) {
        cli::cli_inform("Set {n_converted} value{?s} to NA.")
      }
    }
  } else {
    # Named pairs → per-variable
    for (nm in nms[nzchar(nms)]) {
      if (!nm %in% names(data)) {
        cli::cli_abort("Variable {.var {nm}} not found in data.")
      }
      na_values <- dots[[nm]]
      if (!is.numeric(na_values)) {
        cli::cli_abort("Missing values for {.var {nm}} must be numeric.")
      }
      data[[nm]] <- .set_na_vec(data[[nm]], na_values, tag, verbose)
    }
  }

  invisible(data)
}


#' Internal: set values to NA in a single vector
#' @noRd
.set_na_vec <- function(x, na_values, tag = TRUE, verbose = FALSE) {
  if (!is.numeric(x)) return(x)

  na_values <- sort(unique(na_values))
  if (length(na_values) == 0L) return(x)

  if (!isTRUE(tag)) {
    # Simple replacement with regular NA
    raw <- as.double(x)
    n_replaced <- 0L
    for (val in na_values) {
      mask <- !is.na(raw) & raw == val
      n_replaced <- n_replaced + sum(mask)
      raw[mask] <- NA_real_
    }
    # Preserve labels (remove labels for the now-missing values)
    labels <- attr(x, "labels", exact = TRUE)
    if (!is.null(labels)) {
      labels <- labels[!is.na(labels) & !unname(labels) %in% na_values]
      attr(raw, "labels") <- if (length(labels) > 0L) labels else NULL
    }
    attr(raw, "label") <- attr(x, "label", exact = TRUE)
    if (verbose && n_replaced > 0L) {
      cli::cli_inform("Set {n_replaced} value{?s} to NA.")
    }
    return(raw)
  }

  # Tagged NA replacement
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg haven} is required for tagged NAs.")
  }

  tag_pool <- c(letters, LETTERS, as.character(0:9))

  # Check if variable already has tagged NAs — extend existing map

  existing_map <- attr(x, "na_tag_map", exact = TRUE)
  if (!is.null(existing_map) && is.numeric(existing_map)) {
    # Skip values already tagged
    already_tagged <- unname(existing_map)
    new_values <- setdiff(na_values, already_tagged)
    if (length(new_values) == 0L) return(x)

    # Determine next available tag characters
    used_tags <- names(existing_map)
    available_tags <- setdiff(tag_pool, used_tags)
    if (length(new_values) > length(available_tags)) {
      cli::cli_warn("Only {length(available_tags)} tag slots remaining. Some values will not be tagged.")
      new_values <- new_values[seq_len(length(available_tags))]
    }
    new_tags <- available_tags[seq_along(new_values)]
    na_values <- new_values
    tag_chars <- new_tags
  } else {
    if (length(na_values) > length(tag_pool)) {
      cli::cli_warn("Only {length(tag_pool)} tag slots available. Some values will not be tagged.")
      na_values <- na_values[seq_len(length(tag_pool))]
    }
    tag_chars <- tag_pool[seq_along(na_values)]
    existing_map <- NULL
  }

  tagged_nas <- haven::tagged_na(tag_chars)
  raw <- as.double(x)
  labels <- attr(x, "labels", exact = TRUE)
  n_replaced <- 0L

  for (j in seq_along(na_values)) {
    mask <- !is.na(raw) & raw == na_values[j]
    n_matches <- sum(mask)
    if (n_matches > 0L) {
      raw[mask] <- tagged_nas[j]
      n_replaced <- n_replaced + n_matches
    }
  }

  # Update value labels
  if (!is.null(labels)) {
    valid_labels <- labels[!is.na(labels)]
    na_label_entries <- labels[is.na(labels)]

    # Move labels for missing values to tagged NA entries
    for (j in seq_along(na_values)) {
      match_idx <- match(na_values[j], unname(valid_labels))
      if (!is.na(match_idx)) {
        tna <- tagged_nas[j]
        names(tna) <- names(valid_labels)[match_idx]
        na_label_entries <- c(na_label_entries, tna)
        valid_labels <- valid_labels[-match_idx]
      }
    }

    labels <- c(valid_labels, na_label_entries)
  }

  new_x <- haven::labelled(
    raw,
    labels = labels,
    label = attr(x, "label", exact = TRUE)
  )

  # Build combined na_tag_map
  new_map <- stats::setNames(na_values, tag_chars)
  if (!is.null(existing_map)) {
    new_map <- c(existing_map, new_map)
  }

  attr(new_x, "na_tag_map") <- new_map
  attr(new_x, "na_tag_format") <- attr(x, "na_tag_format") %||% "spss"

  if (verbose && n_replaced > 0L) {
    cli::cli_inform("Converted {n_replaced} value{?s} to tagged NAs.")
  }

  new_x
}


# ============================================================================
# unlabel() — Strip All Label Metadata
# ============================================================================

#' Remove All Label Metadata
#'
#' @description
#' Strips all label-related attributes from variables, converting
#' `haven_labelled` vectors to their base R types (numeric or character).
#' This is useful when you need plain data without any labelling for
#' functions or packages that do not support labelled data.
#'
#' @param data A data frame, tibble, or a single vector.
#' @param ... Optional: unquoted variable names (tidyselect supported). If
#'   empty, applies to all columns.
#'
#' @return The data with all labelling removed. `haven_labelled` numeric
#'   vectors become plain `double`, factors remain factors (but lose their
#'   `"label"` attribute).
#'
#' @details
#' The following attributes are removed:
#' \itemize{
#'   \item `"label"` (variable label)
#'   \item `"labels"` (value labels)
#'   \item `"na_tag_map"` (tagged NA mapping)
#'   \item `"na_tag_format"` (tagged NA format)
#'   \item `"na_values"`, `"na_range"` (SPSS missing value specs)
#'   \item `"format.spss"`, `"format.stata"`, `"format.sas"` (format info)
#' }
#'
#' Tagged NAs are converted to regular `NA`.
#'
#' @seealso [strip_tags()] for removing only tagged NAs,
#'   [to_label()] for converting to factors (preserving label info)
#'
#' @family labels
#'
#' @examples
#' \dontrun{
#' # Remove all labels from entire dataset
#' data <- read_spss("survey.sav")
#' data_plain <- unlabel(data)
#'
#' # Remove labels from specific variables
#' data <- unlabel(data, gender, life_satisfaction)
#' }
#'
#' @export
unlabel <- function(data, ...) {
  if (!is.data.frame(data)) {
    return(.unlabel_vec(data))
  }

  dots <- rlang::enexprs(...)
  if (length(dots) == 0L) {
    cols <- seq_len(ncol(data))
  } else {
    cols <- tidyselect::eval_select(rlang::expr(c(...)), data)
  }

  for (i in cols) {
    data[[i]] <- .unlabel_vec(data[[i]])
  }

  data
}


#' Internal: remove all label attributes from a single vector
#' @noRd
.unlabel_vec <- function(x) {
  # Convert tagged NAs to regular NA first
  if (is.numeric(x) && any(is.na(x)) &&
      requireNamespace("haven", quietly = TRUE)) {
    # Check for tagged NAs
    has_tagged <- any(vapply(
      x[is.na(x)],
      function(v) !is.na(haven::na_tag(v)),
      logical(1)
    ))
    if (has_tagged) {
      x <- strip_tags(x)
    }
  }

  # Remove all label-related attributes
  attrs_to_remove <- c(
    "label", "labels",
    "na_tag_map", "na_tag_format",
    "na_values", "na_range",
    "format.spss", "format.stata", "format.sas",
    "display_width"
  )

  for (a in attrs_to_remove) {
    attr(x, a) <- NULL
  }

  # Strip haven_labelled class, convert to base type
  if (inherits(x, "haven_labelled")) {
    x <- as.double(x)
  }

  x
}
