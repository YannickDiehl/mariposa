# ============================================================================
# read_xlsx: Excel import with label reconstruction
# ============================================================================

#' Read Excel Data with Label Reconstruction
#'
#' @description
#' Reads an Excel (`.xlsx`) file and returns a tibble. When the file was
#' created by [write_xlsx()], the accompanying "Labels" sheet is automatically
#' detected and used to reconstruct variable labels, value labels, and tagged
#' NA metadata -- giving you the same labelled structure as the original data.
#'
#' For plain Excel files (without a "Labels" sheet), this function works like
#' a convenient wrapper around the `openxlsx2` reader, returning a clean tibble.
#'
#' @param path Path to the `.xlsx` file.
#' @param sheet Sheet to read. `NULL` (default) auto-detects: reads the
#'   "Data" sheet if present, otherwise the first non-metadata sheet.
#'   Can be a sheet name (character) or index (integer).
#' @param labels If `TRUE` (default), detects a "Labels" sheet and
#'   reconstructs `haven_labelled` columns, factor levels, variable labels,
#'   and tagged NA metadata. Set to `FALSE` to read raw data without
#'   label reconstruction.
#' @param verbose If `TRUE`, prints a summary of reconstructed labels
#'   and tagged NA mappings.
#' @param ... Additional arguments (currently unused).
#'
#' @return A tibble. When a mariposa-formatted Labels sheet is detected:
#'   \itemize{
#'     \item Numeric columns with value labels are returned as
#'       `haven_labelled` vectors
#'     \item Factor columns are reconstructed with their original levels
#'     \item Variable labels are attached via `attr(x, "label")`
#'     \item Tagged NAs are restored with `na_tag_map` and `na_tag_format`
#'       attributes
#'   }
#'
#' @details
#' ## Roundtrip Workflow
#'
#' `read_xlsx()` is designed to work seamlessly with [write_xlsx()]:
#'
#' ```
#' write_xlsx(data, "survey.xlsx")   # Exports data + Labels sheet
#' data2 <- read_xlsx("survey.xlsx") # Reconstructs labels from Labels sheet
#' ```
#'
#' The reconstructed data will have the same variable labels, value labels,
#' and tagged NA codes as the original. System NAs (empty cells in Excel)
#' remain as regular `NA`.
#'
#' ## File Format Detection
#'
#' `read_xlsx()` auto-detects the type of mariposa export:
#' \itemize{
#'   \item **data.frame export** ("Data" + "Labels" sheets): Reads the
#'     "Data" sheet and applies labels.
#'   \item **list export** (multiple data sheets + "Labels" with "Sheet"
#'     column): Reads the specified or first data sheet and applies
#'     matching labels.
#'   \item **codebook export** ("Overview" + "Codebook" sheets): Warns
#'     that this is a documentation file and reads it as a plain table.
#'   \item **plain Excel**: No Labels sheet detected; returns raw data.
#' }
#'
#' ## When to Use This
#'
#' Use `read_xlsx()` when you:
#' \itemize{
#'   \item Want to read back data exported with `write_xlsx()`
#'   \item Need a quick way to import any `.xlsx` file as a tibble
#'   \item Want to share labelled survey data via Excel and read it back
#'     with all metadata intact
#' }
#'
#' @seealso [write_xlsx()] for exporting data with labels,
#'   [read_spss()], [read_stata()], [read_sas()] for importing from
#'   statistical software
#'
#' @family data-import
#'
#' @examples
#' \dontrun{
#' # Read back a mariposa-exported file with full label reconstruction
#' data <- read_xlsx("survey_export.xlsx")
#' attr(data$gender, "labels")   # Value labels restored
#' attr(data$gender, "label")    # Variable label restored
#'
#' # Read a plain Excel file
#' data <- read_xlsx("plain_data.xlsx")
#'
#' # Read a specific sheet
#' data <- read_xlsx("multi_sheet.xlsx", sheet = "Sheet2")
#'
#' # Skip label reconstruction
#' raw <- read_xlsx("survey_export.xlsx", labels = FALSE)
#' }
#'
#' @export
read_xlsx <- function(path, sheet = NULL, labels = TRUE,
                      verbose = FALSE, ...) {
  .check_openxlsx2()
  .validate_xlsx_read_path(path)

  wb <- openxlsx2::wb_load(path)
  sheet_names <- wb$sheet_names
  format <- .detect_xlsx_format(sheet_names)

  # Warn for codebook exports
  if (format == "codebook") {
    cli::cli_warn(c(
      "This file appears to be a codebook export (from {.code write_xlsx(codebook(...))}).",
      "i" = "Codebook files are for documentation, not data roundtripping.",
      "i" = "Reading the {.val {sheet %||% 'Codebook'}} sheet as a plain table."
    ))
  }

  # Determine which sheet to read
  data_sheet <- .resolve_data_sheet(sheet, sheet_names, format)

  # Read the data
  data <- openxlsx2::read_xlsx(path, sheet = data_sheet)
  data <- tibble::as_tibble(data)

  # Apply labels if requested and available
  if (isTRUE(labels) && "Labels" %in% sheet_names && format != "codebook") {
    labels_df <- openxlsx2::read_xlsx(path, sheet = "Labels")

    # For multi-sheet (list) exports: filter labels to this sheet
    if ("Sheet" %in% names(labels_df)) {
      sheet_match <- data_sheet
      if (sheet_match %in% labels_df$Sheet) {
        labels_df <- labels_df[labels_df$Sheet == sheet_match, ]
      }
      # Drop the Sheet column
      labels_df <- labels_df[, setdiff(names(labels_df), "Sheet"), drop = FALSE]
    }

    data <- .apply_labels_from_sheet(data, labels_df, verbose)
  }

  data
}


# ============================================================================
# Internal Helpers
# ============================================================================

#' Validate read path
#' @noRd
.validate_xlsx_read_path <- function(path) {
  if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) {
    cli::cli_abort(c(
      "{.arg path} must end in {.val .xlsx}.",
      "x" = "Got: {.file {path}}"
    ))
  }
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "File not found: {.file {path}}"
    ))
  }
}


#' Detect the type of mariposa Excel file
#' @return Character: "data_frame", "list", "codebook", or "plain"
#' @noRd
.detect_xlsx_format <- function(sheet_names) {
  has_labels   <- "Labels" %in% sheet_names
  has_data     <- "Data" %in% sheet_names
  has_overview <- "Overview" %in% sheet_names
  has_codebook <- "Codebook" %in% sheet_names

  # Codebook export: Overview + Codebook, no Data sheet

  if (has_overview && has_codebook && !has_data) return("codebook")

  # Data frame or list export: has Labels sheet
  if (has_labels) {
    # list vs data_frame is detected later from the Sheet column
    return("data_frame")
  }

  "plain"
}


#' Resolve which sheet to read
#' @noRd
.resolve_data_sheet <- function(sheet, sheet_names, format) {
  # Explicit sheet specified

  if (!is.null(sheet)) {
    if (is.numeric(sheet)) {
      if (sheet < 1L || sheet > length(sheet_names)) {
        cli::cli_abort("Sheet index {sheet} is out of range (1-{length(sheet_names)}).")
      }
      return(sheet_names[sheet])
    }
    if (!sheet %in% sheet_names) {
      cli::cli_abort("Sheet {.val {sheet}} not found. Available: {.val {sheet_names}}")
    }
    return(sheet)
  }

  # Auto-detect
  metadata_sheets <- c("Labels", "Overview", "Codebook")

  if ("Data" %in% sheet_names) return("Data")

  if (format == "codebook") return("Codebook")

  # First non-metadata sheet
  data_sheets <- setdiff(sheet_names, metadata_sheets)
  if (length(data_sheets) > 0L) return(data_sheets[1L])

  # Fallback: first sheet
  sheet_names[1L]
}


# ============================================================================
# Label Reconstruction
# ============================================================================

#' Apply labels from a Labels sheet to a data tibble
#' @noRd
.apply_labels_from_sheet <- function(data, labels_df, verbose = FALSE) {
  # Validate Labels sheet structure
  required_cols <- c("Variable", "Variable_Label", "Value", "Value_Label", "Type")
  if (!all(required_cols %in% names(labels_df))) return(data)

  has_column_type <- "Column_Type" %in% names(labels_df)

  # Parse into per-variable metadata
  var_meta <- .parse_labels_sheet(labels_df, has_column_type)

  n_labelled <- 0L
  n_factors  <- 0L
  n_tagged   <- 0L

  for (var_name in names(var_meta)) {
    if (!var_name %in% names(data)) next

    meta <- var_meta[[var_name]]
    col  <- data[[var_name]]

    # Determine reconstruction type
    col_type <- meta$column_type

    if (col_type == "haven_labelled") {
      result <- .reconstruct_haven_labelled(col, meta)
      data[[var_name]] <- result$col
      n_labelled <- n_labelled + 1L
      if (result$has_tagged) n_tagged <- n_tagged + 1L

    } else if (col_type == "factor") {
      data[[var_name]] <- .reconstruct_factor(col, meta)
      n_factors <- n_factors + 1L

    } else {
      # Only variable label
      if (nzchar(meta$variable_label)) {
        attr(data[[var_name]], "label") <- meta$variable_label
      }
    }
  }

  if (verbose) {
    cli::cli_inform(c(
      "i" = "Label reconstruction: {n_labelled} haven_labelled, {n_factors} factor, {n_tagged} with tagged NAs"
    ))
  }

  data
}


#' Parse Labels sheet into per-variable metadata
#' @return Named list: var_name -> list(variable_label, column_type, valid, missing)
#' @noRd
.parse_labels_sheet <- function(labels_df, has_column_type) {
  vars <- unique(labels_df$Variable)
  result <- list()

  for (v in vars) {
    rows <- labels_df[labels_df$Variable == v, ]

    var_label <- rows$Variable_Label[1L]
    if (is.na(var_label)) var_label <- ""

    valid   <- rows[!is.na(rows$Type) & rows$Type == "valid", , drop = FALSE]
    missing <- rows[!is.na(rows$Type) & rows$Type == "missing", , drop = FALSE]

    # Determine column type
    if (has_column_type && "Column_Type" %in% names(rows)) {
      ct <- rows$Column_Type[1L]
      if (is.na(ct)) ct <- ""
      col_type <- ct
    } else {
      # Heuristic fallback for old-format files
      col_type <- .guess_column_type(valid, missing)
    }

    result[[v]] <- list(
      variable_label = var_label,
      column_type    = col_type,
      valid          = valid,
      missing        = missing
    )
  }

  result
}


#' Heuristic to determine column type when Column_Type is missing
#' @noRd
.guess_column_type <- function(valid, missing) {
  # If there are "missing" rows -> must be haven_labelled
  if (nrow(missing) > 0L) return("haven_labelled")

  if (nrow(valid) == 0L) return("")

  # If Value == Value_Label for all rows AND values are non-numeric -> factor
  vals_are_labels <- all(valid$Value == valid$Value_Label)
  vals_numeric    <- suppressWarnings(!any(is.na(as.numeric(valid$Value))))

  if (vals_are_labels && !vals_numeric) return("factor")

  # Default to haven_labelled for numeric values with different labels
  if (vals_numeric) return("haven_labelled")

  # Non-numeric values with different labels: factor
  "factor"
}


#' Reconstruct a haven_labelled column
#' @return List with col (haven_labelled vector) and has_tagged (logical)
#' @noRd
.reconstruct_haven_labelled <- function(col, meta) {

  has_tagged <- FALSE

  # Ensure numeric
  raw <- suppressWarnings(as.double(col))

  # Build valid labels: named numeric vector
  valid_labels <- numeric(0)
  if (nrow(meta$valid) > 0L) {
    vals  <- suppressWarnings(as.numeric(meta$valid$Value))
    lbls  <- meta$valid$Value_Label

    # Drop rows where Value could not be coerced
    ok <- !is.na(vals)
    vals <- vals[ok]
    lbls <- lbls[ok]

    if (length(vals) > 0L) {
      # Replace NA labels with the value as string
      na_lbl <- is.na(lbls)
      if (any(na_lbl)) lbls[na_lbl] <- as.character(vals[na_lbl])

      # Deduplicate: keep first occurrence of each numeric value
      # (older exports or manual edits can produce duplicate values)
      dup_mask <- duplicated(vals)
      vals <- vals[!dup_mask]
      lbls <- lbls[!dup_mask]
      valid_labels <- stats::setNames(vals, lbls)
    }
  }

  # Handle missing values with tagged NAs
  tagged_na_labels <- numeric(0)
  na_tag_map       <- numeric(0)

  if (nrow(meta$missing) > 0L && requireNamespace("haven", quietly = TRUE)) {
    missing_codes <- suppressWarnings(as.numeric(meta$missing$Value))
    missing_lbls  <- meta$missing$Value_Label

    # Drop rows where Value could not be coerced
    ok <- !is.na(missing_codes)
    missing_codes <- missing_codes[ok]
    missing_lbls  <- missing_lbls[ok]

    # Replace NA labels with the value as string
    na_lbl <- is.na(missing_lbls)
    if (any(na_lbl)) missing_lbls[na_lbl] <- as.character(missing_codes[na_lbl])

    # Resolve overlap: a code listed as both valid and missing is treated as missing
    # (transfer label from valid to missing if missing has no label)
    if (length(valid_labels) > 0L && length(missing_codes) > 0L) {
      overlap <- unname(valid_labels) %in% missing_codes
      if (any(overlap)) {
        for (ov_val in unname(valid_labels[overlap])) {
          m_idx <- match(ov_val, missing_codes)
          if (!is.na(m_idx) && (is.na(missing_lbls[m_idx]) || !nzchar(missing_lbls[m_idx]))) {
            v_idx <- match(ov_val, unname(valid_labels))
            if (!is.na(v_idx)) missing_lbls[m_idx] <- names(valid_labels)[v_idx]
          }
        }
        valid_labels <- valid_labels[!overlap]
      }
    }

    # Deduplicate missing codes themselves
    if (length(missing_codes) > 0L) {
      dup_mask <- duplicated(missing_codes)
      missing_codes <- missing_codes[!dup_mask]
      missing_lbls  <- missing_lbls[!dup_mask]
    }

    if (length(missing_codes) > 0L) {
      tag_pool <- c(letters, LETTERS, as.character(0:9))
      tag_chars <- tag_pool[seq_along(missing_codes)]

      # Create tagged NAs
      tagged_nas <- vapply(tag_chars, haven::tagged_na, double(1))

      # Replace matching values in data with tagged NAs
      for (k in seq_along(missing_codes)) {
        mask <- !is.na(raw) & raw == missing_codes[k]
        if (any(mask)) {
          raw[mask] <- tagged_nas[k]
        }
      }

      # Build tagged NA labels
      tagged_na_labels <- stats::setNames(tagged_nas, missing_lbls)
      na_tag_map <- stats::setNames(missing_codes, tag_chars)
      has_tagged <- TRUE
    }
  }

  # Combine all labels
  all_labels <- c(valid_labels, tagged_na_labels)

  # Final safety: deduplicate non-NA values only.
  # Tagged NAs have unique bit patterns (NA(a), NA(b), ...) but R's
  # duplicated() treats all NaN values as identical, so we must exclude them.
  if (length(all_labels) > 0L) {
    is_na_lbl <- is.na(all_labels)
    non_na_lbl <- all_labels[!is_na_lbl]
    if (anyDuplicated(non_na_lbl) > 0L) {
      non_na_lbl <- non_na_lbl[!duplicated(non_na_lbl)]
    }
    all_labels <- c(non_na_lbl, all_labels[is_na_lbl])
  }

  # Variable label
  var_label <- if (nzchar(meta$variable_label)) meta$variable_label else NULL

  # Build haven::labelled vector
  if (length(all_labels) > 0L && requireNamespace("haven", quietly = TRUE)) {
    result <- haven::labelled(raw, labels = all_labels, label = var_label)
  } else {
    result <- raw
    if (!is.null(var_label)) attr(result, "label") <- var_label
  }

  # Attach tagged NA metadata
  if (has_tagged) {
    attr(result, "na_tag_map") <- na_tag_map
    na_fmt <- .detect_na_format(meta$missing$Value)
    attr(result, "na_tag_format") <- na_fmt
  }

  list(col = result, has_tagged = has_tagged)
}


#' Reconstruct a factor column
#' @noRd
.reconstruct_factor <- function(col, meta) {
  if (nrow(meta$valid) > 0L) {
    lvls <- meta$valid$Value
    result <- factor(col, levels = lvls)
  } else {
    result <- factor(col)
  }

  if (nzchar(meta$variable_label)) {
    attr(result, "label") <- meta$variable_label
  }

  result
}


#' Auto-detect na_tag_format from missing value strings
#' @noRd
.detect_na_format <- function(value_strings) {
  value_strings <- as.character(value_strings)
  if (any(grepl("^\\.[a-z]$", value_strings))) return("stata")
  if (any(grepl("^\\.[A-Z_]$", value_strings))) return("sas")
  "spss"
}
