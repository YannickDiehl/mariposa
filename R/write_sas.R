# ============================================================================
# write_xpt: SAS transport (.xpt) export with tagged NA support
# ============================================================================

#' Export Data to SAS Transport Format
#'
#' @description
#' Writes a data frame to a SAS transport file (`.xpt`), preserving variable
#' labels and missing value types. Tagged NAs from any source format (SPSS,
#' Stata, SAS) are written as SAS special missing values (`.A` through `.Z`,
#' `._`).
#'
#' @param data A data frame to export.
#' @param path Path to the output file. Must end in `.xpt`.
#' @param version SAS transport file version. Either `5` (default, SAS
#'   Transport v5, most compatible) or `8` (SAS Transport v8, supports
#'   longer variable names).
#' @param name Member name for the dataset within the transport file. If
#'   `NULL`, derived from the file name. Maximum 8 characters for version 5.
#'
#' @return Invisibly returns the file path.
#'
#' @details
#' ## Tagged NA Handling
#'
#' SAS supports 28 special missing values: `.` (system missing), `.A`
#' through `.Z`, and `._`. When exporting data with tagged NAs:
#' \itemize{
#'   \item **From SAS** (`read_sas()`, `read_xpt()`): Native special missing
#'     values are preserved in a full roundtrip.
#'   \item **From SPSS** (`read_spss()`): Tagged NAs are written as SAS
#'     special missing values. The original SPSS numeric codes (e.g., -9, -8)
#'     are not preserved, but the distinct missing value types are.
#'   \item **From Stata** (`read_stata()`): Stata extended missing values
#'     are mapped to their SAS equivalents.
#' }
#'
#' ## Limitations
#'
#' SAS transport files do **not** store value labels. If your data has value
#' labels, consider using [write_spss()] or [write_xlsx()] instead, which
#' preserve full label metadata. Variable labels are preserved.
#'
#' @seealso [read_xpt()] and [read_sas()] for importing SAS files,
#'   [write_spss()] for SPSS export,
#'   [write_stata()] for Stata export,
#'   [write_xlsx()] for Excel export
#'
#' @family data-export
#'
#' @examples
#' \dontrun{
#' # Roundtrip: read SAS transport, process, write back
#' data <- read_xpt("survey.xpt")
#' data_clean <- data[data$age >= 18, ]
#' write_xpt(data_clean, "survey_adults.xpt")
#'
#' # Cross-format: SPSS to SAS transport
#' data <- read_spss("survey.sav")
#' write_xpt(data, "survey.xpt")
#' }
#'
#' @export
write_xpt <- function(data, path, version = 5, name = NULL) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg haven} is required for SAS transport export.",
      "i" = "Install it with: {.code install.packages(\"haven\")}"
    ))
  }

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  if (!grepl("\\.xpt$", path, ignore.case = TRUE)) {
    cli::cli_abort(c(
      "{.arg path} must end in {.val .xpt}.",
      "x" = "Got: {.file {path}}"
    ))
  }

  if (!version %in% c(5L, 8L)) {
    cli::cli_abort("{.arg version} must be 5 or 8.")
  }

  # Auto-derive a short member name if not provided
  # (SAS transport v5 requires name <= 8 characters)
  if (is.null(name)) {
    name <- tools::file_path_sans_ext(basename(path))
    max_len <- if (version == 5L) 8L else 32L
    if (nchar(name) > max_len) {
      name <- substr(name, 1L, max_len)
    }
  }

  # Prepare data: ensure tagged NAs are in haven format, clean up attributes
  export_data <- .prepare_for_xpt(data)

  haven::write_xpt(export_data, path = path, version = version, name = name)

  cli::cli_alert_success(
    "Wrote {ncol(data)} variable{?s} ({nrow(data)} obs.) to {.file {basename(path)}}"
  )

  invisible(path)
}


# ---- Internal Helpers -------------------------------------------------------

#' Prepare data for SAS transport export
#'
#' Ensures tagged NAs are in haven's native format (which haven::write_xpt
#' writes as SAS special missing values). Also converts any
#' haven_labelled_spss columns to tagged NA format first. Warns about
#' value labels being lost (not supported in .xpt format).
#'
#' @param data A data frame potentially containing tagged NA columns.
#' @return The data frame ready for haven::write_xpt().
#' @noRd
.prepare_for_xpt <- function(data) {
  # Convert haven_labelled_spss columns to tagged NA format first
  data <- .ensure_tagged_na_format(data)

  has_spss_codes <- FALSE
  has_value_labels <- FALSE

  for (i in seq_len(ncol(data))) {
    x <- data[[i]]
    tag_map <- attr(x, "na_tag_map", exact = TRUE)
    format <- attr(x, "na_tag_format", exact = TRUE)

    if (!is.null(tag_map) && identical(format, "spss")) {
      has_spss_codes <- TRUE
    }

    # Check for value labels (not supported in .xpt)
    labels <- attr(x, "labels", exact = TRUE)
    if (!is.null(labels) && length(labels) > 0L) {
      has_value_labels <- TRUE
    }

    # SAS XPT only supports uppercase tags (A-Z, _)
    # Convert any lowercase tags from SPSS/Stata to uppercase
    x <- .retag_uppercase(x)

    # Clean up mariposa-specific attributes
    attr(x, "na_tag_map") <- NULL
    attr(x, "na_tag_format") <- NULL
    data[[i]] <- x
  }

  if (has_spss_codes) {
    cli::cli_alert_info(
      "SPSS numeric missing codes are mapped to SAS special missing values (.A, .B, ...)."
    )
  }

  if (has_value_labels) {
    cli::cli_alert_info(
      "SAS transport files do not store value labels. Use {.fn write_spss} or {.fn write_xlsx} to preserve labels."
    )
  }

  data
}


#' Convert lowercase tagged NAs to uppercase for SAS XPT format
#'
#' SAS transport files only accept uppercase tags (A-Z, _). This helper
#' converts any lowercase tags (a-z, from SPSS or Stata data) to uppercase.
#'
#' @param x A numeric vector, possibly with tagged NAs.
#' @return The vector with any lowercase tags converted to uppercase.
#' @noRd
.retag_uppercase <- function(x) {
  if (!is.numeric(x)) return(x)

  na_idx <- which(is.na(x))
  if (length(na_idx) == 0L) return(x)

  needs_retag <- FALSE
  new_tags <- character(length(na_idx))

  for (j in seq_along(na_idx)) {
    tag <- haven::na_tag(x[na_idx[j]])
    if (!is.na(tag) && tag != toupper(tag)) {
      needs_retag <- TRUE
      new_tags[j] <- toupper(tag)
    } else {
      new_tags[j] <- tag %||% ""
    }
  }

  if (!needs_retag) return(x)

  # Rebuild tagged NAs with uppercase tags
  for (j in seq_along(na_idx)) {
    tag <- new_tags[j]
    if (nzchar(tag) && !is.na(tag)) {
      x[na_idx[j]] <- haven::tagged_na(tag)
    }
  }

  # Update value labels: remap any tagged NA labels from lowercase to uppercase

  labels <- attr(x, "labels", exact = TRUE)
  if (!is.null(labels)) {
    new_labels <- labels
    for (k in seq_along(labels)) {
      if (is.na(labels[k])) {
        old_tag <- haven::na_tag(labels[k])
        if (!is.na(old_tag) && old_tag != toupper(old_tag)) {
          new_labels[k] <- haven::tagged_na(toupper(old_tag))
        }
      }
    }
    attr(x, "labels") <- new_labels
  }

  x
}
