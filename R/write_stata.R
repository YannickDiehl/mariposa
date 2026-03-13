# ============================================================================
# write_stata: Stata .dta export with tagged NA support
# ============================================================================

#' Export Data to Stata Format
#'
#' @description
#' Writes a data frame to a Stata `.dta` file, preserving variable labels,
#' value labels, and missing value types. Tagged NAs from any source format
#' (SPSS, Stata, SAS) are written as Stata extended missing values
#' (`.a` through `.z`).
#'
#' @param data A data frame to export. Columns of class `haven_labelled` will
#'   have their labels written to the `.dta` file.
#' @param path Path to the output file. Must end in `.dta`.
#' @param version Stata file version to use. Supported values: 8-15.
#'   Default is 14 (Stata 14/15, supports Unicode). Use 13 for compatibility
#'   with older Stata versions.
#'
#' @return Invisibly returns the file path.
#'
#' @details
#' ## Tagged NA Handling
#'
#' Stata natively supports 27 extended missing values: `.a` through `.z` and
#' `.` (system missing). When exporting data with tagged NAs:
#' \itemize{
#'   \item **From Stata** (`read_stata()`): Native extended missing values
#'     are preserved in a full roundtrip.
#'   \item **From SPSS** (`read_spss()`): Tagged NAs are written as Stata
#'     extended missing values (`.a`, `.b`, etc.). The original SPSS numeric
#'     codes (e.g., -9, -8) are not preserved in the Stata file, but the
#'     distinct missing value types and their labels are.
#'   \item **From SAS** (`read_sas()`, `read_xpt()`): SAS special missing
#'     values are mapped to their Stata equivalents.
#' }
#'
#' ## Limitations
#'
#' Stata supports at most 27 distinct missing types per variable. SPSS data
#' with more than 27 tagged NA types in a single variable may lose some
#' distinctions (though this is extremely rare in practice).
#'
#' @seealso [read_stata()] for importing Stata files,
#'   [write_spss()] for SPSS export,
#'   [write_xlsx()] for Excel export,
#'   [write_xpt()] for SAS transport export
#'
#' @family data-export
#'
#' @examples
#' \dontrun{
#' # Roundtrip: read Stata, process, write back
#' data <- read_stata("survey.dta")
#' data_clean <- data[data$age >= 18, ]
#' write_stata(data_clean, "survey_adults.dta")
#'
#' # Cross-format: SPSS to Stata
#' data <- read_spss("survey.sav")
#' write_stata(data, "survey.dta")
#'
#' # Stata 13 compatibility
#' write_stata(data, "survey_v13.dta", version = 13)
#' }
#'
#' @export
write_stata <- function(data, path, version = 14) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg haven} is required for Stata export.",
      "i" = "Install it with: {.code install.packages(\"haven\")}"
    ))
  }

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  if (!grepl("\\.dta$", path, ignore.case = TRUE)) {
    cli::cli_abort(c(
      "{.arg path} must end in {.val .dta}.",
      "x" = "Got: {.file {path}}"
    ))
  }

  # Prepare data: ensure tagged NAs are in haven format, clean up attributes
  export_data <- .prepare_for_stata(data)

  haven::write_dta(export_data, path = path, version = version)

  cli::cli_alert_success(
    "Wrote {ncol(data)} variable{?s} ({nrow(data)} obs.) to {.file {basename(path)}}"
  )

  invisible(path)
}


# ---- Internal Helpers -------------------------------------------------------

#' Prepare data for Stata export
#'
#' Ensures tagged NAs are in haven's native format (which haven::write_dta
#' writes as Stata extended missing values). Also converts any
#' haven_labelled_spss columns (with na_values/na_range attributes) to
#' tagged NA format first, so their missing values are preserved.
#'
#' @param data A data frame potentially containing tagged NA columns.
#' @return The data frame ready for haven::write_dta().
#' @noRd
.prepare_for_stata <- function(data) {
  # Convert haven_labelled_spss columns to tagged NA format first
  data <- .ensure_tagged_na_format(data)

  has_spss_codes <- FALSE
  non_int_labelled <- character(0)

  for (i in seq_len(ncol(data))) {
    x <- data[[i]]
    tag_map <- attr(x, "na_tag_map", exact = TRUE)
    format <- attr(x, "na_tag_format", exact = TRUE)

    if (!is.null(tag_map) && identical(format, "spss")) {
      has_spss_codes <- TRUE
    }

    # Stata only supports value labels for integer variables.
    # Strip non-NA value labels from non-integer labelled columns to prevent
    # haven::write_dta() errors; keep tagged NA labels and variable labels.
    labels <- attr(x, "labels", exact = TRUE)
    if (!is.null(labels) && length(labels) > 0L && is.numeric(x)) {
      valid <- as.double(x[!is.na(x)])
      if (length(valid) > 0L && any(valid != trunc(valid))) {
        non_int_labelled <- c(non_int_labelled, names(data)[i])
        # Keep only tagged NA labels (Stata accepts these on non-integer vars)
        na_labels <- labels[is.na(labels)]
        attr(x, "labels") <- if (length(na_labels) > 0L) na_labels else NULL
      }
    }

    # Clean up mariposa-specific attributes (haven doesn't need them)
    attr(x, "na_tag_map") <- NULL
    attr(x, "na_tag_format") <- NULL
    data[[i]] <- x
  }

  if (has_spss_codes) {
    cli::cli_alert_info(
      "SPSS numeric missing codes are mapped to Stata extended missing values (.a, .b, ...)."
    )
  }

  if (length(non_int_labelled) > 0L) {
    cli::cli_alert_info(
      "Stata only supports value labels for integer variables. Value labels dropped for: {.var {non_int_labelled}}."
    )
  }

  data
}
