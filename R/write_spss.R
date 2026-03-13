# ============================================================================
# write_spss: SPSS .sav export with tagged NA roundtripping
# ============================================================================

#' Export Data to SPSS Format
#'
#' @description
#' Writes a data frame to an SPSS `.sav` file, preserving variable labels,
#' value labels, and user-defined missing values. When exporting data that
#' was imported with [read_spss()] (with `tag.na = TRUE`), the tagged NAs are
#' automatically converted back to SPSS user-defined missing values, enabling
#' full roundtrip fidelity.
#'
#' @param data A data frame to export. Columns of class `haven_labelled` will
#'   have their labels and missing value metadata written to the `.sav` file.
#' @param path Path to the output file. Must end in `.sav` or `.zsav`.
#' @param compress Compression type. One of `"byte"` (default, byte-level
#'   compression), `"none"` (no compression), or `"zsav"` (zlib compression,
#'   requires SPSS v21+).
#'
#' @return Invisibly returns the file path.
#'
#' @details
#' ## Tagged NA Roundtripping
#'
#' Data imported via [read_spss()] stores SPSS user-defined missing values as
#' tagged NAs with an `na_tag_map` attribute mapping tag characters to original
#' codes (e.g., -9, -8). `write_spss()` reverses this process: tagged NAs are
#' converted back to their original numeric codes, and the SPSS user-defined
#' missing value specification is reconstructed so that the exported `.sav`
#' file has the same missing value definitions as the original.
#'
#' ## Cross-Format Export
#'
#' When exporting data originally imported from Stata or SAS (with native
#' extended missing values like `.a`-`.z` or `.A`-`.Z`), these cannot be
#' represented as SPSS user-defined missing values. In this case, they are
#' written as system missing (regular `NA`) with a warning.
#'
#' ## When to Use This
#'
#' Use `write_spss()` when you:
#' \itemize{
#'   \item Need to export processed survey data back to SPSS format
#'   \item Want to preserve user-defined missing value definitions
#'   \item Need roundtrip fidelity: `read_spss()` -> processing -> `write_spss()`
#' }
#'
#' @seealso [read_spss()] for importing SPSS files,
#'   [write_xlsx()] for Excel export,
#'   [write_stata()] for Stata export,
#'   [write_xpt()] for SAS transport export,
#'   [untag_na()], [strip_tags()]
#'
#' @family data-export
#'
#' @examples
#' \dontrun{
#' # Roundtrip: read SPSS, process, write back
#' data <- read_spss("survey.sav")
#' data_clean <- data[data$age >= 18, ]
#' write_spss(data_clean, "survey_adults.sav")
#'
#' # Export with zlib compression (smaller file, requires SPSS v21+)
#' write_spss(data, "survey_compressed.zsav", compress = "zsav")
#' }
#'
#' @export
write_spss <- function(data, path, compress = c("byte", "none", "zsav")) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg haven} is required for SPSS export.",
      "i" = "Install it with: {.code install.packages(\"haven\")}"
    ))
  }

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  compress <- match.arg(compress)

  if (!grepl("\\.(sav|zsav)$", path, ignore.case = TRUE)) {
    cli::cli_abort(c(
      "{.arg path} must end in {.val .sav} or {.val .zsav}.",
      "x" = "Got: {.file {path}}"
    ))
  }

  # Prepare data: convert tagged NAs back to haven_labelled_spss with na_values
  export_data <- .prepare_for_spss(data)

  haven::write_sav(export_data, path = path, compress = compress)

  cli::cli_alert_success(
    "Wrote {ncol(data)} variable{?s} ({nrow(data)} obs.) to {.file {basename(path)}}"
  )

  invisible(path)
}


# ---- Internal Helpers -------------------------------------------------------

#' Prepare data for SPSS export
#'
#' Converts tagged NA columns back to haven_labelled_spss format with
#' na_values attribute, so that haven::write_sav() writes proper
#' SPSS user-defined missing value specifications.
#'
#' @param data A data frame potentially containing tagged NA columns.
#' @return The data frame with columns converted for SPSS export.
#' @noRd
.prepare_for_spss <- function(data) {
  for (i in seq_len(ncol(data))) {
    x <- data[[i]]
    tag_map <- attr(x, "na_tag_map", exact = TRUE)

    if (is.null(tag_map)) next

    if (!is.numeric(tag_map)) {
      # Native Stata/SAS tags (.a, .A) cannot be represented as SPSS na_values
      format_name <- attr(x, "na_tag_format", exact = TRUE)
      if (is.null(format_name)) format_name <- "unknown"
      cli::cli_warn(c(
        "Variable {.var {names(data)[i]}} has native {toupper(format_name)} missing values.",
        "i" = "These cannot be represented as SPSS user-defined missing values and will be written as system missing."
      ))
      data[[i]] <- strip_tags(x)
      next
    }

    # Numeric codes: reconstruct haven_labelled_spss
    na_codes <- unname(tag_map)

    # Untag NAs back to their original numeric codes
    raw <- untag_na(x)

    # Reconstruct labels: convert tagged NA label entries back to regular entries
    labels <- attr(x, "labels", exact = TRUE)
    if (!is.null(labels)) {
      valid_labels <- labels[!is.na(labels)]
      na_entries   <- labels[is.na(labels)]

      if (length(na_entries) > 0L) {
        na_tags <- vapply(na_entries, haven::na_tag, character(1))
        for (j in seq_along(na_entries)) {
          tag <- na_tags[j]
          if (!is.na(tag) && tag %in% names(tag_map)) {
            new_entry <- tag_map[tag]
            names(new_entry) <- names(na_entries)[j]
            valid_labels <- c(valid_labels, new_entry)
          }
        }
      }

      labels <- valid_labels
    }

    # SPSS allows at most 3 discrete na_values, or 1 na_range (+ 1 discrete).
    # When more than 3 codes exist, use na_range to cover them.
    if (length(na_codes) <= 3L) {
      data[[i]] <- haven::labelled_spss(
        raw,
        labels = labels,
        na_values = na_codes,
        label = attr(x, "label", exact = TRUE)
      )
    } else {
      # Use a range covering all missing codes
      na_rng <- c(min(na_codes), max(na_codes))
      data[[i]] <- haven::labelled_spss(
        raw,
        labels = labels,
        na_range = na_rng,
        label = attr(x, "label", exact = TRUE)
      )
    }
  }

  data
}
