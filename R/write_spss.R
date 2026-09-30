# ============================================================================
# write_spss: SPSS .sav export with tagged NA roundtripping
# ============================================================================

#' Export Data to SPSS Format
#'
#' @description
#' Writes a data frame to an SPSS `.sav` file, preserving variable labels,
#' value labels, and user-defined missing values. When exporting data that
#' was imported with [read_spss()] (with `tag_na = TRUE`), the tagged NAs are
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
#' ## Derived Variables
#'
#' Arithmetic on an imported variable (e.g. `x + 1` inside `mutate()`)
#' keeps its tagged NAs but loses the code map, so the original codes are
#' unknown. Such values are written as system missing with a warning
#' naming the variables. [rec()] keeps the map; [std()], [center()],
#' [pomps()] and [to_numeric()] return plain `NA` by design. Numeric
#' columns that carry a bare `"labels"` attribute (without the
#' `haven_labelled` class) are exported with their value labels.
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
#' \donttest{
#' if (requireNamespace("haven", quietly = TRUE)) {
#'   # Roundtrip: write to a temporary .sav, read back
#'   tmp <- tempfile(fileext = ".sav")
#'   write_spss(survey_data, tmp)
#'   data <- read_spss(tmp)
#'
#'   # Export with zlib compression (smaller file, requires SPSS v21+)
#'   tmp_z <- tempfile(fileext = ".zsav")
#'   write_spss(survey_data, tmp_z, compress = "zsav")
#'
#'   unlink(c(tmp, tmp_z))
#' }
#' }
#'
#' @export
write_spss <- function(data, path, compress = c("byte", "none", "zsav")) {
  .check_haven("SPSS export")

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
  data <- .restore_factor_codes(data)
  data <- .promote_bare_labels(data)
  orphaned <- character(0)

  for (i in seq_len(ncol(data))) {
    x <- data[[i]]
    tag_map <- attr(x, "na_tag_map", exact = TRUE)

    if (!is.null(tag_map) && !is.numeric(tag_map)) {
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

    # Tagged NAs without a code map (e.g. after arithmetic on an imported
    # variable: vctrs keeps the NaN payload but drops the map) cannot be
    # written by haven ("character tags for missing values"): system missing.
    if (is.double(x)) {
      tags <- .na_tags(x)
      orphan <- !is.na(tags) & !(tags %in% names(tag_map))
      if (any(orphan)) {
        orphaned <- c(orphaned, names(data)[i])
        raw <- .plain_numeric(x)
        raw[orphan] <- NA_real_
        attributes(raw) <- attributes(x)
        x <- raw
        data[[i]] <- x
      }
      labels <- attr(x, "labels", exact = TRUE)
      if (is.null(tag_map) && !is.null(labels) && anyNA(labels)) {
        # labelled missing types whose codes are unknown: drop the entries
        attr(x, "labels") <- labels[!is.na(labels)]
        data[[i]] <- x
      }
    }

    if (is.null(tag_map)) next

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
        hit <- match(.na_tags(na_entries), names(tag_map))
        ok <- !is.na(hit)
        valid_labels <- c(valid_labels, stats::setNames(
          unname(tag_map)[hit[ok]], names(na_entries)[ok]
        ))
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

      # Guard: the min-max range must not swallow valid values. With codes
      # like c(0, 7, 8, 9) the range 0-9 would silently mark every valid
      # value 1-6 as user-missing in the exported file - data corruption,
      # not a formatting detail.
      observed_valid <- unique(raw[!is.na(raw) & !(raw %in% na_codes)])
      caught <- observed_valid[observed_valid >= na_rng[1] &
                                 observed_valid <= na_rng[2]]
      if (length(caught) > 0) {
        cli::cli_abort(c(
          "Cannot export missing-value codes of {.var {names(data)[i]}} to SPSS.",
          "x" = "SPSS allows at most 3 discrete missing codes; the {length(na_codes)} codes ({paste(sort(na_codes), collapse = ', ')}) would be written as range {na_rng[1]}-{na_rng[2]}, which contains the valid value{?s} {paste(sort(caught), collapse = ', ')}.",
          "i" = "Recode the missing codes to a contiguous block (e.g. with {.fn rec}) or reduce them to 3 codes before exporting."
        ))
      }
      cli::cli_warn(c(
        "Variable {.var {names(data)[i]}}: {length(na_codes)} discrete missing codes exceed SPSS's limit of 3.",
        "i" = "Writing them as missing range {na_rng[1]}-{na_rng[2]} instead."
      ))

      data[[i]] <- haven::labelled_spss(
        raw,
        labels = labels,
        na_range = na_rng,
        label = attr(x, "label", exact = TRUE)
      )
    }
  }

  if (length(orphaned) > 0L) {
    cli::cli_warn(c(
      "Tagged missing values without a code map in {.var {orphaned}} are written as system missing.",
      "i" = "The map is lost by arithmetic on an imported variable; {.fn rec} keeps it (e.g. {.code rec(x, rules = \"rev\")})."
    ))
  }

  data
}


#' Write factors made by to_label() with their original codes
#'
#' haven writes a factor as 1..k with the levels as labels. A to_label()
#' factor carries its original codes ("codes" attribute, e.g. 100, 120,
#' 140): converting it back with to_labelled() exports those codes and
#' labels instead of renumbering them. Other factors are left to haven.
#' @noRd
.restore_factor_codes <- function(data) {
  for (i in seq_len(ncol(data))) {
    x <- data[[i]]
    if (is.factor(x) && !is.null(.factor_codes(x))) {
      data[[i]] <- .to_labelled_vec(x)
    }
  }
  data
}


#' Give numeric columns with a bare "labels" attribute the haven_labelled
#' class
#'
#' haven's writers only store value labels of haven_labelled vectors; a
#' plain numeric vector with a "labels" attribute (sjlabelled, older
#' mariposa versions) silently lost them on export.
#' @noRd
.promote_bare_labels <- function(data) {
  for (i in seq_len(ncol(data))) {
    x <- data[[i]]
    labels <- attr(x, "labels", exact = TRUE)
    if (is.numeric(x) && !inherits(x, "haven_labelled") &&
        !is.null(labels) && is.numeric(labels) && !is.null(names(labels))) {
      data[[i]] <- .with_label_meta(x, x, labels = labels,
                                    label = attr(x, "label", exact = TRUE),
                                    labelled = TRUE)
    }
  }
  data
}
