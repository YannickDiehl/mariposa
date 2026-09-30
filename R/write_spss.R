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
#' file has the same missing value definitions as the original:
#' [read_spss()] remembers the original definition (e.g. `LOWEST THRU -1`),
#' which is written back as long as it still covers every missing code and
#' no valid value. Otherwise up to 3 codes are written as discrete values;
#' more codes use SPSS's "range plus one discrete value" form (the tightest
#' that contains no valid value), announced in one message. If no such
#' range exists, the export stops with an error instead of declaring valid
#' values missing.
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
  ranged <- character(0)

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

    # SPSS allows at most 3 discrete na_values, or 1 na_range + 1 discrete
    # value. .spss_missing_spec() restores the original definition of an
    # imported variable when it still fits, else builds the tightest spec.
    spec <- .spss_missing_spec(raw, na_codes,
                               attr(x, "spss_missing", exact = TRUE),
                               names(data)[i])
    if (isTRUE(spec$announce)) ranged <- c(ranged, spec$text)

    data[[i]] <- haven::labelled_spss(
      raw,
      labels = labels,
      na_values = spec$na_values,
      na_range = spec$na_range,
      label = attr(x, "label", exact = TRUE)
    )
  }

  if (length(ranged) > 0L) {
    cli::cli_inform(c(
      "i" = "SPSS allows at most 3 discrete missing codes; {length(ranged)} variable{?s} with more {?is/are} written with a missing range:",
      stats::setNames(utils::head(ranged, 5L), rep("*", min(5L, length(ranged)))),
      if (length(ranged) > 5L) c(" " = paste0("... and ", length(ranged) - 5L, " more."))
    ))
  }

  if (length(orphaned) > 0L) {
    cli::cli_warn(c(
      "Tagged missing values without a code map in {.var {orphaned}} are written as system missing.",
      "i" = "The map is lost by arithmetic on an imported variable; {.fn rec} keeps it (e.g. {.code rec(x, rules = \"rev\")})."
    ))
  }

  data
}


#' SPSS missing-value specification for a set of missing codes
#'
#' 1. The original definition of an imported variable (read_spss() keeps it
#'    in the "spss_missing" attribute, e.g. LOWEST THRU -1) when it still
#'    covers every code and catches no valid value: exact round trip.
#' 2. Up to 3 codes: discrete values.
#' 3. More: a range plus one discrete value (the narrower of "all but the
#'    lowest" / "all but the highest"), else one range over all codes -
#'    whichever catches no valid value. Announced once by the caller.
#' 4. Otherwise an error: every range would swallow valid values.
#'
#' @return list(na_values, na_range, announce, text)
#' @noRd
.spss_missing_spec <- function(raw, na_codes, original, var_name) {
  na_codes <- sort(unique(na_codes))
  valid <- unique(raw[!is.na(raw) & !(raw %in% na_codes)])
  in_range <- function(v, rng) length(rng) == 2L & v >= rng[1] & v <= rng[2]
  catches <- function(values, rng) {
    any(valid %in% values) || any(in_range(valid, rng))
  }

  if (!is.null(original) && is.list(original)) {
    o_vals <- original$na_values
    o_rng <- original$na_range
    covered <- na_codes %in% o_vals | in_range(na_codes, o_rng)
    fits <- (length(o_rng) == 2L && length(o_vals) <= 1L) ||
      (length(o_rng) == 0L && length(o_vals) <= 3L)
    if (fits && all(covered) && !catches(o_vals, o_rng)) {
      return(list(na_values = o_vals, na_range = o_rng, announce = FALSE))
    }
  }

  if (length(na_codes) <= 3L) {
    return(list(na_values = na_codes, na_range = NULL, announce = FALSE))
  }

  n <- length(na_codes)
  candidates <- list(
    list(na_values = na_codes[1], na_range = na_codes[c(2, n)]),
    list(na_values = na_codes[n], na_range = na_codes[c(1, n - 1)]),
    list(na_values = NULL, na_range = na_codes[c(1, n)])
  )
  ok <- vapply(candidates, function(cand) {
    !catches(cand$na_values, cand$na_range)
  }, logical(1))
  if (!any(ok)) {
    full <- na_codes[c(1, n)]
    rng_txt <- .fmt_missing_range(full)
    caught <- sort(valid[in_range(valid, full)])
    cli::cli_abort(c(
      "Cannot export missing-value codes of {.var {var_name}} to SPSS.",
      "x" = "SPSS allows at most 3 discrete missing codes (or a range plus one); the {n} codes {.val {na_codes}} need the range {rng_txt}, which contains the valid {cli::qty(length(caught))}value{?s} {.val {caught}}.",
      "i" = "Recode the missing codes to a contiguous block (e.g. with {.fn rec}) or reduce them to 3 codes before exporting."
    ))
  }
  widths <- vapply(candidates, function(cand) diff(cand$na_range), numeric(1))
  widths[!ok] <- Inf
  best <- candidates[[which.min(widths)]]
  text <- paste0(var_name, ": ", .fmt_missing_range(best$na_range),
                 if (!is.null(best$na_values)) {
                   paste0(" and ", format(best$na_values))
                 })
  c(best, list(announce = TRUE, text = text))
}


#' Readable SPSS missing range: "-11 to -8", "LOWEST to -1"
#' @noRd
.fmt_missing_range <- function(rng) {
  lo <- if (is.infinite(rng[1])) "LOWEST" else format(rng[1])
  hi <- if (is.infinite(rng[2])) "HIGHEST" else format(rng[2])
  paste(lo, "to", hi)
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
