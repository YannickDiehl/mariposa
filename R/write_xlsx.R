# ============================================================================
# write_xlsx: Excel export with label support
# ============================================================================

#' Export Data to Excel with Label Support
#'
#' @description
#' Writes data to an Excel (`.xlsx`) file with support for variable labels,
#' value labels, and tagged NA metadata. When exporting labelled survey data,
#' a "Labels" reference sheet is automatically included so that reviewers
#' can look up what each numeric code means -- even without R access.
#'
#' Three input types are supported:
#' \enumerate{
#'   \item **Data frame**: Exports the data plus a "Labels" reference sheet
#'     with variable labels, value labels, and missing value codes.
#'   \item **Codebook object**: Exports the codebook as it appears in the
#'     HTML viewer -- with values, value labels, and frequencies stacked
#'     inside each cell, tagged NAs separated by a line, and an overview
#'     sheet with dataset metadata.
#'   \item **Named list**: Each list element becomes a separate sheet.
#'     Elements can be data frames, [frequency()] results, or [codebook()]
#'     results -- types can be mixed freely. For data frame elements, a
#'     combined "Labels" sheet is appended.
#' }
#'
#' @param x Object to export: a data frame, a [codebook()] result, or a
#'   named list of data frames.
#' @param file Path to the output `.xlsx` file. Must end in `.xlsx`.
#' @param overwrite Overwrite existing file? Default: `TRUE`.
#' @param ... Additional arguments passed to methods:
#'   \describe{
#'     \item{`labels`}{(data.frame and list methods) Include a "Labels"
#'       reference sheet? Default: `TRUE`}
#'     \item{`frequencies`}{(codebook method) Include per-variable frequency
#'       sheets? Default: `FALSE`}
#'   }
#'
#' @return Invisibly returns the file path.
#'
#' @details
#' ## Data Frame Export
#'
#' The "Labels" sheet provides a complete lookup table for all variable and
#' value labels in your data, structured in long format. The "Type" column
#' distinguishes between regular value labels ("valid") and tagged missing
#' value labels ("missing"), making it easy to filter in Excel.
#'
#' Data values are written as their underlying numeric or character codes
#' (not as label text), preserving the original coding scheme.
#'
#' ## Codebook Export
#'
#' The codebook export reproduces the HTML codebook layout: each variable
#' occupies one row, with values, value labels, and frequencies stacked
#' within their cells using line breaks. Tagged NAs are shown below a
#' separator line, matching the visual style of `codebook()` in the
#' RStudio Viewer.
#'
#' ## When to Use This
#'
#' Use `write_xlsx()` when you:
#' \itemize{
#'   \item Need to share survey data with non-R users who need label context
#'   \item Want to export a codebook for manual review or documentation
#'   \item Need to combine multiple tables (data, codebook, frequencies) in
#'     one Excel file
#' }
#'
#' @seealso [codebook()] for generating codebook objects,
#'   [frequency()] for frequency tables,
#'   [read_spss()], [read_por()], [read_stata()], [read_sas()],
#'   [read_xpt()], [read_xlsx()] for importing labelled data
#'
#' @family data-export
#'
#' @examples
#' \dontrun{
#' # Export data with automatic label reference sheet
#' write_xlsx(survey_data, "survey_export.xlsx")
#'
#' # Export a codebook (reproduces HTML layout in Excel)
#' codebook(survey_data) |> write_xlsx("codebook.xlsx")
#'
#' # Export a codebook with per-variable frequency sheets
#' codebook(survey_data) |> write_xlsx("codebook_full.xlsx", frequencies = TRUE)
#'
#' # Export frequency tables (single or multiple variables)
#' frequency(survey_data, gender) |> write_xlsx("freq.xlsx")
#' frequency(survey_data, gender, life_satisfaction, region) |>
#'   write_xlsx("freq_multi.xlsx")
#'
#' # Multi-sheet export (mixed types: data frames, frequencies, codebooks)
#' write_xlsx(
#'   list(
#'     "Frequencies" = frequency(survey_data, gender, life_satisfaction),
#'     "Codebook"    = codebook(survey_data),
#'     "Data"        = survey_data
#'   ),
#'   "combined.xlsx"
#' )
#' }
#'
#' @export
write_xlsx <- function(x, file, ...) {
  UseMethod("write_xlsx")
}


# ---- data.frame method ------------------------------------------------------

#' @export
write_xlsx.data.frame <- function(x, file, labels = TRUE,
                                  overwrite = TRUE, ...) {
  .check_openxlsx2()
  .validate_xlsx_path(file, overwrite)

  # Convert haven_labelled_spss to tagged NA format for proper roundtripping
  x <- .ensure_tagged_na_format(x)

  wb <- openxlsx2::wb_workbook()

  # Sheet 1: Data
  .write_sheet(wb, "Data", x)

  # Sheet 2: Labels (optional)
  if (isTRUE(labels)) {
    labels_df <- .build_labels_sheet(x)
    if (nrow(labels_df) > 0L) {
      .write_sheet(wb, "Labels", labels_df)
    }
  }

  openxlsx2::wb_save(wb, file = file, overwrite = overwrite)
  invisible(file)
}


# ---- codebook method --------------------------------------------------------

#' @export
write_xlsx.codebook <- function(x, file, frequencies = FALSE,
                                overwrite = TRUE, ...) {
  .check_openxlsx2()
  .validate_xlsx_path(file, overwrite)

  wb <- openxlsx2::wb_workbook()

  # Sheet 1: Overview
  overview_df <- .build_overview_sheet(x)
  .write_sheet(wb, "Overview", overview_df)

  # Sheet 2: Codebook (mirrors HTML layout with stacked cells)
  .write_codebook_sheet(wb, x)

  # Optional: Per-variable frequency sheets
  if (isTRUE(frequencies) && !is.null(x$frequencies)) {
    for (var_name in names(x$frequencies)) {
      freq_df <- x$frequencies[[var_name]]
      if (is.data.frame(freq_df) && nrow(freq_df) > 0L) {
        sheet_name <- .sanitize_sheet_name(var_name)
        .write_sheet(wb, sheet_name, freq_df)
      }
    }
  }

  openxlsx2::wb_save(wb, file = file, overwrite = overwrite)
  invisible(file)
}


# ---- list method -------------------------------------------------------------

#' @export
write_xlsx.list <- function(x, file, labels = TRUE, overwrite = TRUE, ...) {
  .check_openxlsx2()
  .validate_xlsx_path(file, overwrite)

  # Validate: must be a named list
  if (is.null(names(x)) || any(names(x) == "")) {
    cli::cli_abort(c(
      "{.arg x} must be a named list.",
      "i" = "Each list element becomes a sheet, named after the list entry."
    ))
  }

  # Validate: all elements must be supported types
  supported_types <- vapply(x, function(el) {
    is.data.frame(el) || inherits(el, "frequency") || inherits(el, "codebook")
  }, logical(1))
  if (any(!supported_types)) {
    bad <- names(x)[!supported_types]
    cli::cli_abort(c(
      "All list elements must be data frames, {.cls frequency}, or {.cls codebook} objects.",
      "x" = "Unsupported element{?s}: {.val {bad}}"
    ))
  }

  wb <- openxlsx2::wb_workbook()

  # Write each element to its own sheet, dispatching by type
  for (nm in names(x)) {
    el <- x[[nm]]
    sheet_name <- .sanitize_sheet_name(nm)

    if (inherits(el, "frequency")) {
      # Frequency: create sheet, then write formatted frequency tables
      wb$add_worksheet(sheet_name)
      .write_frequency_tables(wb, sheet_name, el)

    } else if (inherits(el, "codebook")) {
      # Codebook: write codebook sheet (without separate Overview)
      .write_codebook_sheet(wb, el, sheet_name = sheet_name)

    } else if (is.data.frame(el)) {
      # Data frame: standard export with header styling
      el <- .ensure_tagged_na_format(el)
      .write_sheet(wb, sheet_name, el)
    }
  }

  # Combined Labels sheet (only from data.frame elements)
  if (isTRUE(labels)) {
    df_names <- names(x)[vapply(x, is.data.frame, logical(1))]
    if (length(df_names) > 0L) {
      all_labels <- do.call(rbind, lapply(df_names, function(nm) {
        lbl <- .build_labels_sheet(x[[nm]])
        if (nrow(lbl) > 0L) {
          lbl <- cbind(Sheet = nm, lbl, stringsAsFactors = FALSE)
        }
        lbl
      }))
      if (!is.null(all_labels) && nrow(all_labels) > 0L) {
        .write_sheet(wb, "Labels", all_labels)
      }
    }
  }

  openxlsx2::wb_save(wb, file = file, overwrite = overwrite)
  invisible(file)
}


# ---- frequency method -------------------------------------------------------

#' @describeIn write_xlsx Export frequency tables to Excel
#'
#' Exports one or more frequency tables to a single Excel sheet, mirroring
#' the console print output. Each variable gets a header row, stats summary,
#' column headers, data rows, and total rows. Multiple variables are separated
#' by 3 blank rows.
#'
#' @examples
#' \dontrun{
#' # Single variable
#' frequency(survey_data, gender) |> write_xlsx("freq.xlsx")
#'
#' # Multiple variables on one sheet
#' frequency(survey_data, gender, life_satisfaction, region) |>
#'   write_xlsx("freq_multi.xlsx")
#' }
#'
#' @export
write_xlsx.frequency <- function(x, file, overwrite = TRUE, ...) {
  .check_openxlsx2()
  .validate_xlsx_path(file, overwrite)

  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet("Frequency")

  .write_frequency_tables(wb, "Frequency", x)

  openxlsx2::wb_save(wb, file = file, overwrite = overwrite)
  invisible(file)
}


# ============================================================================
# Frequency Table Writer
# ============================================================================

#' Write frequency tables for all variables in a frequency object
#'
#' Iterates over each variable in the frequency object and writes its
#' frequency table as a formatted block. Multiple variables are stacked
#' vertically with 3 blank rows between them.
#'
#' @param wb An openxlsx2 workbook object.
#' @param sheet Sheet name to write to.
#' @param freq_obj A frequency object (class "frequency").
#' @noRd
.write_frequency_tables <- function(wb, sheet, freq_obj) {
  opts     <- freq_obj$options
  vars     <- freq_obj$variables
  results  <- freq_obj$results
  stats_df <- freq_obj$stats
  labels   <- freq_obj$labels

  # Determine active columns based on options
  col_headers <- "Value"
  if (isTRUE(opts$show.labels)) col_headers <- c(col_headers, "Label")
  col_headers <- c(col_headers, "N")
  if (isTRUE(opts$show.prc))   col_headers <- c(col_headers, "Raw %")
  if (isTRUE(opts$show.valid)) col_headers <- c(col_headers, "Valid %")
  if (isTRUE(opts$show.sum))   col_headers <- c(col_headers, "Cum. %")

  ncols <- length(col_headers)
  cur_row <- 1L

  for (v_idx in seq_along(vars)) {
    var_name <- vars[v_idx]
    var_results <- results[results$Variable == var_name, , drop = FALSE]
    var_stats   <- stats_df[stats_df$Variable == var_name, , drop = FALSE]
    var_label   <- if (!is.null(labels) && var_name %in% names(labels)) {
      labels[var_name]
    } else {
      ""
    }

    # --- Row 1: Variable header (merged, dark background) ---
    header_text <- if (nzchar(var_label)) {
      paste0(var_name, " (", var_label, ")")
    } else {
      var_name
    }
    .freq_write_variable_header(wb, sheet, cur_row, ncols, header_text)
    cur_row <- cur_row + 1L

    # --- Row 2: Stats summary (gray background, italic) ---
    if (nrow(var_stats) > 0L) {
      .freq_write_stats_row(wb, sheet, cur_row, ncols, var_stats)
      cur_row <- cur_row + 1L
    }

    # --- Row 3: Column headers (medium gray background) ---
    .freq_write_col_headers(wb, sheet, cur_row, col_headers)
    cur_row <- cur_row + 1L

    # --- Data rows + summary rows ---
    cur_row <- .freq_write_data_rows(
      wb, sheet, cur_row, var_results, col_headers, opts
    )

    # 3 blank rows between variables (except after the last one)
    if (v_idx < length(vars)) {
      cur_row <- cur_row + 3L
    }
  }

  # Column widths
  .freq_set_col_widths(wb, sheet, col_headers)

  # Freeze pane: nothing to freeze (headers repeat per variable)
}


#' Write the variable header row (merged, bold)
#' @noRd
.freq_write_variable_header <- function(wb, sheet, row, ncols, text) {
  start_col <- openxlsx2::int2col(1)
  end_col   <- openxlsx2::int2col(ncols)
  dims <- paste0(start_col, row, ":", end_col, row)

  wb$add_data(sheet = sheet, x = text,
              dims = paste0(start_col, row), col_names = FALSE)
  wb$merge_cells(sheet = sheet, dims = dims)

  # Bold, 11pt
  wb$add_font(sheet = sheet, dims = dims, bold = TRUE, size = 11)
}


#' Write the stats summary row (gray text)
#' @noRd
.freq_write_stats_row <- function(wb, sheet, row, ncols, var_stats) {
  parts <- c(paste0("N=", var_stats$total_n[1]))
  if ("valid_n" %in% names(var_stats)) {
    parts <- c(parts, paste0("Valid N=", var_stats$valid_n[1]))
  }
  if ("mean" %in% names(var_stats) && !is.na(var_stats$mean[1])) {
    parts <- c(parts, paste0("Mean=", round(var_stats$mean[1], 2)))
  }
  if ("sd" %in% names(var_stats) && !is.na(var_stats$sd[1])) {
    parts <- c(parts, paste0("SD=", round(var_stats$sd[1], 2)))
  }
  if ("skewness" %in% names(var_stats) && !is.na(var_stats$skewness[1])) {
    parts <- c(parts, paste0("Skewness=", round(var_stats$skewness[1], 2)))
  }

  stats_text <- paste(parts, collapse = "    ")

  start_col <- openxlsx2::int2col(1)
  end_col   <- openxlsx2::int2col(ncols)
  dims <- paste0(start_col, row, ":", end_col, row)

  wb$add_data(sheet = sheet, x = stats_text,
              dims = paste0(start_col, row), col_names = FALSE)
  wb$merge_cells(sheet = sheet, dims = dims)

  # Gray text, regular size
  wb$add_font(sheet = sheet, dims = dims, size = 10,
              color = openxlsx2::wb_color("888888"))
}


#' Write column headers row (bold, bottom border)
#' @noRd
.freq_write_col_headers <- function(wb, sheet, row, col_headers) {
  for (j in seq_along(col_headers)) {
    dims <- paste0(openxlsx2::int2col(j), row)
    wb$add_data(sheet = sheet, x = col_headers[j], dims = dims,
                col_names = FALSE)
  }

  dims_range <- paste0(
    openxlsx2::int2col(1), row, ":",
    openxlsx2::int2col(length(col_headers)), row
  )
  wb$add_font(sheet = sheet, dims = dims_range, bold = TRUE, size = 10)
  wb$add_border(sheet = sheet, dims = dims_range,
                bottom_color = openxlsx2::wb_color("000000"),
                bottom_border = "thin")
}


#' Write data rows, summary rows, and tagged NA rows
#'
#' Mirrors the print_table() logic from frequency.R.
#'
#' @return The next available row index after writing.
#' @noRd
.freq_write_data_rows <- function(wb, sheet, start_row, results, col_headers, opts) {
  cur_row <- start_row

  has_tagged_rows <- "na_display_value" %in% names(results) &&
    any(!is.na(results$na_display_value))

  if (has_tagged_rows) {
    cur_row <- .freq_write_tagged_layout(
      wb, sheet, cur_row, results, col_headers, opts
    )
  } else {
    cur_row <- .freq_write_standard_layout(
      wb, sheet, cur_row, results, col_headers, opts
    )
  }

  cur_row
}


#' Standard layout: valid rows → Total Valid → NA rows → Total Missing
#' @noRd
.freq_write_standard_layout <- function(wb, sheet, start_row, results,
                                         col_headers, opts) {
  cur_row <- start_row
  ncols <- length(col_headers)

  na_idx <- is.na(results$value)
  valid_rows <- results[!na_idx, , drop = FALSE]
  na_rows    <- results[na_idx, , drop = FALSE]
  has_na <- nrow(na_rows) > 0L && isTRUE(opts$show.na)

  # Write valid data rows
  for (i in seq_len(nrow(valid_rows))) {
    .freq_write_one_row(wb, sheet, cur_row, valid_rows[i, ], col_headers,
                        opts, style = "data")
    cur_row <- cur_row + 1L
  }

  # Summary section
  valid_freq <- sum(valid_rows$freq, na.rm = TRUE)
  valid_prc  <- sum(valid_rows$prc, na.rm = TRUE)

  if (has_na) {
    na_freq <- sum(na_rows$freq, na.rm = TRUE)
    na_prc  <- sum(na_rows$prc, na.rm = TRUE)

    # Total Valid row
    .freq_write_summary_row(wb, sheet, cur_row, col_headers, opts,
                            value = "Total", sublabel = "Total Valid",
                            freq = valid_freq, prc = valid_prc,
                            valid_prc = 100, cum_prc = NA)
    cur_row <- cur_row + 1L

    # NA data rows
    for (i in seq_len(nrow(na_rows))) {
      .freq_write_one_row(wb, sheet, cur_row, na_rows[i, ], col_headers,
                          opts, style = "na")
      cur_row <- cur_row + 1L
    }

    # Total Missing row
    .freq_write_summary_row(wb, sheet, cur_row, col_headers, opts,
                            value = "Total", sublabel = "Total Missing",
                            freq = na_freq, prc = na_prc,
                            valid_prc = NA, cum_prc = NA)
    cur_row <- cur_row + 1L
  } else {
    # Simple total
    .freq_write_summary_row(wb, sheet, cur_row, col_headers, opts,
                            value = "Total", sublabel = "",
                            freq = valid_freq, prc = valid_prc,
                            valid_prc = 100, cum_prc = NA)
    cur_row <- cur_row + 1L
  }

  cur_row
}


#' Tagged NA layout: uses pre-expanded results with na_display_value
#' @noRd
.freq_write_tagged_layout <- function(wb, sheet, start_row, results,
                                       col_headers, opts) {
  cur_row <- start_row
  ncols <- length(col_headers)

  prev_was_na_row <- FALSE

  for (i in seq_len(nrow(results))) {
    row <- results[i, ]

    is_na_row <- isTRUE(row$is_na_row)
    ndv <- row$na_display_value

    # Determine row style and whether to insert a separator
    if (!is.na(ndv)) {
      if (ndv == "Total") {
        # Total Valid summary row
        lbl <- as.character(row$label)
        .freq_write_summary_row(
          wb, sheet, cur_row, col_headers, opts,
          value = "Total", sublabel = if (is.na(lbl)) "Total Valid" else lbl,
          freq = row$freq, prc = row$prc,
          valid_prc = if (is.na(row$valid_prc)) NA else row$valid_prc,
          cum_prc = if (is.na(row$cum_prc)) NA else row$cum_prc
        )
        cur_row <- cur_row + 1L
        prev_was_na_row <- FALSE
        next

      } else if (ndv == "NA(total)") {
        # Total Missing summary row
        lbl <- as.character(row$label)
        .freq_write_summary_row(
          wb, sheet, cur_row, col_headers, opts,
          value = "Total", sublabel = if (is.na(lbl)) "Total Missing" else lbl,
          freq = row$freq, prc = row$prc,
          valid_prc = NA, cum_prc = NA
        )
        cur_row <- cur_row + 1L
        prev_was_na_row <- FALSE
        next

      } else if (is_na_row) {
        # Individual tagged NA row
        .freq_write_one_row(wb, sheet, cur_row, row, col_headers, opts,
                            style = "tagged_na", display_value = ndv)
        cur_row <- cur_row + 1L
        prev_was_na_row <- TRUE
        next
      }
    }

    # Regular data row
    .freq_write_one_row(wb, sheet, cur_row, row, col_headers, opts,
                        style = "data")
    cur_row <- cur_row + 1L
    prev_was_na_row <- FALSE
  }

  cur_row
}


#' Write a single data row to the frequency sheet
#' @param style One of "data", "na", "tagged_na"
#' @param display_value Optional override for the value cell (for tagged NAs)
#' @noRd
.freq_write_one_row <- function(wb, sheet, row, data_row, col_headers, opts,
                                 style = "data", display_value = NULL) {
  ncols <- length(col_headers)
  col_idx <- 0L

  # Value
  col_idx <- col_idx + 1L
  if (!is.null(display_value)) {
    val <- display_value
  } else if (is.na(data_row$value)) {
    val <- "NA"
  } else {
    val <- as.character(data_row$value)
  }
  .write_cell(wb, sheet, row, col_idx, val)

  # Label
  if (isTRUE(opts$show.labels)) {
    col_idx <- col_idx + 1L
    lbl <- if (is.na(data_row$label) || data_row$label == "") "" else as.character(data_row$label)
    .write_cell(wb, sheet, row, col_idx, lbl)
  }

  # N
  col_idx <- col_idx + 1L
  .write_cell(wb, sheet, row, col_idx, round(data_row$freq))

  # Raw %
  if (isTRUE(opts$show.prc)) {
    col_idx <- col_idx + 1L
    .write_cell(wb, sheet, row, col_idx,
                if (is.na(data_row$prc)) "" else round(data_row$prc, 2))
  }

  # Valid %
  if (isTRUE(opts$show.valid)) {
    col_idx <- col_idx + 1L
    .write_cell(wb, sheet, row, col_idx,
                if (is.na(data_row$valid_prc)) "" else round(data_row$valid_prc, 2))
  }

  # Cum. %
  if (isTRUE(opts$show.sum)) {
    col_idx <- col_idx + 1L
    .write_cell(wb, sheet, row, col_idx,
                if (is.na(data_row$cum_prc)) "" else round(data_row$cum_prc, 2))
  }

  # NA rows: gray text to visually separate from valid data
  if (style == "tagged_na" || style == "na") {
    dims <- paste0(openxlsx2::int2col(1), row, ":",
                   openxlsx2::int2col(ncols), row)
    wb$add_font(sheet = sheet, dims = dims,
                color = openxlsx2::wb_color("999999"))
  }
}


#' Write a summary row (Total Valid, Total Missing, Total)
#' @noRd
.freq_write_summary_row <- function(wb, sheet, row, col_headers, opts,
                                     value, sublabel, freq, prc,
                                     valid_prc, cum_prc) {
  ncols <- length(col_headers)
  col_idx <- 0L

  # Value column: "Total"

  col_idx <- col_idx + 1L
  .write_cell(wb, sheet, row, col_idx, value)

  # Label column: "Total Valid" / "Total Missing" / ""
  if (isTRUE(opts$show.labels)) {
    col_idx <- col_idx + 1L
    .write_cell(wb, sheet, row, col_idx, sublabel)
  }

  # N
  col_idx <- col_idx + 1L
  .write_cell(wb, sheet, row, col_idx, round(freq))

  # Raw %
  if (isTRUE(opts$show.prc)) {
    col_idx <- col_idx + 1L
    .write_cell(wb, sheet, row, col_idx,
                if (is.na(prc)) "" else round(prc, 2))
  }

  # Valid %
  if (isTRUE(opts$show.valid)) {
    col_idx <- col_idx + 1L
    .write_cell(wb, sheet, row, col_idx,
                if (is.na(valid_prc)) "" else round(valid_prc, 2))
  }

  # Cum. %
  if (isTRUE(opts$show.sum)) {
    col_idx <- col_idx + 1L
    .write_cell(wb, sheet, row, col_idx,
                if (is.na(cum_prc)) "" else round(cum_prc, 2))
  }

  # Bold + top border
  dims <- paste0(openxlsx2::int2col(1), row, ":",
                 openxlsx2::int2col(ncols), row)
  wb$add_font(sheet = sheet, dims = dims, bold = TRUE, size = 10)
  wb$add_border(sheet = sheet, dims = dims,
                top_color = openxlsx2::wb_color("000000"),
                top_border = "thin")
}


#' Set column widths for frequency sheet
#' @noRd
.freq_set_col_widths <- function(wb, sheet, col_headers) {
  for (j in seq_along(col_headers)) {
    w <- switch(col_headers[j],
      "Value" = 18,
      "Label" = 28,
      "N"     = 12,
      "Raw %" = 12,
      "Valid %" = 12,
      "Cum. %" = 12,
      14  # default
    )
    wb$set_col_widths(sheet = sheet, cols = j, widths = w)
  }
}


# ============================================================================
# Internal Helpers
# ============================================================================

#' Convert haven_labelled_spss columns to tagged NA format
#'
#' When data is loaded with `haven::read_sav(user_na = TRUE)` directly
#' (instead of through `read_spss()`), columns have `haven_labelled_spss`
#' class with `na_values`/`na_range` attributes but no tagged NAs.
#' This function converts them to the tagged NA format with `na_tag_map`
#' so that write_xlsx() can produce a correct Labels sheet and read_xlsx()
#' can reconstruct the missing value structure on roundtrip.
#'
#' @param data A data frame potentially containing haven_labelled_spss columns.
#' @return The data frame with haven_labelled_spss columns converted.
#' @noRd
.ensure_tagged_na_format <- function(data) {
  has_spss_na <- vapply(data, function(col) {
    inherits(col, "haven_labelled_spss") ||
      (inherits(col, "haven_labelled") &&
         (!is.null(attr(col, "na_values", exact = TRUE)) ||
          !is.null(attr(col, "na_range", exact = TRUE))))
  }, logical(1))

  if (any(has_spss_na)) {
    data <- .tag_spss_missing_values(data, verbose = FALSE)
  }

  data
}


#' Check openxlsx2 availability
#' @noRd
.check_openxlsx2 <- function() {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg openxlsx2} is required for Excel export.",
      "i" = "Install it with: {.code install.packages(\"openxlsx2\")}"
    ))
  }
}


#' Validate xlsx file path
#' @noRd
.validate_xlsx_path <- function(file, overwrite) {
  if (!grepl("\\.xlsx$", file, ignore.case = TRUE)) {
    cli::cli_abort(c(
      "{.arg file} must end in {.val .xlsx}.",
      "x" = "Got: {.file {file}}"
    ))
  }
  if (!isTRUE(overwrite) && file.exists(file)) {
    cli::cli_abort(c(
      "File already exists: {.file {file}}",
      "i" = "Use {.code overwrite = TRUE} to replace it."
    ))
  }
}


#' Sanitize sheet name to Excel limits
#' @noRd
.sanitize_sheet_name <- function(name) {
  # Replace invalid Excel sheet name characters: [ ] * ? / \ :
  name <- gsub("[][*?/\\\\:]", "_", name)
  # Truncate to 31 characters (Excel limit)
  if (nchar(name) > 31L) {
    name <- substr(name, 1L, 31L)
  }
  name
}


#' Write a data frame to a worksheet with header formatting
#' @noRd
.write_sheet <- function(wb, sheet_name, df) {
  # Sanitize name to ensure add_worksheet and add_data use the same name
  sheet_name <- .sanitize_sheet_name(sheet_name)
  wb$add_worksheet(sheet_name)

  # Convert haven_labelled columns to base types for Excel compatibility
  for (i in seq_len(ncol(df))) {
    if (inherits(df[[i]], "haven_labelled")) {
      tag_map <- attr(df[[i]], "na_tag_map")
      if (!is.null(tag_map) && is.numeric(tag_map)) {
        # Preserve original missing codes (-9, -11, etc.) in data cells
        # so read_xlsx() can reconstruct tagged NAs on roundtrip
        df[[i]] <- untag_na(df[[i]])
      } else {
        df[[i]] <- as.double(df[[i]])
      }
    }
  }

  wb$add_data(sheet = sheet_name, x = df)

  # Style header row: bold, dark background, white text
  if (nrow(df) > 0L || ncol(df) > 0L) {
    .style_header_row(wb, sheet_name, ncol(df))
    # Freeze first row
    wb$freeze_pane(sheet = sheet_name, first_row = TRUE)
    # Auto column widths
    wb$set_col_widths(sheet = sheet_name, cols = seq_len(ncol(df)),
                      widths = "auto")
  }
}


#' Apply header formatting to a worksheet
#' @noRd
.style_header_row <- function(wb, sheet, ncol) {
  if (ncol == 0L) return(invisible(NULL))
  dims <- paste0(openxlsx2::int2col(1), "1:",
                 openxlsx2::int2col(ncol), "1")
  wb$add_font(sheet = sheet, dims = dims, bold = TRUE,
              color = openxlsx2::wb_color("FFFFFF"))
  wb$add_fill(sheet = sheet, dims = dims,
              color = openxlsx2::wb_color("3B3F51"))
}


# ============================================================================
# Labels Sheet Builder (for data.frame and list methods)
# ============================================================================

#' Build the Labels reference sheet from a data frame
#' @noRd
.build_labels_sheet <- function(data) {
  rows <- list()

  for (col_name in names(data)) {
    x <- data[[col_name]]
    var_label <- attr(x, "label", exact = TRUE)
    if (is.null(var_label)) var_label <- ""

    labels_attr <- attr(x, "labels", exact = TRUE)
    tag_map <- attr(x, "na_tag_map", exact = TRUE)

    # Determine column type for roundtrip reconstruction
    col_type <- if (inherits(x, "haven_labelled")) {
      "haven_labelled"
    } else if (is.factor(x)) {
      "factor"
    } else {
      ""
    }

    has_content <- FALSE

    # Value labels (non-NA entries, excluding values claimed by na_tag_map)
    if (!is.null(labels_attr)) {
      non_na_mask <- !is.na(labels_attr)
      if (any(non_na_mask)) {
        valid_labels <- labels_attr[non_na_mask]
        # Exclude values that are in na_tag_map (those are missing, not valid)
        if (!is.null(tag_map)) {
          in_tag_map <- unname(valid_labels) %in% unname(tag_map)
          valid_labels <- valid_labels[!in_tag_map]
        }
        for (j in seq_along(valid_labels)) {
          rows[[length(rows) + 1L]] <- data.frame(
            Variable = col_name,
            Variable_Label = var_label,
            Value = as.character(valid_labels[j]),
            Value_Label = names(valid_labels)[j],
            Type = "valid",
            Column_Type = col_type,
            stringsAsFactors = FALSE
          )
        }
        if (length(valid_labels) > 0L) has_content <- TRUE
      }
    }

    # Factor levels (if not haven-labelled)
    if (is.factor(x) && is.null(labels_attr)) {
      lvls <- levels(x)
      for (lv in lvls) {
        rows[[length(rows) + 1L]] <- data.frame(
          Variable = col_name,
          Variable_Label = var_label,
          Value = lv,
          Value_Label = lv,
          Type = "valid",
          Column_Type = col_type,
          stringsAsFactors = FALSE
        )
      }
      has_content <- TRUE
    }

    # Tagged NA entries
    if (!is.null(tag_map)) {
      for (k in seq_along(tag_map)) {
        code <- tag_map[k]
        tag_char <- names(tag_map)[k]

        na_label <- ""
        if (!is.null(labels_attr)) {
          # First: try tagged NA entries (normal case after .tag_spss_missing_values)
          na_entries <- labels_attr[is.na(labels_attr)]
          if (length(na_entries) > 0L && requireNamespace("haven", quietly = TRUE)) {
            na_tags <- vapply(na_entries, haven::na_tag, character(1))
            match_idx <- match(tag_char, na_tags)
            if (!is.na(match_idx)) {
              na_label <- names(na_entries)[match_idx]
            }
          }
          # Fallback: look up the numeric code in non-NA labels
          # (covers the case where labels were not converted to tagged NAs)
          if (!nzchar(na_label)) {
            non_na_entries <- labels_attr[!is.na(labels_attr)]
            code_match <- match(code, unname(non_na_entries))
            if (!is.na(code_match)) {
              na_label <- names(non_na_entries)[code_match]
            }
          }
        }

        rows[[length(rows) + 1L]] <- data.frame(
          Variable = col_name,
          Variable_Label = var_label,
          Value = as.character(code),
          Value_Label = na_label,
          Type = "missing",
          Column_Type = col_type,
          stringsAsFactors = FALSE
        )
      }
      has_content <- TRUE
    }

    # Variable with label but no value labels: still include
    if (!has_content && nzchar(var_label)) {
      rows[[length(rows) + 1L]] <- data.frame(
        Variable = col_name,
        Variable_Label = var_label,
        Value = "",
        Value_Label = "",
        Type = "",
        Column_Type = col_type,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0L) {
    return(data.frame(
      Variable = character(0), Variable_Label = character(0),
      Value = character(0), Value_Label = character(0),
      Type = character(0), Column_Type = character(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, rows)
}


# ============================================================================
# Codebook Sheet Builder (mirrors HTML layout)
# ============================================================================

#' Write the Codebook sheet that mirrors the HTML codebook layout
#'
#' Each variable gets ONE row. Values, Value Labels, and Freq. are stacked
#' inside cells using line breaks. Tagged NAs appear below a "---" separator,
#' matching the visual style of the HTML codebook.
#'
#' @noRd
.write_codebook_sheet <- function(wb, cb_obj, sheet_name = "Codebook") {
  sheet <- .sanitize_sheet_name(sheet_name)
  wb$add_worksheet(sheet)

  tbl <- cb_obj$codebook
  opts <- cb_obj$options
  freq_list <- cb_obj$frequencies

  # Determine columns based on codebook options
  headers <- c()
  if (isTRUE(opts$show.id)) headers <- c(headers, "ID")
  headers <- c(headers, "Name")
  if (isTRUE(opts$show.type)) headers <- c(headers, "Type")
  if (isTRUE(opts$show.labels)) headers <- c(headers, "Label")
  if (isTRUE(opts$show.values)) headers <- c(headers, "Values", "Value Labels")
  if (isTRUE(opts$show.freq)) headers <- c(headers, "Freq.")

  ncols <- length(headers)

  # Write header row
  for (j in seq_along(headers)) {
    dims <- paste0(openxlsx2::int2col(j), "1")
    wb$add_data(sheet = sheet, x = headers[j], dims = dims,
                col_names = FALSE)
  }
  .style_header_row(wb, sheet, ncols)
  wb$freeze_pane(sheet = sheet, first_row = TRUE)

  # Build each variable row
  for (i in seq_len(nrow(tbl))) {
    row_idx <- i + 1L  # Excel row (1-based, header is row 1)
    col_idx <- 0L

    # --- ID ---
    if (isTRUE(opts$show.id)) {
      col_idx <- col_idx + 1L
      .write_cell(wb, sheet, row_idx, col_idx, tbl$position[i])
    }

    # --- Name ---
    col_idx <- col_idx + 1L
    .write_cell(wb, sheet, row_idx, col_idx, tbl$name[i])

    # --- Type ---
    if (isTRUE(opts$show.type)) {
      col_idx <- col_idx + 1L
      .write_cell(wb, sheet, row_idx, col_idx, tbl$type_short[i])
    }

    # --- Label ---
    if (isTRUE(opts$show.labels)) {
      col_idx <- col_idx + 1L
      lbl <- if (!is.na(tbl$label[i])) tbl$label[i] else ""
      .write_cell(wb, sheet, row_idx, col_idx, lbl)
    }

    # --- Values, Value Labels, Freq (stacked cells) ---
    if (isTRUE(opts$show.values) || isTRUE(opts$show.freq)) {
      stacked <- .build_stacked_cells(tbl, i, freq_list, opts)

      if (isTRUE(opts$show.values)) {
        col_idx <- col_idx + 1L
        .write_cell(wb, sheet, row_idx, col_idx, stacked$values, wrap = TRUE)
        col_idx <- col_idx + 1L
        .write_cell(wb, sheet, row_idx, col_idx, stacked$labels, wrap = TRUE)
      }

      if (isTRUE(opts$show.freq)) {
        col_idx <- col_idx + 1L
        .write_cell(wb, sheet, row_idx, col_idx, stacked$freqs, wrap = TRUE)
      }
    }
  }

  # Column widths: auto for metadata columns, fixed for stacked columns
  wb$set_col_widths(sheet = sheet, cols = seq_len(ncols), widths = "auto")

  # Widen value/label/freq columns since auto-width doesn't handle \n well
  if (isTRUE(opts$show.values)) {
    val_col <- which(headers == "Values")
    lbl_col <- which(headers == "Value Labels")
    if (length(val_col) > 0L) {
      wb$set_col_widths(sheet = sheet, cols = val_col, widths = 14)
    }
    if (length(lbl_col) > 0L) {
      wb$set_col_widths(sheet = sheet, cols = lbl_col, widths = 28)
    }
  }
  if (isTRUE(opts$show.freq)) {
    freq_col <- which(headers == "Freq.")
    if (length(freq_col) > 0L) {
      wb$set_col_widths(sheet = sheet, cols = freq_col, widths = 10)
    }
  }

  # Vertical alignment: top for all data cells
  if (nrow(tbl) > 0L) {
    data_dims <- paste0(
      openxlsx2::int2col(1), "2:",
      openxlsx2::int2col(ncols), nrow(tbl) + 1L
    )
    wb$add_cell_style(sheet = sheet, dims = data_dims,
                      vertical = "top")
  }
}


#' Build stacked cell content for Values, Value Labels, and Freq columns
#'
#' Mirrors the HTML codebook: valid values first, then "---" separator,
#' then tagged NAs (italic in the HTML, shown with their codes).
#'
#' @return List with elements: values, labels, freqs (each a single string
#'   with \\n line breaks)
#' @noRd
.build_stacked_cells <- function(tbl, i, freq_list, opts) {
  emp_vals <- tbl$empirical_values[[i]]
  val_labels <- tbl$value_labels[[i]]
  is_rng <- tbl$is_range[i]
  na_vals <- tbl$na_values[[i]]
  na_lbls <- tbl$na_labels[[i]]
  n_sys_na <- tbl$n_system_na[i]
  freq_data <- freq_list[[tbl$name[i]]]

  val_lines <- character(0)
  lbl_lines <- character(0)
  freq_lines <- character(0)

  if (is_rng) {
    # Range display (single line)
    val_lines <- paste(emp_vals, collapse = "")
    lbl_lines <- ""
    freq_lines <- ""
  } else if (length(emp_vals) > 0L) {
    # Per-value lines
    for (v in emp_vals) {
      val_lines <- c(val_lines, v)

      # Value label
      lbl_text <- if (!is.null(val_labels) && v %in% names(val_labels)) {
        unname(val_labels[v])
      } else {
        ""
      }
      lbl_lines <- c(lbl_lines, lbl_text)

      # Frequency
      freq_text <- ""
      if (!is.null(freq_data)) {
        freq_row <- freq_data[as.character(freq_data$value) == v, ]
        if (nrow(freq_row) > 0L) {
          freq_text <- as.character(round(freq_row$freq[1]))
        }
      }
      freq_lines <- c(freq_lines, freq_text)
    }
  }

  # Tagged NA section (below separator)
  if (isTRUE(opts$show.na) && length(na_vals) > 0L) {
    sep <- "\u2500\u2500\u2500"  # "───" thin horizontal line
    val_lines <- c(val_lines, sep)
    lbl_lines <- c(lbl_lines, sep)
    freq_lines <- c(freq_lines, sep)

    for (nav in na_vals) {
      val_lines <- c(val_lines, nav)

      # NA label
      na_lbl_text <- if (nav %in% names(na_lbls)) {
        unname(na_lbls[nav])
      } else {
        ""
      }
      lbl_lines <- c(lbl_lines, na_lbl_text)

      # NA frequency
      na_freq_text <- ""
      if (!is.null(freq_data) && "na_display_value" %in% names(freq_data)) {
        na_freq_row <- freq_data[
          !is.na(freq_data$na_display_value) &
            freq_data$na_display_value == nav, ]
        if (nrow(na_freq_row) > 0L) {
          na_freq_text <- as.character(round(na_freq_row$freq[1]))
        }
      }
      freq_lines <- c(freq_lines, na_freq_text)
    }

    # System NA row
    if (n_sys_na > 0L) {
      val_lines <- c(val_lines, "NA")
      lbl_lines <- c(lbl_lines, "(system missing)")

      sys_freq_text <- ""
      if (!is.null(freq_data) && "na_display_value" %in% names(freq_data)) {
        sys_freq_row <- freq_data[
          !is.na(freq_data$na_display_value) &
            freq_data$na_display_value == "NA", ]
        if (nrow(sys_freq_row) > 0L) {
          sys_freq_text <- as.character(round(sys_freq_row$freq[1]))
        } else {
          sys_freq_text <- as.character(n_sys_na)
        }
      } else {
        sys_freq_text <- as.character(n_sys_na)
      }
      freq_lines <- c(freq_lines, sys_freq_text)
    }
  }

  list(
    values = paste(val_lines, collapse = "\n"),
    labels = paste(lbl_lines, collapse = "\n"),
    freqs  = paste(freq_lines, collapse = "\n")
  )
}


#' Write a single cell value to a worksheet
#' @noRd
.write_cell <- function(wb, sheet, row, col, value, wrap = FALSE) {
  dims <- paste0(openxlsx2::int2col(col), row)
  wb$add_data(sheet = sheet, x = value, dims = dims, col_names = FALSE)
  if (wrap) {
    wb$add_cell_style(sheet = sheet, dims = dims, wrap_text = TRUE)
  }
}


# ============================================================================
# Overview Sheet Builder
# ============================================================================

#' Build the Overview sheet from a codebook object
#' @noRd
.build_overview_sheet <- function(cb) {
  info <- cb$data_info

  rows <- list()
  .add_row <- function(field, value) {
    rows[[length(rows) + 1L]] <<- data.frame(
      Field = field, Value = as.character(value), stringsAsFactors = FALSE
    )
  }

  if (!is.null(info$name))        .add_row("Dataset", info$name)
  if (!is.null(info$nrow))        .add_row("Observations", info$nrow)
  if (!is.null(info$ncol))        .add_row("Variables", info$ncol)
  if (!is.null(info$n_selected))  .add_row("Selected Variables", info$n_selected)
  if (!is.null(info$n_numeric))   .add_row("Numeric Variables", info$n_numeric)
  if (!is.null(info$n_factors))   .add_row("Factor Variables", info$n_factors)
  if (!is.null(info$n_character)) .add_row("Character Variables", info$n_character)
  if (!is.null(info$n_labelled))  .add_row("Labelled Variables", info$n_labelled)
  if (!is.null(info$n_missing_any)) .add_row("Variables with Missing", info$n_missing_any)
  if (!is.null(cb$weights))       .add_row("Weights", cb$weights)

  .add_row("Generated", format(Sys.time(), "%Y-%m-%d %H:%M"))

  do.call(rbind, rows)
}
