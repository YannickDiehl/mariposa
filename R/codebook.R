# =============================================================================
# CODEBOOK - Interactive Data Dictionary for Survey Data
# =============================================================================
# Generates a professional HTML codebook displayed in the RStudio Viewer,
# inspired by sjPlot::view_df(). Shows variable names, types, labels,
# empirical values, value labels, and frequency counts.

# Internal Helpers
# ----------------

#' Classify a variable's type for codebook display
#' @param x A data vector
#' @return List with type (full description) and type_short (abbreviated)
#' @keywords internal
.classify_type <- function(x) {
  if (inherits(x, "haven_labelled")) {
    storage <- typeof(x)
    type_short <- paste0("lbl+", if (storage == "double") "dbl" else "int")
    type <- paste0("haven_labelled<", storage, ">")
  } else if (is.ordered(x)) {
    type_short <- paste0("ord(", length(levels(x)), ")")
    type <- "ordered factor"
  } else if (is.factor(x)) {
    type_short <- paste0("fct(", length(levels(x)), ")")
    type <- "factor"
  } else if (is.integer(x)) {
    type_short <- "int"
    type <- "integer"
  } else if (is.numeric(x)) {
    type_short <- "dbl"
    type <- "numeric"
  } else if (is.character(x)) {
    type_short <- "chr"
    type <- "character"
  } else if (is.logical(x)) {
    type_short <- "lgl"
    type <- "logical"
  } else if (inherits(x, "Date")) {
    type_short <- "date"
    type <- "Date"
  } else if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    type_short <- "dttm"
    type <- "datetime"
  } else {
    type_short <- substr(class(x)[1], 1, 3)
    type <- class(x)[1]
  }

  list(type = type, type_short = type_short)
}

#' Extract codebook metadata for a single variable
#' @param x Data vector
#' @param var_name Variable name
#' @param position Column position in original data
#' @param max_values Maximum number of values to display before truncating
#' @return Named list of metadata
#' @keywords internal
.extract_var_metadata <- function(x, var_name, position, max_values = 10) {
  type_info <- .classify_type(x)

  # Variable label (haven-style) — coerce to character for safety,

  # because some SPSS imports store numeric label attributes
  var_label <- attr(x, "label")
  var_label <- if (is.null(var_label)) {
    NA_character_
  } else {
    paste(as.character(var_label), collapse = " | ")
  }

  # Value labels via existing helper
  raw_labels <- .build_label_map(x)

  # For haven-labelled without label_map, extract directly
  if (is.null(raw_labels) && !is.null(attr(x, "labels"))) {
    value_labels_attr <- attr(x, "labels")
    raw_labels <- stats::setNames(
      names(value_labels_attr),
      as.character(value_labels_attr)
    )
  }

  # For factors, always show levels as value labels
  if (is.null(raw_labels) && is.factor(x)) {
    lvls <- levels(x)
    raw_labels <- stats::setNames(lvls, lvls)
  }

  # Basic counts
  n_total <- length(x)
  n_missing <- sum(is.na(x))
  n_valid <- n_total - n_missing
  n_unique <- length(unique(x[!is.na(x)]))

  # Empirical values: the actual distinct values found in the data
  if (n_valid == 0) {
    empirical_values <- "(all missing)"
    is_range <- TRUE
  } else if (is.factor(x)) {
    empirical_values <- levels(x)
    is_range <- FALSE
  } else if (is.logical(x)) {
    empirical_values <- as.character(sort(unique(x[!is.na(x)])))
    is_range <- FALSE
  } else if (is.character(x)) {
    uvals <- sort(unique(x[!is.na(x)]))
    if (length(uvals) > max_values) {
      empirical_values <- c(uvals[seq_len(max_values)],
                            paste0("... (", length(uvals) - max_values, " more)"))
    } else {
      empirical_values <- uvals
    }
    is_range <- FALSE
  } else if (is.numeric(x) || inherits(x, "haven_labelled")) {
    uvals <- sort(unique(x[!is.na(x)]))
    if (length(uvals) <= max_values) {
      empirical_values <- as.character(uvals)
      is_range <- FALSE
    } else {
      empirical_values <- paste(min(uvals), "-", max(uvals))
      is_range <- TRUE
    }
  } else {
    uvals <- sort(unique(as.character(x[!is.na(x)])))
    if (length(uvals) > max_values) {
      empirical_values <- paste(length(uvals), "unique values")
    } else {
      empirical_values <- uvals
    }
    is_range <- length(uvals) > max_values
  }

  # Remove NA-keyed labels from raw_labels — tagged NA labels are displayed

  # separately in the NA section, so they must not count toward truncation
  if (!is.null(raw_labels) && !is.null(attr(x, "na_tag_map"))) {
    na_keys <- names(raw_labels) == "NA" | is.na(names(raw_labels))
    raw_labels <- raw_labels[!na_keys]
  }

  # Truncate value labels if too many
  truncated <- FALSE
  n_total_labels <- if (!is.null(raw_labels)) length(raw_labels) else 0L
  if (!is.null(raw_labels) && length(raw_labels) > max_values) {
    raw_labels <- raw_labels[seq_len(max_values)]
    truncated <- TRUE
  }

  # Extract tagged NA metadata (from read_spss, read_stata, read_sas, etc.)
  na_values <- character(0)
  na_labels <- character(0)
  n_system_na <- 0L

  tag_map <- attr(x, "na_tag_map")
  if (!is.null(tag_map) && requireNamespace("haven", quietly = TRUE)) {
    # Missing value codes sorted ascending
    na_values <- as.character(sort(tag_map))

    # Build label lookup: missing value code -> label name
    labels_attr <- attr(x, "labels")
    if (!is.null(labels_attr)) {
      na_labels_attr <- labels_attr[is.na(labels_attr)]
      na_label_map <- character(0)
      for (j in seq_along(na_labels_attr)) {
        tag_char <- haven::na_tag(na_labels_attr[j])
        if (!is.na(tag_char) && tag_char %in% names(tag_map)) {
          code <- as.character(tag_map[tag_char])
          na_label_map[code] <- names(na_labels_attr)[j]
        }
      }
      na_labels <- na_label_map
    }

    # Count system NAs (untagged)
    na_mask <- is.na(x)
    if (any(na_mask)) {
      na_tags <- vapply(x[na_mask], function(v) {
        tg <- haven::na_tag(v)
        if (is.na(tg)) NA_character_ else tg
      }, character(1))
      n_system_na <- sum(is.na(na_tags))
    }
  }

  list(
    position         = position,
    name             = var_name,
    label            = var_label,
    type             = type_info$type,
    type_short       = type_info$type_short,
    n                = n_total,
    n_valid          = n_valid,
    n_missing        = n_missing,
    n_unique         = n_unique,
    empirical_values = empirical_values,
    is_range         = is_range,
    value_labels     = raw_labels,
    n_labels         = n_total_labels,
    truncated        = truncated,
    na_values        = na_values,
    na_labels        = na_labels,
    n_system_na      = n_system_na
  )
}


# =============================================================================
# Main Function
# =============================================================================

#' Create a Codebook for Your Data
#'
#' @description
#' `codebook()` creates an interactive HTML data dictionary that opens in the
#' RStudio Viewer. It shows you everything about your dataset at a glance:
#' variable names, types, labels, the empirical values found in the data,
#' value labels, and frequency counts.
#'
#' Think of it as a professional "cheat sheet" for your dataset -- especially
#' useful when working with labelled survey data imported from SPSS, Stata,
#' or SAS.
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... Optional: specific variables to include. If empty, all variables
#'   are shown. Supports tidyselect helpers like `starts_with("trust")`.
#' @param weights Optional survey weights for weighted frequency calculations
#' @param show.id Show variable position number? (Default: TRUE)
#' @param show.type Show data type? (Default: TRUE)
#' @param show.labels Show variable labels? (Default: TRUE)
#' @param show.values Show empirical values and value labels? (Default: TRUE)
#' @param show.freq Show frequency counts? (Default: TRUE)
#' @param show.na Show tagged missing value types in the codebook? (Default:
#'   TRUE). When data was imported with [read_spss()], [read_stata()],
#'   [read_sas()], or [read_xpt()] using tagged NAs, they are displayed
#'   with their original missing value codes, labels, and frequencies below
#'   the valid values, separated by a thin gray line.
#' @param show.unused Show all defined value labels, even those with zero
#'   observations? (Default: FALSE). Useful for seeing the full codebook
#'   including response options that no respondent selected.
#' @param max.values Maximum number of values to display per variable
#'   before truncating or showing a range (Default: 10)
#' @param max.len Maximum character width for labels before truncation
#'   (Default: 50)
#' @param sort.by.name Sort variables alphabetically instead of by position?
#'   (Default: FALSE)
#' @param file Path to save the HTML codebook. If NULL (default), a temporary
#'   file is used and the codebook opens in the Viewer.
#'
#' @return Invisibly returns a list of class `"codebook"` containing:
#'   \describe{
#'     \item{codebook}{Tibble with one row per variable and all metadata}
#'     \item{data_info}{List with dataset-level information (name, nrow, ncol, etc.)}
#'     \item{html}{The generated HTML as an htmltools object}
#'     \item{weights}{Name of the weight variable, or NULL}
#'     \item{options}{List of all display options}
#'     \item{frequencies}{Named list of frequency tables per variable}
#'   }
#'
#' @details
#' ## What the Codebook Shows
#'
#' For each variable, the codebook displays (depending on options):
#' - **ID**: Column position in the dataset
#' - **Name**: Variable name (in monospace font)
#' - **Type**: Data type (numeric, factor, ordered factor, haven_labelled, etc.)
#' - **Label**: Variable label (from SPSS/Stata/SAS imports or manual assignment)
#' - **Values**: The empirical values found in the data
#' - **Value Labels**: Labels assigned to those values (if any)
#' - **Freq.**: Frequency count for each value
#'
#' ## When to Use This
#'
#' Use `codebook()` when you:
#' - First receive a new dataset and want to understand its structure
#' - Work with labelled data (SPSS, Stata, SAS) and need to see all value labels
#' - Want to document your dataset for colleagues or publications
#' - Need to quickly see value distributions across variables
#'
#' @examples
#' # View the full codebook (opens in RStudio Viewer)
#' data(survey_data)
#' codebook(survey_data)
#'
#' # Only trust-related variables
#' codebook(survey_data, starts_with("trust"))
#'
#' # Save to file for sharing
#' codebook(survey_data, file = tempfile(fileext = ".html"))
#'
#' # Compact console overview (without opening Viewer)
#' cb <- codebook(survey_data)
#' print(cb)
#'
#' # Detailed console output
#' summary(cb)
#'
#' @seealso [describe()] for detailed numeric summaries, [frequency()] for
#'   detailed frequency tables, [read_spss()], [read_por()], [read_stata()],
#'   [read_sas()], [read_xpt()] for importing data with tagged NAs
#'
#' @export
codebook <- function(data, ..., weights = NULL,
                     show.id = TRUE, show.type = TRUE, show.labels = TRUE,
                     show.values = TRUE, show.freq = TRUE,
                     show.na = TRUE, show.unused = FALSE,
                     max.values = 10, max.len = 50,
                     sort.by.name = FALSE,
                     file = NULL) {

  # Input validation
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }

  # Capture data name before any modifications
  data_name <- deparse(substitute(data))
  if (nchar(data_name) > 50) data_name <- "data"

  # Handle grouped data -- ungroup but record group vars
  grp_vars <- if (inherits(data, "grouped_df")) dplyr::group_vars(data) else NULL
  data <- dplyr::ungroup(data)

  # Process weights
  weights_quo <- rlang::enquo(weights)
  weights_info <- .process_weights(data, weights_quo)
  w_name <- weights_info$name

  # Select variables (all if ... is empty)
  dots <- rlang::enquos(...)
  if (length(dots) == 0) {
    var_names <- names(data)
  } else {
    vars <- tidyselect::eval_select(rlang::expr(c(...)), data)
    var_names <- names(vars)
  }

  # Optionally sort alphabetically
  if (sort.by.name) var_names <- sort(var_names)

  # Extract metadata for each variable
  metadata_list <- lapply(seq_along(var_names), function(i) {
    vn <- var_names[i]
    pos <- match(vn, names(data))
    .extract_var_metadata(data[[vn]], vn, pos, max.values)
  })

  # Combine into tibble
  codebook_df <- tibble::tibble(
    position         = vapply(metadata_list, `[[`, integer(1), "position"),
    name             = vapply(metadata_list, `[[`, character(1), "name"),
    label            = vapply(metadata_list, `[[`, character(1), "label"),
    type             = vapply(metadata_list, `[[`, character(1), "type"),
    type_short       = vapply(metadata_list, `[[`, character(1), "type_short"),
    n                = vapply(metadata_list, `[[`, integer(1), "n"),
    n_valid          = vapply(metadata_list, `[[`, integer(1), "n_valid"),
    n_missing        = vapply(metadata_list, `[[`, integer(1), "n_missing"),
    n_unique         = vapply(metadata_list, `[[`, integer(1), "n_unique"),
    empirical_values = lapply(metadata_list, `[[`, "empirical_values"),
    is_range         = vapply(metadata_list, `[[`, logical(1), "is_range"),
    value_labels     = lapply(metadata_list, `[[`, "value_labels"),
    n_labels         = vapply(metadata_list, `[[`, integer(1), "n_labels"),
    truncated        = vapply(metadata_list, `[[`, logical(1), "truncated"),
    na_values        = lapply(metadata_list, `[[`, "na_values"),
    na_labels        = lapply(metadata_list, `[[`, "na_labels"),
    n_system_na      = vapply(metadata_list, `[[`, integer(1), "n_system_na")
  )

  # Compute frequencies for all variables (for the Freq. column)
  freq_list <- list()
  w_vec <- weights_info$vector
  for (vn in var_names) {
    x <- data[[vn]]
    n_unique_var <- codebook_df$n_unique[codebook_df$name == vn]
    is_categorical <- is.factor(x) || is.character(x) || is.logical(x) ||
      !is.null(attr(x, "labels"))
    has_few_values <- n_unique_var <= max.values
    has_tagged <- !is.null(attr(x, "na_tag_map"))
    if (is_categorical || has_few_values) {
      freq_list[[vn]] <- calculate_single_frequency(
        x, w_vec,
        sort.frq = "none",
        show.na = (show.na && has_tagged),
        show.unused = show.unused
      )
    }
  }

  # Build data_info summary
  data_info <- list(
    name          = data_name,
    nrow          = nrow(data),
    ncol          = ncol(data),
    n_selected    = length(var_names),
    n_labelled    = sum(!is.na(codebook_df$label)),
    n_factors     = sum(vapply(var_names, function(v) is.factor(data[[v]]), logical(1))),
    n_numeric     = sum(vapply(var_names, function(v) is.numeric(data[[v]]), logical(1))),
    n_character   = sum(vapply(var_names, function(v) is.character(data[[v]]), logical(1))),
    n_missing_any = sum(codebook_df$n_missing > 0),
    groups        = grp_vars
  )

  # Store options
  opts <- list(
    show.id = show.id, show.type = show.type, show.labels = show.labels,
    show.values = show.values, show.freq = show.freq,
    show.na = show.na, show.unused = show.unused,
    max.values = max.values, max.len = max.len
  )

  # Build result
  result <- structure(list(
    codebook    = codebook_df,
    data_info   = data_info,
    variables   = var_names,
    weights     = w_name,
    options     = opts,
    frequencies = freq_list,
    html        = NULL
  ), class = "codebook")

  # Generate HTML
  result$html <- .codebook_to_html(result)

  # Display in Viewer
  if (interactive()) {
    html_file <- if (!is.null(file)) file else tempfile(fileext = ".html")
    htmltools::save_html(result$html, file = html_file)
    viewer <- getOption("viewer", utils::browseURL)
    viewer(html_file)
  } else if (!is.null(file)) {
    htmltools::save_html(result$html, file = file)
  }


  invisible(result)
}


# =============================================================================
# HTML Builder
# =============================================================================

#' Build HTML codebook from result object
#' @param result A codebook result list
#' @return An htmltools tag object
#' @keywords internal
.codebook_to_html <- function(result) {
  cb <- result$codebook
  info <- result$data_info
  opts <- result$options
  freq_list <- result$frequencies

  # CSS — Subtle-Accent Design
  css <- "
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto,
                   'Helvetica Neue', Arial, sans-serif;
      margin: 24px;
      color: #1a1a1a;
      background: #fff;
      font-size: 13px;
      line-height: 1.5;
      -webkit-font-smoothing: antialiased;
    }
    h2 {
      margin: 0 0 2px 0;
      color: #1a1a1a;
      font-size: 16px;
      font-weight: 600;
      letter-spacing: -0.01em;
    }
    .subtitle {
      color: #888;
      margin: 0 0 16px 0;
      font-size: 12px;
      letter-spacing: 0.01em;
    }
    table {
      border-collapse: collapse;
      width: 100%;
      font-size: 13px;
    }
    thead th {
      background: #3b3f51;
      color: #fff;
      padding: 9px 12px;
      text-align: left;
      font-weight: 500;
      font-size: 11px;
      text-transform: uppercase;
      letter-spacing: 0.05em;
      position: sticky;
      top: 0;
      z-index: 1;
    }
    tbody td {
      padding: 7px 12px;
      border-bottom: 1px solid #eee;
      vertical-align: top;
      color: #1a1a1a;
    }
    tbody tr:nth-child(even) { background: #f9f9fb; }
    tbody tr:hover { background: #f0f0f5; }
    .var-name {
      font-family: 'SFMono-Regular', Consolas, 'Liberation Mono',
                   Menlo, monospace;
      font-size: 12.5px;
      font-weight: 500;
      white-space: nowrap;
      color: #1a1a1a;
    }
    .var-label { color: #666; }
    .type-badge {
      display: inline-block;
      padding: 1px 7px;
      border-radius: 4px;
      font-size: 11px;
      font-family: 'SFMono-Regular', Consolas, 'Liberation Mono',
                   Menlo, monospace;
      font-weight: 500;
      letter-spacing: 0.01em;
    }
    .type-num, .type-fct, .type-chr, .type-lbl,
    .type-lgl, .type-date, .type-other {
      background: #ededf0;
      color: #555;
    }
    .values-cell {
      font-size: 12px;
      font-family: 'SFMono-Regular', Consolas, 'Liberation Mono',
                   Menlo, monospace;
      white-space: nowrap;
      color: #444;
    }
    .labels-cell { font-size: 12px; color: #666; white-space: nowrap; }
    .values-cell > div, .labels-cell > div, .freq-cell > div {
      min-height: 1.2em;
    }
    .freq-cell {
      font-size: 12px;
      font-family: 'SFMono-Regular', Consolas, 'Liberation Mono',
                   Menlo, monospace;
      text-align: right;
      color: #444;
    }
    .range-text { }
    .num-col { text-align: right; color: #888; }
    .truncated-note { color: #aaa; font-style: italic; font-size: 11px; }
    .na-separator {
      border-top: 1px solid #ddd;
      margin-top: 3px;
      padding-top: 3px;
      height: 1px;
    }
    .na-value { color: #999; font-style: italic; }
    .footer {
      margin-top: 16px;
      font-size: 11px;
      color: #bbb;
    }
  "

  # Header columns: ID | Name | Type | Label | Values | Value Labels | Freq.
  header_cells <- list()
  if (opts$show.id) header_cells <- c(header_cells, list(htmltools::tags$th("ID")))
  header_cells <- c(header_cells, list(htmltools::tags$th("Name")))
  if (opts$show.type) header_cells <- c(header_cells, list(htmltools::tags$th("Type")))
  if (opts$show.labels) header_cells <- c(header_cells, list(htmltools::tags$th("Label")))
  if (opts$show.values) {
    header_cells <- c(header_cells, list(
      htmltools::tags$th("Values"),
      htmltools::tags$th("Value Labels")
    ))
  }
  if (opts$show.freq) header_cells <- c(header_cells, list(htmltools::tags$th("Freq.")))

  # Build rows
  rows <- lapply(seq_len(nrow(cb)), function(i) {
    cells <- list()

    # ID
    if (opts$show.id) {
      cells <- c(cells, list(htmltools::tags$td(class = "num-col", cb$position[i])))
    }

    # Name
    cells <- c(cells, list(htmltools::tags$td(class = "var-name", cb$name[i])))

    # Type (with color badge)
    if (opts$show.type) {
      type_class <- if (grepl("^(dbl|int|num)", cb$type_short[i])) {
        "type-badge type-num"
      } else if (grepl("^(fct|ord)", cb$type_short[i])) {
        "type-badge type-fct"
      } else if (cb$type_short[i] == "chr") {
        "type-badge type-chr"
      } else if (grepl("^lbl", cb$type_short[i])) {
        "type-badge type-lbl"
      } else if (cb$type_short[i] == "lgl") {
        "type-badge type-lgl"
      } else if (grepl("date|time|dttm", cb$type_short[i])) {
        "type-badge type-date"
      } else {
        "type-badge type-other"
      }
      cells <- c(cells, list(
        htmltools::tags$td(htmltools::tags$span(class = type_class, cb$type_short[i]))
      ))
    }

    # Label
    if (opts$show.labels) {
      lbl <- if (!is.na(cb$label[i])) {
        .truncate_labels(cb$label[i], opts$max.len)
      } else {
        ""
      }
      cells <- c(cells, list(htmltools::tags$td(class = "var-label", lbl)))
    }

    # Values + Value Labels + Freq columns
    # Build all three column div-lists in parallel, then create <td> cells
    if (opts$show.values || opts$show.freq) {
      emp_vals <- cb$empirical_values[[i]]
      val_labels <- cb$value_labels[[i]]
      is_rng <- cb$is_range[i]

      # When show.unused is TRUE, add value label keys not yet in emp_vals
      if (opts$show.unused && !is_rng && !is.null(val_labels) &&
          length(val_labels) > 0) {
        label_keys <- names(val_labels)
        # Filter out NA-valued label keys (tagged NA labels map to "NA")
        label_keys <- label_keys[!is.na(label_keys) & label_keys != "NA"]
        missing_keys <- setdiff(label_keys, emp_vals)
        if (length(missing_keys) > 0) {
          # Sort numerically if possible, otherwise alphabetically
          num_keys <- suppressWarnings(as.numeric(missing_keys))
          if (!all(is.na(num_keys))) {
            missing_keys <- missing_keys[order(num_keys)]
          } else {
            missing_keys <- sort(missing_keys)
          }
          # Merge into emp_vals at correct sorted positions
          all_vals <- c(emp_vals, missing_keys)
          num_all <- suppressWarnings(as.numeric(all_vals))
          if (!all(is.na(num_all))) {
            all_vals <- all_vals[order(num_all)]
          }
          emp_vals <- all_vals
        }
      }
      freq_data <- freq_list[[cb$name[i]]]
      na_vals <- cb$na_values[[i]]
      na_lbls <- cb$na_labels[[i]]
      n_sys_na <- cb$n_system_na[i]

      if (is_rng) {
        # Range display (single line, e.g., "18 - 95")
        if (opts$show.values) {
          cells <- c(cells, list(
            htmltools::tags$td(class = "values-cell range-text", emp_vals),
            htmltools::tags$td("")  # empty Value Labels for range
          ))
        }
        if (opts$show.freq) {
          cells <- c(cells, list(htmltools::tags$td("")))
        }
      } else {
        # Per-value display — build parallel div lists
        val_divs <- lapply(emp_vals, function(v) htmltools::tags$div(v))

        lbl_divs <- lapply(emp_vals, function(v) {
          lbl_text <- if (!is.null(val_labels) && v %in% names(val_labels)) {
            .truncate_labels(unname(val_labels[v]), opts$max.len)
          } else {
            "\u00A0"
          }
          htmltools::tags$div(lbl_text)
        })

        freq_divs <- if (!is.null(freq_data)) {
          lapply(emp_vals, function(v) {
            freq_row <- freq_data[as.character(freq_data$value) == v, ]
            if (nrow(freq_row) > 0) {
              htmltools::tags$div(as.character(round(freq_row$freq[1])))
            } else {
              htmltools::tags$div("\u00A0")
            }
          })
        } else {
          lapply(emp_vals, function(v) htmltools::tags$div("\u00A0"))
        }

        # Truncation note for value labels
        if (!is.null(val_labels) && cb$truncated[i]) {
          trunc_note <- htmltools::tags$div(
            class = "truncated-note",
            paste0("... (", cb$n_labels[i] - opts$max.values, " more)")
          )
          val_divs <- c(val_divs, list(htmltools::tags$div("\u00A0")))
          lbl_divs <- c(lbl_divs, list(trunc_note))
          freq_divs <- c(freq_divs, list(htmltools::tags$div("\u00A0")))
        }

        # Append tagged NA rows (separator + per-NA-code rows + system NA)
        if (opts$show.na && length(na_vals) > 0) {
          # Thin separator
          sep_div <- htmltools::tags$div(class = "na-separator", "\u00A0")
          val_divs <- c(val_divs, list(sep_div))
          lbl_divs <- c(lbl_divs, list(sep_div))
          freq_divs <- c(freq_divs, list(sep_div))

          # One row per tagged NA code
          for (nav in na_vals) {
            val_divs <- c(val_divs, list(
              htmltools::tags$div(class = "na-value", nav)
            ))
            na_lbl_text <- if (nav %in% names(na_lbls)) {
              .truncate_labels(unname(na_lbls[nav]), opts$max.len)
            } else {
              ""
            }
            lbl_divs <- c(lbl_divs, list(
              htmltools::tags$div(class = "na-value", na_lbl_text)
            ))
            # Freq from expanded frequency table (match on na_display_value)
            if (!is.null(freq_data) && "na_display_value" %in% names(freq_data)) {
              na_freq_row <- freq_data[
                !is.na(freq_data$na_display_value) &
                  freq_data$na_display_value == nav, ]
              freq_text <- if (nrow(na_freq_row) > 0) {
                as.character(round(na_freq_row$freq[1]))
              } else {
                "0"
              }
            } else {
              freq_text <- ""
            }
            freq_divs <- c(freq_divs, list(
              htmltools::tags$div(class = "na-value", freq_text)
            ))
          }

          # System NA row if present
          if (n_sys_na > 0) {
            val_divs <- c(val_divs, list(
              htmltools::tags$div(class = "na-value", "NA")
            ))
            lbl_divs <- c(lbl_divs, list(
              htmltools::tags$div(class = "na-value", "(system missing)")
            ))
            # System NA freq from expanded table (display value is "NA")
            if (!is.null(freq_data) && "na_display_value" %in% names(freq_data)) {
              sys_freq_row <- freq_data[
                !is.na(freq_data$na_display_value) &
                  freq_data$na_display_value == "NA", ]
              sys_freq_text <- if (nrow(sys_freq_row) > 0) {
                as.character(round(sys_freq_row$freq[1]))
              } else {
                as.character(n_sys_na)
              }
            } else {
              sys_freq_text <- as.character(n_sys_na)
            }
            freq_divs <- c(freq_divs, list(
              htmltools::tags$div(class = "na-value", sys_freq_text)
            ))
          }
        }

        # Create the <td> cells
        if (opts$show.values) {
          cells <- c(cells, list(
            htmltools::tags$td(class = "values-cell", htmltools::tagList(val_divs))
          ))
          if (!is.null(val_labels) && length(val_labels) > 0) {
            cells <- c(cells, list(
              htmltools::tags$td(class = "labels-cell", htmltools::tagList(lbl_divs))
            ))
          } else if (opts$show.na && length(na_vals) > 0) {
            # Has NA labels but no valid value labels
            cells <- c(cells, list(
              htmltools::tags$td(class = "labels-cell", htmltools::tagList(lbl_divs))
            ))
          } else {
            cells <- c(cells, list(htmltools::tags$td("")))
          }
        }

        if (opts$show.freq) {
          if (!is.null(freq_data)) {
            cells <- c(cells, list(
              htmltools::tags$td(class = "freq-cell", htmltools::tagList(freq_divs))
            ))
          } else {
            cells <- c(cells, list(htmltools::tags$td("")))
          }
        }
      }
    }

    htmltools::tags$tr(cells)
  })

  # Subtitle
  subtitle_parts <- c(
    paste0(format(info$nrow, big.mark = ","), " observations"),
    paste0(info$n_selected, " variables")
  )
  if (info$n_labelled > 0) {
    subtitle_parts <- c(subtitle_parts, paste0(info$n_labelled, " labelled"))
  }
  if (info$n_missing_any > 0) {
    subtitle_parts <- c(subtitle_parts,
                        paste0(info$n_missing_any, " with missing data"))
  }
  if (!is.null(result$weights)) {
    subtitle_parts <- c(subtitle_parts, paste0("weighted by: ", result$weights))
  }

  # Build page
  htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$title(paste("Codebook:", info$name)),
      htmltools::tags$style(htmltools::HTML(css))
    ),
    htmltools::tags$body(
      htmltools::tags$h2(paste("Codebook:", info$name)),
      htmltools::tags$p(class = "subtitle", paste(subtitle_parts, collapse = " | ")),
      htmltools::tags$table(
        htmltools::tags$thead(htmltools::tags$tr(header_cells)),
        htmltools::tags$tbody(rows)
      ),
      htmltools::tags$p(
        class = "footer",
        paste0("Generated by mariposa ", utils::packageVersion("mariposa"),
               " on ", Sys.Date())
      )
    )
  )
}


# =============================================================================
# Print Method (compact console fallback)
# =============================================================================

#' Print a Compact Console Overview of the Codebook
#'
#' @description
#' Displays a concise one-line-per-variable table in the console, showing
#' position, name, type, and label. The full HTML codebook with values,
#' value labels, and frequencies is displayed in the RStudio Viewer.
#'
#' @param x A codebook object from [codebook()]
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the codebook object
#'
#' @examples
#' data(survey_data)
#' cb <- codebook(survey_data)
#' print(cb)
#'
#' @export
print.codebook <- function(x, ...) {
  info <- x$data_info
  cb <- x$codebook

  # Type summary
  types <- table(cb$type_short)
  type_parts <- vapply(names(types), function(t) {
    paste0(types[t], " ", t)
  }, character(1))

  cat(sprintf("\nCodebook: %s\n", info$name))
  cat(sprintf("%s variables | %s observations",
              info$n_selected,
              format(info$nrow, big.mark = ",")))
  if (info$n_labelled > 0) cat(sprintf(" | %s labelled", info$n_labelled))
  cat("\n")
  cat(sprintf("Types: %s\n", paste(type_parts, collapse = ", ")))
  cat("-- Open HTML viewer for full codebook with values and frequencies\n")

  invisible(x)
}


# =============================================================================
# Summary Method (verbose console output)
# =============================================================================

#' Detailed Console Summary of the Codebook
#'
#' @description
#' Creates a detailed summary object for console display, showing per-variable
#' information with toggleable sections. Use this when you want detailed
#' text output without opening the HTML Viewer.
#'
#' @param object A codebook object from [codebook()]
#' @param overview Show dataset-level overview? (Default: TRUE)
#' @param variable_details Show per-variable detail blocks? (Default: TRUE)
#' @param value_labels Show value labels within variable details? (Default: TRUE)
#' @param digits Number of decimal places for numeric output (Default: 3)
#' @param ... Additional arguments (ignored)
#'
#' @return A summary.codebook object (printed by [print.summary.codebook()])
#'
#' @examples
#' data(survey_data)
#' cb <- codebook(survey_data)
#' summary(cb)
#' summary(cb, value_labels = FALSE)
#'
#' @export
#' @method summary codebook
summary.codebook <- function(object, overview = TRUE, variable_details = TRUE,
                             value_labels = TRUE, digits = 3, ...) {
  show <- list(
    overview = overview,
    variable_details = variable_details,
    value_labels = value_labels
  )
  build_summary_object(object, show, digits, "summary.codebook")
}


#' Print a Detailed Console Codebook Summary
#'
#' @description
#' Renders the verbose codebook summary to the console with boolean-gated
#' sections for overview, variable details, and value labels.
#'
#' @param x A summary.codebook object from [summary.codebook()]
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the summary object
#'
#' @examples
#' data(survey_data)
#' cb <- codebook(survey_data)
#' print(summary(cb))
#'
#' @export
#' @method print summary.codebook
print.summary.codebook <- function(x, ...) {
  cb <- x$codebook
  info <- x$data_info
  opts <- x$options

  print_header("Codebook")

  # Overview section
  if (isTRUE(x$show$overview)) {
    cat(sprintf("  Dataset: %s\n", info$name))
    cat(sprintf("  Observations: %s\n", format(info$nrow, big.mark = ",")))
    cat(sprintf("  Variables: %d (%d numeric, %d factor, %d character)\n",
                info$n_selected, info$n_numeric, info$n_factors, info$n_character))
    cat(sprintf("  Variables with labels: %d\n", info$n_labelled))
    if (info$n_missing_any > 0) {
      cat(sprintf("  Variables with missing data: %d\n", info$n_missing_any))
    }
    if (!is.null(info$groups)) {
      cat(sprintf("  Grouped by: %s\n", paste(info$groups, collapse = ", ")))
    }
    if (!is.null(x$weights)) {
      cat(sprintf("  Weights: %s\n", x$weights))
    }
  }

  # Variable details
  if (isTRUE(x$show$variable_details)) {
    cat("\nVariable Details:\n")
    cat(paste(rep("-", 50), collapse = ""), "\n")

    for (i in seq_len(nrow(cb))) {
      cat(sprintf("\n[%d] %s (%s)\n", cb$position[i], cb$name[i], cb$type[i]))

      if (!is.na(cb$label[i])) {
        cat(sprintf("    Label: %s\n",
                    .truncate_labels(cb$label[i], opts$max.len)))
      }

      # Empirical values
      emp_vals <- cb$empirical_values[[i]]
      if (cb$is_range[i]) {
        cat(sprintf("    Values: %s\n", emp_vals))
      } else {
        vals_str <- paste(emp_vals, collapse = ", ")
        if (nchar(vals_str) > 70) {
          vals_str <- paste0(substr(vals_str, 1, 67), "...")
        }
        cat(sprintf("    Values: %s\n", vals_str))
      }

      # Value labels
      if (isTRUE(x$show$value_labels)) {
        val_labels <- cb$value_labels[[i]]
        if (!is.null(val_labels) && length(val_labels) > 0) {
          cat("    Value labels:\n")
          for (j in seq_along(val_labels)) {
            lbl <- .truncate_labels(unname(val_labels[j]), opts$max.len)
            cat(sprintf("      %s = %s\n", names(val_labels)[j], lbl))
          }
          if (cb$truncated[i]) {
            cat(sprintf("      ... (%d more)\n",
                        cb$n_labels[i] - opts$max.values))
          }
        }
      }
    }
  }

  invisible(x)
}
