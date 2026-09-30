#' Read SPSS Data with Tagged Missing Values
#'
#' @description
#' Reads an SPSS `.sav` file and preserves user-defined missing values as
#' tagged NAs instead of converting them to regular `NA`. This allows you to
#' distinguish between different types of missing data (e.g., "no answer",
#' "not applicable", "refused") while still treating them as `NA` in
#' standard R operations.
#'
#' @param path Path to an SPSS `.sav` file.
#' @param tag_na If `TRUE` (the default), user-defined missing values are
#'   converted to tagged NAs using [haven::tagged_na()]. If `FALSE`, the file
#'   is read with standard `haven::read_sav()` behavior (all missing values
#'   become regular `NA`).
#' @param encoding Character encoding for the file. If `NULL`, haven's default
#'   encoding detection is used.
#' @param verbose If `TRUE`, prints a message summarizing how many values were
#'   converted.
#'
#' @return A tibble with the SPSS data. When `tag_na = TRUE`:
#'   \itemize{
#'     \item User-defined missing values are stored as tagged NAs
#'     \item `is.na()` returns `TRUE` for these values (standard R behavior)
#'     \item The original SPSS missing codes can be recovered via
#'       [na_frequencies()], [untag_na()], or [haven::na_tag()]
#'     \item Each tagged variable has an `"na_tag_map"` attribute mapping
#'       tag characters to original SPSS codes, and an `"spss_missing"`
#'       attribute with the original missing-value definition (used by
#'       [write_spss()] for an exact round trip)
#'   }
#'   A file that is not an SPSS `.sav` file (e.g. a Stata or Excel file)
#'   is reported with its apparent type and the matching reader.
#'
#' @details
#' SPSS allows defining specific values as "user-defined missing values"
#' (e.g., -9 = "no answer", -8 = "don't know"). When reading `.sav` files
#' with `haven::read_sav()`, these are silently converted to `NA`, losing the
#' information about *why* a value is missing.
#'
#' `read_spss()` preserves this information using haven's tagged NA system:
#' each missing value type gets a unique tag character (a-z, A-Z, 0-9) that
#' can be inspected with [haven::na_tag()]. The values still behave as `NA`
#' in all standard R operations (`mean()`, `sum()`, `is.na()`, etc.).
#'
#' Use the companion functions to work with the tagged NAs:
#' \itemize{
#'   \item [na_frequencies()] - Frequency table of missing types
#'   \item [untag_na()] - Convert tagged NAs back to original SPSS codes
#'   \item [strip_tags()] - Convert tagged NAs to regular NAs (drop tags)
#' }
#'
#' @seealso [na_frequencies()], [untag_na()], [strip_tags()], [haven::read_sav()],
#'   [frequency()], [read_por()]
#'
#' @family data-import
#'
#' @examples
#' \donttest{
#' if (requireNamespace("haven", quietly = TRUE)) {
#'   # Roundtrip through a temporary .sav file (with your own data,
#'   # simply pass its path instead)
#'   tmp <- tempfile(fileext = ".sav")
#'   write_spss(survey_data, tmp)
#'   data <- read_spss(tmp)
#'
#'   # Standard R operations work normally (NAs are excluded)
#'   mean(data$life_satisfaction, na.rm = TRUE)
#'
#'   # frequency() shows each missing type separately
#'   data |> frequency(life_satisfaction)
#'
#'   unlink(tmp)
#' }
#' }
#'
#' @export
read_spss <- function(path, tag_na = TRUE, encoding = NULL, verbose = FALSE) {
  .check_required("path")
  .check_haven("SPSS import")
  .check_file_type(path, "sav", "read_spss")

  # Read with user_na = TRUE to preserve missing value metadata as attributes
  # (haven_labelled_spss class keeps values but marks them via na_range/na_values)
  data <- haven::read_sav(file = path, encoding = encoding, user_na = tag_na)

  if (!tag_na) return(data)

  .tag_spss_missing_values(data, verbose)
}


#' Read SPSS Portable Data with Tagged Missing Values
#'
#' @description
#' Reads an SPSS portable `.por` file and preserves user-defined missing values
#' as tagged NAs. This is the portable format equivalent of [read_spss()] for
#' `.sav` files.
#'
#' @param path Path to an SPSS `.por` file.
#' @param tag_na If `TRUE` (the default), user-defined missing values are
#'   converted to tagged NAs using [haven::tagged_na()]. If `FALSE`, the file
#'   is read with standard `haven::read_por()` behavior.
#' @param verbose If `TRUE`, prints a message summarizing how many values were
#'   converted.
#'
#' @return A tibble with the SPSS data. See [read_spss()] for details on
#'   tagged NA handling.
#'
#' @details
#' The SPSS portable format (`.por`) is an older, platform-independent format.
#' Unlike `.sav` files, the portable format does not support specifying a
#' character encoding. Tagged NA handling is identical to [read_spss()].
#'
#' @seealso [read_spss()], [na_frequencies()], [untag_na()], [strip_tags()],
#'   [haven::read_por()]
#'
#' @family data-import
#'
#' @examples
#' \donttest{
#' # .por files cannot be produced from R, so this only runs when one
#' # is present in the working directory
#' if (requireNamespace("haven", quietly = TRUE) &&
#'     file.exists("survey.por")) {
#'   data <- read_por("survey.por")
#'   na_frequencies(data$satisfaction)
#' }
#' }
#'
#' @export
read_por <- function(path, tag_na = TRUE, verbose = FALSE) {
  .check_required("path")
  .check_haven("SPSS portable import")
  .check_file_type(path, "por", "read_por")

  data <- haven::read_por(file = path, user_na = tag_na)

  if (!tag_na) return(data)

  .tag_spss_missing_values(data, verbose)
}


# ---- Internal Helpers -------------------------------------------------------

#' Guess a data file's format from its first bytes
#'
#' @return One of "sav", "por", "dta", "sas7bdat", "xpt", "xlsx", "zip",
#'   "gzip", "text", or NA when unknown
#' @noRd
.sniff_file_type <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  b <- readBin(con, "raw", n = 512L)
  if (length(b) == 0L) return(NA_character_)
  txt <- rawToChar(b[b != as.raw(0)])
  has <- function(s) grepl(s, txt, fixed = TRUE, useBytes = TRUE)
  starts <- function(s) {
    n <- nchar(s, type = "bytes")
    length(b) >= n && identical(b[seq_len(n)], charToRaw(s))
  }
  sas_magic <- as.raw(c(0xc2, 0xea, 0x81, 0x60, 0xb3, 0x14, 0x11, 0xcf,
                        0xbd, 0x92, 0x08, 0x00, 0x09, 0xc7, 0x31, 0x8c))
  if (starts("$FL2") || starts("$FL3")) return("sav")
  if (starts("<stata_dta>")) return("dta")
  if (length(b) >= 2L && as.integer(b[1]) %in% 102:115 &&
      as.integer(b[2]) %in% 1:2) return("dta")
  if (length(b) >= 28L && identical(b[13:28], sas_magic)) return("sas7bdat")
  if (starts("HEADER RECORD")) return("xpt")
  if (has("SPSS PORT FILE")) return("por")
  if (starts("PK\003\004")) {
    return(if (has("[Content_Types].xml") || has("xl/")) "xlsx" else "zip")
  }
  if (length(b) >= 2L && identical(b[1:2], as.raw(c(0x1f, 0x8b)))) {
    return("gzip")
  }
  if (all(as.integer(b) %in% c(9L, 10L, 13L, 32:126, 128:255))) return("text")
  NA_character_
}


#' Stop early, and say why, when a reader gets the wrong kind of file
#'
#' readstat's own errors for a mismatched format are cryptic ("Unable to
#' convert string to the requested encoding", "This version of the file
#' format is not supported"). Unknown formats are left to haven.
#'
#' @param path File path
#' @param expected Accepted types (see .sniff_file_type())
#' @param fn Name of the calling reader, for the message
#' @noRd
.check_file_type <- function(path, expected, fn, call = rlang::caller_env()) {
  if (!is.character(path) || length(path) != 1L) return(invisible(TRUE))
  if (grepl("^(https?|ftp)://", path)) return(invisible(TRUE))
  if (!file.exists(path)) {
    cli::cli_abort("File {.file {path}} does not exist.", call = call)
  }
  found <- .sniff_file_type(path)
  if (is.na(found) || found %in% expected) return(invisible(TRUE))

  what <- c(sav = "an SPSS data file (.sav)", por = "an SPSS portable file (.por)",
            dta = "a Stata file (.dta)", sas7bdat = "a SAS data file (.sas7bdat)",
            xpt = "a SAS transport file (.xpt)", xlsx = "an Excel workbook (.xlsx)",
            zip = "a zip archive", gzip = "a gzip-compressed file (e.g. .rds)",
            text = "a text file (e.g. .csv)")
  reader <- c(sav = "read_spss", por = "read_por", dta = "read_stata",
              sas7bdat = "read_sas", xpt = "read_xpt", xlsx = "read_xlsx")
  hint <- if (found %in% names(reader)) {
    paste0("Use {.fn ", reader[[found]], "} instead.")
  } else if (found == "text") {
    "Read it with e.g. {.fn utils::read.csv} or {.fn readr::read_csv}."
  } else if (found == "gzip") {
    "An R data file is read with {.fn readRDS}."
  } else {
    NULL
  }
  cli::cli_abort(c(
    "{.fn {fn}} cannot read {.file {basename(path)}}: it looks like {what[[found]]}.",
    if (!is.null(hint)) c("i" = hint)
  ), call = call)
}


#' Convert SPSS user-defined missing values to tagged NAs
#'
#' Shared internal helper for read_spss() and read_por().
#' Iterates over numeric columns, replaces values marked as missing
#' (via na_values/na_range attributes) with haven::tagged_na(),
#' and stores the mapping in the na_tag_map attribute.
#'
#' @param data A tibble read with haven::read_sav() or haven::read_por()
#'   with user_na = TRUE.
#' @param verbose If TRUE, print a summary message.
#' @return The modified tibble with tagged NAs.
#' @noRd
.tag_spss_missing_values <- function(data, verbose) {
  n_converted <- 0L
  n_vars_converted <- 0L

  for (i in seq_len(ncol(data))) {
    x <- data[[i]]
    na_values <- attr(x, "na_values", exact = TRUE)
    na_range  <- attr(x, "na_range", exact = TRUE)
    labels    <- attr(x, "labels", exact = TRUE)

    if (is.null(na_values) && is.null(na_range)) next
    if (!is.numeric(x)) next

    # Access raw underlying values - bypass haven_labelled_spss custom is.na()
    raw <- as.double(x)

    # Collect all actual values that should become missing
    missing_vals <- numeric(0)

    if (!is.null(na_values)) {
      missing_vals <- c(missing_vals, na_values)
    }

    if (!is.null(na_range)) {
      range_lo <- na_range[1]
      range_hi <- na_range[2]
      vals_in_range <- unique(raw[!is.na(raw) & raw >= range_lo & raw <= range_hi])
      missing_vals <- c(missing_vals, vals_in_range)
    }

    missing_vals <- sort(unique(missing_vals))
    if (length(missing_vals) == 0L) next

    # Assign tag characters: a-z, A-Z, 0-9 (62 possible tags per variable)
    tag_pool <- c(letters, LETTERS, as.character(0:9))
    if (length(missing_vals) > length(tag_pool)) {
      cli::cli_warn(
        "Variable {.var {names(data)[i]}} has {length(missing_vals)} missing value codes; only the first {length(tag_pool)} will be tagged."
      )
      missing_vals <- missing_vals[seq_len(length(tag_pool))]
    }
    tag_chars <- tag_pool[seq_along(missing_vals)]

    # Create tagged NAs
    tagged_nas <- haven::tagged_na(tag_chars)
    names(tagged_nas) <- as.character(missing_vals)

    # Replace matching values with tagged NAs
    for (j in seq_along(missing_vals)) {
      mask <- !is.na(raw) & raw == missing_vals[j]
      n_matches <- sum(mask)
      if (n_matches > 0L) {
        raw[mask] <- tagged_nas[j]
        n_converted <- n_converted + n_matches
      }
    }

    # Update value labels: keep valid labels, replace missing-value labels
    # with their tagged NA equivalents
    if (!is.null(labels)) {
      is_missing_label <- labels %in% missing_vals
      valid_labels   <- labels[!is_missing_label]
      missing_labels <- labels[is_missing_label]

      new_missing_labels <- numeric(0)
      for (ml in seq_along(missing_labels)) {
        val <- unname(missing_labels[ml])
        idx <- match(val, missing_vals)
        if (!is.na(idx)) {
          tna <- tagged_nas[idx]
          names(tna) <- names(missing_labels)[ml]
          new_missing_labels <- c(new_missing_labels, tna)
        }
      }

      labels <- c(valid_labels, new_missing_labels)
    }

    # Build new haven_labelled vector (regular, not _spss subclass)
    new_x <- haven::labelled(
      raw,
      labels = labels,
      label = attr(x, "label", exact = TRUE)
    )

    # Store the tag-to-code mapping for recovery
    attr(new_x, "na_tag_map") <- stats::setNames(missing_vals, tag_chars)
    attr(new_x, "na_tag_format") <- "spss"
    # ... and the original SPSS definition (e.g. LOWEST THRU -1), so that
    # write_spss() can restore it exactly instead of rebuilding it from the
    # codes that happen to occur
    attr(new_x, "spss_missing") <- list(na_values = na_values,
                                        na_range = na_range)

    data[[i]] <- new_x
    n_vars_converted <- n_vars_converted + 1L
  }

  if (verbose) {
    cli::cli_inform(
      "Converted {format(n_converted, big.mark = ',')} values in {n_vars_converted} variable{?s} to tagged NAs."
    )
  }

  data
}


#' Tag user-specified values as missing (for Stata/SAS files without native tags)
#'
#' When Stata or SAS files contain missing value codes as regular numeric values
#' (e.g., -9, -42), this helper converts them to tagged NAs.
#' Reuses the same logic as .tag_spss_missing_values() but with
#' user-supplied missing values instead of attribute-derived ones.
#'
#' @param data A tibble read with haven::read_dta(), read_sas(), or read_xpt().
#' @param missing_values Numeric vector of values to treat as missing.
#' @param format Character string: "stata" or "sas".
#' @param verbose If TRUE, print a summary message.
#' @return The modified tibble with tagged NAs.
#' @noRd
.tag_user_missing_values <- function(data, missing_values, format, verbose) {
  # Guard for the set_na() unnamed-data-frame path, which reaches this
  # function without a prior haven check (the read_* callers guarantee
  # haven before getting here, so this is a no-op for them).
  .check_haven("tagged NAs")

  missing_values <- sort(unique(missing_values))
  if (length(missing_values) == 0L) return(data)

  # Assign tag characters: a-z, A-Z, 0-9 (62 possible tags)
  tag_pool <- c(letters, LETTERS, as.character(0:9))
  if (length(missing_values) > length(tag_pool)) {
    cli::cli_warn(
      "{.arg tag_na} has {length(missing_values)} values; only the first {length(tag_pool)} will be tagged."
    )
    missing_values <- missing_values[seq_len(length(tag_pool))]
  }
  tag_chars <- tag_pool[seq_along(missing_values)]

  # Pre-compute tagged NAs once (same for all variables)
  tagged_nas <- haven::tagged_na(tag_chars)
  names(tagged_nas) <- as.character(missing_values)

  n_converted <- 0L
  n_vars_converted <- 0L

  for (i in seq_len(ncol(data))) {
    x <- data[[i]]
    if (!is.numeric(x)) next

    # Skip columns that already have na_tag_map (native tagged NAs)
    if (!is.null(attr(x, "na_tag_map"))) next

    raw <- as.double(x)
    labels <- attr(x, "labels", exact = TRUE)

    # Find which missing values actually occur in this column
    present_idx <- which(vapply(
      missing_values,
      function(mv) any(!is.na(raw) & raw == mv),
      logical(1)
    ))
    if (length(present_idx) == 0L) next

    # Replace matching values with tagged NAs
    for (j in present_idx) {
      mask <- !is.na(raw) & raw == missing_values[j]
      n_matches <- sum(mask)
      if (n_matches > 0L) {
        raw[mask] <- tagged_nas[j]
        n_converted <- n_converted + n_matches
      }
    }

    # Update value labels: replace missing-value labels with tagged NA equivalents
    if (!is.null(labels)) {
      is_missing_label <- labels %in% missing_values
      valid_labels   <- labels[!is_missing_label]
      missing_labels <- labels[is_missing_label]

      new_missing_labels <- numeric(0)
      for (ml in seq_along(missing_labels)) {
        val <- unname(missing_labels[ml])
        idx <- match(val, missing_values)
        if (!is.na(idx)) {
          tna <- tagged_nas[idx]
          names(tna) <- names(missing_labels)[ml]
          new_missing_labels <- c(new_missing_labels, tna)
        }
      }

      labels <- c(valid_labels, new_missing_labels)
    }

    # Rebuild as haven_labelled vector
    new_x <- haven::labelled(
      raw,
      labels = labels,
      label = attr(x, "label", exact = TRUE)
    )

    # Store tag-to-code mapping (numeric, like SPSS) — only for present values
    attr(new_x, "na_tag_map") <- stats::setNames(
      missing_values[present_idx], tag_chars[present_idx]
    )
    attr(new_x, "na_tag_format") <- format

    data[[i]] <- new_x
    n_vars_converted <- n_vars_converted + 1L
  }

  if (verbose && n_vars_converted > 0L) {
    cli::cli_inform(
      "Converted {format(n_converted, big.mark = ',')} values in {n_vars_converted} variable{?s} to tagged NAs."
    )
  }

  data
}


#' Detect native extended missing values across all columns
#'
#' Shared step-1 of read_stata()/read_sas()/read_xpt(): scans every numeric
#' column for native tagged missing values (.a-.z / .A-.Z) and attaches the
#' na_tag_map attribute where found. Previously copy-pasted in three files.
#'
#' @param data Imported data frame
#' @param format "stata" or "sas"
#' @param verbose Announce the number of tagged variables?
#' @return The data frame with na_tag_map attributes attached
#' @noRd
.detect_native_tags <- function(data, format, verbose = TRUE) {
  n_vars_tagged <- 0L

  for (i in seq_len(ncol(data))) {
    x <- data[[i]]
    if (!is.numeric(x)) next

    result <- .build_na_tag_map_from_native(x, format)

    if (!is.null(attr(result, "na_tag_map"))) {
      data[[i]] <- result
      n_vars_tagged <- n_vars_tagged + 1L
    }
  }

  if (verbose && n_vars_tagged > 0L) {
    cli::cli_inform(
      "Found native tagged missing values in {n_vars_tagged} variable{?s}."
    )
  }

  data
}

#' Build na_tag_map from native tagged NAs (Stata/SAS)
#'
#' For formats that have native tagged NAs (Stata .a-.z, SAS .A-.Z/._),
#' haven already returns tagged NA values. This helper scans a column,
#' discovers the tag characters present, and attaches the na_tag_map
#' and na_tag_format attributes so that na_frequencies(), frequency(),
#' and codebook() can work with them.
#'
#' @param x A numeric vector potentially containing native tagged NAs.
#' @param format Either "stata" or "sas".
#' @return The vector with na_tag_map and na_tag_format attributes set,
#'   or unmodified if no tagged NAs are found.
#' @noRd
.build_na_tag_map_from_native <- function(x, format) {
  if (!is.numeric(x)) return(x)

  na_mask <- is.na(x)
  if (!any(na_mask)) return(x)

  tags <- .na_tags(x[na_mask])
  unique_tags <- sort(unique(tags[!is.na(tags)]))

  if (length(unique_tags) == 0L) return(x)

  # Build display codes: ".a", ".b" for Stata; ".A", ".B", "._" for SAS
  native_codes <- paste0(".", unique_tags)

  attr(x, "na_tag_map") <- stats::setNames(native_codes, unique_tags)
  attr(x, "na_tag_format") <- format
  x
}


# ---- Universal Tagged NA Helpers --------------------------------------------

#' Frequency Table of Missing Value Types
#'
#' @description
#' Shows a breakdown of the different types of missing values in a variable
#' that was read with [read_spss()], [read_stata()], [read_sas()], or
#' [read_xpt()] and contains tagged NAs.
#'
#' @param x A numeric vector with tagged NAs, or a data frame.
#' @param ... For a data frame: the variables to tabulate (tidyselect). If
#'   empty, every numeric variable with missing values is tabulated.
#'
#' @return A data frame with one row per missing type, ordered by code like
#'   the missing block of [frequency()] (system missing last, listed only
#'   when it occurs):
#'   \item{code}{The original missing value code: numeric for SPSS codes
#'     (e.g., -9, -8), character for native format codes (e.g., ".a" for
#'     Stata, ".A" for SAS)}
#'   \item{label}{The value label for this missing type (if available)}
#'   \item{n}{Number of cases with this missing type}
#'   \item{prc}{Percent of all cases}
#'   \item{tag}{The internal tag character of the tagged NA}
#'   For a data frame, a first column `variable` names the variable. Without
#'   any missing values the (empty) result is returned invisibly with a
#'   message.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("haven", quietly = TRUE)) {
#'   # Declare -9/-8 as distinct tagged missing types, then inspect them
#'   x <- set_na(c(1, 2, -9, 3, -8, -9), -9, -8)
#'   na_frequencies(x)
#'   #   code label n      prc tag
#'   # 1   -9  <NA> 2 33.33333   a
#'   # 2   -8  <NA> 1 16.66667   b
#' }
#' }
#'
#' @seealso [read_spss()], [read_stata()], [read_sas()], [read_xpt()],
#'   [untag_na()], [strip_tags()]
#' @family data-import
#' @export
na_frequencies <- function(x, ...) {
  .check_required("x")
  .check_haven("tagged NA inspection")

  if (is.data.frame(x)) {
    dots <- rlang::enexprs(...)
    .check_dot_names(names(dots))
    cols <- if (length(dots) == 0L) {
      which(vapply(x, function(v) {
        is.numeric(v) && !is.factor(v) && anyNA(v)
      }, logical(1)))
    } else {
      tidyselect::eval_select(rlang::expr(c(...)), x)
    }
    tables <- lapply(cols, function(i) {
      tbl <- .na_frequency_table(x[[i]], names(x)[i])
      if (nrow(tbl) == 0L) return(NULL)
      cbind(variable = names(x)[i], tbl, stringsAsFactors = FALSE)
    })
    tables <- tables[!vapply(tables, is.null, logical(1))]
    if (length(tables) == 0L) {
      cli::cli_inform("No missing values in the selected variables.")
      return(invisible(.na_frequency_table(numeric(0), "x")))
    }
    # codes may be numeric (SPSS) in one variable and ".a" in another
    if (length(unique(vapply(tables, function(t) class(t$code)[1],
                             character(1)))) > 1L) {
      tables <- lapply(tables, function(t) {
        t$code <- as.character(t$code)
        t
      })
    }
    out <- do.call(rbind, tables)
    rownames(out) <- NULL
    return(out)
  }

  if (!is.numeric(x)) {
    cli::cli_abort("{.arg x} must be a numeric vector.")
  }
  out <- .na_frequency_table(x)
  if (nrow(out) == 0L) {
    x_name <- sub(".*\\$", "", deparse(substitute(x))[1])
    cli::cli_inform("No missing values in {.var {x_name}}.")
    return(invisible(out))
  }
  out
}


#' Missing-type table of one numeric vector (see na_frequencies())
#' @noRd
.na_frequency_table <- function(x, var_name = "x") {
  tag_map <- attr(x, "na_tag_map", exact = TRUE)
  labels  <- attr(x, "labels", exact = TRUE)
  numeric_codes <- is.null(tag_map) || is.numeric(tag_map)

  raw <- .plain_numeric(x)
  na_mask <- is.na(raw)
  empty_code <- if (numeric_codes) numeric(0) else character(0)
  if (!any(na_mask)) {
    return(data.frame(code = empty_code, label = character(0),
                      n = integer(0), prc = numeric(0), tag = character(0),
                      stringsAsFactors = FALSE))
  }

  tags <- .na_tags(raw[na_mask])
  tagged <- unique(tags[!is.na(tags)])
  n_tag <- vapply(tagged, function(t) sum(tags == t, na.rm = TRUE), integer(1))
  n_sys <- sum(is.na(tags))

  code <- if (!is.null(tag_map)) unname(tag_map)[match(tagged, names(tag_map))]
          else rep(if (numeric_codes) NA_real_ else NA_character_, length(tagged))
  label <- rep(NA_character_, length(tagged))
  if (!is.null(labels)) {
    na_labels <- labels[is.na(labels)]
    hit <- match(tagged, .na_tags(na_labels))
    label[!is.na(hit)] <- names(na_labels)[hit[!is.na(hit)]]
  }

  out <- data.frame(code = code, label = label, n = unname(n_tag),
                    tag = tagged, stringsAsFactors = FALSE)
  # Order by code, like the missing block of frequency() (SPSS order);
  # tags without a known code after them
  out <- out[order(is.na(out$code), out$code, out$tag), , drop = FALSE]
  if (n_sys > 0L) {
    out <- rbind(out, data.frame(
      code = if (numeric_codes) NA_real_ else NA_character_,
      label = "(System Missing)", n = n_sys, tag = NA_character_,
      stringsAsFactors = FALSE
    ))
  }
  out$prc <- out$n / length(raw) * 100
  out <- out[, c("code", "label", "n", "prc", "tag")]
  rownames(out) <- NULL
  out
}


#' Convert Tagged NAs Back to Original Codes
#'
#' @description
#' Replaces tagged NAs with their original missing value codes. Works with
#' data imported via [read_spss()] (with `tag_na = TRUE`) or any reader
#' that used the `tag_na` parameter ([read_stata()], [read_sas()],
#' [read_xpt()]). For native Stata/SAS tagged NAs (e.g., `.a`, `.A`) that
#' have no numeric codes to recover, use [strip_tags()] instead.
#'
#' @param x A numeric vector with tagged NAs, or a data frame.
#' @param ... For a data frame: the columns to convert (tidyselect). If
#'   empty, every numeric column is converted. Ignored for vectors.
#'
#' @return The input with tagged NAs that have numeric codes replaced by
#'   their original values (e.g., -9, -8, -42). System NAs (untagged)
#'   remain `NA`. Value labels and the variable label are kept (labels of
#'   missing types are attached to their codes again), so the result is
#'   `haven_labelled` when the input was. For native Stata/SAS tagged NAs
#'   (no numeric codes), falls back to [strip_tags()] behavior with a
#'   warning. A data frame is returned with the selected columns converted.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("haven", quietly = TRUE)) {
#'   # Tag -9/-8 as missing, then recover the original codes
#'   x <- set_na(c(1, 2, -9, 3, -8), -9, -8)
#'   untag_na(x)   # -9 and -8 are back
#' }
#' }
#'
#' @seealso [read_spss()], [read_stata()], [read_sas()], [na_frequencies()],
#'   [strip_tags()]
#' @family data-import
#' @export
untag_na <- function(x, ...) {
  .check_required("x")
  .check_haven("tagged NA recovery")
  if (is.data.frame(x)) {
    return(.tag_fun_df(x, untag_na, rlang::enexprs(...),
                       rlang::expr(c(...))))
  }
  .check_tag_input(x, "untag_na")

  tag_map <- attr(x, "na_tag_map", exact = TRUE)
  if (is.null(tag_map)) return(x)

  # Check if tag_map contains recoverable numeric codes (from tag_na)
  # vs native format codes (character strings like ".a", ".A")
  has_numeric_codes <- is.numeric(tag_map)

  if (!has_numeric_codes) {
    fmt <- attr(x, "na_tag_format") %||% "unknown"
    cli::cli_warn(c(
      "{.fn untag_na} recovers numeric missing codes.",
      "i" = "For {toupper(fmt)} data with native tagged NAs, there are no numeric codes to recover.",
      "i" = "Use {.fn strip_tags} to convert them to regular {.val NA}."
    ))
    return(strip_tags(x))
  }

  # Vectorized: one tag read for the whole vector. The former per-element
  # vapply(x[na_positions], haven::na_tag) split the labelled vector into
  # one vctrs object per missing value (untag_na over ALLBUS: 9 s).
  raw <- as.double(.plain_numeric(x))
  tags <- .na_tags(raw)
  hit <- !is.na(tags) & tags %in% names(tag_map)
  raw[hit] <- unname(tag_map)[match(tags[hit], names(tag_map))]

  # Labels: valid labels plus the labels of the missing types, now attached
  # to their codes again (they used to be dropped with the class)
  labels <- attr(x, "labels", exact = TRUE)
  if (!is.null(labels)) {
    lab_vals <- as.double(.plain_numeric(labels))
    lab_tags <- .na_tags(lab_vals)
    code <- match(lab_tags, names(tag_map))
    lab_vals[!is.na(code)] <- unname(tag_map)[code[!is.na(code)]]
    labels <- stats::setNames(lab_vals, names(labels))
  }

  .with_label_meta(raw, x, labels = labels,
                   label = attr(x, "label", exact = TRUE),
                   keep_missing = FALSE,
                   labelled = inherits(x, "haven_labelled") ||
                     length(labels) > 0L)
}


#' Strip Tags from Tagged NAs
#'
#' @description
#' Converts all tagged NAs to regular (untagged) `NA` values, effectively
#' removing the missing value type information. Works with data from any
#' format: [read_spss()], [read_por()], [read_stata()], [read_sas()], or
#' [read_xpt()].
#'
#' @param x A numeric vector with tagged NAs, or a data frame.
#' @param ... For a data frame: the columns to convert (tidyselect). If
#'   empty, every numeric column is converted. Ignored for vectors.
#'
#' @return The input with all tagged NAs replaced by regular `NA`. Value
#'   labels for missing types are removed; labels for valid values and the
#'   variable label are preserved. A `haven_labelled` input stays
#'   `haven_labelled`, so the labels survive [write_spss()] and
#'   [write_stata()]. A data frame is returned with the selected columns
#'   converted.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("haven", quietly = TRUE)) {
#'   x <- set_na(c(1, 2, -9, 3, -8), -9, -8)
#'   # Remove tag information: all missings become plain NA
#'   strip_tags(x)
#' }
#' }
#'
#' @seealso [read_spss()], [read_stata()], [read_sas()], [read_xpt()],
#'   [na_frequencies()], [untag_na()]
#' @family data-import
#' @export
strip_tags <- function(x, ...) {
  .check_required("x")
  if (is.data.frame(x)) {
    return(.tag_fun_df(x, strip_tags, rlang::enexprs(...),
                       rlang::expr(c(...))))
  }
  .check_tag_input(x, "strip_tags")
  labels <- attr(x, "labels", exact = TRUE)
  .with_label_meta(
    x, x,
    labels = if (!is.null(labels)) labels[!is.na(labels)],
    label = attr(x, "label", exact = TRUE),
    keep_missing = FALSE,
    labelled = inherits(x, "haven_labelled") || length(labels) > 0L
  )
}


#' Input check for strip_tags()/untag_na() vectors
#' @noRd
.check_tag_input <- function(x, fn, call = rlang::caller_env()) {
  if (!is.numeric(x) || is.factor(x)) {
    cli::cli_abort(c(
      "{.fn {fn}} needs a numeric vector (or a data frame), not {.cls {class(x)[1]}}.",
      "i" = "Tagged missing values only exist in numeric variables."
    ), call = call)
  }
  invisible(TRUE)
}


#' Apply strip_tags()/untag_na() to data frame columns
#'
#' Selected columns (tidyselect), or every numeric column when nothing is
#' selected; non-numeric columns are left unchanged.
#' @noRd
.tag_fun_df <- function(data, fun, dots, select_expr,
                        call = rlang::caller_env()) {
  .check_dot_names(names(dots), call = call)
  cols <- if (length(dots) == 0L) {
    which(vapply(data, function(v) is.numeric(v) && !is.factor(v), logical(1)))
  } else {
    tidyselect::eval_select(select_expr, data, env = call)
  }
  for (i in cols) {
    if (!is.numeric(data[[i]]) || is.factor(data[[i]])) next
    data[[i]] <- fun(data[[i]])
  }
  data
}


#' Re-attach label and missing-value metadata to a transformed vector
#'
#' Shared finisher for every function that computes a new numeric vector
#' from a (possibly imported) labelled one: rec(), to_numeric(),
#' strip_tags(), to_labelled(), std(), center(), pomps(). Arithmetic keeps
#' the tagged-NA payload bits but vctrs drops the class and the na_tag_map,
#' which left "orphan" tags that write_spss() could not write and that
#' na_frequencies() could not map to codes.
#'
#' With `keep_missing = TRUE` the tags of `x`'s na_tag_map survive, the map
#' and format are re-attached and `x`'s labelled missing types are appended
#' to `labels`. Tags that are not in the map (or all tags, with
#' `keep_missing = FALSE`) become plain NA.
#'
#' @param result Numeric vector computed from x
#' @param x The source vector (metadata donor)
#' @param labels Value labels for the valid values of `result`, or NULL
#' @param label Variable label, or NULL
#' @param keep_missing Keep x's missing types (tags + map + missing labels)?
#' @param labelled Force (TRUE) or suppress (FALSE) the haven_labelled
#'   class; NULL = haven_labelled when labels or a tag map remain
#' @return haven_labelled vector, or a plain double with a "label" attribute
#' @noRd
.with_label_meta <- function(result, x, labels = NULL, label = NULL,
                             keep_missing = TRUE, labelled = NULL) {
  result <- as.double(.plain_numeric(result))

  tag_map <- if (isTRUE(keep_missing)) attr(x, "na_tag_map", exact = TRUE)
  tags <- .na_tags(result)
  orphan <- !is.na(tags) & !(tags %in% names(tag_map))
  if (any(orphan)) result[orphan] <- NA_real_

  if (!is.null(labels)) {
    labels <- stats::setNames(as.double(.plain_numeric(labels)), names(labels))
    labels <- labels[!is.na(labels)]
  }
  if (!is.null(tag_map)) {
    x_labels <- attr(x, "labels", exact = TRUE)
    if (!is.null(x_labels)) {
      miss <- x_labels[is.na(x_labels)]
      miss <- miss[.na_tags(miss) %in% names(tag_map)]
      labels <- c(labels, stats::setNames(as.double(.plain_numeric(miss)),
                                          names(miss)))
    }
  }
  if (length(labels) == 0L) labels <- NULL

  make_labelled <- labelled %||% (!is.null(labels) || !is.null(tag_map))
  if (isTRUE(make_labelled) && requireNamespace("haven", quietly = TRUE)) {
    out <- haven::labelled(result, labels = labels, label = label)
  } else {
    out <- result
    if (!is.null(labels)) attr(out, "labels") <- labels
    if (!is.null(label)) attr(out, "label") <- label
  }
  if (!is.null(tag_map)) {
    attr(out, "na_tag_map") <- tag_map
    attr(out, "na_tag_format") <- attr(x, "na_tag_format", exact = TRUE) %||%
      "spss"
    attr(out, "spss_missing") <- attr(x, "spss_missing", exact = TRUE)
  }
  out
}
