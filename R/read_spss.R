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
#' @param tag.na If `TRUE` (the default), user-defined missing values are
#'   converted to tagged NAs using [haven::tagged_na()]. If `FALSE`, the file
#'   is read with standard `haven::read_sav()` behavior (all missing values
#'   become regular `NA`).
#' @param encoding Character encoding for the file. If `NULL`, haven's default
#'   encoding detection is used.
#' @param verbose If `TRUE`, prints a message summarizing how many values were
#'   converted.
#'
#' @return A tibble with the SPSS data. When `tag.na = TRUE`:
#'   \itemize{
#'     \item User-defined missing values are stored as tagged NAs
#'     \item `is.na()` returns `TRUE` for these values (standard R behavior)
#'     \item The original SPSS missing codes can be recovered via
#'       [na_frequencies()], [untag_na()], or [haven::na_tag()]
#'     \item Each tagged variable has an `"na_tag_map"` attribute mapping
#'       tag characters to original SPSS codes
#'   }
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
#' \dontrun{
#' # Read SPSS file with tagged missing values
#' data <- read_spss("survey.sav")
#'
#' # Check what types of missing values exist
#' na_frequencies(data$satisfaction)
#'
#' # Standard R operations work normally (NAs are excluded)
#' mean(data$satisfaction, na.rm = TRUE)
#'
#' # frequency() shows each missing type separately
#' data %>% frequency(satisfaction)
#'
#' # Recover original SPSS codes
#' original_codes <- untag_na(data$satisfaction)
#'
#' # Convert to regular NAs (standard behavior)
#' data_clean <- strip_tags(data$satisfaction)
#' }
#'
#' @export
read_spss <- function(path, tag.na = TRUE, encoding = NULL, verbose = FALSE) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg haven} is required for SPSS import.",
      "i" = "Install it with: {.code install.packages(\"haven\")}"
    ))
  }

  # Read with user_na = TRUE to preserve missing value metadata as attributes
  # (haven_labelled_spss class keeps values but marks them via na_range/na_values)
  data <- haven::read_sav(file = path, encoding = encoding, user_na = tag.na)

  if (!tag.na) return(data)

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
#' @param tag.na If `TRUE` (the default), user-defined missing values are
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
#' \dontrun{
#' data <- read_por("survey.por")
#' na_frequencies(data$satisfaction)
#' }
#'
#' @export
read_por <- function(path, tag.na = TRUE, verbose = FALSE) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg haven} is required for SPSS portable import.",
      "i" = "Install it with: {.code install.packages(\"haven\")}"
    ))
  }

  data <- haven::read_por(file = path, user_na = tag.na)

  if (!tag.na) return(data)

  .tag_spss_missing_values(data, verbose)
}


# ---- Internal Helpers -------------------------------------------------------

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
  missing_values <- sort(unique(missing_values))
  if (length(missing_values) == 0L) return(data)

  # Assign tag characters: a-z, A-Z, 0-9 (62 possible tags)
  tag_pool <- c(letters, LETTERS, as.character(0:9))
  if (length(missing_values) > length(tag_pool)) {
    cli::cli_warn(
      "{.arg tag.na} has {length(missing_values)} values; only the first {length(tag_pool)} will be tagged."
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

  tags <- vapply(x[na_mask], haven::na_tag, character(1))
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
#' @param x A numeric vector with tagged NAs.
#'
#' @return A data frame with columns:
#'   \item{tag}{The tag character (e.g., a-z for Stata, A-Z for SAS,
#'     a-z/A-Z/0-9 for SPSS)}
#'   \item{n}{Number of cases with this missing type}
#'   \item{code}{The original missing value code: numeric SPSS codes (e.g.,
#'     -9, -8) or native format codes (e.g., ".a" for Stata, ".A" for SAS)}
#'   \item{label}{The value label for this missing type (if available)}
#'
#' @examples
#' \dontrun{
#' # SPSS data
#' data <- read_spss("survey.sav")
#' na_frequencies(data$satisfaction)
#' #   tag    n  code            label
#' # 1   b 1774   -11       TNZ: SPLIT
#' # 2   c   63    -9     KEINE ANGABE
#' # 3   d   11    -8    WEISS NICHT
#' # 4   a    6   -42 DATENFEHLER: MFN
#'
#' # Stata data
#' data <- read_stata("survey.dta")
#' na_frequencies(data$income)
#' #   tag  n code           label
#' # 1   a 42   .a     Not applicable
#' # 2   b 15   .b     Refused
#' }
#'
#' @seealso [read_spss()], [read_stata()], [read_sas()], [read_xpt()],
#'   [untag_na()], [strip_tags()]
#' @family data-import
#' @export
na_frequencies <- function(x) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg haven} is required for tagged NA inspection.",
      "i" = "Install it with: {.code install.packages(\"haven\")}"
    ))
  }
  if (!is.numeric(x)) {
    cli::cli_abort("{.arg x} must be a numeric vector.")
  }

  tag_map <- attr(x, "na_tag_map")
  labels  <- attr(x, "labels")

  na_mask <- is.na(x)
  if (!any(na_mask)) {
    return(data.frame(
      tag = character(0), n = integer(0),
      code = character(0), label = character(0),
      stringsAsFactors = FALSE
    ))
  }

  tags <- vapply(x[na_mask], haven::na_tag, character(1))
  tag_tbl <- table(tags, useNA = "always")

  result <- data.frame(
    tag = names(tag_tbl),
    n = as.integer(tag_tbl),
    stringsAsFactors = FALSE
  )

  # Add original codes (numeric for SPSS, character for Stata/SAS)
  if (!is.null(tag_map)) {
    result$code <- as.character(tag_map[result$tag])
  } else {
    result$code <- NA_character_
  }

  # Add value labels
  if (!is.null(labels)) {
    na_labels <- labels[is.na(labels)]
    label_tags <- vapply(na_labels, haven::na_tag, character(1))
    label_lookup <- stats::setNames(names(na_labels), label_tags)
    result$label <- label_lookup[result$tag]
  } else {
    result$label <- NA_character_
  }

  # Handle system NAs (tag = NA)
  sys_na_rows <- is.na(result$tag)
  result$label[sys_na_rows] <- "(System Missing)"
  result$code[sys_na_rows]  <- NA_character_

  result <- result[order(-result$n), , drop = FALSE]
  rownames(result) <- NULL
  result
}


#' Convert Tagged NAs Back to Original Codes
#'
#' @description
#' Replaces tagged NAs with their original missing value codes. Works with
#' data imported via [read_spss()] (with `tag.na = TRUE`) or any reader
#' that used the `tag.na` parameter ([read_stata()], [read_sas()],
#' [read_xpt()]). For native Stata/SAS tagged NAs (e.g., `.a`, `.A`) that
#' have no numeric codes to recover, use [strip_tags()] instead.
#'
#' @param x A numeric vector with tagged NAs.
#'
#' @return A numeric vector where tagged NAs with numeric codes have been
#'   replaced with their original values (e.g., -9, -8, -42). System NAs
#'   (untagged) remain as `NA`. For native Stata/SAS tagged NAs (no numeric
#'   codes), falls back to [strip_tags()] behavior with a warning.
#'
#' @examples
#' \dontrun{
#' # SPSS data
#' data <- read_spss("survey.sav")
#' original <- untag_na(data$satisfaction)
#'
#' # Stata data with tag.na
#' data <- read_stata("survey.dta", tag.na = c(-9, -8, -42))
#' original <- untag_na(data$income)
#' }
#'
#' @seealso [read_spss()], [read_stata()], [read_sas()], [na_frequencies()],
#'   [strip_tags()]
#' @family data-import
#' @export
untag_na <- function(x) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg haven} is required for tagged NA recovery.",
      "i" = "Install it with: {.code install.packages(\"haven\")}"
    ))
  }

  tag_map <- attr(x, "na_tag_map")
  if (is.null(tag_map)) return(as.double(x))

  # Check if tag_map contains recoverable numeric codes (from tag.na)
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

  raw <- as.double(x)
  na_positions <- which(is.na(x))

  if (length(na_positions) == 0L) return(raw)

  # Get the tag for each NA position
  na_tags <- vapply(x[na_positions], haven::na_tag, character(1))

  # Replace each tagged NA with its original code
  for (j in seq_along(na_positions)) {
    tag <- na_tags[j]
    if (!is.na(tag) && tag %in% names(tag_map)) {
      raw[na_positions[j]] <- tag_map[tag]
    }
  }

  raw
}


#' Strip Tags from Tagged NAs
#'
#' @description
#' Converts all tagged NAs to regular (untagged) `NA` values, effectively
#' removing the missing value type information. Works with data from any
#' format: [read_spss()], [read_por()], [read_stata()], [read_sas()], or
#' [read_xpt()].
#'
#' @param x A numeric vector with tagged NAs.
#'
#' @return A numeric vector where all tagged NAs have been replaced with
#'   regular `NA`. Value labels for missing types are removed; labels for
#'   valid values are preserved.
#'
#' @examples
#' \dontrun{
#' data <- read_spss("survey.sav")
#' # Remove tag information, keep only valid labels
#' clean <- strip_tags(data$satisfaction)
#' }
#'
#' @seealso [read_spss()], [read_stata()], [read_sas()], [read_xpt()],
#'   [na_frequencies()], [untag_na()]
#' @family data-import
#' @export
strip_tags <- function(x) {
  raw <- as.double(x)
  raw[is.na(x)] <- NA_real_

  # Keep only labels for valid (non-NA) values
  labels <- attr(x, "labels")
  if (!is.null(labels)) {
    attr(raw, "labels") <- labels[!is.na(labels)]
  }
  attr(raw, "label") <- attr(x, "label", exact = TRUE)

  raw
}
