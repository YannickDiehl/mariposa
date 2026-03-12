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
#'   [frequency()]
#'
#' @family spss
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
    stop("Package \"haven\" required. Please install it.", call. = FALSE)
  }

  # Read with user_na = TRUE to preserve missing value metadata as attributes
  # (haven_labelled_spss class keeps values but marks them via na_range/na_values)
  data <- haven::read_sav(file = path, encoding = encoding, user_na = tag.na)

  if (!tag.na) return(data)

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
      warning(
        sprintf(
          "Variable '%s' has %d missing value codes; only the first %d will be tagged.",
          names(data)[i], length(missing_vals), length(tag_pool)
        ),
        call. = FALSE
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

    data[[i]] <- new_x
    n_vars_converted <- n_vars_converted + 1L
  }

  if (verbose) {
    message(
      sprintf(
        "Converted %s values in %d variables to tagged NAs.",
        format(n_converted, big.mark = ","), n_vars_converted
      )
    )
  }

  data
}


#' Frequency Table of Missing Value Types
#'
#' @description
#' Shows a breakdown of the different types of missing values in a variable
#' that was read with [read_spss()] using `tag.na = TRUE`.
#'
#' @param x A numeric vector with tagged NAs (from [read_spss()]).
#'
#' @return A data frame with columns:
#'   \item{tag}{The tag character (a-z, A-Z, 0-9)}
#'   \item{n}{Number of cases with this missing type}
#'   \item{spss_code}{The original SPSS missing value code}
#'   \item{label}{The SPSS value label for this missing type}
#'
#' @examples
#' \dontrun{
#' data <- read_spss("survey.sav")
#' na_frequencies(data$satisfaction)
#' #   tag    n spss_code            label
#' # 1   b 1774       -11       TNZ: SPLIT
#' # 2   c   63        -9     KEINE ANGABE
#' # 3   d   11        -8 WEISS NICHT
#' # 4   a    6       -42 DATENFEHLER: MFN
#' }
#'
#' @seealso [read_spss()], [untag_na()], [strip_tags()]
#' @family spss
#' @export
na_frequencies <- function(x) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Package \"haven\" required. Please install it.", call. = FALSE)
  }
  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector.", call. = FALSE)
  }

  tag_map <- attr(x, "na_tag_map")
  labels  <- attr(x, "labels")

  na_mask <- is.na(x)
  if (!any(na_mask)) {
    return(data.frame(
      tag = character(0), n = integer(0),
      spss_code = numeric(0), label = character(0),
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

  # Add original SPSS codes
  if (!is.null(tag_map)) {
    result$spss_code <- tag_map[result$tag]
  } else {
    result$spss_code <- NA_real_
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
  result$label[sys_na_rows]     <- "(System Missing)"
  result$spss_code[sys_na_rows] <- NA_real_

  result <- result[order(-result$n), , drop = FALSE]
  rownames(result) <- NULL
  result
}


#' Convert Tagged NAs Back to Original SPSS Codes
#'
#' @description
#' Replaces tagged NAs with their original SPSS missing value codes, turning
#' `NA` values back into the numeric codes that SPSS used (e.g., -9, -8, -42).
#'
#' @param x A numeric vector with tagged NAs (from [read_spss()]).
#'
#' @return A numeric vector where tagged NAs have been replaced with their
#'   original SPSS codes. System NAs (untagged) remain as `NA`.
#'
#' @examples
#' \dontrun{
#' data <- read_spss("survey.sav")
#' # Recover original codes
#' original <- untag_na(data$satisfaction)
#' table(original, useNA = "always")
#' }
#'
#' @seealso [read_spss()], [na_frequencies()], [strip_tags()]
#' @family spss
#' @export
untag_na <- function(x) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Package \"haven\" required. Please install it.", call. = FALSE)
  }

  tag_map <- attr(x, "na_tag_map")
  if (is.null(tag_map)) return(as.double(x))

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
#' removing the missing value type information. This produces the same result
#' as reading the file with `haven::read_sav()` directly.
#'
#' @param x A numeric vector with tagged NAs (from [read_spss()]).
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
#' @seealso [read_spss()], [na_frequencies()], [untag_na()]
#' @family spss
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
