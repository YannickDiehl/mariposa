# ============================================================================
# Recode & Dummy-Coding
# ============================================================================
# Functions for recoding, reversing, dichotomizing, and dummy-coding variables.
# Supports sjmisc-compatible string syntax for recoding rules.


# ============================================================================
# rec() — Flexible Recoding
# ============================================================================

#' Recode Variables Using String Syntax
#'
#' @description
#' \code{rec()} recodes values of a variable using an intuitive string syntax.
#' It consolidates recoding, reversing, dichotomizing, and missing value handling
#' in a single function — the R equivalent of SPSS's \code{RECODE} command.
#'
#' @param data A data frame or numeric vector. When a data frame is passed,
#'   use \code{...} to select variables.
#' @param ... Variables to recode (tidyselect). Only used when \code{data}
#'   is a data frame.
#' @param rules A character string defining the recoding rules (see Details).
#' @param as.factor If \code{TRUE}, return the result as a factor. Default:
#'   \code{FALSE}.
#' @param suffix A character string appended to the column names for the
#'   recoded variables (e.g., \code{"_r"}). If \code{NULL} (default), the
#'   original columns are overwritten in-place.
#' @param var.label A new variable label. If \code{NULL}, the existing label
#'   is kept with \code{" (recoded)"} appended.
#' @param val.labels A named character vector of value labels for the new
#'   values (e.g., \code{c("1" = "Low", "2" = "Medium", "3" = "High")}).
#'   If \code{NULL}, labels are taken from inline \code{[Label]} syntax in
#'   \code{rules} (if present). Explicit \code{val.labels} always override
#'   inline labels.
#'
#' @return If \code{data} is a vector, a recoded vector is returned. If
#'   \code{data} is a data frame, the modified data frame is returned
#'   (invisibly).
#'
#' @details
#' ## Recoding Syntax
#'
#' Rules are specified as a semicolon-separated string of
#' \code{"old=new"} pairs:
#'
#' \tabular{lll}{
#'   \strong{Syntax}       \tab \strong{Meaning}        \tab \strong{Example} \cr
#'   \code{"old=new"}      \tab Single value             \tab \code{"1=0; 2=1"} \cr
#'   \code{"lo:hi=new"}    \tab Range of values          \tab \code{"1:3=1; 4:6=2"} \cr
#'   \code{"old=new [Label]"} \tab Inline value label    \tab \code{"1:2=1 [Low]; 3:5=2 [High]"} \cr
#'   \code{"else=new"}     \tab Catch-all for unmatched  \tab \code{"1=1; else=NA"} \cr
#'   \code{"copy"}         \tab Keep original value      \tab \code{"1:3=copy; else=NA"} \cr
#'   \code{"min"/"max"}    \tab Dynamic boundaries       \tab \code{"min:3=1; 4:max=2"} \cr
#'   \code{"rev"}          \tab Reverse scale            \tab \code{"rev"} \cr
#'   \code{"dicho"}        \tab Median split             \tab \code{"dicho"} \cr
#'   \code{"dicho(x)"}     \tab Fixed cut-point          \tab \code{"dicho(3)"} \cr
#'   \code{"mean"}         \tab Mean split               \tab \code{"mean"} \cr
#'   \code{"quart"}        \tab Quartile split (4 groups) \tab \code{"quart"} \cr
#'   \code{"NA=new"}       \tab Replace NA               \tab \code{"NA=0; else=copy"} \cr
#'   \code{"val=NA"}       \tab Set values to NA          \tab \code{"-9=NA; -8=NA"} \cr
#' }
#'
#' Rules are evaluated in order — the first matching rule wins.
#'
#' ## Inline Value Labels
#'
#' You can attach value labels directly in the rules string using
#' square brackets after the new value:
#'
#' \code{"1:2=1 [Low]; 3=2 [Medium]; 4:5=3 [High]"}
#'
#' This is equivalent to specifying
#' \code{val.labels = c("1" = "Low", "2" = "Medium", "3" = "High")}
#' but more compact and self-documenting. If both inline labels and
#' \code{val.labels} are provided, \code{val.labels} takes precedence.
#'
#' ## Special Modes
#'
#' \code{"rev"} reverses the scale by computing
#' \code{max(x) + min(x) - x}. Value labels are mirrored accordingly.
#'
#' \code{"dicho"} dichotomizes at the median: values \eqn{\le} median become 0,
#' values \eqn{>} median become 1.
#'
#' \code{"dicho(x)"} dichotomizes at a fixed cut-point \code{x}:
#' values \eqn{\le x} become 0, values \eqn{> x} become 1.
#'
#' \code{"mean"} dichotomizes at the arithmetic mean: values \eqn{\le} mean
#' become 0, values \eqn{>} mean become 1.
#'
#' \code{"quart"} splits into four quartile groups using \code{quantile()}:
#' values \eqn{\le} Q1 become 1, Q1–Q2 become 2, Q2–Q3 become 3,
#' and \eqn{>} Q3 become 4. Quartile boundaries are computed unweighted.
#'
#' @examples
#' library(dplyr)
#' data(survey_data)
#'
#' # Collapse a 5-point scale to 3 categories (inline labels)
#' data <- rec(survey_data, trust_government,
#'             rules = "1:2=1 [Low]; 3=2 [Medium]; 4:5=3 [High]")
#'
#' # Reverse a scale (with suffix to keep original)
#' data <- rec(survey_data, trust_government, trust_media,
#'             rules = "rev", suffix = "_r")
#'
#' # Dichotomize at the median
#' data <- rec(survey_data, age, rules = "dicho", suffix = "_d")
#'
#' # Set missing value codes to NA
#' data <- rec(survey_data, starts_with("trust"),
#'             rules = "-9=NA; -8=NA; else=copy")
#'
#' # Replace NA with 0
#' data <- rec(survey_data, trust_government,
#'             rules = "NA=0; else=copy")
#'
#' # Quartile split
#' data <- rec(survey_data, age, rules = "quart", suffix = "_q")
#'
#' # Use inside mutate()
#' survey_data <- survey_data %>%
#'   mutate(
#'     trust_gov_3 = rec(trust_government,
#'                       rules = "1:2=1 [Low]; 3=2 [Medium]; 4:5=3 [High]"),
#'     age_q = rec(age, rules = "quart")
#'   )
#'
#' @seealso [to_dummy()] for creating dummy variables,
#'   [set_na()] for declaring missing values,
#'   [to_label()] for converting to factor labels
#'
#' @family recode
#' @export
rec <- function(data, ..., rules, as.factor = FALSE, suffix = NULL,
                var.label = NULL, val.labels = NULL) {

  # ============================================================================
  # VECTOR INPUT
  # ============================================================================

  if (!is.data.frame(data)) {
    if (!is.atomic(data)) {
      cli::cli_abort("{.arg data} must be a data frame, vector, or factor.")
    }
    return(.rec_vec(data, rules = rules, as.factor = as.factor,
                    var.label = var.label, val.labels = val.labels))
  }

  # ============================================================================
  # DATA FRAME INPUT
  # ============================================================================

  if (missing(rules)) {
    cli::cli_abort("{.arg rules} is required.")
  }

  vars <- .process_variables(data, ...)

  for (i in vars) {
    col_name <- names(data)[i]
    result <- .rec_vec(data[[i]], rules = rules, as.factor = as.factor,
                       var.label = var.label, val.labels = val.labels)

    out_name <- if (!is.null(suffix)) paste0(col_name, suffix) else col_name
    data[[out_name]] <- result
  }

  invisible(data)
}


# ============================================================================
# Internal: Apply recoding to a single vector
# ============================================================================

#' @noRd
.rec_vec <- function(x, rules, as.factor = FALSE, var.label = NULL,
                     val.labels = NULL) {

  if (!is.character(rules) || length(rules) != 1L) {
    cli::cli_abort("{.arg rules} must be a single character string.")
  }

  # Preserve variable label
  orig_label <- attr(x, "label", exact = TRUE)
  new_label <- var.label %||% {
    if (!is.null(orig_label)) paste0(orig_label, " (recoded)") else NULL
  }

  # ---- Special modes --------------------------------------------------------

  rules_trimmed <- trimws(rules)

  if (rules_trimmed == "rev") {
    result <- .apply_rev(x)
    if (!is.null(new_label)) attr(result, "label") <- new_label
    if (isTRUE(as.factor)) result <- .rec_to_factor(result, val.labels)
    return(result)
  }

  if (grepl("^dicho(\\(|$)", rules_trimmed)) {
    cut_point <- NULL
    m <- regmatches(rules_trimmed, regexec("^dicho\\(([^)]+)\\)$", rules_trimmed))
    if (length(m[[1]]) == 2L) {
      cut_point <- as.numeric(m[[1]][2])
      if (is.na(cut_point)) {
        cli::cli_abort("Invalid cut-point in {.val {rules}}.")
      }
    }
    result <- .apply_dicho(x, cut_point = cut_point, val.labels = val.labels)
    if (!is.null(new_label)) attr(result, "label") <- new_label
    if (isTRUE(as.factor)) result <- .rec_to_factor(result, val.labels)
    return(result)
  }

  if (rules_trimmed == "quart") {
    result <- .apply_quart(x, val.labels = val.labels)
    if (!is.null(new_label)) attr(result, "label") <- new_label
    if (isTRUE(as.factor)) result <- .rec_to_factor(result, val.labels)
    return(result)
  }

  if (rules_trimmed == "mean") {
    x_num <- suppressWarnings(as.numeric(x))
    cut_point <- mean(x_num, na.rm = TRUE)
    result <- .apply_dicho(x, cut_point = cut_point, val.labels = val.labels)
    if (!is.null(new_label)) attr(result, "label") <- new_label
    if (isTRUE(as.factor)) result <- .rec_to_factor(result, val.labels)
    return(result)
  }

  # ---- Standard recoding ----------------------------------------------------

  parsed <- .parse_rec_rules(rules, x)
  result <- .apply_rec_rules(x, parsed)

  # Apply value labels: explicit val.labels > inline + preserved originals > none
  effective_labels <- val.labels

  if (!is.null(val.labels)) {
    # Explicit val.labels always take full precedence
    attr(result, "labels") <- stats::setNames(
      as.numeric(names(val.labels)),
      unname(val.labels)
    )
  } else {
    # Collect inline labels from parsed rules
    inline_labels <- character(0)
    for (rule in parsed) {
      if (!is.null(rule$label) && !is.na(rule$new) &&
          !identical(rule$new, "copy")) {
        key <- as.character(rule$new)
        if (!(key %in% names(inline_labels))) {
          inline_labels[key] <- rule$label
        }
      }
    }

    # When copy is used, preserve original labels for copied values
    has_copy <- any(vapply(parsed, function(r) identical(r$new, "copy"),
                           logical(1)))
    orig_labels <- attr(x, "labels", exact = TRUE)

    if (has_copy && !is.null(orig_labels)) {
      # Values explicitly created by recode (not copy)
      recoded_new_vals <- unique(unlist(lapply(parsed, function(r) {
        if (!identical(r$new, "copy") && !is.na(r$new)) r$new else NULL
      })))

      # Build merged label map: key = value string, value = label text
      merged <- character(0)
      valid_orig <- orig_labels[!is.na(orig_labels)]
      result_vals <- unique(result[!is.na(result)])

      for (j in seq_along(valid_orig)) {
        v <- unname(valid_orig[j])
        # Keep if value exists in result AND was not a recode target
        if (v %in% result_vals && !(v %in% recoded_new_vals)) {
          merged[as.character(v)] <- names(valid_orig)[j]
        }
      }

      # Inline labels override original labels
      for (key in names(inline_labels)) {
        merged[key] <- inline_labels[key]
      }

      if (length(merged) > 0L) {
        effective_labels <- merged
        attr(result, "labels") <- stats::setNames(
          as.numeric(names(merged)),
          unname(merged)
        )
      }
    } else if (length(inline_labels) > 0L) {
      effective_labels <- inline_labels
      attr(result, "labels") <- stats::setNames(
        as.numeric(names(inline_labels)),
        unname(inline_labels)
      )
    }
  }

  # Set variable label
  if (!is.null(new_label)) attr(result, "label") <- new_label

  # Optional factor conversion
  if (isTRUE(as.factor)) result <- .rec_to_factor(result, effective_labels)

  result
}


# ============================================================================
# Internal: Parse recoding rules string
# ============================================================================

#' @noRd
.parse_rec_rules <- function(rules, x) {
  # Split by semicolons
  parts <- trimws(strsplit(rules, ";")[[1]])
  parts <- parts[nzchar(parts)]

  if (length(parts) == 0L) {
    cli::cli_abort("No valid rules found in {.arg rules}.")
  }

  x_min <- suppressWarnings(min(as.numeric(x), na.rm = TRUE))
  x_max <- suppressWarnings(max(as.numeric(x), na.rm = TRUE))

  parsed <- vector("list", length(parts))

  for (i in seq_along(parts)) {
    part <- parts[i]

    # Split on the first "=" only (labels in [...] may contain "=")
    eq_pos <- regexpr("=", part, fixed = TRUE)
    if (eq_pos < 1L) {
      cli::cli_abort("Invalid rule: {.val {part}}. Expected format: {.val old=new}.")
    }

    lhs <- trimws(substr(part, 1L, eq_pos - 1L))
    rhs <- trimws(substr(part, eq_pos + 1L, nchar(part)))

    # Extract inline label [label] from RHS
    inline_label <- NULL
    if (grepl("\\[.+\\]\\s*$", rhs)) {
      m <- regmatches(rhs, regexec("\\[(.+)\\]\\s*$", rhs))
      inline_label <- trimws(m[[1]][2])
      rhs <- trimws(sub("\\s*\\[.+\\]\\s*$", "", rhs))
    }

    # ---- Parse the NEW (right-hand) value -----------------------------------
    new_val <- if (toupper(rhs) == "NA") {
      NA_real_
    } else if (toupper(rhs) == "COPY") {
      "copy"
    } else {
      val <- suppressWarnings(as.numeric(rhs))
      if (is.na(val)) {
        cli::cli_abort("Invalid new value: {.val {rhs}} in rule {.val {part}}.")
      }
      val
    }

    # ---- Parse the OLD (left-hand) specification ----------------------------

    if (toupper(lhs) == "ELSE") {
      parsed[[i]] <- list(type = "else", new = new_val, label = inline_label)
      next
    }

    if (toupper(lhs) == "NA") {
      parsed[[i]] <- list(type = "na", new = new_val, label = inline_label)
      next
    }

    # Check for range "lo:hi"
    if (grepl(":", lhs)) {
      range_parts <- strsplit(lhs, ":", fixed = TRUE)[[1]]
      if (length(range_parts) != 2L) {
        cli::cli_abort("Invalid range: {.val {lhs}} in rule {.val {part}}.")
      }
      lo_str <- trimws(range_parts[1])
      hi_str <- trimws(range_parts[2])

      lo <- if (toupper(lo_str) == "MIN") x_min
            else suppressWarnings(as.numeric(lo_str))
      hi <- if (toupper(hi_str) == "MAX") x_max
            else suppressWarnings(as.numeric(hi_str))

      if (is.na(lo) || is.na(hi)) {
        cli::cli_abort("Invalid range values in {.val {part}}.")
      }

      parsed[[i]] <- list(type = "range", from = lo, to = hi, new = new_val,
                          label = inline_label)
      next
    }

    # Single value
    old_val <- suppressWarnings(as.numeric(lhs))
    if (is.na(old_val)) {
      cli::cli_abort("Invalid old value: {.val {lhs}} in rule {.val {part}}.")
    }

    parsed[[i]] <- list(type = "value", from = old_val, new = new_val,
                        label = inline_label)
  }

  parsed
}


# ============================================================================
# Internal: Apply parsed rules to a vector
# ============================================================================

#' @noRd
.apply_rec_rules <- function(x, parsed) {
  x_num <- suppressWarnings(as.numeric(x))
  result <- rep(NA_real_, length(x))
  matched <- rep(FALSE, length(x))

  for (rule in parsed) {

    if (rule$type == "na") {
      # Match NA values (including tagged NAs)
      mask <- is.na(x_num) & !matched
      if (any(mask)) {
        if (identical(rule$new, "copy")) {
          # copy on NA → keep NA
          result[mask] <- NA_real_
        } else {
          result[mask] <- rule$new
        }
        matched[mask] <- TRUE
      }
      next
    }

    if (rule$type == "else") {
      mask <- !matched
      if (any(mask)) {
        if (identical(rule$new, "copy")) {
          result[mask] <- x_num[mask]
        } else {
          result[mask] <- rule$new
        }
        matched[mask] <- TRUE
      }
      next
    }

    # Skip NA positions for value/range rules (NAs only matched by "NA=" rule)
    available <- !is.na(x_num) & !matched

    if (rule$type == "value") {
      mask <- available & x_num == rule$from
      if (any(mask)) {
        if (identical(rule$new, "copy")) {
          result[mask] <- x_num[mask]
        } else {
          result[mask] <- rule$new
        }
        matched[mask] <- TRUE
      }
      next
    }

    if (rule$type == "range") {
      mask <- available & x_num >= rule$from & x_num <= rule$to
      if (any(mask)) {
        if (identical(rule$new, "copy")) {
          result[mask] <- x_num[mask]
        } else {
          result[mask] <- rule$new
        }
        matched[mask] <- TRUE
      }
      next
    }
  }

  # Unmatched values that are not NA stay as NA in result
  result
}


# ============================================================================
# Internal: Reverse a scale
# ============================================================================

#' @noRd
.apply_rev <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  x_min <- min(x_num, na.rm = TRUE)
  x_max <- max(x_num, na.rm = TRUE)

  result <- x_max + x_min - x_num

  # Mirror value labels
  old_labels <- attr(x, "labels", exact = TRUE)
  if (!is.null(old_labels)) {
    # Filter out NA-tagged labels
    valid <- !is.na(old_labels)
    valid_labels <- old_labels[valid]
    na_labels <- old_labels[!valid]

    # Reverse the values
    new_vals <- x_max + x_min - unname(valid_labels)
    new_labels <- stats::setNames(new_vals, names(valid_labels))

    # Recombine
    attr(result, "labels") <- c(new_labels, na_labels)
  }

  # Preserve variable label
  attr(result, "label") <- attr(x, "label", exact = TRUE)

  result
}


# ============================================================================
# Internal: Dichotomize
# ============================================================================

#' @noRd
.apply_dicho <- function(x, cut_point = NULL, val.labels = NULL) {
  x_num <- suppressWarnings(as.numeric(x))

  if (is.null(cut_point)) {
    cut_point <- stats::median(x_num, na.rm = TRUE)
  }

  result <- ifelse(is.na(x_num), NA_real_,
                   ifelse(x_num <= cut_point, 0, 1))

  # Apply value labels
  if (!is.null(val.labels)) {
    attr(result, "labels") <- stats::setNames(
      as.numeric(names(val.labels)),
      unname(val.labels)
    )
  }

  result
}


# ============================================================================
# Internal: Quartile split
# ============================================================================

#' @noRd
.apply_quart <- function(x, val.labels = NULL) {
  x_num <- suppressWarnings(as.numeric(x))

  q <- stats::quantile(x_num, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

  result <- ifelse(is.na(x_num), NA_real_,
                   ifelse(x_num <= q[1], 1,
                   ifelse(x_num <= q[2], 2,
                   ifelse(x_num <= q[3], 3, 4))))

  # Apply value labels
  if (!is.null(val.labels)) {
    attr(result, "labels") <- stats::setNames(
      as.numeric(names(val.labels)),
      unname(val.labels)
    )
  }

  result
}


# ============================================================================
# Internal: Convert to factor
# ============================================================================

#' @noRd
.rec_to_factor <- function(x, val.labels = NULL) {
  if (!is.null(val.labels)) {
    lvls <- as.numeric(names(val.labels))
    lbl_text <- unname(val.labels)
    result <- factor(x, levels = lvls, labels = lbl_text)
  } else {
    result <- factor(x)
  }

  # Preserve variable label
  var_lbl <- attr(x, "label", exact = TRUE)
  if (!is.null(var_lbl)) attr(result, "label") <- var_lbl

  result
}


# ============================================================================
# to_dummy() — Dummy Coding
# ============================================================================

#' Create Dummy Variables (One-Hot Encoding)
#'
#' @description
#' Creates 0/1 dummy variables from categorical variables. Column names are
#' derived from value labels when available, making results readable for
#' SPSS-style data.
#'
#' Unlike \code{model.matrix()}, \code{to_dummy()} uses value labels for
#' column naming and handles \code{haven_labelled} vectors correctly.
#'
#' @param data A data frame or vector. When a data frame is passed, use
#'   \code{...} to select variables.
#' @param ... Variables to dummy-code (tidyselect). Only used when \code{data}
#'   is a data frame.
#' @param suffix How to name dummy columns: \code{"val"} (default) uses the
#'   raw value (e.g., \code{gender_1}), \code{"label"} uses the value label
#'   (e.g., \code{gender_Male}).
#' @param ref A value to use as reference category (omitted from output).
#'   If \code{NULL} (default), all categories get a dummy variable.
#'   Set to a specific value for n-1 coding (e.g., for regression).
#' @param append If \code{TRUE} (default), the dummy columns are appended to
#'   the original data frame. If \code{FALSE}, only the dummy columns are
#'   returned. Ignored when \code{data} is a vector.
#'
#' @return If \code{append = TRUE} (default), the original data frame with
#'   dummy columns appended. If \code{append = FALSE}, a tibble with only the
#'   dummy columns. For vector input, always a tibble of dummy columns.
#'
#' @details
#' ## Column Naming
#'
#' With \code{suffix = "val"}: \code{{varname}_{value}} (e.g., \code{gender_1},
#' \code{gender_2}). With \code{suffix = "label"}: \code{{varname}_{label}}
#' where labels are cleaned (spaces replaced with \code{_}, special characters
#' removed).
#'
#' ## Reference Category
#'
#' For regression, you typically need n-1 dummy variables. Set \code{ref} to
#' the value of the reference category to omit it.
#'
#' @examples
#' library(dplyr)
#' data(survey_data)
#'
#' # Create dummies and append to data (default)
#' data <- to_dummy(survey_data, gender)
#'
#' # Use labels for column names
#' data <- to_dummy(survey_data, gender, suffix = "label")
#'
#' # n-1 dummies with reference category
#' data <- to_dummy(survey_data, gender, ref = 1)
#'
#' # Multiple variables
#' data <- to_dummy(survey_data, gender, education, suffix = "label")
#'
#' # Return only the dummy columns (without original data)
#' dummies <- to_dummy(survey_data, gender, append = FALSE)
#'
#' @seealso [rec()] for general recoding, [to_label()] for converting to factor
#'
#' @family recode
#' @export
to_dummy <- function(data, ..., suffix = "val", ref = NULL, append = TRUE) {

  suffix <- match.arg(suffix, choices = c("val", "label"))

  # ============================================================================
  # VECTOR INPUT
  # ============================================================================

  if (!is.data.frame(data)) {
    var_name <- deparse(substitute(data))
    # Clean up deparse artifacts
    if (grepl("\\$", var_name)) var_name <- sub(".*\\$", "", var_name)
    return(.to_dummy_vec(data, var_name = var_name, suffix = suffix, ref = ref))
  }

  # ============================================================================
  # DATA FRAME INPUT
  # ============================================================================

  vars <- .process_variables(data, ...)
  dummy_cols <- tibble::tibble(.rows = nrow(data))

  for (i in vars) {
    dummies <- .to_dummy_vec(data[[i]], var_name = names(data)[i],
                             suffix = suffix, ref = ref)
    dummy_cols <- dplyr::bind_cols(dummy_cols, dummies)
  }

  if (isTRUE(append)) {
    dplyr::bind_cols(data, dummy_cols)
  } else {
    dummy_cols
  }
}


# ============================================================================
# Internal: Create dummies for a single vector
# ============================================================================

#' @noRd
.to_dummy_vec <- function(x, var_name, suffix = "val", ref = NULL) {
  # Get unique values (excluding NA)
  if (is.factor(x)) {
    vals <- levels(x)
    is_factor <- TRUE
  } else {
    vals <- sort(unique(x[!is.na(x)]))
    is_factor <- FALSE
  }

  # Build label map for column naming
  label_map <- NULL
  if (suffix == "label") {
    if (is_factor) {
      label_map <- stats::setNames(as.character(vals), as.character(vals))
    } else {
      vl <- attr(x, "labels", exact = TRUE)
      if (!is.null(vl)) {
        # Only use non-NA labels
        vl <- vl[!is.na(vl)]
        label_map <- stats::setNames(names(vl), as.character(unname(vl)))
      }
    }
  }

  # Remove reference category
  if (!is.null(ref)) {
    ref_char <- as.character(ref)
    vals <- vals[as.character(vals) != ref_char]
    if (length(vals) == 0L) {
      cli::cli_abort(
        "Reference value {.val {ref}} removed all categories for variable {.var {var_name}}."
      )
    }
  }

  # Create dummy columns
  result <- tibble::tibble(.rows = length(x))

  for (v in vals) {
    # Determine column name
    if (suffix == "label" && !is.null(label_map)) {
      lbl <- label_map[as.character(v)]
      if (!is.na(lbl)) {
        col_suffix <- .clean_label_for_colname(lbl)
      } else {
        col_suffix <- as.character(v)
      }
    } else {
      col_suffix <- as.character(v)
    }

    col_name <- paste0(var_name, "_", col_suffix)

    dummy <- ifelse(is.na(x), NA_integer_, as.integer(x == v))

    result[[col_name]] <- dummy
  }

  result
}


# ============================================================================
# Internal: Clean a label for use as column name
# ============================================================================

#' @noRd
.clean_label_for_colname <- function(label) {
  # Replace spaces with underscores
  out <- gsub("\\s+", "_", label)
  # Remove anything that's not alphanumeric or underscore
  out <- gsub("[^A-Za-z0-9_]", "", out)
  # Remove leading/trailing underscores
  out <- gsub("^_+|_+$", "", out)
  # Collapse multiple underscores
  out <- gsub("_+", "_", out)
  out
}
