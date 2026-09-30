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
#' @param as_factor If \code{TRUE}, return the result as a factor. Default:
#'   \code{FALSE}.
#' @param suffix A character string appended to the column names for the
#'   recoded variables (e.g., \code{"_r"}). If \code{NULL} (default), the
#'   original columns are overwritten in-place.
#' @param var_label A new variable label. If \code{NULL}, the existing label
#'   is kept with \code{" (recoded)"} appended (once: recoding a recoded
#'   variable does not stack the suffix).
#' @param val_labels A named character vector of value labels for the new
#'   values (e.g., \code{c("1" = "Low", "2" = "Medium", "3" = "High")}).
#'   If \code{NULL}, labels are taken from inline \code{[Label]} syntax in
#'   \code{rules} (if present). Explicit \code{val_labels} always override
#'   inline labels.
#' @param as.factor,var.label,val.labels Defunct dot-case argument names,
#'   removed in mariposa 0.6.9. Calling the function with any of them is an
#'   error; use the snake_case equivalents instead. (The formals are
#'   retained only so that the old names error clearly instead of being
#'   swallowed by \code{...}.)
#'
#' @return If \code{data} is a vector, a recoded vector is returned. If
#'   \code{data} is a data frame, the modified data frame is returned
#'   (invisibly). A recoded variable is \code{haven_labelled} whenever it
#'   carries value labels or the missing-value types of an imported variable,
#'   so the labels survive \code{\link{write_spss}()} and
#'   \code{\link{write_stata}()}. With \code{as_factor = TRUE} the factor
#'   levels are ordered by code and named by the result's value labels
#'   (unlabelled values keep their code as level name).
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
#'   \code{"a,b=new"}      \tab List of values/ranges    \tab \code{"1,2=1; 3,4:5=2"} \cr
#'   \code{"old=new [Label]"} \tab Inline value label    \tab \code{"1:2=1 [Low]; 3:5=2 [High]"} \cr
#'   \code{"else=new"}     \tab Catch-all for unmatched  \tab \code{"1=1; else=NA"} \cr
#'   \code{"copy"}         \tab Keep original value      \tab \code{"1:3=copy; else=NA"} \cr
#'   \code{"min"/"max"}    \tab Dynamic boundaries       \tab \code{"min:3=1; 4:max=2"} \cr
#'   \code{"rev"}          \tab Reverse scale            \tab \code{"rev"} \cr
#'   \code{"rev(lo, hi)"}  \tab Reverse a lo-hi scale    \tab \code{"rev(1, 5)"} \cr
#'   \code{"dicho"}        \tab Median split             \tab \code{"dicho"} \cr
#'   \code{"dicho(x)"}     \tab Fixed cut-point          \tab \code{"dicho(3)"} \cr
#'   \code{"mean"}         \tab Mean split               \tab \code{"mean"} \cr
#'   \code{"quart"}        \tab Quartile split (4 groups) \tab \code{"quart"} \cr
#'   \code{"NA=new"}       \tab Replace NA               \tab \code{"NA=0; else=copy"} \cr
#'   \code{"val=NA"}       \tab Set values to NA          \tab \code{"-9=NA; -8=NA"} \cr
#' }
#'
#' Rules are evaluated in order — the first matching rule wins. Keywords
#' (\code{else}, \code{copy}, \code{NA}, \code{min}, \code{max},
#' \code{rev}, \code{dicho}, \code{mean}, \code{quart}) are
#' case-insensitive. A range must be written low to high (\code{"1:5"}, not
#' \code{"5:1"}). Inline labels may contain semicolons
#' (\code{"1:2=1 [low; poor]"}).
#'
#' Valid values that match no rule become \code{NA}, with a warning listing
#' them: add \code{"else=copy"} to keep them (what SPSS's in-place
#' \code{RECODE} does) or \code{"else=NA"} to confirm.
#'
#' ## Missing Values of Imported Data
#'
#' Missing values keep their type (the tagged NAs of \code{\link{read_spss}()},
#' e.g. "no answer" vs. "not applicable") unless an \code{"NA=..."} or
#' \code{"else=..."} rule recodes them, like SPSS's \code{RECODE} keeps
#' user-missing codes. \code{\link{na_frequencies}()}, \code{frequency()}
#' and \code{\link{write_spss}()} therefore still see them on the result.
#' A \code{haven_labelled_spss} vector (from
#' \code{haven::read_sav(user_na = TRUE)}) is first converted to this
#' tagged-NA form, so its user-missing codes are neither reversed nor
#' recoded as valid values.
#'
#' ## Decimal Values
#'
#' Both single values and ranges accept decimals (e.g. \code{"3.6=2"} or
#' \code{"2.5:3.5=1"}). Single-value matching compares values as strings
#' (rounded to 15 significant digits), so decimal codes match reliably even
#' when stored with floating-point representation error.
#'
#' ## Inline Value Labels
#'
#' You can attach value labels directly in the rules string using
#' square brackets after the new value:
#'
#' \code{"1:2=1 [Low]; 3=2 [Medium]; 4:5=3 [High]"}
#'
#' This is equivalent to specifying
#' \code{val_labels = c("1" = "Low", "2" = "Medium", "3" = "High")}
#' but more compact and self-documenting. If both inline labels and
#' \code{val_labels} are provided, \code{val_labels} takes precedence.
#'
#' ## Special Modes
#'
#' \code{"rev"} reverses the scale by computing \code{hi + lo - x}. The
#' scale range \code{lo}-\code{hi} is taken from the value labels of the
#' valid codes (together with the observed values), so an item answered
#' only with 2-5 on a labelled 1-5 scale becomes 4-1, not 5-2. Without
#' value labels the observed minimum and maximum are used and a message
#' says so; set the range explicitly with \code{"rev(lo, hi)"}, e.g.
#' \code{rules = "rev(1, 5)"}. Value labels are mirrored accordingly;
#' missing values keep their type.
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
#'             rules = "rev(1, 5)", suffix = "_r")
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
rec <- function(data, ..., rules, as_factor = FALSE, suffix = NULL,
                var_label = NULL, val_labels = NULL,
                as.factor = NULL, var.label = NULL, val.labels = NULL) {

  # ---- Removed dot-case arguments: hard error (see VERSIONING_POLICY.md, 4).
  # The formals stay as NULL sentinels because `...` is consumed by
  # tidyselect: without them, an old dot-case name would silently be
  # misinterpreted as a variable selection.
  if (!is.null(as.factor)) .stop_removed_arg("as.factor", "as_factor")
  if (!is.null(var.label)) .stop_removed_arg("var.label", "var_label")
  if (!is.null(val.labels)) .stop_removed_arg("val.labels", "val_labels")

  if (missing(rules)) {
    cli::cli_abort(c(
      "{.arg rules} is required.",
      "i" = "For example {.code rules = \"1:2=1; 3:5=2\"} or {.code rules = \"rev\"}."
    ))
  }

  # ============================================================================
  # VECTOR INPUT
  # ============================================================================

  if (!is.data.frame(data)) {
    if (!is.atomic(data)) {
      cli::cli_abort("{.arg data} must be a data frame, vector, or factor.")
    }
    return(.rec_vec(data, rules = rules, as_factor = as_factor,
                    var_label = var_label, val_labels = val_labels,
                    var_name = sub(".*\\$", "", deparse(substitute(data))[1])))
  }

  # ============================================================================
  # DATA FRAME INPUT
  # ============================================================================

  # Recoding is not a per-group computation: a grouping variable may be
  # recoded too
  vars <- .process_variables(data, ..., drop_groups = FALSE)

  for (i in vars) {
    col_name <- names(data)[i]
    result <- .rec_vec(data[[i]], rules = rules, as_factor = as_factor,
                       var_label = var_label, val_labels = val_labels,
                       var_name = col_name)

    out_name <- if (!is.null(suffix)) paste0(col_name, suffix) else col_name
    data[[out_name]] <- result
  }

  invisible(data)
}


# ============================================================================
# Internal: Apply recoding to a single vector
# ============================================================================

#' @noRd
.rec_vec <- function(x, rules, as_factor = FALSE, var_label = NULL,
                     val_labels = NULL, var_name = "x") {

  if (!is.character(rules) || length(rules) != 1L) {
    cli::cli_abort("{.arg rules} must be a single character string.")
  }

  # haven_labelled_spss (haven::read_sav(user_na = TRUE)): user-missing
  # codes are ordinary values there. Convert to the tagged-NA form of
  # read_spss() so they stay missing (not reversed or recoded as valid
  # values) and keep their codes and labels for na_frequencies() and
  # write_spss().
  if (inherits(x, "haven_labelled_spss")) {
    x <- .tag_spss_missing_values(tibble::tibble(x = x), verbose = FALSE)$x
  }

  # Preserve variable label (the suffix is added once, not per call)
  orig_label <- attr(x, "label", exact = TRUE)
  new_label <- var_label %||% {
    if (is.null(orig_label)) NULL
    else if (endsWith(orig_label, " (recoded)")) orig_label
    else paste0(orig_label, " (recoded)")
  }

  # ---- Special modes --------------------------------------------------------

  # Keywords (rev, dicho, mean, quart) are case-insensitive
  rules_trimmed <- tolower(trimws(rules))

  # Every mode ends here: the result keeps the missing types of x (tagged
  # NAs + na_tag_map + labelled missing codes) and is haven_labelled when it
  # carries labels, so write_spss()/na_frequencies()/frequency() keep
  # working on recoded imported variables.
  finish <- function(result, labels) {
    out <- .with_label_meta(result, x, labels = labels, label = new_label)
    if (isTRUE(as_factor)) out <- .rec_to_factor(out, labels)
    out
  }
  explicit_labels <- if (!is.null(val_labels)) {
    stats::setNames(as.numeric(names(val_labels)), unname(val_labels))
  }

  rev_match <- regmatches(
    rules_trimmed,
    regexec("^rev(\\(\\s*([^,]*?)\\s*,\\s*([^)]*?)\\s*\\))?$", rules_trimmed)
  )[[1]]
  if (length(rev_match) > 0L) {
    bounds <- NULL
    if (nzchar(rev_match[2])) {
      bounds <- suppressWarnings(as.numeric(rev_match[3:4]))
      if (anyNA(bounds) || bounds[1] >= bounds[2]) {
        cli::cli_abort(c(
          "Invalid scale range in {.val {rules}}.",
          "i" = "Use {.code rev(lo, hi)} with numbers lo < hi, e.g. {.code rules = \"rev(1, 5)\"}."
        ))
      }
    }
    result <- .apply_rev(x, bounds = bounds, var_name = var_name)
    return(finish(result, explicit_labels %||% attr(result, "labels")))
  }

  if (grepl("^dicho(\\(|$)", rules_trimmed)) {
    cut_point <- NULL
    m <- regmatches(rules_trimmed, regexec("^dicho\\(([^)]*)\\)$", rules_trimmed))
    if (rules_trimmed != "dicho") {
      cut_point <- if (length(m[[1]]) == 2L) {
        suppressWarnings(as.numeric(trimws(m[[1]][2])))
      } else {
        NA_real_
      }
      if (is.na(cut_point)) {
        cli::cli_abort(c(
          "Invalid cut-point in {.val {rules}}.",
          "i" = "Use a number, e.g. {.code rules = \"dicho(3)\"}, or {.code \"dicho\"} for a median split."
        ))
      }
    }
    result <- .apply_dicho(x, cut_point = cut_point)
    return(finish(result, explicit_labels))
  }

  if (rules_trimmed == "quart") {
    result <- .apply_quart(x)
    return(finish(result, explicit_labels))
  }

  if (rules_trimmed == "mean") {
    cut_point <- mean(.rec_numeric(x), na.rm = TRUE)
    result <- .apply_dicho(x, cut_point = cut_point)
    return(finish(result, explicit_labels))
  }

  # ---- Standard recoding ----------------------------------------------------

  parsed <- .parse_rec_rules(rules, x)
  result <- .apply_rec_rules(x, parsed)
  unmatched <- attr(result, "unmatched")
  attr(result, "unmatched") <- NULL
  if (length(unmatched) > 0L) {
    n_unmatched <- length(unmatched)
    shown <- if (n_unmatched > 10L) c(utils::head(unmatched, 10L), "...") else unmatched
    cli::cli_warn(c(
      "{n_unmatched} value{?s} of {.var {var_name}} matched no rule and became {.val NA}: {shown}.",
      "i" = "Add {.code else=copy} to keep them (as SPSS's in-place {.code RECODE} does) or {.code else=NA} to confirm."
    ))
  }

  # Apply value labels: explicit val_labels > inline + preserved originals > none
  effective_labels <- NULL

  if (!is.null(val_labels)) {
    # Explicit val_labels always take full precedence
    effective_labels <- explicit_labels
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
        effective_labels <- stats::setNames(as.numeric(names(merged)),
                                            unname(merged))
      }
    } else if (length(inline_labels) > 0L) {
      effective_labels <- stats::setNames(as.numeric(names(inline_labels)),
                                          unname(inline_labels))
    }
  }

  finish(result, effective_labels)
}


#' Numeric view of a rec() input: bare numbers (tagged-NA payloads kept),
#' factor codes, or character parsed as numbers
#' @noRd
.rec_numeric <- function(x) {
  if (is.character(x)) return(suppressWarnings(as.numeric(x)))
  as.double(.plain_numeric(x))
}


# ============================================================================
# Internal: Parse recoding rules string
# ============================================================================

#' @noRd
.parse_rec_rules <- function(rules, x) {
  # Split by semicolons that are not inside an inline [label]
  parts <- trimws(strsplit(rules, ";(?![^\\[]*\\])", perl = TRUE)[[1]])
  parts <- parts[nzchar(parts)]

  if (length(parts) == 0L) {
    cli::cli_abort("No valid rules found in {.arg rules}.")
  }

  x_num <- .rec_numeric(x)
  x_min <- suppressWarnings(min(x_num, na.rm = TRUE))
  x_max <- suppressWarnings(max(x_num, na.rm = TRUE))

  parsed <- list()

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
    # A comma-separated list ("1,2=1", "1, 4:5=2") is shorthand for one rule
    # per element with the same new value, like SPSS's RECODE (1,2=1).
    elements <- trimws(strsplit(lhs, ",", fixed = TRUE)[[1]])
    if (length(elements) == 0L || any(!nzchar(elements))) {
      cli::cli_abort("Invalid old value list {.val {lhs}} in rule {.val {part}}.")
    }

    for (el in elements) {
      rule <- list(new = new_val, label = inline_label)

      if (toupper(el) == "ELSE") {
        if (length(elements) > 1L) {
          cli::cli_abort("{.val else} cannot be part of a value list in rule {.val {part}}.")
        }
        parsed[[length(parsed) + 1L]] <- c(list(type = "else"), rule)
        next
      }

      if (toupper(el) == "NA") {
        parsed[[length(parsed) + 1L]] <- c(list(type = "na"), rule)
        next
      }

      # Range "lo:hi"
      if (grepl(":", el, fixed = TRUE)) {
        range_parts <- strsplit(el, ":", fixed = TRUE)[[1]]
        if (length(range_parts) != 2L) {
          cli::cli_abort("Invalid range: {.val {el}} in rule {.val {part}}.")
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
        literal <- toupper(lo_str) != "MIN" && toupper(hi_str) != "MAX"
        if (literal && lo > hi) {
          cli::cli_abort(c(
            "Invalid range {.val {el}} in rule {.val {part}}: the lower bound comes first.",
            "i" = "Did you mean {.val {paste0(hi_str, ':', lo_str)}}?"
          ))
        }

        parsed[[length(parsed) + 1L]] <- c(list(type = "range", from = lo,
                                                to = hi), rule)
        next
      }

      # Single value
      old_val <- suppressWarnings(as.numeric(el))
      if (is.na(old_val)) {
        cli::cli_abort("Invalid old value: {.val {el}} in rule {.val {part}}.")
      }
      parsed[[length(parsed) + 1L]] <- c(list(type = "value", from = old_val),
                                         rule)
    }
  }

  parsed
}


# ============================================================================
# Internal: Apply parsed rules to a vector
# ============================================================================

#' @noRd
.apply_rec_rules <- function(x, parsed) {
  x_num <- .rec_numeric(x)
  # Character form for single-value matching. as.character() rounds to 15
  # significant digits, which absorbs floating-point representation error so
  # decimal codes (e.g. 3.6, or a computed 0.1 + 0.2) match reliably. Mirrors
  # sjmisc::rec(). Ranges below stay numeric (>=/<=), which is already robust.
  x_chr <- as.character(x_num)
  result <- rep(NA_real_, length(x))
  matched <- rep(FALSE, length(x))

  for (rule in parsed) {

    if (rule$type == "na") {
      # Match NA values (including tagged NAs)
      mask <- is.na(x_num) & !matched
      if (any(mask)) {
        if (identical(rule$new, "copy")) {
          # copy on NA → keep NA (including its missing type)
          result[mask] <- x_num[mask]
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
      mask <- available & x_chr == as.character(rule$from)
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

  # Unmatched missing values keep their missing type (tagged NA) like an
  # SPSS in-place RECODE keeps user-missing codes; unmatched valid values
  # become NA.
  keep_na <- !matched & is.na(x_num)
  result[keep_na] <- x_num[keep_na]

  # Valid values no rule matched (set to NA): reported by .rec_vec()
  attr(result, "unmatched") <- sort(unique(x_num[!matched & !is.na(x_num)]))
  result
}


# ============================================================================
# Internal: Reverse a scale
# ============================================================================

#' @noRd
.apply_rev <- function(x, bounds = NULL, var_name = "x") {
  x_num <- .rec_numeric(x)
  valid_labels <- .valid_value_labels(x)
  observed <- range(x_num, na.rm = TRUE, finite = TRUE)
  if (all(is.na(x_num))) observed <- c(NA_real_, NA_real_)

  if (!is.null(bounds)) {
    # Explicit scale range: rev(lo, hi)
    lo <- bounds[1]
    hi <- bounds[2]
    outside <- sort(unique(x_num[!is.na(x_num) & (x_num < lo | x_num > hi)]))
    if (length(outside) > 0L) {
      cli::cli_warn(c(
        "{.var {var_name}} has {cli::qty(length(outside))}value{?s} outside the scale range {lo}-{hi}: {outside}.",
        "i" = "They are reversed as well ({lo} + {hi} - x); recode them first (e.g. to {.val NA}) if they are not part of the scale."
      ))
    }
  } else if (length(valid_labels) > 0L) {
    # Scale range: the labelled codes (the questionnaire's scale points)
    # together with the observed values. The observed range alone reversed
    # a 1-5 item answered only with 2-5 as 5..2 instead of 4..1.
    rng <- range(c(unname(valid_labels), observed), na.rm = TRUE)
    lo <- rng[1]
    hi <- rng[2]
    if (!isTRUE(all(observed == rng))) {
      cli::cli_inform(c(
        "i" = "Reversing {.var {var_name}} on the scale {lo}-{hi} defined by its value labels (observed {observed[1]}-{observed[2]})."
      ))
    }
  } else {
    lo <- observed[1]
    hi <- observed[2]
    if (!is.na(lo)) {
      cli::cli_inform(c(
        "i" = "Reversing {.var {var_name}} around its observed range {lo}-{hi} (no value labels define the scale).",
        " " = "Set the scale range explicitly if it differs, e.g. {.code rules = \"rev(1, 5)\"}."
      ))
    }
  }

  # Arithmetic keeps the tagged-NA payloads (missing types) of x
  result <- if (is.na(lo)) x_num else hi + lo - x_num

  # Mirror the valid value labels (missing labels are re-attached by
  # .with_label_meta())
  if (length(valid_labels) > 0L && !is.na(lo)) {
    attr(result, "labels") <- stats::setNames(
      hi + lo - unname(valid_labels), names(valid_labels)
    )
  }

  result
}


#' Value labels of the valid (non-missing) codes
#'
#' Drops tagged-NA label entries and codes declared missing (na_tag_map,
#' or na_values / na_range of a haven_labelled_spss vector).
#' @noRd
.valid_value_labels <- function(x) {
  labels <- attr(x, "labels", exact = TRUE)
  if (is.null(labels) || !is.numeric(labels)) return(NULL)
  vals <- as.double(.plain_numeric(labels))
  keep <- !is.na(vals)
  tag_map <- attr(x, "na_tag_map", exact = TRUE)
  if (is.numeric(tag_map)) keep <- keep & !(vals %in% unname(tag_map))
  na_values <- attr(x, "na_values", exact = TRUE)
  if (!is.null(na_values)) keep <- keep & !(vals %in% na_values)
  na_range <- attr(x, "na_range", exact = TRUE)
  if (length(na_range) == 2L) {
    keep <- keep & !(vals >= na_range[1] & vals <= na_range[2])
  }
  stats::setNames(vals[keep], names(labels)[keep])
}


# ============================================================================
# Internal: Dichotomize
# ============================================================================

#' @noRd
.apply_dicho <- function(x, cut_point = NULL) {
  x_num <- .rec_numeric(x)

  if (is.null(cut_point)) {
    cut_point <- stats::median(x_num, na.rm = TRUE)
  }

  # Missing positions keep their (tagged) NA
  result <- x_num
  ok <- !is.na(x_num)
  result[ok] <- ifelse(x_num[ok] <= cut_point, 0, 1)
  result
}


# ============================================================================
# Internal: Quartile split
# ============================================================================

#' @noRd
.apply_quart <- function(x) {
  x_num <- .rec_numeric(x)

  q <- stats::quantile(x_num, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

  # Missing positions keep their (tagged) NA
  result <- x_num
  ok <- !is.na(x_num)
  v <- x_num[ok]
  result[ok] <- ifelse(v <= q[1], 1, ifelse(v <= q[2], 2,
                                             ifelse(v <= q[3], 3, 4)))
  result
}


# ============================================================================
# Internal: Convert to factor
# ============================================================================

#' @noRd
.rec_to_factor <- function(x, labels = NULL) {
  # Levels in code order (as SPSS), named by the value labels of the result
  # (haven form: names = text, values = codes). Values without a label keep
  # their code as level name instead of silently becoming NA; duplicate
  # label texts get their code appended so distinct values never merge.
  raw <- .plain_numeric(x)
  if (!is.null(labels)) labels <- labels[!is.na(labels)]
  codes <- sort(unique(c(as.double(.plain_numeric(labels)), raw[!is.na(raw)])))
  lv <- as.character(codes)
  hit <- match(codes, as.double(.plain_numeric(labels)))
  lv[!is.na(hit)] <- names(labels)[hit[!is.na(hit)]]
  dup <- lv %in% lv[duplicated(lv)]
  lv[dup] <- paste0(lv[dup], " (", codes[dup], ")")

  result <- factor(match(raw, codes), levels = seq_along(codes), labels = lv)
  # Original codes for the to_numeric()/to_labelled() round trip
  attr(result, "codes") <- stats::setNames(codes, lv)

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
#'   Set to a specific value for n-1 coding (e.g., for regression). For a
#'   factor, give a level name (\code{ref = "Male"}) or a number: the
#'   original code of a \code{\link{to_label}()} factor, otherwise the level
#'   position (\code{ref = 1} = first level). A \code{ref} that matches no
#'   category is an error.
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
#' where labels are cleaned (umlauts transliterated, e.g. "männlich" ->
#' \code{maennlich}; spaces replaced with \code{_}; other special
#' characters removed). Values without a usable label (e.g. "..") use their
#' value; labels shared by several values get the value appended, so every
#' category keeps its own column.
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

  vars <- .process_variables(data, ..., drop_groups = FALSE)
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
  is_factor <- is.factor(x)
  if (is_factor) {
    vals <- levels(x)
    keys <- as.integer(x)                 # position of the level
    val_keys <- seq_along(vals)
  } else {
    raw <- if (is.numeric(x)) as.double(.plain_numeric(x)) else x
    vals <- sort(unique(raw[!is.na(raw)]))
    keys <- match(raw, vals)
    val_keys <- seq_along(vals)
  }

  # Column suffixes: raw values, or cleaned value labels
  col_suffix <- as.character(vals)
  if (suffix == "label") {
    if (is_factor) {
      lbl <- as.character(vals)
    } else {
      vl <- attr(x, "labels", exact = TRUE)
      lbl <- rep(NA_character_, length(vals))
      if (!is.null(vl)) {
        vl <- vl[!is.na(vl)]
        hit <- match(vals, as.double(.plain_numeric(vl)))
        lbl[!is.na(hit)] <- names(vl)[hit[!is.na(hit)]]
      }
    }
    cleaned <- .clean_label_for_colname(lbl)
    use <- !is.na(cleaned) & nzchar(cleaned)
    col_suffix[use] <- cleaned[use]
    # Labels shared by several values (ALLBUS ".." scale points) or empty
    # after cleaning fall back to / get the value, so no column is lost
    dup <- col_suffix %in% col_suffix[duplicated(col_suffix)]
    col_suffix[dup & use] <- paste0(col_suffix[dup & use], "_",
                                    as.character(vals)[dup & use])
  }

  # Remove reference category
  if (!is.null(ref)) {
    ref_idx <- .dummy_ref_index(x, vals, ref, var_name)
    vals <- vals[-ref_idx]
    val_keys <- val_keys[-ref_idx]
    col_suffix <- col_suffix[-ref_idx]
    if (length(vals) == 0L) {
      cli::cli_abort(
        "Reference value {.val {ref}} removed all categories for variable {.var {var_name}}."
      )
    }
  }

  # Create dummy columns
  result <- tibble::tibble(.rows = length(x))
  for (j in seq_along(vals)) {
    dummy <- as.integer(keys == val_keys[j])
    result[[paste0(var_name, "_", col_suffix[j])]] <- dummy
  }

  result
}


#' Index of the reference category of to_dummy()
#'
#' Factors: a level name, or a number (the original code of a to_label()
#' factor, otherwise the level position). Other vectors: a value. A ref
#' that matches nothing is an error (it used to be ignored silently).
#' @noRd
.dummy_ref_index <- function(x, vals, ref, var_name) {
  if (length(ref) != 1L || is.na(ref)) {
    cli::cli_abort("{.arg ref} must be a single value.")
  }
  idx <- NA_integer_
  if (is.factor(x)) {
    if (is.character(ref)) {
      idx <- match(ref, vals)
    } else if (is.numeric(ref)) {
      codes <- attr(x, "codes", exact = TRUE)
      if (!is.null(codes) && identical(names(codes), levels(x)) &&
          ref %in% codes) {
        idx <- match(ref, codes)
      } else if (ref == round(ref) && ref >= 1 && ref <= length(vals)) {
        idx <- as.integer(ref)
      }
    }
  } else {
    idx <- match(as.character(ref), as.character(vals))
  }
  if (is.na(idx)) {
    shown <- if (is.factor(x)) {
      paste0("a level name (", paste(utils::head(vals, 6), collapse = ", "),
             if (length(vals) > 6) ", ..." else "", ") or a position 1-",
             length(vals))
    } else {
      paste0("one of the values ", paste(utils::head(vals, 10), collapse = ", "),
             if (length(vals) > 10) ", ..." else "")
    }
    cli::cli_abort(c(
      "{.arg ref} = {.val {ref}} is not a category of {.var {var_name}}.",
      "i" = "Use {shown}."
    ))
  }
  idx
}


# ============================================================================
# Internal: Clean a label for use as column name
# ============================================================================

#' @noRd
.clean_label_for_colname <- function(label) {
  # Transliterate German umlauts ("männlich" -> "maennlich", not
  # "mnnlich"), then other accented letters via iconv where available
  out <- label
  from <- c("\u00e4", "\u00f6", "\u00fc", "\u00c4", "\u00d6", "\u00dc",
            "\u00df")
  to <- c("ae", "oe", "ue", "Ae", "Oe", "Ue", "ss")
  for (k in seq_along(from)) out <- gsub(from[k], to[k], out, fixed = TRUE)
  ascii <- suppressWarnings(iconv(out, from = "UTF-8", to = "ASCII//TRANSLIT",
                                  sub = ""))
  out <- ifelse(is.na(ascii), out, ascii)
  # Replace spaces with underscores
  out <- gsub("\\s+", "_", out)
  # Remove anything that's not alphanumeric or underscore
  out <- gsub("[^A-Za-z0-9_]", "", out)
  # Remove leading/trailing underscores
  out <- gsub("^_+|_+$", "", out)
  # Collapse multiple underscores
  out <- gsub("_+", "_", out)
  out
}
