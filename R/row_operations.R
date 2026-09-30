# ============================================================================
# Row-Wise Operations
# ============================================================================
# Functions for computing row-level aggregates across selected columns.
# Designed for use inside dplyr::mutate() with tidyselect support.


# ============================================================================
# row_means() — Row-Wise Means
# ============================================================================

#' Compute Row Means Across Items
#'
#' @description
#' Calculates the mean across multiple variables for each row. This is the
#' standard way to create scale scores from survey items — the R equivalent
#' of SPSS's \code{COMPUTE score = MEAN(var1, var2, var3)}.
#'
#' Use \code{min_valid} to require a minimum number of non-missing items,
#' mirroring SPSS's \code{MEAN.n()} syntax: \code{min_valid = 2} corresponds
#' to \code{MEAN.2(a, b, c)}.
#'
#' @param data Your survey data (a data frame or tibble). Inside
#'   \code{mutate()}, the recommended form is \code{pick()}:
#'   \code{mutate(score = row_means(pick(item1, item2, item3)))}. It also
#'   works in a grouped \code{mutate()}; \code{.} (the magrittr placeholder)
#'   is the whole data set and therefore only works without groups.
#' @param ... The variables to average. Use bare column names separated by
#'   commas, or tidyselect helpers like \code{starts_with("trust")}. If no
#'   variables are specified, all numeric columns in \code{data} are used
#'   (useful with \code{pick()}; non-numeric columns are ignored with a
#'   warning).
#' @param min_valid Minimum number of non-missing values required to compute
#'   a mean (a whole number). If a row has fewer valid values, \code{NA} is
#'   returned. Default is \code{NULL} (compute mean if at least 1 value is
#'   valid).
#' @param na.rm Remove missing values before calculating? Default: \code{TRUE}.
#'
#' @return A numeric vector with one value per row — the mean across the
#'   selected variables. Use inside \code{dplyr::mutate()} to add it as a
#'   new column.
#'
#' @details
#' ## How It Works
#'
#' For each row, \code{row_means()} computes the arithmetic mean of the
#' selected variables. Missing values are ignored by default, so a respondent
#' who answered 2 out of 3 items still gets a score.
#'
#' ## The min_valid Parameter
#'
#' In practice, you often want to require a minimum number of valid responses.
#' If someone only answered 1 out of 5 items, a mean based on a single item
#' may not be reliable. Set \code{min_valid} to control this:
#'
#' \itemize{
#'   \item \code{min_valid = NULL} (default): Compute mean with any number of
#'     valid values (at least 1)
#'   \item \code{min_valid = 2}: Require at least 2 valid values
#'   \item \code{min_valid = 3}: Require at least 3 valid values
#' }
#'
#' ## When to Use This
#'
#' Use \code{row_means()} after checking reliability with
#' \code{\link{reliability}}:
#' \enumerate{
#'   \item Run \code{reliability()} to check if items form a reliable scale
#'   \item If Cronbach's Alpha is acceptable (typically > .70), create the index
#'   \item Use the index in further analyses (t-tests, correlations, regression)
#' }
#'
#' @examples
#' library(dplyr)
#' data(survey_data)
#'
#' # Create a trust scale from 3 items (recommended: pick())
#' survey_data <- survey_data %>%
#'   mutate(m_trust = row_means(pick(trust_government, trust_media,
#'                                   trust_science)))
#'
#' # tidyselect helpers inside pick(); also works after group_by()
#' survey_data <- survey_data %>%
#'   group_by(region) %>%
#'   mutate(m_trust = row_means(pick(starts_with("trust")))) %>%
#'   ungroup()
#'
#' # Alternative without groups: the data placeholder `.`
#' survey_data <- survey_data %>%
#'   mutate(m_trust = row_means(., trust_government, trust_media, trust_science))
#'
#' # Require at least 2 valid items (like SPSS MEAN.2)
#' survey_data <- survey_data %>%
#'   mutate(m_trust = row_means(., trust_government, trust_media,
#'                              trust_science, min_valid = 2))
#'
#' @seealso [row_sums()] for row-wise sums, [row_count()] for counting
#'   specific values, [pomps()] for rescaling to 0-100
#'
#' @family scale
#' @export
row_means <- function(data, ..., min_valid = NULL, na.rm = TRUE) {

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame or tibble.")
  }

  mat <- .row_op_matrix(data, ...)

  .row_aggregate(mat, fun = "mean", min_valid = min_valid, na.rm = na.rm)
}


# ============================================================================
# row_sums() — Row-Wise Sums
# ============================================================================

#' Compute Row Sums Across Items
#'
#' @description
#' Calculates the sum across multiple variables for each row. This is the
#' R equivalent of SPSS's \code{COMPUTE total = SUM(var1, var2, var3)}.
#'
#' Use \code{min_valid} to require a minimum number of non-missing items,
#' mirroring SPSS's \code{SUM.n()} syntax.
#'
#' @inheritParams row_means
#'
#' @return A numeric vector with one value per row — the sum across the
#'   selected variables.
#'
#' @details
#' ## When to Use row_sums() vs row_means()
#'
#' \itemize{
#'   \item \code{row_means()}: For Likert-type scales where you want an
#'     average score (preserves the original scale range)
#'   \item \code{row_sums()}: For count-based scores (e.g., number of
#'     symptoms endorsed) or when you need a total score
#' }
#'
#' @examples
#' library(dplyr)
#' data(survey_data)
#'
#' # Total score across items
#' survey_data <- survey_data %>%
#'   mutate(total = row_sums(., trust_government, trust_media, trust_science))
#'
#' # With min_valid (like SPSS SUM.3)
#' survey_data <- survey_data %>%
#'   mutate(total = row_sums(., trust_government, trust_media,
#'                           trust_science, min_valid = 3))
#'
#' @seealso [row_means()] for row-wise means, [row_count()] for counting
#'   specific values
#'
#' @family scale
#' @export
row_sums <- function(data, ..., min_valid = NULL, na.rm = TRUE) {

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame or tibble.")
  }

  mat <- .row_op_matrix(data, ...)

  .row_aggregate(mat, fun = "sum", min_valid = min_valid, na.rm = na.rm)
}


# ============================================================================
# row_count() — Count Specific Values Per Row
# ============================================================================

#' Count Occurrences of a Value Across Columns
#'
#' @description
#' Counts how often a specific value appears in each row across the selected
#' variables. Useful for data quality checks (e.g., "How many items did a
#' respondent answer with -9?") or for creating count-based indices.
#'
#' @param data Your survey data (a data frame or tibble).
#' @param ... The variables to check. Supports tidyselect.
#' @param count The value(s) to count: one value (\code{count = 5}) or a
#'   set (\code{count = c(4, 5)} counts cells equal to 4 or 5, like SPSS's
#'   \code{COUNT n = v1 TO v5 (4, 5)}). \code{NA} counts missing values
#'   (SPSS keyword \code{MISSING}). Missing codes of imported data (e.g.
#'   -9 read by \code{\link{read_spss}()} as a tagged NA) are counted when
#'   listed, as SPSS's \code{COUNT} counts user-missing values.
#' @param na.rm If \code{TRUE} (default), \code{NA} values are ignored.
#'   If \code{FALSE}, any row containing a missing value that is not
#'   counted returns \code{NA}.
#'
#' @return An integer vector with one value per row — the count of how
#'   often \code{count} appears.
#'
#' @examples
#' library(dplyr)
#' data(survey_data)
#'
#' # How many items did each respondent answer with the highest value (5)?
#' survey_data <- survey_data %>%
#'   mutate(n_top = row_count(., trust_government, trust_media,
#'                            trust_science, count = 5))
#'
#' # Top-2 box (4 or 5) and number of missing answers per respondent
#' survey_data <- survey_data %>%
#'   mutate(n_top2 = row_count(., trust_government, trust_media,
#'                             trust_science, count = c(4, 5)),
#'          n_miss = row_count(., trust_government, trust_media,
#'                             trust_science, count = NA))
#'
#' @seealso [row_sums()] for row-wise sums, [row_means()] for row-wise means
#'
#' @family scale
#' @export
row_count <- function(data, ..., count, na.rm = TRUE) {

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame or tibble.")
  }

  if (missing(count)) {
    cli::cli_abort("{.arg count} is required. Specify the value to count.")
  }
  if (length(count) == 0L || !(is.numeric(count) || all(is.na(count)))) {
    cli::cli_abort(c(
      "{.arg count} must be one or more numeric values (or {.val NA}).",
      "i" = "For example {.code count = 5}, {.code count = c(4, 5)} or {.code count = NA}."
    ))
  }

  mat <- .row_op_matrix(data, ...)

  # Missing codes of imported data (tagged NAs, e.g. -9) are counted when
  # listed in `count`, like SPSS's COUNT counts user-missing values: match
  # against the original codes. NA in `count` counts every missing value
  # (SPSS keyword MISSING).
  codes <- .row_op_codes(data, colnames(mat), mat)
  count_vals <- as.double(count[!is.na(count)])
  matches <- matrix(codes %in% count_vals, nrow = nrow(mat))
  if (anyNA(count)) matches <- matches | is.na(mat)

  result <- as.integer(rowSums(matches))
  if (!isTRUE(na.rm) && !anyNA(count)) {
    # A row with a value that is missing and not counted returns NA
    has_na <- rowSums(is.na(codes)) > 0L
    result[has_na] <- NA_integer_
  }
  result
}


# ============================================================================
# Internal Helpers
# ============================================================================

#' Build a numeric matrix from data + tidyselect
#' @noRd
.row_op_matrix <- function(data, ..., call = rlang::caller_env()) {
  dots <- rlang::enquos(...)
  fn <- rlang::call_name(rlang::frame_call(call)) %||% "row_means"

  # Inside a grouped mutate(), `.` is the whole data set while the result
  # must have one value per row of the current group: dplyr then aborts
  # with a size-mismatch error that does not say what to do.
  n_group <- tryCatch(length(dplyr::cur_group_rows()),
                      error = function(e) NULL)
  if (!is.null(n_group) && nrow(data) != n_group) {
    cli::cli_abort(c(
      "{.fn {fn}} received {nrow(data)} rows, but the current group of {.fn mutate} has {n_group}.",
      "i" = "In a grouped {.fn mutate}, {.code .} is the whole data set. Use {.code pick()} instead, e.g. {.code mutate(score = {fn}(pick(item1, item2, item3)))}."
    ), call = call)
  }

  if (length(dots) == 0L) {
    # No variables specified — use all numeric columns (for pick() pattern)
    is_num <- vapply(data, is.numeric, logical(1))
    numeric_cols <- names(data)[is_num]
    if (length(numeric_cols) == 0L) {
      cli::cli_abort("No numeric variables found in {.arg data}.", call = call)
    }
    if (any(!is_num)) {
      dropped <- names(data)[!is_num]
      cli::cli_warn(c(
        "{.fn {fn}} ignored non-numeric column{?s} {.var {dropped}}.",
        "i" = "Only numeric columns are aggregated; leave {cli::qty(length(dropped))}{?it/them} out of {.code pick()} to silence this."
      ), call = call)
    }
    as.matrix(data[, numeric_cols, drop = FALSE])
  } else {
    # Row-wise, not per group: a grouping variable may be part of the row
    vars <- .process_variables(data, ..., drop_groups = FALSE, call = call)
    var_names <- names(vars)

    for (var_name in var_names) {
      if (!is.numeric(data[[var_name]])) {
        cli::cli_abort(
          "Variable {.var {var_name}} is not numeric."
        )
      }
    }

    as.matrix(data[, var_names, drop = FALSE])
  }
}


#' Matrix of original codes: tagged NAs of imported columns replaced by
#' their missing codes (na_tag_map), everything else as in `mat`
#' @noRd
.row_op_codes <- function(data, var_names, mat) {
  codes <- mat
  for (j in seq_along(var_names)) {
    x <- data[[var_names[j]]]
    if (is.numeric(attr(x, "na_tag_map", exact = TRUE))) {
      codes[, j] <- as.double(.plain_numeric(untag_na(x)))
    }
  }
  codes
}


#' Compute row-wise aggregate (mean or sum) with min_valid support
#' @noRd
.row_aggregate <- function(mat, fun = c("mean", "sum"), min_valid = NULL,
                           na.rm = TRUE) {
  fun <- match.arg(fun)
  n_items <- ncol(mat)

  # Validate min_valid
  if (!is.null(min_valid)) {
    if (!is.numeric(min_valid) || length(min_valid) != 1L ||
        is.na(min_valid) || min_valid < 1L || min_valid != round(min_valid)) {
      cli::cli_abort(c(
        "{.arg min_valid} must be a positive whole number of items.",
        "x" = "Got {.val {min_valid}}."
      ))
    }
    if (min_valid > n_items) {
      cli::cli_warn(
        "{.arg min_valid} ({min_valid}) is greater than the number of items ({n_items}). All rows will be {.val NA}."
      )
    }
  }

  # Count valid (non-NA) values per row
  n_valid <- rowSums(!is.na(mat))

  # Compute aggregate
  if (fun == "mean") {
    result <- rowMeans(mat, na.rm = na.rm)
  } else {
    result <- rowSums(mat, na.rm = na.rm)
  }

  # Rows where all values are NA produce NaN (mean) or 0 (sum)
  result[n_valid == 0L] <- NA_real_

  # Apply min_valid constraint
  if (!is.null(min_valid)) {
    result[n_valid < min_valid] <- NA_real_
  }

  result
}
