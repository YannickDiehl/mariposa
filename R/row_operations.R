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
#' @param data Your survey data (a data frame or tibble). When used inside
#'   \code{mutate()}, pass \code{.} or use \code{pick()}.
#' @param ... The variables to average. Use bare column names separated by
#'   commas, or tidyselect helpers like \code{starts_with("trust")}. If no
#'   variables are specified, all numeric columns in \code{data} are used
#'   (useful with \code{pick()}).
#' @param min_valid Minimum number of non-missing values required to compute
#'   a mean. If a row has fewer valid values, \code{NA} is returned. Default
#'   is \code{NULL} (compute mean if at least 1 value is valid).
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
#' # Create a trust scale from 3 items
#' survey_data <- survey_data %>%
#'   mutate(m_trust = row_means(., trust_government, trust_media, trust_science))
#'
#' # Using pick()
#' survey_data <- survey_data %>%
#'   mutate(m_trust = row_means(pick(starts_with("trust"))))
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
#' @param count The value to count.
#' @param na.rm If \code{TRUE} (default), \code{NA} values are ignored.
#'   If \code{FALSE}, any row containing \code{NA} returns \code{NA}.
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

  mat <- .row_op_matrix(data, ...)

  matches <- mat == count

  if (isTRUE(na.rm)) {
    # NA == count gives NA; treat as "not a match"
    matches[is.na(matches)] <- FALSE
    as.integer(rowSums(matches))
  } else {
    # If any value in the row is NA, return NA
    has_na <- rowSums(is.na(mat)) > 0L
    result <- as.integer(rowSums(matches, na.rm = TRUE))
    result[has_na] <- NA_integer_
    result
  }
}


# ============================================================================
# Internal Helpers
# ============================================================================

#' Build a numeric matrix from data + tidyselect
#' @noRd
.row_op_matrix <- function(data, ...) {
  dots <- rlang::enquos(...)

  if (length(dots) == 0L) {
    # No variables specified — use all numeric columns (for pick() pattern)
    numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
    if (length(numeric_cols) == 0L) {
      cli::cli_abort("No numeric variables found in {.arg data}.")
    }
    as.matrix(data[, numeric_cols, drop = FALSE])
  } else {
    vars <- .process_variables(data, ...)
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


#' Compute row-wise aggregate (mean or sum) with min_valid support
#' @noRd
.row_aggregate <- function(mat, fun = c("mean", "sum"), min_valid = NULL,
                           na.rm = TRUE) {
  fun <- match.arg(fun)
  n_items <- ncol(mat)

  # Validate min_valid
  if (!is.null(min_valid)) {
    if (!is.numeric(min_valid) || length(min_valid) != 1L || min_valid < 1L) {
      cli::cli_abort("{.arg min_valid} must be a positive integer.")
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
