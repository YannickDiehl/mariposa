
#' Create a Mean Index Across Items
#'
#' @description
#' \code{scale_index()} calculates the mean across multiple items for each
#' respondent - the standard way to create scale scores from survey items.
#' This is the R equivalent of SPSS's \code{COMPUTE m_X = MEAN(var1, var2, var3)}.
#'
#' For example, if you measured trust with 3 items (trust in government,
#' trust in media, trust in science), \code{scale_index()} creates a single
#' trust score by averaging across those items.
#'
#' @param data Your survey data (a data frame or tibble). When used inside
#'   \code{mutate()}, pass \code{.} or use \code{pick()}.
#' @param ... The items to average. Use bare column names separated by commas,
#'   or tidyselect helpers like \code{starts_with("trust")}. If no variables
#'   are specified, all numeric columns in \code{data} are used (useful with
#'   \code{pick()}).
#' @param min_valid Minimum number of non-missing items required to compute a
#'   mean. If a respondent has fewer valid items, \code{NA} is returned for that
#'   row. This works like SPSS's \code{MEAN.x()} syntax: \code{min_valid = 2}
#'   corresponds to \code{MEAN.2(a, b, c)} in SPSS. Default is \code{NULL}
#'   (compute mean if at least 1 value is valid).
#' @param na.rm Remove missing values before calculating? (Default: TRUE)
#'
#' @return A numeric vector with one value per row - the mean across the
#'   selected items. Use inside \code{dplyr::mutate()} to add it as a new
#'   column to your data.
#'
#' @details
#' ## How It Works
#'
#' For each respondent (row), \code{scale_index()} computes the arithmetic
#' mean of the selected items. Missing values are ignored by default, so a
#' respondent who answered 2 out of 3 items still gets a score.
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
#' Use \code{scale_index()} after checking reliability with
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
#'   mutate(m_trust = scale_index(., trust_government, trust_media, trust_science))
#'
#' # Using pick() - works with both %>% and |> pipes
#' survey_data <- survey_data %>%
#'   mutate(m_trust = scale_index(
#'     pick(trust_government, trust_media, trust_science)
#'   ))
#'
#' # Using tidyselect helpers
#' survey_data <- survey_data %>%
#'   mutate(m_trust = scale_index(., starts_with("trust")))
#'
#' # Require at least 2 valid items (like SPSS MEAN.2)
#' survey_data <- survey_data %>%
#'   mutate(m_trust = scale_index(., trust_government, trust_media,
#'                                trust_science, min_valid = 2))
#'
#' # Standalone usage (returns vector)
#' scores <- scale_index(survey_data, trust_government, trust_media, trust_science)
#'
#' @seealso
#' \code{\link{pomps}} for transforming scales to Percent of Maximum Possible
#' Scores.
#'
#' @family scale
#' @export
scale_index <- function(data, ..., min_valid = NULL, na.rm = TRUE) {

  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================

  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame or tibble.")
  }

  # ============================================================================
  # VARIABLE SELECTION
  # ============================================================================

  # Check if ... is empty (e.g., when used with pick())
  dots <- rlang::enquos(...)

  if (length(dots) == 0) {
    # No variables specified - use all numeric columns
    # This enables the pick() pattern: scale_index(pick(var1, var2, var3))
    numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
    if (length(numeric_cols) == 0) {
      cli_abort("No numeric variables found in {.arg data}.")
    }
    mat <- as.matrix(data[, numeric_cols, drop = FALSE])
  } else {
    # tidyselect mode
    vars <- .process_variables(data, ...)
    var_names <- names(vars)

    # Validate all selected variables are numeric
    for (var_name in var_names) {
      if (!is.numeric(data[[var_name]])) {
        cli_abort(
          "Variable {.var {var_name}} is not numeric. {.fn scale_index} requires numeric items."
        )
      }
    }

    mat <- as.matrix(data[, var_names, drop = FALSE])
  }

  # ============================================================================
  # VALIDATE min_valid
  # ============================================================================

  n_items <- ncol(mat)

  if (!is.null(min_valid)) {
    if (!is.numeric(min_valid) || length(min_valid) != 1 || min_valid < 1) {
      cli_abort("{.arg min_valid} must be a positive integer.")
    }
    if (min_valid > n_items) {
      cli_warn(
        "{.arg min_valid} ({min_valid}) is greater than the number of items ({n_items}). All rows will be {.val NA}."
      )
    }
  }

  # ============================================================================
  # CALCULATE ROW MEANS
  # ============================================================================

  # Count valid (non-NA) values per row
  n_valid <- rowSums(!is.na(mat))

  # Calculate row means
  means <- rowMeans(mat, na.rm = na.rm)

  # Rows where all values are NA produce NaN with rowMeans(na.rm=TRUE)
  means[n_valid == 0] <- NA_real_

  # Apply min_valid constraint
  if (!is.null(min_valid)) {
    means[n_valid < min_valid] <- NA_real_
  }

  return(means)
}


#' Transform Scores to Percent of Maximum Possible (POMPS)
#'
#' @description
#' \code{pomps()} transforms scores to a 0-100 scale using the Percent of
#' Maximum Possible Scores method. This makes different scales directly
#' comparable regardless of their original range.
#'
#' This is the R equivalent of the SPSS formula:
#' \code{COMPUTE v81p = ((v81 - 1) / (7 - 1)) * 100}.
#'
#' A score of 0 means the minimum possible score, 100 means the maximum.
#' The transformation preserves all correlations between variables.
#'
#' @param x A numeric vector to transform (e.g., a column from your data).
#' @param scale_min The theoretical minimum of the scale. If \code{NULL}
#'   (default), the observed minimum of \code{x} is used.
#' @param scale_max The theoretical maximum of the scale. If \code{NULL}
#'   (default), the observed maximum of \code{x} is used.
#'
#' @return A numeric vector of the same length as \code{x}, with values
#'   rescaled to the 0-100 range.
#'
#' @details
#' ## The Formula
#'
#' POMPS = ((score - scale_min) / (scale_max - scale_min)) * 100
#'
#' ## Why Specify scale_min and scale_max?
#'
#' By default, \code{pomps()} uses the observed minimum and maximum of your
#' data. However, for Likert scales you should specify the \emph{theoretical}
#' range:
#'
#' \itemize{
#'   \item A 1-5 Likert scale: \code{scale_min = 1, scale_max = 5}
#'   \item A 1-7 Likert scale: \code{scale_min = 1, scale_max = 7}
#'   \item A 0-10 scale: \code{scale_min = 0, scale_max = 10}
#' }
#'
#' Using theoretical values ensures that the transformation is consistent
#' across samples and time points.
#'
#' ## When to Use This
#'
#' \itemize{
#'   \item Comparing variables measured on different scales
#'   \item Creating profile plots across scales with different ranges
#'   \item Reporting scale scores in an intuitive 0-100 format
#' }
#'
#' @examples
#' library(dplyr)
#' data(survey_data)
#'
#' # Transform a 1-5 Likert scale to POMPS
#' survey_data <- survey_data %>%
#'   mutate(trust_gov_pomps = pomps(trust_government, scale_min = 1, scale_max = 5))
#'
#' # Transform multiple variables with the same scale
#' survey_data <- survey_data %>%
#'   mutate(across(
#'     c(trust_government, trust_media, trust_science),
#'     ~ pomps(.x, scale_min = 1, scale_max = 5),
#'     .names = "{.col}_pomps"
#'   ))
#'
#' # Auto-detect range (uses observed min/max)
#' survey_data <- survey_data %>%
#'   mutate(age_pomps = pomps(age))
#'
#' @seealso
#' \code{\link{scale_index}} for creating mean indices across items.
#'
#' @family scale
#' @export
pomps <- function(x, scale_min = NULL, scale_max = NULL) {

  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================

  if (!is.numeric(x)) {
    cli_abort("{.arg x} must be a numeric vector.")
  }

  # ============================================================================
  # DETERMINE SCALE RANGE
  # ============================================================================

  if (is.null(scale_min)) {
    scale_min <- min(x, na.rm = TRUE)
  }

  if (is.null(scale_max)) {
    scale_max <- max(x, na.rm = TRUE)
  }

  if (scale_min >= scale_max) {
    cli_abort("{.arg scale_min} ({scale_min}) must be less than {.arg scale_max} ({scale_max}).")
  }

  # ============================================================================
  # POMPS TRANSFORMATION
  # ============================================================================

  result <- ((x - scale_min) / (scale_max - scale_min)) * 100

  return(result)
}
