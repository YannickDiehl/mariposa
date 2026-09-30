
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
#'   rescaled to the 0-100 range. The variable label of \code{x} is kept;
#'   missing values (including the SPSS missing types of imported data)
#'   become plain \code{NA}, as in an SPSS \code{COMPUTE}.
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
#' across samples and time points. Values outside the range (e.g. an
#' unrecoded "don't know" = 9 on a 1-5 scale) give scores below 0 or above
#' 100; \code{pomps()} warns about them, so recode missing codes first.
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
#' \code{\link{row_means}} for creating mean indices across items.
#'
#' @family scale
#' @export
pomps <- function(x, scale_min = NULL, scale_max = NULL) {

  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================

  x_name <- sub(".*\\$", "", deparse(substitute(x))[1])

  if (!is.numeric(x)) {
    cli_abort("{.arg x} must be a numeric vector.")
  }

  # Bare numbers: missing values of imported variables (tagged NAs) become
  # plain NA, like an SPSS COMPUTE (system-missing result). Arithmetic on
  # the labelled vector kept the tag payloads without their code map, which
  # made write_spss() fail.
  raw <- as.double(.plain_numeric(x))
  raw[is.na(raw)] <- NA_real_

  for (arg in c("scale_min", "scale_max")) {
    val <- get(arg)
    if (!is.null(val) &&
        (!is.numeric(val) || length(val) != 1L || !is.finite(val))) {
      cli_abort(c(
        "{.arg {arg}} must be a single finite number.",
        "x" = "Got {.val {format(val)}}."
      ))
    }
  }

  # ============================================================================
  # DETERMINE SCALE RANGE
  # ============================================================================

  if ((is.null(scale_min) || is.null(scale_max)) && all(is.na(raw))) {
    cli_abort(c(
      "{.var {x_name}} has no valid values to derive the scale range from.",
      "i" = "Set {.arg scale_min} and {.arg scale_max} explicitly."
    ))
  }

  if (is.null(scale_min)) {
    scale_min <- min(raw, na.rm = TRUE)
  }

  if (is.null(scale_max)) {
    scale_max <- max(raw, na.rm = TRUE)
  }

  if (scale_min >= scale_max) {
    cli_abort("{.arg scale_min} ({scale_min}) must be less than {.arg scale_max} ({scale_max}).")
  }

  # Values outside the theoretical range (typically an unrecoded
  # "don't know" = 9 on a 1-5 scale) silently gave scores such as -25 or 200
  outside <- sort(unique(raw[!is.na(raw) & (raw < scale_min | raw > scale_max)]))
  if (length(outside) > 0L) {
    cli::cli_warn(c(
      "{.var {x_name}} has {cli::qty(length(outside))}value{?s} outside the scale range {scale_min}-{scale_max}: {outside}.",
      "i" = "They give scores below 0 or above 100. Recode them first (e.g. missing codes to {.val NA} with {.fn set_na} or {.fn rec}) or adjust {.arg scale_min}/{.arg scale_max}."
    ))
  }

  # ============================================================================
  # POMPS TRANSFORMATION
  # ============================================================================

  result <- ((raw - scale_min) / (scale_max - scale_min)) * 100
  attr(result, "label") <- attr(x, "label", exact = TRUE)

  return(result)
}
