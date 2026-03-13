# ============================================================================
# Standardization & Centering
# ============================================================================
# Functions for z-standardizing and mean-centering variables.
# Supports group-by for group-mean centering.


# ============================================================================
# std() — Z-Standardization
# ============================================================================

#' Standardize Variables (Z-Scores)
#'
#' @description
#' Standardizes variables by centering on the mean and dividing by a measure
#' of spread. Supports multiple standardization methods including robust
#' alternatives.
#'
#' When used on a grouped data frame (via \code{dplyr::group_by()}),
#' standardization is performed within each group.
#'
#' @param data A data frame or numeric vector.
#' @param ... Variables to standardize (tidyselect). Only used when \code{data}
#'   is a data frame.
#' @param method Standardization method:
#'   \describe{
#'     \item{\code{"sd"}}{Standard z-score: \code{(x - mean) / sd} (default)}
#'     \item{\code{"2sd"}}{Gelman's 2-SD method: \code{(x - mean) / (2 * sd)}.
#'       Useful in regression with binary predictors.}
#'     \item{\code{"mad"}}{Robust: \code{(x - median) / mad}. Resistant to
#'       outliers.}
#'     \item{\code{"gmd"}}{Gini's Mean Difference: \code{(x - mean) / gmd}.
#'       A robust alternative.}
#'   }
#' @param weights Optional survey weights (unquoted column name or numeric
#'   vector). When provided, weighted mean and weighted SD are used for
#'   standardization. Only supported for methods \code{"sd"} and \code{"2sd"}.
#' @param suffix A character string appended to column names (e.g.,
#'   \code{"_z"}). If \code{NULL} (default), the original columns are
#'   overwritten.
#' @param na.rm Remove missing values before computing mean and SD?
#'   Default: \code{TRUE}.
#'
#' @return If \code{data} is a vector, a standardized numeric vector. If
#'   \code{data} is a data frame, the modified data frame (invisibly).
#'
#' @details
#' ## Standardization Methods
#'
#' \itemize{
#'   \item \strong{sd (default)}: Standard z-transformation. Mean = 0, SD = 1.
#'   \item \strong{2sd}: Divides by 2 standard deviations (Gelman, 2008). This
#'     makes standardized continuous predictors comparable to binary predictors
#'     in regression.
#'   \item \strong{mad}: Uses the Median Absolute Deviation instead of SD.
#'     Robust against outliers.
#'   \item \strong{gmd}: Uses Gini's Mean Difference — a robust spread measure
#'     based on all pairwise absolute differences.
#' }
#'
#' ## Weighted Standardization
#'
#' When \code{weights} is provided, the weighted mean and weighted standard
#' deviation (using SPSS frequency weight formula) are used. This is only
#' supported for methods \code{"sd"} and \code{"2sd"}. The robust methods
#' \code{"mad"} and \code{"gmd"} do not support weights.
#'
#' ## Group-By Standardization
#'
#' When \code{data} is grouped (via \code{group_by()}), standardization is
#' performed separately within each group. This is useful for within-group
#' comparisons. Weights are also subsetted per group.
#'
#' @examples
#' library(dplyr)
#' data(survey_data)
#'
#' # Standard z-scores
#' data <- std(survey_data, age, income, suffix = "_z")
#'
#' # Gelman 2-SD standardization (for regression)
#' data <- std(survey_data, income, age, method = "2sd",
#'             suffix = "_z")
#'
#' # Robust standardization
#' data <- std(survey_data, income, method = "mad", suffix = "_z")
#'
#' # Weighted standardization
#' data <- std(survey_data, income, age,
#'             weights = sampling_weight, suffix = "_z")
#'
#' # Group-wise standardization
#' data <- survey_data %>%
#'   group_by(region) %>%
#'   std(income, suffix = "_z")
#'
#' @seealso [center()] for mean-centering without scaling,
#'   [pomps()] for rescaling to 0-100
#'
#' @family transform
#' @export
std <- function(data, ..., method = "sd", weights = NULL, suffix = NULL,
                na.rm = TRUE) {

  method <- match.arg(method, choices = c("sd", "2sd", "mad", "gmd"))

  # ============================================================================
  # VECTOR INPUT
  # ============================================================================

  weights_quo <- rlang::enquo(weights)

  if (!is.data.frame(data)) {
    if (!is.numeric(data)) {
      cli::cli_abort("{.arg data} must be numeric.")
    }
    w <- if (!rlang::quo_is_null(weights_quo)) rlang::eval_tidy(weights_quo) else NULL
    return(.std_vec(data, method = method, w = w, na.rm = na.rm))
  }

  # ============================================================================
  # DATA FRAME INPUT
  # ============================================================================

  # Resolve weights column
  weights_info <- .process_weights(data, weights_quo)
  w <- weights_info$vector

  # Validate weights + method combination
  if (!is.null(w) && method %in% c("mad", "gmd")) {
    cli::cli_abort(
      "Weighted standardization is not supported for method {.val {method}}. Use {.val sd} or {.val 2sd}."
    )
  }

  vars <- .process_variables(data, ...)
  is_grouped <- inherits(data, "grouped_df")

  for (i in vars) {
    col_name <- names(data)[i]
    out_name <- if (!is.null(suffix)) paste0(col_name, suffix) else col_name

    if (!is.numeric(data[[col_name]])) {
      cli::cli_warn("Skipping non-numeric variable {.var {col_name}}.")
      next
    }

    if (is_grouped) {
      group_indices <- dplyr::group_indices(data)
      result <- data[[col_name]]
      for (g in unique(group_indices)) {
        mask <- group_indices == g
        w_g <- if (!is.null(w)) w[mask] else NULL
        result[mask] <- .std_vec(result[mask], method = method, w = w_g,
                                 na.rm = na.rm)
      }
      data[[out_name]] <- result
    } else {
      data[[out_name]] <- .std_vec(data[[col_name]], method = method, w = w,
                                   na.rm = na.rm)
    }

    # Update variable label
    orig_label <- attr(data[[col_name]], "label", exact = TRUE)
    if (!is.null(orig_label)) {
      attr(data[[out_name]], "label") <- paste0(orig_label, " (standardized)")
    }

    # Remove value labels (z-scores are not categorical)
    attr(data[[out_name]], "labels") <- NULL
  }

  invisible(data)
}


# ============================================================================
# Internal: Standardize a single vector
# ============================================================================

#' @noRd
.std_vec <- function(x, method = "sd", w = NULL, na.rm = TRUE) {

  # Weighted path
  if (!is.null(w) && .validate_weights(w, verbose = FALSE)) {

    if (method %in% c("mad", "gmd")) {
      cli::cli_abort(
        "Weighted standardization is not supported for method {.val {method}}."
      )
    }

    # Align NA removal for x and w
    if (isTRUE(na.rm)) {
      valid <- !is.na(x) & !is.na(w)
      xv <- x[valid]
      wv <- w[valid]
    } else {
      xv <- x
      wv <- w
    }

    w_mean <- sum(xv * wv) / sum(wv)
    V1 <- sum(wv)
    w_var <- sum(wv * (xv - w_mean)^2) / (V1 - 1)
    w_sd <- sqrt(w_var)
    spread <- if (method == "2sd") 2 * w_sd else w_sd

    if (is.na(spread) || spread == 0) {
      cli::cli_warn("Standard deviation is zero or NA. Returning {.val NA}.")
      return(rep(NA_real_, length(x)))
    }

    return((x - w_mean) / spread)
  }

  # Unweighted path
  center <- mean(x, na.rm = na.rm)
  spread <- switch(method,
    sd   = stats::sd(x, na.rm = na.rm),
    `2sd` = 2 * stats::sd(x, na.rm = na.rm),
    mad  = stats::mad(x, na.rm = na.rm),
    gmd  = .gmd(x, na.rm = na.rm)
  )

  if (is.na(spread) || spread == 0) {
    cli::cli_warn("Standard deviation is zero or NA. Returning {.val NA}.")
    return(rep(NA_real_, length(x)))
  }

  if (method == "mad") {
    center <- stats::median(x, na.rm = na.rm)
  }

  (x - center) / spread
}


# ============================================================================
# Internal: Gini's Mean Difference
# ============================================================================

#' @noRd
.gmd <- function(x, na.rm = TRUE) {
  if (isTRUE(na.rm)) x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2L) return(NA_real_)
  mean(abs(outer(x, x, "-")))
}


# ============================================================================
# center() — Mean Centering
# ============================================================================

#' Center Variables (Mean Centering)
#'
#' @description
#' Centers variables by subtracting the mean. When used on a grouped data
#' frame (via \code{dplyr::group_by()}), this becomes group-mean centering —
#' the R equivalent of separate centering within each group.
#'
#' @param data A data frame or numeric vector.
#' @param ... Variables to center (tidyselect). Only used when \code{data}
#'   is a data frame.
#' @param weights Optional survey weights (unquoted column name or numeric
#'   vector). When provided, the weighted mean is subtracted instead of the
#'   unweighted mean.
#' @param suffix A character string appended to column names (e.g.,
#'   \code{"_c"}). If \code{NULL} (default), original columns are overwritten.
#' @param na.rm Remove missing values before computing the mean?
#'   Default: \code{TRUE}.
#'
#' @return If \code{data} is a vector, a centered numeric vector. If
#'   \code{data} is a data frame, the modified data frame (invisibly).
#'
#' @details
#' ## Grand-Mean vs. Group-Mean Centering
#'
#' \itemize{
#'   \item \strong{Grand-mean centering} (ungrouped): Subtracts the overall
#'     mean. A centered value of 0 means the respondent is at the sample
#'     average.
#'   \item \strong{Group-mean centering} (grouped): Subtracts the group mean.
#'     Useful in multilevel models to separate within-group and between-group
#'     effects. This replaces sjmisc's separate \code{de_mean()} function.
#' }
#'
#' ## Weighted Centering
#'
#' When \code{weights} is provided, the weighted mean is used for centering.
#' This accounts for survey design in the centering computation.
#'
#' @examples
#' library(dplyr)
#' data(survey_data)
#'
#' # Grand-mean centering
#' data <- center(survey_data, income, age, suffix = "_c")
#'
#' # Weighted centering
#' data <- center(survey_data, income, age,
#'                weights = sampling_weight, suffix = "_c")
#'
#' # Group-mean centering (replaces sjmisc::de_mean)
#' data <- survey_data %>%
#'   group_by(region) %>%
#'   center(income, age, suffix = "_gmc")
#'
#' @seealso [std()] for full standardization (centering + scaling)
#'
#' @family transform
#' @export
center <- function(data, ..., weights = NULL, suffix = NULL, na.rm = TRUE) {

  # ============================================================================
  # VECTOR INPUT
  # ============================================================================

  weights_quo <- rlang::enquo(weights)

  if (!is.data.frame(data)) {
    if (!is.numeric(data)) {
      cli::cli_abort("{.arg data} must be numeric.")
    }
    w <- if (!rlang::quo_is_null(weights_quo)) rlang::eval_tidy(weights_quo) else NULL
    return(.center_vec(data, w = w, na.rm = na.rm))
  }

  # ============================================================================
  # DATA FRAME INPUT
  # ============================================================================

  # Resolve weights column
  weights_info <- .process_weights(data, weights_quo)
  w <- weights_info$vector

  vars <- .process_variables(data, ...)
  is_grouped <- inherits(data, "grouped_df")

  for (i in vars) {
    col_name <- names(data)[i]
    out_name <- if (!is.null(suffix)) paste0(col_name, suffix) else col_name

    if (!is.numeric(data[[col_name]])) {
      cli::cli_warn("Skipping non-numeric variable {.var {col_name}}.")
      next
    }

    if (is_grouped) {
      group_indices <- dplyr::group_indices(data)
      result <- data[[col_name]]
      for (g in unique(group_indices)) {
        mask <- group_indices == g
        w_g <- if (!is.null(w)) w[mask] else NULL
        result[mask] <- .center_vec(result[mask], w = w_g, na.rm = na.rm)
      }
      data[[out_name]] <- result
    } else {
      data[[out_name]] <- .center_vec(data[[col_name]], w = w, na.rm = na.rm)
    }

    # Update variable label
    orig_label <- attr(data[[col_name]], "label", exact = TRUE)
    if (!is.null(orig_label)) {
      attr(data[[out_name]], "label") <- paste0(orig_label, " (centered)")
    }

    # Remove value labels (centered values are not categorical)
    attr(data[[out_name]], "labels") <- NULL
  }

  invisible(data)
}


# ============================================================================
# Internal: Center a single vector
# ============================================================================

#' @noRd
.center_vec <- function(x, w = NULL, na.rm = TRUE) {
  if (!is.null(w) && .validate_weights(w, verbose = FALSE)) {
    if (isTRUE(na.rm)) {
      valid <- !is.na(x) & !is.na(w)
      xv <- x[valid]
      wv <- w[valid]
    } else {
      xv <- x
      wv <- w
    }
    w_mean <- sum(xv * wv) / sum(wv)
    return(x - w_mean)
  }
  x - mean(x, na.rm = na.rm)
}
