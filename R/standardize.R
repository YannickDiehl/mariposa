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
#' @param weights Optional survey weights. When provided, weighted mean and
#'   weighted SD are used for standardization. Only supported for methods
#'   \code{"sd"} and \code{"2sd"}. Give a column name (unquoted or as a string),
#'   an expression such as \code{sampling_weight * 2}, or a numeric vector with
#'   one weight per row.
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
#' ## Missing Values
#'
#' The result is a plain numeric variable (value labels are dropped, the
#' variable label is kept with " (standardized)" appended). Missing values
#' of imported data, including their SPSS missing types, become plain
#' \code{NA} -- as SPSS's \code{DESCRIPTIVES /SAVE} gives system-missing
#' z-scores.
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
  .check_required("data")

  method <- match.arg(method, choices = c("sd", "2sd", "mad", "gmd"))

  # ============================================================================
  # VECTOR INPUT
  # ============================================================================

  weights_quo <- rlang::enquo(weights)

  if (!is.data.frame(data)) {
    .check_dots_unused(...)
    if (!is.numeric(data)) {
      cli::cli_abort("{.arg data} must be numeric.")
    }
    w <- if (!rlang::quo_is_null(weights_quo)) rlang::eval_tidy(weights_quo) else NULL
    x_name <- sub(".*\\$", "", deparse(substitute(data))[1])
    out <- .std_vec(data, method = method, w = w, na.rm = na.rm,
                    where = paste0("{.var ", x_name, "}"))
    lbl <- attr(data, "label", exact = TRUE)
    if (!is.null(lbl)) attr(out, "label") <- paste0(lbl, " (standardized)")
    return(out)
  }

  # ============================================================================
  # DATA FRAME INPUT
  # ============================================================================

  # Resolve weights (only the vector is used: the returned data keep the
  # weights column as it was, and an expression such as w * 2 adds none)
  weights_info <- .process_weights(data, weights_quo)
  w <- weights_info$vector

  # Validate weights + method combination
  if (!is.null(w) && method %in% c("mad", "gmd")) {
    cli::cli_abort(
      "Weighted standardization is not supported for method {.val {method}}. Use {.val sd} or {.val 2sd}."
    )
  }

  vars <- .process_variables(data, ...)

  for (i in vars) {
    col_name <- names(data)[i]
    out_name <- if (!is.null(suffix)) paste0(col_name, suffix) else col_name

    if (!is.numeric(data[[col_name]])) {
      cli::cli_warn("Skipping non-numeric variable {.var {col_name}}.")
      next
    }

    data[[out_name]] <- .transform_by_group(
      data, col_name, w, suffix_label = " (standardized)",
      fun = function(x, w, where) {
        .std_vec(x, method = method, w = w, na.rm = na.rm, where = where)
      }
    )
  }

  invisible(data)
}


#' Apply a per-group transformation (std/center) to one column
#'
#' Returns a plain double (value labels and the labelled class dropped, the
#' variable label read BEFORE the column is overwritten and extended by
#' `suffix_label`). Grouped data are transformed within each group; the
#' result vector is built fresh, so an in-place grouped call no longer
#' leaves a haven_labelled (dbl+lbl) column behind.
#' @noRd
.transform_by_group <- function(data, col_name, w, suffix_label, fun) {
  x <- data[[col_name]]
  orig_label <- attr(x, "label", exact = TRUE)

  if (inherits(data, "grouped_df")) {
    group_indices <- dplyr::group_indices(data)
    keys <- dplyr::group_keys(data)
    result <- rep(NA_real_, length(x))
    for (g in unique(group_indices)) {
      mask <- group_indices == g
      w_g <- if (!is.null(w)) w[mask] else NULL
      where <- paste0("{.var ", col_name, "} (",
                      .format_group_label(keys[g, , drop = FALSE]), ")")
      result[mask] <- fun(x[mask], w_g, where)
    }
  } else {
    result <- fun(x, w, paste0("{.var ", col_name, "}"))
  }

  result <- as.double(result)
  if (!is.null(orig_label)) attr(result, "label") <- paste0(orig_label, suffix_label)
  result
}


# ============================================================================
# Internal: Standardize a single vector
# ============================================================================

#' @noRd
.std_vec <- function(x, method = "sd", w = NULL, na.rm = TRUE,
                     where = "{.var x}") {
  # Bare numbers; missing values of imported variables (tagged NAs) become
  # plain NA in the result, like SPSS DESCRIPTIVES /SAVE (system-missing
  # z-scores). Tagged payloads without their code map would otherwise break
  # write_spss().
  x <- as.double(.plain_numeric(x))
  x[is.na(x)] <- NA_real_

  # Weighted path (SPSS frequency-weight mean/SD from kernels-weighted.R)
  if (!is.null(w)) {
    .check_weights(w)

    if (method %in% c("mad", "gmd")) {
      cli::cli_abort(
        "Weighted standardization is not supported for method {.val {method}}."
      )
    }

    w <- .plain_numeric(w)
    center <- .w_mean(x, w, na.rm = na.rm)
    w_sd <- .w_sd(x, w, na.rm = na.rm)
    spread <- if (method == "2sd") 2 * w_sd else w_sd
  } else {
    # Unweighted path
    center <- if (method == "mad") {
      stats::median(x, na.rm = na.rm)
    } else {
      mean(x, na.rm = na.rm)
    }
    spread <- switch(method,
      sd   = stats::sd(x, na.rm = na.rm),
      `2sd` = 2 * stats::sd(x, na.rm = na.rm),
      mad  = stats::mad(x, na.rm = na.rm),
      gmd  = .gmd(x, na.rm = na.rm)
    )
  }

  if (is.na(spread) || spread == 0) {
    cli::cli_warn(c(
      paste0(where, ": the spread (", method, ") is zero or not computable."),
      "i" = "The standardized values are {.val NA}."
    ))
    return(rep(NA_real_, length(x)))
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
#' @param weights Optional survey weights. When provided, the weighted mean is
#'   subtracted instead of the unweighted mean. Give a column name (unquoted or
#'   as a string), an expression such as \code{sampling_weight * 2}, or a
#'   numeric vector with one weight per row.
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
#' ## Missing Values
#'
#' The result is a plain numeric variable (value labels are dropped, the
#' variable label is kept with " (centered)" appended). Missing values of
#' imported data, including their SPSS missing types, become plain
#' \code{NA}, as in an SPSS \code{COMPUTE}.
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
  .check_required("data")

  # ============================================================================
  # VECTOR INPUT
  # ============================================================================

  weights_quo <- rlang::enquo(weights)

  if (!is.data.frame(data)) {
    .check_dots_unused(...)
    if (!is.numeric(data)) {
      cli::cli_abort("{.arg data} must be numeric.")
    }
    w <- if (!rlang::quo_is_null(weights_quo)) rlang::eval_tidy(weights_quo) else NULL
    out <- .center_vec(data, w = w, na.rm = na.rm)
    lbl <- attr(data, "label", exact = TRUE)
    if (!is.null(lbl)) attr(out, "label") <- paste0(lbl, " (centered)")
    return(out)
  }

  # ============================================================================
  # DATA FRAME INPUT
  # ============================================================================

  # Resolve weights (only the vector is used: the returned data keep the
  # weights column as it was, and an expression such as w * 2 adds none)
  weights_info <- .process_weights(data, weights_quo)
  w <- weights_info$vector

  vars <- .process_variables(data, ...)

  for (i in vars) {
    col_name <- names(data)[i]
    out_name <- if (!is.null(suffix)) paste0(col_name, suffix) else col_name

    if (!is.numeric(data[[col_name]])) {
      cli::cli_warn("Skipping non-numeric variable {.var {col_name}}.")
      next
    }

    data[[out_name]] <- .transform_by_group(
      data, col_name, w, suffix_label = " (centered)",
      fun = function(x, w, where) .center_vec(x, w = w, na.rm = na.rm)
    )
  }

  invisible(data)
}


# ============================================================================
# Internal: Center a single vector
# ============================================================================

#' @noRd
.center_vec <- function(x, w = NULL, na.rm = TRUE) {
  # Bare numbers, missing types become plain NA (see .std_vec)
  x <- as.double(.plain_numeric(x))
  x[is.na(x)] <- NA_real_
  if (!is.null(w)) {
    .check_weights(w)
    # SPSS frequency-weight mean from kernels-weighted.R
    return(x - .w_mean(x, .plain_numeric(w), na.rm = na.rm))
  }
  x - mean(x, na.rm = na.rm)
}
