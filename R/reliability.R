
#' Check How Reliably Your Scale Measures a Concept
#'
#' @description
#' \code{reliability()} calculates Cronbach's Alpha and detailed item statistics
#' to evaluate whether your survey items form a reliable scale. This is the R
#' equivalent of SPSS's \code{RELIABILITY /MODEL=ALPHA /STATISTICS=DESCRIPTIVE CORR
#' /SUMMARY=TOTAL}.
#'
#' For example, if you have 3 items measuring "trust", reliability analysis
#' tells you whether these items consistently measure the same concept.
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The items to analyze. Use bare column names separated by commas,
#'   or tidyselect helpers like \code{starts_with("trust")}.
#' @param weights Optional survey weights for population-representative results
#' @param na.rm Remove missing values before calculating? (Default: TRUE).
#'   Uses listwise deletion (only complete cases across all items).
#'
#' @return A reliability result object containing:
#' \describe{
#'   \item{alpha}{Cronbach's Alpha (unstandardized)}
#'   \item{alpha_standardized}{Cronbach's Alpha based on standardized items}
#'   \item{n_items}{Number of items in the scale}
#'   \item{item_statistics}{Mean, SD, and N for each item}
#'   \item{item_total}{Corrected Item-Total Correlation and Alpha if Item Deleted}
#'   \item{inter_item_cor}{Inter-item correlation matrix}
#'   \item{n}{Sample size (listwise)}
#' }
#'
#' @details
#' ## Understanding the Results
#'
#' **Cronbach's Alpha** tells you how internally consistent your scale is:
#' \itemize{
#'   \item Alpha > 0.90: Excellent reliability
#'   \item Alpha 0.80 - 0.90: Good reliability
#'   \item Alpha 0.70 - 0.80: Acceptable reliability
#'   \item Alpha 0.60 - 0.70: Questionable reliability
#'   \item Alpha < 0.60: Poor reliability - reconsider items
#' }
#'
#' **Item-Total Correlation** shows how well each item fits the scale:
#' \itemize{
#'   \item Values > 0.40: Item fits well
#'   \item Values 0.20 - 0.40: Item may need review
#'   \item Values < 0.20: Consider removing the item
#' }
#'
#' **Alpha if Item Deleted** shows what happens if you remove an item:
#' \itemize{
#'   \item If alpha increases when removing an item, that item hurts reliability
#'   \item If alpha decreases, the item contributes to the scale
#' }
#'
#' ## When to Use This
#'
#' Run \code{reliability()} before creating scale scores with
#' \code{\link{scale_index}}:
#' \enumerate{
#'   \item Select your items
#'   \item Check reliability
#'   \item If acceptable (alpha > .70), create the index
#'   \item If not, review items and consider removing problematic ones
#' }
#'
#' @examples
#' library(dplyr)
#' data(survey_data)
#'
#' # Check reliability of trust items
#' reliability(survey_data, trust_government, trust_media, trust_science)
#'
#' # With survey weights
#' reliability(survey_data, trust_government, trust_media, trust_science,
#'             weights = sampling_weight)
#'
#' # Using tidyselect helpers
#' reliability(survey_data, starts_with("trust"))
#'
#' # Grouped by region
#' survey_data %>%
#'   group_by(region) %>%
#'   reliability(trust_government, trust_media, trust_science)
#'
#' @seealso
#' \code{\link{scale_index}} for creating mean indices after checking reliability.
#'
#' \code{\link{pearson_cor}} for bivariate correlations.
#'
#' @family scale
#' @export
reliability <- function(data, ..., weights = NULL, na.rm = TRUE) {

  # ============================================================================
  # INPUT VALIDATION AND SETUP
  # ============================================================================

  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame or tibble.")
  }

  # Get variable names using tidyselect
  vars <- .process_variables(data, ...)
  var_names <- names(vars)

  # Validate all selected variables are numeric
  for (var_name in var_names) {
    if (!is.numeric(data[[var_name]])) {
      cli_abort(
        "Variable {.var {var_name}} is not numeric. {.fn reliability} requires numeric items."
      )
    }
  }

  if (length(var_names) < 2) {
    cli_abort("{.fn reliability} requires at least 2 items.")
  }

  # Process weights
  weights_info <- .process_weights(data, rlang::enquo(weights))

  # Check if data is grouped
  is_grouped <- inherits(data, "grouped_df")

  # ============================================================================
  # GROUPED OR UNGROUPED ANALYSIS
  # ============================================================================

  if (is_grouped) {
    group_vars <- dplyr::group_vars(data)
    group_keys_df <- dplyr::group_keys(data)
    group_list <- dplyr::group_split(data)

    results_list <- list()
    for (i in seq_along(group_list)) {
      group_data <- group_list[[i]]
      group_weights <- if (!is.null(weights_info$name)) group_data[[weights_info$name]] else NULL

      results_list[[i]] <- .reliability_core(
        group_data, var_names, group_weights, na.rm
      )
      results_list[[i]]$group_values <- as.list(group_keys_df[i, , drop = FALSE])
    }

    result <- list(
      groups = results_list,
      variables = var_names,
      weights = weights_info$name,
      is_grouped = TRUE,
      group_vars = group_vars,
      n_items = length(var_names)
    )
  } else {
    core_result <- .reliability_core(
      data, var_names, weights_info$vector, na.rm
    )

    result <- c(core_result, list(
      variables = var_names,
      weights = weights_info$name,
      is_grouped = FALSE,
      group_vars = NULL
    ))
  }

  class(result) <- "reliability"
  return(result)
}


# ============================================================================
# CORE COMPUTATION
# ============================================================================

#' Compute reliability statistics for a single group
#' @keywords internal
.reliability_core <- function(data, var_names, weights_vec, na.rm) {

  # Extract item matrix and apply listwise deletion
  item_data <- data[, var_names, drop = FALSE]

  if (na.rm) {
    complete <- stats::complete.cases(item_data)
    if (!is.null(weights_vec)) {
      complete <- complete & !is.na(weights_vec)
      weights_vec <- weights_vec[complete]
    }
    item_data <- item_data[complete, , drop = FALSE]
  }

  n <- nrow(item_data)
  k <- length(var_names)
  mat <- as.matrix(item_data)

  if (n < 2) {
    cli_warn("Insufficient data for reliability analysis (n = {n}).")
    return(list(
      alpha = NA_real_,
      alpha_standardized = NA_real_,
      n_items = k,
      item_statistics = NULL,
      item_total = NULL,
      inter_item_cor = NULL,
      n = n
    ))
  }

  # ============================================================================
  # COVARIANCE AND CORRELATION MATRICES
  # ============================================================================

  if (!is.null(weights_vec)) {
    cov_mat <- .weighted_cov(mat, weights_vec)
    cor_mat <- .weighted_cor(mat, weights_vec)
    w_n <- sum(weights_vec)
  } else {
    cov_mat <- stats::cov(mat)
    cor_mat <- stats::cor(mat)
    w_n <- n
  }

  rownames(cor_mat) <- colnames(cor_mat) <- var_names
  rownames(cov_mat) <- colnames(cov_mat) <- var_names

  # ============================================================================
  # CRONBACH'S ALPHA (UNSTANDARDIZED)
  # ============================================================================
  # Formula: alpha = (k / (k-1)) * (1 - sum(item_var) / total_var)
  # where total_var = variance of sum of all items

  item_variances <- diag(cov_mat)
  total_variance <- sum(cov_mat)  # sum of entire covariance matrix = var(sum)

  alpha <- (k / (k - 1)) * (1 - sum(item_variances) / total_variance)

  # ============================================================================
  # STANDARDIZED ALPHA
  # ============================================================================
  # Formula: alpha_std = (k * mean_r) / (1 + (k-1) * mean_r)

  # Mean of off-diagonal correlations
  off_diag <- cor_mat[upper.tri(cor_mat)]
  mean_r <- mean(off_diag)

  alpha_standardized <- (k * mean_r) / (1 + (k - 1) * mean_r)

  # ============================================================================
  # ITEM STATISTICS (Mean, SD, N per item)
  # ============================================================================

  if (!is.null(weights_vec)) {
    item_means <- vapply(var_names, function(v) {
      sum(mat[, v] * weights_vec) / sum(weights_vec)
    }, numeric(1))
    item_sds <- sqrt(item_variances)
    item_n <- rep(w_n, k)
  } else {
    item_means <- colMeans(mat)
    item_sds <- sqrt(item_variances)
    item_n <- rep(n, k)
  }

  item_statistics <- tibble::tibble(
    item = var_names,
    mean = as.numeric(item_means),
    sd = as.numeric(item_sds),
    n = item_n
  )

  # ============================================================================
  # ITEM-TOTAL STATISTICS
  # ============================================================================

  scale_mean_if_deleted <- numeric(k)
  scale_var_if_deleted <- numeric(k)
  corrected_item_total_r <- numeric(k)
  alpha_if_deleted <- numeric(k)

  for (i in seq_len(k)) {
    # Items without item i
    remaining_idx <- setdiff(seq_len(k), i)
    remaining_mat <- mat[, remaining_idx, drop = FALSE]
    remaining_cov <- cov_mat[remaining_idx, remaining_idx, drop = FALSE]

    # Scale Mean if Item Deleted = sum of remaining item means
    scale_mean_if_deleted[i] <- sum(item_means[remaining_idx])

    # Scale Variance if Item Deleted = sum of remaining covariance matrix
    scale_var_if_deleted[i] <- sum(remaining_cov)

    # Corrected Item-Total Correlation
    # = correlation of item i with sum of remaining items
    if (!is.null(weights_vec)) {
      total_remaining <- rowSums(remaining_mat)
      corrected_item_total_r[i] <- .weighted_cor_vec(mat[, i], total_remaining, weights_vec)
    } else {
      total_remaining <- rowSums(remaining_mat)
      corrected_item_total_r[i] <- stats::cor(mat[, i], total_remaining)
    }

    # Alpha if Item Deleted
    k_rem <- k - 1
    item_var_rem <- diag(remaining_cov)
    total_var_rem <- sum(remaining_cov)

    if (total_var_rem > 0 && k_rem > 1) {
      alpha_if_deleted[i] <- (k_rem / (k_rem - 1)) * (1 - sum(item_var_rem) / total_var_rem)
    } else {
      alpha_if_deleted[i] <- NA_real_
    }
  }

  item_total <- tibble::tibble(
    item = var_names,
    scale_mean_if_deleted = scale_mean_if_deleted,
    scale_var_if_deleted = scale_var_if_deleted,
    corrected_item_total_r = corrected_item_total_r,
    alpha_if_deleted = alpha_if_deleted
  )

  # ============================================================================
  # RETURN RESULT
  # ============================================================================

  list(
    alpha = alpha,
    alpha_standardized = alpha_standardized,
    n_items = k,
    item_statistics = item_statistics,
    item_total = item_total,
    inter_item_cor = cor_mat,
    n = n,
    weighted_n = if (!is.null(weights_vec)) w_n else NULL
  )
}


# ============================================================================
# WEIGHTED COVARIANCE AND CORRELATION HELPERS
# ============================================================================

#' Compute weighted covariance matrix (SPSS-compatible)
#' @description Uses frequency-weighted formula: cov = sum(w*(x-mx)*(y-my)) / (V1 - 1)
#' @keywords internal
.weighted_cov <- function(mat, w) {
  k <- ncol(mat)
  V1 <- sum(w)

  # Weighted means
  w_means <- colSums(mat * w) / V1

  # Center the data
  centered <- sweep(mat, 2, w_means)

  # Weighted covariance: (t(centered) %*% diag(w) %*% centered) / (V1 - 1)
  cov_mat <- (t(centered * w) %*% centered) / (V1 - 1)

  return(cov_mat)
}

#' Compute weighted correlation matrix from weighted covariance
#' @keywords internal
.weighted_cor <- function(mat, w) {
  cov_mat <- .weighted_cov(mat, w)
  sds <- sqrt(diag(cov_mat))
  cor_mat <- cov_mat / outer(sds, sds)
  # Ensure diagonal is exactly 1
  diag(cor_mat) <- 1
  return(cor_mat)
}

#' Compute weighted correlation between two vectors
#' @keywords internal
.weighted_cor_vec <- function(x, y, w) {
  V1 <- sum(w)
  mx <- sum(x * w) / V1
  my <- sum(y * w) / V1
  cov_xy <- sum(w * (x - mx) * (y - my)) / (V1 - 1)
  var_x <- sum(w * (x - mx)^2) / (V1 - 1)
  var_y <- sum(w * (y - my)^2) / (V1 - 1)
  if (var_x <= 0 || var_y <= 0) return(NA_real_)
  cov_xy / sqrt(var_x * var_y)
}


# ============================================================================
# PRINT METHOD
# ============================================================================

#' @export
print.reliability <- function(x, digits = 3, ...) {

  # Header
  title <- get_standard_title("Reliability Analysis", x$weights, "Results")
  print_header(title)

  if (x$is_grouped) {
    .print_reliability_grouped(x, digits)
  } else {
    .print_reliability_ungrouped(x, digits)
  }

  invisible(x)
}

#' Print reliability results for ungrouped data
#' @keywords internal
.print_reliability_ungrouped <- function(x, digits = 3) {

  # Info section
  print_info_section(list(
    "Items" = paste(x$variables, collapse = ", "),
    "N of Items" = x$n_items,
    "Weights" = x$weights
  ))

  # Reliability Statistics
  cat("\n")
  cat("Reliability Statistics\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")

  cat(sprintf("  Cronbach's Alpha:              %s\n",
              format(round(x$alpha, digits), nsmall = digits)))
  cat(sprintf("  Alpha (standardized):          %s\n",
              format(round(x$alpha_standardized, digits), nsmall = digits)))
  cat(sprintf("  N of Items:                    %d\n", x$n_items))

  n_label <- if (!is.null(x$weighted_n)) {
    sprintf("%.2f (weighted)", x$weighted_n)
  } else {
    as.character(x$n)
  }
  cat(sprintf("  N (listwise):                  %s\n", n_label))

  # Item Statistics
  if (!is.null(x$item_statistics)) {
    cat("\nItem Statistics\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")

    item_df <- x$item_statistics
    item_df$mean <- round(item_df$mean, digits)
    item_df$sd <- round(item_df$sd, digits)
    if (!is.null(x$weighted_n)) {
      item_df$n <- round(item_df$n, 2)
    }
    print(as.data.frame(item_df), row.names = FALSE)
  }

  # Inter-Item Correlation Matrix
  if (!is.null(x$inter_item_cor)) {
    .print_cor_matrix(x$inter_item_cor, digits = digits,
                      title = "Inter-Item Correlation Matrix:",
                      type = "correlation")
  }

  # Item-Total Statistics
  if (!is.null(x$item_total)) {
    cat("\nItem-Total Statistics\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")

    total_df <- data.frame(
      item = x$item_total$item,
      scale_mean_deleted = round(x$item_total$scale_mean_if_deleted, 2),
      scale_var_deleted = round(x$item_total$scale_var_if_deleted, digits),
      corrected_r = round(x$item_total$corrected_item_total_r, digits),
      alpha_deleted = round(x$item_total$alpha_if_deleted, digits),
      stringsAsFactors = FALSE
    )
    print(total_df, row.names = FALSE)
  }
}

#' Print reliability results for grouped data
#' @keywords internal
.print_reliability_grouped <- function(x, digits = 3) {

  for (group_result in x$groups) {
    # Group header
    group_values <- group_result$group_values
    print_group_header(group_values)

    # Create a temporary ungrouped-like structure for printing
    temp <- c(group_result, list(
      variables = x$variables,
      weights = x$weights,
      n_items = x$n_items
    ))

    .print_reliability_ungrouped(temp, digits)
  }
}
