
#' Explore the Structure Behind Your Survey Items
#'
#' @description
#' \code{efa()} performs Exploratory Factor Analysis (EFA) using Principal
#' Component Analysis (PCA) to discover underlying patterns in your survey items.
#' This is the R equivalent of SPSS's \code{FACTOR /EXTRACTION PC /ROTATION VARIMAX}.
#'
#' For example, if you have 6 items measuring different attitudes, EFA can reveal
#' whether they group into 2-3 underlying dimensions (components).
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The items to analyze. Use bare column names separated by commas,
#'   or tidyselect helpers like \code{starts_with("trust")}.
#' @param n_factors Number of components to extract. Default \code{NULL} uses
#'   the Kaiser criterion (eigenvalue > 1).
#' @param rotation Rotation method: \code{"varimax"} (default, orthogonal),
#'   \code{"oblimin"} (oblique, allows correlated factors), or \code{"none"}.
#' @param extraction Extraction method. Currently only \code{"pca"} (Principal
#'   Component Analysis) is supported.
#' @param weights Optional survey weights for population-representative results.
#' @param use How to handle missing data for correlation computation:
#'   \code{"pairwise"} (default, matches SPSS) or \code{"complete"} (listwise).
#' @param sort Logical. Sort loadings by size within each component? Default \code{TRUE}.
#' @param blank Numeric. Suppress (hide) loadings with absolute value below this
#'   threshold in the print output. Default \code{0.40} (matches SPSS BLANK(.40)).
#'   Set to \code{0} to show all loadings.
#' @param na.rm Logical. Remove missing values? Default \code{TRUE}.
#'
#' @return An \code{efa} result object containing:
#' \describe{
#'   \item{loadings}{Component loading matrix (rotated if rotation applied)}
#'   \item{unrotated_loadings}{Unrotated component matrix}
#'   \item{eigenvalues}{All eigenvalues from the correlation matrix}
#'   \item{variance_explained}{Tibble with Total, % of Variance, Cumulative %}
#'   \item{rotation_variance}{Tibble with rotation sums of squared loadings}
#'   \item{communalities}{Extraction communalities for each variable}
#'   \item{kmo}{List with overall KMO and per-item MSA values}
#'   \item{bartlett}{List with chi_sq, df, and p_value}
#'   \item{rotation}{Rotation method used}
#'   \item{extraction}{Extraction method used}
#'   \item{n_factors}{Number of components extracted}
#'   \item{correlation_matrix}{Correlation matrix used for analysis}
#'   \item{pattern_matrix}{Pattern matrix (oblimin only, NULL otherwise)}
#'   \item{structure_matrix}{Structure matrix (oblimin only, NULL otherwise)}
#'   \item{factor_correlations}{Factor correlation matrix (oblimin only, NULL otherwise)}
#'   \item{variables}{Character vector of variable names}
#'   \item{weights}{Weights variable name or NULL}
#'   \item{n}{Sample size}
#'   \item{sort}{Whether loadings are sorted}
#'   \item{blank}{Suppression threshold}
#' }
#'
#' @details
#' ## Understanding the Results
#'
#' **KMO (Kaiser-Meyer-Olkin)** measures sampling adequacy:
#' \itemize{
#'   \item KMO > 0.90: Marvelous
#'   \item KMO 0.80 - 0.90: Meritorious
#'   \item KMO 0.70 - 0.80: Middling
#'   \item KMO 0.60 - 0.70: Mediocre
#'   \item KMO 0.50 - 0.60: Miserable
#'   \item KMO < 0.50: Unacceptable - don't use factor analysis
#' }
#'
#' **Bartlett's Test of Sphericity** tests whether correlations are significantly
#' different from zero. A significant result (p < .05) means the correlation
#' matrix is suitable for factor analysis.
#'
#' **Eigenvalues** indicate how much variance each component explains.
#' The Kaiser criterion retains components with eigenvalue > 1.
#'
#' **Factor Loadings** show how strongly each item relates to each component:
#' \itemize{
#'   \item |loading| > 0.70: Strong association
#'   \item |loading| 0.40 - 0.70: Moderate association
#'   \item |loading| < 0.40: Weak (suppressed by default)
#' }
#'
#' **Communalities** show how much of each item's variance is explained by
#' the extracted components. Low communalities (< 0.40) suggest the item
#' doesn't fit well with the others.
#'
#' ## Choosing a Rotation
#'
#' \itemize{
#'   \item \strong{Varimax} (default): Assumes factors are uncorrelated.
#'     Produces simpler, easier-to-interpret results.
#'   \item \strong{Oblimin}: Allows factors to be correlated.
#'     More realistic for social science data. Produces both a Pattern Matrix
#'     (unique contributions) and Structure Matrix (total correlations).
#'   \item \strong{None}: No rotation. Rarely useful for interpretation.
#' }
#'
#' @examples
#' library(dplyr)
#' data(survey_data)
#'
#' # Basic EFA with Varimax rotation
#' efa(survey_data,
#'     political_orientation, environmental_concern, life_satisfaction,
#'     trust_government, trust_media, trust_science)
#'
#' # With Oblimin rotation (allows correlated factors)
#' efa(survey_data,
#'     political_orientation, environmental_concern, life_satisfaction,
#'     trust_government, trust_media, trust_science,
#'     rotation = "oblimin")
#'
#' # Fix number of factors
#' efa(survey_data,
#'     political_orientation, environmental_concern, life_satisfaction,
#'     trust_government, trust_media, trust_science,
#'     n_factors = 2)
#'
#' # With survey weights
#' efa(survey_data,
#'     political_orientation, environmental_concern, life_satisfaction,
#'     trust_government, trust_media, trust_science,
#'     weights = sampling_weight)
#'
#' # Grouped by region
#' survey_data %>%
#'   group_by(region) %>%
#'   efa(political_orientation, environmental_concern, life_satisfaction,
#'       trust_government, trust_media, trust_science)
#'
#' @seealso
#' \code{\link{reliability}} for checking scale reliability before creating indices.
#'
#' \code{\link{scale_index}} for creating mean indices after identifying factors.
#'
#' @family scale
#' @export
efa <- function(data, ...,
                n_factors = NULL,
                rotation = "varimax",
                extraction = "pca",
                weights = NULL,
                use = "pairwise",
                sort = TRUE,
                blank = 0.40,
                na.rm = TRUE) {

  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================

  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame or tibble.")
  }

  # Validate rotation
  rotation <- match.arg(rotation, c("varimax", "oblimin", "none"))

  # Validate extraction
  extraction <- match.arg(extraction, c("pca"))

  # Validate use
  use <- match.arg(use, c("pairwise", "complete"))

  # Check GPArotation availability for oblimin

  if (rotation == "oblimin") {
    if (!requireNamespace("GPArotation", quietly = TRUE)) {
      cli_abort(c(
        "Package {.pkg GPArotation} is required for oblimin rotation.",
        "i" = "Install it with: {.code install.packages(\"GPArotation\")}"
      ))
    }
  }

  # Get variable names using tidyselect
  vars <- .process_variables(data, ...)
  var_names <- names(vars)

  # Validate all selected variables are numeric
  for (var_name in var_names) {
    if (!is.numeric(data[[var_name]])) {
      cli_abort(
        "Variable {.var {var_name}} is not numeric. {.fn efa} requires numeric items."
      )
    }
  }

  if (length(var_names) < 2) {
    cli_abort("{.fn efa} requires at least 2 variables.")
  }

  # Validate n_factors
  if (!is.null(n_factors)) {
    n_factors <- as.integer(n_factors)
    if (n_factors < 1 || n_factors > length(var_names)) {
      cli_abort("{.arg n_factors} must be between 1 and {length(var_names)} (number of variables).")
    }
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

      results_list[[i]] <- .efa_core(
        group_data, var_names, group_weights, n_factors, rotation,
        extraction, use, sort, blank, na.rm
      )
      results_list[[i]]$group_values <- as.list(group_keys_df[i, , drop = FALSE])
    }

    result <- list(
      groups = results_list,
      variables = var_names,
      weights = weights_info$name,
      is_grouped = TRUE,
      group_vars = group_vars,
      n_factors_input = n_factors,
      rotation = rotation,
      extraction = extraction,
      sort = sort,
      blank = blank
    )
  } else {
    core_result <- .efa_core(
      data, var_names, weights_info$vector, n_factors, rotation,
      extraction, use, sort, blank, na.rm
    )

    result <- c(core_result, list(
      variables = var_names,
      weights = weights_info$name,
      is_grouped = FALSE,
      group_vars = NULL,
      n_factors_input = n_factors,
      sort = sort,
      blank = blank
    ))
  }

  class(result) <- "efa"
  return(result)
}


# ============================================================================
# CORE COMPUTATION
# ============================================================================

#' Compute EFA for a single group
#' @keywords internal
.efa_core <- function(data, var_names, weights_vec, n_factors, rotation,
                      extraction, use, sort, blank, na.rm) {

  k <- length(var_names)

  # ============================================================================
  # CORRELATION MATRIX (pairwise or listwise)
  # ============================================================================

  if (use == "pairwise") {
    cor_result <- .efa_pairwise_cor(data, var_names, weights_vec)
  } else {
    cor_result <- .efa_listwise_cor(data, var_names, weights_vec, na.rm)
  }

  cor_mat <- cor_result$cor_mat
  n_obs <- cor_result$n_obs  # pairwise N matrix or single listwise N
  # For Bartlett's test, use the harmonic mean of pairwise N (SPSS approach)
  n_bartlett <- cor_result$n_bartlett

  # ============================================================================
  # KMO AND BARTLETT'S TEST
  # ============================================================================

  kmo_result <- .compute_kmo(cor_mat)
  bartlett_result <- .compute_bartlett(cor_mat, n_bartlett, k)

  # ============================================================================
  # EIGENVALUE DECOMPOSITION
  # ============================================================================

  eig <- eigen(cor_mat, symmetric = TRUE)
  eigenvalues <- eig$values
  eigenvectors <- eig$vectors

  # Determine number of factors
  if (is.null(n_factors)) {
    n_factors_used <- sum(eigenvalues > 1)
    if (n_factors_used == 0) n_factors_used <- 1
  } else {
    n_factors_used <- n_factors
  }

  # ============================================================================
  # VARIANCE EXPLAINED TABLE
  # ============================================================================

  total_var <- k  # For PCA on correlation matrix, total variance = k
  prc_var <- eigenvalues / total_var * 100
  cum_prc <- cumsum(prc_var)

  variance_explained <- tibble::tibble(
    component = seq_along(eigenvalues),
    eigenvalue = eigenvalues,
    prc_variance = prc_var,
    cumulative_prc = cum_prc
  )

  # ============================================================================
  # UNROTATED COMPONENT MATRIX
  # ============================================================================

  # Loadings = eigenvectors * sqrt(eigenvalues) for extracted components
  raw_loadings <- eigenvectors[, seq_len(n_factors_used), drop = FALSE] %*%
    diag(sqrt(eigenvalues[seq_len(n_factors_used)]), nrow = n_factors_used)
  rownames(raw_loadings) <- var_names
  colnames(raw_loadings) <- paste0("PC", seq_len(n_factors_used))

  # Communalities from extraction
  communalities <- rowSums(raw_loadings^2)
  names(communalities) <- var_names

  # ============================================================================
  # ROTATION
  # ============================================================================

  pattern_matrix <- NULL
  structure_matrix <- NULL
  factor_correlations <- NULL
  rotation_variance <- NULL

  if (n_factors_used < 2 || rotation == "none") {
    # No rotation possible or requested
    rotated_loadings <- raw_loadings
    rotation_used <- if (n_factors_used < 2) "none" else rotation

    # Rotation sums = extraction sums when no rotation
    rot_ss <- colSums(rotated_loadings^2)
    rotation_variance <- tibble::tibble(
      component = seq_len(n_factors_used),
      ss_loading = rot_ss,
      prc_variance = rot_ss / total_var * 100,
      cumulative_prc = cumsum(rot_ss / total_var * 100)
    )

  } else if (rotation == "varimax") {
    vm <- stats::varimax(raw_loadings, normalize = TRUE)
    rotated_loadings <- as.matrix(unclass(vm$loadings))
    rownames(rotated_loadings) <- var_names
    colnames(rotated_loadings) <- paste0("PC", seq_len(n_factors_used))
    rotation_used <- "varimax"

    # Rotation sums of squared loadings
    rot_ss <- colSums(rotated_loadings^2)
    rotation_variance <- tibble::tibble(
      component = seq_len(n_factors_used),
      ss_loading = rot_ss,
      prc_variance = rot_ss / total_var * 100,
      cumulative_prc = cumsum(rot_ss / total_var * 100)
    )

  } else if (rotation == "oblimin") {
    ob <- GPArotation::oblimin(raw_loadings, normalize = TRUE)
    pattern_matrix <- ob$loadings
    rownames(pattern_matrix) <- var_names
    colnames(pattern_matrix) <- paste0("PC", seq_len(n_factors_used))
    class(pattern_matrix) <- "matrix"

    # Factor correlation matrix (Phi)
    factor_correlations <- ob$Phi
    rownames(factor_correlations) <- colnames(factor_correlations) <- paste0("PC", seq_len(n_factors_used))

    # Structure matrix = Pattern * Phi
    structure_matrix <- pattern_matrix %*% factor_correlations
    rownames(structure_matrix) <- var_names
    colnames(structure_matrix) <- paste0("PC", seq_len(n_factors_used))

    # For oblimin, the "loadings" returned are the pattern matrix
    rotated_loadings <- pattern_matrix
    rotation_used <- "oblimin"

    # Rotation sums of squared loadings (for oblimin, just SS of pattern)
    rot_ss <- colSums(pattern_matrix^2)
    rotation_variance <- tibble::tibble(
      component = seq_len(n_factors_used),
      ss_loading = rot_ss
    )
  }

  # ============================================================================
  # DESCRIPTIVE STATISTICS
  # ============================================================================

  # Per-variable descriptive stats (pairwise N)
  item_stats <- .efa_item_stats(data, var_names, weights_vec)

  # ============================================================================
  # RETURN RESULT
  # ============================================================================

  list(
    loadings = rotated_loadings,
    unrotated_loadings = raw_loadings,
    eigenvalues = eigenvalues,
    variance_explained = variance_explained,
    rotation_variance = rotation_variance,
    communalities = communalities,
    kmo = kmo_result,
    bartlett = bartlett_result,
    rotation = rotation_used,
    extraction = extraction,
    n_factors = n_factors_used,
    correlation_matrix = cor_mat,
    pattern_matrix = pattern_matrix,
    structure_matrix = structure_matrix,
    factor_correlations = factor_correlations,
    item_statistics = item_stats,
    n = n_bartlett
  )
}


# ============================================================================
# CORRELATION MATRIX COMPUTATION
# ============================================================================

#' Compute pairwise correlation matrix (SPSS default for FACTOR)
#' @keywords internal
.efa_pairwise_cor <- function(data, var_names, weights_vec) {
  k <- length(var_names)

  if (!is.null(weights_vec)) {
    # Weighted pairwise correlations
    cor_mat <- matrix(1, k, k)
    n_mat <- matrix(0, k, k)
    rownames(cor_mat) <- colnames(cor_mat) <- var_names
    rownames(n_mat) <- colnames(n_mat) <- var_names

    for (i in seq_len(k)) {
      for (j in seq_len(k)) {
        if (i == j) {
          # Count valid cases for diagonal
          valid <- !is.na(data[[var_names[i]]]) & !is.na(weights_vec)
          n_mat[i, j] <- sum(weights_vec[valid])
          next
        }
        if (j > i) next  # Will fill from lower triangle

        x <- data[[var_names[i]]]
        y <- data[[var_names[j]]]
        valid <- !is.na(x) & !is.na(y) & !is.na(weights_vec)

        if (sum(valid) < 2) {
          cor_mat[i, j] <- cor_mat[j, i] <- NA_real_
          n_mat[i, j] <- n_mat[j, i] <- sum(weights_vec[valid])
          next
        }

        xv <- x[valid]
        yv <- y[valid]
        wv <- weights_vec[valid]

        cor_mat[i, j] <- cor_mat[j, i] <- .weighted_cor_vec(xv, yv, wv)
        n_mat[i, j] <- n_mat[j, i] <- sum(wv)
      }
    }

    # Use minimum pairwise N for Bartlett (matches SPSS)
    off_diag_n <- n_mat[lower.tri(n_mat)]
    n_bartlett <- min(off_diag_n)

  } else {
    # Unweighted pairwise correlations
    mat <- as.matrix(data[, var_names, drop = FALSE])
    cor_mat <- stats::cor(mat, use = "pairwise.complete.obs")
    rownames(cor_mat) <- colnames(cor_mat) <- var_names

    # Pairwise N matrix
    n_mat <- matrix(0, k, k)
    for (i in seq_len(k)) {
      for (j in i:k) {
        valid <- !is.na(mat[, i]) & !is.na(mat[, j])
        n_mat[i, j] <- n_mat[j, i] <- sum(valid)
      }
    }

    # Use minimum pairwise N for Bartlett (matches SPSS)
    off_diag_n <- n_mat[lower.tri(n_mat)]
    n_bartlett <- min(off_diag_n)
  }

  list(cor_mat = cor_mat, n_obs = n_mat, n_bartlett = n_bartlett)
}

#' Compute listwise correlation matrix
#' @keywords internal
.efa_listwise_cor <- function(data, var_names, weights_vec, na.rm) {
  item_data <- data[, var_names, drop = FALSE]
  complete <- stats::complete.cases(item_data)

  if (!is.null(weights_vec)) {
    complete <- complete & !is.na(weights_vec)
    weights_vec <- weights_vec[complete]
  }

  item_data <- item_data[complete, , drop = FALSE]
  n <- nrow(item_data)
  mat <- as.matrix(item_data)

  if (!is.null(weights_vec)) {
    cor_mat <- .weighted_cor(mat, weights_vec)
    n_eff <- sum(weights_vec)
  } else {
    cor_mat <- stats::cor(mat)
    n_eff <- n
  }

  rownames(cor_mat) <- colnames(cor_mat) <- var_names

  list(cor_mat = cor_mat, n_obs = n, n_bartlett = n_eff)
}


# ============================================================================
# KMO AND BARTLETT'S TEST
# ============================================================================

#' Compute Kaiser-Meyer-Olkin Measure of Sampling Adequacy
#' @description
#' KMO measures the proportion of variance among variables that might be
#' common variance. SPSS-compatible implementation.
#' @keywords internal
.compute_kmo <- function(cor_mat) {
  # Anti-image approach (SPSS method)
  # 1. Compute inverse of correlation matrix
  k <- ncol(cor_mat)
  inv_cor <- tryCatch(solve(cor_mat), error = function(e) {
    # If singular, use pseudo-inverse
    MASS_available <- requireNamespace("MASS", quietly = TRUE)
    if (MASS_available) {
      MASS::ginv(cor_mat)
    } else {
      # Fallback: add small ridge
      solve(cor_mat + diag(1e-10, k))
    }
  })

  # 2. Compute partial correlation matrix from inverse
  # S_ij = -inv_ij / sqrt(inv_ii * inv_jj)
  d <- diag(inv_cor)
  partial_cor <- -inv_cor / sqrt(outer(d, d))
  diag(partial_cor) <- 1

  # 3. KMO overall = sum(r_ij^2) / (sum(r_ij^2) + sum(a_ij^2))
  # where r_ij are correlations and a_ij are partial correlations (off-diagonal)
  r_sq_sum <- sum(cor_mat[lower.tri(cor_mat)]^2)
  a_sq_sum <- sum(partial_cor[lower.tri(partial_cor)]^2)

  kmo_overall <- r_sq_sum / (r_sq_sum + a_sq_sum)

  # 4. Per-item MSA (diagonal of anti-image correlation)
  kmo_per_item <- numeric(k)
  names(kmo_per_item) <- colnames(cor_mat)
  for (i in seq_len(k)) {
    r_sq_i <- sum(cor_mat[i, -i]^2)
    a_sq_i <- sum(partial_cor[i, -i]^2)
    kmo_per_item[i] <- r_sq_i / (r_sq_i + a_sq_i)
  }

  list(
    overall = kmo_overall,
    per_item = kmo_per_item
  )
}

#' Compute Bartlett's Test of Sphericity
#' @description
#' Tests whether the correlation matrix is significantly different from
#' an identity matrix. SPSS-compatible formula.
#' @keywords internal
.compute_bartlett <- function(cor_mat, n, k) {
  # Bartlett's test: chi_sq = -((n - 1) - (2*k + 5)/6) * log(det(R))
  log_det <- determinant(cor_mat, logarithm = TRUE)

  if (log_det$sign <= 0) {
    # Singular or near-singular matrix
    return(list(chi_sq = NA_real_, df = k * (k - 1) / 2, p_value = NA_real_))
  }

  log_det_val <- as.numeric(log_det$modulus)
  chi_sq <- -((n - 1) - (2 * k + 5) / 6) * log_det_val
  df <- k * (k - 1) / 2
  p_value <- stats::pchisq(chi_sq, df = df, lower.tail = FALSE)

  list(
    chi_sq = chi_sq,
    df = as.integer(df),
    p_value = p_value
  )
}


# ============================================================================
# ITEM DESCRIPTIVE STATISTICS
# ============================================================================

#' Compute per-item descriptive statistics for EFA
#' @keywords internal
.efa_item_stats <- function(data, var_names, weights_vec) {
  stats_list <- lapply(var_names, function(v) {
    x <- data[[v]]
    valid <- !is.na(x)
    if (!is.null(weights_vec)) {
      valid <- valid & !is.na(weights_vec)
      w <- weights_vec[valid]
      xv <- x[valid]
      wm <- sum(xv * w) / sum(w)
      wvar <- sum(w * (xv - wm)^2) / (sum(w) - 1)
      tibble::tibble(
        variable = v,
        mean = wm,
        sd = sqrt(wvar),
        analysis_n = sum(w),
        missing_n = sum(!valid)
      )
    } else {
      xv <- x[valid]
      tibble::tibble(
        variable = v,
        mean = mean(xv),
        sd = stats::sd(xv),
        analysis_n = length(xv),
        missing_n = sum(!valid)
      )
    }
  })
  dplyr::bind_rows(stats_list)
}


# ============================================================================
# PRINT METHOD
# ============================================================================

#' @export
print.efa <- function(x, digits = 3, ...) {

  # Header
  rotation_label <- switch(x$rotation %||% "varimax",
    "varimax" = "Varimax",
    "oblimin" = "Oblimin",
    "none" = "Unrotated"
  )
  title <- get_standard_title(
    paste0("Exploratory Factor Analysis (", rotation_label, ")"),
    x$weights,
    "Results"
  )
  print_header(title)

  if (x$is_grouped) {
    .print_efa_grouped(x, digits)
  } else {
    .print_efa_ungrouped(x, digits)
  }

  invisible(x)
}

#' Print EFA results for ungrouped data
#' @keywords internal
.print_efa_ungrouped <- function(x, digits = 3) {

  blank <- x$blank %||% 0.40
  sort_loadings <- x$sort %||% TRUE

  # Info section
  print_info_section(list(
    "Variables" = paste(x$variables, collapse = ", "),
    "Extraction" = "Principal Component Analysis",
    "Rotation" = switch(x$rotation,
      "varimax" = "Varimax with Kaiser Normalization",
      "oblimin" = "Oblimin with Kaiser Normalization",
      "none" = "None"
    ),
    "N of Factors" = as.character(x$n_factors),
    "Weights" = x$weights
  ))

  # KMO and Bartlett's Test
  cat("\n")
  cat("KMO and Bartlett's Test\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  cat(sprintf("  Kaiser-Meyer-Olkin Measure:     %s\n",
              format(round(x$kmo$overall, digits), nsmall = digits)))
  cat(sprintf("  Bartlett's Chi-Square:          %.3f\n", x$bartlett$chi_sq))
  cat(sprintf("  df:                             %d\n", x$bartlett$df))
  cat(sprintf("  Sig.:                           %.3f\n", x$bartlett$p_value))

  # Communalities
  cat("\nCommunalities\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  comm_df <- data.frame(
    variable = names(x$communalities),
    initial = rep(1.000, length(x$communalities)),
    extraction = round(as.numeric(x$communalities), digits),
    stringsAsFactors = FALSE
  )
  print(comm_df, row.names = FALSE)
  cat("Extraction Method: Principal Component Analysis.\n")

  # Total Variance Explained
  cat("\nTotal Variance Explained\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")

  ve <- x$variance_explained
  n_f <- x$n_factors

  for (i in seq_len(nrow(ve))) {
    if (i <= n_f) {
      # Show both initial and extraction columns
      cat(sprintf("  PC%d  Eigenvalue: %s  Variance: %s%%  Cumulative: %s%%\n",
                  i,
                  format(round(ve$eigenvalue[i], digits), nsmall = digits),
                  format(round(ve$prc_variance[i], digits), nsmall = digits),
                  format(round(ve$cumulative_prc[i], digits), nsmall = digits)))
    } else {
      cat(sprintf("  PC%d  Eigenvalue: %s  Variance: %s%%  Cumulative: %s%%\n",
                  i,
                  format(round(ve$eigenvalue[i], digits), nsmall = digits),
                  format(round(ve$prc_variance[i], digits), nsmall = digits),
                  format(round(ve$cumulative_prc[i], digits), nsmall = digits)))
    }
  }

  # Rotation sums if available
  if (!is.null(x$rotation_variance) && x$rotation != "none") {
    cat("\nRotation Sums of Squared Loadings\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
    rv <- x$rotation_variance
    for (i in seq_len(nrow(rv))) {
      if ("prc_variance" %in% names(rv)) {
        cat(sprintf("  PC%d  SS Loading: %s  Variance: %s%%  Cumulative: %s%%\n",
                    i,
                    format(round(rv$ss_loading[i], digits), nsmall = digits),
                    format(round(rv$prc_variance[i], digits), nsmall = digits),
                    format(round(rv$cumulative_prc[i], digits), nsmall = digits)))
      } else {
        # Oblimin: only SS loadings (no cumulative)
        cat(sprintf("  PC%d  SS Loading: %s\n",
                    i,
                    format(round(rv$ss_loading[i], digits), nsmall = digits)))
      }
    }
  }

  # Component Matrix (unrotated)
  cat("\nComponent Matrix (unrotated)\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  .print_loading_matrix(x$unrotated_loadings, blank, sort_loadings, digits)
  cat("Extraction Method: Principal Component Analysis.\n")

  # Rotated loadings
  if (x$rotation == "varimax") {
    cat("\nRotated Component Matrix\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
    .print_loading_matrix(x$loadings, blank, sort_loadings, digits)
    cat("Extraction Method: Principal Component Analysis.\n")
    cat("Rotation Method: Varimax with Kaiser Normalization.\n")

  } else if (x$rotation == "oblimin") {
    cat("\nPattern Matrix\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
    .print_loading_matrix(x$pattern_matrix, blank, sort_loadings, digits)
    cat("Extraction Method: Principal Component Analysis.\n")
    cat("Rotation Method: Oblimin with Kaiser Normalization.\n")

    cat("\nStructure Matrix\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
    .print_loading_matrix(x$structure_matrix, blank, sort_loadings, digits)

    cat("\nComponent Correlation Matrix\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
    fc <- round(x$factor_correlations, digits)
    print(fc, quote = FALSE)
  }
}

#' Print EFA results for grouped data
#' @keywords internal
.print_efa_grouped <- function(x, digits = 3) {

  for (group_result in x$groups) {
    # Group header
    group_values <- group_result$group_values
    print_group_header(group_values)

    # Create a temporary ungrouped-like structure for printing
    temp <- c(group_result, list(
      variables = x$variables,
      weights = x$weights,
      sort = x$sort,
      blank = x$blank
    ))

    .print_efa_ungrouped(temp, digits)
  }
}

#' Print a loading matrix with blank suppression and optional sorting
#' @keywords internal
.print_loading_matrix <- function(mat, blank = 0.40, sort_loadings = TRUE, digits = 3) {
  k <- nrow(mat)
  n_f <- ncol(mat)

  # Determine display order
  if (sort_loadings && n_f > 1) {
    # Sort by maximum absolute loading per factor
    max_factor <- apply(abs(mat), 1, which.max)
    max_loading <- apply(abs(mat), 1, max)
    # Sort by factor assignment first, then by loading magnitude (descending)
    ord <- order(max_factor, -max_loading)
  } else if (sort_loadings) {
    ord <- order(-abs(mat[, 1]))
  } else {
    ord <- seq_len(k)
  }

  # Format the matrix
  display_mat <- matrix("", nrow = k, ncol = n_f)
  rownames(display_mat) <- rownames(mat)
  colnames(display_mat) <- colnames(mat)

  for (i in seq_len(k)) {
    for (j in seq_len(n_f)) {
      val <- mat[i, j]
      if (abs(val) >= blank) {
        display_mat[i, j] <- format(round(val, digits), nsmall = digits, width = 7)
      } else {
        display_mat[i, j] <- format("", width = 7)
      }
    }
  }

  # Print in sorted order
  display_mat <- display_mat[ord, , drop = FALSE]
  print(display_mat, quote = FALSE, right = TRUE)
}
