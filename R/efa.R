
#' Explore the Structure Behind Your Survey Items
#'
#' @description
#' \code{efa()} performs Exploratory Factor Analysis (EFA) to discover underlying
#' patterns in your survey items. Supports both Principal Component Analysis (PCA)
#' and Maximum Likelihood (ML) extraction. This is the R equivalent of SPSS's
#' \code{FACTOR} procedure.
#'
#' For example, if you have 6 items measuring different attitudes, EFA can reveal
#' whether they group into 2-3 underlying dimensions (factors or components).
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The items to analyze. Use bare column names separated by commas,
#'   or tidyselect helpers like \code{starts_with("trust")}.
#' @param n_factors Number of components to extract. Default \code{NULL} uses
#'   the Kaiser criterion (eigenvalue > 1).
#' @param rotation Rotation method: \code{"varimax"} (default, orthogonal),
#'   \code{"oblimin"} (oblique, allows correlated factors),
#'   \code{"promax"} (oblique, power-based), or \code{"none"}.
#' @param extraction Extraction method: \code{"pca"} (default, Principal
#'   Component Analysis) or \code{"ml"} (Maximum Likelihood, enables
#'   goodness-of-fit testing, assumes multivariate normality).
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
#'   \item{initial_communalities}{Initial communalities (1.0 for PCA, SMC for ML)}
#'   \item{goodness_of_fit}{Goodness-of-fit test (ML only): chi_sq, df, p_value. NULL for PCA.}
#'   \item{uniquenesses}{Unique variances per variable (ML only, NULL for PCA)}
#'   \item{pattern_matrix}{Pattern matrix (oblimin/promax only, NULL otherwise)}
#'   \item{structure_matrix}{Structure matrix (oblimin/promax only, NULL otherwise)}
#'   \item{factor_correlations}{Factor correlation matrix (oblimin/promax only, NULL otherwise)}
#'   \item{variables}{Character vector of variable names}
#'   \item{weights}{Weights variable name or NULL}
#'   \item{n}{Sample size}
#'   \item{col_prefix}{Column name prefix: \code{"PC"} for PCA, \code{"Factor"} for ML}
#'   \item{sort}{Whether loadings are sorted}
#'   \item{blank}{Suppression threshold}
#' }
#'   Use \code{summary()} for the full SPSS-style output with toggleable sections.
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
#' ## Choosing an Extraction Method
#'
#' \itemize{
#'   \item \strong{PCA} (default): Extracts components explaining maximum total
#'     variance. Simple and robust. Does not assume normality.
#'   \item \strong{ML}: Extracts factors explaining shared variance only. Assumes
#'     multivariate normality. Provides a goodness-of-fit test to evaluate model
#'     fit. A non-significant chi-square (p > .05) suggests adequate fit.
#' }
#'
#' ## Choosing a Rotation
#'
#' \itemize{
#'   \item \strong{Varimax} (default): Assumes factors are uncorrelated.
#'     Produces simpler, easier-to-interpret results.
#'   \item \strong{Oblimin}: Allows factors to be correlated.
#'     More realistic for social science data. Produces both a Pattern Matrix
#'     (unique contributions) and Structure Matrix (total correlations).
#'   \item \strong{Promax}: Oblique rotation based on a power transformation
#'     of Varimax results. Like Oblimin, produces Pattern and Structure matrices.
#'     Common alternative to Oblimin in SPSS.
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
#' # Maximum Likelihood extraction
#' efa(survey_data,
#'     political_orientation, environmental_concern, life_satisfaction,
#'     trust_government, trust_media, trust_science,
#'     extraction = "ml")
#'
#' # Promax rotation (oblique)
#' efa(survey_data,
#'     political_orientation, environmental_concern, life_satisfaction,
#'     trust_government, trust_media, trust_science,
#'     rotation = "promax")
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
#' # --- Three-layer output ---
#' result <- efa(survey_data, political_orientation, environmental_concern,
#'               life_satisfaction, trust_government, trust_media, trust_science)
#' result              # compact overview
#' summary(result)     # full detailed output with all sections
#' summary(result, communalities = FALSE)  # hide communalities table
#'
#' @seealso
#' \code{\link{reliability}} for checking scale reliability before creating indices.
#'
#' \code{\link{scale_index}} for creating mean indices after identifying factors.
#'
#' \code{\link{summary.efa}} for detailed output with toggleable sections.
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
  rotation <- match.arg(rotation, c("varimax", "oblimin", "promax", "none"))

  # Validate extraction
  extraction <- match.arg(extraction, c("pca", "ml"))

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

  # Validate ML degrees of freedom constraint
  if (extraction == "ml" && !is.null(n_factors)) {
    ml_max <- .ml_max_factors(length(var_names))
    if (n_factors > ml_max) {
      cli_abort(c(
        "{.arg n_factors} = {n_factors} is too many for ML extraction with {length(var_names)} variables.",
        "i" = "Maximum factors for ML extraction: {.val {ml_max}}.",
        "i" = "Use {.code extraction = \"pca\"} for more factors, or reduce {.arg n_factors}."
      ))
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
  # EXTRACTION (PCA or ML)
  # ============================================================================

  if (extraction == "pca") {
    ext <- .efa_extract_pca(cor_mat, k, n_factors, var_names)
  } else if (extraction == "ml") {
    ext <- .efa_extract_ml(cor_mat, k, n_factors, n_bartlett, var_names)
  }

  raw_loadings <- ext$raw_loadings
  eigenvalues <- ext$eigenvalues
  n_factors_used <- ext$n_factors_used
  communalities <- ext$communalities
  variance_explained <- ext$variance_explained
  col_prefix <- ext$col_prefix
  total_var <- k  # For correlation matrix, total variance = k

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
    colnames(rotated_loadings) <- paste0(col_prefix, seq_len(n_factors_used))
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
    colnames(pattern_matrix) <- paste0(col_prefix, seq_len(n_factors_used))
    class(pattern_matrix) <- "matrix"

    # Factor correlation matrix (Phi)
    factor_correlations <- ob$Phi
    rownames(factor_correlations) <- colnames(factor_correlations) <- paste0(col_prefix, seq_len(n_factors_used))

    # Structure matrix = Pattern * Phi
    structure_matrix <- pattern_matrix %*% factor_correlations
    rownames(structure_matrix) <- var_names
    colnames(structure_matrix) <- paste0(col_prefix, seq_len(n_factors_used))

    # For oblimin, the "loadings" returned are the pattern matrix
    rotated_loadings <- pattern_matrix
    rotation_used <- "oblimin"

    # Rotation sums of squared loadings (for oblimin, just SS of pattern)
    rot_ss <- colSums(pattern_matrix^2)
    rotation_variance <- tibble::tibble(
      component = seq_len(n_factors_used),
      ss_loading = rot_ss
    )

  } else if (rotation == "promax") {
    pm <- stats::promax(raw_loadings, m = 4)  # m=4 matches SPSS Kappa=4 default
    pattern_matrix <- unclass(pm$loadings)
    rownames(pattern_matrix) <- var_names
    colnames(pattern_matrix) <- paste0(col_prefix, seq_len(n_factors_used))

    # Compute factor correlations (Phi) from rotation matrix
    rotmat <- pm$rotmat
    Phi_raw <- solve(t(rotmat) %*% rotmat)
    D <- diag(1 / sqrt(diag(Phi_raw)))
    factor_correlations <- D %*% Phi_raw %*% D
    rownames(factor_correlations) <- colnames(factor_correlations) <-
      paste0(col_prefix, seq_len(n_factors_used))

    # Structure matrix = Pattern * Phi
    structure_matrix <- pattern_matrix %*% factor_correlations
    rownames(structure_matrix) <- var_names
    colnames(structure_matrix) <- paste0(col_prefix, seq_len(n_factors_used))

    # For promax, the "loadings" returned are the pattern matrix
    rotated_loadings <- pattern_matrix
    rotation_used <- "promax"

    # Rotation sums of squared loadings (oblique: SS only, no cumulative)
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
    initial_communalities = ext$initial_communalities,
    kmo = kmo_result,
    bartlett = bartlett_result,
    goodness_of_fit = ext$goodness_of_fit,
    uniquenesses = ext$uniquenesses,
    rotation = rotation_used,
    extraction = extraction,
    n_factors = n_factors_used,
    correlation_matrix = cor_mat,
    pattern_matrix = pattern_matrix,
    structure_matrix = structure_matrix,
    factor_correlations = factor_correlations,
    item_statistics = item_stats,
    n = n_bartlett,
    col_prefix = col_prefix
  )
}


# ============================================================================
# EXTRACTION METHODS
# ============================================================================

#' PCA extraction for EFA
#' @description Extracts components using eigenvalue decomposition (SPSS /EXTRACTION PC)
#' @keywords internal
.efa_extract_pca <- function(cor_mat, k, n_factors, var_names) {
  eig <- eigen(cor_mat, symmetric = TRUE)
  eigenvalues <- eig$values
  eigenvectors <- eig$vectors

  # Determine number of factors (Kaiser criterion)
  if (is.null(n_factors)) {
    n_factors_used <- sum(eigenvalues > 1)
    if (n_factors_used == 0) n_factors_used <- 1
  } else {
    n_factors_used <- n_factors
  }

  # Variance explained table
  total_var <- k
  prc_var <- eigenvalues / total_var * 100
  cum_prc <- cumsum(prc_var)

  variance_explained <- tibble::tibble(
    component = seq_along(eigenvalues),
    eigenvalue = eigenvalues,
    prc_variance = prc_var,
    cumulative_prc = cum_prc
  )

  # Unrotated component matrix
  raw_loadings <- eigenvectors[, seq_len(n_factors_used), drop = FALSE] %*%
    diag(sqrt(eigenvalues[seq_len(n_factors_used)]), nrow = n_factors_used)
  rownames(raw_loadings) <- var_names
  colnames(raw_loadings) <- paste0("PC", seq_len(n_factors_used))

  # Communalities from extraction
  communalities <- rowSums(raw_loadings^2)
  names(communalities) <- var_names

  # Initial communalities are always 1.0 for PCA
  initial_communalities <- stats::setNames(rep(1.0, length(var_names)), var_names)

  list(
    raw_loadings = raw_loadings,
    eigenvalues = eigenvalues,
    n_factors_used = n_factors_used,
    communalities = communalities,
    variance_explained = variance_explained,
    initial_communalities = initial_communalities,
    goodness_of_fit = NULL,
    uniquenesses = NULL,
    col_prefix = "PC"
  )
}


#' ML extraction for EFA
#' @description Maximum Likelihood extraction using stats::factanal()
#'   (SPSS /EXTRACTION ML). Provides goodness-of-fit testing.
#' @keywords internal
.efa_extract_ml <- function(cor_mat, k, n_factors, n_obs, var_names) {
  # Eigenvalues from correlation matrix (for variance explained table)
  eig <- eigen(cor_mat, symmetric = TRUE)
  eigenvalues <- eig$values

  # Determine number of factors (Kaiser criterion)
  if (is.null(n_factors)) {
    n_factors_used <- sum(eigenvalues > 1)
    if (n_factors_used == 0) n_factors_used <- 1
  } else {
    n_factors_used <- n_factors
  }

  # Check ML degrees-of-freedom constraint
  ml_max <- .ml_max_factors(k)
  if (n_factors_used > ml_max) {
    cli_warn(c(
      "Kaiser criterion suggests {n_factors_used} factors, but ML extraction supports at most {ml_max} with {k} variables.",
      "i" = "Reducing to {ml_max} factor{?s}."
    ))
    n_factors_used <- ml_max
  }

  # Call factanal with correlation matrix (supports weighted data via covmat)
  fa_result <- tryCatch(
    stats::factanal(factors = n_factors_used, covmat = cor_mat,
                    n.obs = as.integer(round(n_obs)), rotation = "none"),
    error = function(e) {
      cli_abort(c(
        "ML extraction failed: {e$message}",
        "i" = "Try {.code extraction = \"pca\"} or reduce the number of factors."
      ))
    }
  )

  # Extract unrotated loadings
  raw_loadings <- unclass(fa_result$loadings)
  rownames(raw_loadings) <- var_names
  colnames(raw_loadings) <- paste0("Factor", seq_len(n_factors_used))

  # Communalities = 1 - uniquenesses
  uniquenesses <- fa_result$uniquenesses
  communalities <- 1 - uniquenesses
  names(communalities) <- var_names
  names(uniquenesses) <- var_names

  # Initial communalities = SMC (squared multiple correlations)
  # SMC = 1 - 1/diag(R^-1)
  inv_diag <- tryCatch(
    diag(solve(cor_mat)),
    error = function(e) {
      # Fallback for singular matrices
      rep(NA_real_, k)
    }
  )
  initial_communalities <- 1 - 1 / inv_diag
  names(initial_communalities) <- var_names

  # Variance explained table (eigenvalues from correlation matrix)
  total_var <- k
  prc_var <- eigenvalues / total_var * 100
  cum_prc <- cumsum(prc_var)

  variance_explained <- tibble::tibble(
    component = seq_along(eigenvalues),
    eigenvalue = eigenvalues,
    prc_variance = prc_var,
    cumulative_prc = cum_prc
  )

  # Goodness-of-fit from factanal
  goodness_of_fit <- NULL
  if (!is.null(fa_result$STATISTIC) && !is.null(fa_result$PVAL)) {
    goodness_of_fit <- list(
      chi_sq = as.numeric(fa_result$STATISTIC),
      df = as.integer(fa_result$dof),
      p_value = as.numeric(fa_result$PVAL)
    )
  }

  list(
    raw_loadings = raw_loadings,
    eigenvalues = eigenvalues,
    n_factors_used = n_factors_used,
    communalities = communalities,
    variance_explained = variance_explained,
    initial_communalities = initial_communalities,
    goodness_of_fit = goodness_of_fit,
    uniquenesses = uniquenesses,
    col_prefix = "Factor"
  )
}


#' Compute maximum number of factors for ML extraction
#' @description ML requires non-negative degrees of freedom:
#'   df = ((p - f)^2 - p - f) / 2 >= 0
#' @keywords internal
.ml_max_factors <- function(p) {
  for (f in seq_len(p)) {
    dof <- ((p - f)^2 - p - f) / 2
    if (dof < 0) return(as.integer(f - 1L))
  }
  return(as.integer(p))
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
# HELPERS
# ============================================================================

#' Interpret KMO value
#' @keywords internal
.kmo_interpretation <- function(kmo) {
  if (is.na(kmo)) return("")
  if (kmo >= 0.90) return("Marvelous")
  if (kmo >= 0.80) return("Meritorious")
  if (kmo >= 0.70) return("Middling")
  if (kmo >= 0.60) return("Mediocre")
  if (kmo >= 0.50) return("Miserable")
  "Unacceptable"
}


# ============================================================================
# PRINT METHOD (compact)
# ============================================================================

#' Print EFA results (compact)
#'
#' @description
#' Compact print method for objects of class \code{"efa"}.
#' Shows KMO value, number of factors, total variance explained,
#' extraction method, and rotation in a concise format.
#'
#' For the full detailed output including communalities, variance
#' explained per factor, and rotated component matrices, use \code{summary()}.
#'
#' @param x An object of class \code{"efa"} returned by \code{\link{efa}}.
#' @param digits Number of decimal places to display. Default is \code{3}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- efa(survey_data, political_orientation, environmental_concern,
#'               life_satisfaction, trust_government, trust_media, trust_science)
#' result              # compact overview
#' summary(result)     # full detailed output
#'
#' @export
#' @method print efa
print.efa <- function(x, digits = 3, ...) {
  weighted_tag <- if (!is.null(x$weights)) " [Weighted]" else ""

  extraction_label <- switch(x$extraction %||% "pca",
    "pca" = "PCA",
    "ml" = "ML",
    "paf" = "PAF"
  )
  rotation_label <- switch(x$rotation %||% "varimax",
    "varimax" = "Varimax",
    "oblimin" = "Oblimin",
    "promax" = "Promax",
    "none" = "Unrotated"
  )

  if (isTRUE(x$is_grouped)) {
    for (group_result in x$groups) {
      group_values <- group_result$group_values
      group_label <- paste(names(group_values), "=", group_values, collapse = ", ")
      cat(sprintf("[%s]\n", group_label))
      .print_efa_compact(group_result, length(x$variables), extraction_label,
                         rotation_label, weighted_tag, digits)
    }
  } else {
    .print_efa_compact(x, length(x$variables), extraction_label,
                       rotation_label, weighted_tag, digits)
  }

  invisible(x)
}

#' Print compact one-liner for a single EFA result
#' @keywords internal
.print_efa_compact <- function(res, n_vars, extraction_label, rotation_label,
                               weighted_tag, digits) {
  n_factors <- res$n_factors
  component_label <- if (n_factors == 1) "component" else "components"

  kmo <- res$kmo$overall
  kmo_interp <- .kmo_interpretation(kmo)

  # Total variance explained by extracted components
  ve <- res$variance_explained
  total_var_pct <- ve$cumulative_prc[min(n_factors, nrow(ve))]

  cat(sprintf("Exploratory Factor Analysis: %d items, %d %s (%s/%s)%s\n",
              n_vars, n_factors, component_label,
              extraction_label, rotation_label, weighted_tag))
  cat(sprintf("  KMO = %s (%s), Variance explained: %s%%\n",
              format(round(kmo, digits), nsmall = digits),
              kmo_interp,
              format(round(total_var_pct, 1), nsmall = 1)))
}


# ============================================================================
# SUMMARY METHOD
# ============================================================================

#' Summarize an exploratory factor analysis
#'
#' @description
#' Creates a detailed summary of an EFA result. All sections are shown by
#' default; set individual toggles to \code{FALSE} to suppress specific sections.
#'
#' @param object An \code{efa} result object
#' @param kmo_bartlett Show KMO and Bartlett's test? (Default: TRUE)
#' @param communalities Show communalities table? (Default: TRUE)
#' @param variance_explained Show total variance explained? (Default: TRUE)
#' @param unrotated_matrix Show unrotated component/factor matrix? (Default: TRUE)
#' @param rotated_matrix Show rotated matrix (varimax)? (Default: TRUE)
#' @param pattern_matrix Show pattern matrix (oblimin/promax)? (Default: TRUE)
#' @param structure_matrix Show structure matrix (oblimin/promax)? (Default: TRUE)
#' @param factor_correlations Show factor correlation matrix (oblimin/promax)? (Default: TRUE)
#' @param digits Number of decimal places (Default: 3)
#' @param ... Additional arguments (ignored)
#'
#' @return A \code{summary.efa} object (list with \code{$show} toggles)
#'
#' @examples
#' result <- efa(survey_data, political_orientation, environmental_concern,
#'              life_satisfaction, trust_government, trust_media, trust_science)
#' summary(result)
#' summary(result, communalities = FALSE)
#'
#' @seealso \code{\link{efa}} for the main analysis function.
#' @export
#' @method summary efa
summary.efa <- function(object, kmo_bartlett = TRUE, communalities = TRUE,
                        variance_explained = TRUE, unrotated_matrix = TRUE,
                        rotated_matrix = TRUE, pattern_matrix = TRUE,
                        structure_matrix = TRUE, factor_correlations = TRUE,
                        digits = 3, ...) {
  show <- list(
    kmo_bartlett = kmo_bartlett,
    communalities = communalities,
    variance_explained = variance_explained,
    unrotated_matrix = unrotated_matrix,
    rotated_matrix = rotated_matrix,
    pattern_matrix = pattern_matrix,
    structure_matrix = structure_matrix,
    factor_correlations = factor_correlations
  )
  build_summary_object(object, show, digits, "summary.efa")
}


#' Print summary of EFA results (detailed output)
#'
#' @description
#' Displays the detailed SPSS-style output for an Exploratory Factor Analysis,
#' with sections controlled by the boolean parameters passed to
#' \code{\link{summary.efa}}.  Sections include KMO and Bartlett's test,
#' communalities, variance explained, and rotated component/pattern matrices.
#'
#' @param x A \code{summary.efa} object created by
#'   \code{\link{summary.efa}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- efa(survey_data, political_orientation, environmental_concern,
#'               life_satisfaction, trust_government, trust_media, trust_science)
#' summary(result)                        # all sections
#' summary(result, communalities = FALSE) # hide communalities
#'
#' @seealso \code{\link{efa}} for the main analysis,
#'   \code{\link{summary.efa}} for summary options.
#' @export
#' @method print summary.efa
print.summary.efa <- function(x, ...) {
  extraction_label <- switch(x$extraction %||% "pca",
    "pca" = "PCA",
    "ml" = "Maximum Likelihood",
    "paf" = "Principal Axis Factoring"
  )
  rotation_label <- switch(x$rotation %||% "varimax",
    "varimax" = "Varimax",
    "oblimin" = "Oblimin",
    "promax" = "Promax",
    "none" = "Unrotated"
  )
  title <- get_standard_title(
    paste0("Exploratory Factor Analysis (", extraction_label, ", ", rotation_label, ")"),
    x$weights,
    "Results"
  )
  print_header(title)

  if (isTRUE(x$is_grouped)) {
    .print_efa_grouped(x, x$digits)
  } else {
    .print_efa_ungrouped(x, x$digits)
  }

  invisible(x)
}

#' Print EFA results for ungrouped data
#' @keywords internal
.print_efa_ungrouped <- function(x, digits = 3) {
  show_kmo <- if (!is.null(x$show)) isTRUE(x$show$kmo_bartlett) else TRUE
  show_comm <- if (!is.null(x$show)) isTRUE(x$show$communalities) else TRUE
  show_var <- if (!is.null(x$show)) isTRUE(x$show$variance_explained) else TRUE
  show_unrotated <- if (!is.null(x$show)) isTRUE(x$show$unrotated_matrix) else TRUE
  show_rotated <- if (!is.null(x$show)) isTRUE(x$show$rotated_matrix) else TRUE
  show_pattern <- if (!is.null(x$show)) isTRUE(x$show$pattern_matrix) else TRUE
  show_structure <- if (!is.null(x$show)) isTRUE(x$show$structure_matrix) else TRUE
  show_factor_cor <- if (!is.null(x$show)) isTRUE(x$show$factor_correlations) else TRUE

  blank <- x$blank %||% 0.40
  sort_loadings <- x$sort %||% TRUE

  # Dynamic labels based on extraction method
  extraction_full <- switch(x$extraction %||% "pca",
    "pca" = "Principal Component Analysis",
    "ml" = "Maximum Likelihood",
    "paf" = "Principal Axis Factoring"
  )
  matrix_label <- if ((x$extraction %||% "pca") == "pca") "Component" else "Factor"
  prefix <- x$col_prefix %||% "PC"

  # Info section
  print_info_section(list(
    "Variables" = paste(x$variables, collapse = ", "),
    "Extraction" = extraction_full,
    "Rotation" = switch(x$rotation,
      "varimax" = "Varimax with Kaiser Normalization",
      "oblimin" = "Oblimin with Kaiser Normalization",
      "promax" = "Promax with Kaiser Normalization",
      "none" = "None"
    ),
    "N of Factors" = as.character(x$n_factors),
    "Weights" = x$weights
  ))

  # KMO and Bartlett's Test
  if (show_kmo) {
    cat("\n")
    cat("KMO and Bartlett's Test\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
    cat(sprintf("  Kaiser-Meyer-Olkin Measure:     %s\n",
                format(round(x$kmo$overall, digits), nsmall = digits)))
    cat(sprintf("  Bartlett's Chi-Square:          %.3f\n", x$bartlett$chi_sq))
    cat(sprintf("  df:                             %d\n", x$bartlett$df))
    cat(sprintf("  Sig.:                           %.3f\n", x$bartlett$p_value))

    # Goodness-of-fit Test (ML only)
    if (!is.null(x$goodness_of_fit)) {
      cat("\nGoodness-of-fit Test\n")
      cat(paste(rep("-", 40), collapse = ""), "\n")
      cat(sprintf("  Chi-Square:                     %.3f\n", x$goodness_of_fit$chi_sq))
      cat(sprintf("  df:                             %d\n", x$goodness_of_fit$df))
      cat(sprintf("  Sig.:                           %.3f\n", x$goodness_of_fit$p_value))
    }
  }

  # Communalities
  if (show_comm) {
    cat("\nCommunalities\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")

    # Initial communalities: 1.0 for PCA, SMC for ML/PAF
    initial_vals <- if (!is.null(x$initial_communalities)) {
      round(as.numeric(x$initial_communalities[names(x$communalities)]), digits)
    } else {
      rep(1.000, length(x$communalities))
    }

    comm_df <- data.frame(
      variable = names(x$communalities),
      initial = initial_vals,
      extraction = round(as.numeric(x$communalities), digits),
      stringsAsFactors = FALSE
    )
    print(comm_df, row.names = FALSE)
    cat(sprintf("Extraction Method: %s.\n", extraction_full))
  }

  # Total Variance Explained
  if (show_var) {
    cat("\nTotal Variance Explained\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")

    ve <- x$variance_explained
    n_f <- x$n_factors

    for (i in seq_len(nrow(ve))) {
      cat(sprintf("  %s%d  Eigenvalue: %s  Variance: %s%%  Cumulative: %s%%\n",
                  prefix, i,
                  format(round(ve$eigenvalue[i], digits), nsmall = digits),
                  format(round(ve$prc_variance[i], digits), nsmall = digits),
                  format(round(ve$cumulative_prc[i], digits), nsmall = digits)))
    }

    # Rotation sums if available
    if (!is.null(x$rotation_variance) && x$rotation != "none") {
      cat("\nRotation Sums of Squared Loadings\n")
      cat(paste(rep("-", 40), collapse = ""), "\n")
      rv <- x$rotation_variance
      for (i in seq_len(nrow(rv))) {
        if ("prc_variance" %in% names(rv)) {
          cat(sprintf("  %s%d  SS Loading: %s  Variance: %s%%  Cumulative: %s%%\n",
                      prefix, i,
                      format(round(rv$ss_loading[i], digits), nsmall = digits),
                      format(round(rv$prc_variance[i], digits), nsmall = digits),
                      format(round(rv$cumulative_prc[i], digits), nsmall = digits)))
        } else {
          # Oblique rotation: only SS loadings (no cumulative)
          cat(sprintf("  %s%d  SS Loading: %s\n",
                      prefix, i,
                      format(round(rv$ss_loading[i], digits), nsmall = digits)))
        }
      }
    }
  }

  # Unrotated matrix
  if (show_unrotated) {
    cat(sprintf("\n%s Matrix (unrotated)\n", matrix_label))
    cat(paste(rep("-", 40), collapse = ""), "\n")
    .print_loading_matrix(x$unrotated_loadings, blank, sort_loadings, digits)
    cat(sprintf("Extraction Method: %s.\n", extraction_full))
  }

  # Rotated loadings
  if (x$rotation == "varimax") {
    if (show_rotated) {
      cat(sprintf("\nRotated %s Matrix\n", matrix_label))
      cat(paste(rep("-", 40), collapse = ""), "\n")
      .print_loading_matrix(x$loadings, blank, sort_loadings, digits)
      cat(sprintf("Extraction Method: %s.\n", extraction_full))
      cat("Rotation Method: Varimax with Kaiser Normalization.\n")
    }

  } else if (x$rotation %in% c("oblimin", "promax")) {
    rot_label <- if (x$rotation == "oblimin") "Oblimin" else "Promax"

    if (show_pattern) {
      cat("\nPattern Matrix\n")
      cat(paste(rep("-", 40), collapse = ""), "\n")
      .print_loading_matrix(x$pattern_matrix, blank, sort_loadings, digits)
      cat(sprintf("Extraction Method: %s.\n", extraction_full))
      cat(sprintf("Rotation Method: %s with Kaiser Normalization.\n", rot_label))
    }

    if (show_structure) {
      cat("\nStructure Matrix\n")
      cat(paste(rep("-", 40), collapse = ""), "\n")
      .print_loading_matrix(x$structure_matrix, blank, sort_loadings, digits)
    }

    if (show_factor_cor) {
      cat(sprintf("\n%s Correlation Matrix\n", matrix_label))
      cat(paste(rep("-", 40), collapse = ""), "\n")
      fc <- round(x$factor_correlations, digits)
      print(fc, quote = FALSE)
    }
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
