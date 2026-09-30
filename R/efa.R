
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
#' @param n_factors Number of components to extract (a whole number).
#'   Default \code{NULL} uses the Kaiser criterion (eigenvalue > 1). A single
#'   component cannot be rotated; it is shown unrotated with SPSS's note.
#' @param rotation Rotation method: \code{"varimax"} (default, orthogonal),
#'   \code{"oblimin"} (oblique direct oblimin, delta = 0, allows correlated
#'   factors), \code{"promax"} (oblique, power 4), or \code{"none"}. All
#'   rotations use Kaiser normalization and SPSS FACTOR's own algorithms and
#'   stopping rules (at most 25 iterations, as SPSS \code{/CRITERIA
#'   ITERATE(25)}), so they reproduce SPSS's rotated matrices; they differ
#'   slightly from \code{stats::varimax()}, \code{stats::promax()} and
#'   \code{GPArotation::oblimin()}.
#' @param extraction Extraction method: \code{"pca"} (default, Principal
#'   Component Analysis) or \code{"ml"} (Maximum Likelihood, enables
#'   goodness-of-fit testing, assumes multivariate normality).
#' @param weights Optional survey weights for population-representative results.
#' @param use How to handle missing data for correlation computation:
#'   \code{"pairwise"} (default, matches SPSS) or \code{"complete"} (listwise;
#'   \code{"listwise"} is accepted as an alias).
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
#'   \item{variance_explained}{Tibble with the initial eigenvalues: Total,
#'     % of Variance, Cumulative % (one row per variable)}
#'   \item{extraction_variance}{Tibble with the extraction sums of squared
#'     loadings of the extracted components/factors (SPSS "Extraction Sums
#'     of Squared Loadings"). For ML this is the variance the common factors
#'     explain - the figure the compact print reports.}
#'   \item{rotation_variance}{Tibble with rotation sums of squared loadings
#'     (for the oblique rotations the column sums of squares of the
#'     structure matrix, as SPSS reports them)}
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
#'   \item{rotation_iterations, rotation_converged}{Iterations of the
#'     rotation as SPSS counts them ("Rotation converged in 4 iterations";
#'     for promax those of its varimax step) and whether it converged;
#'     NULL without rotation}
#'   \item{variables}{Character vector of variable names}
#'   \item{variable_labels}{Named character vector with the variable labels
#'     (\code{NA} where a variable has none); \code{summary()} shows them
#'     next to the names, shortened to the console width in the tables}
#'   \item{weights}{Weights variable name or NULL}
#'   \item{item_statistics}{Tibble with mean, SD, analysis N and missing N
#'     per item. With \code{use = "pairwise"} each item uses its own valid
#'     cases; with \code{use = "complete"} all items use the complete cases.}
#'   \item{n}{Sample size: the smallest pairwise N (\code{use = "pairwise"},
#'     the N of Bartlett's test as in SPSS) or the number of complete cases
#'     (\code{use = "complete"}); the sum of weights when weighted}
#'   \item{use}{The missing-data handling used (\code{"pairwise"} or
#'     \code{"complete"})}
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
#' # With Oblimin rotation
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
#' \code{\link{row_means}} for creating mean indices after identifying factors.
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

  # Validate rotation, extraction, use (English errors; "listwise" is the
  # SPSS name for complete-case deletion and accepted as an alias)
  rotation <- .efa_match_choice(rotation, c("varimax", "oblimin", "promax", "none"),
                                "rotation")
  extraction <- .efa_match_choice(extraction, c("pca", "ml"), "extraction")
  if (identical(use, "listwise")) use <- "complete"
  use <- .efa_match_choice(use, c("pairwise", "complete"), "use",
                           note = "{.val listwise} is accepted as an alias of {.val complete}.")

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

  # Validate n_factors (2.7 used to be truncated to 2 silently)
  if (!is.null(n_factors)) {
    if (!is.numeric(n_factors) || length(n_factors) != 1 || is.na(n_factors) ||
        n_factors != round(n_factors)) {
      cli_abort(c(
        "{.arg n_factors} must be a whole number (or {.code NULL} for the Kaiser criterion).",
        "x" = "You supplied {.val {n_factors}}."
      ))
    }
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
  data <- weights_info$data

  # Variable labels (shown in summary(), as SPSS does)
  variable_labels <- .scale_item_labels(data, var_names)

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
      group_label <- .format_group_label(group_keys_df[i, , drop = FALSE])

      # A group whose correlation matrix is undefined (constant item, a
      # single case, ...) is skipped with a warning instead of aborting
      # the analysis of every other group.
      res <- tryCatch(
        .efa_core(
          group_data, var_names, group_weights, n_factors, rotation,
          extraction, use, sort, blank, na.rm, group_label = group_label
        ),
        mariposa_efa_undefined = function(e) e
      )
      if (inherits(res, "mariposa_efa_undefined")) {
        .efa_warn_skipped_group(group_label, res$reasons, res$headline)
        res <- list(not_computed = res$headline, reasons = res$reasons)
      }
      results_list[[i]] <- res
      results_list[[i]]$group_values <- as.list(group_keys_df[i, , drop = FALSE])
    }

    result <- list(
      groups = results_list,
      variables = var_names,
      variable_labels = variable_labels,
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
      variable_labels = variable_labels,
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
#' @noRd
.efa_core <- function(data, var_names, weights_vec, n_factors, rotation,
                      extraction, use, sort, blank, na.rm, group_label = NULL) {

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

  # An undefined correlation (constant item, no valid values, no cases in
  # common) makes every later step meaningless: stop with the reason.
  .efa_check_defined(cor_mat, data, var_names, weights_vec, use,
                     cor_result$n_cases)

  # A singular (not positive definite) matrix still has principal
  # components, but no KMO, Bartlett test or ML solution (SPSS: "This
  # matrix is not positive definite").
  pd <- .efa_pd_check(cor_mat, var_names, cor_result$n_cases)
  if (!pd$pd && extraction == "ml") {
    .efa_abort_undefined(
      c(pd$reasons,
        "ML extraction needs a positive definite correlation matrix."),
      headline = "the correlation matrix is not positive definite",
      hint = "Remove the redundant item(s) or use {.code extraction = \"pca\"}."
    )
  }
  .efa_warn_pd(pd, k, cor_result$n_cases, group_label)

  # ============================================================================
  # KMO AND BARTLETT'S TEST
  # ============================================================================

  kmo_result <- .compute_kmo(cor_mat, pd$pd)
  bartlett_result <- .compute_bartlett(cor_mat, n_bartlett, k, pd$pd)

  # ============================================================================
  # EXTRACTION (PCA or ML)
  # ============================================================================

  if (extraction == "pca") {
    ext <- .efa_extract_pca(cor_mat, k, n_factors, var_names)
  } else if (extraction == "ml") {
    ext <- .efa_extract_ml(cor_mat, k, n_factors, n_bartlett, var_names,
                           group_label)
  }

  raw_loadings <- ext$raw_loadings
  eigenvalues <- ext$eigenvalues
  n_factors_used <- ext$n_factors_used
  communalities <- ext$communalities
  variance_explained <- ext$variance_explained
  col_prefix <- ext$col_prefix
  total_var <- k  # For correlation matrix, total variance = k

  # Extraction Sums of Squared Loadings (SPSS "Total Variance Explained",
  # middle block). For PCA these equal the retained eigenvalues; for ML
  # they are the variance the common factors actually explain, which is
  # far below the eigenvalue share of the same number of components.
  ext_ss <- unname(colSums(raw_loadings^2))
  extraction_variance <- tibble::tibble(
    component = seq_len(n_factors_used),
    ss_loading = ext_ss,
    prc_variance = ext_ss / total_var * 100,
    cumulative_prc = cumsum(ext_ss / total_var * 100)
  )

  # ============================================================================
  # ROTATION
  # ============================================================================

  pattern_matrix <- NULL
  structure_matrix <- NULL
  factor_correlations <- NULL
  rotation_variance <- NULL
  rotation_iterations <- NULL
  rotation_converged <- NULL

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
    # SPSS's cyclic pairwise varimax (see .efa_varimax(); stats::varimax()
    # stopped early and missed SPSS's transformation matrix by up to .004)
    vm <- .efa_varimax(raw_loadings)
    .efa_warn_rotation(vm, "Varimax", group_label)
    rotation_iterations <- vm$iterations
    rotation_converged <- vm$converged
    rotated_loadings <- vm$loadings
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
    # SPSS's direct oblimin (see .efa_oblimin(); GPArotation::oblimin()
    # iterated to the exact optimum and differed from SPSS by up to .002)
    ob <- .efa_oblimin(raw_loadings)
    .efa_warn_rotation(ob, "Oblimin", group_label)
    rotation_iterations <- ob$iterations
    rotation_converged <- ob$converged
    pattern_matrix <- ob$pattern
    rownames(pattern_matrix) <- var_names
    colnames(pattern_matrix) <- paste0(col_prefix, seq_len(n_factors_used))

    # Factor correlation matrix (Phi)
    factor_correlations <- ob$phi
    rownames(factor_correlations) <- colnames(factor_correlations) <- paste0(col_prefix, seq_len(n_factors_used))

    # Structure matrix = Pattern * Phi
    structure_matrix <- pattern_matrix %*% factor_correlations
    rownames(structure_matrix) <- var_names
    colnames(structure_matrix) <- paste0(col_prefix, seq_len(n_factors_used))

    # For oblimin, the "loadings" returned are the pattern matrix
    rotated_loadings <- pattern_matrix
    rotation_used <- "oblimin"

    # Rotation sums of squared loadings (oblique: SS only, no cumulative),
    # from the structure matrix as in SPSS (efa_output.txt Test 1b:
    # 1.599 / 1.041 / 1.022; the pattern matrix gave 1.600 / 1.043 / 1.025)
    rotation_variance <- tibble::tibble(
      component = seq_len(n_factors_used),
      ss_loading = unname(colSums(structure_matrix^2))
    )

  } else if (rotation == "promax") {
    # SPSS FACTOR /ROTATION PROMAX(4) (Kaiser-normalized target; see
    # .efa_promax() for why stats::promax() differs)
    pm <- .efa_promax(raw_loadings, power = 4)
    .efa_warn_rotation(pm, "Promax", group_label)
    rotation_iterations <- pm$iterations
    rotation_converged <- pm$converged
    col_names <- paste0(col_prefix, seq_len(n_factors_used))
    pattern_matrix <- pm$pattern
    dimnames(pattern_matrix) <- list(var_names, col_names)
    factor_correlations <- pm$phi
    dimnames(factor_correlations) <- list(col_names, col_names)

    # Structure matrix = Pattern * Phi
    structure_matrix <- pattern_matrix %*% factor_correlations
    dimnames(structure_matrix) <- list(var_names, col_names)

    # For promax, the "loadings" returned are the pattern matrix
    rotated_loadings <- pattern_matrix
    rotation_used <- "promax"

    # Rotation sums of squared loadings (oblique: SS only, no cumulative).
    # SPSS takes them from the structure matrix (efa_ml_promax_output.txt P1:
    # 1.599 / 1.039 / 1.021); the pattern matrix overstates them.
    rotation_variance <- tibble::tibble(
      component = seq_len(n_factors_used),
      ss_loading = unname(colSums(structure_matrix^2))
    )
  }

  # ============================================================================
  # DESCRIPTIVE STATISTICS
  # ============================================================================

  # Per-variable descriptive stats (Analysis N follows `use`)
  item_stats <- .efa_item_stats(data, var_names, weights_vec, use)

  # ============================================================================
  # RETURN RESULT
  # ============================================================================

  list(
    loadings = rotated_loadings,
    unrotated_loadings = raw_loadings,
    eigenvalues = eigenvalues,
    variance_explained = variance_explained,
    extraction_variance = extraction_variance,
    rotation_variance = rotation_variance,
    communalities = communalities,
    initial_communalities = ext$initial_communalities,
    kmo = kmo_result,
    bartlett = bartlett_result,
    goodness_of_fit = ext$goodness_of_fit,
    uniquenesses = ext$uniquenesses,
    rotation = rotation_used,
    rotation_requested = rotation,
    rotation_iterations = rotation_iterations,
    rotation_converged = rotation_converged,
    extraction = extraction,
    n_factors = n_factors_used,
    correlation_matrix = cor_mat,
    pattern_matrix = pattern_matrix,
    structure_matrix = structure_matrix,
    factor_correlations = factor_correlations,
    item_statistics = item_stats,
    n = n_bartlett,
    use = use,
    col_prefix = col_prefix
  )
}


# ============================================================================
# EXTRACTION METHODS
# ============================================================================

#' PCA extraction for EFA
#' @description Extracts components using eigenvalue decomposition (SPSS /EXTRACTION PC)
#' @noRd
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
  # A singular matrix has (numerically) zero or tiny negative eigenvalues;
  # clamp them so that no NaN loadings arise when such a component is
  # requested explicitly.
  raw_loadings <- eigenvectors[, seq_len(n_factors_used), drop = FALSE] %*%
    diag(sqrt(pmax(eigenvalues[seq_len(n_factors_used)], 0)),
         nrow = n_factors_used)
  raw_loadings <- .efa_reflect(raw_loadings)
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
#' @noRd
.efa_extract_ml <- function(cor_mat, k, n_factors, n_obs, var_names,
                            group_label = NULL) {
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
    in_group <- if (is.null(group_label)) "" else paste0(" (group ", group_label, ")")
    cli_warn(c(
      "Kaiser criterion suggests {n_factors_used} factors{in_group}, but ML extraction supports at most {ml_max} with {k} variables.",
      "i" = "Reducing to {ml_max} factor{?s}."
    ))
    n_factors_used <- ml_max
  }

  # Call factanal with correlation matrix (supports weighted data via covmat)
  fa_result <- tryCatch(
    stats::factanal(factors = n_factors_used, covmat = cor_mat,
                    n.obs = as.integer(round(n_obs)), rotation = "none"),
    error = function(e) {
      .efa_abort_undefined(
        paste0("ML extraction failed: ", conditionMessage(e)),
        headline = "ML extraction failed",
        hint = "Try {.code extraction = \"pca\"} or reduce the number of factors."
      )
    }
  )

  # Extract unrotated loadings (factanal already reflects to positive
  # column sums; applied again so both extractions share one rule)
  raw_loadings <- .efa_reflect(unclass(fa_result$loadings))
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


#' Reflect extracted components/factors to a positive loading sum
#'
#' @description
#' An eigenvector (and hence a component) is only defined up to its sign,
#' and eigen() picks one arbitrarily: three positively correlated items
#' could come out with all-negative loadings, and the sign could differ
#' between groups. SPSS FACTOR reflects every extracted column so that its
#' loadings sum to a positive value; this reproduces the signs of every
#' Component/Factor, Rotated, Pattern and Structure matrix and every factor
#' correlation in the SPSS v29 reference output (efa_output.txt,
#' efa_ml_promax_output.txt). The reflection is applied to the unrotated
#' solution only: varimax, oblimin and promax are sign-equivariant, so the
#' rotated matrices and the factor correlations follow consistently (a
#' second reflection after rotation contradicts SPSS for the oblique
#' solutions).
#' @param L Unrotated loading matrix (variables x factors)
#' @return L with columns of negative sum multiplied by -1
#' @noRd
.efa_reflect <- function(L) {
  flip <- colSums(L) < 0
  L[, flip] <- -L[, flip]
  L
}


#' Promax rotation as SPSS FACTOR computes it
#'
#' @description
#' IBM SPSS Statistics Algorithms, FACTOR, "Promax Rotation"
#' (Hendrickson & White, 1964):
#' 1. varimax rotation with Kaiser normalization (.efa_varimax()): Lambda_R;
#' 2. target P with p_ij = |b_ij|^(k+1) / b_ij, where b_ij are the rows of
#'    Lambda_R normalized to unit length (Kaiser normalization);
#' 3. least-squares fit L = (Lambda_R' Lambda_R)^-1 Lambda_R' P;
#' 4. Q = L D with D = diag(L'L)^-1/2 (unit-length columns);
#' 5. C = diag((Q'Q)^-1)^-1/2; pattern = Lambda_R Q C^-1, factor
#'    correlations = C (Q'Q)^-1 C.
#' stats::promax() skips the row normalization in step 2 (its target is
#' built from the raw varimax loadings), which moved pattern loadings by up
#' to .03 and the factor correlations from SPSS -.002 / -.012 to .055 /
#' .155 (efa_ml_promax_output.txt, Test P1). This version reproduces every
#' PCA + promax pattern, structure and correlation matrix of that reference
#' run to its printed precision.
#' @param L Unrotated loading matrix (variables x factors, >= 2 factors)
#' @param power Promax power k (SPSS default 4)
#' @return list(pattern, phi, iterations, converged); iterations and
#'   convergence are those of the varimax step (SPSS's footnote)
#' @noRd
.efa_promax <- function(L, power = 4) {
  vm <- .efa_varimax(L)
  V <- vm$loadings
  h <- sqrt(rowSums(V^2))
  h[h == 0] <- 1                     # a variable without common variance
  B <- V / h
  P <- sign(B) * abs(B)^power        # = |b|^(k+1) / b, 0 for b = 0
  Lm <- solve(crossprod(V), crossprod(V, P))
  Q <- sweep(Lm, 2, sqrt(colSums(Lm^2)), "/")
  QQi <- solve(crossprod(Q))
  c_inv <- sqrt(diag(QQi))           # diagonal of C^-1
  pattern <- V %*% sweep(Q, 2, c_inv, "*")
  phi <- QQi / outer(c_inv, c_inv)
  diag(phi) <- 1
  list(pattern = unname(pattern), phi = unname(phi),
       iterations = vm$iterations, converged = vm$converged,
       maxit = vm$maxit)
}


#' Varimax rotation as SPSS FACTOR computes it
#'
#' @description
#' IBM SPSS Statistics Algorithms, FACTOR, "Orthogonal Rotations" (Kaiser's
#' cyclic algorithm, Harman 1976):
#' - rows normalized by the square root of the communalities (Kaiser);
#' - every iteration rotates each pair of factors (j < k) by the angle
#'   P = atan2(X, Y) / 4 with u = l_j^2 - l_k^2, v = 2 l_j l_k,
#'   X = D - 2AB/n, Y = C - (A^2 - B^2)/n (A = sum u, B = sum v,
#'   C = sum(u^2 - v^2), D = sum 2uv);
#' - iteration stops when the varimax criterion
#'   SV = sum_j (n sum_i l_ij^4 - (sum_i l_ij^2)^2) / n^2 grows by at most
#'   1e-5, or after `maxit` iterations (SPSS /CRITERIA ITERATE, default 25);
#'   SPSS counts the final check as an iteration;
#' - the rotated factors are de-normalized, reflected to a positive sum and
#'   ordered by their sums of squared loadings (descending).
#' stats::varimax() uses a different (SVD) algorithm with a relative stopping
#' rule that ends earlier: its transformation matrices missed SPSS's by up
#' to .004 (efa_output.txt Test 1d). This version reproduces every Component
#' Transformation Matrix and every rotation sum of squares of the SPSS
#' reference runs.
#' @param L Unrotated loading matrix (variables x factors, >= 2 factors)
#' @param maxit Maximum number of iterations (SPSS default 25)
#' @param eps Convergence criterion on the varimax criterion (SPSS: 1e-5)
#' @return list(loadings, rotmat, iterations, converged, maxit)
#' @noRd
.efa_varimax <- function(L, maxit = 25L, eps = 1e-5) {
  L <- unname(as.matrix(L))
  n <- nrow(L)
  m <- ncol(L)
  h <- sqrt(rowSums(L^2))
  h[h == 0] <- 1
  A <- L / h
  Tm <- diag(m)
  criterion <- function(A) sum(n * colSums(A^4) - colSums(A^2)^2) / n^2

  converged <- FALSE
  sv_old <- NA_real_
  iterations <- 0L
  for (iteration in seq_len(maxit)) {
    iterations <- iteration
    sv <- criterion(A)
    if (iteration > 1L && sv - sv_old <= eps) {
      converged <- TRUE
      break
    }
    sv_old <- sv
    for (j in seq_len(m - 1L)) {
      for (k in (j + 1L):m) {
        u <- A[, j]^2 - A[, k]^2
        v <- 2 * A[, j] * A[, k]
        a <- sum(u)
        b <- sum(v)
        X <- sum(2 * u * v) - 2 * a * b / n
        Y <- sum(u^2 - v^2) - (a^2 - b^2) / n
        angle <- atan2(X, Y) / 4
        if (abs(sin(angle)) <= 1e-15) next
        cs <- cos(angle)
        sn <- sin(angle)
        rot <- matrix(c(cs, sn, -sn, cs), 2)
        A[, c(j, k)] <- A[, c(j, k)] %*% rot
        Tm[, c(j, k)] <- Tm[, c(j, k)] %*% rot
      }
    }
  }
  R <- A * h
  flip <- colSums(R) < 0
  R[, flip] <- -R[, flip]
  Tm[, flip] <- -Tm[, flip]
  ord <- order(-colSums(R^2))
  list(loadings = R[, ord, drop = FALSE], rotmat = Tm[, ord, drop = FALSE],
       iterations = iterations, converged = converged, maxit = maxit)
}


#' Direct oblimin rotation (delta = 0) as SPSS FACTOR computes it
#'
#' @description
#' IBM SPSS Statistics Algorithms, FACTOR, "Oblique Rotations" (Jennrich &
#' Sampson, 1966), with Kaiser normalization:
#' - one factor p at a time is replaced by (f_p + a f_q) / sqrt(A),
#'   A = 1 + 2 a c_pq + a^2, for every other factor q: the pattern columns
#'   become sqrt(A) l_p and l_q - a l_p, the correlations of factor p
#'   (c_ip + a c_iq) / sqrt(A);
#' - a minimizes the quartimin criterion
#'   F = sum_i [(sum_j l_ij^2)^2 - sum_j l_ij^4] (a quartic in a, solved
#'   through the roots of its cubic derivative);
#' - iteration stops when an iteration lowers F by less than 1e-4 of its
#'   start value (SPSS RCONVERGE), or after `maxit` iterations.
#' GPArotation::oblimin() iterates to the exact optimum instead and differed
#' from SPSS by up to .002 (efa_output.txt Test 2b); this version reproduces
#' the pattern, structure and correlation matrices and SPSS's iteration
#' counts of all oblimin reference runs (and needs no extra package).
#' @param L Unrotated loading matrix (variables x factors, >= 2 factors)
#' @param maxit Maximum number of iterations (SPSS default 25)
#' @param eps Relative convergence criterion (SPSS RCONVERGE .0001)
#' @return list(pattern, phi, iterations, converged, maxit)
#' @noRd
.efa_oblimin <- function(L, maxit = 25L, eps = 1e-4) {
  L <- unname(as.matrix(L))
  m <- ncol(L)
  h <- sqrt(rowSums(L^2))
  h[h == 0] <- 1
  B <- L / h
  C <- diag(m)
  criterion <- function(B) {
    s <- rowSums(B^2)
    sum(s^2) - sum(B^4)
  }
  f_start <- criterion(B)
  f_old <- f_start

  converged <- FALSE
  iterations <- 0L
  for (iteration in seq_len(maxit)) {
    iterations <- iteration
    for (p in seq_len(m)) {
      for (q in seq_len(m)[-p]) {
        lp <- B[, p]
        lq <- B[, q]
        cpq <- C[p, q]
        s <- rowSums(B[, -c(p, q), drop = FALSE]^2)
        # F(a) - const = sum_i [ lp^2 A(a) Q(a) + s (lp^2 A(a) + Q(a)) ] with
        # A(a) = 1 + 2 cpq a + a^2 and Q(a) = (lq - a lp)^2
        lp2 <- lp^2
        b0 <- lq^2
        b1 <- -2 * lp * lq
        b2 <- lp2
        coef <- c(
          sum(lp2 * b0) + sum(s * (lp2 + b0)),
          sum(lp2 * (b1 + 2 * cpq * b0)) + sum(s * (2 * cpq * lp2 + b1)),
          sum(lp2 * (b2 + 2 * cpq * b1 + b0)) + sum(s * (lp2 + b2)),
          sum(lp2 * (2 * cpq * b2 + b1)),
          sum(lp2 * b2)
        )
        if (coef[5] <= 0) next
        roots <- polyroot(coef[-1] * seq_len(4))
        roots <- Re(roots[abs(Im(roots)) < 1e-8 * max(1, Mod(roots))])
        roots <- roots[1 + 2 * cpq * roots + roots^2 > 0]
        if (length(roots) == 0) next
        value <- vapply(roots, function(a) sum(coef * a^(0:4)), numeric(1))
        a <- roots[which.min(value)]
        A <- 1 + 2 * cpq * a + a^2
        B[, p] <- sqrt(A) * lp
        B[, q] <- lq - a * lp
        new_c <- (C[p, ] + a * C[q, ]) / sqrt(A)
        C[p, ] <- new_c
        C[, p] <- new_c
        C[p, p] <- 1
      }
    }
    f_new <- criterion(B)
    if (f_old - f_new < f_start * eps) {
      converged <- TRUE
      break
    }
    f_old <- f_new
  }
  list(pattern = B * h, phi = C, iterations = iterations,
       converged = converged, maxit = maxit)
}


#' Warn when a rotation stopped at its iteration limit
#'
#' SPSS: "Rotation failed to converge in 25 iterations." The solution after
#' the last iteration is kept (as SPSS prints it).
#' @param rot Result of .efa_varimax() / .efa_oblimin() / .efa_promax()
#' @param label Rotation name for the message
#' @param group_label Group label (grouped analyses) or NULL
#' @noRd
.efa_warn_rotation <- function(rot, label, group_label = NULL) {
  if (isTRUE(rot$converged)) return(invisible(NULL))
  in_group <- if (is.null(group_label)) "" else paste0(" (group ", group_label, ")")
  cli_warn(c(
    "!" = "{label} rotation failed to converge in {rot$maxit} iterations{in_group}.",
    "i" = "The solution after the last iteration is shown; interpret it with caution."
  ))
  invisible(NULL)
}


#' Compute maximum number of factors for ML extraction
#' @description ML requires non-negative degrees of freedom:
#'   df = ((p - f)^2 - p - f) / 2 >= 0
#' @noRd
.ml_max_factors <- function(p) {
  for (f in seq_len(p)) {
    dof <- ((p - f)^2 - p - f) / 2
    if (dof < 0) return(as.integer(f - 1L))
  }
  return(as.integer(p))
}


# ============================================================================
# DEGENERATE CORRELATION MATRICES
# ============================================================================

#' Match a character option against its choices (English error)
#'
#' Like match.arg() (partial matching allowed), but the error is a cli
#' message in English instead of base R's translated "'arg' should be one
#' of ..." (German: "'arg' sollte eines von ... sein").
#' @noRd
.efa_match_choice <- function(x, choices, arg, note = NULL) {
  if (is.character(x) && length(x) == 1 && !is.na(x)) {
    hit <- pmatch(x, choices)
    if (!is.na(hit)) return(choices[hit])
  }
  shown <- if (is.character(x)) x else deparse(x)
  cli::cli_abort(c(
    "{.arg {arg}} must be one of {.or {.val {choices}}}.",
    "x" = "You supplied {.val {shown}}.",
    if (!is.null(note)) c("i" = note)
  ), call = rlang::caller_env())
}

#' Abort because the correlation matrix cannot be analysed
#'
#' Carries class "mariposa_efa_undefined" plus the reasons, so that a
#' grouped efa() can skip just this group with a warning.
#' @param reasons Character vector, one line per problem (plain text)
#' @param headline Short reason for the "not computed (...)" print line
#' @param hint cli-formatted hint
#' @noRd
.efa_abort_undefined <- function(reasons, headline, hint) {
  esc <- function(s) gsub("}", "}}", gsub("{", "{{", s, fixed = TRUE), fixed = TRUE)
  cli::cli_abort(
    c("{.fn efa} cannot analyse these items: {headline}.",
      stats::setNames(esc(reasons), rep("x", length(reasons))),
      "i" = hint),
    class = "mariposa_efa_undefined",
    reasons = reasons, headline = headline,
    call = NULL
  )
}

#' Warn that a group of a grouped efa() was skipped
#' @noRd
.efa_warn_skipped_group <- function(group_label, reasons, headline) {
  esc <- function(s) gsub("}", "}}", gsub("{", "{{", s, fixed = TRUE), fixed = TRUE)
  cli::cli_warn(c(
    "{.fn efa} skipped group {group_label}: {headline}.",
    stats::setNames(esc(reasons), rep("x", length(reasons))),
    "i" = "The other groups are analysed as usual."
  ))
}

#' Stop when a correlation of the matrix is undefined (NA)
#'
#' Names the cause: items without valid values, constant items, and item
#' pairs without (varying) cases in common. Previously eigen() aborted with
#' the base error "infinite or missing values in 'x'".
#' @noRd
.efa_check_defined <- function(cor_mat, data, var_names, weights_vec, use,
                               n_cases) {
  if (!anyNA(cor_mat)) return(invisible(NULL))

  mat <- .efa_item_matrix(data, var_names)
  rows <- if (is.null(weights_vec)) rep(TRUE, nrow(mat)) else !is.na(weights_vec)
  if (use == "complete") rows <- rows & stats::complete.cases(mat)

  reasons <- character(0)
  if (use == "complete" && n_cases < 2) {
    reasons <- sprintf(
      "Only %d complete case%s: every item must be observed together in at least 2 cases.",
      n_cases, if (n_cases == 1) "" else "s")
  } else {
    flagged <- character(0)
    for (v in var_names) {
      x <- mat[rows, v]
      x <- x[!is.na(x)]
      if (length(x) == 0) {
        reasons <- c(reasons, sprintf("`%s` has no valid values.", v))
        flagged <- c(flagged, v)
      } else if (length(x) == 1) {
        reasons <- c(reasons, sprintf("`%s` has only 1 valid value.", v))
        flagged <- c(flagged, v)
      } else if (length(unique(x)) == 1) {
        reasons <- c(reasons, sprintf(
          "`%s` is constant (every valid value is %s).", v, format(x[1])))
        flagged <- c(flagged, v)
      }
    }
    ok <- setdiff(var_names, flagged)
    if (length(ok) >= 2) {
      sub <- cor_mat[ok, ok, drop = FALSE]
      idx <- which(is.na(sub) & upper.tri(sub), arr.ind = TRUE)
      for (r in seq_len(nrow(idx))) {
        a <- ok[idx[r, 1]]
        b <- ok[idx[r, 2]]
        both <- sum(rows & !is.na(mat[, a]) & !is.na(mat[, b]))
        reasons <- c(reasons, if (both < 2) {
          sprintf("`%s` and `%s` have %d case%s with valid values on both.",
                  a, b, both, if (both == 1) "" else "s")
        } else {
          sprintf("`%s` and `%s`: one of them is constant in the cases they share.",
                  a, b)
        })
      }
    }
  }
  if (length(reasons) == 0) {
    reasons <- "At least one correlation could not be computed."
  }
  .efa_abort_undefined(
    reasons,
    headline = "the correlation matrix cannot be computed",
    hint = "Remove the item(s) listed above or analyse other cases."
  )
}

#' Check whether the correlation matrix is positive definite
#'
#' @return list(pd, reasons): reasons names perfectly correlated item pairs
#'   and too few cases (n <= k) when the matrix is singular.
#' @noRd
.efa_pd_check <- function(cor_mat, var_names, n_cases) {
  k <- length(var_names)
  ev <- eigen(cor_mat, symmetric = TRUE, only.values = TRUE)$values
  pd <- min(ev) > 1e-8
  reasons <- character(0)
  if (!pd) {
    idx <- which(abs(cor_mat) > 1 - 1e-8 & upper.tri(cor_mat), arr.ind = TRUE)
    for (r in seq_len(nrow(idx))) {
      reasons <- c(reasons, sprintf(
        "`%s` and `%s` are perfectly correlated (r = %s).",
        var_names[idx[r, 1]], var_names[idx[r, 2]],
        formatC(cor_mat[idx[r, 1], idx[r, 2]], format = "f", digits = 3)))
    }
    if (n_cases <= k) {
      reasons <- c(reasons, sprintf(
        "Only %d cases for %d variables: the number of cases must exceed the number of variables.",
        n_cases, k))
    }
    if (length(reasons) == 0) {
      reasons <- "At least one item is a linear combination of the others."
    }
  }
  list(pd = pd, reasons = reasons)
}

#' Warn about a singular matrix or too few cases
#' @noRd
.efa_warn_pd <- function(pd, k, n_cases, group_label) {
  in_group <- if (is.null(group_label)) "" else paste0(" (group ", group_label, ")")
  esc <- function(s) gsub("}", "}}", gsub("{", "{{", s, fixed = TRUE), fixed = TRUE)
  if (!pd$pd) {
    cli::cli_warn(c(
      "The correlation matrix is not positive definite{in_group}: KMO and Bartlett's test are not computed.",
      stats::setNames(esc(pd$reasons), rep("i", length(pd$reasons))),
      "i" = "The components are shown, but interpret them with caution."
    ))
  } else if (n_cases <= k) {
    cli::cli_warn(c(
      "Only {n_cases} cases for {k} variables{in_group}.",
      "i" = "Factor solutions need clearly more cases than variables; interpret with caution."
    ))
  }
  invisible(NULL)
}


# ============================================================================
# CORRELATION MATRIX COMPUTATION
# ============================================================================

#' Compute pairwise correlation matrix (SPSS default for FACTOR)
#' @noRd
.efa_pairwise_cor <- function(data, var_names, weights_vec) {
  k <- length(var_names)

  if (!is.null(weights_vec)) {
    # Weighted pairwise correlations
    item_mat <- .efa_item_matrix(data, var_names)
    cor_mat <- matrix(1, k, k)
    n_mat <- matrix(0, k, k)
    rownames(cor_mat) <- colnames(cor_mat) <- var_names
    rownames(n_mat) <- colnames(n_mat) <- var_names

    for (i in seq_len(k)) {
      for (j in seq_len(k)) {
        if (i == j) {
          # Count valid cases for diagonal
          valid <- !is.na(item_mat[, i]) & !is.na(weights_vec)
          n_mat[i, j] <- sum(weights_vec[valid])
          next
        }
        if (j > i) next  # Will fill from lower triangle

        x <- item_mat[, i]
        y <- item_mat[, j]
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
    mat <- .efa_item_matrix(data, var_names)
    cor_mat <- .efa_cor_quiet(mat, use = "pairwise.complete.obs")
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

  # Unweighted number of cases behind the smallest pairwise correlation
  # (bounds the rank of the matrix; weights do not add information)
  item_mat <- .efa_item_matrix(data, var_names)
  ok_w <- if (is.null(weights_vec)) rep(TRUE, nrow(item_mat)) else !is.na(weights_vec)
  obs <- !is.na(item_mat) & ok_w
  n_cases <- min(crossprod(obs)[lower.tri(diag(k))])

  list(cor_mat = cor_mat, n_obs = n_mat, n_bartlett = n_bartlett,
       n_cases = n_cases)
}

#' Item columns as a plain numeric matrix
#'
#' Label classes are dropped (.plain_numeric) so that haven_labelled items
#' never route the correlation code through vctrs dispatch.
#' @noRd
.efa_item_matrix <- function(data, var_names) {
  mat <- vapply(var_names, function(v) as.double(.plain_numeric(data[[v]])),
                numeric(nrow(data)))
  matrix(mat, nrow = nrow(data), dimnames = list(NULL, var_names))
}

#' stats::cor() without its "standard deviation is zero" warning
#'
#' A constant item makes its correlations NA; .efa_check_defined() names
#' the item in an English mariposa error, so the (translated) base warning
#' is muffled - and only that one.
#' @noRd
.efa_cor_quiet <- function(mat, use = "everything") {
  # Raised from C (stats/src/cov.c), so it is translated via the "stats"
  # catalog, not "R-stats"
  zero_sd <- gettext("the standard deviation is zero", domain = "stats")
  withCallingHandlers(
    stats::cor(mat, use = use),
    warning = function(w) {
      if (conditionMessage(w) %in% c(zero_sd, "the standard deviation is zero")) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

#' Compute listwise correlation matrix
#' @noRd
.efa_listwise_cor <- function(data, var_names, weights_vec, na.rm) {
  mat <- .efa_item_matrix(data, var_names)
  complete <- stats::complete.cases(mat)

  if (!is.null(weights_vec)) {
    complete <- complete & !is.na(weights_vec)
    weights_vec <- weights_vec[complete]
  }

  mat <- mat[complete, , drop = FALSE]
  n <- nrow(mat)
  k <- length(var_names)

  if (n < 2) {
    # No correlation is defined; .efa_check_defined() reports why
    cor_mat <- matrix(NA_real_, k, k)
    n_eff <- if (!is.null(weights_vec)) sum(weights_vec) else n
  } else if (!is.null(weights_vec)) {
    cor_mat <- .weighted_cor(mat, weights_vec)
    n_eff <- sum(weights_vec)
  } else {
    cor_mat <- .efa_cor_quiet(mat)
    n_eff <- n
  }

  rownames(cor_mat) <- colnames(cor_mat) <- var_names

  list(cor_mat = cor_mat, n_obs = n, n_bartlett = n_eff, n_cases = n)
}


# ============================================================================
# KMO AND BARTLETT'S TEST
# ============================================================================

#' Compute Kaiser-Meyer-Olkin Measure of Sampling Adequacy
#' @description
#' KMO measures the proportion of variance among variables that might be
#' common variance. SPSS-compatible implementation.
#' @noRd
.compute_kmo <- function(cor_mat, pd = TRUE) {
  k <- ncol(cor_mat)
  if (!pd) {
    # The anti-image needs R^-1; a pseudo-inverse produced meaningless
    # values (0.500, NaN). SPSS prints no KMO for such a matrix either.
    return(list(overall = NA_real_,
                per_item = stats::setNames(rep(NA_real_, k), colnames(cor_mat))))
  }
  # Anti-image approach (SPSS method)
  # 1. Compute inverse of correlation matrix
  inv_cor <- solve(cor_mat)

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
#' @noRd
.compute_bartlett <- function(cor_mat, n, k, pd = TRUE) {
  # Bartlett's test: chi_sq = -((n - 1) - (2*k + 5)/6) * log(det(R))
  log_det <- determinant(cor_mat, logarithm = TRUE)

  if (!pd || log_det$sign <= 0 || !is.finite(log_det$modulus)) {
    # Singular matrix: log(det) is -Inf (chi-square "Inf", p "0.000")
    return(list(chi_sq = NA_real_, df = as.integer(k * (k - 1) / 2),
                p_value = NA_real_))
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
#' @noRd
.efa_item_stats <- function(data, var_names, weights_vec, use = "pairwise") {
  mat <- .efa_item_matrix(data, var_names)
  # SPSS "Descriptive Statistics": with pairwise deletion every item uses
  # its own valid cases; with listwise deletion all items use the cases
  # the analysis is based on (the complete cases).
  in_analysis <- if (use == "complete") {
    stats::complete.cases(mat)
  } else {
    rep(TRUE, nrow(mat))
  }
  if (!is.null(weights_vec)) in_analysis <- in_analysis & !is.na(weights_vec)

  stats_list <- lapply(var_names, function(v) {
    x <- mat[, v]
    valid <- in_analysis & !is.na(x)
    xv <- x[valid]
    if (!is.null(weights_vec)) {
      w <- weights_vec[valid]
      tibble::tibble(
        variable = v,
        mean = if (length(xv)) .w_mean(xv, w) else NA_real_,
        sd = if (length(xv) > 1) sqrt(.w_var(xv, w)) else NA_real_,
        analysis_n = sum(w),
        missing_n = sum(!valid)
      )
    } else {
      tibble::tibble(
        variable = v,
        mean = if (length(xv)) mean(xv) else NA_real_,
        sd = if (length(xv) > 1) stats::sd(xv) else NA_real_,
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

#' Extraction sums of squared loadings of a (possibly older) efa result
#'
#' Results created before the extraction_variance field existed are
#' completed from their unrotated loadings.
#' @param res efa result (or one group's result)
#' @return tibble(component, ss_loading, prc_variance, cumulative_prc)
#' @noRd
.efa_extraction_variance <- function(res) {
  if (!is.null(res$extraction_variance)) return(res$extraction_variance)
  ss <- unname(colSums(res$unrotated_loadings^2))
  k <- nrow(res$unrotated_loadings)
  tibble::tibble(component = seq_along(ss), ss_loading = ss,
                 prc_variance = ss / k * 100,
                 cumulative_prc = cumsum(ss / k * 100))
}

#' Interpret KMO value
#' @noRd
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
  if (isTRUE(x$is_grouped)) {
    for (group_result in x$groups) {
      group_values <- group_result$group_values
      group_label <- .format_group_label(group_values)
      cat(sprintf("[%s]\n", group_label))
      .print_efa_compact(group_result, length(x$variables), extraction_label,
                         weighted_tag, digits)
    }
  } else {
    .print_efa_compact(x, length(x$variables), extraction_label,
                       weighted_tag, digits)
  }

  invisible(x)
}

#' SPSS's note when a requested rotation is impossible
#'
#' A single component/factor cannot be rotated; efa() leaves it unrotated.
#' SPSS says so instead of silently printing an unrotated solution.
#' @return Character scalar or NULL
#' @noRd
.efa_rotation_note <- function(res) {
  requested <- res$rotation_requested %||% res$rotation %||% "none"
  if (!identical(requested, "none") && identical(as.integer(res$n_factors), 1L)) {
    unit <- if (identical(res$extraction, "ml")) "factor" else "component"
    sprintf("Only one %s was extracted. The solution cannot be rotated.", unit)
  } else {
    NULL
  }
}

#' Print compact one-liner for a single EFA result
#' @noRd
.print_efa_compact <- function(res, n_vars, extraction_label,
                               weighted_tag, digits) {
  if (!is.null(res$not_computed)) {
    cat(sprintf("Exploratory Factor Analysis: %d items%s\n", n_vars, weighted_tag))
    cat(sprintf("  not computed (%s)\n", res$not_computed))
    return(invisible(NULL))
  }

  n_factors <- res$n_factors
  unit <- if (identical(res$extraction, "ml")) "factor" else "component"
  component_label <- if (n_factors == 1) unit else paste0(unit, "s")

  kmo <- res$kmo$overall
  kmo_text <- if (is.na(kmo)) {
    "KMO = not computed"
  } else {
    sprintf("KMO = %s (%s)", fmt_num(kmo, digits), .kmo_interpretation(kmo))
  }

  # Variance explained by the extracted solution (SPSS "Extraction Sums of
  # Squared Loadings", cumulative %); for ML this is not the eigenvalue share
  total_var_pct <- .efa_extraction_variance(res)$cumulative_prc[n_factors]

  rotation_label <- switch(res$rotation %||% "none",
    "varimax" = "Varimax",
    "oblimin" = "Oblimin",
    "promax" = "Promax",
    "none" = "Unrotated"
  )

  n_info <- .efa_n_info(res)
  cat(sprintf("Exploratory Factor Analysis: %d items, %d %s (%s/%s)%s\n",
              n_vars, n_factors, component_label,
              extraction_label, rotation_label, weighted_tag))
  cat(sprintf("  %s, Variance explained: %s%%, N = %s (%s)\n",
              kmo_text, fmt_num(total_var_pct, 1), n_info$value, n_info$basis))
  note <- .efa_rotation_note(res)
  if (!is.null(note)) cat("  ", note, "\n", sep = "")
}

#' Sample size of an EFA and what it refers to
#'
#' With pairwise deletion every correlation has its own N; the smallest
#' one is the N of Bartlett's test (SPSS). With listwise deletion it is the
#' number of complete cases. Weighted N is the sum of weights.
#' @return list(value = formatted N, basis = "smallest pairwise"/"listwise")
#' @noRd
.efa_n_info <- function(res) {
  list(
    value = formatC(res$n, format = "f", digits = 0),
    basis = if (identical(res$use, "complete")) "listwise" else "smallest pairwise"
  )
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
#' @param descriptives Show the per-item descriptive statistics (mean, SD,
#'   analysis N, missing N; SPSS \code{/PRINT UNIVARIATE})? (Default: TRUE)
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
                        descriptives = TRUE, digits = 3, ...) {
  show <- list(
    descriptives = descriptives,
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
#' @noRd
.print_efa_ungrouped <- function(x, digits = 3) {
  show_kmo <- if (!is.null(x$show)) isTRUE(x$show$kmo_bartlett) else TRUE
  show_comm <- if (!is.null(x$show)) isTRUE(x$show$communalities) else TRUE
  show_var <- if (!is.null(x$show)) isTRUE(x$show$variance_explained) else TRUE
  show_unrotated <- if (!is.null(x$show)) isTRUE(x$show$unrotated_matrix) else TRUE
  show_rotated <- if (!is.null(x$show)) isTRUE(x$show$rotated_matrix) else TRUE
  show_pattern <- if (!is.null(x$show)) isTRUE(x$show$pattern_matrix) else TRUE
  show_structure <- if (!is.null(x$show)) isTRUE(x$show$structure_matrix) else TRUE
  show_factor_cor <- if (!is.null(x$show)) isTRUE(x$show$factor_correlations) else TRUE
  show_desc <- if (!is.null(x$show)) !isFALSE(x$show$descriptives) else TRUE

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

  rotation_note <- .efa_rotation_note(x)

  # Info section (items with their variable labels, as SPSS shows them)
  labels <- x$variable_labels
  .print_scale_item_list("Variables", x$variables, labels)
  info <- list(
    "Extraction" = extraction_full,
    "Rotation" = switch(x$rotation,
      "varimax" = "Varimax with Kaiser Normalization",
      "oblimin" = "Oblimin with Kaiser Normalization",
      "promax" = "Promax with Kaiser Normalization",
      "none" = if (is.null(rotation_note)) "None" else "None (a single solution cannot be rotated)"
    ),
    "N of Factors" = as.character(x$n_factors),
    "Weights" = x$weights
  )
  names(info)[names(info) == "N of Factors"] <- paste0("N of ", matrix_label, "s")
  print_info_section(info)
  n_info <- .efa_n_info(x)
  cat(sprintf("- N (%s): %s%s\n", n_info$basis, n_info$value,
              if (!is.null(x$weights)) " (weighted)" else ""))

  # Descriptive Statistics (SPSS /PRINT UNIVARIATE)
  if (show_desc && !is.null(x$item_statistics)) {
    cat("\nDescriptive Statistics\n")
    desc <- as.data.frame(x$item_statistics)
    desc$variable <- .scale_row_labels(desc$variable, labels,
                                       other_width = 47 + digits)
    print_stat_table(
      desc, digits = digits,
      col_types = c(analysis_n = "int", missing_n = "int"),
      col_labels = c(variable = "Variable", mean = "Mean",
                     sd = "Std. Deviation", analysis_n = "Analysis N",
                     missing_n = "Missing N")
    )
  }

  # KMO and Bartlett's Test
  if (show_kmo) {
    cat("\n")
    cat("KMO and Bartlett's Test\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
    if (is.na(x$kmo$overall) && is.na(x$bartlett$chi_sq)) {
      cat("  not computed (the correlation matrix is not positive definite)\n")
    } else {
      cat(sprintf("  Kaiser-Meyer-Olkin Measure:     %s\n",
                  fmt_num(x$kmo$overall, digits)))
      cat(sprintf("  Bartlett's Chi-Square:          %s\n",
                  fmt_num(x$bartlett$chi_sq, digits)))
      cat(sprintf("  df:                             %d\n", as.integer(x$bartlett$df)))
      cat(sprintf("  Sig.:                           %s\n",
                  fmt_p(x$bartlett$p_value, digits)))
    }

    # Goodness-of-fit Test (ML only)
    if (!is.null(x$goodness_of_fit)) {
      cat("\nGoodness-of-fit Test\n")
      cat(paste(rep("-", 40), collapse = ""), "\n")
      cat(sprintf("  Chi-Square:                     %s\n",
                  fmt_num(x$goodness_of_fit$chi_sq, digits)))
      cat(sprintf("  df:                             %d\n", as.integer(x$goodness_of_fit$df)))
      cat(sprintf("  Sig.:                           %s\n",
                  fmt_p(x$goodness_of_fit$p_value, digits)))
    }
  }

  # Communalities
  if (show_comm) {
    cat("\nCommunalities\n")

    # Initial communalities: 1.0 for PCA, SMC for ML/PAF
    initial_vals <- if (!is.null(x$initial_communalities)) {
      as.numeric(x$initial_communalities[names(x$communalities)])
    } else {
      rep(1, length(x$communalities))
    }

    comm_df <- data.frame(
      variable = .scale_row_labels(names(x$communalities), labels,
                                   other_width = 24 + 2 * digits),
      initial = initial_vals,
      extraction = as.numeric(x$communalities),
      stringsAsFactors = FALSE
    )
    # Fixed decimals in both columns (print.data.frame showed "1" next to
    # "0.457")
    print_stat_table(comm_df, digits = digits,
                     col_types = c(initial = "num", extraction = "num"),
                     col_labels = c(variable = "Variable", initial = "Initial",
                                    extraction = "Extraction"))
    cat(sprintf("Extraction Method: %s.\n", extraction_full))
  }

  # Total Variance Explained
  if (show_var) {
    cat("\nTotal Variance Explained\n")
    .print_efa_variance_table(x, matrix_label, extraction_full, digits)
  }

  # Unrotated matrix
  if (show_unrotated) {
    cat(sprintf("\n%s Matrix (unrotated)\n", matrix_label))
    cat(paste(rep("-", 40), collapse = ""), "\n")
    .print_loading_matrix(x$unrotated_loadings, blank, sort_loadings, digits, labels)
    cat(sprintf("Extraction Method: %s.\n", extraction_full))
  }
  if (!is.null(rotation_note) && show_rotated) {
    cat(sprintf("\nRotated %s Matrix\n", matrix_label))
    cat(paste(rep("-", 40), collapse = ""), "\n")
    cat(rotation_note, "\n", sep = "")
  }

  # Rotated loadings
  if (x$rotation == "varimax") {
    if (show_rotated) {
      cat(sprintf("\nRotated %s Matrix\n", matrix_label))
      cat(paste(rep("-", 40), collapse = ""), "\n")
      .print_loading_matrix(x$loadings, blank, sort_loadings, digits, labels)
      cat(sprintf("Extraction Method: %s.\n", extraction_full))
      cat("Rotation Method: Varimax with Kaiser Normalization.\n")
      .print_rotation_convergence(x)
    }

  } else if (x$rotation %in% c("oblimin", "promax")) {
    rot_label <- if (x$rotation == "oblimin") "Oblimin" else "Promax"

    if (show_pattern) {
      cat("\nPattern Matrix\n")
      cat(paste(rep("-", 40), collapse = ""), "\n")
      .print_loading_matrix(x$pattern_matrix, blank, sort_loadings, digits, labels)
      cat(sprintf("Extraction Method: %s.\n", extraction_full))
      cat(sprintf("Rotation Method: %s with Kaiser Normalization.\n", rot_label))
      .print_rotation_convergence(x)
    }

    if (show_structure) {
      cat("\nStructure Matrix\n")
      cat(paste(rep("-", 40), collapse = ""), "\n")
      .print_loading_matrix(x$structure_matrix, blank, sort_loadings, digits, labels)
    }

    if (show_factor_cor) {
      cat(sprintf("\n%s Correlation Matrix\n", matrix_label))
      cat(paste(rep("-", 40), collapse = ""), "\n")
      fc <- round(x$factor_correlations, digits)
      print(fc, quote = FALSE)
    }
  }
}

#' SPSS's rotation footnote ("Rotation converged in 4 iterations.")
#' @param x efa result (one group)
#' @noRd
.print_rotation_convergence <- function(x) {
  it <- x$rotation_iterations
  if (is.null(it)) return(invisible(NULL))
  if (isTRUE(x$rotation_converged)) {
    cat(sprintf("Rotation converged in %s iterations.\n", it))
  } else {
    cat(sprintf("Rotation failed to converge in %s iterations.\n", it))
  }
  invisible(NULL)
}

#' Print EFA results for grouped data
#' @noRd
.print_efa_grouped <- function(x, digits = 3) {

  for (group_result in x$groups) {
    # Group header
    group_values <- group_result$group_values
    print_group_header(group_values)

    if (!is.null(group_result$not_computed)) {
      cat(sprintf("  not computed (%s)\n", group_result$not_computed))
      for (r in group_result$reasons) cat("  - ", r, "\n", sep = "")
      next
    }

    # Create a temporary ungrouped-like structure for printing
    temp <- c(group_result, list(
      variables = x$variables,
      variable_labels = x$variable_labels,
      weights = x$weights,
      sort = x$sort,
      blank = x$blank
    ))

    .print_efa_ungrouped(temp, digits)
  }
}

#' Print SPSS's "Total Variance Explained" table
#'
#' One row per variable: Initial Eigenvalues for every component, the
#' Extraction Sums of Squared Loadings and (when rotated) the Rotation Sums
#' for the extracted ones - the SPSS FACTOR layout. Oblique rotations show
#' only the rotation totals, with SPSS's footnote.
#' @param x efa result (one group)
#' @param unit_label "Component" (PCA) or "Factor" (ML)
#' @param extraction_full Extraction method for the footer
#' @param digits Decimal places
#' @noRd
.print_efa_variance_table <- function(x, unit_label, extraction_full, digits) {
  ve <- x$variance_explained
  k <- nrow(ve)
  pad_k <- function(v) c(v, rep(NA_real_, k - length(v)))
  ev <- .efa_extraction_variance(x)

  groups <- list(
    list(label = "Initial Eigenvalues",
         cols = list(ve$eigenvalue, ve$prc_variance, ve$cumulative_prc)),
    list(label = "Extraction Sums",
         cols = list(pad_k(ev$ss_loading), pad_k(ev$prc_variance),
                     pad_k(ev$cumulative_prc)))
  )
  rv <- x$rotation_variance
  oblique <- FALSE
  if (!is.null(rv) && !identical(x$rotation, "none")) {
    oblique <- !"prc_variance" %in% names(rv)
    groups[[3]] <- list(
      label = "Rotation Sums",
      cols = if (oblique) list(pad_k(rv$ss_loading)) else
        list(pad_k(rv$ss_loading), pad_k(rv$prc_variance),
             pad_k(rv$cumulative_prc))
    )
  }
  sub_labels <- c("Total", "% Var.", "Cum. %")

  # Format cells and size every column to its content
  row_labels <- as.character(seq_len(k))
  w0 <- max(nchar(unit_label), nchar(row_labels))
  groups <- lapply(groups, function(g) {
    g$cells <- lapply(g$cols, fmt_num, digits = digits)
    g$heads <- sub_labels[seq_along(g$cols)]
    g$widths <- mapply(function(h, cl) max(nchar(h), nchar(cl)),
                       g$heads, g$cells)
    inner <- sum(g$widths) + length(g$widths) - 1L
    if (nchar(g$label) > inner) {
      last <- length(g$widths)
      g$widths[last] <- g$widths[last] + nchar(g$label) - inner
      inner <- nchar(g$label)
    }
    g$inner <- inner
    g
  })
  total_width <- w0 + sum(vapply(groups, function(g) g$inner + 2L, integer(1)))
  border <- paste0("  ", strrep("-", total_width))

  line1 <- paste0("  ", strrep(" ", w0), paste0(vapply(groups, function(g) {
    paste0("  ", pad_utf8(g$label, g$inner))
  }, character(1)), collapse = ""))
  line2 <- paste0("  ", pad_utf8(unit_label, w0), paste0(vapply(groups, function(g) {
    paste0("  ", paste(mapply(pad_utf8, g$heads, g$widths, "right"),
                       collapse = " "))
  }, character(1)), collapse = ""))

  cat(border, "\n", sep = "")
  cat(sub(" +$", "", line1), "\n", sep = "")
  cat(line2, "\n", sep = "")
  cat(border, "\n", sep = "")
  for (i in seq_len(k)) {
    cells <- vapply(groups, function(g) {
      paste0("  ", paste(mapply(function(cl, w) pad_utf8(cl[i], w, "right"),
                                g$cells, g$widths), collapse = " "))
    }, character(1))
    cat(sub(" +$", "", paste0("  ", pad_utf8(row_labels[i], w0, "right"),
                              paste0(cells, collapse = ""))), "\n", sep = "")
  }
  cat(border, "\n", sep = "")
  cat("Sums = sums of squared loadings.\n")
  cat(sprintf("Extraction Method: %s.\n", extraction_full))
  if (oblique) {
    cat(sprintf("When %ss are correlated, sums of squared loadings cannot be added\n",
                tolower(unit_label)))
    cat("to obtain a total variance.\n")
  }
  invisible(NULL)
}

#' Print a loading matrix with blank suppression and optional sorting
#' @noRd
.print_loading_matrix <- function(mat, blank = 0.40, sort_loadings = TRUE, digits = 3,
                                  labels = NULL) {
  k <- nrow(mat)
  n_f <- ncol(mat)
  cell_width <- max(7L, digits + 4L)

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
  # Variable name plus its (console-fitted) label, as SPSS labels the rows
  rownames(display_mat) <- .scale_row_labels(rownames(mat), labels,
                                             other_width = n_f * (cell_width + 1L))
  colnames(display_mat) <- colnames(mat)

  for (i in seq_len(k)) {
    for (j in seq_len(n_f)) {
      val <- mat[i, j]
      display_mat[i, j] <- pad_utf8(if (abs(val) >= blank) fmt_num(val, digits) else "",
                                    cell_width, "right")
    }
  }

  # Print in sorted order
  display_mat <- display_mat[ord, , drop = FALSE]
  print(display_mat, quote = FALSE, right = TRUE)
}
