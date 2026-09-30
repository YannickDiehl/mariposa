
#' Check How Reliably Your Scale Measures a Concept
#'
#' @description
#' \code{reliability()} calculates Cronbach's Alpha, McDonald's Omega, and
#' detailed item statistics to evaluate whether your survey items form a
#' reliable scale. This is the R
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
#'   \item{omega}{McDonald's Omega (raw/total, one-factor ML model; NA for
#'     fewer than 3 items)}
#'   \item{omega_std}{Standardized Omega (correlation metric)}
#'   \item{n_items}{Number of items in the scale}
#'   \item{item_statistics}{Mean, SD, and N for each item}
#'   \item{item_total}{Corrected Item-Total Correlation, Alpha if Item
#'     Deleted, and Omega if Item Deleted}
#'   \item{inter_item_cor}{Inter-item correlation matrix}
#'   \item{n}{Sample size (listwise)}
#' }
#'   Use \code{summary()} for the full SPSS-style output with toggleable sections.
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
#' ## McDonald's Omega
#'
#' **McDonald's Omega** (omega total) is a factor-model-based reliability
#' coefficient. Where alpha assumes every item measures the construct
#' equally well (tau-equivalence), omega fits a one-factor model and lets
#' each item carry its own loading. The two agree when items are roughly
#' tau-equivalent; omega is typically slightly higher (and the more accurate
#' estimate) when loadings differ across items, which is the common case in
#' survey scales (Hayes & Coutts, 2020).
#'
#' \code{reliability()} reports two variants, mirroring the two alpha
#' variants: \code{omega} from the covariance metric (analogous to raw
#' alpha) and \code{omega_std} from the correlation metric (analogous to
#' standardized alpha). **Omega if Item Deleted** refits the one-factor
#' model without each item, mirroring Alpha if Item Deleted.
#'
#' A one-factor model needs at least 3 items to be identified: with fewer
#' than 3 items the omega fields are \code{NA} (alpha is still computed),
#' and Omega if Item Deleted is \code{NA} whenever the reduced scale would
#' fall below 3 items. Omega is also \code{NA}, with a warning, when the
#' items' correlation matrix is singular or the one-factor solution is a
#' Heywood case (an item's uniqueness at the lower bound, i.e. a loading
#' of about 1: the "factor" is then that single item, not the scale).
#'
#' Items with zero variance are removed from the scale with a warning, as
#' SPSS RELIABILITY does (\code{$removed_items}).
#'
#' ## Weighted variants and validation status
#'
#' Cronbach's alpha and the item statistics are validated against SPSS v29
#' \code{RELIABILITY} output (weighted and unweighted). McDonald's omega is
#' currently an R-only statistic (Tier 4 per the Validation Charter): SPSS
#' offers omega from v27 onward, but IBM's algorithm documentation for it is
#' not publicly retrievable and no SPSS v29 reference run exists yet.
#' mariposa computes omega from a one-factor maximum-likelihood solution
#' (the same estimator family as \code{\link{efa}} with \code{fm = "ml"}).
#' The weighted omega uses the same weighted correlation and covariance
#' matrices as the weighted alpha and reduces exactly to the unweighted
#' omega when all weights equal 1 (enforced by an internal invariance
#' suite); see \code{vignette("spss-compatibility")} for validation status.
#'
#' ## When to Use This
#'
#' Run \code{reliability()} before creating scale scores with
#' \code{\link{row_means}}:
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
#' # --- Three-layer output ---
#' result <- reliability(survey_data, trust_government, trust_media, trust_science)
#' result              # compact one-line overview
#' summary(result)     # full detailed output with all sections
#' summary(result, inter_item_correlations = FALSE)  # hide correlations
#'
#' @seealso
#' \code{\link{row_means}} for creating mean indices after checking reliability.
#'
#' \code{\link{pearson_cor}} for bivariate correlations.
#'
#' \code{\link{summary.reliability}} for detailed output with toggleable sections.
#'
#' @references
#' McDonald, R. P. (1999). \emph{Test Theory: A Unified Treatment}.
#' Mahwah, NJ: Lawrence Erlbaum.
#'
#' Hayes, A. F., & Coutts, J. J. (2020). Use omega rather than Cronbach's
#' alpha for estimating reliability. But... \emph{Communication Methods and
#' Measures}, 14(1), 1-24.
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

  # Once per call (it was repeated for every group)
  if (length(var_names) < 3) {
    cli_warn(c(
      "McDonald's omega requires at least 3 items; a one-factor model is not identified for k = {length(var_names)}.",
      "i" = "Omega fields are set to NA. Cronbach's alpha is unaffected."
    ))
  }

  # Process weights
  weights_info <- .process_weights(data, rlang::enquo(weights))
  data <- weights_info$data

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
        group_data, var_names, group_weights, na.rm,
        group_label = .format_group_label(group_keys_df[i, , drop = FALSE])
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
#' @noRd
.reliability_core <- function(data, var_names, weights_vec, na.rm,
                              group_label = NULL) {

  # Item matrix as plain numbers (label classes dropped)
  k <- length(var_names)
  mat <- vapply(var_names, function(v) as.double(.plain_numeric(data[[v]])),
                numeric(nrow(data)))
  mat <- matrix(mat, nrow = nrow(data), dimnames = list(NULL, var_names))
  if (!is.null(weights_vec)) weights_vec <- .plain_numeric(weights_vec)
  in_group <- if (is.null(group_label)) "" else paste0(" (group ", group_label, ")")

  if (!na.rm) {
    # na.rm = FALSE keeps incomplete cases, so every statistic is
    # undefined as soon as one value is missing (the NA previously reached
    # an if() and crashed with "missing value where TRUE/FALSE needed").
    incomplete <- !stats::complete.cases(mat)
    if (!is.null(weights_vec)) incomplete <- incomplete | is.na(weights_vec)
    if (any(incomplete)) {
      miss_vars <- var_names[colSums(is.na(mat)) > 0]
      if (!is.null(weights_vec) && anyNA(weights_vec)) miss_vars <- c(miss_vars, "the weights")
      cli_warn(c(
        "{.fn reliability} with {.code na.rm = FALSE}{in_group}: {sum(incomplete)} case{?s} with missing values; all statistics are NA.",
        "i" = "Missing values in: {.var {miss_vars}}.",
        "i" = "Use {.code na.rm = TRUE} for listwise deletion (as SPSS RELIABILITY does)."
      ))
      return(.reliability_na_result(
        k, nrow(mat), "missing values with na.rm = FALSE",
        weighted_n = if (!is.null(weights_vec)) sum(weights_vec, na.rm = TRUE) else NULL
      ))
    }
  }

  # Listwise deletion (only complete cases across all items)
  mat_all <- mat
  complete <- stats::complete.cases(mat)
  if (!is.null(weights_vec)) {
    complete <- complete & !is.na(weights_vec)
    weights_vec <- weights_vec[complete]
  }
  mat <- mat[complete, , drop = FALSE]
  n <- nrow(mat)

  if (n < 2) {
    empty <- var_names[colSums(!is.na(mat_all)) == 0]
    cli_warn(c(
      "{.fn reliability}{in_group}: only {n} complete case{?s}; the scale cannot be analysed.",
      if (length(empty)) c("x" = "No valid values in {.var {empty}}.") else
        c("i" = "Listwise deletion keeps only cases with valid values on every item.")
    ))
    return(.reliability_na_result(
      k, n, sprintf("%d complete case%s", n, if (n == 1) "" else "s"),
      weighted_n = if (!is.null(weights_vec)) sum(weights_vec) else NULL
    ))
  }

  # Zero-variance items: SPSS RELIABILITY removes them from the scale with
  # a warning. Keeping them biased alpha (k counted an item without
  # variance), made the correlations NA and leaked base-R warnings.
  item_var_raw <- vapply(var_names, function(v) {
    if (is.null(weights_vec)) stats::var(mat[, v]) else .w_var(mat[, v], weights_vec)
  }, numeric(1))
  zero_var <- !is.na(item_var_raw) & item_var_raw <= 0
  removed_items <- var_names[zero_var]
  if (length(removed_items) > 0) {
    k_before <- k
    var_names <- var_names[!zero_var]
    mat <- mat[, !zero_var, drop = FALSE]
    k <- length(var_names)
    cli_warn(c(
      "{.fn reliability}{in_group}: {cli::qty(length(removed_items))}item{?s} with zero variance {?is/are} removed from the scale: {.var {removed_items}}.",
      "i" = "SPSS RELIABILITY removes zero-variance items the same way.",
      if (k >= 2 && k < 3 && k_before >= 3) c("i" = "McDonald's omega needs at least 3 items and is not computed.")
    ))
    if (k < 2) {
      res <- .reliability_na_result(
        k, n, "fewer than 2 items with non-zero variance",
        weighted_n = if (!is.null(weights_vec)) sum(weights_vec) else NULL)
      res$removed_items <- removed_items
      return(res)
    }
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
    cor_mat <- .efa_cor_quiet(mat)
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
  # MCDONALD'S OMEGA (one-factor ML model)
  # ============================================================================
  # PROVENANCE: SPSS v27+ offers McDonald's omega in RELIABILITY, but IBM's
  # algorithm documentation for it is not publicly retrievable. mariposa
  # therefore computes omega from a ONE-FACTOR maximum-likelihood solution
  # (stats::factanal on the (weighted) correlation matrix -- the same
  # estimator family as efa()'s fm = "ml"), with the raw-metric omega
  # obtained by rescaling loadings/uniquenesses via the (weighted)
  # covariance diagonal. n.obs follows the package's frequency-weight
  # convention (unrounded sum(w), Charter §5.1). Pending the SPSS v29
  # reference run (.claude/spss-syntax-omega-references.sps, 0.6.13) this
  # statistic is Tier 4 / Internal per the Validation Charter (§4).
  # References: McDonald (1999); Hayes & Coutts (2020).

  omega_note <- NULL
  if (k < 3) {
    # (warned once per call in reliability(), not once per group)
    omega <- NA_real_
    omega_std <- NA_real_
    omega_if_deleted <- rep(NA_real_, k)
    omega_note <- "requires at least 3 items"
  } else {
    pd <- .efa_pd_check(cor_mat, var_names, n)
    om <- if (pd$pd) {
      .omega_one_factor(cor_mat, cov_mat, w_n)
    } else {
      list(omega = NA_real_, omega_std = NA_real_, error = "singular")
    }
    if (!is.null(om$error)) {
      esc <- function(s) gsub("}", "}}", gsub("{", "{{", s, fixed = TRUE), fixed = TRUE)
      if (identical(om$error, "singular")) {
        omega_note <- "the items' correlation matrix is singular"
        cli_warn(c(
          "McDonald's omega is not computed{in_group}: the items' correlation matrix is singular.",
          stats::setNames(esc(pd$reasons), rep("i", length(pd$reasons))),
          "i" = "Cronbach's alpha is unaffected."
        ))
      } else if (identical(om$error, "heywood")) {
        heywood_items <- om$items
        omega_note <- "Heywood case in the one-factor model"
        cli_warn(c(
          "McDonald's omega is not computed{in_group}: the one-factor solution is a Heywood case.",
          "i" = "{.var {heywood_items}} {cli::qty(length(heywood_items))}ha{?s/ve} a uniqueness at the lower bound (a loading of about 1), so the factor reflects {?this item/these items} rather than the scale.",
          "i" = "This usually means the items share little common variance; Cronbach's alpha is unaffected."
        ))
      } else {
        omega_note <- "the one-factor model could not be fitted"
        cli_warn(c(
          "McDonald's omega is not computed{in_group}: the one-factor maximum-likelihood model could not be fitted.",
          "i" = "Cronbach's alpha is unaffected."
        ))
      }
    }
    omega <- om$omega
    omega_std <- om$omega_std

    # Omega if Item Deleted: refit the one-factor model on the reduced
    # correlation matrix (mirrors Alpha if Item Deleted). For k - 1 < 3 the
    # reduced model is unidentified -> NA. Refit failures degrade to NA
    # silently (the headline warning above already covers pathologies).
    omega_if_deleted <- vapply(seq_len(k), function(i) {
      if (k - 1 < 3) return(NA_real_)
      idx <- setdiff(seq_len(k), i)
      .omega_one_factor(cor_mat[idx, idx, drop = FALSE],
                        cov_mat[idx, idx, drop = FALSE], w_n)$omega
    }, numeric(1))
  }

  # ============================================================================
  # ITEM STATISTICS (Mean, SD, N per item)
  # ============================================================================

  if (!is.null(weights_vec)) {
    item_means <- vapply(var_names, function(v) {
      .w_mean(mat[, v], weights_vec)
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
      corrected_item_total_r[i] <- .efa_cor_quiet(cbind(mat[, i], total_remaining))[1, 2]
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
    alpha_if_deleted = alpha_if_deleted,
    omega_if_deleted = omega_if_deleted
  )

  # ============================================================================
  # NEGATIVE ALPHA (SPSS footnote)
  # ============================================================================
  # alpha < 0 exactly when the average inter-item covariance is negative -
  # nearly always an item worded in the opposite direction that was not
  # reverse-coded. SPSS footnotes the value; mariposa also names the items
  # whose corrected item-total correlation is negative.
  negative_items <- NULL
  if (isTRUE(alpha < 0) || isTRUE(alpha_standardized < 0)) {
    negative_items <- var_names[!is.na(corrected_item_total_r) &
                                  corrected_item_total_r < 0]
    alpha_txt <- fmt_num(alpha, 3)
    cli_warn(c(
      "Cronbach's alpha is negative ({alpha_txt}){in_group} due to a negative average covariance among items.",
      "i" = "This violates reliability model assumptions; check the item codings.",
      if (length(negative_items)) c(
        "i" = "Negative corrected item-total correlation: {.var {negative_items}}. Reverse-code items worded in the opposite direction."
      )
    ))
  }

  # ============================================================================
  # RETURN RESULT
  # ============================================================================

  list(
    alpha = alpha,
    alpha_standardized = alpha_standardized,
    omega = omega,
    omega_std = omega_std,
    n_items = k,
    item_statistics = item_statistics,
    item_total = item_total,
    inter_item_cor = cor_mat,
    n = n,
    weighted_n = if (!is.null(weights_vec)) w_n else NULL,
    removed_items = if (length(removed_items)) removed_items else NULL,
    omega_note = omega_note,
    negative_alpha = isTRUE(alpha < 0) || isTRUE(alpha_standardized < 0),
    negative_items = negative_items
  )
}


#' Result skeleton for a scale that cannot be analysed
#'
#' @param k Number of items
#' @param n Number of cases
#' @param reason Short text for "not computed (<reason>)" in the output
#' @param weighted_n Sum of weights or NULL
#' @noRd
.reliability_na_result <- function(k, n, reason, weighted_n = NULL) {
  list(
    alpha = NA_real_,
    alpha_standardized = NA_real_,
    omega = NA_real_,
    omega_std = NA_real_,
    n_items = k,
    item_statistics = NULL,
    item_total = NULL,
    inter_item_cor = NULL,
    n = n,
    weighted_n = weighted_n,
    not_computed = reason
  )
}


# ============================================================================
# MCDONALD'S OMEGA HELPER
# ============================================================================

#' Fit a one-factor ML model and compute McDonald's omega
#'
#' @description
#' Fits a single-factor maximum-likelihood factor model to the (possibly
#' weighted) correlation matrix via stats::factanal() and returns both the
#' standardized omega (correlation metric) and the raw/total omega
#' (covariance metric, obtained by rescaling loadings with item SDs and
#' uniquenesses with item variances). Non-convergence and Heywood-adjacent
#' factanal failures are caught and reported as NA plus an error message.
#' See the provenance note in .reliability_core().
#'
#' @param cor_mat (weighted) correlation matrix of the items
#' @param cov_mat (weighted) covariance matrix of the items
#' @param n_obs number of listwise-complete cases; for weighted analyses the
#'   unrounded sum of weights (Charter §5.1 convention)
#' @return list(omega, omega_std, error) — error is NULL on success,
#'   "singular" for a singular correlation matrix, "no_fit" when factanal
#'   failed. factanal's own (translated) warnings and errors are not passed
#'   on; the caller words the warning.
#' @noRd
.omega_one_factor <- function(cor_mat, cov_mat, n_obs) {
  ev <- eigen(cor_mat, symmetric = TRUE, only.values = TRUE)$values
  if (anyNA(ev) || min(ev) <= 1e-8) {
    return(list(omega = NA_real_, omega_std = NA_real_, error = "singular"))
  }
  fit <- tryCatch(
    withCallingHandlers(
      stats::factanal(covmat = cor_mat, factors = 1, n.obs = n_obs),
      warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(list(omega = NA_real_, omega_std = NA_real_, error = "no_fit"))
  }

  # Correlation metric: loadings lambda_i, uniquenesses theta_i
  lambda <- as.numeric(fit$loadings)
  theta  <- as.numeric(fit$uniquenesses)

  # Heywood case: a uniqueness stuck at factanal's lower bound (0.005)
  # means the "factor" is essentially that single item (loading ~ 1). Omega
  # from such a boundary solution is not a reliability estimate (e.g.
  # alpha 0.037 next to omega 0.349), so it is not reported.
  at_bound <- theta <= 0.005 + 1e-4
  if (any(at_bound)) {
    return(list(omega = NA_real_, omega_std = NA_real_, error = "heywood",
                items = rownames(cor_mat)[at_bound]))
  }

  # Standardized omega: (sum lambda)^2 / ((sum lambda)^2 + sum theta)
  omega_std <- sum(lambda)^2 / (sum(lambda)^2 + sum(theta))

  # Raw (total) omega: rescale to the covariance metric using the item
  # SDs/variances from the same (weighted) covariance matrix diagonal
  item_var   <- diag(cov_mat)
  lambda_raw <- lambda * sqrt(item_var)
  theta_raw  <- theta * item_var
  omega <- sum(lambda_raw)^2 / (sum(lambda_raw)^2 + sum(theta_raw))

  list(omega = omega, omega_std = omega_std, error = NULL)
}


# ============================================================================
# WEIGHTED COVARIANCE AND CORRELATION HELPERS
# ============================================================================

#' Compute weighted covariance matrix (SPSS-compatible)
#' @description Uses frequency-weighted formula: cov = sum(w*(x-mx)*(y-my)) / (V1 - 1)
#' @noRd
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
#' @noRd
.weighted_cor <- function(mat, w) {
  cov_mat <- .weighted_cov(mat, w)
  sds <- sqrt(diag(cov_mat))
  cor_mat <- cov_mat / outer(sds, sds)
  # Ensure diagonal is exactly 1
  diag(cor_mat) <- 1
  return(cor_mat)
}

#' Compute weighted correlation between two vectors
#' @noRd
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
# HELPERS
# ============================================================================

#' Interpret Cronbach's Alpha value
#' @noRd
.alpha_interpretation <- function(alpha) {
  if (is.na(alpha)) return("")
  # A negative alpha is not "poor reliability" but a violated model
  # (negative average covariance, usually an unreversed item)
  if (alpha < 0) return("negative; check item coding")
  if (alpha >= 0.90) return("Excellent")
  if (alpha >= 0.80) return("Good")
  if (alpha >= 0.70) return("Acceptable")
  if (alpha >= 0.60) return("Questionable")
  "Poor"
}


# ============================================================================
# PRINT METHOD (compact)
# ============================================================================

#' Print reliability results (compact)
#'
#' @description
#' Compact print method for objects of class \code{"reliability"}.
#' Shows Cronbach's Alpha (with quality interpretation), McDonald's Omega,
#' and item count in a single line per group.
#'
#' For the full detailed output including item statistics, inter-item
#' correlations, and item-total statistics, use \code{summary()}.
#'
#' @param x An object of class \code{"reliability"} returned by
#'   \code{\link{reliability}}.
#' @param digits Number of decimal places to display. Default is \code{3}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- reliability(survey_data, trust_government, trust_media, trust_science)
#' result              # compact one-line overview
#' summary(result)     # full detailed output
#'
#' @export
#' @method print reliability
print.reliability <- function(x, digits = 3, ...) {
  weighted_tag <- if (!is.null(x$weights)) " [Weighted]" else ""

  if (isTRUE(x$is_grouped)) {
    for (group_result in x$groups) {
      group_values <- group_result$group_values
      group_label <- .format_group_label(group_values)
      cat(sprintf("[%s]\n", group_label))
      .print_reliability_compact(group_result, x$n_items, weighted_tag, digits)
    }
  } else {
    .print_reliability_compact(x, x$n_items, weighted_tag, digits)
  }

  invisible(x)
}

#' Print compact one-liner for a single reliability result
#' @noRd
.print_reliability_compact <- function(res, n_items, weighted_tag, digits) {
  if (!is.null(res$not_computed)) {
    cat(sprintf("Reliability Analysis: %d items%s\n", n_items, weighted_tag))
    cat(sprintf("  not computed (%s)\n", res$not_computed))
    return(invisible(NULL))
  }
  alpha <- res$alpha
  interp <- .alpha_interpretation(alpha)
  n_display <- if (!is.null(res$weighted_n)) {
    sprintf("%.0f", res$weighted_n)
  } else {
    as.character(res$n)
  }
  omega <- res$omega %||% NA_real_
  omega_text <- if (is.na(omega)) "not computed" else fmt_num(omega, digits)
  n_items <- res$n_items %||% n_items

  cat(sprintf("Reliability Analysis: %d items%s\n", n_items, weighted_tag))
  cat(sprintf("  Cronbach's Alpha = %s (%s), McDonald's Omega = %s, N = %s\n",
              fmt_num(alpha, digits), interp, omega_text, n_display))
}


# ============================================================================
# SUMMARY METHOD
# ============================================================================

#' Summarize a reliability analysis
#'
#' @description
#' Creates a detailed summary of a reliability analysis result. All sections
#' are shown by default; set individual toggles to \code{FALSE} to suppress
#' specific sections.
#'
#' @param object A \code{reliability} result object
#' @param reliability_statistics Show Cronbach's Alpha and McDonald's Omega
#'   statistics? (Default: TRUE)
#' @param item_statistics Show per-item means and SDs? (Default: TRUE)
#' @param inter_item_correlations Show inter-item correlation matrix? (Default: TRUE)
#' @param item_total_statistics Show item-total statistics? (Default: TRUE)
#' @param digits Number of decimal places (Default: 3)
#' @param ... Additional arguments (ignored)
#'
#' @return A \code{summary.reliability} object (list with \code{$show} toggles)
#'
#' @examples
#' result <- reliability(survey_data, trust_government, trust_media, trust_science)
#' summary(result)
#' summary(result, inter_item_correlations = FALSE)
#'
#' @seealso \code{\link{reliability}} for the main analysis function.
#' @export
#' @method summary reliability
summary.reliability <- function(object, reliability_statistics = TRUE,
                                item_statistics = TRUE,
                                inter_item_correlations = TRUE,
                                item_total_statistics = TRUE,
                                digits = 3, ...) {
  show <- list(
    reliability_statistics = reliability_statistics,
    item_statistics = item_statistics,
    inter_item_correlations = inter_item_correlations,
    item_total_statistics = item_total_statistics
  )
  build_summary_object(object, show, digits, "summary.reliability")
}


#' Print summary of reliability analysis results (detailed output)
#'
#' @description
#' Displays the detailed SPSS-style output for a reliability analysis, with
#' sections controlled by the boolean parameters passed to
#' \code{\link{summary.reliability}}.  Sections include reliability
#' statistics, item statistics, inter-item correlations, and
#' item-total statistics.
#'
#' @param x A \code{summary.reliability} object created by
#'   \code{\link{summary.reliability}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- reliability(survey_data, trust_government, trust_media, trust_science)
#' summary(result)                                  # all sections
#' summary(result, inter_item_correlations = FALSE)  # hide correlations
#'
#' @seealso \code{\link{reliability}} for the main analysis,
#'   \code{\link{summary.reliability}} for summary options.
#' @export
#' @method print summary.reliability
print.summary.reliability <- function(x, ...) {
  title <- get_standard_title("Reliability Analysis", x$weights, "Results")
  print_header(title)

  if (isTRUE(x$is_grouped)) {
    .print_reliability_grouped(x, x$digits)
  } else {
    .print_reliability_ungrouped(x, x$digits)
  }

  invisible(x)
}

#' Print reliability results for ungrouped data
#' @noRd
.print_reliability_ungrouped <- function(x, digits = 3) {
  show_reliability_stats <- if (!is.null(x$show)) isTRUE(x$show$reliability_statistics) else TRUE
  show_item_stats <- if (!is.null(x$show)) isTRUE(x$show$item_statistics) else TRUE
  show_inter_item <- if (!is.null(x$show)) isTRUE(x$show$inter_item_correlations) else TRUE
  show_item_total <- if (!is.null(x$show)) isTRUE(x$show$item_total_statistics) else TRUE

  # Info section
  print_info_section(list(
    "Items" = paste(x$variables, collapse = ", "),
    "N of Items" = x$n_items,
    "Removed (zero variance)" = if (length(x$removed_items)) {
      paste(x$removed_items, collapse = ", ")
    },
    "Weights" = x$weights
  ))

  if (!is.null(x$not_computed)) {
    cat(sprintf("\n  not computed (%s)\n", x$not_computed))
    return(invisible(NULL))
  }

  # Reliability Statistics
  if (show_reliability_stats) {
    cat("\n")
    cat("Reliability Statistics\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")

    omega_na <- sprintf("not computed (%s)", x$omega_note %||% "see warning")
    show_value <- function(v, na_text) if (is.na(v)) na_text else fmt_num(v, digits)
    cat(sprintf("  Cronbach's Alpha:              %s\n",
                show_value(x$alpha, "not computed")))
    cat(sprintf("  Alpha (standardized):          %s\n",
                show_value(x$alpha_standardized, "not computed")))
    cat(sprintf("  McDonald's Omega:              %s\n",
                show_value(x$omega %||% NA_real_, omega_na)))
    cat(sprintf("  Omega (standardized):          %s\n",
                show_value(x$omega_std %||% NA_real_, omega_na)))
    cat(sprintf("  N of Items:                    %d\n", x$n_items))

    n_label <- if (!is.null(x$weighted_n)) {
      sprintf("%.2f (weighted)", x$weighted_n)
    } else {
      as.character(x$n)
    }
    cat(sprintf("  N (listwise):                  %s\n", n_label))
    if (isTRUE(x$negative_alpha)) .print_negative_alpha_note(x$negative_items)
  }

  # Item Statistics (fixed decimals; weighted N with 2 decimals as SPSS)
  if (show_item_stats && !is.null(x$item_statistics)) {
    cat("\nItem Statistics\n")
    item_df <- as.data.frame(x$item_statistics)
    item_df$n <- if (!is.null(x$weighted_n)) {
      formatC(item_df$n, format = "f", digits = 2)
    } else {
      formatC(round(item_df$n), format = "d")
    }
    print_stat_table(item_df, digits = digits,
                     col_types = c(mean = "num", sd = "num", n = "char"),
                     col_labels = c(item = "Item", mean = "Mean",
                                    sd = "Std. Deviation", n = "N"))
  }

  # Inter-Item Correlation Matrix
  if (show_inter_item && !is.null(x$inter_item_cor)) {
    cat("\nInter-Item Correlation Matrix\n")
    .print_numbered_matrix(x$inter_item_cor, digits)
  }

  # Item-Total Statistics
  if (show_item_total && !is.null(x$item_total)) {
    cat("\nItem-Total Statistics\n")
    it <- x$item_total
    omega_del <- it$omega_if_deleted
    cols <- list(
      list(h1 = "Scale Mean", h2 = "if Deleted", v = it$scale_mean_if_deleted),
      list(h1 = "Scale Var.", h2 = "if Deleted", v = it$scale_var_if_deleted),
      list(h1 = "Corrected", h2 = "Item-Total", v = it$corrected_item_total_r),
      list(h1 = "Alpha if", h2 = "Deleted", v = it$alpha_if_deleted)
    )
    if (!is.null(omega_del)) {
      cols[[5]] <- list(h1 = "Omega if", h2 = "Deleted", v = omega_del)
    }
    .print_two_header_table(it$item, "Item", cols, digits)

    # A 3-item scale leaves a 2-item one-factor model after deletion, which
    # is not identified - explain the empty column instead of leaving it bare.
    if (!is.null(omega_del) && all(is.na(omega_del))) {
      if (length(omega_del) == 3) {
        cat("Note: Omega if item deleted requires at least 4 items\n")
        cat("(a one-factor model on the remaining 2 items is not identified).\n")
      } else if (!is.null(x$omega_note)) {
        cat(sprintf("Note: Omega if item deleted is not computed (%s).\n",
                    x$omega_note))
      }
    }
  }
}

#' Print a table with a two-line column header
#'
#' Used for Item-Total Statistics, whose SPSS column names ("Scale Mean if
#' Item Deleted", ...) are too long for one header line: print.data.frame
#' wrapped the table at 80 columns under snake_case names.
#' @param labels Row labels (first column)
#' @param label_head Header of the first column
#' @param cols List of list(h1, h2, v): header lines and numeric values
#' @param digits Decimal places
#' @noRd
.print_two_header_table <- function(labels, label_head, cols, digits) {
  w0 <- max(nchar(label_head), nchar(labels, type = "width"))
  cells <- lapply(cols, function(cl) fmt_num(cl$v, digits))
  widths <- mapply(function(cl, ce) max(nchar(cl$h1), nchar(cl$h2), nchar(ce)),
                   cols, cells)
  total <- w0 + sum(widths + 2L)
  border <- paste0("  ", strrep("-", total))
  line <- function(first, parts) {
    paste0("  ", pad_utf8(first, w0),
           paste0("  ", mapply(pad_utf8, parts, widths, "right"), collapse = ""))
  }
  cat(border, "\n", sep = "")
  cat(line("", vapply(cols, `[[`, "", "h1")), "\n", sep = "")
  cat(line(label_head, vapply(cols, `[[`, "", "h2")), "\n", sep = "")
  cat(border, "\n", sep = "")
  for (i in seq_along(labels)) {
    cat(line(labels[i], vapply(cells, `[`, "", i)), "\n", sep = "")
  }
  cat(border, "\n", sep = "")
  invisible(NULL)
}

#' Print a correlation matrix with numbered columns
#'
#' Rows show "(1) item_name", columns only "(1)", "(2)", ...: the matrix
#' stays narrow however long the item names are, the digits argument is
#' honoured (the shared .print_cor_matrix() forced 2 decimals for more than
#' 6 items), and columns are split into blocks that fit the console.
#' @noRd
.print_numbered_matrix <- function(mat, digits) {
  k <- ncol(mat)
  idx <- paste0("(", seq_len(k), ")")
  row_lab <- paste(idx, rownames(mat))
  w0 <- max(nchar(row_lab, type = "width"))
  cells <- matrix(fmt_num(as.numeric(mat), digits), k)
  wc <- max(nchar(cells), nchar(idx))
  per_block <- max(1L, floor((getOption("width", 80) - 2 - w0) / (wc + 2)))
  blocks <- split(seq_len(k), ceiling(seq_len(k) / per_block))
  for (b in blocks) {
    border <- paste0("  ", strrep("-", w0 + length(b) * (wc + 2)))
    cat(border, "\n", sep = "")
    cat("  ", strrep(" ", w0),
        paste0("  ", vapply(idx[b], pad_utf8, "", wc, "right"), collapse = ""),
        "\n", sep = "")
    cat(border, "\n", sep = "")
    for (i in seq_len(k)) {
      cat("  ", pad_utf8(row_lab[i], w0),
          paste0("  ", vapply(cells[i, b], pad_utf8, "", wc, "right"), collapse = ""),
          "\n", sep = "")
    }
    cat(border, "\n", sep = "")
  }
  invisible(NULL)
}

#' SPSS's footnote for a negative alpha, plus the items to check
#' @noRd
.print_negative_alpha_note <- function(negative_items) {
  cat("Note: The value is negative due to a negative average covariance among\n")
  cat("items. This violates reliability model assumptions. You may want to\n")
  cat("check item codings.\n")
  if (length(negative_items)) {
    cat(sprintf("Negative corrected item-total correlation: %s\n",
                paste(negative_items, collapse = ", ")))
  }
  invisible(NULL)
}

#' Print reliability results for grouped data
#' @noRd
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
