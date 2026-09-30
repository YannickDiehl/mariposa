
#' Analysis of Covariance: ANCOVA
#'
#' @description
#' \code{ancova()} tests whether group means differ after controlling for one or
#' more continuous covariates. It performs a factorial ANCOVA using Type III Sum of
#' Squares, matching SPSS UNIANOVA output with the WITH keyword.
#'
#' Think of it as:
#' - An ANOVA that removes the effect of confounding variables first
#' - Testing group differences on "adjusted" means
#' - Combining regression (covariates) and ANOVA (factors) in one model
#'
#' The test tells you:
#' - Whether each factor has a significant effect AFTER controlling for covariates
#' - The relationship between each covariate and the outcome
#' - Effect sizes for factors and covariates (partial eta squared)
#' - Estimated marginal means (group means adjusted for covariates)
#'
#' @param data Your survey data (a data frame or tibble)
#' @param dv The numeric dependent variable to analyze (unquoted)
#' @param between Character vector or unquoted variable names specifying the
#'   between-subjects factors (1-3 factors). These must be categorical variables
#'   (factor, character, or labelled numeric).
#' @param covariate Unquoted variable names of the continuous covariates to control
#'   for. Use \code{c(age, income)} for multiple covariates.
#' @param weights Optional survey weights for population-representative
#'   results. Give a column name (unquoted or as a string), an expression such
#'   as \code{sampling_weight * 2}, or a numeric vector with one weight per row.
#' @param ss_type Deprecated; only 3 (Type III, the SPSS default) is
#'   implemented. Passing 2 issues a warning and computes Type III.
#'
#' @return An object of class \code{"ancova"} containing:
#' \describe{
#'   \item{anova_table}{Tibble with Source, SS, df, MS, F, p, Partial Eta Squared}
#'   \item{parameter_estimates}{Tibble with regression coefficients (B, SE,
#'     t, p, CI, partial eta squared) in SPSS coding: one row per category,
#'     the last category of each factor is the reference and reported as a
#'     redundant 0 (\code{redundant = TRUE})}
#'   \item{descriptives}{Tibble with unadjusted cell means, SDs, and Ns}
#'   \item{estimated_marginal_means}{Tibble with adjusted cell means (covariates at grand mean)}
#'   \item{emm_main_effects}{For 2+ factors: named list of tibbles with the
#'     adjusted main-effect means (unweighted average of the cell means, as
#'     SPSS \code{/EMMEANS=TABLES(factor)}); NULL for one factor}
#'   \item{levene_test}{Tibble with Levene's test of equality of error
#'     variances (f, df1, df2, p): as in SPSS UNIANOVA, a one-way ANOVA of
#'     the absolute residuals of the ANCOVA model across the cells of the
#'     design. Also available as \code{levene_test(result)}.}
#'   \item{r_squared}{R-squared and Adjusted R-squared}
#'   \item{model}{The underlying lm model object}
#'   \item{call_info}{List with metadata (dv, factors, covariates, weighted, etc.)}
#' }
#'   Use \code{summary()} for the full SPSS-style output with toggleable sections.
#'   For data grouped with \code{group_by()}, one ANCOVA is computed per group:
#'   the tables carry the group keys as leading columns and
#'   \code{group_results} holds the complete result of each group
#'   (\code{NULL}, with a warning, for a group that cannot be analysed).
#'
#' @details
#' ## Understanding the Results
#'
#' **Adjusted Means (Estimated Marginal Means)**:
#' These are the group means after statistically removing the effect of the
#' covariate(s). They answer: "What would the group means be if all groups had
#' the same covariate values?"
#'
#' **Covariate Effects**: The covariate row in the ANOVA table shows whether the
#' covariate significantly predicts the DV after adjusting for the factors.
#'
#' **Factor Effects**: These show whether the factor affects the DV after
#' controlling for the covariate. This is the primary test of interest.
#'
#' **Partial Eta Squared** (Effect Size):
#' \itemize{
#'   \item Less than 0.01: Negligible
#'   \item 0.01 to 0.06: Small
#'   \item 0.06 to 0.14: Medium
#'   \item 0.14 or greater: Large
#' }
#'
#' ## When to Use This
#'
#' Use ANCOVA when:
#' - You have group comparisons (ANOVA) but want to control for a confound
#' - Your covariate is continuous and linearly related to the DV
#' - You want to increase statistical power by removing known variance sources
#' - The covariate's relationship with the DV is the same across groups
#'   (homogeneity of regression slopes assumption)
#'
#' @seealso
#' \code{\link{factorial_anova}} for ANOVA without covariates.
#'
#' \code{\link{linear_regression}} for regression analysis.
#'
#' \code{\link{oneway_anova}} for single-factor ANOVA.
#'
#' \code{\link{summary.ancova}} for detailed output with toggleable sections.
#'
#' @references
#' Huitema, B. E. (2011). The Analysis of Covariance and Alternatives
#' (2nd ed.). Wiley.
#'
#' IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # One-way ANCOVA: income by education, controlling for age
#' survey_data %>%
#'   ancova(dv = income, between = c(education), covariate = c(age))
#'
#' # Two-way ANCOVA with weights
#' survey_data %>%
#'   ancova(dv = income, between = c(gender, education),
#'          covariate = c(age), weights = sampling_weight)
#'
#' # Multiple covariates
#' survey_data %>%
#'   ancova(dv = income, between = c(education),
#'          covariate = c(age, political_orientation))
#'
#' # --- Three-layer output ---
#' result <- ancova(survey_data, dv = income, between = c(education),
#'                  covariate = c(age))
#' result              # compact overview
#' summary(result)     # full detailed output with all sections
#' summary(result, marginal_means = FALSE)  # hide estimated marginal means
#'
#' @family hypothesis_tests
#' @export
ancova <- function(data, dv, between, covariate, weights = NULL, ss_type = 3) {
  .check_required(c("data", "dv", "between", "covariate"))

  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================

  .reject_partial_args()
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }

  if (ss_type %in% c(2, 2L)) {
    cli_warn(c(
      "!" = "Type II sums of squares are not implemented; computing Type III (the SPSS default).",
      "i" = "The {.arg ss_type} argument is deprecated and will be removed in a future release."
    ))
    ss_type <- 3
  } else if (!ss_type %in% c(3, 3L)) {
    cli_abort("{.arg ss_type} must be 3 (Type III, the SPSS default).")
  }

  # Process DV
  dv_quo <- rlang::enquo(dv)
  dv_name <- rlang::as_name(dv_quo)

  if (!dv_name %in% names(data)) {
    cli_abort("Dependent variable {.var {dv_name}} not found in data.")
  }
  if (!is.numeric(data[[dv_name]])) {
    cli_abort("Dependent variable {.var {dv_name}} must be numeric.")
  }

  # Process between-subjects factors (reuse factorial_anova parser)
  between_quo <- rlang::enquo(between)
  between_names <- .parse_between_factors(between_quo, data)

  if (length(between_names) < 1) {
    cli_abort("{.fn ancova} requires at least 1 between-subjects factor.")
  }
  if (length(between_names) > 3) {
    cli_abort("{.fn ancova} supports at most 3 between-subjects factors.")
  }

  # Validate factors
  for (bn in between_names) {
    if (!bn %in% names(data)) {
      cli_abort("Factor {.var {bn}} not found in data.")
    }
  }

  # Process covariates
  covariate_quo <- rlang::enquo(covariate)
  covariate_names <- .parse_between_factors(covariate_quo, data)

  if (length(covariate_names) < 1) {
    cli_abort("{.fn ancova} requires at least 1 covariate.")
  }

  for (cn in covariate_names) {
    if (!cn %in% names(data)) {
      cli_abort("Covariate {.var {cn}} not found in data.")
    }
    if (!is.numeric(data[[cn]])) {
      cli_abort("Covariate {.var {cn}} must be numeric.")
    }
  }

  # Process weights
  weights_quo <- rlang::enquo(weights)
  weights_info <- .process_weights(data, weights_quo)
  data <- weights_info$data
  w_name <- weights_info$name

  # Grouped data (group_by()): one complete analysis per group, the
  # weighting semantics of each group's fit are those of an ungrouped call
  if (inherits(data, "grouped_df")) {
    return(.ancova_grouped(data, dv_name, between_names, covariate_names, w_name, ss_type))
  }

  .ancova_fit(data, dv_name, between_names, covariate_names, w_name, ss_type, call = rlang::current_env())
}

#' One ancova (the whole data or one group of a grouped analysis)
#'
#' Problems that make the analysis impossible (too few cases, a factor with
#' one level, a DV without variance) abort for ungrouped data and skip the
#' group with a warning naming it for grouped data (.fit_problem()).
#' @noRd
.ancova_fit <- function(data, dv_name, between_names, covariate_names, w_name, ss_type, group_info = NULL,
                    call = rlang::caller_env()) {
  # ============================================================================
  # DATA PREPARATION
  # ============================================================================

  all_vars <- c(dv_name, between_names, covariate_names)
  if (!is.null(w_name)) all_vars <- c(all_vars, w_name)

  complete_idx <- stats::complete.cases(data[, all_vars, drop = FALSE])
  if (!is.null(w_name)) {
    complete_idx <- complete_idx & data[[w_name]] > 0
  }

  data_complete <- data[complete_idx, , drop = FALSE]
  n_total <- nrow(data_complete)
  n_missing <- sum(!complete_idx)

  if (n_total < length(between_names) + length(covariate_names) + 2) {
    .fit_problem(sprintf("insufficient observations (%d) after removing missing values",
                         n_total), dv_name, group_info, call)
  }

  # Convert factors
  for (bn in between_names) {
    # SPSS order (by code) and value labels instead of codes
    data_complete[[bn]] <- .group_factor(data_complete[[bn]])
    data_complete[[bn]] <- droplevels(data_complete[[bn]])
    if (nlevels(data_complete[[bn]]) < 2) {
      .fit_problem(sprintf("factor `%s` has only one level with valid data (%s)",
                           bn, paste(levels(data_complete[[bn]]), collapse = "")),
                   dv_name, group_info, call)
    }
  }

  # A constant DV (or one constant within every cell) has SS = 0/0 up to
  # floating-point noise: every F would be pure rounding error
  reason <- .dv_degenerate_reason(
    data_complete[[dv_name]],
    interaction(data_complete[between_names], drop = TRUE)
  )
  if (!is.null(reason)) .fit_problem(reason, dv_name, group_info, call)
  .warn_empty_cells("ancova", data_complete, between_names, group_info)

  # ============================================================================
  # MODEL FITTING WITH TYPE III SS
  # ============================================================================

  # Save + register restoration BEFORE changing the option, so an
  # interrupt between the two lines cannot leak the changed setting
  # (CRAN review 2026-09: on.exit() must immediately follow the save).
  old_contrasts <- options("contrasts")$contrasts
  on.exit(options(contrasts = old_contrasts), add = TRUE)
  options(contrasts = c("contr.sum", "contr.poly"))

  # Build formula: dv ~ covariate1 + covariate2 + factor1 * factor2
  # SPSS convention: covariates are listed before factors
  cov_part <- paste(covariate_names, collapse = " + ")
  factor_part <- paste(between_names, collapse = " * ")
  formula_str <- paste(dv_name, "~", cov_part, "+", factor_part)
  model_formula <- stats::as.formula(formula_str)

  # Fit the model
  if (!is.null(w_name)) {
    data_complete$.wt <- data_complete[[w_name]]
    model <- stats::lm(model_formula, data = data_complete, weights = .wt)
  } else {
    model <- stats::lm(model_formula, data = data_complete)
  }

  # Type III SS
  type3 <- stats::drop1(model, scope = . ~ ., test = "F")

  # ============================================================================
  # BUILD ANOVA TABLE
  # ============================================================================

  anova_table <- .build_ancova_table(model, type3, between_names,
                                      covariate_names, dv_name, n_total, w_name)

  # ============================================================================
  # PARAMETER ESTIMATES
  # ============================================================================

  param_est <- .compute_parameter_estimates(model_formula, data_complete,
                                            between_names, w_name)

  # ============================================================================
  # DESCRIPTIVE STATISTICS (unadjusted)
  # ============================================================================

  descriptives <- .compute_factorial_descriptives(data_complete, dv_name,
                                                   between_names, w_name)

  # ============================================================================
  # ESTIMATED MARGINAL MEANS (adjusted for covariates at grand mean)
  # ============================================================================

  emm <- .compute_estimated_marginal_means(model, data_complete, dv_name,
                                            between_names, covariate_names,
                                            w_name)
  # Main-effect marginal means (SPSS /EMMEANS=TABLES(factor)) for designs
  # with several factors; with one factor the cell table is the main effect
  emm_main <- if (length(between_names) > 1) {
    .compute_emm_main_effects(model, data_complete, between_names,
                              covariate_names, w_name)
  } else NULL

  # ============================================================================
  # LEVENE'S TEST
  # ============================================================================

  levene_result <- .compute_ancova_levene(model, data_complete, dv_name,
                                          between_names, w_name)

  # ============================================================================
  # R-SQUARED
  # ============================================================================

  ss_error <- anova_table$ss[anova_table$source == "Error"]
  ss_corrected_total <- anova_table$ss[anova_table$source == "Corrected Total"]
  r_squared <- 1 - ss_error / ss_corrected_total

  # Count df: covariates + factor terms
  model_rows <- !anova_table$source %in%
    c("Error", "Total", "Corrected Total", "Corrected Model", "Intercept")
  df_model <- sum(anova_table$df[model_rows])
  df_error <- anova_table$df[anova_table$source == "Error"]
  adj_r_squared <- 1 - (1 - r_squared) * (n_total - 1) / df_error

  # ============================================================================
  # RESULT OBJECT
  # ============================================================================

  result <- structure(
    list(
      anova_table = anova_table,
      parameter_estimates = param_est,
      descriptives = descriptives,
      estimated_marginal_means = emm,
      emm_main_effects = emm_main,
      levene_test = levene_result,
      r_squared = c(r_squared = r_squared, adj_r_squared = adj_r_squared),
      model = model,
      call_info = list(
        dv = dv_name,
        factors = between_names,
        covariates = covariate_names,
        weighted = !is.null(w_name),
        weight_name = w_name,
        n_total = n_total,
        n_missing = n_missing,
        ss_type = ss_type
      ),
      data = data_complete,
      variables = dv_name,
      group = between_names,
      weights = w_name,
      is_grouped = FALSE,
      groups = NULL
    ),
    class = "ancova"
  )

  return(result)
}


# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

#' Levene's test of equality of error variances for ANCOVA
#'
#' SPSS UNIANOVA (/PRINT HOMOGENEITY) tests the equality of the ERROR
#' variances of the fitted model: a one-way ANOVA of the absolute residuals
#' of the full model (covariates and all factor terms) across the cells of
#' the design. Centring the DV on the raw cell means instead ignores the
#' covariates and differs from SPSS as soon as a covariate matters
#' (life_satisfaction BY gender WITH age: 1.277 instead of SPSS 1.306).
#' Without covariates the two coincide, so factorial_anova() keeps its
#' cell-mean version. Reproduces every unweighted Levene test in
#' tests/spss_reference/outputs/ancova_output.txt.
#'
#' The weighted (/REGWGT) path is deliberately unchanged
#' (.compute_factorial_levene(): sqrt(w) * |y - weighted cell mean|) until
#' the pending SPSS reference run for weighted analyses.
#' @param model The fitted lm of the ANCOVA
#' @param data The complete cases the model was fitted on
#' @return Tibble with f, df1, df2, p
#' @noRd
.compute_ancova_levene <- function(model, data, dv_name, between_names, w_name) {
  z <- abs(unname(stats::residuals(model)))
  if (!is.null(w_name)) {
    # /REGWGT: SPSS tests sqrt(w) * |residual| (reproduces ancova_output.txt
    # 2a/2b/4a/4b/6a exactly); w == 1 reduces to the unweighted test
    z <- .w_regwgt_levene_dev(z, data[[w_name]])
  }
  g <- interaction(data[between_names], drop = TRUE, sep = "_")
  levene_aov <- summary(stats::aov(z ~ g))[[1]]
  tibble::tibble(
    f = levene_aov["g", "F value"],
    df1 = as.integer(levene_aov["g", "Df"]),
    df2 = as.integer(levene_aov["Residuals", "Df"]),
    p = levene_aov["g", "Pr(>F)"]
  )
}

#' Grouped ANCOVA: one fit per group_by() group (see .factorial_anova_grouped)
#' @noRd
.ancova_grouped <- function(data, dv_name, between_names, covariate_names,
                            w_name, ss_type) {
  g <- .grouped_model_fits("ancova", data, dv_name, function(d, gi) {
    .ancova_fit(d, dv_name, between_names, covariate_names, w_name, ss_type,
                group_info = gi)
  })
  fits <- g$fits
  ok <- !vapply(fits, is.null, logical(1))
  structure(
    list(
      anova_table = .bind_group_tables(fits, g$keys, "anova_table"),
      parameter_estimates = .bind_group_tables(fits, g$keys, "parameter_estimates"),
      descriptives = .bind_group_tables(fits, g$keys, "descriptives"),
      estimated_marginal_means = .bind_group_tables(fits, g$keys, "estimated_marginal_means"),
      emm_main_effects = NULL,
      levene_test = .bind_group_tables(fits, g$keys, "levene_test"),
      r_squared = NULL,
      model = NULL,
      call_info = list(
        dv = dv_name,
        factors = between_names,
        covariates = covariate_names,
        weighted = !is.null(w_name),
        weight_name = w_name,
        n_total = sum(vapply(fits[ok], function(f) f$call_info$n_total, numeric(1))),
        n_missing = sum(vapply(fits[ok], function(f) f$call_info$n_missing, numeric(1))),
        ss_type = ss_type
      ),
      data = NULL,
      variables = dv_name,
      group = between_names,
      weights = w_name,
      is_grouped = TRUE,
      groups = g$group_vars,
      group_keys = g$keys,
      group_results = fits,
      group_notes = g$notes
    ),
    class = "ancova"
  )
}


#' Build ANCOVA table from lm model and drop1 results
#' @noRd
.build_ancova_table <- function(model, type3, between_names, covariate_names,
                                 dv_name, n_total, w_name) {

  model_summary <- summary(model)

  # All term names (covariates first, then factors and interactions)
  term_names <- attr(stats::terms(model), "term.labels")

  # SS Error
  if (!is.null(w_name)) {
    resid_raw <- stats::residuals(model)
    w_vec <- model$model$`(weights)`
    ss_error <- sum(w_vec * resid_raw^2)
  } else {
    ss_error <- sum(stats::residuals(model)^2)
  }

  df_error <- model$df.residual

  # Extract Type III SS for each term
  term_ss <- numeric(length(term_names))
  term_df <- integer(length(term_names))
  names(term_ss) <- term_names
  names(term_df) <- term_names

  for (tn in term_names) {
    if (tn %in% rownames(type3)) {
      term_ss[tn] <- type3[tn, "Sum of Sq"]
      term_df[tn] <- type3[tn, "Df"]
    }
  }

  ms_error <- ss_error / df_error
  tested <- .type3_testable_terms(term_ss, term_df, ms_error, ss_error, df_error)
  term_ss <- tested$ss
  term_ms <- tested$ms
  term_f <- tested$f
  term_p <- tested$p
  term_eta <- tested$eta

  # Corrected Total and Corrected Model
  if (!is.null(w_name)) {
    y <- model$model[[1]]
    w_vec <- model$model$`(weights)`
    grand_mean <- sum(y * w_vec) / sum(w_vec)
    ss_corrected_total <- sum(w_vec * (y - grand_mean)^2)
    ss_total <- sum(w_vec * y^2)
  } else {
    y <- model$model[[1]]
    grand_mean <- mean(y)
    ss_corrected_total <- sum((y - grand_mean)^2)
    ss_total <- sum(y^2)
  }

  ss_corrected_model <- ss_corrected_total - ss_error
  # Model degrees of freedom = rank - 1 (equals the sum of the Type III df
  # in a full-rank design; with empty cells some terms have df 0)
  df_corrected_model <- model$rank - 1L
  ms_corrected_model <- ss_corrected_model / df_corrected_model
  f_corrected_model <- ms_corrected_model / ms_error
  p_corrected_model <- stats::pf(f_corrected_model, df_corrected_model,
                                  df_error, lower.tail = FALSE)
  eta_corrected_model <- ss_corrected_model / (ss_corrected_model + ss_error)

  # Type III Intercept SS (using b0 / se(b0) approach)
  df_intercept <- 1
  vcov_mat <- stats::vcov(model)
  b0 <- stats::coef(model)["(Intercept)"]
  se_b0 <- sqrt(vcov_mat["(Intercept)", "(Intercept)"])
  f_intercept <- (b0 / se_b0)^2
  ss_intercept <- unname(f_intercept * ms_error)
  ms_intercept <- ss_intercept
  f_intercept <- unname(f_intercept)
  p_intercept <- stats::pf(f_intercept, df_intercept, df_error, lower.tail = FALSE)
  eta_intercept <- ss_intercept / (ss_intercept + ss_error)

  # Total
  df_total <- n_total
  df_corrected_total <- n_total - 1

  # Format term names (replace ":" with " * " for interactions)
  display_names <- gsub(":", " * ", term_names)

  sources <- c("Corrected Model", "Intercept", display_names,
               "Error", "Total", "Corrected Total")
  ss_vals <- c(ss_corrected_model, ss_intercept, term_ss,
               ss_error, ss_total, ss_corrected_total)
  df_vals <- c(df_corrected_model, df_intercept, term_df,
               df_error, df_total, df_corrected_total)
  ms_vals <- c(ms_corrected_model, ms_intercept, term_ms,
               ms_error, NA_real_, NA_real_)
  f_vals <- c(f_corrected_model, f_intercept, term_f,
              NA_real_, NA_real_, NA_real_)
  p_vals <- c(p_corrected_model, p_intercept, term_p,
              NA_real_, NA_real_, NA_real_)
  eta_vals <- c(eta_corrected_model, eta_intercept, term_eta,
                NA_real_, NA_real_, NA_real_)

  tibble::tibble(
    source = sources,
    ss = ss_vals,
    df = as.integer(df_vals),
    ms = ms_vals,
    f = f_vals,
    p = p_vals,
    partial_eta_sq = eta_vals,
    note = c(NA_character_, NA_character_, tested$note,
             NA_character_, NA_character_, NA_character_)
  )
}


#' Parameter estimates in SPSS UNIANOVA coding
#'
#' The Type III table needs sum-to-zero contrasts, but SPSS's "Parameter
#' Estimates" use indicator coding with the LAST category of every factor as
#' the reference: one row per category ("[education=Basic Secondary]"), the
#' last one (and every interaction cell involving a last category) set to 0
#' as redundant. The model is therefore refitted with contr.SAS (treatment
#' contrasts, base = last level) for all factors; the fit, residuals and
#' Type III tests are identical. Previously R's contrasts leaked into the
#' table (education.L/.Q/.C for ordered factors, gender1 for contr.sum).
#'
#' @return tibble parameter, b, se, t, p, ci_lower, ci_upper,
#'   partial_eta_sq, redundant
#' @noRd
.compute_parameter_estimates <- function(model_formula, data, between_names,
                                         w_name) {
  ctr <- stats::setNames(rep(list("contr.SAS"), length(between_names)),
                         between_names)
  fit <- if (!is.null(w_name)) {
    stats::lm(model_formula, data = data, weights = .wt, contrasts = ctr)
  } else {
    stats::lm(model_formula, data = data, contrasts = ctr)
  }

  coefs <- summary(fit)$coefficients
  ci <- suppressWarnings(stats::confint(fit))
  df_error <- fit$df.residual

  one_row <- function(label, coef_name, redundant = FALSE) {
    if (!redundant && coef_name %in% rownames(coefs)) {
      t_val <- coefs[coef_name, "t value"]
      tibble::tibble(
        parameter = label,
        b = coefs[coef_name, "Estimate"],
        se = coefs[coef_name, "Std. Error"],
        t = t_val,
        p = coefs[coef_name, "Pr(>|t|)"],
        ci_lower = ci[coef_name, 1],
        ci_upper = ci[coef_name, 2],
        partial_eta_sq = t_val^2 / (t_val^2 + df_error),
        redundant = FALSE
      )
    } else {
      # SPSS: "This parameter is set to zero because it is redundant"
      # (reference category, or not estimable because of an empty cell)
      tibble::tibble(parameter = label, b = 0, se = NA_real_, t = NA_real_,
                     p = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
                     partial_eta_sq = NA_real_, redundant = TRUE)
    }
  }

  rows <- list(one_row("Intercept", "(Intercept)"))
  for (term in attr(stats::terms(fit), "term.labels")) {
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    if (!all(parts %in% between_names)) {
      rows[[length(rows) + 1]] <- one_row(term, term)
      next
    }
    lvls <- lapply(parts, function(f) levels(data[[f]]))
    # SPSS order: the last factor of the term varies fastest
    grid <- rev(expand.grid(rev(lvls), stringsAsFactors = FALSE))
    for (r in seq_len(nrow(grid))) {
      lv <- unlist(grid[r, ], use.names = FALSE)
      is_last <- mapply(function(l, all_l) identical(l, all_l[length(all_l)]),
                        lv, lvls)
      rows[[length(rows) + 1]] <- one_row(
        paste0("[", parts, "=", lv, "]", collapse = " * "),
        paste0(parts, lv, collapse = ":"),
        redundant = any(is_last)
      )
    }
  }
  dplyr::bind_rows(rows)
}


#' Main-effect estimated marginal means (SPSS /EMMEANS=TABLES(factor))
#'
#' For each factor level: the unweighted mean of the predicted cell means
#' over all levels of the other factors, covariates at their (weighted)
#' means, SE from the contrast vector L (the averaged design rows):
#' sqrt(L V L'). Not estimable (NA) when the model is rank deficient (an
#' empty design cell).
#' @noRd
.compute_emm_main_effects <- function(model, data, between_names,
                                      covariate_names, w_name) {
  lvls <- lapply(between_names, function(b) levels(data[[b]]))
  grid <- expand.grid(lvls, stringsAsFactors = FALSE)
  names(grid) <- between_names
  for (b in between_names) {
    grid[[b]] <- factor(grid[[b]], levels = levels(data[[b]]))
  }
  for (cn in covariate_names) {
    grid[[cn]] <- if (!is.null(w_name)) {
      sum(data[[cn]] * data[[w_name]]) / sum(data[[w_name]])
    } else {
      mean(data[[cn]])
    }
  }
  tt <- stats::delete.response(stats::terms(model))
  X <- stats::model.matrix(tt, grid, contrasts.arg = model$contrasts,
                           xlev = model$xlevels)
  beta <- stats::coef(model)
  V <- stats::vcov(model)
  estimable <- !anyNA(beta)
  t_crit <- stats::qt(0.975, model$df.residual)

  out <- lapply(between_names, function(b) {
    rows <- lapply(levels(data[[b]]), function(l) {
      L <- colMeans(X[grid[[b]] == l, , drop = FALSE])
      est <- if (estimable) sum(L * beta) else NA_real_
      se <- if (estimable) sqrt(drop(t(L) %*% V %*% L)) else NA_real_
      row <- tibble::tibble(level = factor(l, levels = levels(data[[b]])),
                            mean = est, se = se,
                            ci_lower = est - t_crit * se,
                            ci_upper = est + t_crit * se)
      names(row)[1] <- b
      row
    })
    dplyr::bind_rows(rows)
  })
  stats::setNames(out, between_names)
}


#' Compute estimated marginal means (adjusted for covariates at their grand mean)
#' @noRd
.compute_estimated_marginal_means <- function(model, data, dv_name,
                                               between_names, covariate_names,
                                               w_name) {

  # Create prediction grid: all factor combinations, covariates at grand mean
  factor_cols <- data[, between_names, drop = FALSE]
  cells <- unique(factor_cols)
  cells <- cells[do.call(order, cells), , drop = FALSE]

  # Set covariates to their grand mean
  for (cn in covariate_names) {
    if (!is.null(w_name)) {
      cells[[cn]] <- sum(data[[cn]] * data[[w_name]]) / sum(data[[w_name]])
    } else {
      cells[[cn]] <- mean(data[[cn]])
    }
  }

  # Add .wt column if needed (predict needs all model columns)
  if (!is.null(w_name)) {
    cells$.wt <- 1  # dummy, not used in prediction
  }

  # Predict
  pred <- stats::predict(model, newdata = cells, se.fit = TRUE)

  # Build result
  result <- cells[, between_names, drop = FALSE]
  result$mean <- pred$fit
  result$se <- pred$se.fit

  # CI: 95% confidence interval
  df_error <- model$df.residual
  t_crit <- stats::qt(0.975, df_error)
  result$ci_lower <- pred$fit - t_crit * pred$se.fit
  result$ci_upper <- pred$fit + t_crit * pred$se.fit

  tibble::as_tibble(result)
}


# ==============================================================================
# PRINT / SUMMARY METHODS
# ==============================================================================

#' Print ANCOVA results (compact)
#'
#' @description
#' Compact print method for objects of class \code{"ancova"}.
#' Shows factor effects and covariates with F statistics, p-values,
#' and effect sizes.
#'
#' For the full detailed output, use \code{summary()}.
#'
#' @param x An object of class \code{"ancova"} returned by
#'   \code{\link{ancova}}.
#' @param digits Number of decimal places to display. Default is \code{3}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- ancova(survey_data, dv = life_satisfaction, between = gender, covariate = age)
#' result              # compact overview
#' summary(result)     # full detailed output
#'
#' @export
#' @method print ancova
print.ancova <- function(x, digits = 3, ...) {

  info <- x$call_info
  weighted_tag <- if (info$weighted) " [Weighted]" else ""
  factor_str <- paste(info$factors, collapse = ", ")
  cov_str <- paste(info$covariates, collapse = ", ")

  if (isTRUE(x$is_grouped)) {
    cat(sprintf("ANCOVA: %s by %s, covariate: %s%s\n",
                info$dv, factor_str, cov_str, weighted_tag))
    .print_grouped_fits(x, function(fit, label) {
      cat(sprintf("[%s] N = %s\n", label,
                  fmt_int(fit$call_info$n_total)))
      .print_effect_lines(fit$anova_table, digits, covariates = info$covariates)
    })
    print_summary_hint()
    return(invisible(x))
  }

  # N once in the title (it was appended to the last effect line only)
  cat(sprintf("ANCOVA: %s by %s, covariate: %s%s, N = %s\n",
              info$dv, factor_str, cov_str, weighted_tag,
              fmt_int(info$n_total)))
  .print_effect_lines(x$anova_table, digits, covariates = info$covariates)

  print_summary_hint()
  invisible(x)
}


#' Summary method for ANCOVA results
#'
#' @description
#' Creates a summary object that produces detailed output when printed,
#' including the full ANOVA table, parameter estimates, estimated marginal
#' means, and Levene's test.
#'
#' @param object An \code{ancova} result object.
#' @param between_subjects Logical. Show the ANOVA table? (Default: TRUE)
#' @param parameter_estimates Logical. Show parameter estimates? (Default: TRUE)
#' @param marginal_means Logical. Show estimated marginal means? (Default: TRUE)
#' @param levene_test Logical. Show Levene's test? (Default: TRUE)
#' @param digits Number of decimal places for formatting (Default: 3).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.ancova} object.
#'
#' @examples
#' result <- ancova(survey_data, dv = life_satisfaction, between = gender, covariate = age)
#' summary(result)
#' summary(result, marginal_means = FALSE)
#'
#' @seealso \code{\link{ancova}} for the main analysis function.
#' @export
#' @method summary ancova
summary.ancova <- function(object, between_subjects = TRUE,
                            parameter_estimates = TRUE,
                            marginal_means = TRUE,
                            levene_test = TRUE,
                            digits = 3, ...) {
  build_summary_object(
    object     = object,
    show       = list(between_subjects = between_subjects,
                      parameter_estimates = parameter_estimates,
                      marginal_means = marginal_means,
                      levene_test = levene_test),
    digits     = digits,
    class_name = "summary.ancova"
  )
}


#' Print summary of ANCOVA results (detailed output)
#'
#' @description
#' Displays the detailed SPSS-style output for an ANCOVA, with sections
#' controlled by the boolean parameters passed to
#' \code{\link{summary.ancova}}.  Sections include the ANCOVA table with
#' Type III sums of squares, effect sizes, estimated marginal means, and
#' Levene's test for homogeneity of variances.
#'
#' @param x A \code{summary.ancova} object created by
#'   \code{\link{summary.ancova}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- ancova(survey_data, dv = life_satisfaction, between = gender, covariate = age)
#' summary(result)                          # all sections
#' summary(result, marginal_means = FALSE)  # hide marginal means
#'
#' @seealso \code{\link{ancova}} for the main analysis,
#'   \code{\link{summary.ancova}} for summary options.
#' @export
#' @method print summary.ancova
print.summary.ancova <- function(x, ...) {

  digits <- x$digits
  info <- x$call_info
  n_factors <- length(info$factors)

  if (n_factors > 1) {
    design_label <- paste0(n_factors, "-Way ANCOVA")
  } else {
    design_label <- "One-Way ANCOVA"
  }

  # Header
  test_type <- get_standard_title(
    paste0("ANCOVA (", design_label, ")"),
    x$weights,
    "Results"
  )
  print_header(test_type, newline_before = FALSE)

  # Info section
  cat("\n")
  factor_str <- paste(info$factors, collapse = " x ")
  cov_str <- paste(info$covariates, collapse = ", ")
  grouped <- isTRUE(x$is_grouped)
  test_info <- list(
    "Dependent variable" = info$dv,
    "Factor(s)" = factor_str,
    "Covariate(s)" = cov_str,
    "Sum of squares" = "Type III",
    "Weights variable" = info$weight_name,
    "Grouped by" = if (grouped) paste(x$groups, collapse = ", "),
    "N (complete cases)" = if (!grouped) as.character(info$n_total),
    "Missing" = if (!grouped) as.character(info$n_missing)
  )
  print_info_section(test_info)
  cat("\n")

  # Resolve show toggles
  show_between <- if (!is.null(x$show)) isTRUE(x$show$between_subjects) else TRUE
  show_params  <- if (!is.null(x$show)) isTRUE(x$show$parameter_estimates) else TRUE
  show_emm     <- if (!is.null(x$show)) isTRUE(x$show$marginal_means) else TRUE
  show_levene  <- if (!is.null(x$show)) isTRUE(x$show$levene_test) else TRUE

  sections <- function(fit) {
    if (show_between) .print_between_subjects(fit, digits)

    # ---- PARAMETER ESTIMATES ----
    if (show_params) {
      cat("\nParameter Estimates\n")
      pe <- fit$parameter_estimates
      redundant <- if ("redundant" %in% names(pe)) pe$redundant else rep(FALSE, nrow(pe))
      .print_table_utf8(data.frame(
        Parameter = pe$parameter,
        B = ifelse(redundant, "0 (a)", .fmt_coef(pe$b, digits)),
        SE = .fmt_coef(pe$se, digits),
        t = fmt_num(pe$t, digits),
        Sig = fmt_p(pe$p, digits),
        Lower = .fmt_coef(pe$ci_lower, digits),
        Upper = .fmt_coef(pe$ci_upper, digits),
        Eta = fmt_num(pe$partial_eta_sq, digits),
        stringsAsFactors = FALSE
      ), col_labels = c(SE = "Std. Error", Lower = "95% CI Lower",
                        Upper = "95% CI Upper", Eta = "Partial Eta Squared"))
      if (any(redundant)) {
        cat("(a) This parameter is set to zero because it is redundant (SPSS coding: the\n")
        cat("    last category of each factor is the reference).\n")
      }
    }

    # ---- ESTIMATED MARGINAL MEANS ----
    if (show_emm) {
      cat("\nEstimated Marginal Means\n")
      cat("(Evaluated at covariate means)\n")
      emm_table <- function(emm, factors) {
        if (all(is.na(emm$mean))) {
          cat("  not estimable (the design has empty cells)\n")
          return(invisible(NULL))
        }
        tbl <- as.data.frame(lapply(emm[factors], as.character),
                             stringsAsFactors = FALSE, check.names = FALSE)
        tbl$Mean <- fmt_num(emm$mean, digits)
        tbl$`Std. Error` <- fmt_num(emm$se, digits)
        tbl$`95% CI Lower` <- fmt_num(emm$ci_lower, digits)
        tbl$`95% CI Upper` <- fmt_num(emm$ci_upper, digits)
        .print_table_utf8(tbl, left = length(factors))
      }
      # Main effects first (SPSS /EMMEANS=TABLES(factor)), then the cells
      for (f in names(fit$emm_main_effects)) {
        cat(sprintf("\n%s\n", f))
        emm_table(fit$emm_main_effects[[f]], f)
      }
      if (length(info$factors) > 1) {
        cat(sprintf("\n%s\n", paste(info$factors, collapse = " * ")))
      }
      emm_table(fit$estimated_marginal_means, info$factors)
    }

    if (show_levene) .print_levene_line(fit$levene_test, digits)
  }
  if (grouped) {
    .print_grouped_fits(x, function(fit, label) {
      cat(sprintf("N (complete cases): %s, Missing: %s\n\n",
                  fit$call_info$n_total, fit$call_info$n_missing))
      sections(fit)
    }, style = "header")
  } else {
    sections(x)
  }

  # Significance legend
  if (show_between || show_levene) {
    print_significance_legend()
  }

  invisible(x)
}
