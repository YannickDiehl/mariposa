
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
#' @param weights Optional survey weights for population-representative results
#'   (unquoted variable name)
#' @param ss_type Type of Sum of Squares: 3 (default, SPSS standard) or 2.
#'   Type III is recommended for unbalanced designs.
#'
#' @return An object of class \code{"ancova"} containing:
#' \describe{
#'   \item{anova_table}{Tibble with Source, SS, df, MS, F, p, Partial Eta Squared}
#'   \item{parameter_estimates}{Tibble with regression coefficients (B, SE, t, p)}
#'   \item{descriptives}{Tibble with unadjusted cell means, SDs, and Ns}
#'   \item{estimated_marginal_means}{Tibble with adjusted means (covariates at grand mean)}
#'   \item{levene_test}{Tibble with Levene's test results}
#'   \item{r_squared}{R-squared and Adjusted R-squared}
#'   \item{model}{The underlying lm model object}
#'   \item{call_info}{List with metadata (dv, factors, covariates, weighted, etc.)}
#' }
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
#' @family hypothesis_tests
#' @export
ancova <- function(data, dv, between, covariate, weights = NULL, ss_type = 3) {

  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================

  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }

  if (!ss_type %in% c(2, 3)) {
    cli_abort("{.arg ss_type} must be 2 or 3.")
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
  w_name <- weights_info$name

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
    cli_abort("Insufficient observations ({n_total}) after removing missing values.")
  }

  # Convert factors
  for (bn in between_names) {
    if (!is.factor(data_complete[[bn]])) {
      data_complete[[bn]] <- factor(data_complete[[bn]])
    }
    data_complete[[bn]] <- droplevels(data_complete[[bn]])
  }

  # ============================================================================
  # MODEL FITTING WITH TYPE III SS
  # ============================================================================

  old_contrasts <- options("contrasts")$contrasts
  options(contrasts = c("contr.sum", "contr.poly"))
  on.exit(options(contrasts = old_contrasts), add = TRUE)

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

  param_est <- .compute_parameter_estimates(model, w_name)

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

  # ============================================================================
  # LEVENE'S TEST
  # ============================================================================

  levene_result <- .compute_factorial_levene(data_complete, dv_name,
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

#' Build ANCOVA table from lm model and drop1 results
#' @keywords internal
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
  term_ms <- term_ss / term_df
  term_f <- term_ms / ms_error
  term_p <- stats::pf(term_f, term_df, df_error, lower.tail = FALSE)
  term_eta <- term_ss / (term_ss + ss_error)

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
  df_corrected_model <- sum(term_df)
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
    partial_eta_sq = eta_vals
  )
}


#' Compute parameter estimates from the model
#' @keywords internal
.compute_parameter_estimates <- function(model, w_name) {

  coefs <- summary(model)$coefficients
  param_names <- rownames(coefs)

  # Extract values
  b_vals <- coefs[, "Estimate"]
  se_vals <- coefs[, "Std. Error"]
  t_vals <- coefs[, "t value"]
  p_vals <- coefs[, "Pr(>|t|)"]

  # Confidence intervals
  ci <- stats::confint(model)

  # Partial eta squared for each parameter
  df_error <- model$df.residual
  eta_vals <- t_vals^2 / (t_vals^2 + df_error)

  tibble::tibble(
    parameter = param_names,
    b = b_vals,
    se = se_vals,
    t = t_vals,
    p = p_vals,
    ci_lower = ci[, 1],
    ci_upper = ci[, 2],
    partial_eta_sq = eta_vals
  )
}


#' Compute estimated marginal means (adjusted for covariates at their grand mean)
#' @keywords internal
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
# PRINT METHOD
# ==============================================================================

#' Print ANCOVA results
#'
#' @param x An object of class \code{"ancova"}.
#' @param digits Number of decimal places (default: 3).
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.ancova <- function(x, digits = 3, ...) {

  info <- x$call_info
  n_factors <- length(info$factors)
  n_covariates <- length(info$covariates)

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
  print_header(test_type)

  # Info section
  cat("\n")
  factor_str <- paste(info$factors, collapse = " x ")
  cov_str <- paste(info$covariates, collapse = ", ")
  test_info <- list(
    "Dependent variable" = info$dv,
    "Factor(s)" = factor_str,
    "Covariate(s)" = cov_str,
    "Type III Sum of Squares" = paste("Type", info$ss_type),
    "Weights variable" = info$weight_name,
    "N (complete cases)" = as.character(info$n_total),
    "Missing" = as.character(info$n_missing)
  )
  print_info_section(test_info)
  cat("\n")

  # ---- ANOVA TABLE ----
  cat("Tests of Between-Subjects Effects\n")
  at <- x$anova_table

  fmt_ss <- format(round(at$ss, digits), nsmall = digits, big.mark = "")
  fmt_df <- format(at$df)
  fmt_ms <- ifelse(is.na(at$ms), "", format(round(at$ms, digits), nsmall = digits, big.mark = ""))
  fmt_f <- ifelse(is.na(at$f), "", format(round(at$f, digits), nsmall = digits))
  fmt_p <- ifelse(is.na(at$p), "",
                  ifelse(at$p < 0.001, "<.001", format(round(at$p, digits), nsmall = digits)))
  fmt_eta <- ifelse(is.na(at$partial_eta_sq), "",
                    format(round(at$partial_eta_sq, digits), nsmall = digits))

  fmt_sig <- vapply(at$p, function(pv) {
    if (is.na(pv)) return("")
    as.character(add_significance_stars(pv))
  }, character(1))

  display_df <- data.frame(
    Source = at$source,
    `Type III SS` = fmt_ss,
    df = fmt_df,
    `Mean Square` = fmt_ms,
    F = fmt_f,
    Sig. = fmt_p,
    `Partial Eta Sq` = fmt_eta,
    ` ` = fmt_sig,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  border_width <- max(nchar(capture.output(print(display_df, row.names = FALSE))),
                      40)
  border <- paste(rep("-", border_width), collapse = "")
  cat(border, "\n")
  print(display_df, row.names = FALSE, right = FALSE)
  cat(border, "\n")

  # R-Squared footnote
  cat(sprintf("R Squared = %s (Adjusted R Squared = %s)\n",
              format(round(x$r_squared["r_squared"], digits), nsmall = digits),
              format(round(x$r_squared["adj_r_squared"], digits), nsmall = digits)))

  # ---- PARAMETER ESTIMATES ----
  cat("\nParameter Estimates\n")
  pe <- x$parameter_estimates

  fmt_pe <- data.frame(
    Parameter = pe$parameter,
    B = format(round(pe$b, digits), nsmall = digits),
    `Std. Error` = format(round(pe$se, digits), nsmall = digits),
    t = format(round(pe$t, digits), nsmall = digits),
    Sig. = ifelse(pe$p < 0.001, "<.001", format(round(pe$p, digits), nsmall = digits)),
    `Lower Bound` = format(round(pe$ci_lower, digits), nsmall = digits),
    `Upper Bound` = format(round(pe$ci_upper, digits), nsmall = digits),
    `Partial Eta Sq` = format(round(pe$partial_eta_sq, digits), nsmall = digits),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  pe_border <- max(nchar(capture.output(print(fmt_pe, row.names = FALSE))), 40)
  cat(paste(rep("-", pe_border), collapse = ""), "\n")
  print(fmt_pe, row.names = FALSE, right = FALSE)
  cat(paste(rep("-", pe_border), collapse = ""), "\n")

  # ---- ESTIMATED MARGINAL MEANS ----
  cat("\nEstimated Marginal Means\n")
  cat("(Evaluated at covariate means)\n")

  emm <- x$estimated_marginal_means
  fmt_emm <- emm
  for (bn in info$factors) {
    fmt_emm[[bn]] <- as.character(fmt_emm[[bn]])
  }
  fmt_emm$mean <- format(round(emm$mean, digits), nsmall = digits)
  fmt_emm$se <- format(round(emm$se, digits), nsmall = digits)
  fmt_emm$ci_lower <- format(round(emm$ci_lower, digits), nsmall = digits)
  fmt_emm$ci_upper <- format(round(emm$ci_upper, digits), nsmall = digits)
  names(fmt_emm)[names(fmt_emm) == "mean"] <- "Mean"
  names(fmt_emm)[names(fmt_emm) == "se"] <- "Std. Error"
  names(fmt_emm)[names(fmt_emm) == "ci_lower"] <- "Lower Bound"
  names(fmt_emm)[names(fmt_emm) == "ci_upper"] <- "Upper Bound"

  emm_border <- max(nchar(capture.output(print(fmt_emm, row.names = FALSE))), 40)
  cat(paste(rep("-", emm_border), collapse = ""), "\n")
  print(as.data.frame(fmt_emm), row.names = FALSE, right = FALSE)
  cat(paste(rep("-", emm_border), collapse = ""), "\n")

  # ---- LEVENE'S TEST ----
  cat("\nLevene's Test of Equality of Error Variances\n")
  lev <- x$levene_test
  cat(sprintf("  F(%d, %d) = %s, p = %s\n",
              lev$df1, lev$df2,
              format(round(lev$f, digits), nsmall = digits),
              ifelse(lev$p < 0.001, "<.001",
                     format(round(lev$p, digits), nsmall = digits))))

  # Significance legend
  print_significance_legend()

  invisible(x)
}
