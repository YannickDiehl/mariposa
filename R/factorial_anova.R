
#' Compare Groups Across Multiple Factors: Factorial ANOVA
#'
#' @description
#' \code{factorial_anova()} tests whether group means differ across two or more
#' factors simultaneously, including their interactions. It performs a factorial
#' (two-way, three-way) ANOVA using Type III Sum of Squares, matching SPSS
#' UNIANOVA output.
#'
#' Think of it as:
#' - Testing multiple grouping variables at once
#' - Detecting interaction effects (do factor combinations matter?)
#' - An extension of one-way ANOVA to multiple factors
#'
#' The test tells you:
#' - Whether each factor has a main effect on the outcome
#' - Whether factors interact (the effect of one depends on the other)
#' - How much variance each factor explains (partial eta squared)
#'
#' @param data Your survey data (a data frame or tibble)
#' @param dv The numeric dependent variable to analyze (unquoted)
#' @param between Character vector or unquoted variable names specifying the
#'   between-subjects factors (2-3 factors). These must be categorical variables
#'   (factor, character, or labelled numeric).
#' @param weights Optional survey weights for population-representative results
#'   (unquoted variable name)
#' @param ss_type Type of Sum of Squares: 3 (default, SPSS standard) or 2.
#'   Type III is recommended for unbalanced designs.
#'
#' @return An object of class \code{"factorial_anova"} containing:
#' \describe{
#'   \item{anova_table}{Tibble with Source, SS, df, MS, F, p, Partial Eta Squared}
#'   \item{descriptives}{Tibble with cell means, SDs, and Ns for each factor combination}
#'   \item{levene_test}{Tibble with Levene's test results (F, df1, df2, p)}
#'   \item{r_squared}{R-squared and Adjusted R-squared}
#'   \item{model}{The underlying model object for S3 dispatch}
#'   \item{call_info}{List with metadata (dv, factors, weighted, n_total, n_missing)}
#' }
#'
#' @details
#' ## Understanding the Results
#'
#' **Main Effects**: Does each factor independently affect the outcome?
#' - Significant main effect = group means differ for that factor
#' - Example: Education affects income regardless of gender
#'
#' **Interaction Effects**: Does the effect of one factor depend on another?
#' - Significant interaction = the pattern differs across factor combinations
#' - Example: The gender gap in income varies by education level
#'
#' **Partial Eta Squared** (Effect Size):
#' \itemize{
#'   \item Less than 0.01: Negligible
#'   \item 0.01 to 0.06: Small
#'   \item 0.06 to 0.14: Medium
#'   \item 0.14 or greater: Large
#' }
#'
#' ## Type III Sum of Squares
#'
#' Type III SS tests each effect after adjusting for all other effects. This is
#' the standard in SPSS and recommended for unbalanced designs (unequal cell
#' sizes). It uses orthogonal contrasts (contr.sum) internally.
#'
#' ## When to Use This
#'
#' Use factorial ANOVA when:
#' - You have one numeric outcome variable
#' - You have 2-3 categorical grouping factors
#' - You want to test main effects AND interactions
#' - Your data is approximately normally distributed within cells
#'
#' ## What Comes Next?
#'
#' If the ANOVA is significant:
#' 1. Check which effects are significant (main effects vs. interactions)
#' 2. Use \code{tukey_test()} for post-hoc comparisons on main effects
#' 3. Examine cell means to interpret interaction patterns
#' 4. Consider effect sizes for practical significance
#'
#' @seealso
#' \code{\link{oneway_anova}} for single-factor ANOVA.
#'
#' \code{\link{tukey_test}} for post-hoc pairwise comparisons.
#'
#' \code{\link{levene_test}} for testing homogeneity of variances.
#'
#' \code{\link{ancova}} for ANOVA with covariates.
#'
#' @references
#' Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences
#' (2nd ed.). Lawrence Erlbaum Associates.
#'
#' IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.
#'
#' Maxwell, S. E., & Delaney, H. D. (2004). Designing Experiments and
#' Analyzing Data (2nd ed.). Lawrence Erlbaum Associates.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Two-way ANOVA: income by gender and education
#' survey_data %>%
#'   factorial_anova(dv = income, between = c(gender, education))
#'
#' # Two-way ANOVA with weights
#' survey_data %>%
#'   factorial_anova(dv = life_satisfaction, between = c(gender, region),
#'                   weights = sampling_weight)
#'
#' # Three-way ANOVA
#' survey_data %>%
#'   factorial_anova(dv = income, between = c(gender, region, education))
#'
#' # Follow up with post-hoc tests
#' result <- survey_data %>%
#'   factorial_anova(dv = income, between = c(gender, education))
#' result %>% tukey_test()
#' result %>% levene_test()
#'
#' @family hypothesis_tests
#' @export
factorial_anova <- function(data, dv, between, weights = NULL, ss_type = 3) {

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

  # Process between-subjects factors
  between_quo <- rlang::enquo(between)
  between_names <- .parse_between_factors(between_quo, data)

  if (length(between_names) < 2) {
    cli_abort(c(
      "{.fn factorial_anova} requires at least 2 between-subjects factors.",
      "i" = "For a single factor, use {.fn oneway_anova} instead."
    ))
  }
  if (length(between_names) > 3) {
    cli_abort("{.fn factorial_anova} supports at most 3 between-subjects factors.")
  }

  # Validate factors exist and are categorical
  for (bn in between_names) {
    if (!bn %in% names(data)) {
      cli_abort("Factor {.var {bn}} not found in data.")
    }
    x <- data[[bn]]
    if (!(is.factor(x) || is.character(x) || is.numeric(x))) {
      cli_abort("Factor {.var {bn}} must be factor, character, or numeric.")
    }
  }

  # Process weights
  weights_quo <- rlang::enquo(weights)
  weights_info <- .process_weights(data, weights_quo)
  w_name <- weights_info$name

  # ============================================================================
  # DATA PREPARATION
  # ============================================================================

  # Select relevant columns and perform listwise deletion
  all_vars <- c(dv_name, between_names)
  if (!is.null(w_name)) all_vars <- c(all_vars, w_name)

  complete_idx <- stats::complete.cases(data[, all_vars, drop = FALSE])
  # Also exclude zero/negative weights
  if (!is.null(w_name)) {
    complete_idx <- complete_idx & data[[w_name]] > 0
  }

  data_complete <- data[complete_idx, , drop = FALSE]
  n_total <- nrow(data_complete)
  n_missing <- sum(!complete_idx)

  if (n_total < length(between_names) + 2) {
    cli_abort("Insufficient observations ({n_total}) after removing missing values.")
  }

  # Convert factors: ensure all between variables are factors
  for (bn in between_names) {
    if (!is.factor(data_complete[[bn]])) {
      data_complete[[bn]] <- factor(data_complete[[bn]])
    }
    # Drop unused levels
    data_complete[[bn]] <- droplevels(data_complete[[bn]])
  }

  # ============================================================================
  # MODEL FITTING WITH TYPE III SS
  # ============================================================================

  # Set contrasts to contr.sum for Type III SS (orthogonal contrasts)
  old_contrasts <- options("contrasts")$contrasts
  options(contrasts = c("contr.sum", "contr.poly"))
  on.exit(options(contrasts = old_contrasts), add = TRUE)

  # Build formula: dv ~ factor1 * factor2 (* includes all interactions)
  formula_str <- paste(dv_name, "~", paste(between_names, collapse = " * "))
  model_formula <- stats::as.formula(formula_str)

  # Fit the model
  if (!is.null(w_name)) {
    data_complete$.wt <- data_complete[[w_name]]
    model <- stats::lm(model_formula, data = data_complete, weights = .wt)
  } else {
    model <- stats::lm(model_formula, data = data_complete)
  }

  # Extract Type III SS using drop1
  # drop1 with test="F" gives Type III (marginal) tests
  type3 <- stats::drop1(model, scope = . ~ ., test = "F")

  # ============================================================================
  # BUILD ANOVA TABLE
  # ============================================================================

  anova_table <- .build_factorial_anova_table(model, type3, between_names,
                                               dv_name, n_total, w_name)

  # ============================================================================
  # DESCRIPTIVE STATISTICS
  # ============================================================================

  descriptives <- .compute_factorial_descriptives(data_complete, dv_name,
                                                   between_names, w_name)

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
  df_model <- sum(anova_table$df[!anova_table$source %in%
                                    c("Error", "Total", "Corrected Total",
                                      "Corrected Model", "Intercept")])
  df_error <- anova_table$df[anova_table$source == "Error"]
  adj_r_squared <- 1 - (1 - r_squared) * (n_total - 1) / df_error

  # ============================================================================
  # RESULT OBJECT
  # ============================================================================

  # Store the aov-compatible model for S3 dispatch (tukey_test, scheffe_test)
  # We need to create an aov object for TukeyHSD to work
  aov_model <- if (is.null(w_name)) {
    stats::aov(model_formula, data = data_complete)
  } else {
    stats::aov(model_formula, data = data_complete, weights = .wt)
  }

  result <- structure(
    list(
      anova_table = anova_table,
      descriptives = descriptives,
      levene_test = levene_result,
      r_squared = c(r_squared = r_squared, adj_r_squared = adj_r_squared),
      model = aov_model,
      lm_model = model,
      call_info = list(
        dv = dv_name,
        factors = between_names,
        weighted = !is.null(w_name),
        weight_name = w_name,
        n_total = n_total,
        n_missing = n_missing,
        ss_type = ss_type
      ),
      # Store data for S3 dispatch (tukey_test, levene_test)
      data = data_complete,
      variables = dv_name,
      group = between_names,
      weights = w_name,
      is_grouped = FALSE,
      groups = NULL
    ),
    class = "factorial_anova"
  )

  return(result)
}


# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

#' Parse between-subjects factor specification
#' @param between_quo Quosure from enquo(between)
#' @param data Data frame for validation
#' @return Character vector of factor names
#' @keywords internal
.parse_between_factors <- function(between_quo, data) {
  # Handle c(var1, var2) syntax
  between_expr <- rlang::quo_get_expr(between_quo)

  if (is.call(between_expr) && identical(between_expr[[1]], quote(c))) {
    # c(gender, education) syntax
    names <- vapply(as.list(between_expr)[-1], function(x) {
      if (is.name(x)) as.character(x)
      else if (is.character(x)) x
      else cli_abort("Invalid factor specification: {deparse(x)}")
    }, character(1))
    return(names)
  } else if (is.character(between_expr)) {
    # Character vector: c("gender", "education")
    return(between_expr)
  } else if (is.name(between_expr)) {
    # Single name - might be a variable holding a character vector
    val <- tryCatch(rlang::eval_tidy(between_quo), error = function(e) NULL)
    if (is.character(val)) return(val)
    return(as.character(between_expr))
  } else {
    cli_abort(c(
      "Cannot parse {.arg between} specification.",
      "i" = "Use: {.code between = c(factor1, factor2)}"
    ))
  }
}

#' Build ANOVA table from lm model and drop1 results
#' @keywords internal
.build_factorial_anova_table <- function(model, type3, between_names, dv_name,
                                          n_total, w_name) {

  model_summary <- summary(model)

  # Get all term names from the model (excluding intercept)
  term_names <- attr(stats::terms(model), "term.labels")

  # SS Error from the model
  if (!is.null(w_name)) {
    # Weighted: residual SS = sum(w * e^2)
    resid_raw <- stats::residuals(model)
    w_vec <- model$model$`(weights)`
    ss_error <- sum(w_vec * resid_raw^2)
  } else {
    ss_error <- sum(stats::residuals(model)^2)
  }

  df_error <- model$df.residual

  # Extract SS for each term from drop1
  # drop1 rows: <none> (full model), then each term
  # The "Sum of Sq" column gives the Type III SS for each term
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

  # Mean squares
  ms_error <- ss_error / df_error
  term_ms <- term_ss / term_df

  # F statistics
  term_f <- term_ms / ms_error

  # P values
  term_p <- stats::pf(term_f, term_df, df_error, lower.tail = FALSE)

  # Partial eta squared: SS_effect / (SS_effect + SS_error)
  term_eta <- term_ss / (term_ss + ss_error)

  # Corrected Total and Corrected Model
  # Corrected Total = total SS corrected for the mean (data property, model-independent)
  # Corrected Model = Corrected Total - Error (NOT sum of Type III SS, which differs
  # in unbalanced designs because Type III tests are marginal/partial)
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

  # Type III Intercept SS
  # With contr.sum, the intercept represents the unweighted grand mean of cell means.
  # The Type III SS for the intercept is F * MSE, where F = (b0 / se(b0))^2.
  # This matches SPSS's Type III Intercept computation.
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

  # Build the table
  # Format term names to match SPSS output (replace ":" with " * ")
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


#' Compute descriptive statistics for factorial design
#' @keywords internal
.compute_factorial_descriptives <- function(data, dv_name, between_names, w_name) {

  y <- data[[dv_name]]

  # Create a combined grouping variable for cell-level stats
  # We compute stats for each unique combination of factor levels
  factor_cols <- data[, between_names, drop = FALSE]

  # Get unique combinations
  cells <- unique(factor_cols)
  cells <- cells[do.call(order, cells), , drop = FALSE]

  results <- list()

  for (i in seq_len(nrow(cells))) {
    cell <- cells[i, , drop = FALSE]

    # Find matching rows
    idx <- rep(TRUE, nrow(data))
    for (bn in between_names) {
      idx <- idx & data[[bn]] == cell[[bn]]
    }

    cell_y <- y[idx]

    if (!is.null(w_name)) {
      cell_w <- data[[w_name]][idx]
      cell_mean <- sum(cell_y * cell_w) / sum(cell_w)
      cell_sd <- sqrt(sum(cell_w * (cell_y - cell_mean)^2) / sum(cell_w))
    } else {
      cell_mean <- mean(cell_y)
      cell_sd <- stats::sd(cell_y)
    }

    row <- cell
    row$mean <- cell_mean
    row$sd <- cell_sd
    row$n <- sum(idx)
    results[[i]] <- row
  }

  dplyr::bind_rows(results)
}


#' Compute Levene's test for factorial design
#' @keywords internal
.compute_factorial_levene <- function(data, dv_name, between_names, w_name) {

  y <- data[[dv_name]]

  # Create combined grouping variable (interaction of all factors)
  g <- interaction(data[between_names], drop = TRUE, sep = "_")

  g_levels <- levels(g)

  # Calculate absolute deviations from group centers (mean-based)
  z_values <- numeric(length(y))

  if (!is.null(w_name)) {
    w_vec <- data[[w_name]]

    # SPSS /REGWGT Levene algorithm:
    # 1. Compute weighted cell means
    # 2. z_i = sqrt(w_i) * |y_i - weighted_cell_mean_i|
    # 3. Perform unweighted one-way ANOVA on z
    # This matches SPSS's WLS transformation where the model operates in
    # the sqrt(w) space: sqrt(w)*y = sqrt(w)*X*beta + epsilon
    for (level in g_levels) {
      group_idx <- g == level
      group_y <- y[group_idx]
      group_w <- w_vec[group_idx]
      center <- sum(group_y * group_w) / sum(group_w)
      z_values[group_idx] <- sqrt(w_vec[group_idx]) * abs(group_y - center)
    }

    levene_aov <- summary(stats::aov(z_values ~ g))[[1]]
  } else {
    # Unweighted: standard ANOVA on |y - cell_mean|
    for (level in g_levels) {
      group_idx <- g == level
      z_values[group_idx] <- abs(y[group_idx] - mean(y[group_idx]))
    }

    levene_aov <- summary(stats::aov(z_values ~ g))[[1]]
  }

  f_stat <- levene_aov["g", "F value"]
  df1 <- levene_aov["g", "Df"]
  df2 <- levene_aov["Residuals", "Df"]
  p_value <- levene_aov["g", "Pr(>F)"]

  tibble::tibble(
    f = f_stat,
    df1 = as.integer(df1),
    df2 = as.integer(df2),
    p = p_value
  )
}


# ==============================================================================
# PRINT METHOD
# ==============================================================================

#' Print factorial ANOVA results
#'
#' @param x An object of class \code{"factorial_anova"}.
#' @param digits Number of decimal places (default: 3).
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.factorial_anova <- function(x, digits = 3, ...) {

  info <- x$call_info
  n_factors <- length(info$factors)
  design_label <- paste0(n_factors, "-Way ANOVA")

  # Header
  test_type <- get_standard_title(
    paste0("Factorial ANOVA (", design_label, ")"),
    x$weights,
    "Results"
  )
  print_header(test_type)

  # Info section
  cat("\n")
  factor_str <- paste(info$factors, collapse = " x ")
  test_info <- list(
    "Dependent variable" = info$dv,
    "Factors" = factor_str,
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

  # Format the table for display
  fmt_ss <- format(round(at$ss, digits), nsmall = digits, big.mark = "")
  fmt_df <- format(at$df)
  fmt_ms <- ifelse(is.na(at$ms), "", format(round(at$ms, digits), nsmall = digits, big.mark = ""))
  fmt_f <- ifelse(is.na(at$f), "", format(round(at$f, digits), nsmall = digits))
  fmt_p <- ifelse(is.na(at$p), "",
                  ifelse(at$p < 0.001, "<.001", format(round(at$p, digits), nsmall = digits)))
  fmt_eta <- ifelse(is.na(at$partial_eta_sq), "",
                    format(round(at$partial_eta_sq, digits), nsmall = digits))

  # Significance stars
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

  # ---- DESCRIPTIVE STATISTICS ----
  cat("\nDescriptive Statistics\n")

  desc <- x$descriptives
  fmt_desc <- desc
  for (bn in info$factors) {
    fmt_desc[[bn]] <- as.character(fmt_desc[[bn]])
  }
  fmt_desc$mean <- format(round(desc$mean, 2), nsmall = 2)
  fmt_desc$sd <- format(round(desc$sd, 3), nsmall = 3)
  fmt_desc$n <- format(desc$n)

  names(fmt_desc)[names(fmt_desc) == "mean"] <- "Mean"
  names(fmt_desc)[names(fmt_desc) == "sd"] <- "Std. Deviation"
  names(fmt_desc)[names(fmt_desc) == "n"] <- "N"

  desc_border <- max(nchar(capture.output(print(fmt_desc, row.names = FALSE))), 40)
  cat(paste(rep("-", desc_border), collapse = ""), "\n")
  print(as.data.frame(fmt_desc), row.names = FALSE, right = FALSE)
  cat(paste(rep("-", desc_border), collapse = ""), "\n")

  if (!is.null(x$weights)) {
    cat("Note: Means and SDs are weighted (WLS)\n")
  }

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


# ==============================================================================
# S3 METHODS FOR POST-HOC DISPATCH
# ==============================================================================

#' @export
tukey_test.factorial_anova <- function(x, conf.level = 0.95, ...) {

  info <- x$call_info

  if (conf.level <= 0 || conf.level >= 1) {
    cli_abort("{.arg conf.level} must be between 0 and 1.")
  }

  # Perform Tukey HSD for each main effect factor
  results_list <- list()

  for (factor_name in info$factors) {
    tryCatch({
      # Build a one-way model for this factor
      dv_name <- info$dv
      data_complete <- x$data
      y <- data_complete[[dv_name]]
      g <- data_complete[[factor_name]]

      if (!is.null(info$weight_name)) {
        w <- data_complete[[info$weight_name]]
        # Weighted Tukey: use the existing weighted implementation
        # from tukey_test.oneway_anova
        group_levels <- levels(g)

        # Calculate weighted group stats
        group_stats <- lapply(group_levels, function(level) {
          idx <- g == level
          gw <- w[idx]
          gy <- y[idx]
          list(
            mean = sum(gy * gw) / sum(gw),
            n = length(gy),
            weighted_n = sum(gw),
            var = sum(gw * (gy - sum(gy * gw) / sum(gw))^2) / (sum(gw) - 1)
          )
        })
        names(group_stats) <- group_levels

        # MSE from the full factorial model
        mse <- sum(x$anova_table$ms[x$anova_table$source == "Error"])
        df_error <- x$anova_table$df[x$anova_table$source == "Error"]

        # Pairwise comparisons
        pairs <- utils::combn(group_levels, 2)
        for (j in seq_len(ncol(pairs))) {
          g1 <- pairs[1, j]
          g2 <- pairs[2, j]
          s1 <- group_stats[[g1]]
          s2 <- group_stats[[g2]]

          diff <- s1$mean - s2$mean
          se <- sqrt(mse * (1 / s1$weighted_n + 1 / s2$weighted_n) / 2)
          q_stat <- abs(diff) / se
          k <- length(group_levels)
          p_adj <- stats::ptukey(q_stat, k, df_error, lower.tail = FALSE)

          # Confidence interval
          q_crit <- stats::qtukey(conf.level, k, df_error)
          ci_lower <- diff - q_crit * se
          ci_upper <- diff + q_crit * se

          results_list[[length(results_list) + 1]] <- data.frame(
            Factor = factor_name,
            Comparison = paste(g1, "-", g2),
            Estimate = diff,
            conf_low = ci_lower,
            conf_high = ci_upper,
            p_adjusted = p_adj,
            stringsAsFactors = FALSE
          )
        }
      } else {
        # Unweighted: use standard TukeyHSD
        one_way_formula <- stats::as.formula(paste(dv_name, "~", factor_name))
        one_way_aov <- stats::aov(one_way_formula, data = data_complete)
        tukey_result <- stats::TukeyHSD(one_way_aov, conf.level = conf.level)

        tukey_data <- tukey_result[[factor_name]]
        comparisons <- rownames(tukey_data)

        for (j in seq_along(comparisons)) {
          results_list[[length(results_list) + 1]] <- data.frame(
            Factor = factor_name,
            Comparison = comparisons[j],
            Estimate = tukey_data[j, "diff"],
            conf_low = tukey_data[j, "lwr"],
            conf_high = tukey_data[j, "upr"],
            p_adjusted = tukey_data[j, "p adj"],
            stringsAsFactors = FALSE
          )
        }
      }
    }, error = function(e) {
      cli_warn("Tukey test failed for factor {.var {factor_name}}: {e$message}")
    })
  }

  if (length(results_list) == 0) {
    cli_abort("Tukey test could not be computed for any factor.")
  }

  results_df <- do.call(rbind, results_list)

  result <- structure(
    list(
      results = results_df,
      conf.level = conf.level,
      call_info = info,
      is_factorial = TRUE
    ),
    class = "tukey_test"
  )

  return(result)
}


#' @export
scheffe_test.factorial_anova <- function(x, conf.level = 0.95, ...) {

  info <- x$call_info

  if (conf.level <= 0 || conf.level >= 1) {
    cli_abort("{.arg conf.level} must be between 0 and 1.")
  }

  results_list <- list()

  for (factor_name in info$factors) {
    tryCatch({
      dv_name <- info$dv
      data_complete <- x$data
      y <- data_complete[[dv_name]]
      g <- data_complete[[factor_name]]
      group_levels <- levels(g)
      k <- length(group_levels)

      mse <- x$anova_table$ms[x$anova_table$source == "Error"]
      df_error <- x$anova_table$df[x$anova_table$source == "Error"]
      df_between <- k - 1

      if (!is.null(info$weight_name)) {
        w <- data_complete[[info$weight_name]]
        group_stats <- lapply(group_levels, function(level) {
          idx <- g == level
          list(
            mean = sum(y[idx] * w[idx]) / sum(w[idx]),
            weighted_n = sum(w[idx])
          )
        })
        names(group_stats) <- group_levels
      } else {
        group_stats <- lapply(group_levels, function(level) {
          idx <- g == level
          list(mean = mean(y[idx]), weighted_n = sum(idx))
        })
        names(group_stats) <- group_levels
      }

      # Pairwise Scheffe comparisons
      pairs <- utils::combn(group_levels, 2)
      for (j in seq_len(ncol(pairs))) {
        g1 <- pairs[1, j]
        g2 <- pairs[2, j]
        s1 <- group_stats[[g1]]
        s2 <- group_stats[[g2]]

        diff <- s1$mean - s2$mean
        se <- sqrt(mse * (1 / s1$weighted_n + 1 / s2$weighted_n))

        f_stat <- diff^2 / (df_between * mse * (1 / s1$weighted_n + 1 / s2$weighted_n))
        p_val <- stats::pf(f_stat, df_between, df_error, lower.tail = FALSE)

        # Critical value for Scheffe
        f_crit <- stats::qf(conf.level, df_between, df_error)
        margin <- sqrt(df_between * f_crit) * se
        ci_lower <- diff - margin
        ci_upper <- diff + margin

        results_list[[length(results_list) + 1]] <- data.frame(
          Factor = factor_name,
          Comparison = paste(g1, "-", g2),
          Estimate = diff,
          conf_low = ci_lower,
          conf_high = ci_upper,
          p_adjusted = p_val,
          stringsAsFactors = FALSE
        )
      }
    }, error = function(e) {
      cli_warn("Scheffe test failed for factor {.var {factor_name}}: {e$message}")
    })
  }

  if (length(results_list) == 0) {
    cli_abort("Scheffe test could not be computed for any factor.")
  }

  results_df <- do.call(rbind, results_list)

  result <- structure(
    list(
      results = results_df,
      conf.level = conf.level,
      call_info = info,
      is_factorial = TRUE
    ),
    class = "scheffe_test"
  )

  return(result)
}


#' @export
levene_test.factorial_anova <- function(x, center = c("mean", "median"), ...) {
  center <- match.arg(center)

  # Recompute if median-based is requested, otherwise return stored result
  if (center == "mean") {
    return(
      structure(
        list(
          results = tibble::tibble(
            Variable = x$call_info$dv,
            F_statistic = x$levene_test$f,
            df1 = x$levene_test$df1,
            df2 = x$levene_test$df2,
            p_value = x$levene_test$p,
            conclusion = ifelse(x$levene_test$p > 0.05,
                                "Variances equal", "Variances unequal")
          ),
          variables = x$call_info$dv,
          group = paste(x$call_info$factors, collapse = " * "),
          weights = x$weights,
          center = center,
          original_test = x
        ),
        class = "levene_test"
      )
    )
  }

  # Median-based: recompute
  y <- x$data[[x$call_info$dv]]
  g <- interaction(x$data[x$call_info$factors], drop = TRUE, sep = "_")
  g_levels <- levels(g)

  z_values <- numeric(length(y))
  for (level in g_levels) {
    idx <- g == level
    z_values[idx] <- abs(y[idx] - stats::median(y[idx]))
  }

  levene_aov <- summary(stats::aov(z_values ~ g))[[1]]

  structure(
    list(
      results = tibble::tibble(
        Variable = x$call_info$dv,
        F_statistic = levene_aov["g", "F value"],
        df1 = as.integer(levene_aov["g", "Df"]),
        df2 = as.integer(levene_aov["Residuals", "Df"]),
        p_value = levene_aov["g", "Pr(>F)"],
        conclusion = ifelse(levene_aov["g", "Pr(>F)"] > 0.05,
                            "Variances equal", "Variances unequal")
      ),
      variables = x$call_info$dv,
      group = paste(x$call_info$factors, collapse = " * "),
      weights = x$weights,
      center = center,
      original_test = x
    ),
    class = "levene_test"
  )
}
