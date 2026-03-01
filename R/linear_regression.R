#' Run a Linear Regression
#'
#' @description
#' \code{linear_regression()} performs bivariate or multiple linear regression
#' with SPSS-compatible output. Wraps \code{stats::lm()} and adds standardized
#' coefficients (Beta), a formatted ANOVA table, and a model summary matching
#' SPSS REGRESSION output.
#'
#' Supports two interface styles:
#' \itemize{
#'   \item \strong{Formula interface:} \code{linear_regression(data, life_satisfaction ~ age + education)}
#'   \item \strong{SPSS-style:} \code{linear_regression(data, dependent = life_satisfaction, predictors = c(age, education))}
#' }
#'
#' @param data Your survey data (a data frame or tibble). If grouped
#'   (via \code{dplyr::group_by()}), separate regressions are run for each group.
#' @param formula A formula specifying the model (e.g., \code{y ~ x1 + x2}).
#'   If provided, \code{dependent} and \code{predictors} are ignored.
#' @param dependent The dependent variable (unquoted). Used with \code{predictors}
#'   when no formula is given.
#' @param predictors Predictor variable(s) (unquoted, supports tidyselect).
#'   Used with \code{dependent} when no formula is given.
#' @param weights Optional survey weights (unquoted variable name). When
#'   specified, weighted least squares (WLS) is used, matching SPSS WEIGHT BY.
#' @param standardized Logical. If \code{TRUE} (default), standardized
#'   coefficients (Beta) are calculated and included in the output.
#' @param conf.level Confidence level for coefficient intervals (default 0.95).
#'
#' @return An object of class \code{"linear_regression"} containing:
#' \describe{
#'   \item{coefficients}{Tibble with B, Std.Error, Beta, t, p, CI_lower, CI_upper}
#'   \item{model_summary}{List with R, R_squared, adj_R_squared, std_error}
#'   \item{anova}{Tibble with Sum of Squares, df, Mean Square, F, Sig.}
#'   \item{descriptives}{Tibble with Mean, Std.Deviation, N for all variables}
#'   \item{model}{The underlying \code{lm} object}
#'   \item{formula}{The formula used}
#'   \item{n}{Sample size (listwise complete cases)}
#'   \item{dependent}{Name of the dependent variable}
#'   \item{predictor_names}{Names of predictor variables}
#'   \item{weighted}{Logical indicating whether weights were used}
#'   \item{weight_name}{Name of the weight variable (or NULL)}
#' }
#'
#' @details
#' ## Understanding the Results
#'
#' The output includes four sections matching SPSS REGRESSION output:
#' \itemize{
#'   \item \strong{Model Summary}: R, R-squared, Adjusted R-squared, and
#'     Standard Error of the Estimate. R-squared tells you how much variance
#'     in the dependent variable is explained by the predictors.
#'   \item \strong{ANOVA}: Tests whether the overall model is significant.
#'     A significant F-test means at least one predictor matters.
#'   \item \strong{Coefficients}: B (unstandardized), Beta (standardized),
#'     t-value, p-value, and confidence intervals for each predictor.
#'   \item \strong{Descriptives}: Mean, SD, and N for all variables in the model.
#' }
#'
#' Interpreting coefficients:
#' \itemize{
#'   \item \strong{B (unstandardized)}: For each 1-unit increase in the predictor,
#'     the dependent variable changes by B units
#'   \item \strong{Beta (standardized)}: Allows comparison across predictors with
#'     different scales. Larger absolute Beta = stronger effect
#'   \item \strong{p-value}: Values below 0.05 indicate statistically significant
#'     predictors
#' }
#'
#' ## When to Use This
#'
#' Use \code{linear_regression()} when:
#' \itemize{
#'   \item Your dependent variable is continuous (e.g., income, satisfaction score)
#'   \item You want to predict an outcome from one or more predictors
#'   \item You need standardized coefficients to compare predictor importance
#' }
#'
#' For binary outcomes (yes/no, 0/1), use \code{\link{logistic_regression}} instead.
#'
#' ## Technical Details
#'
#' \strong{Missing Data}: Listwise deletion is used (matching SPSS REGRESSION
#' /MISSING LISTWISE). Cases with any missing values on the dependent variable,
#' predictors, or weights are excluded.
#'
#' \strong{Weights}: When weights are specified, they are treated as frequency
#' weights (matching SPSS WEIGHT BY behavior). The model is fitted using weighted
#' least squares via \code{lm(weights = ...)}.
#'
#' \strong{Standardized Coefficients}: Beta = B * (SD_x / SD_y). This matches
#' the SPSS standardized coefficient output. Not available for the intercept.
#'
#' \strong{Grouped Analysis}: When \code{data} is grouped via
#' \code{dplyr::group_by()}, a separate regression is run for each group
#' (matching SPSS SPLIT FILE BY).
#'
#' @examples
#' library(dplyr)
#' data(survey_data)
#'
#' # Bivariate regression
#' linear_regression(survey_data, life_satisfaction ~ age)
#'
#' # Multiple regression
#' linear_regression(survey_data, income ~ age + education + life_satisfaction)
#'
#' # SPSS-style interface
#' linear_regression(survey_data,
#'                   dependent = life_satisfaction,
#'                   predictors = c(trust_government, trust_media, trust_science))
#'
#' # Weighted regression
#' linear_regression(survey_data, life_satisfaction ~ age, weights = sampling_weight)
#'
#' # Grouped by region
#' survey_data |>
#'   dplyr::group_by(region) |>
#'   linear_regression(life_satisfaction ~ age)
#'
#' @seealso
#' \code{\link{logistic_regression}} for binary outcome variables.
#'
#' \code{\link{describe}} for checking variable distributions before regression.
#'
#' \code{\link{pearson_cor}} for checking bivariate correlations.
#'
#' @family regression
#' @export
linear_regression <- function(data, formula = NULL,
                              dependent = NULL, predictors = NULL,
                              weights = NULL,
                              standardized = TRUE,
                              conf.level = 0.95) {

  # ============================================================================
  # INPUT VALIDATION & FORMULA CONSTRUCTION
  # ============================================================================

  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame or tibble.")
  }

  # Process weights
  weights_quo <- rlang::enquo(weights)
  has_weights <- !rlang::quo_is_null(weights_quo)
  weight_name <- NULL
  weights_vec <- NULL

  if (has_weights) {
    weight_name <- rlang::as_name(weights_quo)
    if (!weight_name %in% names(data)) {
      cli_abort("Weight variable {.var {weight_name}} not found in data.")
    }
    weights_vec <- data[[weight_name]]
  }

  # Build formula
  if (!is.null(formula)) {
    # Formula interface
    if (!inherits(formula, "formula")) {
      cli_abort("{.arg formula} must be a formula object (e.g., {.code y ~ x1 + x2}).")
    }
    model_formula <- formula
    dep_name <- as.character(formula[[2]])
    pred_names <- all.vars(formula[[3]])
  } else {
    # SPSS-style interface
    dep_quo <- rlang::enquo(dependent)
    pred_quo <- rlang::enquo(predictors)

    if (rlang::quo_is_null(dep_quo)) {
      cli_abort("Either {.arg formula} or {.arg dependent} must be specified.")
    }

    dep_name <- rlang::as_name(dep_quo)

    # Process predictors using tidyselect
    pred_pos <- tidyselect::eval_select(pred_quo, data)
    pred_names <- names(pred_pos)

    if (length(pred_names) == 0) {
      cli_abort("At least one predictor variable must be specified.")
    }

    model_formula <- stats::as.formula(
      paste(dep_name, "~", paste(pred_names, collapse = " + "))
    )
  }

  # Validate variables exist
  all_vars <- c(dep_name, pred_names)
  missing_vars <- setdiff(all_vars, names(data))
  if (length(missing_vars) > 0) {
    cli_abort("Variable(s) not found in data: {paste(missing_vars, collapse = ', ')}.")
  }

  # ============================================================================
  # GROUPED ANALYSIS
  # ============================================================================

  is_grouped <- inherits(data, "grouped_df")

  if (is_grouped) {
    group_vars <- dplyr::group_vars(data)
    group_split <- dplyr::group_split(data)
    group_keys <- dplyr::group_keys(data)

    group_results <- lapply(seq_along(group_split), function(i) {
      grp_data <- group_split[[i]]
      grp_weights <- if (has_weights) grp_data[[weight_name]] else NULL

      result <- .lm_core(grp_data, model_formula, dep_name, pred_names,
                         grp_weights, standardized, conf.level)
      gv <- as.list(group_keys[i, , drop = FALSE])
      gv <- lapply(gv, function(v) if (is.factor(v)) as.character(v) else v)
      result$group_values <- gv
      result
    })

    structure(
      list(
        groups = group_results,
        formula = model_formula,
        dependent = dep_name,
        predictor_names = pred_names,
        weighted = has_weights,
        weight_name = weight_name,
        is_grouped = TRUE,
        group_vars = group_vars,
        standardized = standardized,
        conf.level = conf.level
      ),
      class = "linear_regression"
    )
  } else {
    result <- .lm_core(data, model_formula, dep_name, pred_names,
                       weights_vec, standardized, conf.level)
    result$formula <- model_formula
    result$dependent <- dep_name
    result$predictor_names <- pred_names
    result$weighted <- has_weights
    result$weight_name <- weight_name
    result$is_grouped <- FALSE
    result$standardized <- standardized
    result$conf.level <- conf.level
    class(result) <- "linear_regression"
    result
  }
}


# ============================================================================
# CORE COMPUTATION
# ============================================================================

#' Core linear regression computation
#' @keywords internal
.lm_core <- function(data, formula, dep_name, pred_names, weights_vec,
                     standardized, conf.level) {

  all_vars <- c(dep_name, pred_names)

  # Listwise deletion (SPSS MISSING LISTWISE)
  complete <- stats::complete.cases(data[, all_vars, drop = FALSE])
  if (!is.null(weights_vec)) {
    complete <- complete & !is.na(weights_vec)
    weights_vec <- weights_vec[complete]
  }
  data_complete <- data[complete, , drop = FALSE]
  n <- nrow(data_complete)

  if (n < length(pred_names) + 2) {
    cli_abort("Insufficient observations for the number of predictors.")
  }

  # Convert factors to numeric (matching SPSS behavior: ordered factor -> integer codes)
  for (v in all_vars) {
    if (is.factor(data_complete[[v]])) {
      data_complete[[v]] <- as.numeric(data_complete[[v]])
    }
  }

  # ============================================================================
  # FIT MODEL
  # ============================================================================

  if (!is.null(weights_vec)) {
    # Add weights to data frame so lm() can find them
    data_complete$.wt <- weights_vec
    model <- stats::lm(formula, data = data_complete, weights = .wt)
  } else {
    model <- stats::lm(formula, data = data_complete)
  }

  model_summary <- summary(model)

  # ============================================================================
  # DESCRIPTIVE STATISTICS (matching SPSS output)
  # ============================================================================

  descriptives <- .lm_descriptives(data_complete, all_vars, weights_vec)

  # ============================================================================
  # SPSS-COMPATIBLE WEIGHTED STATISTICS
  # ============================================================================
  # SPSS WEIGHT BY treats weights as frequency weights:
  # - N = sum(weights) (not nrow)
  # - df uses weighted N
  # - sigma, Std.Error, t, F are all based on weighted df
  # R's lm(weights=) uses actual N for df, so we must adjust.

  k <- length(pred_names)  # number of predictors

  if (!is.null(weights_vec)) {
    n_effective <- round(sum(weights_vec))

    # Weighted residual SS: sum(w * e^2) -- already computed by lm(weights=)
    residuals_raw <- stats::residuals(model)
    ss_residual <- sum(weights_vec * residuals_raw^2)

    # Weighted total SS
    y <- data_complete[[dep_name]]
    wm_y <- stats::weighted.mean(y, weights_vec)
    ss_total <- sum(weights_vec * (y - wm_y)^2)
    ss_regression <- ss_total - ss_residual

    # Degrees of freedom (SPSS uses weighted N)
    df_regression <- k
    df_residual <- n_effective - k - 1
    df_total <- n_effective - 1

    # Mean squares and F
    ms_regression <- ss_regression / df_regression
    ms_residual <- ss_residual / df_residual
    f_stat <- ms_regression / ms_residual
    f_p <- stats::pf(f_stat, df_regression, df_residual, lower.tail = FALSE)

    # Sigma (Std. Error of Estimate)
    sigma_spss <- sqrt(ms_residual)

    # R-squared
    r_squared <- ss_regression / ss_total
    adj_r_squared <- 1 - (1 - r_squared) * df_total / df_residual
    r_multiple <- sqrt(r_squared)

    # Coefficient standard errors with SPSS-compatible df
    # The ratio sigma_spss / sigma_r adjusts the standard errors
    sigma_r <- model_summary$sigma
    se_ratio <- sigma_spss / sigma_r

    coefs_raw <- model_summary$coefficients
    adj_se <- coefs_raw[, "Std. Error"] * se_ratio
    adj_t <- coefs_raw[, "Estimate"] / adj_se
    adj_p <- 2 * stats::pt(abs(adj_t), df = df_residual, lower.tail = FALSE)

    # Adjusted CI
    alpha <- 1 - conf.level
    t_crit <- stats::qt(1 - alpha / 2, df = df_residual)
    ci_lower <- coefs_raw[, "Estimate"] - t_crit * adj_se
    ci_upper <- coefs_raw[, "Estimate"] + t_crit * adj_se

    # Build results
    model_stats <- list(
      R = r_multiple,
      R_squared = r_squared,
      adj_R_squared = adj_r_squared,
      std_error = sigma_spss
    )

    anova_table <- tibble::tibble(
      Source = c("Regression", "Residual", "Total"),
      Sum_of_Squares = c(ss_regression, ss_residual, ss_total),
      df = as.integer(c(df_regression, df_residual, df_total)),
      Mean_Square = c(ms_regression, ms_residual, NA_real_),
      F_statistic = c(f_stat, NA_real_, NA_real_),
      Sig = c(f_p, NA_real_, NA_real_)
    )

    # Standardized coefficients
    term_names <- rownames(coefs_raw)
    beta <- rep(NA_real_, length(term_names))
    if (standardized) {
      w <- weights_vec
      sd_y <- sqrt(sum(w * (y - wm_y)^2) / (sum(w) - 1))
      for (i in seq_along(term_names)) {
        tn <- term_names[i]
        if (tn == "(Intercept)") next
        x <- data_complete[[tn]]
        sd_x <- sqrt(sum(w * (x - stats::weighted.mean(x, w))^2) / (sum(w) - 1))
        beta[i] <- coefs_raw[i, "Estimate"] * sd_x / sd_y
      }
    }

    coef_table <- tibble::tibble(
      Term = term_names,
      B = coefs_raw[, "Estimate"],
      Std.Error = adj_se,
      Beta = beta,
      t = adj_t,
      p = adj_p,
      CI_lower = ci_lower,
      CI_upper = ci_upper
    )

    n_report <- n_effective

  } else {
    # Unweighted: use standard lm() output directly
    n_report <- n

    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    r_multiple <- sqrt(r_squared)
    std_error <- model_summary$sigma

    model_stats <- list(
      R = r_multiple,
      R_squared = r_squared,
      adj_R_squared = adj_r_squared,
      std_error = std_error
    )

    anova_table <- .lm_anova(model, model_summary)

    coef_table <- .lm_coefficients(model, model_summary, data_complete,
                                   dep_name, pred_names, weights_vec,
                                   standardized, conf.level)
  }

  # ============================================================================
  # RETURN STRUCTURE
  # ============================================================================

  list(
    coefficients = coef_table,
    model_summary = model_stats,
    anova = anova_table,
    descriptives = descriptives,
    model = model,
    n = n_report
  )
}


# ============================================================================
# HELPER: DESCRIPTIVE STATISTICS
# ============================================================================

#' Compute descriptive statistics for regression variables
#' @keywords internal
.lm_descriptives <- function(data, var_names, weights_vec) {
  desc_list <- lapply(var_names, function(v) {
    x <- data[[v]]
    if (!is.null(weights_vec)) {
      # Weighted descriptives (matching SPSS WEIGHT BY behavior)
      w <- weights_vec
      wm <- stats::weighted.mean(x, w)
      # Weighted SD: SPSS uses frequency-weighted SD
      # SD = sqrt(sum(w * (x - wm)^2) / (sum(w) - 1))
      wsd <- sqrt(sum(w * (x - wm)^2) / (sum(w) - 1))
      wn <- round(sum(w))  # SPSS shows weighted N as integer
      tibble::tibble(Variable = v, Mean = wm, Std.Deviation = wsd, N = wn)
    } else {
      tibble::tibble(Variable = v, Mean = mean(x), Std.Deviation = stats::sd(x),
                     N = length(x))
    }
  })
  do.call(rbind, desc_list)
}


# ============================================================================
# HELPER: ANOVA TABLE
# ============================================================================

#' Compute ANOVA table for linear regression
#' @keywords internal
.lm_anova <- function(model, model_summary) {
  anova_result <- stats::anova(model)

  # Calculate Regression and Residual sums of squares
  k <- length(model$coefficients) - 1  # number of predictors
  ss_total <- sum(anova_result[["Sum Sq"]])
  ss_residual <- anova_result[["Sum Sq"]][nrow(anova_result)]
  ss_regression <- ss_total - ss_residual
  df_regression <- k
  df_residual <- model$df.residual
  df_total <- df_regression + df_residual
  ms_regression <- ss_regression / df_regression
  ms_residual <- ss_residual / df_residual
  f_stat <- model_summary$fstatistic[1]
  p_value <- stats::pf(f_stat, model_summary$fstatistic[2],
                        model_summary$fstatistic[3], lower.tail = FALSE)

  tibble::tibble(
    Source = c("Regression", "Residual", "Total"),
    Sum_of_Squares = c(ss_regression, ss_residual, ss_total),
    df = as.integer(c(df_regression, df_residual, df_total)),
    Mean_Square = c(ms_regression, ms_residual, NA_real_),
    F_statistic = c(f_stat, NA_real_, NA_real_),
    Sig = c(p_value, NA_real_, NA_real_)
  )
}


# ============================================================================
# HELPER: COEFFICIENTS TABLE
# ============================================================================

#' Compute coefficients table with standardized coefficients
#' @keywords internal
.lm_coefficients <- function(model, model_summary, data, dep_name, pred_names,
                             weights_vec, standardized, conf.level) {

  coefs <- model_summary$coefficients
  ci <- stats::confint(model, level = conf.level)

  # Term names
  term_names <- rownames(coefs)
  n_terms <- length(term_names)

  # Standardized coefficients (Beta)
  beta <- rep(NA_real_, n_terms)

  if (standardized) {
    y <- data[[dep_name]]

    if (!is.null(weights_vec)) {
      w <- weights_vec
      sd_y <- sqrt(sum(w * (y - stats::weighted.mean(y, w))^2) / (sum(w) - 1))
    } else {
      sd_y <- stats::sd(y)
    }

    for (i in seq_along(term_names)) {
      tn <- term_names[i]
      if (tn == "(Intercept)") next

      x <- data[[tn]]
      if (!is.null(weights_vec)) {
        w <- weights_vec
        sd_x <- sqrt(sum(w * (x - stats::weighted.mean(x, w))^2) / (sum(w) - 1))
      } else {
        sd_x <- stats::sd(x)
      }

      beta[i] <- coefs[i, "Estimate"] * sd_x / sd_y
    }
  }

  tibble::tibble(
    Term = term_names,
    B = coefs[, "Estimate"],
    Std.Error = coefs[, "Std. Error"],
    Beta = beta,
    t = coefs[, "t value"],
    p = coefs[, "Pr(>|t|)"],
    CI_lower = ci[, 1],
    CI_upper = ci[, 2]
  )
}


# ============================================================================
# PRINT METHOD
# ============================================================================

#' @export
print.linear_regression <- function(x, ...) {
  if (isTRUE(x$is_grouped)) {
    .print_lm_grouped(x)
  } else {
    .print_lm_ungrouped(x)
  }
  invisible(x)
}


#' Print ungrouped linear regression result
#' @keywords internal
.print_lm_ungrouped <- function(x) {
  # Header
  title <- get_standard_title("Linear Regression", x$weight_name, "Results")
  print_header(title)

  # Formula info
  formula_str <- deparse(x$formula)
  info <- list(
    "Formula" = formula_str,
    "Method" = "ENTER (all predictors)",
    "N" = x$n
  )
  if (isTRUE(x$weighted)) {
    info[["Weights"]] <- x$weight_name
  }
  print_info_section(info)

  cat("\n")

  # Model Summary
  .print_model_summary(x$model_summary)

  cat("\n")

  # ANOVA Table
  .print_anova_table(x$anova)

  cat("\n")

  # Coefficients Table
  .print_coefficients_table(x$coefficients, x$standardized)

  # Significance legend
  print_significance_legend(TRUE)
}


#' Print grouped linear regression results
#' @keywords internal
.print_lm_grouped <- function(x) {
  title <- get_standard_title("Linear Regression", x$weight_name, "Results")
  print_header(title)

  formula_str <- deparse(x$formula)
  info <- list(
    "Formula" = formula_str,
    "Method" = "ENTER (all predictors)",
    "Grouped by" = paste(x$group_vars, collapse = ", ")
  )
  if (isTRUE(x$weighted)) {
    info[["Weights"]] <- x$weight_name
  }
  print_info_section(info)

  for (grp in x$groups) {
    cat("\n")
    print_group_header(grp$group_values)

    cat(sprintf("  N: %d\n", grp$n))
    cat("\n")

    .print_model_summary(grp$model_summary)
    cat("\n")
    .print_anova_table(grp$anova)
    cat("\n")
    .print_coefficients_table(grp$coefficients, x$standardized)
  }

  print_significance_legend(TRUE)
}


# ============================================================================
# PRINT HELPERS
# ============================================================================

#' Print model summary table
#' @keywords internal
.print_model_summary <- function(ms) {
  cat("  Model Summary\n")
  w <- 60
  cat(paste0("  ", strrep("-", w), "\n"))
  cat(sprintf("  %-25s %10.3f\n", "R", ms$R))
  cat(sprintf("  %-25s %10.3f\n", "R Square", ms$R_squared))
  cat(sprintf("  %-25s %10.3f\n", "Adjusted R Square", ms$adj_R_squared))
  cat(sprintf("  %-25s %10.3f\n", "Std. Error of Estimate", ms$std_error))
  cat(paste0("  ", strrep("-", w), "\n"))
}


#' Print ANOVA table
#' @keywords internal
.print_anova_table <- function(anova) {
  cat("  ANOVA\n")
  w <- 78
  cat(paste0("  ", strrep("-", w), "\n"))
  cat(sprintf("  %-14s %16s %5s %16s %10s %8s\n",
              "Source", "Sum of Squares", "df", "Mean Square", "F", "Sig."))
  cat(paste0("  ", strrep("-", w), "\n"))

  for (i in seq_len(nrow(anova))) {
    ss_str <- format(round(anova$Sum_of_Squares[i], 3), big.mark = "", nsmall = 3)
    df_str <- format(anova$df[i])

    if (i <= 2) {
      ms_str <- format(round(anova$Mean_Square[i], 3), big.mark = "", nsmall = 3)
    } else {
      ms_str <- ""
    }

    if (i == 1) {
      f_str <- format(round(anova$F_statistic[i], 3), nsmall = 3)
      p_str <- format(round(anova$Sig[i], 3), nsmall = 3)
      stars <- add_significance_stars(anova$Sig[i])
    } else {
      f_str <- ""
      p_str <- ""
      stars <- ""
    }

    cat(sprintf("  %-14s %16s %5s %16s %10s %8s %s\n",
                anova$Source[i], ss_str, df_str, ms_str, f_str, p_str, stars))
  }
  cat(paste0("  ", strrep("-", w), "\n"))
}


#' Print coefficients table
#' @keywords internal
.print_coefficients_table <- function(coefs, show_beta) {
  cat("  Coefficients\n")

  if (show_beta) {
    w <- 88
    cat(paste0("  ", strrep("-", w), "\n"))
    cat(sprintf("  %-25s %10s %10s %8s %10s %8s %s\n",
                "Term", "B", "Std.Error", "Beta", "t", "Sig.", ""))
    cat(paste0("  ", strrep("-", w), "\n"))

    for (i in seq_len(nrow(coefs))) {
      term <- coefs$Term[i]
      if (nchar(term) > 25) term <- paste0(substr(term, 1, 22), "...")

      beta_str <- if (is.na(coefs$Beta[i])) "" else sprintf("%.3f", coefs$Beta[i])
      stars <- add_significance_stars(coefs$p[i])

      cat(sprintf("  %-25s %10.3f %10.3f %8s %10.3f %8.3f %s\n",
                  term, coefs$B[i], coefs$Std.Error[i], beta_str,
                  coefs$t[i], coefs$p[i], stars))
    }
  } else {
    w <- 78
    cat(paste0("  ", strrep("-", w), "\n"))
    cat(sprintf("  %-25s %10s %10s %10s %8s %s\n",
                "Term", "B", "Std.Error", "t", "Sig.", ""))
    cat(paste0("  ", strrep("-", w), "\n"))

    for (i in seq_len(nrow(coefs))) {
      term <- coefs$Term[i]
      if (nchar(term) > 25) term <- paste0(substr(term, 1, 22), "...")
      stars <- add_significance_stars(coefs$p[i])

      cat(sprintf("  %-25s %10.3f %10.3f %10.3f %8.3f %s\n",
                  term, coefs$B[i], coefs$Std.Error[i],
                  coefs$t[i], coefs$p[i], stars))
    }
  }
  cat(paste0("  ", strrep("-", w), "\n"))
}
