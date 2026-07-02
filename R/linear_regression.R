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
#' @param use How to handle missing data: \code{"listwise"} (default) drops any
#'   case with a missing value on any variable (matching SPSS /MISSING LISTWISE).
#'   \code{"pairwise"} computes the regression from a pairwise
#'   covariance/correlation matrix, retaining more cases (matching SPSS
#'   /MISSING PAIRWISE).
#' @param standardized Logical. If \code{TRUE} (default), standardized
#'   coefficients (Beta) are calculated and included in the output.
#' @param conf.level Confidence level for coefficient intervals (default 0.95).
#' @param factors How factor predictors are entered into the model:
#'   \code{"dummy"} (default, matches base R \code{lm()}) expands a factor
#'   with \code{L} levels into \code{L - 1} dummy contrasts; \code{"numeric"}
#'   silently coerces factor levels to their integer codes, matching SPSS
#'   \code{REGRESSION} default behavior (ordinal-as-scale). The "numeric"
#'   mode emits a one-line \code{cli::cli_inform()} listing the coerced
#'   variables. The "numeric" mode is required to reproduce SPSS results
#'   when factor predictors carry ordered meaning (e.g., 4-level education).
#'
#' @return For ungrouped + listwise data, an object of class
#'   \code{c("linear_regression", "lm")} — \strong{the fitted \code{lm}
#'   itself}, with mariposa-specific slots attached:
#' \describe{
#'   \item{coef_table}{SPSS-style tibble with B, Std.Error, Beta, t, p,
#'     CI_lower, CI_upper. For weighted models, SE / t / p are adjusted to
#'     SPSS's frequency-weight df (see Technical Details).}
#'   \item{anova_table}{SPSS-style overall-model ANOVA tibble (Source ×
#'     Sum_of_Squares / df / Mean_Square / F_statistic / Sig).}
#'   \item{model_summary}{List with R, R_squared, adj_R_squared, std_error.}
#'   \item{descriptives}{Tibble with Mean, Std.Deviation, N for all variables.}
#'   \item{n}{Sample size (listwise complete cases; weighted N when weighted).}
#'   \item{formula, dependent, predictor_names, weighted, weight_name, use, is_grouped, standardized, conf.level}{Call metadata.}
#' }
#'   Because the object inherits from \code{"lm"}, all standard generics
#'   (\code{predict()}, \code{anova()}, \code{vcov()}, \code{confint()},
#'   \code{residuals()}, \code{fitted()}, \code{coef()},
#'   \code{model.matrix()}, \code{broom::tidy()}, \code{broom::glance()},
#'   \code{broom::augment()}) dispatch natively without unwrapping.
#'   \code{summary()} returns the SPSS-style mariposa summary; for the
#'   raw lm summary use \code{stats::summary.lm()} on the same object.
#'
#'   For \code{use = "pairwise"} (no single fitted lm available) or for
#'   grouped data, returns a list of class \code{"linear_regression"}.
#'   Pairwise results expose the same SPSS-style tables but not the lm
#'   generics; grouped results hold one fitted lm-inheriting model per
#'   group under \code{$groups}.
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
#' \strong{Missing Data}: By default, listwise deletion is used (matching SPSS
#' REGRESSION /MISSING LISTWISE). Set \code{use = "pairwise"} to match SPSS
#' /MISSING PAIRWISE, which computes the regression from a pairwise
#' covariance matrix. Pairwise deletion retains more cases and produces
#' results closer to SPSS output when data has varying patterns of missingness.
#'
#' \strong{Weights}: When weights are specified, they are treated as frequency
#' weights (matching SPSS WEIGHT BY behavior). The model is fitted using weighted
#' least squares via \code{lm(weights = ...)}.
#'
#' \strong{Standardized Coefficients}: Beta = B * (SD_x / SD_y). This matches
#' the SPSS standardized coefficient output. Not available for the intercept.
#' For dummy-coded factor terms (\code{factors = "dummy"}), the SD of the
#' contrast column from the design matrix is used.
#'
#' \strong{Factor Predictors}: By default (\code{factors = "dummy"}),
#' factor predictors are expanded into \code{L - 1} dummy contrasts via
#' R's \code{stats::model.matrix()}, matching base R \code{lm()}. Pass
#' \code{factors = "numeric"} to silently coerce factor levels to their
#' integer codes (SPSS \code{REGRESSION} default). The "numeric" mode is
#' required to reproduce SPSS results for ordinal predictors like
#' education or Likert scales that SPSS treats as continuous.
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
#' # Factor predictors: dummy-coding (default, matches base R lm())
#' linear_regression(survey_data, income ~ age + education)
#'
#' # Factor predictors: SPSS-style ordinal-as-scale
#' linear_regression(survey_data, income ~ age + education,
#'                   factors = "numeric")
#'
#' # --- Three-layer output ---
#' result <- linear_regression(survey_data, life_satisfaction ~ age + income)
#' result                                  # compact one-line overview
#' summary(result)                         # full detailed SPSS-style output
#' summary(result, descriptives = FALSE)   # hide descriptives section
#'
#' @seealso
#' \code{\link{logistic_regression}} for binary outcome variables.
#'
#' \code{\link{describe}} for checking variable distributions before regression.
#'
#' \code{\link{pearson_cor}} for checking bivariate correlations.
#'
#' \code{\link{summary.linear_regression}} for detailed output with toggleable sections.
#'
#' @family regression
#' @export
linear_regression <- function(data, formula = NULL,
                              dependent = NULL, predictors = NULL,
                              weights = NULL,
                              use = c("listwise", "pairwise"),
                              standardized = TRUE,
                              conf.level = 0.95,
                              factors = c("dummy", "numeric")) {

  # ============================================================================
  # INPUT VALIDATION & FORMULA CONSTRUCTION
  # ============================================================================

  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame or tibble.")
  }

  use <- match.arg(use)
  factors <- match.arg(factors)

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
    .check_weights(weights_vec, weight_name)
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
                         grp_weights, use, standardized, conf.level, factors)
      gv <- as.list(group_keys[i, , drop = FALSE])
      gv <- lapply(gv, function(v) if (is.factor(v)) as.character(v) else v)
      result$group_values <- gv
      # Each listwise group result IS an lm — tag it as linear_regression so
      # predict()/anova()/broom generics on a single group dispatch natively.
      if (inherits(result, "lm")) {
        class(result) <- c("linear_regression", class(result))
      } else {
        class(result) <- "linear_regression"
      }
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
        use = use,
        is_grouped = TRUE,
        group_vars = group_vars,
        standardized = standardized,
        conf.level = conf.level
      ),
      class = "linear_regression"
    )
  } else {
    result <- .lm_core(data, model_formula, dep_name, pred_names,
                       weights_vec, use, standardized, conf.level, factors)
    # Listwise: result IS the lm (mariposa slots attached).
    # Pairwise: result is a custom list (no fitted lm available).
    result$formula <- model_formula
    result$dependent <- dep_name
    result$predictor_names <- pred_names
    result$weighted <- has_weights
    result$weight_name <- weight_name
    result$use <- use
    result$is_grouped <- FALSE
    result$standardized <- standardized
    result$conf.level <- conf.level
    if (inherits(result, "lm")) {
      class(result) <- c("linear_regression", class(result))
    } else {
      class(result) <- "linear_regression"
    }
    result
  }
}


# ============================================================================
# CORE COMPUTATION
# ============================================================================

#' Core linear regression computation
#' @noRd
.lm_core <- function(data, formula, dep_name, pred_names, weights_vec,
                     use, standardized, conf.level, factors = "dummy") {

  # Dispatch to pairwise implementation if requested
  if (use == "pairwise") {
    return(.lm_core_pairwise(data, dep_name, pred_names, weights_vec,
                             standardized, conf.level, factors))
  }

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

  # Factor predictor handling — see @param factors documentation.
  # "dummy" (default): let stats::lm() expand factors into L-1 contrasts.
  # "numeric": coerce factor levels to integer codes (SPSS REGRESSION default).
  factor_vars <- pred_names[
    vapply(data_complete[pred_names], is.factor, logical(1))
  ]
  if (length(factor_vars) > 0 && factors == "numeric") {
    cli::cli_inform(c(
      i = "Factor predictor(s) coerced to numeric (SPSS-style ordinal scaling):",
      "*" = "{.var {factor_vars}}"
    ))
    for (v in factor_vars) {
      data_complete[[v]] <- as.numeric(data_complete[[v]])
    }
  }
  # The dependent variable is always coerced to numeric (it is the response,
  # never categorical — use logistic_regression() for binary outcomes).
  if (is.factor(data_complete[[dep_name]])) {
    data_complete[[dep_name]] <- as.numeric(data_complete[[dep_name]])
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

  # Number of estimated model terms excluding the intercept. Must be counted
  # on the fitted model, not on pred_names: a factor with L levels expands to
  # L-1 dummy terms and formula operators (interactions, poly()) add terms,
  # all of which consume regression df. model$rank also excludes aliased
  # coefficients in rank-deficient fits.
  k <- model$rank - attr(stats::terms(model), "intercept")

  if (!is.null(weights_vec)) {
    # SPSS WEIGHT BY for REGRESSION: WLS with sum(w) as effective N.
    # Per Validation Charter §5.1: use UNROUNDED sum(w) in all internal
    # calculations (variance, SE, df, t/F, R^2). Round only for the displayed
    # N column. Earlier mariposa versions rounded too early via
    # `n_effective <- round(sum(w))`, producing systematic drift in df, F,
    # and CI bounds. Fixed in 0.7.0.
    sw <- sum(weights_vec)            # unrounded; for all calculations
    n_display <- round(sw)            # rounded; only for $n display

    # Weighted residual SS: sum(w * e^2) -- already computed by lm(weights=)
    residuals_raw <- stats::residuals(model)
    ss_residual <- sum(weights_vec * residuals_raw^2)

    # Weighted total SS
    y <- data_complete[[dep_name]]
    wm_y <- stats::weighted.mean(y, weights_vec)
    ss_total <- sum(weights_vec * (y - wm_y)^2)
    ss_regression <- ss_total - ss_residual

    # Degrees of freedom (SPSS uses unrounded weighted N)
    df_regression <- k
    df_residual <- sw - k - 1         # non-integer for weighted data
    df_total <- sw - 1

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

    # df column stays numeric (non-integer for weighted); print rounds for SPSS display
    anova_table <- tibble::tibble(
      Source = c("Regression", "Residual", "Total"),
      Sum_of_Squares = c(ss_regression, ss_residual, ss_total),
      df = c(df_regression, df_residual, df_total),
      Mean_Square = c(ms_regression, ms_residual, NA_real_),
      F_statistic = c(f_stat, NA_real_, NA_real_),
      Sig = c(f_p, NA_real_, NA_real_)
    )

    # Standardized coefficients (uses model.matrix to support dummy-coded factor terms)
    term_names <- rownames(coefs_raw)
    beta <- rep(NA_real_, length(term_names))
    if (standardized) {
      w <- weights_vec
      X <- stats::model.matrix(model)
      sd_y <- sqrt(sum(w * (y - wm_y)^2) / (sw - 1))
      for (i in seq_along(term_names)) {
        tn <- term_names[i]
        if (tn == "(Intercept)") next
        x_col <- X[, tn]
        wm_x <- stats::weighted.mean(x_col, w)
        sd_x <- sqrt(sum(w * (x_col - wm_x)^2) / (sw - 1))
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

    n_report <- n_display

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

  # Collinearity diagnostics (SPSS REGRESSION: Tolerance, VIF per term)
  collin <- .lm_collinearity(model, weights_vec)
  idx <- match(coef_table$Term, collin$term)
  coef_table$Tolerance <- collin$tolerance[idx]
  coef_table$VIF <- collin$vif[idx]

  # ============================================================================
  # RETURN STRUCTURE
  # ============================================================================
  # The result object IS the fitted lm — we attach mariposa-specific tables as
  # additional slots so that all base-R and broom generics (predict, anova,
  # vcov, confint, residuals, fitted, formula, model.matrix, tidy, glance,
  # augment, ...) dispatch natively through the "lm" class. The SPSS-style
  # tables live under non-colliding names ($coef_table, $anova_table,
  # $model_summary, $descriptives). lm's $coefficients stays the numeric
  # vector that downstream methods expect.

  out <- model
  out$coef_table   <- coef_table
  out$anova_table  <- anova_table
  out$model_summary <- model_stats
  out$descriptives <- descriptives
  out$n            <- n_report
  out
}


# ============================================================================
# PAIRWISE MISSING: CORE COMPUTATION
# ============================================================================

#' Core pairwise regression computation (matching SPSS /MISSING PAIRWISE)
#' @noRd
.lm_core_pairwise <- function(data, dep_name, pred_names, weights_vec,
                               standardized, conf.level, factors = "dummy") {

  all_vars <- c(dep_name, pred_names)
  k <- length(pred_names)
  p <- length(all_vars)
  has_weights <- !is.null(weights_vec)

  # Pairwise deletion operates on a numeric correlation matrix, so factor
  # predictors cannot be dummy-expanded here. Either coerce (factors="numeric")
  # or refuse to proceed.
  factor_vars <- all_vars[vapply(data[all_vars], is.factor, logical(1))]
  if (length(factor_vars) > 0) {
    if (factors == "dummy") {
      cli_abort(c(
        "Pairwise deletion ({.code use = \"pairwise\"}) does not support \\
         dummy-coded factor predictors.",
        "*" = "Affected: {.var {factor_vars}}",
        "i" = "Either use {.code use = \"listwise\"} (supports dummy contrasts) \\
               or {.code factors = \"numeric\"} (SPSS-style ordinal coercion)."
      ))
    }
    for (v in factor_vars) {
      data[[v]] <- as.numeric(data[[v]])
    }
  }

  # --------------------------------------------------------------------------
  # Step 1: Individual variable statistics (all available cases per variable)
  # --------------------------------------------------------------------------

  var_mean <- var_sd <- var_n <- numeric(p)
  names(var_mean) <- names(var_sd) <- names(var_n) <- all_vars

  for (v in all_vars) {
    x <- data[[v]]
    valid <- !is.na(x)
    if (has_weights) {
      valid <- valid & !is.na(weights_vec) & weights_vec > 0
      w <- weights_vec[valid]
      xv <- x[valid]
      m <- stats::weighted.mean(xv, w)
      n_w <- sum(w)
      s <- sqrt(sum(w * (xv - m)^2) / (n_w - 1))
    } else {
      xv <- x[valid]
      m <- mean(xv)
      n_w <- length(xv)
      s <- stats::sd(xv)
    }
    var_mean[v] <- m
    var_sd[v] <- s
    var_n[v] <- n_w
  }

  # --------------------------------------------------------------------------
  # Step 2: Pairwise correlation matrix and N matrix
  # --------------------------------------------------------------------------

  cor_mat <- matrix(1, p, p, dimnames = list(all_vars, all_vars))
  n_mat <- matrix(0, p, p, dimnames = list(all_vars, all_vars))

  for (i in seq_len(p)) {
    n_mat[i, i] <- var_n[all_vars[i]]
    if (i < p) {
      for (j in (i + 1):p) {
        vi <- all_vars[i]
        vj <- all_vars[j]
        xi <- data[[vi]]
        xj <- data[[vj]]
        valid <- !is.na(xi) & !is.na(xj)
        if (has_weights) {
          valid <- valid & !is.na(weights_vec) & weights_vec > 0
          w <- weights_vec[valid]
          xiv <- xi[valid]
          xjv <- xj[valid]
          n_w <- sum(w)
          mi <- stats::weighted.mean(xiv, w)
          mj <- stats::weighted.mean(xjv, w)
          cov_ij <- sum(w * (xiv - mi) * (xjv - mj)) / (n_w - 1)
          si <- sqrt(sum(w * (xiv - mi)^2) / (n_w - 1))
          sj <- sqrt(sum(w * (xjv - mj)^2) / (n_w - 1))
          r <- cov_ij / (si * sj)
        } else {
          xiv <- xi[valid]
          xjv <- xj[valid]
          n_w <- sum(valid)
          r <- stats::cor(xiv, xjv)
        }
        cor_mat[i, j] <- cor_mat[j, i] <- r
        n_mat[i, j] <- n_mat[j, i] <- n_w
      }
    }
  }

  # --------------------------------------------------------------------------
  # Step 3: Regression from correlation matrix
  # --------------------------------------------------------------------------

  R_XX <- cor_mat[pred_names, pred_names, drop = FALSE]
  R_XY <- cor_mat[pred_names, dep_name]

  # Standardized coefficients: Beta = solve(R_XX) %*% R_XY
  Beta <- as.vector(solve(R_XX) %*% R_XY)
  names(Beta) <- pred_names

  # R-squared
  R_sq <- as.numeric(crossprod(Beta, R_XY))
  R_mult <- sqrt(R_sq)

  # --------------------------------------------------------------------------
  # Step 4: Effective N and degrees of freedom
  # --------------------------------------------------------------------------

  N_eff <- round(min(n_mat[all_vars, all_vars]))
  df_reg <- k
  df_res <- N_eff - k - 1
  df_tot <- N_eff - 1

  if (df_res < 1) {
    cli_abort("Insufficient pairwise observations for the number of predictors.")
  }

  adj_R_sq <- 1 - (1 - R_sq) * df_tot / df_res

  # --------------------------------------------------------------------------
  # Step 5: Unstandardized coefficients
  # --------------------------------------------------------------------------

  sd_y <- var_sd[dep_name]
  sd_x <- var_sd[pred_names]
  mean_y <- var_mean[dep_name]
  mean_x <- var_mean[pred_names]

  B <- Beta * sd_y / sd_x
  B0 <- mean_y - sum(B * mean_x)

  # --------------------------------------------------------------------------
  # Step 6: ANOVA
  # --------------------------------------------------------------------------

  SS_tot <- sd_y^2 * df_tot
  SS_reg <- R_sq * SS_tot
  SS_res <- (1 - R_sq) * SS_tot

  MS_reg <- SS_reg / df_reg
  MS_res <- SS_res / df_res

  F_stat <- MS_reg / MS_res
  F_p <- stats::pf(F_stat, df_reg, df_res, lower.tail = FALSE)

  sigma <- sqrt(MS_res)

  # --------------------------------------------------------------------------
  # Step 7: Standard errors via Z'WZ matrix (augmented with intercept)
  # --------------------------------------------------------------------------
  # Z'WZ = [[N, N*mean_x'], [N*mean_x, (N-1)*Cov_XX + N*outer(mean_x)]]
  # where Cov_XX = diag(sd_x) %*% R_XX %*% diag(sd_x)

  D <- diag(sd_x, nrow = k, ncol = k)
  Cov_XX <- D %*% R_XX %*% D

  ZWZ <- matrix(0, k + 1, k + 1)
  ZWZ[1, 1] <- N_eff
  ZWZ[1, 2:(k + 1)] <- N_eff * mean_x
  ZWZ[2:(k + 1), 1] <- N_eff * mean_x
  ZWZ[2:(k + 1), 2:(k + 1)] <- df_tot * Cov_XX + N_eff * outer(mean_x, mean_x)

  Var_all <- MS_res * solve(ZWZ)
  SE_all <- sqrt(diag(Var_all))

  SE_B0 <- SE_all[1]
  SE_B <- SE_all[2:(k + 1)]

  # t-values and p-values
  t_B0 <- B0 / SE_B0
  t_B <- B / SE_B
  p_B0 <- 2 * stats::pt(abs(t_B0), df_res, lower.tail = FALSE)
  p_B <- 2 * stats::pt(abs(t_B), df_res, lower.tail = FALSE)

  # Confidence intervals
  alpha <- 1 - conf.level
  t_crit <- stats::qt(1 - alpha / 2, df_res)

  # Pre-compute to avoid tibble column name collision with variable 'B'
  all_B <- c(B0, B)
  all_SE <- c(SE_B0, SE_B)
  all_Beta <- c(NA_real_, if (standardized) Beta else rep(NA_real_, k))
  all_t <- c(t_B0, t_B)
  all_p <- c(p_B0, p_B)
  all_CI_lower <- all_B - t_crit * all_SE
  all_CI_upper <- all_B + t_crit * all_SE

  # --------------------------------------------------------------------------
  # Step 8: Build output (same structure as listwise for print compatibility)
  # --------------------------------------------------------------------------

  coef_table <- tibble::tibble(
    Term = c("(Intercept)", pred_names),
    B = all_B,
    Std.Error = all_SE,
    Beta = all_Beta,
    t = all_t,
    p = all_p,
    CI_lower = all_CI_lower,
    CI_upper = all_CI_upper
  )

  # Collinearity from the pairwise correlation matrix: VIF = diag(R_XX^-1)
  vif_pair <- tryCatch(diag(solve(R_XX)), error = function(e) rep(NA_real_, k))
  coef_table$Tolerance <- c(NA_real_, 1 / vif_pair)
  coef_table$VIF <- c(NA_real_, vif_pair)

  model_stats <- list(
    R = R_mult,
    R_squared = R_sq,
    adj_R_squared = adj_R_sq,
    std_error = sigma
  )

  anova_table <- tibble::tibble(
    Source = c("Regression", "Residual", "Total"),
    Sum_of_Squares = c(SS_reg, SS_res, SS_tot),
    df = as.integer(c(df_reg, df_res, df_tot)),
    Mean_Square = c(MS_reg, MS_res, NA_real_),
    F_statistic = c(F_stat, NA_real_, NA_real_),
    Sig = c(F_p, NA_real_, NA_real_)
  )

  descriptives <- tibble::tibble(
    Variable = all_vars,
    Mean = var_mean[all_vars],
    Std.Deviation = var_sd[all_vars],
    N = round(var_n[all_vars])
  )

  # Pairwise deletion has no single fitted lm object — return a custom list.
  # predict()/anova()/broom generics that require a fitted model will error
  # via predict.linear_regression() with a pointer to use = "listwise".
  list(
    coef_table = coef_table,
    model_summary = model_stats,
    anova_table = anova_table,
    descriptives = descriptives,
    n = N_eff
  )
}


# ============================================================================
# HELPER: DESCRIPTIVE STATISTICS
# ============================================================================

#' Compute descriptive statistics for regression variables
#'
#' For factor predictors entered as dummies (the default), this still reports
#' Mean/SD of the integer-coded factor levels — that matches what SPSS
#' \code{REGRESSION} prints in its Descriptive Statistics block (ordinal-as-
#' scale summary), regardless of how the factor is entered into the model.
#' @noRd
.lm_descriptives <- function(data, var_names, weights_vec) {
  desc_list <- lapply(var_names, function(v) {
    x <- data[[v]]
    if (is.factor(x)) {
      # Descriptives are informational; coerce only here, model uses dummy coding
      x <- as.numeric(x)
    }
    if (!is.null(weights_vec)) {
      # Weighted descriptives (matching SPSS WEIGHT BY behavior)
      w <- weights_vec
      wm <- stats::weighted.mean(x, w)
      # Weighted SD using unrounded sum(w) — Charter §5.1
      sw <- sum(w)
      wsd <- sqrt(sum(w * (x - wm)^2) / (sw - 1))
      wn <- round(sw)
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
#' @noRd
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
#' @noRd
.lm_coefficients <- function(model, model_summary, data, dep_name, pred_names,
                             weights_vec, standardized, conf.level) {

  coefs <- model_summary$coefficients
  ci <- stats::confint(model, level = conf.level)

  # Term names
  term_names <- rownames(coefs)
  n_terms <- length(term_names)

  # Standardized coefficients (Beta) — uses the design matrix column so dummy-
  # encoded factor terms (e.g. "educationhigh") resolve correctly.
  beta <- rep(NA_real_, n_terms)

  if (standardized) {
    y <- data[[dep_name]]
    X <- stats::model.matrix(model)

    if (!is.null(weights_vec)) {
      w <- weights_vec
      sd_y <- sqrt(sum(w * (y - stats::weighted.mean(y, w))^2) / (sum(w) - 1))
    } else {
      sd_y <- stats::sd(y)
    }

    for (i in seq_along(term_names)) {
      tn <- term_names[i]
      if (tn == "(Intercept)") next

      x_col <- X[, tn]
      if (!is.null(weights_vec)) {
        w <- weights_vec
        wm_x <- stats::weighted.mean(x_col, w)
        sd_x <- sqrt(sum(w * (x_col - wm_x)^2) / (sum(w) - 1))
      } else {
        sd_x <- stats::sd(x_col)
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
# COMPACT PRINT METHOD
# ============================================================================

#' Print linear regression results (compact)
#'
#' @description
#' Compact print method for objects of class \code{"linear_regression"}.
#' Shows R-squared, adjusted R-squared, F statistic, and p-value.
#'
#' For the full detailed output, use \code{summary()}.
#'
#' @param x An object of class \code{"linear_regression"} returned by
#'   \code{\link{linear_regression}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- linear_regression(survey_data, life_satisfaction ~ age + income)
#' result              # compact one-line overview
#' summary(result)     # full detailed output
#'
#' @export
#' @method print linear_regression
print.linear_regression <- function(x, ...) {
  weighted_tag <- if (isTRUE(x$weighted)) " [Weighted]" else ""
  formula_str <- deparse(x$formula)

  if (isTRUE(x$is_grouped)) {
    grouped_tag <- sprintf(" [Grouped: %s]", paste(x$group_vars, collapse = ", "))
    cat(sprintf("Linear Regression: %s%s%s\n", formula_str, weighted_tag, grouped_tag))
    for (grp in x$groups) {
      grp_label <- paste(names(grp$group_values), "=",
                         unlist(grp$group_values), collapse = ", ")
      f_stat <- grp$anova_table$F_statistic[1]
      f_df1 <- as.integer(round(grp$anova_table$df[1]))
      f_df2 <- as.integer(round(grp$anova_table$df[2]))
      f_p <- grp$anova_table$Sig[1]
      p_str <- format_p_compact(f_p)
      stars <- add_significance_stars(f_p)
      cat(sprintf("  %s: R2 = %.3f, adj.R2 = %.3f, F(%d, %d) = %.2f, %s %s, N = %d\n",
                  grp_label,
                  grp$model_summary$R_squared,
                  grp$model_summary$adj_R_squared,
                  f_df1, f_df2, f_stat,
                  p_str, stars, grp$n))
    }
  } else {
    cat(sprintf("Linear Regression: %s%s\n", formula_str, weighted_tag))
    f_stat <- x$anova_table$F_statistic[1]
    f_df1 <- as.integer(round(x$anova_table$df[1]))
    f_df2 <- as.integer(round(x$anova_table$df[2]))
    f_p <- x$anova_table$Sig[1]
    p_str <- format_p_compact(f_p)
    stars <- add_significance_stars(f_p)
    cat(sprintf("  R2 = %.3f, adj.R2 = %.3f, F(%d, %d) = %.2f, %s %s, N = %d\n",
                x$model_summary$R_squared,
                x$model_summary$adj_R_squared,
                f_df1, f_df2, f_stat,
                p_str, stars, x$n))
  }

  invisible(x)
}


# ============================================================================
# SUMMARY METHOD
# ============================================================================

#' Summary method for linear regression results
#'
#' @description
#' Creates a summary object that produces detailed output when printed,
#' including model summary, ANOVA table, and coefficient table.
#'
#' @param object A \code{linear_regression} result object.
#' @param model_summary Logical. Show model summary (R, R-squared)? (Default: TRUE)
#' @param anova_table Logical. Show ANOVA table? (Default: TRUE)
#' @param coefficients Logical. Show coefficients table? (Default: TRUE)
#' @param collinearity Logical. Show collinearity diagnostics (Tolerance,
#'   VIF per model term)? (Default: TRUE)
#' @param descriptives Logical. Show the Descriptive Statistics table
#'   (Mean, SD, N for the dependent and predictor variables)? (Default: TRUE)
#' @param digits Number of decimal places for formatting (Default: 3).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.linear_regression} object.
#'
#' @examples
#' result <- linear_regression(survey_data, life_satisfaction ~ age + trust_government)
#' summary(result)
#' summary(result, descriptives = FALSE)
#'
#' @seealso \code{\link{linear_regression}} for the main analysis function.
#' @export
#' @method summary linear_regression
summary.linear_regression <- function(object, model_summary = TRUE,
                                       anova_table = TRUE,
                                       coefficients = TRUE,
                                       collinearity = TRUE,
                                       descriptives = TRUE,
                                       digits = 3, ...) {
  build_summary_object(
    object     = object,
    show       = list(model_summary = model_summary,
                      anova_table   = anova_table,
                      coefficients  = coefficients,
                      collinearity  = collinearity,
                      descriptives  = descriptives),
    digits     = digits,
    class_name = "summary.linear_regression"
  )
}


#' Print summary of linear regression results (detailed output)
#'
#' @description
#' Displays the detailed SPSS-style output for a linear regression, with
#' sections controlled by the boolean parameters passed to
#' \code{\link{summary.linear_regression}}.  Sections include model summary
#' (R-squared, F-test), coefficients table (B, SE, Beta, t, p), and
#' collinearity diagnostics (Tolerance, VIF).
#'
#' @param x A \code{summary.linear_regression} object created by
#'   \code{\link{summary.linear_regression}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- linear_regression(survey_data, life_satisfaction ~ age + income)
#' summary(result)                           # all sections
#' summary(result, collinearity = FALSE)     # hide VIF/Tolerance
#'
#' @seealso \code{\link{linear_regression}} for the main analysis,
#'   \code{\link{summary.linear_regression}} for summary options.
#' @export
#' @method print summary.linear_regression
print.summary.linear_regression <- function(x, ...) {
  if (isTRUE(x$is_grouped)) {
    .print_summary_lm_grouped(x)
  } else {
    .print_summary_lm_ungrouped(x)
  }
  invisible(x)
}


#' Print ungrouped linear regression summary (verbose)
#' @noRd
.print_summary_lm_ungrouped <- function(x) {
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
  if (identical(x$use, "pairwise")) {
    info[["Missing"]] <- "Pairwise deletion"
  }
  print_info_section(info)

  show_model <- if (!is.null(x$show)) isTRUE(x$show$model_summary) else TRUE
  show_anova <- if (!is.null(x$show)) isTRUE(x$show$anova_table) else TRUE
  show_coefs <- if (!is.null(x$show)) isTRUE(x$show$coefficients) else TRUE
  show_collin <- if (!is.null(x$show)) isTRUE(x$show$collinearity) else TRUE
  show_desc  <- if (!is.null(x$show)) isTRUE(x$show$descriptives) else TRUE

  if (show_desc) {
    cat("\n")
    .print_descriptives_table(x$descriptives)
  }

  if (show_model) {
    cat("\n")
    .print_model_summary(x$model_summary)
  }

  if (show_anova) {
    cat("\n")
    .print_anova_table(x$anova_table)
  }

  if (show_coefs) {
    cat("\n")
    .print_coefficients_table(x$coef_table, x$standardized)
  }

  if (show_collin) {
    .print_collinearity_table(x$coef_table)
  }

  # Show significance legend if any p-value section is visible
  if (show_anova || show_coefs) {
    print_significance_legend(TRUE)
  }
}


#' Print grouped linear regression summary (verbose)
#' @noRd
.print_summary_lm_grouped <- function(x) {
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

  show_model <- if (!is.null(x$show)) isTRUE(x$show$model_summary) else TRUE
  show_anova <- if (!is.null(x$show)) isTRUE(x$show$anova_table) else TRUE
  show_coefs <- if (!is.null(x$show)) isTRUE(x$show$coefficients) else TRUE
  show_collin <- if (!is.null(x$show)) isTRUE(x$show$collinearity) else TRUE
  show_desc  <- if (!is.null(x$show)) isTRUE(x$show$descriptives) else TRUE

  for (grp in x$groups) {
    cat("\n")
    print_group_header(grp$group_values)

    cat(sprintf("  N: %d\n", grp$n))

    if (show_desc) {
      cat("\n")
      .print_descriptives_table(grp$descriptives)
    }

    if (show_model) {
      cat("\n")
      .print_model_summary(grp$model_summary)
    }

    if (show_anova) {
      cat("\n")
      .print_anova_table(grp$anova_table)
    }

    if (show_coefs) {
      cat("\n")
      .print_coefficients_table(grp$coef_table, x$standardized)
    }

    if (show_collin) {
      .print_collinearity_table(grp$coef_table)
    }
  }

  if (show_anova || show_coefs) {
    print_significance_legend(TRUE)
  }
}



# ============================================================================
# PRINT HELPERS
# ============================================================================

#' Print model summary table
#' @noRd
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
#' @noRd
.print_anova_table <- function(anova) {
  cat("  ANOVA\n")
  w <- 78
  cat(paste0("  ", strrep("-", w), "\n"))
  cat(sprintf("  %-14s %16s %5s %16s %10s %8s\n",
              "Source", "Sum of Squares", "df", "Mean Square", "F", "Sig."))
  cat(paste0("  ", strrep("-", w), "\n"))

  for (i in seq_len(nrow(anova))) {
    ss_str <- format(round(anova$Sum_of_Squares[i], 3), big.mark = "", nsmall = 3)
    # df may be non-integer for weighted models — SPSS rounds for display
    df_str <- format(round(anova$df[i]))

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


#' Print descriptive statistics table
#' @noRd
.print_descriptives_table <- function(desc) {
  if (is.null(desc) || nrow(desc) == 0) return(invisible(NULL))
  cat("  Descriptive Statistics\n")
  w <- 70
  cat(paste0("  ", strrep("-", w), "\n"))
  cat(sprintf("  %-35s %12s %12s %6s\n",
              "Variable", "Mean", "Std.Dev.", "N"))
  cat(paste0("  ", strrep("-", w), "\n"))
  for (i in seq_len(nrow(desc))) {
    v <- desc$Variable[i]
    if (nchar(v) > 35) v <- paste0(substr(v, 1, 32), "...")
    cat(sprintf("  %-35s %12.3f %12.3f %6d\n",
                v, desc$Mean[i], desc$Std.Deviation[i],
                as.integer(round(desc$N[i]))))
  }
  cat(paste0("  ", strrep("-", w), "\n"))
}


#' Collinearity diagnostics per model term
#'
#' Tolerance and VIF as SPSS REGRESSION reports them: VIF_j = j-th diagonal
#' of the inverse correlation matrix of the model-matrix columns (excluding
#' the intercept), Tolerance = 1/VIF. For weighted fits the weighted
#' correlation matrix is used, consistent with the frequency-weight
#' convention elsewhere in the file.
#'
#' @return tibble with columns term, tolerance, vif (NA when the
#'   correlation matrix is singular, e.g. aliased terms)
#' @noRd
.lm_collinearity <- function(model, weights_vec = NULL) {
  X <- stats::model.matrix(model)
  terms_keep <- setdiff(colnames(X), "(Intercept)")
  if (length(terms_keep) == 0) {
    return(tibble::tibble(term = character(0), tolerance = numeric(0),
                          vif = numeric(0)))
  }
  if (length(terms_keep) == 1) {
    return(tibble::tibble(term = terms_keep, tolerance = 1, vif = 1))
  }
  Xk <- X[, terms_keep, drop = FALSE]
  cor_mat <- tryCatch({
    if (!is.null(weights_vec)) {
      stats::cov.wt(Xk, wt = weights_vec, cor = TRUE)$cor
    } else {
      stats::cor(Xk)
    }
  }, error = function(e) NULL)
  vif <- rep(NA_real_, length(terms_keep))
  if (!is.null(cor_mat)) {
    inv <- tryCatch(solve(cor_mat), error = function(e) NULL)
    if (!is.null(inv)) vif <- diag(inv)
  }
  tibble::tibble(term = terms_keep, tolerance = 1 / vif, vif = vif)
}


#' Print collinearity statistics block (Tolerance / VIF)
#' @noRd
.print_collinearity_table <- function(coefs) {
  if (!"VIF" %in% names(coefs)) return(invisible(NULL))
  rows <- which(!is.na(coefs$VIF))
  if (length(rows) == 0) return(invisible(NULL))

  cat("\n  Collinearity Statistics\n")
  w <- 50
  cat(paste0("  ", strrep("-", w), "\n"))
  cat(sprintf("  %-25s %10s %10s\n", "Term", "Tolerance", "VIF"))
  cat(paste0("  ", strrep("-", w), "\n"))
  for (i in rows) {
    term <- coefs$Term[i]
    if (nchar(term) > 25) term <- paste0(substr(term, 1, 22), "...")
    cat(sprintf("  %-25s %10.3f %10.3f\n",
                term, coefs$Tolerance[i], coefs$VIF[i]))
  }
  cat(paste0("  ", strrep("-", w), "\n"))
  cat("  VIF > 10 (Tolerance < 0.1) indicates problematic collinearity.\n")
}


#' Print coefficients table
#' @noRd
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


# ============================================================================
# NATIVE GENERIC SUPPORT (predict / anova / etc.)
# ============================================================================
# Listwise + ungrouped linear_regression results inherit from "lm", so
# predict.lm, anova.lm, vcov.lm, confint.lm, residuals.lm, fitted.lm,
# formula.lm, model.matrix.lm, broom::tidy.lm, broom::glance.lm, and
# broom::augment.lm all dispatch natively without further code.
#
# Grouped and pairwise results do not have a single fitted lm to dispatch
# on. These overrides surface actionable error messages instead of letting
# users hit cryptic failures inside predict.default or similar.

.lr_require_lm <- function(object, generic) {
  if (isTRUE(object$is_grouped)) {
    cli_abort(c(
      "{.code {generic}()} is not supported on a grouped {.cls linear_regression}.",
      i = "Each element of {.code object$groups} is itself a fitted model.",
      i = "Use {.code lapply(object$groups, {generic}, ...)} for per-group results."
    ))
  }
  if (identical(object$use, "pairwise") || !inherits(object, "lm")) {
    cli_abort(c(
      "{.code {generic}()} is not available for pairwise-deleted regressions.",
      i = "Pairwise deletion does not produce a fitted {.cls lm} object.",
      i = "Refit with {.code use = \"listwise\"} to enable {.code {generic}()}."
    ))
  }
}

#' Predict from a linear_regression model
#'
#' For listwise + ungrouped results, dispatches to \code{stats::predict.lm}
#' (the result inherits from \code{"lm"}). For grouped or pairwise results,
#' raises an informative error.
#'
#' @param object A \code{linear_regression} result.
#' @param ... Passed to \code{stats::predict.lm}.
#' @export
#' @method predict linear_regression
predict.linear_regression <- function(object, ...) {
  .lr_require_lm(object, "predict")
  NextMethod()
}

#' ANOVA for a linear_regression model
#'
#' For listwise + ungrouped results, dispatches to \code{stats::anova.lm}
#' (sequential Type-I sum of squares per term). For the SPSS-style
#' overall-model ANOVA table, use \code{object$anova_table}.
#'
#' @param object A \code{linear_regression} result.
#' @param ... Passed to \code{stats::anova.lm}.
#' @export
#' @method anova linear_regression
anova.linear_regression <- function(object, ...) {
  .lr_require_lm(object, "anova")
  NextMethod()
}
