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
#'   If provided, \code{dependent} and \code{predictors} are ignored. The
#'   outcome may be transformed (\code{log(income) ~ age}); \code{y ~ .}
#'   uses all other columns except the weights and grouping variables.
#' @param dependent The dependent variable (unquoted). Used with \code{predictors}
#'   when no formula is given.
#' @param predictors Predictor variable(s) (unquoted, supports tidyselect).
#'   Used with \code{dependent} when no formula is given. The dependent,
#'   weights and grouping variables are never used as predictors (a
#'   selection such as \code{where(is.numeric)} drops them with a message).
#'   Character predictors are entered as factors.
#' @param weights Optional survey weights (unquoted variable name, or an
#'   expression such as \code{sampling_weight * 2}). When specified,
#'   weighted least squares (WLS) is used, matching SPSS WEIGHT BY.
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
#'   with \code{L} levels into \code{L - 1} contrasts; \code{"numeric"}
#'   silently coerces factor levels to their integer codes, matching SPSS
#'   \code{REGRESSION} default behavior (ordinal-as-scale). The "numeric"
#'   mode emits a one-line \code{cli::cli_inform()} listing the coerced
#'   variables. The "numeric" mode is required to reproduce SPSS results
#'   when factor predictors carry ordered meaning (e.g., 4-level education).
#'   Note that for \emph{ordered} factors, "dummy" applies R's default
#'   polynomial contrasts (terms suffixed \code{.L}, \code{.Q}, \code{.C}),
#'   not treatment dummies; convert with
#'   \code{factor(x, ordered = FALSE)} first if you want dummy coding.
#'   \code{factors} applies to factors only: labelled predictors from SPSS
#'   files (\code{haven_labelled}) are numeric and enter with their numeric
#'   codes, exactly as in SPSS REGRESSION; \code{summary()} notes them.
#'   Convert them with \code{\link{to_label}} first to get dummy coding.
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
#' least squares via \code{lm(weights = ...)}; the coefficients are those of
#' \code{lm()}, but N is \code{sum(w)} and the residual df are
#' \code{sum(w) - rank} (unrounded), as in SPSS. \code{lm()} itself treats
#' weights as analytic (precision) weights with df = cases - rank, so the
#' inherited generics are adjusted to the SPSS convention:
#' \code{vcov()}, \code{confint()}, \code{nobs()} (unrounded \code{sum(w)}),
#' \code{df.residual()}, \code{anova()} (single model), \code{predict()}
#' (standard errors and intervals), \code{broom::tidy()} and
#' \code{broom::glance()} all agree with \code{summary()}.
#' \code{stats::summary.lm()}, \code{logLik()}, \code{AIC()} and
#' \code{BIC()} keep lm's analytic-weight definitions.
#'
#' \strong{Standardized Coefficients}: Beta = B * (SD_x / SD_y). This matches
#' the SPSS standardized coefficient output. Not available for the intercept.
#' For dummy-coded factor terms (\code{factors = "dummy"}), the SD of the
#' contrast column from the design matrix is used.
#'
#' \strong{Factor Predictors}: By default (\code{factors = "dummy"}),
#' factor predictors are expanded into \code{L - 1} contrasts via
#' R's \code{stats::model.matrix()}, matching base R \code{lm()}: unordered
#' factors get treatment (dummy) contrasts against the first level, while
#' \emph{ordered} factors get R's default polynomial contrasts
#' (\code{.L}/\code{.Q}/\code{.C} terms). Pass
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
  user_call <- match.call()

  # Process weights (a column name or an expression such as w * 2)
  wi <- .regression_weights(data, rlang::enquo(weights))
  data <- wi$data
  weight_name <- wi$name
  weights_vec <- wi$vec
  has_weights <- !is.null(weight_name)

  # Build and validate the formula (both interfaces)
  fb <- .build_regression_formula(
    data, formula, rlang::enquo(dependent), rlang::enquo(predictors),
    weight_name = weight_name, allow_lhs_call = TRUE,
    env = parent.frame()
  )
  model_formula <- fb$formula
  dep_name <- fb$dep_name
  dep_vars <- fb$dep_vars
  pred_names <- fb$pred_names
  # The call a user would type, in formula form: update()/step() re-run it
  user_call <- .regression_call(user_call, model_formula)
  # Labelled (SPSS) predictors enter with their numeric codes, as in SPSS
  # REGRESSION; the summary says so (factors = "dummy" applies to factors)
  labelled_preds <- .labelled_predictors(data, pred_names)

  if (use == "pairwise" && !identical(dep_vars, dep_name)) {
    cli_abort(c(
      "{.code use = \"pairwise\"} needs a plain dependent variable, not {.code {dep_name}}.",
      i = "Create the transformed variable in the data first, or use {.code use = \"listwise\"}."
    ))
  }

  # ============================================================================
  # GROUPED ANALYSIS
  # ============================================================================

  is_grouped <- inherits(data, "grouped_df")

  if (is_grouped) {
    group_vars <- dplyr::group_vars(data)
    group_split <- dplyr::group_split(data)
    group_keys <- dplyr::group_keys(data)

    # A group that cannot be fitted is skipped with a warning (SPSS SPLIT
    # FILE carries on with the other splits)
    fits <- .fit_groups(group_split, group_keys, function(grp_data) {
      grp_weights <- if (has_weights) grp_data[[weight_name]] else NULL
      result <- .lm_core(grp_data, model_formula, dep_name, pred_names,
                         grp_weights, use, standardized, conf.level, factors,
                         dep_vars = dep_vars)
      # Each listwise group result IS an lm — tag it as linear_regression so
      # predict()/anova()/broom generics on a single group dispatch natively.
      if (inherits(result, "lm")) {
        class(result) <- c("linear_regression", class(result))
      } else {
        class(result) <- "linear_regression"
      }
      result
    })
    group_results <- fits$results

    structure(
      list(
        groups = group_results,
        skipped_groups = fits$skipped,
        formula = model_formula,
        dependent = dep_name,
        predictor_names = pred_names,
        weighted = has_weights,
        weight_name = weight_name,
        use = use,
        is_grouped = TRUE,
        group_vars = group_vars,
        standardized = standardized,
        conf.level = conf.level,
        call = user_call,
        labelled_predictors = labelled_preds
      ),
      class = "linear_regression"
    )
  } else {
    result <- .lm_core(data, model_formula, dep_name, pred_names,
                       weights_vec, use, standardized, conf.level, factors,
                       dep_vars = dep_vars)
    # Listwise: result IS the lm (mariposa slots attached).
    # Pairwise: result is a custom list (no fitted lm available).
    result$call <- user_call
    result$labelled_predictors <- labelled_preds
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
                     use, standardized, conf.level, factors = "dummy",
                     dep_vars = dep_name) {

  # Dispatch to pairwise implementation if requested
  if (use == "pairwise") {
    # Pairwise regression is computed from the correlation matrix of the raw
    # variables, so formula operators (interactions, I(), poly(), ...) cannot
    # be honored — refuse rather than silently fitting main effects only.
    term_labels <- attr(stats::terms(formula), "term.labels")
    unsupported <- setdiff(term_labels, pred_names)
    if (length(unsupported) > 0) {
      cli_abort(c(
        "{.code use = \"pairwise\"} supports only plain additive predictors.",
        "*" = "Unsupported: {.code {unsupported}}",
        i = "Use {.code use = \"listwise\"} for interactions or transformed terms."
      ))
    }
    return(.lm_core_pairwise(data, dep_name, pred_names, weights_vec,
                             standardized, conf.level, factors))
  }

  all_vars <- unique(c(dep_vars, pred_names))

  # Listwise deletion (SPSS MISSING LISTWISE)
  complete <- stats::complete.cases(data[, all_vars, drop = FALSE])
  if (!is.null(weights_vec)) {
    complete <- complete & !is.na(weights_vec)
    weights_vec <- weights_vec[complete]
  }
  data_complete <- data[complete, , drop = FALSE]
  n <- nrow(data_complete)

  if (n < length(pred_names) + 2) {
    .abort_insufficient_cases(data, all_vars, n, length(pred_names))
  }

  # Character predictors are categorical: enter them as factors (as lm()
  # would), so descriptives and the numeric mode treat them like factors.
  # Labelled (SPSS) variables enter with their numeric codes: fit on the
  # bare numbers so lm() does not depend on haven's vctrs arithmetic
  # methods being registered (see .plain_numeric).
  for (v in all_vars) {
    if (is.character(data_complete[[v]]) && v %in% pred_names) {
      data_complete[[v]] <- factor(data_complete[[v]])
    } else if (inherits(data_complete[[v]], "haven_labelled")) {
      data_complete[[v]] <- .plain_numeric(data_complete[[v]])
    }
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
  if (identical(dep_vars, dep_name) && is.factor(data_complete[[dep_name]])) {
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

  # A transformation can still yield NA (e.g. log of a negative value);
  # lm() drops those rows - keep data and weights aligned with the fit
  if (!is.null(model$na.action)) {
    drop_rows <- as.integer(model$na.action)
    data_complete <- data_complete[-drop_rows, , drop = FALSE]
    if (!is.null(weights_vec)) weights_vec <- weights_vec[-drop_rows]
    n <- nrow(data_complete)
  }
  # The response as fitted - differs from the raw column for a
  # transformed outcome such as log(income)
  y <- as.numeric(stats::model.response(model$model))

  # Perfectly collinear terms come back as NA coefficients; SPSS excludes
  # such variables from the equation with a note. Surface the exclusion
  # instead of failing silently downstream.
  aliased <- names(stats::coef(model))[is.na(stats::coef(model))]
  if (length(aliased) > 0) {
    cli::cli_inform(c(
      i = "Excluded due to perfect collinearity (matching SPSS): {.var {aliased}}"
    ))
  }

  model_summary <- summary(model)

  # ============================================================================
  # DESCRIPTIVE STATISTICS (matching SPSS output)
  # ============================================================================

  descriptives <- .lm_descriptives(data_complete, dep_name, y, pred_names,
                                   weights_vec)

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
    # and CI bounds. Fixed in 0.6.4.
    sw <- sum(weights_vec)            # unrounded; for all calculations
    n_display <- round(sw)            # rounded; only for $n display

    # Weighted residual SS: sum(w * e^2) -- already computed by lm(weights=)
    residuals_raw <- stats::residuals(model)
    ss_residual <- sum(weights_vec * residuals_raw^2)

    # Weighted total SS (y = the fitted response, see above)
    wm_y <- stats::weighted.mean(y, weights_vec)
    ss_total <- sum(weights_vec * (y - wm_y)^2)
    ss_regression <- ss_total - ss_residual

    # Degrees of freedom (SPSS uses unrounded weighted N). sw - rank equals
    # sw - k - 1 for intercept models and stays correct without an intercept.
    df_regression <- k
    df_residual <- sw - model$rank    # non-integer for weighted data
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
                                   standardized, conf.level, y = y)
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
  if (!is.null(weights_vec)) {
    # Frequency-weight quantities for the inherited lm generics (vcov,
    # confint, nobs, df.residual, anova, predict, broom): lm() itself
    # treats weights as analytic weights with df = cases - rank.
    out$spss_weights <- list(sum_w = sw, df_residual = df_residual,
                             sigma = sigma_spss)
  }
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
  factor_vars <- all_vars[vapply(data[all_vars], function(x) {
    is.factor(x) || is.character(x)
  }, logical(1))]
  for (v in factor_vars) {
    if (is.character(data[[v]])) data[[v]] <- factor(data[[v]])
  }
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

  # Unrounded (Charter 5.1): the smallest pairwise N - a weight sum when
  # weighted - enters df, SS and the standard errors; $n rounds for display
  N_eff <- min(n_mat[all_vars, all_vars])
  df_reg <- k
  df_res <- N_eff - k - 1
  df_tot <- N_eff - 1

  if (df_res < 1) {
    cli_abort(c(
      "Too few pairwise observations for {k} predictor{?s}.",
      i = "The smallest pairwise N is {round(N_eff, 1)}; at least {k + 2} are needed."
    ), class = "mariposa_degenerate_fit")
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
    # numeric: non-integer for weighted data; print rounds for display
    df = c(df_reg, df_res, df_tot),
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
    n = round(N_eff)
  )
}


# ============================================================================
# HELPER: DESCRIPTIVE STATISTICS
# ============================================================================

#' Compute descriptive statistics for regression variables
#'
#' One row per outcome and numeric predictor (Mean, SD, N as in SPSS
#' REGRESSION /DESCRIPTIVES). A factor predictor entered with dummy coding
#' gets one row per dummy (non-reference level), named like its
#' coefficient: the mean of a dummy is the share of that category - the
#' variables SPSS would describe for a dummy-coded predictor. (The mean of
#' the factor's level index, shown before, has no meaning for a nominal
#' variable.) Factors entered with factors = "numeric" are already
#' integer codes here. Weighted: SPSS frequency weights via the kernels,
#' unrounded sum(w) (Charter 5.1), N rounded for display.
#' @noRd
.lm_descriptives <- function(data, dep_name, y, pred_names, weights_vec) {
  stat_row <- function(label, x) {
    x <- as.numeric(x)
    if (!is.null(weights_vec)) {
      tibble::tibble(Variable = label,
                     Mean = .w_mean(x, weights_vec),
                     Std.Deviation = .w_sd(x, weights_vec),
                     N = round(sum(weights_vec)))
    } else {
      tibble::tibble(Variable = label, Mean = mean(x),
                     Std.Deviation = stats::sd(x), N = length(x))
    }
  }

  # Outcome: the fitted response (a transformed outcome such as
  # log(income) has no column of its own)
  rows <- list(stat_row(dep_name, y))
  for (v in pred_names) {
    x <- data[[v]]
    if (is.factor(x)) {
      levs <- levels(droplevels(x))
      for (lv in levs[-1]) {
        rows[[length(rows) + 1]] <- stat_row(paste0(v, lv), x == lv)
      }
    } else {
      rows[[length(rows) + 1]] <- stat_row(v, .plain_numeric(x))
    }
  }
  do.call(rbind, rows)
}


# ============================================================================
# HELPER: ANOVA TABLE
# ============================================================================

#' Compute ANOVA table for linear regression
#' @noRd
.lm_anova <- function(model, model_summary) {
  anova_result <- stats::anova(model)

  # Regression df = number of estimated non-intercept terms. Counted on the
  # model rank (same rule as the weighted path): aliased coefficients in
  # rank-deficient fits carry no df.
  k <- model$rank - attr(stats::terms(model), "intercept")
  ss_total <- sum(anova_result[["Sum Sq"]])
  ss_residual <- anova_result[["Sum Sq"]][nrow(anova_result)]
  ss_regression <- ss_total - ss_residual
  df_regression <- k
  df_residual <- model$df.residual
  df_total <- df_regression + df_residual
  ms_regression <- ss_regression / df_regression
  ms_residual <- ss_residual / df_residual
  f_stat <- unname(model_summary$fstatistic[1])
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
                             weights_vec, standardized, conf.level,
                             y = data[[dep_name]]) {

  coefs <- model_summary$coefficients
  # summary() drops aliased (NA) coefficients while confint() keeps them as
  # NA rows — align on the summary's rows so rank-deficient fits don't
  # produce tables of mismatched length.
  ci <- stats::confint(model, level = conf.level)
  ci <- ci[rownames(coefs), , drop = FALSE]

  # Term names
  term_names <- rownames(coefs)
  n_terms <- length(term_names)

  # Standardized coefficients (Beta) — uses the design matrix column so dummy-
  # encoded factor terms (e.g. "educationhigh") resolve correctly.
  beta <- rep(NA_real_, n_terms)

  if (standardized) {
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
#' @param digits Number of decimal places for R-squared and the p-value
#'   (the F statistic keeps one decimal fewer). (Default: 3)
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
print.linear_regression <- function(x, digits = 3, ...) {
  weighted_tag <- if (isTRUE(x$weighted)) " [Weighted]" else ""
  formula_str <- .formula_label(x$formula)

  fit_line <- function(m) {
    sprintf("R2 = %s, adj.R2 = %s, F(%s, %s) = %s, %s, N = %s",
            .fmt_fixed(m$model_summary$R_squared, digits),
            .fmt_fixed(m$model_summary$adj_R_squared, digits),
            .fmt_n(m$anova_table$df[1]), .fmt_n(m$anova_table$df[2]),
            .fmt_est(m$anova_table$F_statistic[1], max(digits - 1L, 1L)),
            format_p_stars(m$anova_table$Sig[1], digits),
            .fmt_n(m$n))
  }

  if (isTRUE(x$is_grouped)) {
    grouped_tag <- sprintf(" [Grouped: %s]", paste(x$group_vars, collapse = ", "))
    cat(sprintf("Linear Regression: %s%s%s\n", formula_str, weighted_tag, grouped_tag))
    for (grp in x$groups) {
      cat(sprintf("  %s: %s\n", .format_group_label(grp$group_values),
                  fit_line(grp)))
    }
    .print_skipped_groups(x$skipped_groups)
  } else {
    cat(sprintf("Linear Regression: %s%s\n", formula_str, weighted_tag))
    cat(sprintf("  %s\n", fit_line(x)))
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
#' @param conf_int Logical. Show the confidence-interval columns for B in the
#'   coefficients table (SPSS \code{/STATISTICS CI})? The interval level is
#'   the \code{conf.level} passed to \code{\link{linear_regression}}.
#'   (Default: TRUE)
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
#' summary(result, conf_int = FALSE)   # hide the CI columns
#'
#' @seealso \code{\link{linear_regression}} for the main analysis function.
#' @export
#' @method summary linear_regression
summary.linear_regression <- function(object, model_summary = TRUE,
                                       anova_table = TRUE,
                                       coefficients = TRUE,
                                       conf_int = TRUE,
                                       collinearity = TRUE,
                                       descriptives = TRUE,
                                       digits = 3, ...) {
  build_summary_object(
    object     = object,
    show       = list(model_summary = model_summary,
                      anova_table   = anova_table,
                      coefficients  = coefficients,
                      conf_int      = conf_int,
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

  digits <- x$digits %||% 3

  # Formula info
  formula_str <- .formula_label(x$formula)
  info <- list(
    "Formula" = formula_str,
    "Method" = "ENTER (all predictors)",
    "N" = .fmt_n(x$n)
  )
  if (isTRUE(x$weighted)) {
    info[["Weights"]] <- x$weight_name
  }
  if (identical(x$use, "pairwise")) {
    info[["Missing"]] <- "Pairwise deletion"
  }
  print_info_section(info)
  .print_labelled_note(x$labelled_predictors)

  show_model <- if (!is.null(x$show)) isTRUE(x$show$model_summary) else TRUE
  show_anova <- if (!is.null(x$show)) isTRUE(x$show$anova_table) else TRUE
  show_coefs <- if (!is.null(x$show)) isTRUE(x$show$coefficients) else TRUE
  show_ci    <- if (!is.null(x$show)) isTRUE(x$show$conf_int) else TRUE
  show_collin <- if (!is.null(x$show)) isTRUE(x$show$collinearity) else TRUE
  show_desc  <- if (!is.null(x$show)) isTRUE(x$show$descriptives) else TRUE

  if (show_desc) {
    cat("\n")
    .print_descriptives_table(x$descriptives, digits)
  }

  if (show_model) {
    cat("\n")
    .print_model_summary(x$model_summary, digits)
  }

  if (show_anova) {
    cat("\n")
    .print_anova_table(x$anova_table, digits)
  }

  if (show_coefs) {
    cat("\n")
    .print_coefficients_table(x$coef_table, x$standardized, show_ci, digits,
                              x$conf.level)
  }

  if (show_collin) {
    .print_collinearity_table(x$coef_table, digits)
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

  digits <- x$digits %||% 3

  formula_str <- .formula_label(x$formula)
  info <- list(
    "Formula" = formula_str,
    "Method" = "ENTER (all predictors)",
    "Grouped by" = paste(x$group_vars, collapse = ", ")
  )
  if (isTRUE(x$weighted)) {
    info[["Weights"]] <- x$weight_name
  }
  if (identical(x$use, "pairwise")) {
    info[["Missing"]] <- "Pairwise deletion"
  }
  print_info_section(info)
  .print_labelled_note(x$labelled_predictors)

  show_model <- if (!is.null(x$show)) isTRUE(x$show$model_summary) else TRUE
  show_anova <- if (!is.null(x$show)) isTRUE(x$show$anova_table) else TRUE
  show_coefs <- if (!is.null(x$show)) isTRUE(x$show$coefficients) else TRUE
  show_ci    <- if (!is.null(x$show)) isTRUE(x$show$conf_int) else TRUE
  show_collin <- if (!is.null(x$show)) isTRUE(x$show$collinearity) else TRUE
  show_desc  <- if (!is.null(x$show)) isTRUE(x$show$descriptives) else TRUE

  for (grp in x$groups) {
    cat("\n")
    print_group_header(grp$group_values)

    cat(sprintf("  N: %s\n", .fmt_n(grp$n)))

    if (show_desc) {
      cat("\n")
      .print_descriptives_table(grp$descriptives, digits)
    }

    if (show_model) {
      cat("\n")
      .print_model_summary(grp$model_summary, digits)
    }

    if (show_anova) {
      cat("\n")
      .print_anova_table(grp$anova_table, digits)
    }

    if (show_coefs) {
      cat("\n")
      .print_coefficients_table(grp$coef_table, x$standardized, show_ci,
                                digits, x$conf.level)
    }

    if (show_collin) {
      .print_collinearity_table(grp$coef_table, digits)
    }
  }
  .print_skipped_groups(x$skipped_groups, verbose = TRUE)

  if (show_anova || show_coefs) {
    print_significance_legend(TRUE)
  }
}



# ============================================================================
# PRINT HELPERS
# ============================================================================

#' Print model summary table
#' @noRd
.print_model_summary <- function(ms, digits = 3) {
  .print_kv_block(
    "Model Summary",
    c("R", "R Square", "Adjusted R Square", "Std. Error of the Estimate"),
    c(.fmt_fixed(ms$R, digits), .fmt_fixed(ms$R_squared, digits),
      .fmt_fixed(ms$adj_R_squared, digits), .fmt_est(ms$std_error, digits))
  )
}


#' Print ANOVA table
#'
#' Columns are pre-formatted text sized to their content by
#' print_stat_table(): digits honoured, Sig. in SPSS table style
#' ("<.001"), df rounded for display (non-integer when weighted).
#' @noRd
.print_anova_table <- function(anova, digits = 3) {
  cat("  ANOVA\n")
  first <- seq_len(nrow(anova)) == 1L
  tab <- data.frame(
    Source = anova$Source,
    ss = .fmt_est(anova$Sum_of_Squares, digits),
    dfv = .fmt_n(anova$df),
    ms = .fmt_est(anova$Mean_Square, digits),
    fv = ifelse(first, .fmt_est(anova$F_statistic, digits), ""),
    pv = ifelse(first, fmt_p(anova$Sig, digits, style = "table"), ""),
    stars = ifelse(first, add_significance_stars(anova$Sig), ""),
    stringsAsFactors = FALSE
  )
  print_stat_table(tab, col_labels = c(ss = "Sum of Squares", dfv = "df",
                                       ms = "Mean Square", fv = "F",
                                       pv = "Sig.", stars = ""))
}


#' Print descriptive statistics table
#' @noRd
.print_descriptives_table <- function(desc, digits = 3) {
  if (is.null(desc) || nrow(desc) == 0) return(invisible(NULL))
  cat("  Descriptive Statistics\n")
  tab <- data.frame(
    Variable = desc$Variable,
    mean = .fmt_est(desc$Mean, digits),
    sd = .fmt_est(desc$Std.Deviation, digits),
    nv = .fmt_n(desc$N),
    stringsAsFactors = FALSE
  )
  print_stat_table(tab, col_labels = c(mean = "Mean", sd = "Std. Deviation",
                                       nv = "N"))
}


#' Collinearity diagnostics per model term
#'
#' Tolerance and VIF as SPSS REGRESSION reports them: VIF_j = j-th diagonal
#' of the inverse correlation matrix of the model-matrix columns (excluding
#' the intercept), Tolerance = 1/VIF. For weighted fits the weighted
#' correlation matrix is used, consistent with the frequency-weight
#' convention elsewhere in the file.
#'
#' Aliased columns (NA coefficient, excluded for perfect collinearity) are
#' dropped first, as SPSS reports Tolerance/VIF for the retained terms
#' only; keeping them makes the correlation matrix singular.
#'
#' @return tibble with columns term, tolerance, vif (NA when the
#'   correlation matrix is singular)
#' @noRd
.lm_collinearity <- function(model, weights_vec = NULL) {
  X <- stats::model.matrix(model)
  X <- X[, !is.na(stats::coef(model)), drop = FALSE]
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
.print_collinearity_table <- function(coefs, digits = 3) {
  if (!"VIF" %in% names(coefs)) return(invisible(NULL))
  rows <- which(!is.na(coefs$VIF))
  if (length(rows) == 0) return(invisible(NULL))

  cat("\n  Collinearity Statistics\n")
  tab <- data.frame(
    Term = coefs$Term[rows],
    tol = .fmt_est(coefs$Tolerance[rows], digits),
    vif = .fmt_est(coefs$VIF[rows], digits),
    stringsAsFactors = FALSE
  )
  print_stat_table(tab, col_labels = c(tol = "Tolerance", vif = "VIF"))
  cat("  VIF > 10 (Tolerance < 0.1) indicates problematic collinearity.\n")
}


#' Print coefficients table
#'
#' show_ci appends the confidence-interval columns for B (SPSS
#' /STATISTICS CI); the interval level is the conf.level the model was
#' fitted with. Term names are never truncated: print_stat_table() sizes
#' the column to the longest term (dummy names of long factor levels were
#' cut to 25 characters and became ambiguous).
#' @noRd
.print_coefficients_table <- function(coefs, show_beta, show_ci = FALSE,
                                      digits = 3, conf.level = 0.95) {
  cat("  Coefficients\n")
  show_ci <- isTRUE(show_ci) && all(c("CI_lower", "CI_upper") %in% names(coefs))

  tab <- data.frame(
    Term = coefs$Term,
    b = .fmt_est(coefs$B, digits),
    se = .fmt_est(coefs$Std.Error, digits),
    stringsAsFactors = FALSE
  )
  if (isTRUE(show_beta)) tab$beta <- .fmt_est(coefs$Beta, digits)
  tab$tv <- .fmt_est(coefs$t, digits)
  tab$pv <- fmt_p(coefs$p, digits, style = "table")
  if (show_ci) {
    tab$lo <- .fmt_est(coefs$CI_lower, digits)
    tab$hi <- .fmt_est(coefs$CI_upper, digits)
  }
  tab$stars <- add_significance_stars(coefs$p)

  ci <- .ci_label(conf.level)
  print_stat_table(tab, col_labels = c(b = "B", se = "Std. Error",
                                       beta = "Beta", tv = "t", pv = "Sig.",
                                       lo = paste(ci, "CI Lower"),
                                       hi = paste(ci, "CI Upper"),
                                       stars = ""))
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
#' @return A numeric vector of predictions (or a matrix/list, depending on
#'   the arguments), as returned by \code{stats::predict.lm}.
#' @export
#' @method predict linear_regression
predict.linear_regression <- function(object, ...) {
  .lr_require_lm(object, "predict")
  fw <- object$spss_weights
  if (!is.null(fw) && !"scale" %in% names(list(...))) {
    # Weighted: residual scale and df of the SPSS frequency-weight fit
    return(stats::predict(.lr_strip_class(object), ...,
                          scale = fw$sigma, df = fw$df_residual))
  }
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
#' @return An \code{anova} table (sequential Type-I sums of squares), as
#'   returned by \code{stats::anova.lm}.
#' @export
#' @method anova linear_regression
anova.linear_regression <- function(object, ...) {
  .lr_require_lm(object, "anova")
  fw <- object$spss_weights
  if (is.null(fw) || length(list(...)) > 0) {
    return(NextMethod())
  }
  # Weighted single-model table: residual df = sum(w) - rank (SPSS
  # frequency weights) instead of lm's cases - rank
  a <- stats::anova(.lr_strip_class(object))
  res <- nrow(a)
  a[res, "Df"] <- fw$df_residual
  a[res, "Mean Sq"] <- a[res, "Sum Sq"] / fw$df_residual
  if (res > 1) {
    terms_rows <- seq_len(res - 1)
    f <- a[terms_rows, "Mean Sq"] / a[res, "Mean Sq"]
    a[terms_rows, "F value"] <- f
    a[terms_rows, "Pr(>F)"] <- stats::pf(f, a[terms_rows, "Df"],
                                         fw$df_residual, lower.tail = FALSE)
  }
  a
}

#' Variance-covariance matrix of a linear_regression model
#'
#' Unweighted models: \code{stats::vcov()} of the \code{lm}. Weighted
#' models: the covariance matrix under SPSS frequency weights (residual
#' variance with \code{sum(w) - rank} df), whose square-rooted diagonal
#' equals the Std.Error column of \code{summary()}.
#'
#' @param object A \code{linear_regression} result (ungrouped, listwise).
#' @param ... Passed to \code{stats::vcov()}.
#' @return A square numeric matrix.
#' @export
#' @method vcov linear_regression
vcov.linear_regression <- function(object, ...) {
  .lr_require_lm(object, "vcov")
  v <- stats::vcov(.lr_strip_class(object), ...)
  fw <- object$spss_weights
  if (!is.null(fw)) {
    # lm's residual variance uses (cases - rank) df, SPSS's (sum(w) - rank);
    # the unscaled (X'WX)^-1 part is identical
    v <- v * object$df.residual / fw$df_residual
  }
  v
}

#' Confidence intervals for linear_regression coefficients
#'
#' t-based intervals for B. For weighted models they use the SPSS
#' frequency-weight standard errors and df (\code{sum(w) - rank}) and
#' equal the CI columns of \code{summary()}.
#'
#' @param object A \code{linear_regression} result (ungrouped, listwise).
#' @param parm Coefficients to compute intervals for (names or indices;
#'   default all).
#' @param level Confidence level (default 0.95).
#' @param ... Not used.
#' @return A matrix with one row per coefficient and columns for the lower
#'   and upper limits.
#' @export
#' @method confint linear_regression
confint.linear_regression <- function(object, parm, level = 0.95, ...) {
  .lr_require_lm(object, "confint")
  fw <- object$spss_weights
  if (is.null(fw)) return(NextMethod())
  cf <- stats::coef(object)
  ses <- sqrt(diag(vcov.linear_regression(object)))
  pnames <- names(ses)
  if (missing(parm)) {
    parm <- pnames
  } else if (is.numeric(parm)) {
    parm <- pnames[parm]
  }
  a <- (1 - level) / 2
  a <- c(a, 1 - a)
  fac <- stats::qt(a, fw$df_residual)
  pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%")
  ci <- array(NA_real_, dim = c(length(parm), 2L), dimnames = list(parm, pct))
  ci[] <- cf[parm] + ses[parm] %o% fac
  ci
}

#' Number of observations of a linear_regression model
#'
#' Unweighted: the number of complete cases. Weighted: the unrounded sum
#' of the frequency weights of the complete cases - SPSS's N (the
#' summary shows it rounded).
#'
#' @param object A \code{linear_regression} result (ungrouped, listwise).
#' @param ... Not used.
#' @return A single number.
#' @export
#' @method nobs linear_regression
nobs.linear_regression <- function(object, ...) {
  .lr_require_lm(object, "nobs")
  fw <- object$spss_weights
  if (!is.null(fw)) return(fw$sum_w)
  NextMethod()
}

#' Update and re-fit a linear_regression model
#'
#' Re-runs \code{\link{linear_regression}} with a modified formula or
#' arguments, e.g. \code{update(model, . ~ . + income)}; \code{step()}
#' works through it. Needs the data by name: a model fitted inside a
#' \code{\%>\%} pipe cannot be updated.
#'
#' @param object A \code{linear_regression} result (also grouped or
#'   pairwise).
#' @param formula. Changes to the formula (see \code{stats::update()}).
#' @param ... Further arguments of \code{linear_regression()} to change.
#' @param evaluate If \code{FALSE}, return the updated call.
#' @return A new \code{linear_regression} result (or the call).
#' @export
#' @method update linear_regression
update.linear_regression <- function(object, formula., ..., evaluate = TRUE) {
  .check_regression_update(object, "linear_regression")
  NextMethod()
}

#' Coefficients of a linear_regression model
#'
#' The unstandardized coefficients B. Also available for pairwise results
#' (taken from the coefficients table); grouped results hold one model per
#' group in \code{$groups}.
#'
#' @param object A \code{linear_regression} result.
#' @param ... Not used.
#' @return A named numeric vector.
#' @export
#' @method coef linear_regression
coef.linear_regression <- function(object, ...) {
  if (isTRUE(object$is_grouped)) .lr_require_lm(object, "coef")
  if (!inherits(object, "lm")) {
    return(stats::setNames(object$coef_table$B, object$coef_table$Term))
  }
  NextMethod()
}

#' Residuals of a linear_regression model
#'
#' Dispatches to \code{stats::residuals()} for the fitted \code{lm};
#' grouped and pairwise results raise an informative error.
#'
#' @param object A \code{linear_regression} result (ungrouped, listwise).
#' @param ... Passed to the \code{lm} method.
#' @return A numeric vector.
#' @export
#' @method residuals linear_regression
residuals.linear_regression <- function(object, ...) {
  .lr_require_lm(object, "residuals")
  NextMethod()
}

#' Fitted values of a linear_regression model
#'
#' Dispatches to \code{stats::fitted()} for the fitted \code{lm};
#' grouped and pairwise results raise an informative error.
#'
#' @param object A \code{linear_regression} result (ungrouped, listwise).
#' @param ... Passed to the \code{lm} method.
#' @return A numeric vector.
#' @export
#' @method fitted linear_regression
fitted.linear_regression <- function(object, ...) {
  .lr_require_lm(object, "fitted")
  NextMethod()
}

#' Residual degrees of freedom of a linear_regression model
#'
#' Weighted models: \code{sum(w) - rank} (SPSS frequency weights,
#' non-integer); unweighted: the \code{lm} value.
#'
#' @param object A \code{linear_regression} result (ungrouped, listwise).
#' @param ... Not used.
#' @return A single number.
#' @export
#' @method df.residual linear_regression
df.residual.linear_regression <- function(object, ...) {
  .lr_require_lm(object, "df.residual")
  fw <- object$spss_weights
  if (!is.null(fw)) return(fw$df_residual)
  NextMethod()
}


# ============================================================================
# SHARED REGRESSION HELPERS (linear, logistic, marginal effects)
# ============================================================================

#' One-line text form of a model formula
#'
#' deparse() splits formulas longer than ~60 characters into several
#' strings; pasted into sprintf()/cat() that printed the title twice and
#' made print_info_section() abort on a length-2 value.
#' @noRd
.formula_label <- function(f) {
  if (is.null(f)) return("")
  paste(trimws(deparse(f, width.cutoff = 500L)), collapse = " ")
}

#' The call stored on a regression result
#'
#' lm()/glm() recorded their internal call (formula = formula, data =
#' data_complete, weights = .wt), so update()/step() failed with "object
#' 'data_complete' not found" and printed internal names. The stored call
#' is now the user's own call in formula form (dependent=/predictors= are
#' replaced by the equivalent formula), which update() re-evaluates.
#' @noRd
.regression_call <- function(cl, model_formula) {
  cl$dependent <- NULL
  cl$predictors <- NULL
  cl$formula <- model_formula
  cl
}

#' Guard for update(): the stored call must be re-evaluable
#' @noRd
.check_regression_update <- function(object, cls, call = rlang::caller_env()) {
  if (!is.null(object$group_values) && !isTRUE(object$is_grouped)) {
    cli_abort(c(
      "{.fn update} is not available for a single group of a grouped {.cls {cls}}.",
      i = "Update the grouped result instead; it refits every group."
    ), call = call)
  }
  cl <- object$call
  if (is.null(cl)) {
    cli_abort("This {.cls {cls}} result stores no call to update.", call = call)
  }
  if (identical(cl$data, quote(.))) {
    cli_abort(c(
      "{.fn update} cannot re-use the data: the model was fitted inside a pipe ({.code %>%}), where the data has no name.",
      i = "Fit the model with the data by name, e.g. {.code {cls}(my_data, y ~ x)}, then {.fn update} works."
    ), call = call)
  }
  invisible(TRUE)
}

#' Backtick non-syntactic variable names for formula text
#' @noRd
.bt_name <- function(x) {
  ifelse(make.names(x) == x, x,
         paste0("`", gsub("`", "\\\\`", x), "`"))
}

#' Resolve the weights argument of the regression functions
#'
#' A bare column name (or string) is looked up in the data; any other
#' expression (sampling_weight * 2, survey_data$w) is evaluated with the
#' data as mask and stored as a column named by its text, so grouped fits
#' and the printed "Weights" line keep working - rlang::as_name() used to
#' abort with "Can't convert a call to a string".
#'
#' @return list(data, name, vec); name/vec NULL when unweighted
#' @noRd
.regression_weights <- function(data, weights_quo, call = rlang::caller_env()) {
  if (rlang::quo_is_null(weights_quo)) {
    return(list(data = data, name = NULL, vec = NULL))
  }
  expr <- rlang::quo_get_expr(weights_quo)
  if (rlang::is_symbol(expr) || rlang::is_string(expr)) {
    name <- rlang::as_name(weights_quo)
    if (!name %in% names(data)) {
      cli_abort("Weight variable {.var {name}} not found in data.", call = call)
    }
    vec <- data[[name]]
  } else if (rlang::is_call(expr, c("all_of", "any_of"))) {
    pos <- tidyselect::eval_select(weights_quo, data)
    if (length(pos) != 1) {
      cli_abort("{.arg weights} must select exactly one variable.", call = call)
    }
    name <- names(pos)
    vec <- data[[name]]
  } else {
    name <- paste(trimws(deparse(expr, width.cutoff = 500L)), collapse = " ")
    vec <- tryCatch(
      rlang::eval_tidy(weights_quo, data),
      error = function(e) {
        cli_abort(c(
          "Could not evaluate {.arg weights} = {.code {name}}.",
          x = "{conditionMessage(e)}"
        ), call = call)
      }
    )
    if (length(vec) == 1L) vec <- rep(vec, nrow(data))
    if (length(vec) != nrow(data)) {
      cli_abort(
        "{.arg weights} = {.code {name}} gives {length(vec)} value{?s}, but the data have {nrow(data)} rows.",
        call = call
      )
    }
  }
  # Bare numbers in the vector AND the column (grouped fits re-read it):
  # SPSS weights with NA fail every comparison (see .plain_numeric)
  vec <- .plain_numeric(vec)
  .check_weights(vec, name, call = call)
  data[[name]] <- vec
  list(data = data, name = name, vec = vec)
}

#' Build and validate the model formula of the regression functions
#'
#' One place for both interfaces:
#' - formula: a transformed outcome (log(income) ~ age) is allowed when
#'   allow_lhs_call = TRUE (linear) - its raw variables are validated, not
#'   the function names ("Variable(s) not found in data: log."); `y ~ .`
#'   expands to all other columns except the weights and grouping
#'   variables; intercept-only models and an outcome that is also a
#'   predictor are refused with a clear message.
#' - dependent/predictors: names are backticked (non-syntactic names such
#'   as "my var" failed to parse), and the outcome, weights and grouping
#'   variables are dropped from a tidyselect predictor selection such as
#'   where(is.numeric) with a message.
#'
#' @return list(formula, dep_name (label), dep_vars (raw variables of the
#'   outcome), pred_names (raw predictor variables))
#' @noRd
.build_regression_formula <- function(data, formula, dep_quo, pred_quo,
                                      weight_name = NULL,
                                      allow_lhs_call = TRUE,
                                      env = rlang::caller_env(2),
                                      call = rlang::caller_env()) {
  group_vars <- if (inherits(data, "grouped_df")) dplyr::group_vars(data) else character(0)

  if (!is.null(formula)) {
    if (!inherits(formula, "formula")) {
      cli_abort("{.arg formula} must be a formula object (e.g., {.code y ~ x1 + x2}).",
                call = call)
    }
    if (length(formula) != 3L) {
      cli_abort("{.arg formula} needs a dependent variable on the left: {.code y ~ x1 + x2}.",
                call = call)
    }
    lhs <- formula[[2]]
    lhs_text <- paste(trimws(deparse(lhs, width.cutoff = 500L)), collapse = " ")
    if (!is.name(lhs) && (!allow_lhs_call || rlang::is_call(lhs, "cbind") ||
                          length(all.vars(lhs)) != 1L)) {
      cli_abort(c(
        "The dependent variable must be a single variable, not {.code {lhs_text}}.",
        i = "Create the variable in the data first, then use its name."
      ), call = call)
    }
    dep_vars <- all.vars(lhs)
    dep_name <- if (is.name(lhs)) as.character(lhs) else lhs_text

    if ("." %in% all.vars(formula[[3]])) {
      cols <- setdiff(names(data), c(dep_vars, weight_name, group_vars))
      tt <- stats::terms(formula,
                         data = as.data.frame(data)[0, cols, drop = FALSE])
      labels <- attr(tt, "term.labels")
      if (length(labels) > 0) {
        formula <- stats::reformulate(labels, response = lhs,
                                      intercept = attr(tt, "intercept") == 1L,
                                      env = environment(formula))
      }
    }
    pred_names <- all.vars(formula[[3]])
  } else {
    if (rlang::quo_is_null(dep_quo)) {
      cli_abort("Either {.arg formula} or {.arg dependent} must be specified.",
                call = call)
    }
    dep_name <- tryCatch(rlang::as_name(dep_quo), error = function(e) {
      cli_abort(c(
        "{.arg dependent} must be a single variable name.",
        i = "Use the formula interface for a transformed outcome, e.g. {.code log(y) ~ x}."
      ), call = call)
    })
    dep_vars <- dep_name
    pred_names <- names(tidyselect::eval_select(pred_quo, data))
    dropped <- intersect(pred_names, c(dep_name, weight_name, group_vars))
    if (length(dropped) > 0) {
      cli_inform(c(
        i = "Not used as predictor{?s}: {.var {dropped}} (dependent, weights or grouping variable)."
      ))
      pred_names <- setdiff(pred_names, dropped)
    }
    if (length(pred_names) == 0) {
      cli_abort("At least one predictor variable must be specified.", call = call)
    }
    formula <- stats::as.formula(
      paste(.bt_name(dep_name), "~", paste(.bt_name(pred_names), collapse = " + ")),
      env = env
    )
  }

  missing_vars <- setdiff(c(dep_vars, pred_names), names(data))
  if (length(missing_vars) > 0) {
    cli_abort("Variable{?s} not found in data: {.var {missing_vars}}.", call = call)
  }
  if (length(attr(stats::terms(formula), "term.labels")) == 0) {
    ftxt <- .formula_label(formula)
    cli_abort(c(
      "The model has no predictor: {.code {ftxt}}.",
      i = "A regression needs at least one predictor, e.g. {.code y ~ x}."
    ), call = call)
  }
  both <- intersect(dep_vars, pred_names)
  if (length(both) > 0) {
    cli_abort("{.var {both}} cannot be both the dependent variable and a predictor.",
              call = call)
  }

  list(formula = formula, dep_name = dep_name, dep_vars = dep_vars,
       pred_names = pred_names)
}

#' Display a (possibly weighted) sample size as a whole number
#'
#' sprintf("%d") needs an R integer: a weighted N above 2^31 (expansion
#' weights) failed with "invalid format '%d'", and a non-integer double
#' failed as well. Rounds for display only (Charter 5.1).
#' @noRd
.fmt_n <- function(n) {
  out <- formatC(round(as.numeric(n)), format = "f", digits = 0)
  out[is.na(n)] <- "NA"
  out
}

#' Fixed-decimal display without a negative zero
#'
#' For bounded fit statistics (R2, adjusted R2, pseudo R2): -0.0002 shows
#' as "0.000", not "-0.000". NA shows as "".
#' @noRd
.fmt_fixed <- function(x, digits = 3) {
  x <- as.numeric(x)
  out <- formatC(x, format = "f", digits = digits)
  out <- sub("^-(0(\\.0*)?)$", "\\1", out)
  out[is.na(x)] <- ""
  out
}

#' Display estimates: fixed decimals, scientific where fixed hides them
#'
#' A per-unit effect such as income's B (0.00062) printed as "0.000" and a
#' separation estimate (Exp(B) ~ 4e30) as a 31-digit number that broke the
#' table. Values that would round to zero at `digits` decimals, and values
#' of 1e10 or more, are shown in scientific notation with `digits`
#' significant digits (as SPSS shows "6.234E-4"). NA shows as "".
#' @noRd
.fmt_est <- function(x, digits = 3) {
  x <- as.numeric(x)
  out <- formatC(x, format = "f", digits = digits)
  sci <- !is.na(x) & is.finite(x) & x != 0 &
    (abs(x) < 0.5 * 10^(-digits) | abs(x) >= 1e10)
  out[sci] <- formatC(x[sci], format = "e", digits = max(digits - 1L, 1L))
  out[is.na(x)] <- ""
  out
}

#' Display a ratio estimate with its interval (odds ratios)
#'
#' Row-wise: adds decimals (up to digits + 6) until the displayed limits
#' differ - an odds ratio per EUR of income printed as 1.001 [1.001,
#' 1.001]. Returns a character matrix with columns est, lower, upper.
#' @noRd
.fmt_ratio_ci <- function(est, lower, upper, digits = 3) {
  out <- matrix("", nrow = length(est), ncol = 3)
  for (i in seq_along(est)) {
    d <- digits
    lo <- lower[i]
    hi <- upper[i]
    if (!is.na(lo) && !is.na(hi) && is.finite(lo) && is.finite(hi) &&
        lo != hi && max(abs(c(lo, hi))) < 1e10) {
      while (d < digits + 6 &&
             formatC(lo, format = "f", digits = d) ==
             formatC(hi, format = "f", digits = d)) {
        d <- d + 1L
      }
    }
    out[i, ] <- .fmt_est(c(est[i], lo, hi), d)
  }
  out
}

#' Confidence-level label for table headers ("95%")
#' @noRd
.ci_label <- function(conf.level) {
  paste0(format(100 * (conf.level %||% 0.95), trim = TRUE,
                drop0trailing = TRUE), "%")
}

#' Two-column statistic/value block (model summaries)
#'
#' Width follows the content (pad_utf8), replacing fixed %-25s/%-30s
#' layouts.
#' @noRd
.print_kv_block <- function(title, labels, values) {
  cat("  ", title, "\n", sep = "")
  lw <- max(nchar(labels, type = "width"))
  vw <- max(nchar(values, type = "width"), 1L)
  border <- paste0("  ", strrep("-", lw + vw + 3L), "\n")
  cat(border)
  for (i in seq_along(labels)) {
    cat("  ", pad_utf8(labels[i], lw), "   ",
        pad_utf8(values[i], vw, align = "right"), "\n", sep = "")
  }
  cat(border)
  invisible(NULL)
}

#' Note on labelled predictors entered with their numeric codes (REG-19)
#' @noRd
.print_labelled_note <- function(vars, procedure = "REGRESSION") {
  if (length(vars) == 0) return(invisible(NULL))
  cat(sprintf(
    "- Note: labelled predictor%s %s entered with %s numeric codes, as SPSS %s does.\n  Use to_label() first to enter %s as dummy-coded categories.\n",
    if (length(vars) > 1) "s" else "",
    paste(vars, collapse = ", "),
    if (length(vars) > 1) "their" else "its",
    procedure,
    if (length(vars) > 1) "them" else "it"
  ))
  invisible(NULL)
}

#' Labelled (haven) predictors among the model variables
#' @noRd
.labelled_predictors <- function(data, pred_names) {
  pred_names[vapply(pred_names, function(v) {
    x <- data[[v]]
    inherits(x, "haven_labelled") ||
      (is.numeric(x) && !is.null(attr(x, "labels", exact = TRUE)))
  }, logical(1))]
}

#' Abort for too few complete cases, naming the cause
#'
#' Replaces the bare "Insufficient observations for the number of
#' predictors": an all-missing variable is named, otherwise the counts are
#' given. Classed so grouped fits can skip the group (SPSS SPLIT FILE).
#' @noRd
.abort_insufficient_cases <- function(data, all_vars, n, n_pred,
                                      call = rlang::caller_env()) {
  if (n == 0) {
    empty <- all_vars[vapply(all_vars, function(v) all(is.na(data[[v]])),
                             logical(1))]
    if (length(empty) > 0) {
      cli_abort(
        "No complete cases: {.var {empty}} {?has/have} no non-missing values.",
        class = "mariposa_degenerate_fit", call = call
      )
    }
    cli_abort(
      "No complete cases: no case has valid values on all of {.var {all_vars}}.",
      class = "mariposa_degenerate_fit", call = call
    )
  }
  cli_abort(c(
    "Only {n} complete case{?s} for {n_pred} predictor term{?s}.",
    i = "The model needs at least {n_pred + 2} complete cases."
  ), class = "mariposa_degenerate_fit", call = call)
}

#' Fit one model per group, skipping groups that cannot be fitted
#'
#' SPSS SPLIT FILE reports a split it cannot analyse and continues with the
#' others; one small group used to abort the whole grouped regression
#' without saying which group. Failing groups become a warning naming the
#' group and the reason, and are listed in $skipped.
#'
#' @param group_split,group_keys From dplyr::group_split()/group_keys()
#' @param fit_fun function(group_data) returning the fitted result
#' @return list(results = fitted groups (with $group_values), skipped =
#'   list of list(group_values, reason))
#' @noRd
.fit_groups <- function(group_split, group_keys, fit_fun,
                        call = rlang::caller_env()) {
  results <- list()
  skipped <- list()
  for (i in seq_along(group_split)) {
    gv <- as.list(group_keys[i, , drop = FALSE])
    gv <- lapply(gv, function(v) if (is.factor(v)) as.character(v) else v)
    res <- tryCatch(fit_fun(group_split[[i]]), error = function(e) e)
    if (inherits(res, "error")) {
      label <- .format_group_label(gv)
      reason <- cli::ansi_strip(strsplit(conditionMessage(res), "\n")[[1]][1])
      cli_warn(c(
        "Group {label} skipped: no model could be fitted.",
        x = "{reason}"
      ))
      skipped[[length(skipped) + 1]] <- list(group_values = gv,
                                             reason = reason)
      next
    }
    res$group_values <- gv
    results[[length(results) + 1]] <- res
  }
  if (length(results) == 0) {
    reasons <- unique(vapply(skipped, `[[`, character(1), "reason"))
    # literal text in cli bullets: escape glue braces
    reasons <- gsub("}", "}}", gsub("{", "{{", reasons, fixed = TRUE),
                    fixed = TRUE)
    cli_abort(c(
      "No group could be fitted.",
      stats::setNames(reasons, rep("x", length(reasons)))
    ), call = call)
  }
  list(results = results, skipped = skipped)
}

#' Compact lines for skipped groups ("group: not computed (reason)")
#' @noRd
.print_skipped_groups <- function(skipped, verbose = FALSE) {
  for (s in skipped) {
    if (verbose) {
      cat("\n")
      print_group_header(s$group_values)
      cat(sprintf("  Model not computed: %s\n", s$reason))
    } else {
      cat(sprintf("  %s: not computed (%s)\n",
                  .format_group_label(s$group_values), s$reason))
    }
  }
  invisible(NULL)
}
