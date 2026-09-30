#' Run a Logistic Regression
#'
#' @description
#' \code{logistic_regression()} performs binary logistic regression with
#' SPSS-compatible output. Wraps \code{stats::glm(family = binomial)} and adds
#' odds ratios, pseudo R-squared measures, classification table, and model tests
#' matching SPSS LOGISTIC REGRESSION output.
#'
#' Supports two interface styles:
#' \itemize{
#'   \item \strong{Formula interface:} \code{logistic_regression(data, high_satisfaction ~ age + income)}
#'   \item \strong{SPSS-style:} \code{logistic_regression(data, dependent = high_satisfaction, predictors = c(age, income))}
#' }
#'
#' @param data Your survey data (a data frame or tibble). If grouped
#'   (via \code{dplyr::group_by()}), separate regressions are run for each group.
#' @param formula A formula specifying the model (e.g., \code{y ~ x1 + x2}).
#'   If provided, \code{dependent} and \code{predictors} are ignored.
#' @param dependent The dependent variable (unquoted). Used with \code{predictors}
#'   when no formula is given. Must have exactly two distinct values
#'   (e.g. 0/1, 1/2, a two-level factor); see Technical Details.
#' @param predictors Predictor variable(s) (unquoted, supports tidyselect).
#'   Used with \code{dependent} when no formula is given. The dependent,
#'   weights and grouping variables are never used as predictors (a
#'   selection such as \code{where(is.numeric)} drops them with a message).
#'   Character predictors are entered as factors.
#' @param weights Optional survey weights (unquoted variable name, or an
#'   expression such as \code{sampling_weight * 2}). When specified,
#'   weighted maximum likelihood estimation is used, matching SPSS WEIGHT BY
#'   behavior.
#' @param conf.level Confidence level for odds ratio intervals (default 0.95).
#' @param factors How factor predictors are entered into the model:
#'   \code{"dummy"} (default, matches base R \code{glm()}) expands a factor
#'   with \code{L} levels into \code{L - 1} contrasts; \code{"numeric"}
#'   silently coerces factor levels to their integer codes, matching SPSS
#'   \code{LOGISTIC REGRESSION} default behavior when no \code{/CATEGORICAL}
#'   subcommand is given. The "numeric" mode emits a one-line
#'   \code{cli::cli_inform()} listing the coerced variables. Note that for
#'   \emph{ordered} factors, "dummy" applies R's default polynomial
#'   contrasts (terms suffixed \code{.L}, \code{.Q}, \code{.C}), not
#'   treatment dummies; convert with \code{factor(x, ordered = FALSE)}
#'   first if you want dummy coding.
#'
#' @return For ungrouped data, an object of class
#'   \code{c("logistic_regression", "glm", "lm")} — \strong{the fitted
#'   \code{glm} itself}, with mariposa-specific slots attached:
#' \describe{
#'   \item{coef_table}{Tibble with B, S.E., Wald, df, Sig., Exp(B), CI_lower, CI_upper}
#'   \item{model_summary}{List with minus2LL, cox_snell_r2, nagelkerke_r2, mcfadden_r2}
#'   \item{omnibus_test}{List with chi_sq, df, p for overall model test}
#'   \item{classification}{List with table, overall_pct, pct_correct_0, pct_correct_1}
#'   \item{hosmer_lemeshow}{List with chi_sq, df, p (goodness-of-fit test)}
#'   \item{dv_encoding}{Tibble Original -> Internal (0/1) of the outcome
#'     categories (SPSS "Dependent Variable Encoding")}
#'   \item{n}{Sample size (listwise complete cases; weighted N when weighted)}
#'   \item{formula, dependent, predictor_names, weighted, weight_name, is_grouped, conf.level}{Call metadata.}
#' }
#'   Because the object inherits from \code{"glm"}, all standard
#'   generics (\code{predict()}, \code{anova()}, \code{vcov()},
#'   \code{confint()}, \code{residuals()}, \code{fitted()},
#'   \code{coef()}, \code{broom::tidy()}, \code{broom::glance()},
#'   \code{broom::augment()}) dispatch natively without unwrapping.
#'   \code{summary()} returns the SPSS-style mariposa summary; for
#'   the raw glm summary use \code{stats::summary.glm()} on the same
#'   object. \code{confint()} returns Wald intervals on the log-odds
#'   scale (the SPSS method; \code{exp(confint(model))} gives the
#'   "C.I. for EXP(B)" of the summary), profile-likelihood intervals via
#'   \code{confint(model, method = "profile")}; see
#'   \code{\link{confint.logistic_regression}}.
#'
#'   For grouped data, returns a list of class \code{"logistic_regression"}
#'   with \code{$groups} holding one fitted glm-inheriting result per group.
#'
#' @details
#' ## Understanding the Results
#'
#' The output includes five sections matching SPSS LOGISTIC REGRESSION output:
#' \itemize{
#'   \item \strong{Omnibus Test}: Tests whether the model as a whole is significant.
#'     A significant chi-square means the model predicts better than chance.
#'   \item \strong{Model Summary}: -2 Log Likelihood and pseudo R-squared values.
#'     Lower -2LL = better fit. Higher R-squared = more variance explained.
#'   \item \strong{Hosmer-Lemeshow Test}: Goodness-of-fit test. A non-significant
#'     result (p > 0.05) means the model fits the data well.
#'   \item \strong{Classification Table}: How well the model classifies cases.
#'     Shows percentage correctly predicted for each group and overall.
#'   \item \strong{Coefficients}: B, Wald test, odds ratios (Exp(B)), and CIs.
#' }
#'
#' Interpreting odds ratios (Exp(B)):
#' \itemize{
#'   \item \strong{Exp(B) > 1}: Predictor increases the odds of the outcome
#'   \item \strong{Exp(B) < 1}: Predictor decreases the odds of the outcome
#'   \item \strong{Exp(B) = 1}: Predictor has no effect on the odds
#' }
#'
#' ## When to Use This
#'
#' Use \code{logistic_regression()} when:
#' \itemize{
#'   \item Your dependent variable is binary (yes/no, 0/1, pass/fail)
#'   \item You want to predict group membership from one or more predictors
#'   \item You need odds ratios to interpret predictor effects
#' }
#'
#' For continuous outcomes, use \code{\link{linear_regression}} instead.
#'
#' ## Technical Details
#'
#' \strong{Dependent Variable}: Any variable with exactly two distinct
#' observed values, coded internally as 0/1 like SPSS LOGISTIC REGRESSION
#' does: for numeric and labelled variables the lower value becomes 0 and
#' the higher 1 (so 1/2 and 0/1 codings give the same model); for factors
#' unused levels are dropped and the first remaining level becomes 0;
#' character values are ordered alphabetically; logicals code
#' \code{FALSE} = 0, \code{TRUE} = 1. The model predicts the probability of
#' the category coded 1 - the compact print names it
#' (\code{[P(y = category)]}), and \code{summary()} shows the SPSS
#' "Dependent Variable Encoding" table (also stored as
#' \code{$dv_encoding}). A grouped analysis uses one encoding for all
#' groups.
#'
#' \strong{Missing Data}: Listwise deletion is used (matching SPSS LOGISTIC
#' REGRESSION default behavior).
#'
#' \strong{Weights}: When weights are specified, they are treated as frequency
#' weights (matching SPSS WEIGHT BY behavior).
#'
#' \strong{Pseudo R-squared}: Three measures are reported:
#' \itemize{
#'   \item Cox & Snell R-squared (bounded below 1)
#'   \item Nagelkerke R-squared (adjusted to reach 1)
#'   \item McFadden R-squared (1 - LL_model/LL_null)
#' }
#'
#' \strong{Factor Predictors}: By default (\code{factors = "dummy"}),
#' factor predictors are expanded into \code{L - 1} contrasts via
#' R's \code{stats::model.matrix()}, matching base R \code{glm()}: unordered
#' factors get treatment (dummy) contrasts against the first level, while
#' \emph{ordered} factors get R's default polynomial contrasts
#' (\code{.L}/\code{.Q}/\code{.C} terms). Pass \code{factors = "numeric"}
#' to silently coerce factor levels to their integer codes (SPSS
#' \code{LOGISTIC REGRESSION} default without an explicit
#' \code{/CATEGORICAL} subcommand).
#'
#' \strong{Grouped Analysis}: When \code{data} is grouped via
#' \code{dplyr::group_by()}, a separate regression is run for each group
#' (matching SPSS SPLIT FILE BY).
#'
#' @examples
#' library(dplyr)
#' data(survey_data)
#'
#' # Create binary DV
#' survey_data$high_satisfaction <- ifelse(survey_data$life_satisfaction >= 4, 1, 0)
#'
#' # Bivariate logistic regression
#' logistic_regression(survey_data, high_satisfaction ~ age)
#'
#' # Multiple logistic regression
#' logistic_regression(survey_data, high_satisfaction ~ age + income + education)
#'
#' # SPSS-style interface
#' logistic_regression(survey_data,
#'                     dependent = high_satisfaction,
#'                     predictors = c(age, income))
#'
#' # Weighted logistic regression
#' logistic_regression(survey_data, high_satisfaction ~ age,
#'                     weights = sampling_weight)
#'
#' # Grouped by region
#' survey_data |>
#'   dplyr::group_by(region) |>
#'   logistic_regression(high_satisfaction ~ age)
#'
#' # Factor predictors: dummy-coding (default, matches base R glm())
#' logistic_regression(survey_data, high_satisfaction ~ age + education)
#'
#' # Factor predictors: SPSS-style ordinal-as-scale
#' logistic_regression(survey_data, high_satisfaction ~ age + education,
#'                     factors = "numeric")
#'
#' # --- Three-layer output ---
#' result <- logistic_regression(survey_data, high_satisfaction ~ age + income)
#' result                                    # compact one-line overview
#' summary(result)                           # full detailed SPSS-style output
#' summary(result, classification = FALSE)   # hide classification table
#'
#' @seealso
#' \code{\link{linear_regression}} for continuous outcome variables.
#'
#' \code{\link{chi_square}} for testing associations between categorical variables.
#'
#' \code{\link{summary.logistic_regression}} for detailed output with toggleable sections.
#'
#' @family regression
#' @export
logistic_regression <- function(data, formula = NULL,
                                dependent = NULL, predictors = NULL,
                                weights = NULL,
                                conf.level = 0.95,
                                factors = c("dummy", "numeric")) {

  # ============================================================================
  # INPUT VALIDATION & FORMULA CONSTRUCTION
  # ============================================================================

  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame or tibble.")
  }

  factors <- match.arg(factors)

  # Process weights (a column name or an expression such as w * 2)
  wi <- .regression_weights(data, rlang::enquo(weights))
  data <- wi$data
  weight_name <- wi$name
  weights_vec <- wi$vec
  has_weights <- !is.null(weight_name)

  # Build and validate the formula (both interfaces). The outcome must be a
  # plain variable: its two values define the SPSS encoding.
  fb <- .build_regression_formula(
    data, formula, rlang::enquo(dependent), rlang::enquo(predictors),
    weight_name = weight_name, allow_lhs_call = FALSE,
    env = parent.frame()
  )
  model_formula <- fb$formula
  dep_name <- fb$dep_name
  pred_names <- fb$pred_names
  all_vars <- c(dep_name, pred_names)

  # Outcome encoding (SPSS "Dependent Variable Encoding"), fixed once on
  # the cases in the analysis so every group models the same category
  is_grouped <- inherits(data, "grouped_df")

  in_analysis <- stats::complete.cases(data[, all_vars, drop = FALSE])
  if (has_weights) in_analysis <- in_analysis & !is.na(weights_vec)
  n_in <- sum(in_analysis)
  if (n_in == 0 || (!is_grouped && n_in < length(pred_names) + 2)) {
    .abort_insufficient_cases(data, all_vars, n_in, length(pred_names))
  }
  dv_encoding <- .logistic_dv_encoding(data[[dep_name]][in_analysis], dep_name)

  # ============================================================================
  # GROUPED ANALYSIS
  # ============================================================================

  if (is_grouped) {
    group_vars <- dplyr::group_vars(data)
    group_split <- dplyr::group_split(data)
    group_keys <- dplyr::group_keys(data)

    # A group that cannot be fitted is skipped with a warning (SPSS SPLIT
    # FILE carries on with the other splits)
    fits <- .fit_groups(group_split, group_keys, function(grp_data) {
      grp_weights <- if (has_weights) grp_data[[weight_name]] else NULL
      result <- .glm_core(grp_data, model_formula, dep_name, pred_names,
                          grp_weights, conf.level, factors, dv_encoding)
      # Each group result IS a glm — tag it so per-group predict/anova/broom
      # generics dispatch natively.
      if (inherits(result, "glm")) {
        class(result) <- c("logistic_regression", class(result))
      } else {
        class(result) <- "logistic_regression"
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
        is_grouped = TRUE,
        group_vars = group_vars,
        conf.level = conf.level,
        dv_encoding = .logistic_encoding_table(dv_encoding),
        dv_labels = dv_encoding$short
      ),
      class = "logistic_regression"
    )
  } else {
    result <- .glm_core(data, model_formula, dep_name, pred_names,
                        weights_vec, conf.level, factors, dv_encoding)
    # result IS the fitted glm (with mariposa slots attached).
    result$formula <- model_formula
    result$dependent <- dep_name
    result$predictor_names <- pred_names
    result$weighted <- has_weights
    result$weight_name <- weight_name
    result$is_grouped <- FALSE
    result$conf.level <- conf.level
    if (inherits(result, "glm")) {
      class(result) <- c("logistic_regression", class(result))
    } else {
      class(result) <- "logistic_regression"
    }
    result
  }
}


# ============================================================================
# CORE COMPUTATION
# ============================================================================

# Fit a glm while muffling ONLY the expected fractional-frequency-weight
# warning. suppressWarnings() would also swallow separation and
# non-convergence warnings ("fitted probabilities numerically 0 or 1
# occurred", "algorithm did not converge") - exactly the pathologies a
# survey analyst must see.
# stats raises the warning via gettextf(), so it arrives translated (e.g.
# "Nicht-ganzzahlige #Erfolge in einem binomial-GLM" under a German
# locale); the target text is rebuilt through the same R-stats catalog so
# the match holds in every locale.
.glm_quiet_weights <- function(expr) {
  target <- gettextf("non-integer #successes in a %s glm!", "binomial",
                     domain = "R-stats")
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (identical(conditionMessage(w), target)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}


#' Core logistic regression computation
#' @noRd
.glm_core <- function(data, formula, dep_name, pred_names, weights_vec,
                      conf.level, factors = "dummy", dv_encoding = NULL) {

  all_vars <- c(dep_name, pred_names)

  # Listwise deletion
  complete <- stats::complete.cases(data[, all_vars, drop = FALSE])
  if (!is.null(weights_vec)) {
    complete <- complete & !is.na(weights_vec)
    weights_vec <- weights_vec[complete]
  }
  data_complete <- data[complete, , drop = FALSE]
  n_actual <- nrow(data_complete)

  if (n_actual < length(pred_names) + 2) {
    .abort_insufficient_cases(data, all_vars, n_actual, length(pred_names))
  }

  # Character predictors are categorical: enter them as factors (as glm()
  # would), so the numeric mode and the AMEs treat them like factors
  for (v in pred_names) {
    if (is.character(data_complete[[v]])) {
      data_complete[[v]] <- factor(data_complete[[v]])
    }
  }

  # Factor predictor handling — see @param factors documentation.
  # "dummy" (default): let stats::glm() expand factors into L-1 contrasts.
  # "numeric": coerce factor levels to integer codes (SPSS LOGISTIC default).
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

  # Binary DV -> internal 0/1 (SPSS "Dependent Variable Encoding"). The
  # encoding is fixed once for the whole data set (see logistic_regression)
  # so that every group of a grouped analysis models the same category.
  if (is.null(dv_encoding)) {
    dv_encoding <- .logistic_dv_encoding(data_complete[[dep_name]], dep_name)
  }
  y01 <- .logistic_apply_encoding(data_complete[[dep_name]], dv_encoding)
  if (length(unique(y01)) < 2) {
    cli_abort(c(
      "Dependent variable {.var {dep_name}} has only one observed value ({.val {dv_encoding$short[unique(y01) + 1]}}) here.",
      i = "Logistic regression needs cases in both outcome categories."
    ), class = "mariposa_degenerate_fit")
  }
  data_complete[[dep_name]] <- y01

  # ============================================================================
  # FIT MODEL
  # ============================================================================

  # Suppress non-integer warning when using frequency weights with binomial GLM
  # This is expected behavior matching SPSS WEIGHT BY
  if (!is.null(weights_vec)) {
    data_complete$.wt <- weights_vec
    model <- .glm_quiet_weights(
      stats::glm(formula, data = data_complete, family = stats::binomial(),
                 weights = .wt)
    )
  } else {
    model <- stats::glm(formula, data = data_complete, family = stats::binomial())
  }

  # ============================================================================
  # SPSS-COMPATIBLE STATISTICS
  # ============================================================================

  # N: SPSS uses weighted N when WEIGHT BY is active.
  # Per Validation Charter §5.1: use UNROUNDED sum(w) in formulas
  # (Cox & Snell denominator, classification percentages), round only for
  # the displayed N. Fixed in 0.6.4.
  if (!is.null(weights_vec)) {
    sw <- sum(weights_vec)
    n_internal <- sw                # unrounded; for pseudo-R^2 formulas
    n_report   <- round(sw)         # rounded; for $n display
  } else {
    n_internal <- n_actual
    n_report   <- n_actual
  }

  # -2 Log Likelihood — taken from the deviance, NOT stats::logLik().
  # For a 0/1 response the residual deviance equals -2*LL exactly (the
  # saturated log-likelihood is 0), and unlike logLik() — whose binomial
  # aic() rounds prior weights via dbinom(round(m*y), round(m), mu) — the
  # deviance honors fractional frequency weights unrounded (Charter §5.1).
  # model$null.deviance is the intercept-only fit on the same data/weights.
  minus2LL_model <- model$deviance
  minus2LL_null <- model$null.deviance

  # Log likelihoods
  LL_model <- -minus2LL_model / 2
  LL_null <- -minus2LL_null / 2

  # Omnibus test (Model chi-square)
  omnibus_chi_sq <- minus2LL_null - minus2LL_model
  # df = difference in estimated parameters between model and null model.
  # Counted on the fitted models (not pred_names): dummy-expanded factors
  # and interaction terms each consume one df.
  omnibus_df <- model$df.null - model$df.residual
  omnibus_p <- stats::pchisq(omnibus_chi_sq, df = omnibus_df, lower.tail = FALSE)

  # Pseudo R-squared measures (use unrounded n_internal per Charter §5.1)
  # Cox & Snell R²: 1 - (L0/L1)^(2/n)
  cox_snell <- 1 - exp((minus2LL_model - minus2LL_null) / n_internal)

  # Max Cox & Snell (for Nagelkerke adjustment)
  cox_snell_max <- 1 - exp(minus2LL_null / n_internal * (-1))

  # Nagelkerke R²: Cox & Snell / max(Cox & Snell)
  nagelkerke <- cox_snell / cox_snell_max

  # McFadden R²: 1 - (LL_model / LL_null)
  mcfadden <- 1 - (LL_model / LL_null)

  model_stats <- list(
    minus2LL = minus2LL_model,
    cox_snell_r2 = cox_snell,
    nagelkerke_r2 = nagelkerke,
    mcfadden_r2 = mcfadden
  )

  omnibus <- list(
    chi_sq = omnibus_chi_sq,
    df = omnibus_df,
    p = omnibus_p
  )

  # ============================================================================
  # COEFFICIENTS TABLE
  # ============================================================================

  coef_summary <- summary(model)$coefficients
  term_names <- rownames(coef_summary)

  B <- coef_summary[, "Estimate"]
  SE <- coef_summary[, "Std. Error"]
  Wald <- (B / SE)^2
  coef_df <- rep(1L, length(term_names))
  p_vals <- coef_summary[, "Pr(>|z|)"]
  exp_B <- exp(B)

  # CI for Exp(B) -- based on Wald CI for B
  alpha <- 1 - conf.level
  z_crit <- stats::qnorm(1 - alpha / 2)
  ci_lower_B <- B - z_crit * SE
  ci_upper_B <- B + z_crit * SE
  ci_lower_expB <- exp(ci_lower_B)
  ci_upper_expB <- exp(ci_upper_B)

  coef_table <- tibble::tibble(
    Term = term_names,
    B = B,
    S.E. = SE,
    Wald = Wald,
    df = coef_df,
    Sig. = p_vals,
    `Exp(B)` = exp_B,
    CI_lower = ci_lower_expB,
    CI_upper = ci_upper_expB
  )

  # ============================================================================
  # CLASSIFICATION TABLE
  # ============================================================================

  predicted_probs <- stats::fitted(model)
  predicted_class <- ifelse(predicted_probs >= 0.5, 1, 0)
  actual <- data_complete[[dep_name]]

  if (!is.null(weights_vec)) {
    # Weighted classification counts
    w <- weights_vec
    n0 <- sum(w[actual == 0])
    n1 <- sum(w[actual == 1])
    correct_0 <- sum(w[actual == 0 & predicted_class == 0])
    correct_1 <- sum(w[actual == 1 & predicted_class == 1])
  } else {
    n0 <- sum(actual == 0)
    n1 <- sum(actual == 1)
    correct_0 <- sum(actual == 0 & predicted_class == 0)
    correct_1 <- sum(actual == 1 & predicted_class == 1)
  }

  pct_correct_0 <- if (n0 > 0) correct_0 / n0 * 100 else NA_real_
  pct_correct_1 <- if (n1 > 0) correct_1 / n1 * 100 else NA_real_
  overall_pct <- (correct_0 + correct_1) / (n0 + n1) * 100

  classification <- list(
    n_0 = round(n0),
    n_1 = round(n1),
    correct_0 = round(correct_0),
    correct_1 = round(correct_1),
    pct_correct_0 = pct_correct_0,
    pct_correct_1 = pct_correct_1,
    overall_pct = overall_pct,
    cutoff = 0.5
  )

  # ============================================================================
  # HOSMER-LEMESHOW TEST
  # ============================================================================

  hosmer_lemeshow <- .hosmer_lemeshow_test(actual, predicted_probs, weights_vec)

  # ============================================================================
  # RETURN STRUCTURE
  # ============================================================================
  # The result object IS the fitted glm — mariposa-specific tables are
  # attached as additional slots so base-R and broom generics (predict,
  # anova, vcov, confint, residuals, fitted, formula, model.matrix, tidy,
  # glance, augment, ...) dispatch natively through the "glm"/"lm" classes.
  # glm's $coefficients stays the numeric vector downstream methods expect.

  out <- model
  out$coef_table       <- coef_table
  out$model_summary    <- model_stats
  out$omnibus_test     <- omnibus
  out$classification   <- classification
  out$hosmer_lemeshow  <- hosmer_lemeshow
  out$n                <- n_report
  out$dv_encoding      <- .logistic_encoding_table(dv_encoding)
  out$dv_labels        <- dv_encoding$short
  out
}


#' Encoding of a binary outcome (SPSS "Dependent Variable Encoding")
#'
#' Accepts any variable with exactly two distinct observed values, like
#' SPSS LOGISTIC REGRESSION: numeric/labelled - the lower value is coded
#' 0, the higher 1; factor - unused levels are dropped, the first
#' remaining level is 0; character - alphabetical order (as glm() orders
#' character levels); logical - FALSE = 0, TRUE = 1.
#'
#' @param x Outcome values of the cases in the analysis
#' @param dep_name Variable name (messages)
#' @return list(type, values, original, short): original = display text
#'   "value (label)", short = label if any, else value
#' @noRd
.logistic_dv_encoding <- function(x, dep_name) {
  if (is.logical(x)) {
    type <- "logical"
    vals <- sort(unique(x[!is.na(x)]))
    original <- short <- as.character(vals)
  } else if (is.factor(x)) {
    type <- "factor"
    vals <- levels(droplevels(x[!is.na(x)]))
    original <- short <- vals
  } else if (is.character(x)) {
    type <- "character"
    vals <- sort(unique(x[!is.na(x)]))
    original <- short <- vals
  } else if (is.numeric(x)) {
    type <- "numeric"
    xv <- .plain_numeric(x)
    vals <- sort(unique(xv[!is.na(xv)]))
    original <- short <- as.character(vals)
    labs <- attr(x, "labels", exact = TRUE)
    if (!is.null(labs) && length(vals) > 0) {
      hit <- match(vals, .plain_numeric(labs))
      has <- !is.na(hit)
      short[has] <- names(labs)[hit[has]]
      original[has] <- paste0(original[has], " (", short[has], ")")
    }
  } else {
    cli_abort(c(
      "Dependent variable {.var {dep_name}} must be binary.",
      x = "It is of class {.cls {class(x)[1]}}.",
      i = "Use a numeric, factor, character or logical variable with two values."
    ))
  }

  k <- length(vals)
  if (k == 0) {
    cli_abort("Dependent variable {.var {dep_name}} has no non-missing values.")
  }
  if (k == 1) {
    cli_abort(c(
      "Dependent variable {.var {dep_name}} has only one observed value ({.val {short}}).",
      i = "Logistic regression needs cases in both outcome categories."
    ), class = "mariposa_degenerate_fit")
  }
  if (k > 2) {
    shown <- if (k > 6) c(short[1:5], "...") else short
    cli_abort(c(
      "Dependent variable {.var {dep_name}} must be binary: it has {k} distinct values.",
      x = "Values: {paste(shown, collapse = ', ')}",
      i = "Recode it into two categories first, e.g. with {.fn rec}."
    ))
  }
  list(type = type, values = vals, original = original, short = short)
}

#' Map an outcome to internal 0/1 with a fixed encoding
#' @noRd
.logistic_apply_encoding <- function(x, enc) {
  switch(enc$type,
    logical = as.integer(x),
    numeric = as.integer(.plain_numeric(x) == enc$values[2]),
    as.integer(as.character(x) == enc$values[2])
  )
}

#' Encoding table stored on the result (Original Value -> Internal Value)
#' @noRd
.logistic_encoding_table <- function(enc) {
  tibble::tibble(Original = enc$original, Internal = c(0L, 1L))
}


# ============================================================================
# HOSMER-LEMESHOW TEST
# ============================================================================

#' Hosmer-Lemeshow goodness-of-fit test
#' @noRd
.hosmer_lemeshow_test <- function(observed, predicted, weights_vec = NULL,
                                 n_groups = 10) {
  # Sort by predicted probability
  ord <- order(predicted)
  predicted <- predicted[ord]
  observed <- observed[ord]
  if (!is.null(weights_vec)) {
    weights_vec <- weights_vec[ord]
  }

  n <- length(predicted)
  # Create groups based on deciles of predicted probabilities
  group_size <- ceiling(n / n_groups)
  groups <- rep(seq_len(n_groups), each = group_size)[seq_len(n)]

  chi_sq <- 0
  actual_groups <- 0

  for (g in unique(groups)) {
    idx <- groups == g
    if (!is.null(weights_vec)) {
      w <- weights_vec[idx]
      n_g <- sum(w)
      obs_events <- sum(w[observed[idx] == 1])
      exp_events <- sum(w * predicted[idx])
    } else {
      n_g <- sum(idx)
      obs_events <- sum(observed[idx])
      exp_events <- sum(predicted[idx])
    }

    exp_nonevents <- n_g - exp_events

    # Avoid division by zero
    if (exp_events > 0 && exp_nonevents > 0) {
      chi_sq <- chi_sq +
        (obs_events - exp_events)^2 / exp_events +
        ((n_g - obs_events) - exp_nonevents)^2 / exp_nonevents
      actual_groups <- actual_groups + 1
    }
  }

  hl_df <- actual_groups - 2
  if (hl_df < 1) hl_df <- 1
  hl_p <- stats::pchisq(chi_sq, df = hl_df, lower.tail = FALSE)

  list(
    chi_sq = chi_sq,
    df = hl_df,
    p = hl_p
  )
}


# ============================================================================
# COMPACT PRINT METHOD
# ============================================================================

#' Print logistic regression results (compact)
#'
#' @description
#' Compact print method for objects of class \code{"logistic_regression"}.
#' Shows Nagelkerke R-squared, chi-squared test, and classification accuracy.
#'
#' For the full detailed output, use \code{summary()}.
#'
#' @param x An object of class \code{"logistic_regression"} returned by
#'   \code{\link{logistic_regression}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' survey_data$high_satisfaction <- as.integer(survey_data$life_satisfaction > 3)
#' result <- logistic_regression(survey_data, high_satisfaction ~ age + income)
#' result              # compact one-line overview
#' summary(result)     # full detailed output
#'
#' @export
#' @method print logistic_regression
print.logistic_regression <- function(x, ...) {
  weighted_tag <- if (isTRUE(x$weighted)) " [Weighted]" else ""
  formula_str <- .formula_label(x$formula)
  # The modelled category (internal value 1), e.g. "[P(vote = yes)]"
  outcome_tag <- if (!is.null(x$dv_labels)) {
    sprintf(" [P(%s = %s)]", x$dependent, x$dv_labels[2])
  } else ""

  fit_line <- function(m) {
    sprintf("Nagelkerke R2 = %.3f, chi2(%d) = %.2f, %s, Accuracy = %.1f%%, N = %s",
            m$model_summary$nagelkerke_r2,
            as.integer(m$omnibus_test$df), m$omnibus_test$chi_sq,
            format_p_stars(m$omnibus_test$p),
            m$classification$overall_pct,
            .fmt_n(m$n))
  }

  if (isTRUE(x$is_grouped)) {
    grouped_tag <- sprintf(" [Grouped: %s]", paste(x$group_vars, collapse = ", "))
    cat(sprintf("Logistic Regression: %s%s%s%s\n", formula_str, outcome_tag,
                weighted_tag, grouped_tag))
    for (grp in x$groups) {
      cat(sprintf("  %s: %s\n", .format_group_label(grp$group_values),
                  fit_line(grp)))
    }
    .print_skipped_groups(x$skipped_groups)
  } else {
    cat(sprintf("Logistic Regression: %s%s%s\n", formula_str, outcome_tag,
                weighted_tag))
    cat(sprintf("  %s\n", fit_line(x)))
  }

  invisible(x)
}


# ============================================================================
# SUMMARY METHOD
# ============================================================================

#' Summary method for logistic regression results
#'
#' @description
#' Creates a summary object that produces detailed output when printed,
#' including omnibus test, model summary, Hosmer-Lemeshow test, classification
#' table, and coefficient table.
#'
#' @param object A \code{logistic_regression} result object.
#' @param omnibus_test Logical. Show omnibus test of model coefficients? (Default: TRUE)
#' @param model_summary Logical. Show model summary (pseudo R-squared)? (Default: TRUE)
#' @param hosmer_lemeshow Logical. Show Hosmer-Lemeshow test? (Default: TRUE)
#' @param classification Logical. Show classification table? (Default: TRUE)
#' @param coefficients Logical. Show coefficients table? (Default: TRUE)
#' @param digits Number of decimal places for formatting (Default: 3).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.logistic_regression} object.
#'
#' @examples
#' survey_data$high_satisfaction <- ifelse(survey_data$life_satisfaction >= 4, 1, 0)
#' result <- logistic_regression(survey_data, high_satisfaction ~ age + income)
#' summary(result)
#' summary(result, classification = FALSE)
#'
#' @seealso \code{\link{logistic_regression}} for the main analysis function.
#' @export
#' @method summary logistic_regression
summary.logistic_regression <- function(object, omnibus_test = TRUE,
                                         model_summary = TRUE,
                                         hosmer_lemeshow = TRUE,
                                         classification = TRUE,
                                         coefficients = TRUE,
                                         digits = 3, ...) {
  build_summary_object(
    object     = object,
    show       = list(omnibus_test    = omnibus_test,
                      model_summary   = model_summary,
                      hosmer_lemeshow = hosmer_lemeshow,
                      classification  = classification,
                      coefficients    = coefficients),
    digits     = digits,
    class_name = "summary.logistic_regression"
  )
}


#' Print summary of logistic regression results (detailed output)
#'
#' @description
#' Displays the detailed SPSS-style output for a logistic regression, with
#' sections controlled by the boolean parameters passed to
#' \code{\link{summary.logistic_regression}}.  Sections include the omnibus
#' test of model coefficients, model fit statistics (Nagelkerke R-squared,
#' Hosmer-Lemeshow), classification table, and coefficients with odds ratios.
#'
#' @param x A \code{summary.logistic_regression} object created by
#'   \code{\link{summary.logistic_regression}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' survey_data$high_satisfaction <- as.integer(survey_data$life_satisfaction > 3)
#' result <- logistic_regression(survey_data, high_satisfaction ~ age + income)
#' summary(result)                         # all sections
#' summary(result, classification = FALSE) # hide classification table
#'
#' @seealso \code{\link{logistic_regression}} for the main analysis,
#'   \code{\link{summary.logistic_regression}} for summary options.
#' @export
#' @method print summary.logistic_regression
print.summary.logistic_regression <- function(x, ...) {
  if (isTRUE(x$is_grouped)) {
    .print_summary_logistic_grouped(x)
  } else {
    .print_summary_logistic_ungrouped(x)
  }
  invisible(x)
}


#' Print ungrouped logistic regression summary (verbose)
#' @noRd
.print_summary_logistic_ungrouped <- function(x) {
  title <- get_standard_title("Logistic Regression", x$weight_name, "Results")
  print_header(title)

  formula_str <- .formula_label(x$formula)
  info <- list(
    "Formula" = formula_str,
    "Method" = "ENTER",
    "N" = x$n
  )
  if (isTRUE(x$weighted)) {
    info[["Weights"]] <- x$weight_name
  }
  print_info_section(info)

  show_omnibus <- if (!is.null(x$show)) isTRUE(x$show$omnibus_test) else TRUE
  show_model <- if (!is.null(x$show)) isTRUE(x$show$model_summary) else TRUE
  show_hl <- if (!is.null(x$show)) isTRUE(x$show$hosmer_lemeshow) else TRUE
  show_class <- if (!is.null(x$show)) isTRUE(x$show$classification) else TRUE
  show_coefs <- if (!is.null(x$show)) isTRUE(x$show$coefficients) else TRUE

  cat("\n")
  .print_dv_encoding(x$dv_encoding)

  if (show_omnibus) {
    cat("\n")
    .print_omnibus_test(x$omnibus_test)
  }

  if (show_model) {
    cat("\n")
    .print_logistic_model_summary(x$model_summary)
  }

  if (show_hl) {
    cat("\n")
    .print_hosmer_lemeshow(x$hosmer_lemeshow)
  }

  if (show_class) {
    cat("\n")
    .print_classification_table(x$classification, x$dv_labels)
  }

  if (show_coefs) {
    cat("\n")
    .print_logistic_coefficients(x$coef_table)
  }

  # Show significance legend if any section with p-values is visible
  if (show_omnibus || show_coefs) {
    print_significance_legend(TRUE)
  }
}


#' Print grouped logistic regression summary (verbose)
#' @noRd
.print_summary_logistic_grouped <- function(x) {
  title <- get_standard_title("Logistic Regression", x$weight_name, "Results")
  print_header(title)

  formula_str <- .formula_label(x$formula)
  info <- list(
    "Formula" = formula_str,
    "Method" = "ENTER",
    "Grouped by" = paste(x$group_vars, collapse = ", ")
  )
  if (isTRUE(x$weighted)) {
    info[["Weights"]] <- x$weight_name
  }
  print_info_section(info)

  show_omnibus <- if (!is.null(x$show)) isTRUE(x$show$omnibus_test) else TRUE
  show_model <- if (!is.null(x$show)) isTRUE(x$show$model_summary) else TRUE
  show_hl <- if (!is.null(x$show)) isTRUE(x$show$hosmer_lemeshow) else TRUE
  show_class <- if (!is.null(x$show)) isTRUE(x$show$classification) else TRUE
  show_coefs <- if (!is.null(x$show)) isTRUE(x$show$coefficients) else TRUE

  cat("\n")
  .print_dv_encoding(x$dv_encoding)

  for (grp in x$groups) {
    cat("\n")
    print_group_header(grp$group_values)

    cat(sprintf("  N: %s\n", .fmt_n(grp$n)))

    if (show_omnibus) {
      cat("\n")
      .print_omnibus_test(grp$omnibus_test)
    }

    if (show_model) {
      cat("\n")
      .print_logistic_model_summary(grp$model_summary)
    }

    if (show_hl) {
      cat("\n")
      .print_hosmer_lemeshow(grp$hosmer_lemeshow)
    }

    if (show_class) {
      cat("\n")
      .print_classification_table(grp$classification, x$dv_labels)
    }

    if (show_coefs) {
      cat("\n")
      .print_logistic_coefficients(grp$coef_table)
    }
  }
  .print_skipped_groups(x$skipped_groups, verbose = TRUE)

  if (show_omnibus || show_coefs) {
    print_significance_legend(TRUE)
  }
}


# ============================================================================
# PRINT HELPERS
# ============================================================================

#' Print omnibus test of model coefficients
#' @noRd
.print_omnibus_test <- function(omnibus) {
  cat("  Omnibus Tests of Model Coefficients\n")
  w <- 50
  cat(paste0("  ", strrep("-", w), "\n"))
  cat(sprintf("  %-20s %12s %5s %10s\n", "", "Chi-square", "df", "Sig."))
  cat(paste0("  ", strrep("-", w), "\n"))
  stars <- add_significance_stars(omnibus$p)
  cat(sprintf("  %-20s %12.3f %5d %10.3f %s\n",
              "Model", omnibus$chi_sq, omnibus$df, omnibus$p, stars))
  cat(paste0("  ", strrep("-", w), "\n"))
}


#' Print logistic model summary
#' @noRd
.print_logistic_model_summary <- function(ms) {
  cat("  Model Summary\n")
  w <- 60
  cat(paste0("  ", strrep("-", w), "\n"))
  cat(sprintf("  %-30s %12.3f\n", "-2 Log Likelihood", ms$minus2LL))
  cat(sprintf("  %-30s %12.3f\n", "Cox & Snell R Square", ms$cox_snell_r2))
  cat(sprintf("  %-30s %12.3f\n", "Nagelkerke R Square", ms$nagelkerke_r2))
  cat(sprintf("  %-30s %12.3f\n", "McFadden R Square", ms$mcfadden_r2))
  cat(paste0("  ", strrep("-", w), "\n"))
}


#' Print Hosmer-Lemeshow test
#' @noRd
.print_hosmer_lemeshow <- function(hl) {
  cat("  Hosmer and Lemeshow Test\n")
  w <- 50
  cat(paste0("  ", strrep("-", w), "\n"))
  cat(sprintf("  %-20s %12s %5s %10s\n", "", "Chi-square", "df", "Sig."))
  cat(paste0("  ", strrep("-", w), "\n"))
  cat(sprintf("  %-20s %12.3f %5d %10.3f\n",
              "", hl$chi_sq, hl$df, hl$p))
  cat(paste0("  ", strrep("-", w), "\n"))
}


#' Print the Dependent Variable Encoding table (SPSS)
#' @noRd
.print_dv_encoding <- function(enc) {
  if (is.null(enc)) return(invisible(NULL))
  cat("  Dependent Variable Encoding\n")
  print_stat_table(data.frame(`Original Value` = enc$Original,
                              `Internal Value` = enc$Internal,
                              check.names = FALSE, stringsAsFactors = FALSE))
}


#' Print classification table
#'
#' Rows and columns carry the outcome categories (value labels / factor
#' levels) instead of the internal 0/1 codes.
#' @noRd
.print_classification_table <- function(cls, labels = c("0", "1")) {
  cat(sprintf(
    "  Classification Table (cutoff = %.2f; rows: observed, columns: predicted)\n",
    cls$cutoff))
  labels <- labels %||% c("0", "1")
  incorrect_0 <- cls$n_0 - cls$correct_0
  incorrect_1 <- cls$n_1 - cls$correct_1
  pct <- function(v) ifelse(is.na(v), "", sprintf("%.1f", v))
  tab <- data.frame(
    Observed = c(labels, "Overall Percentage"),
    c0 = c(cls$correct_0, incorrect_1, NA),
    c1 = c(incorrect_0, cls$correct_1, NA),
    correct = pct(c(cls$pct_correct_0, cls$pct_correct_1, cls$overall_pct)),
    stringsAsFactors = FALSE
  )
  print_stat_table(tab, col_types = c(c0 = "int", c1 = "int"),
                   col_labels = c(c0 = labels[1], c1 = labels[2],
                                  correct = "% Correct"))
}


#' Print logistic regression coefficients table
#' @noRd
.print_logistic_coefficients <- function(coefs) {
  cat("  Variables in the Equation\n")
  w <- 95
  cat(paste0("  ", strrep("-", w), "\n"))
  cat(sprintf("  %-20s %9s %9s %9s %4s %8s %10s %9s %9s %s\n",
              "Term", "B", "S.E.", "Wald", "df", "Sig.", "Exp(B)",
              "Lower", "Upper", ""))
  cat(paste0("  ", strrep("-", w), "\n"))

  for (i in seq_len(nrow(coefs))) {
    term <- coefs$Term[i]
    if (nchar(term) > 20) term <- paste0(substr(term, 1, 17), "...")

    stars <- add_significance_stars(coefs$Sig.[i])

    # For intercept, don't show Exp(B) CI
    if (coefs$Term[i] == "(Intercept)") {
      cat(sprintf("  %-20s %9.3f %9.3f %9.3f %4d %8.3f %10.3f %9s %9s %s\n",
                  term, coefs$B[i], coefs$S.E.[i], coefs$Wald[i],
                  coefs$df[i], coefs$Sig.[i], coefs$`Exp(B)`[i],
                  "", "", stars))
    } else {
      cat(sprintf("  %-20s %9.3f %9.3f %9.3f %4d %8.3f %10.3f %9.3f %9.3f %s\n",
                  term, coefs$B[i], coefs$S.E.[i], coefs$Wald[i],
                  coefs$df[i], coefs$Sig.[i], coefs$`Exp(B)`[i],
                  coefs$CI_lower[i], coefs$CI_upper[i], stars))
    }
  }
  cat(paste0("  ", strrep("-", w), "\n"))
}


# ============================================================================
# NATIVE GENERIC SUPPORT
# ============================================================================
# Listwise + ungrouped logistic_regression results inherit from "glm" /
# "lm", so predict.glm, anova.glm, vcov.glm, confint.glm, residuals.glm,
# fitted.glm, formula, model.matrix, broom::tidy.glm, broom::glance.glm,
# broom::augment.glm dispatch natively. Grouped results need per-group
# iteration; the overrides below surface actionable errors.

.glr_require_glm <- function(object, generic) {
  if (isTRUE(object$is_grouped)) {
    cli_abort(c(
      "{.code {generic}()} is not supported on a grouped {.cls logistic_regression}.",
      i = "Each element of {.code object$groups} is itself a fitted model.",
      i = "Use {.code lapply(object$groups, {generic}, ...)} for per-group results."
    ))
  }
  if (!inherits(object, "glm")) {
    cli_abort(c(
      "{.code {generic}()} requires the underlying {.cls glm} object.",
      i = "Refit without grouping to enable {.code {generic}()}."
    ))
  }
}

#' Predict from a logistic_regression model
#'
#' For ungrouped results, dispatches to \code{stats::predict.glm}
#' (the result inherits from \code{"glm"}). For grouped results, raises
#' an informative error pointing at \code{object$groups}.
#'
#' @param object A \code{logistic_regression} result.
#' @param ... Passed to \code{stats::predict.glm}.
#' @return A numeric vector of predictions on the scale requested via
#'   \code{type} (link scale by default), as returned by
#'   \code{stats::predict.glm}.
#' @export
#' @method predict logistic_regression
predict.logistic_regression <- function(object, ...) {
  .glr_require_glm(object, "predict")
  NextMethod()
}

#' ANOVA for a logistic_regression model
#'
#' For ungrouped results, dispatches to \code{stats::anova.glm} (sequential
#' deviance per term). For the SPSS-style omnibus chi-square test, use
#' \code{object$omnibus_test}.
#'
#' @param object A \code{logistic_regression} result.
#' @param ... Passed to \code{stats::anova.glm}.
#' @return An \code{anova} table (sequential analysis of deviance per
#'   term), as returned by \code{stats::anova.glm}.
#' @export
#' @method anova logistic_regression
anova.logistic_regression <- function(object, ...) {
  .glr_require_glm(object, "anova")
  # anova.glm refits the sub-models: muffle the expected fractional
  # frequency-weight warning of each refit (weighted models)
  .glm_quiet_weights(stats::anova(.glr_strip_class(object), ...))
}

#' Confidence intervals for logistic regression coefficients
#'
#' @description
#' Confidence intervals for the coefficients B (log-odds scale) of a
#' \code{\link{logistic_regression}} model.
#'
#' The default \code{method = "wald"} gives Wald intervals
#' (\eqn{B \pm z_{1-\alpha/2} \cdot SE}), the interval SPSS LOGISTIC
#' REGRESSION reports as "95\% C.I. for EXP(B)" and the one
#' \code{summary()} prints: \code{exp(confint(model))} reproduces the
#' Lower/Upper columns of the coefficients table. \code{method = "profile"}
#' gives the profile-likelihood intervals of \code{stats::confint()} for
#' \code{glm} objects.
#'
#' @param object A \code{logistic_regression} result (ungrouped).
#' @param parm Coefficients to compute intervals for (names or indices;
#'   default all).
#' @param level Confidence level (default 0.95).
#' @param method \code{"wald"} (default, SPSS) or \code{"profile"}.
#' @param ... Passed to the underlying \code{confint} method.
#' @return A matrix with one row per coefficient and columns for the lower
#'   and upper limits (log-odds scale).
#'
#' @examples
#' survey_data$high_satisfaction <- as.integer(survey_data$life_satisfaction >= 4)
#' model <- logistic_regression(survey_data, high_satisfaction ~ age + gender)
#' confint(model)        # Wald (SPSS)
#' exp(confint(model))   # SPSS "95% C.I. for EXP(B)"
#'
#' @export
#' @method confint logistic_regression
confint.logistic_regression <- function(object, parm, level = 0.95,
                                        method = c("wald", "profile"), ...) {
  .glr_require_glm(object, "confint")
  method <- match.arg(method)
  fit <- .glr_strip_class(object)
  if (method == "wald") {
    stats::confint.default(fit, parm = parm, level = level, ...)
  } else {
    .glm_quiet_weights(stats::confint(fit, parm = parm, level = level, ...))
  }
}

#' Profile likelihood for a logistic_regression model
#'
#' Dispatches to the \code{glm} method (\code{stats::profile()}); used by
#' \code{confint(model, method = "profile")}.
#'
#' @param fitted A \code{logistic_regression} result (ungrouped).
#' @param ... Passed to \code{stats::profile()}.
#' @return A \code{"profile"} object, as returned for \code{glm} fits.
#' @export
#' @method profile logistic_regression
profile.logistic_regression <- function(fitted, ...) {
  .glr_require_glm(fitted, "profile")
  .glm_quiet_weights(stats::profile(.glr_strip_class(fitted), ...))
}
