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
#'   when no formula is given. Must be binary (0/1 or two-level factor).
#' @param predictors Predictor variable(s) (unquoted, supports tidyselect).
#'   Used with \code{dependent} when no formula is given.
#' @param weights Optional survey weights (unquoted variable name). When
#'   specified, weighted maximum likelihood estimation is used, matching
#'   SPSS WEIGHT BY behavior.
#' @param conf.level Confidence level for odds ratio intervals (default 0.95).
#'
#' @return An object of class \code{"logistic_regression"} containing:
#' \describe{
#'   \item{coefficients}{Tibble with B, S.E., Wald, df, Sig., Exp(B), CI_lower, CI_upper}
#'   \item{model_summary}{List with minus2LL, cox_snell_r2, nagelkerke_r2, mcfadden_r2}
#'   \item{omnibus_test}{List with chi_sq, df, p for overall model test}
#'   \item{classification}{List with table, overall_pct, pct_correct_0, pct_correct_1}
#'   \item{hosmer_lemeshow}{List with chi_sq, df, p (goodness-of-fit test)}
#'   \item{model}{The underlying \code{glm} object}
#'   \item{formula}{The formula used}
#'   \item{n}{Sample size (listwise complete cases)}
#'   \item{dependent}{Name of the dependent variable}
#'   \item{predictor_names}{Names of predictor variables}
#'   \item{weighted}{Logical indicating whether weights were used}
#'   \item{weight_name}{Name of the weight variable (or NULL)}
#' }
#'   Use \code{summary()} for the full SPSS-style output with toggleable sections.
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
#' \strong{Dependent Variable}: Must be binary. Factors with exactly 2 levels
#' are automatically converted to 0/1 (first level = 0, second level = 1).
#' Numeric variables must contain only 0 and 1 values.
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
#' # --- Three-layer output ---
#' result <- logistic_regression(survey_data, high_satisfaction ~ age + income)
#' result              # compact one-line overview
#' summary(result)     # full detailed SPSS-style output
#' summary(result, classification_table = FALSE)  # hide classification
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
    if (!inherits(formula, "formula")) {
      cli_abort("{.arg formula} must be a formula object (e.g., {.code y ~ x1 + x2}).")
    }
    model_formula <- formula
    dep_name <- as.character(formula[[2]])
    pred_names <- all.vars(formula[[3]])
  } else {
    dep_quo <- rlang::enquo(dependent)
    pred_quo <- rlang::enquo(predictors)

    if (rlang::quo_is_null(dep_quo)) {
      cli_abort("Either {.arg formula} or {.arg dependent} must be specified.")
    }

    dep_name <- rlang::as_name(dep_quo)

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

      result <- .glm_core(grp_data, model_formula, dep_name, pred_names,
                          grp_weights, conf.level)
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
        conf.level = conf.level
      ),
      class = "logistic_regression"
    )
  } else {
    result <- .glm_core(data, model_formula, dep_name, pred_names,
                        weights_vec, conf.level)
    result$formula <- model_formula
    result$dependent <- dep_name
    result$predictor_names <- pred_names
    result$weighted <- has_weights
    result$weight_name <- weight_name
    result$is_grouped <- FALSE
    result$conf.level <- conf.level
    class(result) <- "logistic_regression"
    result
  }
}


# ============================================================================
# CORE COMPUTATION
# ============================================================================

#' Core logistic regression computation
#' @keywords internal
.glm_core <- function(data, formula, dep_name, pred_names, weights_vec,
                      conf.level) {

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
    cli_abort("Insufficient observations for the number of predictors.")
  }

  # Convert factors to numeric for predictors
  for (v in pred_names) {
    if (is.factor(data_complete[[v]])) {
      data_complete[[v]] <- as.numeric(data_complete[[v]])
    }
  }

  # Validate binary DV
  dv <- data_complete[[dep_name]]
  if (is.factor(dv)) {
    if (nlevels(dv) != 2) {
      cli_abort("Dependent variable must be binary (exactly 2 levels).")
    }
    data_complete[[dep_name]] <- as.numeric(dv) - 1  # first level=0, second=1
    dv <- data_complete[[dep_name]]
  } else {
    unique_vals <- sort(unique(dv))
    if (!all(unique_vals %in% c(0, 1))) {
      cli_abort("Dependent variable must be binary (0/1).")
    }
  }

  # ============================================================================
  # FIT MODEL
  # ============================================================================

  # Suppress non-integer warning when using frequency weights with binomial GLM
  # This is expected behavior matching SPSS WEIGHT BY
  if (!is.null(weights_vec)) {
    data_complete$.wt <- weights_vec
    model <- suppressWarnings(
      stats::glm(formula, data = data_complete, family = stats::binomial(),
                 weights = .wt)
    )
  } else {
    model <- stats::glm(formula, data = data_complete, family = stats::binomial())
  }

  # Also fit null model for pseudo R-squared calculations
  null_formula <- stats::as.formula(paste(dep_name, "~ 1"))
  if (!is.null(weights_vec)) {
    null_model <- suppressWarnings(
      stats::glm(null_formula, data = data_complete,
                 family = stats::binomial(), weights = .wt)
    )
  } else {
    null_model <- stats::glm(null_formula, data = data_complete,
                             family = stats::binomial())
  }

  # ============================================================================
  # SPSS-COMPATIBLE STATISTICS
  # ============================================================================

  # N: SPSS uses weighted N when WEIGHT BY is active
  if (!is.null(weights_vec)) {
    n_report <- round(sum(weights_vec))
  } else {
    n_report <- n_actual
  }

  # -2 Log Likelihood
  minus2LL_model <- -2 * as.numeric(stats::logLik(model))
  minus2LL_null <- -2 * as.numeric(stats::logLik(null_model))

  # Log likelihoods
  LL_model <- as.numeric(stats::logLik(model))
  LL_null <- as.numeric(stats::logLik(null_model))

  # Omnibus test (Model chi-square)
  omnibus_chi_sq <- minus2LL_null - minus2LL_model
  omnibus_df <- length(pred_names)
  omnibus_p <- stats::pchisq(omnibus_chi_sq, df = omnibus_df, lower.tail = FALSE)

  # Pseudo R-squared measures
  # Cox & Snell R²: 1 - (L0/L1)^(2/n)
  cox_snell <- 1 - exp((minus2LL_model - minus2LL_null) / n_report)

  # Max Cox & Snell (for Nagelkerke adjustment)
  cox_snell_max <- 1 - exp(minus2LL_null / n_report * (-1))

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

  list(
    coefficients = coef_table,
    model_summary = model_stats,
    omnibus_test = omnibus,
    classification = classification,
    hosmer_lemeshow = hosmer_lemeshow,
    model = model,
    n = n_report
  )
}


# ============================================================================
# HOSMER-LEMESHOW TEST
# ============================================================================

#' Hosmer-Lemeshow goodness-of-fit test
#' @keywords internal
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
  formula_str <- deparse(x$formula)

  if (isTRUE(x$is_grouped)) {
    grouped_tag <- sprintf(" [Grouped: %s]", paste(x$group_vars, collapse = ", "))
    cat(sprintf("Logistic Regression: %s%s%s\n", formula_str, weighted_tag, grouped_tag))
    for (grp in x$groups) {
      grp_label <- paste(names(grp$group_values), "=",
                         unlist(grp$group_values), collapse = ", ")
      chi_sq <- grp$omnibus_test$chi_sq
      chi_df <- grp$omnibus_test$df
      chi_p <- grp$omnibus_test$p
      p_str <- format_p_compact(chi_p)
      stars <- add_significance_stars(chi_p)
      cat(sprintf("  %s: Nagelkerke R2 = %.3f, chi2(%d) = %.2f, %s %s, Accuracy = %.1f%%, N = %d\n",
                  grp_label,
                  grp$model_summary$nagelkerke_r2,
                  chi_df, chi_sq,
                  p_str, stars,
                  grp$classification$overall_pct,
                  grp$n))
    }
  } else {
    cat(sprintf("Logistic Regression: %s%s\n", formula_str, weighted_tag))
    chi_sq <- x$omnibus_test$chi_sq
    chi_df <- x$omnibus_test$df
    chi_p <- x$omnibus_test$p
    p_str <- format_p_compact(chi_p)
    stars <- add_significance_stars(chi_p)
    cat(sprintf("  Nagelkerke R2 = %.3f, chi2(%d) = %.2f, %s %s, Accuracy = %.1f%%, N = %d\n",
                x$model_summary$nagelkerke_r2,
                chi_df, chi_sq,
                p_str, stars,
                x$classification$overall_pct,
                x$n))
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
#' \code{\link{summary.logistic_regression}}.  Sections include model fit
#' statistics (Nagelkerke R-squared, Hosmer-Lemeshow), classification table,
#' coefficients with odds ratios, and model comparison (chi-squared test).
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
#' summary(result)                              # all sections
#' summary(result, classification_table = FALSE) # hide classification
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
#' @keywords internal
.print_summary_logistic_ungrouped <- function(x) {
  title <- get_standard_title("Logistic Regression", x$weight_name, "Results")
  print_header(title)

  formula_str <- deparse(x$formula)
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
    .print_classification_table(x$classification)
  }

  if (show_coefs) {
    cat("\n")
    .print_logistic_coefficients(x$coefficients)
  }

  # Show significance legend if any section with p-values is visible
  if (show_omnibus || show_coefs) {
    print_significance_legend(TRUE)
  }
}


#' Print grouped logistic regression summary (verbose)
#' @keywords internal
.print_summary_logistic_grouped <- function(x) {
  title <- get_standard_title("Logistic Regression", x$weight_name, "Results")
  print_header(title)

  formula_str <- deparse(x$formula)
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

  for (grp in x$groups) {
    cat("\n")
    print_group_header(grp$group_values)

    cat(sprintf("  N: %d\n", grp$n))

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
      .print_classification_table(grp$classification)
    }

    if (show_coefs) {
      cat("\n")
      .print_logistic_coefficients(grp$coefficients)
    }
  }

  if (show_omnibus || show_coefs) {
    print_significance_legend(TRUE)
  }
}


# ============================================================================
# PRINT HELPERS
# ============================================================================

#' Print omnibus test of model coefficients
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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


#' Print classification table
#' @keywords internal
.print_classification_table <- function(cls) {
  cat("  Classification Table (cutoff = 0.50)\n")
  w <- 65
  cat(paste0("  ", strrep("-", w), "\n"))
  cat(sprintf("  %-20s %20s %20s\n", "", "Predicted", ""))
  cat(sprintf("  %-20s %10s %10s %15s\n",
              "Observed", "0", "1", "% Correct"))
  cat(paste0("  ", strrep("-", w), "\n"))

  incorrect_0 <- cls$n_0 - cls$correct_0
  incorrect_1 <- cls$n_1 - cls$correct_1

  cat(sprintf("  %-20s %10d %10d %14.1f\n",
              "0", cls$correct_0, incorrect_0, cls$pct_correct_0))
  cat(sprintf("  %-20s %10d %10d %14.1f\n",
              "1", incorrect_1, cls$correct_1, cls$pct_correct_1))
  cat(paste0("  ", strrep("-", w), "\n"))
  cat(sprintf("  %-20s %10s %10s %14.1f\n",
              "Overall Percentage", "", "", cls$overall_pct))
  cat(paste0("  ", strrep("-", w), "\n"))
}


#' Print logistic regression coefficients table
#' @keywords internal
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
