#' Average Marginal Effects
#'
#' @description
#' \code{marginal_effects()} computes average marginal effects (AMEs) for
#' a fitted \code{\link{logistic_regression}} model — the Stata
#' \code{margins, dydx(*)} workhorse. AMEs translate logit coefficients
#' into the language reports actually use: \emph{percentage-point changes
#' in the predicted probability}.
#'
#' For each continuous predictor, the AME is the derivative of the
#' predicted probability with respect to that predictor, averaged over
#' the observed sample. For each factor level, it is the average
#' \emph{discrete change} in predicted probability compared to the
#' reference level.
#'
#' @param model A fitted model object. Currently implemented for
#'   \code{\link{logistic_regression}} results (ungrouped or grouped).
#' @param conf.level Confidence level for the AME intervals
#'   (default 0.95).
#' @param ... Additional arguments (not used).
#'
#' @return An object of class \code{"marginal_effects"} whose
#'   \code{$results} tibble holds one row per predictor term (and group
#'   combination) with:
#' \describe{
#'   \item{Term}{Predictor name; factor rows read
#'     \code{"var: level vs. ref"}}
#'   \item{Type}{\code{"dydx"} (continuous derivative) or
#'     \code{"discrete"} (factor-level contrast)}
#'   \item{AME}{Average marginal effect on the probability scale}
#'   \item{SE}{Delta-method standard error}
#'   \item{z, p_value}{Wald test of the AME}
#'   \item{CI_lower, CI_upper}{Confidence interval at \code{conf.level}}
#' }
#'
#' @details
#' ## Understanding the Output
#'
#' An AME of 0.03 for \code{age} means: averaged over the sample, one
#' additional year of age increases the predicted probability of the
#' outcome by 3 percentage points. Unlike odds ratios, AMEs are directly
#' comparable across models and across groups, which is why they are the
#' preferred reporting style in much of the social sciences.
#'
#' ## When to Use This
#'
#' \itemize{
#'   \item To report logistic regression results on the probability
#'     scale instead of (or alongside) odds ratios
#'   \item To compare predictor effects across groups (run on a
#'     \code{group_by()}-fitted model)
#' }
#'
#' ## Technical Details
#'
#' Continuous predictors use a centered numerical derivative of the
#' predicted probability; factor predictors use the average discrete
#' change against the reference level. Standard errors come from the
#' delta method with the analytic gradient of the AME with respect to
#' the coefficients and the model's Wald covariance matrix, matching the
#' default in Stata's \code{margins} and R's \pkg{margins} package.
#' Weighted models average with their frequency weights (unrounded,
#' Charter §5.1); at \code{weights == 1} the weighted AME reduces
#' exactly to the unweighted one.
#'
#' \strong{Linear regression}: AMEs are not needed there — for a linear
#' model without interactions or transformations, the unstandardized
#' coefficient B \emph{is} the marginal effect, already shown in the
#' \code{\link{linear_regression}} coefficients table. Calling
#' \code{marginal_effects()} on a \code{linear_regression} result says
#' so instead of duplicating the table.
#'
#' \strong{Validation}: SPSS has no AME procedure, so this function is
#' Tier 4 (Internal) under the Validation Charter — verified against the
#' analytic logit-AME formula and an independent finite-difference
#' delta-method recomputation, not against SPSS output.
#'
#' @examples
#' data(survey_data)
#' survey_data$high_satisfaction <- as.integer(survey_data$life_satisfaction >= 4)
#'
#' model <- logistic_regression(survey_data,
#'                              high_satisfaction ~ age + income + education)
#' marginal_effects(model)
#'
#' # Weighted model
#' model_w <- logistic_regression(survey_data, high_satisfaction ~ age + income,
#'                                weights = sampling_weight)
#' marginal_effects(model_w)
#'
#' # --- Three-layer output ---
#' ame <- marginal_effects(model)
#' ame                 # compact overview
#' summary(ame)        # full detailed output
#'
#' @seealso
#' \code{\link{logistic_regression}} for fitting the model.
#'
#' \code{\link{summary.marginal_effects}} for detailed output.
#'
#' @family regression
#' @export
marginal_effects <- function(model, conf.level = 0.95, ...) {
  UseMethod("marginal_effects")
}

#' @export
marginal_effects.default <- function(model, conf.level = 0.95, ...) {
  cli_abort(c(
    "{.fn marginal_effects} is not implemented for {.cls {class(model)[1]}} objects.",
    i = "Fit the model with {.fn logistic_regression} first."
  ))
}

#' @export
#' @method marginal_effects linear_regression
marginal_effects.linear_regression <- function(model, conf.level = 0.95, ...) {
  cli_abort(c(
    "Average marginal effects are not needed for a linear model.",
    i = "For a linear model without interactions, the coefficient B {.emph is} the marginal effect.",
    i = "See the Coefficients table: {.code summary(model)}."
  ))
}

#' @export
#' @method marginal_effects logistic_regression
marginal_effects.logistic_regression <- function(model, conf.level = 0.95, ...) {
  if (conf.level <= 0 || conf.level >= 1) {
    cli_abort("{.arg conf.level} must be between 0 and 1.")
  }

  if (isTRUE(model$is_grouped)) {
    group_vars <- model$group_vars
    per_group <- lapply(model$groups, function(grp) {
      rows <- .ame_glm(grp, conf.level)
      gv <- as.data.frame(grp$group_values, stringsAsFactors = FALSE)
      cbind(gv, rows)
    })
    results <- tibble::as_tibble(do.call(rbind, per_group))
    n <- vapply(model$groups, function(g) g$n, numeric(1))
  } else {
    if (!inherits(model, "glm")) {
      cli_abort("{.fn marginal_effects} requires the fitted {.cls glm} object.")
    }
    group_vars <- character(0)
    results <- .ame_glm(model, conf.level)
    n <- model$n
  }

  structure(
    list(
      results = results,
      dependent = model$dependent,
      formula = model$formula,
      weighted = isTRUE(model$weighted),
      weight_name = model$weight_name,
      conf.level = conf.level,
      n = n,
      is_grouped = isTRUE(model$is_grouped),
      group_vars = group_vars
    ),
    class = "marginal_effects"
  )
}


#' AME computation for one fitted binomial glm
#'
#' For each raw predictor variable in the model frame, builds a pair of
#' counterfactual design matrices (x + h / x - h for continuous, level /
#' reference for factors), averages the difference of the predicted
#' probabilities with the model's frequency weights, and derives the
#' delta-method SE from the analytic gradient
#'   d AME / d beta = wmean( mu'(eta_A) X_A - mu'(eta_B) X_B ) / scale
#' with mu'(eta) = mu (1 - mu) for the logit link.
#' @noRd
.ame_glm <- function(model, conf.level) {
  mf <- model$model
  beta <- stats::coef(model)
  V <- stats::vcov(model)
  tt <- stats::delete.response(stats::terms(model))
  w <- model$prior.weights
  sw <- sum(w)

  pred_vars <- all.vars(tt)
  z_crit <- stats::qnorm(1 - (1 - conf.level) / 2)

  build_X <- function(newdata) {
    stats::model.matrix(tt, newdata, xlev = model$xlevels,
                        contrasts.arg = model$contrasts)
  }
  linkinv <- stats::binomial()$linkinv

  one_contrast <- function(data_a, data_b, scale, term, type) {
    Xa <- build_X(data_a)
    Xb <- build_X(data_b)
    # Aliased (NA) coefficients contribute nothing
    keep <- !is.na(beta)
    eta_a <- as.vector(Xa[, keep, drop = FALSE] %*% beta[keep])
    eta_b <- as.vector(Xb[, keep, drop = FALSE] %*% beta[keep])
    mu_a <- linkinv(eta_a)
    mu_b <- linkinv(eta_b)

    ame <- sum(w * (mu_a - mu_b)) / sw / scale

    dmu_a <- mu_a * (1 - mu_a)
    dmu_b <- mu_b * (1 - mu_b)
    grad <- (crossprod(Xa[, keep, drop = FALSE], w * dmu_a) -
               crossprod(Xb[, keep, drop = FALSE], w * dmu_b)) / sw / scale
    se <- sqrt(as.numeric(t(grad) %*% V %*% grad))

    z <- ame / se
    tibble::tibble(
      Term = term,
      Type = type,
      AME = ame,
      SE = se,
      z = z,
      p_value = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
      CI_lower = ame - z_crit * se,
      CI_upper = ame + z_crit * se
    )
  }

  rows <- list()
  for (v in pred_vars) {
    x <- mf[[v]]
    if (is.factor(x)) {
      levs <- model$xlevels[[v]] %||% levels(x)
      ref <- levs[1]
      for (lv in levs[-1]) {
        da <- mf; da[[v]] <- factor(lv, levels = levs)
        db <- mf; db[[v]] <- factor(ref, levels = levs)
        rows[[length(rows) + 1]] <- one_contrast(
          da, db, scale = 1,
          term = sprintf("%s: %s vs. %s", v, lv, ref),
          type = "discrete"
        )
      }
    } else if (is.numeric(x)) {
      h <- stats::sd(x)
      if (!is.finite(h) || h == 0) h <- 1
      h <- h * 1e-4
      da <- mf; da[[v]] <- x + h
      db <- mf; db[[v]] <- x - h
      rows[[length(rows) + 1]] <- one_contrast(
        da, db, scale = 2 * h, term = v, type = "dydx"
      )
    }
  }

  do.call(rbind, rows)
}


# ============================================================================
# COMPACT PRINT METHOD
# ============================================================================

#' Print average marginal effects (compact)
#'
#' @description
#' Compact print method for objects of class \code{"marginal_effects"}:
#' one line per predictor with the AME on the probability scale.
#'
#' @param x An object of class \code{"marginal_effects"} returned by
#'   \code{\link{marginal_effects}}.
#' @param digits Number of decimal places (default: 3).
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' survey_data$high_satisfaction <- as.integer(survey_data$life_satisfaction >= 4)
#' model <- logistic_regression(survey_data, high_satisfaction ~ age + income)
#' marginal_effects(model)
#'
#' @export
#' @method print marginal_effects
print.marginal_effects <- function(x, digits = 3, ...) {
  weighted_tag <- if (isTRUE(x$weighted)) " [Weighted]" else ""
  cat(sprintf("Average Marginal Effects: %s%s\n", deparse(x$formula), weighted_tag))

  print_rows <- function(rows, indent = "  ") {
    for (i in seq_len(nrow(rows))) {
      r <- rows[i, ]
      cat(sprintf("%s%s: AME = %.*f, %s %s\n",
                  indent, r$Term, digits, r$AME,
                  format_p_compact(r$p_value, digits),
                  add_significance_stars(r$p_value)))
    }
  }

  if (x$is_grouped) {
    groups <- unique(x$results[x$group_vars])
    for (gi in seq_len(nrow(groups))) {
      gv <- groups[gi, , drop = FALSE]
      cat(sprintf("[%s]\n", paste(names(gv), "=", unlist(gv), collapse = ", ")))
      rows <- x$results
      for (g in names(gv)) rows <- rows[rows[[g]] == gv[[g]], ]
      print_rows(rows)
    }
  } else {
    print_rows(x$results)
  }

  cat("AME = average change in predicted probability. Use summary() for detailed output.\n")
  invisible(x)
}


# ============================================================================
# SUMMARY METHOD
# ============================================================================

#' Summary method for average marginal effects
#'
#' @description
#' Creates a summary object that produces the detailed AME table
#' (estimate, delta-method SE, z, p, confidence interval) when printed.
#'
#' @param object A \code{marginal_effects} result object.
#' @param effects Logical. Show the AME table? (Default: TRUE)
#' @param digits Number of decimal places for formatting (Default: 3).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.marginal_effects} object.
#'
#' @examples
#' survey_data$high_satisfaction <- as.integer(survey_data$life_satisfaction >= 4)
#' model <- logistic_regression(survey_data, high_satisfaction ~ age + income)
#' summary(marginal_effects(model))
#'
#' @seealso \code{\link{marginal_effects}} for the main analysis function.
#' @export
#' @method summary marginal_effects
summary.marginal_effects <- function(object, effects = TRUE, digits = 3, ...) {
  build_summary_object(
    object     = object,
    show       = list(effects = effects),
    digits     = digits,
    class_name = "summary.marginal_effects"
  )
}


#' Print summary of average marginal effects (detailed output)
#'
#' @description
#' Displays the detailed AME table for a \code{\link{marginal_effects}}
#' result, per group for grouped models.
#'
#' @param x A \code{summary.marginal_effects} object created by
#'   \code{\link{summary.marginal_effects}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' survey_data$high_satisfaction <- as.integer(survey_data$life_satisfaction >= 4)
#' model <- logistic_regression(survey_data, high_satisfaction ~ age + income)
#' summary(marginal_effects(model))
#'
#' @seealso \code{\link{marginal_effects}} for the main analysis,
#'   \code{\link{summary.marginal_effects}} for summary options.
#' @export
#' @method print summary.marginal_effects
print.summary.marginal_effects <- function(x, ...) {
  digits <- x$digits %||% 3

  title <- get_standard_title("Average Marginal Effects", x$weight_name, "Results")
  print_header(title)

  info <- list(
    "Formula" = deparse(x$formula),
    "Scale" = "Predicted probability",
    "Std. errors" = "Delta method"
  )
  if (isTRUE(x$weighted)) info[["Weights"]] <- x$weight_name
  if (x$is_grouped) {
    info[["Grouped by"]] <- paste(x$group_vars, collapse = ", ")
  } else {
    info[["N"]] <- x$n
  }
  print_info_section(info)

  if (isTRUE(x$show$effects)) {
    stat_cols <- c("Term", "AME", "SE", "z", "p_value", "CI_lower", "CI_upper")
    labels <- c(p_value = "p", CI_lower = "CI Lower", CI_upper = "CI Upper")

    emit <- function(rows) {
      df <- rows[stat_cols]
      df$sig <- add_significance_stars(df$p_value)
      format_stat_table(df, digits = digits, col_labels = labels)
    }

    cat("\n")
    if (x$is_grouped) {
      for_each_group(x$results, x$group_vars, function(rows, group_values) {
        emit(rows)
      })
    } else {
      emit(x$results)
    }

    cat("\nFactor rows show the average discrete change vs. the reference level.\n")
    print_significance_legend(TRUE)
  }

  invisible(x)
}
