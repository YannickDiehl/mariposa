# =============================================================================
# BROOM TIDIERS for linear_regression / logistic_regression
# =============================================================================
# These methods are registered conditionally on broom load via .onLoad() in
# R/zzz.R (using rlang::s3_register). They are NOT roxygen-exported, since
# the generic comes from a Suggests-only package — the s3_register pattern
# is the broom-recommended way to plug in tidiers without making broom a
# hard dependency.
#
# Implementation: strip the linear_regression / logistic_regression class so
# the underlying lm / glm methods of broom's tidiers run with summary(x)
# correctly dispatching to summary.lm / summary.glm (instead of our
# specialised SPSS-style summary method). This restores the full broom
# output shape (5 tidy columns, full glance scalars, augment with .fitted
# etc.).
# =============================================================================


# -----------------------------------------------------------------------------
# helpers
# -----------------------------------------------------------------------------

.lr_strip_class <- function(x) {
  class(x) <- setdiff(class(x), "linear_regression")
  x
}

.glr_strip_class <- function(x) {
  class(x) <- setdiff(class(x), "logistic_regression")
  x
}

.lr_broom_require_lm <- function(x, generic_name) {
  if (isTRUE(x$is_grouped)) {
    cli_abort(c(
      "{.code broom::{generic_name}()} is not supported on a grouped {.cls linear_regression}.",
      i = "Each element of {.code x$groups} is itself a fitted model.",
      i = "Use {.code lapply(x$groups, broom::{generic_name}, ...)} for per-group results."
    ))
  }
  if (!inherits(x, "lm")) {
    cli_abort(c(
      "{.code broom::{generic_name}()} requires the underlying {.cls lm} object.",
      i = "Pairwise deletion does not produce a fitted lm.",
      i = "Refit with {.code use = \"listwise\"} to enable broom tidiers."
    ))
  }
}

.glr_broom_require_glm <- function(x, generic_name) {
  if (isTRUE(x$is_grouped)) {
    cli_abort(c(
      "{.code broom::{generic_name}()} is not supported on a grouped {.cls logistic_regression}.",
      i = "Each element of {.code x$groups} is itself a fitted model.",
      i = "Use {.code lapply(x$groups, broom::{generic_name}, ...)} for per-group results."
    ))
  }
  if (!inherits(x, "glm")) {
    cli_abort(c(
      "{.code broom::{generic_name}()} requires the underlying {.cls glm} object.",
      i = "Refit without grouping to enable broom tidiers."
    ))
  }
}


# -----------------------------------------------------------------------------
# linear_regression — tidy / glance / augment
# -----------------------------------------------------------------------------

tidy.linear_regression <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  .lr_broom_require_lm(x, "tidy")
  out <- broom::tidy(.lr_strip_class(x), conf.int = conf.int,
                     conf.level = conf.level, ...)
  if (!is.null(x$spss_weights)) {
    # Weighted: SPSS frequency-weight SE / t / p / CI (as in summary())
    # instead of lm's analytic-weight values
    se <- sqrt(diag(vcov.linear_regression(x)))
    idx <- match(out$term, names(se))
    out$std.error <- unname(se[idx])
    out$statistic <- out$estimate / out$std.error
    out$p.value <- 2 * stats::pt(abs(out$statistic),
                                 df = x$spss_weights$df_residual,
                                 lower.tail = FALSE)
    if (isTRUE(conf.int)) {
      ci <- confint.linear_regression(x, level = conf.level)
      out$conf.low <- unname(ci[idx, 1])
      out$conf.high <- unname(ci[idx, 2])
    }
  }
  out
}

glance.linear_regression <- function(x, ...) {
  .lr_broom_require_lm(x, "glance")
  out <- broom::glance(.lr_strip_class(x), ...)
  if (!is.null(x$spss_weights)) {
    # Weighted: fit statistics under SPSS frequency weights (N = sum(w)),
    # identical to the summary() Model Summary / ANOVA tables
    out$adj.r.squared <- x$model_summary$adj_R_squared
    out$sigma <- x$model_summary$std_error
    out$statistic <- x$anova_table$F_statistic[1]
    out$p.value <- x$anova_table$Sig[1]
    out$df.residual <- x$spss_weights$df_residual
    out$nobs <- x$spss_weights$sum_w
  }
  out
}

augment.linear_regression <- function(x, ...) {
  .lr_broom_require_lm(x, "augment")
  broom::augment(.lr_strip_class(x), ...)
}


# -----------------------------------------------------------------------------
# logistic_regression — tidy / glance / augment
# -----------------------------------------------------------------------------

tidy.logistic_regression <- function(x, conf.int = FALSE, conf.level = 0.95,
                                     exponentiate = FALSE, ...) {
  .glr_broom_require_glm(x, "tidy")
  out <- .glm_quiet_weights(
    broom::tidy(.glr_strip_class(x), conf.int = FALSE,
                exponentiate = FALSE, ...)
  )
  if (isTRUE(conf.int)) {
    # Wald intervals - the SPSS "C.I. for EXP(B)" that summary() prints
    # (see confint.logistic_regression). broom's glm tidier would profile
    # the likelihood instead: different numbers, and one non-integer
    # warning per profiling fit for weighted models.
    ci <- confint.logistic_regression(x, level = conf.level)
    idx <- match(out$term, rownames(ci))
    out$conf.low <- unname(ci[idx, 1])
    out$conf.high <- unname(ci[idx, 2])
  }
  if (isTRUE(exponentiate)) {
    out$estimate <- exp(out$estimate)
    if (isTRUE(conf.int)) {
      out$conf.low <- exp(out$conf.low)
      out$conf.high <- exp(out$conf.high)
    }
  }
  out
}

glance.logistic_regression <- function(x, ...) {
  .glr_broom_require_glm(x, "glance")
  broom::glance(.glr_strip_class(x), ...)
}

augment.logistic_regression <- function(x, ...) {
  .glr_broom_require_glm(x, "augment")
  broom::augment(.glr_strip_class(x), ...)
}
