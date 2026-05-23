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
  broom::tidy(.lr_strip_class(x), conf.int = conf.int, conf.level = conf.level, ...)
}

glance.linear_regression <- function(x, ...) {
  .lr_broom_require_lm(x, "glance")
  broom::glance(.lr_strip_class(x), ...)
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
  broom::tidy(.glr_strip_class(x),
              conf.int = conf.int, conf.level = conf.level,
              exponentiate = exponentiate, ...)
}

glance.logistic_regression <- function(x, ...) {
  .glr_broom_require_glm(x, "glance")
  broom::glance(.glr_strip_class(x), ...)
}

augment.logistic_regression <- function(x, ...) {
  .glr_broom_require_glm(x, "augment")
  broom::augment(.glr_strip_class(x), ...)
}
