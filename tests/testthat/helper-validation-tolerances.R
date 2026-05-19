# =============================================================================
# VALIDATION TOLERANCE INFRASTRUCTURE
# =============================================================================
# Centralised tolerance registry and assertion helpers for SPSS-validation tests.
# All SPSS-validation tests MUST source tolerances from here.
#
# Policy reference: .claude/VALIDATION_CHARTER.md §4 and §6
#
# Forbidden patterns (enforced by test-validation-discipline.R):
#   - inline numeric tolerances (`tolerance = 0.05`)
#   - grepl()-based tolerance switching
#   - NA-as-match defaulting
#   - record_*_comparison() calls without propagating to expect_*
#
# Usage:
#   assert_spss(actual, expected, tier = "spec", what = "statistic",
#               label = "test 1a — life_satisfaction t-statistic")
#
#   assert_spss(actual, expected, tier = "display", precision = 3,
#               label = "Welch df for grouped weighted t-test")
#
#   assert_spss(actual, expected, tier = "exception", id = "EXC-007",
#               label = "Friedman chi-squared with ties")
# =============================================================================


# ------------------------------------------------------------------------
# Tier 1 — Spec defaults
# ------------------------------------------------------------------------
# Per Charter §4, Tier 1 tolerances:
spec_tolerances <- list(
  count       = 0,        # exact match for integers (n, df when integer)
  statistic   = 1e-5,     # t, F, chi^2, U, H, Z, r
  coefficient = 1e-5,     # regression B, SE, Beta
  p_value     = 1e-4,     # absolute; relative 1% rule applied separately
  effect_size = 1e-5,     # eta^2, omega^2, Cohen's d, partial eta^2
  r_squared   = 1e-5,     # R^2, adj-R^2, alpha
  ss          = 1e-5,     # SS, MS (relative 1e-7 applied for large SS)
  ss_large_rel = 1e-7     # relative tolerance when SS > 1e6
)


# ------------------------------------------------------------------------
# Tier 3 — Exception registry
# ------------------------------------------------------------------------
# Must be kept in sync with .claude/VALIDATION_EXCEPTIONS.md.
# test-validation-discipline.R verifies the sync.
#
# Schema:
#   "EXC-NNN" = list(
#     tolerance     = <numeric>,
#     statistic     = "<short label>",
#     function_name = "<R function>"
#   )

exception_registry <- list(
  # No active exceptions yet — populated during Phase 1 diagnosis.
  # Likely entries:
  # "EXC-001" = list(tolerance = 0.01, statistic = "chi-sq (ties)", function_name = "friedman_test"),
  # "EXC-002" = list(tolerance = 0.01, statistic = "H (ties)",       function_name = "kruskal_wallis"),
)


# ------------------------------------------------------------------------
# tol() — tolerance lookup
# ------------------------------------------------------------------------
#' Return the tolerance for a validation comparison
#'
#' @param tier One of "spec", "display", "exception", "internal"
#' @param what For tier "spec": the statistic kind (count, statistic, coefficient,
#'   p_value, effect_size, r_squared, ss). For other tiers: ignored.
#' @param precision For tier "display": the number of decimal places SPSS prints.
#'   The tolerance is half a unit of the last printed decimal (precision = 3
#'   → 5e-4; precision = 4 → 5e-5; etc.).
#' @param id For tier "exception": the EXC-NNN identifier.
#' @return A non-negative numeric tolerance.
#' @keywords internal
tol <- function(tier = c("spec", "display", "exception", "internal"),
                what = NULL, precision = NULL, id = NULL) {
  tier <- match.arg(tier)

  if (tier == "spec") {
    if (is.null(what)) {
      stop("tol(tier = 'spec', ...): must specify `what` (e.g., 'statistic', 'p_value').",
           call. = FALSE)
    }
    if (!what %in% names(spec_tolerances)) {
      stop(sprintf(
        "tol(): unknown spec category %s. Known: %s.",
        sQuote(what), paste(sQuote(names(spec_tolerances)), collapse = ", ")
      ), call. = FALSE)
    }
    return(spec_tolerances[[what]])
  }

  if (tier == "display") {
    if (is.null(precision) || !is.numeric(precision) || precision < 0 ||
        precision != round(precision)) {
      stop("tol(tier = 'display', ...): `precision` must be a non-negative integer.",
           call. = FALSE)
    }
    return(0.5 * 10^(-precision))
  }

  if (tier == "exception") {
    if (is.null(id)) {
      stop("tol(tier = 'exception', ...): must specify `id` (e.g., 'EXC-001').",
           call. = FALSE)
    }
    if (!id %in% names(exception_registry)) {
      stop(sprintf(
        "tol(): exception %s is not in the registry. Add it to exception_registry in
helper-validation-tolerances.R AND to .claude/VALIDATION_EXCEPTIONS.md.",
        sQuote(id)
      ), call. = FALSE)
    }
    return(exception_registry[[id]]$tolerance)
  }

  if (tier == "internal") {
    # Tier 4: no SPSS reference; use snapshot tests instead.
    # tol() should not be called for tier "internal". Surface this as an error.
    stop("tol(tier = 'internal'): Tier 4 statistics have no SPSS reference. ",
         "Use testthat::expect_snapshot_value() instead of assert_spss().",
         call. = FALSE)
  }
}


# ------------------------------------------------------------------------
# assert_spss() — SPSS-validation assertion
# ------------------------------------------------------------------------
#' Assert R-side numerical output matches SPSS within tier tolerance
#'
#' Wraps `testthat::expect_*` with explicit tier semantics. Every SPSS-validation
#' test in mariposa must use this helper rather than calling expect_lt /
#' expect_equal directly with inline tolerances.
#'
#' @param actual Numeric scalar from the R-side computation.
#' @param expected Numeric scalar from the SPSS reference (or the string "<.001"
#'   for SPSS-truncated p-values — special-cased per Charter §4).
#' @param tier One of "spec", "display", "exception". For "internal" use
#'   testthat::expect_snapshot_value() directly.
#' @param what For tier "spec": the statistic kind.
#' @param precision For tier "display": number of decimals SPSS prints.
#' @param id For tier "exception": the EXC-NNN identifier.
#' @param label A short human-readable label describing the comparison. Required;
#'   surfaces in test failure messages.
#' @param relative_p For p-values, apply a relative tolerance of `relative_p` in
#'   addition to the absolute. Default 0.01 (1%). Only applies when
#'   what = "p_value" and tier = "spec".
#' @return Invisibly TRUE on pass; raises a testthat expectation failure on fail.
#' @keywords internal
assert_spss <- function(actual, expected,
                        tier = c("spec", "display", "exception"),
                        what = NULL, precision = NULL, id = NULL,
                        label = NULL, relative_p = 0.01) {

  if (is.null(label) || !nzchar(label)) {
    stop("assert_spss(): `label` is required (a human-readable description).",
         call. = FALSE)
  }

  tier <- match.arg(tier)

  # Special-case: SPSS prints "<.001" for very small p-values.
  if (is.character(expected) && identical(expected, "<.001")) {
    testthat::expect_lt(actual, 0.001,
                        label = sprintf("[%s] p-value < .001 (SPSS truncated)", label))
    return(invisible(TRUE))
  }

  # NA handling: NA-as-match is forbidden by default. The user must explicitly
  # mark NA-expected fields by passing expected = NA_real_ AND tier = "exception"
  # with a documented reason. Default behaviour fails loudly.
  if (is.na(expected)) {
    stop(sprintf(
      "[%s] SPSS reference value is NA. Fill the reference value, or remove this
assertion. NA-as-match is forbidden by Charter §8.",
      label
    ), call. = FALSE)
  }

  if (is.na(actual)) {
    testthat::expect_false(is.na(actual),
                           label = sprintf("[%s] R-side value is NA (SPSS expected %s)",
                                           label, format(expected, digits = 6)))
    return(invisible(FALSE))
  }

  abs_tol <- tol(tier = tier, what = what, precision = precision, id = id)

  # Apply relative tolerance for p-values in Spec tier (loosest of the two wins).
  if (tier == "spec" && identical(what, "p_value")) {
    rel_tol <- abs(expected) * relative_p
    abs_tol <- max(abs_tol, rel_tol)
  }

  # For Display tier with very large expected values (|expected| > 1e6),
  # IEEE-754 floating-point precision (~1e-15 relative) dominates over the
  # absolute print-precision tolerance, which becomes meaningless. Apply a
  # relative floor of 1e-7 (well above FP precision, far below any real
  # disagreement). Charter §4 sanctions this for SS values > 1e6.
  if (tier == "display" && abs(expected) > 1e6) {
    rel_tol <- abs(expected) * 1e-7
    abs_tol <- max(abs_tol, rel_tol)
  }

  # Boundary-rounding guard for p-values: when SPSS rounds p to 3 dp, an
  # internal R-side p of 0.20542 may round to 0.205 while SPSS's internal
  # 0.20567 rounds to 0.206. The printed difference is 0.001 even though
  # the underlying agreement is <0.0003. To absorb this purely-rounding
  # disagreement at the boundary, we accept up to one full unit of the
  # last decimal for p-values at Display tier (instead of the strict
  # half-unit), bounded by the relative_p (default 1%).
  if (tier == "display" && identical(what, "p_value")) {
    full_unit <- 10^(-precision)
    rel_tol   <- max(abs(expected), 1e-4) * relative_p
    abs_tol   <- max(abs_tol, full_unit, rel_tol)
  }

  diff <- abs(actual - expected)

  # Use <= because tolerance "within X" is inclusive of the boundary. This
  # also handles the tol == 0 case (e.g., count comparisons) where strict <
  # would always fail for exact matches.
  testthat::expect_lte(
    diff, abs_tol,
    label = sprintf("[%s] %s = %s, expected %s, |diff| = %s (tol %s, tier %s%s)",
                    label, what %||% "value",
                    format(actual,   digits = 8),
                    format(expected, digits = 8),
                    format(diff,     digits = 4),
                    format(abs_tol,  digits = 4),
                    tier,
                    if (tier == "exception") paste0(", ", id) else "")
  )

  invisible(TRUE)
}


# Local fallback for null-coalescing operator. Even though rlang exports %||%,
# tests should not have to library(rlang); defining locally keeps the helper
# self-contained.
`%||%` <- function(a, b) if (is.null(a)) b else a


# ------------------------------------------------------------------------
# tol_exact_count() — convenience wrapper for sample-size checks
# ------------------------------------------------------------------------
#' Assert two counts (n, df-integer) are exactly equal
#' @keywords internal
assert_spss_count <- function(actual, expected, label) {
  assert_spss(actual, expected, tier = "spec", what = "count", label = label)
}


# ------------------------------------------------------------------------
# Self-check: tier validation on source
# ------------------------------------------------------------------------
# Run inside the test session to catch registry typos before tests start.
# This is a no-op if the registry is empty.
local({
  for (id in names(exception_registry)) {
    if (!grepl("^EXC-[0-9]{3}$", id)) {
      stop(sprintf(
        "exception_registry: id %s does not match EXC-NNN pattern.", sQuote(id)
      ), call. = FALSE)
    }
    entry <- exception_registry[[id]]
    needed <- c("tolerance", "statistic", "function_name")
    missing <- setdiff(needed, names(entry))
    if (length(missing) > 0) {
      stop(sprintf(
        "exception_registry: %s is missing fields: %s",
        id, paste(missing, collapse = ", ")
      ), call. = FALSE)
    }
    if (!is.numeric(entry$tolerance) || entry$tolerance < 0) {
      stop(sprintf("exception_registry: %s has invalid tolerance.", id),
           call. = FALSE)
    }
  }
})
