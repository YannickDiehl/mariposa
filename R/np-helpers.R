# =============================================================================
# Shared helpers for the non-parametric and categorical tests
# =============================================================================
# Used by mann_whitney, kruskal_wallis, wilcoxon_test, friedman_test,
# binomial_test, chisq_gof, fisher_test, mcnemar_test, chi_square,
# dunn_test and pairwise_wilcoxon.
# =============================================================================

#' " in group var = value" suffix for warnings about one split
#'
#' @param key One-row data frame of group values (from group_keys()/
#'   group_modify()), or NULL for ungrouped data
#' @return Character scalar ("" when ungrouped)
#' @noRd
.np_where <- function(key) {
  if (is.null(key) || length(key) == 0 || NROW(key) == 0) return("")
  paste0(" in group ", .format_group_label(key))
}

#' Observed categories of a categorical variable, SPSS style
#'
#' SPSS tabulates the categories that occur in the data, in code order, and
#' shows value labels. Factor levels without cases (e.g. after filter())
#' are dropped - an empty level used to produce NaN statistics or a phantom
#' category with an extra degree of freedom. Labelled variables become a
#' factor of their value labels (codes without a label keep the code).
#'
#' @param x Vector (factor, haven_labelled, character, numeric, logical)
#' @return A factor without unused levels
#' @noRd
.np_factor <- function(x) {
  droplevels(.group_factor(x))
}

#' Values to rank for the rank tests
#'
#' Ordered factors are ranked by their level order (integer codes), as
#' kruskal_wallis()/friedman_test() effectively did and SPSS does with the
#' codes of an ordinal variable; labelled variables by their numeric codes
#' (tagged NAs become NA); logicals as 0/1.
#'
#' @param x Vector
#' @return Numeric vector
#' @noRd
.np_rank_values <- function(x) {
  if (is.ordered(x)) return(as.integer(x))
  if (inherits(x, "haven_labelled")) return(.plain_numeric(x))
  if (is.logical(x)) return(as.numeric(x))
  x
}

#' Check that the variables of a rank test are ordinal or metric
#'
#' @param data Data frame
#' @param vars Variable names
#' @return invisible(TRUE); aborts for nominal/character variables
#' @noRd
.np_check_rank_vars <- function(data, vars, call = rlang::caller_env()) {
  for (v in vars) {
    x <- data[[v]]
    if (is.ordered(x) || is.logical(x) || (is.numeric(x) && !is.factor(x))) next
    kind <- if (is.factor(x)) {
      "a nominal (unordered) factor"
    } else {
      paste0("of type <", class(x)[1], ">")
    }
    cli_abort(c(
      "{.var {v}} is {kind}; rank tests need a numeric variable or an ordered factor.",
      "i" = "For ordinal categories use {.code factor(x, levels = ..., ordered = TRUE)}; for nominal variables use {.fn chi_square}."
    ), call = call)
  }
  invisible(TRUE)
}

#' Codes behind the levels of a .np_factor() result
#'
#' @param x The original vector
#' @param levels Levels of the .np_factor() result
#' @return Character vector of codes (NULL when x is not labelled)
#' @noRd
.np_codes <- function(x, levels) {
  if (!inherits(x, "haven_labelled")) return(NULL)
  v <- .plain_numeric(x)
  vals <- sort(unique(v[!is.na(v)]))
  f <- .group_factor(x)
  as.character(vals[match(levels, levels(f))])
}

#' Grouping columns of a test result
#'
#' Results store their group_by() columns in `$groups`; objects created
#' before that field existed fall back to "all columns that are not result
#' columns".
#'
#' @param x Result object
#' @param result_cols Names of the non-group columns of `x$results`
#' @return Character vector
#' @noRd
.np_group_cols <- function(x, result_cols) {
  if (!is.null(x$groups)) return(x$groups)
  setdiff(names(x$results), c(result_cols, "reason", "sig"))
}

#' "not computed" text for a skipped result row
#'
#' @param results Results data frame
#' @param i Row index
#' @param grouped Is the result grouped (group_by())?
#' @return e.g. "not computed for this group (x has no valid values)"
#' @noRd
.np_not_computed <- function(results, i, grouped) {
  sprintf("not computed%s (%s)", if (isTRUE(grouped)) " for this group" else "",
          .np_reason(results, i))
}

#' Check the values of one variable before a rank test
#'
#' @param x Values after removing missing cases
#' @param var_name Variable name
#' @return invisible(TRUE); aborts with a reason otherwise
#' @noRd
.np_check_values <- function(x, var_name) {
  if (length(x) == 0) {
    cli_abort("{.var {var_name}} has no valid values")
  }
  if (length(unique(x)) < 2) {
    cli_abort("all values of {.var {var_name}} are identical")
  }
  invisible(TRUE)
}

#' Verbal labels for the effect sizes of the rank tests
#'
#' One set of thresholds per measure, shared by the compact print and the
#' summary legends (mann_whitney and wilcoxon_test used different ones).
#'
#' @param x Effect size
#' @return Lower-case label ("-" for NA)
#' @noRd
.interpret_r_effect <- function(x) {
  if (is.na(x)) return("-")
  x <- abs(x)
  if (x < 0.1) "negligible" else if (x < 0.3) "small" else
    if (x < 0.5) "medium" else "large"
}

#' Verbal label for epsilon-squared (Kruskal-Wallis)
#' @noRd
.interpret_epsilon2 <- function(x) {
  if (is.na(x)) return("-")
  if (x < 0.01) "negligible" else if (x < 0.06) "small" else
    if (x < 0.14) "medium" else "large"
}

#' Verbal label for Kendall's W (Friedman)
#' @noRd
.interpret_kendall_w <- function(x) {
  if (is.na(x)) return("-")
  if (x < 0.1) "negligible" else if (x < 0.3) "weak" else
    if (x < 0.5) "moderate" else "strong"
}

#' Legend of the r effect-size labels (mann_whitney, wilcoxon_test)
#' @noRd
.print_r_effect_legend <- function() {
  cat("\nEffect Size Interpretation (r):\n")
  cat("- Negligible: |r| < 0.1\n")
  cat("- Small: 0.1 <= |r| < 0.3\n")
  cat("- Medium: 0.3 <= |r| < 0.5\n")
  cat("- Large: |r| >= 0.5\n")
}

#' Format a (possibly weighted) count for display: integer, no decimals
#'
#' fmt_int(): formatC(format = "d") printed NA for sums of weights of 2^31
#' or more.
#' @noRd
.np_count <- function(n) {
  fmt_int(n)
}

#' First line of an error message, unwrapped and without styling
#'
#' conditionMessage() of a cli error is wrapped at the console width and
#' contains the bullet lines; for a one-line "reason" the unformatted
#' header (rlang stores it in `e$message[1]`) is used.
#'
#' @param e Condition
#' @return Character scalar without a trailing period
#' @noRd
.np_error_reason <- function(e) {
  msg <- if (inherits(e, "rlang_error") && length(e$message) > 0) {
    e$message[[1]]
  } else {
    sub("\n.*", "", conditionMessage(e))
  }
  msg <- gsub("\\s+", " ", cli::ansi_strip(msg))
  sub("[.]\\s*$", "", trimws(msg))
}

#' Reason text for a result row that was not computed
#'
#' @param results Results data frame with an optional `reason` column
#' @param i Row index
#' @return Character scalar ("see warning" when no reason is stored)
#' @noRd
.np_reason <- function(results, i) {
  reason <- if ("reason" %in% names(results)) results$reason[i] else NA
  if (is.null(reason) || length(reason) == 0 || is.na(reason) ||
      !nzchar(reason)) "see warning" else reason
}

#' Contingency table of the observed categories (SPSS CROSSTABS style)
#'
#' Categories come from .np_factor() (value labels, code order, empty
#' levels dropped). With weights the cell counts are rounded as SPSS does
#' and rows/columns whose rounded total is 0 are dropped.
#'
#' @param v1,v2 Row and column variable
#' @param w Weights or NULL
#' @param dnn Names of the two dimensions
#' @return A table
#' @noRd
.np_crosstab <- function(v1, v2, w = NULL, dnn) {
  f1 <- .np_factor(v1)
  f2 <- .np_factor(v2)
  ok <- !is.na(f1) & !is.na(f2)
  if (!is.null(w)) {
    ok <- ok & !is.na(w)
    tbl <- round(tapply(w[ok], list(f1[ok], f2[ok]), sum))
    tbl[is.na(tbl)] <- 0
    tbl <- as.table(tbl)
  } else {
    tbl <- table(f1[ok], f2[ok])
  }
  tbl <- tbl[rowSums(tbl) > 0, colSums(tbl) > 0, drop = FALSE]
  names(dimnames(tbl)) <- dnn
  tbl
}

#' Exact binomial p-value as SPSS NPAR TESTS /BINOMIAL reports it
#'
#' Test proportion .5: two-tailed, twice the smaller tail (at most 1), which
#' equals stats::binom.test() for the symmetric distribution. Any other test
#' proportion: one-tailed in the direction of the observed proportion of
#' Group 1 (SPSS footnote "Alternative hypothesis states that the proportion
#' of cases in the first group < p"). Tails come from pbinom(), so the cost
#' does not grow with n (weighted counts in the billions).
#'
#' @param x Count of Group 1
#' @param n Total count
#' @param p Test proportion of Group 1
#' @return list(p_value, alternative = "two.sided", "less" or "greater")
#' @noRd
.binom_exact_p <- function(x, n, p) {
  lower <- stats::pbinom(x, n, p)
  upper <- stats::pbinom(x - 1, n, p, lower.tail = FALSE)
  if (isTRUE(all.equal(p, 0.5))) {
    return(list(p_value = min(1, 2 * min(lower, upper)),
                alternative = "two.sided"))
  }
  if (x / n <= p) {
    list(p_value = lower, alternative = "less")
  } else {
    list(p_value = upper, alternative = "greater")
  }
}

#' Clopper-Pearson confidence interval of a proportion
#'
#' The two-sided exact interval stats::binom.test() reports, computed from
#' the beta quantiles directly.
#'
#' @param x Count of successes
#' @param n Total count
#' @param conf.level Confidence level
#' @return Numeric vector c(lower, upper)
#' @noRd
.clopper_pearson <- function(x, n, conf.level) {
  alpha <- (1 - conf.level) / 2
  c(if (x == 0) 0 else stats::qbeta(alpha, x, n - x + 1),
    if (x == n) 1 else stats::qbeta(1 - alpha, x + 1, n - x))
}
