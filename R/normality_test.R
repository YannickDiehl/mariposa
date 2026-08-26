#' Test Variables for Normality
#'
#' @description
#' \code{normality_test()} checks whether numeric variables follow a normal
#' distribution, producing the two tests SPSS prints in its EXAMINE
#' "Tests of Normality" table:
#' \itemize{
#'   \item \strong{Kolmogorov-Smirnov} with Lilliefors significance
#'     correction (the "Kolmogorov-Smirnov(a)" column in SPSS)
#'   \item \strong{Shapiro-Wilk} (computed for n between 3 and 5000,
#'     matching the SPSS convention)
#' }
#'
#' Use it before a t-test, ANOVA, or Pearson correlation to check the
#' normality assumption, together with \code{\link{levene_test}} for the
#' equal-variances assumption.
#'
#' @param data Your survey data (a data frame or tibble). If grouped
#'   (via \code{dplyr::group_by()}), the tests are run separately per
#'   group — matching SPSS \code{EXAMINE ... BY factor}.
#' @param ... Variables to test (unquoted, supports tidyselect). All
#'   selected variables must be numeric.
#'
#' @return An object of class \code{"normality_test"} whose \code{$results}
#'   tibble holds one row per variable (and group combination) with:
#' \describe{
#'   \item{n}{Number of valid (non-missing) cases}
#'   \item{ks_statistic, ks_df, ks_p}{Kolmogorov-Smirnov statistic with
#'     Lilliefors-corrected p-value (Dallal-Wilkinson approximation);
#'     df equals n as in SPSS}
#'   \item{shapiro_w, shapiro_p}{Shapiro-Wilk W and p-value
#'     (\code{NA} when n < 3 or n > 5000)}
#' }
#'
#' @details
#' ## Understanding the Output
#'
#' For both tests the null hypothesis is "the variable is normally
#' distributed":
#' \itemize{
#'   \item \strong{p >= 0.05}: No significant deviation from normality
#'     detected.
#'   \item \strong{p < 0.05}: The variable deviates significantly from a
#'     normal distribution.
#' }
#'
#' With large survey samples these tests flag even tiny, practically
#' irrelevant deviations. Combine them with the skewness and kurtosis
#' from \code{\link{describe}} before deciding against a parametric test.
#'
#' ## When to Use This
#'
#' \itemize{
#'   \item Before \code{\link{t_test}}, \code{\link{oneway_anova}}, or
#'     \code{\link{pearson_cor}} to check the normality assumption
#'   \item Grouped (via \code{group_by()}) to check normality \emph{within}
#'     each comparison group — the form of the assumption that actually
#'     matters for group comparisons
#' }
#'
#' If normality is clearly violated, consider the rank-based
#' alternatives: \code{\link{mann_whitney}}, \code{\link{kruskal_wallis}},
#' or \code{\link{spearman_rho}}.
#'
#' ## Technical Details
#'
#' The Lilliefors-corrected p-value uses the Dallal-Wilkinson (1986)
#' approximation, the same correction SPSS applies in EXAMINE
#' ("Lilliefors Significance Correction"). Shapiro-Wilk uses
#' \code{stats::shapiro.test()} and is reported for 3 <= n <= 5000,
#' as in SPSS. Cases with missing values are excluded per variable.
#'
#' \strong{Weights}: \code{normality_test()} deliberately takes no
#' \code{weights} argument. Normality checks are a diagnostic of the
#' \emph{sample} distribution, and neither Shapiro-Wilk nor the
#' Lilliefors correction has a well-defined fractional-frequency-weight
#' form. Run the test on the unweighted sample.
#'
#' An SPSS v29 EXAMINE reference run is pending; until it lands the
#' statistics are verified against independent R implementations of the
#' same published formulas (see the SPSS compatibility vignette).
#'
#' @examples
#' library(dplyr)
#' data(survey_data)
#'
#' # Test a single variable
#' normality_test(survey_data, age)
#'
#' # Several variables at once
#' normality_test(survey_data, age, income, life_satisfaction)
#'
#' # Within comparison groups (SPSS: EXAMINE ... BY gender)
#' survey_data %>%
#'   group_by(gender) %>%
#'   normality_test(age, income)
#'
#' # --- Three-layer output ---
#' result <- normality_test(survey_data, age, income)
#' result              # compact overview
#' summary(result)     # full SPSS-style table
#'
#' @references
#' Dallal, G. E., & Wilkinson, L. (1986). An analytic approximation to the
#' distribution of Lilliefors's test statistic for normality.
#' \emph{The American Statistician}, 40(4), 294-296.
#'
#' @seealso
#' \code{\link{levene_test}} for the equal-variances assumption.
#'
#' \code{\link{describe}} for skewness and kurtosis.
#'
#' \code{\link{summary.normality_test}} for detailed output.
#'
#' @family descriptive
#' @export
normality_test <- function(data, ...) {
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame or tibble.")
  }

  vars <- .process_variables(data, ...)
  var_names <- names(vars)

  for (v in var_names) {
    if (!is.numeric(data[[v]])) {
      cli_abort("Variable {.var {v}} is not numeric. {.fn normality_test} requires numeric variables.")
    }
  }

  is_grouped <- inherits(data, "grouped_df")

  if (is_grouped) {
    group_vars <- dplyr::group_vars(data)
    group_split <- dplyr::group_split(data)
    group_keys <- dplyr::group_keys(data)

    results <- do.call(rbind, lapply(seq_along(group_split), function(i) {
      rows <- .normality_rows(group_split[[i]], var_names)
      cbind(group_keys[i, , drop = FALSE], rows)
    }))
    results <- tibble::as_tibble(results)
  } else {
    group_vars <- character(0)
    results <- .normality_rows(data, var_names)
  }

  structure(
    list(
      results = results,
      variables = var_names,
      group_vars = group_vars,
      is_grouped = is_grouped
    ),
    class = "normality_test"
  )
}


#' Compute the per-variable normality-test rows for one data block
#' @noRd
.normality_rows <- function(data, var_names) {
  rows <- lapply(var_names, function(v) {
    x <- data[[v]]
    x <- x[!is.na(x)]
    n <- length(x)

    ks_stat <- ks_p <- sw_w <- sw_p <- NA_real_

    if (n >= 4 && stats::sd(x) > 0) {
      ks <- .lilliefors_test(x)
      ks_stat <- ks$statistic
      ks_p <- ks$p
    }
    if (n >= 3 && n <= 5000 && stats::sd(x) > 0) {
      sw <- tryCatch(stats::shapiro.test(x), error = function(e) NULL)
      if (!is.null(sw)) {
        sw_w <- unname(sw$statistic)
        sw_p <- sw$p.value
      }
    }

    tibble::tibble(
      Variable = v,
      n = n,
      ks_statistic = ks_stat,
      ks_df = n,
      ks_p = ks_p,
      shapiro_w = sw_w,
      shapiro_p = sw_p
    )
  })
  do.call(rbind, rows)
}


#' Kolmogorov-Smirnov test with Lilliefors significance correction
#'
#' Statistic: KS distance of the standardized sample against N(0, 1) with
#' estimated parameters. p-value: Dallal-Wilkinson (1986) analytic
#' approximation, the same correction SPSS EXAMINE reports as
#' "Lilliefors Significance Correction".
#' @noRd
.lilliefors_test <- function(x) {
  x <- sort(x)
  n <- length(x)
  p <- stats::pnorm((x - mean(x)) / stats::sd(x))
  D_plus <- max(seq_len(n) / n - p)
  D_minus <- max(p - (seq_len(n) - 1) / n)
  K <- max(D_plus, D_minus)

  # Dallal-Wilkinson (1986) approximation; for n > 100 the statistic is
  # rescaled to the n = 100 reference as published.
  if (n <= 100) {
    Kd <- K
    nd <- n
  } else {
    Kd <- K * (n / 100)^0.49
    nd <- 100
  }
  pvalue <- exp(-7.01256 * Kd^2 * (nd + 2.78019) +
                  2.99587 * Kd * sqrt(nd + 2.78019) -
                  0.122119 + 0.974598 / sqrt(nd) + 1.67997 / nd)

  # The approximation is accurate for p <= 0.1; above that, use the
  # Stephens (1974) modified-statistic polynomial (as in nortest).
  if (pvalue > 0.1) {
    KK <- (sqrt(n) - 0.01 + 0.85 / sqrt(n)) * K
    pvalue <- if (KK <= 0.302) {
      1
    } else if (KK <= 0.5) {
      2.76773 - 19.828315 * KK + 80.709644 * KK^2 -
        138.55152 * KK^3 + 81.218052 * KK^4
    } else if (KK <= 0.9) {
      -4.901232 + 40.662806 * KK - 97.490286 * KK^2 +
        94.029866 * KK^3 - 32.355711 * KK^4
    } else if (KK <= 1.31) {
      6.198765 - 19.558097 * KK + 23.186922 * KK^2 -
        12.234627 * KK^3 + 2.423045 * KK^4
    } else {
      0
    }
  }

  list(statistic = K, p = min(max(pvalue, 0), 1))
}


# ============================================================================
# COMPACT PRINT METHOD
# ============================================================================

#' Print normality test results (compact)
#'
#' @description
#' Compact print method for objects of class \code{"normality_test"}.
#' Shows one line per variable with both test results. For grouped
#' analyses, only the dimensions are shown — use \code{summary()} for the
#' per-group tables.
#'
#' @param x An object of class \code{"normality_test"} returned by
#'   \code{\link{normality_test}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- normality_test(survey_data, age, income)
#' result              # compact overview
#' summary(result)     # full detailed output
#'
#' @export
#' @method print normality_test
print.normality_test <- function(x, ...) {
  cat(sprintf("Normality Tests: %s\n", paste(x$variables, collapse = ", ")))

  if (x$is_grouped) {
    n_groups <- nrow(unique(x$results[x$group_vars]))
    cat(sprintf("  %d group combination(s) x %d variable(s) [Grouped: %s]\n",
                n_groups, length(x$variables),
                paste(x$group_vars, collapse = ", ")))
  } else {
    for (i in seq_len(nrow(x$results))) {
      r <- x$results[i, ]
      sw_str <- if (is.na(r$shapiro_w)) {
        "Shapiro-Wilk n/a"
      } else {
        sprintf("Shapiro-Wilk W = %.3f, %s", r$shapiro_w,
                format_p_compact(r$shapiro_p))
      }
      ks_str <- if (is.na(r$ks_statistic)) {
        "KS n/a"
      } else {
        sprintf("KS = %.3f, %s", r$ks_statistic, format_p_compact(r$ks_p))
      }
      cat(sprintf("  %s: %s; %s (n = %d)\n", r$Variable, ks_str, sw_str, r$n))
    }
  }

  cat("Use summary() for detailed output.\n")
  invisible(x)
}


# ============================================================================
# SUMMARY METHOD
# ============================================================================

#' Summary method for normality test results
#'
#' @description
#' Creates a summary object that produces the detailed SPSS-style
#' "Tests of Normality" table when printed.
#'
#' @param object A \code{normality_test} result object.
#' @param tests Logical. Show the tests-of-normality table? (Default: TRUE)
#' @param digits Number of decimal places for formatting (Default: 3).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.normality_test} object.
#'
#' @examples
#' result <- normality_test(survey_data, age, income)
#' summary(result)
#'
#' @seealso \code{\link{normality_test}} for the main analysis function.
#' @export
#' @method summary normality_test
summary.normality_test <- function(object, tests = TRUE, digits = 3, ...) {
  build_summary_object(
    object     = object,
    show       = list(tests = tests),
    digits     = digits,
    class_name = "summary.normality_test"
  )
}


#' Print summary of normality test results (detailed output)
#'
#' @description
#' Displays the SPSS-style "Tests of Normality" table (Kolmogorov-Smirnov
#' with Lilliefors correction, Shapiro-Wilk) for a
#' \code{\link{normality_test}} result, per group for grouped data.
#'
#' @param x A \code{summary.normality_test} object created by
#'   \code{\link{summary.normality_test}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- normality_test(survey_data, age, income)
#' summary(result)
#'
#' @seealso \code{\link{normality_test}} for the main analysis,
#'   \code{\link{summary.normality_test}} for summary options.
#' @export
#' @method print summary.normality_test
print.summary.normality_test <- function(x, ...) {
  digits <- x$digits %||% 3

  print_header(get_standard_title("Normality Tests", NULL, "Results"))

  info <- list("Variables" = paste(x$variables, collapse = ", "))
  if (x$is_grouped) {
    info[["Grouped by"]] <- paste(x$group_vars, collapse = ", ")
  }
  print_info_section(info)

  if (isTRUE(x$show$tests)) {
    stat_cols <- c("Variable", "ks_statistic", "ks_df", "ks_p",
                   "shapiro_w", "shapiro_p")
    labels <- c(ks_statistic = "KS", ks_df = "df", ks_p = "KS p",
                shapiro_w = "W", shapiro_p = "W p")

    cat("\nTests of Normality\n")
    if (x$is_grouped) {
      for_each_group(x$results, x$group_vars, function(rows, group_values) {
        format_stat_table(rows[stat_cols], digits = digits,
                          col_types = c(ks_p = "pvalue", shapiro_p = "pvalue"),
                          col_labels = labels)
      })
    } else {
      format_stat_table(x$results[stat_cols], digits = digits,
                        col_types = c(ks_p = "pvalue", shapiro_p = "pvalue"),
                        col_labels = labels)
    }

    cat("\nKS = Kolmogorov-Smirnov statistic with Lilliefors significance correction.\n")
    cat("W = Shapiro-Wilk statistic (computed for 3 <= n <= 5000, as in SPSS).\n")
    cat("p < 0.05 indicates a significant deviation from normality.\n")
  }

  invisible(x)
}
