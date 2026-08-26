#' Partial Correlation
#'
#' @description
#' \code{partial_cor()} computes partial correlations between variables
#' while controlling for one or more other variables — the SPSS
#' \code{PARTIAL CORR} procedure (Stata: \code{pcorr}). It answers the
#' question "how strongly are x and y related once the influence of the
#' control variables is removed?"
#'
#' @param data Your survey data (a data frame or tibble). If grouped
#'   (via \code{dplyr::group_by()}), separate partial correlations are
#'   computed per group (SPSS SPLIT FILE).
#' @param ... Variables to correlate (unquoted, supports tidyselect).
#'   At least two.
#' @param controls Control variable(s) to partial out (unquoted, supports
#'   tidyselect; SPSS \code{BY} list). At least one, must not overlap
#'   with the analysis variables.
#' @param weights Optional survey weights (unquoted variable name),
#'   treated as frequency weights matching SPSS \code{WEIGHT BY}.
#'
#' @return An object of class \code{"partial_cor"} whose
#'   \code{$correlations} tibble holds one row per variable pair (and
#'   group combination) with:
#' \describe{
#'   \item{partial_r}{Partial correlation controlling for the
#'     \code{controls}}
#'   \item{zero_order_r}{The ordinary (zero-order) Pearson correlation of
#'     the pair, for comparison — SPSS \code{/STATISTICS=CORR}}
#'   \item{df}{Degrees of freedom, n - 2 - k with k control variables
#'     (non-integer for weighted data, Charter §5.1)}
#'   \item{t_stat, p_value}{t-test of the partial correlation
#'     (two-tailed, the SPSS default)}
#'   \item{n}{Listwise-complete sample size (rounded weighted N when
#'     weighted)}
#' }
#'   For three or more analysis variables, \code{$matrices} additionally
#'   holds the full partial-correlation matrix per group.
#'
#' @details
#' ## Understanding the Output
#'
#' Comparing \code{partial_r} against \code{zero_order_r} tells you what
#' the controls contribute:
#' \itemize{
#'   \item \strong{Partial clearly smaller than zero-order}: much of the
#'     original association runs through the control variables
#'     (confounding or mediation).
#'   \item \strong{Partial similar to zero-order}: the association is
#'     largely independent of the controls.
#'   \item \strong{Partial larger than zero-order}: a suppressor
#'     situation — the controls masked part of the association.
#' }
#'
#' ## When to Use This
#'
#' \itemize{
#'   \item Check whether a bivariate correlation survives controlling for
#'     demographics (age, education, ...)
#'   \item Separate the direct association of two attitudes from what a
#'     shared cause explains
#' }
#'
#' For full multivariate control with several predictors, use
#' \code{\link{linear_regression}} instead.
#'
#' ## Technical Details
#'
#' Cases are deleted \strong{listwise} across the analysis and control
#' variables (SPSS \code{/MISSING=LISTWISE}, the PARTIAL CORR default).
#' The partial correlation is computed from the Pearson correlation
#' matrix of all variables via the inverse of the control-variable block;
#' the same Pearson formula as \code{\link{pearson_cor}} is used
#' throughout, so weighted results follow the SPSS frequency-weight
#' convention with unrounded \code{sum(w)} in df and test statistics.
#' Significance is two-tailed (the SPSS default).
#'
#' An SPSS v29 PARTIAL CORR reference run is pending; until it lands the
#' statistics are verified against the independent
#' residual-of-regressions characterization (see the SPSS compatibility
#' vignette).
#'
#' @examples
#' library(dplyr)
#' data(survey_data)
#'
#' # Does the satisfaction-income correlation survive controlling for age?
#' partial_cor(survey_data, life_satisfaction, income, controls = age)
#'
#' # Several variables, several controls
#' partial_cor(survey_data, trust_government, trust_media, trust_science,
#'             controls = c(age, political_orientation))
#'
#' # Weighted (SPSS WEIGHT BY)
#' partial_cor(survey_data, life_satisfaction, income,
#'             controls = age, weights = sampling_weight)
#'
#' # Grouped (SPSS SPLIT FILE)
#' survey_data %>%
#'   group_by(gender) %>%
#'   partial_cor(life_satisfaction, income, controls = age)
#'
#' # --- Three-layer output ---
#' result <- partial_cor(survey_data, life_satisfaction, income, controls = age)
#' result              # compact overview
#' summary(result)     # full detailed output
#'
#' @seealso
#' \code{\link{pearson_cor}} for zero-order correlations.
#'
#' \code{\link{linear_regression}} for multivariate control.
#'
#' \code{\link{summary.partial_cor}} for detailed output.
#'
#' @family correlation
#' @export
partial_cor <- function(data, ..., controls, weights = NULL) {
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame or tibble.")
  }

  vars <- .process_variables(data, ...)
  var_names <- names(vars)
  if (length(var_names) < 2) {
    cli_abort("At least two analysis variables must be specified.")
  }

  controls_quo <- rlang::enquo(controls)
  if (rlang::quo_is_missing(controls_quo) || rlang::quo_is_null(controls_quo)) {
    cli_abort(c(
      "{.arg controls} must specify at least one control variable.",
      i = "For correlations without controls, use {.fn pearson_cor}."
    ))
  }
  control_pos <- tidyselect::eval_select(controls_quo, data)
  control_names <- names(control_pos)
  if (length(control_names) == 0) {
    cli_abort("{.arg controls} must specify at least one control variable.")
  }

  overlap <- intersect(var_names, control_names)
  if (length(overlap) > 0) {
    cli_abort("Variable(s) {.var {overlap}} cannot be both analysis and control variables.")
  }

  all_vars <- c(var_names, control_names)
  for (v in all_vars) {
    if (!is.numeric(data[[v]])) {
      cli_abort("Variable {.var {v}} is not numeric. {.fn partial_cor} requires numeric variables.")
    }
  }

  weights_info <- .process_weights(data, rlang::enquo(weights))
  w_name <- weights_info$name

  is_grouped <- inherits(data, "grouped_df")

  if (is_grouped) {
    group_vars <- dplyr::group_vars(data)
    group_split <- dplyr::group_split(data)
    group_keys <- dplyr::group_keys(data)

    per_group <- lapply(seq_along(group_split), function(i) {
      .partial_cor_group(group_split[[i]], var_names, control_names, w_name)
    })
    correlations <- do.call(rbind, lapply(seq_along(per_group), function(i) {
      cbind(group_keys[i, , drop = FALSE], per_group[[i]]$rows)
    }))
    correlations <- tibble::as_tibble(correlations)
    matrices <- lapply(per_group, function(g) g$matrix)
  } else {
    group_vars <- character(0)
    single <- .partial_cor_group(data, var_names, control_names, w_name)
    correlations <- single$rows
    matrices <- list(single$matrix)
  }

  structure(
    list(
      correlations = correlations,
      matrices = matrices,
      variables = var_names,
      controls = control_names,
      weights = w_name,
      is_grouped = is_grouped,
      group_vars = group_vars,
      group_keys = if (is_grouped) group_keys else NULL
    ),
    class = "partial_cor"
  )
}


#' Partial correlations for one data block
#' @noRd
.partial_cor_group <- function(data, var_names, control_names, w_name) {
  all_vars <- c(var_names, control_names)
  k <- length(control_names)

  # Listwise deletion across analysis + control variables (SPSS
  # /MISSING=LISTWISE); zero-weight cases are excluded like in the
  # Pearson kernel.
  complete <- stats::complete.cases(data[, all_vars, drop = FALSE])
  w <- if (!is.null(w_name)) data[[w_name]] else NULL
  if (!is.null(w)) {
    complete <- complete & !is.na(w) & w > 0
    w <- w[complete]
  }
  data <- data[complete, , drop = FALSE]

  # Unrounded weighted N for df (Charter §5.1); rounded for display.
  n_eff <- if (!is.null(w)) sum(w) else nrow(data)
  n_display <- if (!is.null(w)) round(n_eff) else nrow(data)

  # Pearson correlation matrix over all variables, using the same pair
  # kernel as pearson_cor() (single home of the weighted formula).
  p_all <- length(all_vars)
  R <- diag(1, p_all)
  dimnames(R) <- list(all_vars, all_vars)
  for (i in seq_len(p_all - 1)) {
    for (j in (i + 1):p_all) {
      pr <- .pearson_pair(data[[all_vars[i]]], data[[all_vars[j]]], w)
      R[i, j] <- R[j, i] <- pr$correlation
    }
  }

  c_idx <- match(control_names, all_vars)
  Rcc_inv <- tryCatch(solve(R[c_idx, c_idx, drop = FALSE]),
                      error = function(e) NULL)
  if (is.null(Rcc_inv)) {
    cli_warn("Control-variable correlation matrix is singular; partial correlations are NA.")
  }

  n_vars <- length(var_names)
  partial_mat <- diag(1, n_vars)
  dimnames(partial_mat) <- list(var_names, var_names)

  rows <- list()
  for (i in seq_len(n_vars - 1)) {
    for (j in (i + 1):n_vars) {
      r_xy <- R[var_names[i], var_names[j]]
      if (is.null(Rcc_inv) || is.na(r_xy)) {
        r_p <- NA_real_
      } else {
        b_i <- R[c_idx, var_names[i]]
        b_j <- R[c_idx, var_names[j]]
        num <- r_xy - as.numeric(t(b_i) %*% Rcc_inv %*% b_j)
        den_i <- 1 - as.numeric(t(b_i) %*% Rcc_inv %*% b_i)
        den_j <- 1 - as.numeric(t(b_j) %*% Rcc_inv %*% b_j)
        r_p <- if (den_i > 0 && den_j > 0) num / sqrt(den_i * den_j) else NA_real_
      }
      partial_mat[i, j] <- partial_mat[j, i] <- r_p

      df <- n_eff - 2 - k
      if (!is.na(r_p) && df > 0 && abs(r_p) < 1) {
        t_stat <- r_p * sqrt(df / (1 - r_p^2))
        p_value <- 2 * stats::pt(abs(t_stat), df = df, lower.tail = FALSE)
      } else {
        t_stat <- NA_real_
        p_value <- NA_real_
      }

      rows[[length(rows) + 1]] <- tibble::tibble(
        var1 = var_names[i],
        var2 = var_names[j],
        partial_r = r_p,
        zero_order_r = r_xy,
        df = df,
        t_stat = t_stat,
        p_value = p_value,
        n = n_display
      )
    }
  }

  list(rows = do.call(rbind, rows), matrix = partial_mat)
}


# ============================================================================
# COMPACT PRINT METHOD
# ============================================================================

#' Print partial correlation results (compact)
#'
#' @description
#' Compact print method for objects of class \code{"partial_cor"}:
#' one line per variable pair with the partial correlation, p-value, and
#' the zero-order correlation for comparison.
#'
#' @param x An object of class \code{"partial_cor"} returned by
#'   \code{\link{partial_cor}}.
#' @param digits Number of decimal places (default: 3).
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- partial_cor(survey_data, life_satisfaction, income, controls = age)
#' result              # compact overview
#' summary(result)     # full detailed output
#'
#' @export
#' @method print partial_cor
print.partial_cor <- function(x, digits = 3, ...) {
  weighted_tag <- if (!is.null(x$weights)) " [Weighted]" else ""
  cat(sprintf("Partial Correlation: %s | controlling for %s%s\n",
              paste(x$variables, collapse = ", "),
              paste(x$controls, collapse = ", "),
              weighted_tag))

  print_rows <- function(corrs, indent = "  ") {
    for (i in seq_len(nrow(corrs))) {
      r <- corrs[i, ]
      cat(sprintf("%s%s x %s: partial r = %.*f, %s %s (zero-order r = %.*f), N = %d\n",
                  indent, r$var1, r$var2,
                  digits, r$partial_r,
                  format_p_compact(r$p_value, digits),
                  add_significance_stars(r$p_value),
                  digits, r$zero_order_r,
                  r$n))
    }
  }

  if (x$is_grouped) {
    groups <- unique(x$correlations[x$group_vars])
    for (gi in seq_len(nrow(groups))) {
      gv <- groups[gi, , drop = FALSE]
      cat(sprintf("[%s]\n", paste(names(gv), "=", unlist(gv), collapse = ", ")))
      corrs <- x$correlations
      for (g in names(gv)) corrs <- corrs[corrs[[g]] == gv[[g]], ]
      print_rows(corrs)
    }
  } else {
    print_rows(x$correlations)
  }

  cat("Use summary() for detailed output.\n")
  invisible(x)
}


# ============================================================================
# SUMMARY METHOD
# ============================================================================

#' Summary method for partial correlation results
#'
#' @description
#' Creates a summary object that produces detailed output when printed:
#' the pairwise partial-correlation table (with zero-order comparison,
#' df, t, and p) and — for three or more variables — the
#' partial-correlation matrix.
#'
#' @param object A \code{partial_cor} result object.
#' @param pairwise Logical. Show the pairwise results table? (Default: TRUE)
#' @param matrix Logical. Show the partial-correlation matrix (three or
#'   more analysis variables)? (Default: TRUE)
#' @param digits Number of decimal places for formatting (Default: 3).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.partial_cor} object.
#'
#' @examples
#' result <- partial_cor(survey_data, trust_government, trust_media,
#'                       trust_science, controls = age)
#' summary(result)
#' summary(result, matrix = FALSE)
#'
#' @seealso \code{\link{partial_cor}} for the main analysis function.
#' @export
#' @method summary partial_cor
summary.partial_cor <- function(object, pairwise = TRUE, matrix = TRUE,
                                digits = 3, ...) {
  build_summary_object(
    object     = object,
    show       = list(pairwise = pairwise, matrix = matrix),
    digits     = digits,
    class_name = "summary.partial_cor"
  )
}


#' Print summary of partial correlation results (detailed output)
#'
#' @description
#' Displays the detailed SPSS-style output for a
#' \code{\link{partial_cor}} result, with sections controlled by the
#' boolean parameters passed to \code{\link{summary.partial_cor}}.
#'
#' @param x A \code{summary.partial_cor} object created by
#'   \code{\link{summary.partial_cor}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- partial_cor(survey_data, life_satisfaction, income, controls = age)
#' summary(result)
#'
#' @seealso \code{\link{partial_cor}} for the main analysis,
#'   \code{\link{summary.partial_cor}} for summary options.
#' @export
#' @method print summary.partial_cor
print.summary.partial_cor <- function(x, ...) {
  digits <- x$digits %||% 3
  show_pairwise <- isTRUE(x$show$pairwise)
  show_matrix <- isTRUE(x$show$matrix) && length(x$variables) > 2

  print_header(get_standard_title("Partial Correlation", x$weights, "Results"))

  info <- list(
    "Variables" = paste(x$variables, collapse = ", "),
    "Controlling for" = paste(x$controls, collapse = ", "),
    "Weights Variable" = x$weights,
    "Missing" = "Listwise deletion"
  )
  if (x$is_grouped) {
    info[["Grouped by"]] <- paste(x$group_vars, collapse = ", ")
  }
  print_info_section(info)

  stat_cols <- c("var1", "var2", "partial_r", "zero_order_r", "df",
                 "t_stat", "p_value", "n")
  labels <- c(var1 = "Variable 1", var2 = "Variable 2",
              partial_r = "Partial r", zero_order_r = "Zero-order r",
              t_stat = "t", p_value = "p")

  print_block <- function(corrs, matrix_idx) {
    if (show_matrix) {
      .print_cor_matrix(x$matrices[[matrix_idx]], digits = digits,
                        title = "Partial Correlation Matrix:",
                        type = "correlation")
    }
    if (show_pairwise) {
      cat("\nPairwise Results:\n")
      df <- corrs[stat_cols]
      df$sig <- add_significance_stars(df$p_value)
      format_stat_table(df, digits = digits, col_labels = labels)
    }
  }

  if (x$is_grouped) {
    groups <- unique(x$correlations[x$group_vars])
    for (gi in seq_len(nrow(groups))) {
      cat("\n")
      print_group_header(groups[gi, , drop = FALSE])
      corrs <- x$correlations
      for (g in names(groups)) corrs <- corrs[corrs[[g]] == groups[gi, g][[1]], ]
      print_block(corrs, gi)
    }
  } else {
    print_block(x$correlations, 1)
  }

  if (show_pairwise) {
    print_significance_legend(TRUE)
  }
  invisible(x)
}
