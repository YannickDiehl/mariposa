
#' Test If Two Categories Are Related
#'
#' @description
#' \code{chi_square()} helps you discover if two categorical variables are
#' related or independent. For example, is education level related to voting
#' preference? Or are they independent of each other?
#'
#' The test tells you:
#' - Whether the relationship is statistically significant
#' - How strong the relationship is (effect sizes)
#' - What patterns exist in your data
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... Two categorical variables to test (e.g., gender, region)
#' @param weights Optional survey weights for population-representative results
#' @param correct Apply continuity correction for small samples? (Default: FALSE)
#'
#' @return Test results showing whether the variables are related, including:
#' - Chi-squared statistic and p-value
#' - Observed vs expected frequencies
#' - Effect sizes to measure relationship strength
#'   Use \code{summary()} for the full SPSS-style output with toggleable sections.
#'
#' @details
#' ## Understanding the Results
#'
#' **P-value**: If p < 0.05, the variables are likely related (not independent)
#' - p < 0.001: Very strong evidence of relationship
#' - p < 0.01: Strong evidence of relationship
#' - p < 0.05: Moderate evidence of relationship
#' - p >= 0.05: No significant relationship found
#'
#' **Effect Sizes** (How strong is the relationship?):
#' - **Cramer's V**: Works for any table size (0 = no relationship, 1 = perfect relationship)
#'   - < 0.1: Negligible relationship
#'   - 0.1-0.3: Small relationship
#'   - 0.3-0.5: Medium relationship
#'   - 0.5 or higher: Large relationship
#' - **Phi**: Only for 2x2 tables (similar interpretation as Cramer's V)
#' - **Gamma**: For ordinal data (-1 to +1, shows direction of relationship)
#'
#' ## When to Use This
#'
#' Use chi-squared test when:
#' - Both variables are categorical (gender, region, education level, etc.)
#' - You want to know if they're related or independent
#' - You have at least 5 observations in most cells
#'
#' ## Reading the Frequency Tables
#'
#' - **Observed**: What you actually found in your data
#' - **Expected**: What you'd expect if variables were independent
#' - Large differences suggest a relationship exists
#'
#' ## Tips for Success
#'
#' - Check that most cells have at least 5 observations
#' - Use weights for population estimates
#' - Look at both significance (p-value) and strength (effect sizes)
#' - Consider using crosstab() for detailed percentage breakdowns
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#' 
#' # Basic chi-squared test for independence
#' survey_data %>% chi_square(gender, region)
#' 
#' # With weights
#' survey_data %>% chi_square(gender, education, weights = sampling_weight)
#' 
#' # Grouped analysis
#' survey_data %>% 
#'   group_by(region) %>% 
#'   chi_square(gender, employment)
#' 
#' # With continuity correction
#' survey_data %>% chi_square(gender, region, correct = TRUE)
#'
#' # --- Three-layer output ---
#' result <- chi_square(survey_data, gender, education)
#' result              # compact one-line overview
#' summary(result)     # full detailed output with all sections
#' summary(result, cross_tabulation = FALSE)  # hide cross-tabulation
#'
#' @seealso
#' \code{\link[stats]{chisq.test}} for the base R chi-squared test.
#'
#' \code{\link{crosstab}} for detailed cross-tabulation tables.
#'
#' \code{\link{frequency}} for single-variable frequency tables.
#'
#' \code{\link{summary.chi_square}} for detailed output with toggleable sections.
#'
#' @references
#' Pearson, K. (1900). On the criterion that a given system of deviations from
#' the probable in the case of a correlated system of variables is such that it
#' can be reasonably supposed to have arisen from random sampling.
#' \emph{Philosophical Magazine}, 50(302), 157--175.
#'
#' Cramer, H. (1946). \emph{Mathematical Methods of Statistics}. Princeton
#' University Press.
#'
#' IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.
#'
#' @family hypothesis_tests
#' @export
chi_square <- function(data, ..., weights = NULL, correct = FALSE) {
  
  # Get data structure
  is_grouped <- inherits(data, "grouped_df")
  group_vars <- if (is_grouped) dplyr::group_vars(data) else NULL

  # Select variables using centralized helper
  vars <- .process_variables(data, ...)
  var_names <- names(vars)

  if (length(var_names) != 2) {
    cli_abort("Exactly two variables must be specified for {.fn chi_square}.")
  }

  # Process weights using centralized helper
  weights_info <- .process_weights(data, rlang::enquo(weights))
  data <- weights_info$data
  w_name <- weights_info$name

  # One test per data slice (the whole data or one group_by() group)
  run_slice <- function(slice, key = NULL) {
    w <- if (!is.null(w_name)) slice[[w_name]] else NULL
    .chi_square_one(slice[[var_names[1]]], slice[[var_names[2]]], w,
                    correct = correct, var_names = var_names, key = key)
  }

  if (is_grouped) {
    data_list <- dplyr::group_split(data)
    group_keys <- dplyr::group_keys(data)
    results_list <- lapply(seq_along(data_list), function(i) {
      key <- group_keys[i, , drop = FALSE]
      cbind(key, run_slice(data_list[[i]], key))
    })
    results_df <- do.call(rbind, results_list)
    rownames(results_df) <- NULL
  } else {
    results_df <- run_slice(data)
  }

  # Create result object
  result <- list(
    results = results_df,
    variables = var_names,
    weights = w_name,
    correct = correct,
    is_grouped = is_grouped,
    groups = group_vars
  )
  
  class(result) <- "chi_square"
  return(result)
}

#' Chi-square test and effect sizes for one data slice
#'
#' Builds the contingency table from the observed categories only (SPSS
#' CROSSTABS: empty factor levels, e.g. left over after filter(), are not
#' categories; with weights the cell counts are rounded as SPSS does and a
#' category whose rounded count is 0 is dropped). A table with fewer than
#' two rows or columns (constant variable) is not testable: the row is
#' returned with NA statistics, a `reason`, and a warning naming the
#' variables and group.
#'
#' @param v1,v2 The two variables (one slice)
#' @param w Weights or NULL
#' @param correct Yates continuity correction (2x2 only, as chisq.test)
#' @param var_names Names of the two variables
#' @param key One-row group key or NULL
#' @return One-row data frame
#' @noRd
.chi_square_one <- function(v1, v2, w, correct, var_names, key = NULL) {
  where <- .np_where(key)
  f1 <- .np_factor(v1)
  f2 <- .np_factor(v2)
  ok <- !is.na(f1) & !is.na(f2)
  if (!is.null(w)) {
    ok <- ok & !is.na(w)  # missing weight: case excluded
    w_ok <- w[ok]
    g1 <- f1[ok]
    g2 <- f2[ok]
    tbl <- round(stats::xtabs(w_ok ~ g1 + g2))  # SPSS rounds cell counts
  } else {
    tbl <- table(f1[ok], f2[ok])
  }
  tbl <- tbl[rowSums(tbl) > 0, colSums(tbl) > 0, drop = FALSE]
  names(dimnames(tbl)) <- var_names
  n <- sum(tbl)
  r <- nrow(tbl)
  c <- ncol(tbl)

  empty_row <- function(reason) {
    data.frame(
      chi_squared = NA_real_, df = NA_real_, p_value = NA_real_,
      n = n,
      observed = I(list(tbl)), expected = I(list(NULL)),
      residuals = I(list(NULL)),
      cramers_v = NA_real_, phi = NA_real_, gamma = NA_real_,
      contingency_c = NA_real_,
      table_rows = r, table_cols = c,
      phi_p_value = NA_real_, cramers_v_p_value = NA_real_,
      gamma_p_value = NA_real_,
      reason = reason,
      stringsAsFactors = FALSE
    )
  }

  if (r < 2 || c < 2) {
    one_cat <- var_names[c(r < 2, c < 2)]
    reason <- if (n == 0) {
      "no valid cases"
    } else {
      paste0(paste(one_cat, collapse = " and "),
             if (length(one_cat) == 1) " has" else " have",
             " only one observed category")
    }
    cli_warn(c(
      "Chi-squared test not computed for {.var {var_names[1]}} × {.var {var_names[2]}}{where}.",
      "x" = "{reason}."
    ))
    return(empty_row(reason))
  }

  test_result <- stats::chisq.test(tbl, correct = correct)
  chi_squared <- as.numeric(test_result$statistic)
  p_value <- test_result$p.value

  # SPSS footnotes cells with an expected count below 5
  exp_tbl <- test_result$expected
  n_low <- sum(exp_tbl < 5)
  if (n_low > 0) {
    pct_low <- round(100 * n_low / length(exp_tbl), 1)
    cli_warn("{n_low} cell{?s} ({pct_low}%){where} ha{?s/ve} expected count < 5. Chi-squared approximation may be unreliable.")
  }

  gam <- .goodman_gamma_stats(tbl)

  data.frame(
    chi_squared = chi_squared,
    df = as.numeric(test_result$parameter),
    p_value = p_value,
    n = n,
    observed = I(list(test_result$observed)),
    expected = I(list(exp_tbl)),
    residuals = I(list(test_result$residuals)),
    # Phi and Cramer's V share the chi-square p-value (SPSS)
    cramers_v = sqrt(chi_squared / (n * min(r - 1, c - 1))),
    phi = sqrt(chi_squared / n),
    gamma = gam$gamma,
    contingency_c = sqrt(chi_squared / (chi_squared + n)),
    table_rows = r,
    table_cols = c,
    phi_p_value = p_value,
    cramers_v_p_value = p_value,
    gamma_p_value = gam$p_value,
    reason = NA_character_,
    stringsAsFactors = FALSE
  )
}

#' Goodman and Kruskal's gamma with its ASE0-based p-value (SPSS)
#'
#' SPSS CROSSTABS algorithm: for every cell (i, j)
#'   C_ij = cases above-left + cases below-right (concordant with the cell)
#'   D_ij = cases above-right + cases below-left (discordant with the cell)
#'   P = sum n_ij C_ij, Q = sum n_ij D_ij  (each pair counted twice)
#'   gamma = (P - Q) / (P + Q)
#'   ASE0 = 2 / (P + Q) * sqrt(sum n_ij (C_ij - D_ij)^2 - (P - Q)^2 / N)
#' and the approximate significance is 2 * pnorm(-|gamma / ASE0|). The four
#' quadrant sums come from 2-D cumulative sums (O(r * c); the former
#' element-wise loop over `[.table` took ~50 s on age x income).
#'
#' @param obs_table Contingency table (rows and columns in category order)
#' @return list(gamma, p_value)
#' @noRd
.goodman_gamma_stats <- function(obs_table) {
  n <- matrix(as.numeric(obs_table), nrow = nrow(obs_table))
  r <- nrow(n)
  k <- ncol(n)
  rr <- r:1
  kk <- k:1

  # inclusive 2-D cumulative sum from the top-left corner
  cum2 <- function(m) {
    if (nrow(m) > 1) for (i in 2:nrow(m)) m[i, ] <- m[i, ] + m[i - 1, ]
    if (ncol(m) > 1) for (j in 2:ncol(m)) m[, j] <- m[, j] + m[, j - 1]
    m
  }
  # strictly above-left of each cell: the cumulative sum shifted by (1, 1)
  above_left <- function(m) {
    s <- cum2(m)
    out <- matrix(0, nrow(m), ncol(m))
    if (nrow(m) > 1 && ncol(m) > 1) {
      out[-1, -1] <- s[-nrow(m), -ncol(m), drop = FALSE]
    }
    out
  }

  C_mat <- above_left(n) +
    above_left(n[rr, kk, drop = FALSE])[rr, kk, drop = FALSE]  # below-right
  D_mat <- above_left(n[, kk, drop = FALSE])[, kk, drop = FALSE] +  # above-right
    above_left(n[rr, , drop = FALSE])[rr, , drop = FALSE]           # below-left

  P <- sum(n * C_mat)
  Q <- sum(n * D_mat)
  if ((P + Q) == 0) return(list(gamma = 0, p_value = NA_real_))

  gamma <- (P - Q) / (P + Q)
  inner <- sum(n * (C_mat - D_mat)^2) - (P - Q)^2 / sum(n)
  p_value <- if (inner > 0) {
    ase0 <- (2 / (P + Q)) * sqrt(inner)
    2 * stats::pnorm(-abs(gamma / ase0))
  } else {
    NA_real_
  }
  list(gamma = gamma, p_value = p_value)
}

# Helper functions for print method

#' Print chi-square effect sizes table
#' @param df Data frame containing effect size columns
#' @param i Row index to extract from
#' @param digits Number of decimal places
#' @noRd
.print_chi_effect_sizes <- function(df, i, digits) {
  cramers_v <- df$cramers_v[i]
  if (is.na(cramers_v)) return(invisible(NULL))

  rows <- df$table_rows[i]
  cols <- df$table_cols[i]
  is_2x2 <- (rows == 2 && cols == 2)
  n <- df$n[i]

  measure <- c("Cramer's V", if (is_2x2) "Phi", "Gamma")
  value <- c(cramers_v, if (is_2x2) df$phi[i], df$gamma[i])
  p <- c(df$cramers_v_p_value[i], if (is_2x2) df$phi_p_value[i],
         df$gamma_p_value[i])
  interp <- c(.interpret_cramers_v(cramers_v),
              if (is_2x2) .interpret_phi(df$phi[i]),
              .interpret_gamma(df$gamma[i]))

  effect_table <- data.frame(
    Measure = measure,
    Value = value,
    p = p,
    stars = add_significance_stars(p),
    Interpretation = interp,
    stringsAsFactors = FALSE
  )

  cat("\nEffect Sizes:\n")
  print_stat_table(effect_table, digits = digits, indent = 0,
                   col_types = c(Value = "num"),
                   col_labels = c(p = "p value", stars = ""))
  cat(sprintf("Table size: %d\u00d7%d | N = %s\n", as.integer(rows),
              as.integer(cols), format(n, big.mark = "")))

  if (!is_2x2) {
    cat("Note: Phi coefficient only shown for 2x2 tables\n")
  }
}

.interpret_cramers_v <- function(v) {
  if (is.na(v)) return("-")
  if (v < 0.1) return("Neglig.")
  if (v < 0.3) return("Small")
  if (v < 0.5) return("Medium")
  return("Large")
}

.interpret_phi <- function(phi) {
  if (is.na(phi)) return("-")
  abs_phi <- abs(phi)
  if (abs_phi < 0.1) return("Neglig.")
  if (abs_phi < 0.3) return("Small")
  if (abs_phi < 0.5) return("Medium")
  return("Large")
}

.interpret_gamma <- function(g) {
  if (is.na(g)) return("-")
  abs_g <- abs(g)
  if (abs_g < 0.1) return("Weak")
  if (abs_g < 0.3) return("Moderate")
  return("Strong")
}

#' Print chi-squared test results (compact)
#'
#' @description
#' Compact print method for objects of class \code{"chi_square"}.
#' Shows a one-line summary per test with test statistic, p-value,
#' effect size, and sample size.
#'
#' For the full detailed output, use \code{summary()}.
#'
#' @param x An object of class \code{"chi_square"} returned by
#'   \code{\link{chi_square}}.
#' @param digits Number of decimal places to display. Default is \code{3}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- chi_square(survey_data, gender, education)
#' result              # compact one-line overview
#' summary(result)     # full detailed output
#'
#' @export
#' @method print chi_square
print.chi_square <- function(x, digits = 3, ...) {
  weighted_tag <- if (!is.null(x$weights)) " [Weighted]" else ""
  var_label <- paste(x$variables[1], "\u00d7", x$variables[2])

  if (isTRUE(x$is_grouped)) {
    groups <- unique(x$results[x$groups])
    for (i in seq_len(nrow(groups))) {
      group_values <- groups[i, , drop = FALSE]
      group_label <- .format_group_label(group_values)
      cat(sprintf("[%s]\n", group_label))

      # Find row matching this group
      group_row <- x$results
      for (g in names(group_values)) {
        group_row <- group_row[.group_match(group_row[[g]], group_values[[g]]), ]
      }
      for (j in seq_len(nrow(group_row))) {
        .print_chi_square_compact(group_row, j, var_label, weighted_tag, digits)
      }
    }
  } else {
    for (i in seq_len(nrow(x$results))) {
      .print_chi_square_compact(x$results, i, var_label, weighted_tag, digits)
    }
  }

  invisible(x)
}

#' Print a compact one-line summary for a single chi-squared test
#' @param results Data frame with chi-squared results
#' @param i Row index
#' @param var_label Variable label string (e.g. "gender x region")
#' @param weighted_tag Weighted tag string (e.g. " \[Weighted\]" or "")
#' @param digits Number of decimal places
#' @noRd
.print_chi_square_compact <- function(results, i, var_label, weighted_tag, digits) {
  chi_val <- results$chi_squared[i]
  df_val  <- results$df[i]
  p_val   <- results$p_value[i]
  v_val   <- results$cramers_v[i]
  n_val   <- results$n[i]

  cat(sprintf("Chi-Squared Test: %s%s\n", var_label, weighted_tag))

  if (is.na(chi_val)) {
    reason <- results$reason[i]
    cat(sprintf("  not computed (%s)\n",
                if (is.null(reason) || is.na(reason)) "see warning" else reason))
  } else if (!is.na(v_val)) {
    v_interp <- .interpret_cramers_v(v_val)
    cat(sprintf("  chi2(%d) = %.*f, %s %s, V = %.*f (%s), N = %d\n",
                as.integer(df_val), digits, chi_val,
                format_p_compact(p_val, digits),
                add_significance_stars(p_val),
                digits, v_val, tolower(v_interp), as.integer(n_val)))
  } else {
    cat(sprintf("  chi2(%d) = %.*f, %s %s, N = %d\n",
                as.integer(df_val), digits, chi_val,
                format_p_compact(p_val, digits),
                add_significance_stars(p_val),
                as.integer(n_val)))
  }
}

#' Summary method for chi-squared test results
#'
#' @description
#' Creates a summary object that produces detailed output when printed,
#' including observed and expected frequency tables, test results, and
#' effect size measures.
#'
#' @param object A \code{chi_square} result object.
#' @param observed Logical. Show observed frequency table? (Default: TRUE)
#' @param expected Logical. Show expected frequency table? (Default: TRUE)
#' @param results Logical. Show chi-squared test results table? (Default: TRUE)
#' @param effect_sizes Logical. Show effect size measures? (Default: TRUE)
#' @param digits Number of decimal places for formatting (Default: 3).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.chi_square} object.
#'
#' @examples
#' result <- chi_square(survey_data, gender, education)
#' summary(result)
#' summary(result, expected = FALSE)
#'
#' @seealso \code{\link{chi_square}} for the main analysis function.
#' @export
#' @method summary chi_square
summary.chi_square <- function(object, observed = TRUE, expected = TRUE,
                               results = TRUE, effect_sizes = TRUE,
                               digits = 3, ...) {
  build_summary_object(
    object = object,
    show = list(observed = observed, expected = expected,
                results = results, effect_sizes = effect_sizes),
    digits = digits,
    class_name = "summary.chi_square"
  )
}

#' Print summary of chi-squared test results (detailed output)
#'
#' @description
#' Displays the detailed SPSS-style output for a chi-squared test, with
#' sections controlled by the boolean parameters passed to
#' \code{\link{summary.chi_square}}.  Sections include cross-tabulation,
#' test results, and effect sizes (Cramer's V, Phi).
#'
#' @param x A \code{summary.chi_square} object created by
#'   \code{\link{summary.chi_square}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- chi_square(survey_data, gender, education)
#' summary(result)                          # all sections
#' summary(result, cross_tabulation = FALSE) # hide crosstab
#'
#' @seealso \code{\link{chi_square}} for the main analysis,
#'   \code{\link{summary.chi_square}} for summary options.
#' @export
#' @method print summary.chi_square
print.summary.chi_square <- function(x, ...) {
  digits <- x$digits

  # Header
  test_type <- get_standard_title("Chi-Squared Test of Independence", x$weights, "")
  print_header(test_type)

  # Resolve show toggles (default TRUE when called without summary object)
  show <- list(
    observed     = if (!is.null(x$show)) isTRUE(x$show$observed) else TRUE,
    expected     = if (!is.null(x$show)) isTRUE(x$show$expected) else TRUE,
    results      = if (!is.null(x$show)) isTRUE(x$show$results) else TRUE,
    effect_sizes = if (!is.null(x$show)) isTRUE(x$show$effect_sizes) else TRUE
  )

  cat("\n")
  test_info <- list(
    "Variables" = paste(x$variables[1], "\u00d7", x$variables[2]),
    "Grouped by" = if (isTRUE(x$is_grouped)) paste(x$groups, collapse = ", "),
    "Weights variable" = x$weights,
    "Continuity correction" = if (isTRUE(x$correct)) "Yates' correction applied (2x2 tables)" else NULL
  )
  print_info_section(test_info)

  if (!isTRUE(x$is_grouped)) {
    .print_chi_block(x$results, 1, digits, show)
  } else {
    for (i in seq_len(nrow(x$results))) {
      print_group_header(x$results[i, x$groups, drop = FALSE])
      .print_chi_block(x$results, i, digits, show)
    }
  }

  if (show$results || show$effect_sizes) {
    print_significance_legend()
  }

  invisible(x)
}

#' Print the tables of one chi-square test (one slice)
#' @param results Results data frame
#' @param i Row index
#' @param digits Decimal places
#' @param show Named list of section toggles
#' @noRd
.print_chi_block <- function(results, i, digits, show) {
  obs <- results$observed[[i]]

  if (show$observed && !is.null(obs)) {
    cat("\nObserved Frequencies:\n")
    .print_chi_matrix(obs, digits = 0)
  }

  if (is.na(results$chi_squared[i])) {
    reason <- results$reason[i]
    cat(sprintf("\nChi-squared test not computed (%s).\n",
                if (is.null(reason) || is.na(reason)) "see warning" else reason))
    return(invisible(NULL))
  }

  if (show$expected) {
    cat("\nExpected Frequencies:\n")
    .print_chi_matrix(results$expected[[i]], digits = digits)
  }

  if (show$results) {
    cat("\nChi-Squared Test Results:\n")
    tab <- data.frame(
      Statistic = "Pearson Chi-Square",
      Value = results$chi_squared[i],
      df = results$df[i],
      p = results$p_value[i],
      stars = add_significance_stars(results$p_value[i]),
      stringsAsFactors = FALSE
    )
    print_stat_table(tab, digits = digits, indent = 0,
                     col_types = c(Value = "num", df = "int"),
                     col_labels = c(Statistic = "", p = "p value", stars = ""))
  }

  if (show$effect_sizes) {
    .print_chi_effect_sizes(results, i, digits)
  }
  invisible(NULL)
}

#' Print a contingency matrix with fixed decimals and full labels
#' @param mat Table/matrix with named dimnames
#' @param digits Decimals (0 for counts)
#' @noRd
.print_chi_matrix <- function(mat, digits) {
  vals <- unclass(mat)
  out <- matrix(formatC(as.numeric(vals), format = "f", digits = digits),
                nrow = nrow(vals), dimnames = dimnames(vals))
  print(out, quote = FALSE, right = TRUE)
}

# Extract a single effect-size column from a chi_square result as a plain
# numeric vector, named by the group values for grouped input.
.extract_chi_effect_size <- function(res, column) {
  df <- res$results
  out <- df[[column]]
  if (isTRUE(res$is_grouped) && length(res$groups) > 0 && nrow(df) > 1) {
    label_cols <- intersect(res$groups, names(df))
    if (length(label_cols) > 0) {
      names(out) <- apply(df[label_cols], 1, paste, collapse = ":")
    }
  }
  out
}

#' Effect Sizes for Contingency Tables
#'
#' @description
#' Convenience helpers that run \code{\link{chi_square}} and return just the
#' requested effect size as a numeric value (named by group for grouped
#' data): \code{phi()} for 2x2 tables, \code{cramers_v()} for larger tables,
#' and \code{goodman_gamma()} for ordinal variables.
#'
#' For the full test output (chi-square statistic, p-value, all effect
#' sizes), call \code{\link{chi_square}} directly.
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... Exactly two categorical variables, as in \code{chi_square()}
#' @param weights Optional survey weights
#'
#' @return A numeric vector with the effect size (one element per group for
#'   grouped data).
#'
#' @examples
#' data(survey_data)
#' phi(survey_data, gender, region)
#' cramers_v(survey_data, education, region)
#' goodman_gamma(survey_data, education, life_satisfaction)
#'
#' @seealso \code{\link{chi_square}}
#' @family effect_sizes
#' @export
phi <- function(data, ..., weights = NULL) {
  res <- chi_square(data, ..., weights = {{ weights }})
  .extract_chi_effect_size(res, "phi")
}

#' @rdname phi
#' @export
cramers_v <- function(data, ..., weights = NULL) {
  res <- chi_square(data, ..., weights = {{ weights }})
  .extract_chi_effect_size(res, "cramers_v")
}

#' @rdname phi
#' @export
goodman_gamma <- function(data, ..., weights = NULL) {
  res <- chi_square(data, ..., weights = {{ weights }})
  .extract_chi_effect_size(res, "gamma")
}
