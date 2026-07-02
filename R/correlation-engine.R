# =============================================================================
# Shared correlation engine
# =============================================================================
# pearson_cor(), spearman_rho(), and kendall_tau() share ~85% of their
# structure: input validation, listwise/pairwise deletion, the per-pair
# computation loop, matrix assembly, grouped execution, result-object
# construction, and the compact print -> summary -> verbose print stack.
# This file implements that machinery once. The three front-end files keep
# only their roxygen docs, the per-pair statistic (.pearson_pair(),
# .spearman_pair(), .kendall_pair() -- the SPSS-validated math), and thin
# wrappers/methods that delegate here.
#
# The `spec` list describes the per-method differences:
#
#   Computation / result structure
#     class_name    S3 class of the result object
#     result_names  top-level element names (and order) of the result
#     matrices      named list: matrix name -> list(init =, diag =); diag
#                   is the fixed diagonal value, or "n" for the sample-size
#                   diagonal (see diag_n)
#     extract       function(res) -> named list matrix name -> value, used
#                   to fill the off-diagonal cells from a pair result
#     diag_n        "weight_sum": SPSS CORRELATIONS convention, rounded sum
#                   of weights over valid cases (pearson/kendall);
#                   "count": SPSS NONPAR CORR convention, count of valid
#                   cases with weight > 0 (spearman)
#     df_source     "matrices": long table read back from the assembled
#                   matrices (pearson/kendall); "pairs": long table built
#                   directly from the pair results (spearman, whose t_stat
#                   is not stored in any matrix)
#     df_cols       df_source == "matrices": named character vector,
#                   column name -> matrix name; df_source == "pairs":
#                   named list, column name -> function(res)
#     sig_inline    TRUE: sig column added per row while building
#                   (spearman); FALSE: added by finalize_df afterwards
#     name_rows     TRUE: name the per-group row list "var1_var2" and reset
#                   row names after rbind (spearman's historical
#                   construction, preserved byte-for-byte)
#     group_cols    "front": cbind the group keys before each row
#                   (pearson/kendall); "back": append the group columns
#                   after the pair columns (spearman)
#     finalize_df   optional function(df) applied to the combined long
#                   table (significance stars, r_squared, ...)
#
#   Print / summary display
#     compact_title, stat_col, stat_label          compact print()
#     verbose_title, info, params                  summary header block
#     pair_stat_prefix, pair_extras                2-variable verbose block
#     matrix_key, matrix_title, p_title            3+ variable matrices
#     pairwise_df                                  3+ variable pair table
# =============================================================================

#' Shared driver for the three correlation functions
#'
#' @param data A data frame (possibly grouped)
#' @param ... Variables to correlate (tidyselect, forwarded verbatim)
#' @param weights Weights quosure (wrappers pass \code{rlang::enquo(weights)})
#' @param alternative Alternative hypothesis (raw formal, matched here)
#' @param use Missing-data handling (raw formal, matched here)
#' @param na.rm Deprecated alias for \code{use}
#' @param conf.level Confidence level (pearson only; NULL to skip check)
#' @param pair_fn function(x, y, w, alternative) computing one pair
#' @param spec Per-method specification list (see header comment)
#' @param call Environment reported in error conditions
#' @return Classed result object (see \code{spec$class_name})
#' @noRd
.correlate <- function(data, ..., weights, alternative, use, na.rm = NULL,
                       conf.level = NULL, pair_fn, spec,
                       call = rlang::caller_env()) {

  # Input validation
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.", call = call)
  }

  # Handle deprecated na.rm parameter
  if (!is.null(na.rm)) {
    cli_warn("{.arg na.rm} is deprecated in correlation functions. Use {.arg use} instead.")
    use <- na.rm
  }
  use <- match.arg(use, c("pairwise", "listwise"))
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))

  if (!is.null(conf.level) && (conf.level <= 0 || conf.level >= 1)) {
    cli_abort("{.arg conf.level} must be between 0 and 1.", call = call)
  }

  # Check if data is grouped
  is_grouped <- inherits(data, "grouped_df")
  group_vars <- if (is_grouped) dplyr::group_vars(data) else NULL

  # Select variables using centralized helper
  vars <- .process_variables(data, ..., call = call)
  var_names <- names(vars)

  if (length(var_names) < 2) {
    cli_abort("At least two variables must be specified for correlation analysis.",
              call = call)
  }

  # Validate that all selected variables are numeric
  for (var_name in var_names) {
    if (!is.numeric(data[[var_name]])) {
      cli_abort("Variable {.var {var_name}} is not numeric.", call = call)
    }
  }

  # Process weights using centralized helper
  weights_info <- .process_weights(data, weights, call = call)
  w_name <- weights_info$name

  n_vars <- length(var_names)
  m_names <- names(spec$matrices)

  # Compute matrices + long-format rows for one group (or the whole data)
  run_group <- function(group_data) {
    weights_vec <- if (!is.null(w_name)) group_data[[w_name]] else NULL

    # Handle listwise deletion if requested
    if (use == "listwise") {
      complete_cases <- complete.cases(group_data[var_names])
      if (!is.null(weights_vec)) {
        complete_cases <- complete_cases & !is.na(weights_vec)
      }
      group_data <- group_data[complete_cases, ]
      if (!is.null(weights_vec)) {
        weights_vec <- weights_vec[complete_cases]
      }
    }

    # Initialize storage
    mats <- lapply(spec$matrices, function(m) {
      matrix(m$init, n_vars, n_vars, dimnames = list(var_names, var_names))
    })

    # Diagonal: perfect correlation with self; N follows the per-method
    # SPSS convention (see spec$diag_n)
    for (i in seq_len(n_vars)) {
      for (m in m_names) {
        d <- spec$matrices[[m]]$diag
        if (identical(d, "n")) {
          if (!is.null(weights_vec)) {
            if (identical(spec$diag_n, "weight_sum")) {
              valid_idx <- !is.na(group_data[[var_names[i]]]) & !is.na(weights_vec)
              mats[[m]][i, i] <- round(sum(weights_vec[valid_idx]))
            } else {
              valid <- !is.na(group_data[[var_names[i]]]) &
                       !is.na(weights_vec) & weights_vec > 0
              mats[[m]][i, i] <- sum(valid)
            }
          } else {
            mats[[m]][i, i] <- sum(!is.na(group_data[[var_names[i]]]))
          }
        } else {
          mats[[m]][i, i] <- d
        }
      }
    }

    # Calculate correlations for each pair (i < j) and store symmetrically
    pair_results <- list()
    for (i in 1:(n_vars - 1)) {
      for (j in (i + 1):n_vars) {
        res <- pair_fn(group_data[[var_names[i]]],
                       group_data[[var_names[j]]],
                       weights_vec,
                       alternative)
        vals <- spec$extract(res)
        for (m in names(vals)) {
          mats[[m]][i, j] <- mats[[m]][j, i] <- vals[[m]]
        }
        pair_results[[length(pair_results) + 1]] <- list(i = i, j = j, res = res)
      }
    }

    # Convert to long format (one 1-row data frame per pair)
    rows <- lapply(pair_results, function(pr) {
      if (identical(spec$df_source, "pairs")) {
        vals <- lapply(spec$df_cols, function(f) f(pr$res))
      } else {
        vals <- lapply(spec$df_cols, function(m) mats[[m]][pr$i, pr$j])
      }
      if (isTRUE(spec$sig_inline)) {
        vals$sig <- add_significance_stars(pr$res$p_value)
      }
      do.call(data.frame,
              c(list(var1 = var_names[pr$i], var2 = var_names[pr$j]),
                vals,
                list(stringsAsFactors = FALSE)))
    })
    if (isTRUE(spec$name_rows)) {
      names(rows) <- vapply(pair_results, function(pr) {
        paste(var_names[pr$i], var_names[pr$j], sep = "_")
      }, character(1))
    }

    list(matrices = mats, rows = rows)
  }

  # Main execution logic
  if (is_grouped) {
    data_list <- dplyr::group_split(data)
    group_keys <- dplyr::group_keys(data)

    per_group <- lapply(data_list, run_group)
    matrices_list <- lapply(per_group, function(g) g$matrices)

    group_dfs <- lapply(seq_along(per_group), function(gi) {
      rows <- per_group[[gi]]$rows
      group_info <- group_keys[gi, , drop = FALSE]

      if (identical(spec$group_cols, "front")) {
        do.call(rbind, lapply(rows, function(r) cbind(group_info, r)))
      } else {
        df <- do.call(rbind, rows)
        rownames(df) <- NULL
        for (col in names(group_info)) {
          df[[col]] <- rep(group_info[[col]], nrow(df))
        }
        df
      }
    })

    correlations_df <- do.call(rbind, group_dfs)
    n_obs <- NULL  # per-group sample sizes live in matrices_list
  } else {
    single <- run_group(data)
    matrices_list <- list(single$matrices)
    correlations_df <- do.call(rbind, single$rows)
    if (isTRUE(spec$name_rows)) {
      rownames(correlations_df) <- NULL
    }
    n_obs <- single$matrices$n_obs
    group_keys <- NULL
  }

  if (!is.null(spec$finalize_df)) {
    correlations_df <- spec$finalize_df(correlations_df)
  }

  # Create result object (spec$result_names fixes names and order per class)
  full <- list(
    correlations = correlations_df,
    n_obs = n_obs,
    matrices = matrices_list,
    variables = var_names,
    weights = w_name,
    conf.level = conf.level,
    use = use,
    alternative = alternative,
    is_grouped = is_grouped,
    groups = group_vars,
    group_keys = if (is_grouped) group_keys else NULL
  )
  result <- full[spec$result_names]
  class(result) <- spec$class_name
  result
}

# =============================================================================
# Shared print/summary stack
# =============================================================================

#' Compact print driver shared by the three correlation classes
#' @noRd
.print_cor_result <- function(x, spec, digits = 3) {
  weighted_tag <- if (!is.null(x$weights)) " [Weighted]" else ""
  corrs <- x$correlations

  if (isTRUE(x$is_grouped)) {
    groups <- unique(corrs[x$groups])
    for (gi in seq_len(nrow(groups))) {
      group_values <- groups[gi, , drop = FALSE]
      group_label <- paste(names(group_values), "=", group_values, collapse = ", ")
      cat(sprintf("[%s]\n", group_label))

      group_corrs <- corrs
      for (g in names(group_values)) {
        group_corrs <- group_corrs[group_corrs[[g]] == group_values[[g]], ]
      }
      .print_cor_compact(x, group_corrs, weighted_tag, digits, spec)
    }
  } else {
    .print_cor_compact(x, corrs, weighted_tag, digits, spec)
  }

  invisible(x)
}

#' Print compact correlation output for one group or ungrouped
#' @noRd
.print_cor_compact <- function(x, corrs, weighted_tag, digits, spec) {
  n_vars <- length(x$variables)
  stat <- corrs[[spec$stat_col]]

  if (n_vars == 2) {
    pair_label <- paste(x$variables[1], "x", x$variables[2])
    cat(sprintf("%s: %s%s\n", spec$compact_title, pair_label, weighted_tag))
    p_val <- as.numeric(corrs$p_value[1])
    cat(sprintf("  %s = %.*f, %s %s, N = %d\n",
                spec$stat_label, digits, stat[1],
                format_p_compact(p_val, digits),
                add_significance_stars(p_val),
                corrs$n[1]))
  } else {
    n_sig <- sum(as.numeric(corrs$p_value) < 0.05, na.rm = TRUE)
    n_pairs <- nrow(corrs)
    cat(sprintf("%s: %d variables%s\n", spec$compact_title, n_vars, weighted_tag))

    for (i in seq_len(n_pairs)) {
      pair_label <- paste(corrs$var1[i], "x", corrs$var2[i])
      p_val <- as.numeric(corrs$p_value[i])
      line <- sprintf("  %-30s %s = %.*f, %s %s",
                      paste0(pair_label, ":"),
                      spec$stat_label, digits, stat[i],
                      format_p_compact(p_val, digits),
                      add_significance_stars(p_val))
      cat(line, "\n")
    }
    cat(sprintf("  %d/%d pairs significant (p < .05), N = %d\n",
                n_sig, n_pairs, corrs$n[1]))
  }
}

#' Verbose print driver shared by the three summary.* correlation classes
#' @noRd
.print_cor_summary <- function(x, spec) {
  digits <- x$digits
  show_cor <- if (!is.null(x$show)) isTRUE(x$show$correlation_matrix) else TRUE
  show_p   <- if (!is.null(x$show)) isTRUE(x$show$pvalue_matrix) else TRUE
  show_n   <- if (!is.null(x$show)) isTRUE(x$show$n_matrix) else TRUE

  # Header
  title <- get_standard_title(spec$verbose_title, x$weights, "")
  print_header(title)

  # Info section
  cat("\n")
  print_info_section(spec$info(x))
  if (!is.null(spec$params)) {
    print_test_parameters(spec$params(x))
  }
  cat("\n")

  if (isTRUE(x$is_grouped)) {
    group_combinations <- unique(x$correlations[x$groups])

    for (gi in seq_len(nrow(group_combinations))) {
      print_group_header(group_combinations[gi, , drop = FALSE])

      group_corrs <- x$correlations
      for (g in names(group_combinations)) {
        group_corrs <- group_corrs[group_corrs[[g]] == group_combinations[gi, g], ]
      }

      .print_cor_verbose(x, group_corrs, gi, show_cor, show_p, show_n, digits, spec)
    }
  } else {
    .print_cor_verbose(x, x$correlations, 1, show_cor, show_p, show_n, digits, spec)
  }

  # Footer
  print_significance_legend()
  invisible(x)
}

#' Print verbose correlation output for one group
#' @noRd
.print_cor_verbose <- function(x, corrs, matrix_idx, show_cor, show_p, show_n,
                               digits, spec) {
  n_vars <- length(x$variables)

  if (n_vars == 2) {
    # For 2 variables, show single-pair detail with optional sections
    if (show_cor) {
      cat(sprintf("\n  %s = %.*f\n", spec$pair_stat_prefix, digits,
                  corrs[[spec$stat_col]][1]))
    }
    if (show_p) {
      cat(sprintf("  p-value: %s %s\n",
                  format_p_compact(as.numeric(corrs$p_value[1]), digits),
                  add_significance_stars(as.numeric(corrs$p_value[1]))))
    }
    if (show_n) {
      cat(sprintf("  N = %d\n", corrs$n[1]))
    }
    # Always-shown per-method extras (CI/r-squared, t-statistic, z-score)
    spec$pair_extras(corrs, digits)
  } else {
    # For 3+ variables, show matrices and pairwise table

    if (show_cor) {
      .print_cor_matrix(x$matrices[[matrix_idx]][[spec$matrix_key]],
                        digits = digits,
                        title = spec$matrix_title,
                        type = "correlation")
    }

    if (show_p) {
      .print_cor_matrix(x$matrices[[matrix_idx]]$p_values, digits = 4,
                        title = spec$p_title(x),
                        type = "pvalue")
    }

    if (show_n) {
      .print_cor_matrix(x$matrices[[matrix_idx]]$n_obs, digits = 0,
                        title = "Sample Size Matrix:",
                        type = "n")
    }

    # Pairwise results always shown
    cat("\nPairwise Results:\n")
    border_width <- paste(rep("-", 16), collapse = "")
    cat(border_width, "\n")

    print(spec$pairwise_df(corrs, digits), row.names = FALSE)
    cat(border_width, "\n")
  }
}
