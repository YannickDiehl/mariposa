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
#     weights_filter_only
#                   TRUE when weights only select cases and never enter
#                   the statistic (spearman, SPSS NONPAR CORR): the output
#                   then carries no "Weighted" label
#
#   Print / summary display
#     compact_title, stat_col, stat_label          compact print()
#     verbose_title, info, params                  summary header block
#     pair_stat_prefix, pair_extras                2-variable verbose block
#                   (pair_extras returns formatted lines; the engine's
#                   print layer emits them — spec callbacks never cat())
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
  data <- weights_info$data
  w_name <- weights_info$name

  n_vars <- length(var_names)
  m_names <- names(spec$matrices)

  # Compute matrices + long-format rows for one group (or the whole data)
  run_group <- function(group_data, group_label = NULL) {
    weights_vec <- if (!is.null(w_name)) group_data[[w_name]] else NULL

    # Pair computations run on bare numbers: labelled vectors would send
    # every element access through vctrs dispatch (see .plain_numeric)
    for (v in var_names) group_data[[v]] <- .plain_numeric(group_data[[v]])

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

    # Degenerate pairs (NA coefficient): one warning naming the cause,
    # instead of silent NA rows (and one base-R warning per pair)
    .warn_cor_not_computed(pair_results, group_data, var_names, weights_vec,
                           spec$stat_col, group_label)

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

    per_group <- lapply(seq_along(data_list), function(gi) {
      run_group(data_list[[gi]],
                group_label = .format_group_label(group_keys[gi, , drop = FALSE]))
    })
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
  weighted_tag <- if (!is.null(x$weights) && !isTRUE(spec$weights_filter_only)) {
    " [Weighted]"
  } else ""
  corrs <- x$correlations

  if (isTRUE(x$is_grouped)) {
    groups <- unique(corrs[x$groups])
    for (gi in seq_len(nrow(groups))) {
      group_values <- groups[gi, , drop = FALSE]
      group_label <- .format_group_label(group_values)
      cat(sprintf("[%s]\n", group_label))

      group_corrs <- corrs
      for (g in names(group_values)) {
        group_corrs <- group_corrs[.group_match(group_corrs[[g]], group_values[[g]]), ]
      }
      .print_cor_compact(x, group_corrs, weighted_tag, digits, spec)
    }
  } else {
    .print_cor_compact(x, corrs, weighted_tag, digits, spec)
  }

  invisible(x)
}

#' Print compact correlation output for one group or ungrouped
#'
#' Pair labels are padded to the longest label (display width); a
#' not-computable coefficient reads "not computed (<reason>)"; the N is
#' the range over the pairs (pairwise deletion gives each pair its own N);
#' with more than 15 pairs only the strongest significant pairs are listed
#' so the compact print stays compact.
#' @noRd
.print_cor_compact <- function(x, corrs, weighted_tag, digits, spec) {
  n_vars <- length(x$variables)
  stat <- as.numeric(corrs[[spec$stat_col]])
  p <- as.numeric(corrs$p_value)
  n <- as.numeric(corrs$n)
  alt_tag <- .cor_alternative_tag(x$alternative)
  texts <- .cor_stat_text(spec$stat_label, stat, p, n, digits, spec$min_n)

  if (n_vars == 2) {
    pair_label <- paste(x$variables[1], "x", x$variables[2])
    cat(sprintf("%s: %s%s%s\n", spec$compact_title, pair_label, alt_tag,
                weighted_tag))
    cat(sprintf("  %s, N = %s\n", texts[1], .fmt_n(n[1])))
  } else {
    n_sig <- sum(p < 0.05, na.rm = TRUE)
    n_pairs <- nrow(corrs)
    cat(sprintf("%s: %d variables%s%s\n", spec$compact_title, n_vars,
                alt_tag, weighted_tag))

    labels <- paste0(corrs$var1, " x ", corrs$var2, ":")
    show <- seq_len(n_pairs)
    limited <- n_pairs > 15L
    if (limited) {
      sig <- which(!is.na(p) & p < 0.05)
      show <- utils::head(sig[order(-abs(stat[sig]))], 10L)
    }
    if (length(show) > 0) {
      w <- max(nchar(labels[show], type = "width"))
      for (i in show) {
        cat("  ", pad_utf8(labels[i], w), " ", texts[i], "\n", sep = "")
      }
    }
    if (limited) {
      cat(sprintf("  (%s: the %d strongest significant pairs; summary() shows all %d)\n",
                  if (length(show) > 0) "Shown" else "None shown",
                  length(show), n_pairs))
    }
    cat(sprintf("  %d/%d pairs significant (p < .05), %s\n",
                n_sig, n_pairs, .cor_n_range(n, stat)))
  }
}

#' "r = 0.123, p = 0.045 *" per pair, or "not computed (<reason>)"
#' @noRd
.cor_stat_text <- function(label, stat, p, n, digits, min_n = 3) {
  out <- sprintf("%s = %s, %s", label,
                 formatC(stat, format = "f", digits = digits),
                 vapply(p, format_p_stars, character(1), digits = digits))
  miss <- is.na(stat)
  out[miss] <- ifelse(!is.na(n[miss]) & n[miss] < (min_n %||% 3),
                      "not computed (too few valid cases)",
                      "not computed (no variance)")
  out
}

#' "N = 2186" or "N = 2076-2500" over the computed pairs
#' @noRd
.cor_n_range <- function(n, stat = NULL) {
  use <- if (!is.null(stat) && any(!is.na(stat))) n[!is.na(stat)] else n
  use <- use[!is.na(use)]
  if (length(use) == 0) return("N = NA")
  lo <- min(use)
  hi <- max(use)
  if (round(lo) == round(hi)) {
    paste("N =", .fmt_n(lo))
  } else {
    paste0("N = ", .fmt_n(lo), "-", .fmt_n(hi))
  }
}

#' Title tag for a one-sided test ("" for two-sided)
#' @noRd
.cor_alternative_tag <- function(alternative) {
  if (is.null(alternative) || identical(alternative, "two.sided")) return("")
  sprintf(" [one-sided: %s]", alternative)
}

#' Warn once about pairs whose coefficient could not be computed
#'
#' A constant variable (or one with too few valid cases) used to leave NA
#' rows silently - plus one translated base-R "standard deviation is zero"
#' warning per pair. Names the variables and, for grouped data, the group.
#' @noRd
.warn_cor_not_computed <- function(pair_results, group_data, var_names,
                                   weights_vec, stat_col, group_label = NULL) {
  bad <- Filter(function(pr) is.na(pr$res[[stat_col]]), pair_results)
  if (length(bad) == 0) return(invisible(NULL))
  involved <- unique(unlist(lapply(bad, function(pr) var_names[c(pr$i, pr$j)])))
  valid_values <- function(v) {
    x <- group_data[[v]]
    keep <- !is.na(x)
    if (!is.null(weights_vec)) keep <- keep & !is.na(weights_vec) & weights_vec > 0
    x[keep]
  }
  n_valid <- vapply(involved, function(v) length(valid_values(v)), numeric(1))
  constant <- involved[n_valid >= 3 & vapply(involved, function(v) {
    length(unique(valid_values(v))) == 1L
  }, logical(1))]
  sparse <- involved[n_valid < 3]
  where <- if (!is.null(group_label)) paste0(" (", group_label, ")") else ""
  msg <- c("Some correlations are not computed{where}.")
  if (length(constant) > 0) {
    msg <- c(msg, x = "No variance: {.var {constant}}.")
  }
  if (length(sparse) > 0) {
    msg <- c(msg, x = "Too few valid cases: {.var {sparse}}.")
  }
  if (length(constant) == 0 && length(sparse) == 0) {
    pairs <- vapply(bad, function(pr) {
      paste(var_names[pr$i], "x", var_names[pr$j])
    }, character(1))
    msg <- c(msg, x = "Too few valid paired cases: {pairs}.")
  }
  cli_warn(msg)
  invisible(NULL)
}

#' Verbose print driver shared by the three summary.* correlation classes
#' @noRd
.print_cor_summary <- function(x, spec) {
  digits <- x$digits
  show_cor <- if (!is.null(x$show)) isTRUE(x$show$correlation_matrix) else TRUE
  show_p   <- if (!is.null(x$show)) isTRUE(x$show$pvalue_matrix) else TRUE
  show_n   <- if (!is.null(x$show)) isTRUE(x$show$n_matrix) else TRUE

  # Header
  title <- get_standard_title(
    spec$verbose_title,
    if (isTRUE(spec$weights_filter_only)) NULL else x$weights, ""
  )
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
        group_corrs <- group_corrs[.group_match(group_corrs[[g]], group_combinations[[g]][gi]), ]
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
  tails <- if (identical(x$alternative %||% "two.sided", "two.sided")) {
    "2-tailed"
  } else {
    "1-tailed"
  }

  if (n_vars == 2) {
    # For 2 variables, show single-pair detail with optional sections
    stat <- as.numeric(corrs[[spec$stat_col]][1])
    p <- as.numeric(corrs$p_value[1])
    n <- as.numeric(corrs$n[1])
    if (show_cor) {
      if (is.na(stat)) {
        cat(sprintf("\n  %s: %s\n", spec$pair_stat_prefix,
                    .cor_stat_text("", stat, p, n, digits, spec$min_n)))
      } else {
        cat(sprintf("\n  %s = %.*f\n", spec$pair_stat_prefix, digits, stat))
      }
    }
    if (show_p && !is.na(stat)) {
      cat(sprintf("  p-value (%s): %s\n", tails, format_p_stars(p, digits)))
    }
    if (show_n) {
      cat(sprintf("  N = %s\n", .fmt_n(n)))
    }
    # Always-shown per-method extras (CI/r-squared, t-statistic, z-score);
    # the spec callback returns formatted lines so that all console output
    # stays inside this print layer
    if (!is.na(stat)) cat(spec$pair_extras(corrs, digits, x), sep = "")
  } else {
    # For 3+ variables, show matrices and pairwise table
    mats <- x$matrices[[matrix_idx]]

    if (show_cor) {
      # Significance flags on the coefficients, as SPSS FLAG does
      .print_cor_matrix_fit(mats[[spec$matrix_key]], spec$matrix_title,
                            type = "correlation", digits = digits,
                            p_mat = mats$p_values)
    }

    if (show_p) {
      .print_cor_matrix_fit(mats$p_values, spec$p_title(x), type = "pvalue",
                            digits = digits)
    }

    if (show_n) {
      .print_cor_matrix_fit(mats$n_obs, "Sample Size Matrix:", type = "n")
    }

    # Pairwise results always shown (pre-formatted text columns)
    cat("\nPairwise Results:\n")
    tab <- spec$pairwise_df(corrs, digits, x)
    print_stat_table(tab, col_types = stats::setNames(rep("char", ncol(tab)),
                                                      names(tab)),
                     col_labels = attr(tab, "col_labels"))
    if (any(is.na(corrs[[spec$stat_col]]))) {
      cat("  n.c. = not computed (no variance or too few valid cases)\n")
    }
  }
}

#' Print a correlation / p-value / N matrix that fits the console
#'
#' Replaces .print_cor_matrix() for the correlation classes: honours
#' `digits` for any number of variables (it dropped to 2 decimals above 6
#' variables), leaves the p-value diagonal blank and shows p in SPSS table
#' style ("<.001" instead of 0.0000), flags significant coefficients, and
#' splits the columns into blocks that fit getOption("width") instead of
#' temporarily raising the width option (lines wider than the console).
#'
#' @param mat Square matrix with variable dimnames
#' @param title Section title
#' @param type "correlation", "pvalue" or "n"
#' @param digits Decimal places (correlation / p)
#' @param p_mat Optional p-value matrix; adds significance stars to
#'   correlation cells
#' @noRd
.print_cor_matrix_fit <- function(mat, title, type = c("correlation", "pvalue", "n"),
                                  digits = 3, p_mat = NULL) {
  type <- match.arg(type)
  vars <- rownames(mat)
  k <- ncol(mat)
  num <- matrix("", k, k)
  star <- matrix("", k, k)
  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      v <- mat[i, j]
      num[i, j] <- switch(type,
        correlation = if (i == j) "1" else if (is.na(v)) "" else
          formatC(v, format = "f", digits = digits),
        pvalue = if (i == j) "" else fmt_p(v, digits, style = "table"),
        n = if (is.na(v)) "" else .fmt_n(v)
      )
      if (type == "correlation" && !is.null(p_mat) && i != j) {
        star[i, j] <- add_significance_stars(p_mat[i, j])
      }
    }
  }
  sw <- max(nchar(star), 0L)
  col_w <- vapply(seq_len(k), function(j) {
    max(nchar(vars[j], type = "width"), nchar(num[, j], type = "width"))
  }, numeric(1))
  row_w <- max(nchar(vars, type = "width"))

  # Greedy column blocks that fit the console width
  avail <- max(getOption("width", 80L) - row_w, 1L)
  blocks <- list()
  current <- integer(0)
  used <- 0
  for (j in seq_len(k)) {
    need <- 2 + col_w[j] + sw
    if (length(current) > 0 && used + need > avail) {
      blocks[[length(blocks) + 1]] <- current
      current <- integer(0)
      used <- 0
    }
    current <- c(current, j)
    used <- used + need
  }
  blocks[[length(blocks) + 1]] <- current

  border <- strrep("-", nchar(title, type = "width"))
  cat("\n", title, "\n", border, "\n", sep = "")
  for (b in seq_along(blocks)) {
    cols <- blocks[[b]]
    if (b > 1) cat("\n")
    header <- paste0(vapply(cols, function(j) {
      paste0("  ", pad_utf8(vars[j], col_w[j], align = "right"), strrep(" ", sw))
    }, character(1)), collapse = "")
    cat(strrep(" ", row_w), header, "\n", sep = "")
    for (i in seq_len(k)) {
      cells <- paste0(vapply(cols, function(j) {
        paste0("  ", pad_utf8(num[i, j], col_w[j], align = "right"),
               if (sw > 0) pad_utf8(star[i, j], sw) else "")
      }, character(1)), collapse = "")
      cat(pad_utf8(vars[i], row_w), cells, "\n", sep = "")
    }
  }
  cat(border, "\n", sep = "")
  invisible(NULL)
}
