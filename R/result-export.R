# =============================================================================
# Result export: as.data.frame(), as_tibble() and broom::tidy() for results
# =============================================================================
# Every analysis function returns a classed list whose tables sit in
# different fields ($results, $correlations, $comparisons, $anova_table,
# $coef_table, per-group lists, ...), several of them wide (describe(),
# w_quantile()) or with list-columns (t_test()$results$group_stats,
# chi_square()$results$observed). as.data.frame() used to fail for every
# class ("cannot coerce class ... to a data.frame") and write.csv() failed
# on the list-columns.
#
# One converter per class returns a named list of flat tables:
#   - the first table is the main result - one row per test / variable /
#     term / pair, grouping variables as leading columns - and is what
#     as.data.frame() returns (tibble::as_tibble() reaches it through
#     tibble's default method);
#   - further tables hold the secondary output SPSS prints next to the test
#     (group descriptives, mean ranks, item statistics, model fit) and are
#     written by write_xlsx() below the main table.
# .xp_finish() makes every table plain: no list-columns, no grouped_df,
# labelled group keys as value-label text, no row names.
#
# broom::tidy() methods reuse the main table with broom's column names
# (statistic, p.value, parameter, estimate, conf.low, conf.high, ...); they
# are registered on broom load in R/zzz.R (broom is only suggested).
# =============================================================================


# ---- class registry -----------------------------------------------------------

#' Result classes that can be exported (as.data.frame, write_xlsx, tidy)
#' @noRd
.xp_result_classes <- c(
  "ancova", "binomial_test", "chi_square", "chisq_gof", "codebook",
  "crosstab", "describe", "dunn_test", "efa", "factorial_anova",
  "fisher_test", "frequency", "friedman_test", "kendall_tau",
  "kruskal_wallis", "levene_test", "linear_regression",
  "logistic_regression", "mann_whitney", "marginal_effects", "mcnemar_test",
  "multiple_response", "normality_test", "oneway_anova",
  "pairwise_wilcoxon", "partial_cor", "pearson_cor", "reliability",
  "scheffe_test", "spearman_rho", "t_test", "tukey_test", "w_iqr",
  "w_kurtosis", "w_mean", "w_median", "w_modus", "w_quantile", "w_range",
  "w_sd", "w_se", "w_skew", "w_var", "wilcoxon_test"
)

#' Classes that get a broom::tidy() method from this file
#'
#' The regressions keep their lm/glm-based tidiers (R/broom_methods.R); a
#' codebook is a data dictionary, not a statistical result.
#' @noRd
.xp_tidy_classes <- setdiff(.xp_result_classes,
                            c("linear_regression", "logistic_regression",
                              "codebook"))

#' The exportable result class of an object (NA if none)
#' @noRd
.xp_class <- function(x) {
  hit <- class(x)[class(x) %in% .xp_result_classes]
  if (length(hit) == 0L) NA_character_ else hit[1]
}

#' Display title of a result class (main table, Excel sheet)
#' @noRd
.xp_title <- function(cls) {
  switch(cls,
    ancova              = "ANCOVA",
    binomial_test       = "Binomial Test",
    chi_square          = "Chi-Square Test",
    chisq_gof           = "Chi-Square Goodness of Fit",
    codebook            = "Codebook",
    crosstab            = "Crosstabulation",
    describe            = "Descriptive Statistics",
    dunn_test           = "Dunn Test",
    efa                 = "Factor Loadings",
    factorial_anova     = "ANOVA",
    fisher_test         = "Fisher Exact Test",
    frequency           = "Frequencies",
    friedman_test       = "Friedman Test",
    kendall_tau         = "Kendall Correlations",
    kruskal_wallis      = "Kruskal-Wallis Test",
    levene_test         = "Levene Test",
    linear_regression   = "Coefficients",
    logistic_regression = "Coefficients",
    mann_whitney        = "Mann-Whitney Test",
    marginal_effects    = "Average Marginal Effects",
    mcnemar_test        = "McNemar Test",
    multiple_response   = "Multiple Response",
    normality_test      = "Tests of Normality",
    oneway_anova        = "One-Way ANOVA",
    pairwise_wilcoxon   = "Pairwise Wilcoxon Tests",
    partial_cor         = "Partial Correlations",
    pearson_cor         = "Pearson Correlations",
    reliability         = "Reliability Statistics",
    scheffe_test        = "Scheffe Test",
    spearman_rho        = "Spearman Correlations",
    t_test              = "t-Test",
    tukey_test          = "Tukey HSD",
    w_iqr               = "Weighted IQR",
    w_kurtosis          = "Weighted Kurtosis",
    w_mean              = "Weighted Mean",
    w_median            = "Weighted Median",
    w_modus             = "Weighted Mode",
    w_quantile          = "Weighted Quantiles",
    w_range             = "Weighted Range",
    w_sd                = "Weighted SD",
    w_se                = "Weighted SE",
    w_skew              = "Weighted Skewness",
    w_var               = "Weighted Variance",
    wilcoxon_test       = "Wilcoxon Signed-Rank Test",
    cls
  )
}


# ---- core -------------------------------------------------------------------

#' All export tables of a result object
#'
#' @param x A mariposa result object
#' @return Named list of plain data frames; the first is the main table
#' @noRd
.xp_tables <- function(x, call = rlang::caller_env()) {
  cls <- .xp_class(x)
  if (is.na(cls)) {
    cli::cli_abort(c(
      "Cannot convert an object of class {.cls {class(x)}} to a table.",
      "i" = "Table export works for the results of mariposa's analysis functions."
    ), call = call)
  }
  tabs <- switch(cls,
    ancova              = .xp_ancova(x),
    chi_square          = .xp_chi_square(x),
    chisq_gof           = .xp_chisq_gof(x),
    codebook            = .xp_codebook(x),
    crosstab            = .xp_crosstab(x),
    describe            = .xp_wide(x, cls),
    efa                 = .xp_efa(x),
    factorial_anova     = .xp_factorial(x),
    fisher_test         = .xp_fisher(x),
    frequency           = .xp_frequency(x),
    friedman_test       = .xp_friedman(x),
    kendall_tau         = ,
    partial_cor         = ,
    pearson_cor         = ,
    spearman_rho        = list(x$correlations),
    kruskal_wallis      = .xp_kruskal(x),
    linear_regression   = .xp_linear(x),
    logistic_regression = .xp_logistic(x),
    mann_whitney        = .xp_mann_whitney(x),
    mcnemar_test        = .xp_mcnemar(x),
    multiple_response   = .xp_multiple_response(x),
    oneway_anova        = .xp_oneway(x),
    reliability         = .xp_reliability(x),
    t_test              = .xp_t_test(x),
    w_quantile          = .xp_wide(x, cls),
    list(x[["results"]] %||% x[["comparisons"]])
  )
  # The main table stays first even when empty; empty secondary tables
  # are dropped
  if (is.null(tabs[[1]])) tabs[[1]] <- data.frame()
  names(tabs)[1] <- .xp_title(cls)
  keep <- c(TRUE, !vapply(tabs[-1], is.null, logical(1)))
  tabs <- tabs[keep]
  gv <- .xp_group_vars(x)
  lapply(tabs, .xp_finish, group_vars = gv)
}

#' The main export table of a result object
#' @noRd
.xp_main <- function(x, call = rlang::caller_env()) {
  .xp_tables(x, call = call)[[1]]
}

#' Grouping variables of a result object
#'
#' Result classes store them as `$group_vars` or `$groups` (the latter is a
#' list of per-group results for reliability/efa/regressions).
#' @noRd
.xp_group_vars <- function(x) {
  if (!isTRUE(x[["is_grouped"]])) return(character(0))
  gv <- x[["group_vars"]]
  if (!is.character(gv)) gv <- x[["groups"]]
  if (is.character(gv)) gv else character(0)
}

#' Make an export table plain
#'
#' Drops list-columns (and AsIs list-columns such as chi_square()'s
#' observed tables), ungroups, turns labelled columns into value-label text
#' (factor in code order, see .group_factor()), strips names from atomic
#' columns, puts grouping variables first and resets row names.
#' @noRd
.xp_finish <- function(df, group_vars = character(0)) {
  if (is.null(df)) return(NULL)
  if (inherits(df, "grouped_df")) df <- dplyr::ungroup(df)
  df <- as.data.frame(df, stringsAsFactors = FALSE, optional = TRUE)
  df <- df[!vapply(df, is.list, logical(1))]
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled")) {
      col <- .group_factor(col)
    } else if (inherits(col, "AsIs")) {
      col <- unclass(col)
    }
    if (is.atomic(col) && !is.null(names(col))) names(col) <- NULL
    df[[nm]] <- col
  }
  gv <- intersect(group_vars, names(df))
  df <- df[c(gv, setdiff(names(df), gv))]
  rownames(df) <- NULL
  df
}

#' Scalar from a nested list (NA when absent or not a scalar)
#' @noRd
.xp_get <- function(l, ...) {
  for (k in c(...)) {
    if (!is.list(l) || is.null(l[[k]])) return(NA)
    l <- l[[k]]
  }
  if (length(l) != 1L) NA else unname(l)
}

#' Columns of a data frame without list-columns
#' @noRd
.xp_flat <- function(df) {
  if (inherits(df, "grouped_df")) df <- dplyr::ungroup(df)
  df <- as.data.frame(df, stringsAsFactors = FALSE, optional = TRUE)
  df[!vapply(df, is.list, logical(1))]
}

#' Insert columns after a given column (at the front if it is absent)
#' @noRd
.xp_insert <- function(df, new, after = "Variable") {
  pos <- match(after, names(df))
  if (is.na(pos)) pos <- 0L
  left <- df[seq_len(pos)]
  right <- df[setdiff(seq_along(df), seq_len(pos))]
  cbind(left, as.data.frame(new, stringsAsFactors = FALSE, optional = TRUE),
        right)
}

#' Long table from a per-row list-column of per-level lists
#'
#' @param res Results table (one row per test)
#' @param col Name of the list-column; each cell is a list of levels, each
#'   level a list of scalars
#' @param keep Identifier columns of `res` repeated on every level row
#' @param level_field Field holding the level name ("level" / "name")
#' @param fields Numeric fields to extract, named by their output column
#' @noRd
.xp_expand <- function(res, col, keep, level_field, fields,
                       level_col = "group") {
  if (inherits(res, "grouped_df")) res <- dplyr::ungroup(res)
  if (!col %in% names(res)) return(NULL)
  keep <- intersect(keep, names(res))
  rows <- lapply(seq_len(nrow(res)), function(i) {
    items <- res[[col]][[i]]
    if (!is.list(items) || length(items) == 0L) return(NULL)
    items <- items[vapply(items, is.list, logical(1))]
    if (length(items) == 0L) return(NULL)
    out <- res[rep(i, length(items)), keep, drop = FALSE]
    out[[level_col]] <- vapply(items, function(it) {
      as.character(.xp_get(it, level_field))
    }, character(1))
    for (nm in names(fields)) {
      out[[nm]] <- vapply(items, function(it) {
        as.numeric(.xp_get(it, fields[[nm]]))
      }, numeric(1))
    }
    out
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) return(NULL)
  dplyr::bind_rows(rows)
}

#' Bind per-group tables of a list-of-results object
#'
#' reliability(), efa(), the regressions and crosstab() store grouped
#' results as a list of per-group results, each carrying its group key in
#' `$group_values` (or `$group_info`).
#' @noRd
.xp_bind_groups <- function(items, fun) {
  out <- lapply(items, function(it) {
    tab <- fun(it)
    if (is.null(tab) || nrow(tab) == 0L) return(NULL)
    tab <- .xp_flat(tab)
    key <- it[["group_values"]] %||% it[["group_info"]]
    if (is.null(key)) return(tab)
    key <- lapply(as.list(key), function(v) rep(v[1], nrow(tab)))
    cbind(tibble::as_tibble(key), tab)
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0L) return(NULL)
  dplyr::bind_rows(out)
}

#' Per-group dispatch for results that are either one fit or a list of fits
#' @noRd
.xp_by_group <- function(x, fun) {
  if (isTRUE(x[["is_grouped"]]) && is.list(x[["groups"]]) &&
      !is.data.frame(x[["groups"]])) {
    .xp_bind_groups(x$groups, fun)
  } else {
    tab <- fun(x)
    if (is.null(tab)) NULL else .xp_flat(tab)
  }
}


# ---- wide tables (describe, w_quantile) ---------------------------------------

#' One row per variable from a "<variable>_<statistic>" wide table
#'
#' Longer variable names claim their columns first, so `trust` never takes
#' the statistics of `trust_media`.
#' @noRd
.xp_wide <- function(x, cls) {
  res <- .xp_flat(x$results)
  vars <- x$variables
  gv <- intersect(.xp_group_vars(x), names(res))
  cols <- setdiff(names(res), gv)
  claimed <- character(0)
  owned <- list()
  for (v in vars[order(-nchar(vars))]) {
    pre <- paste0(v, "_")
    hit <- cols[startsWith(cols, pre) & !cols %in% claimed]
    owned[[v]] <- hit
    claimed <- c(claimed, hit)
  }
  blocks <- lapply(seq_along(vars), function(j) {
    v <- vars[j]
    stats <- res[owned[[v]]]
    names(stats) <- substring(owned[[v]], nchar(v) + 2L)
    cbind(res[gv], Variable = rep(v, nrow(res)), stats,
          .row = seq_len(nrow(res)), .var = j, stringsAsFactors = FALSE)
  })
  out <- dplyr::bind_rows(blocks)
  out <- out[order(out$.row, out$.var), , drop = FALSE]
  out$.row <- NULL
  out$.var <- NULL
  list(out)
}


# ---- descriptive classes -----------------------------------------------------

#' frequency(): one row per category, summary rows dropped
#'
#' The tagged-missing layout interleaves "Total Valid"/"Total Missing"
#' summary rows and keeps missing codes in `na_display_value`; the export
#' keeps category rows only, with the missing code as value and a
#' `missing` flag.
#' @noRd
.xp_frequency <- function(x) {
  res <- .xp_flat(x$results)
  if ("na_display_value" %in% names(res)) {
    ndv <- res$na_display_value
    total_row <- !is.na(ndv) & ndv %in% c("Total", "NA(total)")
    miss <- res$is_na_row %in% TRUE
    code <- ifelse(miss & !total_row & !is.na(ndv) & ndv != "NA", ndv,
                   NA_character_)
    if (any(!is.na(code))) {
      num_code <- suppressWarnings(as.numeric(code))
      if (is.numeric(res$value) && !anyNA(num_code[!is.na(code)])) {
        res$value[!is.na(code)] <- num_code[!is.na(code)]
      } else {
        res$value <- as.character(res$value)
        res$value[!is.na(code)] <- code[!is.na(code)]
      }
    }
    res$missing <- miss
    res <- res[!total_row, setdiff(names(res), c("is_na_row", "na_display_value")),
               drop = FALSE]
  } else {
    res$missing <- is.na(res$value)
  }
  front <- intersect(c("Variable", "value", "label"), names(res))
  list(res[c(front, setdiff(names(res), front))])
}

#' crosstab(): one row per cell (row category x column category)
#' @noRd
.xp_crosstab_cells <- function(ct) {
  tab <- ct$table
  if (is.null(tab) || length(tab) == 0L) return(NULL)
  # Value labels as category text; two codes sharing a label keep their
  # code so the categories stay distinct
  lab <- function(lv, map) {
    shown <- unname(.apply_labels(lv, map, max_width = Inf))
    dup <- shown %in% shown[duplicated(shown)] & shown != lv
    shown[dup] <- paste0(shown[dup], " (", lv[dup], ")")
    shown
  }
  row_lv <- lab(ct$row_levels, ct$row_label_map)
  col_lv <- lab(ct$col_levels, ct$col_label_map)
  nr <- length(row_lv)
  nc <- length(col_lv)
  out <- data.frame(
    row = factor(rep(row_lv, times = nc), levels = unique(row_lv)),
    col = factor(rep(col_lv, each = nr), levels = unique(col_lv)),
    n = as.vector(unclass(tab)),
    stringsAsFactors = FALSE
  )
  cell <- function(m) if (is.null(m)) NULL else as.vector(unclass(m))
  add <- list(row_pct = cell(ct$row_pct), col_pct = cell(ct$col_pct),
              total_pct = cell(ct$total_pct), expected = cell(ct$expected),
              adj_residual = cell(ct$adj_residuals))
  for (nm in names(add)) {
    if (length(add[[nm]]) == nrow(out)) out[[nm]] <- add[[nm]]
  }
  # Category columns are named after the variables (as in
  # as.data.frame(table)); a clash with a statistic column gets a suffix
  key <- c(ct$row_var %||% "row", ct$col_var %||% "col")
  if (key[1] == key[2]) key[2] <- paste0(key[2], "_2")
  clash <- key %in% names(out)[-(1:2)]
  key[clash] <- paste0(key[clash], "_category")
  names(out)[1:2] <- key
  out
}

#' @noRd
.xp_crosstab <- function(x) {
  if (isTRUE(x$is_grouped)) {
    list(.xp_bind_groups(x$results, .xp_crosstab_cells))
  } else {
    list(.xp_crosstab_cells(x))
  }
}

#' codebook(): one row per variable, value lists as text
#' @noRd
.xp_codebook <- function(x) {
  cb <- x$codebook
  pairs <- function(v, keys = names(v)) {
    if (length(v) == 0L) return("")
    if (is.null(keys)) return(paste(v, collapse = "; "))
    paste(paste0(keys, " = ", v), collapse = "; ")
  }
  out <- .xp_flat(cb)
  out$values <- vapply(cb$empirical_values, function(v) {
    paste(v[!is.na(v)], collapse = "; ")
  }, character(1))
  # "1 = Low; 2 = High"; factor levels are their own labels ("Male; Female")
  out$value_labels <- vapply(cb$value_labels, function(v) {
    if (is.null(v)) return("")
    if (identical(names(v), unname(v))) pairs(unname(v), NULL)
    else pairs(unname(v), names(v))
  }, character(1))
  out$missing_values <- vapply(seq_len(nrow(cb)), function(i) {
    nav <- cb$na_values[[i]]
    if (length(nav) == 0L) return("")
    lbl <- cb$na_labels[[i]]
    shown <- ifelse(nav %in% names(lbl), paste0(nav, " = ", lbl[nav]), nav)
    paste(shown, collapse = "; ")
  }, character(1))
  drop <- intersect(c("is_range", "truncated"), names(out))
  list(out[setdiff(names(out), drop)])
}

#' multiple_response(): the response table (+ the by-crosstab)
#' @noRd
.xp_multiple_response <- function(x) {
  by_tab <- x$by_results
  if (!is.null(by_tab) && !is.null(x$by)) {
    names(by_tab)[names(by_tab) == "by_level"] <- x$by
  }
  list(x$results, `By Group` = by_tab)
}


# ---- parametric tests ---------------------------------------------------------

#' @noRd
.xp_t_test <- function(x) {
  res <- .xp_flat(x$results)
  gs <- if (inherits(x$results, "grouped_df")) {
    dplyr::ungroup(x$results)$group_stats
  } else {
    x$results$group_stats
  }
  if (!is.null(gs)) {
    if (is.null(x$group)) {
      res <- .xp_insert(res, list(
        mean = vapply(gs, function(g) as.numeric(.xp_get(g, "means")), 1),
        sd = vapply(gs, function(g) as.numeric(.xp_get(g, "sd")), 1)
      ), after = "Variable")
    } else {
      res <- .xp_insert(res, list(
        group1 = vapply(gs, function(g) as.character(.xp_get(g, "group1", "name")), ""),
        group2 = vapply(gs, function(g) as.character(.xp_get(g, "group2", "name")), "")
      ), after = "Variable")
      res <- .xp_insert(res, list(
        mean1 = vapply(gs, function(g) as.numeric(.xp_get(g, "group1", "mean")), 1),
        mean2 = vapply(gs, function(g) as.numeric(.xp_get(g, "group2", "mean")), 1),
        sd1 = vapply(gs, function(g) as.numeric(.xp_get(g, "group1", "sd")), 1),
        sd2 = vapply(gs, function(g) as.numeric(.xp_get(g, "group2", "sd")), 1)
      ), after = "n2")
    }
  }
  list(res)
}

#' @noRd
.xp_oneway <- function(x) {
  gv <- .xp_group_vars(x)
  desc <- .xp_expand(x$results, "group_stats", c(gv, "Variable"), "level",
                     c(n = "n", mean = "mean", sd = "sd", se = "se",
                       ci_lower = "ci_lower", ci_upper = "ci_upper"))
  list(x$results, Descriptives = desc)
}

#' @noRd
.xp_factorial <- function(x) {
  fit <- NULL
  if (is.numeric(x$r_squared) && length(x$r_squared) >= 2L) {
    fit <- data.frame(r_squared = unname(x$r_squared[1]),
                      adj_r_squared = unname(x$r_squared[2]))
  }
  list(x$anova_table,
       Descriptives = x$descriptives,
       `Levene Test` = if (is.data.frame(x$levene_test)) x$levene_test,
       `Model Fit` = fit)
}

#' @noRd
.xp_ancova <- function(x) {
  emm_main <- NULL
  if (is.list(x$emm_main_effects) && length(x$emm_main_effects) > 0L) {
    emm_main <- lapply(names(x$emm_main_effects), function(f) {
      tab <- .xp_flat(x$emm_main_effects[[f]])
      names(tab)[names(tab) == f] <- "level"
      cbind(factor = f, tab, stringsAsFactors = FALSE)
    })
    emm_main <- dplyr::bind_rows(lapply(emm_main, function(t) {
      t$level <- as.character(t$level)
      t
    }))
  }
  list(x$anova_table,
       `Parameter Estimates` = x$parameter_estimates,
       `Estimated Marginal Means` = x$estimated_marginal_means,
       `Marginal Means (Main Effects)` = emm_main,
       Descriptives = x$descriptives,
       `Levene Test` = if (is.data.frame(x$levene_test)) x$levene_test)
}


# ---- nonparametric and categorical tests --------------------------------------

#' @noRd
.xp_mann_whitney <- function(x) {
  res <- .xp_flat(x$results)
  gs <- dplyr::ungroup(x$results)$group_stats
  if (!is.null(gs)) {
    res <- .xp_insert(res, list(
      group1 = vapply(gs, function(g) as.character(.xp_get(g, "group1", "name")), ""),
      group2 = vapply(gs, function(g) as.character(.xp_get(g, "group2", "name")), ""),
      n1 = vapply(gs, function(g) as.numeric(.xp_get(g, "group1", "n")), 1),
      n2 = vapply(gs, function(g) as.numeric(.xp_get(g, "group2", "n")), 1),
      mean_rank1 = vapply(gs, function(g) as.numeric(.xp_get(g, "group1", "rank_mean")), 1),
      mean_rank2 = vapply(gs, function(g) as.numeric(.xp_get(g, "group2", "rank_mean")), 1)
    ), after = "Variable")
  }
  list(res)
}

#' @noRd
.xp_kruskal <- function(x) {
  gv <- .xp_group_vars(x)
  ranks <- .xp_expand(x$results, "group_stats", c(gv, "Variable"), "name",
                      c(n = "n", mean_rank = "rank_mean"))
  list(x$results, Ranks = ranks)
}

#' @noRd
.xp_friedman <- function(x) {
  res <- dplyr::ungroup(x$results)
  gv <- intersect(.xp_group_vars(x), names(res))
  ranks <- NULL
  if ("mean_ranks" %in% names(res)) {
    ranks <- dplyr::bind_rows(lapply(seq_len(nrow(res)), function(i) {
      mr <- res$mean_ranks[[i]]
      if (!is.list(mr) && !is.numeric(mr)) return(NULL)
      if (length(mr) == 0L) return(NULL)
      out <- as.data.frame(res[rep(i, length(mr)), gv, drop = FALSE])
      out$Variable <- names(mr)
      out$mean_rank <- as.numeric(unlist(mr, use.names = FALSE))
      out
    }))
  }
  list(res, Ranks = ranks)
}

#' @noRd
.xp_chi_square <- function(x) {
  res <- .xp_flat(x$results)
  vars <- x$variables
  if (length(vars) == 2L) {
    res <- .xp_insert(res, list(row_var = rep(vars[1], nrow(res)),
                                col_var = rep(vars[2], nrow(res))),
                      after = "")
  }
  list(res)
}

#' @noRd
.xp_fisher <- function(x) {
  res <- .xp_flat(x$results)
  if (!is.null(x$row_var) && !is.null(x$col_var)) {
    res <- .xp_insert(res, list(row_var = rep(x$row_var, nrow(res)),
                                col_var = rep(x$col_var, nrow(res))),
                      after = "")
  }
  list(res)
}

#' @noRd
.xp_mcnemar <- function(x) {
  res <- .xp_flat(x$results)
  if (!is.null(x$var1_name) && !is.null(x$var2_name)) {
    res <- .xp_insert(res, list(var1 = rep(x$var1_name, nrow(res)),
                                var2 = rep(x$var2_name, nrow(res))),
                      after = "")
  }
  list(res)
}

#' @noRd
.xp_chisq_gof <- function(x) {
  freq <- x$frequencies
  if (is.data.frame(freq) && length(x$variables) == 1L) {
    freq <- cbind(Variable = x$variables, .xp_flat(freq),
                  stringsAsFactors = FALSE)
  } else {
    freq <- NULL
  }
  list(x$results, Frequencies = freq)
}


# ---- scales -----------------------------------------------------------------------

#' @noRd
.xp_reliability <- function(x) {
  scale_row <- function(r) {
    data.frame(
      n_items = as.integer(r$n_items %||% NA_integer_),
      alpha = as.numeric(r$alpha %||% NA_real_),
      alpha_standardized = as.numeric(r$alpha_standardized %||% NA_real_),
      omega = as.numeric(r$omega %||% NA_real_),
      omega_standardized = as.numeric(r$omega_std %||% NA_real_),
      n = as.numeric(r$n %||% NA_real_),
      weighted_n = as.numeric(r$weighted_n %||% NA_real_)
    )
  }
  list(.xp_by_group(x, scale_row),
       `Item Statistics` = .xp_by_group(x, function(r) r$item_statistics),
       `Item-Total Statistics` = .xp_by_group(x, function(r) r$item_total))
}

#' @noRd
.xp_efa <- function(x) {
  loadings <- function(r) {
    L <- r$loadings
    if (is.null(L)) return(NULL)
    L <- unclass(L)
    out <- data.frame(Variable = rownames(L), stringsAsFactors = FALSE)
    for (j in seq_len(ncol(L))) out[[colnames(L)[j]]] <- unname(L[, j])
    comm <- r$communalities
    if (!is.null(comm)) out$communality <- unname(comm[out$Variable])
    out
  }
  structure_m <- function(r) {
    S <- r$structure_matrix
    if (is.null(S)) return(NULL)
    S <- unclass(S)
    out <- data.frame(Variable = rownames(S), stringsAsFactors = FALSE)
    for (j in seq_len(ncol(S))) out[[colnames(S)[j]]] <- unname(S[, j])
    out
  }
  factor_cor <- function(r) {
    Phi <- r$factor_correlations
    if (is.null(Phi)) return(NULL)
    Phi <- unclass(Phi)
    out <- data.frame(factor = rownames(Phi) %||% colnames(Phi),
                      stringsAsFactors = FALSE)
    for (j in seq_len(ncol(Phi))) out[[colnames(Phi)[j]]] <- unname(Phi[, j])
    out
  }
  fit <- function(r) {
    out <- data.frame(
      kmo = as.numeric(.xp_get(r, "kmo", "overall")),
      bartlett_chi_sq = as.numeric(.xp_get(r, "bartlett", "chi_sq")),
      bartlett_df = as.numeric(.xp_get(r, "bartlett", "df")),
      bartlett_p = as.numeric(.xp_get(r, "bartlett", "p_value")),
      n = as.numeric(r$n %||% NA_real_)
    )
    if (is.list(r$goodness_of_fit)) {
      out$gof_chi_sq <- as.numeric(.xp_get(r, "goodness_of_fit", "chi_sq"))
      out$gof_df <- as.numeric(.xp_get(r, "goodness_of_fit", "df"))
      out$gof_p <- as.numeric(.xp_get(r, "goodness_of_fit", "p_value"))
    }
    out
  }
  list(.xp_by_group(x, loadings),
       `Structure Matrix` = .xp_by_group(x, structure_m),
       `Factor Correlations` = .xp_by_group(x, factor_cor),
       `Total Variance Explained` = .xp_by_group(x, function(r) r$variance_explained),
       `Rotation Sums of Squared Loadings` = .xp_by_group(x, function(r) r$rotation_variance),
       `KMO and Bartlett` = .xp_by_group(x, fit))
}


# ---- regressions --------------------------------------------------------------

#' @noRd
.xp_linear <- function(x) {
  summ <- function(r) {
    ms <- r$model_summary
    if (is.null(ms)) return(NULL)
    out <- as.data.frame(lapply(ms, function(v) as.numeric(v[1])))
    at <- r$anova_table
    if (is.data.frame(at) && nrow(at) >= 2L) {
      out$F_statistic <- as.numeric(at$F_statistic[1])
      out$df1 <- as.numeric(at$df[1])
      out$df2 <- as.numeric(at$df[2])
      out$p_value <- as.numeric(at$Sig[1])
    }
    out$n <- as.numeric(r$n[1] %||% NA_real_)
    out
  }
  list(.xp_by_group(x, function(r) r$coef_table),
       `Model Summary` = .xp_by_group(x, summ),
       ANOVA = .xp_by_group(x, function(r) r$anova_table))
}

#' @noRd
.xp_logistic <- function(x) {
  summ <- function(r) {
    ms <- r$model_summary
    if (is.null(ms)) return(NULL)
    out <- as.data.frame(lapply(ms, function(v) as.numeric(v[1])))
    out$n <- as.numeric(r$n[1] %||% NA_real_)
    out
  }
  tests <- function(r) {
    rows <- list()
    if (is.list(r$omnibus_test)) {
      rows[[length(rows) + 1L]] <- data.frame(
        test = "Omnibus (Model)",
        chi_squared = as.numeric(.xp_get(r, "omnibus_test", "chi_sq")),
        df = as.numeric(.xp_get(r, "omnibus_test", "df")),
        p_value = as.numeric(.xp_get(r, "omnibus_test", "p")),
        stringsAsFactors = FALSE)
    }
    if (is.list(r$hosmer_lemeshow)) {
      rows[[length(rows) + 1L]] <- data.frame(
        test = "Hosmer-Lemeshow",
        chi_squared = as.numeric(.xp_get(r, "hosmer_lemeshow", "chi_sq")),
        df = as.numeric(.xp_get(r, "hosmer_lemeshow", "df")),
        p_value = as.numeric(.xp_get(r, "hosmer_lemeshow", "p")),
        stringsAsFactors = FALSE)
    }
    if (length(rows) == 0L) NULL else do.call(rbind, rows)
  }
  classification <- function(r) {
    cl <- r$classification
    if (!is.list(cl)) return(NULL)
    scal <- cl[vapply(cl, function(v) is.atomic(v) && length(v) == 1L,
                      logical(1))]
    if (length(scal) == 0L) NULL else as.data.frame(scal)
  }
  list(.xp_by_group(x, function(r) r$coef_table),
       `Model Summary` = .xp_by_group(x, summ),
       `Model Tests` = .xp_by_group(x, tests),
       Classification = .xp_by_group(x, classification))
}


# ---- broom-style tidy table ---------------------------------------------------------

#' Column renames (new = old) and test labels for tidy()
#' @noRd
.xp_tidy_spec <- function(x, cls) {
  alt <- x[["alternative"]]
  switch(cls,
    t_test = list(
      rename = c(estimate = "mean_diff", statistic = "t_stat",
                 parameter = "df", p.value = "p_value",
                 conf.low = "conf_int_lower", conf.high = "conf_int_upper"),
      method = if (is.null(x$group)) {
        "One-sample t-test"
      } else if (isTRUE(x$var.equal)) {
        "Two-sample t-test"
      } else {
        "Welch two-sample t-test"
      },
      alternative = alt),
    oneway_anova = list(
      rename = c(statistic = "F_statistic", num.df = "df1", den.df = "df2",
                 p.value = "p_value"),
      method = "One-way ANOVA"),
    levene_test = list(
      rename = c(statistic = "F_statistic", num.df = "df1", den.df = "df2",
                 p.value = "p_value"),
      method = "Levene's test"),
    factorial_anova = ,
    ancova = list(
      rename = c(term = "source", sumsq = "ss", meansq = "ms",
                 statistic = "f", p.value = "p")),
    mann_whitney = list(
      rename = c(statistic = "U", p.value = "p_value"),
      method = "Mann-Whitney U test", alternative = alt),
    kruskal_wallis = list(
      rename = c(statistic = "H", parameter = "df", p.value = "p_value"),
      method = "Kruskal-Wallis H test"),
    wilcoxon_test = list(
      rename = c(statistic = "Z", p.value = "p_value"),
      method = "Wilcoxon signed-rank test", alternative = alt),
    friedman_test = list(
      rename = c(statistic = "chi_squared", parameter = "df",
                 p.value = "p_value"),
      method = "Friedman test"),
    binomial_test = list(
      rename = c(estimate = "obs_prop1", statistic = "n1",
                 parameter = "n_total", p.value = "p_value",
                 conf.low = "ci_lower", conf.high = "ci_upper"),
      method = "Exact binomial test", alternative = alt),
    chi_square = list(
      rename = c(statistic = "chi_squared", parameter = "df",
                 p.value = "p_value"),
      method = "Pearson's chi-squared test"),
    chisq_gof = list(
      rename = c(statistic = "chi_squared", parameter = "df",
                 p.value = "p_value"),
      method = "Chi-squared goodness-of-fit test"),
    mcnemar_test = list(
      rename = c(statistic = "chi_squared", parameter = "df",
                 p.value = "p_value"),
      method = "McNemar's test"),
    fisher_test = list(
      rename = c(estimate = "odds_ratio", conf.low = "or_ci_lower",
                 conf.high = "or_ci_upper", p.value = "p_value")),
    tukey_test = list(
      rename = c(contrast = "Comparison", estimate = "Estimate",
                 std.error = "SE", statistic = "t_value",
                 conf.low = "conf_low", conf.high = "conf_high",
                 adj.p.value = "p_adjusted"),
      method = "Tukey HSD"),
    scheffe_test = list(
      rename = c(contrast = "Comparison", estimate = "Estimate",
                 std.error = "SE", statistic = "F_value",
                 conf.low = "conf_low", conf.high = "conf_high",
                 adj.p.value = "p_adjusted"),
      method = "Scheffe test"),
    dunn_test = ,
    pairwise_wilcoxon = list(
      rename = c(statistic = "z", p.value = "p", adj.p.value = "p_adj")),
    pearson_cor = list(
      rename = c(estimate = "correlation", p.value = "p_value",
                 conf.low = "conf_int_lower", conf.high = "conf_int_upper"),
      method = "Pearson correlation", alternative = alt),
    spearman_rho = list(
      rename = c(estimate = "rho", statistic = "t_stat", p.value = "p_value"),
      method = "Spearman rank correlation", alternative = alt),
    kendall_tau = list(
      rename = c(estimate = "tau", statistic = "z_score", p.value = "p_value"),
      method = "Kendall's tau-b", alternative = alt),
    partial_cor = list(
      rename = c(estimate = "partial_r", statistic = "t_stat",
                 parameter = "df", p.value = "p_value"),
      method = "Partial correlation"),
    marginal_effects = list(
      rename = c(term = "Term", type = "Type", estimate = "AME",
                 std.error = "SE", statistic = "z", p.value = "p_value",
                 conf.low = "CI_lower", conf.high = "CI_upper")),
    list()
  )
}

#' normality_test(): one row per variable and test (tidy long format)
#' @noRd
.xp_tidy_normality <- function(df) {
  id <- setdiff(names(df), c("ks_statistic", "ks_df", "ks_p", "shapiro_w",
                             "shapiro_p", "n"))
  ks <- df[id]
  ks$method <- "Kolmogorov-Smirnov (Lilliefors)"
  ks$statistic <- df$ks_statistic
  ks$parameter <- as.numeric(df$ks_df)
  ks$p.value <- df$ks_p
  sw <- df[id]
  sw$method <- "Shapiro-Wilk"
  sw$statistic <- df$shapiro_w
  sw$parameter <- as.numeric(df$n)
  sw$p.value <- df$shapiro_p
  out <- rbind(ks, sw)
  out[order(rep(seq_len(nrow(df)), 2L)), , drop = FALSE]
}

#' broom::tidy() for mariposa results
#'
#' The main export table with broom's column names. Registered for every
#' class in .xp_tidy_classes when broom is loaded (see R/zzz.R).
#'
#' @param x A mariposa result object
#' @param ... Ignored
#' @return A tibble
#' @noRd
.xp_tidy <- function(x, ...) {
  cls <- .xp_class(x)
  df <- .xp_main(x)
  if (identical(cls, "normality_test")) {
    df <- .xp_tidy_normality(df)
  } else {
    spec <- .xp_tidy_spec(x, cls)
    ren <- spec$rename
    ren <- ren[ren %in% names(df)]
    names(df)[match(ren, names(df))] <- names(ren)
    if (!is.null(spec$method) && !"method" %in% names(df)) {
      df$method <- rep(spec$method, nrow(df))
    }
    if (length(spec$alternative) == 1L && !"alternative" %in% names(df)) {
      df$alternative <- rep(spec$alternative, nrow(df))
    }
  }
  names(df)[names(df) == "Variable"] <- "variable"
  df <- df[setdiff(names(df), "sig")]
  rownames(df) <- NULL
  tibble::as_tibble(df)
}


# ---- exported methods --------------------------------------------------------------

#' Convert Analysis Results to Data Frames
#'
#' @description
#' Turns the result of any mariposa analysis into a plain data frame that
#' can be saved with [utils::write.csv()], combined with
#' [dplyr::bind_rows()] or used for plotting. [tibble::as_tibble()] works
#' the same way (it calls `as.data.frame()`), [write_xlsx()] writes the
#' table (plus the secondary tables SPSS shows next to it) to Excel, and
#' with the \pkg{broom} package loaded, `broom::tidy()` returns the table
#' with broom's column names.
#'
#' @param x A result of one of mariposa's analysis functions, e.g.
#'   [t_test()], [describe()], [crosstab()] or [linear_regression()].
#' @param row.names,optional Ignored; present for compatibility with
#'   [base::as.data.frame()].
#' @param ... Ignored.
#'
#' @return A plain `data.frame` (no list-columns, no row names) holding
#'   the main result table:
#'   \itemize{
#'     \item one row per variable (per test) for tests and descriptive
#'       statistics, with the columns of `x$results`; group-level details
#'       stored as list-columns are flattened into columns (for example
#'       `group1`, `group2`, `mean1`, `mean2` for [t_test()]) or dropped
#'       (the observed/expected tables of [chi_square()]);
#'     \item one row per pair for correlations and post-hoc tests;
#'     \item one row per term for ANOVA tables and regression
#'       coefficients, and one row per item for [efa()] loadings;
#'     \item one row per cell for [crosstab()] and per category for
#'       [frequency()] (summary rows dropped, a `missing` column flags
#'       missing values);
#'     \item one row per scale for [reliability()].
#'   }
#'   For grouped analyses ([dplyr::group_by()]) the grouping variables are
#'   the leading columns; labelled grouping variables show their value
#'   labels.
#'
#' @details
#' `broom::tidy()` (available once \pkg{broom} is loaded) renames the
#' common columns to broom's conventions: `statistic`, `p.value`,
#' `parameter` (or `num.df`/`den.df` for F tests), `estimate`, `conf.low`,
#' `conf.high`, `std.error`, `adj.p.value` (post-hoc tests), `term`
#' (ANOVA tables), plus `method` and, where it applies, `alternative`.
#' [normality_test()] results are tidied to one row per variable and test.
#' The regressions keep their own `tidy()`, `glance()` and `augment()`
#' methods.
#'
#' @examples
#' tt <- t_test(survey_data, age, income, group = gender)
#' as.data.frame(tt)
#'
#' # Grouped analyses get the grouping variables as leading columns
#' survey_data |>
#'   dplyr::group_by(region) |>
#'   describe(age, income) |>
#'   as.data.frame()
#'
#' # One row per cell of a crosstab
#' as.data.frame(crosstab(survey_data, gender, region))
#'
#' # Save as CSV or convert to a tibble
#' csv <- tempfile(fileext = ".csv")
#' utils::write.csv(as.data.frame(tt), csv, row.names = FALSE)
#' tibble::as_tibble(chi_square(survey_data, gender, region))
#' unlink(csv)
#'
#' # broom-style columns
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   broom::tidy(oneway_anova(survey_data, age, group = education))
#' }
#'
#' @seealso [write_xlsx()] for Excel export of results.
#' @name as.data.frame.mariposa
NULL

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.ancova <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.binomial_test <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.chi_square <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.chisq_gof <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.codebook <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.crosstab <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.describe <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.dunn_test <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.efa <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.factorial_anova <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.fisher_test <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.frequency <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.friedman_test <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.kendall_tau <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.kruskal_wallis <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.levene_test <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.linear_regression <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.logistic_regression <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.mann_whitney <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.marginal_effects <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.mcnemar_test <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.multiple_response <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.normality_test <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.oneway_anova <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.pairwise_wilcoxon <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.partial_cor <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.pearson_cor <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.reliability <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.scheffe_test <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.spearman_rho <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.t_test <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.tukey_test <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.w_iqr <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.w_kurtosis <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.w_mean <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.w_median <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.w_modus <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.w_quantile <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.w_range <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.w_sd <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.w_se <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.w_skew <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.w_var <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)

#' @rdname as.data.frame.mariposa
#' @export
as.data.frame.wilcoxon_test <- function(x, row.names = NULL, optional = FALSE, ...) .xp_main(x)
