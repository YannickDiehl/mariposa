# =============================================================================
# Factory for Weighted Statistics Functions (w_*)
# =============================================================================
# This file provides the shared infrastructure for all w_* functions.
# Each w_* function delegates to .w_statistic() with its specific computation
# function, reducing ~3,000 lines of duplicated boilerplate to a single
# shared implementation.
#
# Architecture:
#   w_mean(data, ..., weights)  -->  .w_statistic(data, ..., stat_fn, ...)
#   w_sd(data, ..., weights)    -->  .w_statistic(data, ..., stat_fn, ...)
#   ...etc
# =============================================================================

#' Internal factory function for all weighted statistics
#'
#' Handles summarise-context detection, data frame processing, grouped/ungrouped
#' flow, result structuring, and S3 object creation for all w_* functions.
#'
#' @param data Data frame or numeric vector (in summarise context)
#' @param ... Variable selection (tidyselect)
#' @param weights Weight variable (unquoted)
#' @param na.rm Remove missing values
#' @param stat_fn Function(x, w) that computes the statistic. Returns scalar.
#'   Takes (x, w) where x is numeric vector, w is weight vector or NULL.
#' @param stat_name Short name for the statistic (e.g., "mean", "sd")
#' @param weighted_col Column name for weighted results (e.g., "weighted_mean")
#' @param unweighted_col Column name for unweighted results (e.g., "mean")
#' @param class_name S3 class name (e.g., "w_mean")
#' @param extra_args Named list of extra arguments stored in the result object
#' @return S3 object of class `class_name`, or scalar in summarise context
#' @keywords internal
.w_statistic <- function(data, ..., weights = NULL, na.rm = TRUE,
                         stat_fn, stat_name, weighted_col, unweighted_col,
                         class_name, extra_args = list()) {

  # --- Summarise context: data is a numeric vector, not a data frame ---------
  if (is.numeric(data) && !is.data.frame(data)) {
    x <- data
    weights_arg <- substitute(weights)
    weights_vec <- .evaluate_weights(weights_arg, parent.frame())

    if (!.are_weights(weights_vec) || !.validate_weights(weights_vec, verbose = FALSE)) {
      # Unweighted
      if (na.rm) x <- x[!is.na(x)]
      return(stat_fn(x, w = NULL))
    } else {
      # Weighted
      if (na.rm) {
        valid <- !is.na(x) & !is.na(weights_vec)
        x <- x[valid]
        weights_vec <- weights_vec[valid]
      }
      if (length(x) == 0) return(NA_real_)
      return(stat_fn(x, w = weights_vec))
    }
  }

  # --- Data frame mode -------------------------------------------------------
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }

  vars <- .process_variables(data, ...)
  var_names <- names(vars)

  weights_quo <- rlang::enquo(weights)
  if (rlang::quo_is_null(weights_quo)) {
    weights_vec <- NULL
    weights_name <- NULL
  } else {
    weights_name <- rlang::as_name(weights_quo)
    if (!weights_name %in% names(data)) {
      cli_abort("Weights variable {.var {weights_name}} not found in data.")
    }
    weights_vec <- data[[weights_name]]
  }

  is_grouped <- inherits(data, "grouped_df")

  # --- Compute per variable (grouped or ungrouped) ---------------------------
  .compute_vars <- function(df, w_vec) {
    result_cols <- list()
    for (var_name in var_names) {
      x <- df[[var_name]]

      if (is.null(w_vec)) {
        if (na.rm) x <- x[!is.na(x)]
        stat_val <- stat_fn(x, w = NULL)
        n_val <- length(x)
        eff_n <- n_val
      } else {
        if (na.rm) {
          valid <- !is.na(x) & !is.na(w_vec)
          x <- x[valid]
          w <- w_vec[valid]
        } else {
          w <- w_vec
        }

        if (length(x) == 0) {
          stat_val <- NA_real_
          n_val <- 0
          eff_n <- 0
        } else {
          stat_val <- stat_fn(x, w = w)
          n_val <- length(x)
          eff_n <- sum(w)^2 / sum(w^2)
        }
      }

      result_cols[[var_name]] <- stat_val
      result_cols[[paste0(var_name, "_n")]] <- n_val
      result_cols[[paste0(var_name, "_eff_n")]] <- eff_n
    }
    tibble::tibble(!!!result_cols)
  }

  if (is_grouped) {
    group_vars <- dplyr::group_vars(data)

    results <- data %>%
      dplyr::group_modify(~ {
        w_vec <- if (!is.null(weights_name)) .x[[weights_name]] else NULL
        .compute_vars(.x, w_vec)
      }) %>%
      dplyr::ungroup()
  } else {
    results <- .compute_vars(data, weights_vec)
  }

  # --- Transform to standardized long/wide format ---------------------------
  final_results <- .w_format_results(
    results, var_names, weights_name, is_grouped,
    weighted_col = weighted_col,
    unweighted_col = unweighted_col,
    is_grouped_flag = is_grouped
  )

  # --- Create S3 object -----------------------------------------------------
  result <- c(
    list(
      results = final_results,
      variables = var_names,
      weight_var = weights_name,
      weights = weights_name,
      is_grouped = is_grouped,
      grouped = is_grouped,
      groups = if (is_grouped) dplyr::group_vars(data) else NULL
    ),
    extra_args
  )

  class(result) <- class_name
  result
}


#' Format raw w_* results into standardized output structure
#'
#' Handles multi-variable (long format) vs single-variable, and
#' grouped vs ungrouped results.
#'
#' @param results Raw tibble from computation
#' @param var_names Character vector of variable names
#' @param weights_name Weight variable name or NULL
#' @param is_grouped Logical
#' @param weighted_col Name for weighted statistic column
#' @param unweighted_col Name for unweighted statistic column
#' @param is_grouped_flag Same as is_grouped (kept for clarity)
#' @return Formatted tibble
#' @keywords internal
.w_format_results <- function(results, var_names, weights_name, is_grouped,
                              weighted_col, unweighted_col, is_grouped_flag) {

  if (length(var_names) > 1) {
    # Multiple variables: build long format
    results_long <- list()

    if (is_grouped) {
      group_vars <- setdiff(names(results),
                            c(var_names,
                              paste0(var_names, "_n"),
                              paste0(var_names, "_eff_n")))

      group_combinations <- results[group_vars] %>% dplyr::distinct()

      for (var_name in var_names) {
        for (j in seq_len(nrow(group_combinations))) {
          group_filter <- group_combinations[j, , drop = FALSE]
          group_results <- results
          for (grp in names(group_filter)) {
            group_results <- group_results[group_results[[grp]] == group_filter[[grp]], ]
          }

          if (nrow(group_results) > 0) {
            row_data <- group_filter
            row_data$Variable <- var_name

            if (!is.null(weights_name)) {
              row_data[[weighted_col]] <- group_results[[var_name]][1]
              row_data$effective_n <- group_results[[paste0(var_name, "_eff_n")]][1]
            } else {
              row_data[[unweighted_col]] <- group_results[[var_name]][1]
              row_data$n <- group_results[[paste0(var_name, "_n")]][1]
            }

            results_long[[length(results_long) + 1]] <- row_data
          }
        }
      }
    } else {
      for (i in seq_along(var_names)) {
        var_name <- var_names[i]
        row_data <- tibble::tibble(Variable = var_name)

        if (!is.null(weights_name)) {
          row_data[[weighted_col]] <- results[[var_name]][1]
          row_data$effective_n <- results[[paste0(var_name, "_eff_n")]][1]
        } else {
          row_data[[unweighted_col]] <- results[[var_name]][1]
          row_data$n <- results[[paste0(var_name, "_n")]][1]
        }

        results_long[[i]] <- row_data
      }
    }

    dplyr::bind_rows(results_long)

  } else {
    # Single variable: direct column mapping
    var_name <- var_names[1]

    if (!is.null(weights_name)) {
      results %>%
        dplyr::mutate(
          !!weighted_col := !!rlang::sym(var_name),
          effective_n = !!rlang::sym(paste0(var_name, "_eff_n"))
        )
    } else {
      results %>%
        dplyr::mutate(
          !!unweighted_col := !!rlang::sym(var_name),
          n = !!rlang::sym(paste0(var_name, "_n"))
        )
    }
  }
}


#' Generic print method for w_* statistic objects
#'
#' Shared print implementation used by all standard w_* functions.
#'
#' @param x A w_* object
#' @param stat_label Display name (e.g., "Mean", "Standard Deviation")
#' @param weighted_col Column name for weighted values
#' @param unweighted_col Column name for unweighted values
#' @param digits Number of decimal places
#' @keywords internal
.print_w_statistic <- function(x, stat_label, weighted_col, unweighted_col,
                               digits = 3) {
  test_type <- get_standard_title(stat_label, x$weights, "Statistics")
  print_header(test_type)

  is_grouped_data <- !is.null(x$is_grouped) && x$is_grouped

  .make_output_df <- function(var_name, data_row) {
    if (!is.null(x$weights)) {
      data.frame(
        Variable = var_name,
        stat = round(data_row[[weighted_col]][1], digits),
        Effective_N = round(data_row$effective_n[1], 1),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        Variable = var_name,
        stat = round(data_row[[unweighted_col]][1], digits),
        N = round(data_row$n[1], 0),
        stringsAsFactors = FALSE
      )
    }
  }

  # Rename the generic "stat" column to the proper name
  .rename_stat <- function(df) {
    col <- if (!is.null(x$weights)) weighted_col else unweighted_col
    names(df)[names(df) == "stat"] <- col
    df
  }

  if (is_grouped_data) {
    for (group_val in unique(x$results[[x$groups[1]]])) {
      group_results <- x$results[x$results[[x$groups[1]]] == group_val, ]
      cat(sprintf("\nGroup: %s = %s\n", x$groups[1], group_val))

      for (var_name in unique(group_results$Variable)) {
        var_data <- group_results[group_results$Variable == var_name, ]
        cat(sprintf("\n--- %s ---\n", var_name))
        print(.rename_stat(.make_output_df(var_name, var_data)), row.names = FALSE)
      }
    }
  } else {
    variables <- if ("Variable" %in% names(x$results)) {
      unique(x$results$Variable)
    } else {
      x$variables
    }

    for (var_name in variables) {
      cat(sprintf("\n--- %s ---\n", var_name))

      if ("Variable" %in% names(x$results)) {
        var_data <- x$results[x$results$Variable == var_name, ]
      } else {
        var_data <- x$results
      }

      print(.rename_stat(.make_output_df(var_name, var_data)), row.names = FALSE)
    }
  }

  cat("\n")
  invisible(x)
}
