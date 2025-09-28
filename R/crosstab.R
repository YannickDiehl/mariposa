#' Cross-tabulation Table
#'
#' @description
#' Creates cross-tabulation frequency tables with optional percentages for
#' categorical variables. Supports survey weights and grouped data operations.
#'
#' @param data A data frame containing the variables
#' @param row The row variable (unquoted name)
#' @param col The column variable (unquoted name)
#' @param weights Optional weights variable (unquoted name)
#' @param percentages Character vector specifying which percentages to calculate.
#'   Options are "none", "row", "col", "total", or "all" (default: "row")
#' @param na.rm Logical; if TRUE, remove missing values (default: TRUE)
#' @param digits Number of decimal places for percentages (default: 1)
#'
#' @return An object of class "crosstab_results" containing:
#' - Frequency table with counts
#' - Row/column/total percentages (if requested)
#' - Marginal totals
#' - Metadata about the analysis
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Basic crosstab
#' survey_data %>% crosstab(gender, region)
#'
#' # With weights and all percentages
#' survey_data %>% crosstab(gender, education,
#'                          weights = sampling_weight,
#'                          percentages = "all")
#'
#' # Grouped analysis
#' survey_data %>%
#'   group_by(employment) %>%
#'   crosstab(gender, region, weights = sampling_weight)
#'
#' # Column percentages only
#' survey_data %>% crosstab(education, employment, percentages = "col")
#'
#' @export
crosstab <- function(data, row, col,
                    weights = NULL,
                    percentages = c("row", "none", "col", "total", "all"),
                    na.rm = TRUE,
                    digits = 1) {
  UseMethod("crosstab")
}

#' @export
crosstab.data.frame <- function(data, row, col,
                                weights = NULL,
                                percentages = c("row", "none", "col", "total", "all"),
                                na.rm = TRUE,
                                digits = 1) {

  # Match percentages argument
  percentages <- match.arg(percentages)

  # Capture variable names
  row_quo <- rlang::enquo(row)
  col_quo <- rlang::enquo(col)
  weights_quo <- rlang::enquo(weights)

  # Get variable names as strings
  row_var <- rlang::as_name(row_quo)
  col_var <- rlang::as_name(col_quo)

  # Check if variables exist in data
  if (!row_var %in% names(data)) {
    stop(paste("Row variable '", row_var, "' not found in data", sep = ""))
  }
  if (!col_var %in% names(data)) {
    stop(paste("Column variable '", col_var, "' not found in data", sep = ""))
  }

  # Extract variables
  row_data <- data[[row_var]]
  col_data <- data[[col_var]]

  # Handle weights
  is_weighted <- !rlang::quo_is_null(weights_quo)
  weights_var <- NULL
  weights_vec <- NULL

  if (is_weighted) {
    weights_var <- rlang::as_name(weights_quo)
    if (!weights_var %in% names(data)) {
      stop(paste("Weights variable '", weights_var, "' not found in data", sep = ""))
    }
    weights_vec <- data[[weights_var]]

    # Validate weights
    if (!is.numeric(weights_vec)) {
      stop("Weights must be numeric")
    }
    if (any(weights_vec < 0, na.rm = TRUE)) {
      warning("Negative weights detected. Results may be invalid.")
    }
  }

  # Handle missing values
  if (na.rm) {
    valid_cases <- !is.na(row_data) & !is.na(col_data)
    if (is_weighted) {
      valid_cases <- valid_cases & !is.na(weights_vec)
    }

    n_missing <- sum(!valid_cases)

    row_data <- row_data[valid_cases]
    col_data <- col_data[valid_cases]
    if (is_weighted) {
      weights_vec <- weights_vec[valid_cases]
    }
  } else {
    n_missing <- sum(is.na(row_data) | is.na(col_data))
  }

  # Create contingency table
  if (is_weighted) {
    # Use actual decimal weights (not rounded)
    # SPSS applies decimal weights and rounds results, not weights
    tab <- xtabs(weights_vec ~ row_data + col_data)
    # Note: Results can be rounded later for display if needed
  } else {
    # Create unweighted table
    tab <- table(row_data, col_data)
  }

  # Convert to matrix for easier calculations
  tab_matrix <- as.matrix(tab)

  # Calculate marginals
  row_totals <- rowSums(tab_matrix)
  col_totals <- colSums(tab_matrix)
  grand_total <- sum(tab_matrix)

  # Initialize percentage tables
  row_pct <- NULL
  col_pct <- NULL
  total_pct <- NULL

  # Calculate percentages based on request
  if (percentages %in% c("row", "all")) {
    row_pct <- sweep(tab_matrix, 1, row_totals, "/") * 100
    row_pct[is.nan(row_pct)] <- 0  # Handle division by zero
  }

  if (percentages %in% c("col", "all")) {
    col_pct <- sweep(tab_matrix, 2, col_totals, "/") * 100
    col_pct[is.nan(col_pct)] <- 0  # Handle division by zero
  }

  if (percentages %in% c("total", "all")) {
    total_pct <- (tab_matrix / grand_total) * 100
    total_pct[is.nan(total_pct)] <- 0  # Handle division by zero
  }

  # Create results object
  result <- list(
    table = tab_matrix,
    row_totals = row_totals,
    col_totals = col_totals,
    total = grand_total,
    row_pct = row_pct,
    col_pct = col_pct,
    total_pct = total_pct,
    row_var = row_var,
    col_var = col_var,
    row_levels = rownames(tab_matrix),
    col_levels = colnames(tab_matrix),
    weights_var = weights_var,
    percentages = percentages,
    n_valid = grand_total,
    n_missing = n_missing,
    is_weighted = is_weighted,
    is_grouped = FALSE,
    digits = digits
  )

  class(result) <- "crosstab_results"
  return(result)
}

#' @export
crosstab.grouped_df <- function(data, row, col,
                                weights = NULL,
                                percentages = c("row", "none", "col", "total", "all"),
                                na.rm = TRUE,
                                digits = 1) {

  # Match percentages argument
  percentages <- match.arg(percentages)

  # Get grouping variables
  group_vars <- dplyr::group_vars(data)

  # Capture variable names
  row_quo <- rlang::enquo(row)
  col_quo <- rlang::enquo(col)
  weights_quo <- rlang::enquo(weights)

  row_var <- rlang::as_name(row_quo)
  col_var <- rlang::as_name(col_quo)
  weights_var <- if (!rlang::quo_is_null(weights_quo)) rlang::as_name(weights_quo) else NULL

  # Split data by groups
  data_list <- dplyr::group_split(data)
  group_keys <- dplyr::group_keys(data)

  # Apply crosstab to each group
  results_list <- lapply(seq_along(data_list), function(i) {
    group_data <- data_list[[i]]

    # Run crosstab for this group
    if (!is.null(weights_var)) {
      result <- crosstab.data.frame(group_data,
                                    !!row_quo, !!col_quo,
                                    weights = !!weights_quo,
                                    percentages = percentages,
                                    na.rm = na.rm,
                                    digits = digits)
    } else {
      result <- crosstab.data.frame(group_data,
                                    !!row_quo, !!col_quo,
                                    percentages = percentages,
                                    na.rm = na.rm,
                                    digits = digits)
    }

    # Add group information
    result$group_info <- group_keys[i, , drop = FALSE]
    result
  })

  # Combine results
  combined_result <- list(
    results = results_list,
    row_var = row_var,
    col_var = col_var,
    weights_var = weights_var,
    percentages = percentages,
    is_weighted = !is.null(weights_var),
    is_grouped = TRUE,
    group_vars = group_vars,
    digits = digits
  )

  class(combined_result) <- "crosstab_results"
  return(combined_result)
}

#' Print method for crosstab results
#' @export
print.crosstab_results <- function(x, ...) {

  if (x$is_grouped) {
    # Print grouped results
    cat("\nGrouped Crosstabulation\n")
    cat("=======================\n\n")

    for (i in seq_along(x$results)) {
      result <- x$results[[i]]

      # Print group header
      cat("Group: ")
      group_info <- result$group_info
      for (j in seq_along(group_info)) {
        if (j > 1) cat(", ")
        cat(names(group_info)[j], " = ", as.character(group_info[[j]]), sep = "")
      }
      cat("\n")
      cat(paste(rep("-", 50), collapse = ""), "\n")

      # Print the crosstab for this group
      .print_single_crosstab(result)
      cat("\n")
    }
  } else {
    # Print single crosstab
    .print_single_crosstab(x)
  }

  invisible(x)
}

# Helper function to print a single crosstab
.print_single_crosstab <- function(x) {

  # Header
  cat("\nCrosstabulation: ", x$row_var, " × ", x$col_var, "\n", sep = "")
  cat(paste(rep("─", 50), collapse = ""), "\n\n")

  # Get dimensions
  n_rows <- length(x$row_levels)
  n_cols <- length(x$col_levels)

  # Prepare the display table
  display_table <- x$table

  # Add row totals
  display_table <- cbind(display_table, Total = x$row_totals)

  # Add column totals row
  col_totals_row <- c(x$col_totals, x$total)
  display_table <- rbind(display_table, Total = col_totals_row)

  # Format the table for printing
  # First, print column headers
  col_width <- max(10, max(nchar(x$col_levels)) + 2)
  row_label_width <- max(12, max(nchar(x$row_levels)) + 2)

  # Print column variable name
  cat(sprintf("%-*s", row_label_width, ""))
  cat(x$col_var, "\n")

  # Print column headers
  cat(x$row_var, "\n")
  cat(sprintf("%-*s", row_label_width, ""))
  for (col in x$col_levels) {
    cat(sprintf("%*s", col_width, col))
  }
  cat(sprintf("%*s", col_width, "Total"))
  cat("\n")

  # Print separator
  total_width <- row_label_width + (n_cols + 1) * col_width
  cat(paste(rep("─", total_width), collapse = ""), "\n")

  # Print each row with counts
  for (i in 1:n_rows) {
    # Row label
    cat(sprintf("%-*s", row_label_width, x$row_levels[i]))

    # Counts
    for (j in 1:n_cols) {
      cat(sprintf("%*d", col_width, display_table[i, j]))
    }
    # Row total
    cat(sprintf("%*d", col_width, display_table[i, n_cols + 1]))
    cat("\n")

    # Print percentages if requested
    if (x$percentages != "none") {
      # Row percentages
      if (!is.null(x$row_pct)) {
        cat(sprintf("%-*s", row_label_width, ""))
        for (j in 1:n_cols) {
          pct_val <- sprintf("%.1f%%", x$row_pct[i, j])
          cat(sprintf("%*s", col_width, pct_val))
        }
        cat(sprintf("%*s", col_width, "100.0%"))
        cat("  (row %)\n")
      }

      # Column percentages
      if (!is.null(x$col_pct)) {
        cat(sprintf("%-*s", row_label_width, ""))
        for (j in 1:n_cols) {
          pct_val <- sprintf("%.1f%%", x$col_pct[i, j])
          cat(sprintf("%*s", col_width, pct_val))
        }
        # Row's percentage of total
        row_of_total <- (x$row_totals[i] / x$total) * 100
        cat(sprintf("%*.1f%%", col_width, row_of_total))
        cat("  (col %)\n")
      }

      # Total percentages
      if (!is.null(x$total_pct)) {
        cat(sprintf("%-*s", row_label_width, ""))
        for (j in 1:n_cols) {
          pct_val <- sprintf("%.1f%%", x$total_pct[i, j])
          cat(sprintf("%*s", col_width, pct_val))
        }
        # Row total percentage
        row_total_pct <- (x$row_totals[i] / x$total) * 100
        cat(sprintf("%*.1f%%", col_width, row_total_pct))
        cat("  (total %)\n")
      }

      if (i < n_rows) cat("\n")
    }
  }

  # Print total row separator
  cat(paste(rep("─", total_width), collapse = ""), "\n")

  # Print totals row
  cat(sprintf("%-*s", row_label_width, "Total"))
  for (j in 1:n_cols) {
    cat(sprintf("%*d", col_width, x$col_totals[j]))
  }
  cat(sprintf("%*d", col_width, x$total))
  cat("\n")

  # Print column percentages for total row
  if (!is.null(x$col_pct)) {
    cat(sprintf("%-*s", row_label_width, ""))
    for (j in 1:n_cols) {
      col_of_total <- (x$col_totals[j] / x$total) * 100
      cat(sprintf("%*.1f%%", col_width, col_of_total))
    }
    cat(sprintf("%*s", col_width, "100.0%"))
    cat("\n")
  }

  # Footer
  cat("\n")
  if (x$is_weighted) {
    cat(sprintf("N = %d (Weighted)\n", x$n_valid))
  } else {
    cat(sprintf("N = %d\n", x$n_valid))
  }

  if (x$n_missing > 0) {
    cat(sprintf("Missing cases: %d\n", x$n_missing))
  }
}