#' Compare Two Categories: See How They Relate
#'
#' @description
#' \code{crosstab()} shows you how two categorical variables relate to each other.
#' It creates a table that reveals patterns - like whether education level differs
#' by region, or if gender influences product preferences.
#'
#' Think of it as a two-way frequency table that shows:
#' - How many people fall into each combination of categories
#' - What percentage each cell represents
#' - Whether there are patterns or associations
#'
#' @param data Your survey data (a data frame or tibble)
#' @param row The variable for table rows (e.g., education, age_group)
#' @param col The variable for table columns (e.g., region, gender)
#' @param weights Optional survey weights for population-representative results
#' @param percentages Which percentages to show:
#'   \itemize{
#'     \item \code{"row"} (default): Percentages across each row (adds to 100% horizontally)
#'     \item \code{"col"}: Percentages down each column (adds to 100% vertically)
#'     \item \code{"total"}: Percentage of the entire table
#'     \item \code{"all"}: Show all three types
#'     \item \code{"none"}: Just counts, no percentages
#'   }
#' @param na.rm Remove missing values before calculating? (Default: TRUE)
#' @param digits Decimal places for percentages (Default: 1)
#'
#' @return A cross-tabulation table showing the relationship between two variables
#'
#' @details
#' ## Understanding the Results
#'
#' The crosstab table shows:
#' - **Cell counts**: Number of people in each combination
#' - **Row %**: Distribution within each row (e.g., "Among those with high school education, X% live in the East")
#' - **Column %**: Distribution within each column (e.g., "Among those in the East, X% have high school education")
#' - **Total %**: Percentage of the entire sample (e.g., "X% of all respondents have high school education AND live in the East")
#'
#' ## When to Use This
#'
#' Use crosstab when you want to:
#' - See if two categorical variables are related
#' - Compare distributions across groups
#' - Find patterns in survey responses
#' - Create demographic breakdowns
#'
#' ## Choosing Percentages
#'
#' - **Row %**: Use when your row variable is the grouping factor
#'   (e.g., "How does region vary BY education level?")
#' - **Column %**: Use when your column variable is the grouping factor
#'   (e.g., "How does education vary BY region?")
#' - **Total %**: Use to understand the overall sample composition
#'
#' ## Tips for Success
#'
#' - Start with row or column percentages, not both at once
#' - Use chi-squared test to check if the relationship is statistically significant
#' - Watch for small cell counts (< 5) which may be unreliable
#' - Consider combining sparse categories if many cells are empty
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
#' @seealso
#' \code{\link[base]{table}} for base R contingency tables.
#'
#' \code{\link{frequency}} for single-variable frequency tables.
#'
#' \code{\link{chi_square}} for testing if the cross-tabulated variables
#' are related.
#'
#' @family descriptive
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
    cli_abort("Row variable {.var {row_var}} not found in data.")
  }
  if (!col_var %in% names(data)) {
    cli_abort("Column variable {.var {col_var}} not found in data.")
  }

  # Build label maps before any subsetting (preserves attributes)
  row_label_map <- .build_label_map(data[[row_var]])
  col_label_map <- .build_label_map(data[[col_var]])

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
      cli_abort("Weights variable {.var {weights_var}} not found in data.")
    }
    weights_vec <- data[[weights_var]]

    # Validate weights
    if (!is.numeric(weights_vec)) {
      cli_abort("Weights variable {.var {weights_var}} must be numeric.")
    }
    if (any(weights_vec < 0, na.rm = TRUE)) {
      cli_warn("Negative weights detected. Results may be invalid.")
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
    row_label_map = row_label_map,
    col_label_map = col_label_map,
    weights_var = weights_var,
    percentages = percentages,
    n_valid = grand_total,
    n_missing = n_missing,
    is_weighted = is_weighted,
    is_grouped = FALSE,
    digits = digits
  )

  class(result) <- "crosstab"
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

  class(combined_result) <- "crosstab"
  return(combined_result)
}

#' Print method for crosstab results
#' @param x A crosstab result object
#' @param digits Number of decimal places for percentages (default: 1)
#' @param ... Additional arguments (currently unused)
#' @export
print.crosstab <- function(x, digits = 1, ...) {

  if (x$is_grouped) {
    # Print grouped results using standardized header
    title <- get_standard_title("Grouped Crosstabulation", x$weights_var, "")
    print_header(title)
    cat("\n")

    for (i in seq_along(x$results)) {
      result <- x$results[[i]]

      # Print group header using standardized helper
      group_info <- result$group_info
      print_group_header(as.data.frame(group_info, stringsAsFactors = FALSE))

      # Print the crosstab for this group
      .print_single_crosstab(result, digits = digits)
      cat("\n")
    }
  } else {
    # Print single crosstab
    .print_single_crosstab(x, digits = digits)
  }

  invisible(x)
}

# Helper function to print a single crosstab
.print_single_crosstab <- function(x, digits = 1) {

  # Header
  title <- paste0("Crosstabulation: ", x$row_var, " \u00d7 ", x$col_var)
  cat("\n", title, "\n", sep = "")
  cat(paste(rep("-", nchar(title)), collapse = ""), "\n")

  # Info section
  pct_label <- switch(x$percentages,
    "row" = "Row percentages",
    "col" = "Column percentages",
    "total" = "Total percentages",
    "all" = "All percentages (row, col, total)",
    "none" = "Counts only"
  )
  n_label <- if (x$is_weighted) sprintf("%.0f (weighted)", x$n_valid) else sprintf("%.0f", x$n_valid)
  test_info <- list(
    "Row variable" = x$row_var,
    "Column variable" = x$col_var,
    "Percentages" = pct_label,
    "Weights variable" = x$weights_var,
    "N (valid)" = n_label,
    "Missing" = if (x$n_missing > 0) sprintf("%.0f", x$n_missing) else NULL
  )
  print_info_section(test_info)
  cat("\n")

  # Apply value labels if available
  display_row_levels <- .apply_labels(x$row_levels, x$row_label_map)
  display_col_levels <- .apply_labels(x$col_levels, x$col_label_map)

  # Get dimensions
  n_rows <- length(x$row_levels)
  n_cols <- length(x$col_levels)

  # Determine percentage sub-labels for width calculation
  pct_sub_labels <- c()
  if (!is.null(x$row_pct))   pct_sub_labels <- c(pct_sub_labels, "  row %")
  if (!is.null(x$col_pct))   pct_sub_labels <- c(pct_sub_labels, "  col %")
  if (!is.null(x$total_pct)) pct_sub_labels <- c(pct_sub_labels, "  total %")

  # Row label column width (interior between | delimiters)
  all_first_col <- c(display_row_levels, "Total", x$row_var, pct_sub_labels)
  row_label_width <- max(12, max(nchar(all_first_col)) + 2)

  # Data column width (interior between | delimiters)
  max_count <- max(c(x$table, x$row_totals, x$col_totals, x$total))
  count_chars <- nchar(sprintf("%.0f", max_count))
  pct_chars <- digits + 5  # "100.0%" = 6 chars at digits=1
  header_chars <- max(nchar(c(display_col_levels, "Total")))
  data_col_width <- max(8, max(count_chars, pct_chars, header_chars) + 2)

  # --- Local helper closures ---

  # Horizontal line: +------------+--------+--------+
  ct_line <- function(char = "-") {
    parts <- c(paste(rep(char, row_label_width), collapse = ""))
    for (k in seq_len(n_cols + 1)) {
      parts <- c(parts, paste(rep(char, data_col_width), collapse = ""))
    }
    cat("+", paste(parts, collapse = "+"), "+\n", sep = "")
  }

  # Spanning header row: |            |       region             |
  ct_spanning_row <- function(label, spanning_text) {
    label_cell <- pad_utf8(paste0(" ", label), row_label_width)
    span_width <- (n_cols + 1) * data_col_width + n_cols
    text_len <- nchar(spanning_text, type = "chars")
    left_pad <- max(1, (span_width - text_len) %/% 2)
    centered <- paste0(paste(rep(" ", left_pad), collapse = ""), spanning_text)
    span_cell <- pad_utf8(centered, span_width)
    cat("|", label_cell, "|", span_cell, "|\n", sep = "")
  }

  # Data row: | Label      |    800 |    300 |   1100 |
  ct_row <- function(label, values) {
    label_cell <- pad_utf8(paste0(" ", label), row_label_width)
    value_cells <- vapply(values, function(v) {
      pad_utf8(paste0(v, " "), data_col_width, "right")
    }, character(1))
    cat("|", label_cell, "|", paste(value_cells, collapse = "|"), "|\n", sep = "")
  }

  # Format percentage value
  fmt_pct <- function(value) sprintf(paste0("%.", digits, "f%%"), value)

  # --- Table header ---
  ct_line()
  ct_spanning_row("", x$col_var)
  ct_row(x$row_var, c(display_col_levels, "Total"))
  ct_line()

  # --- Data rows ---
  for (i in seq_len(n_rows)) {
    # Count row
    count_vals <- c(
      vapply(seq_len(n_cols), function(j) sprintf("%.0f", x$table[i, j]), character(1)),
      sprintf("%.0f", x$row_totals[i])
    )
    ct_row(display_row_levels[i], count_vals)

    # Row percentage sub-row
    if (!is.null(x$row_pct)) {
      pct_vals <- c(
        vapply(seq_len(n_cols), function(j) fmt_pct(x$row_pct[i, j]), character(1)),
        fmt_pct(100)
      )
      ct_row("  row %", pct_vals)
    }

    # Column percentage sub-row
    if (!is.null(x$col_pct)) {
      row_of_total <- (x$row_totals[i] / x$total) * 100
      pct_vals <- c(
        vapply(seq_len(n_cols), function(j) fmt_pct(x$col_pct[i, j]), character(1)),
        fmt_pct(row_of_total)
      )
      ct_row("  col %", pct_vals)
    }

    # Total percentage sub-row
    if (!is.null(x$total_pct)) {
      row_total_pct <- (x$row_totals[i] / x$total) * 100
      pct_vals <- c(
        vapply(seq_len(n_cols), function(j) fmt_pct(x$total_pct[i, j]), character(1)),
        fmt_pct(row_total_pct)
      )
      ct_row("  total %", pct_vals)
    }

    # Separator between row groups (= before Total)
    if (i < n_rows) ct_line()
  }

  # --- Total row ---
  ct_line("=")
  total_vals <- c(
    vapply(seq_len(n_cols), function(j) sprintf("%.0f", x$col_totals[j]), character(1)),
    sprintf("%.0f", x$total)
  )
  ct_row("Total", total_vals)

  # Total row's column percentage sub-row
  if (!is.null(x$col_pct)) {
    pct_vals <- c(
      vapply(seq_len(n_cols), function(j) {
        fmt_pct((x$col_totals[j] / x$total) * 100)
      }, character(1)),
      fmt_pct(100)
    )
    ct_row("  col %", pct_vals)
  }

  ct_line()
}