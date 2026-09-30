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
#' @param na.rm Remove missing values before calculating? (Default: TRUE).
#'   With \code{FALSE}, missing values of either variable form their own
#'   \code{"NA"} row or column.
#' @param digits Decimal places for percentages (Default: 1)
#'
#' @return A cross-tabulation table showing the relationship between two
#'   variables. The result also carries the expected cell counts
#'   (\code{$expected}) and adjusted standardized residuals
#'   (\code{$adj_residuals}); display the residuals with
#'   \code{summary(result, residuals = TRUE)}.
#'
#' @details
#' ## Understanding the Results
#'
#' The crosstab table shows:
#' - **Cell counts**: Number of people in each combination
#' - **Row %**: Distribution within each row (e.g., "Among those with high school education, X% live in the East")
#' - **Column %**: Distribution within each column (e.g., "Among those in the East, X% have high school education")
#' - **Total %**: Percentage of the entire sample (e.g., "X% of all respondents have high school education AND live in the East")
#' - **Adjusted residuals** (via \code{summary(result, residuals = TRUE)},
#'   SPSS \code{/CELLS=ASRESID}): which cells deviate from independence.
#'   After a significant \code{\link{chi_square}} test, cells with
#'   |adj. residual| > 2 are the ones driving the association.
#'   For weighted tables the residuals are computed on the rounded
#'   weighted cell counts (see below); an SPSS v29 reference run for the
#'   residuals is pending, so they are currently verified against the
#'   Haberman formula (\code{chisq.test()$stdres}) rather than SPSS output.
#'
#' ## Weighted Tables
#'
#' As SPSS CROSSTABS does by default (\code{/COUNT ROUND CELL}), each
#' weighted cell count is rounded to a whole number first; the row and
#' column totals are sums of the rounded cells and all percentages come
#' from the rounded counts. The table total can therefore differ by a few
#' cases from the rounded sum of weights shown as "N (valid)" (SPSS: 2518
#' vs. 2516 for education x employment in \code{survey_data}).
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
  .reject_partial_args()
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
  .check_crosstab_vars(row_quo, col_quo)

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
  # Variable labels for the title and spanner (as SPSS shows them)
  var_label_of <- function(v) {
    lb <- attr(v, "label", exact = TRUE)
    if (is.null(lb) || !nzchar(lb[1])) NULL else as.character(lb[1])
  }
  row_label <- var_label_of(data[[row_var]])
  col_label <- var_label_of(data[[col_var]])

  # Extract variables
  row_data <- data[[row_var]]
  col_data <- data[[col_var]]

  # Handle weights through the package-wide policy (.process_weights():
  # bare numbers, negative weights are an error). crosstab() used to
  # validate on its own - only warning on negative weights - and compared
  # the raw column, which fails for SPSS weights containing NA.
  weights_info <- .process_weights(data, weights_quo)
  data <- weights_info$data
  weights_var <- weights_info$name
  weights_vec <- weights_info$vector
  is_weighted <- !is.null(weights_var)

  # Handle missing values. na.rm = FALSE keeps cases with a missing value
  # as their own "NA" category (it used to have no visible effect: table()
  # dropped them anyway while the header still counted them as missing).
  # Cases without a weight are never tabulated.
  has_weight <- if (is_weighted) !is.na(weights_vec) else rep(TRUE, length(row_data))
  if (na.rm) {
    valid_cases <- !is.na(row_data) & !is.na(col_data) & has_weight
  } else {
    valid_cases <- has_weight
    row_data <- .na_as_category(row_data)
    col_data <- .na_as_category(col_data)
  }
  # Missing cases as SPSS's Case Processing Summary counts them: with
  # weights the sum of the weights of the excluded cases (it was an
  # unweighted count next to a weighted N)
  n_missing <- if (is_weighted) {
    sum(weights_vec[!valid_cases & has_weight])
  } else {
    sum(!valid_cases)
  }

  row_data <- row_data[valid_cases]
  col_data <- col_data[valid_cases]
  if (is_weighted) {
    weights_vec <- weights_vec[valid_cases]
  }

  # Create contingency table
  if (is_weighted) {
    # SPSS CROSSTABS default /COUNT ROUND CELL: every weighted cell count
    # is rounded first; margins are sums of the rounded cells and all
    # percentages, expected counts and residuals come from the rounded
    # table (as chi_square() does). Keeping the unrounded cells showed
    # e.g. 402 + 447 with a margin of 848 and percentages off by up to
    # 0.1 point from SPSS.
    tab <- round(xtabs(weights_vec ~ row_data + col_data))
  } else {
    # Create unweighted table
    tab <- table(row_data, col_data)
  }

  # Convert to matrix for easier calculations
  tab_matrix <- as.matrix(tab)

  # Like SPSS CROSSTABS, show observed categories only: table()/xtabs()
  # keep unused factor levels, which printed "0 0 0" rows with a row
  # percentage of 100% (0 of 0) in the Total column
  tab_matrix <- tab_matrix[rowSums(tab_matrix) > 0, colSums(tab_matrix) > 0,
                           drop = FALSE]

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

  # Expected counts and adjusted standardized residuals
  # (SPSS CROSSTABS /CELLS=EXPECTED ASRESID; Haberman 1973). Weighted
  # tables use the rounded cell counts (/COUNT ROUND CELL above); with
  # weights == 1 this reduces exactly to the unweighted result. Cells in
  # empty rows/columns yield NA.
  expected <- outer(row_totals, col_totals) / grand_total
  adj_correction <- outer(1 - row_totals / grand_total,
                          1 - col_totals / grand_total)
  adj_residuals <- (tab_matrix - expected) / sqrt(expected * adj_correction)
  adj_residuals[!is.finite(adj_residuals)] <- NA_real_

  # Create results object
  result <- list(
    table = tab_matrix,
    row_totals = row_totals,
    col_totals = col_totals,
    total = grand_total,
    row_pct = row_pct,
    col_pct = col_pct,
    total_pct = total_pct,
    expected = expected,
    adj_residuals = adj_residuals,
    row_var = row_var,
    col_var = col_var,
    row_label = row_label,
    col_label = col_label,
    row_levels = rownames(tab_matrix),
    col_levels = colnames(tab_matrix),
    row_label_map = row_label_map,
    col_label_map = col_label_map,
    weights_var = weights_var,
    percentages = percentages,
    # Valid cases as SPSS's Case Processing Summary reports them: with
    # weights the (unrounded) sum of weights, which can differ from the
    # total of the rounded cells
    n_valid = if (is_weighted) sum(weights_vec) else grand_total,
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
  .check_crosstab_vars(row_quo, col_quo)

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
#'
#' @description
#' Prints the full cross-tabulation table (cell counts and percentages).
#' The output of \code{crosstab()} is a contingency table by nature, so
#' \code{print()} and \code{summary()} display the same table;
#' \code{summary()} additionally offers section toggles (including cell
#' \code{residuals}) and a \code{digits} option.
#'
#' No significance test is included; use \code{\link{chi_square}} for a
#' test of independence.
#'
#' @param x A crosstab result object
#' @param digits Number of decimal places for percentages (default: the
#'   \code{digits} given to \code{\link{crosstab}}, i.e. 1 unless set)
#' @param ... Additional arguments (currently unused)
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- crosstab(survey_data, gender, region)
#' result              # full cross-tabulation table
#' summary(result)     # same table, with section toggles
#'
#' @export
#' @method print crosstab
print.crosstab <- function(x, digits = x$digits %||% 1, ...) {
  print(summary(x, digits = digits))
  invisible(x)
}

#' Summary method for crosstab results
#'
#' @description
#' Creates a summary object that produces detailed output when printed,
#' including the full cross-tabulation table with cell counts, marginal
#' totals, and the requested percentage breakdowns.
#'
#' @param object A \code{crosstab} result object.
#' @param crosstab_table Logical. Show the cross-tabulation table?
#'   (Default: TRUE)
#' @param percentages Logical. Show the percentage sub-rows inside the
#'   table (as requested via the \code{percentages} argument of
#'   \code{\link{crosstab}})? (Default: TRUE)
#' @param residuals Logical. Show the adjusted standardized residual as a
#'   sub-row in each cell (SPSS \code{CROSSTABS /CELLS=ASRESID})? After a
#'   significant \code{\link{chi_square}} test, cells with an absolute
#'   adjusted residual above roughly 2 are the ones deviating from
#'   independence. (Default: FALSE, matching SPSS's opt-in cell display)
#' @param digits Number of decimal places for percentages (Default: the
#'   \code{digits} given to \code{\link{crosstab}}, i.e. 1 unless set).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.crosstab} object.
#'
#' @examples
#' result <- crosstab(survey_data, gender, region)
#' summary(result)
#' summary(result, percentages = FALSE)
#' summary(result, residuals = TRUE)   # which cells drive the association?
#'
#' @seealso \code{\link{crosstab}} for the main analysis function.
#' @export
#' @method summary crosstab
summary.crosstab <- function(object, crosstab_table = TRUE,
                             percentages = TRUE, residuals = FALSE,
                             digits = object$digits %||% 1, ...) {
  build_summary_object(
    object     = object,
    show       = list(crosstab_table = crosstab_table,
                      percentages = percentages,
                      residuals = residuals),
    digits     = digits,
    class_name = "summary.crosstab"
  )
}

#' Print summary of crosstab results (detailed output)
#'
#' @description
#' Displays the full cross-tabulation table for a \code{crosstab} result,
#' with sections controlled by the boolean parameters passed to
#' \code{\link{summary.crosstab}}. For grouped analyses, a separate table
#' is displayed for each group combination.
#'
#' @param x A \code{summary.crosstab} object created by
#'   \code{\link{summary.crosstab}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- crosstab(survey_data, gender, region)
#' summary(result)                       # full table
#' summary(result, percentages = FALSE) # counts only
#'
#' @seealso \code{\link{crosstab}} for the main analysis,
#'   \code{\link{summary.crosstab}} for summary options.
#' @export
#' @method print summary.crosstab
print.summary.crosstab <- function(x, ...) {
  digits <- x$digits

  show_table       <- isTRUE(x$show$crosstab_table)
  show_percentages <- isTRUE(x$show$percentages)
  show_residuals   <- isTRUE(x$show$residuals)

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
      .print_single_crosstab(result, digits = digits,
                             show_table = show_table,
                             show_percentages = show_percentages,
                             show_residuals = show_residuals)
      cat("\n")
    }
  } else {
    # Print single crosstab
    .print_single_crosstab(x, digits = digits,
                           show_table = show_table,
                           show_percentages = show_percentages,
                           show_residuals = show_residuals)
  }

  invisible(x)
}

# Helper function to print a single crosstab
# show_table / show_percentages / show_residuals gate the table body, the
# percentage sub-rows, and the adjusted-residual sub-rows (used by
# print.summary.crosstab)
.print_single_crosstab <- function(x, digits = 1, show_table = TRUE,
                                   show_percentages = TRUE,
                                   show_residuals = FALSE,
                                   width = getOption("width", 80)) {
  show_residuals <- show_residuals && !is.null(x$adj_residuals)

  # Variable labels as SPSS shows them (names when a variable has none)
  row_lab <- x$row_label %||% x$row_var
  col_lab <- x$col_label %||% x$col_var

  # Header
  title <- paste0("Crosstabulation: ", row_lab, " x ", col_lab)
  cat("\n", title, "\n", sep = "")
  cat(strrep("-", nchar(title, type = "width")), "\n", sep = "")

  # Info section ("Counts only" when summary(percentages = FALSE) hides
  # the percentage sub-rows)
  pct_label <- if (!show_percentages) "Counts only" else switch(x$percentages,
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
    "Missing" = if (round(x$n_missing) > 0) {
      sprintf(if (x$is_weighted) "%.0f (weighted)" else "%.0f", x$n_missing)
    }
  )
  print_info_section(test_info)
  cat("\n")

  if (!show_table) return(invisible(NULL))

  # Full category labels (they used to be cut at 20 characters, so eight
  # ALLBUS ISCO rows all read "FUEHRUNGSKRAEFTE,...")
  display_row_levels <- .apply_labels(x$row_levels, x$row_label_map, max_width = Inf)
  display_col_levels <- .apply_labels(x$col_levels, x$col_label_map, max_width = Inf)

  n_rows <- length(x$row_levels)
  n_cols <- length(x$col_levels)
  fmt_pct <- function(value) sprintf(paste0("%.", digits, "f%%"), value)
  cnt <- function(value) sprintf("%.0f", value)

  # --- Table body as data: one block per row category + the Total block --
  blocks <- list()
  for (i in seq_len(n_rows)) {
    lines <- list(list(label = display_row_levels[i], main = TRUE,
                       cells = c(cnt(x$table[i, ]), cnt(x$row_totals[i]))))
    if (show_percentages && !is.null(x$row_pct)) {
      lines[[length(lines) + 1]] <- list(label = "  row %", main = FALSE,
                                         cells = c(fmt_pct(x$row_pct[i, ]), fmt_pct(100)))
    }
    if (show_percentages && !is.null(x$col_pct)) {
      lines[[length(lines) + 1]] <- list(label = "  col %", main = FALSE,
                                         cells = c(fmt_pct(x$col_pct[i, ]),
                                                   fmt_pct(x$row_totals[i] / x$total * 100)))
    }
    if (show_percentages && !is.null(x$total_pct)) {
      lines[[length(lines) + 1]] <- list(label = "  total %", main = FALSE,
                                         cells = c(fmt_pct(x$total_pct[i, ]),
                                                   fmt_pct(x$row_totals[i] / x$total * 100)))
    }
    # Adjusted standardized residual sub-row (SPSS /CELLS=ASRESID; SPSS
    # prints 1 decimal). No value in the Total column - residuals are
    # defined per cell, not for marginals.
    if (show_residuals) {
      res <- x$adj_residuals[i, ]
      lines[[length(lines) + 1]] <- list(label = "  adj.res.", main = FALSE,
                                         cells = c(ifelse(is.na(res), "", sprintf("%.1f", res)), ""))
    }
    blocks[[i]] <- lines
  }

  # Total row's percentage sub-rows: every requested type, as SPSS prints
  # them for its Total row - % within the row variable and % of total are
  # the column shares, % within the column variable is 100% per column
  col_share <- fmt_pct(x$col_totals / x$total * 100)
  total_lines <- list(list(label = "Total", main = TRUE,
                           cells = c(cnt(x$col_totals), cnt(x$total))))
  if (show_percentages && !is.null(x$row_pct)) {
    total_lines[[length(total_lines) + 1]] <- list(label = "  row %", main = FALSE,
                                                   cells = c(col_share, fmt_pct(100)))
  }
  if (show_percentages && !is.null(x$col_pct)) {
    total_lines[[length(total_lines) + 1]] <- list(label = "  col %", main = FALSE,
                                                   cells = rep(fmt_pct(100), n_cols + 1))
  }
  if (show_percentages && !is.null(x$total_pct)) {
    total_lines[[length(total_lines) + 1]] <- list(label = "  total %", main = FALSE,
                                                   cells = c(col_share, fmt_pct(100)))
  }

  # --- Column widths: each column sized to its own content ---------------
  dw <- function(s) nchar(s, type = "width")
  all_lines <- c(unlist(blocks, recursive = FALSE), total_lines)
  n_cells <- n_cols + 1L
  cell_w <- vapply(seq_len(n_cells), function(j) {
    max(dw(vapply(all_lines, function(l) l$cells[j], character(1))), 1L)
  }, numeric(1))
  col_heads <- c(display_col_levels, "Total")
  head_w <- pmax(cell_w, dw(col_heads))

  sub_labels <- vapply(Filter(function(l) !l$main, all_lines), function(l) l$label, character(1))
  first_min <- max(dw(c(sub_labels, "Total")), 1L)
  first_w <- max(first_min, dw(display_row_levels), dw(row_lab))
  table_width <- function(fw, hw) 1L + (fw + 3L) + sum(hw + 3L)

  # Too wide for the console: first wrap the column headings (at spaces,
  # hyphens, commas, slashes), then the row labels
  if (table_width(first_w, head_w) > width) {
    word_w <- vapply(col_heads, function(h) max(dw(.ct_words(h))), numeric(1))
    head_w <- pmax(cell_w, pmin(head_w, word_w))
  }
  if (table_width(first_w, head_w) > width) {
    avail <- width - table_width(0L, head_w)
    first_w <- max(first_min, 12L, min(first_w, avail))
  }

  # --- Header: spanner (column variable label) + column headings ---------
  head_lines <- lapply(seq_len(n_cells), function(j) .ct_wrap(col_heads[j], head_w[j]))
  n_head <- max(lengths(head_lines))
  span_inner <- sum(head_w + 2L) + (n_cells - 1L)
  span_lines <- .ct_wrap(col_lab, span_inner - 2L)
  row_head <- .ct_wrap(row_lab, first_w)
  n_top <- max(length(span_lines), length(row_head) - n_head)
  first_lines <- c(rep("", n_top + n_head - length(row_head)), row_head)

  cell <- function(s, w, left = FALSE) {
    paste0(" ", pad_utf8(s, w, align = if (left) "left" else "right"), " ")
  }
  rule <- function(ch = "-") {
    cat("+", strrep(ch, first_w + 2L), "+",
        paste(strrep(ch, head_w + 2L), collapse = "+"), "+\n", sep = "")
  }
  emit <- function(label, cells) {
    cat("|", cell(label, first_w, left = TRUE), "|",
        paste(vapply(seq_len(n_cells), function(j) cell(cells[j], head_w[j]),
                     character(1)), collapse = "|"),
        "|\n", sep = "")
  }

  rule()
  for (k in seq_len(n_top)) {
    txt <- if (k <= length(span_lines)) span_lines[k] else ""
    left_pad <- max(0L, (span_inner - dw(txt)) %/% 2L)
    cat("|", cell(first_lines[k], first_w, left = TRUE), "|",
        pad_utf8(paste0(strrep(" ", left_pad), txt), span_inner), "|\n", sep = "")
  }
  for (k in seq_len(n_head)) {
    heads <- vapply(head_lines, function(h) {
      c(rep("", n_head - length(h)), h)[k]
    }, character(1))
    emit(first_lines[n_top + k], heads)
  }
  rule()

  # --- Body: long row labels wrap onto continuation lines ----------------
  emit_block <- function(lines) {
    for (l in lines) {
      lab <- if (l$main) .ct_wrap(l$label, first_w) else l$label
      emit(lab[1], l$cells)
      for (extra in lab[-1]) emit(extra, rep("", n_cells))
    }
  }
  for (i in seq_len(n_rows)) {
    emit_block(blocks[[i]])
    if (i < n_rows) rule()
  }
  rule("=")
  emit_block(total_lines)
  rule()

  if (show_residuals) {
    cat("adj.res. = adjusted standardized residual; |adj.res.| > 2 marks cells\n")
    cat("deviating from independence (use chi_square() for the overall test).\n")
  }
  invisible(NULL)
}

#' Break a label into pieces at spaces, hyphens, commas and slashes
#' @noRd
.ct_words <- function(text) {
  if (is.na(text)) return("NA")
  w <- regmatches(text, gregexpr("[^ ,/-]*[,/-]*", text))[[1]]
  w <- trimws(w)
  w[nzchar(w)]
}

#' Wrap a crosstab label to a display width
#'
#' Breaks after spaces, hyphens, commas and slashes ("VOLKS-," /
#' "HAUPTSCHULE"); a piece longer than the width is split hard.
#' @noRd
.ct_wrap <- function(text, width) {
  if (is.na(text)) text <- "NA"
  width <- max(1L, width)
  if (nchar(text, type = "width") <= width) return(text)
  pieces <- regmatches(text, gregexpr("[^ ,/-]*[ ,/-]*", text))[[1]]
  pieces <- pieces[nzchar(pieces)]
  lines <- character(0)
  cur <- ""
  for (p in pieces) {
    cand <- paste0(cur, p)
    if (!nzchar(cur) || nchar(trimws(cand, "right"), type = "width") <= width) {
      cur <- cand
    } else {
      lines <- c(lines, trimws(cur, "right"))
      cur <- p
    }
  }
  lines <- c(lines, trimws(cur, "right"))
  out <- character(0)
  for (ln in lines) {
    while (nchar(ln, type = "width") > width) {
      out <- c(out, substr(ln, 1, width))
      ln <- substr(ln, width + 1, nchar(ln))
    }
    out <- c(out, ln)
  }
  out
}


#' Both crosstab() variables must be given
#'
#' crosstab(data, gender) used to fail inside rlang::as_name() with the
#' base error 'argument "x" is missing, with no default'.
#' @noRd
.check_crosstab_vars <- function(row_quo, col_quo, call = rlang::caller_env()) {
  if (rlang::quo_is_missing(row_quo)) {
    cli_abort(c(
      "{.fn crosstab} needs two variables.",
      "x" = "{.arg row} and {.arg col} are missing."
    ), call = call)
  }
  if (rlang::quo_is_missing(col_quo)) {
    cli_abort(c(
      "{.fn crosstab} needs two variables.",
      "x" = "{.arg col} is missing.",
      "i" = "For a single variable use {.fn frequency}."
    ), call = call)
  }
  invisible(TRUE)
}

#' Missing values as an explicit "NA" category (crosstab(na.rm = FALSE))
#'
#' The categories keep the order table() would give them (factor levels,
#' sorted values; labelled vectors by their codes), with "NA" last.
#' @noRd
.na_as_category <- function(v) {
  f <- if (is.factor(v)) v else factor(if (inherits(v, "haven_labelled")) .plain_numeric(v) else v)
  if (!anyNA(f)) return(f)
  f <- addNA(f, ifany = TRUE)
  levels(f)[is.na(levels(f))] <- "NA"
  f
}
