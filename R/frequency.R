#' Count How Many People Chose Each Option
#'
#' @description
#' \code{frequency()} helps you understand categorical data by showing how many people
#' chose each option. It's perfect for survey questions with fixed choices like
#' education level, yes/no questions, or rating scales.
#'
#' Think of it as creating a summary table that shows:
#' - How many people chose each option
#' - What percentage that represents
#' - Running totals to see cumulative patterns
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The categorical variables you want to analyze. You can list multiple
#'   variables separated by commas, or use helpers like \code{starts_with("trust")}
#' @param weights Optional survey weights for population-representative results.
#'   Without weights, you get sample frequencies. With weights, you get
#'   population estimates.
#' @param sort_frq How to order the results:
#'   \itemize{
#'     \item \code{"none"} (default): Keep original order
#'     \item \code{"asc"}: Sort from lowest to highest frequency
#'     \item \code{"desc"}: Sort from highest to lowest frequency
#'   }
#' @param show_na Include missing values in the table? (Default: TRUE)
#' @param show_prc Show raw percentages including missing values? (Default: TRUE)
#' @param show_valid Show percentages excluding missing values? (Default: TRUE).
#'   The cumulative percentages are cumulative valid percentages, so
#'   \code{show_valid = FALSE} hides them too.
#' @param show_sum Show the cumulative (valid) percentages? (Default: TRUE)
#' @param show_labels Show category labels if available? (Default: "auto" - shows
#'   labels when they exist)
#' @param show_unused Show all defined value labels, even those with zero
#'   observations? (Default: FALSE). When TRUE, values that have labels defined
#'   (e.g., from statistical software files) but no cases in the data are
#'   included with frequency 0. This is useful for labelled datasets where
#'   unused categories should still appear in the output. The same applies
#'   to empty factor levels, which are hidden by default (as SPSS lists only
#'   observed values). Automatically enables label display.
#' @param sort.frq,show.na,show.prc,show.valid,show.sum,show.labels,show.unused
#'   Defunct dot-case argument names, removed in mariposa 0.6.9. Calling
#'   the function with any of them is an error; use the snake_case
#'   equivalents instead. (The formals are retained only so that the old
#'   names error clearly instead of being swallowed by `...`.)
#'
#' @return A frequency table showing counts and percentages for each category
#'
#' @details
#' ## Understanding the Results
#'
#' The frequency table follows the SPSS FREQUENCIES layout:
#' - **N**: Number of responses in each category (weighted: sum of weights,
#'   displayed rounded)
#' - **Raw %**: Percentage including missing values (use for "response rate")
#' - **Valid %**: Percentage excluding missing values (use for "among those who answered")
#' - **Cum. %**: Running total of the valid percentages (helps identify cutoff points)
#' - **Total valid**, the missing categories, **Total missing** (with two
#'   or more missing categories) and the grand **Total**; without missing
#'   values a single **Total** row ends the table.
#'
#' Factors, character and logical variables show their categories in the
#' Value column; labelled numeric variables show the code and its label.
#' Long labels are never cut; they wrap when the table would be wider than
#' the console.
#'
#' ## When to Use This
#'
#' Use \code{frequency()} when you have:
#' - Categorical variables (gender, region, education level)
#' - Yes/No questions
#' - Rating scales (satisfied/neutral/dissatisfied)
#' - Any question with a fixed set of options
#'
#' ## Weights Make a Difference
#'
#' Without weights, you're describing your sample. With weights, you're estimating
#' population values. Always use weights for population inference.
#'
#' ## Tagged Missing Values
#'
#' When data is imported with tagged NAs (e.g., via [read_spss()] with
#' `tag_na = TRUE`, or [read_stata()], [read_sas()], [read_xpt()] with the
#' `tag_na` parameter), `frequency()` automatically expands the missing value
#' section to show each missing type individually (with its original missing
#' value code and label), plus summary rows for **Total Valid** and **Total
#' Missing**.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#' 
#' # Basic categorical analysis
#' survey_data %>% frequency(gender)
#' 
#' # Multiple variables with weights
#' survey_data %>% frequency(gender, region, weights = sampling_weight)
#' 
#' # Grouped analysis by region
#' survey_data %>% 
#'   group_by(region) %>% 
#'   frequency(gender, weights = sampling_weight)
#' 
#' # Education levels with sorting
#' survey_data %>% frequency(education, sort_frq = "desc")
#' 
#' # Employment status with custom display options
#' survey_data %>% frequency(employment, weights = sampling_weight, 
#'                          show_na = TRUE, show_sum = TRUE)
#'
#' @seealso
#' \code{\link[base]{table}} for base R frequency tables.
#'
#' \code{\link{crosstab}} for cross-tabulation of two variables.
#'
#' \code{\link{chi_square}} for testing relationships between categories.
#'
#' \code{\link{describe}} for numeric variable summaries.
#'
#' @family descriptive
#' @export
frequency <- function(data, ..., weights = NULL, sort_frq = "none",
                     show_na = TRUE, show_prc = TRUE, show_valid = TRUE, show_sum = TRUE, show_labels = "auto", show_unused = FALSE,
                     sort.frq = NULL, show.na = NULL, show.prc = NULL,
                     show.valid = NULL, show.sum = NULL, show.labels = NULL,
                     show.unused = NULL) {
  .check_required("data")

  if (!is.data.frame(data)) cli_abort("{.arg data} must be a data frame.")

  # ---- Removed dot-case arguments: hard error (see VERSIONING_POLICY.md, 4).
  # The formals stay as NULL sentinels because `...` is consumed by
  # tidyselect: without them, an old dot-case name would silently be
  # misinterpreted as a variable selection.
  if (!is.null(sort.frq)) .stop_removed_arg("sort.frq", "sort_frq")
  if (!is.null(show.na)) .stop_removed_arg("show.na", "show_na")
  if (!is.null(show.prc)) .stop_removed_arg("show.prc", "show_prc")
  if (!is.null(show.valid)) .stop_removed_arg("show.valid", "show_valid")
  if (!is.null(show.sum)) .stop_removed_arg("show.sum", "show_sum")
  if (!is.null(show.labels)) .stop_removed_arg("show.labels", "show_labels")
  if (!is.null(show.unused)) .stop_removed_arg("show.unused", "show_unused")

  # Validate sort_frq: typos error instead of silently not sorting
  sort_frq <- match.arg(sort_frq, choices = c("none", "asc", "desc"))

  # Validate show_labels: only TRUE, FALSE, or "auto" are accepted
  if (!(isTRUE(show_labels) || isFALSE(show_labels) ||
        identical(show_labels, "auto"))) {
    cli_abort(
      "{.arg show_labels} must be {.val TRUE}, {.val FALSE}, or {.val auto}."
    )
  }

  # Check grouping and get variable names
  is_grouped <- inherits(data, "grouped_df")
  grp_vars <- if (is_grouped) dplyr::group_vars(data) else NULL

  # Select variables using centralized helper
  vars <- .process_variables(data, ...)
  var_names <- names(vars)

  # Process weights using centralized helper
  weights_info <- .process_weights(data, rlang::enquo(weights))
  data <- weights_info$data
  w_name <- weights_info$name
  
  # When show_unused is TRUE, force labels on (unused labels without label column make no sense)
  if (show_unused && show_labels == "auto") {
    show_labels <- TRUE
  }

  # Handle show_labels logic: auto-detect or use explicit user setting
  if (show_labels == "auto") {
    has_meaningful_labels <- any(vapply(var_names, function(var) {
      x <- data[[var]]
      
      # For factors, check if levels are different from their numeric representation
      if (is.factor(x)) {
        levels_x <- levels(x)
        # Check if factor levels are just numbers or provide meaningful labels
        numeric_levels <- suppressWarnings(as.numeric(levels_x))
        return(!all(!is.na(numeric_levels) & numeric_levels == seq_along(levels_x)))
      }
      
      # For variables with sjlabelled value labels
      if (!is.null(attr(x, "labels"))) {
        value_labels <- attr(x, "labels")

        # Labelled missing-value codes (tagged NAs such as ALLBUS -32
        # "NICHT GENERIERBAR") that occur in the data: their label is the
        # only explanation of the code, so show the Label column even for a
        # metric variable whose valid values carry no labels
        if (show_na && any(is.na(value_labels)) && anyNA(x)) {
          label_tags <- .na_tags(value_labels[is.na(value_labels)])
          data_tags <- .na_tags(x[is.na(x)])
          if (any(label_tags %in% data_tags[!is.na(data_tags)])) return(TRUE)
        }

        # Get actually occurring values in the data (excluding NA)
        actual_values <- unique(x[!is.na(x)])
        
        # Check if any of the actual values have meaningful labels
        # (i.e., labels that are different from the values themselves)
        actual_values_with_labels <- actual_values[actual_values %in% value_labels]
        
        if (length(actual_values_with_labels) > 0) {
          # Check if labels for actual values are different from the values
          actual_label_names <- names(value_labels)[value_labels %in% actual_values_with_labels]
          actual_label_values <- as.character(actual_values_with_labels)
          return(!all(actual_label_names == actual_label_values))
        } else {
          # No labels for actually occurring values
          return(FALSE)
        }
      }
      
      # Variables with only variable labels (attr "label") but no value labels
      # are not considered as having meaningful value labels for display
      # This is just a variable description, not value labels
      
      return(FALSE)
    }, logical(1)))
    
    # Set show_labels based on auto-detection
    show_labels <- has_meaningful_labels
  } else {
    # Use explicit user setting (TRUE or FALSE)
    show_labels <- as.logical(show_labels)
  }
  
  # Calculate frequencies
  if (is_grouped) {
    results <- calculate_grouped_frequencies(data, var_names, w_name, sort_frq, show_na, show_unused)
  } else {
    results <- calculate_ungrouped_frequencies(data, var_names, w_name, sort_frq, show_na, show_unused)
  }
  
  # Create S3 object
  structure(list(
    results = results$frequencies,
    stats = results$stats,
    variables = var_names,
    weights = w_name,
    groups = grp_vars,
    is_grouped = is_grouped,
    options = list(show_na = show_na, show_prc = show_prc, show_valid = show_valid, show_sum = show_sum, show_labels = show_labels, show_unused = show_unused),
    # "numeric" (incl. labelled) or "text" (factor/character/logical): text
    # categories print in the Value column without a duplicate Label column
    value_types = vapply(var_names, function(var) {
      if (is.numeric(data[[var]])) "numeric" else "text"
    }, character(1)),
    labels = vapply(var_names, function(var) {
      lbl <- attr(data[[var]], "label", exact = TRUE)
      if (is.null(lbl)) var else paste(as.character(lbl), collapse = " | ")
    }, character(1))
  ), class = "frequency")
}

# Helper function: Calculate frequency statistics for a single variable
calculate_single_frequency <- function(x, w = NULL, sort_frq = "none", show_na = TRUE, show_unused = FALSE) {
  # Check for tagged NAs (from read_spss, read_stata, read_sas, etc.)
  has_tagged_na <- !is.null(attr(x, "na_tag_map")) &&
    requireNamespace("haven", quietly = TRUE)

  if (is.null(w)) {
    # Unweighted frequencies
    freq_table <- table(x, useNA = if (show_na) "ifany" else "no")
    # table() keeps every factor level; like SPSS FREQUENCIES, empty
    # categories are listed only on request (show_unused = TRUE)
    if (is.factor(x) && !show_unused) {
      freq_table <- freq_table[freq_table > 0 | is.na(names(freq_table))]
    }
    total <- sum(freq_table)
    na_idx <- is.na(names(freq_table))
    valid_total <- sum(freq_table[!na_idx])

    # For unweighted: if show_na is TRUE, total includes missing
    # Raw % should always include missing in denominator
    total_all <- if (show_na) total else (total + sum(is.na(x)))

    prc <- as.numeric(freq_table) / total_all * 100
    # Valid % calculation - only for non-NA values
    valid_prc <- rep(NA, length(freq_table))
    if (any(!na_idx)) {
      valid_prc[!na_idx] <- as.numeric(freq_table[!na_idx]) / valid_total * 100
    }
    # Cumulative % based on valid %
    cum_prc <- rep(NA, length(freq_table))
    if (any(!na_idx)) {
      cum_prc[!na_idx] <- cumsum(valid_prc[!na_idx])
    }

    # Handle factors differently than numeric variables
    if (is.factor(x)) {
      value_col <- names(freq_table)
    } else {
      value_col <- suppressWarnings(as.numeric(names(freq_table)))
      # If conversion fails, keep as character
      if (all(is.na(value_col))) {
        value_col <- names(freq_table)
      }
    }

    result <- data.frame(
      value = value_col,
      label = get_value_labels(x, names(freq_table)),
      freq = as.numeric(freq_table),
      prc = prc,
      valid_prc = valid_prc,
      cum_freq = cumsum(as.numeric(freq_table)),
      cum_prc = cum_prc,
      stringsAsFactors = FALSE
    )
  } else {
    # Weighted frequencies
    valid_idx <- !is.na(x) & !is.na(w)
    if (!any(valid_idx) && !show_na) {
      return(data.frame(value = numeric(0), label = character(0), freq = numeric(0),
                       prc = numeric(0), valid_prc = numeric(0), cum_freq = numeric(0),
                       cum_prc = numeric(0), n_eff = numeric(0), stringsAsFactors = FALSE))
    }

    # Calculate frequencies for valid values. Values are bare numbers (as
    # in the unweighted branch): keeping the haven_labelled class made the
    # rbind() of several variables with different label sets fail in
    # vec_cast ("loss of precision"), depending on the variable order.
    if (any(valid_idx)) {
      x_valid <- x[valid_idx]
      if (inherits(x_valid, "haven_labelled")) x_valid <- .plain_numeric(x_valid)
      w_valid <- w[valid_idx]
      unique_vals <- sort(unique(x_valid))
      # show_unused: empty factor levels are listed with frequency 0
      if (is.factor(x_valid) && show_unused) {
        unique_vals <- factor(levels(x_valid), levels = levels(x_valid))
      }
      freq_weighted <- vapply(unique_vals, function(val) sum(w_valid[x_valid == val]), numeric(1))
      n_eff <- (sum(w_valid))^2 / sum(w_valid^2)
    } else {
      unique_vals <- numeric(0)
      freq_weighted <- numeric(0)
      n_eff <- NA
    }

    # Add NA values if show_na = TRUE and there are missing values
    if (show_na && any(is.na(x))) {
      na_freq <- sum(w[is.na(x) & !is.na(w)])
      if (na_freq > 0 || any(is.na(x))) {  # Include NA row if there are any NA values
        unique_vals <- c(unique_vals, NA)
        freq_weighted <- c(freq_weighted, na_freq)
      }
    }

    # Calculate totals
    total_valid_weighted <- sum(freq_weighted[!is.na(unique_vals)])
    total_all_weighted <- sum(w[!is.na(w)])  # Sum of all weights, including those with missing x

    # Calculate percentages
    # Raw % uses total including missing, Valid % uses only valid total
    prc <- freq_weighted / total_all_weighted * 100  # Raw % based on ALL observations

    # Valid % - only for non-NA values
    valid_prc <- rep(NA, length(freq_weighted))
    non_na_idx <- !is.na(unique_vals)
    if (any(non_na_idx) && total_valid_weighted > 0) {
      valid_prc[non_na_idx] <- freq_weighted[non_na_idx] / total_valid_weighted * 100
    }

    # Cumulative frequencies and percentages (only for non-NA values)
    cum_freq <- rep(NA, length(freq_weighted))
    cum_prc <- rep(NA, length(freq_weighted))
    if (any(non_na_idx)) {
      cum_freq[non_na_idx] <- cumsum(freq_weighted[non_na_idx])
      cum_prc[non_na_idx] <- cumsum(valid_prc[non_na_idx])
    }
    # Set cumulative values for NA row if it exists
    if (any(is.na(unique_vals))) {
      na_pos <- which(is.na(unique_vals))
      cum_freq[na_pos] <- NA
      cum_prc[na_pos] <- NA
    }

    result <- data.frame(
      # factor categories as text, like the unweighted branch: a factor
      # value column turned the numeric values of the next variable into
      # NA when the tables of several variables were row-bound
      value = if (is.factor(unique_vals)) as.character(unique_vals) else unique_vals,
      label = get_value_labels(x, as.character(unique_vals)),
      freq = freq_weighted,
      prc = prc,
      valid_prc = valid_prc,
      cum_freq = cum_freq,
      cum_prc = cum_prc,
      n_eff = rep(n_eff, length(unique_vals)),
      stringsAsFactors = FALSE
    )
  }

  # --- Tagged NA expansion: split single NA row into per-tag rows + total ---
  if (has_tagged_na && show_na && any(is.na(x))) {
    result <- .expand_tagged_na_rows(result, x, w)
  }

  # Inject unused value labels as rows with freq=0
  # (skip labels that are tagged NAs - those are already expanded above)
  if (show_unused && !is.null(attr(x, "labels"))) {
    all_labels <- attr(x, "labels")  # named numeric: c("LINKS" = 1, "RECHTS" = 10, ...)

    # Filter out NA labels (tagged NAs) - they are handled by tagged NA expansion
    valid_labels <- all_labels[!is.na(all_labels)]
    observed_values <- result$value[!is.na(result$value)]
    unused_labels <- valid_labels[!valid_labels %in% observed_values]

    if (length(unused_labels) > 0) {
      has_n_eff <- "n_eff" %in% names(result)
      unused_rows <- data.frame(
        value = as.numeric(unused_labels),
        label = names(unused_labels),
        freq = 0,
        prc = 0,
        valid_prc = 0,
        cum_freq = NA,
        cum_prc = NA,
        stringsAsFactors = FALSE
      )
      if (has_n_eff) {
        unused_rows$n_eff <- result$n_eff[1]
      }
      if ("is_na_row" %in% names(result)) {
        unused_rows$is_na_row <- FALSE
      }
      if ("na_display_value" %in% names(result)) {
        unused_rows$na_display_value <- NA_character_
      }

      # Separate NA rows, merge non-NA rows, re-sort, re-append NA rows
      na_rows <- result[is.na(result$value), , drop = FALSE]
      non_na_rows <- result[!is.na(result$value), , drop = FALSE]
      combined <- rbind(non_na_rows, unused_rows)
      combined <- combined[order(combined$value), , drop = FALSE]

      # Recalculate cumulative % for all non-NA rows
      combined$cum_prc <- cumsum(combined$valid_prc)
      combined$cum_freq <- cumsum(combined$freq)

      # Re-append NA rows
      if (nrow(na_rows) > 0) {
        result <- rbind(combined, na_rows)
      } else {
        result <- combined
      }
      rownames(result) <- NULL
    }
  }

  # Sort if requested: by frequency (as documented, SPSS /FORMAT=AFREQ|DFREQ).
  # NA/total rows keep their position at the bottom; cumulative statistics
  # are recomputed in display order so the Cum. % column stays monotone.
  if (sort_frq %in% c("asc", "desc")) {
    is_value_row <- !is.na(result$value)
    value_rows <- result[is_value_row, , drop = FALSE]
    other_rows <- result[!is_value_row, , drop = FALSE]
    value_rows <- value_rows[order(value_rows$freq,
                                   decreasing = (sort_frq == "desc")), ,
                             drop = FALSE]
    value_rows$cum_freq <- cumsum(value_rows$freq)
    value_rows$cum_prc <- cumsum(value_rows$valid_prc)
    result <- rbind(value_rows, other_rows)
    rownames(result) <- NULL
  }

  return(result)
}

# Helper: Expand the single NA row into per-tag rows + a total row
# Called when tagged NAs are detected (any format: SPSS, Stata, SAS)
.expand_tagged_na_rows <- function(result, x, w) {
  tag_map <- attr(x, "na_tag_map")  # named numeric (SPSS/tag_na): c("a" = -42), or character (native): c("a" = ".a")
  labels  <- attr(x, "labels")
  has_n_eff <- "n_eff" %in% names(result)

  # Find the existing aggregate NA row
  na_row_idx <- which(is.na(result$value))
  if (length(na_row_idx) == 0L) return(result)

  na_total_freq <- result$freq[na_row_idx]
  na_total_prc  <- result$prc[na_row_idx]

  # Get tags for each NA observation
  na_mask <- is.na(x)
  na_tags <- .na_tags(x[na_mask])

  # Count per tag (unweighted or weighted)
  unique_tags <- sort(unique(na_tags[!is.na(na_tags)]))

  # Also handle system NAs (untagged)
  n_system_na <- sum(is.na(na_tags))

  # Build a full-length tag vector (empty string for non-NA, tag char for tagged NA, NA for system NA)
  all_tags <- rep("", length(x))
  all_tags[na_mask] <- ifelse(is.na(na_tags), NA_character_, na_tags)

  tag_rows <- list()
  for (tag in unique_tags) {
    tag_match <- !is.na(all_tags) & all_tags == tag
    tag_mask <- na_mask & tag_match

    if (is.null(w)) {
      tag_freq <- sum(tag_mask)
    } else {
      tag_freq <- sum(w[tag_mask & !is.na(w)])
    }

    # Total for Raw % denominator
    if (is.null(w)) {
      total_all <- length(x)
    } else {
      total_all <- sum(w[!is.na(w)])
    }
    tag_prc <- tag_freq / total_all * 100

    # Find label for this tag
    tag_label <- ""
    if (!is.null(labels)) {
      na_labels <- labels[is.na(labels)]
      label_tags <- vapply(na_labels, haven::na_tag, character(1))
      match_idx <- which(label_tags == tag)
      if (length(match_idx) > 0L) {
        tag_label <- names(na_labels)[match_idx[1]]
      }
    }

    # Display value: show original missing value code
    display_val <- if (tag %in% names(tag_map)) as.character(tag_map[tag]) else paste0("NA(", tag, ")")

    row_data <- data.frame(
      value = NA_real_,
      label = tag_label,
      freq = tag_freq,
      prc = tag_prc,
      valid_prc = NA_real_,
      cum_freq = NA_real_,
      cum_prc = NA_real_,
      stringsAsFactors = FALSE
    )
    if (has_n_eff) {
      row_data$n_eff <- result$n_eff[1]
    }
    row_data$is_na_row <- TRUE
    row_data$na_display_value <- display_val

    tag_rows <- c(tag_rows, list(row_data))
  }

  # Add system NA row if present
  if (n_system_na > 0L) {
    # System NAs are positions where na_mask is TRUE but all_tags is NA (no tag)
    sys_mask <- na_mask & is.na(all_tags)
    if (is.null(w)) {
      sys_freq <- sum(sys_mask)
      total_all <- length(x)
    } else {
      sys_freq <- sum(w[sys_mask & !is.na(w)])
      total_all <- sum(w[!is.na(w)])
    }
    row_data <- data.frame(
      value = NA_real_,
      label = "",
      freq = sys_freq,
      prc = sys_freq / total_all * 100,
      valid_prc = NA_real_,
      cum_freq = NA_real_,
      cum_prc = NA_real_,
      stringsAsFactors = FALSE
    )
    if (has_n_eff) {
      row_data$n_eff <- result$n_eff[1]
    }
    row_data$is_na_row <- TRUE
    row_data$na_display_value <- "NA"

    tag_rows <- c(tag_rows, list(row_data))
  }

  # Build the NA total (sum) row
  total_row <- data.frame(
    value = NA_real_,
    label = "Total Missing",
    freq = na_total_freq,
    prc = na_total_prc,
    valid_prc = NA_real_,
    cum_freq = NA_real_,
    cum_prc = NA_real_,
    stringsAsFactors = FALSE
  )
  if (has_n_eff) {
    total_row$n_eff <- result$n_eff[1]
  }
  total_row$is_na_row <- TRUE
  total_row$na_display_value <- "NA(total)"

  # Replace the original aggregate NA row with the expanded rows
  non_na_result <- result[-na_row_idx, , drop = FALSE]
  # Add is_na_row column to non-NA rows
  if (nrow(non_na_result) > 0L) {
    non_na_result$is_na_row <- FALSE
    non_na_result$na_display_value <- NA_character_
  } else {
    non_na_result$is_na_row <- logical(0)
    non_na_result$na_display_value <- character(0)
  }

  # Build the Valid total (sum) row
  valid_total_freq <- sum(non_na_result$freq)
  valid_total_prc  <- sum(non_na_result$prc)
  valid_total_row <- data.frame(
    value = NA_real_,
    label = "Total Valid",
    freq = valid_total_freq,
    prc = valid_total_prc,
    valid_prc = 100,
    cum_freq = NA_real_,
    cum_prc = NA_real_,
    stringsAsFactors = FALSE
  )
  if (has_n_eff) {
    valid_total_row$n_eff <- result$n_eff[1]
  }
  valid_total_row$is_na_row <- FALSE
  valid_total_row$na_display_value <- "Total"

  expanded_na <- do.call(rbind, tag_rows)
  result <- rbind(non_na_result, valid_total_row, expanded_na, total_row)
  rownames(result) <- NULL

  result
}

# Helper function: Calculate descriptive statistics
calculate_single_stats <- function(x, w = NULL) {
  if (is.null(w)) {
    x_valid <- x[!is.na(x)]
    n <- length(x_valid)
    
    # For numeric variables, calculate mean and sd.
    # Skewness via the shared SPSS Type-2 helper so the header line agrees
    # with describe() and w_skew() (it previously used a Type-1 population
    # formula - audit finding).
    if (is.numeric(x_valid)) {
      mean_val <- if (n > 0) mean(x_valid) else NA_real_
      sd_val <- if (n > 1) sd(x_valid) else NA_real_
      skewness <- if (n > 2 && sd_val > 0) .calc_skewness(x_valid) else NA
    } else {
      # For factors or other non-numeric variables
      mean_val <- NA
      sd_val <- NA
      skewness <- NA
    }
  } else {
    valid_idx <- !is.na(x) & !is.na(w)
    if (!any(valid_idx)) {
      return(list(mean = NA, sd = NA, total_n = length(x), valid_n = 0, skewness = NA))
    }
    
    x_valid <- x[valid_idx]
    w_valid <- w[valid_idx]
    
    # Weighted mean/sd/skewness via the shared SPSS frequency-weight
    # formulas (sum(w)-1 denominator, Type-2 skewness) so the header line
    # agrees with describe() and the w_* functions (it previously used
    # population formulas - audit finding).
    if (is.numeric(x_valid)) {
      w_sum <- sum(w_valid)
      mean_val <- .w_mean(x_valid, w_valid, na.rm = FALSE)
      sd_val <- if (w_sum > 1) .w_sd(x_valid, w_valid, na.rm = FALSE) else NA
      skewness <- if (length(x_valid) > 2 && !is.na(sd_val) && sd_val > 0) {
        .calc_skewness(x_valid, w_valid)
      } else NA
    } else {
      # For factors or other non-numeric variables
      mean_val <- NA
      sd_val <- NA
      skewness <- NA
    }
  }
  
  list(mean = mean_val, sd = sd_val, 
       total_n = if (is.null(w)) length(x) else sum(w[!is.na(w)]), 
       valid_n = if (is.null(w)) length(x_valid) else sum(w_valid), 
       skewness = skewness)
}

# Helper function: Process variables for a dataset
process_variables <- function(data, var_names, w_name, sort_frq, show_na = TRUE, show_unused = FALSE, group_info = NULL) {
  frequencies_list <- list()
  stats_list <- list()
  
  for (var_name in var_names) {
    x <- data[[var_name]]
    w <- if (!is.null(w_name)) data[[w_name]] else NULL
    
    # Calculate frequencies and stats
    freq_result <- calculate_single_frequency(x, w, sort_frq, show_na, show_unused)
    freq_result$Variable <- var_name
    
    stats <- calculate_single_stats(x, w)
    stats_df <- data.frame(Variable = var_name, mean = stats$mean, sd = stats$sd,
                          total_n = stats$total_n, valid_n = stats$valid_n,
                          skewness = stats$skewness, stringsAsFactors = FALSE)
    
    # Add group information if provided
    if (!is.null(group_info) && nrow(freq_result) > 0) {
      group_info_expanded <- group_info[rep(1, nrow(freq_result)), , drop = FALSE]
      freq_result <- cbind(group_info_expanded, freq_result)
      stats_df <- cbind(group_info, stats_df)
    }
    
    frequencies_list[[var_name]] <- freq_result
    stats_list[[var_name]] <- stats_df
  }

  # Normalize columns before rbind: some variables may have tagged-NA columns

  # (is_na_row, na_display_value) while others do not
  if (length(frequencies_list) > 1L) {
    all_cols <- unique(unlist(lapply(frequencies_list, names)))
    frequencies_list <- lapply(frequencies_list, function(df) {
      missing_cols <- setdiff(all_cols, names(df))
      for (mc in missing_cols) {
        df[[mc]] <- if (mc == "is_na_row") FALSE else NA
      }
      df[all_cols]
    })
  }

  list(frequencies = do.call(rbind, frequencies_list), stats = do.call(rbind, stats_list))
}

# Helper function: Calculate frequencies for ungrouped data
calculate_ungrouped_frequencies <- function(data, var_names, w_name, sort_frq, show_na = TRUE, show_unused = FALSE) {
  process_variables(data, var_names, w_name, sort_frq, show_na, show_unused)
}

# Helper function: Calculate frequencies for grouped data
calculate_grouped_frequencies <- function(data, var_names, w_name, sort_frq, show_na = TRUE, show_unused = FALSE) {
  data_list <- dplyr::group_split(data)
  group_keys <- dplyr::group_keys(data)

  results_list <- lapply(seq_along(data_list), function(i) {
    process_variables(data_list[[i]], var_names, w_name, sort_frq, show_na, show_unused, group_keys[i, , drop = FALSE])
  })
  
  freq_parts <- lapply(results_list, `[[`, "frequencies")

  # Normalize columns across groups (tagged-NA columns may differ)
  if (length(freq_parts) > 1L) {
    all_cols <- unique(unlist(lapply(freq_parts, names)))
    freq_parts <- lapply(freq_parts, function(df) {
      missing_cols <- setdiff(all_cols, names(df))
      for (mc in missing_cols) {
        df[[mc]] <- if (mc == "is_na_row") FALSE else NA
      }
      df[all_cols]
    })
  }

  list(
    frequencies = do.call(rbind, freq_parts),
    stats = do.call(rbind, lapply(results_list, `[[`, "stats"))
  )
}

#' Print method for frequency objects
#'
#' @description
#' Prints the full frequency tables (counts, raw/valid/cumulative
#' percentages, missing value breakdowns). The output of
#' \code{frequency()} is a frequency table by nature, so \code{print()}
#' and \code{summary()} display the same tables; \code{summary()}
#' additionally offers section toggles and a \code{digits} option.
#'
#' @param x An object of class "frequency"
#' @param digits Number of decimal places of the percentages (default: 2);
#'   the summary statistics line uses at least two decimals.
#' @param ... Additional arguments passed to print
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- frequency(survey_data, gender)
#' result              # full frequency tables
#' summary(result)     # same tables, with section toggles
#'
#' @export
#' @method print frequency
print.frequency <- function(x, digits = 2, ...) {
  print(summary(x, digits = digits))
  invisible(x)
}

#' Summary method for frequency results
#'
#' @description
#' Creates a summary object that produces detailed output when printed,
#' including the per-variable summary statistics line (total N, valid N,
#' mean, SD, skewness) and the full ASCII frequency tables with counts,
#' raw/valid/cumulative percentages, and missing value breakdowns.
#'
#' @param object A \code{frequency} result object.
#' @param frequency_table Logical. Show the frequency tables?
#'   (Default: TRUE)
#' @param summary_stats Logical. Show the per-variable summary statistics
#'   line? (Default: TRUE)
#' @param digits Number of decimal places for percentages and summary
#'   statistics (Default: 2); the statistics keep at least two decimals.
#' @param ... Additional arguments (not used).
#' @return A \code{summary.frequency} object.
#'
#' @examples
#' result <- frequency(survey_data, gender)
#' summary(result)
#' summary(result, summary_stats = FALSE)
#'
#' @seealso \code{\link{frequency}} for the main analysis function.
#' @export
#' @method summary frequency
summary.frequency <- function(object, frequency_table = TRUE,
                              summary_stats = TRUE, digits = 2, ...) {
  build_summary_object(
    object     = object,
    show       = list(frequency_table = frequency_table,
                      summary_stats = summary_stats),
    digits     = digits,
    class_name = "summary.frequency"
  )
}

#' Print summary of frequency results (detailed output)
#'
#' @description
#' Prints formatted frequency statistics with ASCII tables, with sections
#' controlled by the boolean parameters passed to
#' \code{\link{summary.frequency}}.
#'
#' @param x A \code{summary.frequency} object created by
#'   \code{\link{summary.frequency}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- frequency(survey_data, gender)
#' summary(result)                          # all sections
#' summary(result, frequency_table = FALSE) # only summary statistics
#'
#' @seealso \code{\link{frequency}} for the main analysis,
#'   \code{\link{summary.frequency}} for summary options.
#' @export
#' @method print summary.frequency
print.summary.frequency <- function(x, ...) {
  digits <- x$digits

  # Resolve show toggles
  show_frequency_table <- isTRUE(x$show$frequency_table)
  show_summary_stats   <- isTRUE(x$show$summary_stats)

  title <- get_standard_title("Frequency Analysis", x$weights, "Results")
  print_header(title)

  # The header statistics keep at least two decimals: print(digits = 0)
  # is meant for the percentages and used to turn mean = 3.63 into 4
  stat_digits <- max(2L, digits)

  for (var in x$variables) {
    var_label <- x$labels[var]
    cat(sprintf("\n%s\n", format_variable_name(var, var_label)))
    is_text <- .fre_is_text(x, var)

    if (x$is_grouped) {
      unique_groups <- unique(x$results[x$groups])

      for (i in seq_len(nrow(unique_groups))) {
        group_values <- unique_groups[i, , drop = FALSE]

        group_results <- x$results
        for (g in names(group_values)) {
          group_results <- group_results[.group_match(group_results[[g]], group_values[[g]]), ]
        }
        group_results <- group_results[group_results$Variable == var, ]
        if (nrow(group_results) == 0) next

        group_stats <- x$stats
        for (g in names(group_values)) {
          group_stats <- group_stats[.group_match(group_stats[[g]], group_values[[g]]), ]
        }
        stats <- group_stats[group_stats$Variable == var, ]

        print_group_header(group_values)
        if (show_summary_stats) {
          cat(.fre_stats_line(stats, stat_digits), "\n\n", sep = "")
        } else {
          cat("\n")
        }
        if (show_frequency_table) {
          .print_fre_table(group_results, x$options, digits = digits, is_text = is_text)
        }
      }
    } else {
      var_results <- x$results[x$results$Variable == var, ]
      stats <- x$stats[x$stats$Variable == var, ]

      if (show_summary_stats) {
        cat(.fre_stats_line(stats, stat_digits), "\n\n", sep = "")
      }
      if (show_frequency_table) {
        .print_fre_table(var_results, x$options, digits = digits, is_text = is_text)
      }
    }
  }

  invisible(x)
}

#' Is a frequency() variable categorical text (factor/character/logical)?
#'
#' Text variables show their category in the Value column and get no
#' Label column (it duplicated the value). Objects created before
#' value_types was stored fall back to inspecting the value column.
#' @noRd
.fre_is_text <- function(x, var) {
  if (!is.null(x$value_types) && var %in% names(x$value_types)) {
    return(identical(unname(x$value_types[var]), "text"))
  }
  v <- x$results$value[x$results$Variable == var]
  v <- v[!is.na(v)]
  !is.numeric(v) && anyNA(suppressWarnings(as.numeric(as.character(v))))
}

#' "# total N=... valid N=... mean=... sd=... skewness=..." header line
#'
#' Statistics that do not exist (non-numeric variable, no valid values)
#' are left out instead of printing "mean=NA" or "mean=NaN".
#' @noRd
.fre_stats_line <- function(stats, digits) {
  parts <- c(sprintf("total N=%.0f", stats$total_n[1]),
             sprintf("valid N=%.0f", stats$valid_n[1]))
  add <- function(name, v) {
    if (length(v) == 1 && is.numeric(v) && !is.na(v)) {
      paste0(name, "=", formatC(v, format = "f", digits = digits))
    }
  }
  parts <- c(parts, add("mean", stats$mean[1]), add("sd", stats$sd[1]),
             add("skewness", stats$skewness[1]))
  paste0("# ", paste(parts, collapse = " "))
}

#' Wrap a text to a display width (hard-breaking overlong words)
#' @noRd
.fre_wrap <- function(text, width) {
  if (is.na(text) || !nzchar(text) || nchar(text, type = "width") <= width) {
    return(if (is.na(text)) "" else text)
  }
  lines <- strwrap(text, width = width + 1)
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

#' Print one frequency table in the SPSS FREQUENCIES layout
#'
#' Valid categories, "Total valid", the missing categories, "Total
#' missing" (only with two or more missing categories, as in SPSS) and
#' the grand "Total". Without missing values a single "Total" row ends the
#' table. Cells that have no value stay empty (they used to read "NA").
#' Every column is sized to its content (N was a fixed 8 characters and
#' cut large weighted counts), labels are left-aligned and never cut; when
#' the table is wider than the console, long labels wrap onto extra lines.
#'
#' @param results Frequency rows of one variable (and group)
#' @param options The frequency object's options
#' @param digits Decimals of the percentages
#' @param is_text Categorical text variable (no Label column)
#' @param width Console width
#' @noRd
.print_fre_table <- function(results, options, digits = 2, is_text = FALSE,
                             width = getOption("width", 80)) {
  pct <- function(v) ifelse(is.na(v), "", formatC(v, format = "f", digits = digits))
  cnt <- function(v) ifelse(is.na(v), "", sprintf("%.0f", round(v)))

  # --- Split the rows: valid categories and missing categories -------------
  tagged <- "na_display_value" %in% names(results) &&
    any(!is.na(results$na_display_value))
  if (tagged) {
    ndv <- results$na_display_value
    na_row <- !is.na(results$is_na_row) & results$is_na_row
    valid_rows <- results[!na_row & is.na(ndv), , drop = FALSE]
    miss_sel <- na_row & !is.na(ndv) & ndv != "NA(total)"
    miss_rows <- results[miss_sel, , drop = FALSE]
    miss_values <- ndv[miss_sel]
  } else {
    valid_rows <- results[!is.na(results$value), , drop = FALSE]
    miss_rows <- results[is.na(results$value), , drop = FALSE]
    miss_values <- rep("NA", nrow(miss_rows))
  }
  if (!isTRUE(options$show_na)) {
    miss_rows <- miss_rows[0, , drop = FALSE]
    miss_values <- character(0)
  }

  label_of <- function(l) ifelse(is.na(l), "", as.character(l))
  # No Label column for text categories (it repeated the value) or when no
  # row of this table has a label (a numeric variable next to a labelled one)
  show_lab <- isTRUE(options$show_labels) && !is_text &&
    any(nzchar(c(label_of(valid_rows$label), label_of(miss_rows$label))))
  show_prc <- isTRUE(options$show_prc)
  show_valid <- isTRUE(options$show_valid)
  # Cum. % is the cumulative VALID percent: it goes with Valid %
  show_cum <- isTRUE(options$show_sum) && show_valid

  # --- Assemble the rows (kind: "cat" = category row, "sum" = total row) ---
  rows <- list()
  add_row <- function(kind, value, label, n, raw, valid, cum) {
    rows[[length(rows) + 1]] <<- list(kind = kind, value = value, label = label,
                                      n = n, raw = raw, valid = valid, cum = cum)
  }
  for (i in seq_len(nrow(valid_rows))) {
    r <- valid_rows[i, , drop = FALSE]
    add_row("cat", as.character(r$value), label_of(r$label), cnt(r$freq),
            pct(r$prc), pct(r$valid_prc), pct(r$cum_prc))
  }
  valid_n <- sum(valid_rows$freq, na.rm = TRUE)
  valid_raw <- sum(valid_rows$prc, na.rm = TRUE)
  valid_100 <- if (valid_n > 0) pct(100) else ""
  has_valid <- nrow(valid_rows) > 0
  has_miss <- nrow(miss_rows) > 0

  if (has_miss) {
    if (has_valid) {
      add_row("sum", "Total valid", "", cnt(valid_n), pct(valid_raw), valid_100, "")
    }
    for (i in seq_len(nrow(miss_rows))) {
      r <- miss_rows[i, , drop = FALSE]
      add_row("cat", miss_values[i], label_of(r$label), cnt(r$freq), pct(r$prc), "", "")
    }
    miss_n <- sum(miss_rows$freq, na.rm = TRUE)
    miss_raw <- sum(miss_rows$prc, na.rm = TRUE)
    if (nrow(miss_rows) > 1) {
      add_row("sum", "Total missing", "", cnt(miss_n), pct(miss_raw), "", "")
    }
    add_row("sum", "Total", "", cnt(valid_n + miss_n), pct(valid_raw + miss_raw), "", "")
  } else {
    add_row("sum", "Total", "", cnt(valid_n), pct(valid_raw), valid_100, "")
  }

  # --- Columns ---------------------------------------------------------------
  cols <- c("value", if (show_lab) "label", "n", if (show_prc) "raw",
            if (show_valid) "valid", if (show_cum) "cum")
  headers <- c(value = "Value", label = "Label", n = "N", raw = "Raw %",
               valid = "Valid %", cum = "Cum. %")[cols]
  dw <- function(s) nchar(s, type = "width")
  cat_rows <- Filter(function(r) r$kind == "cat", rows)
  sum_rows <- Filter(function(r) r$kind == "sum", rows)
  widths <- vapply(cols, function(cl) {
    vals <- vapply(if (cl %in% c("value", "label")) cat_rows else rows,
                   function(r) r[[cl]], character(1))
    max(dw(headers[[cl]]), dw(vals), 1L)
  }, integer(1))

  # Total rows span the Value (and Label) columns
  n_span <- if (show_lab) 2L else 1L
  span_w <- function() sum(widths[seq_len(n_span)]) + 3L * (n_span - 1L)
  need <- max(dw(vapply(sum_rows, function(r) r$value, character(1))))
  if (need > span_w()) {
    widths[n_span] <- widths[n_span] + (need - span_w())
  }

  # Too wide for the console: wrap the labels
  total_w <- 1L + sum(widths + 3L)
  if (show_lab && total_w > width) {
    widths["label"] <- max(10L, widths[["label"]] - (total_w - width))
    need <- max(dw(vapply(sum_rows, function(r) r$value, character(1))))
    if (need > span_w()) widths["label"] <- widths[["label"]] + (need - span_w())
  }

  text_col <- c(value = is_text, label = TRUE, n = FALSE, raw = FALSE,
                valid = FALSE, cum = FALSE)

  rule <- function() {
    cat("+", paste(strrep("-", widths + 2L), collapse = "+"), "+\n", sep = "")
  }
  cell <- function(s, w, left) {
    paste0(" ", pad_utf8(s, w, align = if (left) "left" else "right"), " ")
  }
  emit <- function(r) {
    if (r$kind == "sum") {
      first <- cell(r$value, span_w(), TRUE)
      rest <- cols[-seq_len(n_span)]
    } else {
      first <- NULL
      rest <- cols
    }
    lab_lines <- if (show_lab && r$kind == "cat") .fre_wrap(r$label, widths[["label"]]) else ""
    for (k in seq_along(lab_lines)) {
      cells <- vapply(rest, function(cl) {
        s <- if (cl == "label") lab_lines[k] else if (k == 1L) r[[cl]] else ""
        cell(s, widths[[cl]], text_col[[cl]])
      }, character(1))
      cat("|", paste(c(first, cells), collapse = "|"), "|\n", sep = "")
      first <- if (!is.null(first)) cell("", span_w(), TRUE)
    }
  }

  rule()
  cat("|", paste(vapply(cols, function(cl) {
    cell(headers[[cl]], widths[[cl]], text_col[[cl]])
  }, character(1)), collapse = "|"), "|\n", sep = "")
  rule()
  prev <- "cat"
  for (r in rows) {
    if (r$kind == "sum" || prev == "sum") rule()
    emit(r)
    prev <- r$kind
  }
  rule()
  cat("\n")
  invisible(NULL)
}

#' @rdname frequency
#' @usage fre(data, ..., weights = NULL, sort_frq = "none", show_na = TRUE,
#'   show_prc = TRUE, show_valid = TRUE, show_sum = TRUE, show_labels = "auto",
#'   show_unused = FALSE, sort.frq = NULL, show.na = NULL, show.prc = NULL,
#'   show.valid = NULL, show.sum = NULL, show.labels = NULL, show.unused = NULL)
#' @export
fre <- frequency

