
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
#'
#' @details
#' ## Understanding the Results
#'
#' **P-value**: If p < 0.05, the variables are likely related (not independent)
#' - p < 0.001: Very strong evidence of relationship
#' - p < 0.01: Strong evidence of relationship
#' - p < 0.05: Moderate evidence of relationship
#' - p ≥ 0.05: No significant relationship found
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
#' @seealso
#' \code{\link[stats]{chisq.test}} for the base R chi-squared test.
#'
#' \code{\link{crosstab}} for detailed cross-tabulation tables.
#'
#' \code{\link{frequency}} for single-variable frequency tables.
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

  # Build label maps for display (before any subsetting)
  label_maps <- stats::setNames(
    list(.build_label_map(data[[var_names[1]]]),
         .build_label_map(data[[var_names[2]]])),
    var_names
  )

  # Process weights using centralized helper
  weights_info <- .process_weights(data, rlang::enquo(weights))
  w_name <- weights_info$name
  
  # Perform chi-squared test
  if (is_grouped) {
    # Group analysis
    data_list <- dplyr::group_split(data)
    group_keys <- dplyr::group_keys(data)
    
    results_list <- lapply(seq_along(data_list), function(i) {
      group_data <- data_list[[i]]
      group_info <- group_keys[i, , drop = FALSE]
      
      # Test for this group
      result <- tryCatch({
        # Extract variables
        var1 <- group_data[[var_names[1]]]
        var2 <- group_data[[var_names[2]]]
        
        if (!is.null(w_name)) {
          # Weighted test - round weights to integers for SPSS compatibility
          weights_data <- group_data[[w_name]]
          tbl <- xtabs(weights_data ~ var1 + var2)
          tbl <- round(tbl)  # Round to match SPSS behavior
        } else {
          # Unweighted test
          tbl <- table(var1, var2)
        }
        names(dimnames(tbl)) <- var_names

        test_result <- chisq.test(tbl, correct = correct)

        # Create results entry
        data.frame(
          group_info,
          chi_squared = as.numeric(test_result$statistic),
          df = as.numeric(test_result$parameter),
          p_value = test_result$p.value,
          n = sum(tbl),
          observed = I(list(test_result$observed)),
          expected = I(list(test_result$expected)),
          residuals = I(list(test_result$residuals)),
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        # Return NA results if chi-squared test fails
        data.frame(
          group_info,
          chi_squared = NA_real_,
          df = NA_integer_,
          p_value = NA_real_,
          n = NA_integer_,
          observed = I(list(NULL)),
          expected = I(list(NULL)), 
          residuals = I(list(NULL)),
          stringsAsFactors = FALSE
        )
      })
    })
    
    # Combine all results
    results_df <- do.call(rbind, results_list)
    
  } else {
    # Single analysis for whole dataset
    var1 <- data[[var_names[1]]]
    var2 <- data[[var_names[2]]]
    
    if (!is.null(w_name)) {
      # Weighted test - round weights to integers for SPSS compatibility
      weights_data <- data[[w_name]]
      tbl <- xtabs(weights_data ~ var1 + var2)
      tbl <- round(tbl)  # Round to match SPSS behavior
    } else {
      # Unweighted test
      tbl <- table(var1, var2)
    }
    names(dimnames(tbl)) <- var_names

    test_result <- chisq.test(tbl, correct = correct)

    # Create results dataframe
    results_df <- data.frame(
      chi_squared = as.numeric(test_result$statistic),
      df = as.numeric(test_result$parameter),
      p_value = test_result$p.value,
      n = sum(tbl),
      stringsAsFactors = FALSE
    )
    
    # Add tables as list columns
    results_df$observed <- list(test_result$observed)
    results_df$expected <- list(test_result$expected)
    results_df$residuals <- list(test_result$residuals)
  }
  
  # Warn if any expected cell counts are < 5
  for (i in seq_len(nrow(results_df))) {
    exp_tbl <- results_df$expected[[i]]
    if (!is.null(exp_tbl)) {
      n_low <- sum(exp_tbl < 5)
      if (n_low > 0) {
        pct_low <- round(100 * n_low / length(exp_tbl), 1)
        cli_warn("{n_low} cell{?s} ({pct_low}%) ha{?s/ve} expected count < 5. Chi-squared approximation may be unreliable.")
      }
    }
  }

  # Calculate effect sizes
  results_df$cramers_v <- NA_real_
  results_df$phi <- NA_real_
  results_df$gamma <- NA_real_
  results_df$contingency_c <- NA_real_
  results_df$table_rows <- NA_integer_
  results_df$table_cols <- NA_integer_

  # Add p-value fields for effect sizes
  results_df$phi_p_value <- NA_real_
  results_df$cramers_v_p_value <- NA_real_
  results_df$gamma_p_value <- NA_real_

  for (i in seq_len(nrow(results_df))) {
    if (!is.null(results_df$observed[[i]]) && !is.na(results_df$chi_squared[i])) {
      # Calculate effect sizes
      n <- results_df$n[i]
      chi_squared <- results_df$chi_squared[i]

      # Get table dimensions
      r <- nrow(results_df$observed[[i]])
      c <- ncol(results_df$observed[[i]])

      # Store dimensions for conditional display
      results_df$table_rows[i] <- r
      results_df$table_cols[i] <- c

      # Phi coefficient (most meaningful for 2x2 tables)
      results_df$phi[i] <- sqrt(chi_squared / n)

      # Cramer's V (appropriate for any table size)
      results_df$cramers_v[i] <- sqrt(chi_squared / (n * min(r - 1, c - 1)))

      # Contingency coefficient C
      results_df$contingency_c[i] <- sqrt(chi_squared / (chi_squared + n))

      # Phi and Cramer's V p-values are the same as chi-squared p-value
      results_df$phi_p_value[i] <- results_df$p_value[i]
      results_df$cramers_v_p_value[i] <- results_df$p_value[i]

      # Goodman and Kruskal's Gamma
      # Calculate concordant (C_ij) and discordant (D_ij) pair counts per cell
      obs_table <- results_df$observed[[i]]

      # For each cell (i,j), compute:
      #   C_ij = sum of all cells below-right (concordant with this cell)
      #   D_ij = sum of all cells below-left (discordant with this cell)
      C_mat <- matrix(0, nrow = r, ncol = c)
      D_mat <- matrix(0, nrow = r, ncol = c)

      for (row1 in 1:r) {
        for (col1 in 1:c) {
          # Concordant: cells below and to the right
          if (row1 < r && col1 < c) {
            for (row2 in (row1 + 1):r) {
              for (col2 in (col1 + 1):c) {
                C_mat[row1, col1] <- C_mat[row1, col1] + obs_table[row2, col2]
              }
            }
          }
          # Discordant: cells below and to the left
          if (row1 < r && col1 > 1) {
            for (row2 in (row1 + 1):r) {
              for (col2 in 1:(col1 - 1)) {
                D_mat[row1, col1] <- D_mat[row1, col1] + obs_table[row2, col2]
              }
            }
          }
        }
      }

      # P = total concordant pairs, Q = total discordant pairs
      P <- as.numeric(sum(obs_table * C_mat))
      Q <- as.numeric(sum(obs_table * D_mat))

      # Calculate Gamma
      if ((P + Q) > 0) {
        results_df$gamma[i] <- (P - Q) / (P + Q)
        gamma <- results_df$gamma[i]

        # ASE0 under null hypothesis (SPSS method)
        # Formula: ASE0 = (2 / (P + Q)) * sqrt(sum(n_ij * (C_ij - D_ij)^2) - (P - Q)^2 / n)
        # Reference: Goodman & Kruskal (1963); Agresti (2002)
        sum_nij_diff_sq <- sum(as.numeric(obs_table) * (C_mat - D_mat)^2)
        inner <- sum_nij_diff_sq - (P - Q)^2 / n

        if (inner > 0) {
          ase0 <- (2 / (P + Q)) * sqrt(inner)

          # Z-statistic and two-tailed p-value (SPSS approach)
          z_stat <- gamma / ase0
          results_df$gamma_p_value[i] <- 2 * (1 - pnorm(abs(z_stat)))
        } else {
          results_df$gamma_p_value[i] <- NA_real_
        }
      } else {
        results_df$gamma[i] <- 0
        results_df$gamma_p_value[i] <- NA_real_
      }
    }
  }
  
  # Create result object
  result <- list(
    results = results_df,
    variables = var_names,
    weights = w_name,
    correct = correct,
    is_grouped = is_grouped,
    groups = group_vars,
    label_maps = label_maps
  )
  
  class(result) <- "chi_square"
  return(result)
}

# Helper functions for print method

#' Print chi-square effect sizes table
#' @param df Data frame containing effect size columns
#' @param i Row index to extract from
#' @param digits Number of decimal places
#' @keywords internal
.print_chi_effect_sizes <- function(df, i, digits) {
  cramers_v <- df$cramers_v[i]
  if (is.na(cramers_v)) return(invisible(NULL))

  rows <- df$table_rows[i]
  cols <- df$table_cols[i]
  is_2x2 <- (rows == 2 && cols == 2)
  n <- df$n[i]

  phi <- df$phi[i]
  gamma <- df$gamma[i]
  cramers_v_p <- df$cramers_v_p_value[i]
  phi_p <- df$phi_p_value[i]
  gamma_p <- df$gamma_p_value[i]

  cat("\nEffect Sizes:\n")
  border_width <- paste(rep("-", 70), collapse = "")
  cat(border_width, "\n")

  fmt_p <- function(p) ifelse(p < 0.001, "<.001", round(p, digits))

  if (is_2x2) {
    effect_table <- data.frame(
      Measure = c("Cramer's V", "Phi", "Gamma"),
      Value = round(c(cramers_v, phi, gamma), digits),
      p_value = c(fmt_p(cramers_v_p), fmt_p(phi_p), fmt_p(gamma_p)),
      sig = c(as.character(add_significance_stars(cramers_v_p)),
              as.character(add_significance_stars(phi_p)),
              as.character(add_significance_stars(gamma_p))),
      Interpretation = c(.interpret_cramers_v(cramers_v),
                         .interpret_phi(phi),
                         .interpret_gamma(gamma)),
      stringsAsFactors = FALSE
    )
  } else {
    effect_table <- data.frame(
      Measure = c("Cramer's V", "Gamma"),
      Value = round(c(cramers_v, gamma), digits),
      p_value = c(fmt_p(cramers_v_p), fmt_p(gamma_p)),
      sig = c(as.character(add_significance_stars(cramers_v_p)),
              as.character(add_significance_stars(gamma_p))),
      Interpretation = c(.interpret_cramers_v(cramers_v),
                         .interpret_gamma(gamma)),
      stringsAsFactors = FALSE
    )
  }

  print(effect_table, row.names = FALSE)
  cat(border_width, "\n")
  cat(sprintf("Table size: %d\u00d7%d | N = %d\n", rows, cols, n))

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

.format_p_value <- function(p, digits = 3) {
  if (is.na(p)) return("     -")
  if (p < 0.001) return(" <.001")
  return(sprintf(" %5.3f", p))
}

#' Print method for chi_square
#'
#' @param x Chi-squared test results object
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments passed to print
#' @export
print.chi_square <- function(x, digits = 3, ...) {

  # Determine test type using standardized helper
  test_type <- get_standard_title("Chi-Squared Test of Independence", x$weights, "")
  print_header(test_type)
  
  # Add significance stars using standard helper
  sig <- sapply(x$results$p_value, add_significance_stars)
  
  if (!x$is_grouped) {
    # Simple test display using standardized helpers
    cat("\n")
    test_info <- list(
      "Variables" = paste(x$variables[1], "\u00d7", x$variables[2]),
      "Weights variable" = x$weights,
      "Continuity correction" = if (x$correct) "Yates' correction applied" else NULL
    )
    print_info_section(test_info)
    cat("\n")

    # Apply value labels to observed/expected matrices for display
    obs_display <- .relabel_matrix(x$results$observed[[1]], x$label_maps)
    exp_display <- .relabel_matrix(x$results$expected[[1]], x$label_maps)

    # Print observed frequencies
    cat("Observed Frequencies:\n")
    print(obs_display)

    # Print expected frequencies
    cat("\nExpected Frequencies:\n")
    print(round(exp_display, digits))

    # Print chi-squared results
    results_table <- data.frame(
      Chi_squared = round(x$results$chi_squared[1], digits),
      df = x$results$df[1],
      p_value = ifelse(x$results$p_value[1] < 0.001,
                     "<.001", round(x$results$p_value[1], digits)),
      sig = as.character(sig[1])
    )

    cat("\nChi-Squared Test Results:\n")
    border_width <- paste(rep("-", 50), collapse = "")
    cat(border_width, "\n")
    print(results_table, row.names = FALSE)
    cat(border_width, "\n")

    .print_chi_effect_sizes(x$results, 1, digits)

  } else {
    # Grouped tests
    cat("\nVariables tested:", paste(x$variables, collapse = " \u00d7 "), "\n")
    cat("Grouped by:", paste(x$groups, collapse = ", "), "\n")

    # Create a unified table with all results
    results_table <- x$results

    # Print results for each group
    for (i in seq_len(nrow(results_table))) {
      # Print group header using standardized helper
      group_values <- results_table[i, x$groups, drop = FALSE]
      print_group_header(group_values)
      cat("\n")  # Blank line

      # Apply value labels to observed matrix for display
      obs_display <- .relabel_matrix(results_table$observed[[i]], x$label_maps)

      # Print observed frequencies
      cat("Observed Frequencies:\n")
      print(obs_display)

      # Print test results
      test_results <- data.frame(
        Chi_squared = round(results_table$chi_squared[i], digits),
        df = results_table$df[i],
        p_value = ifelse(results_table$p_value[i] < 0.001,
                       "<.001", round(results_table$p_value[i], digits)),
        sig = as.character(sig[i])
      )

      cat("\nChi-Squared Test Results:\n")
      border_width <- paste(rep("-", 50), collapse = "")
      cat(border_width, "\n")
      print(test_results, row.names = FALSE)
      cat(border_width, "\n")

      .print_chi_effect_sizes(results_table, i, digits)
    }
  }
  
  print_significance_legend()

  invisible(x)
}

#' @rdname chi_square
#' @export
phi <- chi_square

#' @rdname chi_square
#' @export
cramers_v <- chi_square

#' @rdname chi_square
#' @export
goodman_gamma <- chi_square
