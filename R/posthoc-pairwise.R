# =============================================================================
# Shared pairwise post-hoc engine (Tukey HSD / Scheffe)
# =============================================================================
# tukey_test() and scheffe_test() share ~95% of their machinery: per-variable
# and per-group execution, weighted group statistics, pooled within-group MSE,
# and the printed report. Only the ~25-line critical-value / p-value / CI
# cores differ. This file hosts:
#
#   .pairwise_posthoc()        engine behind *_test.oneway_anova methods
#   .pairwise_posthoc_single() one variable: group stats + pooled MSE + core
#   .tukey_stats()             studentized-range core (qtukey/ptukey)
#   .scheffe_stats()           (k-1)*F core (qf/pf)
#   .print_pairwise_posthoc()  shared print implementation
#
# The statistical code paths are copied verbatim from the original
# R/tukey_test.R and R/scheffe_test.R — do not "improve" any formula here;
# the SPSS validation suite depends on bit-identical behavior.
# =============================================================================

#' Tukey HSD critical-value / p-value / CI core
#'
#' Studentized range distribution: q_crit for the CI margin, and the
#' t-statistic converted to a q-statistic for the adjusted p-value.
#'
#' @param diff Mean difference (group1 - group2)
#' @param se Standard error of the difference
#' @param n_groups Number of groups k
#' @param df Error degrees of freedom (pooled weighted df)
#' @param conf.level Confidence level
#' @return List with stat (t), conf_low, conf_high, p_adjusted
#' @noRd
.tukey_stats <- function(diff, se, n_groups, df, conf.level) {
  # t-statistic
  t_stat <- diff / se

  # Critical value from studentized range distribution
  q_crit <- qtukey(conf.level, n_groups, df) / sqrt(2)

  # Confidence interval
  margin <- q_crit * se
  conf_low <- diff - margin
  conf_high <- diff + margin

  # Adjusted p-value using studentized range distribution
  # Convert t-statistic to q-statistic: q = t * sqrt(2)
  q_stat <- abs(t_stat) * sqrt(2)
  p_adjusted <- ptukey(q_stat, n_groups, df, lower.tail = FALSE)

  list(
    stat = t_stat,
    conf_low = conf_low,
    conf_high = conf_high,
    p_adjusted = p_adjusted
  )
}

#' Scheffe critical-value / p-value / CI core
#'
#' Scheffe F-statistic with (k-1)*F critical value for the CI margin.
#'
#' @param diff Mean difference (group1 - group2)
#' @param se Standard error of the difference
#' @param mse Pooled within-group mean square error
#' @param eff_n1,eff_n2 Effective group sample sizes
#' @param k Number of groups
#' @param df2 Error degrees of freedom (pooled weighted df)
#' @param conf.level Confidence level
#' @return List with stat (F), conf_low, conf_high, p_adjusted
#' @noRd
.scheffe_stats <- function(diff, se, mse, eff_n1, eff_n2, k, df2, conf.level) {
  # Scheffe F-statistic
  # F = (mean_diff^2) / ((k-1) * MSE * (1/n1 + 1/n2))
  f_stat <- (diff^2) / ((k - 1) * mse * (1/eff_n1 + 1/eff_n2))

  # Degrees of freedom
  df1 <- k - 1

  # P-value from F-distribution
  p_value <- pf(f_stat, df1, df2, lower.tail = FALSE)

  # Critical value for Scheffe test
  # S = sqrt((k-1) * F_critical)
  f_critical <- qf(conf.level, df1, df2)
  s_critical <- sqrt((k - 1) * f_critical)

  # Confidence interval
  margin <- s_critical * se
  conf_low <- diff - margin
  conf_high <- diff + margin

  list(
    stat = f_stat,
    conf_low = conf_low,
    conf_high = conf_high,
    p_adjusted = p_value
  )
}

#' Pairwise post-hoc test for a single variable
#'
#' Shared scaffolding (NA removal, group levels, weighted group statistics,
#' pooled MSE) with method-specific cores. The unweighted Tukey path
#' delegates to stats::TukeyHSD(), exactly as the original implementation.
#'
#' @param data Data frame
#' @param var_name Dependent variable name
#' @param group_name Grouping factor name
#' @param weight_name Optional weights variable name
#' @param method "tukey" or "scheffe"
#' @param conf.level Confidence level
#' @return data.frame of pairwise comparisons, or NULL if < 2 groups
#' @noRd
.pairwise_posthoc_single <- function(data, var_name, group_name,
                                     weight_name = NULL,
                                     method = c("tukey", "scheffe"),
                                     conf.level = 0.95) {
  method <- match.arg(method)

  # Get the variable values
  y <- data[[var_name]]
  g <- data[[group_name]]

  # Remove NA values
  valid_indices <- !is.na(y) & !is.na(g)
  if (!is.null(weight_name)) {
    w <- data[[weight_name]]
    valid_indices <- valid_indices & !is.na(w) & w > 0
    w <- w[valid_indices]
  }
  y <- y[valid_indices]
  g <- g[valid_indices]

  # Get group levels
  if (is.factor(g)) {
    group_levels <- levels(g)[levels(g) %in% unique(g)]
  } else {
    group_levels <- unique(g)
  }

  if (length(group_levels) < 2) {
    return(NULL)
  }

  # Unweighted Tukey HSD: delegate to stats::TukeyHSD (verbatim original path)
  if (method == "tukey" && is.null(weight_name)) {
    aov_result <- aov(y ~ g)
    tukey_result <- TukeyHSD(aov_result, conf.level = conf.level)

    # Extract results
    tukey_data <- tukey_result$g
    comparisons <- rownames(tukey_data)

    results_df <- data.frame(
      Variable = var_name,
      Comparison = comparisons,
      Estimate = tukey_data[, "diff"],
      conf_low = tukey_data[, "lwr"],
      conf_high = tukey_data[, "upr"],
      p_adjusted = tukey_data[, "p adj"],
      stringsAsFactors = FALSE
    )

    return(results_df)
  }

  # Calculate group statistics
  if (is.null(weight_name)) {
    # Unweighted statistics (Scheffe only; Tukey returned above)
    group_stats <- lapply(group_levels, function(level) {
      group_indices <- g == level
      group_data <- y[group_indices]

      n <- length(group_data)
      mean_val <- mean(group_data, na.rm = TRUE)
      var_val <- var(group_data, na.rm = TRUE)

      list(
        level = level,
        n = n,
        eff_n = n,
        mean = mean_val,
        var = var_val
      )
    })
  } else {
    # Weighted statistics (byte-identical between Tukey and Scheffe originals)
    group_stats <- lapply(group_levels, function(level) {
      group_indices <- g == level
      group_data <- y[group_indices]
      group_weights <- w[group_indices]

      n <- length(group_data)
      weighted_n <- sum(group_weights)

      # For SPSS compatibility: Use sum of weights as sample size
      # SPSS treats frequency weights as literal counts
      eff_n <- sum(group_weights)

      # Weighted mean and variance (SPSS formula)
      weighted_mean <- sum(group_data * group_weights) / sum(group_weights)
      # SPSS uses Bessel's correction: divide by (sum(weights) - 1)
      weighted_var <- sum(group_weights * (group_data - weighted_mean)^2) / (sum(group_weights) - 1)

      list(
        level = level,
        n = n,
        eff_n = eff_n,
        weighted_n = weighted_n,
        mean = weighted_mean,
        var = weighted_var
      )
    })
  }
  names(group_stats) <- group_levels

  # Calculate MSE (Mean Square Error) using SPSS approach:
  # pooled within-group variance from the weighted group statistics
  total_weighted_ss <- 0
  total_weighted_df <- 0

  for (level in group_levels) {
    stat <- group_stats[[level]]
    # Weight the sum of squares by effective sample size
    ss_within <- (stat$eff_n - 1) * stat$var
    total_weighted_ss <- total_weighted_ss + ss_within
    total_weighted_df <- total_weighted_df + (stat$eff_n - 1)
  }

  mse <- total_weighted_ss / total_weighted_df

  # Number of groups
  n_groups <- length(group_levels)

  # Generate all pairwise comparisons
  comparisons <- combn(group_levels, 2, simplify = FALSE)

  results_list <- lapply(comparisons, function(pair) {
    g1 <- pair[1]
    g2 <- pair[2]

    stat1 <- group_stats[[g1]]
    stat2 <- group_stats[[g2]]

    # Mean difference
    diff <- stat1$mean - stat2$mean

    # Standard error using SPSS formula: SE = sqrt(MSE * (1/n1 + 1/n2))
    se <- sqrt(mse * (1/stat1$eff_n + 1/stat2$eff_n))

    if (method == "tukey") {
      core <- .tukey_stats(diff, se, n_groups, total_weighted_df, conf.level)

      data.frame(
        Variable = var_name,
        Comparison = paste(g1, "-", g2),
        Estimate = diff,
        SE = se,
        t_value = core$stat,
        conf_low = core$conf_low,
        conf_high = core$conf_high,
        p_adjusted = core$p_adjusted,
        stringsAsFactors = FALSE
      )
    } else {
      core <- .scheffe_stats(diff, se, mse, stat1$eff_n, stat2$eff_n,
                             n_groups, total_weighted_df, conf.level)

      data.frame(
        Variable = var_name,
        Comparison = paste(g1, "-", g2),
        Estimate = diff,
        SE = se,
        F_value = core$stat,
        conf_low = core$conf_low,
        conf_high = core$conf_high,
        p_adjusted = core$p_adjusted,
        stringsAsFactors = FALSE
      )
    }
  })

  results_df <- do.call(rbind, results_list)
  return(results_df)
}

#' Pairwise post-hoc engine for oneway_anova results
#'
#' Shared per-variable / per-group execution behind
#' tukey_test.oneway_anova() and scheffe_test.oneway_anova().
#'
#' @param x A oneway_anova result object
#' @param method "tukey" or "scheffe"
#' @param conf.level Confidence level for intervals
#' @return Object of class "tukey_test" or "scheffe_test"
#' @noRd
.pairwise_posthoc <- function(x, method = c("tukey", "scheffe"),
                              conf.level = 0.95) {
  method <- match.arg(method)
  method_label <- switch(method, tukey = "Tukey", scheffe = "Scheffe")

  # Input validation
  if (conf.level <= 0 || conf.level >= 1) {
    cli_abort("{.arg conf.level} must be between 0 and 1.")
  }

  # Main execution logic
  all_results <- list()

  if (x$is_grouped) {
    # Get original data and split by groups
    data <- x$data
    data_list <- dplyr::group_split(data)
    group_keys <- dplyr::group_keys(data)

    # Perform post-hoc test for each group
    for (i in seq_along(data_list)) {
      group_data <- data_list[[i]]
      group_info <- group_keys[i, , drop = FALSE]

      # Perform test for each variable in this group
      for (var_name in x$variables) {
        tryCatch({
          single_result <- .pairwise_posthoc_single(
            group_data, var_name, x$group, x$weights, method, conf.level
          )

          if (!is.null(single_result)) {
            # Add group information - repeat group_info for each result row
            n_rows <- nrow(single_result)
            group_info_expanded <- group_info[rep(1, n_rows), , drop = FALSE]
            result_with_groups <- cbind(group_info_expanded, single_result)
            all_results <- append(all_results, list(result_with_groups))
          }
        }, error = function(e) {
          cli_warn("{method_label} test failed for variable {.var {var_name}} in group {paste(unlist(group_info), collapse = ', ')}: {e$message}")
        })
      }
    }

  } else {
    # Perform test for each variable (ungrouped)
    for (var_name in x$variables) {
      tryCatch({
        single_result <- .pairwise_posthoc_single(
          x$data, var_name, x$group, x$weights, method, conf.level
        )

        if (!is.null(single_result)) {
          all_results <- append(all_results, list(single_result))
        }
      }, error = function(e) {
        cli_warn("{method_label} test failed for variable {.var {var_name}}: {e$message}")
      })
    }
  }

  # Combine all results
  if (length(all_results) > 0) {
    results_df <- do.call(rbind, all_results)
  } else {
    results_df <- data.frame()
  }

  # Create S3 object
  structure(
    list(
      results = results_df,
      variables = x$variables,
      group = x$group,
      weights = x$weights,
      groups = x$groups,
      is_grouped = x$is_grouped,
      conf.level = conf.level,
      anova_results = x  # Store original ANOVA results
    ),
    class = paste0(method, "_test")
  )
}

# =============================================================================
# Shared print implementation
# =============================================================================

#' Render one pairwise-comparison table
#'
#' Single table renderer used by all three display branches (factorial,
#' grouped, ungrouped) that were previously pasted three times per print
#' method.
#'
#' @param rows Result rows containing Comparison/Estimate/conf_low/
#'   conf_high/p_adjusted/sig
#' @param digits Decimal places
#' @return invisible(NULL)
#' @noRd
.print_posthoc_table <- function(rows, digits) {
  tbl <- rows[, c("Comparison", "Estimate", "conf_low",
                  "conf_high", "p_adjusted", "sig")]
  format_stat_table(
    tbl,
    digits = digits,
    col_types = c(Estimate = "num", conf_low = "num", conf_high = "num"),
    col_labels = c(Estimate = "Difference", conf_low = "Lower CI",
                   conf_high = "Upper CI", p_adjusted = "p-value",
                   sig = "Sig")
  )
  invisible(NULL)
}

#' Shared print implementation for tukey_test / scheffe_test objects
#'
#' Parameterized by the method-specific title, notes, results label, and
#' interpretation lines; everything else (factorial branch, grouped branch,
#' ungrouped branch, empty-results message, significance legend) is common.
#'
#' @param x A "tukey_test" or "scheffe_test" object
#' @param digits Decimal places
#' @param title Base title, e.g. "Tukey HSD Post-Hoc Test"
#' @param method_notes Character vector of note lines printed after the
#'   test parameters (indented, one per line)
#' @param results_label Label used in per-variable blocks, e.g. "Tukey"
#'   ("Tukey Results" / "Weighted Tukey Results")
#' @param interpretation_notes Character vector of method-specific
#'   interpretation lines appended after the shared ones
#' @return invisible(x)
#' @noRd
.print_pairwise_posthoc <- function(x, digits = 3, title, method_notes,
                                    results_label, interpretation_notes) {
  # Determine test type using standardized helper
  weights_name <- x$weights
  test_type <- get_standard_title(title, weights_name, "Results")
  print_header(test_type, newline_before = FALSE)

  print_method_notes <- function() {
    for (note in method_notes) {
      cat(sprintf("  %s\n", note))
    }
    cat("\n")
  }

  print_interpretation <- function() {
    cat("\nInterpretation:\n")
    cat("- Positive differences: First group > Second group\n")
    cat("- Negative differences: First group < Second group\n")
    cat("- Confidence intervals not containing 0 indicate significant differences\n")
    for (line in interpretation_notes) {
      cat(sprintf("%s\n", line))
    }
  }

  # --- Factorial ANOVA post-hoc: results grouped by Factor column -----------
  if (isTRUE(x$is_factorial)) {
    cat("\n")
    test_info <- list(
      "Dependent variable" = x$call_info$dv,
      "Factors" = x$group,
      "Weights variable" = x$weights
    )
    print_info_section(test_info)
    test_params <- list(conf.level = x$conf.level)
    print_test_parameters(test_params)
    print_method_notes()

    x$results$sig <- sapply(x$results$p_adjusted, add_significance_stars)

    for (fct in unique(x$results$Factor)) {
      cat(sprintf("\n--- Factor: %s ---\n\n", fct))
      .print_posthoc_table(x$results[x$results$Factor == fct, ], digits)
    }

    print_significance_legend()
    print_interpretation()
    return(invisible(x))
  }

  # Print basic info using standardized helpers
  cat("\n")
  test_info <- list(
    "Dependent variable" = if (!x$is_grouped && length(x$variables) == 1) x$variables[1] else NULL,
    "Grouping variable" = x$group,
    "Weights variable" = x$weights
  )
  print_info_section(test_info)
  test_params <- list(conf.level = x$conf.level)
  print_test_parameters(test_params)
  print_method_notes()

  if (nrow(x$results) == 0) {
    cat("No post-hoc comparisons available.\n")
    cat("This may occur when:\n")
    cat("- ANOVA was not significant\n")
    cat("- Groups have insufficient sample sizes\n")
    cat("- Data contains only missing values\n")
    return(invisible(x))
  }

  # Add significance stars using standardized helper
  x$results$sig <- sapply(x$results$p_adjusted, add_significance_stars)

  # Print each variable as separate block (shared by grouped and ungrouped)
  print_variable_blocks <- function(rows) {
    for (var in x$variables) {
      var_results <- rows[rows$Variable == var, , drop = FALSE]
      if (nrow(var_results) == 0) next

      cat(sprintf("\n--- %s ---\n", var))
      cat("\n")  # Add blank line after variable name

      cat(sprintf("%s:\n", ifelse(!is.null(x$weights),
                                  paste("Weighted", results_label, "Results"),
                                  paste(results_label, "Results"))))
      .print_posthoc_table(var_results, digits)
      cat("\n")
    }
  }

  if (isTRUE(x$is_grouped)) {
    for_each_group(x$results, x$groups, function(rows, group_values) {
      # Print group header using standardized helper
      print_group_header(group_values)
      print_variable_blocks(rows)
    }, header = FALSE)
  } else {
    print_variable_blocks(x$results)
  }

  print_significance_legend()
  print_interpretation()

  invisible(x)
}
