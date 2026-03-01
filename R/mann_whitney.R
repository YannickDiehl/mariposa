
#' Compare Two Groups Without Assuming Normal Data
#'
#' @description
#' \code{mann_whitney()} compares two groups when your data isn't normally
#' distributed or when you have ordinal data (like ratings or rankings). It's the
#' go-to alternative when t-tests aren't appropriate.
#'
#' Think of it as:
#' - A robust way to compare groups that works with any data shape
#' - Perfect for Likert scales, ratings, or skewed distributions
#' - A test that compares the typical values between groups
#'
#' The test tells you:
#' - Whether one group tends to have higher values than the other
#' - How strong the difference is (effect size)
#' - Which group has higher average ranks
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The variables you want to compare between groups. You can list
#'   multiple variables or use helpers like \code{starts_with("satisfaction")}
#' @param group The categorical variable that defines your two groups (e.g., gender,
#'   treatment/control). Must have exactly two groups.
#' @param weights Optional survey weights for population-representative results
#' @param mu The hypothesized difference (Default: 0, meaning no difference)
#' @param alternative Direction of the test:
#'   \itemize{
#'     \item \code{"two.sided"} (default): Test if groups are different
#'     \item \code{"greater"}: Test if group 1 > group 2
#'     \item \code{"less"}: Test if group 1 < group 2
#'   }
#' @param conf.level Confidence level for intervals (Default: 0.95 = 95%)
#'
#' @return Test results showing whether groups differ, including:
#' - U and W statistics (test statistics)
#' - Z-score and p-value (are groups different?)
#' - Effect size r (how big is the difference?)
#' - Rank means for each group (which group is higher?)
#'
#' @details
#' ## Understanding the Results
#'
#' **P-value**: If p < 0.05, the groups are significantly different
#' - p < 0.001: Very strong evidence of difference
#' - p < 0.01: Strong evidence of difference
#' - p < 0.05: Moderate evidence of difference
#' - p ≥ 0.05: No significant difference found
#'
#' **Effect Size r** (How big is the difference?):
#' - |r| < 0.1: Negligible difference
#' - |r| ~ 0.1: Small difference
#' - |r| ~ 0.3: Medium difference
#' - |r| ~ 0.5: Large difference
#' - |r| > 0.5: Very large difference
#'
#' **Rank Mean Difference**:
#' - Positive: Group 1 tends to have higher values
#' - Negative: Group 2 tends to have higher values
#' - Zero: Groups have similar distributions
#'
#' ## When to Use This
#'
#' Use Mann-Whitney test when:
#' - Your data is not normally distributed (skewed, outliers)
#' - You have ordinal data (rankings, Likert scales)
#' - Sample sizes are small (< 30 per group)
#' - You want a robust alternative to the t-test
#' - You're comparing satisfaction ratings, income, or other skewed variables
#'
#' ## Advantages Over t-test
#'
#' - Works with any data distribution
#' - Not affected by outliers
#' - Valid for ordinal data
#' - No assumptions about variance
#' - More robust for real-world data
#'
#' ## Tips for Success
#'
#' - Each group should have at least 5 observations
#' - The test compares distributions, not just means
#' - Look at both p-values and effect sizes
#' - Consider plotting the data to see the pattern
#' - Use this for Likert scales and rating data
#'
#' @seealso
#' \code{\link[stats]{wilcox.test}} for the base R Wilcoxon test function.
#' 
#' \code{\link[survey]{svyranktest}} for survey-weighted rank tests.
#' 
#' \code{\link{t_test}} for parametric t-tests.
#' 
#' @references
#' Mann, H. B., & Whitney, D. R. (1947). On a test of whether one of two random 
#' variables is stochastically larger than the other. The Annals of Mathematical 
#' Statistics, 18(1), 50-60.
#' 
#' Wilcoxon, F. (1945). Individual comparisons by ranking methods. Biometrics
#' Bulletin, 1(6), 80-83.
#'
#' Lumley, T., & Scott, A. (2013). Two-sample rank tests under complex sampling.
#' Biometrika, 100(4), 831-842.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#' 
#' # Basic Mann-Whitney test (non-parametric comparison)
#' survey_data %>%
#'   mann_whitney(age, group = gender)
#' 
#' # Multiple variables
#' survey_data %>%
#'   mann_whitney(age, income, life_satisfaction, group = region)
#' 
#' # Using tidyselect helpers
#' survey_data %>%
#'   mann_whitney(starts_with("trust_"), group = gender)
#' 
#' # Weighted analysis
#' survey_data %>%
#'   mann_whitney(income, group = region, weights = sampling_weight)
#' 
#' # Grouped analysis (separate tests for each education level)
#' survey_data %>%
#'   group_by(education) %>%
#'   mann_whitney(life_satisfaction, group = gender)
#' 
#' # One-sided test
#' survey_data %>%
#'   mann_whitney(life_satisfaction, group = region, alternative = "greater")
#' 
#' # Store results for further analysis
#' result <- survey_data %>%
#'   mann_whitney(income, group = gender, weights = sampling_weight)
#' print(result)
#'
#' @family hypothesis_tests
#' @export
mann_whitney <- function(data, ..., group, weights = NULL, mu = 0, 
                             alternative = c("two.sided", "less", "greater"),
                             conf.level = 0.95) {
  
  # Input validation
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }
  
  alternative <- match.arg(alternative)
  
  # Check if data is grouped
  is_grouped <- inherits(data, "grouped_df")
  grp_vars <- if (is_grouped) dplyr::group_vars(data) else NULL
  
  # Select variables using centralized helper
  vars <- .process_variables(data, ...)
  var_names <- names(vars)

  # Process group variable (required for Mann-Whitney test)
  group_quo <- enquo(group)
  if (quo_is_null(group_quo)) {
    cli_abort("{.arg group} is required for Mann-Whitney test.")
  }

  g_var <- eval_select(expr(!!group_quo), data = data)
  g_name <- names(g_var)[1]  # Take first name

  # Process weights using centralized helper
  weights_info <- .process_weights(data, rlang::enquo(weights))
  w_name <- weights_info$name

  # Helper function to perform Mann-Whitney test for a single variable
  perform_single_mann_whitney <- function(data, var_name, group_name, weight_name = NULL) {
    # Get the variable values
    x <- data[[var_name]]
    g <- data[[group_name]]
    
    # Remove NA values
    valid_indices <- !is.na(x) & !is.na(g)
    if (!is.null(weight_name)) {
      w <- data[[weight_name]]
      valid_indices <- valid_indices & !is.na(w)
      w <- w[valid_indices]
    }
    x <- x[valid_indices]
    g <- g[valid_indices]
    
    # Get unique levels preserving original order (SPSS convention)
    if (is.factor(g)) {
      all_levels <- levels(g)
      g_levels <- all_levels[all_levels %in% unique(g)]
    } else {
      # For non-factors, sort to ensure consistency
      g_levels <- sort(unique(g))
    }
    
    if (length(g_levels) != 2) {
      cli_abort(c(
        "Mann-Whitney test requires exactly 2 groups.",
        "x" = "Found {length(g_levels)} group{?s} in variable {.var {group_name}}.",
        "i" = "For >2 groups, use a Kruskal-Wallis test instead."
      ))
    }
    
    # Split data by groups
    x1 <- x[g == g_levels[1]]
    x2 <- x[g == g_levels[2]]
    w1 <- if (!is.null(weight_name)) w[g == g_levels[1]] else NULL
    w2 <- if (!is.null(weight_name)) w[g == g_levels[2]] else NULL
    
    if (is.null(weight_name)) {
      # Calculate sample sizes
      n1 <- length(x1)
      n2 <- length(x2)
      
      # Manual rank calculation (more reliable than wilcox.test for our purposes)
      all_values <- c(x1, x2)
      all_ranks <- rank(all_values)
      
      # Calculate rank sums
      R1 <- sum(all_ranks[1:n1])  # Rank sum of group 1
      R2 <- sum(all_ranks[(n1+1):(n1+n2)])  # Rank sum of group 2
      
      # Rank means
      rank_mean1 <- R1 / n1
      rank_mean2 <- R2 / n2
      
      # Calculate U statistics
      U1 <- R1 - n1 * (n1 + 1) / 2  # U for group 1
      U2 <- R2 - n2 * (n2 + 1) / 2  # U for group 2
      
      # Use the smaller U as the Mann-Whitney U statistic (standard convention)
      U <- min(U1, U2)
      
      # W is the rank sum of the group with smaller U (SPSS convention)
      W_report <- if(U1 < U2) R1 else R2
      
      # Calculate Z statistic using U with continuity correction
      expected_U <- n1 * n2 / 2
      
      # Check for ties and calculate corrected variance (SPSS method)
      tie_groups <- table(all_values)
      tie_correction <- sum(tie_groups^3 - tie_groups)
      
      # Variance with tie correction (matching SPSS calculation)
      var_U <- n1 * n2 * ((n1 + n2 + 1) - tie_correction / ((n1 + n2) * (n1 + n2 - 1))) / 12
      
      # Calculate Z without continuity correction (to match SPSS exactly)
      Z <- (U - expected_U) / sqrt(var_U)
      
      # Effect size
      r <- abs(Z) / sqrt(n1 + n2)
      
      # Get p-value from wilcox.test for accuracy
      test_result <- wilcox.test(x1, x2, alternative = alternative,
                               mu = mu, exact = FALSE, conf.int = TRUE,
                               conf.level = conf.level)
      p_value <- test_result$p.value
      
      group_stats <- list(
        group1 = list(name = as.character(g_levels[1]), rank_mean = rank_mean1, n = n1),
        group2 = list(name = as.character(g_levels[2]), rank_mean = rank_mean2, n = n2)
      )
      
    } else {
      # ---------------------------------------------------------------
      # Weighted: Lumley & Scott (2013) design-based rank test
      # ---------------------------------------------------------------
      # Uses Horvitz-Thompson midranks + WLS regression + sandwich
      # variance. Validated against survey::svyranktest() to machine
      # precision.
      # ---------------------------------------------------------------

      n <- length(x)

      # 1. Weighted midranks (Horvitz-Thompson estimator)
      ii <- order(x)
      N_pop <- sum(w)
      rankhat <- numeric(n)
      rankhat[ii] <- ave(cumsum(w[ii]) - w[ii] / 2, factor(x[ii]))
      rankscore <- rankhat / N_pop

      # 2. WLS regression: rankscore ~ group
      g_numeric <- as.numeric(g == g_levels[2])
      xmat <- cbind(1, g_numeric)
      XtWX_inv <- solve(crossprod(xmat, w * xmat))
      beta <- XtWX_inv %*% crossprod(xmat, w * rankscore)
      residuals_vec <- as.vector(rankscore - xmat %*% beta)

      # 3. Influence function + sandwich variance
      infn <- (xmat * residuals_vec) %*% XtWX_inv
      V <- n * var(w * infn)

      # 4. t-test on the group coefficient
      t_stat <- as.numeric(beta[2]) / sqrt(V[2, 2])
      df_t <- n - 2

      # 5. p-value respecting alternative hypothesis
      if (alternative == "two.sided") {
        p_value <- 2 * pt(-abs(t_stat), df = df_t)
      } else if (alternative == "less") {
        p_value <- pt(t_stat, df = df_t, lower.tail = FALSE)
      } else {
        p_value <- pt(t_stat, df = df_t)
      }

      # 6. Weighted group statistics
      w1 <- w[g == g_levels[1]]
      w2 <- w[g == g_levels[2]]
      rankhat1 <- rankhat[g == g_levels[1]]
      rankhat2 <- rankhat[g == g_levels[2]]

      n1_weighted <- sum(w1)
      n2_weighted <- sum(w2)
      rank_mean1 <- sum(w1 * rankhat1) / n1_weighted
      rank_mean2 <- sum(w2 * rankhat2) / n2_weighted

      # 7. Descriptive U and W from weighted ranks
      R1 <- sum(w1 * rankhat1)
      R2 <- sum(w2 * rankhat2)
      U1 <- R1 - n1_weighted * (n1_weighted + 1) / 2
      U2 <- R2 - n2_weighted * (n2_weighted + 1) / 2
      U <- min(U1, U2)
      W_report <- if (U1 < U2) R1 else R2

      # 8. Z = t-statistic (asymptotically equivalent)
      Z <- t_stat

      # 9. Effect size: r = |t| / sqrt(t^2 + df)
      r <- abs(t_stat) / sqrt(t_stat^2 + df_t)

      group_stats <- list(
        group1 = list(name = as.character(g_levels[1]),
                      rank_mean = rank_mean1,
                      n = round(n1_weighted, 1)),
        group2 = list(name = as.character(g_levels[2]),
                      rank_mean = rank_mean2,
                      n = round(n2_weighted, 1))
      )
    }

    return(list(
      U = U,
      W = W_report,
      Z = Z,
      p_value = p_value,
      effect_size_r = r,
      rank_mean_diff = rank_mean1 - rank_mean2,
      group_levels = g_levels,
      group_stats = group_stats
    ))
  }

  # Main computation function
  compute_results <- function(data) {
    results_list <- list()
    
    for (var_name in var_names) {
      tryCatch({
        result <- perform_single_mann_whitney(data, var_name, g_name, w_name)
        
        results_list[[var_name]] <- tibble(
          Variable = var_name,
          U = result$U,
          W = result$W,
          Z = result$Z,
          p_value = result$p_value,
          effect_size_r = result$effect_size_r,
          rank_mean_diff = result$rank_mean_diff,
          group_stats = list(result$group_stats)
        )
        
      }, error = function(e) {
        cli_warn("Mann-Whitney test failed for variable {.var {var_name}}: {e$message}")
        results_list[[var_name]] <- tibble(
          Variable = var_name,
          U = NA_real_,
          W = NA_real_,
          Z = NA_real_,
          p_value = NA_real_,
          effect_size_r = NA_real_,
          rank_mean_diff = NA_real_,
          group_stats = list(NULL)
        )
      })
    }
    
    bind_rows(results_list)
  }

  # Execute computation
  if (is_grouped) {
    results <- data %>%
      group_modify(~ compute_results(.x))
  } else {
    results <- compute_results(data)
  }

  # Get group levels for output
  if (!is.null(g_name)) {
    group_col <- data[[g_name]]
    if (is.factor(group_col)) {
      all_levels <- levels(group_col)
      group_levels <- all_levels[all_levels %in% unique(group_col)]
    } else {
      group_levels <- unique(group_col)
    }
  } else {
    group_levels <- NULL
  }

  # Create result object
  result <- list(
    results = results,
    variables = var_names,
    group = g_name,
    weights = w_name,
    group_levels = group_levels,
    is_grouped = is_grouped,
    mu = mu,
    alternative = alternative,
    conf.level = conf.level,
    data = data[, unique(c(var_names, g_name, w_name, grp_vars)), drop = FALSE]
  )
  
  class(result) <- "mann_whitney"
  return(result)
}

# Helper: print a single variable block (rank means + test table)
#' @keywords internal
.print_mw_variable_block <- function(var_name, row_data, stats, weights, digits) {
  cat(var_name, "\n", sep = "")
  cat(paste(rep("-", nchar(var_name)), collapse = ""), "\n", sep = "")
  cat("\n")

  # Print group rank means
  if (!is.null(stats) && !is.null(stats$group1)) {
    cat(sprintf("  %s: rank mean = %.1f, n = %.1f\n",
                stats$group1$name, stats$group1$rank_mean, stats$group1$n))
    cat(sprintf("  %s: rank mean = %.1f, n = %.1f\n",
                stats$group2$name, stats$group2$rank_mean, stats$group2$n))
    cat("\n")
  }

  # Build results table
  results_df <- data.frame(
    Test = "Mann-Whitney U",
    U = ifelse(is.na(row_data$U), "NA",
               format(round(row_data$U, 0), big.mark = ",")),
    W = ifelse(is.na(row_data$W), "NA",
               format(round(row_data$W, 0), big.mark = ",")),
    Z = round(row_data$Z, digits),
    p_value = round(row_data$p_value, digits),
    effect_r = round(row_data$effect_size_r, digits),
    sig = row_data$sig,
    stringsAsFactors = FALSE
  )

  # Dynamic border width from actual table content
  col_widths <- sapply(names(results_df), function(col) {
    max(nchar(as.character(results_df[[col]])), nchar(col), na.rm = TRUE)
  })
  total_width <- sum(col_widths) + length(col_widths) - 1
  border <- paste(rep("-", total_width), collapse = "")

  label <- if (!is.null(weights)) "Weighted Mann-Whitney U Test Results" else "Mann-Whitney U Test Results"
  cat(sprintf("\n%s:\n", label))
  cat(border, "\n")
  print(results_df, row.names = FALSE)
  cat(border, "\n\n")
}

#' Print method for Mann-Whitney test results
#'
#' @param x A mann_whitney object
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments (not used)
#' @export
#' @method print mann_whitney
print.mann_whitney <- function(x, digits = 3, ...) {

  # Determine test type using standardized helper
  weights_name <- x$weights
  test_type <- get_standard_title("Mann-Whitney U Test", weights_name, "Results")
  print_header(test_type)

  # Ensure p-values are numeric
  x$results$p_value <- as.numeric(x$results$p_value)

  # Add significance stars using standard helper
  x$results$sig <- sapply(x$results$p_value, add_significance_stars)

  # Template Standard: Dual grouped data detection
  is_grouped_data <- isTRUE(x$is_grouped)

  # Print info about grouping variable if present
  if (!is.null(x$group)) {
    group_levels <- x$group_levels
    if (length(group_levels) >= 2) {
      cat("\n")
      test_info <- list(
        "Grouping variable" = x$group,
        "Groups compared" = sprintf("%s vs. %s", as.character(group_levels[1]), as.character(group_levels[2])),
        "Weights variable" = weights_name
      )
      print_info_section(test_info)
      print_test_parameters(list(
        mu = x$mu,
        alternative = x$alternative,
        conf.level = x$conf.level
      ))
      cat("\n")
    }
  }

  if (is_grouped_data) {
    # Get unique groups
    group_vars <- setdiff(names(x$results), c("Variable", "U", "W", "Z", "p_value",
                                               "effect_size_r", "rank_mean_diff", "group_stats", "sig"))
    groups <- unique(x$results[group_vars])

    for (i in seq_len(nrow(groups))) {
      group_values <- groups[i, , drop = FALSE]
      print_group_header(group_values)

      # Filter results for current group
      group_results <- x$results
      for (g in names(group_values)) {
        group_results <- group_results[group_results[[g]] == group_values[[g]], ]
      }
      group_results <- group_results[!is.na(group_results$Variable), ]
      if (nrow(group_results) == 0) next

      for (j in seq_len(nrow(group_results))) {
        .print_mw_variable_block(
          var_name  = group_results$Variable[j],
          row_data  = group_results[j, ],
          stats     = group_results$group_stats[[j]],
          weights   = x$weights,
          digits    = digits
        )
      }
    }
  } else {
    valid_results <- x$results[!is.na(x$results$Variable), ]

    for (i in seq_len(nrow(valid_results))) {
      .print_mw_variable_block(
        var_name  = valid_results$Variable[i],
        row_data  = valid_results[i, ],
        stats     = valid_results$group_stats[[i]],
        weights   = x$weights,
        digits    = digits
      )
    }
  }

  if (!is.null(x$weights)) {
    cat("\nNote: Weighted analysis uses design-based rank test (Lumley & Scott, 2013).\n")
    cat("U and W are descriptive statistics derived from weighted ranks.\n")
  }

  print_significance_legend()

  cat("\nEffect Size Interpretation (r):\n")
  cat("- Small effect: |r| ~ 0.1\n")
  cat("- Medium effect: |r| ~ 0.3\n")
  cat("- Large effect: |r| ~ 0.5\n")

  invisible(x)
}



