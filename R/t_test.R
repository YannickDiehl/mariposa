
#' Test If Two Groups Differ
#'
#' @description
#' \code{t_test()} helps you determine if two groups have different average values.
#' For example, do men and women have different satisfaction scores? Does Region A
#' spend more than Region B? Is this year better than last year?
#'
#' The test tells you:
#' - **Whether the difference is statistically significant**
#' - **How big the difference is** (effect sizes)
#' - **The likely range of the true difference** (confidence interval)
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The numeric variables to test. List multiple variables or use
#'   tidyselect helpers like \code{starts_with("trust")}.
#' @param group The variable that defines your two groups (e.g., gender, region).
#'   Must have exactly two categories. Leave empty for one-sample tests.
#' @param weights Optional survey weights for population-representative results
#' @param var.equal Should we assume equal variances? (Default: FALSE)
#'   \itemize{
#'     \item \code{FALSE}: Safer, works even if groups vary differently (Welch's test)
#'     \item \code{TRUE}: Traditional approach, assumes equal spread (Student's test)
#'   }
#' @param mu Comparison value (Default: 0). For two-sample tests, usually 0.
#'   For one-sample tests, your benchmark value.
#' @param alternative Direction of the test:
#'   \itemize{
#'     \item \code{"two.sided"} (default): Any difference
#'     \item \code{"greater"}: First group is higher
#'     \item \code{"less"}: First group is lower
#'   }
#' @param conf.level Confidence level (Default: 0.95 = 95%)
#'
#' @return Test results showing whether groups differ, including:
#' - t-statistic and p-value for statistical significance
#' - Mean difference and confidence interval
#' - Effect sizes (Cohen's d, Hedges' g, Glass' Delta)
#' - Group statistics (mean, SD, sample size)
#'
#' @details
#' ## Understanding the Results
#'
#' **P-value**: Is the difference statistically significant?
#' - p < 0.001: Very strong evidence of a difference
#' - p < 0.01: Strong evidence of a difference
#' - p < 0.05: Moderate evidence of a difference
#' - p ≥ 0.05: No significant difference found
#'
#' **Effect Sizes** (How big is the difference?):
#' - **Cohen's d**: The standard measure
#'   - |d| < 0.2: Negligible difference
#'   - |d| = 0.2-0.5: Small difference
#'   - |d| = 0.5-0.8: Medium difference
#'   - |d| > 0.8: Large difference
#' - **Hedges' g**: Corrected for small samples
#' - **Glass' Delta**: Uses control group SD only
#'
#' **Confidence Interval**: Range where the true difference likely falls
#' - If it includes 0, groups may not differ
#' - Width indicates precision (narrower = more precise)
#'
#' ## When to Use This
#'
#' Use t-test when:
#' - You have exactly two groups to compare
#' - Your outcome variable is numeric (continuous)
#' - Groups are independent (different people in each)
#' - Data is roughly normally distributed (or n ≥ 30 per group)
#'
#' Don't use when:
#' - You have more than two groups (use ANOVA)
#' - Data is severely skewed with small samples (use Mann-Whitney)
#' - Groups are paired/matched (use paired t-test - coming soon)
#' - Variables are categorical (use chi-square)
#'
#' ## Reading the Output
#'
#' The results show both variance assumptions:
#' - **Welch's test** (var.equal = FALSE): Safer, doesn't assume equal variances
#' - **Student's test** (var.equal = TRUE): Traditional, assumes equal variances
#'
#' A result with t = 2.45, p = 0.015, d = 0.62 means:
#' - Groups are 2.45 standard errors apart (t-statistic)
#' - 1.5% chance this is due to random variation (p-value)
#' - Medium-sized practical difference (Cohen's d)
#'
#' ## Tips for Success
#'
#' - Always check sample sizes (aim for 30+ per group)
#' - Consider both statistical significance AND effect size
#' - Use weights for population-level conclusions
#' - Plot your data first to check assumptions
#' - Report confidence intervals along with p-values
#'
#' @seealso 
#' \code{\link[stats]{t.test}} for the base R t-test function.
#' 
#' \code{\link{print.t_test}} for printing results.
#' 
#' \code{\link[dplyr]{group_by}} for grouped analyses.
#' 
#' @references
#' Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). 
#' Lawrence Erlbaum Associates.
#' 
#' Hedges, L. V. (1981). Distribution theory for Glass's estimator of effect size 
#' and related estimators. Journal of Educational Statistics, 6(2), 107-128.
#' 
#' Glass, G. V. (1976). Primary, secondary, and meta-analysis of research. 
#' Educational Researcher, 5(10), 3-8.
#' 
#' Welch, B. L. (1947). The generalization of "Student's" problem when several 
#' different population variances are involved. Biometrika, 34(1-2), 28-35.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Basic two-sample test
#' survey_data %>%
#'   t_test(life_satisfaction, group = gender)
#'
#' # With survey weights
#' survey_data %>%
#'   t_test(life_satisfaction, group = gender, weights = sampling_weight)
#'
#' # Multiple variables
#' survey_data %>%
#'   t_test(age, income, life_satisfaction, group = region, weights = sampling_weight)
#'
#' # One-sample test against a benchmark
#' survey_data %>%
#'   t_test(life_satisfaction, mu = 5, weights = sampling_weight)
#'
#' # Grouped analysis
#' survey_data %>%
#'   group_by(region) %>%
#'   t_test(life_satisfaction, group = gender, weights = sampling_weight)
#'
#' # Equal variance assumption
#' survey_data %>%
#'   t_test(life_satisfaction, group = gender, var.equal = TRUE)
#'
#' # One-sided test
#' survey_data %>%
#'   t_test(income, group = gender, alternative = "greater")
#'
#' # Using tidyselect helpers
#' survey_data %>%
#'   t_test(starts_with("trust"), group = gender, weights = sampling_weight)
#'
#' # Store results for further analysis
#' result <- survey_data %>%
#'   t_test(life_satisfaction, group = gender, weights = sampling_weight)
#' print(result)
#'
#' @family hypothesis_tests
#' @export
t_test <- function(data, ..., group = NULL, weights = NULL, 
                  var.equal = FALSE, mu = 0, alternative = c("two.sided", "less", "greater"),
                  conf.level = 0.95) {
  
  # Input validation
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }
  
  alternative <- match.arg(alternative)

  # Validate conf.level
  if (!is.numeric(conf.level) || length(conf.level) != 1 ||
      conf.level <= 0 || conf.level >= 1) {
    cli_abort("{.arg conf.level} must be a single number between 0 and 1 (exclusive).")
  }

  # Check if data is grouped
  is_grouped <- inherits(data, "grouped_df")
  group_vars <- if (is_grouped) group_vars(data) else NULL
  
  # Get variable names using tidyselect
  dots <- enquos(...)
  group_quo <- enquo(group)
  weights_quo <- enquo(weights)
  
  # Evaluate selections
  vars <- eval_select(expr(c(!!!dots)), data = data)
  var_names <- names(vars)

  # Validate that selected variables are numeric
  for (vn in var_names) {
    if (!is.numeric(data[[vn]])) {
      cli_abort("Variable {.var {vn}} is not numeric. {.fn t_test} requires numeric variables.")
    }
  }

  if (!quo_is_null(group_quo)) {
    g_var <- eval_select(expr(!!group_quo), data = data)
    g_name <- names(g_var)
  } else {
    g_name <- NULL
  }
  
  if (!quo_is_null(weights_quo)) {
    w_var <- eval_select(expr(!!weights_quo), data = data)
    w_name <- names(w_var)
  } else {
    w_name <- NULL
  }
  




  # Helper function to calculate Cohen's d (following sjstats approach exactly)
  calculate_cohens_d <- function(x1, x2, w1 = NULL, w2 = NULL) {
    if (is.null(w1) || is.null(w2)) {
      # Unweighted Cohen's d
      x1 <- x1[!is.na(x1)]
      x2 <- x2[!is.na(x2)]
      
      d <- mean(x1) - mean(x2)
      s1 <- sd(x1)
      s2 <- sd(x2)
      n1 <- length(x1)
      n2 <- length(x2)
      
      # Pooled standard deviation
      s <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
      cohens_d <- d / s
      
      # Glass' Delta (uses only control group SD - first group)
      glass_delta <- d / s1
      
      # Hedges' bias correction (methodologically superior)
      # J = 1 - 3/(4*(n1+n2-2)-1) 
      hedges_j <- 1 - (3 / (4 * (n1 + n2 - 2) - 1))
      hedges_g <- cohens_d * hedges_j
      
    } else {
      # Weighted Cohen's d using proper weighted means and pooled SD
      # Remove NA values
      valid1 <- !is.na(x1) & !is.na(w1)
      valid2 <- !is.na(x2) & !is.na(w2)
      x1 <- x1[valid1]
      x2 <- x2[valid2]
      w1 <- w1[valid1]
      w2 <- w2[valid2]

      # Weighted means
      w_mean1 <- sum(w1 * x1) / sum(w1)
      w_mean2 <- sum(w2 * x2) / sum(w2)
      d <- w_mean1 - w_mean2

      # Weighted sum of squares per group
      ss1 <- sum(w1 * (x1 - w_mean1)^2)
      ss2 <- sum(w2 * (x2 - w_mean2)^2)
      V1_1 <- sum(w1)
      V1_2 <- sum(w2)
      V1_total <- V1_1 + V1_2

      # Weighted pooled SD (SPSS frequency weights formula)
      s_pooled <- sqrt((ss1 + ss2) / (V1_total - 2))
      cohens_d <- d / s_pooled

      # Glass' Delta (uses only control group weighted SD)
      s1_weighted <- sqrt(ss1 / (V1_1 - 1))
      glass_delta <- d / s1_weighted

      # Hedges' bias correction using effective N
      eff_n1 <- sum(w1)^2 / sum(w1^2)
      eff_n2 <- sum(w2)^2 / sum(w2^2)
      hedges_j <- 1 - (3 / (4 * (eff_n1 + eff_n2 - 2) - 1))
      hedges_g <- cohens_d * hedges_j
    }
    
    # Return all three effect sizes in a list
    if (exists("hedges_g") && exists("glass_delta")) {
      return(list(cohens_d = cohens_d, hedges_g = hedges_g, glass_delta = glass_delta))
    } else {
      return(list(cohens_d = cohens_d, hedges_g = cohens_d, glass_delta = cohens_d))
    }
  }

  # Helper function to perform t-test for a single variable
  perform_single_t_test <- function(data, var_name, group_name = NULL, weight_name = NULL) {
    # Get the variable values
    x <- data[[var_name]]
    
    # Remove NA values
    valid_indices <- !is.na(x)
    if (!is.null(group_name)) {
      g_temp <- data[[group_name]]
      valid_indices <- valid_indices & !is.na(g_temp)
    }
    if (!is.null(weight_name)) {
      w <- data[[weight_name]]
      valid_indices <- valid_indices & !is.na(w)
      w <- w[valid_indices]
    }
    x <- x[valid_indices]
    
    if (is.null(group_name)) {
      # One-sample t-test
      if (is.null(weight_name)) {
        test_result <- t.test(x, mu = mu, alternative = alternative, conf.level = conf.level)
        group_stats <- list(means = mean(x, na.rm = TRUE), n = length(x))
      } else {
        # Weighted one-sample t-test using SPSS frequency weights approach
        weighted_mean <- sum(x * w) / sum(w)

        # SPSS uses rounded sum of weights as effective sample size
        n_eff <- round(sum(w))

        # Calculate variance using SPSS frequency weights formula
        # Uses n_eff - 1 in denominator (frequency weights approach)
        numerator <- sum(w * (x - weighted_mean)^2)
        weighted_var <- numerator / (n_eff - 1)

        # Calculate SE using SPSS formula with effective sample size
        weighted_sd <- sqrt(weighted_var)
        se <- weighted_sd / sqrt(n_eff)

        # Calculate t-statistic
        t_stat <- (weighted_mean - mu) / se

        # Degrees of freedom based on effective sample size
        df <- n_eff - 1

        if (alternative == "two.sided") {
          p_value <- 2 * pt(-abs(t_stat), df)
        } else if (alternative == "less") {
          p_value <- pt(t_stat, df)
        } else {
          p_value <- pt(t_stat, df, lower.tail = FALSE)
        }

        conf_int <- weighted_mean + c(-1, 1) * qt((1 + conf.level)/2, df) * se

        test_result <- list(
          statistic = t_stat,
          parameter = df,
          p.value = p_value,
          conf.int = conf_int,
          estimate = weighted_mean
        )
        # Return effective sample size for consistency with SPSS
        group_stats <- list(means = weighted_mean, n = n_eff)
      }
      
      return(list(
        t_stat = as.numeric(test_result$statistic),
        df = as.numeric(test_result$parameter),
        p_value = as.numeric(test_result$p.value),
        mean_diff = as.numeric(test_result$estimate),
        conf_int = as.numeric(test_result$conf.int),
        cohens_d = list(cohens_d = NA, hedges_g = NA, glass_delta = NA),
        group_levels = NULL,
        group_stats = group_stats,
        equal_var_result = NULL,  # Not applicable for one-sample
        unequal_var_result = NULL,  # Not applicable for one-sample
        is_weighted = !is.null(weight_name)
      ))
      
    } else {
      # Two-sample t-test
      g <- data[[group_name]][valid_indices]
      
      # Get unique levels preserving factor order
      if (is.factor(g)) {
        all_levels <- levels(g)
        g_levels <- all_levels[all_levels %in% unique(g)]
      } else {
        g_levels <- unique(g)
      }
      
      if (length(g_levels) != 2) {
        cli_abort(c(
          "Grouping variable {.var {group_name}} must have exactly 2 levels.",
          "x" = "Found {length(g_levels)} level{?s}."
        ))
      }
      
      # Split data by groups
      x1 <- x[g == g_levels[1]]
      x2 <- x[g == g_levels[2]]
      w1 <- if (!is.null(weight_name)) w[g == g_levels[1]] else NULL
      w2 <- if (!is.null(weight_name)) w[g == g_levels[2]] else NULL
      
      if (is.null(weight_name)) {
        # Unweighted two-sample t-test - SPSS style (both equal and unequal variance)
        # Calculate both variants like SPSS does
        test_equal <- t.test(x1, x2, var.equal = TRUE, alternative = alternative, 
                            conf.level = conf.level, mu = mu)
        test_unequal <- t.test(x1, x2, var.equal = FALSE, alternative = alternative, 
                              conf.level = conf.level, mu = mu)
        
        # Use the variant specified by var.equal parameter as primary result
        test_result <- if (var.equal) test_equal else test_unequal
        
        # Store both results for SPSS-style display
        test_result$equal_var_result <- test_equal
        test_result$unequal_var_result <- test_unequal
        
        cohens_d <- calculate_cohens_d(x1, x2)
        
        group_stats <- list(
          group1 = list(name = as.character(g_levels[1]), mean = mean(x1, na.rm = TRUE), n = length(x1)),
          group2 = list(name = as.character(g_levels[2]), mean = mean(x2, na.rm = TRUE), n = length(x2))
        )
        
      } else {
        # Weighted two-sample t-test using SPSS frequency weights approach
        mu_x <- sum(x1 * w1) / sum(w1)  # weighted mean group 1
        mu_y <- sum(x2 * w2) / sum(w2)  # weighted mean group 2

        # Use rounded effective sample sizes (SPSS frequency weights)
        n1_eff <- round(sum(w1))
        n2_eff <- round(sum(w2))

        # Calculate weighted variances using SPSS frequency weights formula
        # Uses n_eff - 1 in denominator
        var_x <- sum(w1 * (x1 - mu_x)^2) / (n1_eff - 1)
        var_y <- sum(w2 * (x2 - mu_y)^2) / (n2_eff - 1)

        # Standard deviations
        sd_x <- sqrt(var_x)
        sd_y <- sqrt(var_y)

        # Mean difference
        mean_diff <- mu_x - mu_y

        # === EQUAL VARIANCE (Student's t-test) ===
        # Calculate pooled standard deviation
        pooled_var <- ((n1_eff - 1) * var_x + (n2_eff - 1) * var_y) / (n1_eff + n2_eff - 2)
        pooled_sd <- sqrt(pooled_var)

        # Standard error for equal variance
        se_equal <- pooled_sd * sqrt(1/n1_eff + 1/n2_eff)

        # t-statistic and df for equal variance
        t_stat_equal <- (mean_diff - mu) / se_equal
        df_equal <- n1_eff + n2_eff - 2

        # === UNEQUAL VARIANCE (Welch's t-test) ===
        # Standard errors for each group
        se_x <- sd_x / sqrt(n1_eff)
        se_y <- sd_y / sqrt(n2_eff)

        # Combined standard error for unequal variance
        se_unequal <- sqrt(se_x^2 + se_y^2)

        # t-statistic for unequal variance
        t_stat_unequal <- (mean_diff - mu) / se_unequal

        # Welch-Satterthwaite degrees of freedom
        df_unequal <- (se_x^2 + se_y^2)^2 / (se_x^4 / (n1_eff - 1) + se_y^4 / (n2_eff - 1))

        # Calculate p-values for both methods
        if (alternative == "two.sided") {
          p_value_equal <- 2 * pt(-abs(t_stat_equal), df_equal)
          p_value_unequal <- 2 * pt(-abs(t_stat_unequal), df_unequal)
        } else if (alternative == "less") {
          p_value_equal <- pt(t_stat_equal, df_equal)
          p_value_unequal <- pt(t_stat_unequal, df_unequal)
        } else {
          p_value_equal <- pt(t_stat_equal, df_equal, lower.tail = FALSE)
          p_value_unequal <- pt(t_stat_unequal, df_unequal, lower.tail = FALSE)
        }

        # Calculate confidence intervals for both methods
        if (alternative == "two.sided") {
          conf_int_equal <- mean_diff + c(-1, 1) * qt((1 + conf.level)/2, df_equal) * se_equal
          conf_int_unequal <- mean_diff + c(-1, 1) * qt((1 + conf.level)/2, df_unequal) * se_unequal
        } else if (alternative == "less") {
          conf_int_equal <- c(-Inf, mean_diff + qt(conf.level, df_equal) * se_equal)
          conf_int_unequal <- c(-Inf, mean_diff + qt(conf.level, df_unequal) * se_unequal)
        } else {
          conf_int_equal <- c(mean_diff - qt(conf.level, df_equal) * se_equal, Inf)
          conf_int_unequal <- c(mean_diff - qt(conf.level, df_unequal) * se_unequal, Inf)
        }

        # Calculate Cohen's d using weighted data
        cohens_d <- calculate_cohens_d(x1, x2, w1, w2)

        test_equal_weighted <- list(
          statistic = t_stat_equal,
          parameter = df_equal,
          p.value = p_value_equal,
          conf.int = conf_int_equal,
          estimate = c(mu_x, mu_y)
        )

        test_unequal_weighted <- list(
          statistic = t_stat_unequal,
          parameter = df_unequal,
          p.value = p_value_unequal,
          conf.int = conf_int_unequal,
          estimate = c(mu_x, mu_y)
        )
        
        test_result <- test_unequal_weighted  # Use unequal as primary
        test_result$equal_var_result <- test_equal_weighted
        test_result$unequal_var_result <- test_unequal_weighted
        
        # Group statistics with effective sample sizes
        group_stats <- list(
          group1 = list(name = as.character(g_levels[1]), mean = mu_x, n = n1_eff),
          group2 = list(name = as.character(g_levels[2]), mean = mu_y, n = n2_eff)
        )
      }
      
      return(list(
        t_stat = test_result$statistic,
        df = test_result$parameter,
        p_value = test_result$p.value,
        mean_diff = if (length(test_result$estimate) > 1) {
          test_result$estimate[1] - test_result$estimate[2]
        } else {
          test_result$estimate
        },
        conf_int = test_result$conf.int,
        cohens_d = cohens_d,
        group_levels = g_levels,
        group_stats = group_stats,
        # Store both variance assumptions for SPSS-style display (now for all tests)
        equal_var_result = test_result$equal_var_result,
        unequal_var_result = test_result$unequal_var_result,
        is_weighted = !is.null(weight_name)
      ))
    }
  }
  
  # Main execution logic
  if (is_grouped) {
    # Split data by groups
    data_list <- group_split(data)
    group_keys <- group_keys(data)
    
    # Perform t-tests for each group
    results_list <- lapply(seq_along(data_list), function(i) {
      group_data <- data_list[[i]]
      group_info <- group_keys[i, , drop = FALSE]
      
      # Regular t-tests for each variable in this group
        group_results <- lapply(var_names, function(var_name) {
          tryCatch({
            test_result <- perform_single_t_test(group_data, var_name, g_name, w_name)
            
            result_df <- data.frame(
              group_info,
              Variable = var_name,
              t_stat = test_result$t_stat,
              df = test_result$df,
              p_value = test_result$p_value,
              mean_diff = test_result$mean_diff,
              cohens_d = if(is.list(test_result$cohens_d)) test_result$cohens_d$cohens_d else test_result$cohens_d,
              hedges_g = if(is.list(test_result$cohens_d)) test_result$cohens_d$hedges_g else test_result$cohens_d,
              glass_delta = if(is.list(test_result$cohens_d)) test_result$cohens_d$glass_delta else test_result$cohens_d,
              conf_int_lower = test_result$conf_int[1],
              conf_int_upper = test_result$conf_int[2],
              # Add test-compatible CI column aliases
              CI_lower = test_result$conf_int[1],
              CI_upper = test_result$conf_int[2],
              # Add sample size columns for test compatibility
              n1 = if(!is.null(test_result$group_stats)) {
                if(!is.null(test_result$group_stats$group1)) test_result$group_stats$group1$n else test_result$group_stats$n
              } else NA,
              n2 = if(!is.null(test_result$group_stats)) {
                if(!is.null(test_result$group_stats$group2)) test_result$group_stats$group2$n else NA
              } else NA
            )
            result_df$group_stats <- list(test_result$group_stats)
            result_df$equal_var_result <- list(test_result$equal_var_result)
            result_df$unequal_var_result <- list(test_result$unequal_var_result)
            result_df$is_weighted <- test_result$is_weighted
            result_df
          }, error = function(e) {
            data.frame(
              group_info,
              Variable = var_name,
              t_stat = NA,
              df = NA,
              p_value = NA,
              mean_diff = NA,
              cohens_d = NA,
              hedges_g = NA,
              glass_delta = NA,
              conf_int_lower = NA,
              conf_int_upper = NA,
              CI_lower = NA,
              CI_upper = NA,
              n1 = NA,
              n2 = NA,
              group_stats = list(NULL),
              equal_var_result = list(NULL),
              unequal_var_result = list(NULL),
              is_weighted = NA
            )
          })
        })
        
        do.call(rbind, group_results)
    })
    
    results_df <- do.call(rbind, results_list)
    
  } else {
    # Perform t-tests for each variable (ungrouped)
    results_list <- lapply(var_names, function(var_name) {
      tryCatch({
        test_result <- perform_single_t_test(data, var_name, g_name, w_name)
        
        result_df <- data.frame(
          Variable = var_name,
          t_stat = test_result$t_stat,
          df = test_result$df,
          p_value = test_result$p_value,
          mean_diff = test_result$mean_diff,
          cohens_d = if(is.list(test_result$cohens_d)) test_result$cohens_d$cohens_d else test_result$cohens_d,
          hedges_g = if(is.list(test_result$cohens_d)) test_result$cohens_d$hedges_g else test_result$cohens_d,
          glass_delta = if(is.list(test_result$cohens_d)) test_result$cohens_d$glass_delta else test_result$cohens_d,
          conf_int_lower = test_result$conf_int[1],
          conf_int_upper = test_result$conf_int[2],
          # Add test-compatible CI column aliases
          CI_lower = test_result$conf_int[1],
          CI_upper = test_result$conf_int[2],
          # Add sample size columns for test compatibility
          n1 = if(!is.null(test_result$group_stats)) {
            if(!is.null(test_result$group_stats$group1)) test_result$group_stats$group1$n else test_result$group_stats$n
          } else NA,
          n2 = if(!is.null(test_result$group_stats)) {
            if(!is.null(test_result$group_stats$group2)) test_result$group_stats$group2$n else NA
          } else NA
        )
        result_df$group_stats <- list(test_result$group_stats)
        result_df$equal_var_result <- list(test_result$equal_var_result)
        result_df$unequal_var_result <- list(test_result$unequal_var_result)
        result_df$is_weighted <- test_result$is_weighted
        result_df
      }, error = function(e) {
        stop(e$message)
      })
    })
    
    results_df <- do.call(rbind, results_list)
  }
  
  # Create S3 object
  structure(
    list(
      results = results_df,
      variables = var_names,
      group = g_name,
      weights = w_name,
      var.equal = var.equal,
      groups = group_vars,
      is_grouped = is_grouped,
      mu = mu,
      alternative = alternative,
      conf.level = conf.level,
      group_levels = if (!is.null(g_name)) {
        g_var <- data[[g_name]]
        if (is.factor(g_var)) {
          all_levels <- levels(g_var)
          all_levels[all_levels %in% unique(g_var)]
        } else {
          unique(g_var)
        }
      } else NULL,
      data = data[, unique(c(var_names, g_name, w_name, group_vars)), drop = FALSE]
    ),
    class = "t_test"
  )
}

#' Get table border for t-test formatting (similar to describe function)
#' @keywords internal
.get_t_test_border <- function(df) {
  col_widths <- sapply(names(df), function(col) {
    max(nchar(as.character(df[[col]])), nchar(col), na.rm = TRUE)
  })
  total_width <- sum(col_widths) + length(col_widths) - 1
  return(paste(rep("-", total_width), collapse = ""))
}

#' Create effect size data frame
#' @keywords internal
.create_effect_size_df <- function(var_name, cohens_d_val, hedges_g_val, glass_delta_val, is_paired = FALSE) {
  effect_size_g <- if (abs(hedges_g_val) < 0.2) "negligible" else
                  if (abs(hedges_g_val) < 0.5) "small" else
                  if (abs(hedges_g_val) < 0.8) "medium" else "large"
  
  if (is_paired) {
    # For paired tests, don't show Glass' Delta
    data.frame(
      Variable = var_name,
      Cohens_d = round(cohens_d_val, 3),
      Hedges_g = round(hedges_g_val, 3),
      Effect_Size = effect_size_g,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      Variable = var_name,
      Cohens_d = round(cohens_d_val, 3),
      Hedges_g = round(hedges_g_val, 3),
      Glass_Delta = round(glass_delta_val, 3),
      Effect_Size = effect_size_g,
      stringsAsFactors = FALSE
    )
  }
}

# Internal implementation shared by both print methods
.print_t_test_impl <- function(x, digits = 3) {
  weights_name <- x$weight_var %||% x$weights
  is_weighted <- !is.null(weights_name)
  is_grouped_data <- (!is.null(x$grouped) && x$grouped) ||
                     (!is.null(x$is_grouped) && x$is_grouped)
  results_label <- if (is_weighted) "Weighted t-test Results" else "t-test Results"

  # Print test info
  if (!is.null(x$group)) {
    group_levels <- x$group_levels
    if (length(group_levels) >= 2) {
      cat("\n")
      test_info <- list(
        "Grouping variable" = x$group,
        "Groups compared" = sprintf("%s vs. %s",
                                   as.character(group_levels[1]),
                                   as.character(group_levels[2])),
        "Weights variable" = weights_name
      )
      print_info_section(test_info)
      test_params <- list(
        mu = x$mu,
        alternative = x$alternative,
        conf.level = x$conf.level
      )
      print_test_parameters(test_params)
      cat("\n")
    }
  }

  # Internal helper: print a single variable row from a results data frame
  .print_var_row <- function(row_results, idx, show_group_means) {
    var_name <- row_results$Variable[idx]
    stats <- row_results$group_stats[[idx]]

    cat(sprintf("\n--- %s ---\n\n", var_name))

    # Group means
    if (show_group_means && !is.null(stats) && !is.null(stats$group1)) {
      cat(sprintf("  %s: mean = %.*f, n = %.1f\n",
                  stats$group1$name, digits, stats$group1$mean, stats$group1$n))
      cat(sprintf("  %s: mean = %.*f, n = %.1f\n",
                  stats$group2$name, digits, stats$group2$mean, stats$group2$n))
    }

    # Results table
    has_both <- !is.null(row_results$equal_var_result[[idx]])

    if (has_both) {
      equal_result <- row_results$equal_var_result[[idx]]
      unequal_result <- row_results$unequal_var_result[[idx]]
      spss_df <- data.frame(
        Assumption = c("Equal variances", "Unequal variances"),
        t_stat = c(round(equal_result$statistic, 3), round(unequal_result$statistic, 3)),
        df = c(equal_result$parameter, round(unequal_result$parameter, 3)),
        p_value = c(round(equal_result$p.value, 3), round(unequal_result$p.value, 3)),
        mean_diff = c(round(equal_result$estimate[1] - equal_result$estimate[2], 3),
                     round(unequal_result$estimate[1] - unequal_result$estimate[2], 3)),
        conf_int = c(sprintf("[%.3f, %.3f]", equal_result$conf.int[1], equal_result$conf.int[2]),
                    sprintf("[%.3f, %.3f]", unequal_result$conf.int[1], unequal_result$conf.int[2]))
      )
      spss_df$sig <- cut(spss_df$p_value,
                        breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                        labels = c("***", "**", "*", ""),
                        right = FALSE)
      cat(sprintf("\n%s:\n", results_label))
      border <- paste(rep("-", 80), collapse = "")
      cat(border, "\n")
      print(spss_df, row.names = FALSE)
      cat(border, "\n")
    } else {
      results_df <- data.frame(
        Assumption = "t-test",
        t_stat = round(row_results$t_stat[idx], digits),
        df = round(row_results$df[idx], digits),
        p_value = round(row_results$p_value[idx], digits),
        mean_diff = round(row_results$mean_diff[idx], digits),
        conf_int = sprintf("[%.3f, %.3f]",
                          row_results$conf_int_lower[idx],
                          row_results$conf_int_upper[idx]),
        sig = row_results$sig[idx]
      )
      cat(sprintf("\n%s:\n", results_label))
      border <- paste(rep("-", 70), collapse = "")
      cat(border, "\n")
      print(results_df, row.names = FALSE)
      cat(border, "\n")
    }

    # Effect sizes
    cohens_d_val <- row_results$cohens_d[idx]
    hedges_g_val <- if ("hedges_g" %in% names(row_results)) row_results$hedges_g[idx] else cohens_d_val
    glass_delta_val <- if ("glass_delta" %in% names(row_results)) row_results$glass_delta[idx] else cohens_d_val

    if (!is.na(cohens_d_val)) {
      effect_df <- .create_effect_size_df(var_name, cohens_d_val, hedges_g_val, glass_delta_val, FALSE)
      cat("\nEffect Sizes:\n")
      cat(paste(rep("-", 12), collapse = ""), "\n")
      print(effect_df, row.names = FALSE)
    }
    cat("\n")
  }

  if (is_grouped_data) {
    groups <- unique(x$results[x$groups])
    for (i in seq_len(nrow(groups))) {
      group_values <- groups[i, , drop = FALSE]
      group_info <- sapply(names(group_values), function(g) {
        val <- group_values[[g]]
        if (is.factor(val)) paste(g, "=", levels(val)[val]) else paste(g, "=", val)
      })
      cat(sprintf("\nGroup: %s\n", paste(group_info, collapse = ", ")))

      group_results <- x$results
      for (g in names(group_values)) {
        group_results <- group_results[group_results[[g]] == group_values[[g]], ]
      }
      if (nrow(group_results) == 0) next
      group_results <- group_results[!is.na(group_results$Variable), ]
      if (nrow(group_results) == 0) next

      for (j in seq_len(nrow(group_results))) {
        .print_var_row(group_results, j, show_group_means = TRUE)
      }
    }
  } else {
    valid_results <- x$results[!is.na(x$results$Variable), ]
    for (i in seq_len(nrow(valid_results))) {
      .print_var_row(valid_results, i, show_group_means = !is.null(x$group))
    }
  }

  # Footer
  print_significance_legend()
  cat("\nEffect Size Interpretation:\n")
  cat("- Cohen's d: pooled standard deviation (classic)\n")
  cat("- Hedges' g: bias-corrected Cohen's d (preferred)\n")
  cat("- Glass' Delta: control group standard deviation only\n")
  cat("- Small effect: |effect| ~ 0.2\n")
  cat("- Medium effect: |effect| ~ 0.5\n")
  cat("- Large effect: |effect| ~ 0.8\n")
}

#' Print t-test results
#'
#' @description
#' Print method for objects of class \code{"t_test"}. Provides a
#' formatted display of t-test results including group statistics, test
#' statistics, p-values, effect sizes, and confidence intervals.
#'
#' @param x An object of class \code{"t_test"} returned by \code{\link{t_test}}.
#' @param digits Integer specifying the number of decimal places to display
#'   for numeric values. Default is \code{3}.
#' @param ... Additional arguments passed to \code{\link[base]{print}}. Currently unused.
#'
#' @details
#' The print method displays:
#' \itemize{
#'   \item Group-specific descriptive statistics (means and sample sizes)
#'   \item Test statistics (t-statistic, degrees of freedom, p-value)
#'   \item Effect size (Cohen's d) with interpretation
#'   \item Confidence intervals for mean differences
#'   \item Significance indicators (* p < 0.05, ** p < 0.01, *** p < 0.001)
#' }
#'
#' For grouped analyses (when data is grouped with \code{\link[dplyr]{group_by}}),
#' results are displayed separately for each group combination.
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @export
#' @method print t_test
print.t_test <- function(x, digits = 3, ...) {
  weights_name <- x$weight_var %||% x$weights
  test_type <- get_standard_title("t-Test", weights_name, "Results")
  print_header(test_type, newline_before = FALSE)
  x$results$p_value <- as.numeric(x$results$p_value)
  x$results$sig <- sapply(x$results$p_value, add_significance_stars)
  .print_t_test_impl(x, digits)
  invisible(x)
}


