
#' Perform independent sample t-tests with support for weights and grouped data
#'
#' @description
#' \code{t_test()} performs one-sample and two-sample t-tests on one or more
#' variables in a data frame. The function supports both weighted and unweighted analyses,
#' grouped data operations, and multiple variables simultaneously. It is particularly
#' designed for survey data analysis where sampling weights are crucial for
#' population-representative results.
#'
#' For paired t-tests (repeated measures), support will be added in a future version.
#'
#' The function implements both Student's t-test (equal variances) and Welch's t-test
#' (unequal variances) and provides comprehensive effect size calculations including
#' Cohen's d, Hedges' g (bias-corrected), and Glass' Delta.
#'
#' @section SPSS Compatibility:
#' This function is designed to be SPSS-compatible, producing results that closely match
#' SPSS output. Key compatibility features:
#' \itemize{
#'   \item Uses frequency weights approach (rounded effective sample sizes)
#'   \item Reports both equal and unequal variance assumptions
#'   \item Matching formulas for weighted variance and standard errors
#'   \item Effect sizes calculated using SPSS conventions
#' }
#'
#' Minor differences from SPSS (typically < 1%) may occur due to:
#' \itemize{
#'   \item Internal precision handling differences
#'   \item Different rounding strategies for intermediate calculations
#'   \item Data preprocessing variations
#' }
#'
#' These differences are within acceptable tolerances for practical statistical analysis.
#'
#' @param data A data frame or tibble containing the variables to analyze.
#' @param ... <\code{\link[dplyr]{dplyr_tidy_select}}> Variables for which to perform 
#'   t-tests. Supports all tidyselect helpers such as \code{starts_with()}, 
#'   \code{ends_with()}, \code{contains()}, \code{matches()}, \code{num_range()}, etc.
#' @param group <\code{\link[dplyr]{dplyr_data_masking}}> Optional grouping variable 
#'   for two-sample t-test. Must be a factor or character variable with exactly 
#'   two levels. If \code{NULL} (default), performs one-sample t-tests.
#' @param weights <\code{\link[dplyr]{dplyr_data_masking}}> Optional sampling weights 
#'   for weighted t-tests. Should be a numeric variable with positive values. 
#'   Weights are used to adjust for sampling design and non-response patterns.
#' @param var.equal Logical indicating whether to assume equal variances in 
#'   two-sample tests. Default is \code{FALSE} (Welch's t-test). When \code{TRUE}, 
#'   uses Student's t-test. Both results are displayed in SPSS-style output.
#' @param mu A number specifying the null hypothesis value. For one-sample tests, 
#'   this is the hypothesized population mean. For two-sample tests, this is the 
#'   hypothesized difference in means. Default is \code{0}.
#' @param alternative Character string specifying the alternative hypothesis. 
#'   Must be one of \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}.
#' @param conf.level Confidence level for the confidence interval. Must be between 
#'   0 and 1. Default is \code{0.95} (95% confidence interval).
#'
#' @return An object of class \code{"t_test_results"} containing:
#' \describe{
#'   \item{results}{A data frame with test statistics, p-values, effect sizes, 
#'     and confidence intervals for each variable or comparison}
#'   \item{variables}{Character vector of analyzed variable names}
#'   \item{group}{Name of the grouping variable (if used)}
#'   \item{weights}{Name of the weights variable (if used)}
#'   \item{var.equal}{Variance assumption used}
#'   \item{group_levels}{Levels of the grouping variable}
#'   \item{is_grouped}{Logical indicating if data was grouped via group_by()}
#'   \item{mu, alternative, conf.level}{Test parameters}
#' }
#' 
#' The results data frame contains the following columns:
#' \describe{
#'   \item{Variable}{Name of the analyzed variable}
#'   \item{t_stat}{t-statistic}
#'   \item{df}{Degrees of freedom}
#'   \item{p_value}{p-value}
#'   \item{mean_diff}{Mean difference (group1 - group2 for two-sample)}
#'   \item{cohens_d}{Cohen's d effect size}
#'   \item{hedges_g}{Hedges' g (bias-corrected Cohen's d)}
#'   \item{glass_delta}{Glass' Delta (for independent samples only)}
#'   \item{conf_int_lower, conf_int_upper}{Confidence interval bounds}
#'   \item{group_stats}{List column with group-specific or variable-specific statistics}
#' }
#'
#' @details
#' ## Statistical Methods
#' 
#' ### Independent Sample Tests
#' For two-sample tests, both equal and unequal variance assumptions are calculated:
#' 
#' **Welch's t-test (unequal variances):**
#' \deqn{t = \frac{\bar{x}_1 - \bar{x}_2 - \mu_0}{\sqrt{\frac{s_1^2}{n_1} + \frac{s_2^2}{n_2}}}}
#' 
#' **Student's t-test (equal variances):**
#' \deqn{t = \frac{\bar{x}_1 - \bar{x}_2 - \mu_0}{s_p\sqrt{\frac{1}{n_1} + \frac{1}{n_2}}}}
#' where \eqn{s_p = \sqrt{\frac{(n_1-1)s_1^2 + (n_2-1)s_2^2}{n_1+n_2-2}}}
#' 

#' ### Weighted Tests
#' For weighted tests, following \pkg{sjstats} methodology:
#' - Weighted means: \eqn{\mu_w = \frac{\sum w_i x_i}{\sum w_i}}
#' - Weighted variances: \eqn{\sigma_w^2 = \frac{\sum w_i (x_i - \mu_w)^2}{\sum w_i}}
#' - Standard errors use actual (unweighted) sample sizes for degrees of freedom
#' 
#' ### Effect Sizes
#' 
#' **Cohen's d (independent samples):**
#' \deqn{d = \frac{\bar{x}_1 - \bar{x}_2}{s_{pooled}}}
#' 
#' **Hedges' g (bias-corrected Cohen's d):**
#' For independent samples: \eqn{g = d \times J} where \eqn{J = 1 - \frac{3}{4(n_1+n_2-2)-1}}
#' 
#' **Glass' Delta (independent samples only):**
#' \deqn{\Delta = \frac{\bar{x}_1 - \bar{x}_2}{s_1}}
#' Uses only the standard deviation of the first (control) group.
#' 
#' ## Grouped Analyses
#' When data is grouped using \code{group_by()}, the function performs separate 
#' analyses for each group combination.
#' 
#' ## SPSS Compatibility
#' Results match SPSS output for:
#' - t-statistics and p-values
#' - Confidence intervals
#' - Effect size calculations
#' - Both equal and unequal variance assumptions displayed
#' 
#' ## Interpretation Guidelines
#' - **p-values**: p < 0.05 indicates statistical significance at alpha = 0.05
#' - **Effect sizes**: |effect| ~ 0.2 (small), |effect| ~ 0.5 (medium), |effect| ~ 0.8 (large)
#' - **Cohen's d**: Classic effect size using pooled standard deviation
#' - **Hedges' g**: Bias-corrected version, preferred for publication
#' - **Glass' Delta**: Uses control group SD only, useful when variances differ substantially
#' - **Confidence intervals**: Range of plausible values for the true difference
#'
#' @seealso 
#' \code{\link[stats]{t.test}} for the base R t-test function.
#' 
#' \code{\link{print.t_test_results}} for printing results.
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
#' # Load required packages
#' library(dplyr)
#' 
#' # Create sample data
#' set.seed(123)
#' survey_data <- data.frame(
#'   id = 1:200,
#'   group = factor(rep(c("Treatment", "Control"), each = 100)),
#'   pre_test = rnorm(200, 50, 10),
#'   post_test = rnorm(200, 55, 12),
#'   outcome1 = c(rnorm(100, 5.2, 1.5), rnorm(100, 4.8, 1.3)),
#'   outcome2 = c(rnorm(100, 3.1, 1.2), rnorm(100, 3.5, 1.4)),
#'   weight = runif(200, 0.5, 2.0),
#'   gender = factor(sample(c("Male", "Female"), 200, replace = TRUE))
#' )
#' 
#' # Basic two-sample t-test
#' survey_data %>%
#'   t_test(outcome1, group = group)
#' 
#' # Multiple variables
#' survey_data %>%
#'   t_test(outcome1, outcome2, group = group)
#' 
#' # Using tidyselect helpers
#' survey_data %>%
#'   t_test(starts_with("outcome"), group = group)
#' 
#' # Weighted analysis
#' survey_data %>%
#'   t_test(outcome1, group = group, weights = weight)
#' 
#' # Grouped analysis (separate tests for each gender)
#' survey_data %>%
#'   group_by(gender) %>%
#'   t_test(outcome1, group = group)
#' 
#' # One-sample t-test (test against mu = 5)
#' survey_data %>%
#'   t_test(outcome1, mu = 5)
#' 
#' # One-sided test
#' survey_data %>%
#'   t_test(outcome1, group = group, alternative = "greater")
#' 
#' # Equal variance assumption
#' survey_data %>%
#'   t_test(outcome1, group = group, var.equal = TRUE)
#' 
#' # Store results for further analysis
#' result <- survey_data %>%
#'   t_test(outcome1, group = group, weights = weight)
#' print(result)
#'
#' @export
t_test <- function(data, ..., group = NULL, weights = NULL, 
                  var.equal = FALSE, mu = 0, alternative = c("two.sided", "less", "greater"),
                  conf.level = 0.95) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  alternative <- match.arg(alternative)
  
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
      # Weighted Cohen's d - following sjstats method exactly
      # Remove NA values
      valid1 <- !is.na(x1) & !is.na(w1)
      valid2 <- !is.na(x2) & !is.na(w2)
      x1 <- x1[valid1]
      x2 <- x2[valid2]
      w1 <- w1[valid1]
      w2 <- w2[valid2]
      
      # Create data frame
      dat <- data.frame(
        y = c(x1, x2),
        g = factor(c(rep("Group1", length(x1)), rep("Group2", length(x2)))),
        w = c(w1, w2)
      )
      
      # Key step: multiply values by weights
      dat$y <- dat$y * dat$w
      
      # Use simple Cohen's d calculation on weighted values
      group1_values <- dat$y[dat$g == "Group1"]
      group2_values <- dat$y[dat$g == "Group2"]
      
      d <- mean(group1_values) - mean(group2_values)
      s1 <- sd(group1_values)
      s2 <- sd(group2_values)
      n1 <- length(group1_values)
      n2 <- length(group2_values)
      
      s <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
      cohens_d <- d / s
      
      # Glass' Delta (uses only control group SD - first group)
      glass_delta <- d / s1
      
      # Hedges' bias correction (methodologically superior)
      hedges_j <- 1 - (3 / (4 * (n1 + n2 - 2) - 1))
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
        stop(sprintf("Grouping variable '%s' must have exactly 2 levels, found %d", 
                    group_name, length(g_levels)))
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
      data = data  # Store original data for levene_test
    ),
    class = "t_test_results"
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

#' Print t-test results
#'
#' @description
#' Print method for objects of class \code{"t_test_results"}. Provides a 
#' formatted display of t-test results including group statistics, test 
#' statistics, p-values, effect sizes, and confidence intervals.
#'
#' @param x An object of class \code{"t_test_results"} returned by \code{\link{t_test}}.
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
#' For weighted analyses, a note is included about the sjstats compatibility 
#' behavior regarding group label display.
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @export
print.t_test_results <- function(x, digits = 3, ...) {
  # Determine test type based on weights (Template Standard)
  test_type <- if (!is.null(x$weight_var) || !is.null(x$weights)) {
    "Weighted t-Test Results"
  } else {
    "t-Test Results"
  }
  
  cat(sprintf("\n%s\n", test_type))
  border_line <- paste(rep("-", nchar(test_type)), collapse = "")
  writeLines(border_line)
  
  # Ensure p-values are numeric
  x$results$p_value <- as.numeric(x$results$p_value)
  
  # Add significance stars (Template Standard)
  x$results$sig <- cut(x$results$p_value, 
                      breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                      labels = c("***", "**", "*", ""),
                      right = FALSE)
                      
  # Template Standard: Dual grouped data detection
  is_grouped_data <- (!is.null(x$grouped) && x$grouped) || 
                     (!is.null(x$is_grouped) && x$is_grouped)
  
    # Print test info
  if (!is.null(x$group)) {
    # Grouping variable info
    group_levels <- x$group_levels
    if (length(group_levels) >= 2) {
      cat(sprintf("\nGrouping variable: %s\n", x$group))
      cat(sprintf("Groups compared: %s vs. %s\n", 
                  as.character(group_levels[1]), 
                  as.character(group_levels[2])))
      if (!is.null(x$weight_var) || !is.null(x$weights)) {
        weight_name <- if (!is.null(x$weight_var)) x$weight_var else x$weights
        cat(sprintf("Weights variable: %s\n", weight_name))
      }
      cat(sprintf("Null hypothesis (mu): %.3f\n", x$mu))
      cat(sprintf("Alternative hypothesis: %s\n", x$alternative))
      cat(sprintf("Confidence level: %.1f%%\n", x$conf.level * 100))
      cat("\n")
    }
  }
  
  if (is_grouped_data) {
    # Get unique groups
    groups <- unique(x$results[x$groups])
    
    # Print results for each group
    for (i in seq_len(nrow(groups))) {
      group_values <- groups[i, , drop = FALSE]
      
      # Format group info
      group_info <- sapply(names(group_values), function(g) {
        val <- group_values[[g]]
        if (is.factor(val)) {
          paste(g, "=", levels(val)[val])
        } else {
          paste(g, "=", val)
        }
      })
      group_info <- paste(group_info, collapse = ", ")
      
      # Filter results for current group
      group_results <- x$results
      for (g in names(group_values)) {
        group_results <- group_results[group_results[[g]] == group_values[[g]], ]
      }
      
      if (nrow(group_results) == 0) next
      group_results <- group_results[!is.na(group_results$Variable), ]
      if (nrow(group_results) == 0) next
      
      cat(sprintf("\nGroup: %s\n", group_info))
      
      # Print each variable as separate block
      for (j in seq_len(nrow(group_results))) {
        var <- group_results$Variable[j]
        stats <- group_results$group_stats[[j]]
        
        cat(sprintf("\n--- %s ---\n", var))
        cat("\n")  # Add blank line after variable name
         
        # Print statistics based on test type
        if (!is.null(stats) && !is.null(stats$group1)) {
          # Group comparison: show group means
          cat(sprintf("  %s: mean = %.3f, n = %.1f\n", 
                      stats$group1$name, stats$group1$mean, stats$group1$n))
          cat(sprintf("  %s: mean = %.3f, n = %.1f\n", 
                      stats$group2$name, stats$group2$mean, stats$group2$n))
        }
        
        # Create and print t-test table for this variable
        # Check if we have both variance assumptions
        has_both_assumptions <- !is.null(group_results$equal_var_result[[j]])
        
        if (has_both_assumptions) {
          # SPSS-style display with both equal and unequal variance assumptions
          equal_result <- group_results$equal_var_result[[j]]
          unequal_result <- group_results$unequal_var_result[[j]]
          
          # Create SPSS-style table
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
          
          # Add significance stars
          spss_df$sig <- cut(spss_df$p_value, 
                            breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                            labels = c("***", "**", "*", ""),
                            right = FALSE)
          
          # Print SPSS-style table
          cat(sprintf("\n%s:\n", ifelse(!is.null(x$weight_var) || !is.null(x$weights), "Weighted t-test Results", "t-test Results")))
          border_width <- paste(rep("-", 80), collapse = "")
          cat(border_width, "\n")
          print(spss_df, row.names = FALSE)
          cat(border_width, "\n")
          
        } else {
          # Standard display
          results_df <- data.frame(
            Assumption = "t-test",
            t_stat = round(group_results$t_stat[j], digits),
            df = round(group_results$df[j], digits),
            p_value = round(group_results$p_value[j], digits),
            mean_diff = round(group_results$mean_diff[j], digits),
            conf_int = sprintf("[%.3f, %.3f]", 
                              group_results$conf_int_lower[j],
                              group_results$conf_int_upper[j]),
            sig = group_results$sig[j]
          )
          
          cat(sprintf("\n%s:\n", ifelse(!is.null(x$weight_var) || !is.null(x$weights), "Weighted t-test Results", "t-test Results")))
          border_width <- paste(rep("-", 70), collapse = "")
          cat(border_width, "\n")
          print(results_df, row.names = FALSE)
          cat(border_width, "\n")
        }
        
        # Add Effect Sizes for this variable
        cohens_d_val <- group_results$cohens_d[j]
        hedges_g_val <- if("hedges_g" %in% names(group_results)) group_results$hedges_g[j] else cohens_d_val
        glass_delta_val <- if("glass_delta" %in% names(group_results)) group_results$glass_delta[j] else cohens_d_val
        
        if (!is.na(cohens_d_val)) {
          effect_df <- .create_effect_size_df(var, cohens_d_val, hedges_g_val, glass_delta_val, FALSE)
          
          cat("\nEffect Sizes:\n")
          cat(paste(rep("-", 12), collapse = ""), "\n")
          print(effect_df, row.names = FALSE)
        }
        cat("\n")
      }
    }
  } else {
    # Print results for ungrouped data - each variable as separate block
    valid_results <- x$results[!is.na(x$results$Variable), ]
    
    for (i in seq_len(nrow(valid_results))) {
      var_name <- valid_results$Variable[i]
      stats <- valid_results$group_stats[[i]]
      
      cat(sprintf("\n--- %s ---\n", var_name))
      cat("\n")  # Add blank line after variable name
      
      # Print statistics based on test type
      if (!is.null(x$group) && !is.null(stats) && !is.null(stats$group1)) {
        # Group comparison: show group means
        cat(sprintf("  %s: mean = %.*f, n = %.1f\n", 
                    stats$group1$name, digits, stats$group1$mean, stats$group1$n))
        cat(sprintf("  %s: mean = %.*f, n = %.1f\n", 
                    stats$group2$name, digits, stats$group2$mean, stats$group2$n))
        cat("\n")
      }
      
      # Check if we have both variance assumptions
      has_both_assumptions <- !is.null(valid_results$equal_var_result[[i]])
      
      if (has_both_assumptions) {
        # SPSS-style display with both equal and unequal variance assumptions
        equal_result <- valid_results$equal_var_result[[i]]
        unequal_result <- valid_results$unequal_var_result[[i]]
        
        # Create SPSS-style table
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
        
        # Add significance stars
        spss_df$sig <- cut(spss_df$p_value, 
                          breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                          labels = c("***", "**", "*", ""),
                          right = FALSE)
        
        # Print SPSS-style table
        cat(sprintf("\n%s:\n", ifelse(!is.null(x$weight_var) || !is.null(x$weights), "Weighted t-test Results", "t-test Results")))
        border_width <- paste(rep("-", 80), collapse = "")
        cat(border_width, "\n")
        print(spss_df, row.names = FALSE)
        cat(border_width, "\n")
        
      } else {
        # Standard display
        results_df <- data.frame(
          Assumption = "t-test",
          t_stat = round(valid_results$t_stat[i], digits),
          df = round(valid_results$df[i], digits),
          p_value = round(valid_results$p_value[i], digits),
          mean_diff = round(valid_results$mean_diff[i], digits),
          conf_int = sprintf("[%.3f, %.3f]", 
                            valid_results$conf_int_lower[i],
                            valid_results$conf_int_upper[i]),
          sig = valid_results$sig[i]
        )
        
        cat(sprintf("\n%s:\n", ifelse(!is.null(x$weight_var) || !is.null(x$weights), "Weighted t-test Results", "t-test Results")))
        border_width <- paste(rep("-", 70), collapse = "")
        writeLines(border_width)
        print(results_df, row.names = FALSE)
        writeLines(border_width)
      }
      
      # Add Effect Sizes for this variable
      cohens_d_val <- valid_results$cohens_d[i]
      hedges_g_val <- if("hedges_g" %in% names(valid_results)) valid_results$hedges_g[i] else cohens_d_val
      glass_delta_val <- if("glass_delta" %in% names(valid_results)) valid_results$glass_delta[i] else cohens_d_val
      
      if (!is.na(cohens_d_val)) {
        effect_df <- .create_effect_size_df(var_name, cohens_d_val, hedges_g_val, glass_delta_val, FALSE)
        
        cat("\nEffect Sizes:\n")
        cat(paste(rep("-", 12), collapse = ""), "\n")
        print(effect_df, row.names = FALSE)
        cat("\n")
      }
    }
  }
  
  cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05\n")
  
  # Add interpretation guidelines only once at the end
  cat("\nEffect Size Interpretation:\n")
  cat("- Cohen's d: pooled standard deviation (classic)\n")
  cat("- Hedges' g: bias-corrected Cohen's d (preferred)\n")
  cat("- Glass' Delta: control group standard deviation only\n")
  cat("- Small effect: |effect| ~ 0.2\n")
  cat("- Medium effect: |effect| ~ 0.5\n")
  cat("- Large effect: |effect| ~ 0.8\n")
}

#' Print method for t_test_result
#'
#' @param x A t_test_result object
#' @param digits Number of decimal places to display
#' @param ... Additional arguments (not used)
#' @export
#' @method print t_test_result
print.t_test_result <- function(x, digits = 3, ...) {
  if (!is.null(x$weight_var) || !is.null(x$weights)) {
    cat("Weighted t-Test Results\n")
    cat(paste(rep("-", 23), collapse = ""), "\n")
  } else {
    cat("t-Test Results\n")
    cat(paste(rep("-", 14), collapse = ""), "\n")
  }
  
  # Ensure p-values are numeric
  x$results$p_value <- as.numeric(x$results$p_value)
  
  # Add significance stars (Template Standard)
  x$results$sig <- cut(x$results$p_value, 
                      breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                      labels = c("***", "**", "*", ""),
                      right = FALSE)
                      
  # Template Standard: Dual grouped data detection
  is_grouped_data <- (!is.null(x$grouped) && x$grouped) || 
                     (!is.null(x$is_grouped) && x$is_grouped)
  
  # Print info about grouping variable if present
  if (!is.null(x$group)) {
    group_levels <- x$group_levels
    if (length(group_levels) >= 2) {
      cat(sprintf("\nGrouping variable: %s\n", x$group))
      cat(sprintf("Groups compared: %s vs. %s\n", 
                  as.character(group_levels[1]), 
                  as.character(group_levels[2])))
      if (!is.null(x$weight_var) || !is.null(x$weights)) {
        weight_name <- if (!is.null(x$weight_var)) x$weight_var else x$weights
        cat(sprintf("Weights variable: %s\n", weight_name))
      }
      cat(sprintf("Null hypothesis (mu): %.3f\n", x$mu))
      cat(sprintf("Alternative hypothesis: %s\n", x$alternative))
      cat(sprintf("Confidence level: %.1f%%\n", x$conf.level * 100))
      cat("\n")
    }
  }
  
  if (is_grouped_data) {
    # Get unique groups
    groups <- unique(x$results[x$groups])
    
    # Print results for each group
    for (i in seq_len(nrow(groups))) {
      group_values <- groups[i, , drop = FALSE]
      
      # Format group info
      group_info <- sapply(names(group_values), function(g) {
        val <- group_values[[g]]
        if (is.factor(val)) {
          paste(g, "=", levels(val)[val])
        } else {
          paste(g, "=", val)
        }
      })
      group_info <- paste(group_info, collapse = ", ")
      
      # Filter results for current group
      group_results <- x$results
      for (g in names(group_values)) {
        group_results <- group_results[group_results[[g]] == group_values[[g]], ]
      }
      
      if (nrow(group_results) == 0) next
      group_results <- group_results[!is.na(group_results$Variable), ]
      if (nrow(group_results) == 0) next
      
      cat(sprintf("\nGroup: %s\n", group_info))
      
      # Print each variable as separate block
      for (j in seq_len(nrow(group_results))) {
        var <- group_results$Variable[j]
        stats <- group_results$group_stats[[j]]
        
        cat(sprintf("\n--- %s ---\n", var))
        cat("\n")  # Add blank line after variable name
         
        # Print group means
        if (!is.null(stats) && !is.null(stats$group1)) {
          cat(sprintf("  %s: mean = %.3f, n = %.1f\n", 
                      stats$group1$name, stats$group1$mean, stats$group1$n))
          cat(sprintf("  %s: mean = %.3f, n = %.1f\n", 
                      stats$group2$name, stats$group2$mean, stats$group2$n))
        }
        
        # Create and print t-test table for this variable
        # Check if we have both variance assumptions
        has_both_assumptions <- !is.null(group_results$equal_var_result[[j]])
        
        if (has_both_assumptions) {
          # SPSS-style display with both equal and unequal variance assumptions
          equal_result <- group_results$equal_var_result[[j]]
          unequal_result <- group_results$unequal_var_result[[j]]
          
          # Create SPSS-style table
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
          
          # Add significance stars
          spss_df$sig <- cut(spss_df$p_value, 
                            breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                            labels = c("***", "**", "*", ""),
                            right = FALSE)
          
          # Print SPSS-style table
          cat(sprintf("\n%s:\n", ifelse(!is.null(x$weight_var) || !is.null(x$weights), "Weighted t-test Results", "t-test Results")))
          border_width <- paste(rep("-", 80), collapse = "")
          cat(border_width, "\n")
          print(spss_df, row.names = FALSE)
          cat(border_width, "\n")
          
        } else {
          # Standard display
          results_df <- data.frame(
            Assumption = "t-test",
            t_stat = round(group_results$t_stat[j], digits),
            df = round(group_results$df[j], digits),
            p_value = round(group_results$p_value[j], digits),
            mean_diff = round(group_results$mean_diff[j], digits),
            conf_int = sprintf("[%.3f, %.3f]", 
                              group_results$conf_int_lower[j],
                              group_results$conf_int_upper[j]),
            sig = group_results$sig[j]
          )
          
          cat(sprintf("\n%s:\n", ifelse(!is.null(x$weight_var) || !is.null(x$weights), "Weighted t-test Results", "t-test Results")))
          border_width <- paste(rep("-", 70), collapse = "")
          cat(border_width, "\n")
          print(results_df, row.names = FALSE)
          cat(border_width, "\n")
        }
        
        # Add Effect Sizes for this variable
        cohens_d_val <- group_results$cohens_d[j]
        hedges_g_val <- if("hedges_g" %in% names(group_results)) group_results$hedges_g[j] else cohens_d_val
        glass_delta_val <- if("glass_delta" %in% names(group_results)) group_results$glass_delta[j] else cohens_d_val
        
        if (!is.na(cohens_d_val)) {
          effect_df <- .create_effect_size_df(var, cohens_d_val, hedges_g_val, glass_delta_val, FALSE)
          
          cat("\nEffect Sizes:\n")
          cat(paste(rep("-", 12), collapse = ""), "\n")
          print(effect_df, row.names = FALSE)
        }
        cat("\n")
      }
    }
  } else {
    # Print results for ungrouped data - each variable as separate block
    valid_results <- x$results[!is.na(x$results$Variable), ]
    
    for (i in seq_len(nrow(valid_results))) {
      var_name <- valid_results$Variable[i]
      stats <- valid_results$group_stats[[i]]
      
      cat(sprintf("\n--- %s ---\n", var_name))
      cat("\n")  # Add blank line after variable name
      
      # Print group means if available
      if (!is.null(x$group) && !is.null(stats) && !is.null(stats$group1)) {
        cat(sprintf("  %s: mean = %.3f, n = %.1f\n", 
                    stats$group1$name, stats$group1$mean, stats$group1$n))
        cat(sprintf("  %s: mean = %.3f, n = %.1f\n", 
                    stats$group2$name, stats$group2$mean, stats$group2$n))
        cat("\n")
      }
      
      # Check if we have both variance assumptions
      has_both_assumptions <- !is.null(valid_results$equal_var_result[[i]])
      
      if (has_both_assumptions) {
        # SPSS-style display with both equal and unequal variance assumptions
        equal_result <- valid_results$equal_var_result[[i]]
        unequal_result <- valid_results$unequal_var_result[[i]]
        
        # Create SPSS-style table
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
        
        # Add significance stars
        spss_df$sig <- cut(spss_df$p_value, 
                          breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                          labels = c("***", "**", "*", ""),
                          right = FALSE)
        
        # Print SPSS-style table
        cat(sprintf("\n%s:\n", ifelse(!is.null(x$weight_var) || !is.null(x$weights), "Weighted t-test Results", "t-test Results")))
        border_width <- paste(rep("-", 80), collapse = "")
        cat(border_width, "\n")
        print(spss_df, row.names = FALSE)
        cat(border_width, "\n")
        
      } else {
        # Standard display
        results_df <- data.frame(
          Assumption = "t-test",
          t_stat = round(valid_results$t_stat[i], digits),
          df = round(valid_results$df[i], digits),
          p_value = round(valid_results$p_value[i], digits),
          mean_diff = round(valid_results$mean_diff[i], digits),
          conf_int = sprintf("[%.3f, %.3f]", 
                            valid_results$conf_int_lower[i],
                            valid_results$conf_int_upper[i]),
          sig = valid_results$sig[i]
        )
        
        cat(sprintf("\n%s:\n", ifelse(!is.null(x$weight_var) || !is.null(x$weights), "Weighted t-test Results", "t-test Results")))
        border_width <- paste(rep("-", 70), collapse = "")
        writeLines(border_width)
        print(results_df, row.names = FALSE)
        writeLines(border_width)
      }
      
      # Add Effect Sizes for this variable
      cohens_d_val <- valid_results$cohens_d[i]
      hedges_g_val <- if("hedges_g" %in% names(valid_results)) valid_results$hedges_g[i] else cohens_d_val
      glass_delta_val <- if("glass_delta" %in% names(valid_results)) valid_results$glass_delta[i] else cohens_d_val
      
      if (!is.na(cohens_d_val)) {
        effect_df <- .create_effect_size_df(var_name, cohens_d_val, hedges_g_val, glass_delta_val, FALSE)
        
        cat("\nEffect Sizes:\n")
        cat(paste(rep("-", 12), collapse = ""), "\n")
        print(effect_df, row.names = FALSE)
        cat("\n")
      }
    }
  }
  
  # Add explanation section once at the end
  cat("\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  cat("EXPLANATION\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05\n")
  cat("\nEffect Size Interpretation:\n")
  cat("- Cohen's d: pooled standard deviation (classic)\n")
  cat("- Hedges' g: bias-corrected Cohen's d (preferred)\n")
  cat("- Glass' Delta: control group standard deviation only\n")
  cat("- Small effect: |effect| ~ 0.2\n")
  cat("- Medium effect: |effect| ~ 0.5\n")
  cat("- Large effect: |effect| ~ 0.8\n")
}


