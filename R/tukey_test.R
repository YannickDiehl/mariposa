
#' Find Which Specific Groups Differ After ANOVA
#'
#' @description
#' \code{tukey_test()} tells you exactly which groups are different from each other
#' after ANOVA finds overall differences. It's like a follow-up investigation that
#' pinpoints where the differences lie.
#'
#' Think of it as:
#' - ANOVA says "there are differences somewhere"
#' - Tukey test says "specifically, Group A differs from Group C"
#' - A way to make all possible comparisons while controlling error rates
#'
#' @param x ANOVA results from \code{oneway_anova()}
#' @param conf.level Confidence level for intervals (Default: 0.95 = 95%)
#' @param ... Additional arguments (currently unused)
#'
#' @return Pairwise comparison results showing:
#' - Which group pairs are significantly different
#' - Size of the difference between each pair
#' - Adjusted p-values (controlling for multiple comparisons)
#' - Confidence intervals for each difference
#'
#' @details
#' ## Understanding the Results
#'
#' **Adjusted P-values**: Control for multiple comparisons
#' - p < 0.05: Groups are significantly different
#' - p ≥ 0.05: No significant difference between these groups
#' - When you make many comparisons, chance alone could produce false positives
#' - Tukey adjustment protects against this by being more conservative
#'
#' **Mean Differences**:
#' - Positive: First group has higher average than second
#' - Negative: Second group has higher average than first
#' - Zero in confidence interval: No significant difference
#'
#' ## When to Use Tukey Test
#'
#' Use Tukey test when:
#' - Your ANOVA shows significant differences (p < 0.05)
#' - You want to know which specific groups differ
#' - You need to compare all possible pairs
#' - Group sizes are roughly equal
#' - Variances are roughly equal across groups
#'
#' ## Tukey vs. Scheffe
#'
#' **Tukey Test:**
#' - Less conservative (easier to find differences)
#' - Best for equal group sizes
#' - Protects only pairwise comparisons
#' - Narrower confidence intervals
#'
#' **Scheffe Test:**
#' - Most conservative (hardest to find differences)
#' - Best for unequal group sizes
#' - Protects against all possible comparisons
#' - Wider confidence intervals
#'
#' ## Reading the Output
#'
#' Example: "Group A - Group B: Diff = 3.2, p = 0.012"
#' - Group A's average is 3.2 units higher than Group B's
#' - This difference is statistically significant (p < 0.05)
#' - You can be confident these groups truly differ
#'
#' ## Tips for Success
#'
#' - Only run post-hoc tests if ANOVA is significant
#' - Focus on comparisons that make theoretical sense
#' - Consider practical significance, not just statistical
#' - Report both the difference and its confidence interval
#' - Remember: non-significant doesn't mean "exactly equal"
#'
#' @seealso 
#' \code{\link{oneway_anova}} for performing ANOVA tests.
#' 
#' \code{\link[stats]{TukeyHSD}} for the base R Tukey HSD function.
#' 
#' \code{\link{levene_test}} for testing homogeneity of variances.
#' 
#' @references
#' Tukey, J. W. (1949). Comparing individual means in the analysis of variance. 
#' Biometrics, 5(2), 99-114.
#' 
#' Kramer, C. Y. (1956). Extension of multiple range tests to group means with 
#' unequal numbers of replications. Biometrics, 12(3), 307-310.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Perform ANOVA followed by Tukey post-hoc test
#' anova_result <- survey_data %>%
#'   oneway_anova(life_satisfaction, group = education)
#'
#' # Tukey post-hoc comparisons
#' anova_result %>% tukey_test()
#'
#' # With weights
#' anova_weighted <- survey_data %>%
#'   oneway_anova(life_satisfaction, group = education, weights = sampling_weight)
#'
#' anova_weighted %>% tukey_test()
#'
#' # Multiple variables
#' anova_multi <- survey_data %>%
#'   oneway_anova(trust_government, trust_science, group = education)
#'
#' anova_multi %>% tukey_test()
#'
#' # Grouped analysis
#' anova_grouped <- survey_data %>%
#'   group_by(region) %>%
#'   oneway_anova(life_satisfaction, group = education)
#'
#' anova_grouped %>% tukey_test()
#'
#' @family posthoc
#' @export
tukey_test <- function(x, conf.level = 0.95, ...) {
  UseMethod("tukey_test")
}

#' @rdname tukey_test
#' @export
tukey_test.default <- function(x, conf.level = 0.95, ...) {
  cls <- paste(class(x), collapse = "/")
  cli_abort(c(
    "{.fn tukey_test} is not available for objects of class {.cls {cls}}.",
    "i" = "Tukey HSD requires results from {.fn oneway_anova}.",
    "i" = "Example: {.code oneway_anova(data, dv, group) |> tukey_test()}"
  ))
}

#' @export
tukey_test.oneway_anova <- function(x, conf.level = 0.95, ...) {
  # Shared engine in R/posthoc-pairwise.R (Tukey-specific cores in
  # .tukey_stats(); unweighted path delegates to stats::TukeyHSD)
  .pairwise_posthoc(x, method = "tukey", conf.level = conf.level)
}
#' Print Tukey HSD test results
#'
#' @description
#' Print method for objects of class \code{"tukey_test"}. Provides a 
#' formatted display of Tukey post-hoc test results including pairwise 
#' comparisons, confidence intervals, and adjusted p-values.
#'
#' @param x An object of class \code{"tukey_test"} returned by \code{\link{tukey_test}}.
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments passed to \code{\link[base]{print}}. Currently unused.
#'
#' @details
#' The print method displays:
#' \itemize{
#'   \item Pairwise group comparisons with mean differences
#'   \item Confidence intervals for differences
#'   \item Tukey-adjusted p-values controlling family-wise error rate
#'   \item Significance indicators (* p < 0.05, ** p < 0.01, *** p < 0.001)
#' }
#' 
#' For grouped analyses, results are displayed separately for each group combination.
#' For weighted analyses, effective sample sizes are used in calculations.
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @export
print.tukey_test <- function(x, digits = 3, ...) {
  # Shared print implementation in R/posthoc-pairwise.R
  .print_pairwise_posthoc(
    x,
    digits = digits,
    title = "Tukey HSD Post-Hoc Test",
    method_notes = "Family-wise error rate controlled using Tukey HSD",
    results_label = "Tukey",
    interpretation_notes = "- p-values are adjusted for multiple comparisons (family-wise error control)"
  )
}
