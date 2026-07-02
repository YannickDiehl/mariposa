#' Compare All Groups More Conservatively After ANOVA
#'
#' @description
#' \code{scheffe_test()} tells you which groups differ after ANOVA, using the most
#' conservative approach. It's like Tukey's test but even more careful about
#' avoiding false positives.
#'
#' Think of it as:
#' - The most cautious post-hoc test available
#' - A way to compare groups when sample sizes are very unequal
#' - Insurance against finding differences that aren't real
#'
#' @param x ANOVA results from \code{oneway_anova()}
#' @param conf.level Confidence level for intervals (Default: 0.95 = 95%)
#' @param ... Additional arguments (currently unused)
#'
#' @return Pairwise comparison results showing:
#' - Which group pairs are significantly different
#' - Size of the difference between each pair
#' - Adjusted p-values (extra conservative)
#' - Confidence intervals for each difference
#'
#' @details
#' ## Understanding the Results
#'
#' **Adjusted P-values**: Extra conservative to prevent false positives
#' - p < 0.05: Groups are significantly different (you can be very confident)
#' - p ≥ 0.05: No significant difference between these groups
#' - Scheffe adjustments are stricter than other methods
#'
#' **Confidence Intervals**: Wider than Tukey's
#' - Do not include 0: Groups differ significantly
#' - Include 0: No significant difference
#' - Wider intervals reflect extra caution
#'
#' ## When to Use Scheffe Test
#'
#' Use Scheffe test when:
#' - Your ANOVA shows significant differences (p < 0.05)
#' - Group sizes are very unequal
#' - You want to be extra cautious about false positives
#' - You might test complex comparisons (not just pairs)
#' - Sample sizes are small
#'
#' ## Scheffe vs. Tukey
#'
#' **Scheffe Test:**
#' - Most conservative (hardest to find differences)
#' - Best for unequal group sizes
#' - Protects against all possible comparisons
#' - Wider confidence intervals
#'
#' **Tukey Test:**
#' - Less conservative (easier to find differences)
#' - Best for equal group sizes
#' - Protects only pairwise comparisons
#' - Narrower confidence intervals
#'
#' ## Reading the Output
#'
#' Example: "Group A - Group B: Diff = 3.2, p = 0.082"
#' - Group A's average is 3.2 units higher than Group B's
#' - This difference is NOT significant with Scheffe (p > 0.05)
#' - It might be significant with less conservative tests
#'
#' ## Tips for Success
#'
#' - Scheffe may not find differences even when ANOVA does
#' - This is normal - it's being extra careful
#' - Consider Tukey if group sizes are similar
#' - Report which post-hoc test you used and why
#' - Focus on confidence intervals, not just p-values
#'
#' @seealso
#' \code{\link{oneway_anova}} for performing ANOVA tests.
#'
#' \code{\link{tukey_test}} for Tukey HSD post-hoc tests.
#'
#' \code{\link{levene_test}} for testing homogeneity of variances.
#'
#' @references
#' Scheffe, H. (1953). A method for judging all contrasts in the analysis of variance.
#' Biometrika, 40(1-2), 87-110.
#'
#' Scheffe, H. (1959). The Analysis of Variance. New York: Wiley.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#'
#' # Perform ANOVA followed by Scheffe post-hoc test
#' anova_result <- survey_data %>%
#'   oneway_anova(life_satisfaction, group = education)
#'
#' # Scheffe post-hoc comparisons
#' anova_result %>% scheffe_test()
#'
#' # Multiple variables
#' anova_result_multi <- survey_data %>%
#'   oneway_anova(life_satisfaction, income, group = education)
#'
#' anova_result_multi %>% scheffe_test()
#'
#' # Weighted analysis
#' anova_weighted <- survey_data %>%
#'   oneway_anova(life_satisfaction, group = education, weights = sampling_weight)
#'
#' anova_weighted %>% scheffe_test()
#'
#' # Grouped analysis
#' anova_grouped <- survey_data %>%
#'   group_by(region) %>%
#'   oneway_anova(life_satisfaction, group = education)
#'
#' anova_grouped %>% scheffe_test()
#'
#' # Custom confidence level (99%)
#' anova_result %>% scheffe_test(conf.level = 0.99)
#'
#' @family posthoc
#' @export
scheffe_test <- function(x, conf.level = 0.95, ...) {
  UseMethod("scheffe_test")
}

#' @rdname scheffe_test
#' @export
scheffe_test.default <- function(x, conf.level = 0.95, ...) {
  cls <- paste(class(x), collapse = "/")
  cli_abort(c(
    "{.fn scheffe_test} is not available for objects of class {.cls {cls}}.",
    "i" = "Scheffe's test requires results from {.fn oneway_anova}.",
    "i" = "Example: {.code oneway_anova(data, dv, group) |> scheffe_test()}"
  ))
}

#' @export
scheffe_test.oneway_anova <- function(x, conf.level = 0.95, ...) {
  # Shared engine in R/posthoc-pairwise.R (Scheffe-specific core in
  # .scheffe_stats(): (k-1)*F critical values)
  .pairwise_posthoc(x, method = "scheffe", conf.level = conf.level)
}

#' Print Scheffe test results (compact)
#'
#' @description
#' Compact print method for objects of class \code{"scheffe_test"}. Shows
#' one line per variable (or factor, or group combination) with the number
#' of pairwise comparisons and how many are significant at the .05 level.
#'
#' For the full comparison tables (mean differences, confidence intervals,
#' adjusted p-values), use \code{summary()}.
#'
#' @param x An object of class \code{"scheffe_test"} returned by \code{\link{scheffe_test}}.
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments passed to \code{\link[base]{print}}. Currently unused.
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- oneway_anova(survey_data, life_satisfaction,
#'                        group = education) |> scheffe_test()
#' result              # compact overview
#' summary(result)     # full comparison tables
#'
#' @export
#' @method print scheffe_test
print.scheffe_test <- function(x, digits = 3, ...) {
  weighted_tag <- if (!is.null(x$weights)) " [Weighted]" else ""
  group_tag <- if (!is.null(x$group)) paste0(" by ", x$group) else ""
  title <- sprintf("Scheffe Post-Hoc Test%s%s", group_tag, weighted_tag)

  # Shared compact print core in R/posthoc-pairwise.R
  .print_posthoc_compact(
    title      = title,
    results    = x$results,
    p_col      = "p_adjusted",
    group_vars = if (isTRUE(x$is_grouped)) x$groups else NULL,
    by_col     = if (isTRUE(x$is_factorial)) "Factor" else "Variable"
  )
  invisible(x)
}

#' Summary method for Scheffe test results
#'
#' @description
#' Creates a summary object that produces detailed output when printed,
#' including the pairwise comparison tables with mean differences,
#' confidence intervals, Scheffe-adjusted p-values, and interpretation.
#'
#' @param object A \code{scheffe_test} result object.
#' @param parameters Logical. Show test parameters (confidence level,
#'   method notes)? (Default: TRUE)
#' @param comparisons Logical. Show the pairwise comparison tables?
#'   (Default: TRUE)
#' @param interpretation Logical. Show the interpretation section?
#'   (Default: TRUE)
#' @param digits Number of decimal places for formatting (Default: 3).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.scheffe_test} object.
#'
#' @examples
#' result <- oneway_anova(survey_data, life_satisfaction,
#'                        group = education) |> scheffe_test()
#' summary(result)
#' summary(result, interpretation = FALSE)
#'
#' @seealso \code{\link{scheffe_test}} for the main analysis function.
#' @export
#' @method summary scheffe_test
summary.scheffe_test <- function(object, parameters = TRUE, comparisons = TRUE,
                                 interpretation = TRUE, digits = 3, ...) {
  build_summary_object(
    object     = object,
    show       = list(parameters = parameters, comparisons = comparisons,
                      interpretation = interpretation),
    digits     = digits,
    class_name = "summary.scheffe_test"
  )
}

#' Print summary of Scheffe test results (detailed output)
#'
#' @description
#' Displays the detailed output for Scheffe post-hoc comparisons, with
#' sections controlled by the boolean parameters passed to
#' \code{\link{summary.scheffe_test}}. The display includes:
#' \itemize{
#'   \item Pairwise group comparisons with mean differences
#'   \item Confidence intervals for differences (widest among all post-hoc tests)
#'   \item Scheffe-adjusted p-values controlling family-wise error rate
#'   \item Significance indicators (* p < 0.05, ** p < 0.01, *** p < 0.001)
#' }
#'
#' For grouped analyses, results are displayed separately for each group combination.
#' For weighted analyses, effective sample sizes are used in calculations.
#'
#' @param x A \code{summary.scheffe_test} object created by
#'   \code{\link{summary.scheffe_test}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' result <- oneway_anova(survey_data, life_satisfaction,
#'                        group = education) |> scheffe_test()
#' summary(result)                       # all sections
#' summary(result, comparisons = FALSE)  # hide comparison tables
#'
#' @seealso \code{\link{scheffe_test}} for the main analysis,
#'   \code{\link{summary.scheffe_test}} for summary options.
#' @export
#' @method print summary.scheffe_test
print.summary.scheffe_test <- function(x, ...) {
  # Shared verbose implementation in R/posthoc-pairwise.R
  .print_pairwise_posthoc(
    x,
    digits = x$digits,
    title = "Scheffe Post-Hoc Test",
    method_notes = c(
      "Family-wise error rate controlled using Scheffe's method",
      "Note: Most conservative post-hoc test (widest confidence intervals)"
    ),
    results_label = "Scheffe",
    interpretation_notes = c(
      "- p-values are adjusted for all possible contrasts (most conservative)",
      "- Scheffe test has wider CIs than Tukey HSD"
    ),
    show = x$show
  )
  invisible(x)
}
