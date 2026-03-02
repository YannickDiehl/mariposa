# Test print helper functions
library(testthat)
library(mariposa)

# ============================================================================
# print_header()
# ============================================================================

test_that("print_header prints title and dash separator", {
  output <- capture.output(mariposa:::print_header("My Title"))
  # First line is empty (newline_before = TRUE), second is the title, third is dashes

  expect_equal(output[1], "")
  expect_equal(output[2], "My Title")
  expect_equal(output[3], "--------")
})

test_that("print_header omits leading newline when newline_before = FALSE", {
  output <- capture.output(mariposa:::print_header("Test", newline_before = FALSE))
  expect_equal(output[1], "Test")
  expect_equal(output[2], "----")
  expect_length(output, 2)
})

test_that("print_header dash length matches title length", {
  title <- "A Very Long Title For Testing Purposes"
  output <- capture.output(mariposa:::print_header(title, newline_before = FALSE))
  expect_equal(nchar(output[2]), nchar(title))
})

test_that("print_header handles single-character title", {
  output <- capture.output(mariposa:::print_header("X", newline_before = FALSE))
  expect_equal(output[1], "X")
  expect_equal(output[2], "-")
})

test_that("print_header handles empty string title", {
  output <- capture.output(mariposa:::print_header("", newline_before = FALSE))
  expect_equal(output[1], "")
  # nchar("") = 0, so no dashes
  expect_equal(output[2], "")
})

# ============================================================================
# print_info_section()
# ============================================================================

test_that("print_info_section prints named list as key-value pairs", {
  info <- list("Test" = "t-test", "Method" = "Independent")
  output <- capture.output(mariposa:::print_info_section(info))
  expect_equal(output[1], "- Test: t-test")
  expect_equal(output[2], "- Method: Independent")
})

test_that("print_info_section applies indentation", {
  info <- list("Key" = "Value")
  output <- capture.output(mariposa:::print_info_section(info, indent = 4))
  expect_equal(output[1], "    - Key: Value")
})

test_that("print_info_section skips NULL values", {
  info <- list("Present" = "yes", "Missing" = NULL, "Also" = "here")
  output <- capture.output(mariposa:::print_info_section(info))
  expect_length(output, 2)
  expect_equal(output[1], "- Present: yes")
  expect_equal(output[2], "- Also: here")
})

test_that("print_info_section skips NA values", {
  info <- list("Present" = "yes", "Missing" = NA)
  output <- capture.output(mariposa:::print_info_section(info))
  expect_length(output, 1)
  expect_equal(output[1], "- Present: yes")
})

test_that("print_info_section skips empty string values", {
  info <- list("Present" = "yes", "Empty" = "")
  output <- capture.output(mariposa:::print_info_section(info))
  expect_length(output, 1)
  expect_equal(output[1], "- Present: yes")
})

test_that("print_info_section handles numeric values", {
  info <- list("N" = 100, "Mean" = 3.14)
  output <- capture.output(mariposa:::print_info_section(info))
  expect_equal(output[1], "- N: 100")
  expect_equal(output[2], "- Mean: 3.14")
})

test_that("print_info_section produces no output for all-NULL list", {
  info <- list("A" = NULL, "B" = NULL)
  output <- capture.output(mariposa:::print_info_section(info))
  expect_length(output, 0)
})

test_that("print_info_section handles zero indent", {
  info <- list("Key" = "Value")
  output <- capture.output(mariposa:::print_info_section(info, indent = 0))
  expect_equal(output[1], "- Key: Value")
})

# ============================================================================
# add_significance_stars()
# ============================================================================

test_that("add_significance_stars returns *** for p < 0.001", {
  result <- mariposa:::add_significance_stars(0.0001)
  expect_equal(as.character(result), "***")
})

test_that("add_significance_stars returns ** for p in [0.001, 0.01)", {
  result <- mariposa:::add_significance_stars(0.005)
  expect_equal(as.character(result), "**")

  result_boundary <- mariposa:::add_significance_stars(0.001)
  expect_equal(as.character(result_boundary), "**")
})

test_that("add_significance_stars returns * for p in [0.01, 0.05)", {

  result <- mariposa:::add_significance_stars(0.03)
  expect_equal(as.character(result), "*")

  result_boundary <- mariposa:::add_significance_stars(0.01)
  expect_equal(as.character(result_boundary), "*")
})

test_that("add_significance_stars returns empty for p >= 0.05", {
  result <- mariposa:::add_significance_stars(0.05)
  expect_equal(as.character(result), "")

  result_high <- mariposa:::add_significance_stars(0.99)
  expect_equal(as.character(result_high), "")
})

test_that("add_significance_stars returns empty for NA p-value", {
  result <- mariposa:::add_significance_stars(NA)
  expect_equal(result, "")
})

test_that("add_significance_stars handles p = 0", {
  result <- mariposa:::add_significance_stars(0)
  expect_equal(as.character(result), "***")
})

test_that("add_significance_stars handles p = 1", {
  result <- mariposa:::add_significance_stars(1)
  expect_equal(as.character(result), "")
})

test_that("add_significance_stars respects custom breaks and labels", {
  result <- mariposa:::add_significance_stars(
    0.08,
    breaks = c(-Inf, 0.01, 0.10, Inf),
    labels = c("!!", "!", "")
  )
  expect_equal(as.character(result), "!")
})

# ============================================================================
# print_significance_legend()
# ============================================================================

test_that("print_significance_legend prints legend when show = TRUE", {
  output <- capture.output(mariposa:::print_significance_legend(show = TRUE))
  # output[1] is the blank line from cat("\n"), output[2] is the legend line
  legend_text <- paste(output, collapse = "\n")
  expect_true(grepl("Signif. codes", legend_text))
  expect_true(grepl("\\*\\*\\*", legend_text))
  expect_true(grepl("0.001", legend_text))
  expect_true(grepl("0.05", legend_text))
})

test_that("print_significance_legend prints nothing when show = FALSE", {
  output <- capture.output(mariposa:::print_significance_legend(show = FALSE))
  expect_length(output, 0)
})

# ============================================================================
# print_group_header()
# ============================================================================

test_that("print_group_header prints named vector group values", {
  group_values <- c("region" = "North")
  output <- capture.output(mariposa:::print_group_header(group_values))
  # First line blank (from leading \n), second is header text, third is dashes
  expect_equal(output[2], "Group: region = North")
  expect_equal(nchar(output[3]), nchar("Group: region = North"))
})

test_that("print_group_header handles multiple group values", {
  group_values <- c("region" = "North", "gender" = "Female")
  output <- capture.output(mariposa:::print_group_header(group_values))
  expect_equal(output[2], "Group: region = North, gender = Female")
})

test_that("print_group_header uses custom prefix", {
  group_values <- c("city" = "Berlin")
  output <- capture.output(mariposa:::print_group_header(group_values, prefix = "Filter"))
  expect_equal(output[2], "Filter: city = Berlin")
})

test_that("print_group_header handles data frame input", {
  group_df <- data.frame(region = "South", stringsAsFactors = FALSE)
  output <- capture.output(mariposa:::print_group_header(group_df))
  expect_equal(output[2], "Group: region = South")
})

test_that("print_group_header handles data frame with factor column", {
  group_df <- data.frame(region = factor("West"), stringsAsFactors = FALSE)
  output <- capture.output(mariposa:::print_group_header(group_df))
  expect_equal(output[2], "Group: region = West")
})

test_that("print_group_header handles multi-column data frame", {
  group_df <- data.frame(
    region = "East",
    gender = "Male",
    stringsAsFactors = FALSE
  )
  output <- capture.output(mariposa:::print_group_header(group_df))
  expect_equal(output[2], "Group: region = East, gender = Male")
})

test_that("print_group_header dash separator matches header text length", {
  group_values <- c("variable" = "value")
  output <- capture.output(mariposa:::print_group_header(group_values))
  header_text <- output[2]
  dash_line <- output[3]
  expect_equal(nchar(dash_line), nchar(header_text))
})

# ============================================================================
# pad_utf8()
# ============================================================================

test_that("pad_utf8 left-aligns by default", {
  result <- mariposa:::pad_utf8("abc", 10)
  expect_equal(nchar(result), 10)
  expect_true(grepl("^abc", result))
  expect_true(grepl("\\s+$", result))
})

test_that("pad_utf8 right-aligns when requested", {
  result <- mariposa:::pad_utf8("abc", 10, align = "right")
  expect_equal(nchar(result), 10)
  expect_true(grepl("abc$", result))
  expect_true(grepl("^\\s+", result))
})

test_that("pad_utf8 handles German umlauts correctly", {
  # "ae" in UTF-8 uses 2 bytes but 1 display character
  text_umlaut <- "\u00e4"  # a-umlaut
  result <- mariposa:::pad_utf8(text_umlaut, 5)
  # Display width should be 5 characters
  expect_equal(nchar(result, type = "chars"), 5)
})

test_that("pad_utf8 handles multiple umlauts", {
  text <- "\u00dc\u00f6\u00e4"  # Ueoe ae
  result <- mariposa:::pad_utf8(text, 10)
  # 3 display chars + 7 spaces = 10 display chars
  expect_equal(nchar(result, type = "chars"), 10)
})

test_that("pad_utf8 handles text exactly matching width", {
  result <- mariposa:::pad_utf8("exact", 5)
  expect_equal(result, "exact")
  expect_equal(nchar(result), 5)
})

test_that("pad_utf8 handles text longer than width", {
  # sprintf does not truncate, so text wider than width is preserved
  result <- mariposa:::pad_utf8("toolong", 3)
  expect_true(grepl("toolong", result))
})

test_that("pad_utf8 converts NA to string NA", {
  result <- mariposa:::pad_utf8(NA, 5)
  expect_true(grepl("NA", result))
  expect_equal(nchar(result, type = "chars"), 5)
})

test_that("pad_utf8 converts numeric input to character", {
  result <- mariposa:::pad_utf8(42, 6)
  expect_true(grepl("42", result))
  expect_equal(nchar(result, type = "chars"), 6)
})

test_that("pad_utf8 handles empty string", {
  result <- mariposa:::pad_utf8("", 5)
  expect_equal(nchar(result), 5)
  expect_equal(trimws(result), "")
})

test_that("pad_utf8 handles umlaut with right alignment", {
  text <- "M\u00fcnchen"  # Muenchen
  result <- mariposa:::pad_utf8(text, 15, align = "right")
  expect_equal(nchar(result, type = "chars"), 15)
  expect_true(grepl("M\u00fcnchen$", result))
})

# ============================================================================
# get_table_width()
# ============================================================================

test_that("get_table_width returns min_width for NULL input", {
  result <- mariposa:::get_table_width(NULL)
  expect_equal(result, 40)
})
test_that("get_table_width returns min_width for empty data frame", {
  result <- mariposa:::get_table_width(data.frame())
  expect_equal(result, 40)
})

test_that("get_table_width returns at least min_width for narrow data frame", {
  df <- data.frame(a = 1)
  result <- mariposa:::get_table_width(df)
  expect_gte(result, 40)
})

test_that("get_table_width returns custom min_width", {
  result <- mariposa:::get_table_width(NULL, min_width = 60)
  expect_equal(result, 60)
})

test_that("get_table_width returns larger width for wide data frame", {
  df <- data.frame(
    very_long_column_name_one = 1:3,
    very_long_column_name_two = 4:6,
    very_long_column_name_three = 7:9
  )
  result <- mariposa:::get_table_width(df)
  # The printed output should be wider than the default 40
  expect_gte(result, 40)
})

test_that("get_table_width returns numeric value", {
  df <- data.frame(x = 1:5, y = 6:10)
  result <- mariposa:::get_table_width(df)
  expect_type(result, "double")
})

# ============================================================================
# print_separator()
# ============================================================================

test_that("print_separator prints 40 dashes", {
  output <- capture.output(mariposa:::print_separator())
  expect_equal(output[1], paste(rep("-", 40), collapse = ""))
  expect_equal(nchar(output[1]), 40)
})

test_that("print_separator ignores extra arguments", {
  output <- capture.output(mariposa:::print_separator("ignored", 123))
  expect_equal(nchar(output[1]), 40)
})

# ============================================================================
# format_number()
# ============================================================================

test_that("format_number formats to 3 decimal places by default", {
  result <- mariposa:::format_number(3.14159)
  expect_equal(trimws(result), "3.142")
})

test_that("format_number respects custom digits", {
  result <- mariposa:::format_number(3.14159, digits = 2)
  expect_equal(trimws(result), "3.14")
})

test_that("format_number returns NA string for NA input", {
  result <- mariposa:::format_number(NA)
  expect_equal(result, "NA")
})

test_that("format_number pads with trailing zeros", {
  result <- mariposa:::format_number(1, digits = 3)
  expect_equal(trimws(result), "1.000")
})

test_that("format_number handles zero", {
  result <- mariposa:::format_number(0, digits = 3)
  expect_equal(trimws(result), "0.000")
})

test_that("format_number handles negative numbers", {
  result <- mariposa:::format_number(-2.5, digits = 2)
  expect_equal(trimws(result), "-2.50")
})

test_that("format_number uses scientific notation for very small values when requested", {
  result <- mariposa:::format_number(0.00001, digits = 3, scientific = TRUE)
  expect_true(grepl("[eE]", result))
})

test_that("format_number without scientific flag falls through to format(round())", {
  # When scientific = FALSE, the function calls format(round(x, digits), nsmall = digits)
  # R's format() may still use scientific notation for very small numbers
  result <- mariposa:::format_number(0.00001, digits = 5, scientific = FALSE)
  # The function does NOT force scientific = FALSE in format(), so R decides
  expect_type(result, "character")
})

test_that("format_number scientific threshold is 0.0001", {
  # Value at threshold (abs(x) == 0.0001, NOT < 0.0001) should NOT trigger
  # the explicit scientific branch
  result_at <- mariposa:::format_number(0.0001, digits = 4, scientific = TRUE)
  # abs(0.0001) < 0.0001 is FALSE, so it falls through to format(round())
  expect_type(result_at, "character")

  # Value below threshold should trigger explicit scientific notation
  result_below <- mariposa:::format_number(0.00009, digits = 3, scientific = TRUE)
  expect_true(grepl("[eE]", result_below))
})

test_that("format_number handles large numbers", {
  result <- mariposa:::format_number(123456.789, digits = 2)
  expect_equal(trimws(result), "123456.79")
})

# ============================================================================
# get_standard_title()
# ============================================================================

test_that("get_standard_title creates unweighted title by default", {
  result <- mariposa:::get_standard_title("T-Test")
  expect_equal(result, "T-Test Results")
})

test_that("get_standard_title adds Weighted prefix when weights provided", {
  result <- mariposa:::get_standard_title("T-Test", weights = "w")
  expect_equal(result, "Weighted T-Test Results")
})

test_that("get_standard_title uses custom suffix", {
  result <- mariposa:::get_standard_title("Descriptive", suffix = "Statistics")
  expect_equal(result, "Descriptive Statistics")
})

test_that("get_standard_title combines weights and custom suffix", {
  result <- mariposa:::get_standard_title("ANOVA", weights = "w", suffix = "Table")
  expect_equal(result, "Weighted ANOVA Table")
})

test_that("get_standard_title treats NULL weights as unweighted", {
  result <- mariposa:::get_standard_title("Chi-Square", weights = NULL)
  expect_equal(result, "Chi-Square Results")
})

test_that("get_standard_title handles empty test name", {
  result <- mariposa:::get_standard_title("")
  expect_equal(result, " Results")
})

# ============================================================================
# print_test_parameters()
# ============================================================================

test_that("print_test_parameters prints confidence level", {
  output <- capture.output(
    mariposa:::print_test_parameters(list(conf.level = 0.95))
  )
  expect_length(output, 1)
  expect_equal(output[1], "- Confidence level: 95.0%")
})

test_that("print_test_parameters prints alternative hypothesis", {
  output <- capture.output(
    mariposa:::print_test_parameters(list(alternative = "two.sided"))
  )
  expect_length(output, 1)
  expect_equal(output[1], "- Alternative hypothesis: two.sided")
})

test_that("print_test_parameters prints mu value", {
  output <- capture.output(
    mariposa:::print_test_parameters(list(mu = 0))
  )
  expect_length(output, 1)
  expect_equal(output[1], "- Null hypothesis (mu): 0.000")
})

test_that("print_test_parameters prints all parameters when provided", {
  params <- list(conf.level = 0.99, alternative = "less", mu = 5)
  output <- capture.output(mariposa:::print_test_parameters(params))
  expect_length(output, 3)
  expect_equal(output[1], "- Confidence level: 99.0%")
  expect_equal(output[2], "- Alternative hypothesis: less")
  expect_equal(output[3], "- Null hypothesis (mu): 5.000")
})

test_that("print_test_parameters prints nothing for empty list", {
  output <- capture.output(mariposa:::print_test_parameters(list()))
  expect_length(output, 0)
})

test_that("print_test_parameters skips NULL elements", {
  params <- list(conf.level = NULL, alternative = "greater")
  output <- capture.output(mariposa:::print_test_parameters(params))
  expect_length(output, 1)
  expect_equal(output[1], "- Alternative hypothesis: greater")
})

test_that("print_test_parameters handles non-standard conf.level", {
  output <- capture.output(
    mariposa:::print_test_parameters(list(conf.level = 0.90))
  )
  expect_equal(output[1], "- Confidence level: 90.0%")
})

# ============================================================================
# format_variable_name()
# ============================================================================

test_that("format_variable_name returns var when no label", {
  result <- mariposa:::format_variable_name("age")
  expect_equal(result, "age")
})

test_that("format_variable_name returns var when label is NULL", {
  result <- mariposa:::format_variable_name("age", label = NULL)
  expect_equal(result, "age")
})

test_that("format_variable_name returns var when label is NA", {
  result <- mariposa:::format_variable_name("age", label = NA)
  expect_equal(result, "age")
})

test_that("format_variable_name returns var when label is empty string", {
  result <- mariposa:::format_variable_name("age", label = "")
  expect_equal(result, "age")
})

test_that("format_variable_name returns var when label equals var", {
  result <- mariposa:::format_variable_name("age", label = "age")
  expect_equal(result, "age")
})

test_that("format_variable_name appends label in parentheses", {
  result <- mariposa:::format_variable_name("age", label = "Age in years")
  expect_equal(result, "age (Age in years)")
})

test_that("format_variable_name handles label with special characters", {
  result <- mariposa:::format_variable_name("q1", label = "Frage 1 (\u00c4nderung)")
  expect_equal(result, "q1 (Frage 1 (\u00c4nderung))")
})

# ============================================================================
# .print_cor_matrix()
# ============================================================================

test_that(".print_cor_matrix prints title and border", {
  mat <- matrix(c(1, 0.5, 0.5, 1), nrow = 2,
                dimnames = list(c("x", "y"), c("x", "y")))
  output <- capture.output(mariposa:::.print_cor_matrix(mat))
  full_text <- paste(output, collapse = "\n")
  expect_true(grepl("Correlation Matrix:", full_text))
  expect_true(grepl("---", full_text))
})

test_that(".print_cor_matrix uses custom title", {
  mat <- matrix(c(1, 0.3, 0.3, 1), nrow = 2,
                dimnames = list(c("a", "b"), c("a", "b")))
  output <- capture.output(
    mariposa:::.print_cor_matrix(mat, title = "Custom Title:")
  )
  expect_true(any(grepl("Custom Title:", output)))
})

test_that(".print_cor_matrix handles different types", {
  mat <- matrix(c(1, 0.5, 0.5, 1), nrow = 2,
                dimnames = list(c("x", "y"), c("x", "y")))

  # Correlation type
  output_corr <- capture.output(
    mariposa:::.print_cor_matrix(mat, type = "correlation")
  )
  expect_true(length(output_corr) > 0)

  # P-value type
  pmat <- matrix(c(0, 0.01, 0.01, 0), nrow = 2,
                 dimnames = list(c("x", "y"), c("x", "y")))
  output_p <- capture.output(
    mariposa:::.print_cor_matrix(pmat, type = "pvalue")
  )
  expect_true(length(output_p) > 0)

  # N type
  nmat <- matrix(c(100, 95, 95, 100), nrow = 2,
                 dimnames = list(c("x", "y"), c("x", "y")))
  output_n <- capture.output(
    mariposa:::.print_cor_matrix(nmat, type = "n")
  )
  expect_true(length(output_n) > 0)
})

test_that(".print_cor_matrix respects digits parameter", {
  mat <- matrix(c(1, 0.123456, 0.123456, 1), nrow = 2,
                dimnames = list(c("x", "y"), c("x", "y")))
  output <- capture.output(
    mariposa:::.print_cor_matrix(mat, digits = 2)
  )
  full_text <- paste(output, collapse = " ")
  expect_true(grepl("0.12", full_text))
})

test_that(".print_cor_matrix restores console width option", {
  old_width <- getOption("width")
  mat <- matrix(c(1, 0.5, 0.5, 1), nrow = 2,
                dimnames = list(c("x", "y"), c("x", "y")))
  capture.output(mariposa:::.print_cor_matrix(mat))
  expect_equal(getOption("width"), old_width)
})

test_that(".print_cor_matrix handles large matrix (>6 vars) by reducing digits", {
  n <- 8
  mat <- matrix(runif(n * n), nrow = n,
                dimnames = list(paste0("v", 1:n), paste0("v", 1:n)))
  diag(mat) <- 1
  # Should not error
  output <- capture.output(
    mariposa:::.print_cor_matrix(mat, digits = 4, type = "correlation")
  )
  expect_true(length(output) > 0)
})

# ============================================================================
# .print_single_pair()
# ============================================================================

test_that(".print_single_pair prints basic correlation output", {
  corr_row <- data.frame(
    r = 0.567,
    n = 100,
    p_value = 0.001,
    sig = "**",
    stringsAsFactors = FALSE
  )
  output <- capture.output(
    mariposa:::.print_single_pair(corr_row,
                                  stat_label = "r",
                                  stat_col = "r",
                                  corr_name = "Correlation")
  )
  full_text <- paste(output, collapse = "\n")
  expect_true(grepl("Correlation: r = 0.567", full_text))
  expect_true(grepl("Sample size: n = 100", full_text))
  expect_true(grepl("p-value: 0.0010", full_text))
  expect_true(grepl("Significance: \\*\\*", full_text))
})

test_that(".print_single_pair prints secondary statistic when provided", {
  corr_row <- data.frame(
    rho = 0.45,
    z_score = 2.33,
    n = 50,
    p_value = 0.02,
    sig = "*",
    stringsAsFactors = FALSE
  )
  output <- capture.output(
    mariposa:::.print_single_pair(corr_row,
                                  stat_label = "rho",
                                  stat_col = "rho",
                                  corr_name = "Spearman's rho",
                                  secondary_label = "Z-score",
                                  secondary_col = "z_score")
  )
  full_text <- paste(output, collapse = "\n")
  expect_true(grepl("Z-score: 2.330", full_text))
})

test_that(".print_single_pair prints confidence interval when columns provided", {
  corr_row <- data.frame(
    r = 0.5,
    ci_lower = 0.32,
    ci_upper = 0.65,
    n = 80,
    p_value = 0.0001,
    sig = "***",
    stringsAsFactors = FALSE
  )
  output <- capture.output(
    mariposa:::.print_single_pair(corr_row,
                                  stat_label = "r",
                                  stat_col = "r",
                                  ci_lower_col = "ci_lower",
                                  ci_upper_col = "ci_upper")
  )
  full_text <- paste(output, collapse = "\n")
  expect_true(grepl("95% CI: \\[0.320, 0.650\\]", full_text))
})

test_that(".print_single_pair shows 2-tailed label for two.sided alternative", {
  corr_row <- data.frame(
    r = 0.3, n = 50, p_value = 0.03, sig = "*",
    stringsAsFactors = FALSE
  )
  output <- capture.output(
    mariposa:::.print_single_pair(corr_row,
                                  stat_label = "r",
                                  stat_col = "r",
                                  alternative = "two.sided")
  )
  expect_true(any(grepl("p-value \\(2-tailed\\)", output)))
})

test_that(".print_single_pair shows 1-tailed label for one-sided alternative", {
  corr_row <- data.frame(
    r = 0.3, n = 50, p_value = 0.015, sig = "*",
    stringsAsFactors = FALSE
  )
  output_less <- capture.output(
    mariposa:::.print_single_pair(corr_row,
                                  stat_label = "r",
                                  stat_col = "r",
                                  alternative = "less")
  )
  expect_true(any(grepl("p-value \\(1-tailed\\)", output_less)))

  output_greater <- capture.output(
    mariposa:::.print_single_pair(corr_row,
                                  stat_label = "r",
                                  stat_col = "r",
                                  alternative = "greater")
  )
  expect_true(any(grepl("p-value \\(1-tailed\\)", output_greater)))
})

test_that(".print_single_pair shows plain p-value label when alternative is NULL", {
  corr_row <- data.frame(
    tau = 0.25, n = 40, p_value = 0.05, sig = "",
    stringsAsFactors = FALSE
  )
  output <- capture.output(
    mariposa:::.print_single_pair(corr_row,
                                  stat_label = "tau",
                                  stat_col = "tau",
                                  corr_name = "Kendall's tau",
                                  alternative = NULL)
  )
  expect_true(any(grepl("p-value: 0.0500", output)))
  expect_false(any(grepl("tailed", output)))
})

test_that(".print_single_pair shows ns for empty significance", {
  corr_row <- data.frame(
    r = 0.05, n = 30, p_value = 0.80, sig = "",
    stringsAsFactors = FALSE
  )
  output <- capture.output(
    mariposa:::.print_single_pair(corr_row,
                                  stat_label = "r",
                                  stat_col = "r")
  )
  expect_true(any(grepl("Significance: ns", output)))
})

test_that(".print_single_pair omits CI when columns are NULL", {
  corr_row <- data.frame(
    r = 0.5, n = 60, p_value = 0.001, sig = "**",
    stringsAsFactors = FALSE
  )
  output <- capture.output(
    mariposa:::.print_single_pair(corr_row,
                                  stat_label = "r",
                                  stat_col = "r",
                                  ci_lower_col = NULL,
                                  ci_upper_col = NULL)
  )
  expect_false(any(grepl("CI", output)))
})
