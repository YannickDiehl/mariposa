# ============================================================================
# HELPER FUNCTIONS - UNIT TESTS
# ============================================================================
# Purpose: Validate internal helper functions in R/helpers.R
# Tests: Weight validation, data processing, label handling, kurtosis
# Created: 2026-03-02
#
# All functions tested here are internal (non-exported), accessed via
# mariposa::: prefix. Tests cover edge cases, error conditions, and
# typical usage patterns.
# ============================================================================

library(testthat)
library(mariposa)
library(dplyr)

# ============================================================================
# TEST DATA
# ============================================================================

test_df <- tibble::tibble(
  id = 1:10,
  x = c(1.5, 2.3, 3.1, 4.7, 5.2, 6.0, 7.4, 8.8, 9.1, 10.0),
  y = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
  group = factor(c("A", "A", "B", "B", "C", "C", "A", "B", "C", "A")),
  wt = c(1.2, 0.8, 1.5, 1.0, 0.9, 1.3, 1.1, 0.7, 1.4, 1.0),
  char_col = letters[1:10]
)

# ============================================================================
# .are_weights() TESTS
# ============================================================================

test_that(".are_weights returns FALSE for NULL", {
  expect_false(mariposa:::.are_weights(NULL))
})

test_that(".are_weights returns TRUE for valid numeric vector", {
  expect_true(mariposa:::.are_weights(c(1.0, 2.0, 3.0)))
})

test_that(".are_weights returns TRUE for single numeric value", {
  expect_true(mariposa:::.are_weights(1.5))
})

test_that(".are_weights returns FALSE for character vector", {
  expect_false(mariposa:::.are_weights(c("a", "b", "c")))
})

test_that(".are_weights returns FALSE for empty numeric vector", {
  expect_false(mariposa:::.are_weights(numeric(0)))
})

test_that(".are_weights returns TRUE for integer vector", {
  expect_true(mariposa:::.are_weights(c(1L, 2L, 3L)))
})

test_that(".are_weights returns FALSE for logical vector", {
  expect_false(mariposa:::.are_weights(c(TRUE, FALSE)))
})

test_that(".are_weights returns TRUE for vector containing NA", {
  # NA does not prevent numeric check; it is still a numeric vector

  expect_true(mariposa:::.are_weights(c(1.0, NA, 3.0)))
})

# ============================================================================
# .validate_weights() TESTS
# ============================================================================

test_that(".validate_weights returns TRUE for all-positive weights", {
  expect_true(mariposa:::.validate_weights(c(1.0, 2.0, 3.0)))
})

test_that(".validate_weights returns FALSE and warns for negative weights", {
  expect_warning(
    result <- mariposa:::.validate_weights(c(1.0, -0.5, 2.0)),
    "negative or zero"
  )
  expect_false(result)
})

test_that(".validate_weights returns FALSE and warns for zero weights", {
  expect_warning(
    result <- mariposa:::.validate_weights(c(1.0, 0.0, 2.0)),
    "negative or zero"
  )
  expect_false(result)
})

test_that(".validate_weights suppresses warnings when verbose = FALSE", {
  expect_no_warning(
    result <- mariposa:::.validate_weights(c(1.0, -0.5, 2.0), verbose = FALSE)
  )
  expect_false(result)
})

test_that(".validate_weights returns TRUE when NA present but rest positive", {
  # all(weights > 0, na.rm = TRUE) ignores NAs
  expect_true(mariposa:::.validate_weights(c(1.0, NA, 3.0)))
})

test_that(".validate_weights returns FALSE for NULL weights", {
  expect_false(mariposa:::.validate_weights(NULL))
})

test_that(".validate_weights returns FALSE for empty vector", {
  expect_false(mariposa:::.validate_weights(numeric(0)))
})

test_that(".validate_weights returns FALSE for character input", {
  expect_false(mariposa:::.validate_weights(c("a", "b")))
})

test_that(".validate_weights returns FALSE and warns for all-negative weights", {
  expect_warning(
    result <- mariposa:::.validate_weights(c(-1.0, -2.0, -3.0)),
    "negative or zero"
  )
  expect_false(result)
})

# ============================================================================
# .evaluate_weights() TESTS
# ============================================================================

test_that(".evaluate_weights evaluates a simple expression", {
  wt_vec <- c(1.0, 2.0, 3.0)
  result <- mariposa:::.evaluate_weights(quote(wt_vec))
  expect_equal(result, c(1.0, 2.0, 3.0))
})

test_that(".evaluate_weights returns NULL for NULL argument", {
  result <- mariposa:::.evaluate_weights(NULL)
  expect_null(result)
})

test_that(".evaluate_weights returns NULL for non-existent variable", {
  result <- suppressWarnings(
    mariposa:::.evaluate_weights(quote(nonexistent_weights_var_xyz))
  )
  expect_null(result)
})

# ============================================================================
# .process_variables() TESTS
# ============================================================================

test_that(".process_variables returns named integer vector for valid selection", {
  result <- mariposa:::.process_variables(test_df, x, y)
  expect_type(result, "integer")
  expect_named(result, c("x", "y"))
  expect_equal(unname(result), c(2L, 3L))
})

test_that(".process_variables works with tidyselect helpers", {
  df <- tibble::tibble(trust_a = 1:3, trust_b = 4:6, other = 7:9)
  result <- mariposa:::.process_variables(df, starts_with("trust"))
  expect_named(result, c("trust_a", "trust_b"))
})

test_that(".process_variables errors on empty selection", {
  df <- tibble::tibble(a = 1:3, b = 4:6)
  expect_error(
    mariposa:::.process_variables(df, starts_with("zzz")),
    "No variables selected"
  )
})

test_that(".process_variables errors on non-data-frame input", {
  expect_error(
    mariposa:::.process_variables(c(1, 2, 3), x),
    "must be a data frame"
  )
})

test_that(".process_variables errors on list input", {
  expect_error(
    mariposa:::.process_variables(list(x = 1:3), x),
    "must be a data frame"
  )
})

test_that(".process_variables selects single variable", {
  result <- mariposa:::.process_variables(test_df, x)
  expect_length(result, 1)
  expect_named(result, "x")
})

test_that(".process_variables works with survey_data", {
  data(survey_data)
  result <- mariposa:::.process_variables(
    survey_data, age, income, life_satisfaction
  )
  expect_length(result, 3)
  expect_named(result, c("age", "income", "life_satisfaction"))
})

# ============================================================================
# .process_weights() TESTS
# ============================================================================

test_that(".process_weights returns NULL for NULL weights", {
  null_quo <- rlang::quo(NULL)
  result <- mariposa:::.process_weights(test_df, null_quo)
  expect_null(result$vector)
  expect_null(result$name)
})

test_that(".process_weights returns weight vector and name for valid weights", {
  wt_quo <- rlang::quo(wt)
  result <- mariposa:::.process_weights(test_df, wt_quo)
  expect_equal(result$vector, test_df$wt)
  expect_equal(result$name, "wt")
})

test_that(".process_weights errors for missing column", {
  missing_quo <- rlang::quo(nonexistent_weight)
  expect_error(
    mariposa:::.process_weights(test_df, missing_quo),
    "not found in data"
  )
})

test_that(".process_weights errors for non-numeric column", {
  char_quo <- rlang::quo(char_col)
  expect_error(
    mariposa:::.process_weights(test_df, char_quo),
    "must be numeric"
  )
})

test_that(".process_weights works with survey_data sampling_weight", {
  data(survey_data)
  sw_quo <- rlang::quo(sampling_weight)
  result <- mariposa:::.process_weights(survey_data, sw_quo)
  expect_equal(result$vector, survey_data$sampling_weight)
  expect_equal(result$name, "sampling_weight")
})

# ============================================================================
# .effective_n() TESTS
# ============================================================================

test_that(".effective_n returns 0 for NULL weights", {
  result <- mariposa:::.effective_n(NULL)
  expect_equal(result, 0)
})

test_that(".effective_n returns n for equal weights", {
  # All weights = 1: sum(w)^2 / sum(w^2) = n^2 / n = n

  weights <- rep(1, 100)
  result <- mariposa:::.effective_n(weights)
  expect_equal(result, 100)
})

test_that(".effective_n is less than n for varied weights", {
  weights <- c(0.5, 1.0, 1.5, 2.0, 2.5)
  n <- length(weights)
  result <- mariposa:::.effective_n(weights)
  expect_true(result < n)
  expect_true(result > 0)
})

test_that(".effective_n returns 1 for single weight", {
  # sum(w)^2 / sum(w^2) = w^2 / w^2 = 1
  result <- mariposa:::.effective_n(5.0)
  expect_equal(result, 1)
})

test_that(".effective_n equals n for constant weights", {
  # All weights = k: sum(w)^2 / sum(w^2) = (n*k)^2 / (n*k^2) = n
  weights <- rep(3.7, 50)
  result <- mariposa:::.effective_n(weights)
  expect_equal(result, 50, tolerance = 1e-10)
})

test_that(".effective_n computes correctly for known values", {
  # Weights: 1, 2, 3
  # sum(w) = 6, sum(w^2) = 1+4+9 = 14
  # effective_n = 36/14 = 2.571429
  weights <- c(1, 2, 3)
  result <- mariposa:::.effective_n(weights)
  expect_equal(result, 36 / 14, tolerance = 1e-10)
})

test_that(".effective_n handles NA in weights", {
  # With na.rm = TRUE: sum(c(1,2,3), na.rm=T) = 6, sum(c(1,4,9), na.rm=T) = 14
  weights <- c(1, 2, NA, 3)
  result <- mariposa:::.effective_n(weights)
  expected <- sum(c(1, 2, 3))^2 / sum(c(1, 4, 9))
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that(".effective_n returns length for non-numeric input", {
  # .are_weights returns FALSE for character -> returns length(weights)
  result <- mariposa:::.effective_n(c("a", "b", "c"))
  expect_equal(result, 3)
})

# ============================================================================
# get_value_labels() TESTS
# ============================================================================

test_that("get_value_labels returns factor level labels", {
  fct <- factor(c("Male", "Female", "Male"), levels = c("Male", "Female"))
  result <- mariposa:::get_value_labels(fct, c("Male", "Female"))
  expect_equal(result, c("Male", "Female"))
})

test_that("get_value_labels handles NA in freq_names for factor", {
  fct <- factor(c("A", "B"), levels = c("A", "B"))
  result <- mariposa:::get_value_labels(fct, c("A", NA))
  expect_equal(result[1], "A")
  expect_true(is.na(result[2]))
})

test_that("get_value_labels returns labels for haven-labelled vector", {
  x <- c(1, 2, 3)
  attr(x, "labels") <- c("Low" = 1, "Medium" = 2, "High" = 3)
  result <- mariposa:::get_value_labels(x, c(1, 2, 3))
  expect_equal(result, c("Low", "Medium", "High"))
})

test_that("get_value_labels handles NA in freq_names for haven-labelled", {
  x <- c(1, 2)
  attr(x, "labels") <- c("Low" = 1, "High" = 2)
  result <- mariposa:::get_value_labels(x, c(1, NA))
  expect_equal(result[1], "Low")
  expect_true(is.na(result[2]))
})

test_that("get_value_labels returns empty strings for plain numeric", {
  x <- c(1, 2, 3, 4, 5)
  result <- mariposa:::get_value_labels(x, c(1, 2, 3))
  expect_equal(result, c("", "", ""))
})

test_that("get_value_labels returns empty strings for plain integer", {
  x <- 1:5
  result <- mariposa:::get_value_labels(x, c(1, 2))
  expect_equal(result, c("", ""))
})

# ============================================================================
# .build_label_map() TESTS
# ============================================================================

test_that(".build_label_map returns named vector for factor with meaningful levels", {
  fct <- factor(c("Male", "Female"), levels = c("Male", "Female"))
  result <- mariposa:::.build_label_map(fct)
  expect_type(result, "character")
  expect_equal(result, c("Male" = "Male", "Female" = "Female"))
})

test_that(".build_label_map returns NULL for factor with sequential numeric levels", {
  # Levels "1", "2", "3" map to 1, 2, 3 -> treated as non-meaningful

  fct <- factor(c(1, 2, 3))
  result <- mariposa:::.build_label_map(fct)
  expect_null(result)
})

test_that(".build_label_map returns map for factor with non-sequential numeric levels", {
  # Levels "2", "5", "8" -> not sequential 1:n, so meaningful

  fct <- factor(c(2, 5, 8))
  result <- mariposa:::.build_label_map(fct)
  expect_type(result, "character")
})

test_that(".build_label_map returns named vector for haven-labelled vector", {
  x <- c(1, 2, 3)
  attr(x, "labels") <- c("Low" = 1, "Medium" = 2, "High" = 3)
  result <- mariposa:::.build_label_map(x)
  expect_type(result, "character")
  # names are numeric values as strings, values are label text
  expect_equal(result[["1"]], "Low")
  expect_equal(result[["2"]], "Medium")
  expect_equal(result[["3"]], "High")
})

test_that(".build_label_map returns NULL for plain numeric vector", {
  x <- c(1.5, 2.5, 3.5)
  result <- mariposa:::.build_label_map(x)
  expect_null(result)
})

test_that(".build_label_map returns NULL for plain integer vector", {
  x <- 1:10
  result <- mariposa:::.build_label_map(x)
  expect_null(result)
})

test_that(".build_label_map returns NULL for character vector", {
  x <- c("a", "b", "c")
  result <- mariposa:::.build_label_map(x)
  expect_null(result)
})

test_that(".build_label_map works with survey_data factor columns", {
  data(survey_data)
  result <- mariposa:::.build_label_map(survey_data$gender)
  expect_type(result, "character")
  expect_true("Male" %in% result)
  expect_true("Female" %in% result)
})

# ============================================================================
# .truncate_labels() TESTS
# ============================================================================

test_that(".truncate_labels does not truncate short labels", {
  labels <- c("Short", "OK")
  result <- mariposa:::.truncate_labels(labels, max_width = 20)
  expect_equal(result, labels)
})

test_that(".truncate_labels truncates long labels", {
  labels <- c("This is a very long label that should be cut")
  result <- mariposa:::.truncate_labels(labels, max_width = 20)
  expect_equal(nchar(result), 20)
  expect_true(grepl("\\.\\.\\.$", result))
})

test_that(".truncate_labels handles exact boundary", {
  # Label is exactly max_width characters - should NOT be truncated
  label <- paste0(rep("a", 20), collapse = "")
  result <- mariposa:::.truncate_labels(label, max_width = 20)
  expect_equal(result, label)
  expect_equal(nchar(result), 20)
})

test_that(".truncate_labels handles one character over boundary", {
  # Label is max_width + 1 -> should be truncated
  label <- paste0(rep("a", 21), collapse = "")
  result <- mariposa:::.truncate_labels(label, max_width = 20)
  expect_equal(nchar(result), 20)
  expect_true(grepl("\\.\\.\\.$", result))
})

test_that(".truncate_labels preserves short labels in mixed vector", {
  labels <- c("Short", "This is a very long label exceeding max")
  result <- mariposa:::.truncate_labels(labels, max_width = 15)
  expect_equal(result[1], "Short")
  expect_equal(nchar(result[2]), 15)
})

test_that(".truncate_labels works with small max_width", {
  label <- "Hello World"
  result <- mariposa:::.truncate_labels(label, max_width = 6)
  expect_equal(result, "Hel...")
  expect_equal(nchar(result), 6)
})

test_that(".truncate_labels uses default max_width of 20", {
  long_label <- paste0(rep("x", 30), collapse = "")
  result <- mariposa:::.truncate_labels(long_label)
  expect_equal(nchar(result), 20)
})

# ============================================================================
# .apply_labels() TESTS
# ============================================================================

test_that(".apply_labels returns original levels when label_map is NULL", {
  levels <- c("1", "2", "3")
  result <- mariposa:::.apply_labels(levels, NULL)
  expect_equal(result, levels)
})

test_that(".apply_labels maps values to labels", {
  levels <- c("1", "2", "3")
  label_map <- c("1" = "Low", "2" = "Medium", "3" = "High")
  result <- mariposa:::.apply_labels(levels, label_map, max_width = 20)
  expect_equal(unname(result), c("Low", "Medium", "High"))
})

test_that(".apply_labels keeps unmapped values as-is", {
  levels <- c("1", "2", "4")
  label_map <- c("1" = "Low", "2" = "Medium", "3" = "High")
  result <- mariposa:::.apply_labels(levels, label_map, max_width = 20)
  expect_equal(unname(result), c("Low", "Medium", "4"))
})

test_that(".apply_labels truncates long mapped labels", {
  levels <- c("1")
  label_map <- c("1" = "An extremely long label that exceeds max width")
  result <- mariposa:::.apply_labels(levels, label_map, max_width = 15)
  expect_equal(unname(nchar(result)), 15)
  expect_true(grepl("\\.\\.\\.$", result))
})

# ============================================================================
# .relabel_matrix() TESTS
# ============================================================================

test_that(".relabel_matrix returns original matrix when label_maps is NULL", {
  mat <- matrix(1:4, nrow = 2,
                dimnames = list(row = c("1", "2"), col = c("A", "B")))
  result <- mariposa:::.relabel_matrix(mat, NULL)
  expect_identical(result, mat)
})

test_that(".relabel_matrix returns NULL when mat is NULL", {
  result <- mariposa:::.relabel_matrix(NULL, list())
  expect_null(result)
})

test_that(".relabel_matrix relabels row dimension", {
  mat <- matrix(1:4, nrow = 2,
                dimnames = list(row = c("1", "2"), col = c("A", "B")))
  label_maps <- list(row = c("1" = "Low", "2" = "High"))
  result <- mariposa:::.relabel_matrix(mat, label_maps, max_width = 20)
  expect_equal(unname(rownames(result)), c("Low", "High"))
  expect_equal(colnames(result), c("A", "B"))
})

test_that(".relabel_matrix relabels both dimensions", {
  mat <- matrix(1:4, nrow = 2,
                dimnames = list(
                  row_dim = c("1", "2"),
                  col_dim = c("3", "4")
                ))
  label_maps <- list(
    row_dim = c("1" = "Low", "2" = "High"),
    col_dim = c("3" = "Yes", "4" = "No")
  )
  result <- mariposa:::.relabel_matrix(mat, label_maps, max_width = 20)
  expect_equal(unname(rownames(result)), c("Low", "High"))
  expect_equal(unname(colnames(result)), c("Yes", "No"))
})

test_that(".relabel_matrix preserves matrix values", {
  mat <- matrix(c(10, 20, 30, 40), nrow = 2,
                dimnames = list(row = c("1", "2"), col = c("A", "B")))
  label_maps <- list(row = c("1" = "First", "2" = "Second"))
  result <- mariposa:::.relabel_matrix(mat, label_maps, max_width = 20)
  expect_equal(as.vector(result), c(10, 20, 30, 40))
})

test_that(".relabel_matrix ignores dimensions without label maps", {
  mat <- matrix(1:6, nrow = 2,
                dimnames = list(
                  rows = c("1", "2"),
                  cols = c("A", "B", "C")
                ))
  label_maps <- list(rows = c("1" = "X", "2" = "Y"))
  result <- mariposa:::.relabel_matrix(mat, label_maps, max_width = 20)
  expect_equal(unname(rownames(result)), c("X", "Y"))
  expect_equal(colnames(result), c("A", "B", "C"))
})

# ============================================================================
# .calc_kurtosis() TESTS
# ============================================================================

test_that(".calc_kurtosis returns finite value for normal-like data", {
  set.seed(123)
  x <- rnorm(1000)
  result <- mariposa:::.calc_kurtosis(x)
  # Excess kurtosis of standard normal is 0

  expect_true(is.finite(result))
  expect_equal(result, 0, tolerance = 0.3)
})

test_that(".calc_kurtosis excess = TRUE returns excess kurtosis", {
  set.seed(42)
  x <- rnorm(500)
  excess <- mariposa:::.calc_kurtosis(x, excess = TRUE)
  raw <- mariposa:::.calc_kurtosis(x, excess = FALSE)
  # Raw kurtosis = excess kurtosis + 3
  expect_equal(raw, excess + 3, tolerance = 1e-10)
})

test_that(".calc_kurtosis matches hand-calculated value", {
  # Simple dataset for hand calculation
  x <- c(1, 2, 3, 4, 5)
  n <- 5
  mean_x <- 3
  m2 <- sum((x - mean_x)^2) / n  # 2.0
  m4 <- sum((x - mean_x)^4) / n  # 6.8
  g2 <- m4 / (m2^2) - 3           # 6.8/4 - 3 = -1.3
  G2 <- ((n + 1) * g2 + 6) * (n - 1) / ((n - 2) * (n - 3))

  result <- mariposa:::.calc_kurtosis(x, excess = TRUE)
  expect_equal(result, G2, tolerance = 1e-10)
})

test_that(".calc_kurtosis weighted computation differs from unweighted", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  w <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)

  unweighted <- mariposa:::.calc_kurtosis(x, w = NULL, excess = TRUE)
  weighted <- mariposa:::.calc_kurtosis(x, w = w, excess = TRUE)

  # Different weights should give different results

  expect_false(isTRUE(all.equal(unweighted, weighted)))
})

test_that(".calc_kurtosis weighted matches hand calculation", {
  x <- c(1, 2, 3)
  w <- c(1, 2, 1)
  w_sum <- sum(w)  # 4
  w_mean <- sum(x * w) / w_sum  # (1+4+3)/4 = 2
  m2 <- sum(w * (x - w_mean)^2) / w_sum  # (1*1 + 2*0 + 1*1)/4 = 0.5
  m4 <- sum(w * (x - w_mean)^4) / w_sum  # (1*1 + 2*0 + 1*1)/4 = 0.5
  g2 <- m4 / (m2^2) - 3                   # 0.5/0.25 - 3 = -1
  n <- w_sum
  G2 <- ((n + 1) * g2 + 6) * (n - 1) / ((n - 2) * (n - 3))

  result <- mariposa:::.calc_kurtosis(x, w = w, excess = TRUE)
  expect_equal(result, G2, tolerance = 1e-10)
})

test_that(".calc_kurtosis with equal weights matches unweighted", {
  x <- c(2, 4, 6, 8, 10)
  w <- rep(1, 5)

  unweighted <- mariposa:::.calc_kurtosis(x, w = NULL, excess = TRUE)
  weighted <- mariposa:::.calc_kurtosis(x, w = w, excess = TRUE)

  expect_equal(unweighted, weighted, tolerance = 1e-10)
})

test_that(".calc_kurtosis raw kurtosis is always excess + 3", {
  x <- c(1, 3, 5, 7, 9, 2, 4, 6, 8, 10)

  # Unweighted

  excess_uw <- mariposa:::.calc_kurtosis(x, excess = TRUE)
  raw_uw <- mariposa:::.calc_kurtosis(x, excess = FALSE)
  expect_equal(raw_uw - excess_uw, 3, tolerance = 1e-10)

  # Weighted
  w <- c(1.2, 0.8, 1.5, 1.0, 0.9, 1.3, 1.1, 0.7, 1.4, 1.0)
  excess_w <- mariposa:::.calc_kurtosis(x, w = w, excess = TRUE)
  raw_w <- mariposa:::.calc_kurtosis(x, w = w, excess = FALSE)
  expect_equal(raw_w - excess_w, 3, tolerance = 1e-10)
})

test_that(".calc_kurtosis returns positive excess kurtosis for heavy-tailed data", {
  # Uniform-like data has negative excess kurtosis; heavy-tailed has positive
  set.seed(99)
  heavy_tailed <- c(rep(0, 100), -50, 50)  # Outlier-laden data
  result <- mariposa:::.calc_kurtosis(heavy_tailed, excess = TRUE)
  expect_true(result > 0)
})

test_that(".calc_kurtosis returns negative excess kurtosis for uniform data", {
  # Uniform distribution has theoretical excess kurtosis of -1.2
  x <- seq(1, 100, by = 1)
  result <- mariposa:::.calc_kurtosis(x, excess = TRUE)
  expect_true(result < 0)
})
