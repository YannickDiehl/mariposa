# ============================================================================
# FIND_VAR - UNIT TESTS
# ============================================================================
# Purpose: Validate find_var() variable search function
# ============================================================================

library(testthat)
library(mariposa)

# ============================================================================
# TEST DATA
# ============================================================================

test_data <- tibble::tibble(
  v101 = 1:5,
  v102 = 6:10,
  trust_gov = 11:15,
  trust_media = 16:20,
  age = 21:25
)

# Add variable labels
attr(test_data$v101, "label") <- "Zufriedenheit mit der Regierung"
attr(test_data$v102, "label") <- "Vertrauen in die Medien"
attr(test_data$trust_gov, "label") <- "Trust in government"
attr(test_data$trust_media, "label") <- "Trust in media"
attr(test_data$age, "label") <- "Age of respondent"

# ============================================================================
# BASIC SEARCH TESTS
# ============================================================================

test_that("find_var finds variables by name", {
  result <- find_var(test_data, "trust", search = "name")
  expect_equal(nrow(result), 2)
  expect_true("trust_gov" %in% result$name)
  expect_true("trust_media" %in% result$name)
})

test_that("find_var finds variables by label", {
  result <- find_var(test_data, "Vertrauen", search = "label")
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "v102")
})

test_that("find_var finds variables by name and label", {
  result <- find_var(test_data, "trust")
  # Should find trust_gov, trust_media (by name), and v102 (by label "Vertrauen" doesn't match,
  # but "Trust" in labels does match)
  expect_true(nrow(result) >= 2)
  expect_true("trust_gov" %in% result$name)
  expect_true("trust_media" %in% result$name)
})

test_that("find_var search is case-insensitive", {
  result1 <- find_var(test_data, "TRUST", search = "name")
  result2 <- find_var(test_data, "trust", search = "name")
  expect_equal(nrow(result1), nrow(result2))
})

# ============================================================================
# RESULT STRUCTURE
# ============================================================================

test_that("find_var returns correct columns", {
  result <- find_var(test_data, "age")
  expect_named(result, c("col", "name", "label"))
})

test_that("find_var col column contains correct positions", {
  result <- find_var(test_data, "age")
  expect_equal(result$col, 5L)
})

test_that("find_var label column contains variable labels", {
  result <- find_var(test_data, "age")
  expect_equal(result$label, "Age of respondent")
})

# ============================================================================
# REGEX SUPPORT
# ============================================================================

test_that("find_var supports regex patterns", {
  result <- find_var(test_data, "^v[0-9]+", search = "name")
  expect_equal(nrow(result), 2)
  expect_true("v101" %in% result$name)
  expect_true("v102" %in% result$name)
})

test_that("find_var supports regex in label search", {
  result <- find_var(test_data, "^Trust", search = "label")
  expect_equal(nrow(result), 2)
})

# ============================================================================
# EDGE CASES
# ============================================================================

test_that("find_var returns empty data frame when no match", {
  expect_message(
    result <- find_var(test_data, "nonexistent_variable_xyz")
  )
  expect_equal(nrow(result), 0)
  expect_named(result, c("col", "name", "label"))
})

test_that("find_var handles data without labels", {
  no_label_data <- tibble::tibble(x = 1:3, y = 4:6)
  result <- find_var(no_label_data, "x")
  expect_equal(nrow(result), 1)
  expect_equal(result$label, "")
})

test_that("find_var errors on non-data-frame input", {
  expect_error(find_var(c(1, 2, 3), "x"))
})

test_that("find_var errors on non-character pattern", {
  expect_error(find_var(test_data, 123))
})

test_that("find_var works with survey_data", {
  data(survey_data)
  result <- find_var(survey_data, "trust")
  expect_true(nrow(result) > 0)
})
