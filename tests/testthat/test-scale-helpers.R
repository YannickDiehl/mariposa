# ============================================================================
# SCALE HELPERS - UNIT TESTS
# ============================================================================
# Purpose: Validate pomps() function
# Tests: Correctness with hand-calculated reference values
# Created: 2026-03-01
#
# These functions are pure arithmetic transformations, so validation
# is against hand-calculated values rather than SPSS output.
#
# Note: scale_index() was removed in v0.6.0 and replaced by row_means()
# in R/row_operations.R. See test-row-operations.R for row_means() tests.
# ============================================================================

library(testthat)
library(dplyr)
library(mariposa)

# ============================================================================
# POMPS() TESTS
# ============================================================================

test_that("pomps transforms correctly with explicit scale range", {
  x <- c(1, 2, 3, 4, 5)
  result <- pomps(x, scale_min = 1, scale_max = 5)

  # ((1-1)/(5-1))*100=0, ((2-1)/(5-1))*100=25, ((3-1)/(5-1))*100=50,
  # ((4-1)/(5-1))*100=75, ((5-1)/(5-1))*100=100
  expect_equal(result, c(0, 25, 50, 75, 100))
})

test_that("pomps transforms correctly with 1-7 scale", {
  x <- c(1, 4, 7)
  result <- pomps(x, scale_min = 1, scale_max = 7)

  # ((1-1)/6)*100=0, ((4-1)/6)*100=50, ((7-1)/6)*100=100
  expect_equal(result, c(0, 50, 100))
})

test_that("pomps auto-detects range when scale_min/max are NULL", {
  x <- c(2, 4, 6, 8, 10)
  result <- pomps(x)

  # auto: min=2, max=10
  # ((2-2)/8)*100=0, ((4-2)/8)*100=25, ((6-2)/8)*100=50,
  # ((8-2)/8)*100=75, ((10-2)/8)*100=100
  expect_equal(result, c(0, 25, 50, 75, 100))
})

test_that("pomps handles NA values", {
  x <- c(1, NA, 3, 4, 5)
  result <- pomps(x, scale_min = 1, scale_max = 5)

  expect_equal(result[1], 0)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 50)
  expect_equal(result[5], 100)
})

test_that("pomps preserves correlations", {
  set.seed(42)
  x <- rnorm(100, mean = 3, sd = 1)
  y <- x * 2 + rnorm(100, sd = 0.1)

  x_pomps <- pomps(x)
  y_pomps <- pomps(y)

  # Pearson correlation should be preserved (linear transformation)
  cor_original <- cor(x, y)
  cor_pomps <- cor(x_pomps, y_pomps)

  expect_equal(cor_original, cor_pomps, tolerance = 1e-10)
})

test_that("pomps returns values between 0 and 100 with explicit range", {
  x <- c(1, 2, 3, 4, 5)
  result <- pomps(x, scale_min = 1, scale_max = 5)

  expect_true(all(result >= 0 & result <= 100))
})

test_that("pomps errors on non-numeric input", {
  expect_error(pomps(c("a", "b", "c")))
})

test_that("pomps errors when scale_min >= scale_max", {
  expect_error(pomps(c(1, 2, 3), scale_min = 5, scale_max = 1))
  expect_error(pomps(c(1, 2, 3), scale_min = 3, scale_max = 3))
})

test_that("pomps works in mutate", {
  data <- tibble::tibble(score = c(1, 2, 3, 4, 5))
  result <- data %>%
    mutate(score_pomps = pomps(score, scale_min = 1, scale_max = 5))

  expect_equal(result$score_pomps, c(0, 25, 50, 75, 100))
})

test_that("pomps works with across() for multiple variables", {
  data <- tibble::tibble(
    v1 = c(1, 3, 5),
    v2 = c(2, 3, 4)
  )

  result <- data %>%
    mutate(across(
      c(v1, v2),
      ~ pomps(.x, scale_min = 1, scale_max = 5),
      .names = "{.col}_pomps"
    ))

  expect_equal(result$v1_pomps, c(0, 50, 100))
  expect_equal(result$v2_pomps, c(25, 50, 75))
})

test_that("pomps can handle values outside scale range", {
  # Values outside the scale range should still work (can be > 100 or < 0)
  x <- c(0, 3, 6)
  result <- pomps(x, scale_min = 1, scale_max = 5)

  # ((0-1)/4)*100 = -25
  expect_equal(result[1], -25)
  # ((6-1)/4)*100 = 125
  expect_equal(result[3], 125)
})

test_that("pomps works with survey_data", {
  data(survey_data)
  result <- pomps(survey_data$trust_government, scale_min = 1, scale_max = 5)

  expect_length(result, nrow(survey_data))
  expect_true(is.numeric(result))
})
