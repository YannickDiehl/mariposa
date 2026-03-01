# ============================================================================
# SCALE HELPERS - UNIT TESTS
# ============================================================================
# Purpose: Validate scale_index() and pomps() functions
# Tests: Correctness with hand-calculated reference values
# Created: 2026-03-01
#
# These functions are pure arithmetic transformations, so validation
# is against hand-calculated values rather than SPSS output.
# ============================================================================

library(testthat)
library(dplyr)
library(mariposa)

# ============================================================================
# TEST DATA
# ============================================================================

test_data <- tibble::tibble(
  id = 1:6,
  item1 = c(1, 2, 3, 4, 5, NA),
  item2 = c(2, 3, 4, 5, 1, 3),
  item3 = c(3, 4, 5, 1, 2, NA),
  item4 = c(4, 5, 1, 2, 3, 4),
  group = c("A", "A", "A", "B", "B", "B")
)

# ============================================================================
# SCALE_INDEX() TESTS
# ============================================================================

test_that("scale_index computes correct row means", {
  result <- scale_index(test_data, item1, item2, item3)

  # Hand-calculated: mean(1,2,3)=2, mean(2,3,4)=3, mean(3,4,5)=4,
  #                  mean(4,5,1)=10/3, mean(5,1,2)=8/3
  expect_equal(result[1], 2)
  expect_equal(result[2], 3)
  expect_equal(result[3], 4)
  expect_equal(result[4], 10 / 3, tolerance = 1e-10)
  expect_equal(result[5], 8 / 3, tolerance = 1e-10)
})

test_that("scale_index handles NA values with na.rm = TRUE", {
  result <- scale_index(test_data, item1, item2, item3)

  # Row 6: item1=NA, item2=3, item3=NA -> mean(3) = 3
  expect_equal(result[6], 3)
})

test_that("scale_index returns NA when all values are NA", {
  all_na_data <- tibble::tibble(
    a = c(NA_real_, 1),
    b = c(NA_real_, 2),
    c = c(NA_real_, 3)
  )
  result <- scale_index(all_na_data, a, b, c)

  expect_true(is.na(result[1]))
  expect_equal(result[2], 2)
})

test_that("scale_index min_valid works correctly", {
  result <- scale_index(test_data, item1, item2, item3, min_valid = 2)

  # Row 6: only 1 valid value (item2=3), min_valid=2 -> NA
  expect_true(is.na(result[6]))

  # Rows 1-5: all 3 values valid -> should have means
  expect_equal(result[1], 2)
  expect_equal(result[2], 3)
})

test_that("scale_index min_valid = 3 requires all items", {
  result <- scale_index(test_data, item1, item2, item3, min_valid = 3)

  # Row 6: only 1 valid value -> NA
  expect_true(is.na(result[6]))

  # Rows 1-5: all 3 values valid -> should have means
  expect_equal(result[1], 2)
})

test_that("scale_index works with 4 items", {
  result <- scale_index(test_data, item1, item2, item3, item4)

  # Row 1: mean(1,2,3,4) = 2.5
  expect_equal(result[1], 2.5)
  # Row 2: mean(2,3,4,5) = 3.5
  expect_equal(result[2], 3.5)
})

test_that("scale_index works with single item", {
  result <- scale_index(test_data, item1)

  # Should just return the item values
  expect_equal(result[1], 1)
  expect_equal(result[2], 2)
  expect_true(is.na(result[6]))
})

test_that("scale_index works with tidyselect helpers", {
  result <- scale_index(test_data, starts_with("item"))

  # All 4 items: Row 1: mean(1,2,3,4) = 2.5
  expect_equal(result[1], 2.5)
})

test_that("scale_index works with pick() pattern", {
  result <- test_data %>%
    mutate(mean_score = scale_index(pick(item1, item2, item3)))

  expect_equal(result$mean_score[1], 2)
  expect_equal(result$mean_score[2], 3)
})

test_that("scale_index works in mutate with dot pattern", {
  result <- test_data %>%
    mutate(mean_score = scale_index(., item1, item2, item3))

  expect_equal(result$mean_score[1], 2)
  expect_equal(result$mean_score[2], 3)
})

test_that("scale_index returns vector of correct length", {
  result <- scale_index(test_data, item1, item2, item3)
  expect_length(result, nrow(test_data))
})

test_that("scale_index errors on non-data-frame input", {
  expect_error(scale_index(c(1, 2, 3), item1))
})

test_that("scale_index errors on non-numeric variables", {
  expect_error(scale_index(test_data, group))
})

test_that("scale_index warns when min_valid exceeds number of items", {
  expect_warning(
    result <- scale_index(test_data, item1, item2, min_valid = 5)
  )
  # All should be NA
  expect_true(all(is.na(result)))
})

test_that("scale_index with na.rm = FALSE returns NA if any item is NA", {
  result <- scale_index(test_data, item1, item2, item3, na.rm = FALSE)

  # Row 6: item1=NA -> entire row should be NA
  expect_true(is.na(result[6]))

  # Rows 1-5: no NAs -> should have values
  expect_equal(result[1], 2)
})

test_that("scale_index works with survey_data", {
  data(survey_data)
  result <- scale_index(survey_data, trust_government, trust_media, trust_science)

  expect_length(result, nrow(survey_data))
  expect_true(is.numeric(result))
})

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
