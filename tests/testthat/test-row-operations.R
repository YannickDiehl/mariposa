# ============================================================================
# ROW OPERATIONS - UNIT TESTS
# ============================================================================
# Purpose: Validate row_means(), row_sums(), row_count()
# Tests: Correctness with hand-calculated reference values
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
# row_means() TESTS
# ============================================================================

test_that("row_means computes correct row means", {
  result <- row_means(test_data, item1, item2, item3)

  expect_equal(result[1], 2)
  expect_equal(result[2], 3)
  expect_equal(result[3], 4)
  expect_equal(result[4], 10 / 3, tolerance = 1e-10)
  expect_equal(result[5], 8 / 3, tolerance = 1e-10)
})

test_that("row_means handles NA values with na.rm = TRUE", {
  result <- row_means(test_data, item1, item2, item3)
  # Row 6: item1=NA, item2=3, item3=NA -> mean(3) = 3
  expect_equal(result[6], 3)
})

test_that("row_means returns NA when all values are NA", {
  all_na_data <- tibble::tibble(
    a = c(NA_real_, 1),
    b = c(NA_real_, 2),
    c = c(NA_real_, 3)
  )
  result <- row_means(all_na_data, a, b, c)
  expect_true(is.na(result[1]))
  expect_equal(result[2], 2)
})

test_that("row_means min_valid works correctly", {
  result <- row_means(test_data, item1, item2, item3, min_valid = 2)
  # Row 6: only 1 valid value (item2=3), min_valid=2 -> NA
  expect_true(is.na(result[6]))
  # Rows 1-5: all 3 values valid
  expect_equal(result[1], 2)
})

test_that("row_means min_valid = 3 requires all items", {
  result <- row_means(test_data, item1, item2, item3, min_valid = 3)
  expect_true(is.na(result[6]))
  expect_equal(result[1], 2)
})

test_that("row_means works with 4 items", {
  result <- row_means(test_data, item1, item2, item3, item4)
  expect_equal(result[1], 2.5)
  expect_equal(result[2], 3.5)
})

test_that("row_means works with tidyselect helpers", {
  result <- row_means(test_data, starts_with("item"))
  expect_equal(result[1], 2.5)
})

test_that("row_means works with pick() pattern", {
  result <- test_data %>%
    mutate(mean_score = row_means(pick(item1, item2, item3)))
  expect_equal(result$mean_score[1], 2)
  expect_equal(result$mean_score[2], 3)
})

test_that("row_means works in mutate with dot pattern", {
  result <- test_data %>%
    mutate(mean_score = row_means(., item1, item2, item3))
  expect_equal(result$mean_score[1], 2)
  expect_equal(result$mean_score[2], 3)
})

test_that("row_means returns vector of correct length", {
  result <- row_means(test_data, item1, item2, item3)
  expect_length(result, nrow(test_data))
})

test_that("row_means errors on non-data-frame input", {
  expect_error(row_means(c(1, 2, 3), item1))
})

test_that("row_means errors on non-numeric variables", {
  expect_error(row_means(test_data, group))
})

test_that("row_means warns when min_valid exceeds number of items", {
  expect_warning(
    result <- row_means(test_data, item1, item2, min_valid = 5)
  )
  expect_true(all(is.na(result)))
})

test_that("row_means with na.rm = FALSE returns NA if any item is NA", {
  result <- row_means(test_data, item1, item2, item3, na.rm = FALSE)
  expect_true(is.na(result[6]))
  expect_equal(result[1], 2)
})

# ============================================================================
# row_sums() TESTS
# ============================================================================

test_that("row_sums computes correct row sums", {
  result <- row_sums(test_data, item1, item2, item3)
  # Row 1: 1+2+3=6, Row 2: 2+3+4=9
  expect_equal(result[1], 6)
  expect_equal(result[2], 9)
  expect_equal(result[3], 12)
})

test_that("row_sums handles NA with na.rm = TRUE", {
  result <- row_sums(test_data, item1, item2, item3)
  # Row 6: NA + 3 + NA = 3
  expect_equal(result[6], 3)
})

test_that("row_sums returns NA when all values are NA", {
  all_na_data <- tibble::tibble(
    a = c(NA_real_, 1),
    b = c(NA_real_, 2)
  )
  result <- row_sums(all_na_data, a, b)
  expect_true(is.na(result[1]))
  expect_equal(result[2], 3)
})

test_that("row_sums min_valid works correctly", {
  result <- row_sums(test_data, item1, item2, item3, min_valid = 2)
  # Row 6: only 1 valid value -> NA
  expect_true(is.na(result[6]))
  expect_equal(result[1], 6)
})

test_that("row_sums with na.rm = FALSE returns NA if any item is NA", {
  result <- row_sums(test_data, item1, item2, item3, na.rm = FALSE)
  expect_true(is.na(result[6]))
  expect_equal(result[1], 6)
})

test_that("row_sums works with tidyselect", {
  result <- row_sums(test_data, starts_with("item"))
  # Row 1: 1+2+3+4=10
  expect_equal(result[1], 10)
})

test_that("row_sums works with pick() pattern", {
  result <- test_data %>%
    mutate(total = row_sums(pick(item1, item2, item3)))
  expect_equal(result$total[1], 6)
})

test_that("row_sums errors on non-data-frame input", {
  expect_error(row_sums(c(1, 2, 3), item1))
})

# ============================================================================
# row_count() TESTS
# ============================================================================

test_that("row_count counts specific values correctly", {
  count_data <- tibble::tibble(
    a = c(1, 2, 3, 1, 1),
    b = c(1, 1, 2, 2, 1),
    c = c(2, 1, 1, 1, 1)
  )
  result <- row_count(count_data, a, b, c, count = 1)
  # Row 1: a=1, b=1, c=2 -> 2 ones
  expect_equal(result[1], 2L)
  # Row 5: a=1, b=1, c=1 -> 3 ones
  expect_equal(result[5], 3L)
  # Row 3: a=3, b=2, c=1 -> 1 one
  expect_equal(result[3], 1L)
})

test_that("row_count handles NA with na.rm = TRUE", {
  na_data <- tibble::tibble(
    a = c(1, NA, 1),
    b = c(1, 1, NA),
    c = c(NA, 1, 1)
  )
  result <- row_count(na_data, a, b, c, count = 1)
  # Row 1: a=1, b=1, c=NA -> count=2 (NA ignored)
  expect_equal(result[1], 2L)
  # Row 2: a=NA, b=1, c=1 -> count=2
  expect_equal(result[2], 2L)
})

test_that("row_count with na.rm = FALSE returns NA if any value is NA", {
  na_data <- tibble::tibble(
    a = c(1, NA),
    b = c(1, 1)
  )
  result <- row_count(na_data, a, b, count = 1, na.rm = FALSE)
  expect_equal(result[1], 2L)
  expect_true(is.na(result[2]))
})

test_that("row_count returns 0 when value not found", {
  data <- tibble::tibble(a = c(1, 2), b = c(3, 4))
  result <- row_count(data, a, b, count = 99)
  expect_equal(result, c(0L, 0L))
})

test_that("row_count returns integer vector", {
  result <- row_count(test_data, item1, item2, item3, count = 3)
  expect_type(result, "integer")
})

test_that("row_count errors when count is missing", {
  expect_error(row_count(test_data, item1, item2))
})

test_that("row_count errors on non-data-frame input", {
  expect_error(row_count(c(1, 2, 3), count = 1))
})

test_that("row_count works with tidyselect", {
  result <- row_count(test_data, starts_with("item"), count = 1)
  expect_length(result, nrow(test_data))
})
