# ============================================================================
# Tests for set_na() and unlabel()
# ============================================================================

library(testthat)


# ============================================================================
# set_na() tests
# ============================================================================

test_that("set_na() with tag = TRUE creates tagged NAs on vector", {
  skip_if_not_installed("haven")

  x <- haven::labelled(
    c(1, 2, -9, 3, -8, -9),
    labels = c("Low" = 1, "Med" = 2, "High" = 3,
               "No answer" = -9, "Refused" = -8)
  )

  result <- set_na(x, -9, -8, tag = TRUE)

  # Tagged NAs should be NA
  expect_true(is.na(result[3]))
  expect_true(is.na(result[5]))
  expect_true(is.na(result[6]))

  # Valid values preserved
  expect_equal(as.double(result[1]), 1)
  expect_equal(as.double(result[4]), 3)

  # na_tag_map attribute should exist
  tag_map <- attr(result, "na_tag_map")
  expect_false(is.null(tag_map))
  expect_true(-9 %in% tag_map)
  expect_true(-8 %in% tag_map)
})

test_that("set_na() with tag = FALSE creates regular NAs", {
  skip_if_not_installed("haven")

  x <- haven::labelled(
    c(1, 2, -9, 3, -8),
    labels = c("Low" = 1, "Med" = 2, "High" = 3,
               "No answer" = -9, "Refused" = -8)
  )

  result <- set_na(x, -9, -8, tag = FALSE)

  expect_true(is.na(result[3]))
  expect_true(is.na(result[5]))

  # No tagged NA metadata
  expect_null(attr(result, "na_tag_map"))
})

test_that("set_na() on data frame applies to all numeric columns", {
  skip_if_not_installed("haven")

  df <- tibble::tibble(
    x = haven::labelled(c(1, -9, 2), labels = c("A" = 1, "B" = 2, "Miss" = -9)),
    y = haven::labelled(c(-9, 3, 4), labels = c("C" = 3, "D" = 4, "Miss" = -9)),
    z = c("a", "b", "c")
  )

  result <- set_na(df, -9, tag = TRUE)

  expect_true(is.na(result$x[2]))
  expect_true(is.na(result$y[1]))
  # Character column untouched
  expect_equal(result$z, c("a", "b", "c"))
})

test_that("set_na() with named pairs targets specific variables", {
  skip_if_not_installed("haven")

  df <- tibble::tibble(
    x = c(1, -9, 2),
    y = c(-9, 3, 4)
  )

  result <- set_na(df, x = c(-9), tag = FALSE)

  expect_true(is.na(result$x[2]))
  # y should be untouched
  expect_equal(result$y[1], -9)
})

test_that("set_na() preserves existing tagged NAs when adding new ones", {
  skip_if_not_installed("haven")

  # First set_na call
  x <- c(1, 2, -9, -8, -7, 3)
  result1 <- set_na(x, -9, tag = TRUE)
  expect_true(is.na(result1[3]))
  expect_equal(as.double(result1[4]), -8)

  # Second set_na call should extend the tag map
  result2 <- set_na(result1, -8, tag = TRUE)
  expect_true(is.na(result2[3]))
  expect_true(is.na(result2[4]))

  tag_map <- attr(result2, "na_tag_map")
  expect_equal(length(tag_map), 2)
})

test_that("set_na() errors on missing values specification", {
  expect_error(set_na(data.frame(x = 1:3)), "No values")
})

test_that("set_na() errors on non-numeric missing values", {
  expect_error(set_na(c(1, 2, 3), "a", "b"), "numeric")
})

test_that("set_na() preserves valid value labels", {
  skip_if_not_installed("haven")

  x <- haven::labelled(
    c(1, 2, -9, 3),
    labels = c("Low" = 1, "Med" = 2, "High" = 3, "Missing" = -9)
  )

  result <- set_na(x, -9, tag = TRUE)
  labels <- attr(result, "labels")
  valid_labels <- labels[!is.na(labels)]
  expect_true("Low" %in% names(valid_labels))
  expect_true("Med" %in% names(valid_labels))
  expect_true("High" %in% names(valid_labels))
})


# ============================================================================
# unlabel() tests
# ============================================================================

test_that("unlabel() strips haven_labelled class", {
  skip_if_not_installed("haven")

  x <- haven::labelled(
    c(1, 2, 3),
    labels = c("A" = 1, "B" = 2, "C" = 3),
    label = "Test variable"
  )

  result <- unlabel(x)

  expect_false(inherits(result, "haven_labelled"))
  expect_type(result, "double")
  expect_null(attr(result, "label"))
  expect_null(attr(result, "labels"))
})

test_that("unlabel() converts tagged NAs to regular NA", {
  skip_if_not_installed("haven")

  x <- haven::labelled(
    c(1, 2, haven::tagged_na("a")),
    labels = c("Yes" = 1, "No" = 2, "Miss" = haven::tagged_na("a"))
  )
  attr(x, "na_tag_map") <- c("a" = -9)
  attr(x, "na_tag_format") <- "spss"

  result <- unlabel(x)

  expect_true(is.na(result[3]))
  expect_null(attr(result, "na_tag_map"))
  expect_null(attr(result, "na_tag_format"))
})

test_that("unlabel() on data frame strips all columns", {
  skip_if_not_installed("haven")

  df <- tibble::tibble(
    x = haven::labelled(c(1, 2), labels = c("A" = 1, "B" = 2),
                        label = "X var"),
    y = haven::labelled(c(3, 4), labels = c("C" = 3, "D" = 4),
                        label = "Y var"),
    z = c("a", "b")
  )

  result <- unlabel(df)

  expect_false(inherits(result$x, "haven_labelled"))
  expect_false(inherits(result$y, "haven_labelled"))
  expect_null(attr(result$x, "label"))
  expect_null(attr(result$y, "labels"))
  expect_type(result$z, "character")
})

test_that("unlabel() with specific columns only strips those", {
  skip_if_not_installed("haven")

  df <- tibble::tibble(
    x = haven::labelled(c(1, 2), labels = c("A" = 1, "B" = 2),
                        label = "X var"),
    y = haven::labelled(c(3, 4), labels = c("C" = 3, "D" = 4),
                        label = "Y var")
  )

  result <- unlabel(df, x)

  expect_false(inherits(result$x, "haven_labelled"))
  # y should remain labelled
  expect_s3_class(result$y, "haven_labelled")
  expect_equal(attr(result$y, "label"), "Y var")
})

test_that("unlabel() removes format attributes", {
  skip_if_not_installed("haven")

  x <- haven::labelled(c(1, 2), labels = c("A" = 1, "B" = 2))
  attr(x, "format.spss") <- "F8.2"
  attr(x, "display_width") <- 10

  result <- unlabel(x)
  expect_null(attr(result, "format.spss"))
  expect_null(attr(result, "display_width"))
})
