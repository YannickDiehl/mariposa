# ============================================================================
# Tests for label conversion functions
# ============================================================================

library(testthat)

# ---- Helper ---------------------------------------------------------------

make_labelled_vec <- function() {
  if (!requireNamespace("haven", quietly = TRUE)) skip("haven not installed")
  haven::labelled(
    c(1, 2, 1, 3, 2),
    labels = c("Low" = 1, "Medium" = 2, "High" = 3),
    label = "Satisfaction"
  )
}

make_labelled_df <- function() {
  if (!requireNamespace("haven", quietly = TRUE)) skip("haven not installed")
  tibble::tibble(
    gender = haven::labelled(
      c(1, 2, 1, 2),
      labels = c("Male" = 1, "Female" = 2),
      label = "Gender"
    ),
    score = haven::labelled(
      c(1, 2, 3, 4),
      labels = c("Low" = 1, "Medium" = 2, "High" = 3, "Very High" = 4),
      label = "Score"
    ),
    plain = c(10, 20, 30, 40)
  )
}


# ============================================================================
# to_label() tests
# ============================================================================

test_that("to_label() converts vector to factor with correct levels", {
  x <- make_labelled_vec()
  result <- to_label(x)

  expect_s3_class(result, "factor")
  expect_equal(levels(result), c("Low", "Medium", "High"))
  expect_equal(as.character(result), c("Low", "Medium", "Low", "High", "Medium"))
})

test_that("to_label() preserves variable label", {
  x <- make_labelled_vec()
  result <- to_label(x)
  expect_equal(attr(result, "label"), "Satisfaction")
})

test_that("to_label() on data frame converts labelled columns", {
  df <- make_labelled_df()
  result <- to_label(df)

  expect_s3_class(result$gender, "factor")
  expect_s3_class(result$score, "factor")
  expect_type(result$plain, "double")
})

test_that("to_label() on data frame with specific columns", {
  df <- make_labelled_df()
  result <- to_label(df, gender)

  expect_s3_class(result$gender, "factor")
  # score should remain labelled
  expect_s3_class(result$score, "haven_labelled")
})

test_that("to_label() with ordered = TRUE creates ordered factor", {
  x <- make_labelled_vec()
  result <- to_label(x, ordered = TRUE)
  expect_true(is.ordered(result))
})

test_that("to_label() with drop.unused removes empty levels", {
  skip_if_not_installed("haven")
  x <- haven::labelled(
    c(1, 1, 1),
    labels = c("A" = 1, "B" = 2, "C" = 3)
  )

  result_keep <- to_label(x, drop.unused = FALSE)
  expect_equal(length(levels(result_keep)), 3)

  result_drop <- to_label(x, drop.unused = TRUE)
  expect_equal(length(levels(result_drop)), 1)
})

test_that("to_label() with add.non.labelled includes unlabelled values", {
  skip_if_not_installed("haven")
  x <- haven::labelled(
    c(1, 2, 99),
    labels = c("Yes" = 1, "No" = 2)
  )

  result_default <- to_label(x)
  expect_true(is.na(result_default[3]))

  result_add <- to_label(x, add.non.labelled = TRUE)
  expect_equal(as.character(result_add[3]), "99")
})

test_that("to_label() handles tagged NAs with drop.na = TRUE", {
  skip_if_not_installed("haven")
  x <- haven::labelled(
    c(1, 2, haven::tagged_na("a")),
    labels = c("Yes" = 1, "No" = 2, "Missing" = haven::tagged_na("a"))
  )

  result <- to_label(x, drop.na = TRUE)
  expect_true(is.na(result[3]))
  expect_false("Missing" %in% levels(result))
})

test_that("to_label() returns factor as-is", {
  f <- factor(c("a", "b", "c"))
  expect_identical(to_label(f), f)
})


# ============================================================================
# to_character() tests
# ============================================================================

test_that("to_character() converts to character using labels", {
  x <- make_labelled_vec()
  result <- to_character(x)

  expect_type(result, "character")
  expect_equal(result, c("Low", "Medium", "Low", "High", "Medium"),
               ignore_attr = TRUE)
})

test_that("to_character() preserves variable label", {
  x <- make_labelled_vec()
  result <- to_character(x)
  expect_equal(attr(result, "label"), "Satisfaction")
})

test_that("to_character() on data frame converts labelled columns", {
  df <- make_labelled_df()
  result <- to_character(df)

  expect_type(result$gender, "character")
  expect_type(result$score, "character")
  expect_type(result$plain, "double")
})

test_that("to_character() on data frame with specific columns", {
  df <- make_labelled_df()
  result <- to_character(df, gender)

  expect_type(result$gender, "character")
  expect_s3_class(result$score, "haven_labelled")
})


# ============================================================================
# to_numeric() tests
# ============================================================================

test_that("to_numeric() extracts numeric from haven_labelled", {
  x <- make_labelled_vec()
  result <- to_numeric(x)

  expect_type(result, "double")
  expect_equal(result, c(1, 2, 1, 3, 2), ignore_attr = TRUE)
})

test_that("to_numeric() preserves variable label", {
  x <- make_labelled_vec()
  result <- to_numeric(x)
  expect_equal(attr(result, "label"), "Satisfaction")
})

test_that("to_numeric() converts numeric factor using level values", {
  f <- factor(c("10", "20", "30", "20"))
  result <- to_numeric(f)
  expect_equal(result, c(10, 20, 30, 20))
})

test_that("to_numeric() converts text factor to sequential integers", {
  f <- factor(c("low", "high", "medium", "low"))
  result <- to_numeric(f)
  expect_equal(result, c(2, 1, 3, 2))  # alphabetical: high=1, low=2, medium=3
})

test_that("to_numeric() with use.labels = FALSE uses indices", {
  f <- factor(c("10", "20", "30"), levels = c("10", "20", "30"))
  result <- to_numeric(f, use.labels = FALSE)
  expect_equal(result, c(1, 2, 3))
})

test_that("to_numeric() with start.at adjusts range", {
  f <- factor(c("a", "b", "c"))
  result <- to_numeric(f, start.at = 0)
  expect_equal(result, c(0, 1, 2))
})

test_that("to_numeric() with keep.labels stores former levels", {
  f <- factor(c("Yes", "No", "Yes"))
  result <- to_numeric(f, keep.labels = TRUE)
  labels <- attr(result, "labels")
  expect_true("Yes" %in% names(labels))
  expect_true("No" %in% names(labels))
})

test_that("to_numeric() on data frame converts factor columns", {
  df <- tibble::tibble(
    f = factor(c("1", "2", "3")),
    n = c(10, 20, 30)
  )
  result <- to_numeric(df)
  expect_type(result$f, "double")
  expect_equal(result$f, c(1, 2, 3))
  # numeric column unchanged
  expect_equal(result$n, c(10, 20, 30))
})

test_that("to_numeric() already numeric returns as double", {
  result <- to_numeric(c(1L, 2L, 3L))
  expect_type(result, "double")
})


# ============================================================================
# to_labelled() tests
# ============================================================================

test_that("to_labelled() converts factor to haven_labelled", {
  skip_if_not_installed("haven")
  f <- factor(c("Male", "Female", "Male"))
  result <- to_labelled(f)

  expect_s3_class(result, "haven_labelled")
  labels <- attr(result, "labels")
  expect_true("Male" %in% names(labels))
  expect_true("Female" %in% names(labels))
  expect_equal(as.double(result), c(2, 1, 2))  # Female=1, Male=2 (alphabetical)
})

test_that("to_labelled() with custom labels", {
  skip_if_not_installed("haven")
  x <- c(1, 2, 3)
  result <- to_labelled(x,
    labels = c("Low" = 1, "Medium" = 2, "High" = 3),
    label = "Rating"
  )

  expect_s3_class(result, "haven_labelled")
  expect_equal(attr(result, "label"), "Rating")
  expect_equal(attr(result, "labels")[["Low"]], 1)
})

test_that("to_labelled() converts character to labelled", {
  skip_if_not_installed("haven")
  x <- c("yes", "no", "yes", "maybe")
  result <- to_labelled(x)

  expect_s3_class(result, "haven_labelled")
  labels <- attr(result, "labels")
  expect_equal(length(labels), 3)
})

test_that("to_labelled() on already labelled just updates labels", {
  skip_if_not_installed("haven")
  x <- haven::labelled(c(1, 2), labels = c("A" = 1, "B" = 2))
  result <- to_labelled(x, label = "Updated")

  expect_s3_class(result, "haven_labelled")
  expect_equal(attr(result, "label"), "Updated")
})

test_that("to_labelled() on data frame converts factor columns", {
  skip_if_not_installed("haven")
  df <- tibble::tibble(
    f = factor(c("X", "Y", "X")),
    n = c(1, 2, 3)
  )
  result <- to_labelled(df)

  expect_s3_class(result$f, "haven_labelled")
  expect_type(result$n, "double")
})

test_that("to_labelled() errors on unsupported type", {
  skip_if_not_installed("haven")
  expect_error(to_labelled(as.Date("2024-01-01")), "Cannot convert")
})
