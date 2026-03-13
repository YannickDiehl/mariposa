# ============================================================================
# Tests for label management functions
# ============================================================================

library(testthat)

# ---- Helper: create labelled test data ------------------------------------

make_labelled_df <- function() {
  if (!requireNamespace("haven", quietly = TRUE)) skip("haven not installed")

  gender <- haven::labelled(
    c(1, 2, 1, 2, 1),
    labels = c("Male" = 1, "Female" = 2),
    label = "Gender of respondent"
  )
  satisfaction <- haven::labelled(
    c(1, 2, 3, 4, 5),
    labels = c("Very low" = 1, "Low" = 2, "Medium" = 3,
               "High" = 4, "Very high" = 5),
    label = "Life satisfaction"
  )
  age <- c(25, 30, 35, 40, 45)
  attr(age, "label") <- "Age in years"

  name <- c("Alice", "Bob", "Charlie", "Diana", "Eve")

  tibble::tibble(gender = gender, satisfaction = satisfaction,
                 age = age, name = name)
}


# ============================================================================
# var_label() tests
# ============================================================================

test_that("var_label() GET: retrieves all variable labels", {
  df <- make_labelled_df()
  result <- var_label(df)

  expect_type(result, "character")
  expect_length(result, 4)
  expect_equal(result[["gender"]], "Gender of respondent")
  expect_equal(result[["satisfaction"]], "Life satisfaction")
  expect_equal(result[["age"]], "Age in years")
  expect_true(is.na(result[["name"]]))
})

test_that("var_label() GET: retrieves specific variable labels", {
  df <- make_labelled_df()
  result <- var_label(df, gender, age)

  expect_length(result, 2)
  expect_equal(result[["gender"]], "Gender of respondent")
  expect_equal(result[["age"]], "Age in years")
})

test_that("var_label() GET: works on single vector", {
  df <- make_labelled_df()
  expect_equal(var_label(df$gender), "Gender of respondent")
  expect_null(var_label(df$name))
})

test_that("var_label() SET: assigns labels", {
  df <- make_labelled_df()
  result <- var_label(df, name = "Participant name")

  expect_equal(attr(result$name, "label"), "Participant name")
  # Original labels should be unchanged

  expect_equal(attr(result$gender, "label"), "Gender of respondent")
})

test_that("var_label() SET: removes labels with NULL", {
  df <- make_labelled_df()
  result <- var_label(df, gender = NULL)
  expect_null(attr(result$gender, "label", exact = TRUE))
})

test_that("var_label() SET: errors on non-existent variable", {
  df <- make_labelled_df()
  expect_error(var_label(df, nonexistent = "label"), "not found")
})

test_that("var_label() SET: errors on non-string label", {
  df <- make_labelled_df()
  expect_error(var_label(df, gender = 123), "character string")
})


# ============================================================================
# val_labels() tests
# ============================================================================

test_that("val_labels() GET: retrieves labels from vector", {
  df <- make_labelled_df()
  result <- val_labels(df$gender)

  expect_type(result, "double")
  expect_equal(result[["Male"]], 1)
  expect_equal(result[["Female"]], 2)
})

test_that("val_labels() GET: retrieves labels for specific variables", {
  df <- make_labelled_df()
  result <- val_labels(df, gender, satisfaction)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_equal(result$gender[["Male"]], 1)
  expect_length(result$satisfaction, 5)
})

test_that("val_labels() GET: single variable returns vector not list", {
  df <- make_labelled_df()
  result <- val_labels(df, gender)

  # Should return vector directly, not list
  expect_type(result, "double")
  expect_equal(result[["Male"]], 1)
})

test_that("val_labels() GET: returns NULL for unlabelled vector", {
  expect_null(val_labels(c(1, 2, 3)))
})

test_that("val_labels() GET: returns empty list when no labels exist", {
  df <- tibble::tibble(x = 1:5, y = letters[1:5])
  result <- val_labels(df)
  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("val_labels() SET: assigns value labels", {
  df <- make_labelled_df()
  result <- val_labels(df,
    age = c("Young" = 25, "Middle" = 35, "Old" = 45)
  )

  labels <- attr(result$age, "labels")
  expect_equal(labels[["Young"]], 25)
  expect_equal(labels[["Old"]], 45)
  # Should now be haven_labelled
  expect_s3_class(result$age, "haven_labelled")
})

test_that("val_labels() SET: .add merges with existing labels", {
  df <- make_labelled_df()
  result <- val_labels(df,
    gender = c("Non-binary" = 3),
    .add = TRUE
  )

  labels <- attr(result$gender, "labels")
  expect_equal(length(labels), 3)
  expect_equal(labels[["Male"]], 1)
  expect_equal(labels[["Non-binary"]], 3)
})

test_that("val_labels() SET: replaces by default", {
  df <- make_labelled_df()
  result <- val_labels(df,
    gender = c("M" = 1, "F" = 2)
  )

  labels <- attr(result$gender, "labels")
  expect_equal(length(labels), 2)
  expect_equal(labels[["M"]], 1)
  expect_false("Male" %in% names(labels))
})

test_that("val_labels() SET: NULL removes all labels", {
  df <- make_labelled_df()
  result <- val_labels(df, gender = NULL)
  expect_null(attr(result$gender, "labels"))
})

test_that("val_labels() SET: errors on unnamed vector", {
  df <- make_labelled_df()
  expect_error(val_labels(df, gender = c(1, 2)), "named numeric vector")
})


# ============================================================================
# copy_labels() tests
# ============================================================================

test_that("copy_labels() restores variable labels after filter", {
  df <- make_labelled_df()
  subset <- df[df$age > 30, ]

  # dplyr operations can strip labels - simulate
  attr(subset$gender, "label") <- NULL

  result <- copy_labels(subset, df)
  expect_equal(attr(result$gender, "label"), "Gender of respondent")
})

test_that("copy_labels() restores value labels", {
  df <- make_labelled_df()
  subset <- df[1:3, ]
  attr(subset$gender, "labels") <- NULL

  result <- copy_labels(subset, df)
  labels <- attr(result$gender, "labels")
  expect_equal(labels[["Male"]], 1)
  expect_equal(labels[["Female"]], 2)
})

test_that("copy_labels() handles columns only in target", {
  df <- make_labelled_df()
  subset <- df
  subset$new_col <- 1:5

  result <- copy_labels(subset, df)
  # new_col should be unchanged
  expect_equal(result$new_col, 1:5)
})

test_that("copy_labels() errors on non-data-frame input", {
  expect_error(copy_labels(1:5, data.frame(x = 1)), "data frame")
  expect_error(copy_labels(data.frame(x = 1), 1:5), "data frame")
})


# ============================================================================
# drop_labels() tests
# ============================================================================

test_that("drop_labels() removes labels for absent values", {
  skip_if_not_installed("haven")

  x <- haven::labelled(
    c(1, 1, 2, 2),
    labels = c("A" = 1, "B" = 2, "C" = 3, "D" = 4)
  )

  result <- drop_labels(x)
  labels <- attr(result, "labels")
  expect_equal(length(labels), 2)
  expect_true("A" %in% names(labels))
  expect_true("B" %in% names(labels))
  expect_false("C" %in% names(labels))
})

test_that("drop_labels() works on data frame", {
  skip_if_not_installed("haven")

  df <- tibble::tibble(
    x = haven::labelled(c(1, 2), labels = c("A" = 1, "B" = 2, "C" = 3)),
    y = haven::labelled(c(10, 20), labels = c("X" = 10, "Y" = 20, "Z" = 30))
  )

  result <- drop_labels(df)
  expect_equal(length(attr(result$x, "labels")), 2)
  expect_equal(length(attr(result$y, "labels")), 2)
})

test_that("drop_labels() preserves tagged NA labels by default", {
  skip_if_not_installed("haven")

  x <- haven::labelled(
    c(1, 2, haven::tagged_na("a")),
    labels = c("Yes" = 1, "No" = 2, "Missing" = haven::tagged_na("a"),
               "Unused" = 3)
  )
  attr(x, "na_tag_map") <- c("a" = -9)

  result <- drop_labels(x)
  labels <- attr(result, "labels")
  # "Unused" should be gone, but "Missing" (NA label) should remain
  expect_false("Unused" %in% names(labels))
  expect_true("Missing" %in% names(labels))
})
