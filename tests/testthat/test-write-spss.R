# test-write-spss.R
# Tests for write_spss() SPSS .sav export

skip_if_not_installed("haven")

# ============================================================================
# Helper: Create tagged NA test data (same as test-read-spss.R)
# ============================================================================
make_spss_test_data <- function() {
  x <- haven::labelled(
    c(1, 2, 3, 1, 2,
      haven::tagged_na("a"), haven::tagged_na("b"), NA),
    labels = c(
      "Yes" = 1, "No" = 2, "Maybe" = 3,
      "No answer" = haven::tagged_na("a"),
      "Refused" = haven::tagged_na("b")
    ),
    label = "Survey item"
  )
  attr(x, "na_tag_map") <- c("a" = -9, "b" = -8)
  attr(x, "na_tag_format") <- "spss"

  y <- haven::labelled(
    c(10, 20, 30, 10, 20, 30, 10, 20),
    labels = c("Low" = 10, "Medium" = 20, "High" = 30),
    label = "Category"
  )

  tibble::tibble(q1 = x, cat = y)
}


# ============================================================================
# Tests for input validation
# ============================================================================
test_that("write_spss requires haven", {
  expect_true(is.function(write_spss))
})

test_that("write_spss rejects non-data.frame input", {
  expect_error(write_spss(1:5, tempfile(fileext = ".sav")), "data frame")
})

test_that("write_spss rejects non-.sav file paths", {
  df <- data.frame(x = 1:3)
  expect_error(write_spss(df, "output.csv"), "sav")
  expect_error(write_spss(df, "output.xlsx"), "sav")
  expect_error(write_spss(df, "output.dta"), "sav")
})

test_that("write_spss accepts .zsav extension", {
  df <- data.frame(x = 1:3)
  tmp <- tempfile(fileext = ".zsav")
  on.exit(unlink(tmp), add = TRUE)

  expect_no_error(write_spss(df, tmp, compress = "zsav"))
  expect_true(file.exists(tmp))
})


# ============================================================================
# Tests for basic export
# ============================================================================
test_that("write_spss creates valid .sav file", {
  df <- data.frame(x = c(1, 2, 3), y = c("a", "b", "c"),
                   stringsAsFactors = FALSE)
  tmp <- tempfile(fileext = ".sav")
  on.exit(unlink(tmp), add = TRUE)

  result <- write_spss(df, tmp)

  expect_true(file.exists(tmp))
  expect_equal(result, tmp)
})

test_that("write_spss preserves basic data values", {
  df <- data.frame(
    int_col = c(1L, 2L, 3L),
    dbl_col = c(1.5, 2.5, 3.5)
  )
  tmp <- tempfile(fileext = ".sav")
  on.exit(unlink(tmp), add = TRUE)

  write_spss(df, tmp)
  read_back <- haven::read_sav(tmp)

  expect_equal(as.double(read_back$int_col), c(1, 2, 3))
  expect_equal(as.double(read_back$dbl_col), c(1.5, 2.5, 3.5))
})

test_that("write_spss returns file path invisibly", {
  df <- data.frame(x = 1)
  tmp <- tempfile(fileext = ".sav")
  on.exit(unlink(tmp), add = TRUE)

  result <- write_spss(df, tmp)
  expect_equal(result, tmp)
})


# ============================================================================
# Tests for variable labels
# ============================================================================
test_that("write_spss preserves variable labels", {
  x <- haven::labelled(c(1, 2, 3), label = "Test Variable")
  df <- data.frame(item = x)
  tmp <- tempfile(fileext = ".sav")
  on.exit(unlink(tmp), add = TRUE)

  write_spss(df, tmp)
  read_back <- haven::read_sav(tmp)

  expect_equal(attr(read_back$item, "label"), "Test Variable")
})


# ============================================================================
# Tests for value labels
# ============================================================================
test_that("write_spss preserves value labels", {
  x <- haven::labelled(
    c(1, 2, 3, 1),
    labels = c("Male" = 1, "Female" = 2, "Other" = 3),
    label = "Gender"
  )
  df <- data.frame(gender = x)
  tmp <- tempfile(fileext = ".sav")
  on.exit(unlink(tmp), add = TRUE)

  write_spss(df, tmp)
  read_back <- haven::read_sav(tmp)

  labs <- attr(read_back$gender, "labels")
  expect_true("Male" %in% names(labs))
  expect_true("Female" %in% names(labs))
  expect_true("Other" %in% names(labs))
  expect_equal(unname(labs["Male"]), 1)
  expect_equal(unname(labs["Female"]), 2)
})


# ============================================================================
# Tests for tagged NA roundtripping
# ============================================================================
test_that("write_spss reconstructs SPSS user-defined missing values", {
  df <- make_spss_test_data()
  tmp <- tempfile(fileext = ".sav")
  on.exit(unlink(tmp), add = TRUE)

  write_spss(df, tmp)

  # Read back with user_na = TRUE to verify missing value definitions
  read_back <- haven::read_sav(tmp, user_na = TRUE)

  # q1 should have na_values attribute with -9 and -8
  na_vals <- attr(read_back$q1, "na_values")
  expect_true(!is.null(na_vals))
  expect_true(-9 %in% na_vals)
  expect_true(-8 %in% na_vals)
})

test_that("write_spss preserves missing value labels in roundtrip", {
  df <- make_spss_test_data()
  tmp <- tempfile(fileext = ".sav")
  on.exit(unlink(tmp), add = TRUE)

  write_spss(df, tmp)
  read_back <- haven::read_sav(tmp, user_na = TRUE)

  labs <- attr(read_back$q1, "labels")
  expect_true("No answer" %in% names(labs))
  expect_true("Refused" %in% names(labs))
})

test_that("write_spss valid values survive roundtrip", {
  df <- make_spss_test_data()
  tmp <- tempfile(fileext = ".sav")
  on.exit(unlink(tmp), add = TRUE)

  write_spss(df, tmp)
  read_back <- haven::read_sav(tmp)

  # Valid values should be preserved (NAs become regular NA without user_na)
  valid_vals <- as.double(read_back$q1[!is.na(read_back$q1)])
  expect_equal(sort(valid_vals), c(1, 1, 2, 2, 3))
})

test_that("write_spss full roundtrip via read_spss", {
  df <- make_spss_test_data()
  tmp <- tempfile(fileext = ".sav")
  on.exit(unlink(tmp), add = TRUE)

  write_spss(df, tmp)
  read_back <- read_spss(tmp)

  # Tagged NAs should be reconstructed
  expect_false(is.null(attr(read_back$q1, "na_tag_map")))

  # Original codes should be recoverable
  tag_map <- attr(read_back$q1, "na_tag_map")
  expect_true(-9 %in% tag_map)
  expect_true(-8 %in% tag_map)

  # Valid value labels preserved
  labs <- attr(read_back$q1, "labels")
  valid_labs <- labs[!is.na(labs)]
  expect_true("Yes" %in% names(valid_labs))
  expect_true("No" %in% names(valid_labs))
  expect_true("Maybe" %in% names(valid_labs))

  # Variable label preserved
  expect_equal(attr(read_back$q1, "label"), "Survey item")

  # Column without tagged NAs preserved
  expect_equal(attr(read_back$cat, "label"), "Category")
})


# ============================================================================
# Tests for columns without tagged NAs
# ============================================================================
test_that("write_spss passes through columns without tag_map", {
  x <- haven::labelled(c(1, 2, 3), labels = c("A" = 1, "B" = 2))
  df <- data.frame(val = x)
  tmp <- tempfile(fileext = ".sav")
  on.exit(unlink(tmp), add = TRUE)

  expect_no_error(write_spss(df, tmp))

  read_back <- haven::read_sav(tmp)
  expect_equal(as.double(read_back$val), c(1, 2, 3))
})


# ============================================================================
# Tests for edge cases
# ============================================================================
test_that("write_spss handles empty data frame", {
  df <- data.frame(x = numeric(0), y = character(0))
  tmp <- tempfile(fileext = ".sav")
  on.exit(unlink(tmp), add = TRUE)

  expect_no_error(write_spss(df, tmp))
  expect_true(file.exists(tmp))
})

test_that("write_spss handles all-NA column", {
  x <- haven::labelled(
    c(haven::tagged_na("a"), haven::tagged_na("a"), NA),
    labels = c("Missing" = haven::tagged_na("a")),
    label = "All missing"
  )
  attr(x, "na_tag_map") <- c("a" = -9)
  attr(x, "na_tag_format") <- "spss"
  df <- data.frame(q1 = x)
  tmp <- tempfile(fileext = ".sav")
  on.exit(unlink(tmp), add = TRUE)

  expect_no_error(write_spss(df, tmp))

  read_back <- haven::read_sav(tmp, user_na = TRUE)
  na_vals <- attr(read_back$q1, "na_values")
  expect_true(-9 %in% na_vals)
})

test_that("write_spss handles data with only regular NAs", {
  df <- data.frame(x = c(1, NA, 3, NA, 5))
  tmp <- tempfile(fileext = ".sav")
  on.exit(unlink(tmp), add = TRUE)

  expect_no_error(write_spss(df, tmp))

  read_back <- haven::read_sav(tmp)
  expect_equal(sum(is.na(read_back$x)), 2)
})

test_that("write_spss handles haven_labelled_spss input (not yet tagged)", {
  # Simulate direct haven::read_sav(user_na = TRUE) output
  x <- haven::labelled_spss(
    c(1, 2, -9, -8, NA),
    labels = c("Yes" = 1, "No" = 2, "No answer" = -9, "Refused" = -8),
    na_values = c(-9, -8),
    label = "Direct haven input"
  )
  df <- tibble::tibble(q1 = x)
  tmp <- tempfile(fileext = ".sav")
  on.exit(unlink(tmp), add = TRUE)

  expect_no_error(write_spss(df, tmp))

  # Verify roundtrip
  read_back <- haven::read_sav(tmp, user_na = TRUE)
  na_vals <- attr(read_back$q1, "na_values")
  expect_true(-9 %in% na_vals)
  expect_true(-8 %in% na_vals)
})

test_that("write_spss handles >3 missing codes via na_range", {
  # SPSS allows at most 3 discrete na_values; more requires na_range
  x <- haven::labelled(
    c(1, 2, 3,
      haven::tagged_na("a"), haven::tagged_na("b"),
      haven::tagged_na("c"), haven::tagged_na("d"), NA),
    labels = c(
      "Low" = 1, "Med" = 2, "High" = 3,
      "Code A" = haven::tagged_na("a"),
      "Code B" = haven::tagged_na("b"),
      "Code C" = haven::tagged_na("c"),
      "Code D" = haven::tagged_na("d")
    ),
    label = "Multi-missing"
  )
  attr(x, "na_tag_map") <- c("a" = -42, "b" = -11, "c" = -9, "d" = -8)
  attr(x, "na_tag_format") <- "spss"
  df <- tibble::tibble(q1 = x)
  tmp <- tempfile(fileext = ".sav")
  on.exit(unlink(tmp), add = TRUE)

  # Should not error (uses na_range instead of 4 discrete na_values)
  expect_no_error(write_spss(df, tmp))

  # Verify the file is readable and missing values are preserved
  read_back <- haven::read_sav(tmp, user_na = TRUE)
  na_rng <- attr(read_back$q1, "na_range")
  expect_false(is.null(na_rng))
  expect_equal(na_rng, c(-42, -8))

  # Valid values still present
  valid <- as.double(read_back$q1[!is.na(read_back$q1)])
  expect_equal(sort(valid), c(1, 2, 3))
})

test_that("write_spss compression options work", {
  df <- data.frame(x = 1:100)
  tmp_byte <- tempfile(fileext = ".sav")
  tmp_none <- tempfile(fileext = ".sav")
  on.exit(unlink(c(tmp_byte, tmp_none)), add = TRUE)

  expect_no_error(write_spss(df, tmp_byte, compress = "byte"))
  expect_no_error(write_spss(df, tmp_none, compress = "none"))

  # Both should produce valid files
  expect_true(file.exists(tmp_byte))
  expect_true(file.exists(tmp_none))
})
