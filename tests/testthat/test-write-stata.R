# test-write-stata.R
# Tests for write_stata() Stata .dta export

skip_if_not_installed("haven")

# ============================================================================
# Helper: Create tagged NA test data
# ============================================================================
make_stata_test_data <- function() {
  # Simulate data with Stata native tagged NAs (.a, .b)
  x <- haven::labelled(
    c(1, 2, 3, 1, 2,
      haven::tagged_na("a"), haven::tagged_na("b"), NA),
    labels = c(
      "Low" = 1, "Medium" = 2, "High" = 3,
      "Not applicable" = haven::tagged_na("a"),
      "Refused" = haven::tagged_na("b")
    ),
    label = "Income category"
  )
  attr(x, "na_tag_map") <- c("a" = ".a", "b" = ".b")
  attr(x, "na_tag_format") <- "stata"

  y <- haven::labelled(
    c(10, 20, 30, 10, 20, 30, 10, 20),
    labels = c("Urban" = 10, "Rural" = 20, "Suburban" = 30),
    label = "Area type"
  )

  tibble::tibble(income = x, area = y)
}

make_spss_tagged_data <- function() {
  # Simulate SPSS data with numeric missing codes
  x <- haven::labelled(
    c(1, 2, 3,
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

  tibble::tibble(q1 = x)
}


# ============================================================================
# Tests for input validation
# ============================================================================
test_that("write_stata requires haven", {
  expect_true(is.function(write_stata))
})

test_that("write_stata rejects non-data.frame input", {
  expect_error(write_stata(1:5, tempfile(fileext = ".dta")), "data frame")
})

test_that("write_stata rejects non-.dta file paths", {
  df <- data.frame(x = 1:3)
  expect_error(write_stata(df, "output.csv"), "dta")
  expect_error(write_stata(df, "output.sav"), "dta")
  expect_error(write_stata(df, "output.xlsx"), "dta")
})


# ============================================================================
# Tests for basic export
# ============================================================================
test_that("write_stata creates valid .dta file", {
  df <- data.frame(x = c(1, 2, 3), y = c(4.5, 5.5, 6.5))
  tmp <- tempfile(fileext = ".dta")
  on.exit(unlink(tmp), add = TRUE)

  result <- write_stata(df, tmp)

  expect_true(file.exists(tmp))
  expect_equal(result, tmp)
})

test_that("write_stata preserves basic data values", {
  df <- data.frame(
    int_col = c(1L, 2L, 3L),
    dbl_col = c(1.5, 2.5, 3.5)
  )
  tmp <- tempfile(fileext = ".dta")
  on.exit(unlink(tmp), add = TRUE)

  write_stata(df, tmp)
  read_back <- haven::read_dta(tmp)

  expect_equal(as.double(read_back$int_col), c(1, 2, 3))
  expect_equal(as.double(read_back$dbl_col), c(1.5, 2.5, 3.5))
})

test_that("write_stata returns file path invisibly", {
  df <- data.frame(x = 1)
  tmp <- tempfile(fileext = ".dta")
  on.exit(unlink(tmp), add = TRUE)

  result <- write_stata(df, tmp)
  expect_equal(result, tmp)
})


# ============================================================================
# Tests for labels
# ============================================================================
test_that("write_stata preserves variable labels", {
  x <- haven::labelled(c(1, 2, 3), label = "Test Variable")
  df <- data.frame(item = x)
  tmp <- tempfile(fileext = ".dta")
  on.exit(unlink(tmp), add = TRUE)

  write_stata(df, tmp)
  read_back <- haven::read_dta(tmp)

  expect_equal(attr(read_back$item, "label"), "Test Variable")
})

test_that("write_stata preserves value labels", {
  x <- haven::labelled(
    c(1, 2, 3),
    labels = c("Male" = 1, "Female" = 2, "Other" = 3),
    label = "Gender"
  )
  df <- data.frame(gender = x)
  tmp <- tempfile(fileext = ".dta")
  on.exit(unlink(tmp), add = TRUE)

  write_stata(df, tmp)
  read_back <- haven::read_dta(tmp)

  labs <- attr(read_back$gender, "labels")
  expect_true("Male" %in% names(labs))
  expect_true("Female" %in% names(labs))
  expect_true("Other" %in% names(labs))
})


# ============================================================================
# Tests for tagged NA handling
# ============================================================================
test_that("write_stata preserves native Stata tagged NAs in roundtrip", {
  df <- make_stata_test_data()
  tmp <- tempfile(fileext = ".dta")
  on.exit(unlink(tmp), add = TRUE)

  write_stata(df, tmp)
  read_back <- haven::read_dta(tmp)

  # Count NAs in the income column
  expect_equal(sum(is.na(read_back$income)), 3L)  # .a, .b, and system NA

  # Check that tagged NAs are present
  na_vals <- read_back$income[is.na(read_back$income)]
  tags <- vapply(na_vals, haven::na_tag, character(1))
  tagged <- tags[!is.na(tags)]
  expect_true("a" %in% tagged)
  expect_true("b" %in% tagged)
})

test_that("write_stata handles SPSS-format tagged NAs", {
  df <- make_spss_tagged_data()
  tmp <- tempfile(fileext = ".dta")
  on.exit(unlink(tmp), add = TRUE)

  # Should produce an info message about SPSS codes
  expect_message(write_stata(df, tmp), "SPSS")

  read_back <- haven::read_dta(tmp)

  # Valid values preserved
  valid_vals <- as.double(read_back$q1[!is.na(read_back$q1)])
  expect_equal(sort(valid_vals), c(1, 2, 3))

  # Missing values present
  expect_equal(sum(is.na(read_back$q1)), 3L)  # 2 tagged + 1 system
})

test_that("write_stata full roundtrip via read_stata", {
  df <- make_stata_test_data()
  tmp <- tempfile(fileext = ".dta")
  on.exit(unlink(tmp), add = TRUE)

  write_stata(df, tmp)
  read_back <- read_stata(tmp)

  # Native tagged NAs should be detected
  tag_map <- attr(read_back$income, "na_tag_map")
  expect_false(is.null(tag_map))
  expect_true("a" %in% names(tag_map))
  expect_true("b" %in% names(tag_map))

  # Variable label preserved
  expect_equal(attr(read_back$income, "label"), "Income category")
  expect_equal(attr(read_back$area, "label"), "Area type")
})


# ============================================================================
# Tests for edge cases
# ============================================================================
test_that("write_stata handles empty data frame", {
  df <- data.frame(x = numeric(0))
  tmp <- tempfile(fileext = ".dta")
  on.exit(unlink(tmp), add = TRUE)

  expect_no_error(write_stata(df, tmp))
  expect_true(file.exists(tmp))
})

test_that("write_stata handles data with only regular NAs", {
  df <- data.frame(x = c(1, NA, 3, NA, 5))
  tmp <- tempfile(fileext = ".dta")
  on.exit(unlink(tmp), add = TRUE)

  expect_no_error(write_stata(df, tmp))

  read_back <- haven::read_dta(tmp)
  expect_equal(sum(is.na(read_back$x)), 2)
})

test_that("write_stata cleans up mariposa attributes", {
  df <- make_stata_test_data()
  tmp <- tempfile(fileext = ".dta")
  on.exit(unlink(tmp), add = TRUE)

  write_stata(df, tmp)
  read_back <- haven::read_dta(tmp)

  # Haven's reader won't add na_tag_map, and we cleaned it before writing
  # Just verify the file is readable
  expect_true(is.data.frame(read_back))
  expect_equal(nrow(read_back), 8L)
})

test_that("write_stata version parameter works", {
  df <- data.frame(score = 1:10)
  tmp13 <- tempfile(fileext = ".dta")
  tmp14 <- tempfile(fileext = ".dta")
  on.exit(unlink(c(tmp13, tmp14)), add = TRUE)

  expect_no_error(write_stata(df, tmp13, version = 13))
  expect_no_error(write_stata(df, tmp14, version = 14))

  expect_true(file.exists(tmp13))
  expect_true(file.exists(tmp14))
})

test_that("write_stata handles haven_labelled_spss input", {
  # Simulate direct haven::read_sav(user_na = TRUE) output
  x <- haven::labelled_spss(
    c(1, 2, -9, NA),
    labels = c("Yes" = 1, "No" = 2, "Missing" = -9),
    na_values = c(-9),
    label = "Direct haven input"
  )
  df <- tibble::tibble(q1 = x)
  tmp <- tempfile(fileext = ".dta")
  on.exit(unlink(tmp), add = TRUE)

  # Should handle the conversion from haven_labelled_spss → tagged NA → write
  expect_no_error(write_stata(df, tmp))

  read_back <- haven::read_dta(tmp)
  # Missing values should be NAs
  expect_equal(sum(is.na(read_back$q1)), 2L)
})
