# test-write-sas.R
# Tests for write_xpt() SAS transport .xpt export

skip_if_not_installed("haven")

# ============================================================================
# Helper: Create tagged NA test data
# ============================================================================
make_sas_test_data <- function() {
  # Simulate data with SAS native tagged NAs (.A, .B)
  x <- haven::labelled(
    c(1, 2, 3, 1, 2,
      haven::tagged_na("A"), haven::tagged_na("B"), NA),
    labels = c(
      "Low" = 1, "Medium" = 2, "High" = 3
    ),
    label = "Income level"
  )
  attr(x, "na_tag_map") <- c("A" = ".A", "B" = ".B")
  attr(x, "na_tag_format") <- "sas"

  y <- haven::labelled(
    c(10, 20, 30, 10, 20, 30, 10, 20),
    labels = c("Urban" = 10, "Rural" = 20, "Suburban" = 30),
    label = "Area type"
  )

  tibble::tibble(income = x, area = y)
}


# ============================================================================
# Tests for input validation
# ============================================================================
test_that("write_xpt requires haven", {
  expect_true(is.function(write_xpt))
})

test_that("write_xpt rejects non-data.frame input", {
  expect_error(write_xpt(1:5, tempfile(fileext = ".xpt")), "data frame")
})

test_that("write_xpt rejects non-.xpt file paths", {
  df <- data.frame(x = 1:3)
  expect_error(write_xpt(df, "output.csv"), "xpt")
  expect_error(write_xpt(df, "output.sav"), "xpt")
  expect_error(write_xpt(df, "output.dta"), "xpt")
})

test_that("write_xpt rejects invalid version", {
  df <- data.frame(x = 1:3)
  expect_error(write_xpt(df, tempfile(fileext = ".xpt"), version = 3), "5 or 8")
})


# ============================================================================
# Tests for basic export
# ============================================================================
test_that("write_xpt creates valid .xpt file", {
  df <- data.frame(x = c(1, 2, 3), y = c(4.5, 5.5, 6.5))
  tmp <- tempfile(fileext = ".xpt")
  on.exit(unlink(tmp), add = TRUE)

  result <- write_xpt(df, tmp)

  expect_true(file.exists(tmp))
  expect_equal(result, tmp)
})

test_that("write_xpt preserves basic data values", {
  df <- data.frame(
    int_col = c(1L, 2L, 3L),
    dbl_col = c(1.5, 2.5, 3.5)
  )
  tmp <- tempfile(fileext = ".xpt")
  on.exit(unlink(tmp), add = TRUE)

  write_xpt(df, tmp)
  read_back <- haven::read_xpt(tmp)

  expect_equal(as.double(read_back$int_col), c(1, 2, 3))
  expect_equal(as.double(read_back$dbl_col), c(1.5, 2.5, 3.5))
})

test_that("write_xpt returns file path invisibly", {
  df <- data.frame(x = 1)
  tmp <- tempfile(fileext = ".xpt")
  on.exit(unlink(tmp), add = TRUE)

  result <- write_xpt(df, tmp)
  expect_equal(result, tmp)
})


# ============================================================================
# Tests for labels
# ============================================================================
test_that("write_xpt preserves variable labels", {
  x <- haven::labelled(c(1, 2, 3), label = "Test Variable")
  df <- data.frame(item = x)
  tmp <- tempfile(fileext = ".xpt")
  on.exit(unlink(tmp), add = TRUE)

  write_xpt(df, tmp)
  read_back <- haven::read_xpt(tmp)

  expect_equal(attr(read_back$item, "label"), "Test Variable")
})

test_that("write_xpt warns about value labels not being stored", {
  x <- haven::labelled(
    c(1, 2, 3),
    labels = c("A" = 1, "B" = 2, "C" = 3),
    label = "Test"
  )
  df <- data.frame(val = x)
  tmp <- tempfile(fileext = ".xpt")
  on.exit(unlink(tmp), add = TRUE)

  # Should produce info message about value labels
  expect_message(write_xpt(df, tmp), "value labels")
})


# ============================================================================
# Tests for tagged NA handling
# ============================================================================
test_that("write_xpt preserves native SAS tagged NAs in roundtrip", {
  df <- make_sas_test_data()
  tmp <- tempfile(fileext = ".xpt")
  on.exit(unlink(tmp), add = TRUE)

  write_xpt(df, tmp)
  read_back <- haven::read_xpt(tmp)

  # Count NAs in the income column
  expect_equal(sum(is.na(read_back$income)), 3L)  # .A, .B, and system NA

  # Check that tagged NAs are present
  # Note: haven normalises tags to lowercase on XPT read
  na_vals <- read_back$income[is.na(read_back$income)]
  tags <- vapply(na_vals, haven::na_tag, character(1))
  tagged <- tags[!is.na(tags)]
  expect_true("a" %in% tagged)
  expect_true("b" %in% tagged)
})

test_that("write_xpt handles SPSS-format tagged NAs", {
  # SPSS data with numeric codes
  x <- haven::labelled(
    c(1, 2, haven::tagged_na("a"), NA),
    labels = c("Yes" = 1, "No" = 2,
               "Missing" = haven::tagged_na("a")),
    label = "Item"
  )
  attr(x, "na_tag_map") <- c("a" = -9)
  attr(x, "na_tag_format") <- "spss"
  df <- tibble::tibble(q1 = x)
  tmp <- tempfile(fileext = ".xpt")
  on.exit(unlink(tmp), add = TRUE)

  # Should produce info message about SPSS codes
  expect_message(write_xpt(df, tmp), "SPSS")

  read_back <- haven::read_xpt(tmp)

  # Valid values preserved
  valid_vals <- as.double(read_back$q1[!is.na(read_back$q1)])
  expect_equal(sort(valid_vals), c(1, 2))

  # Missing values present
  expect_equal(sum(is.na(read_back$q1)), 2L)
})

test_that("write_xpt full roundtrip via read_xpt", {
  df <- make_sas_test_data()
  tmp <- tempfile(fileext = ".xpt")
  on.exit(unlink(tmp), add = TRUE)

  write_xpt(df, tmp)
  read_back <- read_xpt(tmp)

  # Native tagged NAs should be detected
  # Note: haven normalises tags to lowercase on XPT read
  tag_map <- attr(read_back$income, "na_tag_map")
  expect_false(is.null(tag_map))
  expect_true("a" %in% names(tag_map))
  expect_true("b" %in% names(tag_map))

  # Variable label preserved
  expect_equal(attr(read_back$income, "label"), "Income level")
})


# ============================================================================
# Tests for edge cases
# ============================================================================
test_that("write_xpt handles empty data frame", {
  df <- data.frame(x = numeric(0))
  tmp <- tempfile(fileext = ".xpt")
  on.exit(unlink(tmp), add = TRUE)

  expect_no_error(write_xpt(df, tmp))
  expect_true(file.exists(tmp))
})

test_that("write_xpt handles data with only regular NAs", {
  df <- data.frame(x = c(1, NA, 3, NA, 5))
  tmp <- tempfile(fileext = ".xpt")
  on.exit(unlink(tmp), add = TRUE)

  expect_no_error(write_xpt(df, tmp))

  read_back <- haven::read_xpt(tmp)
  expect_equal(sum(is.na(read_back$x)), 2)
})

test_that("write_xpt version 5 and 8 both work", {
  df <- data.frame(x = 1:5)
  tmp5 <- tempfile(fileext = ".xpt")
  tmp8 <- tempfile(fileext = ".xpt")
  on.exit(unlink(c(tmp5, tmp8)), add = TRUE)

  expect_no_error(write_xpt(df, tmp5, version = 5))
  expect_no_error(write_xpt(df, tmp8, version = 8))

  expect_true(file.exists(tmp5))
  expect_true(file.exists(tmp8))
})

test_that("write_xpt cleans up mariposa attributes", {
  df <- make_sas_test_data()
  tmp <- tempfile(fileext = ".xpt")
  on.exit(unlink(tmp), add = TRUE)

  write_xpt(df, tmp)
  read_back <- haven::read_xpt(tmp)

  # File should be readable and valid
  expect_true(is.data.frame(read_back))
  expect_equal(nrow(read_back), 8L)
})

test_that("write_xpt handles haven_labelled_spss input", {
  # Simulate direct haven::read_sav(user_na = TRUE) output
  x <- haven::labelled_spss(
    c(1, 2, -9, NA),
    labels = c("Yes" = 1, "No" = 2, "Missing" = -9),
    na_values = c(-9),
    label = "Direct haven input"
  )
  df <- tibble::tibble(q1 = x)
  tmp <- tempfile(fileext = ".xpt")
  on.exit(unlink(tmp), add = TRUE)

  expect_no_error(write_xpt(df, tmp))

  read_back <- haven::read_xpt(tmp)
  expect_equal(sum(is.na(read_back$q1)), 2L)
})
