# test-read-sas.R
# Tests for read_sas(), read_xpt() and integration with tagged NA helpers

skip_if_not_installed("haven")

# ============================================================================
# Helper: Create a synthetic SAS-like labelled vector with native tagged NAs
# ============================================================================
make_sas_tagged_vector <- function() {
  raw <- c(1, 2, 3,
           haven::tagged_na("A"), haven::tagged_na("A"),
           haven::tagged_na("B"),
           NA)  # system missing (SAS ".")

  labels <- c(
    "Low" = 1, "Medium" = 2, "High" = 3,
    "Not applicable" = haven::tagged_na("A"),
    "Refused" = haven::tagged_na("B")
  )

  x <- haven::labelled(raw, labels = labels, label = "Score")
  x
}

# ============================================================================
# Tests for read_sas() — file-based tests skipped (haven::write_sas deprecated)
# ============================================================================
test_that("read_sas has correct signature", {
  expect_true(is.function(read_sas))
  args <- formals(read_sas)
  expect_true("path" %in% names(args))
  expect_true("catalog_file" %in% names(args))
  expect_true("encoding" %in% names(args))
  expect_true("catalog_encoding" %in% names(args))
  expect_true("tag.na" %in% names(args))
  expect_true("verbose" %in% names(args))
})

test_that("read_sas errors with non-existent file", {
  expect_error(read_sas("nonexistent.sas7bdat"))
})

# ============================================================================
# Tests for read_xpt() with temp .xpt file
# ============================================================================
test_that("read_xpt reads .xpt file and annotates tagged NAs", {
  df <- tibble::tibble(
    ID = 1:4,
    SCORE = c(1, 2, haven::tagged_na("A"), NA)
  )
  df$SCORE <- haven::labelled(df$SCORE,
                               labels = c("Low" = 1, "High" = 2,
                                          "N/A" = haven::tagged_na("A")),
                               label = "Score")

  tmp <- tempfile(fileext = ".xpt")
  on.exit(unlink(tmp), add = TRUE)
  haven::write_xpt(df, tmp)

  data <- read_xpt(tmp)

  tag_map <- attr(data$SCORE, "na_tag_map")
  expect_false(is.null(tag_map))
  expect_true(is.character(tag_map))

  # XPT round-trip converts uppercase tags to lowercase
  expect_true(any(grepl("^\\.", tag_map)))  # codes start with "."
  expect_equal(attr(data$SCORE, "na_tag_format"), "sas")
})

test_that("read_xpt verbose mode prints summary", {
  df <- tibble::tibble(
    X = haven::labelled(
      c(1, haven::tagged_na("A")),
      labels = c("A" = 1, "Miss" = haven::tagged_na("A")),
      label = "Test"
    )
  )

  tmp <- tempfile(fileext = ".xpt")
  on.exit(unlink(tmp), add = TRUE)
  haven::write_xpt(df, tmp)

  msgs <- capture.output(read_xpt(tmp, verbose = TRUE), type = "message")
  expect_true(any(grepl("tagged missing values", msgs)))
})

test_that("read_xpt with no tagged NAs sets no attributes", {
  df <- tibble::tibble(X = c(1, 2, 3, NA))

  tmp <- tempfile(fileext = ".xpt")
  on.exit(unlink(tmp), add = TRUE)
  haven::write_xpt(df, tmp)

  data <- read_xpt(tmp)
  expect_null(attr(data$X, "na_tag_map"))
})

# ============================================================================
# Tests for na_frequencies() with SAS data (in-memory)
# ============================================================================
test_that("na_frequencies works with SAS-style tagged NAs", {
  x <- make_sas_tagged_vector()
  x <- .build_na_tag_map_from_native(x, "sas")

  result <- na_frequencies(x)

  expect_s3_class(result, "data.frame")
  expect_true("code" %in% names(result))

  a_row <- result[result$tag == "A" & !is.na(result$tag), ]
  b_row <- result[result$tag == "B" & !is.na(result$tag), ]

  expect_equal(a_row$code, ".A")
  expect_equal(b_row$code, ".B")
  expect_equal(a_row$n, 2L)
  expect_equal(b_row$n, 1L)

  # Labels
  expect_equal(a_row$label, "Not applicable")
  expect_equal(b_row$label, "Refused")
})

# ============================================================================
# Tests for strip_tags() with SAS data
# ============================================================================
test_that("strip_tags works with SAS-style tagged NAs", {
  x <- make_sas_tagged_vector()
  stripped <- strip_tags(x)

  expect_true(is.numeric(stripped))
  expect_equal(stripped[1:3], c(1, 2, 3))
  expect_equal(sum(is.na(stripped)), 4L)  # 2A + 1B + 1 system
})

# ============================================================================
# Tests for untag_na() with SAS data (should warn)
# ============================================================================
test_that("untag_na warns for native SAS tagged NAs and falls back to strip_tags", {
  x <- make_sas_tagged_vector()
  x <- .build_na_tag_map_from_native(x, "sas")

  expect_warning(result <- untag_na(x), "SAS")
  expected <- strip_tags(x)
  expect_equal(result, expected)
})

# ============================================================================
# Tests for frequency() integration with SAS tagged NAs
# ============================================================================
test_that("frequency shows SAS tagged NA codes", {
  x <- make_sas_tagged_vector()
  x <- .build_na_tag_map_from_native(x, "sas")
  df <- data.frame(score = x)

  result <- frequency(df, score)
  tbl <- result$results

  expect_true("na_display_value" %in% names(tbl))

  na_display_vals <- tbl$na_display_value[!is.na(tbl$na_display_value)]
  expect_true(".A" %in% na_display_vals)
  expect_true(".B" %in% na_display_vals)
  expect_true("Total" %in% na_display_vals)
  expect_true("NA(total)" %in% na_display_vals)
})

# ============================================================================
# Tests for codebook() integration with SAS tagged NAs
# ============================================================================
test_that("codebook works with SAS tagged NAs", {
  x <- make_sas_tagged_vector()
  x <- .build_na_tag_map_from_native(x, "sas")
  df <- data.frame(score = x)

  expect_no_error(result <- codebook(df))
  expect_s3_class(result, "codebook")

  cb <- result$codebook
  na_vals <- cb$na_values[[1]]
  expect_true(length(na_vals) > 0)
  expect_true(".A" %in% na_vals)
  expect_true(".B" %in% na_vals)
})

# ============================================================================
# Tests for tag.na parameter in read_sas() and read_xpt()
# ============================================================================

test_that("read_sas has tag.na parameter", {
  args <- formals(read_sas)
  expect_true("tag.na" %in% names(args))
  expect_null(args$tag.na)
})

test_that("read_sas tag.na validates input", {
  # Validation happens before file read
  expect_error(
    read_sas("any_path.sas7bdat", tag.na = "not_numeric"),
    "numeric vector"
  )
})

# Helper: Create XPT file with numeric missing codes
make_xpt_with_numeric_missings <- function() {
  df <- tibble::tibble(
    ID = 1:6,
    SCORE = c(10, 20, 30, -9, -8, NA)
  )
  score_labels <- c("No answer" = -9, "Don't know" = -8)
  df$SCORE <- haven::labelled(df$SCORE, labels = score_labels,
                               label = "Test score")
  tmp <- tempfile(fileext = ".xpt")
  haven::write_xpt(df, tmp)
  tmp
}

test_that("read_xpt with tag.na converts numeric values to tagged NAs", {
  tmp <- make_xpt_with_numeric_missings()
  on.exit(unlink(tmp), add = TRUE)

  data <- read_xpt(tmp, tag.na = c(-9, -8))

  tag_map <- attr(data$SCORE, "na_tag_map")
  expect_false(is.null(tag_map))
  expect_true(is.numeric(tag_map))
  expect_true(-9 %in% tag_map)
  expect_true(-8 %in% tag_map)

  expect_equal(attr(data$SCORE, "na_tag_format"), "sas")

  # -9 and -8 should now be NA
  expect_equal(sum(is.na(data$SCORE)), 3L)  # -9 + -8 + NA

  # Valid values preserved
  valid <- as.double(data$SCORE[!is.na(data$SCORE)])
  expect_equal(sort(valid), c(10, 20, 30))
})

test_that("read_xpt tag.na verbose prints conversion count", {
  tmp <- make_xpt_with_numeric_missings()
  on.exit(unlink(tmp), add = TRUE)

  expect_message(
    read_xpt(tmp, tag.na = c(-9, -8), verbose = TRUE),
    "Converted"
  )
})

test_that("read_xpt tag.na validates input", {
  tmp <- make_xpt_with_numeric_missings()
  on.exit(unlink(tmp), add = TRUE)

  expect_error(
    read_xpt(tmp, tag.na = "not_numeric"),
    "numeric vector"
  )
})

test_that("untag_na recovers numeric codes from tag.na XPT data", {
  tmp <- make_xpt_with_numeric_missings()
  on.exit(unlink(tmp), add = TRUE)

  data <- read_xpt(tmp, tag.na = c(-9, -8))

  # Should recover without warning
  expect_no_warning(result <- untag_na(data$SCORE))

  expect_equal(sort(result[!is.na(result)]),
               sort(c(10, 20, 30, -9, -8)))
})

test_that("frequency shows tagged NA codes from tag.na XPT data", {
  tmp <- make_xpt_with_numeric_missings()
  on.exit(unlink(tmp), add = TRUE)

  data <- read_xpt(tmp, tag.na = c(-9, -8))

  result <- frequency(data, SCORE)
  tbl <- result$results

  expect_true("na_display_value" %in% names(tbl))
  na_display_vals <- tbl$na_display_value[!is.na(tbl$na_display_value)]
  expect_true("-9" %in% na_display_vals)
  expect_true("-8" %in% na_display_vals)
})

test_that("codebook works with tag.na XPT data", {
  tmp <- make_xpt_with_numeric_missings()
  on.exit(unlink(tmp), add = TRUE)

  data <- read_xpt(tmp, tag.na = c(-9, -8))

  expect_no_error(result <- codebook(data))
  expect_s3_class(result, "codebook")

  cb <- result$codebook
  na_vals <- cb$na_values[[which(cb$name == "SCORE")]]
  expect_true(length(na_vals) > 0)
  expect_true(-9 %in% na_vals)
  expect_true(-8 %in% na_vals)
})

# ============================================================================
# Tests for .tag_user_missing_values() helper directly
# ============================================================================
test_that(".tag_user_missing_values converts specified values", {
  df <- tibble::tibble(
    x = haven::labelled(
      c(1, 2, 3, -9, -8, -9, NA),
      labels = c("A" = 1, "B" = 2, "C" = 3,
                 "No answer" = -9, "Don't know" = -8),
      label = "Test"
    )
  )

  result <- .tag_user_missing_values(df, c(-9, -8), "sas", FALSE)

  tag_map <- attr(result$x, "na_tag_map")
  expect_false(is.null(tag_map))
  expect_true(is.numeric(tag_map))
  expect_true(-9 %in% tag_map)
  expect_true(-8 %in% tag_map)

  # 2x(-9) + 1x(-8) + 1xNA = 4 NAs
  expect_equal(sum(is.na(result$x)), 4L)
})

test_that(".tag_user_missing_values skips columns with existing tags", {
  df <- tibble::tibble(
    x = haven::labelled(
      c(1, haven::tagged_na("A"), NA),
      labels = c("A" = 1, "Miss" = haven::tagged_na("A")),
      label = "Test"
    )
  )
  # Add native tag map
  df$x <- .build_na_tag_map_from_native(df$x, "sas")

  result <- .tag_user_missing_values(df, c(-9), "sas", FALSE)

  # Should not have changed the existing tag_map
  expect_true(is.character(attr(result$x, "na_tag_map")))
})

test_that(".tag_user_missing_values skips non-numeric columns", {
  df <- tibble::tibble(
    name = c("Alice", "Bob", "-9")
  )

  result <- .tag_user_missing_values(df, c(-9), "stata", FALSE)

  # Character column untouched
  expect_null(attr(result$name, "na_tag_map"))
  expect_equal(result$name, c("Alice", "Bob", "-9"))
})
