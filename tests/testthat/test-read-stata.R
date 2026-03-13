# test-read-stata.R
# Tests for read_stata() and integration with tagged NA helpers

skip_if_not_installed("haven")

# ============================================================================
# Helper: Create a synthetic Stata-like labelled vector with native tagged NAs
# ============================================================================
make_stata_tagged_vector <- function() {
  raw <- c(1, 2, 3, 4, 5,
           haven::tagged_na("a"), haven::tagged_na("a"),
           haven::tagged_na("b"),
           NA)  # system missing (Stata's ".")

  labels <- c(
    "Low" = 1, "Medium" = 2, "High" = 3,
    "Very high" = 4, "Excellent" = 5,
    "Not applicable" = haven::tagged_na("a"),
    "Refused" = haven::tagged_na("b")
  )

  x <- haven::labelled(raw, labels = labels, label = "Rating")
  x
}

make_stata_tagged_df <- function() {
  data.frame(rating = make_stata_tagged_vector())
}

# ============================================================================
# Tests for read_stata() with temp .dta file
# ============================================================================
test_that("read_stata reads .dta file and annotates tagged NAs", {
  # Create a dataset with tagged NAs and write to temp file
  df <- tibble::tibble(
    id = 1:6,
    score = c(1, 2, 3, haven::tagged_na("a"), haven::tagged_na("b"), NA)
  )
  score_labels <- c("Low" = 1, "Med" = 2, "High" = 3,
                     "N/A" = haven::tagged_na("a"),
                     "Refused" = haven::tagged_na("b"))
  df$score <- haven::labelled(df$score, labels = score_labels, label = "Score")

  tmp <- tempfile(fileext = ".dta")
  on.exit(unlink(tmp), add = TRUE)
  haven::write_dta(df, tmp)

  data <- read_stata(tmp)

  # Check na_tag_map exists on score column

  tag_map <- attr(data$score, "na_tag_map")
  expect_false(is.null(tag_map))

  # Tag map should be character (native format codes)
  expect_true(is.character(tag_map))
  expect_true(".a" %in% tag_map)
  expect_true(".b" %in% tag_map)

  # na_tag_format should be "stata"
  expect_equal(attr(data$score, "na_tag_format"), "stata")

  # id column should NOT have na_tag_map (no tagged NAs)
  expect_null(attr(data$id, "na_tag_map"))
})

test_that("read_stata verbose mode prints summary", {
  df <- tibble::tibble(
    x = haven::labelled(
      c(1, 2, haven::tagged_na("a")),
      labels = c("A" = 1, "B" = 2, "Miss" = haven::tagged_na("a")),
      label = "Test"
    )
  )

  tmp <- tempfile(fileext = ".dta")
  on.exit(unlink(tmp), add = TRUE)
  haven::write_dta(df, tmp)

  msgs <- capture.output(read_stata(tmp, verbose = TRUE), type = "message")
  expect_true(any(grepl("tagged missing values", msgs)))
})

test_that("read_stata with no tagged NAs sets no attributes", {
  df <- tibble::tibble(
    x = c(1, 2, 3, NA)
  )

  tmp <- tempfile(fileext = ".dta")
  on.exit(unlink(tmp), add = TRUE)
  haven::write_dta(df, tmp)

  data <- read_stata(tmp)
  expect_null(attr(data$x, "na_tag_map"))
})

# ============================================================================
# Tests for na_frequencies() with Stata data
# ============================================================================
test_that("na_frequencies works with Stata-style tagged NAs", {
  x <- make_stata_tagged_vector()

  # Manually add na_tag_map as read_stata() would
  x <- .build_na_tag_map_from_native(x, "stata")

  result <- na_frequencies(x)

  expect_s3_class(result, "data.frame")
  expect_true("code" %in% names(result))

  # Check Stata-style codes
  a_row <- result[result$tag == "a" & !is.na(result$tag), ]
  b_row <- result[result$tag == "b" & !is.na(result$tag), ]

  expect_equal(a_row$code, ".a")
  expect_equal(b_row$code, ".b")
  expect_equal(a_row$n, 2L)
  expect_equal(b_row$n, 1L)

  # Labels
  expect_equal(a_row$label, "Not applicable")
  expect_equal(b_row$label, "Refused")

  # System NA
  sys_row <- result[is.na(result$tag), ]
  expect_equal(sys_row$n, 1L)
  expect_equal(sys_row$label, "(System Missing)")
})

# ============================================================================
# Tests for strip_tags() with Stata data
# ============================================================================
test_that("strip_tags works with Stata-style tagged NAs", {
  x <- make_stata_tagged_vector()
  stripped <- strip_tags(x)

  expect_true(is.numeric(stripped))
  # Valid values unchanged
  expect_equal(stripped[1:5], c(1, 2, 3, 4, 5))
  # All NAs are regular
  expect_equal(sum(is.na(stripped)), 4L)  # 2 + 1 + 1 system
})

# ============================================================================
# Tests for untag_na() with Stata data (should warn)
# ============================================================================
test_that("untag_na warns for native Stata tagged NAs and falls back to strip_tags", {
  x <- make_stata_tagged_vector()
  x <- .build_na_tag_map_from_native(x, "stata")

  expect_warning(result <- untag_na(x), "STATA")

  # Result should be same as strip_tags
  expected <- strip_tags(x)
  expect_equal(result, expected)
})

# ============================================================================
# Tests for frequency() integration with Stata tagged NAs
# ============================================================================
test_that("frequency shows Stata tagged NA codes", {
  x <- make_stata_tagged_vector()
  x <- .build_na_tag_map_from_native(x, "stata")
  df <- data.frame(rating = x)

  result <- frequency(df, rating)
  tbl <- result$results

  # Should have tagged NA expansion
  expect_true("na_display_value" %in% names(tbl))

  na_display_vals <- tbl$na_display_value[!is.na(tbl$na_display_value)]
  # Stata codes show as ".a", ".b"
  expect_true(".a" %in% na_display_vals)
  expect_true(".b" %in% na_display_vals)
  expect_true("Total" %in% na_display_vals)
  expect_true("NA(total)" %in% na_display_vals)
})

# ============================================================================
# Tests for codebook() integration with Stata tagged NAs
# ============================================================================
test_that("codebook works with Stata tagged NAs", {
  x <- make_stata_tagged_vector()
  x <- .build_na_tag_map_from_native(x, "stata")
  df <- data.frame(rating = x)

  expect_no_error(result <- codebook(df))
  expect_s3_class(result, "codebook")

  cb <- result$codebook
  expect_true("na_values" %in% names(cb))

  # Should show Stata-style codes
  na_vals <- cb$na_values[[1]]
  expect_true(length(na_vals) > 0)
  expect_true(".a" %in% na_vals)
  expect_true(".b" %in% na_vals)
})

# ============================================================================
# Tests for tag.na parameter in read_stata()
# ============================================================================

# Helper: Create Stata file with numeric missing codes (no native tagged NAs)
make_stata_with_numeric_missings <- function() {
  df <- tibble::tibble(
    id = 1:8,
    income = c(1000, 2000, 3000, -9, -8, -9, NA, 4000)
  )
  income_labels <- c(
    "No answer" = -9,
    "Don't know" = -8
  )
  df$income <- haven::labelled(df$income, labels = income_labels,
                                label = "Monthly income")
  tmp <- tempfile(fileext = ".dta")
  haven::write_dta(df, tmp)
  tmp
}

test_that("read_stata with tag.na converts numeric values to tagged NAs", {
  tmp <- make_stata_with_numeric_missings()
  on.exit(unlink(tmp), add = TRUE)

  data <- read_stata(tmp, tag.na = c(-9, -8))

  tag_map <- attr(data$income, "na_tag_map")
  expect_false(is.null(tag_map))

  # Tag map should be numeric (recoverable codes)
  expect_true(is.numeric(tag_map))
  expect_true(-9 %in% tag_map)
  expect_true(-8 %in% tag_map)

  # na_tag_format should be "stata"
  expect_equal(attr(data$income, "na_tag_format"), "stata")

  # -9 and -8 should now be NA
  expect_equal(sum(is.na(data$income)), 4L)  # 2x(-9) + 1x(-8) + 1xNA

  # Valid values should be preserved
  valid <- as.double(data$income[!is.na(data$income)])
  expect_equal(sort(valid), c(1000, 2000, 3000, 4000))
})

test_that("read_stata tag.na verbose prints conversion count", {
  tmp <- make_stata_with_numeric_missings()
  on.exit(unlink(tmp), add = TRUE)

  expect_message(
    read_stata(tmp, tag.na = c(-9, -8), verbose = TRUE),
    "Converted"
  )
})

test_that("read_stata tag.na validates input", {
  tmp <- make_stata_with_numeric_missings()
  on.exit(unlink(tmp), add = TRUE)

  expect_error(
    read_stata(tmp, tag.na = "not_numeric"),
    "numeric vector"
  )
})

test_that("read_stata tag.na skips columns with native tagged NAs", {
  # Create file with native tagged NAs
  df <- tibble::tibble(
    x = c(1, 2, haven::tagged_na("a"), NA)
  )
  df$x <- haven::labelled(df$x,
    labels = c("A" = 1, "B" = 2, "Miss" = haven::tagged_na("a")),
    label = "Test")

  tmp <- tempfile(fileext = ".dta")
  on.exit(unlink(tmp), add = TRUE)
  haven::write_dta(df, tmp)

  # tag.na should not overwrite native tags
  data <- read_stata(tmp, tag.na = c(-9))

  tag_map <- attr(data$x, "na_tag_map")
  # Should still have native character-based tag_map
  expect_true(is.character(tag_map))
})

# ============================================================================
# Tests for untag_na() with tag.na data (should recover codes)
# ============================================================================
test_that("untag_na recovers numeric codes from tag.na Stata data", {
  tmp <- make_stata_with_numeric_missings()
  on.exit(unlink(tmp), add = TRUE)

  data <- read_stata(tmp, tag.na = c(-9, -8))

  # untag_na should recover codes WITHOUT warning
  expect_no_warning(result <- untag_na(data$income))

  # Check recovered values
  expect_equal(sort(result[!is.na(result)]),
               sort(c(1000, 2000, 3000, -9, -9, -8, 4000)))
})

# ============================================================================
# Tests for frequency() with tag.na Stata data
# ============================================================================
test_that("frequency shows tagged NA codes from tag.na Stata data", {
  tmp <- make_stata_with_numeric_missings()
  on.exit(unlink(tmp), add = TRUE)

  data <- read_stata(tmp, tag.na = c(-9, -8))

  result <- frequency(data, income)
  tbl <- result$results

  expect_true("na_display_value" %in% names(tbl))

  na_display_vals <- tbl$na_display_value[!is.na(tbl$na_display_value)]
  # Numeric codes should be shown
  expect_true("-9" %in% na_display_vals)
  expect_true("-8" %in% na_display_vals)
})

# ============================================================================
# Tests for codebook() with tag.na Stata data
# ============================================================================
test_that("codebook works with tag.na Stata data", {
  tmp <- make_stata_with_numeric_missings()
  on.exit(unlink(tmp), add = TRUE)

  data <- read_stata(tmp, tag.na = c(-9, -8))

  expect_no_error(result <- codebook(data))
  expect_s3_class(result, "codebook")

  cb <- result$codebook
  na_vals <- cb$na_values[[which(cb$name == "income")]]
  expect_true(length(na_vals) > 0)
  expect_true(-9 %in% na_vals)
  expect_true(-8 %in% na_vals)
})
