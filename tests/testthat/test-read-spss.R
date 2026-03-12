# test-read-spss.R
# Tests for read_spss(), na_frequencies(), untag_na(), strip_tags()
# and frequency() integration with tagged NAs

# All tests gated on haven availability
skip_if_not_installed("haven")

# ============================================================================
# Helper: Create a small synthetic SPSS-like labelled vector with tagged NAs
# ============================================================================
make_tagged_vector <- function() {
  # Simulate what read_spss() produces:
  # Values 1-5 are valid, -9 and -11 are user-defined missing
  raw <- c(1, 2, 3, 4, 5, 1, 2, 3,
           haven::tagged_na("a"), haven::tagged_na("a"),
           haven::tagged_na("b"), haven::tagged_na("b"), haven::tagged_na("b"),
           NA)  # system NA

  labels <- c(
    "Strongly disagree" = 1,
    "Disagree" = 2,
    "Neutral" = 3,
    "Agree" = 4,
    "Strongly agree" = 5,
    "No answer" = haven::tagged_na("a"),
    "Not applicable" = haven::tagged_na("b")
  )

  x <- haven::labelled(raw, labels = labels, label = "Satisfaction")
  attr(x, "na_tag_map") <- c("a" = -9, "b" = -11)
  x
}

# Helper: create a minimal data frame with tagged NA vector
make_tagged_df <- function() {
  data.frame(satisfaction = make_tagged_vector())
}

# ============================================================================
# Tests for na_frequencies()
# ============================================================================
test_that("na_frequencies returns correct counts per tag", {
  x <- make_tagged_vector()
  result <- na_frequencies(x)

  expect_s3_class(result, "data.frame")
  expect_true("tag" %in% names(result))
  expect_true("n" %in% names(result))
  expect_true("spss_code" %in% names(result))
  expect_true("label" %in% names(result))

  # Should have entries for tag "a", "b", and system NA
  expect_true("a" %in% result$tag)
  expect_true("b" %in% result$tag)
  expect_true(any(is.na(result$tag)))  # system NA row

  # Check counts
  a_row <- result[result$tag == "a" & !is.na(result$tag), ]
  b_row <- result[result$tag == "b" & !is.na(result$tag), ]
  sys_row <- result[is.na(result$tag), ]

  expect_equal(a_row$n, 2L)
  expect_equal(b_row$n, 3L)
  expect_equal(sys_row$n, 1L)
})

test_that("na_frequencies returns correct SPSS codes", {
  x <- make_tagged_vector()
  result <- na_frequencies(x)

  a_row <- result[result$tag == "a" & !is.na(result$tag), ]
  b_row <- result[result$tag == "b" & !is.na(result$tag), ]

  expect_equal(a_row$spss_code, -9)
  expect_equal(b_row$spss_code, -11)
})

test_that("na_frequencies returns correct labels", {
  x <- make_tagged_vector()
  result <- na_frequencies(x)

  a_row <- result[result$tag == "a" & !is.na(result$tag), ]
  b_row <- result[result$tag == "b" & !is.na(result$tag), ]

  expect_equal(a_row$label, "No answer")
  expect_equal(b_row$label, "Not applicable")
})

test_that("na_frequencies handles no-NA vector gracefully", {
  x <- haven::labelled(c(1, 2, 3), labels = c("A" = 1, "B" = 2, "C" = 3))
  result <- na_frequencies(x)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("na_frequencies rejects non-numeric input", {
  expect_error(na_frequencies("text"), "`x` must be a numeric vector")
})

test_that("na_frequencies results are sorted by count descending", {
  x <- make_tagged_vector()
  result <- na_frequencies(x)

  # First row should have highest count
  expect_true(result$n[1] >= result$n[nrow(result)])
})

# ============================================================================
# Tests for untag_na()
# ============================================================================
test_that("untag_na recovers original SPSS codes", {
  x <- make_tagged_vector()
  recovered <- untag_na(x)

  expect_true(is.numeric(recovered))
  # Valid values unchanged
  expect_equal(recovered[1:8], c(1, 2, 3, 4, 5, 1, 2, 3))
  # Tagged NAs become their original codes
  expect_equal(recovered[9], -9)   # tag "a" -> -9
  expect_equal(recovered[10], -9)  # tag "a" -> -9
  expect_equal(recovered[11], -11) # tag "b" -> -11
  expect_equal(recovered[12], -11) # tag "b" -> -11
  expect_equal(recovered[13], -11) # tag "b" -> -11
  # System NA stays NA
  expect_true(is.na(recovered[14]))
})

test_that("untag_na returns double vector without tag_map", {
  x <- haven::labelled(c(1, 2, NA), labels = c("A" = 1, "B" = 2))
  result <- untag_na(x)
  expect_true(is.double(result))
  expect_equal(result[1:2], c(1, 2))
  expect_true(is.na(result[3]))
})

# ============================================================================
# Tests for strip_tags()
# ============================================================================
test_that("strip_tags converts all tagged NAs to regular NA", {
  x <- make_tagged_vector()
  stripped <- strip_tags(x)

  expect_true(is.numeric(stripped))
  # Valid values unchanged
  expect_equal(stripped[1:8], c(1, 2, 3, 4, 5, 1, 2, 3))
  # All NAs are regular (untagged)
  na_positions <- which(is.na(stripped))
  expect_equal(length(na_positions), 6L)  # 2 + 3 + 1 system

  # Tags should be gone - all NA tags should be NA (= untagged)
  for (pos in na_positions) {
    expect_true(is.na(haven::na_tag(stripped[pos])))
  }
})

test_that("strip_tags preserves valid labels and removes NA labels", {
  x <- make_tagged_vector()
  stripped <- strip_tags(x)

  remaining_labels <- attr(stripped, "labels")
  # Only valid value labels should remain
  expect_true(all(!is.na(remaining_labels)))
  expect_equal(length(remaining_labels), 5L)  # 5 valid levels
})

test_that("strip_tags preserves variable label", {
  x <- make_tagged_vector()
  stripped <- strip_tags(x)
  expect_equal(attr(stripped, "label"), "Satisfaction")
})

# ============================================================================
# Tests for read_spss() with synthetic .sav file
# ============================================================================

# Create a temporary .sav file for testing read_spss() itself
test_that("read_spss with tag.na=TRUE creates tagged NAs", {
  skip_if_not_installed("haven")

  # Create a small test dataset
  df <- data.frame(
    age = c(25, 30, 35, -9, 40, -9, -8),
    gender = c(1, 2, 1, 2, 1, -9, 1)
  )

  # Add SPSS-style labels and missing values
  df$age <- haven::labelled_spss(
    df$age,
    labels = c("No answer" = -9, "Don't know" = -8),
    na_values = c(-9, -8),
    label = "Age"
  )
  df$gender <- haven::labelled_spss(
    df$gender,
    labels = c("Male" = 1, "Female" = 2, "No answer" = -9),
    na_values = c(-9),
    label = "Gender"
  )

  # Write and re-read with read_spss
  tmp <- tempfile(fileext = ".sav")
  on.exit(unlink(tmp), add = TRUE)
  haven::write_sav(df, tmp)

  data_tagged <- read_spss(tmp, tag.na = TRUE, verbose = FALSE)

  # Check that tagged NAs exist and are properly NA
  age <- data_tagged$age
  expect_true(is.numeric(age))
  expect_equal(sum(is.na(age)), 3L)  # two -9 + one -8

  # Check that tags are recoverable
  tag_map <- attr(age, "na_tag_map")
  expect_false(is.null(tag_map))
  expect_true(-9 %in% tag_map)
  expect_true(-8 %in% tag_map)

  # Check na_frequencies works on the result
  freqs <- na_frequencies(age)
  expect_true(nrow(freqs) > 0L)
  total_missing <- sum(freqs$n)
  expect_equal(total_missing, 3L)
})

test_that("read_spss with tag.na=FALSE behaves like haven::read_sav", {
  skip_if_not_installed("haven")

  df <- data.frame(
    score = haven::labelled_spss(
      c(1, 2, 3, -9),
      labels = c("Low" = 1, "Med" = 2, "High" = 3, "NA" = -9),
      na_values = c(-9),
      label = "Score"
    )
  )

  tmp <- tempfile(fileext = ".sav")
  on.exit(unlink(tmp), add = TRUE)
  haven::write_sav(df, tmp)

  data_notag <- read_spss(tmp, tag.na = FALSE)

  # -9 should be a regular NA (no tag)
  expect_equal(sum(is.na(data_notag$score)), 1L)
  expect_true(is.na(haven::na_tag(data_notag$score[4])))
})

test_that("read_spss verbose mode prints summary", {
  skip_if_not_installed("haven")

  df <- data.frame(
    x = haven::labelled_spss(
      c(1, 2, -9),
      labels = c("A" = 1, "B" = 2, "Miss" = -9),
      na_values = c(-9),
      label = "Test"
    )
  )

  tmp <- tempfile(fileext = ".sav")
  on.exit(unlink(tmp), add = TRUE)
  haven::write_sav(df, tmp)

  expect_message(
    read_spss(tmp, tag.na = TRUE, verbose = TRUE),
    "Converted"
  )
})

# ============================================================================
# Tests for frequency() integration with tagged NAs
# ============================================================================
test_that("frequency shows expanded tagged NA rows", {
  df <- make_tagged_df()

  result <- frequency(df, satisfaction)

  # The result should have results
  expect_true(nrow(result$results) > 0)

  tbl <- result$results

  # Should have is_na_row column (signals tagged NA expansion happened)
  expect_true("is_na_row" %in% names(tbl))
  expect_true("na_display_value" %in% names(tbl))

  # Should have individual NA rows, a Total Valid row, and a NA(total) row
  na_display_vals <- tbl$na_display_value[!is.na(tbl$na_display_value)]
  expect_true("Total" %in% na_display_vals)      # Total Valid
  expect_true("NA(total)" %in% na_display_vals)   # Total Missing

  # Check that per-tag rows show SPSS codes
  expect_true("-9" %in% na_display_vals)   # tag "a" -> SPSS code -9
  expect_true("-11" %in% na_display_vals)  # tag "b" -> SPSS code -11
})

test_that("frequency tagged NA counts are correct", {
  df <- make_tagged_df()
  result <- frequency(df, satisfaction)
  tbl <- result$results

  # tag "a" rows: freq = 2
  a_row <- tbl[!is.na(tbl$na_display_value) & tbl$na_display_value == "-9", ]
  expect_equal(nrow(a_row), 1L)
  expect_equal(a_row$freq, 2)

  # tag "b" rows: freq = 3
  b_row <- tbl[!is.na(tbl$na_display_value) & tbl$na_display_value == "-11", ]
  expect_equal(nrow(b_row), 1L)
  expect_equal(b_row$freq, 3)

  # Total Missing: 2 + 3 + 1 system = 6
  total_missing <- tbl[!is.na(tbl$na_display_value) & tbl$na_display_value == "NA(total)", ]
  expect_equal(nrow(total_missing), 1L)
  expect_equal(total_missing$freq, 6)

  # Total Valid: 8
  total_valid <- tbl[!is.na(tbl$na_display_value) & tbl$na_display_value == "Total", ]
  expect_equal(nrow(total_valid), 1L)
  expect_equal(total_valid$freq, 8)
})

test_that("frequency with non-tagged data has no is_na_row column", {
  data(survey_data, package = "mariposa")
  result <- frequency(survey_data, gender)
  tbl <- result$results

  # Standard data should NOT have tagged NA columns
  expect_false("is_na_row" %in% names(tbl))
  expect_false("na_display_value" %in% names(tbl))
})

test_that("frequency print does not error with tagged NAs", {
  df <- make_tagged_df()
  result <- frequency(df, satisfaction)

  # Printing should not throw an error
  expect_no_error(capture.output(print(result)))
})

test_that("frequency with show.unused and tagged NAs works together", {
  df <- make_tagged_df()
  result <- frequency(df, satisfaction, show.unused = TRUE)
  tbl <- result$results

  # All 5 valid values should be present
  valid_rows <- tbl[!is.na(tbl$value) | (!is.na(tbl$na_display_value) & tbl$na_display_value == "Total"), ]
  # Values 1-5 should all be there
  non_na_vals <- tbl$value[!is.na(tbl$value)]
  expect_true(all(1:5 %in% non_na_vals))
})

test_that("frequency with show.na=FALSE hides tagged NA rows", {
  df <- make_tagged_df()
  result <- frequency(df, satisfaction, show.na = FALSE)
  tbl <- result$results

  # Should not have any NA rows or tagged NA expansion
  expect_false(any(is.na(tbl$value)))
})

# ============================================================================
# Tests for codebook() integration with tagged NAs
# ============================================================================
test_that("codebook with tagged NA data produces result without errors", {
  df <- make_tagged_df()
  expect_no_error(
    result <- codebook(df)
  )
  expect_s3_class(result, "codebook")
})

test_that("codebook result contains na_values metadata", {
  df <- make_tagged_df()
  result <- codebook(df)
  cb <- result$codebook

  # Should have na_values column
  expect_true("na_values" %in% names(cb))
  expect_true("na_labels" %in% names(cb))
  expect_true("n_system_na" %in% names(cb))

  # satisfaction variable should have tagged NA info
  sat_na_vals <- cb$na_values[[1]]
  expect_true(length(sat_na_vals) > 0)
  expect_true("-9" %in% sat_na_vals)
  expect_true("-11" %in% sat_na_vals)

  # Labels should map codes to label text

  sat_na_lbls <- cb$na_labels[[1]]
  expect_true("-9" %in% names(sat_na_lbls))
  expect_true("-11" %in% names(sat_na_lbls))
  expect_equal(unname(sat_na_lbls["-9"]), "No answer")
  expect_equal(unname(sat_na_lbls["-11"]), "Not applicable")

  # System NA count
  expect_equal(cb$n_system_na[1], 1L)
})

test_that("codebook with show.na=FALSE has empty na-related freq expansion", {
  df <- make_tagged_df()
  result <- codebook(df, show.na = FALSE)

  # Frequencies should NOT have na_display_value column
  freq <- result$frequencies[["satisfaction"]]
  expect_false("na_display_value" %in% names(freq))
})

test_that("codebook with show.unused=TRUE includes all value labels", {
  # Create a vector where value 4 ("Agree") has zero observations
  raw <- c(1, 2, 3, 5, 1, 2, 3, 5)
  labels <- c(
    "Strongly disagree" = 1,
    "Disagree" = 2,
    "Neutral" = 3,
    "Agree" = 4,
    "Strongly agree" = 5
  )
  x <- haven::labelled(raw, labels = labels, label = "Test")
  df <- data.frame(test_var = x)

  result <- codebook(df, show.unused = TRUE)
  freq <- result$frequencies[["test_var"]]

  # Value 4 should appear with freq = 0
  row4 <- freq[as.character(freq$value) == "4", ]
  expect_equal(nrow(row4), 1L)
  expect_equal(row4$freq, 0)
})

test_that("codebook backward compat: survey_data unchanged", {
  data(survey_data, package = "mariposa")
  result <- codebook(survey_data)
  cb <- result$codebook

  # Standard data should have empty na_values
  expect_true(all(vapply(cb$na_values, length, integer(1)) == 0L))
  expect_true(all(cb$n_system_na == 0L))
})

test_that("codebook print does not error with tagged NAs", {
  df <- make_tagged_df()
  result <- codebook(df)
  expect_no_error(capture.output(print(result)))
})

test_that("codebook summary does not error with tagged NAs", {
  df <- make_tagged_df()
  result <- codebook(df)
  expect_no_error(capture.output(print(summary(result))))
})
