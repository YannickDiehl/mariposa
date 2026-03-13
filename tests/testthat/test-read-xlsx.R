# test-read-xlsx.R
# Tests for read_xlsx() with label reconstruction

skip_if_not_installed("openxlsx2")

# ============================================================================
# Tests for basic reading and validation
# ============================================================================
test_that("read_xlsx reads a plain Excel file as tibble", {
  df <- data.frame(x = 1:5, y = letters[1:5], stringsAsFactors = FALSE)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  openxlsx2::write_xlsx(df, tmp)
  result <- read_xlsx(tmp)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 5L)
  expect_equal(ncol(result), 2L)
})

test_that("read_xlsx preserves column types", {
  df <- data.frame(
    int_col = c(1, 2, 3),
    dbl_col = c(1.5, 2.5, 3.5),
    chr_col = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  openxlsx2::write_xlsx(df, tmp)
  result <- read_xlsx(tmp)

  expect_type(result$dbl_col, "double")
  expect_type(result$chr_col, "character")
})

test_that("read_xlsx handles NA values", {
  df <- data.frame(x = c(1, NA, 3), y = c("a", NA, "c"),
                   stringsAsFactors = FALSE)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  openxlsx2::write_xlsx(df, tmp)
  result <- read_xlsx(tmp)

  expect_true(is.na(result$x[2]))
  expect_true(is.na(result$y[2]))
})

test_that("read_xlsx reads specific sheet by name", {
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet("First")
  wb$add_data(sheet = "First", x = data.frame(a = 1:3))
  wb$add_worksheet("Second")
  wb$add_data(sheet = "Second", x = data.frame(b = 4:6))
  openxlsx2::wb_save(wb, tmp, overwrite = TRUE)

  result <- read_xlsx(tmp, sheet = "Second")
  expect_true("b" %in% names(result))
})

test_that("read_xlsx reads specific sheet by index", {
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet("First")
  wb$add_data(sheet = "First", x = data.frame(a = 1:3))
  wb$add_worksheet("Second")
  wb$add_data(sheet = "Second", x = data.frame(b = 4:6))
  openxlsx2::wb_save(wb, tmp, overwrite = TRUE)

  result <- read_xlsx(tmp, sheet = 2)
  expect_true("b" %in% names(result))
})

test_that("read_xlsx errors on non-existent file", {
  expect_error(read_xlsx("nonexistent_file.xlsx"), "not found")
})

test_that("read_xlsx errors on non-.xlsx file", {
  expect_error(read_xlsx("data.csv"), "xlsx")
})

test_that("read_xlsx errors on invalid sheet index", {
  df <- data.frame(x = 1:3)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  openxlsx2::write_xlsx(df, tmp)
  expect_error(read_xlsx(tmp, sheet = 99), "out of range")
})

test_that("read_xlsx errors on non-existent sheet name", {
  df <- data.frame(x = 1:3)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  openxlsx2::write_xlsx(df, tmp)
  expect_error(read_xlsx(tmp, sheet = "Missing"), "not found")
})

# ============================================================================
# Tests for format detection
# ============================================================================
test_that("read_xlsx auto-detects Data sheet", {
  df <- data.frame(x = 1:3)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp, labels = FALSE)
  result <- read_xlsx(tmp)

  expect_equal(result$x, c(1, 2, 3))
})

test_that("read_xlsx warns for codebook exports", {
  data("survey_data", package = "mariposa")
  cb <- codebook(survey_data, gender)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(cb, tmp)
  expect_warning(read_xlsx(tmp), "codebook")
})

test_that("read_xlsx reads first non-metadata sheet when no Data sheet", {
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet("Results")
  wb$add_data(sheet = "Results", x = data.frame(val = 1:5))
  openxlsx2::wb_save(wb, tmp, overwrite = TRUE)

  result <- read_xlsx(tmp)
  expect_true("val" %in% names(result))
})

# ============================================================================
# Tests for label reconstruction
# ============================================================================
test_that("read_xlsx reconstructs variable labels", {
  df <- data.frame(x = 1:3, y = 4:6)
  attr(df$x, "label") <- "First Variable"
  attr(df$y, "label") <- "Second Variable"
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  result <- read_xlsx(tmp)

  expect_equal(attr(result$x, "label"), "First Variable")
  expect_equal(attr(result$y, "label"), "Second Variable")
})

test_that("read_xlsx reconstructs haven_labelled with value labels", {
  skip_if_not_installed("haven")

  x <- haven::labelled(c(1, 2, 3, 1, 2),
                        labels = c("Yes" = 1, "No" = 2, "Maybe" = 3),
                        label = "Response")
  df <- data.frame(q1 = x)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  result <- read_xlsx(tmp)

  expect_s3_class(result$q1, "haven_labelled")
  expect_equal(attr(result$q1, "label"), "Response")
  expect_equal(attr(result$q1, "labels"), c("Yes" = 1, "No" = 2, "Maybe" = 3))
  expect_equal(as.double(result$q1), c(1, 2, 3, 1, 2))
})

test_that("read_xlsx reconstructs tagged NAs from missing codes", {
  skip_if_not_installed("haven")

  raw <- c(1, 2, haven::tagged_na("a"), haven::tagged_na("b"), NA)
  x <- haven::labelled(raw,
    labels = c("Yes" = 1, "No" = 2,
               "No answer" = haven::tagged_na("a"),
               "Refused" = haven::tagged_na("b")),
    label = "Survey Q")
  attr(x, "na_tag_map") <- c("a" = -9, "b" = -8)
  attr(x, "na_tag_format") <- "spss"
  df <- data.frame(q1 = x)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  result <- read_xlsx(tmp)

  # Valid values preserved
  expect_equal(as.double(result$q1[1:2]), c(1, 2))
  # Tagged NAs restored
  expect_true(is.na(result$q1[3]))
  expect_true(is.na(result$q1[4]))
  expect_equal(haven::na_tag(result$q1[3]), "a")
  expect_equal(haven::na_tag(result$q1[4]), "b")
  # System NA still NA
  expect_true(is.na(result$q1[5]))
})

test_that("read_xlsx sets na_tag_map attribute correctly", {
  skip_if_not_installed("haven")

  raw <- c(1, 2, haven::tagged_na("a"))
  x <- haven::labelled(raw,
    labels = c("Yes" = 1, "No" = 2,
               "No answer" = haven::tagged_na("a")),
    label = "Q1")
  attr(x, "na_tag_map") <- c("a" = -9)
  attr(x, "na_tag_format") <- "spss"
  df <- data.frame(q1 = x)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  result <- read_xlsx(tmp)

  expect_equal(attr(result$q1, "na_tag_map"), c("a" = -9))
  expect_equal(attr(result$q1, "na_tag_format"), "spss")
})

test_that("read_xlsx labels=FALSE skips reconstruction", {
  skip_if_not_installed("haven")

  x <- haven::labelled(c(1, 2, 3),
                        labels = c("A" = 1, "B" = 2),
                        label = "Test")
  df <- data.frame(val = x)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  result <- read_xlsx(tmp, labels = FALSE)

  expect_false(inherits(result$val, "haven_labelled"))
  expect_null(attr(result$val, "label"))
})

# ============================================================================
# Tests for factor reconstruction
# ============================================================================
test_that("read_xlsx reconstructs factor columns", {
  df <- data.frame(color = factor(c("Red", "Blue", "Green", "Red")))
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  result <- read_xlsx(tmp)

  expect_s3_class(result$color, "factor")
  expect_true("Red" %in% levels(result$color))
  expect_true("Blue" %in% levels(result$color))
  expect_true("Green" %in% levels(result$color))
})

test_that("read_xlsx distinguishes factor from haven_labelled via Column_Type", {
  skip_if_not_installed("haven")

  x <- haven::labelled(c(1, 2, 1),
                        labels = c("Yes" = 1, "No" = 2))
  df <- data.frame(
    response = x,
    category = factor(c("A", "B", "A"))
  )
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  result <- read_xlsx(tmp)

  expect_s3_class(result$response, "haven_labelled")
  expect_s3_class(result$category, "factor")
})

# ============================================================================
# Tests for roundtrip fidelity
# ============================================================================
test_that("roundtrip preserves plain numeric and character columns", {
  df <- data.frame(
    nums = c(1.5, 2.5, 3.5),
    chars = c("hello", "world", "test"),
    stringsAsFactors = FALSE
  )
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  result <- read_xlsx(tmp)

  expect_equal(result$nums, df$nums)
  expect_equal(result$chars, df$chars)
})

test_that("roundtrip preserves variable labels on plain columns", {
  df <- data.frame(x = 1:3)
  attr(df$x, "label") <- "My Variable"
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  result <- read_xlsx(tmp)

  expect_equal(attr(result$x, "label"), "My Variable")
})

test_that("roundtrip with survey_data preserves structure", {
  skip_if_not_installed("haven")

  data("survey_data", package = "mariposa")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(survey_data, tmp)
  result <- read_xlsx(tmp)

  expect_equal(names(result), names(survey_data))
  expect_equal(nrow(result), nrow(survey_data))
})

# ============================================================================
# Tests for multi-sheet (list) files
# ============================================================================
test_that("read_xlsx reads from multi-sheet list export", {
  skip_if_not_installed("haven")

  df1 <- data.frame(
    x = haven::labelled(c(1, 2), labels = c("A" = 1, "B" = 2))
  )
  df2 <- data.frame(
    y = haven::labelled(c(3, 4), labels = c("C" = 3, "D" = 4))
  )
  data_list <- list("First" = df1, "Second" = df2)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(data_list, tmp)

  # Read first data sheet (default)
  result <- read_xlsx(tmp)
  expect_true("x" %in% names(result))

  # Read specific sheet
  result2 <- read_xlsx(tmp, sheet = "Second")
  expect_true("y" %in% names(result2))
})

test_that("read_xlsx applies per-sheet labels from list export", {
  skip_if_not_installed("haven")

  df1 <- data.frame(
    x = haven::labelled(c(1, 2), labels = c("A" = 1, "B" = 2))
  )
  attr(df1$x, "label") <- "Var X"
  df2 <- data.frame(
    y = haven::labelled(c(3, 4), labels = c("C" = 3, "D" = 4))
  )
  attr(df2$y, "label") <- "Var Y"
  data_list <- list("First" = df1, "Second" = df2)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(data_list, tmp)

  result <- read_xlsx(tmp, sheet = "First")
  expect_s3_class(result$x, "haven_labelled")
  expect_equal(attr(result$x, "label"), "Var X")
})

# ============================================================================
# Tests for backward compatibility (old format without Column_Type)
# ============================================================================
test_that("read_xlsx handles Labels sheet without Column_Type column", {
  skip_if_not_installed("haven")

  # Manually create an old-format file without Column_Type
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  wb <- openxlsx2::wb_workbook()

  # Data sheet with numeric codes
  wb$add_worksheet("Data")
  wb$add_data(sheet = "Data", x = data.frame(q1 = c(1, 2, -9, -8)))

  # Old-format Labels sheet (no Column_Type)
  labels_df <- data.frame(
    Variable = rep("q1", 4),
    Variable_Label = rep("Question 1", 4),
    Value = c("1", "2", "-9", "-8"),
    Value_Label = c("Yes", "No", "No answer", "Refused"),
    Type = c("valid", "valid", "missing", "missing"),
    stringsAsFactors = FALSE
  )
  wb$add_worksheet("Labels")
  wb$add_data(sheet = "Labels", x = labels_df)

  openxlsx2::wb_save(wb, tmp, overwrite = TRUE)

  result <- read_xlsx(tmp)

  # Should still reconstruct via heuristic
  expect_s3_class(result$q1, "haven_labelled")
  expect_equal(attr(result$q1, "labels")[["Yes"]], 1)
})

test_that("read_xlsx heuristic detects factors from old-format Labels", {
  # Manually create old-format with factor-like entries
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  wb <- openxlsx2::wb_workbook()

  wb$add_worksheet("Data")
  wb$add_data(sheet = "Data",
              x = data.frame(color = c("Red", "Blue", "Green"),
                             stringsAsFactors = FALSE))

  labels_df <- data.frame(
    Variable = rep("color", 3),
    Variable_Label = rep("Farbe", 3),
    Value = c("Red", "Blue", "Green"),
    Value_Label = c("Red", "Blue", "Green"),
    Type = c("valid", "valid", "valid"),
    stringsAsFactors = FALSE
  )
  wb$add_worksheet("Labels")
  wb$add_data(sheet = "Labels", x = labels_df)

  openxlsx2::wb_save(wb, tmp, overwrite = TRUE)

  result <- read_xlsx(tmp)
  expect_s3_class(result$color, "factor")
})

# ============================================================================
# Tests for edge cases
# ============================================================================
test_that("read_xlsx handles empty data frame", {
  df <- data.frame(x = integer(0), y = character(0))
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp, labels = FALSE)
  result <- read_xlsx(tmp)

  expect_equal(nrow(result), 0L)
})

test_that("read_xlsx handles variable in Labels but not in data", {
  # Labels sheet references a column that doesn't exist in data
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet("Data")
  wb$add_data(sheet = "Data", x = data.frame(x = 1:3))

  labels_df <- data.frame(
    Variable = "nonexistent",
    Variable_Label = "Ghost",
    Value = "1",
    Value_Label = "One",
    Type = "valid",
    Column_Type = "haven_labelled",
    stringsAsFactors = FALSE
  )
  wb$add_worksheet("Labels")
  wb$add_data(sheet = "Labels", x = labels_df)

  openxlsx2::wb_save(wb, tmp, overwrite = TRUE)

  # Should not error, just skip the missing variable
  result <- read_xlsx(tmp)
  expect_true("x" %in% names(result))
})

test_that("read_xlsx verbose mode prints summary", {
  skip_if_not_installed("haven")

  x <- haven::labelled(c(1, 2), labels = c("A" = 1, "B" = 2))
  df <- data.frame(val = x)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  expect_message(read_xlsx(tmp, verbose = TRUE), "Label reconstruction")
})

test_that("read_xlsx returns tibble for all input types", {
  df <- data.frame(x = 1:5)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  result <- read_xlsx(tmp)

  expect_s3_class(result, "tbl_df")
})


# ============================================================================
# Tests for duplicate / malformed labels (defensive reading)
# ============================================================================
test_that("read_xlsx handles duplicate valid label values gracefully", {
  skip_if_not_installed("haven")

  # Simulate a Labels sheet with duplicate Value entries (e.g. from older export)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet("Data")
  wb$add_data(sheet = "Data", x = data.frame(x = c(1, 2, 1, 2)))

  # Two entries map to Value=1 (duplicate)
  labels_df <- data.frame(
    Variable = rep("x", 3),
    Variable_Label = rep("Test var", 3),
    Value = c("1", "1", "2"),
    Value_Label = c("First", "Duplicate", "Second"),
    Type = rep("valid", 3),
    Column_Type = rep("haven_labelled", 3),
    stringsAsFactors = FALSE
  )
  wb$add_worksheet("Labels")
  wb$add_data(sheet = "Labels", x = labels_df)
  openxlsx2::wb_save(wb, tmp, overwrite = TRUE)

  # Should not error — duplicates are deduplicated (first wins)
  result <- read_xlsx(tmp)
  expect_s3_class(result, "tbl_df")

  labs <- attr(result$x, "labels")
  expect_true(length(labs) == 2L)
  expect_equal(unname(labs), c(1, 2))
  # First occurrence kept: "First" for value 1
  expect_equal(names(labs)[1L], "First")
})

test_that("read_xlsx handles overlap between valid and missing codes", {
  skip_if_not_installed("haven")

  # Missing code -9 also appears as valid (malformed Labels sheet)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet("Data")
  wb$add_data(sheet = "Data", x = data.frame(x = c(1, 2, -9)))

  labels_df <- data.frame(
    Variable = rep("x", 4),
    Variable_Label = rep("Test var", 4),
    Value = c("1", "2", "-9", "-9"),
    Value_Label = c("Yes", "No", "Wrong valid entry", "No answer"),
    Type = c("valid", "valid", "valid", "missing"),
    Column_Type = rep("haven_labelled", 4),
    stringsAsFactors = FALSE
  )
  wb$add_worksheet("Labels")
  wb$add_data(sheet = "Labels", x = labels_df)
  openxlsx2::wb_save(wb, tmp, overwrite = TRUE)

  # Missing takes precedence over valid when codes overlap
  result <- read_xlsx(tmp)
  expect_s3_class(result, "tbl_df")

  labs <- attr(result$x, "labels")
  # 2 valid labels (1, 2) — -9 removed from valid because it's also missing
  valid_labs <- labs[!is.na(labs)]
  expect_equal(length(valid_labs), 2L)
  expect_equal(unname(sort(valid_labs)), c(1, 2))

  # -9 should be tagged NA; missing label "No answer" is kept (not overwritten

  # by valid label) since it already has a non-empty label
  na_labs <- labs[is.na(labs)]
  expect_equal(length(na_labs), 1L)
  expect_equal(names(na_labs), "No answer")

  # na_tag_map should contain -9

  tag_map <- attr(result$x, "na_tag_map")
  expect_true(-9 %in% unname(tag_map))
})

test_that("read_xlsx handles duplicate missing codes", {
  skip_if_not_installed("haven")

  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet("Data")
  wb$add_data(sheet = "Data", x = data.frame(x = c(1, 2, -9, -9)))

  labels_df <- data.frame(
    Variable = rep("x", 4),
    Variable_Label = rep("Test var", 4),
    Value = c("1", "2", "-9", "-9"),
    Value_Label = c("Yes", "No", "No answer", "Duplicate missing"),
    Type = c("valid", "valid", "missing", "missing"),
    Column_Type = rep("haven_labelled", 4),
    stringsAsFactors = FALSE
  )
  wb$add_worksheet("Labels")
  wb$add_data(sheet = "Labels", x = labels_df)
  openxlsx2::wb_save(wb, tmp, overwrite = TRUE)

  # Should not error — duplicate missing codes deduplicated
  result <- read_xlsx(tmp)
  expect_s3_class(result, "tbl_df")

  tag_map <- attr(result$x, "na_tag_map")
  # Only one entry for -9
  expect_equal(length(tag_map), 1L)
  expect_equal(unname(tag_map), -9)
})

test_that("read_xlsx preserves all tagged NA labels (not just first)", {
  skip_if_not_installed("haven")

  # Multiple missing codes: all must survive the roundtrip, not just the first
  x <- haven::labelled(
    c(1, 2, 3, -9, -8, -7),
    labels = c("Low" = 1, "Med" = 2, "High" = 3,
               "No answer" = haven::tagged_na("a"),
               "Refused" = haven::tagged_na("b"),
               "Not applicable" = haven::tagged_na("c")),
    label = "Test scale"
  )
  attr(x, "na_tag_map") <- c(a = -9, b = -8, c = -7)
  attr(x, "na_tag_format") <- "spss"

  df <- data.frame(val = x)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  result <- read_xlsx(tmp)

  labs <- attr(result$val, "labels")
  tag_map <- attr(result$val, "na_tag_map")

  # All 3 valid + 3 missing labels must be present
  expect_equal(length(labs), 6L)
  expect_equal(sum(!is.na(labs)), 3L)  # 3 valid
  expect_equal(sum(is.na(labs)), 3L)   # 3 tagged NA

  # All missing code names preserved
  na_names <- names(labs)[is.na(labs)]
  expect_true("No answer" %in% na_names)
  expect_true("Refused" %in% na_names)
  expect_true("Not applicable" %in% na_names)

  # All 3 missing codes in tag_map
  expect_equal(length(tag_map), 3L)
  expect_equal(sort(unname(tag_map)), c(-9, -8, -7))
})

test_that("read_xlsx handles NA in Value_Label", {
  skip_if_not_installed("haven")

  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet("Data")
  wb$add_data(sheet = "Data", x = data.frame(x = c(1, 2, -9)))

  labels_df <- data.frame(
    Variable = rep("x", 3),
    Variable_Label = rep("Test var", 3),
    Value = c("1", "2", "-9"),
    Value_Label = c("Yes", NA, NA),
    Type = c("valid", "valid", "missing"),
    Column_Type = rep("haven_labelled", 3),
    stringsAsFactors = FALSE
  )
  wb$add_worksheet("Labels")
  wb$add_data(sheet = "Labels", x = labels_df)
  openxlsx2::wb_save(wb, tmp, overwrite = TRUE)

  # Should not error — NA labels replaced with value string
  result <- read_xlsx(tmp)
  expect_s3_class(result, "tbl_df")

  labs <- attr(result$x, "labels")
  # Label for value 2 should be "2" (fallback), not NA
  expect_false(any(is.na(names(labs))))
})

test_that("write_xlsx/read_xlsx roundtrip preserves missing values with regular numeric labels", {
  skip_if_not_installed("haven")

  # Simulate data where labels attribute contains regular numeric values
  # for codes that are also in na_tag_map (the ALLBUS-style pattern)
  x <- haven::labelled(
    c(1, 2, 3, haven::tagged_na("a"), haven::tagged_na("b"), haven::tagged_na("c")),
    labels = c("Low" = 1, "Mid" = 2, "High" = 3,
               "Filter" = -10, "No answer" = -9, "Don't know" = -8),
    label = "Scale item"
  )
  attr(x, "na_tag_map") <- c(a = -10, b = -9, c = -8)
  attr(x, "na_tag_format") <- "spss"

  df <- tibble::tibble(lm27 = x)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  result <- read_xlsx(tmp)

  # Same number of missing values
  expect_equal(sum(is.na(result$lm27)), 3L)
  expect_equal(sum(!is.na(result$lm27)), 3L)

  # na_tag_map restored with all 3 codes
  tag_map <- attr(result$lm27, "na_tag_map")
  expect_equal(length(tag_map), 3L)
  expect_equal(sort(unname(tag_map)), c(-10, -9, -8))

  # Valid labels preserved
  labs <- attr(result$lm27, "labels")
  valid_labs <- labs[!is.na(labs)]
  expect_equal(length(valid_labs), 3L)
  expect_equal(sort(unname(valid_labs)), c(1, 2, 3))

  # Missing labels preserved
  na_labs <- labs[is.na(labs)]
  expect_equal(length(na_labs), 3L)
  na_names <- names(na_labs)
  expect_true("Filter" %in% na_names)
  expect_true("No answer" %in% na_names)
  expect_true("Don't know" %in% na_names)
})
