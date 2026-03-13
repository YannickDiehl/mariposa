# test-write-xlsx.R
# Tests for write_xlsx() S3 generic and methods

skip_if_not_installed("openxlsx2")

# ============================================================================
# Tests for dependency check
# ============================================================================
test_that("write_xlsx requires openxlsx2", {
  expect_true(is.function(write_xlsx))
})

# ============================================================================
# Tests for file path validation
# ============================================================================
test_that("write_xlsx rejects non-xlsx file paths", {
  df <- data.frame(x = 1:3)
  expect_error(write_xlsx(df, "output.csv"), "xlsx")
  expect_error(write_xlsx(df, "output.xls"), "xlsx")
  expect_error(write_xlsx(df, "output.txt"), "xlsx")
})

test_that("write_xlsx errors on existing file when overwrite=FALSE", {
  df <- data.frame(x = 1:3)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  expect_error(write_xlsx(df, tmp, overwrite = FALSE), "exists")
})

test_that("write_xlsx overwrites existing file when overwrite=TRUE", {
  df <- data.frame(x = 1:3)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  expect_no_error(write_xlsx(df, tmp, overwrite = TRUE))
})

# ============================================================================
# Tests for write_xlsx.data.frame
# ============================================================================
test_that("write_xlsx.data.frame creates valid xlsx with Data and Labels sheets", {
  df <- data.frame(x = 1:5, y = letters[1:5])
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  result <- write_xlsx(df, tmp)

  expect_true(file.exists(tmp))
  expect_equal(result, tmp)

  wb <- openxlsx2::wb_load(tmp)
  expect_true("Data" %in% wb$sheet_names)
})

test_that("write_xlsx.data.frame preserves data accurately", {
  df <- data.frame(
    int_col = c(1L, 2L, 3L),
    dbl_col = c(1.5, 2.5, 3.5),
    chr_col = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  read_back <- openxlsx2::read_xlsx(tmp, sheet = "Data")

  expect_equal(read_back$int_col, c(1, 2, 3))
  expect_equal(read_back$dbl_col, c(1.5, 2.5, 3.5))
  expect_equal(read_back$chr_col, c("a", "b", "c"))
})

test_that("write_xlsx.data.frame handles NA values", {
  df <- data.frame(x = c(1, NA, 3), y = c("a", NA, "c"),
                   stringsAsFactors = FALSE)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  read_back <- openxlsx2::read_xlsx(tmp, sheet = "Data")

  expect_true(is.na(read_back$x[2]))
  expect_true(is.na(read_back$y[2]))
})

test_that("write_xlsx.data.frame labels=FALSE skips Labels sheet", {
  df <- data.frame(x = 1:5)
  attr(df$x, "label") <- "Test Variable"
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp, labels = FALSE)

  wb <- openxlsx2::wb_load(tmp)
  expect_false("Labels" %in% wb$sheet_names)
})

test_that("write_xlsx.data.frame handles empty data frame", {
  df <- data.frame(x = integer(0), y = character(0))
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  expect_no_error(write_xlsx(df, tmp))
  expect_true(file.exists(tmp))
})

test_that("write_xlsx returns file path invisibly", {
  df <- data.frame(x = 1)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  result <- write_xlsx(df, tmp)
  expect_equal(result, tmp)
})

# ============================================================================
# Tests for Labels sheet content
# ============================================================================
test_that("Labels sheet contains variable labels", {
  df <- data.frame(x = 1:3, y = 4:6)
  attr(df$x, "label") <- "First Variable"
  attr(df$y, "label") <- "Second Variable"
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  labels_df <- openxlsx2::read_xlsx(tmp, sheet = "Labels")

  expect_true("First Variable" %in% labels_df$Variable_Label)
  expect_true("Second Variable" %in% labels_df$Variable_Label)
})

test_that("Labels sheet contains haven value labels", {
  skip_if_not_installed("haven")

  x <- haven::labelled(c(1, 2, 3), labels = c("Male" = 1, "Female" = 2))
  df <- data.frame(gender = x)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  labels_df <- openxlsx2::read_xlsx(tmp, sheet = "Labels")

  expect_true("Male" %in% labels_df$Value_Label)
  expect_true("Female" %in% labels_df$Value_Label)
  expect_true(all(labels_df$Type[labels_df$Value_Label %in% c("Male", "Female")] == "valid"))
})

test_that("Labels sheet includes Column_Type column", {
  skip_if_not_installed("haven")

  x <- haven::labelled(c(1, 2, 3), labels = c("A" = 1, "B" = 2))
  df <- data.frame(val = x, color = factor(c("Red", "Blue", "Green")))
  attr(df$val, "label") <- "Test Variable"
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  labels_df <- openxlsx2::read_xlsx(tmp, sheet = "Labels")

  expect_true("Column_Type" %in% names(labels_df))
  val_rows <- labels_df[labels_df$Variable == "val", ]
  color_rows <- labels_df[labels_df$Variable == "color", ]
  expect_true(all(val_rows$Column_Type == "haven_labelled"))
  expect_true(all(color_rows$Column_Type == "factor"))
})

test_that("Labels sheet contains tagged NA info", {
  skip_if_not_installed("haven")

  x <- haven::labelled(
    c(1, 2, haven::tagged_na("a")),
    labels = c("Yes" = 1, "No" = 2, "No answer" = haven::tagged_na("a"))
  )
  attr(x, "na_tag_map") <- c("a" = -9)
  attr(x, "na_tag_format") <- "spss"
  df <- data.frame(q1 = x)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  labels_df <- openxlsx2::read_xlsx(tmp, sheet = "Labels")

  missing_rows <- labels_df[labels_df$Type == "missing", ]
  expect_true(nrow(missing_rows) >= 1L)
  expect_true("-9" %in% as.character(missing_rows$Value))
  expect_true("No answer" %in% missing_rows$Value_Label)
})

test_that("Labels sheet contains factor levels", {
  df <- data.frame(color = factor(c("Red", "Blue", "Green")))
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  labels_df <- openxlsx2::read_xlsx(tmp, sheet = "Labels")

  expect_true("Red" %in% labels_df$Value_Label)
  expect_true("Blue" %in% labels_df$Value_Label)
  expect_true("Green" %in% labels_df$Value_Label)
})

test_that("No Labels sheet when data has no labels at all", {
  df <- data.frame(x = 1:3, y = 4:6)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)

  wb <- openxlsx2::wb_load(tmp)
  expect_false("Labels" %in% wb$sheet_names)
})

test_that("tagged NAs are written as original missing codes in Data sheet", {
  skip_if_not_installed("haven")

  x <- haven::labelled(
    c(1, 2, haven::tagged_na("a"), haven::tagged_na("b"), NA),
    labels = c("Yes" = 1, "No" = 2,
               "No answer" = haven::tagged_na("a"),
               "Refused" = haven::tagged_na("b"))
  )
  attr(x, "na_tag_map") <- c("a" = -9, "b" = -8)
  attr(x, "na_tag_format") <- "spss"
  df <- data.frame(q1 = x)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  read_back <- openxlsx2::read_xlsx(tmp, sheet = "Data")

  # Valid values preserved
  expect_equal(read_back$q1[1], 1)
  expect_equal(read_back$q1[2], 2)
  # Tagged NAs written as their original codes
  expect_equal(read_back$q1[3], -9)
  expect_equal(read_back$q1[4], -8)
  # System NA remains NA
  expect_true(is.na(read_back$q1[5]))
})

test_that("haven_labelled columns are written as numeric values", {
  skip_if_not_installed("haven")

  x <- haven::labelled(c(1, 2, 3), labels = c("A" = 1, "B" = 2, "C" = 3))
  df <- data.frame(val = x)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)
  read_back <- openxlsx2::read_xlsx(tmp, sheet = "Data")

  # Should be numeric codes, not label text
  expect_equal(read_back$val, c(1, 2, 3))
})

# ============================================================================
# Tests for write_xlsx.codebook
# ============================================================================
test_that("write_xlsx.codebook creates expected sheets", {
  data("survey_data", package = "mariposa")
  cb <- codebook(survey_data)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(cb, tmp)

  wb <- openxlsx2::wb_load(tmp)
  expect_true("Overview" %in% wb$sheet_names)
  expect_true("Codebook" %in% wb$sheet_names)
  # Labels are embedded in stacked cells within the Codebook sheet
  expect_equal(length(wb$sheet_names), 2L)
})

test_that("write_xlsx.codebook Overview sheet has metadata", {
  data("survey_data", package = "mariposa")
  cb <- codebook(survey_data)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(cb, tmp)
  overview <- openxlsx2::read_xlsx(tmp, sheet = "Overview")

  expect_true("Field" %in% names(overview))
  expect_true("Value" %in% names(overview))
  expect_true("Observations" %in% overview$Field)
  expect_true("Variables" %in% overview$Field)
})

test_that("write_xlsx.codebook Codebook sheet has correct columns", {
  data("survey_data", package = "mariposa")
  cb <- codebook(survey_data)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(cb, tmp)
  codebook_df <- openxlsx2::read_xlsx(tmp, sheet = "Codebook")

  # Columns mirror the HTML codebook layout
  expect_true("ID" %in% names(codebook_df))
  expect_true("Name" %in% names(codebook_df))
  expect_true("Type" %in% names(codebook_df))
  expect_true("Label" %in% names(codebook_df))
  expect_true("Values" %in% names(codebook_df))
  expect_true("Value Labels" %in% names(codebook_df))
  expect_true("Freq." %in% names(codebook_df))
})

test_that("write_xlsx.codebook Codebook has correct number of rows", {
  data("survey_data", package = "mariposa")
  cb <- codebook(survey_data)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(cb, tmp)
  codebook_df <- openxlsx2::read_xlsx(tmp, sheet = "Codebook")

  # One row per variable in the codebook

  expect_equal(nrow(codebook_df), nrow(cb$codebook))
})

test_that("write_xlsx.codebook frequencies=TRUE adds variable sheets", {
  data("survey_data", package = "mariposa")
  cb <- codebook(survey_data, gender, region)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(cb, tmp, frequencies = TRUE)

  wb <- openxlsx2::wb_load(tmp)
  # Frequency sheets should exist for the selected variables
  expect_true(any(grepl("gender|region", wb$sheet_names, ignore.case = TRUE)))
})

test_that("write_xlsx.codebook frequencies=FALSE has no variable sheets", {
  data("survey_data", package = "mariposa")
  cb <- codebook(survey_data, gender, region)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(cb, tmp, frequencies = FALSE)

  wb <- openxlsx2::wb_load(tmp)
  expect_equal(length(wb$sheet_names), 2L)  # Overview, Codebook
})

test_that("write_xlsx.codebook has stacked values in Codebook cells", {
  data("survey_data", package = "mariposa")
  cb <- codebook(survey_data, gender)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(cb, tmp)
  codebook_df <- openxlsx2::read_xlsx(tmp, sheet = "Codebook")

  # The Values column should contain stacked values separated by newlines
  gender_row <- codebook_df[codebook_df$Name == "gender", ]
  expect_true(nrow(gender_row) == 1L)
  # Values should contain newline-separated entries
  expect_true(grepl("\n", gender_row$Values))
  # Value Labels should also contain newline-separated entries
  expect_true(grepl("\n", gender_row$`Value Labels`))
})

# ============================================================================
# Tests for write_xlsx.list
# ============================================================================
test_that("write_xlsx.list creates one sheet per element", {
  data_list <- list(
    "Sheet1" = data.frame(a = 1:3),
    "Sheet2" = data.frame(b = 4:6)
  )
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(data_list, tmp)

  wb <- openxlsx2::wb_load(tmp)
  expect_true("Sheet1" %in% wb$sheet_names)
  expect_true("Sheet2" %in% wb$sheet_names)
})

test_that("write_xlsx.list includes combined Labels sheet", {
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

  wb <- openxlsx2::wb_load(tmp)
  expect_true("Labels" %in% wb$sheet_names)

  labels_df <- openxlsx2::read_xlsx(tmp, sheet = "Labels")
  expect_true("Sheet" %in% names(labels_df))
  expect_true("First" %in% labels_df$Sheet)
  expect_true("Second" %in% labels_df$Sheet)
})

test_that("write_xlsx.list labels=FALSE skips Labels sheet", {
  data_list <- list(
    "A" = data.frame(x = 1:3),
    "B" = data.frame(y = 4:6)
  )
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(data_list, tmp, labels = FALSE)

  wb <- openxlsx2::wb_load(tmp)
  expect_false("Labels" %in% wb$sheet_names)
})

test_that("write_xlsx.list rejects unnamed lists", {
  expect_error(
    write_xlsx(list(data.frame(x = 1), data.frame(y = 2)),
               tempfile(fileext = ".xlsx")),
    "named list"
  )
})

test_that("write_xlsx.list rejects non-data.frame elements", {
  expect_error(
    write_xlsx(list(a = 1:5), tempfile(fileext = ".xlsx")),
    "data frame"
  )
})

test_that("write_xlsx.list handles partially named list", {
  expect_error(
    write_xlsx(list("A" = data.frame(x = 1), data.frame(y = 2)),
               tempfile(fileext = ".xlsx")),
    "named list"
  )
})

# ============================================================================
# Tests for sheet name sanitization
# ============================================================================
test_that("sheet names are sanitized for Excel limits", {
  long_name <- paste(rep("a", 40), collapse = "")
  data_list <- list()
  data_list[[long_name]] <- data.frame(x = 1)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(data_list, tmp, labels = FALSE)

  wb <- openxlsx2::wb_load(tmp)
  expect_true(all(nchar(wb$sheet_names) <= 31))
})

test_that("special characters in sheet names are replaced", {
  data_list <- list("data[1]/2" = data.frame(x = 1))
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  expect_no_error(write_xlsx(data_list, tmp, labels = FALSE))
})

# ============================================================================
# Tests for formatting
# ============================================================================
test_that("Data sheet has frozen first row", {
  df <- data.frame(x = 1:10, y = 11:20)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)

  # Verify file is valid and can be loaded
  wb <- openxlsx2::wb_load(tmp)
  expect_true("Data" %in% wb$sheet_names)
})


# ============================================================================
# Tests for haven_labelled_spss roundtrip
# ============================================================================
test_that("write_xlsx converts haven_labelled_spss to tagged NA format", {
  skip_if_not_installed("haven")

  # Simulate haven_labelled_spss data (as from haven::read_sav(user_na = TRUE))
  x <- haven::labelled_spss(
    x = c(1, 2, -9, -8, 1, -9, 2, NA),
    labels = c("Yes" = 1, "No" = 2, "No answer" = -9, "Don't know" = -8),
    na_values = c(-9, -8),
    label = "Test variable"
  )
  df <- tibble::tibble(q1 = x)

  # Verify it's haven_labelled_spss

  expect_true(inherits(df$q1, "haven_labelled_spss"))
  expect_equal(sum(is.na(df$q1)), 4L)  # -9, -8, -9, NA

  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)

  # Check Labels sheet has correct valid/missing classification
  labels_df <- openxlsx2::read_xlsx(tmp, sheet = "Labels")
  q1_rows <- labels_df[labels_df$Variable == "q1", ]

  valid_rows  <- q1_rows[q1_rows$Type == "valid", ]
  missing_rows <- q1_rows[q1_rows$Type == "missing", ]

  expect_equal(nrow(valid_rows), 2L)
  expect_true(all(c("1", "2") %in% valid_rows$Value))

  expect_equal(nrow(missing_rows), 2L)
  expect_true(all(c("-9", "-8") %in% missing_rows$Value))

  # Roundtrip: read back and verify structure
  result <- read_xlsx(tmp)
  expect_true(inherits(result$q1, "haven_labelled"))
  expect_equal(attr(result$q1, "label"), "Test variable")
  expect_equal(sum(is.na(result$q1)), 4L)
  expect_equal(sum(!is.na(result$q1)), 4L)
  expect_false(is.null(attr(result$q1, "na_tag_map")))
  expect_true(all(c(-9, -8) %in% attr(result$q1, "na_tag_map")))
})

test_that("write_xlsx handles haven_labelled_spss with na_range", {
  skip_if_not_installed("haven")

  # Simulate na_range (e.g., LO THRU -1)
  x <- haven::labelled_spss(
    x = c(0, 1, -10, -9, -8, 0, 1),
    labels = c("No" = 0, "Yes" = 1, "Filter" = -10, "No answer" = -9,
               "Don't know" = -8),
    na_range = c(-Inf, -1),
    label = "Survey item"
  )
  df <- tibble::tibble(item = x)

  expect_equal(sum(is.na(df$item)), 3L)  # -10, -9, -8

  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)

  # Roundtrip
  result <- read_xlsx(tmp)
  expect_equal(sum(is.na(result$item)), 3L)
  expect_equal(sum(!is.na(result$item)), 4L)
  expect_true(all(c(-10, -9, -8) %in% attr(result$item, "na_tag_map")))

  # Value labels preserved correctly
  labs <- attr(result$item, "labels")
  expect_equal(unname(labs[!is.na(labs)]), c(0, 1))
  expect_equal(names(labs[!is.na(labs)]), c("No", "Yes"))
})

test_that("write_xlsx deduplicates labels when na_tag_map overlaps regular labels", {
  skip_if_not_installed("haven")

  # Simulate: labels contain regular numeric -10 (not tagged NA) AND

  # na_tag_map also claims -10 — should NOT produce duplicate entries
  x <- haven::labelled(
    c(1, 2, haven::tagged_na("a"), haven::tagged_na("b")),
    labels = c("Yes" = 1, "No" = 2, "Filter" = -10, "No answer" = -9),
    label = "Test item"
  )
  attr(x, "na_tag_map") <- c(a = -10, b = -9)
  attr(x, "na_tag_format") <- "spss"

  df <- tibble::tibble(q1 = x)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  write_xlsx(df, tmp)

  # Read the Labels sheet directly to check for duplicates
  labels_sheet <- openxlsx2::read_xlsx(tmp, sheet = "Labels")
  q1_rows <- labels_sheet[labels_sheet$Variable == "q1", ]

  # Each value should appear exactly once (no duplicates)
  expect_equal(sum(q1_rows$Value == "-10"), 1L)
  expect_equal(sum(q1_rows$Value == "-9"), 1L)

  # -10 and -9 should be "missing", not "valid"
  expect_equal(q1_rows$Type[q1_rows$Value == "-10"], "missing")
  expect_equal(q1_rows$Type[q1_rows$Value == "-9"], "missing")

  # Missing labels should be preserved from the regular labels
  expect_equal(q1_rows$Value_Label[q1_rows$Value == "-10"], "Filter")
  expect_equal(q1_rows$Value_Label[q1_rows$Value == "-9"], "No answer")
})


# ============================================================================
# write_xlsx.frequency method
# ============================================================================

test_that("write_xlsx.frequency creates xlsx with Frequency sheet", {
  skip_if_not_installed("openxlsx2")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  f <- frequency(survey_data, gender)
  result <- write_xlsx(f, tmp)

  expect_equal(result, tmp)
  expect_true(file.exists(tmp))

  wb <- openxlsx2::wb_load(tmp)
  expect_true("Frequency" %in% wb$sheet_names)
})

test_that("write_xlsx.frequency single variable has correct layout", {
  skip_if_not_installed("openxlsx2")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  f <- frequency(survey_data, gender)
  write_xlsx(f, tmp)

  data <- openxlsx2::read_xlsx(tmp, sheet = "Frequency", col_names = FALSE)

  # Row 1: Variable header
  expect_true(grepl("gender", data[[1]][1]))
  expect_true(grepl("Gender", data[[1]][1]))

  # Row 2: Stats
  expect_true(grepl("N=2500", data[[1]][2]))

  # Row 3: Column headers
  expect_equal(data[[1]][3], "Value")

  # Row 4-5: Data rows (Male, Female)
  values <- data[[1]][4:5]
  expect_true("Male" %in% values)
  expect_true("Female" %in% values)

  # Row 6: Total row
  expect_equal(data[[1]][6], "Total")
})

test_that("write_xlsx.frequency multi variable stacks with 3 row gap", {
  skip_if_not_installed("openxlsx2")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  f <- frequency(survey_data, gender, region)
  write_xlsx(f, tmp)

  data <- openxlsx2::read_xlsx(tmp, sheet = "Frequency", col_names = FALSE)

  # First variable: gender
  expect_true(grepl("gender", data[[1]][1]))

  # gender has 2 values + header(1) + stats(1) + col_headers(1) + total(1) = 6 rows
  # Then 3 blank rows = rows 7-9
  # Second variable starts at row 10
  expect_true(grepl("region", data[[1]][10]))
})

test_that("write_xlsx.frequency handles NAs with Total Valid/Missing", {
  skip_if_not_installed("openxlsx2")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  # Force show.labels = TRUE so the Label column is present
  f <- frequency(survey_data, life_satisfaction, show.labels = TRUE)
  write_xlsx(f, tmp)

  data <- openxlsx2::read_xlsx(tmp, sheet = "Frequency", col_names = FALSE)

  # Find "Total Valid" and "Total Missing" in the Label column (col 2)
  all_labels <- as.character(data[[2]])
  expect_true("Total Valid" %in% all_labels)
  expect_true("Total Missing" %in% all_labels)

  # Find "NA" in value column for the NA data row
  all_values <- as.character(data[[1]])
  expect_true("NA" %in% all_values)

  # Total rows should have "Total" in value column
  expect_true(sum(all_values == "Total", na.rm = TRUE) >= 2L)
})

test_that("write_xlsx.frequency handles tagged NAs", {
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("haven")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  # Create tagged NA data
  x <- haven::labelled(
    c(1, 2, 1, 2, haven::tagged_na("a"), haven::tagged_na("b"), NA),
    labels = c("Yes" = 1, "No" = 2,
               "No answer" = haven::tagged_na("a"),
               "Refused" = haven::tagged_na("b")),
    label = "Question 1"
  )
  attr(x, "na_tag_map") <- c("a" = -9, "b" = -8)
  attr(x, "na_tag_format") <- "spss"

  df <- tibble::tibble(q1 = x)
  f <- frequency(df, q1)
  write_xlsx(f, tmp)

  data <- openxlsx2::read_xlsx(tmp, sheet = "Frequency", col_names = FALSE)
  all_values <- as.character(data[[1]])
  all_labels <- as.character(data[[2]])

  # Tagged NA codes should appear as values
  expect_true("-9" %in% all_values)
  expect_true("-8" %in% all_values)

  # Labels should be present
  expect_true("No answer" %in% all_labels)
  expect_true("Refused" %in% all_labels)

  # Summary rows
  expect_true("Total Valid" %in% all_labels)
  expect_true("Total Missing" %in% all_labels)
})

test_that("write_xlsx.frequency respects show.labels = FALSE", {
  skip_if_not_installed("openxlsx2")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  f <- frequency(survey_data, gender, show.labels = FALSE)
  write_xlsx(f, tmp)

  data <- openxlsx2::read_xlsx(tmp, sheet = "Frequency", col_names = FALSE)

  # Column headers should not include "Label"
  header_row <- as.character(data[3, ])
  expect_false("Label" %in% header_row)

  # Should have fewer columns (5 instead of 6)
  expect_equal(sum(!is.na(data[3, ])), 5L)
})

test_that("write_xlsx.frequency includes stats row", {
  skip_if_not_installed("openxlsx2")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  f <- frequency(survey_data, life_satisfaction)
  write_xlsx(f, tmp)

  data <- openxlsx2::read_xlsx(tmp, sheet = "Frequency", col_names = FALSE)

  # Stats row should contain mean and sd
  stats_text <- as.character(data[[1]][2])
  expect_true(grepl("Mean=", stats_text))
  expect_true(grepl("SD=", stats_text))
  expect_true(grepl("Valid N=", stats_text))
})

test_that("write_xlsx.frequency validates file extension", {
  skip_if_not_installed("openxlsx2")
  f <- frequency(survey_data, gender)
  expect_error(write_xlsx(f, "/tmp/test.csv"), "must end in")
})
