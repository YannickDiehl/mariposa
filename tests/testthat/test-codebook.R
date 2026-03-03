# =============================================================================
# Tests for codebook()
# =============================================================================

# --- Basic functionality ---

test_that("codebook returns correct class", {
  data(survey_data, package = "mariposa")
  result <- codebook(survey_data)
  expect_s3_class(result, "codebook")
  expect_true("codebook" %in% names(result))
  expect_true("data_info" %in% names(result))
  expect_true("html" %in% names(result))
  expect_true("options" %in% names(result))
  expect_true("frequencies" %in% names(result))
})

test_that("codebook works with plain data.frame", {
  df <- data.frame(x = 1:10, y = letters[1:10], stringsAsFactors = FALSE)
  result <- codebook(df)
  expect_s3_class(result, "codebook")
  expect_equal(nrow(result$codebook), 2)
})

test_that("codebook works with tibble", {
  tbl <- tibble::tibble(a = 1:5, b = factor(c("A", "B", "A", "B", "A")))
  result <- codebook(tbl)
  expect_s3_class(result, "codebook")
  expect_equal(nrow(result$codebook), 2)
})

test_that("codebook handles all-numeric datasets", {
  df <- data.frame(x = rnorm(50), y = 1:50, z = runif(50))
  result <- codebook(df)
  expect_true(all(result$codebook$type_short %in% c("dbl", "int")))
})

test_that("codebook handles all-factor datasets", {
  df <- data.frame(
    a = factor(c("X", "Y", "Z")),
    b = factor(c("A", "B", "C"))
  )
  result <- codebook(df)
  expect_true(all(grepl("^fct", result$codebook$type_short)))
})

test_that("codebook handles mixed types", {
  df <- data.frame(
    num = 1:10,
    chr = letters[1:10],
    fct = factor(rep(c("A", "B"), 5)),
    lgl = c(TRUE, FALSE, TRUE, TRUE, FALSE, NA, TRUE, FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  result <- codebook(df)
  expect_equal(nrow(result$codebook), 4)
  types <- result$codebook$type_short
  expect_true("int" %in% types)
  expect_true("chr" %in% types)
  expect_true(any(grepl("fct", types)))
  expect_true("lgl" %in% types)
})


# --- Variable selection ---

test_that("empty ... selects all variables", {
  df <- data.frame(a = 1:5, b = 6:10, c = 11:15)
  result <- codebook(df)
  expect_equal(nrow(result$codebook), 3)
  expect_equal(result$codebook$name, c("a", "b", "c"))
})

test_that("explicit variable names work", {
  data(survey_data, package = "mariposa")
  result <- codebook(survey_data, age, gender)
  expect_equal(nrow(result$codebook), 2)
  expect_equal(result$codebook$name, c("age", "gender"))
})

test_that("tidyselect helpers work", {
  data(survey_data, package = "mariposa")
  result <- codebook(survey_data, starts_with("trust"))
  expect_true(all(grepl("^trust", result$codebook$name)))
  expect_equal(nrow(result$codebook), 3)
})

test_that("single variable selection works", {
  data(survey_data, package = "mariposa")
  result <- codebook(survey_data, age)
  expect_equal(nrow(result$codebook), 1)
  expect_equal(result$codebook$name, "age")
})


# --- Metadata accuracy ---

test_that("position numbers match original column order", {
  data(survey_data, package = "mariposa")
  result <- codebook(survey_data)
  expected_positions <- seq_along(names(survey_data))
  expect_equal(result$codebook$position, expected_positions)
})

test_that("variable labels are extracted correctly", {
  data(survey_data, package = "mariposa")
  result <- codebook(survey_data, age)
  expect_equal(result$codebook$label[1], "Age in years")
})

test_that("unique counts are accurate", {
  df <- data.frame(
    all_same = rep(1, 10),
    all_diff = 1:10,
    some_dup = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
  )
  result <- codebook(df)
  expect_equal(result$codebook$n_unique, c(1L, 10L, 5L))
})


# --- Type classification ---

test_that(".classify_type identifies types correctly", {
  expect_equal(.classify_type(1:5)$type_short, "int")
  expect_equal(.classify_type(c(1.0, 2.0))$type_short, "dbl")
  expect_equal(.classify_type(factor(c("a", "b")))$type_short, "fct(2)")
  expect_equal(.classify_type(ordered(c("a", "b"), levels = c("a", "b")))$type_short, "ord(2)")
  expect_equal(.classify_type(c("a", "b"))$type_short, "chr")
  expect_equal(.classify_type(c(TRUE, FALSE))$type_short, "lgl")
  expect_equal(.classify_type(Sys.Date())$type_short, "date")
})


# --- Empirical values ---

test_that("empirical values for factors show levels", {
  df <- data.frame(x = factor(c("Low", "Medium", "High"),
                               levels = c("Low", "Medium", "High")))
  result <- codebook(df)
  emp <- result$codebook$empirical_values[[1]]
  expect_equal(emp, c("Low", "Medium", "High"))
  expect_false(result$codebook$is_range[1])
})

test_that("empirical values for numeric with few values show all values", {
  df <- data.frame(x = c(1, 2, 3, 4, 5, 1, 2, 3))
  result <- codebook(df)
  emp <- result$codebook$empirical_values[[1]]
  expect_equal(emp, as.character(1:5))
  expect_false(result$codebook$is_range[1])
})

test_that("empirical values for numeric with many values show range", {
  df <- data.frame(x = 1:100)
  result <- codebook(df, max.values = 10)
  emp <- result$codebook$empirical_values[[1]]
  expect_true(grepl("-", emp))
  expect_true(result$codebook$is_range[1])
})

test_that("empirical values for all-NA show (all missing)", {
  df <- data.frame(x = rep(NA_real_, 5))
  result <- codebook(df)
  emp <- result$codebook$empirical_values[[1]]
  expect_equal(emp, "(all missing)")
  expect_true(result$codebook$is_range[1])
})

test_that("empirical values for character show distinct values", {
  df <- data.frame(x = c("a", "b", "c", "a", "b"), stringsAsFactors = FALSE)
  result <- codebook(df)
  emp <- result$codebook$empirical_values[[1]]
  expect_equal(emp, c("a", "b", "c"))
  expect_false(result$codebook$is_range[1])
})

test_that("empirical values for logical show TRUE/FALSE", {
  df <- data.frame(x = c(TRUE, FALSE, TRUE, NA))
  result <- codebook(df)
  emp <- result$codebook$empirical_values[[1]]
  expect_equal(emp, c("FALSE", "TRUE"))
})


# --- Value labels ---

test_that("factor levels appear as value labels", {
  df <- data.frame(x = factor(c("Low", "Medium", "High"),
                               levels = c("Low", "Medium", "High")))
  result <- codebook(df)
  val_labels <- result$codebook$value_labels[[1]]
  expect_true(!is.null(val_labels))
  expect_equal(length(val_labels), 3)
})

test_that("max.values truncates long value lists", {
  df <- data.frame(x = factor(letters))
  result <- codebook(df, max.values = 5)
  val_labels <- result$codebook$value_labels[[1]]
  expect_equal(length(val_labels), 5)
  expect_true(result$codebook$truncated[1])
  expect_equal(result$codebook$n_labels[1], 26)
})


# --- Frequencies ---

test_that("frequencies are computed by default", {
  df <- data.frame(x = factor(c("a", "b", "a", "b", "a")))
  result <- codebook(df)
  expect_true(!is.null(result$frequencies))
  expect_true("x" %in% names(result$frequencies))
  expect_true(is.data.frame(result$frequencies$x))
})

test_that("frequencies contain correct counts for factors", {
  df <- data.frame(x = factor(c("a", "b", "a", "b", "a")))
  result <- codebook(df)
  freq <- result$frequencies$x
  expect_true(nrow(freq) >= 2)
  total <- sum(freq$freq)
  expect_equal(total, 5)
})

test_that("frequencies computed for numeric with few unique values", {
  df <- data.frame(x = c(1, 2, 3, 1, 2, 3, 1))
  result <- codebook(df)
  expect_true("x" %in% names(result$frequencies))
})

test_that("frequencies not computed for high-cardinality numeric", {
  df <- data.frame(x = rnorm(100))
  result <- codebook(df, max.values = 10)
  # x has ~100 unique values, should not have frequencies
  expect_true(is.null(result$frequencies$x) || length(result$frequencies$x) == 0)
})

test_that("show.freq = FALSE suppresses freq column in options", {
  df <- data.frame(x = factor(c("a", "b", "a")))
  result <- codebook(df, show.freq = FALSE)
  expect_false(result$options$show.freq)
})


# --- HTML generation ---

test_that("html is generated as htmltools object", {
  df <- data.frame(x = 1:5, y = factor(c("a", "b", "a", "b", "a")))
  result <- codebook(df)
  expect_true(inherits(result$html, "shiny.tag"))
})

test_that("html contains expected column headers", {
  df <- data.frame(x = 1:5, y = factor(c("a", "b", "a", "b", "a")))
  result <- codebook(df)
  html_text <- as.character(result$html)
  expect_true(grepl("Codebook:", html_text))
  expect_true(grepl("ID", html_text, fixed = TRUE))
  expect_true(grepl("Name", html_text, fixed = TRUE))
  expect_true(grepl("Type", html_text, fixed = TRUE))
  expect_true(grepl("Values", html_text, fixed = TRUE))
  expect_true(grepl("Value Labels", html_text, fixed = TRUE))
  expect_true(grepl("Freq", html_text, fixed = TRUE))
})

test_that("html contains variable names", {
  df <- data.frame(my_var = 1:5, other_var = factor(c("a", "b", "a", "b", "a")))
  result <- codebook(df)
  html_text <- as.character(result$html)
  expect_true(grepl("my_var", html_text, fixed = TRUE))
  expect_true(grepl("other_var", html_text, fixed = TRUE))
})


# --- Print and summary methods ---

test_that("print.codebook produces output", {
  df <- data.frame(a = 1:5, b = factor(c("x", "y", "x", "y", "x")))
  result <- codebook(df)
  output <- capture.output(print(result))
  expect_true(length(output) > 0)
  expect_true(any(grepl("Codebook:", output)))
  expect_true(any(grepl("a", output)))
  expect_true(any(grepl("b", output)))
})

test_that("summary.codebook returns correct class", {
  df <- data.frame(a = 1:5)
  result <- codebook(df)
  s <- summary(result)
  expect_s3_class(s, "summary.codebook")
})

test_that("print.summary.codebook produces output", {
  data(survey_data, package = "mariposa")
  result <- codebook(survey_data)
  s <- summary(result)
  output <- capture.output(print(s))
  expect_true(length(output) > 0)
  expect_true(any(grepl("Codebook", output)))
  expect_true(any(grepl("Dataset:", output)))
  expect_true(any(grepl("age", output)))
})

test_that("summary shows empirical values", {
  df <- data.frame(x = factor(c("A", "B", "C")))
  result <- codebook(df)
  s <- summary(result)
  output <- capture.output(print(s))
  expect_true(any(grepl("Values:", output)))
  expect_true(any(grepl("A", output)))
})

test_that("summary boolean toggles work", {
  data(survey_data, package = "mariposa")
  result <- codebook(survey_data)

  # All FALSE shows minimal output
  s_minimal <- summary(result, overview = FALSE, variable_details = FALSE,
                       value_labels = FALSE)
  output_minimal <- capture.output(print(s_minimal))
  expect_true(any(grepl("Codebook", output_minimal)))  # header always shown
  expect_false(any(grepl("Dataset:", output_minimal)))  # overview off

  # value_labels off
  s_no_labels <- summary(result, value_labels = FALSE)
  output_no_labels <- capture.output(print(s_no_labels))
  expect_true(any(grepl("Dataset:", output_no_labels)))  # overview on
  expect_true(any(grepl("Values:", output_no_labels)))   # values still shown
})


# --- Data info ---

test_that("data_info captures correct counts", {
  data(survey_data, package = "mariposa")
  result <- codebook(survey_data)
  info <- result$data_info
  expect_equal(info$nrow, 2500)
  expect_equal(info$ncol, 16)
  expect_equal(info$n_selected, 16)
  expect_true(info$n_numeric > 0)
  expect_true(info$n_factors > 0)
  expect_true(info$n_missing_any > 0)
})


# --- Options and special behavior ---

test_that("sort.by.name orders alphabetically", {
  df <- data.frame(z = 1, a = 2, m = 3)
  result <- codebook(df, sort.by.name = TRUE)
  expect_equal(result$codebook$name, c("a", "m", "z"))
})

test_that("grouped data is ungrouped", {
  df <- dplyr::group_by(
    data.frame(g = c("A", "B", "A", "B"), x = 1:4),
    g
  )
  result <- codebook(df)
  expect_equal(result$data_info$groups, "g")
  expect_equal(nrow(result$codebook), 2)
})

test_that("file parameter saves HTML", {
  df <- data.frame(x = 1:5)
  tmp <- tempfile(fileext = ".html")
  result <- codebook(df, file = tmp)
  expect_true(file.exists(tmp))
  content <- readLines(tmp)
  expect_true(any(grepl("Codebook:", content)))
  unlink(tmp)
})


# --- Edge cases ---

test_that("codebook handles all-NA variable", {
  df <- data.frame(x = 1:5, y = rep(NA_real_, 5))
  result <- codebook(df)
  row_y <- result$codebook[result$codebook$name == "y", ]
  expect_equal(row_y$n_missing, 5L)
  expect_equal(row_y$n_unique, 0L)
  emp <- row_y$empirical_values[[1]]
  expect_equal(emp, "(all missing)")
})

test_that("codebook handles single-row dataset", {
  df <- data.frame(a = 1, b = "text", stringsAsFactors = FALSE)
  result <- codebook(df)
  expect_equal(nrow(result$codebook), 2)
  expect_equal(result$data_info$nrow, 1)
})

test_that("codebook handles long labels with truncation", {
  df <- data.frame(x = 1:5)
  attr(df$x, "label") <- paste(rep("A", 100), collapse = "")
  result <- codebook(df, max.len = 20)
  expect_equal(result$options$max.len, 20)
})

test_that("codebook rejects non-data.frame input", {
  expect_error(codebook(1:10), "must be a data frame")
  expect_error(codebook("not a df"), "must be a data frame")
})
