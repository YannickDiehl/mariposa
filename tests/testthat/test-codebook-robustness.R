# =============================================================================
# Codebook robustness regression tests (0.6.14)
# =============================================================================
# One numbered section per finding of the codebook stress test. Keep the
# numbering in sync with NEWS.md 0.6.14.

# --- 1. attr() partial matching must not fake variable labels ---------------

test_that("1. value labels without a variable label yield an empty label", {
  skip_if_not_installed("haven")
  df <- data.frame(x = 1:3)
  df$x <- haven::labelled(c(1, 2, 3),
                          labels = c(One = 1, Two = 2, Three = 3))

  result <- codebook(df)

  # Before the fix, attr(x, "label") partially matched "labels" and the
  # label column showed "1 | 2 | 3"
  expect_true(is.na(result$codebook$label[1]))
  expect_equal(result$data_info$n_labelled, 0)

  # A real variable label is still picked up
  attr(df$x, "label") <- "A real label"
  result2 <- codebook(df)
  expect_equal(result2$codebook$label[1], "A real label")
  expect_equal(result2$data_info$n_labelled, 1)
})


# --- 2. Inline data expressions (multi-line deparse) -------------------------

test_that("2. codebook() works with inline data expressions", {
  expect_no_error({
    result <- codebook(data.frame(
      d = as.Date("2024-01-01") + 0:24,
      ts = as.POSIXct("2024-01-01 10:00:00", tz = "UTC") + 3600 * (0:24)
    ))
  })
  # Long inline expressions collapse to the generic name "data"
  expect_equal(result$data_info$name, "data")
})


# --- 3. List columns are skipped, not crashed on -----------------------------

test_that("3. list columns are skipped with a warning", {
  df <- data.frame(x = 1:3)
  df$lst <- list(1, "a", TRUE)

  expect_warning(result <- codebook(df), "list column")
  expect_equal(result$codebook$name, "x")
  expect_false("lst" %in% names(result$frequencies))
})

test_that("3. a data frame with only list columns aborts cleanly", {
  df <- data.frame(x = 1:2)
  df$l1 <- list(1, 2)
  df$l2 <- list("a", "b")
  df <- df[, c("l1", "l2")]

  expect_error(
    suppressWarnings(codebook(df)),
    "list columns"
  )
})


# --- 4. Tagged-NA breakdown in HTML, xlsx AND summary ------------------------

test_that("4. all-user-missing variable shows NA codes in all three layers", {
  skip_if_not_installed("haven")
  df <- data.frame(x = c(-9, -8, -9, -8, -9))
  df$x <- haven::labelled(df$x, labels = c("Refused" = -9, "Dont know" = -8))
  df <- set_na(df, x = c(-9, -8))

  cb <- codebook(df)

  # HTML: the range branch must append the tagged-NA rows too
  html_text <- as.character(cb$html)
  expect_true(grepl("Refused", html_text, fixed = TRUE))
  expect_true(grepl(">-9<", html_text, fixed = TRUE))
  expect_true(grepl("na-value", html_text, fixed = TRUE))

  # Summary: show_na renders a missing-values section with codes/labels/counts
  out <- capture.output(print(summary(cb)))
  expect_true(any(grepl("Missing values:", out)))
  expect_true(any(grepl("-9 = Refused \\(n = 3\\)", out)))
  expect_true(any(grepl("-8 = Dont know \\(n = 2\\)", out)))

  # xlsx: stacked cells carry the same codes, labels, and counts
  skip_if_not_installed("openxlsx2")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  write_xlsx(cb, tmp)
  sheet <- openxlsx2::read_xlsx(tmp, sheet = "Codebook")
  expect_true(grepl("-9", sheet$Values[1], fixed = TRUE))
  expect_true(grepl("Refused", sheet$`Value Labels`[1], fixed = TRUE))
  expect_true(grepl("3", sheet$Freq.[1], fixed = TRUE))
})

test_that("4. NA frequencies are computed for high-cardinality variables", {
  skip_if_not_installed("haven")
  df <- data.frame(x = c(seq(0.1, 20, by = 0.13), rep(-9, 5)))
  df$x <- haven::labelled(df$x, labels = c("Refused" = -9))
  df <- set_na(df, x = c(-9))

  cb <- codebook(df, max_values = 10)

  # Range-displayed variable, but the frequency table exists for NA counts
  expect_true(cb$codebook$is_range[1])
  expect_true("x" %in% names(cb$frequencies))
  freq <- cb$frequencies$x
  na_row <- freq[!is.na(freq$na_display_value) & freq$na_display_value == "-9", ]
  expect_equal(round(na_row$freq[1]), 5)

  # And the HTML range branch shows the NA breakdown below the range
  html_text <- as.character(cb$html)
  expect_true(grepl("distinct", html_text, fixed = TRUE))
  expect_true(grepl("Refused", html_text, fixed = TRUE))
})


# --- 5. Central display formatting for numeric values ------------------------

test_that("5. fractional values display with 4 significant digits", {
  df <- data.frame(x = c(1 / 3, 2 / 3, 1))
  cb <- codebook(df)
  expect_equal(cb$codebook$empirical_values[[1]], c("0.3333", "0.6667", "1"))
  # Whole numbers keep their integer look
  expect_false(any(grepl("1\\.0", cb$codebook$empirical_values[[1]])))
})

test_that("5. tiny values are never shown in raw scientific notation", {
  df <- data.frame(x = c(1e-8, 2e-8, 3e-8))
  cb <- codebook(df)
  emp <- cb$codebook$empirical_values[[1]]
  expect_equal(emp, c("0.00000001", "0.00000002", "0.00000003"))
  expect_false(any(grepl("e-", emp)))
})

test_that("5. frequency matching still works for fractional values", {
  df <- data.frame(x = rep(c(1 / 3, 2 / 3), 3))
  cb <- codebook(df)

  # Keys carry the raw representation and match the frequency rows
  keys <- cb$codebook$empirical_keys[[1]]
  expect_true(all(keys %in% as.character(cb$frequencies$x$value)))

  # The HTML freq column contains the actual counts (3 per value)
  html_text <- as.character(cb$html)
  expect_true(grepl("0.3333", html_text, fixed = TRUE))
  expect_true(grepl(">3</div>", html_text, fixed = TRUE))
})

test_that("5. codebook xlsx frequency sheets round percentages to 2 decimals", {
  skip_if_not_installed("openxlsx2")
  df <- data.frame(x = factor(c("a", "a", "b", "c", "c", "c", "d")))
  cb <- codebook(df)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  write_xlsx(cb, tmp, frequencies = TRUE)

  freq_sheet <- openxlsx2::read_xlsx(tmp, sheet = "x")
  for (col in intersect(c("prc", "valid_prc", "cum_prc", "n_eff"),
                        names(freq_sheet))) {
    vals <- as.numeric(freq_sheet[[col]])
    vals <- vals[!is.na(vals)]
    expect_equal(vals, round(vals, 2))
  }
})


# --- 6. max_values / max_len validation --------------------------------------

test_that("6. max_values and max_len are validated up front", {
  df <- data.frame(x = 1:3)
  expect_error(codebook(df, max_values = 0), "max_values")
  expect_error(codebook(df, max_values = c(2, 3)), "max_values")
  expect_error(codebook(df, max_values = 2.5), "max_values")
  expect_error(codebook(df, max_values = NA), "max_values")
  expect_error(codebook(df, max_values = "10"), "max_values")
  expect_error(codebook(df, max_len = 3), "max_len")
  expect_error(codebook(df, max_len = "a"), "max_len")
})


# --- 7. Factor levels respect max_values --------------------------------------

test_that("7. factor levels are truncated to max_values with a note", {
  df <- data.frame(x = factor(letters))
  cb <- codebook(df, max_values = 5)

  emp <- cb$codebook$empirical_values[[1]]
  expect_length(emp, 6)
  expect_equal(emp[1:5], letters[1:5])
  expect_match(emp[6], "\\(21 more\\)")

  # The note entry has no matching key
  keys <- cb$codebook$empirical_keys[[1]]
  expect_true(is.na(keys[6]))
})


# --- 8. Range display shows cardinality ---------------------------------------

test_that("8. range displays include the distinct-value count", {
  cb <- codebook(data.frame(x = 1:100))
  expect_equal(cb$codebook$empirical_values[[1]], "1 - 100 (100 distinct)")

  # Fractional ranges use the display formatter (no 15-digit doubles)
  set.seed(42)
  cb2 <- codebook(data.frame(x = runif(50)))
  emp2 <- cb2$codebook$empirical_values[[1]]
  expect_match(emp2, "\\(50 distinct\\)")
  expect_false(grepl("e-", emp2))
  expect_false(grepl("[0-9]{8,}", emp2))  # no full-precision doubles
})


# --- 9. view argument controls the Viewer side effect -------------------------

test_that("9. view defaults off in non-interactive sessions", {
  df <- data.frame(x = 1:3)
  cb <- codebook(df)
  expect_false(cb$viewed)

  # The print hint no longer claims a Viewer was opened
  out <- capture.output(print(cb))
  expect_false(any(grepl("Open HTML viewer", out)))
  expect_true(any(grepl("view = TRUE", out)))
})

test_that("9. view = FALSE suppresses the Viewer but file= still writes", {
  df <- data.frame(x = 1:3)
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  cb <- codebook(df, file = tmp, view = FALSE)
  expect_true(file.exists(tmp))
  expect_false(cb$viewed)
})

test_that("9. view = TRUE opens the viewer and sets the viewed flag", {
  df <- data.frame(x = 1:3)
  opened <- FALSE
  old <- options(viewer = function(url, ...) opened <<- TRUE)
  on.exit(options(old), add = TRUE)

  cb <- codebook(df, view = TRUE)
  expect_true(cb$viewed)
  expect_true(opened)
  out <- capture.output(print(cb))
  expect_true(any(grepl("Open HTML viewer", out)))
})


# --- 10. file= into a nonexistent directory -----------------------------------

test_that("10. file into a nonexistent directory aborts with a clear error", {
  df <- data.frame(x = 1:3)
  bad_file <- file.path(tempdir(), "no_such_dir_xyz", "cb.html")
  expect_error(codebook(df, file = bad_file), "does not exist")
})


# --- 11. Polish: pluralization, value truncation, empty data ------------------

test_that("11. singular counts are not pluralized", {
  df <- data.frame(x = 1)
  out <- capture.output(print(codebook(df)))
  expect_true(any(grepl("1 variable | 1 observation", out, fixed = TRUE)))
  expect_false(any(grepl("1 variables", out)))
  expect_false(any(grepl("1 observations", out)))

  html_text <- as.character(codebook(df)$html)
  expect_false(grepl("1 variables", html_text))
  expect_false(grepl("1 observations", html_text))
})

test_that("11. very long character values are truncated to max_len", {
  df <- data.frame(x = paste0(strrep("a", 100), 1:3))
  cb <- codebook(df, max_len = 20)
  emp <- cb$codebook$empirical_values[[1]]
  expect_true(all(nchar(emp) <= 20))
  expect_true(all(grepl("\\.\\.\\.$", emp)))

  # Raw keys stay untruncated for matching
  keys <- cb$codebook$empirical_keys[[1]]
  expect_true(all(nchar(keys) == 101))
})

test_that("11. zero-row data frames say (no observations)", {
  df <- data.frame(x = numeric(0))
  cb <- codebook(df)
  expect_equal(cb$codebook$empirical_values[[1]], "(no observations)")

  # All-missing columns with rows keep the (all missing) wording
  cb2 <- codebook(data.frame(x = rep(NA_real_, 5)))
  expect_equal(cb2$codebook$empirical_values[[1]], "(all missing)")
})
