# =============================================================================
# 0.7.4 practice-test fixes — batch X3 (cross-cutting): central formatting
# helpers (print_stat_table, pad_utf8, group headers, whole-number display),
# weighted-formula hygiene and the print/summary consistency sweep
# =============================================================================
# One block per finding ID (see NEWS "Practice-test fixes").

data(survey_data, envir = environment())

# Display width of every line of a captured table block
.widths <- function(lines) nchar(sub("\\s+$", "", lines), type = "width")

# --- EDGE-19 / PAR-22 / EDGE-17: print_stat_table() ---------------------------

test_that("EDGE-19: print_stat_table() pads by display width, not bytes", {
  # sprintf("%-20s") counts bytes: every umlaut shifted the rest of its row
  # one column to the left (pearson/tukey/linreg/dunn tables).
  df <- data.frame(
    Variable = c("Größe_öü", "age", "Zufriedenheit_ä"),
    value = c(1.5, 2.25, 3),
    stringsAsFactors = FALSE
  )
  out <- capture.output(mariposa:::print_stat_table(df, digits = 3))
  expect_length(unique(.widths(out)), 1L)
  # right-aligned numbers end in the same display column
  rows <- out[grepl("[0-9]\\.[0-9]{3}", out)]
  ends <- regexpr("[0-9]$", sub("\\s+$", "", rows))
  expect_length(unique(nchar(substr(rows, 1, ends), type = "width")), 1L)
})

test_that("EDGE-17: print_stat_table() shows whole numbers beyond 2^31", {
  # formatC(format = "d") coerces to integer: counts or sums of weights of
  # 2^31 and more (expansion weights) printed "NA" with a coercion warning.
  df <- data.frame(Group = c("a", "b"), N = c(3e9, 12))
  expect_silent(out <- capture.output(mariposa:::print_stat_table(df)))
  expect_true(any(grepl("3000000000", out, fixed = TRUE)))
  expect_false(any(grepl("NA", out, fixed = TRUE)))
})

test_that("FMT-SPACE: print_stat_table() lines carry no trailing blank", {
  # cat(row, "\n") appended a space to every table line.
  df <- data.frame(Term = c("a", "b"), B = c(1.5, -2))
  out <- capture.output(mariposa:::print_stat_table(df))
  expect_false(any(grepl(" $", out)))
})

test_that("FMT-ALIGN: leading text columns of print_stat_table() are left-aligned", {
  # Only the first column was left-aligned: "Group 2" of dunn_test() and
  # "Variable 2" of partial_cor() were right-aligned under their header.
  df <- data.frame(g1 = c("Basic", "University"), g2 = c("University", "Basic"),
                   z = c(-1.5, 2), stringsAsFactors = FALSE)
  out <- capture.output(mariposa:::print_stat_table(df, digits = 3))
  hdr <- out[2]
  row2 <- out[5]
  expect_identical(regexpr("g2", hdr, fixed = TRUE)[1],
                   regexpr("Basic", row2, fixed = TRUE)[1])
  # a numeric first column is right-aligned like every number
  num_first <- data.frame(H = c(1.5, 171.178), df = c(3, 3))
  out2 <- capture.output(mariposa:::print_stat_table(num_first, digits = 3))
  expect_match(out2[4], "^    1\\.500")
})
