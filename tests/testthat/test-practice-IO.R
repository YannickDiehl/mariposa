# Regression tests for the 2026-09 practice-test findings, batch IO
# (import/export, labels, recoding, row operations, pomps, standardize).
# Each block names the finding ID and what was wrong.

# IO-01: read_sas() passed catalog_encoding = NULL explicitly, which haven
# 2.5.x rejects ("Expected string vector of length 1") -> every .sas7bdat
# failed with the default arguments.
test_that("IO-01: read_sas() reads a .sas7bdat file with default arguments", {
  skip_if_not_installed("haven")
  f <- file.path(system.file("examples", package = "haven"), "iris.sas7bdat")
  skip_if_not(file.exists(f))
  d <- read_sas(f)
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 150L)
})
