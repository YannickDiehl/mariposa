# test-read-por.R
# Tests for read_por()
# Note: haven does not provide write_por(), so file-based round-trip tests
# are limited. The shared .tag_spss_missing_values() helper is thoroughly
# tested via the read_spss() test suite.

skip_if_not_installed("haven")

# ============================================================================
# Tests for read_por() function interface
# ============================================================================
test_that("read_por has correct signature", {
  expect_true(is.function(read_por))
  args <- formals(read_por)
  expect_true("path" %in% names(args))
  expect_true("tag.na" %in% names(args))
  expect_true("verbose" %in% names(args))

  # read_por should NOT have encoding parameter
  expect_false("encoding" %in% names(args))

  # Defaults
  expect_true(args$tag.na)
  expect_false(args$verbose)
})

test_that("read_por errors with non-existent file", {
  expect_error(read_por("nonexistent.por"))
})

test_that("read_por is callable", {
  # The actual read behavior is tested via read_spss since they share
  # the .tag_spss_missing_values() internal helper
  expect_true(exists("read_por"))
})
