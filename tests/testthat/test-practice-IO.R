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

# IO-16: untag_na(), na_frequencies() and write_xpt()'s re-tagging read the
# NA tag element by element (vapply(x[i], haven::na_tag)) - every element
# went through vctrs dispatch: untag_na() over ALLBUS took 9 s, write_spss()
# 12 s, write_xlsx() 27 s. The tag reads are vectorized now.
test_that("IO-16: vectorized untag_na() recovers codes, keeps system NA", {
  skip_if_not_installed("haven")
  x <- set_na(c(1, -9, 2, NA, -8, -9), -9, -8)
  expect_equal(as.vector(unclass(untag_na(x))), c(1, -9, 2, NA, -8, -9))
  # an unknown tag (not in the map) stays NA
  y <- x
  attr(y, "na_tag_map") <- attr(y, "na_tag_map")[1]
  expect_true(is.na(untag_na(y)[5]))
  nf <- na_frequencies(x)
  expect_equal(sum(nf$n), 4L)
})

test_that("IO-16: untag_na() on a large labelled vector is fast", {
  skip_on_cran()
  skip_if_not_installed("haven")
  set.seed(1)
  big <- set_na(sample(c(1:5, -9, -8), 2e5, replace = TRUE), -9, -8)
  elapsed <- system.time(u <- untag_na(big))[["elapsed"]]
  u <- as.vector(unclass(u))
  expect_equal(sum(u == -9 | u == -8), sum(is.na(big)))
  expect_lt(elapsed, 1)
})

test_that("IO-16: write_xpt() re-tags lowercase tags to uppercase", {
  skip_if_not_installed("haven")
  x <- set_na(c(1, -9, 2, -8), -9, -8)
  r <- .retag_uppercase(x)
  expect_equal(.na_tags(r), c(NA, "A", NA, "B"))
  expect_equal(attr(r, "na_tag_map"), attr(x, "na_tag_map"))
})
