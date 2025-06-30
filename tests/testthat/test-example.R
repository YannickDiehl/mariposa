test_that("basic arithmetic works", {
  expect_equal(2 + 2, 4)
})

test_that("package can be loaded", {
  expect_true(requireNamespace("SurveyStat", quietly = TRUE))
})