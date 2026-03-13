# ============================================================================
# STANDARDIZE & CENTER - UNIT TESTS
# ============================================================================
# Purpose: Validate std() and center() functions
# ============================================================================

library(testthat)
library(dplyr)
library(mariposa)

# ============================================================================
# TEST DATA
# ============================================================================

test_data <- tibble::tibble(
  id = 1:10,
  x = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20),
  y = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
  group = c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B")
)
attr(test_data$x, "label") <- "Test variable X"

# ============================================================================
# std() — STANDARD Z-SCORE
# ============================================================================

test_that("std with method=sd produces mean 0 and sd 1", {
  result <- std(test_data, x, suffix = "_z")
  z <- result$x_z
  expect_equal(mean(z), 0, tolerance = 1e-10)
  expect_equal(sd(z), 1, tolerance = 1e-10)
})

test_that("std preserves original when suffix is used", {
  result <- std(test_data, x, suffix = "_z")
  expect_equal(result$x, test_data$x)
  expect_true("x_z" %in% names(result))
})

test_that("std overwrites in-place without suffix", {
  result <- std(test_data, x)
  # x should now be z-scores
  expect_equal(mean(result$x), 0, tolerance = 1e-10)
})

test_that("std computes correct z-scores", {
  x <- c(2, 4, 6)
  # mean = 4, sd = 2
  result <- std(x)
  expect_equal(result, c(-1, 0, 1))
})

# ============================================================================
# std() — 2-SD METHOD
# ============================================================================

test_that("std with method=2sd divides by 2*SD", {
  x <- c(2, 4, 6)
  # mean = 4, sd = 2, 2*sd = 4
  result <- std(x, method = "2sd")
  expect_equal(result, c(-0.5, 0, 0.5))
})

# ============================================================================
# std() — MAD METHOD
# ============================================================================

test_that("std with method=mad uses median and MAD", {
  result <- std(c(1, 2, 3, 4, 5), method = "mad")
  expect_true(is.numeric(result))
  expect_length(result, 5)
  # Median-centered
  expect_equal(result[3], 0, tolerance = 1e-10)
})

# ============================================================================
# std() — GMD METHOD
# ============================================================================

test_that("std with method=gmd produces numeric result", {
  result <- std(c(1, 2, 3, 4, 5), method = "gmd")
  expect_true(is.numeric(result))
  expect_length(result, 5)
})

# ============================================================================
# std() — NA HANDLING
# ============================================================================

test_that("std handles NA values", {
  x <- c(1, 2, NA, 4, 5)
  result <- std(x)
  expect_true(is.na(result[3]))
  expect_true(is.numeric(result[1]))
})

test_that("std warns when SD is zero", {
  expect_warning(result <- std(c(5, 5, 5, 5)))
  expect_true(all(is.na(result)))
})

# ============================================================================
# std() — DATA FRAME WITH GROUP-BY
# ============================================================================

test_that("std with group_by standardizes within groups", {
  result <- test_data %>%
    group_by(group) %>%
    std(x, suffix = "_z")

  # Within each group, mean should be 0
  group_means <- result %>%
    summarise(m = mean(x_z), .groups = "drop")
  expect_equal(group_means$m[1], 0, tolerance = 1e-10)
  expect_equal(group_means$m[2], 0, tolerance = 1e-10)
})

test_that("std works with multiple variables", {
  result <- std(test_data, x, y, suffix = "_z")
  expect_true("x_z" %in% names(result))
  expect_true("y_z" %in% names(result))
  expect_equal(mean(result$x_z), 0, tolerance = 1e-10)
  expect_equal(mean(result$y_z), 0, tolerance = 1e-10)
})

test_that("std works with tidyselect", {
  data2 <- tibble::tibble(q1 = c(1, 2, 3), q2 = c(4, 5, 6), name = c("a", "b", "c"))
  result <- std(data2, starts_with("q"), suffix = "_z")
  expect_true("q1_z" %in% names(result))
  expect_true("q2_z" %in% names(result))
  expect_false("name_z" %in% names(result))
})

# ============================================================================
# std() — LABEL HANDLING
# ============================================================================

test_that("std appends (standardized) to variable label", {
  result <- std(test_data, x, suffix = "_z")
  expect_equal(attr(result$x_z, "label"), "Test variable X (standardized)")
})

test_that("std removes value labels", {
  labelled <- tibble::tibble(x = c(1, 2, 3, 4, 5))
  attr(labelled$x, "labels") <- c("Low" = 1, "High" = 5)
  result <- std(labelled, x)
  expect_null(attr(result$x, "labels"))
})

# ============================================================================
# std() — VECTOR INPUT
# ============================================================================

test_that("std works with vector input", {
  result <- std(c(10, 20, 30))
  expect_equal(mean(result), 0, tolerance = 1e-10)
  expect_equal(sd(result), 1, tolerance = 1e-10)
})

test_that("std errors on non-numeric vector", {
  expect_error(std(c("a", "b", "c")))
})

# ============================================================================
# center() — GRAND-MEAN CENTERING
# ============================================================================

test_that("center subtracts the mean", {
  result <- center(c(10, 20, 30))
  # mean = 20
  expect_equal(result, c(-10, 0, 10))
})

test_that("center produces mean 0", {
  result <- center(test_data, x, suffix = "_c")
  expect_equal(mean(result$x_c), 0, tolerance = 1e-10)
})

test_that("center preserves original when suffix is used", {
  result <- center(test_data, x, suffix = "_c")
  expect_equal(result$x, test_data$x)
  expect_true("x_c" %in% names(result))
})

test_that("center overwrites in-place without suffix", {
  result <- center(test_data, x)
  expect_equal(mean(result$x), 0, tolerance = 1e-10)
})

# ============================================================================
# center() — GROUP-MEAN CENTERING
# ============================================================================

test_that("center with group_by does group-mean centering", {
  result <- test_data %>%
    group_by(group) %>%
    center(x, suffix = "_gmc")

  # Within each group, mean should be 0
  group_means <- result %>%
    summarise(m = mean(x_gmc), .groups = "drop")
  expect_equal(group_means$m[1], 0, tolerance = 1e-10)
  expect_equal(group_means$m[2], 0, tolerance = 1e-10)
})

test_that("center group-mean values are correct", {
  # Group A: x = 2, 4, 6, 8, 10 -> mean = 6
  # Group B: x = 12, 14, 16, 18, 20 -> mean = 16
  result <- test_data %>%
    group_by(group) %>%
    center(x, suffix = "_gmc")

  expect_equal(result$x_gmc[1], 2 - 6)   # -4
  expect_equal(result$x_gmc[5], 10 - 6)  # 4
  expect_equal(result$x_gmc[6], 12 - 16) # -4
  expect_equal(result$x_gmc[10], 20 - 16) # 4
})

# ============================================================================
# center() — NA HANDLING
# ============================================================================

test_that("center handles NA values", {
  x <- c(10, NA, 30)
  result <- center(x)
  expect_true(is.na(result[2]))
  # mean(10, 30, na.rm=TRUE) = 20
  expect_equal(result[1], -10)
  expect_equal(result[3], 10)
})

# ============================================================================
# center() — LABEL HANDLING
# ============================================================================

test_that("center appends (centered) to variable label", {
  result <- center(test_data, x, suffix = "_c")
  expect_equal(attr(result$x_c, "label"), "Test variable X (centered)")
})

test_that("center removes value labels", {
  labelled <- tibble::tibble(x = c(1, 2, 3, 4, 5))
  attr(labelled$x, "labels") <- c("Low" = 1, "High" = 5)
  result <- center(labelled, x)
  expect_null(attr(result$x, "labels"))
})

# ============================================================================
# center() — MULTIPLE VARIABLES
# ============================================================================

test_that("center works with multiple variables", {
  result <- center(test_data, x, y, suffix = "_c")
  expect_true("x_c" %in% names(result))
  expect_true("y_c" %in% names(result))
  expect_equal(mean(result$x_c), 0, tolerance = 1e-10)
  expect_equal(mean(result$y_c), 0, tolerance = 1e-10)
})

# ============================================================================
# center() — ERROR HANDLING
# ============================================================================

test_that("center errors on non-numeric vector", {
  expect_error(center(c("a", "b", "c")))
})

test_that("center warns on non-numeric columns in data frame", {
  data2 <- tibble::tibble(x = c(1, 2, 3), name = c("a", "b", "c"))
  expect_warning(center(data2, name))
})


# ============================================================================
# WEIGHTED STANDARDIZATION — std()
# ============================================================================

w_test_data <- tibble::tibble(
  x = c(2, 4, 6, 8, 10),
  w = c(1, 1, 1, 1, 6),
  group = c("A", "A", "A", "B", "B")
)

test_that("weighted std produces correct z-scores", {
  # Weighted mean = sum(x*w)/sum(w) = (2+4+6+8+60)/10 = 80/10 = 8
  # Weighted var = sum(w*(x-8)^2)/(10-1)
  #   = (1*36 + 1*16 + 1*4 + 1*0 + 6*4) / 9
  #   = (36+16+4+0+24)/9 = 80/9
  # Weighted sd = sqrt(80/9)
  result <- std(w_test_data, x, weights = w, suffix = "_z")
  w_mean <- 8
  w_sd <- sqrt(80 / 9)
  expected <- (w_test_data$x - w_mean) / w_sd
  expect_equal(result$x_z, expected, tolerance = 1e-10)
})

test_that("weighted std with method=2sd divides by 2*weighted SD", {
  result <- std(w_test_data, x, weights = w, method = "2sd", suffix = "_z")
  w_mean <- 8
  w_sd <- sqrt(80 / 9)
  expected <- (w_test_data$x - w_mean) / (2 * w_sd)
  expect_equal(result$x_z, expected, tolerance = 1e-10)
})

test_that("weighted std errors for mad method", {
  expect_error(
    std(w_test_data, x, weights = w, method = "mad"),
    "not supported"
  )
})

test_that("weighted std errors for gmd method", {
  expect_error(
    std(w_test_data, x, weights = w, method = "gmd"),
    "not supported"
  )
})

test_that("weighted std with group_by standardizes within groups", {
  result <- w_test_data %>%
    group_by(group) %>%
    std(x, weights = w, suffix = "_z")

  # Group A: x=2,4,6 w=1,1,1 -> all weights equal
  # wmean_A = (2+4+6)/3 = 4, wvar = sum(1*(x-4)^2)/2 = (4+0+4)/2 = 4, wsd = 2
  expect_equal(result$x_z[1], (2 - 4) / 2, tolerance = 1e-10)
  expect_equal(result$x_z[2], (4 - 4) / 2, tolerance = 1e-10)
  expect_equal(result$x_z[3], (6 - 4) / 2, tolerance = 1e-10)

  # Group B: x=8,10 w=1,6 -> wmean = (8+60)/7 = 68/7
  wmean_B <- 68 / 7
  wvar_B <- (1 * (8 - wmean_B)^2 + 6 * (10 - wmean_B)^2) / 6
  wsd_B <- sqrt(wvar_B)
  expect_equal(result$x_z[4], (8 - wmean_B) / wsd_B, tolerance = 1e-10)
  expect_equal(result$x_z[5], (10 - wmean_B) / wsd_B, tolerance = 1e-10)
})

test_that("weighted std with vector input", {
  x <- c(2, 4, 6, 8, 10)
  w <- c(1, 1, 1, 1, 6)
  result <- std(x, weights = w)
  w_mean <- 8
  w_sd <- sqrt(80 / 9)
  expected <- (x - w_mean) / w_sd
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("weighted std handles NAs in x and weights", {
  d <- tibble::tibble(x = c(2, NA, 6, 8, 10), w = c(1, 1, 1, NA, 6))
  result <- std(d, x, weights = w, suffix = "_z")
  # Valid pairs: (2,1), (6,1), (10,6) -> wmean = (2+6+60)/8 = 8.5
  # wvar = (1*(2-8.5)^2 + 1*(6-8.5)^2 + 6*(10-8.5)^2)/(8-1)
  wmean <- 8.5
  wvar <- (1 * 42.25 + 1 * 6.25 + 6 * 2.25) / 7
  wsd <- sqrt(wvar)
  expect_equal(result$x_z[1], (2 - wmean) / wsd, tolerance = 1e-10)
  expect_true(is.na(result$x_z[2]))
  expect_equal(result$x_z[3], (6 - wmean) / wsd, tolerance = 1e-10)
  # x=8 has valid x but NA weight — standardized using wmean/wsd from valid pairs
  expect_equal(result$x_z[4], (8 - wmean) / wsd, tolerance = 1e-10)
  expect_equal(result$x_z[5], (10 - wmean) / wsd, tolerance = 1e-10)
})

test_that("std without weights is unchanged (NULL default)", {
  result1 <- std(test_data, x, suffix = "_z")
  result2 <- std(test_data, x, weights = NULL, suffix = "_z")
  expect_equal(result1$x_z, result2$x_z)
})


# ============================================================================
# WEIGHTED CENTERING — center()
# ============================================================================

test_that("weighted center subtracts weighted mean", {
  result <- center(w_test_data, x, weights = w, suffix = "_c")
  # wmean = 8
  expect_equal(result$x_c, w_test_data$x - 8, tolerance = 1e-10)
})

test_that("weighted center with group_by uses group-weighted means", {
  result <- w_test_data %>%
    group_by(group) %>%
    center(x, weights = w, suffix = "_c")

  # Group A: wmean = (2+4+6)/3 = 4
  expect_equal(result$x_c[1], 2 - 4, tolerance = 1e-10)
  expect_equal(result$x_c[2], 4 - 4, tolerance = 1e-10)
  expect_equal(result$x_c[3], 6 - 4, tolerance = 1e-10)

  # Group B: wmean = (8+60)/7 = 68/7
  wmean_B <- 68 / 7
  expect_equal(result$x_c[4], 8 - wmean_B, tolerance = 1e-10)
  expect_equal(result$x_c[5], 10 - wmean_B, tolerance = 1e-10)
})

test_that("weighted center with vector input", {
  x <- c(2, 4, 6, 8, 10)
  w <- c(1, 1, 1, 1, 6)
  result <- center(x, weights = w)
  expect_equal(result, x - 8, tolerance = 1e-10)
})

test_that("weighted center handles NAs", {
  d <- tibble::tibble(x = c(2, NA, 6, 8, 10), w = c(1, 1, 1, NA, 6))
  result <- center(d, x, weights = w, suffix = "_c")
  wmean <- 8.5
  expect_equal(result$x_c[1], 2 - wmean, tolerance = 1e-10)
  expect_true(is.na(result$x_c[2]))
  expect_equal(result$x_c[3], 6 - wmean, tolerance = 1e-10)
  expect_equal(result$x_c[4], 8 - wmean, tolerance = 1e-10)
  expect_equal(result$x_c[5], 10 - wmean, tolerance = 1e-10)
})

test_that("center without weights is unchanged", {
  result1 <- center(test_data, x, suffix = "_c")
  result2 <- center(test_data, x, weights = NULL, suffix = "_c")
  expect_equal(result1$x_c, result2$x_c)
})
