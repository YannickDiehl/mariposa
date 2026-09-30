# =============================================================================
# Practice-test regressions: correlation & regression batch (REG-*, EDGE-17)
# =============================================================================
# Each block names the finding ID from the 2026-09 field report and what was
# wrong before the fix.

data(survey_data)

.reg_sd2 <- function() {
  d <- survey_data
  d$high_sat <- as.integer(d$life_satisfaction >= 4)
  d
}

# REG-01: a formula longer than ~70 characters deparsed to two lines - the
# compact title was printed twice and summary() aborted in
# print_info_section() ("'length = 2' in coercion to 'logical(1)'").
test_that("REG-01: long formulas print on one line in every regression output", {
  f <- life_satisfaction ~ age + income + trust_government + trust_media + trust_science
  m <- linear_regression(survey_data, f)
  out <- capture.output(print(m))
  expect_equal(sum(grepl("^Linear Regression:", out)), 1L)
  expect_true(any(grepl("trust_media + trust_science", out, fixed = TRUE)))
  expect_no_error(out_s <- capture.output(print(summary(m))))
  expect_true(any(grepl("Formula: .*trust_science", out_s)))

  d <- .reg_sd2()
  fl <- high_sat ~ age + income + trust_government + trust_media + trust_science
  ml <- logistic_regression(d, fl)
  out <- capture.output(print(ml))
  expect_equal(sum(grepl("^Logistic Regression:", out)), 1L)
  expect_no_error(capture.output(print(summary(ml))))

  me <- marginal_effects(ml)
  out <- capture.output(print(me))
  expect_equal(sum(grepl("^Average Marginal Effects:", out)), 1L)
  expect_no_error(capture.output(print(summary(me))))

  g <- dplyr::group_by(survey_data, region)
  mg <- linear_regression(g, f)
  expect_no_error(capture.output(print(summary(mg))))
  expect_equal(sum(grepl("^Linear Regression:", capture.output(print(mg)))), 1L)
})
