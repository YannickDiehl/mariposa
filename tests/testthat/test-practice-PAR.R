# =============================================================================
# 0.7.4 practice-test fixes — batch PAR (parametric tests, post-hoc,
# assumption checks): t_test, oneway_anova, factorial_anova, ancova,
# levene_test, tukey_test, scheffe_test, normality_test
# =============================================================================
# One block per finding ID (see NEWS "Bug fixes (2026-09 field report)").

data(survey_data, envir = environment())

# Labelled copies of survey_data factors, as read_spss() would deliver them
.lab <- function(f) {
  haven::labelled(as.integer(f), stats::setNames(seq_along(levels(f)), levels(f)))
}

# --- PAR-02 / PAR-10: grouping variables in SPSS order with value labels -----

test_that("PAR-02: t_test() group order follows the codes, not the row order", {
  # Levels came from unique() (order of appearance): sorting the data by the
  # grouping variable flipped the sign of t and of the mean difference.
  d <- survey_data
  d$female <- as.integer(d$gender == "Female")
  r1 <- t_test(d, life_satisfaction, group = female)
  r2 <- t_test(d[order(-d$female), ], life_satisfaction, group = female)
  expect_equal(r1$results$t_stat, r2$results$t_stat)
  expect_equal(r1$results$mean_diff, r2$results$mean_diff)
  expect_identical(as.character(r2$group_levels), c("0", "1"))
})

test_that("PAR-10: labelled grouping variables show value labels, not codes", {
  skip_if_not_installed("haven")
  # Codes were printed ("Groups compared: 1 vs. 2", Tukey rows "1 - 2",
  # descriptives and EMMs by code) although group_by() headers showed labels.
  d <- survey_data
  d$sex <- .lab(d$gender)
  d$edu <- .lab(d$education)

  tt <- t_test(d, life_satisfaction, group = sex)
  expect_identical(as.character(tt$group_levels), c("Male", "Female"))
  out <- capture.output(summary(tt))
  expect_true(any(grepl("Male vs. Female", out, fixed = TRUE)))

  ow <- oneway_anova(d, life_satisfaction, group = edu)
  expect_identical(names(ow$results$group_stats[[1]]), levels(survey_data$education))
  tk <- tukey_test(ow)
  expect_true(all(grepl("Secondary|University", tk$results$Comparison)))
  sc <- scheffe_test(ow)
  expect_true(all(grepl("Secondary|University", sc$results$Comparison)))

  fa <- factorial_anova(d, dv = life_satisfaction, between = c(sex, edu))
  expect_setequal(as.character(fa$descriptives$sex), c("Male", "Female"))
  expect_true(all(grepl("Secondary|University|Male|Female",
                        tukey_test(fa)$results$Comparison)))

  an <- ancova(d, dv = life_satisfaction, between = sex, covariate = age)
  expect_setequal(as.character(an$estimated_marginal_means$sex), c("Male", "Female"))

  # numbers are unchanged by the relabelling
  ref <- oneway_anova(survey_data, life_satisfaction, group = education)
  expect_equal(ow$results$F_statistic, ref$results$F_statistic)
})
