# ============================================================================
# ANCOVA - SPSS VALIDATION TESTS
# ============================================================================
# Purpose: Validate R ancova() against SPSS UNIANOVA with WITH keyword
# Dataset: survey_data (2,500 respondent synthetic survey)
# DVs: income, life_satisfaction
# Factors: gender, region, education
# Covariates: age, political_orientation
# Weight: sampling_weight (via /REGWGT for WLS)
# Created: 2026-03-02
# SPSS Version: 29
#
# SPSS syntax: tests/spss_reference/syntax/ancova.sps
# SPSS output: tests/spss_reference/outputs/ancova_output.txt
#
# Test matrix:
#   Tests 1a-1c: Unweighted, one-way ANCOVA (1 factor + 1 covariate)
#   Tests 2a-2b: Weighted, one-way ANCOVA
#   Tests 3a-3b: Unweighted, two-way ANCOVA (2 factors + 1 covariate)
#   Tests 4a-4b: Weighted, two-way ANCOVA
#   Test 5a:     Unweighted, one-way ANCOVA with 2 covariates
#   Test 6a:     Weighted, one-way ANCOVA with 2 covariates
# ============================================================================

library(testthat)
library(dplyr)
library(haven)
library(mariposa)

# ============================================================================
# SPSS REFERENCE VALUES
# ============================================================================

spss <- list()

# Test 1a: income BY education WITH age (unweighted)
spss$test_1a <- list(
  n = 2186,
  anova = list(
    corrected_model = list(ss = 1752821002.875, df = 4, ms = 438205250.719, f = 349.723, p = 0.000, eta = 0.391),
    intercept       = list(ss = 3472400779.184, df = 1, ms = 3472400779.184, f = 2771.252, p = 0.000, eta = 0.560),
    age             = list(ss = 37721.406, df = 1, ms = 37721.406, f = 0.030, p = 0.862, eta = 0.000),
    education       = list(ss = 1752630664.966, df = 3, ms = 584210221.656, f = 466.246, p = 0.000, eta = 0.391),
    error           = list(ss = 2732810163.640, df = 2181, ms = 1253007.870),
    total           = list(ss = 35290790000.000, df = 2186),
    corrected_total = list(ss = 4485631166.515, df = 2185)
  ),
  r_squared = 0.391, adj_r_squared = 0.390,
  levene = list(f = 103.953, df1 = 3, df2 = 2182, p = 0.000),
  emm = list(
    means = c(2758.939, 3592.634, 4224.320, 5336.870),
    se    = c(41.294, 47.822, 47.836, 59.438)
  )
)

# Test 1b: life_satisfaction BY gender WITH age (unweighted)
spss$test_1b <- list(
  n = 2421,
  anova = list(
    corrected_model = list(ss = 4.071, df = 2, ms = 2.036, f = 1.532, p = 0.216, eta = 0.001),
    intercept       = list(ss = 3410.020, df = 1, ms = 3410.020, f = 2565.986, p = 0.000, eta = 0.515),
    age             = list(ss = 2.691, df = 1, ms = 2.691, f = 2.025, p = 0.155, eta = 0.001),
    gender          = list(ss = 1.418, df = 1, ms = 1.418, f = 1.067, p = 0.302, eta = 0.000),
    error           = list(ss = 3213.356, df = 2418, ms = 1.329),
    total           = list(ss = 35088.000, df = 2421),
    corrected_total = list(ss = 3217.428, df = 2420)
  ),
  r_squared = 0.001, adj_r_squared = 0.000,
  levene = list(f = 1.306, df1 = 1, df2 = 2419, p = 0.253),
  emm = list(
    means = c(3.603, 3.651),
    se    = c(0.034, 0.032)
  )
)

# Test 1c: life_satisfaction BY education WITH political_orientation (unweighted)
spss$test_1c <- list(
  n = 2228,
  anova = list(
    corrected_model         = list(ss = 250.020, df = 4, ms = 62.505, f = 50.633, p = 0.000, eta = 0.083),
    intercept               = list(ss = 4137.304, df = 1, ms = 4137.304, f = 3351.442, p = 0.000, eta = 0.601),
    political_orientation   = list(ss = 0.004, df = 1, ms = 0.004, f = 0.004, p = 0.952, eta = 0.000),
    education               = list(ss = 250.017, df = 3, ms = 83.339, f = 67.509, p = 0.000, eta = 0.083),
    error                   = list(ss = 2744.260, df = 2223, ms = 1.234),
    total                   = list(ss = 32210.000, df = 2228),
    corrected_total         = list(ss = 2994.280, df = 2227)
  ),
  r_squared = 0.083, adj_r_squared = 0.082,
  levene = list(f = 24.350, df1 = 3, df2 = 2224, p = 0.000)
)

# Test 2a: income BY education WITH age (weighted)
spss$test_2a <- list(
  n = 2186,
  anova = list(
    corrected_model = list(ss = 1726313730.354, df = 4, ms = 431578432.589, f = 344.227, p = 0.000, eta = 0.387),
    intercept       = list(ss = 3522091861.884, df = 1, ms = 3522091861.884, f = 2809.219, p = 0.000, eta = 0.563),
    age             = list(ss = 24217.849, df = 1, ms = 24217.849, f = 0.019, p = 0.889, eta = 0.000),
    education       = list(ss = 1726217469.350, df = 3, ms = 575405823.117, f = 458.943, p = 0.000, eta = 0.387),
    error           = list(ss = 2734455037.942, df = 2181, ms = 1253762.053),
    total           = list(ss = 35297675826.553, df = 2186),
    corrected_total = list(ss = 4460768768.296, df = 2185)
  ),
  r_squared = 0.387, adj_r_squared = 0.386,
  levene = list(f = 95.368, df1 = 3, df2 = 2182, p = 0.000)
)

# Test 2b: life_satisfaction BY gender WITH age (weighted)
spss$test_2b <- list(
  n = 2421,
  anova = list(
    corrected_model = list(ss = 4.290, df = 2, ms = 2.145, f = 1.606, p = 0.201, eta = 0.001),
    intercept       = list(ss = 3468.736, df = 1, ms = 3468.736, f = 2597.637, p = 0.000, eta = 0.518),
    age             = list(ss = 2.770, df = 1, ms = 2.770, f = 2.074, p = 0.150, eta = 0.001),
    gender          = list(ss = 1.534, df = 1, ms = 1.534, f = 1.148, p = 0.284, eta = 0.000),
    error           = list(ss = 3228.859, df = 2418, ms = 1.335),
    total           = list(ss = 35249.294, df = 2421),
    corrected_total = list(ss = 3233.149, df = 2420)
  ),
  r_squared = 0.001, adj_r_squared = 0.001,
  levene = list(f = 0.902, df1 = 1, df2 = 2419, p = 0.342)
)

# Test 3a: income BY gender * education WITH age (unweighted)
spss$test_3a <- list(
  n = 2186,
  anova = list(
    corrected_model    = list(ss = 1754673974.673, df = 8, ms = 219334246.834, f = 174.844, p = 0.000, eta = 0.391),
    intercept          = list(ss = 3459603648.688, df = 1, ms = 3459603648.688, f = 2757.845, p = 0.000, eta = 0.559),
    age                = list(ss = 21904.825, df = 1, ms = 21904.825, f = 0.017, p = 0.895, eta = 0.000),
    gender             = list(ss = 121746.808, df = 1, ms = 121746.808, f = 0.097, p = 0.755, eta = 0.000),
    education          = list(ss = 1743507542.109, df = 3, ms = 581169180.703, f = 463.283, p = 0.000, eta = 0.390),
    gender_education   = list(ss = 1486565.745, df = 3, ms = 495521.915, f = 0.395, p = 0.757, eta = 0.001),
    error              = list(ss = 2730957191.842, df = 2177, ms = 1254458.977),
    total              = list(ss = 35290790000.000, df = 2186),
    corrected_total    = list(ss = 4485631166.515, df = 2185)
  ),
  r_squared = 0.391, adj_r_squared = 0.389,
  levene = list(f = 45.006, df1 = 7, df2 = 2178, p = 0.000)
)

# Test 3b: life_satisfaction BY gender * region WITH age (unweighted)
spss$test_3b <- list(
  n = 2421,
  anova = list(
    corrected_model = list(ss = 5.879, df = 4, ms = 1.470, f = 1.106, p = 0.352, eta = 0.002),
    intercept       = list(ss = 3146.242, df = 1, ms = 3146.242, f = 2366.870, p = 0.000, eta = 0.495),
    age             = list(ss = 2.567, df = 1, ms = 2.567, f = 1.931, p = 0.165, eta = 0.001),
    gender          = list(ss = 0.013, df = 1, ms = 0.013, f = 0.010, p = 0.921, eta = 0.000),
    region          = list(ss = 0.010, df = 1, ms = 0.010, f = 0.007, p = 0.931, eta = 0.000),
    gender_region   = list(ss = 1.789, df = 1, ms = 1.789, f = 1.346, p = 0.246, eta = 0.001),
    error           = list(ss = 3211.549, df = 2416, ms = 1.329),
    total           = list(ss = 35088.000, df = 2421),
    corrected_total = list(ss = 3217.428, df = 2420)
  ),
  r_squared = 0.002, adj_r_squared = 0.000,
  levene = list(f = 1.562, df1 = 3, df2 = 2417, p = 0.197)
)

# Test 4a: income BY gender * education WITH age (weighted)
spss$test_4a <- list(
  n = 2186,
  anova = list(
    corrected_model    = list(ss = 1727833011.823, df = 8, ms = 215979126.478, f = 172.044, p = 0.000, eta = 0.387),
    intercept          = list(ss = 3512177106.461, df = 1, ms = 3512177106.461, f = 2797.728, p = 0.000, eta = 0.562),
    age                = list(ss = 15715.020, df = 1, ms = 15715.020, f = 0.013, p = 0.911, eta = 0.000),
    gender             = list(ss = 143752.719, df = 1, ms = 143752.719, f = 0.115, p = 0.735, eta = 0.000),
    education          = list(ss = 1715889821.560, df = 3, ms = 571963273.854, f = 455.614, p = 0.000, eta = 0.386),
    gender_education   = list(ss = 1122625.930, df = 3, ms = 374208.643, f = 0.298, p = 0.827, eta = 0.000),
    error              = list(ss = 2732935756.474, df = 2177, ms = 1255367.826),
    total              = list(ss = 35297675826.553, df = 2186),
    corrected_total    = list(ss = 4460768768.296, df = 2185)
  ),
  r_squared = 0.387, adj_r_squared = 0.385,
  levene = list(f = 41.256, df1 = 7, df2 = 2178, p = 0.000)
)

# Test 4b: life_satisfaction BY gender * region WITH age (weighted)
spss$test_4b <- list(
  n = 2421,
  anova = list(
    corrected_model = list(ss = 6.389, df = 4, ms = 1.597, f = 1.196, p = 0.311, eta = 0.002),
    intercept       = list(ss = 3200.086, df = 1, ms = 3200.086, f = 2396.028, p = 0.000, eta = 0.498),
    age             = list(ss = 2.675, df = 1, ms = 2.675, f = 2.003, p = 0.157, eta = 0.001),
    gender          = list(ss = 0.016, df = 1, ms = 0.016, f = 0.012, p = 0.912, eta = 0.000),
    region          = list(ss = 0.013, df = 1, ms = 0.013, f = 0.010, p = 0.922, eta = 0.000),
    gender_region   = list(ss = 2.093, df = 1, ms = 2.093, f = 1.567, p = 0.211, eta = 0.001),
    error           = list(ss = 3226.760, df = 2416, ms = 1.336),
    total           = list(ss = 35249.294, df = 2421),
    corrected_total = list(ss = 3233.149, df = 2420)
  ),
  r_squared = 0.002, adj_r_squared = 0.000,
  levene = list(f = 2.412, df1 = 3, df2 = 2417, p = 0.065)
)

# Test 5a: income BY education WITH age, political_orientation (unweighted, 2 covariates)
spss$test_5a <- list(
  n = 2008,
  anova = list(
    corrected_model         = list(ss = 1582046608.943, df = 5, ms = 316409321.789, f = 252.422, p = 0.000, eta = 0.387),
    intercept               = list(ss = 1958785035.087, df = 1, ms = 1958785035.087, f = 1562.659, p = 0.000, eta = 0.438),
    age                     = list(ss = 21945.241, df = 1, ms = 21945.241, f = 0.018, p = 0.895, eta = 0.000),
    political_orientation   = list(ss = 1712167.106, df = 1, ms = 1712167.106, f = 1.366, p = 0.243, eta = 0.001),
    education               = list(ss = 1576957660.342, df = 3, ms = 525652553.448, f = 419.350, p = 0.000, eta = 0.386),
    error                   = list(ss = 2509497136.078, df = 2002, ms = 1253495.073),
    total                   = list(ss = 32140360000.000, df = 2008),
    corrected_total         = list(ss = 4091543745.020, df = 2007)
  ),
  r_squared = 0.387, adj_r_squared = 0.385,
  levene = list(f = 97.401, df1 = 3, df2 = 2004, p = 0.000)
)

# Test 6a: income BY education WITH age, political_orientation (weighted, 2 covariates)
spss$test_6a <- list(
  n = 2008,
  anova = list(
    corrected_model         = list(ss = 1555386966.105, df = 5, ms = 311077393.221, f = 248.257, p = 0.000, eta = 0.383),
    intercept               = list(ss = 1964488458.775, df = 1, ms = 1964488458.775, f = 1567.770, p = 0.000, eta = 0.439),
    age                     = list(ss = 4531.335, df = 1, ms = 4531.335, f = 0.004, p = 0.952, eta = 0.000),
    political_orientation   = list(ss = 1467183.764, df = 1, ms = 1467183.764, f = 1.171, p = 0.279, eta = 0.001),
    education               = list(ss = 1550660494.565, df = 3, ms = 516886831.522, f = 412.504, p = 0.000, eta = 0.382),
    error                   = list(ss = 2508598693.486, df = 2002, ms = 1253046.300),
    total                   = list(ss = 32122529623.043, df = 2008),
    corrected_total         = list(ss = 4063985659.590, df = 2007)
  ),
  r_squared = 0.383, adj_r_squared = 0.381,
  levene = list(f = 89.755, df1 = 3, df2 = 2004, p = 0.000)
)


# ============================================================================
# TOLERANCE DEFINITIONS
# ============================================================================

tol <- list(
  ss       = 0.01,
  ms       = 0.01,
  f        = 0.001,
  p        = 0.001,
  eta      = 0.001,
  r_sq     = 0.001,
  df       = 0,
  n        = 0,
  levene_f = 0.1,      # Levene small algorithm differences allowed
  levene_p = 0.01,
  emm_mean = 0.01,
  emm_se   = 0.01
)


# ============================================================================
# HELPER: Validate ANOVA table row
# ============================================================================

check_anova_row <- function(r_table, source_name, spss_ref, tol, label) {
  row <- r_table[r_table$source == source_name, ]
  expect_equal(nrow(row), 1, label = paste(label, "- row exists:", source_name))

  expect_equal(round(unname(row$ss), 3), spss_ref$ss, tolerance = tol$ss,
               label = paste(label, source_name, "- SS"))
  expect_equal(unname(row$df), spss_ref$df, tolerance = tol$df,
               label = paste(label, source_name, "- df"))

  if (!is.null(spss_ref$ms)) {
    expect_equal(round(unname(row$ms), 3), spss_ref$ms, tolerance = tol$ms,
                 label = paste(label, source_name, "- MS"))
  }
  if (!is.null(spss_ref$f)) {
    expect_equal(round(unname(row$f), 3), spss_ref$f, tolerance = tol$f,
                 label = paste(label, source_name, "- F"))
  }
  if (!is.null(spss_ref$p)) {
    expect_equal(round(unname(row$p), 3), spss_ref$p, tolerance = tol$p,
                 label = paste(label, source_name, "- p"))
  }
  if (!is.null(spss_ref$eta)) {
    expect_equal(round(unname(row$partial_eta_sq), 3), spss_ref$eta,
                 tolerance = tol$eta,
                 label = paste(label, source_name, "- Partial Eta Sq"))
  }
}


# ============================================================================
# HELPER: Run standard ANCOVA checks
# ============================================================================

run_ancova_test <- function(result, ref, label, anova_sources) {
  at <- result$anova_table

  # Sample size
  expect_equal(result$call_info$n_total, ref$n, label = paste(label, "N"))

  # ANOVA table
  for (src_name in names(anova_sources)) {
    display_name <- anova_sources[[src_name]]
    check_anova_row(at, display_name, ref$anova[[src_name]], tol, label)
  }

  # R-Squared
  expect_equal(round(unname(result$r_squared["r_squared"]), 3), ref$r_squared,
               tolerance = tol$r_sq, label = paste(label, "R-squared"))
  expect_equal(round(unname(result$r_squared["adj_r_squared"]), 3), ref$adj_r_squared,
               tolerance = tol$r_sq, label = paste(label, "Adj R-squared"))

  # Levene's Test
  expect_equal(round(unname(result$levene_test$f), 3), ref$levene$f,
               tolerance = tol$levene_f, label = paste(label, "Levene F"))
  expect_equal(unname(result$levene_test$df1), ref$levene$df1,
               tolerance = tol$df, label = paste(label, "Levene df1"))
  expect_equal(unname(result$levene_test$df2), ref$levene$df2,
               tolerance = tol$df, label = paste(label, "Levene df2"))
}


# ============================================================================
# TEST 1a: Unweighted - income BY education WITH age
# ============================================================================

test_that("Test 1a: Unweighted ANCOVA - income BY education WITH age", {
  result <- ancova(survey_data,
                   dv = income, between = c(education),
                   covariate = c(age))

  sources <- list(
    corrected_model = "Corrected Model", intercept = "Intercept",
    age = "age", education = "education",
    error = "Error", total = "Total", corrected_total = "Corrected Total"
  )
  run_ancova_test(result, spss$test_1a, "Test 1a", sources)

  # Estimated Marginal Means
  emm <- result$estimated_marginal_means
  expect_equal(unname(round(emm$mean, 3)), spss$test_1a$emm$means,
               tolerance = tol$emm_mean, label = "Test 1a EMM means")
  expect_equal(unname(round(emm$se, 3)), spss$test_1a$emm$se,
               tolerance = tol$emm_se, label = "Test 1a EMM SE")
})


# ============================================================================
# TEST 1b: Unweighted - life_satisfaction BY gender WITH age
# ============================================================================

test_that("Test 1b: Unweighted ANCOVA - life_satisfaction BY gender WITH age", {
  result <- ancova(survey_data,
                   dv = life_satisfaction, between = c(gender),
                   covariate = c(age))

  sources <- list(
    corrected_model = "Corrected Model", intercept = "Intercept",
    age = "age", gender = "gender",
    error = "Error", total = "Total", corrected_total = "Corrected Total"
  )
  run_ancova_test(result, spss$test_1b, "Test 1b", sources)

  emm <- result$estimated_marginal_means
  expect_equal(unname(round(emm$mean, 3)), spss$test_1b$emm$means,
               tolerance = tol$emm_mean, label = "Test 1b EMM means")
  expect_equal(unname(round(emm$se, 3)), spss$test_1b$emm$se,
               tolerance = tol$emm_se, label = "Test 1b EMM SE")
})


# ============================================================================
# TEST 1c: Unweighted - life_satisfaction BY education WITH political_orientation
# ============================================================================

test_that("Test 1c: Unweighted ANCOVA - life_satisfaction BY education WITH political_orientation", {
  result <- ancova(survey_data,
                   dv = life_satisfaction, between = c(education),
                   covariate = c(political_orientation))

  sources <- list(
    corrected_model = "Corrected Model", intercept = "Intercept",
    political_orientation = "political_orientation", education = "education",
    error = "Error", total = "Total", corrected_total = "Corrected Total"
  )
  run_ancova_test(result, spss$test_1c, "Test 1c", sources)
})


# ============================================================================
# TEST 2a: Weighted - income BY education WITH age
# ============================================================================

test_that("Test 2a: Weighted ANCOVA - income BY education WITH age", {
  result <- ancova(survey_data,
                   dv = income, between = c(education),
                   covariate = c(age), weights = sampling_weight)

  sources <- list(
    corrected_model = "Corrected Model", intercept = "Intercept",
    age = "age", education = "education",
    error = "Error", total = "Total", corrected_total = "Corrected Total"
  )
  run_ancova_test(result, spss$test_2a, "Test 2a", sources)
})


# ============================================================================
# TEST 2b: Weighted - life_satisfaction BY gender WITH age
# ============================================================================

test_that("Test 2b: Weighted ANCOVA - life_satisfaction BY gender WITH age", {
  result <- ancova(survey_data,
                   dv = life_satisfaction, between = c(gender),
                   covariate = c(age), weights = sampling_weight)

  sources <- list(
    corrected_model = "Corrected Model", intercept = "Intercept",
    age = "age", gender = "gender",
    error = "Error", total = "Total", corrected_total = "Corrected Total"
  )
  run_ancova_test(result, spss$test_2b, "Test 2b", sources)
})


# ============================================================================
# TEST 3a: Unweighted - income BY gender * education WITH age
# ============================================================================

test_that("Test 3a: Unweighted two-way ANCOVA - income BY gender * education WITH age", {
  result <- ancova(survey_data,
                   dv = income, between = c(gender, education),
                   covariate = c(age))

  sources <- list(
    corrected_model = "Corrected Model", intercept = "Intercept",
    age = "age", gender = "gender", education = "education",
    gender_education = "gender * education",
    error = "Error", total = "Total", corrected_total = "Corrected Total"
  )
  run_ancova_test(result, spss$test_3a, "Test 3a", sources)
})


# ============================================================================
# TEST 3b: Unweighted - life_satisfaction BY gender * region WITH age
# ============================================================================

test_that("Test 3b: Unweighted two-way ANCOVA - life_satisfaction BY gender * region WITH age", {
  result <- ancova(survey_data,
                   dv = life_satisfaction, between = c(gender, region),
                   covariate = c(age))

  sources <- list(
    corrected_model = "Corrected Model", intercept = "Intercept",
    age = "age", gender = "gender", region = "region",
    gender_region = "gender * region",
    error = "Error", total = "Total", corrected_total = "Corrected Total"
  )
  run_ancova_test(result, spss$test_3b, "Test 3b", sources)
})


# ============================================================================
# TEST 4a: Weighted - income BY gender * education WITH age
# ============================================================================

test_that("Test 4a: Weighted two-way ANCOVA - income BY gender * education WITH age", {
  result <- ancova(survey_data,
                   dv = income, between = c(gender, education),
                   covariate = c(age), weights = sampling_weight)

  sources <- list(
    corrected_model = "Corrected Model", intercept = "Intercept",
    age = "age", gender = "gender", education = "education",
    gender_education = "gender * education",
    error = "Error", total = "Total", corrected_total = "Corrected Total"
  )
  run_ancova_test(result, spss$test_4a, "Test 4a", sources)
})


# ============================================================================
# TEST 4b: Weighted - life_satisfaction BY gender * region WITH age
# ============================================================================

test_that("Test 4b: Weighted two-way ANCOVA - life_satisfaction BY gender * region WITH age", {
  result <- ancova(survey_data,
                   dv = life_satisfaction, between = c(gender, region),
                   covariate = c(age), weights = sampling_weight)

  sources <- list(
    corrected_model = "Corrected Model", intercept = "Intercept",
    age = "age", gender = "gender", region = "region",
    gender_region = "gender * region",
    error = "Error", total = "Total", corrected_total = "Corrected Total"
  )
  run_ancova_test(result, spss$test_4b, "Test 4b", sources)
})


# ============================================================================
# TEST 5a: Unweighted - income BY education WITH age, political_orientation
# ============================================================================

test_that("Test 5a: Unweighted ANCOVA with 2 covariates", {
  result <- ancova(survey_data,
                   dv = income, between = c(education),
                   covariate = c(age, political_orientation))

  sources <- list(
    corrected_model = "Corrected Model", intercept = "Intercept",
    age = "age", political_orientation = "political_orientation",
    education = "education",
    error = "Error", total = "Total", corrected_total = "Corrected Total"
  )
  run_ancova_test(result, spss$test_5a, "Test 5a", sources)
})


# ============================================================================
# TEST 6a: Weighted - income BY education WITH age, political_orientation
# ============================================================================

test_that("Test 6a: Weighted ANCOVA with 2 covariates", {
  result <- ancova(survey_data,
                   dv = income, between = c(education),
                   covariate = c(age, political_orientation),
                   weights = sampling_weight)

  sources <- list(
    corrected_model = "Corrected Model", intercept = "Intercept",
    age = "age", political_orientation = "political_orientation",
    education = "education",
    error = "Error", total = "Total", corrected_total = "Corrected Total"
  )
  run_ancova_test(result, spss$test_6a, "Test 6a", sources)
})


# ============================================================================
# STRUCTURAL TESTS
# ============================================================================

test_that("ancova returns correct structure", {
  result <- ancova(survey_data, dv = income, between = c(education),
                   covariate = c(age))

  expect_s3_class(result, "ancova")
  expect_true(all(c("anova_table", "parameter_estimates", "descriptives",
                     "estimated_marginal_means", "levene_test", "r_squared",
                     "model", "call_info") %in% names(result)))

  expect_s3_class(result$anova_table, "tbl_df")
  expect_s3_class(result$parameter_estimates, "tbl_df")
  expect_s3_class(result$estimated_marginal_means, "tbl_df")

  expect_equal(result$call_info$dv, "income")
  expect_equal(result$call_info$factors, "education")
  expect_equal(result$call_info$covariates, "age")
  expect_false(result$call_info$weighted)
})

test_that("ancova handles weighted analysis", {
  result <- ancova(survey_data, dv = income, between = c(education),
                   covariate = c(age), weights = sampling_weight)

  expect_true(result$call_info$weighted)
  expect_equal(result$call_info$weight_name, "sampling_weight")
})

test_that("ancova handles multiple covariates", {
  result <- ancova(survey_data, dv = income, between = c(education),
                   covariate = c(age, political_orientation))

  expect_equal(result$call_info$covariates, c("age", "political_orientation"))
  # Should have rows for both covariates
  at <- result$anova_table
  expect_true("age" %in% at$source)
  expect_true("political_orientation" %in% at$source)
})


# ============================================================================
# INPUT VALIDATION TESTS
# ============================================================================

test_that("ancova rejects invalid inputs", {
  expect_error(ancova(survey_data, dv = gender, between = c(education),
                      covariate = c(age)))
  expect_error(ancova("not_df", dv = income, between = c(education),
                      covariate = c(age)))
})


# ============================================================================
# PRINT METHOD TEST
# ============================================================================

test_that("print.ancova produces output without errors", {
  result <- ancova(survey_data, dv = income, between = c(education),
                   covariate = c(age))

  expect_output(print(result), "ANCOVA")
  expect_output(print(result), "eta2p")
  expect_output(print(result), "covariate")
  # Verbose sections available via summary()
  expect_output(print(summary(result)), "Tests of Between-Subjects Effects")
  expect_output(print(summary(result)), "Parameter Estimates")
  expect_output(print(summary(result)), "Estimated Marginal Means")
  expect_output(print(summary(result)), "Levene")
})
