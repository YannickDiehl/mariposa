# ==============================================================================
# SPSS Validation Tests for Weighted Statistics (w_* functions)
# ==============================================================================
# Validated against: SPSS Version 29.0
# Output file: tests/spss_reference/outputs/weighted_statistics_output.txt
# Dataset: survey_data (2,500 respondents)
# Variables: age, income
# Weight: sampling_weight
# Grouping: region (East/West)
# ==============================================================================

# --- SPSS Reference Values ---------------------------------------------------

spss_ref <- list(
  # ============================================================================
  # SCENARIO 1: UNWEIGHTED, UNGROUPED
  # ============================================================================
  uw_ug = list(
    age = list(
      n = 2500, mean = 50.5496, sd = 16.97602, var = 288.185,
      se = 0.33952, skew = 0.172, kurtosis = -0.364,
      range = 77.00, min = 18.00, max = 95.00,
      median = 50.0000, mode = 18.00, q25 = 38.0000, q75 = 62.0000
    ),
    income = list(
      n = 2186, mean = 3753.9341, sd = 1432.80161, var = 2052920.442,
      se = 30.64510, skew = 0.730, kurtosis = 0.376,
      range = 7200.00, min = 800.00, max = 8000.00,
      median = 3500.0000, mode = 3200.00, q25 = 2700.0000, q75 = 4600.0000
    )
  ),

  # ============================================================================
  # SCENARIO 2: WEIGHTED, UNGROUPED
  # ============================================================================
  w_ug = list(
    age = list(
      n = 2516, mean = 50.5144, sd = 17.08382, var = 291.857,
      se = 0.34058, skew = 0.159, kurtosis = -0.396,
      range = 77.00, min = 18.00, max = 95.00,
      median = 50.0000, mode = 18.00, q25 = 38.0000, q75 = 63.0000
    ),
    income = list(
      n = 2201, mean = 3743.0994, sd = 1423.96558, var = 2027677.966,
      se = 30.35257, skew = 0.725, kurtosis = 0.388,
      range = 7200.00, min = 800.00, max = 8000.00,
      median = 3500.0000, mode = 3200.00, q25 = 2700.0000, q75 = 4600.0000
    )
  ),

  # ============================================================================
  # SCENARIO 3: UNWEIGHTED, GROUPED BY REGION
  # ============================================================================
  uw_g = list(
    east = list(
      age = list(
        n = 485, mean = 51.8680, sd = 17.42028, var = 303.466,
        se = 0.79101, skew = 0.148, kurtosis = -0.337,
        range = 77.00, min = 18.00, max = 95.00,
        median = 52.0000, mode = 18.00, q25 = 39.0000, q75 = 63.0000
      ),
      income = list(
        n = 429, mean = 3752.4476, sd = 1386.87938, var = 1923434.416,
        se = 66.95917, skew = 0.729, kurtosis = 0.489,
        range = 7200.00, min = 800.00, max = 8000.00,
        median = 3600.0000, mode = 3800.00, q25 = 2800.0000, q75 = 4500.0000
      )
    ),
    west = list(
      age = list(
        n = 2015, mean = 50.2323, sd = 16.85635, var = 284.137,
        se = 0.37551, skew = 0.175, kurtosis = -0.373,
        range = 77.00, min = 18.00, max = 95.00,
        median = 49.0000, mode = 18.00, q25 = 38.0000, q75 = 62.0000
      ),
      income = list(
        n = 1757, mean = 3754.2971, sd = 1444.17770, var = 2085649.235,
        se = 34.45361, skew = 0.731, kurtosis = 0.353,
        range = 7200.00, min = 800.00, max = 8000.00,
        median = 3500.0000, mode = 3100.00, q25 = 2700.0000, q75 = 4600.0000
      )
    )
  ),

  # ============================================================================
  # SCENARIO 4: WEIGHTED, GROUPED BY REGION
  # ============================================================================
  w_g = list(
    east = list(
      age = list(
        n = 509, mean = 52.2778, sd = 17.59548, var = 309.601,
        se = 0.77988, skew = 0.098, kurtosis = -0.389,
        range = 77.00, min = 18.00, max = 95.00,
        median = 53.0000, mode = 18.00, q25 = 40.0000, q75 = 64.0000
      ),
      income = list(
        n = 449, mean = 3760.6866, sd = 1388.32120, var = 1927435.755,
        se = 65.48281, skew = 0.721, kurtosis = 0.502,
        range = 7200.00, min = 800.00, max = 8000.00,
        median = 3600.0000, mode = 3800.00, q25 = 2800.0000, q75 = 4500.0000
      )
    ),
    west = list(
      age = list(
        n = 2007, mean = 50.0672, sd = 16.92689, var = 286.520,
        se = 0.37783, skew = 0.170, kurtosis = -0.396,
        range = 77.00, min = 18.00, max = 95.00,
        median = 49.0000, mode = 18.00, q25 = 38.0000, q75 = 62.0000
      ),
      income = list(
        n = 1751, mean = 3738.5858, sd = 1433.32495, var = 2054420.399,
        se = 34.24889, skew = 0.727, kurtosis = 0.364,
        range = 7200.00, min = 800.00, max = 8000.00,
        median = 3500.0000, mode = 3100.00, q25 = 2700.0000, q75 = 4600.0000
      )
    )
  )
)

# --- Tolerances ---------------------------------------------------------------

tol <- list(
  mean     = 0.01,
  sd       = 0.01,
  var      = 1,        # variance can be large numbers
  se       = 0.001,
  skew     = 0.01,
  kurtosis = 0.01,
  range    = 0,        # exact
  min_max  = 0,        # exact
  median   = 0.5,      # weighted median algorithms may differ
  quantile = 0.5,      # weighted quantile algorithms may differ
  mode     = 0,        # exact
  iqr      = 1         # derived from quantiles
)

# ==============================================================================
# SCENARIO 1: UNWEIGHTED, UNGROUPED
# ==============================================================================

test_that("w_mean: unweighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_mean(survey_data, age, income)

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  expect_equal(age_row$mean, spss_ref$uw_ug$age$mean, tolerance = tol$mean)
  expect_equal(inc_row$mean, spss_ref$uw_ug$income$mean, tolerance = tol$mean)
})

test_that("w_sd: unweighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_sd(survey_data, age, income)

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  expect_equal(age_row$sd, spss_ref$uw_ug$age$sd, tolerance = tol$sd)
  expect_equal(inc_row$sd, spss_ref$uw_ug$income$sd, tolerance = tol$sd)
})

test_that("w_var: unweighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_var(survey_data, age, income)

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  expect_equal(age_row$var, spss_ref$uw_ug$age$var, tolerance = tol$var)
  expect_equal(inc_row$var, spss_ref$uw_ug$income$var, tolerance = tol$var)
})

test_that("w_se: unweighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_se(survey_data, age, income)

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  expect_equal(age_row$se, spss_ref$uw_ug$age$se, tolerance = tol$se)
  expect_equal(inc_row$se, spss_ref$uw_ug$income$se, tolerance = tol$se)
})

test_that("w_skew: unweighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_skew(survey_data, age, income)

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  expect_equal(age_row$skew, spss_ref$uw_ug$age$skew, tolerance = tol$skew)
  expect_equal(inc_row$skew, spss_ref$uw_ug$income$skew, tolerance = tol$skew)
})

test_that("w_kurtosis: unweighted, ungrouped matches SPSS", {
  data(survey_data)
  # SPSS reports excess kurtosis by default
  result <- w_kurtosis(survey_data, age, income, excess = TRUE)

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  expect_equal(age_row$kurtosis, spss_ref$uw_ug$age$kurtosis, tolerance = tol$kurtosis)
  expect_equal(inc_row$kurtosis, spss_ref$uw_ug$income$kurtosis, tolerance = tol$kurtosis)
})

test_that("w_median: unweighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_median(survey_data, age, income)

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  expect_equal(age_row$median, spss_ref$uw_ug$age$median, tolerance = tol$median)
  expect_equal(inc_row$median, spss_ref$uw_ug$income$median, tolerance = tol$median)
})

test_that("w_range: unweighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_range(survey_data, age, income)

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  expect_equal(age_row$range, spss_ref$uw_ug$age$range, tolerance = tol$range)
  expect_equal(inc_row$range, spss_ref$uw_ug$income$range, tolerance = tol$range)
})

test_that("w_iqr: unweighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_iqr(survey_data, age, income)

  age_iqr_spss <- spss_ref$uw_ug$age$q75 - spss_ref$uw_ug$age$q25
  inc_iqr_spss <- spss_ref$uw_ug$income$q75 - spss_ref$uw_ug$income$q25

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  expect_equal(age_row$iqr, age_iqr_spss, tolerance = tol$iqr)
  expect_equal(inc_row$iqr, inc_iqr_spss, tolerance = tol$iqr)
})

test_that("w_modus: unweighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_modus(survey_data, age, income)

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  expect_equal(as.numeric(age_row$mode), spss_ref$uw_ug$age$mode, tolerance = tol$mode)
  expect_equal(as.numeric(inc_row$mode), spss_ref$uw_ug$income$mode, tolerance = tol$mode)
})

test_that("w_quantile: unweighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_quantile(survey_data, age, income, probs = c(0.25, 0.5, 0.75))

  # Extract quantile values from result (unname to drop "25%" etc. labels)
  age_q25 <- unname(result$results[["age_25%"]])
  age_q50 <- unname(result$results[["age_50%"]])
  age_q75 <- unname(result$results[["age_75%"]])

  expect_equal(age_q25, spss_ref$uw_ug$age$q25, tolerance = tol$quantile)
  expect_equal(age_q50, spss_ref$uw_ug$age$median, tolerance = tol$quantile)
  expect_equal(age_q75, spss_ref$uw_ug$age$q75, tolerance = tol$quantile)
})

# ==============================================================================
# SCENARIO 2: WEIGHTED, UNGROUPED
# ==============================================================================

test_that("w_mean: weighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_mean(survey_data, age, income, weights = sampling_weight)

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  expect_equal(age_row$weighted_mean, spss_ref$w_ug$age$mean, tolerance = tol$mean)
  expect_equal(inc_row$weighted_mean, spss_ref$w_ug$income$mean, tolerance = tol$mean)
})

test_that("w_sd: weighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_sd(survey_data, age, income, weights = sampling_weight)

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  expect_equal(age_row$weighted_sd, spss_ref$w_ug$age$sd, tolerance = tol$sd)
  expect_equal(inc_row$weighted_sd, spss_ref$w_ug$income$sd, tolerance = tol$sd)
})

test_that("w_var: weighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_var(survey_data, age, income, weights = sampling_weight)

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  expect_equal(age_row$weighted_var, spss_ref$w_ug$age$var, tolerance = tol$var)
  expect_equal(inc_row$weighted_var, spss_ref$w_ug$income$var, tolerance = tol$var)
})

test_that("w_se: weighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_se(survey_data, age, income, weights = sampling_weight)

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  expect_equal(age_row$weighted_se, spss_ref$w_ug$age$se, tolerance = tol$se)
  expect_equal(inc_row$weighted_se, spss_ref$w_ug$income$se, tolerance = tol$se)
})

test_that("w_skew: weighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_skew(survey_data, age, income, weights = sampling_weight)

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  expect_equal(age_row$weighted_skew, spss_ref$w_ug$age$skew, tolerance = tol$skew)
  expect_equal(inc_row$weighted_skew, spss_ref$w_ug$income$skew, tolerance = tol$skew)
})

test_that("w_kurtosis: weighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_kurtosis(survey_data, age, income, weights = sampling_weight, excess = TRUE)

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  expect_equal(age_row$weighted_kurtosis, spss_ref$w_ug$age$kurtosis, tolerance = tol$kurtosis)
  expect_equal(inc_row$weighted_kurtosis, spss_ref$w_ug$income$kurtosis, tolerance = tol$kurtosis)
})

test_that("w_median: weighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_median(survey_data, age, income, weights = sampling_weight)

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  expect_equal(age_row$weighted_median, spss_ref$w_ug$age$median, tolerance = tol$median)
  expect_equal(inc_row$weighted_median, spss_ref$w_ug$income$median, tolerance = tol$median)
})

test_that("w_range: weighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_range(survey_data, age, income, weights = sampling_weight)

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  # Range should not change with weights (same data values)
  expect_equal(age_row$weighted_range, spss_ref$w_ug$age$range, tolerance = tol$range)
  expect_equal(inc_row$weighted_range, spss_ref$w_ug$income$range, tolerance = tol$range)
})

test_that("w_iqr: weighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_iqr(survey_data, age, income, weights = sampling_weight)

  age_iqr_spss <- spss_ref$w_ug$age$q75 - spss_ref$w_ug$age$q25
  inc_iqr_spss <- spss_ref$w_ug$income$q75 - spss_ref$w_ug$income$q25

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  expect_equal(unname(age_row$weighted_iqr), age_iqr_spss, tolerance = tol$iqr)
  expect_equal(unname(inc_row$weighted_iqr), inc_iqr_spss, tolerance = tol$iqr)
})

test_that("w_modus: weighted, ungrouped matches SPSS", {
  data(survey_data)
  result <- w_modus(survey_data, age, income, weights = sampling_weight)

  age_row <- result$results[result$results$Variable == "age", ]
  inc_row <- result$results[result$results$Variable == "income", ]

  expect_equal(as.numeric(age_row$weighted_mode), spss_ref$w_ug$age$mode, tolerance = tol$mode)
  expect_equal(as.numeric(inc_row$weighted_mode), spss_ref$w_ug$income$mode, tolerance = tol$mode)
})

# ==============================================================================
# SCENARIO 3: UNWEIGHTED, GROUPED BY REGION
# ==============================================================================

test_that("w_mean: unweighted, grouped matches SPSS", {
  data(survey_data)
  result <- survey_data %>%
    dplyr::group_by(region) %>%
    w_mean(age, income)

  east <- result$results[result$results$region == "East", ]
  west <- result$results[result$results$region == "West", ]

  expect_equal(east$mean[east$Variable == "age"], spss_ref$uw_g$east$age$mean, tolerance = tol$mean)
  expect_equal(east$mean[east$Variable == "income"], spss_ref$uw_g$east$income$mean, tolerance = tol$mean)
  expect_equal(west$mean[west$Variable == "age"], spss_ref$uw_g$west$age$mean, tolerance = tol$mean)
  expect_equal(west$mean[west$Variable == "income"], spss_ref$uw_g$west$income$mean, tolerance = tol$mean)
})

test_that("w_sd: unweighted, grouped matches SPSS", {
  data(survey_data)
  result <- survey_data %>%
    dplyr::group_by(region) %>%
    w_sd(age, income)

  east <- result$results[result$results$region == "East", ]
  west <- result$results[result$results$region == "West", ]

  expect_equal(east$sd[east$Variable == "age"], spss_ref$uw_g$east$age$sd, tolerance = tol$sd)
  expect_equal(east$sd[east$Variable == "income"], spss_ref$uw_g$east$income$sd, tolerance = tol$sd)
  expect_equal(west$sd[west$Variable == "age"], spss_ref$uw_g$west$age$sd, tolerance = tol$sd)
  expect_equal(west$sd[west$Variable == "income"], spss_ref$uw_g$west$income$sd, tolerance = tol$sd)
})

test_that("w_var: unweighted, grouped matches SPSS", {
  data(survey_data)
  result <- survey_data %>%
    dplyr::group_by(region) %>%
    w_var(age, income)

  east <- result$results[result$results$region == "East", ]
  west <- result$results[result$results$region == "West", ]

  expect_equal(east$var[east$Variable == "age"], spss_ref$uw_g$east$age$var, tolerance = tol$var)
  expect_equal(east$var[east$Variable == "income"], spss_ref$uw_g$east$income$var, tolerance = tol$var)
  expect_equal(west$var[west$Variable == "age"], spss_ref$uw_g$west$age$var, tolerance = tol$var)
  expect_equal(west$var[west$Variable == "income"], spss_ref$uw_g$west$income$var, tolerance = tol$var)
})

test_that("w_skew: unweighted, grouped matches SPSS", {
  data(survey_data)
  result <- survey_data %>%
    dplyr::group_by(region) %>%
    w_skew(age, income)

  east <- result$results[result$results$region == "East", ]
  west <- result$results[result$results$region == "West", ]

  expect_equal(east$skew[east$Variable == "age"], spss_ref$uw_g$east$age$skew, tolerance = tol$skew)
  expect_equal(east$skew[east$Variable == "income"], spss_ref$uw_g$east$income$skew, tolerance = tol$skew)
  expect_equal(west$skew[west$Variable == "age"], spss_ref$uw_g$west$age$skew, tolerance = tol$skew)
  expect_equal(west$skew[west$Variable == "income"], spss_ref$uw_g$west$income$skew, tolerance = tol$skew)
})

test_that("w_kurtosis: unweighted, grouped matches SPSS", {
  data(survey_data)
  result <- survey_data %>%
    dplyr::group_by(region) %>%
    w_kurtosis(age, income, excess = TRUE)

  east <- result$results[result$results$region == "East", ]
  west <- result$results[result$results$region == "West", ]

  expect_equal(east$kurtosis[east$Variable == "age"], spss_ref$uw_g$east$age$kurtosis, tolerance = tol$kurtosis)
  expect_equal(east$kurtosis[east$Variable == "income"], spss_ref$uw_g$east$income$kurtosis, tolerance = tol$kurtosis)
  expect_equal(west$kurtosis[west$Variable == "age"], spss_ref$uw_g$west$age$kurtosis, tolerance = tol$kurtosis)
  expect_equal(west$kurtosis[west$Variable == "income"], spss_ref$uw_g$west$income$kurtosis, tolerance = tol$kurtosis)
})

# ==============================================================================
# SCENARIO 4: WEIGHTED, GROUPED BY REGION
# ==============================================================================

test_that("w_mean: weighted, grouped matches SPSS", {
  data(survey_data)
  result <- survey_data %>%
    dplyr::group_by(region) %>%
    w_mean(age, income, weights = sampling_weight)

  east <- result$results[result$results$region == "East", ]
  west <- result$results[result$results$region == "West", ]

  expect_equal(east$weighted_mean[east$Variable == "age"], spss_ref$w_g$east$age$mean, tolerance = tol$mean)
  expect_equal(east$weighted_mean[east$Variable == "income"], spss_ref$w_g$east$income$mean, tolerance = tol$mean)
  expect_equal(west$weighted_mean[west$Variable == "age"], spss_ref$w_g$west$age$mean, tolerance = tol$mean)
  expect_equal(west$weighted_mean[west$Variable == "income"], spss_ref$w_g$west$income$mean, tolerance = tol$mean)
})

test_that("w_sd: weighted, grouped matches SPSS", {
  data(survey_data)
  result <- survey_data %>%
    dplyr::group_by(region) %>%
    w_sd(age, income, weights = sampling_weight)

  east <- result$results[result$results$region == "East", ]
  west <- result$results[result$results$region == "West", ]

  expect_equal(east$weighted_sd[east$Variable == "age"], spss_ref$w_g$east$age$sd, tolerance = tol$sd)
  expect_equal(east$weighted_sd[east$Variable == "income"], spss_ref$w_g$east$income$sd, tolerance = tol$sd)
  expect_equal(west$weighted_sd[west$Variable == "age"], spss_ref$w_g$west$age$sd, tolerance = tol$sd)
  expect_equal(west$weighted_sd[west$Variable == "income"], spss_ref$w_g$west$income$sd, tolerance = tol$sd)
})

test_that("w_var: weighted, grouped matches SPSS", {
  data(survey_data)
  result <- survey_data %>%
    dplyr::group_by(region) %>%
    w_var(age, income, weights = sampling_weight)

  east <- result$results[result$results$region == "East", ]
  west <- result$results[result$results$region == "West", ]

  expect_equal(east$weighted_var[east$Variable == "age"], spss_ref$w_g$east$age$var, tolerance = tol$var)
  expect_equal(east$weighted_var[east$Variable == "income"], spss_ref$w_g$east$income$var, tolerance = tol$var)
  expect_equal(west$weighted_var[west$Variable == "age"], spss_ref$w_g$west$age$var, tolerance = tol$var)
  expect_equal(west$weighted_var[west$Variable == "income"], spss_ref$w_g$west$income$var, tolerance = tol$var)
})

test_that("w_skew: weighted, grouped matches SPSS", {
  data(survey_data)
  result <- survey_data %>%
    dplyr::group_by(region) %>%
    w_skew(age, income, weights = sampling_weight)

  east <- result$results[result$results$region == "East", ]
  west <- result$results[result$results$region == "West", ]

  expect_equal(east$weighted_skew[east$Variable == "age"], spss_ref$w_g$east$age$skew, tolerance = tol$skew)
  expect_equal(east$weighted_skew[east$Variable == "income"], spss_ref$w_g$east$income$skew, tolerance = tol$skew)
  expect_equal(west$weighted_skew[west$Variable == "age"], spss_ref$w_g$west$age$skew, tolerance = tol$skew)
  expect_equal(west$weighted_skew[west$Variable == "income"], spss_ref$w_g$west$income$skew, tolerance = tol$skew)
})

test_that("w_kurtosis: weighted, grouped matches SPSS", {
  data(survey_data)
  result <- survey_data %>%
    dplyr::group_by(region) %>%
    w_kurtosis(age, income, weights = sampling_weight, excess = TRUE)

  east <- result$results[result$results$region == "East", ]
  west <- result$results[result$results$region == "West", ]

  expect_equal(east$weighted_kurtosis[east$Variable == "age"], spss_ref$w_g$east$age$kurtosis, tolerance = tol$kurtosis)
  expect_equal(east$weighted_kurtosis[east$Variable == "income"], spss_ref$w_g$east$income$kurtosis, tolerance = tol$kurtosis)
  expect_equal(west$weighted_kurtosis[west$Variable == "age"], spss_ref$w_g$west$age$kurtosis, tolerance = tol$kurtosis)
  expect_equal(west$weighted_kurtosis[west$Variable == "income"], spss_ref$w_g$west$income$kurtosis, tolerance = tol$kurtosis)
})
