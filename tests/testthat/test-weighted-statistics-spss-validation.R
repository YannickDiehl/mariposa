# =============================================================================
# weighted_statistics — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::w_mean, w_sd, w_var, w_median, w_modus,
#          w_quantile, w_iqr, w_range, w_se, w_skew, w_kurtosis against
#          SPSS v29 DESCRIPTIVES / FREQUENCIES.
# Reference: weighted_statistics_output.txt
#
# Each scenario ran DESCRIPTIVES (N, Range, Minimum, Maximum, Mean, SE, SD,
# Variance, Skewness, Kurtosis) and FREQUENCIES (N, Missing, Median, Mode,
# Percentiles 25/50/75) on age and income. Every statistic is asserted
# against the w_* function that returns it, in all four scenarios:
#   1 unweighted/ungrouped, 2 weighted/ungrouped,
#   3 unweighted/grouped by region, 4 weighted/grouped by region.
# N and Missing are asserted on every w_* result. The standard errors of
# skewness and kurtosis are not returned by any w_* function (not asserted).
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# Per variable: DESCRIPTIVES row (Tests 1a-4a) and FREQUENCIES column
# (Tests 1b-4b). N is identical in both tables.
spss_values <- list(

  # ---- Scenario 1: Unweighted / Ungrouped ---------------------------------
  test_1_unweighted = list(
    all = list(
      age = c(
        # weighted_statistics_output.txt:14 (DESCRIPTIVES, Test 1a)
        n = 2500, range = 77.00, min = 18.00, max = 95.00, mean = 50.5496,
        se = 0.33952, sd = 16.97602, var = 288.185, skew = 0.172, kurt = -0.364,
        # weighted_statistics_output.txt:32-38 (FREQUENCIES, Test 1b)
        missing = 0, median = 50.0000, mode = 18.00,
        q25 = 38.0000, q50 = 50.0000, q75 = 62.0000
      ),
      income = c(
        # weighted_statistics_output.txt:16-17 (DESCRIPTIVES, Test 1a;
        # SD and Variance wrap onto line 17)
        n = 2186, range = 7200.00, min = 800.00, max = 8000.00,
        mean = 3753.9341, se = 30.64510, sd = 1432.80161, var = 2052920.442,
        skew = 0.730, kurt = 0.376,
        # weighted_statistics_output.txt:32-38 (FREQUENCIES, Test 1b)
        missing = 314, median = 3500.0000, mode = 3200.00,
        q25 = 2700.0000, q50 = 3500.0000, q75 = 4600.0000
      )
    )
  ),

  # ---- Scenario 2: Weighted / Ungrouped -----------------------------------
  test_2_weighted = list(
    all = list(
      age = c(
        # weighted_statistics_output.txt:53 (DESCRIPTIVES, Test 2a)
        n = 2516, range = 77.00, min = 18.00, max = 95.00, mean = 50.5144,
        se = 0.34058, sd = 17.08382, var = 291.857, skew = 0.159, kurt = -0.396,
        # weighted_statistics_output.txt:71-77 (FREQUENCIES, Test 2b)
        missing = 0, median = 50.0000, mode = 18.00,
        q25 = 38.0000, q50 = 50.0000, q75 = 63.0000
      ),
      income = c(
        # weighted_statistics_output.txt:55-56 (DESCRIPTIVES, Test 2a)
        n = 2201, range = 7200.00, min = 800.00, max = 8000.00,
        mean = 3743.0994, se = 30.35257, sd = 1423.96558, var = 2027677.966,
        skew = 0.725, kurt = 0.388,
        # weighted_statistics_output.txt:71-77 (FREQUENCIES, Test 2b)
        missing = 315, median = 3500.0000, mode = 3200.00,
        q25 = 2700.0000, q50 = 3500.0000, q75 = 4600.0000
      )
    )
  ),

  # ---- Scenario 3: Unweighted / Grouped by region -------------------------
  test_3_unweighted_grouped = list(
    East = list(
      age = c(
        # weighted_statistics_output.txt:92 (DESCRIPTIVES, Test 3a East)
        n = 485, range = 77.00, min = 18.00, max = 95.00, mean = 51.8680,
        se = 0.79101, sd = 17.42028, var = 303.466, skew = 0.148, kurt = -0.337,
        # weighted_statistics_output.txt:119-125 (FREQUENCIES, Test 3b East)
        missing = 0, median = 52.0000, mode = 18.00,
        q25 = 39.0000, q50 = 52.0000, q75 = 63.0000
      ),
      income = c(
        # weighted_statistics_output.txt:94-95 (DESCRIPTIVES, Test 3a East)
        n = 429, range = 7200.00, min = 800.00, max = 8000.00,
        mean = 3752.4476, se = 66.95917, sd = 1386.87938, var = 1923434.416,
        skew = 0.729, kurt = 0.489,
        # weighted_statistics_output.txt:119-125 (FREQUENCIES, Test 3b East)
        missing = 56, median = 3600.0000, mode = 3800.00,
        q25 = 2800.0000, q50 = 3600.0000, q75 = 4500.0000
      )
    ),
    West = list(
      age = c(
        # weighted_statistics_output.txt:101 (DESCRIPTIVES, Test 3a West)
        n = 2015, range = 77.00, min = 18.00, max = 95.00, mean = 50.2323,
        se = 0.37551, sd = 16.85635, var = 284.137, skew = 0.175, kurt = -0.373,
        # weighted_statistics_output.txt:126-132 (FREQUENCIES, Test 3b West)
        missing = 0, median = 49.0000, mode = 18.00,
        q25 = 38.0000, q50 = 49.0000, q75 = 62.0000
      ),
      income = c(
        # weighted_statistics_output.txt:103-104 (DESCRIPTIVES, Test 3a West)
        n = 1757, range = 7200.00, min = 800.00, max = 8000.00,
        mean = 3754.2971, se = 34.45361, sd = 1444.17770, var = 2085649.235,
        skew = 0.731, kurt = 0.353,
        # weighted_statistics_output.txt:126-132 (FREQUENCIES, Test 3b West)
        missing = 258, median = 3500.0000, mode = 3100.00,
        q25 = 2700.0000, q50 = 3500.0000, q75 = 4600.0000
      )
    )
  ),

  # ---- Scenario 4: Weighted / Grouped by region ---------------------------
  test_4_weighted_grouped = list(
    East = list(
      age = c(
        # weighted_statistics_output.txt:147 (DESCRIPTIVES, Test 4a East)
        n = 509, range = 77.00, min = 18.00, max = 95.00, mean = 52.2778,
        se = 0.77988, sd = 17.59548, var = 309.601, skew = 0.098, kurt = -0.389,
        # weighted_statistics_output.txt:174-180 (FREQUENCIES, Test 4b East)
        missing = 0, median = 53.0000, mode = 18.00,
        q25 = 40.0000, q50 = 53.0000, q75 = 64.0000
      ),
      income = c(
        # weighted_statistics_output.txt:149-150 (DESCRIPTIVES, Test 4a East)
        n = 449, range = 7200.00, min = 800.00, max = 8000.00,
        mean = 3760.6866, se = 65.48281, sd = 1388.32120, var = 1927435.755,
        skew = 0.721, kurt = 0.502,
        # weighted_statistics_output.txt:174-180 (FREQUENCIES, Test 4b East)
        missing = 60, median = 3600.0000, mode = 3800.00,
        q25 = 2800.0000, q50 = 3600.0000, q75 = 4500.0000
      )
    ),
    West = list(
      age = c(
        # weighted_statistics_output.txt:156 (DESCRIPTIVES, Test 4a West)
        n = 2007, range = 77.00, min = 18.00, max = 95.00, mean = 50.0672,
        se = 0.37783, sd = 16.92689, var = 286.520, skew = 0.170, kurt = -0.396,
        # weighted_statistics_output.txt:181-187 (FREQUENCIES, Test 4b West)
        missing = 0, median = 49.0000, mode = 18.00,
        q25 = 38.0000, q50 = 49.0000, q75 = 62.0000
      ),
      income = c(
        # weighted_statistics_output.txt:158-159 (DESCRIPTIVES, Test 4a West)
        n = 1751, range = 7200.00, min = 800.00, max = 8000.00,
        mean = 3738.5858, se = 34.24889, sd = 1433.32495, var = 2054420.399,
        skew = 0.727, kurt = 0.364,
        # weighted_statistics_output.txt:181-187 (FREQUENCIES, Test 4b West)
        missing = 256, median = 3500.0000, mode = 3100.00,
        q25 = 2700.0000, q50 = 3500.0000, q75 = 4600.0000
      )
    )
  )
)


# Decimals SPSS prints per statistic (identical for age and income in every
# scenario). The IQR is not printed; its reference is P75 - P25 of the
# printed percentiles (4 dp each).
w_precision <- c(
  range = 2, min = 2, max = 2, mean = 4, se = 5, sd = 5, var = 3,
  skew = 3, kurt = 3, median = 4, mode = 2, q25 = 4, q50 = 4, q75 = 4,
  iqr = 4
)

# Long-format w_* functions: SPSS statistic -> (function, result column).
# With weights the column carries a "weighted_" prefix.
w_long <- list(
  mean   = c("w_mean",     "mean"),
  se     = c("w_se",       "se"),
  sd     = c("w_sd",       "sd"),
  var    = c("w_var",      "var"),
  skew   = c("w_skew",     "skew"),
  kurt   = c("w_kurtosis", "kurtosis"),
  range  = c("w_range",    "range"),
  median = c("w_median",   "median"),
  mode   = c("w_modus",    "mode"),
  iqr    = c("w_iqr",      "iqr")
)

# w_quantile returns one wide row per group: <var>_Min, <var>_25%, ...
w_quantile_cols <- c(min = "Min", q25 = "25%", q50 = "50%", q75 = "75%",
                     max = "Max")

w_fns <- list(
  w_mean = w_mean, w_se = w_se, w_sd = w_sd, w_var = w_var,
  w_skew = w_skew, w_kurtosis = w_kurtosis, w_range = w_range,
  w_median = w_median, w_modus = w_modus, w_iqr = w_iqr,
  w_quantile = w_quantile
)


data(survey_data, envir = environment())


# N and Missing: counts without weights (Spec); with weights sums of
# weights that SPSS prints rounded (Display, 0 dp).
assert_w_counts <- function(n, missing, spss, weighted, label) {
  if (weighted) {
    assert_spss(as.numeric(n), spss[["n"]], tier = "display", precision = 0,
                label = sprintf("%s N (weighted)", label))
    assert_spss(as.numeric(missing), spss[["missing"]], tier = "display",
                precision = 0, label = sprintf("%s Missing (weighted)", label))
  } else {
    assert_spss_count(as.numeric(n), spss[["n"]],
                      label = sprintf("%s N", label))
    assert_spss_count(as.numeric(missing), spss[["missing"]],
                      label = sprintf("%s Missing", label))
  }
}

# Run all eleven w_* functions for one scenario and compare every cell.
check_w_scenario <- function(spss, weighted, grouped, scenario) {
  d <- if (grouped) group_by(survey_data, region) else survey_data
  results <- lapply(w_fns, function(f) {
    r <- if (weighted) f(d, age, income, weights = sampling_weight)
         else f(d, age, income)
    as.data.frame(r$results)
  })
  pfx <- if (weighted) "weighted_" else ""

  for (grp in names(spss)) {
    for (var in names(spss[[grp]])) {
      exp <- spss[[grp]][[var]]
      exp[["iqr"]] <- exp[["q75"]] - exp[["q25"]]
      cell <- sprintf("%s%s | %s", scenario,
                      if (grouped) sprintf(" [%s]", grp) else "", var)

      # Long-format functions: one row per (group, variable)
      for (stat in names(w_long)) {
        fn  <- w_long[[stat]][1]
        res <- results[[fn]]
        sel <- res$Variable == var
        if (grouped) sel <- sel & as.character(res$region) == grp
        row <- res[sel, , drop = FALSE]
        expect_equal(nrow(row), 1L, label = sprintf("[%s] %s rows", cell, fn))
        assert_spss(as.numeric(row[[paste0(pfx, w_long[[stat]][2])]]),
                    exp[[stat]], tier = "display",
                    precision = w_precision[[stat]],
                    label = sprintf("[%s] %s", cell, fn))
        assert_w_counts(row[[if (weighted) "weighted_n" else "n"]],
                        row[["missing"]], exp, weighted,
                        sprintf("[%s] %s", cell, fn))
      }

      # w_quantile: wide format, one row per group
      qres <- results$w_quantile
      qrow <- if (grouped) qres[as.character(qres$region) == grp, , drop = FALSE]
              else qres
      expect_equal(nrow(qrow), 1L, label = sprintf("[%s] w_quantile rows", cell))
      for (stat in names(w_quantile_cols)) {
        assert_spss(as.numeric(qrow[[paste0(var, "_", w_quantile_cols[[stat]])]]),
                    exp[[stat]], tier = "display",
                    precision = w_precision[[stat]],
                    label = sprintf("[%s] w_quantile %s", cell,
                                    w_quantile_cols[[stat]]))
      }
      assert_w_counts(qrow[[paste0(var, if (weighted) "_weighted_n" else "_n")]],
                      qrow[[paste0(var, "_missing")]], exp, weighted,
                      sprintf("[%s] w_quantile", cell))
    }
  }
}


test_that("Scenario 1: w_* statistics unweighted — match SPSS", {
  check_w_scenario(spss_values$test_1_unweighted,
                   weighted = FALSE, grouped = FALSE, scenario = "1 unweighted")
})

test_that("Scenario 2: w_* statistics weighted — match SPSS", {
  check_w_scenario(spss_values$test_2_weighted,
                   weighted = TRUE, grouped = FALSE, scenario = "2 weighted")
})

test_that("Scenario 3: w_* statistics unweighted, grouped by region — match SPSS", {
  check_w_scenario(spss_values$test_3_unweighted_grouped,
                   weighted = FALSE, grouped = TRUE, scenario = "3 grouped")
})

test_that("Scenario 4: w_* statistics weighted, grouped by region — match SPSS", {
  check_w_scenario(spss_values$test_4_weighted_grouped,
                   weighted = TRUE, grouped = TRUE,
                   scenario = "4 weighted grouped")
})
