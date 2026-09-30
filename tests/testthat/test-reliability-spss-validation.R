# =============================================================================
# reliability — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::reliability() (Cronbach's alpha) against SPSS v29
#          RELIABILITY procedure.
# Reference: reliability_output.txt
#
# Every table SPSS prints is asserted, in all four scenarios:
#   Reliability Statistics  alpha, standardized alpha, N of items, and the
#                           negative-alpha footnote (a) -> $negative_alpha
#   Item Statistics         mean, SD, N per item (weighted N at 2 dp)
#   Inter-Item Correlations upper triangle of the matrix
#   Item-Total Statistics   scale mean / variance if item deleted, corrected
#                           item-total r, alpha if item deleted
# The Squared Multiple Correlation column is not computed by mariposa (not
# asserted).
#
# Scales: trust (Tests 1a/2a/3a/4a), attitude (1b/2b/3b/4b), full 6-item
# scale (1c/2c, ungrouped only).
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# Each cell: item_stats rows are the items in scale order (Mean | SD);
# inter_item is the upper triangle of the correlation matrix row by row
# ((1,2), (1,3), ..., (2,3), ...); item_total columns are Scale Mean if
# Item Deleted | Scale Variance if Item Deleted | Corrected Item-Total
# Correlation | Cronbach's Alpha if Item Deleted.
spss_values <- list(

  # ---- Scenario 1: Unweighted / Ungrouped ---------------------------------
  test_1a_trust = list(
    # reliability_output.txt:10
    alpha = 0.047, alpha_std = 0.048, n_items = 3L, negative_alpha = FALSE,
    # reliability_output.txt:15-17
    n = 2135L,
    item_stats = rbind(trust_government = c(2.62, 1.162),
                       trust_media      = c(2.43, 1.156),
                       trust_science    = c(3.62, 1.034)),
    # reliability_output.txt:23-26
    inter_item = c(0.014, 0.020, 0.015),
    # reliability_output.txt:33-37
    item_total = rbind(trust_government = c(6.05, 2.440, 0.024, 0.029),
                       trust_media      = c(6.25, 2.467, 0.020, 0.040),
                       trust_science    = c(5.05, 2.723, 0.025, 0.027))
  ),
  test_1b_attitude = list(
    # reliability_output.txt:47-48 (footnote a: negative alpha)
    alpha = -0.929, alpha_std = -0.958, n_items = 3L, negative_alpha = TRUE,
    # reliability_output.txt:53-55
    n = 2139L,
    item_stats = rbind(life_satisfaction     = c(3.62, 1.162),
                       environmental_concern = c(3.58, 1.189),
                       political_orientation = c(2.72, 1.084)),
    # reliability_output.txt:61-64
    inter_item = c(0.002, 0.002, -0.588),
    # reliability_output.txt:71-76
    item_total = rbind(life_satisfaction     = c(6.30, 1.072,  0.004, -2.829),
                       environmental_concern = c(6.34, 2.529, -0.399,  0.003),
                       political_orientation = c(7.20, 2.769, -0.419,  0.004))
  ),
  test_1c_full = list(
    # reliability_output.txt:87-88 (footnote a: negative alpha)
    alpha = -0.208, alpha_std = -0.204, n_items = 6L, negative_alpha = TRUE,
    # reliability_output.txt:93-98
    n = 1826L,
    item_stats = rbind(trust_government      = c(2.62, 1.162),
                       trust_media           = c(2.43, 1.147),
                       trust_science         = c(3.60, 1.039),
                       life_satisfaction     = c(3.65, 1.154),
                       environmental_concern = c(3.58, 1.202),
                       political_orientation = c(2.72, 1.088)),
    # reliability_output.txt:104-113
    inter_item = c(0.022, 0.024, 0.012, 0.064, -0.060,
                   0.017, 0.044, 0.005, 0.011,
                   -0.016, -0.033, 0.050,
                   0.025, -0.006,
                   -0.594),
    # reliability_output.txt:120-131
    item_total = rbind(trust_government      = c(15.98, 5.042,  0.034, -0.326),
                       trust_media           = c(16.16, 4.998,  0.050, -0.348),
                       trust_science         = c(15.00, 5.403,  0.018, -0.283),
                       life_satisfaction     = c(14.95, 5.076,  0.031, -0.320),
                       environmental_concern = c(15.02, 6.503, -0.225,  0.046),
                       political_orientation = c(15.88, 6.967, -0.275,  0.080))
  ),

  # ---- Scenario 2: Weighted / Ungrouped -----------------------------------
  # SPSS honors the weights; N is the sum of weights, printed with 2 dp.
  test_2a_trust_weighted = list(
    # reliability_output.txt:144
    alpha = 0.052, alpha_std = 0.053, n_items = 3L, negative_alpha = FALSE,
    # reliability_output.txt:149-151
    n = 2150.20,
    item_stats = rbind(trust_government = c(2.62, 1.162),
                       trust_media      = c(2.43, 1.158),
                       trust_science    = c(3.62, 1.033)),
    # reliability_output.txt:157-160
    inter_item = c(0.017, 0.021, 0.017),
    # reliability_output.txt:167-171
    item_total = rbind(trust_government = c(6.06, 2.451, 0.026, 0.033),
                       trust_media      = c(6.25, 2.469, 0.023, 0.041),
                       trust_science    = c(5.05, 2.737, 0.027, 0.033))
  ),
  test_2b_attitude_weighted = list(
    # reliability_output.txt:181-182 (footnote a: negative alpha)
    alpha = -0.929, alpha_std = -0.957, n_items = 3L, negative_alpha = TRUE,
    # reliability_output.txt:187-189
    n = 2151.88,
    item_stats = rbind(life_satisfaction     = c(3.62, 1.160),
                       environmental_concern = c(3.58, 1.187),
                       political_orientation = c(2.73, 1.085)),
    # reliability_output.txt:195-198
    inter_item = c(0.002, -0.001, -0.586),
    # reliability_output.txt:205-210
    item_total = rbind(life_satisfaction     = c(6.31, 1.077,  0.002, -2.799),
                       environmental_concern = c(6.34, 2.520, -0.398, -0.002),
                       political_orientation = c(7.20, 2.762, -0.419,  0.005))
  ),
  test_2c_full_weighted = list(
    # reliability_output.txt:221-222 (footnote a: negative alpha)
    alpha = -0.203, alpha_std = -0.200, n_items = 6L, negative_alpha = TRUE,
    # reliability_output.txt:227-232
    n = 1837.79,
    item_stats = rbind(trust_government      = c(2.62, 1.162),
                       trust_media           = c(2.44, 1.150),
                       trust_science         = c(3.60, 1.040),
                       life_satisfaction     = c(3.65, 1.153),
                       environmental_concern = c(3.58, 1.198),
                       political_orientation = c(2.72, 1.088)),
    # reliability_output.txt:238-247
    inter_item = c(0.024, 0.025, 0.017, 0.067, -0.061,
                   0.020, 0.043, 0.002, 0.013,
                   -0.021, -0.036, 0.054,
                   0.025, -0.010,
                   -0.591),
    # reliability_output.txt:254-265
    item_total = rbind(trust_government      = c(15.99, 5.037,  0.039, -0.326),
                       trust_media           = c(16.17, 5.006,  0.051, -0.343),
                       trust_science         = c(15.01, 5.417,  0.019, -0.278),
                       life_satisfaction     = c(14.96, 5.105,  0.030, -0.311),
                       environmental_concern = c(15.03, 6.525, -0.224,  0.050),
                       political_orientation = c(15.89, 6.973, -0.273,  0.081))
  ),

  # ---- Scenario 3: Unweighted / Grouped by region -------------------------
  test_3a_trust_grouped = list(
    East = list(
      # reliability_output.txt:278
      alpha = 0.037, alpha_std = 0.042, n_items = 3L, negative_alpha = FALSE,
      # reliability_output.txt:284-286
      n = 422L,
      item_stats = rbind(trust_government = c(2.62, 1.165),
                         trust_media      = c(2.40, 1.091),
                         trust_science    = c(3.66, 1.009)),
      # reliability_output.txt:295-299
      inter_item = c(-0.017, 0.001, 0.060),
      # reliability_output.txt:313-320
      item_total = rbind(trust_government = c(6.06, 2.339, -0.012,  0.112),
                         trust_media      = c(6.28, 2.378,  0.026,  0.002),
                         trust_science    = c(5.01, 2.503,  0.042, -0.035))
    ),
    West = list(
      # reliability_output.txt:279
      alpha = 0.050, alpha_std = 0.050, n_items = 3L, negative_alpha = FALSE,
      # reliability_output.txt:287-289
      n = 1713L,
      item_stats = rbind(trust_government = c(2.62, 1.161),
                         trust_media      = c(2.44, 1.172),
                         trust_science    = c(3.62, 1.040)),
      # reliability_output.txt:301-305
      inter_item = c(0.021, 0.025, 0.005),
      # reliability_output.txt:323-330
      item_total = rbind(trust_government = c(6.05, 2.467, 0.032, 0.010),
                         trust_media      = c(6.24, 2.491, 0.019, 0.049),
                         trust_science    = c(5.06, 2.779, 0.021, 0.041))
    )
  ),
  test_3b_attitude_grouped = list(
    East = list(
      # reliability_output.txt:342, 344 (footnote a: negative alpha)
      alpha = -0.855, alpha_std = -0.934, n_items = 3L, negative_alpha = TRUE,
      # reliability_output.txt:349-351
      n = 411L,
      item_stats = rbind(life_satisfaction     = c(3.61, 1.219),
                         environmental_concern = c(3.59, 1.189),
                         political_orientation = c(2.73, 1.088)),
      # reliability_output.txt:361-366
      inter_item = c(0.014, 0.016, -0.606),
      # reliability_output.txt:381-387
      item_total = rbind(life_satisfaction     = c(6.32, 1.028,  0.034, -3.049),
                         environmental_concern = c(6.34, 2.713, -0.390,  0.032),
                         political_orientation = c(7.20, 2.942, -0.409,  0.029))
    ),
    West = list(
      # reliability_output.txt:343-344 (footnote a: negative alpha)
      alpha = -0.948, alpha_std = -0.964, n_items = 3L, negative_alpha = TRUE,
      # reliability_output.txt:352-354
      n = 1728L,
      item_stats = rbind(life_satisfaction     = c(3.62, 1.148),
                         environmental_concern = c(3.58, 1.189),
                         political_orientation = c(2.72, 1.083)),
      # reliability_output.txt:368-373
      inter_item = c(-0.001, -0.002, -0.584),
      # reliability_output.txt:390-396
      item_total = rbind(life_satisfaction     = c(6.30, 1.083, -0.003, -2.780),
                         environmental_concern = c(6.34, 2.486, -0.402, -0.004),
                         political_orientation = c(7.20, 2.729, -0.422, -0.002))
    )
  ),

  # ---- Scenario 4: Weighted / Grouped by region ---------------------------
  test_4a_trust_weighted_grouped = list(
    East = list(
      # reliability_output.txt:410
      alpha = 0.061, alpha_std = 0.066, n_items = 3L, negative_alpha = FALSE,
      # reliability_output.txt:416-418
      n = 443.65,
      item_stats = rbind(trust_government = c(2.61, 1.166),
                         trust_media      = c(2.40, 1.092),
                         trust_science    = c(3.66, 1.007)),
      # reliability_output.txt:427-431
      inter_item = c(-0.014, 0.016, 0.068),
      # reliability_output.txt:445-452
      item_total = rbind(trust_government = c(6.06, 2.356, 0.000,  0.127),
                         trust_media      = c(6.27, 2.412, 0.034,  0.030),
                         trust_science    = c(5.01, 2.517, 0.058, -0.028))
    ),
    West = list(
      # reliability_output.txt:411
      alpha = 0.051, alpha_std = 0.050, n_items = 3L, negative_alpha = FALSE,
      # reliability_output.txt:419-421
      n = 1706.55,
      item_stats = rbind(trust_government = c(2.62, 1.161),
                         trust_media      = c(2.44, 1.175),
                         trust_science    = c(3.61, 1.040)),
      # reliability_output.txt:433-437
      inter_item = c(0.024, 0.023, 0.006),
      # reliability_output.txt:455-462
      item_total = rbind(trust_government = c(6.06, 2.477, 0.033, 0.011),
                         trust_media      = c(6.24, 2.485, 0.021, 0.044),
                         trust_science    = c(5.07, 2.795, 0.020, 0.047))
    )
  ),
  test_4b_attitude_weighted_grouped = list(
    East = list(
      # reliability_output.txt:474, 476 (footnote a: negative alpha)
      alpha = -0.882, alpha_std = -0.956, n_items = 3L, negative_alpha = TRUE,
      # reliability_output.txt:481-483
      n = 429.65,
      item_stats = rbind(life_satisfaction     = c(3.62, 1.214),
                         environmental_concern = c(3.59, 1.194),
                         political_orientation = c(2.74, 1.088)),
      # reliability_output.txt:493-498
      inter_item = c(0.012, 0.011, -0.606),
      # reliability_output.txt:513-519
      item_total = rbind(life_satisfaction     = c(6.33, 1.033,  0.025, -3.047),
                         environmental_concern = c(6.35, 2.686, -0.394,  0.021),
                         political_orientation = c(7.21, 2.934, -0.415,  0.024))
    ),
    West = list(
      # reliability_output.txt:475-476 (footnote a: negative alpha)
      alpha = -0.942, alpha_std = -0.958, n_items = 3L, negative_alpha = TRUE,
      # reliability_output.txt:484-486
      n = 1722.24,
      item_stats = rbind(life_satisfaction     = c(3.62, 1.147),
                         environmental_concern = c(3.58, 1.185),
                         political_orientation = c(2.73, 1.084)),
      # reliability_output.txt:500-505
      inter_item = c(0.000, -0.004, -0.580),
      # reliability_output.txt:522-528. SPSS prints the political_orientation
      # alpha if deleted as -2.13E-005: three significant digits, i.e. the
      # 7th decimal (precision override below).
      item_total = rbind(life_satisfaction     = c(6.31, 1.089, -0.004, -2.741),
                         environmental_concern = c(6.34, 2.480, -0.400, -0.009),
                         political_orientation = c(7.20, 2.721, -0.420, -2.13e-05)),
      alpha_if_deleted_precision = c(political_orientation = 7)
    )
  )
)


data(survey_data, envir = environment())

trust_items    <- c("trust_government", "trust_media", "trust_science")
attitude_items <- c("life_satisfaction", "environmental_concern",
                    "political_orientation")


# reliability() warns when alpha is negative (the SPSS footnote); that
# warning is expected here and asserted through $negative_alpha instead.
# McDonald's omega (not part of SPSS RELIABILITY, Tier 4) warns when its
# one-factor fit is a Heywood case, as in some regional subsamples; omega is
# not asserted here. Any other warning still surfaces.
reliability_spss <- function(...) {
  withCallingHandlers(
    reliability(...),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("alpha is negative", msg) ||
          grepl("omega is not computed", msg)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}


compare_alpha <- function(r, spss, scenario, is_weighted = FALSE) {
  lbl <- function(what) sprintf("[%s] %s", scenario, what)
  items <- rownames(spss$item_stats)

  # ---- Reliability Statistics ----
  assert_spss(as.numeric(r$alpha), spss$alpha,
              tier = "display", precision = 3,
              label = lbl("Cronbach's alpha"))
  assert_spss(as.numeric(r$alpha_standardized), spss$alpha_std,
              tier = "display", precision = 3,
              label = lbl("alpha standardized"))
  assert_spss_count(r$n_items, spss$n_items, label = lbl("N of items"))
  expect_identical(r$negative_alpha, spss$negative_alpha,
                   label = lbl("negative-alpha flag (SPSS footnote a)"))

  # ---- Item Statistics: N (count; weighted: sum of weights, 2 dp) ----
  assert_n <- function(actual, what) {
    if (is_weighted) {
      assert_spss(as.numeric(actual), spss$n, tier = "display",
                  precision = 2, label = lbl(paste(what, "(weighted)")))
    } else {
      assert_spss_count(as.numeric(actual), spss$n, label = lbl(what))
    }
  }
  assert_n(if (is_weighted) r$weighted_n else r$n, "N")

  is_ <- r$item_statistics
  expect_identical(is_$item, items, label = lbl("item order"))
  for (i in seq_along(items)) {
    assert_spss(is_$mean[i], spss$item_stats[i, 1],
                tier = "display", precision = 2,
                label = lbl(sprintf("%s item mean", items[i])))
    assert_spss(is_$sd[i], spss$item_stats[i, 2],
                tier = "display", precision = 3,
                label = lbl(sprintf("%s item SD", items[i])))
    assert_n(is_$n[i], sprintf("%s item N", items[i]))
  }

  # ---- Inter-Item Correlation Matrix (upper triangle) ----
  pairs <- utils::combn(items, 2)
  for (j in seq_len(ncol(pairs))) {
    assert_spss(r$inter_item_cor[pairs[1, j], pairs[2, j]], spss$inter_item[j],
                tier = "display", precision = 3,
                label = lbl(sprintf("r(%s, %s)", pairs[1, j], pairs[2, j])))
  }

  # ---- Item-Total Statistics ----
  it <- r$item_total
  expect_identical(it$item, items, label = lbl("item-total order"))
  cols <- c("scale_mean_if_deleted", "scale_var_if_deleted",
            "corrected_item_total_r", "alpha_if_deleted")
  prec <- c(2, 3, 3, 3)
  for (i in seq_along(items)) {
    for (k in seq_along(cols)) {
      p <- prec[k]
      if (cols[k] == "alpha_if_deleted" &&
          items[i] %in% names(spss$alpha_if_deleted_precision)) {
        p <- spss$alpha_if_deleted_precision[[items[i]]]
      }
      assert_spss(it[[cols[k]]][i], spss$item_total[i, k],
                  tier = "display", precision = p,
                  label = lbl(sprintf("%s %s", items[i], cols[k])))
    }
  }
}

# Grouped scenarios: SPSS prints East before West.
compare_alpha_grouped <- function(r, spss, scenario, is_weighted = FALSE) {
  regions <- vapply(r$groups, function(g) as.character(g$group_values$region),
                    character(1))
  expect_identical(regions, c("East", "West"),
                   label = sprintf("[%s] region order", scenario))
  for (i in seq_along(r$groups)) {
    compare_alpha(r$groups[[i]], spss[[regions[i]]],
                  sprintf("%s [%s]", scenario, regions[i]),
                  is_weighted = is_weighted)
  }
}


# =============================================================================
# SCENARIO 1 — UNWEIGHTED / UNGROUPED
# =============================================================================

test_that("Test 1a: reliability trust scale unweighted — matches SPSS", {
  r <- survey_data |>
    reliability(trust_government, trust_media, trust_science)
  compare_alpha(r, spss_values$test_1a_trust, "1a: trust scale")
})

test_that("Test 1b: reliability attitude scale unweighted — matches SPSS", {
  r <- survey_data |> reliability_spss(all_of(attitude_items))
  compare_alpha(r, spss_values$test_1b_attitude, "1b: attitude scale")
})

test_that("Test 1c: reliability full scale unweighted — matches SPSS", {
  r <- survey_data |> reliability_spss(all_of(c(trust_items, attitude_items)))
  compare_alpha(r, spss_values$test_1c_full, "1c: full scale")
})


# =============================================================================
# SCENARIO 2 — WEIGHTED / UNGROUPED
# =============================================================================

test_that("Test 2a: reliability trust scale weighted — matches SPSS", {
  r <- survey_data |>
    reliability(trust_government, trust_media, trust_science,
                weights = sampling_weight)
  compare_alpha(r, spss_values$test_2a_trust_weighted, "2a: weighted trust",
                is_weighted = TRUE)
})

test_that("Test 2b: reliability attitude scale weighted — matches SPSS", {
  r <- survey_data |>
    reliability_spss(all_of(attitude_items), weights = sampling_weight)
  compare_alpha(r, spss_values$test_2b_attitude_weighted,
                "2b: weighted attitude", is_weighted = TRUE)
})

test_that("Test 2c: reliability full scale weighted — matches SPSS", {
  r <- survey_data |>
    reliability_spss(all_of(c(trust_items, attitude_items)),
                     weights = sampling_weight)
  compare_alpha(r, spss_values$test_2c_full_weighted, "2c: weighted full",
                is_weighted = TRUE)
})


# =============================================================================
# SCENARIO 3 — UNWEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 3a: reliability trust scale grouped by region — matches SPSS", {
  r <- survey_data |> group_by(region) |>
    reliability_spss(all_of(trust_items))
  compare_alpha_grouped(r, spss_values$test_3a_trust_grouped, "3a: trust")
})

test_that("Test 3b: reliability attitude scale grouped by region — matches SPSS", {
  r <- survey_data |> group_by(region) |>
    reliability_spss(all_of(attitude_items))
  compare_alpha_grouped(r, spss_values$test_3b_attitude_grouped,
                        "3b: attitude")
})


# =============================================================================
# SCENARIO 4 — WEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 4a: reliability trust scale weighted, grouped — matches SPSS", {
  r <- survey_data |> group_by(region) |>
    reliability_spss(all_of(trust_items), weights = sampling_weight)
  compare_alpha_grouped(r, spss_values$test_4a_trust_weighted_grouped,
                        "4a: weighted trust", is_weighted = TRUE)
})

test_that("Test 4b: reliability attitude scale weighted, grouped — matches SPSS", {
  r <- survey_data |> group_by(region) |>
    reliability_spss(all_of(attitude_items), weights = sampling_weight)
  compare_alpha_grouped(r, spss_values$test_4b_attitude_weighted_grouped,
                        "4b: weighted attitude", is_weighted = TRUE)
})
