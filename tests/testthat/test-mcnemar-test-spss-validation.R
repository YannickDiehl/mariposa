# =============================================================================
# mcnemar_test — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::mcnemar_test() against SPSS v29 CROSSTABS
#          /STATISTICS=MCNEMAR.
# Reference output: tests/spss_reference/outputs/mcnemar_test_output.txt
#
# SPSS uses derived binary variables (trust_*_high = trust_* >= 4).
# mariposa exposes: chi_squared, p_value (asymptotic), exact_p, N, b, c and
# the 2x2 table ($tables). SPSS prints the exact binomial p only.
#
# The weighted runs are CROSSTABS with WEIGHT BY (not NPAR TESTS /MCNEMAR,
# whose weighted semantics are still pending): /COUNT ROUND CELL rounds each
# cell first and the McNemar test uses the rounded discordant cells. Weighted
# counts are asserted at Display precision 0 (Charter §5).
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# `table`: the SPSS Count table read row by row, margins included:
#   a, b, row total 1, c, d, row total 2, column total 1, column total 2, N
spss_values <- list(
  # ---- Test 1: trust_gov_high vs trust_media_high ----
  test_1_gov_media = list(
    n = 2227L, b = 338L, c = 448L,
    table = c(1342, 338, 1680,     # mcnemar_test_output.txt:38
              448,  99,  547,      # mcnemar_test_output.txt:39
              1790, 437, 2227),    # mcnemar_test_output.txt:40
    exact_p = "<.001"      # mcnemar_test_output.txt:45
  ),
  # ---- Test 2: trust_gov_high vs trust_sci_high ----
  test_2_gov_sci = list(
    n = 2255L, b = 1016L, c = 203L,
    table = c(675, 1016, 1691,     # mcnemar_test_output.txt:86
              203, 361,  564,      # mcnemar_test_output.txt:87
              878, 1377, 2255),    # mcnemar_test_output.txt:88
    exact_p = "<.001"      # mcnemar_test_output.txt:93
  ),
  # ---- Test 3: trust_media_high vs trust_sci_high ----
  test_3_media_sci = list(
    n = 2272L, b = 1128L, c = 169L,
    table = c(701, 1128, 1829,     # mcnemar_test_output.txt:134
              169, 274,  443,      # mcnemar_test_output.txt:135
              870, 1402, 2272),    # mcnemar_test_output.txt:136
    exact_p = "<.001"      # mcnemar_test_output.txt:141
  ),

  # ---- Weighted (WEIGHT BY sampling_weight), ungrouped ----
  weighted_gov_media = list(
    table = c(1351, 342, 1693,     # mcnemar_test_output.txt:182
              449,  102, 551,      # mcnemar_test_output.txt:183
              1800, 444, 2244),    # mcnemar_test_output.txt:184
    b = 342L,                      # mcnemar_test_output.txt:182
    c = 449L,                      # mcnemar_test_output.txt:183
    exact_p = "<.001",             # mcnemar_test_output.txt:189
    n = 2244L                      # mcnemar_test_output.txt:190
  ),
  weighted_gov_sci = list(
    table = c(681, 1024, 1705,     # mcnemar_test_output.txt:230
              203, 364,  567,      # mcnemar_test_output.txt:231
              884, 1388, 2272),    # mcnemar_test_output.txt:232
    b = 1024L,                     # mcnemar_test_output.txt:230
    c = 203L,                      # mcnemar_test_output.txt:231
    exact_p = "<.001",             # mcnemar_test_output.txt:237
    n = 2272L                      # mcnemar_test_output.txt:238
  ),

  # ---- Unweighted, SPLIT FILE region ----
  grouped_gov_media = list(
    East = list(
      table = c(264, 60, 324,      # mcnemar_test_output.txt:279
                97,  14, 111,      # mcnemar_test_output.txt:280
                361, 74, 435),     # mcnemar_test_output.txt:281
      b = 60L,                     # mcnemar_test_output.txt:279
      c = 97L,                     # mcnemar_test_output.txt:280
      exact_p = 0.004,             # mcnemar_test_output.txt:289
      n = 435L                     # mcnemar_test_output.txt:290
    ),
    West = list(
      table = c(1078, 278, 1356,   # mcnemar_test_output.txt:282
                351,  85,  436,    # mcnemar_test_output.txt:283
                1429, 363, 1792),  # mcnemar_test_output.txt:284
      b = 278L,                    # mcnemar_test_output.txt:282
      c = 351L,                    # mcnemar_test_output.txt:283
      exact_p = 0.004,             # mcnemar_test_output.txt:291
      n = 1792L                    # mcnemar_test_output.txt:292
    )
  ),
  grouped_gov_sci = list(
    East = list(
      table = c(130, 202, 332,     # mcnemar_test_output.txt:333
                44,  68,  112,     # mcnemar_test_output.txt:334
                174, 270, 444),    # mcnemar_test_output.txt:335
      b = 202L,                    # mcnemar_test_output.txt:333
      c = 44L,                     # mcnemar_test_output.txt:334
      exact_p = "<.001",           # mcnemar_test_output.txt:343
      n = 444L                     # mcnemar_test_output.txt:344
    ),
    West = list(
      table = c(545, 814,  1359,   # mcnemar_test_output.txt:336
                159, 293,  452,    # mcnemar_test_output.txt:337
                704, 1107, 1811),  # mcnemar_test_output.txt:338
      b = 814L,                    # mcnemar_test_output.txt:336
      c = 159L,                    # mcnemar_test_output.txt:337
      exact_p = "<.001",           # mcnemar_test_output.txt:345
      n = 1811L                    # mcnemar_test_output.txt:346
    )
  ),

  # ---- Weighted, SPLIT FILE region ----
  weighted_grouped_gov_media = list(
    East = list(
      table = c(278, 64, 342,      # mcnemar_test_output.txt:387
                101, 15, 116,      # mcnemar_test_output.txt:388
                379, 79, 458),     # mcnemar_test_output.txt:389
      b = 64L,                     # mcnemar_test_output.txt:387
      c = 101L,                    # mcnemar_test_output.txt:388
      exact_p = 0.005,             # mcnemar_test_output.txt:397
      n = 458L                     # mcnemar_test_output.txt:398
    ),
    West = list(
      table = c(1073, 278, 1351,   # mcnemar_test_output.txt:390
                348,  87,  435,    # mcnemar_test_output.txt:391
                1421, 365, 1786),  # mcnemar_test_output.txt:392
      b = 278L,                    # mcnemar_test_output.txt:390
      c = 348L,                    # mcnemar_test_output.txt:391
      exact_p = 0.006,             # mcnemar_test_output.txt:399
      n = 1786L                    # mcnemar_test_output.txt:400
    )
  )
)


data(survey_data, envir = environment())

# Derive binary variables matching SPSS labels (>=4)
survey_data$trust_gov_high   <- as.integer(survey_data$trust_government >= 4)
survey_data$trust_media_high <- as.integer(survey_data$trust_media     >= 4)
survey_data$trust_sci_high   <- as.integer(survey_data$trust_science   >= 4)


compare_mcnemar <- function(row, spss, scenario) {
  assert_spss_count(as.numeric(row$n), spss$n,
                    label = sprintf("[%s] N", scenario))
  assert_spss_count(as.numeric(row$b), spss$b,
                    label = sprintf("[%s] b (discordant Low→High)", scenario))
  assert_spss_count(as.numeric(row$c), spss$c,
                    label = sprintf("[%s] c (discordant High→Low)", scenario))
  assert_spss(as.numeric(row$exact_p), spss$exact_p,
              tier = "display", precision = 3, what = "p_value",
              label = sprintf("[%s] exact p (2-sided)", scenario))
}

# The 2x2 Count table SPSS prints (cells + margins), from mcnemar_test()'s
# own table; `weighted` asserts the rounded weighted counts at Display(0).
compare_mcnemar_table <- function(tab, spss_table, scenario, weighted = FALSE) {
  tab <- unclass(tab)
  got <- c(tab[1, ], sum(tab[1, ]), tab[2, ], sum(tab[2, ]),
           colSums(tab), sum(tab))
  cells <- c("Low x Low", "Low x High", "Low x Total",
             "High x Low", "High x High", "High x Total",
             "Total x Low", "Total x High", "Total x Total")
  expect_equal(length(got), length(spss_table))
  for (i in seq_along(spss_table)) {
    lab <- sprintf("[%s] count %s", scenario, cells[i])
    if (weighted) {
      assert_spss(unname(got[i]), spss_table[i], tier = "display", precision = 0,
                  label = lab)
    } else {
      assert_spss_count(unname(got[i]), spss_table[i], label = lab)
    }
  }
}

# One mcnemar_test() result row (+ its table) against one SPSS block
compare_mcnemar_full <- function(row, tab, spss, scenario, weighted = FALSE) {
  if (weighted) {
    assert_spss(as.numeric(row$n), spss$n, tier = "display", precision = 0,
                label = sprintf("[%s] N", scenario))
    assert_spss(as.numeric(row$b), spss$b, tier = "display", precision = 0,
                label = sprintf("[%s] b (discordant Low→High)", scenario))
    assert_spss(as.numeric(row$c), spss$c, tier = "display", precision = 0,
                label = sprintf("[%s] c (discordant High→Low)", scenario))
    assert_spss(as.numeric(row$exact_p), spss$exact_p,
                tier = "display", precision = 3, what = "p_value",
                label = sprintf("[%s] exact p (2-sided)", scenario))
  } else {
    compare_mcnemar(row, spss, scenario)
  }
  compare_mcnemar_table(tab, spss$table, scenario, weighted)
}

compare_mcnemar_grouped <- function(r, spss, scenario, weighted = FALSE) {
  expect_identical(as.character(r$results$region), names(spss),
                   label = sprintf("[%s] split-file group order", scenario))
  for (i in seq_along(names(spss))) {
    g <- names(spss)[i]
    compare_mcnemar_full(r$results[i, ], r$tables[[i]], spss[[g]],
                         sprintf("%s %s", scenario, g), weighted)
  }
}


test_that("Test 1: McNemar trust_gov vs trust_media — matches SPSS", {
  r <- survey_data |> mcnemar_test(trust_gov_high, trust_media_high)
  compare_mcnemar(r$results, spss_values$test_1_gov_media, "1: gov vs media")
  compare_mcnemar_table(r$tables[[1]], spss_values$test_1_gov_media$table,
                        "1: gov vs media")
})

test_that("Test 2: McNemar trust_gov vs trust_sci — matches SPSS", {
  r <- survey_data |> mcnemar_test(trust_gov_high, trust_sci_high)
  compare_mcnemar(r$results, spss_values$test_2_gov_sci, "2: gov vs sci")
  compare_mcnemar_table(r$tables[[1]], spss_values$test_2_gov_sci$table,
                        "2: gov vs sci")
})

test_that("Test 3: McNemar trust_media vs trust_sci — matches SPSS", {
  r <- survey_data |> mcnemar_test(trust_media_high, trust_sci_high)
  compare_mcnemar(r$results, spss_values$test_3_media_sci, "3: media vs sci")
  compare_mcnemar_table(r$tables[[1]], spss_values$test_3_media_sci$table,
                        "3: media vs sci")
})

test_that("Weighted: McNemar trust_gov vs trust_media (CROSSTABS, WEIGHT BY) — matches SPSS", {
  r <- survey_data |>
    mcnemar_test(trust_gov_high, trust_media_high, weights = sampling_weight)
  compare_mcnemar_full(r$results, r$tables[[1]], spss_values$weighted_gov_media,
                       "W: gov vs media", weighted = TRUE)
})

test_that("Weighted: McNemar trust_gov vs trust_sci (CROSSTABS, WEIGHT BY) — matches SPSS", {
  r <- survey_data |>
    mcnemar_test(trust_gov_high, trust_sci_high, weights = sampling_weight)
  compare_mcnemar_full(r$results, r$tables[[1]], spss_values$weighted_gov_sci,
                       "W: gov vs sci", weighted = TRUE)
})

test_that("Grouped: McNemar trust_gov vs trust_media by region — matches SPSS", {
  r <- survey_data |> group_by(region) |>
    mcnemar_test(trust_gov_high, trust_media_high)
  compare_mcnemar_grouped(r, spss_values$grouped_gov_media, "G: gov vs media")
})

test_that("Grouped: McNemar trust_gov vs trust_sci by region — matches SPSS", {
  r <- survey_data |> group_by(region) |>
    mcnemar_test(trust_gov_high, trust_sci_high)
  compare_mcnemar_grouped(r, spss_values$grouped_gov_sci, "G: gov vs sci")
})

test_that("Weighted grouped: McNemar trust_gov vs trust_media by region — matches SPSS", {
  r <- survey_data |> group_by(region) |>
    mcnemar_test(trust_gov_high, trust_media_high, weights = sampling_weight)
  compare_mcnemar_grouped(r, spss_values$weighted_grouped_gov_media,
                          "WG: gov vs media", weighted = TRUE)
})
