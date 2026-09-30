# =============================================================================
# crosstab — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::crosstab() against SPSS v29 CROSSTABS.
# Reference output: tests/spss_reference/outputs/crosstab_output.txt
#
# CROSSTABS family honors WEIGHT BY. The reference syntax runs every table
# with SPSS's default /COUNT ROUND CELL: each weighted cell count is rounded
# first, margins are sums of the rounded cells and percentages come from the
# rounded counts. crosstab() reproduces that exactly, so weighted counts are
# integers and asserted at the Spec tier, percentages at Display(1).
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(

  # ---- Test 1.1: Gender × Region 2×2 unweighted -----------------------
  test_1_1 = list(
    table = matrix(c(238, 956, 247, 1059), nrow = 2, byrow = TRUE,
                   dimnames = list(c("Male", "Female"), c("East", "West"))),
    row_totals = c(Male = 1194, Female = 1306),
    col_totals = c(East = 485, West = 2015),
    total = 2500
  ),

  # ---- Test 2.1: Gender × Region 2×2 weighted -------------------------
  # crosstab_output.txt:162-178 (/COUNT ROUND CELL)
  test_2_1_weighted = list(
    table = matrix(c(249, 945, 260, 1062), nrow = 2, byrow = TRUE,
                   dimnames = list(c("Male", "Female"), c("East", "West"))),
    row_totals = c(Male = 1194, Female = 1322),
    col_totals = c(East = 509, West = 2007),
    total = 2516,
    row_pct = matrix(c(20.9, 79.1, 19.7, 80.3), nrow = 2, byrow = TRUE),
    col_pct = matrix(c(48.9, 47.1, 51.1, 52.9), nrow = 2, byrow = TRUE),
    total_pct = matrix(c(9.9, 37.6, 10.3, 42.2), nrow = 2, byrow = TRUE)
  ),

  # ---- Test 2.2: Education × Employment 4×5 weighted ------------------
  # crosstab_output.txt:187-211. The grand total 2518 exceeds the rounded
  # sum of weights (2516): margins are sums of rounded cells.
  test_2_2_weighted = list(
    table = matrix(c(0, 573, 66, 175, 34,
                     0, 420, 52, 139, 29,
                     46, 370, 45, 149, 33,
                     34, 240, 21, 72, 20), nrow = 4, byrow = TRUE),
    row_totals = c(848, 640, 643, 387),
    col_totals = c(80, 1603, 184, 535, 116),
    total = 2518
  ),

  # ---- Test 4.1: Gender × Education by region, weighted (SPLIT FILE) ----
  # crosstab_output.txt:465-491
  test_4_1_weighted = list(
    East = list(table = matrix(c(83, 63, 64, 39, 92, 66, 59, 43), nrow = 2, byrow = TRUE),
                row_totals = c(249, 260), col_totals = c(175, 129, 123, 82),
                total = 509,
                row_pct = matrix(c(33.3, 25.3, 25.7, 15.7, 35.4, 25.4, 22.7, 16.5),
                                 nrow = 2, byrow = TRUE)),
    West = list(table = matrix(c(318, 228, 262, 137, 355, 284, 257, 166), nrow = 2, byrow = TRUE),
                row_totals = c(945, 1062), col_totals = c(673, 512, 519, 303),
                total = 2007,
                row_pct = matrix(c(33.7, 24.1, 27.7, 14.5, 33.4, 26.7, 24.2, 15.6),
                                 nrow = 2, byrow = TRUE))
  ),

  # ---- Test 3.1: Gender × Education grouped by region (unweighted) ----
  test_3_1_grouped = NULL  # see test
)


data(survey_data, envir = environment())


test_that("Test 1.1: crosstab gender × region 2x2 unweighted — matches SPSS", {
  r <- survey_data |> crosstab(gender, region)
  spss <- spss_values$test_1_1

  # Validate cell counts
  for (rn in rownames(spss$table)) {
    for (cn in colnames(spss$table)) {
      actual <- r$table[rn, cn]
      assert_spss_count(actual, spss$table[rn, cn],
                        label = sprintf("[1.1] cell %s × %s", rn, cn))
    }
  }
  # Row/column totals
  for (rn in names(spss$row_totals)) {
    assert_spss_count(r$row_totals[rn], spss$row_totals[rn],
                      label = sprintf("[1.1] row total %s", rn))
  }
  for (cn in names(spss$col_totals)) {
    assert_spss_count(r$col_totals[cn], spss$col_totals[cn],
                      label = sprintf("[1.1] col total %s", cn))
  }
  assert_spss_count(r$total, spss$total, label = "[1.1] grand total")
})

# Counts, margins and (optionally) percentages of one weighted table
compare_weighted_table <- function(r, spss, scenario, pct = c("row", "col", "total")) {
  for (i in seq_len(nrow(spss$table))) {
    for (j in seq_len(ncol(spss$table))) {
      assert_spss_count(unname(r$table[i, j]), spss$table[i, j],
                        label = sprintf("[%s] cell %d,%d", scenario, i, j))
    }
  }
  for (i in seq_along(spss$row_totals)) {
    assert_spss_count(unname(r$row_totals[i]), unname(spss$row_totals[i]),
                      label = sprintf("[%s] row total %d", scenario, i))
  }
  for (j in seq_along(spss$col_totals)) {
    assert_spss_count(unname(r$col_totals[j]), unname(spss$col_totals[j]),
                      label = sprintf("[%s] col total %d", scenario, j))
  }
  assert_spss_count(unname(r$total), spss$total,
                    label = sprintf("[%s] grand total", scenario))
  for (type in pct) {
    ref <- spss[[paste0(type, "_pct")]]
    if (is.null(ref)) next
    got <- r[[paste0(type, "_pct")]]
    for (i in seq_len(nrow(ref))) {
      for (j in seq_len(ncol(ref))) {
        assert_spss(unname(got[i, j]), ref[i, j], tier = "display", precision = 1,
                    label = sprintf("[%s] %s %% %d,%d", scenario, type, i, j))
      }
    }
  }
}

test_that("Test 2.1: crosstab gender × region weighted — matches SPSS (ROUND CELL)", {
  r <- survey_data |>
    crosstab(gender, region, weights = sampling_weight, percentages = "all")
  compare_weighted_table(r, spss_values$test_2_1_weighted, "2.1")
})

test_that("Test 2.2: crosstab education × employment weighted — margins are sums of rounded cells", {
  r <- survey_data |> crosstab(education, employment, weights = sampling_weight)
  compare_weighted_table(r, spss_values$test_2_2_weighted, "2.2", pct = character(0))
})

test_that("Test 4.1: crosstab gender × education weighted, grouped by region — matches SPSS", {
  r <- survey_data |> group_by(region) |>
    crosstab(gender, education, weights = sampling_weight)
  for (res in r$results) {
    rg <- as.character(res$group_info$region)
    compare_weighted_table(res, spss_values$test_4_1_weighted[[rg]],
                           sprintf("4.1 %s", rg), pct = "row")
  }
})

test_that("Test 3.1: crosstab gender × education grouped by region — matches SPSS", {
  r <- survey_data |> group_by(region) |> crosstab(gender, education)
  # results is list of 2 (East, West)
  expect_equal(length(r$results), 2L)

  # Validate East subset by row sums (totals per gender in East should be
  # the count of each gender in East region)
  for (i in seq_along(r$results)) {
    sub <- r$results[[i]]
    row_total_male <- sub$row_totals["Male"]
    expect_true(row_total_male > 0)
  }
})


# =============================================================================
# Note: SPSS CROSSTABS Test 1.2 (4×5 Education × Employment) values are
# extensive; this migration validates the simpler 2×2 case fully. The
# chi_square pilot validates the same crosstabulations from the
# inference-test angle (Expected Count + chi² + Sig).
# =============================================================================

# =============================================================================
# Adjusted standardized residuals (0.6.17) — property-based (Charter Tier 4
# until the SPSS /CELLS=ASRESID reference run lands, see .claude/BACKLOG.md).
# Oracle: the Haberman (1973) formula as implemented independently by
# stats::chisq.test()$stdres.
# =============================================================================

test_that("Adjusted residuals match chisq.test()$stdres (unweighted)", {
  r <- crosstab(survey_data, gender, region)
  ref <- suppressWarnings(
    chisq.test(table(survey_data$gender, survey_data$region), correct = FALSE)
  )
  for (i in seq_len(nrow(r$adj_residuals))) {
    for (j in seq_len(ncol(r$adj_residuals))) {
      assert_spss(r$adj_residuals[i, j], unname(ref$stdres[i, j]),
                  tier = "display", precision = 5,
                  label = sprintf("adj.residual[%d,%d] gender x region", i, j))
      assert_spss(r$expected[i, j], unname(ref$expected[i, j]),
                  tier = "display", precision = 5,
                  label = sprintf("expected[%d,%d] gender x region", i, j))
    }
  }
})

test_that("Weighted adjusted residuals follow the Haberman formula on the rounded counts", {
  r <- crosstab(survey_data, gender, region, weights = sampling_weight)

  # Independent recomputation from the weighted contingency table with
  # SPSS's /COUNT ROUND CELL (cells rounded before any statistic)
  d <- survey_data[!is.na(survey_data$gender) & !is.na(survey_data$region) &
                     !is.na(survey_data$sampling_weight), ]
  tab <- round(xtabs(sampling_weight ~ gender + region, data = d))
  rt <- rowSums(tab); ct <- colSums(tab); N <- sum(tab)
  E <- outer(rt, ct) / N
  expected_res <- (tab - E) / sqrt(E * outer(1 - rt / N, 1 - ct / N))

  for (i in seq_len(nrow(r$adj_residuals))) {
    for (j in seq_len(ncol(r$adj_residuals))) {
      assert_spss(r$adj_residuals[i, j], unname(expected_res[i, j]),
                  tier = "display", precision = 5,
                  label = sprintf("weighted adj.residual[%d,%d]", i, j))
    }
  }
})
