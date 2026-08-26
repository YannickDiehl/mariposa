# =============================================================================
# multiple_response — PROPERTY-BASED VALIDATION (Charter Tier-4)
# =============================================================================
# Purpose: Validate that multiple_response() implements the SPSS
# MULT RESPONSE dichotomy-set semantics, without an SPSS reference run
# (pending, see .claude/BACKLOG.md and spss-syntax-0.7.0-references.sps).
#
# Oracle: direct hand-computation from the indicator matrix (colSums /
# weighted sums), independent of the implementation's matrix algebra.
# Case rule under test (SPSS): a case is valid with at least one
# non-missing indicator; all-missing cases are excluded; with BY, the by
# variable must be non-missing too.
#
# Scenario coverage (Charter §8): all four scenarios (unweighted/weighted
# x ungrouped/grouped), plus the BY mode.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


data(survey_data, envir = environment())
mr_data <- survey_data %>%
  mutate(
    gov     = as.integer(trust_government >= 4),
    media   = as.integer(trust_media >= 4),
    science = as.integer(trust_science >= 4)
  )


test_that("Unweighted frequencies match hand-computed mentions and both percent bases", {
  r <- multiple_response(mr_data, gov, media, science)

  ind <- mr_data[, c("gov", "media", "science")]
  valid <- rowSums(!is.na(ind)) > 0
  ind_v <- ind[valid, ]
  mentions <- colSums(ind_v == 1, na.rm = TRUE)
  n_cases <- sum(valid)
  n_resp <- sum(mentions)

  assert_spss_count(round(r$n_cases), n_cases, label = "valid cases")
  assert_spss_count(round(r$n_responses), n_resp, label = "total responses")

  for (v in c("gov", "media", "science")) {
    row <- r$results[r$results$Option == v, ]
    assert_spss_count(round(row$n), unname(mentions[v]),
                      label = sprintf("mentions for %s", v))
    assert_spss(row$pct_responses, unname(mentions[v]) / n_resp * 100,
                tier = "display", precision = 5,
                label = sprintf("%% of responses for %s", v))
    assert_spss(row$pct_cases, unname(mentions[v]) / n_cases * 100,
                tier = "display", precision = 5,
                label = sprintf("%% of cases for %s", v))
  }

  # % of responses sums to 100 by construction
  assert_spss(sum(r$results$pct_responses), 100,
              tier = "display", precision = 6, label = "responses % sums to 100")
})


test_that("All-missing cases are excluded, partially missing cases stay valid", {
  d <- tibble(
    a = c(1, 0, NA, NA, 1),
    b = c(0, 1, NA, 1, NA)
  )
  r <- multiple_response(d, a, b)
  # Case 3 (all NA) excluded; cases 4 and 5 valid with one usable indicator
  assert_spss_count(round(r$n_cases), 4L, label = "valid cases with partial NA")
  assert_spss_count(round(r$n_missing), 1L, label = "excluded all-missing case")
  assert_spss_count(round(r$results$n[r$results$Option == "a"]), 2L,
                    label = "mentions a (NA indicator adds nothing)")
  assert_spss_count(round(r$results$n[r$results$Option == "b"]), 2L,
                    label = "mentions b")
})


test_that("Weighted frequencies use unrounded weight sums", {
  r <- multiple_response(mr_data, gov, media, science,
                         weights = sampling_weight)

  ind <- mr_data[, c("gov", "media", "science")]
  w <- mr_data$sampling_weight
  valid <- rowSums(!is.na(ind)) > 0 & !is.na(w)
  ind_v <- ind[valid, ]
  w_v <- w[valid]
  n_cases <- sum(w_v)

  for (v in c("gov", "media", "science")) {
    m <- sum(w_v * (ind_v[[v]] == 1), na.rm = TRUE)
    row <- r$results[r$results$Option == v, ]
    assert_spss(row$n, m, tier = "display", precision = 5,
                label = sprintf("weighted mentions for %s", v))
    assert_spss(row$pct_cases, m / n_cases * 100,
                tier = "display", precision = 5,
                label = sprintf("weighted %% of cases for %s", v))
  }
  assert_spss(r$n_cases, n_cases, tier = "display", precision = 5,
              label = "weighted valid cases, unrounded")
})


test_that("BY mode matches per-level hand computation and excludes by-missing cases", {
  r <- multiple_response(mr_data, gov, media, science, by = gender)

  ind <- mr_data[, c("gov", "media", "science")]
  valid <- rowSums(!is.na(ind)) > 0 & !is.na(mr_data$gender)
  for (g in levels(droplevels(mr_data$gender[valid]))) {
    idx <- valid & mr_data$gender == g
    cases_g <- sum(idx)
    for (v in c("gov", "media")) {
      m <- sum(ind[[v]][idx] == 1, na.rm = TRUE)
      row <- r$by_results[r$by_results$by_level == g &
                            r$by_results$Option == v, ]
      assert_spss_count(round(row$n), m,
                        label = sprintf("BY mentions %s / %s", v, g))
      assert_spss(row$pct_cases, m / cases_g * 100,
                  tier = "display", precision = 5,
                  label = sprintf("BY %% of cases %s / %s", v, g))
      assert_spss_count(round(row$n_cases_level), cases_g,
                        label = sprintf("BY column cases %s", g))
    }
  }
})


test_that("Grouped results equal per-subset ungrouped results (SPSS SPLIT FILE)", {
  rg <- mr_data |> group_by(region) |> multiple_response(gov, media, science)
  for (g in unique(mr_data$region)) {
    sub <- mr_data[mr_data$region == g, ]
    ru <- multiple_response(sub, gov, media, science)
    rows <- rg$results[rg$results$region == g, ]
    for (i in seq_len(nrow(rows))) {
      assert_spss(rows$n[i], ru$results$n[i], tier = "display", precision = 6,
                  label = sprintf("grouped mentions == subset (%s, %s)",
                                  g, rows$Option[i]))
      assert_spss(rows$pct_cases[i], ru$results$pct_cases[i],
                  tier = "display", precision = 6,
                  label = sprintf("grouped %% of cases == subset (%s, %s)",
                                  g, rows$Option[i]))
    }
  }
})


test_that("Weighted grouped frequencies equal per-subset weighted results", {
  rg <- mr_data |> group_by(region) |>
    multiple_response(gov, media, science, weights = sampling_weight)
  g <- unique(mr_data$region)[1]
  sub <- mr_data[mr_data$region == g, ]
  ru <- multiple_response(sub, gov, media, science, weights = sampling_weight)
  rows <- rg$results[rg$results$region == g, ]
  for (i in seq_len(nrow(rows))) {
    assert_spss(rows$n[i], ru$results$n[i], tier = "display", precision = 6,
                label = sprintf("weighted grouped mentions == subset (%s)",
                                rows$Option[i]))
  }
})


test_that("counted values other than 1 work", {
  d <- tibble(a = c(2, 2, 0, 1), b = c(0, 2, 2, 2))
  r <- multiple_response(d, a, b, counted = 2)
  assert_spss_count(round(r$results$n[r$results$Option == "a"]), 2L,
                    label = "counted = 2 mentions a")
  assert_spss_count(round(r$results$n[r$results$Option == "b"]), 3L,
                    label = "counted = 2 mentions b")
})


test_that("Input contracts error clearly", {
  expect_error(multiple_response(mr_data, gov), "at least two")
  expect_error(multiple_response(mr_data, gov, media, by = gov),
               "cannot be part of the set")
})
