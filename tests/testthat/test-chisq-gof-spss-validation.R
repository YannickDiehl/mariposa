# =============================================================================
# chisq_gof — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::chisq_gof() against SPSS v29 NPAR /CHISQUARE.
# Reference output: tests/spss_reference/outputs/chisq_gof_output.txt
#
# NPAR TESTS family — the weighted chi-square GOF (NPAR with WEIGHT BY) is
# pending an SPSS WEIGHT BY reference run and is not asserted here. Covered:
# unweighted ungrouped (Tests 1a-1e, 1f with /EXPECTED=5 3 2) and unweighted
# SPLIT FILE region (Test 3). The weighted-grouped scenario is pending for the
# same reason.
#
# Frequencies table: chisq_gof() returns Observed N / Expected N / Residual
# ($frequencies) for ungrouped and (since 0.7.4) grouped calls; expected
# counts and residuals are unrounded and print half up like SPSS (East:
# 121.25 -> 121.3, -0.25 -> -.3).
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(
  test_gender       = list(chi_sq = 5.018,    df = 1L, p = 0.025, n = 2500L),    # chisq_gof_output.txt:32
  test_education    = list(chi_sq = 156.454,  df = 3L, p = "<.001", n = 2500L),  # chisq_gof_output.txt:70
  test_region       = list(chi_sq = 936.360,  df = 1L, p = "<.001", n = 2500L),  # chisq_gof_output.txt:106
  test_employment   = list(chi_sq = 3276.116, df = 4L, p = "<.001", n = 2500L),  # chisq_gof_output.txt:145
  test_interview    = list(chi_sq = 816.620,  df = 2L, p = "<.001", n = 2500L),  # chisq_gof_output.txt:182

  # ---- Test 1f: interview_mode /EXPECTED=5 3 2 ----
  test_interview_532 = list(
    chi_sq = 95.413,          # chisq_gof_output.txt:220
    df = 2L,                  # chisq_gof_output.txt:221
    p = "<.001",              # chisq_gof_output.txt:222
    n = 2500L                 # chisq_gof_output.txt:215
  ),

  # ---- Frequencies tables: one row per category, SPSS order ----
  # columns: Observed N, Expected N, Residual
  freq_gender = rbind(
    Male   = c(1194, 1250.0, -56.0),    # chisq_gof_output.txt:25
    Female = c(1306, 1250.0, 56.0)      # chisq_gof_output.txt:26
  ),
  freq_education = rbind(
    `Basic Secondary`        = c(841, 625.0, 216.0),    # chisq_gof_output.txt:61
    `Intermediate Secondary` = c(629, 625.0, 4.0),      # chisq_gof_output.txt:62
    `Academic Secondary`     = c(631, 625.0, 6.0),      # chisq_gof_output.txt:63
    University               = c(399, 625.0, -226.0)    # chisq_gof_output.txt:64
  ),
  freq_region = rbind(
    East = c(485,  1250.0, -765.0),     # chisq_gof_output.txt:99
    West = c(2015, 1250.0, 765.0)       # chisq_gof_output.txt:100
  ),
  freq_employment = rbind(
    Student    = c(78,   500.0, -422.0),    # chisq_gof_output.txt:135
    Employed   = c(1600, 500.0, 1100.0),    # chisq_gof_output.txt:136
    Unemployed = c(182,  500.0, -318.0),    # chisq_gof_output.txt:137
    Retired    = c(525,  500.0, 25.0),      # chisq_gof_output.txt:138
    Other      = c(115,  500.0, -385.0)     # chisq_gof_output.txt:139
  ),
  freq_interview = rbind(
    `Face-to-face` = c(1485, 833.3, 651.7),     # chisq_gof_output.txt:174
    Telephone      = c(655,  833.3, -178.3),    # chisq_gof_output.txt:175
    Online         = c(360,  833.3, -473.3)     # chisq_gof_output.txt:176
  ),
  freq_interview_532 = rbind(
    `Face-to-face` = c(1485, 1250.0, 235.0),    # chisq_gof_output.txt:212
    Telephone      = c(655,  750.0,  -95.0),    # chisq_gof_output.txt:213
    Online         = c(360,  500.0,  -140.0)    # chisq_gof_output.txt:214
  ),

  # ---- Test 3: SPLIT FILE region (unweighted) ----
  grouped_education = list(
    East = list(chi_sq = 34.645,     # chisq_gof_output.txt:375
                df = 3L,             # chisq_gof_output.txt:376
                p = "<.001",         # chisq_gof_output.txt:377
                n = 485L),           # chisq_gof_output.txt:365
    West = list(chi_sq = 122.888,    # chisq_gof_output.txt:378
                df = 3L,             # chisq_gof_output.txt:379
                p = "<.001",         # chisq_gof_output.txt:380
                n = 2015L)           # chisq_gof_output.txt:370
  ),
  grouped_gender = list(
    East = list(chi_sq = 0.167,      # chisq_gof_output.txt:418
                df = 1L,             # chisq_gof_output.txt:419
                p = 0.683,           # chisq_gof_output.txt:420
                n = 485L),           # chisq_gof_output.txt:410
    West = list(chi_sq = 5.265,      # chisq_gof_output.txt:421
                df = 1L,             # chisq_gof_output.txt:422
                p = 0.022,           # chisq_gof_output.txt:423
                n = 2015L)           # chisq_gof_output.txt:413
  ),
  grouped_freq_education = list(
    East = rbind(
      `Basic Secondary`        = c(170, 121.3, 48.8),     # chisq_gof_output.txt:360
      `Intermediate Secondary` = c(121, 121.3, -0.3),     # chisq_gof_output.txt:361
      `Academic Secondary`     = c(115, 121.3, -6.3),     # chisq_gof_output.txt:362
      University               = c(79, 121.3, -42.3)      # chisq_gof_output.txt:363
    ),
    West = rbind(
      `Basic Secondary`        = c(671, 503.8, 167.3),    # chisq_gof_output.txt:365
      `Intermediate Secondary` = c(508, 503.8, 4.3),      # chisq_gof_output.txt:366
      `Academic Secondary`     = c(516, 503.8, 12.3),     # chisq_gof_output.txt:367
      University               = c(320, 503.8, -183.8)    # chisq_gof_output.txt:368
    )
  ),
  grouped_freq_gender = list(
    East = rbind(
      Male   = c(238, 242.5, -4.5),     # chisq_gof_output.txt:409
      Female = c(247, 242.5, 4.5)       # chisq_gof_output.txt:410
    ),
    West = rbind(
      Male   = c(956, 1007.5, -51.5),   # chisq_gof_output.txt:412
      Female = c(1059, 1007.5, 51.5)    # chisq_gof_output.txt:413
    )
  )
)


data(survey_data, envir = environment())


compare_gof <- function(row, spss, scenario) {
  assert_spss(as.numeric(row$chi_squared), spss$chi_sq,
              tier = "display", precision = 3,
              label = sprintf("[%s] chi²", scenario))
  assert_spss_count(as.numeric(row$df), spss$df,
                    label = sprintf("[%s] df", scenario))
  assert_spss(as.numeric(row$p_value), spss$p,
              tier = "display", precision = 3, what = "p_value",
              label = sprintf("[%s] p-value", scenario))
  assert_spss_count(as.numeric(row$n), spss$n,
                    label = sprintf("[%s] N", scenario))
}

# SPSS "Frequencies" table: category order, Observed N (exact), Expected N
# and Residual (1 decimal as printed)
compare_gof_frequencies <- function(freq, spss, scenario) {
  expect_identical(as.character(freq$category), rownames(spss),
                   label = sprintf("[%s] category order", scenario))
  for (i in seq_len(nrow(spss))) {
    cat_lab <- sprintf("[%s] %s", scenario, rownames(spss)[i])
    assert_spss_count(as.numeric(freq$observed[i]), spss[i, 1],
                      label = paste(cat_lab, "observed N"))
    assert_spss(as.numeric(freq$expected[i]), spss[i, 2],
                tier = "display", precision = 1,
                label = paste(cat_lab, "expected N"))
    assert_spss(as.numeric(freq$residual[i]), spss[i, 3],
                tier = "display", precision = 1,
                label = paste(cat_lab, "residual"))
  }
}

compare_gof_grouped <- function(r, spss, scenario, spss_freq = NULL) {
  expect_identical(as.character(r$results$region), names(spss),
                   label = sprintf("[%s] split-file group order", scenario))
  for (i in seq_along(names(spss))) {
    compare_gof(r$results[i, ], spss[[i]],
                sprintf("%s %s", scenario, names(spss)[i]))
  }
  for (rg in names(spss_freq)) {
    freq <- r$frequencies[as.character(r$frequencies$region) == rg, ]
    compare_gof_frequencies(freq, spss_freq[[rg]],
                            sprintf("%s %s", scenario, rg))
  }
}


test_that("Test 1: chisq_gof gender — matches SPSS", {
  r <- survey_data |> chisq_gof(gender)
  compare_gof(r$results, spss_values$test_gender, "gender")
  compare_gof_frequencies(r$frequencies, spss_values$freq_gender, "gender")
})

test_that("Test 2: chisq_gof education — matches SPSS", {
  r <- survey_data |> chisq_gof(education)
  compare_gof(r$results, spss_values$test_education, "education")
  compare_gof_frequencies(r$frequencies, spss_values$freq_education, "education")
})

test_that("Test 3: chisq_gof region — matches SPSS", {
  r <- survey_data |> chisq_gof(region)
  compare_gof(r$results, spss_values$test_region, "region")
  compare_gof_frequencies(r$frequencies, spss_values$freq_region, "region")
})

test_that("Test 4: chisq_gof employment — matches SPSS", {
  r <- survey_data |> chisq_gof(employment)
  compare_gof(r$results, spss_values$test_employment, "employment")
  compare_gof_frequencies(r$frequencies, spss_values$freq_employment, "employment")
})

test_that("Test 5: chisq_gof interview_mode — matches SPSS", {
  r <- survey_data |> chisq_gof(interview_mode)
  compare_gof(r$results, spss_values$test_interview, "interview_mode")
  compare_gof_frequencies(r$frequencies, spss_values$freq_interview,
                          "interview_mode")
})

test_that("Test 1f: chisq_gof interview_mode with /EXPECTED=5 3 2 — matches SPSS", {
  # SPSS divides the /EXPECTED values by their sum; so does chisq_gof()
  r <- survey_data |> chisq_gof(interview_mode, expected = c(5, 3, 2))
  compare_gof(r$results, spss_values$test_interview_532, "interview_mode 5:3:2")
  compare_gof_frequencies(r$frequencies, spss_values$freq_interview_532,
                          "interview_mode 5:3:2")
})

test_that("Test 3 grouped: chisq_gof education by region — matches SPSS", {
  r <- survey_data |> group_by(region) |> chisq_gof(education)
  compare_gof_grouped(r, spss_values$grouped_education, "education by region",
                      spss_values$grouped_freq_education)
})

test_that("Test 3 grouped: chisq_gof gender by region — matches SPSS", {
  r <- survey_data |> group_by(region) |> chisq_gof(gender)
  compare_gof_grouped(r, spss_values$grouped_gender, "gender by region",
                      spss_values$grouped_freq_gender)
})
