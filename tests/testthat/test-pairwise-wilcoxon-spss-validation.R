# =============================================================================
# pairwise_wilcoxon — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::pairwise_wilcoxon() (post-hoc for Friedman)
#          against SPSS pairwise Wilcoxon signed-rank tests.
# Reference: pairwise_wilcoxon_output.txt (syntax: pairwise_wilcoxon.sps)
#
# SPSS runs one NPAR TESTS /WILCOXON per pair (pairwise deletion, so N
# differs per pair and from the listwise Friedman N). Per pair it prints
# N, Z, the unadjusted Asymp. Sig. and a "Based on ... ranks" footnote —
# asserted against pairwise_wilcoxon()'s n, z, p and z_based_on (p_adj is
# mariposa's Bonferroni step and has no SPSS counterpart). The Ranks table
# above each Z is asserted through wilcoxon_test() on the same pair, unless
# test-wilcoxon-test-spss-validation.R already asserts the identical table.
#
# Scenarios in pairwise_wilcoxon_output.txt:
#   Test 1a trust triplet (unweighted): only the Friedman table (:1-34) was
#     captured, so the pairs are cited from wilcoxon_test_output.txt (1a-1c)
#   Test 1b lifestyle triplet (:71-150)          — asserted
#   Test 2a trust triplet, WEIGHTED (:152-231)   — not asserted (weighted
#     NPAR TESTS pending the SPSS WEIGHT BY run)
#   Test 3 trust triplet split by region (:233-330) — asserted
#   Test 4 same, WEIGHTED (:332-429)             — not asserted (pending)
#   Test 5a longitudinal, 6 pairs (:468-610)     — asserted
#   Test 5b longitudinal split by group (:612-790) — asserted
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# Individual Wilcoxon signed-rank tests per pair; SPSS's Z comes from the
# smaller rank sum, so it is <= 0 (0.7.4: asserted with its sign). Pairs are
# var1 WITH var2 (PAIRED): SPSS ranks var2 - var1. Rows without n_neg have
# their Ranks table asserted in test-wilcoxon-test-spss-validation.R.
spss_values <- list(

  # ---- Test 1a: trust triplet (tables in wilcoxon_test_output.txt 1a-1c) --
  trust_pairs = list(
    list(var1 = "trust_government", var2 = "trust_media", n = 2227,          # wilcoxon_test_output.txt:31
         z = -5.097, p = "<.001",                                             # wilcoxon_test_output.txt:39
         based_on = "positive ranks"),                                        # wilcoxon_test_output.txt:42
    list(var1 = "trust_government", var2 = "trust_science", n = 2255,        # wilcoxon_test_output.txt:74
         z = -25.945, p = "<.001",                                            # wilcoxon_test_output.txt:82
         based_on = "negative ranks"),                                        # wilcoxon_test_output.txt:85
    list(var1 = "trust_media", var2 = "trust_science", n = 2272,             # wilcoxon_test_output.txt:117
         z = -29.091, p = "<.001",                                            # wilcoxon_test_output.txt:125
         based_on = "negative ranks")                                         # wilcoxon_test_output.txt:128
  ),

  # ---- Test 1b: lifestyle triplet -----------------------------------------
  lifestyle = list(
    list(var1 = "life_satisfaction", var2 = "environmental_concern",
         n_neg = 901, mean_rank_neg = 882.20, sum_rank_neg = 794858.50,      # pairwise_wilcoxon_output.txt:96
         n_pos = 852, mean_rank_pos = 871.51, sum_rank_pos = 742522.50,      # pairwise_wilcoxon_output.txt:97
         n_ties = 571, n = 2324,                                             # pairwise_wilcoxon_output.txt:98
         z = -1.260, p = 0.207,                                              # pairwise_wilcoxon_output.txt:107
         based_on = "positive ranks"),                                       # pairwise_wilcoxon_output.txt:110
    list(var1 = "life_satisfaction", var2 = "political_orientation",
         n_neg = 1338, mean_rank_neg = 942.70, sum_rank_neg = 1261336.00,    # pairwise_wilcoxon_output.txt:116
         n_pos = 423,  mean_rank_pos = 685.83, sum_rank_pos = 290105.00,     # pairwise_wilcoxon_output.txt:117
         n_ties = 467, n = 2228,                                             # pairwise_wilcoxon_output.txt:118
         z = -23.129, p = "<.001",                                           # pairwise_wilcoxon_output.txt:127
         based_on = "positive ranks"),                                       # pairwise_wilcoxon_output.txt:130
    list(var1 = "environmental_concern", var2 = "political_orientation",
         n_neg = 1380, mean_rank_neg = 1037.84, sum_rank_neg = 1432219.50,   # pairwise_wilcoxon_output.txt:136
         n_pos = 593,  mean_rank_pos = 868.69,  sum_rank_pos = 515131.50,    # pairwise_wilcoxon_output.txt:137
         n_ties = 234, n = 2207,                                             # pairwise_wilcoxon_output.txt:138
         z = -18.316, p = "<.001",                                           # pairwise_wilcoxon_output.txt:147
         based_on = "positive ranks")                                        # pairwise_wilcoxon_output.txt:150
  ),

  # ---- Test 3: trust triplet split by region ------------------------------
  # gov/media and gov/science Ranks tables: wilcoxon Tests 3a/3b
  region = list(
    list(grp = "East", var1 = "trust_government", var2 = "trust_media",
         n = 435,                                                            # pairwise_wilcoxon_output.txt:261
         z = -2.727, p = 0.006,                                              # pairwise_wilcoxon_output.txt:273
         based_on = "positive ranks"),                                       # pairwise_wilcoxon_output.txt:278
    list(grp = "West", var1 = "trust_government", var2 = "trust_media",
         n = 1792,                                                           # pairwise_wilcoxon_output.txt:265
         z = -4.346, p = "<.001",                                            # pairwise_wilcoxon_output.txt:275
         based_on = "positive ranks"),                                       # pairwise_wilcoxon_output.txt:278
    list(grp = "East", var1 = "trust_government", var2 = "trust_science",
         n = 444,                                                            # pairwise_wilcoxon_output.txt:287
         z = -11.635, p = "<.001",                                           # pairwise_wilcoxon_output.txt:299
         based_on = "negative ranks"),                                       # pairwise_wilcoxon_output.txt:304
    list(grp = "West", var1 = "trust_government", var2 = "trust_science",
         n = 1811,                                                           # pairwise_wilcoxon_output.txt:291
         z = -23.191, p = "<.001",                                           # pairwise_wilcoxon_output.txt:301
         based_on = "negative ranks"),                                       # pairwise_wilcoxon_output.txt:304
    list(grp = "East", var1 = "trust_media", var2 = "trust_science",
         n_neg = 60,  mean_rank_neg = 107.15, sum_rank_neg = 6429.00,        # pairwise_wilcoxon_output.txt:310
         n_pos = 312, mean_rank_pos = 201.76, sum_rank_pos = 62949.00,       # pairwise_wilcoxon_output.txt:311
         n_ties = 75, n = 447,                                               # pairwise_wilcoxon_output.txt:312
         z = -13.820, p = "<.001",                                           # pairwise_wilcoxon_output.txt:325
         based_on = "negative ranks"),                                       # pairwise_wilcoxon_output.txt:330
    list(grp = "West", var1 = "trust_media", var2 = "trust_science",
         n_neg = 262,  mean_rank_neg = 539.23, sum_rank_neg = 141277.00,     # pairwise_wilcoxon_output.txt:314
         n_pos = 1246, mean_rank_pos = 799.77, sum_rank_pos = 996509.00,     # pairwise_wilcoxon_output.txt:315
         n_ties = 317, n = 1825,                                             # pairwise_wilcoxon_output.txt:316
         z = -25.630, p = "<.001",                                           # pairwise_wilcoxon_output.txt:327
         based_on = "negative ranks")                                        # pairwise_wilcoxon_output.txt:330
  ),

  # ---- Test 5a: longitudinal_data_wide, 6 pairs (T_j - T_i) ---------------
  # T1/T2 and T1/T3 Ranks tables: wilcoxon Tests 5a/5b
  longitudinal = list(
    list(var1 = "score_T1", var2 = "score_T2", n = 105,                      # pairwise_wilcoxon_output.txt:499
         z = -5.427, p = "<.001",                                            # pairwise_wilcoxon_output.txt:507
         based_on = "negative ranks"),                                       # pairwise_wilcoxon_output.txt:510
    list(var1 = "score_T1", var2 = "score_T3", n = 96,                       # pairwise_wilcoxon_output.txt:519
         z = -6.132, p = "<.001",                                            # pairwise_wilcoxon_output.txt:527
         based_on = "negative ranks"),                                       # pairwise_wilcoxon_output.txt:530
    list(var1 = "score_T1", var2 = "score_T4",
         n_neg = 14, mean_rank_neg = 18.57, sum_rank_neg = 260.00,           # pairwise_wilcoxon_output.txt:536
         n_pos = 64, mean_rank_pos = 44.08, sum_rank_pos = 2821.00,          # pairwise_wilcoxon_output.txt:537
         n_ties = 0, n = 78,                                                 # pairwise_wilcoxon_output.txt:538
         z = -6.378, p = "<.001",                                            # pairwise_wilcoxon_output.txt:547
         based_on = "negative ranks"),                                       # pairwise_wilcoxon_output.txt:550
    list(var1 = "score_T2", var2 = "score_T3",
         n_neg = 28, mean_rank_neg = 38.96, sum_rank_neg = 1091.00,          # pairwise_wilcoxon_output.txt:556
         n_pos = 67, mean_rank_pos = 51.78, sum_rank_pos = 3469.00,          # pairwise_wilcoxon_output.txt:557
         n_ties = 0, n = 95,                                                 # pairwise_wilcoxon_output.txt:558
         z = -4.413, p = "<.001",                                            # pairwise_wilcoxon_output.txt:567
         based_on = "negative ranks"),                                       # pairwise_wilcoxon_output.txt:570
    list(var1 = "score_T2", var2 = "score_T4",
         n_neg = 18, mean_rank_neg = 27.50, sum_rank_neg = 495.00,           # pairwise_wilcoxon_output.txt:576
         n_pos = 58, mean_rank_pos = 41.91, sum_rank_pos = 2431.00,          # pairwise_wilcoxon_output.txt:577
         n_ties = 0, n = 76,                                                 # pairwise_wilcoxon_output.txt:578
         z = -5.012, p = "<.001",                                            # pairwise_wilcoxon_output.txt:587
         based_on = "negative ranks"),                                       # pairwise_wilcoxon_output.txt:590
    list(var1 = "score_T3", var2 = "score_T4",
         n_neg = 31, mean_rank_neg = 28.84, sum_rank_neg = 894.00,           # pairwise_wilcoxon_output.txt:596
         n_pos = 46, mean_rank_pos = 45.85, sum_rank_pos = 2109.00,          # pairwise_wilcoxon_output.txt:597
         n_ties = 0, n = 77,                                                 # pairwise_wilcoxon_output.txt:598
         z = -3.085, p = 0.002,                                              # pairwise_wilcoxon_output.txt:607
         based_on = "negative ranks")                                        # pairwise_wilcoxon_output.txt:610
  ),

  # ---- Test 5b: longitudinal split by group -------------------------------
  # T1/T2 Ranks tables: wilcoxon Test 5c
  longitudinal_by_group = list(
    list(grp = "Control", var1 = "score_T1", var2 = "score_T2",
         n = 51,                                                             # pairwise_wilcoxon_output.txt:643
         z = -2.512, p = 0.012,                                              # pairwise_wilcoxon_output.txt:655
         based_on = "negative ranks"),                                       # pairwise_wilcoxon_output.txt:660
    list(grp = "Treatment", var1 = "score_T1", var2 = "score_T2",
         n = 54,                                                             # pairwise_wilcoxon_output.txt:647
         z = -5.007, p = "<.001",                                            # pairwise_wilcoxon_output.txt:657
         based_on = "negative ranks"),                                       # pairwise_wilcoxon_output.txt:660
    list(grp = "Control", var1 = "score_T1", var2 = "score_T3",
         n_neg = 18, mean_rank_neg = 20.56, sum_rank_neg = 370.00,           # pairwise_wilcoxon_output.txt:666
         n_pos = 31, mean_rank_pos = 27.58, sum_rank_pos = 855.00,           # pairwise_wilcoxon_output.txt:667
         n_ties = 0, n = 49,                                                 # pairwise_wilcoxon_output.txt:668
         z = -2.412, p = 0.016,                                              # pairwise_wilcoxon_output.txt:681
         based_on = "negative ranks"),                                       # pairwise_wilcoxon_output.txt:686
    list(grp = "Treatment", var1 = "score_T1", var2 = "score_T3",
         n_neg = 3,  mean_rank_neg = 6.00,  sum_rank_neg = 18.00,            # pairwise_wilcoxon_output.txt:670
         n_pos = 44, mean_rank_pos = 25.23, sum_rank_pos = 1110.00,          # pairwise_wilcoxon_output.txt:671
         n_ties = 0, n = 47,                                                 # pairwise_wilcoxon_output.txt:672
         z = -5.778, p = "<.001",                                            # pairwise_wilcoxon_output.txt:683
         based_on = "negative ranks"),                                       # pairwise_wilcoxon_output.txt:686
    list(grp = "Control", var1 = "score_T1", var2 = "score_T4",
         n_neg = 14, mean_rank_neg = 15.36, sum_rank_neg = 215.00,           # pairwise_wilcoxon_output.txt:692
         n_pos = 27, mean_rank_pos = 23.93, sum_rank_pos = 646.00,           # pairwise_wilcoxon_output.txt:693
         n_ties = 0, n = 41,                                                 # pairwise_wilcoxon_output.txt:694
         z = -2.793, p = 0.005,                                              # pairwise_wilcoxon_output.txt:707
         based_on = "negative ranks"),                                       # pairwise_wilcoxon_output.txt:712
    # empty negative-rank category: SPSS prints mean rank and sum as .00
    list(grp = "Treatment", var1 = "score_T1", var2 = "score_T4",
         n_neg = 0,  mean_rank_neg = 0.00,  sum_rank_neg = 0.00,             # pairwise_wilcoxon_output.txt:696
         n_pos = 37, mean_rank_pos = 19.00, sum_rank_pos = 703.00,           # pairwise_wilcoxon_output.txt:697
         n_ties = 0, n = 37,                                                 # pairwise_wilcoxon_output.txt:698
         z = -5.303, p = "<.001",                                            # pairwise_wilcoxon_output.txt:709
         based_on = "negative ranks"),                                       # pairwise_wilcoxon_output.txt:712
    list(grp = "Control", var1 = "score_T2", var2 = "score_T3",
         n_neg = 21, mean_rank_neg = 23.81, sum_rank_neg = 500.00,           # pairwise_wilcoxon_output.txt:718
         n_pos = 26, mean_rank_pos = 24.15, sum_rank_pos = 628.00,           # pairwise_wilcoxon_output.txt:719
         n_ties = 0, n = 47,                                                 # pairwise_wilcoxon_output.txt:720
         z = -0.677, p = 0.498,                                              # pairwise_wilcoxon_output.txt:733
         based_on = "negative ranks"),                                       # pairwise_wilcoxon_output.txt:738
    list(grp = "Treatment", var1 = "score_T2", var2 = "score_T3",
         n_neg = 7,  mean_rank_neg = 17.00, sum_rank_neg = 119.00,           # pairwise_wilcoxon_output.txt:722
         n_pos = 41, mean_rank_pos = 25.78, sum_rank_pos = 1057.00,          # pairwise_wilcoxon_output.txt:723
         n_ties = 0, n = 48,                                                 # pairwise_wilcoxon_output.txt:724
         z = -4.810, p = "<.001",                                            # pairwise_wilcoxon_output.txt:735
         based_on = "negative ranks"),                                       # pairwise_wilcoxon_output.txt:738
    # 306 / 16 = 19.125 exactly, which SPSS prints as 19.13: |19.125 -
    # 19.13| sits on the inclusive half-unit boundary of precision 2
    list(grp = "Control", var1 = "score_T2", var2 = "score_T4",
         n_neg = 16, mean_rank_neg = 19.13, sum_rank_neg = 306.00,           # pairwise_wilcoxon_output.txt:744
         n_pos = 23, mean_rank_pos = 20.61, sum_rank_pos = 474.00,           # pairwise_wilcoxon_output.txt:745
         n_ties = 0, n = 39,                                                 # pairwise_wilcoxon_output.txt:746
         z = -1.172, p = 0.241,                                              # pairwise_wilcoxon_output.txt:759
         based_on = "negative ranks"),                                       # pairwise_wilcoxon_output.txt:764
    list(grp = "Treatment", var1 = "score_T2", var2 = "score_T4",
         n_neg = 2,  mean_rank_neg = 8.50,  sum_rank_neg = 17.00,            # pairwise_wilcoxon_output.txt:748
         n_pos = 35, mean_rank_pos = 19.60, sum_rank_pos = 686.00,           # pairwise_wilcoxon_output.txt:749
         n_ties = 0, n = 37,                                                 # pairwise_wilcoxon_output.txt:750
         z = -5.046, p = "<.001",                                            # pairwise_wilcoxon_output.txt:761
         based_on = "negative ranks"),                                       # pairwise_wilcoxon_output.txt:764
    list(grp = "Control", var1 = "score_T3", var2 = "score_T4",
         n_neg = 20, mean_rank_neg = 18.40, sum_rank_neg = 368.00,           # pairwise_wilcoxon_output.txt:770
         n_pos = 21, mean_rank_pos = 23.48, sum_rank_pos = 493.00,           # pairwise_wilcoxon_output.txt:771
         n_ties = 0, n = 41,                                                 # pairwise_wilcoxon_output.txt:772
         z = -0.810, p = 0.418,                                              # pairwise_wilcoxon_output.txt:785
         based_on = "negative ranks"),                                       # pairwise_wilcoxon_output.txt:790
    list(grp = "Treatment", var1 = "score_T3", var2 = "score_T4",
         n_neg = 11, mean_rank_neg = 10.00, sum_rank_neg = 110.00,           # pairwise_wilcoxon_output.txt:774
         n_pos = 25, mean_rank_pos = 22.24, sum_rank_pos = 556.00,           # pairwise_wilcoxon_output.txt:775
         n_ties = 0, n = 36,                                                 # pairwise_wilcoxon_output.txt:776
         z = -3.503, p = "<.001",                                            # pairwise_wilcoxon_output.txt:787
         based_on = "negative ranks")                                        # pairwise_wilcoxon_output.txt:790
  )
)


# =============================================================================
# COMPARISON HELPER
# =============================================================================

#' Assert one SPSS Wilcoxon block: N, Z, p and footnote from the
#' pairwise_wilcoxon() comparison row; the Ranks table (when cached) from
#' wilcoxon_test() on the same pair of `data` (grouped like the Friedman
#' input when `by` names the split variable).
check_pair <- function(comparisons, data, ref, section, by = NULL) {
  sel <- comparisons$var1 == ref$var1 & comparisons$var2 == ref$var2
  if (!is.null(by)) sel <- sel & comparisons[[by]] == ref$grp
  row <- comparisons[sel, , drop = FALSE]
  lbl <- sprintf("[%s %s%s vs %s]", section,
                 if (is.null(by)) "" else paste0(ref$grp, ": "),
                 ref$var1, ref$var2)
  expect_equal(nrow(row), 1L, label = paste(lbl, "comparison row found"))

  assert_spss_count(row$n, ref$n, label = paste(lbl, "N"))
  assert_spss(row$z, ref$z, tier = "display", precision = 3,
              label = paste(lbl, "Z"))
  expect_identical(row$z_based_on, ref$based_on,
                   label = paste(lbl, "Z based on"))
  assert_spss(row$p, ref$p, tier = "display", precision = 3,
              what = "p_value", label = paste(lbl, "p (unadjusted)"))

  if (is.null(ref$n_neg)) return(invisible(NULL))

  wx <- wilcoxon_test(data, all_of(ref$var1), all_of(ref$var2))$results
  if (!is.null(by)) wx <- wx[wx[[by]] == ref$grp, , drop = FALSE]
  expect_equal(nrow(wx), 1L, label = paste(lbl, "wilcoxon_test row found"))
  for (f in c("n_neg", "n_pos", "n_ties")) {
    assert_spss_count(wx[[f]], ref[[f]], label = paste(lbl, "Ranks", f))
  }
  assert_spss_count(wx$n_total, ref$n, label = paste(lbl, "Ranks Total"))
  for (f in c("mean_rank_neg", "sum_rank_neg", "mean_rank_pos", "sum_rank_pos")) {
    assert_spss(wx[[f]], ref[[f]], tier = "display", precision = 2,
                label = paste(lbl, "Ranks", f))
  }
}


data(survey_data, envir = environment())
data(longitudinal_data_wide, envir = environment())


test_that("Test 1a: pairwise_wilcoxon trust triplet — N, Z, p match SPSS Wilcoxon refs", {
  fr <- survey_data |>
    friedman_test(trust_government, trust_media, trust_science)
  r  <- pairwise_wilcoxon(fr)

  expect_equal(nrow(r$comparisons), 3L)
  for (pair_ref in spss_values$trust_pairs) {
    check_pair(r$comparisons, survey_data, pair_ref, "1a")
  }

  # All p_adj should be effectively zero (extremely strong differences)
  expect_true(all(r$comparisons$p_adj < 1e-5))
})


test_that("Test 1b: pairwise_wilcoxon lifestyle triplet — matches SPSS", {
  fr <- survey_data |>
    friedman_test(life_satisfaction, environmental_concern, political_orientation)
  r  <- pairwise_wilcoxon(fr)

  expect_equal(nrow(r$comparisons), 3L)
  for (pair_ref in spss_values$lifestyle) {
    check_pair(r$comparisons, survey_data, pair_ref, "1b")
  }
})


test_that("Test 3: pairwise_wilcoxon trust triplet, grouped by region — matches SPSS", {
  grouped <- survey_data |> group_by(region)
  r <- pairwise_wilcoxon(
    friedman_test(grouped, trust_government, trust_media, trust_science)
  )

  expect_equal(nrow(r$comparisons), 6L)  # 3 pairs x 2 regions
  for (pair_ref in spss_values$region) {
    check_pair(r$comparisons, grouped, pair_ref, "3", by = "region")
  }
})


test_that("pairwise_wilcoxon Bonferroni adjustment ≥ raw p", {
  fr <- survey_data |>
    friedman_test(trust_government, trust_media, trust_science)
  r  <- pairwise_wilcoxon(fr)
  expect_true(all(r$comparisons$p_adj >= r$comparisons$p - 1e-15))
})


test_that("Test 5a: pairwise_wilcoxon longitudinal_data_wide — 6 pairs from 4 timepoints", {
  fr <- longitudinal_data_wide |>
    friedman_test(score_T1, score_T2, score_T3, score_T4)
  r  <- pairwise_wilcoxon(fr)

  expect_equal(nrow(r$comparisons), 6L)  # C(4,2)
  for (pair_ref in spss_values$longitudinal) {
    check_pair(r$comparisons, longitudinal_data_wide, pair_ref, "5a")
  }
})


test_that("Test 5b: pairwise_wilcoxon longitudinal, grouped by group — matches SPSS", {
  grouped <- longitudinal_data_wide |> group_by(group)
  r <- pairwise_wilcoxon(
    friedman_test(grouped, score_T1, score_T2, score_T3, score_T4)
  )

  expect_equal(nrow(r$comparisons), 12L)  # 6 pairs x 2 groups
  for (pair_ref in spss_values$longitudinal_by_group) {
    check_pair(r$comparisons, grouped, pair_ref, "5b", by = "group")
  }
})
