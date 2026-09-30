# =============================================================================
# oneway_anova — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::oneway_anova() against SPSS v29 ONEWAY procedure.
# Reference syntax:  tests/spss_reference/syntax/oneway_anova_test.sps
# Reference output:  tests/spss_reference/outputs/oneway_anova_output.txt
#
# Charter reference: .claude/VALIDATION_CHARTER.md
#
# Scenario coverage (per Charter §8) — every section of the reference output:
#   Scenario 1 — Unweighted / Ungrouped         (Tests 1a-c: life_sat / income / age by
#                education; 1d: three trust variables at once; 1e/1f: life_sat /
#                income by employment, 5 groups)
#   Scenario 2 — Weighted   / Ungrouped         (Tests 2a-f, as 1a-f)
#   Scenario 3 — Unweighted / Grouped by region (Tests 3a-c by education, 3d by employment)
#   Scenario 4 — Weighted   / Grouped by region (Tests 4a-d, as 3a-d)
#
# Plus auxiliary scenarios:
#   Test  5a     — the ONEWAY of the /POSTHOC=TUKEY run (Descriptives + ANOVA;
#                  its Multiple Comparisons table: test-tukey-test-spss-validation.R)
#   Tests 6a, 6b — alternative CI levels (90%, 99%)
#   Test  7      — multiple variables at once
#
# Not asserted: the Descriptives "Total" rows and Minimum/Maximum columns and
# the Brown-Forsythe rows (mariposa does not compute them).
#
# Tolerance tier assignments (per Charter §5):
#   N (unweighted)            — Spec (count, exact integer)
#   N (weighted, displayed)   — Display(0), tol ±0.5
#   Mean                      — Display(2) for 1-5 scales, Display(4) for income/age
#   SD                        — Display(3) for 1-5 scales, Display(5) for income/age
#   SE                        — Display(3) for 1-5 scales, Display(5) for income/age
#   CI bounds                 — Display(2) for 1-5 scales, Display(4) for income/age
#   ANOVA SS                  — Display(3)
#   ANOVA MS                  — Display(3)
#   F-statistic               — Display(3)
#   p-value                   — Display(3) p_value (one unit), sentinel "<.001"
#   df_between                — Spec (integer)
#   df_within (unweighted)    — Spec (integer, n - k)
#   df_within (weighted)      — Spec (integer, floor(sum(w)) - k as SPSS ONEWAY)
#   Welch F, df2              — Display(3)
#
# History (R/oneway_anova.R): the weighted df_within was once round(sum(w)) - k
# (same pattern as the t_test bug); it is floor(sum(w)) - k like SPSS ONEWAY.
# The weighted descriptives SE uses sum(w) (SPSS), asserted in Tests 2/4.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# =============================================================================
# SPSS REFERENCE VALUES (with citation comments per Charter §7)
# =============================================================================

spss_values <- list(

  # =========================================================================
  # SCENARIO 1: UNWEIGHTED / UNGROUPED
  # =========================================================================

  # ---- Test 1a: life_satisfaction by education --------------------------
  test_1a_life_by_education = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 809,  mean = 3.20, sd = 1.243, se = 0.044, ci_lower = 3.12, ci_upper = 3.29),   # oneway_anova_output.txt:13
      "Intermediate Secondary" = list(n = 618,  mean = 3.70, sd = 1.112, se = 0.045, ci_lower = 3.61, ci_upper = 3.79),   # oneway_anova_output.txt:14
      "Academic Secondary"     = list(n = 607,  mean = 3.85, sd = 0.998, se = 0.041, ci_lower = 3.77, ci_upper = 3.93),   # oneway_anova_output.txt:15
      "University"             = list(n = 387,  mean = 4.05, sd = 0.957, se = 0.049, ci_lower = 3.95, ci_upper = 4.14)    # oneway_anova_output.txt:16
    ),
    anova = list(
      ss_between = 247.347,    # oneway_anova_output.txt:23
      df_between = 3,          # oneway_anova_output.txt:23
      ms_between = 82.449,     # oneway_anova_output.txt:23
      f_stat     = 67.096,     # oneway_anova_output.txt:23
      p_value    = "<.001",    # oneway_anova_output.txt:23  (SPSS prints ".000")
      ss_within  = 2970.080,   # oneway_anova_output.txt:24
      df_within  = 2417,       # oneway_anova_output.txt:24
      ms_within  = 1.229,      # oneway_anova_output.txt:24
      ss_total   = 3217.428,   # oneway_anova_output.txt:25
      df_total   = 2420        # oneway_anova_output.txt:25
    ),
    welch = list(
      f_stat = 64.489,         # oneway_anova_output.txt:31
      df1    = 3,              # oneway_anova_output.txt:31
      df2    = 1229.456,       # oneway_anova_output.txt:31
      p      = "<.001"         # oneway_anova_output.txt:31
    ),
    var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
  ),

  # ---- Test 1b: income by education -------------------------------------
  test_1b_income_by_education = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 735, mean = 2759.0476, sd = 786.56752,  se = 29.01298, ci_lower = 2702.0893, ci_upper = 2816.0059),   # oneway_anova_output.txt:45
      "Intermediate Secondary" = list(n = 548, mean = 3592.5182, sd = 995.63887,  se = 42.53158, ci_lower = 3508.9730, ci_upper = 3676.0635),   # oneway_anova_output.txt:46
      "Academic Secondary"     = list(n = 548, mean = 4224.0876, sd = 1178.63526, se = 50.34880, ci_lower = 4125.1869, ci_upper = 4322.9883),   # oneway_anova_output.txt:47
      "University"             = list(n = 355, mean = 5337.1831, sd = 1660.95846, se = 88.15452, ci_lower = 5163.8107, ci_upper = 5510.5555)    # oneway_anova_output.txt:48
    ),
    anova = list(
      ss_between = 1752783281.470,  # oneway_anova_output.txt:55
      df_between = 3,                # oneway_anova_output.txt:55
      ms_between = 584261093.824,    # oneway_anova_output.txt:55
      f_stat     = 466.494,          # oneway_anova_output.txt:55
      p_value    = "<.001",          # oneway_anova_output.txt:55
      ss_within  = 2732847885.046,   # oneway_anova_output.txt:56
      df_within  = 2182,             # oneway_anova_output.txt:56
      ms_within  = 1252450.910,      # oneway_anova_output.txt:56
      ss_total   = 4485631166.515,   # oneway_anova_output.txt:57
      df_total   = 2185              # oneway_anova_output.txt:57
    ),
    welch = list(
      f_stat = 418.250,    # oneway_anova_output.txt:63
      df1    = 3,          # oneway_anova_output.txt:63
      df2    = 978.181,    # oneway_anova_output.txt:63
      p      = "<.001"     # oneway_anova_output.txt:63
    ),
    var_precision = list(mean = 4, sd = 5, se = 5, ci = 4)
  ),

  # ---- Test 1c: age by education ----------------------------------------
  test_1c_age_by_education = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 841, mean = 50.1165, sd = 16.86658, se = 0.58161, ci_lower = 48.9750, ci_upper = 51.2581),   # oneway_anova_output.txt:77
      "Intermediate Secondary" = list(n = 629, mean = 51.1924, sd = 17.23894, se = 0.68736, ci_lower = 49.8426, ci_upper = 52.5422),   # oneway_anova_output.txt:78
      "Academic Secondary"     = list(n = 631, mean = 51.2266, sd = 17.04358, se = 0.67849, ci_lower = 49.8942, ci_upper = 52.5590),   # oneway_anova_output.txt:79
      "University"             = list(n = 399, mean = 49.3784, sd = 16.64904, se = 0.83349, ci_lower = 47.7398, ci_upper = 51.0170)    # oneway_anova_output.txt:80
    ),
    anova = list(
      ss_between = 1254.099,     # oneway_anova_output.txt:87
      df_between = 3,             # oneway_anova_output.txt:87
      ms_between = 418.033,       # oneway_anova_output.txt:87
      f_stat     = 1.451,         # oneway_anova_output.txt:87
      p_value    = 0.226,         # oneway_anova_output.txt:87
      ss_within  = 718920.751,    # oneway_anova_output.txt:88
      df_within  = 2496,          # oneway_anova_output.txt:88
      ms_within  = 288.029,       # oneway_anova_output.txt:88
      ss_total   = 720174.850,    # oneway_anova_output.txt:89
      df_total   = 2499           # oneway_anova_output.txt:89
    ),
    welch = list(
      f_stat = 1.464,     # oneway_anova_output.txt:95
      df1    = 3,         # oneway_anova_output.txt:95
      df2    = 1228.520,  # oneway_anova_output.txt:95
      p      = 0.223      # oneway_anova_output.txt:95
    ),
    var_precision = list(mean = 4, sd = 5, se = 5, ci = 4)
  ),

  # =========================================================================
  # SCENARIO 2: WEIGHTED / UNGROUPED
  # =========================================================================

  # ---- Test 2a: life_satisfaction by education, weighted ----------------
  test_2a_life_by_education_weighted = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 816, mean = 3.21, sd = 1.243, se = 0.044, ci_lower = 3.12, ci_upper = 3.29),   # oneway_anova_output.txt:226
      "Intermediate Secondary" = list(n = 630, mean = 3.70, sd = 1.110, se = 0.044, ci_lower = 3.61, ci_upper = 3.78),   # oneway_anova_output.txt:227
      "Academic Secondary"     = list(n = 618, mean = 3.85, sd = 0.997, se = 0.040, ci_lower = 3.77, ci_upper = 3.93),   # oneway_anova_output.txt:228
      "University"             = list(n = 373, mean = 4.04, sd = 0.962, se = 0.050, ci_lower = 3.94, ci_upper = 4.14)    # oneway_anova_output.txt:229
    ),
    anova = list(
      ss_between = 241.130,    # oneway_anova_output.txt:236
      df_between = 3,           # oneway_anova_output.txt:236
      ms_between = 80.377,      # oneway_anova_output.txt:236
      f_stat     = 65.333,      # oneway_anova_output.txt:236
      p_value    = "<.001",     # oneway_anova_output.txt:236
      ss_within  = 2992.019,    # oneway_anova_output.txt:237
      df_within  = 2432,        # oneway_anova_output.txt:237  (SPSS rounds display)
      ms_within  = 1.230,       # oneway_anova_output.txt:237
      ss_total   = 3233.149,    # oneway_anova_output.txt:238
      df_total   = 2435         # oneway_anova_output.txt:238
    ),
    welch = list(
      f_stat = 62.636,    # oneway_anova_output.txt:244
      df1    = 3,         # oneway_anova_output.txt:244
      df2    = 1216.114,  # oneway_anova_output.txt:244
      p      = "<.001"    # oneway_anova_output.txt:244
    ),
    var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
  ),

  # ---- Test 2b: income by education, weighted ---------------------------
  test_2b_income_by_education_weighted = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 741, mean = 2759.2606, sd = 787.77480,  se = 28.93759, ci_lower = 2702.4511, ci_upper = 2816.0701),   # oneway_anova_output.txt:258
      "Intermediate Secondary" = list(n = 558, mean = 3590.2177, sd = 994.46762,  se = 42.08720, ci_lower = 3507.5488, ci_upper = 3672.8867),   # oneway_anova_output.txt:259
      "Academic Secondary"     = list(n = 558, mean = 4225.3255, sd = 1180.12280, se = 49.95106, ci_lower = 4127.2101, ci_upper = 4323.4409),   # oneway_anova_output.txt:260
      "University"             = list(n = 343, mean = 5331.3370, sd = 1664.10362, se = 89.80740, ci_lower = 5154.6932, ci_upper = 5507.9807)    # oneway_anova_output.txt:261
    ),
    anova = list(
      ss_between = 1726289512.505,   # oneway_anova_output.txt:268
      df_between = 3,                 # oneway_anova_output.txt:268
      ms_between = 575429837.502,     # oneway_anova_output.txt:268
      f_stat     = 462.115,           # oneway_anova_output.txt:268
      p_value    = "<.001",           # oneway_anova_output.txt:268
      ss_within  = 2734479255.792,    # oneway_anova_output.txt:269
      df_within  = 2196,              # oneway_anova_output.txt:269
      ms_within  = 1245209.133,       # oneway_anova_output.txt:269
      ss_total   = 4460768768.296,    # oneway_anova_output.txt:270
      df_total   = 2199               # oneway_anova_output.txt:270
    ),
    welch = list(
      f_stat = 413.705,    # oneway_anova_output.txt:276
      df1    = 3,          # oneway_anova_output.txt:276
      df2    = 969.609,    # oneway_anova_output.txt:276
      p      = "<.001"     # oneway_anova_output.txt:276
    ),
    var_precision = list(mean = 4, sd = 5, se = 5, ci = 4)
  ),

  # ---- Test 2c: age by education, weighted ------------------------------
  test_2c_age_by_education_weighted = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 848, mean = 50.1260, sd = 16.94224, se = 0.58176, ci_lower = 48.9842, ci_upper = 51.2679),   # oneway_anova_output.txt:290
      "Intermediate Secondary" = list(n = 641, mean = 50.9696, sd = 17.33275, se = 0.68466, ci_lower = 49.6252, ci_upper = 52.3141),   # oneway_anova_output.txt:291
      "Academic Secondary"     = list(n = 642, mean = 51.1916, sd = 17.17437, se = 0.67786, ci_lower = 49.8605, ci_upper = 52.5227),   # oneway_anova_output.txt:292
      "University"             = list(n = 385, mean = 49.4834, sd = 16.81672, se = 0.85687, ci_lower = 47.7987, ci_upper = 51.1682)    # oneway_anova_output.txt:293
    ),
    anova = list(
      ss_between = 964.501,        # oneway_anova_output.txt:300
      df_between = 3,               # oneway_anova_output.txt:300
      ms_between = 321.500,         # oneway_anova_output.txt:300
      f_stat     = 1.102,           # oneway_anova_output.txt:300
      p_value    = 0.347,           # oneway_anova_output.txt:300
      ss_within  = 733082.646,      # oneway_anova_output.txt:301
      df_within  = 2512,            # oneway_anova_output.txt:301
      ms_within  = 291.832,         # oneway_anova_output.txt:301
      ss_total   = 734047.147,      # oneway_anova_output.txt:302
      df_total   = 2515             # oneway_anova_output.txt:302
    ),
    welch = list(
      f_stat = 1.109,     # oneway_anova_output.txt:308
      df1    = 3,         # oneway_anova_output.txt:308
      df2    = 1215.388,  # oneway_anova_output.txt:308
      p      = 0.344      # oneway_anova_output.txt:308
    ),
    var_precision = list(mean = 4, sd = 5, se = 5, ci = 4)
  ),

  # =========================================================================
  # SCENARIO 3: UNWEIGHTED / GROUPED by region (East/West)
  # =========================================================================

  # ---- Test 3a: life_satisfaction by education, grouped by region -------
  test_3a_life_by_education_grouped = list(
    East = list(
      descriptives = list(
        "Basic Secondary"        = list(n = 161, mean = 3.30, sd = 1.337, se = 0.105, ci_lower = 3.10, ci_upper = 3.51),   # oneway_anova_output.txt:439
        "Intermediate Secondary" = list(n = 119, mean = 3.64, sd = 1.140, se = 0.105, ci_lower = 3.43, ci_upper = 3.85),   # oneway_anova_output.txt:440
        "Academic Secondary"     = list(n = 110, mean = 3.84, sd = 1.088, se = 0.104, ci_lower = 3.63, ci_upper = 4.04),   # oneway_anova_output.txt:441
        "University"             = list(n = 75,  mean = 3.95, sd = 1.025, se = 0.118, ci_lower = 3.71, ci_upper = 4.18)    # oneway_anova_output.txt:442
      ),
      anova = list(
        ss_between = 29.235,     # oneway_anova_output.txt:454
        df_between = 3,           # oneway_anova_output.txt:454
        ms_between = 9.745,       # oneway_anova_output.txt:454
        f_stat     = 6.950,       # oneway_anova_output.txt:454
        p_value    = "<.001",     # oneway_anova_output.txt:454
        ss_within  = 646.390,     # oneway_anova_output.txt:455
        df_within  = 461,         # oneway_anova_output.txt:455
        ms_within  = 1.402,       # oneway_anova_output.txt:455
        ss_total   = 675.626,     # oneway_anova_output.txt:456
        df_total   = 464          # oneway_anova_output.txt:456
      ),
      welch = list(
        f_stat = 6.682,    # oneway_anova_output.txt:465
        df1    = 3,        # oneway_anova_output.txt:465
        df2    = 233.415,  # oneway_anova_output.txt:465
        p      = "<.001"   # oneway_anova_output.txt:465
      ),
      var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
    ),
    West = list(
      descriptives = list(
        "Basic Secondary"        = list(n = 648, mean = 3.18, sd = 1.219, se = 0.048, ci_lower = 3.08, ci_upper = 3.27),   # oneway_anova_output.txt:444
        "Intermediate Secondary" = list(n = 499, mean = 3.72, sd = 1.106, se = 0.050, ci_lower = 3.62, ci_upper = 3.81),   # oneway_anova_output.txt:445
        "Academic Secondary"     = list(n = 497, mean = 3.86, sd = 0.978, se = 0.044, ci_lower = 3.77, ci_upper = 3.94),   # oneway_anova_output.txt:446
        "University"             = list(n = 312, mean = 4.07, sd = 0.939, se = 0.053, ci_lower = 3.97, ci_upper = 4.18)    # oneway_anova_output.txt:447
      ),
      anova = list(
        ss_between = 221.625,     # oneway_anova_output.txt:457
        df_between = 3,            # oneway_anova_output.txt:457
        ms_between = 73.875,       # oneway_anova_output.txt:457
        f_stat     = 62.153,       # oneway_anova_output.txt:457
        p_value    = "<.001",      # oneway_anova_output.txt:457
        ss_within  = 2320.132,     # oneway_anova_output.txt:458
        df_within  = 1952,         # oneway_anova_output.txt:458
        ms_within  = 1.189,        # oneway_anova_output.txt:458
        ss_total   = 2541.756,     # oneway_anova_output.txt:459
        df_total   = 1955          # oneway_anova_output.txt:459
      ),
      welch = list(
        f_stat = 59.852,    # oneway_anova_output.txt:467
        df1    = 3,         # oneway_anova_output.txt:467
        df2    = 992.905,   # oneway_anova_output.txt:467
        p      = "<.001"    # oneway_anova_output.txt:467
      ),
      var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
    )
  ),

  # =========================================================================
  # SCENARIO 4: WEIGHTED / GROUPED by region
  # =========================================================================

  # ---- Test 4a: life_satisfaction by education, weighted, grouped -------
  test_4a_life_by_education_weighted_grouped = list(
    East = list(
      descriptives = list(
        "Basic Secondary"        = list(n = 165, mean = 3.31, sd = 1.334, se = 0.104, ci_lower = 3.11, ci_upper = 3.52),   # oneway_anova_output.txt:611
        "Intermediate Secondary" = list(n = 127, mean = 3.64, sd = 1.134, se = 0.101, ci_lower = 3.44, ci_upper = 3.84),   # oneway_anova_output.txt:612
        "Academic Secondary"     = list(n = 118, mean = 3.82, sd = 1.100, se = 0.101, ci_lower = 3.62, ci_upper = 4.02),   # oneway_anova_output.txt:613
        "University"             = list(n = 78,  mean = 3.95, sd = 1.024, se = 0.116, ci_lower = 3.72, ci_upper = 4.18)    # oneway_anova_output.txt:614
      ),
      anova = list(
        ss_between = 28.855,     # oneway_anova_output.txt:626
        df_between = 3,           # oneway_anova_output.txt:626
        ms_between = 9.618,       # oneway_anova_output.txt:626
        f_stat     = 6.868,       # oneway_anova_output.txt:626
        p_value    = "<.001",     # oneway_anova_output.txt:626
        ss_within  = 676.447,     # oneway_anova_output.txt:627
        df_within  = 483,         # oneway_anova_output.txt:627
        ms_within  = 1.401,       # oneway_anova_output.txt:627
        ss_total   = 705.302,     # oneway_anova_output.txt:628
        df_total   = 486          # oneway_anova_output.txt:628
      ),
      welch = list(
        f_stat = 6.621,    # oneway_anova_output.txt:637
        df1    = 3,        # oneway_anova_output.txt:637
        df2    = 245.073,  # oneway_anova_output.txt:637
        p      = "<.001"   # oneway_anova_output.txt:637
      ),
      var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
    ),
    West = list(
      descriptives = list(
        "Basic Secondary"        = list(n = 650, mean = 3.18, sd = 1.219, se = 0.048, ci_lower = 3.09, ci_upper = 3.27),   # oneway_anova_output.txt:616
        "Intermediate Secondary" = list(n = 503, mean = 3.71, sd = 1.104, se = 0.049, ci_lower = 3.62, ci_upper = 3.81),   # oneway_anova_output.txt:617
        "Academic Secondary"     = list(n = 500, mean = 3.86, sd = 0.973, se = 0.043, ci_lower = 3.77, ci_upper = 3.94),   # oneway_anova_output.txt:618
        "University"             = list(n = 295, mean = 4.06, sd = 0.946, se = 0.055, ci_lower = 3.95, ci_upper = 4.17)    # oneway_anova_output.txt:619
      ),
      anova = list(
        ss_between = 216.014,     # oneway_anova_output.txt:629
        df_between = 3,            # oneway_anova_output.txt:629
        ms_between = 72.005,       # oneway_anova_output.txt:629
        f_stat     = 60.548,       # oneway_anova_output.txt:629
        p_value    = "<.001",      # oneway_anova_output.txt:629
        ss_within  = 2311.831,     # oneway_anova_output.txt:630
        df_within  = 1944,         # oneway_anova_output.txt:630  (display; internal non-integer)
        ms_within  = 1.189,        # oneway_anova_output.txt:630
        ss_total   = 2527.845,     # oneway_anova_output.txt:631
        df_total   = 1947          # oneway_anova_output.txt:631
      ),
      welch = list(
        f_stat = 58.124,    # oneway_anova_output.txt:639
        df1    = 3,         # oneway_anova_output.txt:639
        df2    = 967.680,   # oneway_anova_output.txt:639
        p      = "<.001"    # oneway_anova_output.txt:639
      ),
      var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
    )
  ),

  # =========================================================================
  # AUXILIARY: alternative CI levels and multi-variable
  # =========================================================================

  # ---- Test 6a: 90% CI for life_sat by education (unweighted) -----------
  # ANOVA table identical to Test 1a; only CI bounds change.
  test_6a_life_by_education_90ci = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 809, mean = 3.20, sd = 1.243, se = 0.044, ci_lower = 3.13, ci_upper = 3.28),   # oneway_anova_output.txt:828
      "Intermediate Secondary" = list(n = 618, mean = 3.70, sd = 1.112, se = 0.045, ci_lower = 3.63, ci_upper = 3.77),   # oneway_anova_output.txt:829
      "Academic Secondary"     = list(n = 607, mean = 3.85, sd = 0.998, se = 0.041, ci_lower = 3.79, ci_upper = 3.92),   # oneway_anova_output.txt:830
      "University"             = list(n = 387, mean = 4.05, sd = 0.957, se = 0.049, ci_lower = 3.97, ci_upper = 4.13)    # oneway_anova_output.txt:831
    ),
    anova = list(
      ss_between = 247.347, df_between = 3, ms_between = 82.449, f_stat = 67.096, p_value = "<.001",  # oneway_anova_output.txt:838
      ss_within  = 2970.080, df_within  = 2417, ms_within  = 1.229,  # oneway_anova_output.txt:839
      ss_total   = 3217.428, df_total   = 2420   # oneway_anova_output.txt:840
    ),
    var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
  ),

  # ---- Test 6b: 99% CI for life_sat by education (unweighted) -----------
  test_6b_life_by_education_99ci = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 809, mean = 3.20, sd = 1.243, se = 0.044, ci_lower = 3.09, ci_upper = 3.32),   # oneway_anova_output.txt:852
      "Intermediate Secondary" = list(n = 618, mean = 3.70, sd = 1.112, se = 0.045, ci_lower = 3.59, ci_upper = 3.82),   # oneway_anova_output.txt:853
      "Academic Secondary"     = list(n = 607, mean = 3.85, sd = 0.998, se = 0.041, ci_lower = 3.75, ci_upper = 3.96),   # oneway_anova_output.txt:854
      "University"             = list(n = 387, mean = 4.05, sd = 0.957, se = 0.049, ci_lower = 3.92, ci_upper = 4.17)    # oneway_anova_output.txt:855
    ),
    anova = list(
      ss_between = 247.347, df_between = 3, ms_between = 82.449, f_stat = 67.096, p_value = "<.001",  # oneway_anova_output.txt:862
      ss_within  = 2970.080, df_within  = 2417, ms_within  = 1.229,  # oneway_anova_output.txt:863
      ss_total   = 3217.428, df_total   = 2420   # oneway_anova_output.txt:864
    ),
    var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
  ),

  # ---- Test 7: multiple variables (selected additions, not duplicates) -
  # The first three variables (life_sat, income, age) duplicate Tests
  # 1a-c (already validated). Test 7 adds political_orientation and
  # environmental_concern: their descriptives, ANOVA and Welch rows.
  test_7_political = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 768, mean = 2.72, sd = 1.074, se = 0.039, ci_lower = 2.65, ci_upper = 2.80),  # oneway_anova_output.txt:890
      "Intermediate Secondary" = list(n = 582, mean = 2.75, sd = 1.078, se = 0.045, ci_lower = 2.66, ci_upper = 2.84),  # oneway_anova_output.txt:891
      "Academic Secondary"     = list(n = 582, mean = 2.74, sd = 1.096, se = 0.045, ci_lower = 2.65, ci_upper = 2.83),  # oneway_anova_output.txt:892
      "University"             = list(n = 367, mean = 2.65, sd = 1.108, se = 0.058, ci_lower = 2.53, ci_upper = 2.76)   # oneway_anova_output.txt:893
    ),
    var_precision = list(mean = 2, sd = 3, se = 3, ci = 2),
    anova = list(
      ss_between = 2.632,         # oneway_anova_output.txt:913
      df_between = 3,              # oneway_anova_output.txt:913
      ms_between = 0.877,          # oneway_anova_output.txt:913
      f_stat     = 0.744,          # oneway_anova_output.txt:913
      p_value    = 0.526,          # oneway_anova_output.txt:913
      ss_within  = 2707.203,       # oneway_anova_output.txt:914
      df_within  = 2295,           # oneway_anova_output.txt:914
      ms_within  = 1.180,          # oneway_anova_output.txt:914
      ss_total   = 2709.836,       # oneway_anova_output.txt:915
      df_total   = 2298            # oneway_anova_output.txt:915
    ),
    welch = list(
      f_stat = 0.722,     # oneway_anova_output.txt:926
      df1    = 3,         # oneway_anova_output.txt:926
      df2    = 1124.261,  # oneway_anova_output.txt:926
      p      = 0.539      # oneway_anova_output.txt:926
    )
  ),
  test_7_environmental = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 808, mean = 3.60, sd = 1.188, se = 0.042, ci_lower = 3.52, ci_upper = 3.68),  # oneway_anova_output.txt:895
      "Intermediate Secondary" = list(n = 606, mean = 3.55, sd = 1.218, se = 0.049, ci_lower = 3.45, ci_upper = 3.65),  # oneway_anova_output.txt:896
      "Academic Secondary"     = list(n = 606, mean = 3.53, sd = 1.158, se = 0.047, ci_lower = 3.43, ci_upper = 3.62),  # oneway_anova_output.txt:897
      "University"             = list(n = 380, mean = 3.63, sd = 1.223, se = 0.063, ci_lower = 3.51, ci_upper = 3.75)   # oneway_anova_output.txt:898
    ),
    var_precision = list(mean = 2, sd = 3, se = 3, ci = 2),
    anova = list(
      ss_between = 3.550,          # oneway_anova_output.txt:916
      df_between = 3,               # oneway_anova_output.txt:916
      ms_between = 1.183,           # oneway_anova_output.txt:916
      f_stat     = 0.830,           # oneway_anova_output.txt:916
      p_value    = 0.477,           # oneway_anova_output.txt:916
      ss_within  = 3413.690,        # oneway_anova_output.txt:917
      df_within  = 2396,            # oneway_anova_output.txt:917
      ms_within  = 1.425,           # oneway_anova_output.txt:917
      ss_total   = 3417.240,        # oneway_anova_output.txt:918
      df_total   = 2399             # oneway_anova_output.txt:918
    ),
    welch = list(
      f_stat = 0.832,     # oneway_anova_output.txt:927
      df1    = 3,         # oneway_anova_output.txt:927
      df2    = 1168.306,  # oneway_anova_output.txt:927
      p      = 0.476      # oneway_anova_output.txt:927
    )
  )
)


# =============================================================================
# SPSS REFERENCE VALUES — remaining sections (0.7.4 audit)
# =============================================================================
# Same layout as above. Several dependent variables in one ONEWAY (1d/2d) are
# keyed by variable, SPLIT FILE sections by region. Test 5a is the ONEWAY of
# the /POSTHOC=TUKEY run (no Robust Tests requested).

spss_values$test_1d_trust_by_education <- list(
  trust_government = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 791, mean = 2.65, sd = 1.149, se = 0.041, ci_lower = 2.57, ci_upper = 2.73),  # oneway_anova_output.txt:108
      "Intermediate Secondary" = list(n = 592, mean = 2.58, sd = 1.175, se = 0.048, ci_lower = 2.49, ci_upper = 2.68),  # oneway_anova_output.txt:109
      "Academic Secondary"     = list(n = 595, mean = 2.61, sd = 1.183, se = 0.049, ci_lower = 2.51, ci_upper = 2.71),  # oneway_anova_output.txt:110
      "University"             = list(n = 376, mean = 2.64, sd = 1.146, se = 0.059, ci_lower = 2.53, ci_upper = 2.76)   # oneway_anova_output.txt:111
    ),
    anova = list(
      ss_between = 1.753, df_between = 3, ms_between = 0.584, f_stat = 0.431, p_value = 0.731,  # oneway_anova_output.txt:127
      ss_within  = 3182.484, df_within  = 2350, ms_within  = 1.354,  # oneway_anova_output.txt:128
      ss_total   = 3184.237, df_total   = 2353   # oneway_anova_output.txt:129
    ),
    welch = list(f_stat = 0.431, df1 = 3, df2 = 1155.612, p = 0.731),  # oneway_anova_output.txt:140
    var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
  ),
  trust_media = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 797, mean = 2.44, sd = 1.163, se = 0.041, ci_lower = 2.36, ci_upper = 2.52),  # oneway_anova_output.txt:113
      "Intermediate Secondary" = list(n = 594, mean = 2.50, sd = 1.188, se = 0.049, ci_lower = 2.40, ci_upper = 2.60),  # oneway_anova_output.txt:114
      "Academic Secondary"     = list(n = 599, mean = 2.46, sd = 1.149, se = 0.047, ci_lower = 2.37, ci_upper = 2.56),  # oneway_anova_output.txt:115
      "University"             = list(n = 377, mean = 2.38, sd = 1.149, se = 0.059, ci_lower = 2.26, ci_upper = 2.49)   # oneway_anova_output.txt:116
    ),
    anova = list(
      ss_between = 3.662, df_between = 3, ms_between = 1.221, f_stat = 0.902, p_value = 0.439,  # oneway_anova_output.txt:130
      ss_within  = 3198.645, df_within  = 2363, ms_within  = 1.354,  # oneway_anova_output.txt:131
      ss_total   = 3202.308, df_total   = 2366   # oneway_anova_output.txt:132
    ),
    welch = list(f_stat = 0.901, df1 = 3, df2 = 1161.543, p = 0.440),  # oneway_anova_output.txt:142
    var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
  ),
  trust_science = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 807, mean = 3.64, sd = 1.051, se = 0.037, ci_lower = 3.57, ci_upper = 3.71),  # oneway_anova_output.txt:118
      "Intermediate Secondary" = list(n = 610, mean = 3.61, sd = 1.005, se = 0.041, ci_lower = 3.53, ci_upper = 3.69),  # oneway_anova_output.txt:119
      "Academic Secondary"     = list(n = 597, mean = 3.69, sd = 1.040, se = 0.043, ci_lower = 3.60, ci_upper = 3.77),  # oneway_anova_output.txt:120
      "University"             = list(n = 384, mean = 3.63, sd = 0.996, se = 0.051, ci_lower = 3.53, ci_upper = 3.73)   # oneway_anova_output.txt:121
    ),
    anova = list(
      ss_between = 1.918, df_between = 3, ms_between = 0.639, f_stat = 0.605, p_value = 0.612,  # oneway_anova_output.txt:133
      ss_within  = 2529.658, df_within  = 2394, ms_within  = 1.057,  # oneway_anova_output.txt:134
      ss_total   = 2531.576, df_total   = 2397   # oneway_anova_output.txt:135
    ),
    welch = list(f_stat = 0.605, df1 = 3, df2 = 1185.828, p = 0.612),  # oneway_anova_output.txt:144
    var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
  )
)

spss_values$test_1e_life_by_employment <- list(
  descriptives = list(
    "Student"    = list(n = 75, mean = 3.95, sd = 0.943, se = 0.109, ci_lower = 3.73, ci_upper = 4.16),  # oneway_anova_output.txt:158
    "Employed"   = list(n = 1545, mean = 3.63, sd = 1.173, se = 0.030, ci_lower = 3.57, ci_upper = 3.68),  # oneway_anova_output.txt:159
    "Unemployed" = list(n = 178, mean = 3.66, sd = 1.099, se = 0.082, ci_lower = 3.50, ci_upper = 3.83),  # oneway_anova_output.txt:160
    "Retired"    = list(n = 511, mean = 3.56, sd = 1.146, se = 0.051, ci_lower = 3.46, ci_upper = 3.66),  # oneway_anova_output.txt:161
    "Other"      = list(n = 112, mean = 3.71, sd = 1.094, se = 0.103, ci_lower = 3.51, ci_upper = 3.92)   # oneway_anova_output.txt:162
  ),
  anova = list(
    ss_between = 11.197, df_between = 4, ms_between = 2.799, f_stat = 2.109, p_value = 0.077,  # oneway_anova_output.txt:169
    ss_within  = 3206.230, df_within  = 2416, ms_within  = 1.327,  # oneway_anova_output.txt:170
    ss_total   = 3217.428, df_total   = 2420   # oneway_anova_output.txt:171
  ),
  welch = list(f_stat = 2.809, df1 = 4, df2 = 301.749, p = 0.026),  # oneway_anova_output.txt:177
  var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
)

spss_values$test_1f_income_by_employment <- list(
  descriptives = list(
    "Student"    = list(n = 65, mean = 4632.3077, sd = 1397.77811, se = 173.37304, ci_lower = 4285.9552, ci_upper = 4978.6602),  # oneway_anova_output.txt:191
    "Employed"   = list(n = 1390, mean = 3724.6763, sd = 1446.05011, se = 38.78608, ci_lower = 3648.5906, ci_upper = 3800.7619),  # oneway_anova_output.txt:192
    "Unemployed" = list(n = 159, mean = 3644.6541, sd = 1220.58545, se = 96.79872, ci_lower = 3453.4677, ci_upper = 3835.8405),  # oneway_anova_output.txt:193
    "Retired"    = list(n = 471, mean = 3746.2845, sd = 1435.81173, se = 66.15871, ci_lower = 3616.2810, ci_upper = 3876.2880),  # oneway_anova_output.txt:194
    "Other"      = list(n = 101, mean = 3799.0099, sd = 1408.22548, se = 140.12367, ci_lower = 3521.0085, ci_upper = 4077.0113)   # oneway_anova_output.txt:195
  ),
  anova = list(
    ss_between = 53471553.510, df_between = 4, ms_between = 13367888.377, f_stat = 6.578, p_value = "<.001",  # oneway_anova_output.txt:202
    ss_within  = 4432159613.005, df_within  = 2181, ms_within  = 2032168.553,  # oneway_anova_output.txt:203
    ss_total   = 4485631166.515, df_total   = 2185   # oneway_anova_output.txt:204
  ),
  welch = list(f_stat = 6.855, df1 = 4, df2 = 263.672, p = "<.001"),  # oneway_anova_output.txt:210
  var_precision = list(mean = 4, sd = 5, se = 5, ci = 4)
)

spss_values$test_2d_trust_by_education_weighted <- list(
  trust_government = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 799, mean = 2.65, sd = 1.146, se = 0.041, ci_lower = 2.57, ci_upper = 2.73),  # oneway_anova_output.txt:321
      "Intermediate Secondary" = list(n = 603, mean = 2.58, sd = 1.178, se = 0.048, ci_lower = 2.49, ci_upper = 2.68),  # oneway_anova_output.txt:322
      "Academic Secondary"     = list(n = 606, mean = 2.61, sd = 1.188, se = 0.048, ci_lower = 2.51, ci_upper = 2.70),  # oneway_anova_output.txt:323
      "University"             = list(n = 363, mean = 2.64, sd = 1.139, se = 0.060, ci_lower = 2.52, ci_upper = 2.76)   # oneway_anova_output.txt:324
    ),
    anova = list(
      ss_between = 1.649, df_between = 3, ms_between = 0.550, f_stat = 0.406, p_value = 0.749,  # oneway_anova_output.txt:340
      ss_within  = 3206.580, df_within  = 2366, ms_within  = 1.355,  # oneway_anova_output.txt:341
      ss_total   = 3208.229, df_total   = 2369   # oneway_anova_output.txt:342
    ),
    welch = list(f_stat = 0.406, df1 = 3, df2 = 1145.159, p = 0.749),  # oneway_anova_output.txt:353
    var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
  ),
  trust_media = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 803, mean = 2.44, sd = 1.166, se = 0.041, ci_lower = 2.36, ci_upper = 2.53),  # oneway_anova_output.txt:326
      "Intermediate Secondary" = list(n = 606, mean = 2.50, sd = 1.184, se = 0.048, ci_lower = 2.41, ci_upper = 2.60),  # oneway_anova_output.txt:327
      "Academic Secondary"     = list(n = 609, mean = 2.46, sd = 1.155, se = 0.047, ci_lower = 2.37, ci_upper = 2.55),  # oneway_anova_output.txt:328
      "University"             = list(n = 364, mean = 2.39, sd = 1.156, se = 0.061, ci_lower = 2.27, ci_upper = 2.51)   # oneway_anova_output.txt:329
    ),
    anova = list(
      ss_between = 3.233, df_between = 3, ms_between = 1.078, f_stat = 0.792, p_value = 0.498,  # oneway_anova_output.txt:343
      ss_within  = 3235.507, df_within  = 2377, ms_within  = 1.361,  # oneway_anova_output.txt:344
      ss_total   = 3238.740, df_total   = 2380   # oneway_anova_output.txt:345
    ),
    welch = list(f_stat = 0.788, df1 = 3, df2 = 1149.996, p = 0.501),  # oneway_anova_output.txt:355
    var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
  ),
  trust_science = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 813, mean = 3.64, sd = 1.048, se = 0.037, ci_lower = 3.57, ci_upper = 3.71),  # oneway_anova_output.txt:331
      "Intermediate Secondary" = list(n = 622, mean = 3.61, sd = 1.010, se = 0.040, ci_lower = 3.53, ci_upper = 3.69),  # oneway_anova_output.txt:332
      "Academic Secondary"     = list(n = 608, mean = 3.68, sd = 1.037, se = 0.042, ci_lower = 3.60, ci_upper = 3.77),  # oneway_anova_output.txt:333
      "University"             = list(n = 370, mean = 3.62, sd = 0.994, se = 0.052, ci_lower = 3.52, ci_upper = 3.72)   # oneway_anova_output.txt:334
    ),
    anova = list(
      ss_between = 1.777, df_between = 3, ms_between = 0.592, f_stat = 0.561, p_value = 0.641,  # oneway_anova_output.txt:346
      ss_within  = 2543.405, df_within  = 2409, ms_within  = 1.056,  # oneway_anova_output.txt:347
      ss_total   = 2545.183, df_total   = 2412   # oneway_anova_output.txt:348
    ),
    welch = list(f_stat = 0.563, df1 = 3, df2 = 1174.013, p = 0.639),  # oneway_anova_output.txt:357
    var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
  )
)

spss_values$test_2e_life_by_employment_weighted <- list(
  descriptives = list(
    "Student"    = list(n = 77, mean = 3.93, sd = 0.949, se = 0.108, ci_lower = 3.71, ci_upper = 4.14),  # oneway_anova_output.txt:371
    "Employed"   = list(n = 1548, mean = 3.62, sd = 1.171, se = 0.030, ci_lower = 3.56, ci_upper = 3.68),  # oneway_anova_output.txt:372
    "Unemployed" = list(n = 180, mean = 3.67, sd = 1.095, se = 0.082, ci_lower = 3.50, ci_upper = 3.83),  # oneway_anova_output.txt:373
    "Retired"    = list(n = 519, mean = 3.55, sd = 1.149, se = 0.050, ci_lower = 3.45, ci_upper = 3.65),  # oneway_anova_output.txt:374
    "Other"      = list(n = 112, mean = 3.72, sd = 1.093, se = 0.103, ci_lower = 3.52, ci_upper = 3.92)   # oneway_anova_output.txt:375
  ),
  anova = list(
    ss_between = 11.176, df_between = 4, ms_between = 2.794, f_stat = 2.108, p_value = 0.077,  # oneway_anova_output.txt:382
    ss_within  = 3221.973, df_within  = 2431, ms_within  = 1.325,  # oneway_anova_output.txt:383
    ss_total   = 3233.149, df_total   = 2435   # oneway_anova_output.txt:384
  ),
  welch = list(f_stat = 2.737, df1 = 4, df2 = 305.915, p = 0.029),  # oneway_anova_output.txt:390
  var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
)

spss_values$test_2f_income_by_employment_weighted <- list(
  descriptives = list(
    "Student"    = list(n = 66, mean = 4648.7778, sd = 1388.26184, se = 170.31453, ci_lower = 4308.6796, ci_upper = 4988.8759),  # oneway_anova_output.txt:404
    "Employed"   = list(n = 1393, mean = 3709.5855, sd = 1434.35369, se = 38.42651, ci_lower = 3634.2054, ci_upper = 3784.9656),  # oneway_anova_output.txt:405
    "Unemployed" = list(n = 161, mean = 3634.6663, sd = 1196.23529, se = 94.31317, ci_lower = 3448.4060, ci_upper = 3820.9267),  # oneway_anova_output.txt:406
    "Retired"    = list(n = 479, mean = 3742.6560, sd = 1433.91694, se = 65.52860, ci_lower = 3613.8962, ci_upper = 3871.4158),  # oneway_anova_output.txt:407
    "Other"      = list(n = 101, mean = 3784.2688, sd = 1413.51332, se = 140.32491, ci_lower = 3505.8840, ci_upper = 4062.6535)   # oneway_anova_output.txt:408
  ),
  anova = list(
    ss_between = 58127400.293, df_between = 4, ms_between = 14531850.073, f_stat = 7.245, p_value = "<.001",  # oneway_anova_output.txt:415
    ss_within  = 4402641368.002, df_within  = 2195, ms_within  = 2005759.165,  # oneway_anova_output.txt:416
    ss_total   = 4460768768.296, df_total   = 2199   # oneway_anova_output.txt:417
  ),
  welch = list(f_stat = 7.555, df1 = 4, df2 = 267.828, p = "<.001"),  # oneway_anova_output.txt:423
  var_precision = list(mean = 4, sd = 5, se = 5, ci = 4)
)

spss_values$test_3b_income_by_education_grouped <- list(
  East = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 154, mean = 2897.4026, sd = 782.56726, se = 63.06107, ci_lower = 2772.8198, ci_upper = 3021.9854),  # oneway_anova_output.txt:481
      "Intermediate Secondary" = list(n = 107, mean = 3609.3458, sd = 981.29032, se = 94.86492, ci_lower = 3421.2669, ci_upper = 3797.4247),  # oneway_anova_output.txt:482
      "Academic Secondary"     = list(n = 98, mean = 4200.0000, sd = 1264.66653, se = 127.75061, ci_lower = 3946.4504, ci_upper = 4453.5496),  # oneway_anova_output.txt:483
      "University"             = list(n = 70, mean = 5225.7143, sd = 1641.72812, se = 196.22404, ci_lower = 4834.2580, ci_upper = 5617.1705)   # oneway_anova_output.txt:484
    ),
    anova = list(
      ss_between = 286346600.540, df_between = 3, ms_between = 95448866.847, f_stat = 75.558, p_value = "<.001",  # oneway_anova_output.txt:496
      ss_within  = 536883329.531, df_within  = 425, ms_within  = 1263254.893,  # oneway_anova_output.txt:497
      ss_total   = 823229930.070, df_total   = 428   # oneway_anova_output.txt:498
    ),
    welch = list(f_stat = 64.226, df1 = 3, df2 = 183.963, p = "<.001"),  # oneway_anova_output.txt:507
    var_precision = list(mean = 4, sd = 5, se = 5, ci = 4)
  ),
  West = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 581, mean = 2722.3752, sd = 784.20740, se = 32.53441, ci_lower = 2658.4756, ci_upper = 2786.2748),  # oneway_anova_output.txt:486
      "Intermediate Secondary" = list(n = 441, mean = 3588.4354, sd = 1000.14888, se = 47.62614, ci_lower = 3494.8324, ci_upper = 3682.0384),  # oneway_anova_output.txt:487
      "Academic Secondary"     = list(n = 450, mean = 4229.3333, sd = 1160.47836, se = 54.70547, ci_lower = 4121.8228, ci_upper = 4336.8439),  # oneway_anova_output.txt:488
      "University"             = list(n = 285, mean = 5364.5614, sd = 1667.36706, se = 98.76630, ci_lower = 5170.1545, ci_upper = 5558.9683)   # oneway_anova_output.txt:489
    ),
    anova = list(
      ss_between = 1471355044.129, df_between = 3, ms_between = 490451681.376, f_stat = 392.398, p_value = "<.001",  # oneway_anova_output.txt:499
      ss_within  = 2191045012.787, df_within  = 1753, ms_within  = 1249883.065,  # oneway_anova_output.txt:500
      ss_total   = 3662400056.916, df_total   = 1756   # oneway_anova_output.txt:501
    ),
    welch = list(f_stat = 356.580, df1 = 3, df2 = 790.232, p = "<.001"),  # oneway_anova_output.txt:509
    var_precision = list(mean = 4, sd = 5, se = 5, ci = 4)
  )
)

spss_values$test_3c_age_by_education_grouped <- list(
  East = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 170, mean = 50.9529, sd = 16.76281, se = 1.28565, ci_lower = 48.4149, ci_upper = 53.4909),  # oneway_anova_output.txt:523
      "Intermediate Secondary" = list(n = 121, mean = 50.9091, sd = 18.19295, se = 1.65390, ci_lower = 47.6345, ci_upper = 54.1837),  # oneway_anova_output.txt:524
      "Academic Secondary"     = list(n = 115, mean = 54.1739, sd = 17.59349, se = 1.64060, ci_lower = 50.9239, ci_upper = 57.4239),  # oneway_anova_output.txt:525
      "University"             = list(n = 79, mean = 51.9494, sd = 17.36479, se = 1.95369, ci_lower = 48.0599, ci_upper = 55.8389)   # oneway_anova_output.txt:526
    ),
    anova = list(
      ss_between = 865.612, df_between = 3, ms_between = 288.537, f_stat = 0.951, p_value = 0.416,  # oneway_anova_output.txt:538
      ss_within  = 146011.943, df_within  = 481, ms_within  = 303.559,  # oneway_anova_output.txt:539
      ss_total   = 146877.555, df_total   = 484   # oneway_anova_output.txt:540
    ),
    welch = list(f_stat = 0.935, df1 = 3, df2 = 233.459, p = 0.425),  # oneway_anova_output.txt:549
    var_precision = list(mean = 4, sd = 5, se = 5, ci = 4)
  ),
  West = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 671, mean = 49.9046, sd = 16.89864, se = 0.65236, ci_lower = 48.6237, ci_upper = 51.1855),  # oneway_anova_output.txt:528
      "Intermediate Secondary" = list(n = 508, mean = 51.2598, sd = 17.02184, se = 0.75522, ci_lower = 49.7761, ci_upper = 52.7436),  # oneway_anova_output.txt:529
      "Academic Secondary"     = list(n = 516, mean = 50.5698, sd = 16.86592, se = 0.74248, ci_lower = 49.1111, ci_upper = 52.0284),  # oneway_anova_output.txt:530
      # mean 15598 / 320 = 48.74375 exactly, printed half up by SPSS
      "University"             = list(n = 320, mean = 48.7438, sd = 16.43368, se = 0.91867, ci_lower = 46.9363, ci_upper = 50.5512)   # oneway_anova_output.txt:531
    ),
    anova = list(
      ss_between = 1376.231, df_between = 3, ms_between = 458.744, f_stat = 1.616, p_value = 0.184,  # oneway_anova_output.txt:541
      ss_within  = 570875.072, df_within  = 2011, ms_within  = 283.876,  # oneway_anova_output.txt:542
      ss_total   = 572251.303, df_total   = 2014   # oneway_anova_output.txt:543
    ),
    welch = list(f_stat = 1.642, df1 = 3, df2 = 992.155, p = 0.178),  # oneway_anova_output.txt:551
    var_precision = list(mean = 4, sd = 5, se = 5, ci = 4)
  )
)

spss_values$test_3d_life_by_employment_grouped <- list(
  East = list(
    descriptives = list(
      "Student"    = list(n = 10, mean = 3.70, sd = 1.418, se = 0.448, ci_lower = 2.69, ci_upper = 4.71),  # oneway_anova_output.txt:565
      "Employed"   = list(n = 299, mean = 3.63, sd = 1.198, se = 0.069, ci_lower = 3.49, ci_upper = 3.76),  # oneway_anova_output.txt:566
      "Unemployed" = list(n = 30, mean = 3.63, sd = 1.351, se = 0.247, ci_lower = 3.13, ci_upper = 4.14),  # oneway_anova_output.txt:567
      "Retired"    = list(n = 105, mean = 3.51, sd = 1.186, se = 0.116, ci_lower = 3.28, ci_upper = 3.74),  # oneway_anova_output.txt:568
      "Other"      = list(n = 21, mean = 4.00, sd = 1.140, se = 0.249, ci_lower = 3.48, ci_upper = 4.52)   # oneway_anova_output.txt:569
    ),
    anova = list(
      ss_between = 4.284, df_between = 4, ms_between = 1.071, f_stat = 0.734, p_value = 0.569,  # oneway_anova_output.txt:582
      ss_within  = 671.342, df_within  = 460, ms_within  = 1.459,  # oneway_anova_output.txt:583
      ss_total   = 675.626, df_total   = 464   # oneway_anova_output.txt:584
    ),
    welch = list(f_stat = 0.766, df1 = 4, df2 = 42.132, p = 0.554),  # oneway_anova_output.txt:593
    var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
  ),
  West = list(
    descriptives = list(
      "Student"    = list(n = 65, mean = 3.98, sd = 0.857, se = 0.106, ci_lower = 3.77, ci_upper = 4.20),  # oneway_anova_output.txt:571
      "Employed"   = list(n = 1246, mean = 3.63, sd = 1.167, se = 0.033, ci_lower = 3.56, ci_upper = 3.69),  # oneway_anova_output.txt:572
      "Unemployed" = list(n = 148, mean = 3.67, sd = 1.046, se = 0.086, ci_lower = 3.50, ci_upper = 3.84),  # oneway_anova_output.txt:573
      "Retired"    = list(n = 406, mean = 3.57, sd = 1.137, se = 0.056, ci_lower = 3.46, ci_upper = 3.68),  # oneway_anova_output.txt:574
      "Other"      = list(n = 91, mean = 3.65, sd = 1.079, se = 0.113, ci_lower = 3.42, ci_upper = 3.87)   # oneway_anova_output.txt:575
    ),
    anova = list(
      ss_between = 9.961, df_between = 4, ms_between = 2.490, f_stat = 1.919, p_value = 0.105,  # oneway_anova_output.txt:585
      ss_within  = 2531.795, df_within  = 1951, ms_within  = 1.298,  # oneway_anova_output.txt:586
      ss_total   = 2541.756, df_total   = 1955   # oneway_anova_output.txt:587
    ),
    welch = list(f_stat = 3.075, df1 = 4, df2 = 256.259, p = 0.017),  # oneway_anova_output.txt:595
    var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
  )
)

spss_values$test_4b_income_by_education_weighted_grouped <- list(
  East = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 158, mean = 2899.6797, sd = 789.33868, se = 62.73662, ci_lower = 2775.7648, ci_upper = 3023.5945),  # oneway_anova_output.txt:653
      "Intermediate Secondary" = list(n = 114, mean = 3626.2774, sd = 970.29703, se = 90.99553, ci_lower = 3445.9937, ci_upper = 3806.5611),  # oneway_anova_output.txt:654
      "Academic Secondary"     = list(n = 105, mean = 4190.4290, sd = 1269.96243, se = 123.91044, ci_lower = 3944.7111, ci_upper = 4436.1469),  # oneway_anova_output.txt:655
      "University"             = list(n = 72, mean = 5229.8556, sd = 1661.94283, se = 195.25394, ci_lower = 4840.5727, ci_upper = 5619.1385)   # oneway_anova_output.txt:656
    ),
    anova = list(
      ss_between = 295185395.664, df_between = 3, ms_between = 98395131.888, f_stat = 76.917, p_value = "<.001",  # oneway_anova_output.txt:668
      ss_within  = 569260669.071, df_within  = 445, ms_within  = 1279237.459,  # oneway_anova_output.txt:669
      ss_total   = 864446064.734, df_total   = 448   # oneway_anova_output.txt:670
    ),
    welch = list(f_stat = 65.650, df1 = 3, df2 = 194.051, p = "<.001"),  # oneway_anova_output.txt:679
    var_precision = list(mean = 4, sd = 5, se = 5, ci = 4)
  ),
  West = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 583, mean = 2721.1199, sd = 783.68799, se = 32.46252, ci_lower = 2657.3619, ci_upper = 2784.8779),  # oneway_anova_output.txt:658
      "Intermediate Secondary" = list(n = 445, mean = 3580.9961, sd = 1001.41978, se = 47.49240, ci_lower = 3487.6581, ci_upper = 3674.3342),  # oneway_anova_output.txt:659
      "Academic Secondary"     = list(n = 453, mean = 4233.4151, sd = 1159.64176, se = 54.47719, ci_lower = 4126.3552, ci_upper = 4340.4750),  # oneway_anova_output.txt:660
      "University"             = list(n = 271, mean = 5358.4769, sd = 1666.70330, se = 101.26360, ci_lower = 5159.1099, ci_upper = 5557.8439)   # oneway_anova_output.txt:661
    ),
    anova = list(
      ss_between = 1436187402.137, df_between = 3, ms_between = 478729134.046, f_stat = 387.201, p_value = "<.001",  # oneway_anova_output.txt:671
      ss_within  = 2159960586.534, df_within  = 1747, ms_within  = 1236382.706,  # oneway_anova_output.txt:672
      ss_total   = 3596147988.671, df_total   = 1750   # oneway_anova_output.txt:673
    ),
    welch = list(f_stat = 351.163, df1 = 3, df2 = 771.507, p = "<.001"),  # oneway_anova_output.txt:681
    var_precision = list(mean = 4, sd = 5, se = 5, ci = 4)
  )
)

spss_values$test_4c_age_by_education_weighted_grouped <- list(
  East = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 175, mean = 51.2003, sd = 16.88029, se = 1.27606, ci_lower = 48.6817, ci_upper = 53.7189),  # oneway_anova_output.txt:695
      "Intermediate Secondary" = list(n = 129, mean = 51.0399, sd = 18.37052, se = 1.61840, ci_lower = 47.8376, ci_upper = 54.2422),  # oneway_anova_output.txt:696
      "Academic Secondary"     = list(n = 123, mean = 54.7787, sd = 17.60817, se = 1.58752, ci_lower = 51.6360, ci_upper = 57.9213),  # oneway_anova_output.txt:697
      "University"             = list(n = 82, mean = 52.7692, sd = 17.73869, se = 1.95687, ci_lower = 48.8758, ci_upper = 56.6626)   # oneway_anova_output.txt:698
    ),
    anova = list(
      ss_between = 1189.911, df_between = 3, ms_between = 396.637, f_stat = 1.283, p_value = 0.279,  # oneway_anova_output.txt:710
      ss_within  = 156097.339, df_within  = 505, ms_within  = 309.104,  # oneway_anova_output.txt:711
      ss_total   = 157287.250, df_total   = 508   # oneway_anova_output.txt:712
    ),
    welch = list(f_stat = 1.274, df1 = 3, df2 = 245.463, p = 0.284),  # oneway_anova_output.txt:721
    var_precision = list(mean = 4, sd = 5, se = 5, ci = 4)
  ),
  West = list(
    descriptives = list(
      "Basic Secondary"        = list(n = 673, mean = 49.8468, sd = 16.95967, se = 0.65369, ci_lower = 48.5632, ci_upper = 51.1303),  # oneway_anova_output.txt:700
      "Intermediate Secondary" = list(n = 512, mean = 50.9520, sd = 17.08045, se = 0.75482, ci_lower = 49.4690, ci_upper = 52.4349),  # oneway_anova_output.txt:701
      "Academic Secondary"     = list(n = 519, mean = 50.3411, sd = 16.97622, se = 0.74525, ci_lower = 48.8771, ci_upper = 51.8052),  # oneway_anova_output.txt:702
      "University"             = list(n = 303, mean = 48.5924, sd = 16.47548, se = 0.94649, ci_lower = 46.7298, ci_upper = 50.4549)   # oneway_anova_output.txt:703
    ),
    anova = list(
      ss_between = 1131.554, df_between = 3, ms_between = 377.185, f_stat = 1.317, p_value = 0.267,  # oneway_anova_output.txt:713
      ss_within  = 573644.048, df_within  = 2003, ms_within  = 286.392,  # oneway_anova_output.txt:714
      ss_total   = 574775.602, df_total   = 2006   # oneway_anova_output.txt:715
    ),
    welch = list(f_stat = 1.348, df1 = 3, df2 = 967.532, p = 0.257),  # oneway_anova_output.txt:723
    var_precision = list(mean = 4, sd = 5, se = 5, ci = 4)
  )
)

spss_values$test_4d_life_by_employment_weighted_grouped <- list(
  East = list(
    descriptives = list(
      "Student"    = list(n = 10, mean = 3.65, sd = 1.431, se = 0.442, ci_lower = 2.65, ci_upper = 4.64),  # oneway_anova_output.txt:737
      "Employed"   = list(n = 308, mean = 3.64, sd = 1.189, se = 0.068, ci_lower = 3.50, ci_upper = 3.77),  # oneway_anova_output.txt:738
      "Unemployed" = list(n = 32, mean = 3.66, sd = 1.342, se = 0.238, ci_lower = 3.17, ci_upper = 4.14),  # oneway_anova_output.txt:739
      "Retired"    = list(n = 116, mean = 3.50, sd = 1.196, se = 0.111, ci_lower = 3.28, ci_upper = 3.72),  # oneway_anova_output.txt:740
      "Other"      = list(n = 21, mean = 4.04, sd = 1.120, se = 0.243, ci_lower = 3.54, ci_upper = 4.55)   # oneway_anova_output.txt:741
    ),
    anova = list(
      ss_between = 5.568, df_between = 4, ms_between = 1.392, f_stat = 0.959, p_value = 0.430,  # oneway_anova_output.txt:754
      ss_within  = 699.734, df_within  = 482, ms_within  = 1.452,  # oneway_anova_output.txt:755
      ss_total   = 705.302, df_total   = 486   # oneway_anova_output.txt:756
    ),
    welch = list(f_stat = 1.017, df1 = 4, df2 = 44.066, p = 0.409),  # oneway_anova_output.txt:765
    var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
  ),
  West = list(
    descriptives = list(
      "Student"    = list(n = 66, mean = 3.97, sd = 0.856, se = 0.105, ci_lower = 3.76, ci_upper = 4.18),  # oneway_anova_output.txt:743
      "Employed"   = list(n = 1240, mean = 3.62, sd = 1.167, se = 0.033, ci_lower = 3.55, ci_upper = 3.68),  # oneway_anova_output.txt:744
      "Unemployed" = list(n = 148, mean = 3.67, sd = 1.039, se = 0.085, ci_lower = 3.50, ci_upper = 3.84),  # oneway_anova_output.txt:745
      "Retired"    = list(n = 404, mean = 3.57, sd = 1.136, se = 0.057, ci_lower = 3.46, ci_upper = 3.68),  # oneway_anova_output.txt:746
      "Other"      = list(n = 91, mean = 3.64, sd = 1.079, se = 0.113, ci_lower = 3.42, ci_upper = 3.87)   # oneway_anova_output.txt:747
    ),
    anova = list(
      ss_between = 9.778, df_between = 4, ms_between = 2.444, f_stat = 1.886, p_value = 0.110,  # oneway_anova_output.txt:757
      ss_within  = 2518.068, df_within  = 1943, ms_within  = 1.296,  # oneway_anova_output.txt:758
      ss_total   = 2527.845, df_total   = 1947   # oneway_anova_output.txt:759
    ),
    welch = list(f_stat = 3.028, df1 = 4, df2 = 258.609, p = 0.018),  # oneway_anova_output.txt:767
    var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
  )
)

spss_values$test_5a_life_by_education_posthoc_run <- list(
  descriptives = list(
    "Basic Secondary"        = list(n = 809, mean = 3.20, sd = 1.243, se = 0.044, ci_lower = 3.12, ci_upper = 3.29),  # oneway_anova_output.txt:783
    "Intermediate Secondary" = list(n = 618, mean = 3.70, sd = 1.112, se = 0.045, ci_lower = 3.61, ci_upper = 3.79),  # oneway_anova_output.txt:784
    "Academic Secondary"     = list(n = 607, mean = 3.85, sd = 0.998, se = 0.041, ci_lower = 3.77, ci_upper = 3.93),  # oneway_anova_output.txt:785
    "University"             = list(n = 387, mean = 4.05, sd = 0.957, se = 0.049, ci_lower = 3.95, ci_upper = 4.14)   # oneway_anova_output.txt:786
  ),
  anova = list(
    ss_between = 247.347, df_between = 3, ms_between = 82.449, f_stat = 67.096, p_value = "<.001",  # oneway_anova_output.txt:793
    ss_within  = 2970.080, df_within  = 2417, ms_within  = 1.229,  # oneway_anova_output.txt:794
    ss_total   = 3217.428, df_total   = 2420   # oneway_anova_output.txt:795
  ),
  var_precision = list(mean = 2, sd = 3, se = 3, ci = 2)
)


# =============================================================================
# COMPARISON HELPERS
# =============================================================================

#' Compare per-group descriptives (n, mean, sd, se, ci_lower, ci_upper)
#'
#' @param group_stats Per-group named list from result$results$group_stats[[i]].
#' @param spss_desc Named list (group_label = list(n, mean, sd, se, ci_lower,
#'   ci_upper)). Entries with missing fields are skipped.
#' @param var_precision list(mean=, sd=, se=, ci=) of SPSS print precisions.
#' @param scenario Scenario label for failure messages.
#' @param is_weighted Whether this is a weighted scenario; determines N tier.
compare_descriptives <- function(group_stats, spss_desc, var_precision,
                                  scenario, is_weighted = FALSE) {
  for (lvl in names(spss_desc)) {
    expected <- spss_desc[[lvl]]
    actual   <- group_stats[[lvl]]
    if (is.null(actual)) {
      stop(sprintf("[%s] missing R-side group: %s", scenario, lvl), call. = FALSE)
    }

    # N: Spec(count) for unweighted; Display(0) for weighted
    if (!is.null(expected$n)) {
      actual_n <- if (is_weighted && !is.null(actual$weighted_n)) {
        actual$weighted_n
      } else {
        actual$n
      }
      if (is_weighted) {
        assert_spss(actual_n, expected$n,
                    tier = "display", precision = 0,
                    label = sprintf("[%s | %s] N (weighted, displayed)", scenario, lvl))
      } else {
        assert_spss_count(actual_n, expected$n,
                          label = sprintf("[%s | %s] N", scenario, lvl))
      }
    }

    if (!is.null(expected$mean)) {
      assert_spss(actual$mean, expected$mean,
                  tier = "display", precision = var_precision$mean,
                  label = sprintf("[%s | %s] mean", scenario, lvl))
    }
    if (!is.null(expected$sd)) {
      assert_spss(actual$sd, expected$sd,
                  tier = "display", precision = var_precision$sd,
                  label = sprintf("[%s | %s] sd", scenario, lvl))
    }
    if (!is.null(expected$se)) {
      assert_spss(actual$se, expected$se,
                  tier = "display", precision = var_precision$se,
                  label = sprintf("[%s | %s] se", scenario, lvl))
    }
    if (!is.null(expected$ci_lower)) {
      assert_spss(actual$ci_lower, expected$ci_lower,
                  tier = "display", precision = var_precision$ci,
                  label = sprintf("[%s | %s] CI lower", scenario, lvl))
    }
    if (!is.null(expected$ci_upper)) {
      assert_spss(actual$ci_upper, expected$ci_upper,
                  tier = "display", precision = var_precision$ci,
                  label = sprintf("[%s | %s] CI upper", scenario, lvl))
    }
  }
}


#' Compare the ANOVA table (SS / df / MS / F / p, three sources)
#'
#' @param anova_table Data frame from result$results$anova_table[[i]].
#'   Columns: Source, Sum_Squares, df, Mean_Square, F, p_value.
#' @param spss_anova Expected list with ss_between, df_between, ms_between,
#'   f_stat, p_value, ss_within, df_within, ms_within, ss_total, df_total.
#' @param scenario Label.
#' @param is_weighted Whether to treat df_within as Display(0) vs Spec.
compare_anova_table <- function(anova_table, spss_anova, scenario,
                                 is_weighted = FALSE) {

  # Between Groups
  bg <- anova_table[anova_table$Source == "Between Groups", , drop = FALSE]
  assert_spss(as.numeric(bg$Sum_Squares), spss_anova$ss_between,
              tier = "display", precision = 3,
              label = sprintf("[%s] SS Between", scenario))
  assert_spss_count(as.numeric(bg$df), spss_anova$df_between,
                    label = sprintf("[%s] df Between", scenario))
  assert_spss(as.numeric(bg$Mean_Square), spss_anova$ms_between,
              tier = "display", precision = 3,
              label = sprintf("[%s] MS Between", scenario))
  assert_spss(as.numeric(bg$F), spss_anova$f_stat,
              tier = "display", precision = 3,
              label = sprintf("[%s] F-statistic", scenario))
  assert_spss(as.numeric(bg$p_value), spss_anova$p_value,
              tier = "display", precision = 3, what = "p_value",
              label = sprintf("[%s] p-value", scenario))

  # Within Groups
  wg <- anova_table[anova_table$Source == "Within Groups", , drop = FALSE]
  assert_spss(as.numeric(wg$Sum_Squares), spss_anova$ss_within,
              tier = "display", precision = 3,
              label = sprintf("[%s] SS Within", scenario))
  # df_within: exact integer in both unweighted (n - k) and weighted
  # (floor(sum(w)) - k, SPSS ONEWAY convention).
  assert_spss_count(as.numeric(wg$df), spss_anova$df_within,
                    label = sprintf("[%s] df Within", scenario))
  assert_spss(as.numeric(wg$Mean_Square), spss_anova$ms_within,
              tier = "display", precision = 3,
              label = sprintf("[%s] MS Within", scenario))

  # Total row
  tot <- anova_table[anova_table$Source == "Total", , drop = FALSE]
  assert_spss(as.numeric(tot$Sum_Squares), spss_anova$ss_total,
              tier = "display", precision = 3,
              label = sprintf("[%s] SS Total", scenario))
  # df_total = df_between + df_within (integer in both cases).
  assert_spss_count(as.numeric(tot$df), spss_anova$df_total,
                    label = sprintf("[%s] df Total", scenario))
}


#' Compare Welch test (F, df1, df2, p)
compare_welch <- function(welch_result, spss_welch, scenario) {
  if (is.null(welch_result)) {
    stop(sprintf("[%s] R-side welch_result is NULL", scenario), call. = FALSE)
  }
  assert_spss(as.numeric(welch_result$statistic), spss_welch$f_stat,
              tier = "display", precision = 3,
              label = sprintf("[%s, Welch] F", scenario))
  assert_spss_count(as.numeric(welch_result$parameter[1]), spss_welch$df1,
                    label = sprintf("[%s, Welch] df1", scenario))
  assert_spss(as.numeric(welch_result$parameter[2]), spss_welch$df2,
              tier = "display", precision = 3,
              label = sprintf("[%s, Welch] df2", scenario))
  assert_spss(as.numeric(welch_result$p.value), spss_welch$p,
              tier = "display", precision = 3, what = "p_value",
              label = sprintf("[%s, Welch] p-value", scenario))
}


#' Convenience: extract the single-variable result row and helpers
extract_single <- function(result, var_name = NULL) {
  r <- result$results
  if (!is.null(var_name)) {
    r <- r[r$Variable == var_name, , drop = FALSE]
  } else if (nrow(r) > 1L) {
    r <- r[1, , drop = FALSE]
  }
  list(
    row         = r,
    group_stats = r$group_stats[[1]],
    anova_table = r$anova_table[[1]],
    welch       = r$welch_result[[1]]
  )
}

#' Extract one (region, variable) cell from a grouped result
extract_grouped_cell <- function(result, region, var_name = NULL) {
  r <- result$results
  sel <- r$region == region
  if (!is.null(var_name)) sel <- sel & r$Variable == var_name
  r <- r[sel, , drop = FALSE]
  if (nrow(r) != 1L) {
    stop(sprintf("extract_grouped_cell: expected 1 row, got %d", nrow(r)),
         call. = FALSE)
  }
  list(
    row         = r,
    group_stats = r$group_stats[[1]],
    anova_table = r$anova_table[[1]],
    welch       = r$welch_result[[1]]
  )
}

#' Compare one SPSS cell (descriptives, ANOVA table, Welch row if printed)
compare_oneway_cell <- function(cell, spss, scenario, is_weighted = FALSE) {
  compare_descriptives(cell$group_stats, spss$descriptives, spss$var_precision,
                       scenario, is_weighted = is_weighted)
  compare_anova_table(cell$anova_table, spss$anova, scenario,
                      is_weighted = is_weighted)
  if (!is.null(spss$welch)) compare_welch(cell$welch, spss$welch, scenario)
}

#' Compare every cell of a result; `spss` is keyed by region (SPLIT FILE) or,
#' for several dependent variables, by variable
compare_oneway_cells <- function(result, spss, scenario,
                                 by = c("region", "Variable"),
                                 is_weighted = FALSE) {
  by <- match.arg(by)
  for (key in names(spss)) {
    cell <- if (by == "region") {
      extract_grouped_cell(result, key)
    } else {
      extract_single(result, key)
    }
    compare_oneway_cell(cell, spss[[key]], sprintf("%s [%s]", scenario, key),
                        is_weighted = is_weighted)
  }
}


# =============================================================================
# DATA SETUP
# =============================================================================

data(survey_data, envir = environment())


# =============================================================================
# SCENARIO 1 — UNWEIGHTED / UNGROUPED
# =============================================================================

test_that("Test 1a: life_satisfaction by education — matches SPSS", {
  result <- survey_data |> oneway_anova(life_satisfaction, group = education)
  s <- extract_single(result, "life_satisfaction")
  spss <- spss_values$test_1a_life_by_education

  compare_descriptives(s$group_stats, spss$descriptives, spss$var_precision,
                       "1a: life_sat by education", is_weighted = FALSE)
  compare_anova_table(s$anova_table, spss$anova,
                      "1a: life_sat by education", is_weighted = FALSE)
  compare_welch(s$welch, spss$welch, "1a: life_sat by education")
})

test_that("Test 1b: income by education — matches SPSS", {
  result <- survey_data |> oneway_anova(income, group = education)
  s <- extract_single(result, "income")
  spss <- spss_values$test_1b_income_by_education

  compare_descriptives(s$group_stats, spss$descriptives, spss$var_precision,
                       "1b: income by education", is_weighted = FALSE)
  compare_anova_table(s$anova_table, spss$anova,
                      "1b: income by education", is_weighted = FALSE)
  compare_welch(s$welch, spss$welch, "1b: income by education")
})

test_that("Test 1c: age by education — matches SPSS", {
  result <- survey_data |> oneway_anova(age, group = education)
  s <- extract_single(result, "age")
  spss <- spss_values$test_1c_age_by_education

  compare_descriptives(s$group_stats, spss$descriptives, spss$var_precision,
                       "1c: age by education", is_weighted = FALSE)
  compare_anova_table(s$anova_table, spss$anova,
                      "1c: age by education", is_weighted = FALSE)
  compare_welch(s$welch, spss$welch, "1c: age by education")
})

test_that("Test 1d: three trust variables by education — matches SPSS", {
  result <- survey_data |>
    oneway_anova(trust_government, trust_media, trust_science, group = education)
  compare_oneway_cells(result, spss_values$test_1d_trust_by_education,
                       "1d: trust by education", by = "Variable")
})

test_that("Tests 1e/1f: life_satisfaction and income by employment — matches SPSS", {
  r1e <- survey_data |> oneway_anova(life_satisfaction, group = employment)
  compare_oneway_cell(extract_single(r1e, "life_satisfaction"),
                      spss_values$test_1e_life_by_employment,
                      "1e: life_sat by employment")

  r1f <- survey_data |> oneway_anova(income, group = employment)
  compare_oneway_cell(extract_single(r1f, "income"),
                      spss_values$test_1f_income_by_employment,
                      "1f: income by employment")
})


# =============================================================================
# SCENARIO 2 — WEIGHTED / UNGROUPED
# =============================================================================

test_that("Test 2a: life_satisfaction by education, weighted — matches SPSS", {
  result <- survey_data |>
    oneway_anova(life_satisfaction, group = education, weights = sampling_weight)
  s <- extract_single(result, "life_satisfaction")
  spss <- spss_values$test_2a_life_by_education_weighted

  compare_descriptives(s$group_stats, spss$descriptives, spss$var_precision,
                       "2a: weighted life_sat by education", is_weighted = TRUE)
  compare_anova_table(s$anova_table, spss$anova,
                      "2a: weighted life_sat by education", is_weighted = TRUE)
  compare_welch(s$welch, spss$welch, "2a: weighted life_sat by education")
})

test_that("Test 2b: income by education, weighted — matches SPSS", {
  result <- survey_data |>
    oneway_anova(income, group = education, weights = sampling_weight)
  s <- extract_single(result, "income")
  spss <- spss_values$test_2b_income_by_education_weighted

  compare_descriptives(s$group_stats, spss$descriptives, spss$var_precision,
                       "2b: weighted income by education", is_weighted = TRUE)
  compare_anova_table(s$anova_table, spss$anova,
                      "2b: weighted income by education", is_weighted = TRUE)
  compare_welch(s$welch, spss$welch, "2b: weighted income by education")
})

test_that("Test 2c: age by education, weighted — matches SPSS", {
  result <- survey_data |>
    oneway_anova(age, group = education, weights = sampling_weight)
  s <- extract_single(result, "age")
  spss <- spss_values$test_2c_age_by_education_weighted

  compare_descriptives(s$group_stats, spss$descriptives, spss$var_precision,
                       "2c: weighted age by education", is_weighted = TRUE)
  compare_anova_table(s$anova_table, spss$anova,
                      "2c: weighted age by education", is_weighted = TRUE)
  compare_welch(s$welch, spss$welch, "2c: weighted age by education")
})

test_that("Test 2d: three trust variables by education, weighted — matches SPSS", {
  result <- survey_data |>
    oneway_anova(trust_government, trust_media, trust_science, group = education,
                 weights = sampling_weight)
  compare_oneway_cells(result, spss_values$test_2d_trust_by_education_weighted,
                       "2d: weighted trust by education", by = "Variable",
                       is_weighted = TRUE)
})

test_that("Tests 2e/2f: life_satisfaction and income by employment, weighted — matches SPSS", {
  r2e <- survey_data |>
    oneway_anova(life_satisfaction, group = employment, weights = sampling_weight)
  compare_oneway_cell(extract_single(r2e, "life_satisfaction"),
                      spss_values$test_2e_life_by_employment_weighted,
                      "2e: weighted life_sat by employment", is_weighted = TRUE)

  r2f <- survey_data |>
    oneway_anova(income, group = employment, weights = sampling_weight)
  compare_oneway_cell(extract_single(r2f, "income"),
                      spss_values$test_2f_income_by_employment_weighted,
                      "2f: weighted income by employment", is_weighted = TRUE)
})


# =============================================================================
# SCENARIO 3 — UNWEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 3a: life_satisfaction by education, grouped by region — matches SPSS", {
  result <- survey_data |>
    group_by(region) |>
    oneway_anova(life_satisfaction, group = education)

  spss <- spss_values$test_3a_life_by_education_grouped
  for (rg in c("East", "West")) {
    cell <- extract_grouped_cell(result, rg)
    compare_descriptives(cell$group_stats, spss[[rg]]$descriptives,
                         spss[[rg]]$var_precision,
                         sprintf("3a: life_sat by education [%s]", rg),
                         is_weighted = FALSE)
    compare_anova_table(cell$anova_table, spss[[rg]]$anova,
                        sprintf("3a: life_sat by education [%s]", rg),
                        is_weighted = FALSE)
    compare_welch(cell$welch, spss[[rg]]$welch,
                  sprintf("3a: life_sat by education [%s]", rg))
  }
})

test_that("Tests 3b-3d: income, age by education and life_satisfaction by employment, grouped by region — matches SPSS", {
  by_region <- survey_data |> group_by(region)

  compare_oneway_cells(by_region |> oneway_anova(income, group = education),
                       spss_values$test_3b_income_by_education_grouped,
                       "3b: income by education")
  compare_oneway_cells(by_region |> oneway_anova(age, group = education),
                       spss_values$test_3c_age_by_education_grouped,
                       "3c: age by education")
  compare_oneway_cells(by_region |> oneway_anova(life_satisfaction, group = employment),
                       spss_values$test_3d_life_by_employment_grouped,
                       "3d: life_sat by employment")
})


# =============================================================================
# SCENARIO 4 — WEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 4a: life_satisfaction by education, weighted, grouped by region — matches SPSS", {
  result <- survey_data |>
    group_by(region) |>
    oneway_anova(life_satisfaction, group = education, weights = sampling_weight)

  spss <- spss_values$test_4a_life_by_education_weighted_grouped
  for (rg in c("East", "West")) {
    cell <- extract_grouped_cell(result, rg)
    compare_descriptives(cell$group_stats, spss[[rg]]$descriptives,
                         spss[[rg]]$var_precision,
                         sprintf("4a: weighted life_sat by education [%s]", rg),
                         is_weighted = TRUE)
    compare_anova_table(cell$anova_table, spss[[rg]]$anova,
                        sprintf("4a: weighted life_sat by education [%s]", rg),
                        is_weighted = TRUE)
    compare_welch(cell$welch, spss[[rg]]$welch,
                  sprintf("4a: weighted life_sat by education [%s]", rg))
  }
})

test_that("Tests 4b-4d: income, age by education and life_satisfaction by employment, weighted, grouped by region — matches SPSS", {
  by_region <- survey_data |> group_by(region)

  compare_oneway_cells(
    by_region |> oneway_anova(income, group = education, weights = sampling_weight),
    spss_values$test_4b_income_by_education_weighted_grouped,
    "4b: weighted income by education", is_weighted = TRUE)
  compare_oneway_cells(
    by_region |> oneway_anova(age, group = education, weights = sampling_weight),
    spss_values$test_4c_age_by_education_weighted_grouped,
    "4c: weighted age by education", is_weighted = TRUE)
  compare_oneway_cells(
    by_region |> oneway_anova(life_satisfaction, group = employment,
                              weights = sampling_weight),
    spss_values$test_4d_life_by_employment_weighted_grouped,
    "4d: weighted life_sat by employment", is_weighted = TRUE)
})


# =============================================================================
# AUXILIARY SCENARIOS
# =============================================================================

test_that("Test 5a: ONEWAY of the Tukey post-hoc run — matches SPSS", {
  # /STATISTICS DESCRIPTIVES HOMOGENEITY /POSTHOC=TUKEY: no Robust Tests
  result <- survey_data |> oneway_anova(life_satisfaction, group = education)
  compare_oneway_cell(extract_single(result, "life_satisfaction"),
                      spss_values$test_5a_life_by_education_posthoc_run,
                      "5a: life_sat by education (post-hoc run)")
})

test_that("Test 6a: 90% CI for life_satisfaction by education — matches SPSS", {
  result <- survey_data |>
    oneway_anova(life_satisfaction, group = education, conf.level = 0.90)
  compare_oneway_cell(extract_single(result, "life_satisfaction"),
                      spss_values$test_6a_life_by_education_90ci,
                      "6a: 90% CI life_sat")
})

test_that("Test 6b: 99% CI for life_satisfaction by education — matches SPSS", {
  result <- survey_data |>
    oneway_anova(life_satisfaction, group = education, conf.level = 0.99)
  compare_oneway_cell(extract_single(result, "life_satisfaction"),
                      spss_values$test_6b_life_by_education_99ci,
                      "6b: 99% CI life_sat")
})

test_that("Test 7: multiple variables (political_orientation, environmental_concern) — matches SPSS", {
  result <- survey_data |>
    oneway_anova(life_satisfaction, income, age,
                 political_orientation, environmental_concern,
                 group = education)

  # One row per variable, in the syntax order SPSS lists them in
  expect_identical(result$results$Variable,
                   c("life_satisfaction", "income", "age",
                     "political_orientation", "environmental_concern"))

  # Validate the two NEW variables (the other three duplicate Tests 1a-c)
  compare_oneway_cell(extract_single(result, "political_orientation"),
                      spss_values$test_7_political, "7: political_orientation")
  compare_oneway_cell(extract_single(result, "environmental_concern"),
                      spss_values$test_7_environmental, "7: environmental_concern")
})


# =============================================================================
# EDGE CASES
# =============================================================================

test_that("Edge case: error when grouping variable has < 2 levels", {
  d <- survey_data[survey_data$education == "Basic Secondary", , drop = FALSE]
  expect_error(
    oneway_anova(d, life_satisfaction, group = education),
    regexp = "at least 2"
  )
})

test_that("Edge case: missing values reduce per-group N exactly by the NAs", {
  d <- survey_data
  # Inject 5 NAs into "Basic Secondary" rows for life_satisfaction
  basic_idx <- which(d$education == "Basic Secondary" & !is.na(d$life_satisfaction))
  d$life_satisfaction[basic_idx[1:5]] <- NA

  base <- survey_data |> oneway_anova(life_satisfaction, group = education)
  red  <- d            |> oneway_anova(life_satisfaction, group = education)

  base_n <- base$results$group_stats[[1]][["Basic Secondary"]]$n
  red_n  <- red$results$group_stats[[1]][["Basic Secondary"]]$n
  assert_spss_count(red_n, base_n - 5L,
                    label = "missing-value edge case: N reduction by 5")
})


# =============================================================================
# NOTE — VALIDATION GAPS
# =============================================================================
# 1. Brown-Forsythe statistic: SPSS prints it; mariposa does not expose it
#    on result$results. Adding it to the output (and to welch_result or a new
#    brown_forsythe field) would close this gap. Phase 2 work.
# 2. Effect sizes (eta², epsilon², omega²): SPSS ONEWAY does not print these
#    by default in v29; mariposa computes them. They are R-only here and not
#    validated against SPSS. The values are exposed on result$results$
#    eta_squared / epsilon_squared / omega_squared if needed for Tier-4
#    snapshot tests later.
# 3. ANOVA Total Mean_Square: SPSS leaves blank; mariposa likewise stores "".
#    No assertion needed.
# =============================================================================
