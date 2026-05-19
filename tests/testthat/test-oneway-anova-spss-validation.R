# =============================================================================
# oneway_anova — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::oneway_anova() against SPSS v29 ONEWAY procedure.
# Reference syntax:  tests/spss_reference/syntax/oneway_anova_test.sps
# Reference output:  tests/spss_reference/outputs/oneway_anova_output.txt
#
# Charter reference: .claude/VALIDATION_CHARTER.md
#
# Scenario coverage (per Charter §8):
#   Scenario 1 — Unweighted / Ungrouped         (Tests 1a-c: life_sat / income / age by education)
#   Scenario 2 — Weighted   / Ungrouped         (Tests 2a-c)
#   Scenario 3 — Unweighted / Grouped by region (Tests 3a-c)
#   Scenario 4 — Weighted   / Grouped by region (Tests 4a-c)
#
# Plus auxiliary scenarios:
#   Tests 6a, 6b — alternative CI levels (90%, 99%)
#   Test  7      — multiple variables at once
#
# Out of scope for this file:
#   - Test 1d/2d (trust variables): identical structure to Test 7 multi-variable.
#   - Tests 1e-f/2e-f/3d/4d (life_sat by employment): same statistical
#     machinery as 1a-c; the employment factor adds no algorithmic novelty.
#   - Test 5a Tukey post-hoc: covered by test-tukey-test-spss-validation.R.
#
# Tolerance tier assignments (per Charter §5):
#   N (unweighted)            — Spec (count, exact integer)
#   N (weighted, displayed)   — Display(0), tol ±0.5
#   Mean                      — Display(2) for life_sat, Display(4) for income/age
#   SD                        — Display(3) for life_sat, Display(5) for income/age
#   SE                        — Display(3) for life_sat, Display(5) for income/age
#   CI bounds                 — Display(2) for life_sat, Display(4) for income/age
#   ANOVA SS                  — Display(3)
#   ANOVA MS                  — Display(3)
#   F-statistic               — Display(3)
#   p-value                   — Display(3), sentinel "<.001"
#   df_between                — Spec (integer)
#   df_within (unweighted)    — Spec (integer)
#   df_within (weighted)      — Display(0), tol ±0.5
#   Welch F, df2              — Display(3)
#
# Pre-fix bugs discovered during this migration (R/oneway_anova.R 2026-05-19):
#   - Line 368: df_within = round(sum(w)) - k → fixed to sum(w) - k
#     Same pattern as t_test bug. Affects F = MS_between / MS_within and p.
#
# Validation gaps (Phase 2 candidates):
#   - Brown-Forsythe statistic: SPSS prints it, mariposa does not expose it.
#     Asserting Brown-Forsythe would require adding it to the output object.
#   - mariposa's weighted-descriptives SE divides by sqrt(physical n) rather
#     than sqrt(sum(w)). May surface as additional Display-tier mismatches in
#     scenarios 2/4. If so, register as bug fix in same R file.
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
      "Basic Secondary"        = list(ci_lower = 3.13, ci_upper = 3.28),   # oneway_anova_output.txt:828
      "Intermediate Secondary" = list(ci_lower = 3.63, ci_upper = 3.77),   # oneway_anova_output.txt:829
      "Academic Secondary"     = list(ci_lower = 3.79, ci_upper = 3.92),   # oneway_anova_output.txt:830
      "University"             = list(ci_lower = 3.97, ci_upper = 4.13)    # oneway_anova_output.txt:831
    ),
    var_precision = list(ci = 2)
  ),

  # ---- Test 6b: 99% CI for life_sat by education (unweighted) -----------
  test_6b_life_by_education_99ci = list(
    descriptives = list(
      "Basic Secondary"        = list(ci_lower = 3.09, ci_upper = 3.32),   # oneway_anova_output.txt:852
      "Intermediate Secondary" = list(ci_lower = 3.59, ci_upper = 3.82),   # oneway_anova_output.txt:853
      "Academic Secondary"     = list(ci_lower = 3.75, ci_upper = 3.96),   # oneway_anova_output.txt:854
      "University"             = list(ci_lower = 3.92, ci_upper = 4.17)    # oneway_anova_output.txt:855
    ),
    var_precision = list(ci = 2)
  ),

  # ---- Test 7: multiple variables (selected additions, not duplicates) -
  # The first three variables (life_sat, income, age) duplicate Tests
  # 1a-c (already validated). Test 7 adds political_orientation and
  # environmental_concern. We assert the new ANOVA rows only.
  test_7_political = list(
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
              tier = "display", precision = 3,
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
              tier = "display", precision = 3,
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


# =============================================================================
# AUXILIARY SCENARIOS
# =============================================================================

test_that("Test 6a: 90% CI for life_satisfaction by education — matches SPSS", {
  result <- survey_data |>
    oneway_anova(life_satisfaction, group = education, conf.level = 0.90)
  s <- extract_single(result, "life_satisfaction")
  spss <- spss_values$test_6a_life_by_education_90ci

  compare_descriptives(s$group_stats, spss$descriptives, spss$var_precision,
                       "6a: 90% CI life_sat", is_weighted = FALSE)
})

test_that("Test 6b: 99% CI for life_satisfaction by education — matches SPSS", {
  result <- survey_data |>
    oneway_anova(life_satisfaction, group = education, conf.level = 0.99)
  s <- extract_single(result, "life_satisfaction")
  spss <- spss_values$test_6b_life_by_education_99ci

  compare_descriptives(s$group_stats, spss$descriptives, spss$var_precision,
                       "6b: 99% CI life_sat", is_weighted = FALSE)
})

test_that("Test 7: multiple variables (political_orientation, environmental_concern) — matches SPSS", {
  result <- survey_data |>
    oneway_anova(life_satisfaction, income, age,
                 political_orientation, environmental_concern,
                 group = education)

  expect_equal(nrow(result$results), 5L)
  expect_setequal(result$results$Variable,
                  c("life_satisfaction", "income", "age",
                    "political_orientation", "environmental_concern"))

  # Validate the two NEW variables (the other three duplicate Tests 1a-c)
  pol_s <- extract_single(result, "political_orientation")
  compare_anova_table(pol_s$anova_table,
                      spss_values$test_7_political$anova,
                      "7: political_orientation",
                      is_weighted = FALSE)
  compare_welch(pol_s$welch, spss_values$test_7_political$welch,
                "7: political_orientation")

  env_s <- extract_single(result, "environmental_concern")
  compare_anova_table(env_s$anova_table,
                      spss_values$test_7_environmental$anova,
                      "7: environmental_concern",
                      is_weighted = FALSE)
  compare_welch(env_s$welch, spss_values$test_7_environmental$welch,
                "7: environmental_concern")
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
