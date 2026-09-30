# =============================================================================
# t_test — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::t_test() against SPSS v29 T-TEST procedure.
# Reference syntax:  tests/spss_reference/syntax/t_test.sps
# Reference output:  tests/spss_reference/outputs/t_test_output.txt
#
# Charter reference: .claude/VALIDATION_CHARTER.md
#
# Scenario coverage (per Charter §8, all four scenarios required):
#   Scenario 1 — Unweighted / Ungrouped         (Tests 1a-d)
#   Scenario 2 — Weighted   / Ungrouped         (Tests 2a-d)
#   Scenario 3 — Unweighted / Grouped by region (Tests 3a-c)
#   Scenario 4 — Weighted   / Grouped by region (Tests 4a-c)
#
# Plus auxiliary scenarios from the SPSS reference:
#   Tests 5a-b — One-sample with non-zero mu (income, age)
#   Test  6    — Multiple variables at once
#   Tests 7a-b — Alternative confidence levels (90%, 99%)
#
# Asserted per SPSS row (Independent Samples Test: both the equal-variance
# and the Welch row; One-Sample Test and One-Sample Statistics):
#   t, df, one- and two-sided Sig., Mean Difference, Std. Error Difference,
#   CI bounds; N, Mean, SD, SE of the mean (one-sample); the headline
#   columns of $results (Welch row by default); the one-sided p through
#   mariposa's own alternative = "less" / "greater" path.
#
# Tolerance tiers (Charter §4/§5):
#   N, unweighted integer df          — Spec (count) / Display(0)
#   t, Welch df, p                    — Display, 3 decimals (p: what = "p_value";
#                                       ".000" -> sentinel "<.001")
#   equal-variance df                 — Display(0): non-integer sum(w) - 2
#                                       when weighted (Charter §5.1)
#   Mean Difference, SE Difference,   — Display with the decimals SPSS printed,
#   CI bounds, Mean, SD, SE             cached per entry as `dp` (e.g. 3 for
#                                       the 1-5 scales, 5 for income/age)
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# =============================================================================
# SPSS REFERENCE VALUES (with citation comments per Charter §7)
# =============================================================================

# `dp` = the number of decimals SPSS printed for the Mean Difference,
# Std. Error Difference and CI columns (one-sample: per statistic).
spss_values <- list(

  # =========================================================================
  # SCENARIO 1: UNWEIGHTED / UNGROUPED
  # =========================================================================

  # ---- Test 1a: one-sample, life_satisfaction, mu = 3.0 -----------------
  test_1a_one_sample = list(
    dp = c(mean = 2L, sd = 3L, se = 3L, mean_diff = 3L, ci = 2L),  # decimals printed
    n           = 2421,        # t_test_output.txt:11
    mean        = 3.63,        # t_test_output.txt:11  (SPSS prints 2 decimals)
    sd          = 1.153,       # t_test_output.txt:11
    se          = 0.023,       # t_test_output.txt:11
    t_stat      = 26.809,      # t_test_output.txt:18
    df          = 2420,        # t_test_output.txt:18
    p_one_sided = "<.001",     # t_test_output.txt:18  (SPSS prints ".000")
    p_two_sided = "<.001",     # t_test_output.txt:18  (SPSS prints ".000")
    mean_diff   = 0.628,       # t_test_output.txt:18
    ci_lower    = 0.58,        # t_test_output.txt:18  (SPSS prints 2 decimals)
    ci_upper    = 0.67         # t_test_output.txt:18
  ),

  # ---- Test 1b: two-sample, life_satisfaction by gender -----------------
  test_1b_life_by_gender = list(
    dp = 3L,  # decimals printed: Mean/SE Difference, CI
    equal_var = list(
      t_stat      = -1.019,    # t_test_output.txt:33
      df          = 2419,      # t_test_output.txt:33
      p_one_sided = 0.154,     # t_test_output.txt:33
      p_two_sided = 0.308,     # t_test_output.txt:33
      mean_diff   = -0.048,    # t_test_output.txt:33
      se_diff     = 0.047,     # t_test_output.txt:33
      ci_lower    = -0.140,    # t_test_output.txt:33
      ci_upper    = 0.044      # t_test_output.txt:33
    ),
    welch = list(
      t_stat      = -1.018,    # t_test_output.txt:35
      df          = 2384.147,  # t_test_output.txt:35
      p_one_sided = 0.154,     # t_test_output.txt:35
      p_two_sided = 0.309,     # t_test_output.txt:35
      mean_diff   = -0.048,    # t_test_output.txt:35
      se_diff     = 0.047,     # t_test_output.txt:35
      ci_lower    = -0.140,    # t_test_output.txt:35
      ci_upper    = 0.044      # t_test_output.txt:35
    )
  ),

  # ---- Test 1c: two-sample, income by gender ----------------------------
  test_1c_income_by_gender = list(
    dp = 5L,  # decimals printed: Mean/SE Difference, CI
    equal_var = list(
      t_stat      = 0.690,     # t_test_output.txt:48
      df          = 2184,      # t_test_output.txt:48
      p_one_sided = 0.245,     # t_test_output.txt:48
      p_two_sided = 0.490,     # t_test_output.txt:48
      mean_diff   = 42.31961,  # t_test_output.txt:48  (5-decimal precision)
      se_diff     = 61.35430,  # t_test_output.txt:48
      ci_lower    = -77.99928, # t_test_output.txt:48
      ci_upper    = 162.63850  # t_test_output.txt:48
    ),
    welch = list(
      t_stat      = 0.690,     # t_test_output.txt:49
      df          = 2169.337,  # t_test_output.txt:49
      p_one_sided = 0.245,     # t_test_output.txt:49
      p_two_sided = 0.490,     # t_test_output.txt:49
      mean_diff   = 42.31961,  # t_test_output.txt:49
      se_diff     = 61.34398,  # t_test_output.txt:49
      ci_lower    = -77.97949, # t_test_output.txt:49
      ci_upper    = 162.61872  # t_test_output.txt:49
    )
  ),

  # ---- Test 1d: two-sample, age by gender -------------------------------
  test_1d_age_by_gender = list(
    dp = 5L,  # decimals printed: Mean/SE Difference, CI
    equal_var = list(
      t_stat      = -0.229,    # t_test_output.txt:62
      df          = 2498,      # t_test_output.txt:62
      p_one_sided = 0.409,     # t_test_output.txt:62
      p_two_sided = 0.819,     # t_test_output.txt:62
      mean_diff   = -0.15587,  # t_test_output.txt:62
      se_diff     = 0.67985,   # t_test_output.txt:62
      ci_lower    = -1.48900,  # t_test_output.txt:62
      ci_upper    = 1.17726    # t_test_output.txt:62
    ),
    welch = list(
      t_stat      = -0.229,    # t_test_output.txt:63
      df          = 2468.094,  # t_test_output.txt:63
      p_one_sided = 0.409,     # t_test_output.txt:63
      p_two_sided = 0.819,     # t_test_output.txt:63
      mean_diff   = -0.15587,  # t_test_output.txt:63
      se_diff     = 0.68047,   # t_test_output.txt:63
      ci_lower    = -1.49023,  # t_test_output.txt:63
      ci_upper    = 1.17849    # t_test_output.txt:63
    )
  ),

  # =========================================================================
  # SCENARIO 2: WEIGHTED / UNGROUPED
  # =========================================================================

  # ---- Test 2a: one-sample weighted, life_satisfaction, mu = 3.0 --------
  test_2a_one_sample_weighted = list(
    dp = c(mean = 2L, sd = 3L, se = 3L, mean_diff = 3L, ci = 2L),  # decimals printed
    n           = 2437,        # t_test_output.txt:75  (rounded sum of weights)
    mean        = 3.62,        # t_test_output.txt:75
    sd          = 1.152,       # t_test_output.txt:75
    se          = 0.023,       # t_test_output.txt:75
    t_stat      = 26.771,      # t_test_output.txt:82
    df          = 2436,        # t_test_output.txt:82
    p_one_sided = "<.001",     # t_test_output.txt:82
    p_two_sided = "<.001",     # t_test_output.txt:82
    mean_diff   = 0.625,       # t_test_output.txt:82
    ci_lower    = 0.58,        # t_test_output.txt:82
    ci_upper    = 0.67         # t_test_output.txt:82
  ),

  # ---- Test 2b: two-sample weighted, life_satisfaction by gender --------
  test_2b_life_by_gender_weighted = list(
    dp = 3L,  # decimals printed: Mean/SE Difference, CI
    equal_var = list(
      t_stat      = -1.070,    # t_test_output.txt:97
      df          = 2435,      # t_test_output.txt:97
      p_one_sided = 0.142,     # t_test_output.txt:97
      p_two_sided = 0.285,     # t_test_output.txt:97
      mean_diff   = -0.050,    # t_test_output.txt:97
      se_diff     = 0.047,     # t_test_output.txt:97
      ci_lower    = -0.142,    # t_test_output.txt:97
      ci_upper    = 0.042      # t_test_output.txt:97
    ),
    welch = list(
      t_stat      = -1.069,    # t_test_output.txt:99
      df          = 2391.291,  # t_test_output.txt:99
      p_one_sided = 0.143,     # t_test_output.txt:99
      p_two_sided = 0.285,     # t_test_output.txt:99
      mean_diff   = -0.050,    # t_test_output.txt:99
      se_diff     = 0.047,     # t_test_output.txt:99
      ci_lower    = -0.142,    # t_test_output.txt:99
      ci_upper    = 0.042      # t_test_output.txt:99
    )
  ),

  # ---- Test 2c: two-sample weighted, income by gender -------------------
  test_2c_income_by_gender_weighted = list(
    dp = 5L,  # decimals printed: Mean/SE Difference, CI
    equal_var = list(
      t_stat      = 0.751,     # t_test_output.txt:112
      df          = 2199,      # t_test_output.txt:112
      p_one_sided = 0.226,     # t_test_output.txt:112
      p_two_sided = 0.453,     # t_test_output.txt:112
      mean_diff   = 45.61972,  # t_test_output.txt:112
      se_diff     = 60.78060,  # t_test_output.txt:112
      ci_lower    = -73.57366, # t_test_output.txt:112
      ci_upper    = 164.81311  # t_test_output.txt:112
    ),
    welch = list(
      t_stat      = 0.751,     # t_test_output.txt:113
      df          = 2178.724,  # t_test_output.txt:113
      p_one_sided = 0.227,     # t_test_output.txt:113
      p_two_sided = 0.453,     # t_test_output.txt:113
      mean_diff   = 45.61972,  # t_test_output.txt:113
      se_diff     = 60.78235,  # t_test_output.txt:113
      ci_lower    = -73.57770, # t_test_output.txt:113
      ci_upper    = 164.81715  # t_test_output.txt:113
    )
  ),

  # ---- Test 2d: two-sample weighted, age by gender ----------------------
  test_2d_age_by_gender_weighted = list(
    dp = 5L,  # decimals printed: Mean/SE Difference, CI
    equal_var = list(
      t_stat      = 0.138,     # t_test_output.txt:126
      df          = 2514,      # t_test_output.txt:126
      p_one_sided = 0.445,     # t_test_output.txt:126
      p_two_sided = 0.890,     # t_test_output.txt:126
      mean_diff   = 0.09444,   # t_test_output.txt:126
      se_diff     = 0.68216,   # t_test_output.txt:126
      ci_lower    = -1.24322,  # t_test_output.txt:126
      ci_upper    = 1.43210    # t_test_output.txt:126
    ),
    welch = list(
      t_stat      = 0.138,     # t_test_output.txt:127
      df          = 2483.483,  # t_test_output.txt:127
      p_one_sided = 0.445,     # t_test_output.txt:127
      p_two_sided = 0.890,     # t_test_output.txt:127
      mean_diff   = 0.09444,   # t_test_output.txt:127
      se_diff     = 0.68251,   # t_test_output.txt:127
      ci_lower    = -1.24391,  # t_test_output.txt:127
      ci_upper    = 1.43279    # t_test_output.txt:127
    )
  ),

  # =========================================================================
  # SCENARIO 3: UNWEIGHTED / GROUPED by region
  # =========================================================================

  # ---- Test 3a: life_satisfaction by gender, grouped by region ----------
  test_3a_life_by_gender_grouped = list(
    East = list(
      dp = 3L,
      equal_var = list(
        t_stat      = 0.598,   # t_test_output.txt:143
        df          = 463,     # t_test_output.txt:143
        p_one_sided = 0.275,   # t_test_output.txt:143
        p_two_sided = 0.550,   # t_test_output.txt:143
        mean_diff   = 0.067,   # t_test_output.txt:143
        se_diff     = 0.112,   # t_test_output.txt:143
        ci_lower    = -0.153,  # t_test_output.txt:143
        ci_upper    = 0.287    # t_test_output.txt:143
      ),
      welch = list(
        t_stat      = 0.598,   # t_test_output.txt:146
        df          = 462.235, # t_test_output.txt:146
        p_one_sided = 0.275,   # t_test_output.txt:146
        p_two_sided = 0.550,   # t_test_output.txt:146
        mean_diff   = 0.067,   # t_test_output.txt:146
        se_diff     = 0.112,   # t_test_output.txt:146
        ci_lower    = -0.153,  # t_test_output.txt:146
        ci_upper    = 0.287    # t_test_output.txt:146
      )
    ),
    West = list(
      dp = 3L,
      equal_var = list(
        t_stat      = -1.453,  # t_test_output.txt:148
        df          = 1954,    # t_test_output.txt:148
        p_one_sided = 0.073,   # t_test_output.txt:148
        p_two_sided = 0.146,   # t_test_output.txt:148
        mean_diff   = -0.075,  # t_test_output.txt:148
        se_diff     = 0.052,   # t_test_output.txt:148
        ci_lower    = -0.176,  # t_test_output.txt:148
        ci_upper    = 0.026    # t_test_output.txt:148
      ),
      welch = list(
        t_stat      = -1.451,  # t_test_output.txt:151
        df          = 1916.526,# t_test_output.txt:151
        p_one_sided = 0.073,   # t_test_output.txt:151
        p_two_sided = 0.147,   # t_test_output.txt:151
        mean_diff   = -0.075,  # t_test_output.txt:151
        se_diff     = 0.052,   # t_test_output.txt:151
        ci_lower    = -0.176,  # t_test_output.txt:151
        ci_upper    = 0.026    # t_test_output.txt:151
      )
    )
  ),

  # ---- Test 3b: income by gender, grouped by region ---------------------
  test_3b_income_by_gender_grouped = list(
    East = list(
      dp = 5L,
      equal_var = list(
        t_stat      = 1.426,        # t_test_output.txt:165
        df          = 427,          # t_test_output.txt:165
        p_one_sided = 0.077,        # t_test_output.txt:165
        p_two_sided = 0.155,        # t_test_output.txt:165
        mean_diff   = 190.84737,    # t_test_output.txt:165
        se_diff     = 133.83880,    # t_test_output.txt:165
        ci_lower    = -72.21751,    # t_test_output.txt:165
        ci_upper    = 453.91224     # t_test_output.txt:165
      ),
      welch = list(
        t_stat      = 1.420,        # t_test_output.txt:167
        df          = 412.750,      # t_test_output.txt:167
        p_one_sided = 0.078,        # t_test_output.txt:167
        p_two_sided = 0.156,        # t_test_output.txt:167
        mean_diff   = 190.84737,    # t_test_output.txt:167
        se_diff     = 134.38503,    # t_test_output.txt:167
        ci_lower    = -73.31706,    # t_test_output.txt:167
        ci_upper    = 455.01180     # t_test_output.txt:167
      )
    ),
    West = list(
      dp = 5L,
      equal_var = list(
        t_stat      = 0.087,        # t_test_output.txt:169
        df          = 1755,         # t_test_output.txt:169
        p_one_sided = 0.465,        # t_test_output.txt:169
        p_two_sided = 0.930,        # t_test_output.txt:169
        mean_diff   = 6.03322,      # t_test_output.txt:169
        se_diff     = 68.99648,     # t_test_output.txt:169
        ci_lower    = -129.29072,   # t_test_output.txt:169
        ci_upper    = 141.35717     # t_test_output.txt:169
      ),
      welch = list(
        t_stat      = 0.088,        # t_test_output.txt:171
        df          = 1748.855,     # t_test_output.txt:171
        p_one_sided = 0.465,        # t_test_output.txt:171
        p_two_sided = 0.930,        # t_test_output.txt:171
        mean_diff   = 6.03322,      # t_test_output.txt:171
        se_diff     = 68.90101,     # t_test_output.txt:171
        ci_lower    = -129.10379,   # t_test_output.txt:171
        ci_upper    = 141.17024     # t_test_output.txt:171
      )
    )
  ),

  # ---- Test 3c: age by gender, grouped by region ------------------------
  test_3c_age_by_gender_grouped = list(
    East = list(
      dp = 5L,
      equal_var = list(
        t_stat      = -0.942,       # t_test_output.txt:186
        df          = 483,          # t_test_output.txt:186
        p_one_sided = 0.173,        # t_test_output.txt:186
        p_two_sided = 0.347,        # t_test_output.txt:186
        mean_diff   = -1.48995,     # t_test_output.txt:186
        se_diff     = 1.58249,      # t_test_output.txt:186
        ci_lower    = -4.59935,     # t_test_output.txt:186
        ci_upper    = 1.61946       # t_test_output.txt:186
      ),
      welch = list(
        t_stat      = -0.940,       # t_test_output.txt:187
        df          = 477.409,      # t_test_output.txt:187
        p_one_sided = 0.174,        # t_test_output.txt:187
        p_two_sided = 0.348,        # t_test_output.txt:187
        mean_diff   = -1.48995,     # t_test_output.txt:187
        se_diff     = 1.58458,      # t_test_output.txt:187
        ci_lower    = -4.60356,     # t_test_output.txt:187
        ci_upper    = 1.62367       # t_test_output.txt:187
      )
    ),
    West = list(
      dp = 5L,
      equal_var = list(
        t_stat      = 0.193,        # t_test_output.txt:188
        df          = 2013,         # t_test_output.txt:188
        p_one_sided = 0.423,        # t_test_output.txt:188
        p_two_sided = 0.847,        # t_test_output.txt:188
        mean_diff   = 0.14522,      # t_test_output.txt:188
        se_diff     = 0.75219,      # t_test_output.txt:188
        ci_lower    = -1.32994,     # t_test_output.txt:188
        ci_upper    = 1.62037       # t_test_output.txt:188
      ),
      welch = list(
        t_stat      = 0.193,        # t_test_output.txt:189
        df          = 1988.415,     # t_test_output.txt:189
        p_one_sided = 0.424,        # t_test_output.txt:189
        p_two_sided = 0.847,        # t_test_output.txt:189
        mean_diff   = 0.14522,      # t_test_output.txt:189
        se_diff     = 0.75253,      # t_test_output.txt:189
        ci_lower    = -1.33061,     # t_test_output.txt:189
        ci_upper    = 1.62104       # t_test_output.txt:189
      )
    )
  ),

  # =========================================================================
  # SCENARIO 4: WEIGHTED / GROUPED by region
  # =========================================================================

  # ---- Test 4a: life_satisfaction by gender, weighted, grouped ----------
  test_4a_life_by_gender_weighted_grouped = list(
    East = list(
      dp = 3L,
      equal_var = list(
        t_stat      = 0.641,        # t_test_output.txt:205
        df          = 486,          # t_test_output.txt:205
        p_one_sided = 0.261,        # t_test_output.txt:205
        p_two_sided = 0.522,        # t_test_output.txt:205
        mean_diff   = 0.070,        # t_test_output.txt:205
        se_diff     = 0.109,        # t_test_output.txt:205
        ci_lower    = -0.144,       # t_test_output.txt:205
        ci_upper    = 0.284         # t_test_output.txt:205
      ),
      welch = list(
        t_stat      = 0.641,        # t_test_output.txt:208
        df          = 484.658,      # t_test_output.txt:208
        p_one_sided = 0.261,        # t_test_output.txt:208
        p_two_sided = 0.522,        # t_test_output.txt:208
        mean_diff   = 0.070,        # t_test_output.txt:208
        se_diff     = 0.109,        # t_test_output.txt:208
        ci_lower    = -0.144,       # t_test_output.txt:208
        ci_upper    = 0.284         # t_test_output.txt:208
      )
    ),
    West = list(
      dp = 3L,
      equal_var = list(
        t_stat      = -1.550,       # t_test_output.txt:210
        df          = 1947,         # t_test_output.txt:210
        p_one_sided = 0.061,        # t_test_output.txt:210
        p_two_sided = 0.121,        # t_test_output.txt:210
        mean_diff   = -0.080,       # t_test_output.txt:210
        se_diff     = 0.052,        # t_test_output.txt:210
        ci_lower    = -0.182,       # t_test_output.txt:210
        ci_upper    = 0.021         # t_test_output.txt:210
      ),
      welch = list(
        t_stat      = -1.548,       # t_test_output.txt:213
        df          = 1901.144,     # t_test_output.txt:213
        p_one_sided = 0.061,        # t_test_output.txt:213
        p_two_sided = 0.122,        # t_test_output.txt:213
        mean_diff   = -0.080,       # t_test_output.txt:213
        se_diff     = 0.052,        # t_test_output.txt:213
        ci_lower    = -0.182,       # t_test_output.txt:213
        ci_upper    = 0.021         # t_test_output.txt:213
      )
    )
  ),

  # ---- Test 4b: income by gender, weighted, grouped by region -----------
  test_4b_income_by_gender_weighted_grouped = list(
    East = list(
      dp = 5L,
      equal_var = list(
        t_stat      = 1.681,        # t_test_output.txt:227
        df          = 447,          # t_test_output.txt:227
        p_one_sided = 0.047,        # t_test_output.txt:227
        p_two_sided = 0.093,        # t_test_output.txt:227
        mean_diff   = 219.82616,    # t_test_output.txt:227
        se_diff     = 130.76218,    # t_test_output.txt:227
        ci_lower    = -37.15804,    # t_test_output.txt:227
        ci_upper    = 476.81037     # t_test_output.txt:227
      ),
      welch = list(
        t_stat      = 1.674,        # t_test_output.txt:229
        df          = 431.197,      # t_test_output.txt:229
        p_one_sided = 0.047,        # t_test_output.txt:229
        p_two_sided = 0.095,        # t_test_output.txt:229
        mean_diff   = 219.82616,    # t_test_output.txt:229
        se_diff     = 131.30194,    # t_test_output.txt:229
        ci_lower    = -38.24528,    # t_test_output.txt:229
        ci_upper    = 477.89761     # t_test_output.txt:229
      )
    ),
    West = list(
      dp = 5L,
      equal_var = list(
        t_stat      = 0.009,        # t_test_output.txt:231
        df          = 1749,         # t_test_output.txt:231
        p_one_sided = 0.496,        # t_test_output.txt:231
        p_two_sided = 0.993,        # t_test_output.txt:231
        mean_diff   = 0.64379,      # t_test_output.txt:231
        se_diff     = 68.61062,     # t_test_output.txt:231
        ci_lower    = -133.92366,   # t_test_output.txt:231
        ci_upper    = 135.21124     # t_test_output.txt:231
      ),
      welch = list(
        t_stat      = 0.009,        # t_test_output.txt:233
        df          = 1740.190,     # t_test_output.txt:233
        p_one_sided = 0.496,        # t_test_output.txt:233
        p_two_sided = 0.993,        # t_test_output.txt:233
        mean_diff   = 0.64379,      # t_test_output.txt:233
        se_diff     = 68.49794,     # t_test_output.txt:233
        ci_lower    = -133.70315,   # t_test_output.txt:233
        ci_upper    = 134.99072     # t_test_output.txt:233
      )
    )
  ),

  # ---- Test 4c: age by gender, weighted, grouped by region --------------
  test_4c_age_by_gender_weighted_grouped = list(
    East = list(
      dp = 5L,
      equal_var = list(
        t_stat      = -0.669,       # t_test_output.txt:248
        df          = 507,          # t_test_output.txt:248
        p_one_sided = 0.252,        # t_test_output.txt:248
        p_two_sided = 0.503,        # t_test_output.txt:248
        mean_diff   = -1.04503,     # t_test_output.txt:248
        se_diff     = 1.56092,      # t_test_output.txt:248
        ci_lower    = -4.11170,     # t_test_output.txt:248
        ci_upper    = 2.02163       # t_test_output.txt:248
      ),
      welch = list(
        t_stat      = -0.669,       # t_test_output.txt:249
        df          = 502.251,      # t_test_output.txt:249
        p_one_sided = 0.252,        # t_test_output.txt:249
        p_two_sided = 0.504,        # t_test_output.txt:249
        mean_diff   = -1.04503,     # t_test_output.txt:249
        se_diff     = 1.56272,      # t_test_output.txt:249
        ci_lower    = -4.11530,     # t_test_output.txt:249
        ci_upper    = 2.02524       # t_test_output.txt:249
      )
    ),
    West = list(
      dp = 5L,
      equal_var = list(
        t_stat      = 0.462,        # t_test_output.txt:250
        df          = 2005,         # t_test_output.txt:250
        p_one_sided = 0.322,        # t_test_output.txt:250
        p_two_sided = 0.644,        # t_test_output.txt:250
        mean_diff   = 0.34999,      # t_test_output.txt:250
        se_diff     = 0.75709,      # t_test_output.txt:250
        ci_lower    = -1.13478,     # t_test_output.txt:250
        ci_upper    = 1.83475       # t_test_output.txt:250
      ),
      welch = list(
        t_stat      = 0.462,        # t_test_output.txt:251
        df          = 1978.869,     # t_test_output.txt:251
        p_one_sided = 0.322,        # t_test_output.txt:251
        p_two_sided = 0.644,        # t_test_output.txt:251
        mean_diff   = 0.34999,      # t_test_output.txt:251
        se_diff     = 0.75702,      # t_test_output.txt:251
        ci_lower    = -1.13466,     # t_test_output.txt:251
        ci_upper    = 1.83463       # t_test_output.txt:251
      )
    )
  ),

  # =========================================================================
  # AUXILIARY SCENARIOS
  # =========================================================================

  # ---- Test 5a: one-sample, income, mu = 5000 ---------------------------
  test_5a_income_one_sample = list(
    dp = c(mean = 4L, sd = 5L, se = 5L, mean_diff = 5L, ci = 4L),  # decimals printed
    n           = 2186,                # t_test_output.txt:263
    mean        = 3753.9341,           # t_test_output.txt:263
    sd          = 1432.80161,          # t_test_output.txt:263
    se          = 30.64510,            # t_test_output.txt:263
    t_stat      = -40.661,             # t_test_output.txt:270
    df          = 2185,                # t_test_output.txt:270
    p_one_sided = "<.001",             # t_test_output.txt:270
    p_two_sided = "<.001",             # t_test_output.txt:270
    mean_diff   = -1246.06587,         # t_test_output.txt:270
    ci_lower    = -1306.1624,          # t_test_output.txt:270
    ci_upper    = -1185.9693           # t_test_output.txt:270
  ),

  # ---- Test 5b: one-sample, age, mu = 45 --------------------------------
  test_5b_age_one_sample = list(
    dp = c(mean = 4L, sd = 5L, se = 5L, mean_diff = 5L, ci = 4L),  # decimals printed
    n           = 2500,                # t_test_output.txt:280
    mean        = 50.5496,             # t_test_output.txt:280
    sd          = 16.97602,            # t_test_output.txt:280
    se          = 0.33952,             # t_test_output.txt:280
    t_stat      = 16.345,              # t_test_output.txt:287
    df          = 2499,                # t_test_output.txt:287
    p_one_sided = "<.001",             # t_test_output.txt:287
    p_two_sided = "<.001",             # t_test_output.txt:287
    mean_diff   = 5.54960,             # t_test_output.txt:287
    ci_lower    = 4.8838,              # t_test_output.txt:287
    ci_upper    = 6.2154               # t_test_output.txt:287
  ),

  # ---- Test 6: multiple variables at once -------------------------------
  # Only the additional rows (trust_government / trust_media / trust_science).
  # The life_satisfaction / income / age rows duplicate Tests 1b-d.
  test_6_multi_var_trust = list(
    trust_government = list(
      dp = 3L,
      equal_var = list(
        t_stat      = -0.673,           # t_test_output.txt:310
        df          = 2352,             # t_test_output.txt:310
        p_one_sided = 0.250,            # t_test_output.txt:310
        p_two_sided = 0.501,            # t_test_output.txt:310
        mean_diff   = -0.032,           # t_test_output.txt:310
        se_diff     = 0.048,            # t_test_output.txt:310
        ci_lower    = -0.126,           # t_test_output.txt:310
        ci_upper    = 0.062             # t_test_output.txt:310
      ),
      welch = list(
        t_stat      = -0.672,           # t_test_output.txt:313
        df          = 2313.961,         # t_test_output.txt:313
        p_one_sided = 0.251,            # t_test_output.txt:313
        p_two_sided = 0.501,            # t_test_output.txt:313
        mean_diff   = -0.032,           # t_test_output.txt:313
        se_diff     = 0.048,            # t_test_output.txt:313
        ci_lower    = -0.127,           # t_test_output.txt:313
        ci_upper    = 0.062             # t_test_output.txt:313
      )
    ),
    trust_media = list(
      dp = 3L,
      equal_var = list(
        t_stat      = -2.172,           # t_test_output.txt:314
        df          = 2365,             # t_test_output.txt:314
        p_one_sided = 0.015,            # t_test_output.txt:314
        p_two_sided = 0.030,            # t_test_output.txt:314
        mean_diff   = -0.104,           # t_test_output.txt:314
        se_diff     = 0.048,            # t_test_output.txt:314
        ci_lower    = -0.198,           # t_test_output.txt:314
        ci_upper    = -0.010            # t_test_output.txt:314
      ),
      welch = list(
        t_stat      = -2.172,           # t_test_output.txt:316
        df          = 2342.242,         # t_test_output.txt:316
        p_one_sided = 0.015,            # t_test_output.txt:316
        p_two_sided = 0.030,            # t_test_output.txt:316
        mean_diff   = -0.104,           # t_test_output.txt:316
        se_diff     = 0.048,            # t_test_output.txt:316
        ci_lower    = -0.198,           # t_test_output.txt:316
        ci_upper    = -0.010            # t_test_output.txt:316
      )
    ),
    trust_science = list(
      dp = 3L,
      equal_var = list(
        t_stat      = -1.490,           # t_test_output.txt:317
        df          = 2396,             # t_test_output.txt:317
        p_one_sided = 0.068,            # t_test_output.txt:317
        p_two_sided = 0.136,            # t_test_output.txt:317
        mean_diff   = -0.063,           # t_test_output.txt:317
        se_diff     = 0.042,            # t_test_output.txt:317
        ci_lower    = -0.145,           # t_test_output.txt:317
        ci_upper    = 0.020             # t_test_output.txt:317
      ),
      welch = list(
        t_stat      = -1.487,           # t_test_output.txt:320
        df          = 2350.552,         # t_test_output.txt:320
        p_one_sided = 0.069,            # t_test_output.txt:320
        p_two_sided = 0.137,            # t_test_output.txt:320
        mean_diff   = -0.063,           # t_test_output.txt:320
        se_diff     = 0.042,            # t_test_output.txt:320
        ci_lower    = -0.145,           # t_test_output.txt:320
        ci_upper    = 0.020             # t_test_output.txt:320
      )
    )
  ),

  # ---- Test 7a: alternative 90% CI --------------------------------------
  # t/df/p/mean_diff/se_diff are those of Test 1b; only the CI differs.
  test_7a_life_by_gender_90ci = list(
    dp = 3L,
    equal_var = list(
      t_stat      = -1.019,           # t_test_output.txt:334
      df          = 2419,             # t_test_output.txt:334
      p_one_sided = 0.154,            # t_test_output.txt:334
      p_two_sided = 0.308,            # t_test_output.txt:334
      mean_diff   = -0.048,           # t_test_output.txt:334
      se_diff     = 0.047,            # t_test_output.txt:334
      ci_lower    = -0.125,           # t_test_output.txt:334
      ci_upper    = 0.029             # t_test_output.txt:334
    ),
    welch = list(
      t_stat      = -1.018,           # t_test_output.txt:336
      df          = 2384.147,         # t_test_output.txt:336
      p_one_sided = 0.154,            # t_test_output.txt:336
      p_two_sided = 0.309,            # t_test_output.txt:336
      mean_diff   = -0.048,           # t_test_output.txt:336
      se_diff     = 0.047,            # t_test_output.txt:336
      ci_lower    = -0.125,           # t_test_output.txt:336
      ci_upper    = 0.029             # t_test_output.txt:336
    )
  ),

  # ---- Test 7b: alternative 99% CI --------------------------------------
  test_7b_life_by_gender_99ci = list(
    dp = 3L,
    equal_var = list(
      t_stat      = -1.019,           # t_test_output.txt:350
      df          = 2419,             # t_test_output.txt:350
      p_one_sided = 0.154,            # t_test_output.txt:350
      p_two_sided = 0.308,            # t_test_output.txt:350
      mean_diff   = -0.048,           # t_test_output.txt:350
      se_diff     = 0.047,            # t_test_output.txt:350
      ci_lower    = -0.169,           # t_test_output.txt:350
      ci_upper    = 0.073             # t_test_output.txt:350
    ),
    welch = list(
      t_stat      = -1.018,           # t_test_output.txt:352
      df          = 2384.147,         # t_test_output.txt:352
      p_one_sided = 0.154,            # t_test_output.txt:352
      p_two_sided = 0.309,            # t_test_output.txt:352
      mean_diff   = -0.048,           # t_test_output.txt:352
      se_diff     = 0.047,            # t_test_output.txt:352
      ci_lower    = -0.169,           # t_test_output.txt:352
      ci_upper    = 0.073             # t_test_output.txt:352
    )
  )
)


# =============================================================================
# COMPARISON HELPERS
# =============================================================================
# Per Charter §8: each helper internally calls assert_spss() for every
# numerical comparison. No tolerance literals, no NA-as-match defaulting.
#
# Each test passes a `run(alternative)` closure that calls t_test(); the
# helpers call it once two-sided and once with the one-sided alternative in
# the direction of the SPSS t ("less" for t < 0, "greater" otherwise), so the
# SPSS "One-Sided p" is compared with mariposa's own one-sided test.
# =============================================================================

# One-sided alternative in the direction of the observed (SPSS) t
one_sided_alternative <- function(t_spss) if (t_spss < 0) "less" else "greater"

# Row selector for grouped / multi-variable results (exactly one row)
row_where <- function(...) {
  crit <- list(...)
  function(res) {
    sel <- rep(TRUE, nrow(res))
    for (k in names(crit)) sel <- sel & as.character(res[[k]]) == crit[[k]]
    if (sum(sel) != 1L) {
      stop(sprintf("row_where(): expected exactly 1 row for %s; got %d",
                   paste(names(crit), crit, sep = "=", collapse = ", "),
                   sum(sel)), call. = FALSE)
    }
    res[sel, , drop = FALSE]
  }
}
first_row <- function(res) res[1, , drop = FALSE]


#' Compare a one-sample t-test against SPSS One-Sample Statistics / Test
#'
#' mariposa's one-sample $results row: t_stat, df, p_value, mean_diff
#' (observed mean - mu, as SPSS), conf_int_lower/upper (CI of the
#' difference, as SPSS), n1; group_stats: means, sd, se (SPSS "One-Sample
#' Statistics"). `spss$dp` holds the decimals SPSS printed for mean, sd,
#' se, mean_diff and the CI.
compare_one_sample <- function(run, spss, scenario) {
  r  <- run()$results[1, ]
  dp <- spss$dp

  assert_spss(r$t_stat, spss$t_stat, tier = "display", precision = 3,
              label = sprintf("[%s] t-statistic", scenario))
  # df = sum(w) - 1 is non-integer when weighted; SPSS prints an integer.
  assert_spss(r$df, spss$df, tier = "display", precision = 0,
              label = sprintf("[%s] df", scenario))
  assert_spss(r$p_value, spss$p_two_sided, tier = "display", precision = 3,
              what = "p_value", label = sprintf("[%s] two-sided p", scenario))

  alt <- one_sided_alternative(spss$t_stat)
  r1 <- run(alt)$results[1, ]
  assert_spss(r1$p_value, spss$p_one_sided, tier = "display", precision = 3,
              what = "p_value",
              label = sprintf("[%s] one-sided p (alternative = \"%s\")", scenario, alt))

  assert_spss(r$mean_diff, spss$mean_diff, tier = "display",
              precision = dp[["mean_diff"]],
              label = sprintf("[%s] Mean Difference", scenario))
  assert_spss(r$conf_int_lower, spss$ci_lower, tier = "display",
              precision = dp[["ci"]], label = sprintf("[%s] CI lower", scenario))
  assert_spss(r$conf_int_upper, spss$ci_upper, tier = "display",
              precision = dp[["ci"]], label = sprintf("[%s] CI upper", scenario))

  # N: exact for unweighted, the rounded sum of weights when weighted
  assert_spss_count(r$n1, spss$n, label = sprintf("[%s] N", scenario))

  # One-Sample Statistics
  st <- r$group_stats[[1]]
  assert_spss_count(st$n, spss$n, label = sprintf("[%s] Statistics N", scenario))
  assert_spss(st$means, spss$mean, tier = "display", precision = dp[["mean"]],
              label = sprintf("[%s] Mean", scenario))
  assert_spss(st$sd, spss$sd, tier = "display", precision = dp[["sd"]],
              label = sprintf("[%s] Std. Deviation", scenario))
  assert_spss(st$se, spss$se, tier = "display", precision = dp[["se"]],
              label = sprintf("[%s] Std. Error Mean", scenario))

  invisible(NULL)
}


#' Compare both rows of an SPSS Independent Samples Test
#'
#' mariposa keeps both rows: $equal_var_result (Student) and
#' $unequal_var_result (Welch) hold htest objects (statistic, parameter,
#' p.value, estimate, stderr, conf.int). The headline $results columns
#' report the Welch row (var.equal = FALSE by default). `spss$dp` holds the
#' decimals SPSS printed for Mean Difference, Std. Error Difference and CI.
#'
#' @param run    function(alternative = "two.sided") returning a t_test result
#' @param spss   spss_values entry with $dp, $equal_var and $welch
#' @param select picks the result row (grouped / multi-variable results)
compare_two_sample <- function(run, spss, scenario, select = first_row) {
  r  <- select(run()$results)
  dp <- spss$dp

  rows <- list(equal_var = r$equal_var_result[[1]],
               welch     = r$unequal_var_result[[1]])
  for (branch in names(rows)) {
    o <- rows[[branch]]
    s <- spss[[branch]]
    lab <- sprintf("[%s, %s]", scenario,
                   if (branch == "welch") "Welch" else "equal-var")
    if (is.null(o)) {
      stop(sprintf("%s R-side result row is NULL", lab), call. = FALSE)
    }

    assert_spss(as.numeric(o$statistic), s$t_stat, tier = "display",
                precision = 3, label = paste(lab, "t"))
    # Welch df: SPSS prints 3 decimals. Equal-variance df = sw1 + sw2 - 2
    # (non-integer when weighted; SPSS prints an integer).
    assert_spss(as.numeric(o$parameter), s$df, tier = "display",
                precision = if (branch == "welch") 3L else 0L,
                label = paste(lab, "df"))
    assert_spss(as.numeric(o$p.value), s$p_two_sided, tier = "display",
                precision = 3, what = "p_value", label = paste(lab, "two-sided p"))
    assert_spss(as.numeric(o$estimate[1] - o$estimate[2]), s$mean_diff,
                tier = "display", precision = dp,
                label = paste(lab, "Mean Difference"))
    assert_spss(as.numeric(o$stderr), s$se_diff, tier = "display",
                precision = dp, label = paste(lab, "Std. Error Difference"))
    assert_spss(as.numeric(o$conf.int[1]), s$ci_lower, tier = "display",
                precision = dp, label = paste(lab, "CI lower"))
    assert_spss(as.numeric(o$conf.int[2]), s$ci_upper, tier = "display",
                precision = dp, label = paste(lab, "CI upper"))
  }

  # ---- Headline columns of $results: the Welch row ---------------------
  w <- spss$welch
  lab <- sprintf("[%s, $results]", scenario)
  assert_spss(r$t_stat, w$t_stat, tier = "display", precision = 3,
              label = paste(lab, "t_stat"))
  assert_spss(r$df, w$df, tier = "display", precision = 3,
              label = paste(lab, "df"))
  assert_spss(r$p_value, w$p_two_sided, tier = "display", precision = 3,
              what = "p_value", label = paste(lab, "p_value"))
  assert_spss(r$mean_diff, w$mean_diff, tier = "display", precision = dp,
              label = paste(lab, "mean_diff"))
  assert_spss(r$conf_int_lower, w$ci_lower, tier = "display", precision = dp,
              label = paste(lab, "conf_int_lower"))
  assert_spss(r$conf_int_upper, w$ci_upper, tier = "display", precision = dp,
              label = paste(lab, "conf_int_upper"))

  # ---- One-sided p through alternative = "less" / "greater" ------------
  alt <- one_sided_alternative(spss$equal_var$t_stat)
  ro <- select(run(alt)$results)
  lab <- sprintf("[%s] one-sided p (alternative = \"%s\")", scenario, alt)
  assert_spss(as.numeric(ro$equal_var_result[[1]]$p.value),
              spss$equal_var$p_one_sided, tier = "display", precision = 3,
              what = "p_value", label = paste(lab, "equal-var"))
  assert_spss(as.numeric(ro$unequal_var_result[[1]]$p.value),
              spss$welch$p_one_sided, tier = "display", precision = 3,
              what = "p_value", label = paste(lab, "Welch"))
  assert_spss(ro$p_value, spss$welch$p_one_sided, tier = "display",
              precision = 3, what = "p_value", label = paste(lab, "$results"))

  # ---- Total N ---------------------------------------------------------
  # SPSS's equal-variance df is sw1 + sw2 - 2 (unrounded sum of weights),
  # printed as an integer; mariposa's n1/n2 are rounded per group, so the
  # total is recovered from the df instead.
  assert_spss(as.numeric(rows$equal_var$parameter) + 2, spss$equal_var$df + 2,
              tier = "display", precision = 0,
              label = sprintf("[%s] total N (via df+2)", scenario))

  invisible(NULL)
}


# =============================================================================
# DATA SETUP
# =============================================================================

data(survey_data, envir = environment())


# =============================================================================
# SCENARIO 1 — UNWEIGHTED / UNGROUPED
# =============================================================================

test_that("Test 1a: one-sample, life_satisfaction, mu = 3.0", {
  run <- function(alternative = "two.sided") {
    t_test(survey_data, life_satisfaction, mu = 3.0, alternative = alternative)
  }
  compare_one_sample(run, spss_values$test_1a_one_sample,
                     "1a: one-sample life_sat mu=3")
})

test_that("Test 1b: two-sample, life_satisfaction by gender", {
  run <- function(alternative = "two.sided") {
    t_test(survey_data, life_satisfaction, group = gender,
           alternative = alternative)
  }
  compare_two_sample(run, spss_values$test_1b_life_by_gender,
                     "1b: life_sat by gender")
})

test_that("Test 1c: two-sample, income by gender", {
  run <- function(alternative = "two.sided") {
    t_test(survey_data, income, group = gender, alternative = alternative)
  }
  compare_two_sample(run, spss_values$test_1c_income_by_gender,
                     "1c: income by gender")
})

test_that("Test 1d: two-sample, age by gender", {
  run <- function(alternative = "two.sided") {
    t_test(survey_data, age, group = gender, alternative = alternative)
  }
  compare_two_sample(run, spss_values$test_1d_age_by_gender,
                     "1d: age by gender")
})


# =============================================================================
# SCENARIO 2 — WEIGHTED / UNGROUPED
# =============================================================================

test_that("Test 2a: one-sample weighted, life_satisfaction, mu = 3.0", {
  run <- function(alternative = "two.sided") {
    t_test(survey_data, life_satisfaction, mu = 3.0, weights = sampling_weight,
           alternative = alternative)
  }
  compare_one_sample(run, spss_values$test_2a_one_sample_weighted,
                     "2a: weighted one-sample life_sat mu=3")
})

test_that("Test 2b: two-sample weighted, life_satisfaction by gender", {
  run <- function(alternative = "two.sided") {
    t_test(survey_data, life_satisfaction, group = gender,
           weights = sampling_weight, alternative = alternative)
  }
  compare_two_sample(run, spss_values$test_2b_life_by_gender_weighted,
                     "2b: weighted life_sat by gender")
})

test_that("Test 2c: two-sample weighted, income by gender", {
  run <- function(alternative = "two.sided") {
    t_test(survey_data, income, group = gender, weights = sampling_weight,
           alternative = alternative)
  }
  compare_two_sample(run, spss_values$test_2c_income_by_gender_weighted,
                     "2c: weighted income by gender")
})

test_that("Test 2d: two-sample weighted, age by gender", {
  run <- function(alternative = "two.sided") {
    t_test(survey_data, age, group = gender, weights = sampling_weight,
           alternative = alternative)
  }
  compare_two_sample(run, spss_values$test_2d_age_by_gender_weighted,
                     "2d: weighted age by gender")
})


# =============================================================================
# SCENARIO 3 — UNWEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 3a: life_satisfaction by gender, grouped by region", {
  run <- function(alternative = "two.sided") {
    survey_data |>
      group_by(region) |>
      t_test(life_satisfaction, group = gender, alternative = alternative)
  }
  for (rg in c("East", "West")) {
    compare_two_sample(run, spss_values$test_3a_life_by_gender_grouped[[rg]],
                       sprintf("3a: life_sat by gender [%s]", rg),
                       select = row_where(region = rg))
  }
})

test_that("Test 3b: income by gender, grouped by region", {
  run <- function(alternative = "two.sided") {
    survey_data |>
      group_by(region) |>
      t_test(income, group = gender, alternative = alternative)
  }
  for (rg in c("East", "West")) {
    compare_two_sample(run, spss_values$test_3b_income_by_gender_grouped[[rg]],
                       sprintf("3b: income by gender [%s]", rg),
                       select = row_where(region = rg))
  }
})

test_that("Test 3c: age by gender, grouped by region", {
  run <- function(alternative = "two.sided") {
    survey_data |>
      group_by(region) |>
      t_test(age, group = gender, alternative = alternative)
  }
  for (rg in c("East", "West")) {
    compare_two_sample(run, spss_values$test_3c_age_by_gender_grouped[[rg]],
                       sprintf("3c: age by gender [%s]", rg),
                       select = row_where(region = rg))
  }
})


# =============================================================================
# SCENARIO 4 — WEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 4a: life_satisfaction by gender, weighted, grouped by region", {
  run <- function(alternative = "two.sided") {
    survey_data |>
      group_by(region) |>
      t_test(life_satisfaction, group = gender, weights = sampling_weight,
             alternative = alternative)
  }
  for (rg in c("East", "West")) {
    compare_two_sample(run,
                       spss_values$test_4a_life_by_gender_weighted_grouped[[rg]],
                       sprintf("4a: weighted life_sat by gender [%s]", rg),
                       select = row_where(region = rg))
  }
})

test_that("Test 4b: income by gender, weighted, grouped by region", {
  run <- function(alternative = "two.sided") {
    survey_data |>
      group_by(region) |>
      t_test(income, group = gender, weights = sampling_weight,
             alternative = alternative)
  }
  for (rg in c("East", "West")) {
    compare_two_sample(run,
                       spss_values$test_4b_income_by_gender_weighted_grouped[[rg]],
                       sprintf("4b: weighted income by gender [%s]", rg),
                       select = row_where(region = rg))
  }
})

test_that("Test 4c: age by gender, weighted, grouped by region", {
  run <- function(alternative = "two.sided") {
    survey_data |>
      group_by(region) |>
      t_test(age, group = gender, weights = sampling_weight,
             alternative = alternative)
  }
  for (rg in c("East", "West")) {
    compare_two_sample(run,
                       spss_values$test_4c_age_by_gender_weighted_grouped[[rg]],
                       sprintf("4c: weighted age by gender [%s]", rg),
                       select = row_where(region = rg))
  }
})


# =============================================================================
# AUXILIARY SCENARIOS
# =============================================================================

test_that("Test 5a: one-sample, income, mu = 5000", {
  run <- function(alternative = "two.sided") {
    t_test(survey_data, income, mu = 5000, alternative = alternative)
  }
  compare_one_sample(run, spss_values$test_5a_income_one_sample,
                     "5a: one-sample income mu=5000")
})

test_that("Test 5b: one-sample, age, mu = 45", {
  run <- function(alternative = "two.sided") {
    t_test(survey_data, age, mu = 45, alternative = alternative)
  }
  compare_one_sample(run, spss_values$test_5b_age_one_sample,
                     "5b: one-sample age mu=45")
})

test_that("Test 6: multiple variables simultaneously", {
  # SPSS Test 6 runs t-tests on six DVs at once. The life_satisfaction /
  # income / age rows (t_test_output.txt:300-309) print exactly the values
  # of Tests 1b-1d and are compared with those entries; the trust_* rows
  # have their own entries.
  run <- function(alternative = "two.sided") {
    t_test(survey_data, life_satisfaction, income, age,
           trust_government, trust_media, trust_science,
           group = gender, alternative = alternative)
  }
  result <- run()
  # SPSS order of the rows
  expect_identical(result$results$Variable,
                   c("life_satisfaction", "income", "age",
                     "trust_government", "trust_media", "trust_science"))

  entries <- c(spss_values[c("test_1b_life_by_gender",
                             "test_1c_income_by_gender",
                             "test_1d_age_by_gender")],
               spss_values$test_6_multi_var_trust)
  names(entries) <- c("life_satisfaction", "income", "age",
                      names(spss_values$test_6_multi_var_trust))
  for (v in names(entries)) {
    compare_two_sample(run, entries[[v]], sprintf("6: %s by gender", v),
                       select = row_where(Variable = v))
  }
})

test_that("Test 7a: alternative CI level 90%, life_satisfaction by gender", {
  run <- function(alternative = "two.sided") {
    t_test(survey_data, life_satisfaction, group = gender, conf.level = 0.90,
           alternative = alternative)
  }
  compare_two_sample(run, spss_values$test_7a_life_by_gender_90ci,
                     "7a: life_sat by gender, 90% CI")
})

test_that("Test 7b: alternative CI level 99%, life_satisfaction by gender", {
  run <- function(alternative = "two.sided") {
    t_test(survey_data, life_satisfaction, group = gender, conf.level = 0.99,
           alternative = alternative)
  }
  compare_two_sample(run, spss_values$test_7b_life_by_gender_99ci,
                     "7b: life_sat by gender, 99% CI")
})


# =============================================================================
# EDGE CASES
# =============================================================================
# Per Charter §8: edge cases must produce value assertions, not just
# expect_no_error(). When SPSS has documented behavior, compare. When R-only,
# snapshot. When the behavior is purely structural (e.g., NA propagation),
# assert the specific structural property, not just absence-of-error.
# =============================================================================

test_that("Edge case: missing values reduce N exactly by the number of NAs", {
  test_data <- survey_data
  # Introduce exactly 10 NAs in life_satisfaction (above any pre-existing NAs)
  na_indices <- which(!is.na(test_data$life_satisfaction))[1:10]
  test_data$life_satisfaction[na_indices] <- NA

  baseline <- survey_data |> t_test(life_satisfaction, group = gender)
  reduced  <- test_data  |> t_test(life_satisfaction, group = gender)

  baseline_n <- baseline$results$n1[1] + baseline$results$n2[1]
  reduced_n  <- reduced$results$n1[1]  + reduced$results$n2[1]

  # Exactly 10 more NA cases should be excluded.
  assert_spss_count(reduced_n, baseline_n - 10L,
                    label = "missing-value edge case — N reduction")
})

test_that("Edge case: tidyselect helper selects three trust_* variables", {
  result <- survey_data |>
    t_test(starts_with("trust_"), group = gender)

  expect_equal(nrow(result$results), 3L)
  expect_setequal(result$results$Variable,
                  c("trust_government", "trust_media", "trust_science"))
})
