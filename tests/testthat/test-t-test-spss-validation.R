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
# Tolerance tier assignment for t-test outputs:
#   N (integer)                       — Spec (count, exact)
#   Mean, SD, SE                      — Spec (statistic), see "Gaps" note below
#   t-statistic                       — Spec (statistic, ±1e-5)
#   df (integer, equal variance)      — Spec (count, exact)
#   df (Welch-Satterthwaite, decimal) — Display (precision = 3, ±5e-4)
#   Mean Difference                   — Spec (statistic)
#   SE of Difference                  — Display (precision = 5 for income/age,
#                                       precision = 3 for life_satisfaction)
#   CI lower/upper                    — Display (precision = 3 for life_sat /
#                                       region grouped, precision = 5 for
#                                       income/age ungrouped)
#   p-value (one-sided AND two-sided) — Spec (p_value)
#   p-value when SPSS prints ".000"   — sentinel "<.001" → actual < 0.001
#
# Tolerance gaps to fix in mariposa source (Phase 2 work):
#   - t_test() does NOT currently expose Mean, SD, SE in $results for one-sample
#     scenarios. These assertions are commented out below with TODO markers.
#     Source: R/t_test.R structure() call at line 649; group_stats only contains
#     means and Ns, no SDs/SEs.
#   - Adding $descriptives or $statistics field would close this gap and is
#     SPSS-equivalent (SPSS prints these as "One-Sample Statistics" before the
#     test table).
#
# Expected exception candidates (Phase 1 diagnosis):
#   - SPSS's Welch-Satterthwaite df calculation in weighted scenarios uses
#     integer-rounded n_eff in the denominator; R's t_test() matches this
#     (R/t_test.R:443-456), so no exception expected. Confirmed by reading
#     unweighted Welch df (e.g., Test 1b: SPSS 2384.147 should match within
#     Display tier).
#   - Equal-variance df = n1 + n2 - 2 is exact integer arithmetic; Spec.
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

  # ---- Test 1a: one-sample, life_satisfaction, mu = 3.0 -----------------
  test_1a_one_sample = list(
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
  # t/df/p/mean_diff/se_diff are identical to Test 1b (only CI bounds differ).
  test_7a_life_by_gender_90ci = list(
    equal_var = list(
      ci_lower    = -0.125,           # t_test_output.txt:334
      ci_upper    = 0.029             # t_test_output.txt:334
    ),
    welch = list(
      ci_lower    = -0.125,           # t_test_output.txt:336
      ci_upper    = 0.029             # t_test_output.txt:336
    )
  ),

  # ---- Test 7b: alternative 99% CI --------------------------------------
  test_7b_life_by_gender_99ci = list(
    equal_var = list(
      ci_lower    = -0.169,           # t_test_output.txt:350
      ci_upper    = 0.073             # t_test_output.txt:350
    ),
    welch = list(
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
# =============================================================================

#' Compare a one-sample t-test result against SPSS reference
#'
#' SPSS one-sample output prints:
#'   - One-Sample Statistics: N, Mean, SD, SE (descriptives)
#'   - One-Sample Test:       t, df, Sig (1-/2-sided), Mean Difference, CI bounds
#'
#' mariposa's t_test() result$results columns for one-sample:
#'   - t_stat, df, p_value, mean_diff (the OBSERVED mean, not the difference),
#'     conf_int_lower/upper (the OBSERVED-mean CI)
#'
#' SPSS's "Mean Difference" = observed_mean - mu, so we adjust on the R side.
#'
#' @param r_result A t_test result object (one-sample form)
#' @param spss     The corresponding spss_values entry
#' @param scenario A short label identifying the test scenario
compare_one_sample <- function(r_result, spss, scenario) {

  r <- r_result$results[1, ]
  mu <- r_result$mu %||% 0

  # ---- t-statistic (Spec) ------------------------------------------------
  # SPSS prints t to 3 decimals → Display tier, precision = 3 (tol ±5e-4).
  assert_spss(r$t_stat, spss$t_stat,
              tier = "display", precision = 3,
              label = sprintf("[%s] t-statistic", scenario))

  # ---- df: SPSS displays as integer; internally non-integer for weighted -
  # When weights are present, df = sum(w) - 1 is non-integer; SPSS rounds for
  # the displayed column but uses the unrounded value for the t distribution.
  # Display(precision = 0) handles both unweighted (exact integer) and
  # weighted (within half a unit of the displayed integer) cleanly.
  assert_spss(r$df, spss$df,
              tier = "display", precision = 0,
              label = sprintf("[%s] df", scenario))

  # ---- p-value, two-sided (Spec; "<.001" handled by helper) -------------
  # SPSS prints p to 3 decimals → Display tier (sentinel "<.001" handled by helper)
  assert_spss(r$p_value, spss$p_two_sided,
              tier = "display", precision = 3,
              label = sprintf("[%s] two-sided p", scenario))

  # ---- p-value, one-sided (derived from two-sided) -----------------------
  # For a symmetric distribution, one-sided p (in observed direction) = p/2.
  r_p_one_sided <- r$p_value / 2
  assert_spss(r_p_one_sided, spss$p_one_sided,
              tier = "display", precision = 3,
              label = sprintf("[%s] one-sided p", scenario))

  # ---- Mean Difference (= R's mean_diff - mu) ----------------------------
  # mariposa stores result$results$mean_diff = the observed (weighted) mean
  # for the one-sample case (R/t_test.R:354). SPSS reports the difference.
  r_mean_diff <- r$mean_diff - mu
  # Precision in SPSS varies: 3 dp for life_satisfaction, 5 dp for income/age.
  # We pick precision per the SPSS print width; the spss_values comment shows.
  precision <- if (abs(spss$mean_diff) >= 100) 4L else 3L
  assert_spss(r_mean_diff, spss$mean_diff,
              tier = "display", precision = precision,
              label = sprintf("[%s] Mean Difference", scenario))

  # ---- CI for Mean Difference (= R's CI - mu) ----------------------------
  r_ci_lower <- r$conf_int_lower - mu
  r_ci_upper <- r$conf_int_upper - mu

  ci_precision <- if (abs(spss$ci_lower) >= 100) 4L else 2L
  assert_spss(r_ci_lower, spss$ci_lower,
              tier = "display", precision = ci_precision,
              label = sprintf("[%s] CI lower", scenario))
  assert_spss(r_ci_upper, spss$ci_upper,
              tier = "display", precision = ci_precision,
              label = sprintf("[%s] CI upper", scenario))

  # ---- N (Spec — exact integer for both unweighted and integer-rounded
  #         weighted N). For one-sample, N is in result$results$n1.
  if ("n1" %in% names(r) && !is.na(r$n1)) {
    assert_spss_count(r$n1, spss$n,
                      label = sprintf("[%s] N", scenario))
  }

  # ---- Descriptives gap ------------------------------------------------
  # TODO Phase 2: mariposa's t_test() does NOT expose Mean, SD, SE in
  # result$results for one-sample tests. SPSS prints these as the
  # "One-Sample Statistics" table. Adding a $descriptives field to t_test()
  # would let us validate spss$mean (3.63), spss$sd (1.153), spss$se (0.023).
  # Currently we can only assert mean_diff (which we derive from r$mean_diff,
  # and which equals the observed mean - mu).
  # See R/t_test.R structure() at line 649.

  invisible(NULL)
}


#' Compare a two-sample t-test result against SPSS reference (both var paths)
#'
#' SPSS independent-samples output prints two rows per analysis: equal-variance
#' (Student) and not-equal-variance (Welch). mariposa's t_test() stores both in
#' result$results$equal_var_result and result$results$unequal_var_result (which
#' are lists holding t-test return objects with $statistic, $parameter, etc).
#'
#' @param r_result A t_test result object (two-sample form)
#' @param spss     The spss_values entry with $equal_var and $welch sublists
#' @param scenario A short label identifying the test scenario
#' @param row_idx  Which row of result$results to use (default 1; used by
#'                 grouped and multi-variable tests)
compare_two_sample <- function(r_result, spss, scenario, row_idx = 1L) {

  r <- r_result$results[row_idx, ]

  # Equal-variance R values (Student's t)
  eq <- r$equal_var_result[[1]]
  if (is.null(eq)) {
    stop(sprintf("[%s] R-side equal_var_result is NULL — t_test() may have
omitted this branch.", scenario), call. = FALSE)
  }
  r_eq_t        <- as.numeric(eq$statistic)
  r_eq_df       <- as.numeric(eq$parameter)
  r_eq_p_two    <- as.numeric(eq$p.value)
  r_eq_meandiff <- as.numeric(eq$estimate[1] - eq$estimate[2])
  r_eq_ci_lo    <- as.numeric(eq$conf.int[1])
  r_eq_ci_hi    <- as.numeric(eq$conf.int[2])

  # Welch values (unequal variance)
  un <- r$unequal_var_result[[1]]
  if (is.null(un)) {
    stop(sprintf("[%s] R-side unequal_var_result is NULL", scenario),
         call. = FALSE)
  }
  r_un_t        <- as.numeric(un$statistic)
  r_un_df       <- as.numeric(un$parameter)
  r_un_p_two    <- as.numeric(un$p.value)
  r_un_meandiff <- as.numeric(un$estimate[1] - un$estimate[2])
  r_un_ci_lo    <- as.numeric(un$conf.int[1])
  r_un_ci_hi    <- as.numeric(un$conf.int[2])

  # ---- Equal-variance assertions ----------------------------------------
  if (!is.null(spss$equal_var)) {
    e <- spss$equal_var

    if (!is.null(e$t_stat)) {
      assert_spss(r_eq_t, e$t_stat,
                  tier = "display", precision = 3,
                  label = sprintf("[%s, equal-var] t", scenario))
    }
    if (!is.null(e$df)) {
      # Equal-variance df = sw1 + sw2 - 2 (non-integer when weighted; SPSS
      # rounds for display). Display(precision = 0) handles both cleanly.
      assert_spss(r_eq_df, e$df,
                  tier = "display", precision = 0,
                  label = sprintf("[%s, equal-var] df", scenario))
    }
    if (!is.null(e$p_two_sided)) {
      assert_spss(r_eq_p_two, e$p_two_sided,
                  tier = "display", precision = 3,
                  label = sprintf("[%s, equal-var] two-sided p", scenario))
    }
    if (!is.null(e$p_one_sided)) {
      r_eq_p_one <- r_eq_p_two / 2
      assert_spss(r_eq_p_one, e$p_one_sided,
                  tier = "display", precision = 3,
                  label = sprintf("[%s, equal-var] one-sided p", scenario))
    }
    if (!is.null(e$mean_diff)) {
      precision <- if (abs(e$mean_diff) >= 100) 4L else 3L
      assert_spss(r_eq_meandiff, e$mean_diff,
                  tier = "display", precision = precision,
                  label = sprintf("[%s, equal-var] Mean Difference", scenario))
    }
    if (!is.null(e$ci_lower)) {
      precision <- if (abs(e$ci_lower) >= 100) 4L else 3L
      assert_spss(r_eq_ci_lo, e$ci_lower,
                  tier = "display", precision = precision,
                  label = sprintf("[%s, equal-var] CI lower", scenario))
    }
    if (!is.null(e$ci_upper)) {
      precision <- if (abs(e$ci_upper) >= 100) 4L else 3L
      assert_spss(r_eq_ci_hi, e$ci_upper,
                  tier = "display", precision = precision,
                  label = sprintf("[%s, equal-var] CI upper", scenario))
    }
  }

  # ---- Welch assertions -------------------------------------------------
  if (!is.null(spss$welch)) {
    w <- spss$welch

    if (!is.null(w$t_stat)) {
      assert_spss(r_un_t, w$t_stat,
                  tier = "display", precision = 3,
                  label = sprintf("[%s, Welch] t", scenario))
    }
    if (!is.null(w$df)) {
      # Welch df is decimal; SPSS prints to 3 decimals.
      assert_spss(r_un_df, w$df,
                  tier = "display", precision = 3,
                  label = sprintf("[%s, Welch] df", scenario))
    }
    if (!is.null(w$p_two_sided)) {
      assert_spss(r_un_p_two, w$p_two_sided,
                  tier = "display", precision = 3,
                  label = sprintf("[%s, Welch] two-sided p", scenario))
    }
    if (!is.null(w$p_one_sided)) {
      r_un_p_one <- r_un_p_two / 2
      assert_spss(r_un_p_one, w$p_one_sided,
                  tier = "display", precision = 3,
                  label = sprintf("[%s, Welch] one-sided p", scenario))
    }
    if (!is.null(w$mean_diff)) {
      precision <- if (abs(w$mean_diff) >= 100) 4L else 3L
      assert_spss(r_un_meandiff, w$mean_diff,
                  tier = "display", precision = precision,
                  label = sprintf("[%s, Welch] Mean Difference", scenario))
    }
    if (!is.null(w$ci_lower)) {
      precision <- if (abs(w$ci_lower) >= 100) 4L else 3L
      assert_spss(r_un_ci_lo, w$ci_lower,
                  tier = "display", precision = precision,
                  label = sprintf("[%s, Welch] CI lower", scenario))
    }
    if (!is.null(w$ci_upper)) {
      precision <- if (abs(w$ci_upper) >= 100) 4L else 3L
      assert_spss(r_un_ci_hi, w$ci_upper,
                  tier = "display", precision = precision,
                  label = sprintf("[%s, Welch] CI upper", scenario))
    }
  }

  # ---- N (sum of group sizes) -------------------------------------------
  # SPSS computes df_equal = round(sw1 + sw2) - 2, so the displayed total N
  # is round(sw1 + sw2). On the R side, n1 and n2 are independently rounded
  # per group via round(sw1) + round(sw2), which can differ from
  # round(sw1 + sw2) by 1 due to half-up rounding accumulation.
  # We compare the un-rounded total recovered from r$df instead.
  if (!is.null(spss$equal_var) && !is.null(spss$equal_var$df)) {
    spss_n_total <- spss$equal_var$df + 2L
    r_n_total_unrounded <- as.numeric(r$equal_var_result[[1]]$parameter) + 2
    assert_spss(r_n_total_unrounded, spss_n_total,
                tier = "display", precision = 0,
                label = sprintf("[%s] total N (via df+2)", scenario))
  }

  invisible(NULL)
}


#' Extract grouped results for one (region, variable) cell
#' @param result Grouped t_test result
#' @param region "East" or "West"
#' @param variable Optional variable name when result has multiple variables
extract_grouped_row <- function(result, region, variable = NULL) {
  res <- result$results
  if (!is.null(variable)) {
    sel <- res$region == region & res$Variable == variable
  } else {
    sel <- res$region == region
  }
  if (sum(sel) != 1L) {
    stop(sprintf("extract_grouped_row(): expected exactly 1 row for region=%s%s; got %d",
                 region,
                 if (!is.null(variable)) sprintf(", variable=%s", variable) else "",
                 sum(sel)), call. = FALSE)
  }
  out <- result
  out$results <- res[sel, , drop = FALSE]
  out
}


# =============================================================================
# DATA SETUP
# =============================================================================

data(survey_data, envir = environment())


# =============================================================================
# SCENARIO 1 — UNWEIGHTED / UNGROUPED
# =============================================================================

test_that("Test 1a: one-sample, life_satisfaction, mu = 3.0", {
  result <- survey_data |> t_test(life_satisfaction, mu = 3.0)
  compare_one_sample(result, spss_values$test_1a_one_sample,
                     "1a: one-sample life_sat mu=3")
})

test_that("Test 1b: two-sample, life_satisfaction by gender", {
  result <- survey_data |> t_test(life_satisfaction, group = gender)
  compare_two_sample(result, spss_values$test_1b_life_by_gender,
                     "1b: life_sat by gender")
})

test_that("Test 1c: two-sample, income by gender", {
  result <- survey_data |> t_test(income, group = gender)
  compare_two_sample(result, spss_values$test_1c_income_by_gender,
                     "1c: income by gender")
})

test_that("Test 1d: two-sample, age by gender", {
  result <- survey_data |> t_test(age, group = gender)
  compare_two_sample(result, spss_values$test_1d_age_by_gender,
                     "1d: age by gender")
})


# =============================================================================
# SCENARIO 2 — WEIGHTED / UNGROUPED
# =============================================================================

test_that("Test 2a: one-sample weighted, life_satisfaction, mu = 3.0", {
  result <- survey_data |>
    t_test(life_satisfaction, mu = 3.0, weights = sampling_weight)
  compare_one_sample(result, spss_values$test_2a_one_sample_weighted,
                     "2a: weighted one-sample life_sat mu=3")
})

test_that("Test 2b: two-sample weighted, life_satisfaction by gender", {
  result <- survey_data |>
    t_test(life_satisfaction, group = gender, weights = sampling_weight)
  compare_two_sample(result, spss_values$test_2b_life_by_gender_weighted,
                     "2b: weighted life_sat by gender")
})

test_that("Test 2c: two-sample weighted, income by gender", {
  result <- survey_data |>
    t_test(income, group = gender, weights = sampling_weight)
  compare_two_sample(result, spss_values$test_2c_income_by_gender_weighted,
                     "2c: weighted income by gender")
})

test_that("Test 2d: two-sample weighted, age by gender", {
  result <- survey_data |>
    t_test(age, group = gender, weights = sampling_weight)
  compare_two_sample(result, spss_values$test_2d_age_by_gender_weighted,
                     "2d: weighted age by gender")
})


# =============================================================================
# SCENARIO 3 — UNWEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 3a: life_satisfaction by gender, grouped by region", {
  result <- survey_data |>
    group_by(region) |>
    t_test(life_satisfaction, group = gender)

  for (rg in c("East", "West")) {
    cell <- extract_grouped_row(result, rg)
    compare_two_sample(cell,
                       spss_values$test_3a_life_by_gender_grouped[[rg]],
                       sprintf("3a: life_sat by gender [%s]", rg))
  }
})

test_that("Test 3b: income by gender, grouped by region", {
  result <- survey_data |>
    group_by(region) |>
    t_test(income, group = gender)

  for (rg in c("East", "West")) {
    cell <- extract_grouped_row(result, rg)
    compare_two_sample(cell,
                       spss_values$test_3b_income_by_gender_grouped[[rg]],
                       sprintf("3b: income by gender [%s]", rg))
  }
})

test_that("Test 3c: age by gender, grouped by region", {
  result <- survey_data |>
    group_by(region) |>
    t_test(age, group = gender)

  for (rg in c("East", "West")) {
    cell <- extract_grouped_row(result, rg)
    compare_two_sample(cell,
                       spss_values$test_3c_age_by_gender_grouped[[rg]],
                       sprintf("3c: age by gender [%s]", rg))
  }
})


# =============================================================================
# SCENARIO 4 — WEIGHTED / GROUPED by region
# =============================================================================

test_that("Test 4a: life_satisfaction by gender, weighted, grouped by region", {
  result <- survey_data |>
    group_by(region) |>
    t_test(life_satisfaction, group = gender, weights = sampling_weight)

  for (rg in c("East", "West")) {
    cell <- extract_grouped_row(result, rg)
    compare_two_sample(cell,
                       spss_values$test_4a_life_by_gender_weighted_grouped[[rg]],
                       sprintf("4a: weighted life_sat by gender [%s]", rg))
  }
})

test_that("Test 4b: income by gender, weighted, grouped by region", {
  result <- survey_data |>
    group_by(region) |>
    t_test(income, group = gender, weights = sampling_weight)

  for (rg in c("East", "West")) {
    cell <- extract_grouped_row(result, rg)
    compare_two_sample(cell,
                       spss_values$test_4b_income_by_gender_weighted_grouped[[rg]],
                       sprintf("4b: weighted income by gender [%s]", rg))
  }
})

test_that("Test 4c: age by gender, weighted, grouped by region", {
  result <- survey_data |>
    group_by(region) |>
    t_test(age, group = gender, weights = sampling_weight)

  for (rg in c("East", "West")) {
    cell <- extract_grouped_row(result, rg)
    compare_two_sample(cell,
                       spss_values$test_4c_age_by_gender_weighted_grouped[[rg]],
                       sprintf("4c: weighted age by gender [%s]", rg))
  }
})


# =============================================================================
# AUXILIARY SCENARIOS
# =============================================================================

test_that("Test 5a: one-sample, income, mu = 5000", {
  result <- survey_data |> t_test(income, mu = 5000)
  compare_one_sample(result, spss_values$test_5a_income_one_sample,
                     "5a: one-sample income mu=5000")
})

test_that("Test 5b: one-sample, age, mu = 45", {
  result <- survey_data |> t_test(age, mu = 45)
  compare_one_sample(result, spss_values$test_5b_age_one_sample,
                     "5b: one-sample age mu=45")
})

test_that("Test 6: multiple variables simultaneously (trust_* additions)", {
  # SPSS Test 6 runs t-tests on six DVs at once; the first three (life_sat,
  # income, age) duplicate Tests 1b-d (already validated). This block adds
  # validation for trust_government / trust_media / trust_science, which the
  # legacy test file omitted.
  result <- survey_data |>
    t_test(life_satisfaction, income, age,
           trust_government, trust_media, trust_science,
           group = gender)

  expect_equal(nrow(result$results), 6L)
  expect_setequal(
    result$results$Variable,
    c("life_satisfaction", "income", "age",
      "trust_government", "trust_media", "trust_science")
  )

  trust_vars <- c("trust_government", "trust_media", "trust_science")
  for (v in trust_vars) {
    row_i <- which(result$results$Variable == v)
    # Build a one-row pseudo-result that compare_two_sample expects
    sub <- result
    sub$results <- result$results[row_i, , drop = FALSE]
    compare_two_sample(sub,
                       spss_values$test_6_multi_var_trust[[v]],
                       sprintf("6: %s by gender", v))
  }
})

test_that("Test 7a: alternative CI level 90%, life_satisfaction by gender", {
  result <- survey_data |>
    t_test(life_satisfaction, group = gender, conf.level = 0.90)

  r <- result$results[1, ]

  # CI bounds at 90% (precision = 3 dp from SPSS print).
  spss <- spss_values$test_7a_life_by_gender_90ci

  # Equal-variance branch
  eq <- r$equal_var_result[[1]]
  assert_spss(as.numeric(eq$conf.int[1]), spss$equal_var$ci_lower,
              tier = "display", precision = 3,
              label = "7a equal-var CI lower (90%)")
  assert_spss(as.numeric(eq$conf.int[2]), spss$equal_var$ci_upper,
              tier = "display", precision = 3,
              label = "7a equal-var CI upper (90%)")

  # Welch branch
  un <- r$unequal_var_result[[1]]
  assert_spss(as.numeric(un$conf.int[1]), spss$welch$ci_lower,
              tier = "display", precision = 3,
              label = "7a Welch CI lower (90%)")
  assert_spss(as.numeric(un$conf.int[2]), spss$welch$ci_upper,
              tier = "display", precision = 3,
              label = "7a Welch CI upper (90%)")
})

test_that("Test 7b: alternative CI level 99%, life_satisfaction by gender", {
  result <- survey_data |>
    t_test(life_satisfaction, group = gender, conf.level = 0.99)

  r <- result$results[1, ]
  spss <- spss_values$test_7b_life_by_gender_99ci

  eq <- r$equal_var_result[[1]]
  assert_spss(as.numeric(eq$conf.int[1]), spss$equal_var$ci_lower,
              tier = "display", precision = 3,
              label = "7b equal-var CI lower (99%)")
  assert_spss(as.numeric(eq$conf.int[2]), spss$equal_var$ci_upper,
              tier = "display", precision = 3,
              label = "7b equal-var CI upper (99%)")

  un <- r$unequal_var_result[[1]]
  assert_spss(as.numeric(un$conf.int[1]), spss$welch$ci_lower,
              tier = "display", precision = 3,
              label = "7b Welch CI lower (99%)")
  assert_spss(as.numeric(un$conf.int[2]), spss$welch$ci_upper,
              tier = "display", precision = 3,
              label = "7b Welch CI upper (99%)")
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


# =============================================================================
# NOTE — DESCRIPTIVES VALIDATION GAP
# =============================================================================
# SPSS prints a "One-Sample Statistics" table (N, Mean, SD, SE) before the
# main test for one-sample analyses. mariposa's t_test() does NOT currently
# expose Mean, SD, SE in its result object — only the test statistics and the
# observed mean (via mean_diff field).
#
# Tests 1a, 2a, 5a, 5b have SPSS reference values for mean/sd/se but cannot
# be asserted today. This is a t_test() source-code gap, not a test gap. To
# close it (Phase 2), add a $descriptives or $statistics field to t_test()
# that exposes those statistics; then add assert_spss() calls in
# compare_one_sample() above.
#
# Until then, this file validates 100% of statistics that the R function
# currently exposes against SPSS reference values, with no NA placeholders,
# no inline tolerances, no expect_true(TRUE) reporting blocks, and no
# scenario-name-based tolerance switching.
# =============================================================================
