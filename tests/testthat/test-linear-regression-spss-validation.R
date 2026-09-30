# =============================================================================
# linear_regression — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::linear_regression() against SPSS v29 REGRESSION.
# Reference: tests/spss_reference/outputs/linear_regression_output.txt
#
# Coverage (Charter §8 four-scenario rule):
#   1a — unweighted / ungrouped / bivariate
#   1b — unweighted / ungrouped / three trust predictors
#   1c — unweighted / ungrouped / multiple (factor predictor via factors="numeric")
#   1d — unweighted / ungrouped / four predictors (negative adjusted R^2)
#   2a — weighted / ungrouped / bivariate  (Charter §5.1 weighted-df fix)
#   2b — weighted / ungrouped / three trust predictors
#   2c — weighted / ungrouped / multiple
#   3a, 3b — unweighted / grouped (region) / bivariate, three predictors
#   4a, 4b — weighted / grouped (region) / bivariate, three predictors
#
# Every scenario asserts N, Descriptive Statistics (Mean, SD, N; SPSS
# order), Model Summary, ANOVA and Coefficients (SPSS term order). Not
# asserted: the Correlations table (not part of the result object).
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(

  # -------------------------------------------------------------------------
  # Test 1a — life_satisfaction ~ age (unweighted, ungrouped)
  # -------------------------------------------------------------------------
  test_1a_life_age = list(
    n = 2421L,
    desc = list(   # Descriptive Statistics: variable, Mean, Std. Deviation, N
      c("life_satisfaction", "3.63", "1.153", "2421"),  # linear_regression_output.txt:9
      c("age", "50.5832", "16.99976", "2421")   # linear_regression_output.txt:10
    ),
    R = 0.029, R2 = 0.001, adj_R2 = 0.000,        # linear_regression_output.txt:21
    se_estimate = 1.153,                          # linear_regression_output.txt:21
    anova = list(ss_reg = 2.653, df_reg = 1L, ms_reg = 2.653,
                 ss_res = 3214.775, df_res = 2419L, ms_res = 1.329,
                 ss_tot = 3217.428, df_tot = 2420L,
                 F = 1.996, p = 0.158),           # linear_regression_output.txt:27-29
    coefs = list(
      intercept = list(B = 3.727, SE = 0.074, t = 50.663, p = "<.001"),  # :37
      age       = list(B = -0.002, SE = 0.001, Beta = -0.029,
                       t = -1.413, p = 0.158)                            # :38
    )
  ),

  # -------------------------------------------------------------------------
  # Test 1b — life_satisfaction ~ trust_government + trust_media + trust_science
  # -------------------------------------------------------------------------
  test_1b_life_trust = list(
    n = 2066L,
    desc = list(   # Descriptive Statistics: variable, Mean, Std. Deviation, N
      c("life_satisfaction", "3.66", "1.144", "2066"),  # linear_regression_output.txt:47
      c("trust_government", "2.62", "1.164", "2066"),  # linear_regression_output.txt:48
      c("trust_media", "2.42", "1.153", "2066"),  # linear_regression_output.txt:49
      c("trust_science", "3.62", "1.031", "2066")   # linear_regression_output.txt:50
    ),
    R = 0.041, R2 = 0.002, adj_R2 = 0.000,  # linear_regression_output.txt:70
    se_estimate = 1.144,  # linear_regression_output.txt:70
    anova = list(ss_reg = 4.570, df_reg = 3L, ms_reg = 1.523,
                 ss_res = 2699.432, df_res = 2062L, ms_res = 1.309,
                 ss_tot = 2704.002, df_tot = 2065L,
                 F = 1.164, p = 0.322),  # linear_regression_output.txt:76-78
    coefs = list(
      intercept = list(B = 3.675, SE = 0.118, t = 31.054, p = "<.001"),  # linear_regression_output.txt:86
      trust_government = list(B = -0.008, SE = 0.022, Beta = -0.008, t = -0.361, p = 0.718),  # linear_regression_output.txt:87
      trust_media = list(B = 0.035, SE = 0.022, Beta = 0.035, t = 1.598, p = 0.110),  # linear_regression_output.txt:88
      trust_science = list(B = -0.023, SE = 0.024, Beta = -0.021, t = -0.932, p = 0.351)   # linear_regression_output.txt:89
    )
  ),

  # -------------------------------------------------------------------------
  # Test 1c — income ~ age + education + life_satisfaction
  # education is an ordered factor; SPSS treats it ordinal-as-scale.
  # mariposa default is factors="dummy" (polynomial contrasts for ordered),
  # so this test must call with factors="numeric" to match SPSS.
  # -------------------------------------------------------------------------
  test_1c_income_multi = list(
    n = 2115L,
    desc = list(   # Descriptive Statistics: variable, Mean, Std. Deviation, N
      c("income", "3757.6832", "1430.92329", "2115"),  # linear_regression_output.txt:98
      c("age", "50.8274", "16.99546", "2115"),  # linear_regression_output.txt:99
      c("education", "2.2388", "1.08408", "2115"),  # linear_regression_output.txt:100
      c("life_satisfaction", "3.64", "1.148", "2115")   # linear_regression_output.txt:101
    ),
    R = 0.686, R2 = 0.471, adj_R2 = 0.470,         # linear_regression_output.txt:120
    se_estimate = 1041.88947,                      # linear_regression_output.txt:120
    se_estimate_dp = 5L,                           # printed with 5 decimals
    anova = list(ss_reg = 2036941094.578, df_reg = 3L, ms_reg = 678980364.860,
                 ss_res = 2291561553.177, df_res = 2111L, ms_res = 1085533.659,
                 ss_tot = 4328502647.755, df_tot = 2114L,
                 F = 625.481, p = "<.001"),        # linear_regression_output.txt:126-128
    coefs = list(
      intercept    = list(B = 800.493, SE = 105.893, t = 7.559,  p = "<.001"),  # :136
      age          = list(B = -0.194,  SE = 1.333,   Beta = -0.002,
                          t = -0.145, p = 0.885),                               # :137
      education    = list(B = 711.841, SE = 21.706,  Beta = 0.539,
                          t = 32.794, p = "<.001"),                             # :138
      life_satisfaction = list(B = 377.527, SE = 20.505, Beta = 0.303,
                               t = 18.412, p = "<.001")                         # :139
    )
  ),

  # -------------------------------------------------------------------------
  # Test 1d — life_satisfaction ~ age + environmental_concern +
  #           political_orientation + trust_government (adjusted R^2 < 0)
  # -------------------------------------------------------------------------
  test_1d_life_four = list(
    n = 2017L,
    desc = list(   # Descriptive Statistics: variable, Mean, Std. Deviation, N
      c("life_satisfaction", "3.63", "1.162", "2017"),  # linear_regression_output.txt:148
      c("age", "50.4993", "16.99852", "2017"),  # linear_regression_output.txt:149
      c("environmental_concern", "3.58", "1.194", "2017"),  # linear_regression_output.txt:150
      c("political_orientation", "2.72", "1.085", "2017"),  # linear_regression_output.txt:151
      c("trust_government", "2.62", "1.165", "2017")   # linear_regression_output.txt:152
    ),
    R = 0.037, R2 = 0.001, adj_R2 = -0.001,  # linear_regression_output.txt:177
    se_estimate = 1.162,  # linear_regression_output.txt:177
    anova = list(ss_reg = 3.773, df_reg = 4L, ms_reg = 0.943,
                 ss_res = 2716.201, df_res = 2012L, ms_res = 1.350,
                 ss_tot = 2719.973, df_tot = 2016L,
                 F = 0.699, p = 0.593),  # linear_regression_output.txt:183-185
    coefs = list(
      intercept = list(B = 3.581, SE = 0.187, t = 19.165, p = "<.001"),  # linear_regression_output.txt:193
      age = list(B = -0.002, SE = 0.002, Beta = -0.023, t = -1.043, p = 0.297),  # linear_regression_output.txt:194
      environmental_concern = list(B = 0.014, SE = 0.027, Beta = 0.015, t = 0.530, p = 0.596),  # linear_regression_output.txt:195
      political_orientation = list(B = 0.006, SE = 0.030, Beta = 0.005, t = 0.199, p = 0.843),  # linear_regression_output.txt:196
      trust_government = list(B = 0.025, SE = 0.022, Beta = 0.026, t = 1.145, p = 0.252)   # linear_regression_output.txt:197
    )
  ),

  # -------------------------------------------------------------------------
  # Test 2a — life_satisfaction ~ age (weighted, ungrouped)
  # KEY TEST for Charter §5.1 weighted-df fix.
  # -------------------------------------------------------------------------
  test_2a_life_age_weighted = list(
    n = 2437L,
    desc = list(   # Descriptive Statistics: variable, Mean, Std. Deviation, N
      c("life_satisfaction", "3.62", "1.152", "2437"),  # linear_regression_output.txt:208
      c("age", "50.5524", "17.10830", "2437")   # linear_regression_output.txt:209
    ),
    R = 0.029, R2 = 0.001, adj_R2 = 0.000,         # linear_regression_output.txt:220
    se_estimate = 1.152,                           # linear_regression_output.txt:220
    anova = list(ss_reg = 2.757, df_reg = 1L, ms_reg = 2.757,
                 ss_res = 3230.392, df_res = 2435L, ms_res = 1.327,
                 ss_tot = 3233.149, df_tot = 2436L,
                 F = 2.078, p = 0.150),            # linear_regression_output.txt:226-228
    coefs = list(
      intercept = list(B = 3.724, SE = 0.073, t = 51.152, p = "<.001"),         # :236
      age       = list(B = -0.002, SE = 0.001, Beta = -0.029,
                       t = -1.441, p = 0.150)                                   # :237
    )
  ),

  # -------------------------------------------------------------------------
  # Test 2b — weighted life_satisfaction ~ trust_* (ungrouped)
  # -------------------------------------------------------------------------
  test_2b_life_trust_weighted = list(
    n = 2080L,
    desc = list(   # Descriptive Statistics: variable, Mean, Std. Deviation, N
      c("life_satisfaction", "3.65", "1.143", "2080"),  # linear_regression_output.txt:246
      c("trust_government", "2.63", "1.164", "2080"),  # linear_regression_output.txt:247
      c("trust_media", "2.43", "1.156", "2080"),  # linear_regression_output.txt:248
      c("trust_science", "3.62", "1.032", "2080")   # linear_regression_output.txt:249
    ),
    R = 0.042, R2 = 0.002, adj_R2 = 0.000,  # linear_regression_output.txt:269
    se_estimate = 1.143,  # linear_regression_output.txt:269
    anova = list(ss_reg = 4.808, df_reg = 3L, ms_reg = 1.603,
                 ss_res = 2712.928, df_res = 2076L, ms_res = 1.307,
                 ss_tot = 2717.736, df_tot = 2079L,
                 F = 1.226, p = 0.299),  # linear_regression_output.txt:275-277
    coefs = list(
      intercept = list(B = 3.680, SE = 0.118, t = 31.277, p = "<.001"),  # linear_regression_output.txt:285
      trust_government = list(B = -0.003, SE = 0.022, Beta = -0.004, t = -0.161, p = 0.872),  # linear_regression_output.txt:286
      trust_media = list(B = 0.034, SE = 0.022, Beta = 0.034, t = 1.567, p = 0.117),  # linear_regression_output.txt:287
      trust_science = list(B = -0.027, SE = 0.024, Beta = -0.025, t = -1.127, p = 0.260)   # linear_regression_output.txt:288
    )
  ),

  # -------------------------------------------------------------------------
  # Test 2c — weighted multiple: income ~ age + education + life_satisfaction
  # -------------------------------------------------------------------------
  test_2c_income_multi_weighted = list(
    n = 2130L,
    desc = list(   # Descriptive Statistics: variable, Mean, Std. Deviation, N
      c("income", "3747.2961", "1422.77071", "2130"),  # linear_regression_output.txt:297
      c("age", "50.8075", "17.08223", "2130"),  # linear_regression_output.txt:298
      c("education", "2.2285", "1.07570", "2130"),  # linear_regression_output.txt:299
      c("life_satisfaction", "3.64", "1.147", "2130")   # linear_regression_output.txt:300
    ),
    R = 0.686, R2 = 0.470, adj_R2 = 0.469,         # linear_regression_output.txt:319
    se_estimate = 1036.28697,                      # linear_regression_output.txt:319
    se_estimate_dp = 5L,                           # printed with 5 decimals
    anova = list(ss_reg = 2026419335.048, df_reg = 3L, ms_reg = 675473111.683,
                 ss_res = 2282895332.715, df_res = 2126L, ms_res = 1073890.693,
                 ss_tot = 4309314667.763, df_tot = 2129L,
                 F = 628.996, p = "<.001"),        # linear_regression_output.txt:325-327
    coefs = list(
      intercept    = list(B = 784.981, SE = 104.720, t = 7.496,  p = "<.001"),  # :335
      age          = list(B = -0.110,  SE = 1.315,   Beta = -0.001,
                          t = -0.083, p = 0.934),                               # :336
      education    = list(B = 709.765, SE = 21.659,  Beta = 0.537,
                          t = 32.770, p = "<.001"),                             # :337
      life_satisfaction = list(B = 381.257, SE = 20.309, Beta = 0.307,
                               t = 18.773, p = "<.001")                         # :338
    )
  ),

  # -------------------------------------------------------------------------
  # Test 3a — life_satisfaction ~ age grouped by region (unweighted)
  # -------------------------------------------------------------------------
  test_3a_grouped_east = list(
    n = 465L,
    desc = list(   # Descriptive Statistics: variable, Mean, Std. Deviation, N
      c("life_satisfaction", "3.62", "1.207", "465"),  # linear_regression_output.txt:349
      c("age", "51.8860", "17.43553", "465")   # linear_regression_output.txt:350
    ),
    R = 0.043, R2 = 0.002, adj_R2 = 0.000, se_estimate = 1.207,   # :367
    anova = list(ss_reg = 1.276, df_reg = 1L, ms_reg = 1.276,
                 ss_res = 674.350, df_res = 463L, ms_res = 1.456,
                 ss_tot = 675.626, df_tot = 464L,
                 F = 0.876, p = 0.350),                            # :374
    coefs = list(
      intercept = list(B = 3.775, SE = 0.176, t = 21.467, p = "<.001"),  # :387
      age       = list(B = -0.003, SE = 0.003, Beta = -0.043,
                       t = -0.936, p = 0.350)                            # :388
    )
  ),

  test_3a_grouped_west = list(
    n = 1956L,
    desc = list(   # Descriptive Statistics: variable, Mean, Std. Deviation, N
      c("life_satisfaction", "3.63", "1.140", "1956"),  # linear_regression_output.txt:351
      c("age", "50.2735", "16.88427", "1956")   # linear_regression_output.txt:352
    ),
    R = 0.025, R2 = 0.001, adj_R2 = 0.000, se_estimate = 1.140,   # :368
    anova = list(ss_reg = 1.556, df_reg = 1L, ms_reg = 1.556,
                 ss_res = 2540.200, df_res = 1954L, ms_res = 1.300,
                 ss_tot = 2541.756, df_tot = 1955L,
                 F = 1.197, p = 0.274),                            # :377
    coefs = list(
      intercept = list(B = 3.714, SE = 0.081, t = 45.860, p = "<.001"),  # :389
      age       = list(B = -0.002, SE = 0.002, Beta = -0.025,
                       t = -1.094, p = 0.274)                            # :390
    )
  ),

  # -------------------------------------------------------------------------
  # Test 3b — life_satisfaction ~ trust_* grouped by region (unweighted)
  # -------------------------------------------------------------------------
  test_3b_grouped_east = list(
    n = 404L,
    desc = list(   # Descriptive Statistics: variable, Mean, Std. Deviation, N
      c("life_satisfaction", "3.64", "1.211", "404"),  # linear_regression_output.txt:399
      c("trust_government", "2.63", "1.173", "404"),  # linear_regression_output.txt:400
      c("trust_media", "2.40", "1.092", "404"),  # linear_regression_output.txt:401
      c("trust_science", "3.65", "1.014", "404")   # linear_regression_output.txt:402
    ),
    R = 0.095, R2 = 0.009, adj_R2 = 0.002,  # linear_regression_output.txt:443
    se_estimate = 1.210,  # linear_regression_output.txt:443
    anova = list(ss_reg = 5.367, df_reg = 3L, ms_reg = 1.789,
                 ss_res = 585.307, df_res = 400L, ms_res = 1.463,
                 ss_tot = 590.673, df_tot = 403L,
                 F = 1.222, p = 0.301),  # linear_regression_output.txt:451-453
    coefs = list(
      intercept = list(B = 4.169, SE = 0.288, t = 14.467, p = "<.001"),  # linear_regression_output.txt:465
      trust_government = list(B = -0.035, SE = 0.051, Beta = -0.033, t = -0.673, p = 0.502),  # linear_regression_output.txt:466
      trust_media = list(B = -0.065, SE = 0.055, Beta = -0.058, t = -1.169, p = 0.243),  # linear_regression_output.txt:467
      trust_science = list(B = -0.077, SE = 0.060, Beta = -0.064, t = -1.287, p = 0.199)   # linear_regression_output.txt:468
    )
  ),

  test_3b_grouped_west = list(
    n = 1662L,
    desc = list(   # Descriptive Statistics: variable, Mean, Std. Deviation, N
      c("life_satisfaction", "3.66", "1.128", "1662"),  # linear_regression_output.txt:403
      c("trust_government", "2.62", "1.162", "1662"),  # linear_regression_output.txt:404
      c("trust_media", "2.43", "1.168", "1662"),  # linear_regression_output.txt:405
      c("trust_science", "3.61", "1.036", "1662")   # linear_regression_output.txt:406
    ),
    R = 0.059, R2 = 0.003, adj_R2 = 0.002,  # linear_regression_output.txt:444
    se_estimate = 1.127,  # linear_regression_output.txt:444
    anova = list(ss_reg = 7.294, df_reg = 3L, ms_reg = 2.431,
                 ss_res = 2105.952, df_res = 1658L, ms_res = 1.270,
                 ss_tot = 2113.247, df_tot = 1661L,
                 F = 1.914, p = 0.125),  # linear_regression_output.txt:454-456
    coefs = list(
      intercept = list(B = 3.561, SE = 0.129, t = 27.518, p = "<.001"),  # linear_regression_output.txt:469
      trust_government = list(B = -0.002, SE = 0.024, Beta = -0.002, t = -0.088, p = 0.930),  # linear_regression_output.txt:470
      trust_media = list(B = 0.056, SE = 0.024, Beta = 0.058, t = 2.375, p = 0.018),  # linear_regression_output.txt:471
      trust_science = list(B = -0.009, SE = 0.027, Beta = -0.008, t = -0.337, p = 0.736)   # linear_regression_output.txt:472
    )
  ),

  # -------------------------------------------------------------------------
  # Test 4a — life_satisfaction ~ age weighted + grouped by region
  # KEY TEST for §5.1 fix interaction with grouping.
  # -------------------------------------------------------------------------
  test_4a_weighted_grouped_east = list(
    n = 488L,
    desc = list(   # Descriptive Statistics: variable, Mean, Std. Deviation, N
      c("life_satisfaction", "3.62", "1.203", "488"),  # linear_regression_output.txt:483
      c("age", "52.3065", "17.61659", "488")   # linear_regression_output.txt:484
    ),
    R = 0.046, R2 = 0.002, adj_R2 = 0.000, se_estimate = 1.203,   # :501
    anova = list(ss_reg = 1.514, df_reg = 1L, ms_reg = 1.514,
                 ss_res = 703.788, df_res = 486L, ms_res = 1.448,
                 ss_tot = 705.302, df_tot = 487L,
                 F = 1.045, p = 0.307),                            # :508
    coefs = list(
      intercept = list(B = 3.789, SE = 0.171, t = 22.179, p = "<.001"),  # :521
      age       = list(B = -0.003, SE = 0.003, Beta = -0.046,
                       t = -1.022, p = 0.307)                            # :522
    )
  ),

  test_4a_weighted_grouped_west = list(
    n = 1949L,
    desc = list(   # Descriptive Statistics: variable, Mean, Std. Deviation, N
      c("life_satisfaction", "3.63", "1.139", "1949"),  # linear_regression_output.txt:485
      c("age", "50.1131", "16.95486", "1949")   # linear_regression_output.txt:486
    ),
    R = 0.025, R2 = 0.001, adj_R2 = 0.000, se_estimate = 1.139,   # :502
    anova = list(ss_reg = 1.518, df_reg = 1L, ms_reg = 1.518,
                 ss_res = 2526.327, df_res = 1947L, ms_res = 1.298,
                 ss_tot = 2527.845, df_tot = 1948L,
                 F = 1.170, p = 0.280),                            # :511
    coefs = list(
      intercept = list(B = 3.708, SE = 0.081, t = 46.034, p = "<.001"),  # :523
      age       = list(B = -0.002, SE = 0.002, Beta = -0.025,
                       t = -1.082, p = 0.280)                            # :524
    )
  ),

  # -------------------------------------------------------------------------
  # Test 4b — life_satisfaction ~ trust_* weighted + grouped by region
  # -------------------------------------------------------------------------
  test_4b_weighted_grouped_east = list(
    n = 424L,
    desc = list(   # Descriptive Statistics: variable, Mean, Std. Deviation, N
      c("life_satisfaction", "3.65", "1.206", "424"),  # linear_regression_output.txt:533
      c("trust_government", "2.62", "1.173", "424"),  # linear_regression_output.txt:534
      c("trust_media", "2.40", "1.093", "424"),  # linear_regression_output.txt:535
      c("trust_science", "3.65", "1.012", "424")   # linear_regression_output.txt:536
    ),
    R = 0.102, R2 = 0.010, adj_R2 = 0.003,  # linear_regression_output.txt:577
    se_estimate = 1.204,  # linear_regression_output.txt:577
    anova = list(ss_reg = 6.418, df_reg = 3L, ms_reg = 2.139,
                 ss_res = 609.770, df_res = 420L, ms_res = 1.450,
                 ss_tot = 616.187, df_tot = 423L,
                 F = 1.475, p = 0.221),  # linear_regression_output.txt:585-587
    coefs = list(
      intercept = list(B = 4.215, SE = 0.278, t = 15.168, p = "<.001"),  # linear_regression_output.txt:599
      trust_government = list(B = -0.039, SE = 0.050, Beta = -0.038, t = -0.784, p = 0.433),  # linear_regression_output.txt:600
      trust_media = list(B = -0.062, SE = 0.054, Beta = -0.056, t = -1.148, p = 0.252),  # linear_regression_output.txt:601
      trust_science = list(B = -0.086, SE = 0.058, Beta = -0.072, t = -1.484, p = 0.138)   # linear_regression_output.txt:602
    )
  ),

  test_4b_weighted_grouped_west = list(
    n = 1656L,
    desc = list(   # Descriptive Statistics: variable, Mean, Std. Deviation, N
      c("life_satisfaction", "3.65", "1.127", "1656"),  # linear_regression_output.txt:537
      c("trust_government", "2.63", "1.162", "1656"),  # linear_regression_output.txt:538
      c("trust_media", "2.43", "1.171", "1656"),  # linear_regression_output.txt:539
      c("trust_science", "3.61", "1.037", "1656")   # linear_regression_output.txt:540
    ),
    R = 0.059, R2 = 0.003, adj_R2 = 0.002,  # linear_regression_output.txt:578
    se_estimate = 1.126,  # linear_regression_output.txt:578
    anova = list(ss_reg = 7.342, df_reg = 3L, ms_reg = 2.447,
                 ss_res = 2094.198, df_res = 1652L, ms_res = 1.268,
                 ss_tot = 2101.540, df_tot = 1655L,
                 F = 1.930, p = 0.123),  # linear_regression_output.txt:588-590
    coefs = list(
      intercept = list(B = 3.548, SE = 0.129, t = 27.398, p = "<.001"),  # linear_regression_output.txt:603
      trust_government = list(B = 0.005, SE = 0.024, Beta = 0.005, t = 0.216, p = 0.829),  # linear_regression_output.txt:604
      trust_media = list(B = 0.056, SE = 0.024, Beta = 0.058, t = 2.356, p = 0.019),  # linear_regression_output.txt:605
      trust_science = list(B = -0.012, SE = 0.027, Beta = -0.011, t = -0.435, p = 0.664)   # linear_regression_output.txt:606
    )
  )
)


data(survey_data, envir = environment())


# =============================================================================
# Reusable per-test assertion helpers
# =============================================================================

# Assert `actual` against the string SPSS printed (Descriptive Statistics):
# Display tier with the printed number of decimals.
assert_printed <- function(actual, printed, label) {
  decimals <- if (grepl(".", printed, fixed = TRUE)) {
    nchar(sub("^[^.]*[.]", "", printed))
  } else {
    0L
  }
  assert_spss(actual, as.numeric(printed), tier = "display",
              precision = decimals, label = label)
}

# Descriptive Statistics: variables in SPSS order (dependent first), Mean,
# Std. Deviation, N. Weighted N is the rounded sum of weights (Display 0).
assert_lm_desc <- function(result, spss, scenario, weighted) {
  d <- result$descriptives
  expect_identical(d$Variable, vapply(spss$desc, `[`, "", 1L),
                   label = sprintf("[%s] descriptives in SPSS order", scenario))
  for (row in spss$desc) {
    x <- d[d$Variable == row[1], , drop = FALSE]
    lab <- sprintf("[%s] %s", scenario, row[1])
    assert_printed(x$Mean, row[2], paste(lab, "Mean"))
    assert_printed(x$Std.Deviation, row[3], paste(lab, "Std. Deviation"))
    if (weighted) {
      assert_spss(x$N, as.numeric(row[4]), tier = "display", precision = 0,
                  label = paste(lab, "N"))
    } else {
      assert_spss_count(x$N, as.integer(row[4]), label = paste(lab, "N"))
    }
  }
}

assert_lm_model <- function(result, spss, scenario) {
  assert_spss(result$model_summary$R,             spss$R,
              tier = "display", precision = 3, label = sprintf("[%s] R", scenario))
  assert_spss(result$model_summary$R_squared,     spss$R2,
              tier = "display", precision = 3, label = sprintf("[%s] R^2", scenario))
  assert_spss(result$model_summary$adj_R_squared, spss$adj_R2,
              tier = "display", precision = 3, label = sprintf("[%s] adj-R^2", scenario))
  # SPSS prints the Std. Error of the Estimate with 3 decimals, with 5 for
  # the income models (se_estimate_dp)
  assert_spss(result$model_summary$std_error,     spss$se_estimate,
              tier = "display", precision = spss$se_estimate_dp %||% 3L,
              label = sprintf("[%s] SE estimate", scenario))
}

assert_lm_anova <- function(result, spss, scenario) {
  reg <- result$anova_table[result$anova_table$Source == "Regression", ]
  res <- result$anova_table[result$anova_table$Source == "Residual", ]
  tot <- result$anova_table[result$anova_table$Source == "Total", ]

  assert_spss(reg$Sum_of_Squares, spss$anova$ss_reg,
              tier = "display", precision = 3, label = sprintf("[%s] SS Reg", scenario))
  assert_spss(res$Sum_of_Squares, spss$anova$ss_res,
              tier = "display", precision = 3, label = sprintf("[%s] SS Res", scenario))
  assert_spss(tot$Sum_of_Squares, spss$anova$ss_tot,
              tier = "display", precision = 3, label = sprintf("[%s] SS Tot", scenario))

  # df: SPSS prints integer; mariposa stores non-integer for weighted; precision=0
  assert_spss(reg$df, spss$anova$df_reg,
              tier = "display", precision = 0, label = sprintf("[%s] df Reg", scenario))
  assert_spss(res$df, spss$anova$df_res,
              tier = "display", precision = 0, label = sprintf("[%s] df Res", scenario))
  assert_spss(tot$df, spss$anova$df_tot,
              tier = "display", precision = 0, label = sprintf("[%s] df Tot", scenario))

  assert_spss(reg$Mean_Square, spss$anova$ms_reg,
              tier = "display", precision = 3, label = sprintf("[%s] MS Reg", scenario))
  assert_spss(res$Mean_Square, spss$anova$ms_res,
              tier = "display", precision = 3, label = sprintf("[%s] MS Res", scenario))

  assert_spss(reg$F_statistic, spss$anova$F,
              tier = "display", precision = 3, label = sprintf("[%s] F", scenario))
  assert_spss(reg$Sig, spss$anova$p,
              tier = "display", precision = 3, what = "p_value",
              label = sprintf("[%s] ANOVA Sig", scenario))
}

assert_lm_coef <- function(result, term_label, spss_coef, scenario) {
  row <- result$coef_table[result$coef_table$Term == term_label, ]
  if (nrow(row) == 0) {
    fail(sprintf("[%s] term %s not found in coefficients", scenario, term_label))
    return(invisible(FALSE))
  }
  assert_spss(row$B,         spss_coef$B,
              tier = "display", precision = 3,
              label = sprintf("[%s] %s B", scenario, term_label))
  assert_spss(row$Std.Error, spss_coef$SE,
              tier = "display", precision = 3,
              label = sprintf("[%s] %s SE", scenario, term_label))
  assert_spss(row$t,         spss_coef$t,
              tier = "display", precision = 3,
              label = sprintf("[%s] %s t", scenario, term_label))
  assert_spss(row$p,         spss_coef$p,
              tier = "display", precision = 3, what = "p_value",
              label = sprintf("[%s] %s p", scenario, term_label))
  if (!is.null(spss_coef$Beta)) {
    assert_spss(row$Beta, spss_coef$Beta,
                tier = "display", precision = 3,
                label = sprintf("[%s] %s Beta", scenario, term_label))
  }
}

# Everything SPSS REGRESSION prints for one model: N, Descriptive Statistics,
# Model Summary, ANOVA and the Coefficients (terms in SPSS order)
assert_lm_scenario <- function(result, spss, scenario, weighted = FALSE) {
  if (weighted) {
    assert_spss(result$n, spss$n, tier = "display", precision = 0,
                label = sprintf("[%s] N", scenario))
  } else {
    assert_spss_count(result$n, spss$n, label = sprintf("[%s] N", scenario))
  }
  assert_lm_desc(result, spss, scenario, weighted)
  assert_lm_model(result, spss, scenario)
  assert_lm_anova(result, spss, scenario)
  terms <- ifelse(names(spss$coefs) == "intercept", "(Intercept)", names(spss$coefs))
  expect_identical(result$coef_table$Term, terms,
                   label = sprintf("[%s] coefficients in SPSS order", scenario))
  for (i in seq_along(terms)) {
    assert_lm_coef(result, terms[i], spss$coefs[[i]], scenario)
  }
}

# One region of a grouped result
lm_group <- function(result, region) {
  Filter(function(g) identical(g$group_values$region, region), result$groups)[[1]]
}


# =============================================================================
# TESTS
# =============================================================================

# ---- Test 1a: unweighted / ungrouped / bivariate ----------------------------
test_that("[1a] linear_regression life_sat ~ age — matches SPSS", {
  r <- linear_regression(survey_data, life_satisfaction ~ age)
  assert_lm_scenario(r, spss_values$test_1a_life_age, "1a")
})


# ---- Test 1b: unweighted / ungrouped / three trust predictors --------------
test_that("[1b] linear_regression life_sat ~ trust_* — matches SPSS", {
  r <- linear_regression(survey_data,
                         life_satisfaction ~ trust_government + trust_media +
                           trust_science)
  assert_lm_scenario(r, spss_values$test_1b_life_trust, "1b")
})


# ---- Test 1c: unweighted / ungrouped / multiple with factor ----------------
test_that("[1c] linear_regression income ~ age + education + life_sat — matches SPSS", {
  # SPSS treats education ordinally; pass factors="numeric" for parity.
  r <- suppressMessages(
    linear_regression(survey_data,
                      income ~ age + education + life_satisfaction,
                      factors = "numeric")
  )
  assert_lm_scenario(r, spss_values$test_1c_income_multi, "1c")
})


# ---- Test 1d: unweighted / ungrouped / four predictors, adj. R^2 < 0 -------
test_that("[1d] linear_regression life_sat ~ four predictors — matches SPSS", {
  r <- linear_regression(survey_data,
                         life_satisfaction ~ age + environmental_concern +
                           political_orientation + trust_government)
  assert_lm_scenario(r, spss_values$test_1d_life_four, "1d")
})


# ---- Test 2a: weighted / ungrouped / bivariate -----------------------------
# Charter §5.1 — uses unrounded sum(w) in all internal calculations.
test_that("[2a] weighted linear_regression life_sat ~ age — matches SPSS (Charter §5.1)", {
  r <- linear_regression(survey_data, life_satisfaction ~ age,
                         weights = sampling_weight)
  assert_lm_scenario(r, spss_values$test_2a_life_age_weighted, "2a",
                     weighted = TRUE)
})


# ---- Test 2b: weighted / ungrouped / three trust predictors ----------------
test_that("[2b] weighted linear_regression life_sat ~ trust_* — matches SPSS", {
  r <- linear_regression(survey_data,
                         life_satisfaction ~ trust_government + trust_media +
                           trust_science,
                         weights = sampling_weight)
  assert_lm_scenario(r, spss_values$test_2b_life_trust_weighted, "2b",
                     weighted = TRUE)
})


# ---- Test 2c: weighted / ungrouped / multiple ------------------------------
test_that("[2c] weighted linear_regression income ~ multi — matches SPSS", {
  r <- suppressMessages(
    linear_regression(survey_data,
                      income ~ age + education + life_satisfaction,
                      weights = sampling_weight,
                      factors = "numeric")
  )
  assert_lm_scenario(r, spss_values$test_2c_income_multi_weighted, "2c",
                     weighted = TRUE)
})


# ---- Test 3a: unweighted / grouped / bivariate -----------------------------
test_that("[3a] grouped linear_regression life_sat ~ age (by region) — matches SPSS", {
  r <- survey_data |>
    dplyr::group_by(region) |>
    linear_regression(life_satisfaction ~ age)

  expect_true(isTRUE(r$is_grouped))
  expect_equal(length(r$groups), 2L)

  assert_lm_scenario(lm_group(r, "East"), spss_values$test_3a_grouped_east, "3a-East")
  assert_lm_scenario(lm_group(r, "West"), spss_values$test_3a_grouped_west, "3a-West")
})


# ---- Test 3b: unweighted / grouped / three trust predictors ----------------
test_that("[3b] grouped linear_regression life_sat ~ trust_* (by region) — matches SPSS", {
  r <- survey_data |>
    dplyr::group_by(region) |>
    linear_regression(life_satisfaction ~ trust_government + trust_media +
                        trust_science)

  expect_equal(length(r$groups), 2L)
  assert_lm_scenario(lm_group(r, "East"), spss_values$test_3b_grouped_east, "3b-East")
  assert_lm_scenario(lm_group(r, "West"), spss_values$test_3b_grouped_west, "3b-West")
})


# ---- Test 4a: weighted / grouped / bivariate -------------------------------
test_that("[4a] weighted+grouped linear_regression — matches SPSS (Charter §5.1)", {
  r <- survey_data |>
    dplyr::group_by(region) |>
    linear_regression(life_satisfaction ~ age, weights = sampling_weight)

  expect_true(isTRUE(r$is_grouped))
  expect_equal(length(r$groups), 2L)

  assert_lm_scenario(lm_group(r, "East"), spss_values$test_4a_weighted_grouped_east,
                     "4a-East", weighted = TRUE)
  assert_lm_scenario(lm_group(r, "West"), spss_values$test_4a_weighted_grouped_west,
                     "4a-West", weighted = TRUE)
})


# ---- Test 4b: weighted / grouped / three trust predictors ------------------
test_that("[4b] weighted+grouped linear_regression life_sat ~ trust_* — matches SPSS", {
  r <- survey_data |>
    dplyr::group_by(region) |>
    linear_regression(life_satisfaction ~ trust_government + trust_media +
                        trust_science,
                      weights = sampling_weight)

  expect_equal(length(r$groups), 2L)
  assert_lm_scenario(lm_group(r, "East"), spss_values$test_4b_weighted_grouped_east,
                     "4b-East", weighted = TRUE)
  assert_lm_scenario(lm_group(r, "West"), spss_values$test_4b_weighted_grouped_west,
                     "4b-West", weighted = TRUE)
})


# =============================================================================
# Behavior tests for the new `factors` argument
# =============================================================================

test_that("factors='dummy' (default) produces L-1 contrasts for unordered factor", {
  set.seed(1)
  df <- tibble::tibble(
    y = rnorm(60),
    x = factor(rep(c("a", "b", "c"), each = 20))
  )
  r <- linear_regression(df, y ~ x)
  # Two dummy contrasts + intercept = 3 rows
  expect_equal(nrow(r$coef_table), 3L)
  expect_setequal(r$coef_table$Term, c("(Intercept)", "xb", "xc"))
})

test_that("factors='numeric' coerces factor levels to integer codes (single B)", {
  set.seed(1)
  df <- tibble::tibble(
    y = rnorm(60),
    x = factor(rep(c("a", "b", "c"), each = 20))
  )
  r <- suppressMessages(linear_regression(df, y ~ x, factors = "numeric"))
  # Single ordinal-as-scale slope + intercept = 2 rows
  expect_equal(nrow(r$coef_table), 2L)
  expect_setequal(r$coef_table$Term, c("(Intercept)", "x"))
})

test_that("factors='numeric' emits one-line cli_inform listing coerced variables", {
  set.seed(1)
  df <- tibble::tibble(y = rnorm(30), x = factor(rep(c("a","b","c"), each = 10)))
  expect_message(
    linear_regression(df, y ~ x, factors = "numeric"),
    regexp = "coerced to numeric"
  )
})

test_that("pairwise + dummy + factor predictor errors with actionable message", {
  set.seed(1)
  df <- tibble::tibble(y = rnorm(30), x = factor(rep(c("a","b","c"), each = 10)))
  expect_error(
    linear_regression(df, y ~ x, use = "pairwise"),
    regexp = "Pairwise deletion"
  )
})


# =============================================================================
# Native generic dispatch (the object IS an lm) — added 0.6.3
# =============================================================================

test_that("listwise+ungrouped result inherits from lm", {
  r <- linear_regression(survey_data, life_satisfaction ~ age + income)
  expect_true(inherits(r, "lm"))
  expect_true(inherits(r, "linear_regression"))
  expect_identical(class(r)[1], "linear_regression")
})

test_that("coef(r) returns lm-style named numeric vector matching coef_table$B", {
  r <- linear_regression(survey_data, life_satisfaction ~ age + income)
  cv <- coef(r)
  expect_type(cv, "double")
  expect_named(cv)
  expect_equal(unname(cv), unname(r$coef_table$B))
})

test_that("predict(r, newdata) dispatches to predict.lm", {
  r <- linear_regression(survey_data, life_satisfaction ~ age + income)
  nd <- head(survey_data, 5)
  p <- predict(r, newdata = nd)
  # Independent recompute via stats::lm on the same complete cases
  d <- survey_data[stats::complete.cases(
    survey_data[, c("life_satisfaction","age","income")]), ]
  expect_equal(unname(p),
               unname(predict(stats::lm(life_satisfaction ~ age + income, d),
                              newdata = nd)))
})

test_that("anova(r) dispatches to anova.lm (sequential SS table)", {
  r <- linear_regression(survey_data, life_satisfaction ~ age + income)
  a <- anova(r)
  expect_s3_class(a, "anova")
  # Three rows: age, income, Residuals
  expect_equal(nrow(a), 3L)
})

test_that("vcov / confint / residuals / fitted / formula dispatch natively", {
  r <- linear_regression(survey_data, life_satisfaction ~ age + income)
  expect_true(is.matrix(vcov(r)))
  expect_true(is.matrix(confint(r)))
  expect_type(residuals(r), "double")
  expect_type(fitted(r), "double")
  expect_s3_class(formula(r), "formula")
  expect_equal(nobs(r), r$n)
})

test_that("model.matrix(r) dispatches natively", {
  r <- linear_regression(survey_data, life_satisfaction ~ age + income)
  X <- stats::model.matrix(r)
  expect_true(is.matrix(X))
  expect_equal(ncol(X), 3L)  # intercept + 2 predictors
})

test_that("predict() on grouped result errors with actionable message", {
  rg <- survey_data |>
    dplyr::group_by(region) |>
    linear_regression(life_satisfaction ~ age)
  expect_error(predict(rg), regexp = "grouped")
})

test_that("predict() on pairwise result errors with actionable message", {
  rp <- linear_regression(survey_data, life_satisfaction ~ age + income,
                          use = "pairwise")
  expect_error(predict(rp), regexp = "pairwise")
})

test_that("each group element of a grouped result inherits from lm", {
  rg <- survey_data |>
    dplyr::group_by(region) |>
    linear_regression(life_satisfaction ~ age)
  for (grp in rg$groups) {
    expect_true(inherits(grp, "lm"))
    # Per-group predict should work directly and return finite predictions
    nd <- head(survey_data, 3)
    pred <- predict(grp, newdata = nd)
    expect_type(pred, "double")
    expect_length(pred, 3L)
    expect_true(all(is.finite(pred)))
  }
})
