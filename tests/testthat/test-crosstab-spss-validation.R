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
#
# Cell caches (cells_*): every Count / Expected Count / % within / % of Total
# / Residual line SPSS prints, one R line per SPSS line (layout and the
# comparison helper: helper-crosstab-cells.R). SPSS's three-way tables
# (TABLES = a BY b BY c) are compared layer by layer with group_by(c); an
# unweighted "Total" layer is the ungrouped two-way table. The weighted
# "Total" layers (Tests 2.3, 2.4, 4.3, 6.3) are not comparable: SPSS sums the
# layers' rounded cells, and crosstab() has no layer argument to reproduce
# that, so they are not cached. crosstab_output.txt ends inside Test 6.4
# (line 1018); its West layer is compared up to that line.
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

  # ===========================================================================
  # CROSSTABS cells, all tests (see header and helper-crosstab-cells.R)
  # ===========================================================================

  # ---- Test 1.1: Gender x Region ----
  cells_1_1 = list(
    all = list(
      count = c(238, 956, 1194,    # crosstab_output.txt:10
                247, 1059, 1306,   # crosstab_output.txt:16
                485, 2015, 2500),  # crosstab_output.txt:22
      expected = c(231.6, 962.4,    # crosstab_output.txt:11
                   253.4, 1052.6),  # crosstab_output.txt:17
      residual = c(6.4, -6.4,   # crosstab_output.txt:15
                   -6.4, 6.4),  # crosstab_output.txt:21
      row_pct = c(19.9, 80.1,   # crosstab_output.txt:12
                  18.9, 81.1,   # crosstab_output.txt:18
                  19.4, 80.6),  # crosstab_output.txt:24
      col_pct = c(49.1, 47.4, 47.8,   # crosstab_output.txt:13
                  50.9, 52.6, 52.2),  # crosstab_output.txt:19
      total_pct = c(9.5, 38.2, 47.8,  # crosstab_output.txt:14
                    9.9, 42.4, 52.2,  # crosstab_output.txt:20
                    19.4, 80.6)       # crosstab_output.txt:26
    )
  ),

  # ---- Test 1.2: Education x Employment ----
  cells_1_2 = list(
    all = list(
      count = c(0, 571, 65, 171, 34, 841,        # crosstab_output.txt:35
                0, 412, 51, 137, 29, 629,        # crosstab_output.txt:41
                44, 366, 44, 145, 32, 631,       # crosstab_output.txt:47
                34, 251, 22, 72, 20, 399,        # crosstab_output.txt:53
                78, 1600, 182, 525, 115, 2500),  # crosstab_output.txt:59
      expected = c(26.2, 538.2, 61.2, 176.6, 38.7,  # crosstab_output.txt:36
                   19.6, 402.6, 45.8, 132.1, 28.9,  # crosstab_output.txt:42
                   19.7, 403.8, 45.9, 132.5, 29.0,  # crosstab_output.txt:48
                   12.4, 255.4, 29.0, 83.8, 18.4),  # crosstab_output.txt:54
      residual = c(-26.2, 32.8, 3.8, -5.6, -4.7,   # crosstab_output.txt:40
                   -19.6, 9.4, 5.2, 4.9, 0.1,      # crosstab_output.txt:46
                   24.3, -37.8, -1.9, 12.5, 3.0,   # crosstab_output.txt:52
                   21.6, -4.4, -7.0, -11.8, 1.6),  # crosstab_output.txt:58
      row_pct = c(0.0, 67.9, 7.7, 20.3, 4.0,   # crosstab_output.txt:37
                  0.0, 65.5, 8.1, 21.8, 4.6,   # crosstab_output.txt:43
                  7.0, 58.0, 7.0, 23.0, 5.1,   # crosstab_output.txt:49
                  8.5, 62.9, 5.5, 18.0, 5.0,   # crosstab_output.txt:55
                  3.1, 64.0, 7.3, 21.0, 4.6),  # crosstab_output.txt:61
      col_pct = c(0.0, 35.7, 35.7, 32.6, 29.6, 33.6,    # crosstab_output.txt:38
                  0.0, 25.8, 28.0, 26.1, 25.2, 25.2,    # crosstab_output.txt:44
                  56.4, 22.9, 24.2, 27.6, 27.8, 25.2,   # crosstab_output.txt:50
                  43.6, 15.7, 12.1, 13.7, 17.4, 16.0),  # crosstab_output.txt:56
      total_pct = c(0.0, 22.8, 2.6, 6.8, 1.4, 33.6,  # crosstab_output.txt:39
                    0.0, 16.5, 2.0, 5.5, 1.2, 25.2,  # crosstab_output.txt:45
                    1.8, 14.6, 1.8, 5.8, 1.3, 25.2,  # crosstab_output.txt:51
                    1.4, 10.0, 0.9, 2.9, 0.8, 16.0,  # crosstab_output.txt:57
                    3.1, 64.0, 7.3, 21.0, 4.6)       # crosstab_output.txt:63
    )
  ),

  # ---- Test 1.3: Life satisfaction x Gender BY Region ----
  cells_1_3 = list(
    East = list(
      count = c(14, 16, 30,      # crosstab_output.txt:72
                29, 29, 58,      # crosstab_output.txt:74
                48, 58, 106,     # crosstab_output.txt:76
                68, 68, 136,     # crosstab_output.txt:78
                69, 66, 135,     # crosstab_output.txt:80
                228, 237, 465),  # crosstab_output.txt:82
      col_pct = c(6.1, 6.8, 6.5,     # crosstab_output.txt:73
                  12.7, 12.2, 12.5,  # crosstab_output.txt:75
                  21.1, 24.5, 22.8,  # crosstab_output.txt:77
                  29.8, 28.7, 29.2,  # crosstab_output.txt:79
                  30.3, 27.8, 29.0)  # crosstab_output.txt:81
    ),
    West = list(
      count = c(46, 42, 88,        # crosstab_output.txt:84
                117, 131, 248,     # crosstab_output.txt:86
                251, 243, 494,     # crosstab_output.txt:88
                261, 334, 595,     # crosstab_output.txt:90
                246, 285, 531,     # crosstab_output.txt:92
                921, 1035, 1956),  # crosstab_output.txt:94
      col_pct = c(5.0, 4.1, 4.5,     # crosstab_output.txt:85
                  12.7, 12.7, 12.7,  # crosstab_output.txt:87
                  27.3, 23.5, 25.3,  # crosstab_output.txt:89
                  28.3, 32.3, 30.4,  # crosstab_output.txt:91
                  26.7, 27.5, 27.1)  # crosstab_output.txt:93
    ),
    Total = list(
      count = c(60, 58, 118,        # crosstab_output.txt:96
                146, 160, 306,      # crosstab_output.txt:98
                299, 301, 600,      # crosstab_output.txt:100
                329, 402, 731,      # crosstab_output.txt:102
                315, 351, 666,      # crosstab_output.txt:104
                1149, 1272, 2421),  # crosstab_output.txt:106
      col_pct = c(5.2, 4.6, 4.9,     # crosstab_output.txt:97
                  12.7, 12.6, 12.6,  # crosstab_output.txt:99
                  26.0, 23.7, 24.8,  # crosstab_output.txt:101
                  28.6, 31.6, 30.2,  # crosstab_output.txt:103
                  27.4, 27.6, 27.5)  # crosstab_output.txt:105
    )
  ),

  # ---- Test 1.4: Life satisfaction x Region BY Gender ----
  cells_1_4 = list(
    Male = list(
      count = c(14, 46, 60,       # crosstab_output.txt:116
                29, 117, 146,     # crosstab_output.txt:118
                48, 251, 299,     # crosstab_output.txt:120
                68, 261, 329,     # crosstab_output.txt:122
                69, 246, 315,     # crosstab_output.txt:124
                228, 921, 1149),  # crosstab_output.txt:126
      col_pct = c(6.1, 5.0, 5.2,     # crosstab_output.txt:117
                  12.7, 12.7, 12.7,  # crosstab_output.txt:119
                  21.1, 27.3, 26.0,  # crosstab_output.txt:121
                  29.8, 28.3, 28.6,  # crosstab_output.txt:123
                  30.3, 26.7, 27.4)  # crosstab_output.txt:125
    ),
    Female = list(
      count = c(16, 42, 58,        # crosstab_output.txt:128
                29, 131, 160,      # crosstab_output.txt:130
                58, 243, 301,      # crosstab_output.txt:132
                68, 334, 402,      # crosstab_output.txt:134
                66, 285, 351,      # crosstab_output.txt:136
                237, 1035, 1272),  # crosstab_output.txt:138
      col_pct = c(6.8, 4.1, 4.6,     # crosstab_output.txt:129
                  12.2, 12.7, 12.6,  # crosstab_output.txt:131
                  24.5, 23.5, 23.7,  # crosstab_output.txt:133
                  28.7, 32.3, 31.6,  # crosstab_output.txt:135
                  27.8, 27.5, 27.6)  # crosstab_output.txt:137
    ),
    Total = list(
      count = c(30, 88, 118,       # crosstab_output.txt:140
                58, 248, 306,      # crosstab_output.txt:142
                106, 494, 600,     # crosstab_output.txt:144
                136, 595, 731,     # crosstab_output.txt:146
                135, 531, 666,     # crosstab_output.txt:148
                465, 1956, 2421),  # crosstab_output.txt:150
      col_pct = c(6.5, 4.5, 4.9,     # crosstab_output.txt:141
                  12.5, 12.7, 12.6,  # crosstab_output.txt:143
                  22.8, 25.3, 24.8,  # crosstab_output.txt:145
                  29.2, 30.4, 30.2,  # crosstab_output.txt:147
                  29.0, 27.1, 27.5)  # crosstab_output.txt:149
    )
  ),

  # ---- Test 2.1: Gender x Region (weighted) ----
  cells_2_1 = list(
    all = list(
      count = c(249, 945, 1194,    # crosstab_output.txt:162
                260, 1062, 1322,   # crosstab_output.txt:168
                509, 2007, 2516),  # crosstab_output.txt:174
      expected = c(241.6, 952.4,    # crosstab_output.txt:163
                   267.4, 1054.6),  # crosstab_output.txt:169
      residual = c(7.4, -7.4,   # crosstab_output.txt:167
                   -7.4, 7.4),  # crosstab_output.txt:173
      row_pct = c(20.9, 79.1,   # crosstab_output.txt:164
                  19.7, 80.3,   # crosstab_output.txt:170
                  20.2, 79.8),  # crosstab_output.txt:176
      col_pct = c(48.9, 47.1, 47.5,   # crosstab_output.txt:165
                  51.1, 52.9, 52.5),  # crosstab_output.txt:171
      total_pct = c(9.9, 37.6, 47.5,   # crosstab_output.txt:166
                    10.3, 42.2, 52.5,  # crosstab_output.txt:172
                    20.2, 79.8)        # crosstab_output.txt:178
    )
  ),

  # ---- Test 2.2: Education x Employment (weighted) ----
  cells_2_2 = list(
    all = list(
      count = c(0, 573, 66, 175, 34, 848,        # crosstab_output.txt:187
                0, 420, 52, 139, 29, 640,        # crosstab_output.txt:193
                46, 370, 45, 149, 33, 643,       # crosstab_output.txt:199
                34, 240, 21, 72, 20, 387,        # crosstab_output.txt:205
                80, 1603, 184, 535, 116, 2518),  # crosstab_output.txt:211
      expected = c(26.9, 539.9, 62.0, 180.2, 39.1,  # crosstab_output.txt:188
                   20.3, 407.4, 46.8, 136.0, 29.5,  # crosstab_output.txt:194
                   20.4, 409.3, 47.0, 136.6, 29.6,  # crosstab_output.txt:200
                   12.3, 246.4, 28.3, 82.2, 17.8),  # crosstab_output.txt:206
      residual = c(-26.9, 33.1, 4.0, -5.2, -5.1,   # crosstab_output.txt:192
                   -20.3, 12.6, 5.2, 3.0, -0.5,    # crosstab_output.txt:198
                   25.6, -39.3, -2.0, 12.4, 3.4,   # crosstab_output.txt:204
                   21.7, -6.4, -7.3, -10.2, 2.2),  # crosstab_output.txt:210
      row_pct = c(0.0, 67.6, 7.8, 20.6, 4.0,   # crosstab_output.txt:189
                  0.0, 65.6, 8.1, 21.7, 4.5,   # crosstab_output.txt:195
                  7.2, 57.5, 7.0, 23.2, 5.1,   # crosstab_output.txt:201
                  8.8, 62.0, 5.4, 18.6, 5.2,   # crosstab_output.txt:207
                  3.2, 63.7, 7.3, 21.2, 4.6),  # crosstab_output.txt:213
      col_pct = c(0.0, 35.7, 35.9, 32.7, 29.3, 33.7,    # crosstab_output.txt:190
                  0.0, 26.2, 28.3, 26.0, 25.0, 25.4,    # crosstab_output.txt:196
                  57.5, 23.1, 24.5, 27.9, 28.4, 25.5,   # crosstab_output.txt:202
                  42.5, 15.0, 11.4, 13.5, 17.2, 15.4),  # crosstab_output.txt:208
      total_pct = c(0.0, 22.8, 2.6, 6.9, 1.4, 33.7,  # crosstab_output.txt:191
                    0.0, 16.7, 2.1, 5.5, 1.2, 25.4,  # crosstab_output.txt:197
                    1.8, 14.7, 1.8, 5.9, 1.3, 25.5,  # crosstab_output.txt:203
                    1.4, 9.5, 0.8, 2.9, 0.8, 15.4,   # crosstab_output.txt:209
                    3.2, 63.7, 7.3, 21.2, 4.6)       # crosstab_output.txt:215
    )
  ),

  # ---- Test 2.3: Life satisfaction x Gender BY Region (weighted; Total layer 248-259 not comparable) ----
  cells_2_3 = list(
    East = list(
      count = c(15, 16, 31,      # crosstab_output.txt:224
                30, 30, 60,      # crosstab_output.txt:226
                50, 62, 112,     # crosstab_output.txt:228
                72, 72, 144,     # crosstab_output.txt:230
                73, 69, 142,     # crosstab_output.txt:232
                240, 249, 489),  # crosstab_output.txt:234
      col_pct = c(6.3, 6.4, 6.3,     # crosstab_output.txt:225
                  12.5, 12.0, 12.3,  # crosstab_output.txt:227
                  20.8, 24.9, 22.9,  # crosstab_output.txt:229
                  30.0, 28.9, 29.4,  # crosstab_output.txt:231
                  30.4, 27.7, 29.0)  # crosstab_output.txt:233
    ),
    West = list(
      count = c(46, 42, 88,        # crosstab_output.txt:236
                117, 130, 247,     # crosstab_output.txt:238
                250, 247, 497,     # crosstab_output.txt:240
                259, 335, 594,     # crosstab_output.txt:242
                240, 284, 524,     # crosstab_output.txt:244
                912, 1038, 1950),  # crosstab_output.txt:246
      col_pct = c(5.0, 4.0, 4.5,     # crosstab_output.txt:237
                  12.8, 12.5, 12.7,  # crosstab_output.txt:239
                  27.4, 23.8, 25.5,  # crosstab_output.txt:241
                  28.4, 32.3, 30.5,  # crosstab_output.txt:243
                  26.3, 27.4, 26.9)  # crosstab_output.txt:245
    )
  ),

  # ---- Test 2.4: Life satisfaction x Region BY Gender (weighted; Total layer 292-303 not comparable) ----
  cells_2_4 = list(
    Male = list(
      count = c(15, 46, 61,       # crosstab_output.txt:268
                30, 117, 147,     # crosstab_output.txt:270
                50, 250, 300,     # crosstab_output.txt:272
                72, 259, 331,     # crosstab_output.txt:274
                73, 240, 313,     # crosstab_output.txt:276
                240, 912, 1152),  # crosstab_output.txt:278
      col_pct = c(6.3, 5.0, 5.3,     # crosstab_output.txt:269
                  12.5, 12.8, 12.8,  # crosstab_output.txt:271
                  20.8, 27.4, 26.0,  # crosstab_output.txt:273
                  30.0, 28.4, 28.7,  # crosstab_output.txt:275
                  30.4, 26.3, 27.2)  # crosstab_output.txt:277
    ),
    Female = list(
      count = c(16, 42, 58,        # crosstab_output.txt:280
                30, 130, 160,      # crosstab_output.txt:282
                62, 247, 309,      # crosstab_output.txt:284
                72, 335, 407,      # crosstab_output.txt:286
                69, 284, 353,      # crosstab_output.txt:288
                249, 1038, 1287),  # crosstab_output.txt:290
      col_pct = c(6.4, 4.0, 4.5,     # crosstab_output.txt:281
                  12.0, 12.5, 12.4,  # crosstab_output.txt:283
                  24.9, 23.8, 24.0,  # crosstab_output.txt:285
                  28.9, 32.3, 31.6,  # crosstab_output.txt:287
                  27.7, 27.4, 27.4)  # crosstab_output.txt:289
    )
  ),

  # ---- Test 3.1: Gender x Education, SPLIT FILE region ----
  cells_3_1 = list(
    East = list(
      count = c(82, 60, 59, 37, 238,      # crosstab_output.txt:314
                88, 61, 56, 42, 247,      # crosstab_output.txt:319
                170, 121, 115, 79, 485),  # crosstab_output.txt:324
      row_pct = c(34.5, 25.2, 24.8, 15.5,   # crosstab_output.txt:315
                  35.6, 24.7, 22.7, 17.0,   # crosstab_output.txt:320
                  35.1, 24.9, 23.7, 16.3),  # crosstab_output.txt:325
      col_pct = c(48.2, 49.6, 51.3, 46.8, 49.1,   # crosstab_output.txt:316
                  51.8, 50.4, 48.7, 53.2, 50.9),  # crosstab_output.txt:321
      total_pct = c(16.9, 12.4, 12.2, 7.6, 49.1,  # crosstab_output.txt:318
                    18.1, 12.6, 11.5, 8.7, 50.9,  # crosstab_output.txt:323
                    35.1, 24.9, 23.7, 16.3)       # crosstab_output.txt:328
    ),
    West = list(
      count = c(319, 229, 261, 147, 956,    # crosstab_output.txt:329
                352, 279, 255, 173, 1059,   # crosstab_output.txt:334
                671, 508, 516, 320, 2015),  # crosstab_output.txt:339
      row_pct = c(33.4, 24.0, 27.3, 15.4,   # crosstab_output.txt:330
                  33.2, 26.3, 24.1, 16.3,   # crosstab_output.txt:335
                  33.3, 25.2, 25.6, 15.9),  # crosstab_output.txt:340
      col_pct = c(47.5, 45.1, 50.6, 45.9, 47.4,   # crosstab_output.txt:331
                  52.5, 54.9, 49.4, 54.1, 52.6),  # crosstab_output.txt:336
      total_pct = c(15.8, 11.4, 13.0, 7.3, 47.4,  # crosstab_output.txt:333
                    17.5, 13.8, 12.7, 8.6, 52.6,  # crosstab_output.txt:338
                    33.3, 25.2, 25.6, 15.9)       # crosstab_output.txt:343
    )
  ),

  # ---- Test 3.2: Life satisfaction x Gender, SPLIT FILE region ----
  cells_3_2 = list(
    East = list(
      count = c(14, 16, 30,      # crosstab_output.txt:352
                29, 29, 58,      # crosstab_output.txt:355
                48, 58, 106,     # crosstab_output.txt:358
                68, 68, 136,     # crosstab_output.txt:361
                69, 66, 135,     # crosstab_output.txt:364
                228, 237, 465),  # crosstab_output.txt:367
      expected = c(14.7, 15.3,   # crosstab_output.txt:353
                   28.4, 29.6,   # crosstab_output.txt:356
                   52.0, 54.0,   # crosstab_output.txt:359
                   66.7, 69.3,   # crosstab_output.txt:362
                   66.2, 68.8),  # crosstab_output.txt:365
      col_pct = c(6.1, 6.8, 6.5,     # crosstab_output.txt:354
                  12.7, 12.2, 12.5,  # crosstab_output.txt:357
                  21.1, 24.5, 22.8,  # crosstab_output.txt:360
                  29.8, 28.7, 29.2,  # crosstab_output.txt:363
                  30.3, 27.8, 29.0)  # crosstab_output.txt:366
    ),
    West = list(
      count = c(46, 42, 88,        # crosstab_output.txt:370
                117, 131, 248,     # crosstab_output.txt:373
                251, 243, 494,     # crosstab_output.txt:376
                261, 334, 595,     # crosstab_output.txt:379
                246, 285, 531,     # crosstab_output.txt:382
                921, 1035, 1956),  # crosstab_output.txt:385
      expected = c(41.4, 46.6,     # crosstab_output.txt:371
                   116.8, 131.2,   # crosstab_output.txt:374
                   232.6, 261.4,   # crosstab_output.txt:377
                   280.2, 314.8,   # crosstab_output.txt:380
                   250.0, 281.0),  # crosstab_output.txt:383
      col_pct = c(5.0, 4.1, 4.5,     # crosstab_output.txt:372
                  12.7, 12.7, 12.7,  # crosstab_output.txt:375
                  27.3, 23.5, 25.3,  # crosstab_output.txt:378
                  28.3, 32.3, 30.4,  # crosstab_output.txt:381
                  26.7, 27.5, 27.1)  # crosstab_output.txt:384
    )
  ),

  # ---- Test 3.3: Education x Employment BY Gender, SPLIT FILE region ----
  cells_3_3 = list(
    East_Male = list(
      count = c(0, 49, 6, 18, 9, 82,       # crosstab_output.txt:396
                0, 40, 7, 10, 3, 60,       # crosstab_output.txt:398
                3, 32, 3, 20, 1, 59,       # crosstab_output.txt:400
                4, 24, 0, 7, 2, 37,        # crosstab_output.txt:402
                7, 145, 16, 55, 15, 238),  # crosstab_output.txt:404
      col_pct = c(0.0, 33.8, 37.5, 32.7, 60.0, 34.5,  # crosstab_output.txt:397
                  0.0, 27.6, 43.8, 18.2, 20.0, 25.2,  # crosstab_output.txt:399
                  42.9, 22.1, 18.8, 36.4, 6.7, 24.8,  # crosstab_output.txt:401
                  57.1, 16.6, 0.0, 12.7, 13.3, 15.5)  # crosstab_output.txt:403
    ),
    East_Female = list(
      count = c(0, 62, 5, 18, 3, 88,      # crosstab_output.txt:406
                0, 39, 5, 16, 1, 61,      # crosstab_output.txt:408
                3, 37, 4, 12, 0, 56,      # crosstab_output.txt:410
                1, 28, 1, 10, 2, 42,      # crosstab_output.txt:412
                4, 166, 15, 56, 6, 247),  # crosstab_output.txt:414
      col_pct = c(0.0, 37.3, 33.3, 32.1, 50.0, 35.6,  # crosstab_output.txt:407
                  0.0, 23.5, 33.3, 28.6, 16.7, 24.7,  # crosstab_output.txt:409
                  75.0, 22.3, 26.7, 21.4, 0.0, 22.7,  # crosstab_output.txt:411
                  25.0, 16.9, 6.7, 17.9, 33.3, 17.0)  # crosstab_output.txt:413
    ),
    East_Total = list(
      count = c(0, 111, 11, 36, 12, 170,     # crosstab_output.txt:416
                0, 79, 12, 26, 4, 121,       # crosstab_output.txt:418
                6, 69, 7, 32, 1, 115,        # crosstab_output.txt:420
                5, 52, 1, 17, 4, 79,         # crosstab_output.txt:422
                11, 311, 31, 111, 21, 485),  # crosstab_output.txt:424
      col_pct = c(0.0, 35.7, 35.5, 32.4, 57.1, 35.1,  # crosstab_output.txt:417
                  0.0, 25.4, 38.7, 23.4, 19.0, 24.9,  # crosstab_output.txt:419
                  54.5, 22.2, 22.6, 28.8, 4.8, 23.7,  # crosstab_output.txt:421
                  45.5, 16.7, 3.2, 15.3, 19.0, 16.3)  # crosstab_output.txt:423
    ),
    West_Male = list(
      count = c(0, 216, 24, 62, 17, 319,     # crosstab_output.txt:426
                0, 144, 16, 58, 11, 229,     # crosstab_output.txt:428
                18, 152, 18, 58, 15, 261,    # crosstab_output.txt:430
                11, 93, 10, 23, 10, 147,     # crosstab_output.txt:432
                29, 605, 68, 201, 53, 956),  # crosstab_output.txt:434
      col_pct = c(0.0, 35.7, 35.3, 30.8, 32.1, 33.4,   # crosstab_output.txt:427
                  0.0, 23.8, 23.5, 28.9, 20.8, 24.0,   # crosstab_output.txt:429
                  62.1, 25.1, 26.5, 28.9, 28.3, 27.3,  # crosstab_output.txt:431
                  37.9, 15.4, 14.7, 11.4, 18.9, 15.4)  # crosstab_output.txt:433
    ),
    West_Female = list(
      count = c(0, 244, 30, 73, 5, 352,       # crosstab_output.txt:436
                0, 189, 23, 53, 14, 279,      # crosstab_output.txt:438
                20, 145, 19, 55, 16, 255,     # crosstab_output.txt:440
                18, 106, 11, 32, 6, 173,      # crosstab_output.txt:442
                38, 684, 83, 213, 41, 1059),  # crosstab_output.txt:444
      col_pct = c(0.0, 35.7, 36.1, 34.3, 12.2, 33.2,   # crosstab_output.txt:437
                  0.0, 27.6, 27.7, 24.9, 34.1, 26.3,   # crosstab_output.txt:439
                  52.6, 21.2, 22.9, 25.8, 39.0, 24.1,  # crosstab_output.txt:441
                  47.4, 15.5, 13.3, 15.0, 14.6, 16.3)  # crosstab_output.txt:443
    ),
    West_Total = list(
      count = c(0, 460, 54, 135, 22, 671,       # crosstab_output.txt:446
                0, 333, 39, 111, 25, 508,       # crosstab_output.txt:448
                38, 297, 37, 113, 31, 516,      # crosstab_output.txt:450
                29, 199, 21, 55, 16, 320,       # crosstab_output.txt:452
                67, 1289, 151, 414, 94, 2015),  # crosstab_output.txt:454
      col_pct = c(0.0, 35.7, 35.8, 32.6, 23.4, 33.3,   # crosstab_output.txt:447
                  0.0, 25.8, 25.8, 26.8, 26.6, 25.2,   # crosstab_output.txt:449
                  56.7, 23.0, 24.5, 27.3, 33.0, 25.6,  # crosstab_output.txt:451
                  43.3, 15.4, 13.9, 13.3, 17.0, 15.9)  # crosstab_output.txt:453
    )
  ),

  # ---- Test 4.1: Gender x Education, weighted, SPLIT FILE region ----
  cells_4_1 = list(
    East = list(
      count = c(83, 63, 64, 39, 249,      # crosstab_output.txt:466
                92, 66, 59, 43, 260,      # crosstab_output.txt:471
                175, 129, 123, 82, 509),  # crosstab_output.txt:476
      row_pct = c(33.3, 25.3, 25.7, 15.7,   # crosstab_output.txt:467
                  35.4, 25.4, 22.7, 16.5,   # crosstab_output.txt:472
                  34.4, 25.3, 24.2, 16.1),  # crosstab_output.txt:477
      col_pct = c(47.4, 48.8, 52.0, 47.6, 48.9,   # crosstab_output.txt:468
                  52.6, 51.2, 48.0, 52.4, 51.1),  # crosstab_output.txt:473
      total_pct = c(16.3, 12.4, 12.6, 7.7, 48.9,  # crosstab_output.txt:470
                    18.1, 13.0, 11.6, 8.4, 51.1,  # crosstab_output.txt:475
                    34.4, 25.3, 24.2, 16.1)       # crosstab_output.txt:480
    ),
    West = list(
      count = c(318, 228, 262, 137, 945,    # crosstab_output.txt:481
                355, 284, 257, 166, 1062,   # crosstab_output.txt:486
                673, 512, 519, 303, 2007),  # crosstab_output.txt:491
      row_pct = c(33.7, 24.1, 27.7, 14.5,   # crosstab_output.txt:482
                  33.4, 26.7, 24.2, 15.6,   # crosstab_output.txt:487
                  33.5, 25.5, 25.9, 15.1),  # crosstab_output.txt:492
      col_pct = c(47.3, 44.5, 50.5, 45.2, 47.1,   # crosstab_output.txt:483
                  52.7, 55.5, 49.5, 54.8, 52.9),  # crosstab_output.txt:488
      total_pct = c(15.8, 11.4, 13.1, 6.8, 47.1,  # crosstab_output.txt:485
                    17.7, 14.2, 12.8, 8.3, 52.9,  # crosstab_output.txt:490
                    33.5, 25.5, 25.9, 15.1)       # crosstab_output.txt:495
    )
  ),

  # ---- Test 4.2: Life satisfaction x Gender, weighted, SPLIT FILE region ----
  cells_4_2 = list(
    East = list(
      count = c(15, 16, 31,      # crosstab_output.txt:504
                30, 30, 60,      # crosstab_output.txt:507
                50, 62, 112,     # crosstab_output.txt:510
                72, 72, 144,     # crosstab_output.txt:513
                73, 69, 142,     # crosstab_output.txt:516
                240, 249, 489),  # crosstab_output.txt:519
      expected = c(15.2, 15.8,   # crosstab_output.txt:505
                   29.4, 30.6,   # crosstab_output.txt:508
                   55.0, 57.0,   # crosstab_output.txt:511
                   70.7, 73.3,   # crosstab_output.txt:514
                   69.7, 72.3),  # crosstab_output.txt:517
      col_pct = c(6.3, 6.4, 6.3,     # crosstab_output.txt:506
                  12.5, 12.0, 12.3,  # crosstab_output.txt:509
                  20.8, 24.9, 22.9,  # crosstab_output.txt:512
                  30.0, 28.9, 29.4,  # crosstab_output.txt:515
                  30.4, 27.7, 29.0)  # crosstab_output.txt:518
    ),
    West = list(
      count = c(46, 42, 88,        # crosstab_output.txt:522
                117, 130, 247,     # crosstab_output.txt:525
                250, 247, 497,     # crosstab_output.txt:528
                259, 335, 594,     # crosstab_output.txt:531
                240, 284, 524,     # crosstab_output.txt:534
                912, 1038, 1950),  # crosstab_output.txt:537
      expected = c(41.2, 46.8,     # crosstab_output.txt:523
                   115.5, 131.5,   # crosstab_output.txt:526
                   232.4, 264.6,   # crosstab_output.txt:529
                   277.8, 316.2,   # crosstab_output.txt:532
                   245.1, 278.9),  # crosstab_output.txt:535
      col_pct = c(5.0, 4.0, 4.5,     # crosstab_output.txt:524
                  12.8, 12.5, 12.7,  # crosstab_output.txt:527
                  27.4, 23.8, 25.5,  # crosstab_output.txt:530
                  28.4, 32.3, 30.5,  # crosstab_output.txt:533
                  26.3, 27.4, 26.9)  # crosstab_output.txt:536
    )
  ),

  # ---- Test 4.3: Education x Employment BY Gender, weighted, SPLIT FILE region (Total layers 568-577/598-607 not comparable) ----
  cells_4_3 = list(
    East_Male = list(
      count = c(0, 48, 6, 20, 9, 83,       # crosstab_output.txt:548
                0, 42, 7, 11, 3, 63,       # crosstab_output.txt:550
                3, 34, 4, 23, 1, 65,       # crosstab_output.txt:552
                4, 25, 0, 8, 2, 39,        # crosstab_output.txt:554
                7, 149, 17, 62, 15, 250),  # crosstab_output.txt:556
      col_pct = c(0.0, 32.2, 35.3, 32.3, 60.0, 33.2,  # crosstab_output.txt:549
                  0.0, 28.2, 41.2, 17.7, 20.0, 25.2,  # crosstab_output.txt:551
                  42.9, 22.8, 23.5, 37.1, 6.7, 26.0,  # crosstab_output.txt:553
                  57.1, 16.8, 0.0, 12.9, 13.3, 15.6)  # crosstab_output.txt:555
    ),
    East_Female = list(
      count = c(0, 64, 5, 19, 3, 91,      # crosstab_output.txt:558
                0, 41, 5, 18, 1, 65,      # crosstab_output.txt:560
                3, 38, 4, 13, 0, 58,      # crosstab_output.txt:562
                1, 28, 1, 11, 2, 43,      # crosstab_output.txt:564
                4, 171, 15, 61, 6, 257),  # crosstab_output.txt:566
      col_pct = c(0.0, 37.4, 33.3, 31.1, 50.0, 35.4,  # crosstab_output.txt:559
                  0.0, 24.0, 33.3, 29.5, 16.7, 25.3,  # crosstab_output.txt:561
                  75.0, 22.2, 26.7, 21.3, 0.0, 22.6,  # crosstab_output.txt:563
                  25.0, 16.4, 6.7, 18.0, 33.3, 16.7)  # crosstab_output.txt:565
    ),
    West_Male = list(
      count = c(0, 216, 24, 62, 17, 319,     # crosstab_output.txt:578
                0, 144, 16, 57, 11, 228,     # crosstab_output.txt:580
                19, 152, 18, 59, 15, 263,    # crosstab_output.txt:582
                10, 86, 9, 22, 10, 137,      # crosstab_output.txt:584
                29, 598, 67, 200, 53, 947),  # crosstab_output.txt:586
      col_pct = c(0.0, 36.1, 35.8, 31.0, 32.1, 33.7,   # crosstab_output.txt:579
                  0.0, 24.1, 23.9, 28.5, 20.8, 24.1,   # crosstab_output.txt:581
                  65.5, 25.4, 26.9, 29.5, 28.3, 27.8,  # crosstab_output.txt:583
                  34.5, 14.4, 13.4, 11.0, 18.9, 14.5)  # crosstab_output.txt:585
    ),
    West_Female = list(
      count = c(0, 245, 31, 74, 5, 355,       # crosstab_output.txt:588
                0, 193, 24, 53, 15, 285,      # crosstab_output.txt:590
                21, 146, 19, 54, 16, 256,     # crosstab_output.txt:592
                18, 100, 11, 31, 5, 165,      # crosstab_output.txt:594
                39, 684, 85, 212, 41, 1061),  # crosstab_output.txt:596
      col_pct = c(0.0, 35.8, 36.5, 34.9, 12.2, 33.5,   # crosstab_output.txt:589
                  0.0, 28.2, 28.2, 25.0, 36.6, 26.9,   # crosstab_output.txt:591
                  53.8, 21.3, 22.4, 25.5, 39.0, 24.1,  # crosstab_output.txt:593
                  46.2, 14.6, 12.9, 14.6, 12.2, 15.6)  # crosstab_output.txt:595
    )
  ),

  # ---- Test 5.1: Income x Life satisfaction (missing values) ----
  cells_5_1 = list(
    all = list(
      count = c(4, 0, 0, 0, 0, 4,                # crosstab_output.txt:618
                2, 0, 1, 0, 0, 3,                # crosstab_output.txt:621
                3, 2, 0, 0, 0, 5,                # crosstab_output.txt:624
                1, 3, 1, 0, 0, 5,                # crosstab_output.txt:627
                2, 2, 1, 1, 0, 6,                # crosstab_output.txt:630
                3, 4, 1, 1, 0, 9,                # crosstab_output.txt:633
                4, 5, 2, 1, 0, 12,               # crosstab_output.txt:636
                2, 1, 6, 1, 0, 10,               # crosstab_output.txt:639
                8, 4, 6, 0, 0, 18,               # crosstab_output.txt:642
                3, 8, 6, 2, 0, 19,               # crosstab_output.txt:645
                5, 6, 11, 1, 0, 23,              # crosstab_output.txt:648
                10, 12, 12, 4, 0, 38,            # crosstab_output.txt:651
                11, 11, 14, 4, 0, 40,            # crosstab_output.txt:654
                7, 18, 10, 1, 0, 36,             # crosstab_output.txt:657
                8, 9, 9, 4, 0, 30,               # crosstab_output.txt:660
                16, 13, 21, 4, 0, 54,            # crosstab_output.txt:663
                11, 21, 19, 5, 0, 56,            # crosstab_output.txt:666
                0, 8, 13, 15, 18, 54,            # crosstab_output.txt:669
                0, 8, 16, 20, 26, 70,            # crosstab_output.txt:672
                0, 8, 10, 20, 17, 55,            # crosstab_output.txt:675
                0, 7, 23, 18, 21, 69,            # crosstab_output.txt:678
                0, 7, 14, 20, 13, 54,            # crosstab_output.txt:681
                0, 11, 16, 15, 18, 60,           # crosstab_output.txt:684
                0, 13, 15, 24, 16, 68,           # crosstab_output.txt:687
                0, 12, 16, 22, 19, 69,           # crosstab_output.txt:690
                0, 11, 16, 26, 17, 70,           # crosstab_output.txt:693
                0, 14, 19, 21, 11, 65,           # crosstab_output.txt:696
                0, 12, 18, 25, 13, 68,           # crosstab_output.txt:699
                0, 11, 22, 12, 15, 60,           # crosstab_output.txt:702
                0, 7, 12, 23, 11, 53,            # crosstab_output.txt:705
                0, 6, 13, 22, 16, 57,            # crosstab_output.txt:708
                0, 9, 18, 25, 16, 68,            # crosstab_output.txt:711
                0, 0, 8, 14, 26, 48,             # crosstab_output.txt:714
                0, 0, 9, 17, 25, 51,             # crosstab_output.txt:717
                0, 0, 10, 15, 16, 41,            # crosstab_output.txt:720
                0, 0, 8, 15, 18, 41,             # crosstab_output.txt:723
                0, 0, 11, 11, 14, 36,            # crosstab_output.txt:726
                0, 0, 8, 13, 23, 44,             # crosstab_output.txt:729
                0, 0, 9, 12, 14, 35,             # crosstab_output.txt:732
                0, 0, 10, 15, 16, 41,            # crosstab_output.txt:735
                0, 0, 9, 15, 16, 40,             # crosstab_output.txt:738
                0, 0, 7, 12, 15, 34,             # crosstab_output.txt:741
                0, 0, 0, 13, 15, 28,             # crosstab_output.txt:744
                0, 0, 4, 9, 14, 27,              # crosstab_output.txt:747
                0, 0, 2, 10, 7, 19,              # crosstab_output.txt:750
                0, 0, 6, 10, 17, 33,             # crosstab_output.txt:753
                0, 0, 7, 9, 10, 26,              # crosstab_output.txt:756
                0, 0, 8, 6, 6, 20,               # crosstab_output.txt:759
                0, 0, 3, 15, 8, 26,              # crosstab_output.txt:762
                0, 0, 4, 11, 7, 22,              # crosstab_output.txt:765
                0, 0, 3, 6, 4, 13,               # crosstab_output.txt:768
                0, 0, 4, 7, 10, 21,              # crosstab_output.txt:771
                0, 0, 2, 10, 3, 15,              # crosstab_output.txt:774
                0, 0, 0, 3, 1, 4,                # crosstab_output.txt:777
                0, 0, 3, 1, 1, 5,                # crosstab_output.txt:780
                0, 0, 1, 4, 5, 10,               # crosstab_output.txt:783
                0, 0, 1, 4, 3, 8,                # crosstab_output.txt:786
                0, 0, 1, 7, 4, 12,               # crosstab_output.txt:789
                0, 0, 3, 3, 4, 10,               # crosstab_output.txt:792
                0, 0, 3, 6, 1, 10,               # crosstab_output.txt:795
                0, 0, 1, 2, 2, 5,                # crosstab_output.txt:798
                0, 0, 0, 3, 3, 6,                # crosstab_output.txt:801
                0, 0, 3, 0, 2, 5,                # crosstab_output.txt:804
                0, 0, 4, 2, 3, 9,                # crosstab_output.txt:807
                0, 0, 0, 3, 2, 5,                # crosstab_output.txt:810
                0, 0, 2, 0, 1, 3,                # crosstab_output.txt:813
                0, 0, 1, 3, 1, 5,                # crosstab_output.txt:816
                0, 0, 2, 0, 3, 5,                # crosstab_output.txt:819
                0, 0, 1, 1, 6, 8,                # crosstab_output.txt:822
                0, 0, 1, 0, 0, 1,                # crosstab_output.txt:825
                0, 0, 1, 3, 2, 6,                # crosstab_output.txt:828
                0, 0, 1, 0, 2, 3,                # crosstab_output.txt:831
                0, 0, 3, 15, 8, 26,              # crosstab_output.txt:834
                100, 263, 525, 642, 585, 2115),  # crosstab_output.txt:837
      row_pct = c(100.0, 0.0, 0.0, 0.0, 0.0,     # crosstab_output.txt:619
                  66.7, 0.0, 33.3, 0.0, 0.0,     # crosstab_output.txt:622
                  60.0, 40.0, 0.0, 0.0, 0.0,     # crosstab_output.txt:625
                  20.0, 60.0, 20.0, 0.0, 0.0,    # crosstab_output.txt:628
                  33.3, 33.3, 16.7, 16.7, 0.0,   # crosstab_output.txt:631
                  33.3, 44.4, 11.1, 11.1, 0.0,   # crosstab_output.txt:634
                  33.3, 41.7, 16.7, 8.3, 0.0,    # crosstab_output.txt:637
                  20.0, 10.0, 60.0, 10.0, 0.0,   # crosstab_output.txt:640
                  44.4, 22.2, 33.3, 0.0, 0.0,    # crosstab_output.txt:643
                  15.8, 42.1, 31.6, 10.5, 0.0,   # crosstab_output.txt:646
                  21.7, 26.1, 47.8, 4.3, 0.0,    # crosstab_output.txt:649
                  26.3, 31.6, 31.6, 10.5, 0.0,   # crosstab_output.txt:652
                  27.5, 27.5, 35.0, 10.0, 0.0,   # crosstab_output.txt:655
                  19.4, 50.0, 27.8, 2.8, 0.0,    # crosstab_output.txt:658
                  26.7, 30.0, 30.0, 13.3, 0.0,   # crosstab_output.txt:661
                  29.6, 24.1, 38.9, 7.4, 0.0,    # crosstab_output.txt:664
                  19.6, 37.5, 33.9, 8.9, 0.0,    # crosstab_output.txt:667
                  0.0, 14.8, 24.1, 27.8, 33.3,   # crosstab_output.txt:670
                  0.0, 11.4, 22.9, 28.6, 37.1,   # crosstab_output.txt:673
                  0.0, 14.5, 18.2, 36.4, 30.9,   # crosstab_output.txt:676
                  0.0, 10.1, 33.3, 26.1, 30.4,   # crosstab_output.txt:679
                  0.0, 13.0, 25.9, 37.0, 24.1,   # crosstab_output.txt:682
                  0.0, 18.3, 26.7, 25.0, 30.0,   # crosstab_output.txt:685
                  0.0, 19.1, 22.1, 35.3, 23.5,   # crosstab_output.txt:688
                  0.0, 17.4, 23.2, 31.9, 27.5,   # crosstab_output.txt:691
                  0.0, 15.7, 22.9, 37.1, 24.3,   # crosstab_output.txt:694
                  0.0, 21.5, 29.2, 32.3, 16.9,   # crosstab_output.txt:697
                  0.0, 17.6, 26.5, 36.8, 19.1,   # crosstab_output.txt:700
                  0.0, 18.3, 36.7, 20.0, 25.0,   # crosstab_output.txt:703
                  0.0, 13.2, 22.6, 43.4, 20.8,   # crosstab_output.txt:706
                  0.0, 10.5, 22.8, 38.6, 28.1,   # crosstab_output.txt:709
                  0.0, 13.2, 26.5, 36.8, 23.5,   # crosstab_output.txt:712
                  0.0, 0.0, 16.7, 29.2, 54.2,    # crosstab_output.txt:715
                  0.0, 0.0, 17.6, 33.3, 49.0,    # crosstab_output.txt:718
                  0.0, 0.0, 24.4, 36.6, 39.0,    # crosstab_output.txt:721
                  0.0, 0.0, 19.5, 36.6, 43.9,    # crosstab_output.txt:724
                  0.0, 0.0, 30.6, 30.6, 38.9,    # crosstab_output.txt:727
                  0.0, 0.0, 18.2, 29.5, 52.3,    # crosstab_output.txt:730
                  0.0, 0.0, 25.7, 34.3, 40.0,    # crosstab_output.txt:733
                  0.0, 0.0, 24.4, 36.6, 39.0,    # crosstab_output.txt:736
                  0.0, 0.0, 22.5, 37.5, 40.0,    # crosstab_output.txt:739
                  0.0, 0.0, 20.6, 35.3, 44.1,    # crosstab_output.txt:742
                  0.0, 0.0, 0.0, 46.4, 53.6,     # crosstab_output.txt:745
                  0.0, 0.0, 14.8, 33.3, 51.9,    # crosstab_output.txt:748
                  0.0, 0.0, 10.5, 52.6, 36.8,    # crosstab_output.txt:751
                  0.0, 0.0, 18.2, 30.3, 51.5,    # crosstab_output.txt:754
                  0.0, 0.0, 26.9, 34.6, 38.5,    # crosstab_output.txt:757
                  0.0, 0.0, 40.0, 30.0, 30.0,    # crosstab_output.txt:760
                  0.0, 0.0, 11.5, 57.7, 30.8,    # crosstab_output.txt:763
                  0.0, 0.0, 18.2, 50.0, 31.8,    # crosstab_output.txt:766
                  0.0, 0.0, 23.1, 46.2, 30.8,    # crosstab_output.txt:769
                  0.0, 0.0, 19.0, 33.3, 47.6,    # crosstab_output.txt:772
                  0.0, 0.0, 13.3, 66.7, 20.0,    # crosstab_output.txt:775
                  0.0, 0.0, 0.0, 75.0, 25.0,     # crosstab_output.txt:778
                  0.0, 0.0, 60.0, 20.0, 20.0,    # crosstab_output.txt:781
                  0.0, 0.0, 10.0, 40.0, 50.0,    # crosstab_output.txt:784
                  0.0, 0.0, 12.5, 50.0, 37.5,    # crosstab_output.txt:787
                  0.0, 0.0, 8.3, 58.3, 33.3,     # crosstab_output.txt:790
                  0.0, 0.0, 30.0, 30.0, 40.0,    # crosstab_output.txt:793
                  0.0, 0.0, 30.0, 60.0, 10.0,    # crosstab_output.txt:796
                  0.0, 0.0, 20.0, 40.0, 40.0,    # crosstab_output.txt:799
                  0.0, 0.0, 0.0, 50.0, 50.0,     # crosstab_output.txt:802
                  0.0, 0.0, 60.0, 0.0, 40.0,     # crosstab_output.txt:805
                  0.0, 0.0, 44.4, 22.2, 33.3,    # crosstab_output.txt:808
                  0.0, 0.0, 0.0, 60.0, 40.0,     # crosstab_output.txt:811
                  0.0, 0.0, 66.7, 0.0, 33.3,     # crosstab_output.txt:814
                  0.0, 0.0, 20.0, 60.0, 20.0,    # crosstab_output.txt:817
                  0.0, 0.0, 40.0, 0.0, 60.0,     # crosstab_output.txt:820
                  0.0, 0.0, 12.5, 12.5, 75.0,    # crosstab_output.txt:823
                  0.0, 0.0, 100.0, 0.0, 0.0,     # crosstab_output.txt:826
                  0.0, 0.0, 16.7, 50.0, 33.3,    # crosstab_output.txt:829
                  0.0, 0.0, 33.3, 0.0, 66.7,     # crosstab_output.txt:832
                  0.0, 0.0, 11.5, 57.7, 30.8,    # crosstab_output.txt:835
                  4.7, 12.4, 24.8, 30.4, 27.7),  # crosstab_output.txt:838
      col_pct = c(4.0, 0.0, 0.0, 0.0, 0.0, 0.2,   # crosstab_output.txt:620
                  2.0, 0.0, 0.2, 0.0, 0.0, 0.1,   # crosstab_output.txt:623
                  3.0, 0.8, 0.0, 0.0, 0.0, 0.2,   # crosstab_output.txt:626
                  1.0, 1.1, 0.2, 0.0, 0.0, 0.2,   # crosstab_output.txt:629
                  2.0, 0.8, 0.2, 0.2, 0.0, 0.3,   # crosstab_output.txt:632
                  3.0, 1.5, 0.2, 0.2, 0.0, 0.4,   # crosstab_output.txt:635
                  4.0, 1.9, 0.4, 0.2, 0.0, 0.6,   # crosstab_output.txt:638
                  2.0, 0.4, 1.1, 0.2, 0.0, 0.5,   # crosstab_output.txt:641
                  8.0, 1.5, 1.1, 0.0, 0.0, 0.9,   # crosstab_output.txt:644
                  3.0, 3.0, 1.1, 0.3, 0.0, 0.9,   # crosstab_output.txt:647
                  5.0, 2.3, 2.1, 0.2, 0.0, 1.1,   # crosstab_output.txt:650
                  10.0, 4.6, 2.3, 0.6, 0.0, 1.8,  # crosstab_output.txt:653
                  11.0, 4.2, 2.7, 0.6, 0.0, 1.9,  # crosstab_output.txt:656
                  7.0, 6.8, 1.9, 0.2, 0.0, 1.7,   # crosstab_output.txt:659
                  8.0, 3.4, 1.7, 0.6, 0.0, 1.4,   # crosstab_output.txt:662
                  16.0, 4.9, 4.0, 0.6, 0.0, 2.6,  # crosstab_output.txt:665
                  11.0, 8.0, 3.6, 0.8, 0.0, 2.6,  # crosstab_output.txt:668
                  0.0, 3.0, 2.5, 2.3, 3.1, 2.6,   # crosstab_output.txt:671
                  0.0, 3.0, 3.0, 3.1, 4.4, 3.3,   # crosstab_output.txt:674
                  0.0, 3.0, 1.9, 3.1, 2.9, 2.6,   # crosstab_output.txt:677
                  0.0, 2.7, 4.4, 2.8, 3.6, 3.3,   # crosstab_output.txt:680
                  0.0, 2.7, 2.7, 3.1, 2.2, 2.6,   # crosstab_output.txt:683
                  0.0, 4.2, 3.0, 2.3, 3.1, 2.8,   # crosstab_output.txt:686
                  0.0, 4.9, 2.9, 3.7, 2.7, 3.2,   # crosstab_output.txt:689
                  0.0, 4.6, 3.0, 3.4, 3.2, 3.3,   # crosstab_output.txt:692
                  0.0, 4.2, 3.0, 4.0, 2.9, 3.3,   # crosstab_output.txt:695
                  0.0, 5.3, 3.6, 3.3, 1.9, 3.1,   # crosstab_output.txt:698
                  0.0, 4.6, 3.4, 3.9, 2.2, 3.2,   # crosstab_output.txt:701
                  0.0, 4.2, 4.2, 1.9, 2.6, 2.8,   # crosstab_output.txt:704
                  0.0, 2.7, 2.3, 3.6, 1.9, 2.5,   # crosstab_output.txt:707
                  0.0, 2.3, 2.5, 3.4, 2.7, 2.7,   # crosstab_output.txt:710
                  0.0, 3.4, 3.4, 3.9, 2.7, 3.2,   # crosstab_output.txt:713
                  0.0, 0.0, 1.5, 2.2, 4.4, 2.3,   # crosstab_output.txt:716
                  0.0, 0.0, 1.7, 2.6, 4.3, 2.4,   # crosstab_output.txt:719
                  0.0, 0.0, 1.9, 2.3, 2.7, 1.9,   # crosstab_output.txt:722
                  0.0, 0.0, 1.5, 2.3, 3.1, 1.9,   # crosstab_output.txt:725
                  0.0, 0.0, 2.1, 1.7, 2.4, 1.7,   # crosstab_output.txt:728
                  0.0, 0.0, 1.5, 2.0, 3.9, 2.1,   # crosstab_output.txt:731
                  0.0, 0.0, 1.7, 1.9, 2.4, 1.7,   # crosstab_output.txt:734
                  0.0, 0.0, 1.9, 2.3, 2.7, 1.9,   # crosstab_output.txt:737
                  0.0, 0.0, 1.7, 2.3, 2.7, 1.9,   # crosstab_output.txt:740
                  0.0, 0.0, 1.3, 1.9, 2.6, 1.6,   # crosstab_output.txt:743
                  0.0, 0.0, 0.0, 2.0, 2.6, 1.3,   # crosstab_output.txt:746
                  0.0, 0.0, 0.8, 1.4, 2.4, 1.3,   # crosstab_output.txt:749
                  0.0, 0.0, 0.4, 1.6, 1.2, 0.9,   # crosstab_output.txt:752
                  0.0, 0.0, 1.1, 1.6, 2.9, 1.6,   # crosstab_output.txt:755
                  0.0, 0.0, 1.3, 1.4, 1.7, 1.2,   # crosstab_output.txt:758
                  0.0, 0.0, 1.5, 0.9, 1.0, 0.9,   # crosstab_output.txt:761
                  0.0, 0.0, 0.6, 2.3, 1.4, 1.2,   # crosstab_output.txt:764
                  0.0, 0.0, 0.8, 1.7, 1.2, 1.0,   # crosstab_output.txt:767
                  0.0, 0.0, 0.6, 0.9, 0.7, 0.6,   # crosstab_output.txt:770
                  0.0, 0.0, 0.8, 1.1, 1.7, 1.0,   # crosstab_output.txt:773
                  0.0, 0.0, 0.4, 1.6, 0.5, 0.7,   # crosstab_output.txt:776
                  0.0, 0.0, 0.0, 0.5, 0.2, 0.2,   # crosstab_output.txt:779
                  0.0, 0.0, 0.6, 0.2, 0.2, 0.2,   # crosstab_output.txt:782
                  0.0, 0.0, 0.2, 0.6, 0.9, 0.5,   # crosstab_output.txt:785
                  0.0, 0.0, 0.2, 0.6, 0.5, 0.4,   # crosstab_output.txt:788
                  0.0, 0.0, 0.2, 1.1, 0.7, 0.6,   # crosstab_output.txt:791
                  0.0, 0.0, 0.6, 0.5, 0.7, 0.5,   # crosstab_output.txt:794
                  0.0, 0.0, 0.6, 0.9, 0.2, 0.5,   # crosstab_output.txt:797
                  0.0, 0.0, 0.2, 0.3, 0.3, 0.2,   # crosstab_output.txt:800
                  0.0, 0.0, 0.0, 0.5, 0.5, 0.3,   # crosstab_output.txt:803
                  0.0, 0.0, 0.6, 0.0, 0.3, 0.2,   # crosstab_output.txt:806
                  0.0, 0.0, 0.8, 0.3, 0.5, 0.4,   # crosstab_output.txt:809
                  0.0, 0.0, 0.0, 0.5, 0.3, 0.2,   # crosstab_output.txt:812
                  0.0, 0.0, 0.4, 0.0, 0.2, 0.1,   # crosstab_output.txt:815
                  0.0, 0.0, 0.2, 0.5, 0.2, 0.2,   # crosstab_output.txt:818
                  0.0, 0.0, 0.4, 0.0, 0.5, 0.2,   # crosstab_output.txt:821
                  0.0, 0.0, 0.2, 0.2, 1.0, 0.4,   # crosstab_output.txt:824
                  0.0, 0.0, 0.2, 0.0, 0.0, 0.0,   # crosstab_output.txt:827
                  0.0, 0.0, 0.2, 0.5, 0.3, 0.3,   # crosstab_output.txt:830
                  0.0, 0.0, 0.2, 0.0, 0.3, 0.1,   # crosstab_output.txt:833
                  0.0, 0.0, 0.6, 2.3, 1.4, 1.2)   # crosstab_output.txt:836
    )
  ),

  # ---- Test 5.2: Political orientation x Life satisfaction ----
  cells_5_2 = list(
    all = list(
      count = c(16, 49, 80, 103, 102, 350,       # crosstab_output.txt:848
                31, 75, 126, 166, 150, 548,      # crosstab_output.txt:850
                40, 103, 201, 234, 223, 801,     # crosstab_output.txt:852
                18, 49, 109, 141, 105, 422,      # crosstab_output.txt:854
                6, 12, 33, 22, 34, 107,          # crosstab_output.txt:856
                111, 288, 549, 666, 614, 2228),  # crosstab_output.txt:858
      total_pct = c(0.7, 2.2, 3.6, 4.6, 4.6, 15.7,    # crosstab_output.txt:849
                    1.4, 3.4, 5.7, 7.5, 6.7, 24.6,    # crosstab_output.txt:851
                    1.8, 4.6, 9.0, 10.5, 10.0, 36.0,  # crosstab_output.txt:853
                    0.8, 2.2, 4.9, 6.3, 4.7, 18.9,    # crosstab_output.txt:855
                    0.3, 0.5, 1.5, 1.0, 1.5, 4.8,     # crosstab_output.txt:857
                    5.0, 12.9, 24.6, 29.9, 27.6)      # crosstab_output.txt:859
    )
  ),

  # ---- Test 6.1: Political orientation x Region BY Gender ----
  cells_6_1 = list(
    Male = list(
      count = c(30, 140, 170,     # crosstab_output.txt:870
                58, 219, 277,     # crosstab_output.txt:872
                81, 313, 394,     # crosstab_output.txt:874
                41, 171, 212,     # crosstab_output.txt:876
                6, 42, 48,        # crosstab_output.txt:878
                216, 885, 1101),  # crosstab_output.txt:880
      col_pct = c(13.9, 15.8, 15.4,  # crosstab_output.txt:871
                  26.9, 24.7, 25.2,  # crosstab_output.txt:873
                  37.5, 35.4, 35.8,  # crosstab_output.txt:875
                  19.0, 19.3, 19.3,  # crosstab_output.txt:877
                  2.8, 4.7, 4.4)     # crosstab_output.txt:879
    ),
    Female = list(
      count = c(42, 149, 191,     # crosstab_output.txt:882
                50, 245, 295,     # crosstab_output.txt:884
                80, 347, 427,     # crosstab_output.txt:886
                36, 188, 224,     # crosstab_output.txt:888
                19, 42, 61,       # crosstab_output.txt:890
                227, 971, 1198),  # crosstab_output.txt:892
      col_pct = c(18.5, 15.3, 15.9,  # crosstab_output.txt:883
                  22.0, 25.2, 24.6,  # crosstab_output.txt:885
                  35.2, 35.7, 35.6,  # crosstab_output.txt:887
                  15.9, 19.4, 18.7,  # crosstab_output.txt:889
                  8.4, 4.3, 5.1)     # crosstab_output.txt:891
    ),
    Total = list(
      count = c(72, 289, 361,      # crosstab_output.txt:894
                108, 464, 572,     # crosstab_output.txt:896
                161, 660, 821,     # crosstab_output.txt:898
                77, 359, 436,      # crosstab_output.txt:900
                25, 84, 109,       # crosstab_output.txt:902
                443, 1856, 2299),  # crosstab_output.txt:904
      col_pct = c(16.3, 15.6, 15.7,  # crosstab_output.txt:895
                  24.4, 25.0, 24.9,  # crosstab_output.txt:897
                  36.3, 35.6, 35.7,  # crosstab_output.txt:899
                  17.4, 19.3, 19.0,  # crosstab_output.txt:901
                  5.6, 4.5, 4.7)     # crosstab_output.txt:903
    )
  ),

  # ---- Test 6.2: Political orientation x Gender BY Region ----
  cells_6_2 = list(
    East = list(
      count = c(30, 42, 72,      # crosstab_output.txt:914
                58, 50, 108,     # crosstab_output.txt:916
                81, 80, 161,     # crosstab_output.txt:918
                41, 36, 77,      # crosstab_output.txt:920
                6, 19, 25,       # crosstab_output.txt:922
                216, 227, 443),  # crosstab_output.txt:924
      col_pct = c(13.9, 18.5, 16.3,  # crosstab_output.txt:915
                  26.9, 22.0, 24.4,  # crosstab_output.txt:917
                  37.5, 35.2, 36.3,  # crosstab_output.txt:919
                  19.0, 15.9, 17.4,  # crosstab_output.txt:921
                  2.8, 8.4, 5.6)     # crosstab_output.txt:923
    ),
    West = list(
      count = c(140, 149, 289,    # crosstab_output.txt:926
                219, 245, 464,    # crosstab_output.txt:928
                313, 347, 660,    # crosstab_output.txt:930
                171, 188, 359,    # crosstab_output.txt:932
                42, 42, 84,       # crosstab_output.txt:934
                885, 971, 1856),  # crosstab_output.txt:936
      col_pct = c(15.8, 15.3, 15.6,  # crosstab_output.txt:927
                  24.7, 25.2, 25.0,  # crosstab_output.txt:929
                  35.4, 35.7, 35.6,  # crosstab_output.txt:931
                  19.3, 19.4, 19.3,  # crosstab_output.txt:933
                  4.7, 4.3, 4.5)     # crosstab_output.txt:935
    ),
    Total = list(
      count = c(170, 191, 361,      # crosstab_output.txt:938
                277, 295, 572,      # crosstab_output.txt:940
                394, 427, 821,      # crosstab_output.txt:942
                212, 224, 436,      # crosstab_output.txt:944
                48, 61, 109,        # crosstab_output.txt:946
                1101, 1198, 2299),  # crosstab_output.txt:948
      col_pct = c(15.4, 15.9, 15.7,  # crosstab_output.txt:939
                  25.2, 24.6, 24.9,  # crosstab_output.txt:941
                  35.8, 35.6, 35.7,  # crosstab_output.txt:943
                  19.3, 18.7, 19.0,  # crosstab_output.txt:945
                  4.4, 5.1, 4.7)     # crosstab_output.txt:947
    )
  ),

  # ---- Test 6.3: Political orientation x Region BY Gender, weighted (Total layer 982-993 not comparable) ----
  cells_6_3 = list(
    Male = list(
      count = c(32, 138, 170,     # crosstab_output.txt:958
                60, 214, 274,     # crosstab_output.txt:960
                86, 310, 396,     # crosstab_output.txt:962
                43, 168, 211,     # crosstab_output.txt:964
                6, 43, 49,        # crosstab_output.txt:966
                227, 873, 1100),  # crosstab_output.txt:968
      col_pct = c(14.1, 15.8, 15.5,  # crosstab_output.txt:959
                  26.4, 24.5, 24.9,  # crosstab_output.txt:961
                  37.9, 35.5, 36.0,  # crosstab_output.txt:963
                  18.9, 19.2, 19.2,  # crosstab_output.txt:965
                  2.6, 4.9, 4.5)     # crosstab_output.txt:967
    ),
    Female = list(
      count = c(44, 150, 194,     # crosstab_output.txt:970
                53, 246, 299,     # crosstab_output.txt:972
                83, 349, 432,     # crosstab_output.txt:974
                38, 189, 227,     # crosstab_output.txt:976
                21, 42, 63,       # crosstab_output.txt:978
                239, 976, 1215),  # crosstab_output.txt:980
      col_pct = c(18.4, 15.4, 16.0,  # crosstab_output.txt:971
                  22.2, 25.2, 24.6,  # crosstab_output.txt:973
                  34.7, 35.8, 35.6,  # crosstab_output.txt:975
                  15.9, 19.4, 18.7,  # crosstab_output.txt:977
                  8.8, 4.3, 5.2)     # crosstab_output.txt:979
    )
  ),

  # ---- Test 6.4: Political orientation x Gender BY Region, weighted (output file ends at line 1018) ----
  cells_6_4 = list(
    East = list(
      count = c(32, 44, 76,      # crosstab_output.txt:1002
                60, 53, 113,     # crosstab_output.txt:1004
                86, 83, 169,     # crosstab_output.txt:1006
                43, 38, 81,      # crosstab_output.txt:1008
                6, 21, 27,       # crosstab_output.txt:1010
                227, 239, 466),  # crosstab_output.txt:1012
      col_pct = c(14.1, 18.4, 16.3,  # crosstab_output.txt:1003
                  26.4, 22.2, 24.2,  # crosstab_output.txt:1005
                  37.9, 34.7, 36.3,  # crosstab_output.txt:1007
                  18.9, 15.9, 17.4,  # crosstab_output.txt:1009
                  2.6, 8.8, 5.8)     # crosstab_output.txt:1011
    ),
    West = list(
      count = c(138, 150, 288,   # crosstab_output.txt:1014
                214, 246, 460,   # crosstab_output.txt:1016
                310, 349, 659),  # crosstab_output.txt:1018
      col_pct = c(15.8, 15.4, 15.6,  # crosstab_output.txt:1015
                  24.5, 25.2, 24.9)  # crosstab_output.txt:1017
    )
  )
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

# -----------------------------------------------------------------------------
# Every printed CROSSTABS cell, all six scenarios
# -----------------------------------------------------------------------------

# crosstab() with all three percentage types (optionally weighted)
xt_all <- function(data, row, col, weighted = FALSE) {
  if (weighted) {
    crosstab(data, {{ row }}, {{ col }}, weights = sampling_weight,
             percentages = "all")
  } else {
    crosstab(data, {{ row }}, {{ col }}, percentages = "all")
  }
}

test_that("Test 1.1: gender × region — all CROSSTABS cells match SPSS", {
  assert_xt_cells(xt_all(survey_data, gender, region),
                  spss_values$cells_1_1$all, "[1.1]")
})

test_that("Test 1.2: education × employment — all CROSSTABS cells match SPSS", {
  assert_xt_cells(xt_all(survey_data, education, employment),
                  spss_values$cells_1_2$all, "[1.2]")
})

test_that("Test 1.3: life_satisfaction × gender BY region — layers match SPSS", {
  sp <- spss_values$cells_1_3
  g <- survey_data |> group_by(region) |> xt_all(life_satisfaction, gender)
  for (rg in c("East", "West")) {
    assert_xt_cells(xt_layer(g, region = rg), sp[[rg]], sprintf("[1.3 %s]", rg))
  }
  assert_xt_cells(xt_all(survey_data, life_satisfaction, gender), sp$Total,
                  "[1.3 Total]")
})

test_that("Test 1.4: life_satisfaction × region BY gender — layers match SPSS", {
  sp <- spss_values$cells_1_4
  g <- survey_data |> group_by(gender) |> xt_all(life_satisfaction, region)
  for (gd in c("Male", "Female")) {
    assert_xt_cells(xt_layer(g, gender = gd), sp[[gd]], sprintf("[1.4 %s]", gd))
  }
  assert_xt_cells(xt_all(survey_data, life_satisfaction, region), sp$Total,
                  "[1.4 Total]")
})

test_that("Test 2.1: gender × region weighted — all CROSSTABS cells match SPSS", {
  assert_xt_cells(xt_all(survey_data, gender, region, weighted = TRUE),
                  spss_values$cells_2_1$all, "[2.1]")
})

test_that("Test 2.2: education × employment weighted — all CROSSTABS cells match SPSS", {
  assert_xt_cells(xt_all(survey_data, education, employment, weighted = TRUE),
                  spss_values$cells_2_2$all, "[2.2]")
})

test_that("Test 2.3: life_satisfaction × gender BY region weighted — layers match SPSS", {
  sp <- spss_values$cells_2_3
  g <- survey_data |> group_by(region) |>
    xt_all(life_satisfaction, gender, weighted = TRUE)
  for (rg in c("East", "West")) {
    assert_xt_cells(xt_layer(g, region = rg), sp[[rg]], sprintf("[2.3 %s]", rg))
  }
})

test_that("Test 2.4: life_satisfaction × region BY gender weighted — layers match SPSS", {
  sp <- spss_values$cells_2_4
  g <- survey_data |> group_by(gender) |>
    xt_all(life_satisfaction, region, weighted = TRUE)
  for (gd in c("Male", "Female")) {
    assert_xt_cells(xt_layer(g, gender = gd), sp[[gd]], sprintf("[2.4 %s]", gd))
  }
})

test_that("Test 3.1: crosstab gender × education grouped by region — matches SPSS", {
  sp <- spss_values$cells_3_1
  g <- survey_data |> group_by(region) |> xt_all(gender, education)
  expect_equal(length(g$results), 2L)
  for (rg in c("East", "West")) {
    assert_xt_cells(xt_layer(g, region = rg), sp[[rg]], sprintf("[3.1 %s]", rg))
  }
})

test_that("Test 3.2: life_satisfaction × gender grouped by region — matches SPSS", {
  sp <- spss_values$cells_3_2
  g <- survey_data |> group_by(region) |> xt_all(life_satisfaction, gender)
  for (rg in c("East", "West")) {
    assert_xt_cells(xt_layer(g, region = rg), sp[[rg]], sprintf("[3.2 %s]", rg))
  }
})

test_that("Test 3.3: education × employment BY gender, grouped by region — matches SPSS", {
  sp <- spss_values$cells_3_3
  g  <- survey_data |> group_by(region, gender) |> xt_all(education, employment)
  gt <- survey_data |> group_by(region) |> xt_all(education, employment)
  for (rg in c("East", "West")) {
    for (gd in c("Male", "Female")) {
      assert_xt_cells(xt_layer(g, region = rg, gender = gd),
                      sp[[paste(rg, gd, sep = "_")]], sprintf("[3.3 %s %s]", rg, gd))
    }
    assert_xt_cells(xt_layer(gt, region = rg), sp[[paste(rg, "Total", sep = "_")]],
                    sprintf("[3.3 %s Total]", rg))
  }
})

test_that("Test 4.1: gender × education weighted, grouped by region — all cells match SPSS", {
  sp <- spss_values$cells_4_1
  g <- survey_data |> group_by(region) |> xt_all(gender, education, weighted = TRUE)
  for (rg in c("East", "West")) {
    assert_xt_cells(xt_layer(g, region = rg), sp[[rg]], sprintf("[4.1 %s]", rg))
  }
})

test_that("Test 4.2: life_satisfaction × gender weighted, grouped by region — matches SPSS", {
  sp <- spss_values$cells_4_2
  g <- survey_data |> group_by(region) |>
    xt_all(life_satisfaction, gender, weighted = TRUE)
  for (rg in c("East", "West")) {
    assert_xt_cells(xt_layer(g, region = rg), sp[[rg]], sprintf("[4.2 %s]", rg))
  }
})

test_that("Test 4.3: education × employment BY gender weighted, grouped by region — matches SPSS", {
  sp <- spss_values$cells_4_3
  g <- survey_data |> group_by(region, gender) |>
    xt_all(education, employment, weighted = TRUE)
  for (rg in c("East", "West")) {
    for (gd in c("Male", "Female")) {
      assert_xt_cells(xt_layer(g, region = rg, gender = gd),
                      sp[[paste(rg, gd, sep = "_")]], sprintf("[4.3 %s %s]", rg, gd))
    }
  }
})

test_that("Test 5.1: income × life_satisfaction (missing values, 73 rows) — matches SPSS", {
  # Cases missing on either variable are excluded (N = 2115); income codes
  # are ordered numerically as in SPSS
  assert_xt_cells(xt_all(survey_data, income, life_satisfaction),
                  spss_values$cells_5_1$all, "[5.1]")
})

test_that("Test 5.2: political_orientation × life_satisfaction — matches SPSS", {
  assert_xt_cells(xt_all(survey_data, political_orientation, life_satisfaction),
                  spss_values$cells_5_2$all, "[5.2]")
})

test_that("Test 6.1: political_orientation × region BY gender — layers match SPSS", {
  sp <- spss_values$cells_6_1
  g <- survey_data |> group_by(gender) |> xt_all(political_orientation, region)
  for (gd in c("Male", "Female")) {
    assert_xt_cells(xt_layer(g, gender = gd), sp[[gd]], sprintf("[6.1 %s]", gd))
  }
  assert_xt_cells(xt_all(survey_data, political_orientation, region), sp$Total,
                  "[6.1 Total]")
})

test_that("Test 6.2: political_orientation × gender BY region — layers match SPSS", {
  sp <- spss_values$cells_6_2
  g <- survey_data |> group_by(region) |> xt_all(political_orientation, gender)
  for (rg in c("East", "West")) {
    assert_xt_cells(xt_layer(g, region = rg), sp[[rg]], sprintf("[6.2 %s]", rg))
  }
  assert_xt_cells(xt_all(survey_data, political_orientation, gender), sp$Total,
                  "[6.2 Total]")
})

test_that("Test 6.3: political_orientation × region BY gender weighted — layers match SPSS", {
  sp <- spss_values$cells_6_3
  g <- survey_data |> group_by(gender) |>
    xt_all(political_orientation, region, weighted = TRUE)
  for (gd in c("Male", "Female")) {
    assert_xt_cells(xt_layer(g, gender = gd), sp[[gd]], sprintf("[6.3 %s]", gd))
  }
})

test_that("Test 6.4: political_orientation × gender BY region weighted — layers match SPSS", {
  sp <- spss_values$cells_6_4
  g <- survey_data |> group_by(region) |>
    xt_all(political_orientation, gender, weighted = TRUE)
  assert_xt_cells(xt_layer(g, region = "East"), sp$East, "[6.4 East]")
  # The SPSS output file ends after the West layer's third row
  assert_xt_cells(xt_layer(g, region = "West"), sp$West, "[6.4 West]",
                  truncated = TRUE)
})


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
