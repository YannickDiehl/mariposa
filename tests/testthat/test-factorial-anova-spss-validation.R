# =============================================================================
# factorial_anova — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::factorial_anova() against SPSS v29 UNIANOVA
#          (Type III SS).
# Reference: factorial_anova_output.txt
#
# Coverage:
#   unweighted 1a-1c, 3a, 3b — Between-Subjects Factors N, cell descriptives,
#                              Levene (mean, median), Type III table, R^2
#   weighted   2a, 2b, 4a, 4b — cell descriptives (/REGWGT)
#   Not asserted: the weighted Type III / Levene tables (pending the SPSS
#   WEIGHT BY run). Grouping: UNIANOVA was not run with SPLIT FILE.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


# SPSS Test 1a: life_satisfaction by gender × region
spss_values <- list(
  test_1a_life_gender_region = list(
    rows = list(
      "Corrected Model"  = list(ss = 3.311,    df = 3L,    ms = 1.104,    f = 0.830,    p = 0.477, eta2 = 0.001),    # factorial_anova_output.txt:61
      "Intercept"        = list(ss = 19718.320, df = 1L,   ms = 19718.320, f = 14828.083, p = "<.001", eta2 = 0.860),
      "gender"           = list(ss = 0.006,    df = 1L,    ms = 0.006,    f = 0.005,    p = 0.946, eta2 = 0.000),
      "region"           = list(ss = 0.025,    df = 1L,    ms = 0.025,    f = 0.019,    p = 0.891, eta2 = 0.000),
      "gender * region" = list(ss = 1.893,    df = 1L,    ms = 1.893,    f = 1.424,    p = 0.233, eta2 = 0.001),
      "Error"            = list(ss = 3214.116,  df = 2417L, ms = 1.330),
      "Total"            = list(ss = 35088.000, df = 2421L),
      "Corrected Total"  = list(ss = 3217.428,  df = 2420L)
    )
  )
)


data(survey_data, envir = environment())


test_that("Test 1a: factorial life_sat ~ gender * region — matches SPSS", {
  r <- factorial_anova(survey_data, dv = life_satisfaction,
                        between = c(gender, region))
  spss <- spss_values$test_1a_life_gender_region
  at <- r$anova_table

  for (source_name in names(spss$rows)) {
    expected <- spss$rows[[source_name]]
    row <- at[at$source == source_name, , drop = FALSE]
    if (nrow(row) != 1L) {
      stop(sprintf("source '%s' not found in mariposa anova_table", source_name),
           call. = FALSE)
    }
    if (!is.null(expected$ss)) {
      assert_spss(as.numeric(row$ss), expected$ss,
                  tier = "display", precision = 3,
                  label = sprintf("[%s] SS", source_name))
    }
    if (!is.null(expected$df)) {
      assert_spss_count(as.numeric(row$df), expected$df,
                        label = sprintf("[%s] df", source_name))
    }
    if (!is.null(expected$ms)) {
      assert_spss(as.numeric(row$ms), expected$ms,
                  tier = "display", precision = 3,
                  label = sprintf("[%s] MS", source_name))
    }
    if (!is.null(expected$f)) {
      assert_spss(as.numeric(row$f), expected$f,
                  tier = "display", precision = 3,
                  label = sprintf("[%s] F", source_name))
    }
    if (!is.null(expected$p)) {
      assert_spss(as.numeric(row$p), expected$p,
                  tier = "display", precision = 3, what = "p_value",
                  label = sprintf("[%s] p", source_name))
    }
    if (!is.null(expected$eta2)) {
      assert_spss(as.numeric(row$partial_eta_sq), expected$eta2,
                  tier = "display", precision = 3,
                  label = sprintf("[%s] partial eta²", source_name))
    }
  }
})


# =============================================================================
# Weighted cell descriptives (/REGWGT reference, 0.7.4 audit)
# =============================================================================
# The weighted references use UNIANOVA /REGWGT: weighted cell means, SD with
# the weighted sum of squares over n - 1, N = number of cases.

spss_values$test_2a_cells <- list(
  list(gender = "Male",   region = "East", mean = 3.66, sd = 1.238, n = 228L),   # factorial_anova_output.txt:255
  list(gender = "Male",   region = "West", mean = 3.58, sd = 1.147, n = 921L),   # factorial_anova_output.txt:256
  list(gender = "Female", region = "East", mean = 3.59, sd = 1.230, n = 237L),   # factorial_anova_output.txt:258
  list(gender = "Female", region = "West", mean = 3.66, sd = 1.128, n = 1035L)   # factorial_anova_output.txt:259
)

test_that("Test 2a: weighted cell descriptives — match SPSS /REGWGT", {
  r <- factorial_anova(survey_data, dv = life_satisfaction,
                       between = c(gender, region), weights = sampling_weight)
  d <- r$descriptives
  for (cell in spss_values$test_2a_cells) {
    row <- d[d$gender == cell$gender & d$region == cell$region, , drop = FALSE]
    lab <- sprintf("[2a %s/%s]", cell$gender, cell$region)
    assert_spss(as.numeric(row$mean), cell$mean, tier = "display",
                precision = 2, label = paste(lab, "mean"))
    assert_spss(as.numeric(row$sd), cell$sd, tier = "display",
                precision = 3, label = paste(lab, "SD"))
    assert_spss_count(as.numeric(row$n), cell$n, label = paste(lab, "N"))
  }
})


# =============================================================================
# Unweighted scenarios: every table SPSS prints (0.7.4 assertion audit)
# =============================================================================
# Between-Subjects Factors N, the cell Descriptive Statistics, Levene (based
# on mean and on median), the Type III table and R squared of Tests 1a-1c,
# 3a, 3b. Each value is cached as the string SPSS printed, so the Display
# precision is the number of printed decimals (Charter §4) and a printed
# ".000" p-value is the Spec sentinel "<.001". SPSS's marginal "Total" rows
# of Descriptive Statistics are not part of mariposa's cell table.

spss_values$unweighted <- list(
  # ---- Test 1a: UNIANOVA life_satisfaction BY gender region
  "1a" = list(
    dv = "life_satisfaction", between = c("gender", "region"),
    # Between-Subjects Factors: factor, level, N
    bsf = list(
      c("gender", "Male", "1149"),  # factorial_anova_output.txt:27
      c("gender", "Female", "1272"),  # factorial_anova_output.txt:28
      c("region", "East", "465"),  # factorial_anova_output.txt:29
      c("region", "West", "1956")   # factorial_anova_output.txt:30
    ),
    # Descriptive Statistics: gender, region, Mean, Std. Deviation, N
    cells = list(
      c("Male", "East", "3.65", "1.209", "228"),  # factorial_anova_output.txt:36
      c("Male", "West", "3.59", "1.154", "921"),  # factorial_anova_output.txt:37
      c("Female", "East", "3.59", "1.206", "237"),  # factorial_anova_output.txt:39
      c("Female", "West", "3.67", "1.127", "1035")   # factorial_anova_output.txt:40
    ),
    levene_mean   = c("1.586", "3", "2417", ".191"),  # factorial_anova_output.txt:49
    levene_median = c("1.405", "3", "2417", ".240"),  # factorial_anova_output.txt:50
    # (Type III table: asserted by the Test 1a block above)
    r2 = c(".001", ".000")  # factorial_anova_output.txt:69
  ),

  # ---- Test 1b: UNIANOVA income BY gender education
  "1b" = list(
    dv = "income", between = c("gender", "education"),
    # Between-Subjects Factors: factor, level, N
    bsf = list(
      c("gender", "Male", "1046"),  # factorial_anova_output.txt:97
      c("gender", "Female", "1140"),  # factorial_anova_output.txt:98
      c("education", "Basic Secondary", "735"),  # factorial_anova_output.txt:99
      c("education", "Intermediate Secondary", "548"),  # factorial_anova_output.txt:100
      c("education", "Academic Secondary", "548"),  # factorial_anova_output.txt:101
      c("education", "University", "355")   # factorial_anova_output.txt:102
    ),
    # Descriptive Statistics: gender, education, Mean, Std. Deviation, N
    cells = list(
      c("Male", "Basic Secondary", "2803.4286", "774.95889", "350"),  # factorial_anova_output.txt:108
      c("Male", "Intermediate Secondary", "3574.0891", "996.64920", "247"),  # factorial_anova_output.txt:109
      c("Male", "Academic Secondary", "4246.4539", "1180.77942", "282"),  # factorial_anova_output.txt:110
      c("Male", "University", "5318.5629", "1718.80537", "167"),  # factorial_anova_output.txt:111
      c("Female", "Basic Secondary", "2718.7013", "795.83118", "385"),  # factorial_anova_output.txt:113
      c("Female", "Intermediate Secondary", "3607.6412", "996.21354", "301"),  # factorial_anova_output.txt:114
      c("Female", "Academic Secondary", "4200.3759", "1178.11804", "266"),  # factorial_anova_output.txt:115
      c("Female", "University", "5353.7234", "1612.26481", "188")   # factorial_anova_output.txt:116
    ),
    levene_mean   = c("44.988", "7", "2178", ".000"),  # factorial_anova_output.txt:127
    levene_median = c("44.534", "7", "2178", ".000"),  # factorial_anova_output.txt:128
    # Tests of Between-Subjects Effects: source, SS, df, MS, F, Sig., partial eta^2
    effects = list(
      c("Corrected Model", "1754652069.848", "7", "250664581.407", "199.909", ".000", ".391"),  # factorial_anova_output.txt:139
      c("Intercept", "32212607064.511", "1", "32212607064.511", "25690.075", ".000", ".922"),  # factorial_anova_output.txt:140
      c("gender", "122637.601", "1", "122637.601", ".098", ".755", ".000"),  # factorial_anova_output.txt:141
      c("education", "1743617622.524", "3", "581205874.175", "463.521", ".000", ".390"),  # factorial_anova_output.txt:142
      c("gender * education", "1499440.759", "3", "499813.586", ".399", ".754", ".001"),  # factorial_anova_output.txt:143
      c("Error", "2730979096.667", "2178", "1253893.066"),  # factorial_anova_output.txt:144
      c("Total", "35290790000.000", "2186"),  # factorial_anova_output.txt:145
      c("Corrected Total", "4485631166.515", "2185")   # factorial_anova_output.txt:146
    ),
    r2 = c(".391", ".389")  # factorial_anova_output.txt:147
  ),

  # ---- Test 1c: UNIANOVA trust_government BY gender region
  "1c" = list(
    dv = "trust_government", between = c("gender", "region"),
    # Between-Subjects Factors: factor, level, N
    bsf = list(
      c("gender", "Male", "1123"),  # factorial_anova_output.txt:175
      c("gender", "Female", "1231"),  # factorial_anova_output.txt:176
      c("region", "East", "460"),  # factorial_anova_output.txt:177
      c("region", "West", "1894")   # factorial_anova_output.txt:178
    ),
    # Descriptive Statistics: gender, region, Mean, Std. Deviation, N
    cells = list(
      c("Male", "East", "2.57", "1.172", "223"),  # factorial_anova_output.txt:184
      c("Male", "West", "2.61", "1.189", "900"),  # factorial_anova_output.txt:185
      c("Female", "East", "2.65", "1.168", "237"),  # factorial_anova_output.txt:187
      c("Female", "West", "2.63", "1.138", "994")   # factorial_anova_output.txt:188
    ),
    levene_mean   = c("1.241", "3", "2350", ".293"),  # factorial_anova_output.txt:197
    levene_median = c(".773", "3", "2350", ".509"),  # factorial_anova_output.txt:198
    # Tests of Between-Subjects Effects: source, SS, df, MS, F, Sig., partial eta^2
    effects = list(
      c("Corrected Model", "1.086", "3", ".362", ".267", ".849", ".000"),  # factorial_anova_output.txt:209
      c("Intercept", "10112.993", "1", "10112.993", "7466.042", ".000", ".761"),  # factorial_anova_output.txt:210
      c("gender", "1.004", "1", "1.004", ".741", ".389", ".000"),  # factorial_anova_output.txt:211
      c("region", ".091", "1", ".091", ".067", ".796", ".000"),  # factorial_anova_output.txt:212
      c("gender * region", ".394", "1", ".394", ".291", ".590", ".000"),  # factorial_anova_output.txt:213
      c("Error", "3183.150", "2350", "1.355"),  # factorial_anova_output.txt:214
      c("Total", "19351.000", "2354"),  # factorial_anova_output.txt:215
      c("Corrected Total", "3184.237", "2353")   # factorial_anova_output.txt:216
    ),
    r2 = c(".000", "-.001")  # factorial_anova_output.txt:217
  ),

  # ---- Test 3a: UNIANOVA life_satisfaction BY gender region education
  "3a" = list(
    dv = "life_satisfaction", between = c("gender", "region", "education"),
    # Between-Subjects Factors: factor, level, N
    bsf = list(
      c("gender", "Male", "1149"),  # factorial_anova_output.txt:395
      c("gender", "Female", "1272"),  # factorial_anova_output.txt:396
      c("region", "East", "465"),  # factorial_anova_output.txt:397
      c("region", "West", "1956"),  # factorial_anova_output.txt:398
      c("education", "Basic Secondary", "809"),  # factorial_anova_output.txt:399
      c("education", "Intermediate Secondary", "618"),  # factorial_anova_output.txt:400
      c("education", "Academic Secondary", "607"),  # factorial_anova_output.txt:401
      c("education", "University", "387")   # factorial_anova_output.txt:402
    ),
    # Descriptive Statistics: gender, region, education, Mean, Std. Deviation, N
    cells = list(
      c("Male", "East", "Basic Secondary", "3.32", "1.329", "76"),  # factorial_anova_output.txt:408
      c("Male", "East", "Intermediate Secondary", "3.66", "1.183", "59"),  # factorial_anova_output.txt:409
      c("Male", "East", "Academic Secondary", "3.86", "1.135", "56"),  # factorial_anova_output.txt:410
      c("Male", "East", "University", "4.03", ".928", "37"),  # factorial_anova_output.txt:411
      c("Male", "West", "Basic Secondary", "3.17", "1.212", "306"),  # factorial_anova_output.txt:413
      c("Male", "West", "Intermediate Secondary", "3.64", "1.135", "222"),  # factorial_anova_output.txt:414
      c("Male", "West", "Academic Secondary", "3.86", ".981", "249"),  # factorial_anova_output.txt:415
      c("Male", "West", "University", "3.94", "1.072", "144"),  # factorial_anova_output.txt:416
      c("Female", "East", "Basic Secondary", "3.29", "1.352", "85"),  # factorial_anova_output.txt:423
      c("Female", "East", "Intermediate Secondary", "3.62", "1.106", "60"),  # factorial_anova_output.txt:424
      c("Female", "East", "Academic Secondary", "3.81", "1.047", "54"),  # factorial_anova_output.txt:425
      c("Female", "East", "University", "3.87", "1.119", "38"),  # factorial_anova_output.txt:426
      c("Female", "West", "Basic Secondary", "3.18", "1.227", "342"),  # factorial_anova_output.txt:428
      c("Female", "West", "Intermediate Secondary", "3.77", "1.081", "277"),  # factorial_anova_output.txt:429
      c("Female", "West", "Academic Secondary", "3.86", ".978", "248"),  # factorial_anova_output.txt:430
      c("Female", "West", "University", "4.18", ".794", "168")   # factorial_anova_output.txt:431
    ),
    levene_mean   = c("7.061", "15", "2405", ".000"),  # factorial_anova_output.txt:457
    levene_median = c("5.560", "15", "2405", ".000"),  # factorial_anova_output.txt:458
    # Tests of Between-Subjects Effects: source, SS, df, MS, F, Sig., partial eta^2
    effects = list(
      c("Corrected Model", "258.289", "15", "17.219", "13.995", ".000", ".080"),  # factorial_anova_output.txt:469
      c("Intercept", "19011.236", "1", "19011.236", "15451.124", ".000", ".865"),  # factorial_anova_output.txt:470
      c("gender", ".082", "1", ".082", ".067", ".796", ".000"),  # factorial_anova_output.txt:471
      c("region", ".132", "1", ".132", ".107", ".744", ".000"),  # factorial_anova_output.txt:472
      c("education", "128.816", "3", "42.939", "34.898", ".000", ".042"),  # factorial_anova_output.txt:473
      c("gender * region", "2.351", "1", "2.351", "1.911", ".167", ".001"),  # factorial_anova_output.txt:474
      c("gender * education", ".277", "3", ".092", ".075", ".973", ".000"),  # factorial_anova_output.txt:475
      c("region * education", "3.326", "3", "1.109", ".901", ".440", ".001"),  # factorial_anova_output.txt:476
      c("gender * region * education", "1.655", "3", ".552", ".448", ".719", ".001"),  # factorial_anova_output.txt:477
      c("Error", "2959.139", "2405", "1.230"),  # factorial_anova_output.txt:478
      c("Total", "35088.000", "2421"),  # factorial_anova_output.txt:479
      c("Corrected Total", "3217.428", "2420")   # factorial_anova_output.txt:480
    ),
    r2 = c(".080", ".075")  # factorial_anova_output.txt:481
  ),

  # ---- Test 3b: UNIANOVA income BY gender region education
  "3b" = list(
    dv = "income", between = c("gender", "region", "education"),
    # Between-Subjects Factors: factor, level, N
    bsf = list(
      c("gender", "Male", "1046"),  # factorial_anova_output.txt:509
      c("gender", "Female", "1140"),  # factorial_anova_output.txt:510
      c("region", "East", "429"),  # factorial_anova_output.txt:511
      c("region", "West", "1757"),  # factorial_anova_output.txt:512
      c("education", "Basic Secondary", "735"),  # factorial_anova_output.txt:513
      c("education", "Intermediate Secondary", "548"),  # factorial_anova_output.txt:514
      c("education", "Academic Secondary", "548"),  # factorial_anova_output.txt:515
      c("education", "University", "355")   # factorial_anova_output.txt:516
    ),
    # Descriptive Statistics: gender, region, education, Mean, Std. Deviation, N
    cells = list(
      c("Male", "East", "Basic Secondary", "2839.7260", "806.69761", "73"),  # factorial_anova_output.txt:522
      c("Male", "East", "Intermediate Secondary", "3739.2157", "1081.86477", "51"),  # factorial_anova_output.txt:523
      c("Male", "East", "Academic Secondary", "4280.0000", "1307.90376", "50"),  # factorial_anova_output.txt:524
      c("Male", "East", "University", "5612.1212", "1446.71127", "33"),  # factorial_anova_output.txt:525
      c("Male", "West", "Basic Secondary", "2793.8628", "767.59451", "277"),  # factorial_anova_output.txt:527
      c("Male", "West", "Intermediate Secondary", "3531.1224", "971.59703", "196"),  # factorial_anova_output.txt:528
      c("Male", "West", "Academic Secondary", "4239.2241", "1154.50006", "232"),  # factorial_anova_output.txt:529
      c("Male", "West", "University", "5246.2687", "1776.82054", "134"),  # factorial_anova_output.txt:530
      c("Female", "East", "Basic Secondary", "2949.3827", "761.43343", "81"),  # factorial_anova_output.txt:537
      c("Female", "East", "Intermediate Secondary", "3491.0714", "872.87962", "56"),  # factorial_anova_output.txt:538
      c("Female", "East", "Academic Secondary", "4116.6667", "1226.19171", "48"),  # factorial_anova_output.txt:539
      c("Female", "East", "University", "4881.0811", "1745.44452", "37"),  # factorial_anova_output.txt:540
      c("Female", "West", "Basic Secondary", "2657.2368", "794.71040", "304"),  # factorial_anova_output.txt:542
      c("Female", "West", "Intermediate Secondary", "3634.2857", "1022.07600", "245"),  # factorial_anova_output.txt:543
      c("Female", "West", "Academic Secondary", "4218.8073", "1169.37277", "218"),  # factorial_anova_output.txt:544
      c("Female", "West", "University", "5469.5364", "1562.30571", "151")   # factorial_anova_output.txt:545
    ),
    levene_mean   = c("21.493", "15", "2170", ".000"),  # factorial_anova_output.txt:571
    levene_median = c("21.150", "15", "2170", ".000"),  # factorial_anova_output.txt:572
    # Tests of Between-Subjects Effects: source, SS, df, MS, F, Sig., partial eta^2
    effects = list(
      c("Corrected Model", "1777233472.651", "15", "118482231.510", "94.929", ".000", ".396"),  # factorial_anova_output.txt:583
      c("Intercept", "20213547861.158", "1", "20213547861.158", "16195.332", ".000", ".882"),  # factorial_anova_output.txt:584
      c("gender", "3714208.739", "1", "3714208.739", "2.976", ".085", ".001"),  # factorial_anova_output.txt:585
      c("region", "70450.166", "1", "70450.166", ".056", ".812", ".000"),  # factorial_anova_output.txt:586
      c("education", "1045824300.441", "3", "348608100.147", "279.309", ".000", ".279"),  # factorial_anova_output.txt:587
      c("gender * region", "7200424.027", "1", "7200424.027", "5.769", ".016", ".003"),  # factorial_anova_output.txt:588
      c("gender * education", "2235577.666", "3", "745192.555", ".597", ".617", ".001"),  # factorial_anova_output.txt:589
      c("region * education", "3707480.291", "3", "1235826.764", ".990", ".396", ".001"),  # factorial_anova_output.txt:590
      c("gender * region * education", "14559992.317", "3", "4853330.772", "3.889", ".009", ".005"),  # factorial_anova_output.txt:591
      c("Error", "2708397693.864", "2170", "1248109.536"),  # factorial_anova_output.txt:592
      c("Total", "35290790000.000", "2186"),  # factorial_anova_output.txt:593
      c("Corrected Total", "4485631166.515", "2185")   # factorial_anova_output.txt:594
    ),
    r2 = c(".396", ".392")  # factorial_anova_output.txt:595
  )
)

# Weighted (/REGWGT) cell descriptives of Tests 2b, 4a, 4b (2a: see above):
# weighted mean, SD with the weighted sum of squares over n - 1, N = number
# of cases. The weighted Type III / Levene tables are pending the SPSS run
# with WEIGHT BY and are deliberately not asserted.

spss_values$weighted_cells <- list(
  # ---- Test 2b: UNIANOVA income BY gender education /REGWGT
  "2b" = list(
    dv = "income", between = c("gender", "education"),
    # Descriptive Statistics: gender, education, Mean, Std. Deviation, N
    cells = list(
      c("Male", "Basic Secondary", "2801.2376", "781.15116", "350"),  # factorial_anova_output.txt:328
      c("Male", "Intermediate Secondary", "3579.2969", "1000.71738", "247"),  # factorial_anova_output.txt:329
      c("Male", "Academic Secondary", "4244.2258", "1196.72201", "282"),  # factorial_anova_output.txt:330
      c("Male", "University", "5314.4098", "1694.94091", "167"),  # factorial_anova_output.txt:331
      c("Female", "Basic Secondary", "2721.4740", "799.03345", "385"),  # factorial_anova_output.txt:333
      c("Female", "Intermediate Secondary", "3598.9657", "1007.90453", "301"),  # factorial_anova_output.txt:334
      c("Female", "Academic Secondary", "4205.1651", "1186.90520", "266"),  # factorial_anova_output.txt:335
      c("Female", "University", "5346.1764", "1587.16986", "188")   # factorial_anova_output.txt:336
    )
  ),

  # ---- Test 4a: UNIANOVA life_satisfaction BY gender region education /REGWGT
  "4a" = list(
    dv = "life_satisfaction", between = c("gender", "region", "education"),
    # Descriptive Statistics: gender, region, education, Mean, Std. Deviation, N
    cells = list(
      c("Male", "East", "Basic Secondary", "3.31", "1.338", "76"),  # factorial_anova_output.txt:637
      c("Male", "East", "Intermediate Secondary", "3.65", "1.211", "59"),  # factorial_anova_output.txt:638
      c("Male", "East", "Academic Secondary", "3.85", "1.198", "56"),  # factorial_anova_output.txt:639
      c("Male", "East", "University", "4.05", ".945", "37"),  # factorial_anova_output.txt:640
      c("Male", "West", "Basic Secondary", "3.17", "1.209", "306"),  # factorial_anova_output.txt:642
      c("Male", "West", "Intermediate Secondary", "3.64", "1.130", "222"),  # factorial_anova_output.txt:643
      c("Male", "West", "Academic Secondary", "3.86", ".979", "249"),  # factorial_anova_output.txt:644
      c("Male", "West", "University", "3.91", "1.048", "144"),  # factorial_anova_output.txt:645
      c("Female", "East", "Basic Secondary", "3.31", "1.373", "85"),  # factorial_anova_output.txt:652
      c("Female", "East", "Intermediate Secondary", "3.63", "1.139", "60"),  # factorial_anova_output.txt:653
      c("Female", "East", "Academic Secondary", "3.79", "1.085", "54"),  # factorial_anova_output.txt:654
      c("Female", "East", "University", "3.86", "1.137", "38"),  # factorial_anova_output.txt:655
      c("Female", "West", "Basic Secondary", "3.19", "1.233", "342"),  # factorial_anova_output.txt:657
      c("Female", "West", "Intermediate Secondary", "3.76", "1.090", "277"),  # factorial_anova_output.txt:658
      c("Female", "West", "Academic Secondary", "3.86", ".975", "248"),  # factorial_anova_output.txt:659
      c("Female", "West", "University", "4.19", ".776", "168")   # factorial_anova_output.txt:660
    )
  ),

  # ---- Test 4b: UNIANOVA income BY gender region education /REGWGT
  "4b" = list(
    dv = "income", between = c("gender", "region", "education"),
    # Descriptive Statistics: gender, region, education, Mean, Std. Deviation, N
    cells = list(
      c("Male", "East", "Basic Secondary", "2841.4813", "838.21595", "73"),  # factorial_anova_output.txt:752
      c("Male", "East", "Intermediate Secondary", "3766.1403", "1096.56791", "51"),  # factorial_anova_output.txt:753
      c("Male", "East", "Academic Secondary", "4263.8261", "1381.72209", "50"),  # factorial_anova_output.txt:754
      c("Male", "East", "University", "5639.1947", "1499.96082", "33"),  # factorial_anova_output.txt:755
      c("Male", "West", "Basic Secondary", "2790.3582", "766.64490", "277"),  # factorial_anova_output.txt:757
      c("Male", "West", "Intermediate Secondary", "3528.0193", "970.99181", "196"),  # factorial_anova_output.txt:758
      c("Male", "West", "Academic Secondary", "4239.6280", "1156.30727", "232"),  # factorial_anova_output.txt:759
      c("Male", "West", "University", "5224.1483", "1734.63325", "134"),  # factorial_anova_output.txt:760
      c("Female", "East", "Basic Secondary", "2951.7000", "766.00083", "81"),  # factorial_anova_output.txt:767
      c("Female", "East", "Intermediate Secondary", "3502.1003", "894.51614", "56"),  # factorial_anova_output.txt:768
      c("Female", "East", "Academic Secondary", "4110.5624", "1251.68972", "48"),  # factorial_anova_output.txt:769
      c("Female", "East", "University", "4849.7961", "1780.12712", "37"),  # factorial_anova_output.txt:770
      c("Female", "West", "Basic Secondary", "2658.6740", "797.24558", "304"),  # factorial_anova_output.txt:772
      c("Female", "West", "Intermediate Secondary", "3622.3236", "1032.36820", "245"),  # factorial_anova_output.txt:773
      c("Female", "West", "Academic Secondary", "4226.8185", "1174.08209", "218"),  # factorial_anova_output.txt:774
      c("Female", "West", "University", "5474.4390", "1517.33571", "151")   # factorial_anova_output.txt:775
    )
  )
)


# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

# Assert `actual` against the string SPSS printed: Display tier with the
# printed number of decimals; ".000" as a p-value is the "<.001" sentinel.
assert_printed <- function(actual, printed, label, p_value = FALSE) {
  if (p_value && identical(printed, ".000")) {
    return(assert_spss(actual, "<.001", tier = "display", precision = 3,
                       what = "p_value", label = label))
  }
  decimals <- if (grepl(".", printed, fixed = TRUE)) {
    nchar(sub("^[^.]*[.]", "", printed))
  } else {
    0L
  }
  assert_spss(actual, as.numeric(printed), tier = "display",
              precision = decimals, what = if (p_value) "p_value" else NULL,
              label = label)
}

# "Male/East"-style keys of the factor columns of a result table
cell_keys <- function(tab, factors) {
  do.call(paste, c(lapply(factors, function(f) as.character(tab[[f]])),
                   sep = "/"))
}

# Descriptive Statistics: the cells in SPSS order, Mean, SD, N
assert_cells <- function(d, s, id) {
  k <- length(s$between)
  keys <- vapply(s$cells, function(row) paste(row[seq_len(k)], collapse = "/"), "")
  expect_identical(cell_keys(d, s$between), keys,
                   label = sprintf("[%s] descriptive cells in SPSS order", id))
  for (i in seq_along(s$cells)) {
    row <- s$cells[[i]]
    lab <- sprintf("[%s %s]", id, keys[i])
    x <- d[cell_keys(d, s$between) == keys[i], , drop = FALSE]
    assert_printed(as.numeric(x$mean), row[k + 1], paste(lab, "Mean"))
    assert_printed(as.numeric(x$sd), row[k + 2], paste(lab, "Std. Deviation"))
    assert_spss_count(as.numeric(x$n), as.integer(row[k + 3]), paste(lab, "N"))
  }
}

# Between-Subjects Factors: N per factor level = sum of its cell Ns
assert_bsf <- function(d, s, id) {
  for (row in s$bsf) {
    assert_spss_count(sum(d$n[as.character(d[[row[1]]]) == row[2]]),
                      as.integer(row[3]),
                      sprintf("[%s] Between-Subjects N %s=%s", id, row[1], row[2]))
  }
}

# One Levene row: statistic, df1, df2, Sig.
assert_levene <- function(f, df1, df2, p, printed, lab) {
  assert_printed(f, printed[1], paste(lab, "statistic"))
  assert_spss_count(df1, as.integer(printed[2]), paste(lab, "df1"))
  assert_spss_count(df2, as.integer(printed[3]), paste(lab, "df2"))
  assert_printed(p, printed[4], paste(lab, "Sig."), p_value = TRUE)
}

# Tests of Between-Subjects Effects, sources in SPSS order
effect_cols <- c("ss", "df", "ms", "f", "p", "partial_eta_sq")
assert_effects <- function(at, s, id) {
  expect_identical(at$source, vapply(s$effects, `[`, "", 1L),
                   label = sprintf("[%s] sources in SPSS order", id))
  for (row in s$effects) {
    x <- at[at$source == row[1], , drop = FALSE]
    for (j in seq_along(row)[-1]) {
      col <- effect_cols[j - 1L]
      lab <- sprintf("[%s %s] %s", id, row[1], col)
      if (col == "df") {
        assert_spss_count(as.numeric(x$df), as.integer(row[j]), lab)
      } else {
        assert_printed(as.numeric(x[[col]]), row[j], lab, p_value = col == "p")
      }
    }
  }
}


for (id in names(spss_values$unweighted)) {
  test_that(sprintf("Test %s: unweighted UNIANOVA tables match SPSS", id), {
    s <- spss_values$unweighted[[id]]
    r <- factorial_anova(survey_data, dv = !!rlang::sym(s$dv),
                         between = !!s$between)

    assert_bsf(r$descriptives, s, id)
    assert_cells(r$descriptives, s, id)

    lev <- r$levene_test
    assert_levene(lev$f, lev$df1, lev$df2, lev$p, s$levene_mean,
                  sprintf("[%s] Levene based on mean", id))
    lt <- levene_test(r)$results
    assert_levene(lt$F_statistic, lt$df1, lt$df2, lt$p_value, s$levene_mean,
                  sprintf("[%s] levene_test(factorial_anova)", id))
    lmed <- levene_test(r, center = "median")$results
    assert_levene(lmed$F_statistic, lmed$df1, lmed$df2, lmed$p_value,
                  s$levene_median,
                  sprintf("[%s] Levene based on median", id))

    if (!is.null(s$effects)) assert_effects(r$anova_table, s, id)
    assert_printed(r$r_squared[["r_squared"]], s$r2[1],
                   sprintf("[%s] R Squared", id))
    assert_printed(r$r_squared[["adj_r_squared"]], s$r2[2],
                   sprintf("[%s] Adjusted R Squared", id))
  })
}

for (id in names(spss_values$weighted_cells)) {
  test_that(sprintf("Test %s: weighted cell descriptives match SPSS /REGWGT", id), {
    s <- spss_values$weighted_cells[[id]]
    r <- factorial_anova(survey_data, dv = !!rlang::sym(s$dv),
                         between = !!s$between, weights = sampling_weight)
    assert_cells(r$descriptives, s, id)
  })
}
