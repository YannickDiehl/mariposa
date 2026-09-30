# =============================================================================
# ancova — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::ancova() against SPSS v29 UNIANOVA with WITH.
# Reference: ancova_output.txt
#
# Coverage:
#   unweighted 1a-1c, 3a, 3b, 5a — Between-Subjects Factors N, cell
#     descriptives, Levene, Type III table, R^2, Parameter Estimates,
#     Estimated Marginal Means and their covariate values
#   weighted 2a, 2b, 4a, 4b, 6a — cell descriptives and Levene (/REGWGT)
#   Not asserted: the weighted Type III / parameter / EMM tables (pending the
#   SPSS WEIGHT BY run). Grouping: UNIANOVA was not run with SPLIT FILE.
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(
  # SPSS Test 1a: income ~ age + education (covariate = age, factor = education)
  test_1a_income_age_education = list(
    "Corrected Model" = list(ss = 1752821002.875, df = 4L, ms = 438205250.719, f = 349.723, p = "<.001", eta2 = 0.391),    # ancova_output.txt:55
    "Intercept"       = list(ss = 3472400779.184, df = 1L, ms = 3472400779.184, f = 2771.252, p = "<.001", eta2 = 0.560),  # ancova_output.txt:56
    "age"             = list(ss = 37721.406,      df = 1L, ms = 37721.406, f = 0.030, p = 0.862, eta2 = 0.000),            # ancova_output.txt:57
    "education"       = list(ss = 1752630664.966, df = 3L, ms = 584210221.656, f = 466.246, p = "<.001", eta2 = 0.391),    # ancova_output.txt:58
    "Error"           = list(ss = 2732810163.640, df = 2181L, ms = 1253007.870),   # ancova_output.txt:59
    "Total"           = list(ss = 35290790000.000, df = 2186L),                    # ancova_output.txt:60
    "Corrected Total" = list(ss = 4485631166.515, df = 2185L)                      # ancova_output.txt:61
  )
)


data(survey_data, envir = environment())


test_that("Test 1a: ancova income ~ age (covariate) + education — matches SPSS", {
  r <- ancova(survey_data, dv = income, between = education, covariate = age)
  spss <- spss_values$test_1a_income_age_education

  for (source_name in names(spss)) {
    expected <- spss[[source_name]]
    row <- r$anova_table[r$anova_table$source == source_name, , drop = FALSE]
    if (nrow(row) != 1L) {
      stop(sprintf("source '%s' not in ancova output", source_name), call. = FALSE)
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
# LEVENE'S TEST OF EQUALITY OF ERROR VARIANCES (unweighted scenarios)
# =============================================================================
# SPSS UNIANOVA tests the absolute residuals of the full model (covariates +
# factors) across the design cells. The weighted scenarios 2a, 2b, 4a, 4b,
# 6a were run with /REGWGT (the current weighting mode of ancova()): there
# SPSS tests sqrt(w) * |WLS residual|. They are re-referenced once the
# pending SPSS run with WEIGHT BY decides the weighting mode.
# p printed as ".000" by SPSS -> Spec sentinel "<.001" (Charter §4).

spss_levene <- list(
  # ancova_output.txt:47 (Test 1a: income BY education WITH age)
  t1a = list(dv = "income", between = "education", cov = "age",
             f = 103.953, df1 = 3L, df2 = 2182L, p = "<.001"),
  # ancova_output.txt:131 (Test 1b: life_satisfaction BY gender WITH age)
  t1b = list(dv = "life_satisfaction", between = "gender", cov = "age",
             f = 1.306, df1 = 1L, df2 = 2419L, p = 0.253),
  # ancova_output.txt:215 (Test 1c: life_satisfaction BY education WITH political_orientation)
  t1c = list(dv = "life_satisfaction", between = "education",
             cov = "political_orientation",
             f = 24.350, df1 = 3L, df2 = 2224L, p = "<.001"),
  # ancova_output.txt:495 (Test 3a: income BY gender education WITH age)
  t3a = list(dv = "income", between = c("gender", "education"), cov = "age",
             f = 45.006, df1 = 7L, df2 = 2178L, p = "<.001"),
  # ancova_output.txt:603 (Test 3b: life_satisfaction BY gender region WITH age)
  t3b = list(dv = "life_satisfaction", between = c("gender", "region"), cov = "age",
             f = 1.562, df1 = 3L, df2 = 2417L, p = 0.197),
  # ancova_output.txt:923 (Test 5a: income BY education WITH age political_orientation)
  t5a = list(dv = "income", between = "education",
             cov = c("age", "political_orientation"),
             f = 97.401, df1 = 3L, df2 = 2004L, p = "<.001")
)

test_that("Test 2a: weighted cell descriptives — match SPSS /REGWGT", {
  # /REGWGT: weighted mean, SD with the weighted sum of squares over n - 1,
  # N = number of cases (0.7.4 audit: the SD divided by sum(w))
  cells <- list(
    "Basic Secondary"        = list(mean = 2759.2606, sd = 791.04390,  n = 735L),  # ancova_output.txt:294
    "Intermediate Secondary" = list(mean = 3590.2177, sd = 1003.80253, n = 548L),  # ancova_output.txt:295
    "Academic Secondary"     = list(mean = 4225.3255, sd = 1191.04073, n = 548L),  # ancova_output.txt:296
    "University"             = list(mean = 5331.3370, sd = 1636.49099, n = 355L)   # ancova_output.txt:297
  )
  r <- ancova(survey_data, dv = income, between = education, covariate = age,
              weights = sampling_weight)
  d <- r$descriptives
  for (lv in names(cells)) {
    row <- d[d$education == lv, , drop = FALSE]
    assert_spss(as.numeric(row$mean), cells[[lv]]$mean, tier = "display",
                precision = 4, label = sprintf("[2a %s] mean", lv))
    assert_spss(as.numeric(row$sd), cells[[lv]]$sd, tier = "display",
                precision = 5, label = sprintf("[2a %s] SD", lv))
    assert_spss_count(as.numeric(row$n), cells[[lv]]$n,
                      label = sprintf("[2a %s] N", lv))
  }
})

test_that("Levene (unweighted 1a-1c, 3a-3b, 5a): matches SPSS UNIANOVA", {
  for (id in names(spss_levene)) {
    s <- spss_levene[[id]]
    r <- ancova(survey_data, dv = !!rlang::sym(s$dv), between = !!s$between,
                covariate = !!s$cov)
    lev <- r$levene_test
    assert_spss(lev$f, s$f, tier = "display", precision = 3,
                label = sprintf("[%s] Levene F", id))
    assert_spss_count(lev$df1, s$df1, label = sprintf("[%s] Levene df1", id))
    assert_spss_count(lev$df2, s$df2, label = sprintf("[%s] Levene df2", id))
    assert_spss(lev$p, s$p, tier = "display", precision = 3, what = "p_value",
                label = sprintf("[%s] Levene p", id))

    # levene_test() on the ancova result reports the same test
    lt <- levene_test(r)$results
    assert_spss(lt$F_statistic, s$f, tier = "display", precision = 3,
                label = sprintf("[%s] levene_test(ancova) F", id))
    assert_spss_count(lt$df2, s$df2,
                      label = sprintf("[%s] levene_test(ancova) df2", id))
    assert_spss(lt$p_value, s$p, tier = "display", precision = 3, what = "p_value",
                label = sprintf("[%s] levene_test(ancova) p", id))
  }
})


spss_levene_weighted <- list(
  # ancova_output.txt:305 (Test 2a: income BY education WITH age, /REGWGT)
  t2a = list(dv = "income", between = "education", cov = "age",
             f = 95.368, df1 = 3L, df2 = 2182L, p = "<.001"),
  # ancova_output.txt:395 (Test 2b: life_satisfaction BY gender WITH age)
  t2b = list(dv = "life_satisfaction", between = "gender", cov = "age",
             f = 0.902, df1 = 1L, df2 = 2419L, p = 0.342),
  # ancova_output.txt:711 (Test 4a: income BY gender education WITH age)
  t4a = list(dv = "income", between = c("gender", "education"), cov = "age",
             f = 41.256, df1 = 7L, df2 = 2178L, p = "<.001"),
  # ancova_output.txt:825 (Test 4b: life_satisfaction BY gender region WITH age)
  t4b = list(dv = "life_satisfaction", between = c("gender", "region"), cov = "age",
             f = 2.412, df1 = 3L, df2 = 2417L, p = 0.065),
  # ancova_output.txt:1015 (Test 6a: income BY education WITH age political_orientation)
  t6a = list(dv = "income", between = "education",
             cov = c("age", "political_orientation"),
             f = 89.755, df1 = 3L, df2 = 2004L, p = "<.001")
)

test_that("Levene (weighted /REGWGT 2a, 2b, 4a, 4b, 6a): matches SPSS UNIANOVA", {
  for (id in names(spss_levene_weighted)) {
    s <- spss_levene_weighted[[id]]
    r <- ancova(survey_data, dv = !!rlang::sym(s$dv), between = !!s$between,
                covariate = !!s$cov, weights = sampling_weight)
    lev <- r$levene_test
    assert_spss(lev$f, s$f, tier = "display", precision = 3,
                label = sprintf("[%s] weighted Levene F", id))
    assert_spss_count(lev$df1, s$df1, label = sprintf("[%s] weighted Levene df1", id))
    assert_spss_count(lev$df2, s$df2, label = sprintf("[%s] weighted Levene df2", id))
    assert_spss(lev$p, s$p, tier = "display", precision = 3, what = "p_value",
                label = sprintf("[%s] weighted Levene p", id))
  }
})


# =============================================================================
# Unweighted scenarios: every table SPSS prints (0.7.4 assertion audit)
# =============================================================================
# Between-Subjects Factors N, cell Descriptive Statistics, the Type III table,
# R squared, Parameter Estimates, Estimated Marginal Means and the covariate
# values they are evaluated at, for Tests 1a-1c, 3a, 3b, 5a (Levene: see
# above). Each value is cached as the string SPSS printed, so the Display
# precision is the number of printed decimals (Charter §4) and a printed
# ".000" p-value is the Spec sentinel "<.001". SPSS's marginal "Total" rows
# of Descriptive Statistics are not part of mariposa's cell table.

spss_values$unweighted <- list(
  # ---- Test 1a: UNIANOVA income BY education WITH age
  "1a" = list(
    dv = "income", between = c("education"), covariate = c("age"),
    # Between-Subjects Factors: factor, level, N
    bsf = list(
      c("education", "Basic Secondary", "735"),  # ancova_output.txt:28
      c("education", "Intermediate Secondary", "548"),  # ancova_output.txt:29
      c("education", "Academic Secondary", "548"),  # ancova_output.txt:30
      c("education", "University", "355")   # ancova_output.txt:31
    ),
    # Descriptive Statistics: education, Mean, Std. Deviation, N
    cells = list(
      c("Basic Secondary", "2759.0476", "786.56752", "735"),  # ancova_output.txt:37
      c("Intermediate Secondary", "3592.5182", "995.63887", "548"),  # ancova_output.txt:38
      c("Academic Secondary", "4224.0876", "1178.63526", "548"),  # ancova_output.txt:39
      c("University", "5337.1831", "1660.95846", "355")   # ancova_output.txt:40
    ),
    # (Type III table: asserted by the Test 1a block above)
    r2 = c(".391", ".390"),  # ancova_output.txt:62
    # Parameter Estimates: parameter, B, SE, t, Sig., CI lower, CI upper, partial eta^2
    # ("0" = redundant parameter set to zero; level codes mapped to labels)
    params = list(
      c("Intercept", "5349.320", "91.774", "58.288", ".000", "5169.346", "5529.293", ".609"),  # ancova_output.txt:69
      c("age", "-.245", "1.412", "-.174", ".862", "-3.014", "2.524", ".000"),  # ancova_output.txt:70
      c("[education=Basic Secondary]", "-2577.931", "72.359", "-35.627", ".000", "-2719.830", "-2436.032", ".368"),  # ancova_output.txt:71
      c("[education=Intermediate Secondary]", "-1744.236", "76.304", "-22.859", ".000", "-1893.871", "-1594.600", ".193"),  # ancova_output.txt:72
      c("[education=Academic Secondary]", "-1112.550", "76.328", "-14.576", ".000", "-1262.233", "-962.866", ".089"),  # ancova_output.txt:73
      c("[education=University]", "0")   # ancova_output.txt:74
    ),
    # Estimated Marginal Means: education, Mean, SE, CI lower, CI upper
    emm = list(
      c("Basic Secondary", "2758.939", "41.294", "2677.960", "2839.918"),  # ancova_output.txt:83
      c("Intermediate Secondary", "3592.634", "47.822", "3498.852", "3686.416"),  # ancova_output.txt:84
      c("Academic Secondary", "4224.320", "47.836", "4130.511", "4318.130"),  # ancova_output.txt:85
      c("University", "5336.870", "59.438", "5220.309", "5453.431")   # ancova_output.txt:86
    ),
    covariate_at = c(age = "50.8170")  # ancova_output.txt:87
  ),

  # ---- Test 1b: UNIANOVA life_satisfaction BY gender WITH age
  "1b" = list(
    dv = "life_satisfaction", between = c("gender"), covariate = c("age"),
    # Between-Subjects Factors: factor, level, N
    bsf = list(
      c("gender", "Male", "1149"),  # ancova_output.txt:116
      c("gender", "Female", "1272")   # ancova_output.txt:117
    ),
    # Descriptive Statistics: gender, Mean, Std. Deviation, N
    cells = list(
      c("Male", "3.60", "1.165", "1149"),  # ancova_output.txt:123
      c("Female", "3.65", "1.142", "1272")   # ancova_output.txt:124
    ),
    # Tests of Between-Subjects Effects: source, SS, df, MS, F, Sig., partial eta^2
    effects = list(
      c("Corrected Model", "4.071", "2", "2.036", "1.532", ".216", ".001"),  # ancova_output.txt:139
      c("Intercept", "3410.020", "1", "3410.020", "2565.986", ".000", ".515"),  # ancova_output.txt:140
      c("age", "2.691", "1", "2.691", "2.025", ".155", ".001"),  # ancova_output.txt:141
      c("gender", "1.418", "1", "1.418", "1.067", ".302", ".000"),  # ancova_output.txt:142
      c("Error", "3213.356", "2418", "1.329"),  # ancova_output.txt:143
      c("Total", "35088.000", "2421"),  # ancova_output.txt:144
      c("Corrected Total", "3217.428", "2420")   # ancova_output.txt:145
    ),
    r2 = c(".001", ".000"),  # ancova_output.txt:146
    # Parameter Estimates: parameter, B, SE, t, Sig., CI lower, CI upper, partial eta^2
    # ("0" = redundant parameter set to zero; level codes mapped to labels)
    params = list(
      c("Intercept", "3.750", ".077", "48.670", ".000", "3.599", "3.902", ".495"),  # ancova_output.txt:153
      c("age", "-.002", ".001", "-1.423", ".155", "-.005", ".001", ".001"),  # ancova_output.txt:154
      c("[gender=Male]", "-.048", ".047", "-1.033", ".302", "-.140", ".044", ".000"),  # ancova_output.txt:155
      c("[gender=Female]", "0")   # ancova_output.txt:156
    ),
    # Estimated Marginal Means: gender, Mean, SE, CI lower, CI upper
    emm = list(
      c("Male", "3.603", ".034", "3.536", "3.669"),  # ancova_output.txt:165
      c("Female", "3.651", ".032", "3.588", "3.715")   # ancova_output.txt:166
    ),
    covariate_at = c(age = "50.5832")  # ancova_output.txt:167
  ),

  # ---- Test 1c: UNIANOVA life_satisfaction BY education WITH political_orientation
  "1c" = list(
    dv = "life_satisfaction", between = c("education"), covariate = c("political_orientation"),
    # Between-Subjects Factors: factor, level, N
    bsf = list(
      c("education", "Basic Secondary", "739"),  # ancova_output.txt:196
      c("education", "Intermediate Secondary", "573"),  # ancova_output.txt:197
      c("education", "Academic Secondary", "559"),  # ancova_output.txt:198
      c("education", "University", "357")   # ancova_output.txt:199
    ),
    # Descriptive Statistics: education, Mean, Std. Deviation, N
    cells = list(
      c("Basic Secondary", "3.17", "1.237", "739"),  # ancova_output.txt:205
      c("Intermediate Secondary", "3.71", "1.115", "573"),  # ancova_output.txt:206
      c("Academic Secondary", "3.86", "1.006", "559"),  # ancova_output.txt:207
      c("University", "4.04", ".975", "357")   # ancova_output.txt:208
    ),
    # Tests of Between-Subjects Effects: source, SS, df, MS, F, Sig., partial eta^2
    effects = list(
      c("Corrected Model", "250.020", "4", "62.505", "50.633", ".000", ".083"),  # ancova_output.txt:223
      c("Intercept", "4137.304", "1", "4137.304", "3351.442", ".000", ".601"),  # ancova_output.txt:224
      c("political_orientation", ".004", "1", ".004", ".004", ".952", ".000"),  # ancova_output.txt:225
      c("education", "250.017", "3", "83.339", "67.509", ".000", ".083"),  # ancova_output.txt:226
      c("Error", "2744.260", "2223", "1.234"),  # ancova_output.txt:227
      c("Total", "32210.000", "2228"),  # ancova_output.txt:228
      c("Corrected Total", "2994.280", "2227")   # ancova_output.txt:229
    ),
    r2 = c(".083", ".082"),  # ancova_output.txt:230
    # Parameter Estimates: parameter, B, SE, t, Sig., CI lower, CI upper, partial eta^2
    # ("0" = redundant parameter set to zero; level codes mapped to labels)
    params = list(
      c("Intercept", "4.039", ".082", "49.128", ".000", "3.877", "4.200", ".521"),  # ancova_output.txt:237
      c("political_orientation", ".001", ".022", ".060", ".952", "-.041", ".044", ".000"),  # ancova_output.txt:238
      c("[education=Basic Secondary]", "-.873", ".072", "-12.187", ".000", "-1.013", "-.732", ".063"),  # ancova_output.txt:239
      c("[education=Intermediate Secondary]", "-.330", ".075", "-4.404", ".000", "-.477", "-.183", ".009"),  # ancova_output.txt:240
      c("[education=Academic Secondary]", "-.185", ".075", "-2.460", ".014", "-.333", "-.038", ".003"),  # ancova_output.txt:241
      c("[education=University]", "0")   # ancova_output.txt:242
    ),
    # Estimated Marginal Means: education, Mean, SE, CI lower, CI upper
    emm = list(
      c("Basic Secondary", "3.169", ".041", "3.089", "3.249"),  # ancova_output.txt:251
      c("Intermediate Secondary", "3.712", ".046", "3.621", "3.803"),  # ancova_output.txt:252
      c("Academic Secondary", "3.857", ".047", "3.765", "3.949"),  # ancova_output.txt:253
      c("University", "4.042", ".059", "3.927", "4.157")   # ancova_output.txt:254
    ),
    covariate_at = c(political_orientation = "2.73")  # ancova_output.txt:255
  ),

  # ---- Test 3a: UNIANOVA income BY gender education WITH age
  "3a" = list(
    dv = "income", between = c("gender", "education"), covariate = c("age"),
    # Between-Subjects Factors: factor, level, N
    bsf = list(
      c("gender", "Male", "1046"),  # ancova_output.txt:464
      c("gender", "Female", "1140"),  # ancova_output.txt:465
      c("education", "Basic Secondary", "735"),  # ancova_output.txt:466
      c("education", "Intermediate Secondary", "548"),  # ancova_output.txt:467
      c("education", "Academic Secondary", "548"),  # ancova_output.txt:468
      c("education", "University", "355")   # ancova_output.txt:469
    ),
    # Descriptive Statistics: gender, education, Mean, Std. Deviation, N
    cells = list(
      c("Male", "Basic Secondary", "2803.4286", "774.95889", "350"),  # ancova_output.txt:475
      c("Male", "Intermediate Secondary", "3574.0891", "996.64920", "247"),  # ancova_output.txt:476
      c("Male", "Academic Secondary", "4246.4539", "1180.77942", "282"),  # ancova_output.txt:477
      c("Male", "University", "5318.5629", "1718.80537", "167"),  # ancova_output.txt:478
      c("Female", "Basic Secondary", "2718.7013", "795.83118", "385"),  # ancova_output.txt:480
      c("Female", "Intermediate Secondary", "3607.6412", "996.21354", "301"),  # ancova_output.txt:481
      c("Female", "Academic Secondary", "4200.3759", "1178.11804", "266"),  # ancova_output.txt:482
      c("Female", "University", "5353.7234", "1612.26481", "188")   # ancova_output.txt:483
    ),
    # Tests of Between-Subjects Effects: source, SS, df, MS, F, Sig., partial eta^2
    effects = list(
      c("Corrected Model", "1754673974.673", "8", "219334246.834", "174.844", ".000", ".391"),  # ancova_output.txt:503
      c("Intercept", "3459603648.688", "1", "3459603648.688", "2757.845", ".000", ".559"),  # ancova_output.txt:504
      c("age", "21904.825", "1", "21904.825", ".017", ".895", ".000"),  # ancova_output.txt:505
      c("gender", "121746.808", "1", "121746.808", ".097", ".755", ".000"),  # ancova_output.txt:506
      c("education", "1743507542.109", "3", "581169180.703", "463.283", ".000", ".390"),  # ancova_output.txt:507
      c("gender * education", "1486565.745", "3", "495521.915", ".395", ".757", ".001"),  # ancova_output.txt:508
      c("Error", "2730957191.842", "2177", "1254458.977"),  # ancova_output.txt:509
      c("Total", "35290790000.000", "2186"),  # ancova_output.txt:510
      c("Corrected Total", "4485631166.515", "2185")   # ancova_output.txt:511
    ),
    r2 = c(".391", ".389"),  # ancova_output.txt:512
    # Parameter Estimates: parameter, B, SE, t, Sig., CI lower, CI upper, partial eta^2
    # ("0" = redundant parameter set to zero; level codes mapped to labels)
    params = list(
      c("Intercept", "5363.034", "107.875", "49.715", ".000", "5151.485", "5574.583", ".532"),  # ancova_output.txt:519
      c("age", "-.187", "1.414", "-.132", ".895", "-2.960", "2.586", ".000"),  # ancova_output.txt:520
      c("[gender=Male]", "-35.274", "119.101", "-.296", ".767", "-268.838", "198.290", ".000"),  # ancova_output.txt:521
      c("[gender=Female]", "0"),  # ancova_output.txt:522
      c("[education=Basic Secondary]", "-2634.729", "99.679", "-26.432", ".000", "-2830.205", "-2439.253", ".243"),  # ancova_output.txt:523
      c("[education=Intermediate Secondary]", "-1745.926", "104.123", "-16.768", ".000", "-1950.118", "-1541.734", ".114"),  # ancova_output.txt:524
      c("[education=Academic Secondary]", "-1153.000", "106.750", "-10.801", ".000", "-1362.343", "-943.657", ".051"),  # ancova_output.txt:525
      c("[education=University]", "0"),  # ancova_output.txt:526
      c("[gender=Male] * [education=Basic Secondary]", "119.601", "145.023", ".825", ".410", "-164.796", "403.999", ".000"),  # ancova_output.txt:527
      c("[gender=Male] * [education=Intermediate Secondary]", "1.983", "153.097", ".013", ".990", "-298.250", "302.215", ".000"),  # ancova_output.txt:528
      c("[gender=Male] * [education=Academic Secondary]", "81.382", "152.807", ".533", ".594", "-218.281", "381.045", ".000"),  # ancova_output.txt:529
      c("[gender=Male] * [education=University]", "0"),  # ancova_output.txt:530
      c("[gender=Female] * [education=Basic Secondary]", "0"),  # ancova_output.txt:531
      c("[gender=Female] * [education=Intermediate Secondary]", "0"),  # ancova_output.txt:532
      c("[gender=Female] * [education=Academic Secondary]", "0"),  # ancova_output.txt:533
      c("[gender=Female] * [education=University]", "0")   # ancova_output.txt:534
    ),
    # Estimated Marginal Means: gender, education, Mean, SE, CI lower, CI upper
    emm = list(
      c("Male", "Basic Secondary", "2803.136", "59.909", "2685.652", "2920.621"),  # ancova_output.txt:543
      c("Male", "Intermediate Secondary", "3574.321", "71.287", "3434.523", "3714.119"),  # ancova_output.txt:544
      c("Male", "Academic Secondary", "4246.646", "66.712", "4115.819", "4377.472"),  # ancova_output.txt:545
      c("Male", "University", "5318.264", "86.700", "5148.241", "5488.287"),  # ancova_output.txt:546
      c("Female", "Basic Secondary", "2718.809", "57.088", "2606.857", "2830.761"),  # ancova_output.txt:547
      c("Female", "Intermediate Secondary", "3607.612", "64.558", "3481.011", "3734.213"),  # ancova_output.txt:548
      c("Female", "Academic Secondary", "4200.538", "68.684", "4065.845", "4335.231"),  # ancova_output.txt:549
      c("Female", "University", "5353.538", "81.698", "5193.323", "5513.753")   # ancova_output.txt:550
    ),
    covariate_at = c(age = "50.8170")  # ancova_output.txt:551
  ),

  # ---- Test 3b: UNIANOVA life_satisfaction BY gender region WITH age
  "3b" = list(
    dv = "life_satisfaction", between = c("gender", "region"), covariate = c("age"),
    # Between-Subjects Factors: factor, level, N
    bsf = list(
      c("gender", "Male", "1149"),  # ancova_output.txt:580
      c("gender", "Female", "1272"),  # ancova_output.txt:581
      c("region", "East", "465"),  # ancova_output.txt:582
      c("region", "West", "1956")   # ancova_output.txt:583
    ),
    # Descriptive Statistics: gender, region, Mean, Std. Deviation, N
    cells = list(
      c("Male", "East", "3.65", "1.209", "228"),  # ancova_output.txt:589
      c("Male", "West", "3.59", "1.154", "921"),  # ancova_output.txt:590
      c("Female", "East", "3.59", "1.206", "237"),  # ancova_output.txt:592
      c("Female", "West", "3.67", "1.127", "1035")   # ancova_output.txt:593
    ),
    # Tests of Between-Subjects Effects: source, SS, df, MS, F, Sig., partial eta^2
    effects = list(
      c("Corrected Model", "5.879", "4", "1.470", "1.106", ".352", ".002"),  # ancova_output.txt:611
      c("Intercept", "3146.242", "1", "3146.242", "2366.870", ".000", ".495"),  # ancova_output.txt:612
      c("age", "2.567", "1", "2.567", "1.931", ".165", ".001"),  # ancova_output.txt:613
      c("gender", ".013", "1", ".013", ".010", ".921", ".000"),  # ancova_output.txt:614
      c("region", ".010", "1", ".010", ".007", ".931", ".000"),  # ancova_output.txt:615
      c("gender * region", "1.789", "1", "1.789", "1.346", ".246", ".001"),  # ancova_output.txt:616
      c("Error", "3211.549", "2416", "1.329"),  # ancova_output.txt:617
      c("Total", "35088.000", "2421"),  # ancova_output.txt:618
      c("Corrected Total", "3217.428", "2420")   # ancova_output.txt:619
    ),
    r2 = c(".002", ".000"),  # ancova_output.txt:620
    # Parameter Estimates: parameter, B, SE, t, Sig., CI lower, CI upper, partial eta^2
    # ("0" = redundant parameter set to zero; level codes mapped to labels)
    params = list(
      c("Intercept", "3.762", ".078", "48.188", ".000", "3.609", "3.915", ".490"),  # ancova_output.txt:627
      c("age", "-.002", ".001", "-1.390", ".165", "-.005", ".001", ".001"),  # ancova_output.txt:628
      c("[gender=Male]", "-.075", ".052", "-1.435", ".151", "-.177", ".027", ".001"),  # ancova_output.txt:629
      c("[gender=Female]", "0"),  # ancova_output.txt:630
      c("[region=East]", "-.074", ".083", "-.893", ".372", "-.237", ".089", ".000"),  # ancova_output.txt:631
      c("[region=West]", "0"),  # ancova_output.txt:632
      c("[gender=Male] * [region=East]", ".138", ".119", "1.160", ".246", "-.095", ".372", ".001"),  # ancova_output.txt:633
      c("[gender=Male] * [region=West]", "0"),  # ancova_output.txt:634
      c("[gender=Female] * [region=East]", "0"),  # ancova_output.txt:635
      c("[gender=Female] * [region=West]", "0")   # ancova_output.txt:636
    ),
    # Estimated Marginal Means: gender, region, Mean, SE, CI lower, CI upper
    emm = list(
      c("Male", "East", "3.654", ".076", "3.504", "3.804"),  # ancova_output.txt:645
      c("Male", "West", "3.590", ".038", "3.516", "3.665"),  # ancova_output.txt:646
      c("Female", "East", "3.591", ".075", "3.444", "3.738"),  # ancova_output.txt:647
      c("Female", "West", "3.665", ".036", "3.595", "3.735")   # ancova_output.txt:648
    ),
    covariate_at = c(age = "50.5832")  # ancova_output.txt:649
  ),

  # ---- Test 5a: UNIANOVA income BY education WITH age political_orientation
  "5a" = list(
    dv = "income", between = c("education"), covariate = c("age", "political_orientation"),
    # Between-Subjects Factors: factor, level, N
    bsf = list(
      c("education", "Basic Secondary", "671"),  # ancova_output.txt:904
      c("education", "Intermediate Secondary", "510"),  # ancova_output.txt:905
      c("education", "Academic Secondary", "501"),  # ancova_output.txt:906
      c("education", "University", "326")   # ancova_output.txt:907
    ),
    # Descriptive Statistics: education, Mean, Std. Deviation, N
    cells = list(
      c("Basic Secondary", "2735.7675", "777.21429", "671"),  # ancova_output.txt:913
      c("Intermediate Secondary", "3595.4902", "1003.94079", "510"),  # ancova_output.txt:914
      c("Academic Secondary", "4215.5689", "1173.71084", "501"),  # ancova_output.txt:915
      c("University", "5286.5031", "1668.42221", "326")   # ancova_output.txt:916
    ),
    # Tests of Between-Subjects Effects: source, SS, df, MS, F, Sig., partial eta^2
    effects = list(
      c("Corrected Model", "1582046608.943", "5", "316409321.789", "252.422", ".000", ".387"),  # ancova_output.txt:931
      c("Intercept", "1958785035.087", "1", "1958785035.087", "1562.659", ".000", ".438"),  # ancova_output.txt:932
      c("age", "21945.241", "1", "21945.241", ".018", ".895", ".000"),  # ancova_output.txt:933
      c("political_orientation", "1712167.106", "1", "1712167.106", "1.366", ".243", ".001"),  # ancova_output.txt:934
      c("education", "1576957660.342", "3", "525652553.448", "419.350", ".000", ".386"),  # ancova_output.txt:935
      c("Error", "2509497136.078", "2002", "1253495.073"),  # ancova_output.txt:936
      c("Total", "32140360000.000", "2008"),  # ancova_output.txt:937
      c("Corrected Total", "4091543745.020", "2007")   # ancova_output.txt:938
    ),
    r2 = c(".387", ".385"),  # ancova_output.txt:939
    # Parameter Estimates: parameter, B, SE, t, Sig., CI lower, CI upper, partial eta^2
    # ("0" = redundant parameter set to zero; level codes mapped to labels)
    params = list(
      c("Intercept", "5366.554", "114.485", "46.876", ".000", "5142.033", "5591.075", ".523"),  # ancova_output.txt:946
      c("age", "-.196", "1.479", "-.132", ".895", "-3.096", "2.704", ".000"),  # ancova_output.txt:947
      c("political_orientation", "-26.771", "22.906", "-1.169", ".243", "-71.693", "18.151", ".001"),  # ancova_output.txt:948
      c("[education=Basic Secondary]", "-2548.205", "75.621", "-33.697", ".000", "-2696.509", "-2399.900", ".362"),  # ancova_output.txt:949
      c("[education=Intermediate Secondary]", "-1686.928", "79.501", "-21.219", ".000", "-1842.841", "-1531.015", ".184"),  # ancova_output.txt:950
      c("[education=Academic Secondary]", "-1067.765", "79.794", "-13.382", ".000", "-1224.254", "-911.277", ".082"),  # ancova_output.txt:951
      c("[education=University]", "0")   # ancova_output.txt:952
    ),
    # Estimated Marginal Means: education, Mean, SE, CI lower, CI upper
    emm = list(
      c("Basic Secondary", "2735.624", "43.231", "2650.841", "2820.408"),  # ancova_output.txt:961
      c("Intermediate Secondary", "3596.901", "49.594", "3499.640", "3694.163"),  # ancova_output.txt:962
      c("Academic Secondary", "4216.064", "50.054", "4117.900", "4314.228"),  # ancova_output.txt:963
      c("University", "5283.829", "62.076", "5162.090", "5405.569")   # ancova_output.txt:964
    ),
    covariate_at = c(age = "50.7629", political_orientation = "2.72")  # ancova_output.txt:965
  )
)

# Weighted (/REGWGT) cell descriptives of Tests 2b, 4a, 4b, 6a (2a: see
# above). The weighted Type III / parameter / EMM tables are pending the
# SPSS run with WEIGHT BY and are deliberately not asserted.

spss_values$weighted_cells <- list(
  # ---- Test 2b: UNIANOVA life_satisfaction BY gender WITH age /REGWGT
  "2b" = list(
    dv = "life_satisfaction", between = c("gender"), covariate = c("age"),
    # Descriptive Statistics: gender, Mean, Std. Deviation, N
    cells = list(
      c("Male", "3.60", "1.165", "1149"),  # ancova_output.txt:386
      c("Female", "3.65", "1.147", "1272")   # ancova_output.txt:387
    )
  ),

  # ---- Test 4a: UNIANOVA income BY gender education WITH age /REGWGT
  "4a" = list(
    dv = "income", between = c("gender", "education"), covariate = c("age"),
    # Descriptive Statistics: gender, education, Mean, Std. Deviation, N
    cells = list(
      c("Male", "Basic Secondary", "2801.2376", "781.15116", "350"),  # ancova_output.txt:690
      c("Male", "Intermediate Secondary", "3579.2969", "1000.71738", "247"),  # ancova_output.txt:691
      c("Male", "Academic Secondary", "4244.2258", "1196.72201", "282"),  # ancova_output.txt:692
      c("Male", "University", "5314.4098", "1694.94091", "167"),  # ancova_output.txt:693
      c("Female", "Basic Secondary", "2721.4740", "799.03345", "385"),  # ancova_output.txt:695
      c("Female", "Intermediate Secondary", "3598.9657", "1007.90453", "301"),  # ancova_output.txt:696
      c("Female", "Academic Secondary", "4205.1651", "1186.90520", "266"),  # ancova_output.txt:697
      c("Female", "University", "5346.1764", "1587.16986", "188")   # ancova_output.txt:698
    )
  ),

  # ---- Test 4b: UNIANOVA life_satisfaction BY gender region WITH age /REGWGT
  "4b" = list(
    dv = "life_satisfaction", between = c("gender", "region"), covariate = c("age"),
    # Descriptive Statistics: gender, region, Mean, Std. Deviation, N
    cells = list(
      c("Male", "East", "3.66", "1.238", "228"),  # ancova_output.txt:810
      c("Male", "West", "3.58", "1.147", "921"),  # ancova_output.txt:811
      c("Female", "East", "3.59", "1.230", "237"),  # ancova_output.txt:813
      c("Female", "West", "3.66", "1.128", "1035")   # ancova_output.txt:814
    )
  ),

  # ---- Test 6a: UNIANOVA income BY education WITH age political_orientation /REGWGT
  "6a" = list(
    dv = "income", between = c("education"), covariate = c("age", "political_orientation"),
    # Descriptive Statistics: education, Mean, Std. Deviation, N
    cells = list(
      c("Basic Secondary", "2736.8169", "781.32352", "671"),  # ancova_output.txt:1004
      c("Intermediate Secondary", "3592.4857", "1010.96302", "510"),  # ancova_output.txt:1005
      c("Academic Secondary", "4218.1950", "1185.13868", "501"),  # ancova_output.txt:1006
      c("University", "5278.5537", "1644.15557", "326")   # ancova_output.txt:1007
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

# "Male/Basic Secondary"-style keys of the factor columns of a result table
cell_keys <- function(tab, factors) {
  do.call(paste, c(lapply(factors, function(f) as.character(tab[[f]])),
                   sep = "/"))
}

# Rows keyed by their first `k` fields, checked in SPSS order; `cols` are the
# result columns holding the remaining printed fields
assert_keyed_rows <- function(tab, rows, factors, cols, id, what,
                              count_cols = character()) {
  k <- length(factors)
  keys <- vapply(rows, function(row) paste(row[seq_len(k)], collapse = "/"), "")
  expect_identical(cell_keys(tab, factors), keys,
                   label = sprintf("[%s] %s rows in SPSS order", id, what))
  for (i in seq_along(rows)) {
    x <- tab[cell_keys(tab, factors) == keys[i], , drop = FALSE]
    for (j in seq_along(cols)) {
      lab <- sprintf("[%s %s %s] %s", id, what, keys[i], cols[j])
      printed <- rows[[i]][k + j]
      if (cols[j] %in% count_cols) {
        assert_spss_count(as.numeric(x[[cols[j]]]), as.integer(printed), lab)
      } else {
        assert_printed(as.numeric(x[[cols[j]]]), printed, lab)
      }
    }
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

# Parameter Estimates, parameters in SPSS order (last category = 0,
# printed "0(a)": redundant)
param_cols <- c("b", "se", "t", "p", "ci_lower", "ci_upper", "partial_eta_sq")
assert_params <- function(pe, s, id) {
  expect_identical(pe$parameter, vapply(s$params, `[`, "", 1L),
                   label = sprintf("[%s] parameters in SPSS order", id))
  for (row in s$params) {
    x <- pe[pe$parameter == row[1], , drop = FALSE]
    lab <- sprintf("[%s %s]", id, row[1])
    if (length(row) == 2L) {
      expect_true(isTRUE(x$redundant), label = paste(lab, "redundant"))
      expect_equal(as.numeric(x$b), 0, label = paste(lab, "B"))
      next
    }
    expect_false(isTRUE(x$redundant), label = paste(lab, "redundant"))
    for (j in seq_along(row)[-1]) {
      col <- param_cols[j - 1L]
      assert_printed(as.numeric(x[[col]]), row[j], paste(lab, col),
                     p_value = col == "p")
    }
  }
}


for (id in names(spss_values$unweighted)) {
  test_that(sprintf("Test %s: unweighted UNIANOVA tables match SPSS", id), {
    s <- spss_values$unweighted[[id]]
    r <- ancova(survey_data, dv = !!rlang::sym(s$dv), between = !!s$between,
                covariate = !!s$covariate)

    assert_bsf(r$descriptives, s, id)
    assert_keyed_rows(r$descriptives, s$cells, s$between,
                      c("mean", "sd", "n"), id, "cell", count_cols = "n")
    if (!is.null(s$effects)) assert_effects(r$anova_table, s, id)
    assert_printed(r$r_squared[["r_squared"]], s$r2[1],
                   sprintf("[%s] R Squared", id))
    assert_printed(r$r_squared[["adj_r_squared"]], s$r2[2],
                   sprintf("[%s] Adjusted R Squared", id))
    assert_params(r$parameter_estimates, s, id)
    assert_keyed_rows(r$estimated_marginal_means, s$emm, s$between,
                      c("mean", "se", "ci_lower", "ci_upper"), id, "EMM")
    # The EMMs are evaluated at the covariate means of the analysis cases
    for (cv in names(s$covariate_at)) {
      assert_printed(mean(r$data[[cv]]), s$covariate_at[[cv]],
                     sprintf("[%s] EMM covariate value %s", id, cv))
    }
  })
}

for (id in names(spss_values$weighted_cells)) {
  test_that(sprintf("Test %s: weighted cell descriptives match SPSS /REGWGT", id), {
    s <- spss_values$weighted_cells[[id]]
    r <- ancova(survey_data, dv = !!rlang::sym(s$dv), between = !!s$between,
                covariate = !!s$covariate, weights = sampling_weight)
    assert_keyed_rows(r$descriptives, s$cells, s$between,
                      c("mean", "sd", "n"), id, "cell", count_cols = "n")
  })
}
