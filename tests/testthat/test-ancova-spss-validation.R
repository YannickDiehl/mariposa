# =============================================================================
# ancova — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::ancova() against SPSS v29 UNIANOVA with WITH.
# Reference: ancova_output.txt
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(
  # SPSS Test 1a: income ~ age + education (covariate = age, factor = education)
  test_1a_income_age_education = list(
    "Corrected Model" = list(ss = 1752821002.875, df = 4L, ms = 438205250.719, f = 349.723, p = "<.001", eta2 = 0.391),    # line 55
    "Intercept"       = list(ss = 3472400779.184, df = 1L, f = 2771.252, p = "<.001", eta2 = 0.560),
    "age"             = list(ss = 37721.406,      df = 1L, f = 0.030, p = 0.862, eta2 = 0.000),
    "education"       = list(ss = 1752630664.966, df = 3L, f = 466.246, p = "<.001", eta2 = 0.391),
    "Error"           = list(ss = 2732810163.640, df = 2181L, ms = 1253007.870),
    "Total"           = list(ss = 35290790000.000, df = 2186L),
    "Corrected Total" = list(ss = 4485631166.515, df = 2185L)
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
