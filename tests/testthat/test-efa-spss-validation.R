# =============================================================================
# efa — SPSS VALIDATION (Charter-compliant)
# =============================================================================
# Purpose: Validate mariposa::efa() against SPSS v29 FACTOR.
# Reference: efa_output.txt
#
# Validates KMO, Bartlett's, eigenvalues, variance explained, communalities.
# PCA + Varimax (default). ML/Promax handled in separate ref file (skipped).
# =============================================================================

library(testthat)
library(dplyr)
library(mariposa)


spss_values <- list(
  test_1a = list(
    # 6 variables: political_orientation, environmental_concern,
    # life_satisfaction, trust_government, trust_media, trust_science
    kmo = 0.505,                              # efa_output.txt:92
    bartlett_chi_sq = 932.068,                # efa_output.txt:93
    bartlett_df = 15L,                        # efa_output.txt:94
    bartlett_p = "<.001",                     # efa_output.txt:95
    eigenvalues = c(1.600, 1.041, 1.017, 0.980, 0.949, 0.412),  # lines 112-117
    var_explained = c(26.666, 17.358, 16.955, 16.334, 15.814, 6.873),
    cumulative = c(26.666, 44.024, 60.979, 77.313, 93.127, 100.000),
    communalities = c(political_orientation = 0.786,                # lines 100-105
                       environmental_concern = 0.783,
                       life_satisfaction     = 0.668,
                       trust_government      = 0.347,
                       trust_media           = 0.475,
                       trust_science         = 0.598)
  )
)


data(survey_data, envir = environment())


test_that("Test 1a: EFA 6-variable PCA/Varimax — matches SPSS", {
  r <- efa(survey_data,
            political_orientation, environmental_concern, life_satisfaction,
            trust_government, trust_media, trust_science)
  spss <- spss_values$test_1a

  # KMO
  assert_spss(as.numeric(r$kmo$overall), spss$kmo,
              tier = "display", precision = 3,
              label = "[1a] KMO overall")

  # Bartlett's test
  assert_spss(as.numeric(r$bartlett$chi_sq), spss$bartlett_chi_sq,
              tier = "display", precision = 3,
              label = "[1a] Bartlett chi²")
  assert_spss_count(as.numeric(r$bartlett$df), spss$bartlett_df,
                    label = "[1a] Bartlett df")
  assert_spss(as.numeric(r$bartlett$p_value), spss$bartlett_p,
              tier = "display", precision = 3, what = "p_value",
              label = "[1a] Bartlett p")

  # Eigenvalues + variance explained
  for (i in seq_along(spss$eigenvalues)) {
    assert_spss(r$variance_explained$eigenvalue[i], spss$eigenvalues[i],
                tier = "display", precision = 3,
                label = sprintf("[1a] component %d eigenvalue", i))
    assert_spss(r$variance_explained$prc_variance[i], spss$var_explained[i],
                tier = "display", precision = 3,
                label = sprintf("[1a] component %d %% variance", i))
    assert_spss(r$variance_explained$cumulative_prc[i], spss$cumulative[i],
                tier = "display", precision = 3,
                label = sprintf("[1a] component %d cumulative %%", i))
  }

  # Communalities (named numeric vector keyed by variable)
  for (var in names(spss$communalities)) {
    actual <- r$communalities[[var]]
    assert_spss(as.numeric(actual), spss$communalities[[var]],
                tier = "display", precision = 3,
                label = sprintf("[1a] communality (%s)", var))
  }
})
