# ============================================================================
# GAMMA ASE TESTING - PART 3
# Testing if SPSS displays ASE_1 but uses ASE_0 for p-values
# ============================================================================

library(dplyr)
library(SurveyStat)
data(survey_data)

# From SPSS output, we know:
# - The displayed ASE values
# - The t-statistics (Approximate T)
# - The p-values (Approximate Significance)

# Key insight: t = gamma / ASE, and we can verify this

spss_data <- data.frame(
  test = c("1a", "1b", "1c"),
  vars1 = c("gender", "education", "gender"),
  vars2 = c("region", "employment", "education"),
  gamma = c(0.033, -0.065, -0.008),
  ase_displayed = c(0.051, 0.028, 0.030),
  t_stat = c(0.644, -2.321, -0.272),
  p_value = c(0.520, 0.020, 0.786)
)

# Verify t = gamma / ASE for displayed values
spss_data$t_calculated <- spss_data$gamma / spss_data$ase_displayed
spss_data$t_match <- round(spss_data$t_calculated, 3) == round(spss_data$t_stat, 3)

cat("Verification that t = gamma / ASE_displayed:\n")
print(spss_data[, c("test", "gamma", "ase_displayed", "t_stat", "t_calculated", "t_match")])

# Now, work backwards from p-value to find what ASE would give that p-value
cat("\n\nWorking backwards from p-values:\n")

for(i in 1:nrow(spss_data)) {
  target_p <- spss_data$p_value[i]
  gamma <- spss_data$gamma[i]

  # Find what t-value gives this p-value (using normal distribution)
  # For two-tailed test: p = 2 * (1 - pnorm(abs(t)))
  # So: 1 - p/2 = pnorm(abs(t))
  # abs(t) = qnorm(1 - p/2)

  t_from_p_norm <- qnorm(1 - target_p/2)

  # Also try t-distribution with various df
  n <- 2500  # sample size
  t_from_p_t <- qt(1 - target_p/2, df = n - 2)

  # What ASE would these imply?
  ase_from_norm <- abs(gamma) / t_from_p_norm
  ase_from_t <- abs(gamma) / t_from_p_t

  cat("\nTest", spss_data$test[i], ":\n")
  cat("  Target p-value:", target_p, "\n")
  cat("  t from normal dist:", round(t_from_p_norm, 3), "\n")
  cat("  t from t-dist (df=n-2):", round(t_from_p_t, 3), "\n")
  cat("  SPSS reported t:", spss_data$t_stat[i], "\n")
  cat("  ASE that would give p via normal:", round(ase_from_norm, 4), "\n")
  cat("  ASE that would give p via t-dist:", round(ase_from_t, 4), "\n")
  cat("  SPSS displayed ASE:", spss_data$ase_displayed[i], "\n")
}

# The key finding: SPSS's t-stat matches gamma/ASE_displayed perfectly
# But the p-value doesn't match what we'd expect from that t-stat

cat("\n\n======== CHECKING P-VALUE CALCULATION ========\n")
for(i in 1:nrow(spss_data)) {
  t <- spss_data$t_stat[i]

  # Calculate p-values using different methods
  p_norm <- 2 * (1 - pnorm(abs(t)))
  p_t_large <- 2 * (1 - pt(abs(t), df = 2498))
  p_t_small <- 2 * (1 - pt(abs(t), df = 10))

  cat("\nTest", spss_data$test[i], "(t =", t, "):\n")
  cat("  p-value from normal:", round(p_norm, 3), "\n")
  cat("  p-value from t(df=2498):", round(p_t_large, 3), "\n")
  cat("  p-value from t(df=10):", round(p_t_small, 3), "\n")
  cat("  SPSS p-value:", spss_data$p_value[i], "\n")
}

# This confirms: SPSS calculates p-values correctly from the t-statistics
# So the issue must be in how we calculate ASE

cat("\n\n======== FINAL INSIGHT ========\n")
cat("SPSS shows the correct t = gamma/ASE relationship\n")
cat("SPSS shows the correct p-value for the t-statistic\n")
cat("Therefore, SPSS must be using a different ASE formula than we are!\n")
cat("\nThe ASE SPSS displays IS the one used for the p-value calculation.\n")
cat("We need to find what formula produces ASE = 0.051 for the 2x2 table.\n")