# ============================================================================
# GAMMA ASE TESTING - PART 5
# Testing simpler ASE formulas that might match SPSS
# ============================================================================

library(dplyr)
library(SurveyStat)
data(survey_data)

# Looking at the patterns from previous tests:
# freeman_approx gives ratios of: 2.55 (2x2), 1.40 (4x5), 1.50 (2x4)
# These don't match simple table dimensions
# Let's explore what SPSS might be doing differently

calculate_gamma_ase_simplified <- function(tab) {
  r <- nrow(tab)
  c <- ncol(tab)
  n <- sum(tab)

  # Calculate P and Q
  P <- 0
  Q <- 0
  for(i in 1:r) {
    for(j in 1:c) {
      n1 <- tab[i,j]
      if(n1 > 0) {
        if(i < r && j < c) {
          for(i2 in (i+1):r) {
            for(j2 in (j+1):c) {
              P <- P + n1 * tab[i2, j2]
            }
          }
        }
        if(i < r && j > 1) {
          for(i2 in (i+1):r) {
            for(j2 in 1:(j-1)) {
              Q <- Q + n1 * tab[i2, j2]
            }
          }
        }
      }
    }
  }

  gamma <- (P - Q) / (P + Q)

  # Test different simple formulas
  ases <- list()

  # Base formula we've been using
  ases$base <- 2 * sqrt(P * Q) / ((P + Q) * sqrt(n))

  # What if SPSS uses a different denominator?
  # Maybe they use the number of pairs differently
  n_pairs <- n * (n - 1) / 2
  ases$pairs_denom <- sqrt(4 * P * Q) / (P + Q) / sqrt(n_pairs)

  # What if they use a simpler variance formula?
  # Standard error based on gamma itself
  ases$gamma_based <- sqrt((1 - gamma^2) / n)

  # What if it's based on the proportion of concordant/discordant pairs?
  prop_concordant <- P / n_pairs
  prop_discordant <- Q / n_pairs
  ases$proportion_based <- sqrt((prop_concordant + prop_discordant) * (1 - gamma^2) / n)

  # What if SPSS uses cell frequencies differently?
  # Average cell frequency
  avg_cell <- n / (r * c)
  ases$cell_avg <- 2 * sqrt(P * Q) / ((P + Q) * sqrt(avg_cell))

  # What if there's a degrees of freedom adjustment?
  df <- (r - 1) * (c - 1)
  ases$df_adjusted <- 2 * sqrt(P * Q) / ((P + Q) * sqrt(n)) * sqrt(df)

  # What if SPSS uses effective sample size?
  # For a 2x2 table, each cell contributes to multiple pairs
  # Effective n might be different
  ases$eff_n <- 2 * sqrt(P * Q) / ((P + Q) * sqrt(n / min(r, c)))

  # What if it's based on marginal distributions?
  row_margins <- rowSums(tab)
  col_margins <- colSums(tab)
  margin_factor <- sum(row_margins^2) * sum(col_margins^2) / n^4
  ases$margin_based <- sqrt(margin_factor) * (2 / (P + Q))

  # Another possibility: Maybe SPSS uses a completely different formula
  # Based on the observed ratios: 0.051/0.020 ≈ 2.55 for 2x2
  # This is close to sqrt(6.5), but why?

  # What if they multiply by a correction factor?
  # For 2x2: need 0.051 from 0.020
  # For 4x5: need 0.028 from 0.020
  # For 2x4: need 0.030 from 0.020

  # These ratios suggest table-specific adjustments
  # Let's calculate what adjustment would be needed
  base_simple <- 2 * sqrt(P * Q) / ((P + Q) * sqrt(n))

  return(list(
    gamma = gamma,
    P = P,
    Q = Q,
    n = n,
    r = r,
    c = c,
    ases = ases,
    base_simple = base_simple
  ))
}

# Test cases
test_cases <- list(
  test1a = list(vars = c("gender", "region"), spss_ase = 0.051),
  test1b = list(vars = c("education", "employment"), spss_ase = 0.028),
  test1c = list(vars = c("gender", "education"), spss_ase = 0.030)
)

cat("Testing simplified ASE formulas:\n")
cat("=" , rep("=", 70), "\n", sep="")

results_summary <- data.frame()

for(test_name in names(test_cases)) {
  test <- test_cases[[test_name]]
  tab <- table(survey_data[[test$vars[1]]], survey_data[[test$vars[2]]])

  result <- calculate_gamma_ase_simplified(tab)

  cat("\n", test_name, " (", result$r, "x", result$c, " table):\n", sep="")
  cat("  Gamma =", round(result$gamma, 4), "\n")
  cat("  SPSS ASE =", test$spss_ase, "\n")
  cat("  Base simple ASE =", round(result$base_simple, 4), "\n")
  cat("  Ratio needed:", round(test$spss_ase / result$base_simple, 3), "\n\n")

  cat("  ASE calculations:\n")
  for(method in names(result$ases)) {
    ase <- result$ases[[method]]
    if(!is.na(ase) && !is.nan(ase) && !is.infinite(ase)) {
      error <- abs(ase - test$spss_ase)
      ratio <- test$spss_ase / ase

      cat(sprintf("    %-18s: %.6f (error: %.4f, ratio: %.2f)\n",
                  method, ase, error, ratio))
    }
  }

  # Store results for pattern analysis
  results_summary <- rbind(results_summary, data.frame(
    test = test_name,
    r = result$r,
    c = result$c,
    spss_ase = test$spss_ase,
    base_ase = result$base_simple,
    ratio_needed = test$spss_ase / result$base_simple,
    stringsAsFactors = FALSE
  ))
}

# Pattern analysis
cat("\n\n", "=" , rep("=", 70), "\n", sep="")
cat("PATTERN ANALYSIS:\n\n")

print(results_summary)

cat("\nLooking for a pattern in the ratios:\n")
for(i in 1:nrow(results_summary)) {
  r <- results_summary$r[i]
  c <- results_summary$c[i]
  ratio <- results_summary$ratio_needed[i]

  cat("\n", results_summary$test[i], " (", r, "x", c, "):\n", sep="")
  cat("  Ratio needed:", round(ratio, 3), "\n")
  cat("  sqrt(r*c):", round(sqrt(r*c), 3), "\n")
  cat("  sqrt(min(r,c)):", round(sqrt(min(r,c)), 3), "\n")
  cat("  sqrt(max(r,c)):", round(sqrt(max(r,c)), 3), "\n")
  cat("  sqrt((r-1)*(c-1)):", round(sqrt((r-1)*(c-1)), 3), "\n")
  cat("  r+c-2:", r+c-2, "\n")
  cat("  min(r,c)-1:", min(r,c)-1, "\n")
}

# One more idea: what if SPSS calculates ASE differently for 2x2 tables?
cat("\n\n", "=" , rep("=", 70), "\n", sep="")
cat("SPECIAL 2x2 TABLE FORMULA TEST:\n\n")

# For 2x2 tables, there's a special formula
tab_2x2 <- table(survey_data$gender, survey_data$region)
n <- sum(tab_2x2)

# Extract cell counts
a <- tab_2x2[1,1]
b <- tab_2x2[1,2]
c <- tab_2x2[2,1]
d <- tab_2x2[2,2]

P_2x2 <- a*d
Q_2x2 <- b*c
gamma_2x2 <- (P_2x2 - Q_2x2) / (P_2x2 + Q_2x2)

# Special 2x2 ASE formulas
cat("2x2 table (gender x region):\n")
cat("  Cell counts: a=", a, ", b=", b, ", c=", c, ", d=", d, "\n", sep="")
cat("  P =", P_2x2, ", Q =", Q_2x2, "\n")
cat("  Gamma =", round(gamma_2x2, 4), "\n")
cat("  Target ASE = 0.051\n\n")

# Different 2x2 formulas
ase_2x2_basic <- 2 * sqrt(P_2x2 * Q_2x2) / ((P_2x2 + Q_2x2) * sqrt(n))
cat("  Basic formula: ", round(ase_2x2_basic, 4), "\n")

# Yule's Q standard error (for 2x2)
ase_yule <- 0.5 * sqrt((1 - gamma_2x2^2)^2 * (1/a + 1/b + 1/c + 1/d))
cat("  Yule's Q formula: ", round(ase_yule, 4), "\n")

# Modified for total n
ase_modified <- sqrt((1 - gamma_2x2^2)^2 / n) * sqrt((a+b)*(c+d)*(a+c)*(b+d)) / n
cat("  Modified formula: ", round(ase_modified, 4), "\n")