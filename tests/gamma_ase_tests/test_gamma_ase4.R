# ============================================================================
# GAMMA ASE TESTING - PART 4
# Testing the full Goodman-Kruskal ASE formula
# ============================================================================

library(dplyr)
library(SurveyStat)
data(survey_data)

# Full Goodman-Kruskal ASE formula
calculate_gamma_ase_full <- function(tab) {
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

  # Calculate the full ASE using different methods
  ases <- list()

  # Method 1: Siegel & Castellan (1988) formula
  # ASE = (2/(P+Q)) * sqrt(sum(n_ij * (Q*P_ij - P*Q_ij)^2) / n)
  sum1 <- 0
  for(i in 1:r) {
    for(j in 1:c) {
      n_ij <- tab[i, j]
      if(n_ij > 0) {
        P_ij <- 0
        Q_ij <- 0

        if(i < r && j < c) {
          for(i2 in (i+1):r) {
            for(j2 in (j+1):c) {
              P_ij <- P_ij + tab[i2, j2]
            }
          }
        }

        if(i < r && j > 1) {
          for(i2 in (i+1):r) {
            for(j2 in 1:(j-1)) {
              Q_ij <- Q_ij + tab[i2, j2]
            }
          }
        }

        diff1 <- Q * P_ij - P * Q_ij
        sum1 <- sum1 + n_ij * diff1^2
      }
    }
  }

  ases$siegel <- (2 / (P + Q)) * sqrt(sum1 / n^3)

  # Method 2: ASE under H0 (null hypothesis)
  # This is simpler: ASE0 = 4*sqrt(P*Q) / ((P+Q)^2 * sqrt(n))
  ases$h0_simple <- 4 * sqrt(P * Q) / ((P + Q)^2 * sqrt(n))

  # Method 3: Freeman (1987) - includes tied pairs
  # For ordinal variables with possible ties
  # We'll skip exact tied calculation for now but include structure
  ases$freeman_approx <- 2 * sqrt(P * Q) / ((P + Q) * sqrt(n))

  # Method 4: With finite population correction
  ases$finite_pop <- 2 * sqrt(P * Q) / ((P + Q) * sqrt(n - 1))

  # Method 5: What if there's a different n in denominator?
  # Using n^2 instead of n^3 in the Siegel formula
  ases$siegel_n2 <- (2 / (P + Q)) * sqrt(sum1 / n^2)

  # Method 6: What if the factor is different?
  ases$siegel_factor4 <- (4 / (P + Q)) * sqrt(sum1 / n^3)

  return(list(
    gamma = gamma,
    P = P,
    Q = Q,
    n = n,
    ases = ases
  ))
}

# Test cases
test_cases <- list(
  test1a = list(vars = c("gender", "region"), spss_ase = 0.051),
  test1b = list(vars = c("education", "employment"), spss_ase = 0.028),
  test1c = list(vars = c("gender", "education"), spss_ase = 0.030)
)

cat("Testing different ASE formulas:\n")
cat("=" , rep("=", 70), "\n", sep="")

for(test_name in names(test_cases)) {
  test <- test_cases[[test_name]]
  tab <- table(survey_data[[test$vars[1]]], survey_data[[test$vars[2]]])

  result <- calculate_gamma_ase_full(tab)

  cat("\n", test_name, " (", nrow(tab), "x", ncol(tab), " table):\n", sep="")
  cat("  Gamma =", round(result$gamma, 4), "\n")
  cat("  P =", result$P, ", Q =", result$Q, "\n")
  cat("  SPSS ASE =", test$spss_ase, "\n\n")

  cat("  ASE calculations:\n")
  for(method in names(result$ases)) {
    ase <- result$ases[[method]]
    error <- abs(ase - test$spss_ase)
    ratio <- test$spss_ase / ase

    cat(sprintf("    %-15s: %.6f (error: %.4f, ratio: %.2f)\n",
                method, ase, error, ratio))
  }
}

# Let's also check if multiplying by sqrt of table size helps
cat("\n\n", "=" , rep("=", 70), "\n", sep="")
cat("Testing if table dimensions affect the scaling:\n\n")

for(test_name in names(test_cases)) {
  test <- test_cases[[test_name]]
  tab <- table(survey_data[[test$vars[1]]], survey_data[[test$vars[2]]])

  result <- calculate_gamma_ase_full(tab)
  r <- nrow(tab)
  c <- ncol(tab)

  # Try the Siegel formula with different adjustments
  base_ase <- result$ases$siegel

  cat(test_name, " (", r, "x", c, "):\n", sep="")
  cat("  Base Siegel ASE:", round(base_ase, 6), "\n")
  cat("  SPSS ASE:", test$spss_ase, "\n")
  cat("  Ratio needed:", round(test$spss_ase / base_ase, 3), "\n")

  # Test different multipliers
  cat("  With adjustments:\n")
  cat("    * sqrt(r*c):", round(base_ase * sqrt(r*c), 4),
      "  ratio:", round(base_ase * sqrt(r*c) / test$spss_ase, 2), "\n")
  cat("    * r*c:", round(base_ase * r*c, 4),
      "  ratio:", round(base_ase * r*c / test$spss_ase, 2), "\n")
  cat("    * (r+c)/2:", round(base_ase * (r+c)/2, 4),
      "  ratio:", round(base_ase * (r+c)/2 / test$spss_ase, 2), "\n\n")
}