# ============================================================================
# GAMMA ASE TESTING - PART 2
# Exploring the relationship between scaling factors and table dimensions
# ============================================================================

library(dplyr)
library(SurveyStat)
data(survey_data)

# Function to calculate everything for a test
analyze_test <- function(var1_name, var2_name, spss_ase, spss_gamma) {
  tab <- table(survey_data[[var1_name]], survey_data[[var2_name]])
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
  base_ase <- 2 * sqrt(P * Q) / ((P + Q) * sqrt(n))
  scaling_needed <- spss_ase / base_ase

  # Calculate variance sum for Davis formula
  var_sum <- 0
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

        diff <- P_ij - Q_ij - gamma * (P_ij + Q_ij)
        var_sum <- var_sum + n_ij * diff * diff
      }
    }
  }

  # Test different dimension-based adjustments
  adjustments <- list(
    sqrt_min = sqrt(min(r, c)),
    sqrt_min_minus_1 = sqrt(min(r-1, c-1) + 1),
    sqrt_rc = sqrt(r * c / 4),
    sqrt_df = sqrt((r-1) * (c-1) + 1),
    harmonic_mean = 2 * r * c / (r + c)
  )

  result <- data.frame(
    table_size = paste0(r, "x", c),
    r = r,
    c = c,
    n = n,
    P = P,
    Q = Q,
    gamma = gamma,
    spss_ase = spss_ase,
    base_ase = round(base_ase, 4),
    scaling_needed = round(scaling_needed, 3),
    var_sum = var_sum
  )

  for(name in names(adjustments)) {
    result[[name]] <- adjustments[[name]]
  }

  return(result)
}

# Analyze all test cases
results <- rbind(
  analyze_test("gender", "region", 0.051, 0.033),          # 2x2
  analyze_test("education", "employment", 0.028, -0.065),  # 4x5
  analyze_test("gender", "education", 0.030, -0.008)       # 2x4
)

print(results[, c("table_size", "spss_ase", "base_ase", "scaling_needed")])

cat("\n\nTrying dimension-based adjustments:\n")
for(i in 1:nrow(results)) {
  cat("\n", results$table_size[i], "table:\n")
  cat("  Need scaling factor:", results$scaling_needed[i], "\n")
  cat("  sqrt(min(r,c)):", round(results$sqrt_min[i], 3), "\n")
  cat("  sqrt(min(r-1,c-1)+1):", round(results$sqrt_min_minus_1[i], 3), "\n")
  cat("  sqrt(r*c/4):", round(results$sqrt_rc[i], 3), "\n")
  cat("  sqrt((r-1)*(c-1)+1):", round(results$sqrt_df[i], 3), "\n")
}

# Try a different approach: What if SPSS uses the ASE formula differently?
cat("\n\n======== TESTING HYPOTHESIS ========\n")
cat("What if SPSS multiplies by sqrt(min(r,c)) or similar?\n\n")

for(i in 1:nrow(results)) {
  test_ase <- results$base_ase[i] * sqrt(min(results$r[i], results$c[i]))
  cat(results$table_size[i], ": base_ase * sqrt(min(r,c)) =",
      round(test_ase, 4), "vs SPSS =", results$spss_ase[i], "\n")
}

# Another hypothesis: cell-count based adjustment
cat("\n\nWhat about adjustments based on effective sample size?\n")
for(i in 1:nrow(results)) {
  # Effective sample size for contingency tables
  eff_n <- results$n[i] / (results$r[i] * results$c[i])
  adj_factor <- sqrt(results$n[i] / eff_n) / results$n[i]

  cat(results$table_size[i], ": Effective n adjustment factor =",
      round(adj_factor, 3), "\n")
}

# Test if it's related to Cramer's V correction
cat("\n\nTesting Cramer's V related adjustment:\n")
for(i in 1:nrow(results)) {
  cramers_denominator <- sqrt(min(results$r[i] - 1, results$c[i] - 1))
  adj_ase <- results$base_ase[i] * sqrt((results$r[i] * results$c[i]) /
                                         (min(results$r[i], results$c[i])))

  cat(results$table_size[i], ": Adjusted ASE =", round(adj_ase, 4),
      "vs SPSS =", results$spss_ase[i], "\n")
}