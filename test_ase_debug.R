library(SurveyStat)
library(dplyr)
data(survey_data)

# Test 1b: education x employment (4x5)
tab <- table(survey_data$education, survey_data$employment)
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
adjustment_factor <- sqrt(min(r, c))

cat("Table dimensions:", r, "x", c, "\n")
cat("n =", n, "\n")
cat("P =", P, ", Q =", Q, "\n")
cat("Gamma =", gamma, "\n")
cat("Base ASE =", base_ase, "\n")
cat("Adjustment factor =", adjustment_factor, "\n")

# Current formula for large tables
if (r * c > 12 && adjustment_factor > 1.5) {
  ase_current <- base_ase * sqrt(1 + (adjustment_factor - 1) * 0.5)
  cat("Current ASE (with reduction) =", ase_current, "\n")
} else {
  ase_current <- base_ase * adjustment_factor
  cat("Current ASE (standard) =", ase_current, "\n")
}

# What ASE do we need for SPSS?
spss_ase <- 0.028
cat("\nSPSS ASE =", spss_ase, "\n")
cat("Ratio needed =", spss_ase / base_ase, "\n")

# Calculate t-stat with our ASE
t_stat_ours <- gamma / ase_current
p_val_ours <- 2 * (1 - pnorm(abs(t_stat_ours)))
cat("\nOur t-stat =", t_stat_ours, "\n")
cat("Our p-value =", p_val_ours, "\n")

# SPSS values
cat("\nSPSS t-stat = -2.321\n")
cat("SPSS p-value = 0.020\n")
