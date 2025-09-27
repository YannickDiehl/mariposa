library(SurveyStat)
library(dplyr)
data(survey_data)

# Test 1a: 2x2 table
tab <- table(survey_data$gender, survey_data$region)
r <- nrow(tab)
c <- ncol(tab)
n <- sum(tab)

cat("Table dimensions:", r, "x", c, "\n")

# Extract cells for 2x2
a <- tab[1,1]
b <- tab[1,2]
c_val <- tab[2,1]
d <- tab[2,2]

cat("Cell values: a=", a, ", b=", b, ", c=", c_val, ", d=", d, "\n")

# Calculate P and Q
P <- a * d
Q <- b * c_val
gamma <- (P - Q) / (P + Q)

cat("P =", P, ", Q =", Q, "\n")
cat("Gamma =", gamma, "\n")

# Yule's Q formula
if (a > 0 && b > 0 && c_val > 0 && d > 0) {
  yule_ase <- 0.5 * sqrt((1 - gamma^2)^2 * (1/a + 1/b + 1/c_val + 1/d))
  cat("Yule's ASE =", yule_ase, "\n")
  
  t_yule <- gamma / yule_ase
  p_yule <- 2 * (1 - pnorm(abs(t_yule)))
  cat("Yule's t-stat =", t_yule, "\n")
  cat("Yule's p-value =", p_yule, "\n")
} else {
  cat("Cannot calculate Yule's ASE - zero cells\n")
}

cat("\nSPSS ASE = 0.051\n")
cat("SPSS p-value = 0.520\n")

# Check what our current code would do
adjustment_factor <- sqrt(min(r, c))
base_ase <- 2 * sqrt(P * Q) / ((P + Q) * sqrt(n))
ase_current <- base_ase * adjustment_factor
cat("\nCurrent formula ASE =", ase_current, "\n")
