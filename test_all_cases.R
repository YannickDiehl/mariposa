library(SurveyStat)
library(dplyr)
data(survey_data)

test_gamma_ase <- function(var1, var2, spss_ase, spss_p, test_name) {
  tab <- table(survey_data[[var1]], survey_data[[var2]])
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
  
  cat("\n", test_name, " (", r, "x", c, "):\n", sep="")
  cat("  Gamma =", round(gamma, 4), "\n")
  cat("  Base ASE =", round(base_ase, 5), "\n")
  cat("  SPSS ASE =", spss_ase, "\n")
  cat("  Ratio needed =", round(spss_ase / base_ase, 3), "\n")
  
  # Current formula
  adjustment_factor <- sqrt(min(r, c))
  if (r * c > 12 && adjustment_factor > 1.5) {
    ase_current <- base_ase * sqrt(1 + (adjustment_factor - 1) * 0.5)
  } else {
    ase_current <- base_ase * adjustment_factor
  }
  
  t_current <- gamma / ase_current
  p_current <- 2 * (1 - pnorm(abs(t_current)))
  
  cat("  Current ASE =", round(ase_current, 5), "\n")
  cat("  Current p =", round(p_current, 3), " (SPSS:", spss_p, ")\n")
}

cat("===== GAMMA P-VALUE VALIDATION ANALYSIS =====\n")

# Test cases
test_gamma_ase("gender", "region", 0.051, 0.520, "Test 1a")
test_gamma_ase("education", "employment", 0.028, 0.020, "Test 1b")  
test_gamma_ase("gender", "education", 0.030, 0.786, "Test 1c")
