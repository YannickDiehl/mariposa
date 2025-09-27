# ============================================================================
# GAMMA ASE TESTING SCRIPT
# Goal: Find the exact formula SPSS uses for Gamma's ASE
# ============================================================================

library(dplyr)
library(SurveyStat)
data(survey_data)

# SPSS Reference Values
spss_tests <- list(
  test1a = list(
    vars = c("gender", "region"),
    gamma = 0.033, ase = 0.051, t = 0.644, p = 0.520,
    n = 2500, cramers_v_p = 0.519
  ),
  test1b = list(
    vars = c("education", "employment"),
    gamma = -0.065, ase = 0.028, t = -2.321, p = 0.020,
    n = 2500, cramers_v_p = 0.000
  ),
  test1c = list(
    vars = c("gender", "education"),
    gamma = -0.008, ase = 0.030, t = -0.272, p = 0.786,
    n = 2500, cramers_v_p = 0.325
  )
)

# Function to calculate P and Q for a table
calculate_PQ <- function(tab) {
  r <- nrow(tab)
  c <- ncol(tab)
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

  return(list(P = P, Q = Q))
}

# Test different ASE formulas
test_ase_formulas <- function(test_name, spss_ref) {
  cat("\n========", test_name, "========\n")

  # Get the table
  var1 <- spss_ref$vars[1]
  var2 <- spss_ref$vars[2]
  tab <- table(survey_data[[var1]], survey_data[[var2]])

  r <- nrow(tab)
  c <- ncol(tab)
  n <- sum(tab)

  # Calculate P and Q
  pq <- calculate_PQ(tab)
  P <- pq$P
  Q <- pq$Q
  gamma <- (P - Q) / (P + Q)

  cat("Table:", r, "x", c, "\n")
  cat("N =", n, "\n")
  cat("P =", P, ", Q =", Q, "\n")
  cat("Gamma =", round(gamma, 4), "(SPSS:", spss_ref$gamma, ")\n")
  cat("Target ASE =", spss_ref$ase, "\n\n")

  # Test different ASE formulas
  formulas <- list()

  # Formula 1: Basic
  formulas$basic <- 2 * sqrt(P * Q) / ((P + Q) * sqrt(n))

  # Formula 2: With n-1
  formulas$n_minus_1 <- 2 * sqrt(P * Q) / ((P + Q) * sqrt(n - 1))

  # Formula 3: Different scaling
  formulas$alt_scaling <- sqrt(4 * P * Q / n) / (P + Q)

  # Formula 4: With total pairs consideration
  total_pairs <- n * (n - 1) / 2
  formulas$total_pairs <- 2 * sqrt(P * Q / total_pairs) / (P + Q)

  # Formula 5: Proportional approach
  prop_p <- P / (P + Q)
  prop_q <- Q / (P + Q)
  formulas$proportional <- 2 * sqrt(prop_p * prop_q / n)

  # Formula 6: With finite population correction
  formulas$finite_pop <- 2 * sqrt(P * Q) / ((P + Q) * sqrt(n)) * sqrt((n - 1) / n)

  # Formula 7: Cell-based variance (simplified)
  # This would need the full cell calculation but let's try an approximation
  formulas$cell_approx <- spss_ref$ase  # Placeholder to see the target

  # Print results
  cat("ASE Formula Tests:\n")
  for(name in names(formulas)) {
    ase <- formulas[[name]]
    if(name != "cell_approx") {
      error <- abs(ase - spss_ref$ase)
      ratio <- spss_ref$ase / ase
      cat(sprintf("  %-15s: ASE = %.4f, Error = %.4f, Ratio = %.2f\n",
                  name, ase, error, ratio))
    }
  }

  # What scaling factor would we need?
  base_ase <- 2 * sqrt(P * Q) / ((P + Q) * sqrt(n))
  needed_factor <- spss_ref$ase / base_ase
  cat("\nScaling factor needed for basic formula:", round(needed_factor, 3), "\n")

  # Test if it's related to degrees of freedom
  df1 <- (r - 1) * (c - 1)
  df2 <- n - 2
  df3 <- min(r - 1, c - 1)

  cat("\nPossible df relationships:\n")
  cat("  (r-1)*(c-1) =", df1, ", sqrt(df1/n) =", round(sqrt(df1/n), 3), "\n")
  cat("  n-2 =", df2, "\n")
  cat("  min(r-1, c-1) =", df3, "\n")
}

# Run tests for each case
for(test_name in names(spss_tests)) {
  test_ase_formulas(test_name, spss_tests[[test_name]])
}

# Look for patterns in the scaling factors
cat("\n\n======== PATTERN ANALYSIS ========\n")
cat("Looking for patterns in what makes SPSS ASE values...\n\n")

# Calculate what multiplier would fix our basic ASE for each test
for(test_name in names(spss_tests)) {
  spss_ref <- spss_tests[[test_name]]
  var1 <- spss_ref$vars[1]
  var2 <- spss_ref$vars[2]
  tab <- table(survey_data[[var1]], survey_data[[var2]])

  pq <- calculate_PQ(tab)
  P <- pq$P
  Q <- pq$Q
  n <- sum(tab)

  base_ase <- 2 * sqrt(P * Q) / ((P + Q) * sqrt(n))
  multiplier <- spss_ref$ase / base_ase

  cat(test_name, ": Need to multiply by", round(multiplier, 3), "\n")
  cat("  Table size:", nrow(tab), "x", ncol(tab), "\n")
  cat("  P/(P+Q) =", round(P/(P+Q), 3), "\n")
  cat("  Q/(P+Q) =", round(Q/(P+Q), 3), "\n\n")
}