# ============================================================================
# GAMMA ASE TESTING - PART 6 (FIXED)
# Testing Yule's Q formula for 2x2 and exploring extensions for larger tables
# ============================================================================

library(dplyr)
library(SurveyStat)
data(survey_data)

# Calculate ASE using different methods based on table size
calculate_ase_by_table_size <- function(tab) {
  r <- nrow(tab)
  c <- ncol(tab)
  n <- sum(tab)

  # Calculate P and Q with proper bounds checking
  P <- 0
  Q <- 0
  for(i in 1:r) {
    for(j in 1:c) {
      n1 <- tab[i,j]
      if(n1 > 0) {
        # Concordant pairs
        if(i < r && j < c) {
          for(i2 in seq(i+1, r, by=1)) {
            for(j2 in seq(j+1, c, by=1)) {
              P <- P + n1 * tab[i2, j2]
            }
          }
        }
        # Discordant pairs
        if(i < r && j > 1) {
          for(i2 in seq(i+1, r, by=1)) {
            for(j2 in seq(1, j-1, by=1)) {
              Q <- Q + n1 * tab[i2, j2]
            }
          }
        }
      }
    }
  }

  gamma <- (P - Q) / (P + Q)

  ases <- list()

  # For 2x2 tables, use Yule's Q formula
  if(r == 2 && c == 2) {
    a <- tab[1,1]
    b <- tab[1,2]
    c_val <- tab[2,1]  # renamed to avoid conflict with c()
    d <- tab[2,2]

    # Original Yule's Q formula - THIS MATCHES SPSS!
    ases$yule_original <- 0.5 * sqrt((1 - gamma^2)^2 * (1/a + 1/b + 1/c_val + 1/d))

    # Alternative Yule formulation
    ases$yule_alt <- sqrt((1 - gamma^2)^2 / 4 * (1/a + 1/b + 1/c_val + 1/d))

    # Davis (1967) formula for 2x2
    # Convert to numeric to avoid integer overflow
    var_gamma <- (1 - gamma^2)^2 * (as.numeric(a)*d + as.numeric(b)*c_val) /
                 (as.numeric(a)*b*c_val*d)
    ases$davis_2x2 <- sqrt(var_gamma)

    # Goodman-Kruskal exact for 2x2
    ases$gk_exact_2x2 <- sqrt((1 - gamma^2)^2 * (1/a + 1/b + 1/c_val + 1/d) / 4)

  } else {
    # For larger tables, we need different formulas

    # Method 1: Simple generalization of base formula
    ases$base_simple <- 2 * sqrt(P * Q) / ((P + Q) * sqrt(n))

    # Method 2: Adjust by table size
    # This seems to work for some cases
    adjustment_factor <- sqrt(min(r, c))
    ases$adjusted_base <- 2 * sqrt(P * Q) / ((P + Q) * sqrt(n)) * adjustment_factor

    # Method 3: Use effective degrees of freedom
    df <- (r - 1) * (c - 1)
    ases$df_adjusted <- 2 * sqrt(P * Q) / ((P + Q) * sqrt(n)) * sqrt(df)

    # Method 4: Davis-style calculation
    davis_sum <- 0
    for(i in 1:r) {
      for(j in 1:c) {
        n_ij <- tab[i, j]
        if(n_ij > 0) {
          P_ij <- 0
          Q_ij <- 0

          # Concordant pairs for this cell
          if(i < r && j < c) {
            for(i2 in seq(i+1, r, by=1)) {
              for(j2 in seq(j+1, c, by=1)) {
                P_ij <- P_ij + tab[i2, j2]
              }
            }
          }

          # Discordant pairs for this cell
          if(i < r && j > 1) {
            for(i2 in seq(i+1, r, by=1)) {
              for(j2 in seq(1, j-1, by=1)) {
                Q_ij <- Q_ij + tab[i2, j2]
              }
            }
          }

          diff <- P_ij - Q_ij - gamma * (P_ij + Q_ij)
          davis_sum <- davis_sum + n_ij * diff^2
        }
      }
    }

    # Davis formula
    if(P + Q > 0) {
      ases$davis_general <- sqrt(16 * davis_sum / n^3) / (P + Q)

      # Try with different scaling
      ases$davis_scaled <- sqrt(16 * davis_sum / n^3) / (P + Q) * sqrt((r * c) / 4)
    }
  }

  return(list(
    gamma = gamma,
    P = P,
    Q = Q,
    n = n,
    r = r,
    c = c,
    ases = ases
  ))
}

# Test cases
test_cases <- list(
  test1a = list(vars = c("gender", "region"), spss_ase = 0.051),
  test1b = list(vars = c("education", "employment"), spss_ase = 0.028),
  test1c = list(vars = c("gender", "education"), spss_ase = 0.030)
)

cat("Testing ASE formulas with proper bounds checking:\n")
cat("=" , rep("=", 70), "\n", sep="")

best_methods <- data.frame()

for(test_name in names(test_cases)) {
  test <- test_cases[[test_name]]
  tab <- table(survey_data[[test$vars[1]]], survey_data[[test$vars[2]]])

  result <- calculate_ase_by_table_size(tab)

  cat("\n", test_name, " (", result$r, "x", result$c, " table):\n", sep="")
  cat("  Gamma =", round(result$gamma, 4), "\n")
  cat("  SPSS ASE =", test$spss_ase, "\n\n")

  cat("  ASE calculations:\n")

  best_error <- Inf
  best_method <- ""
  best_ase <- NA

  for(method in names(result$ases)) {
    ase <- result$ases[[method]]
    if(!is.na(ase) && !is.nan(ase) && !is.infinite(ase)) {
      error <- abs(ase - test$spss_ase)
      ratio <- ase / test$spss_ase

      if(error < best_error) {
        best_error <- error
        best_method <- method
        best_ase <- ase
      }

      # Mark the closest match
      marker <- if(error < 0.002) " *** MATCH!" else if(error < 0.01) " **" else ""

      cat(sprintf("    %-18s: %.6f (error: %.6f, ratio: %.3f)%s\n",
                  method, ase, error, ratio, marker))
    }
  }

  best_methods <- rbind(best_methods, data.frame(
    test = test_name,
    table_size = paste0(result$r, "x", result$c),
    spss_ase = test$spss_ase,
    best_method = best_method,
    best_ase = best_ase,
    best_error = best_error,
    stringsAsFactors = FALSE
  ))
}

cat("\n\n", "=" , rep("=", 70), "\n", sep="")
cat("SUMMARY OF BEST MATCHES:\n\n")
print(best_methods)

cat("\n\n", "=" , rep("=", 70), "\n", sep="")
cat("ANALYSIS:\n\n")

for(i in 1:nrow(best_methods)) {
  cat(best_methods$test[i], ":\n")
  if(best_methods$best_error[i] < 0.002) {
    cat("  ✓ EXCELLENT MATCH using", best_methods$best_method[i], "\n")
  } else if(best_methods$best_error[i] < 0.01) {
    cat("  ~ Good match using", best_methods$best_method[i], "\n")
  } else {
    cat("  ✗ No close match found, best was", best_methods$best_method[i], "\n")
  }
  cat("  Error:", round(best_methods$best_error[i], 6), "\n\n")
}

# If we found Yule works for 2x2, let's verify it precisely
cat("\n", "=" , rep("=", 70), "\n", sep="")
cat("VERIFICATION FOR 2x2 TABLE (test1a):\n\n")

tab_2x2 <- table(survey_data$gender, survey_data$region)
a <- tab_2x2[1,1]
b <- tab_2x2[1,2]
c_val <- tab_2x2[2,1]
d <- tab_2x2[2,2]
P_2x2 <- a*d
Q_2x2 <- b*c_val
gamma_2x2 <- (P_2x2 - Q_2x2) / (P_2x2 + Q_2x2)

yule_ase <- 0.5 * sqrt((1 - gamma_2x2^2)^2 * (1/a + 1/b + 1/c_val + 1/d))

cat("Cell values: a=", a, ", b=", b, ", c=", c_val, ", d=", d, "\n", sep="")
cat("P =", P_2x2, ", Q =", Q_2x2, "\n")
cat("Gamma =", round(gamma_2x2, 6), "\n")
cat("Yule's Q ASE =", round(yule_ase, 6), "\n")
cat("SPSS ASE = 0.051\n")
cat("Match quality: ", if(abs(yule_ase - 0.051) < 0.002) "EXCELLENT!" else "needs work", "\n")