# ============================================================================
# GAMMA ASE TESTING - PART 6
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

  ases <- list()

  # For 2x2 tables, use Yule's Q formula
  if(r == 2 && c == 2) {
    a <- tab[1,1]
    b <- tab[1,2]
    c <- tab[2,1]
    d <- tab[2,2]

    # Original Yule's Q formula
    ases$yule_original <- 0.5 * sqrt((1 - gamma^2)^2 * (1/a + 1/b + 1/c + 1/d))

    # Alternative Yule formulation
    ases$yule_alt <- sqrt((1 - gamma^2)^2 / 4 * (1/a + 1/b + 1/c + 1/d))

    # Davis (1967) formula for 2x2
    # Convert to numeric to avoid integer overflow
    var_gamma <- (1 - gamma^2)^2 * (as.numeric(a)*d + as.numeric(b)*c) / (as.numeric(a)*b*c*d)
    ases$davis_2x2 <- sqrt(var_gamma)

    # Goodman-Kruskal exact for 2x2
    ases$gk_exact_2x2 <- sqrt((1 - gamma^2)^2 * (1/a + 1/b + 1/c + 1/d) / 4)

  }

  # For larger tables, try generalizations of Yule's formula
  # Calculate cell-based variance components
  var_sum <- 0
  cell_weight_sum <- 0

  for(i in 1:r) {
    for(j in 1:c) {
      if(tab[i,j] > 0) {
        # Count concordant and discordant pairs for this cell
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

        # Contribution to variance (generalized Yule approach)
        weight <- 1 / tab[i,j]
        diff <- P_ij - Q_ij - gamma * (P_ij + Q_ij)

        var_sum <- var_sum + weight * diff^2
        cell_weight_sum <- cell_weight_sum + weight
      }
    }
  }

  # Generalized Yule-like formula
  ases$yule_generalized <- sqrt((1 - gamma^2)^2 * cell_weight_sum / (4 * (r-1) * (c-1)))

  # Modified generalization
  ases$modified_gen <- sqrt(var_sum / n^2) * sqrt((r * c) / 4)

  # Another approach: scale Yule formula by table dimensions
  # Average cell reciprocal
  avg_reciprocal <- 0
  cell_count <- 0
  for(i in 1:r) {
    for(j in 1:c) {
      if(tab[i,j] > 0) {
        avg_reciprocal <- avg_reciprocal + 1/tab[i,j]
        cell_count <- cell_count + 1
      }
    }
  }
  avg_reciprocal <- avg_reciprocal / cell_count

  ases$scaled_yule <- 0.5 * sqrt((1 - gamma^2)^2 * avg_reciprocal)

  # Davis formula generalized for rxc tables
  # This is what might work for larger tables
  davis_sum <- 0
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
        davis_sum <- davis_sum + n_ij * diff^2
      }
    }
  }

  # Davis generalized formula
  ases$davis_general <- sqrt(16 * davis_sum / n^3) / (P + Q)

  # Try adjusting by degrees of freedom
  ases$davis_df <- sqrt(16 * davis_sum / n^3) / (P + Q) * sqrt((r-1)*(c-1))

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

cat("Testing Yule's Q and generalizations:\n")
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

  for(method in names(result$ases)) {
    ase <- result$ases[[method]]
    if(!is.na(ase) && !is.nan(ase) && !is.infinite(ase)) {
      error <- abs(ase - test$spss_ase)
      ratio <- ase / test$spss_ase

      if(error < best_error) {
        best_error <- error
        best_method <- method
      }

      # Mark the closest match
      marker <- if(error < 0.002) " ***" else if(error < 0.01) " **" else ""

      cat(sprintf("    %-18s: %.6f (error: %.4f, ratio: %.2f)%s\n",
                  method, ase, error, ratio, marker))
    }
  }

  best_methods <- rbind(best_methods, data.frame(
    test = test_name,
    table_size = paste0(result$r, "x", result$c),
    best_method = best_method,
    best_error = best_error,
    stringsAsFactors = FALSE
  ))
}

cat("\n\n", "=" , rep("=", 70), "\n", sep="")
cat("BEST METHODS SUMMARY:\n\n")
print(best_methods)

# Let's check if the Davis formula needs a specific adjustment
cat("\n\n", "=" , rep("=", 70), "\n", sep="")
cat("EXPLORING DAVIS FORMULA ADJUSTMENTS:\n\n")

for(test_name in names(test_cases)) {
  test <- test_cases[[test_name]]
  tab <- table(survey_data[[test$vars[1]]], survey_data[[test$vars[2]]])

  result <- calculate_ase_by_table_size(tab)

  if("davis_general" %in% names(result$ases)) {
    davis_val <- result$ases$davis_general
    if(!is.na(davis_val) && !is.nan(davis_val)) {
      needed_multiplier <- test$spss_ase / davis_val

      cat(test_name, " (", result$r, "x", result$c, "):\n", sep="")
      cat("  Davis general ASE:", round(davis_val, 4), "\n")
      cat("  SPSS ASE:", test$spss_ase, "\n")
      cat("  Multiplier needed:", round(needed_multiplier, 3), "\n")
      cat("  sqrt(min(r,c)):", round(sqrt(min(result$r, result$c)), 3), "\n")
      cat("  1/sqrt(min(r,c)):", round(1/sqrt(min(result$r, result$c)), 3), "\n\n")
    }
  }
}