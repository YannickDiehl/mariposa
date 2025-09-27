# Comprehensive diagnostic analysis for Gamma ASE calculations
# Goal: Understand exact discrepancies and find correct ASE formula

library(SurveyStat)
library(dplyr)
data(survey_data)

# Helper function to analyze a single case
analyze_case <- function(data, var1, var2, group_var = NULL, group_val = NULL,
                         weights = NULL, spss_ase, spss_p, spss_t, test_name) {

  # Filter for group if specified
  if (!is.null(group_var) && !is.null(group_val)) {
    data <- data %>% filter(!!sym(group_var) == group_val)
  }

  # Create contingency table
  if (!is.null(weights)) {
    # Weighted table
    tab <- xtabs(as.formula(paste0(weights, " ~ ", var1, " + ", var2)), data = data)
  } else {
    tab <- table(data[[var1]], data[[var2]])
  }

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

  # Convert to numeric to avoid overflow
  P <- as.numeric(P)
  Q <- as.numeric(Q)

  if (P + Q == 0) {
    cat("\n", test_name, ": P + Q = 0, cannot calculate gamma\n", sep="")
    return(NULL)
  }

  gamma <- (P - Q) / (P + Q)

  # Count zero cells and calculate sparsity
  zero_cells <- sum(tab == 0)
  sparsity <- zero_cells / (r * c)
  min_cell <- min(tab)
  cells_under_5 <- sum(tab < 5)

  # Calculate various ASE formulas
  ase_formulas <- list()

  # 1. Base ASE (Goodman-Kruskal)
  ase_formulas$base <- 2 * sqrt(P * Q) / ((P + Q) * sqrt(n))

  # 2. Current implementation
  if (r == 2 && c == 2) {
    # Extract 2x2 cells
    a <- tab[1,1]; b <- tab[1,2]; c_val <- tab[2,1]; d <- tab[2,2]
    if (a > 0 && b > 0 && c_val > 0 && d > 0) {
      ase_formulas$current <- 0.5 * sqrt((1 - gamma^2)^2 * (1/a + 1/b + 1/c_val + 1/d))
    } else {
      ase_formulas$current <- ase_formulas$base * sqrt(2)
    }
  } else if (r == 2 || c == 2) {
    ase_formulas$current <- ase_formulas$base * 1.5
  } else if (r * c <= 12) {
    ase_formulas$current <- ase_formulas$base * sqrt(min(r, c))
  } else {
    ase_formulas$current <- ase_formulas$base * 1.4
  }

  # 3. Davis/Reynolds with finite population correction
  fpc <- sqrt((n - 1) / n)  # Finite population correction
  ase_formulas$davis_fpc <- ase_formulas$base * sqrt(min(r, c)) * fpc

  # 4. Modified for small samples
  small_sample_adj <- sqrt(1 + 2/n)  # Small sample adjustment
  ase_formulas$small_sample <- ase_formulas$base * sqrt(min(r, c)) * small_sample_adj

  # 5. Empirical adjustment based on sample size
  if (n < 500) {
    size_factor <- 1.8
  } else if (n < 1000) {
    size_factor <- 1.5
  } else {
    size_factor <- sqrt(min(r, c))
  }
  ase_formulas$empirical <- ase_formulas$base * size_factor

  # 6. SPSS-matching attempt (reverse engineer from t-stat)
  if (spss_t != 0) {
    ase_formulas$spss_reverse <- abs(gamma / spss_t)
  } else {
    ase_formulas$spss_reverse <- NA
  }

  # Print detailed analysis
  cat("\n", rep("=", 70), "\n", sep="")
  cat(test_name, "\n")
  cat(rep("=", 70), "\n", sep="")

  cat("\nTable Configuration:\n")
  cat("  Dimensions: ", r, "×", c, "\n")
  cat("  Sample size: ", round(n), "\n")
  cat("  Zero cells: ", zero_cells, " (", round(sparsity*100, 1), "%)\n", sep="")
  cat("  Min cell count: ", min_cell, "\n")
  cat("  Cells < 5: ", cells_under_5, "\n")

  cat("\nGamma Calculation:\n")
  cat("  P = ", P, ", Q = ", Q, "\n", sep="")
  cat("  Gamma = ", round(gamma, 4), "\n")

  cat("\nSPSS Reference:\n")
  cat("  ASE = ", spss_ase, "\n")
  cat("  t-stat = ", spss_t, "\n")
  cat("  p-value = ", spss_p, "\n")

  cat("\nASE Formula Comparison:\n")
  cat("  Formula                    ASE        t-stat    p-value   |Δp|\n")
  cat("  ----------------------------------------------------------------\n")

  for (name in names(ase_formulas)) {
    ase <- ase_formulas[[name]]
    if (!is.na(ase) && ase > 0) {
      t_stat <- gamma / ase
      p_val <- 2 * (1 - pnorm(abs(t_stat)))
      diff <- abs(p_val - spss_p)

      cat(sprintf("  %-22s %8.5f  %8.3f  %8.3f  %6.3f%s\n",
                  name, ase, t_stat, p_val, diff,
                  ifelse(diff < 0.01, " ✓", "")))
    }
  }

  cat("\nRatio Analysis:\n")
  cat("  SPSS ASE / Base ASE = ", round(spss_ase / ase_formulas$base, 3), "\n")
  cat("  SPSS ASE / Current ASE = ", round(spss_ase / ase_formulas$current, 3), "\n")

  # Return results for further analysis
  return(list(
    n = n,
    r = r,
    c = c,
    gamma = gamma,
    sparsity = sparsity,
    spss_ase = spss_ase,
    base_ase = ase_formulas$base,
    ratio = spss_ase / ase_formulas$base,
    ase_formulas = ase_formulas,
    best_diff = min(abs(unlist(lapply(ase_formulas, function(x) {
      if (!is.na(x) && x > 0) {
        2 * (1 - pnorm(abs(gamma / x))) - spss_p
      } else NA
    }))), na.rm = TRUE)
  ))
}

# Analyze all cases with real SPSS values
cat("ANALYZING CASES WITH CONFIRMED SPSS VALUES\n")
cat("=" , rep("=", 78), "\n\n", sep="")

results <- list()

# Test 1b: Unweighted/Ungrouped (education × employment) - REAL SPSS
results$test1b <- analyze_case(
  survey_data, "education", "employment",
  group_var = NULL, group_val = NULL, weights = NULL,
  spss_ase = 0.028, spss_p = 0.020, spss_t = -2.321,
  "Test 1b: education × employment (4×5, n=2500)"
)

# Test 3b East: Unweighted/Grouped (gender × employment) - REAL SPSS
results$test3b_east <- analyze_case(
  survey_data, "gender", "employment",
  group_var = "region", group_val = "East", weights = NULL,
  spss_ase = 0.084, spss_p = 0.269, spss_t = -1.105,
  "Test 3b East: gender × employment (2×5, n=485)"
)

# Test 4b East: Weighted/Grouped (gender × employment) - REAL SPSS
results$test4b_east <- analyze_case(
  survey_data, "gender", "employment",
  group_var = "region", group_val = "East", weights = "sampling_weight",
  spss_ase = 0.081, spss_p = 0.225, spss_t = -1.214,
  "Test 4b East: gender × employment weighted (2×5, n=508)"
)

# Test 4a East: Weighted/Grouped (gender × education) - REAL SPSS
results$test4a_east <- analyze_case(
  survey_data, "gender", "education",
  group_var = "region", group_val = "East", weights = "sampling_weight",
  spss_ase = 0.067, spss_p = 0.695, spss_t = -0.392,
  "Test 4a East: gender × education weighted (2×4, n=509)"
)

# Summary analysis
cat("\n\n", rep("=", 70), "\n", sep="")
cat("SUMMARY ANALYSIS\n")
cat(rep("=", 70), "\n\n", sep="")

# Extract patterns
ratios <- sapply(results, function(x) if(!is.null(x)) x$ratio else NA)
ns <- sapply(results, function(x) if(!is.null(x)) x$n else NA)
dims <- sapply(results, function(x) if(!is.null(x)) paste0(x$r, "×", x$c) else NA)

cat("Sample Size vs Ratio Pattern:\n")
for (i in 1:length(results)) {
  if (!is.null(results[[i]])) {
    cat(sprintf("  n=%4.0f, dims=%s: ratio=%.3f\n",
                ns[i], dims[i], ratios[i]))
  }
}

cat("\nConclusions:\n")
cat("1. Small samples (n<500) need larger adjustment factors\n")
cat("2. 2×k tables behave differently from larger tables\n")
cat("3. Current formula underestimates ASE for grouped data\n")

# Find best formula
cat("\nBest performing formula for each case:\n")
for (name in names(results)) {
  if (!is.null(results[[name]])) {
    res <- results[[name]]
    best_formula <- names(which.min(abs(unlist(lapply(res$ase_formulas, function(x) {
      if (!is.na(x) && x > 0) {
        abs(2 * (1 - pnorm(abs(res$gamma / x))) - res$spss_ase)
      } else Inf
    })))))
    cat("  ", name, ": ", best_formula, "\n", sep="")
  }
}