# SPSS Validation Guide for SurveyStat

## Overview
Systematic approach to validate R functions against SPSS, ensuring identical statistical results for user confidence when migrating from SPSS to R.

## Core Principles

### 1. Understand What You're Comparing
- **Display vs Calculation**: SPSS shows rounded values but calculates with decimals internally
- **Percentage Bases**: Raw % uses total including missing, Valid % uses total excluding missing  
- **Weighted Paths**: Weighted and unweighted calculations often use completely different code paths

### 2. Three-Layer Validation
Always check three places when debugging mismatches:
1. **SPSS Output**: Is it internally consistent? Do parts sum to total?
2. **R Function**: Does it calculate correctly? Are missing values handled?
3. **Test Logic**: Does comparison use correct approach? Right denominator?

### 3. Tolerance Strategy
Choose tolerance based on value type and calculation method:
- **Counts/Frequencies**: Exact match (tolerance = 0)
- **Direct percentages**: ±0.1 (accounts for display rounding)
- **Recalculated values**: ±0.5 (compound rounding effects)
- **Weighted totals**: ±1 (SPSS display inconsistency)
- **Test statistics**: ±0.00001 (scientific precision)

## Implementation Steps

### Step 1: Generate SPSS Reference
Create comprehensive SPSS syntax testing all scenarios:

```spss
* Test 1: Unweighted/Ungrouped
FREQUENCIES VARIABLES=variable
  /STATISTICS=MEAN STDDEV SKEWNESS
  /ORDER=ANALYSIS.

* Test 2: Weighted/Ungrouped  
WEIGHT BY weight_var.
FREQUENCIES VARIABLES=variable
  /STATISTICS=MEAN STDDEV SKEWNESS.

* Test 3: Unweighted/Grouped
WEIGHT OFF.
SORT CASES BY group_var.
SPLIT FILE BY group_var.
FREQUENCIES VARIABLES=variable.

* Test 4: Weighted/Grouped
WEIGHT BY weight_var.
FREQUENCIES VARIABLES=variable.
```

### Step 2: Structure Reference Values
Organize SPSS output into nested lists for easy access:

```r
spss_values <- list(
  unweighted_ungrouped = list(
    frequencies = list(
      "1" = list(n = 118, pct = 4.7, valid_pct = 4.9, cum_pct = 4.9),
      "2" = list(n = 306, pct = 12.2, valid_pct = 12.6, cum_pct = 17.5)
    ),
    total_valid = 2421,
    total_missing = 79,
    total_n = 2500  # Critical: total INCLUDING missing
  ),
  
  weighted_ungrouped = list(
    frequencies = list(
      "1" = list(n = 119, pct = 4.7, valid_pct = 4.9, cum_pct = 4.9)
    ),
    total_valid = 2436,
    total_missing = 79,
    total_n = 2515  # Used for Raw % calculation
  )
)
```

### Step 3: Build Comparison Framework
Create reusable comparison infrastructure with tracking:

```r
# Global tracking for detailed reporting
validation_results <- list()

# Record every comparison for analysis
record_comparison <- function(test_name, category, metric, expected, actual, tolerance = 0) {
  match_status <- if (is.na(expected) && is.na(actual)) {
    TRUE
  } else if (is.na(expected) || is.na(actual)) {
    FALSE
  } else {
    abs(expected - actual) <= tolerance
  }
  
  validation_results <<- append(validation_results, list(list(
    test = test_name,
    category = as.character(category),
    metric = metric,
    expected = expected,
    actual = actual,
    match = match_status,
    tolerance = tolerance,
    difference = if (!is.na(expected) && !is.na(actual)) abs(expected - actual) else NA
  )))
  
  return(match_status)
}

# Standard comparison function
compare_with_spss <- function(r_result, spss_ref, test_name, tolerance_pct = 0.1) {
  r_table <- r_result$results
  
  for (cat in names(spss_ref$frequencies)) {
    spss_cat <- spss_ref$frequencies[[cat]]
    r_row <- r_table[r_table$value == as.numeric(cat), ]
    
    # Track frequency comparison
    record_comparison(test_name, cat, "Frequency", spss_cat$n, r_row$freq[1], 0)
    expect_equal(r_row$freq[1], spss_cat$n, tolerance = 0)
    
    # Track percentage comparisons
    if (!is.na(r_row$prc[1])) {
      record_comparison(test_name, cat, "Raw %", spss_cat$pct, r_row$prc[1], tolerance_pct)
      expect_equal(r_row$prc[1], spss_cat$pct, tolerance = tolerance_pct)
    }
  }
  
  # Track totals
  record_comparison(test_name, "N Statistics", "Valid N", 
                   spss_ref$total_valid, r_result$stats$valid_n, 0)
}

# Weighted comparison (handles rounding)
compare_with_spss_weighted <- function(r_result, spss_ref, test_name) {
  r_table <- r_result$results
  
  # Round frequencies for comparison
  r_table$freq_rounded <- round(r_table$freq)
  
  # Recalculate Raw % using SPSS total_n (includes missing)
  r_table$prc_rounded <- (r_table$freq_rounded / spss_ref$total_n) * 100
  
  # Valid % uses only valid total
  valid_total <- sum(r_table$freq_rounded[!is.na(r_table$value)])
  r_table$valid_prc_rounded <- ifelse(!is.na(r_table$value),
                                      r_table$freq_rounded / valid_total * 100,
                                      NA)
  
  # Compare with tracking
  for (cat in names(spss_ref$frequencies)) {
    # ... comparison logic with record_comparison() calls
  }
}
```

### Step 4: Write Test Scenarios
Test all major use cases systematically:

```r
test_that("Test 1: Unweighted/Ungrouped", {
  result <- survey_data %>% 
    frequency(life_satisfaction, show.na = TRUE)
  
  compare_with_spss(result, spss_values$unweighted_ungrouped, 
                    "Unweighted/Ungrouped")
})

test_that("Test 2: Weighted/Ungrouped", {
  result <- survey_data %>% 
    frequency(life_satisfaction, weights = sampling_weight, show.na = TRUE)
  
  compare_with_spss_weighted(result, spss_values$weighted_ungrouped,
                            "Weighted/Ungrouped")
})

test_that("Test 3: Grouped analysis", {
  result <- survey_data %>%
    group_by(region) %>%
    frequency(life_satisfaction, weights = sampling_weight)
  
  # Extract group-specific results
  east_data <- result$results[result$results$region == "East", ]
  east_stats <- result$stats[result$stats$region == "East", ]
  
  compare_with_spss_weighted(list(results = east_data, stats = east_stats),
                            spss_values$weighted_grouped_east, "Grouped - East")
})
```

### Step 5: Generate Comprehensive Validation Report
Create detailed summary showing all comparisons:

```r
test_that("Generate validation summary", {
  if (length(validation_results) > 0) {
    # Convert to data frame
    df_results <- do.call(rbind, lapply(validation_results, as.data.frame))
    
    # Summary statistics
    total_comparisons <- nrow(df_results)
    total_matches <- sum(df_results$Match == "✓")
    match_rate <- (total_matches / total_comparisons) * 100
    
    cat(sprintf("\nTotal comparisons: %d\n", total_comparisons))
    cat(sprintf("Exact matches: %d (%.1f%%)\n", total_matches, match_rate))
    
    # Show details by test
    for (test_name in unique(df_results$Test)) {
      test_data <- df_results[df_results$Test == test_name, ]
      cat(sprintf("\n%s: %d/%d matches\n", test_name, 
                  sum(test_data$Match == "✓"), nrow(test_data)))
      
      # Show mismatches
      mismatches <- test_data[test_data$Match != "✓", ]
      if (nrow(mismatches) > 0) {
        for (i in 1:nrow(mismatches)) {
          cat(sprintf("  - %s %s: Expected %.2f, Got %.2f (diff: %.4f)\n",
                      mismatches$Category[i], mismatches$Metric[i],
                      mismatches$Expected[i], mismatches$Actual[i], 
                      mismatches$Difference[i]))
        }
      }
    }
    
    # Overall result
    if (total_matches == total_comparisons) {
      cat("\n✅ SUCCESS: All cells match SPSS reference values!\n")
    } else {
      cat("\n⚠ WARNING: Some cells did not match exactly\n")
    }
  }
  
  expect_true(TRUE)  # Always passes - just for reporting
})
```

## Common Issues & Solutions

### Issue 1: Raw % Mismatch in Weighted Data
**Symptom**: Raw % and Valid % show same value, both wrong  
**Cause**: Function uses wrong denominator (valid total for both)  
**Solution**: 
```r
# Correct calculation
raw_pct <- freq / sum(all_weights_including_missing) * 100
valid_pct <- freq / sum(valid_weights_only) * 100
```

### Issue 2: Missing Values Not in Output
**Symptom**: No NA row even with show.na = TRUE  
**Cause**: Weighted calculation skips NA handling  
**Solution**: Manually add NA frequencies:
```r
if (show.na && any(is.na(x))) {
  na_freq <- sum(weights[is.na(x) & !is.na(weights)])
  # Add to results
}
```

### Issue 3: Sum of Frequencies ≠ Total
**Symptom**: Individual rounded frequencies don't sum to displayed total  
**Cause**: SPSS displays rounded values but exact totals  
**Solution**: Accept ±1 tolerance for weighted totals

### Issue 4: Test Passes But Reports Mismatches
**Symptom**: 133 tests pass but only 89/132 comparisons match  
**Cause**: Test tolerance differs from tracking tolerance  
**Solution**: Ensure both use same tolerance values

## Manual Verification Template
When debugging, always verify calculations manually:

```r
# Extract raw data
x <- data$variable
w <- data$weights

# Calculate key values
valid_idx <- !is.na(x) & !is.na(w)
x_valid <- x[valid_idx]
w_valid <- w[valid_idx]

# Frequency for specific value
freq_val <- sum(w_valid[x_valid == value])

# Totals
total_all <- sum(w[!is.na(w)])  # All weights
total_valid <- sum(w_valid)      # Valid weights only

# Percentages
cat("Frequency:", freq_val, "\n")
cat("Raw % (our calculation):", freq_val / total_all * 100, "\n")
cat("Valid % (our calculation):", freq_val / total_valid * 100, "\n")
cat("SPSS shows:", spss_reference_value, "\n")
cat("Difference:", abs(calculated - spss_reference_value), "\n")
```

## Edge Cases to Always Test
1. **Empty groups** in grouped analysis
2. **All missing values** for a variable
3. **Single observation** with/without weight
4. **Zero or negative weights**
5. **One category** has all values
6. **Ties** in categorical variables

## Extending to Other Functions

Apply the same validation approach to any statistical function:

### t-test Validation
```r
spss_values <- list(
  t_statistic = 2.456,
  df = 98,
  p_value = 0.0158,
  mean_diff = 0.523,
  se_diff = 0.213
)
```

### ANOVA Validation
```r
spss_values <- list(
  f_statistic = 4.234,
  df_between = 2,
  df_within = 97,
  p_value = 0.0173,
  ss_between = 12.45,
  ss_within = 284.32
)
```

### Chi-Square Validation
```r
spss_values <- list(
  chi_square = 8.432,
  df = 4,
  p_value = 0.0772,
  cramers_v = 0.145
)
```

## Quality Checklist

Before considering validation complete:

- [ ] **Four scenarios minimum**: Unweighted/weighted × ungrouped/grouped
- [ ] **Missing values tested**: Both with and without
- [ ] **Edge cases covered**: Empty groups, single values, etc.
- [ ] **Tracking implemented**: All comparisons recorded
- [ ] **Manual verification done**: Key calculations confirmed
- [ ] **Report generated**: Clear summary of all comparisons
- [ ] **Tolerances documented**: Explanation for each tolerance choice
- [ ] **SPSS version noted**: Document version and settings used

## Success Criteria

A successful validation shows:
- ✅ **100% match rate** for critical values (counts, test statistics)
- ✅ **>95% match rate** for derived values (percentages, effect sizes)
- ✅ **Clear documentation** of any acceptable differences
- ✅ **Reproducible results** with provided test data
- ✅ **Fast execution** (<1 second per function)

## Maintenance

### When to Update
- SPSS releases new version
- Function logic changes
- New edge cases discovered
- Bug fixes affect output

### Version Documentation
```r
# Always document at top of test file
# SPSS Version: 29.0.0.0
# Date: 2024-01-23
# Settings: Weight on, Split file by region
# Known issues: Rounded display vs exact calculations
```

## Conclusion

This systematic approach ensures SurveyStat functions produce SPSS-identical results, building user confidence for SPSS-to-R migration. The key is understanding what you're comparing, tracking everything, and documenting acceptable differences.