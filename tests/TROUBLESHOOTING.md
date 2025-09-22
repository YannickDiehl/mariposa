# 🛠️ Test Troubleshooting Guide

Quick solutions for common test failures in SurveyStat.

## 🔴 Most Common Issues

### 1. "Could not parse SPSS values"

**Quick Fix:**
```r
# Wrong line offset - adjust by 1-2 lines
data_line <- lines[desc_start + 6]  # Try +5, +7, +8
```

**Debug:**
```r
lines <- readLines("output.txt")
test_start <- grep("TEST 1:", lines)[1]
cat(lines[test_start:(test_start+20)], sep="\n")  # See actual structure
```

### 2. "Arguments imply differing number of rows"

**Quick Fix:**
```r
# Strip names from R statistics
as.numeric(test_result$statistic)  # Not test_result$statistic
as.numeric(test_result$parameter)  # Not test_result$parameter
```

### 3. Wrong values extracted from SPSS

**Quick Fix:**
```r
# Decimal numbers like .819 need special regex
gregexpr("-?\\d*\\.?\\d+", text)  # Not "-?\\d+\\.?\\d*"
```

**Debug:**
```r
text <- "-.229  2498  .409  .819"
numbers <- regmatches(text, gregexpr("-?\\d*\\.?\\d+", text))[[1]]
print(numbers)  # Should show: "-.229" "2498" ".409" ".819"
```

## 📊 Tolerance Quick Reference

```r
# Standard tolerances for different statistics
expect_equal(mean, spss_mean, tolerance = 0.01)      # Means, SDs
expect_equal(t_stat, spss_t, tolerance = 0.001)      # Test statistics
expect_equal(p_value, spss_p, tolerance = 0.01)      # P-values
expect_equal(n, spss_n, tolerance = 0.1)             # Counts (should match exactly)
expect_equal(skew, spss_skew, tolerance = 0.05)      # Skewness/Kurtosis
```

## 🔍 Debug Commands

### Find TEST markers
```r
lines <- readLines("spss_output.txt")
grep("TEST", lines)  # Shows all test locations
```

### Check what's being parsed
```r
# See exact line content
cat("Line content:", lines[150], "\n")

# Extract numbers from line
numbers <- as.numeric(extract_numbers(lines[150]))
print(numbers)
```

### Compare values with precision
```r
cat(sprintf("SPSS: %.10f\nR:    %.10f\nDiff: %.10f\n", 
            spss_value, r_value, abs(spss_value - r_value)))
```

## 🚨 SPSS Output Issues

### Missing OMSEND
**Symptom:** Output contains multiple test results mixed together

**Fix:** Add to end of SPSS syntax:
```spss
OMSEND.
EXECUTE.
```

### German/Long Variable Names
**Symptom:** Variable names split across lines

**Fix:** Look 1-2 lines down from variable name:
```r
var_start <- grep("Lebenszuf", lines)[1]
data_line <- lines[var_start + 2]  # Data often 2 lines down
```

## 🎯 Function-Specific Issues

### t_test()
- **One-sample vs two-sample**: Check structure with `!is.null(result$group_stats$group1)`
- **Levene's test**: Parse from "Equal variances assumed" line

### chi_squared_test()
- **Effect sizes**: Look in "Symmetric Measures" table
- **Footnotes**: Remove with `gsub("\\(.*?\\)", "", line)`

### describe()
- **Multiple variables**: Each variable adds ~2 lines to offset
- **Percentiles**: Found in separate "Percentiles" section

### frequency()
- **Categories**: Parse between "Frequency" and "Total" lines
- **Valid N**: Different from Total N (excludes missing)

## 💡 Prevention Tips

### When writing parsers:
```r
# 1. Always use improved regex
gregexpr("-?\\d*\\.?\\d+", text)

# 2. Always strip names
as.numeric(value)

# 3. Always check structure
if (!is.null(obj$field))

# 4. Always handle multiline
lines[(start):(start+10)]
```

### When writing tests:
```r
# 1. Use helper function for paths
get_spss_output_path()

# 2. Skip gracefully
if (is.null(spss_values)) skip("Could not parse")

# 3. Use appropriate tolerance
tolerance = 0.01  # Not 0.001 for means
```

## 🆘 Still Stuck?

1. **Check working examples:**
   - `test-describe-spss-validation.R` (100% passing)
   - `helper-spss-parser.R` (proven parsers)

2. **Enable verbose output:**
   ```r
   Sys.setenv(TESTTHAT_VERBOSE = "true")
   ```

3. **Save debug data:**
   ```r
   saveRDS(list(lines = lines, values = values), "debug.rds")
   ```

---
**Quick Links:**
- [QUICK_START.md](spss_reference/QUICK_START.md) - Add new validations
- [README.md](README.md) - Overview
- [TEST_STATUS.md](TEST_STATUS.md) - Current results