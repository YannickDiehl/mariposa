# 📚 Complete SPSS Validation Workflow

Comprehensive guide for creating SPSS validation tests for SurveyStat functions.

## 🎯 Overview

This workflow validates R functions against SPSS to ensure statistical equivalence. We've achieved 100% compatibility for `describe()`, `t_test()`, `chi_squared_test()`, and `frequency()`.

## 📋 Prerequisites

1. **SPSS Installation** (version 25+ recommended)
2. **Test Data Files**:
   - `survey_data.sav` - Main test dataset
   - `longitudinal_data.sav` - For repeated measures
3. **Templates** in `tests/spss_reference/templates/`

## 🔄 Complete Workflow

### Phase 1: SPSS Syntax Creation

#### 1.1 Basic Structure
Every SPSS syntax file MUST include:

```spss
* ============================================================================
* FUNCTION_NAME() SPSS VALIDATION
* ============================================================================

* CRITICAL: Open Output Management System
OMS /SELECT ALL 
    /DESTINATION FORMAT=TEXT 
    OUTFILE='/absolute/path/to/output.txt' 
    /TAG='validation'.

GET FILE='/absolute/path/to/survey_data.sav'.

* ============================================================================
* TEST SECTIONS
* ============================================================================

ECHO '==== TEST 1: DESCRIPTIVE NAME ===='.
* Your SPSS commands here

ECHO '==== TEST 2: ANOTHER TEST ===='.
* More commands

* CRITICAL: Close Output Management System
OMSEND.
EXECUTE.
```

#### 1.2 Test Organization
Structure tests by complexity:

1. **Basic unweighted** (TEST 1-3)
2. **Weighted** (TEST 4-6)  
3. **Missing data handling** (TEST 7-9)
4. **Grouped analysis** (TEST 10-12)
5. **Edge cases** (TEST 13+)

#### 1.3 Common SPSS Commands

**Descriptive Statistics:**
```spss
DESCRIPTIVES VARIABLES=age income
  /STATISTICS=MEAN STDDEV MIN MAX VARIANCE SKEWNESS KURTOSIS.
```

**T-Test:**
```spss
T-TEST GROUPS=gender(1 2)
  /VARIABLES=life_satisfaction
  /CRITERIA=CI(.95).
```

**Chi-Square:**
```spss
CROSSTABS
  /TABLES=gender BY region
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI CC
  /CELLS=COUNT.
```

**Frequencies:**
```spss
FREQUENCIES VARIABLES=education
  /STATISTICS=MEAN MODE
  /ORDER=ANALYSIS.
```

### Phase 2: Parser Development

#### 2.1 Parser Structure
Add to `helper-spss-parser.R`:

```r
extract_function_values <- function(file_path, test_number = 1, variable = NULL) {
  lines <- readLines(file_path, warn = FALSE)
  
  # Find test section
  test_pattern <- paste0("TEST ", test_number, ":")
  test_start <- grep(test_pattern, lines)[1]
  
  if (is.na(test_start)) {
    return(NULL)
  }
  
  # Find relevant table (adjust pattern for your output)
  table_pattern <- "Your Table Header"
  table_start <- grep(table_pattern, lines[test_start:length(lines)])[1]
  
  if (is.na(table_start)) {
    return(NULL)
  }
  
  # Adjust for actual position
  table_start <- test_start + table_start - 1
  
  # Extract data (adjust offset based on actual output)
  data_line <- lines[table_start + 6]  # Common offsets: 4-8
  
  # Clean and extract numbers
  # Use improved regex for decimals like .819
  numbers <- as.numeric(unlist(regmatches(data_line, 
                                          gregexpr("-?\\d*\\.?\\d+", data_line))))
  
  # Map to statistics (adjust based on your function)
  return(list(
    statistic1 = numbers[1],
    statistic2 = numbers[2],
    # Add more mappings
  ))
}
```

#### 2.2 Line Offset Debugging
```r
# Helper to find correct offset
debug_spss_output <- function(file_path, test_number) {
  lines <- readLines(file_path)
  test_start <- grep(paste0("TEST ", test_number), lines)[1]
  
  # Show 30 lines after test marker
  cat("Lines from test start:\n")
  for (i in 0:30) {
    cat(sprintf("%3d: %s\n", i, lines[test_start + i]))
  }
}
```

#### 2.3 Common Parsing Patterns

**For tables with headers:**
```r
# Find header, then count lines to data
header_line <- grep("Descriptive Statistics", lines)[1]
data_line <- lines[header_line + 6]  # Adjust offset
```

**For multi-line variables:**
```r
# German/long names span multiple lines
var_line <- grep("Lebenszuf", lines)[1]
data_line <- lines[var_line + 2]  # Data often 2 lines down
```

**For grouped output:**
```r
# Each group has its own section
group_sections <- grep("region =", lines)
for (section in group_sections) {
  # Parse each group separately
}
```

### Phase 3: Test Implementation

#### 3.1 Test File Structure
Create `test-function-spss-validation.R`:

```r
# Load dependencies
library(testthat)
library(dplyr)
source(test_path("helper-spss-parser.R"))

# Helper for data loading
load_survey_data <- function() {
  data(survey_data, envir = environment())
  return(survey_data)
}

# Helper for SPSS file path
get_spss_output_path <- function() {
  paths <- c(
    test_path("../spss_reference/outputs/function_output.txt"),
    "tests/spss_reference/outputs/function_output.txt"
  )
  
  for (path in paths) {
    if (file.exists(path)) return(path)
  }
  
  skip("SPSS output file not found")
}

# Test sections
describe("SECTION 1: Basic Tests", {
  test_that("function() matches SPSS - basic case", {
    survey_data <- load_survey_data()
    spss_file <- get_spss_output_path()
    
    # Get SPSS values
    spss_values <- extract_function_values(spss_file, test_number = 1)
    
    if (is.null(spss_values)) {
      skip("Could not parse SPSS values")
    }
    
    # Run R function
    result <- your_function(survey_data, your_args)
    
    # Compare with appropriate tolerances
    expect_equal(result$statistic, spss_values$statistic1, 
                 tolerance = 0.01)
  })
})
```

#### 3.2 Tolerance Guidelines

| Statistic | Tolerance | Reason |
|-----------|-----------|--------|
| Means, SDs | 0.01 | Rounding differences |
| Variances | 0.01 | Squared differences accumulate |
| Test statistics (t, F) | 0.001 | High precision needed |
| P-values | 0.01 | Algorithm differences |
| Confidence intervals | 0.01 | Depends on t-distribution |
| Effect sizes | 0.002 | Derived from other stats |
| Skewness/Kurtosis | 0.05 | Different formulas |
| Counts (N, df) | 0.1 | Should be exact |

### Phase 4: Validation & Reporting

#### 4.1 Run Tests
```r
# Run all validation tests
devtools::test(filter = "spss-validation")

# Run specific function
testthat::test_file("tests/testthat/test-function-spss-validation.R")
```

#### 4.2 Generate Reports
Create report generator in `validation_reports/`:

```r
generate_function_validation_report <- function() {
  # Run tests and capture results
  results <- testthat::test_file("test-function-spss-validation.R")
  
  # Create comparison table
  comparison <- tibble(
    Test = test_names,
    SPSS = spss_values,
    R = r_values,
    Difference = abs(spss_values - r_values),
    Tolerance = tolerances,
    Pass = Difference <= Tolerance
  )
  
  # Generate report
  report <- paste0(
    "SPSS Validation Report: function()\n",
    "===================================\n\n",
    "Summary: ", sum(comparison$Pass), "/", nrow(comparison), " tests passed\n\n",
    format_comparison_table(comparison)
  )
  
  writeLines(report, "validation_reports/function_validation_report.txt")
}
```

## 🏆 Best Practices

### DO's ✅
1. **Always include OMSEND** in SPSS syntax
2. **Use TEST markers** for every test section
3. **Strip names** with `as.numeric()`
4. **Use improved regex** `-?\\d*\\.?\\d+`
5. **Check structure** before accessing fields
6. **Document line offsets** in comments
7. **Use appropriate tolerances** for each statistic

### DON'Ts ❌
1. **Don't hardcode** SPSS values in tests
2. **Don't skip OMSEND** - causes output contamination
3. **Don't use strict tolerances** for derived statistics
4. **Don't parse without checking** TEST marker exists
5. **Don't assume structure** - check with `is.null()`

## 📊 Validation Success Criteria

Your validation is complete when:
- ✅ All TEST sections have passing tests
- ✅ Tolerances are justified and documented
- ✅ Parser reliably extracts correct values
- ✅ Report shows clear comparison
- ✅ Edge cases are covered

## 🔍 Debugging Checklist

When tests fail:

1. **Check SPSS output exists and has TEST markers**
   ```r
   lines <- readLines("output.txt")
   grep("TEST", lines)
   ```

2. **Verify parser finds correct lines**
   ```r
   debug_spss_output("output.txt", test_number = 1)
   ```

3. **Check extracted values**
   ```r
   spss_values <- extract_function_values("output.txt", 1)
   str(spss_values)
   ```

4. **Compare with high precision**
   ```r
   sprintf("%.10f", c(spss_value, r_value))
   ```

5. **Verify tolerance is appropriate**
   ```r
   diff <- abs(spss_value - r_value)
   cat("Requires tolerance:", diff)
   ```

## 📁 File Structure

```
tests/spss_reference/
├── syntax/
│   └── function_validation.sps    # SPSS syntax
├── outputs/
│   └── function_output.txt        # SPSS output
├── templates/
│   ├── SPSS_SYNTAX_TEMPLATE.sps   # Starter syntax
│   ├── PARSER_TEMPLATE.R          # Parser template
│   └── TEST_TEMPLATE.R            # Test template
└── docs/
    └── VALIDATION_WORKFLOW.md     # This file
```

## 🚀 Next Steps

1. **For new functions**: Start with [QUICK_START.md](QUICK_START.md)
2. **For issues**: See [../TROUBLESHOOTING.md](../TROUBLESHOOTING.md)
3. **For examples**: Study successful implementations:
   - `test-describe-spss-validation.R`
   - `test-t-test-spss-validation.R`

---
**Success Rate:** 100% compatibility achieved for validated functions