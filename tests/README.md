# SurveyStat Test Suite Documentation

## 🎯 Overview

Complete testing infrastructure for the SurveyStat R package, including unit tests, SPSS validation, and automated reporting.

**Current Status**: ✅ **100% SPSS Compatibility** (110/110 tests passing)

## 📁 Directory Structure

```
tests/
├── testthat/              # Test files and helpers
│   ├── test-*.R           # Function tests
│   ├── helper-*.R         # Parsing & utilities
│   └── setup/teardown.R   # Test lifecycle
│
├── spss_reference/        # SPSS validation system
│   ├── data/              # .sav test datasets
│   ├── syntax/            # .sps SPSS scripts
│   ├── outputs/           # .txt SPSS results
│   └── templates/         # Starter templates
│
└── validation_reports/    # Auto-generated reports
    └── generate_*.R       # Report generators
```

## 🚀 Quick Start Guide

### 1️⃣ Run Tests

```r
# Run all tests
devtools::test()

# Run specific function tests
testthat::test_file("tests/testthat/test-describe-spss-validation.R")

# Generate validation report
source("tests/validation_reports/generate_validation_reports.R")
generate_all_validation_reports()
```

### 2️⃣ Add New SPSS Validation

**Option A: Simple Workflow** (Recommended for beginners)
```r
# See: spss_reference/QUICK_START.md
# 1. Copy template → 2. Run SPSS → 3. Create parser → 4. Write test
```

**Option B: Full Workflow** (For complex functions)
```r
# See: spss_reference/VALIDATION_WORKFLOW.md
# Complete guide with all edge cases and options
```

### 3️⃣ Debug Failed Tests

```r
# See: TROUBLESHOOTING.md for common issues
# Quick check:
source("tests/testthat/helper-spss-parser.R")
values <- extract_spss_values("path/to/output.txt", test_number = 1)
str(values)
```

## 📊 Test Coverage Status

| Function | Unit Tests | SPSS Tests | Pass Rate | Status |
|----------|------------|------------|-----------|--------|
| `describe()` | 49 | 49 | 100% | ✅ Production |
| `t_test()` | 34 | 34 | 100% | ✅ Production |
| `chi_squared_test()` | 21 | 21 | 100% | ✅ Production |
| `frequency()` | 6 | 6 | 100% | ✅ Production |
| `oneway_anova_test()` | 15 | - | - | 🔄 In Progress |
| `mann_whitney_test()` | 12 | - | - | 🔄 In Progress |
| Other functions | TBD | - | - | 📋 Planned |

## 📖 Documentation Map

| Document | Purpose | When to Use |
|----------|---------|-------------|
| **This File** | Overview & navigation | Start here |
| **[QUICK_START.md](spss_reference/QUICK_START.md)** | Simple validation workflow | Adding new SPSS tests |
| **[TROUBLESHOOTING.md](TROUBLESHOOTING.md)** | Fix common issues | When tests fail |
| **[TEST_STATUS.md](TEST_STATUS.md)** | Current validation results | Check test status |
| **[spss_reference/VALIDATION_WORKFLOW.md](spss_reference/VALIDATION_WORKFLOW.md)** | Detailed SPSS guide | Complex validations |

## 🛠️ Key Files

### Test Files
- `test-*-spss-validation.R` - SPSS validation tests
- `test-*.R` - Unit tests

### Helper Files  
- `helper-spss-parser.R` - Parse SPSS output files
- `helper-validation-reports.R` - Generate reports

### Templates
- `SPSS_SYNTAX_TEMPLATE.sps` - SPSS syntax template
- `PARSER_TEMPLATE.R` - Parser function template
- `TEST_TEMPLATE.R` - Test file template

## ✅ Success Metrics

- **SPSS Compatibility**: 100% match within tolerances
- **Test Coverage**: >90% for validated functions
- **Documentation**: Complete for all test types
- **Automation**: Full CI/CD integration ready

---
*Last Updated: 2025-09-16 | Version: 2.0*