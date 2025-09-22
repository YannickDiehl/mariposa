# 📊 SPSS Validation System

System for validating SurveyStat functions against IBM SPSS Statistics.

**Status:** ✅ **100% SPSS Compatibility** (110/110 tests passing)

## 📁 Structure

```
spss_reference/
├── data/              # Test datasets (.sav files)
├── syntax/            # SPSS scripts (.sps files)
├── outputs/           # SPSS results (.txt files)
├── templates/         # Starter templates
└── docs/              # Additional documentation
```

## 🚀 Getting Started

### Quick Start (5 minutes)
➡️ **[QUICK_START.md](QUICK_START.md)** - Add SPSS validation in 5 steps

### Full Guide (detailed)
➡️ **[VALIDATION_WORKFLOW.md](VALIDATION_WORKFLOW.md)** - Complete workflow with all options

### Having Issues?
➡️ **[../TROUBLESHOOTING.md](../TROUBLESHOOTING.md)** - Common problems & solutions

## ✅ Validated Functions

| Function | Tests | Status | Example File |
|----------|-------|--------|--------------|
| `describe()` | 49 | ✅ 100% | `test-describe-spss-validation.R` |
| `t_test()` | 34 | ✅ 100% | `test-t-test-spss-validation.R` |
| `chi_squared_test()` | 21 | ✅ 100% | `test-chi-squared-spss-validation.R` |
| `frequency()` | 6 | ✅ 100% | `test-frequency-validation.R` |

## 🛠️ Key Components

### Parser Functions
Location: `tests/testthat/helper-spss-parser.R`
- `extract_spss_values()` - General purpose parser
- `parse_spss_descriptives()` - For DESCRIPTIVES output
- `parse_spss_t_test()` - For T-TEST output
- `parse_spss_chi_squared()` - For CROSSTABS output

### Templates
Location: `templates/`
- `SPSS_SYNTAX_TEMPLATE.sps` - SPSS syntax starter
- `PARSER_TEMPLATE.R` - Parser function template
- `TEST_TEMPLATE.R` - Test file template

## 🔧 Critical Requirements

### SPSS Syntax MUST Include:
```spss
OMS /SELECT ALL /DESTINATION FORMAT=TEXT OUTFILE='path.txt' /TAG='val'.
* Your commands here
OMSEND.  # CRITICAL - prevents output contamination
EXECUTE.
```

### Every Test Needs:
- Clear TEST markers: `ECHO '==== TEST 1: NAME ===='.`
- Absolute file paths
- Sequential numbering

## 📈 Success Metrics

- **Statistical Accuracy**: 100% match within tolerances
- **Test Coverage**: All major use cases covered
- **Automation**: Full CI/CD ready
- **Documentation**: Complete guides and templates

---
**Last Updated:** 2025-09-16 | **Version:** 2.0