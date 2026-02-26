# Print Method Style Guide for mariposa

## Overview
This document describes the consistent style implemented across all print methods in the mariposa package. All print methods use centralized helper functions from `R/print_helpers.R` to ensure visual consistency, reduce code duplication, and simplify maintenance.

## Architecture

### Central Helper File: `R/print_helpers.R`
All standardized formatting is provided by these helper functions:

| Function | Purpose |
|----------|---------|
| `print_header()` | Header with `cli_rule()` separator |
| `get_standard_title()` | Consistent `[Weighted] {Test} {Suffix}` format |
| `print_info_section()` | Structured test information (bullet list via `cli_bullets()`) |
| `print_test_parameters()` | Display conf.level, alternative, mu parameters |
| `add_significance_stars()` | p-value → `***` / `**` / `*` / `` |
| `print_significance_legend()` | Standard significance codes footer |
| `print_group_header()` | Unified `Group: var = value` heading via `cli_h2()` |
| `print_separator()` | Horizontal separator via `cli_rule()` |
| `format_number()` | Numeric formatting with decimal places |
| `format_variable_name()` | Variable name with optional label |
| `get_table_width()` | Dynamic table width calculation |
| `print_results_table()` | Table printing with numeric formatting |
| `print_footer_notes()` | Optional footer notes |

## Standard Print Method Structure

Every print method follows this consistent order:

```
1. Header           → print_header(get_standard_title(...))
2. Test Info        → print_info_section(list(...))
3. Test Parameters  → print_test_parameters(list(...))
4. Results Table    → formatted output (function-specific)
5. Significance     → print_significance_legend()
6. Return           → invisible(x)
```

## Conventions

### 1. Headers
All print methods use `cli_rule()` via `print_header()`:
```r
title <- get_standard_title("t-Test", weights_name, "Results")
print_header(title)
```
Output examples:
- `── t-Test Results ──────────────────────────────`
- `── Weighted t-Test Results ────────────────────`
- `── Descriptive Statistics ──────────────────────`

### 2. Information Sections
Test metadata uses `print_info_section()` with named list:
```r
print_info_section(list(
  "Dependent Variable" = variable_name,
  "Grouping Variable"  = group_name,
  "Weights Variable"   = weights_name
))
```
Output: bullet list via `cli_bullets()`.

### 3. Test Parameters
Standard parameters use `print_test_parameters()`:
```r
print_test_parameters(list(
  conf.level  = x$conf.level,
  alternative = x$alternative,
  mu          = x$mu
))
```

### 4. Group Headers
All grouped analyses use `print_group_header()`:
```r
print_group_header(group_values)
```
Output: `── Group: region = East, gender = Male ──`

### 5. Significance Stars
All p-value formatting uses `add_significance_stars()`:
```r
x$results$sig <- sapply(x$results$p_value, add_significance_stars)
```
Cut points: `***` p < 0.001, `**` p < 0.01, `*` p < 0.05

Footer via `print_significance_legend()`.

### 6. Border System
- **Headers/Separators**: `cli_rule()` (dynamic width, `──` style)
- **Table borders**: ASCII `-` via `paste(rep("-", width), collapse = "")`
- **Variable pair blocks** (correlations): `--- var1 × var2 ---`
- **No Unicode box-drawing characters** (`═`, `─`, `┌`, `┐`, etc.) in any print method

### 7. Return Value
Every print method returns `invisible(x)` as its last statement.

## Updated Print Methods

### ✅ Fully Standardized (all helpers used)
| # | Function | Helpers Used |
|---|----------|-------------|
| 1 | `print.t_test` | header, title, sig-stars, legend, invisible |
| 2 | `print.describe` | header, title, info, invisible |
| 3 | `print.frequency` | header, title, invisible (custom table preserved) |
| 4 | `print.oneway_anova` | header, title, info, params, sig-stars, legend, invisible |
| 5 | `print.rm_anova` | header, title, info, params, sig-stars, legend, invisible |
| 6 | `print.mann_whitney` | header, title, info, params, group-header, legend, invisible |
| 7 | `print.chi_square` | header, title, info, group-header, legend, invisible |
| 8 | `print.pearson_cor` | header, title, group-header, legend, invisible |
| 9 | `print.spearman_rho` | header, title, info, params, group-header, legend, invisible |
| 10 | `print.kendall_tau` | header, title, group-header, legend, invisible |
| 11 | `print.tukey_test` | header, title, info, params, group-header, sig-stars, legend, invisible |
| 12 | `print.scheffe_test` | header, title, info, params, group-header, sig-stars, legend, invisible |
| 13 | `print.levene_test` | header, title, info, sig-stars, legend, invisible |
| 14 | `print.crosstab` | header, title, group-header, invisible |
| 15 | `print.w_mean` | header, title, invisible |
| 16 | `print.w_sd` | header, title, invisible |

### 🔄 Remaining (weighted statistics, minor updates needed)
These functions work correctly but could adopt more helpers:
- `w_median`, `w_var`, `w_se`, `w_iqr`, `w_range`, `w_quantile`, `w_skew`, `w_kurtosis`, `w_modus`

Pattern to apply:
```r
test_type <- get_standard_title("Statistic Name", x$weights, "Statistics")
print_header(test_type)
# ... results ...
invisible(x)
```

## Design Decisions

### Preserved Distinctive Elements
Some functions retain custom formatting where it adds value:
1. **frequency** — ASCII box table format (`+---+`) for clear tabular display
2. **crosstab** — Special table layout for cross-tabulation matrices
3. **Correlation functions** — Matrix display with dynamic width adjustment

### Deliberately Not Changed
- **levene_test grouped header**: Complex multi-path group detection logic (handles `"Group"` column, separate group columns, and fallback cases) — uses custom formatting instead of `print_group_header()`

## Adding a New Print Method

Follow this template when creating a new print method:

```r
#' @export
print.my_result <- function(x, digits = 3, ...) {
  # 1. Header
  weights_name <- x$weight_var %||% x$weights
  title <- get_standard_title("My Test", weights_name, "Results")
  print_header(title)

  # 2. Test info
  print_info_section(list(
    "Variable"  = x$variable,
    "Group"     = x$group,
    "Weights"   = weights_name
  ))

  # 3. Parameters
  print_test_parameters(list(
    conf.level  = x$conf.level,
    alternative = x$alternative
  ))

  # 4. Results (function-specific)
  # ... display results ...

  # 5. Significance (if applicable)
  x$results$sig <- sapply(x$results$p_value, add_significance_stars)
  print_significance_legend()

  # 6. Return
  invisible(x)
}
```

## Benefits
1. **Consistency** — Users see familiar output format across all functions
2. **Maintainability** — Style changes propagate through helper functions
3. **No Unicode issues** — ASCII borders avoid terminal encoding problems
4. **Extensibility** — New functions adopt the standard with minimal effort
5. **CLI integration** — `cli` package provides responsive, terminal-aware formatting

## Implementation Notes
- All helper functions are `@keywords internal` (not exported)
- The `%||%` operator from `rlang` is used for null coalescing
- `cli_rule()` adapts to terminal width automatically
- Significance stars use `cut()` with `right = FALSE` for standard breakpoints
- `cli_bullets()` provides consistent bullet formatting with `*` prefix
- `cli_h2()` provides styled group headings

---
*Last updated: February 2026*
