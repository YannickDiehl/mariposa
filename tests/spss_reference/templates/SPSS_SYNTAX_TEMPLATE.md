# SPSS Syntax Template

## Copy This Template and Fill In [PLACEHOLDERS]

```spss
* ============================================================================
* SPSS Syntax for [FUNCTION_NAME] Validation
* Date: [TODAY_DATE]
* ============================================================================

* CRITICAL: Start output capture (prevents file contamination)
OMS
  /SELECT ALL
  /DESTINATION FORMAT=TEXT OUTFILE='[ABSOLUTE_PATH_TO_OUTPUT_FILE]'
  /TAG='validation'.

* Load data file
GET FILE='[ABSOLUTE_PATH_TO_DATA_FILE]'.

* ============================================================================
* BEGIN TESTS
* ============================================================================

ECHO '################################################################################'.
ECHO '#                     BEGIN [FUNCTION_NAME] VALIDATION                         #'.
ECHO '################################################################################'.

* ----------------------------------------------------------------------------
* TEST 1: Basic Unweighted Single Variable
* ----------------------------------------------------------------------------
ECHO '==== TEST 1: UNWEIGHTED SINGLE VARIABLE ===='.

[SPSS_COMMAND_HERE]
  /STATISTICS=ALL.

* ----------------------------------------------------------------------------
* TEST 2: Multiple Variables
* ----------------------------------------------------------------------------
ECHO '==== TEST 2: MULTIPLE VARIABLES ===='.

[SPSS_COMMAND_HERE] VARIABLES=var1 var2 var3
  /STATISTICS=ALL.

* ----------------------------------------------------------------------------
* TEST 3: Weighted Analysis
* ----------------------------------------------------------------------------
ECHO '==== TEST 3: WEIGHTED ANALYSIS ===='.

WEIGHT BY [WEIGHT_VARIABLE].
[SPSS_COMMAND_HERE]
  /STATISTICS=ALL.
WEIGHT OFF.

* ----------------------------------------------------------------------------
* TEST 4: Grouped Analysis
* ----------------------------------------------------------------------------
ECHO '==== TEST 4: GROUPED ANALYSIS ===='.

SORT CASES BY [GROUP_VARIABLE].
SPLIT FILE BY [GROUP_VARIABLE].
[SPSS_COMMAND_HERE]
  /STATISTICS=ALL.
SPLIT FILE OFF.

* ----------------------------------------------------------------------------
* TEST 5: With Missing Data
* ----------------------------------------------------------------------------
ECHO '==== TEST 5: WITH MISSING DATA ===='.

[SPSS_COMMAND_HERE]
  /MISSING=LISTWISE
  /STATISTICS=ALL.

ECHO '################################################################################'.
ECHO '#                      END [FUNCTION_NAME] VALIDATION                          #'.
ECHO '################################################################################'.

* CRITICAL: Close output capture (MUST HAVE or outputs merge!)
OMSEND.
EXECUTE.
```

## Placeholders to Replace

| Placeholder | Replace With | Example |
|------------|--------------|---------|
| `[FUNCTION_NAME]` | R function name | `describe` or `t_test` |
| `[TODAY_DATE]` | Current date | `2025-09-16` |
| `[ABSOLUTE_PATH_TO_OUTPUT_FILE]` | Full path to output | `/Users/name/project/tests/outputs/describe_output.txt` |
| `[ABSOLUTE_PATH_TO_DATA_FILE]` | Full path to data | `/Users/name/project/tests/data/survey_data.sav` |
| `[SPSS_COMMAND_HERE]` | SPSS procedure | `DESCRIPTIVES VARIABLES=age` |
| `[WEIGHT_VARIABLE]` | Weight column name | `sampling_weight` |
| `[GROUP_VARIABLE]` | Grouping column | `region` or `gender` |

## Common SPSS Commands by Function

| R Function | SPSS Command |
|-----------|--------------|
| `describe()` | `DESCRIPTIVES` and `FREQUENCIES` |
| `t_test()` | `T-TEST` |
| `chi_squared_test()` | `CROSSTABS` |
| `oneway_anova_test()` | `ONEWAY` |
| `mann_whitney_test()` | `NPAR TESTS /M-W` |
| `frequency()` | `FREQUENCIES` |

## Critical Rules

1. **ALWAYS include OMS and OMSEND** - Without these, outputs merge together
2. **ALWAYS use absolute paths** - Relative paths cause failures
3. **ALWAYS number tests sequentially** - TEST 1, TEST 2, TEST 3...
4. **ALWAYS reset state after changes** - WEIGHT OFF, SPLIT FILE OFF
5. **ALWAYS use unique output filename** - One file per function

## Example: Filled Template for describe()

```spss
OMS
  /SELECT ALL
  /DESTINATION FORMAT=TEXT OUTFILE='/Users/john/SurveyStat/tests/outputs/describe_output.txt'
  /TAG='validation'.

GET FILE='/Users/john/SurveyStat/tests/data/survey_data.sav'.

ECHO '==== TEST 1: UNWEIGHTED SINGLE VARIABLE ===='.
DESCRIPTIVES VARIABLES=age
  /STATISTICS=MEAN STDDEV VARIANCE MIN MAX RANGE SEMEAN KURTOSIS SKEWNESS.

OMSEND.
EXECUTE.
```