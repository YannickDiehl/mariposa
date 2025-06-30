# SPSS Syntax Files

This directory contains SPSS syntax (.sps) files for generating reference outputs.

## Purpose

These syntax files ensure reproducible SPSS analyses that can be run consistently to generate reference outputs for validation.

## File Organization

Syntax files are organized by function and analysis type:

```
syntax/
├── t_test_basic.sps          # Basic t-test syntax
├── t_test_weighted.sps       # Weighted t-test syntax  
├── t_test_grouped.sps        # Grouped t-test syntax
├── describe_basic.sps        # Basic descriptives syntax
├── describe_weighted.sps     # Weighted descriptives syntax
├── describe_grouped.sps      # Grouped descriptives syntax
├── levene_test_basic.sps     # Basic Levene test syntax
├── levene_test_weighted.sps  # Weighted Levene test syntax
└── levene_test_grouped.sps   # Grouped Levene test syntax
```

## Syntax Standards

### Header Template
```spss
* SPSS Syntax for SurveyStat Package Validation
* Function: [function_name]
* Analysis Type: [analysis_type]  
* Generated: [date]
* 
* Purpose: Generate reference output for R package validation
* Dataset: survey_data.sav (identical to R survey_data)

GET FILE='path/to/survey_data.sav'.
```

### Output Settings
```spss
* Set output format for validation
OUTPUT EXPORT
  /CONTENTS EXPORT=VISIBLE LAYERS=PRINTSETTING MODELVIEWS=PRINTSETTING
  /TXT DOCUMENTFILE='output_filename.txt'
  /REPLACE.
```

### Analysis Commands
```spss
* Main analysis commands go here
* Use identical variable names and settings as R functions
* Include all necessary SPSS options for complete output
```

## Usage Workflow

1. Open SPSS and load survey_data.sav
2. Run appropriate .sps syntax file
3. Export output as TXT file to outputs/ directory
4. Verify output format matches expected structure
5. Run R validation tests to check compatibility

## Version Control

- Include SPSS version information in syntax comments
- Document any SPSS-specific settings or preferences
- Note any deviations from default SPSS behavior