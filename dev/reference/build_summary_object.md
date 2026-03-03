# Build a summary object from a mariposa result

Copies all fields from the original result object and adds `$show`
(boolean toggle list) and `$digits`.

## Usage

``` r
build_summary_object(object, show, digits, class_name)
```

## Arguments

- object:

  The original result object

- show:

  Named list of logical toggles for sections

- digits:

  Number of decimal places for formatting

- class_name:

  Summary class name (e.g. `"summary.t_test"`)

## Value

A list with class `class_name`
