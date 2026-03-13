# Recode Variables Using String Syntax

`rec()` recodes values of a variable using an intuitive string syntax.
It consolidates recoding, reversing, dichotomizing, and missing value
handling in a single function — the R equivalent of SPSS's `RECODE`
command.

## Usage

``` r
rec(
  data,
  ...,
  rules,
  as.factor = FALSE,
  suffix = NULL,
  var.label = NULL,
  val.labels = NULL
)
```

## Arguments

- data:

  A data frame or numeric vector. When a data frame is passed, use `...`
  to select variables.

- ...:

  Variables to recode (tidyselect). Only used when `data` is a data
  frame.

- rules:

  A character string defining the recoding rules (see Details).

- as.factor:

  If `TRUE`, return the result as a factor. Default: `FALSE`.

- suffix:

  A character string appended to the column names for the recoded
  variables (e.g., `"_r"`). If `NULL` (default), the original columns
  are overwritten in-place.

- var.label:

  A new variable label. If `NULL`, the existing label is kept with
  `" (recoded)"` appended.

- val.labels:

  A named character vector of value labels for the new values (e.g.,
  `c("1" = "Low", "2" = "Medium", "3" = "High")`). If `NULL`, labels are
  taken from inline `[Label]` syntax in `rules` (if present). Explicit
  `val.labels` always override inline labels.

## Value

If `data` is a vector, a recoded vector is returned. If `data` is a data
frame, the modified data frame is returned (invisibly).

## Details

### Recoding Syntax

Rules are specified as a semicolon-separated string of `"old=new"`
pairs:

|                     |                           |                               |
|---------------------|---------------------------|-------------------------------|
| **Syntax**          | **Meaning**               | **Example**                   |
| `"old=new"`         | Single value              | `"1=0; 2=1"`                  |
| `"lo:hi=new"`       | Range of values           | `"1:3=1; 4:6=2"`              |
| `"old=new [Label]"` | Inline value label        | `"1:2=1 [Low]; 3:5=2 [High]"` |
| `"else=new"`        | Catch-all for unmatched   | `"1=1; else=NA"`              |
| `"copy"`            | Keep original value       | `"1:3=copy; else=NA"`         |
| `"min"/"max"`       | Dynamic boundaries        | `"min:3=1; 4:max=2"`          |
| `"rev"`             | Reverse scale             | `"rev"`                       |
| `"dicho"`           | Median split              | `"dicho"`                     |
| `"dicho(x)"`        | Fixed cut-point           | `"dicho(3)"`                  |
| `"mean"`            | Mean split                | `"mean"`                      |
| `"quart"`           | Quartile split (4 groups) | `"quart"`                     |
| `"NA=new"`          | Replace NA                | `"NA=0; else=copy"`           |
| `"val=NA"`          | Set values to NA          | `"-9=NA; -8=NA"`              |

Rules are evaluated in order — the first matching rule wins.

### Inline Value Labels

You can attach value labels directly in the rules string using square
brackets after the new value:

`"1:2=1 [Low]; 3=2 [Medium]; 4:5=3 [High]"`

This is equivalent to specifying
`val.labels = c("1" = "Low", "2" = "Medium", "3" = "High")` but more
compact and self-documenting. If both inline labels and `val.labels` are
provided, `val.labels` takes precedence.

### Special Modes

`"rev"` reverses the scale by computing `max(x) + min(x) - x`. Value
labels are mirrored accordingly.

`"dicho"` dichotomizes at the median: values \\\le\\ median become 0,
values \\\>\\ median become 1.

`"dicho(x)"` dichotomizes at a fixed cut-point `x`: values \\\le x\\
become 0, values \\\> x\\ become 1.

`"mean"` dichotomizes at the arithmetic mean: values \\\le\\ mean become
0, values \\\>\\ mean become 1.

`"quart"` splits into four quartile groups using
[`quantile()`](https://rdrr.io/r/stats/quantile.html): values \\\le\\ Q1
become 1, Q1–Q2 become 2, Q2–Q3 become 3, and \\\>\\ Q3 become 4.
Quartile boundaries are computed unweighted.

## See also

[`to_dummy()`](https://YannickDiehl.github.io/mariposa/reference/to_dummy.md)
for creating dummy variables,
[`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md)
for declaring missing values,
[`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)
for converting to factor labels

Other recode:
[`to_dummy()`](https://YannickDiehl.github.io/mariposa/reference/to_dummy.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# Collapse a 5-point scale to 3 categories (inline labels)
data <- rec(survey_data, trust_government,
            rules = "1:2=1 [Low]; 3=2 [Medium]; 4:5=3 [High]")

# Reverse a scale (with suffix to keep original)
data <- rec(survey_data, trust_government, trust_media,
            rules = "rev", suffix = "_r")

# Dichotomize at the median
data <- rec(survey_data, age, rules = "dicho", suffix = "_d")

# Set missing value codes to NA
data <- rec(survey_data, starts_with("trust"),
            rules = "-9=NA; -8=NA; else=copy")

# Replace NA with 0
data <- rec(survey_data, trust_government,
            rules = "NA=0; else=copy")

# Quartile split
data <- rec(survey_data, age, rules = "quart", suffix = "_q")

# Use inside mutate()
survey_data <- survey_data %>%
  mutate(
    trust_gov_3 = rec(trust_government,
                      rules = "1:2=1 [Low]; 3=2 [Medium]; 4:5=3 [High]"),
    age_q = rec(age, rules = "quart")
  )
```
