# Analyze Multiple Response Sets

`multiple_response()` analyzes "check all that apply" survey questions —
the SPSS `MULT RESPONSE` procedure. It takes a set of 0/1 indicator
variables (one per answer option) and produces the two tables SPSS users
know:

- Without `by`: the **frequencies table** — number of mentions per
  option, *percent of responses* (sums to 100%), and *percent of cases*
  (sums above 100%, because respondents can tick several options).

- With `by`: the **crosstab** of the set against a categorical variable
  — mentions and case-based column percentages per group.

## Usage

``` r
multiple_response(data, ..., by = NULL, counted = 1, weights = NULL)
```

## Arguments

- data:

  Your survey data (a data frame or tibble). If grouped (via
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)),
  separate tables are produced per group (SPSS SPLIT FILE).

- ...:

  The indicator variables of the set (unquoted, supports tidyselect,
  e.g. `starts_with("info_")`). Each is checked against `counted`.

- by:

  Optional categorical variable (unquoted) to cross the set against
  (SPSS `MULT RESPONSE ... BY factor`).

- counted:

  The value that counts as a mention (default `1`, matching SPSS
  dichotomy sets with counted value 1).

- weights:

  Optional survey weights (unquoted variable name), treated as frequency
  weights matching SPSS `WEIGHT BY`.

## Value

An object of class `"multiple_response"` whose `$results` tibble holds
one row per answer option (and group combination) with:

- Option:

  Variable name of the option

- Label:

  The option's variable label (falls back to the name)

- n:

  Number of mentions (weighted sum when weighted)

- pct_responses:

  Share of all mentions — sums to 100%

- pct_cases:

  Share of valid cases mentioning the option — can sum above 100%

With `by`, `$by_results` additionally holds the long-form crosstab
(columns `by_level`, `Option`, `Label`, `n`, `pct_cases`). `$n_cases` is
the number of valid cases (at least one non-missing indicator),
`$n_responses` the total number of mentions.

## Details

### Understanding the Output

The two percentage columns answer different questions:

- **Percent of responses**: "Of all boxes ticked, how many were this
  option?" — describes the mix of answers.

- **Percent of cases**: "What share of respondents ticked this option?"
  — usually the number reports need. It sums above 100% whenever
  respondents tick more than one box.

### Case handling

Following SPSS MULT RESPONSE, a case is **valid** if it has at least one
non-missing indicator in the set; cases missing on *all* indicators are
excluded and reported as missing. Within a valid case, missing
indicators simply contribute no mention. With `by`, cases missing on the
`by` variable are excluded too.

### Technical Details

Weighted analyses count mentions and cases as unrounded sums of weights
(Charter §5.1); displayed Ns are rounded. With `weights == 1` the
weighted table reduces exactly to the unweighted one. Only dichotomy
sets (indicator + `counted` value) are supported; SPSS's category-range
sets are not.

An SPSS v29 MULT RESPONSE reference run is pending; until it lands the
counts and percentages are verified against direct hand-computation from
the indicator matrix (see the SPSS compatibility vignette).

## See also

[`frequency`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
for single-variable frequency tables.

[`crosstab`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)
for ordinary two-variable crosstabs.

[`summary.multiple_response`](https://YannickDiehl.github.io/mariposa/reference/summary.multiple_response.md)
for detailed output.

Other descriptive:
[`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md),
[`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md),
[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md),
[`normality_test()`](https://YannickDiehl.github.io/mariposa/reference/normality_test.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# Build an example set: which institutions does a respondent
# trust highly (rating of 4 or 5)?
trust <- survey_data %>%
  mutate(
    gov     = as.integer(trust_government >= 4),
    media   = as.integer(trust_media >= 4),
    science = as.integer(trust_science >= 4)
  )

# Frequencies table: % of responses vs. % of cases
multiple_response(trust, gov, media, science)
#> Multiple Response Set (3 options)
#>   gov: n = 583 (23.3% of cases)
#>   media: n = 470 (18.8% of cases)
#>   science: n = 1479 (59.2% of cases)
#>   Valid cases: 2500, total responses: 2532
#> Use summary() for detailed output.

# Crossed against gender, with weights
multiple_response(trust, gov, media, science,
                  by = gender, weights = sampling_weight)
#> Multiple Response Set (3 options) BY gender [Weighted]
#>   gov: n = 586 (23.3% of cases)
#>   media: n = 477 (18.9% of cases)
#>   science: n = 1490 (59.2% of cases)
#>   Valid cases: 2516, total responses: 2553
#> Use summary() for detailed output.

# --- Three-layer output ---
result <- multiple_response(trust, gov, media, science)
result              # compact overview
#> Multiple Response Set (3 options)
#>   gov: n = 583 (23.3% of cases)
#>   media: n = 470 (18.8% of cases)
#>   science: n = 1479 (59.2% of cases)
#>   Valid cases: 2500, total responses: 2532
#> Use summary() for detailed output.
summary(result)     # full detailed output
#> 
#> Multiple Response Results
#> -------------------------
#> - Set: gov, media, science
#> - Counted value: 1
#> 
#> Frequencies
#>   --------------------------------------------- 
#>   Option   Responses n  Responses %  % of Cases 
#>   --------------------------------------------- 
#>   gov            583.0         23.0        23.3 
#>   media          470.0         18.6        18.8 
#>   science       1479.0         58.4        59.2 
#>   --------------------------------------------- 
#>   Valid cases: 2500 | Total responses: 2532 | Excluded (all missing): 0
#>   % of Cases can sum above 100% (multiple mentions per case).
```
