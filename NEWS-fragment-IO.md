* `read_sas()` reads `.sas7bdat` files again with the default arguments.
  It passed `catalog_encoding = NULL` explicitly, which haven 2.5 rejects
  ("Expected string vector of length 1"), so every call without an explicit
  catalog encoding failed. The catalog encoding now falls back to the data
  file's encoding, as documented.
* `untag_na()`, `na_frequencies()`, `write_spss()`, `write_xlsx()` and
  `write_xpt()` are much faster on large imported files. The NA tag of
  every missing value was read one element at a time, each through vctrs
  dispatch, and the Excel "Labels" sheet was built from thousands of
  one-row data frames. Full ALLBUS 2023 (579 variables): `untag_na()` over
  all columns 9.8 s -> 0.15 s, `write_spss()` 11.5 s -> 1.7 s,
  `write_xlsx()` 22 s -> 8 s (the rest is openxlsx2 itself). Output is
  unchanged.
* `write_spss()` no longer crashes ("Failed to insert value ...: The file
  format does not supported character tags for missing values") after
  `rec()`, `std()`, `center()`, `pomps()`, `to_numeric()` or arithmetic on
  variables imported with `read_spss()`. These results kept the tagged-NA
  payloads but lost the code map. Now `rec()` keeps the missing-value
  types, their code map and their labels (so `na_frequencies()`,
  `frequency()` and the SPSS export still show "no answer" etc.), while
  `std()`, `center()`, `pomps()` and `to_numeric()` return plain `NA` by
  design (as an SPSS `COMPUTE` gives system-missing). `write_spss()` writes
  remaining unmapped tags as system missing with one warning naming the
  variables.
* Value labels created by `rec()` (inline `[label]` syntax, `val_labels`,
  mirrored labels of `"rev"`), kept by `strip_tags()` or by
  `to_numeric(keep_labels = TRUE)` now survive `write_spss()` and
  `write_stata()`. They were attached as a bare `labels` attribute without
  the `haven_labelled` class, which haven's writers ignore. The results are
  `haven_labelled` now; `to_labelled()` picks up an existing `labels`
  attribute; the exporters also promote such bare attributes themselves.
* `rec(as_factor = TRUE)` names the levels by the result's value labels
  (e.g. the mirrored labels of `"rev"`, previously ignored: levels "1".."7")
  in code order. Values without a label keep their code as level name
  instead of becoming `NA`, and duplicate label texts are disambiguated by
  their code.
* `rec(rules = "rev")` reverses on the scale range instead of the observed
  range. It computed `max(x) + min(x) - x` over the data, so a 1-5 item
  answered only with 2-5 became 5..2 instead of 4..1, and the value labels
  were mirrored to codes that do not exist. The range now comes from the
  value labels of the valid codes (together with the observed values);
  without labels the observed range is used with a message, and the new
  syntax `rules = "rev(1, 5)"` sets the range explicitly (values outside it
  are reported).
* `rec()` on `haven_labelled_spss` vectors (`haven::read_sav(user_na =
  TRUE)`) treats the user-missing codes as missing: they were reversed or
  recoded like valid values and lost their codes and labels. The input is
  converted to the tagged-NA form of `read_spss()` first. `val_labels` is
  honoured with `rules = "rev"` (it was silently ignored).
* `rec()` syntax is more forgiving and never loses values silently:
  valid values that match no rule still become `NA` but now with a warning
  that lists them and suggests `else=copy` (SPSS's in-place `RECODE`
  keeps them) or `else=NA`; value lists work (`"1,2=1; 3,4:5=2"`, as
  SPSS's `RECODE (1,2=1)`); keywords are case-insensitive (`"REV"`,
  `"Dicho(3)"`); a reversed range such as `"5:1=1"` is an error instead of
  silently matching nothing; inline labels may contain semicolons
  (`"[niedrig; gering]"`); `"dicho(x)"` and a missing `rules` argument give
  clear English errors instead of leaked base-R (German) messages; the
  `" (recoded)"` label suffix is no longer appended again on every call.
* `to_label()` and `to_character()` no longer merge distinct codes that
  share a label text. ALLBUS labels the scale points 2-6 of 88 items "..",
  so e.g. `pt12` collapsed into three levels ("GAR KEIN VERTRAUEN", "..",
  "GROSSES VERTRAUEN") and the `to_numeric()` round trip turned 3-6 into 2.
  Duplicate texts now get their code appended (`".. (2)"`, `".. (3)"`), the
  same rule the test functions use for grouping variables.
* `to_label(data)` and `to_character(data)` without a variable selection
  no longer turn metric variables into factors. ALLBUS `age` and `isei08`
  (whose only labels are missing codes) became all-`NA` factors and the
  weight a factor with one level per value, silently. Now only variables
  whose values are all value-labelled are converted; the others are left
  unchanged with one message listing them. Selected variables are still
  converted, and whenever values without a label become `NA` a warning
  names the variables and points to `add_non_labelled = TRUE`.
* `copy_labels()` no longer forces the source's value labels and class
  onto columns that were converted or summarised: a `to_label()` factor
  became `int+lbl` 1, 2, 3 carrying the labels 1/5/9 of the source codes,
  and group means were labelled as if they were codes. Value labels,
  missing-value metadata and the class are now copied only when the target
  still holds the source's codes; the variable label is always copied.
* `set_na(data, -9, -8, tag = FALSE)` (the documented example) no longer
  strips the labels of every numeric column (`survey_data`: 15 variable
  labels -> 6). Variable labels, the value labels of the remaining codes,
  the `haven_labelled` class and existing missing-value types are kept;
  integer columns stay integer.
* `val_labels()` refuses to set value labels on a factor or character
  column (they were stored but never used) and points to `to_labelled()`;
  `set_na()` warns when a named variable or a vector is a factor instead of
  silently returning it unchanged.
* `to_dummy()` fixes: with `suffix = "label"`, values sharing a label text
  (ALLBUS ".." scale points) all wrote into one column `pt12_` and their
  dummies were lost - every category now gets its own column (value
  appended to duplicate or empty labels); umlauts are transliterated
  (`"männlich"` -> `maennlich`, was `mnnlich`); `ref` works for factors,
  by level name or number (it was compared with the level names only, so
  `ref = 1` silently returned all dummies), and a `ref` that matches no
  category is an error.
* `row_count()` counts value sets and missing values: `count = c(4, 5)`
  was recycled over the cells (wrong counts without a warning), and
  `count = NA` always returned 0. It now counts cells equal to any listed
  value (SPSS `COUNT n = v1 TO v5 (4, 5)`), `NA` counts missing values
  (SPSS `MISSING`), and SPSS missing codes of imported data (e.g. -9, a
  tagged NA after `read_spss()`) are counted when listed - they were
  always 0.
* `row_means()`, `row_sums()` and `row_count()` inside a grouped
  `mutate()` with the `.` placeholder now stop with an explanation and the
  `pick()` form instead of dplyr's bare size-mismatch error; `pick()` is
  the documented, recommended form. Non-numeric columns handed over by
  `pick()` are ignored with a warning naming them (they were dropped
  silently), and a fractional `min_valid` (e.g. 2.5) is rejected.
* `pomps()` warns when values lie outside `scale_min`-`scale_max` (an
  unrecoded "don't know" = 9 on a 1-5 scale silently scored 200), checks
  that `scale_min`/`scale_max` are single finite numbers (a vector gave
  the German base error "Bedingung hat Länge > 1", `NA` a cryptic one),
  and says clearly when an all-`NA` input leaves no range to derive.
* `std()` and `center()` keep the variable label of imported (SPSS)
  variables when overwriting them in place: the label was read after the
  column had already been replaced, so it was lost. Grouped
  standardization/centering returns a plain numeric column instead of
  leaving a `dbl+lbl` vector behind, vector input keeps its label (with
  " (standardized)"/" (centered)"), and the zero-spread warning names the
  variable and the group (e.g. "`x` (g = a): the spread (sd) is zero").
  The weighted mean/SD now come from the shared SPSS kernels (results
  unchanged).
* `write_spss()` and `write_stata()` export a factor created by
  `to_label()` with its original codes and value labels: haven renumbered
  it 1..k (ALLBUS `dm06` codes 100, 120, ... became 1, 2, ...), although
  the factor carries its codes. Other factors are still written as 1..k
  with their levels as labels.
* `read_spss()` -> `write_spss()` round trips the SPSS missing-value
  definitions exactly and quietly. `read_spss()` kept only the codes that
  occur, so an unchanged ALLBUS export produced 133 warnings ("4 discrete
  missing codes exceed SPSS's limit of 3 ... range -42--8") and rewrote
  `LOWEST THRU -1` as `-42 THRU -8`. The original definition is now
  remembered (attribute `spss_missing`) and written back while it fits
  (ALLBUS 2023: all 579 definitions and values identical, no warning).
  Variables with more than 3 codes otherwise use SPSS's "range plus one
  discrete value" form when that avoids valid values (e.g. codes 0, 7, 8,
  9 around valid 1-6, which used to be an error), reported in one message
  with readable ranges ("-11 to -8 and -42").
* `read_spss()`, `read_por()`, `read_stata()`, `read_sas()`, `read_xpt()`
  and `read_xlsx()` recognise a file of the wrong type from its first bytes
  and say what it looks like and which reader to use (e.g. "read_spss()
  cannot read x.dta: it looks like a Stata file (.dta). Use read_stata()
  instead."), instead of readstat's cryptic errors; a missing file is
  reported as such.
* `write_xpt()` no longer truncates variable names silently: the default
  SAS transport version 5 allows 8 characters, and truncation even created
  duplicate names. It now warns (listing old -> new names, suggesting
  `version = 8`) and refuses to write when truncation would produce
  duplicates.
* `write_xlsx()` makes sheet names unique within Excel's 31-character,
  case-insensitive limit (two list names sharing their first 31
  characters, or an element called "Labels", crashed without writing a
  file); renamed sheets are listed in one message. A missing output
  directory is reported clearly, as in `codebook(file = )`.
* `write_xlsx()` of a grouped `frequency()` result writes one block per
  group, headed by the variable and the group ("gender (Gender) - region =
  East") with its own N line. All groups were written as one block under
  the first group's header, without group labels, and the Total row summed
  the groups (Raw % = 200). Weighted N is rounded like the console print
  ("N=2516", was "N=5245.99999999998").
