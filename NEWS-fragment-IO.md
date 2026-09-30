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
