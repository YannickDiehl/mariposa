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
