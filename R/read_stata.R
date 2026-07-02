#' Read Stata Data with Tagged Missing Values
#'
#' @description
#' Reads a Stata `.dta` file and integrates with mariposa's tagged NA system.
#' Handles two scenarios:
#' \enumerate{
#'   \item **Native extended missing values** (`.a` through `.z`): Automatically
#'     detected and annotated.
#'   \item **Numeric missing codes** (e.g., -9, -42): When `tag_na` is provided,
#'     these regular values are converted to tagged NAs, giving the same result
#'     as [read_spss()] with `tag_na = TRUE`.
#' }
#'
#' @param path Path to a Stata `.dta` file.
#' @param encoding Character encoding for the file. If `NULL`, haven's default
#'   encoding detection is used. Generally only needed for Stata 13 files and
#'   earlier.
#' @param tag_na Numeric vector of values to treat as missing (e.g.,
#'   `c(-9, -8, -42)`). These values will be converted to tagged NAs across
#'   all numeric variables. Use this when Stata files contain SPSS-style missing
#'   codes stored as regular values. Default: `NULL` (only detect native Stata
#'   extended missing values).
#' @param verbose If `TRUE`, prints a message summarizing how many variables
#'   contain tagged missing values.
#' @param tag.na Deprecated dot-case argument name. See the "Deprecated
#'   arguments" section below.
#'
#' @return A tibble with the Stata data. Variables with missing value codes
#'   have:
#'   \itemize{
#'     \item Tagged NAs for each missing type
#'     \item An `"na_tag_map"` attribute mapping tag characters to original
#'       codes
#'     \item `is.na()` returns `TRUE` for these values (standard R behavior)
#'   }
#'
#' @details
#' ## Deprecated arguments
#'
#' The dot-case argument name `tag.na` is deprecated; use the snake_case
#' equivalent `tag_na` instead. The old name still works but issues a
#' deprecation warning (once per session) and will be removed in a future
#' release.
#'
#' ## Native Extended Missing Values
#'
#' Stata supports 27 distinct missing value types: `.` (system missing) and
#' `.a` through `.z` (extended missing values). The `haven` package preserves
#' these as tagged NAs automatically. `read_stata()` adds the `na_tag_map`
#' attribute so that mariposa's tagged NA functions work seamlessly.
#'
#' ## Numeric Missing Codes (tag_na)
#'
#' Many Stata files -- especially those converted from SPSS -- store missing
#' value codes as regular numeric values (e.g., -9 = "No answer", -42 =
#' "Data error"). The `tag_na` parameter converts these to tagged NAs,
#' enabling proper handling in [frequency()], [codebook()], and other
#' functions.
#'
#' When `tag_na` is used, [untag_na()] can recover the original numeric codes.
#'
#' @seealso [na_frequencies()], [strip_tags()], [untag_na()], [read_spss()],
#'   [read_sas()], [read_xpt()], [haven::read_dta()]
#'
#' @family data-import
#'
#' @examples
#' \dontrun{
#' # Read Stata file with native extended missing values
#' data <- read_stata("survey.dta")
#'
#' # Read Stata file with SPSS-style missing codes
#' data <- read_stata("survey.dta", tag_na = c(-9, -8, -42, -11))
#'
#' # Check what types of missing values exist
#' na_frequencies(data$income)
#'
#' # frequency() and codebook() show each missing type separately
#' data %>% frequency(income)
#' codebook(data)
#'
#' # Recover original codes or convert to regular NAs
#' untag_na(data$income)   # Recovers -9, -8, etc.
#' strip_tags(data$income) # Converts all to NA
#' }
#'
#' @export
read_stata <- function(path, encoding = NULL, tag_na = NULL, verbose = FALSE,
                       tag.na = NULL) {
  .check_haven("Stata import")

  # ---- Deprecated dot-case argument bridge (see VERSIONING_POLICY.md, 4) ----
  if (!is.null(tag.na)) {
    .warn_deprecated_arg("tag.na", "tag_na")
    if (missing(tag_na)) tag_na <- tag.na
  }

  if (!is.null(tag_na) && !is.numeric(tag_na)) {
    cli::cli_abort("{.arg tag_na} must be a numeric vector of missing value codes.")
  }

  data <- haven::read_dta(file = path, encoding = encoding)

  # Step 1: detect native extended missing values (shared helper)
  data <- .detect_native_tags(data, "stata", verbose)

  # Step 2: Convert user-specified missing values to tagged NAs
  if (!is.null(tag_na)) {
    data <- .tag_user_missing_values(data, tag_na, "stata", verbose)
  }

  data
}
