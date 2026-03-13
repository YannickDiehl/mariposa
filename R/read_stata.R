#' Read Stata Data with Tagged Missing Values
#'
#' @description
#' Reads a Stata `.dta` file and integrates with mariposa's tagged NA system.
#' Handles two scenarios:
#' \enumerate{
#'   \item **Native extended missing values** (`.a` through `.z`): Automatically
#'     detected and annotated.
#'   \item **Numeric missing codes** (e.g., -9, -42): When `tag.na` is provided,
#'     these regular values are converted to tagged NAs, giving the same result
#'     as [read_spss()] with `tag.na = TRUE`.
#' }
#'
#' @param path Path to a Stata `.dta` file.
#' @param encoding Character encoding for the file. If `NULL`, haven's default
#'   encoding detection is used. Generally only needed for Stata 13 files and
#'   earlier.
#' @param tag.na Numeric vector of values to treat as missing (e.g.,
#'   `c(-9, -8, -42)`). These values will be converted to tagged NAs across
#'   all numeric variables. Use this when Stata files contain SPSS-style missing
#'   codes stored as regular values. Default: `NULL` (only detect native Stata
#'   extended missing values).
#' @param verbose If `TRUE`, prints a message summarizing how many variables
#'   contain tagged missing values.
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
#' ## Native Extended Missing Values
#'
#' Stata supports 27 distinct missing value types: `.` (system missing) and
#' `.a` through `.z` (extended missing values). The `haven` package preserves
#' these as tagged NAs automatically. `read_stata()` adds the `na_tag_map`
#' attribute so that mariposa's tagged NA functions work seamlessly.
#'
#' ## Numeric Missing Codes (tag.na)
#'
#' Many Stata files -- especially those converted from SPSS -- store missing
#' value codes as regular numeric values (e.g., -9 = "No answer", -42 =
#' "Data error"). The `tag.na` parameter converts these to tagged NAs,
#' enabling proper handling in [frequency()], [codebook()], and other
#' functions.
#'
#' When `tag.na` is used, [untag_na()] can recover the original numeric codes.
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
#' data <- read_stata("survey.dta", tag.na = c(-9, -8, -42, -11))
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
read_stata <- function(path, encoding = NULL, tag.na = NULL, verbose = FALSE) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg haven} is required for Stata import.",
      "i" = "Install it with: {.code install.packages(\"haven\")}"
    ))
  }

  if (!is.null(tag.na) && !is.numeric(tag.na)) {
    cli::cli_abort("{.arg tag.na} must be a numeric vector of missing value codes.")
  }

  data <- haven::read_dta(file = path, encoding = encoding)

  # Step 1: Detect native Stata extended missing values (.a through .z)
  n_vars_tagged <- 0L

  for (i in seq_len(ncol(data))) {
    x <- data[[i]]
    if (!is.numeric(x)) next

    result <- .build_na_tag_map_from_native(x, "stata")

    if (!is.null(attr(result, "na_tag_map"))) {
      data[[i]] <- result
      n_vars_tagged <- n_vars_tagged + 1L
    }
  }

  if (verbose && n_vars_tagged > 0L) {
    cli::cli_inform(
      "Found native tagged missing values in {n_vars_tagged} variable{?s}."
    )
  }

  # Step 2: Convert user-specified missing values to tagged NAs
  if (!is.null(tag.na)) {
    data <- .tag_user_missing_values(data, tag.na, "stata", verbose)
  }

  data
}
