#' Read SAS Data with Tagged Missing Values
#'
#' @description
#' Reads a SAS `.sas7bdat` file and integrates with mariposa's tagged NA
#' system. Handles two scenarios:
#' \enumerate{
#'   \item **Native special missing values** (`.A` through `.Z`, `._`):
#'     Automatically detected and annotated.
#'   \item **Numeric missing codes** (e.g., -9, -42): When `tag.na` is
#'     provided, these regular values are converted to tagged NAs, giving the
#'     same result as [read_spss()] with `tag.na = TRUE`.
#' }
#'
#' @param path Path to a SAS `.sas7bdat` data file.
#' @param catalog_file Path to a SAS catalog file (`.sas7bcat`) for value
#'   labels. If `NULL`, no catalog is used.
#' @param encoding Character encoding for the data file. If `NULL`, haven's
#'   default encoding detection is used.
#' @param catalog_encoding Character encoding for the catalog file. If `NULL`,
#'   the same encoding as the data file is used.
#' @param tag.na Numeric vector of values to treat as missing (e.g.,
#'   `c(-9, -8, -42)`). These values will be converted to tagged NAs across
#'   all numeric variables. Use this when SAS files contain missing codes
#'   stored as regular values. Default: `NULL` (only detect native SAS
#'   special missing values).
#' @param verbose If `TRUE`, prints a message summarizing how many variables
#'   contain tagged missing values.
#'
#' @return A tibble with the SAS data. Variables with missing value codes
#'   have:
#'   \itemize{
#'     \item Tagged NAs for each missing type
#'     \item An `"na_tag_map"` attribute mapping tag characters to original
#'       codes
#'     \item `is.na()` returns `TRUE` for these values (standard R behavior)
#'   }
#'
#' @details
#' ## Native Special Missing Values
#'
#' SAS supports 28 distinct missing value types: `.` (system missing), `.A`
#' through `.Z`, and `._` (underscore). The `haven` package preserves these
#' as tagged NAs automatically. `read_sas()` adds the `na_tag_map` attribute
#' so that mariposa's tagged NA functions work seamlessly.
#'
#' ## Numeric Missing Codes (tag.na)
#'
#' Some SAS files store missing value codes as regular numeric values (e.g.,
#' -9 = "No answer"). The `tag.na` parameter converts these to tagged NAs,
#' enabling proper handling in [frequency()], [codebook()], and other
#' functions.
#'
#' When `tag.na` is used, [untag_na()] can recover the original numeric codes.
#'
#' @seealso [read_xpt()], [na_frequencies()], [strip_tags()], [untag_na()],
#'   [read_spss()], [read_stata()], [haven::read_sas()]
#'
#' @family data-import
#'
#' @examples
#' \dontrun{
#' # Read SAS file with native special missing values
#' data <- read_sas("survey.sas7bdat")
#'
#' # Read with catalog for value labels
#' data <- read_sas("survey.sas7bdat", catalog_file = "formats.sas7bcat")
#'
#' # Read SAS file with numeric missing codes
#' data <- read_sas("survey.sas7bdat", tag.na = c(-9, -8, -42))
#'
#' # Check what types of missing values exist
#' na_frequencies(data$income)
#'
#' # Recover original codes or convert to regular NAs
#' untag_na(data$income)   # Recovers -9, -8, etc.
#' strip_tags(data$income) # Converts all to NA
#' }
#'
#' @export
read_sas <- function(path, catalog_file = NULL, encoding = NULL,
                     catalog_encoding = NULL, tag.na = NULL, verbose = FALSE) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg haven} is required for SAS import.",
      "i" = "Install it with: {.code install.packages(\"haven\")}"
    ))
  }

  # Automatically delegate .xpt files to read_xpt()
  if (grepl("\\.xpt$", path, ignore.case = TRUE)) {
    cli::cli_alert_info(
      "Detected {.file .xpt} file. Using {.fn read_xpt} instead of {.fn read_sas}."
    )
    return(read_xpt(path = path, tag.na = tag.na, verbose = verbose))
  }

  if (!is.null(tag.na) && !is.numeric(tag.na)) {
    cli::cli_abort("{.arg tag.na} must be a numeric vector of missing value codes.")
  }

  data <- haven::read_sas(
    data_file = path,
    catalog_file = catalog_file,
    encoding = encoding,
    catalog_encoding = catalog_encoding
  )

  # Step 1: Detect native SAS special missing values (.A through .Z, ._)
  n_vars_tagged <- 0L

  for (i in seq_len(ncol(data))) {
    x <- data[[i]]
    if (!is.numeric(x)) next

    result <- .build_na_tag_map_from_native(x, "sas")

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
    data <- .tag_user_missing_values(data, tag.na, "sas", verbose)
  }

  data
}


#' Read SAS Transport File with Tagged Missing Values
#'
#' @description
#' Reads a SAS transport file (`.xpt`) and integrates with mariposa's tagged
#' NA system. Handles both native SAS special missing values and user-specified
#' numeric missing codes (via `tag.na`). Transport files are a
#' platform-independent SAS data format commonly used for FDA submissions and
#' data exchange.
#'
#' @param path Path to a SAS transport file (`.xpt`).
#' @param tag.na Numeric vector of values to treat as missing (e.g.,
#'   `c(-9, -8, -42)`). These values will be converted to tagged NAs across
#'   all numeric variables. Default: `NULL` (only detect native SAS special
#'   missing values).
#' @param verbose If `TRUE`, prints a message summarizing how many variables
#'   contain tagged missing values.
#'
#' @return A tibble with the SAS data. See [read_sas()] for details on
#'   tagged NA handling.
#'
#' @details
#' SAS transport files support the same special missing values as
#' `.sas7bdat` files (`.A`-`.Z`, `._`). This format is self-contained and
#' does not require a separate catalog file for value labels.
#'
#' When `tag.na` is used, [untag_na()] can recover the original numeric codes.
#'
#' @seealso [read_sas()], [na_frequencies()], [strip_tags()], [untag_na()],
#'   [haven::read_xpt()]
#'
#' @family data-import
#'
#' @examples
#' \dontrun{
#' # Read transport file with native missing values
#' data <- read_xpt("survey.xpt")
#'
#' # Read with numeric missing codes
#' data <- read_xpt("survey.xpt", tag.na = c(-9, -8, -42))
#'
#' na_frequencies(data$income)
#' }
#'
#' @export
read_xpt <- function(path, tag.na = NULL, verbose = FALSE) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg haven} is required for SAS transport import.",
      "i" = "Install it with: {.code install.packages(\"haven\")}"
    ))
  }

  if (!is.null(tag.na) && !is.numeric(tag.na)) {
    cli::cli_abort("{.arg tag.na} must be a numeric vector of missing value codes.")
  }

  data <- haven::read_xpt(file = path)

  # Step 1: Detect native SAS special missing values
  n_vars_tagged <- 0L

  for (i in seq_len(ncol(data))) {
    x <- data[[i]]
    if (!is.numeric(x)) next

    result <- .build_na_tag_map_from_native(x, "sas")

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
    data <- .tag_user_missing_values(data, tag.na, "sas", verbose)
  }

  data
}
