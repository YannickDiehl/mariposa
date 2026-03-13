# ============================================================================
# Variable Search
# ============================================================================
# Search for variables by name or variable label in SPSS-style datasets.


#' Find Variables by Name or Label
#'
#' @description
#' Searches for variables in your data by matching a pattern against variable
#' names, variable labels, or both. This is especially useful for SPSS datasets
#' where variable names are often cryptic codes (e.g., \code{v104}, \code{q23a_1})
#' and the actual meaning is stored in variable labels.
#'
#' @param data A data frame (typically imported from SPSS with \code{\link{read_spss}}).
#' @param pattern A search term or regular expression to match against variable
#'   names and/or labels. Case-insensitive by default.
#' @param search Where to search: \code{"name_label"} (default) searches both
#'   variable names and labels, \code{"name"} searches only names, \code{"label"}
#'   searches only labels.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{col}{Column position in the data}
#'     \item{name}{Variable name}
#'     \item{label}{Variable label (or \code{""} if none)}
#'   }
#'
#' @details
#' ## When to Use This
#'
#' \itemize{
#'   \item You imported an SPSS file and need to find which variable contains
#'     "trust" or "satisfaction"
#'   \item You want to quickly identify all variables related to a topic
#'   \item You know the German/English label text but not the variable code
#' }
#'
#' ## Pattern Matching
#'
#' The \code{pattern} argument supports regular expressions. Matching is
#' case-insensitive. Some examples:
#'
#' \itemize{
#'   \item \code{"trust"} — matches "trust", "Trust", "distrust", "trustworthy"
#'   \item \code{"^trust"} — matches only names/labels starting with "trust"
#'   \item \code{"^q[0-9]+"} — matches variable names like q1, q23, q104
#'   \item \code{"zufried"} — finds German labels containing "Zufriedenheit"
#' }
#'
#' @examples
#' library(dplyr)
#' data(survey_data)
#'
#' # Find all variables related to "trust"
#' find_var(survey_data, "trust")
#'
#' # Search only in variable labels
#' find_var(survey_data, "satisfaction", search = "label")
#'
#' # Use regex to find numbered items
#' find_var(survey_data, "^q[0-9]+", search = "name")
#'
#' @seealso [var_label()] for getting/setting variable labels,
#'   [val_labels()] for value labels
#'
#' @family labels
#' @export
find_var <- function(data, pattern, search = c("name_label", "name", "label")) {

  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  if (!is.character(pattern) || length(pattern) != 1L) {
    cli::cli_abort("{.arg pattern} must be a single character string.")
  }

  search <- match.arg(search)

  # ============================================================================
  # EXTRACT NAMES AND LABELS
  # ============================================================================

  var_names <- names(data)
  var_labels <- vapply(data, function(col) {
    lbl <- attr(col, "label")
    if (is.null(lbl)) "" else as.character(lbl)
  }, character(1), USE.NAMES = FALSE)

  # ============================================================================
  # SEARCH
  # ============================================================================

  match_name <- grepl(pattern, var_names, ignore.case = TRUE)
  match_label <- grepl(pattern, var_labels, ignore.case = TRUE)

  matches <- switch(search,
    name_label = match_name | match_label,
    name       = match_name,
    label      = match_label
  )

  # ============================================================================
  # BUILD RESULT
  # ============================================================================

  idx <- which(matches)

  if (length(idx) == 0L) {
    cli::cli_inform("No variables found matching {.val {pattern}}.")
    return(data.frame(col = integer(0), name = character(0),
                      label = character(0), stringsAsFactors = FALSE))
  }

  result <- data.frame(
    col   = idx,
    name  = var_names[idx],
    label = var_labels[idx],
    stringsAsFactors = FALSE
  )

  rownames(result) <- NULL
  result
}
