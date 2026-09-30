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
#' @param fixed If \code{TRUE}, \code{pattern} is searched as literal text
#'   (case-insensitive), e.g. \code{find_var(data, "BEFRAGTE(R)", fixed =
#'   TRUE)} for label text with parentheses. Default \code{FALSE}: regular
#'   expression. A pattern that is not a valid regular expression (e.g.
#'   \code{"("}) is searched as literal text automatically, and a message
#'   points to \code{fixed = TRUE} when the literal text would give other
#'   matches.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{col}{Column position in the data}
#'     \item{name}{Variable name}
#'     \item{label}{Variable label (or \code{""} if none)}
#'   }
#'   Without matches, an empty data frame is returned invisibly (with a
#'   message).
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
#' Label text often contains characters with a special meaning in regular
#' expressions, such as parentheses: \code{"BEFRAGTE(R)"} as a regular
#' expression matches "BEFRAGTER". Use \code{fixed = TRUE} to search for
#' the text exactly as written.
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
#' # Literal text with regex characters, e.g. parentheses in a label
#' find_var(survey_data, "(1=left", fixed = TRUE)
#'
#' @seealso [var_label()] for getting/setting variable labels,
#'   [val_labels()] for value labels
#'
#' @family labels
#' @export
find_var <- function(data, pattern, search = c("name_label", "name", "label"),
                     fixed = FALSE) {
  .check_required(c("data", "pattern"))
  .check_value_arg("pattern", "a single character string")

  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  if (!is.character(pattern) || length(pattern) != 1L || is.na(pattern)) {
    cli::cli_abort("{.arg pattern} must be a single character string.")
  }

  if (!is.logical(fixed) || length(fixed) != 1L || is.na(fixed)) {
    cli::cli_abort("{.arg fixed} must be {.code TRUE} or {.code FALSE}.")
  }

  search <- match.arg(search)

  # ============================================================================
  # EXTRACT NAMES AND LABELS
  # ============================================================================

  var_names <- names(data)
  var_labels <- vapply(data, function(col) {
    lbl <- attr(col, "label", exact = TRUE)
    if (is.null(lbl)) "" else as.character(lbl)[1]
  }, character(1), USE.NAMES = FALSE)

  # ============================================================================
  # SEARCH
  # ============================================================================

  # Case-insensitive in both modes; literal matching compares lower-case text
  find <- function(x, literal) {
    if (literal) {
      grepl(tolower(pattern), tolower(x), fixed = TRUE)
    } else {
      grepl(pattern, x, ignore.case = TRUE)
    }
  }
  in_scope <- function(literal) {
    m_name <- find(var_names, literal)
    m_label <- find(var_labels, literal)
    switch(search,
      name_label = m_name | m_label,
      name       = m_name,
      label      = m_label
    )
  }

  if (isTRUE(fixed)) {
    matches <- in_scope(TRUE)
  } else {
    # An invalid regular expression (e.g. a lone "(") is searched as text
    # instead of failing with a regex-engine warning/error
    matches <- tryCatch(
      withCallingHandlers(in_scope(FALSE),
                          warning = function(w) stop(conditionMessage(w))),
      error = function(e) NULL
    )
    if (is.null(matches)) {
      cli::cli_inform(c(
        "i" = "{.val {pattern}} is not a valid regular expression; searched for the literal text instead."
      ))
      matches <- in_scope(TRUE)
    } else if (grepl("[][()\\\\.^$|*+?{}]", pattern) &&
               any(literal <- in_scope(TRUE)) &&
               !identical(matches, literal)) {
      # Label text often contains regex characters ("BEFRAGTE(R)"): when
      # the literal text exists but gives other matches, say that the
      # pattern was used as a regular expression
      cli::cli_inform(c(
        "i" = "{.val {pattern}} was used as a regular expression; use {.code fixed = TRUE} to search for the literal text."
      ))
    }
  }

  # ============================================================================
  # BUILD RESULT
  # ============================================================================

  idx <- which(matches)

  if (length(idx) == 0L) {
    cli::cli_inform("No variables found matching {.val {pattern}}.")
    # Invisible: printing a 0-row data frame only shows R's "<0 rows>"
    return(invisible(data.frame(col = integer(0), name = character(0),
                                label = character(0),
                                stringsAsFactors = FALSE)))
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
