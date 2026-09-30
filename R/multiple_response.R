#' Analyze Multiple Response Sets
#'
#' @description
#' \code{multiple_response()} analyzes "check all that apply" survey
#' questions — the SPSS \code{MULT RESPONSE} procedure. It takes a set of
#' 0/1 indicator variables (one per answer option) and produces the two
#' tables SPSS users know:
#' \itemize{
#'   \item Without \code{by}: the \strong{frequencies table} — number of
#'     mentions per option, \emph{percent of responses} (sums to 100%),
#'     and \emph{percent of cases} (sums above 100%, because respondents
#'     can tick several options).
#'   \item With \code{by}: the \strong{crosstab} of the set against a
#'     categorical variable — mentions and case-based column percentages
#'     per group.
#' }
#'
#' @param data Your survey data (a data frame or tibble). If grouped
#'   (via \code{dplyr::group_by()}), separate tables are produced per
#'   group (SPSS SPLIT FILE).
#' @param ... The indicator variables of the set (unquoted, supports
#'   tidyselect, e.g. \code{starts_with("info_")}). Each is checked
#'   against \code{counted}.
#' @param by Optional categorical variable (unquoted) to cross the set
#'   against (SPSS \code{MULT RESPONSE ... BY factor}).
#' @param counted The value that counts as a mention (default \code{1},
#'   matching SPSS dichotomy sets with counted value 1). For factor or
#'   character indicators give the level that marks a mention, e.g.
#'   \code{counted = "yes"}; a value that does not occur among their
#'   levels is an error.
#' @param weights Optional survey weights (unquoted variable name),
#'   treated as frequency weights matching SPSS \code{WEIGHT BY}.
#'
#' @return An object of class \code{"multiple_response"} whose
#'   \code{$results} tibble holds one row per answer option (and group
#'   combination) with:
#' \describe{
#'   \item{Option}{Variable name of the option}
#'   \item{Label}{The option's variable label (falls back to the name)}
#'   \item{n}{Number of mentions (weighted sum when weighted)}
#'   \item{pct_responses}{Share of all mentions — sums to 100%}
#'   \item{pct_cases}{Share of valid cases mentioning the option — can
#'     sum above 100%}
#' }
#'   With \code{by}, \code{$by_results} additionally holds the long-form
#'   crosstab (columns \code{by_level}, \code{Option}, \code{Label},
#'   \code{n}, \code{pct_cases}). \code{$n_cases} is the number of valid
#'   cases (at least one non-missing indicator), \code{$n_responses} the
#'   total number of mentions.
#'
#' @details
#' ## Understanding the Output
#'
#' The two percentage columns answer different questions:
#' \itemize{
#'   \item \strong{Percent of responses}: "Of all boxes ticked, how many
#'     were this option?" — describes the mix of answers.
#'   \item \strong{Percent of cases}: "What share of respondents ticked
#'     this option?" — usually the number reports need. It sums above
#'     100% whenever respondents tick more than one box.
#' }
#'
#' ## Case handling
#'
#' Following SPSS MULT RESPONSE, a case is \strong{valid} if it has at
#' least one non-missing indicator in the set; cases missing on
#' \emph{all} indicators are excluded and reported as missing. Within a
#' valid case, missing indicators simply contribute no mention. With
#' \code{by}, cases missing on the \code{by} variable are excluded too.
#'
#' ## Technical Details
#'
#' Weighted analyses count mentions and cases as unrounded sums of
#' weights (Charter §5.1); displayed Ns are rounded. With
#' \code{weights == 1} the weighted table reduces exactly to the
#' unweighted one. Only dichotomy sets (indicator + \code{counted}
#' value) are supported; SPSS's category-range sets are not.
#'
#' An SPSS v29 MULT RESPONSE reference run is pending; until it lands
#' the counts and percentages are verified against direct
#' hand-computation from the indicator matrix (see the SPSS
#' compatibility vignette).
#'
#' @examples
#' library(dplyr)
#' data(survey_data)
#'
#' # Build an example set: which institutions does a respondent
#' # trust highly (rating of 4 or 5)?
#' trust <- survey_data %>%
#'   mutate(
#'     gov     = as.integer(trust_government >= 4),
#'     media   = as.integer(trust_media >= 4),
#'     science = as.integer(trust_science >= 4)
#'   )
#'
#' # Frequencies table: % of responses vs. % of cases
#' multiple_response(trust, gov, media, science)
#'
#' # Crossed against gender, with weights
#' multiple_response(trust, gov, media, science,
#'                   by = gender, weights = sampling_weight)
#'
#' # --- Three-layer output ---
#' result <- multiple_response(trust, gov, media, science)
#' result              # full tables
#' summary(result)     # same tables, with section toggles
#'
#' @seealso
#' \code{\link{frequency}} for single-variable frequency tables.
#'
#' \code{\link{crosstab}} for ordinary two-variable crosstabs.
#'
#' \code{\link{summary.multiple_response}} for detailed output.
#'
#' @family descriptive
#' @export
multiple_response <- function(data, ..., by = NULL, counted = 1,
                              weights = NULL) {
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame or tibble.")
  }
  if (length(counted) != 1 || is.na(counted)) {
    cli_abort("{.arg counted} must be a single non-missing value.")
  }

  vars <- .process_variables(data, ...)
  var_names <- names(vars)
  if (length(var_names) < 2) {
    cli_abort("A multiple response set needs at least two indicator variables.")
  }

  # Factor/character indicators are compared as text: counted = 1 against
  # levels "no"/"yes" silently gave 0 mentions
  for (v in var_names) {
    x <- data[[v]]
    if (is.factor(x) || is.character(x)) {
      vals <- if (is.factor(x)) levels(x) else sort(unique(x[!is.na(x)]))
      if (!as.character(counted) %in% vals) {
        cli_abort(c(
          "Indicator {.var {v}} has no value equal to {.arg counted} = {.val {counted}}.",
          "i" = "Its values are {.val {vals}}; set {.arg counted} to the one that marks a mention."
        ))
      }
    }
  }

  by_quo <- rlang::enquo(by)
  by_name <- if (!rlang::quo_is_null(by_quo)) rlang::as_name(by_quo) else NULL
  if (!is.null(by_name)) {
    if (!by_name %in% names(data)) {
      cli_abort("{.arg by} variable {.var {by_name}} not found in data.")
    }
    if (by_name %in% var_names) {
      cli_abort("{.arg by} variable {.var {by_name}} cannot be part of the set.")
    }
  }

  weights_info <- .process_weights(data, rlang::enquo(weights))
  data <- weights_info$data
  w_name <- weights_info$name

  # Option labels from variable labels (exact match; codebook lesson)
  labels <- vapply(var_names, function(v) {
    lb <- attr(data[[v]], "label", exact = TRUE)
    if (is.null(lb) || !nzchar(lb)) v else lb
  }, character(1))

  is_grouped <- inherits(data, "grouped_df")

  if (is_grouped) {
    group_vars <- dplyr::group_vars(data)
    group_split <- dplyr::group_split(data)
    group_keys <- dplyr::group_keys(data)

    per_group <- lapply(seq_along(group_split), function(i) {
      .mr_block(group_split[[i]], var_names, labels, by_name, counted, w_name)
    })
    # Prefix each group's table with its key, repeated to the table's rows:
    # cbind() of the one-row key did not recycle a haven_labelled key
    # ("arguments imply differing number of rows: 1, 2")
    with_key <- function(i, tab) {
      dplyr::bind_cols(group_keys[rep(i, nrow(tab)), , drop = FALSE], tab)
    }
    results <- dplyr::bind_rows(lapply(seq_along(per_group), function(i) {
      with_key(i, per_group[[i]]$results)
    }))
    by_results <- if (!is.null(by_name)) {
      dplyr::bind_rows(lapply(seq_along(per_group), function(i) {
        with_key(i, per_group[[i]]$by_results)
      }))
    } else {
      NULL
    }
    n_cases <- vapply(per_group, function(g) g$n_cases, numeric(1))
    n_responses <- vapply(per_group, function(g) g$n_responses, numeric(1))
    n_missing <- vapply(per_group, function(g) g$n_missing, numeric(1))
  } else {
    group_vars <- character(0)
    single <- .mr_block(data, var_names, labels, by_name, counted, w_name)
    results <- single$results
    by_results <- single$by_results
    n_cases <- single$n_cases
    n_responses <- single$n_responses
    n_missing <- single$n_missing
  }

  structure(
    list(
      results = results,
      by_results = by_results,
      variables = var_names,
      labels = labels,
      by = by_name,
      counted = counted,
      weights = w_name,
      n_cases = n_cases,
      n_responses = n_responses,
      n_missing = n_missing,
      is_grouped = is_grouped,
      group_vars = group_vars
    ),
    class = "multiple_response"
  )
}


#' Frequencies + optional by-crosstab for one data block
#' @noRd
.mr_block <- function(data, var_names, labels, by_name, counted, w_name) {
  ind <- as.matrix(as.data.frame(lapply(data[var_names], function(x) {
    as.numeric(x == counted)  # NA stays NA
  })))

  # SPSS MULT RESPONSE: valid case = at least one non-missing indicator;
  # with BY, the by variable must be non-missing too.
  valid <- rowSums(!is.na(ind)) > 0
  if (!is.null(by_name)) {
    valid <- valid & !is.na(data[[by_name]])
  }
  w <- if (!is.null(w_name)) data[[w_name]] else rep(1, nrow(data))
  valid <- valid & !is.na(w)

  ind <- ind[valid, , drop = FALSE]
  ind[is.na(ind)] <- 0  # missing indicator within a valid case: no mention
  w <- w[valid]

  n_cases <- sum(w)                       # unrounded (Charter §5.1)
  n_missing <- sum(!valid)
  mentions <- as.vector(t(ind) %*% w)     # weighted mentions per option
  n_responses <- sum(mentions)

  results <- tibble::tibble(
    Option = var_names,
    Label = unname(labels),
    n = mentions,
    pct_responses = if (n_responses > 0) mentions / n_responses * 100 else NA_real_,
    pct_cases = if (n_cases > 0) mentions / n_cases * 100 else NA_real_
  )

  by_results <- NULL
  if (!is.null(by_name)) {
    # SPSS order (by code) and value labels instead of codes: a labelled
    # by variable used to head the columns with "1"/"2"
    by_vals <- .group_factor(data[[by_name]][valid])
    by_levels <- levels(droplevels(by_vals))
    rows <- lapply(by_levels, function(lv) {
      idx <- by_vals == lv
      w_lv <- w[idx]
      cases_lv <- sum(w_lv)
      m_lv <- as.vector(t(ind[idx, , drop = FALSE]) %*% w_lv)
      tibble::tibble(
        by_level = as.character(lv),
        Option = var_names,
        Label = unname(labels),
        n = m_lv,
        pct_cases = if (cases_lv > 0) m_lv / cases_lv * 100 else NA_real_,
        n_cases_level = cases_lv
      )
    })
    by_results <- do.call(rbind, rows)
  }

  list(results = results, by_results = by_results,
       n_cases = n_cases, n_responses = n_responses, n_missing = n_missing)
}


# ============================================================================
# PRINT METHOD
# ============================================================================

#' Print multiple response results
#'
#' @description
#' Prints the full SPSS-style MULT RESPONSE tables (frequencies and, when
#' \code{by=} was used, the crosstab). The output of
#' \code{multiple_response()} is a frequency table by nature, so
#' \code{print()} and \code{summary()} display the same tables;
#' \code{summary()} additionally offers section toggles and a
#' \code{digits} option.
#'
#' @param x An object of class \code{"multiple_response"} returned by
#'   \code{\link{multiple_response}}.
#' @param digits Number of decimal places for percentages (default: 1).
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' d <- survey_data
#' d$gov <- as.integer(d$trust_government >= 4)
#' d$media <- as.integer(d$trust_media >= 4)
#' multiple_response(d, gov, media)
#'
#' @export
#' @method print multiple_response
print.multiple_response <- function(x, digits = 1, ...) {
  print(summary(x, digits = digits))
  invisible(x)
}


# ============================================================================
# SUMMARY METHOD
# ============================================================================

#' Summary method for multiple response results
#'
#' @description
#' Creates a summary object that produces the detailed SPSS-style
#' MULT RESPONSE output when printed: the frequencies table (percent of
#' responses and of cases) and, when \code{by} was given, the crosstab
#' with case-based column percentages.
#'
#' @param object A \code{multiple_response} result object.
#' @param frequencies Logical. Show the frequencies table? (Default: TRUE)
#' @param crosstab Logical. Show the by-crosstab (when \code{by} was
#'   given)? (Default: TRUE)
#' @param digits Number of decimal places for percentages (Default: 1).
#' @param ... Additional arguments (not used).
#' @return A \code{summary.multiple_response} object.
#'
#' @examples
#' d <- survey_data
#' d$gov <- as.integer(d$trust_government >= 4)
#' d$media <- as.integer(d$trust_media >= 4)
#' summary(multiple_response(d, gov, media))
#'
#' @seealso \code{\link{multiple_response}} for the main analysis function.
#' @export
#' @method summary multiple_response
summary.multiple_response <- function(object, frequencies = TRUE,
                                      crosstab = TRUE, digits = 1, ...) {
  build_summary_object(
    object     = object,
    show       = list(frequencies = frequencies, crosstab = crosstab),
    digits     = digits,
    class_name = "summary.multiple_response"
  )
}


#' Print summary of multiple response results (detailed output)
#'
#' @description
#' Displays the detailed SPSS-style MULT RESPONSE tables for a
#' \code{\link{multiple_response}} result, per group for grouped data.
#'
#' @param x A \code{summary.multiple_response} object created by
#'   \code{\link{summary.multiple_response}}.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' d <- survey_data
#' d$gov <- as.integer(d$trust_government >= 4)
#' d$media <- as.integer(d$trust_media >= 4)
#' summary(multiple_response(d, gov, media, by = gender))
#'
#' @seealso \code{\link{multiple_response}} for the main analysis,
#'   \code{\link{summary.multiple_response}} for summary options.
#' @export
#' @method print summary.multiple_response
print.summary.multiple_response <- function(x, ...) {
  digits <- x$digits %||% 1
  show_freq <- isTRUE(x$show$frequencies)
  show_ct <- isTRUE(x$show$crosstab) && !is.null(x$by_results)

  print_header(get_standard_title("Multiple Response", x$weights, "Results"))

  info <- list(
    "Set" = paste(x$variables, collapse = ", "),
    "Counted value" = x$counted,
    "Weights Variable" = x$weights
  )
  if (!is.null(x$by)) info[["By"]] <- x$by
  if (x$is_grouped) info[["Grouped by"]] <- paste(x$group_vars, collapse = ", ")
  print_info_section(info)

  # Counts are shown as whole numbers (SPSS; weighted counts are sums of
  # weights, display-rounded), percentages with `digits` decimals
  pct_txt <- function(v) ifelse(is.na(v), "", formatC(v, format = "f", digits = digits))

  emit_freq <- function(rows, n_cases, n_responses, n_missing) {
    cat("\nFrequencies\n")
    df <- data.frame(
      Option = c(rows$Label, "Total"),
      `Responses n` = c(rows$n, n_responses),
      `Responses %` = c(rows$pct_responses, if (n_responses > 0) 100 else NA),
      `% of Cases` = c(rows$pct_cases, sum(rows$pct_cases)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    .print_desc_table(df, digits = digits, count_cols = "Responses n",
                      footer_rows = 1L)
    cat(sprintf("  Valid cases: %.0f | Total responses: %.0f | Excluded (all missing): %.0f\n",
                n_cases, n_responses, n_missing))
    cat("  % of Cases can sum above 100% (multiple mentions per case).\n")
  }

  # SPSS MULT RESPONSE crosstab: count and % of the column's cases per
  # cell, a Total column (all valid cases) and a Total row with the number
  # of cases (respondents) per column
  emit_ct <- function(rows, total_rows, n_cases) {
    cat(sprintf("\nCrosstab: set BY %s (%% of cases per column)\n", x$by))
    levels <- unique(rows$by_level)
    options <- unique(rows$Label)
    cell <- function(n, p) sprintf("%.0f (%s%%)", n, pct_txt(p))
    wide <- data.frame(Option = c(options, "Total (cases)"), stringsAsFactors = FALSE)
    for (lv in levels) {
      sub <- rows[rows$by_level == lv, ]
      wide[[lv]] <- c(cell(sub$n, sub$pct_cases),
                      sprintf("%.0f", sub$n_cases_level[1]))
    }
    wide[["Total"]] <- c(cell(total_rows$n, total_rows$pct_cases),
                         sprintf("%.0f", n_cases))
    right <- stats::setNames(rep("right", length(levels) + 1L), c(levels, "Total"))
    .print_desc_table(wide, digits = digits, count_cols = character(0),
                      footer_rows = 1L, align = right)
    cat("  Percentages and totals are based on cases (respondents).\n")
  }

  if (x$is_grouped) {
    groups <- unique(x$results[x$group_vars])
    for (gi in seq_len(nrow(groups))) {
      cat("\n")
      print_group_header(groups[gi, , drop = FALSE])
      rows <- x$results
      for (g in names(groups)) rows <- rows[.group_match(rows[[g]], groups[[g]][gi]), ]
      if (show_freq) {
        emit_freq(rows, x$n_cases[gi], x$n_responses[gi], x$n_missing[gi])
      }
      if (show_ct) {
        ct_rows <- x$by_results
        for (g in names(groups)) ct_rows <- ct_rows[.group_match(ct_rows[[g]], groups[[g]][gi]), ]
        emit_ct(ct_rows, rows, x$n_cases[gi])
      }
    }
  } else {
    if (show_freq) emit_freq(x$results, x$n_cases, x$n_responses, x$n_missing)
    if (show_ct) emit_ct(x$by_results, x$results, x$n_cases)
  }

  invisible(x)
}
