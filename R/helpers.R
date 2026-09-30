# Helper Functions for Weighted Statistical Functions
# ===================================================
# This file contains shared utility functions used across all w_* functions
# to ensure consistency and reduce code duplication.

#' @importFrom dplyr %>%
NULL

#' Strip label classes from a numeric vector for computation
#'
#' haven_labelled(_spss) vectors route every `[`, arithmetic and comparison
#' through vctrs S3 dispatch: element-wise loops over them are ~200x
#' slower, and comparisons on labelled_spss vectors with NA and an
#' `na_range` hit haven's lossy-cast check (`if (!any(lossy))` on NA).
#' Computation code therefore works on the bare numbers. Tagged NAs stay
#' NA (their payload bits survive), all attributes are dropped.
#'
#' @param x Numeric vector, possibly haven_labelled
#' @return Bare double/integer vector without attributes
#' @noRd
.plain_numeric <- function(x) {
  as.vector(unclass(x))
}

#' Vectorized NA tags
#'
#' The single reader for tagged-NA letters. vapply(x, haven::na_tag, ...)
#' splits a labelled vector into one vctrs object per element (~75% of
#' codebook() runtime on ALLBUS); haven::na_tag() is itself vectorized.
#' Integers cannot carry tags (and na_tag() errors on them).
#'
#' @param x Numeric vector, possibly haven_labelled
#' @return Character vector: the tag letter, NA for untagged or non-NA
#' @noRd
.na_tags <- function(x) {
  x <- .plain_numeric(x)
  if (!is.double(x)) return(rep(NA_character_, length(x)))
  haven::na_tag(x)
}

# Weight Validation Functions
# ---------------------------

#' Check if weights are valid (similar to datawizard)
#' @param weights Numeric vector of weights
#' @return Logical indicating if weights are valid
#' @noRd
.are_weights <- function(weights) {
  !is.null(weights) && is.numeric(weights) && length(weights) > 0
}

#' Validate weights (similar to datawizard)
#' @param weights Numeric vector of weights
#' @param verbose Logical; if TRUE, show warnings
#' @return Logical indicating if weights are positive
#' @noRd
.validate_weights <- function(weights, verbose = TRUE) {
  if (!.are_weights(weights)) {
    return(FALSE)
  }

  pos <- all(weights > 0, na.rm = TRUE)

  if (isTRUE(!pos) && isTRUE(verbose)) {
    cli_warn("Some weights were negative or zero. Weighting not carried out.")
  }

  pos
}

#' Enforce the package-wide weights policy
#'
#' Single policy applied by every weighted entry point: weights must be
#' numeric and non-negative, otherwise the call errors. Zero weights are
#' allowed (the case contributes nothing to weighted sums; SPSS likewise
#' excludes zero-weighted cases), NA weights are handled by each caller's
#' na.rm logic. This replaces the historical mix of silent
#' fall-back-to-unweighted, warn-and-continue, and no validation at all,
#' which produced different results for the same input depending on the
#' entry point.
#'
#' @param weights Numeric vector of weights (NULL is allowed and ignored)
#' @param name Optional display name of the weights variable for messages
#' @return invisible(TRUE); errors on violation
#' @noRd
.check_weights <- function(weights, name = NULL, call = rlang::caller_env()) {
  if (is.null(weights)) return(invisible(TRUE))
  label <- if (!is.null(name)) paste0("Weights variable {.var ", name, "}") else "Weights"
  if (!is.numeric(weights)) {
    cli_abort(paste0(label, " must be numeric, not {.cls {class(weights)[1]}}."),
              call = call)
  }
  n_neg <- sum(.plain_numeric(weights) < 0, na.rm = TRUE)
  if (n_neg > 0) {
    cli_abort(c(
      paste0(label, " must not contain negative values."),
      "x" = "{n_neg} negative weight{?s} found.",
      "i" = "Negative weights are not meaningful for survey analysis."
    ), call = call)
  }
  invisible(TRUE)
}

# Data Processing Functions
# -------------------------

#' Process variable selection using tidyselect
#' @description
#' Central function for processing variable selection expressions using tidyselect.
#' Handles the ... expressions passed to statistical functions and returns a named
#' vector of column positions.
#'
#' A named argument in `...` is refused (.check_dot_names()): a misspelled
#' argument (`weight =`, `na_rm =`, `groups =`) used to become a tidyselect
#' rename and produced an unweighted result, a garbage row, or an unrelated
#' error.
#'
#' Grouping columns of grouped data are dropped from the selection
#' (.drop_grouping_vars(), as dplyr::across() does) unless
#' `drop_groups = FALSE` (transformations such as rec() may recode a
#' grouping variable).
#'
#' @param data A data frame
#' @param ... Variable selection expressions (tidyselect compatible)
#' @param drop_groups Drop the grouping columns of grouped data from the
#'   selection (with a message)?
#' @param call Environment of the user-facing function (its formals are used
#'   for the "Did you mean" suggestions of misspelled arguments)
#' @return Named integer vector of column positions
#' @noRd
.process_variables <- function(data, ..., drop_groups = TRUE,
                               call = rlang::caller_env()) {
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.", call = call)
  }
  .check_dot_names(names(rlang::enquos(...)), call = call)

  vars <- tidyselect::eval_select(rlang::expr(c(...)), data)

  if (length(vars) == 0) {
    cli_abort("No variables selected. Please specify at least one variable.", call = call)
  }

  if (drop_groups) vars <- .drop_grouping_vars(data, vars, call = call)
  vars
}

#' Process weights parameter
#' @description
#' Central function for processing the weights parameter in statistical functions.
#' Handles both NULL weights (unweighted) and specified weights with validation.
#'
#' @param data A data frame
#' @param weights_quo Quoted expression for weights (from rlang::enquo)
#' @return List with vector (numeric vector or NULL) and name (character or NULL)
#' @noRd
.process_weights <- function(data, weights_quo, call = rlang::caller_env()) {
  if (rlang::quo_is_null(weights_quo)) {
    return(list(vector = NULL, name = NULL, data = data))
  }

  weights_name <- rlang::as_name(weights_quo)

  if (!weights_name %in% names(data)) {
    cli_abort("Weights variable {.var {weights_name}} not found in data.", call = call)
  }

  weights_vec <- data[[weights_name]]

  if (!is.numeric(weights_vec)) {
    cli_abort("Weights variable {.var {weights_name}} must be numeric.", call = call)
  }

  # Downstream code works on bare numbers: an SPSS weight
  # (haven_labelled_spss with na_range) fails every comparison once it
  # contains NA (see .plain_numeric). Callers take `data` back so later
  # reads of the weights column see the stripped vector too.
  weights_vec <- .plain_numeric(weights_vec)
  data[[weights_name]] <- weights_vec

  .check_weights(weights_vec, weights_name, call = call)

  list(vector = weights_vec, name = weights_name, data = data)
}

#' Calculate effective sample size for weighted data
#' @param weights Numeric vector of weights
#' @return Effective sample size
#' @noRd
.effective_n <- function(weights) {
  if (is.null(weights) || !.are_weights(weights)) {
    return(length(weights))
  }
  sum(weights, na.rm = TRUE)^2 / sum(weights^2, na.rm = TRUE)
}

# Value Label Functions
# --------------------

#' Get value labels for a variable
#' @param x The data vector (factor or haven-labelled)
#' @param freq_names Values to look up labels for
#' @return Character vector of labels (or empty strings if no labels)
#' @noRd
get_value_labels <- function(x, freq_names) {
  if (is.factor(x)) {
    factor_levels <- levels(x)
    ifelse(is.na(freq_names), NA, factor_levels[match(freq_names, factor_levels)] %||% as.character(freq_names))
  } else if (!is.null(attr(x, "labels"))) {
    value_labels <- attr(x, "labels")
    ifelse(is.na(freq_names), NA, names(value_labels)[match(freq_names, value_labels)] %||% as.character(freq_names))
  } else {
    # For variables without value labels, return empty strings instead of duplicating values
    rep("", length(freq_names))
  }
}

#' Build a named vector mapping raw values to display labels
#' @param x The original data vector (factor or haven-labelled)
#' @return Named character vector (names = raw values as strings, values = display labels).
#'   Returns NULL if no meaningful labels exist.
#' @noRd
.build_label_map <- function(x) {
  if (is.factor(x)) {
    lvls <- levels(x)
    # Check if levels are just sequential numbers (not meaningful)
    numeric_levels <- suppressWarnings(as.numeric(lvls))
    if (all(!is.na(numeric_levels) & numeric_levels == seq_along(lvls))) {
      return(NULL)
    }
    # Factor levels are the labels; map level string to itself
    result <- stats::setNames(lvls, lvls)
    return(result)
  }
  if (!is.null(attr(x, "labels"))) {
    value_labels <- attr(x, "labels")
    # Map: names are the raw numeric values (as strings), values are the label text
    result <- stats::setNames(names(value_labels), as.character(value_labels))
    return(result)
  }
  return(NULL)
}

#' Truncate labels that exceed a maximum width
#' @param labels Character vector of labels
#' @param max_width Maximum character width
#' @return Character vector with truncated labels
#' @noRd
.truncate_labels <- function(labels, max_width = 20) {
  ifelse(nchar(labels) > max_width,
         paste0(substr(labels, 1, max_width - 3), "..."),
         labels)
}

#' Apply label map to a set of level names and truncate
#' @param levels Character vector of raw level names
#' @param label_map Named character vector from .build_label_map(), or NULL
#' @param max_width Maximum character width for truncation
#' @return Character vector of display labels
#' @noRd
.apply_labels <- function(levels, label_map, max_width = 20) {
  if (is.null(label_map)) return(levels)
  mapped <- label_map[levels]
  display <- ifelse(is.na(mapped), levels, mapped)
  .truncate_labels(display, max_width)
}

#' Relabel a matrix's dimnames using label maps
#' @param mat A matrix with dimnames
#' @param label_maps Named list of label maps (one per dimension)
#' @param max_width Maximum character width for truncation
#' @return Matrix with relabelled dimnames
#' @noRd
.relabel_matrix <- function(mat, label_maps, max_width = 20) {
  if (is.null(mat) || is.null(label_maps)) return(mat)
  dn <- dimnames(mat)
  for (i in seq_along(dn)) {
    dim_name <- names(dn)[i]
    lm <- label_maps[[dim_name]]
    if (!is.null(lm)) {
      dn[[i]] <- .apply_labels(dn[[i]], lm, max_width)
    }
  }
  dimnames(mat) <- dn
  mat
}

#' Require the haven package for an import/export feature
#'
#' Central guard replacing 20+ hand-rolled requireNamespace blocks
#' (pattern borrowed from .check_openxlsx2 in write_xlsx.R).
#'
#' @param purpose Short text naming the feature, e.g. "SPSS import"
#' @noRd
.check_haven <- function(purpose) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg haven} is required for {purpose}.",
      "i" = "Install it with: {.code install.packages(\"haven\")}"
    ), call = rlang::caller_env())
  }
  invisible(TRUE)
}

#' Error because a removed dot-case argument was used
#'
#' Hard-error stub for dot-case argument names whose deprecation bridge was
#' removed (see VERSIONING_POLICY.md, section 4). Used only in functions
#' whose `...` is consumed by tidyselect: full removal of the formal would
#' let the old name land silently in `...` and be misinterpreted as a
#' variable selection. Keeping the formal as a `NULL` sentinel guarantees a
#' clear error instead.
#'
#' @param old_name The removed dot-case argument name (string)
#' @param new_name The replacement snake_case argument name (string)
#' @noRd
.stop_removed_arg <- function(old_name, new_name) {
  cli::cli_abort(
    "The {.arg {old_name}} argument was removed in mariposa 0.6.9; use {.arg {new_name}} instead.",
    call = rlang::caller_env()
  )
}

#' Grouping variable as a factor in SPSS order with value labels
#'
#' SPSS orders groups by code and shows value labels. Used by the tests that
#' split by a `group` variable so that (a) a numeric or labelled grouping
#' variable is ordered by value, not by first appearance in the data (the
#' sign of t no longer depends on row order), and (b) labelled codes are
#' displayed with their value labels ("East" instead of "1"). Factors are
#' returned unchanged; values without a label keep their code as level
#' name; missing values (including tagged NAs) stay NA. Duplicate label
#' texts (ALLBUS uses ".." for unlabelled scale points) get their code
#' appended so distinct groups are never merged.
#'
#' @param g Grouping vector
#' @return A factor
#' @noRd
.group_factor <- function(g) {
  if (is.factor(g)) return(g)
  if (inherits(g, "haven_labelled")) {
    v <- .plain_numeric(g)
    vals <- sort(unique(v[!is.na(v)]))
    lv <- as.character(vals)
    labs <- attr(g, "labels", exact = TRUE)
    if (!is.null(labs)) {
      labs <- labs[!is.na(labs)]
      hit <- match(vals, .plain_numeric(labs))
      lv[!is.na(hit)] <- names(labs)[hit[!is.na(hit)]]
    }
    dup <- lv %in% lv[duplicated(lv)]
    lv[dup] <- paste0(lv[dup], " (", vals[dup], ")")
    return(factor(match(v, vals), levels = seq_along(vals), labels = lv))
  }
  factor(g)
}

# Entry helpers (argument and input handling)
# -------------------------------------------

#' Drop grouping variables from a variable selection
#'
#' Selecting a grouping column of grouped data (explicitly, or through a
#' helper such as where(is.numeric)) made the analyses crash or misbehave:
#' group_modify() hands each group's data without its grouping columns, and
#' within a group the grouping variable is a constant (correlations with
#' zero SD, reliability using it as an item, t-tests on constant data). As
#' dplyr::across() does, grouping columns are excluded from the analysed
#' variables; a message says so. Called by .process_variables().
#'
#' @param data Data frame (possibly grouped)
#' @param vars Named integer vector from .process_variables()
#' @param call Caller environment for the error
#' @return `vars` without grouping columns; errors when nothing is left
#' @noRd
.drop_grouping_vars <- function(data, vars, call = rlang::caller_env()) {
  if (!inherits(data, "grouped_df")) return(vars)
  grp <- intersect(names(vars), dplyr::group_vars(data))
  if (length(grp) == 0) return(vars)
  vars <- vars[!names(vars) %in% grp]
  if (length(vars) == 0) {
    cli_abort(c(
      "No variables left to analyze.",
      "x" = "{.var {grp}} {?is a/are} grouping variable{?s}; {?it defines/they define} the groups."
    ), call = call)
  }
  cli::cli_inform(
    "Grouping variable{?s} {.var {grp}} {?is/are} not analyzed ({?it defines/they define} the groups)."
  )
  vars
}

#' Name of the function running in a frame (for messages)
#' @return The name, or NA when it cannot be determined
#' @noRd
.frame_fn_name <- function(env) {
  cl <- tryCatch(rlang::frame_call(env), error = function(e) NULL)
  nm <- if (is.call(cl)) tryCatch(rlang::call_name(cl), error = function(e) NULL)
  if (is.null(nm)) NA_character_ else nm
}

#' Closest formal argument name for a misspelled argument
#'
#' Same name up to case and `.`/`_` (na_rm -> na.rm), an abbreviation
#' (weight -> weights, conf -> conf.level) or a small typo (groups -> group,
#' weigths -> weights; utils::adist() <= 2 and shorter than the names).
#'
#' @param nm The name as typed
#' @param candidates Formal argument names
#' @return Character vector of suggestions (empty when nothing is close)
#' @noRd
.closest_arg <- function(nm, candidates) {
  candidates <- setdiff(candidates, "...")
  if (length(candidates) == 0 || !nzchar(nm)) return(character(0))
  norm <- function(s) gsub("[._]", "", tolower(s))
  hit <- candidates[norm(candidates) == norm(nm)]
  if (length(hit) > 0) return(hit[1])
  if (nchar(nm) >= 2) {
    hit <- candidates[startsWith(candidates, nm)]
    if (length(hit) > 0) return(hit)
  }
  d <- utils::adist(nm, candidates)[1, ]
  ok <- d <= 2 & d < nchar(nm) & d < nchar(candidates)
  if (any(ok)) return(candidates[ok][which.min(d[ok])])
  character(0)
}

#' Escape braces for cli/glue interpolation
#' @noRd
.cli_escape <- function(x) {
  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
}

#' Refuse named arguments in a variable-selection `...`
#'
#' In functions whose `...` selects variables, a named argument is either a
#' misspelled argument (`weight = w` for `weights`, `na_rm = TRUE`,
#' `groups = g`) or a tidyselect rename that the analyses do not support.
#' Both used to be passed on to tidyselect: the analysis ran unweighted with
#' a bogus extra "variable" row, or failed with an unrelated error
#' ("Variable weight is not numeric", "Exactly two variables must be
#' specified"). The error names the closest formal argument of the
#' user-facing function (found from `call`).
#'
#' @param dot_names Names of the `...` arguments ("" for unnamed)
#' @param call Environment of the user-facing function
#' @param selection Is `...` a variable selection (explain renames)?
#' @noRd
.check_dot_names <- function(dot_names, call = rlang::caller_env(),
                             selection = TRUE) {
  dot_names <- dot_names[!is.na(dot_names) & nzchar(dot_names)]
  if (length(dot_names) == 0) return(invisible(TRUE))

  fn <- tryCatch(rlang::frame_fn(call), error = function(e) NULL)
  candidates <- if (is.function(fn)) names(formals(fn)) else character(0)
  # Removed dot-case arguments kept as error sentinels (show.na next to
  # show_na) are no suggestion
  dotted <- grepl(".", candidates, fixed = TRUE) &
    gsub(".", "_", candidates, fixed = TRUE) %in% candidates
  candidates <- candidates[!dotted]
  fn_name <- .frame_fn_name(call)
  where <- if (is.na(fn_name)) "" else paste0(" of {.fn ", .cli_escape(fn_name), "}")

  bullets <- character(0)
  for (nm in dot_names) {
    sug <- .closest_arg(nm, candidates)
    if (length(sug) > 0) {
      sug_txt <- paste0("{.arg ", .cli_escape(sug), "}", collapse = ", ")
      bullets <- c(bullets, i = paste0("Did you mean ", sug_txt, " instead of {.arg ",
                                       .cli_escape(nm), "}?"))
    }
  }
  if (length(bullets) == 0 && selection) {
    bullets <- c(
      i = "Variables in {.arg ...} are selected without names; a name would rename the variable, which is not supported here.",
      i = "Rename the variable beforehand if needed, e.g. with {.fn dplyr::rename}."
    )
  } else if (length(bullets) == 0 && !is.na(fn_name)) {
    bullets <- c(i = paste0("See {.code ?", .cli_escape(fn_name), "} for the arguments."))
  }
  cli_abort(c(
    paste0("Unknown argument{?s} {.arg {dot_names}}", where, "."),
    bullets
  ), call = call)
}

#' Refuse any argument in an unused `...`
#'
#' For functions whose `...` is not used (fisher_test(), mcnemar_test()) or
#' not used in a mode (the vector input of the w_* functions and std()):
#' arguments there were silently ignored, e.g. `w_mean(x, weight = w)` inside
#' summarise() computed an unweighted mean.
#'
#' @param ... The function's `...`
#' @param call Environment of the user-facing function
#' @noRd
.check_dots_unused <- function(..., call = rlang::caller_env()) {
  n <- ...length()
  if (n == 0) return(invisible(TRUE))
  .check_dot_names(names(rlang::enquos(...)), call = call, selection = FALSE)
  fn_name <- .frame_fn_name(call)
  cli_abort(c(
    "{n} unused argument{?s} in {.arg ...}.",
    "i" = if (!is.na(fn_name)) paste0("See {.code ?", .cli_escape(fn_name), "} for the arguments.")
  ), call = call)
}

#' Refuse partially matched argument names
#'
#' R matches abbreviated argument names (`weight =` for `weights`,
#' `percent =` for `percentages`) for formals before `...`, but not for
#' formals after it. mariposa's analysis functions accept full argument
#' names only, so that `weight =` behaves the same everywhere: an error with
#' the full name instead of silently working in some functions and being
#' taken as a variable in others. Call at the top of functions whose formals
#' can be partially matched.
#'
#' @param env The function's environment
#' @noRd
.reject_partial_args <- function(env = rlang::caller_env()) {
  cl <- tryCatch(rlang::frame_call(env), error = function(e) NULL)
  fn <- tryCatch(rlang::frame_fn(env), error = function(e) NULL)
  if (!is.call(cl) || !is.function(fn)) return(invisible(TRUE))
  given <- rlang::names2(as.list(cl)[-1])
  given <- given[nzchar(given)]
  if (length(given) == 0) return(invisible(TRUE))
  fmls <- names(formals(fn))
  dots_at <- match("...", fmls)
  matchable <- if (is.na(dots_at)) fmls else fmls[seq_len(dots_at - 1L)]
  partial <- given[!given %in% fmls &
                     vapply(given, function(g) any(startsWith(matchable, g)), logical(1))]
  if (length(partial) == 0) return(invisible(TRUE))
  nm <- partial[1]
  full <- matchable[startsWith(matchable, nm)][1]
  fn_name <- .frame_fn_name(env)
  where <- if (is.na(fn_name)) "" else paste0(" of {.fn ", .cli_escape(fn_name), "}")
  cli_abort(c(
    paste0("Unknown argument {.arg {nm}}", where, "."),
    "i" = "Did you mean {.arg {full}}? Argument names must be written in full."
  ), call = env)
}
