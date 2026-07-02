# Helper Functions for Weighted Statistical Functions
# ===================================================
# This file contains shared utility functions used across all w_* functions
# to ensure consistency and reduce code duplication.

#' @importFrom dplyr %>%
NULL

# Weight Validation Functions
# ---------------------------

#' Check if weights are valid (similar to datawizard)
#' @param weights Numeric vector of weights
#' @return Logical indicating if weights are valid
#' @keywords internal
.are_weights <- function(weights) {
  !is.null(weights) && is.numeric(weights) && length(weights) > 0
}

#' Validate weights (similar to datawizard)
#' @param weights Numeric vector of weights
#' @param verbose Logical; if TRUE, show warnings
#' @return Logical indicating if weights are positive
#' @keywords internal
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
#' @keywords internal
.check_weights <- function(weights, name = NULL) {
  if (is.null(weights)) return(invisible(TRUE))
  label <- if (!is.null(name)) paste0("Weights variable {.var ", name, "}") else "Weights"
  if (!is.numeric(weights)) {
    cli_abort(paste0(label, " must be numeric, not {.cls {class(weights)[1]}}."))
  }
  n_neg <- sum(weights < 0, na.rm = TRUE)
  if (n_neg > 0) {
    cli_abort(c(
      paste0(label, " must not contain negative values."),
      "x" = "{n_neg} negative weight{?s} found.",
      "i" = "Negative weights are not meaningful for survey analysis."
    ))
  }
  invisible(TRUE)
}

#' Evaluate weights in summarise() context
#' @param weights_arg Quoted expression for weights
#' @param parent_env Parent environment to search for weights
#' @return Numeric vector of weights or NULL
#' @keywords internal
.evaluate_weights <- function(weights_arg, parent_env = parent.frame()) {
  tryCatch({
    eval(weights_arg, envir = parent_env)
  }, error = function(e) {
    # If weights evaluation fails, try to find it in a data frame
    if (is.name(weights_arg)) {
      weight_name <- as.character(weights_arg)
      
      # Try to find weights in parent environments
      for (i in 1:5) {  # Check up to 5 parent frames
        env <- parent.frame(i + 1)  # +1 because we're already one level deep
        if (exists(weight_name, envir = env)) {
          return(get(weight_name, envir = env))
        }
      }
      
      # If still not found, warn and return NULL
      cli_warn("Weights variable {.var {weight_name}} not found. Falling back to unweighted calculation.")
      return(NULL)
    }
    return(NULL)
  })
}

# Data Processing Functions
# -------------------------

#' Process variable selection using tidyselect
#' @description
#' Central function for processing variable selection expressions using tidyselect.
#' Handles the ... expressions passed to statistical functions and returns a named
#' vector of column positions.
#'
#' @param data A data frame
#' @param ... Variable selection expressions (tidyselect compatible)
#' @return Named integer vector of column positions
#' @keywords internal
.process_variables <- function(data, ...) {
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }

  vars <- tidyselect::eval_select(rlang::expr(c(...)), data)

  if (length(vars) == 0) {
    cli_abort("No variables selected. Please specify at least one variable.")
  }

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
#' @keywords internal
.process_weights <- function(data, weights_quo) {
  if (rlang::quo_is_null(weights_quo)) {
    return(list(vector = NULL, name = NULL))
  }

  weights_name <- rlang::as_name(weights_quo)

  if (!weights_name %in% names(data)) {
    cli_abort("Weights variable {.var {weights_name}} not found in data.")
  }

  weights_vec <- data[[weights_name]]

  if (!is.numeric(weights_vec)) {
    cli_abort("Weights variable {.var {weights_name}} must be numeric.")
  }

  .check_weights(weights_vec, weights_name)

  list(vector = weights_vec, name = weights_name)
}

#' Calculate effective sample size for weighted data
#' @param weights Numeric vector of weights
#' @return Effective sample size
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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

# Statistical Computation Helpers
# -------------------------------

#' Calculate skewness using the SPSS Type-2 sample-corrected formula
#'
#' For unweighted data: type1_skew * sqrt(n*(n-1)) / (n-2)
#' where type1_skew = m3 / m2^(3/2) (population skewness).
#' For weighted data: substitutes n = sum(w) per SPSS frequency-weight convention.
#' Reference: Joanes & Gill (1998).
#'
#' @param x Numeric vector (length >= 3)
#' @param w Numeric vector of weights, or NULL
#' @return Numeric scalar
#' @keywords internal
.calc_skewness <- function(x, w = NULL) {
  if (is.null(w)) {
    n <- length(x)
    mean_x <- mean(x)
    m2 <- sum((x - mean_x)^2) / n
    m3 <- sum((x - mean_x)^3) / n
    type1_skew <- m3 / (m2^(3/2))
    type1_skew * sqrt(n * (n - 1)) / (n - 2)
  } else {
    w_sum <- sum(w)
    w_mean <- sum(x * w) / w_sum
    m2 <- sum(w * (x - w_mean)^2) / w_sum
    m3 <- sum(w * (x - w_mean)^3) / w_sum
    type1_skew <- m3 / (m2^(3/2))
    n <- w_sum
    type1_skew * sqrt(n * (n - 1)) / (n - 2)
  }
}

#' Calculate kurtosis using the SPSS sample-corrected formula
#'
#' Uses the Type 2 (sample excess kurtosis) formula matching SPSS FREQUENCIES output.
#' For unweighted data: G2 = ((n+1)*g2 + 6) * (n-1) / ((n-2)*(n-3))
#' where g2 = m4/m2^2 - 3 (population excess kurtosis).
#' For weighted data: Uses frequency-weighted N = sum(w) in place of n.
#' Reference: Joanes & Gill (1998), "Comparing measures of sample skewness and kurtosis"
#'
#' @param x Numeric vector of values (must have length >= 4)
#' @param w Numeric vector of weights, or NULL for unweighted
#' @param excess If TRUE, return excess kurtosis; if FALSE, return raw kurtosis
#' @return Numeric scalar
#' @keywords internal
.calc_kurtosis <- function(x, w = NULL, excess = TRUE) {
  if (is.null(w)) {
    # Unweighted: SPSS Type 2 sample excess kurtosis
    n <- length(x)
    mean_x <- mean(x)
    m2 <- sum((x - mean_x)^2) / n
    m4 <- sum((x - mean_x)^4) / n
    g2 <- m4 / (m2^2) - 3
    # Bias correction (Type 2 / SPSS formula)
    G2 <- ((n + 1) * g2 + 6) * (n - 1) / ((n - 2) * (n - 3))
    if (excess) G2 else G2 + 3
  } else {
    # Weighted: Use weighted N = sum(w) as effective frequency count
    w_sum <- sum(w)
    w_mean <- sum(x * w) / w_sum
    m2 <- sum(w * (x - w_mean)^2) / w_sum
    m4 <- sum(w * (x - w_mean)^4) / w_sum
    g2 <- m4 / (m2^2) - 3
    n <- w_sum  # SPSS treats sum of frequency weights as N
    G2 <- ((n + 1) * g2 + 6) * (n - 1) / ((n - 2) * (n - 3))
    if (excess) G2 else G2 + 3
  }
}



#' Weighted mid-ranks (frequency-weight convention)
#'
#' Mid-ranks each case would receive if it were replicated according to its
#' frequency weight: a tie group with total weight W_g occupying cumulative
#' weight positions (end - W_g, end] gets the mid-rank end - W_g + (W_g+1)/2.
#' For unit weights this reproduces rank(x, ties.method = "average") exactly,
#' and for integer weights it equals the mid-ranks of the expanded data set
#' (the SPSS WEIGHT BY convention). Used by all weighted rank tests.
#'
#' @param x Numeric vector of values to rank
#' @param w Numeric vector of weights (same length, no NAs)
#' @return Numeric vector of weighted mid-ranks
#' @keywords internal
.weighted_midranks <- function(x, w) {
  f <- factor(x)  # levels sort ascending for numeric input
  group_w <- as.numeric(tapply(w, f, sum))
  group_end <- cumsum(group_w)
  group_mid <- group_end - group_w + (group_w + 1) / 2
  group_mid[as.integer(f)]
}

#' Weighted Wilcoxon signed-rank core (frequency-weight convention)
#'
#' Shared implementation used by wilcoxon_test() and pairwise_wilcoxon() so
#' the two cannot drift apart (they previously duplicated this block and a
#' bias fix had to be applied twice). Substitutes sum(w) for n in the
#' standard variance formula; exact for integer frequency weights.
#'
#' @param d_no_ties Paired differences with zero differences removed
#' @param w_no_ties Corresponding weights
#' @return list with rankhat, V, E_V, Var_V, Z, p_value (two-sided), N_pop
#' @keywords internal
.weighted_signed_rank_z <- function(d_no_ties, w_no_ties) {
  abs_d <- abs(d_no_ties)
  rankhat <- .weighted_midranks(abs_d, w_no_ties)

  pos_in_ranked <- d_no_ties > 0
  V <- sum(w_no_ties[pos_in_ranked] * rankhat[pos_in_ranked])

  N_pop <- sum(w_no_ties)
  E_V <- N_pop * (N_pop + 1) / 4

  tie_groups_w <- tapply(w_no_ties, factor(abs_d), sum)
  tie_correction <- sum(tie_groups_w^3 - tie_groups_w)
  Var_V <- N_pop * (N_pop + 1) * (2 * N_pop + 1) / 24 - tie_correction / 48

  Z <- if (Var_V > 0) (V - E_V) / sqrt(Var_V) else 0

  list(rankhat = rankhat, V = V, E_V = E_V, Var_V = Var_V, Z = Z,
       p_value = 2 * stats::pnorm(abs(Z), lower.tail = FALSE), N_pop = N_pop)
}

#' Require the haven package for an import/export feature
#'
#' Central guard replacing 20+ hand-rolled requireNamespace blocks
#' (pattern borrowed from .check_openxlsx2 in write_xlsx.R).
#'
#' @param purpose Short text naming the feature, e.g. "SPSS import"
#' @keywords internal
.check_haven <- function(purpose) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg haven} is required for {purpose}.",
      "i" = "Install it with: {.code install.packages(\"haven\")}"
    ), call = rlang::caller_env())
  }
  invisible(TRUE)
}
