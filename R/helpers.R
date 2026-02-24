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

# Statistical Computation Helpers
# -------------------------------

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


