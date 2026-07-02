# =============================================================================
# Weighted-statistics kernels
# =============================================================================
# THE single home for every weighted formula in the package. describe(),
# frequency(), the w_* functions, std()/center() and the rank tests all
# delegate here - a formula must never exist twice (audit lesson: the
# weighted variance once lived in six files and drifted).
#
# Conventions: SPSS frequency-weight semantics (sum(w) as N, sum(w)-1
# Bessel denominator, Type-2 skewness/kurtosis, Type-6/HAVERAGE quantiles).
# =============================================================================


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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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


# ============================================================================
# STATISTICAL CALCULATION FUNCTIONS
# ============================================================================

#' Validate and clean weights - central validation function
#' @noRd
.validate_and_clean_weights <- function(x, weights, na.rm = TRUE) {
  if (is.null(weights)) {
    return(list(x = x, weights = NULL, valid = TRUE))
  }
  
  # Basic validation checks
  if (length(weights) != length(x)) {
    cli_abort("Length of weights ({length(weights)}) does not match length of data ({length(x)}).")
  }

  # Package-wide policy: negative weights are an error, never a fallback
  .check_weights(weights)

  if (all(is.na(weights))) {
    cli_warn("All weights are missing. Using unweighted calculation.")
    return(list(x = x, weights = NULL, valid = FALSE))
  }
  
  # Remove missing values if requested
  if (na.rm) {
    complete_cases <- !is.na(x) & !is.na(weights)
    x_clean <- x[complete_cases]
    weights_clean <- weights[complete_cases]
  } else {
    x_clean <- x
    weights_clean <- weights
  }
  
  return(list(x = x_clean, weights = weights_clean, valid = TRUE))
}

# .effective_n() is defined in helpers.R — no local redefinition needed

#' Weighted mean
#' @noRd
.w_mean <- function(x, weights = NULL, na.rm = TRUE) {
  cleaned <- .validate_and_clean_weights(x, weights, na.rm)
  
  if (!cleaned$valid || is.null(cleaned$weights)) {
    return(mean(cleaned$x, na.rm = na.rm))
  }
  
  if (sum(cleaned$weights, na.rm = TRUE) == 0) {
    return(NA_real_)
  }
  
  return(sum(cleaned$x * cleaned$weights, na.rm = na.rm) / sum(cleaned$weights, na.rm = na.rm))
}

#' Weighted median using cumulative weights approach
#' @noRd
.w_median <- function(x, weights = NULL, na.rm = TRUE) {
  # SPSS's median IS the HAVERAGE (Type-6) 50th percentile. Delegating to
  # .w_quantile() guarantees Median == Q50 in every output; the former
  # cumulative-weight step function disagreed with .w_quantile(0.5) for
  # fractional weights (audit finding).
  unname(.w_quantile(x, weights = weights, probs = 0.5, na.rm = na.rm))
}

#' Weighted variance with Bessel's correction
#' @noRd
.w_var <- function(x, weights = NULL, na.rm = TRUE) {
  cleaned <- .validate_and_clean_weights(x, weights, na.rm)
  
  if (!cleaned$valid || is.null(cleaned$weights)) {
    return(var(cleaned$x, na.rm = na.rm))
  }
  
  x_clean <- cleaned$x
  w_clean <- cleaned$weights
  
  if (length(x_clean) <= 1) return(NA_real_)
  
  # Calculate weighted variance using SPSS formula
  # SPSS uses: variance = sum(w * (x - w_mean)^2) / (V1 - 1)
  # where V1 = sum of weights
  w_mean <- .w_mean(x_clean, w_clean, na.rm = na.rm)
  V1 <- sum(w_clean, na.rm = na.rm)  # Sum of weights
  
  if (V1 <= 1) return(NA_real_)  # Need V1 > 1 for denominator
  
  numerator <- sum(w_clean * (x_clean - w_mean)^2, na.rm = na.rm)
  denominator <- V1 - 1  # SPSS formula: V1 - 1
  
  return(numerator / denominator)
}

#' Weighted standard deviation
#' @noRd
.w_sd <- function(x, weights = NULL, na.rm = TRUE) {
  return(sqrt(.w_var(x, weights, na.rm)))
}

#' Weighted standard error using SPSS formula
#' @noRd
.w_se <- function(x, weights = NULL, na.rm = TRUE) {
  cleaned <- .validate_and_clean_weights(x, weights, na.rm)
  
  if (!cleaned$valid || is.null(cleaned$weights)) {
    n <- sum(!is.na(x))
    return(sd(x, na.rm = na.rm) / sqrt(n))
  }
  
  # Calculate weighted standard error using SPSS formula
  # SPSS uses: SE = SD / sqrt(V1) where V1 = sum of weights
  V1 <- sum(cleaned$weights, na.rm = na.rm)
  
  if (V1 <= 0) return(NA_real_)
  
  w_sd <- .w_sd(cleaned$x, cleaned$weights, na.rm = na.rm)
  return(w_sd / sqrt(V1))  # SPSS formula: SD / sqrt(V1)
}

#' Weighted range (uses actual data range)
#' @noRd
.w_range <- function(x, weights = NULL, na.rm = TRUE) {
  cleaned <- .validate_and_clean_weights(x, weights, na.rm)
  
  if (!cleaned$valid || is.null(cleaned$weights)) {
    return(diff(range(cleaned$x, na.rm = na.rm)))
  }
  
  # For weighted range, we still use the actual range of values
  return(diff(range(cleaned$x, na.rm = na.rm)))
}

#' Weighted IQR using weighted quantiles
#' @noRd
.w_iqr <- function(x, weights = NULL, na.rm = TRUE) {
  q75 <- .w_quantile(x, weights, probs = 0.75, na.rm = na.rm)
  q25 <- .w_quantile(x, weights, probs = 0.25, na.rm = na.rm)
  return(q75 - q25)
}

#' Weighted quantiles (SPSS Type-6 HAVERAGE with linear interpolation)
#'
#' For weighted data, weights are treated as frequency multipliers. The
#' Type-6 position is h = p * (W + 1) where W = sum(w). The percentile is
#' linearly interpolated between the values whose cumulative weights bracket h.
#' Matches IBM SPSS FREQUENCIES /PERCENTILES (HAVERAGE) algorithm.
#'
#' For unweighted data, delegates to stats::quantile(type = 6) which is the
#' SPSS-compatible default (HAVERAGE) — base R's default is type = 7.
#'
#' @noRd
.w_quantile <- function(x, weights = NULL, probs = 0.5, na.rm = TRUE) {
  cleaned <- .validate_and_clean_weights(x, weights, na.rm)

  if (!cleaned$valid || is.null(cleaned$weights)) {
    # SPSS uses Type-6 (HAVERAGE), not R's default Type-7
    return(quantile(cleaned$x, probs = probs, na.rm = na.rm, type = 6))
  }

  x_clean <- cleaned$x
  w_clean <- cleaned$weights

  if (length(x_clean) == 0) return(rep(NA_real_, length(probs)))

  ord      <- order(x_clean)
  x_sorted <- x_clean[ord]
  w_sorted <- w_clean[ord]
  cum_w    <- cumsum(w_sorted)
  W        <- sum(w_sorted)

  result <- vapply(probs, function(p) {
    # Type-6 position
    h <- p * (W + 1)

    if (h <= cum_w[1])               return(x_sorted[1])
    if (h >= cum_w[length(cum_w)])   return(x_sorted[length(x_sorted)])

    # Find the bracket: cum_w[j-1] < h <= cum_w[j]
    j <- which(cum_w >= h)[1]
    if (j == 1L) return(x_sorted[1])

    lo_w <- cum_w[j - 1]
    hi_w <- cum_w[j]
    if (hi_w == lo_w) return(x_sorted[j])

    # Linear interpolation between adjacent observations
    frac <- (h - lo_w) / (hi_w - lo_w)
    x_sorted[j - 1] + frac * (x_sorted[j] - x_sorted[j - 1])
  }, numeric(1))

  names(result) <- paste0(probs * 100, "%")
  result
}

#' Weighted skewness with bias correction
#' @noRd
.w_skew <- function(x, weights = NULL, na.rm = TRUE) {
  cleaned <- .validate_and_clean_weights(x, weights, na.rm)
  if (!cleaned$valid || is.null(cleaned$weights)) {
    xc <- if (na.rm) x[!is.na(x)] else x
    if (length(xc) < 3) return(NA_real_)
    return(.calc_skewness(xc, w = NULL))
  }
  if (length(cleaned$x) < 3) return(NA_real_)
  .calc_skewness(cleaned$x, w = cleaned$weights)
}

#' Weighted kurtosis (SPSS Type-2; delegates to .calc_kurtosis)
#' @noRd
.w_kurtosis <- function(x, weights = NULL, na.rm = TRUE, excess = TRUE) {
  cleaned <- .validate_and_clean_weights(x, weights, na.rm)
  if (!cleaned$valid || is.null(cleaned$weights)) {
    xc <- if (na.rm) x[!is.na(x)] else x
    if (length(xc) < 4) return(NA_real_)
    return(.calc_kurtosis(xc, w = NULL, excess = excess))
  }
  if (length(cleaned$x) < 4) return(NA_real_)
  .calc_kurtosis(cleaned$x, w = cleaned$weights, excess = excess)
}

#' Weighted mode (most frequent value by weight)
#' @noRd
.w_mode <- function(x, weights = NULL, na.rm = TRUE) {
  cleaned <- .validate_and_clean_weights(x, weights, na.rm)
  
  if (!cleaned$valid || is.null(cleaned$weights)) {
    # Unweighted mode
    x_clean <- if (na.rm) x[!is.na(x)] else x
    if (length(x_clean) == 0) return(NA_real_)
    
    freq_table <- table(x_clean)
    mode_val <- as.numeric(names(freq_table)[which.max(freq_table)])
    return(mode_val)
  }
  
  # Weighted mode
  x_clean <- cleaned$x
  w_clean <- cleaned$weights
  
  if (length(x_clean) == 0) return(NA_real_)
  
  unique_vals <- unique(x_clean)
  weighted_freqs <- numeric(length(unique_vals))
  
  for (i in seq_along(unique_vals)) {
    weighted_freqs[i] <- sum(w_clean[x_clean == unique_vals[i]], na.rm = na.rm)
  }
  
  mode_val <- unique_vals[which.max(weighted_freqs)]
  return(mode_val)
}
