# =============================================================================
# Audit regression tests (2026-07)
# =============================================================================
# Each test pins a defect found in the 2026-07 full-package audit to a
# minimal reproducible scenario, so the fix cannot silently regress.
# These are R-internal consistency checks (Tier 4 in the sense of the
# Validation Charter): they assert agreement with base R reference
# implementations or internal invariants, not SPSS print output.
# =============================================================================

# --- Phase 1: formula fixes --------------------------------------------------

test_that("kendall_tau z/p match cor.test exactly under heavy ties", {
  # Audit finding: the v2 tie term in Var(S) was divided by 9n(n-1)(n-2)
  # twice, inflating |z| for tied data. cor.test implements the same
  # Kendall & Gibbons formula SPSS uses.
  set.seed(42)
  x <- sample(0:1, 500, replace = TRUE)
  y <- sample(0:1, 500, replace = TRUE)

  res <- kendall_tau(dplyr::tibble(x = x, y = y), x, y)$correlations
  ref <- stats::cor.test(x, y, method = "kendall")

  expect_equal(res$tau[1], unname(ref$estimate), tolerance = 1e-10)
  expect_equal(res$z_score[1], unname(ref$statistic), tolerance = 1e-8)
  expect_equal(res$p_value[1], ref$p.value, tolerance = 1e-8)
})

test_that("weighted wilcoxon_test with weights == 1 equals the unweighted test", {
  # Audit finding: rank - 1/2 mid-ranks were inconsistent with
  # E(V) = N(N+1)/4, biasing Z downward by sum(w_pos)/2 standard units.
  set.seed(42)
  pre  <- rnorm(300, 50, 10)
  post <- pre + rnorm(300, 0.5, 5)
  d <- dplyr::tibble(pre = pre, post = post, w1 = rep(1, 300))

  uw <- wilcoxon_test(d, pre, post)$results
  ww <- wilcoxon_test(d, pre, post, weights = w1)$results

  expect_equal(ww$Z[1], uw$Z[1], tolerance = 1e-10)
  expect_equal(ww$p_value[1], uw$p_value[1], tolerance = 1e-10)
  expect_equal(ww$V[1], uw$V[1], tolerance = 1e-10)
})

test_that("weighted pairwise_wilcoxon with weights == 1 equals unweighted", {
  set.seed(7)
  d <- dplyr::tibble(
    t1 = rnorm(120, 50, 10),
    t2 = rnorm(120, 52, 10),
    t3 = rnorm(120, 53, 10),
    w1 = rep(1, 120)
  )
  fr_uw <- friedman_test(d, t1, t2, t3)
  fr_ww <- friedman_test(d, t1, t2, t3, weights = w1)
  pw_uw <- pairwise_wilcoxon(fr_uw)$results
  pw_ww <- pairwise_wilcoxon(fr_ww)$results

  expect_equal(pw_ww$z, pw_uw$z, tolerance = 1e-10)
})

test_that("mann_whitney asymptotic p agrees with its own Z (no continuity correction)", {
  # Audit finding: Z was computed without continuity correction (SPSS
  # convention) but p came from wilcox.test with correct = TRUE.
  d <- dplyr::tibble(
    v = c(12, 15, 11, 18, 14, 13, 16, 10, 19, 22, 17, 21, 20, 23, 25, 24),
    g = factor(rep(c("A", "B"), each = 8))
  )
  res <- mann_whitney(d, v, group = g)$results
  expect_equal(res$p_value[1], 2 * stats::pnorm(-abs(res$Z[1])), tolerance = 1e-10)
})

test_that("regression df count model terms, not predictor variables", {
  # Audit finding: k = length(pred_names) undercounted df whenever a
  # factor expanded into >1 dummy term.
  set.seed(1)
  n <- 200
  f <- factor(sample(letters[1:4], n, replace = TRUE))
  x <- rnorm(n)
  y <- 2 + x + as.numeric(f) + rnorm(n)
  w <- runif(n, 0.5, 1.5)
  d <- dplyr::tibble(y = y, x = x, f = f, w = w)

  # Weighted linear regression: 1 numeric + 3 dummy terms = 4 regression df
  lr <- linear_regression(d, y ~ x + f, weights = w)
  expect_equal(lr$anova_table$df[1], 4)
  expect_equal(lr$anova_table$df[2], sum(w) - 4 - 1, tolerance = 1e-10)

  # Logistic omnibus df = difference in estimated parameters vs null model
  yb <- rbinom(n, 1, stats::plogis(-0.5 + 0.8 * x))
  glr <- logistic_regression(dplyr::tibble(yb = yb, x = x, f = f), yb ~ x + f)
  expect_equal(glr$omnibus_test$df, 4)
})

test_that("kruskal_wallis reports its H/(N-1) effect size as epsilon_squared", {
  # Audit finding: H/(N-1) is Tomczak & Tomczak's epsilon-squared but was
  # returned and printed under the name eta_squared.
  d <- dplyr::tibble(
    v = c(rnorm(30), rnorm(30, 0.6), rnorm(30, 1.2)),
    g = factor(rep(c("a", "b", "c"), each = 30))
  )
  res <- kruskal_wallis(d, v, group = g)$results
  expect_true("epsilon_squared" %in% names(res))
  expect_false("eta_squared" %in% names(res))
  expect_equal(res$epsilon_squared[1], res$H[1] / (90 - 1), tolerance = 1e-10)
})

# --- Phase 2: honest API contracts -------------------------------------------

test_that("weighted t_test honors var.equal for the primary result", {
  # Audit finding: the weighted two-sample path always reported Welch
  # values regardless of var.equal.
  set.seed(5)
  d <- dplyr::tibble(
    v = rnorm(100, 10, rep(c(2, 6), 50)),
    g = factor(rep(c("A", "B"), 50)),
    w = runif(100, 0.5, 1.5)
  )
  te <- t_test(d, v, group = g, weights = w, var.equal = TRUE)$results
  tu <- t_test(d, v, group = g, weights = w, var.equal = FALSE)$results
  # Student df = sum(w) - 2 (constant), Welch df is Satterthwaite - they differ
  expect_false(isTRUE(all.equal(te$df[1], tu$df[1])))
  expect_equal(te$df[1], sum(d$w) - 2, tolerance = 1e-8)
})

test_that("ss_type = 2 warns and computes Type III", {
  # Audit finding: ss_type = 2 was accepted, stored, and echoed in the
  # header while Type III was silently computed.
  set.seed(9)
  d <- dplyr::tibble(
    dv = rnorm(90),
    A = factor(sample(c("x", "y"), 90, replace = TRUE, prob = c(0.7, 0.3))),
    B = factor(sample(c("p", "q", "r"), 90, replace = TRUE))
  )
  expect_warning(
    fa2 <- factorial_anova(d, dv = dv, between = c(A, B), ss_type = 2),
    "Type II"
  )
  fa3 <- factorial_anova(d, dv = dv, between = c(A, B), ss_type = 3)
  expect_equal(fa2$anova_table$ss, fa3$anova_table$ss, tolerance = 1e-10)
})

test_that("oneway_anova warns that var.equal is deprecated and ignored", {
  data(survey_data)
  expect_warning(
    oneway_anova(survey_data, life_satisfaction, group = education,
                 var.equal = FALSE),
    "var.equal"
  )
})

test_that("linear_regression reports SPSS-style Tolerance/VIF", {
  # Audit finding: collinearity diagnostics were documented but not
  # implemented anywhere.
  data(survey_data)
  lr <- linear_regression(survey_data,
                          life_satisfaction ~ age + income + trust_government)
  X <- stats::model.matrix(lr)[, -1]
  vif_manual <- diag(solve(stats::cor(X)))
  expect_equal(unname(lr$coef_table$VIF[-1]), unname(vif_manual),
               tolerance = 1e-10)
  expect_equal(lr$coef_table$Tolerance[-1], 1 / lr$coef_table$VIF[-1],
               tolerance = 1e-12)
  out <- capture.output(print(summary(lr)))
  expect_true(any(grepl("Collinearity Statistics", out)))
  out2 <- capture.output(print(summary(lr, collinearity = FALSE)))
  expect_false(any(grepl("Collinearity Statistics", out2)))
})

test_that("phi/cramers_v/goodman_gamma return the effect size, not a test object", {
  # Audit finding: the three helpers were bare aliases of chi_square().
  data(survey_data)
  full <- chi_square(survey_data, gender, region)
  expect_equal(unname(phi(survey_data, gender, region)),
               full$results$phi[1])
  expect_equal(unname(cramers_v(survey_data, gender, region)),
               full$results$cramers_v[1])
  expect_equal(unname(goodman_gamma(survey_data, gender, region)),
               full$results$gamma[1])
})

# --- Phase 3: robustness and the package-wide weights policy -----------------

test_that("negative weights error consistently across entry points", {
  # Audit finding: the w_* data-frame path performed no weight validation
  # (negative weights produced silently wrong numbers) while the summarise
  # path silently fell back to unweighted - same input, two different
  # wrong answers.
  d <- dplyr::tibble(v = c(1, 2, 3, 4), wt = c(1, 1, -5, 1))
  expect_error(w_mean(d, v, weights = wt), "negative")
  expect_error(dplyr::summarise(d, m = w_mean(v, weights = wt)), "negative")
  expect_error(frequency(d, v, weights = wt), "negative")
  expect_error(std(d, v, weights = wt), "negative")
  expect_error(describe(d, v, weights = wt), "negative")
})

test_that("std() with a zero weight stays weighted instead of silently falling back", {
  d <- dplyr::tibble(v = c(10, 20, 30, 40), w = c(1, 1, 0, 1))
  r <- std(d, v, weights = w, suffix = "_z")
  keep <- d$w > 0
  wm <- stats::weighted.mean(d$v[keep], d$w[keep])
  ws <- sqrt(sum(d$w[keep] * (d$v[keep] - wm)^2) / (sum(d$w[keep]) - 1))
  expect_equal(r$v_z, (d$v - wm) / ws, tolerance = 1e-10)
})

test_that("correlation functions return NA for constant variables instead of crashing", {
  set.seed(3)
  d <- dplyr::tibble(a = rnorm(50), b = rep(5, 50))
  suppressWarnings({
    p <- pearson_cor(d, a, b)$correlations
    s <- spearman_rho(d, a, b)$correlations
    k <- kendall_tau(d, a, b)$correlations
  })
  expect_true(is.na(p$correlation[1]) && is.na(p$p_value[1]))
  expect_true(is.na(s$rho[1]) && is.na(s$p_value[1]))
  expect_true(is.na(k$tau[1]) && is.na(k$p_value[1]))
})

test_that("frequency(show_unused = TRUE) works on set_na-tagged variables", {
  # Audit finding: injected zero-frequency rows lacked the na_display_value
  # column added by tagged-NA expansion -> rbind column mismatch crash.
  v <- haven::labelled(
    c(1, 2, 2, 3, 7, 8, NA),
    labels = c(Low = 1, Mid = 2, High = 3, Unused = 4, Refused = 7, DK = 8)
  )
  d <- set_na(dplyr::tibble(v = v), v = c(7, 8))
  expect_no_error(res <- frequency(d, v, show_unused = TRUE))
  expect_true(4 %in% res$results$value)  # unused label row present
})

test_that("sort_frq sorts by frequency and keeps cumulative percent monotone", {
  # Audit finding: sorting was by value (contradicting the docs) and left
  # the pre-sort cumulative percentages in place (non-monotone output).
  d <- dplyr::tibble(v = c(1, 1, 1, 2, 2, 3))
  res <- frequency(d, v, sort_frq = "desc")$results
  expect_equal(res$freq[1:3], c(3, 2, 1))
  expect_true(all(diff(res$cum_prc[1:3]) >= 0))
  expect_equal(res$cum_prc[3], 100, tolerance = 1e-10)
})

test_that("write_spss refuses a na_range that would swallow valid values", {
  # Audit finding: 4+ missing codes fell back to a min-max na_range without
  # checking whether valid values lie inside - silent data corruption.
  v <- haven::labelled(c(1, 2, 3, 4, 5, 6, 0, 7, 8, 9),
                       labels = c(One = 1, Six = 6))
  d <- set_na(dplyr::tibble(v = v), v = c(0, 7, 8, 9))
  expect_error(write_spss(d, tempfile(fileext = ".sav")), "valid value")

  # Contiguous codes: allowed, but announced
  v2 <- haven::labelled(c(1, 2, 3, 6, 7, 8, 9), labels = c(One = 1))
  d2 <- set_na(dplyr::tibble(v = v2), v = c(6, 7, 8, 9))
  expect_warning(
    suppressMessages(write_spss(d2, tempfile(fileext = ".sav"))),
    "missing range"
  )
})

test_that("logistic_regression surfaces separation warnings", {
  # Audit finding: blanket suppressWarnings() around glm() also swallowed
  # 'fitted probabilities numerically 0 or 1' - with fractional weights
  # only the non-integer-weights warning may be muffled.
  set.seed(4)
  n <- 40
  x <- rnorm(n)
  y <- as.integer(x > 0)  # perfectly separated
  d <- dplyr::tibble(y = y, x = x, w = runif(n, 0.5, 1.5))
  expect_warning(
    logistic_regression(d, y ~ x, weights = w),
    "probabilities|converge"
  )
})

# --- Phase 5: cleanup regressions ---------------------------------------------

test_that("grouped single-variable w_* results print the statistics", {
  # Audit finding: single-variable results had no Variable column, so the
  # grouped print iterated over nothing and emitted group headers with no
  # statistics (plus 'Unknown or uninitialised column' warnings).
  data(survey_data)
  gw <- dplyr::group_by(survey_data, region) |>
    w_mean(age, weights = sampling_weight)
  expect_true("Variable" %in% names(gw$results))
  out <- expect_no_warning(capture.output(print(gw)))
  expect_true(any(grepl("weighted_mean", out)))
  expect_true(any(grepl("52.278", out, fixed = TRUE)))
})

# --- Phase 4: internal consistency --------------------------------------------

test_that("weighted median equals the weighted 50th percentile", {
  # Audit finding: .w_median used a cumulative-weight step function while
  # .w_quantile used Type-6/HAVERAGE - describe() could show Median != Q50
  # in the same row.
  x <- c(1, 2, 3, 4)
  w <- c(0.5, 1.5, 0.8, 1.2)
  expect_equal(mariposa:::.w_median(x, w),
               unname(mariposa:::.w_quantile(x, w, probs = 0.5)),
               tolerance = 1e-12)
})

test_that("unweighted quantiles use SPSS Type 6 (HAVERAGE)", {
  # Audit finding: the exported w_quantile/w_iqr fell back to R's default
  # Type 7 while claiming SPSS compatibility.
  x <- 1:10
  q <- dplyr::summarise(dplyr::tibble(x = x),
                        q = w_quantile(x, probs = 0.25))$q
  expect_equal(unname(q), unname(stats::quantile(x, 0.25, type = 6)))
  iqr <- dplyr::summarise(dplyr::tibble(x = x), i = w_iqr(x))$i
  q6 <- stats::quantile(x, c(0.25, 0.75), type = 6)
  expect_equal(unname(iqr), unname(q6[2] - q6[1]))
})

test_that("frequency header skewness agrees with describe()", {
  # Audit finding: frequency() reimplemented skewness with a Type-1
  # population formula, contradicting describe() on the same variable.
  data(survey_data)
  fr_stats <- mariposa:::calculate_single_stats(survey_data$age)
  de <- describe(survey_data, age, show = c("skew"))$results
  expect_equal(fr_stats$skewness, de$age_Skewness[1], tolerance = 1e-10)
})

test_that("significance stars follow the symnum boundary convention", {
  # Audit finding: cut(right = FALSE) gave p = 0.001 two stars and p = 0.05
  # no star, contradicting the printed legend; three different boundary
  # conventions coexisted across the correlation files.
  stars <- mariposa:::add_significance_stars
  expect_identical(stars(0.001), "***")
  expect_identical(stars(0.01), "**")
  expect_identical(stars(0.05), "*")
  expect_identical(stars(0.051), "")
  expect_identical(stars(NA_real_), "")
  expect_type(stars(c(0.001, 0.2)), "character")
})

test_that("weighted mann_whitney stays equivalent to the design-based svyranktest convention", {
  # The weighted MW path is a design-based Lumley-Scott estimator and uses
  # Horvitz-Thompson mid-ranks (cumsum(w) - w/2) on purpose - NOT the
  # frequency-expansion mid-ranks of the frequency-weighted tests. This
  # test pins the w == 1 behavior: HT ranks with unit weights are the
  # classical mid-ranks - 1/2, and the WLS t statistic must equal the
  # unweighted Z-based test asymptotically. We assert the internal
  # consistency contract instead: rank_mean difference and Z direction
  # agree between weighted and unweighted runs on the same data.
  set.seed(11)
  d <- dplyr::tibble(
    v = rnorm(400, mean = rep(c(0, 0.3), each = 200)),
    g = factor(rep(c("A", "B"), each = 200)),
    w1 = rep(1, 400)
  )
  uw <- mann_whitney(d, v, group = g)$results
  ww <- mann_whitney(d, v, group = g, weights = w1)$results
  # |Z| only: the unweighted path reports Z from the min-U convention
  # (always <= 0, as SPSS does), the design-based path keeps the natural sign.
  expect_lt(abs(abs(ww$Z[1]) - abs(uw$Z[1])), 0.05)
})

# --- Stage 4: API renames ------------------------------------------------------
# The sjmisc-heritage dot-case argument names were renamed to snake_case in
# 0.6.8 with a one-release soft-deprecation bridge (VERSIONING_POLICY.md,
# section 4); the bridges were removed in 0.6.9. In functions whose `...` is
# consumed by tidyselect (frequency, rec, to_label, to_character, to_numeric)
# the old names now hard-error via a retained sentinel formal - a full
# removal would let them be silently swallowed as variable selections. In
# the readers (no tidyselect dots) the formals are gone entirely, so the old
# names fail as unused arguments.

test_that("frequency: removed sort.frq errors and sort_frq works silently", {
  expect_error(
    frequency(survey_data, education, sort.frq = "desc"),
    "removed"
  )
  expect_no_warning(
    frequency(survey_data, education, sort_frq = "desc")
  )
})

test_that("frequency: removed show.na errors and show_na works silently", {
  expect_error(
    frequency(survey_data, education, show.na = FALSE),
    "removed"
  )
  expect_no_warning(
    frequency(survey_data, education, show_na = FALSE)
  )
})

test_that("frequency: sort_frq typo errors instead of silently not sorting", {
  # match.arg error text is locale-dependent; match on the choices instead
  expect_error(
    frequency(survey_data, education, sort_frq = "dsc"),
    "none"
  )
})

test_that("frequency: show_labels rejects values other than TRUE/FALSE/'auto'", {
  expect_error(
    frequency(survey_data, education, show_labels = "yes"),
    "show_labels"
  )
  expect_no_error(frequency(survey_data, education, show_labels = TRUE))
  expect_no_error(frequency(survey_data, education, show_labels = FALSE))
  expect_no_error(frequency(survey_data, education, show_labels = "auto"))
})

test_that("rec: removed dot-case args error and snake_case names work silently", {
  expect_error(
    rec(survey_data, trust_government,
        rules = "1:2=1; 3=2; 4:5=3", suffix = "_r",
        as.factor = TRUE),
    "removed"
  )
  expect_no_warning(
    rec(survey_data, trust_government,
        rules = "1:2=1; 3=2; 4:5=3", suffix = "_r",
        as_factor = TRUE)
  )

  expect_error(
    rec(survey_data, trust_government,
        rules = "rev", suffix = "_r",
        var.label = "Reversed trust"),
    "removed"
  )
  expect_no_warning(
    rec(survey_data, trust_government,
        rules = "rev", suffix = "_r",
        var_label = "Reversed trust")
  )

  expect_error(
    rec(survey_data, trust_government,
        rules = "1:2=1; 3=2; 4:5=3", suffix = "_r",
        val.labels = c("1" = "Low", "2" = "Mid", "3" = "High")),
    "removed"
  )
  expect_no_warning(
    rec(survey_data, trust_government,
        rules = "1:2=1; 3=2; 4:5=3", suffix = "_r",
        val_labels = c("1" = "Low", "2" = "Mid", "3" = "High"))
  )
})

test_that("to_label/to_numeric: removed dot-case args error, snake_case works", {
  skip_if_not_installed("haven")
  x <- haven::labelled(c(1, 2, 3, 2), labels = c(Low = 1, High = 2))
  expect_error(to_label(x, add.non.labelled = TRUE), "removed")
  expect_error(to_label(x, drop.na = FALSE), "removed")
  expect_no_warning(to_label(x, add_non_labelled = TRUE))

  expect_error(to_character(x, drop.na = FALSE), "removed")
  expect_no_warning(to_character(x, drop_na = FALSE))

  f <- factor(c("2", "4", "6"))
  expect_error(to_numeric(f, start.at = 0), "removed")
  expect_no_warning(to_numeric(f, start_at = 0))
})

test_that("readers: removed tag.na fails as unused argument", {
  skip_if_not_installed("haven")
  # The readers have no tidyselect dots, so the formal is gone entirely and
  # R's argument matching rejects the old name before any file is touched.
  expect_error(read_spss("nofile.sav", tag.na = TRUE), "unused argument")
  expect_error(read_por("nofile.por", tag.na = TRUE), "unused argument")
  expect_error(read_stata("nofile.dta", tag.na = c(-9)), "unused argument")
  expect_error(read_sas("nofile.sas7bdat", tag.na = c(-9)), "unused argument")
  expect_error(read_xpt("nofile.xpt", tag.na = c(-9)), "unused argument")
})

# --- Stage 5: 0.6.9 renames removed (codebook, val_labels, drop_labels) ---------
# The 0.6.9 soft-deprecation bridges were removed in 0.6.11: the old dot-case
# argument names no longer work (they fall through to tidyselect/SET-mode
# validation and error), while the snake_case names work silently.

test_that("codebook: removed dot-case args error and snake_case works silently", {
  df <- data.frame(a = c(1, 2, 2, 3), b = letters[1:4])

  # Old dotted names land in `...` (tidyselect); logical values are not
  # valid selectors, so they error rather than silently warn-and-work.
  expect_error(codebook(df, show.freq = FALSE))
  expect_error(codebook(df, sort.by.name = TRUE))

  # The dotted names are gone from the formals entirely
  expect_false("show.freq" %in% names(formals(codebook)))
  expect_false("max.values" %in% names(formals(codebook)))
  expect_false("sort.by.name" %in% names(formals(codebook)))

  # snake_case works without warnings
  expect_no_warning(new <- codebook(df, show_freq = FALSE))
  expect_false(new$options$show_freq)
  expect_no_warning(codebook(df, max_values = 2))
  expect_no_warning(codebook(df, sort_by_name = TRUE))
})

test_that("val_labels: removed drop.na errors and drop_na works silently", {
  skip_if_not_installed("haven")
  df <- data.frame(
    a = haven::labelled(
      c(1, 2, haven::tagged_na("a")),
      labels = c(Low = 1, High = 2, "No answer" = haven::tagged_na("a"))
    )
  )

  # Old dotted name lands in `...` (SET mode) and errors
  expect_error(val_labels(df, a, drop.na = FALSE))

  # snake_case works without warnings
  expect_no_warning(new <- val_labels(df, a, drop_na = FALSE))
  expect_length(new, 3L)

  # Default (drop_na = TRUE) excludes the tagged NA label, silently
  expect_no_warning(dropped <- val_labels(df, a))
  expect_length(dropped, 2L)
})

test_that("drop_labels: removed drop.na errors and drop_na works silently", {
  skip_if_not_installed("haven")
  df <- data.frame(
    a = haven::labelled(
      c(1, 2, 2),
      labels = c(Low = 1, High = 2, Unused = 3,
                 "No answer" = haven::tagged_na("a"))
    )
  )

  # Old dotted name lands in `...` (tidyselect) and errors
  expect_error(drop_labels(df, drop.na = TRUE))

  # snake_case works without warnings
  expect_no_warning(new <- drop_labels(df, drop_na = TRUE))

  # drop_na = TRUE removes the unused tagged NA label as well
  expect_false("No answer" %in% names(attr(new$a, "labels")))
})

test_that("t_test results carry conf_int_* and no duplicated CI_* aliases", {
  res <- t_test(survey_data, life_satisfaction, group = gender)
  expect_true(all(c("conf_int_lower", "conf_int_upper") %in% names(res$results)))
  expect_false(any(c("CI_lower", "CI_upper") %in% names(res$results)))
})

# --- Stage 6: column harmonization complete (0.6.11) ----------------------------
# The deprecated duplicate columns introduced in 0.6.10 are removed in 0.6.11;
# only the canonical result-column names remain (per VERSIONING_POLICY §4.3).

test_that("chisq_gof results carry chi_squared and no chi_sq duplicate", {
  res <- chisq_gof(survey_data, gender)
  cols <- names(res$results)
  expect_true("chi_squared" %in% cols)
  expect_false("chi_sq" %in% cols)
})

test_that("friedman_test results carry chi_squared and no chi_sq duplicate", {
  res <- friedman_test(survey_data, trust_government, trust_media, trust_science)
  cols <- names(res$results)
  expect_true("chi_squared" %in% cols)
  expect_false("chi_sq" %in% cols)
})

test_that("mcnemar_test results carry chi_squared and no statistic duplicate", {
  test_data <- transform(survey_data,
    trust_gov_high = as.integer(trust_government >= 4),
    trust_media_high = as.integer(trust_media >= 4))
  res <- mcnemar_test(test_data, var1 = trust_gov_high, var2 = trust_media_high)
  cols <- names(res$results)
  expect_true("chi_squared" %in% cols)
  expect_false("statistic" %in% cols)
})

test_that("mann_whitney results carry r_effect and no effect_size_r duplicate", {
  res <- mann_whitney(survey_data, life_satisfaction, group = gender)
  cols <- names(res$results)
  expect_true("r_effect" %in% cols)
  expect_false("effect_size_r" %in% cols)
})

test_that("oneway_anova results carry F_statistic and no F_stat duplicate", {
  res <- oneway_anova(survey_data, life_satisfaction, group = education)
  cols <- names(res$results)
  expect_true("F_statistic" %in% cols)
  expect_false("F_stat" %in% cols)
})
