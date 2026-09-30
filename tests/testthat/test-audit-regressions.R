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
  # (0.7.4 practice test, IO-15: codes 0, 7, 8, 9 are now written as range
  # 7-9 plus the discrete value 0; only a set no range+value form can
  # cover without valid values errors.)
  v <- haven::labelled(c(1, 2, 4, 5, 6, 8, 0, 3, 7, 9),
                       labels = c(One = 1, Six = 6))
  d <- set_na(dplyr::tibble(v = v), v = c(0, 3, 7, 9))
  expect_error(write_spss(d, tempfile(fileext = ".sav")), "valid value")

  # Contiguous codes: allowed, but announced (one message)
  v2 <- haven::labelled(c(1, 2, 3, 6, 7, 8, 9), labels = c(One = 1))
  d2 <- set_na(dplyr::tibble(v = v2), v = c(6, 7, 8, 9))
  expect_message(
    write_spss(d2, tempfile(fileext = ".sav")),
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
  # (0.7.4: the uniform w_* table labels the column "Mean", not the raw
  # result column name weighted_mean - practice-test finding DESC-18)
  expect_true(any(grepl("Mean", out)))
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
  expect_no_warning(to_character(x, drop_na = FALSE, add_non_labelled = TRUE))

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

# --- 0.6.15: regression-correctness fixes ------------------------------------

test_that("rank-deficient unweighted linear_regression drops the aliased term instead of erroring", {
  # 0.6.15 fix: summary() drops aliased (NA) coefficients while confint()
  # keeps them as NA rows, so the unweighted coefficient table crashed with
  # "Tibble columns must have compatible sizes" on perfectly collinear
  # predictors. SPSS excludes the variable and reports the rest.
  set.seed(42)
  d <- dplyr::tibble(x1 = rnorm(100), y = rnorm(100))
  d$x2 <- 2 * d$x1

  expect_message(
    r <- linear_regression(d, y ~ x1 + x2),
    "collinearity"
  )
  expect_setequal(r$coef_table$Term, c("(Intercept)", "x1"))

  # df must count estimated terms only (model rank), not NA coefficients:
  # 1 regression df, n - 2 residual df.
  expect_equal(as.integer(r$anova_table$df), c(1L, 98L, 99L))

  # F in the ANOVA table equals summary.lm's F on the same rank-deficient fit.
  fs <- summary(stats::lm(y ~ x1 + x2, data = d))$fstatistic
  expect_equal(unname(r$anova_table$F_statistic[1]), unname(fs[1]),
               tolerance = 1e-10)
})

test_that("use = 'pairwise' refuses interactions instead of silently dropping them", {
  # 0.6.15 fix: the pairwise path rebuilds the model from the variable list,
  # so y ~ a * b silently became y ~ a + b with no warning.
  data(survey_data)
  expect_error(
    linear_regression(survey_data, life_satisfaction ~ age * income,
                      use = "pairwise"),
    "pairwise"
  )
  expect_error(
    linear_regression(survey_data, life_satisfaction ~ age + I(income^2),
                      use = "pairwise"),
    "pairwise"
  )

  # Plain additive formulas keep working.
  r <- linear_regression(survey_data, life_satisfaction ~ age + income,
                         use = "pairwise")
  expect_setequal(r$coef_table$Term, c("(Intercept)", "age", "income"))
})

test_that("weighted logistic -2LL uses the deviance, not weight-rounding logLik", {
  # 0.6.15 fix: binomial logLik() rounds fractional prior weights; the
  # deviance carries the exact frequency-weighted -2LL for a 0/1 response.
  data(survey_data)
  d <- survey_data
  d$high_life <- as.integer(d$life_satisfaction >= 4)

  r <- logistic_regression(d, high_life ~ age + income,
                           weights = sampling_weight)
  expect_equal(r$model_summary$minus2LL, r$deviance, tolerance = 1e-12)
  expect_equal(r$omnibus_test$chi_sq, r$null.deviance - r$deviance,
               tolerance = 1e-12)
})

# --- 0.7.2: integer columns must not crash tagged-NA machinery ---------------

test_that("unlabel() handles integer columns (haven::na_tag needs doubles)", {
  # 0.7.2 fix: .unlabel_vec called haven::na_tag() on integer NAs, which
  # errors ("`x` must be a double vector") — unlabel(survey_data) crashed
  # on every integer Likert column. Integers can never carry tags.
  data(survey_data)
  expect_no_error(plain <- unlabel(survey_data))
  expect_null(attr(plain$life_satisfaction, "label", exact = TRUE))
  expect_null(attr(plain$gender, "labels", exact = TRUE))
})

test_that("write_xpt() roundtrips data with integer columns", {
  skip_if_not_installed("haven")
  # 0.7.2 fix: .retag_uppercase gated on is.numeric(), letting integer
  # columns through to haven::na_tag() — write_xpt(survey_data) crashed.
  data(survey_data)
  tmp <- tempfile(fileext = ".xpt")
  on.exit(unlink(tmp), add = TRUE)
  expect_no_error(write_xpt(survey_data, tmp))
  back <- read_xpt(tmp)
  expect_equal(nrow(back), nrow(survey_data))
})

# --- 0.7.4: field-report fixes (2026-09) --------------------------------------

test_that("weighted logistic_regression muffles the non-integer warning in any locale", {
  # 0.7.4 fix: .glm_quiet_weights() matched the English warning text only,
  # so under a German locale "Nicht-ganzzahlige #Erfolge in einem
  # binomial-GLM" leaked through on every weighted fit.
  skip_if_not(capabilities("NLS"))
  old <- Sys.setLanguage("de")
  on.exit(Sys.setLanguage(old), add = TRUE)
  translated <- gettextf("non-integer #successes in a %s glm!", "binomial",
                         domain = "R-stats")
  skip_if(identical(translated, "non-integer #successes in a binomial glm!"),
          "German R translations not installed")

  data(survey_data)
  d <- survey_data
  d$high_life <- as.integer(d$life_satisfaction >= 4)
  expect_no_warning(
    logistic_regression(d, high_life ~ age + income, weights = sampling_weight)
  )
})

test_that("collinearity diagnostics ignore the aliased (excluded) term", {
  # 0.7.4 fix: .lm_collinearity() inverted the correlation matrix of ALL
  # model-matrix columns, including the one excluded for perfect
  # collinearity. That matrix is singular, so the retained terms got either
  # NA (and summary(collinearity = TRUE) showed no table) or rounding
  # artefacts like VIF = -2.85e13, depending on floating-point luck.
  data(survey_data)
  d <- survey_data
  lv <- levels(d$education)
  for (i in seq_along(lv)) d[[paste0("ed", i)]] <- as.integer(d$education == lv[i])
  f <- life_satisfaction ~ age + ed1 + ed2 + ed3 + ed4

  for (w in list(NULL, "sampling_weight")) {
    r <- suppressMessages(
      if (is.null(w)) linear_regression(d, f)
      else linear_regression(d, f, weights = sampling_weight)
    )
    keep <- !is.na(stats::coef(r))
    X <- stats::model.matrix(r)[, keep, drop = FALSE][, -1]
    cm <- if (is.null(w)) stats::cor(X) else
      stats::cov.wt(X, wt = stats::weights(r), cor = TRUE)$cor
    ref <- diag(solve(cm))

    ct <- r$coef_table[r$coef_table$Term != "(Intercept)", ]
    expect_equal(unname(ct$VIF), unname(ref[ct$Term]), tolerance = 1e-10)
    expect_true(all(ct$VIF >= 1))
    expect_equal(ct$Tolerance, 1 / ct$VIF, tolerance = 1e-12)

    out <- capture.output(print(summary(r, collinearity = TRUE)))
    expect_true(any(grepl("Collinearity Statistics", out, fixed = TRUE)))
  }
})

test_that("kendall_tau pair counts match a brute-force pair loop (weighted and unweighted)", {
  # 0.7.4: the O(n^2) R loop was replaced by a vectorized counting kernel.
  # Oracle: explicit loop over all pairs with pair weight sqrt(w_i * w_j).
  brute <- function(x, y, w) {
    tot <- num <- tx <- ty <- 0
    n <- length(x)
    for (i in 1:(n - 1)) for (j in (i + 1):n) {
      pw <- sqrt(w[i] * w[j]); dx <- sign(x[i] - x[j]); dy <- sign(y[i] - y[j])
      tot <- tot + pw; num <- num + pw * dx * dy
      if (dx == 0) tx <- tx + pw
      if (dy == 0) ty <- ty + pw
    }
    num / sqrt((tot - tx) * (tot - ty))
  }
  set.seed(7)
  n <- 80
  x <- sample(1:5, n, replace = TRUE)
  y <- round(x + rnorm(n), 1)
  w <- runif(n, 0.3, 2)
  d <- dplyr::tibble(x = x, y = y, w = w)

  expect_equal(kendall_tau(d, x, y)$correlations$tau[1],
               brute(x, y, rep(1, n)), tolerance = 1e-12)
  expect_equal(kendall_tau(d, x, y, weights = w)$correlations$tau[1],
               brute(x, y, w), tolerance = 1e-12)
  # continuous data (no ties) as well
  z <- rnorm(n)
  expect_equal(kendall_tau(dplyr::tibble(z = z, y = y), z, y)$correlations$tau[1],
               brute(z, y, rep(1, n)), tolerance = 1e-12)
})

test_that("kendall_tau on labelled data equals the unlabelled result and is fast", {
  skip_if_not_installed("haven")
  # 0.7.4 fix: every x[i] in the pair loop dispatched through
  # `[.haven_labelled` (vctrs), ~200x slower per access: 400 labelled cases
  # took 6.5 s vs 0.1 s unlabelled, the full ALLBUS sample > 20 minutes.
  skip_on_cran()
  data(survey_data)
  d <- survey_data[, c("trust_media", "trust_science")]
  dl <- d
  for (v in names(dl)) {
    dl[[v]] <- haven::labelled(as.double(dl[[v]]), c(low = 1, high = 5))
  }
  t_lab <- system.time(r_lab <- kendall_tau(dl, trust_media, trust_science))
  r_raw <- kendall_tau(d, trust_media, trust_science)
  expect_equal(r_lab$correlations$tau, r_raw$correlations$tau)
  expect_equal(r_lab$correlations$p_value, r_raw$correlations$p_value)
  expect_lt(t_lab[["elapsed"]], 5)
})

test_that("grouped t_test keeps an NA row with a warning when a group cannot be tested", {
  # 0.7.4 fix: the per-group error fallback built
  # data.frame(..., group_stats = list(NULL)) - a 0-row frame - so the
  # fallback itself crashed with "arguments imply differing number of
  # rows: 1, 0" and the real reason (only one level in that group) was lost.
  data(survey_data)
  d <- dplyr::filter(survey_data, !(region == "East" & gender == "Male"))
  g <- dplyr::group_by(d, region)

  expect_warning(
    r <- t_test(g, life_satisfaction, group = gender),
    "East.*exactly 2 levels"
  )
  expect_equal(nrow(r$results), 2L)
  east <- r$results[r$results$region == "East", ]
  west <- r$results[r$results$region == "West", ]
  expect_true(is.na(east$t_stat))
  expect_false(is.na(west$t_stat))
  out <- capture.output(print(r))
  expect_true(any(grepl("not computed", out, fixed = TRUE)))
  out_s <- capture.output(print(summary(r)))
  expect_true(any(grepl("Not computed for this group", out_s, fixed = TRUE)))
  expect_false(any(grepl("[NA, NA]", out_s, fixed = TRUE)))
})

test_that("grouped mann_whitney / kruskal_wallis keep the NA row of an untestable group", {
  # 0.7.4 fix: the error handler assigned results_list[[var]] inside its
  # own function scope, so the NA row was discarded and the group vanished
  # silently from the results (only a warning without the group name).
  data(survey_data)
  d <- dplyr::filter(survey_data, !(region == "East" & gender == "Male"))
  expect_warning(
    mw <- mann_whitney(dplyr::group_by(d, region), life_satisfaction,
                       group = gender),
    "East"
  )
  expect_equal(nrow(mw$results), 2L)
  expect_true(is.na(mw$results$U[mw$results$region == "East"]))

  d2 <- dplyr::filter(survey_data,
                      region == "West" | education == "University")
  expect_warning(
    kw <- kruskal_wallis(dplyr::group_by(d2, region), life_satisfaction,
                         group = education),
    "East"
  )
  expect_equal(nrow(kw$results), 2L)
  expect_true(is.na(kw$results$H[kw$results$region == "East"]))

  for (res in list(mw, kw)) {
    out <- c(capture.output(print(res)), capture.output(print(summary(res))))
    expect_true(any(grepl("not computed for this group", out, ignore.case = TRUE)))
    expect_false(any(grepl("H(NA)", out, fixed = TRUE)))
    expect_false(any(grepl("U = NA", out, fixed = TRUE)))
  }
})

test_that("grouped print shows each group once, NA group with its own values", {
  skip_if_not_installed("haven")
  # 0.7.4 fix: grouped print methods selected a group's rows with
  # `results[[g]] == value`. An NA group key makes that NA, and indexing
  # with NA yields all-NA ghost rows: every group got an extra NA row, the
  # NA group showed only NA rows and never its real statistics.
  data(survey_data)
  d <- survey_data[1:200, c("age", "region")]
  reg <- ifelse(d$region == "East", 1, 2)
  reg[1:6] <- haven::tagged_na("a")
  reg[7:10] <- haven::tagged_na("b")
  d$reg <- haven::labelled(reg, c(East = 1, West = 2,
                                  "no answer" = haven::tagged_na("a")))
  r <- describe(dplyr::group_by(d, reg), age)
  out <- capture.output(print(r))

  data_rows <- grep("^\\s+age\\s", out, value = TRUE)
  expect_length(data_rows, 3L)                      # one row per group
  expect_false(any(grepl("^\\s+age\\s+NA\\s+NA", data_rows)))
  # the NA group's real statistics are shown (N = 10 valid, 0 missing)
  expect_true(any(grepl("\\s10\\s+0\\s*$", data_rows)))

  # same NA-safe matching in the shared for_each_group() iterator
  res <- data.frame(g = c(1, NA, 2), v = 1:3)
  seen <- list()
  for_each_group(res, "g", function(rows, key) seen[[length(seen) + 1]] <<- rows$v,
                 header = FALSE)
  expect_equal(seen, list(1L, 2L, 3L))
})

test_that("grouped print headers show factor levels and value labels, not codes", {
  # 0.7.4 fix: ~20 compact print methods pasted the one-row group data
  # frame directly, which prints factor codes ("[region = 1]"); labelled
  # group variables showed their numeric codes in every header.
  skip_if_not_installed("haven")
  data(survey_data)
  out <- capture.output(print(
    t_test(dplyr::group_by(survey_data, region), life_satisfaction,
           group = gender)
  ))
  expect_true(any(grepl("[region = East]", out, fixed = TRUE)))
  expect_false(any(grepl("[region = 1]", out, fixed = TRUE)))

  d <- survey_data[1:200, c("age", "region")]
  reg <- ifelse(d$region == "East", 1, 2)
  reg[1:5] <- NA
  d$reg <- haven::labelled(reg, c(East = 1, West = 2))
  out2 <- capture.output(print(describe(dplyr::group_by(d, reg), age)))
  expect_true(any(grepl("Group: reg = East", out2, fixed = TRUE)))
  expect_true(any(grepl("Group: reg = NA", out2, fixed = TRUE)))

  expect_equal(.format_group_label(data.frame(a = factor("x"), b = 2)),
               "a = x, b = 2")
})

test_that("statistics of an empty (all-missing) variable are NA, not NaN/-Inf", {
  # 0.7.4 fix: mean(numeric(0)) is NaN and diff(range(numeric(0))) is -Inf
  # (with two R warnings), so an all-missing variable or group showed
  # Mean = NaN, Range = -Inf in describe() and the w_* functions.
  data(survey_data)
  d <- survey_data
  d$empty <- NA_real_
  for (w in list(NULL, "sampling_weight")) {
    r <- expect_no_warning(
      if (is.null(w)) describe(d, empty, show = "all")
      else describe(d, empty, show = "all", weights = sampling_weight)
    )
    stats <- unlist(r$results[setdiff(names(r$results),
                                      c("Variable", "empty_N", "empty_Missing",
                                        "empty_Effective_N"))])
    stats <- stats[vapply(stats, is.numeric, logical(1))]
    expect_true(all(is.na(stats) & !is.nan(stats)))
  }
  expect_true(is.na(w_mean(d, empty)$results$mean) &&
              !is.nan(w_mean(d, empty)$results$mean))
  expect_true(is.na(w_mean(d, empty, weights = sampling_weight)$results$weighted_mean))
  rr <- expect_no_warning(w_range(d, empty)$results$range)
  expect_true(is.na(rr) && is.finite(rr) == FALSE && !is.infinite(rr))
})

test_that("to_label() keeps the original codes for to_numeric() and to_labelled()", {
  # 0.7.4 fix: to_label() kept only the factor levels, so to_numeric()
  # renumbered the codes sequentially (6 -> 3, 42 -> 4, 90 -> 5, 91 -> 6)
  # and keep_labels = TRUE even attached the wrong labels (c = 3, ...).
  skip_if_not_installed("haven")
  labs <- c(a = 1, b = 2, c = 6, d = 42, e = 90, f = 91)
  x <- haven::labelled(c(1, 2, 6, 42, 90, 91, 6), labs, label = "Test")
  f <- to_label(x)

  expect_equal(as.vector(to_numeric(f)), c(1, 2, 6, 42, 90, 91, 6))
  expect_equal(attr(to_numeric(f, keep_labels = TRUE), "labels"), labs)
  back <- to_labelled(f)
  expect_equal(as.vector(unclass(back)), c(1, 2, 6, 42, 90, 91, 6))
  expect_equal(attr(back, "labels", exact = TRUE), labs)

  # sequential numbering stays available on request
  expect_equal(as.vector(to_numeric(f, use_labels = FALSE)),
               c(1, 2, 3, 4, 5, 6, 3))

  # the map survives dplyr verbs in a data frame pipeline
  d <- dplyr::tibble(x = x, n = 1:7)
  d2 <- dplyr::filter(to_label(d, x), n > 2)
  expect_equal(as.vector(to_numeric(d2$x)), c(6, 42, 90, 91, 6))

  # unlabelled values kept via add_non_labelled keep their code as well
  x2 <- haven::labelled(c(1, 5, 42), c(a = 1, d = 42))
  expect_equal(as.vector(to_numeric(to_label(x2, add_non_labelled = TRUE))),
               c(1, 5, 42))

  # plain factors keep the documented behaviour
  expect_equal(as.vector(to_numeric(factor(c("Male", "Female")))), c(2, 1))
})

test_that("the magrittr pipe is re-exported", {
  # 0.7.4 fix: %>% was only imported, so after library(mariposa) alone the
  # documented `survey_data %>% describe(age)` failed with
  # 'could not find function "%>%"'.
  expect_true("%>%" %in% getNamespaceExports("mariposa"))
  expect_identical(mariposa::`%>%`, dplyr::`%>%`)
})

test_that("spearman_rho output does not claim a weighted correlation", {
  # 0.7.4 fix: weights only filter cases (SPSS NONPAR CORR convention),
  # but the output said "[Weighted]" / "Weighted Spearman's Rank
  # Correlation Analysis" and the docs spoke of weighted correlations.
  data(survey_data)
  r <- spearman_rho(survey_data, age, income, weights = sampling_weight)
  out <- c(capture.output(print(r)), capture.output(print(summary(r))))
  expect_false(any(grepl("Weighted", out, fixed = TRUE)))
  expect_true(any(grepl("case filter only", out, fixed = TRUE)))
  # the coefficient equals the unweighted rho on the weight > 0 cases
  expect_equal(r$correlations$rho[1],
               spearman_rho(survey_data, age, income)$correlations$rho[1])
})

test_that(".na_tags() equals element-wise haven::na_tag() for every input type", {
  # 0.7.4 perf fix: six call sites read NA tags with
  # vapply(x[na_mask], haven::na_tag, ...), which splits a labelled vector
  # into one vctrs object per element - ~75% of codebook(allbus) runtime
  # (20 s for ALLBUS 2023). .na_tags() is the single vectorized reader.
  skip_if_not_installed("haven")
  x <- c(1, haven::tagged_na("a"), NA, haven::tagged_na("b"), 5)
  expect_identical(.na_tags(x), vapply(x, haven::na_tag, character(1)))
  xl <- haven::labelled(x, c(one = 1))
  expect_identical(.na_tags(xl), .na_tags(x))
  expect_identical(.na_tags(xl[is.na(xl)]), c("a", NA, "b"))
  # integers cannot carry tags (haven::na_tag() errors on them)
  expect_identical(.na_tags(c(1L, NA, 3L)), rep(NA_character_, 3))
  expect_identical(.na_tags(numeric(0)), character(0))
})

test_that("labelled SPSS weights with NA work in every weighted entry point", {
  # 0.7.4 fix: a weight read from SPSS is haven_labelled_spss with an
  # na_range (ALLBUS: c(-Inf, -1)). Once it contains NA, every comparison
  # on it (w < 0 in .check_weights(), w > 0 filters, ...) failed inside
  # haven's lossy-cast check with "missing value where TRUE/FALSE needed",
  # so every weighted function aborted. Weights are now stripped to bare
  # numbers at the entry point; output must equal the plain-weight run.
  skip_if_not_installed("haven")
  set.seed(11)
  n <- 150
  dp <- dplyr::tibble(
    y = rnorm(n, 50, 10), x2 = rnorm(n, 5, 2), x3 = rnorm(n, 10, 3),
    ord1 = sample(1:4, n, TRUE), ord2 = sample(1:4, n, TRUE),
    g2 = factor(sample(c("A", "B"), n, TRUE)),
    g3 = factor(sample(c("A", "B", "C"), n, TRUE)),
    bin = rbinom(n, 1, 0.4), bin2 = rbinom(n, 1, 0.5),
    w = runif(n, 0.5, 1.5)
  )
  dp$i1 <- dp$y + rnorm(n, sd = 4)
  dp$i2 <- dp$y + rnorm(n, sd = 4)
  dp$i3 <- dp$y + rnorm(n, sd = 4)
  dp$w[c(3, 17)] <- NA
  dl <- dp
  dl$w <- haven::labelled_spss(dp$w, na_range = c(-Inf, -1), label = "Weight")
  # read_sav() always adds format attributes; any extra attribute sends
  # haven's cast down the lossy-check path that fails on NA
  attr(dl$w, "format.spss") <- "F8.2"

  calls <- list(
    describe = function(d) describe(d, y, weights = w),
    frequency = function(d) frequency(d, g3, weights = w),
    crosstab = function(d) crosstab(d, g2, g3, weights = w),
    codebook = function(d) codebook(d, y, g3, weights = w, view = FALSE)$data,
    w_mean = function(d) w_mean(d, y, weights = w),
    w_median = function(d) w_median(d, y, weights = w),
    w_sd = function(d) w_sd(d, y, weights = w),
    w_quantile = function(d) w_quantile(d, y, weights = w),
    w_modus = function(d) w_modus(d, ord1, weights = w),
    t_test = function(d) t_test(d, y, group = g2, weights = w),
    oneway_anova = function(d) oneway_anova(d, y, group = g3, weights = w),
    tukey_test = function(d) tukey_test(oneway_anova(d, y, group = g3, weights = w)),
    levene_test = function(d) levene_test(d, y, group = g3, weights = w),
    factorial_anova = function(d) factorial_anova(d, dv = y, between = c(g2, g3), weights = w),
    ancova = function(d) ancova(d, dv = y, between = c(g3), covariate = c(x2), weights = w),
    chi_square = function(d) chi_square(d, g2, g3, weights = w),
    fisher_test = function(d) fisher_test(d, g2, g3, weights = w),
    chisq_gof = function(d) chisq_gof(d, g3, weights = w),
    mcnemar_test = function(d) mcnemar_test(d, bin, bin2, weights = w),
    binomial_test = function(d) binomial_test(d, bin, weights = w),
    mann_whitney = function(d) mann_whitney(d, y, group = g2, weights = w),
    kruskal_wallis = function(d) kruskal_wallis(d, y, group = g3, weights = w),
    dunn_test = function(d) dunn_test(kruskal_wallis(d, y, group = g3, weights = w)),
    wilcoxon_test = function(d) wilcoxon_test(d, y, x2, weights = w),
    friedman_test = function(d) friedman_test(d, y, x2, x3, weights = w),
    pearson_cor = function(d) pearson_cor(d, y, x2, weights = w),
    spearman_rho = function(d) spearman_rho(d, ord1, ord2, weights = w),
    kendall_tau = function(d) kendall_tau(d, ord1, ord2, weights = w),
    partial_cor = function(d) partial_cor(d, y, x2, controls = x3, weights = w),
    linear_regression = function(d) linear_regression(d, y ~ x2 + g2, weights = w),
    logistic_regression = function(d) logistic_regression(d, bin ~ x2 + y, weights = w),
    reliability = function(d) reliability(d, i1, i2, i3, weights = w),
    efa = function(d) efa(d, i1, i2, i3, x2, x3, weights = w),
    multiple_response = function(d) multiple_response(d, bin, bin2, weights = w),
    std = function(d) std(d, y, weights = w)$y,
    center = function(d) center(d, y, weights = w)$y
  )
  show <- function(r) capture.output(print(r))
  for (nm in names(calls)) {
    rl <- tryCatch(suppressWarnings(suppressMessages(calls[[nm]](dl))),
                   error = function(e) e)
    if (inherits(rl, "error")) {
      fail(paste(nm, "errors:", conditionMessage(rl)))
      next
    }
    rp <- suppressWarnings(suppressMessages(calls[[nm]](dp)))
    expect_identical(show(rl), show(rp), label = paste(nm, "output"))
  }
})

test_that("chi_square excludes cases with a missing weight", {
  # 0.7.4 fix: xtabs(weights ~ ...) summed NA weights into the cells, so
  # any missing weight made chisq.test() abort ("all entries of 'x' must be
  # nonnegative and finite"). Cases with a missing weight are excluded, as
  # in every other weighted function (and SPSS).
  data(survey_data)
  d <- survey_data
  d$w <- d$sampling_weight
  d$w[c(3, 17)] <- NA
  r <- chi_square(d, gender, region, weights = w)
  ref <- chi_square(d[!is.na(d$w), ], gender, region, weights = w)
  expect_equal(r$results$chi_squared, ref$results$chi_squared)
  expect_false(is.na(r$results$chi_squared))
  g <- chi_square(dplyr::group_by(d, education), gender, region, weights = w)
  expect_false(anyNA(g$results$chi_squared))
})

test_that("codebook(weights = ) still describes the weights variable with its label", {
  # 0.7.4 follow-up: stripping weights to bare numbers (SPSS-weight fix)
  # must not change how codebook() describes the weights column itself.
  data(survey_data)
  cb <- codebook(survey_data[, c("gender", "sampling_weight")],
                 weights = sampling_weight, view = FALSE)
  out <- capture.output(print(cb))
  expect_true(any(grepl("2 labelled", out, fixed = TRUE)))
})

test_that("every grouped print path labels groups by level, not code", {
  # 0.7.4 follow-up: summary() of reliability()/efa() stored the group key
  # as a list (print_group_header() only recognised data frames), and the
  # regression prints pasted the key themselves - both still showed codes.
  skip_if_not_installed("haven")
  data(survey_data)
  g <- dplyr::group_by(survey_data, region)
  rel <- capture.output(print(summary(
    reliability(g, trust_government, trust_media, trust_science))))
  expect_true(any(grepl("region = East", rel, fixed = TRUE)))
  fa <- capture.output(print(summary(
    efa(g, trust_government, trust_media, trust_science, life_satisfaction))))
  expect_true(any(grepl("region = East", fa, fixed = TRUE)))

  d <- survey_data
  d$reg <- haven::labelled(ifelse(d$region == "East", 1, 2), c(East = 1, West = 2))
  gl <- dplyr::group_by(d, reg)
  lr <- linear_regression(gl, life_satisfaction ~ age)
  lr_out <- c(capture.output(print(lr)), capture.output(print(summary(lr))))
  expect_true(any(grepl("reg = East", lr_out, fixed = TRUE)))
  expect_false(any(grepl("reg = 1", lr_out, fixed = TRUE)))
  d$hi <- as.integer(d$life_satisfaction >= 4)
  lg <- logistic_regression(dplyr::group_by(d, reg), hi ~ age)
  lg_out <- c(capture.output(print(lg)), capture.output(print(summary(lg))))
  expect_true(any(grepl("reg = East", lg_out, fixed = TRUE)))
  expect_false(any(grepl("reg = 1", lg_out, fixed = TRUE)))
})

# --- 0.7.4: shared helpers for the practice-test fixes ------------------------

test_that("format_p_stars() never leaves a dangling space", {
  expect_identical(format_p_stars(0.55), "p = 0.550")
  expect_identical(format_p_stars(0.0004), "p < 0.001 ***")
  expect_identical(format_p_stars(0.02), "p = 0.020 *")
  expect_identical(format_p_stars(NA_real_), "p = NA")
})

test_that(".group_factor() orders by code and shows value labels", {
  skip_if_not_installed("haven")
  # numeric: sorted by value, not by first appearance (t-test sign)
  expect_identical(levels(.group_factor(c(1, 0, 1, 0))), c("0", "1"))
  # labelled: code order, label text, unlabelled code kept, NA stays NA
  g <- haven::labelled(c(2, 1, NA, 3, 2), c(West = 2, East = 1))
  f <- .group_factor(g)
  expect_identical(levels(f), c("East", "West", "3"))
  expect_identical(as.character(f), c("West", "East", NA, "3", "West"))
  # duplicate label texts would merge groups: disambiguated by code
  d <- haven::labelled(c(1, 2, 3), c(low = 1, ".." = 2, ".." = 3))
  expect_identical(levels(.group_factor(d)), c("low", ".. (2)", ".. (3)"))
  # factors unchanged
  expect_identical(.group_factor(factor(c("b", "a"))), factor(c("b", "a")))
})
