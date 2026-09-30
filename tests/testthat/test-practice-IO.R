# Regression tests for the 2026-09 practice-test findings, batch IO
# (import/export, labels, recoding, row operations, pomps, standardize).
# Each block names the finding ID and what was wrong.

# IO-01: read_sas() passed catalog_encoding = NULL explicitly, which haven
# 2.5.x rejects ("Expected string vector of length 1") -> every .sas7bdat
# failed with the default arguments.
test_that("IO-01: read_sas() reads a .sas7bdat file with default arguments", {
  skip_if_not_installed("haven")
  f <- file.path(system.file("examples", package = "haven"), "iris.sas7bdat")
  skip_if_not(file.exists(f))
  d <- read_sas(f)
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 150L)
})

# IO-16: untag_na(), na_frequencies() and write_xpt()'s re-tagging read the
# NA tag element by element (vapply(x[i], haven::na_tag)) - every element
# went through vctrs dispatch: untag_na() over ALLBUS took 9 s, write_spss()
# 12 s, write_xlsx() 27 s. The tag reads are vectorized now.
test_that("IO-16: vectorized untag_na() recovers codes, keeps system NA", {
  skip_if_not_installed("haven")
  x <- set_na(c(1, -9, 2, NA, -8, -9), -9, -8)
  expect_equal(as.vector(unclass(untag_na(x))), c(1, -9, 2, NA, -8, -9))
  # an unknown tag (not in the map) stays NA
  y <- x
  attr(y, "na_tag_map") <- attr(y, "na_tag_map")[1]
  expect_true(is.na(untag_na(y)[5]))
  nf <- na_frequencies(x)
  expect_equal(sum(nf$n), 4L)
})

test_that("IO-16: untag_na() on a large labelled vector is fast", {
  skip_on_cran()
  skip_if_not_installed("haven")
  set.seed(1)
  big <- set_na(sample(c(1:5, -9, -8), 2e5, replace = TRUE), -9, -8)
  elapsed <- system.time(u <- untag_na(big))[["elapsed"]]
  u <- as.vector(unclass(u))
  expect_equal(sum(u == -9 | u == -8), sum(is.na(big)))
  expect_lt(elapsed, 1)
})

test_that("IO-16: write_xpt() re-tags lowercase tags to uppercase", {
  skip_if_not_installed("haven")
  x <- set_na(c(1, -9, 2, -8), -9, -8)
  r <- .retag_uppercase(x)
  expect_equal(.na_tags(r), c(NA, "A", NA, "B"))
  expect_equal(attr(r, "na_tag_map"), attr(x, "na_tag_map"))
})

# Helper for the IO-02/IO-03 blocks: an imported-style item (tagged NAs
# with an SPSS code map and labelled missing types)
io_item <- function() {
  x <- haven::labelled(c(1, 2, 3, -9, 5, -8, 4, NA),
                       labels = c("low" = 1, "high" = 5,
                                  "no answer" = -9, "don't know" = -8),
                       label = "Trust item")
  set_na(x, -9, -8)
}

# IO-02: rec("rev") (and other transformations) kept the tagged-NA payloads
# but dropped na_tag_map/class -> write_spss() crashed ("Failed to insert
# value ... character tags for missing values"), na_frequencies() showed
# code NA.
test_that("IO-02: rec() keeps class, na_tag_map and missing labels", {
  skip_if_not_installed("haven")
  x <- io_item()
  for (rules in c("rev", "1:2=1; 3=2; 4:5=3", "1:3=copy; else=copy",
                  "dicho", "quart", "mean")) {
    r <- rec(x, rules = rules)
    expect_s3_class(r, "haven_labelled")
    expect_equal(attr(r, "na_tag_map"), attr(x, "na_tag_map"))
    nf <- na_frequencies(r)
    expect_setequal(nf$code[!is.na(nf$tag)], c("-9", "-8"))
    lab <- attr(r, "labels")
    expect_setequal(names(lab)[is.na(lab)], c("no answer", "don't know"))
  }
})

test_that("IO-02: write_spss() round trip after rec()/std()/center()/pomps()", {
  skip_if_not_installed("haven")
  d <- tibble::tibble(x = io_item())
  d <- rec(d, x, rules = "rev", suffix = "_r")
  d <- std(d, x, suffix = "_z")
  d <- center(d, x, suffix = "_c")
  d$x_p <- pomps(d$x, 1, 5)
  d$x_n <- to_numeric(d$x)
  tf <- tempfile(fileext = ".sav")
  on.exit(unlink(tf))
  expect_no_error(suppressMessages(write_spss(d, tf)))
  back <- read_spss(tf)
  # reversed item: missing codes restored as SPSS user-missing values
  expect_equal(as.vector(unclass(untag_na(back$x_r))),
               c(5, 4, 3, -9, 1, -8, 2, NA))
  # derived metric variables: clean system-missing values (SPSS COMPUTE)
  expect_true(all(is.na(back$x_z[c(4, 6, 8)])))
  expect_null(attr(d$x_z, "na_tag_map"))
  expect_true(all(is.na(.na_tags(d$x_z))))
  expect_true(all(is.na(.na_tags(d$x_p))))
  expect_true(all(is.na(.na_tags(d$x_n))))
})

test_that("IO-02: write_spss() strips tags that have no code map, naming the variable", {
  skip_if_not_installed("haven")
  d <- tibble::tibble(y = io_item())
  d$y2 <- d$y + 1            # arithmetic keeps the tag payload, drops the map
  tf <- tempfile(fileext = ".sav")
  on.exit(unlink(tf))
  expect_warning(suppressMessages(write_spss(d, tf)), "y2")
  back <- read_spss(tf)
  expect_equal(sum(is.na(back$y2)), 3L)
})

# IO-03: rec() inline labels, strip_tags() and to_numeric(keep_labels = TRUE)
# returned a plain numeric with a bare "labels" attribute (no haven_labelled
# class) -> write_spss()/write_stata() dropped the labels; to_labelled()
# ignored an existing labels attribute.
test_that("IO-03: rec() inline labels survive the SPSS export", {
  skip_if_not_installed("haven")
  sd <- rec(survey_data, trust_government,
            rules = "1:2=1 [low]; 3=2 [mid]; 4:5=3 [high]", suffix = "_3")
  expect_s3_class(sd$trust_government_3, "haven_labelled")
  tf <- tempfile(fileext = ".sav")
  on.exit(unlink(tf))
  suppressMessages(write_spss(sd[, c("id", "trust_government_3")], tf))
  back <- read_spss(tf)
  expect_equal(unname(attr(back$trust_government_3, "labels")), c(1, 2, 3))
  expect_equal(names(attr(back$trust_government_3, "labels")),
               c("low", "mid", "high"))
})

test_that("IO-03: strip_tags() and to_numeric(keep_labels) return haven_labelled", {
  skip_if_not_installed("haven")
  x <- io_item()
  s <- strip_tags(x)
  expect_s3_class(s, "haven_labelled")
  expect_equal(unname(attr(s, "labels")), c(1, 5))
  expect_true(all(is.na(.na_tags(s))))
  f <- factor(c("a", "b", "a"))
  n <- to_numeric(f, keep_labels = TRUE)
  expect_s3_class(n, "haven_labelled")
  expect_equal(attr(n, "labels"), c(a = 1, b = 2))
  # an existing bare labels attribute is picked up by to_labelled()
  bare <- c(1, 2, 1)
  attr(bare, "labels") <- c(yes = 1, no = 2)
  expect_equal(attr(to_labelled(bare), "labels"), c(yes = 1, no = 2))
})

test_that("IO-03: exporters keep a bare labels attribute (safety net)", {
  skip_if_not_installed("haven")
  bare <- c(1, 2, 1)
  attr(bare, "labels") <- c(yes = 1, no = 2)
  d <- data.frame(v = 1:3)
  d$b <- bare
  tf <- tempfile(fileext = ".sav")
  on.exit(unlink(tf))
  suppressMessages(write_spss(d, tf))
  expect_equal(attr(read_spss(tf)$b, "labels"), c(yes = 1, no = 2))
  tf2 <- tempfile(fileext = ".dta")
  on.exit(unlink(tf2), add = TRUE)
  suppressMessages(write_stata(d, tf2))
  expect_equal(unname(attr(read_stata(tf2)$b, "labels")), c(1, 2))
})

# IO-19: rec(rules = "rev", as_factor = TRUE) ignored the mirrored labels
# (levels "1".."7"); explicit val_labels turned unlabelled values into NA.
test_that("IO-19: rec(as_factor = TRUE) uses the result's value labels", {
  skip_if_not_installed("haven")
  x <- haven::labelled(c(1, 2, 3, 5, 4),
                       labels = c("low" = 1, "mid" = 3, "high" = 5))
  f <- rec(x, rules = "rev", as_factor = TRUE)
  expect_equal(levels(f), c("high", "2", "mid", "4", "low"))
  expect_equal(as.character(f), c("low", "4", "mid", "high", "2"))
  # duplicate label texts stay distinct (ALLBUS ".." scale points)
  y <- haven::labelled(c(1, 2, 3, 4), labels = c("none" = 1, ".." = 2,
                                                 ".." = 3, "all" = 4))
  g <- rec(y, rules = "rev", as_factor = TRUE)
  expect_equal(nlevels(g), 4L)
  expect_equal(to_numeric(g), c(4, 3, 2, 1))
  # explicit val_labels no longer drop unlabelled values
  h <- rec(c(1, 2, 3), rules = "1=1; 2=2; 3=3", as_factor = TRUE,
           val_labels = c("1" = "one"))
  expect_false(anyNA(h))
})

# SCALE-08 (= IO-06): rec(rules = "rev") reversed around the OBSERVED
# min/max: c(2, 3, 4, 5) on a 1-5 scale gave 5 4 3 2, labels were mirrored
# to codes that do not exist.
test_that("SCALE-08: rev uses the scale range of the value labels", {
  skip_if_not_installed("haven")
  x <- haven::labelled(c(2, 3, 4, 5, NA),
                       labels = c("low" = 1, "high" = 5))
  r <- rec(x, rules = "rev")
  expect_equal(as.numeric(r), c(4, 3, 2, 1, NA))
  expect_equal(attr(r, "labels"), c(low = 5, high = 1))
  # imported item: missing labels (tagged) do not widen the range
  y <- set_na(haven::labelled(c(2, 3, 7, -9),
                              labels = c("none" = 1, "all" = 7,
                                         "no answer" = -9)), -9)
  ry <- rec(y, rules = "rev")
  expect_equal(as.numeric(ry)[1:3], c(6, 5, 1))
})

test_that("SCALE-08: explicit rev(lo, hi) and message for the observed range", {
  expect_message(r <- rec(c(2, 3, 4, 5, NA), rules = "rev"),
                 "observed range")
  expect_equal(as.numeric(r), c(5, 4, 3, 2, NA))
  expect_no_message(r2 <- rec(c(2, 3, 4, 5, NA), rules = "rev(1, 5)"))
  expect_equal(as.numeric(r2), c(4, 3, 2, 1, NA))
  expect_warning(rec(c(1, 9), rules = "rev(1, 5)"), "outside")
  expect_error(rec(c(1, 2), rules = "rev(5, 1)"), "rev")
  d <- rec(data.frame(q = c(2, 3, 4)), q, rules = "rev(1, 5)")
  expect_equal(d$q, c(4, 3, 2))
})

# SCALE-09: rec() on haven_labelled_spss items (haven::read_sav(user_na =
# TRUE)) dropped the class, the missing codes and their labels, and
# reversed around the missing codes; val_labels = was ignored with "rev".
test_that("SCALE-09: rec() on haven_labelled_spss keeps the missing codes", {
  skip_if_not_installed("haven")
  x <- haven::labelled_spss(c(1, 2, 3, -9, 5),
                            labels = c("low" = 1, "high" = 5,
                                       "no answer" = -9),
                            na_values = -9, label = "Item")
  r <- rec(x, rules = "rev")
  expect_equal(as.numeric(r)[c(1, 2, 3, 5)], c(5, 4, 3, 1))
  expect_true(is.na(r[4]))
  nf <- na_frequencies(r)
  expect_equal(nf$code[!is.na(nf$tag)], "-9")
  tf <- tempfile(fileext = ".sav")
  on.exit(unlink(tf))
  suppressMessages(write_spss(data.frame(r = r), tf))
  back <- haven::read_sav(tf, user_na = TRUE)
  expect_equal(attr(back$r, "na_values"), -9)
  expect_equal(as.numeric(unclass(back$r)), c(5, 4, 3, -9, 1))
})

test_that("SCALE-09: val_labels are honoured with rules = 'rev'", {
  skip_if_not_installed("haven")
  x <- haven::labelled(c(1, 2, 5), labels = c("a" = 1, "e" = 5))
  r <- rec(x, rules = "rev", val_labels = c("1" = "viel", "5" = "wenig"))
  expect_equal(attr(r, "labels"), c(viel = 1, wenig = 5))
})
