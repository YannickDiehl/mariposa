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
    expect_setequal(nf$code[!is.na(nf$tag)], c(-9, -8))
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
  expect_equal(nf$code[!is.na(nf$tag)], -9)
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

# IO-18: rec() syntax problems: unmatched values silently NA, "1,2=1" list
# syntax unsupported, "REV" rejected, "5:1=1" silently all NA, labels with
# ";" broke the parser, "dicho(x)" leaked a German base warning, missing
# rules gave a German base error, " (recoded)" stacked on every call.
test_that("IO-18: unmatched valid values warn with the fix", {
  expect_warning(r <- rec(c(1, 2, 3, 4, 5), rules = "1:3=1"),
                 "else=copy")
  expect_equal(r, c(1, 1, 1, NA, NA))
  expect_no_warning(rec(c(1, 2, 3, 4, 5), rules = "1:3=1; else=NA"))
  expect_no_warning(rec(c(1, 2, 3, 4, 5), rules = "1:3=1; else=copy"))
  expect_no_warning(rec(c(1, 2, NA), rules = "1:2=1"))
  expect_warning(rec(data.frame(q = 1:5), q, rules = "1:3=1"), "q")
})

test_that("IO-18: value lists, case-insensitive keywords, reversed ranges", {
  expect_equal(rec(c(1, 2, 3, 4), rules = "1,2=1; 3,4=2"), c(1, 1, 2, 2))
  expect_equal(rec(c(1, 2, 3, 4, 5), rules = "1, 2:3=1; else=0"),
               c(1, 1, 1, 0, 0))
  expect_message(r <- rec(c(1, 2, 3), rules = "REV"), "observed")
  expect_equal(as.numeric(r), c(3, 2, 1))
  expect_equal(rec(c(1, 5), rules = "DICHO(3)"), c(0, 1))
  expect_error(rec(c(1, 5), rules = "5:1=1"), "1:5")
})

test_that("IO-18: labels may contain semicolons; clear errors in English", {
  skip_if_not_installed("haven")
  r <- rec(c(1, 2, 3), rules = "1:2=1 [niedrig; gering]; 3=2 [hoch]")
  expect_equal(names(attr(r, "labels")), c("niedrig; gering", "hoch"))
  expect_error(rec(c(1, 2), rules = "dicho(x)"), "cut-point")
  expect_no_warning(try(rec(c(1, 2), rules = "dicho(x)"), silent = TRUE))
  expect_error(rec(c(1, 2)), "rules")
  expect_error(rec(c(1, 2)), class = "rlang_error")
})

test_that("IO-18: the ' (recoded)' label suffix does not stack", {
  x <- c(1, 2, 3)
  attr(x, "label") <- "Trust"
  r1 <- rec(x, rules = "1:3=copy")
  r2 <- rec(r1, rules = "1:3=copy")
  expect_equal(attr(r2, "label"), "Trust (recoded)")
})

# IO-04: to_label()/to_character() merged distinct codes sharing a label
# text (ALLBUS ".." on scale points 2-6: one level), and the to_numeric()
# round trip turned 3-6 into 2.
test_that("IO-04: duplicate label texts stay distinct levels", {
  skip_if_not_installed("haven")
  x <- haven::labelled(c(1, 2, 3, 4, 5, 6, 7, 2, 6),
                       labels = c("none" = 1, ".." = 2, ".." = 3, ".." = 4,
                                  ".." = 5, ".." = 6, "full" = 7))
  f <- to_label(x)
  expect_equal(nlevels(f), 7L)
  expect_equal(levels(f)[1:3], c("none", ".. (2)", ".. (3)"))
  expect_equal(to_numeric(f), c(1, 2, 3, 4, 5, 6, 7, 2, 6))
  expect_equal(as.numeric(to_labelled(f)), c(1, 2, 3, 4, 5, 6, 7, 2, 6))
  ch <- to_character(x)
  expect_equal(length(unique(ch)), 7L)
  expect_equal(ch[2], ".. (2)")
})

# IO-05: to_label(df) turned metric variables whose only labels are missing
# codes (ALLBUS age, isei08) into all-NA factors and the weight into a
# factor - silently.
io_metric_df <- function() {
  age <- set_na(haven::labelled(c(18, 45, -9, 70),
                                labels = c("no answer" = -9)), -9)
  tibble::tibble(
    sex = haven::labelled(c(1, 2, 1, 2), labels = c(male = 1, female = 2)),
    age = age,
    w = haven::labelled_spss(c(0.8, 1.2, 1, 1), na_range = c(-Inf, -1)),
    kids = haven::labelled(c(0, 2, 3, 0), labels = c("none" = 0))
  )
}

test_that("IO-05: to_label(df) leaves metric variables alone, with a message", {
  skip_if_not_installed("haven")
  d <- io_metric_df()
  expect_message(r <- to_label(d), "age")
  expect_s3_class(r$sex, "factor")
  expect_false(is.factor(r$age))
  expect_false(is.factor(r$w))
  expect_false(is.factor(r$kids))
  expect_identical(r$age, d$age)
  # to_character() follows the same rule
  expect_message(rc <- to_character(d), "kids")
  expect_type(rc$sex, "character")
  expect_false(is.character(rc$kids))
})

test_that("IO-05: explicit selection converts but warns about lost values", {
  skip_if_not_installed("haven")
  d <- io_metric_df()
  expect_warning(r <- to_label(d, kids), "add_non_labelled")
  expect_s3_class(r$kids, "factor")
  expect_no_warning(r2 <- to_label(d, kids, add_non_labelled = TRUE))
  expect_false(anyNA(r2$kids))
  expect_warning(to_label(d$kids), "add_non_labelled")
})

# IO-07: copy_labels() forced the source class onto converted/summarised
# columns: a to_label()'d factor became int+lbl 1,2,3 with labels 1/5/9.
test_that("IO-07: copy_labels() copies value labels only to compatible columns", {
  skip_if_not_installed("haven")
  src <- tibble::tibble(
    x = haven::labelled(c(1, 5, 9, 9), labels = c(a = 1, b = 5, c = 9),
                        label = "Item x"),
    y = haven::labelled(c(1, 2, 1, 2), labels = c(m = 1, f = 2),
                        label = "Item y")
  )
  conv <- dplyr::mutate(src, x = to_label(x))
  r <- copy_labels(conv, src)
  expect_s3_class(r$x, "factor")
  expect_equal(levels(r$x), c("a", "b", "c"))
  expect_equal(attr(r$x, "label"), "Item x")
  # summarised values are no codes: variable label only
  s <- dplyr::summarise(src, x = mean(as.numeric(x)), y = 1L)
  r2 <- copy_labels(s, src)
  expect_false(inherits(r2$x, "haven_labelled"))
  expect_null(attr(r2$x, "labels"))
  expect_equal(attr(r2$x, "label"), "Item x")
  # compatible (plain subset of the codes, integer): labels restored
  plain <- data.frame(x = c(1L, 9L), y = c(2, 1))
  r3 <- copy_labels(plain, src)
  expect_s3_class(r3$x, "haven_labelled")
  expect_equal(as.numeric(r3$x), c(1, 9))
  expect_equal(attr(r3$y, "labels"), c(m = 1, f = 2))
})

# IO-08: set_na(df, -9, -8, tag = FALSE) (the documented example) stripped
# every variable and value label of the numeric columns.
test_that("IO-08: set_na(tag = FALSE) keeps variable and value labels", {
  r <- set_na(survey_data, -9, -8, tag = FALSE)
  expect_equal(var_label(r), var_label(survey_data))
  expect_identical(r$age, survey_data$age)
  skip_if_not_installed("haven")
  d <- tibble::tibble(
    q = haven::labelled(c(1, -9, 2, -8), labels = c(yes = 1, no = 2,
                                                   "n.a." = -9),
                        label = "Question")
  )
  r2 <- set_na(d, -9, -8, tag = FALSE)
  expect_s3_class(r2$q, "haven_labelled")
  expect_equal(attr(r2$q, "labels"), c(yes = 1, no = 2))
  expect_equal(attr(r2$q, "label"), "Question")
  expect_equal(sum(is.na(r2$q)), 2L)
  # named form and vector form behave the same
  expect_equal(attr(set_na(d, q = -9, tag = FALSE)$q, "label"), "Question")
  expect_s3_class(set_na(d$q, -9, tag = FALSE), "haven_labelled")
})

# IO-22: val_labels() SET on a factor/character column was silently
# accepted (meaningless labels attribute); set_na() silently skipped
# factors.
test_that("IO-22: val_labels() refuses factor and character columns", {
  expect_error(val_labels(survey_data, gender = c(Male = 1)), "factor")
  d <- data.frame(s = c("a", "b"))
  expect_error(val_labels(d, s = c(A = 1)), "character")
  # numeric still works
  r <- val_labels(survey_data, political_orientation = c(left = 1, right = 5))
  expect_equal(attr(r$political_orientation, "labels"), c(left = 1, right = 5))
})

test_that("IO-22: set_na() on a factor warns instead of silently skipping", {
  expect_warning(r <- set_na(survey_data, gender = 1), "factor")
  expect_identical(r$gender, survey_data$gender)
  expect_warning(set_na(survey_data$gender, 1), "factor")
})

# IO-09: to_dummy(): label-based names collided (ALLBUS ".." -> one column
# "pt12_", values lost), umlauts were stripped ("männlich" -> "mnnlich"),
# and `ref` was ignored for factors (both dummies returned).
test_that("IO-09: to_dummy() label names are unique and transliterated", {
  skip_if_not_installed("haven")
  x <- haven::labelled(c(1, 2, 3, 4, 2),
                       labels = c("gar nicht" = 1, ".." = 2, ".." = 3,
                                  "sehr" = 4))
  d <- to_dummy(x, suffix = "label")
  expect_equal(names(d), c("x_gar_nicht", "x_2", "x_3", "x_sehr"))
  expect_equal(d$x_3, c(0L, 0L, 1L, 0L, 0L))
  g <- haven::labelled(c(1, 2), labels = c("männlich" = 1, "weiblich" = 2,
                                           "Größe" = 3))
  expect_equal(names(to_dummy(g, suffix = "label")),
               c("g_maennlich", "g_weiblich"))
  expect_equal(.clean_label_for_colname("Größe Übel"), "Groesse_Uebel")
})

test_that("IO-09: to_dummy() ref works for factors (name or position)", {
  d1 <- to_dummy(survey_data, gender, ref = 1, append = FALSE)
  expect_equal(names(d1), "gender_Female")
  d2 <- to_dummy(survey_data, gender, ref = "Female", append = FALSE)
  expect_equal(names(d2), "gender_Male")
  expect_error(to_dummy(survey_data, gender, ref = "Other"), "ref")
  expect_error(to_dummy(c(1, 2, 3), ref = 9), "ref")
})

# IO-10 (= SCALE-10): row_count(count = c(4, 5)) silently recycled the
# vector over the cells; count = NA returned 0. SCALE-11: count = -9 was
# always 0 on read_spss() data because -9 is a tagged NA there.
test_that("IO-10: row_count() counts value sets and missing values", {
  d <- data.frame(a = c(4, 5, 1, NA), b = c(5, 1, 4, NA), c = c(1, 5, NA, 2))
  expect_equal(row_count(d, a, b, c, count = c(4, 5)), c(2L, 2L, 1L, 0L))
  expect_equal(row_count(d, a, b, c, count = NA), c(0L, 0L, 1L, 2L))
  expect_equal(row_count(d, a, b, c, count = c(5, NA)), c(1L, 2L, 1L, 2L))
  expect_equal(row_count(d, a, b, c, count = 4), c(1L, 0L, 1L, 0L))
  expect_error(row_count(d, a, b, count = "x"), "count")
})

test_that("SCALE-11: row_count() counts SPSS missing codes of imported data", {
  skip_if_not_installed("haven")
  d <- tibble::tibble(
    q1 = set_na(c(1, -9, 3, -8), -9, -8),
    q2 = set_na(c(-9, -9, 2, 1), -9, -8)
  )
  expect_equal(row_count(d, q1, q2, count = -9), c(1L, 2L, 0L, 0L))
  expect_equal(row_count(d, q1, q2, count = c(-9, -8)), c(1L, 2L, 0L, 1L))
  expect_equal(row_count(d, q1, q2, count = NA), c(1L, 2L, 0L, 1L))
})

# SCALE-19: row_means(., ...) inside a grouped mutate() failed with dplyr's
# size-mismatch error; pick() silently dropped non-numeric columns;
# min_valid = 2.5 was accepted.
test_that("SCALE-19: row_means() in grouped mutate: clear error, pick() works", {
  g <- dplyr::group_by(survey_data, region)
  expect_error(
    g %>% dplyr::mutate(m = row_means(., trust_government, trust_media)),
    "pick"
  )
  r <- dplyr::mutate(g, m = row_means(pick(trust_government, trust_media)))
  expect_equal(r$m, row_means(survey_data, trust_government, trust_media))
  # ungrouped `.` keeps working
  r2 <- survey_data %>% dplyr::mutate(m = row_means(., trust_government,
                                                 trust_media))
  expect_equal(r2$m, r$m)
})

test_that("SCALE-19: pick() with non-numeric columns warns; min_valid integer", {
  expect_warning(
    r <- dplyr::mutate(survey_data,
                       m = row_means(pick(gender, trust_government,
                                          trust_media))),
    "gender"
  )
  expect_equal(r$m, row_means(survey_data, trust_government, trust_media))
  expect_error(row_means(survey_data, trust_government, trust_media,
                         min_valid = 2.5), "whole number")
})

# SCALE-15: pomps() silently produced -25 / 200 for values outside the
# scale (a typical unrecoded 9 = "don't know"); scale_min = c(1, 2) gave a
# German base error, scale_min = NA a cryptic one, all-NA input German
# warnings.
test_that("SCALE-15: pomps() warns about values outside the scale range", {
  expect_warning(r <- pomps(c(0, 3, 6, 9), 1, 5), "outside")
  expect_equal(r, c(-25, 50, 125, 200))
  expect_no_warning(pomps(c(1, 3, 5, NA), 1, 5))
})

test_that("SCALE-15: pomps() validates the scale range and all-NA input", {
  expect_error(pomps(1:5, scale_min = c(1, 2), scale_max = 5), "scale_min")
  expect_error(pomps(1:5, scale_min = NA, scale_max = 5), "scale_min")
  expect_error(pomps(1:5, scale_min = 1, scale_max = Inf), "scale_max")
  expect_error(pomps(1:5, scale_min = 5, scale_max = 1), "less than")
  expect_error(pomps(c(NA_real_, NA_real_)), "no valid values")
  expect_no_warning(r <- pomps(c(NA_real_, NA_real_), 1, 5))
  expect_true(all(is.na(r)))
})

# IO-24: std()/center() in place dropped the variable label of SPSS
# variables (label read after the column was overwritten); grouped std()
# left a dbl+lbl column; the zero-SD warning named neither variable nor
# group.
test_that("IO-24: std()/center() keep the variable label, return plain doubles", {
  skip_if_not_installed("haven")
  d <- tibble::tibble(
    g = c(1, 1, 1, 2, 2, 2),
    x = haven::labelled(c(1, 2, 3, 2, 4, 6), labels = c(low = 1),
                        label = "Trust")
  )
  s <- std(d, x)
  expect_equal(attr(s$x, "label"), "Trust (standardized)")
  expect_false(inherits(s$x, "haven_labelled"))
  expect_null(attr(s$x, "labels"))
  cen <- center(d, x)
  expect_equal(attr(cen$x, "label"), "Trust (centered)")
  gs <- std(dplyr::group_by(d, g), x)
  expect_false(inherits(gs$x, "haven_labelled"))
  expect_type(unclass(gs$x), "double")
  expect_equal(as.numeric(gs$x), c(-1, 0, 1, -1, 0, 1))
  expect_equal(attr(gs$x, "label"), "Trust (standardized)")
  gc <- center(dplyr::group_by(d, g), x, suffix = "_c")
  expect_false(inherits(gc$x_c, "haven_labelled"))
  expect_equal(as.numeric(gc$x_c), c(-1, 0, 1, -2, 0, 2))
})

test_that("IO-24: the zero-SD warning names variable and group", {
  d <- data.frame(g = c("a", "a", "b", "b"), x = c(1, 1, 2, 3))
  expect_warning(std(d[1:2, ], x), "`x`")
  expect_warning(std(dplyr::group_by(d, g), x), "g = a")
})

# IO-14: write_spss()/write_stata() renumbered a to_label() factor to 1..k
# (ALLBUS dm06 codes 100, 120, ... became 1, 2, ...), ignoring its "codes"
# attribute.
test_that("IO-14: exporters keep the original codes of to_label() factors", {
  skip_if_not_installed("haven")
  x <- haven::labelled(c(100, 120, 140, 100),
                       labels = c(low = 100, mid = 120, high = 140),
                       label = "Income band")
  d <- data.frame(id = 1:4)
  d$x <- to_label(x)
  tf <- tempfile(fileext = ".sav")
  tf2 <- tempfile(fileext = ".dta")
  on.exit(unlink(c(tf, tf2)))
  suppressMessages(write_spss(d, tf))
  back <- read_spss(tf)
  expect_equal(as.numeric(back$x), c(100, 120, 140, 100))
  expect_equal(attr(back$x, "labels"), c(low = 100, mid = 120, high = 140))
  expect_equal(attr(back$x, "label"), "Income band")
  suppressMessages(write_stata(d, tf2))
  back2 <- read_stata(tf2)
  expect_equal(as.numeric(back2$x), c(100, 120, 140, 100))
  # a plain factor is still written as 1..k with its levels as labels
  d$f <- factor(c("b", "a", "b", "a"))
  suppressMessages(write_spss(d, tf))
  expect_equal(as.numeric(read_spss(tf)$f), c(2, 1, 2, 1))
})

# IO-15: write_spss() on unchanged ALLBUS data emitted ~133 warnings ("4
# discrete missing codes exceed SPSS's limit of 3 ... range -42--8"):
# read_spss() forgot the original definition (LOWEST THRU -1), >3 codes
# always became a min-max range with one warning per variable.
test_that("IO-15: read_spss() -> write_spss() keeps the original missing spec", {
  skip_if_not_installed("haven")
  make <- function(v) haven::labelled_spss(v, labels = c(yes = 1, no = 2),
                                           na_range = c(-Inf, -1))
  d <- tibble::tibble(a = make(c(1, 2, -42, -11, -9, -8)),
                      b = make(c(2, 1, -11, -9, -8, -42)))
  tf <- tempfile(fileext = ".sav")
  tf2 <- tempfile(fileext = ".sav")
  on.exit(unlink(c(tf, tf2)))
  haven::write_sav(d, tf)
  imported <- read_spss(tf)
  msgs <- character(0)
  expect_no_warning(withCallingHandlers(
    write_spss(imported, tf2),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  ))
  expect_false(any(grepl("missing", msgs)))
  back <- haven::read_sav(tf2, user_na = TRUE)
  expect_equal(attr(back$a, "na_range"), c(-Inf, -1))
  expect_equal(as.numeric(unclass(back$a)), c(1, 2, -42, -11, -9, -8))
})

test_that("IO-15: >3 codes use range + one discrete value, one message", {
  skip_if_not_installed("haven")
  x <- set_na(c(1, 2, 3, -42, -11, -9, -8), -42, -11, -9, -8)
  d <- tibble::tibble(a = x, b = x, c = x)
  tf <- tempfile(fileext = ".sav")
  on.exit(unlink(tf))
  msgs <- character(0)
  expect_no_warning(
    withCallingHandlers(
      write_spss(d, tf),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
  )
  range_msgs <- grep("range", msgs, value = TRUE)
  expect_length(range_msgs, 1L)
  expect_match(range_msgs, "-11 to -8")
  back <- haven::read_sav(tf, user_na = TRUE)
  expect_equal(attr(back$a, "na_range"), c(-11, -8))
  expect_equal(attr(back$a, "na_values"), -42)
})

# IO-25: read_spss() on a .dta/.xlsx file and read_stata() on a .sav file
# failed with raw readstat errors ("Unable to convert string to the
# requested encoding", "Failed to parse ... : This version of the file
# format is not supported.").
test_that("IO-25: readers say which file type they were given", {
  skip_if_not_installed("haven")
  sav <- tempfile(fileext = ".sav")
  dta <- tempfile(fileext = ".dta")
  on.exit(unlink(c(sav, dta)))
  haven::write_sav(data.frame(x = 1:3), sav)
  haven::write_dta(data.frame(x = 1:3), dta)
  expect_error(read_spss(dta), "Stata")
  expect_error(read_spss(dta), "read_stata")
  expect_error(read_stata(sav), "SPSS")
  expect_error(read_stata(sav), "read_spss")
  expect_error(read_sas(sav), "SPSS")
  expect_error(read_spss(file.path(tempdir(), "does-not-exist.sav")),
               "does not exist")
  if (requireNamespace("openxlsx2", quietly = TRUE)) {
    xl <- tempfile(fileext = ".xlsx")
    on.exit(unlink(xl), add = TRUE)
    write_xlsx(data.frame(x = 1:3), xl)
    expect_error(read_spss(xl), "Excel")
  }
  # correct types still read
  expect_equal(nrow(read_spss(sav)), 3L)
  expect_equal(nrow(read_stata(dta)), 3L)
})

# IO-13: write_xpt() (default version 5) silently truncated variable names
# to 8 characters, creating duplicates (trust_government, trust_media ->
# "trust_go", "trust_me"; life_satisfaction ...).
test_that("IO-13: write_xpt() warns about truncated names, refuses duplicates", {
  skip_if_not_installed("haven")
  tf <- tempfile(fileext = ".xpt")
  on.exit(unlink(tf))
  d <- data.frame(id = 1:2, satisfaction = c(1, 2))
  expect_warning(suppressMessages(write_xpt(d, tf)), "version = 8")
  d2 <- data.frame(trust_government = 1:2, trust_goals = 3:4)
  expect_error(suppressMessages(write_xpt(d2, tf)), "duplicate")
  expect_no_warning(suppressMessages(write_xpt(d2, tf, version = 8)))
  expect_equal(names(read_xpt(tf)), c("trust_government", "trust_goals"))
})

# IO-26: write_xlsx(list) with two names identical in their first 31
# characters (Excel's limit) crashed without writing a file; a missing
# output directory gave openxlsx2's internal error.
test_that("IO-26: write_xlsx() makes sheet names unique", {
  skip_if_not_installed("openxlsx2")
  tf <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tf))
  d <- data.frame(x = 1:2)
  attr(d$x, "label") <- "X"  # gives the combined Labels sheet content
  lst <- list(d, d, d)
  names(lst) <- c(paste0(strrep("a", 31), "_first"),
                  paste0(strrep("a", 31), "_second"),
                  "Labels")
  expect_message(write_xlsx(lst, tf), "sheet")
  sheets <- openxlsx2::wb_load(tf)$sheet_names
  expect_equal(length(unique(tolower(sheets))), length(sheets))
  expect_true(all(nchar(sheets) <= 31))
  expect_equal(length(sheets), 4L)
})

test_that("IO-26: write_xlsx() reports a missing directory", {
  skip_if_not_installed("openxlsx2")
  bad <- file.path(tempdir(), "no-such-dir-io26", "out.xlsx")
  expect_error(write_xlsx(data.frame(x = 1), bad), "does not exist")
  expect_error(write_xlsx(frequency(survey_data, gender), bad),
               "does not exist")
})

# IO-11: write_xlsx(grouped frequency) wrote one header (first group only),
# no group labels, and summed the groups' rows (Total Raw % = 200); the
# weighted header showed raw floats ("N=5245.99999999998").
test_that("IO-11: write_xlsx() writes one block per group, rounded N", {
  skip_if_not_installed("openxlsx2")
  tf <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tf))
  f <- survey_data %>% dplyr::group_by(region) %>% frequency(gender)
  write_xlsx(f, tf)
  cells <- openxlsx2::wb_to_df(openxlsx2::wb_load(tf), col_names = FALSE)
  txt <- as.character(unlist(cells))
  expect_true(any(grepl("region = East", txt, fixed = TRUE)))
  expect_true(any(grepl("region = West", txt, fixed = TRUE)))
  expect_equal(sum(grepl("^N=", txt)), 2L)
  # every Total row reports 100 %, never 200
  total_rows <- which(cells[[1]] == "Total")
  raw_pct <- suppressWarnings(as.numeric(cells[[4]][total_rows]))
  expect_true(all(raw_pct == 100))

  fw <- frequency(survey_data, gender, weights = sampling_weight)
  write_xlsx(fw, tf)
  txt_w <- as.character(unlist(openxlsx2::wb_to_df(openxlsx2::wb_load(tf),
                                                   col_names = FALSE)))
  n_line <- grep("^N=", txt_w, value = TRUE)
  expect_match(n_line, "^N=2516 ")
})

# IO-17: find_var() was regex-only: "BEFRAGTE(R)" matched "BEFRAGTER"
# labels instead of the literal text, "(" leaked a TRE regex warning, and
# an empty result printed as "<0 Zeilen>".
io_find_df <- function() {
  d <- data.frame(a = 1, b = 2, c = 3)
  attr(d$a, "label") <- "ALTER: BEFRAGTE(R)"
  attr(d$b, "label") <- "GESCHLECHT BEFRAGTER"
  attr(d$c, "label") <- "Einkommen (netto)"
  d
}

test_that("IO-17: find_var(fixed = TRUE) searches literal text", {
  d <- io_find_df()
  expect_equal(find_var(d, "BEFRAGTE(R)", fixed = TRUE)$name, "a")
  # regex stays the default; a differing literal result is pointed out
  expect_message(r <- find_var(d, "BEFRAGTE(R)"), "fixed = TRUE")
  expect_equal(r$name, "b")
  # a genuine regex without literal hits stays quiet
  expect_no_message(r2 <- find_var(d, "^GESCH"))
  expect_equal(r2$name, "b")
})

test_that("IO-17: invalid regex falls back to literal text, empty result is quiet", {
  d <- io_find_df()
  expect_no_warning(expect_message(r <- find_var(d, "("), "literal"))
  expect_equal(r$name, c("a", "c"))
  expect_message(out <- withVisible(find_var(d, "zzz")), "No variables")
  expect_false(out$visible)
  expect_equal(nrow(out$value), 0L)
})

# IO-20: strip_tags()/untag_na() had no data-frame support (German base
# error), weak input checks (strip_tags("a") returned NA), untag_na()
# dropped the value and variable labels.
test_that("IO-20: untag_na() keeps labels, missing labels move to their codes", {
  skip_if_not_installed("haven")
  x <- haven::labelled(c(1, -9, 2, -8), labels = c(yes = 1, no = 2,
                                                  "no answer" = -9),
                       label = "Q1")
  x <- set_na(x, -9, -8)
  u <- untag_na(x)
  expect_s3_class(u, "haven_labelled")
  expect_equal(as.numeric(u), c(1, -9, 2, -8))
  expect_equal(attr(u, "labels"), c(yes = 1, no = 2, "no answer" = -9))
  expect_equal(attr(u, "label"), "Q1")
  expect_null(attr(u, "na_tag_map"))
})

test_that("IO-20: strip_tags()/untag_na() work on data frames", {
  skip_if_not_installed("haven")
  d <- tibble::tibble(a = set_na(c(1, -9, 2), -9), b = c("x", "y", "z"),
                      c = set_na(c(-8, 1, 1), -8))
  s <- strip_tags(d)
  expect_true(all(is.na(.na_tags(s$a))))
  expect_identical(s$b, d$b)
  u <- untag_na(d)
  expect_equal(as.numeric(u$a), c(1, -9, 2))
  expect_equal(as.numeric(u$c), c(-8, 1, 1))
  u2 <- untag_na(d, a)
  expect_equal(as.numeric(u2$a), c(1, -9, 2))
  expect_true(is.na(u2$c[1]))
})

test_that("IO-20: strip_tags()/untag_na() reject non-numeric vectors clearly", {
  skip_if_not_installed("haven")
  expect_error(strip_tags(c("a", "b")), "numeric")
  expect_error(untag_na(c("a", "b")), "numeric")
  expect_error(strip_tags(list(1)), "numeric")
})

# IO-21: na_frequencies() sorted by n (frequency() sorts by code), put the
# technical tag letter first, returned the code as character, had no
# percentages, always added a "(System Missing) 0" row and printed an empty
# result as "<0 Zeilen>".
test_that("IO-21: na_frequencies() is ordered by code with numeric codes and %", {
  skip_if_not_installed("haven")
  x <- haven::labelled(c(1, -42, -9, -9, -11, -11, -11, 2, 3, 4),
                       labels = c("no answer" = -9, "n.a." = -11,
                                  "error" = -42))
  x <- set_na(x, -42, -11, -9)
  nf <- na_frequencies(x)
  expect_equal(names(nf), c("code", "label", "n", "prc", "tag"))
  expect_equal(nf$code, c(-42, -11, -9))
  expect_type(nf$code, "double")
  expect_equal(nf$n, c(1L, 3L, 2L))
  expect_equal(nf$prc, c(10, 30, 20))
  expect_false(any(nf$label == "(System Missing)"))
  y <- x
  y[1] <- NA
  nf2 <- na_frequencies(y)
  expect_equal(nf2$label[nrow(nf2)], "(System Missing)")
  expect_true(is.na(nf2$code[nrow(nf2)]))
})

test_that("IO-21: na_frequencies() without missings is quiet; data frames work", {
  skip_if_not_installed("haven")
  expect_message(out <- withVisible(na_frequencies(c(1, 2, 3))), "No missing")
  expect_false(out$visible)
  d <- tibble::tibble(a = set_na(c(1, -9, -9), -9), b = set_na(c(-8, 1, 2), -8),
                      s = c("x", "y", "z"))
  nf <- na_frequencies(d)
  expect_equal(nf$variable, c("a", "b"))
  expect_equal(nf$code, c(-9, -8))
  expect_equal(na_frequencies(d, b)$variable, "b")
})

# IO-27: no `var_label<-` replacement function; drop_labels() ignored the
# unused levels of factors (and its documented example was a no-op).
test_that("IO-27: var_label<- sets and removes variable labels", {
  x <- c(1, 2, 3)
  var_label(x) <- "Score"
  expect_equal(var_label(x), "Score")
  var_label(x) <- NULL
  expect_null(var_label(x))
  d <- data.frame(a = 1:2, b = 3:4)
  var_label(d) <- list(a = "First", b = "Second")
  expect_equal(var_label(d), c(a = "First", b = "Second"))
  var_label(d) <- c(a = "Only a")
  expect_equal(var_label(d)[["a"]], "Only a")
  expect_equal(var_label(d)[["b"]], "Second")
  expect_error(var_label(d) <- list(zz = "x"), "zz")
  expect_error(var_label(x) <- c("a", "b"), "single")
})

test_that("IO-27: drop_labels() drops unused factor levels, keeps attributes", {
  f <- factor(c("a", "b", "a"), levels = c("a", "b", "c"))
  attr(f, "label") <- "Letters"
  r <- drop_labels(f)
  expect_equal(levels(r), c("a", "b"))
  expect_equal(attr(r, "label"), "Letters")
  skip_if_not_installed("haven")
  g <- to_label(haven::labelled(c(10, 20, 10),
                                labels = c(x = 10, y = 20, z = 30)))
  rg <- drop_labels(g)
  expect_equal(levels(rg), c("x", "y"))
  expect_equal(to_numeric(rg), c(10, 20, 10))
  d <- dplyr::filter(survey_data, education != "Basic Secondary")
  expect_false("Basic Secondary" %in% levels(drop_labels(d)$education))
})
