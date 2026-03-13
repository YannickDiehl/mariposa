# ============================================================================
# RECODE - UNIT TESTS
# ============================================================================
# Purpose: Validate rec() and to_dummy() functions
# ============================================================================

library(testthat)
library(dplyr)
library(mariposa)

# ============================================================================
# TEST DATA
# ============================================================================

test_data <- tibble::tibble(
  id = 1:8,
  score = c(1, 2, 3, 4, 5, 3, NA, 2),
  rating = c(1, 1, 2, 2, 3, 3, 4, 4),
  gender = c(1, 2, 1, 2, 3, 1, 2, 3)
)

# Add value labels
attr(test_data$gender, "labels") <- c("Male" = 1, "Female" = 2, "Other" = 3)
attr(test_data$gender, "label") <- "Gender of respondent"
attr(test_data$score, "label") <- "Satisfaction score"

# ============================================================================
# rec() — SINGLE VALUE RECODING
# ============================================================================

test_that("rec recodes single values", {
  result <- rec(test_data, score, rules = "1=10; 2=20; 3=30; 4=40; 5=50")
  expect_equal(result$score[1], 10)
  expect_equal(result$score[2], 20)
  expect_equal(result$score[5], 50)
})

test_that("rec handles NA values (unmatched stay NA)", {
  result <- rec(test_data, score, rules = "1=10; 2=20; 3=30; 4=40; 5=50")
  expect_true(is.na(result$score[7]))
})

# ============================================================================
# rec() — RANGE RECODING
# ============================================================================

test_that("rec recodes ranges", {
  result <- rec(test_data, score, rules = "1:2=1; 3:4=2; 5=3")
  expect_equal(result$score[1], 1)  # 1 -> 1

  expect_equal(result$score[2], 1)  # 2 -> 1
  expect_equal(result$score[3], 2)  # 3 -> 2
  expect_equal(result$score[4], 2)  # 4 -> 2
  expect_equal(result$score[5], 3)  # 5 -> 3
})

test_that("rec supports min/max in ranges", {
  result <- rec(test_data, score, rules = "min:2=1; 3:max=2")
  expect_equal(result$score[1], 1)  # 1 -> 1 (min=1)
  expect_equal(result$score[2], 1)  # 2 -> 1
  expect_equal(result$score[3], 2)  # 3 -> 2
  expect_equal(result$score[5], 2)  # 5 -> 2 (max=5)
})

# ============================================================================
# rec() — ELSE RULE
# ============================================================================

test_that("rec else catches unmatched values", {
  result <- rec(test_data, score, rules = "1=1; 2=2; else=99")
  expect_equal(result$score[1], 1)
  expect_equal(result$score[2], 2)
  expect_equal(result$score[3], 99)  # 3 -> 99
  expect_equal(result$score[5], 99)  # 5 -> 99
})

test_that("rec else=NA sets unmatched to NA", {
  result <- rec(test_data, score, rules = "1=1; 2=2; else=NA")
  expect_equal(result$score[1], 1)
  expect_equal(result$score[2], 2)
  expect_true(is.na(result$score[3]))
})

# ============================================================================
# rec() — COPY RULE
# ============================================================================

test_that("rec copy preserves original values", {
  result <- rec(test_data, score, rules = "1=99; else=copy")
  expect_equal(result$score[1], 99)  # 1 -> 99
  expect_equal(result$score[2], 2)   # 2 -> copy -> 2
  expect_equal(result$score[3], 3)   # 3 -> copy -> 3
})

# ============================================================================
# rec() — NA HANDLING
# ============================================================================

test_that("rec can set values to NA", {
  result <- rec(test_data, score, rules = "1=NA; 2=NA; else=copy")
  expect_true(is.na(result$score[1]))
  expect_true(is.na(result$score[2]))
  expect_equal(result$score[3], 3)
})

test_that("rec can replace NA with a value", {
  result <- rec(test_data, score, rules = "NA=0; else=copy")
  expect_equal(result$score[7], 0)  # NA -> 0
  expect_equal(result$score[1], 1)  # 1 -> copy -> 1
})

# ============================================================================
# rec() — SPECIAL MODES
# ============================================================================

test_that("rec rev reverses a scale", {
  simple <- tibble::tibble(x = c(1, 2, 3, 4, 5))
  result <- rec(simple, x, rules = "rev")
  expect_equal(result$x, c(5, 4, 3, 2, 1))
})

test_that("rec rev handles NA", {
  simple <- tibble::tibble(x = c(1, NA, 3, 5))
  result <- rec(simple, x, rules = "rev")
  expect_equal(result$x[1], 5)
  expect_true(is.na(result$x[2]))
  expect_equal(result$x[3], 3)
  expect_equal(result$x[4], 1)
})

test_that("rec rev mirrors value labels", {
  labelled_data <- tibble::tibble(
    x = c(1, 2, 3, 4, 5)
  )
  attr(labelled_data$x, "labels") <- c("Strongly disagree" = 1, "Disagree" = 2,
                                         "Neutral" = 3, "Agree" = 4,
                                         "Strongly agree" = 5)
  result <- rec(labelled_data, x, rules = "rev")
  new_labels <- attr(result$x, "labels")
  expect_false(is.null(new_labels))
  # "Strongly disagree" was 1, now should be 5
  expect_equal(unname(new_labels["Strongly disagree"]), 5)
  # "Strongly agree" was 5, now should be 1
  expect_equal(unname(new_labels["Strongly agree"]), 1)
})

test_that("rec dicho dichotomizes at median", {
  simple <- tibble::tibble(x = c(1, 2, 3, 4, 5))
  result <- rec(simple, x, rules = "dicho")
  # Median = 3, <= 3 -> 0, > 3 -> 1
  expect_equal(result$x, c(0, 0, 0, 1, 1))
})

test_that("rec dicho(x) dichotomizes at fixed cut-point", {
  simple <- tibble::tibble(x = c(1, 2, 3, 4, 5))
  result <- rec(simple, x, rules = "dicho(2)")
  # <= 2 -> 0, > 2 -> 1
  expect_equal(result$x, c(0, 0, 1, 1, 1))
})

test_that("rec dicho handles NA", {
  simple <- tibble::tibble(x = c(1, NA, 3, 5))
  result <- rec(simple, x, rules = "dicho")
  expect_true(is.na(result$x[2]))
})

test_that("rec quart splits into 4 quartile groups", {
  # 20 values: 1..20 → Q1=5.75, Q2=10.5, Q3=15.25
  simple <- tibble::tibble(x = 1:20)
  result <- rec(simple, x, rules = "quart")
  # Values 1-5 <= Q1(5.75) → 1
  expect_true(all(result$x[1:5] == 1))
  # Values 6-10 <= Q2(10.5) → 2
  expect_true(all(result$x[6:10] == 2))
  # Values 11-15 <= Q3(15.25) → 3
  expect_true(all(result$x[11:15] == 3))
  # Values 16-20 > Q3 → 4
  expect_true(all(result$x[16:20] == 4))
})

test_that("rec quart handles NA", {
  simple <- tibble::tibble(x = c(1:10, NA))
  result <- rec(simple, x, rules = "quart")
  expect_true(is.na(result$x[11]))
  expect_true(all(!is.na(result$x[1:10])))
})

test_that("rec quart returns only values 1-4", {
  set.seed(42)
  simple <- tibble::tibble(x = rnorm(100))
  result <- rec(simple, x, rules = "quart")
  expect_true(all(result$x[!is.na(result$x)] %in% 1:4))
})

test_that("rec quart works with val.labels", {
  simple <- tibble::tibble(x = 1:20)
  result <- rec(simple, x, rules = "quart",
                val.labels = c("1" = "Q1", "2" = "Q2", "3" = "Q3", "4" = "Q4"))
  labels <- attr(result$x, "labels")
  expect_false(is.null(labels))
  expect_equal(unname(labels["Q1"]), 1)
  expect_equal(unname(labels["Q4"]), 4)
})

test_that("rec quart works with suffix", {
  simple <- tibble::tibble(x = 1:20)
  result <- rec(simple, x, rules = "quart", suffix = "_q")
  expect_true("x_q" %in% names(result))
  expect_true(all(result$x_q %in% 1:4))
  # Original column unchanged
  expect_equal(result$x, 1:20)
})

test_that("rec quart works with as.factor", {
  simple <- tibble::tibble(x = 1:20)
  result <- rec(simple, x, rules = "quart", as.factor = TRUE,
                val.labels = c("1" = "Q1", "2" = "Q2", "3" = "Q3", "4" = "Q4"))
  expect_true(is.factor(result$x))
  expect_equal(levels(result$x), c("Q1", "Q2", "Q3", "Q4"))
})

test_that("rec quart works on vector input", {
  x <- c(1, 5, 10, 15, 20)
  result <- rec(x, rules = "quart")
  expect_true(all(result %in% 1:4))
  expect_equal(result[1], 1)
  expect_equal(result[5], 4)
})

test_that("rec mean splits at the arithmetic mean", {
  simple <- tibble::tibble(x = c(1, 2, 3, 4, 10))
  # mean = 4, <= 4 -> 0, > 4 -> 1
  result <- rec(simple, x, rules = "mean")
  expect_equal(result$x, c(0, 0, 0, 0, 1))
})

test_that("rec mean differs from dicho (median split)", {
  # Skewed data where mean != median
  simple <- tibble::tibble(x = c(1, 2, 3, 4, 100))
  # median = 3, mean = 22
  result_dicho <- rec(simple, x, rules = "dicho")
  result_mean  <- rec(simple, x, rules = "mean")
  # dicho: <= 3 -> 0, > 3 -> 1
  expect_equal(result_dicho$x, c(0, 0, 0, 1, 1))
  # mean: <= 22 -> 0, > 22 -> 1
  expect_equal(result_mean$x, c(0, 0, 0, 0, 1))
})

test_that("rec mean handles NA", {
  simple <- tibble::tibble(x = c(1, NA, 3, 5))
  result <- rec(simple, x, rules = "mean")
  expect_true(is.na(result$x[2]))
  expect_true(all(!is.na(result$x[c(1, 3, 4)])))
})

test_that("rec mean works with suffix and val.labels", {
  simple <- tibble::tibble(x = c(1, 2, 3, 4, 5))
  result <- rec(simple, x, rules = "mean", suffix = "_m",
                val.labels = c("0" = "Low", "1" = "High"))
  expect_true("x_m" %in% names(result))
  labels <- attr(result$x_m, "labels")
  expect_equal(unname(labels["Low"]), 0)
  expect_equal(unname(labels["High"]), 1)
})

test_that("rec mean works on vector input", {
  x <- c(2, 4, 6, 8, 10)
  # mean = 6
  result <- rec(x, rules = "mean")
  expect_equal(result, c(0, 0, 0, 1, 1))
})

# ============================================================================
# rec() — SUFFIX & LABELS
# ============================================================================

test_that("rec suffix creates new columns", {
  result <- rec(test_data, score, rules = "rev", suffix = "_r")
  expect_true("score_r" %in% names(result))
  expect_true("score" %in% names(result))
  # Original unchanged
  expect_equal(result$score[1], 1)
  # Reversed in new column
  expect_equal(result$score_r[1], 5)
})

test_that("rec without suffix overwrites in-place", {
  result <- rec(test_data, score, rules = "1=99; else=copy")
  expect_equal(result$score[1], 99)
  expect_false("score_r" %in% names(result))
})

test_that("rec var.label is applied", {
  result <- rec(test_data, score, rules = "rev", suffix = "_r",
                var.label = "Reversed satisfaction")
  expect_equal(attr(result$score_r, "label"), "Reversed satisfaction")
})

test_that("rec auto-appends (recoded) to variable label", {
  result <- rec(test_data, score, rules = "1:2=1; 3:5=2")
  expect_equal(attr(result$score, "label"), "Satisfaction score (recoded)")
})

test_that("rec val.labels sets value labels", {
  result <- rec(test_data, score, rules = "1:2=1; 3=2; 4:5=3",
                val.labels = c("1" = "Low", "2" = "Medium", "3" = "High"))
  labels <- attr(result$score, "labels")
  expect_false(is.null(labels))
  expect_equal(unname(labels["Low"]), 1)
  expect_equal(unname(labels["High"]), 3)
})

# ============================================================================
# rec() — COPY PRESERVES VALUE LABELS
# ============================================================================

test_that("rec else=copy preserves all original value labels", {
  labelled <- tibble::tibble(x = c(1, 2, 3, 4, 5))
  attr(labelled$x, "labels") <- c("Very low" = 1, "Low" = 2, "Medium" = 3,
                                    "High" = 4, "Very high" = 5)
  # Recode only NA codes, copy everything else
  result <- rec(labelled, x, rules = "-9=NA; -8=NA; else=copy")
  labels <- attr(result$x, "labels")
  expect_false(is.null(labels))
  expect_equal(length(labels), 5)
  expect_equal(unname(labels["Very low"]), 1)
  expect_equal(unname(labels["Very high"]), 5)
})

test_that("rec copy preserves labels only for copied values", {
  labelled <- tibble::tibble(x = c(1, 2, 3, 4, 5))
  attr(labelled$x, "labels") <- c("Very low" = 1, "Low" = 2, "Medium" = 3,
                                    "High" = 4, "Very high" = 5)
  # Recode 1:2 to NA, copy the rest
  result <- rec(labelled, x, rules = "1:2=NA; else=copy")
  labels <- attr(result$x, "labels")
  expect_false(is.null(labels))
  # Only labels for 3, 4, 5 should remain
  expect_equal(length(labels), 3)
  expect_equal(unname(labels["Medium"]), 3)
  expect_equal(unname(labels["High"]), 4)
  expect_equal(unname(labels["Very high"]), 5)
})

test_that("rec copy with inline labels merges correctly", {
  labelled <- tibble::tibble(x = c(1, 2, 3, 4, 5))
  attr(labelled$x, "labels") <- c("Very low" = 1, "Low" = 2, "Medium" = 3,
                                    "High" = 4, "Very high" = 5)
  # Collapse 1:2 into 1 with new label, copy rest
  result <- rec(labelled, x, rules = "1:2=1 [Niedrig]; else=copy")
  labels <- attr(result$x, "labels")
  expect_false(is.null(labels))
  # 1 = "Niedrig" (inline), 3 = "Medium", 4 = "High", 5 = "Very high"
  expect_equal(unname(labels["Niedrig"]), 1)
  expect_equal(unname(labels["Medium"]), 3)
  expect_equal(unname(labels["Very high"]), 5)
  # Original "Very low" and "Low" labels should NOT be present
  expect_true(is.na(match("Very low", names(labels))))
  expect_true(is.na(match("Low", names(labels))))
})

test_that("rec copy does not keep labels for recoded new values", {
  labelled <- tibble::tibble(x = c(1, 2, 3, 4, 5))
  attr(labelled$x, "labels") <- c("A" = 1, "B" = 2, "C" = 3,
                                    "D" = 4, "E" = 5)
  # 4:5 → 1 (new meaning for value 1), copy rest
  result <- rec(labelled, x, rules = "4:5=1; else=copy")
  labels <- attr(result$x, "labels")
  # Original label "A"=1 should NOT be kept (value 1 now has a new meaning)
  expect_true(is.na(match("A", names(labels))))
  # Labels for 2 and 3 should be preserved
  expect_equal(unname(labels["B"]), 2)
  expect_equal(unname(labels["C"]), 3)
})

test_that("rec copy works with gender labels on vector input", {
  x <- c(1, 2, 3, -9, -8)
  attr(x, "labels") <- c("Male" = 1, "Female" = 2, "Other" = 3,
                           "No answer" = -9, "Refused" = -8)
  result <- rec(x, rules = "-9=NA; -8=NA; else=copy")
  labels <- attr(result, "labels")
  expect_equal(length(labels), 3)
  expect_equal(unname(labels["Male"]), 1)
  expect_equal(unname(labels["Female"]), 2)
  expect_equal(unname(labels["Other"]), 3)
})

test_that("rec without copy does not preserve original labels", {
  labelled <- tibble::tibble(x = c(1, 2, 3))
  attr(labelled$x, "labels") <- c("A" = 1, "B" = 2, "C" = 3)
  result <- rec(labelled, x, rules = "1=10; 2=20; 3=30")
  labels <- attr(result$x, "labels")
  # No copy, no inline labels → no labels
  expect_true(is.null(labels))
})

test_that("rec explicit val.labels override copy-preserved labels", {
  labelled <- tibble::tibble(x = c(1, 2, 3))
  attr(labelled$x, "labels") <- c("A" = 1, "B" = 2, "C" = 3)
  result <- rec(labelled, x, rules = "else=copy",
                val.labels = c("1" = "X", "2" = "Y", "3" = "Z"))
  labels <- attr(result$x, "labels")
  expect_equal(unname(labels["X"]), 1)
  expect_equal(unname(labels["Y"]), 2)
  expect_true(is.na(match("A", names(labels))))
})

# ============================================================================
# rec() — INLINE VALUE LABELS
# ============================================================================

test_that("rec inline labels set value labels", {
  result <- rec(test_data, score,
                rules = "1:2=1 [Low]; 3=2 [Medium]; 4:5=3 [High]")
  labels <- attr(result$score, "labels")
  expect_false(is.null(labels))
  expect_equal(unname(labels["Low"]), 1)
  expect_equal(unname(labels["Medium"]), 2)
  expect_equal(unname(labels["High"]), 3)
})

test_that("rec inline labels work with single values", {
  result <- rec(test_data, gender,
                rules = "1=1 [Mann]; 2=2 [Frau]; 3=3 [Divers]")
  labels <- attr(result$gender, "labels")
  expect_false(is.null(labels))
  expect_equal(unname(labels["Mann"]), 1)
  expect_equal(unname(labels["Frau"]), 2)
  expect_equal(unname(labels["Divers"]), 3)
})

test_that("rec explicit val.labels override inline labels", {
  result <- rec(test_data, score,
                rules = "1:2=1 [Low]; 3=2 [Medium]; 4:5=3 [High]",
                val.labels = c("1" = "Niedrig", "2" = "Mittel", "3" = "Hoch"))
  labels <- attr(result$score, "labels")
  expect_equal(unname(labels["Niedrig"]), 1)
  expect_equal(unname(labels["Mittel"]), 2)
  expect_equal(unname(labels["Hoch"]), 3)
  # Inline labels should NOT appear
  expect_true(is.na(match("Low", names(labels))))
})

test_that("rec inline labels with partial labeling", {
  # Only some rules have labels
  result <- rec(test_data, score,
                rules = "1:2=1 [Low]; 3=2; 4:5=3 [High]")
  labels <- attr(result$score, "labels")
  expect_false(is.null(labels))
  expect_equal(unname(labels["Low"]), 1)
  expect_equal(unname(labels["High"]), 3)
  # Value 2 should NOT be in labels
  expect_equal(length(labels), 2)
})

test_that("rec inline labels with else rule", {
  x <- c(1, 2, 3, 99)
  result <- rec(x, rules = "1=1 [Yes]; else=0 [No]")
  labels <- attr(result, "labels")
  expect_false(is.null(labels))
  expect_equal(unname(labels["Yes"]), 1)
  expect_equal(unname(labels["No"]), 0)
})

test_that("rec inline labels work on vector input", {
  x <- c(1, 2, 3, 4, 5)
  result <- rec(x, rules = "1:2=1 [Low]; 3=2 [Mid]; 4:5=3 [High]")
  labels <- attr(result, "labels")
  expect_equal(unname(labels["Low"]), 1)
  expect_equal(unname(labels["Mid"]), 2)
  expect_equal(unname(labels["High"]), 3)
})

test_that("rec inline labels with spaces in label text", {
  x <- c(1, 2, 3)
  result <- rec(x, rules = "1=1 [Strongly Disagree]; 2=2 [Neutral]; 3=3 [Strongly Agree]")
  labels <- attr(result, "labels")
  expect_equal(unname(labels["Strongly Disagree"]), 1)
  expect_equal(unname(labels["Strongly Agree"]), 3)
})

test_that("rec inline labels with as.factor uses labels as levels", {
  result <- rec(test_data, score,
                rules = "1:2=1 [Low]; 3=2 [Medium]; 4:5=3 [High]",
                as.factor = TRUE)
  expect_true(is.factor(result$score))
  expect_equal(levels(result$score), c("Low", "Medium", "High"))
})

test_that("rec inline labels are not set for copy or NA rules", {
  x <- c(1, 2, 3, NA)
  result <- rec(x, rules = "1=1 [Keep]; 2:3=copy; NA=99")
  labels <- attr(result, "labels")
  # Only value 1 should have a label
  expect_equal(length(labels), 1)
  expect_equal(unname(labels["Keep"]), 1)
})

# ============================================================================
# rec() — as.factor
# ============================================================================

test_that("rec as.factor returns factor", {
  result <- rec(test_data, score, rules = "1:2=1; 3=2; 4:5=3",
                as.factor = TRUE,
                val.labels = c("1" = "Low", "2" = "Medium", "3" = "High"))
  expect_true(is.factor(result$score))
  expect_equal(levels(result$score), c("Low", "Medium", "High"))
})

# ============================================================================
# rec() — MULTIPLE VARIABLES
# ============================================================================

test_that("rec works with multiple variables", {
  result <- rec(test_data, score, rating, rules = "rev", suffix = "_r")
  expect_true("score_r" %in% names(result))
  expect_true("rating_r" %in% names(result))
})

test_that("rec works with tidyselect", {
  data2 <- tibble::tibble(q1 = c(1, 2, 3), q2 = c(4, 5, 6), other = c(7, 8, 9))
  result <- rec(data2, starts_with("q"), rules = "rev", suffix = "_r")
  expect_true("q1_r" %in% names(result))
  expect_true("q2_r" %in% names(result))
  expect_false("other_r" %in% names(result))
})

# ============================================================================
# rec() — VECTOR INPUT
# ============================================================================

test_that("rec works with vector input", {
  result <- rec(c(1, 2, 3, 4, 5), rules = "1:2=1; 3:5=2")
  expect_equal(result, c(1, 1, 2, 2, 2))
})

test_that("rec rev works with vector input", {
  result <- rec(c(1, 2, 3, 4, 5), rules = "rev")
  expect_equal(result, c(5, 4, 3, 2, 1))
})

# ============================================================================
# rec() — ERROR HANDLING
# ============================================================================

test_that("rec errors on missing rules", {
  expect_error(rec(test_data, score))
})

test_that("rec errors on invalid rule syntax", {
  expect_error(rec(c(1, 2, 3), rules = "abc"))
})

test_that("rec errors on invalid range", {
  expect_error(rec(c(1, 2, 3), rules = "a:b=1"))
})

# ============================================================================
# rec() — FIRST RULE WINS
# ============================================================================

test_that("rec applies first matching rule", {
  result <- rec(c(1, 2, 3), rules = "1:3=10; 1=99")
  # 1 matches "1:3=10" first, so it becomes 10, not 99
  expect_equal(result[1], 10)
})

# ============================================================================
# to_dummy() TESTS
# ============================================================================

test_that("to_dummy creates correct dummy columns", {
  result <- to_dummy(test_data, gender)
  expect_true("gender_1" %in% names(result))
  expect_true("gender_2" %in% names(result))
  expect_true("gender_3" %in% names(result))
})

test_that("to_dummy values are 0/1", {
  result <- to_dummy(test_data, gender)
  # Row 1: gender=1 -> gender_1=1, gender_2=0, gender_3=0
  expect_equal(result$gender_1[1], 1L)
  expect_equal(result$gender_2[1], 0L)
  expect_equal(result$gender_3[1], 0L)
})

test_that("to_dummy with suffix=label uses value labels", {
  result <- to_dummy(test_data, gender, suffix = "label")
  expect_true("gender_Male" %in% names(result))
  expect_true("gender_Female" %in% names(result))
  expect_true("gender_Other" %in% names(result))
})

test_that("to_dummy ref omits reference category", {
  result <- to_dummy(test_data, gender, ref = 1, append = FALSE)
  expect_false("gender_1" %in% names(result))
  expect_true("gender_2" %in% names(result))
  expect_true("gender_3" %in% names(result))
  expect_equal(ncol(result), 2)
})

test_that("to_dummy handles NA correctly", {
  na_data <- tibble::tibble(x = c(1, 2, NA, 1))
  result <- to_dummy(na_data, x)
  expect_true(is.na(result$x_1[3]))
  expect_true(is.na(result$x_2[3]))
})

test_that("to_dummy works with multiple variables", {
  multi_data <- tibble::tibble(
    a = c(1, 2, 1),
    b = c(10, 20, 10)
  )
  result <- to_dummy(multi_data, a, b)
  expect_true("a_1" %in% names(result))
  expect_true("a_2" %in% names(result))
  expect_true("b_10" %in% names(result))
  expect_true("b_20" %in% names(result))
})

test_that("to_dummy returns tibble", {
  result <- to_dummy(test_data, gender)
  expect_s3_class(result, "tbl_df")
})

test_that("to_dummy with factor input uses levels", {
  fct_data <- tibble::tibble(
    color = factor(c("red", "blue", "red", "green"), levels = c("red", "blue", "green"))
  )
  result <- to_dummy(fct_data, color)
  expect_true("color_red" %in% names(result))
  expect_true("color_blue" %in% names(result))
  expect_true("color_green" %in% names(result))
  expect_equal(result$color_red, c(1L, 0L, 1L, 0L))
})

test_that("to_dummy errors on non-data-frame non-vector input", {
  expect_error(to_dummy(list(1, 2, 3)))
})

test_that("to_dummy cleans label names for columns", {
  labelled_data <- tibble::tibble(x = c(1, 2, 3))
  attr(labelled_data$x, "labels") <- c("Very Good!" = 1, "Not Bad" = 2, "  Bad  " = 3)
  result <- to_dummy(labelled_data, x, suffix = "label", append = FALSE)
  # Should clean: spaces -> _, special chars removed
  col_names <- names(result)
  expect_true(all(!grepl("[!]", col_names)))
  expect_true(all(!grepl("  ", col_names)))
})

test_that("to_dummy append=TRUE appends dummies to original data", {
  simple <- tibble::tibble(id = 1:3, x = c(1, 2, 1))
  result <- to_dummy(simple, x)
  # Original columns preserved
  expect_true("id" %in% names(result))
  expect_true("x" %in% names(result))
  # Dummy columns appended
  expect_true("x_1" %in% names(result))
  expect_true("x_2" %in% names(result))
  expect_equal(ncol(result), 4)  # 2 original + 2 dummies
  # Values correct
  expect_equal(result$id, 1:3)
  expect_equal(result$x_1, c(1L, 0L, 1L))
})

test_that("to_dummy append=FALSE returns only dummy columns", {
  simple <- tibble::tibble(id = 1:3, x = c(1, 2, 1))
  result <- to_dummy(simple, x, append = FALSE)
  expect_false("id" %in% names(result))
  expect_false("x" %in% names(result))
  expect_true("x_1" %in% names(result))
  expect_true("x_2" %in% names(result))
  expect_equal(ncol(result), 2)
})

test_that("to_dummy append=TRUE with multiple variables", {
  simple <- tibble::tibble(id = 1:3, a = c(1, 2, 1), b = c(10, 20, 10))
  result <- to_dummy(simple, a, b)
  expect_equal(ncol(result), 3 + 2 + 2)  # original + a dummies + b dummies
  expect_true("id" %in% names(result))
  expect_true("a_1" %in% names(result))
  expect_true("b_20" %in% names(result))
})
