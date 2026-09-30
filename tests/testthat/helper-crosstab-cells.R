# =============================================================================
# CROSSTABS CELL GRIDS — shared by the SPSS-validation tests of crosstab(),
# fisher_test() and friends
# =============================================================================
# SPSS CROSSTABS prints, per row category and for the Total row, one line per
# requested cell statistic (Count, Expected Count, % within <row>, % within
# <col>, % of Total, Residual). The validation files cache those lines as
# flat vectors in SPSS reading order (row by row, left to right), one R line
# per SPSS line with its citation. Cells that are 100.0% by construction are
# not cached (the row% Total column, the col% Total row, the grand-total
# total%), nor are the Expected-count margins (they repeat the counts):
#
#   count     : cells + row totals, then column totals + N     (R+1) x (C+1)
#   expected  : inner cells                                     R x C
#   residual  : inner cells, Count - Expected Count             R x C
#   row_pct   : inner cells, then the Total row                 (R+1) x C
#   col_pct   : inner cells + the row-total share               R x (C+1)
#   total_pct : as col_pct, then the Total row without 100%     (R+1)x(C+1)-1
#
# xt_spss_grid() builds the same vectors from one crosstab() layer: inner
# cells from $table / $expected / $row_pct / $col_pct / $total_pct, margins
# the way print.crosstab() prints them (row_totals / total, col_totals /
# total). Every element is named "<row> x <col>" for readable failures.
# =============================================================================

xt_spss_grid <- function(r) {
  rn <- as.character(r$row_levels)
  cn <- as.character(r$col_levels)
  rowmaj <- function(m, rows, cols) {
    m <- unclass(m)
    stats::setNames(as.vector(t(m)),
                    as.vector(t(outer(rows, cols, paste, sep = " x "))))
  }
  total_row <- function(v, cols) stats::setNames(unname(v), paste("Total x", cols))

  tab <- unclass(r$table)
  rt  <- unname(r$row_totals)
  ct  <- unname(r$col_totals)
  N   <- r$total

  list(
    count     = c(rowmaj(cbind(tab, rt), rn, c(cn, "Total")),
                  total_row(c(ct, N), c(cn, "Total"))),
    expected  = rowmaj(r$expected, rn, cn),
    residual  = rowmaj(tab - r$expected, rn, cn),
    row_pct   = c(rowmaj(r$row_pct, rn, cn), total_row(100 * ct / N, cn)),
    col_pct   = rowmaj(cbind(r$col_pct, 100 * rt / N), rn, c(cn, "Total")),
    total_pct = c(rowmaj(cbind(r$total_pct, 100 * rt / N), rn, c(cn, "Total")),
                  total_row(100 * ct / N, cn))
  )
}

# Assert every cached SPSS cell vector of one layer against crosstab() output.
# Counts: Spec (exact; weighted tables are /COUNT ROUND CELL integers).
# Expected counts, residuals and percentages: Display, 1 decimal as printed
# (exact rounding ties such as 412 / 1600 = 25.75%, printed 25.8, sit on the
# inclusive tolerance boundary; assert_spss()'s floating-point slack keeps
# them there).
# `truncated = TRUE` compares the leading cells only (an SPSS output file that
# ends mid-table).
assert_xt_cells <- function(r, spss, label, truncated = FALSE) {
  got <- xt_spss_grid(r)
  for (stat in names(spss)) {
    expected <- spss[[stat]]
    actual <- got[[stat]]
    if (is.null(actual)) stop(sprintf("assert_xt_cells(): unknown statistic %s", stat))
    if (truncated) actual <- actual[seq_len(min(length(actual), length(expected)))]
    testthat::expect_equal(length(actual), length(expected),
                           label = sprintf("%s %s: number of cells", label, stat))
    for (i in seq_len(min(length(actual), length(expected)))) {
      cell_label <- sprintf("%s %s [%s]", label, stat, names(actual)[i])
      if (stat == "count") {
        assert_spss_count(unname(actual[i]), expected[i], label = cell_label)
      } else {
        assert_spss(unname(actual[i]), expected[i], tier = "display",
                    precision = 1, label = cell_label)
      }
    }
  }
  invisible(TRUE)
}

# The layer of a grouped crosstab() whose group_info matches `...`
# (e.g. xt_layer(r, region = "East", gender = "Male")).
xt_layer <- function(r, ...) {
  want <- list(...)
  hit <- Filter(function(res) {
    all(vapply(names(want), function(v) {
      identical(as.character(res$group_info[[v]]), want[[v]])
    }, logical(1)))
  }, r$results)
  if (length(hit) != 1L) {
    stop(sprintf("xt_layer(): %d layers match %s", length(hit),
                 paste(names(want), unlist(want), sep = " = ", collapse = ", ")))
  }
  hit[[1]]
}
