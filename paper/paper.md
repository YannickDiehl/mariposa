---
title: 'mariposa: SPSS-compatible statistical analysis of survey data in R'
tags:
  - R
  - survey data
  - social sciences
  - SPSS
  - survey weights
  - labelled data
  - statistics
authors:
  - name: Yannick Diehl
    orcid: 0009-0009-8993-7353
    affiliation: 1
affiliations:
  - name: Philipps-Universität Marburg, Germany
    index: 1
date: 29 September 2026
bibliography: paper.bib
---

# Summary

Survey research in the social sciences has historically been dominated by
IBM SPSS Statistics, and decades of teaching materials, analysis protocols,
and published results are expressed in terms of SPSS procedures and output.
`mariposa` is an R package [@rcore] that supports the complete survey
analysis workflow — data import and export (SPSS, Stata, SAS, Excel) with
full metadata roundtripping, variable and value label management, recoding
and standardization, descriptive statistics, hypothesis tests, correlation,
regression, post-hoc comparisons, and scale analysis — through 80 functions
that produce results consistent with SPSS version 29, validated against
SPSS reference output within documented tolerances.

# Statement of need

Researchers and students migrating from SPSS to R face two practical gaps.

The first gap is *verifiable SPSS compatibility*. When a research group,
journal reviewer, or thesis supervisor expects numbers that match SPSS
output, subtle implementation differences matter: SPSS uses frequency-weight
semantics (weighted N as the sum of weights, with a corresponding Bessel
correction), Type-2 skewness and kurtosis, HAVERAGE (Type-6) weighted
quantiles, and specific post-hoc and residual formulas
[@ibmspss; @dallal1986; @haberman1973]. General-purpose R functions
legitimately make different choices, which produces small but
confidence-eroding discrepancies during migration. `mariposa` implements
the documented SPSS algorithms and validates its results against SPSS
version 29 reference output: several thousand reference assertions run in
continuous integration, tolerances are centralized in a tier registry
rather than scattered across tests, and statistics without an SPSS
equivalent (such as weighted variants of rank-based tests) are explicitly
disclosed in a per-function compatibility vignette rather than silently
approximated.

The second gap is *uniform weighted analysis*. Case weights in SPSS apply
globally to every procedure, but in R weight support varies by package and
function. In `mariposa`, every statistical entry point takes a `weights`
argument backed by one shared set of weighted kernels, so weighted and
unweighted analyses are available through one consistent interface.

`mariposa` is aimed at survey researchers, social scientists, lecturers,
and students who need SPSS-consistent results in a reproducible,
scriptable environment.

# State of the field

Users migrating from SPSS currently assemble their workflow from several
excellent but disjoint tools: `haven` imports and exports labelled data
[@haven], `sjmisc` and `sjlabelled` transform and label it
[@sjmisc; @sjlabelled], `expss` produces SPSS-style tables [@expss], the
`survey` package fits design-based estimators [@lumley2004], and base R or
dedicated packages run the tests. None of these packages claims — or
systematically verifies — agreement with SPSS results across the analysis
workflow, which is precisely the property that matters most during
migration.

`mariposa` was built as a new package rather than an extension of these
tools because SPSS consistency is an end-to-end property: it requires
controlling the weighted formulas, missing-data handling, and output
conventions across every step of the pipeline, not patching individual
functions. The package nevertheless complements rather than replaces the
existing ecosystem: `haven` provides the underlying file parsers, and users
who need design-based variance estimation for complex sampling designs
(strata, clusters, calibration) should use the `survey` package, whose
inferential goals differ from SPSS's frequency-weight model.

# Software design

Three design decisions distinguish the package. First, every weighted
formula lives in a single shared kernel module implementing SPSS
frequency-weight semantics; all statistical functions delegate to it, and a
package-wide invariance suite enforces that every weighted statistic
reproduces its unweighted counterpart when all weights equal one — a
property that guards against the formula drift that easily arises when
weighted formulas are reimplemented per function. Second, all functions
integrate with the tidyverse [@tidyverse]: variables are selected with
tidyselect syntax and every analysis respects `dplyr::group_by()`
partitions. Third, output follows a three-layer pattern — a compact
`print()` overview, a `summary()` method with SPSS-style detail, and
toggleable output sections — so users see familiar, publication-oriented
tables rather than raw list structures.

The SPSS validation framework is part of the package's public policy: a
binding validation charter defines per-statistic tolerance tiers, an
automated discipline test forbids ad-hoc inline tolerances, and a
compatibility vignette reports the validation status of every function.

# Research impact statement

`mariposa` has been developed openly on GitHub since June 2025 and was
released on CRAN in September 2026. The package is used in quantitative
methods teaching at Philipps-Universität Marburg — where students
transition from SPSS-based coursework to reproducible analyses in R — and
in day-to-day survey research practice, covering the full path from raw
data files to publication-ready results. By reproducing the numbers that
SPSS-trained researchers expect, it lowers the main practical barrier to
adopting reproducible, script-based survey analysis in the social
sciences.

# AI usage disclosure

The author used Anthropic's Claude (via the Claude Code development
environment) as a coding assistant throughout the development of this
software and in drafting this manuscript: code generation and refactoring,
test scaffolding, documentation drafting, and copy-editing. All
AI-assisted output was reviewed, edited, and validated by the author, who
designed the package architecture, the validation policy, and the
statistical scope. In addition to human review, the package's correctness
claims rest on an independent oracle: several thousand assertions against
SPSS version 29 reference output, run in continuous integration, together
with package-wide invariance tests. The author takes full responsibility
for the software and the content of this paper.

# Acknowledgements

The package's synthetic example data (`survey_data`, `longitudinal_data`)
were designed to mirror the structure of typical German social survey
datasets while containing no real respondent information.

# References
