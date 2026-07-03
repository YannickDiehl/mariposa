# Versioning Policy for mariposa

## 1. Purpose

This document defines how changes are classified into version increments and
how releases are cut. It exists because release 0.6.4 bundled several themes
(formula fixes, API changes, robustness, cleanup) into one large jump; going
forward, releases stay small and single-themed.

In case of conflict about validation matters, the
[Validation Charter](VALIDATION_CHARTER.md) takes precedence.

## 2. Version scheme

`MAJOR.MINOR.PATCH[.MICRO]` with pre-1.0 semantics:

| Segment | Bumped for | Example |
|---|---|---|
| MICRO (4th) | Invisible changes: docs, comments, internal-only refactors with byte-identical behavior, test-only changes, `.claude/` artifacts | 0.6.4 → 0.6.4.1 |
| PATCH (3rd) | The workhorse: bug fixes (results may change *toward* the correct reference), refactors, output-layer changes, deprecations, and - pre-1.0 only - breaking changes executed with a NEWS "Breaking changes" section and, where feasible, a deprecation bridge (§4) | 0.6.4 → 0.6.5 |
| MINOR (2nd) | Genuinely new capability: new statistical functions or feature sets (the 0.2.0-scale / 0.5.0-scale milestones of the version history) | 0.6.x → 0.7.0 |
| MAJOR | 1.0.0 = API freeze + full SPSS validation coverage per Charter §9. After 1.0, breaking changes require a major bump and the PATCH position reverts to fixes only. | 0.x → 1.0.0 |

## 3. Classification table

| Change type | Bump | NEWS.md |
|---|---|---|
| Typo/docs/comment fix | MICRO | optional |
| Internal refactor, zero behavior change (verified by green suite) | MICRO or PATCH* | short bullet |
| Test suite changes only | MICRO | no |
| Bug fix, output changes toward reference implementation | PATCH | required, "Bug fixes" |
| Robustness fix (crash → error/NA) | PATCH | required |
| Dependency added/removed | PATCH | required |
| New statistic or argument within an existing function | PATCH | required, "New features" |
| New exported function(s) / feature set (milestone-scale) | MINOR | required, "New features" |
| New output section (print/summary layout change) | MINOR | required |
| Deprecation warning introduced | MINOR | required, "Deprecations" |
| Deprecated thing removed / rename executed / result-object shape change | MINOR (pre-1.0) | required, "Breaking changes" section at top |
| Validation tier/tolerance change | per Charter, at least PATCH | required + Charter/Exceptions update |

\* PATCH when the refactor spans many files (easier to bisect against a
release than a micro).

## 4. Breaking changes (pre-1.0 rules)

The package has few users; breaking changes are acceptable but never casual:

1. Every breaking change gets its own bullet under a **"Breaking changes"**
   heading at the top of the release's NEWS entry, with a before/after
   example.
2. Renames ship with a soft-deprecation bridge for **one PATCH release**
   where feasible: the old name keeps working and warns once per session
   (`lifecycle`-style), and is removed in the following PATCH.
   Exceptions (no bridge) are allowed when the old behavior was wrong or
   the bridge is disproportionate effort - say so in NEWS.
3. Result-object fields follow the same rule: add the new field first,
   deprecate the old, remove one PATCH release later.

## 5. Release discipline

- **One theme per release.** A release is "hygiene", "print-layer
  refactor", "API rename" - not all three. If work naturally splits,
  release twice.
- **Small and frequent beats large and rare.** A release should be
  reviewable in one sitting from its NEWS entry.
- **Every release**: green `devtools::test()`, clean
  `devtools::check()`, NEWS.md entry, DESCRIPTION bump - in the same
  commit, titled `chore(release): x.y.z - <theme>`.
- Commits between releases carry conventional prefixes (`fix:`, `feat:`,
  `refactor:`, `docs:`, `test:`, `chore:`); the release commit's NEWS
  entry is assembled from them.

## 6. Current roadmap mapping (2026-07)

| Planned work | Release | Class |
|---|---|---|
| Package hygiene (pkgdown entries, import pruning, dead test registry, error-chain fixes, mechanical sweeps) | 0.6.5 | PATCH |
| Core output utilities + behavior-neutral convergence (table formatter, group iterator, number formatters, kernels file, tukey/scheffe engine, closure hoisting, roxygen standardization) | 0.6.6 | PATCH |
| Print-layer completion: three-layer migration of the remaining verbose print() classes; correlation engine; w_factory generalization | 0.6.7 | PATCH (visible output changes, declared) |
| API unification: dot → snake_case argument renames (with deprecation bridge), result-column harmonization, CI-alias removal | 0.6.8 | PATCH (breaking, with bridge, declared) |
| Deprecation removals + remaining SPSS validation gaps closed - done | 0.6.9 | PATCH |
| Result-column harmonization (canonical names, old kept as duplicate for one release) - done | 0.6.10 | PATCH (with duplicate bridge, declared) |
| Deprecation cleanup: remove 0.6.9 argument bridges + 0.6.10 duplicate columns - done | 0.6.11 | PATCH (breaking, bridges expired, declared) |
| Weighted-rank correctness (kendall_tau tau-b, kruskal_wallis grand mean rank) + w == 1 invariance suite + accurate claims - done | 0.6.12 | PATCH |
| McDonald's omega in reliability() (one-factor ML, weighted variant, omega-if-deleted; Tier 4 pending SPSS reference run) - done | 0.6.13 | PATCH (new statistic within an existing function) |
| SPSS v29 reference runs for the Tier-4/order-statistics gaps (weighted quantiles/median/IQR, tied Kendall, weighted regression w/ categorical predictor) — maintainer runs `.claude/spss-syntax-0.6.4-references.sps`, results pasted into the validation tests | 0.6.x | PATCH (needs maintainer + SPSS v29) |
| Optional polish: format_stat_table adoption, snapshot-test migration, remaining BACKLOG.md quality items | 0.6.x | PATCH/MICRO |
| API freeze + full SPSS validation coverage per Charter §9 | 1.0.0 | MAJOR |

---

*Owner: Yannick Diehl. Changes to this policy: commit titled
`policy: <change>` + NEWS mention in the next release.*
