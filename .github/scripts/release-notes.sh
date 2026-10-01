#!/usr/bin/env bash
# =============================================================================
# release-notes.sh - print the NEWS.md section of one version
# =============================================================================
# Usage: .github/scripts/release-notes.sh <version> [NEWS.md]
#        (version with or without the leading "v": 0.7.4 or v0.7.4)
#
# Prints the body of the "# mariposa <version>" section (heading dropped,
# surrounding blank lines trimmed). Used by .github/workflows/release-notes.yaml
# so the GitHub release notes are always the NEWS.md entry - NEWS.md stays the
# single source.
#
# Fails when the section is missing or empty, and when its heading still says
# "(development)": a version is tagged only after its NEWS heading is final.
# =============================================================================
set -euo pipefail

version="${1:?usage: release-notes.sh <version> [NEWS.md]}"
version="${version#v}"
news="${2:-NEWS.md}"

section=$(awk -v v="$version" '
  /^# mariposa / {
    if (found) exit
    if ($3 == v) {
      found = 1
      if ($0 ~ /\(development\)/) dev = 1
      next
    }
  }
  found { print }
  END {
    if (!found) exit 2
    if (dev) exit 3
  }
' "$news") || status=$?

case "${status:-0}" in
  0) ;;
  2) echo "release-notes: no '# mariposa $version' section in $news" >&2; exit 1 ;;
  3) echo "release-notes: $news still marks $version as (development) - finalize the heading before tagging" >&2; exit 1 ;;
  *) echo "release-notes: awk failed with status $status" >&2; exit 1 ;;
esac

# Drop leading blank lines; the command substitution drops trailing ones.
body=$(printf '%s\n' "$section" | sed -e '/./,$!d')

if [ -z "$body" ]; then
  echo "release-notes: the $version section in $news is empty" >&2
  exit 1
fi

printf '%s\n' "$body"
