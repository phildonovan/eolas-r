#!/usr/bin/env bash
# Pre-release gate for the `eolas` R client.
#
# Run this GREEN before any release (git tag / R package publish). The GitHub
# Actions workflow (.github/workflows/check.yml) is POST-HOC — it tells you the
# release is broken *after* you pushed. This script is the gate that runs
# *before*, so a doc/signature drift never reaches a tag again.
#
# This exact script would have caught the 2026-05-13 "undocumented args on
# eolas_get" regression: the roxygen-drift check below fails when someone edits
# a function signature without re-running roxygen.
#
# Usage:  ./scripts/preflight.sh
# Exit:   0 = safe to release, non-zero = do NOT release.

set -euo pipefail
cd "$(dirname "$0")/.."

fail() { echo "❌ PREFLIGHT FAILED: $*" >&2; exit 1; }

echo "==> 1/3  roxygen drift check (man/ + NAMESPACE in sync with R/)"
Rscript -e 'suppressMessages(roxygen2::roxygenise())' >/dev/null
if ! git diff --quiet -- man/ NAMESPACE; then
  echo "    man/ or NAMESPACE changed after roxygenise — docs are stale:" >&2
  git --no-pager diff --stat -- man/ NAMESPACE >&2
  fail "regenerate docs and commit (this is the class of bug that broke CI on 2026-05-13)"
fi
echo "    OK — docs are in sync"

echo "==> 2/3  R CMD build"
tarball_dir=$(mktemp -d)
R CMD build . --no-build-vignettes >/dev/null 2>&1 || fail "R CMD build failed"
tarball=$(ls -t eolas_*.tar.gz | head -1)

echo "==> 3/3  R CMD check (fails on WARNING or ERROR — matches check-r-package@v2)"
R CMD check "$tarball" --no-manual --output="$tarball_dir" >/dev/null 2>&1 || true
status_line=$(grep '^Status:' "$tarball_dir/eolas.Rcheck/00check.log" 2>/dev/null || echo "Status: UNKNOWN")
echo "    $status_line"
rm -f "$tarball"
rm -rf "$tarball_dir"
case "$status_line" in
  *ERROR*)   fail "R CMD check found ERRORs" ;;
  *WARNING*) fail "R CMD check found WARNINGs (check-r-package@v2 fails on these)" ;;
  *UNKNOWN*) fail "could not parse R CMD check status" ;;
esac

echo "✅ PREFLIGHT OK — safe to release the R client"
