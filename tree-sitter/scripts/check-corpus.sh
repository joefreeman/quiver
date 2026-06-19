#!/usr/bin/env bash
# Parse every real Quiver source file in the repository and fail if any produces a
# syntax error. This is the sync-guard that keeps the tree-sitter grammar in step with
# the canonical parser in quiver-compiler/src/parser.rs: if a syntax change lands without
# a matching grammar update, a real .qv file stops parsing and this check fails.
set -euo pipefail

cd "$(dirname "$0")/.."

shopt -s nullglob
files=(../std/*.qv ../examples/*.qv)
if [ ${#files[@]} -eq 0 ]; then
  echo "No .qv files found to check." >&2
  exit 1
fi

echo "Checking ${#files[@]} Quiver source files for parse errors..."
# `tree-sitter parse` exits non-zero if any file contains an ERROR or MISSING node.
npx tree-sitter parse --quiet "${files[@]}"
echo "All Quiver source files parsed cleanly."
