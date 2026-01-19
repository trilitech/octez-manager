#!/usr/bin/env bash
# Copyright header checker and fixer for octez-manager
# Usage:
#   ./scripts/check-copyright.sh           # Check all files
#   ./scripts/check-copyright.sh --fix     # Fix all files
#   ./scripts/check-copyright.sh file.ml   # Check specific file

set -euo pipefail

# Generate copyright header with proper padding (80 columns)
generate_copyright() {
	local years="$1"
	local copyright_text="Copyright (c) $years Nomadic Labs <contact@nomadic-labs.com>"
	# Line structure: "(* " + text + padding + " *)"
	# Total width: 80, so text + padding must be 80 - 6 = 74
	local padding_needed=$((74 - ${#copyright_text}))
	local padding=$(printf '%*s' "$padding_needed" '')

	cat <<EOF
(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* $copyright_text$padding *)
(*                                                                            *)
(******************************************************************************)
EOF
}

generate_copyright_79() {
	local years="$1"
	local copyright_text="Copyright (c) $years Nomadic Labs <contact@nomadic-labs.com>"
	local padding_needed=$((75 - ${#copyright_text})) # 79 - 4 (for "(* " and " *)")
	local padding=$(printf '%*s' "$padding_needed" '')

	cat <<EOF
(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* $copyright_text$padding *)
(*                                                                           *)
(*****************************************************************************)
EOF
}

FIX_MODE=false
CHECK_SPECIFIC_FILE=""
ERRORS=0

# Parse arguments
while [[ $# -gt 0 ]]; do
	case $1 in
	--fix)
		FIX_MODE=true
		shift
		;;
	--help | -h)
		echo "Usage: $0 [--fix] [file.ml]"
		echo ""
		echo "Options:"
		echo "  --fix     Fix copyright headers automatically"
		echo "  file.ml   Check only the specified file"
		echo ""
		echo "The script checks that all .ml/.mli files have proper copyright headers"
		echo "with the correct year range based on git history."
		exit 0
		;;
	*)
		CHECK_SPECIFIC_FILE="$1"
		shift
		;;
	esac
done

# Get the year range for a file from git history
get_year_range() {
	local file="$1"

	# Check if file is tracked by git
	if ! git ls-files --error-unmatch "$file" &>/dev/null; then
		echo "2026" # Default for new untracked files
		return
	fi

	# Get first and last commit years
	local first_year
	local last_year

	first_year=$(git log --follow --format='%ad' --date=format:'%Y' -- "$file" 2>/dev/null | tail -1)
	last_year=$(git log --follow --format='%ad' --date=format:'%Y' -1 -- "$file" 2>/dev/null)

	# Handle case where file has no history yet (staged but not committed)
	if [[ -z "$first_year" ]]; then
		echo "2026"
		return
	fi

	if [[ "$first_year" == "$last_year" ]]; then
		echo "$first_year"
	else
		echo "$first_year-$last_year"
	fi
}

# Check if file has correct copyright header
check_copyright() {
	local file="$1"
	local year_range
	year_range=$(get_year_range "$file")

	# Read first 6 lines
	local header
	header=$(head -6 "$file" 2>/dev/null || echo "")

	# Check if header exists
	if [[ -z "$header" ]]; then
		echo "ERROR: $file has no copyright header"
		return 1
	fi

	# Check if it's 80-column format (5 stars)
	if ! echo "$header" | grep -q '^(\*\*\*\*\*'; then
		echo "ERROR: $file must use 80-column copyright format (5 stars)"
		echo "  Found: $(head -1 "$file")"
		return 1
	fi

	# Build expected header
	local expected
	expected=$(generate_copyright "$year_range")

	# Compare headers
	if [[ "$header" != "$expected" ]]; then
		echo "ERROR: $file has incorrect copyright header"
		echo "  Expected years: $year_range"
		return 1
	fi

	return 0
}

# Fix copyright header in file
fix_copyright() {
	local file="$1"
	local year_range
	year_range=$(get_year_range "$file")

	# Build correct header (always 80 columns)
	local correct_header
	correct_header=$(generate_copyright "$year_range")

	# Check if file already has a copyright header
	local header
	header=$(head -6 "$file" 2>/dev/null || echo "")

	if echo "$header" | grep -q "SPDX-License-Identifier"; then
		# Replace existing header (first 6 lines)
		local tmpfile
		tmpfile=$(mktemp)
		echo "$correct_header" >"$tmpfile"
		tail -n +7 "$file" >>"$tmpfile"
		mv "$tmpfile" "$file"
		echo "FIXED: $file (updated to $year_range)"
	else
		# Prepend header
		local tmpfile
		tmpfile=$(mktemp)
		echo "$correct_header" >"$tmpfile"
		echo "" >>"$tmpfile"
		cat "$file" >>"$tmpfile"
		mv "$tmpfile" "$file"
		echo "FIXED: $file (added copyright header for $year_range)"
	fi
}

# Process a single file
process_file() {
	local file="$1"

	# Skip non-.ml/.mli files
	if [[ ! "$file" =~ \.(ml|mli)$ ]]; then
		return 0
	fi

	# Skip if file doesn't exist
	if [[ ! -f "$file" ]]; then
		return 0
	fi

	if $FIX_MODE; then
		fix_copyright "$file"
	else
		if ! check_copyright "$file"; then
			((ERRORS++)) || true
		fi
	fi
}

# Main logic
if [[ -n "$CHECK_SPECIFIC_FILE" ]]; then
	# Check specific file
	process_file "$CHECK_SPECIFIC_FILE"
else
	# Check all tracked .ml/.mli files
	while IFS= read -r file; do
		process_file "$file"
	done < <(git ls-files '*.ml' '*.mli')
fi

# Report results
if ! $FIX_MODE; then
	if [[ $ERRORS -eq 0 ]]; then
		echo "✓ All copyright headers are correct"
		exit 0
	else
		echo ""
		echo "✗ Found $ERRORS files with incorrect copyright headers"
		echo "  Run with --fix to automatically correct them"
		exit 1
	fi
fi
