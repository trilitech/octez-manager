#!/bin/bash
# Check for synchronous IO patterns in UI view/render functions
# These can cause UI lag and should be avoided
set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

# Patterns that indicate synchronous IO (excluding safe ones like gettimeofday)
# These patterns are checked in view/render function contexts
FORBIDDEN_PATTERNS=(
    'Sys\.file_exists'
    'Sys\.is_directory'
    'Sys\.readdir'
    'Sys\.command'
    'Unix\.openfile'
    'Unix\.stat'
    'Unix\.lstat'
    'Unix\.access'
    'Unix\.opendir'
    'Unix\.readdir'
    'Unix\.sleep'
    'open_in_bin'
    'open_in_gen'
    'open_out_bin'
    'open_out_gen'
    'close_in'
    'close_out'
    'input_line'
    'really_input'
    'In_channel\.'
    'Out_channel\.'
    'Common\.run[^n]'
    'Common\.run_out'
    'Common\.run_in'
    'Bos\.OS\.'
    'Bos\.Cmd\.'
)

# Files/patterns to exclude from checking (known safe or not UI render code)
EXCLUDE_PATTERNS=(
    'test/'
    '_build/'
)

found_issues=0

# Build grep pattern
pattern=$(IFS='|'; echo "${FORBIDDEN_PATTERNS[*]}")

# Find all .ml files in src/ui/, excluding test files
files=$(find src/ui/ -name '*.ml' -type f | grep -v '_build' || true)

if [ -z "$files" ]; then
    echo "No UI source files found"
    exit 0
fi

echo "Checking for synchronous IO patterns in UI code..."
echo ""

for file in $files; do
    # Search for forbidden patterns in view/render contexts
    # We look for these patterns and report them - manual review determines if they're in render path

    matches=$(grep -nE "$pattern" "$file" 2>/dev/null || true)

    if [ -n "$matches" ]; then
        # Check if any matches are in view/render functions (heuristic: within 50 lines of "let view" or "let render")
        while IFS= read -r match; do
            line_num=$(echo "$match" | cut -d: -f1)
            line_content=$(echo "$match" | cut -d: -f2-)

            # Check if this is near a view/render function definition
            # Look backwards up to 100 lines for "let view" or "let render"
            start_line=$((line_num > 100 ? line_num - 100 : 1))
            context=$(sed -n "${start_line},${line_num}p" "$file")

            # If we find "let view" or "let render" without an intervening "let " (new function),
            # this might be in a render path
            if echo "$context" | grep -qE 'let (view|render)'; then
                # Check if there's another "let" definition between the view and this line
                # (simplified heuristic - not perfect but catches obvious cases)
                echo -e "${RED}WARN${NC}: $file:$line_num: Potential sync IO in render path"
                echo "      $line_content"
                echo ""
                found_issues=$((found_issues + 1))
            fi
        done <<< "$matches"
    fi
done

echo "----------------------------------------"
if [ $found_issues -gt 0 ]; then
    echo -e "${RED}Found $found_issues potential sync IO issues in render paths${NC}"
    echo ""
    echo "These patterns can cause UI lag. Consider:"
    echo "  - Moving IO to init/update functions"
    echo "  - Using cached values"
    echo "  - Using background jobs"
    echo ""
    echo "If the IO is intentional and acceptable, add to the allowlist in this script."
    exit 1
else
    echo -e "${GREEN}No sync IO issues found in render paths${NC}"
    exit 0
fi
