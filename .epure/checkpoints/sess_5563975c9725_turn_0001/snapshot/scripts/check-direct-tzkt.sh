#!/bin/bash
# Fail if any .ml/.mli file outside the canonical TzKT URL locations contains a
# string literal with "api.tzkt.io" or "api.<network>.tzkt.io".
# Canonical locations:
#   lib/common/indexer.ml     — low-level HTTP routing
#   src/rewards/payout_config.ml — payout config default and network helper
set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

CANONICAL_FILES=(
  "lib/common/indexer.ml"
  "lib/common/indexer.mli"
  "src/rewards/payout_config.ml"
  "src/rewards/payout_config.mli"
)

echo "Checking for direct TzKT URL literals outside canonical modules..."
echo ""

found_issues=0

# Search for quoted string literals containing api.tzkt.io or api.<word>.tzkt.io
# We match the pattern inside double quotes to avoid flagging plain doc comments.
pattern='"[^"]*api(\.[a-z]+)?\.tzkt\.io[^"]*"'

is_canonical() {
  local file="$1"
  for canonical in "${CANONICAL_FILES[@]}"; do
    if [ "$file" = "$canonical" ]; then
      return 0
    fi
  done
  return 1
}

while IFS= read -r file; do
    if is_canonical "$file"; then
        continue
    fi

    matches=$(grep -nP "$pattern" "$file" 2>/dev/null || true)
    if [ -n "$matches" ]; then
        echo -e "${RED}ERROR${NC}: $file contains direct TzKT URL literal(s):"
        while IFS= read -r match; do
            echo "  $match"
        done <<< "$matches"
        echo "  TzKT URLs must only appear in lib/common/indexer.ml or src/rewards/payout_config.ml."
        echo ""
        found_issues=$((found_issues + 1))
    fi
done < <(find src lib -name '*.ml' -o -name '*.mli' | grep -v '_build' | grep -v '\.formatted' | sort)

if [ $found_issues -gt 0 ]; then
    echo -e "${RED}Found $found_issues file(s) with direct TzKT URL literals.${NC}"
    echo "TzKT URLs must only appear in the canonical modules listed above."
    exit 1
else
    echo -e "${GREEN}No direct TzKT URL literals found outside canonical modules.${NC}"
    exit 0
fi
