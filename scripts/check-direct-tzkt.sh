#!/bin/bash
# Fail if any .ml/.mli file outside lib/common/indexer.ml contains a
# string literal with "api.tzkt.io" or "api.<network>.tzkt.io".
# This ensures all TzKT traffic is routed through the Indexer module.
set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

CANONICAL_ML="lib/common/indexer.ml"
CANONICAL_MLI="lib/common/indexer.mli"

echo "Checking for direct TzKT URL literals outside ${CANONICAL_ML}..."
echo ""

found_issues=0

# Search for quoted string literals containing api.tzkt.io or api.<word>.tzkt.io
# We match the pattern inside double quotes to avoid flagging plain doc comments.
pattern='"[^"]*api(\.[a-z]+)?\.tzkt\.io[^"]*"'

while IFS= read -r file; do
    # Skip the canonical indexer files (both .ml and .mli)
    if [ "$file" = "$CANONICAL_ML" ] || [ "$file" = "$CANONICAL_MLI" ]; then
        continue
    fi

    matches=$(grep -nP "$pattern" "$file" 2>/dev/null || true)
    if [ -n "$matches" ]; then
        echo -e "${RED}ERROR${NC}: $file contains direct TzKT URL literal(s):"
        while IFS= read -r match; do
            echo "  $match"
        done <<< "$matches"
        echo "  Use Indexer.fetch or Indexer.tzkt_base_url instead."
        echo ""
        found_issues=$((found_issues + 1))
    fi
done < <(find src lib -name '*.ml' -o -name '*.mli' | grep -v '_build' | sort)

if [ $found_issues -gt 0 ]; then
    echo -e "${RED}Found $found_issues file(s) with direct TzKT URL literals.${NC}"
    echo "All TzKT URLs must go through lib/common/indexer.ml."
    exit 1
else
    echo -e "${GREEN}No direct TzKT URL literals found outside ${CANONICAL_ML}.${NC}"
    exit 0
fi
