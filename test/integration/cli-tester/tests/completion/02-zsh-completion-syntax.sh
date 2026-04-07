#!/bin/bash
# Test: Zsh completion script is syntactically valid and escapes full:50 correctly
# Verifies SC-007 (full:50 selectable), SC-009 (zsh -n passes), and
# that colon in zsh value position is escaped as \: (not left bare).
set -euo pipefail
source /tests/lib.sh

test_init "Zsh completion: syntax valid and full:50 colon escaped"

ZSH_SCRIPT="/tests/completion/fixtures/octez-manager.zsh"

# SC-009: zsh -n syntax check
if command -v zsh >/dev/null 2>&1; then
  zsh -n "$ZSH_SCRIPT"
  echo "zsh -n: syntax OK"
else
  echo "INFO: zsh not installed — skipping syntax check"
fi

content=$(cat "$ZSH_SCRIPT")

# SC-007: full\:50 must appear (escaped colon in value position)
assert_contains "$content" 'full\:50' \
  "zsh script must contain escaped 'full\:50' for --snapshot-kind"

# Regression guard: bare 'full:50:' (unescaped) must not appear
if echo "$content" | grep -qF "'full:50:"; then
  echo "ASSERT FAILED: zsh script contains bare 'full:50:' — colon not escaped in value position"
  exit 1
fi
echo "full:50 colon correctly escaped (no bare 'full:50:' found)"

# _describe must be present: verifies subcommand groups use _describe
assert_contains "$content" "_describe" \
  "zsh script should use _describe for subcommand groups"

# State handlers must be emitted: verifies that ->snapshot-kinds, ->history-modes,
# and ->lb-votes actions in option specs have corresponding dispatch arms.
assert_contains "$content" "snapshot-kinds)" \
  "zsh script must contain snapshot-kinds state handler"
assert_contains "$content" "history-modes)" \
  "zsh script must contain history-modes state handler"
assert_contains "$content" "lb-votes)" \
  "zsh script must contain lb-votes state handler"

echo "Test passed"
