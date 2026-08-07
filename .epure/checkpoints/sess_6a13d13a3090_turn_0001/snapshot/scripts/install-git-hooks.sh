#!/bin/bash
# Install git hooks for octez-manager development

HOOKS_DIR=".git/hooks"
SCRIPTS_DIR="scripts"

# Ensure we're in the repo root
if [ ! -d ".git" ]; then
    echo "Error: Must run from repository root"
    exit 1
fi

echo "Installing git hooks..."

# Install pre-commit hook
if [ -f "$SCRIPTS_DIR/pre-commit" ]; then
    cp "$SCRIPTS_DIR/pre-commit" "$HOOKS_DIR/pre-commit"
    chmod +x "$HOOKS_DIR/pre-commit"
    echo "✅ Installed pre-commit hook"
else
    echo "❌ pre-commit hook not found in $SCRIPTS_DIR"
    exit 1
fi

echo ""
echo "Git hooks installed successfully!"
echo ""
echo "The pre-commit hook will:"
echo "  - Format code (dune fmt)"
echo "  - Check/fix copyright headers"
echo "  - Build the project"
echo "  - Run quick unit tests"
echo ""
echo "To bypass hooks temporarily, use: git commit --no-verify"
