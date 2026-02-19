# CI & GitHub Workflow Guide

Guidelines for CI configuration and GitHub interactions. For general project rules, see the root [AGENTS.md](../AGENTS.md).

### Verification Check Pattern

When adding new verification checks (like format-check, completions-check), follow this **non-destructive pattern**:

```makefile
# CORRECT: Non-destructive check (matches CI)
check-something:
	@mkdir -p /tmp/octez-something-check
	@generate-something --out-dir /tmp/octez-something-check
	@if ! diff -q expected/file /tmp/octez-something-check/file >/dev/null 2>&1; then \
		echo "ERROR: Something is out of date."; \
		echo "Run: make generate-something"; \
		diff -u expected/file /tmp/octez-something-check/file || true; \
		rm -rf /tmp/octez-something-check; \
		exit 1; \
	fi
	@rm -rf /tmp/octez-something-check
	@echo "Something is up to date."
```

**Why this pattern?**
- ✅ **Non-destructive**: Doesn't modify the working directory
- ✅ **Consistent with CI**: Same logic runs locally and in CI
- ✅ **Helpful**: Shows actual diff when check fails
- ✅ **Clean**: Cleans up temp directory after check

**Anti-pattern (DO NOT use):**
```makefile
# WRONG: Destructive check
check-something: generate-something
	@git diff --exit-code something/ || exit 1
```

This modifies the working directory, leaving uncommitted changes if the check fails.

## Interacting with GitHub Copilot Reviews

When Copilot reviews a PR, follow these rules to avoid noise:

### DO NOT reply individually to each Copilot comment

Replying to individual Copilot review comments with `@copilot` triggers it to create a **separate PR for each reply**. This creates significant noise (we observed 11 spurious PRs from 10 individual replies).

### DO use a single PR-level comment

After fixing all Copilot feedback, post **one PR-level comment** summarizing all changes, then re-request review:

```bash
# Post a single summary comment
gh pr comment <NUMBER> --body "## Copilot feedback addressed

1. **file.ml:42** — Fixed X
2. **file.ml:99** — Fixed Y
...

@copilot please re-review this PR."

# Re-request copilot as reviewer
gh pr edit <NUMBER> --add-reviewer "copilot-pull-request-reviewer[bot]"
```

### Resolving Copilot threads

Copilot does **not** resolve its own threads, even after re-review. Resolve them via the GraphQL API:

```bash
# Get thread IDs
gh api graphql -f query='{
  repository(owner: "trilitech", name: "octez-manager") {
    pullRequest(number: <NUMBER>) {
      reviewThreads(first: 50) {
        nodes { id isResolved }
      }
    }
  }
}' --jq '.data.repository.pullRequest.reviewThreads.nodes[] | select(.isResolved == false) | .id'

# Resolve a thread
gh api graphql -f query='mutation { resolveReviewThread(input: {threadId: "<THREAD_ID>"}) { thread { isResolved } } }'
```

### Closing spurious Copilot PRs

If Copilot creates unwanted sub-PRs, close them and delete their branches:

```bash
gh pr close <NUMBER> --comment "Closing: auto-created by copilot. Feedback addressed in #<ORIGINAL_PR>." --delete-branch
```
