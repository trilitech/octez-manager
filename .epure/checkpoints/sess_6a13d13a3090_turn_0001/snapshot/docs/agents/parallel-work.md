# Parallel Work with Worktrees & Issue Tracking

Reference guide for concurrent development workflows. See also the root [AGENTS.md](../../AGENTS.md).

## Parallel Work with Worktrees

Multiple agents (or an agent and a human) can work on the repository simultaneously using **git worktrees**. Each worktree is an independent working directory sharing the same `.git` history.

### Setting Up a Worktree

```bash
# Create a worktree for a new branch (from the main repo directory)
git worktree add ../octez-manager-feat-xyz -b feat/xyz

# Or for an existing branch
git worktree add ../octez-manager-fix-123 fix/issue-123

# List active worktrees
git worktree list

# Remove a worktree when done
git worktree remove ../octez-manager-feat-xyz
```

### Worktree Rules

- **Each worktree must be on a different branch** — git enforces this
- **Build artifacts are per-worktree** — each has its own `_build/` directory
- **opam switch is shared** — no need to reinstall dependencies
- **Never delete a worktree directory manually** — always use `git worktree remove`

## Issue Tracking for Parallel Work

When multiple agents may work concurrently, proper issue tracking prevents conflicts and duplicated effort.

### Starting Work on an Issue

1. **Assign the issue to yourself** before starting:
   ```bash
   gh issue edit <NUMBER> --add-assignee @me
   ```
2. **Create a branch** (in a worktree if working in parallel):
   ```bash
   git worktree add ../octez-manager-issue-<NUMBER> -b feat/issue-<NUMBER>
   ```

### Ending a Session

If the issue is **fully resolved**: create the PR and let the PR reference close it (`fixes #NUMBER`).

If work is **incomplete** (session ending, context limit, etc.):
1. **Commit and push** all progress so far
2. **Add a comment to the issue** summarizing:
   - What was done
   - What remains to be done
   - Any blockers or decisions needed
   - The branch name with the in-progress work
3. **Unassign yourself** so another agent can pick it up:
   ```bash
   gh issue edit <NUMBER> --remove-assignee @me
   ```

### Example Issue Comment (Incomplete Work)

```markdown
### Progress update

**Branch:** `feat/issue-42`

**Done:**
- Implemented the new RPC endpoint parser
- Added unit tests for happy path

**Remaining:**
- Error handling for malformed JSON responses
- Integration test

**Notes:**
- The parser needs to handle both v1 and v2 response formats (see `src/rpc_client.ml:180`)
```
