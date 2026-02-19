# LSP & Tooling Setup

Reference guide for setting up OCaml LSP and editor tooling. See also the root [AGENTS.md](../../AGENTS.md).

## OCaml LSP Server

AI coding agents can use the OCaml LSP server (`ocamllsp`) for code intelligence features like go-to-definition, find-references, hover documentation, and workspace symbol search.

### Setup by Tool

#### OpenCode

OpenCode has **built-in OCaml LSP support**. The project includes an `opencode.json` config that routes through `opam exec` to find `ocamllsp` in the project's local opam switch:

```json
{
  "$schema": "https://opencode.ai/config.json",
  "lsp": {
    "ocaml-lsp": {
      "command": ["opam", "exec", "--", "ocamllsp"]
    }
  }
}
```

This file is committed to the repo -- no manual setup needed. Just start OpenCode in the project directory. The LSP server starts automatically when `.ml`/`.mli` files are opened.

**Note:** If you add or change the `opencode.json` config, you must restart the OpenCode session for changes to take effect.

#### Claude Code

The OCaml LSP plugin is available via the [claude-code-lsps](https://github.com/Piebald-AI/claude-code-lsps) marketplace:

```bash
# Add the marketplace (one-time)
claude
/plugin marketplace add Piebald-AI/claude-code-lsps

# Install the OCaml LSP plugin
/plugins  # Navigate to Marketplaces > claude-code-lsps > Browse plugins
# Select ocaml-lsp with spacebar, press "i" to install
# Restart Claude Code
```

#### Other Tools

Any tool that supports LSP can use `ocamllsp`. Ensure the binary is reachable:

```bash
# The binary lives in the project's local opam switch
opam exec -- which ocamllsp
# → /home/<user>/dev/octez-manager/_opam/bin/ocamllsp

# If your tool doesn't go through opam exec, add _opam/bin to PATH:
eval $(opam env)
```

### Building the Index for Project-Wide References

By default, `findReferences` only searches the current file. To enable **project-wide** find references, you must build the ocaml-index:

```bash
opam exec -- dune build @ocaml-index
```

This creates an index in `_build/default/.ocaml-index` that the LSP uses for cross-file reference lookups.

### Keeping the Index Up to Date

**The index must be rebuilt when code changes.** Options:

1. **Manual rebuild** after significant changes:
   ```bash
   opam exec -- dune build @ocaml-index
   ```

2. **Continuous rebuild** during development:
   ```bash
   opam exec -- dune build @ocaml-index --watch
   ```

Note: Unlike `dune build @check`, the `@ocaml-index` target builds the entire project including tests.

### Available LSP Operations

| Operation | Status | Description |
|-----------|--------|-------------|
| `hover` | ✅ | Type signature and documentation |
| `goToDefinition` | ✅ | Jump to symbol definition |
| `findReferences` | ✅ | Find all usages (requires index for cross-file) |
| `documentSymbol` | ✅ | List symbols in current file |
| `workspaceSymbol` | ✅ | Search symbols across project |
| `goToImplementation` | ❌ | Not supported by ocaml-lsp |
| `incomingCalls` | ❌ | Not supported by ocaml-lsp |
| `outgoingCalls` | ❌ | Not supported by ocaml-lsp |

### Requirements

Project-wide references require:
- OCaml 5.2+ (we use 5.3.0)
- Dune 3.16+ (we use 3.20.2)
- ocaml-lsp-server 1.18+ (we use 1.23.1)
- Merlin 5.1-502+ (we use 5.6-504)

All requirements are satisfied by the project's opam switch.

## OpenCode Configuration

The project includes an `opencode.json` config that is committed to the repo. It provides:

### Auto-Formatting

OCaml files (`.ml`, `.mli`) are automatically formatted via `ocamlformat` when written or edited. The formatter runs through `opam exec` to use the project's local switch.

### Custom Commands

The following commands are available in the OpenCode TUI (type `/` to see them):

| Command | Description |
|---------|-------------|
| `/build` | Run `dune build` and fix compilation errors |
| `/test` | Run `dune runtest` and fix test failures |
| `/fmt` | Format code with `dune fmt` |
| `/copyright` | Fix and verify copyright headers |
| `/pre-commit` | Full pre-commit sequence (fmt + copyright + build + test) |
| `/index` | Rebuild OCaml LSP index for project-wide references |
| `/archdb <query>` | Query the architecture database |

### Pre-Allowed Commands

To reduce permission prompts, the following are auto-allowed:

- `opam exec -- dune *` and `opam exec -- ocaml*` (build/test/format)
- Read-only git commands (`status`, `diff`, `log`, `branch`, `show`, `fetch`)
- `gh pr` and `gh issue` (GitHub CLI)
- `sqlite3 docs/architecture.db` (architecture queries)
- `make *` (Makefile targets)

Destructive operations (`gh api`, `git push`, `git rebase`, etc.) still require confirmation.

### Instructions

AGENTS.md is loaded as an instruction file, so its contents are available to the agent as context.

