# Refactoring: Moving Code Between Files

Reference guide for safe code movement between modules. See also the root [AGENTS.md](../../AGENTS.md).

## Refactoring: Moving Code Between Files

When splitting large files or moving code between modules, **never use Read+Write** to copy code. AI agents can accidentally drop lines, subtly modify code, or hallucinate changes when passing large code blocks through their context window.

### CRITICAL RULE: Use Shell Commands for Code Movement

**DO use shell commands for extracting/moving code:**

```bash
# Extract lines 100-300 to a new file
sed -n '100,300p' src/large_file.ml > src/new_module.ml

# Extract from a pattern to another pattern
sed -n '/^let prompt_input/,/^let logging_mode_term/p' src/main.ml > src/cli_prompts.ml

# Split file at specific patterns
csplit src/main.ml '/^let install_node_cmd/' '/^let instance_term/'
```

**DO use Edit tool only for small surgical changes:**
- Adding license headers to new files
- Adding `open` or `include` statements
- Updating `dune` files
- Removing the moved section from the original file (after verifying the extraction)

**DO NOT use Read+Write to "copy" code:**
```
❌ Read src/main.ml → Write src/new_file.ml with "copied" content
```

### Verification Steps

After each code movement:

```bash
# 1. Verify line counts make sense
wc -l src/original.ml src/new_module.ml

# 2. Verify compilation
dune build

# 3. Run tests
dune runtest

# 4. Check formatting
dune fmt

# 5. Optionally verify exact content with checksums
sed -n '100,300p' src/original_backup.ml | md5sum
cat src/new_module.ml | tail -n +7 | md5sum  # skip header lines
```

### Refactoring Workflow

1. **Create a branch** for the refactoring work
2. **Identify extraction boundaries** - find exact line numbers or patterns
3. **Extract with `sed`** - guaranteed exact copy
4. **Add necessary scaffolding** with Edit:
   - License header
   - Module imports (`open`, `include`)
   - Interface file (`.mli`)
5. **Update `dune`** to include the new module
6. **Remove extracted code** from original with Edit
7. **Update original** to use the new module
8. **Verify** with `dune build && dune runtest && dune fmt`
9. **Commit** with clear message describing what was moved
