# Review-tool bundle — consumer notes

This directory carries a small set of files distributed from the
[roster](https://github.com/mathiasbourgoin/roster) repo: the deterministic tools
`roster-review`/`roster-qa` depend on, plus their shared libraries and schema.

**These files are upstream-owned and generated.** `scripts/review-bundle.manifest.json` is the
sole sentinel — it lists every file in the bundle with its expected sha256. **Do not hand-edit
any bundle file or the manifest.** A local edit will be detected as "modified" on the next
verify/upgrade/remove and handled conservatively (skipped with a warning, or refused outright) —
see the recovery guidance those commands print if that happens.

## Commands

Run verification from the consumer repo root. Install, upgrade, and removal remain owned by the
external `review-bundle-install.sh` bootstrapper; that lifecycle script is intentionally not
installed into the consumer bundle.

```bash
# Verify the installed bundle is complete and unmodified (no network calls).
node scripts/review-bundle-verify.js

# Fetch the lifecycle installer from a trusted roster raw URL.
# Replace OWNER, REPOSITORY, and REF with the trusted source coordinates.
RAW_PREFIX='https://raw.githubusercontent.com/OWNER/REPOSITORY/REF'
INSTALLER=$(mktemp)
trap 'rm -f "$INSTALLER"' EXIT
curl -fsSL "$RAW_PREFIX/scripts/review-bundle-install.sh" -o "$INSTALLER"

# Install (first time) or upgrade (already installed).
bash "$INSTALLER" install --from-raw "$RAW_PREFIX"
bash "$INSTALLER" upgrade --from-raw "$RAW_PREFIX"

# Remove the bundle (the shared wrapper, its content-snapshot helper, and
# scripts/package.json are retained for consumer tools).
bash "$INSTALLER" remove
```

Full details, including collision handling and `--force`: see the header comment of
`scripts/review-bundle-install.sh`, and specs/review-tool-distribution.md in the roster repo.
# CommonJS execution in ESM consumers

The installed bundle owns `scripts/package.json` with `type: commonjs`, so its
`.js` tools execute even when the consumer root uses `type: module`. Existing
module-scope files retain ordinary installer collision protection. When adding
this scope, installation refuses unrelated existing `.js` scripts, including ESM
inherited from an ancestor package or detected from syntax. This guard also applies
with `--force`. Configure a compatible `scripts/package.json` or isolate those
scripts before retrying. Existing explicit scopes retain ordinary collision
checks. The app root package is never changed.

Removal keeps `scripts/package.json` even when the bundle originally created it.
This preserves CommonJS semantics for both preexisting consumer scripts and
scripts added after installation. The retained scope is shared configuration;
its collision protection and SHA verification still apply while installed.

The same bundle includes the QA convergence gate, its local schema and dependency
closure, plus the review/QA state contract documents used by installed skills.
Run `node scripts/check-qa-convergence.js <qa-state.json>` in the consumer; no
source checkout is required for that gate.

## Repository content guard

Cross-runtime and green-check runs compare the Git index and Git-visible working
files before and after execution, including contents of paths already dirty.
Ignored untracked files, nested submodule contents beyond superproject-visible
state, and writes restored before the after snapshot are outside this check.
This detects persistent repository changes; it does not provide OS confinement.
Snapshot failures in a Git repository reject the result.

The cross-runtime shell wrapper now requires Node and its adjacent
`scripts/lib/git-tree-snapshot.js` helper. Copying only `xruntime-exec.sh` is no
longer sufficient for Git-repository runs. The bundle installs that helper and
the CommonJS `scripts/package.json` scope. Removal retains all three together so
the shared wrapper remains usable. Snapshotting streams file bytes and its cost
scales with the total size of tracked and nonignored untracked files.
