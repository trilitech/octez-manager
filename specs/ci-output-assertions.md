# Integration command-output assertions

Profile: minimal-freeze.

- AC-1: a successful command that emits a matching value followed by substantial output is observed once, fully consumed, and accepted without SIGPIPE.
- AC-2: matching output from an unexpected nonzero command is rejected; a missing expected value is rejected; literal matching does not reinterpret instance-name metacharacters.
- AC-3: the shared assertions preserve the command's status and both diagnostic streams in the failure report. Strict success-row matching checks stdout; stderr-only matching text cannot satisfy it. Existing cleanup/predicate semantics must not silently mask a newly introduced error.
- AC-4: the selected ShellCheck2337 gate analyzes a nonempty integration-shell inventory under pipefail context, including the shared library. An unsafe fixture fails1; unavailable, incorrect or incapable checker and empty inventory refuse with exit>=2. The capability probe distinguishes installed stable0.11 from the qualified checker.
- AC-5: the actual Signatory dependency test and complete shard2 pass on the patched source and source-bound binary. Earlier CI failures remain in evidence; rerunning until green is not the repair.

The scanner reads unfiltered analyzer JSON and refuses error-level/parse diagnostics (including sourced-file parse warning SC1094) before selecting2337; a filtered-only run is not proof of valid input. Inline suppression of2337/all, including numeric ranges covering2337, is refused. Adjacent and unrelated ranges remain allowed and cannot hide an unsafe finding. Inventory is all regular integration `.sh` files, with explicit pipefail context per source; selected symlinks and symlink-directory traversal refuse, including the supplied root, test ancestor, integration directory and descendants. This is a bounded static check, not a complete dynamic shell dependency or program-safety proof.

## Runnable checks

- CHECK-1 [AC-1] (authentic-success-path): `bash test/integration/cli-tester/selftest-command-output.sh <lib.sh>`. The identical driver against a disposable baseline library must fail1 through an existing helper's early-closing pipeline, not a missing new function. Candidate and late-output completion control pass0.
- CHECK-2 [AC-2, AC-3] (fail-closed-path): the same driver tests missing literal text, stderr-only text, unexpected exit7 with matching text, explicitly expected nonzero, diagnostic preservation and conditional-call failure propagation. Missing fixture/tools refuse>=2.
- CHECK-3 [AC-4] (fail-closed-path): `SHELLCHECK_BINARY=<qualified-checker> python3 test/test_integration_pipefail.py` exercises the adapter `scripts/check-integration-pipefail.py` with the qualified checker. Unsafe finding1; safe inventory0; malformed/suppressed/empty/incorrect-tool cases refuse, never pass. The production adapter scans the complete selected integration inventory.
- CHECK-4 [AC-5] (authentic-success-path): real `signatory/02-baker-dependency.sh` followed by complete parallel shard2 with a receipt for source and executed binary. Remote exact-head CI is required before merge. This checks the documented dependency scenario, not signing or daemon stability.

Implementation records actual commands, full identities and red/green receipts before review. Actual source/binary identities, execution outcomes and limitations are recorded in the implementation report; remote CI remains a shipping gate.


## Qualified gate execution

The command-result API is `assert_command_output EXPECTED_STATUS LITERAL LABEL -- COMMAND [ARG...]` in the existing shared test library. It invokes argv once, captures stdout to completion, retains stderr separately for failed assertions, checks the explicit status, then reuses `assert_contains` for literal stdout. On success it returns captured stdout for further negative/regex checks. Missing tools, invalid invocation/status and capture setup failures refuse with status 2. Assertion failures return 1 explicitly even under conditional calls. Legacy registry fallback and retry predicates remain separate from this strict API. Signatory02 requires request acceptance status 0 and absence of the captured not-found diagnostic; this is not a signing/stable-daemon proof.

The independent workflow `.github/workflows/integration-output-assertions.yml` extracts `/bin/shellcheck` from the immutable image in `scripts/integration-shellcheck.lock.json`, verifies its exact binary SHA256, then runs:

```bash
bash test/integration/cli-tester/selftest-command-output.sh test/integration/cli-tester/tests/lib.sh
bash test/integration/cli-tester/selftest-shard-registration.sh
SHELLCHECK_BINARY=<qualified-checker> python3 test/test_integration_pipefail.py
python3 scripts/check-integration-pipefail.py --shellcheck <qualified-checker>
```

No host-stable fallback or moving Docker tag is used. Adapter tests use explicit fixture locks only to reach independent post-hash refusal paths; production uses the pinned default lock. A separate installed stable0.11 checker is used only as an optional negative control when available, never as a production fallback. Discovery recursively includes new regular `.sh` sources in the selected directory and refuses symlink traversal; it does not claim complete dynamic/non-`.sh` source closure. Every file is sourced from a fixed Bash/pipefail analysis driver with `--check-sourced --external-sources --norc`; option environment overrides are cleared. Capability probes run on every invocation, and error-level or sourced-parse failures refuse before filtering SC2337.

For authentic CHECK-1 RED, copy baseline `test/integration/cli-tester/tests/lib.sh` with `git show 739d49d4:test/integration/cli-tester/tests/lib.sh` into an owned temporary file, then pass that file to the same selftest driver. Its first assertion exercises unchanged-name `external_service_detected`; the old early reader yields assertion1 rather than a missing-new-API setup failure. Current source must yield0, and missing library/tools must refuse2. The historical CI producer status was not logged; the controlled producer/reader experiment establishes the mechanism without proving that exact historical run's status.

Review-boundary ratchet: `python3 test/check_integration_pipefail_boundaries.py --adapter scripts/check-integration-pipefail.py --shellcheck <qualified-checker>`. It returns0 for the candidate,1 for a reproduced policy assertion, and2 for missing/unusable setup. `--case ranges` and `--case ancestors` independently demonstrate both old failures against the reviewed adapter; noncovering ranges and restored ordinary directories are positive controls. The existing adapter suite invokes this standalone check too.


Only actual ShellCheck annotations are suppression policy inputs; quoted strings,
escaped hashes, and heredoc bodies remain data. The adapter uses the pinned
ShellCheck parser to classify each textual candidate: an analysis-only copy gets
an unknown directive key, and its positioned SC1107 diagnostic identifies an
actual annotation. No Bash lexer or parser dependency is added, and source files
are never rewritten. Both the annotation marker and its quoted-literal negative
control are qualified on every invocation. Full, unfiltered analysis of the
original source runs first; parse/setup failures refuse2. Probe-only parse errors
(such as an instrumented heredoc delimiter) cannot prove an annotation and do
not override that original analysis. Tool/JSON/diagnostic failures still refuse.

`python3 test/check_integration_pipefail_boundaries.py --adapter scripts/check-integration-pipefail.py --shellcheck <qualified-checker> --case comments`
checks literal data (single/double/ANSI/locale quotes, escaped hashes, multiline
quotes, quoted/unquoted/tab-indented heredocs and command substitutions), and
retains refusal2 for actual directives, covering ranges/all and nested command
substitutions. The identical driver against the6f adapter returns assertion1 for
the known false refusals after successful tool preflight; missing tools or adapter
still return2. Both original false-refusal examples also have dedicated unit tests.
