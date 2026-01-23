# Test Coverage

This document describes the test coverage infrastructure for octez-manager.

## Overview

Test coverage is collected from two sources:
1. **Unit tests** - OCaml unit tests in `test/unit_tests.ml`
2. **Integration tests** - Docker-based CLI integration tests in `test/integration/`

Coverage data is aggregated and reported automatically on all pull requests.

## Current Coverage

![Coverage Status](https://img.shields.io/badge/coverage-52%25-yellow)

As of the latest main branch: **52.09%** overall coverage

### Coverage by Module

| Category | Coverage | Status |
|----------|----------|--------|
| Test suite | 92.70% | 🟢 Excellent |
| Service management | 85%+ | 🟢 Very good |
| Configuration | 70-85% | 🟢 Good |
| Binary management | 60-85% | 🟡 Acceptable |
| Installation | 30-70% | 🟡 Needs improvement |
| TUI components | 0-20% | 🔴 Low coverage |

## Running Coverage Locally

### Prerequisites

```bash
opam install bisect_ppx
```

### Unit Test Coverage

```bash
# Run unit tests with coverage
./scripts/run-unit-tests-coverage.sh

# Generate HTML report
./scripts/generate-coverage.sh

# View report
open coverage-report/index.html
```

### Integration Test Coverage

```bash
# Build instrumented binary
dune clean
dune build --instrument-with bisect_ppx --release
cp _build/default/src/main.exe test/integration/cli-tester/octez-manager

# Run integration tests with coverage
cd test/integration
docker compose -f docker-compose.yml -f docker-compose.coverage.yml up --build

# Extract coverage data
docker compose -f docker-compose.yml -f docker-compose.coverage.yml cp \
    cli-tester:/coverage/. ../../_coverage-integration/

# Clean up
docker compose -f docker-compose.yml -f docker-compose.coverage.yml down -v
```

### Combined Coverage Report

```bash
# After collecting both unit and integration coverage:
./scripts/generate-coverage.sh
```

The script will automatically find all `.coverage` files in `_coverage/` directory.

## CI Coverage Reporting

Coverage is automatically collected and reported on all pull requests via GitHub Actions.

### Workflow

1. **Unit Tests** - Run with instrumentation, collect coverage
2. **Build Instrumented Binary** - Build octez-manager with bisect_ppx
3. **Integration Tests** - Run Docker tests with instrumented binary
4. **Aggregate** - Combine all coverage data
5. **Report** - Post coverage summary as PR comment

### PR Coverage Comments

Each PR receives a comment with:
- Overall coverage percentage
- Change vs base branch (🟢 increase / 🔴 decrease / ⚪ no change)
- Per-file coverage breakdown
- Link to detailed HTML report

Example:
```
## 📊 Coverage Report

**Overall Coverage:** 52.09% 🟢 (+2.5%)

Coverage: 4555/8745 (52.09%)

### Coverage by File
...
```

## Coverage Goals

### Targets by Component

| Component | Current | Target | Priority |
|-----------|---------|--------|----------|
| Unit tests | 92.7% | 95% | Low (already excellent) |
| Core library | 52% | 70% | High |
| TUI components | 10-20% | 50% | Medium |
| CLI commands | 0-30% | 60% | High |
| Installation flows | 30-70% | 80% | High |

### Quality Gates

- **Required:** Coverage must not decrease on PRs (with 1% tolerance)
- **Recommended:** New features should have >70% coverage
- **Ideal:** Critical paths (installation, service management) should have >80% coverage

## Understanding Coverage Reports

### HTML Report

The HTML report (`coverage-report/index.html`) provides:
- File-by-file coverage breakdown
- Line-by-line highlighting (green = covered, red = not covered)
- Branch coverage information

### Per-File Coverage

Look for:
- **🟢 Green (>80%)**: Well tested, maintain this level
- **🟡 Yellow (50-80%)**: Acceptable, room for improvement
- **🔴 Red (<50%)**: Low coverage, needs attention

### What's Counted

- **Lines executed**: Physical lines of code that ran during tests
- **Branches**: Conditional paths (if/then/else, match cases)
- **Not counted**: Comments, blank lines, type definitions

## Improving Coverage

### Adding Unit Tests

Edit `test/unit_tests.ml`:

```ocaml
let my_new_test () =
  let result = MyModule.my_function input in
  Alcotest.(check string) "expected output" "expected" result

let () =
  Alcotest.run "octez-manager" [
    ( "my_module",
      [
        Alcotest.test_case "my_function" `Quick my_new_test;
      ] );
  ]
```

### Adding Integration Tests

Create new test in `test/integration/cli-tester/tests/<category>/`:

```bash
#!/bin/bash
set -euo pipefail
source /tests/lib.sh

echo "Test: My new feature"

INSTANCE="test-my-feature"
cleanup_instance "$INSTANCE" || true

om install-node \
    --instance "$INSTANCE" \
    --network shadownet \
    --no-enable

if ! instance_exists "$INSTANCE"; then
    echo "ERROR: Installation failed"
    exit 1
fi

cleanup_instance "$INSTANCE"
echo "Test passed"
```

### Focus Areas

To increase overall coverage, prioritize:

1. **Installation flows** (`src/installer/*.ml`)
   - Currently 0-30% coverage
   - High impact on users
   - Relatively straightforward to test

2. **CLI commands** (`src/cli/*.ml`)
   - Currently low coverage
   - Integration tests can cover these well

3. **Error paths**
   - Add tests for failure scenarios
   - Network errors, permission issues, etc.

## Troubleshooting

### No coverage files generated

Check:
1. `bisect_ppx` is installed: `opam list bisect_ppx`
2. Tests actually run: `dune runtest --force`
3. Coverage files in build dir: `find _build -name "*.coverage"`

### Low coverage on new code

Ensure:
1. Code is in an instrumented library (not just executable)
2. Code is actually executed by tests
3. Test assertions actually test the code path

### Integration test coverage not collected

Verify:
1. Binary built with: `dune build --instrument-with bisect_ppx`
2. `BISECT_FILE` environment variable set in container
3. Coverage volume mounted in docker-compose
4. Coverage files extracted after tests

## References

- [Bisect_ppx Documentation](https://github.com/aantron/bisect_ppx)
- [Integration Test Guide](../test/integration/README.md)
- [CI Workflow](.github/workflows/coverage.yml)
