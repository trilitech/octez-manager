# Integration Test Guide

Guidelines for writing integration tests in `test/integration/`. For general project rules, see the root [AGENTS.md](../../AGENTS.md).

## Integration Tests

Integration tests live in `test/integration/cli-tester/tests/` and are run in parallel during CI using time-based sharding.

### Test Independence (CRITICAL)

**Every integration test MUST be completely independent and self-contained.**

Tests are distributed across parallel shards in CI, which means:
- Tests may run in **any order**
- Tests run **simultaneously** in different containers
- Tests **cannot depend** on other tests running first

### Writing Independent Tests

**Required pattern for all integration tests:**

```bash
#!/bin/bash
set -euo pipefail
source /tests/lib.sh

echo "Test: Description"

# 1. Use a unique instance name for this test
TEST_INSTANCE="test-unique-name"

# 2. Cleanup any previous state
cleanup_instance "$TEST_INSTANCE" || true

# 3. Create your own test instances
om install-node \
    --instance "$TEST_INSTANCE" \
    --network shadownet \
    --snapshot \
    --snapshot-no-check \
    --snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
    --rpc-addr "127.0.0.1:UNIQUE_PORT" \
    --service-user tezos \
    --no-enable 2>&1 || true

# 4. Run your test assertions
# ...

# 5. Cleanup at the end
cleanup_instance "$TEST_INSTANCE" || true

echo "Test passed"
```

### DO NOT Do This

```bash
# ❌ BAD: Assumes another test created an instance
if ! instance_exists "$TEST_INSTANCE"; then
    echo "ERROR: Run test 01-install first"
    exit 1
fi

# ❌ BAD: Uses hardcoded instance name shared with other tests
TEST_INSTANCE="test-node"  # Conflicts with other tests!

# ❌ BAD: No cleanup - leaves instances for other tests
om install-node --instance "$TEST_INSTANCE"
# ... test code ...
# exit (no cleanup!)
```

### Port Allocation

When tests need RPC endpoints, use unique ports:
- Test 01: `127.0.0.1:18731`
- Test 02: `127.0.0.1:18732`
- Test 03: `127.0.0.1:18733`
- etc.

Avoid the default `127.0.0.1:8732` which may conflict with other tests running in parallel.

### Verifying Test Independence

Before committing a new test, verify it can run standalone:

```bash
# Run just your test
cd test/integration/cli-tester
./run-tests.sh node/XX-your-test.sh

# Run it multiple times
for i in {1..3}; do ./run-tests.sh node/XX-your-test.sh; done

# Run it alongside other tests (simulates parallel execution)
./run-tests.sh node/01-install.sh & \
./run-tests.sh node/XX-your-test.sh & \
wait
```

If any run fails, the test has dependencies or conflicts.

## Shard Registration

**All integration tests MUST be registered in `test/integration/cli-tester/tests/shards.json`.**

Tests not registered in the shard manifest will **not run in CI**, creating silent test coverage gaps.

### Checking Registration

Before committing a new test, verify it's registered:

```bash
cd test/integration/cli-tester
./selftest-shard-registration.sh
```

This selftest runs automatically in CI and will fail if any tests are unregistered.

### Registering a New Test

When you add a new test file (e.g., `node/26-my-new-test.sh`):

1. **Run the test locally** to measure its duration:
   ```bash
   ./run-tests.sh node/26-my-new-test.sh
   ```
   Note the duration printed at the end (e.g., "12.3s").

2. **Add it to a shard** in `tests/shards.json`:
   - Choose the shard with the lowest `total_duration` to keep shards balanced
   - Add your test path to that shard's `tests` array
   - Update the shard's `test_count` (increment by 1)
   - Update the shard's `total_duration` (add your test's duration)
   - Update the top-level `metadata.total_tests` (increment by 1)

3. **Verify registration**:
   ```bash
   ./selftest-shard-registration.sh
   ```
   It should now pass with no unregistered tests.

### Example: Adding a Test to a Shard

```json
{
  "shard-3": {
    "tests": [
      "node/01-install.sh",
      "node/02-show.sh",
      "node/26-my-new-test.sh"  // Add your test here
    ],
    "test_count": 15,              // Increment
    "total_duration": 132.3        // Add your test's duration
  }
}
```

### Why Shard Registration Matters

- Tests run in parallel across 5 shards in CI for speed
- Each shard runs a subset of tests independently
- Unregistered tests are silently skipped, leading to:
  - Broken features reaching production
  - False confidence in test coverage
  - Difficult-to-diagnose regressions

The selftest prevents this by failing CI if any test is unregistered.

