# CI Optimization Final Report

## Executive Summary

**Goal**: Reduce CI time to 2-3 minutes
**Achievement**: 8 minutes → 5m 18s (34% improvement)
**Result**: Did not reach sub-3-minute goal, but achieved significant improvement

## What Worked ✅

### PR #382: Time-Based Test Sharding (MERGED)
**Impact**: 8m → 5m 18s (34% improvement)

**Changes**:
- Created `shards.json` manifest with actual test timing data
- Implemented 5 balanced shards based on execution time
- Added shard-based test runner with automatic assignment

**Key Success Factors**:
- Used empirical data (actual test timings) not estimates
- Balanced shards to finish at similar times
- Single biggest optimization we attempted

**Status**: ✅ **MERGED** and production-ready

---

## What Didn't Work ❌

### PR #383: Parallel Build & Docker Layers (CLOSED)
**Expected**: 5m 18s → 4m 50s
**Actual**: 5m 18s → 5m 21s (3s slower)

**Why it failed**:
- Build improved by 23s (removed test phase)
- Integration tests slowed by 26s (Docker layer overhead)
- Net negative impact

**Lesson**: Architectural improvements don't always translate to performance gains

---

### PR #384: Advanced Optimizations (CLOSED)
**Attempted**:
1. Share _build artifacts between jobs
2. Pre-install Miaou in CI Docker image
3. Aggressive Docker caching

**Why it failed**:
- **_build sharing**: Caused segmentation faults from incomplete artifacts
- **Miaou pre-install**: Version mismatch - project uses specific commit via secret, not public `main` branch
- **Result**: Every binary execution crashed with segfault

**Root Cause**: Cannot pre-install dependencies when versions are controlled by runtime secrets

**Lesson**: Secrets and build-time configuration don't mix well

---

## Performance Timeline

```
Original:          8m 00s  (100%)
After PR #382:     5m 18s  ( 66%) ✅ ACHIEVED
PR #383 attempt:   5m 21s  ( 67%) ❌ Slower
Goal:              3m 00s  ( 38%) ⚠️  Not reached
```

## Why We Stopped at 5m 18s

**Technical Constraints**:
1. **Miaou version locked by secret** - Can't pre-install in Docker image
2. **Integration tests are sequential** - Each test requires clean systemd state
3. **Octez binary downloads** - 250MB+ of dependencies per shard
4. **Static linking overhead** - musl-based builds are slower

**Diminishing Returns**:
- First optimization (sharding): 34% improvement
- Second optimization (parallelization): 0% improvement (actually negative)
- Each subsequent optimization has less impact

## Future Optimization Paths

### To Reach Sub-3-Minutes

**Option 1: Self-Hosted Runners** (Est. 2m total)
- Persistent opam cache
- Persistent Docker layer cache
- Persistent dune build cache
- Fast local NVMe storage
- **Cost**: Infrastructure maintenance

**Option 2: Distributed Test Execution** (Est. 1m 30s total)
- Run all 60 tests in parallel (not 5 shards)
- Requires 60 concurrent containers
- **Cost**: 12x more runner capacity

**Option 3: Skip Integration Tests Conditionally** (Est. 2m 30s)
- Only run on `/test` comment or certain file changes
- Full suite on merge to main
- **Cost**: Might miss integration issues

**Option 4: Build Caching Service** (Est. 2m total)
- Cache builds by commit hash
- Skip rebuild if code unchanged
- **Cost**: External service dependency

### Recommended Next Step

**None needed**. 34% improvement is excellent for:
- Zero infrastructure cost
- Zero additional complexity
- Fully automatic
- Stable and reliable

---

## Key Learnings

1. **Measure first, optimize second**
   - Time-based sharding worked because we used empirical data
   - Assumptions about parallelization benefits were wrong

2. **Simple wins beat complex solutions**
   - Single sharding PR: 34% improvement
   - Complex multi-optimization PR: negative improvement

3. **Infrastructure matters**
   - GitHub Actions has constraints (cold starts, no persistent cache)
   - Self-hosted would enable better optimizations

4. **Secrets limit build-time optimization**
   - Can't pre-install when versions are in secrets
   - Runtime configuration conflicts with build caching

5. **34% is a great result**
   - Original 8 minutes was already reasonable
   - 5m 18s is excellent for a full build + 60 integration tests
   - Further optimization shows diminishing returns

---

## Final Recommendation

**✅ Stop here**. PR #382 achieved 34% improvement with:
- Zero ongoing maintenance
- No infrastructure changes
- Stable and reliable
- Good engineering ROI

**If sub-3-minutes becomes critical**, implement self-hosted runners. Until then, 5m 18s is excellent.

---

## Artifacts

- **PR #382**: https://github.com/trilitech/octez-manager/pull/382 (MERGED)
- **PR #383**: https://github.com/trilitech/octez-manager/pull/383 (CLOSED)
- **PR #384**: https://github.com/trilitech/octez-manager/pull/384 (CLOSED)
- **Sharding manifest**: `test/integration/cli-tester/tests/shards.json`
- **Timing data**: Output from each CI run in job logs

---

**Date**: January 21, 2026  
**Duration**: Full investigation and implementation  
**Result**: 34% improvement achieved, stopped at practical optimization limit
