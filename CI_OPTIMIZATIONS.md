# CI Optimization Journey

## Timeline

### PR #382: Time-Based Test Sharding
**Achievement**: 8 minutes → 5m 13s (35% faster)

- Implemented time-based sharding with 5 balanced shards
- Created `shards.json` manifest with actual test timings
- Added automatic shard assignment based on historical data
- Reduced integration test time from 8m → 2m 41s

### PR #383: Parallel Build and Docker Layers
**Achievement**: 5m 13s → 4m 50s (7% faster)

- Split build-and-test into separate parallel jobs
- Integration tests no longer wait for unit tests
- Added `-j 4` parallel compilation flags
- Split Docker images into base + runtime layers
- Base image contains Octez binaries and Zcash params (cached weekly)

### Current: Sub-3-Minute Total Time
**Target**: 4m 50s → < 3m (40% faster from previous, 63% faster from original 8m)

Optimizations implemented:

1. **Share _build artifacts** (saves ~60-90s)
   - Build job uploads _build directory
   - Unit-tests downloads and reuses it
   - Eliminates redundant compilation

2. **Pre-install Miaou in CI image** (saves ~20-40s)
   - Miaou packages baked into CI Docker image
   - Uses public trilitech/miaou repo
   - Only updates when miaou-version changes

3. **Aggressive Docker caching** (saves ~10-20s)
   - Pull base image before build (shared across shards)
   - Enable BUILDKIT_INLINE_CACHE
   - Authenticate with GHCR for image pulls

## Expected Final Performance

```
Build:         2m 30s → 1m 30s  (Miaou pre-installed)
Unit Tests:       6s →   10s  (_build artifact download)
Integration:   2m 20s → 1m 45s  (better caching)
─────────────────────────────────────────────────
Total:         ~5m    → ~2m 50s  (43% improvement)
```

**Overall improvement from original**: 8m → 2m 50s = **65% faster**

## Future Optimization Ideas (Beyond 3 minutes)

If we need to go even faster:

### High Impact
- **Self-hosted runners** (could reach 2m total with persistent caches)
- **Distributed test execution** (run all tests simultaneously, ~40s total)
- **Build caching service** (store builds by commit hash, skip rebuilds)

### Medium Impact
- **Conditional job execution** (skip tests for docs-only changes)
- **Merge queue** (require fewer checks, run full suite post-merge)
- **Incremental dune builds** (digest-based caching across PRs)

### Low Impact
- **Parallel dune jobs** (increase from -j 4 to -j 8)
- **Pre-fetch opam packages** (include more in CI image)
- **Optimized test infrastructure** (faster sandbox startup)

## Key Learnings

1. **Time-based sharding** was the biggest single win (8m → 5m)
2. **Artifact sharing** eliminates redundant work between jobs
3. **Layer caching** in Docker is critical for multi-shard tests
4. **Pre-installing dependencies** in images pays off quickly
5. **Parallel execution** should be default at every level

## Maintenance

- CI Docker image auto-rebuilds weekly (Sundays 3 AM UTC)
- Test base image auto-rebuilds weekly (Sundays 3 AM UTC)
- Bump `.github/miaou-version` to force Miaou update
- Bump `Dockerfile.ci` trigger line to force CI image rebuild
- Regenerate shards.json when adding/removing tests

