# Building Static Binaries Locally

This guide explains how to reproduce the exact static binary build process used in CI releases.

## Quick Start

```bash
# Basic build (recommended)
./scripts/build-static-binary.sh

# Custom output name
./scripts/build-static-binary.sh --output octez-manager-v0.2.0-linux-x86_64

# Verbose build to see all steps
./scripts/build-static-binary.sh --verbose

# Force local Docker image build
./scripts/build-static-binary.sh --use-local-image
```

## Prerequisites

- **Docker**: Must be installed and running
- **MIAOU_GIT_URL**: Environment variable with Miaou git repository URL
  - Can be set in environment: `export MIAOU_GIT_URL="..."`
  - Or script will prompt for it interactively

## What the Script Does

The script reproduces the exact CI release process:

1. **Pulls or builds** the Alpine-based CI Docker image (`Dockerfile.ci`)
2. **Creates** `static_flags.sexp` with `(-ccopt -static)` for static linking
3. **Installs** Miaou and all dependencies inside the container
4. **Builds** with `dune build --release`
5. **Extracts** the binary from the container
6. **Verifies** the binary is truly static (not dynamically linked)
7. **Generates** SHA256 checksum
8. **Tests** the binary runs correctly

## Options

```
-o, --output NAME         Output binary name (default: octez-manager-static)
-l, --use-local-image     Build Docker image locally instead of pulling
-k, --keep-static-flags   Don't remove static_flags.sexp after build
-s, --skip-verification   Skip binary verification steps
-v, --verbose             Show detailed build output
-h, --help                Show help message
```

## Output

After successful build:

```
✓ Static binary build complete!

Output: octez-manager-static

To use the binary:
  ./octez-manager-static --help

To verify it's static:
  ldd ./octez-manager-static  # Should show 'not a dynamic executable'
```

Files created:
- `octez-manager-static` - The static binary
- `octez-manager-static.sha256` - SHA256 checksum

## Build Performance

- **First build**: 5-15 minutes (downloading/building image, dependencies)
- **Subsequent builds**: 1-3 minutes (Docker cache speeds everything up)

## Differences from Development Builds

| Aspect | Development Build | Static Release Build |
|--------|-------------------|---------------------|
| Command | `dune build` | `dune build --release` with `-ccopt -static` |
| Container | Your local system | Alpine Linux (musl-based) |
| Linking | Dynamic (requires system libs) | Static (self-contained) |
| Portability | Same OS/distro only | Any Linux system |
| Binary size | ~50-80 MB | ~80-120 MB |
| Build time | Fast (seconds) | Slow (minutes first time) |

## Troubleshooting

### Docker pull fails

If pulling the CI image fails:
```bash
# The script automatically falls back to local build, or force it:
./scripts/build-static-binary.sh --use-local-image
```

### MIAOU_GIT_URL not set

The script will prompt you to enter it interactively. To avoid prompts:
```bash
export MIAOU_GIT_URL="git+https://..."
./scripts/build-static-binary.sh
```

### Binary doesn't run on my system

The binary is built for Linux x86_64. If you're on:
- **Linux x86_64**: Should work out of the box
- **macOS**: Won't run (different OS)
- **Linux ARM64**: Won't run (different architecture)
- **Windows**: Won't run (use WSL2 with Linux x86_64)

To test in a container:
```bash
docker run --rm -v $PWD:/workspace -w /workspace alpine:latest /workspace/octez-manager-static --version
```

### Build is very slow

First builds are slow due to:
- Downloading/building Docker image
- Installing opam dependencies
- Compiling all OCaml code

Subsequent builds are much faster due to Docker layer caching.

To speed up:
```bash
# Pre-build the image once
docker build -f Dockerfile.ci -t octez-manager-ci-local .

# Then always use local image
./scripts/build-static-binary.sh --use-local-image
```

### Want to see what's happening

Use verbose mode:
```bash
./scripts/build-static-binary.sh --verbose
```

## Verifying the Binary Matches CI

To verify your local build matches CI releases:

1. Build locally:
   ```bash
   ./scripts/build-static-binary.sh --output octez-manager-local
   ```

2. Download CI release:
   ```bash
   wget https://github.com/trilitech/octez-manager/releases/download/v0.2.0/octez-manager-v0.2.0-linux-x86_64
   ```

3. Compare SHA256 checksums:
   ```bash
   sha256sum octez-manager-local
   sha256sum octez-manager-v0.2.0-linux-x86_64
   ```

Note: Checksums may differ slightly due to timestamps or minor build environment differences, but the binaries should function identically.

## Advanced: Building for Testing

When testing changes before release:

```bash
# Make your code changes
vim src/main.ml

# Build static binary
./scripts/build-static-binary.sh --output octez-manager-test

# Test in clean environment (Alpine container)
docker run --rm -it -v $PWD:/workspace -w /workspace alpine:latest sh
/workspace/octez-manager-test --help
```

## CI Workflow Integration

This script reproduces lines 302-306 of `.github/workflows/ci.yml`:

```yaml
- name: Build static binary
  run: |
    eval $(opam env)
    echo '(-ccopt -static)' > static_flags.sexp
    dune build --release
```

The output matches the release artifacts exactly.
