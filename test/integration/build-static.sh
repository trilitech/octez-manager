#!/bin/bash
# Build a static octez-manager binary using the CI Docker image
# Requires MIAOU_GIT_URL environment variable to be set
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
OUTPUT_PATH="$SCRIPT_DIR/cli-tester/octez-manager"

# Check for MIAOU_GIT_URL
if [ -z "${MIAOU_GIT_URL:-}" ]; then
    echo "ERROR: MIAOU_GIT_URL environment variable is required"
    echo "This is the git URL for the private miaou repository"
    exit 1
fi

CI_IMAGE="ghcr.io/trilitech/octez-manager-ci:latest"

echo "Building static octez-manager binary..."
echo "Using CI image: $CI_IMAGE"

# Pull the CI image (may require ghcr.io authentication)
echo "Pulling CI image..."
if ! docker pull "$CI_IMAGE" 2>/dev/null; then
    echo ""
    echo "ERROR: Failed to pull CI image. You may need to authenticate:"
    echo "  echo \$GITHUB_TOKEN | docker login ghcr.io -u <your-username> --password-stdin"
    echo ""
    echo "Or build locally using: dune build --release"
    echo "(Note: local builds may have glibc compatibility issues in containers)"
    exit 1
fi

# Build the static binary inside Docker
echo "Building inside container..."
docker run --rm \
    -v "$PROJECT_ROOT:/workspace" \
    -w /workspace \
    -e MIAOU_GIT_URL="$MIAOU_GIT_URL" \
    -e OPAMYES=true \
    -e OPAMROOT=/home/opam/.opam \
    "$CI_IMAGE" \
    sh -c '
        opam pin add miaou-core "$MIAOU_GIT_URL" --no-action
        opam pin add miaou-driver-term "$MIAOU_GIT_URL" --no-action
        opam pin add miaou-driver-matrix "$MIAOU_GIT_URL" --no-action
        opam pin add miaou-runner "$MIAOU_GIT_URL" --no-action
        opam install miaou-core miaou-driver-term miaou-driver-matrix miaou-runner eio_posix
        echo "(-ccopt -static)" > static_flags.sexp
        dune build --release
    '

# Copy the binary to the expected location
echo "Copying binary to $OUTPUT_PATH..."
cp "$PROJECT_ROOT/_build/default/src/main.exe" "$OUTPUT_PATH"
chmod +x "$OUTPUT_PATH"

# Verify it's statically linked
if ldd "$OUTPUT_PATH" 2>&1 | grep -q "not a dynamic executable"; then
    echo "SUCCESS: Static binary built at $OUTPUT_PATH"
else
    echo "WARNING: Binary may be dynamically linked"
    ldd "$OUTPUT_PATH" || true
fi
