#!/bin/bash
set -euo pipefail

# build-static-binary.sh
# Reproduces the exact CI static binary build process locally
# This script matches the release job in .github/workflows/ci.yml

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Configuration
CI_IMAGE="ghcr.io/trilitech/octez-manager-ci:latest"
DEFAULT_OUTPUT="octez-manager-static"
DOCKER_BUILD_IMAGE="octez-manager-ci-local"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Flags
USE_LOCAL_IMAGE=false
SKIP_VERIFICATION=false
OUTPUT_NAME=""
KEEP_STATIC_FLAGS=false
VERBOSE=false

usage() {
	cat <<EOF
Usage: $0 [OPTIONS]

Reproduce the exact CI static binary build process locally using Docker.

OPTIONS:
  -o, --output NAME         Output binary name (default: $DEFAULT_OUTPUT)
  -l, --use-local-image     Build Docker image locally instead of pulling from registry
  -k, --keep-static-flags   Don't remove static_flags.sexp after build
  -s, --skip-verification   Skip binary verification steps
  -v, --verbose             Show detailed build output
  -h, --help                Show this help message

EXAMPLES:
  # Basic build (tries to pull CI image, builds locally if fails)
  $0

  # Build with custom output name
  $0 --output octez-manager-v0.2.0-linux-x86_64

  # Force local image build
  $0 --use-local-image

  # Verbose build with all checks
  $0 --verbose

NOTES:
  - Requires Docker to be installed and running
  - First build may take several minutes (downloading/building image + dependencies)
  - Subsequent builds are much faster due to Docker layer caching
  - The binary will be truly statically linked (Alpine musl-based)
  - MIAOU_GIT_URL must be set in environment or will be prompted

EOF
	exit 0
}

log() {
	echo -e "${BLUE}==>${NC} $*"
}

success() {
	echo -e "${GREEN}✓${NC} $*"
}

warn() {
	echo -e "${YELLOW}⚠${NC} $*"
}

error() {
	echo -e "${RED}✗${NC} $*" >&2
	exit 1
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
	case $1 in
	-o | --output)
		OUTPUT_NAME="$2"
		shift 2
		;;
	-l | --use-local-image)
		USE_LOCAL_IMAGE=true
		shift
		;;
	-k | --keep-static-flags)
		KEEP_STATIC_FLAGS=true
		shift
		;;
	-s | --skip-verification)
		SKIP_VERIFICATION=true
		shift
		;;
	-v | --verbose)
		VERBOSE=true
		shift
		;;
	-h | --help)
		usage
		;;
	*)
		error "Unknown option: $1. Use -h for help."
		;;
	esac
done

# Set default output name if not specified
if [[ -z "$OUTPUT_NAME" ]]; then
	OUTPUT_NAME="$DEFAULT_OUTPUT"
fi

# Check prerequisites
log "Checking prerequisites..."

if ! command -v docker &>/dev/null; then
	error "Docker is not installed. Please install Docker first: https://docs.docker.com/get-docker/"
fi

if ! docker info &>/dev/null; then
	error "Docker daemon is not running. Please start Docker."
fi

success "Docker is available"

# Check for MIAOU_GIT_URL
if [[ -z "${MIAOU_GIT_URL:-}" ]]; then
	warn "MIAOU_GIT_URL environment variable is not set"
	echo "The Miaou library is required to build octez-manager."
	echo "Please enter the Miaou git URL (or press Ctrl+C to exit):"
	read -r MIAOU_GIT_URL
	export MIAOU_GIT_URL

	if [[ -z "$MIAOU_GIT_URL" ]]; then
		error "MIAOU_GIT_URL is required to build"
	fi
fi

# Determine Docker image strategy
DOCKER_IMAGE=""
if [[ "$USE_LOCAL_IMAGE" == true ]]; then
	log "Building Docker image locally..."
	if [[ "$VERBOSE" == true ]]; then
		docker build -f "$REPO_ROOT/Dockerfile.ci" -t "$DOCKER_BUILD_IMAGE" "$REPO_ROOT"
	else
		docker build -f "$REPO_ROOT/Dockerfile.ci" -t "$DOCKER_BUILD_IMAGE" "$REPO_ROOT" >/dev/null 2>&1
	fi
	success "Docker image built: $DOCKER_BUILD_IMAGE"
	DOCKER_IMAGE="$DOCKER_BUILD_IMAGE"
else
	log "Attempting to pull CI image from registry..."
	if docker pull "$CI_IMAGE" >/dev/null 2>&1; then
		success "Using CI image: $CI_IMAGE"
		DOCKER_IMAGE="$CI_IMAGE"
	else
		warn "Failed to pull CI image, building locally..."
		if [[ "$VERBOSE" == true ]]; then
			docker build -f "$REPO_ROOT/Dockerfile.ci" -t "$DOCKER_BUILD_IMAGE" "$REPO_ROOT"
		else
			docker build -f "$REPO_ROOT/Dockerfile.ci" -t "$DOCKER_BUILD_IMAGE" "$REPO_ROOT" >/dev/null 2>&1
		fi
		success "Docker image built: $DOCKER_BUILD_IMAGE"
		DOCKER_IMAGE="$DOCKER_BUILD_IMAGE"
	fi
fi

# Clean up any existing static_flags.sexp
if [[ -f "$REPO_ROOT/static_flags.sexp" ]]; then
	warn "Removing existing static_flags.sexp"
	rm "$REPO_ROOT/static_flags.sexp"
fi

# Build the static binary
log "Building static binary (this may take a few minutes on first run)..."

BUILD_SCRIPT='
set -euo pipefail

# Set up opam environment
eval $(opam env)

# Create static linking flags file
echo "(-ccopt -static)" > static_flags.sexp

# Check if Miaou is already pinned/installed
if ! opam list miaou-core --installed > /dev/null 2>&1; then
  echo "Miaou not found, installing..."
  opam pin add miaou-core "$MIAOU_GIT_URL" --no-action -y
  opam pin add miaou-driver-term "$MIAOU_GIT_URL" --no-action -y
  opam pin add miaou-driver-matrix "$MIAOU_GIT_URL" --no-action -y
  opam pin add miaou-runner "$MIAOU_GIT_URL" --no-action -y
  opam install miaou-core miaou-driver-term miaou-driver-matrix miaou-runner eio_posix -y
else
  echo "Miaou already installed, skipping..."
fi

# Install project dependencies
echo "Installing project dependencies..."
opam install . --deps-only --with-test -y || {
  echo "Failed to install dependencies, trying to update pins..."
  # If deps fail, Miaou might be installed but the project needs different pins
  # This matches what CI does
  opam pin add miaou-core "$MIAOU_GIT_URL" --no-action -y
  opam pin add miaou-driver-term "$MIAOU_GIT_URL" --no-action -y  
  opam pin add miaou-driver-matrix "$MIAOU_GIT_URL" --no-action -y
  opam pin add miaou-runner "$MIAOU_GIT_URL" --no-action -y
  opam install . --deps-only --with-test -y
}

# Build with release profile
echo "Building..."
dune build --release

# Copy binary to output location and fix permissions to match host user
cp _build/default/src/main.exe /output/binary
chmod 755 /output/binary
if [ -n "${HOST_UID:-}" ] && [ -n "${HOST_GID:-}" ]; then
  chown "$HOST_UID:$HOST_GID" /output/binary
fi

echo "Build complete!"
'

# Create output directory
OUTPUT_DIR="$REPO_ROOT/.build-output"
mkdir -p "$OUTPUT_DIR"

# Run the build in Docker
HOST_UID=$(id -u)
HOST_GID=$(id -g)

DOCKER_RUN_ARGS=(
	--rm
	-v "$REPO_ROOT:/workspace"
	-v "$OUTPUT_DIR:/output"
	-w /workspace
	-e "MIAOU_GIT_URL=$MIAOU_GIT_URL"
	-e "OPAMYES=true"
	-e "HOST_UID=$HOST_UID"
	-e "HOST_GID=$HOST_GID"
)

if [[ "$VERBOSE" == false ]]; then
	DOCKER_RUN_ARGS+=(-e "OPAMVERBOSE=0")
fi

if [[ "$VERBOSE" == true ]]; then
	docker run "${DOCKER_RUN_ARGS[@]}" "$DOCKER_IMAGE" bash -c "$BUILD_SCRIPT"
else
	docker run "${DOCKER_RUN_ARGS[@]}" "$DOCKER_IMAGE" bash -c "$BUILD_SCRIPT" 2>&1 | grep -E "Installing|Building|Build complete"
fi

# Move binary to final location
mv "$OUTPUT_DIR/binary" "$REPO_ROOT/$OUTPUT_NAME"
chmod +x "$REPO_ROOT/$OUTPUT_NAME"
rm -rf "$OUTPUT_DIR"

success "Binary created: $OUTPUT_NAME"

# Cleanup static_flags.sexp unless --keep-static-flags is set
if [[ "$KEEP_STATIC_FLAGS" == false ]] && [[ -f "$REPO_ROOT/static_flags.sexp" ]]; then
	rm "$REPO_ROOT/static_flags.sexp"
fi

# Verification steps
if [[ "$SKIP_VERIFICATION" == false ]]; then
	log "Verifying binary..."

	# Check if binary is static
	LDD_OUTPUT=$(docker run --rm -v "$REPO_ROOT:/workspace" -w /workspace alpine:latest ldd "/workspace/$OUTPUT_NAME" 2>&1 || true)

	if echo "$LDD_OUTPUT" | grep -q "not a dynamic executable"; then
		success "Binary is statically linked"
	elif echo "$LDD_OUTPUT" | grep -q "ld-musl"; then
		# Musl-based static binaries show ld-musl, which is normal
		success "Binary is statically linked (musl-based)"
	else
		warn "Binary may not be fully static!"
		echo "$LDD_OUTPUT"
	fi

	# Check if binary runs
	if docker run --rm -v "$REPO_ROOT:/workspace" -w /workspace alpine:latest "/workspace/$OUTPUT_NAME" --version >/dev/null 2>&1; then
		VERSION=$(docker run --rm -v "$REPO_ROOT:/workspace" -w /workspace alpine:latest "/workspace/$OUTPUT_NAME" --version 2>/dev/null | head -n1)
		success "Binary runs successfully: $VERSION"
	else
		warn "Binary test failed (may need specific runtime environment)"
	fi

	# Show file size
	SIZE=$(du -h "$REPO_ROOT/$OUTPUT_NAME" | cut -f1)
	log "Binary size: $SIZE"

	# Generate checksum
	CHECKSUM=$(sha256sum "$REPO_ROOT/$OUTPUT_NAME" | cut -d' ' -f1)
	echo "$CHECKSUM  $OUTPUT_NAME" >"$REPO_ROOT/$OUTPUT_NAME.sha256"
	success "Checksum saved to: $OUTPUT_NAME.sha256"
	log "SHA256: $CHECKSUM"
fi

echo ""
success "Static binary build complete!"
echo ""
echo "Output: $OUTPUT_NAME"
echo ""
echo "To use the binary:"
echo "  ./$OUTPUT_NAME --help"
echo ""
echo "To verify it's static:"
echo "  ldd ./$OUTPUT_NAME  # Should show 'not a dynamic executable'"
echo ""
