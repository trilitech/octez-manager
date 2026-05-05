#!/bin/bash
# Test: install.sh verifies release checksums before installing
set -euo pipefail
TESTS_DIR="${TESTS_DIR:-/tests}"
INSTALL_SH="${INSTALL_SH:-/install.sh}"
source "$TESTS_DIR/lib.sh"

test_init "Installer verifies release checksums"

if [ ! -f "$INSTALL_SH" ]; then
	echo "ERROR: install.sh not found at $INSTALL_SH"
	exit 1
fi

wait_for_http() {
	local url="$1"
	local max_wait="${2:-10}"
	local count=0

	while [ "$count" -lt "$max_wait" ]; do
		if curl -fsS "$url" >/dev/null 2>&1; then
			return 0
		fi
		sleep 1
		count=$((count + 1))
	done

	echo "ERROR: HTTP fixture did not become ready: $url"
	return 1
}

make_release_fixture() {
	local version="$1"
	local mode="$2"
	local root="$3"
	local port="$4"
	local www="$root/www"
	local release_dir="$www/releases/$version"
	local asset_name="octez-manager-${version}-linux-x86_64"
	local asset_path="$release_dir/$asset_name"

	mkdir -p "$release_dir"
	printf '#!/bin/sh\nprintf "installed %s\\n"\n' "$version" >"$asset_path"
	chmod 0644 "$asset_path"
	printf '{"tag_name":"%s"}\n' "$version" >"$www/latest.json"

	local hash
	hash=$(sha256sum "$asset_path" | awk '{print $1}')
	case "$mode" in
	ok)
		printf '%s  %s\n' "$hash" "$asset_name" >"$release_dir/sha256sums.txt"
		;;
	mismatch)
		printf '%064d  %s\n' 0 "$asset_name" >"$release_dir/sha256sums.txt"
		;;
	missing)
		printf '%s  some-other-asset\n' "$hash" >"$release_dir/sha256sums.txt"
		;;
	*)
		echo "ERROR: unknown fixture mode: $mode"
		return 1
		;;
	esac

	python3 -m http.server "$port" --bind 127.0.0.1 --directory "$www" >"$root/http.log" 2>&1 &
	register_process "$!"
	wait_for_http "http://127.0.0.1:$port/latest.json"
}

run_installer() {
	local version="$1"
	local port="$2"
	local prefix="$3"
	shift 3

	env \
		OCTEZ_MANAGER_GITHUB_LATEST_RELEASE_URL="http://127.0.0.1:$port/latest.json" \
		OCTEZ_MANAGER_RELEASE_DOWNLOAD_BASE_URL="http://127.0.0.1:$port/releases" \
		"$@" \
		sh "$INSTALL_SH" --prefix="$prefix/bin"
}

assert_not_installed() {
	local prefix="$1"
	if [ -e "$prefix/bin/octez-manager" ]; then
		echo "ERROR: installer left a binary behind after failed verification"
		ls -l "$prefix/bin/octez-manager"
		return 1
	fi
}

echo "Scenario: successful install from locally served release"
SUCCESS_ROOT=$(mktemp -d /tmp/om-install-success-XXXXXX)
SUCCESS_PREFIX=$(mktemp -d /tmp/om-install-prefix-success-XXXXXX)
register_data_dir "$SUCCESS_ROOT"
register_data_dir "$SUCCESS_PREFIX"
mkdir -p "$SUCCESS_ROOT" "$SUCCESS_PREFIX"
SUCCESS_PORT=$(alloc_port)
SUCCESS_VERSION="v99.0.0"
make_release_fixture "$SUCCESS_VERSION" ok "$SUCCESS_ROOT" "$SUCCESS_PORT"
run_installer "$SUCCESS_VERSION" "$SUCCESS_PORT" "$SUCCESS_PREFIX"
assert_file_exists "$SUCCESS_PREFIX/bin/octez-manager"
if [ ! -x "$SUCCESS_PREFIX/bin/octez-manager" ]; then
	echo "ERROR: installed binary is not executable"
	exit 1
fi
OUTPUT=$("$SUCCESS_PREFIX/bin/octez-manager")
assert_eq "installed $SUCCESS_VERSION" "$OUTPUT" "installed binary output"

echo "Scenario: checksum mismatch aborts without installing"
MISMATCH_ROOT=$(mktemp -d /tmp/om-install-mismatch-XXXXXX)
MISMATCH_PREFIX=$(mktemp -d /tmp/om-install-prefix-mismatch-XXXXXX)
register_data_dir "$MISMATCH_ROOT"
register_data_dir "$MISMATCH_PREFIX"
mkdir -p "$MISMATCH_ROOT" "$MISMATCH_PREFIX"
MISMATCH_PORT=$(alloc_port)
MISMATCH_VERSION="v99.0.1"
make_release_fixture "$MISMATCH_VERSION" mismatch "$MISMATCH_ROOT" "$MISMATCH_PORT"
if run_installer "$MISMATCH_VERSION" "$MISMATCH_PORT" "$MISMATCH_PREFIX" VERSION="$MISMATCH_VERSION" >"$MISMATCH_ROOT/install.log" 2>&1; then
	echo "ERROR: installer succeeded despite checksum mismatch"
	cat "$MISMATCH_ROOT/install.log"
	exit 1
fi
assert_not_installed "$MISMATCH_PREFIX"
assert_contains "$(cat "$MISMATCH_ROOT/install.log")" "FAILED" "checksum mismatch reported"

echo "Scenario: missing checksum entry aborts without installing"
MISSING_ROOT=$(mktemp -d /tmp/om-install-missing-XXXXXX)
MISSING_PREFIX=$(mktemp -d /tmp/om-install-prefix-missing-XXXXXX)
register_data_dir "$MISSING_ROOT"
register_data_dir "$MISSING_PREFIX"
mkdir -p "$MISSING_ROOT" "$MISSING_PREFIX"
MISSING_PORT=$(alloc_port)
MISSING_VERSION="v99.0.2"
make_release_fixture "$MISSING_VERSION" missing "$MISSING_ROOT" "$MISSING_PORT"
if run_installer "$MISSING_VERSION" "$MISSING_PORT" "$MISSING_PREFIX" VERSION="$MISSING_VERSION" >"$MISSING_ROOT/install.log" 2>&1; then
	echo "ERROR: installer succeeded despite missing checksum entry"
	cat "$MISSING_ROOT/install.log"
	exit 1
fi
assert_not_installed "$MISSING_PREFIX"
assert_contains "$(cat "$MISSING_ROOT/install.log")" "No checksum entry found" "missing checksum reported"

echo "Installer checksum verification E2E test passed"
