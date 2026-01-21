# Runtime test container - builds on top of base image
# This layer contains only the octez-manager binary and test scripts
# which change frequently with each commit
#
# The base image contains:
# - debian:bookworm-slim with systemd
# - Octez binaries (node, client, baker, accuser, dal-node)
# - Zcash/Sapling parameters
# - Pre-generated identity file
# - Test user and directories
#
# To build the base image first:
#   docker build -f Dockerfile.base -t ghcr.io/trilitech/octez-manager/test-base:latest .
FROM ghcr.io/trilitech/octez-manager/test-base:latest

# Install octez-manager binary (changes every commit)
COPY cli-tester/octez-manager /usr/local/bin/octez-manager
RUN chmod +x /usr/local/bin/octez-manager

# Copy test scripts (changes frequently)
COPY cli-tester/tests/ /tests/
COPY cli-tester/run-tests.sh /run-tests.sh
RUN chmod +x /run-tests.sh /tests/*.sh /tests/**/*.sh 2>/dev/null || true
