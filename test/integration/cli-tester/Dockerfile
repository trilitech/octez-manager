# CLI tester container for integration testing
# Uses static octez-manager binary (built with musl libc)
FROM debian:bookworm-slim

# Install systemd and dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    systemd \
    systemd-sysv \
    dbus \
    curl \
    jq \
    ca-certificates \
    gnupg \
    netbase \
    && rm -rf /var/lib/apt/lists/*

# Remove unnecessary systemd units
RUN rm -f /lib/systemd/system/multi-user.target.wants/* \
    /etc/systemd/system/*.wants/* \
    /lib/systemd/system/local-fs.target.wants/* \
    /lib/systemd/system/sockets.target.wants/*udev* \
    /lib/systemd/system/sockets.target.wants/*initctl* \
    /lib/systemd/system/sysinit.target.wants/systemd-tmpfiles-setup* \
    /lib/systemd/system/systemd-update-utmp*

# Download Octez binaries directly (more reliable than packages)
ARG OCTEZ_VERSION=v24.0
ARG OCTEZ_BASE_URL=https://octez.tezos.com/releases/octez-v24.0/binaries/x86_64
RUN for binary in octez-node octez-client octez-baker; do \
        echo "Downloading $binary..." && \
        curl -sfL "${OCTEZ_BASE_URL}/${binary}" -o /usr/local/bin/${binary} && \
        chmod +x /usr/local/bin/${binary}; \
    done

# Install octez-manager from build context
COPY cli-tester/octez-manager /usr/local/bin/octez-manager
RUN chmod +x /usr/local/bin/octez-manager

# Copy test scripts
COPY cli-tester/tests/ /tests/
COPY cli-tester/run-tests.sh /run-tests.sh
RUN chmod +x /run-tests.sh /tests/*.sh /tests/**/*.sh 2>/dev/null || true

# Ensure test user exists (octez packages may already create it)
RUN id tezos >/dev/null 2>&1 || useradd -m -s /bin/bash tezos

# Create required directories
RUN mkdir -p /var/lib/octez /etc/octez /var/log/octez \
    && chown -R tezos:tezos /var/lib/octez /etc/octez /var/log/octez

# Install mock systemctl for testing (since systemd isn't running as PID 1)
COPY cli-tester/mock-systemctl.sh /usr/local/bin/systemctl
RUN chmod +x /usr/local/bin/systemctl

VOLUME ["/sys/fs/cgroup"]

# Default: keep container running for test execution
CMD ["sleep", "infinity"]
