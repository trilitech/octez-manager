# CLI tester container for integration testing
# Uses real systemd as PID 1 for testing actual service management
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
    procps \
    && rm -rf /var/lib/apt/lists/*

# Remove unnecessary systemd units that cause issues in containers
RUN rm -rf /lib/systemd/system/multi-user.target.wants/* \
    /etc/systemd/system/*.wants/* \
    /lib/systemd/system/local-fs.target.wants/* \
    /lib/systemd/system/sockets.target.wants/*udev* \
    /lib/systemd/system/sockets.target.wants/*initctl* \
    /lib/systemd/system/sysinit.target.wants/systemd-tmpfiles-setup* \
    /lib/systemd/system/systemd-update-utmp* \
    /lib/systemd/system/getty* \
    2>/dev/null || true

# Download Octez binaries directly (more reliable than packages)
ARG OCTEZ_VERSION=v24.0
ARG OCTEZ_BASE_URL=https://octez.tezos.com/releases/octez-v24.0/binaries/x86_64
RUN for binary in octez-node octez-client octez-baker; do \
        echo "Downloading $binary..." && \
        curl -sfL "${OCTEZ_BASE_URL}/${binary}" -o /usr/local/bin/${binary} && \
        chmod +x /usr/local/bin/${binary}; \
    done

# Download Zcash/Sapling parameters required for node operation
ARG ZCASH_PARAMS_URL=https://download.z.cash/downloads
RUN mkdir -p /usr/local/share/zcash-params && \
    echo "Downloading sapling-output.params..." && \
    curl -sfL "${ZCASH_PARAMS_URL}/sapling-output.params" -o /usr/local/share/zcash-params/sapling-output.params && \
    echo "Downloading sapling-spend.params..." && \
    curl -sfL "${ZCASH_PARAMS_URL}/sapling-spend.params" -o /usr/local/share/zcash-params/sapling-spend.params

# Install octez-manager from build context
COPY cli-tester/octez-manager /usr/local/bin/octez-manager
RUN chmod +x /usr/local/bin/octez-manager

# Copy test scripts
COPY cli-tester/tests/ /tests/
COPY cli-tester/run-tests.sh /run-tests.sh
RUN chmod +x /run-tests.sh /tests/*.sh /tests/**/*.sh 2>/dev/null || true

# Ensure test user exists
RUN id tezos >/dev/null 2>&1 || useradd -m -s /bin/bash tezos

# Create required directories
RUN mkdir -p /var/lib/octez /etc/octez /var/log/octez \
    && chown -R tezos:tezos /var/lib/octez /etc/octez /var/log/octez

# Systemd needs these
VOLUME ["/sys/fs/cgroup", "/run", "/run/lock"]
STOPSIGNAL SIGRTMIN+3

# Run systemd as PID 1
CMD ["/lib/systemd/systemd"]
