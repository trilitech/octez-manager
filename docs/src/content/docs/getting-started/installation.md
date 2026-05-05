---
title: Installation
description: How to install Octez Manager
---

## Prerequisites

- Linux (Ubuntu 22.04+, Debian 12+, or similar)

## Verified Install (Recommended)

Download the latest release binary, verify it against the release checksum
manifest, then install it on your PATH:

```bash
ASSET=octez-manager-linux-x86_64
curl -fsSLO "https://github.com/trilitech/octez-manager/releases/latest/download/${ASSET}"
curl -fsSLO https://github.com/trilitech/octez-manager/releases/latest/download/sha256sums.txt
grep "  ${ASSET}$" sha256sums.txt > "${ASSET}.sha256"
sha256sum -c "${ASSET}.sha256"
chmod +x "${ASSET}"
install -D "${ASSET}" ~/.local/bin/octez-manager
```

This installs Octez Manager to `~/.local/bin/octez-manager`.

> **Note:** If `~/.local/bin` is not in your PATH, add it before launching
> `octez-manager`.

### Version-Pinned Install

For reproducible deployments, pin the release tag explicitly:

```bash
VERSION=v1.0.0
ASSET="octez-manager-${VERSION}-linux-x86_64"
curl -fsSLO "https://github.com/trilitech/octez-manager/releases/download/${VERSION}/${ASSET}"
curl -fsSLO "https://github.com/trilitech/octez-manager/releases/download/${VERSION}/sha256sums.txt"
grep "  ${ASSET}$" sha256sums.txt > "${ASSET}.sha256"
sha256sum -c "${ASSET}.sha256"
chmod +x "${ASSET}"
install -D "${ASSET}" ~/.local/bin/octez-manager
```

### Custom Installation Directory

Use `--prefix` to specify a custom location:

```bash
install -D octez-manager-linux-x86_64 /opt/bin/octez-manager
```

### Verify Installation

```bash
octez-manager --version
```

## Manual Binary Download

If you prefer to download manually, get the binary from [GitHub Releases](https://github.com/trilitech/octez-manager/releases/latest):

```bash
# Download (replace vX.Y.Z with the latest version)
VERSION=vX.Y.Z
ASSET="octez-manager-${VERSION}-linux-x86_64"
curl -LO "https://github.com/trilitech/octez-manager/releases/download/${VERSION}/${ASSET}"
curl -LO "https://github.com/trilitech/octez-manager/releases/download/${VERSION}/sha256sums.txt"
grep "  ${ASSET}$" sha256sums.txt > "${ASSET}.sha256"
sha256sum -c "${ASSET}.sha256"

# Make executable and move to PATH
chmod +x "${ASSET}"
sudo mv "${ASSET}" /usr/local/bin/octez-manager

# Verify
octez-manager --version
```

## Running Modes

Octez Manager creates systemd services to manage your Tezos infrastructure. How you run it determines where services and data are stored:

### User Mode (Recommended for testing)

Run as a regular user. Creates **user-level systemd services** (`systemctl --user`).

```bash
octez-manager
```

| Item | Location |
|------|----------|
| Services | `~/.config/systemd/user/` |
| Configuration | `~/.config/octez/instances/` |
| Data | `~/.local/share/octez/` |

### System Mode (Recommended for production)

Run the manager with root privileges to create system-level systemd units and
prepare service users. The managed Octez daemons do not run as root: generated
units set `User=<service-user>` and `Group=<service-user>`.

```bash
sudo octez-manager
```

| Item | Location |
|------|----------|
| Services | `/etc/systemd/system/` |
| Configuration | `/etc/octez/instances/` |
| Data | `/var/lib/octez/` |

Generated system services include hardening directives such as
`NoNewPrivileges=yes`, `PrivateTmp=yes`, and `ProtectSystem=strict`. Routine
system-mode management currently uses `sudo octez-manager`; a narrower admin
group or polkit profile is not yet documented.

> **Note:** User mode and system mode are independent. Instances created in one mode are not visible in the other.

## Next Steps

- [Using the TUI](/guides/tui-guide/)
- [Setting Up a Node](/guides/node-setup/)
- [Setting Up a Baker](/guides/baker-setup/)
- [Signatory Setup Guide](/guides/signatory-setup/) (for secure key management)
