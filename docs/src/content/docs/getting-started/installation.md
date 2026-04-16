---
title: Installation
description: How to install Octez Manager
---

## Prerequisites

- Linux (Ubuntu 22.04+, Debian 12+, or similar)

## Quick Install (Recommended)

Install with a single command:

```bash
curl -fsSL https://raw.githubusercontent.com/trilitech/octez-manager/main/install.sh | sh
```

This downloads the latest release and installs it to:
- `~/.local/bin/` when run as a regular user
- `/usr/local/bin/` when run as root

> **Note:** If `~/.local/bin` is not in your PATH, the installer will show instructions to add it.

### Custom Installation Directory

Use `--prefix` to specify a custom location:

```bash
curl -fsSL https://raw.githubusercontent.com/trilitech/octez-manager/main/install.sh | sh -s -- --prefix=/opt/bin
```

### Verify Installation

```bash
octez-manager --version
```

## Manual Binary Download

If you prefer to download manually, get the binary from [GitHub Releases](https://github.com/trilitech/octez-manager/releases/latest):

```bash
# Download (replace vX.Y.Z with the latest version)
curl -LO https://github.com/trilitech/octez-manager/releases/latest/download/octez-manager-vX.Y.Z-linux-x86_64

# Make executable and move to PATH
chmod +x octez-manager-v*-linux-x86_64
sudo mv octez-manager-v*-linux-x86_64 /usr/local/bin/octez-manager

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

Run as root. Creates **system-level systemd services** with dedicated service users.

```bash
sudo octez-manager
```

| Item | Location |
|------|----------|
| Services | `/etc/systemd/system/` |
| Configuration | `/etc/octez/instances/` |
| Data | `/var/lib/octez/` |

> **Note:** User mode and system mode are independent. Instances created in one mode are not visible in the other.

## Next Steps

- [Using the TUI](/guides/tui-guide/)
- [Setting Up a Node](/guides/node-setup/)
- [Setting Up a Baker](/guides/baker-setup/)
- [Signatory Setup Guide](/guides/signatory-setup/) (for secure key management)
