---
title: Installation
description: How to install Octez Manager
---

## Prerequisites

- Linux (Ubuntu 22.04+, Debian 12+, or similar)
- **Octez binaries** (`octez-node`, `octez-client`, `octez-baker`, etc.) — See [How to get Octez](https://octez.tezos.com/docs/introduction/howtoget.html) for installation options including:
  - Static binaries (easiest)
  - Debian/Ubuntu packages
  - Building from source

## Quick Install (Recommended)

Install with a single command:

```bash
curl -fsSL https://raw.githubusercontent.com/trilitech/octez-manager/main/install.sh | sh
```

This downloads the latest release and installs it to `/usr/local/bin/`.

Verify the installation:

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

## From Source

### 1. Install OCaml and opam

```bash
# Ubuntu/Debian
sudo apt install opam

# Initialize opam
opam init
eval $(opam env)

# Install OCaml 5.1
opam switch create 5.1.0
eval $(opam env)
```

### 2. Clone and Build

```bash
git clone https://github.com/trilitech/octez-manager.git
cd octez-manager

# Install dependencies
opam install . --deps-only

# Build
dune build

# Install locally
dune install
```

### 3. Verify Installation

```bash
octez-manager --version
octez-manager --help
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
