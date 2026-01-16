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

## Pre-built Binary (Recommended)

Download the latest binary from GitHub releases:

```bash
# Download
curl -LO https://github.com/trilitech/octez-manager/releases/latest/download/octez-manager-v0.1.0-linux-x86_64

# Make executable and move to PATH
chmod +x octez-manager-v0.1.0-linux-x86_64
sudo mv octez-manager-v0.1.0-linux-x86_64 /usr/local/bin/octez-manager

# Verify
octez-manager --version
```

See all releases at [GitHub Releases](https://github.com/trilitech/octez-manager/releases).

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

- [Quick Start Guide](/getting-started/quick-start)
- [Setting Up a Node](/guides/node-setup)
