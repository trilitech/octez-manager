---
title: Installation
description: How to install Octez Manager
---

# Installation

## Prerequisites

- Linux (Ubuntu 22.04+, Debian 12+, or similar)
- OCaml 5.1+ and opam (for building from source)
- Octez binaries installed (for managing nodes/bakers)

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

Octez Manager can run in two modes:

### User Mode (Recommended for testing)

Run without root privileges. Services run as your user.

```bash
octez-manager
```

### System Mode (Recommended for production)

Run with root privileges. Creates dedicated service users and proper system directories.

```bash
sudo octez-manager
```

## Next Steps

- [Quick Start Guide](/octez-manager/getting-started/quick-start)
- [Setting Up a Node](/octez-manager/guides/node-setup)
