<p align="center">
  <img src="assets/logo.png" alt="Octez Manager" width="200">
</p>

<h1 align="center">Octez Manager</h1>

<p align="center">
  A terminal UI for managing Octez blockchain services (nodes, bakers, accusers, DAL nodes).
</p>

<p align="center">
  <a href="https://github.com/trilitech/octez-manager/actions/workflows/coverage.yml"><img src="https://github.com/trilitech/octez-manager/actions/workflows/coverage.yml/badge.svg?branch=main" alt="CI"></a>
  <img src="https://img.shields.io/endpoint?url=https%3A%2F%2Fgist.githubusercontent.com%2Fmathiasbourgoin%2Fd7e91c883186314cdbf0ea6844291bd0%2Fraw%2Foctez-manager-coverage.json" alt="Coverage">
</p>

<p align="center">
  <a href="https://octez-manager.tezos.com"><strong>Documentation</strong></a>
</p>

<p align="center">
  <img src="assets/octez-manager.gif" alt="Octez Manager UI">
</p>

> **Warning**  
> Octez Manager is in active development. Use at your own risk, especially on mainnet.

## Features

- **Install and manage** Octez services as systemd units
- **Terminal UI** with real-time monitoring and logs
- **Web interface** for browser-based remote management
- **RPC Browser** — Interactively explore and query RPC endpoints with syntax highlighting
- **Snapshot import** from tzinit.org with automatic download
- **Multiple instances** per service type
- **Network discovery** from teztnets.com (mainnet, shadownet, etc.)

## Quick Start

### Installation

**Quick install (default location):**

```sh
curl -fsSL https://raw.githubusercontent.com/trilitech/octez-manager/main/install.sh | sh
```

- Root user: installs to `/usr/local/bin` (system-wide)
- Regular user: installs to `~/.local/bin` (user-local)

**Custom installation directory:**

```sh
curl -fsSL https://raw.githubusercontent.com/trilitech/octez-manager/main/install.sh | sh -s -- --prefix=/custom/path
```

**Build from source:**

```sh
git clone https://github.com/trilitech/octez-manager.git
cd octez-manager
opam install . --deps-only
make build
```

### Prerequisites

- OCaml 5.1+, opam 2.1+
- systemd
- Octez binaries in PATH
- sudo access (for system-wide services)

### Launch the UI

```sh
# Terminal UI
octez-manager

# Web interface (browser-based, for remote access)
octez-manager web --port 8080
```

The UI provides installation wizards, service monitoring, log viewing, and snapshot management. The web interface offers the same features accessible from any browser.

### Matrix driver tuning (Miaou 0.3)

When running in matrix mode (`MIAOU_DRIVER=matrix`), you can tune responsiveness and redraw behavior:

```sh
# Tick/render rate caps (defaults are 60)
export MIAOU_MATRIX_TPS=60
export MIAOU_MATRIX_FPS=60

# Full-screen scrub cadence (default 30 frames, 0 disables)
export MIAOU_MATRIX_SCRUB_FRAMES=30
```

Useful presets:

- Lower CPU on constrained hosts: `MIAOU_MATRIX_TPS=30 MIAOU_MATRIX_FPS=30`
- Maximum smoothness on fast terminals: keep defaults at `60/60`
- Investigating redraw artifacts: lower `MIAOU_MATRIX_SCRUB_FRAMES` (or disable with `0`)

### CLI Examples

```sh
# Install a node with snapshot
octez-manager install-node \
  --instance mainnet-node \
  --network mainnet \
  --snapshot \
  --history-mode rolling

# Install a baker
octez-manager install-baker \
  --instance mainnet-baker \
  --node mainnet-node

# List services
octez-manager list

# Service actions
octez-manager instance <name> start|stop|restart|purge

# View logs
octez-manager logs <name>
```

Run `octez-manager --help` for all commands.

## Documentation

- **[CONTRIBUTING.md](./CONTRIBUTING.md)** - Development guidelines
- **[CHANGELOG.md](./CHANGELOG.md)** - Version history
- **[docs/TUI_TESTING_GUIDE.md](./docs/TUI_TESTING_GUIDE.md)** - Writing TUI tests
- **[COVERAGE_PRIORITIES.md](./COVERAGE_PRIORITIES.md)** - Test coverage roadmap

## License

MIT - see [LICENSE](LICENSE).

---

<p align="center">
  <a href="https://www.nomadic-labs.com/">
    <picture>
      <source media="(prefers-color-scheme: dark)" srcset="assets/nomadic-labs-logo-white.png">
      <img src="assets/nomadic-labs-logo.png" alt="Nomadic Labs" height="28">
    </picture>
  </a>
</p>
