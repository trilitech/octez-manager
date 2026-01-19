<p align="center">
  <img src="assets/logo.png" alt="Octez Manager" width="200">
</p>

<h1 align="center">Octez Manager</h1>

<p align="center">
  A terminal UI for managing Octez blockchain services (nodes, bakers, accusers, DAL nodes).
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
- **Snapshot import** from tzinit.org with automatic download
- **Multiple instances** per service type
- **Network discovery** from teztnets.com (mainnet, ghostnet, etc.)

## Quick Start

### Installation

```sh
curl -fsSL https://raw.githubusercontent.com/trilitech/octez-manager/main/install.sh | sh
```

Or build from source:

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
octez-manager
```

The UI provides installation wizards, service monitoring, log viewing, and snapshot management.

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
