# Octez Manager

A terminal UI for managing Octez blockchain services (nodes, bakers, accusers, signers, DAL nodes).

![Octez Manager UI](assets/octez-manager.gif)

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
# Clone and build
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
  --snapshot rolling

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

- **[User Guide](https://trilitech.github.io/octez-manager/)** - Installation, tutorials, CLI reference
- **[CONTRIBUTING.md](./CONTRIBUTING.md)** - Development guidelines
- **[CHANGELOG.md](./CHANGELOG.md)** - Version history

## License

MIT - see [LICENSE](LICENSE).

**Maintainer:** Nomadic Labs (<contact@nomadic-labs.com>)

## License

MIT - see [LICENSE](LICENSE).

**Maintainer:** Nomadic Labs (<contact@nomadic-labs.com>)
