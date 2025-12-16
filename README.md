# Octez Manager

Octez Manager is a tiny command-line utility that installs and manages
octez-node (and related daemon) systemd services without pulling the
full Octez Setup TUI.  It focuses on the minimum required features to
replace the Debian `octez-client`/`octez-node` meta installer shipped
with Octez v22.

## Status

This repository is a work-in-progress extraction from the larger
`octez-setup` project.  Only the service bootstrapper is provided for
now.  The TUI, wizard flows, and analytics are intentionally left out
so this tool stays small and auditable.

## Prerequisites

While Miaou is not yet released on opam, you must pin it manually before building:

```sh
opam pin add miaou <path-to-miaou-repo>#<branch>
```

## Quick start

```sh
opam install . --deps-only --with-test
dune build
```

Then install a node service:

```sh
dune exec -- octez-manager install-node \
  --instance mainnet-node \
  --network mainnet \
  --rpc-addr 127.0.0.1:8732 \
  --net-addr 0.0.0.0:9732 \
  --service-user octez \
  --app-bin-dir /usr/bin
```

Use `dune exec -- octez-manager list-available-networks` to inspect the
chains advertised on teztnets.com (use `--json` for machine output).
`dune exec -- octez-manager list-snapshots --network <alias>` queries
snapshots.tzinit.org and prints the download variants (rolling, full,
full:50, etc.) for a given network. When running `install-node`, pass
`--snapshot` (or just `--snapshot-kind`) plus an optional
`--snapshot-uri` if you want to import a specific file; otherwise the
installer fetches the HTTPS URL advertised by tzinit for the selected
variant. Use `--snapshot-no-check` to skip validation during snapshot
import at bootstrap time. Archive history mode still requires a manual data directory
bootstrap. Later on you can refresh an existing node with the same (or a
new) snapshot via `octez-manager snapshots import --instance <name>` plus
optional `--snapshot-uri`, `--snapshot-kind`, `--network`, and
`--no-check` overrides; the command stops the service, imports the
snapshot, and restarts it if it was running. When you want to wipe the
managed data directory entirely, move logs/identity aside, and re-import
fresh state, use `octez-manager instance <name> refresh-from-new-snapshot`
with the same override flags (or rely on the stored tzinit metadata).

Additional helpers exist for other Octez daemons:
`install-baker`, `install-accuser`, `install-signer`, and `install-dal-node`.
These commands reuse the same systemd
scaffolding, accept the usual `--service-user`, `--app-bin-dir`, and
`--no-enable` flags, and emit custom environment files/drop-ins so
`octez-manager instance <name> ...` can manage every role uniformly. The
signer helper mirrors the Debian sample unit: it installs a socket
signer, optionally pre-authorizes public keys via repeated `--authorize
NAME:KEY` flags, and prints the `octez-client -R` hint needed to tether
bakers to the remote signer endpoint.

For `install-baker`, you can configure DAL node integration via
`--dal-endpoint`:
- Provide an endpoint URL (e.g., `http://localhost:10732`) to use a DAL node
- Use `none` or `disabled` to opt-out with the `--without-dal` flag
- Omit the flag to default to disabled (no DAL integration)
- In interactive mode, you'll be prompted for the DAL endpoint (defaults to 'none')

The baker also accepts `--liquidity-baking-vote` with one of `on`, `off`, or 
`pass`. If omitted, it defaults to `pass`.

You can connect the baker to an existing node instance via `--node-instance` 
(use `octez-manager list` to see available nodes), or specify a custom endpoint 
via `--node-endpoint` (defaults to `http://127.0.0.1:8732`).

Run `octez-manager --help` for the full list of commands. Instance-level
operations now go through the explicit `instance` command:
`octez-manager instance <instance> <action>` where `<action>` is one of
`start`, `stop`, `restart`, `remove`, `purge`, `show`, `show-service`, or
`refresh-from-new-snapshot`.
For example, run
```
dune exec -- octez-manager instance mainnet-node remove --delete-data-dir
```
to unregister an instance but keep the service user around, or run
```
dune exec -- octez-manager instance mainnet-node purge
```
to delete the data dir, log files, and (when run as root) drop the
service user/group if no other managed services reference it. `show`
prints the recorded metadata while `show-service` invokes `systemctl` to
display the full unit (with drop-ins), enablement state, and live status
(use `--role` for non-node daemons).

To purge all registered instances at once, use the dedicated command:
```
dune exec -- octez-manager purge-all
```
This iterates through all services, purging each one and reporting on any
failures.

## Interactive UI

Octez Manager includes a terminal user interface (TUI) powered by Miaou.
Launch it with:

```sh
dune exec -- octez-manager ui
```

The UI is designed for efficiency with a list-first approach:

- **Instances (Home)**: A unified list of all managed services.
  - **Install**: Select "Install new instance" (or press `c`) to launch a wizard for Nodes, Bakers, and more.
    - Networks are pulled from teztnets.com so the dropdown always lists the current aliases and human-friendly names.
    - Snapshot choices are hydrated from snapshots.tzinit.org; picking one records the tzinit slug so refresh/import operations can reuse it.
  - **Manage**: Press `Enter` on any instance to access actions: Start, Stop, Restart, View Logs, Refresh Snapshot, or Purge.
  - **Filter**: Press `f` to cycle through roles (Node, Baker, etc.).
  - **Bulk Actions**: Press `b` to apply actions to all filtered instances.

- **Menu**: Press `m` to access other tools:
  - **Snapshots**: Browse and import snapshots from tzinit.org.
  - **Networks**: Explore available networks.
  - **Signers**: Manage remote signers and authorize keys.
  - **Settings**: Configure global defaults.
  - **Diagnostics & Activity**: Monitor system health and background jobs.

## License

MIT — see [LICENSE](LICENSE).
