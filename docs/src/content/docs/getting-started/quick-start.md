---
title: Quick Start
description: Get a Tezos node running in 5 minutes
---

# Quick Start

Get a Tezos node running in 5 minutes.

## Option 1: Interactive TUI

Launch the TUI:

```bash
octez-manager
```

You'll see the main dashboard:

```
┌─ Instances ──────────────────────────────────────────────────────────┐
│                                                                      │
│  No instances configured yet.                                        │
│                                                                      │
│  Press 'i' to install a new instance.                                │
│                                                                      │
└──────────────────────────────────────────────────────────────────────┘
 i: install  q: quit  ?: help
```

Press `i`, select **Node**, and follow the prompts.

After installation, your dashboard shows:

```
┌─ Instances ──────────────────────────────────────────────────────────┐
│                                                                      │
│  ● mainnet          node     running     mainnet     rolling         │
│                                                                      │
└──────────────────────────────────────────────────────────────────────┘
 i: install  s: start/stop  l: logs  Enter: details  q: quit
```

---

## Option 2: CLI one-liner

Deploy a mainnet node with snapshot:

```bash
octez-manager install-node \
  --instance mainnet \
  --network mainnet \
  --history-mode rolling \
  --snapshot
```

Check status:

```bash
octez-manager list
```

Output:

```
INSTANCE    ROLE    STATUS     NETWORK    MODE
mainnet     node    running    mainnet    rolling
```

---

## View logs

**TUI:** Select instance, press `l`

```
┌─ Logs: mainnet ─────────────────────────────────────────────────────┐
│ Source: journald  r: refresh  t: toggle  /: search  Esc: back       │
├─────────────────────────────────────────────────────────────────────┤
│ Jan 09 12:00:01 validator: applied block BLbc4...                   │
│ Jan 09 12:00:02 p2p: 48 active connections                          │
│ Jan 09 12:00:03 chain: head is now at level 7234521                 │
│ Jan 09 12:00:04 mempool: 15 pending operations                      │
└─────────────────────────────────────────────────────────────────────┘
```

**CLI:**

```bash
octez-manager instance mainnet logs
```

---

## Service control

```bash
# Stop
octez-manager instance mainnet stop

# Start
octez-manager instance mainnet start

# Restart
octez-manager instance mainnet restart

# Remove (keeps data)
octez-manager instance mainnet remove

# Purge (removes data)
octez-manager instance mainnet purge
```

---

## Next steps

- [Setting up a baker](/octez-manager/guides/baker-setup/)
- [TUI keyboard shortcuts](/octez-manager/guides/tui-guide/)
- [CLI reference](/octez-manager/reference/cli/)
