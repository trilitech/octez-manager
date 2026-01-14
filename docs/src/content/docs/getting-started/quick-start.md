---
title: Quick Start
description: Get a Tezos node running in 5 minutes
---

# Quick Start

Get a Shadownet node running in 5 minutes. Shadownet is ideal for testing — it resets periodically and has fast block times.

## Using the TUI (Recommended)

Launch Octez Manager:

```bash
octez-manager
```

Select **[ Install new instance ]** → **Node**, then:

1. Enter instance name: `shadownet`
2. Select network: `shadownet`
3. Choose history mode: `rolling`
4. Keep default addresses
5. Bootstrap method: `Snapshot`

The node installs and begins syncing automatically.

> **Tip:** Press `?` at any time to see available actions.

---

## Using the CLI

Deploy a Shadownet node with one command:

```bash
octez-manager install-node \
  --instance shadownet \
  --network shadownet \
  --history-mode rolling \
  --snapshot
```

Check status:

```bash
octez-manager list
```

```
INSTANCE     ROLE    STATUS     NETWORK      MODE
shadownet    node    running    shadownet    rolling
```

---

## Monitor Your Node

**In the TUI:** Select your node to see sync progress, or press `l` for logs.

**Via CLI:**

```bash
# View logs
octez-manager instance shadownet logs

# Check sync status
curl -s http://127.0.0.1:8732/chains/main/blocks/head/header | jq .level
```

---

## Control Services

```bash
# Stop
octez-manager instance shadownet stop

# Start
octez-manager instance shadownet start

# Restart
octez-manager instance shadownet restart
```

---

## Next Steps

Once your node is synced:

- [Set up a baker](/octez-manager/guides/baker-setup/) to participate in consensus
- [Add a DAL node](/octez-manager/guides/dal-node-setup/) for data availability attestations
- [Learn the TUI](/octez-manager/guides/tui-guide/) for full feature access
