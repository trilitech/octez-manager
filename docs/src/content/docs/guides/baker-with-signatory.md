---
title: Baking with Signatory
description: End-to-end guide to setting up a Tezos baker with Signatory remote signer
---

This guide provides a complete walkthrough for setting up a Tezos baker with [Signatory](https://github.com/ecadlabs/signatory) as a remote signer. By separating key management from your baker, you improve security and can use hardware security modules (HSMs) or cloud KMS solutions.

## Overview

In a standard baker setup, private keys are stored directly in the baker's configuration. With Signatory, the architecture looks like this:

```
[Tezos Node] ←→ [Baker] ←→ [Signatory] ←→ [Keys (File/HSM/Cloud KMS)]
```

- **Tezos Node**: Provides blockchain data
- **Baker**: Baking logic, no private keys
- **Signatory**: Remote signing service, holds/manages keys
- **Keys**: Stored in file, YubiHSM, or cloud KMS

## Prerequisites

Before you begin, ensure you have:

1. **Running Tezos node**: Synced to your target network (see [Setting Up a Node](/guides/node-setup/))
2. **Baker keys**: Generated Tezos keys (public key hash starting with tz1/tz2/tz3)
3. **Staked funds**: Minimum 6,000 tez staked for mainnet (use faucet for testnets)
4. **octez-manager installed**: For service management

## Step-by-Step Setup

### Step 1: Generate Baker Keys

Before installing Signatory, generate or import your baker keys. You'll need the **public key hash** (tz1...) to configure Signatory's authorized keys.

#### Option A: Generate New Keys

```bash
# Generate keys with octez-client
octez-client gen keys my-baker

# Display the public key hash (save this for Step 3)
octez-client show address my-baker
```

Example output:
```
Hash: tz1abc123defghijklmnopqrstuvwxyz
Public Key: edpkXYZ...
```

#### Option B: Import Existing Keys

```bash
# From secret key
octez-client import secret key my-baker unencrypted:edsk...

# From Ledger hardware wallet
octez-client import secret key my-baker "ledger://..."

# From fundraiser mnemonic
octez-client import fundraiser my-baker
```

#### Option C: Use Faucet (Testnets Only)

For testnet baking, use the faucet to generate keys and receive test tokens:

1. Visit the testnet faucet (e.g., https://faucet.shadownet.teztnets.com/)
2. Download the faucet JSON file
3. Import the keys:

```bash
octez-client activate account my-baker from faucet.json
octez-client show address my-baker
```

**Save your public key hash (tz1...)** — you'll need it for Signatory configuration.

### Step 2: Install Signatory

Now install Signatory with file-based keys (for development/testing) or hardware/cloud backend (for production).

#### Development Setup (File Backend)

**Via TUI:**
1. In octez-manager, select **[ Install new instance ]** → **Signatory**
2. Configure:
   - **Instance name**: `dev-signer`
   - **Backend**: `file`
   - **Authorized keys**: Paste your public key hash from Step 2 (e.g., `tz1abc123...`)
   - **Address**: `127.0.0.1:6732` (default, for local baker)
   - **Keys directory**: Use default or specify custom path

**Via CLI:**
```bash
octez-manager install-signatory \
  --instance dev-signer \
  --backend file \
  --authorized-keys tz1abc123... \
  --signatory-version latest
```

This creates a systemd service `octez-signatory-dev-signer` that starts automatically.

#### Production Setup (Hardware/Cloud)

For production, use YubiHSM or cloud KMS backends. These require additional setup:

- **YubiHSM**: See [Signatory Setup Guide - YubiHSM Backend](/guides/signatory-setup/#yubihsm-backend-hardware-security)
- **AWS KMS**: See [Signatory Setup Guide - AWS KMS Backend](/guides/signatory-setup/#aws-kms)
- **Azure/GCP KMS**: See [Signatory Setup Guide - Cloud KMS Backends](/guides/signatory-setup/#cloud-kms-backends-production-cloud)

### Step 3: Import Keys into Signatory

After Signatory is installed, import your baker keys into Signatory's keys directory.

**Determine keys directory:**
```bash
# User mode (default)
KEY_DIR=~/.local/share/octez/signatory/dev-signer/keys

# System mode (if running as root/sudo)
KEY_DIR=/var/lib/octez/signatory/dev-signer/keys
```

**Import the keys:**

```bash
# Copy octez-client keys to Signatory
cp ~/.tezos-client/secret_keys $KEY_DIR/

# Set proper permissions
chmod 600 $KEY_DIR/secret_keys

# Or import directly into Signatory's directory
octez-client -d $KEY_DIR import secret key my-baker unencrypted:edsk...
```

### Step 4: Verify Signatory Configuration

Before installing the baker, verify Signatory is running and recognizes your keys.

**Check service status via TUI:**
1. Launch `octez-manager`
2. Navigate to your Signatory instance (e.g., `dev-signer`)
3. Verify status shows "Running"

**Or via CLI:**
```bash
systemctl --user status octez-signatory-dev-signer
```

Expected output: `active (running)`

**Test Signatory endpoint:**
```bash
curl http://127.0.0.1:6732/keys
```

Expected output (example):
```json
[
  {
    "public_key": "edpkXYZ123...",
    "hash": "tz1abc123..."
  }
]
```

If you see your public key hash, Signatory is correctly configured!

**Check logs for errors:**
```bash
journalctl --user -u octez-signatory-dev-signer -f
```

### Step 5: Install Baker with Remote Signer

Now install the baker, configuring it to use Signatory for signing operations.

**Via TUI:**
1. In octez-manager, select **[ Install new instance ]** → **Baker**
2. Configure:
   - **Node**: Select your running node instance
   - **Instance name**: e.g., `baker-shadownet`
   - **Delegates**: Your baker's public key hash (from Step 2)
   - **Liquidity baking vote**: Choose `on`, `off`, or `pass`
   - **Remote signer**: Select your Signatory instance (`dev-signer`)

**Via CLI:**
```bash
octez-manager install-baker \
  --instance baker-shadownet \
  --node-instance shadownet \
  --delegate tz1abc123... \
  --liquidity-baking-vote pass \
  --remote-signer dev-signer
```

This creates a baker service configured to send signing requests to Signatory instead of using local keys.

### Step 6: Verify Baker Connectivity

After installation, verify the baker can connect to Signatory and is operating correctly.

**Check baker service status via TUI:**
1. Launch `octez-manager`
2. Navigate to your baker instance
3. Verify status shows "Running"
4. Check logs for connection messages

**Or via CLI:**
```bash
systemctl --user status octez-baker-baker-shadownet
```

Expected: `active (running)`

**Check baker logs:**
```bash
journalctl --user -u octez-baker-baker-shadownet -f
```

Look for:
- ✅ "Connected to node"
- ✅ Signing requests (blocks, attestations)
- ❌ No "Failed to connect to remote signer" errors

**Check Signatory logs:**
```bash
journalctl --user -u octez-signatory-dev-signer -f
```

Look for:
- ✅ Incoming signing requests from baker
- ✅ "Authorized" messages for your delegate
- ❌ No "Key not authorized" rejections

If you see these messages, **your baker is successfully using Signatory!**

For monitoring and maintenance procedures, see:
- [Signatory Setup Guide - Monitoring and Maintenance](/guides/signatory-setup/#monitoring-and-maintenance)
- [Baker Setup Guide](/guides/baker-setup/)

## Troubleshooting Common Issues

### Baker Can't Connect to Signatory

**Symptoms:**
```
Error: Failed to connect to remote signer at http://127.0.0.1:6732
```

**Solutions:**
1. Verify Signatory is running: `systemctl --user status octez-signatory-dev-signer`
2. Check port is listening: `ss -tlnp | grep 6732`
3. Test endpoint: `curl http://127.0.0.1:6732/keys`
4. Review Signatory logs: `journalctl --user -u octez-signatory-dev-signer -f`

### Key Not Authorized

**Symptoms:**
```
Signatory log: "Request rejected: key tz1xyz... not authorized"
```

**Solutions:**
1. Verify authorized keys match baker's delegate:
   ```bash
   # Check baker's delegate
   systemctl --user cat octez-baker-baker-shadownet | grep delegate
   
   # Check Signatory's authorized keys
   systemctl --user cat octez-signatory-dev-signer | grep authorized-keys
   ```
2. If mismatch, update Signatory:
   ```bash
   octez-manager install-signatory \
     --instance dev-signer \
     --backend file \
     --authorized-keys tz1CORRECT_HASH... \
     --preserve-data
   ```

### Signing Timeout / High Latency

**Symptoms:**
```
Baker log: "Timeout waiting for signature from remote signer"
```

**Solutions:**
1. Check Signatory request latency:
   ```bash
   # Enable metrics and check duration
   curl http://127.0.0.1:9090/metrics | grep signatory_request_duration
   ```
2. Monitor system load: `top`, `htop`
3. For cloud KMS: Check network connectivity and API quotas
4. Consider co-locating baker and Signatory on same host

### Keys Not Found

**Symptoms:**
```
Signatory log: "No keys found in directory"
curl http://127.0.0.1:6732/keys returns []
```

**Solutions:**
1. Verify keys directory exists and has keys:
   ```bash
   ls -la ~/.local/share/octez/signatory/dev-signer/keys
   ```
2. Import keys if missing (see Step 4)
3. Check permissions: `chmod 600 ~/.local/share/octez/signatory/dev-signer/keys/*`
4. Verify service user has read access

### Double Baking / Slashing Warning

**Symptoms:**
```
Signatory log: "Watermark check failed: level already signed"
```

**Explanation:**
This is a **safety feature** preventing double-baking (which results in slashing). Signatory refused to sign because it would create a duplicate signature at the same block level.

**What NOT to do:**
- ❌ Reset watermark without investigation
- ❌ Run multiple bakers with same keys
- ❌ Disable watermark checks

**What to do:**
1. Verify only ONE baker is running with these keys:
   ```bash
   systemctl --user list-units | grep octez-baker
   ```
2. Check for duplicate baker processes:
   ```bash
   ps aux | grep octez-baker
   ```
3. Review Signatory logs to identify the source of duplicate requests
4. If intentional baker restart, watermark should continue from previous state (file/database watermark)

**If watermark reset is truly necessary** (e.g., keys used on different chain):
```bash
# WARNING: Only do this if you're certain no double-signing risk exists
# For file watermark:
rm ~/.local/share/octez/signatory/dev-signer/watermark.db
systemctl --user restart octez-signatory-dev-signer
```

## Advanced Topics

For advanced configurations (multiple delegates, remote Signatory, DAL integration, high availability, performance tuning), see:
- [Signatory Setup Guide - Advanced Configurations](/guides/signatory-setup/)
- [Baker Setup Guide](/guides/baker-setup/)

## Security Checklist for Production

Before deploying to mainnet with real stake:

- [ ] **Keys in HSM**: YubiHSM, cloud HSM, or equivalent (NOT file-based)
- [ ] **TLS enabled**: All baker-signatory communication encrypted
- [ ] **Firewall configured**: Minimal necessary access
- [ ] **Authorized keys set**: Only known baker keys authorized
- [ ] **Watermark persisted**: File or database-backed (NOT memory)
- [ ] **Backups tested**: Key backup and recovery procedures verified
- [ ] **Monitoring active**: Alerting on Signatory/baker unavailability
- [ ] **Logs reviewed**: Regular audit log reviews
- [ ] **Updates planned**: Process for applying security updates
- [ ] **Disaster recovery documented**: Procedures for key compromise or hardware failure
- [ ] **Test deployment**: Successful testnet baking with identical setup

## Next Steps

- **Learn more about Signatory**: See [Signatory Setup Guide](/guides/signatory-setup/)
- **Optimize your baker**: See [Baker Setup Guide](/guides/baker-setup/)
- **CLI reference**: See [CLI Reference](/reference/cli/) for automation

## External Resources

- [Signatory Documentation](https://signatory.io/docs/) - Official docs
- [Octez Remote Signer Guide](https://octez.tezos.com/docs/introduction/remote_signer.html) - Tezos documentation
- [Baking on Tezos](https://octez.tezos.com/docs/introduction/howtorun.html#running-a-delegate) - General baking guide
- [Tezos Agora](https://www.tezosagora.org/) - Governance and baking discussions

---

*Last updated: February 2026*
