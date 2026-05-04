---
title: Signatory Setup Guide
description: Comprehensive guide to setting up Octez Signatory for secure key management
---

This guide covers setting up [Signatory](https://github.com/ecadlabs/signatory), a remote signing service for Tezos. Signatory separates key management from your baker, improving security by isolating private keys in a dedicated service.

## What is Signatory?

Signatory is a remote signer that enables bakers to keep their private keys separate from their baking infrastructure. Instead of storing keys directly in your baker's configuration, the baker sends signing requests to Signatory over HTTP.

### Benefits

- **Security isolation**: Keys never leave the signing service
- **Hardware security module (HSM) support**: YubiHSM integration for production deployments
- **Cloud KMS support**: AWS KMS, Azure Key Vault, Google Cloud KMS
- **Flexible deployment**: Run locally for development or remotely for production
- **Multi-baker support**: One Signatory instance can serve multiple bakers

### When to Use Signatory

| Scenario | Recommended Approach |
|----------|---------------------|
| **Testing/Development** | File-based Signatory (as described in this guide) |
| **Production** | Hardware-backed Signatory (YubiHSM) or Cloud KMS<br/>See [Signatory documentation](https://signatory.io/docs/) for setup |

## Prerequisites

- **Running Tezos node**: See [Setting Up a Node](/guides/node-setup/)
- **Signatory binary**: Install via octez-manager (see below) or [download manually](https://github.com/ecadlabs/signatory/releases)
- **Baker keys**: Generated or imported Tezos keys

## Quick Start: File-Based Signatory

The simplest setup uses file-based key storage. This is suitable for development and testing, but **not recommended for production** as keys are stored unencrypted on disk.

### Installation via TUI

1. Launch `octez-manager`
2. Select **[ Install new instance ]** → **Signatory**
3. Configure:
   - **Instance name**: e.g., `dev-signer`
   - **Backend**: `file`
   - **Keys directory**: Default or custom path
   - **Authorized keys**: Your baker's public key hash(es)
   - **Address**: `127.0.0.1:6732` (default)

### Installation via CLI

```bash
octez-manager install-signatory \
  --instance dev-signer \
  --backend file \
  --authorized-keys tz1abc123... \
  --signatory-version latest
```

This creates a systemd service `signatory@dev-signer` that starts automatically.

### Adding Keys to Signatory

After installation, you need to import your baker keys into Signatory. The keys directory location depends on your setup:

**User mode** (default):
```bash
KEY_DIR=~/.local/share/octez/signatory/dev-signer/keys
```

**System mode** (when running as root/sudo):
```bash
KEY_DIR=/var/lib/octez/signatory/dev-signer/keys
```

#### Option 1: Copy Existing Keys

If you already have keys in `octez-client`:

```bash
# Find your key files (look for .sk files)
ls ~/.tezos-client/

# Copy the secret key file to Signatory's keys directory
cp ~/.tezos-client/secret_keys $KEY_DIR/

# Ensure proper permissions
chmod 600 $KEY_DIR/secret_keys
```

#### Option 2: Generate New Keys in Signatory

```bash
# Create keys directory if it doesn't exist
mkdir -p $KEY_DIR

# Use octez-client to generate and save to Signatory's directory
octez-client -d $KEY_DIR gen keys my-baker
```

#### Option 3: Import Key from Secret Key

```bash
octez-client -d $KEY_DIR import secret key my-baker unencrypted:edsk...
```

## Next Steps

- **Set up a baker with Signatory**: See [Baker Setup Guide](/guides/baker-setup/)
- **CLI reference**: Detailed command documentation in [CLI Reference](/reference/cli/)
- **Official Signatory docs**: [https://signatory.io/docs/](https://signatory.io/docs/)
- **Signatory GitHub**: [https://github.com/ecadlabs/signatory](https://github.com/ecadlabs/signatory)

## External Resources

- [Signatory Documentation](https://signatory.io/docs/) - Official Signatory documentation (includes HSM and cloud KMS setup)
- [Signatory GitHub](https://github.com/ecadlabs/signatory) - Source code and releases
- [Octez Remote Signer Docs](https://octez.tezos.com/docs/introduction/remote_signer.html) - Tezos documentation on remote signers

---

*Last updated: February 2026*
