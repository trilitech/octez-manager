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

### Verifying the Setup

Check that Signatory is running and recognizes your keys:

```bash
# Check service status
systemctl --user status signatory@dev-signer

# Test Signatory endpoint (should return public keys)
curl http://127.0.0.1:6732/keys
```

Expected output:
```json
[
  {
    "public_key": "edpk...",
    "hash": "tz1abc123..."
  }
]
```

## Backend Options

Signatory supports multiple backends for key storage. Choose based on your security requirements and infrastructure:

### File Backend (Development Only)

**Security Level**: Low (keys stored unencrypted on disk)  
**Cost**: Free  
**Complexity**: Low  
**Use Case**: Development, testing, non-production environments

```bash
octez-manager install-signatory \
  --instance dev-signer \
  --backend file \
  --keys-dir /home/tezos/.tezos-signatory/keys \
  --authorized-keys tz1abc...,tz2def... \
  --service-user tezos
```

**Security Warnings**:
- Keys are stored as plain text files
- Anyone with file system access can read the keys
- No protection against key extraction
- **Never use for mainnet baking with significant stake**

For production deployments with hardware security modules (HSMs) or cloud KMS backends, see the [official Signatory documentation](https://signatory.io/docs/).

## Watermark Storage

Watermarks prevent double-signing by recording the highest block/attestation level signed. Signatory must never sign two different operations at the same level (this would be slashable).

### Memory Watermark (Default)

```bash
octez-manager install-signatory \
  --instance my-signer \
  --watermark memory
```

**Pros**: Simple, no external dependencies  
**Cons**: Lost on restart, not suitable for multi-instance setups

### File Watermark

```bash
octez-manager install-signatory \
  --instance my-signer \
  --watermark file
```

Watermarks persist to disk at `~/.local/share/octez/signatory/<instance>/watermark.db`.

**Pros**: Survives restarts  
**Cons**: Not suitable for multi-instance/HA setups

### Database Watermark (Production)

For production multi-instance setups, use a shared database (DynamoDB, Firestore, etc.). This requires manual Signatory configuration.

See [Signatory watermark documentation](https://signatory.io/docs/watermark) for details.

## Monitoring and Maintenance

### Health Checks

Monitor Signatory availability:

```bash
# Check HTTP health
curl http://127.0.0.1:6732/keys

# Check service status
systemctl --user status signatory@dev-signer

# View recent logs
journalctl --user -u signatory@dev-signer --since "1 hour ago"
```

### Metrics

Signatory can expose Prometheus metrics:

```bash
octez-manager install-signatory \
  --instance my-signer \
  --metrics-address 127.0.0.1:9090
```

Useful metrics:
- `signatory_requests_total`: Total signing requests
- `signatory_requests_authorized`: Successful signings
- `signatory_requests_rejected`: Rejected requests
- `signatory_request_duration_seconds`: Request latency

### Performance Tuning

#### Latency Optimization

Baking requires low-latency signing. To minimize latency:

1. **Co-locate services**: Run Signatory on the same machine as the baker
2. **Use local sockets**: Consider Unix domain sockets instead of TCP (requires manual config)
3. **Reduce network hops**: Avoid remote Signatory for time-critical operations
4. **Monitor latency**: Track `signatory_request_duration_seconds` metric

Typical signing latencies:
- **File backend**: <10ms
- **Hardware/cloud backends**: See [Signatory performance docs](https://signatory.io/docs/) for HSM and KMS latencies

#### High Availability

For production baking, consider redundant Signatory instances:

1. **Primary/standby setup**: Hot standby Signatory with shared watermark database
2. **Load balancing**: Multiple Signatory instances behind a load balancer (requires careful watermark management)
3. **Automated failover**: Baker configured with multiple Signatory endpoints

**Warning**: HA setups require shared watermark storage to prevent double-signing. Never run multiple Signatory instances with memory or file watermarks.

## Troubleshooting

### Signatory Not Responding

**Symptoms**: Baker logs "Failed to connect to remote signer"

**Solutions**:
1. Check service status: `systemctl --user status signatory@<instance>`
2. Verify port is listening: `ss -tlnp | grep 6732`
3. Check Signatory logs: `journalctl --user -u signatory@<instance> -f`
4. Test endpoint: `curl http://127.0.0.1:6732/keys`

### Baker Can't Connect to Signer

**Symptoms**: Baker errors "Connection refused" or "Timeout"

**Solutions**:
1. Verify address configuration matches baker's remote signer URL
2. Check firewall rules: `sudo ufw status`
3. Test connectivity: `telnet <signatory-host> 6732`
4. Ensure Signatory is bound to correct interface (`0.0.0.0` for remote access)

### Permission Denied Errors

**Symptoms**: "Permission denied" in Signatory logs

**Solutions**:
1. Check keys directory permissions: `ls -la ~/.local/share/octez/signatory/*/keys`
2. Verify service user ownership: `chown -R <service-user> <keys-dir>`
3. Ensure keys are readable: `chmod 600 <keys-dir>/*`

### Key Not Authorized

**Symptoms**: "Key not authorized" in Signatory logs

**Solutions**:
1. Verify authorized keys configuration: Check `--authorized-keys` matches baker's public key hash
2. List Signatory's known keys: `curl http://127.0.0.1:6732/keys`
3. Verify baker's delegate address matches an authorized key
4. Check for typos in public key hashes (tz1/tz2/tz3 prefixes)

### High Latency / Timeouts

**Symptoms**: Baker reports slow signing or timeouts

**Solutions**:
1. Check Signatory metrics: `curl http://127.0.0.1:9090/metrics | grep signatory_request_duration`
2. Monitor system load: `top`, `htop`
3. Consider co-locating baker and Signatory
4. For production setups (HSM/cloud KMS), see [Signatory performance tuning docs](https://signatory.io/docs/)

### Watermark Issues

**Symptoms**: "Level already signed" or "Watermark too high"

**Solutions**:
1. Check watermark file integrity (file backend)
2. Verify database connectivity (database backend)
3. Review Signatory logs for watermark updates
4. If necessary, manually reset watermark (requires careful consideration to avoid double-signing)

**Warning**: Resetting watermarks can lead to double-signing and slashing. Only reset if you're certain no operations were actually signed at the disputed level.

### Certificate/TLS Errors

**Symptoms**: "Certificate verification failed" or "TLS handshake error"

**Solutions**:
1. Verify certificate validity: `openssl x509 -in cert.pem -noout -dates`
2. Check certificate matches hostname
3. Ensure baker trusts Signatory's CA (for self-signed certs)
4. Review TLS configuration in Signatory config file

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
