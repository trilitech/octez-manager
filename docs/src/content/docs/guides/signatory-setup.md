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
| **Testing/Development** | File-based Signatory or local keys |
| **Production (Small)** | YubiHSM-backed Signatory |
| **Production (Large/Cloud)** | Cloud KMS-backed Signatory |
| **High Security Requirements** | Hardware-backed Signatory (YubiHSM or cloud HSM) |

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

This creates a systemd service `octez-signatory-dev-signer` that starts automatically.

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
systemctl --user status octez-signatory-dev-signer

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

### YubiHSM Backend (Hardware Security)

**Security Level**: High (keys stored in tamper-resistant hardware)  
**Cost**: ~$650 USD (YubiHSM 2 device)  
**Complexity**: Medium  
**Use Case**: Production baking, high-security requirements

YubiHSM provides hardware-based key storage where private keys never leave the device.

#### Prerequisites

1. **YubiHSM 2 device**: [Purchase from Yubico](https://www.yubico.com/product/yubihsm-2/)
2. **YubiHSM connector**: Software that provides USB access to the device
3. **Initial HSM setup**: Device must be initialized with authentication keys

#### Installation

```bash
# Install YubiHSM connector (platform-dependent)
# Ubuntu/Debian:
sudo apt install yubihsm-connector

# macOS:
brew install yubihsm-connector

# Start the connector
sudo systemctl start yubihsm-connector
```

#### Signatory Setup with YubiHSM

Currently, YubiHSM backend requires manual configuration of the Signatory config file. Support for `--backend yubihsm` in octez-manager is planned for a future release.

**Manual configuration** (`~/.config/octez-manager/signatory/my-signer.yaml`):

```yaml
server:
  address: 127.0.0.1:6732
  utility_address: 127.0.0.1:9583

yubihsm:
  address: 127.0.0.1:12345  # YubiHSM connector address
  password: "password"       # HSM authentication key password
  auth_key_id: 1            # Authentication key ID in HSM

policy:
  authorized_keys:
    - tz1abc...: ["block", "attestation", "preattestation"]
```

See [Signatory YubiHSM documentation](https://signatory.io/docs/yubihsm) for detailed setup instructions.

### Cloud KMS Backends (Production Cloud)

**Security Level**: High (keys managed by cloud HSM)  
**Cost**: Variable (cloud provider pricing)  
**Complexity**: Medium-High  
**Use Case**: Cloud-based production deployments, scalable infrastructure

Cloud KMS backends are planned for future releases of octez-manager. Currently, these require manual Signatory configuration.

#### AWS KMS

**Prerequisites**:
- AWS account with KMS access
- IAM permissions for KMS operations
- DynamoDB table for watermark storage (optional but recommended)

**Manual configuration example**:

```yaml
server:
  address: 0.0.0.0:6732

aws:
  region: us-east-1
  access_key_id: AKIA...
  secret_access_key: secret...

watermark:
  type: dynamodb
  table: signatory-watermarks

policy:
  authorized_keys:
    - tz1prod...: ["block", "attestation"]
```

See [Signatory AWS documentation](https://signatory.io/docs/aws) for details.

#### Azure Key Vault

**Prerequisites**:
- Azure subscription
- Key Vault instance
- Managed identity or service principal with Key Vault permissions

**Manual configuration example**:

```yaml
server:
  address: 0.0.0.0:6732

azure:
  vault: https://my-vault.vault.azure.net/
  tenant_id: ...
  client_id: ...
  client_secret: ...

policy:
  authorized_keys:
    - tz1prod...: ["block", "attestation"]
```

See [Signatory Azure documentation](https://signatory.io/docs/azure) for details.

#### Google Cloud KMS

**Prerequisites**:
- Google Cloud project
- Cloud KMS key ring and keys
- Service account with KMS permissions
- Firestore for watermark storage (optional)

**Manual configuration example**:

```yaml
server:
  address: 0.0.0.0:6732

gcp:
  project: my-project
  location: us-east1
  key_ring: tezos-keys
  credentials_file: /path/to/service-account.json

watermark:
  type: firestore
  collection: signatory-watermarks

policy:
  authorized_keys:
    - tz1prod...: ["block", "attestation"]
```

See [Signatory GCP documentation](https://signatory.io/docs/gcp) for details.

## Security Best Practices

### Key Management

1. **Generate keys securely**: Use hardware RNGs or HSMs when possible
2. **Never share private keys**: Keys should exist in exactly one location
3. **Backup carefully**: Encrypted backups stored offline, test recovery procedures
4. **Rotate keys periodically**: Establish key rotation procedures (requires re-registration with Tezos network)
5. **Use multi-signature**: For high-value accounts, consider multi-signature setups

### Network Security

#### TLS/HTTPS Configuration

For production deployments, always use TLS to encrypt communication between baker and Signatory:

```bash
# Generate self-signed certificate (development)
openssl req -x509 -newkey rsa:4096 -nodes \
  -keyout signatory-key.pem \
  -out signatory-cert.pem \
  -days 365 \
  -subj "/CN=signatory.local"

# Configure Signatory to use TLS (manual config required)
# Edit ~/.config/octez-manager/signatory/<instance>.yaml:
server:
  address: 0.0.0.0:6732
  tls:
    certificate: /path/to/signatory-cert.pem
    key: /path/to/signatory-key.pem
```

For production, use certificates from a trusted CA (Let's Encrypt, etc.).

#### Firewall Rules

Restrict access to Signatory's HTTP port:

```bash
# Only allow connections from baker's IP
sudo ufw allow from 192.168.1.100 to any port 6732 proto tcp

# Or for local-only access
sudo ufw deny 6732
# (systemd socket activation handles local connections)
```

#### SSH Tunneling (Development)

For remote development, use SSH tunnels instead of exposing Signatory ports:

```bash
# On your local machine
ssh -L 6732:localhost:6732 user@remote-signatory-host

# Your local baker can now connect to localhost:6732
```

### Access Control

#### Authorized Keys Management

Signatory's `--authorized-keys` parameter restricts which public key hashes can request signatures:

```bash
# Single key with all permissions (default)
--authorized-keys tz1abc123...

# Multiple keys
--authorized-keys tz1abc123... tz2def456...

# With specific operation permissions
--authorized-keys "tz1abc:block,attestation tz2def:generic"
```

**Operation types**:
- `block`: Block production
- `attestation`: Attestations (formerly endorsements)
- `preattestation`: Pre-attestations
- `attestation_with_dal`: DAL-enabled attestations
- `generic`: Generic signing operations

#### File System Permissions

Ensure only the service user can access key files:

```bash
# Set restrictive permissions on keys directory
chmod 700 ~/.local/share/octez/signatory/*/keys
chmod 600 ~/.local/share/octez/signatory/*/keys/*

# Verify ownership
ls -la ~/.local/share/octez/signatory/*/keys
```

#### Audit Logging

Enable Signatory logging to monitor signing requests:

```bash
# View Signatory logs
journalctl --user -u octez-signatory-dev-signer -f

# Logs include:
# - Incoming signing requests
# - Authorized/rejected requests
# - Signed operation hashes
```

### Production Deployment Checklist

Before deploying Signatory for mainnet baking:

- [ ] **Keys stored in HSM**: YubiHSM, cloud HSM, or equivalent
- [ ] **TLS enabled**: All baker-signatory communication encrypted
- [ ] **Firewall configured**: Minimal access, principle of least privilege
- [ ] **Authorized keys configured**: Only known baker keys allowed
- [ ] **Watermark enabled**: Prevents double-signing (file or database-backed)
- [ ] **Backups tested**: Key backup and recovery procedures verified
- [ ] **Monitoring enabled**: Alerting on Signatory unavailability
- [ ] **Logs reviewed**: Regular audit log reviews
- [ ] **Disaster recovery plan**: Documented procedures for key compromise or hardware failure
- [ ] **High availability**: Consider redundant Signatory instances (advanced setup)

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
systemctl --user status octez-signatory-dev-signer

# View recent logs
journalctl --user -u octez-signatory-dev-signer --since "1 hour ago"
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
- **YubiHSM**: 20-50ms
- **Cloud KMS**: 50-200ms (network-dependent)

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
1. Check service status: `systemctl --user status octez-signatory-<instance>`
2. Verify port is listening: `ss -tlnp | grep 6732`
3. Check Signatory logs: `journalctl --user -u octez-signatory-<instance> -f`
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
3. For cloud KMS: Verify network connectivity and API quotas
4. For YubiHSM: Ensure connector is running and responsive
5. Consider co-locating baker and Signatory

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

- [Signatory Documentation](https://signatory.io/docs/) - Official Signatory documentation
- [Signatory GitHub](https://github.com/ecadlabs/signatory) - Source code and releases
- [Octez Remote Signer Docs](https://octez.tezos.com/docs/introduction/remote_signer.html) - Tezos documentation on remote signers
- [YubiHSM 2 Documentation](https://developers.yubico.com/YubiHSM2/) - YubiHSM setup and usage
- [AWS KMS Documentation](https://docs.aws.amazon.com/kms/) - AWS KMS setup
- [Azure Key Vault Documentation](https://docs.microsoft.com/en-us/azure/key-vault/) - Azure Key Vault setup
- [Google Cloud KMS Documentation](https://cloud.google.com/kms/docs) - GCP KMS setup

---

*Last updated: February 2026*
