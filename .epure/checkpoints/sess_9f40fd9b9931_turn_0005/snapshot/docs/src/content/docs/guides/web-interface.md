---
title: Web Interface
description: Browser-based remote management for Octez Manager
---

The web interface provides browser-based access to Octez Manager, enabling remote management without SSH access. It offers the same functionality as the terminal UI through xterm.js terminal emulation.

## Quick Start

```bash
# Start the web interface
octez-manager web

# With password protection
octez-manager web --password mysecret

# Custom port
octez-manager web --port 8443
```

Then open `http://your-server:8080` in any browser.

## Features

The web interface provides full access to all Octez Manager functionality:

- **Service management** — Install, start, stop, and configure nodes, bakers, accusers, DAL nodes, Signatory, and octez-index
- **Real-time monitoring** — View service status, sync progress, and system metrics
- **Log viewing** — Stream and search logs from any service
- **Binary management** — Download and manage Octez versions
- **RPC browser** — Explore and query node RPC endpoints

## Authentication

### No Authentication (Development)

```bash
octez-manager web
```

Anyone who can reach the server can control your services. Only use this on trusted networks or for local testing.

### Password Protection (Recommended)

```bash
octez-manager web --password mysecret
```

Users must enter the password before gaining access. The password is transmitted over the WebSocket connection.

> **Security:** For production use, place the web interface behind a reverse proxy with HTTPS (nginx, Caddy, etc.) to encrypt the connection.

### Controller and Viewer Roles

The web interface supports two access levels:

| Role | Access | URL |
|------|--------|-----|
| **Controller** | Full control (read/write) | `http://server:8080/` |
| **Viewer** | Read-only (observe only) | `http://server:8080/viewer` |

To set separate passwords:

```bash
octez-manager web --password admin123 --viewer-password viewer123
```

- Controller password grants full access
- Viewer password grants read-only access
- Only one controller can be connected at a time
- Multiple viewers can observe simultaneously

### Environment Variables

Passwords can be set via environment variables:

```bash
export MIAOU_WEB_PASSWORD=admin123
export MIAOU_WEB_VIEWER_PASSWORD=viewer123
octez-manager web
```

This is useful for systemd services or container deployments where command-line passwords might be visible in process listings.

## Running as a Service

To run the web interface as a systemd service:

```ini
# /etc/systemd/system/octez-manager-web.service
[Unit]
Description=Octez Manager Web Interface
After=network.target

[Service]
Type=simple
User=root
Environment=MIAOU_WEB_PASSWORD=your-secure-password
ExecStart=/usr/local/bin/octez-manager web --port 8080
Restart=on-failure

[Install]
WantedBy=multi-user.target
```

```bash
sudo systemctl daemon-reload
sudo systemctl enable octez-manager-web
sudo systemctl start octez-manager-web
```

## Reverse Proxy with HTTPS

For production deployments, use a reverse proxy to add HTTPS encryption.

### Nginx Example

```nginx
server {
    listen 443 ssl;
    server_name octez.example.com;

    ssl_certificate /etc/letsencrypt/live/octez.example.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/octez.example.com/privkey.pem;

    location / {
        proxy_pass http://127.0.0.1:8080;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }
}
```

### Caddy Example

```
octez.example.com {
    reverse_proxy 127.0.0.1:8080
}
```

Caddy automatically handles HTTPS certificates.

## Keyboard Shortcuts

The web interface supports the same keyboard shortcuts as the TUI:

| Key | Action |
|-----|--------|
| `↑`/`↓` | Navigate list |
| `Enter` | Select / Open action menu |
| `Tab` | Fold/unfold instance details |
| `b` | Open Binaries page |
| `d` | Open Diagnostics page |
| `r` | Open RPC Browser |
| `?` | Show help |
| `Esc` | Go back / Close modal |

> **Note:** `q` (quit) is disabled in the web interface to prevent accidental disconnection.

## Troubleshooting

### Connection Refused

- Ensure the web server is running: `octez-manager web`
- Check the port is correct (default: 8080)
- Verify firewall rules allow the port

### WebSocket Connection Failed

- The web interface requires WebSocket support
- Check that your reverse proxy is configured for WebSocket upgrades
- Some corporate firewalls block WebSocket connections

### Terminal Size Issues

The terminal automatically fits to your browser window. If the display looks wrong:
- Try resizing the browser window
- Refresh the page
- Check browser zoom level (100% works best)
