# Octez sandbox for integration testing
# Serves a fresh testnet snapshot via HTTP (downloaded at startup)

FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    python3 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /srv/http

# Copy HTTP server and entrypoint
COPY sandbox/serve.py /serve.py
COPY sandbox/entrypoint.sh /entrypoint.sh
COPY sandbox/teztnets.json /srv/http/teztnets.json

RUN chmod +x /entrypoint.sh

EXPOSE 8080

HEALTHCHECK --interval=5s --timeout=3s --start-period=60s \
    CMD curl -sf http://localhost:8080/health || exit 1

CMD ["/entrypoint.sh"]
