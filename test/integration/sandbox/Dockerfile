# Octez sandbox for integration testing
# Serves a pre-downloaded testnet snapshot via HTTP

FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    python3 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Download tallinnnet rolling snapshot (~42MB)
RUN mkdir -p /srv/http \
    && curl -sfL https://snapshots.eu.tzinit.org/tallinnnet/rolling \
       -o /srv/http/snapshot.rolling

# Copy HTTP server
COPY sandbox/serve.py /serve.py
COPY sandbox/teztnets.json /srv/http/teztnets.json

EXPOSE 8080

HEALTHCHECK --interval=5s --timeout=3s --start-period=5s \
    CMD curl -sf http://localhost:8080/health || exit 1

CMD ["python3", "/serve.py"]
