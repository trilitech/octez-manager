#!/usr/bin/env python3
"""Simple HTTP server for snapshot and teztnets.json"""

import http.server
import json
from pathlib import Path

HTTP_DIR = Path("/srv/http")
PORT = 8080


class Handler(http.server.SimpleHTTPRequestHandler):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory=str(HTTP_DIR), **kwargs)

    def do_GET(self):
        if self.path == "/health":
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps({"status": "ready"}).encode())
        else:
            super().do_GET()

    def log_message(self, format, *args):
        print(f"[http] {args[0]}")


if __name__ == "__main__":
    print(f"[http] Serving {HTTP_DIR} on port {PORT}")
    http.server.HTTPServer(("0.0.0.0", PORT), Handler).serve_forever()
