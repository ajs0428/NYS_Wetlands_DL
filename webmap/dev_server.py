#!/usr/bin/env python3
"""
Static file server with HTTP Range support, for local COG viewer development.

Python's stock `http.server` ignores Range headers, but COG streaming
(geotiff.js under georaster) requires 206 partial responses — without them the
browser downloads whole multi-hundred-MB rasters or fails outright. Stdlib
only; no dependencies.

Usage (on the HPC login node):
  python3 webmap/dev_server.py --root /ibstorage/anthony/NYS_Wetlands_DL --port 8787
Then from a local machine:
  ssh -L 8787:localhost:8787 <user>@cbsuxu10.biohpc.cornell.edu
  open http://localhost:8787/webmap/viewer/
"""
from __future__ import annotations

import argparse
import os
import re
from functools import partial
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer

RANGE_RE = re.compile(r"bytes=(\d*)-(\d*)")


class RangeHandler(SimpleHTTPRequestHandler):
    """SimpleHTTPRequestHandler + single-range GET (RFC 7233) + CORS."""

    # Keep-alive: geotiff.js fetches COG blocks as many small range requests;
    # under the default HTTP/1.0 each one costs a new TCP connection (slow
    # through an ssh tunnel). Safe because every code path sets Content-Length.
    protocol_version = "HTTP/1.1"

    def end_headers(self):
        self.send_header("Accept-Ranges", "bytes")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Headers", "Range")
        self.send_header("Access-Control-Expose-Headers",
                         "Content-Range, Accept-Ranges, Content-Length")
        # never let the browser cache viewer files during development
        base = self.path.split("?", 1)[0]
        if base.endswith((".html", ".js", ".css", ".json")) or base.endswith("/"):
            self.send_header("Cache-Control", "no-store")
        super().end_headers()

    def send_head(self):
        range_header = self.headers.get("Range")
        if range_header is None:
            return super().send_head()

        path = self.translate_path(self.path)
        if os.path.isdir(path) or not os.path.exists(path):
            return super().send_head()

        # Multi-range ("bytes=a-b,c-d") isn't implemented; per RFC 7233 a
        # server may ignore Range and send the full file (200) instead —
        # never truncate to just the first range.
        m = RANGE_RE.match(range_header)
        if not m or "," in range_header:
            return super().send_head()

        size = os.path.getsize(path)
        start_s, end_s = m.groups()
        if start_s == "":
            # suffix range: last N bytes
            length = int(end_s)
            start, end = max(0, size - length), size - 1
        else:
            start = int(start_s)
            end = int(end_s) if end_s else size - 1
        end = min(end, size - 1)

        if start > end or start >= size:
            self.send_error(416, "Requested Range Not Satisfiable")
            return None

        f = open(path, "rb")
        f.seek(start)
        self.range_remaining = end - start + 1
        self.send_response(206)
        self.send_header("Content-Type", self.guess_type(path))
        self.send_header("Content-Range", f"bytes {start}-{end}/{size}")
        self.send_header("Content-Length", str(self.range_remaining))
        self.end_headers()
        return f

    def copyfile(self, source, outputfile):
        remaining = getattr(self, "range_remaining", None)
        if remaining is None:
            return super().copyfile(source, outputfile)
        while remaining > 0:
            chunk = source.read(min(64 * 1024, remaining))
            if not chunk:
                break
            outputfile.write(chunk)
            remaining -= len(chunk)
        self.range_remaining = None

    def log_message(self, fmt, *args):
        # keep errors; keep .tif traffic (so COG streaming is observable);
        # skip the rest of the successful chatter
        line = str(args[0]) if args else ""
        status = str(args[1]) if len(args) > 1 else ""
        if status.startswith(("2", "3")) and ".tif" not in line:
            return
        rng = self.headers.get("Range", "") if hasattr(self, "headers") else ""
        super().log_message(fmt + (f"  [Range: {rng}]" if rng else ""), *args)


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[1])
    ap.add_argument("--root", default=".", help="directory to serve (repo root)")
    ap.add_argument("--port", type=int, default=8787)
    ap.add_argument("--bind", default="127.0.0.1",
                    help="bind address (default localhost-only; use an ssh tunnel)")
    args = ap.parse_args()

    handler = partial(RangeHandler, directory=os.path.abspath(args.root))
    with ThreadingHTTPServer((args.bind, args.port), handler) as httpd:
        print(f"Serving {os.path.abspath(args.root)} at http://{args.bind}:{args.port}")
        print(f"Viewer: http://{args.bind}:{args.port}/webmap/viewer/")
        httpd.serve_forever()


if __name__ == "__main__":
    main()
