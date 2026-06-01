#!/usr/bin/env python3
"""Lightweight patch viewer for NYS wetland training patches.

Serves a tiny web app (stdlib http.server only) that overlays each 256x256
GeoTIFF patch on a Leaflet basemap (Esri World Imagery), so you can skim
through the training data and inspect any band or RGB composite.

Usage
-----
    python dl_patch_viewer.py                       # serve Data/Training_Data/R_Patches
    python dl_patch_viewer.py --patch-dir <dir>     # custom patch directory
    python dl_patch_viewer.py --port 8000 --no-open

Then browse to http://localhost:8000 . Use the dropdown or the
prev/next buttons (or the left/right arrow keys) to skim patches, and the
render-mode selector to switch bands/composites.

No new dependencies: uses rasterio, numpy, matplotlib, pyproj + stdlib only.
"""
from __future__ import annotations

import argparse
import io
import json
import os
import threading
import webbrowser
from functools import lru_cache
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import urlparse, parse_qs, unquote

import numpy as np
import rasterio
import matplotlib

matplotlib.use("Agg")
import matplotlib.cm as cm
import matplotlib.image as mpimg
from pyproj import Transformer

# --------------------------------------------------------------------------
# Configuration / band semantics
# --------------------------------------------------------------------------
SCRIPT_DIR = Path(__file__).resolve().parent

# Class colormap for the label band (matches dl_band_config.json class order).
CLASS_COLORS = {
    0: (33, 145, 140),    # EMW - teal
    1: (94, 201, 98),     # FSW - green
    2: (253, 231, 37),    # SSW - yellow
    3: (140, 81, 50),     # UPL - brown
}
CLASS_NAMES = {0: "EMW", 1: "FSW", 2: "SSW", 3: "UPL", 255: "unlabeled"}

# RGB composites: (mode_key, label, [band_name_r, band_name_g, band_name_b], scale_max)
# scale_max=255 -> NAIP 8-bit reflectance; None -> per-band 2-98 percentile stretch.
COMPOSITES = [
    ("rgb", "NAIP RGB (leaf-on)", ["r", "g", "b"], 255.0),
    ("cir", "NAIP false-color IR (leaf-on)", ["nir", "r", "g"], 255.0),
    ("rgb_lo", "NAIP RGB (leaf-off)", ["r_lo", "g_lo", "b_lo"], 255.0),
    ("cir_lo", "NAIP false-color IR (leaf-off)", ["nir_lo", "r_lo", "g_lo"], 255.0),
]

LABEL_BAND = "MOD_CLASS"

PATCH_DIR = SCRIPT_DIR  # overwritten in main()


# --------------------------------------------------------------------------
# Patch discovery + rendering
# --------------------------------------------------------------------------
def list_patches() -> list[str]:
    return sorted(p.name for p in PATCH_DIR.glob("*.tif"))


@lru_cache(maxsize=64)
def _open_meta(name: str):
    """Return (descriptions, latlon_bounds, count) for a patch, cached."""
    path = PATCH_DIR / name
    with rasterio.open(path) as ds:
        descs = list(ds.descriptions)
        b = ds.bounds
        crs = ds.crs
    # Transform the four corners to WGS84 and take the bounding rectangle.
    tf = Transformer.from_crs(crs, "EPSG:4326", always_xy=True)
    xs = [b.left, b.right, b.right, b.left]
    ys = [b.bottom, b.bottom, b.top, b.top]
    lons, lats = tf.transform(xs, ys)
    latlon = {
        "south": min(lats), "north": max(lats),
        "west": min(lons), "east": max(lons),
    }
    return descs, latlon, len(descs)


def band_index(descs: list[str], band_name: str) -> int | None:
    try:
        return descs.index(band_name) + 1  # rasterio is 1-based
    except ValueError:
        return None


def render_modes(descs: list[str]) -> list[dict]:
    """Available render modes for a patch given its bands."""
    modes: list[dict] = []
    names = set(descs)
    for key, label, bands, _ in COMPOSITES:
        if all(b in names for b in bands):
            modes.append({"key": key, "label": label, "kind": "composite"})
    if LABEL_BAND in names:
        modes.append({"key": "label", "label": "Label (MOD_CLASS)", "kind": "label"})
    for d in descs:
        if d and d != LABEL_BAND:
            modes.append({"key": f"band:{d}", "label": f"Band: {d}", "kind": "band"})
    return modes


def _stretch(arr: np.ndarray, vmax: float | None) -> np.ndarray:
    """Scale a float band to 0..1, ignoring NaN. vmax=255 for fixed 8-bit."""
    out = np.array(arr, dtype=np.float32)
    if vmax is not None:
        return np.clip(out / vmax, 0.0, 1.0)
    valid = out[~np.isnan(out)]
    if valid.size == 0:
        return out
    lo, hi = np.nanpercentile(valid, [2, 98])
    if hi <= lo:
        hi = lo + 1e-6
    return np.clip((out - lo) / (hi - lo), 0.0, 1.0)


def _to_png(rgba: np.ndarray) -> bytes:
    buf = io.BytesIO()
    mpimg.imsave(buf, rgba, format="png")
    return buf.getvalue()


def render_patch(name: str, mode: str) -> bytes:
    """Render the requested mode of a patch to a PNG (NaN -> transparent)."""
    path = PATCH_DIR / name
    descs, _, _ = _open_meta(name)

    with rasterio.open(path) as ds:
        # ---- RGB / false-color composite ----
        comp = next((c for c in COMPOSITES if c[0] == mode), None)
        if comp is not None:
            _, _, bands, vmax = comp
            chans = []
            nan_mask = None
            for bn in bands:
                idx = band_index(descs, bn)
                a = ds.read(idx).astype(np.float32)
                m = np.isnan(a)
                nan_mask = m if nan_mask is None else (nan_mask | m)
                chans.append(_stretch(a, vmax))
            rgb = np.dstack(chans)
            alpha = np.where(nan_mask, 0.0, 1.0)
            rgba = np.dstack([rgb, alpha])
            return _to_png(rgba)

        # ---- Label band: discrete class colors ----
        if mode == "label":
            idx = band_index(descs, LABEL_BAND)
            a = ds.read(idx).astype(np.float32)
            h, w = a.shape
            rgba = np.zeros((h, w, 4), dtype=np.float32)
            for cls, (r, g, b) in CLASS_COLORS.items():
                m = a == cls
                rgba[m] = [r / 255, g / 255, b / 255, 1.0]
            return _to_png(rgba)

        # ---- Single band: viridis with NaN transparent ----
        if mode.startswith("band:"):
            bn = mode.split("band:", 1)[1]
            idx = band_index(descs, bn)
            if idx is None:
                raise KeyError(bn)
            a = ds.read(idx).astype(np.float32)
            nan_mask = np.isnan(a)
            s = _stretch(a, None)
            rgba = cm.viridis(s)  # h x w x 4
            rgba[nan_mask, 3] = 0.0
            return _to_png(rgba.astype(np.float32))

    raise KeyError(mode)


# --------------------------------------------------------------------------
# HTTP handler
# --------------------------------------------------------------------------
class Handler(BaseHTTPRequestHandler):
    def log_message(self, *args):  # quiet
        pass

    def _send(self, code, content_type, body: bytes, cache=False):
        self.send_response(code)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body)))
        if cache:
            self.send_header("Cache-Control", "max-age=3600")
        self.end_headers()
        self.wfile.write(body)

    def _json(self, obj, code=200):
        self._send(code, "application/json", json.dumps(obj).encode())

    def do_GET(self):
        parsed = urlparse(self.path)
        path = parsed.path

        if path == "/":
            self._send(200, "text/html; charset=utf-8", INDEX_HTML.encode())
            return

        if path == "/api/patches":
            self._json({"patches": list_patches()})
            return

        if path == "/api/meta":
            qs = parse_qs(parsed.query)
            name = unquote(qs.get("name", [""])[0])
            if name not in set(list_patches()):
                self._json({"error": "unknown patch"}, 404)
                return
            descs, latlon, count = _open_meta(name)
            self._json({
                "name": name,
                "bounds": latlon,
                "bands": descs,
                "modes": render_modes(descs),
                "class_legend": [
                    {"cls": c, "name": CLASS_NAMES[c],
                     "color": "#%02x%02x%02x" % CLASS_COLORS[c]}
                    for c in sorted(CLASS_COLORS)
                ],
            })
            return

        if path == "/api/render":
            qs = parse_qs(parsed.query)
            name = unquote(qs.get("name", [""])[0])
            mode = unquote(qs.get("mode", ["rgb"])[0])
            if name not in set(list_patches()):
                self._send(404, "text/plain", b"unknown patch")
                return
            try:
                png = render_patch(name, mode)
            except Exception as e:  # noqa: BLE001
                self._send(500, "text/plain", f"render error: {e}".encode())
                return
            self._send(200, "image/png", png, cache=True)
            return

        self._send(404, "text/plain", b"not found")


# --------------------------------------------------------------------------
# Frontend
# --------------------------------------------------------------------------
INDEX_HTML = r"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<title>Wetland Patch Viewer</title>
<link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"/>
<style>
  html, body { margin: 0; height: 100%; font-family: system-ui, sans-serif; }
  #map { position: absolute; top: 0; bottom: 0; left: 0; right: 360px; }
  #panel { position: absolute; top: 0; bottom: 0; right: 0; width: 360px;
           box-sizing: border-box; padding: 14px 16px; overflow-y: auto;
           background: #1d2127; color: #e6e6e6; }
  #panel h1 { font-size: 16px; margin: 0 0 12px; }
  label { display: block; font-size: 12px; margin: 12px 0 4px; color: #9aa4b2; }
  select, input[type=range] { width: 100%; box-sizing: border-box; }
  select { padding: 6px; background: #2a2f38; color: #e6e6e6;
           border: 1px solid #3a414c; border-radius: 6px; }
  .nav { display: flex; gap: 8px; margin-top: 12px; }
  .nav button { flex: 1; padding: 8px; cursor: pointer; font-size: 14px;
                background: #2f6feb; color: #fff; border: none; border-radius: 6px; }
  .nav button:disabled { background: #3a414c; cursor: default; }
  .counter { text-align: center; font-size: 12px; color: #9aa4b2; margin-top: 8px; }
  .legend { margin-top: 16px; }
  .legend .row { display: flex; align-items: center; gap: 8px; font-size: 12px; margin: 4px 0; }
  .legend .sw { width: 14px; height: 14px; border-radius: 3px; border: 1px solid #0006; }
  .opacity-val { float: right; color: #e6e6e6; }
  .meta { margin-top: 16px; font-size: 11px; color: #6b7686; line-height: 1.5; word-break: break-all; }
  .hint { font-size: 11px; color: #6b7686; margin-top: 4px; }
</style>
</head>
<body>
<div id="map"></div>
<div id="panel">
  <h1>🛰️ Wetland Patch Viewer</h1>

  <label for="patch">Patch</label>
  <select id="patch"></select>
  <div class="counter" id="counter"></div>

  <div class="nav">
    <button id="prev">◀ Prev</button>
    <button id="next">Next ▶</button>
  </div>
  <div class="hint">Tip: use ← / → arrow keys to skim.</div>

  <label for="mode">Render mode</label>
  <select id="mode"></select>

  <label for="opacity">Overlay opacity <span class="opacity-val" id="opval">85%</span></label>
  <input type="range" id="opacity" min="0" max="100" value="85"/>

  <div class="legend" id="legend"></div>
  <div class="meta" id="meta"></div>
</div>

<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<script>
const map = L.map('map');
L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {
  maxZoom: 20, attribution: 'Esri World Imagery'
}).addTo(map);
map.setView([42.9, -75.0], 7);

let patches = [];
let idx = 0;
let overlay = null;
let opacity = 0.85;

const $ = id => document.getElementById(id);

async function loadPatchList() {
  const r = await fetch('/api/patches');
  patches = (await r.json()).patches;
  const sel = $('patch');
  sel.innerHTML = '';
  patches.forEach((p, i) => {
    const o = document.createElement('option');
    o.value = i; o.textContent = p;
    sel.appendChild(o);
  });
  if (patches.length) showPatch(0);
}

async function showPatch(i) {
  idx = Math.max(0, Math.min(patches.length - 1, i));
  const name = patches[idx];
  $('patch').value = idx;
  $('counter').textContent = `${idx + 1} / ${patches.length}`;
  $('prev').disabled = idx === 0;
  $('next').disabled = idx === patches.length - 1;

  const meta = await (await fetch('/api/meta?name=' + encodeURIComponent(name))).json();

  // Populate render modes, preserving current selection if still valid.
  const modeSel = $('mode');
  const prev = modeSel.value;
  modeSel.innerHTML = '';
  meta.modes.forEach(m => {
    const o = document.createElement('option');
    o.value = m.key; o.textContent = m.label;
    modeSel.appendChild(o);
  });
  if ([...modeSel.options].some(o => o.value === prev)) modeSel.value = prev;

  // Class legend (shown for label mode).
  const leg = $('legend');
  leg.innerHTML = '<div style="font-size:12px;color:#9aa4b2">Label classes</div>' +
    meta.class_legend.map(c =>
      `<div class="row"><span class="sw" style="background:${c.color}"></span>${c.name}</div>`
    ).join('');

  $('meta').textContent = `bands: ${meta.bands.filter(Boolean).join(', ')}`;

  window._bounds = [[meta.bounds.south, meta.bounds.west],
                    [meta.bounds.north, meta.bounds.east]];
  drawOverlay();
  map.fitBounds(window._bounds, { padding: [40, 40] });
}

function drawOverlay() {
  const name = patches[idx];
  const mode = $('mode').value;
  const url = `/api/render?name=${encodeURIComponent(name)}&mode=${encodeURIComponent(mode)}`;
  if (overlay) map.removeLayer(overlay);
  overlay = L.imageOverlay(url, window._bounds, { opacity }).addTo(map);
}

$('prev').onclick = () => showPatch(idx - 1);
$('next').onclick = () => showPatch(idx + 1);
$('patch').onchange = e => showPatch(parseInt(e.target.value));
$('mode').onchange = drawOverlay;
$('opacity').oninput = e => {
  opacity = e.target.value / 100;
  $('opval').textContent = e.target.value + '%';
  if (overlay) overlay.setOpacity(opacity);
};
document.addEventListener('keydown', e => {
  if (e.target.tagName === 'SELECT') return;
  if (e.key === 'ArrowLeft') showPatch(idx - 1);
  if (e.key === 'ArrowRight') showPatch(idx + 1);
});

loadPatchList();
</script>
</body>
</html>
"""


# --------------------------------------------------------------------------
# Main
# --------------------------------------------------------------------------
def main():
    global PATCH_DIR
    default_dir = (SCRIPT_DIR / "../../Data/Training_Data/R_Patches").resolve()
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--patch-dir", type=Path, default=default_dir,
                    help=f"Directory of *.tif patches (default: {default_dir})")
    ap.add_argument("--port", type=int, default=8000)
    ap.add_argument("--host", default="127.0.0.1")
    ap.add_argument("--no-open", action="store_true",
                    help="Don't auto-open a browser tab")
    args = ap.parse_args()

    PATCH_DIR = args.patch_dir.resolve()
    if not PATCH_DIR.is_dir():
        ap.error(f"patch dir not found: {PATCH_DIR}")
    n = len(list_patches())
    if n == 0:
        ap.error(f"no *.tif patches in {PATCH_DIR}")

    url = f"http://{args.host}:{args.port}"
    print(f"Serving {n} patches from {PATCH_DIR}")
    print(f"Patch viewer running at {url}  (Ctrl-C to stop)")

    server = ThreadingHTTPServer((args.host, args.port), Handler)
    if not args.no_open:
        threading.Timer(0.6, lambda: webbrowser.open(url)).start()
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nStopped.")
        server.shutdown()


if __name__ == "__main__":
    main()
