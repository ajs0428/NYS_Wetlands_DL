# NYS Wetland DL Predictions — Web Map

Static Leaflet viewer for the deep-learning prediction COGs. No build step, no
tile server: the browser streams COGs by HTTP range request and renders them
client-side (`georaster-layer-for-leaflet`). Full plan:
`Python_Code_Analysis/COG_Plan.md`.

```
webmap/
  viewer/          # the static site (index.html, app.js, style.css)
  dev_server.py    # local static server WITH Range support (stdlib only)
```

## Local development (HPC + ssh tunnel)

Stock `python -m http.server` does **not** support Range requests, which COG
streaming requires — use the bundled server instead:

```bash
# on the login node
python3 /ibstorage/anthony/NYS_Wetlands_DL/webmap/dev_server.py \
  --root /ibstorage/anthony/NYS_Wetlands_DL --port 8787

# from your local machine
ssh -L 8787:localhost:8787 <user>@cbsuxu10.biohpc.cornell.edu
# then open http://localhost:8787/webmap/viewer/
```

The viewer's default `COG_BASE` in `viewer/app.js` is the repo-relative
`Data/HUC_DL_Predictions_v2/cogs`, so it works as soon as `manifest.json` and
the `*_cog.tif` files exist there.

## Switching to hosted data (Phase 3)

Edit one line in `viewer/app.js`:

```js
const COG_BASE = "https://storage.googleapis.com/<bucket>/cogs";
```

The bucket needs CORS (`GET`/`HEAD`, `Range` header) and public read — see
COG_Plan.md Phase 3. The viewer itself deploys as static files (GCS `viewer/`
prefix or GitHub Pages).

## Layer model

- Sidebar lists every manifest entry, grouped **Multiclass** / **Binary**,
  filterable by cluster or HUC. COGs are fetched on first toggle only.
- Each active layer has a mode select — **Classes** (band 1, categorical
  Okabe-Ito palette, UPL de-emphasized) or a **per-class probability** band
  (sequential blue ramp, alpha ∝ probability) — plus an opacity slider and
  zoom-to. Class names/band layout come from the manifest, never hardcoded.
- Reference overlays: NYS lidar statewide hillshade (dynamic ArcGIS MapServer
  via esri-leaflet) and Esri World Hillshade (tiles). NWI PMTiles arrives with
  Phase 2b.

## Hard-won gotchas (do not regress)

- **COG_BASE must be an absolute URL** (`new URL(..., location.href).href`):
  georaster fetches COG blocks from inside a Web Worker, which cannot resolve
  page-relative paths — a relative base makes `parseGeoraster` hang forever
  with no error surfaced.
- **EPSG:6347 needs a global proj4**: the predictions' CRS (NAD83(2011)/UTM 18N)
  is missing from georaster-layer-for-leaflet's bundled projection database.
  `index.html` loads proj4 from CDN and `app.js` registers zones 1N–19N
  (EPSG:6330–6348) before any layer is created. Without it, layer creation
  throws "projection not found in proj4 instance".
- `.tif.ovr` HEAD 404s in the server log are normal (georaster probing for an
  external overview sidecar before using the COG's internal overviews).
- `viewer/debug.html` traces the whole pipeline on-page (manifest → parse →
  layer → tiles) — first stop when layers won't render.
- Verified headless (playwright): parse ~40 ms, first tile ~130 ms, internal
  overviews keep a statewide view to ~2 range requests per layer.

## Known follow-ups

- Verify CRS alignment against the imagery basemap at high zoom (looks right
  at HUC scale in headless screenshots).
- NWI overlay (PMTiles + protomaps-leaflet) once Phase 2b produces
  `nwi.pmtiles`.
- Consider a side-by-side swipe control (leaflet-side-by-side) for the
  DL-vs-NWI comparison.
