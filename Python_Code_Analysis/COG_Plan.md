# NYS Wetland DL Predictions — Interactive Web Map Plan

A build plan for tiling ~120 deep-learning wetland prediction rasters and serving
them in an interactive Leaflet map, overlaid with hillshade and legacy NWI data
to show the difference the new maps make.

This document is written for Claude Code. Fill in the `<PLACEHOLDER>` values in
the **Configuration** section before starting, then work phase by phase.

---

## Configuration (fill these in first)

```
PROJECT_ROOT      = <PROJECT_ROOT>        # e.g. /ibstorage/anthony/NYS_Wetlands_DL
DATA_DIR          = <PROJECT_ROOT>/Data/HUC_DL_Predictions_v2   # source *_probs.tif live here
COG_DIR           = <DATA_DIR>/cogs       # COG outputs + manifest.json
FRAG_DIR          = <DATA_DIR>/frags      # per-file manifest fragments (array-safe)
SCRIPTS_DIR       = <PROJECT_ROOT>/webmap/scripts   # make_cogs.py, submit_cogs.sh
VIEWER_DIR        = <PROJECT_ROOT>/webmap/viewer    # static Leaflet site
DEM_PATH          = <PATH_TO_DEM>         # for hillshade generation (Phase 2), optional
NWI_SOURCE        = <PATH_TO_NWI>         # NWI polygons for the AOI (shapefile/gpkg)

GCS_BUCKET        = <GCS_BUCKET_NAME>     # e.g. gs://nys-wetland-dl-cogs
GCS_PUBLIC_URL    = https://storage.googleapis.com/<GCS_BUCKET_NAME>
```

**Repo layout convention:** everything lives under `PROJECT_ROOT` on ibstorage —
scripts, COGs, and the viewer. Data does not move; the viewer reads COGs from GCS
at runtime, not from local disk.

---

## Decisions already made (do not relitigate)

- **Tiling strategy: Cloud-Optimized GeoTIFFs (COGs) + client-side rendering.**
  No tile server. Leaflet reads COG byte ranges directly via
  `georaster-layer-for-leaflet`. Chosen over pre-rendered XYZ tiles so probability
  values stay continuous and restylable in the browser.
- **Probability reduction: argmax class + confidence.** Multiclass `_probs.tif`
  (per-class Float32 stack) is reduced to 2 bands — band 1 = argmax class (Byte,
  0-indexed), band 2 = max probability at that pixel (Byte, 0–100). Binary rasters
  become a single Byte confidence band (0–100). Nodata = 255.
- **Rescale: Float32 → Byte, prob × 100.** Continuous interpretation preserved as
  0–100 integers. `SCALE=0.01` written as raster metadata; viewer divides by 100.
- **Hosting: Google Cloud Storage bucket** (academic account). GCS supports HTTP
  range requests, which COGs require. **Not** Google Earth Engine — GEE is the
  wrong tool for serving arbitrary COGs to Leaflet. University hosting is a fallback
  only if it honors `Range:` headers.
- **NWI overlay: vector, served as PMTiles** (single static file, range-streamed).
  NWI is polygons, not raster — do not rasterize it.

### Validated results (Phase 1 test pair, already run)
- `DLpred_binary_cluster_11_huc_042900030202_probs.tif`: 870 MB → **33 MB** COG.
- `DLpred_multiclass_cluster_250_huc_041402011205_probs.tif`: 2.3 GB → **43 MB** COG,
  confirmed valid COG via `rio cogeo validate`.
- Full set of ~120 rasters expected to land around **4–6 GB total**.

---

## Phase 1 — COG conversion pipeline  ✅ SCRIPT COMPLETE, BATCH PENDING

The conversion script `make_cogs.py` is written and validated on the test pair.
Remaining work: run it across all ~120 rasters via SLURM array, then merge the
manifest.

### Files
- `scripts/make_cogs.py` — conversion + validation + manifest. Already copied over;
  **no code changes needed**, but confirm paths in invocations point at `DATA_DIR`.
- `scripts/submit_cogs.sh` — SLURM array wrapper. **Update the `INDIR`/`OUTDIR`/
  `FRAGDIR` variables** at the top to the resolved config paths.

### Run
```bash
cd <DATA_DIR>
ls -1 *_probs.tif > probs_list.txt
mkdir -p logs cogs frags
wc -l probs_list.txt          # set submit_cogs.sh --array=0-(N-1) to match

sbatch <SCRIPTS_DIR>/submit_cogs.sh

# after the array completes, merge per-file fragments into one manifest:
python <SCRIPTS_DIR>/make_cogs.py --outdir ./cogs --frag-dir ./frags --merge-only
```

### Validate the batch
```bash
ls cogs/*_cog.tif | wc -l                                    # == input count
jq 'length' cogs/manifest.json                               # == input count
jq '[.[] | select(.valid_cog==false)]' cogs/manifest.json    # should be []
```

### Notes / gotchas
- `submit_cogs.sh` throttles to 12 concurrent tasks (`%12`). Raise if the node has
  RAM headroom; multiclass argmax reads the full input stack into memory
  (`src.read()`), so each task holds one input's worth of Float32.
- If a task OOMs (check `logs/cog_*.err`), bump `--mem` and rerun only failed
  indices: `sbatch --array=<i>,<j> <SCRIPTS_DIR>/submit_cogs.sh`.
- **Class scheme is 0-indexed and currently keeps class 0 as a real value.** If the
  model's class 0 is "background/non-wetland," decide whether it should be nodata
  instead — this affects viewer coloring. If a change is needed, edit `build_bands`
  in `make_cogs.py` (fold class 0 into the `valid` mask).
- `manifest.json` entry per file carries: `file`, `kind` (binary/multiclass),
  `cluster`, `huc`, `bands`, `scale`, `nodata`, `bounds_wgs84` ([W,S,E,N]),
  `valid_cog`. The viewer relies on this shape.

---

## Phase 2 — Hillshade + NWI overlay layers

Two independent sub-tasks. Both produce static files hosted alongside the COGs.

### 2a. Hillshade
Two options — pick based on whether a project DEM is on hand:
- **Generate from DEM** (preferred for a matched-resolution hillshade):
  ```bash
  gdaldem hillshade <DEM_PATH> hillshade.tif -z 2 -compute_edges
  rio cogeo create hillshade.tif <COG_DIR>/hillshade_cog.tif --cog-profile deflate
  ```
  Add a `hillshade` entry to the manifest (or a small separate `overlays.json`).
- **Use a hosted terrain basemap** and skip generating one (simpler; loses exact
  co-registration with the predictions). Acceptable for a first pass.

### 2b. NWI → PMTiles
```bash
# clip NWI to the prediction AOI, simplify, convert to vector tiles
ogr2ogr -f GeoJSON nwi_clip.geojson <NWI_SOURCE> -clipsrc <aoi_bbox_or_layer>
tippecanoe -o nwi.pmtiles -zg --drop-densest-as-needed \
  --layer=nwi nwi_clip.geojson
```
- Output `nwi.pmtiles` is a single static file, range-streamed like the COGs.
- Preserve the NWI wetland-class attribute so the viewer can color/legend it.
- Keep geometry simplification conservative enough that the old-vs-new comparison
  is still meaningful at the zoom levels users will inspect.

### Deliverables
- `<COG_DIR>/hillshade_cog.tif` (or a decision to use a terrain basemap)
- `<COG_DIR>/nwi.pmtiles`

---

## Phase 3 — GCS deployment

Upload COGs + manifest + overlays, and configure the bucket for browser access.

### Upload
```bash
gcloud auth login                          # academic account
gsutil -m cp <COG_DIR>/*_cog.tif <GCS_BUCKET>/cogs/
gsutil -m cp <COG_DIR>/manifest.json <GCS_BUCKET>/
gsutil -m cp <COG_DIR>/nwi.pmtiles <GCS_BUCKET>/overlays/
```

### CORS (required — browser range reads fail without it)
Create `cors.json`:
```json
[
  {
    "origin": ["*"],
    "method": ["GET", "HEAD"],
    "responseHeader": ["Content-Type", "Range", "Access-Control-Allow-Origin"],
    "maxAgeSeconds": 3600
  }
]
```
Apply and set public read:
```bash
gsutil cors set cors.json <GCS_BUCKET>
gsutil iam ch allUsers:objectViewer <GCS_BUCKET>
```
- Tighten `origin` to the viewer's domain once it has one; `*` is fine for dev.
- After upload, spot-check a range request:
  `curl -I -H "Range: bytes=0-1023" <GCS_PUBLIC_URL>/cogs/<one_file>_cog.tif`
  — expect `206 Partial Content`.

### Notes
- Storage is trivial (~4–6 GB). Egress for a demo map is minimal; academic credits
  cover it comfortably.
- The manifest's `file` field is just the basename; the viewer prepends
  `GCS_PUBLIC_URL/cogs/` to build full URLs.

---

## Phase 4 — Leaflet viewer

A static site under `VIEWER_DIR`. No server to run — all data streams from GCS via
range requests. Deploys to the same GCS bucket or GitHub Pages.

### Stack
- **Leaflet** — base map + layer control.
- **`georaster-layer-for-leaflet`** + **`georaster`** — render COGs client-side
  directly from URLs (no tile server).
- **`pmtiles`** + **`leaflet` vector overlay** (e.g. `protomaps-leaflet`) — NWI.
- Satellite + terrain basemaps (e.g. Esri World Imagery, a terrain/hillshade tile
  layer) as toggleable bases.

### Features
- Reads `manifest.json` on load; builds the layer switcher from it.
- **Group layers by cluster / HUC** (from manifest fields) — 120 flat checkboxes is
  unusable. Collapsible groups or a searchable list.
- **Load COG layers on demand** — fetch a COG only when its checkbox is ticked, not
  all 120 at once.
- **Client-side colormap**: read Byte value, divide by `scale` (0.01) → 0–1
  probability, ramp color. Confidence band drives opacity/intensity; class band
  drives categorical color for multiclass.
- **Opacity slider** per active prediction layer.
- **NWI overlay toggle** for the old-vs-new comparison; consider a swipe/split
  control (e.g. `leaflet-side-by-side`) to compare new predictions against NWI.
- Nodata (255) renders transparent.

### Files
- `viewer/index.html`
- `viewer/app.js`
- `viewer/style.css`
- (viewer reads `GCS_PUBLIC_URL/manifest.json` at runtime)

### Deploy
```bash
gsutil -m cp -r <VIEWER_DIR>/* <GCS_BUCKET>/viewer/
# or push to a gh-pages branch for GitHub Pages hosting
```

### Notes / gotchas
- `georaster-layer-for-leaflet` does **not** auto-apply the `SCALE` metadata tag —
  divide by 100 explicitly in the `pixelValuesToColorFn`.
- Confirm the COG CRS reprojects cleanly to Web Mercator in the browser; if the
  predictions are in a projected NYS CRS, georaster handles reprojection but test
  alignment against the basemap early.
- Build and test the viewer against the **test-pair COGs first** (one binary, one
  multiclass) before wiring in all 120 — same derisking approach that worked for
  Phase 1.

---

## Suggested working order for Claude Code

1. **Phase 1 batch** — resolve config paths, run the SLURM array, merge + validate
   the manifest. (Script is done; this is an execution step.)
2. **Phase 4 viewer skeleton against the test pair** — get one binary + one
   multiclass COG rendering with correct colormap and alignment before scaling.
   (Can proceed in parallel with 1 using the two existing test COGs.)
3. **Phase 3 hosting** — stand up the GCS bucket + CORS, upload the test COGs,
   confirm 206 range responses, point the viewer at live URLs.
4. **Phase 2 overlays** — hillshade + NWI PMTiles once the core prediction map works.
5. **Full upload + full layer list** — push all 120 COGs, regenerate/confirm the
   manifest, verify grouped on-demand loading in the viewer.

## Open items to confirm during the build
- Model class scheme: is class 0 background (→ nodata) or a real class?
- DEM availability for a matched hillshade vs. using a terrain basemap.
- NWI source extent and the attribute field holding wetland class.
- Final viewer hosting: GCS `viewer/` path vs. GitHub Pages.
