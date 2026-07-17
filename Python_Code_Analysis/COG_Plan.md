# NYS Wetland DL Predictions — Interactive Web Map Plan

A build plan for tiling the deep-learning wetland prediction rasters (currently
60 `_probs.tif` in v2: 30 binary + 30 multiclass, one pair per HUC12) and serving
them in an interactive Leaflet map, overlaid with hillshade and legacy NWI data
to show the difference the new maps make.

This document is written for Claude Code. The **Configuration** values below are
resolved to real paths; work phase by phase.

---

## Configuration (resolved 2026-07-12)

```
PROJECT_ROOT      = /ibstorage/anthony/NYS_Wetlands_DL
DATA_DIR          = /ibstorage/anthony/NYS_Wetlands_DL/Data/HUC_DL_Predictions_v2   # 60 *_probs.tif (30 binary + 30 multiclass)
COG_DIR           = $DATA_DIR/cogs        # COG outputs + manifest.json (test pair already here)
FRAG_DIR          = $DATA_DIR/frags       # per-file manifest fragments (array-safe)
CONVERT_SCRIPT    = $PROJECT_ROOT/Python_Code_Analysis/python_make_cogs.py   # repo convention: python in Python_Code_Analysis/
SUBMIT_SCRIPT     = $PROJECT_ROOT/Shell_Scripts/submit_cogs.sh               # repo convention: shell in Shell_Scripts/
VIEWER_DIR        = $PROJECT_ROOT/webmap/viewer    # static Leaflet site (to be created, Phase 4)
DEM_DIR           = /ibstorage/anthony/NYS_Wetlands_Data/Data/TerrainProcessed/HUC_DEMs  # per-HUC DEMs: cluster_<C>_huc_<H>.tif (Phase 2 hillshade)
NWI_SOURCE        = /ibstorage/anthony/NYS_Wetlands_Data/Data/NWI/NY_NWI_6347.gpkg       # statewide NWI, EPSG:6347 (same CRS as predictions)

GCS_BUCKET        = <GCS_BUCKET_NAME>     # e.g. gs://nys-wetland-dl-cogs (create in Phase 3)
GCS_PUBLIC_URL    = https://storage.googleapis.com/<GCS_BUCKET_NAME>
```

**Compute environment (BioHPC login/SLURM node `cbsuxu10`):** SLURM partitions
`R128C40` (cbsuxu01-08) and `R256C128` (cbsuxu09-10). The `wetland-cnn` conda env
(has `rio_cogeo` 7.0.2 / `rasterio` 1.3.6) lives on **node-local**
`/workdir/$USER/miniconda3` — it is only guaranteed visible on cbsuxu10, so
`submit_cogs.sh` pins the array there (`--nodelist=cbsuxu10`; 128 cores, plenty
for 12 concurrent tasks). To fan out across nodes later, build a small shared
venv on `/ibstorage` (rasterio + rio-cogeo only) and drop the pin.

**Repo layout convention:** everything lives under `PROJECT_ROOT` on ibstorage —
scripts, COGs, and the viewer. Data does not move; the viewer reads COGs from GCS
at runtime, not from local disk. Predictions/COGs stay versioned by directory
(`HUC_DL_Predictions_v2`); future model generations get a new `_v<N>` directory
and their own `cogs/` + manifest, so reruns never clobber a published set.

---

## Decisions already made (do not relitigate)

- **Tiling strategy: Cloud-Optimized GeoTIFFs (COGs) + client-side rendering.**
  No tile server. Leaflet reads COG byte ranges directly via
  `georaster-layer-for-leaflet`. Chosen over pre-rendered XYZ tiles so probability
  values stay continuous and restylable in the browser.
- **Band layout (updated 2026-07-12 after user clarification): band 1 = argmax
  class, then per-class probability bands.** Multiclass `_probs.tif` (4-class
  Float32 stack) → **5-band** Byte COG: class (0=EMW 1=FSW 2=SSW 3=UPL), then
  EMW/FSW/SSW/UPL probability (0–100) — full per-class surfaces kept so the
  viewer can show any class's probability, not just the winner's. Binary
  `_probs.tif` (2-band WET/UPL softmax) → **2-band** COG: class (0=WET 1=UPL),
  then WET probability only (UPL = 1 − WET, dropped by design). Nodata = 255.
  Class names are read from source band descriptions and shipped in the
  manifest (`classes`) + raster tags (`CLASSES`), never hardcoded.
- **Rescale: Float32 → Byte, prob × 100.** Continuous interpretation preserved as
  0–100 integers. `SCALE=0.01` written as raster metadata; viewer divides by 100.
- **Hosting: Google Cloud Storage bucket** (academic account). GCS supports HTTP
  range requests, which COGs require. **Not** Google Earth Engine — GEE is the
  wrong tool for serving arbitrary COGs to Leaflet. University hosting is a fallback
  only if it honors `Range:` headers.
- **NWI overlay: vector, served as PMTiles** (single static file, range-streamed).
  NWI is polygons, not raster — do not rasterize it.

### Validated results (Phase 1 test pair, regenerated 2026-07-12 with final band layout)
- `DLpred_binary_cluster_11_huc_042900030202_probs.tif`: 870 MB → **35 MB** COG
  (2-band: class + WET_prob).
- `DLpred_multiclass_cluster_250_huc_041402011205_probs.tif`: 2.3 GB → **107 MB** COG
  (5-band: class + 4 per-class probs), valid COG per `rio_cogeo.cog_info`.
- COG class band spot-checked **100% identical** to the model's own
  `DLpred_multiclass_*.tif` class raster (1M-pixel window).
- Full set is **60 rasters** (30 binary + 30 multiclass), expected around **4–4.5 GB total**.

---

## Phase 1 — COG conversion pipeline  ✅ SCRIPT COMPLETE, BATCH PENDING

The conversion script `make_cogs.py` is written and validated on the test pair.
Remaining work: run it across all ~120 rasters via SLURM array, then merge the
manifest.

### Files
- `Python_Code_Analysis/python_make_cogs.py` — conversion + validation + manifest.
  Implements the decided band layout (5-band multiclass / 2-band binary with
  WET prob only); binary inputs are detected via band descriptions (WET/UPL).
- `Shell_Scripts/submit_cogs.sh` — SLURM array wrapper, paths resolved
  (`--array=0-59%12`, pinned to cbsuxu10 for the conda env), resumable
  (skips tasks whose COG + fragment already exist).

### Run
```bash
DATA_DIR=/ibstorage/anthony/NYS_Wetlands_DL/Data/HUC_DL_Predictions_v2
cd $DATA_DIR
ls -1 $DATA_DIR/*_probs.tif > probs_list.txt   # absolute paths, one per line
mkdir -p logs cogs frags
wc -l probs_list.txt          # 60; submit_cogs.sh --array=0-59 matches

sbatch /ibstorage/anthony/NYS_Wetlands_DL/Shell_Scripts/submit_cogs.sh

# after the array completes, merge per-file fragments into one manifest:
python /ibstorage/anthony/NYS_Wetlands_DL/Python_Code_Analysis/python_make_cogs.py \
  --inputs dummy --outdir $DATA_DIR/cogs --frag-dir $DATA_DIR/frags --merge-only
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
- **Class scheme (RESOLVED): class 0 is a real class.** Multiclass bands are
  0=EMW, 1=FSW, 2=SSW, 3=UPL (from band descriptions). Keep all four as valid
  values; the *viewer* should render UPL de-emphasized/transparent, not the COG.
- `manifest.json` entry per file carries: `file`, `kind` (binary/multiclass),
  `version` (e.g. "v2", inferred from the input dir suffix), `cluster`, `huc`,
  `bands` (e.g. `["class","EMW_prob",...]`), `classes` (class-integer → name,
  e.g. `["EMW","FSW","SSW","UPL"]`), `scale`, `nodata`, `bounds_wgs84`
  ([W,S,E,N]), `valid_cog`. The viewer relies on this shape.

---

## Phase 2 — Hillshade + NWI overlay layers

Two independent sub-tasks. Both produce static files hosted alongside the COGs.

### 2a. Hillshade — DECIDED: hosted REST tile service, no DEM processing
Use a high-resolution hillshade served over REST as a Leaflet tile layer
(Phase 4 work; nothing to generate or host ourselves):
1. **First choice: NYS ITS GIS lidar-derived statewide hillshade** — check the
   services under `https://elevation.its.ny.gov/arcgis/rest/services` (and
   `orthos.dhses.ny.gov`) during the viewer build; ~1 m where lidar exists.
2. **Fallback: Esri World Hillshade** tile service
   (`services.arcgisonline.com/.../Elevation/World_Hillshade/MapServer`) —
   trivial in Leaflet, coarser source.
The per-HUC DEMs at `DEM_DIR` remain available if a co-registered hillshade
COG is ever wanted, but it is out of scope for the web map.

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
- Hillshade REST tile URL confirmed working in Leaflet (Phase 4)
- `$COG_DIR/nwi.pmtiles`

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
- ~~Model class scheme~~ RESOLVED: class 0 = EMW (real class); 0=EMW, 1=FSW, 2=SSW, 3=UPL.
- ~~DEM availability~~ RESOLVED: per-HUC DEMs at `DEM_DIR` (`cluster_<C>_huc_<H>.tif`);
  hillshade can be generated per-HUC to exactly match prediction footprints.
- NWI source found (`NY_NWI_6347.gpkg`, statewide) — confirm the attribute field
  holding wetland class before tippecanoe (Phase 2b).
- ~~Binary COG representation~~ RESOLVED: 2-band (class + WET prob), UPL prob dropped.
- ~~Multiclass COG representation~~ RESOLVED: 5-band (class + all 4 per-class probs).
- ~~Hillshade~~ RESOLVED: REST tile service (NYS lidar hillshade first, Esri
  World Hillshade fallback); no DEM-derived hillshade COG.
- Final viewer hosting: GCS `viewer/` path vs. GitHub Pages.
- v1 predictions (`Data/HUC_DL_Predictions/`, 3 multiclass HUCs) are assumed
  superseded by v2 and excluded from the web map.
