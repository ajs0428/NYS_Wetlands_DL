/* NYS Wetland DL Predictions — Leaflet viewer.
 *
 * Reads manifest.json produced by python_make_cogs.py and renders the COGs
 * client-side via georaster-layer-for-leaflet (HTTP range requests; no tile
 * server). Band layout per the manifest: band 1 = argmax class, then
 * per-class probability bands (Byte 0-100, nodata 255, scale 0.01).
 */

// ---------------------------------------------------------------------------
// Config — switch COG_BASE to the GCS URL for the hosted version, e.g.
//   const COG_BASE = "https://storage.googleapis.com/<bucket>/cogs";
// The relative default works with webmap/dev_server.py serving the repo root.
// ---------------------------------------------------------------------------
const VIEWER_VERSION = "viewer v6"; // bump when debugging cache staleness
// MUST be an absolute URL: georaster fetches COG blocks from inside a Web
// Worker, whose base URL cannot resolve page-relative paths — a relative
// COG_BASE makes the worker's fetch throw and parseGeoraster hang forever.
const COG_BASE = new URL("../../Data/HUC_DL_Predictions_v2/cogs", location.href).href;
const MANIFEST_URL = `${COG_BASE}/manifest.json`;
const NWI_URL = new URL("../../Data/HUC_DL_Predictions_v2/nwi.pmtiles", location.href).href;

const NYS_HILLSHADE_URL =
  "https://elevation.its.ny.gov/arcgis/rest/services/NYS_Statewide_Hillshade/MapServer";
const ESRI_HILLSHADE_URL =
  "https://services.arcgisonline.com/arcgis/rest/services/Elevation/World_Hillshade/MapServer/tile/{z}/{y}/{x}";

// Okabe-Ito (colorblind-safe); keep in sync with the R viz palette if desired.
const CLASS_COLORS = {
  EMW: "#F0E442",
  FSW: "#009E73",
  SSW: "#D55E00",
  WET: "#0072B2",
  UPL: "#999999",
};
const UPL_ALPHA = 0.25; // de-emphasize upland in class layers
const PROB_RAMP = ["#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"];
const NODATA = 255;

// The prediction COGs are EPSG:6347 (NAD83(2011) / UTM zone 18N), which is NOT
// in georaster-layer-for-leaflet's bundled projection database — without a
// definition the layer throws at creation and nothing renders. The bundle
// checks the global proj4 first, so register the NAD83(2011) UTM zones here
// (EPSG:6330–6348 = zones 1N–19N; NY spans 17N–19N).
if (typeof proj4 === "undefined") {
  reportFatal("proj4 failed to load from the CDN — EPSG:6347 layers cannot render");
} else {
  for (let zone = 1; zone <= 19; zone++) {
    proj4.defs(
      `EPSG:${6329 + zone}`,
      `+proj=utm +zone=${zone} +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs`
    );
  }
}

// Surface every uncaught error in the status bar — never fail silently again.
window.addEventListener("error", (e) => reportFatal(e.message));
window.addEventListener("unhandledrejection", (e) =>
  reportFatal(e.reason?.message ?? String(e.reason))
);
function reportFatal(msg) {
  console.error(msg);
  const el = document.getElementById("status");
  if (el) {
    el.textContent = `ERROR: ${msg}`;
    el.classList.add("error");
  }
}

// ---------------------------------------------------------------------------
// Map + basemaps
// ---------------------------------------------------------------------------
const map = L.map("map", { center: [42.9, -75.5], zoom: 7 });

const basemaps = {
  "Esri World Imagery": L.tileLayer(
    "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
    { attribution: "Esri, Maxar, Earthstar Geographics", maxZoom: 19 }
  ),
  OpenStreetMap: L.tileLayer("https://tile.openstreetmap.org/{z}/{x}/{y}.png", {
    attribution: "&copy; OpenStreetMap contributors",
    maxZoom: 19,
  }),
};
basemaps["Esri World Imagery"].addTo(map);
L.control.layers(basemaps, null, { position: "topright", collapsed: true }).addTo(map);
L.control.scale({ imperial: true }).addTo(map);

// Reference hillshades (checkboxes in the sidebar, not the Leaflet control)
const nysHillshade = L.esri.dynamicMapLayer({ url: NYS_HILLSHADE_URL, opacity: 0.5 });
const esriHillshade = L.tileLayer(ESRI_HILLSHADE_URL, { opacity: 0.5, maxZoom: 16 });
wireOverlayToggle("toggle-nys-hillshade", nysHillshade);
wireOverlayToggle("toggle-esri-hillshade", esriHillshade);

function wireOverlayToggle(id, layer) {
  document.getElementById(id).addEventListener("change", (e) => {
    if (e.target.checked) layer.addTo(map);
    else map.removeLayer(layer);
  });
}

// NWI vector overlay — single static .pmtiles, colored by WETLAND_TYPE.
// Palette approximates the USFWS Wetlands Mapper legend.
const NWI_COLORS = {
  "Freshwater Emergent Wetland": "#66C266",
  "Freshwater Forested/Shrub Wetland": "#0D7F3F",
  "Freshwater Pond": "#57C8D9",
  "Estuarine and Marine Wetland": "#E39D5A",
  "Estuarine and Marine Deepwater": "#3B7E8C",
  Riverine: "#99B4DE",
  Lake: "#3E7DAB",
  Other: "#B0A89B",
};
let nwiLayer = null; // created lazily on first toggle
document.getElementById("toggle-nwi").addEventListener("change", async (e) => {
  if (!e.target.checked) {
    if (nwiLayer) map.removeLayer(nwiLayer);
    updateLegend();
    return;
  }
  if (!nwiLayer) {
    try {
      const head = await fetch(NWI_URL, { method: "HEAD" });
      if (!head.ok) throw new Error(`nwi.pmtiles not available (HTTP ${head.status})`);
    } catch (err) {
      reportFatal(err.message);
      e.target.checked = false;
      return;
    }
    nwiLayer = protomapsL.leafletLayer({
      url: NWI_URL,
      attribution: "USFWS National Wetlands Inventory",
      paintRules: [
        {
          dataLayer: "nwi",
          symbolizer: new protomapsL.PolygonSymbolizer({
            fill: (z, f) => NWI_COLORS[f.props.WETLAND_TYPE] ?? "#888888",
            opacity: 0.55,
            stroke: "#33333366",
            width: 0.5,
          }),
        },
      ],
    });
  }
  nwiLayer.addTo(map);
  updateLegend();
});

// ---------------------------------------------------------------------------
// Manifest -> sidebar rows
// ---------------------------------------------------------------------------
const statusEl = document.getElementById("status");
const searchEl = document.getElementById("search");
const rows = new Map(); // key -> row state

init();

async function init() {
  let manifest;
  try {
    const resp = await fetch(MANIFEST_URL);
    if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
    manifest = await resp.json();
  } catch (err) {
    statusEl.textContent =
      `Failed to load manifest (${err.message}). Serve the repo root with ` +
      `webmap/dev_server.py, or set COG_BASE in app.js.`;
    statusEl.classList.add("error");
    return;
  }

  const union = L.latLngBounds([]);
  for (const entry of manifest) {
    if (!entry.valid_cog) continue;
    buildRow(entry);
    union.extend(entryBounds(entry));
  }
  if (union.isValid()) map.fitBounds(union.pad(0.05));

  for (const kind of ["multiclass", "binary"]) {
    const n = manifest.filter((e) => e.kind === kind && e.valid_cog).length;
    document.querySelector(`#group-${kind} .count`).textContent = `(${n})`;
  }
  statusEl.textContent =
    `${manifest.length} predictions · ${manifest[0]?.version ?? ""} · ${VIEWER_VERSION}`;

  searchEl.disabled = false;
  searchEl.addEventListener("input", () => {
    const q = searchEl.value.trim().toLowerCase();
    for (const row of rows.values()) {
      row.el.classList.toggle("hidden", q !== "" && !row.searchText.includes(q));
    }
  });
}

function entryBounds(entry) {
  const [w, s, e, n] = entry.bounds_wgs84;
  return L.latLngBounds([s, w], [n, e]);
}

function buildRow(entry) {
  const key = `${entry.kind}_${entry.cluster}_${entry.huc}`;
  const container = document.querySelector(`#group-${entry.kind} .rows`);
  if (!container) return;

  const el = document.createElement("div");
  el.className = "layer-row";
  el.innerHTML = `
    <label>
      <input type="checkbox" />
      <span class="name">c${entry.cluster} · ${entry.huc}</span>
      <span class="spinner"></span>
    </label>`;
  container.appendChild(el);

  const probBands = entry.bands.filter((b) => b.endsWith("_prob"));
  const row = {
    entry,
    el,
    key,
    searchText: `${entry.cluster} ${entry.huc}`.toLowerCase(),
    georaster: null, // parsed once, cached
    layer: null,
    mode: "class",
    opacity: 0.8,
    probBands,
    controls: null,
  };
  rows.set(key, row);

  el.querySelector("input[type=checkbox]").addEventListener("change", (e) => {
    if (e.target.checked) enableRow(row);
    else disableRow(row);
  });
}

async function enableRow(row) {
  const spinner = row.el.querySelector(".spinner");
  if (!row.georaster) {
    spinner.textContent = "loading…";
    try {
      row.georaster = await parseGeoraster(`${COG_BASE}/${row.entry.file}`);
    } catch (err) {
      spinner.textContent = "load failed";
      console.error(row.entry.file, err);
      return;
    }
    spinner.textContent = "";
  }
  showControls(row);
  refreshLayer(row);
  map.fitBounds(entryBounds(row.entry).pad(0.05));
}

function disableRow(row) {
  if (row.layer) {
    map.removeLayer(row.layer);
    row.layer = null;
  }
  if (row.controls) {
    row.controls.remove();
    row.controls = null;
  }
  updateLegend();
}

// Rebuild the GeoRasterLayer for the row's current mode/opacity. The parsed
// georaster is reused, so this is cheap (no re-fetch).
function refreshLayer(row) {
  if (row.layer) map.removeLayer(row.layer);
  const spinner = row.el.querySelector(".spinner");
  try {
    row.layer = new GeoRasterLayer({
      georaster: row.georaster,
      pixelValuesToColorFn: makeColorFn(row.entry, row.mode),
      resolution: 128,
      opacity: row.opacity,
    });
    row.layer.addTo(map);
    spinner.textContent = "";
  } catch (err) {
    row.layer = null;
    spinner.textContent = "render failed (see console)";
    console.error(row.entry.file, err);
  }
  updateLegend();
}

function makeColorFn(entry, mode) {
  if (mode === "class") {
    const colors = entry.classes.map((name) => {
      const hex = CLASS_COLORS[name] ?? "#e41a1c";
      return name === "UPL" ? hexToRgba(hex, UPL_ALPHA) : hexToRgba(hex, 1.0);
    });
    return (values) => {
      const v = values[0];
      if (v === NODATA || v == null || v >= entry.classes.length) return null;
      return colors[v];
    };
  }
  // mode = "prob:<band name>", e.g. "prob:FSW_prob"
  const bandIdx = entry.bands.indexOf(mode.slice(5));
  if (bandIdx < 0) return () => null;
  return (values) => {
    const v = values[bandIdx];
    if (v === NODATA || v == null) return null;
    const t = Math.min(v, 100) / 100; // Byte 0-100 -> 0-1 probability (scale)
    return rampColor(t, Math.max(0, (v - 2) / 98)); // hide near-zero speckle
  };
}

function showControls(row) {
  if (row.controls) return;
  const ctl = document.createElement("div");
  ctl.className = "layer-controls";

  const options = [`<option value="class">Classes</option>`]
    .concat(
      row.probBands.map(
        (b) => `<option value="prob:${b}">${b.replace("_prob", "")} probability</option>`
      )
    )
    .join("");
  ctl.innerHTML = `
    <div class="ctl"><span>Layer</span><select>${options}</select></div>
    <div class="ctl"><span>Opacity</span>
      <input type="range" min="0" max="100" value="${row.opacity * 100}" /></div>
    <div class="ctl"><button type="button">Zoom to</button></div>`;
  row.el.appendChild(ctl);
  row.controls = ctl;

  ctl.querySelector("select").addEventListener("change", (e) => {
    row.mode = e.target.value;
    refreshLayer(row);
  });
  ctl.querySelector("input[type=range]").addEventListener("input", (e) => {
    row.opacity = e.target.value / 100;
    if (row.layer) row.layer.setOpacity(row.opacity);
  });
  ctl.querySelector("button").addEventListener("click", () => {
    map.fitBounds(entryBounds(row.entry).pad(0.05));
  });
}

// ---------------------------------------------------------------------------
// Legend — reflects the modes of currently active layers
// ---------------------------------------------------------------------------
function updateLegend() {
  const legend = document.getElementById("legend");
  const active = [...rows.values()].filter((r) => r.layer);
  const nwiOn = document.getElementById("toggle-nwi")?.checked;
  if (active.length === 0 && !nwiOn) {
    legend.innerHTML = "";
    return;
  }
  const parts = [];
  if (nwiOn) {
    parts.push("<h3>NWI wetlands</h3>");
    for (const [name, hex] of Object.entries(NWI_COLORS)) {
      parts.push(`<div class="chip-row"><span class="chip" style="background:${hex}"></span>${name}</div>`);
    }
  }
  const classSchemes = new Set(
    active.filter((r) => r.mode === "class").map((r) => r.entry.classes.join(","))
  );
  for (const scheme of classSchemes) {
    parts.push("<h3>Classes</h3>");
    for (const name of scheme.split(",")) {
      const hex = CLASS_COLORS[name] ?? "#e41a1c";
      const style =
        name === "UPL" ? `background:${hexToRgba(hex, UPL_ALPHA)}` : `background:${hex}`;
      parts.push(`<div class="chip-row"><span class="chip" style="${style}"></span>${name}</div>`);
    }
  }
  if (active.some((r) => r.mode.startsWith("prob:"))) {
    parts.push(`
      <h3>Probability</h3>
      <div class="ramp" style="background:linear-gradient(to right, ${PROB_RAMP.join(",")})"></div>
      <div class="ramp-labels"><span>0</span><span>0.5</span><span>1</span></div>`);
  }
  legend.innerHTML = parts.join("");
}

// ---------------------------------------------------------------------------
// Color helpers
// ---------------------------------------------------------------------------
function hexToRgba(hex, alpha) {
  const n = parseInt(hex.slice(1), 16);
  return `rgba(${(n >> 16) & 255},${(n >> 8) & 255},${n & 255},${alpha})`;
}

function rampColor(t, alpha) {
  const stops = PROB_RAMP.map((h) => parseInt(h.slice(1), 16));
  const x = t * (stops.length - 1);
  const i = Math.min(Math.floor(x), stops.length - 2);
  const f = x - i;
  const mix = (a, b) => Math.round(a + (b - a) * f);
  const [a, b] = [stops[i], stops[i + 1]];
  const r = mix((a >> 16) & 255, (b >> 16) & 255);
  const g = mix((a >> 8) & 255, (b >> 8) & 255);
  const bl = mix(a & 255, b & 255);
  return `rgba(${r},${g},${bl},${alpha})`;
}
