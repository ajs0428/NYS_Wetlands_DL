"""
dl_huc_stack.py

Python port of R_Code_Analysis/huc_stack.R -- the single in-memory recipe for
assembling the per-HUC predictor stack. This is the INFERENCE-side twin of what
Raster_ChipsPatches_DL.R does for training chips: it reads the canonical source
rasters (DEM, terrain, hydro, CHM, NAIP, leaf-off ortho, lidar) directly from
their locations and presents them as a single aligned, multi-band stack WITHOUT
ever writing a *_stack.tif to disk.

Nothing here is materialized. Each source is opened lazily; mismatched grids are
aligned to the DEM via rasterio WarpedVRT (a virtual, on-read resample -- the
Python equivalent of terra::resample(r, ref)); and pixels are only ever read
window-by-window by the caller (dl_06b_predict_huc.py). Memory stays bounded by
one tile, and coarse layers are never inflated onto disk.

The band recipe (which layers, what transforms, what names) mirrors huc_stack.R
exactly, so the bands this produces match the training chips the model saw:
    DEM, terrain (TPI dropped), hydro (log flowacc), CHM,
    NAIP (ndvi/ndwi dropped), ortho (_lo suffix), lidar

Band SELECTION is by name downstream (validate_prediction_bands), so this module
does not hardcode which source carries `twi` or `Geomorph_local`; it carries every
source band through under its native description and lets the predictor-name
contract pick the 17 it needs.

Usage (inspect the contract before predicting -- no model needed):
    python dl_huc_stack.py --huc 041402011002 --cluster 208 \
        --data-root /scratch/NYS_Wetlands_Data --inspect
"""

import os
import re
import json
import argparse
from pathlib import Path
from typing import Callable, Dict, List, Optional, Sequence

import numpy as np
import rasterio
from rasterio.windows import Window
from rasterio.vrt import WarpedVRT
from rasterio.enums import Resampling

from dl_band_utils import load_band_config


# --- Canonical source directories (relative to the data-project root) ---------
# Mirrors huc_source_dirs() in huc_stack.R. Paths are relative to --data-root so
# the same code runs against NYS_Wetlands_Data/ locally or a synced copy on the
# GPU server (set --data-root or $NYS_WETLANDS_DATA_ROOT).
SOURCE_SUBDIRS: Dict[str, str] = {
    "dem":   "Data/TerrainProcessed/HUC_DEMs",
    "terr":  "Data/TerrainProcessed/HUC_TerrainMetrics",
    "hydro": "Data/TerrainProcessed/HUC_Hydro",
    "chm":   "Data/CHMs/HUC_CHMs",
    "naip":  "Data/NAIP/HUC_NAIP_Processed",
    "ortho": "Data/Ortho/HUC_Ortho",
    "lidar": "Data/Lidar/HUC_Lidar_Metrics",
}

# Stack order = band order (DEM first -> it is the reference grid). Matches the
# `list(dem=, terr=, hydro=, chm=, naip=, ortho=, lidar=)` order in huc_layers().
SOURCE_ORDER: List[str] = ["dem", "terr", "hydro", "chm", "naip", "ortho", "lidar"]


def resolve_data_root(data_root: Optional[Path] = None) -> Path:
    """Resolve the data-project root from arg, env, or a sensible default."""
    if data_root is not None:
        return Path(data_root)
    env = os.environ.get("NYS_WETLANDS_DATA_ROOT")
    if env:
        return Path(env)
    # Default: sibling NYS_Wetlands_Data next to this DL project.
    return Path(__file__).resolve().parents[3] / "NYS_Wetlands_Data"


def source_dirs(data_root: Path) -> Dict[str, Path]:
    """Absolute source directories under the resolved data root."""
    return {name: data_root / sub for name, sub in SOURCE_SUBDIRS.items()}


# --- Locate the source file for one HUC in each dataset -----------------------
# Mirrors huc_source_paths()/match_one() in huc_stack.R:
#   DEM     -> exclude whitebox ("wbt") intermediates
#   terrain -> the local slope file, excluding 10m / 1000m scales
def _match_one(directory: Path, huc: str, cluster: str,
               extra: Optional[Callable[[str], bool]] = None) -> List[Path]:
    if not directory.is_dir():
        return []
    cid = f"cluster_{cluster}"
    out = []
    for p in sorted(directory.glob("*.tif")):
        name = p.name
        if huc in name and cid in name and (extra is None or extra(name)):
            out.append(p)
    return out


def huc_source_paths(huc: str, cluster: str, dirs: Dict[str, Path]) -> Dict[str, List[Path]]:
    """Resolve the per-HUC source files for each dataset (lazy pointers)."""
    return {
        "dem":   _match_one(dirs["dem"],   huc, cluster, lambda f: "wbt" not in f),
        "terr":  _match_one(dirs["terr"],  huc, cluster,
                            lambda f: ("slp" in f and "local" in f
                                       and not re.search(r"10m|1000m", f))),
        "hydro": _match_one(dirs["hydro"], huc, cluster),
        "chm":   _match_one(dirs["chm"],   huc, cluster),
        "naip":  _match_one(dirs["naip"],  huc, cluster),
        "ortho": _match_one(dirs["ortho"], huc, cluster),
        "lidar": _match_one(dirs["lidar"], huc, cluster),
    }


def huc_sources_ready(paths: Dict[str, List[Path]], huc: str) -> bool:
    """TRUE only if every dataset has at least one file for this HUC."""
    missing = [name for name, files in paths.items() if not files]
    if missing:
        print(f"HUC {huc} missing source(s): {', '.join(missing)}")
        return False
    return True


def _pick1(files: List[Path], label: str) -> Path:
    if len(files) > 1:
        print(f"Multiple {label} files; using first: {files[0].name}")
    return files[0]


# --- Per-source band recipe (mirrors huc_layers() transforms) -----------------
# Each entry: keep predicate (which native bands survive), name transform, and an
# optional pixel transform applied after read. Names not dropped pass through.
def _identity(a: np.ndarray) -> np.ndarray:
    return a


def _log(a: np.ndarray) -> np.ndarray:
    # Matches R's log(hydro$flowacc) (natural log). Baked into training chips, so
    # it must be baked in here too for the model to see the same inputs.
    return np.log(a)


class _SourceBand:
    """One emitted band: where to read it from and how to transform it."""

    def __init__(self, out_name: str, dataset, src_index: int,
                 transform: Callable[[np.ndarray], np.ndarray]):
        self.out_name = out_name
        self.dataset = dataset          # open rasterio dataset OR WarpedVRT
        self.src_index = src_index      # 1-based band index within `dataset`
        self.transform = transform


class VirtualHucStack:
    """
    Lazy, aligned, multi-band view over a HUC's source rasters -- a drop-in,
    read-only stand-in for a rasterio dataset (the bits dl_06 needs):
        .descriptions  .profile  .nodata  .height  .width  .read(indexes, window)

    The DEM defines the reference grid. Sources already on that grid are read
    directly; mismatched sources are wrapped in a WarpedVRT (on-read resample).
    Categorical (one_hot) bands are warped with nearest-neighbour; everything
    else bilinear -- so Geomorph_local is never interpolated.
    """

    def __init__(self, paths: Dict[str, List[Path]],
                 band_config: Optional[dict] = None):
        self._band_config = band_config or {}
        self._open: List = []           # everything to close on exit
        self._bands: List[_SourceBand] = []

        cfg_norm = self._band_config.get("band_normalization", {})

        def is_categorical(name: str) -> bool:
            return cfg_norm.get(name, {}).get("method") == "one_hot"

        # --- DEM: reference grid -------------------------------------------
        dem = rasterio.open(_pick1(paths["dem"], "DEM"))
        self._open.append(dem)
        self.ref = dem
        self.height = dem.height
        self.width = dem.width
        self._ref_profile = dem.profile.copy()
        # DEM is a single band named "DEM".
        self._bands.append(_SourceBand("DEM", dem, 1, _identity))

        # --- the other sources, in stack order -----------------------------
        for key in SOURCE_ORDER[1:]:
            src = rasterio.open(_pick1(paths[key], key))
            self._open.append(src)

            names, keep_idx = self._kept_bands(key, src)
            categorical_present = any(is_categorical(n) for n in names)

            reader = self._aligned_reader(src, categorical_present)

            tfm = _log_transform_for(key)
            for name, idx in zip(names, keep_idx):
                # If we wrapped in a VRT, band order is preserved, so the local
                # index into the reader equals the source index.
                self._bands.append(_SourceBand(name, reader, idx, tfm.get(name, _identity)))

        # --- assembled metadata --------------------------------------------
        self._descriptions = [b.out_name for b in self._bands]
        prof = self._ref_profile
        prof.update(count=len(self._bands), dtype="float32")
        self._profile = prof

    # -- band recipe per source (which native bands survive + their names) --
    def _kept_bands(self, key: str, src) -> (List[str], List[int]):
        descs = list(src.descriptions)
        names = [d if d else f"{key}_band_{i+1}" for i, d in enumerate(descs)]

        keep_idx = list(range(1, src.count + 1))
        out_names = list(names)

        if key == "terr":
            # drop TPI_* (mirrors subset(..., grep("^TPI_", invert=TRUE)))
            keep = [(n, i) for n, i in zip(names, keep_idx)
                    if not re.match(r"^TPI_", n)]
            out_names = [n for n, _ in keep]
            keep_idx = [i for _, i in keep]
        elif key == "naip":
            # drop ndvi/ndwi (subset(c("ndvi","ndwi"), negate=TRUE))
            keep = [(n, i) for n, i in zip(names, keep_idx)
                    if n not in ("ndvi", "ndwi")]
            out_names = [n for n, _ in keep]
            keep_idx = [i for _, i in keep]
        elif key == "ortho":
            # suffix _lo on every band (names(ortho) <- paste0(names, "_lo"))
            out_names = [f"{n}_lo" for n in names]

        return out_names, keep_idx

    # -- align a source to the DEM grid (direct read, or WarpedVRT) ----------
    def _aligned_reader(self, src, categorical_present: bool):
        if self._same_grid(src):
            return src
        resampling = Resampling.nearest if categorical_present else Resampling.bilinear
        vrt = WarpedVRT(
            src,
            crs=self.ref.crs,
            transform=self.ref.transform,
            width=self.ref.width,
            height=self.ref.height,
            resampling=resampling,
        )
        self._open.append(vrt)
        return vrt

    def _same_grid(self, src, tol: float = 1e-6) -> bool:
        if src.crs != self.ref.crs:
            return False
        if (src.width, src.height) != (self.ref.width, self.ref.height):
            return False
        a, b = src.transform, self.ref.transform
        return all(abs(x - y) <= tol for x, y in zip(a[:6], b[:6]))

    # -- rasterio-dataset-like surface --------------------------------------
    @property
    def descriptions(self):
        return tuple(self._descriptions)

    @property
    def profile(self):
        return self._profile.copy()

    @property
    def nodata(self):
        # We emit NaN for nodata; normalize_bands keys off np.isnan.
        return np.nan

    def read(self, indexes: Sequence[int], window: Optional[Window] = None) -> np.ndarray:
        """
        Read the given 1-based virtual band indexes for `window` (DEM pixel
        space). Returns (len(indexes), h, w) float32 with NaN at nodata, source
        transforms applied. Mirrors rasterio's DatasetReader.read signature.
        """
        if np.isscalar(indexes):
            indexes = [int(indexes)]
        out = []
        for idx in indexes:
            band = self._bands[idx - 1]
            arr = band.dataset.read(band.src_index, window=window).astype(np.float32)
            nd = band.dataset.nodata
            if nd is not None and not (isinstance(nd, float) and np.isnan(nd)):
                arr = np.where(arr == nd, np.nan, arr)
            out.append(band.transform(arr))
        return np.stack(out, axis=0)

    # -- lifecycle ----------------------------------------------------------
    def close(self):
        # Close VRTs before their source datasets.
        for d in reversed(self._open):
            try:
                d.close()
            except Exception:
                pass
        self._open = []

    def __enter__(self):
        return self

    def __exit__(self, *exc):
        self.close()


def _log_transform_for(key: str) -> Dict[str, Callable[[np.ndarray], np.ndarray]]:
    """Per-band pixel transforms for a source (mirrors huc_layers())."""
    if key == "hydro":
        return {"flowacc": _log}
    return {}


def open_huc_stack(huc: str, cluster: str,
                   data_root: Optional[Path] = None,
                   band_config: Optional[dict] = None) -> VirtualHucStack:
    """Resolve sources for one HUC and return an open VirtualHucStack."""
    root = resolve_data_root(data_root)
    dirs = source_dirs(root)
    paths = huc_source_paths(huc, cluster, dirs)
    if not huc_sources_ready(paths, huc):
        raise FileNotFoundError(
            f"HUC {huc} (cluster {cluster}) is missing one or more source datasets "
            f"under {root}. Run with --inspect to see what resolved."
        )
    if band_config is None:
        band_config = load_band_config()
    return VirtualHucStack(paths, band_config=band_config)


def _inspect(huc: str, cluster: str, data_root: Optional[Path]):
    """Dry-run: print resolved sources and the assembled band contract."""
    root = resolve_data_root(data_root)
    dirs = source_dirs(root)
    paths = huc_source_paths(huc, cluster, dirs)
    print(f"Data root: {root}")
    print(f"HUC {huc}  cluster {cluster}\n")
    print("Resolved sources:")
    for key in SOURCE_ORDER:
        files = paths[key]
        shown = ", ".join(f.name for f in files) if files else "<MISSING>"
        print(f"  {key:6s}: {shown}")
    if not huc_sources_ready(paths, huc):
        print("\nOne or more sources missing -- cannot assemble stack.")
        return
    print("\nAssembling virtual stack ...")
    with VirtualHucStack(paths, band_config=load_band_config()) as stack:
        print(f"Reference grid: {stack.width} x {stack.height} "
              f"({stack._ref_profile['crs']})")
        print(f"Assembled {len(stack.descriptions)} bands:")
        for i, name in enumerate(stack.descriptions, 1):
            print(f"  {i:2d}. {name}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Assemble a per-HUC predictor stack in memory (no disk write).")
    parser.add_argument("--huc", required=True, help="HUC id (e.g. 041402011002)")
    parser.add_argument("--cluster", required=True, help="cluster number (e.g. 208)")
    parser.add_argument("--data-root", type=Path, default=None,
                        help="Root of the NYS_Wetlands_Data project "
                             "(default: $NYS_WETLANDS_DATA_ROOT or sibling project)")
    parser.add_argument("--inspect", action="store_true",
                        help="Print resolved sources + assembled band names and exit")
    args = parser.parse_args()

    if args.inspect:
        _inspect(args.huc, args.cluster, args.data_root)
    else:
        with open_huc_stack(args.huc, args.cluster, args.data_root) as stack:
            print(f"Virtual stack ready: {len(stack.descriptions)} bands, "
                  f"{stack.width} x {stack.height}")
            print("Bands:", ", ".join(stack.descriptions))
