#!/usr/bin/env python
"""
dl_07_mask_predictions.py

Clip HUC inference rasters to the valid HUC12 boundary.

`dl_06b_predict_huc.py` predicts over the whole DEM bounding box. A watershed is
an irregular shape inside a rectangle, so across the 30 HUCs in
`Shell_Scripts/huc.txt` **54.5% of every prediction raster falls outside the
HUC12 it belongs to** -- 3,465 km2 against the 2,889 km2 that is in scope.

Those pixels are not merely unused. The stack there is out-of-domain and each
architecture answers differently: measured over the raw bounding box, the share
called wetland outside the watershed ranges from 0.01% (unet multiclass) to
98.2% (unet3plus binary), for models whose *in-watershed* answers agree within a
factor of two. Any statistic taken from an unmasked raster is dominated by that
noise, in an arch-dependent direction. The same applies to reference layers:
NWI is clipped to the watershed, so dividing it by the bbox understates it 2.1x.

This step writes nodata everywhere outside the polygon -- 255 for the Byte class
raster, NaN for the Float32 probability raster. Rasters are streamed in row
chunks, so a 1.5 GB `_probs.tif` never lands in memory whole.

There is deliberately no --crop. Measured over all 30 HUCs, the polygon's
bounding box is 99.8% of the raster's (median 100%), because the raster was
built from that bbox in the first place -- cropping saves nothing. The real
saving is compression: masked-out area becomes a uniform nodata run that LZW
collapses, taking one class raster from 3.4 MB to 921 KB.

Usage:
    # in place, one HUC -- what run_predict_factorial.sh calls
    python dl_07_mask_predictions.py --in-place \
        --pattern 'DLpred_multiclass_cluster_102_huc_020200040104*.tif' \
        --in-dir Data/HUC_DL_Predictions_v3

    # a whole arm into a sibling root
    python dl_07_mask_predictions.py \
        --in-dir Data/HUC_DL_Predictions_v3 \
        --out-dir Data/HUC_DL_Predictions_v3_masked

    # every arm, in place, preview first
    for root in Data/HUC_DL_Predictions_v3{,_unet3plus,_mbfusion}; do
        python dl_07_mask_predictions.py --in-dir "$root" --in-place -n
    done

The HUC12 id comes from the filename (`..._huc_<HUCID>[_probs].tif`) and is
matched against the `huc12` column of the cluster GPKG. Idempotent in --out-dir
mode (an existing output is skipped); --in-place always rewrites, so it is
guarded by a marker tag -- a raster already carrying `DL_MASKED=huc12` is left
alone unless --overwrite.
"""

import argparse
import os
import re
import sys
from pathlib import Path

import geopandas as gpd
import numpy as np
import rasterio
from rasterio import features
from rasterio.windows import Window

# Filename contract from dl_06b: DLpred_<mode>_cluster_<C>_huc_<H>[_probs].tif
HUC_RE = re.compile(r"_huc_(\d+)")

# Written into the output's GeoTIFF metadata so a second pass can tell it is done.
MARKER_KEY = "DL_MASKED"
MARKER_VAL = "huc12"

# Repo copy first (staged to the GPU node by rsync_push_v3.sh), then the
# canonical copy in the sibling data project for CPU-side runs.
REPO_ROOT = Path(__file__).resolve().parents[2]
GPKG_CANDIDATES = [
    REPO_ROOT / "Data/NY_HUCS/NY_Cluster_Zones_250_CROP_NAomit_6347.gpkg",
    Path("/ibstorage/anthony/NYS_Wetlands_Data/Data/NY_HUCS/"
         "NY_Cluster_Zones_250_CROP_NAomit_6347.gpkg"),
]

CHUNK_BYTES = 128 << 20      # target per-chunk read, ~128 MB


def resolve_gpkg(explicit):
    if explicit:
        p = Path(explicit)
        if not p.exists():
            sys.exit(f"[dl_07] --gpkg not found: {p}")
        return p
    for p in GPKG_CANDIDATES:
        if p.exists():
            return p
    sys.exit("[dl_07] no HUC vector found. Looked in:\n  " +
             "\n  ".join(str(p) for p in GPKG_CANDIDATES) +
             "\nPass --gpkg, or stage Data/NY_HUCS/ onto the node.")


def build_parser():
    p = argparse.ArgumentParser(
        description="Clip HUC prediction rasters to their HUC12 boundary.")
    p.add_argument("--in-dir", required=True, type=Path,
                   help="Directory of DLpred_*.tif rasters")
    p.add_argument("--out-dir", type=Path,
                   help="Destination root; mutually exclusive with --in-place")
    p.add_argument("--in-place", action="store_true",
                   help="Rewrite each raster where it sits (atomic temp+rename)")
    p.add_argument("--gpkg", default=None,
                   help="HUC vector (default: repo Data/NY_HUCS, then the "
                        "sibling NYS_Wetlands_Data copy)")
    p.add_argument("--huc-field", default="huc12",
                   help="Column holding the HUC12 id (default: huc12)")
    p.add_argument("--pattern", default="DLpred_*.tif",
                   help="Glob for input rasters (default: DLpred_*.tif)")
    p.add_argument("--overwrite", action="store_true",
                   help="Redo rasters already marked as masked / already present")
    p.add_argument("-n", "--dry-run", action="store_true",
                   help="Report what would be written, write nothing")
    return p


def already_masked(path):
    try:
        with rasterio.open(path) as s:
            return s.tags().get(MARKER_KEY) == MARKER_VAL
    except rasterio.RasterioIOError:
        return False


def mask_one(src_path, dst_path, geom_cache, huc, poly, dry_run):
    """Stream src -> dst, writing nodata outside the polygon. Returns kept %."""
    with rasterio.open(src_path) as src:
        is_float = src.dtypes[0].startswith("float")
        nodata = float("nan") if is_float else 255

        # Cache the reprojected geometry per (HUC, CRS) -- keying on CRS alone
        # silently reuses the first HUC's polygon for every later raster, which
        # masks them to nothing.
        key = (huc, src.crs.to_string() if src.crs else None)
        geoms = geom_cache.get(key)
        if geoms is None:
            geoms = list(poly.to_crs(src.crs).geometry)
            geom_cache[key] = geoms

        itemsize = np.dtype(src.dtypes[0]).itemsize
        rows = max(1, min(src.height,
                          CHUNK_BYTES // max(1, src.width * itemsize)))

        profile = src.profile.copy()
        profile.update(nodata=nodata, compress="LZW",
                       tiled=True, blockxsize=512, blockysize=512)
        descriptions = src.descriptions

        if dry_run:
            return None

        kept = 0
        total = 0
        with rasterio.open(dst_path, "w", **profile) as dst:
            for row0 in range(0, src.height, rows):
                nrow = min(rows, src.height - row0)
                win = Window(0, row0, src.width, nrow)
                # One rasterize per chunk, reused across every band.
                keep = features.rasterize(
                    ((g, 1) for g in geoms),
                    out_shape=(nrow, src.width),
                    transform=src.window_transform(win),
                    fill=0, dtype="uint8").astype(bool)
                kept += int(keep.sum())
                total += keep.size
                for b in range(1, src.count + 1):
                    band = src.read(b, window=win)
                    band[~keep] = nodata
                    dst.write(band, b, window=win)
            for i, d in enumerate(descriptions, start=1):
                if d:
                    dst.set_band_description(i, d)
            tags = src.tags()
            tags[MARKER_KEY] = MARKER_VAL
            dst.update_tags(**tags)
    return 100.0 * kept / total if total else 0.0


def main(argv=None):
    args = build_parser().parse_args(argv)

    if args.in_place and args.out_dir:
        sys.exit("[dl_07] pass either --in-place or --out-dir, not both")
    if not args.in_place and not args.out_dir:
        sys.exit("[dl_07] pass --out-dir, or --in-place to rewrite in situ")

    rasters = sorted(args.in_dir.glob(args.pattern))
    if not rasters:
        sys.exit(f"[dl_07] no rasters matching {args.pattern} in {args.in_dir}")

    gpkg = resolve_gpkg(args.gpkg)
    hucs = gpd.read_file(gpkg)
    if args.huc_field not in hucs.columns:
        sys.exit(f"[dl_07] '{args.huc_field}' not in {gpkg} "
                 f"(have: {list(hucs.columns)})")

    dest = "in place" if args.in_place else str(args.out_dir)
    print(f"[dl_07] {len(rasters)} raster(s) in {args.in_dir}")
    print(f"[dl_07] boundaries: {gpkg} ({len(hucs)} features)")
    print(f"[dl_07] writing: {dest}\n")

    if args.out_dir and not args.dry_run:
        args.out_dir.mkdir(parents=True, exist_ok=True)

    geom_cache = {}
    written = skipped = failed = 0

    for src_path in rasters:
        m = HUC_RE.search(src_path.name)
        if not m:
            print(f"  [skip] no _huc_ token: {src_path.name}")
            failed += 1
            continue
        huc = m.group(1)

        if args.in_place:
            dst_final = src_path
            if not args.overwrite and already_masked(src_path):
                skipped += 1
                continue
        else:
            dst_final = args.out_dir / src_path.name
            if dst_final.exists() and not args.overwrite:
                skipped += 1
                continue

        poly = hucs[hucs[args.huc_field] == huc]
        if poly.empty:
            print(f"  [skip] HUC {huc} not in {gpkg.name}: {src_path.name}")
            failed += 1
            continue

        tmp = dst_final.with_suffix(".dl07tmp.tif")
        try:
            kept = mask_one(src_path, tmp, geom_cache, huc, poly, args.dry_run)
        except Exception as exc:                      # noqa: BLE001
            if tmp.exists():
                tmp.unlink()
            print(f"  [fail] {src_path.name}: {exc}")
            failed += 1
            continue

        if args.dry_run:
            print(f"  [would mask] {src_path.name}  ->  {dest}")
            continue

        os.replace(tmp, dst_final)                    # atomic
        print(f"  {src_path.name}  kept {kept:.1f}% of pixels")
        written += 1

    print(f"\n[dl_07] written {written}, skipped {skipped} "
          f"(already masked/present), failed {failed}")
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
