#!/usr/bin/env python3
"""
Phase 1: Convert DL inference probability rasters -> web-ready COGs.

For each input *_probs.tif:
  - binary  (1 band):  scale prob 0-1 -> Byte 0-100, single band "confidence".
  - multiclass (N>=2): argmax over classes -> class band (Byte), and the max
                       prob at that pixel -> confidence band (Byte 0-100).
Outputs 2-band multiclass COGs (band 1 = class, band 2 = confidence) or
1-band binary COGs (band 1 = confidence). Writes a manifest.json for the viewer.

Nodata = 255 (outside the 0-100 data range).

Test-pair usage:
  python make_cogs.py \
    --inputs DLpred_binary_cluster_11_huc_042900030202_probs.tif \
             DLpred_multiclass_cluster_250_huc_041402011205_probs.tif \
    --outdir ./cogs

Batch usage:
  python make_cogs.py --inputs /path/to/*_probs.tif --outdir ./cogs
"""
from __future__ import annotations

import argparse
import json
import re
from pathlib import Path

import numpy as np
import rasterio
from rasterio.io import MemoryFile
from rasterio.warp import transform_bounds
from rio_cogeo.cogeo import cog_translate, cog_info
from rio_cogeo.profiles import cog_profiles

NODATA = 255
SCALE = 0.01  # stored_value * SCALE = probability (0-100 Byte -> 0.0-1.0)

# Pull cluster + huc out of the filename for the manifest / layer grouping.
_NAME_RE = re.compile(
    r"DLpred_(?P<kind>binary|multiclass)_cluster_(?P<cluster>\d+)_huc_(?P<huc>\d+)"
)


def parse_name(path: Path) -> dict:
    m = _NAME_RE.search(path.stem)
    if not m:
        return {"kind": None, "cluster": None, "huc": None}
    return m.groupdict()


def prob_to_byte(arr: np.ndarray, valid: np.ndarray) -> np.ndarray:
    """0-1 float prob -> 0-100 Byte, NODATA where invalid."""
    out = np.full(arr.shape, NODATA, dtype="uint8")
    scaled = np.clip(np.round(arr * 100.0), 0, 100).astype("uint8")
    out[valid] = scaled[valid]
    return out


def build_bands(src: rasterio.DatasetReader):
    """Return (data, count, band_desc) ready to write as a Byte raster."""
    n = src.count
    src_nodata = src.nodata

    if n == 1:
        band = src.read(1).astype("float32")
        valid = np.isfinite(band)
        if src_nodata is not None:
            valid &= band != src_nodata
        conf = prob_to_byte(band, valid)
        return conf[np.newaxis, ...], 1, ["confidence"]

    # multiclass: stack of per-class probs, shape (n, H, W)
    stack = src.read().astype("float32")  # (bands, H, W)
    # valid where at least one class is finite / not nodata
    finite = np.isfinite(stack)
    if src_nodata is not None:
        finite &= stack != src_nodata
    valid = finite.any(axis=0)

    # argmax class (0-indexed -> keep 0-indexed; document in manifest)
    filled = np.where(finite, stack, -np.inf)
    cls = np.argmax(filled, axis=0).astype("uint8")
    maxp = np.max(np.where(finite, stack, 0.0), axis=0)

    conf = prob_to_byte(maxp, valid)
    cls_out = np.where(valid, cls, NODATA).astype("uint8")
    return np.stack([cls_out, conf]), 2, ["class", "confidence"]


def convert_one(in_path: Path, outdir: Path, cfg: dict) -> dict:
    with rasterio.open(in_path) as src:
        data, count, descriptions = build_bands(src)
        profile = src.profile.copy()
        bounds = src.bounds
        crs = src.crs
        transform = src.transform
        height, width = src.height, src.width

    profile.update(
        driver="GTiff",
        dtype="uint8",
        count=count,
        nodata=NODATA,
        compress="deflate",
    )

    out_path = outdir / (in_path.stem.replace("_probs", "") + "_cog.tif")
    dst_profile = cog_profiles.get("deflate")

    with MemoryFile() as mem:
        with mem.open(
            driver="GTiff",
            width=width,
            height=height,
            count=count,
            dtype="uint8",
            crs=crs,
            transform=transform,
            nodata=NODATA,
        ) as tmp:
            tmp.write(data)
            for i, d in enumerate(descriptions, start=1):
                tmp.set_band_description(i, d)
            # self-describing scale so the value is interpretable downstream
            tmp.update_tags(SCALE=SCALE, PROB_BANDS="confidence")

        cog_translate(
            mem,
            out_path,
            dst_profile,
            use_cog_driver=True,
            in_memory=False,
            quiet=True,
        )

    info = cog_info(out_path)
    meta = parse_name(in_path)
    # bounds in WGS84 for the web viewer (leaflet expects lat/lon)
    ll = transform_bounds(crs, "EPSG:4326", *bounds)
    return {
        "file": out_path.name,
        "kind": meta["kind"],
        "cluster": meta["cluster"],
        "huc": meta["huc"],
        "bands": descriptions,
        "scale": SCALE,
        "nodata": NODATA,
        "bounds_wgs84": [ll[0], ll[1], ll[2], ll[3]],  # [W, S, E, N]
        "valid_cog": bool(info.COG),
    }


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--inputs", nargs="+", required=True, type=Path)
    ap.add_argument("--outdir", required=True, type=Path)
    # array-safe manifest handling:
    #   --frag-dir writes one manifest_<stem>.json per file (no clobber);
    #   run with --merge-only afterwards to combine fragments into manifest.json.
    ap.add_argument("--frag-dir", type=Path, default=None,
                    help="write a per-file manifest fragment here instead of manifest.json")
    ap.add_argument("--merge-only", action="store_true",
                    help="skip conversion; merge frag-dir/*.json into outdir/manifest.json")
    args = ap.parse_args()
    args.outdir.mkdir(parents=True, exist_ok=True)

    if args.merge_only:
        frag_dir = args.frag_dir or args.outdir
        frags = sorted(frag_dir.glob("manifest_*.json"))
        merged = [json.loads(f.read_text()) for f in frags]
        man_path = args.outdir / "manifest.json"
        man_path.write_text(json.dumps(merged, indent=2))
        print(f"Merged {len(merged)} fragments -> {man_path}")
        return

    manifest = []
    for p in args.inputs:
        print(f"-> {p.name}")
        entry = convert_one(p, args.outdir, {})
        ok = "OK" if entry["valid_cog"] else "INVALID COG"
        print(f"   {entry['file']}  [{ok}]  bands={entry['bands']}")
        manifest.append(entry)

    if args.frag_dir is not None:
        args.frag_dir.mkdir(parents=True, exist_ok=True)
        # one fragment per input; safe for concurrent SLURM array tasks
        for p, entry in zip(args.inputs, manifest):
            frag = args.frag_dir / f"manifest_{p.stem}.json"
            frag.write_text(json.dumps(entry, indent=2))
        print(f"Wrote {len(manifest)} fragment(s) to {args.frag_dir}")
    else:
        man_path = args.outdir / "manifest.json"
        man_path.write_text(json.dumps(manifest, indent=2))
        print(f"\nWrote {man_path} ({len(manifest)} entries)")


if __name__ == "__main__":
    main()