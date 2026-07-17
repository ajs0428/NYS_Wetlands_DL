#!/usr/bin/env python3
"""
Phase 1: Convert DL inference probability rasters -> web-ready COGs.

Band layout (all Byte, probs scaled 0-100). Band 1 is always the argmax class;
class names are read from the source band descriptions ("<NAME> Probability"):
  - multiclass (EMW/FSW/SSW/UPL): 5 bands — class, EMW_prob, FSW_prob,
    SSW_prob, UPL_prob (full per-class probability surfaces kept).
  - binary (WET/UPL): 2 bands — class, WET_prob. UPL probability is dropped
    by design (softmax pair: UPL = 1 - WET).
Writes a manifest.json (or per-file fragments) for the viewer, including the
class-integer -> name mapping and the prediction version (from the input dir).

Nodata = 255 (outside the 0-100 data range).

Test-pair usage:
  python python_make_cogs.py \
    --inputs DLpred_binary_cluster_11_huc_042900030202_probs.tif \
             DLpred_multiclass_cluster_250_huc_041402011205_probs.tif \
    --outdir ./cogs

Batch usage (per-file, SLURM-array-safe):
  python python_make_cogs.py --inputs one_probs.tif --outdir ./cogs --frag-dir ./frags
  python python_make_cogs.py --outdir ./cogs --frag-dir ./frags --merge-only
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


def class_names_from_descriptions(descriptions) -> list[str]:
    """'EMW Probability' -> 'EMW'; fall back to positional names."""
    names = []
    for i, d in enumerate(descriptions):
        d = (d or "").strip()
        if d.lower().endswith("probability"):
            d = d[: -len("probability")].strip()
        names.append(d or f"class_{i}")
    return names


def build_bands(src: rasterio.DatasetReader):
    """Return (data, band_desc, class_names) ready to write as a Byte raster."""
    src_nodata = src.nodata

    # stack of per-class probs, shape (n, H, W)
    stack = src.read().astype("float32")
    class_names = class_names_from_descriptions(src.descriptions)
    finite = np.isfinite(stack)
    if src_nodata is not None:
        finite &= stack != src_nodata
    valid = finite.any(axis=0)  # valid where at least one class has data

    # argmax class (0-indexed -> keep 0-indexed; names go in the manifest)
    filled = np.where(finite, stack, -np.inf)
    cls = np.argmax(filled, axis=0).astype("uint8")
    cls_out = np.where(valid, cls, NODATA).astype("uint8")

    # binary keeps only the WET probability (UPL = 1 - WET, dropped by design);
    # multiclass keeps every per-class probability surface
    if [c.upper() for c in class_names] == ["WET", "UPL"]:
        keep = [0]
    else:
        keep = list(range(src.count))

    bands = [cls_out] + [prob_to_byte(stack[i], finite[i]) for i in keep]
    descriptions = ["class"] + [f"{class_names[i]}_prob" for i in keep]
    return np.stack(bands), descriptions, class_names


def infer_version(in_path: Path) -> str:
    """HUC_DL_Predictions_v2/... -> 'v2'; unsuffixed dirs -> 'v1'."""
    m = re.search(r"_v(\d+)$", in_path.resolve().parent.name)
    return f"v{m.group(1)}" if m else "v1"


def convert_one(in_path: Path, outdir: Path, version: str | None = None) -> dict:
    with rasterio.open(in_path) as src:
        data, descriptions, class_names = build_bands(src)
        bounds = src.bounds
        crs = src.crs
        transform = src.transform
        height, width = src.height, src.width
    count = data.shape[0]

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
            # self-describing scale/classes so values are interpretable downstream
            tmp.update_tags(
                SCALE=SCALE,
                CLASSES=",".join(class_names),
                PROB_BANDS=",".join(d for d in descriptions if d.endswith("_prob")),
            )

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
        "version": version or infer_version(in_path),
        "cluster": meta["cluster"],
        "huc": meta["huc"],
        "bands": descriptions,
        "classes": class_names,  # class band value i -> classes[i]
        "scale": SCALE,
        "nodata": NODATA,
        "bounds_wgs84": [ll[0], ll[1], ll[2], ll[3]],  # [W, S, E, N]
        "valid_cog": bool(info.COG),
    }


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--inputs", nargs="+", type=Path, default=[],
                    help="*_probs.tif inputs (not needed with --merge-only)")
    ap.add_argument("--outdir", required=True, type=Path)
    ap.add_argument("--version", default=None,
                    help="prediction version for the manifest (default: inferred "
                         "from the input dir suffix, e.g. ..._v2 -> v2)")
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

    if not args.inputs:
        ap.error("--inputs is required unless --merge-only is given")

    manifest = []
    for p in args.inputs:
        print(f"-> {p.name}")
        entry = convert_one(p, args.outdir, args.version)
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