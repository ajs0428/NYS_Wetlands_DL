"""
dl_09b_shap_geomorph_export.py

Within-band SHAP channel breakdown -> CSV. Pure numpy/pandas, CPU, no model
loading: it re-aggregates the spatially-averaged per-channel |SHAP| arrays that
dl_07 persists as <cell>/shap/*_shap_per_channel.npz (the JSON keeps band-level
aggregates only, so the one-hot channels of Geomorph_local are invisible to it).

Walks a factorial results tree (point at the MODE subtree for v2, e.g.
Models/factorial_results_v2/multiclass) --

    <results>/<config>/seed<k>/shap/*_shap_per_channel.npz

and writes one long CSV (default <results>/analysis/shap_geomorph_channels.csv)
with a row per (config, seed, class, channel):

    config, seed, class, channel_index, geomorphon_value, geomorphon_form,
    mean_abs_shap

`class` includes each model class plus "overall" (mean across classes, matching
dl_07's overall aggregation). Channel k of the one-hot band corresponds to
raster value class_range[0]+k; for Geomorph_local (rgeomorphon, GRASS
r.geomorphon convention) values 1..10 are:

    1 flat, 2 peak, 3 ridge, 4 shoulder, 5 spur,
    6 slope, 7 hollow, 8 footslope, 9 valley, 10 pit

Usage:
  python dl_09b_shap_geomorph_export.py --results-dir Models/factorial_results_v2/multiclass
  python dl_09b_shap_geomorph_export.py --results-dir Models/factorial_results_v2/binary
"""

import argparse
from pathlib import Path

import numpy as np
import pandas as pd

GEOMORPHON_FORMS = [
    "flat", "peak", "ridge", "shoulder", "spur",
    "slope", "hollow", "footslope", "valley", "pit",
]


def export_band_channels(results_dir: Path, band: str, first_value: int) -> pd.DataFrame:
    rows = []
    for npz_path in sorted(results_dir.glob("*/seed*/shap/*_shap_per_channel.npz")):
        seed_dir = npz_path.parent.parent
        config = seed_dir.parent.name
        if not (seed_dir.name.startswith("seed") and seed_dir.name[4:].isdigit()):
            continue
        seed = int(seed_dir.name[4:])

        z = np.load(npz_path)
        mask = z["channel_band"] == band
        if not mask.any():
            continue  # config without this band
        shap_abs = z["shap_abs"][:, :, mask]          # (classes, samples, band channels)
        class_names = list(z["class_names"])

        per_class = shap_abs.mean(axis=1)             # (classes, band channels)
        blocks = list(zip(class_names, per_class)) + [("overall", per_class.mean(axis=0))]
        for cls, values in blocks:
            for k, v in enumerate(values):
                value = first_value + k
                form = (GEOMORPHON_FORMS[k] if band == "Geomorph_local"
                        and k < len(GEOMORPHON_FORMS) else str(value))
                rows.append({
                    "config": config,
                    "seed": seed,
                    "class": cls,
                    "channel_index": k,
                    "geomorphon_value": value,
                    "geomorphon_form": form,
                    "mean_abs_shap": float(v),
                })
    return pd.DataFrame(rows)


def main() -> None:
    ap = argparse.ArgumentParser(description="Export one-hot-band SHAP channel breakdown to CSV")
    ap.add_argument("--results-dir", type=Path, required=True,
                    help="Factorial results tree (v2: the mode subtree)")
    ap.add_argument("--band", default="Geomorph_local",
                    help="One-hot band to break out (default Geomorph_local)")
    ap.add_argument("--first-value", type=int, default=1,
                    help="Raster value of channel 0 (dl_band_config class_range[0])")
    ap.add_argument("--output", type=Path, default=None,
                    help="Output CSV (default <results-dir>/analysis/shap_geomorph_channels.csv)")
    args = ap.parse_args()

    df = export_band_channels(args.results_dir, args.band, args.first_value)
    if df.empty:
        print(f"[warn] no {args.band} channels found under {args.results_dir}; nothing written")
        return

    out = args.output or args.results_dir / "analysis" / "shap_geomorph_channels.csv"
    out.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(out, index=False)
    n_cells = df.groupby(["config", "seed"]).ngroups
    print(f"wrote {out}  ({len(df)} rows from {n_cells} cells)")


if __name__ == "__main__":
    main()
