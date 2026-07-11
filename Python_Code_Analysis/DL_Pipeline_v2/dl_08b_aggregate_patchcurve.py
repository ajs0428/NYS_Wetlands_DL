"""
dl_08b_aggregate_patchcurve.py

Aggregation + plots for the two follow-on studies (plan Phases 4 & 5). Pure
pandas/matplotlib -- no GPU, no model loading. Safe to run on a PARTIAL tree
(reports only the cells it finds).

Two modes:

1. PATCH-COUNT CURVE (default). Walks the tree written by
   Shell_Scripts/run_patchcurve.sh --

       <results>/<config>_n<level>/seed<k>/{metrics.json, training_log.json}

   where <level> is 100/200/.../500 or "full". The curve's x-axis is the
   REALIZED training-set size (training_log.json -> data_split.train), not the
   requested cap, so it stays honest as the dataset grows.

   Outputs (under --output-dir, default <results>/analysis):
     patchcurve_long.csv   one row per (config, level, seed): macro_f1, mean_iou,
                           per-class IoU/F1/precision/recall, requested cap,
                           realized train/val/test
     patchcurve.png        macro-F1 & mean-IoU (left) and per-class IoU (right)
                           vs realized #train patches, mean +/- sd over seeds

2. ARCH COMPARISON (--arch-compare). Pairs the U-Net baseline against UNet3+ for
   one config, by seed (same seed -> same test patches), and writes:
     arch_compare.csv      per-seed and seed-mean U-Net vs UNet3+ + delta

Reads BOTH metrics.json schemas: flat (v1, dl_05) and nested under "test_metrics"
(v2, run_config.sh's trainer-journal extract). For v2 trees point at the MODE
subtree (…_v2/<mode> is added by run_config.sh).

Usage:
  # patch curve (v2)
  python dl_08b_aggregate_patchcurve.py --results-dir Models/results_patchcurve_v2/multiclass
  # arch comparison (v2)
  python dl_08b_aggregate_patchcurve.py --arch-compare --config fld_chmret_leafoff \
      --unet-dir Models/factorial_results_v2/multiclass \
      --unet3plus-dir Models/results_arch_v2/multiclass
"""

import argparse
import json
import re
from pathlib import Path
from typing import List, Optional

import numpy as np
import pandas as pd

# Match per dl_05 metrics.json per_class keys; we plot whatever classes appear.
LEVEL_RE = re.compile(r"_n(?P<level>\d+|full)$")


# --- shared loading ----------------------------------------------------------

def _seed_from_dir(p: Path) -> Optional[int]:
    name = p.name
    if name.startswith("seed") and name[4:].isdigit():
        return int(name[4:])
    return None


def _scores(metrics: dict) -> dict:
    """The score block of a metrics.json: nested under "test_metrics" in v2
    (run_config.sh's trainer-journal extract), flat at top level in v1 (dl_05)."""
    return metrics.get("test_metrics") or metrics


def _realized_split(cell: Path) -> dict:
    """Pull the realized train/val/test sizes from the cell's training_log.json.

    Each cell trains once, so we take the last journal entry. Returns {} if the
    log is missing (e.g. a crashed cell that still wrote metrics).
    """
    log = cell / "training_log.json"
    if not log.exists():
        return {}
    try:
        entries = json.loads(log.read_text())
    except json.JSONDecodeError:
        return {}
    if not entries:
        return {}
    last = entries[-1]
    split = last.get("data_split", {})
    cap = last.get("config", {}).get("n_patches")
    return {
        "train_patches": split.get("train"),
        "val_patches": split.get("val"),
        "test_patches": split.get("test"),
        "n_patches_requested": cap,
    }


def load_patchcurve(results_dir: Path) -> pd.DataFrame:
    """Walk <results>/<config>_n<level>/seed<k> into a long per-cell DataFrame."""
    rows: List[dict] = []
    for cfg_dir in sorted(results_dir.iterdir() if results_dir.is_dir() else []):
        if not cfg_dir.is_dir():
            continue
        m = LEVEL_RE.search(cfg_dir.name)
        if not m:
            continue
        level = m.group("level")
        config = cfg_dir.name[: m.start()]
        level_sort = np.inf if level == "full" else int(level)
        for seed_dir in sorted(cfg_dir.glob("seed*")):
            seed = _seed_from_dir(seed_dir)
            if seed is None:
                continue
            mfile = seed_dir / "metrics.json"
            if not mfile.exists():
                continue
            sc = _scores(json.loads(mfile.read_text()))
            row = {
                "config": config,
                "level": level,
                "level_sort": level_sort,
                "seed": seed,
                "macro_f1": sc.get("macro_f1"),
                "mean_iou": sc.get("mean_iou"),
                "overall_accuracy": sc.get("overall_accuracy"),
                **_realized_split(seed_dir),
            }
            for cls, cm in sc.get("per_class", {}).items():
                for metric in ("iou", "f1", "precision", "recall"):
                    row[f"{metric}_{cls}"] = cm.get(metric)
            rows.append(row)
    return pd.DataFrame(rows)


# --- patch-curve outputs -----------------------------------------------------

def write_patchcurve(df: pd.DataFrame, out_dir: Path) -> None:
    out_dir.mkdir(parents=True, exist_ok=True)
    df = df.sort_values(["config", "level_sort", "seed"])
    long_path = out_dir / "patchcurve_long.csv"
    df.drop(columns=["level_sort"]).to_csv(long_path, index=False)
    print(f"wrote {long_path}  ({len(df)} cells)")

    iou_cols = [c for c in df.columns if c.startswith("iou_")]
    # mean +/- sd over seeds, per (config, level), x = mean realized train size
    g = df.groupby(["config", "level_sort"], as_index=False)
    agg = {"macro_f1": ["mean", "std"], "mean_iou": ["mean", "std"],
           "train_patches": "mean"}
    agg.update({c: ["mean", "std"] for c in iou_cols})
    summary = g.agg(agg)
    summary.columns = ["_".join(c).rstrip("_") for c in summary.columns]
    summary = summary.rename(columns={"train_patches_mean": "train_patches"})
    summary_path = out_dir / "patchcurve_summary.csv"
    summary.to_csv(summary_path, index=False)
    print(f"wrote {summary_path}")

    try:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
    except ImportError:
        print("[warn] matplotlib not available; skipping patchcurve.png")
        return

    configs = sorted(summary["config"].unique())
    fig, axes = plt.subplots(1, 2, figsize=(13, 5))
    for config in configs:
        s = summary[summary["config"] == config].sort_values("train_patches")
        x = s["train_patches"]
        axes[0].errorbar(x, s["macro_f1_mean"], yerr=s["macro_f1_std"],
                         marker="o", capsize=3, label=f"{config} macro-F1")
        axes[0].errorbar(x, s["mean_iou_mean"], yerr=s["mean_iou_std"],
                         marker="s", linestyle="--", capsize=3, label=f"{config} mean-IoU")
        for c in iou_cols:
            cls = c.replace("iou_", "")
            axes[1].errorbar(x, s[f"{c}_mean"], yerr=s[f"{c}_std"],
                             marker="o", capsize=3, label=f"{cls}")
    axes[0].set_xlabel("realized #train patches")
    axes[0].set_ylabel("score")
    axes[0].set_title("Accuracy vs training-set size")
    axes[0].legend(fontsize=8)
    axes[0].grid(alpha=0.3)
    axes[1].set_xlabel("realized #train patches")
    axes[1].set_ylabel("per-class IoU")
    axes[1].set_title("Per-class IoU vs training-set size")
    axes[1].legend(fontsize=8)
    axes[1].grid(alpha=0.3)
    fig.tight_layout()
    png = out_dir / "patchcurve.png"
    fig.savefig(png, dpi=150)
    plt.close(fig)
    print(f"wrote {png}")


# --- arch comparison ---------------------------------------------------------

def _load_arch_cells(config_dir: Path, arch_label: str) -> pd.DataFrame:
    rows: List[dict] = []
    for seed_dir in sorted(config_dir.glob("seed*")):
        seed = _seed_from_dir(seed_dir)
        if seed is None:
            continue
        mfile = seed_dir / "metrics.json"
        if not mfile.exists():
            continue
        sc = _scores(json.loads(mfile.read_text()))
        row = {"arch": arch_label, "seed": seed,
               "macro_f1": sc.get("macro_f1"),
               "mean_iou": sc.get("mean_iou"),
               "overall_accuracy": sc.get("overall_accuracy")}
        for cls, cm in sc.get("per_class", {}).items():
            row[f"iou_{cls}"] = cm.get("iou")
        rows.append(row)
    return pd.DataFrame(rows)


def write_arch_compare(config: str, unet_dir: Path, unet3plus_dir: Path,
                       out_dir: Path) -> None:
    out_dir.mkdir(parents=True, exist_ok=True)
    unet = _load_arch_cells(unet_dir / config, "unet")
    u3p = _load_arch_cells(unet3plus_dir / f"{config}_unet3plus", "unet3plus")
    if unet.empty or u3p.empty:
        print(f"[warn] arch-compare: missing cells "
              f"(unet={len(unet)}, unet3plus={len(u3p)}); nothing written")
        return

    metric_cols = [c for c in unet.columns if c not in ("arch", "seed")]
    merged = unet.merge(u3p, on="seed", suffixes=("_unet", "_unet3plus"))
    for c in metric_cols:
        merged[f"delta_{c}"] = merged[f"{c}_unet3plus"] - merged[f"{c}_unet"]

    # seed-mean row appended for a quick headline.
    mean_row = {"seed": "mean"}
    for c in metric_cols:
        for suf in ("_unet", "_unet3plus", ""):
            col = f"delta_{c}" if suf == "" else f"{c}{suf}"
            mean_row[col] = merged[col].mean()
    merged_out = pd.concat([merged, pd.DataFrame([mean_row])], ignore_index=True)

    out = out_dir / "arch_compare.csv"
    merged_out.insert(0, "config", config)
    merged_out.to_csv(out, index=False)
    print(f"wrote {out}  (paired over {len(merged)} seeds)")
    print(merged_out[["config", "seed", "macro_f1_unet", "macro_f1_unet3plus",
                      "delta_macro_f1"]].to_string(index=False))


# --- CLI ---------------------------------------------------------------------

def main() -> None:
    ap = argparse.ArgumentParser(description="Patch-count curve / arch-compare aggregation")
    ap.add_argument("--arch-compare", action="store_true",
                    help="Run the U-Net vs UNet3+ comparison instead of the patch curve")
    # patch-curve args
    ap.add_argument("--results-dir", type=Path, default=Path("results_patchcurve"),
                    help="[patch curve] tree of <config>_n<level>/seed<k> cells")
    ap.add_argument("--output-dir", type=Path, default=None,
                    help="Output dir (default: <results-dir>/analysis)")
    # arch-compare args
    ap.add_argument("--config", type=str, help="[arch-compare] config name")
    ap.add_argument("--unet-dir", type=Path, default=Path("results"),
                    help="[arch-compare] U-Net baseline tree (<config>/seed<k>)")
    ap.add_argument("--unet3plus-dir", type=Path, default=Path("results_arch"),
                    help="[arch-compare] UNet3+ tree (<config>_unet3plus/seed<k>)")
    args = ap.parse_args()

    project_root = Path(__file__).parent.parent.parent

    def resolve(p: Path) -> Path:
        return p if p.is_absolute() else project_root / p

    if args.arch_compare:
        if not args.config:
            ap.error("--arch-compare requires --config")
        unet_dir = resolve(args.unet_dir)
        u3p_dir = resolve(args.unet3plus_dir)
        out_dir = resolve(args.output_dir) if args.output_dir else u3p_dir / "analysis"
        write_arch_compare(args.config, unet_dir, u3p_dir, out_dir)
        return

    results_dir = resolve(args.results_dir)
    out_dir = resolve(args.output_dir) if args.output_dir else results_dir / "analysis"
    df = load_patchcurve(results_dir)
    if df.empty:
        print(f"No patch-curve cells found under {results_dir}")
        return
    write_patchcurve(df, out_dir)


if __name__ == "__main__":
    main()
