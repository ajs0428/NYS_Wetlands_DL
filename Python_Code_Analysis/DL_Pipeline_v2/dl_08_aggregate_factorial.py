"""
dl_08_aggregate_factorial.py

Phase 3 (aggregation half) of the wetland factorial experiment. Walks the
results tree written by Shell_Scripts/run_config.sh --

    results/<config>/seed<k>/{metrics.json, manifest.json, confusion_matrix.csv}

-- into the factorial table and the headline contrasts. Pure pandas: no GPU, no
model loading. Safe to run on a PARTIAL tree while training is still going; it
reports coverage (cells found of the expected config x seed grid) and computes
every contrast it has the cells for, skipping the rest.

The config-name grammar (`<label>_<lidar>_<spectral>`) and the 8 configs come
from dl_experiment_config.py, the single source of truth -- so a design change
never drifts between the runner, the stats, and this aggregation.

Outputs (under --output-dir, default <results>/analysis):
  factorial_long.csv      one row per (config, seed, class): precision/recall/f1/iou/support
  factorial_summary.csv   mean & sd over seeds, per (config, class, metric)
  factorial_table.csv     headline pivot: FSW/UPL IoU+recall, macro-F1, mean-IoU (mean +/- sd)
  confusion_mean/<config>.csv   seed-mean confusion matrix per config (the FSW<->UPL cells)
  contrasts.csv           paired-by-seed effects: LiDAR tiers, leaf-off main effect,
                          LiDAR x leaf-off interaction, label gradient
  coverage.csv            which (config, seed) cells are present / missing

Because the same seed yields the same train/val/test split across all 8 configs
(plan Section 3), contrasts are computed as per-seed PAIRED differences, then
summarised mean +/- sd over the shared seeds -- the correct way to net out
split luck.

Usage:
  python dl_08_aggregate_factorial.py
  python dl_08_aggregate_factorial.py --results-dir Models/factorial_results \
                                      --output-dir  Models/factorial_results/analysis
"""

import argparse
import json
from pathlib import Path
from typing import Dict, List, Optional

import numpy as np
import pandas as pd

from dl_experiment_config import CONFIGS, get_config

# Headline classes for the FSW/UPL below-canopy story (plan Section 3). The
# aggregator still emits every class present in metrics.json; these only drive
# the compact factorial_table and the default contrast metrics.
FOCUS_CLASSES = ["FSW", "UPL"]
CONTRAST_METRICS = ["iou", "recall"]   # per-focus-class; plus macro_f1 / mean_iou globally


# --- Loading -----------------------------------------------------------------

def _seed_from_dir(p: Path) -> Optional[int]:
    """results/<config>/seed3 -> 3 ; None if the dir is not a seed cell."""
    name = p.name
    if name.startswith("seed") and name[4:].isdigit():
        return int(name[4:])
    return None


def _scores(metrics: dict) -> dict:
    """The score block of a metrics.json, schema-agnostic.

    v2 (run_config.sh) nests the scores under "test_metrics"; v1 (dl_05) wrote
    them flat at the top level. Everything downstream reads from this block, so a
    plain `metrics.get("macro_f1")` silently returned None on every v2 cell.
    """
    return metrics.get("test_metrics") or metrics


def _macro(sc: dict, key: str, per_class: dict) -> Optional[float]:
    """Cell-level macro-`key`, preferring the stored value, else the unweighted
    class mean. v2's test_metrics omits macro_recall/macro_precision but keeps
    them per class, so recover them the same way macro_f1 is defined (mean over
    scored classes)."""
    val = sc.get(f"macro_{key}")
    if val is not None:
        return val
    vals = [m[key] for m in per_class.values() if m.get(key) is not None]
    return float(np.mean(vals)) if vals else None


def load_cells(results_dir: Path) -> pd.DataFrame:
    """Walk results/<config>/seed<k>/metrics.json into a long per-class DataFrame.

    One row per (config, seed, class). Cell-level scalars (macro_f1, mean_iou,
    overall_accuracy, in_channels, label, git_commit) are repeated on each of
    that cell's class rows so the frame is self-contained for groupby.
    """
    rows: List[dict] = []
    for cfg_name in CONFIGS:
        cfg_dir = results_dir / cfg_name
        if not cfg_dir.is_dir():
            continue
        cfg = get_config(cfg_name)
        for seed_dir in sorted(cfg_dir.glob("seed*")):
            seed = _seed_from_dir(seed_dir)
            if seed is None:
                continue
            mfile = seed_dir / "metrics.json"
            if not mfile.exists():
                continue
            metrics = json.loads(mfile.read_text())
            sc = _scores(metrics)   # unwrap v2 "test_metrics"; flat for v1
            per_class = sc.get("per_class", {})

            # manifest is optional (a cell can finish metrics before manifest in
            # a crash); fall back to the config registry for the axis labels.
            man_file = seed_dir / "manifest.json"
            manifest = json.loads(man_file.read_text()) if man_file.exists() else {}

            cell_scalars = {
                "config": cfg_name,
                "label": cfg["label"],
                "lidar": cfg["lidar"],
                "spectral": cfg["spectral"],
                "featureset": f"{cfg['lidar']}_{cfg['spectral']}",
                "seed": seed,
                "macro_f1": sc.get("macro_f1"),
                "macro_recall": _macro(sc, "recall", per_class),
                "macro_precision": _macro(sc, "precision", per_class),
                "mean_iou": sc.get("mean_iou"),
                "overall_accuracy": sc.get("overall_accuracy"),
                "in_channels": manifest.get("in_channels", cfg["channels"]),
                "git_commit": manifest.get("git_commit"),
            }
            for cls_name, m in per_class.items():
                rows.append({
                    **cell_scalars,
                    "class": cls_name,
                    "precision": m.get("precision"),
                    "recall": m.get("recall"),
                    "f1": m.get("f1"),
                    "iou": m.get("iou"),
                    "support": m.get("support"),
                })
    if not rows:
        return pd.DataFrame()
    return pd.DataFrame(rows)


def coverage_table(long_df: pd.DataFrame, seeds: List[int]) -> pd.DataFrame:
    """Present/missing grid of (config x seed) cells against the expected seeds."""
    present = set(map(tuple, long_df[["config", "seed"]].drop_duplicates().values)) \
        if not long_df.empty else set()
    recs = []
    for cfg_name in CONFIGS:
        for s in seeds:
            recs.append({"config": cfg_name, "seed": s,
                         "present": (cfg_name, s) in present})
    return pd.DataFrame(recs)


# --- Summaries ---------------------------------------------------------------

def summarise(long_df: pd.DataFrame) -> pd.DataFrame:
    """Mean & sd over seeds, per (config, class, metric). Long form."""
    value_cols = ["precision", "recall", "f1", "iou"]
    g = (long_df
         .groupby(["config", "label", "lidar", "spectral", "featureset", "class"])[value_cols]
         .agg(["mean", "std", "count"]))
    g.columns = [f"{metric}_{stat}" for metric, stat in g.columns]
    return g.reset_index()


def factorial_table(long_df: pd.DataFrame) -> pd.DataFrame:
    """Compact headline pivot: one row per config, FSW/UPL IoU+recall + macro-F1.

    Each cell is a 'mean +/- sd (n)' string for quick reading; the machine-
    readable values live in factorial_summary.csv.
    """
    def fmt(series: pd.Series) -> str:
        vals = series.dropna()
        if vals.empty:
            return "--"
        if len(vals) == 1:
            return f"{vals.mean():.3f} (n=1)"
        return f"{vals.mean():.3f} +/- {vals.std():.3f} (n={len(vals)})"

    recs = []
    for cfg_name in CONFIGS:
        sub = long_df[long_df["config"] == cfg_name]
        if sub.empty:
            continue
        rec = {"config": cfg_name}
        for cls in FOCUS_CLASSES:
            cls_sub = sub[sub["class"] == cls]
            rec[f"{cls}_iou"] = fmt(cls_sub["iou"])
            rec[f"{cls}_recall"] = fmt(cls_sub["recall"])
        # macro_f1 / mean_iou are cell-level: one value per seed -> dedup on seed.
        cell_lvl = sub.drop_duplicates("seed")
        rec["macro_f1"] = fmt(cell_lvl["macro_f1"])
        rec["mean_iou"] = fmt(cell_lvl["mean_iou"])
        recs.append(rec)
    return pd.DataFrame(recs)


def _confusion(metrics: dict):
    """(matrix ndarray, labels) from a metrics.json, schema-agnostic.

    v2 (run_config.sh) stores confusion_matrix as {"labels": [...], "matrix":
    [[...]]}; v1 (dl_05) stored the bare nested list with labels living in
    per_class / class_names. Returns (None, None) if absent.
    """
    cm = metrics.get("confusion_matrix")
    if cm is None:
        return None, None
    if isinstance(cm, dict):                       # v2 self-describing form
        return np.array(cm["matrix"], dtype=float), cm.get("labels")
    labels = metrics.get("class_names") \
        or list(_scores(metrics).get("per_class", {}).keys())   # v1 bare list
    return np.array(cm, dtype=float), labels


def mean_confusion_matrices(results_dir: Path) -> Dict[str, pd.DataFrame]:
    """Seed-mean confusion matrix per config (true rows x predicted cols)."""
    out: Dict[str, pd.DataFrame] = {}
    for cfg_name in CONFIGS:
        cfg_dir = results_dir / cfg_name
        if not cfg_dir.is_dir():
            continue
        mats, class_names = [], None
        for seed_dir in sorted(cfg_dir.glob("seed*")):
            mfile = seed_dir / "metrics.json"
            if not mfile.exists():
                continue
            mat, labels = _confusion(json.loads(mfile.read_text()))
            if mat is None:
                continue
            mats.append(mat)
            if class_names is None:
                class_names = labels
        if mats:
            mean_cm = np.mean(mats, axis=0)
            labels = class_names or [f"c{i}" for i in range(mean_cm.shape[0])]
            out[cfg_name] = pd.DataFrame(mean_cm, index=labels, columns=labels)
    return out


# --- Contrasts (paired by seed) ----------------------------------------------

def _paired_delta(long_df: pd.DataFrame, cfg_a: str, cfg_b: str,
                  cls: str, metric: str) -> Optional[dict]:
    """metric(cfg_a) - metric(cfg_b) for class `cls`, paired on shared seeds.

    Returns mean/sd/n of the per-seed difference, or None if no shared seed has
    both members. Pairing on seed nets out split luck (plan Section 3).
    """
    def pick(cfg):
        s = long_df[(long_df["config"] == cfg) & (long_df["class"] == cls)]
        return s.set_index("seed")[metric]
    a, b = pick(cfg_a), pick(cfg_b)
    shared = sorted(set(a.index) & set(b.index))
    if not shared:
        return None
    diff = (a.loc[shared] - b.loc[shared]).dropna()
    if diff.empty:
        return None
    return {
        "contrast": f"{cfg_a} - {cfg_b}",
        "class": cls,
        "metric": metric,
        "delta_mean": round(float(diff.mean()), 4),
        "delta_sd": round(float(diff.std()), 4) if len(diff) > 1 else np.nan,
        "n_seeds": int(len(diff)),
    }


def compute_contrasts(long_df: pd.DataFrame) -> pd.DataFrame:
    """The headline contrasts (plan Section 5 Phase 3), each paired by seed.

      LiDAR tiers       : chm-nolidar, chmret-chm, chmret-nolidar  (per spectral)
      Leaf-off main eff : leafoff-leafon                           (per LiDAR tier)
      Interaction       : (leafoff-leafon)@chmret - (leafoff-leafon)@nolidar
      Label gradient    : nwi-fld, flddeg-fld, nwi-flddeg          (full feature set)

    Metrics: FSW & UPL {iou,recall}. Field-only configs drive the feature
    ablations; the label gradient uses the chmret_leafoff cells.
    """
    recs: List[dict] = []

    def add(a, b, group, note=""):
        for cls in FOCUS_CLASSES:
            for metric in CONTRAST_METRICS:
                r = _paired_delta(long_df, a, b, cls, metric)
                if r is not None:
                    r["group"] = group
                    r["note"] = note
                    recs.append(r)

    # 1. LiDAR tiers (field labels), at each spectral level.
    for spec in ("leafon", "leafoff"):
        add(f"fld_chm_{spec}",    f"fld_nolidar_{spec}", "lidar_tier", f"chm gain ({spec})")
        add(f"fld_chmret_{spec}", f"fld_chm_{spec}",     "lidar_tier", f"returns gain ({spec})")
        add(f"fld_chmret_{spec}", f"fld_nolidar_{spec}", "lidar_tier", f"full LiDAR gain ({spec})")

    # 2. Leaf-off main effect, at each LiDAR tier (field labels).
    for lidar in ("nolidar", "chm", "chmret"):
        add(f"fld_{lidar}_leafoff", f"fld_{lidar}_leafon", "leafoff_main",
            f"leaf-off gain ({lidar})")

    # 3. Label gradient at the full feature set (chmret_leafoff).
    add("nwi_chmret_leafoff",    "fld_chmret_leafoff",    "label_gradient", "NWI vs field")
    add("flddeg_chmret_leafoff", "fld_chmret_leafoff",    "label_gradient", "degraded vs field (quantity)")
    add("nwi_chmret_leafoff",    "flddeg_chmret_leafoff", "label_gradient", "NWI vs degraded (correctness)")

    df = pd.DataFrame(recs)

    # 4. LiDAR x leaf-off interaction, computed PER SEED as a 4-cell contrast at
    #    the LiDAR extremes (all four configs share a given seed's split, so this
    #    is a valid paired quantity -> proper mean/sd/n, exactly like the
    #    first-order rows):
    #        I_s = [chmret_leafoff - chmret_leafon] - [nolidar_leafoff - nolidar_leafon]  (at seed s)
    #    Sign: >0 the modalities are COMPLEMENTARY (leaf-off still helps once you
    #    have LiDAR); ~0 additive/independent; <0 REDUNDANT (they capture the same
    #    below-canopy signal, so the second adds little).
    inter = []
    for cls in FOCUS_CLASSES:
        for metric in CONTRAST_METRICS:
            def seed_series(cfg):
                sub = long_df[(long_df["config"] == cfg) & (long_df["class"] == cls)]
                return sub.set_index("seed")[metric]
            a, b = seed_series("fld_chmret_leafoff"),  seed_series("fld_chmret_leafon")
            c, d = seed_series("fld_nolidar_leafoff"), seed_series("fld_nolidar_leafon")
            shared = sorted(set(a.index) & set(b.index) & set(c.index) & set(d.index))
            if not shared:
                continue
            i_s = ((a.loc[shared] - b.loc[shared]) - (c.loc[shared] - d.loc[shared])).dropna()
            if i_s.empty:
                continue
            inter.append({
                "group": "interaction",
                "contrast": "(leafoff-leafon)@chmret - @nolidar",
                "class": cls, "metric": metric,
                "delta_mean": round(float(i_s.mean()), 4),
                "delta_sd": round(float(i_s.std()), 4) if len(i_s) > 1 else np.nan,
                "n_seeds": int(len(i_s)),
                "note": "LiDAR x leaf-off (>0 complementary, <0 redundant)",
            })
    if inter:
        df = pd.concat([df, pd.DataFrame(inter)], ignore_index=True)

    if not df.empty:
        cols = ["group", "contrast", "class", "metric",
                "delta_mean", "delta_sd", "n_seeds", "note"]
        df = df[cols]
    return df


# --- Driver ------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    project_root = Path(__file__).parent.parent.parent
    ap.add_argument("--results-dir", type=Path, default=project_root / "results",
                    help="Root holding <config>/seed<k>/ cells (default: <repo>/results)")
    ap.add_argument("--output-dir", type=Path, default=None,
                    help="Where to write the CSVs (default: <results-dir>/analysis)")
    ap.add_argument("--seeds", type=int, nargs="+", default=[0, 1, 2, 3, 4],
                    help="Expected replicate seeds, for the coverage report (default: 0 1 2 3 4)")
    args = ap.parse_args()

    results_dir = args.results_dir if args.results_dir.is_absolute() \
        else project_root / args.results_dir
    out_dir = args.output_dir or (results_dir / "analysis")
    out_dir.mkdir(parents=True, exist_ok=True)

    long_df = load_cells(results_dir)
    cov = coverage_table(long_df, args.seeds)
    cov.to_csv(out_dir / "coverage.csv", index=False)

    n_present = int(cov["present"].sum())
    n_total = len(cov)
    print(f"Coverage: {n_present}/{n_total} cells present "
          f"({len(CONFIGS)} configs x {len(args.seeds)} seeds).")

    if long_df.empty:
        print(f"No completed cells under {results_dir} yet -- wrote coverage.csv only.")
        print("Re-run as cells finish; every output below fills in incrementally.")
        return

    long_df.to_csv(out_dir / "factorial_long.csv", index=False)
    summarise(long_df).to_csv(out_dir / "factorial_summary.csv", index=False)

    table = factorial_table(long_df)
    table.to_csv(out_dir / "factorial_table.csv", index=False)

    cm_dir = out_dir / "confusion_mean"
    cm_dir.mkdir(exist_ok=True)
    for cfg_name, cm_df in mean_confusion_matrices(results_dir).items():
        cm_df.to_csv(cm_dir / f"{cfg_name}.csv")

    contrasts = compute_contrasts(long_df)
    contrasts.to_csv(out_dir / "contrasts.csv", index=False)

    # Console preview of the two things you actually read first.
    print(f"\nWrote {out_dir}/ :")
    for f in ("factorial_long.csv", "factorial_summary.csv", "factorial_table.csv",
              "contrasts.csv", "coverage.csv", "confusion_mean/<config>.csv"):
        print(f"  {f}")
    print("\n--- Factorial table (headline) ---")
    with pd.option_context("display.max_columns", None, "display.width", 200):
        print(table.to_string(index=False))
    if not contrasts.empty:
        print("\n--- Contrasts (paired by seed; delta = first - second) ---")
        with pd.option_context("display.max_columns", None, "display.width", 200):
            print(contrasts.to_string(index=False))


if __name__ == "__main__":
    main()
