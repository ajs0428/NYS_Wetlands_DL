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

2. ARCH COMPARISON (--arch-compare). Compares N architecture arms on ONE config,
   paired by seed (same seed -> same test patches -> an identical evaluation set
   for every arm). Each arm is `--arch-dir <name>=<root>`; the cell inside a root
   is <config>_<name>, or plain <config> for the base grid. v3 runs three arms
   (unet, unet3plus, mbfusion); two still works, and the deprecated
   --unet-dir/--unet3plus-dir flags reproduce the v2 output.

   Outputs (under --output-dir, default <last arm's root>/<mode>/analysis):
     arch_compare_long.csv one row per (arch, seed): every metric + cost. The
                           tidy form -- prefer it for plotting.
     arch_contrasts.csv    paired per-seed deltas vs the baseline arm, with
                           n_better/n_seeds sign consistency. At n=5 that is the
                           credible statistic; no p-values are computed.
     arch_cost.csv         params / GFLOPs / sec-per-epoch per arm, and params
                           as a multiple of the baseline's.
     arch_compare.csv      wide per-seed table + seed-mean row (v2-compatible)

   Named contrast: --confusion-pair (default FSW UPL) adds row-normalized
   directional confusion rates, i.e. the share of true-FSW pixels predicted UPL
   and vice versa -- the specific failure the fusion encoder targets. Absent
   classes (binary mode) are skipped rather than erroring.

Reads BOTH metrics.json schemas: flat (v1, dl_05) and nested under "test_metrics"
(v2, run_config.sh's trainer-journal extract). For v2 trees point at the MODE
subtree (…_v2/<mode> is added by run_config.sh).

Usage:
  # patch curve (v2)
  python dl_08b_aggregate_patchcurve.py --results-dir Models/results_patchcurve_v2/multiclass
  # arch comparison (v3, three arms)
  python dl_08b_aggregate_patchcurve.py --arch-compare \
      --config fld_chmret_leafoff --mode multiclass \
      --arch-dir unet=Models/factorial_results_v3 \
      --arch-dir unet3plus=Models/results_arch_v3 \
      --arch-dir mbfusion=Models/results_arch_fusion_v3
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


# --- arch comparison ------------------------------------------------------
#
# N arms, not two. Each arm is a (name, root) pair; the cell directory inside a
# root is <config>_<name> (the arch drivers' CELL_NAME) or plain <config> (the
# base grid, whose cells are not arch-suffixed), tried in that order.

def _cell_dir(root: Path, config: str, arch: str) -> Optional[Path]:
    """Locate one arm's cell dir. <config>_<arch> first so a root that happens to
    hold both (a shared results tree) resolves to the arch-specific cell."""
    for cand in (root / f"{config}_{arch}", root / config):
        if cand.is_dir():
            return cand
    return None


def _apply_mode(root: Path, mode: Optional[str]) -> Path:
    """run_config.sh writes <root>/<mode>/<cell>/seed<k>. Accept either form:
    a root that already ends in the mode, or one that needs it appended."""
    if not mode or root.name == mode:
        return root
    sub = root / mode
    return sub if sub.is_dir() else root


def _confusion_rates(cell: Path, class_names: List[str],
                     pair: Optional[tuple]) -> dict:
    """Directional confusion rates for one named class pair.

    confusion_matrix.csv is headerless with rows = true, cols = predicted, in the
    cell's class order. Rates are row-normalized, so `conf_A_as_B` reads as "share
    of true-A pixels predicted B" -- comparable across cells with different
    class prevalence, which raw counts are not.
    """
    if not pair:
        return {}
    a, b = pair
    if a not in class_names or b not in class_names:
        return {}       # e.g. FSW/UPL requested on a binary-mode cell
    cm_file = cell / "confusion_matrix.csv"
    if not cm_file.exists():
        return {}
    try:
        cm = pd.read_csv(cm_file, header=None).to_numpy(dtype=float)
    except (ValueError, pd.errors.EmptyDataError):
        return {}
    if cm.shape != (len(class_names), len(class_names)):
        return {}
    ia, ib = class_names.index(a), class_names.index(b)
    sa, sb = cm[ia].sum(), cm[ib].sum()
    out = {
        f"conf_{a}_as_{b}": cm[ia, ib] / sa if sa else np.nan,
        f"conf_{b}_as_{a}": cm[ib, ia] / sb if sb else np.nan,
    }
    # Symmetric swap rate: total mutually-confused pixels over the two classes'
    # combined support. One number for "how much do these two blur together".
    out[f"conf_{a}_{b}_swap"] = ((cm[ia, ib] + cm[ib, ia]) / (sa + sb)
                                 if (sa + sb) else np.nan)
    return out


def _safetensors_params(cell: Path) -> Optional[int]:
    """Exact parameter count from a .safetensors header -- no torch needed.

    Layout: u64 little-endian header length, then that many bytes of JSON
    mapping tensor name -> {dtype, shape, data_offsets}.
    """
    import struct
    for f in sorted(cell.glob("*.safetensors")):
        try:
            with open(f, "rb") as fh:
                (n,) = struct.unpack("<Q", fh.read(8))
                header = json.loads(fh.read(n))
        except (OSError, ValueError, struct.error):
            continue
        total = 0
        for name, spec in header.items():
            if name == "__metadata__" or not isinstance(spec, dict):
                continue
            shape = spec.get("shape") or []
            total += int(np.prod(shape)) if shape else 1
        if total:
            return total
    return None


_LOG_PARAMS = re.compile(r"Total params:\s*([\d.]+)\s*([KMB]?)")
_LOG_FLOPS = re.compile(r"Total FLOPs:\s*([\d.]+)\s*([KMBGT]?)")
_SI = {"": 1, "K": 1e3, "M": 1e6, "B": 1e9, "G": 1e9, "T": 1e12}


def _train_log_costs(cell: Path) -> dict:
    """Params/FLOPs from Lightning's model summary in train.log.

    Fallback only: the summary is rounded ("125 M"), and the FLOPs line exists
    only in the Lightning versions that print it. Params prefer the journal's
    exact count; FLOPs have no other source, so a rounded value beats none.
    """
    log = cell / "train.log"
    if not log.exists():
        return {}
    text = log.read_text(errors="replace")
    out = {}
    if (m := _LOG_PARAMS.search(text)):
        out["params_log"] = float(m.group(1)) * _SI.get(m.group(2), 1)
    if (m := _LOG_FLOPS.search(text)):
        out["gflops"] = float(m.group(1)) * _SI.get(m.group(2), 1) / 1e9
    return out


def _cost(cell: Path) -> dict:
    """Cost provenance for one cell, best-effort across three sources.

    Params: the trainer's journal `cost` block (exact) -> the .safetensors header
    (exact, but absent from a --metrics-only pull) -> train.log (rounded).
    Timing comes only from the journal, so pre-v3 cells report NaN rather than a
    guess reconstructed from file mtimes.
    """
    out = {"params": np.nan, "gflops": np.nan, "sec_per_epoch": np.nan,
           "epochs_run": np.nan, "fit_seconds": np.nan}
    log = cell / "training_log.json"
    journal_cost = {}
    if log.exists():
        try:
            entries = json.loads(log.read_text())
            journal_cost = (entries[-1].get("cost") or {}) if entries else {}
        except (json.JSONDecodeError, AttributeError, IndexError):
            journal_cost = {}
    for k in ("params", "sec_per_epoch", "epochs_run", "fit_seconds"):
        if journal_cost.get(k) is not None:
            out[k] = journal_cost[k]

    from_log = _train_log_costs(cell)
    out["gflops"] = from_log.get("gflops", np.nan)
    if np.isnan(out["params"]):
        st = _safetensors_params(cell)
        out["params"] = st if st is not None else from_log.get("params_log", np.nan)
    return out


def _load_arch_cells(cell_dir: Path, arch_label: str,
                     confusion_pair: Optional[tuple] = None) -> pd.DataFrame:
    rows: List[dict] = []
    for seed_dir in sorted(cell_dir.glob("seed*")):
        seed = _seed_from_dir(seed_dir)
        if seed is None:
            continue
        mfile = seed_dir / "metrics.json"
        if not mfile.exists():
            continue
        metrics = json.loads(mfile.read_text())
        sc = _scores(metrics)
        per_class = sc.get("per_class", {})
        row = {"arch": arch_label, "seed": seed,
               "macro_f1": sc.get("macro_f1"),
               "mean_iou": sc.get("mean_iou"),
               "overall_accuracy": sc.get("overall_accuracy")}
        for cls, cm in per_class.items():
            row[f"iou_{cls}"] = cm.get("iou")
            row[f"f1_{cls}"] = cm.get("f1")
        row.update(_confusion_rates(seed_dir, list(per_class), confusion_pair))
        row.update(_cost(seed_dir))
        rows.append(row)
    return pd.DataFrame(rows)


# Cost columns describe the model, not its accuracy: they are reported in their
# own table and excluded from the paired-delta contrasts.
COST_COLS = ("params", "gflops", "sec_per_epoch", "epochs_run", "fit_seconds")


def _contrasts(long: pd.DataFrame, baseline: str, config: str,
               mode: Optional[str]) -> pd.DataFrame:
    """Paired per-seed deltas of every arm against the baseline.

    Same seed => same test patches, so each seed gives both arms an identical
    evaluation set and the difference is paired. At n=5, sign consistency
    (`n_better`/`n_seeds`) is the credible summary; delta_sd is reported for
    magnitude, not for a t-test.
    """
    metric_cols = [c for c in long.columns
                   if c not in ("arch", "seed") and c not in COST_COLS]
    base = long[long["arch"] == baseline]
    rows: List[dict] = []
    for arch in [a for a in long["arch"].unique() if a != baseline]:
        arm = long[long["arch"] == arch]
        shared = sorted(set(base["seed"]) & set(arm["seed"]))
        if not shared:
            print(f"[warn] {arch}: no seeds shared with {baseline}; skipped")
            continue
        b = base[base["seed"].isin(shared)].set_index("seed").sort_index()
        a = arm[arm["seed"].isin(shared)].set_index("seed").sort_index()
        for m in metric_cols:
            if m not in b or m not in a:
                continue
            d = (a[m] - b[m]).dropna()
            if d.empty:
                continue
            # "Better" is direction-aware: for a confusion RATE, lower is better.
            better = (d < 0) if m.startswith("conf_") else (d > 0)
            rows.append({
                "config": config, "mode": mode, "arch": arch,
                "baseline": baseline, "metric": m,
                "n_seeds": int(len(d)),
                "baseline_mean": b.loc[d.index, m].mean(),
                "arch_mean": a.loc[d.index, m].mean(),
                "delta_mean": d.mean(), "delta_sd": d.std(ddof=1) if len(d) > 1 else np.nan,
                "delta_min": d.min(), "delta_max": d.max(),
                "n_better": int(better.sum()),
                "sign_consistent": bool(better.all() or (~better).all()),
                "lower_is_better": m.startswith("conf_"),
            })
    return pd.DataFrame(rows)


def write_arch_compare(config: str, arms: List[tuple], out_dir: Path,
                       baseline: Optional[str] = None,
                       mode: Optional[str] = None,
                       confusion_pair: Optional[tuple] = None) -> None:
    """arms: list of (arch_name, root_path) in the order given on the CLI."""
    out_dir.mkdir(parents=True, exist_ok=True)
    frames, missing = [], []
    for name, root in arms:
        cell = _cell_dir(_apply_mode(root, mode), config, name)
        if cell is None:
            missing.append(f"{name} (no {config}[_{name}] under {root})")
            continue
        df = _load_arch_cells(cell, name, confusion_pair)
        if df.empty:
            missing.append(f"{name} (no seed*/metrics.json in {cell})")
            continue
        frames.append(df)
    for m in missing:
        print(f"[warn] arch-compare: missing arm -- {m}")
    if not frames:
        print("[warn] arch-compare: no arms found; nothing written")
        return

    long = pd.concat(frames, ignore_index=True)
    long.insert(0, "mode", mode)
    long.insert(0, "config", config)
    long_path = out_dir / "arch_compare_long.csv"
    long.sort_values(["arch", "seed"]).to_csv(long_path, index=False)
    print(f"wrote {long_path}  ({len(long)} cells, "
          f"{long['arch'].nunique()} arms)")

    baseline = baseline or arms[0][0]
    if baseline not in set(long["arch"]):
        print(f"[warn] baseline '{baseline}' has no cells; "
              f"falling back to '{long['arch'].iloc[0]}'")
        baseline = long["arch"].iloc[0]

    # Seed coverage per arm -- the paired contrasts use the intersection, so a
    # short arm quietly shrinks n. Print it rather than let it pass unnoticed.
    print("\nseed coverage:")
    for arch, grp in long.groupby("arch", sort=False):
        print(f"  {arch:<12} n={len(grp)}  seeds={sorted(grp['seed'])}")
    # Each contrast pairs its arm against the baseline on THEIR shared seeds, so a
    # short arm costs only its own n. This all-arm intersection is the figure to
    # quote when the three arms are plotted together.
    shared = set.intersection(*(set(g["seed"]) for _, g in long.groupby("arch")))
    print(f"  common to all arms: {len(shared)} seed(s) {sorted(shared)}\n")

    contrasts = _contrasts(long.drop(columns=["config", "mode"]),
                           baseline, config, mode)
    if not contrasts.empty:
        cpath = out_dir / "arch_contrasts.csv"
        contrasts.to_csv(cpath, index=False)
        print(f"wrote {cpath}  ({len(contrasts)} rows, baseline={baseline})")
        head = contrasts[contrasts["metric"].isin(
            ["macro_f1", "mean_iou"] + [c for c in contrasts["metric"] if c.startswith("conf_")])]
        if not head.empty:
            print(head[["arch", "metric", "baseline_mean", "arch_mean",
                        "delta_mean", "n_better", "n_seeds"]].to_string(index=False))

    cost = (long.groupby("arch", sort=False)[list(COST_COLS)]
                .mean().reset_index())
    if not cost["params"].isna().all():
        base_params = cost.loc[cost["arch"] == baseline, "params"]
        if len(base_params) and base_params.iloc[0]:
            cost["params_x_baseline"] = cost["params"] / base_params.iloc[0]
    cost.insert(0, "mode", mode)
    cost.insert(0, "config", config)
    cpath = out_dir / "arch_cost.csv"
    cost.to_csv(cpath, index=False)
    print(f"\nwrote {cpath}")
    print(cost.to_string(index=False))

    # Wide table, back-compatible with the two-arm v2 output: one column per
    # (metric, arch) plus delta_<metric>_<arch> against the baseline. With
    # exactly one non-baseline arm the delta columns keep their v2 names.
    # Cost columns stay out -- they live in arch_cost.csv (and per-seed in the
    # long table), and here they would masquerade as metrics in a name-pattern
    # pivot of "everything ending in _<arch>".
    metric_cols = [c for c in long.columns
                   if c not in ("config", "mode", "arch", "seed")
                   and c not in COST_COLS]
    wide = None
    for arch, grp in long.groupby("arch", sort=False):
        g = grp.set_index("seed")[metric_cols].add_suffix(f"_{arch}")
        wide = g if wide is None else wide.join(g, how="outer")
    others = [a for a in long["arch"].unique() if a != baseline]
    for arch in others:
        for m in metric_cols:
            bcol, acol = f"{m}_{baseline}", f"{m}_{arch}"
            if bcol in wide and acol in wide:
                suffix = "" if len(others) == 1 else f"_{arch}"
                wide[f"delta_{m}{suffix}"] = wide[acol] - wide[bcol]
    wide = wide.reset_index()
    mean_row = {"seed": "mean"}
    for c in wide.columns:
        if c != "seed":
            mean_row[c] = wide[c].mean()
    wide = pd.concat([wide, pd.DataFrame([mean_row])], ignore_index=True)
    wide.insert(0, "mode", mode)
    wide.insert(0, "config", config)
    wpath = out_dir / "arch_compare.csv"
    wide.to_csv(wpath, index=False)
    print(f"\nwrote {wpath}  (wide; outer join, {len(shared)} seeds common to all arms)")


# --- CLI ---------------------------------------------------------------------

class _ArchDirAction(argparse.Action):
    """--arch-dir name=path, repeatable; order is preserved (arm 1 = default baseline)."""

    def __call__(self, parser, namespace, values, option_string=None):
        if "=" not in values:
            parser.error(f"--arch-dir expects name=path, got {values!r}")
        name, _, path = values.partition("=")
        name, path = name.strip(), path.strip()
        if not name or not path:
            parser.error(f"--arch-dir expects name=path, got {values!r}")
        arms = getattr(namespace, "arch_dir", None) or []
        if any(n == name for n, _ in arms):
            parser.error(f"--arch-dir: duplicate arm name {name!r}")
        arms.append((name, Path(path)))
        namespace.arch_dir = arms


def main() -> None:
    ap = argparse.ArgumentParser(description="Patch-count curve / arch-compare aggregation")
    ap.add_argument("--arch-compare", action="store_true",
                    help="Run the architecture comparison instead of the patch curve")
    # patch-curve args
    ap.add_argument("--results-dir", type=Path, default=Path("results_patchcurve"),
                    help="[patch curve] tree of <config>_n<level>/seed<k> cells")
    ap.add_argument("--output-dir", type=Path, default=None,
                    help="Output dir (default: <results-dir>/analysis, or the last arm's)")
    # arch-compare args
    ap.add_argument("--config", type=str, help="[arch-compare] config name")
    ap.add_argument("--arch-dir", action=_ArchDirAction, metavar="NAME=PATH", default=None,
                    help="[arch-compare] one arm, repeatable: --arch-dir mbfusion=Models/results_arch_fusion_v3")
    ap.add_argument("--mode", type=str, choices=["multiclass", "binary"], default=None,
                    help="[arch-compare] appended to each arm's root if not already there")
    ap.add_argument("--baseline", type=str, default=None,
                    help="[arch-compare] arm the deltas are measured against (default: the first)")
    ap.add_argument("--confusion-pair", nargs=2, metavar=("A", "B"), default=["FSW", "UPL"],
                    help="[arch-compare] class pair for the directional confusion contrast "
                         "(default: FSW UPL -- the failure the fusion encoder targets; "
                         "skipped when either class is absent, e.g. in binary mode)")
    ap.add_argument("--no-confusion", action="store_true",
                    help="[arch-compare] skip the confusion-pair contrast")
    # deprecated two-arm aliases, kept so the v2 arch-compare reproduces verbatim
    ap.add_argument("--unet-dir", type=Path, default=None,
                    help="[deprecated] same as --arch-dir unet=<path>")
    ap.add_argument("--unet3plus-dir", type=Path, default=None,
                    help="[deprecated] same as --arch-dir unet3plus=<path>")
    args = ap.parse_args()

    project_root = Path(__file__).parent.parent.parent

    def resolve(p: Path) -> Path:
        return p if p.is_absolute() else project_root / p

    if args.arch_compare:
        if not args.config:
            ap.error("--arch-compare requires --config")
        arms = list(args.arch_dir or [])
        legacy = [(n, p) for n, p in (("unet", args.unet_dir),
                                      ("unet3plus", args.unet3plus_dir)) if p is not None]
        if legacy:
            if arms:
                ap.error("--unet-dir/--unet3plus-dir are deprecated; "
                         "do not mix them with --arch-dir")
            print("[deprecated] --unet-dir/--unet3plus-dir: use "
                  "--arch-dir unet=<path> --arch-dir unet3plus=<path>")
            arms = legacy
        if not arms:
            ap.error("--arch-compare requires at least one --arch-dir NAME=PATH")
        arms = [(name, resolve(path)) for name, path in arms]
        # Default output next to the LAST arm -- the one under test, matching the
        # v2 behaviour of writing into the UNet3+ tree's analysis/.
        out_dir = (resolve(args.output_dir) if args.output_dir
                   else _apply_mode(arms[-1][1], args.mode) / "analysis")
        pair = None if args.no_confusion else tuple(args.confusion_pair)
        write_arch_compare(args.config, arms, out_dir,
                           baseline=args.baseline, mode=args.mode,
                           confusion_pair=pair)
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
