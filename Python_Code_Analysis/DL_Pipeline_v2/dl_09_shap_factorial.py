"""
dl_09_shap_factorial.py

Phase 3 (SHAP half) of the wetland factorial experiment. Runs the per-band
GradientSHAP analysis from dl_07_shap_analysis across the trained factorial
cells, pairing feature ABLATION (marginal contribution, from dl_08's contrasts)
against feature RELIANCE (importance, here). This BACKPROPS through each model,
so it must run on the GPU node, inside the same docker1 container as training,
BEFORE the reservation is torn down (plan Section 8.5).

It is a thin orchestration layer -- the SHAP math, one-hot -> band aggregation,
and checkpoint/architecture auto-detection all live in dl_07_shap_analysis.run_shap.
For each cell this script only resolves:
  - the cell's best checkpoint     results/<config>/seed<k>/best_*.safetensors
  - the config's per-config stats   Data/Training_Data/stats/...<config>...wp0.5.json
      (-> the correct active-band subset; WetlandPatchDataset honours it)
  - the matching seed               so the SHAP background/test split == the
                                    cell's training split (plan Section 3)
and writes results/<config>/seed<k>/shap/ next to that cell's metrics.

Default scope: the FIELD-trained models only (label == fld), per the plan
("SHAP on the field-trained models"). Override with --configs to include the
nwi/flddeg cells.

Idempotent like the runner: a cell whose shap/<stem>_shap_importance.json already
exists is skipped, so SHAP can be resumed across reservation windows.

Usage (inside the container, on the GPU node):
  python dl_09_shap_factorial.py                       # all field cells x all seeds present
  python dl_09_shap_factorial.py --configs fld_chmret_leafoff --seeds 0
  python dl_09_shap_factorial.py --n-background 50 --n-test 20 --crop-size 128
"""

import argparse
import traceback
from pathlib import Path
from typing import List

from dl_07_shap_analysis import run_shap
from dl_experiment_config import CONFIGS, get_config, stats_basename

# Factorial architecture (HPC settings, plan Section 1). These are only fallbacks
# -- run_shap/load_model auto-detect arch from the checkpoint's .meta.json/hparams
# -- but set them correctly so even a metadata-less checkpoint loads right.
BASE_FILTERS = 64
DEPTH = 5


def find_checkpoint(cell: Path) -> Path:
    """Best checkpoint in a cell, preferring the self-describing safetensors.

    Mirrors run_config.sh step 2: best_*.safetensors, else best_*.ckpt.
    """
    for pattern in ("best_*.safetensors", "best_*.ckpt"):
        hits = sorted(cell.glob(pattern), key=lambda p: p.stat().st_mtime, reverse=True)
        if hits:
            return hits[0]
    raise FileNotFoundError(f"no best_* checkpoint in {cell}")


def cells_to_run(results_dir: Path, configs: List[str], seeds) -> List[tuple]:
    """(config, seed, cell_dir) for every requested cell that has a checkpoint."""
    out = []
    for cfg_name in configs:
        cfg_dir = results_dir / cfg_name
        if not cfg_dir.is_dir():
            continue
        if seeds is None:
            seed_dirs = sorted(cfg_dir.glob("seed*"))
        else:
            seed_dirs = [cfg_dir / f"seed{s}" for s in seeds]
        for sd in seed_dirs:
            if not sd.is_dir():
                continue
            seed = int(sd.name[4:]) if sd.name[4:].isdigit() else None
            if seed is None:
                continue
            out.append((cfg_name, seed, sd))
    return out


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    project_root = Path(__file__).parent.parent.parent
    ap.add_argument("--results-dir", type=Path, default=project_root / "results",
                    help="Root holding <config>/seed<k>/ cells (default: <repo>/results)")
    ap.add_argument("--patches-dir", type=Path,
                    default=project_root / "Data/Training_Data/R_Patches_Merged",
                    help="Patch dir used for the SHAP background/test split")
    ap.add_argument("--stats-dir", type=Path,
                    default=project_root / "Data/Training_Data/stats",
                    help="Dir of per-config stats files (band subset source)")
    ap.add_argument("--configs", nargs="+", default=None,
                    help="Configs to run (default: all FIELD configs, label==fld)")
    ap.add_argument("--seeds", type=int, nargs="+", default=None,
                    help="Seeds to run (default: every seed dir present in each config)")
    ap.add_argument("--n-background", type=int, default=50)
    ap.add_argument("--n-test", type=int, default=20)
    ap.add_argument("--crop-size", type=int, default=128,
                    help="Center-crop size to bound memory (0 to disable)")
    ap.add_argument("--force", action="store_true",
                    help="Recompute even if a cell's shap importance JSON exists")
    args = ap.parse_args()

    def _abs(p: Path) -> Path:
        return p if p.is_absolute() else project_root / p

    results_dir = _abs(args.results_dir)
    patches_dir = _abs(args.patches_dir)
    stats_dir = _abs(args.stats_dir)

    # Default scope = field-trained models (where ablation<->reliance pairs).
    if args.configs:
        configs = list(args.configs)
    else:
        configs = [name for name, cfg in CONFIGS.items() if cfg["label"] == "fld"]

    cells = cells_to_run(results_dir, configs, args.seeds)
    if not cells:
        print(f"No matching cells under {results_dir} for configs={configs} "
              f"seeds={args.seeds}. Has training produced checkpoints yet?")
        return

    print(f"SHAP over {len(cells)} cell(s): "
          f"{sorted({c for c, _, _ in cells})} | bf{BASE_FILTERS}/d{DEPTH}")

    done, skipped, failed = 0, 0, []
    for cfg_name, seed, cell in cells:
        cfg = get_config(cfg_name)
        out_dir = cell / "shap"
        stats_path = stats_dir / stats_basename(cfg_name)

        # Idempotency: any *_shap_importance.json in the cell's shap/ -> done.
        if not args.force and out_dir.exists() and any(out_dir.glob("*_shap_importance.json")):
            print(f"[skip] {cfg_name}/seed{seed}: shap importance JSON present.")
            skipped += 1
            continue

        if not stats_path.exists():
            print(f"[error] {cfg_name}/seed{seed}: missing stats {stats_path} "
                  f"(run dl_make_config_stats.py --all). Skipping.")
            failed.append(f"{cfg_name}/seed{seed} (missing stats)")
            continue

        try:
            ckpt = find_checkpoint(cell)
        except FileNotFoundError as e:
            print(f"[error] {e}. Skipping.")
            failed.append(f"{cfg_name}/seed{seed} (no checkpoint)")
            continue

        print(f"\n=== SHAP {cfg_name}/seed{seed} "
              f"(label={cfg['label']}, {cfg['channels']}ch) ===")
        print(f"  model: {ckpt.name}")
        print(f"  stats: {stats_path.name}")
        try:
            run_shap(
                model_path=ckpt,
                patches_dir=patches_dir,
                stats_path=stats_path,
                output_dir=out_dir,
                seed=seed,                       # == training split (plan Section 3)
                n_background=args.n_background,
                n_test=args.n_test,
                crop_size=args.crop_size if args.crop_size > 0 else None,
                base_filters=BASE_FILTERS,
                depth=DEPTH,
                use_aspp=False,
                aspp_rates=[6, 12, 18],
            )
            done += 1
        except Exception:
            print(f"[error] SHAP failed for {cfg_name}/seed{seed}:")
            traceback.print_exc()
            failed.append(f"{cfg_name}/seed{seed} (runtime error)")

    print(f"\nSHAP summary: {done} computed, {skipped} skipped, {len(failed)} failed.")
    if failed:
        for f in failed:
            print(f"  FAILED: {f}")


if __name__ == "__main__":
    main()
