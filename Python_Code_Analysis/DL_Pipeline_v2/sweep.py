"""
sweep.py

Hyperparameter sweep for loss function tuning.
Runs multiple training configurations sequentially and compares
per-class IoU to find the best setup for minority wetland classes.

Designed for HPC — run inside the Docker container:
    docker1 run --gpus all \
        -v /workdir/<labid>/Data:/app/Data \
        -v /workdir/<labid>/Models:/app/Models \
        python Python_Code_Analysis/DL_Pipeline_v2/sweep.py

Or override specific settings:
    python sweep.py --epochs 30 --base-filters 64
"""

import json
import argparse
from pathlib import Path
from datetime import datetime

import importlib.util
import sys

def _import_module(name, path):
    if name in sys.modules:
        return sys.modules[name]
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module

_script_dir = Path(__file__).parent
_train = _import_module("train_lightning", _script_dir / "04_train_lightning.py")

train = _train.train


# ── Experiment configurations ────────────────────────────────────────
# Each dict overrides the default training args.
# Only specify keys that differ from the defaults below.

EXPERIMENTS = [
    {
        "name": "A_baseline",
        "ce_weight": 0.5,
        "dice_weight": 1.5,
        "focal_gamma": 2.0,
    },
    {
        "name": "B_more_dice",
        "ce_weight": 0.3,
        "dice_weight": 2.0,
        "focal_gamma": 2.0,
    },
    {
        "name": "C_harder_focal",
        "ce_weight": 0.5,
        "dice_weight": 1.5,
        "focal_gamma": 3.0,
    },
    {
        "name": "D_combined",
        "ce_weight": 0.3,
        "dice_weight": 2.0,
        "focal_gamma": 3.0,
    },
]


# ── Sweep runner ─────────────────────────────────────────────────────

def run_sweep(
    patches_dir: Path,
    stats_path: Path,
    output_dir: Path,
    experiments: list,
    epochs: int = 50,
    batch_size: int = 16,
    learning_rate: float = 1e-4,
    base_filters: int = 64,
    depth: int = 4,
    num_workers: int = 4,
    early_stopping_patience: int = 15,
    architecture: str = "resunet34",
    precision: str = "32-true",
    label_smoothing: float = 0.0,
):
    """Run all experiments and collect results."""

    results = []

    for i, exp_config in enumerate(experiments):
        exp = {k: v for k, v in exp_config.items() if k != "name"}
        name = exp_config.get("name", f"exp_{i}")
        print(f"\n{'='*70}")
        print(f"  EXPERIMENT {i+1}/{len(experiments)}: {name}")
        print(f"  {exp}")
        print(f"{'='*70}\n")

        exp_output = output_dir / f"sweep_{name}"

        try:
            history = train(
                patches_dir=patches_dir,
                stats_path=stats_path,
                output_dir=exp_output,
                epochs=epochs,
                batch_size=batch_size,
                learning_rate=learning_rate,
                base_filters=base_filters,
                depth=depth,
                num_workers=num_workers,
                seed=None,  # random seed each run
                early_stopping_patience=early_stopping_patience,
                architecture=architecture,
                precision=precision,
                label_smoothing=label_smoothing,
                **exp,
            )

            results.append({
                "name": name,
                "params": exp,
                "best_val_loss": min(history.get("val_loss", [float("inf")])),
                "best_val_iou": max(history.get("val_iou", [0.0])),
                "best_val_acc": max(history.get("val_accuracy", [0.0])),
                "epochs_run": len(history.get("val_loss", [])),
                "output_dir": str(exp_output),
            })

        except Exception as e:
            print(f"\n  EXPERIMENT {name} FAILED: {e}\n")
            results.append({
                "name": name,
                "params": exp,
                "error": str(e),
            })

    # ── Summary ──────────────────────────────────────────────────────
    print(f"\n{'='*70}")
    print("  SWEEP RESULTS")
    print(f"{'='*70}")
    print(f"\n{'Name':<20} {'CE_w':>6} {'Dice_w':>7} {'Gamma':>6} "
          f"{'Val Loss':>9} {'Val IoU':>8} {'Val Acc':>8} {'Epochs':>7}")
    print("-" * 70)

    for r in results:
        if "error" in r:
            print(f"{r['name']:<20} FAILED: {r['error']}")
            continue
        p = r["params"]
        print(f"{r['name']:<20} {p.get('ce_weight','—'):>6} {p.get('dice_weight','—'):>7} "
              f"{p.get('focal_gamma','—'):>6} "
              f"{r['best_val_loss']:>9.4f} {r['best_val_iou']:>8.4f} "
              f"{r['best_val_acc']:>8.4f} {r['epochs_run']:>7}")

    # Save results JSON
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    results_path = output_dir / f"sweep_results_{timestamp}.json"
    with open(results_path, "w") as f:
        json.dump(results, f, indent=2)
    print(f"\nResults saved to {results_path}")

    # Identify best run
    valid = [r for r in results if "error" not in r]
    if valid:
        best = max(valid, key=lambda r: r["best_val_iou"])
        print(f"\nBest run by Val IoU: {best['name']} "
              f"(IoU={best['best_val_iou']:.4f})")
        print(f"  Checkpoint: {best['output_dir']}")

    return results


# ── CLI ──────────────────────────────────────────────────────────────

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Loss hyperparameter sweep for wetland segmentation"
    )
    parser.add_argument("--patches-dir", type=Path,
                        default=Path("Data/Training_Data/R_Patches"))
    parser.add_argument("--stats-path", type=Path,
                        default=Path("Data/Training_Data/normalization_stats.json"))
    parser.add_argument("--output-dir", type=Path, default=Path("Models"))
    parser.add_argument("--epochs", type=int, default=50)
    parser.add_argument("--batch-size", type=int, default=16)
    parser.add_argument("--lr", type=float, default=1e-4)
    parser.add_argument("--base-filters", type=int, default=64)
    parser.add_argument("--depth", type=int, default=4)
    parser.add_argument("--workers", type=int, default=4)
    parser.add_argument("--early-stopping", type=int, default=15)
    parser.add_argument("--architecture", type=str, default="resunet34",
                        choices=["unet", "resunet34"])
    parser.add_argument("--precision", type=str, default="32-true",
                        choices=["32-true", "16-mixed", "bf16-mixed"])
    args = parser.parse_args()

    # Handle relative paths
    project_root = Path(__file__).parent.parent.parent
    patches_dir = project_root / args.patches_dir if not args.patches_dir.is_absolute() else args.patches_dir
    stats_path = project_root / args.stats_path if not args.stats_path.is_absolute() else args.stats_path
    output_dir = project_root / args.output_dir if not args.output_dir.is_absolute() else args.output_dir

    run_sweep(
        patches_dir=patches_dir,
        stats_path=stats_path,
        output_dir=output_dir,
        experiments=EXPERIMENTS,
        epochs=args.epochs,
        batch_size=args.batch_size,
        learning_rate=args.lr,
        base_filters=args.base_filters,
        depth=args.depth,
        num_workers=args.workers,
        early_stopping_patience=args.early_stopping,
        architecture=args.architecture,
        precision=args.precision,
    )
