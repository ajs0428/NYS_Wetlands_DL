"""
Driver for the class-weight power sweep.

For each p in the grid this runs, sequentially:
  1. (once, up front) sweep_weight_power.py -> per-p normalization_stats_wp{p}.json
  2. dl_04_train_lightning.py  (5/11 baseline config, --output-dir Models/sweep_wp{p})
  3. dl_05_evaluate.py on the resulting checkpoint -> eval_wp{p}.json
  4. parse metrics into a single comparison table (Models/weight_power_sweep_results.{md,csv})

p=1.0 is the control (legacy pure-inverse-frequency weighting). The table
surfaces the metrics tied to the documented failure mode: SSW precision/recall,
the UPL->FSW and UPL->SSW confusion cells, mIoU and macro-F1.

Designed for HPC (CUDA). Run from anywhere; paths resolve to project root.

    python run_weight_power_sweep.py                 # full sweep
    python run_weight_power_sweep.py --dry-run        # print commands only
    python run_weight_power_sweep.py --skip-train     # eval existing ckpts only
    python run_weight_power_sweep.py --powers 1.0 0.5
"""
import argparse
import json
import subprocess
import sys
from pathlib import Path

from dl_band_utils import default_stats_path

PROJECT_ROOT = Path(__file__).parent.parent.parent
SCRIPT_DIR = Path(__file__).parent

# 5/11 baseline config (Run-4 hyperparameters at trimmed 22-channel band set).
BASELINE = dict(
    base_filters=64, depth=4, dropout=0.2, lr="1e-4", weight_decay="1e-4",
    ce_weight="1.0", dice_weight="1.0", focal_gamma="2.0", label_smoothing="0.0",
    batch_size=32, seed=420, early_stopping=25, lr_patience=15,
)


def run(cmd, dry_run):
    printable = " ".join(f'"{c}"' if " " in str(c) else str(c) for c in cmd)
    print(f"\n$ {printable}\n")
    if dry_run:
        return
    subprocess.run([str(c) for c in cmd], check=True)


def newest_checkpoint(arm_dir: Path):
    """Prefer self-describing .safetensors, fall back to .ckpt."""
    for ext in ("*.safetensors", "*.ckpt"):
        cands = sorted(arm_dir.glob(ext), key=lambda p: p.stat().st_mtime, reverse=True)
        if cands:
            return cands[0]
    return None


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--powers", type=float, nargs="+", default=[1.0, 0.7, 0.5, 0.3])
    ap.add_argument("--epochs", type=int, default=100)
    ap.add_argument("--batch-size", type=int, default=BASELINE["batch_size"])
    ap.add_argument("--base-stats", type=Path, default=None,
                    help="Base stats file passed to dl_05 for the test split "
                         "(default: the active mode's <mode>_normalization_stats.json). "
                         "Per-arm files are derived as <stem>_wp<p>.json.")
    ap.add_argument("--skip-train", action="store_true",
                    help="Evaluate existing checkpoints only (no training)")
    ap.add_argument("--dry-run", action="store_true",
                    help="Print all commands without executing")
    args = ap.parse_args()

    py = sys.executable
    # Mode-prefixed base (e.g. binary_normalization_stats.json); per-arm files
    # become <mode>_normalization_stats_wp<p>.json via with_name() below.
    base_stats = args.base_stats or (PROJECT_ROOT / default_stats_path())

    # Step 1: generate all per-p stats files in one shot (no patch rescan).
    # Pass the resolved base so the generated filenames (<base-stem>_wp<p>.json)
    # match the per-arm names derived below -- no reliance on the generator's own
    # default/legacy fallback.
    run([py, SCRIPT_DIR / "sweep_weight_power.py",
         "--base-stats", base_stats,
         "--powers", *[f"{p:g}" for p in args.powers]], args.dry_run)

    results = []
    for p in args.powers:
        tag = f"wp{p:g}"
        arm_dir = PROJECT_ROOT / "Models" / f"sweep_{tag}"
        stats_p = base_stats.with_name(f"{base_stats.stem}_{tag}.json")
        eval_out = arm_dir / f"eval_{tag}.json"
        arm_dir.mkdir(parents=True, exist_ok=True)

        if not args.skip_train:
            run([py, SCRIPT_DIR / "dl_04_train_lightning.py",
                 "--stats-path", stats_p, "--output-dir", arm_dir,
                 "--base-filters", BASELINE["base_filters"],
                 "--depth", BASELINE["depth"], "--dropout", BASELINE["dropout"],
                 "--lr", BASELINE["lr"], "--weight-decay", BASELINE["weight_decay"],
                 "--ce-weight", BASELINE["ce_weight"],
                 "--dice-weight", BASELINE["dice_weight"],
                 "--focal-gamma", BASELINE["focal_gamma"],
                 "--label-smoothing", BASELINE["label_smoothing"],
                 "--batch-size", args.batch_size, "--seed", BASELINE["seed"],
                 "--early-stopping", BASELINE["early_stopping"],
                 "--lr-patience", BASELINE["lr_patience"],
                 "--epochs", args.epochs], args.dry_run)

        ckpt = newest_checkpoint(arm_dir)
        if ckpt is None and not args.dry_run:
            print(f"  !! no checkpoint found in {arm_dir}; skipping {tag}")
            continue

        run([py, SCRIPT_DIR / "dl_05_evaluate.py",
             "--model", ckpt or arm_dir / "<ckpt>",
             "--stats-path", base_stats, "--seed", BASELINE["seed"],
             "--base-filters", BASELINE["base_filters"],
             "--depth", BASELINE["depth"], "--output", eval_out], args.dry_run)

        if args.dry_run or not eval_out.exists():
            continue
        m = json.loads(eval_out.read_text())
        cm = m["confusion_matrix"]  # rows=true [EMW,FSW,SSW,UPL]
        results.append({
            "p": p,
            "mIoU": m["mean_iou"],
            "macroF1": m["macro_f1"],
            "OA": m["overall_accuracy"],
            "SSW_P": m["per_class"]["SSW"]["precision"],
            "SSW_R": m["per_class"]["SSW"]["recall"],
            "SSW_IoU": m["per_class"]["SSW"]["iou"],
            "UPL_to_FSW": cm[3][1],
            "UPL_to_SSW": cm[3][2],
        })

    if not results:
        print("\n(no metrics collected — dry-run or training not yet complete)")
        return

    cols = ["p", "mIoU", "macroF1", "OA", "SSW_P", "SSW_R", "SSW_IoU",
            "UPL_to_FSW", "UPL_to_SSW"]
    md = ["| " + " | ".join(cols) + " |",
          "|" + "|".join(["---"] * len(cols)) + "|"]
    for r in results:
        md.append("| " + " | ".join(
            f"{r[c]:.4f}" if isinstance(r[c], float) else str(r[c]) for c in cols) + " |")
    table = "\n".join(md)
    print("\n" + table + "\n")
    print("Control = p=1.0 (legacy inverse-freq). Target: SSW_P up toward >=0.40, "
          "mIoU not below the p=1.0 value, UPL_to_FSW shrinking.")

    out_md = PROJECT_ROOT / "Models" / "weight_power_sweep_results.md"
    out_md.write_text("# Class-weight power sweep\n\n" + table + "\n")
    out_csv = out_md.with_suffix(".csv")
    out_csv.write_text(",".join(cols) + "\n" + "\n".join(
        ",".join(str(r[c]) for c in cols) for r in results) + "\n")
    print(f"Saved: {out_md}\n       {out_csv}")


if __name__ == "__main__":
    main()
