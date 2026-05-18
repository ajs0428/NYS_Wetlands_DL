"""
Class-weight power sweep helper.

Class weights are a pure function of class_frequencies (stored in
normalization_stats.json) and the exponent p:

    weight_c = (1 / freq_c) ** p,  then renormalized so min non-zero weight = 1.0

p=1.0 reproduces the legacy pure-inverse-frequency weights exactly.
Lower p reduces minority over-prediction (the UPL->FSW/SSW leakage documented
in training_baselines.md). This script does NOT rescan patches — it derives a
per-p stats file from an existing base stats file and prints the matching
training commands.

Usage:
    python sweep_weight_power.py                       # default p grid
    python sweep_weight_power.py --powers 1.0 0.5 0.3
    python sweep_weight_power.py --emit-commands       # also print train cmds
"""
import argparse
import copy
import json
from pathlib import Path

from dl_band_utils import default_stats_path


def derive_weights(class_frequencies: dict, class_names: list, power: float) -> dict:
    """(1/freq)**power, renormalized so the minimum non-zero weight is 1.0."""
    raw = {}
    for name in class_names:
        freq = class_frequencies.get(name, 0)
        raw[name] = (1.0 / freq) ** power if freq > 0 else 0.0
    non_zero = [w for w in raw.values() if w > 0]
    if non_zero:
        m = min(non_zero)
        raw = {k: (v / m if v > 0 else 0.0) for k, v in raw.items()}
    return {k: round(v, 4) for k, v in raw.items()}


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--base-stats", type=Path, default=None,
                        help="Base normalization_stats.json (default: pipeline default)")
    parser.add_argument("--powers", type=float, nargs="+",
                        default=[1.0, 0.7, 0.5, 0.3],
                        help="Weight-power grid (default: 1.0 0.7 0.5 0.3)")
    parser.add_argument("--emit-commands", action="store_true",
                        help="Also print ready-to-run dl_04 training commands")
    args = parser.parse_args()

    if args.base_stats:
        base_path = Path(args.base_stats)
    else:
        project_root = Path(__file__).parent.parent.parent
        base_path = project_root / default_stats_path()
        if not base_path.exists():
            # Fall back to the legacy non-mode-specific filename
            legacy = project_root / "Data/Training_Data/normalization_stats.json"
            if legacy.exists():
                base_path = legacy
    with open(base_path) as f:
        base = json.load(f)

    class_names = base["class_names"]
    freqs = base["class_frequencies"]

    print(f"Base stats: {base_path}  ({base.get('num_patches', '?')} patches)")
    print(f"Frequencies: { {k: round(v, 4) for k, v in freqs.items()} }\n")
    print(f"{'p':>5} | " + " | ".join(f"{n:>8}" for n in class_names) + " | stats file")
    print("-" * (9 + 11 * len(class_names) + 40))

    out_files = {}
    for p in args.powers:
        weights = derive_weights(freqs, class_names, p)
        stats = copy.deepcopy(base)
        stats["class_weights"] = weights
        stats["weight_power"] = p
        out_path = base_path.with_name(f"{base_path.stem}_wp{p:g}.json")
        with open(out_path, "w") as f:
            json.dump(stats, f, indent=2)
        out_files[p] = out_path
        row = " | ".join(f"{weights[n]:>8.3f}" for n in class_names)
        print(f"{p:>5} | {row} | {out_path.name}")

    if args.emit_commands:
        print("\n# Training commands (5/11 baseline config; adjust paths/flags for HPC):")
        for p in args.powers:
            tag = f"wp{p:g}"
            print(
                f"python dl_04_train_lightning.py "
                f'--stats-path "{out_files[p]}" '
                f"--base-filters 64 --depth 4 --dropout 0.2 "
                f"--lr 1e-4 --weight-decay 1e-4 --ce-weight 1.0 --dice-weight 1.0 "
                f"--focal-gamma 2.0 --label-smoothing 0.0 --batch-size 32 "
                f"--seed 420 --early-stopping 25 --lr-patience 15 --epochs 100 "
                f"--output-dir Models/sweep_{tag}"
            )

    print("\nNext: train each arm, then compare UPL->FSW / UPL->SSW confusion "
          "cells and SSW precision against the p=1.0 control.")


if __name__ == "__main__":
    main()
