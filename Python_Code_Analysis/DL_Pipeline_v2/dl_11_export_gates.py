"""
dl_11_export_gates.py

Export per-scale branch-gate rasters from a trained `--arch mbfusion` cell.

Gate maps are a DELIVERABLE of the fusion study, not a debug artifact
(factorial_experiment/PLAN.md Section 6.7): the scale-resolved, per-pixel answer to
"how much does the model weight each input modality here?", and they pair with
SHAP in the writeup.

Standalone by design. It loads the checkpoint and runs its own forward pass
rather than hooking dl_05_evaluate, because (a) PLAN Section 6.1 puts dl_05 out of
scope, and (b) this way gates can be re-exported from any archived cell without
re-running evaluation.

Output: <cell>/gates/<patch_stem>.npz, each holding
    level0..level<depth>   float16 (n_branch, H, W)  -- softmax gates, sum to 1
                                                        over axis 0 at every pixel
    branches               the branch names, in gate-channel order
plus <cell>/gates/gate_summary.json with per-branch/per-scale means.

INTERPRETATION -- the one caveat (PLAN Section 6). After gating, `proj` is a 1x1
conv, so the decoder sees sum_i W_i(f_i * g_i). The gate is a spatial scalar;
W_i is a learned linear map with its own magnitude. Therefore:
  * VALID      within-branch spatial comparison -- "terrain reliance rises in
               depressions relative to sideslopes". The gate is the only thing
               varying across space, so spatial patterns are faithful.
  * CONFOUNDED cross-branch absolute comparison -- "terrain matters more than
               LiDAR overall" -- since a branch with modest gates but large W_i
               can still dominate. GroupNorm equalizes features, not projection
               weights.
So: plot gate maps STANDARDIZED WITHIN BRANCH, and take overall branch importance
from SHAP (dl_09_shap_factorial.py). gate_summary.json reports raw means for
provenance; it is not a branch-importance ranking.

Usage (CPU or GPU; needs the patches + the cell's checkpoint):
    python dl_11_export_gates.py --cell Models/results_arch_fusion_v3/multiclass/\\
        fld_chmret_leafoff_mbfusion/seed0 --config fld_chmret_leafoff --seed 0
"""

import argparse
import json
from pathlib import Path

import numpy as np
import torch

from dl_02_dataset import WetlandPatchDataset
from dl_band_utils import load_band_config
from dl_model_utils import load_model
import dl_experiment_config as X
import dl_patch_pools as P


def _find_checkpoint(cell: Path) -> Path:
    """Newest .safetensors in the cell, else .ckpt (matches run_config.sh's order)."""
    for pattern in ("best_*.safetensors", "best_*.ckpt"):
        hits = sorted(cell.glob(pattern), key=lambda p: p.stat().st_mtime, reverse=True)
        if hits:
            return hits[0]
    raise FileNotFoundError(f"no best_*.safetensors or best_*.ckpt in {cell}")


def export_gates(cell: Path, config: str, seed: int, mode: str,
                 stats_dir: Path, data_root: Path, n_patches: int,
                 leakage_guard: str, device: torch.device) -> int:
    cfg = X.get_config(config)
    eval_stats = stats_dir / X.stats_basename(X.eval_config_name(config), mode=mode)
    if not eval_stats.exists():
        raise FileNotFoundError(f"missing eval stats: {eval_stats}")

    ckpt = _find_checkpoint(cell)
    stats = json.loads(eval_stats.read_text())
    net = load_model(ckpt, device, in_channels=stats["in_channels"],
                     num_classes=len(stats["class_names"]))
    if not hasattr(net, "gate_maps"):
        raise SystemExit(
            f"{ckpt.name} is not an mbfusion model (no gate_maps); "
            f"arch={getattr(net, '__class__').__name__}. Nothing to export."
        )
    net.eval()
    branches = list(net.branch_indices)

    # The SAME held-out field patches the cell was scored on. Deterministic:
    # resolve_pools is seeded, and we take a sorted prefix so re-running the
    # export reproduces the identical patch set (and thus comparable rasters
    # across seeds and across architectures).
    _train, _val, test_pool = P.resolve_pools(
        config=config, seed=seed, data_root=data_root, leakage_guard=leakage_guard)
    test_files = sorted(test_pool)[:n_patches]
    if not test_files:
        raise SystemExit(f"no test patches resolved for {config} seed{seed}")

    ds = WetlandPatchDataset(test_files, eval_stats, augment=False, validate_bands=True)
    out_dir = cell / "gates"
    out_dir.mkdir(parents=True, exist_ok=True)

    print(f"cell     : {cell}")
    print(f"checkpoint: {ckpt.name}")
    print(f"branches : {branches}")
    print(f"patches  : {len(ds)} (of {len(test_pool)} held-out field patches)")

    # Running mean of each branch's gate, per scale, over all exported pixels.
    sums: dict = {}
    counts: dict = {}
    written = 0
    for i in range(len(ds)):
        x, _ = ds[i]
        stem = Path(ds.patch_files[i]).stem
        with torch.no_grad():
            net(x.unsqueeze(0).to(device))
        arrays = {}
        for level, g in enumerate(net.gate_maps()):
            a = g[0].float().cpu().numpy()           # (n_branch, H, W)
            arrays[f"level{level}"] = a.astype(np.float16)
            sums.setdefault(level, np.zeros(len(branches)))
            sums[level] += a.reshape(len(branches), -1).sum(axis=1)
            counts[level] = counts.get(level, 0) + a[0].size
        np.savez_compressed(out_dir / f"{stem}.npz", branches=np.array(branches), **arrays)
        written += 1

    summary = {
        "config": config, "mode": mode, "seed": seed,
        "checkpoint": ckpt.name,
        "branches": branches,
        "n_patches": written,
        "patches": [Path(f).name for f in test_files],
        "mean_gate": {
            f"level{lv}": {b: round(float(sums[lv][j] / counts[lv]), 6)
                           for j, b in enumerate(branches)}
            for lv in sorted(sums)
        },
        "note": ("Means are provenance, NOT a branch-importance ranking: proj is a "
                 "1x1 conv, so a branch with modest gates but large weights can still "
                 "dominate. Compare gates WITHIN a branch across space; take overall "
                 "branch importance from SHAP."),
    }
    (out_dir / "gate_summary.json").write_text(json.dumps(summary, indent=2))

    print(f"\nwrote {written} .npz + gate_summary.json -> {out_dir}")
    print("\nmean gate per branch (provenance only -- see the note in the JSON):")
    hdr = "  level  " + "".join(f"{b:>10s}" for b in branches)
    print(hdr)
    for lv in sorted(sums):
        row = "".join(f"{sums[lv][j]/counts[lv]:10.4f}" for j in range(len(branches)))
        print(f"  {lv:<7d}{row}")
    return 0


def main() -> int:
    root = Path(__file__).resolve().parents[2]
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--cell", type=Path, required=True,
                    help="Cell dir holding the checkpoint (…/<config>_mbfusion/seed<k>)")
    ap.add_argument("--config", type=str, required=True, help="Factorial config name")
    ap.add_argument("--seed", type=int, required=True,
                    help="Seed the cell was trained with (selects the same test split)")
    ap.add_argument("--mode", default="multiclass", choices=["multiclass", "binary"])
    ap.add_argument("--stats-dir", type=Path, default=root / "Data/Training_Data/stats")
    ap.add_argument("--data-root", type=Path, default=root / "Data/Training_Data")
    ap.add_argument("--n-patches", type=int, default=8,
                    help="Held-out patches to export (default 8; float16 keeps this "
                         "a few MB so it rides along with rsync --metrics-only)")
    ap.add_argument("--leakage-guard", default=None, choices=["huc12", "coord"])
    ap.add_argument("--device", default=None, help="cuda | cpu (default: auto)")
    a = ap.parse_args()

    device = torch.device(a.device) if a.device else torch.device(
        "cuda" if torch.cuda.is_available() else "cpu")
    return export_gates(a.cell.resolve(), a.config, a.seed, a.mode,
                        a.stats_dir.resolve(), a.data_root.resolve(), a.n_patches,
                        a.leakage_guard or X.LEAKAGE_GUARD, device)


if __name__ == "__main__":
    raise SystemExit(main())
