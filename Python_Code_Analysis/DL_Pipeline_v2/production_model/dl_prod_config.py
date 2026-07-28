"""
dl_prod_config.py

Single source of truth for the PRODUCTION wetland model -- the one deployable
model, as opposed to the factorial's 48-cell benchmark grid. Imported by:
  - Shell_Scripts/run_production.sh   (resolves recipe -> shell vars)

The factorial answers "which inputs and labels matter?"; this answers "given
that answer, what do we ship?". So this module deliberately does NOT redefine
the band matrix, the label sources, or the pool rules -- it *selects* a
factorial config and pins the training budget around it. Every band/stats/pool
question still resolves through dl_experiment_config, which keeps the shipped
model provably on the same footing as the benchmark that justified it.

Consequence worth stating: changing RECIPE["config"] here does not invent a new
feature set, it points at an existing, already-validated one. If a genuinely new
band combination is ever wanted for production, add it to dl_experiment_config's
CONFIGS (so the preflight and stats subsetter see it too) and reference it here.
"""

import sys
from pathlib import Path
from typing import Dict

# dl_experiment_config and dl_band_utils live one level up (the pipeline root).
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from dl_experiment_config import (  # noqa: E402
    CONFIGS,
    LEAKAGE_GUARD,
    config_bands,
    config_in_channels,
    config_label_source,
    config_patch_dirs,
    config_pool_rule,
    eval_config_name,
    get_config,
    stats_basename,
)
from dl_band_utils import load_band_config  # noqa: E402


# --- The production recipe ----------------------------------------------------
# config: chosen on the factorial-v2 field-test results
# (Models/factorial_results_v2/analysis/cross_mode_summary.csv). nwifield_chmret_leafoff
# leads every headline wetland metric -- WET IoU 0.659 collapsed-multiclass /
# 0.668 binary, WET recall 0.870 / 0.858 -- beating the pure-field
# fld_chmret_leafoff (0.654 / 0.667, recall 0.848) and every NWI-only arm. It is
# the hybrid pool: field labels on the field footprints UNION the NWIextra
# patches outside the test HUC12s, on the full 26-channel feature set.
#
# The remaining constants are held at the factorial's values ON PURPOSE: the
# benchmark's ranking is only evidence for the shipped model if the shipped model
# trains the same way. EPOCHS is the one deliberate departure (see below).
RECIPE: Dict[str, object] = {
    "config": "nwifield_chmret_leafoff",
    "mode": "multiclass",       # ship the 4-class model; binary is derivable by collapsing
    "arch": "unet",
    "base_filters": 64,
    "depth": 5,
    "batch_size": 16,
    "precision": "16-mixed",
    "leakage_guard": LEAKAGE_GUARD,
    # Longer than the factorial's 50. The grid capped epochs to make 48 cells fit
    # a reservation, not because 50 was optimal; a single model can afford more,
    # and EarlyStopping still governs the actual stop.
    "epochs": 100,
    # Three seeds, same as the factorial -- run all three, then ship the best by
    # field-test macro F1 (or ensemble them; see PLAN.md "Open decisions").
    "seeds": [0, 1, 2],
}

# Results root, relative to the repo root. run_config.sh appends /<mode>/<cell>/seed<k>.
RESULTS_SUBDIR: str = "Models/production_model"

# Cell directory name under RESULTS_SUBDIR/<mode>/. Fixed rather than derived
# from the config name so that re-pointing RECIPE["config"] does not silently
# scatter production runs across differently-named folders.
CELL_NAME: str = "production"


def recipe_config() -> str:
    """The factorial config name backing the production recipe (validated)."""
    name = str(RECIPE["config"])
    get_config(name)  # raises with the valid-name list if the recipe drifts
    return name


def describe() -> str:
    """Human-readable one-block summary of exactly what production will train."""
    name = recipe_config()
    cfg = CONFIGS[name]
    bc = load_band_config()
    mode = str(RECIPE["mode"])
    lines = [
        f"config:       {name}",
        f"mode:         {mode}",
        f"label source: {config_label_source(name)}   pool rule: {config_pool_rule(name)}",
        f"patch dirs:   {' '.join(config_patch_dirs(name))}",
        f"guard:        {RECIPE['leakage_guard']}",
        f"bands ({len(config_bands(cfg))}):    {' '.join(config_bands(cfg))}",
        f"in_channels:  {config_in_channels(cfg, bc)}  (plan expects {cfg['channels']})",
        f"arch:         {RECIPE['arch']}  bf{RECIPE['base_filters']} d{RECIPE['depth']}",
        f"schedule:     {RECIPE['epochs']} epochs, batch {RECIPE['batch_size']}, {RECIPE['precision']}",
        f"seeds:        {' '.join(str(s) for s in RECIPE['seeds'])}",
        f"train stats:  {stats_basename(name, mode=mode)}",
        f"eval  stats:  {stats_basename(eval_config_name(name), mode=mode)}  "
        f"(eval config: {eval_config_name(name)}, field-labeled)",
        f"results:      {RESULTS_SUBDIR}/{mode}/{CELL_NAME}/seed<k>/",
    ]
    return "\n".join(lines)


def verify() -> None:
    """Assert the recipe is internally consistent before any GPU time is spent."""
    name = recipe_config()
    cfg = CONFIGS[name]
    n = config_in_channels(cfg, load_band_config())
    assert n == cfg["channels"], (
        f"production config '{name}': resolved {n} channels, plan expects {cfg['channels']}"
    )
    assert RECIPE["mode"] in ("multiclass", "binary"), f"bad mode: {RECIPE['mode']!r}"
    assert RECIPE["arch"] in ("unet", "unet3plus"), f"bad arch: {RECIPE['arch']!r}"
    assert RECIPE["seeds"], "RECIPE['seeds'] is empty -- nothing would train"


def _emit() -> None:
    """Print shell-sourceable vars for run_production.sh.

    Only the production-layer choices are emitted; run_config.sh still calls
    dl_experiment_config --emit itself to resolve bands/stats/pools, so there is
    exactly one place that knows the band matrix.
    """
    vals = {
        "PROD_CONFIG": recipe_config(),
        "PROD_MODE": RECIPE["mode"],
        "PROD_ARCH": RECIPE["arch"],
        "PROD_BASE_FILTERS": RECIPE["base_filters"],
        "PROD_DEPTH": RECIPE["depth"],
        "PROD_EPOCHS": RECIPE["epochs"],
        "PROD_BATCH_SIZE": RECIPE["batch_size"],
        "PROD_PRECISION": RECIPE["precision"],
        "PROD_LEAKAGE_GUARD": RECIPE["leakage_guard"],
        "PROD_SEEDS": " ".join(str(s) for s in RECIPE["seeds"]),
        "PROD_RESULTS_SUBDIR": RESULTS_SUBDIR,
        "PROD_CELL_NAME": CELL_NAME,
    }
    for k, v in vals.items():
        print(f'{k}="{v}"')


if __name__ == "__main__":
    import argparse
    ap = argparse.ArgumentParser(description="Production model recipe (single source of truth)")
    ap.add_argument("--emit", action="store_true",
                    help="Print shell-sourceable PROD_* vars (used by run_production.sh)")
    ap.add_argument("--config", action="store_true",
                    help="Print just the backing factorial config name")
    args = ap.parse_args()

    if args.emit:
        _emit()
    elif args.config:
        print(recipe_config())
    else:
        # Self-check: describe the recipe and assert it is consistent.
        print(describe())
        verify()
        print("\nOK -- recipe consistent.")
