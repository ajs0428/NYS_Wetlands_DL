"""
dl_patch_pools.py

Factorial v2 split/data resolver -- the single place the field-anchored split and
the directory-aware leakage guards live (plan Decisions 4.5-4.6). Given a config
name and a seed it returns concrete (train, val, test) file lists; the preflight
imports the same function so preflight and training agree by construction.

Two hard rules the whole experiment rests on:
  1. TEST is ALWAYS field: `R_Patches` at the seed's held-out field footprints,
     regardless of config or classification mode. Every config/mode is scored on
     the same gold-standard field pixels.
  2. No test footprint may reach any config's train/val pool. This is asserted
     before the lists are returned (`_assert_no_leakage`), so a resolver bug
     surfaces as an error, never as a silently inflated score.

Leakage regimes (LEAKAGE_GUARD in dl_experiment_config.py; --leakage-guard here):
  * "huc12" (DEFAULT / headline) -- the field split is by HUC12: whole watersheds
    are held out for test, and NO config trains in a test HUC12. Under this regime
    `nwiextra`'s pool is a clean SUPERSET of `nwi`'s, so the 1x-vs-2x quantity
    contrast isolates data volume alone. Guards soft spatial autocorrelation.
  * "coord" (sensitivity run) -- the field split is by patch (v1 style): only the
    exact test footprints are held out. Field<->NWI share a footprint via the
    filename twin (nwi_field_twin), so a NWI twin of a test patch is excluded;
    `NWIextra` is geographically disjoint from field (a design invariant the
    preflight enforces: 0 overlap, nearest >=258 m), so no NWIextra patch can
    collide with a test footprint.

File lists are classification-mode-invariant (binary is a downstream label remap,
not a different patch set), so `resolve_pools` takes no mode argument.

Directory identity helpers (field_key / nwi_field_twin / huc12_of) and the config
-> (patch_dirs, pool_rule) registry all come from dl_experiment_config.py; this
module never re-derives them.
"""

from __future__ import annotations

import argparse
import random
from pathlib import Path
from typing import Dict, List, Tuple

import dl_experiment_config as X

# Repo-relative data root: .../Python_Code_Analysis/DL_Pipeline_v2/ -> repo/Data/Training_Data
_REPO_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_DATA_ROOT = _REPO_ROOT / "Data" / "Training_Data"

TRAIN_RATIO, VAL_RATIO, TEST_RATIO = 0.70, 0.15, 0.15
# val's share of the (train+val) remainder -- used to partition the NWIextra
# "extra" patches under the coord regime, which have no field partition to inherit.
_VAL_OF_TRAINVAL = VAL_RATIO / (TRAIN_RATIO + VAL_RATIO)


# --- filesystem ---------------------------------------------------------------

def _list_dir(data_root: Path, dirname: str) -> List[Path]:
    """Sorted *.tif in one patch directory; error if missing/empty."""
    d = data_root / dirname
    if not d.is_dir():
        raise FileNotFoundError(f"patch directory not found: {d}")
    files = sorted(d.glob("*.tif"))
    if not files:
        raise ValueError(f"no .tif patches in {d}")
    return files


# --- field split (the anchor) -------------------------------------------------

def field_huc_split(
    data_root: Path, seed: int, ratios: Tuple[float, float, float] = (TRAIN_RATIO, VAL_RATIO, TEST_RATIO)
) -> Tuple[List[str], List[str], List[str]]:
    """Partition the field HUC12s into (train, val, test) HUC lists by seed.

    The 'huc12' regime anchor: whole watersheds are assigned to one split, so a
    test HUC12 is never trained on by any config. Deterministic for a given seed
    (sort then seeded shuffle); HUC granularity means seed-to-seed variance is
    higher than a patch split -- intended, it estimates cross-watershed transfer.
    """
    tr, va, te = ratios
    assert abs(tr + va + te - 1.0) < 1e-6
    hucs = sorted({X.huc12_of(f) for f in _list_dir(data_root, X.FIELD_TEST_DIR)})
    random.Random(seed).shuffle(hucs)
    n = len(hucs)
    a, b = int(n * tr), int(n * tr) + int(n * va)
    return hucs[:a], hucs[a:b], hucs[b:]


def field_patch_split(
    data_root: Path, seed: int, ratios: Tuple[float, float, float] = (TRAIN_RATIO, VAL_RATIO, TEST_RATIO)
) -> Tuple[List[str], List[str], List[str]]:
    """Partition the field patch basenames into (train, val, test) by seed.

    The 'coord' regime anchor (v1-style patch-level holdout). Keys are field_key()
    (raw R_Patches basename; all 689 unique).
    """
    tr, va, te = ratios
    assert abs(tr + va + te - 1.0) < 1e-6
    names = sorted(X.field_key(f) for f in _list_dir(data_root, X.FIELD_TEST_DIR))
    random.Random(seed).shuffle(names)
    n = len(names)
    a, b = int(n * tr), int(n * tr) + int(n * va)
    return names[:a], names[a:b], names[b:]


# --- pool resolution ----------------------------------------------------------

def _huc_partition(files: List[Path], train_hucs: set, val_hucs: set) -> Tuple[List[Path], List[Path]]:
    """Split files into (train, val) by their HUC12 membership; test HUCs drop out."""
    tr = [f for f in files if X.huc12_of(f) in train_hucs]
    va = [f for f in files if X.huc12_of(f) in val_hucs]
    return tr, va


def _seeded_trainval(files: List[Path], seed: int) -> Tuple[List[Path], List[Path]]:
    """Assign footprint-disjoint 'extra' files to (train, val) by seeded shuffle.

    Used only in the coord regime for R_Patches_NWIextra, which has no field
    partition to inherit (new ground). ~82/18 mirrors the 70/15 whole-set ratio.
    """
    shuffled = sorted(files, key=lambda f: f.name)
    random.Random(seed + 101).shuffle(shuffled)
    cut = int(len(shuffled) * (1.0 - _VAL_OF_TRAINVAL))
    return shuffled[:cut], shuffled[cut:]


def resolve_pools(
    config: str,
    seed: int,
    leakage_guard: str = X.LEAKAGE_GUARD,
    data_root: Path | str = DEFAULT_DATA_ROOT,
    ratios: Tuple[float, float, float] = (TRAIN_RATIO, VAL_RATIO, TEST_RATIO),
) -> Tuple[List[Path], List[Path], List[Path]]:
    """Return (train_files, val_files, test_files) for one config + seed.

    TEST is always field (R_Patches) at the seed's held-out footprints. TRAIN/VAL
    come from the config's label directory(ies) filtered by the source-appropriate
    guard (plan 4.6). Mode-invariant. Raises on any test->train/val leakage.
    """
    data_root = Path(data_root)
    if leakage_guard not in ("huc12", "coord"):
        raise ValueError(f"leakage_guard must be 'huc12' or 'coord', got {leakage_guard!r}")
    X.get_config(config)  # validates name
    rule = X.config_pool_rule(config)

    if leakage_guard == "huc12":
        train_hucs, val_hucs, test_hucs = (set(s) for s in field_huc_split(data_root, seed, ratios))
        field = _list_dir(data_root, X.FIELD_TEST_DIR)
        test = [f for f in field if X.huc12_of(f) in test_hucs]

        if rule in ("anchored", "degrade"):            # fld_*, flddeg
            # flddeg returns the same FIELD files as anchored; the seeded
            # wetland->UPL relabel is applied downstream (dl_degrade_labels.py),
            # train/val only -- test stays undegraded field.
            train, val = _huc_partition(field, train_hucs, val_hucs)
        elif rule == "paired":                          # nwi
            nwi = _list_dir(data_root, "R_Patches_NWI")
            train, val = _huc_partition(nwi, train_hucs, val_hucs)
        elif rule == "extra_pool":                      # nwiextra: NWI U NWIextra
            pool = _list_dir(data_root, "R_Patches_NWI") + _list_dir(data_root, "R_Patches_NWIextra")
            train, val = _huc_partition(pool, train_hucs, val_hucs)
        elif rule == "hybrid_union":                    # nwifield: field U NWIextra
            extra = _list_dir(data_root, "R_Patches_NWIextra")
            ftr, fva = _huc_partition(field, train_hucs, val_hucs)
            etr, eva = _huc_partition(extra, train_hucs, val_hucs)
            train, val = ftr + etr, fva + eva
        else:
            raise ValueError(f"unknown pool_rule {rule!r} for config {config!r}")

    else:  # coord (patch-level field split; footprint-twin guard)
        train_fld, val_fld, test_fld = (set(s) for s in field_patch_split(data_root, seed, ratios))
        field = _list_dir(data_root, X.FIELD_TEST_DIR)
        by_name = {X.field_key(f): f for f in field}
        test = [by_name[n] for n in sorted(test_fld)]

        if rule in ("anchored", "degrade"):
            train = [by_name[n] for n in sorted(train_fld)]
            val = [by_name[n] for n in sorted(val_fld)]
        elif rule == "paired":
            nwi = _list_dir(data_root, "R_Patches_NWI")
            train = [f for f in nwi if X.nwi_field_twin(f) in train_fld]
            val = [f for f in nwi if X.nwi_field_twin(f) in val_fld]
        elif rule == "extra_pool":
            nwi = _list_dir(data_root, "R_Patches_NWI")
            ntr = [f for f in nwi if X.nwi_field_twin(f) in train_fld]
            nva = [f for f in nwi if X.nwi_field_twin(f) in val_fld]
            # NWIextra is footprint-disjoint from field (invariant), so none can
            # collide with a test patch; partition it by seeded shuffle.
            etr, eva = _seeded_trainval(_list_dir(data_root, "R_Patches_NWIextra"), seed)
            train, val = ntr + etr, nva + eva
        elif rule == "hybrid_union":
            ftr = [by_name[n] for n in sorted(train_fld)]
            fva = [by_name[n] for n in sorted(val_fld)]
            etr, eva = _seeded_trainval(_list_dir(data_root, "R_Patches_NWIextra"), seed)
            train, val = ftr + etr, fva + eva
        else:
            raise ValueError(f"unknown pool_rule {rule!r} for config {config!r}")

    _assert_no_leakage(config, seed, leakage_guard, train, val, test, data_root)
    return sorted(train, key=lambda f: f.name), sorted(val, key=lambda f: f.name), test


# --- safety net ---------------------------------------------------------------

def _assert_no_leakage(
    config: str, seed: int, guard: str,
    train: List[Path], val: List[Path], test: List[Path], data_root: Path,
) -> None:
    """Hard-fail if any test footprint reaches train/val (the 4.5 headline gate).

    huc12: no train/val patch may share a HUC12 with a test patch.
    coord:  no train/val patch may map to a test FOOTPRINT -- field basename for
            field patches, the field twin for NWI patches. NWIextra cannot collide
            (footprint-disjoint invariant), so it is exempt from the twin check.
    """
    if guard == "huc12":
        test_hucs = {X.huc12_of(f) for f in test}
        bad = [f.name for f in (train + val) if X.huc12_of(f) in test_hucs]
        if bad:
            raise AssertionError(
                f"[{config} seed{seed} huc12] {len(bad)} train/val patches in a test HUC12, "
                f"e.g. {bad[:3]}")
    else:
        test_fp = {X.field_key(f) for f in test}
        def footprint(f: Path) -> str | None:
            n = f.name
            if n.startswith("NWIextra_"):
                return None  # disjoint from field by construction
            return X.nwi_field_twin(f) if "R_Patches_NWI" in str(f.parent) else X.field_key(f)
        bad = [f.name for f in (train + val) if footprint(f) in test_fp]
        if bad:
            raise AssertionError(
                f"[{config} seed{seed} coord] {len(bad)} train/val patches share a test footprint, "
                f"e.g. {bad[:3]}")


# --- CLI (inspection / preflight aid) -----------------------------------------

def _summary(config: str, seed: int, guard: str, data_root: Path) -> Dict[str, int]:
    tr, va, te = resolve_pools(config, seed, leakage_guard=guard, data_root=data_root)
    return {"train": len(tr), "val": len(va), "test": len(te), "total": len(tr) + len(va) + len(te)}


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description="Resolve factorial-v2 train/val/test pools for a config+seed")
    ap.add_argument("--config", help="config name (default: all configs)")
    ap.add_argument("--seed", type=int, default=0)
    ap.add_argument("--leakage-guard", default=X.LEAKAGE_GUARD, choices=["huc12", "coord"])
    ap.add_argument("--data-root", default=str(DEFAULT_DATA_ROOT))
    ap.add_argument("--list-files", action="store_true", help="print resolved file names, not just counts")
    args = ap.parse_args()

    root = Path(args.data_root)
    configs = [args.config] if args.config else list(X.CONFIGS)
    print(f"data_root={root}  seed={args.seed}  leakage_guard={args.leakage_guard}\n")
    print(f"{'config':26s} {'train':>6s} {'val':>5s} {'test':>5s} {'total':>6s}  leakage")
    for name in configs:
        s = _summary(name, args.seed, args.leakage_guard, root)  # raises if leakage
        print(f"{name:26s} {s['train']:6d} {s['val']:5d} {s['test']:5d} {s['total']:6d}  OK")
    if args.list_files and args.config:
        tr, va, te = resolve_pools(args.config, args.seed, leakage_guard=args.leakage_guard, data_root=root)
        for label, lst in (("TRAIN", tr), ("VAL", va), ("TEST", te)):
            print(f"\n--- {label} ({len(lst)}) ---")
            for f in lst:
                print(f"  {f.name}")
