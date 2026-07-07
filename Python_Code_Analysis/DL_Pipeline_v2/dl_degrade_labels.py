"""
dl_degrade_labels.py  (factorial v2, plan Phase 1.3)

The `flddeg` label degrade: a seeded routine that randomly remaps field *wetland*
pixels -> UPL until the wetland prevalence matches NWI's measured prevalence,
applied to the TRAIN/VAL partition only (the test set is always undegraded field).
This isolates label *correctness/prevalence* from label *quantity* -- `flddeg`
trains on the same field footprints as `fld` but with NWI-like wetland prevalence.

v2 note: unlike the v1 tool (which MATERIALIZED a MOD_CLASS_FLDDEG band into the
merged patch dir), plan Decision 4.1 makes flddeg an IN-MEMORY relabel -- there is
no flddeg directory. This module provides the `label_transform` that
WetlandPatchDataset applies at load time (train/val only);
create_dataloaders_from_pools auto-builds one for the flddeg config.

Design points:
  * Direction: degrade only makes sense when field wetland prevalence EXCEEDS
    NWI's (NWI omits wetlands). flip_prob = 1 - nwi_wet/field_wet, clamped at 0.
    If NWI >= field (no omission signal), flip_prob is 0 and flddeg collapses to
    fld -- make_degrader warns loudly rather than fabricating a degrade.
  * Prevalence is measured DIRECTLY from the patch directories (modal 256x256, the
    same patches training sees), so it is robust to stale stats files.
  * Mode-aware: applied AFTER the dataset's binary remap, so it degrades in the
    live label space -- multiclass wetland ids {0,1,2}->3, binary {0}->1. The flip
    probability is identical in both modes (binary WET == the 3 multiclass wetland
    classes over the same pixels).
  * Deterministic per patch: the RNG is seeded from (base_seed, label bytes), so a
    given patch degrades identically every epoch and reproducibly across runs, and
    the achieved prevalence is stable. Record base_seed + flip_prob in the manifest.

Usage:
    python dl_degrade_labels.py --mode multiclass --seed 0     # measure + dry-run
"""

import argparse
import hashlib
from pathlib import Path
from typing import Dict, Sequence, Tuple

import numpy as np
import rasterio


# --- prevalence & flip probability -------------------------------------------

def measure_wetland_fraction(
    patch_dir: Path, label_band: str = "MOD_CLASS",
    wetland_ids: Sequence[int] = (0, 1, 2), patch_size: int = 256,
) -> Tuple[float, int, int, int]:
    """Aggregate wetland pixel fraction over a directory's modal-size patches.

    Off-size patches are skipped to match the training dataset's modal-size filter,
    so the measured prevalence reflects what actually trains. Returns
    (wetland_fraction, wetland_px, labeled_px, n_patches_used).
    """
    files = sorted(Path(patch_dir).glob("*.tif"))
    wet = lab = used = 0
    for f in files:
        with rasterio.open(f) as s:
            if s.height != patch_size or s.width != patch_size:
                continue
            idx = list(s.descriptions).index(label_band) + 1
            a = s.read(idx)
        v = a[~np.isnan(a)].astype(np.int64)
        lab += int(v.size)
        wet += int(np.isin(v, wetland_ids).sum())
        used += 1
    if lab == 0:
        raise ValueError(f"no labeled pixels found under {patch_dir}")
    return wet / lab, wet, lab, used


def flip_prob(field_wet: float, nwi_wet: float) -> float:
    """P(flip a field wetland pixel -> UPL) to bring field prevalence down to NWI's.

    Clamped to [0, 1]; 0 when NWI wetland prevalence >= field's (no omission to
    emulate). Derivation: after flipping wetland at prob p, wetland fraction =
    field_wet*(1-p); set equal to nwi_wet -> p = 1 - nwi_wet/field_wet.
    """
    if field_wet <= 0:
        raise ValueError("field wetland fraction must be > 0")
    return float(min(1.0, max(0.0, 1.0 - nwi_wet / field_wet)))


# --- the transform -----------------------------------------------------------

class LabelDegrader:
    """Deterministic per-patch wetland->UPL relabel (the flddeg `label_transform`).

    __call__(labels) flips each wetland pixel to UPL independently with probability
    `p`, preserving ignore_index and non-wetland pixels. The RNG is seeded from a
    hash of (base_seed, label bytes), so the same patch yields the same degrade on
    every epoch and every run at a given seed. Operates in whatever label space it
    is handed (multiclass or post-binary-remap) via `wetland_ids`/`upl_id`.
    """

    def __init__(self, p: float, wetland_ids: Sequence[int], upl_id: int,
                 ignore_index: int = 255, base_seed: int = 0):
        self.p = float(p)
        self.wetland_ids = tuple(int(i) for i in wetland_ids)
        self.upl_id = int(upl_id)
        self.ignore_index = int(ignore_index)
        self.base_seed = int(base_seed)

    def _rng(self, labels: np.ndarray) -> np.random.Generator:
        h = hashlib.blake2b(
            np.ascontiguousarray(labels).tobytes(),
            digest_size=8,
            key=int(self.base_seed).to_bytes(8, "little", signed=False),
        ).digest()
        return np.random.default_rng(int.from_bytes(h, "little"))

    def __call__(self, labels: np.ndarray) -> np.ndarray:
        if self.p <= 0.0:
            return labels
        wet = np.isin(labels, self.wetland_ids)
        n = int(wet.sum())
        if n == 0:
            return labels
        flip = self._rng(labels).random(n) < self.p
        out = labels.copy()
        rows, cols = np.where(wet)
        out[rows[flip], cols[flip]] = self.upl_id
        return out


# --- mode helpers & factory --------------------------------------------------

def ids_for_mode(mode: str) -> Tuple[Tuple[int, ...], int]:
    """(wetland_ids, upl_id) in the live label space for a classification mode.

    Matches the stats class_names order: multiclass [EMW,FSW,SSW,UPL] -> {0,1,2}->3;
    binary [WET,UPL] (after the dataset remap) -> {0}->1.
    """
    if mode == "binary":
        return (0,), 1
    if mode == "multiclass":
        return (0, 1, 2), 3
    raise ValueError(f"unknown mode {mode!r}")


def make_degrader(
    mode: str, seed: int, data_root=None,
    field_dir: str = "R_Patches", nwi_dir: str = "R_Patches_NWI",
    ignore_index: int = 255, patch_size: int = 256, verbose: bool = True,
) -> LabelDegrader:
    """Build the flddeg degrader for a mode+seed, measuring prevalence from disk.

    Wetland prevalence is measured in MULTICLASS terms (EMW+FSW+SSW) for both field
    and NWI -- identical to the binary WET fraction over the same pixels -- so the
    flip probability is mode-independent; only the flipped label ids differ.
    """
    import dl_patch_pools as P
    root = Path(data_root or P.DEFAULT_DATA_ROOT)
    wet_ids, upl = ids_for_mode(mode)

    field_wet, _, _, nf = measure_wetland_fraction(root / field_dir, patch_size=patch_size)
    nwi_wet, _, _, nn = measure_wetland_fraction(root / nwi_dir, patch_size=patch_size)
    p = flip_prob(field_wet, nwi_wet)
    if verbose:
        print(f"[degrade] mode={mode} seed={seed} | field_wet={field_wet:.4f} "
              f"({nf} patches) nwi_wet={nwi_wet:.4f} ({nn}) -> flip_prob={p:.4f} "
              f"(achieved wetland ~{field_wet * (1 - p):.4f})")
        if p == 0.0:
            print("[degrade][WARN] NWI wetland prevalence >= field's: no omission to "
                  "emulate, flip_prob=0 -> flddeg is identical to fld. Check the data "
                  "or drop the flddeg config.")
    return LabelDegrader(p, wet_ids, upl, ignore_index=ignore_index, base_seed=seed)


def degraded_class_counts(
    field_counts: Dict[int, int], p: float,
    wetland_ids: Sequence[int], upl_id: int,
) -> Dict[int, int]:
    """Expected class counts after degrade (for building flddeg's stats/weights).

    Analytic: each wetland class keeps a (1-p) share; the removed wetland pixels
    accrue to UPL. Seed-independent in expectation, so one stats file serves all
    seeds. Returns integer-rounded counts.
    """
    out = dict(field_counts)
    moved = 0.0
    for w in wetland_ids:
        c = field_counts.get(w, 0)
        out[w] = int(round(c * (1.0 - p)))
        moved += c * p
    out[upl_id] = int(round(field_counts.get(upl_id, 0) + moved))
    return out


# --- CLI (inspection / dry-run) ----------------------------------------------

if __name__ == "__main__":
    import dl_patch_pools as P
    ap = argparse.ArgumentParser(description="Inspect / dry-run the flddeg label degrade")
    ap.add_argument("--mode", choices=["multiclass", "binary"], default="multiclass")
    ap.add_argument("--seed", type=int, default=0)
    ap.add_argument("--data-root", default=str(P.DEFAULT_DATA_ROOT))
    ap.add_argument("--sample", type=int, default=40,
                    help="patches to dry-run the achieved prevalence on (default 40)")
    args = ap.parse_args()

    root = Path(args.data_root)
    deg = make_degrader(args.mode, args.seed, data_root=root)

    # Dry-run: apply to a sample of field patches, report achieved wetland fraction.
    wet_ids, upl = ids_for_mode(args.mode)
    remap_binary = args.mode == "binary"
    files = sorted((root / "R_Patches").glob("*.tif"))[: args.sample]
    before = after = lab = 0
    for f in files:
        with rasterio.open(f) as s:
            if s.height != 256 or s.width != 256:
                continue
            idx = list(s.descriptions).index("MOD_CLASS") + 1
            a = s.read(idx)
        y = np.where(np.isnan(a), 255, a).astype(np.int64)
        if remap_binary:
            y = np.where(np.isin(y, [0, 1, 2]), 0, np.where(y == 3, 1, y))
        v = y[y != 255]
        lab += int(v.size)
        before += int(np.isin(v, wet_ids).sum())
        yd = deg(y)
        vd = yd[yd != 255]
        after += int(np.isin(vd, wet_ids).sum())
    if lab:
        print(f"[dry-run] {len(files)} field patches: wetland {before / lab:.4f} -> "
              f"{after / lab:.4f} after degrade (flip_prob={deg.p:.4f})")
