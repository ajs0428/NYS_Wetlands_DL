# Wetland DL Factorial Experiment v2 — Claude Code Implementation Plan

**Purpose.** Implement and orchestrate version 2 of the factorial experiment on the NYS Wetlands DL
pipeline. v2 improves on v1 in three ways: it adds a **binary (WET/UPL) classification mode as a full
axis** run alongside multiclass, it adds **more NWI training patches** (the paired set plus an
equal-size "extra" set at new locations → 2× combined) to test whether label *quantity* can close the
NWI-vs-field gap, and it **regroups the LiDAR axis**
(CHM folded in with the return fractions). It investigates:

1. **Label provenance** — how much does training-label quality (NWI vs. field-verified) affect binary
   and multiclass wetland mapping accuracy and skill? Is any gap driven by *quantity* (fewer wetland
   pixels) or *correctness* (the wrong pixels)? Can adding NWI *quantity* (`nwiextra`) reach field-level
   skill? Does a hybrid NWI + field set (`nwifield`) improve skill and accuracy?
2. **Below-canopy feature contribution** — which LiDAR-derived features and which seasonal imagery
   resolve wetland classification into Forested (FSW), Shrub-Scrub (SSW), and Emergent (EMW)?
3. **Binary vs. Multiclass** — repeating the factorial with labels collapsed to wetland-vs-upland
   (FSW/EMW/SSW → WET), do we see different mapping skill and different use of predictor features?

**Thesis:** *LiDAR structure and leaf-off spectra are two below-canopy sensing modalities that resolve
wetland vs. upland in a binary model and wetland-class confusion in a multiclass model.* Clean
field-verified labels make that diagnosis trustworthy in a way NWI-trained studies cannot claim.

This document is the build brief: it specifies the experiment matrix, the pipeline mechanisms to add,
the shell orchestration, and the output layout.

### What changed from v1 (at a glance)
- **Classification mode is now an axis:** every config runs in **both** `multiclass` and `binary`
  (Decision 4.4). Run count doubles; results and stats are separated by a mode token.
- **LiDAR axis dropped to two tiers:** `{nolidar, chmret}` — the CHM-only tier is gone (Decision, §2).
- **Label sources are separate directories, not label bands in one file** (Decision 4.1). This
  reintroduces the cross-source alignment hazard v1 dodged, so the split logic and preflight are
  redesigned around **directory-aware keys** (filename pairing for field↔NWI, HUC12 geography for the
  `NWIextra` leakage guard) and a **field-anchored split** (Decisions 4.5–4.6).
- **Two new label configs:** `nwiextra` (~2× NWI patches, extra locations in the same HUC12s) and
  `nwifield` (field ∪ non-overlapping NWI).

---

## 1. Existing pipeline (context for Claude Code)

The pipeline lives in `Python_Code_Analysis/DL_Pipeline_v2/`. Constraints that shape this work:

- **Bands are discovered at runtime** from GeoTIFF band descriptions — no hardcoded indices. Band
  names drive everything via `dl_band_utils.py`.
- **Classification mode** (`multiclass` / `binary`) is set in `dl_band_config.json`
  (`classification_mode` + `binary_mapping`). `dl_02_dataset.py` already builds a label-remap LUT and
  collapses EMW/FSW/SSW→WET for binary. v2 **exercises both modes**, so mode must be selectable per run,
  not just edited once in the JSON.
- `Geomorph_local` is **one-hot expanded** 1 band → 10 channels. Channel counts must account for this.
- The stats JSON (from `dl_01_compute_statistics.py`) is the single source of truth for `in_channels`,
  per-band normalization, `predictor_names`, `class_names`, `weight_power`, and class weights.
  **Filenames are mode- and weight-power-specific** — `dl_band_utils.stats_filename()` resolves to
  `<mode>_normalization_stats[_wp<power>].json`. The production multiclass master is
  **`multiclass_normalization_stats_wp0.5.json`**; the binary master will be
  **`binary_normalization_stats_wp0.5.json`** (same predictors, different `class_names` and weights).
- **Class weights are power-scaled** (`(1/freq)**P`). Hold **`--weight-power 0.5`** fixed across every
  run so weighting is not a hidden variable; the `_wp0.5` suffix appears on every stats file. Weights
  still differ per config because they are recomputed from the *active label source's* pixel counts —
  that difference is intended; the power is not.
- Training is `dl_04_train_lightning.py`; evaluation `dl_05_evaluate.py`. Both derive `num_classes`
  from `len(stats["class_names"])`, so binary vs. multiclass flows from the stats file automatically.
- Loss is fixed: **plain weighted CE** (`--ce-weight 1.0 --dice-weight 0.0 --focal-gamma 0`).
- Architecture is fixed: **U-Net** (`--arch unet`), HPC settings (`--base-filters 64 --depth 5`).

The experiment changes **inputs, labels, and classification mode only**, holding architecture, loss,
optimizer, schedule, and splits constant.

---

## 2. Experiment matrix (8 configs × 2 modes)

### Constant base (present in every config)
Terrain + leaf-on NAIP: `DEM`, `slope_local`, `Geomorph_local`, `flowacc`, `twi`, `r`, `g`, `b`, `nir`.
= **18 channels** (8 single-channel bands + 10 from one-hot `Geomorph_local`).

### Two experimental axes (field-verified labels only)
- **LiDAR tier (2 levels):**
  - `nolidar` — base only
  - `chmret` — base + `CHM` + `pct_below_1m` + `pct_1m_to_5m` + `pct_above_5m`
- **Leaf-off (2 levels):**
  - `leafon` — base only
  - `leafoff` — base + `r_lo`, `g_lo`, `b_lo`, `nir_lo`

CHM is grouped with the return fractions (this is the v1→v2 change): the LiDAR axis contrasts
*no structure* vs. *full structure*, not CHM-alone vs. CHM+returns.

### Factorial cells (2 × 2, field-verified labels)

| LiDAR ↓ \ Spectral → | leaf-on | leaf-on + leaf-off |
|---|---|---|
| **no LiDAR** | `fld_nolidar_leafon` | `fld_nolidar_leafoff` |
| **CHM + returns** | `fld_chmret_leafon` | `fld_chmret_leafoff` *(= full feature set)* |

### Label block (full feature set only — scope control)

| Config | Training labels | Directory | Notes |
|---|---|---|---|
| `fld_chmret_leafoff` | Field (shared from factorial) | `R_Patches/` | Gold-standard baseline |
| `nwi_chmret_leafoff` | NWI, paired to field locations | `R_Patches_NWI/` | Operational stale-label baseline |
| `nwiextra_chmret_leafoff` | NWI, ~2× (1378 = paired ∪ extra same-HUC12 locations) | `R_Patches_NWI/` ∪ `R_Patches_NWIextra/` | Quantity test: can more NWI reach field skill? |
| `nwifield_chmret_leafoff` | Field ∪ non-overlapping NWI | `R_Patches/` + `R_Patches_NWIextra/` | Hybrid: does adding NWI to field help? |
| `flddeg_chmret_leafoff` | Field, degraded to NWI wetland prevalence | `R_Patches/` + code | Quantity-vs-correctness control |

> **Scope-control rule (do not violate):** the label comparison runs **only** on the full feature set
> (`chmret_leafoff`); feature ablations run **only** on field-verified labels. This keeps the matrix at
> **8 configs**, not the full cross-product.

### Per-config band composition and channel counts

| # | Config | LiDAR bands added | Leaf-off added | `in_channels` |
|---|---|---|---|---|
| 1 | `fld_nolidar_leafon`      | —               | —  | 18 |
| 2 | `fld_nolidar_leafoff`     | —               | 4  | 22 |
| 3 | `fld_chmret_leafon`       | CHM + 3 returns | —  | 22 |
| 4 | `fld_chmret_leafoff`      | CHM + 3 returns | 4  | **26** |
| 5 | `nwi_chmret_leafoff`      | CHM + 3 returns | 4  | 26 |
| 6 | `nwiextra_chmret_leafoff` | CHM + 3 returns | 4  | 26 |
| 7 | `nwifield_chmret_leafoff` | CHM + 3 returns | 4  | 26 |
| 8 | `flddeg_chmret_leafoff`   | CHM + 3 returns | 4  | 26 |

> **Channel sanity anchor = config 4 (`fld_chmret_leafoff`).** It is the full 17-predictor feature set
> (26 channels after `Geomorph_local` one-hot 1→10) and **is** the master stats file's predictor set.
> The preflight asserts `config_in_channels("fld_chmret_leafoff") == 26` against
> `multiclass_normalization_stats_wp0.5.json` byte-for-byte on the predictor side. Configs 5–8 share
> config 4's predictors (they differ only in the label source), so they inherit the same 26.

### Classification mode axis
Every config runs in **both** modes:
- `multiclass` — EMW/FSW/SSW/UPL (4 classes)
- `binary` — WET/UPL (2 classes; EMW/FSW/SSW → WET via `binary_mapping`)

Predictor normalization is identical between modes (same bands); only `class_names`, `num_classes`, and
class weights change. Each mode gets its own master + per-config stats (`multiclass_*` / `binary_*`) and
its own results subtree.

### Run counts
- **R = replicates** (seeds over a fixed split; Decision 4.3).
- 8 configs × 2 modes × R seeds. **R = 3 → 48 runs**; R = 5 → 80 runs.

---

## 3. Shared design constants (apply to all runs)

- **Test set is always field-verified, undegraded, and identical across configs for a given seed.**
  Both label sources and both modes are judged on the same gold-standard field pixels. Non-negotiable —
  it fixes the evaluation confound. (Mechanism: the field-anchored split, Decision 4.5.)
- **Validation follows the training label source.** NWI/nwiextra/nwifield configs early-stop on their
  own (NWI or hybrid) validation labels; field and flddeg configs validate on field labels. Test stays
  field for all.
- **Identical field split across configs** for a given seed, so differences are attributable to
  inputs/labels/mode, not to which patches landed where.
- **Fixed loss** (weighted CE), **fixed class-weight power** (`--weight-power 0.5`), **fixed
  architecture**. Class weights still differ per config/mode (recomputed from the active label source at
  the same power) — intended; the power is held.
- **Metrics:** per-class IoU, recall/precision, macro-F1, and the **full confusion matrix** (the
  UPL↔FSW cells in multiclass; the WET↔UPL cell in binary). Overall accuracy is reported but never the
  headline (it tracks UPL prevalence).

---

## 4. Decisions (all settled)

### 4.1 Label storage — **DECIDED: separate directories, directory-aware keys**
v1 stored multiple label bands (`MOD_CLASS_FLD/_NWI/_FLDDEG`) in one merged patch file, so alignment
was free. **v2 uses one directory per label source** because the NWI patch *count* varies (the quantity
test needs ~2× as many patches, at new locations), which a single fixed-grid file cannot express. As
built (verified on disk 2026-07-06, 689 patches each):

- `R_Patches/` — field-verified patches (label band `MOD_CLASS` = the field label).
- `R_Patches_NWI/` — NWI patches at the **same locations** as `R_Patches` (paired 1:1; `MOD_CLASS` = NWI
  label per the 4.2 semantics). Filename = `"NWI_"` + the field basename.
- `R_Patches_NWIextra/` — NWI patches at **new locations within the same HUC12 watersheds**,
  **geographically disjoint** from field/NWI (0 patch-window overlap; nearest ≥ 258 m). This directory
  holds the *extra* locations only; the **~2× quantity pool is `R_Patches_NWI ∪ R_Patches_NWIextra`
  (1378 distinct footprints)**, assembled at config time (`nwiextra` patch_dirs), not stored redundantly.

Separate directories reintroduce the alignment hazard v1 avoided. Two **directory-aware** relations
replace the (broken) filename-substring key — see the 4.5 keying note for why `cluster_..._patch_N` is
*not* a valid identity:

- **Filename pairing (field↔NWI):** `nwi_field_twin()` strips exactly one leading `"NWI_"`, recovering
  the field basename (verified 1:1 over 689, incl. the 252 NWI-sourced field patches whose twin is
  double-prefixed `NWI_NWI_…`). **Paired twin ⇒ same ground, pixel-for-pixel** (preflight verifies; §5
  Phase 0). This is what lets the field test set judge NWI-trained models and proves the `nwi`-vs-`fld`
  contrast is clean.
- **HUC12 geography (`NWIextra`):** `huc12_of()` — `NWIextra` shares no footprint with field, so its
  leakage guard is watershed-level, never filename-based.

`flddeg` is **not** a directory: it is a seeded in-memory relabel of `R_Patches` field labels on the
train/val partition (Phase 1.3), so its footprints and test set are trivially field-aligned.

### 4.2 NWI "non-wetland" semantics — **DECIDED: confirmed UPL**
A pixel with no NWI wetland polygon is labeled **confirmed UPL (class 3)**, not `ignore_index`. NWI
omission errors therefore enter training as realistic FSW/SSW/EMW→UPL label noise — the operational
stale-label case the experiment measures, and the condition under which `flddeg` (field wetland
prevalence degraded down to NWI's) is the clean quantity-vs-correctness control. `ignore_index=255` is
reserved only for genuinely unlabeled / out-of-footprint pixels, and its mask must be identical between
the paired field and NWI patches so the comparison stays pixel-aligned. Baked into label generation
(Phase 0 rasterization: non-wetland→3, no-data→255), the split logic (no per-source ignore remap), and
the preflight (assert the NWI value set is `{0,1,2,3,255}` and its 255 mask matches the field patch's).

> *Robustness check (optional, later):* an `ignore_index`-for-NWI-omissions variant, but confirmed-UPL
> is the headline.

### 4.3 Replication — **DECIDED: repeated fixed-split seeds, start R=3**
Outer loop `for seed in 0,1,2` over a fixed 70/15/15 field split: 8 configs × 2 modes × 3 seeds =
**48 runs**. The **same seed yields the same field train/val/test partition across all configs and both
modes** (via the field-anchored split, 4.5), so differences are attributable to inputs/labels/mode, not
split luck. The runner is idempotent (§Phase 2), so extending to R=5 just adds new `seed<k>/` cells.
Start at R=3 because the mode axis already doubles the grid vs. v1; extend only if seed variance
warrants it.

### 4.4 Classification mode — **DECIDED: run both multiclass and binary as a full axis**
Every config is trained and evaluated in both modes. Mechanism: mode is selected **per run** (not by
hand-editing `dl_band_config.json`), drives which master/per-config stats file is used
(`multiclass_*` vs `binary_*`), and adds a **mode token to the results root** so the two grids never
collide. Because `num_classes`/`class_names`/weights all flow from the stats file, the trainer and
evaluator need no mode-specific code beyond selecting the right stats and setting `classification_mode`.

### 4.5 Split anchor & leakage guard — **DECIDED: the field split is the anchor**
The single most likely silent failure in v2 is a test patch leaking into a training pool through the
separate directories. The rule:

1. Compute the split **once on `R_Patches` (field)** with `create_data_splits(seed)` →
   `train_fld` / `val_fld` / `test_fld` **sets of field basenames** (`field_key()` = the raw
   `R_Patches` basename; each of the 689 is unique — see the keying note below).
2. **`test_fld` is the test set for every config and both modes** — always drawn from `R_Patches`
   (field labels, undegraded).
3. Each config's **train/val pools are drawn from its own label directory and filtered clear of the
   `test_fld` footprints** using the *source-appropriate* key (below): paired filenames for
   `fld`/`nwi`/`flddeg`, HUC12 geography for `nwiextra`/`nwifield`. For degrade, only the train/val
   partition is relabeled.
4. **Hard preflight assertion:** no `test_fld` footprint reaches any config's train or val pool under
   its guard.

> **Keying note (corrected 2026-07-06, was the v1→v2 keying bug).** Do **not** key patches by the
> `cluster_..._patch_N` substring: it drops the source-dataset prefix (`ADK_WCT_AJS_`, `gps_jc_`,
> `NWI_RSM_`, `NEW_AJS_`, …), which is identity-bearing — every source restarts `patch_N` inside a
> cluster+HUC. On the real data that substring is neither unique within `R_Patches` (56 collisions)
> nor comparable across directories (594 *spurious* field↔`NWIextra` "matches"). Two distinct relations
> replace it (`dl_experiment_config.py`): **filename pairing** for field↔NWI — `R_Patches_NWI` is
> exactly `"NWI_"` + the field basename, so `nwi_field_twin()` strips exactly one leading `NWI_`
> (verified 1:1 over all 689, including the 252 NWI-sourced field patches whose twin is double-prefixed
> `NWI_NWI_…`); and **HUC12 geography** for the `NWIextra` leakage guard via `huc12_of()`.

> **Spatial-leakage sub-decision — DECIDED (2026-07-06): run both, HUC12 is the headline.**
> `R_Patches_NWIextra` is geographically **distinct** from field (0 patch-window overlap; nearest field
> patch ≥ 258 m > the 256 m patch width, so hard/pixel leakage is already zero) yet shares **all 29
> HUC12s**, so a training patch can sit in the *same watershed* as a test patch (soft autocorrelation
> leakage). `LEAKAGE_GUARD` (in `dl_experiment_config.py`, `--leakage-guard` override) selects the
> regime: **`huc12`** (default, headline) drops every `NWIextra` patch sharing a HUC12 with a `test_fld`
> patch — the conservative, reviewer-proof number; **`coord`** (sensitivity run) drops only `NWIextra`
> patches whose 256 m window overlaps a `test_fld` patch (currently none, so it keeps ~all extra data).
> Agreement between the two shows the `nwiextra` quantity gain is real, not autocorrelation. The regime
> is recorded in every manifest.

### 4.6 `nwiextra` and `nwifield` pool construction — **DECIDED**
Test is always `R_Patches[test_fld]`. Because `NWIextra` is a **new-ground, independent-namespace**
directory (never a filename match to field — see the 4.5 keying note), its pools are filtered by the
**HUC12 leakage guard**, not by filename subtraction:

- **`nwiextra`** — train/val pool = **`R_Patches_NWI ∪ R_Patches_NWIextra`** (1378 footprints, ~2× the
  689-patch `nwi` pool) with every HUC12 holding a `test_fld` patch dropped under `LEAKAGE_GUARD`, split
  into train/val by seed (~82/18 of the surviving pool, mirroring 70/15 of the whole). Contrast against
  `nwi` (same labels, 1× count) isolates the effect of NWI *quantity* at fixed correctness.
- **`nwifield`** — train/val pool = field-labeled `R_Patches[train_fld ∪ val_fld]` **∪** the
  `nwiextra` pool above (same HUC12 guard). The union is disjoint by construction — `NWIextra`
  footprints never coincide with field ones (≥ 258 m apart), so there is no label conflict to arbitrate.
  Isolates whether adding NWI *coverage* to a field core helps.

---

## 5. Implementation phases

### Phase 0 — Data prerequisites & preflight (verify before any GPU time)

The label-provenance axis only means something if NWI and field labels are judged on the **exact same
field pixels**. Build/extend `dl_preflight_check.py` to hard-fail before GPU time. v2 assertions
(rewritten for separate directories):

- [ ] **Field↔NWI pairing parity (paired sources).** `{nwi_field_twin(f) : f ∈ R_Patches_NWI}` equals
      `{field_key(f) : f ∈ R_Patches}` exactly (verified 1:1 over 689). Report and fail on any asymmetry.
      Do **not** compare `R_Patches_NWIextra` by filename — it is an independent `patch_N` namespace;
      instead assert `{huc12_of(f) : f ∈ R_Patches_NWIextra} ⊆ {huc12_of(f) : f ∈ R_Patches}` (all extra
      ground lies in field HUC12s — verified: 29/29 shared, 0 extra-only).
- [ ] **Identical footprints per paired twin.** For each field↔NWI twin, the two rasters share CRS,
      transform, width, height, and nodata — same grid, pixel-for-pixel. (`NWIextra` has no field twin
      by design — 0 shared centroids — so it is exempt from this pixel-parity check.)
- [ ] **Predictor parity.** All 17 predictor bands present and named exactly as the pipeline expects,
      identical across directories. Authoritative set = `predictor_names` in
      `multiclass_normalization_stats_wp0.5.json`: `DEM, slope_local, Geomorph_local, flowacc, twi, CHM,
      r, g, b, nir, r_lo, g_lo, b_lo, nir_lo, pct_below_1m, pct_1m_to_5m, pct_above_5m` (returns use the
      `_1m`/`_5m` names, not the older `_0.5m`/`_2m` names still in CLAUDE.md).
- [ ] **Label-value sanity.** Every label band contains only `{0,1,2,3,255}`; flag stray values and
      report per-directory class prevalence so the `flddeg` degradation target (NWI wetland prevalence)
      is *measured*, not assumed. In `binary` mode, assert the remap yields only `{0,1,255}`.
- [ ] **NWI ignore-mask match.** For each paired twin, the NWI 255 mask equals the field 255 mask.
- [ ] **Field-anchored split alignment.** For a given seed, `test_fld` is identical across all configs
      and both modes, and **no `test_fld` footprint reaches any config's resolved train/val pool** under
      its guard — filename identity for `fld`/`nwi`/`flddeg`, and no `test_fld` HUC12 in the
      `nwiextra`/`nwifield` pool under `LEAKAGE_GUARD` (the 4.5 leakage guard — the headline gate).
- [ ] **Channel sanity, per mode.** `config_in_channels("fld_chmret_leafoff")` resolves to **26** for
      both the multiclass and binary masters (predictors are mode-invariant).
- [ ] **NWI labels generated** with 4.2 semantics (non-wetland→3, no-data→255) before the check can go
      green for configs 5–8.

### Phase 1 — Pipeline mechanism changes (Python, built once)

**1.1 Band selection.** Config-driven active-predictor subset so a run trains on a subset of discovered
bands (`config_bands()` in `dl_experiment_config.py` already emits the list). Recompute `in_channels`
with one-hot expansion via `compute_in_channels` (`dl_band_utils.py`). Unit check: `fld_chmret_leafoff`
→ 26.

**1.2 Directory-based label source + field-anchored split.** This replaces v1's label-band toggle. Add
a **`dl_patch_pools.py`** (or extend `create_data_splits`) that, given `(config, seed, mode)`, returns
`train/val/test` file lists per Decisions 4.5–4.6: field split computed on `R_Patches` (by `field_key`),
train/val pulled from the config's label directory(ies) filtered by the source-appropriate guard
(filename pairing for `fld`/`nwi`/`flddeg`, `LEAKAGE_GUARD` HUC12 geography for `nwiextra`/`nwifield`),
**test always `R_Patches[test_fld]`**. A
config→directory(+rule) registry lives in `dl_experiment_config.py` (extends `CONFIGS`), so the datamodule
never hardcodes a path. The loss mask is `ignore_index=255` for all sources (no per-source remap, per 4.2).

**1.3 Degradation utility (`dl_degrade_labels.py`).** Seeded routine that randomly remaps field
*wetland* pixels → UPL on the **train/val partition only** until wetland prevalence matches NWI's
measured prevalence, producing `flddeg`. Record the seed and achieved prevalence in the manifest. Test
labels are never degraded.

**1.4 Per-config × per-mode stats.** Avoid full rescans: compute a **master per mode** once over all 17
predictors at `--weight-power 0.5` (`multiclass_*` already exists; add `binary_*`), then
`dl_make_config_stats.py` subsets the normalization dict to the active predictors, recomputes
`in_channels`, and recomputes class weights **from the active label source** (field/NWI/flddeg pixel
counts) at the same power. Output name:
`<mode>_normalization_stats_<config>_wp0.5.json`. `dl_experiment_config.stats_basename(name, mode=...)`
already generates these names — thread `mode` through `dl_make_config_stats.py`.

> **Master already exists (multiclass).** `multiclass_normalization_stats_wp0.5.json` is the
> 17-predictor / 26-channel master and doubles as config 4's stats. The helper derives the reduced-band
> configs from it. Build the `binary` master the same way (predictors identical; only class stats/weights
> differ).

### Phase 2 — Shell orchestration

**2.1 Shared runner** — `run_config.sh` takes `CONFIG`, `SEED`, and (new) **`MODE`**. It:
1. resolves per-config, per-mode stats (`<mode>_normalization_stats_<config>_wp0.5.json`) via
   `dl_experiment_config.py --emit` and passes `--stats-path`;
2. resolves the config's patch directory + pool rule (4.6) and passes it to the datamodule;
3. sets `classification_mode=<MODE>` for the run;
4. trains with fixed loss + architecture, evaluates on the **field** `test_fld` set;
5. writes to `RESULTS_DIR/<mode>/<config>/seed<k>/`.

**New/changed env knobs:** `MODE` (`multiclass`|`binary`), and **`STATS_DIR`** must become an env knob
(currently hardcoded to `Data/Training_Data/stats` at `run_config.sh:44` — see §12/Section-11 verdict).
`RESULTS_DIR` gains a mode token in the path.

**2.2 Wrapper scripts + drivers.** One wrapper per config (`run_<config>.sh`) looping over seeds, and a
top-level `run_factorial.sh` that walks **(mode × config × seed)** in a fixed order. Keep the 1:1:1
naming (config ⇄ wrapper ⇄ `results/<mode>/<config>/`).

**2.2a Resumability (required).** Idempotent runner: at entry, skip any cell with a completed
`results/<mode>/<config>/seed<k>/metrics.json` (+ `manifest.json`), so the 48-run grid survives
stop/resume across BioHPC reservation windows.

**2.3 Manifest per run** — record: active bands, resolved `in_channels`, **mode**, label source +
resolved patch directory + pool rule, **leakage regime** (patch-level vs HUC12-level), ignore-index
rule, seed, loss params, `weight_power` (0.5) + resolved class weights, stats filename, architecture,
git commit, and (flddeg) degradation seed + achieved prevalence. Every result self-describing.

### Phase 3 — Aggregation & SHAP *(placeholder — user to expand)*

Draft scope only — **hold for the user's additional steps before building:**
- Aggregate `results/<mode>/<config>/seed<k>/metrics.json` into per-mode factorial tables (mean ± sd,
  paired across seeds), plus a **mode-comparison** table (binary vs multiclass on the shared configs).
- Headline contrasts: LiDAR (`nolidar→chmret`), leaf-off main effect, LiDAR×leaf-off interaction, and
  the **label gradient** (`nwi` → `nwiextra` → `nwifield` → `flddeg` → `fld`) — the quantity-vs-
  correctness curve.
- SHAP on the field-trained models (both modes), one-hot bands aggregated back to band level. **GPU-side**
  (it backprops); aggregation is pure pandas, **CPU-side** after sync-back.

---

## 6. Output directory layout

```
Models/factorial_results_v2/
├── multiclass/
│   ├── fld_nolidar_leafon/seed{0,1,2}/
│   │   ├── manifest.json
│   │   ├── metrics.json            # per-class IoU/recall/precision, macro-F1
│   │   ├── confusion_matrix.csv    # full CM (UPL↔FSW cells)
│   │   ├── best.ckpt (+ .safetensors / .meta.json)
│   │   └── train_log.csv
│   ├── fld_nolidar_leafoff/ ...  fld_chmret_leafon/ ...  fld_chmret_leafoff/ ...
│   ├── nwi_chmret_leafoff/ ...  nwiextra_chmret_leafoff/ ...
│   └── nwifield_chmret_leafoff/ ...  flddeg_chmret_leafoff/ ...
└── binary/
    └── <same 8 configs>/seed{0,1,2}/ ...

Data/Training_Data/stats/          # (or stats_v2/ once STATS_DIR is a knob — §12)
├── multiclass_normalization_stats_wp0.5.json                     # multiclass master (= config 4)
├── binary_normalization_stats_wp0.5.json                         # binary master
├── multiclass_normalization_stats_<config>_wp0.5.json  (×8)
└── binary_normalization_stats_<config>_wp0.5.json      (×8)
```

Folder name = `<mode>/<config>` = the row label in the final factorial tables, so Phase-3 aggregation
is a directory walk.

---

## 7. Summary of what Claude Code builds

| Phase | Deliverable | Touches |
|---|---|---|
| 0 | Preflight rewrite: field↔NWI pairing parity + `NWIextra` HUC12⊆field, footprint match, predictor parity, label values, NWI mask, **field-anchored split + leakage guard**, per-mode 26-ch sanity | `dl_preflight_check.py` |
| 1.1 | Band-subset selection + channel recompute | `dl_experiment_config.py`, `dl_band_utils.py` |
| 1.2 | **Directory-based label source + field-anchored, leakage-safe split** | new `dl_patch_pools.py`, `dl_02_dataset.py`, `dl_experiment_config.py` (config→dir registry) |
| 1.3 | Seeded degradation (train/val only) | new `dl_degrade_labels.py` |
| 1.4 | Per-config **× per-mode** stats subsetting (`multiclass_*`/`binary_*`, `_wp0.5`) | `dl_make_config_stats.py`, `dl_01`, `dl_band_utils.py` |
| 2.1 | Shared runner with **`MODE`** + **`STATS_DIR`** knobs, mode-tokened `RESULTS_DIR`, idempotent | `Shell_Scripts/run_config.sh` |
| 2.2 | 8 config wrappers + `run_factorial.sh` walking (mode × config × seed) | `Shell_Scripts/run_<config>.sh`, `run_factorial.sh` |
| 2.3 | Run manifests (mode, dir, pool rule, leakage regime) | runner |
| 2.4 | Results sync-back (mode-tokened tree) | `Shell_Scripts/rsync_results.sh` |
| 3 | Aggregation (CPU) + SHAP (GPU), incl. mode comparison + label gradient | `dl_08*`, `dl_09_shap_factorial.py` — **awaiting user's added analyses** |

**First actions for Claude Code (once the plan is confirmed and data is staged):** update
`dl_experiment_config.py` to the final v2 matrix (§10), build the directory-based split + preflight
(Phase 0/1.2), and verify the per-mode 26-channel sanity before anything else.

---

## 8. Feasibility & HPC execution

Target: **non-GPU node** for orchestration/analysis; reserved **BioHPC GPU node**
(`cbsugpu09`/`cbsugpu10`: Rocky 9.8, 256 cores, 512 GB RAM, 7 TB NVMe `/workdir`, 1× RTX A6000, 48 GB
VRAM) for training via `docker1`. Verdict: **feasible; binding constraints are data generation and
single-GPU serialization, not compute.**

### 8.1 Data readiness — the real gate
- **Feature-ablation axis (configs 1–4, field labels): ready** once the field patches carry the 17
  predictors. Needs Phase 1.1 + 1.4 only.
- **Label-provenance block (configs 5–8): blocked on the NWI directories.** `R_Patches_NWI` exists;
  `R_Patches_NWIextra` is still being built (paired + same-HUC12 extras). Preflight cannot go green for
  5–8 until both exist with 4.2 semantics. **Track NWI-directory generation as the gating task.**
- **Binary mode:** no new data — flows from the existing `binary_mapping`. Needs the `binary_*` stats
  masters (Phase 1.4).

### 8.2 Compute fits the A6000
- ~125M-param U-Net (bf64/d5), 256², batch 16, 26 channels fits 48 GB in fp32; trivial with
  `--precision 16-mixed`. Binary has *fewer* output channels → no extra memory.
- Per run ≈ 20–60 min with early stopping.
- **Totals (single GPU, sequential): 48 runs (R=3, both modes) ≈ 16–48 GPU-h**; 80 runs (R=5) ≈
  27–80 GPU-h. ~2 reservation windows at R=3.
- Disk: 48 runs × (~500 MB `.ckpt` + ~500 MB `.safetensors` + logs) ≈ **~50 GB** of 7 TB.

### 8.3 Single GPU + reservation windows = binding execution constraint
One A6000 ⇒ runs serialize; 48–80 runs won't fit one reservation. Mitigations (baked in): idempotent
runner (skip completed cells) + start R=3. Walk order in `run_factorial.sh` should finish one mode's
grid before the other so a partial run still yields a complete multiclass factorial.

### 8.4 Docker / HPC mechanics
- **`docker1`, never `docker`** — the site wrapper supplying privileges.
- **Mounts only under `/workdir/$USER`** (hard BioHPC rule). `/ibstorage/...` cannot be bind-mounted:
  `rsync` the repo + `Data/` to `/workdir/$USER/nys_wetlands/`, then
  `docker1 run --gpus all --shm-size=8g --user $(id -u):$(id -g) -v /workdir/$USER/nys_wetlands:/app ...`.
  **v2 pushes three patch directories** (`R_Patches`, `R_Patches_NWI`, `R_Patches_NWIextra`) instead of
  one merged dir — size the lean push accordingly.
- Long jobs under `screen`/`tmux`; scratch under `/workdir/$USER/tmp`, not `/tmp`.

> **Agent boundary (`AGENTS.md`):** Claude **prepares** scripts; the **user executes** them. No
> auto-launch of training, Docker, or rsync.

### 8.5 Sync-back & CPU-node analysis
- `rsync_results.sh` pulls `/workdir/.../Models/factorial_results_v2/<mode>/...` back to `/ibstorage`.
  Supports `--dry-run` / `--metrics-only` / per-config filtering. Aggregation is CPU-side post-sync;
  SHAP is GPU-side before teardown.

### 8.6 Feasibility checklist
- [ ] `R_Patches_NWI` + `R_Patches_NWIextra` generated (4.2 semantics); field↔NWI pairing verified 1:1, `NWIextra` HUC12s ⊆ field
- [ ] `binary_*` stats masters built; both masters pass the 26-ch sanity
- [ ] Preflight GREEN for all 8 configs, both modes (esp. the leakage guard)
- [ ] Repo + 3 patch dirs rsynced to `/workdir/$USER`; image loaded via `docker1`
- [ ] `run_factorial.sh` launched in `screen`/`tmux`; resumable across windows
- [ ] R chosen (start R=3); identical field split verified across configs/modes by the preflight
- [ ] `rsync_results.sh` dry-run round-trips the mode-tokened tree
- [ ] SHAP scheduled GPU-side; aggregation CPU-side post-sync
- [ ] Heavy jobs run by the **user** from prepared scripts

---

## 9. Follow-on studies (carried from v1; unchanged mechanics)

Three studies reuse the factorial machinery and the **best** trained config (runtime-chosen from the
factorial table; likely `fld_chmret_leafoff`). They do not change the 8×2×R base grid and hold its
constants unless noted. Pick the **mode** explicitly (default `multiclass`) — the runner's `MODE` knob
threads through.

- **Phase 4 — patch-count learning curve** → `Models/results_patchcurve_v2/<mode>/<config>_n<level>/seed<k>/`.
  `--n-patches` caps the seed-shuffled pool before the split. Levels `{100,200,300,400,500,full}`.
  For NWI configs the curve also answers Q1's *quantity* question directly (`nwiextra` levels).
- **Phase 5 — UNet3+ comparison** → `Models/results_arch_v2/<mode>/<config>_unet3plus/seed<k>/`.
  Same capacity (bf64/d5), deep-supervision ON, `16-mixed` + batch 8 (→4 on OOM). U-Net arm is the base
  grid.
- **Phase 6 — prediction/inference maps** → `Data/HUC_DL_Predictions_v2/DLpred_<mode>_cluster_<C>_huc_<H>.tif`
  (+ `_probs.tif`). Reuses `dl_06b_predict_huc.py` + `dl_huc_stack.py`; pull the ~7 per-HUC source tiles
  with `rsync_huc_sources.sh` first.

`dl_10_factorial_viz.ipynb` §1–§5 cover the base factorial + SHAP (now per mode); §6–§7 the first two
follow-ons; `dl_10b_huc_inference_viz.ipynb` §8 the HUC prevalence.

---

## 10. v2 code & file structure (design spec)

How the code and files reorganize for v2. **This is the spec to implement after the plan is confirmed —
no mechanism code is changed yet.**

### 10.1 `dl_experiment_config.py` (the source of truth) — edits
- **`LIDAR_TIERS`**: drop `"chm"`; keep `{"nolidar": [], "chmret": [CHM, pct_below_1m, pct_1m_to_5m,
  pct_above_5m]}`.
- **`CONFIGS`**: remove `fld_chm_leafon` / `fld_chm_leafoff`; add `nwiextra_chmret_leafoff` and
  `nwifield_chmret_leafoff` (label `nwi`/`nwifield`, `channels: 26`). Final = the 8 rows in §2's table.
- **New: a config → data-source registry.** Each config maps to `{patch_dir, pool_rule}`:
  `fld_*`→(`R_Patches`, anchored); `nwi`→(`R_Patches_NWI`, paired); `nwiextra`→(`R_Patches_NWIextra`,
  extra-pool); `nwifield`→(`R_Patches`+`R_Patches_NWIextra`, hybrid-union); `flddeg`→(`R_Patches`,
  degrade). Consumed by `dl_patch_pools.py` and the runner via `--emit`.
- **`stats_basename(name, mode=...)`** already exists — no change; callers must pass `mode`.
- **`verify_channel_matrix()`** must pass for **both** band-config modes (predictors are mode-invariant).
- `LABEL_SOURCE_ALIASES` becomes vestigial (band-based); replace its role with the directory registry.

### 10.2 Split/data layer
- **New `dl_patch_pools.py`**: `resolve_pools(config, seed, leakage_guard=LEAKAGE_GUARD) -> (train_files,
  val_files, test_files)` implementing Decisions 4.5–4.6 with the directory-aware guards (`field_key` /
  `nwi_field_twin` filename pairing + `huc12_of` HUC12 holdout). Single place the leakage guard lives;
  the preflight imports it so preflight and training agree by construction.
- **`dl_02_dataset.py`**: `create_data_splits` / `create_dataloaders` accept explicit file lists (from
  `resolve_pools`) instead of globbing one `patches_dir`; keep the current single-dir path as the
  `fld_*` fast path. `WetlandDataModule` gains a `mode` param that selects the stats file and
  `classification_mode`.

### 10.3 Stats
- Two masters: `multiclass_normalization_stats_wp0.5.json` (exists), `binary_normalization_stats_wp0.5.json`
  (build). `dl_make_config_stats.py --all --mode {multiclass,binary}` writes the 8 per-config files per
  mode into `STATS_DIR`. Class weights recomputed from the active label directory's pixel counts.

### 10.4 Shell
- `run_config.sh`: add `MODE`; make `STATS_DIR` an env knob (`STATS_DIR="${STATS_DIR:-$DATA/stats}"`);
  route `RESULTS_DIR/<mode>/<config>/seed<k>/`; resolve `patch_dir`/`pool_rule` from `--emit`.
- `run_factorial.sh`: outer loop over `MODES="multiclass binary"`, then configs, then seeds; finish one
  mode fully before the next (partial-reservation safety).
- Wrappers: delete `run_fld_chm_*.sh`; add `run_nwiextra_chmret_leafoff.sh`,
  `run_nwifield_chmret_leafoff.sh`.

### 10.5 Files on disk
- Patch dirs: `R_Patches/`, `R_Patches_NWI/`, `R_Patches_NWIextra/` (no more `R_Patches_Merged` for v2 —
  it was the v1 multi-band file). `R_Patches_Merged_v1` is the frozen v1 snapshot; leave it.
- Results: `Models/factorial_results_v2/<mode>/...` (fresh root ⇒ every cell runs; v1 untouched).
- Stats: keep `Data/Training_Data/stats/` (regenerated for v2) or `stats_v2/` once `STATS_DIR` is a knob.

---

## 11. Section-11 (v2 re-run ritual) — assessment

*Does following the rest of EXECUTION.md §11 work best for v2?* **Partly — keep the discipline, but v2
exceeds §11's "Flavor 1/2" and needs its own prep recipe.**

**Keep, as-is (already done or directly applicable):**
- **Step A (git tag v1)** and **Step B (snapshot gitignored artifacts)** — done, correct. They freeze
  v1's code/docs and physically preserve `factorial_results`, `stats`, the master, and
  `R_Patches_Merged_v1` so v1 SHAP/predict stay reproducible.
- **The footgun table (§11.1)** and the **`EXP_VERSION`-drives-every-root discipline (§11.3)** — still
  the right mental model. v2 sets `EXP_VERSION=v2` and derives `RESULTS_DIR` /`OUT_DIR` from it.

**Where v2 breaks §11's assumptions (so don't follow it literally):**
1. **§11.4 "Flavor 1" prep is merge-based** (`dl_merge_nwi_labels.py` → `R_Patches_Merged`). v2 abandons
   the single merged file for **separate directories**, so the merge step **does not apply**. Replace it
   with: build `R_Patches_NWI`/`R_Patches_NWIextra`, then per-directory + per-mode stats.
2. **v2 is bigger than Flavor 1 *or* 2 simultaneously** — new data layout **and** new configs
   (`nwiextra`, `nwifield`, drop `chm`) **and** a new **mode axis**. Call it **"Flavor 3."** In §11's
   terms it triggers the Flavor-2 code path (edit `dl_experiment_config.py` channels/configs in one
   commit before prep) *plus* new plumbing §11 never contemplated.
3. **The two "optional" §11.5 code gaps become mandatory**, and there are more:
   - `STATS_DIR` must be an env knob (needed to separate modes/versions cleanly) — §11.5 gap #1, now
     required.
   - `dl_10_factorial_viz.ipynb` result roots must be hoisted to one top cell — §11.5 gap #2, now
     required (it must also branch on `<mode>`).
   - **New gaps:** a `MODE` knob + mode token in `RESULTS_DIR`; the directory-based, field-anchored
     split (`dl_patch_pools.py`); and the preflight rewrite for cross-directory alignment + the leakage
     guard. None exist in §11.

**Recommended v2 recipe (a "Flavor 3" that supersedes §11.4 for this version):**
1. Keep Steps A/B (done). `export EXP_VERSION=v2`; derive `RESULTS_DIR=Models/factorial_results_v2`,
   `OUT_DIR=Data/HUC_DL_Predictions_v2`.
2. Land the §10 code edits **in one commit** (matrix, config→dir registry, `dl_patch_pools.py`, `MODE`
   + `STATS_DIR` knobs, preflight) so the preflight stays the guardrail — the Flavor-2 rule, applied to
   v2's larger change.
3. Prep: build the NWI directories → build both stats masters
   (`dl_01 ... --output <mode>_normalization_stats_wp0.5.json` per mode) →
   `dl_make_config_stats.py --all --mode {multiclass,binary}` → **preflight GREEN for all 8 × 2**.
4. `DRY_RUN=1 bash run_factorial.sh` confirms cells target `factorial_results_v2/<mode>/...` before GPU
   time; then the user runs it in `screen`.

**Bottom line:** §11's *snapshot-and-version discipline* is exactly right and already applied; §11's
*Flavor-1 merge prep* is the wrong recipe for v2's separate-directory + mode-axis change. This plan's
§10 + this section are the v2 replacement. Once the v2 code lands, fold this recipe back into
EXECUTION.md as a "Flavor 3" (EXECUTION.md itself is frozen under `factorial-v1`, so the v2 operational
guide should be a new doc or a §11.7 added post-implementation).

---

## 12. Open items before build (not blocking config setup)
- [x] **All three patch dirs verified on disk (2026-07-06, 689 each).** Uniform 18-band schema (17
      predictors + `MOD_CLASS`); channel matrix passes; field↔NWI paired 1:1; `NWIextra` = 689 new
      locations (0 field overlap, all 29 HUC12s ⊆ field). Superseded the location-key scheme with
      directory-aware keys (`field_key`/`nwi_field_twin`/`huc12_of`) after finding the `cluster_..._patch_N`
      substring collides 56× within `R_Patches` and spuriously "matches" 594 `NWIextra` patches.
- [x] **`nwiextra` = 2× pool DECIDED (2026-07-06):** trains on `R_Patches_NWI ∪ R_Patches_NWIextra`
      (1378), not `NWIextra` alone (689 = 1×). Config updated.
- [x] **Sub-decision 4.5 DECIDED (2026-07-06):** run both regimes, **HUC12 holdout is the headline**
      (`LEAKAGE_GUARD="huc12"`), `coord` as a sensitivity run; recorded in every manifest.
- [ ] Phase 3 analysis scope (mode comparison + label-gradient contrasts) — user to expand before build.
