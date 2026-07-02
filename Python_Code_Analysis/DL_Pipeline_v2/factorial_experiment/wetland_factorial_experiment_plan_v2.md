# Wetland DL Factorial Experiment — Claude Code Implementation Plan

**Purpose.** Implement and orchestrate a factorial experiment on the NYS Wetlands DL
pipeline (v2) to answer two coupled questions:

1. **Label provenance** — how much does training-label quality (NWI vs. field-verified)
   affect binary and multiclass wetland mapping accuracy and skill. Is any gap driven by *quantity* (fewer
   wetland pixels) or *correctness* (the wrong pixels)? Can we increase the NWI quantity to acheive the same
   mapping skill as the field-verified? Similary, does a hybrid NWI + field verified acheive improved mapping
   skill and accuracy?
2. **Below-canopy feature contribution** — which LiDAR-derived features and which
   seasonal imagery resolve the forested-wetland (FSW) vs. upland (UPL) confusion that
   above-canopy leaf-on spectral data cannot. Do LiDAR-derived features and seasonal imagery 
   improve binary classification wetland maps?

The unifying thesis: **LiDAR structure and leaf-off spectra are two below-canopy sensing
modalities that resolve the FSW/UPL confusion.** Clean field-verified labels make that
diagnosis trustworthy in a way NWI-trained studies cannot claim.

This document is the build brief. It is intentionally implementation-facing: it specifies
the experiment matrix, the pipeline mechanisms to add, the shell orchestration, and the
output layout. The three Section 4 decisions are now **settled** (4.1 multi-band labels,
4.2 NWI non-wetland→UPL, 4.3 repeated seeds R=3), so Phase 1 code is unblocked.

---

## 1. Existing pipeline (context for Claude Code)

The pipeline lives in `Python_Code_Analysis/DL_Pipeline_v2/`. Key facts that constrain
this work:

- **Bands are discovered at runtime** from GeoTIFF band descriptions — there are no
  hardcoded band indices. Band names drive everything via `dl_band_utils.py`.
- `Geomorph_local` is **one-hot expanded** 1 band → 10 channels. Channel counts must
  account for this.
- The stats JSON (from `dl_01_compute_statistics.py`) is the single source of truth for
  `in_channels`, per-band normalization, `predictor_names`, `class_names`, `weight_power`,
  and class weights. **Filenames are mode- and weight-power-specific** —
  `dl_band_utils.default_stats_path()` / `stats_filename()` resolve to
  `<mode>_normalization_stats[_wp<power>].json`. The current production multiclass file is
  **`multiclass_normalization_stats_wp0.5.json`** (sqrt-inverse class weighting). There is no
  bare `normalization_stats.json` anymore — every reference below uses the mode-prefixed,
  weight-power-suffixed name.
- **Class weights are power-scaled.** `dl_01` takes `--weight-power P`; weights are
  `(1/freq)**P`. Production uses **`--weight-power 0.5`** (sqrt-inverse, gentler on minority
  over-prediction: `{EMW:3.09, FSW:2.16, SSW:3.38, UPL:1.0}`), not pure inverse-freq
  (`P=1.0`, the legacy default). Hold `--weight-power 0.5` fixed across every run so weighting
  is not a hidden variable, and let the `_wp0.5` suffix appear on every stats file.
- Training is `dl_04_train_lightning.py`; evaluation `dl_05_evaluate.py`. Both derive
  `num_classes` from `len(stats["class_names"])`.
- Loss is configurable; **plain weighted CE** has been the best performer
  (`--ce-weight 1.0 --dice-weight 0.0 --focal-gamma 0`). Use it as the fixed loss for
  every run so loss is not a hidden variable.
- Architecture is fixed for this experiment: **U-Net** (`--arch unet`), HPC settings
  (`--base-filters 64 --depth 5`), unless a pilot run shows memory issues.

The experiment changes **inputs and labels only**, holding architecture, loss, optimizer,
schedule, and splits constant across every run.

---

## 2. Experiment matrix (8 configurations)

### Constant base (present in every run)
Terrain + leaf-on NAIP: `DEM`, `slope_local`, `Geomorph_local`, `flowacc`, `twi`,
`r`, `g`, `b`, `nir`. (= 18 channels: 8 single-channel bands + 10 from one-hot Geomorph.)

### Two experimental axes (field-verified labels only)
- **LiDAR tier (3 levels):**
  - `nolidar` — base only
  - `chm` — base + `CHM` (widely available, cheap)
  - `chmret` — base + `CHM` + `pct_below_1m` + `pct_1m_to_5m` + `pct_above_5m`
- **Leaf-off (2 levels):**
  - `leafon` — base only
  - `leafoff` — base + `r_lo`, `g_lo`, `b_lo`, `nir_lo`

### Factorial cells (3 × 2, field-verified labels)

| LiDAR ↓ \ Spectral → | leaf-on | leaf-on + leaf-off |
|---|---|---|
| **no LiDAR** | `fld_nolidar_leafon` | `fld_nolidar_leafoff` |
| **CHM only** | `fld_chm_leafon` | `fld_chm_leafoff` |
| **CHM + returns** | `fld_chmret_leafon` | `fld_chmret_leafoff` *(= full feature set)* |

### Label block (full feature set only — scope control)

| Config | Labels | Notes |
|---|---|---|
| `nwi_chmret_leafoff` | NWI | Operational stale-label baseline |
| `flddeg_chmret_leafoff` | Field, degraded to NWI wetland prevalence | Quantity-vs-correctness control |
| `nwiextra_chmret_leafoff` | NWI | Increased to Field wetland prevalence| 
| `nwifield_chmret_leafoff` | NWI & Field | NWI and Field (not overlapping) combined |
| `fld_chmret_leafoff` | Field (shared from factorial) | Gold-standard baseline |

> **Scope-control rule (do not violate):** the label comparison runs **only** on the full
> feature set; feature ablations run **only** on field-verified labels. NWI never enters
> the factorial. This keeps the matrix at 8 configs instead of 24.

### Per-config band composition and channel counts

| # | Config |  LiDAR bands added  | Leaf-off added | `in_channels` |
|---|---|---|---|---|
| 1 | `fld_nolidar_leafon`        | —              | —    | 18 |
| 2 | `fld_nolidar_leafoff`       | —              | 4    | 22 |
| 3 | `fld_chm_leafon`            | CHM            | —    | 19 |
| 4 | `fld_chm_leafoff`           | CHM            | 4    | 23 |
| 5 | `fld_chmret_leafon`         | CHM + 3 returns| —    | 22 |
| 6 | `fld_chmret_leafoff`        | CHM + 3 returns| 4    | 26 |
| 7 | `nwi_chmret_leafoff`        | CHM + 3 returns| 4    | 26 |
| 8 | `nwiextra_chmret_leafoff`   | CHM + 3 returns| 4    | 26 |
| 9 | `nwifield_chmret_leafoff`   | CHM + 3 returns| 4    | 26 |
| 10 | `flddeg_chmret_leafoff`     | CHM + 3 returns| 4    | 26 |

> Config 6 = 26 channels, matching the pipeline's documented full-feature count (17
> predictors → 26 channels after `Geomorph_local` one-hot 1→10) — use this as a sanity check
> that band selection + one-hot expansion are wired correctly. The existing
> `multiclass_normalization_stats_wp0.5.json` already reports `in_channels: 26` for exactly
> this band set, so config 6 should reproduce it byte-for-byte on the predictor side.

### Run counts
- **R = number of replicates** (folds or seeds; see Decision 4.3).
- R = 3 → **24 runs**; R = 5 → **40 runs**.

---

## 3. Shared design constants (apply to all runs)

- **Test set is always field-verified.** Both label sources are judged against the same
  gold standard. This is non-negotiable — it fixes the evaluation confound.
- **Validation follows the training label source.** NWI run early-stops on NWI validation
  labels; field runs validate on field labels. Test stays field for all.
- **Identical splits across configs** for a given replicate seed/fold, so differences are
  attributable to inputs/labels, not to which patches landed where.
- **Fixed loss** (plain weighted CE), **fixed class-weight power** (`--weight-power 0.5`,
  sqrt-inverse), and **fixed architecture** across all runs. Note: class weights still differ
  per run because they are recomputed from the *active label source's* pixel counts (field vs
  NWI vs flddeg) at the same power — that difference is intended, the power is not.
- **Metrics:** per-class IoU, per-class recall/precision, macro-F1, and the **full
  confusion matrix** (needed for the UPL↔FSW cells). Overall accuracy is reported but
  never used as the headline (it tracks UPL prevalence).
- **Forest-restricted metrics:** additionally compute IoU/recall on forested pixels only
  (or an FSW/UPL boundary buffer) — this is where the below-canopy effect concentrates and
  a global metric dilutes it.

---

## 4. Decisions that block Phase 1

These must be settled before mechanism code is written. Each changes the implementation.

### 4.1 Label storage — **DECIDED: multi-band labels in each patch**
Each patch gains label bands `MOD_CLASS_FLD`, `MOD_CLASS_NWI`, `MOD_CLASS_FLDDEG` alongside
the 17 predictors — one file, one grid, footprint alignment provable by construction. The
current single `MOD_CLASS` band **is** the field label, so `MOD_CLASS_FLD` is a copy/rename
of it; `MOD_CLASS_NWI` comes from Phase 0 NWI rasterization (non-wetland→UPL per 4.2);
`MOD_CLASS_FLDDEG` is written by `dl_degrade_labels.py` (Phase 1.3) from the field band. The
Phase 1.2 toggle selects the active label band **by name**, exactly like predictor discovery.
*Rejected:* separate directories per label source (alignment would have to be re-proven every
run). The preflight (Phase 0) still verifies all label bands share the field band's grid and
255 mask.

### 4.2 NWI "non-wetland" semantics — **DECIDED: confirmed UPL**
A pixel with no NWI wetland polygon is labeled **confirmed UPL (class 3)**, not
`ignore_index`. NWI omission errors therefore enter training as realistic FSW/SSW/EMW→UPL
label noise — the operational stale-label case the experiment is meant to measure, and the
condition under which `flddeg` (degrade field wetland prevalence down to NWI's) is the
clean quantity-vs-correctness control. `ignore_index=255` is reserved only for genuinely
unlabeled/out-of-footprint pixels, and must be the *same* ignore mask across the field and
NWI label bands so the comparison stays pixel-aligned.

This is baked into:
- **Label generation** (Phase 0): rasterize NWI with non-wetland → 3 (UPL); only true
  no-data → 255.
- **The label-source toggle** (Phase 1.2): no per-source ignore remap — `fld`, `nwi`, and
  `flddeg` all use `ignore_index=255` for no-data only.
- **The preflight check** (Phase 0): assert the NWI band's value set is `{0,1,2,3,255}` and
  that its 255 mask is identical to the field band's.

> *Robustness check (optional, later):* an `ignore_index`-for-NWI-omissions variant can be
> run as a sensitivity analysis, but the **confirmed-UPL** version is the headline.

### 4.3 Replication strategy — **DECIDED: repeated fixed-split seeds, start R=3**
The outer loop is `for seed in 0,1,2` over a fixed 70/15/15 split: 8 configs × 3 seeds =
**24 runs** (≈ one A6000 reservation, ~8–24 GPU-h). The **same seed yields the same
train/val/test partition across all 8 configs** (via `create_data_splits(seed)`), so
differences are attributable to inputs/labels, not split luck; the test partition is always
field-labeled. Extend to R=5 (seeds 3–4, 40 runs) only if cross-seed variance warrants. The
runner is idempotent (§2.2a), so extending R just adds new `seed<k>/` cells. 5-fold CV was
considered but deferred to keep the first pass inside one reservation.

---

## 5. Implementation phases

### Phase 0 — Data prerequisites & preflight checks (verify before coding)

The label-provenance axis only means something if **NWI and field labels are judged on the
exact same pixels** — same patch list, same footprints, same predictors, differing *only* in
the label band. The most likely silent failure in this whole experiment is a footprint or
patch-set mismatch that turns a label comparison into an apples-to-oranges artifact. Build a
single **preflight script** (`dl_preflight_check.py`, a Phase-0 deliverable) that hard-fails
before any GPU time is spent. It must assert:

- [ ] **Same patch set.** Every patch filename that exists for one label source exists for
      all of them (no NWI-only or field-only patches). Report and fail on set differences.
- [ ] **Identical footprints per patch.** For each patch, the field and NWI label rasters
      share CRS, transform, width, height, and nodata — i.e. the same grid, pixel-for-pixel
      (per Decision 4.1; trivially satisfied if labels are bands in one file, but *verify*).
- [ ] **Predictor parity.** All 17 predictor bands present and named exactly as the pipeline
      expects, identical across label sources. Authoritative set = `predictor_names` in
      `multiclass_normalization_stats_wp0.5.json`: `DEM, slope_local, Geomorph_local, flowacc,
      twi, CHM, r, g, b, nir, r_lo, g_lo, b_lo, nir_lo, pct_below_1m, pct_1m_to_5m,
      pct_above_5m` (returns use the `_1m`/`_5m` names, not the older `_0.5m`/`_2m` names still
      in CLAUDE.md). Confirmed present in the current 491-patch set on 2026-06-21 — **except
      the NWI label band, which does not yet exist** (see below).
- [ ] **Label-value sanity.** Each label band contains only `{0,1,2,3,255}` (EMW/FSW/SSW/UPL/
      ignore); flag stray values and report per-source class prevalence so the `flddeg`
      degradation target (NWI wetland prevalence) is measured, not assumed.
- [ ] **Split alignment.** For a given seed/fold, the train/val/test patch partition is
      *identical* across all 8 configs (same `create_data_splits` seed → same file lists).
      The test partition must be field-labeled for every config (Section 3, non-negotiable).
- [ ] **Channel sanity.** `compute_in_channels` for `fld_chmret_leafoff` resolves to **26**,
      matching the master stats file byte-for-byte on the predictor side.

> **Current status (2026-06-21):** the 491-patch set carries all 17 predictors *including the
> 4 leaf-off bands* and a single `MOD_CLASS` (field) label — so the feature-ablation axis
> (configs 1–6) is data-ready, but **no NWI label band exists yet.** Generating NWI labels
> over the identical footprints (Decision 4.1 + 4.2) is the one hard data prerequisite, and it
> gates configs 7–8 only. `dl_preflight_check.py` should run clean on the field-only configs
> today and be re-run the moment NWI labels are added.

- [ ] Generate NWI labels with the **decided 4.2 semantics — non-wetland → UPL (3)**, no-data
      → 255, ignore mask identical to the field band; then re-run the preflight check until it
      is green for all label sources.

### Phase 1 — Pipeline mechanism changes (Python, built once)

**1.1 Band selection.** Add a config-driven active-predictor mechanism so a run trains on
a *subset* of discovered bands. Two clean options — pick one:
- a `"active_predictors": [...]` field in a per-config JSON, **or**
- an `--include-bands` CLI arg on `dl_01`/`dl_02`/`dl_04`.
Must correctly recompute `in_channels` with one-hot expansion (`compute_in_channels` in
`dl_band_utils.py`). Add a unit check: `fld_chmret_leafoff` must resolve to 26 channels.

**1.2 Label-source toggle.** Make the target label band selectable (`fld` / `nwi` /
`flddeg`), extending the existing `label_band` config entry rather than replacing it. Per the
**decided 4.2 rule**, all three sources share one ignore convention: `ignore_index=255` for
no-data only — there is **no** per-source omission→ignore remap (NWI non-wetland is already a
hard UPL label from Phase 0). So the toggle only swaps which band is read; the loss mask is
identical across sources.

**1.3 Degradation utility.** Seeded routine that randomly remaps field *wetland* pixels →
UPL until wetland prevalence matches NWI's measured prevalence, producing `flddeg`. Store
the seed and the achieved prevalence in the run manifest. This is the control that
separates quantity from correctness.

**1.4 Per-config stats.** Avoid 8 full rescans: compute master stats once over **all**
bands at `--weight-power 0.5`, then a helper subsets the normalization dict to the active
predictors and recomputes `in_channels`, writing a per-config stats file. Follow the
existing mode-prefixed / wp-suffixed convention so downstream auto-resolution still works —
e.g. `multiclass_normalization_stats_<config>_wp0.5.json` (written via `dl_01`'s `--output`
or the helper). Class weights are recomputed from the *active label source* (field vs NWI vs
flddeg differ) at the **same** `weight_power=0.5`, and the file records `weight_power` so the
power is auditable.

> **Master file already exists.** The current production file
> `multiclass_normalization_stats_wp0.5.json` is computed over all 17 predictors (26 channels)
> at `wp0.5`, so it already *is* the master and doubles as config 6's
> (`fld_chmret_leafoff`) stats. The helper only needs to derive the 7 reduced-band configs
> from it; no rescan is required for the full set.

### Phase 2 — Shell orchestration (the per-config scripts)

**2.1 Shared runner** — `run_config.sh` (function or script) taking:
`CONFIG_NAME`, `ACTIVE_BANDS`, `LABEL_SOURCE`, `SEED/FOLD`. It:
1. loads/generates the per-config stats file
   (`multiclass_normalization_stats_<config>_wp0.5.json`, subset from the master at
   `--weight-power 0.5`) and passes it via `--stats-path`,
2. trains with fixed loss + architecture (`--ce-weight 1.0 --dice-weight 0.0 --focal-gamma 0`,
   `--arch unet --base-filters 64 --depth 5`),
3. evaluates on the held-out **field** test set,
4. writes everything to `results/<config>/seed<k>/`.

**2.2 Eight wrapper scripts** — one per config (e.g. `run_fld_chmret_leafoff.sh`), each
setting its band list + label source and looping over R replicates. One script ⇄ one
configuration ⇄ one output folder, as specified. Wrappers call the shared runner so logic
isn't duplicated.

**2.2a Resumability (required).** A single RTX A6000 forces all runs to serialize, and they
will not fit one BioHPC reservation window (see Section 8). The runner must be
**idempotent**: at entry it checks for a completed `results/<config>/seed<k>/metrics.json`
(+ `manifest.json`) and **skips** that cell, so the whole factorial can be stopped at the end
of a reservation and resumed in the next without recomputing. A top-level
`run_factorial.sh` walks all (config × seed) cells in a fixed order and lets the
skip-completed guard no-op the finished ones.

**2.3 Run manifest per run** — write a `manifest.json` recording: active bands, resolved
`in_channels`, label source, ignore-index rule, seed/fold, loss params, **`weight_power`
(0.5) and the resolved class weights**, the stats filename used, architecture, git commit
hash, and (for flddeg) the degradation seed + achieved prevalence. Makes every result
self-describing and reproducible.

### Phase 3 — Aggregation & SHAP *(to be expanded — placeholder)*

> The user has additional steps to add here. Draft scope only:
- Aggregate `results/<config>/seed<k>/metrics.json` across configs and replicates into the
  factorial table (mean ± sd, paired across folds).
- Compute the headline contrasts: LiDAR tiers (nolidar→chm→chmret), leaf-off main effect at
  each tier, the LiDAR×leaf-off interaction, and the label gradient (nwi vs flddeg vs fld).
- SHAP pass (`dl_07_shap_analysis.py`) on the field-trained models to pair ablation
  (marginal contribution) against reliance (feature importance), with one-hot bands
  aggregated back to band level. **Run SHAP on the GPU node** (it backprops through the
  model); a CPU node is slow for this. Aggregation itself is pure pandas and runs fine on the
  CPU node after sync-back.
- **Hold for user's additional steps before building.**

---

## 6. Output directory layout

```
results/
├── fld_nolidar_leafon/
│   ├── seed0/  (or fold0/)
│   │   ├── manifest.json
│   │   ├── metrics.json            # per-class IoU/recall/precision, macro-F1
│   │   ├── confusion_matrix.csv    # full CM incl. UPL↔FSW cells
│   │   ├── metrics_forest.json     # forest-restricted metrics
│   │   ├── best.ckpt (+ .safetensors / .meta.json)
│   │   └── train_log.csv
│   ├── seed1/ ...
│   └── seed2/ ...
├── fld_nolidar_leafoff/ ...
├── fld_chm_leafon/ ...
├── fld_chm_leafoff/ ...
├── fld_chmret_leafon/ ...
├── fld_chmret_leafoff/ ...
├── nwi_chmret_leafoff/ ...
├── flddeg_chmret_leafoff/ ...
└── stats/
    ├── multiclass_normalization_stats_fld_nolidar_leafon_wp0.5.json ...
    └── multiclass_normalization_stats_wp0.5.json   # master (= config 6 / full feature set)
```

> Stats filenames keep the pipeline's `<mode>_normalization_stats[_<config>][_wp<power>].json`
> convention (mode = `multiclass`, power = `0.5`). The master is the existing production file;
> per-config files insert the config name before the `_wp0.5` suffix.

Folder name = config name = the row label in the final factorial table, so aggregation in
Phase 3 is a directory walk.

---

## 7. Summary of what Claude Code builds

| Phase | Deliverable | Touches |
|---|---|---|
| 0 | Preflight check (same patch set, footprints, predictor parity, label values, split alignment, 26-ch sanity) | new `dl_preflight_check.py` |
| 1.1 | Band-subset selection + channel recompute | `dl_02_dataset.py`, `dl_band_utils.py`, `dl_band_config.json` |
| 1.2 | Label-source toggle | `dl_01`, `dl_02`, config |
| 1.3 | Seeded degradation utility | new `dl_degrade_labels.py` |
| 1.4 | Per-config stats subsetting (mode-prefixed, `_wp0.5`-suffixed; master already exists) | `dl_01` / new helper, `dl_band_utils.py` (`stats_filename`) |
| 2.1 | Shared runner (idempotent / skip-completed) | `Shell_Scripts/run_config.sh` |
| 2.2 | 8 config wrappers + `run_factorial.sh` driver | `Shell_Scripts/run_<config>.sh` |
| 2.3 | Run manifests | runner |
| 2.4 | Results sync-back (GPU `/workdir` → CPU node) | `Shell_Scripts/rsync_results.sh` *(drafted)* |
| 3 | Aggregation (`dl_08_aggregate_factorial.py`, CPU) + SHAP (`dl_09_shap_factorial.py`, GPU-side) | built; forest-restricted metrics deferred; awaiting user's added analyses |

**First action for Claude Code:** settle Section 4 decisions, build `dl_preflight_check.py`
and run it on the field-only configs, then implement Phase 1.1 and verify the 26-channel
sanity check before anything else.

---

## 9. Follow-on studies (added 2026-06-25)

Three studies that reuse the factorial machinery and the **best** trained config (a runtime
parameter, chosen from `factorial_table.csv` — likely `fld_chmret_leafoff`). They do **not**
change the 8×3 base factorial. All hold the base factorial's constants (loss, weight-power,
splits, seeds 0/1/2) unless noted. The shared runner `run_config.sh` gained backward-compatible
env knobs (`ARCH`, `N_PATCHES`, `CELL_NAME`, `CAT_CHANNELS`, `DEEP_SUPERVISION`) that default to
base-factorial behavior, so existing cells are unchanged.

### Phase 4 — Patch-count learning curve

**Question:** how many patches do we need, and what is the accuracy-vs-data curve?

- **Mechanism:** new `--n-patches` flag on `dl_04_train_lightning.py`, threaded through
  `WetlandDataModule` → `create_dataloaders` → `create_data_splits` (`dl_02_dataset.py`). It caps
  the **seed-shuffled** pool *before* the 70/15/15 split, so subsets are reproducible and nested
  per seed. Ignored in k-fold mode.
- **Levels:** {100, 200, 300, 400, 500, full}; full = no cap (re-run later to extend the curve as
  patches are added). R=3 seeds → 18 cells.
- **Driver:** `Shell_Scripts/run_patchcurve.sh <config>` → `results_patchcurve/<config>_n<level>/seed<k>/`.
- **x-axis is the realized train size** (`training_log.json` → `data_split.train`), not the cap.
- **Aggregate:** `dl_08b_aggregate_patchcurve.py --results-dir results_patchcurve` →
  `analysis/patchcurve_long.csv`, `patchcurve_summary.csv`, `patchcurve.png`.

### Phase 5 — UNet3+ architecture comparison

**Question:** how much does architecture (vs U-Net) move accuracy?

- **No model code needed** — UNet3+ already exists (`dl_03_unet3plus_model.py`,
  `dl_model_factory.build_net()`), the trainer accepts `--arch unet3plus --cat-channels
  --deep-supervision`, and eval auto-detects arch from the checkpoint.
- **Fair comparison:** same capacity as the U-Net baseline (`bf=64`, `depth=5`); only the
  architecture changes. **Deep supervision ON by default** (part of the UNet3+ definition here,
  not a tested axis). Memory: `16-mixed` + reduced batch (8; drop to 4 on OOM).
- **Driver:** `Shell_Scripts/run_arch_compare.sh <config>` → `results_arch/<config>_unet3plus/seed<k>/`.
  The U-Net arm already exists in `results/<config>/` on the same seeds → paired comparison.
- **Aggregate:** `dl_08b_aggregate_patchcurve.py --arch-compare --config <config> --unet-dir results
  --unet3plus-dir results_arch` → `analysis/arch_compare.csv`.

### Phase 6 — Prediction / inference maps

**Question:** what do the wall-to-wall predictions look like (class + softmax probability)?

- **Reuses the existing in-memory path** — `dl_06b_predict_huc.py` + `dl_huc_stack.py` assemble the
  17-band / 26-channel predictor stack window-by-window from the canonical per-HUC source rasters
  (no `*_stack.tif` saved). The config's `TRAIN_STATS` file *is* the band/channel contract; bands
  match by name, so any config's predictors (all ⊆ the 17 available) work.
- **Data transfer:** the source rasters live in `NYS_Wetlands_Data/`. Pull only the **7 per-HUC
  tiles** (~4–5 GB/HUC) with the existing `Shell_Scripts/rsync_huc_sources.sh <cluster> <huc>` into
  a `--data-root` tree. Start with a few demo HUCs (full source tree ≈ 1.74 TB).
- **Verify first:** `python dl_huc_stack.py --huc <H> --cluster <C> --data-root <root> --inspect`.
- **Predict:** `Shell_Scripts/run_predict_factorial.sh <config> <cluster> <huc> [seed]` resolves the
  best-macro-F1 checkpoint (or the named seed) + the config's stats and runs `dl_06b_predict_huc.py
  --probs` → `DLpred_<mode>_cluster_<C>_huc_<H>.tif` (class) + `..._probs.tif` (per-class softmax).
  Works unchanged for a UNet3+ checkpoint (arch auto-detected).

### Deliverables (Phases 4–6)

| Phase | Deliverable | Touches |
|---|---|---|
| 4 | `--n-patches` knob | `dl_02_dataset.py`, `dl_04_train_lightning.py` |
| 4 | Patch-curve driver + aggregation/plot | new `Shell_Scripts/run_patchcurve.sh`, new `dl_08b_aggregate_patchcurve.py` |
| 5 | UNet3+ comparison driver | new `Shell_Scripts/run_arch_compare.sh` (+ `--arch-compare` in `dl_08b`) |
| 4/5 | Runner env knobs (`ARCH`/`N_PATCHES`/`CELL_NAME`/`CAT_CHANNELS`/`DEEP_SUPERVISION`) + manifest fields | `Shell_Scripts/run_config.sh` |
| 6 | Best-checkpoint prediction wrapper | new `Shell_Scripts/run_predict_factorial.sh` (reuses `rsync_huc_sources.sh`, `dl_06b_predict_huc.py`, `dl_huc_stack.py`) |

---

## 8. Feasibility & HPC execution

Assessment against the target setup: **non-GPU node now** for orchestration/analysis, with a
reserved **BioHPC GPU node** (`cbsugpu09`/`cbsugpu10`: Rocky 9.8, 256 cores, 512 GB RAM, 7 TB
NVMe `/workdir`, **1× NVIDIA RTX A6000, 48 GB VRAM**) for training via `docker1`. Verdict:
**the experiment is feasible, and ~75% of it (configs 1–6) is data-ready today.** The binding
constraints are data generation and single-GPU serialization, not raw compute.

### 8.1 Data readiness — the real gate
- **Feature-ablation axis (configs 1–6, field labels): ready now.** The 491-patch set already
  carries all 17 predictors including the 4 leaf-off bands. Needs only Phase 1.1 + 1.4.
- **Label-provenance block (configs 7–8): blocked on NWI labels.** Patches hold only the field
  `MOD_CLASS`. NWI must be rasterized over the identical footprints (Decisions 4.1/4.2) before
  `dl_preflight_check.py` can go green for those two cells. `flddeg` is then derivable in code
  (Phase 1.3). **Track NWI-label generation as its own gating task.**

### 8.2 Compute fits the A6000 comfortably
- **Memory:** ~125M-param U-Net (bf64/d5) at 256², batch 16, 26 channels fits 48 GB in fp32;
  trivial with `--precision 16-mixed`. No memory risk.
- **Per run:** ~344 train patches → ~22 steps/epoch; with early stopping ≈ **20–60 min/run**.
- **Totals (single GPU, sequential):** R=3 → 24 runs ≈ **8–24 GPU-h**; R=5 → 40 runs ≈
  **13–40 GPU-h**.
- **Disk:** 40 runs × (~500 MB `.ckpt` + ~500 MB `.safetensors` + logs) ≈ **~40 GB** of 7 TB.

### 8.3 Single GPU + reservation windows = the binding execution constraint
One A6000 means **no cross-run parallelism — runs serialize**, and 24–40 runs will not fit a
single reservation. Mitigations (now baked into the plan):
- **Idempotent runner** (§2.2a): skip any cell with a completed `metrics.json`, so the
  factorial survives stop/resume across reservation windows.
- **Pragmatic replication:** start at **R = 3** (24 runs, roughly one reservation's worth);
  extend to R = 5 only if variance across folds warrants it (Decision 4.3).

### 8.4 Docker / HPC mechanics (per the READMEs **and `AGENTS.md`**)
- **`docker1`, never `docker`** — it's the site wrapper that supplies the required privileges.
- **Mounts must live under `/workdir/$USER`** (hard BioHPC rule from `AGENTS.md`). The repo's
  persistent home is `/ibstorage/anthony/NYS_Wetlands_DL`, which **cannot be bind-mounted into
  the container.** So the GPU-node workflow is: `rsync` the repo + `Data/` to
  `/workdir/$USER/nys_wetlands/`, then
  `docker1 run --gpus all --shm-size=8g --user $(id -u):$(id -g) -v /workdir/$USER/nys_wetlands:/app ...`.
  Host edits to the orchestration scripts reach the container because `/workdir/$USER/...` is
  the mounted tree — keep `/ibstorage` as the source of truth and push to `/workdir` per run.
- Drive the factorial via an **interactive shell or a CMD override** (`run_factorial.sh`), not
  the image's default single-pipeline `CMD`.
- **Long-running execution uses `screen`/`tmux`.** The factorial is 8–40 GPU-h and will
  outlive an SSH session; start `run_factorial.sh` inside `screen` so a disconnect doesn't kill
  it (`AGENTS.md` Long-Running Jobs).
- **Scratch/temp under `/workdir/$USER/tmp`, not `/tmp`** (`/tmp` is small and shared).
- TensorBoard/CSV logs under `Models/`; monitor via the BioHPC port range (8009–8039) or an
  SSH tunnel as the README describes.

> **Agent boundary (`AGENTS.md` "AI Agent behavior"):** Claude Code **prepares** scripts; the
> **user executes** them. I will not auto-launch training, Docker containers, or any
> long-running compute here — every heavy job is delivered as a reviewed script for you to run
> in your own `screen` session. This is exactly the plan's build/run split, now a hard rule.

### 8.5 Sync-back & CPU-node analysis
- **Transfer:** `results/<config>/seed<k>/` is rsync-friendly; `Data/` and `Models/` are
  gitignored, so sync is rsync/scp, never git. **Deliverable: `rsync_results.sh`** (drafted
  alongside this plan), modeled on `rsync_huc_sources.sh`, pulling the GPU node's
  `/workdir/.../results/` back to CPU-node storage. Supports `--dry-run` and per-config
  filtering.
- **Analysis split:** Phase 3 aggregation is pure pandas → runs on the **CPU node** after
  sync-back. **SHAP runs on the GPU node** (it backprops) and its JSON/PNGs sync back with the
  rest. Run SHAP before tearing down the reservation.

### 8.6 Feasibility checklist
- [ ] NWI labels generated over identical footprints, **non-wetland→UPL (decided 4.2)** →
      preflight green for all sources (gates 7–8)
- [ ] Repo + `Data/` rsynced to `/workdir/$USER/nys_wetlands`; Docker image built/loaded
      (`docker1`); mounts only under `/workdir/$USER`
- [ ] `run_factorial.sh` launched inside `screen`/`tmux`; resumable (skip-completed) across
      reservation windows
- [ ] R chosen (start R=3); identical splits verified across configs by the preflight
- [ ] `rsync_results.sh` round-trips a dry-run before the first real sync
- [ ] SHAP scheduled GPU-side; aggregation runs CPU-side post-sync
- [ ] Heavy jobs are run by the **user** from prepared scripts (agent does not auto-execute)
