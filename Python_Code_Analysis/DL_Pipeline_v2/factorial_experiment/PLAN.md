# Wetland DL Factorial Experiment — Design of Record (v3)

The **single design document** for the factorial experiment. It consolidates what
used to live in three files — `wetland_factorial_experiment_plan.md` (v1),
`wetland_factorial_experiment_plan_v2.md` (v2), and `arch_fusion/PLAN.md` (the
multi-branch fusion arm). Those are preserved verbatim under `archive/`; nothing
here supersedes them as history, but **this file is what to read and edit.**

The operational counterpart is [`EXECUTION.md`](EXECUTION.md) — how to actually run
it. Design and rationale live here; commands live there.

> **Status (2026-08-25).** v3 code is **built and CPU-tested; not yet run.**
> `dl_experiment_config.py` carries the v3 matrix (21/25/29 channels), all three
> architecture drivers exist and default to `SEEDS="0 1 2 3 4"`, and the fusion
> model is implemented with a preflight gate on its branch map. What remains is
> CPU prep (the master stats are stale — see §11) and the GPU runs.

> **Agent boundary (`AGENTS.md`).** Claude *prepares* these scripts; **the user
> runs** all GPU/long jobs. Nothing in this workstream auto-launches training,
> containers, or rsync.

---

## 1. Research questions and thesis

1. **Label provenance** — how much does training-label quality (NWI vs.
   field-verified) affect binary and multiclass wetland mapping accuracy and
   skill? Is any gap driven by *quantity* (fewer wetland pixels) or *correctness*
   (the wrong pixels)? Can adding NWI *quantity* (`nwiextra`) reach field-level
   skill? Does a hybrid NWI + field set (`nwifield`) improve on either?
2. **Below-canopy feature contribution** — which LiDAR-derived features and which
   seasonal imagery resolve wetland classification into Forested (FSW),
   Shrub-Scrub (SSW), and Emergent (EMW)?
3. **Binary vs. multiclass** — repeating the factorial with labels collapsed to
   wetland-vs-upland, do we see different mapping skill and different use of
   predictor features?
4. **Architecture (new in v3)** — does an encoder that keeps input modalities
   separate and fuses them with a learned per-pixel gate resolve the UPL↔FSW
   confusion better than a plain U-Net or UNet3+? And do the gate weights
   themselves say *where* and *at what scale* each modality is being relied on?

**Thesis:** *LiDAR structure and leaf-off spectra are two below-canopy sensing
modalities that resolve wetland vs. upland in a binary model and wetland-class
confusion in a multiclass model.* Clean field-verified labels make that diagnosis
trustworthy in a way NWI-trained studies cannot claim.

---

## 2. Version history at a glance

| Aspect | v1 (frozen, tag `factorial-v1`) | v2 (frozen, tag `factorial-v2`) | **v3 (this document)** |
|---|---|---|---|
| Classification | multiclass only | multiclass **+ binary** (full axis) | unchanged from v2 |
| LiDAR axis | 3 tiers (`nolidar`/`chm`/`chmret`) | **2 tiers** (`nolidar`/`chmret`) | unchanged from v2 |
| Configs | 8 (incl. `fld_chm_*`) | 8 (`fld_chm_*` dropped; `nwiextra`, `nwifield` added) | unchanged from v2 |
| Predictor bands | 17 | 17 | **20** (+`TPI_local`, `meanc_local`, `dmv_local`) |
| `in_channels` | 18/22/26 | 18/22/26 | **21/25/29** |
| Label storage | multi-band labels in `R_Patches_Merged` | **one directory per source** | unchanged from v2 |
| Split | held-out split on the merged dir | **field-anchored** by location key | unchanged from v2 |
| Seeds (R) | 3 | 5 (as run) | **5, by default in every driver** |
| Architecture arms | U-Net + UNet3+ (follow-on) | U-Net + UNet3+ | **three: U-Net, UNet3+, `mbfusion`** |
| Patch-count curve | yes | yes | **dropped** (§9.2) |
| Results root | `Models/factorial_results/` | `Models/factorial_results_v2/<mode>/` | `Models/factorial_results_v3/<mode>/` |

**What v3 changes, in one paragraph.** Three terrain metrics were added upstream in
`NYS_Wetlands_Data/` (`step_terrain.sh` / `terrain_metrics_filter_singleVect_CMD.R`),
taking each patch from 18 bands to **21** (20 predictors + `MOD_CLASS`) and the
full feature set from 26 to **29 channels**. Because `in_channels` changed, **no v2
checkpoint is compatible and every arm retrains from scratch** — which is what makes
this a version bump rather than a top-up. v3 also raises the replicate count to
**R=5 across the whole grid** (v2 ran R=5 in practice but the drivers defaulted to 3),
adds a **third architecture arm**, and **drops the patch-count learning curve**.

---

## 3. Experiment matrix

### 3.1 Config-name grammar `<label>_<lidar>_<spectral>`

| Slot | Values | Meaning |
|---|---|---|
| `<label>` | `fld` / `nwi` / `nwiextra` / `nwifield` / `flddeg` | training **label source** |
| `<lidar>` | `nolidar` / `chmret` | LiDAR tier: none / CHM + return fractions |
| `<spectral>` | `leafon` / `leafoff` | leaf-on NAIP only / + leaf-off NAIP RGB+NIR |

### 3.2 Constant base (present in every config)

Terrain + leaf-on NAIP, **12 band names → 21 channels** (11 single-channel bands +
10 from one-hot `Geomorph_local`):

```
DEM  slope_local  TPI_local  Geomorph_local  meanc_local  dmv_local  flowacc  twi
r  g  b  nir
```

### 3.3 The two feature axes (field-verified labels only)

- **LiDAR tier:** `nolidar` (base only) · `chmret` (+ `CHM`, `pct_below_1m`,
  `pct_1m_to_5m`, `pct_above_5m`). CHM is grouped *with* the return fractions, so
  the axis contrasts *no structure* vs. *full structure* — not CHM-alone vs.
  CHM+returns. (This was the v1→v2 change; the `chm` tier is gone.)
- **Leaf-off:** `leafon` (base only) · `leafoff` (+ `r_lo`, `g_lo`, `b_lo`, `nir_lo`).

| LiDAR ↓ \ Spectral → | leaf-on | leaf-on + leaf-off |
|---|---|---|
| **no LiDAR** | `fld_nolidar_leafon` | `fld_nolidar_leafoff` |
| **CHM + returns** | `fld_chmret_leafon` | `fld_chmret_leafoff` *(full feature set)* |

### 3.4 Label block (full feature set only)

| Config | Training labels | Directory | Role |
|---|---|---|---|
| `fld_chmret_leafoff` | field (shared with the factorial cell) | `R_Patches/` | gold-standard baseline |
| `nwi_chmret_leafoff` | NWI, paired to field locations | `R_Patches_NWI/` | operational stale-label baseline |
| `nwiextra_chmret_leafoff` | NWI, ~2× (paired ∪ extra same-HUC12 locations) | `R_Patches_NWI/` ∪ `R_Patches_NWIextra/` | quantity test |
| `nwifield_chmret_leafoff` | field ∪ non-overlapping NWI | `R_Patches/` + `R_Patches_NWIextra/` | hybrid |
| `flddeg_chmret_leafoff` | field, degraded to NWI wetland prevalence | `R_Patches/` + code | quantity-vs-correctness control |

> **Scope-control rule (do not violate).** The label comparison runs **only** on the
> full feature set (`chmret_leafoff`); feature ablations run **only** on
> field-verified labels. This keeps the matrix at **8 configs**, not the full
> cross-product.

### 3.5 Channel counts (v3)

| # | Config | LiDAR added | Leaf-off added | `in_channels` (v3) | was (v2) |
|---|---|---|---|---|---|
| 1 | `fld_nolidar_leafon` | — | — | 21 | 18 |
| 2 | `fld_nolidar_leafoff` | — | 4 | 25 | 22 |
| 3 | `fld_chmret_leafon` | CHM + 3 returns | — | 25 | 22 |
| 4 | `fld_chmret_leafoff` | CHM + 3 returns | 4 | **29** | 26 |
| 5 | `nwi_chmret_leafoff` | CHM + 3 returns | 4 | 29 | 26 |
| 6 | `nwiextra_chmret_leafoff` | CHM + 3 returns | 4 | 29 | 26 |
| 7 | `nwifield_chmret_leafoff` | CHM + 3 returns | 4 | 29 | 26 |
| 8 | `flddeg_chmret_leafoff` | CHM + 3 returns | 4 | 29 | 26 |

> **Channel sanity anchor = config 4.** It is the full 20-predictor set (29 channels
> after the one-hot) and **is** the master stats file's predictor set. The preflight
> asserts `config_in_channels("fld_chmret_leafoff") == 29` against the master.
> Configs 5–8 share config 4's predictors (they differ only in label source), so they
> inherit the same 29.
>
> **`dl_experiment_config.py` is the source of truth, not this table.** Run it with
> no arguments for the channel-matrix self-check; `--list` for names; `--emit
> <config> --mode <mode>` for the shell vars the runner consumes.

### 3.6 Mode axis and run count

Every config runs in **both** modes: `multiclass` (EMW/FSW/SSW/UPL) and `binary`
(WET/UPL, via `binary_mapping`). Predictor normalization is identical between modes;
only `class_names`, `num_classes`, and class weights change, and each mode gets its
own per-config stats and its own results subtree.

**8 configs × 2 modes × 5 seeds = 80 cells** in the base grid, plus 5 cells per mode
for each of the two extra architecture arms (§6) = **80 + 20 = 100 cells**.

---

## 4. Shared design constants

Hold these across every run — they are what make the contrasts interpretable.

- **The test set is always field-verified, undegraded, and identical across configs
  for a given seed.** Both label sources and both modes are judged on the same
  gold-standard field pixels. **Non-negotiable** — it is the whole reason the
  label-provenance contrast is clean. Mechanism: the field-anchored split (§5.4).
- **Validation follows the training label source.** NWI/`nwiextra`/`nwifield` configs
  early-stop on their own validation labels; `fld`/`flddeg` validate on field labels.
  Test stays field for all.
- **Fixed loss:** plain weighted CE (`--ce-weight 1.0 --dice-weight 0.0
  --focal-gamma 0`).
- **Fixed class-weight power:** `--weight-power 0.5`, so weighting is never a hidden
  variable. Weights still *differ* per config/mode because they are recomputed from
  the active label source's pixel counts — that difference is intended; the power is
  held. Every stats filename carries the `_wp0.5` suffix.
- **Fixed schedule and capacity:** AdamW + ReduceLROnPlateau, 50 epochs, batch 16,
  `16-mixed`, base filters 64, depth 5. (The architecture arms vary batch size only,
  for memory — see §6.5.)
- **Metrics:** per-class IoU, recall/precision, macro-F1, and the **full confusion
  matrix** (the UPL↔FSW cells in multiclass; the WET↔UPL cell in binary). Overall
  accuracy is reported but is never the headline — it tracks UPL prevalence.

---

## 5. Settled decisions

All of these were decided in v2 and carry into v3 unchanged. They are recorded here
because the *reasons* are what keep someone from "simplifying" them later.

### 5.1 Label storage — separate directories, directory-aware keys

v1 stored `MOD_CLASS_FLD` / `_NWI` / `_FLDDEG` as bands in one merged patch, so
alignment was free. **v2 moved to one directory per label source** because the NWI
patch *count* varies — the quantity test needs ~2× as many patches, at new
locations, which a single fixed-grid file cannot express.

- `R_Patches/` — field-verified (`MOD_CLASS` = the field label).
- `R_Patches_NWI/` — NWI at the **same locations**, paired 1:1; filename is
  `"NWI_"` + the field basename.
- `R_Patches_NWIextra/` — NWI at **new locations within the same HUC12 watersheds**,
  geographically disjoint from field/NWI (0 patch-window overlap; nearest ≥ 258 m >
  the 256 m patch width). The ~2× pool is `R_Patches_NWI ∪ R_Patches_NWIextra`,
  assembled at config time — not stored redundantly.

`flddeg` is **not** a directory: it is a seeded in-memory relabel of `R_Patches`
field labels on the train/val partition only.

**Keying note (this is the bug that was found and fixed 2026-07-06).** Do **not** key
patches by the `cluster_..._patch_N` substring. It drops the source-dataset prefix
(`ADK_WCT_AJS_`, `gps_jc_`, `NWI_RSM_`, `NEW_AJS_`, …), which is identity-bearing —
every source restarts `patch_N` inside a cluster+HUC. On the real data that substring
is neither unique within `R_Patches` nor comparable across directories — measured on
the v2 689-patch set: **56 collisions** within `R_Patches` and **594 spurious**
field↔`NWIextra` "matches". (Re-measure on the current set if you ever need the
exact figures; the conclusion does not change.) Two relations replace it, in
`dl_experiment_config.py`:

- **`nwi_field_twin()`** — filename pairing for field↔NWI: strip exactly one leading
  `"NWI_"`. Verified 1:1, including the NWI-sourced field patches whose twin is
  double-prefixed `NWI_NWI_…`. A paired twin means *the same ground, pixel for
  pixel* (the preflight verifies CRS/transform/size/mask).
- **`huc12_of()`** — HUC12 geography for `NWIextra`, which shares no footprint with
  field and therefore has no filename relation to it at all.

### 5.2 NWI "non-wetland" semantics — confirmed UPL

A pixel with no NWI wetland polygon is labeled **confirmed UPL (class 3)**, not
`ignore_index`. NWI omission errors therefore enter training as realistic
FSW/SSW/EMW→UPL label noise — which is the operational stale-label case the
experiment exists to measure, and the condition under which `flddeg` is the clean
quantity-vs-correctness control. `ignore_index=255` is reserved for genuinely
unlabeled / out-of-footprint pixels, and its mask must be identical between paired
field and NWI patches so the comparison stays pixel-aligned.

### 5.3 Replication — repeated fixed-split seeds, R=5

Outer loop over seeds `0..4` on a fixed 70/15/15 field split. **The same seed yields
the same field train/val/test partition across all configs, all modes, and all three
architecture arms**, so differences are attributable to inputs/labels/mode/arch, not
split luck — and every contrast can be paired per seed.

v2 defaulted the drivers to R=3 and ran R=5 by hand. **v3 makes R=5 the default in
`run_factorial.sh`, `run_arch_compare.sh`, and `run_arch_fusion.sh`.** The reason is
the three-arm comparison: it pairs on the *intersection* of the arms' seeds, so a
short arm silently shrinks n rather than erroring. Raising the whole grid (rather
than topping up one config) also keeps every factorial contrast at n=5.

### 5.4 Split anchor and leakage guard — the field split is the anchor

The most likely silent failure is a test patch leaking into a training pool through
the separate directories. The rule:

1. Compute the split **once on `R_Patches`** with `create_data_splits(seed)` →
   `train_fld` / `val_fld` / `test_fld` as sets of field basenames.
2. **`test_fld` is the test set for every config and both modes** — always drawn
   from `R_Patches`, field-labeled and undegraded.
3. Each config's train/val pools are drawn from its own label directory and filtered
   clear of the `test_fld` footprints using the *source-appropriate* key: filename
   pairing for `fld`/`nwi`/`flddeg`, HUC12 geography for `nwiextra`/`nwifield`.
4. **Hard preflight assertion:** no `test_fld` footprint reaches any config's train
   or val pool under its guard. This is the headline gate.

`dl_patch_pools.resolve_pools()` is the single place the guard lives, and the
preflight imports it — so preflight and training agree by construction rather than
by review.

**Leakage regime — run both, HUC12 is the headline.** `R_Patches_NWIextra` is
geographically distinct from field (hard/pixel leakage is already zero) yet shares
its HUC12s with field (29/29 on the v2 set), so a training patch can sit in the same watershed as a test patch
(soft autocorrelation leakage). `LEAKAGE_GUARD` selects the regime: **`huc12`**
(default, headline) drops every `NWIextra` patch sharing a HUC12 with a `test_fld`
patch — the conservative, reviewer-proof number; **`coord`** (sensitivity run) drops
only patches whose 256 m window overlaps a `test_fld` patch (currently none).
Agreement between the two is what shows the `nwiextra` quantity gain is real and not
autocorrelation. The regime is recorded in every manifest.

### 5.5 Pool construction for `nwiextra` and `nwifield`

- **`nwiextra`** — train/val pool = `R_Patches_NWI ∪ R_Patches_NWIextra` (~2× the
  `nwi` pool) with every HUC12 holding a `test_fld` patch dropped under
  `LEAKAGE_GUARD`, split into train/val by seed (~82/18 of the survivors, mirroring
  70/15 of the whole). Contrast against `nwi` — same labels, 1× count — isolates the
  effect of NWI *quantity* at fixed correctness.
- **`nwifield`** — train/val pool = field-labeled `R_Patches[train_fld ∪ val_fld]`
  ∪ the `nwiextra` pool above, same guard. The union is disjoint by construction
  (`NWIextra` footprints never coincide with field ones), so there is no label
  conflict to arbitrate.

---

## 6. Architecture comparison — three arms

v3's headline addition. All three arms train on **one config**
(`fld_chmret_leafoff`, 29 ch), **both modes**, **the same 5 seeds**, with
bf64/d5/50-epochs held — so the only variable is the network.

| Arm | Driver | Results root | Cell |
|---|---|---|---|
| U-Net (baseline) | the base grid itself | `Models/factorial_results_v3/` | `<mode>/<config>/seed<k>` |
| UNet3+ | `run_arch_compare.sh` | `Models/results_arch_v3/` | `<mode>/<config>_unet3plus/seed<k>` |
| **`mbfusion`** | `run_arch_fusion.sh` | `Models/results_arch_fusion_v3/` | `<mode>/<config>_mbfusion/seed<k>` |

> **No arm is pre-trained.** The v2 arms exist in `Models/factorial_results_v2/` and
> `Models/results_arch_v2/`, but they are 26-channel runs on the old 689-patch pool
> and are **not comparable** to a v3 cell. All three retrain from scratch.

### 6.1 Scope of the fusion arm

**In scope:** a new `--arch mbfusion`, dispatched by `dl_model_factory.build_net()`,
run on `fld_chmret_leafoff` across seeds 0–4 in both modes, aggregated as a third arm.

**Out of scope — do not do these:**

- Do **not** run `mbfusion` across the 8-config grid. One config only.
- Do **not** build a fusion patch-curve arm (§9.2).
- Do **not** modify `dl_01`–`dl_06`, `dl_02_dataset.py`, or the Lightning module's
  `_shared_step`. The design is deliberately model-side so these stay untouched.
- Do **not** modify the production recipe on the strength of a partial result — the
  production workstream is being spun out into its own project, and revisiting its
  architecture happens *after* the 5-seed comparison lands (§10).

### 6.2 Branch partition

Four branches, drawn by physical process and sensing modality. All inputs are native
1 m (SAR and Sentinel-2 are not in this stack).

| Branch | Bands | Channels | Encoder width |
|---|---|---|---|
| terrain | DEM, slope_local, TPI_local, Geomorph_local, meanc_local, dmv_local, flowacc, twi | 17 | 48 |
| lidar | CHM, pct_below_1m, pct_1m_to_5m, pct_above_5m | 4 | 32 |
| leafon | r, g, b, nir | 4 | 32 |
| leafoff | r_lo, g_lo, b_lo, nir_lo | 4 | 32 |

**29 input channels → 144 fused.** `Geomorph_local` one-hot expands 1 band → 10
channels, which is why terrain is 17 rather than 8. Both constants live in
`dl_experiment_config.py` as `BRANCH_BANDS` / `BRANCH_WIDTHS`, and branches are built
only from bands the **active config** supplies — so a `nolidar` or `leafon` config
drops that branch rather than erroring, and the gate softmaxes over three.

**Why the widths are asymmetric.** Width is a design knob, not a function of input
channel count. Proportional allocation would hand terrain ~59% of encoder width
largely because of how geomorphon happens to be encoded (10 channels carrying ~3.3
bits), starving the LiDAR and leaf-off branches — precisely the two meant to resolve
the UPL→FSW confusion. Terrain leads because wetland occurrence is terrain-driven
(depressions, low positions, channels); the other three lead the vegetation-class
split and are not bottlenecked.

**NDVI/NDWI excluded.** Raw bands only. Including `n_ndvi`/`n_ndwi` for leaf-on alone
would confound the leaf-on/leaf-off gate comparison with channel count; including
them for both adds redundant, highly-correlated channels against the "more bands ≠
better" finding (Maxwell et al., Wu et al.). The illumination-invariance argument for
normalized indices is real but is a cheap later ablation, not a design decision here.

### 6.3 Architecture (Design A)

**Fully parallel branch encoders to full depth, fusion at every encoder scale, one
shared decoder.**

- Each branch runs its own encoder at width `w_b`; at level *L* the branch is
  `w_b * 2**L` wide.
- At each of the 6 scales (depth 5), a `BranchFusion` module gates and concatenates
  the branch features, then projects to `64 * 2**L` — the width the existing U-Net
  decoder expects. That fused output is the decoder skip for that scale.
- The decoder is **bit-identical to the U-Net's** (verified: 82 tensors, matching
  shapes), so the comparison isolates encoder + fusion as the only changed variable.

```python
class BranchFusion(nn.Module):
    def __init__(self, widths, out_ch):          # widths e.g. [48,32,32,32] * 2**L
        super().__init__()
        self.norms = nn.ModuleList(nn.GroupNorm(8, w) for w in widths)
        self.gate  = nn.Conv2d(sum(widths), len(widths), 3, padding=1)
        self.proj  = nn.Conv2d(sum(widths), out_ch, 1)

    def forward(self, feats):
        feats = [n(f) for n, f in zip(self.norms, feats)]
        g = self.gate(torch.cat(feats, 1)).softmax(dim=1)   # B, n_branch, H, W
        self.last_gates = g.detach()
        fused = torch.cat([f * g[:, i:i+1] for i, f in enumerate(feats)], 1)
        return self.proj(fused)
```

Design notes, each of which is load-bearing:

- **Six independent instances, not weight-shared.** Sharing would force one gating
  function across all scales and erase the scale-resolved result the design exists
  to produce.
- **GroupNorm per branch before gating** removes magnitude confounding from unequal
  branch widths — otherwise a wide branch producing larger features would let the
  gate compensate with a smaller weight.
- **Weighted concatenation, not weighted sum.** Concat is what makes unequal branch
  widths compatible with comparable gate scalars.
- **3×3 gate kernel, uniform across scales.** The gate compresses a `144·2**L`
  channel vector to 4 logits; at 1×1 that is a high-variance per-pixel decision
  producing speckled, unreadable gate rasters. 3×3 regularizes it for ~330k params
  total, and matches the ecotone argument: what varies gradually across an FSW/UPL
  transition is *modality reliability* (canopy thins → return fractions degrade →
  leaf-off ground visibility improves). Kernel size is fixed in feature-map units,
  so the ground footprint grows with level (3 m at level 0 → 96 m at level 5) as a
  consequence of downsampling, not as a separate choice.
- **Gates stored as an attribute**, not returned, so `forward()`'s signature is
  unchanged and `dl_05`/`dl_06` need no edits. Analysis reads
  `model.fusions[i].last_gates` after a forward pass.

**Gate resolutions** (256² patches, depth 5): level 0 = 256²/1 m · 1 = 128²/2 m ·
2 = 64²/4 m · 3 = 32²/8 m · 4 = 16²/16 m · 5 (bottleneck) = 8²/32 m.

**Interaction caveat, accepted.** Isolated branches cannot represent cross-modal
interactions (low-lying **and** canopy present **and** wet ground visible leaf-off)
*inside* the encoder. The partial mitigation is structural: the gate is a softmax
*across* branches, so computing it requires seeing all four — every fusion point is
itself a cross-modal interaction, at six scales.

### 6.4 Cost

Conv params scale with the square of width, so four narrow parallel branches are
`(48² + 3·32²) / 64² ≈ 1.31×` a single width-64 encoder — not 4×. On top of the
~100 M of branch encoders, the fusion modules add 12.6 M (six 1×1 `proj`, dominated
by the bottleneck's 4608→2048 = 9.4 M) + 0.33 M (six 3×3 `gate`).

Measured at bf64/d5/29ch: **162.01 M params vs the U-Net's 125.27 M = 1.293×.** So
the honest statement is **encoder side ≈1.5×, whole model ≈1.3×** — write it as
"~1.3× total params" and let the cost table carry the detail. The claim "~1.3× the
*encoder*" does not survive contact with the measurement.

**Params are not the binding constraint — activations are.** At level 0 the
concatenated fused tensor is 144 channels at 256² against the U-Net's 64: **2.25×
the finest-scale activation.** That, not parameter count, is what sets the batch
size. Still lighter than UNet3+.

### 6.5 Integration requirements

The dataset keeps returning a single `(B, 29, H, W)` tensor; the model slices it per
branch with `x[:, idx]`. `build_net()` takes a `branch_indices: dict[str, list[int]]`
alongside `in_channels`. Four hard requirements:

1. **`branch_indices` must be built in post-expansion channel space, from
   `stats["predictor_names"]`.** `Geomorph_local` expands 1 band → 10 channels
   *before* the tensor reaches the model, so terrain's 17 indices include that
   contiguous one-hot block. The authoritative order is the stats file's
   `predictor_names`, because that is what `WetlandPatchDataset` indexes the raster
   by. It is **not** raster band order — `dl_make_config_stats.py` writes
   `predictor_names = config_bands(cfg)` (BASE + LIDAR + SPECTRAL), which puts CHM
   *after* `r,g,b,nir` where the raster puts it *before*. They coincide today only
   for the base bands. Never build the map from raw raster order, and never from
   `BRANCH_BANDS` order.
2. **Serialize `branch_indices` *and* `branch_widths`** into Lightning
   `hyper_parameters` and the `.meta.json` sidecar. `load_model()` auto-detects
   architecture, and both are config-dependent (a `nolidar` cell has three
   branches), so eval and predict cannot reconstruct the model without them.
   `BRANCH_WIDTHS` being a constant is not a reason to skip it — two runs with
   different widths would otherwise be indistinguishable after the fact.
3. **Preflight assertion before any GPU time.** `dl_preflight_check.py` **[9]**
   verifies on CPU that the branch slices reconstruct the full stack: union covers
   all 29 channels, no overlaps, terrain's one-hot block contiguous and correctly
   placed. This is the guard against requirement 1, and it is the *one silent
   failure mode of this whole design* — a wrong map trains fine and reports
   plausible numbers while each encoder reads the wrong bands.
4. **Batch size.** Defaults to 8 (`BATCH_SIZE` env knob); halve to 4 on CUDA OOM.
   Expect one OOM-and-halve iteration on first launch.

### 6.6 Watch for gate collapse

Gated mixtures can collapse, one branch saturating near 1.0 everywhere within the
first few epochs. **Mean gate entropy per scale** is logged to TensorBoard as
`train/gate_entropy/level0..5`. Healthy is near `log(n_branch)` = 1.386 for four
branches; trending toward 0 early means collapse. The standard fix is a temperature
on the gate logits — **deliberately not built in speculatively.**

### 6.7 Reading gate maps — the one caveat

After gating, `proj` is a 1×1 conv over the concatenated features, so the decoder
input is effectively `Σᵢ Wᵢ(fᵢ · gᵢ)`. The gate is a spatial scalar; `Wᵢ` is a
learned linear map with its own magnitude. Therefore:

- **Valid:** within-branch spatial comparison — "terrain reliance rises in
  depressions relative to sideslopes." The gate is the only thing varying across
  space, so spatial patterns are faithful.
- **Confounded:** cross-branch absolute comparison — "terrain matters more than
  LiDAR overall" — since a branch with modest gates but large `Wᵢ` can still
  dominate. GroupNorm equalizes features, not projection weights.

So: report per-branch gate maps **standardized within branch**, and take overall
branch importance from **SHAP** (`dl_09_shap_factorial.py`), which is already in the
pipeline. Keep the two claims separate. The means in `gate_summary.json` are
provenance, not a ranking.

### 6.8 Framing for the methods section

The usual justification for separate modality branches is sensor misalignment —
LiDAR point clouds vs. HSI pixels, registration error (Effah et al. 2025 §3.4).
**That problem does not exist here:** everything is already co-registered on a 1 m
grid as bands of one stack. The defensible justification is instead:

1. **Preventing a spectrally dominant, high-variance modality (leaf-on NAIP) from
   swamping early feature learning.**
2. **Per-branch gating weights as an interpretability output.** The factorial says
   which categories matter when present or absent; branch gating says how much the
   model weights each one, *per pixel and per scale*. This is a scale-resolved
   version of the below-canopy hypothesis and pairs directly with SHAP.

Effah et al. recommend **feature-level fusion** (the majority strategy in wetland
studies; better dimensionality control and alignment, accuracy gains without heavy
computational overhead), which supports a branch encoder over plain channel
stacking. They also repeatedly flag that hybrid-architecture complexity must be
justified — which is what the cost table and the ~1.3× figure are for.

**On Wang et al. (2025), D2HU-Net.** Its "dual branch" is *not* modality fusion: one
encoder, two decoding paths, where a shallow path guides the deep one via the MSAF
module. That is a different axis and should not be conflated with this design in the
writeup. The transferable piece is MSAF's **adaptive per-branch weighting**, adapted
here from decoder-path fusion to modality fusion and extended from global
coefficients to per-pixel gates.

---

## 7. Aggregation and analysis

The CPU/GPU split mirrors the node split: **aggregation is pure pandas on the CPU
node; SHAP is GPU-side** (it backprops through each model, so it must run inside the
container before reservation teardown).

### 7.1 Base factorial (`dl_08_aggregate_factorial.py`)

Walks `<root>/<mode>/<config>/seed<k>/metrics.json` into:

| Output | Contents |
|---|---|
| `factorial_long.csv` | per (config, seed, class) precision/recall/f1/iou |
| `factorial_summary.csv` | mean & sd over seeds |
| `factorial_table.csv` | headline pivot: FSW/UPL IoU+recall, macro-F1 (mean±sd) |
| `contrasts.csv` | paired-by-seed effects |
| `confusion_mean/<config>.csv` | seed-mean confusion matrix |
| `coverage.csv` | which (config × seed) cells are present |

**Headline contrasts:** LiDAR tier (`nolidar→chmret`), leaf-off main effect,
LiDAR×leaf-off interaction, and the **label gradient**
(`nwi → nwiextra → nwifield → flddeg → fld`) — the quantity-vs-correctness curve.
All are **paired by seed**: the same seed gives the same split across all 8 configs,
so per-seed differences net out split luck before the mean±sd. Safe on a partial
tree — it reports coverage and computes every contrast it has cells for.

### 7.2 Architecture comparison (`dl_08b_aggregate_patchcurve.py --arch-compare`)

Takes repeatable `--arch-dir <name>=<path>` pairs, so arm count follows from the CLI
(`--unet-dir`/`--unet3plus-dir` remain as deprecated two-arm aliases; the v2 output
reproduces numerically identically from the new code). Outputs four CSVs:

| File | What it holds |
|---|---|
| `arch_compare_long.csv` | one row per (arch, seed) — every metric plus cost. The tidy form; plot from this. |
| `arch_contrasts.csv` | paired per-seed deltas vs the baseline arm, with `n_better`/`n_seeds` |
| `arch_cost.csv` | params, GFLOPs, sec/epoch per arm, and params as a multiple of baseline |
| `arch_compare.csv` | wide per-seed table + seed-mean row (the v2-shaped output) |

**Metrics, in priority order:**

1. **UPL↔FSW confusion cells** — the specific failure this architecture targets.
   `--confusion-pair A B` (default `FSW UPL`), read per cell from
   `confusion_matrix.csv` and **row-normalized**, so `conf_FSW_as_UPL` is "share of
   true-FSW pixels predicted UPL" — comparable across cells of differing prevalence,
   which raw counts are not. Contrasts on these rows are direction-aware
   (`lower_is_better`). Absent classes (binary mode) are skipped, not zero-filled.
2. Macro F1 and per-class IoU (the standard table), plus per-class F1.
3. WET IoU + recall, so the production-recipe question stays answerable from the
   same output. Binary-mode cells carry `iou_WET`/`f1_WET` through the same columns.

**Report paired per-seed deltas, not only mean ± sd.** Same seed ⇒ same test patches,
so each seed gives all three arches an identical evaluation set. At n=5 the credible
statistic is **consistency of sign** ("fusion beats U-Net in 5/5 seeds by 1.2–2.1
points"). **No p-values are computed, deliberately** — five paired differences do not
support a distributional claim.

**Cost-table provenance**, since the three columns do not share a source: **params**
come from the trainer's journal `cost` block (exact, added in v3), falling back to
the `.safetensors` header (also exact, but absent from a `--metrics-only` pull) and
then to Lightning's rounded summary; **GFLOPs** only from that summary in
`train.log`, so the column is blank where the container's Lightning did not print it;
**sec/epoch** is fit-only wall clock from the journal, v3 onward, left blank rather
than reconstructed from file mtimes for older cells. It is a same-GPU comparison, not
a portable benchmark.

### 7.3 SHAP (`dl_09_shap_factorial.py`)

Runs on the trained cells, resolving each cell's *actual* pools split (same seed +
guard as training, so SHAP background = the cell's train pool and SHAP test = its
held-out field patches) and loading that config's per-config stats, so the band
subset is correct for free. Writes per cell:

- `*_shap_importance.json` — per-band importance in **both** aggregations
- `*_shap_per_channel.npz` — spatially-averaged per-channel |SHAP|, the raw array
  the JSON aggregates
- figures (`*_shap_band_importance*.png`, `*_shap_summary_plot.png`)

**Why both aggregations.** A band's channels are summed back to band level, but the
one-hot `Geomorph_local` band is 10 channels while every continuous band is 1 — so
the SUM inflates it ~10×. The per-channel MEAN (`sum / n_channels`) is the fair
comparison; the truth sits between. Report both side by side, and use the `.npz` to
split Geomorphon into its 10 forms (is the signal concentrated in a few wet-landform
forms, or smeared = artifact?).

Pair `contrasts.csv` (ablation = marginal contribution) against the SHAP importance
JSON (reliance) for the feature story.

### 7.4 Cross-mode comparison

Two apples-to-apples views, since macro-F1 is *not* comparable across modes
(different class counts):

- **UPL** — an identical class in both modes → mean ± sd over seeds directly.
- **WET** — collapse each multiclass model's seed-mean confusion matrix
  EMW/FSW/SSW→WET and compare against the native-binary WET. This is the fair
  "collapse a 4-class model vs. train binary" baseline.

Plus the label-gradient panel in both modes. Output:
`<root>/analysis/cross_mode_summary.csv` + figures.

### 7.5 Figures

`R_Code_Analysis/dl_10_Factorial_viz_R.qmd` is the **active** viz notebook; the
Python `dl_10_factorial_viz.ipynb` is the older sibling (§1–§5 base factorial + SHAP,
§6–§7 follow-ons), and `dl_10b_huc_inference_viz.ipynb` holds the data-heavy HUC
prevalence section. The R notebook's architecture section reads
`arch_compare_long.csv` and is arm-count-agnostic — point `arch_dir_base` at the
fusion root and it renders macro-F1, per-class F1, the FSW↔UPL confusion panel, the
contrast and cost tables, and a gate-weight-by-scale plot. Adding a fourth arm needs
only an entry in `arch_name`/`arch_color`.

---

## 8. Output layout

```
Models/factorial_results_v3/                 # base grid: U-Net, 8 configs
├── multiclass/<config>/seed{0..4}/
│   ├── manifest.json          # bands, in_channels, mode, label source, patch dirs,
│   │                          # pool rule, leakage regime, class weights, loss,
│   │                          # arch, git commit, stats files, degrade provenance
│   ├── metrics.json           # scores nested under "test_metrics" (see EXECUTION §12)
│   ├── confusion_matrix.csv
│   ├── training_log.json      # incl. the v3 "cost" block (params, sec/epoch)
│   ├── best_*.safetensors + .meta.json, best_*.ckpt
│   ├── train.log
│   ├── tb_logs/
│   └── shap/
├── binary/<same 8 configs>/seed{0..4}/
└── analysis/                  # dl_08 output + cross_mode_summary.csv

Models/results_arch_v3/<mode>/<config>_unet3plus/seed{0..4}/
Models/results_arch_fusion_v3/<mode>/<config>_mbfusion/seed{0..4}/
    └── gates/<patch>.npz + gate_summary.json      # fusion arm only

Data/Training_Data/stats/
├── multiclass_normalization_stats_wp0.5.json       # master (= config 4's predictors)
├── binary_normalization_stats_wp0.5.json           # master
├── multiclass_normalization_stats_<config>_wp0.5.json   (×8)
└── binary_normalization_stats_<config>_wp0.5.json       (×8)

Data/HUC_DL_Predictions_v3/DLpred_<mode>_cluster_<C>_huc_<H>.tif  (+ _probs.tif)
```

`<mode>/<cell>` is the row label in the final tables, so aggregation is a directory
walk. Note the `<mode>/` level is inserted by `run_config.sh`
(`$RESULTS_DIR/$MODE/$CELL_NAME/seed$SEED`), so **every** results root in this repo
has that shape — including the arch arms.

---

## 9. Follow-on studies

### 9.1 HUC prediction / inference maps — in scope

Per-HUC class + per-class softmax GeoTIFFs from the best cell, via
`dl_06b_predict_huc.py` + `dl_huc_stack.py`. Needs the ~7 per-HUC source rasters,
which live **outside** the repo mount — hence a two-mount container. Feeds the
webmap COG pipeline and the prevalence summary notebook. Commands in
`EXECUTION.md` §10.

### 9.2 Patch-count learning curve — dropped from v3

`run_patchcurve.sh` and `dl_08b`'s curve mode still work and are unchanged, so the
v2 curve reproduces; the study is simply **not part of v3**. The reason is that the
current 100–500 patch range is under one order of magnitude and won't support a
scaling claim. **Deferred until the training pool reaches 1000s of patches** — at
which point it drops back in with no new tooling.

Note that for a *fusion* curve specifically, the hypothesis is genuinely two-sided:
more capacity argues for a steeper curve, more structural constraint argues for a
flatter one that peaks early. Worth measuring, not assuming.

---

## 10. Relationship to the production model

The factorial asks *which inputs, labels, and architecture matter*; it trains ~100
short, comparable models and throws them away. **None of them is a deliverable.**
The production workstream answers the next question — what do we ship — and is
**being separated into its own project.** Its docs stay where they are
(`../production_model/{PLAN,EXECUTION}.md`, recipe in `dl_prod_config.py`) and are
deliberately *not* consolidated here.

Two things this plan owes it:

- **Its current recipe is stale.** `nwifield_chmret_leafoff` was chosen on
  **factorial-v2** results (26 channels, 689-patch pool). v3 retrains everything at
  29 channels on a ~1012-patch pool, so the config ranking must be re-read from
  `Models/factorial_results_v3/analysis/cross_mode_summary.csv` before the next
  production run.
- **The architecture choice is gated on this comparison.** Do not pre-emptively
  switch production to `mbfusion` (or edit `dl_prod_config.py` at all) until the
  three-arm, 5-seed result lands in both modes.

---

## 11. Pre-run status and open items

### Verified (2026-08-25)

- v3 matrix live in `dl_experiment_config.py`: 21/25/29 channels, self-check passes.
- `BRANCH_BANDS`/`BRANCH_WIDTHS` present; `dl_03_mbfusion_model.py`,
  `dl_11_export_gates.py`, `run_arch_fusion.sh` all exist.
- All three drivers default to `SEEDS="0 1 2 3 4"` and the `_v3` results roots.
- `run_config.sh` defaults `RESULTS_DIR` to `Models/factorial_results_v3`.
- Preflight carries the **[9]** fusion branch-partition gate.
- Patch dirs: `R_Patches`, `R_Patches_NWI`, `R_Patches_NWIextra` — **1012 each** as of
  2026-08-25, and being rebuilt. The preflight, not this line, is the authority on
  counts and pairing.

### Must be resolved before GPU time

- ⚠️ **The multiclass master stats are stale.** Built over **1007** patches;
  `R_Patches` now holds **1012**. Patch count feeds the field class weights, so the
  master and all 16 per-config files must be rebuilt (`EXECUTION.md` §3 steps 2–3).
- ⚠️ **The binary master is a 26-channel file from 2026-06-05** (v2 vintage). The
  per-config binary files are 29-channel and current, and
  `dl_make_config_stats.py` derives both modes' normalization from the *multiclass*
  master — so this may be vestigial, but confirm rather than assume.
- ℹ️ `Data/Training_Data/stats/` still holds v1-vintage `fld_chm_*` files for a tier
  that no longer exists. Harmless (nothing resolves them), but they will confuse a
  reader listing the directory.
- **Preflight GREEN for all 8 configs × 2 modes**, including the leakage guard and
  the **[9]** branch partition, is the hard gate.

### Deferred / open

- **Equal-width control arm** for the fusion encoder (`32/32/32/32`). Edit the
  constant and re-run; the §6.5-2 serialization requirement is what makes the two
  runs distinguishable afterward.
- **Optical branches with NDVI/NDWI** (6 channels each, symmetric) — a cheap later
  ablation if the illumination-invariance question is worth answering.
- **Forest-restricted metrics** (CHM-threshold mask) — not built. `dl_08` will pick
  up a `metrics_forest.json` automatically if a future runner writes one.
- **Fusion patch-curve** — see §9.2.

---

## Pointers

- **How to run it:** [`EXECUTION.md`](EXECUTION.md)
- **Source of truth for the matrix:** `../dl_experiment_config.py`
- **Superseded originals:** `archive/` (v1 plan, v2 plan, v1+v2 EXECUTION,
  `arch_fusion_PLAN.md`)
- **Production workstream (separate project):** `../production_model/`
- **Branching policy:** root `CLAUDE.md` § "Repo & Branching"
