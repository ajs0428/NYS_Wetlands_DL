# Multi-Branch Fusion Encoder (`mbfusion`)

Third architecture arm for the factorial paper's architecture comparison. A multi-branch
encoder that extracts features separately per input category and fuses them with a
per-pixel, softmax-normalized gate at every encoder scale.

**Status:** implemented and CPU-tested; **not yet trained**. Phases 0–4 complete
(data, v3 config registry, model + plumbing, orchestration, aggregation + figures).
Remaining: the GPU runs — the whole v3 grid at R=5, then the two extra arms — and
then the production gate in §8.
**Sibling docs:** `factorial_experiment/EXECUTION.md` (node ritual, transfer pattern),
`production_model/PLAN.md` (the shipped recipe — *not* to be modified by this work).

Measured at bf64/d5/29ch: **162.01 M params vs the U-Net's 125.27 M = 1.293×** — the
~1.3× this document's cost argument predicted (§3). The decoder is verified
bit-identical to the U-Net's (82 tensors, matching shapes), so the comparison
isolates encoder + fusion as intended.

### v3 context (added 2026-08-17)

This arm lands inside **factorial v3**, not alongside v2. The three terrain metrics
`TPI_local`, `meanc_local`, `dmv_local` were added upstream in `NYS_Wetlands_Data/`, so the
patch stacks went 18/20 bands → **21 bands** and the full feature set went 26 → **29
channels** — which is what makes this plan's four-branch, 29-channel partition land exactly.

Verified data state (2026-08-17): `R_Patches` 1007 · `R_Patches_NWI` 1012 ·
`R_Patches_NWIextra` 1014, all 21-band with identical band order; field↔NWI pairing is
1007/1007 (5 NWI patches in `cluster_198/huc_043001081603` have no field twin and are simply
never selected by the `paired` pool rule). `HUC_DL_Stacks_Extracted_Values.json` carries all
19 continuous bands (`Geomorph_local` is one-hot and needs no min/max).

Consequences for this document:

- **No comparison arm is pre-trained.** §5 previously said the U-Net and UNet3+ arms already
  existed. They do, but in `Models/factorial_results_v2/` and `Models/results_arch_v2/` — 26
  channels, 689 field patches. Under v3 **all three arches retrain from scratch** on the
  29-channel stack.
- **Both classification modes** (`multiclass` and `binary`) are in scope for v3, matching v2.
- The patch-count learning curve is **dropped from v3 entirely** (was already out of scope
  here; see §8).

---

## 1 · Scope

**In scope:** a new `--arch mbfusion`, dispatched by `dl_model_factory.build_net()`, run on
the single config `fld_chmret_leafoff` across seeds 0–4, aggregated as a third arm against
the existing U-Net and UNet3+ results.

**Out of scope — do not do these:**

- Do **not** run `mbfusion` across the 8-config grid. One config only.
- Do **not** build a patchcurve arm. Deferred until the training pool reaches 1000s of
  patches; the current 100–500 range is under one order of magnitude and won't support a
  scaling claim.
- Do **not** modify `production_model/dl_prod_config.py` or `PLAN.md`. The production
  recipe holds every knob at factorial values on purpose so the benchmark ranking stays
  valid evidence for the shipped model. Revisiting the architecture there happens *after*
  the 5-seed comparison lands.
- Do **not** modify `dl_01`–`dl_06`, `dl_02_dataset.py`, or the Lightning module's
  `_shared_step`. The design is deliberately model-side so these stay untouched.

---

## 2 · Branch partition

Four branches, drawn by physical process and sensing modality. All inputs are native 1 m
resolution (SAR and Sentinel-2 are excluded from this stack).

| Branch | Bands | Channels | Encoder width |
|---|---|---|---|
| terrain | DEM, slope_local, TPI_local, Geomorph_local, meanc_local, dmv_local, flowacc, twi | 17 | 48 |
| lidar | CHM, pct_below_1m, pct_1m_to_5m, pct_above_5m | 4 | 32 |
| leafon | r, g, b, nir | 4 | 32 |
| leafoff | r_lo, g_lo, b_lo, nir_lo | 4 | 32 |

**29 input channels → 144 fused.** Geomorph_local one-hot expands 1 band → 10 channels,
which is why terrain is 17 rather than 8.

Both constants live in `dl_experiment_config.py`, alongside the existing config matrix:

```python
BRANCH_BANDS = {
    "terrain":  ["DEM", "slope_local", "TPI_local", "Geomorph_local",
                 "meanc_local", "dmv_local", "flowacc", "twi"],
    "lidar":    ["CHM", "pct_below_1m", "pct_1m_to_5m", "pct_above_5m"],
    "leafon":   ["r", "g", "b", "nir"],
    "leafoff":  ["r_lo", "g_lo", "b_lo", "nir_lo"],
}
BRANCH_WIDTHS = {"terrain": 48, "lidar": 32, "leafon": 32, "leafoff": 32}
```

Branches are constructed only from bands the **active config** supplies, so a `nolidar` or
`leafon` config drops that branch rather than erroring, and the gate softmaxes over three.
This keeps the architecture runnable across the grid even though only one config is run here.

**Rationale for asymmetric widths.** Width is an explicit design knob, not proportional to
input channel count. Proportional allocation would hand terrain ~59% of encoder width
largely because of how geomorphon happens to be encoded (10 channels carrying ~3.3 bits),
and would starve the LiDAR and leaf-off branches — precisely the two meant to resolve the
UPL→FSW confusion. Terrain leads because wetland occurrence is terrain-driven
(depressions, low-lying positions, channels); the other three lead the vegetation-class
split and are not bottlenecked.

**NDVI/NDWI excluded.** `n_ndvi` / `n_ndwi` are dropped in favor of raw bands. Including
them for leaf-on only would confound the leaf-on/leaf-off gate comparison with channel
count; including them for both adds redundant, highly-correlated channels against the
"more bands ≠ better" finding (Maxwell et al., Wu et al.). The illumination-invariance
argument for normalized indices is real but is a cheap later ablation (optical branches at
6 channels each), not a design decision here.

---

## 3 · Architecture (Design A)

**Fully parallel branch encoders to full depth**, fusion at every encoder scale, single
shared decoder.

- Each branch runs its own encoder at width `w_b`; at level *L* the branch is `w_b * 2**L` wide.
- At each of the 6 scales (depth 5), a `BranchFusion` module gates and concatenates the
  branch features, then projects to `64 * 2**L` — the width the existing U-Net decoder
  expects. That fused output is the decoder skip for that scale.
- The decoder is **bit-identical to the existing U-Net**, so the arch comparison isolates
  encoder + fusion as the only changed variables.

**Cost.** Conv params scale with the square of width, so four narrow parallel branches are
`(48² + 3·32²) / 64² ≈ 1.31×` a single width-64 encoder — not 4×. Lighter than UNet3+.

That 1.31× covers the **branch convolutions only**. Measured against the real U-Net
(bf64 / d5 / in=29, 125.3 M params total: 76.2 M encoder+bottleneck, 49.1 M decoder+head),
the fusion modules add on top of the ~100 M of branch encoders:

| piece | params |
|---|---|
| 6 × `proj` 1×1 (dominated by the bottleneck's 4608→2048 = 9.4 M) | 12.6 M |
| 6 × `gate` 3×3 | 0.33 M |

So the honest statement is **encoder side ≈1.5×, whole model ≈1.3×** — the headline survives
at model level, but the word "encoder" does not. Write it as "~1.3× total params" and let
§6's cost table carry the detail.

**Params are not the binding constraint — activations are.** At level 0 the concatenated
fused tensor is 144 channels at 256² against the U-Net's 64: **2.25× the finest-scale
activation**. That, not parameter count, is what sets the batch size (see §5).

**Interaction caveat, accepted.** Isolated branches cannot represent cross-modal
interactions (low-lying **and** canopy present **and** wet ground visible leaf-off) inside
the encoder. Partial mitigation is structural: the gate is a softmax *across* branches, so
computing it requires seeing all four — every fusion point is itself a cross-modal
interaction, at six scales.

### Fusion module

Instantiated six times, once per scale, with **independent (not weight-shared) gates** —
sharing would force one gating function across all scales and erase the scale-resolved
result the design exists to produce.

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

Design notes:

- **GroupNorm per branch before gating** removes magnitude confounding from unequal branch
  widths (a wide branch producing larger features would otherwise let the gate compensate
  with a smaller weight).
- **Weighted concatenation, not weighted sum.** Concat is what makes unequal branch widths
  compatible with comparable gate scalars.
- **3×3 gate kernel, uniform across all six scales.** The gate compresses a `144·2**L`
  channel vector to 4 logits; at 1×1 that is a high-variance per-pixel decision producing
  speckled, hard-to-read gate rasters. 3×3 regularizes it for ~330k params total. It also
  matches the ecotone argument: what varies gradually across an FSW/UPL transition is
  *modality reliability* (canopy thins → LiDAR return fractions degrade → leaf-off ground
  visibility improves). Kernel size is fixed in feature-map units, so the ground footprint
  grows with level (3 m at level 0 → 96 m at level 5) as a consequence of downsampling, not
  as a separate choice.
- **Gates stored as an attribute**, not returned. Keeps `forward()`'s signature unchanged
  so `dl_05` / `dl_06` need no edits; analysis code reads `model.fusions[i].last_gates`
  after a forward pass.

### Gate resolutions (256² patches, depth 5)

| Level | Feature map | Ground resolution |
|---|---|---|
| 0 | 256² | 1 m |
| 1 | 128² | 2 m |
| 2 | 64² | 4 m |
| 3 | 32² | 8 m |
| 4 | 16² | 16 m |
| 5 (bottleneck) | 8² | 32 m |

### Monitor for gate collapse

Gated mixtures can collapse, with one branch saturating near 1.0 everywhere in the first
few epochs. Log **mean gate entropy per scale** to TensorBoard. If it collapses, a
temperature on the gate logits is the standard fix — do not build this in speculatively.

---

## 4 · Integration

**Model-side slicing.** The dataset keeps returning a single `(B, 29, H, W)` tensor; the
model slices it per branch with `x[:, idx]`. `build_net()` gains a
`branch_indices: dict[str, list[int]]` argument alongside the existing `in_channels`.
`--arch mbfusion` becomes a drop-in third option next to `unet` / `unet3plus`.

### Hard requirements

1. **`branch_indices` must be built in post-expansion channel space, from the stats file's
   `predictor_names`.** Geomorph_local expands 1 band → 10 channels *before* the tensor
   reaches the model, so terrain's 17 indices include that contiguous one-hot block.

   The authoritative channel order is **`stats["predictor_names"]`**, because that is what
   `WetlandPatchDataset` indexes the raster by (`dl_02_dataset.py`: `predictor_indices =
   [band_names.index(n) for n in predictor_names]`, then `normalize_and_expand` walks that
   same list appending 1 channel per band and 10 for the one-hot). It is *not* raster band
   order — `dl_make_config_stats.py` writes `predictor_names = config_bands(cfg)`, i.e.
   BASE + LIDAR + SPECTRAL, which puts CHM after `r,g,b,nir` where the raster puts it before.
   The two happen to coincide today only for the base bands.

   So: build the map by walking `stats["predictor_names"]` with the expansion rules from
   `dl_band_utils.get_normalization_method` / `compute_in_channels`. Never from raw raster
   order, and never from `BRANCH_BANDS` order. Getting this wrong silently mis-slices: the
   model trains and reports plausible numbers while reading the wrong bands.

2. **Serialize `branch_indices` *and* `branch_widths`** into Lightning `hyper_parameters`
   and the `.meta.json` sidecar. `load_model()` auto-detects architecture on load, and both
   are config-dependent (a `nolidar` cell has three branches), so eval and predict cannot
   reconstruct the model without them. `BRANCH_WIDTHS` being a constant is not a reason to
   skip this — two runs with different widths would otherwise be indistinguishable after
   the fact.

3. **Preflight assertion before any GPU time.** Extend `dl_preflight_check.py` (already a
   hard gate) with a CPU-side check that the branch slices reconstruct the full stack:
   union covers all 29 channels, no overlaps, terrain's one-hot block contiguous and
   correctly placed. This is the guard against failure mode 1.

---

## 5 · Experiment scaffold

Mirrors `run_arch_compare.sh`; introduces no new patterns.

| Piece | Value |
|---|---|
| Results root | `Models/results_arch_fusion_v3/` |
| Cell path | `<mode>/<config>_mbfusion/seed<k>/` |
| Driver | `Shell_Scripts/run_arch_fusion.sh` |
| Config | `fld_chmret_leafoff` (29 ch — all four branches present) |
| Modes | `multiclass` and `binary` |
| Seeds | 0 1 2 3 4 |
| Held | bf64 / d5 / 50 epochs / 16-mixed — same as factorial |

Note the `<mode>/` level in the cell path: `run_config.sh` inserts it
(`$RESULTS_DIR/$MODE/$CELL_NAME/seed$SEED`), so every results root in this repo is
`<root>/{multiclass,binary}/<cell>/seed<k>`. Earlier drafts of this table omitted it.

**Comparison arms are NOT pre-trained.** The v2 arms — U-Net in
`Models/factorial_results_v2/<mode>/fld_chmret_leafoff/` and UNet3+ in
`Models/results_arch_v2/<mode>/fld_chmret_leafoff_unet3plus/` — are 26-channel runs on the
old 689-patch field pool and are **not comparable** to a v3 fusion cell. All three arches
retrain on the 29-channel v3 stack, at **the same 5 seeds** — R=5 is now the v3 default in
`run_factorial.sh` itself (v2 ran 3, which would have left a 5-seed fusion arm with only 3
paired seeds; raising the grid rather than topping up one config keeps every factorial
contrast at n=5 too) — in **both modes**. The v3 roots
are `Models/factorial_results_v3/` (U-Net) and `Models/results_arch_v3/` (UNet3+).

**Two deltas from the arch-compare template:**

- **`BATCH_SIZE` as an env knob, default 8.** UNet3+ already needed 8→4 on the A6000. The
  fusion encoder is ~1.3× the U-Net's plus six fusion modules — lighter than UNet3+, but
  expect one OOM-and-halve iteration on first launch.
- **Gate export.** Gate rasters are a deliverable, not a debug artifact. Run a fixed set of
  held-out patches and write per-scale gate maps to `seed<k>/gates/` — six arrays of
  `(n_branch, H, W)` per patch, small enough to sync back with `--metrics-only`.

  Do this in a **separate script** that loads the checkpoint and does its own forward pass,
  not as an edit to `dl_05_evaluate.py`. §1 puts `dl_05` out of scope, and a standalone
  exporter also re-runs against any archived cell without re-evaluating it.

> **Agent boundary (unchanged):** Claude *prepares* these scripts; **the user runs** all
> GPU/long jobs. Nothing here auto-launches training, containers, or rsync.

---

## 6 · Aggregation & figures

**Implemented.** `dl_08b_aggregate_patchcurve.py --arch-compare` now takes repeatable
`--arch-dir <name>=<path>` pairs, so arm count follows from the CLI:

```bash
python dl_08b_aggregate_patchcurve.py --arch-compare \
  --config fld_chmret_leafoff --mode multiclass \
  --arch-dir unet=Models/factorial_results_v3 \
  --arch-dir unet3plus=Models/results_arch_v3 \
  --arch-dir mbfusion=Models/results_arch_fusion_v3
```

`--mode` is appended to each root when the root does not already end in it, so both
path forms work. The cell inside a root is `<config>_<name>`, falling back to plain
`<config>` for the base grid, whose cells are not arch-suffixed. Output: four CSVs in
the last arm's `analysis/` — `arch_compare_long.csv` (tidy, one row per arch × seed),
`arch_contrasts.csv`, `arch_cost.csv`, and the v2-shaped wide `arch_compare.csv`.

`--unet-dir` / `--unet3plus-dir` remain as deprecated aliases; the v2 two-arm output
reproduces **numerically identically** from the new code (verified column-by-column
against the committed v2 CSVs — the new columns are purely additive).

**Report paired per-seed deltas, not only mean ± sd.** Same seed ⇒ same test patches, so
each seed gives all three arches an identical evaluation set. At n=5, consistency of sign
("fusion beats U-Net in 5/5 seeds by 1.2–2.1 points") is the credible statistic; p-values
on n=5 are not worth the space.

**Metrics, in priority order:**

1. **UPL↔FSW confusion cells** — the specific failure this architecture targets.
   Implemented as `--confusion-pair A B` (default `FSW UPL`), read per cell from
   `confusion_matrix.csv` and **row-normalized**, so `conf_FSW_as_UPL` is "share of
   true-FSW pixels predicted UPL" — comparable across cells of differing prevalence,
   which raw counts are not. Contrasts on these rows are direction-aware
   (`lower_is_better`). Absent classes (binary mode) are skipped, not zero-filled.
2. Macro F1 and per-class IoU (the standard table) — plus per-class F1.
3. WET IoU + recall — the criteria `dl_prod_config.py` used, so the production-recipe
   question stays answerable from the same output. Binary-mode cells carry `iou_WET` /
   `f1_WET` through the same columns.

**Cost table** (`arch_cost.csv`): params, GFLOPs, and sec/epoch per arm, plus params as
a multiple of the baseline's — the "~1.3× the encoder, not 4×" claim as measured numbers
next to UNet3+. Provenance, since the three columns do not share a source: **params**
come from the trainer's journal `cost` block (exact, added in v3), falling back to the
`.safetensors` header (also exact, but absent from a `--metrics-only` pull) and then to
Lightning's rounded summary; **GFLOPs** only from that summary in `train.log`, so the
column is blank where the container's Lightning did not print it; **sec/epoch** is
fit-only wall clock from the journal, v3 onwards, left blank rather than reconstructed
from file mtimes for older cells. It is a same-GPU comparison, not a portable benchmark.

**Figures** live in `R_Code_Analysis/dl_10_Factorial_viz_R.qmd` — the active viz notebook
(the Python `dl_10_factorial_viz.ipynb` is the older sibling). Its architecture section
now reads `arch_compare_long.csv` and is arm-count-agnostic: point `arch_dir_base` at the
fusion root and it renders macro-F1, per-class F1, the FSW↔UPL confusion panel, the
contrast and cost tables, and a gate-weight-by-scale plot. Adding a fourth arm needs only
an entry in `arch_name`/`arch_color`.

### Interpreting gate maps — the one caveat

After gating, `proj` is a 1×1 conv over the concatenated features, so the decoder input is
effectively `Σᵢ Wᵢ(fᵢ · gᵢ)`. The gate is a spatial scalar; `Wᵢ` is a learned linear map
with its own magnitude. Therefore:

- **Valid:** within-branch spatial comparison — "terrain reliance rises in depressions
  relative to sideslopes." The gate is the only thing varying across space, so spatial
  patterns are faithful.
- **Confounded:** cross-branch absolute comparison — "terrain matters more than LiDAR
  overall" — since a branch with modest gates but large `Wᵢ` can still dominate.

GroupNorm equalizes features, not projection weights. So: report per-branch gate maps
**standardized within branch**, and take overall branch importance from **SHAP**
(`dl_09_shap_factorial.py`), which is already in the pipeline. Keep the two claims separate.

---

## 7 · Framing (for the methods section)

The usual justification for separate modality branches is sensor misalignment — LiDAR point
clouds vs. HSI pixels, registration error (Effah et al. 2025 §3.4). **That problem does not
exist here:** everything is already co-registered on a 1 m grid as bands of one stack. The
defensible justification is instead:

1. **Preventing a spectrally dominant, high-variance modality (leaf-on NAIP) from swamping
   early feature learning.**
2. **Per-branch gating weights as an interpretability output.** The factorial says which
   categories matter when present or absent; branch gating says how much the model weights
   each one, *per pixel and per scale*. This is a scale-resolved version of the below-canopy
   hypothesis and pairs directly with the SHAP analysis.

Effah et al. recommend **feature-level fusion** (majority strategy in wetland studies;
better dimensionality control and alignment, accuracy gains without heavy computational
overhead) — which supports a branch encoder over plain channel stacking. They also
repeatedly flag that hybrid-architecture complexity must be justified, which is what the
cost table and the ~1.3× encoder figure are for.

**On Wang et al. (2025), D2HU-Net.** Its "dual branch" is *not* modality fusion: one
encoder, two decoding paths, where a shallow decoding path guides the deep one via the MSAF
module. That is a different axis from this design and should not be conflated with it in
the writeup. The transferable piece is MSAF's **adaptive per-branch weighting**, adapted
here from decoder-path fusion to modality fusion, and extended from global coefficients to
per-pixel gates.

---

## 8 · Deferred / open

- **Fusion patchcurve.** Deferred until 1000s of patches. Machinery (`run_patchcurve.sh`,
  `dl_08b`) already exists and the U-Net curve was run on the same config, so it drops in
  later with no new tooling. Note the hypothesis is genuinely two-sided: more capacity
  argues for a steeper curve, more structural constraint argues for a flatter one that
  peaks early. Worth measuring, not assuming.
- **Statewide production model at 100 epochs.** Gated on the 5-seed comparison landing
  first. Do not pre-emptively edit `dl_prod_config.py`.
- **Equal-width control arm** (`32/32/32/32`). Edit the constant and re-run; serialization
  requirement in §4.2 is what makes the two runs distinguishable afterward.
- **Optical branches with NDVI/NDWI** (6 channels each, symmetric). Cheap later ablation if
  the illumination-invariance question is worth answering.
